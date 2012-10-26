/*
 * Ikarus Scheme -- A compiler for R6RS Scheme.
 * Copyright (C) 2006,2007,2008	 Abdulaziz Ghuloum
 * Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
 *
 * This program is free software:  you can redistribute it and/or modify
 * it under  the terms of  the GNU General  Public License version  3 as
 * published by the Free Software Foundation.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY	  WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See	 the GNU
 * General Public License for more details.
 *
 * You should  have received  a copy of	 the GNU General  Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "internals.h"

#define DEBUG_EXEC	0

ikptr
ik_exec_code (ikpcb * pcb, ikptr code_ptr, ikptr s_argcount, ikptr s_closure)
/* Execute  Scheme  code  and  all   its  continuations  until  no  more
   continuations are stored in the PCB or a system continuation is found
   in the continuations linked list.

   CODE_PTR is a  raw memory pointer referencing the entry  point in the
   closure's code object.

   S_ARGCOUNT  is a  fixnum representing  the negated  number of  Scheme
   arguments.

   S_CLOSURE is a reference to the closure object to execute.

   Return the return value of the last executed continuation. */
{
#if DEBUG_EXEC
  fprintf(stderr, "%s: enter ", __func__);
  ik_fprint(stderr, s_closure);
  fprintf(stderr, "\n", __func__);
#endif
  ikptr		s_argc;
  ikptr		s_next_k;
  s_argc   = ik_asm_enter(pcb, code_ptr+off_code_data, s_argcount, s_closure);
  s_next_k = pcb->next_k;
  while (s_next_k) {
    ikcont * p_next_k = (ikcont *)(long)(s_next_k - vector_tag);
    /* System continuations are created by the FFI to save the current C
       execution contest just before calling back a Scheme function. */
    if (system_continuation_tag == p_next_k->tag)
      break;
    ikptr	top       = p_next_k->top;
    ikptr	rp        = IK_REF(top, 0);
    long	framesize = (long) IK_REF(rp, disp_frame_size);
#if DEBUG_EXEC
    fprintf(stderr, "exec framesize=0x%016lx kontsize=%ld rp=0x%016lx\n",
	    framesize, p_next_k->size, rp);
#endif
    if (0 == framesize)
      framesize = IK_REF(top, wordsize);
    if (framesize <= 0)
      ik_abort("invalid framesize %ld\n", framesize);
    if (framesize < p_next_k->size) {
      /* Insert  a new  continuation  between "s_next_k"  and its  next.
       * Before:
       *
       * pcb
       *  |          s_next_k
       *  |------------>|        s_further
       *      next_k    |--------->|
       *                   next    |-------> NULL
       *                             next
       *
       * after:
       *
       * pcb
       *  |          s_next_k
       *  |------------>|       s_new_kont
       *      next_k    |--------->|       s_further
       *                   next    |-------->|
       *                             next    |--------> NULL
       *                                        next
       */
      ikcont *	p_new_kont = (ikcont*)(long)ik_unsafe_alloc(pcb, sizeof(ikcont));
      p_new_kont->tag  = p_next_k->tag;
      p_new_kont->next = p_next_k->next;
      p_new_kont->top  = top + framesize;
      p_new_kont->size = p_next_k->size - framesize;
      p_next_k->size   = framesize;
      p_next_k->next   = vector_tag | (ikptr)(long)p_new_kont;
      { /* Record in  the dirty vector  the side effect of  mutating the
	   field "p_next_k->next". */
	ik_ulong idx = ((ik_ulong)(&p_next_k->next)) >> IK_PAGESHIFT;
	((int*)(long)(pcb->dirty_vector))[idx] = -1;
      }
    } else if (framesize > p_next_k->size) {
      ik_abort("invalid framesize %ld, expected %ld or less\n\trp = 0x%016lx\n\trp offset = %ld",
	       framesize, p_next_k->size, rp, IK_REF(rp, disp_frame_offset));
    }
    pcb->next_k = p_next_k->next;
#if DEBUG_EXEC
    fprintf(stderr, "%s: reenter\n", __func__);
#endif
    {
      ikptr fbase     = pcb->frame_base - wordsize;
      ikptr new_fbase = fbase - framesize;
      /* The argc is negative for a reason! */
      memmove((char*)(long)new_fbase + s_argc,
	      (char*)(long)fbase     + s_argc,
	      -s_argc);
      memcpy((char*)(long)new_fbase, (char*)(long)top, framesize);
      s_argc   = ik_asm_reenter(pcb, new_fbase, s_argc);
    }
    s_next_k =  pcb->next_k;
  }  /* end of while() */

  /* Retrieve  the return  value from  the stack  and return  it to  the
   * caller.
   *
   *     high memory
   *   |            |
   *   |            |
   *   |------------|
   *   |            | <-- pcb->frame_base
   *   |------------|
   *   |            | <-- pcb->frame_base - wordsize
   *   |------------|
   *   |            | <-- pcb->frame_base - 2 * wordsize
   *   |------------|
   *   |            |
   *   |            |
   *     low memory
   *
   * Remember that  "pcb->frame_base" references a word  that is one-off
   * the end of the stack segment; so the first word in the stack is:
   *
   *    pcb->frame_base - wordsize
   */
  ikptr rv = IK_REF(pcb->frame_base, -2*wordsize);
  return rv;
}

/* end of file */
