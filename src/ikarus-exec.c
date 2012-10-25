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

#undef DEBUG_EXEC

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
  ikptr		s_argc;
  ikptr		s_next_k;
  s_argc   = ik_asm_enter(pcb, code_ptr+off_code_data, s_argcount, s_closure);
  s_next_k = pcb->next_k;
  while (s_next_k) {
    ikcont * kont = (ikcont*)(long)(s_next_k - vector_tag);
    if (kont->tag == system_continuation_tag) {
      /* System continuations are created by the FFI to save the current
	 C  execution   contest  just  before  calling   back  a  Scheme
	 function. */
      break;
    }
    ikptr	top = kont->top;
    ikptr	rp  = IK_REF(top, 0);
    long int	framesize = (long int) IK_REF(rp, disp_frame_size);
#ifdef DEBUG_EXEC
    fprintf(stderr, "exec framesize=0x%016lx kontsize=%ld rp=0x%016lx\n",
	    framesize, kont->size, rp);
#endif
    if (0 == framesize)
      framesize = IK_REF(top, wordsize);
    if (framesize <= 0)
      ik_abort("invalid framesize %ld\n", framesize);
    if (framesize < kont->size) {
      ikcont *	nk = (ikcont*)(long)ik_unsafe_alloc(pcb, sizeof(ikcont));
      nk->tag    = kont->tag;
      nk->next   = kont->next;
      nk->top    = top + framesize;
      nk->size   = kont->size - framesize;
      kont->size = framesize;
      kont->next = vector_tag + (ikptr)(long)nk;
      { /* Record in  the dirty vector  the side effect of  mutating the
	   field "kont->next". */
	ik_ulong idx = ((ik_ulong)(&kont->next)) >> IK_PAGESHIFT;
	((int*)(long)(pcb->dirty_vector))[idx] = -1;
      }
    } else if (framesize > kont->size) {
      ik_abort("invalid framesize %ld, expected %ld or less\n\trp = 0x%016lx\n\trp offset = %ld",
	       framesize, kont->size, rp, IK_REF(rp, disp_frame_offset));
    }
    pcb->next_k = kont->next;
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
  }
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
