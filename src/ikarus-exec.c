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

static void ik_exec_code_log_and_abort (ikpcb * pcb, ikptr s_kont);


ikptr
ik_exec_code (ikpcb * pcb, ikptr s_code, ikptr s_argcount, ikptr s_closure)
/* Execute  Scheme  code  and  all   its  continuations  until  no  more
   continuations are stored in the PCB or a system continuation is found
   in the continuations linked list.

   S_CODE  is  a  tagged  memory pointer  referencing  the  code  object
   implementing S_CLOSURE, if any.

   S_ARGCOUNT  is a  fixnum representing  the negated  number of  Scheme
   arguments.

   S_CLOSURE is a reference to the  closure object to execute; it can be
   the fixnum zero if there is no closure to execute, as when we enter a
   loaded FASL file.

   Return the return value of the last executed continuation. */
{
  /* A fixnum representing the negated number of returned Scheme values.
     It can be zero. */
  ikptr		s_retval_count;
  /* Reference to the  continuation object representing the  C or Scheme
     continuation we want to go back to. */
  ikptr	s_kont;
  if (0 || DEBUG_EXEC) {
    ik_debug_message_start("%s: enter closure 0x%016lx",
			   __func__, (long)s_closure);
    ik_fprint(stderr, IK_REF(s_code, off_code_annotation));
    fprintf(stderr, "\n");
  }
  /* Enter compiled Scheme code.

     Before and after we assert that  the frame pointer equals the frame
     base; this constraint on the Scheme stack is needed by the assembly
     routine  "ik_asm_enter".  It  is  responsibility of  the caller  of
     "ik_exec_code()" to set the Scheme stack appropriately. */
  {
    assert(pcb->frame_base == pcb->frame_pointer);
    s_retval_count = ik_asm_enter(pcb, IK_CODE_ENTRY_POINT(s_code), s_argcount, s_closure);
    assert(pcb->frame_base == pcb->frame_pointer);
  }
  /* Loop until there are continuations to be reinstated. */
  for (s_kont = pcb->next_k; s_kont; s_kont = pcb->next_k) {
#ifndef NDEBUG
    {
      /* Assert that the situation on the Scheme stack is:
       *
       *        high memory
       *  |                      | <- pcb->frame_pointer = pcb->frame_base
       *  |----------------------|
       *  | ik_underflow_handler | <- pcb->frame_pointer - wordsize
       *  |----------------------|
       *  |    return value 0    |
       *  |----------------------|
       *  |    return value 1    |
       *  |----------------------|
       *  |    return value 2    |
       *  |----------------------|
       *  |                      |
       *        low memory
       *
       * Of course we cannot check for the presence of return values.
       */
      ikptr	underflow_handler;
      assert(pcb->frame_base == pcb->frame_pointer);
      underflow_handler = *(ikptr *)(pcb->frame_pointer - wordsize);
      assert(IK_UNDERFLOW_HANDLER == underflow_handler);
    }
#endif
    assert(continuation_primary_tag == (continuation_primary_mask & s_kont));
    if (0 || DEBUG_EXEC) {
      ik_debug_message("%s: resuming process continuation s_kont=0x%016lx",
		       __func__, (long)s_kont);
    }
    ikcont * kont = IK_CONTINUATION_STRUCT(s_kont);
    /* System continuations are created by the FFI to save the current C
       execution contest just before calling back a Scheme function.  So
       if S_KONT is a system continuation:  we have no Scheme code to go
       back to, we just return to the  caller of this C function.  It is
       responsibility of such caller to  reinstate the continuation to C
       code. */
    if (system_continuation_tag == kont->tag)
      break;
    assert(continuation_tag == kont->tag);
    /* RETURN_ADDRESS is a  raw memory address being the  entry point in
       machine code we have to jump back to. */
    ikptr	return_address = IK_REF(kont->top, 0);
    /* FRAMESIZE is stack  frame size of the function we  have to return
       to.  This value was computed at compile time and stored in binary
       code just before the "call" instruction. */
    long	framesize = IK_CALLTABLE_FRAMESIZE(return_address);
    if (0 || DEBUG_EXEC) {
      ik_debug_message("%s: framesize=%ld kont->size=%ld return_address=0x%016lx",
		       __func__, framesize, kont->size, return_address);
    }
    /* NOTE  This note  and the  operation it  describes are  related to
       issue #35.  This block of code is *not* a fix for the bug, just a
       temporary work around that partially mitigates the problem.

       The Scheme code  has returned to the stack  underflow handler, so
       if  the  "next  process  continuation" has  the  stack  underflow
       handler as  return point:  this is  a bogus  continuation.  Let's
       just discard  it and go  on with  the next continuation  from the
       continuation stack.

       The  bug  consists in  the  fact  that  a continuation  with  the
       underflow handler as return point should never be created.  Also,
       such  continuation has  size equal  to zero:  another thing  that
       should never happen.

       This bug causes (in rare  cases) some continuations to be created
       with zero  size, but  return point  different from  the underflow
       handler; such continuations will cause the process to abort.

       (Marco Maggi; Fri Mar 22, 2013) */
    if (IK_UNDERFLOW_HANDLER == return_address) {
      if (1 || DEBUG_EXEC) {
	int	retc = -IK_UNFIX(s_retval_count);
	int	i;
	ik_debug_message("%s: continuation 0x%016lx has ik_underflow_handler as return point,\n\
\tsize=%ld, uplevel continuation is 0x%016lx, return values count %d",
			 __func__, s_kont, (long)kont->size, kont->next, retc);
	for (i=0; i<retc; ++i) {
	  ik_fprint(stderr, *((ikptr *)(pcb->frame_pointer - i * wordsize)));
	  fprintf(stderr, "\n");
	}
      }
      if (1) {
	ik_exec_code_log_and_abort(pcb, s_kont);
      } else {
	continue;
      }
    }
    /* Zero  framesize means  that we  are returning  to a  continuation
       having as  topmost stack frame  a frame  whose size could  not be
       computed at  compile time. In  such cases the framesize  field in
       the call table is set to zero  and the actual stack frame size is
       pushed  on the  stack  frame itself  before  performing a  "call"
       assembly instruction. */
    if (0 == framesize) {
      framesize = IK_REF(kont->top, wordsize);
      if (0 || DEBUG_EXEC) {
	ik_debug_message("%s: retrieved framesize=%ld from above top",
			 __func__, (long)framesize);
      }
    }
    /* Perform  some framesize  validations.  If  these happen  it means
       that there is a bug in Vicare. */
    {
      if (framesize <= 0) {
	ik_abort("invalid caller function framesize %ld\n", framesize);
      }
      if (framesize > kont->size) {
	ik_exec_code_log_and_abort(pcb, s_kont);
      }
    }
    if (framesize < kont->size) {
      /* The process continuation  we have to reinstate  references 2 or
	 more  freezed  frames.  Mutate  S_KONT  to  reference only  the
	 topmost  freezed frame  and  create a  new continuation  object
	 referencing  the  rest of  the  freezed  frames.  Register  the
	 "rest" continuation as "next process continuation". */
      ikcont *	rest_kont   = (ikcont*)(long)ik_unsafe_alloc(pcb, IK_ALIGN(continuation_size));
      ikptr	s_rest_kont = (ikptr)((long)rest_kont) | continuation_primary_tag;
      rest_kont->tag	= continuation_tag;
      rest_kont->next	= kont->next;
      rest_kont->top	= kont->top  + framesize;
      rest_kont->size	= kont->size - framesize;
      kont->size	= framesize;
      kont->next	= s_rest_kont;
      /* FIXME Is it required to signal dirt for both the fields?  Or it
	 always  happens that  a continuation  object's memory  block is
	 fully in a  single page?  In the original Ikarus  code only the
	 "kont->next" dirt was registered, but debugging of Issue #35 is
	 making me paranoid.  (Marco Maggi; Wed Mar 27, 2013) */
      IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, &(kont->size));
      IK_SIGNAL_DIRT_IN_PAGE_OF_POINTER(pcb, &(kont->next));
      if (0 == kont->size) {
	ik_debug_message("%s: next continuation with zero size 0x%016lx,\n\
\tframe return address=0x%016lx",
			 __func__, s_kont, IK_REF(kont->top, 0));
      }
      if (0 == rest_kont->size) {
	ik_debug_message("%s: rest continuation with zero size 0x%016lx,\n\
\ttop frame return address=0x%016lx",
			 __func__, s_rest_kont, IK_REF(rest_kont->top, 0));
      }
      pcb->next_k = kont->next;
    } else {
      /* The process continuation we have to reinstate references only 1
	 freezed  frame.   Just  pop   S_KONT  from  the  "next  process
	 continuations" list. */
      assert(framesize == kont->size);
      pcb->next_k = kont->next;
    }
    /* When  we arrive  here the  situation on  the Scheme  stack is  as
     * follows:
     *
     *        high memory
     *  |                      | <-- pcb->frame_pointer = pcb->frame_base
     *  |----------------------|
     *  | ik_underflow_handler | <-- fbase
     *  |----------------------|
     *  |    return value 0    |
     *  |----------------------|
     *  |    return value 1    |
     *  |----------------------|
     *  |    return value 2    | <-- fbase + s_retval_count
     *  |----------------------|
     *  |                      |
     *        low memory
     *
     * and it is possible that there  are no return values on the Scheme
     * stack.  Let's  assume that  the freezed  frame referenced  by the
     * continuation object S_KONT is as follows:
     *
     *        high memory
     *  |                      |
     *  |----------------------|                 --
     *  |      argument 0      |                 .
     *  |----------------------|                 .
     *  |      argument 1      |                 .
     *  |----------------------|                 . framesize
     *  |      argument 2      |                 .
     *  |----------------------|                 .
     *  |    return address    | <-- kont->top   .
     *  |----------------------|                 --
     *  |                      |
     *        low memory
     *
     * We need to reinstate the  single freezed frame handing the return
     * values to its function.
     */
    {
      assert(pcb->frame_pointer == pcb->frame_base);
      ikptr	fbase      = pcb->frame_base - wordsize;
      ikptr	new_fbase  = fbase - framesize;
      /* Move the return values down a framesize:
       *
       *         high memory
       *  |                      | <-- pcb->frame_pointer = pcb->frame_base
       *  |----------------------|
       *  | ik_underflow_handler | <-- fbase
       *  |----------------------|                --
       *  |  return value 0 src  |                .
       *  |----------------------|                .
       *  |  return value 1 src  |                .
       *  |----------------------|                .
       *  |  return value 2 src  | <-- retval_src . framesize
       *  |----------------------|                .
       *  |                      | <-- new_fbase  .
       *  |----------------------|                --
       *  |  return value 0 dst  |                .
       *  |----------------------|                .
       *  |  return value 1 dst  |                . -s_retval_count
       *  |----------------------|                .
       *  |  return value 2 dst  | <-- retval_dst .
       *  |----------------------|                --
       *  |                      |
       *           low memory
       */
      {
	char *	retval_dst = ((char*)(long)new_fbase) + s_retval_count;
	char *	retval_src = ((char*)(long)fbase)     + s_retval_count;
	memmove(retval_dst, retval_src, -s_retval_count);
      }
      /* Copy  to  this  stack  segment   the  freezed  frame  from  the
       * continuation object S_KONT.
       *
       *       high memory
       *  |                      | <-- pcb->frame_pointer = pcb->frame_base
       *  |----------------------|
       *  | ik_underflow_handler | <-- fbase
       *  |----------------------|                --
       *  |       argument 0     |                .
       *  |----------------------|                .
       *  |       argument 1     |                .
       *  |----------------------|                . framesize
       *  |       argument 2     |                .
       *  |----------------------|                .
       *  |    return address    | <-- new_fbase  .
       *  |----------------------|                --
       *  |  return value 0 dst  |
       *  |----------------------|
       *  |  return value 1 dst  |
       *  |----------------------|
       *  |  return value 2 dst  |
       *  |----------------------|
       *  |                      |
       *        low memory
       */
      memcpy((char*)(long)new_fbase, (char*)(long)(kont->top), framesize);
      /* Then reenter  Scheme code execution using  "new_fbase" as frame
	 pointer.*/
      if (0 || DEBUG_EXEC) {
	ik_debug_message("%s: reenter assembly, return values count %lu",
			 __func__, IK_UNFIX(-s_retval_count));
      }
      assert(pcb->frame_pointer == pcb->frame_base);
      s_retval_count = ik_asm_reenter(pcb, new_fbase, s_retval_count);
      assert(pcb->frame_pointer == pcb->frame_base);
    }
    /* If we  are here: the Scheme  code we have reentered  has returned
       again; we need to reinstate the next process continuation. */
  }  /* end of for() */

  /* Retrieve  the return  value from  the stack  and return  it to  the
   * caller.
   *
   *      high memory
   *   |              |
   *   |--------------|
   *   |              | <-- pcb->frame_base = pcb->frame_pointer
   *   |--------------|                                     --
   *   |              | <-- pcb->frame_base - wordsize      .
   *   |--------------|                                     .
   *   | return value | <-- pcb->frame_base - 2 * wordsize  . Scheme
   *   |--------------|                                     . stack
   *   |              |                                     .
   *   |              |                                     .
   *   |--------------| <-- pcb->stack_base                 --
   *   |              |
   *      low memory
   */
  ikptr rv = IK_REF(pcb->frame_base, -2*wordsize);
  assert(pcb->frame_pointer == pcb->frame_base);
  return rv;
}


static void
ik_exec_code_log_and_abort (ikpcb * pcb, ikptr s_kont)
{
  ikptr underflow_handler	= *(ikptr *)(pcb->frame_pointer - wordsize);
  ikcont * kont			= IK_CONTINUATION_STRUCT(s_kont);
  ikptr	top			= IK_CONTINUATION_TOP(s_kont);
  ikptr	return_address		= IK_REF(top, 0);
  ikptr	call_table_framesize	= IK_CALLTABLE_FRAMESIZE(return_address);
  long	framesize		= (call_table_framesize)? \
    (long) call_table_framesize : IK_REF(top, wordsize);
  ikptr	return_address_offset	= IK_CALLTABLE_OFFSET(return_address);
  ik_abort("%s: internal error while resuming continuation:\n\
\tpcb->heap_base     = 0x%016lx\n\
\tpcb->heap_size     = %ld bytes, %ld words\n\
\tpcb->stack_base    = 0x%016lx\n\
\tpcb->stack_size    = %ld bytes, %ld words\n\
\tpcb->frame_redline = 0x%016lx, delta %ld words\n\
\tpcb->frame_pointer = 0x%016lx\n\
\tpcb->frame_base    = 0x%016lx\n\
\tik_underflow_handler = 0x%016x\n\
\tunderflow_handler    = 0x%016x\n\
\ts_kont        = 0x%016lx\n\
\tkont          = 0x%016lx\n\
\tkont->top     = 0x%016lx\n\
\tkont->size    = %ld\n\
\ts_kont return address           = 0x%016lx\n\
\ts_kont return address framesize = %ld\n\
\tinvalid framesize=%ld, expected %ld(=kont->size) or less",
	   __func__,
	   pcb->heap_base, pcb->heap_size, pcb->heap_size/wordsize,
	   pcb->stack_base, pcb->stack_size, pcb->stack_size/wordsize,
	   pcb->frame_redline, (pcb->stack_base+pcb->stack_size-pcb->frame_redline)/wordsize,
	   pcb->frame_pointer, pcb->frame_base,
	   (ikptr)ik_underflow_handler, underflow_handler,
	   (long)s_kont, (long)kont,
	   top,
	   IK_CONTINUATION_SIZE(s_kont),
	   return_address, call_table_framesize,
	   framesize, kont->size);
  /* Here  we would  like to  print the  annotation object  of the  code
     object. */
  {
    /* Offset  from the  beginning  of  the code  object  of the  return
       address. */
    ikptr	code_offset	= return_address_offset - disp_call_table_offset;
    /* Pointer to the first byte of  executable machine code in the code
       object memory block. */
    ikptr	code_entry	= return_address - code_offset;
    /* Pointer to the first byte of the code object memory block. */
    IK_UNUSED ikptr p_code	= code_entry - disp_code_data;
    ik_debug_message("annotation 0x%016lx", IK_REF(p_code, disp_code_annotation));
    /* ik_fprint(stderr, IK_REF(p_return_address_code, disp_code_annotation)); */
    /* fprintf(stderr, "\n"); */
  }
}

/* end of file */
