# Ikarus Scheme -- A compiler for R6RS Scheme.
# Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
# Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

.text
.globl  ik_asm_enter
.globl _ik_asm_enter
.globl  ik_foreign_call
.globl _ik_foreign_call
.globl  ik_asm_reenter
.globl _ik_asm_reenter
.globl ik_underflow_handler
.globl _ik_underflow_handler



## ------------------------------------------------------------
## 64-bit code.
## ------------------------------------------------------------

#if __x86_64__

.align 8
ik_asm_enter:
_ik_asm_enter:
# c parameters come in registers:
#     %rdi, %rsi, %rdx, %rcx, %r8 and %r9
# return value registers are %rax and %rdi
# callee-save registers:
#     %rbp, %rbx, %r12, r13, r14, %r15 are callee-save


  # First, save all callee-save registers
  mov %rbp,  -8(%rsp)      # preserve
  mov %rbx,  -16(%rsp)     # preserve
  mov %r12,  -24(%rsp)     # preserve
  mov %r13,  -32(%rsp)     # preserve
  mov %r14,  -40(%rsp)     # preserve
  mov %r15,  -48(%rsp)     # preserve

  # closure pointer is the 4th arg, or %rcx
  # argcount is the third arg, or %rdx
  # code is the second arg, or  %rsi
  # pcb is the first arg, or    %rdi
  # return point is at       0(%rsp)

  mov %rdx, %rax     # set up arg count
  mov %rsi, %rdx     # move code pointer to %rdx
  mov %rdi, %rsi     # move pcb into pcb-register (%rsi)
  mov %rcx, %rdi     # move closure pointer into cpr
  mov 0(%rsi), %rbp  # allocation pointer is at 0(pcb)
  sub $64, %rsp         # 64 for alignment
  mov %rsp, 48(%rsi)    # save esp in pcb->system_stack
  mov 16(%rsi), %rsp    # load scheme stack from pcb->frame_pinter
  jmp L_call
  .quad 8
  .quad 0
L_multivalue_label: # FIXME
  .quad  L_multivalue_underflow
  .quad 0
L_call:
  call *%rdx         # goooooooo
  # now we're back
ik_underflow_handler:
_ik_underflow_handler:
  mov %rax, -16(%rsp)   # store the return value
  mov $-8, %rax         # set rvcount = 1
L_do_underflow:
  mov %rsp, 16(%rsi)    # store scheme stack in pcb->frame_pointer
  mov %rbp, 0(%rsi)     # store allocation pointer pcb->allocation_pointer
  mov 48(%rsi), %rsp    # restore system stack from pcb->system_stack
  add $64, %rsp         # 64 for alignment

  # restore callee-save registers
  mov -8(%rsp) ,  %rbp     # restore
  mov -16(%rsp),  %rbx     # restore
  mov -24(%rsp),  %r12     # restore
  mov -32(%rsp),  %r13     # restore
  mov -40(%rsp),  %r14     # restore
  mov -48(%rsp),  %r15     # restore
  ret      # back to C, which handled the underflow
multivalue_underflow:
L_multivalue_underflow:
  add $8, %rsp
  jmp L_do_underflow

## ------------------------------------------------------------

.align 8
ik_asm_reenter:
_ik_asm_reenter:
  # c parameters come in registers:
  #     %rdi, %rsi, %rdx, %rcx, %r8 and %r9
  # return value registers are %rax and %rdi
  # callee-save registers:
  #     %rbp, %rbx, %r12, r13, r14, %r15 are callee-save

  # argc is the third arg       12(%esp)   %rdx
  # scheme stack is second arg  8(%esp)    %rsi
  # pcb is the first arg        4(%esp)    %rdi
  # return point is at          0(%esp)

  mov %rbp,  -8(%rsp)      # preserve
  mov %rbx,  -16(%rsp)     # preserve
  mov %r12,  -24(%rsp)     # preserve
  mov %r13,  -32(%rsp)     # preserve
  mov %r14,  -40(%rsp)     # preserve
  mov %r15,  -48(%rsp)     # preserve

  movq %rdx, %rax        # third arg -> argc
  movq %rsi, %rbx        # second arg -> rbx  (scheme stack)
  movq %rdi, %rsi        # first arg -> pcb
  movq 0(%rsi), %rbp     # allocation pointer is at 0(pcb)


  subq $64, %rsp         # for alignment
  movq %rsp, 48(%rsi)    # save esp in pcb->system_stack
  movq %rbx, %rsp        # load scheme stack from rbx
  cmpq $-8, %rax
  jne L_multi_value_reentry

L_single_value_reentry:
  movq -8(%rsp), %rax
  ret

L_multi_value_reentry:
  movq 0(%rsp), %rbx
  jmp *-18(%rbx)

## ------------------------------------------------------------

# Call a C function From Scheme.   When entering this function:
#
# * %rsi must contain the address of the PCB structure.
#
# * %rdi must contain the address of the C function to call.
#
# * %rax must contain a fixnum representing the encoded number of
#   arguments to the C function, EXcluding the last argument
#   (the "ikpcb * pcb"), such arguments are on the Scheme stack.
#
# NOTE When we enter this routine to call the C function
# "ik_stack_overflow()", we enter with a Scheme stack segment
# and come back with another Scheme stack segment.
#
.align 8
ik_foreign_call:
_ik_foreign_call:
  # Store the Frame Pointer Register into "pcb->frame_pointer".
  movq %rsp, 16(%rsi)    # (movl fpr (pcb-ref 'frame-pointer))
  # Store the Allocation Pointer Register into "pcb->allocation_pointer".
  movq %rbp, 0(%rsi)     # (movl apr (pcb-ref 'allocation-pointer))
  # Save the Frame Pointer Register (FPR, %esp) into RBX.  We will
  # need it later to transfer the arguments from the Scheme stack to
  # the C stack.
  movq %rsp, %rbx        # (movl fpr ebx)
  # Store in %rsp the C stack from "pcb->system_stack".
  movq 48(%rsi), %rsp    # (movl (pcb-ref 'system-stack) esp)

  # Here %rsp references the last used word on the C stack: the
  # "old top of C stack".

  # According to the calling convention for AMD64:
  #
  # * The registers: %rbp, %rbx, %r12 .. %r15 are preserved by the callee.
  #
  # * The registers: %rdi, %rsi, %rdx, %rcx, %r8, %r9 are parameter regs.
  #
  #

  # Move in %r12 the address of the function to call, later we will use
  # this value as argument for the "call" instruction.  %r12 will be
  # preserved by the callee.
  movq %rdi, %r12
  # Move in %r13 the address of the PCB.  %r14 will be preserved by the
  # callee.
  movq %rsi, %r13

  # Move the address of the PCB in first argument register: %rdi.
  movq %rsi, %rdi

  # We want that: after pushing the C arguments on the C stack, %rsp is
  # 16-byte aligned (a calling convention requirement established by
  # C compilers).  Every argument will occupy a wordsize on the Stack,
  # here we want to pad the C stack as needed.
  #
  # Here we align the system stack %rsp by subtracting from it its least
  # significant byte, if %rsp is not already 16-byte aligned.
  #
  # This aligment is fine if the number of arguments less than 5: in this
  # case all the arguments and the PCB address fit in the CPU registers,
  # no arguments pushed on the stack.
  #
  # This alignment is a starting point for further considerations if
  # there are 6 or more arguments, in which case later we will do what
  # is needed.
  #
  # (Here we use %rdx as a scratch register.)
  movq %rsp, %rdx
  andq $15, %rdx	# mask in the least significant byte
  subq %rdx, %rsp	# subtract the least significant byte

  # If there are no arguments: jump to the appropriate label, the only
  # argument is the PCB addres in %rdi.
  cmpq $0, %rax
  je L_zero_args

  # There is one argument: jump to the appropriate label.
  cmpq $-8, %rax
  je L_one_arg

  # There are two arguments: jump to the appropriate label.
  cmpq $-16, %rax
  je L_two_args

  # There are three arguments: jump to the appropriate label.
  cmpq $-24, %rax
  je L_three_args

  # There are four arguments: jump to the appropriate label.
  cmpq $-32, %rax
  je L_four_args

  # There are five arguments: jump to the appropriate label.
  cmpq $-40, %rax
  je L_five_args

  # There are six arguments: jump to the appropriate label.
  cmpq $-48, %rax
  je L_six_args

  # There are more than six arguments: we push the 7th, 8th, ...
  # arguments on the stack, then fall through below to the six
  # arguments label.  Let's consider the example of a function call to:
  #
  #   ikptr fun (ikptr arg1, ikptr arg2, ikptr arg3, ikptr arg4,
  #              ikptr arg5, ikptr arg6, ikptr arg7, ikptr arg8,
  #              ikpcb * pcb);
  #
  # When arriving here: %ebx references the top of the Scheme stack;
  # %eax = -8 * wordsize = -64, the offset to add to %ebx to
  # reference the topmost argument.  On the Scheme stack we have:
  #
  #         high memory
  #   |                      |
  #   |----------------------|
  #   |    return address    | <-- %ebx
  #   |----------------------|
  #   |     C argument 1     | <-- %ebx - 1 * wordsize = %ebx -  8
  #   |----------------------|
  #   |     C argument 2     | <-- %ebx - 2 * wordsize = %ebx - 16
  #   |----------------------|
  #   |     C argument 3     | <-- %ebx - 3 * wordsize = %ebx - 24
  #   |----------------------|
  #   |     C argument 4     | <-- %ebx - 4 * wordsize = %ebx - 32
  #   |----------------------|
  #   |     C argument 5     | <-- %ebx - 5 * wordsize = %ebx - 40
  #   |----------------------|
  #   |     C argument 6     | <-- %ebx - 6 * wordsize = %ebx - 48
  #   |----------------------|
  #   |     C argument 7     | <-- %ebx - 7 * wordsize = %ebx - 56
  #   |----------------------|
  #   |     C argument 8     | <-- %ebx - 8 * wordsize = %ebx + %eax
  #   |----------------------|
  #   |                      |
  #          low memory
  #
  # On the C stack, right before performing the call instruction,
  # we want:
  #
  #         high memory
  #   |                      |
  #   |----------------------|
  #   |  old top of C stack  |
  #   |----------------------|
  #   |    pointer to PCB    | <-- %esp + 2 * wordsize
  #   |----------------------|
  #   |     C argument 8     | <-- %esp + 1 * wordsize
  #   |----------------------|
  #   |     C argument 7     | <-- %esp
  #   |----------------------|
  #   |                      |
  #          low memory
  #
  # and in the CPU registers we want:
  #
  #   %rdi == arg1, %rsi == arg2, %rdx == arg3,
  #   %rcx == arg4, %r8  == arg5, %r9  == arg6
  #
  # To keep %rsp with 16-bytes alignment we do this reasoning:
  #
  # 1. We know that now %rsp is 16-bytes aligned (we did it before).
  #
  # 2. We know that we will push the PCB address on the stack, and it
  #    accounts for 8 bytes.
  #
  # 3. Every argument we will push on the stack accounts for 8 bytes.
  #
  # 4. We store 6 (even) argument in the CPU registers.
  #
  # 5. If we push an even number of machine words on the stack: the
  #    alignment is preserved.
  #
  # so:
  #
  # * If the number of arguments is odd: an odd number of arguments
  #   will be pushed on the stack; plus the PCB address: an even number
  #   of machine words will be pushed on the stack; we *do not* need to
  #   push additional words to keep the alignment.
  #
  # * If the number of arguments is even: an even number of arguments
  #   will be pushed on the stack; plus the PCB address: an odd number
  #   of machine words will be pushed on the stack; we *do* need to push
  #   an additional word on the stack to keep the alignment.
  #
  # If a machine word represents an odd number: its least significant
  # bit is set.  On 64-bit platforms: fixnums are encoded by left-shifting
  # the raw integer by 3 bits.  So a machine word represents an even
  # fixnum if its least significant bits are #b0000, and it represents
  # an odd fixnum if its least significant bits are #b1000.
  #
  # To isolate the least significant nibble in a machine word: we AND
  # it with #b1111 = 15.  If the result of such AND is not zero: we
  # *do not* need to push an additional word on the stack.
  #
  # (Here we use %rdx as a scratch register.)
  movq %rax, %rdx
  andq $15, %rdx
  cmpq $0, %rdx		# is this needed at all???
  jnz L_no_pad
  push $0
L_no_pad:

  # Push the PCB address on the C stack: it is the rightmost argument
  # to the C function.
  push %rdi		 # pcb
  # Copy the arguments from the Scheme stack to the C stack.
L_loop:                  # (label Lloop)
  movq (%rbx,%rax), %rcx # (movl (mem rbx rax) rcx)
  push %rcx              # (pushl rcx)
  addq $8, %rax          # (addl (int 8) rax)
  cmpq $-48, %rax        # (cmpl (int -48) rax)
  jne L_loop             # (jne (label Lloop))
  movq  -8(%rbx), %rdi		# arg1
  movq -16(%rbx), %rsi		# arg2
  movq -24(%rbx), %rdx		# arg3
  movq -32(%rbx), %rcx		# arg4
  movq -40(%rbx), %r8		# arg5
  movq -48(%rbx), %r9		# arg6
  jmp L_set

L_six_args:
  # There are six arguments.  A function call to:
  #
  #   ikptr fun (ikptr arg1, ikptr arg2, ikptr arg3,
  #              ikptr arg4, ikptr arg5, ikptr arg6,
  #              ikpcb * pcb);
  #
  # is performed by setting:
  #
  #   %rdi == arg1, %rsi == arg2, %rdx == arg3,
  #   %rcx == arg4, %r8  == arg5, %r9  == arg6
  #
  # and pushing the PCB address on the stack.  We first push a word
  # on the stack to preserve 16-byte alignment of the value of %rsp.
  #
  push $0
  push %rdi			# pcb
  movq  -8(%rbx), %rdi		# arg1
  movq -16(%rbx), %rsi		# arg2
  movq -24(%rbx), %rdx		# arg3
  movq -32(%rbx), %rcx		# arg4
  movq -40(%rbx), %r8		# arg5
  movq -48(%rbx), %r9		# arg6
  jmp L_set

L_five_args:
  # There are five arguments.  A function call to:
  #
  #   ikptr fun (ikptr arg1, ikptr arg2, ikptr arg3,
  #              ikptr arg4, ikptr arg5,
  #              ikpcb * pcb);
  #
  # is performed by setting:
  #
  #   %rdi == arg1, %rsi == arg2, %rdx == arg3,
  #   %rcx == arg4, %r8  == arg5, %r9  == pcb
  #
  movq %rdi, %r9		# pcb
  movq  -8(%rbx), %rdi		# arg1
  movq -16(%rbx), %rsi		# arg2
  movq -24(%rbx), %rdx		# arg3
  movq -32(%rbx), %rcx		# arg4
  movq -40(%rbx), %r8		# arg5
  jmp L_set

L_four_args:
  # There are four arguments.  A function call to:
  #
  #   ikptr fun (ikptr arg1, ikptr arg2, ikptr arg3,
  #              ikptr arg4,
  #              ikpcb * pcb);
  #
  # is performed by setting:
  #
  #   %rdi == arg1, %rsi == arg2, %rdx == arg3,
  #   %rcx == arg4, %r8  == pcb
  #
  movq %rdi, %r8		# pcb
  movq  -8(%rbx), %rdi		# arg1
  movq -16(%rbx), %rsi		# arg2
  movq -24(%rbx), %rdx		# arg3
  movq -32(%rbx), %rcx		# arg4
  jmp L_set

L_three_args:
  # There are three arguments.  A function call to:
  #
  #   ikptr fun (ikptr arg1, ikptr arg2, ikptr arg3,
  #              ikpcb * pcb);
  #
  # is performed by setting:
  #
  #   %rdi == arg1, %rsi == arg2, %rdx == arg3,
  #   %rcx == pcb
  #
  movq %rdi, %rcx		# pcb
  movq  -8(%rbx), %rdi		# arg1
  movq -16(%rbx), %rsi		# arg2
  movq -24(%rbx), %rdx		# arg3
  jmp L_set

L_two_args:
  # There are two arguments.  A function call to:
  #
  #   ikptr fun (ikptr arg1, ikptr arg2,
  #              ikpcb * pcb);
  #
  # is performed by setting:
  #
  #   %rdi == arg1, %rsi == arg2, %rdx == pcb
  #
  movq %rdi, %rdx		# pcb
  movq  -8(%rbx), %rdi		# arg1
  movq -16(%rbx), %rsi		# arg2
  jmp L_set

L_one_arg:
  # There is one argument.  A function call to:
  #
  #   ikptr fun (ikptr arg1, ikpcb * pcb);
  #
  # is performed by setting:
  #
  #   %rdi == arg1, %rsi == pcb
  #
  movq %rdi, %rsi		# pcb
  movq -8(%rbx), %rdi		# arg1
  jmp L_set

L_zero_args:
  # There are zero arguments.  A function call to:
  #
  #   ikptr fun (ikpcb * pcb);
  #
  # is performed by setting:
  #
  #   %rdi == pcb
  #
  # and we already did it before.

L_set:                   # (label Lset)
  call *%r12             # (call cpr)
L_back:
  # Restore PCB from %r13.
  movq %r13, %rsi
  # Retrieve the Frame Pointer Register from "pcb->frame_pointer".
  movq 16(%rsi), %rsp    # (movl (pcb-ref 'frame-pointer) fpr)
  # Retrieve the Allocation Pointer Register from "pcb->allocation_pointer".
  movq 0(%rsi), %rbp	 # (movl (pcb-ref 'allocation-pointer) apr)
  # Return to Scheme.
  ret


## ------------------------------------------------------------
## 32-bit code.
## ------------------------------------------------------------

#else

.align 8
ik_asm_enter:
_ik_asm_enter:
  # Called by the C function "ik_exec_code()" to enter Scheme code
  # execution; this assembly label is called with the C calling
  # convention.
  #
  # When we enter here the situation on the C stack is as follows:
  #
  #        high memory
  #    |                  |
  #    |------------------|
  #    |  C argument 3    | <-- %esp + 4 * wordsize = 16(%esp)
  #    |------------------|
  #    |  C argument 2    | <-- %esp + 3 * wordsize = 12(%esp)
  #    |------------------|
  #    |  C argument 1    | <-- %esp + 2 * wordsize =  8(%esp)
  #    |------------------|
  #    |  C argument 0    | <-- %esp + 1 * wordsize =  4(%esp)
  #    |------------------|
  #    | C return address | <-- %esp                =  0(%esp)
  #    |------------------|
  #    |                  |
  #         low memory
  #
  # the C arguments are:
  #
  #   argument 0: raw memory pointer referencing the PCB structure
  #   argument 1: raw memory pointer referencing the entry point in
  #               the closure's code object
  #   argument 2: fixnum being the negated number of Scheme arguments
  #   argument 3: tagged pointer referencing the closure object
  #
  # and the "C return address" at 0(%esp) leads back to the C caller.
  #
  # When we enter here we expect the situation on the Scheme stack to be
  # as follows:
  #
  #          high memory
  #    |                   |
  #    |-------------------|
  #    |                   | <-- pcb->frame_pointer = pcb->frame_base
  #    |-------------------|
  #    |    empty word     | <-- pcb->frame_pointer - wordsize
  #    |-------------------|
  #    | Scheme argument 0 | <-- pcb->frame_pointer - 2 * wordsize
  #    |-------------------|
  #    | Scheme argument 1 | <-- pcb->frame_pointer - 3 * wordsize
  #    |-------------------|
  #    | Scheme argument 2 | <-- pcb->frame_pointer - 4 * wordsize
  #    |-------------------|
  #    |                   |
  #          low memory
  #
  # and the "empty" word will be the one containing the return address
  # "ik_underflow_handler".
  #
  # To call the Scheme closure we:
  #
  # 1. Store the fixnum representing the number of arguments in %eax.
  #
  # 2. Store a pointer to closure object in %edi, so we can use it
  #    to access the free variables.
  #
  # 3. Store a pointer to the PCB structure in %esi.
  #
  # 4. Store the allocation pointer "pcb->allocation_pointer" in %ebp.
  #    The allocation pointer is the first word in the PCB structure.
  #
  # 5. Save the C stack pointer from %esp to "pcb->system_stack".
  #
  # 6. Store the pointer to the Scheme stack "pcb->frame_pointer"
  #    in %esp.  The frame pointer is the 3rd word in the PCB
  #    structure (32-bit offset = 8).
  #

  # Preserve registers not preserved by the C call.  According to stuff on
  # the Net: it is responsibility of the C code caller to preserve EAX, ECX
  # and EDX; it is responsibility of the assembly callee to preserve ESI, EBP,
  # EDI, EBX.
  movl %esi, -4(%esp)    # preserve
  movl %ebp, -8(%esp)    # preserve
  movl %edi, -12(%esp)   # preserve
  movl %ebx, -16(%esp)   # preserve
  #
  # When arriving here the situation on the C stack is as follows:
  #
  #        high memory
  #    |                  |
  #    |------------------|
  #    |  C argument 3    | <-- %esp + 4 * wordsize =  16(%esp)
  #    |------------------|
  #    |  C argument 2    | <-- %esp + 3 * wordsize =  12(%esp)
  #    |------------------|
  #    |  C argument 1    | <-- %esp + 2 * wordsize =   8(%esp)
  #    |------------------|
  #    |  C argument 0    | <-- %esp + 1 * wordsize =   4(%esp)
  #    |------------------|
  #    | C return address | <-- %esp                =   0(%esp)
  #    |------------------|
  #    |    saved ESI     | <-- %esp - 1 * wordsize =  -4(%esp)
  #    |------------------|
  #    |    saved EBP     | <-- %esp - 2 * wordsize =  -8(%esp)
  #    |------------------|
  #    |    saved EDI     | <-- %esp - 3 * wordsize = -12(%esp)
  #    |------------------|
  #    |    saved EBX     | <-- %esp - 4 * wordsize = -16(%esp)
  #    |------------------|
  #         low memory
  #

  movl 12(%esp), %eax    # %eax = fixnum being the negated num of args
  movl 16(%esp), %edi    # %edi = pointer to closure object
  movl 4(%esp), %esi     # %esi = pointer to PCB
  movl 0(%esi), %ebp     # %ebp = allocation pointer, it is at 0(pcb)

  # Save the C stack in ECX.  To be used later to retrieve the Scheme
  # code entry point address and actually perform the call.
  movl %esp, %ecx

  # Decrement ESP so that it references the topmost used word on the C stack,
  # then store it in "pcb->system_stack" as top of the C stack.  Having ESP
  # decremented by 16 (4 wordsizes) is also fine for alignment constraints,
  # required by the C compiler.
  subl $16, %esp
  movl %esp, 24(%esi)

  movl 8(%esi), %esp     # load scheme stack from "pcb->frame_pointer"
  jmp L_call
  # Notice that there is no livemask bytevector here.
  .long 4			# We can access this data word
				# by summing "disp_call_table_size"
				# to "ik_underflow_handler".

  .long 0			# We can access this data word
				# by summing "disp_call_table_offset"
				# to "ik_underflow_handler".

  .long L_multivalue_underflow	# We can access this data word by
				# summing "disp_multivalue_rp" to
				# "ik_underflow_handler".
  .byte 0
  .byte 0

  # Call the binary code in the code object.  This instruction decrements
  # %esp and stores in the newly referenced location the return address
  # for the Scheme code; such return address is the label
  # "ik_underflow_handler".  Right after the jump to the Scheme code, the
  # situation on the Scheme stack is as follows:
  #
  #          high memory
  #    |                      |
  #    |----------------------|
  #    |                      | <-- pcb->frame_pointer = pcb->frame_base
  #    |----------------------|
  #    | ik_underflow_handler | <-- %esp
  #    |----------------------|
  #    |   Scheme argument 0  | <-- %esp - 1 * wordsize
  #    |----------------------|
  #    |   Scheme argument 1  | <-- %esp - 2 * wordsize
  #    |----------------------|
  #    |   Scheme argument 2  | <-- %esp - 3 * wordsize = %esp + %eax
  #    |----------------------|
  #    |                      |
  #           low memory
  #
L_call:
  call *8(%ecx)
ik_underflow_handler:
  # Here we are back from the Scheme code, but there may be a
  # continuation waiting to be entered in "pcb->next_k".
  #
  # If we are here it is because a single argument was returned,
  # neither zero arguments nor 2 or more arguments.  This single
  # argument is in %eax.
  #
  # Knowing that to return here a "ret" instruction must have incremented
  # the %esp while it was referencing the label "ik_underflow_handler",
  # now the Scheme stack looks like this:
  #
  #         high memory
  #   |                      |
  #   |----------------------|
  #   |                      | <-- %esp = pcb->frame_base
  #   |----------------------|
  #   | ik_underflow_handler | <-- %esp - wordsize
  #   |----------------------|
  #   |                      |
  #          low memory
  #
  # the word referenced by %esp is unused here: it may be one-off the
  # end of the stack segment or it may be part of an uplevel Scheme
  # continuation.  We want to set the Scheme stack as follows:
  #
  #         high memory
  #   |                      |
  #   |----------------------|
  #   |                      | <-- %esp = pcb->frame_base = pcb->frame_pointer
  #   |----------------------|
  #   | ik_underflow_handler | <-- %esp - 1 * wordsize = %esp + %eax
  #   |----------------------|
  #   | Scheme return value  | <-- %esp - 2 * wordsize
  #   |----------------------|
  #   |                      |
  #          low memory
  #
  # and then do what is needed to go back to the C caller.  Notice that
  # the Scheme return value is at "pcb->frame_base - 2 * wordsize".
  #
  movl %eax, -8(%esp)		# Put the return value on the stack.
  movl $-4, %eax		# Return to the C caller a fixnum
  				# representing the negated number of
  				# Scheme return values.
L_do_underflow:
  # NOTE When coming back here from Scheme code evaluation, it is possible
  # that "pcb->frame_base" and "pcb->frame_pointer" do not have the same
  # values as before entering Scheme code.  In particular: the Scheme routines
  # handling continuations update only "pcb->frame_base", leaving alone
  # "pcb->frame_pointer".  For this reason here we update "pcb->frame_pointer"
  # leaving alone "pcb->frame_base".  (Marco Maggi; Mar 28, 2013)
  movl %esp, 8(%esi)    # save top of scheme stack in "pcb->frame_pointer"
  movl %ebp, 0(%esi)    # save allocation pointer in "pcb->allocation_pointer"

  # Restore from "pcb->system_stack" the address of the topmost used word
  # on the C stack and increment it to reference the return address to the
  # C caller.
  movl 24(%esi), %esp
  addl $16, %esp

  # Restore callee-save registers.
  movl -4(%esp), %esi
  movl -8(%esp), %ebp
  movl -12(%esp), %edi
  movl -16(%esp), %ebx

  # Back to the C caller: the function "ik_exec_code()".
  ret

L_multivalue_underflow:
  # Here we are back from the Scheme code, but there may be a
  # continuation waiting to be entered in "pcb->next_k".
  #
  # If we are here it is because zero, 2 or more arguments were returned
  # by a Scheme function; these returned values are on the Scheme stack,
  # while the negated number of returned values is in %eax as fixnum.
  #
  # Knowing that to return here a "jmp" instruction was executed
  # while %esp was referencing the label "ik_underflow_handler", now the
  # Scheme stack looks like this:
  #
  #         high memory
  #   |                       |
  #   |-----------------------|
  #   |                       | <-- pcb->frame_base
  #   |-----------------------|
  #   | ik_underflow_handler  | <-- %esp
  #   |-----------------------|
  #   | Scheme return value 0 | <-- %esp - 1 * wordsize
  #   |-----------------------|
  #   | Scheme return value 1 | <-- %esp - 2 * wordsize = %esp + %eax
  #   |-----------------------|
  #   | Scheme return value 2 | <-- %esp - 3 * wordsize
  #   |-----------------------|
  #   |                       |
  #          low memory
  #
  # the word referenced by %esp+wordsize is unused here: it may be one-off
  # the end of the stack segment or it may be part of an uplevel Scheme
  # continuation.  We want to set the Scheme stack as follows:
  #
  #         high memory
  #   |                       |
  #   |-----------------------|
  #   |                       | <-- %esp = pcb->frame_base = pcb->frame_pointer
  #   |-----------------------|
  #   | ik_underflow_handler  | <-- %esp - 1 * wordsize
  #   |-----------------------|
  #   | Scheme return value 0 | <-- %esp - 2 * wordsize
  #   |-----------------------|
  #   | Scheme return value 1 | <-- %esp - 3 * wordsize = %esp + %eax
  #   |-----------------------|
  #   | Scheme return value 2 | <-- %esp - 4 * wordsize
  #   |-----------------------|
  #   |                       |
  #          low memory
  #
  # and then do what is needed to go back to the C caller.  Notice that
  # the first Scheme return value is at "pcb->frame_base - 2 * wordsize".
  #
  addl $4, %esp
  jmp L_do_underflow

## ------------------------------------------------------------

.align 8
ik_asm_reenter:
_ik_asm_reenter:
  # Called by the C function "ik_exec_code()" to reenter Scheme code
  # execution at a previously saved continuation; this assembly label
  # is called with the C calling convention.
  #
  # When we enter here the situation on the C stack is as follows:
  #
  #        high memory
  #    |                  |
  #    |------------------|
  #    |  C argument 2    | <-- %esp + 3 * wordsize = 12(%esp)
  #    |------------------|
  #    |  C argument 1    | <-- %esp + 2 * wordsize =  8(%esp)
  #    |------------------|
  #    |  C argument 0    | <-- %esp + 1 * wordsize =  4(%esp)
  #    |------------------|
  #    | C return address | <-- %esp                =  0(%esp)
  #    |------------------|
  #    |                  |
  #         low memory
  #
  # the C arguments are:
  #
  #   argument 0: raw memory pointer referencing the PCB structure
  #   argument 1: raw memory pointer, the top of the Scheme stack
  #   argument 2: fixnum being the negated number of return values
  #
  # and the "C return address" at 0(%esp) leads back to the C caller.
  #
  # The situation on the Scheme stack is as follows:
  #
  #         high memory
  #    |                       |
  #    |-----------------------|
  #    | Scheme return address | <-- 8(%esp) = top of Scheme stack
  #    |-----------------------|
  #    | Scheme return value 0 | <-- 8(%esp) - 1 * wordsize
  #    |-----------------------|
  #    | Scheme return value 1 | <-- 8(%esp) - 2 * wordsize
  #    |-----------------------|
  #    | Scheme return value 2 | <-- 8(%esp) - 3 * wordsize = 8(%esp) + 12(%esp)
  #    |-----------------------|
  #    |                       |
  #          low memory
  #

  # Preserve registers not preserved by the C call.  According to stuff on
  # the Net: it is responsibility of the C code caller to preserve EAX, ECX
  # and EDX; it is responsibility of the assembly callee to preserve ESI, EDI,
  # EBP, EBX.
  movl %esi,  -4(%esp)   #  preserve
  movl %ebp,  -8(%esp)   #  preserve
  movl %edi, -12(%esp)   #  preserve
  movl %ebx, -16(%esp)   #  preserve
  #
  # When arriving here the situation on the C stack is as follows:
  #
  #        high memory
  #    |                  |
  #    |------------------|
  #    |  C argument 2    | <-- %esp + 3 * wordsize =  12(%esp)
  #    |------------------|
  #    |  C argument 1    | <-- %esp + 2 * wordsize =   8(%esp)
  #    |------------------|
  #    |  C argument 0    | <-- %esp + 1 * wordsize =   4(%esp)
  #    |------------------|
  #    | C return address | <-- %esp                =   0(%esp)
  #    |------------------|
  #    |    saved ESI     | <-- %esp - 1 * wordsize =  -4(%esp)
  #    |------------------|
  #    |    saved EBP     | <-- %esp - 2 * wordsize =  -8(%esp)
  #    |------------------|
  #    |    saved EDI     | <-- %esp - 3 * wordsize = -12(%esp)
  #    |------------------|
  #    |    saved EBX     | <-- %esp - 4 * wordsize = -16(%esp)
  #    |------------------|
  #         low memory
  #

  # Make %eax hold a fixnum representing the negated number of args.
  movl 12(%esp), %eax

  # Make %ebx reference the top of the Scheme stack.
  movl  8(%esp), %ebx

  # Make %esi reference the PCB.
  movl 4(%esp), %esi

  # Make %ebp reference the alloc pointer "pcb->allocation_pointer".
  movl 0(%esi), %ebp

  # Decrement ESP so that it references the latest used word on the stack,
  # then store it in "pcb->system_stack" as top of the C stack.  Having ESP
  # decremented by 16 (4 wordsizes) is also fine for alignment constraints,
  # required by the C compiler.
  subl $16, %esp
  movl %esp, 24(%esi)

  # Make %esp reference the Scheme stack.
  movl %ebx, %esp

  # The machine word -4 is the fixnum -1, which represents
  # a single argument.  If %eax == -4, then there is a single
  # argument to the Scheme continuation.
  cmpl $-4, %eax
  jne L_multi_value_reentry

L_single_value_reentry:
  # Move the single return value in %eax.
  movl -4(%esp), %eax
  # Return to the "Scheme return address".
  ret

  # There are multiple return values.  Rather than returning to
  # "Scheme return address" we jump directly to the entry point at
  # offset "disp_multivalue_rp" from "Scheme return address",
  # with %eax holding the fixnum being the negated number of args.
  #
  #  -9 = - (32-bit call instruction size) - (32-bit wordsize)
  #
L_multi_value_reentry:
  movl 0(%esp), %ebx
  jmp *-9(%ebx)

## ------------------------------------------------------------

# Call a C function From Scheme.   When entering this function:
#
# * %esi must contain the address of the PCB structure.
#
# * %edi must contain the address of the C function to call.
#
# * %eax must contain a fixnum representing the encoded number of
#   arguments to the C function, EXcluding the last argument
#   (the "ikpcb * pcb"), such arguments are on the Scheme stack.
#
# NOTE When we enter this routine to call the C function
# "ik_stack_overflow()", we enter with a Scheme stack segment
# and come back with another Scheme stack segment.
#
.align 8
ik_foreign_call:
_ik_foreign_call:
  # Store the Frame Pointer Register into "pcb->frame_pointer".
  movl %esp, 8(%esi)
  # Store the Allocation Pointer Register into "pcb->allocation_pointer".
  movl %ebp, 0(%esi)
  # Save the Frame Pointer Register (FPR, %esp) into EBX.  We will
  # need it later to transfer the arguments from the Scheme stack to
  # the C stack.
  movl %esp, %ebx
  # Store in %esp the C stack from "pcb->system_stack".
  movl 24(%esi), %esp    # (movl (pcb-ref 'system-stack) esp)

  # Here %esp references the last used word on the C stack: the
  # "old top of C stack".

  # We want that: after pushing the C arguments on the C stack, %esp is
  # 16-byte aligned (a calling convention requirement established by
  # C compilers).  Every argument will occupy a wordsize on the Stack,
  # here we want to pad the C stack with zeros as needed.
  #
  # * If %eax is the fixnum 0 (%eax == 0) the C function has 0 arguments;
  #   plus the pointer to PCB: 1 argument;  with the return address on
  #   the stack: 2 words = 8 bytes: 2 padding words are needed.  The C stack
  #   will be:
  #
  #            high memory
  #      |                      |
  #      |----------------------|
  #      |  old top of C stack  |
  #      |----------------------|           --
  #      |    padding word      |           .
  #      |----------------------|           .
  #      |    padding word      |           .
  #      |----------------------|           . 16 bytes
  #      |   pointer to PCB     |           .
  #      |----------------------|           .
  #      |    return address    | <-- %esp  .
  #      |----------------------|           --
  #      |                      |
  #             low memory
  #
  # * If %eax is the fixnum -1 (%eax == -4) the C function has 1 argument;
  #   plus the pointer to PCB: 2 arguments;  with the return address on
  #   the stack: 3 words = 12 bytes: 1 padding word is needed.  The C stack
  #   will be:
  #
  #            high memory
  #      |                      |
  #      |----------------------|
  #      |  old top of C stack  |
  #      |----------------------|           --
  #      |    padding word      |           .
  #      |----------------------|           .
  #      |   pointer to PCB     |           .
  #      |----------------------|           . 16 bytes
  #      |      argument 0      |           .
  #      |----------------------|           .
  #      |    return address    | <-- %esp  .
  #      |----------------------|           --
  #      |                      |
  #             low memory
  #
  # * If %eax is the fixnum -2 (%eax == -8) the C function has 2 arguments;
  #   plus the pointer to PCB: 3 arguments;  with the return address on
  #   the stack: 4 words = 16 bytes: no padding is needed.  The C stack
  #   will be:
  #
  #            high memory
  #      |                      |
  #      |----------------------|
  #      |  old top of C stack  |
  #      |----------------------|           --
  #      |   pointer to PCB     |           .
  #      |----------------------|           .
  #      |      argument 1      |           .
  #      |----------------------|           . 16 bytes
  #      |      argument 0      |           .
  #      |----------------------|           .
  #      |    return address    | <-- %esp  .
  #      |----------------------|           --
  #      |                      |
  #             low memory
  #
  # * If %eax is the fixnum -3 (%eax == -12) the C function has 3 arguments;
  #   plus the pointer to PCB: 4 argument;  with the return address on
  #   the stack: 5 words = 20 bytes: 3 padding words are needed to reach
  #   the next multiple of 16 which is 32.  The C stack will be:
  #
  #            high memory
  #      |                      |
  #      |----------------------|
  #      |  old top of C stack  |
  #      |----------------------|           --
  #      |    padding word      |           .
  #      |----------------------|           .
  #      |    padding word      |           .
  #      |----------------------|           . 16 bytes
  #      |    padding word      |           .
  #      |----------------------|           .
  #      |   pointer to PCB     |           .
  #      |----------------------|           --
  #      |      argument 2      |           .
  #      |----------------------|           .
  #      |      argument 1      |           .
  #      |----------------------|           . 16 bytes
  #      |      argument 0      |           .
  #      |----------------------|           .
  #      |    return address    | <-- %esp  .
  #      |----------------------|           --
  #      |                      |
  #             low memory
  #
  # * If %eax is the fixnum -4 (%eax == -16) the C function has 4 arguments;
  #   plus the pointer to PCB: 5 arguments;  with the return address on
  #   the stack: 6 words = 24 bytes: 2 padding words are needed to reach
  #   the next multiple of 16 which is 32.  This is the same as %eax == 0.
  #   The C stack will be:
  #
  #            high memory
  #      |                      |
  #      |----------------------|
  #      |  old top of C stack  |
  #      |----------------------|           --
  #      |    padding word      |           .
  #      |----------------------|           .
  #      |    padding word      |           .
  #      |----------------------|           . 16 bytes
  #      |   pointer to PCB     |           .
  #      |----------------------|           .
  #      |      argument 3      |           .
  #      |----------------------|           --
  #      |      argument 2      |           .
  #      |----------------------|           .
  #      |      argument 1      |           .
  #      |----------------------|           . 16 bytes
  #      |      argument 0      |           .
  #      |----------------------|           .
  #      |    return address    | <-- %esp  .
  #      |----------------------|           --
  #      |                      |
  #             low memory
  #
  # For the purpose of padding to multiples of 16 bytes: when there
  # are more than 3 arguments to the C function (EXcluding the PCB
  # pointer) we are interested in the arguments count:
  #
  #	(mod (- %eax) 16)
  #
  movl %eax, %ecx
  # Mask the 15 least significant bits: the ones representing the
  # the positive number: (mod (- %eax) 16).
  andl $15, %ecx
check_ecx:
  # 2 arguments + PCB pointer + return address -> no padding
  cmpl $8, %ecx
  je L_zero
  # 3 arguments + PCB pointer + return address -> 1 padding word
  cmpl $12, %ecx
  je L_one
  # 0 arguments + PCB pointer + return address -> 2 padding words
  cmpl $0, %ecx
  je L_two
  # 1 argument  + PCB pointer + return address -> 3 padding words
  push $0
L_two:
  push $0
L_one:
  push $0
L_zero:

  # Now, after pushing the arguments and the return address on the
  # stack: the value of %esp will be 16-byte aligned.

  # When calling a foreign C function: we always push the Process
  # Control Register (%esi, 4 bytes) to the stack; this is the
  # pointer to PCB which is the last argument to every C function.
  push %esi		# (pushl pcr)

  # If there are no arguments: we are done.  Jump to the call
  # instruction.
  cmpl $0, %eax		# (cmpl (int 0) eax)
  je L_set		# (je (label Lset))

  # Remember that when arriving here: %ebx references the top of
  # the Scheme stack, %eax is the the offset to add to %ebx to
  # reference the topmost argument.  For 2 arguments, on the Scheme
  # stack we have:
  #
  #         high memory
  #   |                      |
  #   |----------------------|
  #   |    return address    | <-- %ebx
  #   |----------------------|
  #   |      argument 0      | <-- %ebx - 1 * wordsize
  #   |----------------------|
  #   |      argument 1      | <-- %ebx - 2 * wordsize = %ebx + %eax
  #   |----------------------|
  #   |                      |
  #          low memory
  #
  # On the C stack, right before performing the call instruction,
  # we want:
  #
  #         high memory
  #   |                      |
  #   |----------------------|
  #   |  old top of C stack  |
  #   |----------------------|
  #   |    pointer to PCB    |
  #   |----------------------|
  #   |      argument 1      | <-- %esp + 1 * wordsize = %esp - %eax
  #   |----------------------|
  #   |      argument 0      | <-- %esp
  #   |----------------------|
  #   |                      |
  #          low memory
  #
  # Copy the arguments from the Scheme stack to the C stack.
L_loop:                  # (label Lloop)
  movl (%ebx,%eax), %ecx # (movl (mem ebx eax) ecx)
  push %ecx              # (pushl ecx)
  addl $4, %eax          # (addl (int 4) eax)
  cmpl $0, %eax          # (cmpl (int 0) eax)
  jne L_loop             # (jne (label Lloop))

L_set:                   # (label Lset)
  call *%edi             # (call cpr)
  # The C functions callable here always return a single argument
  # of type "ikptr", and it is in %eax.

  # Load "pcb->frame_pointer" in the Stack Pointer Register.
  movl 8(%esi), %esp     # (movl (pcb-ref 'frame-pointer) fpr)
  # Load "pcb->allocation_pointer" in the Allocation Pointer Register.
  movl 0(%esi), %ebp     # (movl (pcb-ref 'allocation-pointer) apr)

  # Notice that here we do not need to modify "pcb->system_stack".

  # Return to the caller.  If the C function has not touched the
  # Scheme stack: we go back to right after the "call" instruction
  # that entered "ik_foreign_call".  If the foreign function is
  # "ik_stack_overflow()": we return to the assembly label
  # "ik_underflow_handler".
  ret                    # (ret)

#endif

### end of file
# Local Variables:
# comment-start: "#"
# End:
