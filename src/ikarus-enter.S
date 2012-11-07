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

.align 8
ik_foreign_call:
_ik_foreign_call:
  movq %rsp, 16(%rsi)    # (movl fpr (pcb-ref 'frame-pointer))
  movq %rbp, 0(%rsi)     # (movl apr (pcb-ref 'allocation-pointer))
  movq %rsp, %rbx        # (movl fpr ebx)
  movq 48(%rsi), %rsp    # (movl (pcb-ref 'system-stack) esp)
  # %esp is the system stack, %eax is the index to the last arg,
  # %esi is the pcb.

  # align the system stack by subtracting 8 if it's not 16-byte aligned
  movq %rsp, %r12
  andq $15, %r12
  subq %r12, %rsp

  # AMD64 says: %rbp, %rbx, %r12 .. %r15 are preserved by the callee
  #             %rdi, %rsi, %rdx, %rcx, %r8, %r9 are parameter regs

  movq %rdi, %r12 # put target of call in %r12
  movq %rsi, %r13 # put pcb in %r13

  movq %rsi, %rdi # put pcb in first arg reg
  cmpq $0, %rax
  je L_zero_args
  cmpq $-8, %rax
  je L_one_arg
  cmpq $-16, %rax
  je L_two_args
  cmpq $-24, %rax
  je L_three_args
  cmpq $-32, %rax
  je L_four_args

  movq $0, %rbx
  movq %rbx,0(%rbx)
L_four_args:
  movq %rdi, %r8        # pcb
  movq -8(%rbx), %rdi
  movq -16(%rbx), %rsi
  movq -24(%rbx), %rdx
  movq -32(%rbx), %rcx
  jmp L_set
L_three_args:
  movq %rdi, %rcx        # pcb
  movq -8(%rbx), %rdi
  movq -16(%rbx), %rsi
  movq -24(%rbx), %rdx
  jmp L_set
L_two_args:
  movq %rdi, %rdx        # pcb
  movq -8(%rbx), %rdi
  movq -16(%rbx), %rsi
  jmp L_set
L_one_arg:
  movq %rdi, %rsi
  movq -8(%rbx), %rdi
  jmp L_set

L_zero_args:
L_set:                   # (label Lset)
  call *%r12             # (call cpr)
L_back:
  movq %r13, %rsi        # restore pcb from r13
  movq 16(%rsi), %rsp    # get frame pointer from 16(pcb)
  movq 0(%rsi), %rbp     # get allocation pointer from 0(pcb)
  ret                    # return to scheme


## ------------------------------------------------------------
## 32-bit code.
## ------------------------------------------------------------

#else

.align 8
ik_asm_enter:
_ik_asm_enter:
  # Called by the C function "ik_exec_code()" to enter Scheme code
  # execution.  When we enter here: the Scheme arguments are already
  # on the Scheme stack.  The C arguments are on the C stack:
  #
  #   argument 4: 16(%esp) = "ikptr" value referencing the closure
  #   argument 3: 12(%esp) = "ikptr" fixnum being the negated number
  #                          of Scheme arguments
  #   argument 2:  8(%esp) = raw memory pointer referencing the
  #	                     entry point in the closure's code
  #                          object
  #   argument 1:  4(%esp) = raw memory pointer referencing the
  #                          PCB C language structure
  #
  # The return address to go back to the C caller is at: 0(%esp).
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
  movl 12(%esp), %eax    # %eax = fixnum representing the num of args
  movl 16(%esp), %edi    # %edi = pointer to closure object
  movl %esi, -4(%esp)    # preserve
  movl %ebp, -8(%esp)    # preserve
  movl %edi, -12(%esp)   # preserve
  movl 4(%esp), %esi     # %esi = pointer to PCB
  movl 0(%esi), %ebp     # %ebp = allocation pointer is at 0(pcb)
  movl %esp, %ecx        # save the C stack, to be used later
  subl $16, %esp         # 16 for alignment
  movl %esp, 24(%esi)    # save %esp in "pcb->system_stack"
  movl 8(%esi), %esp     # load scheme stack from "pcb->frame_pointer"
  jmp L_call
  .long 4			# We can access this data word
				# by summing "disp_frame_size"
				# to "ik_underflow_handler".

  .long 0			# We can access this data word
				# by summing "disp_frame_offset"
				# to "ik_underflow_handler".

  .long L_multivalue_underflow	# We can access this data word by
				# summing "disp_multivale_rp" to
				# "ik_underflow_handler".
  .byte 0
  .byte 0

  # Call the binary code in the code object.  This instruction pushes
  # the return address to the memory location referenced by %esp.
  # Such return address is the label "ik_underflow_handler", which is
  # often named in the comments as "underflow_handler".
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
  # Knowing that to return here a "ret" instruction must have
  # incremented the %esp while it was referencing the label
  # "ik_underflow_handler", now the Scheme stack looks like this:
  #
  #         high memory
  #   |                      |
  #   |----------------------|
  #   |    return address    | <-- %esp = pcb->frame_base
  #   |----------------------|
  #   | ik_underflow_handler | <-- %esp - wordsize
  #   |----------------------|
  #   |                      |
  #          low memory
  #
  # where "return address" must have been left on the stack by some
  # function.  See for example the primitive instructions in
  # $CALL-WITH-UNDERFLOW-HANDLER.
  #
  # We want to set the Scheme stack like this:
  #
  #         high memory
  #   |                      |
  #   |----------------------|
  #   |    return address    | <-- %esp
  #   |----------------------|
  #   | ik_underflow_handler | <-- %esp - wordsize
  #   |----------------------|
  #   | Scheme return value  | <-- %esp - 2 * wordsize
  #   |----------------------|
  #   |                      |
  #          low memory
  #
  # and then do what is needed to go back to the C caller.
  #
  movl %eax, -8(%esp)   # store the return value
  # Set the value returned to the C caller to 1 encoded as
  # number of arguments.
  movl $-4, %eax
L_do_underflow:
  movl %esp, 8(%esi)    # store scheme stack in "pcb->frame_pointer"
  movl %ebp, 0(%esi)    # store allocation pointer in "pcb->allocation_pointer"
  movl 24(%esi), %esp   # restore C stack from "pcb->system_stack"
  addl $16, %esp        # 24 for alignment (>= 16)
  movl -4(%esp), %esi   # restore callee-save registers
  movl -8(%esp), %ebp   #
  movl -12(%esp), %edi  #
  ret                   # back to the C function "ik_exec_code()"

L_multivalue_underflow:
  addl $4, %esp
  jmp L_do_underflow

## ------------------------------------------------------------

.align 8
ik_asm_reenter:
_ik_asm_reenter:
  # Called by the C function "ik_exec_code()" to reenter Scheme code
  # execution at a previously saved continuation.  When we enter here:
  # the Scheme arguments are already on the Scheme stack.  The C
  # arguments are on the C stack:
  #
  #   argument 3: 12(%esp) = "ikptr" fixnum being the negated number
  #                          of Scheme arguments
  #   argument 2:  8(%esp) = raw memory pointer referencing the
  #	                     previously saved Scheme stack
  #   argument 1:  4(%esp) = raw memory pointer referencing the
  #                          PCB C language structure
  #
  # The return address to go back to the C caller is at: 0(%esp).
  #

  # Make %eax hold a fixnum representing the negated number of args.
  movl 12(%esp), %eax

  # Make %ebx reference the Scheme stack.
  movl  8(%esp), %ebx

  # Preserve registers not preserved by the C call.
  movl %esi, -4(%esp)    #  preserve
  movl %ebp, -8(%esp)    #  preserve
  movl %edi, -12(%esp)   #  preserve

  # Make %esi reference the PCB.
  movl 4(%esp), %esi

  # Make %ebp reference the alloc pointer "pcb->allocation_pointer".
  movl 0(%esi), %ebp

  subl $16, %esp         # 24 for alignment

  # Save the C language stack in "pcb->system_stack".
  movl %esp, 24(%esi)

  # Make %esp reference the Scheme stack.
  movl %ebx, %esp

  # The 32-bit machine word -4 is the fixnum -1, which represents
  # a single argument.  If %eax = -4, then there is a single
  # return value; else there are multiple return values.
  cmpl $-4, %eax
  jne L_multi_value_reentry

L_single_value_reentry:
  # Move the return value in %eax.
  movl -4(%esp), %eax
  # Return to the Scheme continuation.
  ret

L_multi_value_reentry:
  movl 0(%esp), %ebx
  jmp *-9(%ebx)

## ------------------------------------------------------------

.align 8
ik_foreign_call:
_ik_foreign_call:
  movl %esp, 8(%esi)     # (movl fpr (pcb-ref 'frame-pointer))
  movl %ebp, 0(%esi)     # (movl apr (pcb-ref 'allocation-pointer))
  movl %esp, %ebx        # (movl fpr ebx)
  movl 24(%esi), %esp    # (movl (pcb-ref 'system-stack) esp)
  # %esp is the system stack, %eax is the index to the last arg,
  # %esi is the pcb.
  # 0 args require 6 (2) pushes => argc=   0 (0000): %esp +=  -8
  # 1 args require 5 (1) pushes => argc=  -4 (1100): %esp +=  -4
  # 2 args require 4 (0) pushes => argc=  -8 (1000): %esp +=   0
  # 3 args require 3 (3) pushes => argc= -12 (0100): %esp += -12
  movl %eax, %ecx
  andl $15, %ecx
check_ecx:
  cmpl $8, %ecx
  je L_zero
  cmpl $12, %ecx
  je L_one
  cmpl $0, %ecx
  je L_two
  push $0
L_two:
  push $0
L_one:
  push $0
L_zero:

  # Now, the value of %esp is 16-byte aligned
  # we always push %esi (4 bytes) and do a call (4 bytes),
  push %esi             # (pushl pcr)

  cmpl $0, %eax          # (cmpl (int 0) eax)
  je L_set               # (je (label Lset))
L_loop:                  # (label Lloop)
  movl (%ebx,%eax), %ecx # (movl (mem ebx eax) ecx)
  push %ecx             # (pushl ecx)
  addl $4, %eax          # (addl (int 4) eax)
  cmpl $0, %eax          # (cmpl (int 0) eax)
  jne L_loop             # (jne (label Lloop))
L_set:                   # (label Lset)
  call *%edi             # (call cpr)
  movl 8(%esi), %esp     # (movl (pcb-ref 'frame-pointer) fpr)
  movl 0(%esi), %ebp     # (movl (pcb-ref 'allocation-pointer) apr)
  ret                    # (ret)))

#endif

### end of file
# Local Variables:
# comment-start: "#"
# End:
