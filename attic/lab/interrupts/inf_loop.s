
.globl _inf_loop

_inf_loop:
  movl %esp, %eax
  movl $0, %esp
L_loop:
  jmp L_loop
