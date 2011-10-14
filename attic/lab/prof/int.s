
.globl _interrupt_mem
_interrupt_mem:
  movl 4(%esp), %eax
L_loop1:
  subl $1, 0(%eax);
  jz L_int1
  subl $1, 0(%eax);
  jz L_int1
  jmp L_loop1
L_int1:
  ret

.globl _interrupt_reg
_interrupt_reg:
  movl 4(%esp), %eax
L_loop2:
  subl $1, %eax;
  jz L_int2
  subl $1, %eax;
  jz L_int2
  subl $1, %eax;
  jz L_int2
  subl $1, %eax;
  jz L_int2
  subl $1, %eax;
  jz L_int2
  subl $1, %eax;
  jz L_int2
  subl $1, %eax;
  jz L_int2
  jmp L_loop2
L_int2:
  ret


