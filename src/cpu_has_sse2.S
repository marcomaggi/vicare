#  Ikarus Scheme -- A compiler for R6RS Scheme.
#  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
#  
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License version 3 as
#  published by the Free Software Foundation.
#  
#  This program is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.


.text
.globl cpu_has_sse2
.globl _cpu_has_sse2

.align 8

cpu_has_sse2:
_cpu_has_sse2:
  # callee-save registers are $ebx, %esi, %edi, %esp, $ebp
  # cpuid modifies %eax, %ebx, %ecx, %edx
  # only %ebx needs to be saved/restored
#if __x86_64__
  push %rbx
#else
  push %ebx
#endif
  movl $1, %eax
  cpuid
  movl %edx, %eax
  sarl $26, %eax
  andl $1, %eax
#if __x86_64__
  pop %rbx
#else
  pop %ebx
#endif
  ret
