;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(module INTEL-ASSEMBLY-CODE-GENERATION
  (
   ;;function-call table stuff
   CALL-INSTRUCTION-SIZE	DISP-MULTIVALUE-RP
   compile-call-table

   mem
   int		obj		byte		byte-vector
   tail-indirect-cpr-call	indirect-cpr-call
   argc-convention		register?

   label	label-address

   ;;assembly instructions
   addl		andl		call		cltd
   cmpl		idivl		imull		ja
   jb		je		jg		jge
   jl		jle		jmp		jne
   jo		leal		movb		movl
   movzbl	negl		notl		orl
   pop		popl		push		pushl
   ret		sall		sarl		sete
   shrl		subl		xorl

   ;;CPU registers
   al		ah		bh		cl
   eax		ebx		ecx		edx		esp
   AA-REGISTER			CP-REGISTER
   AP-REGISTER			FP-REGISTER
   PC-REGISTER

   NON-8BIT-REGISTERS		ALL-REGISTERS
   %cpu-register-name->index)


;;On the Intel architecture, the CPU registers have special use:
;;
;;AAR = %eax		accumulator and arguments count
;;APR = %ebp		allocation pointer
;;FPR = %esp		frame pointer register
;;PCR = %esi		pointer to PCB
;;CPR = %edi		pointer to closure
;;

;;Accumulator, Arguments count Register (AAR).
;;
;;Notice  that the  return values  of  a function  call  become the  operands of  the
;;continuation; this is because this register  is called "Arguments Count", it counts
;;both the operands to a function call and the operands to the continuation.
;;
;;Use as "Operands Count Register"
;;--------------------------------
;;
;;Upon entering a function: it must hold  a fixnum representing the negated number of
;;stack operands.  Such fixnum is built by  the caller and placed in AA-REGISTER; the
;;callee consumes it by matching it against the number of expected arguments.
;;
;;Examples: if the number  of stack operands is 2, the fixnum  in AA-REGISTER must be
;;-2; if the number of stack operands is 0, the fixnum in AA-REGISTER must be 0.
;;
;;Use as "Return Values Count Register"
;;-------------------------------------
;;
;;When a function returns a single Scheme  object: the immediate Scheme object or the
;;tagged pointer to the Scheme object is stored in this register.
;;
;;When a  function returns  zero, two  or more values:  the immediate  Scheme objects
;;and/or the  tagged pointers  to the Scheme  objects are stored  on the  stack; this
;;register holds a fixnum representing the negated number of objects.
;;
;;Examples: if the  number of return values  is 2, the fixnum in  AA-REGISTER must be
;;-2; if the number of returned values is 0, the fixnum in AA-REGISTER must be 0.
;;
(define-constant AA-REGISTER '%eax)

;;Allocation Pointer Register (APR)
;;
;;Holds the  address of  the first  free machine word  in the  memory segment  of the
;;Scheme heap nursery.
;;
(define-constant AP-REGISTER '%ebp)

;;Closure Pointer Register (CPR)
;;
;;Holds the  tagged address of  a machine word in  the Scheme heap,  representing the
;;reference to the closure object being  executed; when no closure is being executed:
;;it is set  to zero.  It is used to  enter the execution of the closure  code and to
;;access the free variables the closure is closed upon.
;;
;;It is responsibility of  the caller to a closure to store in  the CPR the reference
;;to closure object.
;;
(define-constant CP-REGISTER '%edi)

;;Frame Pointer Register (FPR)
;;
;;Holds the  address of the machine  word in the  memory segment of the  Scheme stack
;;holding the  return address  of the last  performed function call.   It is  used to
;;access  the machine  words on  the stack  containing function  arguments and  local
;;variables.
;;
(define-constant FP-REGISTER '%esp)

;;Process Control Register (PCR)
;;
;;Holds  the raw  address of  a machine  word in  the C  language heap;  such address
;;references the first word in the PCB data structure.
;;
(define-constant PC-REGISTER '%esi)

;;The  list of  CPU registers  that the  code in  a code  object can  use to  store
;;temporary results.
;;
(define-constant ALL-REGISTERS
  (boot.case-word-size
   ((32)
    '(%eax %edi %ebx %edx %ecx))
   ((64)
    '(%eax %edi %ebx %edx %ecx %r8 %r9 %r10 %r11 %r14 %r15))))

(define-constant NON-8BIT-REGISTERS
  (boot.case-word-size
   ((32)	'(%edi))
   ((64)	'(%edi))))

(define-constant esp	'%esp) ; stack frame pointer
(define-constant al	'%al)
(define-constant ah	'%ah)
(define-constant bh	'%bh)
(define-constant cl	'%cl)
(define-constant eax	'%eax)
(define-constant ebx	'%ebx)
(define-constant ecx	'%ecx)
(define-constant edx	'%edx)
(define register?	symbol?)

(module (%cpu-register-name->index)
  ;;CPU registers  are identified by a  symbol whose string  name is the name  of the
  ;;register; so  registers have no index  associated to them like,  for example, the
  ;;VAR  and FVAR  structs have.   For this  reason it  is not  immediate to  collect
  ;;register identifiers in an integer set as defined by the module INTEGER-SET.
  ;;
  ;;This module artificially introduces a map  between register names and indexes, so
  ;;it makes it  possible to use register  identifiers as memebrs of  sets.  Only the
  ;;registers actually  used in the compiler  pass "impose evaluation order"  must be
  ;;mapped.
  ;;
  (define* (%cpu-register-name->index x)
    (cond ((assq x INTEL-CPU-REGISTER/INDEX-MAP)
	   => cdr)
	  (else
	   (compiler-internal-error __who__
	     "expected symbol representing an integer-mapped Intel CPU register name (lower-case)"
	     x))))

  (define-constant INTEL-CPU-REGISTER/INDEX-MAP
    `((,AA-REGISTER	. 0)
      (,CP-REGISTER	. 1)
      (,AP-REGISTER	. 2)
      (,FP-REGISTER	. 3)
      (,PC-REGISTER	. 4)
      (,ecx		. 5)
      (,edx		. 6)))

  #| end of module |# )


;;;; assembly code generation helpers

(define* (mem off val)
  (cond ((fixnum? off)
	 (list 'disp (int off) val))
	((register? off)
	 (list 'disp off val))
	(else
	 (compiler-internal-error __who__
	   "invalid displacement offset"
	   off))))

(define-syntax int
  (syntax-rules ()
    ((_ x) x)))

(define (obj x)		(list 'obj x))
(define (byte x)	(list 'byte x))
(define (byte-vector x) (list 'byte-vector x))
(define (movzbl src targ) (list 'movzbl src targ))
(define (sall src targ)	(list 'sall src targ))
(define (sarl src targ) (list 'sarl src targ))
(define (shrl src targ) (list 'shrl src targ))
(define (notl src)	(list 'notl src))
(define (pushl src)	(list 'pushl src))
(define (popl src)	(list 'popl src))
(define (orl src targ)	(list 'orl src targ))
(define (xorl src targ) (list 'xorl src targ))
(define (andl src targ) (list 'andl src targ))
(define (movl src targ) (list 'movl src targ))
(define (leal src targ) (list 'leal src targ))
(define (movb src targ) (list 'movb src targ))
(define (addl src targ) (list 'addl src targ))
(define (imull src targ) (list 'imull src targ))
(define (idivl src)	(list 'idivl src))
(define (subl src targ) (list 'subl src targ))
(define (push src)	(list 'push src))
(define (pop targ)	(list 'pop targ))
(define (sete targ)	(list 'sete targ))
(define (call targ)	(list 'call targ))

(define (tail-indirect-cpr-call)
  ;;Fetch a  binary code address  from the closure object  referenced by
  ;;the CPR (Closure Pointer Register) and jump directly there.
  ;;
  (jmp  (mem off-closure-code CP-REGISTER)))

(define (indirect-cpr-call)
  ;;Fetch a  binary code address  from the closure object  referenced by
  ;;the CPR (Closure Pointer Register) and perform a call to there.
  ;;
  (call (mem off-closure-code CP-REGISTER)))

(define (negl targ)	(list 'negl targ))
(define (label x)	(list 'label x))
(define (label-address x) (list 'label-address x))
(define (ret)		'(ret))
(define (cltd)		'(cltd))
(define (cmpl arg1 arg2) (list 'cmpl arg1 arg2))
(define (je label)	(list 'je label))
(define (jne label)	(list 'jne label))
(define (jle label)	(list 'jle label))
(define (jge label)	(list 'jge label))
(define (jg label)	(list 'jg label))
(define (jl label)	(list 'jl label))
(define (jb label)	(list 'jb label))
(define (ja label)	(list 'ja label))
(define (jo label)	(list 'jo label))
(define (jmp label)	(list 'jmp label))

(define (argc-convention n)
  ;;At run  time: the  number of  arguments in a  function call  is represented  by a
  ;;negative fixnum  which is  the number  of arguments negated.   Example: -2  <-> 2
  ;;arguments.
  ;;
  (fx- 0 (fxsll n fx-shift)))


;;;; function-call table
;;
;;Whenever  a "call"  assembly  instruction  is generated:  the  compiler, in  truth,
;;generates this sequence:
;;
;;     jmp L0
;;     livemask-bytes		;array of bytes             |
;;     framesize		;data word, a "long"        | call
;;     rp_offset		;data word, a fixnum        | table
;;     multi-value-rp		;data word, assembly label  |
;;     pad-bytes
;;   L0:
;;     call function-address
;;   single-value-rp:		;single value return point
;;     ... instructions...
;;   multi-value-rp:		;multi value return point
;;     ... instructions...
;;
;;and remember that the  "call" pushes on the stack the return  address, which is the
;;label SINGLE-VALUE-RP.
;;
;;If the  called function wants to  return a single argument:  it can just put  it in
;;AA-REGISTER and perform a "ret"; this will make the execution flow jump back to the
;;entry point SINGLE-VALUE-RP.
;;
;;If the called  function wants to return  zero or 2 or more  arguments: it retrieves
;;the  address SINGLE-VALUE-RP  from  the  stack, adds  to  it DISP-MULTIVALUE-RP  as
;;defined below  and it  obtains the  address MULTI-VALUE-RP,  then performs  a "jmp"
;;directly to MULTI-VALUE-RP.

;;Refer  to  the picture  in  src/ikarus-collect.c  for details  on  how
;;call-frames are laid out (search for livemask).
;;
(define-constant CALL-INSTRUCTION-SIZE
  (boot.case-word-size
   ((32) 5)
   ((64) 10)))

;;; The following are "displacements" from the address SINGLE-VALUE-RP.

;;Commented out because unused.
;;
;;(define-constant disp-frame-size
;;  (- (+ CALL-INSTRUCTION-SIZE (* 3 wordsize))))

;;Commented out because unused.
;;
;;(define-constant disp-frame-offset
;;  (- (+ CALL-INSTRUCTION-SIZE (* 2 wordsize))))

;;Multivalue return point.
;;
(define-constant DISP-MULTIVALUE-RP
  (- (+ CALL-INSTRUCTION-SIZE (* 1 wordsize))))

(define (compile-call-table frame-words-count livemask-vec multiarg-rp call-sequence)
  ;;To generate a call to a Scheme function,  we need to follow both the protocol for
  ;;handling multiple  return values, and  the protocol to expose  informations about
  ;;the caller's stack frame for garbage collection purposes.
  ;;
  ;;This  means generating  the following  "calling" chunk  of pseudo-assembly  to be
  ;;included in the body of the caller function:
  ;;
  ;;     jmp L0
  ;;     livemask-bytes		;array of bytes            --
  ;;     framesize		;data word, a "long"       .
  ;;     rp_offset		;data word, a fixnum       . call table
  ;;     multi-value-rp		;data word, assembly label .
  ;;     pad-bytes                                         --
  ;;   L0:
  ;;     call function-address
  ;;   single-value-rp:		;single value return point
  ;;     ... instructions...
  ;;   multi-value-rp:		;multi value return point
  ;;     ... instructions...
  ;;
  ;;and remember that the "call" pushes on the stack the return address, which is the
  ;;label SINGLE-VALUE-RP.
  ;;
  ;;If the callee function  returns a single value: it puts it in  EAX and performs a
  ;;"ret";  this  will  make  the  execution  flow  jump  back  to  the  entry  point
  ;;SINGLE-VALUE-RP.
  ;;
  ;;If the callee function wants to return  zero or 2 or more arguments: it retrieves
  ;;the  address   SINGLE-VALUE-RP  from   the  stack,  adds   to  it   the  constant
  ;;DISP-MULTIVALUE-RP obtaining the address MULTI-VALUE-RP, then it performs a "jmp"
  ;;directly to MULTI-VALUE-RP.
  ;;
  ;;The argument  FRAME-WORDS-COUNT must  be a  non-negative fixnum  representing the
  ;;number of words on the stack frame of the caller function.
  ;;
  ;;The  argument LIVEMASK-VEC  must  be a  vector of  fixnums  each representing  an
  ;;octect; the bits in the octets are live  flags for the machine words on the stack
  ;;of the caller.  See the garbage collector for details.
  ;;
  ;;MULTIARG-RP must be  a symbolic expression representing the address  of the multi
  ;;value return point:  the assembly label MULTI-VALUE-RP in  the pseudo-code above.
  ;;This label must be implemented in the assembly code generated for the caller.
  ;;
  ;;CALL-SEQUENCE must be a symbolic expression representing the assembly code needed
  ;;to actually call the closure object.  Examples of this argument are:
  ;;
  ;;(call %ebx)
  ;;	Call the address in %ebx.
  ;;
  ;;(call (label ?target))
  ;;	Call a label generated at compile time.
  ;;
  ;;(call (disp off-closure-code CPR))
  ;;	Call the closure object referenced by the Closure Pointer Register (CPR).
  ;;
  ;;(call (disp EAX EBX))
  ;;	Call the entry point at offset EAX from the address in EBX.
  ;;
  ;;When  the  execution flow  arrives  on  the calling  chunk  of  code: the  Scheme
  ;;arguments for  the closure to  call are already on  the stack; the  Frame Pointer
  ;;Register  (FPR) references  the uplevel  return  address.  The  situation on  the
  ;;Scheme stack is as follows:
  ;;
  ;;* When FRAME-WORDS-COUNT is 2 or more:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    | <-- FPR .
  ;;   |---------------------------|         --
  ;;   | uplevel Scheme argument 0 |         .
  ;;   |---------------------------|         . stack frame described
  ;;   | uplevel Scheme argument 1 |         . by the call table
  ;;   |---------------------------|         .
  ;;   |        empty word         |         .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;   in this  picture FRAME-WORDS-COUNT  is  3, counting:  return address,  uplevel
  ;;  Scheme argument 0, uplevel Scheme argument 1.
  ;;
  ;;  Before executing the "call" assembly instruction:  we need to adjust the FPR so
  ;;  that it references the machine word right above the "empty word":
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   | uplevel Scheme argument 0 |         .
  ;;   |---------------------------|         .
  ;;   | uplevel Scheme argument 1 | <- FPR  . stack frame described
  ;;   |---------------------------|         . by the call table
  ;;   |        empty word         |         .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;  so that right after the "call" the stack looks like this:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   | uplevel Scheme argument 0 |         .
  ;;   |---------------------------|         .
  ;;   | uplevel Scheme argument 1 |         . stack frame described
  ;;   |---------------------------|         . by the call table
  ;;   |       return address      | <- FPR  .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;  whenever the execution flow returns here we will have to adjust back the FPR so
  ;;  that it again references the "uplevel return address".
  ;;
  ;;* When FRAME-WORDS-COUNT is 1:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    | <- FPR  .
  ;;   |---------------------------|         --
  ;;   |        empty word         |         .  stack frame described
  ;;   |---------------------------|         -- by the call table
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;  only the return address is the frame;  we do not need to adjust the FPR, rather
  ;;  we just do the "call" so that right after it the stack looks like this:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   |        empty word         | <- FPR  .  stack frame described
  ;;   |---------------------------|         -- by the call table
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;* When FRAME-WORDS-COUNT is 0:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   |   uplevel Scheme object   |         .
  ;;   |---------------------------|         .
  ;;                ...                      .
  ;;   |---------------------------|         . stack frame described
  ;;   |   uplevel Scheme object   |         . by the call table
  ;;   |---------------------------|         .
  ;;   |    runtime frame size     | <- FPR  .
  ;;   |---------------------------|         .
  ;;   |        empty word         |         .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  ;;  an unspecified  number of Scheme objects  is on the stack and  FPR references a
  ;;  fixnum  representing the  negated number  of such words;  the number  of Scheme
  ;;  objects  on the stack is  not known at compile  time, rather it is  computed at
  ;;  runtime.  We just need to perform  a "call" instruction, so that right after it
  ;;  the stack looks like this:
  ;;
  ;;           high memory
  ;;   |                           |         --
  ;;               ...                       .
  ;;   |---------------------------|         . uplevel stack frame
  ;;   | uplevel return address    |         .
  ;;   |---------------------------|         --
  ;;   |   uplevel Scheme object   |         .
  ;;   |---------------------------|         .
  ;;                ...                      .
  ;;   |---------------------------|         . stack frame described
  ;;   |   uplevel Scheme object   |         . by the call table
  ;;   |---------------------------|         .
  ;;   |    runtime frame size     | <- FPR  .
  ;;   |---------------------------|         .
  ;;   |        empty word         |         .
  ;;   |---------------------------|         --
  ;;   |      Scheme argument 0    |
  ;;   |---------------------------|
  ;;   |      Scheme argument 1    |
  ;;   |---------------------------|
  ;;   |                           |
  ;;           low memory
  ;;
  (let ((L_CALL (label (gensym "call_label"))))
    (define %adjust-frame-pointer-register
      (let ((FPR-DELTA (if (or (fxzero? frame-words-count)
			       (fx=? frame-words-count 1))
			   #f
			 (fx* (fxsub1 frame-words-count) wordsize))))
	(lambda (asm-instr)
	  (if FPR-DELTA
	      (list asm-instr FPR-DELTA FP-REGISTER)
	    ;;NOP generates no assembly code.
	    '(nop)))))
    (list 'seq
	  (%adjust-frame-pointer-register 'subl)
	  (jmp L_CALL)
	  `(byte-vector ,livemask-vec)
	  `(int ,(* frame-words-count wordsize))
	  '(current-frame-offset)
	  multiarg-rp
	  `(pad ,CALL-INSTRUCTION-SIZE ,L_CALL ,call-sequence)
	  (%adjust-frame-pointer-register 'addl))))


;;;; done

#| end of module: INTEL-ASSEMBLY-CODE-GENERATION |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'assemble-sources		'scheme-indent-function 1)
;; eval: (put 'make-conditional		'scheme-indent-function 2)
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; End:
