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


(module (flatten-codes)
  ;;This module  converts a  struct instance of  type CODES into  a list  of assembly
  ;;language instructions, all inclusive.  Return a  list of lists with the following
  ;;format:
  ;;
  ;;   (?asm-list-for-body ?asm-list-for-clambda ...)
  ;;
  ;;for each sublist in the returned list a code object will be created.
  ;;
  ;;This operation is called "flattening" because the assembly directives like "int+"
  ;;are expanded into actual assembly instructions for the underlying CPU.
  ;;
  ;;Accept as input recordized code containing the following struct types:
  ;;
  ;;   asm-instr	conditional	constant
  ;;   ntcall		primcall	seq
  ;;   shortcut
  ;;
  ;;only the following types are accepted in tail position:
  ;;
  ;;   conditional
  ;;   primcall
  ;;   seq
  ;;   shortcut
  ;;
  ;;Error handling routines
  ;;-----------------------
  ;;
  ;;The input of this module X has is  composed of a set of CLAMBDA and an expression
  ;;BODY; each of them is converted into assembly code.
  ;;
  ;;Each block  of assembly code must  have a way to  report errors; this is  done by
  ;;appending at the end of the main  assembly routine a sequence of subroutines each
  ;;handling a kind of error:
  ;;
  ;;  main_label:
  ;;    ... main routine instructions ...
  ;;  error_1:
  ;;    ... error handler instructions ...
  ;;  error_2:
  ;;    ... error handler instructions ...
  ;;
  ;;The main routine is built by  traversing the input depth-first and accumulating a
  ;;list  of  assembly  instructions;  the   error  handlers  are  appended  to  such
  ;;accumulated list by  storing a reference to  the last pair in the  main list into
  ;;the parameter EXCEPTIONS-CONCATENATION  and prepending to the  tail error handler
  ;;routines as follows:
  ;;
  ;;  (let ((tail-pair (exceptions-concatenation)))
  ;;    (set-cdr! tail-pair (append ?error-handler-instructions
  ;;                                (cdr tail-pair))))
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   asm-instr	code-loc	conditional
  ;;   constant		disp		foreign-label
  ;;   fvar		ntcall		object
  ;;   primcall		seq		shortcut
  ;;
  ;;in addition CLOSURE-MAKER structs can appear in side CONSTANT structs.
  ;;
  (import INTEL-ASSEMBLY-DEFINITIONS)


;;;; helpers

(define-syntax __module_who__
  (identifier-syntax 'flatten-codes))

(define-constant exceptions-concatenation
  (make-parameter #f))

(define-constant exception-label
  (make-parameter #f))

(define (flatten-codes x)
  (Program x))


;;;; processing programs

(module (Program)

  (define (Program x)
    ;;Flatten  the struct  instance X  of type  CODES into  a list  of
    ;;assembly instructions:  the BODY expressions first,  the CLAMBDA
    ;;implementations last.  Return a list of lists with the following
    ;;format:
    ;;
    ;;   (?asm-list-for-body ?asm-list-for-clambda ...)
    ;;
    ;;for each  sublist in  the returned  list a  code object  will be
    ;;created.
    ;;
    ;;The ?ASM-LIST-FOR-BODY has the following format:
    ;;
    ;;   (?number-of-free-vars
    ;;    (label ?body-entry-point)
    ;;    ?asm-instr
    ;;    ...)
    ;;
    ;;where the ?NUMBER-OF-FREE-VARS is always zero.
    ;;
    (struct-case x
      ((codes x.clambda* x.body)
       (cons (cons* 0 (label (gensym "main_label"))
		    (let ((accum (list '(nop))))
		      (parameterize ((exceptions-concatenation accum))
			(T x.body accum))))
	     (map Clambda x.clambda*)))))

  (define (Clambda x)
    ;;Flatten the the  struct instance X of type CLAMBDA,  using a new
    ;;error handler routines tail, as follows:
    ;;
    ;;  (?number-of-free-vars ?annotation
    ;;   (label ?clambda-entry-point)
    ;;   ?asm-instr
    ;;   ...
    ;;   (jmp (label SL_invalid_args))
    ;;   ?handler-asm-instr
    ;;   ...)
    ;;
    ;;where:
    ;;
    ;;* ?ASM-INSTR  are assembly instructions that  select the CLAMBDA
    ;;  case with  the correct number of arguments and  run the actual
    ;;  function code.
    ;;
    ;;*  ?HANDLER-ASM-INSTR  are  assembly  instructions  implementing
    ;;  error handler routines.
    ;;
    ;;If a  CLAMBDA case with the  correct number of arguments  is not
    ;;found: the execution jumps to  the default routine to handle the
    ;;error "wrong number of arguments".
    ;;
    (struct-case x
      ((clambda L case* cp freevar* name)
       (cons* (length freevar*)
	      `(name ,name)
	      (label L)
	      (let ((accum (list '(nop))))
		(parameterize ((exceptions-concatenation accum))
		  (let recur ((case* case*))
		    (if (pair? case*)
			(ClambdaCase (car case*)
				     (recur (cdr case*)))
		      (cons `(jmp (label ,(sl-invalid-args-label))) accum)))))))))

  (define (ClambdaCase x accum)
    ;;Flatten the struct instance of  type CLAMBDA-CASE into a list of
    ;;assembly instructions  prepended to  the accumulator  ACCUM; the
    ;;error  handler  routines are  prepended  to  the tail  of  ACCUM
    ;;referenced by EXCEPTIONS-CONCATENATION.
    ;;
    ;;The generated assembly code must  check if this CLAMBDA case has
    ;;a specification  of requested  arguments matching  the arguments
    ;;given to the CLAMBDA function application; when arriving to this
    ;;code: ARGC-REGISTER  contains a fixnum being  the encoded number
    ;;of given arguments.
    ;;
    ;;For  a CLAMBDA  case with  fixed number  of requested  arguments
    ;;(that is: the  formals are a proper list), we  must check if the
    ;;number  of  requested  arguments  equals  the  number  of  given
    ;;arguments, else we jump to the next case.  The returned list has
    ;;the format:
    ;;
    ;;   ((cmpl ?this-case-number-of-args ARGC-REGISTER)
    ;;    (jne ?next-case-entry-point-label)
    ;;    (label ?case-entry-point)
    ;;    ?case-asm-instr
    ;;    ...
    ;;    (label ?next-case-entry-point-label)
    ;;    . ACCUM)
    ;;
    ;;
    ;;For a CLAMBDA  case with variable number  of requested arguments
    ;;(that is:  the formals are an  improper list), we must  check if
    ;;the number  of requested  mandatory arguments  is less  than the
    ;;number  of given  arguments,  else  we jump  to  the next  case;
    ;;remember  that  the comparison  is  between  encoded numbers  of
    ;;arguments.  The returned list has the format:
    ;;
    ;;   ((cmpl ?this-case-number-of-mandatory-args ARGC-REGISTER)
    ;;    (jg ?next-case-entry-point-label)
    ;;    (label ?case-entry-point)
    ;;    ?case-asm-instr
    ;;    ...
    ;;    (label ?next-case-entry-point-label)
    ;;    . ACCUM)
    ;;
    (struct-case x
      ((clambda-case x.info x.body)
       (struct-case x.info
	 ((case-info x.info.case-entry-point-label x.info.args x.info.proper?)
	  ;;Here  X.INFO.ARGS  is  a  pair   whose  car  is  a  symbol
	  ;;representing the  CPU register holding the  pointer to the
	  ;;current  closure object,  and  whose cdr  is  the list  of
	  ;;properized formals.
	  (let ((next-case-entry-point-label (unique-label "L_clambda_branch")))
	    (cons* `(cmpl ,(argc-convention (if x.info.proper?
						(length (cdr x.info.args))
					      (length (cddr x.info.args))))
			  ,ARGC-REGISTER)
		   (cond (x.info.proper?
			  `(jne ,next-case-entry-point-label))
			 ((> (argc-convention 0)
			     (argc-convention 1))
			  `(jg ,next-case-entry-point-label))
			 (else
			  `(jl ,next-case-entry-point-label)))
		   (let ((accum^ (cons (label x.info.case-entry-point-label)
				       (T x.body
					  (cons next-case-entry-point-label accum)))))
		     (if x.info.proper?
			 accum^
		       (%handle-vararg (length (cdr x.info.args)) accum^))))))))))

  (define (%handle-vararg properized-formals-count accum)
    ;;Generate the assembly code needed to handle the application of a
    ;;CLAMBDA  case accepting  a  variable number  of arguments.   The
    ;;generated code goes in the body of the callee function.
    ;;
    ;;PROPERIZED-FORMALS-COUNT is a fixnum  representing the number of
    ;;arguments, including the rest argument.  For the function:
    ;;
    ;;   (lambda (a b c . rest) . ?body)
    ;;
    ;;this argument is 4.
    ;;
    ;;ACCUM is a  list of assembly instructions  representing the body
    ;;of this CLAMBDA case.
    ;;
    ;;Let's say we want to call the function:
    ;;
    ;;   (define (the-func arg . rest) . ?body)
    ;;   (the-func 1 2 3)
    ;;
    ;;this is  the Scheme language,  so the  caller does not  know how
    ;;THE-FUNC  will  handle  its  arguments:  it  can  only  put  the
    ;;arguments  on  the  Scheme  stack.   Right  after  the  assembly
    ;;instruction "call" to THE-FUNC  has been executed: ARGC-REGISTER
    ;;is  set  to the  fixnum  -3,  which  is  the negated  number  of
    ;;arguments, and the Scheme stack is:
    ;;
    ;;     high memory
    ;; |                |
    ;; |----------------|
    ;; | return address | <-- Frame Pointer Register (FPR)
    ;; |----------------|
    ;; |    fixnum 1    | <-- FPR - 1 * wordsize
    ;; |----------------|
    ;; |    fixnum 2    | <-- FPR - 2 * wordsize
    ;; |----------------|
    ;; |    fixnum 3    | <-- FPR - 3 * wordsize = FPR + ARGC-REGISTER
    ;; |----------------|
    ;; |                |
    ;;     low memory
    ;;
    ;;The fixnum 1 is right where we need it; the fixnums 2 and 3 must
    ;;be put into a proper list.  In the body of THE-FUNC we need code
    ;;that sets the stack to:
    ;;
    ;;     high memory
    ;; |                |
    ;; |----------------|
    ;; | return address | <-- Frame Pointer Register (FPR)
    ;; |----------------|
    ;; |    fixnum 1    |
    ;; |----------------|
    ;; | pair reference | --> (2 3)
    ;; |----------------|
    ;; |    fixnum 3    |
    ;; |----------------|
    ;; |                |
    ;;     low memory
    ;;
    ;;knowing  that  "fixnum  3"  will  be ignored  and  the  list  is
    ;;allocated on the heap.
    ;;
    (define CONTINUE_LABEL	(unique-label "L_varargs_continue_"))
    (define DONE_LABEL	(unique-label "L_varargs_done"))
    (define CONS_LABEL	(unique-label "L_varargs_cons"))
    (define LOOP_HEAD		(unique-label "L_varargs_loop_head"))
    (define mandatory-formals-count
      (fxsub1 properized-formals-count))
    (define properized-formals-argc
      (argc-convention properized-formals-count))
    (cons*
     ;;Check if there are rest arguments to put into a list.  We could
     ;;check if:
     ;;
     ;;  (= (argc-convention properized-formals-count) ARGC-REGISTER)
     ;;
     ;;and jump to CONS_LABEL if they are not equal (jne).  Instead we
     ;;do:
     (cmpl (int (argc-convention mandatory-formals-count)) ARGC-REGISTER)
     (jl CONS_LABEL)

     ;;There are no rest arguments:  the function has been called with
     ;;enough argument to match the mandatory arguments, as in:
     ;;
     ;;   (define (the-func arg . rest) . ?body)
     ;;   (the-func 1)
     ;;
     ;;the REST binding will be set to nil.
     (movl (int nil) ebx)
     (jmp DONE_LABEL)

     ;;Check that  there is enough  room on  the heap to  allocate the
     ;;list of rest arguments; the amount  of words needed to hold the
     ;;list is twice the number of rest arguments.
     CONS_LABEL
     (movl (mem pcb-allocation-redline pcr) ebx)
     (addl ARGC-REGISTER ebx)
     (addl ARGC-REGISTER ebx)
     (cmpl ebx apr)
     (jle LOOP_HEAD)

     ;;If we are here: there is not  enough room on the heap; call the
     ;;primitive  function  DO-VARARG-OVERFLOW  to allocate  new  heap
     ;;space.
     ;;
     ;;Advance FPR to step over the plain arguments on the stack.
     (addl ARGC-REGISTER fpr)
     (pushl cpr)
     (pushl ARGC-REGISTER)
     ;;Make argc positive.
     (negl ARGC-REGISTER)
     ;;Add 4 words to adjust frame size (see the picture below).
     (addl (int (fx* +4 wordsize)) ARGC-REGISTER)
     ;;Push the frame size.
     (pushl ARGC-REGISTER)
     ;;Undo adding 4 words.
     ;;
     ;;NOTE In the original Ikarus code  the number of bytes needed on
     ;;the  heap   for  the  rest   list  was  computed   by  doubling
     ;;ARGC-REGISTER augmented  with 4 word sizes;  this was reserving
     ;;extra space on the heap.  We  avoid it here.  (Marco Maggi; Mar
     ;;26, 2013)
     (addl (int (fx* -4 wordsize)) ARGC-REGISTER)
     ;;Double the  number of arguments  obtaining the number  of bytes
     ;;needed on the heap ...
     (addl ARGC-REGISTER ARGC-REGISTER)
     ;;... pass it as first argument to DO-VARARG-OVERFLOW.
     (movl ARGC-REGISTER (mem (fx* -2 wordsize) fpr))
     ;;DO-VARARG-OVERFLOW is called with one argument.
     (movl (int (argc-convention 1)) ARGC-REGISTER)
     ;;From the  relocation vector of  this code object:  retrieve the
     ;;location gensym associated to DO-VARARG-OVERFLOW and load it in
     ;;the Closure  Pointer Register (CPR).   The "proc" slot  of such
     ;;loc  gensym   contains  a  reference  to   the  closure  object
     ;;implementing DO-VARARG-OVERFLOW.
     (movl (obj (primitive-public-function-name->location-gensym 'do-vararg-overflow)) cpr)
     ;;Load in the Closure Pointer Register a reference to the closure
     ;;object implementing DO-VARARG-OVERFLOW.
     (movl (mem off-symbol-record-proc cpr) cpr)
     ;;When arriving here the Scheme stack is as follows:
     ;;
     ;;       high memory
     ;;   |                |
     ;;   |----------------|
     ;;   | return address |
     ;;   |----------------|          --
     ;;   |    fixnum 1    |          .
     ;;   |----------------|          .
     ;;   |    fixnum 2    |          . stack frame size for the
     ;;   |----------------|          . call to DO-VARARG-OVERFLOW
     ;;   |    fixnum 3    |          .
     ;;   |----------------|          .  --
     ;;   |  closure ref   |          .  .
     ;;   |----------------|          .  .
     ;;   |    arg count   |          .  . 4 words needed to
     ;;   |----------------|          .  . prepare the call
     ;;   |   framesize    | <-- FPR  .  . to DO-VARARG-OVERFLOW
     ;;   |----------------|          .  .
     ;;   |   empty word   |          .  .
     ;;   |----------------|          -- --
     ;;   |needed heap room| <- argument for DO-VARARG-OVERFLOW
     ;;   |----------------|
     ;;   |                |
     ;;       low memory
     ;;
     ;;the empty  machine word is the  one in which "call"  will store
     ;;the return address.
     ;;
     (compile-call-table 0	    ;frame words count
			 '#()	    ;livemask
			 '(int 0) ;multivalue return point, NULL because unused
			 (indirect-cpr-call))
     ;;Pop framesize and drop it.
     (popl ARGC-REGISTER)
     ;;Reload number of arguments for this CLAMBDA case.
     (popl ARGC-REGISTER)
     ;;Reload pointer to current closure object.
     (popl cpr)
     ;;Re-adjust  the  frame  pointer  to step  back  over  the  plain
     ;;arguments on the stack.
     (subl ARGC-REGISTER fpr)

     ;;There is enough room on the heap to allocate the rest list.  We
     ;;allocate it backwards, the list (2 3) is laid out as:
     ;;
     ;;          ---- growing heap ---->
     ;;
     ;;        tail pair         head pair
     ;;   |.................|.................|
     ;;
     ;;    fixnum 3    nil   fixnum 2 pair ref
     ;;   |--------|--------|--------|--------| heap
     ;;       ^                          |
     ;;       |                          |
     ;;        --------------------------
     ;;
     LOOP_HEAD
     (movl (int nil) ebx)

     CONTINUE_LABEL
     (movl ebx (mem disp-cdr apr))	   ;store the cdr
     (movl (mem fpr ARGC-REGISTER) ebx)  ;load the next car value
     (movl ebx (mem disp-car apr))	   ;store the car value
     (movl apr ebx)			   ;load the allocation pointer
     (addl (int pair-tag) ebx)	   ;tag the pointer as reference to pair
     (addl (int pair-size) apr)	   ;increment the allocation pointer
     (addl (int wordsize) ARGC-REGISTER) ;increment the negative arguments count
     ;;Loop if more arguments.
     (cmpl (int properized-formals-argc) ARGC-REGISTER)
     (jle CONTINUE_LABEL)

     DONE_LABEL
     ;;Store nil or the reference to the rest list on the stack, right
     ;;below the  last mandatory argument (overwriting  the first rest
     ;;argument).
     (movl ebx (mem properized-formals-argc fpr))
     accum))

  #| end of module: Program |# )


;;;;

(define (T x accum)
  ;;Flatten the struct instance X, representing recordized code, as if
  ;;it  is  in  tail  position  in some  enclosing  form;  return  the
  ;;accumulated list of assembly instructions having ACCUM as tail.
  ;;
  ;;ACCUM must  be the list  of assembly instructions,  accumulated so
  ;;far, that must be included in  binary code after the ones to which
  ;;X will expand.
  ;;
  (struct-case x
    ((seq e0 e1)
     (E e0 (T e1 accum)))

    ((conditional x.test x.conseq x.altern)
     (let ((label-for-true-predicate  #f)
	   (label-for-false-predicate (unique-label "L_false")))
       (P x.test label-for-true-predicate label-for-false-predicate
	  (T x.conseq
	     (cons label-for-false-predicate
		   (T x.altern accum))))))

    ((primcall op rands)
     (case op
       ((return)
	(cons '(ret) accum))

       ((indirect-jump)
	;;The CPU's Closure Pointer  Register (CP-REGISTER) contains a
	;;reference to the closure object we want to jump to.
	(cons `(jmp (disp ,off-closure-code ,CP-REGISTER))
	      accum))

       ((direct-jump)
	;;We jump directly to a  known address, available as immediate
	;;value.
	(cons `(jmp (label ,(code-loc-label (car rands))))
	      accum))

       (else
	(error __module_who__ "invalid tail" x))))

    ((shortcut body handler)
     ;;Flatten the body instructions:
     ;;
     ;;  (body-asm)
     ;;  ...
     ;;
     ;;and  prepend to  the exception  routines the  flattened handler
     ;;instructions:
     ;;
     ;;  (label L)
     ;;  (handler-asm)
     ;;  ...
     ;;
     (let ((L (unique-interrupt-label)))
       (let* ((handler^ (cons L (T handler '())))
	      (tc       (exceptions-concatenation)))
	 (set-cdr! tc (append handler^ (cdr tc))))
       (parameterize ((exception-label L))
	 (T body accum))))

    (else
     (error __module_who__ "invalid tail" x))))

;;; --------------------------------------------------------------------

(module (E)

  (define (E x accum)
    ;;Flatten X for side effects.
    ;;
    ;;X must be a struct instance representing recordized code.
    ;;
    ;;ACCUM must be the list  of assembly instructions, accumulated so
    ;;far, that  must be  included in  binary code  after the  ones to
    ;;which X will expand.
    ;;
    (struct-case x
      ((seq e0 e1)
       (E e0 (E e1 accum)))

      ((conditional x.test x.conseq x.altern)
       (cond ((interrupt? x.conseq)
	      (let ((label-true  (or (exception-label)
				     (error __module_who__ "no exception label")))
		    (label-false #f))
		(P x.test label-true label-false
		   (E x.altern accum))))
	     ((interrupt? x.altern)
	      (let ((label-true  #f)
		    (label-false (or (exception-label)
				     (error __module_who__ "no exception label"))))
		(P x.test label-true label-false
		   (E x.conseq accum))))
	     (else
	      ;;For  this conditional  case  we generate  code as  the
	      ;;following pseudo-code shows:
	      ;;
	      ;;     x.test
	      ;;   jump-if-false label_false
	      ;;     x.conseq
	      ;;     jmp label_end
	      ;;   label_false:
	      ;;     x.altern
	      ;;   label_end:
	      ;;     accum
	      ;;
	      (let ((label-true  #f)
		    (label-false (unique-label "L_false"))
		    (label-end   (unique-label "L_end")))
		(P x.test label-true label-false
		   (E x.conseq (cons* `(jmp ,label-end)
				      label-false
				      (E x.altern (cons label-end accum)))))))))

      ((ntcall target value args mask size)
       (E-ntcall target value args mask size accum))

      ((asm-instr op d s)
       (E-asm-instr op d s x accum))

      ((primcall op rands)
       (E-primcall op rands x accum))

      ((shortcut body handler)
       ;;Flatten the body instructions inserting a label at the end:
       ;;
       ;;  (body-asm)
       ;;  ...
       ;;  (jmp L_interrupt)
       ;;  ...
       ;;  (label L_return_from_interrupt)
       ;;
       ;;and  prepend to  the exception  routines the  flattened handler
       ;;instructions:
       ;;
       ;;  (label L_interrupt)
       ;;  (handler-asm)
       ;;  ...
       ;;  (jmp L_return_from_interrupt)
       ;;
       (let ((L_interrupt (unique-interrupt-label))
	     (L_return    (unique-label "L_return_from_interrupt")))
	 (let* ((handler^ (cons L_interrupt (E handler `((jmp ,L_return)))))
		(tc       (exceptions-concatenation)))
	   (set-cdr! tc (append handler^ (cdr tc))))
	 (parameterize ((exception-label L_interrupt))
	   (E body (cons L_return accum)))))

      (else
       (error __module_who__ "invalid effect" (unparse-recordized-code x)))))

  (define (E-ntcall target value args mask frame-words-count accum)
    ;;Flatten a  non-tail call;  this is  the call  making use  of the
    ;;"call" assembly instruction.
    ;;
    (define (%call-chunk call-sequence)
      (compile-call-table frame-words-count mask
			  ;;Select the multivalue return point label.
			  (if value
			      (label-address (sl-mv-error-rp-label))
			    (label-address (sl-mv-ignore-rp-label)))
			  call-sequence))
    (cond ((string? target) ;foreign call
	   (cons* `(movl (foreign-label "ik_foreign_call") %ebx)
		  (%call-chunk `(call %ebx))
		  accum))
	  (target ;known call
	   (cons (%call-chunk `(call (label ,target)))
		 accum))
	  (else ;call to closure object
	   (cons (%call-chunk `(call (disp ,off-closure-code ,CP-REGISTER)))
		 accum))))

  (define (E-asm-instr op d s x accum)
    (case op
      ((logand)
       (cons `(andl ,(R s) ,(R d)) accum))

      ((int+)
       (cons `(addl ,(R s) ,(R d)) accum))

      ((int*)
       (cons `(imull ,(R s) ,(R d)) accum))

      ((int-)
       (cons `(subl ,(R s) ,(R d)) accum))

      ((logor)
       (cons `(orl ,(R s) ,(R d)) accum))

      ((logxor)
       (cons `(xorl ,(R s) ,(R d)) accum))

      ((mset)
       (cons `(movl ,(R s) ,(R d)) accum))

      ((move)
       (if (eq? d s)
	   accum
	 (cons `(movl ,(R s) ,(R d)) accum)))

      ((load8)
       (if (eq? d s)
	   accum
	 (cons `(movb ,(R/l s) ,(R/l d)) accum)))

      ((bset)
       (cons `(movb ,(R/l s) ,(R d)) accum))

      ((sll)
       (cons `(sall ,(R/cl s) ,(R d)) accum))

      ((sra)
       (cons `(sarl ,(R/cl s) ,(R d)) accum))

      ((srl)
       (cons `(shrl ,(R/cl s) ,(R d)) accum))

      ((idiv)
       (cons `(idivl ,(R s)) accum))

      ((cltd)
       (cons `(cltd) accum))

      ((bswap!)
       (let ((s (R s))
	     (d (R d)))
	 (unless (eq? s d)
	   (error __module_who__ "invalid instr" (unparse-recordized-code x)))
	 (cons `(bswap ,s) accum)))

      ((mset32)
       (cons `(mov32 ,(R s) ,(R d)) accum))

      ((load32)
       (cons `(mov32 ,(R s) ,(R d)) accum))

      ((int-/overflow)
       (let ((L (or (exception-label)
		    (error __module_who__ "no exception label" (unparse-recordized-code x)))))
	 (cons* `(subl ,(R s) ,(R d))
		`(jo ,L)
		accum)))

      ((sll/overflow)
       (let ((L (or (exception-label)
		    (error __module_who__ "no exception label" (unparse-recordized-code x)))))
	 (cons* `(sall ,(R/cl s) ,(R d))
		`(jo ,L)
		accum)))

      ((int*/overflow)
       (let ((L (or (exception-label)
		    (error __module_who__ "no exception label" (unparse-recordized-code x)))))
	 (cons* `(imull ,(R s) ,(R d))
		`(jo ,L)
		accum)))

      ((int+/overflow)
       (let ((L (or (exception-label)
		    (error __module_who__ "no exception label" (unparse-recordized-code x)))))
	 (cons* `(addl ,(R s) ,(R d))
		`(jo ,L)
		accum)))

      ((fl:store)
       (cons `(movsd xmm0 ,(R (make-disp s d))) accum))

      ((fl:store-single)
       (cons `(movss xmm0 ,(R (make-disp s d))) accum))

      ((fl:load)
       (cons `(movsd ,(R (make-disp s d)) xmm0) accum))

      ((fl:load-single)
       (cons `(movss ,(R (make-disp s d)) xmm0) accum))

      ((fl:from-int)
       (cons `(cvtsi2sd ,(R s) xmm0) accum))

      ((fl:shuffle)
       (cons `(pshufb ,(R (make-disp s d)) xmm0) accum))

      ((fl:add!)
       (cons `(addsd ,(R (make-disp s d)) xmm0) accum))

      ((fl:sub!)
       (cons `(subsd ,(R (make-disp s d)) xmm0) accum))

      ((fl:mul!)
       (cons `(mulsd ,(R (make-disp s d)) xmm0) accum))

      ((fl:div!)
       (cons `(divsd ,(R (make-disp s d)) xmm0) accum))

      (else
       (error __module_who__ "invalid instr" (unparse-recordized-code x)))))

  (define (E-primcall op rands x accum)
    (case op
      ((nop)
       accum)

      ((interrupt)
       (let ((l (or (exception-label)
		    (error __module_who__ "no exception label" (unparse-recordized-code x)))))
	 (cons `(jmp ,l) accum)))

      ((incr/zero?)
       (let ((l (or (exception-label)
		    (error __module_who__ "no exception label" (unparse-recordized-code x)))))
	 (cons* `(addl ,(D (caddr rands)) ,(R (make-disp (car rands) (cadr rands))))
		`(je ,l)
		accum)))

      ((fl:double->single)
       (cons '(cvtsd2ss xmm0 xmm0) accum))

      ((fl:single->double)
       (cons '(cvtss2sd xmm0 xmm0) accum))

      (else
       (error __module_who__ "invalid effect" (unparse-recordized-code x)))))

  (define (interrupt? x)
    (struct-case x
      ((primcall op args)
       (eq? op 'interrupt))
      (else
       #f)))

  (define (R/cl x)
    (struct-case x
      ((constant i)
       (unless (fixnum? i)
	 (error __module_who__ "invalid R/cl" (unparse-recordized-code x)))
       (fxlogand i (- (* wordsize 8) 1)))
      (else
       (if (eq? x ecx)
	   '%cl
	 (error __module_who__ "invalid R/cl" (unparse-recordized-code x))))))

  #| end of module: E |# )

;;; --------------------------------------------------------------------

(define (unique-interrupt-label)
  (label (gensym "L_shortcut_interrupt_handler")))

(define unique-label
  (case-lambda
   (()
    (label (gensym)))
   ((name)
    (label (gensym name)))))

;;; --------------------------------------------------------------------

(module (P)

  (define (P x label-true label-false accum)
    ;;Flatten X as code in predicate position.
    ;;
    ;;X must be a struct instance representing recordized code.
    ;;
    ;;LABEL-TRUE must be the label entry  point for the code to be run
    ;;when X returns true.
    ;;
    ;;LABEL-FALSE must be the label entry point for the code to be run
    ;;when X returns false.
    ;;
    ;;ACCUM must be the list  of assembly instructions, accumulated so
    ;;far, that  must be  included in  binary code  after the  ones to
    ;;which X will expand.
    ;;
    (struct-case x
      ((constant c)
       (cond (c
	      (if label-true
		  (cons `(jmp ,label-true) accum)
		accum))
	     (label-false
	      (cons `(jmp ,label-false) accum))
	     (else
	      accum)))

      ((seq e0 e1)
       (E e0 (P e1 label-true label-false accum)))

      ((conditional x.test x.conseq x.altern)
       (P-conditional x.test x.conseq x.altern label-true label-false accum))

      ((asm-instr op a0 a1)
       (P-asm-instr op a0 a1 label-true label-false accum))

      ((shortcut body handler)
       (let ((L_interrupt (unique-interrupt-label))
	     (L_end       (unique-label "L_end")))
	 (let ((accum (if (and label-true label-false)
			  accum
			(cons L_end accum))))
	   (let* ((handler^ (cons L_interrupt (P handler (or label-true L_end) (or label-false L_end) '())))
		  (tc       (exceptions-concatenation)))
	     (set-cdr! tc (append handler^ (cdr tc))))
	   (parameterize ((exception-label L_interrupt))
	     (P body label-true label-false accum)))))

      (else
       (error __module_who__ "invalid pred" x))))

  (define (P-conditional x.test x.conseq x.altern label-true label-false accum)
    (cond ((and (constant=? x.conseq #t)
		(constant=? x.altern #f))
	   (P x.test label-true label-false accum))

	  ((and (constant=? x.conseq #f)
		(constant=? x.altern #t))
	   (P x.test label-false label-true accum))

	  ((and label-true label-false)
	   (let ((l (unique-label "L_false")))
	     (P x.test #f l (P x.conseq label-true label-false
			       (cons l (P x.altern label-true label-false accum))))))

	  (label-true
	   (let ((label-false (unique-label "L_false"))
		 (l           (unique-label "L_false")))
	     (P x.test #f l (P x.conseq label-true label-false
			       (cons l (P x.altern label-true #f (cons label-false accum)))))))

	  (label-false
	   (let ((label-true (unique-label "L_true"))
		 (l          (unique-label "L_false")))
	     (P x.test #f l (P x.conseq label-true label-false
			       (cons l (P x.altern #f label-false (cons label-true accum)))))))

	  (else
	   (let ((label-false (unique-label "L_false"))
		 (l           (unique-label "L_false")))
	     (P x.test #f l (P x.conseq #f #f
			       (cons `(jmp ,label-false)
				     (cons l (P x.altern #f #f (cons label-false accum))))))))))

  (module (P-asm-instr)

    (define (P-asm-instr op a0 a1 label-true label-false accum)
      (cond ((and label-true label-false)
	     (cmp op a0 a1 label-true (cons `(jmp ,label-false) accum)))
	    (label-true
	     (cmp op a0 a1 label-true accum))
	    (label-false
	     (cmp (%select-negated-P-asm-instr op) a0 a1 label-false accum))
	    (else
	     accum)))

    (define (cmp op a0 a1 lab accum)
      (cond ((memq op '(fl:= fl:!= fl:< fl:<= fl:> fl:>=))
	     (cons* `(ucomisd ,(R (make-disp a0 a1)) xmm0)
		    `(,(jmpname op) ,lab)
		    ;;BOGUS! (Abdulaziz Ghuloum)
		    accum))
	    ((memq op '(fl:o= fl:o!= fl:o< fl:o<= fl:o> fl:o>=))
	     (cons* `(ucomisd ,(R (make-disp a0 a1)) xmm0)
		    `(jp ,lab)
		    `(,(jmpname op) ,lab)
		    accum))
	    ((or (symbol? a0) (constant? a1))
	     (cons* `(cmpl ,(R a1) ,(R a0))
		    `(,(jmpname op) ,lab)
		    accum))
	    ((or (symbol? a1) (constant? a0))
	     (cons* `(cmpl ,(R a0) ,(R a1))
		    `(,(revjmpname op) ,lab)
		    accum))
	    (else
	     (error __module_who__ "invalid cmpops" a0 a1))))

    (define (%select-negated-P-asm-instr x)
      (cond ((assq x '((= !=) (!= =) (< >=) (<= >) (> <=) (>= <)
		       (u< u>=) (u<= u>) (u> u<=) (u>= u<)
		       (fl:= fl:o!=) (fl:!= fl:o=)
		       (fl:< fl:o>=) (fl:<= fl:o>)
		       (fl:> fl:o<=) (fl:>= fl:o<)))
	     => cadr)
	    (else
	     (error __module_who__ "assembly instruction invalid in predicate context" x))))

    (define (jmpname x)
      (cond ((assq x '((= je) (!= jne) (< jl) (<= jle) (> jg) (>= jge)
		       (u< jb) (u<= jbe) (u> ja) (u>= jae)
		       (fl:= je) (fl:!= jne)
		       (fl:< jb) (fl:> ja) (fl:<= jbe) (fl:>= jae)
		       (fl:o= je) (fl:o!= jne)
		       (fl:o< jb) (fl:o> ja) (fl:o<= jbe) (fl:o>= jae)))
	     => cadr)
	    (else
	     (error __module_who__ "invalid jmpname" x))))

    (define (revjmpname x)
      (cond ((assq x '((= je) (!= jne) (< jg) (<= jge) (> jl) (>= jle)
		       (u< ja) (u<= jae) (u> jb) (u>= jbe)))
	     => cadr)
	    (else
	     (error __module_who__ "invalid jmpname" x))))

    #| end of module: P-asm-instr |# )

  (define (constant=? x k)
    (struct-case x
      ((constant k0)
       (equal? k0 k))
      (else
       #f)))

  #| end of module: P |# )

;;; --------------------------------------------------------------------

(define (FVar i)
  ;;Convert the index  of an FVAR into a reference  to machine word on
  ;;the stack.
  ;;
  ;;       high memory
  ;;   |                |
  ;;   |----------------|
  ;;   | return address | <-- frame pointer register (FPR)
  ;;   |----------------|
  ;;   |                | <-- index 1
  ;;   |----------------|
  ;;   |                | <-- index 2
  ;;   |----------------|
  ;;   |                | <-- index 3
  ;;   |----------------|
  ;;   |                |
  ;;       low memory
  ;;
  `(disp ,(* i (- wordsize)) ,fpr))

(module (R R/l D)

  (define (R x)
    (struct-case x
      ((constant c)
       (%process-constant c))
      ((fvar i)
       (FVar i))
      ((disp s0 s1)
       (let ((s0 (D s0))
	     (s1 (D s1)))
	 `(disp ,s0 ,s1)))
      (else
       (if (symbol? x)
	   x
	 (error __module_who__ "invalid R" x)))))

  (module (R/l)

    (define (R/l x)
      (struct-case x
	((constant c)
	 (%process-constant c))
	((fvar i)
	 (FVar i))
	((disp s0 s1)
	 (let ((s0 (D s0))
	       (s1 (D s1)))
	   `(disp ,s0 ,s1)))
	(else
	 (if (symbol? x)
	     (reg/l x)
	   (error __module_who__ "invalid R/l" x)))))

    (define (reg/l x)
      (cond ((assq x '((%eax %al) (%ebx %bl) (%ecx %cl) (%edx %dl)
		       (%r8 %r8l) (%r9 %r9l) (%r10 %r10l) (%r11 %r11l)
		       (%r12 %r12l) (%r13 %r13l) (%r14 %r14l) (%r15 %r15l)))
	     => cadr)
	    (else
	     (error __module_who__ "invalid reg/l" x))))

    #| end of module: R/l |# )

  (define (D x)
    (struct-case x
      ((constant c)
       (%process-constant c))
      (else
       (if (symbol? x)
	   x
	 (error __module_who__ "invalid D" x)))))

  (define (%process-constant x)
    (struct-case x
      ((code-loc label)
       (label-address label))
      ((foreign-label L)
       `(foreign-label ,L))
      ((closure-maker code freevar*)
       (unless (null? freevar*)
	 (error __module_who__ "nonempty closure"))
       `(obj ,x))
      ((object o)
       `(obj ,o))
      (else
       (if (integer? x)
	   x
	 (error __module_who__ "invalid constant C" x)))))

  #| end of module |# )

;;Commented out because unused.  (Marco Maggi; Oct 29, 2012)
;;
;; (define (BYTE x)
;;   (struct-case x
;;     ((constant x)
;;      (unless (and (integer? x)
;; 		    (fx<= x +255)
;; 		    (fx>= x -128))
;;        (error __module_who__ "invalid byte" x))
;;      x)
;;     (else
;;      (error __module_who__ "invalid byte" x))))

;;Commented out because unused.  (Marco Maggi; Oct 29, 2012)
;;
;; (define (reg/h x)
;;   (cond ((assq x '((%eax %ah) (%ebx %bh) (%ecx %ch) (%edx %dh)))
;; 	   => cadr)
;; 	  (else
;; 	   (error __module_who__ "invalid reg/h" x))))


;;;; done

#| end of module: FLATTEN-CODES |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-primcall 'scheme-indent-function 1)
;; eval: (put 'assemble-sources 'scheme-indent-function 1)
;; eval: (put 'make-conditional 'scheme-indent-function 2)
;; eval: (put 'struct-case 'scheme-indent-function 1)
;; End:
