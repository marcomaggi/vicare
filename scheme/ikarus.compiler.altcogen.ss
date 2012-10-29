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


;;;; Introduction
;;
;;Input to cogen is <Program>:
;;
;;  <Expr> ::= (constant x)
;;           | (var)
;;           | (primref name)
;;           | (bind var* <Expr>* <Expr>)
;;           | (fix var* <FixRhs>* <Expr>)
;;           | (conditional <Expr> <Expr> <Expr>)
;;           | (seq <Expr> <Expr>)
;;           | (closure <codeloc> <var>*)  ; thunk special case
;;           | (forcall "name" <Expr>*)
;;           | (funcall <Expr> <Expr>*)
;;           | (jmpcall <label> <Expr> <Expr>*)
;;           | (mvcall <Expr> <clambda>)
;;  <codeloc> ::= (code-loc <label>)
;;  <clambda> ::= (clambda <label> <case>* <cp> <free var>*)
;;  <case>    ::= (clambda-case <info> <body>)
;;  <info>    ::= (clambda-info label <arg var>* proper)
;;  <Program> ::= (codes <clambda>* <Expr>)


(module (alt-cogen
	 compile-call-frame
	 alt-cogen.introduce-primcalls
	 alt-cogen.eliminate-fix
	 alt-cogen.insert-engine-checks
	 alt-cogen.insert-stack-overflow-check
	 alt-cogen.specify-representation
	 alt-cogen.impose-calling-convention/evaluation-order
	 alt-cogen.assign-frame-sizes
	 alt-cogen.color-by-chaitin
	 alt-cogen.flatten-codes)

  (define (alt-cogen x)
    ;;Commenting out this definition causes some passes to be timed.
    (define-inline (time-it name proc)
      (proc))
    (let* ((x (alt-cogen.introduce-primcalls x))
	   (x (alt-cogen.eliminate-fix x))
	   (x (alt-cogen.insert-engine-checks x))
	   (x (alt-cogen.insert-stack-overflow-check x))
	   (x (alt-cogen.specify-representation x))
	   (x (alt-cogen.impose-calling-convention/evaluation-order x))
	   (x (time-it "frame"    (lambda ()
				    (alt-cogen.assign-frame-sizes x))))
	   (x (time-it "register" (lambda ()
				    (alt-cogen.color-by-chaitin x))))
	   (ls (alt-cogen.flatten-codes x)))
      ls))


;;;; helpers

(define-syntax multiple-forms-sequence
  (syntax-rules ()
    ((_ ?e)
     ?e)
    ((_ ?e* ... ?e)
     (make-seq (multiple-forms-sequence ?e* ...) ?e))))

(define (print-code x)
  (parameterize ((print-gensym '#t))
    (pretty-print (unparse-recordized-code x))))


(module (alt-cogen.introduce-primcalls)
  ;;The purpose of this module is to examine all the function calls:
  ;;
  ;;   (?operator ?arg ...)
  ;;
  ;;which, in  recordized code, are  represented by struct  instances of
  ;;type FUNCALL; everything else is left untouched.
  ;;
  ;;If the ?OPERATOR is a struct instance of type PRIMREF representing a
  ;;primitive  operation:  such struct  is  replaced  by an  appropriate
  ;;struct instance of type PRIMCALL; recordized code like:
  ;;
  ;;   #[funcall  #[primref ?name] (?arg ...)]
  ;;
  ;;is converted to:
  ;;
  ;;   #[primcall #[primref ?name] (?arg ...)]
  ;;
  ;;Notice that not all the struct instances of type PRIMREF reference a
  ;;primitive operation: the struct type  PRIMREF is used to represent a
  ;;reference to all  the function bindings exported by  the boot image.
  ;;Only  those for  which ?NAME  is a  symbol satisfying  the predicate
  ;;PRIMOP?   are  primitive  operations;   in  other  words,  only  the
  ;;operations  defined  by  the   syntax  DEFINE-PRIMOP  are  primitive
  ;;operations.  Examples: $CAR, $CDR, FIXNUM? are primitive operations;
  ;;LIST, NUMBER?, STRING-LENGTH are *not* primitive operations.
  ;;
  ;;This module accepts as input a  struct instance of type CODES, whose
  ;;internal recordized code must be composed by struct instances of the
  ;;following types:
  ;;
  ;;   bind		closure		conditional
  ;;   constant		fix		forcall
  ;;   funcall		jmpcall		known
  ;;   primref		seq		var
  ;;
  (define who 'alt-cogen.introduce-primcalls)

  (define (alt-cogen.introduce-primcalls Program)
    (struct-case Program
      ((codes code* body)
       (make-codes ($map/stx Clambda code*)
		   (E body)))
      (else
       (error who "invalid program" Program))))

;;; --------------------------------------------------------------------

  (module (E)

    (define (E x)
      ;;Perform code transformation traversing the whole hierarchy in X,
      ;;which must be  a struct instance representing  recordized code ,
      ;;and building  a new  hierarchy of transformed,  recordized code;
      ;;return the new hierarchy.
      ;;
      ;;The  purpose of  this recordized  code traversal  is to  process
      ;;struct  instances of  type  FUNCALL with  the module  MKFUNCALL;
      ;;everything else is left untouched.
      ;;
      (struct-case x
	((constant)
	 x)

	((var)
	 x)

	((primref)
	 x)

	((bind lhs* rhs* body)
	 (make-bind lhs* ($map/stx E rhs*) (E body)))

	((fix lhs* rhs* body)
	 (make-fix lhs* rhs* (E body)))

	((conditional e0 e1 e2)
	 (make-conditional (E e0) (E e1) (E e2)))

	((seq e0 e1)
	 (make-seq (E e0) (E e1)))

	((closure)
	 x)

	((forcall op arg*)
	 (make-forcall op ($map/stx E arg*)))

	((funcall rator arg*)
	 (mkfuncall (E-known rator) ($map/stx E-known arg*)))

	((jmpcall label rator arg*)
	 (make-jmpcall label (E rator) ($map/stx E arg*)))

	(else
	 (error who "invalid expr" x))))

    (define (E-known x)
      (struct-case x
	((known expr type)
	 (make-known (E expr) type))
	(else
	 (E x))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (module (Clambda)
    ;;The purpose  of this  module is to  apply E to  the body  of every
    ;;CASE-LAMBDA clause.
    ;;
    (define (Clambda x)
      (struct-case x
	((clambda label case* cp free* name)
	 (make-clambda label ($map/stx ClambdaCase case*) cp free* name))
	(else
	 (error who "invalid clambda" x))))

    (define (ClambdaCase x)
      (struct-case x
	((clambda-case info body)
	 (make-clambda-case info (E body)))
	(else
	 (error who "invalid clambda-case" x))))

    #| end of module: Clambda |# )

;;; --------------------------------------------------------------------

  (module (mkfuncall)

    (define (mkfuncall op arg*)
      ;;OP is a struct instance  representing the operator in a function
      ;;application.
      ;;
      ;;ARG* is a list of struct instances representing the arguments of
      ;;a function application.
      ;;
      ;;If  the   operator  is  a   struct  instance  of   type  PRIMREF
      ;;representing a  primitive operation: such struct  is replaced by
      ;;an appropriate struct instance of type PRIMCALL.
      ;;
      (struct-case op
	((known expr)
	 (mkfuncall expr arg*))

	((primref name)
	 (if (%primitive-operation? name)
	     (make-primcall name arg*)
	   (make-funcall op arg*)))

	(else
	 (make-funcall op arg*))))

    (define (%primitive-operation? x)
      ;;Import PRIMOP?  from a module defined  in "pass-specify-rep.ss".
      ;;(Marco Maggi; Oct 14, 2012)
      (import primops)
      (or (eq? x 'debug-call)
	  (primop? x)))

    #| end of module: mkfuncall |# )

;;; --------------------------------------------------------------------

  ;;Commented out because unused.  (Marco Maggi; Oct 14, 2012)
  ;;
  ;; (define (check-gensym x)
  ;;   (unless (gensym? x)
  ;;     (error who "invalid gensym" x)))
  ;;
  ;; (define (check-label x)
  ;;   (struct-case x
  ;;     ((code-loc label)
  ;;      (check-gensym label))
  ;;     (else
  ;;      (error who "invalid label" x))))
  ;;
  ;; (define (check-var x)
  ;;   (struct-case x
  ;;     ((var)
  ;;      (void))
  ;;     (else
  ;;      (error who "invalid var" x))))
  ;;
  ;; (define (check-closure x)
  ;;   (struct-case x
  ;;     ((closure label free*)
  ;;      (check-label label)
  ;;      (for-each check-var free*))
  ;;     (else
  ;;      (error who "invalid closure" x))))

  #| end of module: alt-cogen.introduce-primcalls |# )


(module (alt-cogen.eliminate-fix)
  ;;Despite its name, the purpose of  this module is *not* to remove the
  ;;FIX structures from recordized code.
  ;;
  ;;FIXME The following  description is not quite  correct (Marco Maggi;
  ;;Oct 15, 2012).  Knowing that, in general, every closure has multiple
  ;;clauses (generated by CASE-LAMBDA), let's think of the closure built
  ;;in object as a memory block holding a slot for every free variable:
  ;;
  ;;                  0   1   2   3   4   5
  ;;   |------------|---|---|---|---|---|---| closure object
  ;;         ^
  ;;         |      |.......................|
  ;;    pointer to     one slot for every
  ;;    binary code    free variable
  ;;
  ;;the purpose of this module is to:
  ;;
  ;;1. For every closure's clause make a new struct instance of type VAR
  ;;   called CPVAR.
  ;;
  ;;2. In  every closure  clause's body: find  every struct  instance of
  ;;    type VAR  referencing the  closure itself,  and replace  it with
  ;;   CPVAR.
  ;;
  ;;3. In  every closure  clause's body: find  every struct  instance of
  ;;   type  VAR referencing  a closure's free  variable and  replace it
  ;;    with  a primitive  operation  %CPREF  retrieving the  referenced
  ;;   object from  the associated slot in the data  area of the closure
  ;;   built in object.
  ;;
  ;;4.  For every  ?CLOSURE in the expressions of  the <Program> perform
  ;;   this transformation:
  ;;
  ;;      (let ((T ?closure))
  ;;        T)
  ;;
    (define who 'alt-cogen.eliminate-fix)

  (define (alt-cogen.eliminate-fix Program)
    (struct-case Program
      ((codes code* body)
       ;;First traverse the CASE-LAMBDA  bodies, then traverse the whole
       ;;expression.
       (let* ((code*^ ($map/stx Clambda code*))
	      (E      (make-E #f #f '()))
	      (body^  (E body)))
	 (make-codes code*^ body^)))
      (else
       (error who "invalid program" Program))))

;;; --------------------------------------------------------------------

  (module (Clambda)

    (define (Clambda x)
      ;;X must be a struct instance of type CLAMBDA.
      ;;
      (struct-case x
	((clambda label case* cp free* name)
	 (let ((case-mapper (ClambdaCase cp free*)))
	   (make-clambda label ($map/stx case-mapper case*) #f free* name)))
	(else
	 (error who "invalid clambda" x))))

    (define (ClambdaCase main-cp free*)
      ;;MAIN-CP  must be  a struct  instance of  type VAR  to which  the
      ;;CLOSURE wrapping this CLAMBDA is bound.
      ;;
      ;;FREE*  must  be   a  list  of  struct  instances   of  type  VAR
      ;;representing  the free  variables referenced  by the  clauses of
      ;;this CASE-LAMBDA.
      ;;
      ;;Return  a  function  to  be mapped  over  all  the  CLAMBDA-CASE
      ;;structures representing the clauses of this CLAMBDA.
      ;;
      ;;Notice that CPVAR is prepended to the list of arguments for this
      ;;clause.
      ;;
      (lambda (x)
	;;X must be a struct instance of type CLAMBDA-CASE.
	;;
	(struct-case x
	  ((clambda-case info body)
	   (struct-case info
	     ((case-info label args proper?)
	      (let* ((cpvar (unique-var 'cp))
		     (info^ (make-case-info label (cons cpvar args) proper?))
		     (E     (make-E main-cp cpvar free*))
		     (body^ (E body)))
		(make-clambda-case info^ body^)))))
	  (else
	   (error who "invalid clambda-case" x)))))

    #| end of module: Clambda |# )

;;; --------------------------------------------------------------------

  (define (make-E main-cpvar cpvar free*)

    (define (E x)
      ;;Perform code transformation traversing the whole hierarchy in X,
      ;;which must  be a  struct instance representing  recordized code,
      ;;and building  a new  hierarchy of transformed,  recordized code;
      ;;return the new hierarchy.
      ;;
      ;;The purposes of this code traversal are:
      ;;
      ;;1.  Map  %DO-VAR over  every  struct  instance  of type  VAR  in
      ;;   reference position.
      ;;
      ;;2. Map %DO-FIX to every struct instance of type FIX.
      ;;
      ;;3. Convert every  struct instance of type CLOSURE  into a struct
      ;;   instance of type FIX representing this form:
      ;;
      ;;      (let ((T ?closure))
      ;;        T)
      ;;
      ;;   where T is a unique variable.
      ;;
      (struct-case x
	((constant)
	 x)

	((var)
	 (%do-var x))

	((primref)
	 x)

	((bind lhs* rhs* body)
	 (make-bind lhs* ($map/stx E rhs*) (E body)))

	((fix lhs* rhs* body)
	 (%do-fix lhs* rhs* (E body)))

	((conditional e0 e1 e2)
	 (make-conditional (E e0) (E e1) (E e2)))

	((seq e0 e1)
	 (make-seq (E e0) (E e1)))

	;;FIXME Is it actually possible,  here, to find a CLOSURE struct
	;;not already in a FIX?  (Marco Maggi; Oct 15, 2012)
	;;
	((closure)
	 (let ((t (unique-var 'tmp)))
	   (E (make-fix (list t) (list x) t))))

	((primcall op arg*)
	 (make-primcall op ($map/stx E-known arg*)))

	((forcall op arg*)
	 (make-forcall op ($map/stx E arg*)))

	((funcall rator arg*)
	 (make-funcall (E-known rator) ($map/stx E-known arg*)))

	((jmpcall label rator arg*)
	 (make-jmpcall label (E rator) ($map/stx E arg*)))

	(else
	 (error who "invalid expr" x))))

    (define (E-known x)
      (struct-case x
	((known expr type)
	 (make-known (E expr) type))
	(else
	 (E x))))

    (module (%do-fix)
      ;;The purpose of  this module is to map %DO-VAR  to all the struct
      ;;instances of  type VAR  being free  variables referenced  by the
      ;;closures.   We cannot  move this  module out  of MAKE-E  because
      ;;%DO-VAR is a closure on the arguments of MAKE-E itself.
      ;;
      (define (%do-fix lhs* rhs* body)
	(make-fix lhs* ($map/stx %handle-closure rhs*) body))

      (define (%handle-closure rhs)
	;;RHS must be a struct instance of type CLOSURE.
	;;
	(struct-case rhs
	  ((closure code free* well-known?)
	   (make-closure code ($map/stx %do-var free*) well-known?))))

      #| end of module: %do-fix |# )

    (define (%do-var x)
      ;;This function is a closure  upon the arguments of MAKE-E:
      ;;
      ;;MAIN-CPVAR:  a  struct  instance  of type  VAR  referencing  the
      ;;closure whose body we are traversing.
      ;;
      ;;CPVAR: a struct instance of type VAR associated to the closure's
      ;;clause whose body we are traversing.
      ;;
      ;;FREE*: a list  of struct instances of type  VAR representing the
      ;;free  variables referenced  by  the closure  whose  body we  are
      ;;traversing.
      ;;
      ;;X must  be a struct instance  of type VAR.
      ;;
      ;;If X references the closure itself: replace it with CPVAR.
      ;;
      ;;If X  references a  closure's free variable:  replace it  with a
      ;;primitive operation %CPREF retrieving the referenced object from
      ;;the associated  slot in the  data area  of the closure  built in
      ;;object.
      ;;
      (if (eq? x main-cpvar)
	  cpvar
	(let loop ((free* free*)
		   (i     0))
	  (cond ((null? free*)
		 x)
		((eq? x ($car free*))
		 ;;Replate  a  reference  to   free  variable  with  the
		 ;;appropriate slot accessor.
		 (make-primcall '$cpref (list cpvar (make-constant i))))
		(else
		 (loop ($cdr free*) ($fxadd1 i)))))))

    E)

  #| end of module: alt-cogen.eliminate-fix |# )


(module (alt-cogen.insert-engine-checks)
  ;;This  module traverses  all the  function  bodies and,  if the  body
  ;;contains at least one JMPCALL struct or one FUNCALL struct (in which
  ;;the operator is *not* a PRIMREF), it transforms the ?BODY into:
  ;;
  ;;   (begin
  ;;     (primcall '$do-event '())
  ;;     ?body)
  ;;
  (define who 'alt-cogen.insert-engine-checks)

  (module (alt-cogen.insert-engine-checks)

    (define (alt-cogen.insert-engine-checks x)
      (struct-case x
	((codes list body)
	 (make-codes ($map/stx CodeExpr list)
		     (%process-body body)))))

    (define (CodeExpr x)
      (struct-case x
	((clambda label cases cp free name)
	 (make-clambda label ($map/stx CaseExpr cases) cp free name))))

    (define (CaseExpr x)
      (struct-case x
	((clambda-case info body)
	 (make-clambda-case info (%process-body body)))))

    (define (%process-body body)
      (if (E body)
	  (make-seq (make-primcall '$do-event '())
		    body)
	body))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (E)

    (define (E x)
      ;;The purpose of this recordized  code traversal is to return true
      ;;if:
      ;;
      ;;* At least one of the nested structs is an instance of JMPCALL.
      ;;
      ;;* At least  one of the nested structs is  an instance of FUNCALL
      ;;   in which  the operator  is *not*  a struct  instance of  type
      ;;  PRIMREF.
      ;;
      ;;else the return value is false.
      ;;
      (struct-case x
	((constant)
	 #f)

	((var)
	 #f)

	((primref)
	 #f)

	((jmpcall label rator arg*)
	 #t)

	((funcall rator arg*)
	 (if (%known-primref? rator)
	     (ormap E-known arg*)
	   #t))

	((bind lhs* rhs* body)
	 (or (ormap E rhs*) (E body)))

	((fix lhs* rhs* body)
	 (E body))

	((conditional e0 e1 e2)
	 (or (E e0) (E e1) (E e2)))

	((seq e0 e1)
	 (or (E e0) (E e1)))

	((primcall op arg*)
	 (ormap E-known arg*))

	((forcall op arg*)
	 (ormap E arg*))

	(else
	 (error who "invalid expr" x))))

    (define (E-known x)
      (struct-case x
	((known expr)
	 (E expr))
	(else
	 (E x))))

    (define (%known-primref? x)
      ;;Return true if X is a  struct instance of type PRIMREF, possibly
      ;;wrapped into a struct instance of type KNOWN.
      ;;
      (struct-case x
	((known expr)
	 (%known-primref? expr))
	((primref)
	 #t)
	(else
	 #f)))

    #| end of module: E |# )

  #| end of file: alt-cogen.insert-engine-checks |# )


(module (alt-cogen.insert-stack-overflow-check)
  ;;This  module traverses  all  the  function bodies  and,  if a  ?BODY
  ;;contains only function  calls in tail position, it  transforms it as
  ;;follows:
  ;;
  ;;   (begin
  ;;     (primcall '$stack-overflow-check '())
  ;;     ?body)
  ;;
  (define who 'alt-cogen.insert-stack-overflow-check)

  (module (alt-cogen.insert-stack-overflow-check)

    (define (alt-cogen.insert-stack-overflow-check Program)
      (struct-case Program
	((codes code* body)
	 (make-codes (map Clambda code*)
		     (%process-body body)))))

    (module (Clambda)
      ;;The purpose of this module is  to apply %PROCESS-BODY to all the
      ;;bodies of closure's clauses.
      ;;
      (define (Clambda x)
	(struct-case x
	  ((clambda label case* cp free* name)
	   (make-clambda label (map ClambdaCase case*) cp free* name))))

      (define (ClambdaCase x)
	(struct-case x
	  ((clambda-case info body)
	   (make-clambda-case info (%process-body body)))))

      #| end of module: Clambda |# )

    (define (%process-body body)
      (if (%tail? body)
	  (make-seq (make-primcall '$stack-overflow-check '()) body)
	body))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (%tail?)

    (define (%tail? body)
      ;;Return true if  the recordized code BODY  contains only function
      ;;calls in tail position.
      ;;
      (struct-case body
	((constant)		#f)
	((var)			#f)
	((primref)		#f)

	((bind lhs* rhs* body)
	 (or (ormap %non-tail? rhs*)
	     (%tail? body)))

	((fix lhs* rhs* body)
	 (%tail? body))

	((conditional e0 e1 e2)
	 (or (%non-tail? e0)
	     (%tail? e1)
	     (%tail? e2)))

	((seq e0 e1)
	 (or (%non-tail? e0)
	     (%tail? e1)))

	((primcall op arg*)
	 (ormap %non-tail? arg*))

	((forcall op arg*)
	 (ormap %non-tail? arg*))

	((funcall rator arg*)
	 (or (%non-tail? rator)
	     (ormap %non-tail? arg*)))

	((jmpcall label rator arg*)
	 (or (%non-tail? rator)
	     (ormap %non-tail? arg*)))

	;;Punt.  (Abdulaziz Ghuloum)
	((mvcall rator k)
	 #t)

	(else
	 (error who "invalid expr" body))))

    (module (%non-tail?)

      (define (%non-tail? x)
	;;Notice that this function never  calls %TAIL?.  Return true if
	;;the recordized code X contains any type of function call.
	;;
	(struct-case x
	  ((constant)			#f)
	  ((var)			#f)
	  ((primref)			#f)

	  ((funcall rator arg*)		#t)
	  ((jmpcall label rator arg*)	#t)
	  ((mvcall rator k)		#t)

	  ;;FIXME!  (Abdulaziz Ghuloum)
	  ((primcall op arg*)
	   (ormap %non-tail?-known arg*))

	  ((bind lhs* rhs* body)
	   (or (ormap %non-tail? rhs*)
	       (%non-tail? body)))

	  ((fix lhs* rhs* body)
	   (%non-tail? body))

	  ((conditional e0 e1 e2)
	   (or (%non-tail? e0)
	       (%non-tail? e1)
	       (%non-tail? e2)))

	  ((seq e0 e1)
	   (or (%non-tail? e0)
	       (%non-tail? e1)))

	  ((forcall op arg*)
	   (ormap %non-tail? arg*))

	  ((known expr)
	   (%non-tail? expr))

	  (else
	   (error who "invalid expr" x))))

      (define (%non-tail?-known x)
	(struct-case x
	  ((known expr)
	   (%non-tail? expr))
	  (else
	   (%non-tail? x))))

      #| end of module: %non-tail? |# )

    #| end of module: %tail? |# )

  #| end of module: alt-cogen.insert-stack-overflow-check |# )


;;;; some external code

(include/verbose "pass-specify-rep.ss")


;;;; some CPU registers stuff

(define-constant PARAMETER-REGISTERS '(%edi))

(define-constant RETURN-VALUE-REGISTER '%eax)

;;Continuation pointer register?
;;
(define-constant CP-REGISTER '%edi)

(define-constant ALL-REGISTERS
  (case-word-size
   ((32)
    '(%eax %edi %ebx %edx %ecx))
   ((64)
    '(%eax %edi %ebx %edx %ecx %r8 %r9 %r10 %r11 %r14 %r15))))

(define-constant NON-8BIT-REGISTERS
  (case-word-size
   ((32)	'(%edi))
   ((64)	'(%edi))))

(define-constant ARGC-REGISTER '%eax)

;;; apr = %ebp		allocation pointer
;;; esp = %esp		stack pointer
;;; pcr = %esi		pointer to PCB
;;; cpr = %edi		pointer to closure

(define (register-index x)
  (cond ((assq x '((%eax 0) (%edi 1) (%ebx 2) (%edx 3)
		   (%ecx 4) (%esi 5) (%esp 6) (%ebp 7)))
	 => cadr)
	(else
	 (error 'register-index "not a register" x))))


(module (alt-cogen.impose-calling-convention/evaluation-order)

  (define who 'alt-cogen.impose-calling-convention/evaluation-order)

  (define (alt-cogen.impose-calling-convention/evaluation-order x)
    (Program x))

;;; --------------------------------------------------------------------

  (define locals
    (make-parameter #f))

  (define-inline (%locals-cons A)
    (locals (cons A (locals))))

  (define-inline (%locals-cons* A0 A ...)
    (locals (cons* A0 A ... (locals))))

;;; --------------------------------------------------------------------

  (module (Program)

    (define (Program x)
      (struct-case x
	((codes x.code* x.body)
	 (make-codes (map Clambda x.code*) (Main x.body)))))

    (define (Clambda x)
      (struct-case x
	((clambda x.label x.case* x.cp x.free* x.name)
	 (make-clambda x.label (map ClambdaCase x.case*) x.cp x.free* x.name))))

    (module (ClambdaCase)

      (define (ClambdaCase cas)
	(struct-case cas
	  ((clambda-case cas.info cas.body)
	   (struct-case cas.info
	     ((case-info cas.info.label cas.info.args cas.info.proper)
	      (let-values (((rargs rlocs fargs flocs)
			    (%partition-formals PARAMETER-REGISTERS cas.info.args)))
		;;RARGS = list of register symbols associated to formals.
		;;RLOCS = list of formals associated to symbols.
		;;FARGS = list of formals associated to FVAR structures.
		;;FLOCS = list of FVAR structures associated to formals.
		(parametrise ((locals rargs))
		  (for-each set-var-loc!
		    fargs flocs)
		  (let ((body (let recur ((args rargs)
					  (locs rlocs))
				(if (null? args)
				    (Tail cas.body)
				  (make-seq (%make-move ($car args) ($car locs))
					    (recur    ($cdr args) ($cdr locs)))))))
		    (make-clambda-case
		     (make-case-info cas.info.label (append rlocs flocs) cas.info.proper)
		     (make-locals (locals) body))))))))))

      (define (%partition-formals regs ls)
	;;Recursive  function.   Associate  CASE-LAMBDA formals  to  CPU
	;;available registers.
	;;
	;;REGS must be a list of symbols representing CPU registers.
	;;
	;;LS must be a list of CASE-LAMBDA formals.
	;;
	;;Return 4 values:
	;;
	;;1. a list of register symbols associated to formals;
	;;
	;;2. a list of formals associated  to symbols;
	;;
	;;3. a list of formals associated to FVAR structures;
	;;
	;;4. a list of FVAR structures associated to formals.
	;;
	(cond ((null? regs)
	       ;;If  the  number of  formals  is  <=  of the  number  of
	       ;;registers:  the left-over  registers are  associated to
	       ;;FVAR structures.
	       (let ((flocs (%one-fvar-for-each-left-over-formal 1 ls)))
		 (values '() '() ls flocs)))
	      ((null? ls)
	       ;;If there are more registers than formals: fine.
	       (values '() '() '() '()))
	      (else
	       ;;If there is a register for this formal: associate them.
	       (let-values (((rargs rlocs fargs flocs)
			     (%partition-formals ($cdr regs) ($cdr ls))))
		 (values (cons ($car ls)   rargs)
			 (cons ($car regs) rlocs)
			 fargs flocs)))))

      (define (%one-fvar-for-each-left-over-formal i ls)
	(if (null? ls)
	    '()
	  (cons (mkfvar i)
		(%one-fvar-for-each-left-over-formal ($fxadd1 i) ($cdr ls)))))

      #| end of module: ClambdaCase |# )

    (define (Main x)
      (parametrise ((locals '()))
	(let ((x (Tail x)))
	  (make-locals (locals) x))))

    (module (Tail)

      (define (Tail x)
	(struct-case x

	  ((constant)
	   (VT x))

	  ((var)
	   (VT x))

	  ((primcall op rands)
	   (case-symbols op
	     (($call-with-underflow-handler)
	      (let ((t0		(unique-var 't))
		    (t1		(unique-var 't))
		    (t2		(unique-var 't))
		    (handler	($car rands))
		    (proc	($cadr rands))
		    (k		($caddr rands)))
		(%locals-cons* t0 t1 t2)
		(multiple-forms-sequence
		 (V t0 handler)
		 (V t1 k)
		 (V t2 proc)
		 (%make-move (mkfvar 1) t0)
		 (%make-move (mkfvar 2) t1)
		 (%make-move cpr t2)
		 (%make-move ARGC-REGISTER (make-constant (argc-convention 1)))
		 (make-asm-instr 'int- fpr (make-constant wordsize))
		 (make-primcall 'indirect-jump
		   (list ARGC-REGISTER cpr pcr esp apr (mkfvar 1) (mkfvar 2))))))
	     (else
	      (VT x))))

	  ((bind lhs* rhs* e)
	   (%do-bind lhs* rhs* (Tail e)))

	  ((seq e0 e1)
	   (make-seq (E e0) (Tail e1)))

	  ((conditional e0 e1 e2)
	   (make-conditional (P e0) (Tail e1) (Tail e2)))

	  ((funcall rator rands)
	   (%handle-tail-call #f rator rands))

	  ((jmpcall label rator rands)
	   (%handle-tail-call (make-code-loc label) rator rands))

	  ((forcall)
	   (VT x))

	  ((shortcut body handler)
	   (make-shortcut (Tail body) (Tail handler)))

	  ((known expr)
	   (Tail expr))

	  (else
	   (error who "invalid tail" x))))

      (define (VT x)
	(S x (lambda (x)
	       (make-seq (%make-move RETURN-VALUE-REGISTER x)
			 (make-primcall 'return (list pcr esp apr RETURN-VALUE-REGISTER))))))

      #| end of module: Tail |# )

    #| end of module: Program |# )

;;; --------------------------------------------------------------------
;;; helpers

  (define (S* x* kont)
    (if (null? x*)
	(kont '())
      (S ($car x*) (lambda (a)
		     (S* ($cdr x*) (lambda (d)
				     (kont (cons a d))))))))

  (define (S x kont)
    (struct-case x
      ((bind lhs* rhs* body)
       (%do-bind lhs* rhs* (S body kont)))
      ((seq e0 e1)
       (make-seq (E e0) (S e1 kont)))
      ((known expr)
       (S expr kont))
      (else
       (cond ((or (constant? x)
		  (symbol?   x))
	      (kont x))
	     ((var? x)
	      (cond ((var-loc x)
		     => kont)
		    (else
		     (kont x))))
	     ((or (funcall? x) (primcall? x) (jmpcall? x)
		  (forcall? x) (shortcut? x) (conditional? x))
	      (let ((t (unique-var 'tmp)))
		(%do-bind (list t) (list x) (kont t))))
	     (else
	      (error who "invalid S" x))))))

;;; --------------------------------------------------------------------

  (define (%do-bind lhs* rhs* body)
    (if (null? lhs*)
	body
      (begin
	(%locals-cons ($car lhs*))
	(make-seq (V ($car lhs*) ($car rhs*))
		  (%do-bind ($cdr lhs*) ($cdr rhs*) body)))))

  (define-inline (%make-move lhs rhs)
    (make-asm-instr 'move lhs rhs))

  (define (%do-bind-frmt* nf* v* ac)
    (if (null? nf*)
	ac
      (make-seq (V ($car nf*) ($car v*))
		(%do-bind-frmt* ($cdr nf*) ($cdr v*) ac))))

  (module (handle-nontail-call)

    (define (handle-nontail-call rator rands value-dest call-targ)
      (let-values (((reg-locs reg-args frm-args)
		    (%nontail-locations PARAMETER-REGISTERS (cons rator rands))))
	(let ((regt* (map (lambda (x)
			    (unique-var 'rt))
		       reg-args))
	      (frmt* (map (lambda (x)
			    (make-nfv 'unset-conflicts #f #f #f #f))
		       frm-args)))
	  (let* ((call (make-ntcall call-targ value-dest
				    (cons* ARGC-REGISTER pcr esp apr
					   (append reg-locs frmt*))
				    #f #f))
		 (body (make-nframe
			frmt* #f
			(%do-bind-frmt*
			 frmt* frm-args
			 (%do-bind ($cdr regt*) ($cdr reg-args)
				   ;;evaluate cpt last
				   (%do-bind (list ($car regt*)) (list ($car reg-args))
					     (assign*
					      reg-locs regt*
					      (make-seq
					       (%make-move ARGC-REGISTER
							   (make-constant
							    (argc-convention (length rands))))
					       call))))))))
	    (if value-dest
		(make-seq body (%make-move value-dest RETURN-VALUE-REGISTER))
	      body)))))

    (define (%nontail-locations regs args)
      (cond ((null? args)
	     (values '() '() '()))
	    ((null? regs)
	     (values '() '() args))
	    (else
	     (let-values (((r* rl* f*)
			   (%nontail-locations ($cdr regs) ($cdr args))))
	       (values (cons ($car regs) r*)
		       (cons ($car args) rl*)
		       f*)))))

    #| end of module: handle-nontail-call |# )

  (module (alloc-check)

    (define (alloc-check size)
      (E (make-shortcut
	  (make-conditional ;;; PCB ALLOC-REDLINE
	      (%test size)
	      (make-primcall 'nop '())
	    (make-primcall 'interrupt '()))
	  (make-funcall
	   (make-primcall 'mref
	     (list
	      (make-constant (make-object (primref->symbol 'do-overflow)))
	      (make-constant (- disp-symbol-record-proc symbol-primary-tag))))
	   (list size)))))

    (define (%test size)
      (if (struct-case size
	    ((constant i)
	     (<= i 4096))
	    (else
	     #f))
	  (make-primcall '<=
	    (list apr
		  (make-primcall 'mref
		    (list pcr (make-constant pcb-allocation-redline)))))
	(make-primcall '>=
	  (list (make-primcall 'int-
		  (list (make-primcall 'mref
			  (list pcr (make-constant pcb-allocation-redline)))
			apr))
		size))))

    ;;Commented out by Abdulaziz Ghuloum.
    ;;
    ;; (define (alloc-check size)
    ;;   (E (make-conditional ;;; PCB ALLOC-REDLINE
    ;; 	   (make-primcall '<=
    ;; 			  (list (make-primcall 'int+ (list apr size))
    ;; 				(make-primcall 'mref (list pcr (make-constant 4)))))
    ;; 	   (make-primcall 'nop '())
    ;; 	 (make-funcall
    ;;         (make-primcall 'mref
    ;; 			 (list
    ;; 			  (make-constant (make-object (primref->symbol 'do-overflow)))
    ;; 			  (make-constant (- disp-symbol-record-proc symbol-primary-tag))))
    ;;         (list size)))))

    #| end of module: alloc-check |# )

;;; --------------------------------------------------------------------

  (define (V d x)
    ;;Generate assembly  instructions to compute  a value from  struct X
    ;;and store the result in destination D.
    ;;
    (struct-case x
      ((constant)
       (%make-move d x))

      ((var)
       (cond ((var-loc x)
	      => (lambda (loc)
		   (%make-move d loc)))
	     (else
	      (%make-move d x))))
      ((bind lhs* rhs* e)
       (%do-bind lhs* rhs* (V d e)))

      ((seq e0 e1)
       (make-seq (E e0) (V d e1)))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (V d e1) (V d e2)))

      ((primcall op rands)
       (case-symbols op
         ((alloc)
          (S ($car rands)
             (lambda (size)
               (make-seq (alloc-check size)
			 (S ($cadr rands)
			    (lambda (tag)
			      (make-seq (make-seq (%make-move d apr)
						  (make-asm-instr 'logor d tag))
					(make-asm-instr 'int+ apr size))))))))

         ((mref)
          (S* rands (lambda (rands)
		      (%make-move d (make-disp ($car rands) ($cadr rands))))))

         ((mref32)
          (S* rands (lambda (rands)
		      (make-asm-instr 'load32 d (make-disp ($car rands) ($cadr rands))))))

         ((bref)
          (S* rands (lambda (rands)
		      (make-asm-instr 'load8 d (make-disp ($car rands) ($cadr rands))))))

         ((logand logxor logor int+ int- int*
                  int-/overflow int+/overflow int*/overflow)
          (make-seq (V d ($car rands))
		    (S ($cadr rands) (lambda (s)
				       (make-asm-instr op d s)))))
         ((int-quotient)
          (S* rands (lambda (rands)
		      (multiple-forms-sequence
		       (%make-move eax ($car rands))
		       (make-asm-instr 'cltd edx eax)
		       (make-asm-instr 'idiv eax ($cadr rands))
		       (%make-move d eax)))))

         ((int-remainder)
          (S* rands (lambda (rands)
		      (multiple-forms-sequence
		       (%make-move eax ($car rands))
		       (make-asm-instr 'cltd edx eax)
		       (make-asm-instr 'idiv edx ($cadr rands))
		       (%make-move d edx)))))

         ((sll sra srl sll/overflow)
          (let ((a ($car rands))
		(b ($cadr rands)))
            (if (constant? b)
		(make-seq (V d a)
			  (make-asm-instr op d b))
	      (S b (lambda (b)
		     (multiple-forms-sequence
		      (V d a)
		      (%make-move ecx b)
		      (make-asm-instr op d ecx)))))))

         (else
	  (error who "invalid value op" op))))

      ((funcall rator rands)
       (handle-nontail-call rator rands d #f))

      ((jmpcall label rator rands)
       (handle-nontail-call rator rands d label))

      ((forcall op rands)
       (handle-nontail-call (make-constant (make-foreign-label op))
			    rands d op))

      ((shortcut body handler)
       (make-shortcut (V d body)
		      (V d handler)))

      ((known expr)
       (V d expr))

      (else
       (if (symbol? x)
           (%make-move d x)
	 (error who "invalid value" (unparse-recordized-code x))))))

;;; --------------------------------------------------------------------

  (define (assign* lhs* rhs* tail-body)
    ;;Given a list of left-hand  sides and right-hand sides for assembly
    ;;assignments,  build  and  return a  struct  instance  representing
    ;;recordized code for this pseudo-code:
    ;;
    ;;   (begin
    ;;     (move ?lhs ?rhs)
    ;;     ...
    ;;     . ?tail-body)
    ;;
    (if (null? lhs*)
	tail-body
      (make-seq (%make-move ($car lhs*) ($car rhs*))
		(assign*  ($cdr lhs*) ($cdr rhs*) tail-body))))

;;; --------------------------------------------------------------------

  (define (E x)
    (struct-case x
      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (E e1) (E e2)))

      ((bind lhs* rhs* e)
       (%do-bind lhs* rhs* (E e)))

      ((primcall op rands)
       (case-symbols op
         ((mset bset mset32)
          (S* rands (lambda (s*)
		      (make-asm-instr op (make-disp ($car s*) ($cadr s*))
				      (caddr s*)))))
         ((fl:load fl:store fl:add! fl:sub! fl:mul! fl:div!
                   fl:from-int fl:shuffle bswap!
                   fl:store-single fl:load-single)
          (S* rands (lambda (s*)
		      (make-asm-instr op ($car s*) ($cadr s*)))))
         ((nop interrupt incr/zero? fl:double->single fl:single->double)
	  x)
         (else
	  (error who "invalid instr" x))))

      ((funcall rator rands)
       (handle-nontail-call rator rands #f #f))

      ((jmpcall label rator rands)
       (handle-nontail-call rator rands #f label))

      ((forcall op rands)
       (handle-nontail-call (make-constant (make-foreign-label op))
			    rands #f op))
      ((shortcut body handler)
       (make-shortcut (E body) (E handler)))

      (else
       (error who "invalid effect" x))))

;;; --------------------------------------------------------------------

  (module (P)

    (define (P x)
      (struct-case x
	((constant)
	 x)

	((seq e0 e1)
	 (make-seq (E e0) (P e1)))

	((conditional e0 e1 e2)
	 (make-conditional (P e0) (P e1) (P e2)))

	((bind lhs* rhs* e)
	 (%do-bind lhs* rhs* (P e)))

	((primcall op rands)
	 (let ((a ($car rands)) (b ($cadr rands)))
	   (if (and (constant? a)
		    (constant? b))
	       (let ((t (unique-var 'tmp)))
		 (P (make-bind (list t) (list a)
			       (make-primcall op (list t b)))))
	     (Mem a (lambda (a)
		      (Mem b (lambda (b)
			       (make-asm-instr op a b))))))))

	((shortcut body handler)
	 (make-shortcut (P body) (P handler)))

	(else
	 (error who "invalid pred" x))))

    (define (Mem x kont)
      (struct-case x
	((primcall op arg*)
	 (if (eq? op 'mref)
	     (S* arg* (lambda (arg*)
			(kont (make-disp ($car arg*) ($cadr arg*)))))
	   (S x kont)))
	(else
	 (S x kont))))

    #| end of module: P |# )

;;; --------------------------------------------------------------------

  (module (%handle-tail-call)

    (define (%handle-tail-call target rator rands)
      ;;Handle FUNCALL and JMPCALL structures in tail position.
      ;;
      ;;If  TARGET is  true:  the call  is  a JMPCALL  and  TARGET is  a
      ;;CODE-LOC.
      ;;
      ;;We build and return a struct instance to represent:
      ;;
      ;;1.  For the operator and the operands: a sequence of assignments
      ;;   to store the values in registers or memory locations.
      ;;
      ;;2. Loading the number of  arguments in the appropriate register.
      ;;
      ;;3. The actual call.
      ;;
      (let* ((args (cons rator rands))
	     (locs (%formals-locations PARAMETER-REGISTERS args))
	     (rest (make-seq (%make-move ARGC-REGISTER
					 (make-constant (argc-convention (length rands))))
			     (if target
				 (make-primcall 'direct-jump
				   (cons target (cons* ARGC-REGISTER pcr esp apr locs)))
			       (make-primcall 'indirect-jump
				 (cons* ARGC-REGISTER pcr esp apr locs))))))
	(let recur ((args  (reverse args))
		    (locs  (reverse locs))
		    (targs '())
		    (tlocs '()))
	  (cond ((null? args)
		 (assign* tlocs targs rest))
		((constant? ($car args))
		 (recur ($cdr args)
			($cdr locs)
			(cons ($car args) targs)
			(cons ($car locs) tlocs)))
		((and (fvar? ($car locs))
		      (var? ($car args))
		      (eq? ($car locs)
			   (var-loc ($car args))))
		 (recur ($cdr args)
			($cdr locs)
			targs
			tlocs))
		(else
		 (let ((t (unique-var 'tmp)))
		   (%locals-cons t)
		   (make-seq (V t ($car args))
			     (recur ($cdr args)
				    ($cdr locs)
				    (cons t targs)
				    (cons ($car locs) tlocs)))))))))

    (define (%formals-locations regs args)
      (cond ((null? args)
	     '())
	    ((null? regs)
	     (%one-fvar-for-each-arg 1 args))
	    (else
	     (cons ($car regs) (%formals-locations ($cdr regs) ($cdr args))))))

    (define (%one-fvar-for-each-arg i args)
      (if (null? args)
	  '()
	(cons (mkfvar i)
	      (%one-fvar-for-each-arg (fxadd1 i) ($cdr args)))))

    #| end of module: %handle-tail-call |# )

  #| end of module: alt-cogen.impose-calling-convention/evaluation-order |# )


(module ListySet
  ;;This module has the same API of the module IntegerSet.
  ;;
  (make-empty-set
   singleton
   set-member?		empty-set?
   set-add		set-rem
   set-difference	set-union
   set->list		list->set)

  (define-struct set
    ;;Wrapper for a list of elements used as a set.
    ;;
    (v
		;The list of elements in the set.
     ))

  (define-argument-validation (set who obj)
    (set? obj)
    (assertion-violation who "expected set as argument" obj))

;;; --------------------------------------------------------------------

  (define-inline (make-empty-set)
    (make-set '()))

  (define (singleton x)
    (make-set (list x)))

  (define (set-member? x S)
    (define who 'set-member?)
    (with-arguments-validation (who)
	((set	S))
      (memq x ($set-v S))))

  (define (empty-set? S)
    (define who 'empty-set?)
    (with-arguments-validation (who)
	((set	S))
      (null? ($set-v S))))

  (define (set->list S)
    (define who 'set->list)
    (with-arguments-validation (who)
	((set	S))
      ($set-v S)))

  (define (list->set ls)
    (make-set ls))

  (define (set-add x S)
    (define who 'set-add)
    (with-arguments-validation (who)
	((set	S))
      (if (memq x ($set-v S))
	  S
	(make-set (cons x ($set-v S))))))

  (define ($remq x ell)
    ;;Remove X from the list ELL.
    ;;
    (cond ((null? ell)
	   '())
	  ((eq? x ($car ell))
	   ($cdr ell))
	  (else
	   (cons ($car ell) ($remq x ($cdr ell))))))

  (define (set-rem x S)
    (define who 'set-rem)
    (with-arguments-validation (who)
	((set	S))
      (make-set ($remq x ($set-v S)))))

  (module (set-difference)

    (define (set-difference S1 S2)
      (define who 'set-difference)
      (with-arguments-validation (who)
	  ((set		S1)
	   (set		S2))
	(make-set ($difference ($set-v S1) ($set-v S2)))))

    (define ($difference ell1 ell2)
      ;;Remove from the list ELL1 all  the elements of the list ELL2.  Use
      ;;EQ? for comparison.
      ;;
      (cond ((null? ell2)
	     ell1)
	    (else
	     ($difference ($remq ($car ell2) ell1) ($cdr ell2)))))

    #| end of module: set-difference |# )

  (module (set-union)

    (define (set-union S1 S2)
      (define who 'set-union)
      (with-arguments-validation (who)
	  ((set		S1)
	   (set		S2))
	(make-set ($union ($set-v S1) ($set-v S2)))))

    (define ($union S1 S2)
      (cond ((null? S1)
	     S2)
	    ((memq ($car S1) S2)
	     ($union ($cdr S1) S2))
	    (else
	     (cons ($car S1) (union ($cdr S1) S2)))))

    #| end of module: set-union |# )

  #| end of module: ListySet |# )


(module IntegerSet
  ;;This module implements sets of bits;  each set is a nested hierarchy
  ;;of  lists, pairs  and fixnums  interpreted  as a  tree; fixnums  are
  ;;interpreted as bitvectors.  The empty set is the fixnum zero.
  ;;
  ;;To search for a  bit: we compute a "bit index",  then start from the
  ;;root of the tree and: if the index  is even we go left (the car), if
  ;;the index is odd we go right (the cdr).
  ;;
  ;;This module has the same API of the module ListySet.
  ;;
  (make-empty-set
   singleton
   set-member?		empty-set?
   set-add		set-rem
   set-difference	set-union
   set->list		list->set)

  (begin       ;just comment out this form to switch from unsafe to safe
    (define-inline (car x)		($car x))
    (define-inline (cdr x)		($cdr x))
    (define-inline (fx= x y)		($fx= x y))
    (define-inline (fxsll x amount)	($fxsll x amount))
    (define-inline (fxsra x amount)	($fxsra x amount))
    (define-inline (fxlogor x y)	($fxlogor x y))
    (define-inline (fxlogand x y)	($fxlogand x y))
    (define-inline (fxlognot x)		($fxlognot x))
    (define-inline (fx+ x y)		($fx+ x y))
    (define-inline (fxzero? x)		($fxzero? x))
    (define-inline (fxeven? x)		($fxzero? ($fxlogand x 1))))

;;; --------------------------------------------------------------------

  (define-inline-constant BITS 28)

  (define-inline (make-empty-set)
    0)

  (define-inline ($index-of N)
    ;;Given a  set element N  to be added to,  or searched into,  a set:
    ;;return a fixnum representing the "index"  of the fixnum in which N
    ;;should be stored.
    ;;
    (fxquotient N BITS))

  (define ($mask-of n)
    ;;Given a  set element N  to be added to,  or searched into,  a set:
    ;;return a  fixnum representing the bitmask  of N for the  fixnum in
    ;;which N should be stored.
    ;;
    (fxsll 1 (fxremainder n BITS)))

  (define (singleton N)
    ;;Return a set containing only N.
    ;;
    (set-add N (make-empty-set)))

;;; --------------------------------------------------------------------

  (define (empty-set? S)
    (eqv? S 0))

  (define (set-member? N SET)
    (define who 'set-member?)
    (with-arguments-validation (who)
	((fixnum	N))
      (let loop ((SET SET)
		 (idx ($index-of N))
		 (msk ($mask-of  N))) ;this never changes in the loop
	(cond ((pair? SET)
	       (if (fxeven? idx)
		   (loop (car SET) (fxsra idx 1) msk)
		 (loop (cdr SET) (fxsra idx 1) msk)))
	      ((fxzero? idx)
	       (fx= msk (fxlogand SET msk)))
	      (else
	       #f)))))

  (define (set-add N SET)
    (define who 'set-add)
    (with-arguments-validation (who)
	((fixnum	N))
      (let recur ((SET SET)
		  (idx ($index-of N))
		  (msk ($mask-of  N))) ;this never changes in the loop
	(cond ((pair? SET)
	       (if (fxeven? idx)
		   (let* ((a0 (car SET))
			  (a1 (recur a0 (fxsra idx 1) msk)))
		     (if (eq? a0 a1)
			 SET
		       (cons a1 (cdr SET))))
		 (let* ((d0 (cdr SET))
			(d1 (recur d0 (fxsra idx 1) msk)))
		   (if (eq? d0 d1)
		       SET
		     (cons (car SET) d1)))))
	      ((fxzero? idx)
	       (fxlogor SET msk))
	      (else
	       (if (fxeven? idx)
		   (cons (recur SET (fxsra idx 1) msk) 0)
		 (cons SET (recur 0 (fxsra idx 1) msk))))))))

  (define (cons^ A D)
    (if (and (eq? D 0)
	     (fixnum? A))
        A
      (cons A D)))

  (define (set-rem N SET)
    (define who 'set-rem)
    (with-arguments-validation (who)
	((fixnum	N))
      (let recur ((SET SET)
		  (idx ($index-of N))
		  (msk ($mask-of  N))) ;this never changes in the loop
	(cond ((pair? SET)
	       (if (fxeven? idx)
		   (let* ((a0 (car SET))
			  (a1 (recur a0 (fxsra idx 1) msk)))
		     (if (eq? a0 a1)
			 SET
		       (cons^ a1 (cdr SET))))
		 (let* ((d0 (cdr SET))
			(d1 (recur d0 (fxsra idx 1) msk)))
		   (if (eq? d0 d1)
		       SET
		     (cons^ (car SET) d1)))))
	      ((fxzero? idx)
	       (fxlogand SET (fxlognot msk)))
	      (else
	       SET)))))

  (module (set-union)

    (define (set-union S1 S2)
      (if (pair? S1)
	  (if (pair? S2)
	      (if (eq? S1 S2)
		  S1
		(cons (set-union (car S1) (car S2))
		      (set-union (cdr S1) (cdr S2))))
	    (let ((a0 (car S1)))
	      (let ((a1 (set-union^ a0 S2)))
		(if (eq? a0 a1) S1 (cons a1 (cdr S1))))))
	(if (pair? S2)
	    (let ((a0 (car S2)))
	      (let ((a1 (set-union^ a0 S1)))
		(if (eq? a0 a1) S2 (cons a1 (cdr S2)))))
	  (fxlogor S1 S2))))

    (define (set-union^ S1 M2)
      (if (pair? S1)
	  (let* ((a0 (car S1))
		 (a1 (set-union^ a0 M2)))
	    (if (eq? a0 a1)
		S1
	      (cons a1 (cdr S1))))
	(fxlogor S1 M2)))

    #| end of module: set-union |# )

  (module (set-difference)

    (define (set-difference s1 s2)
      (cond ((pair? s1)
	     (if (pair? s2)
		 (if (eq? s1 s2)
		     0
		   (cons^ (set-difference (car s1) (car s2))
			  (set-difference (cdr s1) (cdr s2))))
	       (let* ((a0 (car s1))
		      (a1 (set-difference^ a0 s2)))
		 (if (eq? a0 a1)
		     s1
		   (cons^ a1 (cdr s1))))))
	    ((pair? s2)
	     (set-difference^^ s1 (car s2)))
	    (else
	     (fxlogand s1 (fxlognot s2)))))

    (define (set-difference^ S1 M2)
      (if (pair? S1)
	  (let* ((a0 (car S1))
		 (a1 (set-difference^ a0 M2)))
	    (if (eq? a0 a1)
		S1
	      (cons^ a1 (cdr S1))))
	(fxlogand S1 (fxlognot M2))))

    (define (set-difference^^ M1 S2)
      (if (pair? S2)
	  (set-difference^^ M1 (car S2))
	(fxlogand M1 (fxlognot S2))))

    #| end of module: set-difference |# )

  (module (list->set)

    (define (list->set ls)
      (define who 'list->set)
      (with-arguments-validation (who)
	  ((list-of-fixnums	ls))
	(let recur ((ls ls)
		    (S  0))
	  (if (null? ls)
	      S
	    (recur (cdr ls) (set-add (car ls) S))))))

    (define-argument-validation (list-of-fixnums who obj)
      (and (list? obj)
	   (for-all fixnum? obj))
      (assertion-violation who "expected list of fixnums as argument" obj))

    #| end of module: list->set |# )

  (define (set->list S)
    (let outer ((i  0)
		(j  1)
		(S  S)
		(ac '()))
      (if (pair? S)
	  (outer i (fxsll j 1) (car S)
		 (outer (fxlogor i j) (fxsll j 1) (cdr S) ac))
	(let inner ((i  (fx* i BITS))
		    (m  S)
		    (ac ac))
	  (if (fxeven? m)
	      (if (fxzero? m)
		  ac
		(inner (fx+ i 1) (fxsra m 1) ac))
	    (inner (fx+ i 1) (fxsra m 1) (cons i ac)))))))

  #| end of module: IntegerSet |# )


(module ListyGraphs
  (empty-graph
   add-edge!
   empty-graph?
   print-graph
   node-neighbors
   delete-node!)
  (import ListySet)

  (define-struct graph
    (ls
		;A list of pairs representing the graph.
		;
		;The  car of  each  pair represents  a  graph's node:  a
		;struct  instance  of type  VAR  or  FVAR, or  a  symbol
		;representing a register.
		;
		;The  cdr  of  each  pair is  an  instance  of  ListySet
		;(whatever it  is defined in that  module), representing
		;the edges outgoing from the node.
		;
		;The  ListySet contains  the target  nodes of  the edges
		;outgoing from  the node represented  by the car  of the
		;pair.
     ))

  (define-inline (empty-graph)
    (make-graph '()))

  (define (empty-graph? G)
    (andmap (lambda (x)
	      (empty-set? ($cdr x)))
	    ($graph-ls G)))

  (module (add-edge!)

    (define (add-edge! G x y)
      (let ((ls ($graph-ls G)))
	(cond ((assq x ls)
	       => (lambda (p0)
		    (unless (set-member? y ($cdr p0))
		      ($set-cdr! p0 (set-add y ($cdr p0)))
		      (cond ((assq y ls)
			     => (lambda (p1)
				  ($set-cdr! p1 (set-add x ($cdr p1)))))
			    (else
			     ($set-graph-ls! G (cons (cons y (single x)) ls)))))))
	      ((assq y ls)
	       => (lambda (p1)
		    ($set-cdr! p1 (set-add x ($cdr p1)))
		    ($set-graph-ls! G (cons (cons x (single y)) ls))))
	      (else
	       ($set-graph-ls! G (cons* (cons x (single y))
					(cons y (single x))
					ls))))))

    (define (single x)
      (set-add x (make-empty-set)))

    #| end of module: add-edge! |# )

  (define (print-graph G)
    (printf "G={\n")
    (parameterize ((print-gensym 'pretty))
      (for-each (lambda (x)
                  (let ((lhs  ($car x))
			(rhs* ($cdr x)))
                    (printf "  ~s => ~s\n"
                            (unparse-recordized-code lhs)
                            (map unparse-recordized-code (set->list rhs*)))))
        ($graph-ls G)))
    (printf "}\n"))

  (define (node-neighbors x G)
    (cond ((assq x ($graph-ls G))
	   => cdr)
	  (else
	   (make-empty-set))))

  (define (delete-node! x G)
    (let ((ls ($graph-ls G)))
      (cond ((assq x ls)
	     => (lambda (p)
		  (for-each (lambda (y)
			      (let ((p (assq y ls)))
				($set-cdr! p (set-rem x ($cdr p)))))
		    (set->list ($cdr p)))
		  ($set-cdr! p (make-empty-set))))
	    (else
	     (void)))))

  #| end of module: ListyGraphs |# )


(module IntegerGraphs
  ;;This module is like ListyGraph, but it makes use of IntegerSet.
  ;;
  (empty-graph
   add-edge!
   empty-graph?
   print-graph
   node-neighbors
   delete-node!)
  (import IntegerSet)

  (define-struct graph
    (ls
		;A list of pairs representing the graph.
		;
		;The  car of  each  pair represents  a  graph's node:  a
		;struct  instance  of type  VAR  or  FVAR, or  a  symbol
		;representing a register.
		;
		;The  cdr of  each  pair is  an  instance of  IntegerSet
		;(whatever it  is defined in that  module), representing
		;the edges outgoing from the node.
		;
		;The  ListySet contains  the target  nodes of  the edges
		;outgoing from  the node represented  by the car  of the
		;pair.
     ))

  (define-inline (empty-graph)
    (make-graph '()))

  (define (empty-graph? g)
    (andmap (lambda (x)
	      (empty-set? ($cdr x)))
	    ($graph-ls g)))

  (define (single x)
    (set-add x (make-empty-set)))

  (define (add-edge! g x y)
    (let ((ls ($graph-ls g)))
      (cond ((assq x ls)
	     => (lambda (p0)
		  (unless (set-member? y ($cdr p0))
		    (set-cdr! p0 (set-add y ($cdr p0)))
		    (cond ((assq y ls)
			   => (lambda (p1)
				(set-cdr! p1 (set-add x ($cdr p1)))))
			  (else
			   ($set-graph-ls! g (cons (cons y (single x)) ls)))))))
	    ((assq y ls)
	     => (lambda (p1)
		  (set-cdr! p1 (set-add x ($cdr p1)))
		  ($set-graph-ls! g (cons (cons x (single y)) ls))))
	    (else
	     ($set-graph-ls! g (cons* (cons x (single y))
				      (cons y (single x))
				      ls))))))

  (define (print-graph g)
    (printf "G={\n")
    (parameterize ((print-gensym 'pretty))
      (for-each (lambda (x)
                  (let ((lhs ($car x))
			(rhs* ($cdr x)))
                    (printf "  ~s => ~s\n"
                            (unparse-recordized-code lhs)
                            (map unparse-recordized-code (set->list rhs*)))))
        ($graph-ls g)))
    (printf "}\n"))

  (define (node-neighbors x g)
    (cond ((assq x ($graph-ls g))
	   => cdr)
	  (else
	   (make-empty-set))))

  (define (delete-node! x g)
    (let ((ls ($graph-ls g)))
      (cond ((assq x ls)
	     => (lambda (p)
		  (for-each (lambda (y)
			      (let ((p (assq y ls)))
				(set-cdr! p (set-rem x ($cdr p)))))
		    (set->list ($cdr p)))
		  (set-cdr! p (make-empty-set))))
	    (else
	     (void)))))

  #| end of module: IntegerGraphs |# )


(module conflict-helpers
  (empty-var-set rem-var add-var union-vars mem-var? for-each-var init-vars!
   empty-nfv-set rem-nfv add-nfv union-nfvs mem-nfv? for-each-nfv init-nfv!
   empty-frm-set rem-frm add-frm union-frms mem-frm?
   empty-reg-set rem-reg add-reg union-regs mem-reg?
   reg?)
  (import IntegerSet)

  (define (add-frm x s)
    (set-add (fvar-idx x) s))

  (define-inline (rem-nfv x s)
    (remq1 x s))

  (define (init-var! x i)
    ($set-var-index! x i)
    ($set-var-var-move! x (empty-var-set))
    ($set-var-reg-move! x (empty-reg-set))
    ($set-var-frm-move! x (empty-frm-set))
    ($set-var-var-conf! x (empty-var-set))
    ($set-var-reg-conf! x (empty-reg-set))
    ($set-var-frm-conf! x (empty-frm-set)))

  (define (init-vars! ls)
    (let loop ((ls ls)
	       (i  0))
      (unless (null? ls)
	(init-var! (car ls) i)
	(loop (cdr ls) (fxadd1 i)))))

  (define (init-nfv! x)
    ($set-nfv-frm-conf! x (empty-frm-set))
    ($set-nfv-nfv-conf! x (empty-nfv-set))
    ($set-nfv-var-conf! x (empty-var-set)))

  (define-inline (reg? x)
    (symbol? x))

  (define-inline (empty-var-set)
    (make-empty-set))

  (define (add-var x s)
    (set-add (var-index x) s))

  (define (mem-var? x s)
    (set-member? (var-index x) s))

  (define (rem-var x s)
    (set-rem (var-index x) s))

  (define-inline (union-vars s1 s2)
    (set-union s1 s2))

  (define (for-each-var s varvec f)
    (for-each (lambda (i) (f (vector-ref varvec i)))
      (set->list s)))

  (define-inline (empty-reg-set)
    (make-empty-set))

  (define (add-reg x s)
    (set-add (register-index x) s))

  (define (rem-reg x s)
    (set-rem (register-index x) s))

  (define (mem-reg? x s)
    (set-member? (register-index x) s))

  (define-inline (union-regs s1 s2)
    (set-union s1 s2))

  (define-inline (empty-frm-set)
    (make-empty-set))

  (define (mem-frm? x s)
    (set-member? (fvar-idx x) s))

  (define (rem-frm x s)
    (set-rem (fvar-idx x) s))

  (define-inline (union-frms s1 s2)
    (set-union s1 s2))

  (define-inline (empty-nfv-set)
    '())

  (define (add-nfv x s)
    (if (memq x s)
	s
      (cons x s)))

  (define-inline (mem-nfv? x s)
    (memq x s))

  (define (union-nfvs s1 s2)
    (let recur ((s1 s1)
		(s2 s2))
      (cond ((null? s1)
	     s2)
	    ((memq (car s1) s2)
	     (recur (cdr s1) s2))
	    (else
	     (cons (car s1)
		   (recur (cdr s1) s2))))))

  (define-inline (for-each-nfv s f)
    (for-each f s))

  #| end of module: conflict-helpers |# )


(define (uncover-frame-conflicts x varvec)
  ;;This function is used only by ALT-COGEN.ASSIGN-FRAME-SIZES.
  ;;
  (import IntegerSet)
  (import conflict-helpers)
  (import (ikarus system $vectors))
  (define who 'uncover-frame-conflicts)

  (define spill-set
    ;;This will be the return value.
    (make-empty-set))

  (define (mark-reg/vars-conf! r vs)
    (for-each-var vs varvec
      (lambda (v)
        ($set-var-reg-conf! v (add-reg r ($var-reg-conf v))))))

  (define (mark-frm/vars-conf! f vs)
    (for-each-var vs varvec
      (lambda (v)
        ($set-var-frm-conf! v (add-frm f ($var-frm-conf v))))))

  (define (mark-frm/nfvs-conf! f ns)
    (for-each-nfv ns
      (lambda (n)
        ($set-nfv-frm-conf! n (add-frm f ($nfv-frm-conf n))))))

  (define (mark-var/vars-conf! v vs)
    (for-each-var vs varvec
      (lambda (w)
        ($set-var-var-conf! w (add-var v ($var-var-conf w)))))
    ($set-var-var-conf! v (union-vars vs ($var-var-conf v))))

  (define (mark-var/frms-conf! v fs)
    ($set-var-frm-conf! v (union-frms fs ($var-frm-conf v))))

  (define (mark-var/regs-conf! v rs)
    ($set-var-reg-conf! v (union-regs rs ($var-reg-conf v))))

  (define (mark-var/nfvs-conf! v ns)
    (for-each-nfv ns
      (lambda (n)
        ($set-nfv-var-conf! n (add-var v ($nfv-var-conf n))))))

  (define (mark-nfv/vars-conf! n vs)
    ($set-nfv-var-conf! n (union-vars vs ($nfv-var-conf n))))

  (define (mark-nfv/frms-conf! n fs)
    ($set-nfv-frm-conf! n (union-frms fs ($nfv-frm-conf n))))

  (define (mark-nfv/nfvs-conf! n ns)
    ($set-nfv-nfv-conf! n (union-nfvs ns ($nfv-nfv-conf n)))
    (for-each-nfv ns
      (lambda (m)
        ($set-nfv-nfv-conf! m (add-nfv n ($nfv-nfv-conf m))))))

  (define (mark-var/var-move! x y)
    ($set-var-var-move! x (add-var y ($var-var-move x)))
    ($set-var-var-move! y (add-var x ($var-var-move y))))

  (define (mark-var/frm-move! x y)
    ($set-var-frm-move! x (add-frm y ($var-frm-move x))))

  (define (mark-var/reg-move! x y)
    ($set-var-reg-move! x (add-reg y ($var-reg-move x))))

  (define (const? x)
    (or (constant? x)
        (code-loc? x)))

  (define (R x vs rs fs ns)
    (cond ((const? x)
	   (values vs rs fs ns))
	  ((reg? x)
	   (values vs (add-reg x rs) fs ns))
	  ((fvar? x)
	   (values vs rs (add-frm x fs) ns))
	  ((var? x)
	   (values (add-var x vs) rs fs ns))
	  ((nfv? x)
	   (values vs rs fs (add-nfv x ns)))
	  ((disp? x)
	   (let-values (((vs rs fs ns)
			 (R (disp-s0 x) vs rs fs ns)))
	     (R (disp-s1 x) vs rs fs ns)))
	  (else
	   (error who "invalid R" x))))

  (define (R* ls vs rs fs ns)
    (if (null? ls)
	(values vs rs fs ns)
      (let-values (((vs rs fs ns)
		    (R (car ls) vs rs fs ns)))
	(R* (cdr ls) vs rs fs ns))))

;;; --------------------------------------------------------------------

  (define (E x vs rs fs ns)
    (struct-case x

      ((seq e0 e1)
       (let-values (((vs rs fs ns)
		     (E e1 vs rs fs ns)))
         (E e0 vs rs fs ns)))

      ((conditional e0 e1 e2)
       (let-values (((vs1 rs1 fs1 ns1)  (E e1 vs rs fs ns))
                    ((vs2 rs2 fs2 ns2)  (E e2 vs rs fs ns)))
         (P e0
            vs1 rs1 fs1 ns1
            vs2 rs2 fs2 ns2
            (union-vars vs1 vs2)
            (union-regs rs1 rs2)
            (union-frms fs1 fs2)
            (union-nfvs ns1 ns2))))

      ((asm-instr op d s)
       (case-symbols op
         ((move load8 load32)
          (cond ((reg? d)
		 (cond ((not (mem-reg? d rs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       ((or (const? s) (disp? s) (reg? s))
			(let ((rs (rem-reg d rs)))
			  (mark-reg/vars-conf! d vs)
			  (R s vs rs fs ns)))
		       ((var? s)
			(let ((rs (rem-reg d rs))
			      (vs (rem-var s vs)))
			  (mark-var/reg-move! s d)
			  (mark-reg/vars-conf! d vs)
			  (values (add-var s vs) rs fs ns)))
		       ((fvar? s)
			(let ((rs (rem-reg d rs)))
			  (mark-reg/vars-conf! d vs)
			  (values vs rs (add-frm s fs) ns)))
		       (else
			(error who "invalid rs" (unparse-recordized-code x)))))
		((fvar? d)
		 (cond ((not (mem-frm? d fs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       ((or (const? s) (disp? s) (reg? s))
			(let ((fs (rem-frm d fs)))
			  (mark-frm/vars-conf! d vs)
			  (mark-frm/nfvs-conf! d ns)
			  (R s vs rs fs ns)))
		       ((var? s)
			(let ((fs (rem-frm d fs))
			      (vs (rem-var s vs)))
			  (mark-var/frm-move! s d)
			  (mark-frm/vars-conf! d vs)
			  (mark-frm/nfvs-conf! d ns)
			  (values (add-var s vs) rs fs ns)))
		       (else
			(error who "invalid fs" s))))
		((var? d)
		 (cond ((not (mem-var? d vs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       ((or (disp? s) (constant? s))
			(let ((vs (rem-var d vs)))
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/regs-conf! d rs)
			  (mark-var/nfvs-conf! d ns)
			  (R s vs rs fs ns)))
		       ((reg? s)
			(let ((vs (rem-var d vs))
			      (rs (rem-reg s rs)))
			  (mark-var/reg-move! d s)
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/regs-conf! d rs)
			  (mark-var/nfvs-conf! d ns)
			  (values vs (add-reg s rs) fs ns)))
		       ((var? s)
			(let ((vs (rem-var d (rem-var s vs))))
			  (mark-var/var-move! d s)
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/regs-conf! d rs)
			  (mark-var/nfvs-conf! d ns)
			  (values (add-var s vs) rs fs ns)))
		       ((fvar? s)
			(let ((vs (rem-var d vs))
			      (fs (rem-frm s fs)))
			  (mark-var/frm-move! d s)
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/regs-conf! d rs)
			  (mark-var/nfvs-conf! d ns)
			  (values vs rs (add-frm s fs) ns)))
		       (else
			(error who "invalid vs" s))))
		((nfv? d)
		 (cond ((not (mem-nfv? d ns))
			(error who "dead nfv"))
		       ((or (disp?     s)
			    (constant? s)
			    (reg?      s))
			(let ((ns (rem-nfv d ns)))
			  (mark-nfv/vars-conf! d vs)
			  (mark-nfv/frms-conf! d fs)
			  (R s vs rs fs ns)))
		       ((var? s)
			(let ((ns (rem-nfv d ns))
			      (vs (rem-var s vs)))
			  (mark-nfv/vars-conf! d vs)
			  (mark-nfv/frms-conf! d fs)
			  (values (add-var s vs) rs fs ns)))
		       ((fvar? s)
			(let ((ns (rem-nfv d ns))
			      (fs (rem-frm s fs)))
			  (mark-nfv/vars-conf! d vs)
			  (mark-nfv/frms-conf! d fs)
			  (values vs rs (add-frm s fs) ns)))
		       (else
			(error who "invalid ns" s))))
		(else
		 (error who "invalid d" d))))
         ((int-/overflow int+/overflow int*/overflow)
          (let ((v (exception-live-set)))
            (unless (vector? v)
              (error who "unbound exception" x v))
            (let ((vs (union-vars vs ($vector-ref v 0)))
                  (rs (union-regs rs ($vector-ref v 1)))
                  (fs (union-frms fs ($vector-ref v 2)))
                  (ns (union-nfvs ns ($vector-ref v 3))))
              (cond ((var? d)
		     (cond ((not (mem-var? d vs))
			    (set-asm-instr-op! x 'nop)
			    (values vs rs fs ns))
			   (else
			    (let ((vs (rem-var d vs)))
			      (mark-var/vars-conf! d vs)
			      (mark-var/frms-conf! d fs)
			      (mark-var/nfvs-conf! d ns)
			      (mark-var/regs-conf! d rs)
			      (R s (add-var d vs) rs fs ns)))))
		    ((reg? d)
		     (if (not (mem-reg? d rs))
			 (values vs rs fs ns)
		       (let ((rs (rem-reg d rs)))
			 (mark-reg/vars-conf! d vs)
			 (R s vs (add-reg d rs) fs ns))))
		    ((nfv? d)
		     (if (not (mem-nfv? d ns))
			 (error who "dead nfv")
		       (let ((ns (rem-nfv d ns)))
			 (mark-nfv/vars-conf! d vs)
			 (mark-nfv/frms-conf! d fs)
			 (R s vs rs fs (add-nfv d ns)))))
		    (else
		     (error who "invalid op d" (unparse-recordized-code x)))))))
         ((nop)
	  (values vs rs fs ns))
         ((logand logor logxor sll sra srl int+ int- int* bswap!
           sll/overflow)
          (cond ((var? d)
		 (cond ((not (mem-var? d vs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       (else
			(let ((vs (rem-var d vs)))
			  (mark-var/vars-conf! d vs)
			  (mark-var/frms-conf! d fs)
			  (mark-var/nfvs-conf! d ns)
			  (mark-var/regs-conf! d rs)
			  (R s (add-var d vs) rs fs ns)))))
		((reg? d)
		 (cond ((not (mem-reg? d rs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       (else
			(let ((rs (rem-reg d rs)))
			  (mark-reg/vars-conf! d vs)
			  (R s vs (add-reg d rs) fs ns)))))
		((nfv? d)
		 (if (not (mem-nfv? d ns))
		     (error who "dead nfv")
		   (let ((ns (rem-nfv d ns)))
		     (mark-nfv/vars-conf! d vs)
		     (mark-nfv/frms-conf! d fs)
		     (R s vs rs fs (add-nfv d ns)))))
		(else
		 (error who "invalid op d" (unparse-recordized-code x)))))
         ((idiv)
          (mark-reg/vars-conf! eax vs)
          (mark-reg/vars-conf! edx vs)
          (R s vs (add-reg eax (add-reg edx rs)) fs ns))
         ((cltd)
          (mark-reg/vars-conf! edx vs)
          (R s vs (rem-reg edx rs) fs ns))
         ((mset mset32 bset
           fl:load fl:store fl:add! fl:sub! fl:mul! fl:div! fl:from-int
           fl:shuffle fl:load-single fl:store-single)
          (R* (list s d) vs rs fs ns))
         (else
	  (error who "invalid effect op" (unparse-recordized-code x)))))

      ((ntcall target value args mask size)
       (set! spill-set (union-vars vs spill-set))
       (for-each-var vs varvec (lambda (x)
				 ($set-var-loc! x #t)))
       (R* args vs (empty-reg-set) fs ns))

      ((nframe nfvs live body)
       (for-each init-nfv! nfvs)
       (set-nframe-live! x (vector vs fs ns))
       (E body vs rs fs ns))

      ((primcall op args)
       (case-symbols op
         ((nop fl:double->single fl:single->double)
	  (values vs rs fs ns))
         ((interrupt incr/zero?)
          (let ((v (exception-live-set)))
            (unless (vector? v)
              (error who "unbound exception2"))
            (values ($vector-ref v 0)
                    ($vector-ref v 1)
                    ($vector-ref v 2)
                    ($vector-ref v 3))))
         (else
	  (error who "invalid effect op" op))))

      ((shortcut body handler)
       (let-values (((vsh rsh fsh nsh)
		     (E handler vs rs fs ns)))
	 (parameterize ((exception-live-set (vector vsh rsh fsh nsh)))
            (E body vs rs fs ns))))

      (else
       (error who "invalid effect" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define (P x vst rst fst nst
               vsf rsf fsf nsf
               vsu rsu fsu nsu)
    (struct-case x
      ((seq e0 e1)
       (let-values (((vs rs fs ns)
                     (P e1 vst rst fst nst
                        vsf rsf fsf nsf
                        vsu rsu fsu nsu)))
         (E e0 vs rs fs ns)))

      ((conditional e0 e1 e2)
       (let-values (((vs1 rs1 fs1 ns1)
                     (P e1 vst rst fst nst
                           vsf rsf fsf nsf
			   vsu rsu fsu nsu))
                    ((vs2 rs2 fs2 ns2)
                     (P e2 vst rst fst nst
                           vsf rsf fsf nsf
			   vsu rsu fsu nsu)))
         (P e0
            vs1 rs1 fs1 ns1
            vs2 rs2 fs2 ns2
            (union-vars vs1 vs2)
            (union-regs rs1 rs2)
            (union-frms fs1 fs2)
            (union-nfvs ns1 ns2))))

      ((constant t)
       (if t
           (values vst rst fst nst)
           (values vsf rsf fsf nsf)))

      ((asm-instr op d s)
       (R* (list d s) vsu rsu fsu nsu))

      ((shortcut body handler)
       (let-values (((vsh rsh fsh nsh)
                     (P handler vst rst fst nst
                                vsf rsf fsf nsf
                                vsu rsu fsu nsu)))
          (parameterize ((exception-live-set (vector vsh rsh fsh nsh)))
            (P body vst rst fst nst
                    vsf rsf fsf nsf
                    vsu rsu fsu nsu))))

      (else
       (error who "invalid pred" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define (T x)
    (struct-case x
      ((seq e0 e1)
       (let-values (((vs rs fs ns) (T e1)))
         (E e0 vs rs fs ns)))

      ((conditional e0 e1 e2)
       (let-values (((vs1 rs1 fs1 ns1) (T e1))
                    ((vs2 rs2 fs2 ns2) (T e2)))
         (P e0
            vs1 rs1 fs1 ns1
            vs2 rs2 fs2 ns2
            (union-vars vs1 vs2)
            (union-regs rs1 rs2)
            (union-frms fs1 fs2)
            (union-nfvs ns1 ns2))))

      ((primcall op arg*)
       (case-symbols op
         ((return indirect-jump direct-jump)
          (R* arg* (empty-var-set)
              (empty-reg-set)
              (empty-frm-set)
              (empty-nfv-set)))
         (else
	  (error who "invalid tail op" x))))

      ((shortcut body handler)
       (let-values (((vsh rsh fsh nsh) (T handler)))
          (parameterize ((exception-live-set (vector vsh rsh fsh nsh)))
             (T body))))

      (else
       (error who "invalid tail" x))))

;;; --------------------------------------------------------------------

  (define exception-live-set
    (make-parameter #f))

  (T x)
  spill-set)


(module (alt-cogen.assign-frame-sizes)
  (import IntegerSet)
  (import conflict-helpers)
  (import (only (ikarus system $vectors)
		$vector-ref
		$vector-set!))
  (define who 'alt-cogen.assign-frame-sizes)

  (define (alt-cogen.assign-frame-sizes x)
    (let ((v (Program x)))
      v))

  (module (Program)
    ;;The purpose of  this module is to apply the  function "Main" below
    ;;to all the bodies.
    ;;
    (define (Program x)
      (struct-case x
	((codes code* body)
	 (make-codes (map Clambda code*) (Main body)))))

    (define (Clambda x)
      (struct-case x
	((clambda label case* cp free* name)
	 (make-clambda label (map ClambdaCase case*) cp free* name))))

    (define (ClambdaCase x)
      (struct-case x
	((clambda-case info body)
	 (make-clambda-case info (Main body)))))

    (define (Main x)
      ;;X must  be a struct instance  of type LOCALS.  Update  the field
      ;;VARS of X and return a  new struct instance of type LOCALS which
      ;;is meant to replace X.
      ;;
      (struct-case x
	((locals vars body)
	 (init-vars! vars)
	 (let* ((varvec		(list->vector vars))
		(call-live*	(uncover-frame-conflicts body varvec))
		(body		(%rewrite body varvec)))
	   (make-locals (cons varvec (%discard-vars-with-loc vars))
			body)))
	(else
	 (error who "invalid main" x))))

    (define (%discard-vars-with-loc vars)
      ;;Given a list of struct instances  of type VAR, return a new list
      ;;containing only those having #f in the LOC field.
      ;;
      (cond ((null? vars)
	     '())
	    (($var-loc ($car vars))
	     (%discard-vars-with-loc ($cdr vars)))
	    (else
	     (cons ($car vars) (%discard-vars-with-loc ($cdr vars))))))

    #| end of module: Program |# )

;;; --------------------------------------------------------------------

  (define (%rewrite x varvec)
    ;;X must be a struct instance representing a recordized body.
    ;;
    ;;A lot of functions are nested here because they need to close upon
    ;;the argument VARVEC.
    ;;
    (define (NFE idx mask x)
      (struct-case x
        ((seq e0 e1)
         (let ((e0^ (E e0)))
           (make-seq e0^ (NFE idx mask e1))))
        ((ntcall target value args mask^ size)
         (make-ntcall target value
            (map (lambda (x)
                   (cond ((symbol? x)
			  x)
			 ((nfv? x)
			  ($nfv-loc x))
			 (else
			  (error who "invalid arg"))))
                 args)
            mask idx))
        (else
	 (error who "invalid NF effect" x))))

    (define (Var x)
      (cond (($var-loc x)
	     => (lambda (loc)
		  (if (fvar? loc)
		      loc
		    (%assign x varvec))))
	    (else x)))

    (define (R x)
      (cond ((or (constant? x)
		 (reg?      x)
		 (fvar?     x))
	     x)
	    ((nfv? x)
	     (or ($nfv-loc x)
		 (error who "unassigned nfv")))
	    ((var? x)
	     (Var x))
	    ((disp? x)
	     (make-disp (R ($disp-s0 x)) (R ($disp-s1 x))))
	    (else
	     (error who "invalid R" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

    (module (E)

      (define (E x)
	(struct-case x
	  ((seq e0 e1)
	   (let ((e0^ (E e0)))
	     (make-seq e0^ (E e1))))

	  ((conditional e0 e1 e2)
	   (make-conditional (P e0) (E e1) (E e2)))

	  ((asm-instr op d s)
	   (E-asm-instr op d s))

	  ((nframe vars live body)
	   (E-nframe vars live body))

	  ((primcall op args)
	   (case-symbols op
	     ((nop interrupt incr/zero? fl:double->single fl:single->double)
	      x)
	     (else
	      (error who "invalid effect prim" op))))

	  ((shortcut body handler)
	   (make-shortcut (E body) (E handler)))

	  (else
	   (error who "invalid effect" (unparse-recordized-code x)))))

      (define (E-asm-instr op d s)
	(case-symbols op
	  ((move load8 load32)
	   ;;If  the   destination  equals  the  source:   convert  this
	   ;;instruction into a NOP.
	   (let ((d (R d))
		 (s (R s)))
	     (if (eq? d s)
		 (make-primcall 'nop '())
	       (make-asm-instr op d s))))

	  (( ;;some assembly instructions
	    logand		logor		logxor
	    int+		int-		int*
	    mset		bset		mset32
	    sll			sra		srl		bswap!
	    cltd		idiv
	    int-/overflow	int+/overflow	int*/overflow
	    fl:load		fl:store
	    fl:add!		fl:sub!		fl:mul!		fl:div!
	    fl:from-int		fl:shuffle	fl:load-single	fl:store-single
	    sll/overflow)
	   (make-asm-instr op (R d) (R s)))

	  ((nop)
	   (make-primcall 'nop '()))

	  (else
	   (error who "invalid op" op))))

      (define (E-nframe vars live body)
	(let ((live-frms1 (map (lambda (i)
				 (Var ($vector-ref varvec i)))
			    (set->list ($vector-ref live 0))))
	      (live-frms2 (set->list ($vector-ref live 1)))
	      (live-nfvs  ($vector-ref live 2)))

	  (define (max-frm ls i)
	    (if (null? ls)
		i
	      (max-frm ($cdr ls) (max i ($fvar-idx ($car ls))))))

	  (define (max-ls ls i)
	    (if (null? ls)
		i
	      (max-ls  ($cdr ls) (max i ($car ls)))))

	  (define (max-nfv ls i)
	    (if (null? ls)
		i
	      (let ((loc ($nfv-loc ($car ls))))
		(unless (fvar? loc)
		  (error who "FVAR not assigned in MAX-NFV" loc))
		(max-nfv ($cdr ls) (max i ($fvar-idx loc))))))

	  (module (actual-frame-size)

	    (define (actual-frame-size vars i)
	      (if (%frame-size-ok? i vars)
		  i
		(actual-frame-size vars ($fxadd1 i))))

	    (define (%frame-size-ok? i vars)
	      (or (null? vars)
		  (let ((x ($car vars)))
		    (and (not (set-member?    i ($nfv-frm-conf x)))
			 (not (%var-conflict? i ($nfv-var-conf x)))
			 (%frame-size-ok? ($fxadd1 i) ($cdr vars))))))

	    (define (%var-conflict? i vs)
	      (ormap (lambda (xi)
		       (let ((loc ($var-loc ($vector-ref varvec xi))))
			 (and (fvar? loc)
			      ($fx= i ($fvar-idx loc)))))
		     (set->list vs)))

	    #| end of module: actual-frame-size |# )

	  (define (%assign-frame-vars! vars i)
	    (unless (null? vars)
	      (let ((v  ($car vars))
		    (fv (mkfvar i)))
		($set-nfv-loc! v fv)
		(for-each (lambda (x)
			    (let ((loc ($nfv-loc x)))
			      (if loc
				  (when ($fx= ($fvar-idx loc) i)
				    (error who "invalid assignment"))
				(begin
				 ($set-nfv-nfv-conf! x (rem-nfv v  ($nfv-nfv-conf x)))
				 ($set-nfv-frm-conf! x (add-frm fv ($nfv-frm-conf x)))))))
		  ($nfv-nfv-conf v))
		(for-each-var ($nfv-var-conf v) varvec
			      (lambda (x)
				(let ((loc ($var-loc x)))
				  (if (fvar? loc)
				      (when (fx= (fvar-idx loc) i)
					(error who "invalid assignment"))
				    ($set-var-frm-conf! x (add-frm fv ($var-frm-conf x))))))))
	      (%assign-frame-vars! ($cdr vars) ($fxadd1 i))))

	  (module (make-mask)

	    (define (make-mask n)
	      (let ((vec (make-vector ($fxsra ($fx+ n 7) 3) 0)))
		(for-each (lambda (fvar)
			    (%set-bit! vec ($fvar-idx fvar)))
		  live-frms1)
		(for-each (lambda (idx)
			    (%set-bit! vec idx))
		  live-frms2)
		(for-each (lambda (nfv)
			    (let ((loc ($nfv-loc nfv)))
			      (when loc
				(%set-bit! vec ($fvar-idx loc)))))
		  live-nfvs)
		vec))

	    (define (%set-bit! vec idx)
	      (let ((q ($fxsra    idx 3))
		    (r ($fxlogand idx 7)))
		($vector-set! vec q ($fxlogor ($vector-ref vec q) ($fxsll 1 r)))))

	    #| end of module: make-mask |# )

	  (let ((i (actual-frame-size
		    vars
		    ($fx+ 2 (max-frm live-frms1
				     (max-nfv live-nfvs
					      (max-ls live-frms2 0)))))))
	    (%assign-frame-vars! vars i)
	    (NFE (fxsub1 i) (make-mask (fxsub1 i)) body))))

      #| end of module: E |# )

;;; --------------------------------------------------------------------

    (define (P x)
      (struct-case x
        ((seq e0 e1)
         (let ((e0^ (E e0)))
           (make-seq e0^ (P e1))))

        ((conditional e0 e1 e2)
         (make-conditional (P e0) (P e1) (P e2)))

        ((asm-instr op d s)
	 (make-asm-instr op (R d) (R s)))

        ((constant)
	 x)

        ((shortcut body handler)
         (make-shortcut (P body) (P handler)))

        (else
	 (error who "invalid pred" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

    (define (T x)
      (struct-case x
        ((seq e0 e1)
         (let ((e0^ (E e0)))
           (make-seq e0^ (T e1))))

        ((conditional e0 e1 e2)
         (make-conditional (P e0) (T e1) (T e2)))

        ((primcall op args)
	 x)

        ((shortcut body handler)
         (make-shortcut (T body) (T handler)))

        (else
	 (error who "invalid tail" (unparse-recordized-code x)))))

    (T x))

;;; --------------------------------------------------------------------

  (module (%assign)

    (define (%assign x varvec)
      (or (%assign-move x varvec)
	  (%assign-any  x varvec)))

    (define (%assign-any x varvec)
      (let ((frms ($var-frm-conf x))
	    (vars ($var-var-conf x)))
	(let loop ((i 1))
	  (if (set-member? i frms)
	      (loop ($fxadd1 i))
	    (let ((fv (mkfvar i)))
	      ($set-var-loc! x fv)
	      (for-each-var vars varvec
			    (lambda (var)
			      ($set-var-frm-conf! var (add-frm fv ($var-frm-conf var)))))
	      fv)))))

    (define (%assign-move x varvec)
      (let ((mr (set->list (set-difference ($var-frm-move x) ($var-frm-conf x)))))
	(if (null? mr)
	    #f
	  (let ((fv (mkfvar ($car mr))))
	    ($set-var-loc! x fv)
	    (for-each-var ($var-var-conf x) varvec
			  (lambda (var)
			    ($set-var-frm-conf! var (add-frm fv ($var-frm-conf var)))))
	    (for-each-var ($var-var-move x) varvec
			  (lambda (var)
			    ($set-var-frm-move! var (add-frm fv ($var-frm-move var)))))
	    fv))))

    #| end of module: assign |# )

  #| end of module: alt-cogen.assign-frame-sizes |# )


(module (alt-cogen.color-by-chaitin)
  (import ListySet)
  (import ListyGraphs)
  ;(import IntegerSet)
  ;(import IntegerGraphs)

  (define (alt-cogen.color-by-chaitin x)
    (Program x))

  (module (Program)
    ;;The purpose of this module is to apply the function %COLOR-PROGRAM
    ;;below to all the bodies.
    ;;
    (define (Program x)
      (struct-case x
        ((codes code* body)
         (make-codes (map Clambda code*) (%color-program body)))))

    (define (Clambda x)
      (struct-case x
        ((clambda label case* cp free* name)
         (make-clambda label (map ClambdaCase case*) cp free* name))))

    (define (ClambdaCase x)
      (struct-case x
        ((clambda-case info body)
         (make-clambda-case info (%color-program body)))))

    (define (%color-program x)
      (define who '%color-program)
      (struct-case x
	((locals x.vars x.body)
	 (let ((varvec ($car x.vars))
	       (sp*    ($cdr x.vars)))
	   (let loop ((sp*^ (list->set sp*))
		      (un*  (make-empty-set))
		      (body x.body))
	     (let-values (((un*^ body^) (add-unspillables un* body)))
	       (let ((G (build-graph body^)))
		 (let-values (((spills sp*^^ env) (color-graph sp*^ un*^ G)))
		   (if (null? spills)
		       (substitute env body^)
		     (let* ((env^   (do-spill spills varvec))
			    (body^^ (substitute env^ body^)))
		       (loop sp*^^ un*^ body^^)))))))))))

    #| end of module: Program |# )

;;; --------------------------------------------------------------------
;;; helpers

  (define-inline (set-for-each f s)
    (for-each f (set->list s)))

;;; --------------------------------------------------------------------

  (define (build-graph x)
    ;;
    ;;A lot of functions are nested here because they need to close upon
    ;;GRAPH.
    ;;
    (define who 'build-graph)
    (define GRAPH
      (empty-graph))
    (define exception-live-set
      (make-parameter #f))

    (define (R* ls)
      (if (null? ls)
	  (make-empty-set)
	(set-union (R  ($car ls))
		   (R* ($cdr ls)))))

    (define (R x)
      (struct-case x
        ((constant)
	 (make-empty-set))
        ((var)
	 (singleton x))
        ((disp s0 s1)
	 (set-union (R s0) (R s1)))
        ((fvar)
	 (make-empty-set))
        ((code-loc)
	 (make-empty-set))
        (else
         (if (symbol? x)
	     (if (memq x ALL-REGISTERS)
		 (set-add x (make-empty-set))
	       (make-empty-set))
	   (error who "invalid R" x)))))

    (module (E)

      (define (E x s)
	(struct-case x
	  ((asm-instr op d v)
	   (E-asm-instr op d v s))

	  ((seq e0 e1)
	   (E e0 (E e1 s)))

	  ((conditional e0 e1 e2)
	   (let ((s1 (E e1 s))
		 (s2 (E e2 s)))
	     (P e0 s1 s2 (set-union s1 s2))))

	  ((ntcall targ value args mask size)
	   (set-union (R* args) s))

	  ((primcall op arg*)
	   (case-symbols op
	     ((nop fl:single->double fl:double->single)
	      s)
	     ((interrupt incr/zero?)
	      (or (exception-live-set)
		  (error who "uninitialized exception")))
	     (else
	      (error who "invalid effect primcall" op))))

	  ((shortcut body handler)
	   (let ((s2 (E handler s)))
	     (parameterize ((exception-live-set s2))
	       (E body s))))

	  (else
	   (error who "invalid effect" (unparse-recordized-code x)))))

      (define (E-asm-instr op d v s)
	(case-symbols op
	  ((move load32)
	   (let ((s (set-rem d s)))
	     (set-for-each (lambda (y)
			     (add-edge! GRAPH d y))
			   s)
	     (set-union (R v) s)))

	  ((load8)
	   (let ((s (set-rem d s)))
	     (set-for-each (lambda (y)
			     (add-edge! GRAPH d y))
			   s)
	     (when (var? d)
	       (for-each (lambda (r)
			   (add-edge! GRAPH d r))
		 NON-8BIT-REGISTERS))
	     (when (var? v)
	       (for-each (lambda (r)
			   (add-edge! GRAPH v r))
		 NON-8BIT-REGISTERS))
	     (set-union (R v) s)))

	  ((int-/overflow int+/overflow int*/overflow)
	   (unless (exception-live-set)
	     (error who "uninitialized live set"))
	   (let ((s (set-rem d (set-union s (exception-live-set)))))
	     (set-for-each (lambda (y)
			     (add-edge! GRAPH d y))
			   s)
	     (set-union (set-union (R v) (R d)) s)))

	  ((logand logxor int+ int- int* logor sll sra srl bswap! sll/overflow)
	   (let ((s (set-rem d s)))
	     (set-for-each (lambda (y)
			     (add-edge! GRAPH d y))
			   s)
	     (set-union (set-union (R v) (R d)) s)))

	  ((bset)
	   (when (var? v)
	     (for-each (lambda (r)
			 (add-edge! GRAPH v r))
	       NON-8BIT-REGISTERS))
	   (set-union (set-union (R v) (R d)) s))

	  ((cltd)
	   (let ((s (set-rem edx s)))
	     (when (register? edx)
	       (set-for-each (lambda (y)
			       (add-edge! GRAPH edx y))
			     s))
	     (set-union (R eax) s)))

	  ((idiv)
	   (let ((s (set-rem eax (set-rem edx s))))
	     (when (register? eax)
	       (set-for-each (lambda (y)
			       (add-edge! GRAPH eax y)
			       (add-edge! GRAPH edx y))
			     s))
	     (set-union (set-union (R eax) (R edx))
			(set-union (R v) s))))

	  (( ;;some assembly instructions
	    mset		mset32
	    fl:load		fl:store
	    fl:add!		fl:sub!
	    fl:mul!		fl:div!
	    fl:from-int		fl:shuffle
	    fl:store-single	fl:load-single)
	   (set-union (R v) (set-union (R d) s)))

	  (else
	   (error who "invalid effect" x))))

      #| end of module: E |# )

    (define (P x st sf su)
      (struct-case x
        ((constant c)
	 (if c st sf))

        ((seq e0 e1)
         (E e0 (P e1 st sf su)))

        ((conditional e0 e1 e2)
         (let ((s1 (P e1 st sf su))
	       (s2 (P e2 st sf su)))
           (P e0 s1 s2 (set-union s1 s2))))

        ((asm-instr op s0 s1)
         (set-union (set-union (R s0) (R s1)) su))

        ((shortcut body handler)
         (let ((s2 (P handler st sf su)))
           (parameterize ((exception-live-set s2))
             (P body st sf su))))

        (else
	 (error who "invalid pred" (unparse-recordized-code x)))))

    (define (T x)
      (struct-case x
        ((conditional e0 e1 e2)
         (let ((s1 (T e1))
	       (s2 (T e2)))
           (P e0 s1 s2 (set-union s1 s2))))

        ((primcall op rands)
         (R* rands))

        ((seq e0 e1)
	 (E e0 (T e1)))

        ((shortcut body handler)
         (let ((s2 (T handler)))
           (parameterize ((exception-live-set s2))
              (T body))))

        (else
	 (error who "invalid tail" (unparse-recordized-code x)))))

    (let ((s (T x)))
      ;;(pretty-print (unparse-recordized-code x))
      ;;(print-graph GRAPH)
      GRAPH))

;;; --------------------------------------------------------------------

  (module (color-graph)

    (define (color-graph sp* un* G)
      (cond ((and (empty-set? sp*)
		  (empty-set? un*))
	     (values '() (make-empty-set) '()))

	    ((find-low-degree (set->list un*) G)
	     => (lambda (un)
		  (let ((n* (node-neighbors un G)))
		    (delete-node! un G)
		    (let-values (((spills sp* env)
				  (color-graph sp* (set-rem un un*) G)))
		      (let ((r (find-color un n* env)))
			(values spills sp* (cons (cons un r) env)))))))

	    ((find-low-degree (set->list sp*) G)
	     => (lambda (sp)
		  (let ((n* (node-neighbors sp G)))
		    (delete-node! sp G)
		    (let-values (((spills sp* env)
				  (color-graph (set-rem sp sp*) un* G)))
		      (let ((r (find-color sp n* env)))
			(values spills (set-add sp sp*) (cons (cons sp r) env)))))))

	    ((pair? (set->list sp*))
	     (let* ((sp ($car (set->list sp*)))
		    (n* (node-neighbors sp G)))
	       (delete-node! sp G)
	       (let-values (((spills sp* env)
			     (color-graph (set-rem sp sp*) un* G)))
		 (let ((r (find-color/maybe sp n* env)))
		   (if r
		       (values spills (set-add sp sp*) (cons (cons sp r) env))
		     (values (cons sp spills) sp* env))))))

	    (else
	     (error 'color-graph "whoaaa"))))

    (define (find-low-degree ls G)
      (cond ((null? ls)
	     #f)
	    ((fx< (length (set->list (node-neighbors ($car ls) G)))
		  (length ALL-REGISTERS))
	     ($car ls))
	    (else
	     (find-low-degree ($cdr ls) G))))

    (define (find-color/maybe x confs env)
      (let ((cr (map (lambda (x)
                       (cond ((symbol? x)
			      x)
			     ((assq x env)
			      => cdr)
			     (else
			      #f)))
		  (set->list confs))))
        (let ((r* (set->list (set-difference (list->set ALL-REGISTERS)
					     (list->set cr)))))
          (if (null? r*)
              #f
	    ($car r*)))))

    (define (find-color x confs env)
      (or (find-color/maybe x confs env)
          (error 'find-color "cannot find color for" x)))

    #| end of module: color-graph |# )

;;; --------------------------------------------------------------------

  (define (substitute env x)
    ;;X must represent recordized code; this function builds and returns
    ;;a new struct instance representing recordized code, which is meant
    ;;to replace X.
    ;;
    ;;The purpose of this function is  to apply the subfunction R to the
    ;;operands in the structures of type ASM-INSTR and PRIMCALL.
    ;;
    ;;A lot  of functions are nested  here because they make  use of the
    ;;subfunction "Var", and "Var" needs to close upon the argument ENV.
    ;;
    (define who 'substitute)

    (module (R)

      (define (R x)
	(struct-case x
	  ((constant)
	   x)
	  ((var)
	   (Var x))
	  ((fvar)
	   x)
	  ((nfv c loc)
	   (or loc
	       (error who "unset nfv in R" x)))
	  ((disp s0 s1)
	   (make-disp (D s0) (D s1)))
	  (else
	   (if (symbol? x)
	       x
	     (error who "invalid R" x)))))

      (define (D x)
	(struct-case x
	  ((constant)
	   x)
	  ((var)
	   (Var x))
	  ((fvar)
	   x)
	  (else
	   (if (symbol? x)
	       x
	     (error who "invalid D" x)))))

      (define (Var x)
	(cond ((assq x env)
	       => cdr)
	      (else
	       x)))

      ;;Commented out because unused.  (Marco Maggi; Oct 29, 2012)
      ;;
      ;; (module (Rhs)
      ;;
      ;;   (define (Rhs x)
      ;; 	(struct-case x
      ;; 	  ((var)
      ;; 	   (Var x))
      ;; 	  ((primcall op rand*)
      ;; 	   (make-primcall op (map Rand rand*)))
      ;; 	  (else x)))
      ;;
      ;;   (define (Rand x)
      ;; 	(struct-case x
      ;; 	  ((var)
      ;; 	   (Var x))
      ;; 	  (else x)))
      ;;
      ;;   #| end of module: Rhs |# )

      ;;Commented out because unused.  (Marco Maggi; Oct 29, 2012)
      ;;
      ;; (define (Lhs x)
      ;;   (struct-case x
      ;;     ((var)
      ;; 	 (Var x))
      ;;     ((nfv confs loc)
      ;;      (or loc
      ;; 	     (error who "LHS not set" x)))
      ;;     (else x)))

      #| end of module: R |# )

    (define (E x)
      ;;substitute effect
      (struct-case x
        ((seq e0 e1)
	 (make-seq (E e0) (E e1)))
        ((conditional e0 e1 e2)
         (make-conditional (P e0) (E e1) (E e2)))
        ((asm-instr op x v)
         (make-asm-instr op (R x) (R v)))
        ((primcall op rands)
         (make-primcall op (map R rands)))
        ((ntcall)
	 x)
        ((shortcut body handler)
         (make-shortcut (E body) (E handler)))
        (else
	 (error who "invalid effect" (unparse-recordized-code x)))))

    (define (P x)
      (struct-case x
        ((constant)
	 x)
        ((asm-instr op x v)
         (make-asm-instr op (R x) (R v)))
        ((conditional e0 e1 e2)
         (make-conditional (P e0) (P e1) (P e2)))
        ((seq e0 e1)
	 (make-seq (E e0) (P e1)))
        ((shortcut body handler)
         (make-shortcut (P body) (P handler)))
        (else
	 (error who "invalid pred" (unparse-recordized-code x)))))

    (define (T x)
      (struct-case x
        ((primcall op rands)
	 x)
        ((conditional e0 e1 e2)
         (make-conditional (P e0) (T e1) (T e2)))
        ((seq e0 e1)
	 (make-seq (E e0) (T e1)))
        ((shortcut body handler)
         (make-shortcut (T body) (T handler)))
        (else
	 (error who "invalid tail" (unparse-recordized-code x)))))

    ;;(print-code x)
    (T x))

;;; --------------------------------------------------------------------

  (define (do-spill sp* varvec)
    (import conflict-helpers)
    (define (find/set-loc x)
      (let loop ((i    1)
		 (conf ($var-frm-conf x)))
        (let ((fv (mkfvar i)))
          (if (mem-frm? fv conf)
	      (loop ($fxadd1 i) conf)
	    (begin
	      (for-each-var ($var-var-conf x) varvec
			    (lambda (y)
			      ($set-var-var-conf! y (rem-var x  ($var-var-conf y)))
			      ($set-var-frm-conf! y (add-frm fv ($var-frm-conf y)))))
	      ($set-var-loc! x fv)
	      (cons x fv))))))
    (map find/set-loc sp*))

;;; --------------------------------------------------------------------

  (module (add-unspillables)

    (define (add-unspillables un* x)
      ;;
      ;;A  lot  of  functions  are  nested  here  because  they  call  the
      ;;subfunction MKU, and the MKU needs to close upon the argument UN*.
      ;;
      (define who 'add-unspillables)

      (define (mku)
	(let ((u (unique-var 'u)))
	  (set! un* (set-add u un*))
	  u))

      (module (E)

	(define (E x)
	  ;;unspillable effect
	  (struct-case x
	    ((seq e0 e1)
	     (make-seq (E e0) (E e1)))

	    ((conditional e0 e1 e2)
	     (make-conditional (P e0) (E e1) (E e2)))

	    ((asm-instr op a b)
	     (E-asm-instr op a b x))

	    ((primcall op rands)
	     (case-symbols op
	       ((nop interrupt incr/zero? fl:single->double fl:double->single)
		x)
	       (else
		(error who "invalid op in" (unparse-recordized-code x)))))

	    ((ntcall)
	     x)

	    ((shortcut body handler)
	     ;;Do BODY first, then HANDLER.
	     (let ((body^ (E body)))
	       (make-shortcut body^ (E handler))))

	    (else
	     (error who "invalid effect" (unparse-recordized-code x)))))

	(define (E-asm-instr op a b x)
	  (case-symbols op
	    ((load8 load32)
	     (%fix-address b (lambda (b)
			       (if (or (register? a) (var? a))
				   (make-asm-instr op a b)
				 (let ((u (mku)))
				   (make-seq (make-asm-instr op u b)
					     (E (make-asm-instr 'move a u))))))))

	    ((logor logxor logand int+ int- int* move int-/overflow int+/overflow int*/overflow)
	     (cond ((and (eq? op 'move)
			 (eq? a b))
		    ;;Source and dest are the same: do nothing.
		    (make-primcall 'nop '()))
		   ((and (= wordsize 8)
			 (not (eq? op 'move))
			 (long-imm? b))
		    (let ((u (mku)))
		      (make-seq (E (make-asm-instr 'move u b))
				(E (make-asm-instr op a u)))))
		   ((and (memq op '(int* int*/overflow))
			 (mem? a))
		    (let ((u (mku)))
		      (make-seq (make-seq (E (make-asm-instr 'move u a))
					  (E (make-asm-instr op u b)))
				(E (make-asm-instr 'move a u)))))
		   ((and (mem? a)
			 (not (small-operand? b)))
		    (let ((u (mku)))
		      (make-seq (E (make-asm-instr 'move u b))
				(E (make-asm-instr op a u)))))
		   ((disp? a)
		    (let ((s0 (disp-s0 a))
			  (s1 (disp-s1 a)))
		      (cond ((not (small-operand? s0))
			     (let ((u (mku)))
			       (make-seq (E (make-asm-instr 'move u s0))
					 (E (make-asm-instr op (make-disp u s1) b)))))
			    ((not (small-operand? s1))
			     (let ((u (mku)))
			       (make-seq (E (make-asm-instr 'move u s1))
					 (E (make-asm-instr op (make-disp s0 u) b)))))
			    ((small-operand? b) x)
			    (else
			     (let ((u (mku)))
			       (make-seq (E (make-asm-instr 'move u b))
					 (E (make-asm-instr op a u))))))))
		   ((disp? b)
		    (let ((s0 (disp-s0 b))
			  (s1 (disp-s1 b)))
		      (cond ((not (small-operand? s0))
			     (let ((u (mku)))
			       (make-seq (E (make-asm-instr 'move u s0))
					 (E (make-asm-instr op a (make-disp u s1))))))
			    ((not (small-operand? s1))
			     (let ((u (mku)))
			       (make-seq (E (make-asm-instr 'move u s1))
					 (E (make-asm-instr op a (make-disp s0 u))))))
			    (else
			     x))))
		   (else
		    x)))

	    ((bswap!)
	     (if (mem? b)
		 (let ((u (mku)))
		   (make-seq (make-seq (E (make-asm-instr 'move u a))
				       (E (make-asm-instr 'bswap! u u)))
			     (E (make-asm-instr 'move b u))))
	       x))

	    ((cltd)
	     (unless (and (symbol? a)
			  (symbol? b))
	       (error who "invalid args to cltd"))
	     x)

	    ((idiv)
	     (unless (symbol? a)
	       (error who "invalid arg to idiv"))
	     (if (or (var? b)
		     (symbol? b))
		 x
	       (let ((u (mku)))
		 (make-seq (E (make-asm-instr 'move u b))
			   (E (make-asm-instr 'idiv a u))))))

	    ((sll sra srl sll/overflow)
	     (unless (or (constant? b)
			 (eq? b ecx))
	       (error who "invalid shift" b))
	     x)

	    ((mset mset32 bset)
	     (if (not (small-operand? b))
		 (let ((u (mku)))
		   (make-seq (E (make-asm-instr 'move u b))
			     (E (make-asm-instr op a u))))
	       (check-disp a
			   (lambda (a)
			     (let ((s0 (disp-s0 a))
				   (s1 (disp-s1 a)))
			       (if (and (constant? s0)
					(constant? s1))
				   (let ((u (mku)))
				     (make-seq (make-seq (E (make-asm-instr 'move u s0))
							 (E (make-asm-instr 'int+ u s1)))
					       (make-asm-instr op
							       (make-disp u (make-constant 0))
							       b)))
				 (make-asm-instr op a b)))))))

	    ((fl:load fl:store fl:add! fl:sub! fl:mul! fl:div! fl:load-single fl:store-single)
	     (check-disp-arg a (lambda (a)
				 (check-disp-arg b (lambda (b)
						     (make-asm-instr op a b))))))

	    ((fl:from-int fl:shuffle)
	     x)

	    (else
	     (error who "invalid effect op" op))))

	(define (%fix-address x kont)
	  ;;Recursive function.
	  ;;
	  (if (disp? x)
	      (let ((s0 (disp-s0 x))
		    (s1 (disp-s1 x)))
		(cond ((not (small-operand? s0))
		       (let ((u (mku)))
			 (make-seq (E (make-asm-instr 'move u s0))
				   (%fix-address (make-disp u s1) kont))))
		      ((not (small-operand? s1))
		       (let ((u (mku)))
			 (make-seq (E (make-asm-instr 'move u s1))
				   (%fix-address (make-disp s0 u) kont))))
		      (else
		       (kont x))))
	    (kont x)))

	#| end of module: E |# )

;;; --------------------------------------------------------------------

      (define (check-disp-arg x kont)
	(if (small-operand? x)
	    (kont x)
	  (let ((u (mku)))
	    (make-seq (E (make-asm-instr 'move u x))
		      (kont u)))))

      (define (check-disp x kont)
	(struct-case x
	  ((disp a b)
	   (check-disp-arg a (lambda (a)
			       (check-disp-arg b (lambda (b)
						   (kont (make-disp a b)))))))
	  (else
	   (kont x))))

;;; --------------------------------------------------------------------

      (define (P x)
	(struct-case x
	  ((constant)
	   x)

	  ((conditional e0 e1 e2)
	   (make-conditional (P e0) (P e1) (P e2)))

	  ((seq e0 e1)
	   (make-seq (E e0) (P e1)))

	  ((asm-instr op a b)
	   (cond ((memq op '(fl:= fl:< fl:<= fl:> fl:>=))
		  (if (mem? a)
		      (let ((u (mku)))
			(make-seq (E (make-asm-instr 'move u a))
				  (make-asm-instr op u b)))
		    x))
		 ((and (not (mem?           a))
		       (not (small-operand? a)))
		  (let ((u (mku)))
		    (make-seq (E (make-asm-instr 'move u a))
			      (P (make-asm-instr op u b)))))
		 ((and (not (mem?           b))
		       (not (small-operand? b)))
		  (let ((u (mku)))
		    (make-seq (E (make-asm-instr 'move u b))
			      (P (make-asm-instr op a u)))))
		 ((and (mem? a)
		       (mem? b))
		  (let ((u (mku)))
		    (make-seq (E (make-asm-instr 'move u b))
			      (P (make-asm-instr op a u)))))
		 (else
		  (check-disp a (lambda (a)
				  (check-disp b (lambda (b)
						  (make-asm-instr op a b))))))))

	  ((shortcut body handler)
	   ;;Do BODY first, then HANDLER.
	   (let ((body (P body)))
	     (make-shortcut body (P handler))))

	  (else
	   (error who "invalid pred" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

      (define (T x)
	(struct-case x
	  ((primcall op rands)
	   x)

	  ((conditional e0 e1 e2)
	   (make-conditional (P e0) (T e1) (T e2)))

	  ((seq e0 e1)
	   (make-seq (E e0) (T e1)))

	  ((shortcut body handler)
	   (make-shortcut (T body) (T handler)))

	  (else
	   (error who "invalid tail" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

      (let ((x (T x)))
	(values un* x))) ;;end of function ADD-UNSPILLABLES

    (define (long-imm? x)
      ;;Return true if X represents a constant signed integer too big to
      ;;fit in 32-bit.
      ;;
      (struct-case x
	((constant n)
	 (cond ((integer? n)
		(not (<= MIN-SIGNED-32-BIT-INTEGER
			 n
			 MAX-SIGNED-32-BIT-INTEGER)))
	       (else #t)))
	(else #f)))

    (define (small-operand? x)
      (case-word-size
       ((32)
	(not (mem? x)))
       ((64)
	(struct-case x
	  ((constant n)
	   (if (integer? n)
	       (<= MIN-SIGNED-32-BIT-INTEGER
		   n
		   MAX-SIGNED-32-BIT-INTEGER)
	     #f))
	  (else
	   (or (register? x)
	       (var?      x)))))))

    (define (mem? x)
      (or (disp? x) (fvar? x)))

    (define-inline-constant MIN-SIGNED-32-BIT-INTEGER
      (- (expt 2 31)))

    (define-inline-constant MAX-SIGNED-32-BIT-INTEGER
      (- (expt 2 31) 1))

    ;;Commented out because unused.  (Marco Maggi; Oct 29, 2012)
    ;;
    ;; (define (S x kont)
    ;;   (if (or (constant? x)
    ;; 	      (var?      x)
    ;; 	      (symbol?   x))
    ;; 	  (kont x)
    ;; 	(let ((u (mku)))
    ;; 	  (make-seq (E (make-asm-instr 'move u x))
    ;; 		    (kont u)))))
    ;;
    ;; (define (S* ls kont)
    ;;   (if (null? ls)
    ;; 	  (kont '())
    ;; 	(S ($car ls) (lambda (a)
    ;; 		       (S* ($cdr ls) (lambda (d)
    ;; 				       (kont (cons a d))))))))

    #| end of module: add-unspillables |# )

  #| end of module: chaitin module |# )


(define (compile-call-frame framesize livemask-vec multiarg-rp call-sequence)
  (let ((L_CALL (label (gensym))))
    (list 'seq
	  (if (or (= framesize 0)
		  (= framesize 1))
	      '(seq) ;this generates no code
	    `(subl ,(* (fxsub1 framesize) wordsize) ,fpr))
	  (jmp L_CALL)
	  `(byte-vector ,livemask-vec)
	  `(int ,(* framesize wordsize))
	  '(current-frame-offset)
	  multiarg-rp
	  `(pad ,call-instruction-size ,L_CALL ,call-sequence)
	  (if (or (= framesize 0) (= framesize 1))
	      '(seq) ;this generates no code
	    `(addl ,(* (fxsub1 framesize) wordsize) ,fpr)))))


(module (alt-cogen.flatten-codes)

  (define who 'alt-cogen.flatten-codes)

  (define exceptions-conc (make-parameter #f))
  (define exception-label (make-parameter #f))

  (define (alt-cogen.flatten-codes x)
    (Program x))

  (module (Program)

    (define (Program x)
      (struct-case x
	((codes code* body)
	 (cons (cons* 0 (label (gensym))
		      (let ((accum (list '(nop))))
			(parameterize ((exceptions-conc accum))
			  (T body accum))))
	       (map Clambda code*)))))

    (define (Clambda x)
      (struct-case x
	((clambda L case* cp free* name)
	 (cons* (length free*)
		`(name ,name)
		(label L)
		(let ((accum (list '(nop))))
		  (parameterize ((exceptions-conc accum))
		    (let recur ((case* case*))
		      (if (null? case*)
			  (cons `(jmp (label ,(sl-invalid-args-label))) accum)
			(ClambdaCase (car case*)
				     (recur (cdr case*)))))))))))

    (define (ClambdaCase x accum)
      (struct-case x
	((clambda-case info body)
	 (struct-case info
	   ((case-info L args proper)
	    (let ((lothers (unique-label)))
	      (cons* `(cmpl ,(argc-convention
			      (if proper
				  (length (cdr args))
				(length (cddr args))))
			    ,ARGC-REGISTER)
		     (cond (proper
			    `(jne ,lothers))
			   ((> (argc-convention 0)
			       (argc-convention 1))
			    `(jg ,lothers))
			   (else
			    `(jl ,lothers)))
		     (%properize args proper
				 (cons (label L)
				       (T body (cons lothers accum)))))))))))

    (define (%properize args proper accum)
      (if proper
	  accum
	(%handle-vararg (length (cdr args)) accum)))

    (define (%handle-vararg fml-count accum)
      (define CONTINUE_LABEL	(unique-label))
      (define DONE_LABEL	(unique-label))
      (define CONS_LABEL	(unique-label))
      (define LOOP_HEAD		(unique-label))
      (define L_CALL		(unique-label))
      (cons* (cmpl (int (argc-convention (fxsub1 fml-count))) eax)
	     (jl CONS_LABEL)
	     (movl (int nil) ebx)
	     (jmp DONE_LABEL)
	     CONS_LABEL
	     (movl (mem pcb-allocation-redline pcr) ebx)
	     (addl eax ebx)
	     (addl eax ebx)
	     (cmpl ebx apr)
	     (jle LOOP_HEAD)
		; overflow
	     (addl eax esp) ; advance esp to cover args
	     (pushl cpr)    ; push current cp
	     (pushl eax)    ; push argc
	     (negl eax)	    ; make argc positive
	     (addl (int (fx* 4 wordsize)) eax) ; add 4 words to adjust frame size
	     (pushl eax)		       ; push frame size
	     (addl eax eax) ; double the number of args
	     (movl eax (mem (fx* -2 wordsize) fpr)) ; pass it as first arg
	     (movl (int (argc-convention 1)) eax)   ; setup argc
	     (movl (obj (primref->symbol 'do-vararg-overflow)) cpr)
	     (movl (mem (- disp-symbol-record-proc record-tag) cpr) cpr)
		;(movl (primref-loc 'do-vararg-overflow) cpr) ; load handler
	     (compile-call-frame 0 '#() '(int 0) (indirect-cpr-call))
	     (popl eax)	    ; pop framesize and drop it
	     (popl eax)	    ; reload argc
	     (popl cpr)	    ; reload cp
	     (subl eax fpr) ; readjust fp
	     LOOP_HEAD
	     (movl (int nil) ebx)
	     CONTINUE_LABEL
	     (movl ebx (mem disp-cdr apr))
	     (movl (mem fpr eax) ebx)
	     (movl ebx (mem disp-car apr))
	     (movl apr ebx)
	     (addl (int pair-tag) ebx)
	     (addl (int pair-size) apr)
	     (addl (int (fxsll 1 fx-shift)) eax)
	     (cmpl (int (fx- 0 (fxsll fml-count fx-shift))) eax)
	     (jle CONTINUE_LABEL)
	     DONE_LABEL
	     (movl ebx (mem (fx- 0 (fxsll fml-count fx-shift)) fpr))
	     accum))

    #| end of module: Program |# )

;;; --------------------------------------------------------------------

  (define (T x accum)
    ;;X must be a struct instance representing recordized code.
    ;;
    ;;ACCUM must  be the list  of assembly instructions,  accumulated so
    ;;far, that must be included in  binary code after the ones to which
    ;;X will expand.
    ;;
    ;;Process X  as if it  is in tail  position in some  enclosing form;
    ;;return a new accumulated list of assembly instructions.
    ;;
    (struct-case x
      ((seq e0 e1)
       (E e0 (T e1 accum)))

      ((conditional x.test x.conseq x.altern)
       (let ((L (unique-label)))
         (P x.test #f L
	    (T x.conseq
	       (cons L (T x.altern accum))))))

      ((primcall op rands)
       (case-symbols op
	 ((return)
	  (cons '(ret) accum))

	 ((indirect-jump)
	  ;;The CPU's  CP-REGISTER contains  a reference to  the closure
	  ;;object we want to jump to.
	  (cons `(jmp (disp ,off-closure-code ,CP-REGISTER))
		accum))

	 ((direct-jump)
	  (cons `(jmp (label ,(code-loc-label (car rands))))
		accum))

	 (else
	  (error who "invalid tail" x))))

      ((shortcut body handler)
       ;;What the heck is a shortcut?
       ;;
       ;;HANDLER is expanded to a list of assembly instructions like:
       ;;
       ;;  (label "ERROR-0")
       ;;  (?assembly)
       ;;  ...
       ;;
       (let ((L (unique-interrupt-label)))
         (let* ((hand (cons L (T handler '())))
		(tc   (exceptions-conc)))
	   (set-cdr! tc (append hand (cdr tc))))
         (parameterize ((exception-label L))
           (T body accum))))

      (else
       (error who "invalid tail" x))))

;;; --------------------------------------------------------------------

  (module (E)

    (define (E x accum)
      ;;flatten effect
      (struct-case x
	((seq e0 e1)
	 (E e0 (E e1 accum)))

	((conditional e0 e1 e2)
	 (cond ((interrupt? e1)
		(let ((L (or (exception-label)
			     (error who "no exception label"))))
		  (P e0 L #f (E e2 accum))))
	       ((interrupt? e2)
		(let ((L (or (exception-label)
			     (error who "no exception label"))))
		  (P e0 #f L (E e1 accum))))
	       (else
		(let ((lf (unique-label)) (le (unique-label)))
		  (P e0 #f lf (E e1 (cons* `(jmp ,le) lf
					   (E e2 (cons le accum)))))))))

	((ntcall target value args mask size)
	 (E-ntcall target value args mask size accum))

	((asm-instr op d s)
	 (E-asm-instr op d s x accum))

	((primcall op rands)
	 (E-primcall op rands x accum))

	((shortcut body handler)
	 (let ((L (unique-interrupt-label))
	       (L2 (unique-label)))
	   (let* ((hand (cons L (E handler `((jmp ,L2)))))
		  (tc   (exceptions-conc)))
	     (set-cdr! tc (append hand (cdr tc))))
	   (parameterize ((exception-label L))
	     (E body (cons L2 accum)))))

	(else
	 (error who "invalid effect" (unparse-recordized-code x)))))

    (define (E-ntcall target value args mask size accum)
      (let ((LCALL (unique-label)))
	(define (rp-label value)
	  (if value
	      (label-address (sl-mv-error-rp-label))
	    (label-address (sl-mv-ignore-rp-label))))
	(cond ((string? target) ;; foreign call
	       (cons* `(movl (foreign-label "ik_foreign_call") %ebx)
		      (compile-call-frame size mask (rp-label value) `(call %ebx))
		      accum))
	      (target ;; known call
	       (cons* (compile-call-frame size mask (rp-label value) `(call (label ,target)))
		      accum))
	      (else
	       (cons* (compile-call-frame size mask (rp-label value)
					  `(call (disp ,(fx- disp-closure-code closure-tag)
						       ,CP-REGISTER)))
		      accum)))))

    (define (E-asm-instr op d s x accum)
      (case-symbols op
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
	     (error who "invalid instr" (unparse-recordized-code x)))
	   (cons `(bswap ,s) accum)))

	((mset32)
	 (cons `(mov32 ,(R s) ,(R d)) accum))

	((load32)
	 (cons `(mov32 ,(R s) ,(R d)) accum))

	((int-/overflow)
	 (let ((L (or (exception-label)
		      (error who "no exception label" (unparse-recordized-code x)))))
	   (cons* `(subl ,(R s) ,(R d))
		  `(jo ,L)
		  accum)))

	((sll/overflow)
	 (let ((L (or (exception-label)
		      (error who "no exception label" (unparse-recordized-code x)))))
	   (cons* `(sall ,(R/cl s) ,(R d))
		  `(jo ,L)
		  accum)))

	((int*/overflow)
	 (let ((L (or (exception-label)
		      (error who "no exception label" (unparse-recordized-code x)))))
	   (cons* `(imull ,(R s) ,(R d))
		  `(jo ,L)
		  accum)))

	((int+/overflow)
	 (let ((L (or (exception-label)
		      (error who "no exception label" (unparse-recordized-code x)))))
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
	 (error who "invalid instr" (unparse-recordized-code x)))))

    (define (E-primcall op rands x accum)
      (case-symbols op
	((nop)
	 accum)

	((interrupt)
	 (let ((l (or (exception-label)
		      (error who "no exception label" (unparse-recordized-code x)))))
	   (cons `(jmp ,l) accum)))

	((incr/zero?)
	 (let ((l (or (exception-label)
		      (error who "no exception label" (unparse-recordized-code x)))))
	   (cons* `(addl ,(D (caddr rands)) ,(R (make-disp (car rands) (cadr rands))))
		  `(je ,l)
		  accum)))

	((fl:double->single)
	 (cons '(cvtsd2ss xmm0 xmm0) accum))

	((fl:single->double)
	 (cons '(cvtss2sd xmm0 xmm0) accum))

	(else
	 (error who "invalid effect" (unparse-recordized-code x)))))

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
	   (error who "invalid R/cl" (unparse-recordized-code x)))
	 (fxlogand i (- (* wordsize 8) 1)))
	(else
	 (if (eq? x ecx)
	     '%cl
	   (error who "invalid R/cl" (unparse-recordized-code x))))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (define (unique-interrupt-label)
    (label (gensym "ERROR")))

  (define (unique-label)
    (label (gensym)))

;;; --------------------------------------------------------------------

  (module (P)

    (define (P x lt lf accum)
      (struct-case x
	((constant c)
	 (if c
	     (if lt
		 (cons `(jmp ,lt) accum)
	       accum)
	   (if lf
	       (cons `(jmp ,lf) accum)
	     accum)))

	((seq e0 e1)
	 (E e0 (P e1 lt lf accum)))

	((conditional e0 e1 e2)
	 (P-conditional e0 e1 e2 lt lf accum))

	((asm-instr op a0 a1)
	 (P-asm-instr op a0 a1 lt lf accum))

	((shortcut body handler)
	 (let ((L  (unique-interrupt-label))
	       (lj (unique-label)))
	   (let ((accum (if (and lt lf)
			    accum
			  (cons lj accum))))
	     (let* ((hand (cons L (P handler (or lt lj) (or lf lj) '())))
		    (tc   (exceptions-conc)))
	       (set-cdr! tc (append hand (cdr tc))))
	     (parameterize ((exception-label L))
	       (P body lt lf accum)))))

	(else
	 (error who "invalid pred" x))))

    (define (P-conditional e0 e1 e2 lt lf accum)
      (cond ((and (constant=? e1 #t)
		  (constant=? e2 #f))
	     (P e0 lt lf accum))

	    ((and (constant=? e1 #f)
		  (constant=? e2 #t))
	     (P e0 lf lt accum))

	    ((and lt lf)
	     (let ((l (unique-label)))
	       (P e0 #f l (P e1 lt lf
			     (cons l (P e2 lt lf accum))))))

	    (lt
	     (let ((lf (unique-label))
		   (l (unique-label)))
	       (P e0 #f l (P e1 lt lf
			     (cons l (P e2 lt #f (cons lf accum)))))))

	    (lf
	     (let ((lt (unique-label))
		   (l  (unique-label)))
	       (P e0 #f l (P e1 lt lf
			     (cons l (P e2 #f lf (cons lt accum)))))))

	    (else
	     (let ((lf (unique-label))
		   (l  (unique-label)))
	       (P e0 #f l (P e1 #f #f
			     (cons `(jmp ,lf)
				   (cons l (P e2 #f #f (cons lf accum))))))))))

    (module (P-asm-instr)

      (define (P-asm-instr op a0 a1 lt lf accum)
	(cond ((and lt lf)
	       (cmp op a0 a1 lt (cons `(jmp ,lf) accum)))
	      (lt
	       (cmp op a0 a1 lt accum))
	      (lf
	       (cmp (notop op) a0 a1 lf accum))
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
	       (error who "invalid cmpops" a0 a1))))

      (define (notop x)
	(cond ((assq x '((= !=) (!= =) (< >=) (<= >) (> <=) (>= <)
			 (u< u>=) (u<= u>) (u> u<=) (u>= u<)
			 (fl:= fl:o!=) (fl:!= fl:o=)
			 (fl:< fl:o>=) (fl:<= fl:o>)
			 (fl:> fl:o<=) (fl:>= fl:o<)))
	       => cadr)
	      (else
	       (error who "invalid notop" x))))

      (define (jmpname x)
	(cond ((assq x '((= je) (!= jne) (< jl) (<= jle) (> jg) (>= jge)
			 (u< jb) (u<= jbe) (u> ja) (u>= jae)
			 (fl:= je) (fl:!= jne)
			 (fl:< jb) (fl:> ja) (fl:<= jbe) (fl:>= jae)
			 (fl:o= je) (fl:o!= jne)
			 (fl:o< jb) (fl:o> ja) (fl:o<= jbe) (fl:o>= jae)))
	       => cadr)
	      (else
	       (error who "invalid jmpname" x))))

      (define (revjmpname x)
	(cond ((assq x '((= je) (!= jne) (< jg) (<= jge) (> jl) (>= jle)
			 (u< ja) (u<= jae) (u> jb) (u>= jbe)))
	       => cadr)
	      (else
	       (error who "invalid jmpname" x))))

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
    `(disp ,(* i (- wordsize)) ,fpr))

  (module (R R/l D)

    (define (R x)
      (struct-case x
	((constant c)
	 (C c))
	((fvar i)
	 (FVar i))
	((disp s0 s1)
	 (let ((s0 (D s0))
	       (s1 (D s1)))
	   `(disp ,s0 ,s1)))
	(else
	 (if (symbol? x)
	     x
	   (error who "invalid R" x)))))

    (module (R/l)

      (define (R/l x)
	(struct-case x
	  ((constant c)
	   (C c))
	  ((fvar i)
	   (FVar i))
	  ((disp s0 s1)
	   (let ((s0 (D s0))
		 (s1 (D s1)))
	     `(disp ,s0 ,s1)))
	  (else
	   (if (symbol? x)
	       (reg/l x)
	     (error who "invalid R/l" x)))))

      (define (reg/l x)
	(cond ((assq x '((%eax %al) (%ebx %bl) (%ecx %cl) (%edx %dl)
			 (%r8 %r8l) (%r9 %r9l) (%r10 %r10l) (%r11 %r11l)
			 (%r12 %r12l) (%r13 %r13l) (%r14 %r14l) (%r15 %r15l)))
	       => cadr)
	      (else
	       (error who "invalid reg/l" x))))

      #| end of module: R/l |# )

    (define (D x)
      (struct-case x
	((constant c)
	 (C c))
	(else
	 (if (symbol? x)
	     x
	   (error who "invalid D" x)))))

    (define (C x)
      (struct-case x
	((code-loc label)
	 (label-address label))
	((foreign-label L)
	 `(foreign-label ,L))
	((closure label free*)
	 (unless (null? free*)
	   (error who "nonempty closure"))
	 `(obj ,x))
	((object o)
	 `(obj ,o))
	(else
	 (if (integer? x)
	     x
	   (error who "invalid constant C" x)))))

    #| end of module |# )

  ;;Commented out because unused.  (Marco Maggi; Oct 29, 2012)
  ;;
  ;; (define (BYTE x)
  ;;   (struct-case x
  ;;     ((constant x)
  ;;      (unless (and (integer? x)
  ;; 		    (fx<= x +255)
  ;; 		    (fx>= x -128))
  ;;        (error who "invalid byte" x))
  ;;      x)
  ;;     (else
  ;;      (error who "invalid byte" x))))

  ;;Commented out because unused.  (Marco Maggi; Oct 29, 2012)
  ;;
  ;; (define (reg/h x)
  ;;   (cond ((assq x '((%eax %ah) (%ebx %bh) (%ecx %ch) (%edx %dh)))
  ;; 	   => cadr)
  ;; 	  (else
  ;; 	   (error who "invalid reg/h" x))))

  #| end of module: alt-cogen.flatten-codes |# )


;;;; done

#| end of module: alt-cogen |# )

;;; end of file
;; Local Variables:
;; eval: (put 'make-primcall 'scheme-indent-function 1)
;; End:
