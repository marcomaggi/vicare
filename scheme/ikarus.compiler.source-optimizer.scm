;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;; the source code optimizer
;;
;;Check the  "makefile.sps" used to build  the boot image to  see if the
;;source  optimizer  is used  (it  should  be, at  maximum  optimization
;;level).
;;
;;See the thesis:
;;
;;   Oscar  Waddell.  "Extending  the Scope  of Syntactic  Abstraction".
;;   PhD.   Thesis.   Indiana  University Computer  Science  Department.
;;   August 1999.
;;
;;available online:
;;
;;   <http://www.cs.indiana.edu/~owaddell/papers/thesis.ps.gz>
;;
;;See also the paper:
;;
;;   Oscar  Waddell,  R. Kent  Dybvig.   "Fast  and Effective  Procedure
;;   Inlining".   Indiana  University.    Computer  Science  Department.
;;   Technical Report No. 484.
;;
(module (source-optimize
	 optimize-level
	 source-optimizer-passes-count
	 cp0-effort-limit
	 cp0-size-limit)
  (define who 'source-optimize)

  (define-constant DEFAULT-CP0-EFFORT-LIMIT	50)
  (define-constant DEFAULT-CP0-SIZE-LIMIT	8)
  (define-constant O3-CP0-EFFORT-LIMIT		(* 4 DEFAULT-CP0-EFFORT-LIMIT))
  (define-constant O3-CP0-SIZE-LIMIT		(* 4 DEFAULT-CP0-SIZE-LIMIT))

  (define cp0-effort-limit
    (make-parameter DEFAULT-CP0-EFFORT-LIMIT
      (lambda (obj)
	(if (and (fixnum? obj)
		 (fxnonnegative? obj))
	    obj
	  (procedure-argument-violation 'cp0-effort-limit
	    "expected positive fixnum as optimisation effort limit"
	    obj)))))

  (define cp0-size-limit
    (make-parameter DEFAULT-CP0-SIZE-LIMIT
      (lambda (obj)
	(if (and (fixnum? obj)
		 (fxnonnegative? obj))
	    obj
	  (procedure-argument-violation 'cp0-size-limit
	    "expected positive fixnum as optimisation size limit"
	    obj)))))

  (define optimize-level
    (make-parameter 2
      (lambda (obj)
	(case obj
	  ((0 1 2 3)
	   obj)
	  (else
	   (procedure-argument-violation 'optimize-level
	     "valid optimization levels are 0, 1, 2, and 3"
	     obj))))))

  (define source-optimizer-passes-count
    (make-parameter 1
      (lambda (obj)
	(if (and (fixnum?     obj)
		 (fxpositive? obj))
	    obj
	  (procedure-argument-violation 'source-optimizer-passes-count
	    "expected positive fixnum as source optimiser passes count"
	    obj)))))

  (define source-optimizer-input
    ;;This is used in case of internal error to show better error context.
    ;;
    (make-parameter #f))

  (module (source-optimize)

    (define (source-optimize expr)
      (case (optimize-level)
	((3)
	 ;;This optimisation level is meant to do the most possible.
	 (parameterize ((cp0-effort-limit	O3-CP0-EFFORT-LIMIT)
			(cp0-size-limit		O3-CP0-SIZE-LIMIT)
			(source-optimizer-passes-count 2))
	   (%do-one-pass expr (source-optimizer-passes-count))))
	((2)
	 (%do-one-pass expr (source-optimizer-passes-count)))
	((1)
	 (let ((expr (parameterize ((cp0-size-limit 0))
		       (%do-one-pass expr (source-optimizer-passes-count)))))
	   #;(debug-print expr)
	   expr))
	(else
	 #;(debug-print expr)
	 expr)))

    (define (%do-one-pass expr passes-count)
      (if (fxzero? passes-count)
	  expr
	(%do-one-pass (parametrise ((source-optimizer-input expr))
			(E expr 'v (make-empty-env) (passive-counter) (passive-counter)))
		      (fxsub1 passes-count))))

    #| end of module: source-optimize |# )


;;;; type definitions

;;Represent  the   bindings  introduced  at  a   lexical  contour:  BIND
;;structures,   FIX  structures,   CLAMBDA-CASE  structures.    See  the
;;documentation of the syntax WITH-EXTENDED-ENV for more details.
;;
(define-struct env
  (lhs*
		;A list of struct  instances of type PRELEX representing
		;the bindings introduced at a lexical contour.
   copy*
		;A list of struct instances  of type PRELEX being copies
		;of   the  ones   in  the   field  "lhs*"   produced  by
		;MAKE-PRELEX-REPLACEMENT.
   old-env
		;False or a struct instance of type ENV representing the
		;environment that encloses this one.
   ))

(define-syntax-rule (make-empty-env)
  #f)

;;Represent the context  in which a primitive function  (CONS, CAR, ...)
;;is  applied.  Instances  of this  type are  moved around  in the  CTXT
;;arguments to functions.
;;
;;Fields:
;;
;;RAND*  is  a  list  of  struct  instances  representing  the  function
;;application operands.
;;
;;CTXT can be  either an evaluation context symbol (one  among: p, e, v)
;;or a nested struct instance of type APP.
;;
;;INLINED is a boolean, true  if the function application represented by
;;this struct instance has been expanded inline (taking advantage of the
;;known primitive function attributes).
;;
(define-structure app
  (rand* ctxt)
  ((inlined			#f)))

;;Represent an expression being the operand to a function call.
;;
(define-structure operand
  (expr env ec)
  ((value			#f)
		;If it is  possible to precompute the  value: such value
		;is saved here.
   (residualize-for-effect	#f)
   (size			0)
		;The expected size of the result.
   (inner-pending		#f)
   (outer-pending		#f)))

;;Counter  structure;  keeps  track  of  the  resources  consumed  while
;;optimization is  performed, so  that when "too  much" is  consumed the
;;optimizer can give up and return the original expression.
;;
(define-struct counter
  (value
		;A fixnum being the current  value of the counter.  This
		;fixnum   is   decremented   while   optimizations   are
		;performed;  when  it  reaches zero:  the  threshold  is
		;crossed.
   ctxt
		;For  a passive  counter:  #f.  For  an active  counter:
		;either an  evaluation context symbol (one  among: p, e,
		;v) or a struct instance of type APP.
   k
		;Continuation function to be called if the threshold for
		;this counter has been reached.
   ))


(module (E)
  ;;Iterate over recordized code performing source optimization transformations; this
  ;;module is the actual source optimizer.
  ;;
  ;;This module accepts  as input recordized code containing the  struct instances of
  ;;the following types:
  ;;
  ;;	assign		bind		clambda
  ;;	conditional	constant	fix
  ;;	forcall		funcall		prelex
  ;;	primref		seq
  ;;
  ;;and returns recordized code composed of the same struct types.
  ;;
  (define (E x ctxt env ec sc)
    (debug-print* 'enter
		  (unparse-recordized-code/pretty x))
    (receive-and-return (rv)
	(%E x ctxt env ec sc)
      (debug-print* 'leave
		    (unparse-recordized-code/pretty rv))))

  (define (%E x ctxt env ec sc)
    ;;Recursive function performing source optimizations.
    ;;
    ;;X must be a struct instance representing recordized code.
    ;;
    ;;CTXT can be either an evaluation  context symbol (one among: p, e,
    ;;v) or a struct instance of type APP.
    ;;
    ;;EC is the effort counter.
    ;;
    ;;SC is the size counter.
    ;;
    (decrement ec 1)
    (struct-case x
      ((constant)
       ;;X is a datum.
       (decrement sc 1)
       x)

      ((prelex)
       ;;X is a lexical variable reference.
       (E-var x ctxt env ec sc))

      ((seq e0 e1)
       ;;The sub-expression E0 is evaluated  for its side effects; the sub-expression
       ;;E1 is evaluated in the context CTXT.
       (make-seq-discarding-useless (E e0 'e   env ec sc)
				    (E e1 ctxt env ec sc)))

      ((conditional x.test x.conseq x.altern)
       (E-conditional x.test x.conseq x.altern ctxt env ec sc))

      ((assign lhs rhs)
       ;;X is a lexical variable assignment: it mutates the binding.
       (E-assign lhs rhs env ec sc))

      ((funcall rator rand*)
       ;;X is a function application.  The operator is in RATOR, the list of operands
       ;;is in RAND*.
       (E-funcall rator (map (lambda (x)
			       (make-operand x env ec))
			  rand*)
		  env ctxt ec sc))

      ((forcall name rand*)
       ;;X is a  foreign function call performed by the  built in macro FOREIGN-CALL.
       ;;The name of the  C language function is in NAME, the list  of operands is in
       ;;RAND*.
       (decrement sc 1)
       (make-forcall name (map (lambda (operand)
				 ;;The operands are evaluated for their return value.
				 (E operand 'v env ec sc))
			    rand*)))

      ((primref name)
       (case-context ctxt
	 ((app)
	  (case name
	    ((debug-call)
	     (E-debug-call ctxt ec sc))
	    (else
	     (fold-prim name ctxt ec sc))))
	 ((v)
	  (decrement sc 1)
	  x)
	 (else
	  (make-constant #t))))

      ((clambda label clause* cp free name)
       (parametrise ((source-optimizer-input x))
	 (E-clambda label clause* cp free name   x ctxt env ec sc)))

      ((bind lhs* rhs* body)
       (E-bind lhs* rhs* body ctxt env ec sc))

      ((fix lhs* rhs* body)
       (E-fix  lhs* rhs* body ctxt env ec sc))

      (else
       (error who
	 "invalid expression for source optimizer"
	 (unparse-recordized-code (source-optimizer-input))
	 x))))

;;; --------------------------------------------------------------------

  (module (E-conditional)

    (define (E-conditional x.test x.conseq x.altern ctxt env ec sc)
      ;;Process a struct instance of  type CONDITIONAL.  Return either a
      ;;struct of type CONDITIONAL or of type SEQ.
      ;;
      ;;CTXT can be  either an evaluation context symbol  (one among: p,
      ;;e, v) or a struct instance of type APP.
      ;;
      ;;EC is the effort counter.  SC is the size counter.
      ;;
      (let ((test (E x.test 'p env ec sc)))
	;;Remember that TEST can be a sequence of expressions, too.
	(struct-case (result-expr test)
	  ((constant test.result.val)
	   ;;We could  precompute the result  of the TEST, so  we output
	   ;;only the CONSEQ or the ALTERN.
	   (make-seq-discarding-useless test (E (if test.result.val x.conseq x.altern)
						ctxt env ec sc)))
	  (else
	   ;;The last expression in TEST could not be precomputed.
	   (let ((ctxt (case-context ctxt
			 ((app) 'v)
			 (else  ctxt))))
	     (let ((optimized-conseq (E x.conseq ctxt env ec sc))
		   (optimized-altern (E x.altern ctxt env ec sc)))
	       (if (%records-equal? optimized-conseq optimized-altern ctxt)
		   ;;If the results of CONSEQ and ALTERN are known to be
		   ;;equal: we  just include TEST for  its side effects,
		   ;;followed by CONSEQ.
		   (make-seq-discarding-useless test optimized-conseq)
		 (begin
		   (decrement sc 1)
		   (%build-conditional test optimized-conseq optimized-altern)))))))))

    (define (%records-equal? x y ctxt)
      ;;Given the struct instances X and Y, representing recordized code
      ;;to be evaluated in the same context CTXT, do what is possible to
      ;;determine if X and Y are equivalent.
      ;;
      ;;If the context  is "predicate": X and Y are  equivalent if it is
      ;;known that  both of them return  true or or both  of them return
      ;;false.
      ;;
      ;;If the context is "for side  effects": X and Y are equivalent if
      ;;it is known that both of them have no side effects.
      ;;
      ;;FIXME This can be probably improved with more deep inspection of
      ;;X and Y.  (Marco Maggi; Nov 4, 2012)
      ;;
      (struct-case x
	((constant x.val)
	 (struct-case y
	   ((constant y.val)
	    (case-context ctxt
	      ((e)
	       ;;Both X and Y are constants and the context if "for side
	       ;;effect"; constants  have no  side effects, so  they are
	       ;;equivalent.
	       #t)
	      ((p)
	       ;;Are they both non-false or both false?
	       (if x.val y.val (not y.val)))
	      (else
	       ;;Are they just the same constant?
	       (eq? x.val y.val))))
	   (else
	    #f)))
	(else
	 #f)))

    (define (%build-conditional test conseq altern)
      ;;If the test as the form:
      ;;
      ;;  (not ?nested-test)
      ;;
      ;;we can just optimize:
      ;;
      ;;  (if (not ?nested-test) ?conseq ?altern)
      ;;  ==> (if ?nested-test ?altern ?conseq)
      ;;
      ;;This is a recursive function to process tests like:
      ;;
      ;;  (not (not (not ?nested-test)))
      ;;
      (or (struct-case test
	    ((funcall rator rand*)
	     (struct-case rator
	       ((primref op)
		;;*NOTE* This form can return #f too!!!
		(and (eq? op 'not)
		     (= (length rand*) 1)
		     (%build-conditional (car rand*) altern conseq)))
	       (else
		#f)))
	    (else
	     #f))
	  (make-conditional test conseq altern)))

    #| end of module: E-conditional |# )

  (define (E-assign lhs rhs env ec sc)
    ;;Process  a  struct   instance  of  type  ASSIGN.   Return   a  struct  instance
    ;;representing recordized  code; due to  the possible optimizations: the  type of
    ;;the returned instance is not known.
    ;;
    ;;If the binding represented  by LHS has been created but  it is never referenced
    ;;in its region: assigning it is useless, because the new value is never used; so
    ;;we just include the RHS for its side effects.  For example:
    ;;
    ;;   (let ((?lhs (some-value)))
    ;;     (set! ?lhs ?rhs)
    ;;     #t)
    ;;
    ;;becomes:
    ;;
    ;;   (let ((?lhs (some-value)))
    ;;     ?rhs
    ;;     #t)
    ;;
    ;;NOTE Beware the following weird case; let's take this core language form:
    ;;
    ;;   (letrec* ((b (lambda () a))
    ;;             (a (b)))
    ;;     '#!void)
    ;;
    ;;both the  bindings are unassigned  in this form,  but the SCC  letrec optimiser
    ;;transforms them as follows:
    ;;
    ;;   (bind ((a_0 (constant #!void)))
    ;;     (fix ((b_0 (lambda () a_0)))
    ;;       (seq
    ;;         (assign a_0 (funcall b_0))
    ;;         (constant #!void))))
    ;;
    ;;note the  ASSIGN struct; in  this very function the  call to B_0  is integrated
    ;;resulting into:
    ;;
    ;;   (bind ((a_0 (constant #!void)))
    ;;     (fix ((b_0 (lambda () a_0)))
    ;;       (seq
    ;;         (assign a_0 a_0)
    ;;         (constant #!void))))
    ;;
    ;;which means that the LHS and RHS of  such assign are EQ?  to each other.  We do
    ;;not want to generate such code, so below we detect it and substitute the ASSIGN
    ;;with a void constant.
    ;;
    (make-seq-discarding-useless
     (if (not (prelex-source-referenced? lhs))
	 (E rhs 'e env ec sc)
       (let ((lhs.copy (%lookup lhs env)))
	 (decrement sc 1)
	 ;;FIXME If  the original binding was  assigned and it still  is after source
	 ;;optimisation: fine, we  register this state in the PRELEX  struct.  But is
	 ;;this correct also when we do  *not* return an ASSIGN struct below?  (Marco
	 ;;Maggi; Fri Aug 29, 2014)
	 (set-prelex-residual-assigned?! lhs.copy (prelex-source-assigned? lhs.copy))
	 (let ((rhs^ (E rhs 'v env ec sc)))
	   (if (eq? lhs.copy rhs^)
	       ;;Weird case discussed above.
	       VOID-CONSTANT
	     (make-assign lhs.copy rhs^)))))
     VOID-CONSTANT))

  (define (E-funcall rator rand* env ctxt ec sc)
    ;;Process a  struct instance of  type FUNCALL, *not*  representing a
    ;;call to DEBUG-CALL.  RATOR must be a struct instance of type
    ;;
    ;;RAND* is a list of  struct instances of type OPERAND, representing
    ;;recordized code which, when evaluated, will return the operands of
    ;;the function call.
    ;;
    ;;ENV
    ;;
    ;;CTXT can be either an evaluation  context symbol (one among: p, e,
    ;;v) or a struct instance of type APP.
    ;;
    ;;EC is the effort counter.  SC is the size counter.
    ;;
    (let* ((ctxt^  (make-app rand* ctxt))
	   (rator^ (E rator ctxt^ env ec sc)))
      (if (app-inlined ctxt^)
	  ;;If this function  call was inlined, we may  need to evaluate
	  ;;some of  the operands for  their side effect.   For example,
	  ;;given the function definition:
	  ;;
	  ;;   (define (fun x y)
	  ;;     (list 123 x))
	  ;;
	  ;;in which Y is not used, we want the call:
	  ;;
	  ;;   (fun 456 (display "ciao"))
	  ;;
	  ;;to be transformed into:
	  ;;
	  ;;   (begin
	  ;;     (display "ciao")
	  ;;     (list 123 456))
	  ;;
	  (residualize-operands rator^ rand* sc)
	(begin
	  (decrement sc (if (primref? rator^) 1 3))
	  (make-funcall rator^ (map (lambda (x)
				      (score-value-visit-operand! x sc))
				 rand*))))))

  (define (E-debug-call ctxt ec sc)
    ;;Process a struct  instance of type PRIMREF  requesting a reference
    ;;to DEBUG-CALL.
    ;;
    ;;CTXT can be either an evaluation  context symbol (one among: p, e,
    ;;v) or a struct instance of type APP.
    ;;
    ;;EC is the effort counter.  SC is the size counter.
    ;;
    (let ((rand* (app-rand* ctxt)))
      (if (< (length rand*) 2)
	  ;;It is a standalone reference to DEBUG-CALL, or maybe not.
	  (begin
	    (decrement sc 1)
	    (mk-primref 'debug-call))
	;;It is the operand of a FUNCALL to DEBUG-CALL.
	(mk-primref 'debug-call)
	;;FIXME Commenting out  this chunk fixes issue  #3, which caused
	;;the following program:
	;;
	;;   (import (rnrs))
	;;   (list (display "ciao\n"))
	;;
	;;to print "ciao"  twice when run with both  the options --debug
	;;and -O2.  Is this because the call to "E-debug-call" is nested
	;;in a call to "E-funcall", so we do twice the same thing?
	;;
	;;This chunk is  left here for future  reference, but eventually
	;;it will be removed.  Do we want to optimize debug calls?  When
	;;debugging, do we not want the  code to be as close as possible
	;;to the original?  (Marco Maggi; Nov 5, 2012)
	;;
	;; (begin
	;;   (let ((src/expr  (car rand*))	;the source annotation
	;; 	(rator     (cadr rand*))	;the wrapped function
	;; 	(rands     (cddr rand*)))	;operands to the wrapped function
	;;     (let* ((ctxt2 (make-app rands (app-ctxt ctxt)))
	;; 	   (rator (E (operand-expr rator)
	;; 		     ctxt2
	;; 		     (operand-env  rator)
	;; 		     (operand-ec   rator)
	;; 		     sc)))
	;;       (if (app-inlined ctxt2)
	;; 	  (begin
	;; 	    (set-app-inlined! ctxt #t)
	;; 	    (residualize-operands rator (cons src/expr rands) sc))
	;; 	(begin
	;; 	  (decrement sc 1)
	;; 	  (mk-primref 'debug-call))))))
	)))

  (define (E-var x ctxt env ec sc)
    ;;Process a lexical variable reference.
    ;;
    ;;X is a struct instance of type PRELEX.
    ;;
    ;;CTXT can be either an evaluation  context symbol (one among: p, e,
    ;;v) or a struct instance of type APP.
    ;;
    ;;EC is the effort counter.  SC is the size counter.
    ;;
    (case-context ctxt
      ((e)
       ;;Variable reference for side effect is nothing.  For example:
       ;;
       ;;   (let ((x 1))
       ;;     (begin
       ;;       x
       ;;       2))
       ;;
       ;;is simplified to:
       ;;
       ;;   (let ((x 1))
       ;;     (begin
       ;;       (void)
       ;;       2))
       ;;
       (prelex-decr-source-reference-count! x)
       VOID-CONSTANT)
      (else
       (let* ((x.copy (%lookup x env))
	      (opnd   (prelex-operand x.copy)))
	 (if (and opnd (not (operand-inner-pending opnd)))
	     (begin
	       (dynamic-wind ;avoid evaluation cycles
		   (lambda () (set-operand-inner-pending! opnd #t))
		   (lambda () (value-visit-operand! opnd))
		   (lambda () (set-operand-inner-pending! opnd #f)))
	       ;;We  would  like  to   attempt  replacing  the  variable
	       ;;reference with the expansion of the referenced value.
	       (if (prelex-source-assigned? x.copy)
		   ;;No optimizations  possible because the  variable is
		   ;;assigned somewhere, so its  value will change; just
		   ;;output the reference to variable.
		   (residualize-ref x.copy sc)
		 ;;Attempt to  substitute the reference with  the result
		 ;;of optimizing the referenced value.  Notice that this
		 ;;case includes:
		 ;;
		 ;;  (define (a)
		 ;;    (do-something))
		 ;;
		 ;;  (a) ;; <-- reference to variable
		 ;;
		 (inline-referenced-operand x.copy opnd ctxt ec sc)))
	   ;;No optimizations  possible.  Just  output the  reference to
	   ;;variable.
	   (residualize-ref x.copy sc))))))

  (define (E-bind lhs* rhs* body ctxt env ec sc)
    ;;Process a BIND.
    ;;
    ;;LHS* is a list of PRELEX structures being the left-hand sides.
    ;;
    ;;RHS* is a  list of struct instances  representing recordized code,
    ;;being the right-hand sides.
    ;;
    ;;BODY is a struct instance representing recordized code.
    ;;
    ;;CTXT can be either an evaluation  context symbol (one among: p, e,
    ;;v) or a struct instance of type APP.
    ;;
    ;;EC is the effort counter.  SC is the size counter.
    ;;
    (let ((rand* (map (lambda (x)
			(make-operand x env ec))
		   rhs*)))
      (with-extended-env ((env lhs*) <== (env lhs* rand*))
	(residualize-operands (make-let-binding lhs* rand* (E body ctxt env ec sc) sc)
			      rand* sc))))

  (define (E-fix lhs* rhs* body ctxt env ec sc)
    ;;Process a FIX structure.
    ;;
    ;;LHS* is a list of PRELEX structures being the left-hand sides.  We
    ;;know that these LHSes will never be assigned.
    ;;
    ;;RHS* is  a list  of struct  instances of  type CLAMBDA,  being the
    ;;right-hand sides.
    ;;
    ;;BODY is a struct instance representing recordized code.
    ;;
    ;;CTXT can be either an evaluation  context symbol (one among: p, e,
    ;;v) or a struct instance of type APP.
    ;;
    ;;EC is the effort counter.  SC is the size counter.
    ;;
    (with-extended-env ((env lhs*) <== (env lhs* #f))
      ;;Register the RHS as operand for the associated LHS.
      (for-each (lambda (lhs rhs)
		  (set-prelex-operand! lhs (make-operand rhs env ec)))
	lhs* rhs*)
      (let* ((optimized-body  (E body ctxt env ec sc))
	     ;;Do  this after  BODY optimization!!!   Discard the  LHSes
	     ;;that are still *not* referenced after body optimization.
	     ;;
	     ;;If all the  calls to a CLAMBDAs have been  inlined: it is
	     ;;useless to keep it.
	     ;;
	     ;;Also if none of the  CLAMBDAs are referenced in the body:
	     ;;all of them are  discarded!!!  For example, the following
	     ;;optimization is performed:
	     ;;
	     ;;  (letrec* ((f (lambda () (g)))
	     ;;            (g (lambda () (f))))
	     ;;    123)
	     ;;  ==> 123
	     ;;
	     (referenced-lhs* (remp (lambda (x)
				      (not (prelex-residual-referenced? x)))
				lhs*)))
	(if (null? referenced-lhs*)
	    optimized-body
	  (begin
	    (decrement sc 1)
	    (make-fix referenced-lhs* (map (lambda (ref-lhs)
					     (let ((opnd (prelex-operand ref-lhs)))
					       (decrement sc (+ (operand-size opnd) 1))
					       (value-visit-operand! opnd)))
					referenced-lhs*)
	      optimized-body))))))

  (define (E-clambda label clause* cp free name   x ctxt env ec sc)
    ;;Process a struct intance of type CLAMBDA.
    ;;
    ;;X is the  original struct instance of type CLAMBDA;  we need it if
    ;;we attempt to inline the function.
    ;;
    ;;ENV
    ;;
    ;;CTXT can be either an evaluation  context symbol (one among: p, e,
    ;;v) or a struct instance of type APP.
    ;;
    ;;EC is the effort counter.  SC is the size counter.
    ;;
    (case-context ctxt
      ((app)
       ;;This  is  the  case  of CASE-LAMBDA  evaluation  and  immediate
       ;;application:
       ;;
       ;;   ((lambda (x) (do-something-with x))
       ;;    123)
       ;;
       (inline-function-application x ctxt env ec sc))
      ((p e)
       ;;This is the case of "predicate" context:
       ;;
       ;;   (if (case-lambda ---)
       ;;       (if-true)
       ;;     (if-false))
       ;;
       ;;or "for side effects" context:
       ;;
       ;;   (begin
       ;;     (case-lambda ---)
       ;;     (return-value))
       ;;
       (make-constant #t))
      (else
       ;;This  is the  case "for  returned value"  context, which  for a
       ;;function means (for example):
       ;;
       ;;  (define the-name (case-lambda ---))
       ;;
       ;;or:
       ;;
       ;;  (do-something 1 2 (case-lambda ---))
       ;;
       ;;So here  we want to  optimize the body of  every CASE-LAMBDA
       ;;clause.
       ;;
       ;;NOTE Yes,  for some reason  we need to  generate new label  gensyms.  (Marco
       ;;Maggi; Mon Sep 1, 2014)
       (decrement sc 2)
       (make-clambda (gensym label)
		     (map (lambda (clause)
			    (struct-case clause
			      ((clambda-case info body)
			       (struct-case info
				 ((case-info label args proper)
				  (with-extended-env ((env args) <== (env args #f))
				    (make-clambda-case
				     (make-case-info (gensym label) args proper)
				     (E body 'v env ec sc))))))))
		       clause*)
		     cp free name))))

;;; --------------------------------------------------------------------

  (define (%lookup x env)
    ;;Search X,  a struct  instance of type  PRELEX, in  the environment
    ;;ENV; if found: return its copy; if not found: return X itself.
    ;;
    (if (env? env)
	(let loop ((lhs*  ($env-lhs* env))
  		   (copy* ($env-copy* env)))
	  (cond ((null? lhs*)
		 (%lookup x ($env-old-env env)))
		((eq? x (car lhs*))
		 (car copy*))
		(else
		 (loop (cdr lhs*) (cdr copy*)))))
      x))

  #| end of module: E |# )


;;;; size and effort counters handling
;;
;;Resource counters  keep track  of the  resources used  when optimizing
;;code; if the optimization of an  expression consumes "too much", it is
;;aborted and the original expression is inserted in the code.
;;
;;It is like this: the source optimizer is a recursive function, calling
;;itself to  optimize nested subexpressions;  when first calling  it, we
;;build resource usage  counters with such a big value  that they should
;;never trigger an optimization abortion.  Upon entering optimization of
;;expressions known to be in some known category: counters with specific
;;limit values are created to limit the resources.
;;
;;The counters  with "such  big values"  are called  "passive"; counters
;;with  actual resoruce  limits are  called "active".   Passive counters
;;have no evaluation context, while active counters are associated to an
;;expression evaluation context (predicate, for returned value, for side
;;effects, function application).
;;

(define (passive-counter)
  ;;Return a counter  with such a big initial value  that it could never
  ;;be decremented below the threshold value.
  ;;
  (make-counter (greatest-fixnum) #f (lambda args
				       (error 'passive-counter "invalid abort"))))

(define (passive-counter-value x)
  (- (greatest-fixnum)
     (counter-value x)))

(define (active-counter? x)
  (and (counter? x)
       (counter-ctxt x)))

(module (decrement)

  (define (decrement x amount)
    ;;Decrement the counter  X, which must be a struct  instance of type
    ;;COUNTER,  and  if  the  threshold   value  is  reached:  call  the
    ;;registered continuation  to abort the optimization  of the current
    ;;expression.
    ;;
    (let ((n (- (counter-value x) amount)))
      (set-counter-value! x n)
      (when (< n 0)
	(%reset-integrated! (counter-ctxt x))
	((counter-k x) #f))))

  ;;Commented out because never used.  (Marco Maggi; Nov  3, 2012)
  ;;
  ;; (define (abort-counter! x)
  ;;   (%reset-integrated! (counter-ctxt x))
  ;;   ((counter-k x) #f))

  (define (%reset-integrated! ctxt)
    ;;Recursively  reset the  inlined status  of CTXT,  which must  be a
    ;;struct instance of type APP.
    ;;
    (set-app-inlined! ctxt #f)
    (let ((nested-ctxt (app-ctxt ctxt)))
      (when (app? nested-ctxt)
	(%reset-integrated! nested-ctxt))))

  #| end of module: decrement |# )


(module (with-extended-env <== make-prelex-replacement)
  ;;Every time the  source optimizer enters a  construct introducing new
  ;;bindings,  it makes  use of  this syntax  to keep  track of  the new
  ;;bindings  and to  replace all  the  old PRELEX  structures with  new
  ;;PRELEX structures.
  ;;
  ;;In recordized  code handed to  the source optimizer: all  the PRELEX
  ;;structures  in  reference  position  representing  values  from  the
  ;;environment  (or unbound)  are left  in the  output, all  the PRELEX
  ;;structures  in binding  position or  referencing local  bindings are
  ;;replaced.
  ;;
  (define-syntax with-extended-env
    (lambda (stx)
      (syntax-case stx (<==)
	((_ ((?new-env ?new-lhs-id) <== (?old-env ?args1 ?rands))
	    ?body0 ?body ...)
	 (and (identifier? #'?new-env)
	      (identifier? #'?new-lhs-id))
	 #'(let-values (((?new-env ?new-lhs-id) (%extend-env ?old-env ?args1 ?rands)))
	     (begin0
		 (let () ?body0 ?body ...)
	       (%copy-assigned-fields-to-source-fields! ?new-lhs-id)))))))

  (define-auxiliary-syntaxes <==)

  (define* (make-prelex-replacement {x prelex?})
    ;;Given a struct instance  X of type PRELEX build and return  a new PRELEX struct
    ;;containing  the  same  values   in  the  fields:  "name",  "source-reference?",
    ;;"global-location", and optionally more.
    ;;
    (receive-and-return (y)
	(make-prelex ($prelex-name x))
      ($set-prelex-source-reference-count! y ($prelex-source-reference-count x))
      ($set-prelex-source-assigned?!       y ($prelex-source-assigned?       x))
      (let ((loc ($prelex-global-location x)))
	;;If the  PRELEX struct X represents  a binding defined by  the core language
	;;form LIBRARY-LETREC*:  LOC is the loc  gensym and X represents  a top level
	;;binding.  If X represents a lexical binding: LOC is #f.
	(when loc
	  ($set-prelex-global-location!        y loc)
	  ;;Top level bindings  must not be removed, even if  they are not referenced
	  ;;in this compilation unit.  So we increment the reference count here: even
	  ;;if actual references are removed the reference count will never be zero.
	  (prelex-incr-source-reference-count! y)
	  ($set-prelex-residual-referenced?!   y #t)))))

;;; --------------------------------------------------------------------

  (define (%extend-env env lhs* rands)
    ;;
    ;;ENV must be an environment.
    ;;
    ;;LHS* must  be a  list of  struct instances of  type PRELEX,  to be
    ;;added to the environment.
    ;;
    ;;RANDS must  be #f or a  list of struct instances  of type OPERAND,
    ;;representing  recordized  code,  being  the values  bound  to  the
    ;;structures in LHS*.
    ;;
    (if (null? lhs*)
	(values env '())
      (let ((copy* (map make-prelex-replacement lhs*)))
	(when rands
	  (for-each (lambda (lhs rhs)
		      (set-prelex-operand! lhs rhs))
	    copy* rands))
	(values (make-env lhs* copy* env) copy*))))

  (define (%copy-assigned-fields-to-source-fields! ls)
    (for-each (lambda (x)
		($set-prelex-source-assigned?!      x ($prelex-residual-assigned?   x))
		(when ($prelex-residual-referenced? x)
		  (prelex-incr-source-reference-count! x)))
      ls))

  #| end of module: with-extended-env |# )


(define-syntax case-context
  ;;Dispatch to a  branch specialised for a  specific evaluation context
  ;;among:  predicate  (p),   for  value  (v),  for   side  effect  (e),
  ;;application (app).
  ;;
  ;;Usage template:
  ;;
  ;;  (case-context ?ctxt-expr
  ;;    ((p)	?predicate-body)
  ;;    ((e)	?side-effects-body)
  ;;    ((v)	?value-body)
  ;;    ((app)	?application-body)
  ;;    (else	?else-body))
  ;;
  ;;where ?CTXT-EXPR is  an expression evaluating to one  of the symbols
  ;;"p", "v",  "e" or a  struct instance of  type APP.  The  clauses can
  ;;appear  in any  order;  the clauses  are optional,  a  clause for  a
  ;;context might be there or not.
  ;;
  ;;The usage template above will expand to:
  ;;
  ;;  (let ((t ?ctxt-expr))
  ;;    (if (eq? t 'p)
  ;;        (begin ?predicate-body)
  ;;      (if (eq? t 'e)
  ;;          (begin ?side-effects-body)
  ;;        (if (eq? t 'v)
  ;;            (begin ?value-body)
  ;;          (if (app? t)
  ;;              (begin ?application-body)
  ;;            (begin ?else-body))))))
  ;;
  ;;It is possible to put multiple context selectors in a single clause,
  ;;for example:
  ;;
  ;;  (case-context ?ctxt-expr
  ;;    ((p e v)	?stuff-body)
  ;;    (else		?else-body))
  ;;
  (lambda (stx)
    (define (%test x)
      (case (syntax->datum x)
	((p)   #'(eq? t 'p))
	((v)   #'(eq? t 'v))
	((e)   #'(eq? t 'e))
	((app) #'(app? t))
	(else
	 (syntax-violation stx "invalid ctxt" x))))
    (define (%extract cls*)
      (syntax-case cls* (else)
	(()
	 #'(error '%extract "unmatched ctxt" t))

	(((else ?body0 ?body ...))
	 #'(begin ?body0 ?body ...))

	((((t* ...) ?body0 ?body ...) rest ...)
	 (with-syntax (((T* ...) (map %test #'(t* ...)))
		       (BODY     (%extract #'(rest ...))))
	   #'(if (or T* ...)
		 (begin ?body0 ?body ...)
	       BODY)))
	))
    (syntax-case stx ()
      ((_ ?expr ?clause ...)
       (with-syntax ((BODY (%extract #'(?clause ...))))
	 #'(let ((t ?expr))
	     BODY)))
      )))


(define (make-seq-discarding-useless expr0 expr1)
  ;;Given a sequence of expressions EXPR0 and EXPR1: discard those that are evaluated
  ;;for  their  side  effects,  but  have   no  side  effects.   Return  a  structure
  ;;representing recordised code, not necessarily a SEQ struct.
  ;;
  ;;EXPR0 and EXPR1  are meant to be fields  from a SEQ struct; both  EXPR0 and EXPR1
  ;;are meant to have been already processed by the function E.
  ;;
  ;;Specifically, recognising that a struct SEQ in recordised code represents a BEGIN
  ;;syntax:
  ;;
  ;;* Given:
  ;;
  ;;     (begin expr0 expr1)
  ;;
  ;;  check if EXPR0 is without side-effects, and in this case discard it and return:
  ;;
  ;;     expr1
  ;;
  ;;* Given:
  ;;
  ;;     (begin (begin expr0.expr0 expr0.expr1) expr1)
  ;;
  ;;  check if EXPR0.EXPR1  is without side-effects, and in this  case discard it and
  ;;  return:
  ;;
  ;;     (begin expr0.expr0 expr1)
  ;;
  ;;* Given:
  ;;
  ;;     (begin expr0 (begin expr1.expr0 expr1.expr1))
  ;;
  ;;  check if EXPR1.EXPR0  is without side-effects, and in this  case discard it and
  ;;  return:
  ;;
  ;;     (begin expr0 expr1.expr1)
  ;;
  ;;Return a  (seq expr0 expr1)  with a  seq-less expr1 if  both expr0 and  expr1 are
  ;;constructed properly.
  ;;
  (if (simple-expression-without-side-effects? expr0)
      expr1
    (let ((expr0 (struct-case expr0
		   ((seq expr0.expr0 expr0.expr1)
		    (if (simple-expression-without-side-effects? expr0.expr1)
			expr0.expr0
		      expr0))
		   (else
		    expr0))))
      (struct-case expr1
	((seq expr1.expr0 expr1.expr1)
	 (make-seq (make-seq expr0 expr1.expr0) expr1.expr1))
	(else
	 (make-seq expr0 expr1))))))

(define (simple-expression-without-side-effects? x)
  ;;Check quickly whether something is effect-free.
  ;;
  (struct-case x
    ((constant) #t)
    ((prelex)   #t)
    ((primref)  #t)
    ((clambda)  #t)
    (else       #f)))

(define (result-expr x)
  ;;Return the "last"  value of an expression.
  ;;
  (struct-case x
    ((seq e0 e1)
     (result-expr e1))
    (else
     x)))

;;; --------------------------------------------------------------------

(define (residualize-ref x sc)
  ;;X  must  be  a  struct  instance  of  type  PRELEX  representing  an
  ;;identifier in reference position.  SC is the size counter.
  ;;
  ;;Just  return  X   after  marking  it  as   "still  referenced  after
  ;;optimization attempt".
  ;;
  (decrement sc 1)
  (set-prelex-residual-referenced?! x #t)
  x)

(define (residualize-operands tail-expr rand* sc)
  ;;Generate a sequence of expressions to allow evaluating some operands
  ;;for their side effects.
  ;;
  ;;Example: if a function call is inlined, we may need to evaluate some
  ;;of  its  operands   for  their  side  effect;   given  the  function
  ;;definition:
  ;;
  ;;   (define (fun x y)
  ;;     (list 123 x))
  ;;
  ;;in which Y is not used, we want the call:
  ;;
  ;;   (fun 456 (display "ciao"))
  ;;
  ;;to be transformed into:
  ;;
  ;;   (begin
  ;;     (display "ciao")
  ;;     (list 123 456))
  ;;
  ;;Example: if a LET binding is never referenced, we want the following
  ;;code:
  ;;
  ;;   (let ((a (do-this))
  ;;         (b 123))
  ;;     (do-that b))
  ;;
  ;;to be optimized into:
  ;;
  ;;   (begin
  ;;     (do-this)
  ;;     (let ((b 123))
  ;;       (do-that b)))
  ;;
  ;;TAIL-EXPR  is  a  struct  instance  representing  already  optimized
  ;;recordized  code which  may have  left some  of its  operands to  be
  ;;evaluated; this code must be evaluated after the left-over operands.
  ;;
  ;;RAND* must  be null or  a list of  struct instances of  type OPERAND
  ;;representing all  the operands that  were present in  the expression
  ;;that originated TAIL-EXPR; some of them may need to be evaluated for
  ;;their side effects,  others can be ignored  because already included
  ;;in TAIL-EXPR.
  ;;
  ;;SC is the size counter.
  ;;
  (cond ((null? rand*)
	 tail-expr)
	((not (operand-residualize-for-effect (car rand*)))
	 ;;This operand does not need evaluation.
	 (residualize-operands tail-expr (cdr rand*) sc))
	(else
	 (let* ((opnd     (car rand*))
		(pre-expr (or (operand-value opnd)
			      (struct-case opnd
				((operand opnd.expr opnd.env opnd.ec)
				 (E opnd.expr 'e opnd.env opnd.ec sc))))))
	   (if (simple-expression-without-side-effects? pre-expr)
	       ;;This operand has no side effects.
	       (residualize-operands tail-expr (cdr rand*) sc)
	     (begin
	       (decrement sc (operand-size opnd))
	       (make-seq-discarding-useless pre-expr
					    (residualize-operands tail-expr (cdr rand*) sc))))))))

;;; --------------------------------------------------------------------

(define (value-visit-operand! rand)
  ;;RAND  must  be  a  struct  instance of  type  OPERAND.   Run  source
  ;;optimization for  RAND and  return the  result.  Also  the optimized
  ;;result and  its counted size  are cached  in the fields  "value" and
  ;;"size" of RAND.
  ;;
  (or (operand-value rand)
      (let* ((sc (passive-counter))
	     (e  (struct-case rand
		   ((operand expr env ec)
		    (E expr 'v env sc ec)))))
	(set-operand-value! rand e)
	(set-operand-size!  rand (passive-counter-value sc))
	e)))

(define (score-value-visit-operand! rand sc)
  ;;Like  VALUE-VISIT-OPERAND! but  also accounts  for the  operation by
  ;;decrementing the size counter SC.
  ;;
  (begin0
      (value-visit-operand! rand)
    (decrement sc (operand-size rand))))


(module (inline-referenced-operand)

  (define (inline-referenced-operand x opnd ctxt ec sc)
    ;;Attempt to substitute a variable reference to with an expansion of
    ;;the referenced value.
    ;;
    ;;X is a  struct instance of type PRELEX  representing an identifier
    ;;in reference position.
    ;;
    ;;OPND is the operand associated to X.
    ;;
    ;;CTXT can be either an evaluation  context symbol (one among: p, e,
    ;;v) or a struct instance of type APP.
    ;;
    ;;EC is the effort counter.
    ;;
    ;;SC is the size counter.
    ;;
    (let ((rhs (result-expr (operand-value opnd))))
      (struct-case rhs
	((constant)
	 ;;The referenced value is a constant.  For example, simplify as
	 ;;follows:
	 ;;
	 ;;   (let ((X 1))
	 ;;     X) ;; <- reference to varaible
	 ;;   ==> (let ((X 1))
	 ;;         1)
	 ;;
	 rhs)
	((prelex)
	 ;;The referenced value is itself a variable reference.  Attempt
	 ;;to simplify as follows:
	 ;;
	 ;;   (let ((y 1)
	 ;;         (X y))
	 ;;     X) ;; <- reference to variable
	 ;;   ==> (let ((y 1)
	 ;;             (X y))
	 ;;         y)
	 ;;
	 (if (prelex-source-assigned? rhs)
	     (residualize-ref x sc)
	   (let ((opnd (prelex-operand rhs)))
	     (if (and opnd (operand-value opnd))
		 (%copy2 rhs opnd ctxt ec sc)
	       (residualize-ref rhs sc)))))
	(else
	 (%copy2 x opnd ctxt ec sc)))))

  (define (%copy2 x opnd ctxt ec sc)
    (let ((rhs (result-expr (operand-value opnd))))
      (struct-case rhs
	((clambda)
	 ;;X is a reference to user defined function (not a Vicare primitive).
	 (case-context ctxt
	   ((v)
	    (residualize-ref x sc))
	   ((p)
	    (make-constant #t))
	   ((e)
	    VOID-CONSTANT)
	   ((app)
	    ;;This is the case:
	    ;;
	    ;;  (define (a)
	    ;;    (do-something))
	    ;;
	    ;;  (a) ;; <-- reference to variable
	    ;;
	    ;;Attempt to inline the function application.
	    ;;
	    (or (and (enabled-function-application-integration?)
		     (not (operand-outer-pending opnd)) ;avoid evaluation cycles
		     (dynamic-wind
			 (lambda () (set-operand-outer-pending! opnd #t))
			 (lambda ()
			   (call/cc
			       (lambda (abort)
				 (inline-function-application rhs ctxt (make-empty-env)
							      ;;effort counter
							      (if (active-counter? ec)
								  ec
								(make-counter (cp0-effort-limit) ctxt abort))
							      ;;size counter
							      (make-counter (if (active-counter? sc)
										(counter-value sc)
									      (cp0-size-limit))
									    ctxt abort)))))
			 (lambda () (set-operand-outer-pending! opnd #f))))
		(residualize-ref x sc)))))

	((primref primsym)
	 ;;X is a reference to Vicare primitive function.  For example:
	 ;;
	 ;;   (list   ;; <- reference to variable
	 ;;    1 2 3)
	 ;;
	 (case-context ctxt
	   ((v)		rhs)
	   ((p)		(make-constant #t))
	   ((e)		VOID-CONSTANT)
	   ((app)	(fold-prim primsym ctxt ec sc))))

	(else
	 ;;Give up.  No optimizations possible.
	 (residualize-ref x sc)))))

  #| end of module: INLINE-REFERENCED-OPERAND |# )


(module (inline-function-application)
  ;;How do we inline a function?  Let's suppose we have this definition:
  ;;
  ;;   (define (fun x y)
  ;;     (list x y))
  ;;
  ;;and the function application:
  ;;
  ;;   (fun 123 456)
  ;;
  ;;we consider the function application as equivalent to:
  ;;
  ;;   ((lambda (x y) (list x y))
  ;;    123 456)
  ;;
  ;;which is equivalent to:
  ;;
  ;;   (let ((x 123)
  ;;         (y 456))
  ;;     (list x y))
  ;;
  ;;and now we have an expression to optimize as all the others.
  ;;
  (define (inline-function-application proc ctxt env ec sc)
    ;;
    ;;PROC  is  a  struct  instance of  type  CLAMBDA  representing  the
    ;;function to be applied.
    ;;
    ;;CTXT is a struct instance of  type APP representing the context of
    ;;the function application.
    ;;
    ;;ENV is  a struct  instance of type  ENV representing  the bindings
    ;;visible from this function application.
    ;;
    ;;EC is the effort counter.  SC is the size counter.
    ;;
    (struct-case proc
      ((clambda label.unused cases)
       (let ((rand* (app-rand* ctxt)))
	 (struct-case (get-case cases rand*)
	   ((clambda-case info body)
	    (struct-case info
	      ((case-info label.unused formals proper?)
	       (if proper?
		   (%application-with-fixed-formals formals rand* body ctxt env ec sc)
		 (%application-with-var-formals formals rand* body ctxt env ec sc)))))
	   (else
	    (E proc 'v env ec sc)))))))

;;; --------------------------------------------------------------------

  (define (get-case cases rand*)
    (define-inline (main)
      (cond ((memp %compatible? cases)
	     => car)
	    (else #f)))

    (define (%compatible? x)
      (struct-case (clambda-case-info x)
	((case-info label args proper)
	 (let ((rand*.len (length rand*))
	       (args.len  (length args)))
	   (if proper
	       (= rand*.len args.len)
	     (>= rand*.len (- args.len 1)))))))

    (main))

;;; --------------------------------------------------------------------

  (define (%application-with-fixed-formals formals rand* body ctxt env ec sc)
    ;;Here we have a function with a fixed number of arguments:
    ;;
    ;;   ((lambda (x y) (list x y))
    ;;    123 456)
    ;;
    ;;and consider it equivalent to:
    ;;
    ;;   (let ((x 123)
    ;;         (y 456))
    ;;     (list x y))
    ;;
    (with-extended-env ((env formals) <== (env formals rand*))
      (let ((optimized-body (E body (app-ctxt ctxt) env ec sc)))
	(begin0
	  (make-let-binding formals rand* optimized-body sc)
	  (set-app-inlined! ctxt #t)))))

;;; --------------------------------------------------------------------

  (module (%application-with-var-formals)
    ;;Here we have a function with a variable number of arguments:
    ;;
    ;;   ((lambda (x y . rest) (list x y rest))
    ;;    123 456 7 8 9)
    ;;
    ;;and consider it equivalent to:
    ;;
    ;;   (let ((x    123)
    ;;         (y    456)
    ;;         (tmp0 7)
    ;;         (tmp1 8)
    ;;         (tmp2 9))
    ;;     (let ((rest (list tmp0 tmp1 tmp2)))
    ;;       (list x y rest)))
    ;;
    ;;We also want to support the case:
    ;;
    ;;   ((lambda args (list args))
    ;;    1 2 3)
    ;;
    ;;and consider it equivalent to:
    ;;
    ;;   (let ((tmp0 1)
    ;;         (tmp1 2)
    ;;         (tmp3 3))
    ;;     (let ((args (list tmp0 tmp1 tmp2)))
    ;;       (list args)))
    ;;
    (define (%application-with-var-formals formals rand* body ctxt env ec sc)
      (let-values (((lhs* tmp* rest-formal) (%partition formals rand*)))
	(with-extended-env ((env a*) <== (env (append lhs* tmp*) rand*))
	  (let ((rest-rand (make-operand (make-funcall (mk-primref 'list) tmp*)
					 env ec)))
	    (with-extended-env ((env b*) <== (env (list rest-formal) (list rest-rand)))
	      (let* ((optim-body (E body (app-ctxt ctxt) env ec sc))
		     (sublet     (make-let-binding b* (list rest-rand) optim-body sc))
		     (result     (make-let-binding a* rand* sublet sc)))
		(set-app-inlined! ctxt #t)
		result))))))

    (define (%partition formals rand*)
      ;;Given a  list of  CLAMBDA formals  with rest arg  and a  list of
      ;;operands, return 3 values:
      ;;
      ;;1. A  list of struct instances  of type PRELEX being  the LHS of
      ;;   the fixed formals.
      ;;
      ;;2. A  list of struct instances  of type PRELEX being  the LHS of
      ;;   the left-over arguments.
      ;;
      ;;3. A  struct instance of type  PRELEX being the LHS  of the rest
      ;;   argument binding.
      ;;
      (if (pair? (cdr formals))
	  (let ((lhs (car formals)))
	    (receive (lhs* tmp-formal* rest-formal)
		(%partition (cdr formals) (cdr rand*))
	      (values (cons lhs lhs*) tmp-formal* rest-formal)))
	;;Everything else goes into the rest argument.
	(let* ((rest-formal (car formals))
	       (tmp-formal* (map (lambda (unused)
				   (make-prelex-replacement rest-formal))
			      rand*)))
	  (values '() tmp-formal* rest-formal))))

    #| end of module: %APPLICATION-WITH-VAR-FORMALS |# )

  #| end of module: INLINE-FUNCTION-APPLICATION |# )


(module (make-let-binding)
  ;;This module is used to process a LET-like set of bindings.
  ;;
  ;;Basically we want to remove unreferenced bindings:
  ;;
  ;;   (let ((a (do-this))
  ;;         (b 123))
  ;;     (do-that b))
  ;;   ==> (begin
  ;;         (do-this)
  ;;         (let ((b 123))
  ;;           (do-that b)))
  ;;
  ;;and transform bindings that are  only assigned and not referenced as
  ;;follows:
  ;;
  ;;   (letrec ((a (display "ciao"))
  ;;            (b (lambda (x) a (list x))))
  ;;     (set! a 123)
  ;;     123)
  ;;
  ;;Here  is  an  interesting  example  of  what  happens.   After  full
  ;;expansion the code in the core language is:
  ;;
  ;;   (letrec ((a (display "ciao"))
  ;;            (b (lambda (x) a (list x))))
  ;;     (set! a 123)
  ;;     123)
  ;;
  ;;in  which A  is referenced  and assigned,  so an  internal structure
  ;;reports  that it  is  "referenced before  optimization"; the  LETREC
  ;;optimizer transforms the code into (not quite but close enough):
  ;;
  ;;   (let ((a (display '"ciao")))
  ;;     (let ((b (lambda (x)
  ;;                (begin
  ;;                  a
  ;;                  (list x)))))
  ;;       (begin
  ;;         (set! a '123)
  ;;         '123)))
  ;;
  ;;and now comes the source optimizer:
  ;;
  ;;*  First it  processes the  inner  body, and  since A  is marked  as
  ;;  referenced before optimization, it does not remove the assignment;
  ;;  rather it marks A as "still assigned after optimization".
  ;;
  ;;* Then it processes the inner LET, recognising the function B as not
  ;;  referenced; so it removes it:
  ;;
  ;;   (let ((a (display '"ciao")))
  ;;     (begin
  ;;       (set! a '123)
  ;;       '123))
  ;;
  ;;  A is left marked as "not referenced after optimization".
  ;;
  ;;* Finally  it processes  the outer  LET, now A  is actually  no more
  ;;  referenced, but still assigned after optimization and so the final
  ;;  transformation is:
  ;;
  ;;   (begin
  ;;     (display '"ciao")
  ;;     (let ((a #<unspecified>))
  ;;       (begin
  ;;         (set! a '123)
  ;;         '123)))
  ;;
  ;;  because it is too late to remove the assignment.
  ;;
  (define (make-let-binding var* rand* optimized-body sc)
    ;;
    ;;VAR*  is  a list  of  struct  instances  of type  PRELEX,  already
    ;;processed  by  WITH-EXTENDED-ENV,  being the  left-hand  sides  of
    ;;bindings.
    ;;
    ;;RAND* is  a list  of struct  instances of  type OPERAND  being the
    ;;right-hand sides of bindings.
    ;;
    ;;OPTIMIZED-BODY is a struct instance representing already optimized
    ;;recordized code.
    ;;
    ;;SC is the size counter.
    ;;
    ;;If there are no bindings to  begin with, or they are all optimized
    ;;away: just return the OPTIMIZED-BODY.  Else return a BIND struct.
    ;;
    (let-values (((lhs* rhs*) (%attempt-bindings-removal var* rand* sc)))
      (if (null? lhs*)
	  optimized-body
	(make-bind lhs* rhs* optimized-body))))

  (define (%attempt-bindings-removal var* rand* sc)
    (if (pair? var*)
	(receive (lhs* rhs*)
	    (%attempt-bindings-removal (cdr var*) (cdr rand*) sc)
	  (%process-single-binding (car var*) (car rand*) lhs* rhs* sc))
      (values '() '())))

  (define (%process-single-binding var rand lhs* rhs* sc)
    ;;
    ;;VAR must be  a struct instance of type PRELEX  being the left-hand
    ;;side of the binding.
    ;;
    ;;RAND  must  be  a  struct  instance  of  type  OPERAND  being  the
    ;;right-hand side of the binding.
    ;;
    (cond ((prelex-residual-referenced? var)
	   ;;After optimization,  this variable is still  referenced; so
	   ;;we include it in the output.
	   (values (cons var lhs*)
		   (cons (score-value-visit-operand! rand sc)
			 rhs*)))
	  ((prelex-residual-assigned? var)
	   ;;After  optimization, this  variable is  not referenced  but
	   ;;assigned; so we include it in the output.
	   (set-operand-residualize-for-effect! rand #t)
	   (values (cons var lhs*)
		   (cons VOID-CONSTANT rhs*)))
	  (else
	   ;;After optimization, this variable is neither referenced not
	   ;;assigned; so we exclude the binding and mark the operand to
	   ;;be evaluated for its side effects.
	   (set-operand-residualize-for-effect! rand #t)
	   (values lhs* rhs*))))

  #| end of module: make-let-binding |# )


(module (fold-prim)
  ;;Whenever possible attempt  to precompute the result of a  core primitive function
  ;;application.    This  is   the   place  where   the  "foldable",   "effect-free",
  ;;"result-true" and "result-false" primitive application attributes are used.
  ;;
  (module (core-primitive-name->application-attributes*
	   application-attributes-operands-template
	   application-attributes-foldable?
	   application-attributes-effect-free?
	   application-attributes-result-true?
	   application-attributes-result-false?
	   CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES)
    (import CORE-PRIMITIVE-PROPERTIES))

  (define (fold-prim prim-name appctxt ec sc)
    ;;PRIM-NAME must  be a symbol  being the name  of a primitive  function.  APPCTXT
    ;;must be a  struct instance of type APP, representing  the context of appliction
    ;;for PRIM-NAME.  EC is the effort counter.  SC is the size counter.
    ;;
    (let* ((rand*  (app-rand* appctxt))
	   (info   (%primitive-info prim-name rand*))
	   (result (or (and (application-attributes-effect-free? info)
			    (%precompute-effect-free-primitive info appctxt))
		       (and (application-attributes-foldable? info)
			    (%precompute-foldable-primitive prim-name rand* ec)))))
      (if result
	  (begin
	    (decrement ec 1)
	    ;;When we avoid calling a primitive  function, its operands must still be
	    ;;evaluated for  their side  effects; so  mark them  for this.   In other
	    ;;words, we want:
	    ;;
	    ;;  (begin
	    ;;    (list (display "ciao"))
	    ;;    123)
	    ;;
	    ;;to be transformed into:
	    ;;
	    ;;  (begin
	    ;;    (display "ciao")
	    ;;    123)
	    ;;
	    ($for-each/stx (lambda (x)
			     (set-operand-residualize-for-effect! x #t))
	      rand*)
	    (set-app-inlined! appctxt #t)
	    result)
	(begin
	  (decrement sc 1)
	  (mk-primref prim-name)))))

  (define (%precompute-effect-free-primitive info appctxt)
    ;;The  function call  is effect  free.  If  the evaluation  context is  "for side
    ;;effects":  the function  call can  be removed.   If the  evaluation context  is
    ;;"predicate": check if the function call has known boolean result.
    ;;
    (case-context (app-ctxt appctxt)
      ((e)
       VOID-CONSTANT)
      ((p)
       (cond ((application-attributes-result-true?  info)
	      (make-constant #t))
	     ((application-attributes-result-false? info)
	      (make-constant #f))
	     (else #f)))
      (else #f)))

  (define (%precompute-foldable-primitive prim-name rand* ec)
    ;;The function call is foldable; if all the operands are constant: precompute the
    ;;function call and return a struct instance of type CONSTANT; else return false.
    ;;
    ;;PRIM-NAME must be a symbol being the  name of a primitive function.  RAND* must
    ;;be  a  list  of  struct  instances representing  recordized  code  which,  when
    ;;evaluated, will return the arguments.  EC is the effort counter.
    ;;
    (let ((val* (map (lambda (x)
		       (value-visit-operand! x))
		  rand*)))
      (and (andmap constant? val*)
	   (%apply-at-compile-time prim-name (map constant-value val*) ec))))

  (define (%apply-at-compile-time prim-name args ec)
    ;;Apply  the primitive  associated  to PRIM-NAME  to the  list  of arguments;  if
    ;;successful: return a struct instance of  type CONSTANT holding the result; else
    ;;return false.
    ;;
    ;;PRIM-NAME must be a  symbol being the name of a  primitive function.  ARGS must
    ;;be a list of constant values representing the  arguments of a call to P.  EC is
    ;;the effort counter.
    ;;
    ;;See the documentation of the function SYSTEM-VALUE for an explanation of why we
    ;;can extract the function from PRIM-NAME.
    ;;
    (call/cc
	(lambda (escape)
	  (with-exception-handler
	      (lambda (con)
		(decrement ec 10)
		(escape #f))
	    (lambda ()
	      #;(debug-print 'apply-at-compile-time prim-name args)
	      (make-constant (apply (system-value prim-name) args)))))))

;;; --------------------------------------------------------------------

  (define* (%primitive-info prim-name rand*)
    ;;PRIM-NAME  must be  a symbol  representing the  name of  a primitive  function.
    ;;RAND* must be null  or a list representing the arguments in  a function call to
    ;;PRIM-NAME.
    ;;
    ;;This function scans the attributes list of PRIM-NAME searching for entries that
    ;;match the arguments call  represented by RAND*.  If a match  is found: return a
    ;;list of symbols representing the attributes; else return null.
    ;;
    (define (%matches? application-attributes)
      ;;Match the operands in RAND* against the primitive application template in the
      ;;items of APPLICATION-ATTRIBUTES.
      ;;
      (let loop ((rand*             rand*)
		 (operands-template (application-attributes-operands-template application-attributes)))
	(cond ((pair? operands-template)
	       (and (pair? rand*)
		    (let ((optmpl (car operands-template)))
		      (case optmpl
			((_)
			 ;;An argument matched.  Go on with the rest.
			 (loop (cdr rand*) (cdr operands-template)))
			((#f 0 ())
			 ;;The  template specifies  that  this argument  must be  one
			 ;;among: #f,  0, nil; if it  is: we can fold  specially this
			 ;;primitive application; else this template does not match.
			 (let ((v (value-visit-operand! (car rand*))))
			   (and (constant? v)
				(equal? optmpl (constant-value v))
				(loop (cdr rand*) (cdr operands-template)))))
			(else
			 (compiler-internal-error __who__
			   "invalid core primitive template operand specification"
			   prim-name (car operands-template)))))))
	      ((eq? operands-template '_)
	       ;;Success!  The  template is  an improper list:  it represents  a call
	       ;;accepting  any number  of arguments  (possibly after  some mandatory
	       ;;arguments).
	       #t)
	      ((null? operands-template)
	       ;;The  template is  a proper  list.  If  RAND* is  null: success,  the
	       ;;template matches the arguments!  Else this template does not match.
	       (null? rand*))
	      (else
	       (compiler-internal-error __who__
		 "invalid core primitive template operand specification"
		 prim-name operands-template)))))
    (cond ((core-primitive-name->application-attributes* prim-name)
	   => (lambda (application-attributes*)
		;;A proper list:
		;;
		;; (?application-attributes ...)
		;;
		;;in  which  every  ?APPLICATION-ATTRIBUTES   is  a  struct  of  type
		;;APPLICATION-ATTRIBUTES.
		(cond ((find %matches? application-attributes*))
		      (else
		       CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES))))
	  (else
	   CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES)))

  #| end of module: FOLD-PRIM |# )


;;;; done

#| end of source optimiser module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; eval: (put 'case-context		'scheme-indent-function 1)
;; eval: (put 'with-extended-env	'scheme-indent-function 1)
;; End:

