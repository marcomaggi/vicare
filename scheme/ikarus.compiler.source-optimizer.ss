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


;;; Work in progress
;;
;;See the paper:
;;
;;   Oscar  Waddell.  "Extending  the Scope  of Syntactic  Abstraction".
;;   PhD.   Thesis.   Indiana  University Computer  Science  Department.
;;   August 1999.
;;
;;Available online:
;;
;;   <http://www.cs.indiana.edu/~owaddell/papers/thesis.ps.gz>
;;
(module (source-optimize
	 optimize-level
	 cp0-effort-limit
	 cp0-size-limit)
  (define who 'source-optimize)

  (define cp0-effort-limit
    (make-parameter 50))

  (define cp0-size-limit
    (make-parameter 8))

  (define optimize-level
    (make-parameter 0
      (lambda (x)
	(if (memv x '(0 1 2))
	    x
	  (error 'optimize-level "valid optimization levels are 0, 1, and 2")))))

  (define (source-optimize expr)
    (define (%doit expr)
      (E expr 'v EMPTY-ENV (passive-counter) (passive-counter)))
    (case (optimize-level)
      ((2)
       (%doit expr))
      ((1)
       (parameterize ((cp0-size-limit 0))
	 (%doit expr)))
      (else
       expr)))

  (define-inline-constant EMPTY-ENV
    '())


;;;; type definitions

;;Represent the context  in which a primitive function  (CONS, CAR, ...)
;;is applied.
;;
;;Fields:
;;
;;RAND*  is  a  list  of  struct  instances  representing  the  function
;;application operands.
;;
;;CTXT is an evaluation context symbol among: p, e, v, app.
;;
;;INLINED is a boolean, true  if the function application represented by
;;this struct instance has been expanded inline (taking advantage of the
;;known primitive function attributes).
;;
(define-structure (app rand* ctxt)
  ((inlined			#f)))

;;Represent an expression being the operand to a function call.
;;
(define-structure (operand expr env ec)
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
   ctxt
   k
		;Continuation function to be called if the threshold for
		;this counter has been reached.
   ))


(module (E)
  ;;Iterate   over  recordized   code  performing   source  optimization
  ;;transformations; this module is the actual source optimizer.
  ;;
  ;;This module accepts  as input recordized code  containing the struct
  ;;instances of the following types:
  ;;
  ;;assign		bind		clambda
  ;;conditional		constant	fix
  ;;forcall		funcall		prelex
  ;;primref		seq
  ;;
  ;;and returns recordized code composed of the same struct types.
  ;;
  (define (E x ctxt env ec sc)
    ;;Recursive function performing source optimizations.
    ;;
    ;;X must be a struct instance representing recordized code.
    ;;
    ;;CTXT must be a context symbol among: p, e, v, app.
    ;;
    ;;EC is the effort counter.
    ;;
    ;;SC is the size counter.
    ;;
    (decrement ec 1)
    (struct-case x
      ((constant)
       (decrement sc 1)
       x)

      ((prelex)
       (E-var x ctxt env ec sc))

      ((seq e0 e1)
       (mkseq (E e0 'e env ec sc)
	      (E e1 ctxt env ec sc)))

      ((conditional x.test x.conseq x.altern)
       (let ((test (E x.test 'p env ec sc)))
	 (struct-case (result-expr test)
	   ((constant test.val)
	    ;;We could precompute  the result of the TEST,  so we output
	    ;;only the CONSEQ or the ALTERN.
	    (mkseq test (E (if test.val x.conseq x.altern)
			   ctxt env ec sc)))
	   (else
	    ;;The TEST could not be precomputed.
	    (let ((ctxt (case-context ctxt
			  ((app) 'v)
			  (else  ctxt))))
	      (let ((x.conseq (E x.conseq ctxt env ec sc))
		    (x.altern (E x.altern ctxt env ec sc)))
		(if (records-equal? x.conseq x.altern ctxt)
		    ;;If the results  of CONSEQ and ALTERN  are known to
		    ;;be  equal:  we  just  include TEST  for  its  side
		    ;;effects, followed by CONSEQ.
		    (mkseq test x.conseq)
		  (begin
		    (decrement sc 1)
		    (%build-conditional test x.conseq x.altern)))))))))

      ((assign x v)
       (mkseq (let ((x (%lookup x env)))
		(if (not (prelex-source-referenced? x))
		    ;;dead on arrival
		    (E v 'e env ec sc)
		  (begin
		    (decrement sc 1)
		    (set-prelex-residual-assigned?! x (prelex-source-assigned? x))
		    (make-assign x (E v 'v env ec sc)))))
	      (make-constant (void))))

      ((funcall rator rand*)
       (E-call rator (map (lambda (x)
			    (make-operand x env ec))
		       rand*)
	       env ctxt ec sc))

      ((forcall name rand*)
       (decrement sc 1)
       (make-forcall name (map (lambda (x)
				 (E x 'v env ec sc))
			    rand*)))

      ((primref name)
       (case-context ctxt
	 ((app)
	  (case-symbols name
	    ((debug-call)
	     (E-debug-call ctxt ec sc))
	    (else
	     (fold-prim name ctxt ec sc))))
	 ((v)
	  (decrement sc 1)
	  x)
	 (else
	  (make-constant #t))))

      ((clambda label.unused clause* cp free name)
       (%do-clambda clause* cp free name   x ctxt env ec sc))

      ((bind lhs* rhs* body)
       (%do-bind lhs* rhs* body ctxt env ec sc))

      ((fix lhs* rhs* body)
       (%do-fix  lhs* rhs* body ctxt env ec sc))

      (else
       (error who "invalid expression" x))))

;;; --------------------------------------------------------------------

  (define (E-call rator rand* env ctxt ec sc)
    (let* ((ctxt  (make-app rand* ctxt))
	   (rator (E rator ctxt env ec sc)))
      (if (app-inlined ctxt)
	  (residualize-operands rator rand* sc)
	(begin
	  (decrement sc (if (primref? rator) 1 3))
	  (make-funcall rator (map (lambda (x)
				     (score-value-visit-operand! x sc))
				rand*))))))

  (define (E-debug-call ctxt ec sc)
    (let ((rand* (app-rand* ctxt)))
      (if (< (length rand*) 2)
	  (begin
	    (decrement sc 1)
	    (make-primref 'debug-call))
	(begin
	  (let ((src/expr (car rand*))
		(rator (cadr rand*))
		(rands (cddr rand*)))
	    (let ((ctxt2 (make-app rands (app-ctxt ctxt))))
	      (let ((rator (E (operand-expr rator)
			      ctxt2
			      (operand-env rator)
			      (operand-ec rator)
			      sc)))
		(if (app-inlined ctxt2)
		    (begin
		      (set-app-inlined! ctxt #t)
		      (residualize-operands rator (cons src/expr rands) sc))
		  (begin
		    (decrement sc 1)
		    (make-primref 'debug-call))))))))))

  (define (E-var x ctxt env ec sc)
    (case-context ctxt
      ((e)
       (make-constant (void)))
      (else
       (let* ((x    (%lookup x env))
	      (opnd (prelex-operand x)))
	 (if (and opnd (not (operand-inner-pending opnd)))
	     (begin
	       (dynamic-wind
		   (lambda () (set-operand-inner-pending! opnd #t))
		   (lambda () (value-visit-operand! opnd))
		   (lambda () (set-operand-inner-pending! opnd #f)))
	       (if (prelex-source-assigned? x)
		   (residualize-ref x sc)
		 (copy x opnd ctxt ec sc)))
	   (residualize-ref x sc))))))

;;; --------------------------------------------------------------------

  (define (%lookup x env)
    (if (vector? env)
	(let f ((lhs* (vector-ref env 0)) (rhs* (vector-ref env 1)))
	  (cond ((null? lhs*)
		 (%lookup x (vector-ref env 2)))
		((eq? x (car lhs*))
		 (car rhs*))
		(else
		 (f (cdr lhs*) (cdr rhs*)))))
      x))

  (define (%build-conditional e0 e1 e2)
    (or (struct-case e0
	  ((funcall rator rand*)
	   (struct-case rator
	     ((primref op)
	      (and (eq? op 'not)
		   (= (length rand*) 1)
		   (%build-conditional (car rand*) e2 e1)))
	     (else
	      #f)))
	  (else
	   #f))
	(make-conditional e0 e1 e2)))

  (define (%do-bind lhs* rhs* body ctxt env ec sc)
    (let ((rand* (map (lambda (x)
			(make-operand x env ec))
		   rhs*)))
      (with-extended-env ((env lhs*)
			  (env lhs* rand*))
	(residualize-operands (make-let-binding lhs* rand* (E body ctxt env ec sc) sc)
			      rand* sc))))

  (define (%do-fix lhs* rhs* body ctxt env ec sc)
    (with-extended-env ((env lhs*)
			(env lhs* #f))
      (for-each (lambda (lhs rhs)
		  (set-prelex-operand! lhs (make-operand rhs env ec)))
	lhs* rhs*)
      (let* ((body (E body ctxt env ec sc))
	     (lhs* (remp (lambda (x)
			   (not (prelex-residual-referenced? x)))
		     lhs*)))
	(if (null? lhs*)
	    body
	  (begin
	    (decrement sc 1)
	    (make-fix lhs* (map (lambda (x)
				  (let ((opnd (prelex-operand x)))
				    (decrement sc (+ (operand-size opnd) 1))
				    (value-visit-operand! opnd)))
			     lhs*)
		      body))))))

  (define (%do-clambda clause* cp free name   x ctxt env ec sc)
    (case-context ctxt
      ((app)
       ;;This  is  the  case  of CASE-LAMBDA  evaluation  and  immediate
       ;;application:
       ;;
       ;;   ((lambda (x) (do-something-with x))
       ;;    123)
       ;;
       (inline x ctxt env ec sc))
      ((p e)
       ;;This is the case of "predicate" context:
       ;;
       ;;   (if (case-lambda ---)
       ;;       (if-true)
       ;;     (if-false))
       ;;
       ;;or "for side effect" context:
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
       (decrement sc 2)
       (make-clambda (gensym)
		     (map (lambda (x)
			    (struct-case x
			      ((clambda-case info body)
			       (struct-case info
				 ((case-info label args proper)
				  (with-extended-env ((env args)
						      (env args #f))
				    (make-clambda-case
				     (make-case-info (gensym) args proper)
				     (E body 'v env ec sc))))))))
		       clause*)
		     cp free name))))

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

  (define (%reset-integrated! appctxt)
    ;;Recursively reset the  inlined status of APPCTXT, which  must be a
    ;;struct instance of type APP.
    ;;
    (set-app-inlined! appctxt #f)
    (let ((nested-appctxt (app-ctxt appctxt)))
      (when (app? nested-appctxt)
	(%reset-integrated! nested-appctxt))))

  #| end of module: decrement |# )


(module (with-extended-env copy-var)

  (define-syntax with-extended-env
    (syntax-rules ()
      ((_ ((?e2 ?args2)
	   (?e1 ?args1 ?rands))
	  ?body0 ?body ...)
       (let-values (((?e2 ?args2) (%extend ?e1 ?args1 ?rands)))
	 (let ((v (let () ?body0 ?body ...)))
	   (%copy-back ?args2)
	   v)))))

  (define (copy-var x)
    (let ((y (make-prelex (prelex-name x) #f)))
      (set-prelex-source-referenced?! y (prelex-source-referenced? x))
      (set-prelex-source-assigned?!   y (prelex-source-assigned?   x))
      (let ((loc (prelex-global-location x)))
	(when loc
	  (set-prelex-global-location!      y loc)
	  (set-prelex-source-referenced?!   y #t)
	  (set-prelex-residual-referenced?! y #t)))
      y))

;;; --------------------------------------------------------------------

  (define (%extend env lhs* rands)
    (if (null? lhs*)
	(values env '())
      (let ((nlhs* (map copy-var lhs*)))
	(when rands
	  (for-each (lambda (lhs rhs)
		      (set-prelex-operand! lhs rhs))
	    nlhs* rands))
	(values (vector lhs* nlhs* env)
		nlhs*))))

  (define (%copy-back ls)
    (for-each (lambda (x)
		(set-prelex-source-assigned?!   x (prelex-residual-assigned?   x))
		(set-prelex-source-referenced?! x (prelex-residual-referenced? x)))
      ls))

  #| end of module: with-extended-env |# )


(module (primprop)
  ;;Prepare a distributed  table of primitive function  attributes to be
  ;;used to precompute results of function applications.
  ;;
  ;;Attributes are the symbols:
  ;;
  ;;effect-free -  The application produces no side effects.
  ;;
  ;;foldable -     The application can be precomputed at compile time.
  ;;
  ;;result-true -  The application always has non-#f result.
  ;;
  ;;result-false - The application always has #f result.
  ;;
  (define (primprop p)
    (or (getprop p UNIQUE-PROPERTY-KEY) '()))

  (define-constant UNIQUE-PROPERTY-KEY
    (let-syntax
	((expand-time-gensym (lambda (x)
			       (with-syntax ((SYM (datum->syntax #'here (gensym))))
				 #'(quote SYM)))))
      (expand-time-gensym)))

  (module (%initialise-primitive-properties)
    ;;For each  symbol being the  name of  a primitive function:  add an
    ;;element to the property list of the symbol.
    ;;
    ;;The key of the property is UNIQUE-PROPERTY-KEY.
    ;;
    ;;The value of  the property is a list of  sublists representing the
    ;;attributes  for a  mode of  calling the  primitive function.   For
    ;;example, for CONS* the value will be:
    ;;
    ;;   (((_)		foldable effect-free)
    ;;    ((_ . _)	effect-free result-true))
    ;;
    ;;the first sublist  specifies the attributes of CONS*  applied to a
    ;;single argument;  the second  sublist specifies the  attributes of
    ;;CONS* applied to 2 or more arguments.
    ;;
    (define (%initialise-primitive-properties info-list)
      (unless (null? info-list)
	(let* ((a	(car info-list))
	       (cc	(car a))	;list, primitive usage template
	       (cv	(cdr a))	;list of symbols, properties
	       (prim	(car cc))	;symbol, primitive name
	       (args	(cdr cc)))	;list, arguments specification
	  (let-values (((p* info-list) (%get prim (cdr info-list))))
	    (putprop prim UNIQUE-PROPERTY-KEY (cons (cons args cv) p*))
	    (%initialise-primitive-properties info-list)))))

    (define (%get prim info-list)
      ;;Check  if  the  head  of  the  INFO-LIST  represents  additional
      ;;attributes for the primitive PRIM;  if it does return: a sublist
      ;;specifying attributes and the tail  of INFO-LIST; if it does not
      ;;return: nil and INFO-LIST itself.
      ;;
      (if (null? info-list)
	  (values '() '())
	(let* ((a  (car info-list))
	       (cc (car a)))
	  (if (eq? (car cc) prim)
	      (let-values (((p* info-list) (%get prim (cdr info-list))))
		(values (cons (cons (cdr cc) (cdr a))
			      p*)
			info-list))
	    (values '() info-list)))))

    #| end of module: %initialise-primitive-properties |# )

  (define-constant PRIMITIVE-INFO-LIST
    ;;Attributes  specifications for  each mode  of calling  a primitive
    ;;function.  There  can be any  number of specs for  each primitive,
    ;;but they must be in adjacent items.
    ;;
    ;;FIXME  There are  a  lot more  functions for  which  it should  be
    ;;interesting to specify attributes.  (Marco Maggi; Nov 3, 2012)
    ;;
    '(((cons _ _)				 effect-free result-true)
      ((cons* _)			foldable effect-free            )
      ((cons* _ . _)				 effect-free result-true)
      ((list)				foldable effect-free result-true)
      ((list . _)				 effect-free result-true)
      ((reverse ())			foldable effect-free result-true)

      ;;According to  R6RS: STRING and	MAKE-STRING must return	 a newly
      ;;allocated string at every invocation; if we want the same string
      ;;we just	 use the double	 quotes.  So STRING and	 MAKE-STRING are
      ;;not foldable.
      ((string)				    effect-free result-true)
      ((string . _)			    effect-free result-true)
      ((make-string 0)			    effect-free result-true)
      ((make-string 0 _)		    effect-free result-true)
      ((make-string . _)		    effect-free result-true)

      ;;According to R6RS: MAKE-BYTEVECTOR must return a newly allocated
      ;;string at every invocation; so it is not foldable.
      ((make-bytevector 0)		    effect-free result-true)
      ((make-bytevector 0 _)		    effect-free result-true)
      ((make-bytevector . _)				result-true)

      ((string-length _)	   foldable effect-free result-true)
      ((string-ref _ _)		   foldable effect-free result-true)

      ;;According to  R6RS: VECTOR and	MAKE-VECTOR must return	 a newly
      ;;allocated string at every invocation; so they are not foldable.
      ((vector)				    effect-free result-true)
      ((vector . _)			    effect-free result-true)
      ((make-vector 0)			    effect-free result-true)
      ((make-vector 0 _)		    effect-free result-true)
      ((make-vector . _)		    effect-free result-true)

      ((vector-length _)	   foldable effect-free result-true)
      ((vector-ref _ _)		   foldable effect-free		   )
      ((eq? _ _)		   foldable effect-free		   )
      ((eqv? _ _)		   foldable effect-free		   )
      ((assq _ _)		   foldable effect-free		   )
      ((assv _ _)		   foldable effect-free		   )
      ((assoc _ _)		   foldable effect-free		   )
      ((not _)			   foldable effect-free		   )
      ((null? _)		   foldable effect-free		   )
      ((pair? _)		   foldable effect-free		   )
      ((fixnum? _)		   foldable effect-free		   )
      ((vector? _)		   foldable effect-free		   )
      ((string? _)		   foldable effect-free		   )
      ((char? _)		   foldable effect-free		   )
      ((symbol? _)		   foldable effect-free		   )
      ((procedure? _)		   foldable effect-free		   )
      ((eof-object? _)		   foldable effect-free		   )
      ((flonum? _)		   foldable effect-free		   )
      ((cflonum? _)		   foldable effect-free		   )
      ((compnum? _)		   foldable effect-free		   )
      ((integer? _)		   foldable effect-free		   )
      ((bignum? _)		   foldable effect-free		   )
      ((ratnum? _)		   foldable effect-free		   )
      ((pointer? _)		   foldable effect-free		   )
      ((void)			   foldable effect-free result-true)
      ((car _)			   foldable effect-free		   )
      ((cdr _)			   foldable effect-free		   )
      ((caar _)			   foldable effect-free		   )
      ((cadr _)			   foldable effect-free		   )
      ((cdar _)			   foldable effect-free		   )
      ((cddr _)			   foldable effect-free		   )
      ((caaar _)		   foldable effect-free		   )
      ((caadr _)		   foldable effect-free		   )
      ((cadar _)		   foldable effect-free		   )
      ((caddr _)		   foldable effect-free		   )
      ((cdaar _)		   foldable effect-free		   )
      ((cdadr _)		   foldable effect-free		   )
      ((cddar _)		   foldable effect-free		   )
      ((cdddr _)		   foldable effect-free		   )
      ((caaaar _)		   foldable effect-free		   )
      ((caaadr _)		   foldable effect-free		   )
      ((caadar _)		   foldable effect-free		   )
      ((caaddr _)		   foldable effect-free		   )
      ((cadaar _)		   foldable effect-free		   )
      ((cadadr _)		   foldable effect-free		   )
      ((caddar _)		   foldable effect-free		   )
      ((cadddr _)		   foldable effect-free		   )
      ((cdaaar _)		   foldable effect-free		   )
      ((cdaadr _)		   foldable effect-free		   )
      ((cdadar _)		   foldable effect-free		   )
      ((cdaddr _)		   foldable effect-free		   )
      ((cddaar _)		   foldable effect-free		   )
      ((cddadr _)		   foldable effect-free		   )
      ((cdddar _)		   foldable effect-free		   )
      ((cddddr _)		   foldable effect-free		   )
      ((memq _ _)		   foldable effect-free		   )
      ((memv _ _)		   foldable effect-free		   )
      ((length _)		   foldable effect-free result-true)
      ((+ . _)			   foldable effect-free result-true)
      ((* . _)			   foldable effect-free result-true)
      ((/ _ . _)		   foldable effect-free result-true)
      ((- _ . _)		   foldable effect-free result-true)
      ((fx+ _ _)		   foldable effect-free result-true)
      ((fx- _ _)		   foldable effect-free result-true)
      ((fx* _ _)		   foldable effect-free result-true)
      ((fxior . _)		   foldable effect-free result-true)
      ((fxlogor . _)		   foldable effect-free result-true)
      ((fxnot _)		   foldable effect-free result-true)
      ((fxadd1 _)		   foldable effect-free result-true)
      ((fxsub1 _)		   foldable effect-free result-true)
      ((fxzero? _)		   foldable effect-free            )
      ((fx=? _ . _)		   foldable effect-free		   )
      ((fx<? _ . _)		   foldable effect-free		   )
      ((fx<=? _ . _)		   foldable effect-free		   )
      ((fx>? _ . _)		   foldable effect-free		   )
      ((fx>=? _ . _)		   foldable effect-free		   )
      ((fx= _ . _)		   foldable effect-free		   )
      ((fx< _ . _)		   foldable effect-free		   )
      ((fx<= _ . _)		   foldable effect-free		   )
      ((fx> _ . _)		   foldable effect-free		   )
      ((fx>= _ . _)		   foldable effect-free		   )
      ((fxsll _ _)		   foldable effect-free result-true)
      ((fxsra _ _)		   foldable effect-free result-true)
      ((fxremainder _ _)	   foldable effect-free result-true)
      ((fxquotient _ _)		   foldable effect-free result-true)
      ((real-part _)		   foldable effect-free result-true)
      ((imag-part _)		   foldable effect-free result-true)
      ((greatest-fixnum)	   foldable effect-free result-true)
      ((least-fixnum)		   foldable effect-free result-true)
      ((fixnum-width)		   foldable effect-free result-true)
      ((char->integer _)	   foldable effect-free result-true)
      ((integer->char _)	   foldable effect-free result-true)
      ((eof-object)		   foldable effect-free result-true)
      ((zero? _)		   foldable effect-free		   )
      ((= _ . _)		   foldable effect-free		   )
      ((< _ . _)		   foldable effect-free		   )
      ((<= _ . _)		   foldable effect-free		   )
      ((> _ . _)		   foldable effect-free		   )
      ((>= _ . _)		   foldable effect-free		   )
      ((expt _ _)		   foldable effect-free result-true)
      ((log _)			   foldable effect-free result-true)
      ((sll _ _)		   foldable effect-free result-true)
      ((sra _ _)		   foldable effect-free result-true)
      ((inexact _)		   foldable effect-free result-true)
      ((exact _)		   foldable effect-free result-true)
      ((add1 _)			   foldable effect-free result-true)
      ((sub1 _)			   foldable effect-free result-true)
      ((bitwise-and _ _)	   foldable effect-free result-true)
      ((make-rectangular _ _)	   foldable effect-free result-true)
      ((sin _)			   foldable effect-free result-true)
      ((cos _)			   foldable effect-free result-true)
      ((tan _)			   foldable effect-free result-true)
      ((asin _)			   foldable effect-free result-true)
      ((acos _)			   foldable effect-free result-true)
      ((atan _)			   foldable effect-free result-true)
      ((make-eq-hashtable)		    effect-free result-true)
      ((string->number _)	   foldable effect-free		   )
      ((string->number _ _)	   foldable effect-free		   )
      (($fixnum->flonum _)	   foldable effect-free result-true)
      (($char->fixnum _)	   foldable effect-free result-true)
      (($fixnum->char _)	   foldable effect-free result-true)
      (($fxzero? _)		   foldable effect-free		   )
      (($fx+ _ _)		   foldable effect-free result-true)
      (($fx* _ _)		   foldable effect-free result-true)
      (($fx- _ _)		   foldable effect-free result-true)
      (($fx= _ _)		   foldable effect-free		   )
      (($fx>= _ _)		   foldable effect-free		   )
      (($fx> _ _)		   foldable effect-free		   )
      (($fx<= _ _)		   foldable effect-free		   )
      (($fx< _ _)		   foldable effect-free		   )
      (($car _)			   foldable effect-free		   )
      (($cdr _)			   foldable effect-free		   )
      (($struct-ref _ _)	   foldable effect-free		   )
      (($struct/rtd? _ _)	   foldable effect-free		   )
      (($fxsll _ _)		   foldable effect-free result-true)
      (($fxsra _ _)		   foldable effect-free result-true)
      (($fxlogor _ _)		   foldable effect-free result-true)
      (($fxlogand _ _)		   foldable effect-free result-true)
      (($fxadd1 _)		   foldable effect-free result-true)
      (($fxsub1 _)		   foldable effect-free result-true)
      (($vector-length _)	   foldable effect-free result-true)
      (($vector-ref _ _)	   foldable effect-free result-true)
      ;;$MAKE-BYTEVECTOR  must not  be foldable:  it must  return a  new
      ;;bytevector every time.
      (($make-bytevector 0)		    effect-free result-true)
      (($make-bytevector 0 _)		    effect-free result-true)
      (($make-bytevector . _)		    effect-free result-true)
      (($bytevector-u8-ref _ _)	   foldable effect-free result-true)
      (($bytevector-length _)	   foldable effect-free result-true)

;;; --------------------------------------------------------------------

      ((annotation? #f)		    foldable effect-free result-false)
      ((annotation-stripped #f)	    foldable effect-free result-false)

;;; --------------------------------------------------------------------

      ;;This must return a new flonum every time.
      (($make-flonum . _)		     effect-free result-true)

      ;;This must return a new struct every time.
      (($struct . _)			     effect-free result-true)

      ((condition . _))
      ((top-level-value . _))
      ((make-message-condition . _))
      ((make-lexical-violation . _))
      ((make-who-condition . _))
      ((make-error . _))
      ((make-i/o-error . _))
      ((make-i/o-write-error . _))
      ((make-i/o-read-error . _))
      ((make-i/o-file-already-exists-error . _))
      ((make-i/o-file-is-read-only-error . _))
      ((make-i/o-file-protection-error . _))
      ((make-i/o-file-does-not-exist-error . _))
      ((make-undefined-violation . _))
      ((die . _))
      ((gensym . _))
      ((values . _))
      ((error . _))
      ((assertion-violation . _))
      ;;FIXME Reduce to display.  (Abdulaziz Ghuloum)
      ((printf . _))
      ((newline . _))
      ((native-transcoder . _))
      ((open-string-output-port . _))
      ((open-string-input-port . _))
      ((environment . _))
      ((print-gensym . _))
      ((exit . _))
      ((interrupt-handler . _))
      ((display . _))
      ((write-char . _))
      ((current-input-port . _))
      ((current-output-port . _))
      ((current-error-port . _))
      ((standard-input-port . _))
      ((standard-output-port . _))
      ((standard-error-port . _))

      ;;It appears that something can go  wrong if we do these, not sure
      ;;why.  (Marco Maggi; Nov 3, 2012)
      ;;
      ;;((current-input-port . _)		 effect-free result-true)
      ;;((current-output-port . _)		 effect-free result-true)
      ;;((current-error-port . _)		 effect-free result-true)

      ((standard-input-port . _)		 effect-free result-true)
      ((standard-output-port . _)		 effect-free result-true)
      ((standard-error-port . _)		 effect-free result-true)
      ((console-input-port . _)			 effect-free result-true)
      ((console-output-port . _)		 effect-free result-true)
      ((console-error-port . _)			 effect-free result-true)
      (($current-frame . _))
      ((pretty-width . _))
      (($fp-at-base . _))
      ((get-annotated-datum . _))
      (($collect-key . _))
      ((make-non-continuable-violation . _))

      ;;FIXME Reduce to string-copy (Abdulaziz Ghuloum).
      ((format . _))
      ((uuid . _))
      ((print-graph . _))
      ((interaction-environment . _))
      ((make-guardian)					 effect-free result-true)
      ((command-line-arguments))
      ;;FIXME (Abdulaziz Ghuloum)
      ((make-record-type-descriptor . _))
      ((make-assertion-violation . _))
      ((new-cafe . _))
      ((getenv . _))
      ((gensym-prefix . _))
      (($arg-list . _))
      (($make-symbol . _)				 effect-free result-true)
      ((string->utf8 . _)				 effect-free result-true)
      ((string->utf16be . _)				 effect-free result-true)
      ((string->utf16le . _)				 effect-free result-true)
      ((string->utf16n . _)				 effect-free result-true)
      ((string->keyword . _)				 effect-free result-true)
      ((string->ascii . _)				 effect-free result-true)
      ((string->latin1 . _)				 effect-free result-true)
      (($make-call-with-values-procedure . _))
      (($make-values-procedure . _))
      (($unset-interrupted! . _))
      ((make-interrupted-condition . _))
      (($interrupted? . _))
      (($symbol-value . _))
      ((library-extensions . _))
      ;;The base struct type descriptor is a constant created at process
      ;;boot time.
      ((base-rtd . _))
      (($data->transcoder . _)			foldable effect-free result-true)
      ((current-time . _))
      ))

  ;;This is an initialisation expression outside of any function.
  (%initialise-primitive-properties PRIMITIVE-INFO-LIST)

  #| end of module: primprop |# )


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
  ;;"p", "v",  "e", "app".   The clauses  can appear  in any  order; the
  ;;clauses are optional, a clause for a context might be there or not.
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
  ;;          (if (eq? t 'app)
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


(define (mkseq e0 e1)
  ;;Return  a (seq  e0 e1)  with a  seq-less e1  if both  e0 and  e1 are
  ;;constructed properly.
  ;;
  (if (simple-expression-without-side-effects? e0)
      e1
    (let ((e0 (struct-case e0
		((seq e0a e0b)
		 (if (simple-expression-without-side-effects? e0b)
		     e0a
		   e0))
		(else
		 e0))))
      (struct-case e1
	((seq e1a e1b)
	 (make-seq (make-seq e0 e1a) e1b))
	(else
	 (make-seq e0 e1))))))

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
  ;;Return the "last" value of an expression.
  ;;
  (struct-case x
    ((seq e0 e1)
     e1)
    (else
     x)))

(define (records-equal? x y ctxt)
  (struct-case x
    ((constant kx)
     (struct-case y
       ((constant ky)
	(case-context ctxt
	  ((e)
	   #t)
	  ((p)
	   (if kx ky (not ky)))
	  (else
	   (eq? kx ky))))
       (else
	#f)))
    (else
     #f)))

(define (residualize-operands e rand* sc)
  (cond ((null? rand*)
	 e)
	((not (operand-residualize-for-effect (car rand*)))
	 (residualize-operands e (cdr rand*) sc))
	(else
	 (let ((opnd (car rand*)))
	   (let ((e1 (or (operand-value opnd)
			 (struct-case opnd
			   ((operand expr env ec)
			    (E expr 'e env ec sc))))))
	     (if (simple-expression-without-side-effects? e1)
		 (residualize-operands e (cdr rand*) sc)
	       (begin
		 (decrement sc (operand-size opnd))
		 (mkseq e1 (residualize-operands e (cdr rand*) sc)))))))))

(define (value-visit-operand! rand)
  ;;RAND  must  be  a  struct  instance of  type  OPERAND.   Attempt  to
  ;;precompute the value  of RAND and return it; the  value and its size
  ;;are cached in the fields "value" and "size" of RAND.
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
  ;;decrementing the counter SC.
  ;;
  (begin0
      (value-visit-operand! rand)
    (decrement sc (operand-size rand))))


(module (copy)

  (define (copy x opnd ctxt ec sc)
    (let ((rhs (result-expr (operand-value opnd))))
      (struct-case rhs
	((constant)
	 rhs)
	((prelex)
	 (if (prelex-source-assigned? rhs)
	     (residualize-ref x sc)
	   (let ((opnd (prelex-operand rhs)))
	     (if (and opnd (operand-value opnd))
		 (copy2 rhs opnd ctxt ec sc)
	       (residualize-ref rhs sc)))))
	(else
	 (copy2 x opnd ctxt ec sc)))))

  (define (copy2 x opnd ctxt ec sc)
    (let ((rhs (result-expr (operand-value opnd))))
      (struct-case rhs
	((clambda)
	 (case-context ctxt
	   ((v)
	    (residualize-ref x sc))
	   ((p)
	    (make-constant #t))
	   ((e)
	    (make-constant (void)))
	   ((app)
	    (or (and (not (operand-outer-pending opnd)) ;avoid evaluation cycles?
		     (dynamic-wind
			 (lambda ()
			   (set-operand-outer-pending! opnd #t))
			 (lambda ()
			   (call/cc
			       (lambda (abort)
				 (inline rhs ctxt EMPTY-ENV
					 (if (active-counter? ec)
					     ec
					   (make-counter (cp0-effort-limit) ctxt abort))
					 (make-counter (if (active-counter? sc)
							   (counter-value sc)
							 (cp0-size-limit))
						       ctxt abort)))))
			 (lambda ()
			   (set-operand-outer-pending! opnd #f))))
		(residualize-ref x sc)))))

	((primref p)
	 (case-context ctxt
	   ((v)		rhs)
	   ((p)		(make-constant #t))
	   ((e)		(make-constant (void)))
	   ((app)	(fold-prim p ctxt ec sc))))

	(else
	 (residualize-ref x sc)))))

  #| end of module: copy |# )

(define (residualize-ref x sc)
  (decrement sc 1)
  (set-prelex-residual-referenced?! x #t)
  x)


(module (inline)

  (define (inline proc ctxt env ec sc)
    (struct-case proc
      ((clambda g cases cp free name)
       (let ((rand* (app-rand* ctxt)))
	 (struct-case (get-case cases rand*)
	   ((clambda-case info body)
	    (struct-case info
	      ((case-info label args proper)
	       (if proper
		   (with-extended-env ((env args)
				       (env args rand*))
		     (let ((body^ (E body (app-ctxt ctxt) env ec sc)))
		       (begin0
			   (make-let-binding args rand* body^ sc)
			 (set-app-inlined! ctxt #t))))
		 (let-values (((x* t* r) (%partition args rand*)))
		   (with-extended-env ((env a*)
				       (env (append x* t*) rand*))
		     (let* ((clis (make-funcall (make-primref 'list) t*))
			    (rarg (make-operand clis env ec)))
		       (with-extended-env ((env b*)
					   (env (list r) (list rarg)))
			 (let* ((body^  (E body (app-ctxt ctxt) env ec sc))
				(body^^ (make-let-binding b* (list rarg) body^ sc))
				(result (make-let-binding a* rand* body^^ sc)))
			   (set-app-inlined! ctxt #t)
			   result)))))))))
	   (else
	    (E proc 'v env ec sc)))))))

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

  (define (%partition args rand*)
    (if (null? (cdr args))
	(let* ((r  (car args))
	       (t* (map (lambda (x)
			  (copy-var r))
		     rand*)))
	  (values '() t* r))
      (let ((x (car args)))
	(let-values (((x* t* r) (%partition (cdr args) (cdr rand*))))
	  (values (cons x x*) t* r)))))

  #| end of module: inline |# )


(module (make-let-binding)

  (define (make-let-binding var* rand* body sc)
    (let-values (((lhs* rhs*) (%process var* rand* sc)))
      (if (null? lhs*)
	  body
	(make-bind lhs* rhs* body))))

  (define (%process var* rand* sc)
    (if (null? var*)
	(values '() '())
      (let ((var  (car var*))
	    (rand (car rand*)))
	(let-values (((lhs* rhs*) (%process (cdr var*) (cdr rand*) sc)))
	  (%process1 var rand lhs* rhs* sc)))))

  (define (%process1 var rand lhs* rhs* sc)
    (cond ((prelex-residual-referenced? var)
	   (values (cons var lhs*)
		   (cons (score-value-visit-operand! rand sc)
			 rhs*)))
	  ((prelex-residual-assigned? var)
	   (set-operand-residualize-for-effect! rand #t)
	   (values (cons var lhs*)
		   (cons (make-constant (void))
			 rhs*)))
	  (else
	   (set-operand-residualize-for-effect! rand #t)
	   (values lhs* rhs*))))

  #| end of module: make-let-binding |# )


(module (fold-prim)
  ;;Whenever possible  attempt to precompute  the result of  a primitive
  ;;function  application.   This is  the  place  where the  "foldable",
  ;;"effect-free", "result-true" and "result-false" primitive attributes
  ;;are used.
  ;;
  (define (fold-prim primsym appctxt ec sc)
    ;;PRIMSYM must be a symbol being the name of a primitive function.
    ;;
    ;;APPCTXT must  be a struct  instance of type APP,  representing the
    ;;context of appliction for PRIMSYM.
    ;;
    ;;EC is the effort counter.
    ;;
    ;;SC is the size counter.
    ;;
    (let* ((rand*  (app-rand* appctxt))
	   (info   (%primitive-info primsym rand*))
	   (result (or (and (%info-effect-free? info)
			    (%precompute-effect-free-primitive info appctxt))
		       (and (%info-foldable? info)
			    (%precompute-foldable-primitive primsym rand* ec)))))
      (if result
	  (begin
	    (decrement ec 1)
	    (for-each (lambda (x)
			(set-operand-residualize-for-effect! x #t))
	      rand*)
	    (set-app-inlined! appctxt #t)
	    result)
	(begin
	  (decrement sc 1)
	  (make-primref primsym)))))

  (define (%precompute-effect-free-primitive info appctxt)
    ;;The function  call is effect  free.  If the evaluation  context is
    ;;"for  side effects":  the function  call can  be removed.   If the
    ;;evaluation context is "predicate": check  if the function call has
    ;;known boolean result.
    ;;
    (case-context (app-ctxt appctxt)
      ((e)
       (make-constant (void)))
      ((p)
       (cond ((%info-result-true?  info)	(make-constant #t))
	     ((%info-result-false? info)	(make-constant #f))
	     (else				#f)))
      (else
       #f)))

  (define (%precompute-foldable-primitive primsym rand* ec)
    ;;The function call  is foldable; if all the  operands are constant:
    ;;precompute the function call and  return a struct instance of type
    ;;CONSTANT; else return false.
    ;;
    ;;PRIMSYM must be a symbol being the name of a primitive function.
    ;;
    ;;RAND* must be  a list of struct  instances representing recordized
    ;;code which, when evaluated, will return the arguments.
    ;;
    ;;EC is a counter.
    ;;
    (let ((val* (map (lambda (x)
		       (value-visit-operand! x))
		  rand*)))
      (cond ((andmap constant? val*)
	     (%apply-at-compile-time primsym (map constant-value val*) ec))
	    (else
	     #f))))

  (define (%apply-at-compile-time primsym args ec)
    ;;PRIMSYM must be a symbol being the name of a primitive function.
    ;;
    ;;ARGS must be a list  of constant values representing the arguments
    ;;of a call to P.
    ;;
    ;;EC is a counter.
    ;;
    ;;Apply the primitive  associated to P to the list  of arguments; if
    ;;successful: return a struct instance  of type CONSTANT holding the
    ;;result; else return false.
    ;;
    ;;See  the  documentation  of   the  function  SYSTEM-VALUE  for  an
    ;;explanation of why we can extract the function from PRIMSYM.
    ;;
    (call/cc
	(lambda (k)
	  (with-exception-handler
	      (lambda (con)
		(decrement ec 10)
		(k #f))
	    (lambda ()
	      (make-constant (apply (system-value primsym) args)))))))

;;; --------------------------------------------------------------------

  (define (%primitive-info primsym rand*)
    ;;PRIMSYM  must be  a symbol  representing the  name of  a primitive
    ;;function.  RAND* must be null  or a list representing the arguments
    ;;in a function call to PRIMSYM.
    ;;
    ;;This function scans  the attributes list of  PRIMSYM searching for
    ;;entries that match the arguments  call represented by RAND*.  If a
    ;;match  is  found:  return  a  list  of  symbols  representing  the
    ;;attributes; else return nil.
    ;;
    (define who '%primitive-info)
    (define (%matches? attributes-sublist)
      (let loop ((rand*           rand*)
		 (template-params (car attributes-sublist)))
	(cond ((pair? template-params)
	       (and (pair? rand*)
		    (let ((template-arg (car template-params)))
		      (case template-arg
			((_)
			 ;;An argument matched.  Go on with the rest.
			 (loop (cdr rand*) (cdr template-params)))
			((#f 0 ())
			 ;;The  template  specifies that  this  argument
			 ;;must be one  among: #f, 0, nil; if  it is: go
			 ;;on with the rest; else this template does not
			 ;;match.
			 (let ((v (value-visit-operand! (car rand*))))
			   (and (constant? v)
				(equal? template-arg (constant-value v))
				(loop (cdr rand*) (cdr template-params)))))
			(else
			 ;;Invalid template specification.
			 (error who "internal error" primsym (car template-params)))))))
	      ((eq? template-params '_)
	       ;;Success!  The template represents  a call accepting any
	       ;;number  of  arguments  (possibly after  some  mandatory
	       ;;arguments).
	       #t)
	      ((null? template-params)
	       ;;If  RAND* is  null: success,  the template  matches the
	       ;;arguments!  Else this template does not match.
	       (null? rand*))
	      (else
	       (error who "internal error" primsym template-params)))))
    (or (find %matches? (primprop primsym))
	'()))

;;; --------------------------------------------------------------------

  (define-inline (%info-foldable? info)
    (memq 'foldable info))

  (define-inline (%info-effect-free? info)
    (memq 'effect-free info))

  (define-inline (%info-result-true? info)
    (memq 'result-true info))

  (define-inline (%info-result-false? info)
    (memq 'result-false info))

  #| end of module: fold-prim |# )


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'case-context 'scheme-indent-function 1)
;; eval: (put 'with-extended-env 'scheme-indent-function 1)
;; End:

