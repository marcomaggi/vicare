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
      ;;It is determined that trying to comment out the clause for level
      ;;2  makes the  process crash!!!   Commenting out  the clause  for
      ;;level 1 does not crash.  (Marco Maggi, Mon Jun 7, 2010)
      ;;
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

(define-structure (app rand* ctxt)
  ((inlined			#f)))

(define-structure (operand expr env ec)
  ((value			#f)
   (residualize-for-effect	#f)
   (size			0)
   (inner-pending		#f)
   (outer-pending		#f)))

(define-struct counter
  (value ctxt k))


(define (passive-counter)
  (make-counter (greatest-fixnum) #f (lambda args
				       (error 'passive-counter "invalid abort"))))

(define (passive-counter-value x)
  (- (greatest-fixnum)
     (counter-value x)))

(define (active-counter? x)
  (and (counter? x)
       (counter-ctxt x)))

(define (decrement x amt)
  (let ((n (- (counter-value x) amt)))
    (set-counter-value! x n)
    (when (< n 0)
      (reset-integrated! (counter-ctxt x))
      ((counter-k x) #f))))

(define (abort-counter! x)
  (reset-integrated! (counter-ctxt x))
  ((counter-k x) #f))

(define (reset-integrated! ctxt)
  (set-app-inlined! ctxt #f)
  (let ((ctxt (app-ctxt ctxt)))
    (when (app? ctxt)
      (reset-integrated! ctxt))))


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

  (define-constant UNIQUE-PROPERTY-KEY
    (let-syntax
	((expand-time-gensym (lambda (x)
			       (with-syntax ((SYM (datum->syntax #'here (gensym))))
				 #'(quote SYM)))))
      (expand-time-gensym)))

  (define (primprop p)
    (or (getprop p UNIQUE-PROPERTY-KEY) '()))

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
    '(((cons _ _)                           effect-free result-true)
      ((cons* _)                   foldable effect-free            )
      ((cons* _ . _)                        effect-free result-true)
      ((list)                      foldable effect-free result-true)
      ((list . _)                           effect-free result-true)
      ((reverse ())                foldable effect-free result-true)
      ((string)                    foldable effect-free result-true)
      ((string . _)                                     result-true)
      ((make-string 0)             foldable effect-free result-true)
      ((make-string 0 _)           foldable effect-free result-true)
      ((make-string . _)                                result-true)
      ((make-bytevector 0)         foldable effect-free result-true)
      ((make-bytevector 0 _)       foldable             result-true)
      ((make-bytevector . _)                            result-true)
      ((string-length _)           foldable             result-true)
      ((string-ref _ _)            foldable             result-true)
      ((vector)                    foldable effect-free result-true)
      ((vector . _)                         effect-free result-true)
      ((make-vector 0)             foldable effect-free result-true)
      ((make-vector 0 _)           foldable effect-free result-true)
      ((make-vector . _)                                result-true)
      ((vector-length _)           foldable             result-true)
      ((vector-ref _ _)            foldable                        )
      ((eq? _ _)                   foldable effect-free            )
      ((eqv? _ _)                  foldable effect-free            )
      ((assq _ _)                  foldable                        )
      ((assv _ _)                  foldable                        )
      ((assoc _ _)                 foldable                        )
      ((not _)                     foldable effect-free            )
      ((null? _)                   foldable effect-free            )
      ((pair? _)                   foldable effect-free            )
      ((fixnum? _)                 foldable effect-free            )
      ((vector? _)                 foldable effect-free            )
      ((string? _)                 foldable effect-free            )
      ((char? _)                   foldable effect-free            )
      ((symbol? _)                 foldable effect-free            )
      ((procedure? _)              foldable effect-free            )
      ((eof-object? _)             foldable effect-free            )
      ((flonum? _)                 foldable effect-free            )
      ((cflonum? _)                foldable effect-free            )
      ((compnum? _)                foldable effect-free            )
      ((integer? _)                foldable effect-free            )
      ((bignum? _)                 foldable effect-free            )
      ((ratnum? _)                 foldable effect-free            )
      ((pointer? _)                foldable effect-free            )
      ((void)                      foldable effect-free result-true)
      ((car _)                     foldable                        )
      ((cdr _)                     foldable                        )
      ((caar _)                    foldable                        )
      ((cadr _)                    foldable                        )
      ((cdar _)                    foldable                        )
      ((cddr _)                    foldable                        )
      ((caaar _)                   foldable                        )
      ((caadr _)                   foldable                        )
      ((cadar _)                   foldable                        )
      ((caddr _)                   foldable                        )
      ((cdaar _)                   foldable                        )
      ((cdadr _)                   foldable                        )
      ((cddar _)                   foldable                        )
      ((cdddr _)                   foldable                        )
      ((caaaar _)                  foldable                        )
      ((caaadr _)                  foldable                        )
      ((caadar _)                  foldable                        )
      ((caaddr _)                  foldable                        )
      ((cadaar _)                  foldable                        )
      ((cadadr _)                  foldable                        )
      ((caddar _)                  foldable                        )
      ((cadddr _)                  foldable                        )
      ((cdaaar _)                  foldable                        )
      ((cdaadr _)                  foldable                        )
      ((cdadar _)                  foldable                        )
      ((cdaddr _)                  foldable                        )
      ((cddaar _)                  foldable                        )
      ((cddadr _)                  foldable                        )
      ((cdddar _)                  foldable                        )
      ((cddddr _)                  foldable                        )
      ((memq _ _)                  foldable                        )
      ((memv _ _)                  foldable                        )
      ((length _)                  foldable             result-true)
      ((+ . _)                     foldable             result-true)
      ((* . _)                     foldable             result-true)
      ((/ _ . _)                   foldable             result-true)
      ((- _ . _)                   foldable             result-true)
      ((fx+ _ _)                   foldable             result-true)
      ((fx- _ _)                   foldable             result-true)
      ((fx* _ _)                   foldable             result-true)
      ((fxior . _)                 foldable             result-true)
      ((fxlogor . _)               foldable             result-true)
      ((fxnot _)                   foldable             result-true)
      ((fxadd1 _)                  foldable             result-true)
      ((fxsub1 _)                  foldable             result-true)
      ((fxzero? _)                 foldable                        )
      ((fx=? _ . _)                foldable                        )
      ((fx<? _ . _)                foldable                        )
      ((fx<=? _ . _)               foldable                        )
      ((fx>? _ . _)                foldable                        )
      ((fx>=? _ . _)               foldable                        )
      ((fx= _ . _)                 foldable                        )
      ((fx< _ . _)                 foldable                        )
      ((fx<= _ . _)                foldable                        )
      ((fx> _ . _)                 foldable                        )
      ((fx>= _ . _)                foldable                        )
      ((real-part _)               foldable             result-true)
      ((imag-part _)               foldable             result-true)
      ((fxsll _ _)                 foldable             result-true)
      ((fxsra _ _)                 foldable             result-true)
      ((fxremainder _ _)           foldable             result-true)
      ((fxquotient _ _)            foldable             result-true)
      ((greatest-fixnum)           foldable effect-free result-true)
      ((least-fixnum)              foldable effect-free result-true)
      ((fixnum-width)              foldable effect-free result-true)
      ((char->integer _)           foldable             result-true)
      ((integer->char _)           foldable             result-true)
      ((eof-object)                foldable effect-free result-true)
      ((zero? _)                   foldable                        )
      ((= _ . _)                   foldable                        )
      ((< _ . _)                   foldable                        )
      ((<= _ . _)                  foldable                        )
      ((> _ . _)                   foldable                        )
      ((>= _ . _)                  foldable                        )
      ((expt _ _)                  foldable             result-true)
      ((log _)                     foldable             result-true)
      ((sll _ _)                   foldable             result-true)
      ((sra _ _)                   foldable             result-true)
      ((inexact _)                 foldable             result-true)
      ((exact _)                   foldable             result-true)
      ((add1 _)                    foldable             result-true)
      ((sub1 _)                    foldable             result-true)
      ((bitwise-and _ _)           foldable             result-true)
      ((make-rectangular _ _)      foldable             result-true)
      ((sin _)                     foldable             result-true)
      ((cos _)                     foldable             result-true)
      ((tan _)                     foldable             result-true)
      ((asin _)                    foldable             result-true)
      ((acos _)                    foldable             result-true)
      ((atan _)                    foldable             result-true)
      ((make-eq-hashtable)                  effect-free result-true)
      ((string->number _)          foldable                        )
      ((string->number _ _)        foldable                        )
      (($fixnum->flonum _)         foldable effect-free result-true)
      (($char->fixnum _)           foldable effect-free result-true)
      (($fixnum->char _)           foldable effect-free result-true)
      (($fxzero? _)                foldable effect-free            )
      (($fx+ _ _)                  foldable effect-free result-true)
      (($fx* _ _)                  foldable effect-free result-true)
      (($fx- _ _)                  foldable effect-free result-true)
      (($fx= _ _)                  foldable effect-free            )
      (($fx>= _ _)                 foldable effect-free            )
      (($fx> _ _)                  foldable effect-free            )
      (($fx<= _ _)                 foldable effect-free            )
      (($fx< _ _)                  foldable effect-free            )
      (($car _)                    foldable effect-free            )
      (($cdr _)                    foldable effect-free            )
      (($struct-ref _ _)           foldable effect-free            )
      (($struct/rtd? _ _)          foldable effect-free            )
      (($fxsll _ _)                foldable effect-free result-true)
      (($fxsra _ _)                foldable effect-free result-true)
      (($fxlogor _ _)              foldable effect-free result-true)
      (($fxlogand _ _)             foldable effect-free result-true)
      (($fxadd1 _)                 foldable effect-free result-true)
      (($fxsub1 _)                 foldable effect-free result-true)
      (($vector-length _)          foldable effect-free result-true)
      (($vector-ref _ _)           foldable effect-free result-true)
      (($make-bytevector 0)        foldable effect-free result-true)
      (($make-bytevector 0 _)      foldable effect-free result-true)
      (($make-bytevector . _)               effect-free result-true)
      (($bytevector-u8-ref _ _)    foldable effect-free result-true)
      (($bytevector-length _)      foldable effect-free result-true)

;;; --------------------------------------------------------------------

      ((annotation? #f)             foldable effect-free result-false)
      ((annotation-stripped #f)     foldable effect-free result-false)

;;; --------------------------------------------------------------------

      ((condition . _))
      (($make-flonum . _))
      ((top-level-value . _))
      (($struct . _))
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
      ((console-input-port . _))
      ((console-output-port . _))
      ((console-error-port . _))
      ((printf . _)) ;;; FIXME: reduce to display (Abdulaziz Ghuloum)
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
      (($current-frame . _))
      ((pretty-width . _))
      (($fp-at-base . _))
      ((get-annotated-datum . _))
      (($collect-key . _))
      ((make-non-continuable-violation . _))
      ((format . _)) ;;; FIXME, reduce to string-copy (Abdulaziz Ghuloum)
      ((uuid . _))
      ((print-graph . _))
      ((interaction-environment . _))
      ((make-guardian))
      ((command-line-arguments))
      ((make-record-type-descriptor . _)) ;;FIXME (Abdulaziz Ghuloum)
      ((make-assertion-violation . _))
      ((new-cafe . _))
      ((getenv . _))
      ((gensym-prefix . _))
      (($arg-list . _))
      (($make-symbol . _))
      ((string->utf8 . _))
      (($make-call-with-values-procedure . _))
      (($make-values-procedure . _))
      (($unset-interrupted! . _))
      ((make-interrupted-condition . _))
      (($interrupted? . _))
      (($symbol-value . _))
      ((library-extensions . _))
      ((base-rtd . _))
      (($data->transcoder . _))
      ((current-time . _))
      ))

  ;;This is an initialisation expression outside of any function.
  (%initialise-primitive-properties PRIMITIVE-INFO-LIST)

  #| end of module: primprop |# )


(define (primitive-info op args)
  ;;OP must be  a symbol representing the name of  a primitive function.
  ;;ARGS must be null or a list representing the arguments in a function
  ;;call to OP.
  ;;
  ;;This function scans the attributes  list of OP searching for entries
  ;;that match  the arguments call represented  by ARGS.  If a  match is
  ;;found: return  a list of  symbols representing the  attributes; else
  ;;return nil.
  ;;
  (define who 'primitive-info)
  (define (%matches? attributes-sublist)
    (let loop ((args            args)
	       (template-params (car attributes-sublist)))
      (cond ((pair? template-params)
	     (and (pair? args)
		  (let ((template-arg (car template-params)))
		    (case template-arg
		      ((_)
		       ;;An argument matched.  Go on with the rest.
		       (loop (cdr args) (cdr template-params)))
		      ((#f 0 ())
		       ;;The template specifies  that this argument must
		       ;;be one among: #f, 0, nil;  if it is: go on with
		       ;;the rest; else this template does not match.
		       (let ((v (value-visit-operand! (car args))))
			 (and (constant? v)
			      (equal? template-arg (constant-value v))
			      (loop (cdr args) (cdr template-params)))))
		      (else
		       ;;Invalid template specification.
		       (error who "internal error" op (car template-params)))))))
	    ((eq? template-params '_)
	     ;;Success!  The  template represents  a call  accepting any
	     ;;number  of  arguments   (possibly  after  some  mandatory
	     ;;arguments).
	     #t)
	    ((null? template-params)
	     ;;If ARGS is null: success:  the template matches the args!
	     ;;Else this template does not match.
	     (null? args))
	    (else
	     (error who "internal error" op template-params)))))
  (or (find %matches? (primprop op))
      '()))

(define-inline (info-foldable? info)
  (memq 'foldable info))

(define-inline (info-effect-free? info)
  (memq 'effect-free info))

(define-inline (info-result-true? info)
  (memq 'result-true info))

(define-inline (info-result-false? info)
  (memq 'result-false info))


(define-syntax ctxt-case
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

	(((else e e* ...))
	 #'(begin e e* ...))

	((((t* ...) e e* ...) rest ...)
	 (with-syntax (((t* ...) (map %test #'(t* ...)))
		       (body     (%extract #'(rest ...))))
	   #'(if (or t* ...)
		 (begin e e* ...)
	       body)))
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
  (if (simple? e0)
      e1
    (let ((e0 (struct-case e0
		((seq e0a e0b)
		 (if (simple? e0b)
		     e0a
		   e0))
		(else
		 e0))))
      (struct-case e1
	((seq e1a e1b)
	 (make-seq (make-seq e0 e1a) e1b))
	(else
	 (make-seq e0 e1))))))

(define (simple? x)
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
	(ctxt-case ctxt
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
	     (if (simple? e1)
		 (residualize-operands e (cdr rand*) sc)
	       (begin
		 (decrement sc (operand-size opnd))
		 (mkseq e1 (residualize-operands e (cdr rand*) sc)))))))))

(define (value-visit-operand! rand)
  (or (operand-value rand)
      (let* ((sc (passive-counter))
	     (e  (struct-case rand
		   ((operand expr env ec)
		    (E expr 'v env sc ec)))))
	(set-operand-value! rand e)
	(set-operand-size!  rand (passive-counter-value sc))
	e)))

(define (score-value-visit-operand! rand sc)
  (begin0
      (value-visit-operand! rand)
    (decrement sc (operand-size rand))))


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
  (ctxt-case ctxt
    ((e)
     (make-constant (void)))
    (else
     (let* ((x    (lookup x env))
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
	 (ctxt-case ctxt
	   ((v)
	    (residualize-ref x sc))
	   ((p)
	    (make-constant #t))
	   ((e)
	    (make-constant (void)))
	   ((app)
	    (or (and (not (operand-outer-pending opnd))
		     (dynamic-wind
			 (lambda ()
			   (set-operand-outer-pending! opnd #t))
			 (lambda ()
			   (call/cc
			       (lambda (abort)
				 (inline rhs ctxt EMPTY-ENV
					 (if (active-counter? ec)
					     ec
					   (make-counter
					    (cp0-effort-limit)
					    ctxt abort))
					 (make-counter
					  (if (active-counter? sc)
					      (counter-value sc)
					    (cp0-size-limit))
					  ctxt abort)))))
			 (lambda ()
			   (set-operand-outer-pending! opnd #f))))
		(residualize-ref x sc)))))

	((primref p)
	 (ctxt-case ctxt
	   ((v)		rhs)
	   ((p)		(make-constant #t))
	   ((e)		(make-constant (void)))
	   ((app)	(fold-prim p ctxt ec sc))))

	(else
	 (residualize-ref x sc)))))

  #| end of module: copy |# )


(module (inline)

  (define (inline proc ctxt env ec sc)
    (struct-case proc
      ((clambda g cases cp free name)
       (let ((rand* (app-rand* ctxt)))
	 (struct-case (get-case cases rand*)
	   ((clambda-case info body)
	    (struct-case info
	      ((case-info label args proper)
	       (cond (proper
		      (with-extended-env ((env args)
					  (env args rand*))
			(let ((body (E body (app-ctxt ctxt) env ec sc)))
			  (let ((result (make-let-binding args rand* body sc)))
			    (set-app-inlined! ctxt #t)
			    result))))
		     (else
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
				result))))))))))
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


(define (do-bind lhs* rhs* body ctxt env ec sc)
  (let ((rand* (map (lambda (x) (make-operand x env ec)) rhs*)))
    (with-extended-env ((env lhs*) (env lhs* rand*))
		       (residualize-operands
			(make-let-binding lhs* rand*
					  (E body ctxt env ec sc)
					  sc)
			rand* sc))))
  ;;;
(define (make-let-binding var* rand* body sc)
  (define (process1 var rand lhs* rhs*)
    (cond
     ((prelex-residual-referenced? var)
      (values
       (cons var lhs*)
       (cons (score-value-visit-operand! rand sc) rhs*)))
     ((prelex-residual-assigned? var)
      (set-operand-residualize-for-effect! rand #t)
      (values
       (cons var lhs*)
       (cons (make-constant (void)) rhs*)))
     (else
      (set-operand-residualize-for-effect! rand #t)
      (values lhs* rhs*))))
  (define (process var* rand*)
    (cond
     ((null? var*) (values '() '()))
     (else
      (let ((var (car var*)) (rand (car rand*)))
	(let-values (((lhs* rhs*) (process (cdr var*) (cdr rand*))))
	  (process1 var rand lhs* rhs*))))))
  (let-values (((lhs* rhs*) (process var* rand*)))
    (if (null? lhs*) body (make-bind lhs* rhs* body))))
  ;;;
(define (fold-prim p ctxt ec sc)
  (define (get-value p ls)
    (call/cc
        (lambda (k)
          (with-exception-handler
	      (lambda (con)
		(decrement ec 10)
		(k #f))
            (lambda ()
              (make-constant (apply (system-value p) ls)))))))
  (let ((rand* (app-rand* ctxt)))
    (let ((info (primitive-info p rand*)))
      (let ((result
	     (or (and (info-effect-free? info)
		      (ctxt-case (app-ctxt ctxt)
				 ((e) (make-constant (void)))
				 ((p)
				  (cond
				   ((info-result-true? info)
				    (make-constant #t))
				   ((info-result-false? info)
				    (make-constant #f))
				   (else #f)))
				 (else #f)))
		 (and (info-foldable? info)
		      (let ((val*
			     (map (lambda (x) (value-visit-operand! x)) rand*)))
			(cond
			 ((andmap constant? val*)
			  (get-value p (map constant-value val*)))
			 (else #f)))))))
	(if result
	    (begin
	      (decrement ec 1)
	      (for-each
                  (lambda (x)
                    (set-operand-residualize-for-effect! x #t))
		rand*)
	      (set-app-inlined! ctxt #t)
	      result)
	  (begin
	    (decrement sc 1)
	    (make-primref p)))))))
  ;;;
(define (residualize-ref x sc)
  (decrement sc 1)
  (set-prelex-residual-referenced?! x #t)
  x)
  ;;;
(define (build-conditional e0 e1 e2)
  (or (struct-case e0
	((funcall rator rand*)
	 (struct-case rator
	   ((primref op)
	    (and (eq? op 'not)
		 (= (length rand*) 1)
		 (build-conditional (car rand*) e2 e1)))
	   (else #f)))
	(else #f))
      (make-conditional e0 e1 e2)))

(define (E x ctxt env ec sc)
  (decrement ec 1)
  (struct-case x
    ((constant) (decrement sc 1) x)
    ((prelex) (E-var x ctxt env ec sc))
    ((seq e0 e1)
     (mkseq (E e0 'e env ec sc) (E e1 ctxt env ec sc)))
    ((conditional e0 e1 e2)
     (let ((e0 (E e0 'p env ec sc)))
       (struct-case (result-expr e0)
	 ((constant k)
	  (mkseq e0 (E (if k e1 e2) ctxt env ec sc)))
	 (else
	  (let ((ctxt (ctxt-case ctxt ((app) 'v) (else ctxt))))
	    (let ((e1 (E e1 ctxt env ec sc))
		  (e2 (E e2 ctxt env ec sc)))
	      (if (records-equal? e1 e2 ctxt)
		  (mkseq e0 e1)
		(begin
		  (decrement sc 1)
		  (build-conditional e0 e1 e2)))))))))
    ((assign x v)
     (mkseq
      (let ((x (lookup x env)))
	(cond
	 ((not (prelex-source-referenced? x))
              ;;; dead on arrival
	  (E v 'e env ec sc))
	 (else
	  (decrement sc 1)
	  (set-prelex-residual-assigned?! x
					  (prelex-source-assigned? x))
	  (make-assign x (E v 'v env ec sc)))))
      (make-constant (void))))
    ((funcall rator rand*)
     (E-call rator
	     (map (lambda (x) (make-operand x env ec)) rand*)
	     env ctxt ec sc))
    ((forcall name rand*)
     (decrement sc 1)
     (make-forcall name (map (lambda (x) (E x 'v env ec sc)) rand*)))
    ((primref name)
     (ctxt-case ctxt
		((app)
		 (case name
		   ((debug-call) (E-debug-call ctxt ec sc))
		   (else (fold-prim name ctxt ec sc))))
		((v) (decrement sc 1) x)
		(else (make-constant #t))))
    ((clambda g cases cp free name)
     (ctxt-case ctxt
		((app) (inline x ctxt env ec sc))
		((p e) (make-constant #t))
		(else
		 (decrement sc 2)
		 (make-clambda (gensym)
			       (map
				   (lambda (x)
				     (struct-case x
				       ((clambda-case info body)
					(struct-case info
					  ((case-info label args proper)
					   (with-extended-env ((env args) (env args #f))
							      (make-clambda-case
							       (make-case-info (gensym) args proper)
							       (E body 'v env ec sc))))))))
				 cases)
			       cp free name))))
    ((bind lhs* rhs* body)
     (do-bind lhs* rhs* body ctxt env ec sc))
    ((fix lhs* rhs* body)
     (with-extended-env ((env lhs*) (env lhs* #f))
			(for-each
			    (lambda (lhs rhs)
			      (set-prelex-operand! lhs (make-operand rhs env ec)))
			  lhs* rhs*)
			(let ((body (E body ctxt env ec sc)))
			  (let ((lhs* (remp
					  (lambda (x)
					    (not (prelex-residual-referenced? x)))
					lhs*)))
			    (cond
			     ((null? lhs*) body)
			     (else
			      (decrement sc 1)
			      (make-fix lhs*
				  (map (lambda (x)
					 (let ((opnd (prelex-operand x)))
					   (decrement sc (+ (operand-size opnd) 1))
					   (value-visit-operand! opnd)))
				    lhs*)
				body)))))))
    (else
     (error who "invalid expression" x))))

(define (lookup x env)
  (cond
   ((vector? env)
    (let f ((lhs* (vector-ref env 0)) (rhs* (vector-ref env 1)))
      (cond
       ((null? lhs*) (lookup x (vector-ref env 2)))
       ((eq? x (car lhs*)) (car rhs*))
       (else (f (cdr lhs*) (cdr rhs*))))))
   (else x)))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'ctxt-case 'scheme-indent-function 1)
;; eval: (put 'with-extended-env 'scheme-indent-function 1)
;; End:

