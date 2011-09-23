;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;(module primops (primop? cogen-primop)
;  (define (primop? x) #f)
;  (define cogen-primop (lambda args (error 'cogen-primop "not yet"))))
;
;#!eof

;(define-syntax export-all-module
;  (syntax-rules (define)
;    ((_ M (define name* v*) ...)
;     (module M (name* ...)
;       (define name* v*) ...))))
;
;(export-all-module object-representation
;  (define fixnum-scale 4)
;  (define fixnum-shift 2)
;  (define fixnum-tag 0)
;  (define fixnum-mask 3))

(module primops (primop? get-primop set-primop!)
  (define cookie (gensym))
  (define (primop? x)
    (and (getprop x cookie) #t))
  (define (get-primop x)
    (or (getprop x cookie)
        (error 'getprimop "not a primitive" x)))
  (define (set-primop! x v)
    (putprop x cookie v))
  )

(module (specify-representation)
  (import primops)
  (define-struct PH
    (interruptable? p-handler p-handled? v-handler v-handled? e-handler e-handled?))
  (define interrupt-handler
    (make-parameter (lambda () (error 'interrupt-handler "uninitialized"))))
  (define (interrupt)
    ((interrupt-handler))
    (prm 'interrupt))
  (define (with-interrupt-handler p x ctxt args
             make-interrupt-call make-no-interrupt-call
             k)
    (cond
      ((not (PH-interruptable? p))
       (parameterize ((interrupt-handler
                       (lambda ()
                         (error 'cogen "uninterruptable"
                                x args ctxt))))
          (k)))
      (else
       (let ((interrupted? #f))
         (let ((body
                (parameterize ((interrupt-handler
                                (lambda () (set! interrupted? #t))))
                   (k))))
           (cond
             ((not interrupted?) body)
             ((eq? ctxt 'V)
              (let ((h (make-interrupt-call x args)))
                (if (struct-case body
                      ((primcall op) (eq? op 'interrupt))
                      (else #f))
                     (make-no-interrupt-call x args)
                     (make-shortcut body h))))
             ((eq? ctxt 'E)
              (let ((h (make-interrupt-call x args)))
                (if (struct-case body
                      ((primcall op) (eq? op 'interrupt))
                      (else #f))
                     (make-no-interrupt-call x args)
                     (make-shortcut body h))))
             ((eq? ctxt 'P)
              (let ((h (prm '!= (make-interrupt-call x args) (K bool-f))))
                (if (struct-case body
                      ((primcall op) (eq? op 'interrupt))
                      (else #f))
                     (prm '!= (make-no-interrupt-call x args) (K bool-f))
                     (make-shortcut body h))))
             (else (error 'with-interrupt-handler "invalid context" ctxt))))))))
  (define (copy-tag orig new)
    (struct-case orig
      ((known _ t) (make-known new t))
      (else new)))
  (define (remove-tag x)
    (struct-case x
      ((known expr t) expr)
      (else x)))
  (define-syntax with-tmp
    (lambda (x)
      (syntax-case x ()
        ((_ ((lhs* rhs*) ...) b b* ...)
         (with-syntax (((n* ...) (generate-temporaries #'(lhs* ...))))
           #'(let ((lhs* rhs*) ...)
               (let ((n* (unique-var 'lhs*)) ...)
                 (make-bind (list n* ...) (list lhs* ...)
                    (let ((lhs* (copy-tag lhs* n*)) ...)
                      (seq* b b* ...))))))))))
  ;;; if ctxt is V:
  ;;;   if cogen-value, then V
  ;;;   if cogen-pred, then (if P #f #t)
  ;;;   if cogen-effect, then (seq E (void))
  ;;;
  ;;; if ctxt is P:
  ;;;   if cogen-pred, then P
  ;;;   if cogen-value, then (!= V #f)
  ;;;   if cogen-effect, then (seq E #t)
  ;;;
  ;;; if ctxt is E:
  ;;;   if cogen-effect, then E
  ;;;   if cogen-value, then (let ((tmp V)) (nop))
  ;;;   if cogen-pred, then (if P (nop) (nop))
  (define (simplify* args k)
    (define (S* ls)
      (cond
        ((null? ls) (values '() '() '()))
        (else
         (let-values (((lhs* rhs* arg*) (S* (cdr ls))))
           (let ((a (car ls)))
             (struct-case a
               ((known expr type)
                (struct-case expr
                  ((constant i)
                   ;;; erase known tag
                   (values lhs* rhs* (cons expr arg*)))
                  (else
                   ;(printf "known ~s ~s\n" type expr)
                   (let ((tmp (unique-var 'tmp)))
                     (values (cons tmp lhs*)
                             (cons (V expr) rhs*)
                             (cons (make-known tmp type) arg*))))))
               ((constant i)
                (values lhs* rhs* (cons a arg*)))
               (else
                (let ((t (unique-var 'tmp)))
                  (values (cons t lhs*) (cons (V a) rhs*) (cons t arg*))))))))))
    (let-values (((lhs* rhs* args) (S* args)))
      (cond
        ((null? lhs*) (k args))
        (else
         (make-bind lhs* rhs* (k args))))))
  ;;;
  (define (make-cogen-handler make-interrupt-call make-no-interrupt-call)
    (define (cogen-primop x ctxt args)
      (define (interrupt? x)
        (struct-case x
          ((primcall x) (eq? x 'interrupt))
          (else #f)))
      (let ((p (get-primop x)))
         (simplify* args
           (lambda (args)
             (with-interrupt-handler p x ctxt (map T args)
               make-interrupt-call make-no-interrupt-call
               (lambda ()
                 (case ctxt
                   ((P)
                    (cond
                      ((PH-p-handled? p)
                       (apply (PH-p-handler p) args))
                      ((PH-v-handled? p)
                       (let ((e (apply (PH-v-handler p) args)))
                         (if (interrupt? e) e (prm '!= e (K bool-f)))))
                      ((PH-e-handled? p)
                       (let ((e (apply (PH-e-handler p) args)))
                         (if (interrupt? e) e (make-seq e (K #t)))))
                      (else (error 'cogen-primop "not handled" x))))
                   ((V)
                    (cond
                      ((PH-v-handled? p)
                       (apply (PH-v-handler p) args))
                      ((PH-p-handled? p)
                       (let ((e (apply (PH-p-handler p) args)))
                         (if (interrupt? e)
                             e
                             (make-conditional e (K bool-t) (K bool-f)))))
                      ((PH-e-handled? p)
                       (let ((e (apply (PH-e-handler p) args)))
                         (if (interrupt? e) e (make-seq e (K void-object)))))
                      (else (error 'cogen-primop "not handled" x))))
                   ((E)
                    (cond
                      ((PH-e-handled? p)
                       (apply (PH-e-handler p) args))
                      ((PH-p-handled? p)
                       (let ((e (apply (PH-p-handler p) args)))
                         (if (interrupt? e)
                             e
                             (make-conditional e (prm 'nop) (prm 'nop)))))
                      ((PH-v-handled? p)
                       (let ((e (apply (PH-v-handler p) args)))
                         (if (interrupt? e)
                             e
                             (with-tmp ((t e)) (prm 'nop)))))
                      (else (error 'cogen-primop "not handled" x))))
                   (else
                    (error 'cogen-primop "invalid context" ctxt)))))))))
    cogen-primop)
  (module (cogen-primop cogen-debug-primop)
    (define (primop-interrupt-handler x)
      (case x
        ((fx+)                      'error@fx+)
        ((fx-)                      'error@fx-)
        ((fx*)                      'error@fx*)
        ((add1)                     'error@add1)
        ((sub1)                     'error@sub1)
        ((fxadd1)                   'error@fxadd1)
        ((fxsub1)                   'error@fxsub1)
        ((fxarithmetic-shift-left)  'error@fxarithmetic-shift-left)
        ((fxarithmetic-shift-right) 'error@fxarithmetic-shift-right)
        (else                      x)))
    (define (make-interrupt-call op args)
      (make-funcall
        (V (make-primref (primop-interrupt-handler op)))
        args))
    (define (make-no-interrupt-call op args)
      (make-funcall (V (make-primref op)) args))
    (define cogen-primop
      (make-cogen-handler make-interrupt-call make-no-interrupt-call))
    (define (cogen-debug-primop op src/loc ctxt args)
      (define (make-call op args)
        (make-funcall
          (V (make-primref 'debug-call))
          (cons* (V src/loc) (V (make-primref op)) args)))
      ((make-cogen-handler make-call make-call)
       op ctxt args)))


  (define-syntax define-primop
    ;;Transform a declaration like:
    ;;
    ;;  (define-primop $vector-length unsafe
    ;;    ((P x) body-P)	;when used as "conditional test expression"
    ;;    ((E x) body-E)	;when used as "for side-effects expression"
    ;;    ((V x) body-V))	;when used as "for return value expression"
    ;;
    ;;into:
    ;;
    ;;  (begin
    ;;    (define cogen-$vector-length-pred
    ;;      (case-lambda
    ;;       ((x)	body-P)
    ;;       (args	(interrupt))))
    ;;
    ;;    (define cogen-$vector-length-effect
    ;;      (case-lambda
    ;;       ((x)	body-E))
    ;;       (args	(interrupt))))
    ;;
    ;;    (define cogen-$vector-length-value
    ;;      (case-lambda
    ;;       ((x)	body-V)
    ;;       (args	(interrupt))))
    ;;
    ;;    (module ()
    ;;      (set-primop! '$vector-length
    ;;                   (make-PH #f
    ;;                     cogen-$vector-length-pred    #t
    ;;                     cogen-$vector-length-value   #t
    ;;                     cogen-$vector-length-effect  #t))))
    ;;
    ;;The P,  V and  E clauses  are optional and  there can  be multiple
    ;;clauses for each type: they are like SYNTAX-CASE branches.
    ;;
    (lambda (x)
      (define (%cogen-name stx name suffix)
        (datum->syntax stx (string->symbol (format "cogen-~a-~a" suffix (syntax->datum name)))))
      (define (%generate-handler name ctxt case*)
        (define (%filter-cases case*)
	  ;;Extract  from  CASE*  the  cases  matching  CTXT  among  the
	  ;;possible P, V, E.
	  ;;
          (syntax-case case* ()
            (() '())
            ((((?PVE . ?arg*) ?b ?b* ...) . ?rest)
             (free-identifier=? #'?PVE ctxt)
             (cons #'(?arg* ?b ?b* ...) (%filter-cases #'?rest)))
            ((?case . ?rest)
	     (%filter-cases #'?rest))))
        (let ((case* (%filter-cases case*)))
          (with-syntax (((CASE* ...) case*))
            (values #'(case-lambda CASE* ... (args (interrupt)))
		    (not (null? case*))))))
      (syntax-case x ()
        ((?stx ?name ?interruptable ?case* ...)
	 (let ((cases #'(?case* ...)))
	   (with-syntax
	       ((COGEN-P		(%cogen-name #'?stx #'?name "pred"))
		(COGEN-E		(%cogen-name #'?stx #'?name "effect"))
		(COGEN-V		(%cogen-name #'?stx #'?name "value"))
		(INTERRUPTABLE?		(syntax-case #'?interruptable (safe unsafe)
					  (safe   #t)
					  (unsafe #f))))
	     (let-values (((p-handler phandled?) (%generate-handler #'?name #'P cases))
			  ((v-handler vhandled?) (%generate-handler #'?name #'V cases))
			  ((e-handler ehandled?) (%generate-handler #'?name #'E cases)))
	       #`(begin
		   (define COGEN-P #,p-handler)
		   (define COGEN-V #,v-handler)
		   (define COGEN-E #,e-handler)
		   (module ()
		     (set-primop! '?name (make-PH INTERRUPTABLE?
						  COGEN-P #,phandled?
						  COGEN-V #,vhandled?
						  COGEN-E #,ehandled?)))))))))))

  (define (handle-fix lhs* rhs* body)
    (define (closure-size x)
      (struct-case x
        ((closure code free*)
         (if (null? free*)
             0
             (align (+ disp-closure-data
                       (* (length free*) wordsize)))))))
    (define (partition p? lhs* rhs*)
      (cond
        ((null? lhs*) (values '() '() '() '()))
        (else
         (let-values (((a* b* c* d*)
                       (partition p? (cdr lhs*) (cdr rhs*)))
                      ((x y) (values (car lhs*) (car rhs*))))
           (cond
             ((p? x y)
              (values (cons x a*) (cons y b*) c* d*))
             (else
              (values a* b* (cons x c*) (cons y d*))))))))
    (define (combinator? lhs rhs)
      (struct-case rhs
        ((closure code free*) (null? free*))))
    (define (sum n* n)
      (cond
        ((null? n*) n)
        (else (sum (cdr n*) (+ n (car n*))))))
    (define (adders lhs n n*)
      (cond
        ((null? n*) '())
        (else
         (cons (prm 'int+ lhs (K n))
               (adders lhs (+ n (car n*)) (cdr n*))))))
    (define (build-closures lhs* rhs* body)
      (let ((lhs (car lhs*)) (rhs (car rhs*))
            (lhs* (cdr lhs*)) (rhs* (cdr rhs*)))
        (let ((n (closure-size rhs))
              (n* (map closure-size rhs*)))
          (make-bind (list lhs)
                     (list (prm 'alloc
                                (K (sum n* n))
                                (K closure-tag)))
            (make-bind lhs* (adders lhs n n*)
              body)))))
    (define (build-setters lhs* rhs* body)
      (define (build-setter lhs rhs body)
        (struct-case rhs
          ((closure code free*)
           (make-seq
             (prm 'mset lhs
                  (K (- disp-closure-code closure-tag))
                  (V code))
             (let f ((ls free*)
                     (i (- disp-closure-data closure-tag)))
               (cond
                 ((null? ls) body)
                 (else
                  (make-seq
                    (prm 'mset lhs (K i) (V (car ls)))
                    (f (cdr ls) (+ i wordsize))))))))))
      (cond
        ((null? lhs*) body)
        (else
         (build-setter (car lhs*) (car rhs*)
           (build-setters (cdr lhs*) (cdr rhs*) body)))))
    (let-values (((flhs* frhs* clhs* crhs*)
                  (partition combinator? lhs* rhs*)))
      (cond
        ((null? clhs*) (make-bind flhs* (map V frhs*) body))
        ((null? flhs*)
         (build-closures clhs* crhs*
            (build-setters clhs* crhs* body)))
        (else
         (make-bind flhs* (map V frhs*)
           (build-closures clhs* crhs*
             (build-setters clhs* crhs* body)))))))


  (define (constant-rep x)
    (let ((c (constant-value x)))
      (cond
        ((fx? c) (make-constant (* c fx-scale)))
        ((boolean? c) (make-constant (if c bool-t bool-f)))
        ((eq? c (void)) (make-constant void-object))
        ((bwp-object? c) (make-constant bwp-object))
        ((char? c) (make-constant
                     (fxlogor char-tag
                       (fxsll (char->integer c) char-shift))))
        ((null? c) (make-constant nil))
        ((eof-object? c) (make-constant eof))
        ((object? c) (error 'constant-rep "double-wrap"))
        (else (make-constant (make-object c))))))

  (define (V x) ;;; erase known values
    (struct-case x
      ((known x t)
       (unknown-V x))
      (else (unknown-V x))))

  (define (unknown-V x)
    (struct-case x
      ((constant) (constant-rep x))
      ((var)      x)
      ((primref name)
       (prm 'mref
             (K (make-object (primref->symbol name)))
             (K (- disp-symbol-record-value symbol-ptag))))
      ((code-loc) (make-constant x))
      ((closure)  (make-constant x))
      ((bind lhs* rhs* body)
       (make-bind lhs* (map V rhs*) (V body)))
      ((fix lhs* rhs* body)
       (handle-fix lhs* rhs* (V body)))
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (V e1) (V e2)))
      ((seq e0 e1)
       (make-seq (E e0) (V e1)))
      ((primcall op arg*)
       (case op
         ((debug-call)
          (cogen-debug-call op 'V arg* V))
         (else (cogen-primop op 'V arg*))))
      ((forcall op arg*)
       (make-forcall op (map V arg*)))
      ((funcall rator arg*)
       (make-funcall (Function rator) (map V arg*)))
      ((jmpcall label rator arg*)
       (make-jmpcall label (V rator) (map V arg*)))
      (else (error 'cogen-V "invalid value expr" x))))

  (define (cogen-debug-call op ctxt arg* k)
    (define (fail)
      (k (make-funcall (make-primref 'debug-call) arg*)))
    (assert (>= (length arg*) 2))
    (let ((src/expr (car arg*))
          (op (cadr arg*))
          (args (cddr arg*)))
      (struct-case (remove-tag op)
        ((primref name)
         (if (primop? name)
             (cogen-debug-primop name src/expr ctxt args)
             (fail)))
        (else (fail)))))

  (define (P x)
    (struct-case x
      ((constant c) (if c (K #t) (K #f)))
      ((primref)  (K #t))
      ((code-loc) (K #t))
      ((closure)  (K #t))
      ((bind lhs* rhs* body)
       (make-bind lhs* (map V rhs*) (P body)))
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (P e1) (P e2)))
      ((seq e0 e1)
       (make-seq (E e0) (P e1)))
      ((fix lhs* rhs* body)
       (handle-fix lhs* rhs* (P body)))
      ((primcall op arg*)
       (case op
         ((debug-call)
          (cogen-debug-call op 'P arg* P))
         (else (cogen-primop op 'P arg*))))
      ((var)     (prm '!= (V x) (V (K #f))))
      ((funcall) (prm '!= (V x) (V (K #f))))
      ((jmpcall) (prm '!= (V x) (V (K #f))))
      ((forcall) (prm '!= (V x) (V (K #f))))
      ((known expr type)
       ;;; FIXME: suboptimal
       (P expr))
      (else (error 'cogen-P "invalid pred expr" x))))

  (define (E x)
    (struct-case x
      ((constant) (nop))
      ((var)      (nop))
      ((primref)  (nop))
      ((code-loc) (nop))
      ((closure)  (nop))
      ((bind lhs* rhs* body)
       (make-bind lhs* (map V rhs*) (E body)))
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (E e1) (E e2)))
      ((seq e0 e1)
       (make-seq (E e0) (E e1)))
      ((fix lhs* rhs* body)
       (handle-fix lhs* rhs* (E body)))
      ((primcall op arg*)
       (case op
         ((debug-call)
          (cogen-debug-call op 'E arg* E))
         (else (cogen-primop op 'E arg*))))
      ((forcall op arg*)
       (make-forcall op (map V arg*)))
      ((funcall rator arg*)
       (make-funcall (Function rator) (map V arg*)))
      ((jmpcall label rator arg*)
       (make-jmpcall label (V rator) (map V arg*)))
      ((known expr type)
       ;;; FIXME: suboptimal
       (E expr))
      (else (error 'cogen-E "invalid effect expr" x))))

  (define (Function x)
    (define (Function x check?)
      (define (nonproc x check?)
        (cond
          (check?
           (with-tmp ((x (V x)))
             (make-shortcut
               (make-seq
                 (make-conditional
                   (tag-test x closure-mask closure-tag)
                   (prm 'nop)
                   (prm 'interrupt))
                 x)
               (V (make-funcall (make-primref 'error)
                    (list (K 'apply) (K "not a procedure") x))))))
          (else
           (V x))))
      (struct-case x
         ((primcall op args)
          (cond
            ((and (eq? op 'top-level-value)
                  (= (length args) 1)
                  (let f ((x (car args)))
                    (struct-case x
                      ((constant x)
                       (and (symbol? x) x))
                      ((known x t) (f x))
                      (else #f)))) =>
             (lambda (sym)
               (reset-symbol-proc! sym)
               (prm 'mref (T (K sym))
                    (K (- disp-symbol-record-proc symbol-ptag)))))
            (else (nonproc x check?))))
         ((primref op) (V x))
         ((known x t)
          (cond
            ((eq? (T:procedure? t) 'yes)
             ;(record-optimization 'procedure x)
             (Function x #f))
            (else (Function x check?))))
         (else (nonproc x check?))))
    (Function x #t))



  (define record-optimization^
    (let ((h (make-eq-hashtable)))
      (lambda (what expr)
        (let ((n (hashtable-ref h what 0)))
          (hashtable-set! h what (+ n 1))
          (printf "optimize ~a(~s): ~s\n" what n (unparse expr))))))
  (define-syntax record-optimization
    (syntax-rules ()
      ((_ what expr) (void))))

  ;;;========================================================================
  ;;;
  (define (interrupt-unless x)
    (make-conditional x (prm 'nop) (interrupt)))
  (define (interrupt-when x)
    (make-conditional x (interrupt) (prm 'nop)))
  (define (interrupt-unless-fixnum x)
    (interrupt-unless (tag-test x fx-mask fx-tag)))


  (define (T x)
    (struct-case x
      ((var) x)
      ((constant i) (constant-rep x))
      ((known expr type)
       (make-known (T expr) type))
      (else (error 'cogen-T "invalid" (unparse x)))))

  (define (ClambdaCase x)
    (struct-case x
      ((clambda-case info body)
       (make-clambda-case info (V body)))
      (else (error 'specify-rep "invalid clambda-case" x))))
  ;;;
  (define (Clambda x)
    (struct-case x
      ((clambda label case* cp free* name)
       (make-clambda label
          (map ClambdaCase case*)
          cp free* name))
      (else (error 'specify-rep "invalid clambda" x))))
  ;;;
  (define (Program x)
    (struct-case x
      ((codes code* body)
       (let ((code* (map Clambda code*))
             (body (V body)))
         (make-codes code* body)))
      (else (error 'specify-rep "invalid program" x))))

  (define (specify-representation x)
    (let ((x (Program x)))
      x))

  (include "pass-specify-rep-primops.ss"))
