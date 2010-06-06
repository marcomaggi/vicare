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


;;; Work in progress

;;; Oscar Waddell. "Extending the Scope of Syntactic Abstraction". PhD.
;;; Thesis. Indiana University Computer Science Department. August 1999.
;;; Available online:
;;;   http://www.cs.indiana.edu/~owaddell/papers/thesis.ps.gz

(module (source-optimize optimize-level cp0-effort-limit cp0-size-limit)
  (define who 'source-optimize)
  ;;;
  (define-structure (app rand* ctxt)
    ([inlined #f]))
  ;;;
  (define-structure (operand expr env ec)
    ([value                  #f]
     [residualize-for-effect #f]
     [size                    0]
     [inner-pending          #f]
     [outer-pending          #f]))
  ;;;
  (define-structure (counter value ctxt k))
  ;;;
  (define (passive-counter)
    (make-counter (greatest-fixnum) #f
      (lambda args
        (error 'passive-counter "invalid abort"))))
  ;;;
  (define (passive-counter-value x)
    (- (greatest-fixnum) (counter-value x)))
  ;;;
  (define (active-counter? x)
    (and (counter? x) (counter-ctxt x)))
  ;;;
  (define (decrement x amt)
    (let ([n (- (counter-value x) amt)])
      (set-counter-value! x n)
      (when (< n 0)
        (reset-integrated! (counter-ctxt x))
        ((counter-k x) #f))))
  ;;;
  (define (abort-counter! x)
    (reset-integrated! (counter-ctxt x))
    ((counter-k x) #f))
  ;;;
  (define (reset-integrated! ctxt)
    (set-app-inlined! ctxt #f)
    (let ([ctxt (app-ctxt ctxt)])
      (when (app? ctxt)
        (reset-integrated! ctxt))))
  ;;;
  ;;;
  (module (with-extended-env copy-var)
    (define (copy-var x)
      (let ([y (make-prelex (prelex-name x) #f)])
        (set-prelex-source-referenced?! y
          (prelex-source-referenced? x))
        (set-prelex-source-assigned?! y
          (prelex-source-assigned? x))
        (let ([loc (prelex-global-location x)])
          (when loc
            (set-prelex-global-location! y loc)
            (set-prelex-source-referenced?! y #t)
            (set-prelex-residual-referenced?! y #t)))
        y))
    (define (extend env lhs* rands)
      (if (null? lhs*)
          (values env '())
          (let ([nlhs* (map copy-var lhs*)])
            (when rands
              (for-each
                (lambda (lhs rhs)
                  (set-prelex-operand! lhs rhs))
                nlhs* rands))
            (values (vector lhs* nlhs* env) nlhs*))))
    (define (copy-back ls)
      (for-each
        (lambda (x)
          (set-prelex-source-assigned?! x
             (prelex-residual-assigned? x))
          (set-prelex-source-referenced?! x
             (prelex-residual-referenced? x)))
        ls))
    (define-syntax with-extended-env
      (syntax-rules ()
        [(_ ((e2 args2) (e1 args1 rands)) b b* ...)
         (let-values ([(e2 args2) (extend e1 args1 rands)])
           (let ([v (let () b b* ...)])
             (copy-back args2)
             v))])))

  (define cp0-effort-limit (make-parameter 50))
  (define cp0-size-limit (make-parameter 8))

  (define primitive-info-list
    '(
      [(cons _ _)                           effect-free result-true]
      [(cons* _)                   foldable effect-free            ]
      [(cons* _ . _)                        effect-free result-true]
      [(list)                      foldable effect-free result-true]
      [(list . _)                           effect-free result-true]
      [(reverse ())                foldable effect-free result-true]
      [(string)                    foldable effect-free result-true]
      [(string . _)                                     result-true]
      [(make-string 0)             foldable effect-free result-true]
      [(make-string 0 _)           foldable effect-free result-true]
      [(make-string . _)                                result-true]
      [(make-bytevector 0)         foldable effect-free result-true]
      [(make-bytevector 0 _)       foldable             result-true]
      [(make-bytevector . _)                            result-true]
      [(string-length _)           foldable             result-true]
      [(string-ref _ _)            foldable             result-true]
      [(vector)                    foldable effect-free result-true]
      [(vector . _)                         effect-free result-true]
      [(make-vector 0)             foldable effect-free result-true]
      [(make-vector 0 _)           foldable effect-free result-true]
      [(make-vector . _)                                result-true]
      [(vector-length _)           foldable             result-true]
      [(vector-ref _ _)            foldable                        ]
      [(eq? _ _)                   foldable effect-free            ]
      [(eqv? _ _)                  foldable effect-free            ]
      [(assq _ _)                  foldable                        ]
      [(assv _ _)                  foldable                        ]
      [(assoc _ _)                 foldable                        ]
      [(not _)                     foldable effect-free            ]
      [(null? _)                   foldable effect-free            ]
      [(pair? _)                   foldable effect-free            ]
      [(fixnum? _)                 foldable effect-free            ]
      [(vector? _)                 foldable effect-free            ]
      [(string? _)                 foldable effect-free            ]
      [(char? _)                   foldable effect-free            ]
      [(symbol? _)                 foldable effect-free            ]
      [(procedure? _)              foldable effect-free            ]
      [(eof-object? _)             foldable effect-free            ]
      [(flonum? _)                 foldable effect-free            ]
      [(cflonum? _)                foldable effect-free            ]
      [(compnum? _)                foldable effect-free            ]
      [(integer? _)                foldable effect-free            ]
      [(bignum? _)                 foldable effect-free            ]
      [(ratnum? _)                 foldable effect-free            ]
      [(void)                      foldable effect-free result-true]
      [(car _)                     foldable                        ]
      [(cdr _)                     foldable                        ]
      [(caar _)                    foldable                        ]
      [(cadr _)                    foldable                        ]
      [(cdar _)                    foldable                        ]
      [(cddr _)                    foldable                        ]
      [(caaar _)                   foldable                        ]
      [(caadr _)                   foldable                        ]
      [(cadar _)                   foldable                        ]
      [(caddr _)                   foldable                        ]
      [(cdaar _)                   foldable                        ]
      [(cdadr _)                   foldable                        ]
      [(cddar _)                   foldable                        ]
      [(cdddr _)                   foldable                        ]
      [(caaaar _)                  foldable                        ]
      [(caaadr _)                  foldable                        ]
      [(caadar _)                  foldable                        ]
      [(caaddr _)                  foldable                        ]
      [(cadaar _)                  foldable                        ]
      [(cadadr _)                  foldable                        ]
      [(caddar _)                  foldable                        ]
      [(cadddr _)                  foldable                        ]
      [(cdaaar _)                  foldable                        ]
      [(cdaadr _)                  foldable                        ]
      [(cdadar _)                  foldable                        ]
      [(cdaddr _)                  foldable                        ]
      [(cddaar _)                  foldable                        ]
      [(cddadr _)                  foldable                        ]
      [(cdddar _)                  foldable                        ]
      [(cddddr _)                  foldable                        ]
      [(memq _ _)                  foldable                        ]
      [(memv _ _)                  foldable                        ]
      [(length _)                  foldable             result-true]
      [(+ . _)                     foldable             result-true]
      [(* . _)                     foldable             result-true]
      [(/ _ . _)                   foldable             result-true]
      [(- _ . _)                   foldable             result-true]
      [(fx+ _ _)                   foldable             result-true]
      [(fx- _ _)                   foldable             result-true]
      [(fx* _ _)                   foldable             result-true]
      [(fxior . _)                 foldable             result-true]
      [(fxlogor . _)               foldable             result-true]
      [(fxnot _)                   foldable             result-true]
      [(fxadd1 _)                  foldable             result-true]
      [(fxsub1 _)                  foldable             result-true]
      [(fxzero? _)                 foldable                        ]
      [(fx=? _ . _)                foldable                        ]
      [(fx<? _ . _)                foldable                        ]
      [(fx<=? _ . _)               foldable                        ]
      [(fx>? _ . _)                foldable                        ]
      [(fx>=? _ . _)               foldable                        ]
      [(fx= _ . _)                 foldable                        ]
      [(fx< _ . _)                 foldable                        ]
      [(fx<= _ . _)                foldable                        ]
      [(fx> _ . _)                 foldable                        ]
      [(fx>= _ . _)                foldable                        ]
      [(real-part _)               foldable             result-true]
      [(imag-part _)               foldable             result-true]
      [(fxsll _ _)                 foldable             result-true]
      [(fxsra _ _)                 foldable             result-true]
      [(fxremainder _ _)           foldable             result-true]
      [(fxquotient _ _)            foldable             result-true]
      [(greatest-fixnum)           foldable effect-free result-true]
      [(least-fixnum)              foldable effect-free result-true]
      [(fixnum-width)              foldable effect-free result-true]
      [(char->integer _)           foldable             result-true]
      [(integer->char _)           foldable             result-true]
      [(eof-object)                foldable effect-free result-true]
      [(zero? _)                   foldable                        ]
      [(= _ . _)                   foldable                        ]
      [(< _ . _)                   foldable                        ]
      [(<= _ . _)                  foldable                        ]
      [(> _ . _)                   foldable                        ]
      [(>= _ . _)                  foldable                        ]
      [(expt _ _)                  foldable             result-true]
      [(log _)                     foldable             result-true]
      [(sll _ _)                   foldable             result-true]
      [(sra _ _)                   foldable             result-true]
      [(inexact _)                 foldable             result-true]
      [(exact _)                   foldable             result-true]
      [(add1 _)                    foldable             result-true]
      [(sub1 _)                    foldable             result-true]
      [(bitwise-and _ _)           foldable             result-true]
      [(make-rectangular _ _)      foldable             result-true]
      [(sin _)                     foldable             result-true]
      [(cos _)                     foldable             result-true]
      [(tan _)                     foldable             result-true]
      [(asin _)                    foldable             result-true]
      [(acos _)                    foldable             result-true]
      [(atan _)                    foldable             result-true]
      [(make-eq-hashtable)                  effect-free result-true]
      [(string->number _)          foldable                        ]
      [(string->number _ _)        foldable                        ]
      [($fixnum->flonum _)         foldable effect-free result-true]
      [($char->fixnum _)           foldable effect-free result-true]
      [($fixnum->char _)           foldable effect-free result-true]
      [($fxzero? _)                foldable effect-free            ]
      [($fx+ _ _)                  foldable effect-free result-true]
      [($fx* _ _)                  foldable effect-free result-true]
      [($fx- _ _)                  foldable effect-free result-true]
      [($fx= _ _)                  foldable effect-free            ]
      [($fx>= _ _)                 foldable effect-free            ]
      [($fx> _ _)                  foldable effect-free            ]
      [($fx<= _ _)                 foldable effect-free            ]
      [($fx< _ _)                  foldable effect-free            ]
      [($car _)                    foldable effect-free            ]
      [($cdr _)                    foldable effect-free            ]
      [($struct-ref _ _)           foldable effect-free            ]
      [($struct/rtd? _ _)          foldable effect-free            ]
      [($fxsll _ _)                foldable effect-free result-true]
      [($fxsra _ _)                foldable effect-free result-true]
      [($fxlogor _ _)              foldable effect-free result-true]
      [($fxlogand _ _)             foldable effect-free result-true]
      [($fxadd1 _)                 foldable effect-free result-true]
      [($fxsub1 _)                 foldable effect-free result-true]
      [($vector-length _)          foldable effect-free result-true]
      [($vector-ref _ _)           foldable effect-free result-true]
      [($make-bytevector 0)        foldable effect-free result-true]
      [($make-bytevector 0 _)      foldable effect-free result-true]
      [($make-bytevector . _)               effect-free result-true]
      [($bytevector-u8-ref _ _)    foldable effect-free result-true]
      [($bytevector-length _)      foldable effect-free result-true]
      ;;;
      [(annotation? #f)             foldable effect-free result-false]
      [(annotation-stripped #f)     foldable effect-free result-false]
      ;;; unoptimizable
      [(condition . _)]
      [($make-flonum . _)]
      [(top-level-value . _)]
      [($struct . _)]
      [(make-message-condition . _)]
      [(make-lexical-violation . _)]
      [(make-who-condition . _)]
      [(make-error . _)]
      [(make-i/o-error . _)]
      [(make-i/o-write-error . _)]
      [(make-i/o-read-error . _)]
      [(make-i/o-file-already-exists-error . _)]
      [(make-i/o-file-is-read-only-error . _)]
      [(make-i/o-file-protection-error . _)]
      [(make-i/o-file-does-not-exist-error . _)]
      [(make-undefined-violation . _)]
      [(die . _)]
      [(gensym . _)]
      [(values . _)]
      [(error . _)]
      [(assertion-violation . _)]
      [(console-input-port . _)]
      [(console-output-port . _)]
      [(console-error-port . _)]
      [(printf . _)] ;;; FIXME: reduce to display
      [(newline . _)]
      [(native-transcoder . _)]
      [(open-string-output-port . _)]
      [(open-string-input-port . _)]
      [(environment . _)]
      [(print-gensym . _)]
      [(exit . _)]
      [(interrupt-handler . _)]
      [(display . _)]
      [(write-char . _)]
      [(current-input-port . _)]
      [(current-output-port . _)]
      [(current-error-port . _)]
      [(standard-input-port . _)]
      [(standard-output-port . _)]
      [(standard-error-port . _)]
      [($current-frame . _)]
      [(pretty-width . _)]
      [($fp-at-base . _)]
      [(read-annotated . _)]
      [($collect-key . _)]
      [(make-non-continuable-violation . _)]
      [(format . _)] ;;; FIXME, reduce to string-copy
      [(uuid . _)]
      [(print-graph . _)]
      [(interaction-environment . _)]
      [(make-guardian)]
      [(command-line-arguments)]
      [(make-record-type-descriptor . _)] ;;; FIXME
      [(make-assertion-violation . _)]
      [(new-cafe . _)]
      [(getenv . _)]
      [(gensym-prefix . _)]
      [($arg-list . _)]
      [($make-symbol . _)]
      [(string->utf8 . _)]
      [($make-call-with-values-procedure . _)]
      [($make-values-procedure . _)]
      [($unset-interrupted! . _)]
      [(make-interrupted-condition . _)]
      [($interrupted? . _)]
      [($symbol-value . _)]
      [(library-extensions . _)]
      [(base-rtd . _)]
      [($data->transcoder . _)]
      [(current-time . _)]
    ))

  (module (primprop)
    (define-syntax ct-gensym
      (lambda (x)
        (with-syntax ([g (datum->syntax #'here (gensym))])
          #'(quote g))))
    (define g (ct-gensym))
    (define (primprop p)
      (or (getprop p g) '()))
    (define (get prim ls)
      (cond
        [(null? ls) (values '() '())]
        [else
         (let ([a (car ls)])
           (let ([cc (car a)])
             (cond
               [(eq? (car cc) prim)
                (let-values ([(p* ls) (get prim (cdr ls))])
                  (values (cons (cons (cdr cc) (cdr a)) p*) ls))]
               [else (values '() ls)])))]))
    (let f ([ls primitive-info-list])
      (unless (null? ls)
        (let ([a (car ls)])
          (let ([cc (car a)] [cv (cdr a)])
            (let ([prim (car cc)] [args (cdr cc)])
              (let-values ([(p* ls) (get prim (cdr ls))])
                (putprop prim g
                  (cons (cons args cv) p*))
                (f ls))))))))
  (define (primitive-info op args)
    (define (matches? x)
      (let f ([args args] [params (car x)])
        (cond
          [(pair? params)
           (and (pair? args)
                (case (car params)
                  [(_) (f (cdr args) (cdr params))]
                  [(#f 0 ())
                   (let ([v (value-visit-operand! (car args))])
                     (and (constant? v)
                          (equal? (constant-value v) (car params))
                          (f (cdr args) (cdr params))))]
                  [else
                   (error 'primitive-info "cannot happen" op (car params))]))]
          [(eq? params '_) #t]
          [(null? params) (null? args)]
          [else (error 'primitive-info "cannot happen" op params)])))
    (cond
      [(find matches? (primprop op))]
      [else '()]))

  (define (info-foldable? info) (memq 'foldable info))
  (define (info-effect-free? info) (memq 'effect-free info))
  (define (info-result-true? info) (memq 'result-true info))
  (define (info-result-false? info) (memq 'result-false info))

  (define-syntax ctxt-case
    (lambda (stx)
      (define (test x)
        (case (syntax->datum x)
          [(p)   #'(eq? t 'p)]
          [(v)   #'(eq? t 'v)]
          [(e)   #'(eq? t 'e)]
          [(app) #'(app? t)]
          [else (syntax-violation stx "invalid ctxt" x)]))
      (define (extract cls*)
        (syntax-case cls* (else)
          [() #'(error 'extract "unmatched ctxt" t)]
          [([else e e* ...]) #'(begin e e* ...)]
          [([(t* ...) e e* ...] rest ...)
           (with-syntax ([(t* ...) (map test #'(t* ...))]
                         [body (extract #'(rest ...))])
             #'(if (or t* ...)
                   (begin e e* ...)
                   body))]))
      (syntax-case stx ()
        [(_ expr cls* ...)
         (with-syntax ([body (extract #'(cls* ...))])
           #'(let ([t expr])
               body))])))
  (define (mkseq e0 e1)
    ;;; returns a (seq e0 e1) with a seq-less e1 if both
    ;;; e0 and e1 are constructed properly.
    (if (simple? e0)
        e1
        (let ([e0 (struct-case e0
                    [(seq e0a e0b) (if (simple? e0b) e0a e0)]
                    [else e0])])
          (struct-case e1
            [(seq e1a e1b) (make-seq (make-seq e0 e1a) e1b)]
            [else (make-seq e0 e1)]))))
  ;;; simple?: check quickly whether something is effect-free
  (define (simple? x)
    (struct-case x
      [(constant) #t]
      [(prelex)   #t]
      [(primref)  #t]
      [(clambda)  #t]
      [else       #f]))
  ;;; result returns the "last" value of an expression
  (define (result-expr x)
    (struct-case x
      [(seq e0 e1) e1]
      [else        x]))
  ;;;
  (define (records-equal? x y ctxt)
    (struct-case x
      [(constant kx)
       (struct-case y
         [(constant ky)
          (ctxt-case ctxt
            [(e) #t]
            [(p) (if kx ky (not ky))]
            [else (eq? kx ky)])]
         [else #f])]
      [else #f]))
  ;;;
  (define (residualize-operands e rand* sc)
    (cond
      [(null? rand*) e]
      [(not (operand-residualize-for-effect (car rand*)))
       (residualize-operands e (cdr rand*) sc)]
      [else
       (let ([opnd (car rand*)])
         (let ([e1 (or (operand-value opnd)
                       (struct-case opnd
                         [(operand expr env ec)
                          (E expr 'e env ec sc)]))])
           (if (simple? e1)
               (residualize-operands e (cdr rand*) sc)
               (begin
                 (decrement sc (operand-size opnd))
                 (mkseq e1 (residualize-operands e (cdr rand*) sc))))))]))
  (define (value-visit-operand! rand)
    (or (operand-value rand)
        (let ([sc (passive-counter)])
          (let ([e (struct-case rand
                     [(operand expr env ec)
                      (E expr 'v env sc ec)])])
            (set-operand-value! rand e)
            (set-operand-size! rand (passive-counter-value sc))
            e))))
  (define (score-value-visit-operand! rand sc)
    (let ([val (value-visit-operand! rand)])
      (let ([score (operand-size rand)])
        (decrement sc score))
      val))
  (define (E-call rator rand* env ctxt ec sc)
    (let ([ctxt (make-app rand* ctxt)])
      (let ([rator (E rator ctxt env ec sc)])
        (if (app-inlined ctxt)
            (residualize-operands rator rand* sc)
            (begin
              (decrement sc (if (primref? rator) 1 3))
              (make-funcall rator
                (map (lambda (x) (score-value-visit-operand! x sc))
                     rand*)))))))
  ;;;
  (define (E-debug-call ctxt ec sc)
    (let ([rand* (app-rand* ctxt)])
      (cond
        [(< (length rand*) 2)
         (decrement sc 1)
         (make-primref 'debug-call)]
        [else
         (let ([src/expr (car rand*)]
               [rator (cadr rand*)]
               [rands (cddr rand*)])
           (let ([ctxt2 (make-app rands (app-ctxt ctxt))])
             (let ([rator (E (operand-expr rator)
                             ctxt2
                             (operand-env rator)
                             (operand-ec rator)
                             sc)])
               (if (app-inlined ctxt2)
                   (begin
                     (set-app-inlined! ctxt #t)
                     (residualize-operands rator (cons src/expr rands) sc))
                   (begin
                     (decrement sc 1)
                     (make-primref 'debug-call))))))])))
  ;;;
  (define (E-var x ctxt env ec sc)
    (ctxt-case ctxt
      [(e) (make-constant (void))]
      [else
       (let ([x (lookup x env)])
         (let ([opnd (prelex-operand x)])
           (if (and opnd (not (operand-inner-pending opnd)))
               (begin
                 (dynamic-wind
                   (lambda () (set-operand-inner-pending! opnd #t))
                   (lambda () (value-visit-operand! opnd))
                   (lambda () (set-operand-inner-pending! opnd #f)))
                 (if (prelex-source-assigned? x)
                     (residualize-ref x sc)
                     (copy x opnd ctxt ec sc)))
               (residualize-ref x sc))))]))
  ;;;
  (define (copy x opnd ctxt ec sc)
    (let ([rhs (result-expr (operand-value opnd))])
      (struct-case rhs
        [(constant) rhs]
        [(prelex)
         (if (prelex-source-assigned? rhs)
             (residualize-ref x sc)
             (let ([opnd (prelex-operand rhs)])
               (if (and opnd (operand-value opnd))
                   (copy2 rhs opnd ctxt ec sc)
                   (residualize-ref rhs sc))))]
        [else (copy2 x opnd ctxt ec sc)])))
  ;;;
  (define (copy2 x opnd ctxt ec sc)
    (let ([rhs (result-expr (operand-value opnd))])
      (struct-case rhs
        [(clambda)
         (ctxt-case ctxt
           [(v) (residualize-ref x sc)]
           [(p) (make-constant #t)]
           [(e) (make-constant (void))]
           [(app)
            (or (and (not (operand-outer-pending opnd))
                     (dynamic-wind
                       (lambda () (set-operand-outer-pending! opnd #t))
                       (lambda ()
                         (call/cc
                           (lambda (abort)
                             (inline rhs ctxt empty-env
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
                       (lambda () (set-operand-outer-pending! opnd #f))))
                (residualize-ref x sc))])]
        [(primref p)
         (ctxt-case ctxt
           [(v) rhs]
           [(p) (make-constant #t)]
           [(e) (make-constant (void))]
           [(app) (fold-prim p ctxt ec sc)])]
        [else (residualize-ref x sc)])))
  (define (inline proc ctxt env ec sc)
    (define (get-case cases rand*)
      (define (compatible? x)
        (struct-case (clambda-case-info x)
          [(case-info label args proper)
           (cond
             [proper (= (length rand*) (length args))]
             [else (>= (length rand*) (- (length args) 1))])]))
      (cond
        [(memp compatible? cases) => car]
        [else #f]))
    (define (partition args rand*)
      (cond
        [(null? (cdr args))
         (let ([r (car args)])
           (let ([t* (map (lambda (x) (copy-var r)) rand*)])
             (values '() t* r)))]
        [else
         (let ([x (car args)])
           (let-values ([(x* t* r) (partition (cdr args) (cdr rand*))])
             (values (cons x x*) t* r)))]))
    (struct-case proc
      [(clambda g cases cp free name)
       (let ([rand* (app-rand* ctxt)])
         (struct-case (get-case cases rand*)
           [(clambda-case info body)
            (struct-case info
              [(case-info label args proper)
               (cond
                 [proper
                  (with-extended-env ((env args) (env args rand*))
                    (let ([body (E body (app-ctxt ctxt) env ec sc)])
                      (let ([result (make-let-binding args rand* body sc)])
                        (set-app-inlined! ctxt #t)
                        result)))]
                 [else
                  (let-values ([(x* t* r) (partition args rand*)])
                    (with-extended-env ((env a*)
                                        (env (append x* t*) rand*))
                      (let ([rarg (make-operand
                                    (make-funcall (make-primref 'list) t*)
                                    env ec)])
                        (with-extended-env ((env b*)
                                            (env (list r) (list rarg)))
                          (let ([result
                                 (make-let-binding a* rand*
                                   (make-let-binding b* (list rarg)
                                     (E body (app-ctxt ctxt) env ec sc)
                                     sc)
                                   sc)])
                            (set-app-inlined! ctxt #t)
                            result)))))])])]
           [else
             (E proc 'v env ec sc)]))]))
  ;;;
  (define (do-bind lhs* rhs* body ctxt env ec sc)
    (let ([rand* (map (lambda (x) (make-operand x env ec)) rhs*)])
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
        [(prelex-residual-referenced? var)
         (values
            (cons var lhs*)
            (cons (score-value-visit-operand! rand sc) rhs*))]
        [(prelex-residual-assigned? var)
         (set-operand-residualize-for-effect! rand #t)
         (values
            (cons var lhs*)
            (cons (make-constant (void)) rhs*))]
        [else
         (set-operand-residualize-for-effect! rand #t)
         (values lhs* rhs*)]))
    (define (process var* rand*)
      (cond
        [(null? var*) (values '() '())]
        [else
         (let ([var (car var*)] [rand (car rand*)])
           (let-values ([(lhs* rhs*) (process (cdr var*) (cdr rand*))])
             (process1 var rand lhs* rhs*)))]))
    (let-values ([(lhs* rhs*) (process var* rand*)])
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
    (let ([rand* (app-rand* ctxt)])
      (let ([info (primitive-info p rand*)])
        (let ([result
               (or (and (info-effect-free? info)
                        (ctxt-case (app-ctxt ctxt)
                          [(e) (make-constant (void))]
                          [(p)
                           (cond
                             [(info-result-true? info)
                              (make-constant #t)]
                             [(info-result-false? info)
                              (make-constant #f)]
                             [else #f])]
                          [else #f]))
                   (and (info-foldable? info)
                        (let ([val*
                               (map (lambda (x) (value-visit-operand! x)) rand*)])
                          (cond
                            [(andmap constant? val*)
                             (get-value p (map constant-value val*))]
                            [else #f]))))])
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
          [(funcall rator rand*)
           (struct-case rator
             [(primref op)
              (and (eq? op 'not)
                   (= (length rand*) 1)
                   (build-conditional (car rand*) e2 e1))]
             [else #f])]
          [else #f])
        (make-conditional e0 e1 e2)))

  (define (E x ctxt env ec sc)
    (decrement ec 1)
    (struct-case x
      [(constant) (decrement sc 1) x]
      [(prelex) (E-var x ctxt env ec sc)]
      [(seq e0 e1)
       (mkseq (E e0 'e env ec sc) (E e1 ctxt env ec sc))]
      [(conditional e0 e1 e2)
       (let ([e0 (E e0 'p env ec sc)])
         (struct-case (result-expr e0)
           [(constant k)
            (mkseq e0 (E (if k e1 e2) ctxt env ec sc))]
           [else
            (let ([ctxt (ctxt-case ctxt [(app) 'v] [else ctxt])])
              (let ([e1 (E e1 ctxt env ec sc)]
                    [e2 (E e2 ctxt env ec sc)])
                (if (records-equal? e1 e2 ctxt)
                    (mkseq e0 e1)
                    (begin
                      (decrement sc 1)
                      (build-conditional e0 e1 e2)))))]))]
      [(assign x v)
       (mkseq
         (let ([x (lookup x env)])
           (cond
             [(not (prelex-source-referenced? x))
              ;;; dead on arrival
              (E v 'e env ec sc)]
             [else
              (decrement sc 1)
              (set-prelex-residual-assigned?! x
                (prelex-source-assigned? x))
              (make-assign x (E v 'v env ec sc))]))
         (make-constant (void)))]
      [(funcall rator rand*)
       (E-call rator
         (map (lambda (x) (make-operand x env ec)) rand*)
         env ctxt ec sc)]
      [(forcall name rand*)
       (decrement sc 1)
       (make-forcall name (map (lambda (x) (E x 'v env ec sc)) rand*))]
      [(primref name)
       (ctxt-case ctxt
         [(app)
          (case name
            [(debug-call) (E-debug-call ctxt ec sc)]
            [else (fold-prim name ctxt ec sc)])]
         [(v) (decrement sc 1) x]
         [else (make-constant #t)])]
      [(clambda g cases cp free name)
       (ctxt-case ctxt
         [(app) (inline x ctxt env ec sc)]
         [(p e) (make-constant #t)]
         [else
          (decrement sc 2)
          (make-clambda (gensym)
            (map
              (lambda (x)
                (struct-case x
                  [(clambda-case info body)
                   (struct-case info
                     [(case-info label args proper)
                      (with-extended-env ((env args) (env args #f))
                        (make-clambda-case
                          (make-case-info (gensym) args proper)
                          (E body 'v env ec sc)))])]))
              cases)
            cp free name)])]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body ctxt env ec sc)]
      [(fix lhs* rhs* body)
       (with-extended-env ((env lhs*) (env lhs* #f))
         (for-each
           (lambda (lhs rhs)
             (set-prelex-operand! lhs (make-operand rhs env ec)))
            lhs* rhs*)
         (let ([body (E body ctxt env ec sc)])
           (let ([lhs* (remp
                         (lambda (x)
                           (not (prelex-residual-referenced? x)))
                         lhs*)])
             (cond
               [(null? lhs*) body]
               [else
                (decrement sc 1)
                (make-fix lhs*
                  (map (lambda (x)
                         (let ([opnd (prelex-operand x)])
                           (decrement sc (+ (operand-size opnd) 1))
                           (value-visit-operand! opnd)))
                       lhs*)
                  body)]))))]
      [else
       (error who "invalid expression" x)]))
  (define empty-env '())
  (define (lookup x env)
    (cond
      [(vector? env)
       (let f ([lhs* (vector-ref env 0)] [rhs* (vector-ref env 1)])
         (cond
           [(null? lhs*) (lookup x (vector-ref env 2))]
           [(eq? x (car lhs*)) (car rhs*)]
           [else (f (cdr lhs*) (cdr rhs*))]))]
      [else x]))
  (define optimize-level
    (make-parameter 1
      (lambda (x)
        (if (memv x '(0 1 2))
            x
            (die 'optimize-level "valid levels are 0, 1, and 2")))))
  (define (source-optimize expr)
    (define (source-optimize expr)
      (E expr 'v empty-env (passive-counter) (passive-counter)))
    (case (optimize-level)
      [(2)
       (source-optimize expr)]
      [(1)
       (parameterize ([cp0-size-limit 0])
         (source-optimize expr))]
      [else expr]))
)




