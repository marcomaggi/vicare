#!/usr/bin/env ikarus --r6rs-script
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

;;; vim:syntax=scheme
(import 
  (ikarus.compiler)
  (match)
  (except (ikarus) perform-tag-analysis tag-analysis-output
          cp0-effort-limit cp0-size-limit expand/optimize 
          optimizer-output
          optimize-cp optimize-level assembler-output))


(define (compile1 x)
  (let ([p (open-file-output-port "test64.fasl" (file-options no-fail))])
    (parameterize ([optimize-level 0]
                   [assembler-output #t])
      (compile-core-expr-to-port x p))
    (close-output-port p)))

(define (compile-and-run x) 
  (compile1 x)
  (let ([rs (system "../src/ikarus -b test64.fasl > test64.out")])
    (unless (= rs 0) (error 'run1 "died with status" rs))
    (with-input-from-file "test64.out"
      (lambda () 
        (let ([s (get-string-all (current-input-port))])
          (if (eof-object? s) 
              ""
              s))))))

(define (compile-test-and-run expr expected)
  (printf "Compiling:\n")
  (pretty-print expr)
  (let ([val (compile-and-run (fixup expr))])
    (unless (equal? val expected) 
      (error 'compile-test-and-run "failed:got:expected" val expected))))

(define (test-all)
  (for-each
    (lambda (x)
       (compile-test-and-run (car x) (cadr x)))
    all-tests))

(define all-tests
  '([(quote 42)  "42\n"]
    [(quote #f)  "#f\n"]
    [(quote ())  "()\n"]))

(define (self-evaluating? x)
  (or (number? x) (char? x) (boolean? x) (null? x) (string? x)))

(define prims-alist
  '([$fxadd1 $fxadd1]
    [$fxsub1 $fxsub1]
    [$fixnum->char $fixnum->char]
    [$char->fixnum $char->fixnum]
    [fixnum? fixnum?]
    [$fxzero? $fxzero?]
    [null? null?]
    [boolean? boolean?]
    [char? char?]
    [not not]
    [$fxlognot $fxlognot]
    [fx+ $fx+]
    [fx- $fx-]
    [fx* $fx*]
    [fxadd1 $fxadd1]
    [fxsub1 $fxsub1]
    [fxlogor $fxlogor]
    [fxlogand $fxlogand]
    [fxlognot $fxlognot]
    [fx= $fx=]
    [fx< $fx<]
    [fx<= $fx<=]
    [fx> $fx>]
    [fx>= $fx>=]
    [pair? pair?]
    [cons cons]
    [car $car]
    [cdr $cdr]
    [set-car! $set-car!]
    [set-cdr! $set-cdr!]
    [eq? eq?]
    [make-vector $make-vector]
    [vector? vector?]
    [vector-length $vector-length]
    [vector-set! $vector-set!]
    [vector-ref $vector-ref]
    [string? string?]
    [make-string $make-string]
    [string-set! $string-set!]
    [string-ref $string-ref]
    [string-length $string-length]
    [char= $char=]
    [fixnum->char $fixnum->char]
    [char->fixnum $char->fixnum]
    [procedure? procedure?]
    [fxzero? $fxzero?]
    [vector vector]
    [symbol? symbol?]
    [make-bytevector $make-bytevector]
    [bytevector? bytevector?]
    [bytevector-set! $bytevector-set!]
    [bytevector-ref $bytevector-u8-ref]
    [bytevector-length $bytevector-length]
    ))


(define (fixup x)
  (define (P x) 
    `(primitive ,x))
  (define (Expr x env)
    (match x
      [,n (guard (self-evaluating? n)) `(quote ,n)]
      [,var (guard (symbol? var))
       (cond
         [(assq var env) => cdr]
         [else (error 'fixup "unbound var" var)])]
      [(,rator ,[rand*] ...) 
       (guard (assq rator env)) 
       `(,(Expr rator env) ,rand* ...)]
      [(quote ,x) `(quote ,x)]
      [(,prim ,[args] ...) 
       (guard (assq prim prims-alist))
       `((primitive ,(cadr (assq prim prims-alist))) ,args ...)]
      [(if ,[e0] ,[e1] ,[e2])
       `(if ,e0 ,e1 ,e2)]
      [(let ([,lhs* ,[rhs*]] ...) ,body ,body* ...)
       (let ([nlhs* (map gensym lhs*)])
         (let ([env (append (map cons lhs* nlhs*) env)])
           `((case-lambda 
               [,nlhs* 
                (begin ,(Expr body env)
                       ,(map (lambda (x) (Expr x env)) body*)
                       ...)])
             ,rhs* ...)))]
      [(letrec ([,lhs* ,rhs*] ...) ,body ,body* ...)
       (let* ([nlhs* (map gensym lhs*)]
              [env (append (map cons lhs* nlhs*) env)]
              [E (lambda (x) (Expr x env))])
         `(letrec ([,nlhs* ,(map E rhs*)] ...)
            (begin
              ,(E body)
              ,(map E body*) ...)))]
      [(letrec* ([,lhs* ,rhs*] ...) ,body ,body* ...)
       (let* ([nlhs* (map gensym lhs*)]
              [env (append (map cons lhs* nlhs*) env)]
              [E (lambda (x) (Expr x env))])
         `(letrec* ([,nlhs* ,(map E rhs*)] ...)
            (begin
              ,(E body)
              ,(map E body*) ...)))] 
      [(lambda (,fml* ...) ,body ,body* ...)
       (let ([g* (map gensym fml*)])
         (let ([env (append (map cons fml* g*) env)])
           `(case-lambda 
              [(,g* ...) 
               (begin ,(Expr body env)
                      ,(map (lambda (x) (Expr x env)) body*)
                      ...)])))]
      [(lambda (,fml* ... . ,fml) ,body ,body* ...)
       (let ([g* (map gensym fml*)]
             [g (gensym fml)])
         (let ([env (append (map cons 
                                 (cons fml fml*)
                                 (cons g g*))
                            env)])
           `(case-lambda 
              [(,g* ... . ,g) 
               (begin ,(Expr body env)
                      ,(map (lambda (x) (Expr x env)) body*)
                      ...)])))] 
      [(begin ,[e] ,[e*] ...) 
       `(begin ,e ,e* ...)]
      [(and) ''#t]
      [(and ,[e]) e]
      [(and ,[e] ,e* ...)
       `(if ,e ,(Expr `(and ,e* ...) env) (quote #f))]
      [(or) ''#f]
      [(or ,[e]) e]
      [(or ,[e] ,e* ...)
       (let ([t (gensym)])
         `((case-lambda 
             [(,t) 
              (if ,t ,t ,(Expr `(or ,e* ...) env))])
           ,e))]
      [(when ,[e] ,[b] ,[b*] ...) 
       `(if ,e 
            (begin ,b ,b* ...) 
            (,(P 'void)))]
      [(unless ,[e] ,[b] ,[b*] ...) 
       `(if (,(P 'not) ,e)
            (begin ,b ,b* ...)
            (,(P 'void)))]
      [(cond) `(,(P 'void))]
      [(cond ,cls* ... ,cls)
       (let ()
         (define (fst-clause x rest)
           (match x
             [(,e ,arr ,v) 
              (guard (and (eq? arr '=>) (not (assq arr env))))
              (let ([t (gensym)])
                `((case-lambda
                    [(,t) 
                     (if ,t (,(Expr v env) ,t) ,rest)])
                  ,(Expr e env)))]
             [(,e)
              (let ([t (gensym)])
                `((case-lambda
                    [(,t) 
                     (if ,t ,t ,rest)])
                  ,(Expr e env)))]
             [(,e ,e* ...)
              `(if ,(Expr e env)
                    (begin ,(map (lambda (x) (Expr x env)) e*) ...)
                    ,rest)]
             [,others (error 'cond "invalid clause" others)]))
         (define (last-clause x)
           (match x 
             [(,els ,e ,e* ...) 
              (guard (and (eq? els 'else) (not (assq els env))))
              `(begin ,(Expr e env) 
                      ,(map (lambda (x) (Expr x env)) e*)
                      ...)]
             [,others 
              (fst-clause others `(,(P 'void)))]))
         (let f ([cls* cls*] [ac (last-clause cls)]) 
           (cond
             [(null? cls*) ac]
             [else (fst-clause (car cls*) 
                     (f (cdr cls*) ac))])))]
      [(set! ,x ,[v]) 
       (cond
         [(assq x env) => (lambda (p) `(set! ,(cdr p) ,v))]
         [else (error 'fixup "unbound" x)])]
      [(foreign-call ,str ,[arg*] ...) 
       (guard (string? str))
       `(foreign-call ',str ,arg* ...)]
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      [,_ (error 'fixup "invalid expression" _)]))
  (Expr x '()))

(define-syntax add-tests-with-string-output 
  (lambda (x)
    (syntax-case x (=>)
      [(_ name [test => string] ...) 
       #'(set! all-tests
           (append all-tests 
              '([test string] ...)))])))

(begin
 (include "tests/tests-1.1-req.scm")
 (include "tests/tests-1.2-req.scm")
 (include "tests/tests-1.3-req.scm")
 (include "tests/tests-1.4-req.scm")
 (include "tests/tests-1.5-req.scm")
 (include "tests/tests-1.6-req.scm")
 (include "tests/tests-1.7-req.scm")
 (include "tests/tests-1.8-req.scm")
 (include "tests/tests-1.9-req.scm")
 (include "tests/tests-2.1-req.scm")
 (include "tests/tests-2.2-req.scm")
 (include "tests/tests-2.3-req.scm")
 (include "tests/tests-2.4-req.scm")
 (include "tests/tests-2.6-req.scm")
 (include "tests/tests-2.8-req.scm")
 (include "tests/tests-2.9-req.scm")
 )


(current-primitive-locations
  (lambda (x) 
    (define prims
      '(do-overflow 
        do-vararg-overflow
        error 
        $do-event
        $apply-nonprocedure-error-handler 
        $incorrect-args-error-handler 
        $multiple-values-error))
    (cond
      [(memq x prims) x]
      [else (error 'current-primloc "invalid" x)])))


(test-all)
(printf "Passed ~s tests\n" (length all-tests))
(printf "Happy Happy Joy Joy\n")

