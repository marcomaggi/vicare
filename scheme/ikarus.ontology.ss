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

(define-syntax define-ontology
  (lambda (x)
    (define (make-ontology main ls)
      (define (set-cons x ls)
        (cond
          [(memq x ls) ls]
          [else (cons x ls)]))
      (define (union ls1 ls2)
        (cond
          [(null? ls1) ls2]
          [else (union (cdr ls1) (set-cons (car ls1) ls2))]))
      (define (difference ls1 ls2)
        (cond
          [(null? ls1) '()]
          [(memq (car ls1) ls2) (difference (cdr ls1) ls2)]
          [else (cons (car ls1) (difference (cdr ls1) ls2))]))
      (define (collect-names ls)
        (syntax-case ls ()
          [() '()]
          [((name (of name* ...)) . rest)
           (union (cons #'name #'(name* ...)) (collect-names #'rest))]))
      (define (expand x all)
        (define (lookup x ls)
          (cond
            [(null? ls) (values 'tag '())]
            [else
             (let ([a (car ls)])
               (cond
                 [(eq? x (car a)) 
                  (values (cadr a) (cdr ls))]
                 [else
                  (let-values ([(xp ls) (lookup x (cdr ls))])
                    (values xp (cons a ls)))]))]))
        (let f ([x x] [ls ls])
          (let-values ([(xp ls) (lookup x ls)])
            (cond
              [(pair? xp)
               (cons (car xp) (map (lambda (x) (f x ls)) (cdr xp)))]
              [(eq? xp 'tag) x]
              [else (error 'expand-lookup "invalid" xp)]))))
      (define (rename alist x)
        (cond
          [(symbol? x) (cdr (assq x alist))]
          [else (cons (car x) (map (lambda (x) (rename alist x)) (cdr x)))]))
      (define (enumerate ls)
        (let f ([i 1] [ls ls])
          (cond
            [(null? ls) '()]
            [else (cons i (f (* i 2) (cdr ls)))])))
      (define (unique-elements x)
        (define (exclude m ls)
          (cond
            [(null? ls) '()]
            [(zero? (bitwise-and m (car ls)))
             (cons (car ls) (exclude m (cdr ls)))]
            [else (exclude m (cdr ls))]))
        (define (exclusive* m* x**)
          (cond
            [(null? (cdr m*)) (values (car m*) (car x**))]
            [else
             (let-values ([(m1 x1*) (values (car m*) (car x**))]
                          [(m2 x2*) (exclusive* (cdr m*) (cdr x**))])
               (let ([x1* (exclude m2 x1*)]
                     [x2* (exclude m1 x2*)])
                 (values (bitwise-ior m1 m2) (append x1* x2*))))]))
        (define (inclusive* m* x**)
          (cond
            [(null? (cdr m*)) (values (car m*) (car x**))]
            [else
             (let-values ([(m1 x1*) (values (car m*) (car x**))]
                          [(m2 x2*) (inclusive* (cdr m*) (cdr x**))])
               (values (bitwise-ior m1 m2)
                       (remp not
                         (apply append
                           (map (lambda (x)
                                  (map (lambda (y)
                                         (if (= (bitwise-and m1 m2 x)
                                                (bitwise-and m1 m2 y))
                                             (bitwise-ior x y)
                                             #f))
                                       x2*))
                                x1*)))))]))
        (define (f* ls)
          (cond
            [(null? ls) (values '() '())]
            [else
             (let-values ([(m x*) (f (car ls))]
                          [(m* x**) (f* (cdr ls))])
               (values (cons m m*) (cons x* x**)))]))
        (define (f x)
          (cond
            [(integer? x) (values x (list x))]
            [else
             (let ([tag (car x)] [ls (cdr x)])
               (let-values ([(m* x**) (f* ls)])
                 (case tag
                   [(exclusive) (exclusive* m* x**)]
                   [(inclusive) (inclusive* m* x**)]
                   [else (error 'f "invalid")])))]))
        (let-values ([(m ls) (f x)])
          ls))
      (define (expand-names alist)
        (lambda (n)
          (let f ([alist alist])
            (cond
              [(null? alist) '()]
              [(zero? (bitwise-and n (cdar alist)))
               (f (cdr alist))]
              [else 
               (cons (caar alist) (f (cdr alist)))]))))
      (define (extend-alist* ls alist)
        (define (extend-alist x alist)
          (define (lookup x)
            (cond
              [(assq x alist) => cdr]
              [else (error 'lookup "cannot find" x alist)]))
          (let ([name (car x)] [info (cadr x)])
            (let ([tag (car info)] [x* (map lookup (cdr info))])
              (case tag
                [(exclusive) 
                 (cons (cons name (apply bitwise-ior x*)) alist)]
                [(inclusive)
                 (assert (= (apply bitwise-ior x*) (apply bitwise-and x*)))
                 (cons (cons name (apply bitwise-ior x*)) alist)]
                [else (assert #f)]))))
        (cond
          [(null? ls) alist]
          [else
           (extend-alist (car ls)
             (extend-alist* (cdr ls) alist))]))
      (let* ([names (difference (collect-names ls) (map car ls))]
             [names-alist (map cons names (enumerate names))])
        (let* ([expanded (expand main ls)]
               [renamed (rename names-alist expanded)])
          (let* ([unique* (list-sort < (unique-elements renamed))]
                 [canonicals (map (expand-names names-alist) unique*)])
            (let* ([canonical-alist (map cons canonicals (enumerate canonicals))]
                   [seed-alist
                    (map 
                      (lambda (x) 
                        (let ([ls (filter (lambda (y) (memq x (car y))) canonical-alist)])
                          (cons x (apply bitwise-ior (map cdr ls)))))
                      names)])
              (extend-alist* ls seed-alist))))))
    (define (property-names ls)
      (cond
        [(null? ls) '()]
        [else
         (let ([fst (car ls)] [rest (property-names (cdr ls))])
           (let ([name (car fst)] [info (cadr fst)])
             (case (car info)
               [(exclusive) rest]
               [(inclusive) (append (cdr info) rest)]
               [else (assert #f)])))]))
    (define (generate-base-cases T main ls)
      (define (value-name x)
        (datum->syntax T
          (string->symbol
            (string-append 
              (symbol->string (syntax->datum T))
              ":" 
              (symbol->string x)))))
      (define (predicate-name x)
        (datum->syntax T
          (string->symbol
            (string-append 
              (symbol->string (syntax->datum T))
              ":" 
              (symbol->string x)
              "?"))))
      (let ([maind (syntax->datum main)] [lsd (syntax->datum ls)])
        (let ([alist (make-ontology maind lsd)]
              [pnames (property-names lsd)])
          (let ([alist (remp (lambda (x) (memq (car x) pnames)) alist)])
            (map 
              (lambda (x) (list (value-name (car x))
                                (predicate-name (car x))
                                (cdr x)))
              alist)))))
    (syntax-case x ()
      [(_ T T:description T? T:=? T:and T:or [name cls] [name* cls*] ...)
       (with-syntax ([((name* predname* val*) ...)
                      (generate-base-cases #'T #'name
                        #'([name cls] [name* cls*] ...))])
         #'(begin
             (define-record-type (T make-T T?)
               (sealed     #t)
               (fields (immutable n T-n)))
             (define (T:and x0 x1)
               (make-T (bitwise-and (T-n x0) (T-n x1))))
             (define (T:or x0 x1)
               (make-T (bitwise-ior (T-n x0) (T-n x1))))
             (define (test x v)
               (let ([bits (bitwise-and x v)])
                 (cond
                   [(= 0 (bitwise-and x v))  'no]
                   [(= v (bitwise-ior x v)) 'yes]
                   [else                  'maybe])))
             (define name* (make-T val*)) ...
             (define (predname* x) (test (T-n x) val*)) ...
             (define (T:description x)
               (let* ([ls '()]
                      [ls
                       (case (predname* x)
                         [(yes) (cons 'name* ls)]
                         [else  ls])]
                      ...)
                 ls))
             (define (T:=? x y)
               (= (T-n x) (T-n y)))
             ))])))

(define-ontology T T:description T? T=? T:and T:or
  [object        (inclusive obj-tag obj-immediacy obj-truth)]
  [obj-immediacy (exclusive nonimmediate immediate)]
  [immediate     (exclusive fixnum boolean null char void)]
  [obj-truth     (exclusive false non-false)]
  [obj-tag       (exclusive procedure string vector pair null
                            boolean char number void bytevector 
                            symbol other-object)]
  [boolean       (exclusive true false)]
  [number        (inclusive number-tag number-size number-exactness)]
  [number-size   (exclusive negative zero positive)]
  [number-tag    (exclusive fixnum flonum other-number)]
  [number-exactness (exclusive exact inexact)]
  [exact         (exclusive fixnum other-exact)]
  [inexact       (exclusive flonum other-inexact)]
  )

#!eof

(define (do-test expr result expected)
  (if (equal? result expected)
      (printf "OK: ~s -> ~s\n" expr expected)
      (error 'test "failed/got/expected" expr result expected)))

(define-syntax test
  (syntax-rules ()
    [(_ expr expected) (do-test 'expr expr 'expected)]))

(test (T:object? T:object) yes)
(test (T:object? T:true)   yes)
(test (T:true? T:object)   maybe)
(test (T:true? T:true)     yes)
(test (T:true? T:false)    no)
(test (T:true? T:null)     no)
(test (T:non-false? T:true) yes)
(test (T:non-false? T:null) yes)
(test (T:non-false? T:false) no)
(test (T:non-false? T:boolean) maybe)
(test (T:non-false? T:object) maybe)
(test (T:boolean? T:true) yes)
(test (T:boolean? T:false) yes)
(test (T:boolean? (T:or T:true T:false)) yes)
(test (T:boolean? (T:and T:true T:false)) no)
(test (T:object? (T:and T:true T:false)) no)



