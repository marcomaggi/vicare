;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2008,2009  Abdulaziz Ghuloum
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



(library (ikarus.symbol-table)
  (export string->symbol initialize-symbol-table! $symbol-table-size)
  (import 
    (except (ikarus) string->symbol)
    (except (ikarus system $symbols) $symbol-table-size))

  (define-struct symbol-table (length mask vec guardian))
  
  (define (extend-table st)
    (let* ([v1 (symbol-table-vec st)]
           [n1 (vector-length v1)]
           [n2 (+ n1 n1)]
           [mask (- n2 1)]
           [v2 (make-vector n2 '())])
      (define (insert p)
        (unless (null? p)
          (let ([a (car p)] [rest (cdr p)])
            (let ([idx (fxand (symbol-hash a) mask)])
              (set-cdr! p (vector-ref v2 idx))
              (vector-set! v2 idx p))
            (insert rest))))
      (vector-for-each insert v1)
      (set-symbol-table-vec! st v2)
      (set-symbol-table-mask! st mask)))
  
  (define intern-symbol!
    (case-lambda
      [(s idx st)
       (let ([v (symbol-table-vec st)])
         (vector-set! v idx (weak-cons s (vector-ref v idx)))
         ((symbol-table-guardian st) s)
         (let ([n (fx+ (symbol-table-length st) 1)])
           (set-symbol-table-length! st n)
           (when (fx=? n (symbol-table-mask st))
             (extend-table st))))]
      [(s st) 
       (intern-symbol! s 
         (fxand (symbol-hash s) (symbol-table-mask st))
         st)]))
  
  (define (intern str idx st)
    (let ([s ($make-symbol str)])
      ($set-symbol-unique-string! s #f)
      (intern-symbol! s idx st)
      s))
  
  (define (unintern x st)
    (let ([n (fx- (symbol-table-length st) 1)])
      (set-symbol-table-length! st n))
    (let ([idx (fxand (symbol-hash x) (symbol-table-mask st))]
          [v (symbol-table-vec st)])
      (let ([ls (vector-ref v idx)])
        (cond
          [(eq? (car ls) x)
           (vector-set! v idx (cdr ls))]
          [else
           (let f ([prev ls] [ls (cdr ls)])
             (cond
               [(eq? (car ls) x)
                (set-cdr! prev (cdr ls))]
               [else (f ls (cdr ls))]))]))))

   (define (dead? sym)
    (and ($unbound-object? ($symbol-value sym))
         (null? ($symbol-plist sym))))

  (define (bleed-guardian sym st)
    (let ([g (symbol-table-guardian st)])
      (cond
        [(g) => 
         (lambda (a)
           (let loop ([a a])
             (cond
               [(eq? a sym) (g a)]
               [(begin
                  (if (dead? a) (unintern a st) (g a))
                  (g)) => loop])))]))
    sym)
  
  (define (chain-lookup str idx st ls)
    (if (null? ls)
        (let ([sym (intern str idx st)])
          ;;; doesn't need eq? check there
          (bleed-guardian sym st))
        (let ([a (car ls)])
          (if (string=? str (symbol->string a))
              (bleed-guardian a st)
              (chain-lookup str idx st (cdr ls))))))
  
  (define (lookup str ih st)
    (let ([idx (fxand ih (symbol-table-mask st))])
      (let ([v (symbol-table-vec st)])
        (chain-lookup str idx st (vector-ref v idx)))))
  
  
  (module (string->symbol initialize-symbol-table! $symbol-table-size)
    (define st (make-symbol-table 0 3 (make-vector 4 '()) (make-guardian)))
    (define ($symbol-table-size)
      (symbol-table-length st))
    (define (string->symbol x)
      (if (string? x)
          (lookup x (string-hash x) st)
          (die 'string->symbol "not a string" x)))
    (define (initialize-symbol-table!)
      (define (f x)
        (when (pair? x)
          (intern-symbol! (car x) st) 
          (f (cdr x))))
      (vector-for-each f (foreign-call "ikrt_get_symbol_table")))))

#!eof
  
(define (gen-list i n)
  (let f ([i i])
    (if (= i n)
        '()
        (let ([x (string->symbol (format "s~a" i))])
          (cons x (f (+ i 1)))))))

(initialize-symbol-table!)


(time
  (let ()
    (gen-list 0 1000)
    (gen-list 0 1000)
    (time (do ((i 0 (+ i 1))) ((= i 1000)) (collect)))
    (gen-list 0 1000)
    (gen-list 1000 1001)
;    (gen-list 1000 2000)
    #f))



