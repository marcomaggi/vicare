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


(library (ikarus.pretty-formats)
  (export get-fmt pretty-format)
  (import (except (ikarus) pretty-format))

  (define h (make-eq-hashtable))

  (define (get-fmt name)
    (hashtable-ref h name #f))
  
  (define (set-fmt! name fmt)
    (hashtable-set! h name fmt))

  (define pretty-format
    (lambda (x)
      (unless (symbol? x)
        (die 'pretty-format "not a symbol" x))
      (case-lambda
        [() (hashtable-ref h x #f)]
        [(v) (hashtable-set! h x v)])))

  ;;; standard formats
  (set-fmt! 'quote '(read-macro . "'"))
  (set-fmt! 'unquote '(read-macro . ","))
  (set-fmt! 'unquote-splicing '(read-macro . ",@"))
  (set-fmt! 'quasiquote '(read-macro . "`"))
  (set-fmt! 'syntax '(read-macro . "#'"))
  (set-fmt! 'quasisyntax '(read-macro . "#`"))
  (set-fmt! 'unsyntax '(read-macro . "#,"))
  (set-fmt! 'unsyntax-splicing '(read-macro . "#,@"))
  ;(set-fmt! '|#primitive| '(read-macro . "#%"))
  (set-fmt! 'let '(alt 
                    (_ (0 [e 0 e] ...) tab e ...)
                    (_ x (0 [e 0 e] ...) tab e ...)))
  (set-fmt! 'letrec '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'letrec* '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'let-syntax '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'letrec-syntax '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'let* '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'let-values '(_ (0 [e 0 e] ...) tab e tab e* ...))
  (set-fmt! 'cond '(_ tab [0 e ...] ...))
  (set-fmt! 'define '(_ name tab e ...))
  (set-fmt! 'set! '(_ name tab e))
  (set-fmt! 'case-lambda 
     '(_ tab [0 e ...] ...))
  (set-fmt! 'struct-case 
     '(_ e tab [e 0 e ...] ...))
  (set-fmt! 'if '(_ test 3 e ...))
  (set-fmt! 'and '(and test 4 e ...))
  (set-fmt! 'or '(or test 3 e ...))
  (set-fmt! 'begin '(_ tab e ...))
  (set-fmt! 'lambda '(_ fmls tab e tab e* ...))
  (set-fmt! 'case '(_ e tab [e 0 e] ...))
  (set-fmt! 'syntax-rules '(_ kwd* tab [e 0 e] ...))
  (set-fmt! 'syntax-case '(_ expr kwd*  
                             tab (e 0 e 0 e ...) ...))
  (set-fmt! 'module '(alt (_ (fill ...) tab e ...)
                          (_ name (fill ...) tab e ...)))
  (set-fmt! 'library '(_ name tab e ...))
  (set-fmt! 'import '(_ tab e ...))

)


#!eof

(define (test x)
  (pretty-print x)
  (printf "====================================\n"))

(test 12)
(test #t)
(test #f)
(test (if #f #f))
(test '())
(test "string")
(test "\n")
(test "\r")
(test (string (integer->char 0)))
(test (string (integer->char 240)))
(test 'hello)
(test '(foo bar))
(test '
  (define pp 
    (case-lambda
      [(x) (pretty x (current-output-port))]
      [(x p)
       (if (output-port? p)
           (pretty x p)
           (die 'pretty-print "not an output port" p))])))

(test '(384 7384 83947 893478 9137489 3894789 134789314 79817238
        97314897 318947138974 981374 89137489 1374897 13498713
        894713894 137894 89137489 1374 891348314 12 17 9000000 . 17))

(test '(',,@#''(quote (syntax unquote-splicing . 2) 2)))

(test '#(1 2 3))

(test '#(384 7384 83947 893478 9137489 3894789 134789314 79817238
         97314897 318947138974 981374 89137489 1374897 13498713
         894713894 137894 89137489))


(define (test-file x)
  (printf "testing file ~s ...\n" x)
  (with-input-from-file x 
    (lambda ()
      (let f ([i 0])
        (let ([x (read)] [fname (format "tmp.~a.pp" i)])
          (unless (eof-object? x)
            (let ([y
                   (begin
                     (call-with-output-file fname
                        (lambda (p) 
                          (pretty-print x p))
                        'replace)
                     (with-input-from-file fname read))])
              (if (equal? x y)
                  (f (fxadd1 i))
                  (die 'test-file "mismatch" x y)))))))))

