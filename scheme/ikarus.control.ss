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



(library (ikarus control)
  (export call/cf call/cc call-with-current-continuation dynamic-wind exit)
  (import 
    (ikarus system $stack)
    (except (ikarus) call/cf call/cc call-with-current-continuation
            dynamic-wind exit list-tail))

  (define primitive-call/cf
    (lambda (f)
      (if ($fp-at-base)
          (f ($current-frame))
          ($seal-frame-and-call f))))
 
  (define call/cf
    (lambda (f)
      (if (procedure? f)
          (primitive-call/cf f)
          (die 'call/cf "not a procedure" f))))

  (define primitive-call/cc
    (lambda (f)
      (primitive-call/cf
        (lambda (frm)
          (f ($frame->continuation frm))))))

  (define winders '())

  (define len
    (lambda (ls n)
      (if (null? ls)
          n
          (len (cdr ls) (fxadd1 n)))))

  (define list-tail
    (lambda (ls n)
      (if (fxzero? n)
          ls
          (list-tail (cdr ls) (fxsub1 n)))))

  (define drop-uncommon-heads 
    (lambda (x y)
      (if (eq? x y)
          x
          (drop-uncommon-heads (cdr x) (cdr y)))))

  (define common-tail
    (lambda (x y)
      (let ([lx (len x 0)] [ly (len y 0)])
        (let ([x (if (fx> lx ly) (list-tail x (fx- lx ly)) x)]
              [y (if (fx> ly lx) (list-tail y (fx- ly lx)) y)])
          (if (eq? x y)
              x
              (drop-uncommon-heads (cdr x) (cdr y)))))))

  (define unwind*
    (lambda (ls tail)
      (unless (eq? ls tail)
        (set! winders (cdr ls))
        ((cdar ls))
        (unwind* (cdr ls) tail))))

  (define rewind*
    (lambda (ls tail)
      (unless (eq? ls tail)
        (rewind* (cdr ls) tail)
        ((caar ls))
        (set! winders ls))))

  (define do-wind
    (lambda (new)
      (let ([tail (common-tail new winders)])
        (unwind* winders tail)
        (rewind* new tail))))

  (define call/cc
    (lambda (f)
      (unless (procedure? f) 
        (die 'call/cc "not a procedure" f))
      (primitive-call/cc
        (lambda (k)
          (let ([save winders])
            (f (case-lambda
                 [(v) (unless (eq? save winders) (do-wind save)) (k v)]
                 [()  (unless (eq? save winders) (do-wind save)) (k)]
                 [(v1 v2 . v*)
                  (unless (eq? save winders) (do-wind save))
                  (apply k v1 v2 v*)])))))))

  (define call-with-current-continuation
    (lambda (f)
      (unless (procedure? f) 
        (die 'call-with-current-continuation
            "not a procedure" f))
      (call/cc f)))

  (define dynamic-wind
    (lambda (in body out)
      (unless (procedure? in)
        (die 'dynamic-wind "not a procedure" in))
      (unless (procedure? body)
        (die 'dynamic-wind "not a procedure" body))
      (unless (procedure? out)
        (die 'dynamic-wind "not a procedure" out))
      (in)
      (set! winders (cons (cons in out) winders))
      (call-with-values
        body
        (case-lambda
          [(v) (set! winders (cdr winders)) (out) v]
          [()  (set! winders (cdr winders)) (out) (values)]
          [(v1 v2 . v*)
           (set! winders (cdr winders))
           (out)
           (apply values v1 v2 v*)]))))
  
  (define exit
    (case-lambda
      [() (exit 0)]
      [(status) (foreign-call "ikrt_exit" status)]))
  )
