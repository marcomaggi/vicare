;;;SRFI 101: Purely Functional Random-Access Pairs and Lists
;;;Copyright (c) David Van Horn 2009.  All Rights Reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission notice  shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  REMEMBER, THERE  IS  NO SCHEME  UNDERGROUND. IN  NO
;;;EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;;;DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;;OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
;;;THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;;This test suite has been successfully run on Ikarus (0.0.3),
;;;Larceny (0.97), and PLT Scheme (4.2.1.7).

;;;To run:
;;;   cp srfi-101.sls srfi/%3A101.sls
;;;   ikarus --r6rs-script srfi-101-tests.sls
;;;   larceny -r6rs -path . -program srfi-101-tests.sls
;;;   plt-r6rs ++path . srfi-101-tests.sls


#!r6rs
(import (except (rnrs base)
                quote pair? cons car cdr
                caar cadr cddr cdar
                caaar caadr caddr cadar
                cdaar cdadr cdddr cddar
                caaaar caaadr caaddr caadar
                cadaar cadadr cadddr caddar
                cdaaar cdaadr cdaddr cdadar
                cddaar cddadr cddddr cdddar
                null? list? list length
                append reverse list-tail
                list-ref map for-each)
  (prefix (rnrs base) r6:)
  (rnrs exceptions)
  (srfi :101)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 101, functional pairs\n")


;;;; helpers

(define (check-expect c e)
  (if (pair? c)
      (and (pair? e)
	   (check-expect (car c) (car e))
	   (check-expect (cdr c) (cdr e)))
    (equal? c e)))

(define-syntax check-error
  (syntax-rules ()
    ((_ e)
     (check
	 (let ((f (cons 0 0)))
	   (guard (g ((eq? f g)
		      #f)
		     (else 'OK))
	     (begin
	       e
	       (raise f))))
       => 'OK))))


;;;; quote

(check
    (let ((f (lambda () '(x))))
      (eq? (f) (f)))
  (=> check-expect)
  #t)

(check
    '(1 2 3)
  (=> check-expect)
  (list 1 2 3))


;;;; pair?

(check
    (pair? (cons 'a 'b))
  (=> check-expect)
  #t)

(check
    (pair? (list 'a 'b 'c))
  (=> check-expect)
  #t)

(check
    (pair? '())
  (=> check-expect)
  #f)

(check
    (pair? '#(a b))
  (=> check-expect)
  #f)


;;;; cons

(check
    (cons 'a '())
  (=> check-expect)
  (list 'a))

(check
    (cons (list 'a) (list 'b 'c 'd))
  (=> check-expect)
  (list (list 'a) 'b 'c 'd))

(check
    (cons "a" (list 'b 'c))
  (=> check-expect)
  (list "a" 'b 'c))

(check
    (cons 'a 3)
  (=> check-expect)
  (cons 'a 3))

(check
    (cons (list 'a 'b) 'c)
  (=> check-expect)
  (cons (list 'a 'b) 'c))


;;;; car

(check
    (car (list 'a 'b 'c))
  (=> check-expect)
  'a)

(check
    (car (list (list 'a) 'b 'c 'd))
  (=> check-expect)
  (list 'a))

(check
    (car (cons 1 2))
  (=> check-expect)
  1)

(check-error
 (car '()))


;;;; cdr

(check
    (cdr (list (list 'a) 'b 'c 'd))
  (=> check-expect)
  (list 'b 'c 'd))

(check
    (cdr (cons 1 2))
  (=> check-expect)
  2)

(check-error (cdr '()))


;;;; null?

(check
    (eq? null? r6:null?)
  (=> check-expect)
  #t)

(check
    (null? '())
  (=> check-expect)
  #t)

(check
    (null? (cons 1 2))
  (=> check-expect)
  #f)

(check
    (null? 4)
  (=> check-expect)
  #f)


;;;; list?

(check
    (list? (list 'a 'b 'c))
  (=> check-expect)
  #t)

(check
    (list? '())
  (=> check-expect)
  #t)

(check
    (list? (cons 'a 'b))
  (=> check-expect)
  #f)


;;;; list

(check
    (list 'a (+ 3 4) 'c)
  (=> check-expect)
  (list 'a 7 'c))

(check
    (list)
  (=> check-expect)
  '())


;;;; make-list

(check
    (length (make-list 5))
  (=> check-expect)
  5)

(check
    (make-list 5 0)
  (=> check-expect)
  (list 0 0 0 0 0))


;;;; length

(check
    (length (list 'a 'b 'c))
  (=> check-expect)
  3)

(check
    (length (list 'a (list 'b) (list 'c)))
  (=> check-expect)
  3)

(check
    (length '())
  (=> check-expect)
  0)


;;;; append

(check
    (append (list 'x) (list 'y))
  (=> check-expect)
  (list 'x 'y))

(check
    (append (list 'a) (list 'b 'c 'd))
  (=> check-expect)
  (list 'a 'b 'c 'd))

(check
    (append (list 'a (list 'b)) (list (list 'c)))
  (=> check-expect)
  (list 'a (list 'b) (list 'c)))

(check
    (append (list 'a 'b) (cons 'c 'd))
  (=> check-expect)
  (cons 'a (cons 'b (cons 'c 'd))))

(check
    (append '() 'a)
  (=> check-expect)
  'a)


;;;; reverse

(check
    (reverse (list 'a 'b 'c))
  (=> check-expect)
  (list 'c 'b 'a))

(check
    (reverse (list 'a (list 'b 'c) 'd (list 'e (list 'f))))
  (=> check-expect)
  (list (list 'e (list 'f)) 'd (list 'b 'c) 'a))


;;;; list-tail

(check
    (list-tail (list 'a 'b 'c 'd) 2)
  (=> check-expect)
  (list 'c 'd))


;;;; list-ref

(check
    (list-ref (list 'a 'b 'c 'd) 2)
  (=> check-expect)
  'c)


;;;; list-set

(check
    (list-set (list 'a 'b 'c 'd) 2 'x)
  (=> check-expect)
  (list 'a 'b 'x 'd))


;;;; list-ref/update

(let-values (((a b)
              (list-ref/update (list 7 8 9 10) 2 -)))
  (check
      a
    (=> check-expect)
    9)
  (check
      b
    (=> check-expect)
    (list 7 8 -9 10)))


;;;; map

(check
    (map cadr (list (list 'a 'b) (list 'd 'e) (list 'g 'h)))
  (=> check-expect)
  (list 'b 'e 'h))

(check
    (map (lambda (n) (expt n n))
      (list 1 2 3 4 5))
  (=> check-expect)
  (list 1 4 27 256 3125))

(check
    (map + (list 1 2 3) (list 4 5 6))
  (=> check-expect)
  (list 5 7 9))


;;;; for-each

(check
    (let ((v (make-vector 5)))
      (for-each (lambda (i)
		  (vector-set! v i (* i i)))
	(list 0 1 2 3 4))
      v)
  (=> check-expect)
  '#(0 1 4 9 16))


;;;; random-access-list->linear-access-list
;;;; linear-access-list->random-access-list

(check
    (random-access-list->linear-access-list '())
  (=> check-expect)
  '())

(check
    (linear-access-list->random-access-list '())
  (=> check-expect)
  '())

(check
    (random-access-list->linear-access-list (list 1 2 3))
  (=> check-expect)
  (r6:list 1 2 3))

(check
    (linear-access-list->random-access-list (r6:list 1 2 3))
  (=> check-expect)
  (list 1 2 3))


;;;; done

(check-report)

;;; end of file
