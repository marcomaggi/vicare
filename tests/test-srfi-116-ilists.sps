;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 116
;;;Date: Fri Jun 12, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) John Cowan 2014.  All Rights Reserved.
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is  hereby granted, free  of charge, to any  person obtaining a  copy of
;;;this software and associated documentation files (the ``Software''), to deal in the
;;;Software without restriction, including without limitation the rights to use, copy,
;;;modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
;;;and to permit  persons to whom the Software  is furnished to do so,  subject to the
;;;following conditions:
;;;
;;;The above  copyright notice  and this  permission notice shall  be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED ``AS  IS'',  WITHOUT WARRANTY  OF ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING  BUT NOT LIMITED  TO THE WARRANTIES OF  MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND  NONINFRINGEMENT.  IN NO  EVENT SHALL THE  AUTHORS OR
;;;COPYRIGHT HOLDERS BE  LIABLE FOR ANY CLAIM, DAMAGES OR  OTHER LIABILITY, WHETHER IN
;;;AN ACTION  OF CONTRACT, TORT  OR OTHERWISE, ARISING FROM,  OUT OF OR  IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


#!r6rs
(import (vicare)
  (srfi :114)
  (srfi :116)
  (srfi :116 comparators)
  (srfi :116 quotations)
  (vicare containers iteration-thunks)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: SRFI 116, immutable lists\n")


;;;; helpers

(define-constant numbers (iq 0 1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------

(define-syntax test-group
  (syntax-rules ()
    ((_ ?name . ?body)
     (begin . ?body))
    ))

(define iequal? equal?)

;;Immutable pairs are a built-in type for Vicare, so EQUAL? recognises them.
;;
;; (define (iequal? a b)
;;   (cond ((ipair? a)
;; 	 (and (ipair? b)
;; 	      (iequal? (icar a) (icar b))
;; 	      (iequal? (icdr a) (icdr b))))
;; 	((pair? a)
;; 	 (and (pair? b)
;; 	      (iequal? (car a) (car b))
;; 	      (iequal? (cdr a) (cdr b))))
;; 	((vector? a)
;; 	 (and (vector? b)
;; 	      (fx=? (vector-length a)
;; 		    (vector-length b))
;; 	      (vector-for-all iequal? a b)))
;; 	(else
;; 	 (equal? a b))))

(define current-test-comparator
  (make-parameter iequal?))

(define-syntax test
  (syntax-rules ()
    ((_ ?expected ?form)
     (check ?form (=> (current-test-comparator)) ?expected))
    ((_ ?dummy ?expected ?form)
     (check ?form (=> (current-test-comparator)) ?expected))
    ))

(define-syntax test-assert
  (syntax-rules ()
    ((_ ?form)
     (check-for-true ?form))
    ))

(define-syntax test-error
  (syntax-rules ()
    ((_ ?form)
     (check
	 (guard (E ((error? E)
		    #t)
		   (else E))
	   ?form)
       => #t))
    ))

(define-syntax test-procedure-argument-violation
  (syntax-rules ()
    ((_ ?form)
     (check
	 (guard (E ((procedure-argument-violation? E)
		    #t)
		   (else E))
	   ?form)
       => #t))
    ))



(test-group "ilists"

(test-group "ilists/constructors"
  (define abc (ilist 'a 'b 'c))
  (test 'a (icar abc))
  (test 'b (icadr abc))
  (test 'c (icaddr abc))
  (test (ipair 2 1) (xipair 1 2))
  (define abc-dot-d (ipair* 'a 'b 'c 'd))
  (test 'd (icdddr abc-dot-d))
  (test (iq c c c c) (make-ilist 4 'c))
  (test (iq 0 1 2 3) (ilist-tabulate 4 values))
  (test (iq 0 1 2 3 4) (iiota 5))
) ; end ilists/constructors

(test-group "ilists/predicates"
  (test-assert (ipair? (ipair 1 2)))
  (test-assert (proper-ilist? '()))
  (test-assert (proper-ilist? (iq 1 2 3)))
  (test-assert (ilist? '()))
  (test-assert (ilist? (iq 1 2 3)))
  (test-assert (dotted-ilist? (ipair 1 2)))
  (test-assert (dotted-ilist? 2))
  (test-assert (null-ilist? '()))
  (test-assert (not (null-ilist? (iq 1 2 3))))
  (test-error (null-ilist? 'a))
  (test-assert (not-ipair? 'a))
  (test-assert (not (not-ipair? (ipair 'a 'b))))
  (test-assert (ilist= = (iq 1 2 3) (iq 1 2 3)))
  (test-assert (not (ilist= = (iq 1 2 3 4) (iq 1 2 3))))
  (test-assert (not (ilist= = (iq 1 2 3) (iq 1 2 3 4))))
  (test-assert (ilist= = (iq 1 2 3) (iq 1 2 3)))
  (test-assert (not (ilist= = (iq 1 2 3) (iq 1 2 3 4) (iq 1 2 3 4))))
  (test-assert (not (ilist= = (iq 1 2 3) (iq 1 2 3) (iq 1 2 3 4))))
) ; end ilist/predicates

(test-group "ilist/cxrs"
  (define ab (ipair 'a 'b))
  (define cd (ipair 'c 'd))
  (define ef (ipair 'e 'f))
  (define gh (ipair 'g 'h))
  (define abcd (ipair ab cd))
  (define efgh (ipair ef gh))
  (define abcdefgh (ipair abcd efgh))
  (define ij (ipair 'i 'j))
  (define kl (ipair 'k 'l))
  (define mn (ipair 'm 'n))
  (define op (ipair 'o 'p))
  (define ijkl (ipair ij kl))
  (define mnop (ipair mn op))
  (define ijklmnop (ipair ijkl mnop))
  (define abcdefghijklmnop (ipair abcdefgh ijklmnop))
  (test 'a (icaar abcd))
  (test 'b (icdar abcd))
  (test 'c (icadr abcd))
  (test 'd (icddr abcd))
  (test 'a (icaaar abcdefgh))
  (test 'b (icdaar abcdefgh))
  (test 'c (icadar abcdefgh))
  (test 'd (icddar abcdefgh))
  (test 'e (icaadr abcdefgh))
  (test 'f (icdadr abcdefgh))
  (test 'g (icaddr abcdefgh))
  (test 'h (icdddr abcdefgh))
  (test 'a (icaaaar abcdefghijklmnop))
  (test 'b (icdaaar abcdefghijklmnop))
  (test 'c (icadaar abcdefghijklmnop))
  (test 'd (icddaar abcdefghijklmnop))
  (test 'e (icaadar abcdefghijklmnop))
  (test 'f (icdadar abcdefghijklmnop))
  (test 'g (icaddar abcdefghijklmnop))
  (test 'h (icdddar abcdefghijklmnop))
  (test 'i (icaaadr abcdefghijklmnop))
  (test 'j (icdaadr abcdefghijklmnop))
  (test 'k (icadadr abcdefghijklmnop))
  (test 'l (icddadr abcdefghijklmnop))
  (test 'm (icaaddr abcdefghijklmnop))
  (test 'n (icdaddr abcdefghijklmnop))
  (test 'o (icadddr abcdefghijklmnop))
  (test 'p (icddddr abcdefghijklmnop))
) ; end ilists/cxrs

(test-group "ilists/selectors"
  (test 'c (ilist-ref (iq a b c d) 2))
  (define ten (ilist 1 2 3 4 5 6 7 8 9 10))
  (test 1 (ifirst ten))
  (test 2 (isecond ten))
  (test 3 (ithird ten))
  (test 4 (ifourth ten))
  (test 5 (ififth ten))
  (test 6 (isixth ten))
  (test 7 (iseventh ten))
  (test 8 (ieighth ten))
  (test 9 (ininth ten))
  (test 10 (itenth ten))
  (test-procedure-argument-violation (ilist-ref '() 2))
  (test '(1 2) (call-with-values (lambda () (icar+icdr (ipair 1 2))) list))
  (define abcde (iq a b c d e))
  (define dotted (ipair 1 (ipair 2 (ipair 3 'd))))
  (test (iq a b) (itake abcde 2))
  (test (iq c d e) (idrop abcde 2))
  (test (iq c d e) (ilist-tail abcde 2))
  (test (iq 1 2) (itake dotted 2))
  (test (ipair 3 'd) (idrop dotted 2))
  (test (ipair 3 'd) (ilist-tail dotted 2))
  (test 'd (idrop dotted 3))
  (test 'd (ilist-tail dotted 3))
  (test abcde (iappend (itake abcde 4) (idrop abcde 4)))
  (test (iq d e) (itake-right abcde 2))
  (test (iq a b c) (idrop-right abcde 2))
  (test (ipair 2 (ipair 3 'd)) (itake-right dotted 2))
  (test (iq 1) (idrop-right dotted 2))
  (test 'd (itake-right dotted 0))
  (test (iq 1 2 3) (idrop-right dotted 0))
  (test abcde (call-with-values (lambda () (isplit-at abcde 3)) iappend))
  (test 'c (ilast (iq a b c)))
  (test (iq c) (last-ipair (iq a b c)))
) ; end ilists/selectors

(test-group "ilists/misc"
  (test 0 (ilength '()))
  (test 3 (ilength (iq 1 2 3)))
  (test (iq x y) (iappend (iq x) (iq y)))
  (test (iq a b c d) (iappend (iq a b) (iq c d)))
  (test (iq a) (iappend '() (iq a)))
  (test (iq x y) (iappend (iq x y)))
  (test '() (iappend))
  (test (iq a b c d) (iconcatenate (iq (a b) (c d))))
  (test (iq c b a) (ireverse (iq a b c)))
  (test (iq (e (f)) d (b c) a) (ireverse (iq a (b c) d (e (f)))))
  (test (ipair 2 (ipair 1 'd)) (iappend-reverse (iq 1 2) 'd))
  (test (iq (one 1 odd) (two 2 even) (three 3 odd))
    (izip (iq one two three) (iq 1 2 3) (iq odd even odd)))
  (test (iq (1) (2) (3)) (izip (iq 1 2 3)))
  (test (iq 1 2 3) (iunzip1 (iq (1) (2) (3))))
  (test (iq (1 2 3) (one two three))
    (call-with-values
      (lambda () (iunzip2 (iq (1 one) (2 two) (3 three))))
      ilist))
  (test (iq (1 2 3) (one two three) (a b c))
    (call-with-values
      (lambda () (iunzip3 (iq (1 one a) (2 two b) (3 three c))))
      ilist))
  (test (iq (1 2 3) (one two three) (a b c) (4 5 6))
    (call-with-values
      (lambda () (iunzip4 (iq (1 one a 4) (2 two b 5) (3 three c 6))))
      ilist))
  (test (iq (1 2 3) (one two three) (a b c) (4 5 6) (#t #f #t))
    (call-with-values
      (lambda () (iunzip5 (iq (1 one a 4 #t) (2 two b 5 #f) (3 three c 6 #t))))
      ilist))
  (test 3 (icount even? (iq 3 1 4 1 5 9 2 5 6)))
  (test 3 (icount < (iq 1 2 4 8) (iq 2 4 6 8 10 12 14 16)))
) ; end ilists/misc

(test-group "ilists/folds"
  ;; We have to be careful to test both single-list and multiple-list
  ;; code paths, as they are different in this implementation.

  (define lis (iq 1 2 3))
  (test 6 (ifold + 0 lis))
  (test (iq 3 2 1) (ifold ipair '() lis))
  (test 2 (ifold
            (lambda (x count) (if (symbol? x) (+ count 1) count))
            0
            (iq a 0 b)))
  (test 4 (ifold
            (lambda (s max-len) (max max-len (string-length s)))
            0
            (iq "ab" "abcd" "abc")))
  (test 32 (ifold
             (lambda (a b ans) (+ (* a b) ans))
             0
             (iq 1 2 3)
             (iq 4 5 6)))
  (define (z x y ans) (ipair (ilist x y) ans))
  (test (iq (b d) (a c))
    (ifold z '() (iq a b) (iq c d)))
  (test lis (ifold-right ipair '() lis))
  (test (iq 0 2 4) (ifold-right
                   (lambda (x l) (if (even? x) (ipair x l) l))
                   '()
                   (iq 0 1 2 3 4)))
  (test (iq (a c) (b d))
    (ifold-right z '() (iq a b) (iq c d)))
  (test (iq (c) (b c) (a b c))
    (ipair-fold ipair '() (iq a b c)))
  (test (iq ((b) (d)) ((a b) (c d)))
    (ipair-fold z '() (iq a b) (iq c d)))
  (test (iq (a b c) (b c) (c))
    (ipair-fold-right ipair '() (iq a b c)))
  (test (iq ((a b) (c d)) ((b) (d)))
    (ipair-fold-right z '() (iq a b) (iq c d)))
  (test 5 (ireduce max 0 (iq 1 3 5 4 2 0)))
  (test 1 (ireduce - 0 (iq 1 2)))
  (test -1 (ireduce-right - 0 (iq 1 2)))
  (define squares (iq 1 4 9 16 25 36 49 64 81 100))
  (test squares
   (iunfold (lambda (x) (> x 10))
     (lambda (x) (* x x))
     (lambda (x) (+ x 1))
     1))
  (test squares
    (iunfold-right zero?
      (lambda (x) (* x x))
      (lambda (x) (- x 1))
      10))
  (test (iq 1 2 3) (iunfold null-ilist? icar icdr (iq 1 2 3)))
  (test (iq 3 2 1) (iunfold-right null-ilist? icar icdr (iq 1 2 3)))
  (test (iq 1 2 3 4)
    (iunfold null-ilist? icar icdr (iq 1 2) (lambda (x) (iq 3 4))))
  (test (iq b e h) (imap icadr (iq (a b) (d e) (g h))))
  (test (iq b e h) (imap-in-order icadr (iq (a b) (d e) (g h))))
  (test (iq 5 7 9) (imap + (iq 1 2 3) (iq 4 5 6)))
  (test (iq 5 7 9) (imap-in-order + (iq 1 2 3) (iq 4 5 6)))
  (let ((z (let ((count 0)) (lambda (ignored) (set! count (+ count 1)) count))))
    (test (iq 1 2) (imap-in-order z (iq a b))))
  (test '#(0 1 4 9 16)
    (let ((v (make-vector 5)))
      (ifor-each (lambda (i)
                  (vector-set! v i (* i i)))
                (iq 0 1 2 3 4))
    v))
  (test '#(5 7 9 11 13)
    (let ((v (make-vector 5)))
      (ifor-each (lambda (i j)
                  (vector-set! v i (+ i j)))
                (iq 0 1 2 3 4)
                (iq 5 6 7 8 9))
    v))
  (test (iq 1 -1 3 -3 8 -8)
    (iappend-map (lambda (x) (ilist x (- x))) (iq 1 3 8)))
  (test (iq 1 4 2 5 3 6)
    (iappend-map ilist (iq 1 2 3) (iq 4 5 6)))
  (test (vector (iq 0 1 2 3 4) (iq 1 2 3 4) (iq 2 3 4) (iq 3 4) (iq 4))
    (let ((v (make-vector 5)))
      (ipair-for-each (lambda (lis) (vector-set! v (icar lis) lis)) (iq 0 1 2 3 4))
    v))
  (test (vector (iq 5 6 7 8 9) (iq 6 7 8 9) (iq 7 8 9) (iq 8 9) (iq 9))
    (let ((v (make-vector 5)))
      (ipair-for-each (lambda (i j) (vector-set! v (icar i) j))
                (iq 0 1 2 3 4)
                (iq 5 6 7 8 9))
    v))
  (test (iq 1 9 49)
    (ifilter-map (lambda (x) (and (number? x) (* x x))) (iq a 1 b 3 c 7)))
  (test (iq 5 7 9)
    (ifilter-map
      (lambda (x y) (and (number? x) (number? y) (+ x y)))
      (iq 1 a 2 b 3 4)
      (iq 4 0 5 y 6 z)))
) ; end ilists/folds

(test-group "ilists/filtering"
  (test (iq 0 8 8 -4) (ifilter even? (iq 0 7 8 8 43 -4)))
  (test (list (iq one four five) (iq 2 3 6))
    (call-with-values
      (lambda () (ipartition symbol? (iq one 2 3 four five 6)))
      list))
  (test (iq 7 43) (iremove even? (iq 0 7 8 8 43 -4)))
) ; end ilists/filtering

(test-group "ilists/searching"
  (test 2 (ifind even? (iq 1 2 3)))
  (test #t (iany  even? (iq 1 2 3)))
  (test #f (ifind even? (iq 1 7 3)))
  (test #f (iany  even? (iq 1 7 3)))
  (test-error (ifind even? (ipair 1 (ipair 3 'x))))
  (test-error (iany  even? (ipair 1 (ipair 3 'x))))
  (test 4 (ifind even? (iq 3 1 4 1 5 9)))
  (test (iq -8 -5 0 0) (ifind-tail even? (iq 3 1 37 -8 -5 0 0)))
  (test (iq 2 18) (itake-while even? (iq 2 18 3 10 22 9)))
  (test (iq 3 10 22 9) (idrop-while even? (iq 2 18 3 10 22 9)))
  (test (list (iq 2 18) (iq 3 10 22 9))
    (call-with-values
      (lambda () (ispan even? (iq 2 18 3 10 22 9)))
      list))
  (test (list (iq 3 1) (iq 4 1 5 9))
    (call-with-values
      (lambda () (ibreak even? (iq 3 1 4 1 5 9)))
      list))
  (test #t (iany integer? (iq a 3 b 2.7)))
  (test #f (iany integer? (iq a 3.1 b 2.7)))
  (test #t (iany < (iq 3 1 4 1 5) (iq 2 7 1 8 2)))
  (test #t (ievery integer? (iq 1 2 3 4 5)))
  (test #f (ievery integer? (iq 1 2 3 4.5 5)))
  (test #t (ievery < (iq 1 2 3) (iq 4 5 6)))
  (test 2 (ilist-index even? (iq 3 1 4 1 5 9)))
  (test 1 (ilist-index < (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2)))
  (test #f (ilist-index = (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2)))
  (test (iq a b c) (imemq 'a (iq a b c)))
  (test (iq b c) (imemq 'b (iq a b c)))
  (test #f (imemq 'a (iq b c d)))
  (test #f (imemq (ilist 'a) (iq b (a) c)))
  (test (iq (a) c) (imember (ilist 'a) (iq b (a) c)))
  (test (iq 101 102) (imemv 101 (iq 100 101 102)))
) ; end ilists/searching

(test-group "ilists/deletion"
  (test (iq 1 2 4 5) (idelete 3 (iq 1 2 3 4 5)))
  (test (iq 3 4 5) (idelete 5 (iq 3 4 5 6 7) <))
  (test (iq a b c z) (idelete-duplicates (iq a b a c a b c z)))
) ; end ilists/deletion

(test-group "ilists/alists"
  (define e (iq (a 1) (b 2) (c 3))) (test (iq a 1) (iassq 'a e))
  (test (iq b 2) (iassq 'b e))
  (test #f (iassq 'd e))
  (test #f (iassq (ilist 'a) (iq ((a)) ((b)) ((c)))))
  (test (iq (a)) (iassoc (ilist 'a) (iq ((a)) ((b)) ((c)))))
  (define e2 (iq (2 3) (5 7) (11 13)))
  (test (iq 5 7) (iassv 5 e2))
  (test (iq 11 13) (iassoc 5 e2 <))
  (test (ipair (iq 1 1) e2) (ialist-cons 1 (ilist 1) e2))
  (test (iq (2 3) (11 13)) (ialist-delete 5 e2))
  (test (iq (2 3) (5 7)) (ialist-delete 5 e2 <))
) ; end ilists/alists

(test-group "ilists/replacers"
  (test (ipair 1 3) (replace-icar (ipair 2 3) 1))
  (test (ipair 1 3) (replace-icdr (ipair 1 2) 3))
) ; end ilists/replacers

(test-group "ilists/conversion"
  (test (ipair 1 2) (pair->ipair '(1 . 2)))
  (test '(1 . 2) (ipair->pair (ipair 1 2)))
  (test (iq 1 2 3) (list->ilist '(1 2 3)))
  (test '(1 2 3) (ilist->list (iq 1 2 3)))
  (test (ipair 1 (ipair 2 3)) (list->ilist '(1 2 . 3)))
  (test '(1 2 . 3) (ilist->list (ipair 1 (ipair 2 3))))
  (test (ipair (ipair 1 2) (ipair 3 4)) (tree->itree '((1 . 2) . (3 . 4))))
  (test '((1 . 2) . (3 . 4)) (itree->tree (ipair (ipair 1 2) (ipair 3 4))))
  (test (ipair (ipair 1 2) (ipair 3 4)) (gtree->itree (cons (ipair 1 2) (ipair 3 4))))
  (test '((1 . 2) . (3 . 4)) (gtree->tree (cons (ipair 1 2) (ipair 3 4))))
  (test 6 (iapply + (iq 1 2 3)))
  (test 15 (iapply + 1 2 (iq 3 4 5)))
) ; end ilists/conversion

) ; end ilists


(parametrise ((check-test-name 'constructors))

  (check
      (ipair* 1 2 3 4 (iq 5 6 7 8))
    (=> iequal?) (iq 1 2 3 4 5 6 7 8))

;;; --------------------------------------------------------------------

  (check
      (xipair 1 2)
    (=> iequal?) (iq 2 . 1))

;;; --------------------------------------------------------------------

  (check
      (make-ilist 4 'c)
    (=> iequal?) (iq c c c c))

  (check
      (make-ilist 0)
    (=> iequal?) (iq ))

  (check
      (make-ilist 0 #f)
    (=> iequal?) (iq ))

;;; --------------------------------------------------------------------

  (check
      (ilist-tabulate 4 (lambda (i)
			  (ipair i 'a)))
    (=> iequal?) (iq (0 . a)
		     (1 . a)
		     (2 . a)
		     (3 . a)))

  (check
      (ilist-tabulate 1 (lambda (i)
			  (ipair i 'a)))
    (=> iequal?) (iq (0 . a)))

  (check
      (ilist-tabulate 0 (lambda (i)
			  (ipair i 'a)))
    (=> iequal?) (iq ))

;;; --------------------------------------------------------------------

  (check
      (ilist-copy (iq ))
    (=> iequal?) (iq ))

  (check
      (ilist-copy (iq 1))
    (=> iequal?) (iq 1))

  (check
      (ilist-copy numbers)
    (=> iequal?) numbers)

  (check
      (ilist-copy (iq 1 . 2))
    (=> iequal?) (iq 1 . 2))

  (check
      (ilist-copy (iq 1 2 3 . 4))
    (=> iequal?) (iq 1 2 3 . 4))

;;; --------------------------------------------------------------------

  (check
      (iiota 5 10)
    (=> iequal?) (iq 10 11 12 13 14))

  (check
      (iiota 5)
    (=> iequal?) (iq 0 1 2 3 4))

  (check
      (iiota 5 10 5)
    (=> iequal?) (iq 10 15 20 25 30))

  (check
      (guard (exc (else #t))
	(iiota -5 10 5))
    (=> iequal?) #t)

  #f)


(parametrise ((check-test-name 'kind-predicates))

  (check
      (ilist? (iq ))
    (=> iequal?) #t)

  (check
      (ilist? (iq 1 2 3))
    (=> iequal?) #t)

  (check
      (ilist? (iq 1 2 3 . 4))
    (=> iequal?) #f)

  (check
      (ilist? (iq 1 . 2))
    (=> iequal?) #f)

  (check
      (ilist? 123)
    (=> iequal?) #f)

  (check
      (ilist? #\a)
    (=> iequal?) #f)

  (check
      (ilist? 'alpha)
    (=> iequal?) #f)

;;; --------------------------------------------------------------------

  (check
      (dotted-ilist? (iq ))
    (=> iequal?) #f)

  (check
      (dotted-ilist? (iq 1 2 3))
    (=> iequal?) #f)

  (check
      (dotted-ilist? (iq 1 . 2))
    (=> iequal?) #t)

  (check
      (dotted-ilist? (iq 1 2 3 . 4))
    (=> iequal?) #t)

  (check
      (dotted-ilist? 123)
    (=> iequal?) #t)

  (check
      (dotted-ilist? #\a)
    (=> iequal?) #t)

  (check
      (dotted-ilist? 'alpha)
    (=> iequal?) #t)

;;; --------------------------------------------------------------------

  (check
      (ipair? (iq 1 2))
    (=> iequal?) #t)

  (check
      (ipair? (iq 1 . 2))
    (=> iequal?) #t)

  (check
      (ipair? (iq 1))
    (=> iequal?) #t)

  (check
      (ipair? (iq ))
    (=> iequal?) #f)

  (check
      (ipair? 1)
    (=> iequal?) #f)

  #t)


(parametrise ((check-test-name 'comparison))

  (check
      (ilist= =)
    (=> iequal?) #t)

  (check
      (ilist= = numbers)
    (=> iequal?) #t)

  (check
      (ilist= = numbers numbers)
    (=> iequal?) #t)

  (check
      (ilist= = numbers numbers numbers)
    (=> iequal?) #t)

  (check
      (ilist= = numbers numbers numbers numbers)
    (=> iequal?) #t)

;;; --------------------------------------------------------------------

  (check
      (ilist= =
	      (iq 1 2 3 4)
	      (iq 1 9 3 4))
    (=> iequal?) #f)

  (check
      (ilist= =
	      (iq 1 2 3)
	      (iq 9 2 3))
    (=> iequal?) #f)

  (check
      (ilist= =
	      (iq 1 2 3)
	      (iq 9 2 3)
	      (iq 9 2 3))
    (=> iequal?) #f)
  (check
      (ilist= =
	      (iq 9 2 3)
	      (iq 1 2 3)
	      (iq 9 2 3))
    (=> iequal?) #f)

  (check
      (ilist= =
	      (iq 9 2 3)
	      (iq 9 2 3)
	      (iq 1 2 3))
    (=> iequal?) #f)

;;; --------------------------------------------------------------------

  (check
      (ilist= = (iq 1))
    (=> iequal?) #t)

  (check
      (ilist= = (iq 1) (iq 1))
    (=> iequal?) #t)

  (check
      (ilist= = (iq 1) (iq 1) (iq 1))
    (=> iequal?) #t)

  (check
      (ilist= = (iq 1) (iq 1) (iq 1) (iq 1))
    (=> iequal?) #t)

;;; --------------------------------------------------------------------

  (check
      (ilist= = (iq 1) (iq 1 2))
    (=> iequal?) #f)

  (check
      (ilist= = (iq 1 2) (iq 1))
    (=> iequal?) #f)

  (check
      (ilist= = (iq 1 2) (iq 1) (iq 1))
    (=> iequal?) #f)

  (check
      (ilist= = (iq 1) (iq 1 2) (iq 1))
    (=> iequal?) #f)

  (check
      (ilist= = (iq 1) (iq 1) (iq 1 2))
    (=> iequal?) #f)

  (check
      (ilist= = numbers (iq 0 1 2 3 4))
    (=> iequal?) #f)

;;; --------------------------------------------------------------------

  (check
      (ilist= = (iq ))
    (=> iequal?) #t)

  (check
      (ilist= = (iq ) (iq ))
    (=> iequal?) #t)

  (check
      (ilist= = (iq ) (iq ) (iq ))
    (=> iequal?) #t)

  (check
      (ilist= = (iq ) (iq ) (iq ) (iq ))
    (=> iequal?) #t)

;;; --------------------------------------------------------------------

  (check
      (ilist= = (iq ) numbers)
    (=> iequal?) #f)

  (check
      (ilist= = numbers (iq ))
    (=> iequal?) #f)

  (check
      (ilist= = numbers (iq ) (iq ))
    (=> iequal?) #f)

  (check
      (ilist= = (iq ) numbers (iq ))
    (=> iequal?) #f)

  (check
      (ilist= = (iq ) (iq ) numbers)
    (=> iequal?) #f)

  #f)


(parametrise ((check-test-name 'selectors))

  (check (ifirst numbers)	(=> iequal?) 0)
  (check (isecond numbers)	(=> iequal?) 1)
  (check (ithird numbers)	(=> iequal?) 2)
  (check (ifourth numbers)	(=> iequal?) 3)
  (check (ififth numbers)	(=> iequal?) 4)
  (check (isixth numbers)	(=> iequal?) 5)
  (check (iseventh numbers)	(=> iequal?) 6)
  (check (ieighth numbers)	(=> iequal?) 7)
  (check (ininth numbers)	(=> iequal?) 8)
  (check (itenth numbers)	(=> iequal?) 9)

;;; --------------------------------------------------------------------

  (check
      (ilist-ref numbers 0)
    (=> iequal?) 0)

  (check
      (ilist-ref numbers 3)
    (=> iequal?) 3)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (icar+icdr numbers))
	ilist)
    (=> iequal?)
    (ilist (icar numbers) (icdr numbers)))

;;; --------------------------------------------------------------------

  (check
      (itake (iq ) 0)
    (=> iequal?) (iq ))

  (check
      (itake numbers 0)
    (=> iequal?) (iq ))

  (check
      (itake numbers 5)
    (=> iequal?) (iq 0 1 2 3 4))

  (check
      (itake numbers 10)
    (=> iequal?) numbers)

  (check
      (iappend (itake numbers 3)
	       (idrop numbers 3))
    (=> iequal?) numbers)

;;; --------------------------------------------------------------------

  (check
      (idrop-right numbers 5)
    (=> iequal?) (iq 0 1 2 3 4))

  (check
      (idrop-right numbers 0)
    (=> iequal?) numbers)

  (check
      (idrop-right (iq ) 0)
    (=> iequal?) (iq ))

  (check
      (idrop-right numbers 10)
    (=> iequal?) (iq ))

  (check
      (iappend (idrop-right numbers 3)
	       (itake-right numbers 3))
    (=> iequal?) numbers)

;;; --------------------------------------------------------------------

  (check
      (itake-right numbers 5)
    (=> iequal?) (iq 5 6 7 8 9))

  (check
      (itake-right numbers 0)
    (=> iequal?) (iq ))

  (check
      (itake-right (iq ) 0)
    (=> iequal?) (iq ))

  (check
      (itake-right numbers 10)
    (=> iequal?) numbers)

;;; --------------------------------------------------------------------

  (check
      (idrop-right numbers 5)
    (=> iequal?) (iq 0 1 2 3 4))

  (check
      (idrop-right numbers 0)
    (=> iequal?) numbers)

  (check
      (idrop-right (iq ) 0)
    (=> iequal?) (iq ))

  (check
      (idrop-right numbers 10)
    (=> iequal?) (iq ))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (isplit-at numbers 5))
	ilist)
    (=> iequal?) (iq (0 1 2 3 4)
		     (5 6 7 8 9)))

;;; --------------------------------------------------------------------

  (check
      (ilast numbers)
    (=> iequal?) 9)

  (check
      (ilast (iq 9))
    (=> iequal?) 9)

;;; This raises an error.
  ;;
  ;; (check
  ;;     (last (iq ))
  ;;   (=> iequal?) (iq ))

;;; --------------------------------------------------------------------

  (check
      (last-ipair numbers)
    (=> iequal?) (iq 9))

  (check
      (last-ipair (iq 9))
    (=> iequal?) (iq 9))

;;; The empty list is not a pair, so the following raises an error.
  ;;
  ;; (check
  ;;     (last-pair (iq ))
  ;;   (=> iequal?) (iq ))

  #f)


(parametrise ((check-test-name 'miscellaneous))

  (check
      (ilength (iq 1 2 3 4 5 6))
    (=> iequal?) 6)

  (check
      (ilength (iq 1))
    (=> iequal?) 1)

  (check
      (ilength (iq ))
    (=> iequal?) 0)

;;; --------------------------------------------------------------------

  (check
      (iappend (iq x) (iq y))
    (=> iequal?) (iq x y))

  (check
      (iappend (iq a) (iq b c d))
    (=> iequal?) (iq a b c d))

  (check
      (iappend (iq a (b)) (iq (c)))
    (=> iequal?) (iq a (b) (c)))

  (check
      (iappend (iq a b) (iq c . d))
    (=> iequal?) (iq a b c . d))

  (check
      (iappend (iq ) 'a)
    (=> iequal?) 'a)

  (check
      (iappend (iq a) (iq ))
    (=> iequal?) (iq a))

  (check
      (iappend (iq x y))
    (=> iequal?) (iq x y))

  (check
      (iappend)
    (=> iequal?) (iq ))

;;; --------------------------------------------------------------------

  (check
      (iconcatenate (iq ))
    (=> iequal?) (iq ))

  (check
      (iconcatenate (iq ()))
    (=> iequal?) (iq ))

  (check
      (iconcatenate (iq () ()))
    (=> iequal?) (iq ))

  (check
      (iconcatenate (iq () () ()))
    (=> iequal?) (iq ))

  (check
      (iconcatenate (iq (x)))
    (=> iequal?) (iq x))

  (check
      (iconcatenate (iq (x) (y)))
    (=> iequal?) (iq x y))

  (check
      (iconcatenate (iq (x) (y) (z)))
    (=> iequal?) (iq x y z))

  (check
      (iconcatenate (iq (a)
			(b c d)))
    (=> iequal?) (iq a b c d))

  (check
      (iconcatenate (iq (a b)
			(c d)))
    (=> iequal?) (iq a b c d))

  (check
      (iconcatenate (iq (a b)
			(c d)
			(e f)))
    (=> iequal?) (iq a b c d e f))

  (check
      (iconcatenate (iq (a b c d e f g)
			(h i)
			(l m n o)))
    (=> iequal?) (iq a b c d e f g h i l m n o))

  (check
      (iconcatenate (iq (a (b)) ((c))))
    (=> iequal?) (iq a (b) (c)))

  (check
      (iconcatenate (iq (a b) (c . d)))
    (=> iequal?) (iq a b c . d))

  (check
      (iconcatenate (iq () (a)))
    (=> iequal?) (iq a))

  (check
      (iconcatenate (iq (x y)))
    (=> iequal?) (iq x y))

;;; --------------------------------------------------------------------

  (check
      (ireverse (iq ))
    (=> iequal?) (iq ))

  (check
      (ireverse (iq 1))
    (=> iequal?) (iq 1))

  (check
      (ireverse (iq 1 2 3))
    (=> iequal?) (iq 3 2 1))

;;; --------------------------------------------------------------------

  (check
      (iappend-reverse (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (iappend-reverse (iq x) (iq y))
    (=> iequal?) (iq x y))

  (check
      (iappend-reverse (iq 1 2 3) (iq 4 5 6))
    (=> iequal?) (iq 3 2 1 4 5 6))

  (check
      (iappend-reverse (iq a) (iq b c d))
    (=> iequal?) (iq a b c d))

  (check
      (iappend-reverse (iq a (b)) (iq (c)))
    (=> iequal?) (iq (b) a (c)))

  (check
      (iappend-reverse (iq a) (iq ))
    (=> iequal?) (iq a))

;;; --------------------------------------------------------------------

  (check
      (izip (iq))
    (=> iequal?) (iq))

  (check
      (izip (iq one two three)
	    (iq 1 2 3)
	    (iq odd even odd))
    (=> iequal?) (iq (one 1 odd) (two 2 even) (three 3 odd)))

  (check
      (izip (iq 1 2 3))
    (=> iequal?) (iq (1) (2) (3)))

  (check	;unequal length
      (izip (iq one two three)
	    (iq 1 2 3)
	    (iq odd even odd even odd even odd even))
    (=> iequal?) (iq (one 1 odd) (two 2 even) (three 3 odd)))

  (check
      (izip (iq 1 2 3))
    (=> iequal?) (iq (1) (2) (3)))

;;; --------------------------------------------------------------------

  (check
      (iunzip1 (iq (1)))
    (=> iequal?) (iq 1))

  (check
      (iunzip1 (iq (1)
		   (2)))
    (=> iequal?) (iq 1 2))

  (check
      (iunzip1 (iq (1)
		   (2)
		   (3)))
    (=> iequal?) (iq 1 2 3))

  (check
      (iunzip1 (iq (1 one)
		   (2 two)
		   (3 three)))
    (=> iequal?) (iq 1 2 3))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (iunzip2 (iq (1 one))))
	ilist)
    (=> iequal?) (iq (1)
		     (one)))

  (check
      (call-with-values
	  (lambda ()
	    (iunzip2 (iq (1 one)
			 (2 two))))
	ilist)
    (=> iequal?) (iq (1 2)
		     (one two)))

  (check
      (call-with-values
	  (lambda ()
	    (iunzip2 (iq (1 one)
			 (2 two)
			 (3 three))))
	ilist)
    (=> iequal?) (iq (1 2 3)
		     (one two three)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (iunzip3 (iq (1 10 100)
			 (2 20 200)
			 (3 30 300))))
	ilist)
    (=> iequal?) (iq (1 2 3)
		     (10 20 30)
		     (100 200 300)))

  (check
      (call-with-values
	  (lambda ()
	    (iunzip4 (iq (1 10 100 1000)
			 (2 20 200 2000)
			 (3 30 300 3000))))
	ilist)
    (=> iequal?) (iq (1 2 3)
		     (10 20 30)
		     (100 200 300)
		     (1000 2000 3000)))

  (check
      (call-with-values
	  (lambda ()
	    (iunzip5 (iq (1 10 100 1000 10000)
			 (2 20 200 2000 20000)
			 (3 30 300 3000 30000))))
	ilist)
    (=> iequal?) (iq (1 2 3)
		     (10 20 30)
		     (100 200 300)
		     (1000 2000 3000)
		     (10000 20000 30000)))


;;; --------------------------------------------------------------------

  (check
      (icount even? numbers)
    (=> iequal?) 5)

  (check
      (icount even? (iq 1))
    (=> iequal?) 0)

  (check
      (icount even? (iq 2))
    (=> iequal?) 1)

  (check
      (icount even? (iq ))
    (=> iequal?) 0)

  #f)


(parametrise ((check-test-name 'left-folding))

  (check
      (ifold + 0 numbers)
    (=> iequal?) 45)

  (check
      (ifold ipair (iq ) numbers)
    (=> iequal?) (iq 9 8 7 6 5 4 3 2 1 0))

  (check
      (ifold ipair (iq 4 5 6) (iq 3 2 1))
    (=> iequal?) (iq 1 2 3 4 5 6))

  (check
      (ifold ipair (iq 4 5 6) (iq ))
    (=> iequal?) (iq 4 5 6))

  (check
      (ifold ipair (iq 4 5 6) (iq 3))
    (=> iequal?) (iq 3 4 5 6))

  (check
      (ifold (lambda (x count)
	       (if (symbol? x)
		   (+ count 1)
		 count))
	     0
	     (iq a 1 b 2 c 3))
    (=> iequal?) 3)

  (check
      (ifold (lambda (s len)
	       (max len (string-length s)))
	     0
	     (iq "ciao" "hello" "salut" "hola"))
    (=> iequal?) 5)

  (check
      (ifold ipair* (iq )
	     (iq a b c)
	     (iq 1 2 3 4 5))
    (=> iequal?) (iq c 3 b 2 a 1))

  (check
      (ifold ipair* (iq )
	     (iq a)
	     (iq 1))
    (=> iequal?) (iq a 1))

  (check
      (ifold (lambda (a b c knil)
	       (ipair (ilist a b c)
		      knil))
	     (iq )
	     (iq 1 2 3)
	     (iq 10 20 30)
	     (iq 100 200 300))
    (=> iequal?) (iq (3 30 300)
		     (2 20 200)
		     (1 10 100)))

  (check
      (ifold (lambda (a b c knil)
	       (ipair (ilist a b c)
		      knil))
	     (iq )
	     (iq 1 2 3)
	     (iq 10 20)
	     (iq 100 200 300 400))
    (=> iequal?) (iq (2 20 200)
		     (1 10 100)))

  (check
      (ifold iappend
	     (iq )
	     (iq (1 2 3)
		 (4 5)
		 (6 7 8 9)
		 (0)))
    (=> iequal?) (iq 0 6 7 8 9 4 5 1 2 3))

  #f)


(parametrise ((check-test-name 'right-folding))

  (check
      (ifold-right ipair (iq ) (iq 1 2 3))
    (=> iequal?) (iq 1 2 3))

  (check
      (ifold-right ipair (iq ) numbers)
    (=> iequal?) numbers)

  (check
      (ifold-right + 0 numbers)
    (=> iequal?) 45)

  (check
      (ifold-right ipair (iq 4 5 6) (iq 1 2 3))
    (=> iequal?) (iq 1 2 3 4 5 6))

  (check
      (ifold-right (lambda (x count)
		     (if (symbol? x)
			 (+ count 1)
		       count))
		   0
		   (iq a 1 b 2 c 3))
    (=> iequal?) 3)

  (check
      (ifold-right (lambda (s len)
		     (max len (string-length s)))
		   0
		   (iq "ciao" "hello" "salut" "hola"))
    (=> iequal?) 5)

  (check
      (ifold-right (lambda (x l)
		     (if (even? x)
			 (ipair x l)
		       l))
		   (iq )
		   (iq 0 1 2 3 4 5 6 7 8 9))
    (=> iequal?) (iq 0 2 4 6 8))

  (check
      (ifold-right ipair* (iq )
		   (iq a b c)
		   (iq 1 2 3 4 5))
    (=> iequal?) (iq a 1 b 2 c 3))

  (check
      (ifold-right ipair* (iq )
		   (iq a)
		   (iq 1))
    (=> iequal?) (iq a 1))

  (check
      (ifold-right (lambda (a b c knil)
		     (ipair (ilist a b c)
			    knil))
		   (iq )
		   (iq 1 2 3)
		   (iq 10 20 30)
		   (iq 100 200 300))
    (=> iequal?) (iq (1 10 100)
		     (2 20 200)
		     (3 30 300)))

  (check
      (ifold-right (lambda (a b c knil)
		     (ipair (ilist a b c)
			    knil))
		   (iq )
		   (iq 1 2 3)
		   (iq 10 20)
		   (iq 100 200 300 400))
    (=> iequal?) (iq (1 10 100)
		     (2 20 200)))

;;; --------------------------------------------------------------------

  (check
      (ifold-right ipair (iq ) (iq 1 2 3))
    (=> iequal?) (iq 1 2 3))

  (check
      (ifold-right ipair (iq 1 2 3) (iq ))
    (=> iequal?) (iq 1 2 3))

  (check
      (ifold-right ipair (iq 1 2 3) (iq 9))
    (=> iequal?) (iq 9 1 2 3))

  (check
      (ifold-right ipair (iq ) numbers)
    (=> iequal?) numbers)

  (check
      (ifold-right + 0 numbers)
    (=> iequal?) 45)

  (check
      (ifold-right ipair (iq 4 5 6) (iq 1 2 3))
    (=> iequal?) (iq 1 2 3 4 5 6))

  (check
      (ifold-right (lambda (x count)
		     (if (symbol? x)
			 (+ count 1)
		       count))
		   0
		   (iq a 1 b 2 c 3))
    (=> iequal?) 3)

  (check
      (ifold-right (lambda (s len)
		     (max len (string-length s)))
		   0
		   (iq "ciao" "hello" "salut" "hola"))
    (=> iequal?) 5)

  (check
      (ifold-right (lambda (x l)
		     (if (even? x)
			 (ipair x l)
		       l))
		   (iq )
		   (iq 0 1 2 3 4 5 6 7 8 9))
    (=> iequal?) (iq 0 2 4 6 8))

  (check
      (ifold-right ipair* (iq )
		   (iq a b c)
		   (iq 1 2 3))
    (=> iequal?) (iq a 1 b 2 c 3))

  (check
      (ifold-right ipair* (iq )
		   (iq a)
		   (iq 1))
    (=> iequal?) (iq a 1))

  (check
      (ifold-right (lambda (a b c knil)
		     (ipair (ilist a b c)
			    knil))
		   (iq )
		   (iq 1 2 3)
		   (iq 10 20 30)
		   (iq 100 200 300))
    (=> iequal?) (iq (1 10 100)
		     (2 20 200)
		     (3 30 300)))

  (check
      (ifold-right (lambda (a b c knil)
		     (ipair (ilist a b c)
			    knil))
		   (iq )
		   (iq 1 2 3)
		   (iq 10 20)
		   (iq 100 200 300 400))
    (=> iequal?) (iq (1 10 100)
		     (2 20 200)))

  #f)


(parametrise ((check-test-name 'pair-folding))

  (check
      (ipair-fold (lambda (elm knil)
		    (ipair (icar elm) knil))
		  (iq 999)
		  (iq 1 2 3))
    (=> iequal?) (iq 3 2 1 999))

;;; --------------------------------------------------------------------

  (check
      (ipair-fold (lambda (a b c knil)
		    (ipair (ilist (icar a)
				  (icar b)
				  (icar c))
			   knil))
		  (iq 999)
		  (iq 1 2 3)
		  (iq 10 20 30)
		  (iq 100 200 300))
    (=> iequal?) (iq (3 30 300)
		     (2 20 200)
		     (1 10 100)
		     999))

  (check
      (ipair-fold (lambda (a b c knil)
		    (ipair (ilist (icar a)
				  (icar b)
				  (icar c))
			   knil))
		  (iq 999)
		  (iq 1)
		  (iq 10)
		  (iq 100))
    (=> iequal?) (iq (1 10 100)
		     999))

  (check
      (ipair-fold (lambda (a b c knil)
		    (ipair (ilist (icar a)
				  (icar b)
				  (icar c))
			   knil))
		  (iq 999)
		  (iq 1)
		  (iq 10 20 30)
		  (iq 100 200 300))
    (=> iequal?) (iq (1 10 100)
		     999))

  (check
      (ipair-fold (lambda (a b c knil)
		    (ipair (ilist (icar a)
				  (icar b)
				  (icar c))
			   knil))
		  (iq 999)
		  (iq 1 2 3)
		  (iq 10)
		  (iq 100 200 300))
    (=> iequal?) (iq (1 10 100)
		     999))
  (check
      (ipair-fold (lambda (a b c knil)
		    (ipair (ilist (icar a)
				  (icar b)
				  (icar c))
			   knil))
		  (iq 999)
		  (iq 1 2 3)
		  (iq 10 20 30)
		  (iq 100))
    (=> iequal?) (iq (1 10 100)
		     999))

  (check
      (ipair-fold (lambda (a b c knil)
		    (ipair (ilist (icar a)
				  (icar b)
				  (icar c))
			   knil))
		  (iq 999)
		  (iq )
		  (iq 10 20 30)
		  (iq 100 200 300))
    (=> iequal?) (iq 999))

  (check
      (ipair-fold (lambda (a b c knil)
		    (ipair (ilist (icar a)
				  (icar b)
				  (icar c))
			   knil))
		  (iq 999)
		  (iq 1 2 3)
		  (iq )
		  (iq 100 200 300))
    (=> iequal?) (iq 999))

  (check
      (ipair-fold (lambda (a b c knil)
		    (ipair (ilist (icar a)
				  (icar b)
				  (icar c))
			   knil))
		  (iq 999)
		  (iq 1 2 3)
		  (iq 10 20 30)
		  (iq ))
    (=> iequal?) (iq 999))

  #f)


(parametrise ((check-test-name 'reducing))

  (check
      (ireduce + 0 numbers)
    (=> iequal?) 45)

  (check
      (ireduce + 0 (iq ))
    (=> iequal?) 0)

  (check
      (ireduce max 0 (iq 1 2 3 4 5))
    (=> iequal?) 5)

  (check
      (ireduce iappend
	       (iq )
	       (iq (1 2 3)
		   (4 5)
		   (6 7 8 9)
		   (0)))
    (=> iequal?) (iq 0 6 7 8 9 4 5 1 2 3))

;;; --------------------------------------------------------------------

  (check
      (ireduce-right + 0 numbers)
    (=> iequal?) 45)

  (check
      (ireduce-right + 0 (iq ))
    (=> iequal?) 0)

  (check
      (ireduce-right max 0 (iq 1 2 3 4 5))
    (=> iequal?) 5)

  (check
      (ireduce-right iappend
		     (iq )
		     (iq (1 2 3)
			 (4 5)
			 (6 7 8 9)
			 (0)))
    (=> iequal?) (iq 1 2 3 4 5 6 7 8 9 0))

  #f)


(parametrise ((check-test-name 'unfolding))

  (check
      (iunfold (lambda (x) (< 5 x))
	       (lambda (x) (* x x))
	       (lambda (x) (+ x 1))
	       1)
    (=> iequal?) (iq 1 4 9 16 25))

  (check
      (iunfold (lambda (x) (< 5 x))
	       (lambda (x) (* x x))
	       (lambda (x) (+ x 1))
	       1
	       (lambda (x) (- x)))
    (=> iequal?) (iq 1 4 9 16 25 . -6))

  (check
      (iunfold (lambda (x) #t)
	       (lambda (x) (* x x))
	       (lambda (x) (+ x 1))
	       1
	       (lambda (x) (- x)))
    (=> iequal?) -1)

  (check
      (iunfold null? icar icdr numbers)
    (=> iequal?) numbers)

  (check
      (iunfold (lambda (obj)
		 (not (ipair? obj)))
	       icar icdr (iq 1 2 3 4 . 5) values)
    (=> iequal?) (iq 1 2 3 4 . 5))

  (check
      (iunfold null? icar icdr (iq 1 2 3) (lambda (x) (iq 4 5 6)))
    (=> iequal?) (iq 1 2 3 4 5 6))

;;; --------------------------------------------------------------------

  (check
      (iunfold-right zero?
		     (lambda (x) (* x x))
		     (lambda (x) (- x 1))
		     5)
    (=> iequal?) (iq 1 4 9 16 25))

  (check
      (iunfold-right null? icar icdr (iq 1 2 3 4 5))
    (=> iequal?) (iq 5 4 3 2 1))

  (check
      (iunfold-right null? icar icdr (iq 3 2 1) (iq 4 5 6))
    (=> iequal?) (iq 1 2 3 4 5 6))

  #f)


(parametrise ((check-test-name 'mapping))

  (check
      (imap - (iq ))
    (=> iequal?) (iq ))

  (check
      (imap - (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (imap - (iq ) (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (imap - numbers)
    (=> iequal?) (iq 0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (imap +
	    (iq 1 2 3)
	    (iq 10 20 30))
    (=> iequal?) (iq 11 22 33))

  (check
      (imap +
	    (iq 1 2 3)
	    (iq 10 20 30)
	    (iq 100 200 300))
    (=> iequal?) (iq 111 222 333))

;;; --------------------------------------------------------------------

  (check
      (let ((r 0))
	(ifor-each
	 (lambda (e)
	   (set! r (+ e r)))
	 (iq ))
	r)
    (=> iequal?) 0)

  (check
      (let ((r 0))
	(ifor-each
	 (lambda (e1 e2)
	   (set! r (+ e1 e2 r)))
	 (iq ) (iq ))
	r)
    (=> iequal?) 0)

  (check
      (let ((r 0))
	(ifor-each
	 (lambda (e1 e2 e3)
	   (set! r (+ e1 e2 e3 r)))
	 (iq ) (iq ) (iq ))
	r)
    (=> iequal?) 0)

  (check
      (let ((r (iq 0 0)))
	(ifor-each
	 (lambda (e1 e2)
	   (set! r (ilist (+ e1 (icar r))
			  (+ e2 (icadr r)))))
	 (iq 1 10 100)
	 (iq 2 20 200))
	r)
    (=> iequal?) (iq 111 222))


  (check
      (let ((r (iq 0 0 0)))
	(ifor-each
	 (lambda (e1 e2 e3)
	   (set! r (ilist (+ e1 (icar r))
			  (+ e2 (icadr r))
			  (+ e3 (icaddr r)))))
	 (iq 1 10 100)
	 (iq 2 20 200)
	 (iq 3 30 300))
	r)
    (=> iequal?) (iq 111 222 333))

;;; --------------------------------------------------------------------

  (check
      (imap - (iq ))
    (=> iequal?) (iq ))

  (check
      (imap - (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (imap - (iq ) (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (imap - (iq ) (iq ) (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (imap - numbers)
    (=> iequal?) (iq 0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (imap + (iq 1 2 3))
    (=> iequal?) (iq 1 2 3))

  (check
      (imap +
	    (iq 1 2 3)
	    (iq 10 20 30))
    (=> iequal?) (iq 11 22 33))

  (check
      (imap +
	    (iq 1 2 3)
	    (iq 10 20 30)
	    (iq 100 200 300))
    (=> iequal?) (iq 111 222 333))

  (check
      (imap +
	    (iq 1 2 3)
	    (iq 10 20)
	    (iq 100 200 300))
    (=> iequal?) (iq 111 222))

  (check
      (imap +
	    (iq 1 2)
	    (iq 10 20 30)
	    (iq 100 200 300))
    (=> iequal?) (iq 111 222))

  (check
      (imap +
	    (iq 1 2 3)
	    (iq 10 20 30)
	    (iq 100 200))
    (=> iequal?) (iq 111 222))

  (check
      (imap +
	    (iq )
	    (iq 10 20 30)
	    (iq 100 200 300))
    (=> iequal?) (iq ))

  (check
      (imap +
	    (iq 1 2 3)
	    (iq )
	    (iq 100 200 300))
    (=> iequal?) (iq ))

  (check
      (imap +
	    (iq 1 2 3)
	    (iq 10 20 30)
	    (iq ))
    (=> iequal?) (iq ))

;;; --------------------------------------------------------------------

  (check
      (let ((r 0))
	(ifor-each
	 (lambda (e)
	   (set! r (+ e r)))
	 (iq ))
	r)
    (=> iequal?) 0)

  (check
      (let ((r 0))
	(ifor-each
	 (lambda (e1 e2)
	   (set! r (+ e1 e2 r)))
	 (iq ) (iq ))
	r)
    (=> iequal?) 0)

  (check
      (let ((r 0))
	(ifor-each
	 (lambda (e1 e2 e3)
	   (set! r (+ e1 e2 e3 r)))
	 (iq ) (iq ) (iq ))
	r)
    (=> iequal?) 0)

  (check
      (let ((r (iq 0 0)))
	(ifor-each
	 (lambda (e1 e2)
	   (set! r (ilist (+ e1 (icar r))
			  (+ e2 (icadr r)))))
	 (iq 1 10 100)
	 (iq 2 20 200))
	r)
    (=> iequal?) (iq 111 222))


  (check
      (let ((r (iq 0 0 0)))
	(ifor-each
	 (lambda (e1 e2 e3)
	   (set! r (ilist (+ e1 (icar r))
			  (+ e2 (icadr r))
			  (+ e3 (icaddr r)))))
	 (iq 1 10 100)
	 (iq 2 20 200)
	 (iq 3 30 300))
	r)
    (=> iequal?) (iq 111 222 333))

;;; --------------------------------------------------------------------

  (let ()
    (define (f x)
      (ilist x (- x)))

    (check
	(iappend-map f (iq ))
      (=> iequal?) (iq ))

    (check
	(iappend-map ilist (iq ) (iq ))
      (=> iequal?) (iq ))

    (check
	(iappend-map ilist (iq ) (iq ) (iq ))
      (=> iequal?) (iq ))

    (check
	(iappend-map f (iq 1))
      (=> iequal?) (iq 1 -1))

    (check
	(iappend-map ilist (iq 1) (iq 2))
      (=> iequal?) (iq 1 2))

    (check
	(iappend-map ilist (iq 1) (iq 2) (iq 3))
      (=> iequal?) (iq 1 2 3))

    (check
	(iappend-map f (iq 1 3 8))
      (=> iequal?) (iq 1 -1 3 -3 8 -8))

    (check
	(iappend-map ilist
		     (iq 1 2 3)
		     (iq 10 20 30))
      (=> iequal?) (iq 1 10 2 20 3 30))

    (check
	(iappend-map ilist
		     (iq 1 2 3)
		     (iq 10 20 30))
      (=> iequal?) (iq 1 10 2 20 3 30))

    (check
	(iappend-map ilist
		     (iq 1 2 3)
		     (iq 10 20 30)
		     (iq 100 200 300))
      (=> iequal?) (iq 1 10 100 2 20 200 3 30 300))

    (check
	(iappend-map ilist
		     (iq 1 2)
		     (iq 10 20 30)
		     (iq 100 200 300))
      (=> iequal?) (iq 1 10 100 2 20 200))

    (check
	(iappend-map ilist
		     (iq 1 2 3)
		     (iq 10 20)
		     (iq 100 200 300))
      (=> iequal?) (iq 1 10 100 2 20 200))

    (check
	(iappend-map ilist
		     (iq 1 2 3)
		     (iq 10 20 30)
		     (iq 100 200))
      (=> iequal?) (iq 1 10 100 2 20 200))

    #f)

;;; --------------------------------------------------------------------

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x)
	   (set! r (ipair x r)))
	 (iq 1 2 3))
	r)
    (=> iequal?) (iq (3)
		     (2 3)
		     (1 2 3)))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x)
	   (set! r (ipair x r)))
	 (iq ))
	r)
    (=> iequal?) (iq ))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y)
	   (set! r (ipair (ilist x y)
			  r)))
	 (iq )
	 (iq ))
	r)
    (=> iequal?) (iq ))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq )
	 (iq )
	 (iq ))
	r)
    (=> iequal?) (iq ))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x)
	   (set! r (ipair x r)))
	 (iq 1))
	r)
    (=> iequal?) (iq (1)))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x)
	   (set! r (ipair x r)))
	 (iq 1 2))
	r)
    (=> iequal?) (iq (2)
		     (1 2)))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y)
	   (set! r (ipair (ilist x y)
			  r)))
	 (iq 1 2 3)
	 (iq 10 20 30))
	r)
    (=> iequal?) (iq ((3) (30))
		     ((2 3) (20 30))
		     ((1 2 3) (10 20 30))))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq 1 2 3)
	 (iq 10 20 30)
	 (iq 100 200 300))
	r)
    (=> iequal?) (iq ((3) (30) (300))
		     ((2 3) (20 30) (200 300))
		     ((1 2 3) (10 20 30) (100 200 300))))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq 1)
	 (iq 10)
	 (iq 100))
	r)
    (=> iequal?) (iq ((1) (10) (100))))

;;; --------------------------------------------------------------------

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x)
	   (set! r (ipair x r)))
	 (iq 1 2 3))
	r)
    (=> iequal?) (iq (3)
		     (2 3)
		     (1 2 3)))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x)
	   (set! r (ipair x r)))
	 (iq ))
	r)
    (=> iequal?) (iq ))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq )
	 (iq )
	 (iq ))
	r)
    (=> iequal?) (iq ))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y)
	   (set! r (ipair (ilist x y)
			  r)))
	 (iq )
	 (iq ))
	r)
    (=> iequal?) (iq ))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x)
	   (set! r (ipair x r)))
	 (iq 1))
	r)
    (=> iequal?) (iq (1)))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x)
	   (set! r (ipair x r)))
	 (iq 1 2))
	r)
    (=> iequal?) (iq (2)
		     (1 2)))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y)
	   (set! r (ipair (ilist x y)
			  r)))
	 (iq 1 2 3)
	 (iq 10 20 30))
	r)
    (=> iequal?) (iq ((3) (30))
		     ((2 3) (20 30))
		     ((1 2 3) (10 20 30))))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq 1 2 3)
	 (iq 10 20 30)
	 (iq 100 200 300))
	r)
    (=> iequal?) (iq ((3) (30) (300))
		     ((2 3) (20 30) (200 300))
		     ((1 2 3) (10 20 30) (100 200 300))))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq 1 2)
	 (iq 10 20 30)
	 (iq 100 200 300))
	r)
    (=> iequal?) (iq ((2) (20 30) (200 300))
		     ((1 2) (10 20 30) (100 200 300))))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq 1 2 3)
	 (iq 10 20)
	 (iq 100 200 300))
	r)
    (=> iequal?) (iq ((2 3) (20) (200 300))
		     ((1 2 3) (10 20) (100 200 300))))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq 1 2 3)
	 (iq 10 20 30)
	 (iq 100 200))
	r)
    (=> iequal?) (iq ((2 3) (20 30) (200))
		     ((1 2 3) (10 20 30) (100 200))))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq )
	 (iq 10 20 30)
	 (iq 100 200 300))
	r)
    (=> iequal?) (iq ))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq 1 2 3)
	 (iq )
	 (iq 100 200 300))
	r)
    (=> iequal?) (iq ))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq 1 2 3)
	 (iq 10 20 30)
	 (iq ))
	r)
    (=> iequal?) (iq ))

  (check
      (let ((r (iq )))
	(ipair-for-each
	 (lambda (x y z)
	   (set! r (ipair (ilist x y z)
			  r)))
	 (iq 1)
	 (iq 10)
	 (iq 100))
	r)
    (=> iequal?) (iq ((1) (10) (100))))

;;; --------------------------------------------------------------------

  (check
      (ifilter-map
       (lambda (x)
	 (and (number? x)
	      (* x x)))
       (iq a 1 b 3 c 7))
    (=> iequal?) (iq 1 9 49))

  (check
      (ifilter-map - (iq ))
    (=> iequal?) (iq ))

  (check
      (ifilter-map - (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (ifilter-map - (iq ) (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (ifilter-map - (iq ) (iq ) (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (ifilter-map - numbers)
    (=> iequal?) (iq 0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (ifilter-map + (iq 1 2 3))
    (=> iequal?) (iq 1 2 3))

  (check
      (ifilter-map +
		   (iq 1 2 3)
		   (iq 10 20 30))
    (=> iequal?) (iq 11 22 33))

  (check
      (ifilter-map +
		   (iq 1 2 3)
		   (iq 10 20 30)
		   (iq 100 200 300))
    (=> iequal?) (iq 111 222 333))

;;; --------------------------------------------------------------------

  (check
      (ifilter-map
       (lambda (x)
	 (and (number? x)
	      (* x x)))
       (iq a 1 b 3 c 7))
    (=> iequal?) (iq 1 9 49))

  (check
      (ifilter-map - (iq ))
    (=> iequal?) (iq ))

  (check
      (ifilter-map - (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (ifilter-map - (iq ) (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (ifilter-map - (iq ) (iq ) (iq ) (iq ))
    (=> iequal?) (iq ))

  (check
      (ifilter-map - numbers)
    (=> iequal?) (iq 0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (ifilter-map + (iq 1 2 3))
    (=> iequal?) (iq 1 2 3))

  (check
      (ifilter-map +
		   (iq 1 2 3)
		   (iq 10 20 30))
    (=> iequal?) (iq 11 22 33))

  (check
      (ifilter-map +
		   (iq 1 2 3)
		   (iq 10 20 30)
		   (iq 100 200 300))
    (=> iequal?) (iq 111 222 333))

  (check
      (ifilter-map +
		   (iq 1 2 3)
		   (iq 10 20)
		   (iq 100 200 300))
    (=> iequal?) (iq 111 222))

  (check
      (ifilter-map +
		   (iq 1 2)
		   (iq 10 20 30)
		   (iq 100 200 300))
    (=> iequal?) (iq 111 222))

  (check
      (ifilter-map +
		   (iq 1 2 3)
		   (iq 10 20 30)
		   (iq 100 200))
    (=> iequal?) (iq 111 222))

  (check
      (ifilter-map +
		   (iq )
		   (iq 10 20 30)
		   (iq 100 200 300))
    (=> iequal?) (iq ))

  (check
      (ifilter-map +
		   (iq 1 2 3)
		   (iq )
		   (iq 100 200 300))
    (=> iequal?) (iq ))

  (check
      (ifilter-map +
		   (iq 1 2 3)
		   (iq 10 20 30)
		   (iq ))
    (=> iequal?) (iq ))

  #f)


(parametrise ((check-test-name 'filtering))

  (check
      (ifilter even? (iq ))
    (=> iequal?) (iq ))

  (check
      (ifilter even? (iq 1))
    (=> iequal?) (iq ))

  (check
      (ifilter even? (iq 2))
    (=> iequal?) (iq 2))

  (check
      (ifilter even? numbers)
    (=> iequal?) (iq 0 2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (ipartition even? (iq )))
	ilist)
    (=> iequal?) (iq () ()))

  (check
      (call-with-values
	  (lambda ()
	    (ipartition even? (iq 1)))
	ilist)
    (=> iequal?) (iq () (1)))

  (check
      (call-with-values
	  (lambda ()
	    (ipartition even? (iq 2)))
	ilist)
    (=> iequal?) (iq (2) ()))

  (check
      (call-with-values
	  (lambda ()
	    (ipartition even? (iq 1 3)))
	ilist)
    (=> iequal?) (iq () (1 3)))

  (check
      (call-with-values
	  (lambda ()
	    (ipartition even? (iq 2 4)))
	ilist)
    (=> iequal?) (iq (2 4) ()))

  (check
      (call-with-values
	  (lambda ()
	    (ipartition even? numbers))
	ilist)
    (=> iequal?) (iq (0 2 4 6 8)
		     (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check
      (iremove (lambda (obj) (eqv? 8 obj)) numbers)
    (=> iequal?) (iq 0 1 2 3 4 5 6 7 9))

  (check
      (iremove (lambda (obj) (eqv? 8 obj)) (iq 1 2 3))
    (=> iequal?) (iq 1 2 3))

  (check
      (iremove (lambda (obj) (eqv? 8 obj)) (iq 1))
    (=> iequal?) (iq 1))

  (check
      (iremove (lambda (obj) (eqv? 8 obj)) (iq ))
    (=> iequal?) (iq ))

;;; --------------------------------------------------------------------

  (check
      (iremove even? (iq ))
    (=> iequal?) (iq ))

  (check
      (iremove even? (iq 1))
    (=> iequal?) (iq 1))

  (check
      (iremove even? (iq 2))
    (=> iequal?) (iq ))

  (check
      (iremove even? numbers)
    (=> iequal?) (iq 1 3 5 7 9))

  #f)


(parametrise ((check-test-name 'finding))

  (check
      (ifind even? (iq ))
    (=> iequal?) #f)

  (check
      (ifind even? (iq 1))
    (=> iequal?) #f)

  (check
      (ifind even? (iq 2))
    (=> iequal?) 2)

  (check
      (ifind even? (iq 1 2 3))
    (=> iequal?) 2)

;;; --------------------------------------------------------------------

  (check
      (ifind-tail even? (iq ))
    (=> iequal?) #f)

  (check
      (ifind-tail even? (iq 1))
    (=> iequal?) #f)

  (check
      (ifind-tail even? (iq 2))
    (=> iequal?) (iq 2))

  (check
      (ifind-tail even? (iq 1 2 3))
    (=> iequal?) (iq 2 3))

;;; --------------------------------------------------------------------

  (check
      (itake-while even? (iq ))
    (=> iequal?) (iq ))

  (check
      (itake-while even? (iq 1))
    (=> iequal?) (iq ))

  (check
      (itake-while even? (iq 2))
    (=> iequal?) (iq 2))

  (check
      (itake-while even? (iq 2 4 6 1 3))
    (=> iequal?) (iq 2 4 6))

;;; --------------------------------------------------------------------

  (check
      (idrop-while even? (iq ))
    (=> iequal?) (iq ))

  (check
      (idrop-while even? (iq 1))
    (=> iequal?) (iq 1))

  (check
      (idrop-while even? (iq 2))
    (=> iequal?) (iq ))

  (check
      (idrop-while even? (iq 2 4 6 1 3))
    (=> iequal?) (iq 1 3))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (ispan even? (iq )))
	ilist)
    (=> iequal?) (iq () ()))

  (check
      (call-with-values
	  (lambda () (ispan even? (iq 1)))
	ilist)
    (=> iequal?) (iq () (1)))

  (check
      (call-with-values
	  (lambda () (ispan even? (iq 2)))
	ilist)
    (=> iequal?) (iq (2) ()))

  (check
      (call-with-values
	  (lambda () (ispan even? (iq 2 4 6 1 3)))
	ilist)
    (=> iequal?) (iq (2 4 6) (1 3)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (ibreak even? (iq )))
	ilist)
    (=> iequal?) (iq () ()))

  (check
      (call-with-values
	  (lambda () (ibreak even? (iq 1)))
	ilist)
    (=> iequal?) (iq (1) ()))

  (check
      (call-with-values
	  (lambda () (ibreak even? (iq 2)))
	ilist)
    (=> iequal?) (iq () (2)))

  (check
      (call-with-values
	  (lambda () (ibreak even? (iq 1 3 2 4 6)))
	ilist)
    (=> iequal?) (iq (1 3) (2 4 6)))

;;; --------------------------------------------------------------------

  (check
      (iany even? (iq ))
    (=> iequal?) #f)

  (check
      (iany even? (iq 1))
    (=> iequal?) #f)

  (check
      (and (iany even? (iq 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (iany even? (iq 1 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (iany even? (iq 1 3 5 7 2))
	   #t)
    (=> iequal?) #t)

  (check
      (iany (lambda args
	      (integer? (apply + args)))
	    (iq ) (iq ))
    (=> iequal?) #f)

  (check
      (iany (lambda args
	      (integer? (apply + args)))
	    (iq ) (iq ) (iq ))
    (=> iequal?) #f)

;;; The following are  false because when a list  is empty the predicate
;;; is not applied at all and the return value is false.
  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq ) (iq ) (iq ))
	   #t)
    (=> iequal?) #f)
  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq ) (iq ) (iq ))
	   #t)
    (=> iequal?) #f)
  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq ) (iq ) (iq ))
	   #t)
    (=> iequal?) #f)

  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq 1) (iq 1.1) (iq 2))
	   #t)
    (=> iequal?) #f)

  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq 1) (iq 2) (iq 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq 1 2)
		 (iq 2 2.2)
		 (iq 1.1 3))
	   #t)
    (=> iequal?) #f)

  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq 1 2)
		 (iq 2 2)
		 (iq 1.1 3))
	   #t)
    (=> iequal?) #t)

  (check
      (guard (E (else #t))
	(iany (lambda args
		(integer? (apply + args)))
	      (iq 1 2)
	      (iq 2 2)
	      (iq 1.1 3)))
    (=> iequal?) #t)

;;; --------------------------------------------------------------------

  (check
      (iany even? (iq ))
    (=> iequal?) #f)

  (check
      (iany even? (iq 1))
    (=> iequal?) #f)

  (check
      (and (iany even? (iq 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (iany even? (iq 1 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (iany even? (iq 1 3 5 7 2))
	   #t)
    (=> iequal?) #t)

  (check
      (iany (lambda args
	      (integer? (apply + args)))
	    (iq ) (iq ))
    (=> iequal?) #f)

  (check
      (iany (lambda args
	      (integer? (apply + args)))
	    (iq ) (iq ) (iq ))
    (=> iequal?) #f)

;;; The following are  false because when a list  is empty the predicate
;;; is not applied at all and the return value is false.
  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq 1) (iq ) (iq ))
	   #t)
    (=> iequal?) #f)
  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq ) (iq 1) (iq ))
	   #t)
    (=> iequal?) #f)
  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq ) (iq ) (iq 1))
	   #t)
    (=> iequal?) #f)

  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq 1) (iq 1.1) (iq 2))
	   #t)
    (=> iequal?) #f)

  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq 1) (iq 2) (iq 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq 1 2)
		 (iq 2 2.2)
		 (iq 1.1 3))
	   #t)
    (=> iequal?) #f)

  (check
      (and (iany (lambda args
		   (integer? (apply + args)))
		 (iq 1 2)
		 (iq 2 2)
		 (iq 1.1 3))
	   #t)
    (=> iequal?) #t)

;;; --------------------------------------------------------------------

  (check
      (ievery even? (iq ))
    (=> iequal?) #t)

  (check
      (ievery even? (iq 1))
    (=> iequal?) #f)

  (check
      (and (ievery even? (iq 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (ievery even? (iq 1 2))
	   #t)
    (=> iequal?) #f)

  (check
      (and (ievery even? (iq 4 8 10 12))
	   #t)
    (=> iequal?) #t)

  (check
      (ievery (lambda args
		(integer? (apply + args)))
	      (iq ) (iq ))
    (=> iequal?) #t)

  (check
      (ievery (lambda args
		(integer? (apply + args)))
	      (iq ) (iq ) (iq ))
    (=> iequal?) #t)

;;; The following are true because when a list is empty the predicate is
;;; not applied at all and the return value is true.
  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq ) (iq ) (iq ))
	   #t)
    (=> iequal?) #t)
  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq ) (iq ) (iq ))
	   #t)
    (=> iequal?) #t)
  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq ) (iq ) (iq ))
	   #t)
    (=> iequal?) #t)

  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq 1) (iq 1.1) (iq 2))
	   #t)
    (=> iequal?) #f)

  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq 1) (iq 2) (iq 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq 1 2)
		   (iq 2 2.2)
		   (iq 1 3))
	   #t)
    (=> iequal?) #f)

  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq 1 2)
		   (iq 2 2)
		   (iq 1 3))
	   #t)
    (=> iequal?) #t)

  (check
      (guard (E (else #t))
	(ievery (lambda args
		  (integer? (apply + args)))
		(iq 1 2)
		(iq 2 2 2)
		(iq 1 3)))
    (=> iequal?) #t)

;;; --------------------------------------------------------------------

  (check
      (ievery even? (iq ))
    (=> iequal?) #t)

  (check
      (ievery even? (iq 1))
    (=> iequal?) #f)

  (check
      (and (ievery even? (iq 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (ievery even? (iq 1 2))
	   #t)
    (=> iequal?) #f)

  (check
      (and (ievery even? (iq 4 8 10 12))
	   #t)
    (=> iequal?) #t)

  (check
      (ievery (lambda args
		(integer? (apply + args)))
	      (iq ) (iq ))
    (=> iequal?) #t)

  (check
      (ievery (lambda args
		(integer? (apply + args)))
	      (iq ) (iq ) (iq ))
    (=> iequal?) #t)

;;; The following are true because when a list is empty the predicate is
;;; not applied at all and the return value is true.
  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq 1) (iq ) (iq ))
	   #t)
    (=> iequal?) #t)
  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq ) (iq 1) (iq ))
	   #t)
    (=> iequal?) #t)
  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq ) (iq ) (iq 1))
	   #t)
    (=> iequal?) #t)

  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq 1) (iq 1.1) (iq 2))
	   #t)
    (=> iequal?) #f)

  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq 1) (iq 2) (iq 2))
	   #t)
    (=> iequal?) #t)

  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq 1 2)
		   (iq 2 2.2)
		   (iq 1 3))
	   #t)
    (=> iequal?) #f)

  (check
      (and (ievery (lambda args
		     (integer? (apply + args)))
		   (iq 1 2)
		   (iq 2 2)
		   (iq 1 3))
	   #t)
    (=> iequal?) #t)

;;; --------------------------------------------------------------------

  (check
      (ilist-index even? (iq ))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq ) (iq ))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq ) (iq ) (iq ))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq 1))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq 1 3 5))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq 2))
    (=> iequal?) 0)

  (check
      (ilist-index even? (iq 1 2 3 5))
    (=> iequal?) 1)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1 2 3))
    (=> iequal?) 0)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1.1 2 3))
    (=> iequal?) 1)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1 2 3)
		   (iq 1 2 3))
    (=> iequal?) 0)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1.1 2 3)
		   (iq 1 2 3))
    (=> iequal?) 1)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1 2 3)
		   (iq 1.1 2.1 3))
    (=> iequal?) 2)

;;; --------------------------------------------------------------------

  (check
      (ilist-index even? (iq ))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq ) (iq ))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq ) (iq ) (iq ))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq 1))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq 1 3 5))
    (=> iequal?) #f)

  (check
      (ilist-index even? (iq 2))
    (=> iequal?) 0)

  (check
      (ilist-index even? (iq 1 2 3 5))
    (=> iequal?) 1)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1 2 3))
    (=> iequal?) 0)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1.1 2 3))
    (=> iequal?) 1)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1 2 3)
		   (iq 1 2 3))
    (=> iequal?) 0)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1.1 2 3)
		   (iq 1 2 3))
    (=> iequal?) 1)

  (check
      (ilist-index (lambda args
		     (integer? (apply + args)))
		   (iq 1 2 3)
		   (iq 1 2 3)
		   (iq 1.1 2.1 3))
    (=> iequal?) 2)

;;; --------------------------------------------------------------------

  (check
      (imemq 'a (iq a b c))
    (=> iequal?) (iq a b c))

  (check
      (imemq 'b (iq a b c))
    (=> iequal?) (iq b c))

  (check
      (imemq 'a (iq b c d))
    (=> iequal?) #f)

  (check
      (imemq (list 'a) (iq b (a) c))
    (=> iequal?) #f)

;;; --------------------------------------------------------------------

  (check
      (imember (iq a)
	       (iq b (a) c))
    (=> iequal?) (iq (a) c))

  (check
      (imember (iq a)
	       (iq b (a) c))
    (=> iequal?) (iq (a) c))

  (check
      (imember (iq a)
	       (iq b a c))
    (=> iequal?) #f)

  (check
      (imember (iq a)
	       (iq ))
    (=> iequal?) #f)

  (check
      (imember 10
	       (iq 1 2 3 11 4 5)
	       (lambda (a b)
		 (= (+ 1 a) b)))
    (=> iequal?) (iq 11 4 5))

;;; --------------------------------------------------------------------

  (check
      (imemv 101 (iq 100 101 102))
    (=> iequal?) (iq 101 102))

  #f)


(parametrise ((check-test-name 'deletion))

  (check
      (idelete 8 (iq ))
    (=> iequal?) (iq ))

  (check
      (idelete 8 (iq 1))
    (=> iequal?) (iq 1))

  (check
      (idelete 8 (iq 8))
    (=> iequal?) (iq ))

  (check
      (idelete 8 (iq 1 2 3))
    (=> iequal?) (iq 1 2 3))

  (check
      (idelete 8 (iq 1 2 8 3 4 5 8 6 7 8))
    (=> iequal?) (iq 1 2 3 4 5 6 7))

  (check
      (idelete 8 (iq ) =)
    (=> iequal?) (iq ))

  (check
      (idelete 8 (iq 1) =)
    (=> iequal?) (iq 1))

  (check
      (idelete 8 (iq 8) =)
    (=> iequal?) (iq ))

  (check
      (idelete 8 (iq 1 2 3) =)
    (=> iequal?) (iq 1 2 3))

  (check
      (idelete 8 (iq 1 2 8 3 4 5 8 6 7 8) =)
    (=> iequal?) (iq 1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (idelete-duplicates (iq ))
    (=> iequal?) (iq ))

  (check
      (idelete-duplicates (iq 1))
    (=> iequal?) (iq 1))

  (check
      (idelete-duplicates (iq 1 2))
    (=> iequal?) (iq 1 2))

  (check
      (idelete-duplicates (iq 1 1))
    (=> iequal?) (iq 1))

  (check
      (idelete-duplicates (iq 1 1 1))
    (=> iequal?) (iq 1))

  (check
      (idelete-duplicates (iq 1 2 3 2 4 5 4 6 1 7))
    (=> iequal?) (iq 1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (idelete-duplicates (iq ) =)
    (=> iequal?) (iq ))

  (check
      (idelete-duplicates (iq 1) =)
    (=> iequal?) (iq 1))

  (check
      (idelete-duplicates (iq 1 2) =)
    (=> iequal?) (iq 1 2))

  (check
      (idelete-duplicates (iq 1 1) =)
    (=> iequal?) (iq 1))

  (check
      (idelete-duplicates (iq 1 1 1) =)
    (=> iequal?) (iq 1))

  (check
      (idelete-duplicates (iq 1 2 3 2 4 5 4 6 1 7) =)
    (=> iequal?) (iq 1 2 3 4 5 6 7))

  #f)


(parametrise ((check-test-name 'alists))

  (check
      (iassoc 'a
	      (iq (a . 1)
		  (b . 2)
		  (c . 3)))
    (=> iequal?) (iq a . 1))

  (check
      (iassoc 'b
	      (iq (a . 1)
		  (b . 2)
		  (c . 3)))
    (=> iequal?) (iq b . 2))

  (check
      (iassoc 'c
	      (iq (a . 1)
		  (b . 2)
		  (c . 3)))
    (=> iequal?) (iq c . 3))

;;; --------------------------------------------------------------------

  (check
      (iassoc 'c
	      (iq ))
    (=> iequal?) #f)

  (check
      (iassoc 'd
	      (iq (a . 1)
		  (b . 2)
		  (c . 3)))
    (=> iequal?) #f)

  (check
      (iassoc 'a
	      (iq (a . 1)
		  (b . 2)
		  (c . 3)))
    (=> iequal?) (iq a . 1))

  (check
      (iassoc 'b
	      (iq (a . 1)
		  (b . 2)
		  (c . 3)))
    (=> iequal?) (iq b . 2))

  (check
      (iassoc 'c
	      (iq (a . 1)
		  (b . 2)
		  (c . 3)))
    (=> iequal?) (iq c . 3))

  (check
      (iassoc 'a
	      (iq (a . 1)
		  (b . 2)
		  (c . 3))
	      eq?)
    (=> iequal?) (iq a . 1))

  (check
      (iassoc 'b
	      (iq (a . 1)
		  (b . 2)
		  (c . 3))
	      eq?)
    (=> iequal?) (iq b . 2))

  (check
      (iassoc 'c
	      (iq (a . 1)
		  (b . 2)
		  (c . 3))
	      eq?)
    (=> iequal?) (iq c . 3))

;;; --------------------------------------------------------------------

  (check
      (iassq 'c
	     (iq ))
    (=> iequal?) #f)

  (check
      (iassq 'd
	     (iq (a . 1)
		 (b . 2)
		 (c . 3)))
    (=> iequal?) #f)

  (check
      (iassq 'a
	     (iq (a . 1)
		 (b . 2)
		 (c . 3)))
    (=> iequal?) (iq a . 1))

  (check
      (iassq 'b
	     (iq (a . 1)
		 (b . 2)
		 (c . 3)))
    (=> iequal?) (iq b . 2))

  (check
      (iassq 'c
	     (iq (a . 1)
		 (b . 2)
		 (c . 3)))
    (=> iequal?) (iq c . 3))

;;; --------------------------------------------------------------------

  (check
      (iassv 'c
	     (iq ))
    (=> iequal?) #f)

  (check
      (iassv 'd
	     (iq (a . 1)
		 (b . 2)
		 (c . 3)))
    (=> iequal?) #f)

  (check
      (iassv 'a
	     (iq (a . 1)
		 (b . 2)
		 (c . 3)))
    (=> iequal?) (iq a . 1))

  (check
      (iassv 'b
	     (iq (a . 1)
		 (b . 2)
		 (c . 3)))
    (=> iequal?) (iq b . 2))

  (check
      (iassv 'c
	     (iq (a . 1)
		 (b . 2)
		 (c . 3)))
    (=> iequal?) (iq c . 3))

;;; --------------------------------------------------------------------

  (check
      (ialist-cons 'a 1
		   (iq (b . 2)
		       (c . 3)))
    (=> iequal?) (iq (a . 1)
		     (b . 2)
		     (c . 3)))

  (check
      (ialist-cons 'a 1
		   (iq ))
    (=> iequal?) (iq (a . 1)))

  (check
      (ialist-cons 'b 2
		   (iq (b . 2)
		       (c . 3)))
    (=> iequal?) (iq (b . 2)
		     (b . 2)
		     (c . 3)))

;;; --------------------------------------------------------------------

  (check
      (ialist-copy (iq (a . 1)
		       (b . 2)
		       (c . 3)))
    (=> iequal?) (iq (a . 1)
		     (b . 2)
		     (c . 3)))

  (check
      (ialist-copy (iq (a . 1)))
    (=> iequal?) (iq (a . 1)))

  (check
      (ialist-copy (iq ))
    (=> iequal?) (iq ))

;;; --------------------------------------------------------------------

  (check
      (ialist-delete 'a
		     (iq (a . 1)
			 (b . 2)
			 (c . 3)))
    (=> iequal?) (iq (b . 2)
		     (c . 3)))

  (check
      (ialist-delete 'b
		     (iq (a . 1)
			 (b . 2)
			 (c . 3)))
    (=> iequal?) (iq (a . 1)
		     (c . 3)))

  (check
      (ialist-delete 'c
		     (iq (a . 1)
			 (b . 2)
			 (c . 3)))
    (=> iequal?) (iq (a . 1)
		     (b . 2)))

  (check
      (ialist-delete 'd
		     (iq (a . 1)
			 (b . 2)
			 (c . 3)))
    (=> iequal?) (iq (a . 1)
		     (b . 2)
		     (c . 3)))

  (check
      (ialist-delete 'a
		     (iq (a . 1)
			 (a . 2)
			 (c . 3)))
    (=> iequal?) (iq (c . 3)))

  (check
      (ialist-delete 'a
		     (iq ))
    (=> iequal?) (iq ))

  (check
      (ialist-delete 'a
		     (iq (a . 1)))
    (=> iequal?) (iq ))

  (check
      (ialist-delete 'a
		     (iq (a . 1)
			 (b . 2)
			 (c . 3))
		     eq?)
    (=> iequal?) (iq (b . 2)
		     (c . 3)))

  (check
      (ialist-delete 'b
		     (iq (a . 1)
			 (b . 2)
			 (c . 3))
		     eq?)
    (=> iequal?) (iq (a . 1)
		     (c . 3)))

  (check
      (ialist-delete 'c
		     (iq (a . 1)
			 (b . 2)
			 (c . 3))
		     eq?)
    (=> iequal?) (iq (a . 1)
		     (b . 2)))

  (check
      (ialist-delete 'd
		     (iq (a . 1)
			 (b . 2)
			 (c . 3))
		     eq?)
    (=> iequal?) (iq (a . 1)
		     (b . 2)
		     (c . 3)))

  (check
      (ialist-delete 'a
		     (iq (a . 1)
			 (a . 2)
			 (c . 3))
		     eq?)
    (=> iequal?) (iq (c . 3)))

  (check
      (ialist-delete 'a
		     (iq )
		     eq?)
    (=> iequal?) (iq ))

  (check
      (ialist-delete 'a
		     (iq (a . 1))
		     eq?)
    (=> iequal?) (iq ))

  #f)


(parametrise ((check-test-name		'conversion))

  (check (vector->ilist '#())			=> '())
  (check (vector->ilist '#(1))			=> (iq 1))
  (check (vector->ilist '#(1 2 3))		=> (iq 1 2 3))

;;; --------------------------------------------------------------------

  (check (ilist->vector '())			=> '#())
  (check (ilist->vector (iq 1))			=> '#(1))
  (check (ilist->vector (iq 1 2 3))		=> '#(1 2 3))

  #t)


(parametrise ((check-test-name	'comparator-predicates))

  (check-for-true (comparator? ipair-comparator))
  (check-for-true (comparator? ilist-comparator))

;;; --------------------------------------------------------------------

  (check-for-true (comparator-comparison-procedure? ipair-comparator))
  (check-for-true (comparator-comparison-procedure? ilist-comparator))

;;; --------------------------------------------------------------------

  (check-for-true (comparator-hash-function? ipair-comparator))
  (check-for-true (comparator-hash-function? ilist-comparator))

  #t)


(parametrise ((check-test-name	'comparator-accessors))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?C ?a ?b)
       (begin
	 (check-for-true  ((comparator-type-test-procedure ?C) ?a))
	 (check-for-true  ((comparator-type-test-procedure ?C) ?b))
	 (check-for-false ((comparator-type-test-procedure ?C) (void)))
	 (check-for-true  ((comparator-equality-predicate ?C) ?a ?a))
	 (check-for-false ((comparator-equality-predicate ?C) ?a ?b))
	 (check
	     ((comparator-comparison-procedure ?C) ?a ?a)
	   => 0)
	 (check
	     ((comparator-comparison-procedure ?C) ?a ?b)
	   => -1)
	 (check
	     ((comparator-comparison-procedure ?C) ?b ?a)
	   => +1)
	 (check-for-true
	  (non-negative-exact-integer? ((comparator-hash-function ?C) ?a)))
	 (check-for-true
	  (non-negative-exact-integer? ((comparator-hash-function ?C) ?b)))
	 ))
      ))

;;; --------------------------------------------------------------------

  (doit ipair-comparator (iq 1 . 2) (iq 3 . 4))
  (doit ilist-comparator (iq 1 2) (iq 3 4))

;;; --------------------------------------------------------------------
;;; pair comparison

  (let ((cmp (comparator-comparison-procedure ipair-comparator)))
    (check (cmp (iq 1 . 2) (iq 1 . 2)) =>  0)
    (check (cmp (iq 1 . 2) (iq 1 . 3)) => -1) ;2 < 3
    (check (cmp (iq 1 . 4) (iq 1 . 3)) => +1) ;4 > 3
    (check (cmp (iq 1 . 0) (iq 2 . 0)) => -1)
    (check (cmp (iq 3 . 0) (iq 2 . 0)) => +1)
    #f)

;;; --------------------------------------------------------------------
;;; list comparison

  (let ((cmp (comparator-comparison-procedure ilist-comparator)))
    (check (cmp (iq 1 2) (iq 1 2)) =>  0)
    (check (cmp (iq 1 2) (iq 1 3)) => -1) ;2 < 3
    (check (cmp (iq 1 4) (iq 1 3)) => +1) ;4 > 3
    (check (cmp (iq 1 0) (iq 2 0)) => -1)
    (check (cmp (iq 3 0) (iq 2 0)) => +1)

    (check (cmp (iq ) (iq ))	=> 0)
    (check (cmp (iq ) (iq 1))	=> -1)
    (check (cmp (iq 1) (iq ))	=> +1)

    ;;If first items are equal: compare the CADRs.  Here one of the CADRs is null.
    (check (cmp (iq 1 2) (iq 1))	=> +1)
    (check (cmp (iq 1)   (iq 1 2))	=> -1)

    ;;Lists  of  different length,  but  it  does not  matter  because  the CARs  are
    ;;non-equal.
    (check (cmp (iq 1 2) (iq 2))	=> -1)
    (check (cmp (iq 2)   (iq 1 2))	=> +1)
    #f)

  #t)


(parametrise ((check-test-name	'comparator-applicators))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?C ?a ?b)
       (begin
	 (check-for-true  (comparator-test-type ?C ?a))
	 (check-for-true  (comparator-test-type ?C ?b))
	 (check-for-false (comparator-test-type ?C (void)))
	 (check-for-true  (comparator-check-type ?C ?a))
	 (check-for-true  (comparator-check-type ?C ?b))
	 (check-for-true
	  (try
	      (comparator-check-type ?C (void))
	    (catch E
	      ((&comparator-type-error)
	       #t)
	      (else #f))))
	 (check-for-true  (comparator-equal? ?C ?a ?a))
	 (check-for-false (comparator-equal? ?C ?a ?b))
	 (check
	     (comparator-compare ?C ?a ?a)
	   => 0)
	 (check
	     (comparator-compare ?C ?a ?b)
	   => -1)
	 (check
	     (comparator-compare ?C ?b ?a)
	   => +1)
	 (check-for-true
	  (non-negative-exact-integer? (comparator-hash ?C ?a)))
	 (check-for-true
	  (non-negative-exact-integer? (comparator-hash ?C ?b)))
	 ))
      ))

;;; --------------------------------------------------------------------

  (doit ipair-comparator (iq 1 . 2) (iq 3 . 4))
  (doit ilist-comparator (iq 1 2) (iq 3 4))

;;; --------------------------------------------------------------------
;;; pair comparison

  (let ((cmp (comparator-comparison-procedure ipair-comparator)))
    (check (cmp (iq 1 . 2) (iq 1 . 2)) =>  0)
    (check (cmp (iq 1 . 2) (iq 1 . 3)) => -1) ;2 < 3
    (check (cmp (iq 1 . 4) (iq 1 . 3)) => +1) ;4 > 3
    (check (cmp (iq 1 . 0) (iq 2 . 0)) => -1)
    (check (cmp (iq 3 . 0) (iq 2 . 0)) => +1)
    #f)

;;; --------------------------------------------------------------------
;;; list comparison

  (let ((cmp (comparator-comparison-procedure ilist-comparator)))
    (check (cmp (iq 1 2) (iq 1 2)) =>  0)
    (check (cmp (iq 1 2) (iq 1 3)) => -1) ;2 < 3
    (check (cmp (iq 1 4) (iq 1 3)) => +1) ;4 > 3
    (check (cmp (iq 1 0) (iq 2 0)) => -1)
    (check (cmp (iq 3 0) (iq 2 0)) => +1)

    (check (cmp (iq ) (iq ))	=> 0)
    (check (cmp (iq ) (iq 1))	=> -1)
    (check (cmp (iq 1) (iq ))	=> +1)

    ;;If first items are equal: compare the CADRs.  Here one of the CADRs is null.
    (check (cmp (iq 1 2) (iq 1))	=> +1)
    (check (cmp (iq 1)   (iq 1 2))	=> -1)

    ;;Lists  of  different length,  but  it  does not  matter  because  the CARs  are
    ;;non-equal.
    (check (cmp (iq 1 2) (iq 2))	=> -1)
    (check (cmp (iq 2)   (iq 1 2))	=> +1)
    #f)



  #t)


(parametrise ((check-test-name	'ipair-comparator))

  (define-constant C
    (make-ipair-comparator exact-integer-comparator
			   real-comparator))

  ;; type test
  (check-for-true  (comparator-test-type C (iq 1 . 2.0)))
  (check-for-true  (comparator-test-type C (iq 1 . 2.0)))
  (check-for-false (comparator-test-type C (iq )))
  (check-for-false (comparator-test-type C (iq 1 . 2+1i)))
  (check-for-false (comparator-test-type C "ciao"))

  ;; type check
  (check-for-true  (comparator-check-type C (iq 1 . 2.0)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C (iq 1 . 2.0) (iq 1 . 2.0))	=> 0)
  (check (comparator-compare C (iq 1 . 2.0) (iq 1 . 3))	=> -1)
  (check (comparator-compare C (iq 1 . 3)   (iq 1 . 2.0))	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (iq 1 . 2.0))))

  #t)


(parametrise ((check-test-name	'icar-comparator))

  (define-constant C
    (make-icar-comparator exact-integer-comparator))

  ;; type test
  (check-for-true  (comparator-test-type C (iq 1 . 2.0)))
  (check-for-true  (comparator-test-type C (iq 1 . 2.0)))
  (check-for-true  (comparator-test-type C (iq 1 . 2+1i)))
  (check-for-false (comparator-test-type C (iq 2.0 . 1)))
  (check-for-false (comparator-test-type C (iq )))
  (check-for-false (comparator-test-type C "ciao"))

  ;; type check
  (check-for-true  (comparator-check-type C (iq 1 . 2.0)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C (iq 1 . 2) (iq 1 . 3))	=> 0)
  (check (comparator-compare C (iq 1 . 2) (iq 2 . 3))	=> -1)
  (check (comparator-compare C (iq 2 . 2) (iq 1 . 2))	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (iq 1 . 2.0))))

  #t)


(parametrise ((check-test-name	'icdr-comparator))

  (define-constant C
    (make-icdr-comparator exact-integer-comparator))

  ;; type test
  (check-for-true  (comparator-test-type C (iq 2.0 . 1)))
  (check-for-true  (comparator-test-type C (iq 2.0 . 1)))
  (check-for-true  (comparator-test-type C (iq 2+1i . 1)))
  (check-for-false (comparator-test-type C (iq 1 . 2.0)))
  (check-for-false (comparator-test-type C (iq )))
  (check-for-false (comparator-test-type C "ciao"))

  ;; type check
  (check-for-true  (comparator-check-type C (iq 2.0 . 1)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C (iq 2 . 1) (iq 3 . 1))	=> 0)
  (check (comparator-compare C (iq 2 . 1) (iq 3 . 2))	=> -1)
  (check (comparator-compare C (iq 2 . 2) (iq 2 . 1))	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (iq 2.0 . 1))))

  #t)


(parametrise ((check-test-name	'ilist-comparator))

  (define-constant C
    (make-ilist-comparator exact-integer-comparator))

  ;; type test
  (check-for-true  (comparator-test-type C (iq )))
  (check-for-true  (comparator-test-type C (iq 1 2)))
  (check-for-false (comparator-test-type C (iq 1 2 . 3)))
  (check-for-false (comparator-test-type C (iq 1 2.0)))
  (check-for-false (comparator-test-type C "ciao"))
  (check-for-false (comparator-test-type C (iq 1+2i)))

  ;; type check
  (check-for-true  (comparator-check-type C (iq 1 2)))
  (check-for-true
   (try
       (comparator-check-type C (void))
     (catch E
       ((&comparator-type-error)
	#t)
       (else E))))

  ;; comparison
  (check (comparator-compare C (iq 1 2) (iq 1 2))	=> 0)
  (check (comparator-compare C (iq 1 2) (iq 1 3))	=> -1)
  (check (comparator-compare C (iq 1 3) (iq 1 2))	=> +1)

  (check (comparator-compare C (iq )    (iq ))	=> 0)
  (check (comparator-compare C (iq )    (iq 1 2))	=> -1)
  (check (comparator-compare C (iq 1 2) (iq ))	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (iq ))))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (iq 1 2))))

  #t)


(parametrise ((check-test-name	'improper-ilist-comparator))

  (module (C)

    (define element-compare
      (let ((compare (comparator-comparison-procedure exact-integer-comparator)))
	(lambda (A B)
	  (if (ipair? A)
	      (begin
		(assert (ipair? B))
		(let ((rv (compare (icar A) (icar B))))
		  (if (zero? rv)
		      (comparator-compare C (icdr A) (icdr B))
		    rv)))
	    (compare A B)))))

    (define-constant E
      (make-comparator #t #t
		       element-compare
		       (comparator-hash-function default-comparator)))

    (define-constant C
      (make-improper-ilist-comparator E))

    #| end of module |# )

  ;; type test
  (check-for-true (comparator-test-type C (iq )))
  (check-for-true (comparator-test-type C (iq 1 2)))
  (check-for-true (comparator-test-type C (iq 1 2 . 3)))
  (check-for-true (comparator-test-type C (iq 1 2.0)))
  (check-for-true (comparator-test-type C "ciao"))
  (check-for-true (comparator-test-type C (iq 1+2i)))

  ;; type check
  (check-for-true (comparator-check-type C (iq 1 2)))
  (check-for-true (comparator-check-type C (void)))

  ;; comparison
  (check (comparator-compare C (iq 1 2) (iq 1 2))	=> 0)
  (check (comparator-compare C (iq 1 2) (iq 1 3))	=> -1)
  (check (comparator-compare C (iq 1 3) (iq 1 2))	=> +1)

  (check (comparator-compare C (iq )    (iq ))	=> 0)
  (check (comparator-compare C (iq )    (iq 1 2))	=> -1)
  (check (comparator-compare C (iq 1 2) (iq ))	=> +1)

  (check (comparator-compare C (iq 1 2 . 3) (iq 1 2 . 3))	=> 0)
  (check (comparator-compare C (iq 1 2 . 3) (iq 1 2 . 4))	=> -1)
  (check (comparator-compare C (iq 1 2 . 4) (iq 1 2 . 3))	=> +1)

  (check (comparator-compare C (iq 1 2 9 . 3) (iq 1 2 9 . 3))	=> 0)
  (check (comparator-compare C (iq 1 2 9 . 3) (iq 1 2 9 . 4))	=> -1)
  (check (comparator-compare C (iq 1 2 9 . 4) (iq 1 2 9 . 3))	=> +1)

  ;; hash
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (iq ))))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (iq 1 2))))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C (iq 1 2 . 3))))
  (check-for-true
   (non-negative-exact-integer? (comparator-hash C "ciao")))

  #t)


(parametrise ((check-test-name	'srfi-quotation))

  (check (iq)				=> '())
  (check (iq 1)				=> (ilist 1))
  (check (iq (1))			=> (ilist (ilist 1)))
  (check (iq 1 . 2)			=> (ipair 1 2))
  (check (iq 1 2)			=> (ilist 1 2))
  (check (iq 1 2 3)			=> (ilist 1 2 3))

  (check (iq (1) 2 3)			=> (ilist (ilist 1) 2 3))
  (check (iq 1 (2) 3)			=> (ilist 1 (ilist 2) 3))
  (check (iq 1 2 (3))			=> (ilist 1 2 (ilist 3)))

  (check (iq (1 2) 3)			=> (ilist (ilist 1 2) 3))
  (check (iq 1 (2 3))			=> (ilist 1 (ilist 2 3)))

  (check (iq (x y) z)			=> (ilist (ilist 'x 'y) 'z))
  (check (iq x (y z))			=> (ilist 'x (ilist 'y 'z)))

;;; --------------------------------------------------------------------
;;; vectors

  (check (iq #(1 2))			=> (ilist '#(1 2)))
  (check (iq #(1 (2)))			=> (ilist '#(1 (2))))

  #t)


(parametrise ((check-test-name	'vicare-quotation))

  (define %eval-env
    (environment '(rnrs)
		 '(srfi :116)
		 '(srfi :116 quotations)))

  (define (%eval form)
    (eval form %eval-env))

;;; --------------------------------------------------------------------

  (check
      (try
	  (%eval '(iquote))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-form E)))
	  (else E)))
    => '(iquote))

  (check
      (try
	  (%eval '(iquote 2 3))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-form E)))
	  (else E)))
    => '(iquote 2 3))

  (check (iquote 1)			=> 1)
  (check (iquote (1))			=> (ilist 1))
  (check (iquote (1 . 2))		=> (ipair 1 2))
  (check (iquote (1 2))			=> (ilist 1 2))
  (check (iquote (1 2 3))		=> (ilist 1 2 3))

  (check (iquote ((1) 2 3))		=> (ilist (ilist 1) 2 3))
  (check (iquote (1 (2) 3))		=> (ilist 1 (ilist 2) 3))
  (check (iquote (1 2 (3)))		=> (ilist 1 2 (ilist 3)))

  (check (iquote ((1 2) 3))		=> (ilist (ilist 1 2) 3))
  (check (iquote (1 (2 3)))		=> (ilist 1 (ilist 2 3)))

  (check (iquote ((x y) z))		=> (ilist (ilist 'x 'y) 'z))
  (check (iquote (x (y z)))		=> (ilist 'x (ilist 'y 'z)))

;;; --------------------------------------------------------------------
;;; vectors

  (check (iquote #(1 2))		=> '#(1 2))
  (check (iquote #(1 (2)))		=> (vector 1 (ilist 2)))
  (check (iquote (#(1 2)))		=> (ilist '#(1 2)))
  (check (iquote #(1 (2 #(3 4) 5)))	=> (vector 1 (ilist 2 (vector 3 4) 5)))
  (check (iquote #(x (y #(s u) z)))	=> (vector 'x (ilist 'y (vector 's 'u) 'z)))
  (check
      (iquote #(1 (2 #(3 (4 . 5)) 6)))
    => (vector 1 (ilist 2 (vector 3 (ipair 4 5)) 6)))

  #t)


(parametrise ((check-test-name	'vicare-quasiquotation))

  (define %eval-env
    (environment '(rnrs)
		 '(srfi :116)
		 '(srfi :116 quotations)))

  (define (%eval form)
    (eval form %eval-env))

;;; --------------------------------------------------------------------

  (check (iquasiquote 1)			=> 1)
  (check (iquasiquote (1))			=> (iquote (1)))
  (check (iquasiquote (1 . 2))			=> (iquote (1 . 2)))
  (check (iquasiquote (1 2))			=> (iquote (1 2)))
  (check (iquasiquote (1 2 . 3))		=> (iquote (1 2 . 3)))
  (check (iquasiquote ((1) (2)))		=> (iquote ((1) (2))))
  (check (iquasiquote ((1 2) (3 4)))		=> (iquote ((1 2) (3 4))))
  (check (iquasiquote ((1 2) (3 4)))		=> (iquote ((1 2) (3 4))))
  (check (iquasiquote ((1 . 2) (3 . 4)))	=> (iquote ((1 . 2) (3 . 4))))
  (check (iquasiquote ((1 . 2) . (3 . 4)))	=> (iquote ((1 . 2) . (3 . 4))))
  (check (iquasiquote ((1 2) . 3))		=> (iquote ((1 2) . 3)))
  (check (iquasiquote ((1 . 2) . 3))		=> (iquote ((1 . 2) . 3)))

  (check (iquasiquote ())			=> '())
  (check (iquasiquote (()))			=> (ilist '()))
  (check (iquasiquote ((())))			=> (ilist (ilist '())))
  (check (iquasiquote (((1))))			=> (ilist (ilist (ilist 1))))
  (check (iquasiquote (() ()))			=> (ilist '() '()))
  (check (iquasiquote (() . ()))		=> (ipair '() '()))
  (check (iquasiquote (() () ()))		=> (ilist '() '() '()))
  (check (iquasiquote (() () . ()))		=> (ipair* '() '() '()))
  (check (iquasiquote (() () . ()))		=> (ilist '() '()))
  (check (iquasiquote (() () . (1)))		=> (ipair* '() '() (ilist 1)))
  (check (iquasiquote (() () . (1 2)))		=> (ipair* '() '() (ilist 1 2)))

  (check (iquasiquote (1 . ()))			=> (iquote (1)))
  (check (iquasiquote (1 2 . ()))		=> (iquote (1 2)))
  (check (iquasiquote ((1 . 2) . ()))		=> (iquote ((1 . 2))))
  (check (iquasiquote ((1 2) . ()))		=> (iquote ((1 2))))
  (check (iquasiquote ((1 . 2) . ()))		=> (iquote ((1 . 2))))

;;; --------------------------------------------------------------------
;;; vector template

  (check (iquasiquote #(1))			=> '#(1))
  (check (iquasiquote (#(1)))			=> (iquote (#(1))))
  (check (iquasiquote (#(1) #(2)))		=> (iquote (#(1) #(2))))
  (check (iquasiquote (#(1 2) (3 4)))		=> (iquote (#(1 2) (3 4))))
  (check (iquasiquote ((1 2) #(3 4)))		=> (iquote ((1 2) #(3 4))))
  (check (iquasiquote (#(1 2) (3 . 4)))		=> (iquote (#(1 2) (3 . 4))))
  (check (iquasiquote ((1 . 2) #(3 4)))		=> (iquote ((1 . 2) #(3 4))))
  (check (iquasiquote (#(1 2) . 3))		=> (iquote (#(1 2) . 3)))
  (check (iquasiquote ((1 2) . #(3)))		=> (iquote ((1 2) . #(3))))
  (check (iquasiquote (#(1 2) . 3))		=> (iquote (#(1 2) . 3)))
  (check (iquasiquote ((1 . 2) . #(3)))		=> (iquote ((1 . 2) . #(3))))

  (check (iquasiquote #(#()))			=> '#(#()))
  (check (iquasiquote #(#() #()))		=> '#(#() #()))
  (check (iquasiquote #(#() #() #()))		=> '#(#() #() #()))
  (check (iquasiquote #(1 #() 2))		=> '#(1 #() 2))
  (check (iquasiquote #(1 #(2 3) 4))		=> '#(1 #(2 3) 4))

;;; --------------------------------------------------------------------
;;; IUNQUOTE

  (check (iquasiquote (iunquote 1))			=> 1)
  (check (iquasiquote (iunquote (iquote (1 2 3))))	=> (iquote (1 2 3)))

  ;;Invalid multi-operand IUNQUOTE form outside list and vector templates.
  ;;
  (check
      (try
	  (%eval '(iquasiquote (iunquote 1 2)))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(iunquote 1 2))

  (check (iquasiquote ((iunquote) 1))			=> (iquote (1)))
  (check (iquasiquote ((iunquote) (+ 1 2)))		=> (iquote ((+ 1 2))))
  (check (iquasiquote ((iunquote '()) 1))		=> (iquote (() 1)))
  (check (iquasiquote ((iunquote '()) ()))		=> (iquote (() ())))
  (check (iquasiquote ((iunquote '()) . ()))		=> (ipair '() '()))
  (check (iquasiquote ((iunquote 1) 2))			=> (iquote (1 2)))
  (check (iquasiquote ((iunquote (+ 1 2)) . 4))		=> (ipair 3 4))
  (check (iquasiquote ((iunquote (+ 1 2)) . (+ 8 9)))	=> (ipair 3 (iquote (+ 8 9))))
  (check (iquasiquote ((iunquote (+ 1 2)) 4))		=> (ilist 3 4))
  (check (iquasiquote ((iunquote (+ 1 2)) (+ 8 9)))	=> (ilist 3 (iquote (+ 8 9))))

  (check (iquasiquote (1 (iunquote (+ 2 3))))		=> (iquote (1 5)))
  (check (iquasiquote (1 . (iunquote (+ 2 3))))		=> (ipair 1 (+ 2 3)))

  ;;Empty IUNQUOTE form in improper tail position.
  ;;
  (check
      (try
	  (%eval '(iquasiquote (1 . (iunquote))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(iunquote))

  (check
      (iquasiquote ((iunquote (+ 10 1) (+ 20 2) (+ 30 3)) (+ 8 9)))
    => (iquote (11 22 33 (+ 8 9))))

  ;;Syntax error: improper list as IUNQUOTE form.
  ;;
  (check
      (try
	  (%eval '(iquasiquote ((iunquote (+ 10 1) (+ 20 2) . 3) (+ 8 9))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(iunquote (+ 10 1) (+ 20 2) . 3))

;;; vector templates

  (check (iquasiquote #((iunquote) 1))			=> '#(1))
  (check (iquasiquote #((iunquote) (+ 1 2)))		=> `#(,(iquote (+ 1 2))))
  (check (iquasiquote #((iunquote '()) 1))		=> '#(() 1))
  (check (iquasiquote #((iunquote '()) ()))		=> '#(() ()))
  (check (iquasiquote #((iunquote 1) 2))		=> '#(1 2))
  (check (iquasiquote #((iunquote (+ 1 2)) 4))		=> '#(3 4))
  (check (iquasiquote #((iunquote (+ 1 2)) (+ 8 9)))	=> `#(3 ,(iquote (+ 8 9))))

  (check
      (iquasiquote (1 #(2 (iunquote (+ 3. 4.)) (iunquote (+ 5. 6.)) 7) 8))
    => (ilist 1 (vector 2 (+ 3. 4.) (+ 5. 6.) 7) 8))

  (check
      (iquasiquote #((iunquote (+ 10 1) (+ 20 2) (+ 30 3)) (+ 8 9)))
    => `#(11 22 33 ,(iquote (+ 8 9))))

  (check
      (iquasiquote #(1 #((iunquote (+ 2 3)) 4) 5))
    => (vector 1 (vector (+ 2 3) 4) 5))

  (check
      (iquasiquote #(1 #((iquasiquote (+ 2 3)) 4) 5))
    => (vector 1 (vector (ilist 'iquasiquote (iquote (+ 2 3))) 4) 5))

  (check
      (iquasiquote #(1 #((iquasiquote (+ 2 (iunquote (+ 3.1 3.2))) 4) 5)))
    => (vector 1 (vector (iquote (iquasiquote (+ 2 (iunquote (+ 3.1 3.2))) 4)) 5)))

  (check
      (iquasiquote #(1 #((iquasiquote (+ 2 (iunquote (iunquote (+ 3.1 3.2)))) 4) 5)))
    => (vector 1 (vector (ilist 'iquasiquote (ilist '+ 2 (ilist 'iunquote (+ 3.1 3.2))) 4) 5)))

;;;

  ;;Syntax error: improper list as IUNQUOTE form.
  ;;
  (check
      (try
	  (%eval '(iquasiquote #((iunquote (+ 10 1) (+ 20 2) . 3) (+ 8 9))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(iunquote (+ 10 1) (+ 20 2) . 3))

;;; --------------------------------------------------------------------
;;; IUNQUOTE-SPLICING

  (check
      (iquasiquote ((iunquote-splicing (iquote (1 2 3)))))
    => (iquote (1 2 3)))

  (check (iquasiquote ((iunquote-splicing) 1))			=> (iquote (1)))
  (check (iquasiquote ((iunquote-splicing) (+ 1 2)))		=> (iquote ((+ 1 2))))
  (check (iquasiquote ((iunquote-splicing '()) 1))		=> (iquote (1)))
  (check (iquasiquote ((iunquote-splicing '()) ()))		=> (ilist '()))
  (check (iquasiquote ((iunquote-splicing '()) . ()))		=> '())
  (check (iquasiquote ((iunquote-splicing (iquote (1))) 2))	=> (iquote (1 2)))

  ;;Invalid IUNQUOTE-SPLICING form outside list and vector templates.
  ;;
  (check
      (try
	  (%eval '(iquasiquote (iunquote-splicing (ilist 1))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(iunquote-splicing (ilist 1)))

  (check (iquasiquote ((iunquote-splicing (ilist (+ 1 2))) . 4))	=> (ipair 3 4))
  (check (iquasiquote ((iunquote-splicing (ilist (+ 1 2))) . (+ 8 9)))	=> (ipair 3 (iquote (+ 8 9))))
  (check (iquasiquote ((iunquote-splicing (ilist (+ 1 2))) 4))		=> (ilist 3 4))
  (check (iquasiquote ((iunquote-splicing (ilist (+ 1 2))) (+ 8 9)))	=> (ilist 3 (iquote (+ 8 9))))

  (check (iquasiquote (1 (iunquote-splicing (ilist (+ 2 3)))))		=> (iquote (1 5)))
  (check (iquasiquote (1 . (iunquote-splicing (ilist (+ 2 3)))))	=> (iquote (1 5)))

  ;;Empty IUNQUOTE-SPLICING form in improper tail position.
  ;;
  (check
      (try
	  (%eval '(iquasiquote (1 . (iunquote-splicing))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(iunquote-splicing))

  (check
      (iquasiquote (1 #(2 (iunquote-splicing (ilist (+ 3. 4.)) (ilist (+ 5. 6.))) 7) 8))
    => (ilist 1 (vector 2 (+ 3. 4.) (+ 5. 6.) 7) 8))

  (check
      (iquasiquote ((iunquote-splicing (ilist (+ 10 1))
				       (ilist (+ 20 2))
				       (ilist (+ 30 3)))
		    (+ 8 9)))
    => (iquote (11 22 33 (+ 8 9))))

  ;;Syntax error: improper list as IUNQUOTE-SPLICING form.
  ;;
  (check
      (try
	  (%eval '(iquasiquote ((iunquote-splicing (ilist (+ 10 1))
						   (ilist (+ 20 2))
						   . 3) (+ 8 9))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(iunquote-splicing (ilist (+ 10 1)) (ilist (+ 20 2)) . 3))

  ;;Difference between IUNQUOTE and IUNQUOTE-SPLICING.
  ;;
  (check
      (iquasiquote ((iunquote (ilist (+ 10 1))
			      (ilist (+ 20 2))
			      (ilist (+ 30 3)))
		    (+ 8 9)))
    => (iquote ((11) (22) (33) (+ 8 9))))
  (check
      (iquasiquote ((iunquote-splicing (ilist (+ 10 1))
				       (ilist (+ 20 2))
				       (ilist (+ 30 3)))
		    (+ 8 9)))
    => (iquote (11 22 33 (+ 8 9))))

;;; vector templates

  (check (iquasiquote #((iunquote-splicing) 1))				=> '#(1))
  (check (iquasiquote #((iunquote-splicing) (+ 1 2)))			=> `#(,(iquote (+ 1 2))))
  (check (iquasiquote #((iunquote-splicing '()) 1))			=> '#(1))
  (check (iquasiquote #((iunquote-splicing '()) ()))			=> '#(()))
  (check (iquasiquote #((iunquote-splicing (iquote (1))) 2))		=> '#(1 2))
  (check (iquasiquote #((iunquote-splicing (ilist (+ 1 2))) 4))		=> '#(3 4))
  (check (iquasiquote #((iunquote-splicing (ilist (+ 1 2))) (+ 8 9)))	=> `#(3 ,(iquote (+ 8 9))))
  (check
      (iquasiquote #((iunquote-splicing (ilist (+ 10 1))
					(ilist (+ 20 2))
					(ilist (+ 30 3)))
		     (+ 8 9)))
    => `#(11 22 33 ,(iquote (+ 8 9))))

  (check
      (iquasiquote #(1 #((iunquote-splicing (ilist (+ 2 3))) 4) 5))
    => (vector 1 (vector (+ 2 3) 4) 5))

  (check
      (iquasiquote #(1 #((iquasiquote (+ 2 (iunquote-splicing (ilist (+ 3.1 3.2)))) 4) 5)))
    => (vector 1 (vector (iquote (iquasiquote (+ 2 (iunquote-splicing (ilist (+ 3.1 3.2)))) 4)) 5)))

  (check
      (iquasiquote #(1 #((iquasiquote (+ 2 (iunquote-splicing (iunquote-splicing (ilist (+ 3.1 3.2))))) 4) 5)))
    => (vector 1 (vector (ilist 'iquasiquote (ilist '+ 2 (ilist 'iunquote-splicing (+ 3.1 3.2))) 4) 5)))

  ;;Syntax error: improper list as IUNQUOTE-SPLICING form.
  ;;
  (check
      (try
	  (%eval '(iquasiquote #((iunquote-splicing (ilist (+ 10 1))
						    (ilist (+ 20 2))
						    . 3)
				 (+ 8 9))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(iunquote-splicing (ilist (+ 10 1)) (ilist (+ 20 2)) . 3))

;;; --------------------------------------------------------------------
;;; nested quotation

  (check (iquasiquote (1 (iquasiquote (2)) 3))		=> (iquote (1 (iquasiquote (2)) 3)))
  (check (iquasiquote (1 (iquasiquote (2)) . 3))	=> (iquote (1 (iquasiquote (2)) . 3)))

  (check (iquasiquote ((iquasiquote (2)) 3))		=> (iquote ((iquasiquote (2)) 3)))
  (check (iquasiquote ((iquasiquote (2)) . 3))		=> (iquote ((iquasiquote (2)) . 3)))

  (check (iquasiquote (1 (iquasiquote (2))))		=> (iquote (1 (iquasiquote (2)))))
  (check (iquasiquote (1 . (iquasiquote (2))))		=> (iquote (1 . (iquasiquote (2)))))

  (check
      (iquasiquote (1 (iquasiquote (iunquote (+ 1 2))) 3))
    => (iquote (1 (iquasiquote (iunquote (+ 1 2))) 3)))

  (check
      (iquasiquote (1 (iquasiquote (iunquote (iunquote (+ 1 2)))) 3))
    => (iquote (1 (iquasiquote (iunquote 3)) 3)))

  (check
      (iquasiquote (1 (iquasiquote (iunquote (iunquote-splicing (ilist (+ 1 2))))) 3))
    => (ilist 1 (ilist 'iquasiquote (ilist 'iunquote 3)) 3))

  (check
      (iquasiquote (1 (iquasiquote (iunquote (iquasiquote (iunquote (+ 1 2))))) 3))
    => (iquote (1 (iquasiquote (iunquote (iquasiquote (iunquote (+ 1 2))))) 3)))


;;; --------------------------------------------------------------------
;;; misc

  (check
      (iquasiquote (1 2 (iunquote (+ 3 4))))
    => (ilist '1 '2 (+ '3 '4)))

  (check
      (iquasiquote (1 2 (iunquote (+ 3 4)) 5))
    => (ilist '1 '2 (+ '3 '4) '5))

  (check
      (iquasiquote #(1 2 (iunquote (+ 3 4))))
    => (vector '1 '2 (+ '3 '4)))

  #t)


(parametrise ((check-test-name	'ilist))

  (define (xcons a b)
    (cons b a))

;;; --------------------------------------------------------------------

  (check
      (let ((iter (make-ilist-iteration-thunk '())))
        (iteration-thunk-fold xcons '() iter))
    => '())

  (check
      (let ((iter (make-ilist-iteration-thunk (ilist 0))))
        (iteration-thunk-fold xcons '() iter))
    => '(0))

  (check
      (let ((iter (make-ilist-iteration-thunk (ilist 0 1 2 3 4))))
        (iteration-thunk-fold xcons '() iter))
    => '(4 3 2 1 0))

  #t)


;;;; done

(collect 4)
(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
