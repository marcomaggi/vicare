;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 42
;;;Date: Thu Feb  7, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;sebastian.egner@philips.com, Eindhoven, The Netherlands, 25-Apr-2005
;;;Copyright (c) 2008 Derick Eddington
;;;Vicare integration by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (except (vicare)
		read-line)
  (srfi :42)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI 42: eager comprehensions\n")


;;;; do-ec

(check
    (let ((x 0)) (do-ec (set! x (+ x 1))) x)
  => 1)

(check
    (let ((x 0)) (do-ec (:range i 0) (set! x (+ x 1))) x)
  => 0)

(check
    (let ((x 0)) (do-ec (:range i 10) (set! x (+ x 1))) x)
  => 10)

(check
    (let ((x 0)) (do-ec (:range n 10) (:range k n) (set! x (+ x 1))) x)
  => 45)


;;;; list-ec and basic qualifiers

(check (list-ec 1) => '(1))

(check (list-ec (:range i 4) i) => '(0 1 2 3))

(check (list-ec (:range n 3) (:range k (+ n 1)) (list n k))
  => '((0 0) (1 0) (1 1) (2 0) (2 1) (2 2)) )

(check
    (list-ec (:range n 5) (if (even? n)) (:range k (+ n 1)) (list n k))
  => '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )

(check
    (list-ec (:range n 5) (not (even? n)) (:range k (+ n 1)) (list n k))
  => '((1 0) (1 1) (3 0) (3 1) (3 2) (3 3)) )

(check
    (list-ec (:range n 5)
	     (and (even? n) (> n 2))
	     (:range k (+ n 1))
	     (list n k) )
  => '((4 0) (4 1) (4 2) (4 3) (4 4)) )

(check
    (list-ec (:range n 5)
	     (or (even? n) (> n 3))
	     (:range k (+ n 1))
	     (list n k) )
  => '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )

(check
    (let ((x 0)) (list-ec (:range n 10) (begin (set! x (+ x 1))) n) x)
  => 10 )

(check
    (list-ec (nested (:range n 3) (:range k n)) k)
  => '(0 0 1) )


;;;; other comprehensions

(check (append-ec '(a b)) => '(a b))
(check (append-ec (:range i 0) '(a b)) => '())
(check (append-ec (:range i 1) '(a b)) => '(a b))
(check (append-ec (:range i 2) '(a b)) => '(a b a b))

(check (string-ec #\a) => (string #\a))
(check (string-ec (:range i 0) #\a) => "")
(check (string-ec (:range i 1) #\a) => "a")
(check (string-ec (:range i 2) #\a) => "aa")

(check (string-append-ec "ab") => "ab")
(check (string-append-ec (:range i 0) "ab") => "")
(check (string-append-ec (:range i 1) "ab") => "ab")
(check (string-append-ec (:range i 2) "ab") => "abab")

(check (vector-ec 1) => (vector 1))
(check (vector-ec (:range i 0) i) => (vector))
(check (vector-ec (:range i 1) i) => (vector 0))
(check (vector-ec (:range i 2) i) => (vector 0 1))

(check (vector-of-length-ec 1 1) => (vector 1))
(check (vector-of-length-ec 0 (:range i 0) i) => (vector))
(check (vector-of-length-ec 1 (:range i 1) i) => (vector 0))
(check (vector-of-length-ec 2 (:range i 2) i) => (vector 0 1))

(check (sum-ec 1) => 1)
(check (sum-ec (:range i 0) i) => 0)
(check (sum-ec (:range i 1) i) => 0)
(check (sum-ec (:range i 2) i) => 1)
(check (sum-ec (:range i 3) i) => 3)

(check (product-ec 1) => 1)
(check (product-ec (:range i 1 0) i) => 1)
(check (product-ec (:range i 1 1) i) => 1)
(check (product-ec (:range i 9 9) i) => 1)
(check (product-ec (:range i 1 2) i) => 1)
(check (product-ec (:range i 1 3) i) => 2)
(check (product-ec (:range i 1 4) i) => 6)

(check (min-ec 1) => 1)
(check (min-ec (:range i 1) i) => 0)
(check (min-ec (:range i 2) i) => 0)

(check (max-ec 1) => 1)
(check (max-ec (:range i 1) i) => 0)
(check (max-ec (:range i 2) i) => 1)

(check (first-ec #f 1) => 1)
(check (first-ec #f (:range i 0) i) => #f)
(check (first-ec #f (:range i 1) i) => 0)
(check (first-ec #f (:range i 2) i) => 0)

(check
    (let ((last-i -1))
      (first-ec #f (:range i 10) (begin (set! last-i i)) i)
      last-i )
  => 0 )

(check (last-ec #f 1) => 1)
(check (last-ec #f (:range i 0) i) => #f)
(check (last-ec #f (:range i 1) i) => 0)
(check (last-ec #f (:range i 2) i) => 1)

(check (any?-ec #f) => #f)
(check (any?-ec #t) => #t)
(check (any?-ec (:range i 2 2) (even? i)) => #f)
(check (any?-ec (:range i 2 3) (even? i)) => #t)

(check (every?-ec #f) => #f)
(check (every?-ec #t) => #t)
(check (every?-ec (:range i 2 2) (even? i)) => #t)
(check (every?-ec (:range i 2 3) (even? i)) => #t)
(check (every?-ec (:range i 2 4) (even? i)) => #f)

(check
    (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
      (fold-ec 0 (:range i 10) i sum-sqr) )
  => 285 )

(check
    (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
      (fold-ec 0 (:range i 1 5) i sum-sqr) )
  => (+ 1 (* 2 2) (* 3 3) (* 4 4)) )

(check
    (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
      (fold-ec 0 (:range i 1 5) i sum-sqr) )
  => 30)

(check
    (let ((minus-1 (lambda (x) (- x 1)))
	  (sum-sqr (lambda (x result) (+ result (* x x)))))
      (fold3-ec (error #f "wrong") (:range i 10) i minus-1 sum-sqr) )
  => 284 )

(check
    (fold3-ec 'infinity (:range i 0) i min min)
  => 'infinity )

(check
    (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
      (fold3-ec 1234 (:range i 2 5)
		i
		(lambda (x) x)
		sum-sqr))
  => 27)

(check
    (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
      (fold3-ec 1234 (:range i 2 2)
		i
		(lambda (x) x)
		sum-sqr))
  => 1234)


;;;; typed generators

(check (list-ec (:list x '()) x) => '())
(check (list-ec (:list x '(1)) x) => '(1))
(check (list-ec (:list x '(1 2 3)) x) => '(1 2 3))
(check (list-ec (:list x '(1) '(2)) x) => '(1 2))
(check (list-ec (:list x '(1) '(2) '(3)) x) => '(1 2 3))

(check (list-ec (:string c "") c) => '())
(check (list-ec (:string c "1") c) => '(#\1))
(check (list-ec (:string c "123") c) => '(#\1 #\2 #\3))
(check (list-ec (:string c "1" "2") c) => '(#\1 #\2))
(check (list-ec (:string c "1" "2" "3") c) => '(#\1 #\2 #\3))

(check (list-ec (:vector x (vector)) x) => '())
(check (list-ec (:vector x (vector 1)) x) => '(1))
(check (list-ec (:vector x (vector 1 2 3)) x) => '(1 2 3))
(check (list-ec (:vector x (vector 1) (vector 2)) x) => '(1 2))
(check
    (list-ec (:vector x (vector 1) (vector 2) (vector 3)) x)
  => '(1 2 3))

(check (list-ec (:range x -2) x) => '())
(check (list-ec (:range x -1) x) => '())
(check (list-ec (:range x  0) x) => '())
(check (list-ec (:range x  1) x) => '(0))
(check (list-ec (:range x  2) x) => '(0 1))

(check (list-ec (:range x  0  3) x) => '(0 1 2))
(check (list-ec (:range x  1  3) x) => '(1 2))
(check (list-ec (:range x -2 -1) x) => '(-2))
(check (list-ec (:range x -2 -2) x) => '())

(check (list-ec (:range x 1 5  2) x) => '(1 3))
(check (list-ec (:range x 1 6  2) x) => '(1 3 5))
(check (list-ec (:range x 5 1 -2) x) => '(5 3))
(check (list-ec (:range x 6 1 -2) x) => '(6 4 2))

(check (list-ec (:real-range x 0.0 3.0)     x) => '(0. 1. 2.))
(check (list-ec (:real-range x 0   3.0)     x) => '(0. 1. 2.))
(check (list-ec (:real-range x 0   3   1.0) x) => '(0. 1. 2.))

(check
    (string-ec (:char-range c #\a #\z) c)
  => "abcdefghijklmnopqrstuvwxyz" )

(check
    (unwind-protect
	(begin
	  (let ((f (open-file-output-port "tmp1"
					  (file-options no-fail)
					  (buffer-mode block)
					  (native-transcoder))))
	    (do-ec (:range n 10) (begin (write n f) (newline f)))
	    (close-output-port f))
	  (call-with-input-file "tmp1"
	    (lambda (port)
	      (list-ec (:port x port read) x))))
      (when (file-exists? "tmp1")
	(delete-file "tmp1")))
  => (list-ec (:range n 10) n) )

(check
    (unwind-protect
	(begin
	  (let ((f (open-file-output-port "tmp1"
					  (file-options no-fail)
					  (buffer-mode block)
					  (native-transcoder))))
	    (do-ec (:range n 10) (begin (write n f) (newline f)))
	    (close-output-port f))
	  (call-with-input-file "tmp1"
	    (lambda (port) (list-ec (:port x port) x)) ))
      (when (file-exists? "tmp1")
	(delete-file "tmp1")))
  => (list-ec (:range n 10) n))


;;;; the special generators :do :let :parallel :while :until

(check (list-ec (:do ((i 0)) (< i 4) ((+ i 1))) i) => '(0 1 2 3))

(check
    (list-ec
     (:do (let ((x 'x)))
	  ((i 0))
	  (< i 4)
	  (let ((j (- 10 i))))
	  #t
	  ((+ i 1)) )
     j )
  => '(10 9 8 7) )

(check (list-ec (:let x 1) x) => '(1))
(check (list-ec (:let x 1) (:let y (+ x 1)) y) => '(2))
(check (list-ec (:let x 1) (:let x (+ x 1)) x) => '(2))

(check
    (list-ec (:parallel (:range i 1 10) (:list x '(a b c))) (list i x))
  => '((1 a) (2 b) (3 c)) )

(check
    (list-ec (:while (:range i 1 10) (< i 5)) i)
  => '(1 2 3 4) )

(check
    (list-ec (:until (:range i 1 10) (>= i 5)) i)
  => '(1 2 3 4 5) )

		; with generator that might use inner bindings

(check
    (list-ec (:while (:list i '(1 2 3 4 5 6 7 8 9)) (< i 5)) i)
  => '(1 2 3 4) )
		; Was broken in original reference implementation as pointed
		; out by sunnan@handgranat.org on 24-Apr-2005 comp.lang.scheme.
		; Refer to http://groups-beta.google.com/group/comp.lang.scheme/
		; browse_thread/thread/f5333220eaeeed66/75926634cf31c038#75926634cf31c038

(check
    (list-ec (:until (:list i '(1 2 3 4 5 6 7 8 9)) (>= i 5)) i)
  => '(1 2 3 4 5) )

(check
    (list-ec (:while (:vector x (index i) '#(1 2 3 4 5 6 7))
		     (< x 6))
	     x)
  => '(1 2 3 4 5))
		;Was broken in  reference implementation, even after fix
		;for  the  bug  reported   by  Sunnan,  as  reported  by
		;Jens-Axel Soegaard on 4-Jun-2007.

		; combine :while/:until and :parallel

(check
    (list-ec (:while (:parallel (:range i 1 10)
				(:list j '(1 2 3 4 5 6 7 8 9)))
		     (< i 5))
	     (list i j))
  => '((1 1) (2 2) (3 3) (4 4)))

(check
    (list-ec (:until (:parallel (:range i 1 10)
				(:list j '(1 2 3 4 5 6 7 8 9)))
		     (>= i 5))
	     (list i j))
  => '((1 1) (2 2) (3 3) (4 4) (5 5)))

		; check that :while/:until really stop the generator

(check
    (let ((n 0))
      (do-ec (:while (:range i 1 10)
		     (begin (set! n (+ n 1)) (< i 5)))
	     (if #f #f))
      n)
  => 5)

(check
    (let ((n 0))
      (do-ec (:until (:range i 1 10) (begin (set! n (+ n 1)) (>= i 5)))
	     (if #f #f))
      n)
  => 5)

(check
    (let ((n 0))
      (do-ec (:while (:parallel (:range i 1 10)
				(:do () (begin (set! n (+ n 1)) #t) ()))
		     (< i 5))
	     (if #f #f))
      n)
  => 5)

(check
    (let ((n 0))
      (do-ec (:until (:parallel (:range i 1 10)
				(:do () (begin (set! n (+ n 1)) #t) ()))
		     (>= i 5))
	     (if #f #f))
      n)
  => 5)


;;;; the dispatching generator

(check (list-ec (: c '(a b)) c) => '(a b))
(check (list-ec (: c '(a b) '(c d)) c) => '(a b c d))

(check (list-ec (: c "ab") c) => '(#\a #\b))
(check (list-ec (: c "ab" "cd") c) => '(#\a #\b #\c #\d))

(check (list-ec (: c (vector 'a 'b)) c) => '(a b))
(check (list-ec (: c (vector 'a 'b) (vector 'c)) c) => '(a b c))

(check (list-ec (: i 0) i) => '())
(check (list-ec (: i 1) i) => '(0))
(check (list-ec (: i 10) i) => '(0 1 2 3 4 5 6 7 8 9))
(check (list-ec (: i 1 2) i) => '(1))
(check (list-ec (: i 1 2 3) i) => '(1))
(check (list-ec (: i 1 9 3) i) => '(1 4 7))

;;This should run the :real-range generator.
(check (list-ec (: i 0.0 1.0 0.2) i) => '(0. 0.2 0.4 0.6000000000000001 0.8))

(check (list-ec (: c #\a #\c) c) => '(#\a #\b #\c))

(check
    (unwind-protect
	(begin
	  (let ((f (open-file-output-port "tmp1" (file-options no-fail)
					  (buffer-mode block) (native-transcoder))))
	    (do-ec (:range n 10) (begin (write n f) (newline f)))
	    (close-output-port f))
	  (call-with-input-file "tmp1"
	    (lambda (port) (list-ec (: x port read) x)) ))
      (when (file-exists? "tmp1")
	(delete-file "tmp1")))
        => (list-ec (:range n 10) n) )

(check
    (unwind-protect
	(begin
	  (let ((f (open-file-output-port "tmp1" (file-options no-fail)
					  (buffer-mode block) (native-transcoder))))
	    (do-ec (:range n 10) (begin (write n f) (newline f)))
	    (close-output-port f))
	  (call-with-input-file "tmp1"
	    (lambda (port) (list-ec (: x port) x)) ))
      (when (file-exists? "tmp1")
	(delete-file "tmp1")))
  => (list-ec (:range n 10) n))


;;;; with index variable

(check (list-ec (:list c (index i) '(a b)) (list c i)) => '((a 0) (b 1)))
(check (list-ec (:string c (index i) "a") (list c i)) => '((#\a 0)))
(check (list-ec (:vector c (index i) (vector 'a)) (list c i)) => '((a 0)))

(check
    (list-ec (:range i (index j) 0 -3 -1) (list i j))
  => '((0 0) (-1 1) (-2 2)) )

(check
    (list-ec (:real-range i (index j) 0 1 0.2) (list i j))
  => '((0. 0) (0.2 1) (0.4 2) (0.6000000000000001 3) (0.8 4)) )

(check
    (list-ec (:char-range c (index i) #\a #\c) (list c i))
  => '((#\a 0) (#\b 1) (#\c 2)) )

(check
    (list-ec (: x (index i) '(a b c d)) (list x i))
  => '((a 0) (b 1) (c 2) (d 3)) )

(check
    (unwind-protect
	(begin
	  (let ((f (open-file-output-port "tmp1" (file-options no-fail)
					  (buffer-mode block) (native-transcoder))))
	    (do-ec (:range n 10) (begin (write n f) (newline f)))
	    (close-output-port f))
	  (call-with-input-file "tmp1"
	    (lambda (port) (list-ec (: x (index i) port) (list x i)))))
      (when (file-exists? "tmp1")
	(delete-file "tmp1")))
  => '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9)) )


;;;; the examples from the SRFI document

		; from Abstract

(check (list-ec (: i 5) (* i i)) => '(0 1 4 9 16))

(check
    (list-ec (: n 1 4) (: i n) (list n i))
  => '((1 0) (2 0) (2 1) (3 0) (3 1) (3 2)) )

		; from Generators

(check
    (list-ec (: x (index i) "abc") (list x i))
  => '((#\a 0) (#\b 1) (#\c 2)) )

(check
    (list-ec (:string c (index i) "a" "b") (cons c i))
  => '((#\a . 0) (#\b . 1)) )


;;;; little shop of horrors

(check (list-ec (:range x 5) (:range x x) x) => '(0 0 1 0 1 2 0 1 2 3))

(check (list-ec (:list x '(2 "23" (4))) (: y x) y) => '(0 1 #\2 #\3 4))

(check
    (list-ec (:parallel (:integers x)
			(:do ((i 10)) (< x i) ((- i 1))))
	     (list x i))
  => '((0 10) (1 9) (2 8) (3 7) (4 6)) )

(check
    (list-ec (:range i 10)
	     (if (even? i))
	     i)
  => '(0 2 4 6 8))

(check
    (list-ec (:range i 5)
	     (if (even? i))
	     (:let j (+ 1 i))
	     j)
  => '(1 3 5))

(check
    (let* ((ans '())
	   (ell (list-ec (:range i 5)
			 (begin
			   (set! ans (cons i ans)))
			 i)))
      (list ans ell))
  => '((4 3 2 1 0) (0 1 2 3 4)))

(check
    (list-ec (nested (:range i 5)
		     (if (even? i))
		     (:let j (+ 1 i)))
	     j)
  => '(1 3 5))

(check
    (list-ec (:range i 5)
	     (nested (if (even? i))
		     (:let j (+ 1 i)))
	     j)
  => '(1 3 5))

(check
    (list-ec (:range i 5)
	     (if (even? i))
	     (nested (:let j (+ 1 i)))
	     j)
  => '(1 3 5))

(check
    (list-ec (:range i 5)
	     (if (even? i))
	     (:let j (+ 1 i))
	     (nested)
	     j)
  => '(1 3 5))

(check
    (list-ec (:string c (index i) "ciao" "mamma")
	     (cons c i))
  => '((#\c . 0) (#\i . 1) (#\a . 2) (#\o . 3)
       (#\m . 4) (#\a . 5) (#\m . 6) (#\m . 7) (#\a . 8)))



;;;; less artificial examples

(define (factorial n) ; n * (n-1) * .. * 1 for n >= 0
  (product-ec (:range k 2 (+ n 1)) k) )

(check (factorial  0) => 1)
(check (factorial  1) => 1)
(check (factorial  3) => 6)
(check (factorial  5) => 120)


(define (eratosthenes n) ; primes in {2..n-1} for n >= 1
  (let ((p? (make-string n #\1)))
    (do-ec (:range k 2 n)
           (if (char=? (string-ref p? k) #\1))
           (:range i (* 2 k) n k)
           (string-set! p? i #\0) )
    (list-ec (:range k 2 n) (if (char=? (string-ref p? k) #\1)) k) ))

(check
    (eratosthenes 50)
  => '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) )

(check
    (length (eratosthenes 100000))
  => 9592 ) ; we expect 10^5/ln(10^5)


(define (pythagoras n) ; a, b, c s.t. 1 <= a <= b <= c <= n, a^2 + b^2 = c^2
  (list-ec
   (:let sqr-n (* n n))
   (:range a 1 (+ n 1))
		; (begin (display a) (display " "))
   (:let sqr-a (* a a))
   (:range b a (+ n 1))
   (:let sqr-c (+ sqr-a (* b b)))
   (if (<= sqr-c sqr-n))
   (:range c b (+ n 1))
   (if (= (* c c) sqr-c))
   (list a b c) ))

(check
    (pythagoras 15)
  => '((3 4 5) (5 12 13) (6 8 10) (9 12 15)) )

(check
    (length (pythagoras 200))
  => 127 )


(define (qsort xs) ; stable
  (if (null? xs)
      '()
    (let ((pivot (car xs)) (xrest (cdr xs)))
      (append
       (qsort (list-ec (:list x xrest) (if (<  x pivot)) x))
       (list pivot)
       (qsort (list-ec (:list x xrest) (if (>= x pivot)) x)) ))))

(check
    (qsort '(1 5 4 2 4 5 3 2 1 3))
  => '(1 1 2 2 3 3 4 4 5 5) )


(define (pi-BBP m) ; approx. of pi within 16^-m (Bailey-Borwein-Plouffe)
  (sum-ec
   (:range n 0 (+ m 1))
   (:let n8 (* 8 n))
   (* (- (/ 4 (+ n8 1))
	 (+ (/ 2 (+ n8 4))
	    (/ 1 (+ n8 5))
	    (/ 1 (+ n8 6))))
      (/ 1 (expt 16 n)) )))

(check
    (pi-BBP 5)
  => (/ 40413742330349316707 12864093722915635200) )


(define (read-line port) ; next line (incl. #\newline) of port
  (let ((line
         (string-ec
          (:until (:port c port read-char)
                  (char=? c #\newline) )
          c )))
    (if (string=? line "")
        (read-char port) ; eof-object
      line )))

(define (read-lines filename) ; list of all lines
  (call-with-input-file
      filename
    (lambda (port)
      (list-ec (:port line port read-line) line) )))

(check
    (unwind-protect
	(begin
	  (let ((f (open-file-output-port "tmp1"
					  (file-options no-fail)
					  (buffer-mode block)
					  (native-transcoder))))
	    (do-ec (:range n 10) (begin (write n f) (newline f)))
	    (close-output-port f))
	  (read-lines "tmp1"))
      (when (file-exists? "tmp1")
	(delete-file "tmp1")))
  => (list-ec (:char-range c #\0 #\9) (string c #\newline)))


;;;; done

(check-report)

;;; end of file


;;;; done

(check-report)

;;; end of file
