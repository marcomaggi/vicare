;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the (comparisons) library
;;;Date: Thu Aug 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(import (vicare)
  (vicare checks)
  (vicare language-extensions comparisons))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: comparisons\n")


(parameterise ((check-test-name	'examples))

  (define-record-type length
    (fields (immutable meters	meters)))

  (define-record-type box
    (fields (immutable width	width)
	    (immutable height	height)
	    (immutable depth	depth)))

  (define-record-type bowl
    (fields (immutable radius	radius)
	    (immutable open?	open?)))

  (define (length-compare a b)
    (real-compare (meters a) (meters b)))

  (define (box-compare a b)
    (refine-compare
     (length-compare (width  a) (width  b))
     (length-compare (height a) (height b))
     (length-compare (depth  a) (depth  b))))

  (define (length-doit2 op n m)
    (let ((a (make-length n))
	  (b (make-length m)))
      (op length-compare a b)))

  (define (length-doit3 op p q r)
    (let ((a (make-length p))
	  (b (make-length q))
	  (c (make-length r)))
      (op length-compare a b c)))

  (define (box-doit2 op  w1 h1 d1  w2 h2 d2)
    (let ((a (make-box (make-length w1) (make-length h1) (make-length d1)))
	  (b (make-box (make-length w2) (make-length h2) (make-length d2))))
      (op box-compare a b)))

  (define (container-compare c1 c2)
    (select-compare c1 c2
		    (bowl? (boolean-compare (open?  c1) (open?  c2))
			   (length-compare  (radius c1) (radius c2)))
		    (box?  (box-compare c1 c2))
		    (else "neither bowls nor boxes" c1 c2)))

;;; --------------------------------------------------------------------

  (check-for-true	(length-doit2 <? 10 20))
  (check-for-false	(length-doit2 <? 10 10))
  (check-for-false	(length-doit2 <? 20 10))

  (check-for-true	(length-doit2 <=? 10 20))
  (check-for-true	(length-doit2 <=? 10 10))
  (check-for-false	(length-doit2 <=? 20 10))

  (check-for-false	(length-doit2 >? 10 20))
  (check-for-false	(length-doit2 >? 10 10))
  (check-for-true	(length-doit2 >? 20 10))

  (check-for-false	(length-doit2 >=? 10 20))
  (check-for-true	(length-doit2 >=? 10 10))
  (check-for-true	(length-doit2 >=? 20 10))

;;; --------------------------------------------------------------------

  (check-for-false	(length-doit3 </<=? 20 20 20))
  (check-for-true	(length-doit3 </<=? 10 20 20))
  (check-for-false	(length-doit3 </<=? 30 20 20))
  (check-for-false	(length-doit3 </<=? 20 10 20))
  (check-for-false	(length-doit3 </<=? 20 30 20))
  (check-for-false	(length-doit3 </<=? 20 20 10))
  (check-for-false	(length-doit3 </<=? 20 20 30))
  (check-for-true	(length-doit3 </<=? 10 20 30))
  (check-for-false	(length-doit3 </<=? 30 20 10))

  (check-for-false	(length-doit3 <=/<? 20 20 20))
  (check-for-false	(length-doit3 <=/<? 10 20 20))
  (check-for-false	(length-doit3 <=/<? 30 20 20))
  (check-for-false	(length-doit3 <=/<? 20 10 20))
  (check-for-false	(length-doit3 <=/<? 20 30 20))
  (check-for-false	(length-doit3 <=/<? 20 20 10))
  (check-for-true	(length-doit3 <=/<? 20 20 30))
  (check-for-true	(length-doit3 <=/<? 10 20 30))
  (check-for-false	(length-doit3 <=/<? 30 20 10))

  (check-for-false	(length-doit3 >/>=? 20 20 20))
  (check-for-false	(length-doit3 >/>=? 10 20 20))
  (check-for-true	(length-doit3 >/>=? 30 20 20))
  (check-for-false	(length-doit3 >/>=? 20 10 20))
  (check-for-false	(length-doit3 >/>=? 20 30 20))
  (check-for-false	(length-doit3 >/>=? 20 20 10))
  (check-for-false	(length-doit3 >/>=? 20 20 30))
  (check-for-false	(length-doit3 >/>=? 10 20 30))
  (check-for-true	(length-doit3 >/>=? 30 20 10))

  (check-for-false	(length-doit3 >=/>? 20 20 20))
  (check-for-false	(length-doit3 >=/>? 10 20 20))
  (check-for-false	(length-doit3 >=/>? 30 20 20))
  (check-for-false	(length-doit3 >=/>? 20 10 20))
  (check-for-false	(length-doit3 >=/>? 20 30 20))
  (check-for-true	(length-doit3 >=/>? 20 20 10))
  (check-for-false	(length-doit3 >=/>? 20 20 30))
  (check-for-false	(length-doit3 >=/>? 10 20 30))
  (check-for-true	(length-doit3 >=/>? 30 20 10))

;;; --------------------------------------------------------------------

  (check-for-true	(box-doit2 <?  10 10 10  20 20 20))
  (check-for-true	(box-doit2 <?  10 10 10  10 20 20))
  (check-for-true	(box-doit2 <?  10 10 10  10 10 20))
  (check-for-false	(box-doit2 <?  10 10 10  10 10 10))
  (check-for-false	(box-doit2 <?  20 10 10  10 10 10))
  (check-for-false	(box-doit2 <?  10 20 10  10 10 10))
  (check-for-false	(box-doit2 <?  10 10 20  10 10 10))

;;; --------------------------------------------------------------------

  (check-for-true	(<? container-compare
			    (make-box (make-length 10) (make-length 10) (make-length 10))
			    (make-box (make-length 20) (make-length 20) (make-length 20))))

  (check-for-true	(<? container-compare
			    (make-bowl (make-length 10) #t)
			    (make-bowl (make-length 20) #t)))


  #t)


(parameterise ((check-test-name	'atoms))

  (check (boolean-compare #t #t) => 0)
  (check (boolean-compare #f #f) => 0)
  (check (boolean-compare #f #t) => -1)
  (check (boolean-compare #t #f) => +1)

;;; --------------------------------------------------------------------

  (check (char-compare #\a #\a) => 0)
  (check (char-compare #\a #\b) => -1)
  (check (char-compare #\b #\a) => +1)

  (check (char-compare #\A #\A) => 0)
  (check (char-compare #\A #\B) => -1)
  (check (char-compare #\B #\A) => +1)

  (check (char-compare #\a #\A) => +1)
  (check (char-compare #\A #\a) => -1)

  (check (char-compare-ci #\a #\a) => 0)
  (check (char-compare-ci #\a #\b) => -1)
  (check (char-compare-ci #\b #\a) => +1)

  (check (char-compare-ci #\A #\A) => 0)
  (check (char-compare-ci #\A #\B) => -1)
  (check (char-compare-ci #\B #\A) => +1)

  (check (char-compare-ci #\a #\A) => 0)
  (check (char-compare-ci #\A #\a) => 0)

;;; --------------------------------------------------------------------

  (check (string-compare "a" "a") => 0)
  (check (string-compare "a" "b") => -1)
  (check (string-compare "b" "a") => +1)

  (check (string-compare "A" "A") => 0)
  (check (string-compare "A" "B") => -1)
  (check (string-compare "B" "A") => +1)

  (check (string-compare "a" "A") => +1)
  (check (string-compare "A" "a") => -1)

  (check (string-compare-ci "a" "a") => 0)
  (check (string-compare-ci "a" "b") => -1)
  (check (string-compare-ci "b" "a") => +1)

  (check (string-compare-ci "A" "A") => 0)
  (check (string-compare-ci "A" "B") => -1)
  (check (string-compare-ci "B" "A") => +1)

  (check (string-compare-ci "a" "A") => 0)
  (check (string-compare-ci "A" "a") => 0)

;;; --------------------------------------------------------------------

  (check (symbol-compare 'alpha 'alpha) => 0)
  (check (symbol-compare 'alpha 'beta)  => -1)
  (check (symbol-compare 'beta 'alpha)  => +1)

;;; --------------------------------------------------------------------

  (check (integer-compare 1 1) => 0)
  (check (integer-compare 1 2) => -1)
  (check (integer-compare 2 1) => +1)

  (check (rational-compare 1 1) => 0)
  (check (rational-compare 1 2) => -1)
  (check (rational-compare 2 1) => +1)
  (check (rational-compare 1/3 1/3) => 0)
  (check (rational-compare 1/3 2/3) => -1)
  (check (rational-compare 2/3 1/3) => +1)

  (check (real-compare 1 1) => 0)
  (check (real-compare 1 2) => -1)
  (check (real-compare 2 1) => +1)
  (check (real-compare 1/3 1/3) => 0)
  (check (real-compare 1/3 2/3) => -1)
  (check (real-compare 2/3 1/3) => +1)
  (check (real-compare 1. 1.) => 0)
  (check (real-compare 1. 2.) => -1)
  (check (real-compare 2. 1.) => +1)

  (check (complex-compare 1 1) => 0)
  (check (complex-compare 1 2) => -1)
  (check (complex-compare 2 1) => +1)
  (check (complex-compare 1/3 1/3) => 0)
  (check (complex-compare 1/3 2/3) => -1)
  (check (complex-compare 2/3 1/3) => +1)
  (check (complex-compare 1. 1.) => 0)
  (check (complex-compare 1. 2.) => -1)
  (check (complex-compare 2. 1.) => +1)
  (check (complex-compare 1+2i 1+2i) => 0)
  (check (complex-compare 1+2i 2+2i) => -1)
  (check (complex-compare 2+2i 1+2i) => +1)

  (check (number-compare 1 1) => 0)
  (check (number-compare 1 2) => -1)
  (check (number-compare 2 1) => +1)
  (check (number-compare 1/3 1/3) => 0)
  (check (number-compare 1/3 2/3) => -1)
  (check (number-compare 2/3 1/3) => +1)
  (check (number-compare 1. 1.) => 0)
  (check (number-compare 1. 2.) => -1)
  (check (number-compare 2. 1.) => +1)
  (check (number-compare 1+2i 1+2i) => 0)
  (check (number-compare 1+2i 2+2i) => -1)
  (check (number-compare 2+2i 1+2i) => +1)

  (check
      (guard (exc (else (condition-message exc)))
	(integer-compare +nan.0 +nan.0))
    => "expected integer value")

  (check
      (guard (exc (else (condition-message exc)))
	(rational-compare +nan.0 +nan.0))
    => "expected rational value")

  (check (real-compare +nan.0 +nan.0) => +nan.0)
  (check (real-compare +nan.0      1) => +nan.0)
  (check (real-compare 1      +nan.0) => +nan.0)

  (check (complex-compare +nan.0 +nan.0) => +nan.0)
  (check (complex-compare +nan.0      1) => +nan.0)
  (check (complex-compare 1      +nan.0) => +nan.0)

  (check (number-compare +nan.0 +nan.0) => +nan.0)
  (check (number-compare +nan.0      1) => +nan.0)
  (check (number-compare 1      +nan.0) => +nan.0)

  (check
      (guard (exc (else (condition-message exc)))
	(integer-compare +inf.0 +inf.0))
    => "expected integer value")

  (check
      (guard (exc (else (condition-message exc)))
	(rational-compare +inf.0 +inf.0))
    => "expected rational value")

  (check (real-compare +inf.0 +inf.0) => 0)
  (check (real-compare +inf.0      1) => +1)
  (check (real-compare 1      +inf.0) => -1)

  (check (real-compare -inf.0 -inf.0) => 0)
  (check (real-compare -inf.0      1) => -1)
  (check (real-compare 1      -inf.0) => +1)

  (check (complex-compare +inf.0 +inf.0) => 0)
  (check (complex-compare +inf.0      1) => +1)
  (check (complex-compare 1      +inf.0) => -1)

  (check (complex-compare -inf.0 -inf.0) => 0)
  (check (complex-compare -inf.0      1) => +1)
  (check (complex-compare 1      -inf.0) => -1)

  (check (number-compare +inf.0 +inf.0) => 0)
  (check (number-compare +inf.0      1) => +1)
  (check (number-compare 1      +inf.0) => -1)

  (check (number-compare -inf.0 -inf.0) => 0)
  (check (number-compare -inf.0      1) => +1)
  (check (number-compare 1      -inf.0) => -1)

  #t)


(parameterise ((check-test-name	'compound))

  (check (list-compare '()  '())		=> 0)
  (check (list-compare '(1) '())		=> +1)
  (check (list-compare '()  '(1))		=> -1)

  (check (list-compare '(1) '(1))		=> 0)
  (check (list-compare '(1) '(2))		=> -1)
  (check (list-compare '(2) '(1))		=> +1)

  (check (list-compare '(1 1) '(1 1))		=> 0)
  (check (list-compare '(1 1) '(1 2))		=> -1)
  (check (list-compare '(1 2) '(1 1))		=> +1)

  (check (list-compare '(1 1 1) '(1 1))		=> +1)
  (check (list-compare '(1 1)   '(1 1 1))	=> -1)

  (check (list-compare-as-vector '()  '())		=> 0)
  (check (list-compare-as-vector '(1) '())		=> +1)
  (check (list-compare-as-vector '()  '(1))		=> -1)

  (check (list-compare-as-vector '(1) '(1))		=> 0)
  (check (list-compare-as-vector '(1) '(2))		=> -1)
  (check (list-compare-as-vector '(2) '(1))		=> +1)

  (check (list-compare-as-vector '(1 1) '(1 1))		=> 0)
  (check (list-compare-as-vector '(1 1) '(1 2))		=> -1)
  (check (list-compare-as-vector '(1 2) '(1 1))		=> +1)

  (check (list-compare-as-vector '(1 1 1) '(1 1))	=> +1)
  (check (list-compare-as-vector '(1 1)   '(1 1 1))	=> -1)

  (check (vector-compare '#()  '#())		=> 0)
  (check (vector-compare '#(1) '#())		=> +1)
  (check (vector-compare '#()  '#(1))		=> -1)

  (check (vector-compare '#(1) '#(1))		=> 0)
  (check (vector-compare '#(1) '#(2))		=> -1)
  (check (vector-compare '#(2) '#(1))		=> +1)

  (check (vector-compare '#(1 1) '#(1 1))	=> 0)
  (check (vector-compare '#(1 1) '#(1 2))	=> -1)
  (check (vector-compare '#(1 2) '#(1 1))	=> +1)

  (check (vector-compare '#(1 1 1) '#(1 1))	=> +1)
  (check (vector-compare '#(1 1)   '#(1 1 1))	=> -1)

  (check (vector-compare '#(1 1 1) '#(2 1))	=> +1)

  (check (vector-compare-as-list '#()  '#())		=> 0)
  (check (vector-compare-as-list '#(1) '#())		=> +1)
  (check (vector-compare-as-list '#()  '#(1))		=> -1)

  (check (vector-compare-as-list '#(1) '#(1))		=> 0)
  (check (vector-compare-as-list '#(1) '#(2))		=> -1)
  (check (vector-compare-as-list '#(2) '#(1))		=> +1)

  (check (vector-compare-as-list '#(1 1) '#(1 1))	=> 0)
  (check (vector-compare-as-list '#(1 1) '#(1 2))	=> -1)
  (check (vector-compare-as-list '#(1 2) '#(1 1))	=> +1)

  (check (vector-compare-as-list '#(1 1 1) '#(1 1))	=> +1)
  (check (vector-compare-as-list '#(1 1)   '#(1 1 1))	=> -1)

  (check (vector-compare-as-list '#(1 1 1) '#(2 1))	=> -1)

;;; --------------------------------------------------------------------

  (check
      (let ((cmp (pair-compare-car real-compare)))
	(cmp '(1 . 2) '(2 . 1)))
    => -1)

  (check
      (let ((cmp (pair-compare-cdr real-compare)))
	(cmp '(1 . 2) '(2 . 1)))
    => +1)

  (check (pair-compare real-compare real-compare '(1 . 1) '(1 . 1))	=> 0)
  (check (pair-compare real-compare real-compare '(1 . 1) '(2 . 1))	=> -1)
  (check (pair-compare real-compare real-compare '(2 . 1) '(1 . 1))	=> +1)
  (check (pair-compare real-compare real-compare '(1 . 1) '(1 . 2))	=> -1)
  (check (pair-compare real-compare real-compare '(1 . 2) '(1 . 1))	=> +1)

  (check (pair-compare real-compare '()      '())	=> 0)
  (check (pair-compare real-compare '(1 . 1) '())	=> +1)
  (check (pair-compare real-compare '() '(1 . 1))	=> -1)
  (check (pair-compare real-compare '(1 . 1) '(1 . 1))	=> 0)
  (check (pair-compare real-compare '(1 . 1) '(2 . 1))	=> -1)
  (check (pair-compare real-compare '(2 . 1) '(1 . 1))	=> +1)
  (check (pair-compare real-compare '(1 . 1) '(1 . 2))	=> -1)
  (check (pair-compare real-compare '(1 . 2) '(1 . 1))	=> +1)

  (check (pair-compare '()      '())		=> 0)
  (check (pair-compare '(1 . 1) '())		=> +1)
  (check (pair-compare '() '(1 . 1))		=> -1)
  (check (pair-compare '(1 . 1) '(1 . 1))	=> 0)
  (check (pair-compare '(1 . 1) '(2 . 1))	=> -1)
  (check (pair-compare '(2 . 1) '(1 . 1))	=> +1)
  (check (pair-compare '(1 . 1) '(1 . 2))	=> -1)
  (check (pair-compare '(1 . 2) '(1 . 1))	=> +1)

  #t)


(parameterise ((check-test-name	'default))

  (check (default-compare '()		'())		=> 0)
  (check (default-compare '(1 . 2)	'())		=> +1)
  (check (default-compare '()		'(1 . 2))	=> -1)

  (check (default-compare '(1 . 2)	'(1 . 2))	=> 0)
  (check (default-compare '(1 . 2)	#t)		=> -1)
  (check (default-compare '(1 . 2)	#f)		=> -1)
  (check (default-compare #t		'(1 . 2))	=> +1)
  (check (default-compare #f		'(1 . 2))	=> +1)

  (check (default-compare #f		#f)		=> 0)
  (check (default-compare #t		#t)		=> 0)
  (check (default-compare #f		#t)		=> -1)
  (check (default-compare #t		#f)		=> +1)

  (check (default-compare #f		#\a)		=> -1)
  (check (default-compare #\a		#f)		=> +1)

  (check (default-compare "a"		#\a)		=> +1)
  (check (default-compare #\a		"a")		=> -1)

  (check (default-compare "a"		'a)		=> -1)
  (check (default-compare 'a		"a")		=> +1)

  (check (default-compare 5		'a)		=> +1)
  (check (default-compare 'a		5)		=> -1)

  (check (default-compare 5		'#(4))		=> -1)
  (check (default-compare '#(4)		5)		=> +1)

  (check (default-compare (eof-object)	'#(4))		=> +1)
  (check (default-compare '#(4)		(eof-object))	=> -1)

  #t)


(parameterise ((check-test-name	'default))

  (check (refine-compare)	=> 0)
  (check (refine-compare 1)	=> 1)
  (check (refine-compare 0 2)	=> 2)
  (check (refine-compare 0 0 2)	=> 2)

;;; --------------------------------------------------------------------

  (check
      (select-compare 1 1
		      (integer? 10)
		      (else     20))
    => 10)

  (check
      (select-compare 1 1.1
		      (integer? 10)
		      (else     20))
    => -1)

  (check
      (select-compare 1.1 1
		      (integer? 10)
		      (else     20))
    => +1)

  (check
      (select-compare 1.1 1.1
		      (integer? 10)
		      (else     20))
    => 20)

  (check
      (select-compare 1 1
		      (else     20))
    => 20)

  (check
      (select-compare 1 1)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (cond-compare)
    => 0)

  (check
      (cond-compare
       ((#t #t) 10)
       ((#t #t) 20)
       ((#t #t) 30)
       (else    40))
    => 10)

  (check
      (cond-compare
       ((#t #f) 10)
       ((#t #t) 20)
       ((#t #t) 30)
       (else    40))
    => -1)

  (check
      (cond-compare
       ((#f #t) 10)
       ((#t #t) 20)
       ((#t #t) 30)
       (else    40))
    => +1)

  (check
      (cond-compare
       ((#f #f) 10)
       ((#t #t) 20)
       ((#t #t) 30)
       (else    40))
    => 20)

  (check
      (cond-compare
       ((#f #f) 10)
       ((#f #f) 20)
       ((#f #f) 30)
       (else    40))
    => 40)

  (check
      (cond-compare
       ((#f #f) 10)
       ((#f #f) 20)
       ((#f #f) 30))
    => 0)


  #t)



(check-report)

;;; end of file
