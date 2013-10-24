;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for multidimensional arrays
;;;Date: Mon Jul  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (nausicaa)
  (nausicaa containers arrays)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: multidimensional arrays\n")


(parameterise ((check-test-name 'position))

;;;procedural interface

  (let ((pos  (array-position 1 2 3 4 5))
	(pos2 (array-position 8 7 6 5 4)))

    (check (array-position? pos) => #t)
    (check (array-position->string pos) => "#<array-position -- 1 2 3 4 5>")

    (check (assert-array-position pos 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-position 123 'this)) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-position #t 'this)) => #t)

    (check (assert-array-position/or-false pos 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-position/or-false 123 'this)) => #t)
    (check (assert-array-position/or-false #f 'this) => #t)

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-position-display pos port)))
      => "#<array-position -- 1 2 3 4 5>")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-position-write pos port)))
      => "#(1 2 3 4 5)")

    #f)

;;; --------------------------------------------------------------------
;;; label interface

  (let (((pos  <position>) (<position> (1 2 3 4 5)))
	((pos2 <position>) (<position> (8 7 6 5 4))))

    (check (is-a? pos <position>) => #t)
    (check (pos string) => "#<array-position -- 1 2 3 4 5>")

    (check (assert-array-position pos 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-position 123 'this)) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-position #t 'this)) => #t)

    (check (assert-array-position/or-false pos 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-position/or-false 123 'this)) => #t)
    (check (assert-array-position/or-false #f 'this) => #t)

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (pos display port)))
      => "#<array-position -- 1 2 3 4 5>")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (pos write port)))
      => "#(1 2 3 4 5)")

    #f)

  #t)


(parameterise ((check-test-name 'shape))

;;;procedural interface

  (let ((shape  (array-shape '#(1 2 3 4 5)
			     '#(6 7 8 9 10)))
	(shape2 (array-shape '#(5 4 3 2 1)
			     '#(10 9 8 7 6)))
	(pos    (array-position  2 3 4 5 6))
	(pos2   (array-position  2 3 4 20 6)))

    (check (array-shape? shape) => #t)
    (check (array-shape-number-of-dimensions shape) => 5)
    (check (array-shape-number-of-elements shape) => (+ (- 6 1)
							(- 7 2)
							(- 8 3)
							(- 9 4)
							(- 10 5)))
    (check (array-shape->string shape) => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check (array-shape=? shape shape) => #t)
    (check (array-shape=? shape shape2) => #f)

    (check (array-shape-contains? shape pos) => #t)
    (check (array-shape-contains? shape pos2) => #f)

    (check (assert-array-shape shape 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape 123 'this)) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape #t 'this)) => #t)

    (check (assert-array-shape/or-false shape 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape/or-false 123 'this)) => #t)
    (check (assert-array-shape/or-false #f 'this) => #t)

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-shape-display shape port)))
      => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-shape-write shape port)))
      => "(array-shape '#(1 2 3 4 5) '#(6 7 8 9 10))")

    (check-for-true (array-supershape? shape shape))
    (check-for-false (array-supershape? shape shape2))
    (check-for-false (array-supershape?/strict shape shape))
    (check-for-false (array-supershape?/strict shape shape2))

    (check-for-true (array-subshape? shape shape))
    (check-for-false (array-subshape? shape2 shape))
    (check-for-false (array-subshape?/strict shape shape))
    (check-for-false (array-subshape?/strict shape2 shape))

    #f)

  (let ((shape  (array-shape '#(0 0 0 0 0)
			     '#(5 6 7 8 9)))
	(shape2 (array-shape '#(0 0 0 0 0)
			     '#(4 5 6 7 8)))
	(shape3 (array-shape '#(0 0 0 0 0)
			     '#(5 6 2 8 9))))

    (check-for-true (array-supershape? shape shape2))
    (check-for-true (array-supershape? shape shape3))
    (check-for-true (array-supershape?/strict shape shape2))
    (check-for-true (array-supershape?/strict shape shape3))

    (check-for-true (array-subshape? shape2 shape))
    (check-for-true (array-subshape? shape3 shape))
    (check-for-true (array-subshape?/strict shape2 shape))
    (check-for-true (array-subshape?/strict shape3 shape))

    #f)

;;; --------------------------------------------------------------------
;;; class interface

  (let (((shape <shape>)	(<shape> ('#(1 2 3 4 5)
					  '#(6 7 8 9 10))))
	((shape2 <shape>)	(<shape> ('#(5 4 3 2 1)
					  '#(10 9 8 7 6))))
	((pos  <position>)	(<position> (2 3 4  5 6)))
	((pos2 <position>)	(<position> (2 3 4 20 6))))

    (check (is-a? shape <shape>) => #t)
    (check (shape number-of-dimensions) => 5)
    (check (shape number-of-elements) => (+ (- 6 1)
					    (- 7 2)
					    (- 8 3)
					    (- 9 4)
					    (- 10 5)))
    (check (shape string) => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check (shape = shape)  => #t)
    (check (shape = shape2) => #f)

    (check (shape contains? pos)  => #t)
    (check (shape contains? pos2) => #f)

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (shape display port)))
      => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (shape write port)))
      => "(array-shape '#(1 2 3 4 5) '#(6 7 8 9 10))")

    (check-for-true  (shape supershape? shape))
    (check-for-false (shape supershape? shape2))
    (check-for-false (shape supershape?/strict shape))
    (check-for-false (shape supershape?/strict shape2))

    (check-for-true  (shape subshape?  shape))
    (check-for-false (shape2 subshape? shape))
    (check-for-false (shape subshape?/strict  shape))
    (check-for-false (shape2 subshape?/strict shape))

    #f)

  (let (((shape <shape>)  (<shape> ('#(0 0 0 0 0)
				    '#(5 6 7 8 9))))
	((shape2 <shape>) (<shape> ('#(0 0 0 0 0)
				    '#(4 5 6 7 8))))
	((shape3 <shape>) (<shape> ('#(0 0 0 0 0)
				    '#(5 6 2 8 9)))))

    (check-for-true (shape supershape? shape2))
    (check-for-true (shape supershape? shape3))
    (check-for-true (shape supershape?/strict shape2))
    (check-for-true (shape supershape?/strict shape3))

    (check-for-true (shape2 subshape? shape))
    (check-for-true (shape3 subshape? shape))
    (check-for-true (shape2 subshape?/strict shape))
    (check-for-true (shape3 subshape?/strict shape))

    #f)

  #t)


(parameterise ((check-test-name 'array-proc))

  (let ((array  (make-array (array-shape '#(1 2 3 4 5)
					 '#(6 7 8 9 10))))
	(array2	(make-array (array-shape '#(5 4 3 2 1)
					 '#(10 9 8 7 6))))
	(array3 (make-array (array-shape '#(0 0 0 0)
					 '#(3 4 5 6))))
	(array4 (make-array (array-shape '#(0 0 0) ;this is small, good for string representation
					 '#(2 3 4))))
	(array5 (array (array-shape '#(0 0 0) '#(2 3 4))
		        1  2  3  4  5  6  7  8  9 10
		       11 12 13 14 15 16 17 18 19 20
		       21 22 23 24))
	(pos    (array-position  2 3 4 5 6))
	(pos2   (array-position  2 3 4 20 6)))

    (check (array-shape? array) => #t)
    (check (array-shape-number-of-dimensions array) => 5)
    (check (array-shape-number-of-elements array) => (+ (- 6 1)
							(- 7 2)
							(- 8 3)
							(- 9 4)
							(- 10 5)))

    (check-for-true (array? array3))
    (check (array-shape-number-of-dimensions array3) => 4)
    (check (array-shape-number-of-elements array3) => (+ (- 3 0)
							 (- 4 0)
							 (- 5 0)
							 (- 6 0)))
    (check (array-shape->string array) => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check (array-shape=? array array) => #t)
    (check (array-shape=? array array2) => #f)

    (check (array-shape-contains? array pos) => #t)
    (check (array-shape-contains? array pos2) => #f)

    (check (assert-array-shape array 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape 123 'this)) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape #t 'this)) => #t)

    (check (assert-array-shape/or-false array 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape/or-false 123 'this)) => #t)
    (check (assert-array-shape/or-false #f 'this) => #t)

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-shape-display array port)))
      => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-shape-write array port)))
      => "(array-shape '#(1 2 3 4 5) '#(6 7 8 9 10))")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-display array4
			     (lambda (element)
			       (call-with-string-output-port
				   (lambda (port)
				     (display element port))))
			     port)))
      => "#<array #<array-shape -- 0 0 0 -- 2 3 4> #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f >")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-write array4
			   (lambda (element)
			     (call-with-string-output-port
				 (lambda (port)
				   (display element port))))
			   port)))
      => "(array (array-shape '#(0 0 0) '#(2 3 4)) #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f )")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-display array5
			     (lambda (element)
			       (call-with-string-output-port
				   (lambda (port)
				     (display element port))))
			     port)))
      => "#<array #<array-shape -- 0 0 0 -- 2 3 4> 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 >")

    (check (array-ref array5 (array-position  0 0 0)) => 1)
    (check (array-ref array5 (array-position  0 0 1)) => 2)
    (check (array-ref array5 (array-position  0 0 2)) => 3)
    (check (array-ref array5 (array-position  0 0 3)) => 4)
    (check (array-ref array5 (array-position  0 1 0)) => 5)
    (check (array-ref array5 (array-position  0 1 1)) => 6)
    (check (array-ref array5 (array-position  0 1 2)) => 7)
    (check (array-ref array5 (array-position  0 1 3)) => 8)
    (check (array-ref array5 (array-position  0 2 0)) => 9)
    (check (array-ref array5 (array-position  0 2 1)) => 10)
    (check (array-ref array5 (array-position  0 2 2)) => 11)
    (check (array-ref array5 (array-position  0 2 3)) => 12)
    (check (array-ref array5 (array-position  1 0 0)) => 13)
    (check (array-ref array5 (array-position  1 0 1)) => 14)
    (check (array-ref array5 (array-position  1 0 2)) => 15)
    (check (array-ref array5 (array-position  1 0 3)) => 16)
    (check (array-ref array5 (array-position  1 1 0)) => 17)
    (check (array-ref array5 (array-position  1 1 1)) => 18)
    (check (array-ref array5 (array-position  1 1 2)) => 19)
    (check (array-ref array5 (array-position  1 1 3)) => 20)
    (check (array-ref array5 (array-position  1 2 0)) => 21)
    (check (array-ref array5 (array-position  1 2 1)) => 22)
    (check (array-ref array5 (array-position  1 2 2)) => 23)
    (check (array-ref array5 (array-position  1 2 3)) => 24)

    (let ((view (array-view array5 (lambda (position)
				     (vector 1
					     (vector-ref position 0)
					     (vector-ref position 1))))))
      (check (array-ref view (array-position  0 0)) => 13)
      (check (array-ref view (array-position  0 1)) => 14)
      (check (array-ref view (array-position  0 2)) => 15)
      (check (array-ref view (array-position  0 3)) => 16)
      (check (array-ref view (array-position  1 0)) => 17)
      (check (array-ref view (array-position  1 1)) => 18)
      (check (array-ref view (array-position  1 2)) => 19)
      (check (array-ref view (array-position  1 3)) => 20)
      (check (array-ref view (array-position  2 0)) => 21)
      (check (array-ref view (array-position  2 1)) => 22)
      (check (array-ref view (array-position  2 2)) => 23)
      (check (array-ref view (array-position  2 3)) => 24))

    (check-for-true (array-supershape? array array))
    (check-for-false (array-supershape? array array2))
    (check-for-false (array-supershape?/strict array array))
    (check-for-false (array-supershape?/strict array array2))

    (check-for-true (array-subshape? array array))
    (check-for-false (array-subshape? array2 array))
    (check-for-false (array-subshape?/strict array array))
    (check-for-false (array-subshape?/strict array2 array))

    #f)

  (let ((array  (make-array (array-shape '#(0 0 0 0 0)
					 '#(5 6 7 8 9))))
	(array2 (make-array (array-shape '#(0 0 0 0 0)
					 '#(4 5 6 7 8))))
	(array3 (make-array (array-shape '#(0 0 0 0 0)
					 '#(5 6 2 8 9)))))

    (check-for-true (array-supershape? array array2))
    (check-for-true (array-supershape? array array3))
    (check-for-true (array-supershape?/strict array array2))
    (check-for-true (array-supershape?/strict array array3))

    (check-for-true (array-subshape? array2 array))
    (check-for-true (array-subshape? array3 array))
    (check-for-true (array-subshape?/strict array2 array))
    (check-for-true (array-subshape?/strict array3 array))

    #f)

  #t)


(parameterise ((check-test-name 'array-class))

  (let (((array <array>)	(<array> ((<shape> ('#(1 2 3 4 5)
						    '#(6 7 8 9 10)))
					  0)))
  	((array2 <array>)	(<array> ((<shape> ('#(5 4 3 2 1)
						    '#(10 9 8 7 6)))
					  1)))
  	((array3 <array>)	(<array> ((<shape> ('#(0 0 0 0)
						    '#(3 4 5 6)))
					  #f)))
  	((array4 <array>)	(<array> ((<shape> ('#(0 0 0)
						    '#(2 3 4)))
					  #f)))
	;;this is small, good for string representation
  	((array5 <array>)	(array (<shape> ('#(0 0 0) '#(2 3 4)))
  				       1  2  3  4  5  6  7  8  9  10
  				       11 12 13 14 15 16 17 18 19 20
  				       21 22 23 24))
  	((pos <position>)	(array-position  2 3 4 5 6))
  	((pos2 <position>)	(array-position  2 3 4 20 6)))

    (check (is-a? array <shape>) => #t)
    (check (array number-of-dimensions) => 5)
    (check (array number-of-elements) => (+ (- 6 1)
					    (- 7 2)
					    (- 8 3)
					    (- 9 4)
					    (- 10 5)))

    (check-for-true (is-a? array3 <array>))
    (check (array3 number-of-dimensions) => 4)
    (check (array3 number-of-elements) => (+ (- 3 0)
					     (- 4 0)
					     (- 5 0)
					     (- 6 0)))
    (check (array-shape->string array) => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check
    	(with-tags ((array <shape>))
    	  (array = array))
      => #t)
    (check
    	(with-tags ((array <shape>))
    	  (array = array2))
      => #f)

    (check (array contains? pos)  => #t)
    (check (array contains? pos2) => #f)

    (check (array = = array)  => #t)
    (check (array = = array2) => #f)

    (check
    	(with-tags ((array <shape>))
    	  (call-with-string-output-port
    	      (lambda (port)
    		(array display port))))
      => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check
    	(with-tags ((array <shape>))
    	  (call-with-string-output-port
    	      (lambda (port)
    		(array write port))))
      => "(array-shape '#(1 2 3 4 5) '#(6 7 8 9 10))")

    (check
    	(call-with-string-output-port
    	    (lambda (port)
    	      (array4 display (lambda (element)
    				(call-with-string-output-port
    				    (lambda (port)
    				      (display element port))))
		      port)))
      => "#<array #<array-shape -- 0 0 0 -- 2 3 4> #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f >")

    (check
    	(call-with-string-output-port
    	    (lambda (port)
    	      (array4 write (lambda (element)
    			      (call-with-string-output-port
    				  (lambda (port)
    				    (display element port))))
		      port)))
      => "(array (array-shape '#(0 0 0) '#(2 3 4)) #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f )")

    (check
    	(call-with-string-output-port
    	    (lambda (port)
    	      (array5 display (lambda (element)
    				(call-with-string-output-port
    				    (lambda (port)
    				      (display element port))))
		      port)))
      => "#<array #<array-shape -- 0 0 0 -- 2 3 4> 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 >")

    (check (array5[0][0][0]) => 1)
    (check (array5[0][0][1]) => 2)
    (check (array5[0][0][2]) => 3)
    (check (array5[0][0][3]) => 4)
    (check (array5[0][1][0]) => 5)
    (check (array5[0][1][1]) => 6)
    (check (array5[0][1][2]) => 7)
    (check (array5[0][1][3]) => 8)
    (check (array5[0][2][0]) => 9)
    (check (array5[0][2][1]) => 10)
    (check (array5[0][2][2]) => 11)
    (check (array5[0][2][3]) => 12)
    (check (array5[1][0][0]) => 13)
    (check (array5[1][0][1]) => 14)
    (check (array5[1][0][2]) => 15)
    (check (array5[1][0][3]) => 16)
    (check (array5[1][1][0]) => 17)
    (check (array5[1][1][1]) => 18)
    (check (array5[1][1][2]) => 19)
    (check (array5[1][1][3]) => 20)
    (check (array5[1][2][0]) => 21)
    (check (array5[1][2][1]) => 22)
    (check (array5[1][2][2]) => 23)
    (check (array5[1][2][3]) => 24)

    (let (((view <array>) (array-view array5 (lambda (position)
    					       (vector 1
    						       (vector-ref position 0)
    						       (vector-ref position 1))))))
      (check (view[0][0]) => 13)
      (check (view[0][1]) => 14)
      (check (view[0][2]) => 15)
      (check (view[0][3]) => 16)
      (check (view[1][0]) => 17)
      (check (view[1][1]) => 18)
      (check (view[1][2]) => 19)
      (check (view[1][3]) => 20)
      (check (view[2][0]) => 21)
      (check (view[2][1]) => 22)
      (check (view[2][2]) => 23)
      (check (view[2][3]) => 24))

    (check-for-true (array supershape? array))
    (check-for-false (array supershape? array2))
    (check-for-false (array supershape?/strict array))
    (check-for-false (array supershape?/strict array2))

    (check-for-true (array subshape? array))
    (check-for-false (array2 subshape? array))
    (check-for-false (array subshape?/strict array))
    (check-for-false (array2 subshape?/strict array))

    #f)

  (let (((array <array>)	(<array> ((array-shape '#(0 0 0 0 0)
						       '#(5 6 7 8 9))
					  #f)))
	((array2 <array>)	(<array> ((array-shape '#(0 0 0 0 0)
						       '#(4 5 6 7 8))
					  #f)))
	((array3 <array>)	(<array> ((array-shape '#(0 0 0 0 0)
						       '#(5 6 2 8 9))
					  #f))))

    (check-for-true (array supershape? array2))
    (check-for-true (array supershape? array3))
    (check-for-true (array supershape?/strict array2))
    (check-for-true (array supershape?/strict array3))

    (check-for-true (array2 subshape? array))
    (check-for-true (array3 subshape? array))
    (check-for-true (array2 subshape?/strict array))
    (check-for-true (array3 subshape?/strict array))

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
