;;;
;;;Part of: Vicare Scheme
;;;Contents: multidimensional arrays
;;;Date: Sun Jul  5, 2009
;;;
;;;Abstract
;;;
;;;	This  library is  inspired by  SRFI 25  "Multi-dimensional Array
;;;	Primitives ",  by Jussi Piitulainen.  However,  this library was
;;;	written from scratch.
;;;
;;;	  In  the  comments  of  this  library "items"  are  the  values
;;;	collected in Scheme's built-in lists and vectors, "elements" are
;;;	the values collected in the arrays.
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa containers arrays)
  (export

    <position> <shape> <array>

    ;; array positions
    array-position
    array-position?
    assert-array-position
    assert-array-position/or-false
    array-position->string
    array-position-display
    array-position-write

    ;; array shape
    array-shape
    array-shape?
    array-shape-contains?
    assert-array-shape
    assert-array-shape/or-false
    array-shape=?
    array-supershape?
    array-supershape?/strict
    array-subshape?
    array-subshape?/strict
    array-shape-number-of-dimensions
    array-shape-number-of-elements
    array-shape->string
    array-shape-display
    array-shape-write

    ;; arrays
    make-array
    array
    array-view
    array?
    array-copy
    array-ref
    array-set!
    ;;;array-view
    array->string
    array-display
    array-write)
  (import (nausicaa)
    (prefix (vicare containers strings) strings.)
    (prefix (vicare containers lists) lists.)
    (prefix (vicare containers vectors) vectors.)
    (nausicaa containers lists)
    (nausicaa containers vectors)
    (nausicaa containers strings))


;;;; helpers

(define (%coordinate? num)
  (and (integer? num) (exact? num)))


;;;; array position

;;; constructors

(define (array-position . coordinates)
  (unless (lists.every %coordinate? coordinates)
    (assertion-violation 'array-position
      "array coordinates must be non-negative exact integers"
      coordinates))
  (list->vector coordinates))

;;; --------------------------------------------------------------------
;;; predicates and assertions

(define (array-position? position)
  (and (vector? position)
       (vectors.vector-every %coordinate? position)
       #t))

(define (assert-array-position obj func-name)
  (or (array-position? obj)
      (assertion-violation func-name
	"expected array position"
	obj)))

(define (assert-array-position/or-false obj func-name)
  (or (or (not obj) (array-position? obj))
      (assertion-violation func-name
	"expected array position"
	obj)))

;;; --------------------------------------------------------------------
;;; Conversion and port output.

(define (array-position->string position)
  (string-append "#<array-position -- "
		 (strings.string-join (vector->list (vector-map number->string position)) " ")
		 ">"))

(define array-position-display
  (case-lambda
   ((position)
    (array-position-display position (current-output-port)))
   ((position port)
    (display (array-position->string position) port))))

(define array-position-write
  (case-lambda
   ((position)
    (array-position-write position (current-output-port)))
   ((position port)
    (write position port))))

;;; --------------------------------------------------------------------

(define-label <position>
  (parent <xvector>)
  (protocol (lambda () array-position))
  (predicate array-position?)
  (virtual-fields (immutable string array-position->string))
  (methods (display	array-position-display)
	   (write	array-position-write)))


;;;; array shape

(define-class (<shape> array-shape array-shape?)
  (nongenerative nausicaa:arrays:<shape>)
  (opaque #t)
  (fields (immutable (starts <xvector>))
		;A vector holding the start indexes for each dimension.
	  (immutable (pasts <xvector>)))
		;A vector holding the past indexes for each dimension.
  (virtual-fields (immutable (string <string>)		array-shape->string)
		  (immutable number-of-dimensions	array-shape-number-of-dimensions)
		  (immutable number-of-elements		array-shape-number-of-elements))
  (protocol
   (lambda (make-top)
     (lambda ((starts <xvector>) (pasts <xvector>))
       (let ((len (vector-length starts)))
	 (when (or (zero? len) (not (= len (pasts length))))
	   (assertion-violation 'array-shape
	     "invalid number of elements in shape specification"
	     starts pasts))
	 (do* ((i 0 (+ 1 i))
	       (S (starts[i]) (starts[i]))
	       (P (pasts[i]) (pasts[i])))
	     ((= i len))
	   (unless (%coordinate? S)
	     (assertion-violation 'array-shape
	       "invalid elements in shape starts specification"
	       S))
	   (unless (%coordinate? P)
	     (assertion-violation 'array-shape
	       "invalid elements in shape pasts specification"
	       P))))
       ((make-top) starts pasts))))

  (methods (contains?		array-shape-contains?)
	   (index-start		array-shape-index-start)
	   (index-past		array-shape-index-past)
	   (index-last		array-shape-index-last)

	   (=			array-shape=?)
	   (supershape?		array-supershape?)
	   (supershape?/strict	array-supershape?/strict)
	   (subshape?		array-subshape?)
	   (subshape?/strict	array-subshape?/strict)

	   (display		array-shape-display)
	   (write		array-shape-write)))

;;; --------------------------------------------------------------------
;;; predicates and assertions

(define (array-shape-contains? (shape <shape>) position)
  (vectors.vector-every (lambda (start past index)
			  (and (<= start index) (< index past)))
			(shape starts)
			(shape pasts)
			position))

(define (assert-array-shape obj func-name)
  (or (array-shape? obj)
      (assertion-violation func-name
	"expected array shape"
	obj)))

(define (assert-array-shape/or-false obj func-name)
  (or (or (not obj) (array-shape? obj))
      (assertion-violation func-name
	"expected array shape"
	obj)))

;;; --------------------------------------------------------------------
;;; inspection

(define (array-shape-number-of-dimensions (shape <shape>))
  (shape starts length))

(define (array-shape-number-of-elements (shape <shape>))
  (vectors.vector-fold-left (lambda (sum start past)
			      (+ sum (- past start)))
			    0
			    (shape starts)
			    (shape pasts)))

(define (array-shape-index-start (shape <shape>) dimension)
  (shape starts[dimension]))

(define (array-shape-index-past (shape <shape>) dimension)
  (shape pasts[dimension]))

(define (array-shape-index-last (shape <shape>) dimension)
  (+ -1 (shape pasts[dimension])))

;;; --------------------------------------------------------------------
;;; comparison

(define array-shape=?
  (case-lambda
   (((A <shape>) (B <shape>))
    (and (= (A number-of-dimensions) (B number-of-dimensions))
	 (vectors.vector-every
	     (lambda (sa sb pa pb)
	       (and (= sa sb) (= pa pb)))
	   (A starts) (B starts)
	   (A pasts)  (B pasts))))
   ((shape0 . shapes)
    (lists.fold-left/pred array-shape=? shape0 shapes))))

(define array-supershape?
  (case-lambda
   (((A <shape>) (B <shape>))
    (and (= (A number-of-dimensions) (B number-of-dimensions))
	 (vectors.vector-every
	     (lambda (sa sb pa pb)
	       (infix sa <= sb and pb <= pa))
	   (A starts) (B starts)
	   (A pasts)  (B pasts))))
   ((shape0 . shapes)
    (lists.fold-left/pred array-supershape? shape0 shapes))))

(define array-supershape?/strict
  (case-lambda
   (((A <shape>) (B <shape>))
    (and (= (A number-of-dimensions) (B number-of-dimensions))
	 (let ((len (A starts length)))
	   (let loop ((A-is-strict-supershape? #f)
		      (i 0))
	     (if (= i len)
		 A-is-strict-supershape?
	       (let ((Sa (vector-ref (A starts) i))
		     (Sb (vector-ref (B starts) i))
		     (Pa (vector-ref (A pasts)  i))
		     (Pb (vector-ref (B pasts)  i)))
		 (cond ((null? (A starts))
			A-is-strict-supershape?)
		       ((and (<  Sa Sb) (<= Pb Pa))
			(loop #t (+ 1 i)))
		       ((and (<= Sa Sb) (<  Pb Pa))
			(loop #t (+ 1 i)))
		       ((and (<= Sa Sb) (<= Pb Pa))
			(loop A-is-strict-supershape? (+ 1 i)))
		       (else #f))))))))
   ((shape0 . shapes)
    (lists.fold-left/pred array-supershape?/strict shape0 shapes))))

(define (array-subshape? shape0 . shapes)
  (apply array-supershape? (reverse (cons shape0 shapes))))

(define (array-subshape?/strict shape0 . shapes)
  (apply array-supershape?/strict (reverse (cons shape0 shapes))))

;;; --------------------------------------------------------------------
;;; Conversion and port output

(define (array-shape->string (S <shape>))
  (string-append "#<array-shape -- "
		 (strings.string-join (vector->list (S starts map number->string)) " ")
		 " -- "
		 (strings.string-join (vector->list (S pasts map number->string)) " ")
		 ">"))

(define array-shape-display
  (case-lambda
   ((shape)
    (array-shape-display shape (current-output-port)))
   ((shape port)
    (display (array-shape->string shape) port))))

(define array-shape-write
  (case-lambda
   ((shape)
    (array-shape-write shape (current-output-port)))
   (((S <shape>) port)
    (display "(array-shape '" port)
    (write (S starts) port)
    (display " '" port)
    (write (S pasts)  port)
    (display ")" port))))


;;;; arrays

(define-class (<array> make-<array> array?)
  (nongenerative nausicaa:arrays:<array>)
  (opaque #t)
  (parent <shape>)
  (fields (immutable (dimensions <xvector>))
		;A vector holding the lengths of the dimensions.
	  (immutable (factors <xvector>))
		;A  vector  holding  the  factors used  to  compute  the
		;absolute index in the underlying vector.
	  (immutable mapper)
		;A mapper function for coordinates.
	  (immutable (vector <xvector>)))
		;The underlying vector.

  (protocol
   (lambda (make-shape)
     (lambda ((S <shape>) fill-value)
       (let ((dimensions (vector-map - (S pasts) (S starts))))
	 ((make-shape (S starts) (S pasts))
	  dimensions (%compute-factors dimensions) #f
	  (make-vector (vectors.vector-fold-left * 1 dimensions) fill-value))))))

  (getter (lambda (stx tag)
	    (syntax-case stx ()
	      ((?var ((?position0) (?position) ...))
	       ;;Build a POSITION object and hand it to ARRAY-REF.
	       #'(array-ref ?var (array-position ?position0 ?position ...)))
	      )))

  (setter (lambda (stx tag)
	    (syntax-case stx ()
	      ((?var ((?position0) (?position) ...) ?value)
	       ;;Build a POSITION object and hand it to ARRAY-SET!.
	       #'(array-set! ?var (array-position ?position0 ?position ...) ?value))
	      )))

  (method-syntax =
    (syntax-rules ()
      ((_ ?this ?item= . ?arrays)
       (array=? ?item= ?this . ?arrays))))

  (methods (set!	array-set!)
	   (ref		array-ref)
	   (copy	array-copy)
	   (view	array-view)
	   (string	array->string)
	   (display	array-display)
	   (write	array-write)))

;;; --------------------------------------------------------------------
;;; constructors

(define (%compute-factors dimensions)
  (do* ((len     (vector-length dimensions))
	(factors (make-vector len))
	(kmax    (- len 1))
	(k 0 (+ 1 k)))
      ((= k kmax)
       (vector-set! factors (- len 1) 1)
       factors)
    (vector-set! factors k
		 (vectors.subvector-fold-left * 1 (vectors.view dimensions (vectors.start (+ 1 k)))))))

;; (define (%compute-factors dimensions)
;;   (let loop ((dims    (cdr dimensions))
;; 	     (factors '()))
;;     (if (null? dims)
;; 	(reverse (cons 1 factors))
;;       (loop (cdr dims)
;; 	    (cons (apply * dims) factors)))))

(define make-array
  (case-lambda
   ((shape)
    (make-array shape #f))
   ((shape fill-value)
    (<array> (shape fill-value)))))

(define (array shape . elements)
  (let* (((S <array>)	(<array> (shape #f)))
	 ((V <xvector>)	(S vector)))
    (do ((i 0 (+ 1 i))
	 (elements elements (if (null? elements)
				(assertion-violation 'array
				  "number of elements less than size of array")
			      (cdr elements))))
	((= i (V length))
	 (if (null? elements)
	     S
	   (assertion-violation 'array
	     "number of elements exceeds size of array")))
      (set! V[i] (car elements)))))

(define (array-copy (A <array>))
  (make-from-fields <array>
    (A starts copy)
    (A pasts copy)
    (A dimensions copy)
    (A factors copy)
    (A mapper)
    (A vector copy)))

(define (array-view (A <array>) mapper)
  (make-from-fields <array>
    (A starts) (A pasts)
    (A dimensions) (A factors)
    (let ((under (A mapper)))
      (if under
	  (lambda (position)
	    (under (mapper position)))
	mapper))
    (A vector)))

;;; --------------------------------------------------------------------
;;; predicates and assertions

(define (assert-array obj who)
  (or (is-a? obj <array>)
      (assertion-violation who "expected array" obj)))

(define (assert-array/or-false obj who)
  (or (or (not obj) (is-a? obj <array>))
      (assertion-violation who "expected array" obj)))

;;; --------------------------------------------------------------------
;;; comparison

(define array=?
  (case-lambda
   ((item= (A <array>) (B <array>))
    (and (array-shape=? A B)
	 (vector-for-all item= (A vector) (B vector))))
   ((item= array0 . arrays)
    (lists.fold-left/pred (lambda (a b)
			    (array=? item= a b))
			  array0 arrays))))

;;; --------------------------------------------------------------------
;;; accessors

(define (%compute-index who (A <array>) position)
  (vectors.vector-fold-left
      (lambda (offset factor index)
	(infix offset + factor * index))
    0
    (A factors)
    (if (A mapper)
	(A mapper position)
      position)))

(define (array-ref (A <array>) position)
  (A vector [(%compute-index 'array-ref A position)]))

(define (array-set! (A <array>) position value)
  (set! (A vector [(%compute-index 'array-set! A position)]) value))

;;; --------------------------------------------------------------------
;;; Conversion and port output

(define (array->string (A <array>) element->string)
  (string-append "#<array " (array-shape->string A) " "
		 (A vector fold-right
		    (lambda (item string)
		      (string-append (element->string item) " " string))
		    "")
		 ">"))

(define array-display
  (case-lambda
   ((array element->string)
    (array-display array element->string (current-output-port)))
   (((A <array>) element->string port)
    (display (A string element->string) port))))

(define array-write
  (case-lambda
   ((array element->string)
    (array-write array element->string (current-output-port)))
   (((A <array>) element->string port)
    (display "(array " port)
    (array-shape-write A port)
    (display " " port)
    (display (A vector fold-right (lambda (item string)
				    (string-append (element->string item) " " string))
		"")
	     port)
    (display ")" port))))


;;;; done

)

;;; end of file
