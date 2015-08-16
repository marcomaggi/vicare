;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of SRFI 116, comparators
;;;Date: Fri Jun 12, 2015
;;;
;;;Abstract
;;;
;;;	This library is derived from SRFI 114 reference implementation.
;;;
;;;Copyright (C) John Cowan 2013.  All Rights Reserved.
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software  and associated documentation  files (the ``Software''), to  deal in
;;;the Software without restriction, including  without limitation the rights to use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED ``AS  IS'', WITHOUT  WARRANTY OF  ANY KIND,  EXPRESS OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;


#!vicare
(library (vicare containers ilists comparators)
  (export
    ipair-comparator
    ilist-comparator
    make-ipair-comparator
    make-ilist-comparator
    make-improper-ilist-comparator
    make-icar-comparator
    make-icdr-comparator)
  (import (vicare)
    (vicare containers ilists)
    (vicare containers comparators))


;;;; SRFI 114 comparators

(define-syntax define-predefined-comparator
  ;;Define a comparator through a constructor function that caches its result.  It is
  ;;to be used to retrieve the predefined comparators.
  ;;
  (syntax-rules ()
    ((_ ?who ?build-form)
     (begin
       (define-syntax ?who
	 (identifier-syntax (builder)))
       (define builder
	 (let ((C #f))
	   (lambda ()
	     (or C (receive-and-return (rv)
		       ?build-form
		     (set! C rv))))))
       #| end of BEGIN |# ))
    ))

;;; --------------------------------------------------------------------

(module (make-ipair-comparator)

  (define* (make-ipair-comparator {car-K comparator?} {cdr-K comparator?})
    (define car-test-proc
      (comparator-type-test-procedure car-K))
    (define cdr-test-proc
      (comparator-type-test-procedure cdr-K))
    (define (test-proc obj)
      (and (ipair? obj)
	   (car-test-proc (icar obj))
	   (cdr-test-proc (icdr obj))))
    (make-comparator test-proc
		     #t
		     (make-ipair-comparison car-K cdr-K)
		     (make-ipair-hash       car-K cdr-K)))

  (define* (make-ipair-hash {car-K comparator?} {cdr-K comparator?})
    (let ((car-hash (comparator-hash-function car-K))
	  (cdr-hash (comparator-hash-function cdr-K)))
      (lambda (obj)
	(+ (car-hash (icar obj))
	   (cdr-hash (icdr obj))))))

  #| end of module |# )

(define-predefined-comparator ipair-comparator
  (make-ipair-comparator default-comparator default-comparator))

;;; --------------------------------------------------------------------

(define* (make-ilist-comparator {K comparator?})
  (define element-test-proc
    (comparator-type-test-procedure K))
  (define (test-proc obj)
    (if (ipair? obj)
	(and (element-test-proc (icar obj))
	     (test-proc (icdr obj)))
      (null? obj)))
  (make-listwise-comparator test-proc K null? icar icdr))

(define-predefined-comparator ilist-comparator
  (make-ilist-comparator default-comparator))

;;; --------------------------------------------------------------------

(define* (make-icar-comparator {K comparator?})
  (define icar-test-proc
    (comparator-type-test-procedure K))
  (define (test-proc obj)
    (and (ipair? obj)
	 (icar-test-proc (icar obj))))
  (make-comparator test-proc
		   #t
		   (let ((compare (comparator-comparison-procedure K)))
		     (lambda (a b)
		       (compare (icar a) (icar b))))
		   (let ((hash (comparator-hash-function K)))
		     (lambda (obj)
		       (hash (icar obj))))))

(define* (make-icdr-comparator {K comparator?})
  (define icdr-test-proc
    (comparator-type-test-procedure K))
  (define (test-proc obj)
    (and (ipair? obj)
	 (icdr-test-proc (icdr obj))))
  (make-comparator test-proc
		   #t
		   (let ((compare (comparator-comparison-procedure K)))
		     (lambda (a b)
		       (compare (icdr a) (icdr b))))
		   (let ((hash (comparator-hash-function K)))
		     (lambda (obj)
		       (hash (icdr obj))))))

;;; --------------------------------------------------------------------

(module (make-improper-ilist-comparator)

  (define* (make-improper-ilist-comparator {K comparator?})
    (make-comparator #t
		     #t
		     (make-improper-ilist-comparison K)
		     (make-improper-ilist-hash       K)))

  (module (make-improper-ilist-comparison)

    (define* (make-improper-ilist-comparison {K comparator?})
      (let ((pair-compare (make-ipair-comparison K K))
	    (item-compare (comparator-comparison-procedure K)))
	(lambda (a b)
	  ;;A.TYPE and B.TYPE are the indexes of the object types.
	  (let* ((a.type (improper-list-type a))
		 (b.type (improper-list-type b))
		 (result (real-comparison a.type b.type)))
	    (if (zero? result)
		;;A and B have the same type index; they are: both pairs, both nulls,
		;;both some other object.
		(cond ((ipair? a)
		       (pair-compare a b))
		      ((null? a)
		       0)
		      (else
		       (item-compare a b)))
	      result)))))

    (define (improper-list-type obj)
      ;;Compute type index for inexact list comparisons.
      ;;
      (cond ((null? obj)	0)
	    ((ipair? obj)	1)
	    (else		2)))

    (define (real-comparison a b)
      ;;Comparison procedure for real numbers only.
      ;;
      (cond ((< a b)	-1)
	    ((> a b)	+1)
	    (else	0)))

    #| end of module |# )

  (define* (make-improper-ilist-hash {K comparator?})
    (let ((hash (comparator-hash-function K)))
      (lambda (obj)
	(cond ((ipair? obj)
	       (+ (hash (icar obj))
		  (hash (icdr obj))))
	      ((null? obj)
	       0)
	      (else
	       (hash obj))))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define* (make-ipair-comparison {car-K comparator?} {cdr-K comparator?})
  (let ((car-compare (comparator-comparison-procedure car-K))
	(cdr-compare (comparator-comparison-procedure cdr-K)))
    (lambda (a b)
      (let ((result (car-compare (icar a) (icar b))))
	(if (zero? result)
	    (cdr-compare (icdr a) (icdr b))
	  result)))))


;;;; done

  #| end of library |# )

;;; end o file
