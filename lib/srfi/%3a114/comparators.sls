;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of SRFI 114
;;;Date: Sun Mar  8, 2015
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


#!r6rs
(library (srfi :114 comparators)
  (export)
  (import (vicare))


;;; comparison syntaxes

;; Arithmetic if
(define-syntax if3
  (syntax-rules ()
    ((if3 ?expr ?less ?equal ?greater)
     (let ((rv ?expr))
       (case rv
	 ((-1)	?less)
	 ((0)	?equal)
	 ((+1)	?greater)
	 (else
	  (assertion-violation 'if3
	    "bad return value from comparison expression, expected -1, 0, +1" rv)))))
    ))

;; If equal
(define-syntax if=?
  (syntax-rules ()
    ((if=? ?expr ?equal ?unequal)
     (if3 ?expr ?unequal ?equal ?unequal))
    ((if=? ?expr ?equal)
     (if=? ?expr ?equal (void)))
    ))

;; If less than
(define-syntax if<?
  (syntax-rules ()
    ((if<? ?expr ?less ?notless)
     (if3 ?expr ?less ?notless ?notless))
    ((if<? ?expr ?less)
     (if<? ?expr ?less (void)))
    ))

;; If greater than
(define-syntax if>?
  (syntax-rules ()
    ((if>? ?expr ?greater ?notgreater)
     (if3 ?expr ?notgreater ?notgreater ?greater))
    ((if>? ?expr ?greater)
     (if>? ?expr ?greater (void)))
    ))

;; If not equal
(define-syntax if-not=?
  (syntax-rules ()
    ((if-not=? ?expr ?notequal ?equal)
     (if3 ?expr ?notequal ?equal ?notequal))
    ((if-not=? ?expr ?notequal)
     (if-not=? ?expr ?notequal (void)))
    ))

;; If less than or equal
(define-syntax if<=?
  (syntax-rules ()
    ((if<=? ?expr ?lessequal ?greater)
     (if3 ?expr ?lessequal ?lessequal ?greater))
    ((if<=? ?expr ?lessequal)
     (if>? ?expr ?lessequal (void)))
    ))

;; If greater than or equal
(define-syntax if>=?
  (syntax-rules ()
    ((if>=? ?expr ?greaterequal ?less)
     (if3 ?expr ?less ?greaterequal ?greaterequal))
    ((if>=? ?expr ?greaterequal)
     (if>=? ?expr ?greaterequal (void)))
    ))


;;;; definition of comparator records with accessors and basic comparator

(define-record-type comparator
  (fields (immutable type-test		comparator-type-test-procedure)
	  (immutable equality		comparator-equality-predicate)
	  (immutable comparison		comparator-comparison-procedure)
	  (immutable hash		comparator-hash-function)
	  (immutable comparison?	comparator-comparison-procedure?)
	  (immutable hash?		comparator-hash-function?))
  (protocol
   (lambda (make-record)
     (lambda (type-test equality comparison hash)
       (make-record (if (eq? type-test #t)
			(lambda (x) #t)
		      type-test)
		    (if (eq? equality  #t)
			(lambda (x y)
			  (eqv? (comparison x y) 0))
		      equality)
		    (or comparison
			(lambda (x y)
			  (error 'comparator "comparison not supported")))
		    (or hash
			(lambda (x y)
			  (error 'comparator "hashing not supported")))
		    (if comparison #t #f)
		    (if hash #t #f)))))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; primitive applicators

(define (comparator-test-type comparator obj)
  ;;Invoke the test type.
  ;;
  ((comparator-type-test-procedure comparator) obj))

(define (comparator-check-type comparator obj)
  ;;Invoke the test type and throw an error if it fails.
  ;;
  (if (comparator-test-type comparator obj)
      #t
    (error __who__
      "comparator type check failed" comparator obj)))

(define (comparator-equal? comparator obj1 obj2)
  ;;Invoke the equality predicate.
  ;;
  ((comparator-equality-predicate comparator) obj1 obj2))

(define (comparator-compare comparator obj1 obj2)
  ;;Invoke the comparison procedure.
  ;;
  ((comparator-comparison-procedure comparator) obj1 obj2))

(define (comparator-hash comparator obj)
  ;;Invoke the hash function.
  ;;
  ((comparator-hash-function comparator) obj))


;;;; comparison procedure comparators
;;
;;These construct comparison procedures based on comparison predicates.
;;

(define (make-comparison< <)
  (lambda (a b)
    (cond ((< a b)	-1)
	  ((< b a)	+1)
	  (else		0))))

(define (make-comparison> >)
  (lambda (a b)
    (cond ((> a b)	+1)
	  ((> b a)	-1)
	  (else		0))))

(define (make-comparison<= <=)
  (lambda (a b)
    (if (<= a b)
	(if (<= b a) 0 -1)
      1)))

(define (make-comparison>= >=)
  (lambda (a b)
    (if (>= a b)
	(if (>= b a) 0 1)
      -1)))

(define (make-comparison=/< = <)
  (lambda (a b)
    (cond ((= a b)	0)
	  ((< a b)	-1)
	  (else		+1))))

(define (make-comparison=/> = >)
  (lambda (a b)
    (cond ((= a b)	0)
	  ((> a b)	+1)
	  (else		-1))))


;;;; the default comparator

(module (default-comparator)

  (define unknown-object-comparator
    ;;The unknown-object comparator, used as a fallback to everything else
    ;;
    (make-comparator (lambda (obj) #t)
		     (lambda (a b) #t)
		     (lambda (a b) 0)
		     (lambda (obj) 0)))

  ;;Next index for added comparator.
  ;;
  (define first-comparator-index   9)
  (define *next-comparator-index*  9)
  (define *registered-comparators* (list unknown-object-comparator))

  ;;Register a new comparator for use by the default comparator.
  ;;
  (define (comparator-register-default! comparator)
    (set! *registered-comparators* (cons comparator *registered-comparators*))
    (set! *next-comparator-index*  (+ *next-comparator-index* 1)))

  (define (object-type obj)
    ;;Return ordinal  for object types:  null sorts  before pairs, which  sort before
    ;;booleans, etc.  Implementations can extend this.
    ;;
    ;;People who call COMPARATOR-REGISTER-DEFAULT! effectively do extend it.
    ;;
    (cond ((null?	obj)	0)
	  ((pair?	obj)	1)
	  ((boolean?	obj)	2)
	  ((char?	obj)	3)
	  ((string?	obj)	4)
	  ((symbol?	obj)	5)
	  ((number?	obj)	6)
	  ((vector?	obj)	7)
	  ((bytevector?	obj)	8)
	  ;;Add more here if you want: be sure to update comparator-index variables.
	  (else
	   (registered-index obj))))

  (define (registered-index obj)
    ;;Return the index for the registered type of OBJ.
    ;;
    (let loop ((i        0)
	       (registry *registered-comparators*))
      (cond ((null? registry)
	     (+ first-comparator-index i))
	    ((comparator-test-type (car registry) obj)
	     (+ first-comparator-index i))
	    (else
	     (loop (add1 i) (cdr registry))))))

  (define (registered-comparator i)
    ;;Given an index, retrieve a registered conductor.  Index must be:
    ;;
    ;;   i >= first-comparator-index
    ;;
    (list-ref *registered-comparators* (- i first-comparator-index)))

  (define (dispatch-equality type a b)
    (case type
      ((0) 0)	;all empty lists are equal
      ((1) (fxzero? (pair-comparison       a b)))
      ((2) (fxzero? (boolean-comparison    a b)))
      ((3) (fxzero? (char-comparison       a b)))
      ((4) (fxzero? (string-comparison     a b)))
      ((5) (fxzero? (symbol-comparison     a b)))
      ((6) (fxzero? (complex-comparison    a b)))
      ((7) (fxzero? (vector-comparison     a b)))
      ((8) (fxzero? (bytevector-comparison a b)))
      ;;Add more here.
      (else
       (comparator-equal? (registered-comparator type) a b))))

  (define (dispatch-comparison type a b)
    (case type
      ((0) 0)	;all empty lists are equal
      ((1) (pair-comparison       a b))
      ((2) (boolean-comparison    a b))
      ((3) (char-comparison       a b))
      ((4) (string-comparison     a b))
      ((5) (symbol-comparison     a b))
      ((6) (complex-comparison    a b))
      ((7) (vector-comparison     a b))
      ((8) (bytevector-comparison a b))
      ;;Add more here.
      (else
       (comparator-compare (registered-comparator type) a b))))

  (define (default-hash-function obj)
    (case (object-type obj)
      ((0) 0)
      ((1) (pair-hash       obj))
      ((2) (boolean-hash    obj))
      ((3) (char-hash       obj))
      ((4) (string-hash     obj))
      ((5) (symbol-hash     obj))
      ((6) (number-hash     obj))
      ((7) (vector-hash     obj))
      ((8) (bytevector-hash obj))
      ;;Add more here.
      (else
       (comparator-hash (registered-comparator (object-type obj)) obj))))

  (define (default-comparison a b)
    (let ((a-type (object-type a))
	  (b-type (object-type b)))
      (cond ((< a-type b-type)	-1)
	    ((> a-type b-type)	+1)
	    (else
	     (dispatch-comparison a-type a b)))))

  (define (default-equality a b)
    (let ((a-type (object-type a))
	  (b-type (object-type b)))
      (if (= a-type b-type)
	  (dispatch-equality a-type a b)
	#f)))

  (define default-comparator
    (make-comparator #t
		     default-equality
		     default-comparison
		     default-hash-function))

  #| end of module |# )


;;;; comparison predicate constructors

(define (make= comparator)
  (lambda args (apply =? comparator args)))

(define (make< comparator)
  (lambda args (apply <? comparator args)))

(define (make> comparator)
  (lambda args (apply >? comparator args)))

(define (make<= comparator)
  (lambda args (apply <=? comparator args)))

(define (make>= comparator)
  (lambda args (apply >=? comparator args)))


;;;; interval (ternary) comparison predicates

(define in-open-interval?
  (case-lambda
   ((comparator a b c)
    (and (<? comparator a b)
	 (<? comparator b c)))
   ((a b c)
    (in-open-interval? default-comparator a b c))))

(define in-closed-interval?
  (case-lambda
   ((comparator a b c)
    (and (<=? comparator a b)
	 (<=? comparator b c)))
   ((a b c)
    (in-closed-interval? default-comparator a b c))))

(define in-open-closed-interval?
  (case-lambda
   ((comparator a b c)
    (and (<? comparator a b)
	 (<=? comparator b c)))
   ((a b c)
    (in-open-interval? default-comparator a b c))))

(define in-closed-open-interval?
  (case-lambda
   ((comparator a b c)
    (and (<=? comparator a b)
	 (<? comparator b c)))
   ((a b c)
    (in-open-interval? default-comparator a b c))))


;;;; comparison predicates

(define (=? comparator a b . objs)
  (if (comparator-equal? comparator a b)
      (if (null? objs)
	  #t
	(apply =? comparator b objs))
    #f))

(define (<? comparator a b . objs)
  (if (eqv? (comparator-compare comparator a b) -1)
      (if (null? objs)
	  #t
	(apply <? comparator b objs))
    #f))

(define (>? comparator a b . objs)
  (if (eqv? (comparator-compare comparator a b) 1)
      (if (null? objs)
	  #t
	(apply >? comparator b objs))
    #f))

(define (<=? comparator a b . objs)
  (if (not (eqv? (comparator-compare comparator a b) 1))
      (if (null? objs)
	  #t
	(apply <=? comparator b objs))
    #f))

(define (>=? comparator a b . objs)
  (if (not (eqv? (comparator-compare comparator a b) -1))
      (if (null? objs)
	  #t
	(apply >=? comparator b objs))
    #f))


;;; minimum and maximum comparison predicate

(define comparator-min
  (case-lambda
   ((comparator a)
    a)
   ((comparator a b)
    (if (<? comparator a b) a b))
   ((comparator a b . objs)
    (comparator-min comparator a (apply comparator-min comparator b objs)))))

(define comparator-max
  (case-lambda
   ((comparator a)
    a)
   ((comparator a b)
    (if (>? comparator a b) a b))
   ((comparator a b . objs)
    (comparator-max comparator a (apply comparator-max comparator b objs)))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
