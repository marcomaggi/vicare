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


#!vicare
(library (srfi :114 comparators)
  (export
    ;;
    comparator? comparator-comparison-procedure?
    comparator-hash-function?
    ;;
    boolean-comparator char-comparator char-ci-comparator
    string-comparator string-ci-comparator symbol-comparator
    exact-integer-comparator integer-comparator rational-comparator
    real-comparator complex-comparator number-comparator
    pair-comparator list-comparator vector-comparator
    bytevector-comparator
    ;;
    default-comparator comparator-register-default!
    ;;
    make-comparator make-inexact-real-comparator make-vector-comparator
    make-bytevector-comparator make-list-comparator
    make-vectorwise-comparator make-listwise-comparator
    make-car-comparator make-cdr-comparator make-pair-comparator
    make-improper-list-comparator make-selecting-comparator
    make-refining-comparator make-reverse-comparator
    make-debug-comparator
    ;;
    eq-comparator eqv-comparator equal-comparator
    ;;
    comparator-type-test-procedure comparator-equality-predicate
    comparator-comparison-procedure comparator-hash-function
    ;;
    comparator-test-type comparator-check-type comparator-equal?
    comparator-compare comparator-hash
    ;;
    make-comparison< make-comparison> make-comparison<=
    make-comparison>= make-comparison=/< make-comparison=/>
    ;;
    if3 if=? if<? if>? if<=? if>=? if-not=?
    ;;
    =? <? >? <=? >=?
    ;;
    make= make<  make> make<= make>=
    ;;
    in-open-interval? in-closed-interval? in-open-closed-interval?
    in-closed-open-interval?
    ;;
    comparator-min comparator-max
    ;;
    ;; condition objects
    &comparator-error make-comparator-error comparator-error?
    &comparator-type-error make-comparator-type-error comparator-type-error?
    comparator-type-error.comparator comparator-type-error.object
    raise-comparator-type-error)
  (import (vicare))


;;;; helpers

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

(define (%validate-rounding-argument who rounding)
  (case rounding
    ((round ceiling floor truncate)
     (void))
    (else
     (procedure-argument-violation who
       "invalid rounding specification" rounding))))

(define (%validate-nan-handling-argument who nan-handling)
  (case nan-handling
    ((error min max)
     (void))
    (else
     (procedure-argument-violation who
       "invalid nan-handling specification" nan-handling))))

(define (%list-of-comparators? obj)
  (and (list? obj)
       (for-all comparator? obj)))


;;;; condition object types

(define-condition-type &comparator-error
    &error
  make-comparator-error
  comparator-error?)

;;; --------------------------------------------------------------------

(define-condition-type &comparator-type-error
    &comparator-error
  make-comparator-type-error
  comparator-type-error?
  (comparator	comparator-type-error.comparator)
  (object	comparator-type-error.object))

(define* (raise-comparator-type-error who message {comparator comparator?} object)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition (list comparator object))
	      (make-comparator-type-error comparator object))))

(define* (raise-comparator-argument-type-error who message {comparator comparator?} object)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition (list comparator object))
	      (make-procedure-argument-violation)
	      (make-comparator-type-error comparator object))))


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
     (lambda (type-test equality compare hash)
       (fluid-let-syntax ((__who__ (identifier-syntax 'comparator)))
	 (letrec ((cmp (make-record (cond ((eq? type-test #t)
					   (lambda (x) #t))
					  ((procedure? type-test)
					   type-test)
					  (else
					   (procedure-argument-violation __who__
					     "expected procedure or #t as TYPE-TEST argument"
					     type-test)))
				    (cond ((eq? equality #t)
					   (lambda (x y)
					     (eqv? (compare x y) 0)))
					  ((procedure? equality)
					   equality)
					  (else
					   (procedure-argument-violation __who__
					     "expected procedure or #t as EQUALITY argument"
					     equality)))
				    (cond ((not compare)
					   (lambda (x y)
					     (error 'anonymous-comparator
					       "compare not supported by this comparator"
					       cmp)))
					  ((procedure? compare)
					   compare)
					  (else
					   (procedure-argument-violation __who__
					     "expected procedure or #t as COMPARE argument"
					     equality)))
				    (cond ((not hash)
					   (lambda (x y)
					     (error 'anonymous-comparator
					       "hashing not supported by this comparator"
					       cmp)))
					  ((procedure? hash)
					   hash)
					  (else
					   (procedure-argument-violation __who__
					     "expected procedure or #t as HASH argument"
					     hash)))
				    (if compare #t #f)
				    (if hash    #t #f))))
	   cmp)))))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; primitive applicators

(define* (comparator-test-type {comparator comparator?} obj)
  ;;Invoke the test type.
  ;;
  ((comparator-type-test-procedure comparator) obj))

(case-define* comparator-check-type
  (({comparator comparator?} obj who)
   ;;Invoke the test type and throw an error if it fails.
   ;;
   (if (comparator-test-type comparator obj)
       #t
     (raise-comparator-argument-type-error who
       "comparator type check failed" comparator obj)))
  ((comparator obj)
   (comparator-check-type comparator obj __who__)))

(define* (comparator-equal? {comparator comparator?} obj1 obj2)
  ;;Invoke the equality predicate.
  ;;
  (comparator-check-type comparator obj1 __who__)
  (comparator-check-type comparator obj2 __who__)
  ((comparator-equality-predicate comparator) obj1 obj2))

(define* (comparator-compare {comparator comparator?} obj1 obj2)
  ;;Invoke the comparison procedure.
  ;;
  (comparator-check-type comparator obj1 __who__)
  (comparator-check-type comparator obj2 __who__)
  ((comparator-comparison-procedure comparator) obj1 obj2))

(define* (comparator-hash {comparator comparator?} obj)
  ;;Invoke the hash function.
  ;;
  (comparator-check-type comparator obj __who__)
  ((comparator-hash-function comparator) obj))


;;;; comparison procedure comparators
;;
;;These construct comparison procedures based on comparison predicates.
;;

(define* (make-comparison< {< procedure?})
  (lambda (a b)
    (cond ((< a b)	-1)
	  ((< b a)	+1)
	  (else		0))))

(define* (make-comparison> {> procedure?})
  (lambda (a b)
    (cond ((> a b)	+1)
	  ((> b a)	-1)
	  (else		0))))

(define* (make-comparison<= {<= procedure?})
  (lambda (a b)
    (if (<= a b)
	(if (<= b a) 0 -1)
      1)))

(define* (make-comparison>= {>= procedure?})
  (lambda (a b)
    (if (>= a b)
	(if (>= b a) 0 1)
      -1)))

(define* (make-comparison=/< {= procedure?} {< procedure?})
  (lambda (a b)
    (cond ((= a b)	0)
	  ((< a b)	-1)
	  (else		+1))))

(define* (make-comparison=/> {= procedure?} {> procedure?})
  (lambda (a b)
    (cond ((= a b)	0)
	  ((> a b)	+1)
	  (else		-1))))


;;;; the default comparator

(module (default-comparator default-hash-function comparator-register-default!)

  (define-constant unknown-object-comparator
    ;;The unknown-object comparator, used as a fallback to everything else
    ;;
    (make-comparator (lambda (obj) #t)
		     (lambda (a b) #t)
		     (lambda (a b) 0)
		     (lambda (obj) 0)))

  ;;Next index for added comparator.
  ;;
  (define-constant FIRST-COMPARATOR-INDEX	12)
  (define next-comparator-index	9)
  (define *registered-comparators*
    (list unknown-object-comparator))

  (define* (comparator-register-default! {comparator comparator?})
    ;;Register a new comparator for use by the default comparator.
    ;;
    ;;This is intended for other  sample implementations to register their comparator
    ;;objects so that they will be correctly used by the default comparator.  Because
    ;;the whole  idea of registering  comparators depends  on the particulars  of the
    ;;sample  implementation,  it  is  not   portable  to  other  implementations  of
    ;;comparators.

    ;;When  COMPARATOR-REGISTER-DEFAULT!   is  invoked  with  a  single  argument,  a
    ;;comparator,  the default  comparator  is extended  to  understand objects  that
    ;;satisfy  the type  test of  the  registered comparator,  and to  apply it  when
    ;;comparing two  objects that satisfy that  type test.  Such objects  are said to
    ;;belong to "registered types", whereas all other objects other than the built-in
    ;;R7RS-small types belong to "unregistered  types".  When dealing with objects of
    ;;unregistered types, the default comparator makes them compare equal and hash to
    ;;0, a safe but fairly useless default.
    ;;
    ;;The  effect of  comparing two  objects of  different registered  types is  only
    ;;partly    predictable,   as    it    depends   on    the    order   in    which
    ;;`comparator-register-default!` is  called.  However,  the normal  guarantees of
    ;;the default comparator still apply.  In  addition, every object of a registered
    ;;type compares less than all objects of unregistered types.
    ;;
    (set! *registered-comparators* (cons comparator *registered-comparators*))
    (set! next-comparator-index  (add1 next-comparator-index)))

  (define (object-type obj)
    ;;Return ordinal  for object types:  null sorts  before pairs, which  sort before
    ;;booleans, etc.  Implementations can extend this.
    ;;
    ;;People who call COMPARATOR-REGISTER-DEFAULT! effectively do extend it.
    ;;
    (define-syntax-rule (void? ?x)
      (eq? ?x (void)))
    (cond ((null?		obj)	0)
	  ((pair?		obj)	1)
	  ((boolean?		obj)	2)
	  ((char?		obj)	3)
	  ((string?		obj)	4)
	  ((symbol?		obj)	5)
	  ((number?		obj)	6)
	  ((vector?		obj)	7)
	  ((bytevector?		obj)	8)
	  ((void?		obj)	9)
	  ((eof-object?		obj)	10)
	  ((would-block-object? obj)	11)
	  ;;Add more here if you want: be sure to update comparator-index variables.
	  (else
	   (registered-index obj))))

  (define (registered-index obj)
    ;;Return the index for the registered type of OBJ.
    ;;
    (let loop ((i        0)
	       (registry *registered-comparators*))
      (if (pair? registry)
	  (if (comparator-test-type (car registry) obj)
	      (+ FIRST-COMPARATOR-INDEX i)
	    (loop (add1 i) (cdr registry)))
	(+ FIRST-COMPARATOR-INDEX i))))

  (define (registered-comparator i)
    ;;Given an index, retrieve a registered conductor.  Index must be:
    ;;
    ;;   i >= FIRST-COMPARATOR-INDEX
    ;;
    (list-ref *registered-comparators* (- i FIRST-COMPARATOR-INDEX)))

  (define (dispatch-equality type a b)
    (case type
      ((0)	0)	;all empty lists are equal
      ((1)	(fxzero? (pair-comparison       a b)))
      ((2)	(fxzero? (boolean-comparison    a b)))
      ((3)	(fxzero? (char-comparison       a b)))
      ((4)	(fxzero? (string-comparison     a b)))
      ((5)	(fxzero? (symbol-comparison     a b)))
      ((6)	(fxzero? (complex-comparison    a b)))
      ((7)	(fxzero? (vector-comparison     a b)))
      ((8)	(fxzero? (bytevector-comparison a b)))
      ((9)	0)
      ((10)	0)
      ((11)	0)
      ;;Add more here.
      (else
       (comparator-equal? (registered-comparator type) a b))))

  (define (dispatch-comparison type a b)
    (case type
      ((0)	0)	;all empty lists are equal
      ((1)	(pair-comparison       a b))
      ((2)	(boolean-comparison    a b))
      ((3)	(char-comparison       a b))
      ((4)	(string-comparison     a b))
      ((5)	(symbol-comparison     a b))
      ((6)	(complex-comparison    a b))
      ((7)	(vector-comparison     a b))
      ((8)	(bytevector-comparison a b))
      ((9)	eq?)
      ((10)	eq?)
      ((11)	eq?)
      ;;Add more here.
      (else
       (comparator-compare (registered-comparator type) a b))))

  (define (default-hash-function obj)
    (case (object-type obj)
      ((0)	0)
      ((1)	(pair-hash		obj))
      ((2)	(boolean-hash		obj))
      ((3)	(char-hash		obj))
      ((4)	(string-hash		obj))
      ((5)	(symbol-hash		obj))
      ((6)	(number-hash		obj))
      ((7)	(vector-hash		obj))
      ((8)	(bytevector-hash	obj))
      ((9)	(void-hash		obj))
      ((10)	(eof-object-hash	obj))
      ((11)	(would-block-hash	obj))
      ;;Add more here.
      (else
       (comparator-hash (registered-comparator (object-type obj)) obj))))

  (define (default-comparison a b)
    (let ((a.type (object-type a))
	  (b.type (object-type b)))
      (cond ((< a.type b.type)	-1)
	    ((> a.type b.type)	+1)
	    (else
	     (dispatch-comparison a.type a b)))))

  (define (default-equality a b)
    (let ((a.type (object-type a))
	  (b.type (object-type b)))
      (if (= a.type b.type)
	  (dispatch-equality a.type a b)
	#f)))

  (define-predefined-comparator default-comparator
    (make-comparator #t
		     default-equality
		     default-comparison
		     default-hash-function))

  #| end of module |# )


;;;; comparison predicate constructors

(define* (make= {comparator comparator?})
  (lambda args (apply =? comparator args)))

(define* (make< {comparator comparator?})
  (lambda args (apply <? comparator args)))

(define* (make> {comparator comparator?})
  (lambda args (apply >? comparator args)))

(define* (make<= {comparator comparator?})
  (lambda args (apply <=? comparator args)))

(define* (make>= {comparator comparator?})
  (lambda args (apply >=? comparator args)))


;;;; interval (ternary) comparison predicates

(case-define* in-open-interval?
  (({K comparator?} a b c)
   (comparator-check-type K a __who__)
   (comparator-check-type K b __who__)
   (comparator-check-type K c __who__)
   (and (<? K a b)
	(<? K b c)))
  ((a b c)
   (in-open-interval? default-comparator a b c)))

(case-define* in-closed-interval?
  (({K comparator?} a b c)
   (comparator-check-type K a __who__)
   (comparator-check-type K b __who__)
   (comparator-check-type K c __who__)
   (and (<=? K a b)
	(<=? K b c)))
  ((a b c)
   (in-closed-interval? default-comparator a b c)))

(case-define* in-open-closed-interval?
  (({K comparator?} a b c)
   (comparator-check-type K a __who__)
   (comparator-check-type K b __who__)
   (comparator-check-type K c __who__)
   (and (<?  K a b)
	(<=? K b c)))
  ((a b c)
   (in-open-interval? default-comparator a b c)))

(case-define* in-closed-open-interval?
  (({K comparator?} a b c)
   (comparator-check-type K a __who__)
   (comparator-check-type K b __who__)
   (comparator-check-type K c __who__)
   (and (<=? K a b)
	(<?  K b c)))
  ((a b c)
   (in-open-interval? default-comparator a b c)))


;;;; comparison predicates

(define* (=? {K comparator?} a b . objs)
  (comparator-check-type K a __who__)
  (comparator-check-type K b __who__)
  (for-each (lambda (x)
	      (comparator-check-type K x __who__))
    objs)
  (%unsafe-=? K a b objs))

(define (%unsafe-=? K a b objs)
  (if (comparator-equal? K a b)
      (if (pair? objs)
	  (%unsafe-=? K b (car objs) (cdr objs))
	#t)
    #f))

;;; --------------------------------------------------------------------

(define* (<? {K comparator?} a b . objs)
  (comparator-check-type K a __who__)
  (comparator-check-type K b __who__)
  (for-each (lambda (x)
	      (comparator-check-type K x __who__))
    objs)
  (%unsafe-<? K a b objs))

(define (%unsafe-<? K a b objs)
  (if (fx=? -1 (comparator-compare K a b))
      (if (pair? objs)
	  (%unsafe-<? K b (car objs) (cdr objs))
	#t)
    #f))

;;; --------------------------------------------------------------------

(define* (>? {K comparator?} a b . objs)
  (comparator-check-type K a __who__)
  (comparator-check-type K b __who__)
  (for-each (lambda (x)
	      (comparator-check-type K x __who__))
    objs)
  (%unsafe->? K a b objs))

(define (%unsafe->? K a b objs)
  (if (fx=? +1 (comparator-compare K a b))
      (if (pair? objs)
	  (%unsafe->? K b (car objs) (cdr objs))
	#t)
    #f))

;;; --------------------------------------------------------------------

(define* (<=? {K comparator?} a b . objs)
  (comparator-check-type K a __who__)
  (comparator-check-type K b __who__)
  (for-each (lambda (x)
	      (comparator-check-type K x __who__))
    objs)
  (%unsafe-<=? K a b objs))

(define (%unsafe-<=? K a b objs)
  (if (not (fx=? +1 (comparator-compare K a b)))
      (if (pair? objs)
	  (%unsafe-<=? K b (car objs) (cdr objs))
	#t)
    #f))

;;; --------------------------------------------------------------------

(define* (>=? {K comparator?} a b . objs)
  (comparator-check-type K a __who__)
  (comparator-check-type K b __who__)
  (for-each (lambda (x)
	      (comparator-check-type K x __who__))
    objs)
  (%unsafe->=? K a b objs))

(define (%unsafe->=? K a b objs)
  (if (not (fx=? -1 (comparator-compare K a b)))
      (if (pair? objs)
	  (%unsafe->=? K b (car objs) (cdr objs))
	#t)
    #f))


;;; minimum and maximum comparison predicate

(case-define* comparator-min
  (({K comparator?} a)
   (comparator-check-type K a __who__)
   a)
  (({K comparator?} a b)
   (comparator-check-type K a __who__)
   (comparator-check-type K b __who__)
   (%dyadic-comparator-min K a b))
  (({K comparator?} a b . objs)
   (comparator-check-type K a __who__)
   (comparator-check-type K b __who__)
   (for-each (lambda (x)
	       (comparator-check-type K x __who__))
     objs)
   (%dyadic-comparator-min K a
			   (let recur ((objs (cons b objs)))
			     (if (pair? (cdr objs))
				 (%dyadic-comparator-min K (car objs) (recur (cdr objs)))
			       (car objs))))))

(define (%dyadic-comparator-min K a b)
  (if (%unsafe-<? K a b '()) a b))

;;; --------------------------------------------------------------------

(case-define* comparator-max
  (({K comparator?} a)
   (comparator-check-type K a __who__)
   a)
  (({K comparator?} a b)
   (comparator-check-type K a __who__)
   (comparator-check-type K b __who__)
   (if (>? K a b) a b))
  (({K comparator?} a b . objs)
   (comparator-check-type K a __who__)
   (comparator-check-type K b __who__)
   (for-each (lambda (x)
	       (comparator-check-type K x __who__))
     objs)
   (%dyadic-comparator-max K a
			   (let recur ((objs (cons b objs)))
			     (if (pair? (cdr objs))
				 (%dyadic-comparator-max K (car objs) (recur (cdr objs)))
			       (car objs))))))

(define (%dyadic-comparator-max K a b)
  (if (%unsafe->? K a b '()) a b))


;;;; standard comparators and comparator constructors: standard atomic comparators

(define (boolean-comparison a b)
  (cond ((and a b)	0)
	(a		+1)
	(b		-1)
	(else		0)))

(define-predefined-comparator boolean-comparator
  (make-comparator boolean? boolean=? boolean-comparison boolean-hash))

;;; --------------------------------------------------------------------

(define char-comparison
  (make-comparison=/< char=? char<?))

(define-predefined-comparator char-comparator
  (make-comparator char? char=? char-comparison char-hash))

;;; --------------------------------------------------------------------

(define char-ci-comparison
  (make-comparison=/< char-ci=? char-ci<?))

(define char-ci-comparator
  (make-comparator char? char-ci=? char-ci-comparison char-ci-hash))

;;; --------------------------------------------------------------------

(define (real-comparison a b)
  ;;Comparison procedure for real numbers only.
  ;;
  (cond ((< a b)	-1)
	((> a b)	+1)
	(else		0)))

(define (complex-comparison a b)
  ;;Comparison procedure for non-real numbers.
  ;;
  (let ((real-result (real-comparison (real-part a)
				      (real-part b))))
    (if (zero? real-result)
	(real-comparison (imag-part a)
			 (imag-part b))
      real-result)))

(define-predefined-comparator number-comparator
  (make-comparator number? = complex-comparison number-hash))

(define-predefined-comparator complex-comparator
  (make-comparator complex? = complex-comparison number-hash))

(define-predefined-comparator real-comparator
  (make-comparator real? = real-comparison number-hash))

(define-predefined-comparator rational-comparator
  (make-comparator rational? = real-comparison number-hash))

(define-predefined-comparator integer-comparator
  (make-comparator integer? = real-comparison number-hash))

(define-predefined-comparator exact-integer-comparator
  (make-comparator exact-integer? = real-comparison number-hash))

;;; --------------------------------------------------------------------
;;; inexact real comparator

(define (rounded-to x epsilon rounding)
  ;;Return a number appropriately rounded to EPSILON.
  ;;
  (let ((quo (/ x epsilon)))
    (if (procedure? rounding)
	(rounding x epsilon)
      (case rounding
	((round)	(round    quo))
	((ceiling)	(ceiling  quo))
	((floor)	(floor    quo))
	((truncate)	(truncate quo))
	(else
	 (procedure-argument-violation __who__
	   "invalid rounding specification" rounding))))))

(module (make-inexact-real-comparison)

  (define (make-inexact-real-comparison epsilon rounding nan-handling)
    (%validate-rounding-argument     __who__ rounding)
    (%validate-nan-handling-argument __who__ nan-handling)
    (lambda (a b)
      (let ((a-nan? (nan? a))
	    (b-nan? (nan? b)))
	(cond ((and a-nan? b-nan?)
	       0)
	      (a-nan?
	       (nan-comparison nan-handling 'a-nan b))
	      (b-nan?
	       (nan-comparison nan-handling 'b-nan a))
	      (else
	       (real-comparison (rounded-to a epsilon rounding)
				(rounded-to b epsilon rounding)))))))

  (define (nan-comparison nan-handling which other)
    ;;Return result of comparing a NaN with a non-NaN.
    ;;
    (if (procedure? nan-handling)
	(nan-handling other)
      (case nan-handling
	((error)
	 (error __who__
	   "attempt to compare NaN with non-NaN" nan-handling which other))
	((min)
	 (if (eq? which 'a-nan) -1 +1))
	((max)
	 (if (eq? which 'a-nan) +1 -1))
	(else
	 (procedure-argument-violation __who__
	   "invalid nan-handling specification" nan-handling)))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (make-inexact-real-comparator)
  ;;Under Vicare only flonums are inexact and real.
  ;;
  (define (make-inexact-real-comparator epsilon rounding nan-handling)
    (%validate-rounding-argument     __who__ rounding)
    (%validate-nan-handling-argument __who__ nan-handling)
    (make-comparator flonum?
		     #t
		     (make-inexact-real-comparison epsilon rounding nan-handling)
		     (make-inexact-real-hash       epsilon rounding)))

  (define (make-inexact-real-hash epsilon rounding)
    ;;Return 0 for NaN, number-hash otherwise.
    ;;
    (lambda (obj)
      (flonum-hash (rounded-to obj epsilon rounding))))

  #| end of module |# )


;;;; standard comparators and comparator constructors: sequence comparator constructors and comparators
;;
;;The hash functions are  based on djb2, but modulo 2^20 instead of  2^32 in hopes of
;;sticking to fixnums.
;;

(define-constant LIMIT
  (expt 2 20))

;;; --------------------------------------------------------------------

(define* (make-listwise-comparison {comparison procedure?} {list-null? procedure?}
				   {list-car procedure?} {list-cdr procedure?})
  ;;Make a comparison procedure that works listwise.
  ;;
  (letrec ((proc (lambda (a b)
		   ;;A  and  B are  list-like  objects  that  can be  accessed  with:
		   ;;LIST-NULL?,  LIST-CAR,  LIST-CDR.   Notice   that  there  is  no
		   ;;LIST-PAIR? predicate.
		   ;;
		   (let ((a.null? (list-null? a))
			 (b.null? (list-null? b)))
		     (cond ((and a.null? b.null?)	0)
			   (a.null?			-1)
			   (b.null?			+1)
			   (else
			    (let ((result (comparison (list-car a) (list-car b))))
			      (if (zero? result)
				  (proc (list-cdr a) (list-cdr b))
				result))))))))
    proc))

(define* (make-listwise-hash {element-hash procedure?} {list-null? procedure?}
			     {list-car procedure?} {list-cdr procedure?})
  ;;Make a hash function that works listwise.
  ;;
  (lambda (obj)
    (let loop ((obj    obj)
	       (result 5381))
      (if (list-null? obj)
	  result
        (let* ((prod (modulo (* result 33) LIMIT))
               (sum  (+ prod (element-hash (list-car obj)))))
          (loop (list-cdr obj) sum))))))

;;; --------------------------------------------------------------------

(define* (make-vectorwise-comparison {vec-compar procedure?} {vec-length procedure?}
				     {vec-ref procedure?})
  ;;Make a comparison procedure that works vectorwise.
  ;;
  ;;NOTE We know that, under Vicare, vectors and bytevectors have length in the range
  ;;of fixnums.  This function is meant to work on general vector-like objects, which
  ;;might be  non-vectors and  non-bytevectors.  So  we cannot  use the  FX functions
  ;;here, we have to use the general: =, <, add1, sub1, zero?.
  ;;
  (lambda (a b)
    (let ((a.length   (vec-length a))
	  (b.length   (vec-length b)))
      (cond ((< a.length b.length)	-1)
	    ((> a.length b.length)	+1)
	    ;;The lengths are equal.
	    ((zero? a.length)
	     ;;If we are here: it means both the vectors are empty.
	     0)
	    (else
	     ;;If we  are here:  it means the  vectors have the  same length  and are
	     ;;non-empty.
	     (let ((last-index (sub1 a.length)))
	       (let loop ((index 0))
		 (let ((result (vec-compar (vec-ref a index)
					   (vec-ref b index))))
		   (if (zero? result)
		       (if (= index last-index)
			   0
			 (loop (add1 index)))
		     result)))))))))

(define* (make-vectorwise-hash {item-hash procedure?} {vec-length procedure?} {vec-ref procedure?})
  ;;Make a hash function that works vectorwise.
  ;;
  (lambda (obj)
    (let loop ((index   (sub1 (vec-length obj)))
	       (result  5381))
      (if (zero? index)
	  result
        (let* ((prod (modulo (* result 33)
			     LIMIT))
               (sum  (modulo (+ prod (item-hash (vec-ref obj index)))
			     LIMIT)))
          (loop (sub1 index) sum))))))

;;; --------------------------------------------------------------------

(define string-comparison
  (make-comparison=/< string=? string<?))

(define string-ci-comparison
  (make-comparison=/< string-ci=? string-ci<?))

(define-predefined-comparator string-comparator
  (make-comparator string? string=? string-comparison string-hash))

(define-predefined-comparator string-ci-comparator
  (make-comparator string? string-ci=? string-ci-comparison string-ci-hash))

;;; --------------------------------------------------------------------

(define (symbol<? a b)
  (string<? (symbol->string a)
	    (symbol->string b)))

(define symbol-comparison
  (make-comparison=/< symbol=? symbol<?))

(define-predefined-comparator symbol-comparator
  (make-comparator symbol? symbol=? symbol-comparison symbol-hash))

;;; --------------------------------------------------------------------

(define* (make-listwise-comparator {test procedure?} {K comparator?}
				   {null? procedure?} {car procedure?} {cdr procedure?})
  (make-comparator test
		   #t
		   (make-listwise-comparison (comparator-comparison-procedure K) null? car cdr)
		   (make-listwise-hash       (comparator-hash-function        K) null? car cdr)))

(define* (make-vectorwise-comparator {test procedure?} {K comparator?}
				     {vec-length procedure?} {vec-ref procedure?})
  (make-comparator test
		   #t
		   (make-vectorwise-comparison (comparator-comparison-procedure K) vec-length vec-ref)
		   (make-vectorwise-hash       (comparator-hash-function        K) vec-length vec-ref)))

;;; --------------------------------------------------------------------

(define* (make-list-comparator {K comparator?})
  (make-listwise-comparator (lambda (obj)
			      (or (null? obj)
				  (pair? obj)))
			    K null? car cdr))

(define-predefined-comparator list-comparator
  (make-list-comparator default-comparator))

;;; --------------------------------------------------------------------

(define* (make-vector-comparator {K comparator?})
  (make-vectorwise-comparator vector? K vector-length vector-ref))

(define-predefined-comparator vector-comparator
  (make-vector-comparator default-comparator))

(define vector-comparison
  (comparator-comparison-procedure vector-comparator))

(define vector-hash
  (comparator-hash-function vector-comparator))

;;; --------------------------------------------------------------------

(define* (make-bytevector-comparator {K comparator?})
  (make-vectorwise-comparator bytevector? K bytevector-length bytevector-u8-ref))

(define-predefined-comparator bytevector-comparator
  (make-bytevector-comparator default-comparator))

(define bytevector-comparison
  (comparator-comparison-procedure bytevector-comparator))

;;; --------------------------------------------------------------------
;;; pair comparator constructors

(define* (make-car-comparator {K comparator?})
  (make-comparator pair?
		   #t
		   (let ((compare (comparator-comparison-procedure K)))
		     (lambda (a b)
		       (compare (car a) (car b))))
		   (let ((hash (comparator-hash-function K)))
		     (lambda (obj)
		       (hash (car obj))))))

(define* (make-cdr-comparator {K comparator?})
  (make-comparator pair?
		   #t
		   (let ((compare (comparator-comparison-procedure K)))
		     (lambda (a b)
		       (compare (cdr a) (cdr b))))
		   (let ((hash (comparator-hash-function K)))
		     (lambda (obj)
		       (hash (cdr obj))))))

(define* (make-pair-comparison {car-K comparator?} {cdr-K comparator?})
  (let ((car-compare (comparator-comparison-procedure car-K))
	(cdr-compare (comparator-comparison-procedure cdr-K)))
    (lambda (a b)
      (let ((result (car-compare (car a) (car b))))
	(if (zero? result)
	    (cdr-compare (cdr a) (cdr b))
	  result)))))

(define pair-comparison
  (make-pair-comparison default-comparator default-comparator))

(define* (make-pair-hash {car-K comparator?} {cdr-K comparator?})
  (let ((car-hash (comparator-hash-function car-K))
	(cdr-hash (comparator-hash-function cdr-K)))
    (lambda (obj)
      (+ (car-hash (car obj))
	 (cdr-hash (cdr obj))))))

(define* (make-pair-comparator {car-K comparator?} {cdr-K comparator?})
  (make-comparator pair?
		   #t
		   (make-pair-comparison car-K cdr-K)
		   (make-pair-hash       car-K cdr-K)))

(define-predefined-comparator pair-comparator
  (make-pair-comparator default-comparator default-comparator))

(define pair-hash
  (comparator-hash-function pair-comparator))

;;; --------------------------------------------------------------------

(module (make-improper-list-comparison)

  (define* (make-improper-list-comparison {K comparator?})
    (let ((pair-compare (make-pair-comparison K K))
	  (item-compare (comparator-comparison-procedure K)))
      (lambda (a b)
	(let* ((a-type (improper-list-type a))
	       (b-type (improper-list-type b))
	       (result (real-comparison a-type b-type)))
	  (cond ((not (zero? result))
		 result)
		((pair? a)
		 (pair-compare a b))
		((null? a)
		 0)
		(else
		 (item-compare a b)))))))

  (define (improper-list-type obj)
    ;;Compute type index for inexact list comparisons.
    ;;
    (cond ((null? obj)	0)
	  ((pair? obj)	1)
	  (else		2)))

  #| end of module |# )

(define* (make-improper-list-hash {K comparator?})
  (let ((hash (comparator-hash-function K)))
    (lambda (obj)
      (cond ((pair? obj)
	     (+ (hash (car obj))
		(hash (cdr obj))))
	    ((null? obj)
	     0)
	    (else
	     (hash obj))))))

(define* (make-improper-list-comparator {K comparator?})
  (make-comparator #t
		   #t
		   (make-improper-list-comparison K)
		   (make-improper-list-hash       K)))


;;;; wrapped equality predicates
;;
;;These comparators don't have comparison functions.
;;

(define-predefined-comparator eq-comparator
  (make-comparator #t
		   eq?
		   #f
		   default-hash-function))

(define-predefined-comparator eqv-comparator
  (make-comparator #t
		   eqv?
		   #f
		   default-hash-function))

(define-predefined-comparator equal-comparator
  (make-comparator #t
		   equal?
		   #f
		   default-hash-function))


;;;; comparators constructed from other comparators

(define* (make-reverse-comparator {comparator comparator?})
  ;;Reverse the sense of the comparator.
  ;;
  (make-comparator (comparator-type-test-procedure comparator)
		   (comparator-equality-predicate  comparator)
		   (lambda (a b)
		     (- (comparator-compare comparator a b)))
		   (comparator-hash-function comparator)))

(module (make-selecting-comparator
	 make-refining-comparator)

  (module (make-selecting-comparator)
    ;;Selecting comparator: finds the first one that type-tests
    ;;
    (define* (make-selecting-comparator . {comparators %list-of-comparators?})
      (make-comparator (selected-type-test             comparators)
		       (selected-equality-predicate    comparators)
		       (selected-comparison-procedure  comparators)
		       (selected-hash-function         comparators)))

    (define (selected-type-test . comparators)
      (lambda (obj)
	(if (matching-comparator obj comparators) #t #f)))

    (define (selected-equality-predicate comparators)
      (lambda (a b)
	(let ((comparator (matching-comparator a comparators)))
	  (if comparator
	      (comparator-equal? comparator a b)
	    (error 'anonymous-selecting-comparator "no comparator can be selected" a b)))))

    (define (selected-comparison-procedure comparators)
      (lambda (a b)
	(let ((comparator (matching-comparator a comparators)))
	  (if comparator
	      (comparator-compare comparator a b)
	    (error 'anonymous-selecting-comparator "no comparator can be selected" a b)))))

    (define (selected-hash-function comparators)
      (lambda (obj)
	(let ((comparator (matching-comparator obj comparators)))
	  (if comparator
	      (comparator-hash comparator obj)
	    (error 'anonymous-selecting-comparator "no comparator can be selected" obj)))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (make-refining-comparator)

    (define* (make-refining-comparator . {comparators %list-of-comparators?})
      (make-comparator (refined-type-test            comparators)
		       (refined-equality-predicate   comparators)
		       (refined-comparison-procedure comparators)
		       (refined-hash-function        comparators)))

    (define (refined-type-test . comparators)
      (lambda (obj)
	(if (matching-comparator obj comparators) #t #f)))

    (define (refined-equality-predicate comparators)
      ;;Refining comparator:  uses all type-matching  comparators until one  is found
      ;;that can discriminate.
      ;;
      (lambda (a b)
	(let loop ((comparator (matching-comparator a comparators))
		   (first?     #t))
	  (if comparator
	      (if (comparator-equal? comparator a b)
		  (loop (matching-comparator a comparators) #f)
		#f)
	    (if first?
		(error 'anonymous-refining-comparator "no comparator can be selected" a b)
	      #t)))))

    (define (refined-comparison-procedure comparators)
      (lambda (a b)
	(let loop ((comparator (matching-comparator a comparators))
		   (first?     #t))
	  (if comparator
	      (let ((result (comparator-compare comparator a b)))
		(if (eqv? result 0)
		    (loop (matching-comparator a comparators) #f)
		  result))
	    (if first?
		(error 'anonymous-refining-comparator "no comparator can be selected" a b)
	      0)))))

    (define (refined-hash-function comparators)
      (lambda (obj)
	(let loop ((comparators      comparators)
		   (last-comparator  #f))
	  (if (null? comparators)
	      (if last-comparator
		  (comparator-hash last-comparator obj)
		(error 'anonymous-refining-comparator "no comparator can be selected" obj))
	    (if (comparator-test-type (car comparators) obj)
		(loop (cdr comparators)
		      (car comparators))
	      (loop (cdr comparators)
		    last-comparator))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define (matching-comparator obj comparators)
    (cond ((null? comparators)
	   #f)
	  ((comparator-test-type (car comparators) obj)
	   (car comparators))
	  (else
	   (matching-comparator obj (cdr comparators)))))

  #| end of module |# )


;;;; handy debug-assert procedures for debugging comparators

(define (debug-assert bool who what)
  (unless bool
    (error #f
      (string-append (symbol->string what)
		     " failure in "
		     (symbol->string who)))))

(define (debug-deny bool who what)
  (debug-assert (not bool) who what))


;;;; checkers for debugging comparators

(define (check-type-test comparator a)
  (debug-assert (comparator-test-type comparator a) 'type 'validity))

(define (check-reflexive-equality comparator a)
  (debug-assert (comparator-equal? comparator a a) 'equality 'reflexive))

(define (check-reflexive-comparison comparator a)
  (debug-assert (eqv? (comparator-compare comparator a a) 0) 'comparison 'reflexive))

(define (check-symmetric-equality comparator a b)
  (when (comparator-equal? comparator a b)
    (debug-assert (comparator-equal? comparator b a) 'equality 'symmetric))
  (when (not (comparator-equal? comparator a b))
    (debug-deny   (comparator-equal? comparator b a) 'equality 'symmetric)))

(define (check-asymmetric-comparison comparator a b)
  (debug-assert (eqv? (comparator-compare comparator a b)
		      (- (comparator-compare comparator a b)))
		'comparison 'asymmetric))

(define (check-transitive-equality comparator a b c)
  (and (comparator-equal? comparator a b)
       (comparator-equal? comparator b c)
       (debug-assert (comparator-equal? comparator a c) 'equality 'transitive))
  (and (comparator-equal? comparator a b)
       (not (comparator-equal? comparator b c))
       (debug-deny (comparator-equal? comparator a c) 'equality 'transitive))
  (and (not (comparator-equal? comparator a b))
       (comparator-equal? comparator b c)
       (debug-deny (comparator-equal? comparator a c) 'equality 'transitive)))

(define (check-transitive-comparison comparator a b c)
  (define-syntax-rule (<= ?x ?y)
    (<=? comparator ?x ?y))
  (and (<= b a) (<= a c) (debug-assert (<= b c) 'comparison 'transitive))
  (and (<= c a) (<= a b) (debug-assert (<= c b) 'comparison 'transitive))
  (and (<= a b) (<= b c) (debug-assert (<= a c) 'comparison 'transitive))
  (and (<= c b) (<= b a) (debug-assert (<= c a) 'comparison 'transitive))
  (and (<= a c) (<= c b) (debug-assert (<= a b) 'comparison 'transitive))
  (and (<= b c) (<= c a) (debug-assert (<= b a) 'comparison 'transitive)))

(define (check-hash-value value)
  (debug-assert (and (positive? value) (exact-integer? value))
		'validity 'hash-value))

(define (check-all comparator a b c c?)
  (check-type-test comparator a)
  (check-type-test comparator b)
  (if c? (check-type-test comparator c))
  (check-reflexive-equality comparator a)
  (check-reflexive-equality comparator b)
  (if c? (check-reflexive-equality comparator c))
  (check-reflexive-comparison comparator a)
  (check-reflexive-comparison comparator b)
  (if c? (check-reflexive-comparison comparator c))
  (check-symmetric-equality comparator a b)
  (if c? (check-symmetric-equality comparator b c))
  (if c? (check-symmetric-equality comparator a c))
  (check-asymmetric-comparison comparator a b)
  (if c? (check-asymmetric-comparison comparator b c))
  (if c? (check-asymmetric-comparison comparator a c))
  (if c? (check-transitive-equality comparator a b c))
  (if c? (check-transitive-comparison comparator a b c)))

(define (make-debug-comparator comparator)
  (let ((c #f) (c? #f))
    (comparator-comparison-procedure? comparator)
    (make-comparator
     (comparator-type-test-procedure comparator)
     (lambda (a b)
       (check-all comparator a b c c?)
       (when (not c?) (set! c a) (set! c? #t))
       (comparator-equal? comparator a b))
     (if (comparator-comparison-procedure? comparator)
	 (lambda (a b)
	   (check-all comparator a b c c?)
	   (when (not c?) (set! c b) (set! c? #t))
	   (comparator-compare comparator a b))
       #f)
     (if (comparator-hash-function? comparator)
	 (lambda (obj)
	   (let ((value (comparator-hash comparator obj)))
	     (check-hash-value value)
	     value))
       #f))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; eval: (put 'raise-comparator-type-error		'scheme-indent-function 1)
;; eval: (put 'raise-comparator-argument-type-error	'scheme-indent-function 1)
;; End:
