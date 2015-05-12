;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: utility syntaxes
;;;Date: Fri Oct 21, 2011
;;;
;;;Abstract
;;;
;;;	This library is both  installed and used when expanding Vicare's
;;;	own source code.  For this  reason it must export only: bindings
;;;	imported  by Vicare itself,  syntaxes whose  expansion reference
;;;	only bindings imported by Vicare itself.
;;;
;;;Copyright (C) 2011-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (vicare language-extensions syntaxes)
  (export
    ;; miscellaneous extensions
    define-struct-extended	define-record-type-extended
    let-inline			let*-inline
    debug-assert
    with-pathnames
    with-bytevectors		with-bytevectors/or-false
    callet			callet*
    define-exact-integer->symbol-function

    define-list-of-type-predicate
    define-min/max-comparison
    define-equality/sorting-predicate
    define-inequality-predicate

    ;; arguments validation
    define-argument-validation
    with-arguments-validation
    with-dangerous-arguments-validation
    arguments-validation-forms

    ;; miscellaneous dispatching
    cond-numeric-operand	cond-real-numeric-operand
    cond-exact-integer-operand	cond-inexact-integer-operand
    cond-exact-real-numeric-operand)
  (import (vicare)
    (for (prefix (vicare platform configuration)
		 config.)
	 expand)
    (only (vicare arguments validation)
	  define-argument-validation
	  with-arguments-validation
	  with-dangerous-arguments-validation
	  arguments-validation-forms
	  exact-integer.vicare-arguments-validation)
    (vicare unsafe operations)
    (vicare language-extensions define-record-extended))


;;;; some defining syntaxes

(define-syntax let-inline
  (syntax-rules ()
    ((_ ((?var ?expr) ...) ?body0 . ?body)
     (let-syntax ((?var (identifier-syntax ?expr)) ...)
       ?body0 . ?body))))

(define-syntax let*-inline
  (syntax-rules ()
    ((_ () ?body0 . ?body)
     (begin ?body0 . ?body))
    ((_ ((?var0 ?expr0) (?var ?expr) ...) ?body0 . ?body)
     (let-syntax ((?var0 (identifier-syntax ?expr0)))
       (let*-inline ((?var ?expr) ...)
	 ?body0 . ?body)))))

(define-syntax callet
  ;;Transforms:
  ;;
  ;;   (callet printf (string "ciao ~a") (arg 123))
  ;;
  ;;into:
  ;;
  ;;   (printf "ciao ~a" 123)
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?func ?arg ...)
       (let loop ((args		#'(?arg ...))
		  (keys		'())
		  (exprs	'()))
	 (syntax-case args ()
	   (()
	    #`(?func . #,(reverse exprs)))
	   (((?key ?expr) . ?args)
	    (identifier? #'?key)
	    (loop #'?args
		  (cons #'?key  keys)
		  (cons #'?expr exprs)))
	   (((?key ?expr) . ?args)
	    (syntax-violation 'callet
	      "expected identifier as argument key" stx #'(?key ?expr)))
	   ((?arg . ?args)
	    (loop #'?args
		  (cons (car (generate-temporaries '(#f))) keys)
		  (cons #'?arg exprs)))
	   ))))))

(define-syntax callet*
  ;;Transforms:
  ;;
  ;;   (callet printf (string "ciao ~a") (arg 123))
  ;;
  ;;into:
  ;;
  ;;   (let* ((string "ciao ~a")
  ;;          (arg    123))
  ;;     (printf string arg))
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?func ?arg ...)
       (let loop ((args		#'(?arg ...))
		  (keys		'())
		  (exprs	'()))
	 (syntax-case args ()
	   (()
	    (with-syntax (((KEY  ...) (reverse keys))
			  ((EXPR ...) (reverse exprs)))
	      #`(let* ((KEY EXPR) ...)
		  (?func KEY ...))))
	   (((?key ?expr) . ?args)
	    (identifier? #'?key)
	    (loop #'?args
		  (cons #'?key  keys)
		  (cons #'?expr exprs)))
	   (((?key ?expr) . ?args)
	    (syntax-violation 'callet
	      "expected identifier as argument key" stx #'(?key ?expr)))
	   ((?arg . ?args)
	    (loop #'?args
		  (cons (car (generate-temporaries '(#f))) keys)
		  (cons #'?arg exprs)))
	   ))))))


;;;; common function definition helpers

(define-syntax define-list-of-type-predicate
  (syntax-rules ()
    ((_ ?who ?type-pred)
     (define (?who obj)
       (if (pair? obj)
	   (and (?type-pred (car obj))
		(?who (cdr obj)))
	 (null? obj))))
    ))

(define-syntax define-min/max-comparison
  (syntax-rules ()
    ((_ ?who ?unsafe-who ?type-pred)
     (case-define* ?who
       (({x ?type-pred} {y ?type-pred})
	(?unsafe-who x y))

       (({x ?type-pred} {y ?type-pred} {z ?type-pred} . rest)
	;;FIXME At the  next boot image rotation: this validation  must be integrated
	;;with the signature.  (Marco Maggi; Sun May 10, 2015)
	(for-each (lambda (item)
		    (unless (?type-pred item)
		      (procedure-argument-violation __who__ "failed argument validation" item)))
	  rest)
	(let loop ((a  (?unsafe-who x y))
		   (b  z)
		   (ls rest))
	  (if (pair? ls)
	      (loop (?unsafe-who a b) ($car ls) ($cdr ls))
	    (?unsafe-who a b))))

       (({x ?type-pred})
	x)))
    ))

(define-syntax define-equality/sorting-predicate
  (syntax-rules ()
    ((_ ?who ?unsafe-who ?type-pred)
     (case-define* ?who
       (({obj1 ?type-pred} {obj2 ?type-pred})
	(?unsafe-who obj1 obj2))

       (({obj1 ?type-pred} {obj2 ?type-pred} {obj3 ?type-pred})
	(and (?unsafe-who obj1 obj2)
	     (?unsafe-who obj2 obj3)))

       (({fl ?type-pred})
	#t)

       (({obj1 ?type-pred} {obj2 ?type-pred} {obj3 ?type-pred} {obj4 ?type-pred} . obj*)
	;;FIXME At the  next boot image rotation: this validation  must be integrated
	;;with the signature.  (Marco Maggi; Sun May 10, 2015)
	(for-each (lambda (item)
		    (unless (?type-pred item)
		      (procedure-argument-violation __who__ "failed argument validation" item)))
	  obj*)
	(and (?unsafe-who obj1 obj2)
	     (?unsafe-who obj2 obj3)
	     (?unsafe-who obj3 obj4)
	     (let loop ((objX  obj4)
			(obj*  obj*))
	       (if (pair? obj*)
		   (let ((objY (car obj*)))
		     (and (?unsafe-who objX objY)
			  (loop objY (cdr obj*))))
		 #t))))))
    ))

(define-syntax define-inequality-predicate
  ;;Usage examples:
  ;;
  ;;   (define-inequality-predicate fx!=?     $fx!=     fixnum?)
  ;;   (define-inequality-predicate fl!=?     $fl!=     flonum?)
  ;;   (define-inequality-predicate char!=?   $char!=   char?)
  ;;   (define-inequality-predicate string!=? $string!= string?)
  ;;
  (syntax-rules ()
    ((_ ?who ?unsafe-who ?type-pred)
     (case-define* ?who
       (({obj1 ?type-pred} {obj2 ?type-pred})
	(?unsafe-who obj1 obj2))

       (({obj1 ?type-pred} {obj2 ?type-pred} {obj3 ?type-pred})
	(and (?unsafe-who obj1 obj2)
	     (?unsafe-who obj2 obj3)
	     (?unsafe-who obj3 obj1)))

       (({obj1 ?type-pred} {obj2 ?type-pred} {obj3 ?type-pred} {obj4 ?type-pred} . obj*)
	;;FIXME At the  next boot image rotation: this validation  must be integrated
	;;with the signature.  (Marco Maggi; Sun May 10, 2015)
	(for-each (lambda (item)
		    (unless (?type-pred item)
		      (procedure-argument-violation __who__ "failed argument validation" item)))
	  obj*)
	;;We must compare every argument to all the other arguments.
	(let outer-loop ((objX   obj1)
			 (obj*  (cons* obj2 obj3 obj4 obj*)))
	  (let inner-loop ((objX   objX)
			   (obj^*  obj*))
	    (cond ((pair? obj^*)
		   (and (?unsafe-who objX ($car obj^*))
			(inner-loop objX ($cdr obj^*))))
		  ((pair? obj*)
		   (outer-loop (car obj*) (cdr obj*)))
		  (else #t)))))

       (({obj1 ?type-pred})
	#t)))
    ))


;;;; other syntaxes

(define-syntax debug-assert
  ;;This is meant to expand to nothing when debugging is turned off.
  ;;
  (if #t
      (syntax-rules ()
  	((_ ?pred)
  	 (assert ?pred)))
    (syntax-rules ()
      ((_ ?pred)
       (values)))))

(define-syntax with-pathnames
  (syntax-rules ()
    ((_ ((?pathname.bv ?pathname) ...) . ?body)
     (let ((?pathname.bv (let ((pathname ?pathname))
			   (if (bytevector? pathname)
			       pathname
			     ((string->filename-func) pathname))))
	   ...)
       . ?body))))

(define-syntax with-bytevectors
  ;;Used to  preprocess function arguments which must  be bytevectors or
  ;;strings;  the  strings are  converted  to  bytevectors.  This  macro
  ;;assumes that the arguments have already been validated.
  ;;
  ;;The ?VALUE.BV and ?VALUE input forms must be identifiers.
  ;;
  (syntax-rules ()
    ((_ ((?value.bv ?value) ...) . ?body)
     (let ((?value.bv (let ((V ?value))
			(if (bytevector? V)
			    V
			  (string->latin1 V))))
	   ...)
       . ?body))))

(define-syntax with-bytevectors/or-false
  ;;Used  to preprocess  function arguments  which must  be bytevectors,
  ;;strings or  false; the strings  are converted to  bytevectors.  This
  ;;macro assumes that the arguments have already been validated.
  ;;
  ;;The ?VALUE.BV and ?VALUE input forms must be identifiers.
  ;;
  (syntax-rules ()
    ((_ ((?value.bv ?value) ...) . ?body)
     (let ((?value.bv (let ((V ?value))
			(cond ((bytevector? V)
			       V)
			      ((string? V)
			       (string->latin1 V))
			      (else V))))
	   ...)
       . ?body))))


;;;; math functions dispatching

(define-syntax cond-exact-integer-operand
  (syntax-rules (else fixnum? bignum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     (else		?body-el0 ?body-el ...))))))

(define-syntax cond-inexact-integer-operand
  (syntax-rules (else fixnum? bignum? flonum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     (else		?body-el0 ?body-el ...))))))

(define-syntax cond-exact-real-numeric-operand
  (syntax-rules (else fixnum? bignum? ratnum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rn0 ?body-rn ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rn0 ?body-rn ...)
	     (else		?body-el0 ?body-el ...))))))

;;; --------------------------------------------------------------------

(define-syntax cond-real-numeric-operand
  (syntax-rules (else fixnum? bignum? ratnum? flonum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     (else		?body-el0 ?body-el ...))))

    ;;As above but with flonums before ratnums.
    ;;
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     (else		?body-el0 ?body-el ...))))

    ;;As above but with flonums first.
    ;;
    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     (else		?body-el0 ?body-el ...))))
    ))

;;; --------------------------------------------------------------------

(define-syntax cond-numeric-operand
  (syntax-rules (else
		 zero? exact? inexact?
		 fixnum? bignum? ratnum? flonum? compnum? cflonum? real? complex?)

    ;;Dispatch for all the numeric types.
    ;;
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     (else		?body-el0 ?body-el ...))))

    ;;Dispatch for  all the  numeric types,  but flonums  before ratnums
    ;;because they are more likely.
    ;;
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     (else		?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ;;Dispatch for all the numeric types, but flonums first because they
    ;;are most likely.
    ;;
    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     (else		?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ((_ ?num
	((real?)	?body-re0 ?body-re ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((or (fixnum?  num)
		  (bignum?  num)
		  (ratnum?  num)
		  (flonum?  num))
	      ?body-re0 ?body-re ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     (else		?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((zero?)	?body-zr0 ?body-zr ...)
	((real? exact?)	?body-re0 ?body-re ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (import (only (vicare system $fx)
		     $fxzero?))
       (cond ((flonum? num)
	      ?body-fl0 ?body-fl ...)
	     ((and (fixnum? num) ($fxzero? num))
	      ?body-zr0 ?body-zr ...)
	     ((or (fixnum? num) (bignum? num) (ratnum? num))
	      ?body-re0 ?body-re ...)
	     (else
	      ?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((real? exact?)	?body-re0 ?body-re ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((flonum? num)
	      ?body-fl0 ?body-fl ...)
	     ((or (fixnum? num) (bignum? num) (ratnum? num))
	      ?body-re0 ?body-re ...)
	     (else
	      ?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ;;Dispatch for all the numeric types, complex first.
    ;;
    ((_ ?num
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     (else		?body-el0 ?body-el ...))))
    ))


;;;; miscellaneous stuff

(define-syntax define-exact-integer->symbol-function
  ;;This syntax is used  to define a function that maps  an integer to a
  ;;symbol.  It  is to be  used when  interfacing Vicare with  a foreign
  ;;library; often  such foreign  libraries define constant  integers to
  ;;drive the behaviour of functions.
  ;;
  ;;Here is an example with symbols everybody knows:
  ;;
  ;;   (define SEEK_SET 1)
  ;;   (define SEEK_CUR 2)
  ;;   (define SEEK_END 3)
  ;;
  ;;   (define-exact-integer->symbol-function whence->symbol
  ;;     (SEEK_SET SEEK_CUR SEEK_END))
  ;;
  ;;the syntax will expand to:
  ;;
  ;;   (define (whence->symbol code)
  ;;     (define who 'whence->symbol)
  ;;     (with-arguments-validation (who)
  ;;         ((exact-integer      code))
  ;;       (case code
  ;;         ((SEEK_SET)     'SEEK_SET)
  ;;         ((SEEK_CUR)     'SEEK_CUR)
  ;;         ((SEEK_END)     'SEEK_END)
  ;;         (else #f))))
  ;;
  (syntax-rules ()
    ((_ ?who (?code ...))
     (define (?who code)
       (define who '?who)
       (with-arguments-validation (who)
	   ((exact-integer	code))
	 (case code
	   ((?code)	'?code)
	   ...
	   (else #f)))))))


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'case-one-operand 'scheme-indent-function 1)
;;eval: (put 'case-two-operands 'scheme-indent-function 1)
;;End:
