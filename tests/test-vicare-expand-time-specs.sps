;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for the expander, type specifications
;;;Date: Fri Feb  7, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare expander object-spec)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: expand-time type specifications\n")


;;;; type specifications

(define* (fixnum {obj fixnum?})
  obj)

(define* (bignum {obj bignum?})
  obj)

(define* (exact-integer {obj exact-integer?})
  obj)

(define* (ratnum {obj ratnum?})
  obj)

(define* (flonum {obj flonum?})
  obj)

(define* (compnum {obj compnum?})
  obj)

(define* (cflonum {obj cflonum?})
  obj)

(define* (char {obj char?})
  obj)

(define* (bytevector {obj bytevector?})
  obj)

(eval-for-expand
  (set-identifier-object-spec! #'fixnum
    (make-object-spec 'fixnum #'fixnum #'fixnum?))

  (set-identifier-object-spec! #'bignum
    (make-object-spec 'bignum #'bignum #'bignum?))

  (set-identifier-object-spec! #'exact-integer
    (make-object-spec 'exact-integer #'exact-integer #'exact-integer?))

  (set-identifier-object-spec! #'ratnum
    (make-object-spec 'ratnum #'ratnum #'ratnum?))

  (set-identifier-object-spec! #'flonum
    (make-object-spec 'flonum #'flonum #'flonum?))

  (set-identifier-object-spec! #'compnum
    (make-object-spec 'compnum #'compnum #'compnum?))

  (set-identifier-object-spec! #'cflonum
    (make-object-spec 'cflonum #'cflonum #'cflonum?))

  ;;;

  (set-identifier-object-spec! #'char
    (make-object-spec 'char #'char #'char?))

  (set-identifier-object-spec! #'cons
    (let ()
      (import (vicare system $pairs))
      (define (accessor-maker slot-id safe?)
	(case-identifiers slot-id
	  ((car) (if safe? #'car #'$car))
	  ((cdr) (if safe? #'cdr #'$cdr))
	  (else
	   (syntax-violation 'pair
	     "invalid slot name for accessor creation"
	     slot-id))))
      (define (mutator-maker slot-id safe?)
	(case-identifiers slot-id
	  ((car) (if safe? #'set-car! #'$set-car!))
	  ((cdr) (if safe? #'set-cdr! #'$set-cdr!))
	  (else
	   (syntax-violation 'pair
	     "invalid slot name for mutation creation" slot-id))))
      (make-object-spec 'pair #'cons #'pair? accessor-maker mutator-maker)))

  (set-identifier-object-spec! #'vector
    (let ()
      (define (accessor-maker slot-id)
	(case-identifiers slot-id
	  ((length) #'length)
	  (else
	   (syntax-violation 'pair
	     "invalid slot name for accessor creation"
	     slot-id))))
      (define (mutator-maker slot-id)
	(case-identifiers slot-id
	  (else
	   (syntax-violation 'pair
	     "invalid slot name for mutation creation" slot-id))))
      (make-object-spec 'vector #'vector #'vector?)))

  (set-identifier-object-spec! #'list
    (make-object-spec 'list #'list #'list?))

  (set-identifier-object-spec! #'bytevector
    (make-object-spec 'bytevector #'bytevector #'bytevector?))

  (set-identifier-object-spec! #'string
    (make-object-spec 'string #'string #'string?))

  #| end of eval-for-expand |# )


(parametrise ((check-test-name	'spec-inspection))

  (check
      (let ()
  	(define-syntax (get-name stx)
  	  (syntax-case stx ()
  	    ((_ ?type-id)
	     #`(quote #,(datum->syntax #'?type-id (object-spec-name (identifier-object-spec #'?type-id)))))
  	    ))
  	(get-name vector))
    => 'vector)

  #t)


(parametrise ((check-test-name	'type-descriptor))

  (check-for-true
   (object-spec? (type-descriptor vector)))

  (check-for-true
   (object-spec? (type-descriptor fixnum)))

  (check-for-true
   (object-spec? (type-descriptor exact-integer)))

  #t)


(parametrise ((check-test-name	'is-a))

  (check
      (values (is-a? 123 fixnum)
	      (is-a? "123" fixnum))
    => #t #f)

  (check
      (values (is-a? 123 exact-integer)
	      (is-a? (least-positive-bignum) exact-integer)
	      (is-a? "123" exact-integer))
    => #t #t #f)

  (check
      (values (is-a? '#(1 2 3) vector)
	      (is-a? "#(1 2 3)" vector))
    => #t #f)

  #t)


(parametrise ((check-test-name	'slots))

  (check
      (values (slot-ref '(1 . 2) car cons)
	      (slot-ref '(1 . 2) cdr cons))
    => 1 2)

  (check
      (values ($slot-ref '(1 . 2) car cons)
	      ($slot-ref '(1 . 2) cdr cons))
    => 1 2)

  (check
      (let ((P (cons 1 2)))
	(slot-set! P car cons 10)
	(slot-set! P cdr cons 20)
	(values (slot-ref P car cons)
		(slot-ref P cdr cons)))
    => 10 20)

;;; --------------------------------------------------------------------

  (check
      (values ((slot-ref <> car cons) '(1 . 2))
	      ((slot-ref <> cdr cons) '(1 . 2)))
    => 1 2)

  (check
      (values (($slot-ref <> car cons) '(1 . 2))
	      (($slot-ref <> cdr cons) '(1 . 2)))
    => 1 2)

  (check
      (let ((P (cons 1 2)))
	((slot-set! <> car cons <>) P 10)
	((slot-set! <> cdr cons <>) P 20)
	(values ((slot-ref <> car cons) P)
		((slot-ref <> cdr cons) P)))
    => 10 20)

  #t)


(parametrise ((check-test-name	'callable-custom))

  (define (func a b c)
    #;(debug-print 'func a b c)
    (* a (+ b c)))

  (define (fxfunc a b c)
    #;(debug-print 'fxfunc a b c)
    (fx* a (fx+ b c)))

  (eval-for-expand

    (define fixnum-spec
      (identifier-object-spec #'fixnum))

    (define exact-integer-spec
      (identifier-object-spec #'exact-integer))

    (set-identifier-callable-spec! #'func
      (make-callable-spec 'func 3 3
			  (lambda (type-a type-b type-c)
			    (cond ((and (eq? type-a fixnum-spec)
					(eq? type-b fixnum-spec)
					(eq? type-c fixnum-spec))
				   (values #'fxfunc exact-integer-spec))
				  (else
				   (values #'func #f))))))

    #| end of eval-for-expand |# )

  (check
      (func 2 3 4)
    => (* 2 (+ 3 4)))

  (check
      (fxfunc 2 3 4)
    => (* 2 (+ 3 4)))

  (check
      (let ()
	(define-syntax (doit stx)
	  (syntax-case stx ()
	    ((_ ?who . ?args)
	     (let ((dispatcher (callable-spec-dispatcher (identifier-callable-spec #'?who)))
		   (fx-spec    (identifier-object-spec #'fixnum)))
	       (receive (id rv-spec)
		   (dispatcher fx-spec fx-spec fx-spec)
		 #`(#,id . ?args))))
	    ))
	(doit func 2 3 4))
    => (* 2 (+ 3 4)))

  #t)


;;;; done

(check-report)

;;; end of file
