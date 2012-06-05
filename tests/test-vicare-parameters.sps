;;;
;;;Part of: Vicare Scheme
;;;Contents: test for parameters
;;;Date: Fri Feb  3, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare parameters\n")


(define alpha
  (make-parameter 123))

(define beta
  (make-parameter 0
    (lambda (num)
      (unless (number? num)
	(raise 'woppa))
      num)))

;;; --------------------------------------------------------------------

(check
    (alpha)
  => 123)

(check
    (parameterize ((alpha 456))
      (list (alpha)))
  => '(456))

(check
    (parameterize ((alpha 456))
      (list (alpha)
	    (parameterize ((alpha 789))
	      (alpha))))
  => '(456 789))

;;; --------------------------------------------------------------------

(check
    (beta)
  => 0)

(check
    (parametrise ((beta 456))
      (list (beta)))
  => '(456))

(check
    (parametrise ((beta 456))
      (list (beta)
	    (parametrise ((beta 789))
	      (beta))))
  => '(456 789))

(check
    (guard (exc (else exc))
      (parametrise ((beta 'b))
	(list (beta))))
  => 'woppa)


(define gamma
  (make-parameter 0
    (lambda (num)
      (unless (number? num)
	(raise 'woppa))
      (case num
	((0) 'zero)
	((1) 'one)
	((2) 'two)))))

(check		;the converter is NOT applied to the init value
    (gamma)
  => 0)

(check
    (begin
      (gamma 0)
      (gamma))
  => 'zero)

(check
    (begin
      (gamma 1)
      (gamma))
  => 'one)

(check
    (begin
      (gamma 2)
      (gamma))
  => 'two)


(define-syntax label
  (syntax-rules ()
    ((_ ?name)
     (call/cc	;store the continuation of this form
	 (lambda (k)
	   (set! ?name k))))))

(check ;jump in and out of PARAMETRISE to exercise the dynamic environment
    (let ((outer	#f)
	  (inner	#f)
	  (i		0)
	  (alpha	(make-parameter 0))
	  (result	'()))

      (label outer) ;store the continuation of this form
      (set! result (cons (alpha) result))
      (when inner ;jump back into parametrise after jumping out with OUTER
	(inner))

      (parametrise ((alpha 0))
	(label inner) ;store the continuation of this form
	(alpha (+ 1 (alpha)))
	(if (= 3 i)
	    (list (alpha) (reverse result))
	  (begin
	    (set! i (+ 1 i))
	    ;;jump out of parametrise
	    (outer)))))
  => '(4 (0 0 0 0)))

(check ;check that the converter is invoked only once when entering PARAMETRISE
    (let ((outer	#f)
	  (inner	#f)
	  (i		0)
	  (alpha	(make-parameter 0 number->string))
	  (result	'()))

      (label outer) ;store the continuation of this form
      (set! result (cons (alpha) result))
      (when inner ;jump back into parametrise after jumping out with OUTER
	(inner))

      (parametrise ((alpha 0))
	(label inner) ;store the continuation of this form
	(alpha (+ 1 (string->number (alpha))))
	(if (= 3 i)
	    (list (alpha) (reverse result))
	  (begin
	    (set! i (+ 1 i))
	    ;;jump out of parametrise
	    (outer)))))
  => '("4" (0 0 0 0)))


;;;; done

(check-report)

;;; end of file
