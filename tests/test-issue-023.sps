;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for issue 23
;;;Date: Thu Jun 16, 2011
;;;
;;;Abstract
;;;
;;;	The ATAN function is not implemented for complex numbers.
;;;
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rnrs)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 23, missing atan implementation complex numbers\n")


;;;; helpers

(define (%quasi=? X Y)
  (define (sgn x)
    (cond ((zero? x)		0)
	  ((positive? x)	+1)
	  ((negative? x)	-1)
	  (else			#f)))
  (define epsilon 1e-6)
  (define (zero? x)
    (< (abs x) epsilon))
  (cond ((and (real? X) (real? Y))
	 (or (and (nan? X) (nan? Y))
	     (and (infinite? X) (infinite? Y) (= (sgn X) (sgn Y)))
	     (and (eqv? (sgn X) (sgn Y))
		  (< (abs (- X Y)) epsilon))))
	((and (complex? X) (complex? Y))
	 (and (%quasi=? (real-part X) (real-part Y))
	      (%quasi=? (imag-part X) (imag-part Y))))
	((complex? X)
	 (and (zero? (imag-part X))
	      (%quasi=? (real-part X) Y)))
	((complex? Y)
	 (and (zero? (imag-part Y))
	      (%quasi=? (real-part Y) X)))
	(else
	 (assertion-violation '%quasi=? "invalid arguments" X Y))))


;;;; automatic printing of results

;; (define-syntax doit
;;   (syntax-rules ()
;;     ((_ ?expr)
;;      (begin
;;        (display "(check ")
;;        (write (quote ?expr))
;;        (display "\t\t(=> %quasi=?) ")
;;        (write ?expr)
;;        (display ")\n")))))

;; (doit (atan 0))
;; (doit (atan +1))
;; (doit (atan -1))
;; (doit (atan +13))
;; (doit (atan -13))

;; (doit (atan +0.))
;; (doit (atan -0.))
;; (doit (atan +1.1))
;; (doit (atan -1.1))
;; (doit (atan +13.1))
;; (doit (atan -13.1))

;; (doit (atan +0.0i))
;; (doit (atan -0.0i))
;; (doit (atan +1.0i))
;; (doit (atan -1.0i))
;; (doit (atan +1.1i))
;; (doit (atan -1.1i))
;; (doit (atan +13.1i))
;; (doit (atan -13.1i))

;; (doit (atan +0.0+0.0i))
;; (doit (atan +0.0-0.0i))
;; (doit (atan -0.0+0.0i))
;; (doit (atan -0.0-0.0i))

;; (doit (atan +1.0+1.0i))
;; (doit (atan +1.0-1.0i))
;; (doit (atan -1.0+1.0i))
;; (doit (atan -1.0-1.0i))

;; (doit (atan +13.1+3.2i))
;; (doit (atan +13.1-3.2i))
;; (doit (atan -13.1+3.2i))
;; (doit (atan -13.1-3.2i))


;;;; tests
;;
;;For complex arguments,  the values are produced with  the formula from
;;Wikipedia section "Logarithmic forms":
;;
;;	<http://en.wikipedia.org/wiki/Arc_tangent>:
;;
;;  (* +0.5i
;;     (- (log (+ 1 (* -1i x)))
;;        (log (+ 1 (* +1i x)))))
;;

(check (atan 0)			(=> %quasi=?) 0.0)

(check (atan 1)			(=> %quasi=?) 0.7853981633974483)
(check (atan -1)		(=> %quasi=?) -0.7853981633974483)

(check (atan 13)		(=> %quasi=?) 1.4940244355251187)
(check (atan -13)		(=> %quasi=?) -1.4940244355251187)

(check (atan 0.0)		(=> %quasi=?) 0.0)
(check (atan -0.0)		(=> %quasi=?) -0.0)

(check (atan 1.1)		(=> %quasi=?) 0.8329812666744317)
(check (atan -1.1)		(=> %quasi=?) -0.8329812666744317)

(check (atan 13.1)		(=> %quasi=?) 1.494608206620509)
(check (atan -13.1)		(=> %quasi=?) -1.494608206620509)

(check (atan 0.0+0.0i)		(=> %quasi=?) 0.0+0.0i)
(check (atan 0.0-0.0i)		(=> %quasi=?) 0.0+0.0i)

(check (atan 0.0+1.0i)		(=> %quasi=?) +nan.0+inf.0i)
;; (check (atan 0.0-1.0i)		(=> %quasi=?) +nan.0-inf.0i)

;; (check (atan +0.0+1.1i)		(=> %quasi=?) +1.5707963267948966+1.5222612188617113i)
;; (check (atan +0.0-1.1i)		(=> %quasi=?) +1.5707963267948966-1.5222612188617113i)
;; (check (atan -0.0+1.1i)		(=> %quasi=?) +1.5707963267948966+1.5222612188617113i)
;; (check (atan -0.0-1.1i)		(=> %quasi=?) -1.5707963267948966-1.5222612188617113i)

;; (check (atan +0.0+13.1i)	(=> %quasi=?) 1.5707963267948966+0.07648467239071355i)
;; (check (atan +0.0-13.1i)	(=> %quasi=?) +1.5707963267948966-0.07648467239071355i)
;; (check (atan -0.0+13.1i)	(=> %quasi=?) +1.5707963267948966+0.07648467239071355i)
;; (check (atan -0.0-13.1i)	(=> %quasi=?) -1.5707963267948966-0.07648467239071355i)

;; (check (atan 0.0+0.0i)		(=> %quasi=?) 0.0+0.0i)
;; (check (atan 0.0-0.0i)		(=> %quasi=?) 0.0+0.0i)
;; (check (atan -0.0+0.0i)		(=> %quasi=?) 0.0+0.0i)
;; (check (atan -0.0-0.0i)		(=> %quasi=?) 0.0+0.0i)

;; (check (atan 1.0+1.0i)		(=> %quasi=?) 1.0172219678978514+0.40235947810852507i)
;; (check (atan 1.0-1.0i)		(=> %quasi=?) 1.0172219678978514-0.40235947810852507i)
;; (check (atan -1.0+1.0i)		(=> %quasi=?) -1.0172219678978514+0.40235947810852507i)
;; (check (atan -1.0-1.0i)		(=> %quasi=?) -1.0172219678978514-0.40235947810852507i)

;; (check (atan 13.1+3.2i)		(=> %quasi=?) 1.4988610732266454+0.017507835549069872i)
;; (check (atan 13.1-3.2i)		(=> %quasi=?) 1.4988610732266454-0.017507835549069872i)
;; (check (atan -13.1+3.2i)	(=> %quasi=?) -1.4988610732266454+0.017507835549069872i)
;; (check (atan -13.1-3.2i)	(=> %quasi=?) -1.4988610732266454-0.017507835549069872i)


;;;; done

(check-report)

;;; end of file
