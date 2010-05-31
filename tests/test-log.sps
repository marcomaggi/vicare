;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (log nan)
;;;Date: Mon May 31, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (rnrs)
  (rnrs eval)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing log\n")

(define (real-quasi=? ra rb)
  (or (and (infinite? ra)
	   (infinite? rb))
      (< (abs (- ra rb)) 1e-6)))

(define (quasi=? a b)
  (let ((ra (real-part a)) (rb (real-part b))
	(ia (imag-part a)) (ib (imag-part b)))
    ;; (write (list 'xx a b))(newline)
    ;; (write (list 're ra rb (real-quasi=? ra rb)))(newline)
    ;; (write (list 'im ia ib (real-quasi=? ia ib)))(newline)
    ;; (newline)
    (or (and (nan? a)
	     (nan? b))
	(and (real-quasi=? ra rb)
	     (real-quasi=? ia ib)))))


;;;; tests for fixnums

(check
    (guard (E ((assertion-violation? E)
	       (condition-message E))
	      (else #t))
      (eval '(log 0) (environment '(rnrs))))
  => "undefined around 0")

(check (log 1)		=> 0)
(check (log 10)		(=> quasi=?) 2.30258509299)
(check (log -10)	(=> quasi=?) 2.30258509299+3.14159265358793)

(check
    (guard (E (else #t))
      (eval '(log 0) (environment '(rnrs))))
  => #t)

;;;; tests for flonums

(check (log  0.0)			(=> quasi=?) -inf.0)
(check (log  1.0)			(=> quasi=?)  0.0)
(check (log -1.0)			(=> quasi=?)  0.0+3.141592653589793i)
(check (log  10.0)			(=> quasi=?)  2.30258509299)
(check (log -10.0)			(=> quasi=?)  2.30258509299+3.14159265358793i)
(check (log -1.0+0.0i)			(=> quasi=?)  0.0+3.14159265358793i)
(check (log -1.0-0.0i)			(=> quasi=?)  0.0-3.14159265358793i)


(check (log  2.718281828459045)		(=> quasi=?)  1.0)
(check (log  0.36787944117144233)	(=> quasi=?) -1.0)
(check (log  2.0)			(=> quasi=?)  0.6931471805599453)
(check (log 10.0)			(=> quasi=?)  2.302585092994046)
(check (log  0.75)			(=> quasi=?) -0.2876820724517809)
(check (* 2.0 (log -1.0))		(=> quasi=?) 0.0+6.283185307179586i)
(check (log 1024 2)			(=> quasi=?) 10.0)
(check (log 2048.0 2.0)			(=> quasi=?) 11.0)


(check (log +inf.0)		(=> quasi=?) +inf.0)
(check (log -inf.0)		(=> quasi=?) +inf.0+3.14159265358793i)

(check (log +nan.0)		(=> quasi=?) +nan.0)
(check (log +nan.0+10.0i)	(=> quasi=?) +nan.0)
(check (log +nan.0-10.0i)	(=> quasi=?) +nan.0)
(check (log 10.0+nan.0i)	(=> quasi=?) +nan.0)
(check (log 10.0+nan.0i)	(=> quasi=?) +nan.0)


;;;; done

(check-report)

;;; end of file
