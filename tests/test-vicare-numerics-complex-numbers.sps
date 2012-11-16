;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions, complex numbers
;;;Date: Fri Nov 16, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare numerics functions, complex numbers\n")


;;;; helpers

(define bigN0
  (* 2 (greatest-fixnum)))

(define bigN1
  (* 3 (greatest-fixnum)))

(define (flequal? x y)
  (cond ((list? x)
	 (for-all flequal? x y))
	(else
	 (< (abs (- x y)) 1e-5))))

(define (magarg? x y)
  (let ((x.mag	(car  x))
	(x.arg	(cadr x))
	(y.mag	(car  y))
	(y.arg	(cadr y)))
    (and (flequal? x.mag y.mag)
	 (flequal? (sin x.arg)
		   (sin y.arg)))))

(define-syntax catch
  (syntax-rules (=>)
    ((_ ?flag ?body => ?expected)
     (check
	 (guard (E ((assertion-violation? E)
		    (when ?flag
		      (check-pretty-print E))
		    (condition-irritants E))
		   (else E))
	   ?body)
       => ?expected))))


(parametrise ((check-test-name	'make-rectangular))

  (let ((rep 1)
	(imp 2))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(compnum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1/2)
	(imp 2))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(compnum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1)
	(imp 1/2))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(compnum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1/2)
	(imp 2/4))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(compnum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep bigN0)
	(imp 2))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(compnum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1)
	(imp bigN0))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(compnum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep bigN0)
	(imp bigN1))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(compnum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1.2)
	(imp 2))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(compnum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1)
	(imp 2.4))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(compnum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1.2)
	(imp 2.4))
    (check
	(complex? (make-rectangular rep imp))
      => #t)
    (check
	(cflonum? (make-rectangular rep imp))
      => #t)
    (check
	(let ((Z (make-rectangular rep imp)))
	  (list (real-part Z)
		(imag-part Z)))
      => (list rep imp)))

;;; --------------------------------------------------------------------

  (catch #f
      (make-rectangular 1+2i 3)
    => '(1+2i))

  (catch #f
      (make-rectangular 3 1+2i)
    => '(1+2i))

  #t)


(parametrise ((check-test-name	'make-polar))

  (let ((rep 1)
	(imp 2))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1/2)
	(imp 2))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1)
	(imp 1/2))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1/2)
	(imp 2/4))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep bigN0)
	(imp 2))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1)
	(imp bigN0))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> magarg?) (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep bigN0)
	(imp bigN1))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> magarg?) (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1.2)
	(imp 2))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1)
	(imp 2.4))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list rep imp)))

;;; --------------------------------------------------------------------

  (let ((rep 1.2)
	(imp 2.4))
    (check
	(complex? (make-polar rep imp))
      => #t)
    (check
	(cflonum? (make-polar rep imp))
      => #t)
    (check
	(let ((Z (make-polar rep imp)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list rep imp)))

;;; --------------------------------------------------------------------

  (catch #f
      (make-polar 1+2i 3)
    => '(1+2i))

  (catch #f
      (make-polar 3 1+2i)
    => '(1+2i))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'catch 'scheme-indent-function 2)
;; End:
