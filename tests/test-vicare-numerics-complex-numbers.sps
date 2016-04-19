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
;;;Copyright (C) 2012-2013, 2015-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare system $compnums)
  (vicare system $numerics)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions, complex numbers\n")


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

  ;;FIXME To be uncommented  at the next boot image rotation.   (Marco Maggi; Fri May
  ;;8, 2015)
  ;;
  ;; (check-for-procedure-signature-argument-violation
  ;;     (make-rectangular 1+2i 3)
  ;;   => '(make-rectangular 1 real? 1+2i))

  ;;FIXME To be uncommented  at the next boot image rotation.   (Marco Maggi; Fri May
  ;;8, 2015)
  ;;
  ;; (check-for-procedure-signature-argument-violation
  ;;     (make-rectangular 3 1+2i)
  ;;   => '(make-rectangular 2 real? 1+2i))

  #t)


(parametrise ((check-test-name	'make-polar))

  (let ((mag 1)
	(ang 2))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list mag ang)))

;;; --------------------------------------------------------------------

  (let ((mag 1/2)
	(ang 2))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list mag ang)))

;;; --------------------------------------------------------------------

  (let ((mag 1)
	(ang 1/2))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list mag ang)))

;;; --------------------------------------------------------------------

  (let ((mag 1/2)
	(ang 2/4))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list mag ang)))

;;; --------------------------------------------------------------------

  (let ((mag bigN0)
	(ang 2))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list mag ang)))

;;; --------------------------------------------------------------------

  (let ((mag 1)
	(ang bigN0))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> magarg?) (list mag ang)))

;;; --------------------------------------------------------------------

  (let ((mag bigN0)
	(ang bigN1))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> magarg?) (list mag ang)))

;;; --------------------------------------------------------------------

  (let ((mag 1.2)
	(ang 2))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list mag ang)))

;;; --------------------------------------------------------------------

  (let ((mag 1)
	(ang 2.4))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list mag ang)))

;;; --------------------------------------------------------------------

  (let ((mag 1.2)
	(ang 2.4))
    (check
	(complex? (make-polar mag ang))
      => #t)
    (check
	(cflonum? (make-polar mag ang))
      => #t)
    (check
	(let ((Z (make-polar mag ang)))
	  (list (magnitude Z)
		(angle Z)))
      (=> flequal?) (list mag ang)))

  (check
      (magnitude (make-rectangular 1e200 1e200))
    => 1.414213562373095e200)

;;; --------------------------------------------------------------------

  ;;FIXME To be uncommented  at the next boot image rotation.   (Marco Maggi; Fri May
  ;;8, 2015)
  ;;
  ;; (check-for-procedure-signature-argument-violation
  ;;     (make-polar 1+2i 3)
  ;;   => '(make-polar 1 real? 1+2i))

  ;;FIXME To be uncommented  at the next boot image rotation.   (Marco Maggi; Fri May
  ;;8, 2015)
  ;;
  ;; (check-for-procedure-signature-argument-violation
  ;;     (make-polar 3 1+2i)
  ;;   => '(make-polar 2 real? 1+2i))

  #t)


(parametrise ((check-test-name	'conjugate))

;;; fixnums

  (check (complex-conjugate 0)			=> 0)
  (check (complex-conjugate +1)			=> +1)
  (check (complex-conjugate -1)			=> -1)
  (check (complex-conjugate (greatest-fixnum))	=> (greatest-fixnum))
  (check (complex-conjugate (least-fixnum))	=> (least-fixnum))

;;; bignums

  (check (complex-conjugate (+ +1 (greatest-fixnum)))	=> (+ +1 (greatest-fixnum)))
  (check (complex-conjugate (+ -1 (least-fixnum)))	=> (+ -1 (least-fixnum)))

;;; ratnums

  (check (complex-conjugate +1/2)		=> +1/2)
  (check (complex-conjugate -1/2)		=> -1/2)

;;; flonums

  (check (complex-conjugate +0.0)		=> +0.0)
  (check (complex-conjugate -0.0)		=> -0.0)
  (check (complex-conjugate +1.0)		=> +1.0)
  (check (complex-conjugate -1.0)		=> -1.0)
  (check (complex-conjugate +inf.0)		=> +inf.0)
  (check (complex-conjugate -inf.0)		=> -inf.0)
  (check (complex-conjugate +nan.0)		=> +nan.0)

;;; compnums

  (let-syntax
      ((cc (syntax-rules ()
	     ((_ ?op ?expected)
	      (begin
		(check (complex-conjugate ?op)		=> ?expected)
		(check ($complex-conjugate-compnum ?op)	=> ?expected))))))
    (cc +1+1i		+1-1i)
    (cc +1-1i		+1+1i)
    (cc -1+1i		-1-1i)
    (cc -1-1i		-1+1i)

    (cc +1.+1i		+1.-1i)
    (cc +1.-1i		+1.+1i)
    (cc -1.+1i		-1.-1i)
    (cc -1.-1i		-1.+1i)

    (cc +1+1.i		+1-1.i)
    (cc +1-1.i		+1+1.i)
    (cc -1+1.i		-1-1.i)
    (cc -1-1.i		-1+1.i)

    #f)

;;; cflonums

  (let-syntax
      ((cc (syntax-rules ()
	     ((_ ?op ?expected)
	      (begin
		(check (complex-conjugate ?op)		=> ?expected)
		(check ($complex-conjugate-cflonum ?op)	=> ?expected))))))

   (cc +1.0+1.0i	 +1.0-1.0i)
   (cc +1.0-1.0i	 +1.0+1.0i)
   (cc -1.0+1.0i	 -1.0-1.0i)
   (cc -1.0-1.0i	 -1.0+1.0i)

   #f)

  #t)


(parametrise ((check-test-name	'zero))

  (check-for-true	(zero? 0+0.0i))
  (check-for-true	(zero? 0-0.0i))
  (check-for-false	(zero? 1+0.0i))
  (check-for-false	(zero? 0+1.0i))
  (check-for-false	(zero? 0+1i))

  (check-for-true	(non-zero-compnum? 1+0.0i))
  (check-for-true	(non-zero-compnum? 0+1.0i))
  (check-for-false	(non-zero-compnum? 0+0.0i))
  (check-for-false	(non-zero-compnum? 0-0.0i))

  (check-for-true	(non-zero-inexact-compnum? 1+0.0i))
  (check-for-true	(non-zero-inexact-compnum? 0+1.0i))
  (check-for-true	(non-zero-inexact-compnum? 0.0+1i))
  (check-for-false	(non-zero-inexact-compnum? 0+0.0i))
  (check-for-false	(non-zero-inexact-compnum? 0-0.0i))
  (check-for-false	(non-zero-inexact-compnum? 1+2i))

;;; --------------------------------------------------------------------

  (check-for-true	(zero? +0.0+0.0i))
  (check-for-true	(zero? +0.0-0.0i))
  (check-for-true	(zero? -0.0+0.0i))
  (check-for-true	(zero? -0.0-0.0i))
  (check-for-false	(zero? +1.0+0.0i))
  (check-for-false	(zero? +0.0-1.0i))

  (check-for-true	(non-zero-cflonum? +1.0+0.0i))
  (check-for-true	(non-zero-cflonum? +0.0+1.0i))
  (check-for-false	(non-zero-cflonum? +0.0+0.0i))
  (check-for-false	(non-zero-cflonum? +0.0-0.0i))
  (check-for-false	(non-zero-cflonum? -0.0+0.0i))
  (check-for-false	(non-zero-cflonum? -0.0-0.0i))

  (void))


;;;; done

(check-report)

;;; end of file
