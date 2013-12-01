;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for mehve language
;;;Date: Tue Jun 14, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa mehve)
  (prefix (nausicaa mehve language numerics arithmetics)
	  mehve.)
  (prefix (rnrs) rnrs.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Mehve language: arithmetics functions\n")

(initialise-mehve)


;;;; helpers

(define (quasi=? X Y)
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
	 (and (quasi=? (real-part X) (real-part Y))
	      (quasi=? (imag-part X) (imag-part Y))))
	((complex? X)
	 (and (zero? (imag-part X))
	      (quasi=? (real-part X) Y)))
	((complex? Y)
	 (and (zero? (imag-part Y))
	      (quasi=? (real-part Y) X)))
	(else
	 (assertion-violation 'quasi=? "invalid arguments" X Y))))


(parametrise ((check-test-name	'arithmetic-addition))

  (define-method (mehve.addition-1 (o <vector>))
    o)

  (define-method (mehve.addition-2 (a <vector>) (b <vector>))
    (a map + b))

  (define-method (mehve.addition-1 (o <spine>))
    o)

  (define-method (mehve.addition-2 (a <spine>) (b <spine>))
    (a map + b))

;;; --------------------------------------------------------------------

  (check (+)		=> 0)

  ;; one operand
  (check (+ 1)		=> 1)
  (check (+ 1.1)	=> 1.1)
  (check (+ 1.1+2.2i)	=> 1.1+2.2i)

  ;; two operands
  (check (+ 1 3)		(=> quasi=?) 4)
  (check (+ 1.1 3.3)		(=> quasi=?) 4.4)
  (check (+ 1.1+2.2i 3.3+4.4i)	(=> quasi=?) 4.4+6.6i)

  ;; three operands
  (check (+ 1 2 3)		=> 6)

  ;; four operands
  (check (+ 1 2 3 4)		=> 10)

  ;; application
  (check (apply + '())		=> 0)
  (check (apply + '(1))		=> 1)
  (check (apply + 1 '(2))	=> 3)
  (check (apply + 1 '(2 3 4))	=> 10)

  ;; vectors
  (check (+ '#(1 2 3))				=> '#(1 2 3))
  (check (+ '#(1 2 3) '#(4 5 6))		=> '#(5 7 9))
  (check (+ '#(1 2 3) '#(4 5 6) '#(-1 -1 -1))	=> '#(4 6 8))

  ;; lists
  (check (+ '(1 2 3))				=> '(1 2 3))
  (check (+ '(1 2 3) '(4 5 6))			=> '(5 7 9))
  (check (+ '(1 2 3) '(4 5 6) '(-1 -1 -1))	=> '(4 6 8))

;;; --------------------------------------------------------------------

  (check (infix + '(1 2 3))				=> '(1 2 3))
  (check (infix '(1 2 3) + '(4 5 6))			=> '(5 7 9))
  (check (infix '(1 2 3) + '(4 5 6) + '(-1 -1 -1))	=> '(4 6 8))

  (check (infix + '#(1 2 3))				=> '#(1 2 3))
  (check (infix '#(1 2 3) + '#(4 5 6))			=> '#(5 7 9))
  (check (infix '#(1 2 3) + '#(4 5 6) + '#(-1 -1 -1))	=> '#(4 6 8))

  #t)


(parametrise ((check-test-name	'arithmetic-subtraction))

  (define-method (mehve.subtraction-1 (o <vector>))
    (vector-map - o))

  (define-method (mehve.subtraction-2 (a <vector>) (b <vector>))
    (vector-map - a b))

  (define-method (mehve.subtraction-1 (o <spine>))
    (map - o))

  (define-method (mehve.subtraction-2 (a <spine>) (b <spine>))
    (map - a b))

;;; --------------------------------------------------------------------

  (check (-)		=> 0)

  ;; one operand
  (check (- 1)		=> -1)
  (check (- 1.1)	=> -1.1)
  (check (- 1.1-2.2i)	=> -1.1+2.2i)

  ;; two operands
  (check (- 1 3)		(=> quasi=?) -2)
  (check (- 1.1 3.3)		(=> quasi=?) -2.2)
  (check (- 1.1-2.2i 3.3-4.4i)	(=> quasi=?) -2.2+2.2i)

  ;; three operands
  (check (- 1 2 3)		=> -4)

  ;; four operands
  (check (- 1 2 3 4)		=> -8)

  ;; application
  (check (apply - '())		=> 0)
  (check (apply - '(1))		=> -1)
  (check (apply - 1 '(2))	=> -1)
  (check (apply - 1 '(2 3 4))	=> -8)

  ;; vectors
  (check (- '#(1 2 3))				=> '#(-1 -2 -3))
  (check (- '#(1 2 3) '#(4 5 6))		=> '#(-3 -3 -3))
  (check (- '#(1 2 3) '#(4 5 6) '#(-1 -1 -1))	=> '#(-2 -2 -2))

  ;; lists
  (check (- '(1 2 3))				=> '(-1 -2 -3))
  (check (- '(1 2 3) '(4 5 6))			=> '(-3 -3 -3))
  (check (- '(1 2 3) '(4 5 6) '(-1 -1 -1))	=> '(-2 -2 -2))

;;; --------------------------------------------------------------------

  ;; vectors
  (check (infix - '#(1 2 3))				=> '#(-1 -2 -3))
  (check (infix '#(1 2 3) - '#(4 5 6))			=> '#(-3 -3 -3))
  (check (infix '#(1 2 3) - '#(4 5 6) - '#(-1 -1 -1))	=> '#(-2 -2 -2))

  ;; lists
  (check (infix - '(1 2 3))				=> '(-1 -2 -3))
  (check (infix '(1 2 3) - '(4 5 6))			=> '(-3 -3 -3))
  (check (infix '(1 2 3) - '(4 5 6) - '(-1 -1 -1))	=> '(-2 -2 -2))

  #t)


(parametrise ((check-test-name	'arithmetic-multiplication))

  (define-method (mehve.multiplication-1 (o <vector>))
    o)

  (define-method (mehve.multiplication-2 (a <vector>) (b <vector>))
    (vector-map * a b))

  (define-method (mehve.multiplication-1 (o <spine>))
    o)

  (define-method (mehve.multiplication-2 (a <spine>) (b <spine>))
    (map * a b))

;;; --------------------------------------------------------------------

  (check (*)		=> 1)

  ;; one operand
  (check (* 1)		=> 1)
  (check (* 1.1)	=> 1.1)
  (check (* 1.1+2.2i)	=> 1.1+2.2i)

  ;; two operands
  (check (* 1 3)		(=> quasi=?) 3)
  (check (* 1.1 3.3)		(=> quasi=?) (rnrs.* 1.1 3.3))
  (check (* 1.1+2.2i 3.3+4.4i)	(=> quasi=?) (rnrs.* 1.1+2.2i 3.3+4.4i))

  ;; three operands
  (check (* 1 2 3)		=> 6)

  ;; four operands
  (check (* 1 2 3 4)		=> 24)

  ;; application
  (check (apply * '())		=> 1)
  (check (apply * '(1))		=> 1)
  (check (apply * 1 '(2))	=> 2)
  (check (apply * 1 '(2 3 4))	=> 24)

  ;; vectors
  (check (* '#(1 2 3))				=> '#(1 2 3))
  (check (* '#(1 2 3) '#(4 5 6))		=> '#(4 10 18))
  (check (* '#(1 2 3) '#(4 5 6) '#(-1 -1 -1))	=> '#(-4 -10 -18))

  ;; lists
  (check (* '(1 2 3))				=> '(1 2 3))
  (check (* '(1 2 3) '(4 5 6))			=> '(4 10 18))
  (check (* '(1 2 3) '(4 5 6) '(-1 -1 -1))	=> '(-4 -10 -18))

;;; --------------------------------------------------------------------

  (check (infix 1.1 * 3.3)		(=> quasi=?) (rnrs.* 1.1 3.3))
  (check (infix 1.1+2.2i * 3.3+4.4i)	(=> quasi=?) (rnrs.* 1.1+2.2i 3.3+4.4i))

  (check (infix '#(1 2 3) * '#(4 5 6))			=> '#(4 10 18))
  (check (infix '#(1 2 3) * '#(4 5 6) * '#(-1 -1 -1))	=> '#(-4 -10 -18))

  (check (infix '(1 2 3) * '(4 5 6))			=> '(4 10 18))
  (check (infix '(1 2 3) * '(4 5 6) * '(-1 -1 -1))	=> '(-4 -10 -18))

  #t)


(parametrise ((check-test-name	'arithmetic-division))

  (define-method (mehve.division-1 (o <vector>))
    (vector-map / o))

  (define-method (mehve.division-2 (a <vector>) (b <vector>))
    (vector-map / a b))

  (define-method (mehve.division-1 (o <spine>))
    (map / o))

  (define-method (mehve.division-2 (a <spine>) (b <spine>))
    (map / a b))

;;; --------------------------------------------------------------------

  (check (/)		=> +nan.0)

  ;; one operand
  (check (/ 1)		=> 1)
  (check (/ 1.1)	(=> quasi=?) (rnrs./ 1.1))
  (check (/ 1.1+2.2i)	(=> quasi=?) (rnrs./ 1.1+2.2i))

  ;; two operands
  (check (/ 1 3)		(=> quasi=?) (rnrs./ 1 3))
  (check (/ 1.1 3.3)		(=> quasi=?) (rnrs./ 1.1 3.3))
  (check (/ 1.1+2.2i 3.3+4.4i)	(=> quasi=?) (rnrs./ 1.1+2.2i 3.3+4.4i))

  ;; three operands
  (check (/ 1 2 3)		(=> quasi=?) (rnrs./ 1 2 3))

  ;; four operands
  (check (/ 1 2 3 4)		(=> quasi=?) (rnrs./ 1 2 3 4))

  ;; application
  (check (apply / '())		=> +nan.0)
  (check (apply / '(1))		(=> quasi=?) 1)
  (check (apply / 1 '(2))	(=> quasi=?) 1/2)
  (check (apply / 1 '(2 3 4))	(=> quasi=?) (rnrs./ 1 2 3 4))

  ;; vectors
  (check (/ '#(1 2 3))				=> '#(1 1/2 1/3))
  (check (/ '#(1 2 3) '#(4 5 6))		=> '#(1/4 2/5 1/2))
  (check (/ '#(1 2 3) '#(4 5 6) '#(-1 -1 -1))	=> '#(-1/4 -2/5 -1/2))

  ;; lists
  (check (/ '(1 2 3))				=> '(1 1/2 1/3))
  (check (/ '(1 2 3) '(4 5 6))			=> '(1/4 2/5 1/2))
  (check (/ '(1 2 3) '(4 5 6) '(-1 -1 -1))	=> '(-1/4 -2/5 -1/2))

;;; --------------------------------------------------------------------

  (check (infix 1.1 / 3.3)		(=> quasi=?) (rnrs./ 1.1 3.3))
  (check (infix 1.1+2.2i / 3.3+4.4i)	(=> quasi=?) (rnrs./ 1.1+2.2i 3.3+4.4i))

  (check (infix '#(1 2 3) / '#(4 5 6))			=> '#(1/4 2/5 1/2))
  (check (infix '#(1 2 3) / '#(4 5 6) / '#(-1 -1 -1))	=> '#(-1/4 -2/5 -1/2))

  ;; lists
  (check (infix '(1 2 3) / '(4 5 6))			=> '(1/4 2/5 1/2))
  (check (infix '(1 2 3) / '(4 5 6) / '(-1 -1 -1))	=> '(-1/4 -2/5 -1/2))

  #t)


(parametrise ((check-test-name	'arithmetic-division-and-modulus))

  (define-method (mehve.div (a <vector>) (b <vector>))
    (vector-map div a b))

  (define-method (mehve.div (a <spine>) (b <spine>))
    (map div a b))

  (define-method (mehve.div0 (a <vector>) (b <vector>))
    (vector-map div0 a b))

  (define-method (mehve.div0 (a <spine>) (b <spine>))
    (map div0 a b))

  (define-method (mehve.mod (a <vector>) (b <vector>))
    (vector-map mod a b))

  (define-method (mehve.mod (a <spine>) (b <spine>))
    (map mod a b))

  (define-method (mehve.mod0 (a <vector>) (b <vector>))
    (vector-map mod0 a b))

  (define-method (mehve.mod0 (a <spine>) (b <spine>))
    (map mod0 a b))

  (define-method (mehve.div-and-mod (a <vector>) (b <vector>))
    (let (((D <vector>) (make-vector (a length)))
	  ((M <vector>) (make-vector (a length))))
      (do ((i 0 (+ 1 i)))
	  ((= i (a length))
	   (values D M))
	(receive (d m)
	    (div-and-mod (a[i]) (b[i]))
	  (set! D[i] d)
	  (set! M[i] m)))))

  (define-method (mehve.div-and-mod (a <spine>) (b <spine>))
    (let recur ((a a) (b b))
      (if (null? a)
	  (values '() '())
	(receive (d m)
	    (div-and-mod (car a) (car b))
	  (receive (D M)
	      (recur (cdr a) (cdr b))
	    (values (cons d D) (cons m M)))))))

  (define-method (mehve.div0-and-mod0 (a <vector>) (b <vector>))
    (let (((D <vector>) (make-vector (a length)))
	  ((M <vector>) (make-vector (a length))))
      (do ((i 0 (+ 1 i)))
	  ((= i (a length))
	   (values D M))
	(receive (d m)
	    (div0-and-mod0 (a[i]) (b[i]))
	  (set! D[i] d)
	  (set! M[i] m)))))

  (define-method (mehve.div0-and-mod0 (a <spine>) (b <spine>))
    (let recur ((a a) (b b))
      (if (null? a)
	  (values '() '())
	(receive (d m)
	    (div0-and-mod0 (car a) (car b))
	  (receive (D M)
	      (recur (cdr a) (cdr b))
	    (values (cons d D) (cons m M)))))))

;;; --------------------------------------------------------------------

  (define-syntax %check-div-and-mod
    (syntax-rules ()
      ((_ ?fun ?rnrs.fun ?num ?den)
       (check
	   (receive (d m)
	       (?fun ?num ?den)
	     (list d m))
	 => (receive (d m)
		(?rnrs.fun ?num ?den)
	      (list d m))))))

;;; --------------------------------------------------------------------

  ;; two operands
  (check (div  1 3)			=> (rnrs.div  1 3))
  (check (div0 1 3)			=> (rnrs.div0 1 3))
  (check (mod  1 3)			=> (rnrs.mod  1 3))
  (check (mod0 1 3)			=> (rnrs.mod0 1 3))
  (%check-div-and-mod div-and-mod   rnrs.div-and-mod   10 3)
  (%check-div-and-mod div0-and-mod0 rnrs.div0-and-mod0 10 3)

  ;; application
  (check (apply div  10 '(2))		=> (rnrs.div  10 2))
  (check (apply div  '(2 3))		=> (rnrs.div  2 3))
  (check (apply div0 10 '(2))		=> (rnrs.div0 10 2))
  (check (apply div0 '(2 3))		=> (rnrs.div0 2 3))
  (check (apply mod  10 '(2))		=> (rnrs.mod  10 2))
  (check (apply mod  '(2 3))		=> (rnrs.mod  2 3))
  (check (apply mod0 10 '(2))		=> (rnrs.mod0 10 2))
  (check (apply mod0 '(2 3))		=> (rnrs.mod0 2 3))

  ;; vectors
  (check
      (div '#(10 20 30) '#(2 2 2))
    => (vector-map rnrs.div '#(10 20 30) '#(2 2 2)))
  (check
      (div0 '#(10 20 30) '#(2 2 2))
    => (vector-map rnrs.div0 '#(10 20 30) '#(2 2 2)))
  (check
      (mod '#(10 20 30) '#(2 2 2))
    => (vector-map rnrs.mod '#(10 20 30) '#(2 2 2)))
  (check
      (mod0 '#(10 20 30) '#(2 2 2))
    => (vector-map rnrs.mod0 '#(10 20 30) '#(2 2 2)))
  (check
      (receive (d m)
	  (div-and-mod '#(10 20 30) '#(3 4 5))
	(list d m))
    => (list (vector (div 10 3) (div 20 4) (div 30 5))
	     (vector (mod 10 3) (mod 20 4) (mod 30 5))))
  (check
      (receive (d m)
	  (div0-and-mod0 '#(10 20 30) '#(3 4 5))
	(list d m))
    => (list (vector (div0 10 3) (div0 20 4) (div0 30 5))
	     (vector (mod0 10 3) (mod0 20 4) (mod0 30 5))))

  ;; lists
  (check
      (div '(10 20 30) '(2 2 2))
    => (map rnrs.div '(10 20 30) '(2 2 2)))
  (check
      (div0 '(10 20 30) '(2 2 2))
    => (map rnrs.div0 '(10 20 30) '(2 2 2)))
  (check
      (mod '(10 20 30) '(2 2 2))
    => (map rnrs.mod '(10 20 30) '(2 2 2)))
  (check
      (mod0 '(10 20 30) '(2 2 2))
    => (map rnrs.mod0 '(10 20 30) '(2 2 2)))
  (check
      (receive (d m)
	  (div-and-mod '(10 20 30) '(3 4 5))
	(list d m))
    => (list (list (div 10 3) (div 20 4) (div 30 5))
	     (list (mod 10 3) (mod 20 4) (mod 30 5))))
  (check
      (receive (d m)
	  (div0-and-mod0 '(10 20 30) '(3 4 5))
	(list d m))
    => (list (list (div0 10 3) (div0 20 4) (div0 30 5))
	     (list (mod0 10 3) (mod0 20 4) (mod0 30 5))))

  #t)


(parametrise ((check-test-name	'arithmetic-absolute))

  (define-method (mehve.abs (o <vector>))
    (o map abs))

  (define-method (mehve.abs (o <spine>))
    (o map abs))

;;; --------------------------------------------------------------------

  ;; one operand
  (check (abs 3)	=> 3)
  (check (abs -3)	=> 3)
  (check (abs 1.1)	=> 1.1)
  (check (abs -1.1)	=> 1.1)

  ;; application
  (check (apply abs '(3))	=> 3)
  (check (apply abs '(-3))	=> 3)

  ;; vectors
  (check (abs '#(1 -2 -3))	=> '#(1 2 3))

  ;; lists
  (check (abs '(1 -2 -3))	=> '(1 2 3))

;;; --------------------------------------------------------------------

  (check (infix abs (-1.1))	=> 1.1)

  (check (infix abs ('#(1 -2 -3)))	=> '#(1 2 3))

  (check (infix abs ('(1 -2 -3)))	=> '(1 2 3))

  #t)


;;;; done

(check-report)

;;; end of file
