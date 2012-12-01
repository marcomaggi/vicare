;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for bignums
;;;Date: Sat Oct  8, 2011
;;;
;;;Abstract
;;;
;;;	Some tests  are from the file  "scheme/tests/bignums.ss" file in
;;;	the original Ikarus distribution.
;;;
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006-2010 Abdulaziz Ghuloum <aghuloum@cs.indiana.edu>
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
(import (ikarus)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Ikarus bignum functions\n")
(set-port-buffer-mode! (current-output-port) (buffer-mode none))


(parametrise ((check-test-name	'reader))

  (define (test x)
    (define (test1 x prefix radix)
      (let ((s (string-append prefix (number->string x radix))))
	(check
	    (read (open-string-input-port s))
	  => x)))
    (test1 x "#x" 16)
    (test1 x "#o" 8)
    (test1 x "#b" 2))

  (test #b11111111111111111111111111111111111111111111111111)
  (test #b1111111111111111111111111111111111111111)
  (test 39487932748923498234)
  (test #b-11111111111111111111111111111111111111111111111111)
  (test #b-1111111111111111111111111111111111111111)
  (test -39487932748923498234)

  #t)


(parametrise ((check-test-name	'length))

  (define (ref ei)
    (do ((result 0 (+ result 1))
	 (bits (if (negative? ei)
		   (bitwise-not ei)
		 ei)
	       (bitwise-arithmetic-shift bits -1)))
        ((zero? bits)
         result)))
  (define (test n)
    (let ((n0 (bitwise-length n))
	  (n1 (ref n)))
      (check n0 => n1)
      #;(unless (= n0 n1)
      (error 'test-bignum-length "mismatch"
      (format "#x~x" n) n0 n1))
      ))
  (test #xF)
  (test #xFF)
  (test #xFFF)
  (test #xFFFF)
  (test #xFFFFF)
  (test #xFFFFFF)
  (test #xFFFFFFF)
  (test #xFFFFFFFF)
  (test #xFFFFFFFFF)
  (test #xFFFFFFFFFF)
  (test #xFFFFFFFFFFF)
  (test #xFFFFFFFFFFFF)
  (test #xFFFFFFFFFFFFF)
  (test #xFFFFFFFFFFFFFF)
  (test #xFFFFFFFFFFFFFFF)
  (test #xFFFFFFFFFFFFFFFF)
  (test #x-F)
  (test #x-FF)
  (test #x-FFF)
  (test #x-FFFF)
  (test #x-FFFFF)
  (test #x-FFFFFF)
  (test #x-FFFFFFF)
  (test #x-FFFFFFFF)
  (test #x-FFFFFFFFF)
  (test #x-FFFFFFFFFF)
  (test #x-FFFFFFFFFFF)
  (test #x-FFFFFFFFFFFF)
  (test #x-FFFFFFFFFFFFF)
  (test #x-FFFFFFFFFFFFFF)
  (test #x-FFFFFFFFFFFFFFF)
  (test #x-FFFFFFFFFFFFFFFF)

  (test #xE)
  (test #xFE)
  (test #xFFE)
  (test #xFFFE)
  (test #xFFFFE)
  (test #xFFFFFE)
  (test #xFFFFFFE)
  (test #xFFFFFFFE)
  (test #xFFFFFFFFE)
  (test #xFFFFFFFFFE)
  (test #xFFFFFFFFFFE)
  (test #xFFFFFFFFFFFE)
  (test #xFFFFFFFFFFFFE)
  (test #xFFFFFFFFFFFFFE)
  (test #xFFFFFFFFFFFFFFE)
  (test #xFFFFFFFFFFFFFFFE)
  (test #x-E)
  (test #x-FE)
  (test #x-FFE)
  (test #x-FFFE)
  (test #x-FFFFE)
  (test #x-FFFFFE)
  (test #x-FFFFFFE)
  (test #x-FFFFFFFE)
  (test #x-FFFFFFFFE)
  (test #x-FFFFFFFFFE)
  (test #x-FFFFFFFFFFE)
  (test #x-FFFFFFFFFFFE)
  (test #x-FFFFFFFFFFFFE)
  (test #x-FFFFFFFFFFFFFE)
  (test #x-FFFFFFFFFFFFFFE)
  (test #x-FFFFFFFFFFFFFFFE)

  (test #x1)
  (test #x1F)
  (test #x1FF)
  (test #x1FFF)
  (test #x1FFFF)
  (test #x1FFFFF)
  (test #x1FFFFFF)
  (test #x1FFFFFFF)
  (test #x1FFFFFFFF)
  (test #x1FFFFFFFFF)
  (test #x1FFFFFFFFFF)
  (test #x1FFFFFFFFFFF)
  (test #x1FFFFFFFFFFFF)
  (test #x1FFFFFFFFFFFFF)
  (test #x1FFFFFFFFFFFFFF)
  (test #x1FFFFFFFFFFFFFFF)
  (test #x-1)
  (test #x-1F)
  (test #x-1FF)
  (test #x-1FFF)
  (test #x-1FFFF)
  (test #x-1FFFFF)
  (test #x-1FFFFFF)
  (test #x-1FFFFFFF)
  (test #x-1FFFFFFFF)
  (test #x-1FFFFFFFFF)
  (test #x-1FFFFFFFFFF)
  (test #x-1FFFFFFFFFFF)
  (test #x-1FFFFFFFFFFFF)
  (test #x-1FFFFFFFFFFFFF)
  (test #x-1FFFFFFFFFFFFFF)
  (test #x-1FFFFFFFFFFFFFFF)

  (test #x1)
  (test #x10)
  (test #x100)
  (test #x1000)
  (test #x10000)
  (test #x100000)
  (test #x1000000)
  (test #x10000000)
  (test #x100000000)
  (test #x1000000000)
  (test #x10000000000)
  (test #x100000000000)
  (test #x1000000000000)
  (test #x10000000000000)
  (test #x100000000000000)
  (test #x1000000000000000)
  (test #x-1)
  (test #x-10)
  (test #x-100)
  (test #x-1000)
  (test #x-10000)
  (test #x-100000)
  (test #x-1000000)
  (test #x-10000000)
  (test #x-100000000)
  (test #x-1000000000)
  (test #x-10000000000)
  (test #x-100000000000)
  (test #x-1000000000000)
  (test #x-10000000000000)
  (test #x-100000000000000)
  (test #x-1000000000000000)

  (test #x1)
  (test #x11)
  (test #x101)
  (test #x1001)
  (test #x10001)
  (test #x100001)
  (test #x1000001)
  (test #x10000001)
  (test #x100000001)
  (test #x1000000001)
  (test #x10000000001)
  (test #x100000000001)
  (test #x1000000000001)
  (test #x10000000000001)
  (test #x100000000000001)
  (test #x1000000000000001)
  (test #x-1)
  (test #x-11)
  (test #x-101)
  (test #x-1001)
  (test #x-10001)
  (test #x-100001)
  (test #x-1000001)
  (test #x-10000001)
  (test #x-100000001)
  (test #x-1000000001)
  (test #x-10000000001)
  (test #x-100000000001)
  (test #x-1000000000001)
  (test #x-10000000000001)
  (test #x-100000000000001)
  (test #x-1000000000000001)

  #t)


(parametrise ((check-test-name	'bitcount))

  (define (test n)
    (define (pos-count-bits n)
      (if (zero? n)
	  0
	(let ((c (count-bits (bitwise-arithmetic-shift-right n 1))))
	  (if (even? n) c (+ c 1)))))
    (define (count-bits n)
      (if (>= n 0)
	  (pos-count-bits n)
	(bitwise-not (pos-count-bits (bitwise-not n)))))
    (check
	(let ((bc0 (bitwise-bit-count n))
	      (bc1 (count-bits n)))
	  (= bc0 bc1))
      => #t)
    #;(unless (= bc0 bc1) (error 'test-bitcount "failed/expected/got" n bc1 bc0))
    )

  (define (test-fx count n inc)
    (when (fixnum? n)
      ;; (when (zero? (fxlogand count #xFFFF))
      ;; 	(printf "bitwise-bit-count ~s\n" n))
      (test n)
      (test-fx (+ count 1) (+ n inc) inc)))

  (if (= (fixnum-width) 30)
      (test-fx 0 (least-fixnum) #xFF)
    (test-fx 0 (least-fixnum) #xFF00000000))

  (test 28472347823493290482390849023840928390482309480923840923840983)
  (test -847234234903290482390849023840928390482309480923840923840983)

  #t)


(parametrise ((check-test-name	'bignums))

		; first, some simple quotients
  (check-for-true ((lambda (x) (= x 101))  (quotient 348972 3434)))
  (check-for-true ((lambda (x) (= x -101)) (quotient -348972 3434)))
  (check-for-true ((lambda (x) (= x -101)) (quotient 348972 -3434)))
  (check-for-true ((lambda (x) (= x 101))  (quotient -348972 -3434)))

		; then bump first argument to a small bignum:
  (check-for-true ((lambda (x) (= x 2255760))  (quotient 536870912 238)))
  (check-for-true ((lambda (x) (= x -2255760)) (quotient -536870912 238)))
  (check-for-true ((lambda (x) (= x -2255760)) (quotient 536870912 -238)))
  (check-for-true ((lambda (x) (= x 2255760))  (quotient -536870912 -238)))

		; then bump first argument to a big bignum:
  (check-for-true ((lambda (x) (= x 1652556267336712615)) (quotient 536870912238479837489374 324873)))
  (check-for-true ((lambda (x) (= x -1652556267336712615)) (quotient -536870912238479837489374 324873)))
  (check-for-true ((lambda (x) (= x -1652556267336712615)) (quotient 536870912238479837489374 -324873)))
  (check-for-true ((lambda (x) (= x 1652556267336712615)) (quotient -536870912238479837489374 -324873)))

		; then make both arguments bignums, but result fixnum:
  (check-for-true ((lambda (x) (= x 165)) (quotient 536870912238479837489374 3248732398479823749283)))
  (check-for-true ((lambda (x) (= x -165)) (quotient -536870912238479837489374 3248732398479823749283)))
  (check-for-true ((lambda (x) (= x -165)) (quotient 536870912238479837489374 -3248732398479823749283)))
  (check-for-true ((lambda (x) (= x 165)) (quotient -536870912238479837489374 -3248732398479823749283)))

		; then both arguments and result are all bignums:
  (check-for-true
   ((lambda (x) (= x 1652555047284588078))
    (quotient 5368709122384798374893743894798327498234 3248732398479823749283)))
  (check-for-true
   ((lambda (x) (= x -1652555047284588078))
    (quotient -5368709122384798374893743894798327498234 3248732398479823749283)))
  (check-for-true
   ((lambda (x) (= x -1652555047284588078))
    (quotient 5368709122384798374893743894798327498234 -3248732398479823749283)))
  (check-for-true
   ((lambda (x) (= x 1652555047284588078))
    (quotient -5368709122384798374893743894798327498234 -3248732398479823749283)))

  (check-for-true ((lambda (x) (= x 23))            (remainder 23 349839489348)))
  (check-for-true ((lambda (x) (= x -23))           (remainder -23 349839489348)))
  (check-for-true ((lambda (x) (= x 23))            (remainder 23 -349839489348)))
  (check-for-true ((lambda (x) (= x -23))           (remainder -23 -349839489348)))

;;; --------------------------------------------------------------------
;;; Next, modulo

		; first, some simple arguments
  (check-for-true ((lambda (x) (= x 2138))  (modulo 348972 3434)))
  (check-for-true ((lambda (x) (= x 1296))  (modulo -348972 3434)))
  (check-for-true ((lambda (x) (= x -1296)) (modulo 348972 -3434)))
  (check-for-true ((lambda (x) (= x -2138)) (modulo -348972 -3434)))

		; then bignum second argument: can be done with +/-
  (check-for-true ((lambda (x) (= x 349839489325))  (modulo -23 349839489348)))
  (check-for-true ((lambda (x) (= x -23))           (modulo -23 -349839489348)))
  (check-for-true ((lambda (x) (= x 23))            (modulo 23 349839489348)))
  (check-for-true ((lambda (x) (= x -349839489325)) (modulo 23 -349839489348)))

		; then bump first argument to a small bignum:
  (check-for-true ((lambda (x) (= x 32))  (remainder 536870912 238)))
  (check-for-true ((lambda (x) (= x -32)) (remainder -536870912 238)))
  (check-for-true ((lambda (x) (= x 32))  (remainder 536870912 -238)))
  (check-for-true ((lambda (x) (= x -32)) (remainder -536870912 -238)))

  (check-for-true ((lambda (x) (= x 32))   (modulo 536870912 238)))
  (check-for-true ((lambda (x) (= x 206))  (modulo -536870912 238)))
  (check-for-true ((lambda (x) (= x -206)) (modulo 536870912 -238)))
  (check-for-true ((lambda (x) (= x -32))  (modulo -536870912 -238)))

		; then bump first argument to a big bignum:
  (check-for-true
   ((lambda (x) (= x 116479))
    (modulo 536870912238479837489374 324873)))
  (check-for-true
   ((lambda (x) (= x 208394))
    (modulo -536870912238479837489374 324873)))
  (check-for-true
   ((lambda (x) (= x -208394))
    (modulo 536870912238479837489374 -324873)))
  (check-for-true
   ((lambda (x) (= x -116479))
    (modulo -536870912238479837489374 -324873)))

		; then make both arguments bignums
  (check-for-true
   ((lambda (x) (= x 830066489308918857679))
    (modulo 536870912238479837489374 3248732398479823749283)))
  (check-for-true
   ((lambda (x) (= x -2418665909170904891604))
    (modulo 536870912238479837489374 -3248732398479823749283)))
  (check-for-true
   ((lambda (x) (= x 2418665909170904891604))
    (modulo -536870912238479837489374 3248732398479823749283)))
  (check-for-true
   ((lambda (x) (= x -830066489308918857679))
    (modulo -536870912238479837489374 -3248732398479823749283)))

  (check-for-true ((lambda (x) (= x -13)) (bitwise-not 12)))
  (check-for-true ((lambda (x) (= x 11)) (bitwise-not -12)))
  (check-for-true ((lambda (x) (= x 0)) (bitwise-not -1)))
  (check-for-true ((lambda (x) (= x -1)) (bitwise-not 0)))
  (check-for-true ((lambda (x) (= x (least-fixnum))) (bitwise-not (greatest-fixnum))))
  (check-for-true ((lambda (x) (= x (greatest-fixnum))) (bitwise-not (least-fixnum))))

  (check-for-true
   ((lambda (x) (= x -38947389478348937489375))
    (bitwise-not 38947389478348937489374)))
  (check-for-true
   ((lambda (x) (= x -22300745198530623141535718272648361505980416))
    (bitwise-not #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))
  (check-for-true
   ((lambda (x) (= x 38947389478348937489374))
    (bitwise-not -38947389478348937489375)))
  (check-for-true
   ((lambda (x) (= x 22300745198530623141535718272648361505980414))
    (bitwise-not #x-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))
  (check-for-true
   ((lambda (x) (= x -340282366920938463463374607431768211456))
    (bitwise-not #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))
  (check-for-true
   ((lambda (x) (= x 340282366920938463463374607431768211454))
    (bitwise-not #x-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))
  (check-for-true
   ((lambda (x) (= x -79228162514264337593543950337))
    (bitwise-not #x1000000000000000000000000)))
  (check-for-true
   ((lambda (x) (= x 79228162514264337593543950335))
    (bitwise-not #x-1000000000000000000000000)))

  #t)


;;;; done

(check-report)

;;; end of file
