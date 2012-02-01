;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for "ikarus.bytevectors.ss"
;;;Date: Thu Sep 29, 2011
;;;
;;;Abstract
;;;
;;;	Some tests are  from the file "scheme/tests/bytevectors.ss" file
;;;	in the original Ikarus distribution.
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


#!vicare
(import (ikarus)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Ikarus bytevector functions\n")


;;;; helpers

(define (not-bytevector? x)
  (not (bytevector? x)))


(parametrise ((check-test-name	'ikarus))

  (check
      (bytevector? (make-bytevector 1))
    => #t)

  (check
      (bytevector? (make-bytevector 1 17))
    => #t)

  (check
      (bytevector? (make-bytevector 10 -17))
    => #t)

  (check (bytevector? 'foo)			=> #f)
  (check (bytevector? "hey")			=> #f)
  (check (bytevector? (current-output-port))	=> #f)
  (check (bytevector? (current-input-port))	=> #f)
  (check (bytevector? '#(2837 2398 239))	=> #f)

  (check
      (zero? (bytevector-length (make-bytevector 0)))
    => #t)

;;; --------------------------------------------------------------------

  (check-for-true
   ((lambda (x) (= x 100))
    (bytevector-length (make-bytevector 100 -30))))

  (check-for-true
   ((lambda (x) (equal? x '(-127 129 -1 255)))
    (let ((b1 (make-bytevector 16 -127))
	  (b2 (make-bytevector 16 255)))
      (list
       (bytevector-s8-ref b1 0)
       (bytevector-u8-ref b1 0)
       (bytevector-s8-ref b2 0)
       (bytevector-u8-ref b2 0)))))

  (check-for-true
   ((lambda (x) (equal? x '(-126 130 -10 246)))
    (let ((b (make-bytevector 16 -127)))
      (bytevector-s8-set! b 0 -126)
      (bytevector-u8-set! b 1 246)
      (list
       (bytevector-s8-ref b 0)
       (bytevector-u8-ref b 0)
       (bytevector-s8-ref b 1)
       (bytevector-u8-ref b 1)))))

  (check-for-true
   ((lambda (x) (equal? x '(1 2 3 1 2 3 4 8)))
    (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
      (bytevector-copy! b 0 b 3 4)
      (bytevector->u8-list b))))

  (check-for-true
   ((lambda (x) (= x 17))
    (bytevector-uint-ref
     (u8-list->bytevector '(17))
     0 'little 1)))

  (check-for-true
   ((lambda (x) (= x 17))
    (bytevector-uint-ref
     (u8-list->bytevector '(17))
     0 'big 1)))

  (check-for-true
   ((lambda (x) (= x (+ 17 (* 54 256))))
    (bytevector-uint-ref
     (u8-list->bytevector '(17 54))
     0 'little 2)))

  (check-for-true
   ((lambda (x) (= x (+ 17 (* 54 256))))
    (bytevector-uint-ref
     (u8-list->bytevector (reverse '(17 54)))
     0 'big 2)))

  (check-for-true
   ((lambda (x) (= x (+ 17 (* 54 256) (* 98 256 256))))
    (bytevector-uint-ref
     (u8-list->bytevector '(17 54 98))
     0 'little 3)))

  (check-for-true
   ((lambda (x) (= x (+ 17 (* 54 256) (* 98 256 256))))
    (bytevector-uint-ref
     (u8-list->bytevector (reverse '(17 54 98)))
     0 'big 3)))

  (check-for-true
   ((lambda (x) (= x (+ 17 (* 54 256) (* 98 256 256) (* 120 256 256 256))))
    (bytevector-uint-ref
     (u8-list->bytevector '(17 54 98 120))
     0 'little 4)))

  (check-for-true
   ((lambda (x) (= x #x123897348738947983174893204982390489))
    (bytevector-uint-ref
     (u8-list->bytevector
      '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
	     #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12))
     0 'little 18)))

  (check-for-true
   ((lambda (x) (= x #x123897348738947983174893204982390489))
    (bytevector-uint-ref
     (u8-list->bytevector
      (reverse
       '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
	      #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12)))
     0 'big 18)))

  (check-for-true
   ((lambda (x) (equal? x '(513 65283 513 513)))
    (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
      (bytevector->uint-list b 'little 2))))

  (check-for-true
   ((lambda (x) (equal? x '(513 -253 513 513)))
    (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
      (bytevector->sint-list b 'little 2))))

  (check-for-true
   ((lambda (x) (equal? x '(#xfffffffffffffffffffffffffffffffd
		       -3
		       (253 255 255 255 255 255 255 255
			    255 255 255 255 255 255 255 255))))
    (let ((b (make-bytevector 16 -127)))
      (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'little 16)
      (list
       (bytevector-uint-ref b 0 'little 16)
       (bytevector-sint-ref b 0 'little 16)
       (bytevector->u8-list b)))))

  (check-for-true
   ((lambda (x) (equal? x '(#xfffffffffffffffffffffffffffffffd
		       -3
		       (255 255 255 255 255 255 255 255
			    255 255 255 255 255 255 255 253))))
    (let ((b (make-bytevector 16 -127)))
      (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'big 16)
      (list
       (bytevector-uint-ref b 0 'big 16)
       (bytevector-sint-ref b 0 'big 16)
       (bytevector->u8-list b)))))

  (check-for-true
   ((lambda (x) (equal? x '(1 2 3 4)))
    (bytevector->u8-list '#vu8(1 2 3 4))))

  (check-for-true
   ((lambda (x) (= x #xFFFFFFFF))
    (let ((b (make-bytevector 4 0)))
      (bytevector-sint-set! b 0 -1 'little 4)
      (bytevector-uint-ref b 0 'little 4))))

  (check-for-true
   ((lambda (x) (= x #xFFFFFF00))
    (let ((b (make-bytevector 4 0)))
      (bytevector-sint-set! b 0 -256 'little 4)
      (bytevector-uint-ref b 0 'little 4))))

  (check-for-true
   ((lambda (x) (= x #xFFFF0000))
    (let ((b (make-bytevector 4 0)))
      (bytevector-sint-set! b 0 (- (expt 256 2)) 'little 4)
      (bytevector-uint-ref b 0 'little 4))))

  (check-for-true
   ((lambda (x) (= x #xFFFFFFFFFFFF0000))
    (let ((b (make-bytevector 8 0)))
      (bytevector-sint-set! b 0 (- (expt 256 2)) 'little 8)
      (bytevector-uint-ref b 0 'little 8))))

  (check-for-true
   ((lambda (x) (= x #xFFFFFFFF00000000))
    (let ((b (make-bytevector 8 0)))
      (bytevector-sint-set! b 0 (- (expt 256 4)) 'little 8)
      (bytevector-uint-ref b 0 'little 8))))

  (check-for-true
   ((lambda (x) (= x #xFF00000000000000))
    (let ((b (make-bytevector 8 0)))
      (bytevector-sint-set! b 0 (- (expt 256 7)) 'little 8)
      (bytevector-uint-ref b 0 'little 8))))

  (check-for-true
   ((lambda (x) (= x (- 1 (expt 2 63))))
    (let ((b (make-bytevector 8 0)))
      (bytevector-sint-set! b 0 (- 1 (expt 2 63)) 'little 8)
      (bytevector-sint-ref b 0 'little 8))))

  (check-for-true
   ((lambda (x) (= x #x7FFFFFFF))
    (let ((b (make-bytevector 4 38)))
      (bytevector-sint-set! b 0 (sub1 (expt 2 31)) 'little 4)
      (bytevector-sint-ref b 0 'little 4))))

  (check-for-true
   ((lambda (x) (= x #x-80000000))
    (let ((b (make-bytevector 4 38)))
      (bytevector-sint-set! b 0 (- (expt 2 31)) 'little 4)
      (bytevector-sint-ref b 0 'little 4))))

  (check-for-true
   ((lambda (x) (= x #x-100000000))
    (let ((b (make-bytevector 5 38)))
      (bytevector-sint-set! b 0 (- (expt 2 32)) 'little 5)
      (bytevector-sint-ref b 0 'little 5))))

  (check-for-true
   ((lambda (x) (= x #xFFFFFFFF))
    (let ((b (make-bytevector 4 0)))
      (bytevector-sint-set! b 0 -1 'big 4)
      (bytevector-uint-ref b 0 'big 4))))

  (check-for-true
   ((lambda (x) (= x #xFFFFFF00))
    (let ((b (make-bytevector 4 0)))
      (bytevector-sint-set! b 0 -256 'big 4)
      (bytevector-uint-ref b 0 'big 4))))

  (check-for-true
   ((lambda (x) (= x #xFFFF0000))
    (let ((b (make-bytevector 4 0)))
      (bytevector-sint-set! b 0 (- (expt 256 2)) 'big 4)
      (bytevector-uint-ref b 0 'big 4))))

  (check-for-true
   ((lambda (x) (= x #xFFFFFFFFFFFF0000))
    (let ((b (make-bytevector 8 0)))
      (bytevector-sint-set! b 0 (- (expt 256 2)) 'big 8)
      (bytevector-uint-ref b 0 'big 8))))

  (check-for-true
   ((lambda (x) (= x #xFFFFFFFF00000000))
    (let ((b (make-bytevector 8 0)))
      (bytevector-sint-set! b 0 (- (expt 256 4)) 'big 8)
      (bytevector-uint-ref b 0 'big 8))))

  (check-for-true
   ((lambda (x) (= x #xFF00000000000000))
    (let ((b (make-bytevector 8 0)))
      (bytevector-sint-set! b 0 (- (expt 256 7)) 'big 8)
      (bytevector-uint-ref b 0 'big 8))))

  (check-for-true
   ((lambda (x) (= x (- 1 (expt 2 63))))
    (let ((b (make-bytevector 8 0)))
      (bytevector-sint-set! b 0 (- 1 (expt 2 63)) 'big 8)
      (bytevector-sint-ref b 0 'big 8))))

  (check-for-true
   ((lambda (x) (= x #x7FFFFFFF))
    (let ((b (make-bytevector 4 38)))
      (bytevector-sint-set! b 0 (sub1 (expt 2 31)) 'big 4)
      (bytevector-sint-ref b 0 'big 4))))

  (check-for-true
   ((lambda (x) (= x #x-80000000))
    (let ((b (make-bytevector 4 38)))
      (bytevector-sint-set! b 0 (- (expt 2 31)) 'big 4)
      (bytevector-sint-ref b 0 'big 4))))

  (check-for-true
   ((lambda (x) (= x #x-100000000))
    (let ((b (make-bytevector 5 38)))
      (bytevector-sint-set! b 0 (- (expt 2 32)) 'big 5)
      (bytevector-sint-ref b 0 'big 5))))

;;; --------------------------------------------------------------------

  (check-for-true
   ((lambda (x) (= x 65023))
    (bytevector-u16-ref '#vu8(255 253) 0 'little)))

  (check-for-true
   ((lambda (x) (= x 65533))
    (bytevector-u16-ref '#vu8(255 253) 0 'big)))

  (check-for-true
   ((lambda (x) (= x -513))
    (bytevector-s16-ref '#vu8(255 253) 0 'little)))

  (check-for-true
   ((lambda (x) (= x -3))
    (bytevector-s16-ref '#vu8(255 253) 0 'big)))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 2)))
      (bytevector-u16-native-set! v 0 12345)
      (bytevector-u16-native-ref v 0))))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 2)))
      (bytevector-u16-set! v 0 12345 'little)
      (bytevector-u16-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 2)))
      (bytevector-u16-set! v 0 12345 'big)
      (bytevector-u16-ref v 0 'big))))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 2)))
      (bytevector-s16-native-set! v 0 12345)
      (bytevector-s16-native-ref v 0))))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 2)))
      (bytevector-s16-set! v 0 12345 'little)
      (bytevector-s16-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 2)))
      (bytevector-s16-set! v 0 12345 'big)
      (bytevector-s16-ref v 0 'big))))

  (check-for-true
   ((lambda (x) (= x -12345))
    (let ((v (make-bytevector 2)))
      (bytevector-s16-native-set! v 0 -12345)
      (bytevector-s16-native-ref v 0))))

  (check-for-true
   ((lambda (x) (= x -12345))
    (let ((v (make-bytevector 2)))
      (bytevector-s16-set! v 0 -12345 'little)
      (bytevector-s16-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x -12345))
    (let ((v (make-bytevector 2)))
      (bytevector-s16-set! v 0 -12345 'big)
      (bytevector-s16-ref v 0 'big))))

  (check-for-true
   ((lambda (x) (= x 4261412863))
    (let ((v (u8-list->bytevector '(255 255 255 253))))
      (bytevector-u32-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x 4294967293))
    (let ((v (u8-list->bytevector '(255 255 255 253))))
      (bytevector-u32-ref v 0 'big))))

  (check-for-true
   ((lambda (x) (= x -33554433))
    (let ((v (u8-list->bytevector '(255 255 255 253))))
      (bytevector-s32-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x -3))
    (let ((v (u8-list->bytevector '(255 255 255 253))))
      (bytevector-s32-ref v 0 'big))))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 4)))
      (bytevector-u32-set! v 0 12345 'little)
      (bytevector-u32-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 4)))
      (bytevector-u32-set! v 0 12345 'big)
      (bytevector-u32-ref v 0 'big))))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 4)))
      (bytevector-s32-set! v 0 12345 'little)
      (bytevector-s32-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x 12345))
    (let ((v (make-bytevector 4)))
      (bytevector-s32-set! v 0 12345 'big)
      (bytevector-s32-ref v 0 'big))))


  (check-for-true
   ((lambda (x) (= x -12345))
    (let ((v (make-bytevector 4)))
      (bytevector-s32-set! v 0 -12345 'little)
      (bytevector-s32-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x -12345))
    (let ((v (make-bytevector 4)))
      (bytevector-s32-set! v 0 -12345 'big)
      (bytevector-s32-ref v 0 'big))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v (make-bytevector 8)))
      (bytevector-ieee-double-native-set! v 0 17.0)
      (bytevector-ieee-double-native-ref v 0))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v (make-bytevector 8)))
      (bytevector-ieee-double-set! v 0 17.0 'little)
      (bytevector-ieee-double-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v (make-bytevector 8)))
      (bytevector-ieee-double-set! v 0 17.0 'big)
      (bytevector-ieee-double-ref v 0 'big))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v1 (make-bytevector 8)))
      (bytevector-ieee-double-set! v1 0 17.0 'little)
      (let ((v2 (u8-list->bytevector
		 (reverse (bytevector->u8-list v1)))))
	(bytevector-ieee-double-ref v2 0 'big)))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v1 (make-bytevector 8)))
      (bytevector-ieee-double-set! v1 0 17.0 'big)
      (let ((v2 (u8-list->bytevector
		 (reverse (bytevector->u8-list v1)))))
	(bytevector-ieee-double-ref v2 0 'little)))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v (make-bytevector 4)))
      (bytevector-ieee-single-native-set! v 0 17.0)
      (bytevector-ieee-single-native-ref v 0))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v (make-bytevector 4)))
      (bytevector-ieee-single-set! v 0 17.0 'little)
      (bytevector-ieee-single-ref v 0 'little))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v (make-bytevector 4)))
      (bytevector-ieee-single-set! v 0 17.0 'big)
      (bytevector-ieee-single-ref v 0 'big))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v1 (make-bytevector 4)))
      (bytevector-ieee-single-set! v1 0 17.0 'little)
      (let ((v2 (u8-list->bytevector
		 (reverse (bytevector->u8-list v1)))))
	(bytevector-ieee-single-ref v2 0 'big)))))

  (check-for-true
   ((lambda (x) (= x 17.0))
    (let ((v1 (make-bytevector 4)))
      (bytevector-ieee-single-set! v1 0 17.0 'big)
      (let ((v2 (u8-list->bytevector
		 (reverse (bytevector->u8-list v1)))))
	(bytevector-ieee-single-ref v2 0 'little)))))

  (check-for-true
   ((lambda (x) (= x 18302628885633695743))
    (let ((bv (u8-list->bytevector
	       '(255 255 255 255 255 255 255 255
		     255 255 255 255 255 255 255 253))))
      (bytevector-u64-ref bv 8 (endianness little)))))

  (check-for-true
   ((lambda (x) (= x -144115188075855873))
    (let ((bv (u8-list->bytevector
	       '(255 255 255 255 255 255 255 255
		     255 255 255 255 255 255 255 253))))
      (bytevector-s64-ref bv 8 (endianness little)))))

  (check-for-true
   ((lambda (x) (= x 18446744073709551613))
    (let ((bv (u8-list->bytevector
	       '(255 255 255 255 255 255 255 255
		     255 255 255 255 255 255 255 253))))
      (bytevector-u64-ref bv 8 (endianness big)))))

  (check-for-true
   ((lambda (x) (= x -3))
    (let ((bv (u8-list->bytevector
	       '(255 255 255 255 255 255 255 255
		     255 255 255 255 255 255 255 253))))
      (bytevector-s64-ref bv 8 (endianness big)))))

  (check-for-true
   ((lambda (x) (= x 18302628885633695743))
    (let ((bv (u8-list->bytevector
	       '(255 255 255 255 255 255 255 255
		     255 255 255 255 255 255 255 253))))
      (bytevector-u64-native-ref bv 8))))

  (check-for-true
   ((lambda (x) (= x -144115188075855873))
    (let ((bv (u8-list->bytevector
	       '(255 255 255 255 255 255 255 255
		     255 255 255 255 255 255 255 253))))
      (bytevector-s64-native-ref bv 8))))

  (check-for-true
   ((lambda (x) (= x 73))
    (let ((sz (- (* 10 4096) 8)))
      (import (ikarus system $bytevectors))
      (let ((bv (make-bytevector sz)))
	(bytevector-u8-set! bv (- sz 1) 73)
	(collect)
	($bytevector-u8-ref bv (- sz 1))))))

  #t)


;;;; done

(check-report)

;;; end of file
