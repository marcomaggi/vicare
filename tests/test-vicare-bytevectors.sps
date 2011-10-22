;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for bytevector functions
;;;Date: Fri Oct 21, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (ikarus)
		(parameterize	parametrise))
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare bytevector functions\n")


;;;; syntax helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))


;;;; helpers

(define BIGNUM
  (+ 1 (greatest-fixnum)))


(parametrise ((check-test-name	'make-bytevector))

  (check
      (let ((bv (make-bytevector 0)))
	(list (bytevector? bv) (bytevector-length bv) bv))
    => '(#t 0 #vu8()))

  (check
      (let ((bv (make-bytevector 1 123)))
	(list (bytevector? bv) (bytevector-length bv) bv))
    => '(#t 1 #vu8(123)))

  (check
      (let ((bv (make-bytevector 3 123)))
	(list (bytevector? bv) (bytevector-length bv) bv))
    => '(#t 3 #vu8(123 123 123)))

;;; --------------------------------------------------------------------
;;; arguments validation: length

  (check	;length is not an integer
      (catch #f (make-bytevector #\a))
    => '(#\a))

  (check	;length is not an exact integer
      (catch #f (make-bytevector 1.0))
    => '(1.0))

  (check	;length is not a fixnum
      (catch #f (make-bytevector BIGNUM))
    => (list BIGNUM))

  (check	;length is negative
      (catch #f (make-bytevector -2))
    => '(-2))

;;; --------------------------------------------------------------------
;;; arguments validation: byte filler

  (check	;filler is not a fixnum
      (catch #f (make-bytevector 3 #\a))
    => '(#\a))

  (check	;filler is too positive
      (catch #f (make-bytevector 2 256))
    => '(256))

  (check	;filler is too negative
      (catch #f (make-bytevector 2 -129))
    => '(-129))

  #t)


(parametrise ((check-test-name	'bytevector-fill-bang))

  (check
      (let ((bv (make-bytevector 0)))
	(bytevector-fill! bv 1)
	bv)
    => #vu8())

  (check
      (let ((bv (make-bytevector 1)))
	(bytevector-fill! bv 123)
	bv)
    => #vu8(123))

  (check
      (let ((bv (make-bytevector 3)))
	(bytevector-fill! bv 123)
	bv)
    => #vu8(123 123 123))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f (bytevector-fill! #\a 1))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: byte filler

  (check	;filler is not a fixnum
      (catch #f (bytevector-fill! #vu8() #\a))
    => '(#\a))

  (check	;filler is too positive
      (catch #f (bytevector-fill! #vu8() 256))
    => '(256))

  (check	;filler is too negative
      (catch #f (bytevector-fill! #vu8() -129))
    => '(-129))

  #t)


(parametrise ((check-test-name	'bytevector-length))

  (check
      (let ((bv (make-bytevector 0)))
	(bytevector-length bv))
    => 0)

  (check
      (let ((bv (make-bytevector 1)))
	(bytevector-length bv))
    => 1)

  (check
      (let ((bv (make-bytevector 3)))
	(bytevector-length bv))
    => 3)

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f (bytevector-length #\a))
    => '(#\a))

  #t)


(parametrise ((check-test-name	'bytevector-equal))

  (check
      (let ((x (make-bytevector 0))
	    (y (make-bytevector 0)))
	(bytevector=? x y))
    => #t)

  (check
      (let ((x (make-bytevector 1))
	    (y (make-bytevector 0)))
	(bytevector=? x y))
    => #f)

  (check
      (let ((x (make-bytevector 0))
	    (y (make-bytevector 1)))
	(bytevector=? x y))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((x (make-bytevector 1 123))
	    (y (make-bytevector 1 123)))
	(bytevector=? x y))
    => #t)

  (check
      (let ((x (make-bytevector 1 7))
	    (y (make-bytevector 1 2)))
	(bytevector=? x y))
    => #f)

  (check
      (let ((x (make-bytevector 2 123))
	    (y (make-bytevector 1 123)))
	(bytevector=? x y))
    => #f)

  (check
      (bytevector=? #vu8(1 2 3) #vu8(1 2 3))
    => #t)

  (check
      (bytevector=? #vu8(1 2 3) #vu8(1 2 30))
    => #f)

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f (bytevector=? #\a #vu8()))
    => '(#\a))

  (check
      (catch #f (bytevector=? #vu8() #\a))
    => '(#\a))

  #t)


(parametrise ((check-test-name	'bytevector-copy))

  (check
      (bytevector-copy #vu8())
    => #vu8())

  (check
      (bytevector-copy #vu8(1))
    => #vu8(1))

  (check
      (bytevector-copy #vu8(1 2 3))
    => #vu8(1 2 3))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-copy #\a))
    => '(#\a))

  #t)


(parametrise ((check-test-name	'bytevector-copy-bang))

  (check
      (let ((src	(bytevector-copy #vu8(1 2 3)))
	    (dst	(bytevector-copy #vu8(0 0 0)))
	    (src.start	0)
	    (dst.start	0)
	    (count	3))
	(bytevector-copy! src src.start dst dst.start count)
	dst)
    => #vu8(1 2 3))

  (check
      (let ((src	(bytevector-copy #vu8(1  2  3)))
	    (dst	(bytevector-copy #vu8(10 20 30)))
	    (src.start	0)
	    (dst.start	0)
	    (count	0))
	(bytevector-copy! src src.start dst dst.start count)
	dst)
    => #vu8(10 20 30))

;;; --------------------------------------------------------------------

  (check
      (let ((src	(bytevector-copy #vu8(9 10 20 30 9)))
	    (dst	(bytevector-copy #vu8(1 2 3 4 5 6 7 8 9)))
	    (src.start	1)
	    (dst.start	0)
	    (count	3))
	(bytevector-copy! src src.start dst dst.start count)
	dst)
    => #vu8(10 20 30 4 5 6 7 8 9))

  (check
      (let ((src	(bytevector-copy #vu8(9 10 20 30 9)))
	    (dst	(bytevector-copy #vu8(1 2 3 4 5 6 7 8 9)))
	    (src.start	1)
	    (dst.start	6)
	    (count	3))
	(bytevector-copy! src src.start dst dst.start count)
	dst)
    => #vu8(1 2 3 4 5 6 10 20 30))

;;; --------------------------------------------------------------------
;;; same bytevector

  (check	;non-overlapping regions
      (let ((bv		(bytevector-copy #vu8(0 1 2 3 4 5 6 7 8 9)))
	    (src.start	1)
	    (dst.start	6)
	    (count	3))
	(bytevector-copy! bv src.start bv dst.start count)
	bv)
    => #vu8(0 1 2 3 4 5 1 2 3 9))

  (check	;overlapping tail/head
      (let ((bv		(bytevector-copy #vu8(0 1 2 3 4 5 6 7 8 9)))
	    (src.start	2)
	    (dst.start	3)
	    (count	3))
	(bytevector-copy! bv src.start bv dst.start count)
	bv)
    => #vu8(0 1 2 2 3 4 6 7 8 9))

  (check	;overlapping head/tail
      (let ((bv		(bytevector-copy #vu8(0 1 2 3 4 5 6 7 8 9)))
	    (src.start	3)
	    (dst.start	2)
	    (count	3))
	(bytevector-copy! bv src.start bv dst.start count)
	bv)
    => #vu8(0 1 3 4 5 5 6 7 8 9))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-copy! #\a 0 #vu8() 0 1))
    => '(#\a))

  (check
      (catch #f
	(bytevector-copy! #vu8() 0 #\a 0 1))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: start index

  (check	;not a fixnum
      (append (catch #f (bytevector-copy! #vu8() #\a #vu8() 0 1))
	      (catch #f (bytevector-copy! #vu8() 0 #vu8() #\b 1)))
    => '(#\a #\b))

  (check	;too low
      (append (catch #f (bytevector-copy! #vu8() -1 #vu8()  0 1))
	      (catch #f (bytevector-copy! #vu8()  0 #vu8() -2 1)))
    => '(-1 -2))

  (check	;too high
      (append (catch #f (bytevector-copy! #vu8() 1 #vu8() 0 1))
	      (catch #f (bytevector-copy! #vu8() 0 #vu8() 2 1)))
    => '(1 2))

  (check	;too high
      (append (catch #f (bytevector-copy! #vu8(1 2) 10 #vu8(1 2)  0 1))
	      (catch #f (bytevector-copy! #vu8(1 2)  0 #vu8(1 2) 20 1)))
    => '(10 20))

;;; --------------------------------------------------------------------
;;; arguments validation: count

  (check	;not a fixnum
      (catch #f (bytevector-copy! #vu8() 0 #vu8() 0 #\a))
    => '(#\a))

  (check	;negative
      (catch #f (bytevector-copy! #vu8() 0 #vu8() 0 -2))
    => '(-2))

  (check	;too big for source
      (catch #f (bytevector-copy! #vu8(1 2) 0 #vu8(1 2 3) 0 3))
    => '(3))

  (check	;too big for dest
      (catch #f (bytevector-copy! #vu8(1 2 3) 0 #vu8(1 2) 0 3))
    => '(3))

  #t)


(parametrise ((check-test-name	'subbytevector-u8))

;;; argument validation, bytevector

  (check	;argument is not a bytevector
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8 "ciao" 1))
    => '("ciao"))

;;; --------------------------------------------------------------------
;;; argument validation, start index

  (check	;start index not an integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8 '#vu8() #\a))
    => '(#\a))

  (check	;start index not an exact integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8 '#vu8() 1.0))
    => '(1.0))

  (check	;start index is negative
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8 '#vu8() -1))
    => '(-1))

  (check	;start index too big
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8 '#vu8() 1))
    => '(1))

;;; --------------------------------------------------------------------
;;; argument validation, end index

  (check	;end index not an integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8 '#vu8(1) 0 #\a))
    => '(#\a))

  (check	;end index not an exact integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8 '#vu8(1) 0 1.0))
    => '(1.0))

  (check	;end index is negative
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8 '#vu8(1) 0 -1))
    => '(-1))

  (check	;end index too big
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8 '#vu8(1) 0 2))
    => '(2))

;;; --------------------------------------------------------------------

  (check
      (subbytevector-u8 '#vu8(1) 0 1)
    => '#vu8(1))

  (check
      (subbytevector-u8 '#vu8(0 1 2 3 4 5 6 7 8 9) 0 0)
    => '#vu8())

  (check
      (subbytevector-u8 '#vu8(0 1 2 3 4 5 6 7 8 9) 0 1)
    => '#vu8(0))

  (check
      (subbytevector-u8 '#vu8(0 1 2 3 4 5 6 7 8 9) 9 9)
    => '#vu8())

  (check
      (subbytevector-u8 '#vu8(0 1 2 3 4 5 6 7 8 9) 9 10)
    => '#vu8(9))

  (check
      (subbytevector-u8 '#vu8(0 1 2 3 4 5 6 7 8 9) 0 10)
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check
      (subbytevector-u8 '#vu8(0 1 2 3 4 5 6 7 8 9) 3 8)
    => '#vu8(3 4 5 6 7))

  #t)


(parametrise ((check-test-name	'subbytevector-u8/count))

;;; argument validation, bytevector

  (check	;argument is not a bytevector
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8/count "ciao" 1 1))
    => '("ciao"))

;;; --------------------------------------------------------------------
;;; argument validation, start index

  (check	;start index not an integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8/count '#vu8() #\a 1))
    => '(#\a))

  (check	;start index not an exact integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8/count '#vu8() 1.0 1))
    => '(1.0))

  (check	;start index is negative
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8/count '#vu8() -1 1))
    => '(-1))

  (check	;start index too big
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8/count '#vu8() 1 1))
    => '(1))

;;; --------------------------------------------------------------------
;;; argument validation, end index

  (check	;end index not an integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8/count '#vu8(1) 0 #\a))
    => '(#\a))

  (check	;end index not an exact integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8/count '#vu8(1) 0 1.0))
    => '(1.0))

  (check	;end index is negative
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8/count '#vu8(1) 0 -1))
    => '(-1))

  (check	;end index too big
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-u8/count '#vu8(1) 0 2))
    => '(2))

;;; --------------------------------------------------------------------

  (check
      (subbytevector-u8/count '#vu8(1) 0 1)
    => '#vu8(1))

  (check
      (subbytevector-u8/count '#vu8(0 1 2 3 4 5 6 7 8 9) 0 0)
    => '#vu8())

  (check
      (subbytevector-u8/count '#vu8(0 1 2 3 4 5 6 7 8 9) 0 1)
    => '#vu8(0))

  (check
      (subbytevector-u8/count '#vu8(0 1 2 3 4 5 6 7 8 9) 9 0)
    => '#vu8())

  (check
      (subbytevector-u8/count '#vu8(0 1 2 3 4 5 6 7 8 9) 9 1)
    => '#vu8(9))

  (check
      (subbytevector-u8/count '#vu8(0 1 2 3 4 5 6 7 8 9) 0 10)
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check
      (subbytevector-u8/count '#vu8(0 1 2 3 4 5 6 7 8 9) 3 5)
    => '#vu8(3 4 5 6 7))

  #t)


(parametrise ((check-test-name	'subbytevector-s8))

;;; argument validation, bytevector

  (check	;argument is not a bytevector
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8 "ciao" 1))
    => '("ciao"))

;;; --------------------------------------------------------------------
;;; argument validation, start index

  (check	;start index not an integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8 '#vs8() #\a))
    => '(#\a))

  (check	;start index not an exact integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8 '#vs8() 1.0))
    => '(1.0))

  (check	;start index is negative
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8 '#vs8() -1))
    => '(-1))

  (check	;start index too big
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8 '#vs8() 1))
    => '(1))

;;; --------------------------------------------------------------------
;;; argument validation, end index

  (check	;end index not an integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8 '#vs8(1) 0 #\a))
    => '(#\a))

  (check	;end index not an exact integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8 '#vs8(1) 0 1.0))
    => '(1.0))

  (check	;end index is negative
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8 '#vs8(1) 0 -1))
    => '(-1))

  (check	;end index too big
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8 '#vs8(1) 0 2))
    => '(2))

;;; --------------------------------------------------------------------

  (check
      (subbytevector-s8 '#vs8(1) 0 1)
    => '#vs8(1))

  (check
      (subbytevector-s8 '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 0 0)
    => '#vs8())

  (check
      (subbytevector-s8 '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 0 1)
    => '#vs8(0))

  (check
      (subbytevector-s8 '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 9 9)
    => '#vs8())

  (check
      (subbytevector-s8 '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 9 10)
    => '#vs8(9))

  (check
      (subbytevector-s8 '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 0 10)
    => '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9))

  (check
      (subbytevector-s8 '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 3 8)
    => '#vs8(-3 -4 -5 -6 -7))

  #t)


(parametrise ((check-test-name	'subbytevector-s8/count))

;;; argument validation, bytevector

  (check	;argument is not a bytevector
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8/count "ciao" 1 1))
    => '("ciao"))

;;; --------------------------------------------------------------------
;;; argument validation, start index

  (check	;start index not an integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8/count '#vs8() #\a 1))
    => '(#\a))

  (check	;start index not an exact integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8/count '#vs8() 1.0 1))
    => '(1.0))

  (check	;start index is negative
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8/count '#vs8() -1 1))
    => '(-1))

  (check	;start index too big
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8/count '#vs8() 1 1))
    => '(1))

;;; --------------------------------------------------------------------
;;; argument validation, end index

  (check	;end index not an integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8/count '#vs8(1) 0 #\a))
    => '(#\a))

  (check	;end index not an exact integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8/count '#vs8(1) 0 1.0))
    => '(1.0))

  (check	;end index is negative
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8/count '#vs8(1) 0 -1))
    => '(-1))

  (check	;end index too big
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(subbytevector-s8/count '#vs8(1) 0 2))
    => '(2))

;;; --------------------------------------------------------------------

  (check
      (subbytevector-s8/count '#vs8(1) 0 1)
    => '#vs8(1))

  (check
      (subbytevector-s8/count '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 0 0)
    => '#vs8())

  (check
      (subbytevector-s8/count '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 0 1)
    => '#vs8(0))

  (check
      (subbytevector-s8/count '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 9 0)
    => '#vs8())

  (check
      (subbytevector-s8/count '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 9 1)
    => '#vs8(9))

  (check
      (subbytevector-s8/count '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 0 10)
    => '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9))

  (check
      (subbytevector-s8/count '#vs8(0 1 2 -3 -4 -5 -6 -7 8 9) 3 5)
    => '#vs8(-3 -4 -5 -6 -7))

  #t)


(parametrise ((check-test-name	'bytevector-append))

;;; arguments validation

  (check
      (catch #f (bytevector-append 123))
    => '(123))

  (check
      (catch #f (bytevector-append '#vu8() 123))
    => '(123))

;;; --------------------------------------------------------------------

  (check
      (bytevector-append)
    => '#vu8())

  (check
      (bytevector-append '#vu8())
    => '#vu8())

  (check
      (bytevector-append '#vu8() '#vu8())
    => '#vu8())

  (check
      (bytevector-append '#vu8() '#vu8() '#vu8())
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-append '#vu8(1 2 3))
    => '#vu8(1 2 3))

  (check
      (bytevector-append '#vu8(1 2 3) '#vu8(4 5 6))
    => '#vu8(1 2 3 4 5 6))

  (check
      (bytevector-append '#vu8(1 2 3) '#vu8(4 5 6) '#vu8(7 8 9))
    => '#vu8(1 2 3 4 5 6 7 8 9))

  (check
      (bytevector-append '#vu8() '#vu8(4 5 6) '#vu8(7 8 9))
    => '#vu8(4 5 6 7 8 9))

  (check
      (bytevector-append '#vu8(1 2 3) '#vu8() '#vu8(7 8 9))
    => '#vu8(1 2 3 7 8 9))

  (check
      (bytevector-append '#vu8(1 2 3) '#vu8(4 5 6) '#vu8())
    => '#vu8(1 2 3 4 5 6))

  #t)


(parametrise ((check-test-name	'bytevector-u8-set-bang))

  (check
      (let ((bv (make-bytevector 3 0)))
	(bytevector-u8-set! bv 0 10)
	(bytevector-u8-set! bv 1 20)
	(bytevector-u8-set! bv 2 30)
	bv)
    => #vu8(10 20 30))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u8-set! #\a 1 2))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u8-set! #vu8(1 2 3) #\a 2))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u8-set! #vu8(1 2 3) -1 2))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u8-set! #vu8(1 2 3) 4 2))
    => '(4))

  (check	;too high
      (catch #f
	(bytevector-u8-set! #vu8(1 2 3) 3 2))
    => '(3))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-u8-set! #vu8(1 2 3) 1 #\a))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-u8-set! #vu8(1 2 3) 1 -1))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u8-set! #vu8(1 2 3) 1 256))
    => '(256))

  #t)


(parametrise ((check-test-name	'bytevector-u8-ref))

  (check
      (let ((bv #vu8(1 2 3)))
	(list (bytevector-u8-ref bv 0)
	      (bytevector-u8-ref bv 1)
	      (bytevector-u8-ref bv 2)))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u8-ref #\a 1))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u8-ref #vu8(1 2 3) #\a))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u8-ref #vu8(1 2 3) -1))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u8-ref #vu8(1 2 3) 4))
    => '(4))

  (check	;too high
      (catch #f
	(bytevector-u8-ref #vu8(1 2 3) 3))
    => '(3))

  #t)


(parametrise ((check-test-name	'bytevector-s8-set-bang))

  (check
      (let ((bv (make-bytevector 3 0)))
	(bytevector-s8-set! bv 0 10)
	(bytevector-s8-set! bv 1 20)
	(bytevector-s8-set! bv 2 30)
	bv)
    => #vs8(10 20 30))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s8-set! #\a 1 2))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s8-set! #vs8(1 2 3) #\a 2))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s8-set! #vs8(1 2 3) -1 2))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-s8-set! #vs8(1 2 3) 4 2))
    => '(4))

  (check	;too high
      (catch #f
	(bytevector-s8-set! #vs8(1 2 3) 3 2))
    => '(3))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-s8-set! #vs8(1 2 3) 1 #\a))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-s8-set! #vs8(1 2 3) 1 -129))
    => '(-129))

  (check	;too high
      (catch #f
	(bytevector-s8-set! #vs8(1 2 3) 1 128))
    => '(128))

  #t)


(parametrise ((check-test-name	'bytevector-s8-ref))

  (check
      (let ((bv #vs8(1 2 3)))
	(list (bytevector-s8-ref bv 0)
	      (bytevector-s8-ref bv 1)
	      (bytevector-s8-ref bv 2)))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s8-ref #\a 1))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s8-ref #vs8(1 2 3) #\a))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s8-ref #vs8(1 2 3) -1))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-s8-ref #vs8(1 2 3) 4))
    => '(4))

  (check	;too high
      (catch #f
	(bytevector-s8-ref #vs8(1 2 3) 3))
    => '(3))

  #t)


(parametrise ((check-test-name	'bytevector-u16-set-bang))

  (define bytes-per-word	2)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector (mult 3) 0)))
	(bytevector-u16-set! bv (mult 0) 10 (endianness little))
	(bytevector-u16-set! bv (mult 1) 20 (endianness little))
	(bytevector-u16-set! bv (mult 2) 30 (endianness little))
	bv)
    => #vu16l(10 20 30))

  (check
      (let ((bv (make-bytevector (mult 3) 0)))
	(bytevector-u16-set! bv (mult 0) 10 (endianness big))
	(bytevector-u16-set! bv (mult 1) 20 (endianness big))
	(bytevector-u16-set! bv (mult 2) 30 (endianness big))
	bv)
    => #vu16b(10 20 30))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u16-set! #\a 1 2 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u16-set! #vu16l(1 2 3) #\a 2 (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u16-set! #vu16l(1 2 3) -1 2 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u16-set! #vu16l(1 2 3) (mult 4) 2 (endianness little)))
    => (list (mult 4)))

  (check	;too high
      (catch #f
	(bytevector-u16-set! #vu16l(1 2 3) (mult 3) 2 (endianness little)))
    => (list (mult 3)))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-u16-set! #vu16l(1 2 3) 1 #\a (endianness little)))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-u16-set! #vu16l(1 2 3) 1 -1 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u16-set! #vu16l(1 2 3) 1 (+ 1 #xFFFF) (endianness little)))
    => (list (+ 1 #xFFFF)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;not a fixnum
      (catch #f
	(bytevector-u16-set! #vu16l(1 2 3) 1 0 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-u16-ref))

  (define bytes-per-word	2)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (bytevector-u16-ref #vu8(#xF0 #x0A) 0 (endianness little))
    => #x0AF0)

  (check
      (bytevector-u16-ref #vu8(#xF0 #x0A) 0 (endianness big))
    => #xF00A)

  (check
      (bytevector-u16-ref #vu8(#xF0 #x0A) 0 (native-endianness))
    => (case (native-endianness)
	 ((big)		#xF00A)
	 ((little)	#x0AF0)))

  (check
      (let ((bv #vu16l(1 2 3)))
	(list (bytevector-u16-ref bv (mult 0) (endianness little))
	      (bytevector-u16-ref bv (mult 1) (endianness little))
	      (bytevector-u16-ref bv (mult 2) (endianness little))))
    => '(1 2 3))

  (check
      (let ((bv #vu16b(1 2 3)))
	(list (bytevector-u16-ref bv (mult 0) (endianness big))
	      (bytevector-u16-ref bv (mult 1) (endianness big))
	      (bytevector-u16-ref bv (mult 2) (endianness big))))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u16-ref #\a 1 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u16-ref #vu16l(1 2 3) #\a (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u16-ref #vu16l(1 2 3) -1 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u16-ref #vu16l(1 2 3) (mult 4) (endianness little)))
    => (list (mult 4)))

  (check	;too high
      (catch #f
	(bytevector-u16-ref #vu16l(1 2 3) (mult 3) (endianness little)))
    => (list (mult 3)))

  #t)


(parametrise ((check-test-name	'list-to-bv))

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?arg ?result)
			(check (s8-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1))
    (doit '(+1 +2 +3)		#vu8(1 2 3))
    (doit '(-1)			#vu8(#xFF))
    (doit '(-1 -2 -3)		#vu8(#xFF #xFE #xFD)))

;;; --------------------------------------------------------------------
;;; 16-bit little endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?arg ?result)
  			(check (u16l-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0))
    (doit '(+1 +2 +3)		#vu8(1 0 2 0 3 0)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?arg ?result)
    			(check (s16l-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0))
    (doit '(+1 +2 +3)		#vu8(1 0 2 0 3 0))
    (doit '(-1)			#vu8(#xFF #xFF))
    (doit '(-1 -2 -3)		#vu8(#xFF #xFF #xFE #xFF #xFD #xFF)))

;;; --------------------------------------------------------------------
;;; 16-bit big endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?arg ?result)
  			(check (u16b-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 1))
    (doit '(+1 +2 +3)		#vu8(0 1 0 2 0 3)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?arg ?result)
    			(check (s16b-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 1))
    (doit '(+1 +2 +3)		#vu8(0 1 0 2 0 3))
    (doit '(-1)			#vu8(#xFF #xFF))
    (doit '(-1 -2 -3)		#vu8(#xFF #xFF #xFF #xFE #xFF #xFD)))

;;; --------------------------------------------------------------------
;;; 32-bit little endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?arg ?result)
  			(check (u32l-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0 0 0))
    (doit '(+1 +2 +3)		#vu8(1 0 0 0  2 0 0 0  3 0 0 0)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?arg ?result)
    			(check (s32l-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0 0 0))
    (doit '(+1 +2 +3)		#vu8(1 0 0 0  2 0 0 0  3 0 0 0))
    (doit '(-1)			#vu8(#xFF #xFF #xFF #xFF))
    (doit '(-1 -2 -3)		#vu8( ;;
				     #xFF #xFF #xFF #xFF
				     #xFE #xFF #xFF #xFF
				     #xFD #xFF #xFF #xFF)))

;;; --------------------------------------------------------------------
;;; 32-bit big endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?arg ?result)
  			(check (u32b-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 0 0 1))
    (doit '(+1 +2 +3)		#vu8(0 0 0 1  0 0 0 2  0 0 0 3)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?arg ?result)
    			(check (s32b-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 0 0 1))
    (doit '(+1 +2 +3)		#vu8(0 0 0 1  0 0 0 2  0 0 0 3))
    (doit '(-1)			#vu8(#xFF #xFF #xFF #xFF))
    (doit '(-1 -2 -3)		#vu8( ;;
				     #xFF #xFF #xFF #xFF
				     #xFF #xFF #xFF #xFE
				     #xFF #xFF #xFF #xFD)))

;;; --------------------------------------------------------------------
;;; 64-bit little endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?arg ?result)
  			(check (u64l-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0 0 0  0 0 0 0))
    (doit '(+1 +2 +3)		#vu8( ;;
				     1 0 0 0  0 0 0 0
				     2 0 0 0  0 0 0 0
				     3 0 0 0  0 0 0 0)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?arg ?result)
    			(check (s64l-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0 0 0  0 0 0 0))
    (doit '(+1 +2 +3)		#vu8( ;;
				     1 0 0 0  0 0 0 0
				     2 0 0 0  0 0 0 0
				     3 0 0 0  0 0 0 0))
    (doit '(-1)			#vu8(#xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFF))
    (doit '(-1 -2 -3)		#vu8( ;;
				     #xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFF
				     #xFE #xFF #xFF #xFF  #xFF #xFF #xFF #xFF
				     #xFD #xFF #xFF #xFF  #xFF #xFF #xFF #xFF)))

;;; --------------------------------------------------------------------
;;; 64-bit big endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?arg ?result)
  			(check (u64b-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 0 0 0  0 0 0 1))
    (doit '(+1 +2 +3)		#vu8( ;;
				     0 0 0 0  0 0 0 1
				     0 0 0 0  0 0 0 2
				     0 0 0 0  0 0 0 3)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?arg ?result)
    			(check (s64b-list->bytevector ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 0 0 0  0 0 0 1))
    (doit '(+1 +2 +3)		#vu8( ;;
				     0 0 0 0  0 0 0 1
				     0 0 0 0  0 0 0 2
				     0 0 0 0  0 0 0 3))
    (doit '(-1)			#vu8(#xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFF))
    (doit '(-1 -2 -3)		#vu8( ;;
				     #xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFF
				     #xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFE
				     #xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFD)))

  #t)


(parametrise ((check-test-name	'bv-to-list))

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?result ?arg)
			(check (bytevector->s8-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1))
    (doit '(+1 +2 +3)		#vu8(1 2 3))
    (doit '(-1)			#vu8(#xFF))
    (doit '(-1 -2 -3)		#vu8(#xFF #xFE #xFD)))

;;; --------------------------------------------------------------------
;;; 16-bit little endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?result ?arg)
  			(check (bytevector->u16l-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0))
    (doit '(+1 +2 +3)		#vu8(1 0 2 0 3 0)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?result ?arg)
    			(check (bytevector->s16l-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0))
    (doit '(+1 +2 +3)		#vu8(1 0 2 0 3 0))
    (doit '(-1)			#vu8(#xFF #xFF))
    (doit '(-1 -2 -3)		#vu8(#xFF #xFF #xFE #xFF #xFD #xFF)))

;;; --------------------------------------------------------------------
;;; 16-bit big endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?result ?arg)
  			(check (bytevector->u16b-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 1))
    (doit '(+1 +2 +3)		#vu8(0 1 0 2 0 3)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?result ?arg)
    			(check (bytevector->s16b-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 1))
    (doit '(+1 +2 +3)		#vu8(0 1 0 2 0 3))
    (doit '(-1)			#vu8(#xFF #xFF))
    (doit '(-1 -2 -3)		#vu8(#xFF #xFF #xFF #xFE #xFF #xFD)))

;;; --------------------------------------------------------------------
;;; 32-bit little endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?result ?arg)
  			(check (bytevector->u32l-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0 0 0))
    (doit '(+1 +2 +3)		#vu8(1 0 0 0  2 0 0 0  3 0 0 0)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?result ?arg)
    			(check (bytevector->s32l-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0 0 0))
    (doit '(+1 +2 +3)		#vu8(1 0 0 0  2 0 0 0  3 0 0 0))
    (doit '(-1)			#vu8(#xFF #xFF #xFF #xFF))
    (doit '(-1 -2 -3)		#vu8( ;;
				     #xFF #xFF #xFF #xFF
				     #xFE #xFF #xFF #xFF
				     #xFD #xFF #xFF #xFF)))

;;; --------------------------------------------------------------------
;;; 32-bit big endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?result ?arg)
  			(check (bytevector->u32b-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 0 0 1))
    (doit '(+1 +2 +3)		#vu8(0 0 0 1  0 0 0 2  0 0 0 3)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?result ?arg)
    			(check (bytevector->s32b-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 0 0 1))
    (doit '(+1 +2 +3)		#vu8(0 0 0 1  0 0 0 2  0 0 0 3))
    (doit '(-1)			#vu8(#xFF #xFF #xFF #xFF))
    (doit '(-1 -2 -3)		#vu8( ;;
				     #xFF #xFF #xFF #xFF
				     #xFF #xFF #xFF #xFE
				     #xFF #xFF #xFF #xFD)))

;;; --------------------------------------------------------------------
;;; 64-bit little endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?result ?arg)
  			(check (bytevector->u64l-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0 0 0  0 0 0 0))
    (doit '(+1 +2 +3)		#vu8( ;;
				     1 0 0 0  0 0 0 0
				     2 0 0 0  0 0 0 0
				     3 0 0 0  0 0 0 0)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?result ?arg)
    			(check (bytevector->s64l-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(1 0 0 0  0 0 0 0))
    (doit '(+1 +2 +3)		#vu8( ;;
				     1 0 0 0  0 0 0 0
				     2 0 0 0  0 0 0 0
				     3 0 0 0  0 0 0 0))
    (doit '(-1)			#vu8(#xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFF))
    (doit '(-1 -2 -3)		#vu8( ;;
				     #xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFF
				     #xFE #xFF #xFF #xFF  #xFF #xFF #xFF #xFF
				     #xFD #xFF #xFF #xFF  #xFF #xFF #xFF #xFF)))

;;; --------------------------------------------------------------------
;;; 64-bit big endian

  (let-syntax ((doit (syntax-rules ()
  		       ((_ ?result ?arg)
  			(check (bytevector->u64b-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 0 0 0  0 0 0 1))
    (doit '(+1 +2 +3)		#vu8( ;;
				     0 0 0 0  0 0 0 1
				     0 0 0 0  0 0 0 2
				     0 0 0 0  0 0 0 3)))

  (let-syntax ((doit (syntax-rules ()
    		       ((_ ?result ?arg)
    			(check (bytevector->s64b-list ?arg) => ?result)))))
    (doit '()			#vu8())
    (doit '(+1)			#vu8(0 0 0 0  0 0 0 1))
    (doit '(+1 +2 +3)		#vu8( ;;
				     0 0 0 0  0 0 0 1
				     0 0 0 0  0 0 0 2
				     0 0 0 0  0 0 0 3))
    (doit '(-1)			#vu8(#xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFF))
    (doit '(-1 -2 -3)		#vu8( ;;
				     #xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFF
				     #xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFE
				     #xFF #xFF #xFF #xFF  #xFF #xFF #xFF #xFD)))

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
