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
      (guard (E ((assertion-violation? E)
		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-bytevector #\a))
    => '(#\a))

  (check	;length is not an exact integer
      (guard (E ((assertion-violation? E)
		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-bytevector 1.0))
    => '(1.0))

  (check	;length is not a fixnum
      (guard (E ((assertion-violation? E)
		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-bytevector BIGNUM))
    => (list BIGNUM))

  (check	;length is negative
      (guard (E ((assertion-violation? E)
		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-bytevector -2))
    => '(-2))

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


(parametrise ((check-test-name	'append))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(bytevector-append 123))
    => '(123))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(bytevector-append '#vu8() 123))
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
