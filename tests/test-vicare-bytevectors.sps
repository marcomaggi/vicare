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
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (prefix (vicare platform words) words.)
  (vicare unsafe operations)
  ;;FIXME To be removed at the  next boot image rotation.  (Marco Maggi;
  ;;Tue Nov 26, 2013)
  (only (vicare system $bytevectors)
	$bytevector-copy)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare bytevector functions\n")


;;;; syntax helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((procedure-argument-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))

(define-syntax (with-check-for-procedure-argument-validation stx)
  (syntax-case stx ()
    ((?kwd (?who ?validation-expr) ?test0 ?test ...)
     (datum->syntax #'?kwd
		    (syntax->datum
		     #'(let-syntax ((doit (syntax-rules ()
					    ((_ ?body ?arg (... ...))
					     (check-for-procedure-argument-violation
						 ?body
					       => (quasiquote (?who (?validation-expr ?arg (... ...)))))
					     ))))
			 ?test0 ?test ...))))
    ))


;;;; helpers

(define (flonums=? a b)
  (for-all (lambda (x y)
	     (fl<? (flabs (fl- x y)) 1e-6))
    a b))

(define (cflonums=? a b)
  (for-all (lambda (x y)
	     (and (fl<? (flabs (fl- (real-part x) (real-part y))) 1e-6)
		  (fl<? (flabs (fl- (imag-part x) (imag-part y))) 1e-6)))
    a b))


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

  (with-check-for-procedure-argument-validation
      (make-bytevector (bytevector-length? bv.len))
    ;;length is not an integer
    (doit (make-bytevector #\a) #\a)
    ;;length is not an exact integer
    (doit (make-bytevector 1.0) 1.0)
    ;;length is not a fixnum
    (doit (make-bytevector (least-positive-bignum)) ,(least-positive-bignum))
    ;;length is negative
    (doit (make-bytevector -2) -2)
    (void))

;;; --------------------------------------------------------------------
;;; arguments validation: byte filler

  (with-check-for-procedure-argument-validation
      (make-bytevector (bytevector-byte-filler? fill))
    ;;filler is not a fixnum
    (doit (make-bytevector 3 #\a) #\a)
    ;;filler is too positive
    (doit (make-bytevector 2 256) 256)
    ;;filler is too negative
    (doit (make-bytevector 2 -129) -129)
    (void))

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

  (with-check-for-procedure-argument-validation
      (bytevector-fill! (bytevector? bv))
    (doit (bytevector-fill! #\a 1) #\a)
    (void))

;;; --------------------------------------------------------------------
;;; arguments validation: byte filler

  (with-check-for-procedure-argument-validation
      (bytevector-fill! (bytevector-byte-filler? fill))
    ;;filler is not a fixnum
    (doit (bytevector-fill! #vu8() #\a) #\a)
    ;;filler is too positive
    (doit (bytevector-fill! #vu8() 256) 256)
    ;;filler is too negative
    (doit (bytevector-fill! #vu8() -129) -129)
    (void))

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

  (with-check-for-procedure-argument-validation
      (bytevector-length (bytevector? bv))
    (doit (bytevector-length #\a) #\a)
    (void))

  #t)


(parametrise ((check-test-name	'bytevector-empty))

  (check
      (bytevector-empty? '#vu8())
    => #t)

  (check
      (bytevector-empty? '#vu8(1 2 3))
    => #f)

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (with-check-for-procedure-argument-validation
      (bytevector-empty? (bytevector? bv))
    (doit (bytevector-empty? #\a) #\a)
    (void))

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

  (with-check-for-procedure-argument-validation
      (bytevector=? (bytevector? bv1))
    (doit (bytevector=? #\a #vu8()) #\a)
    (void))

  (with-check-for-procedure-argument-validation
      (bytevector=? (bytevector? bv2))
    (doit (bytevector=? #vu8() #\a) #\a)
    (void))

  #t)


(parametrise ((check-test-name	'bytevector-copy-new))

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

  (with-check-for-procedure-argument-validation
      (bytevector-copy (bytevector? src.bv))
    (doit (bytevector-copy #\a) #\a)
    (void))

;;; --------------------------------------------------------------------
;;; unsafe operation

  (check
      ($bytevector-copy #vu8())
    => #vu8())

  (check
      ($bytevector-copy #vu8(1))
    => #vu8(1))

  (check
      ($bytevector-copy #vu8(1 2 3))
    => #vu8(1 2 3))

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

  (with-check-for-procedure-argument-validation
      (bytevector-copy! (bytevector? src))
    (doit (bytevector-copy! #\a 0 #vu8() 0 1) #\a))

  (with-check-for-procedure-argument-validation
      (bytevector-copy! (bytevector? dst))
    (doit (bytevector-copy! #vu8() 0 #\a 0 1) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: start index

  (with-check-for-procedure-argument-validation
      (bytevector-copy! (bytevector-index? src.start))
    ;;not a fixnum
    (doit (bytevector-copy! #vu8() #\a #vu8() 0 1) #\a)
    ;;too low
    (doit (bytevector-copy! #vu8() -1 #vu8()  0 1) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-copy! (bytevector-start-index-and-count-for-word8? src src.start byte-count))
    ;;too high
    (doit (bytevector-copy! #vu8()     1 #vu8()    0 1) #vu8()     1 1)
    (doit (bytevector-copy! #vu8(1 2) 10 #vu8(1 2) 0 1) #vu8(1 2) 10 1))

  (with-check-for-procedure-argument-validation
      (bytevector-copy! (bytevector-index? dst.start))
    ;;not a fixnum
    (doit (bytevector-copy! #vu8() 0 #vu8() #\b 1) #\b)
    ;;too low
    (doit (bytevector-copy! #vu8() 0 #vu8() -2 1) -2))

  (with-check-for-procedure-argument-validation
      (bytevector-copy! (bytevector-start-index-and-count-for-word8? dst dst.start byte-count))
    ;;too high
    (doit (bytevector-copy! #vu8(1)   0 #vu8()     2 1) #vu8()     2 1)
    (doit (bytevector-copy! #vu8(1 2) 0 #vu8(1 2) 20 1) #vu8(1 2) 20 1))

;;; --------------------------------------------------------------------
;;; arguments validation: count

  (with-check-for-procedure-argument-validation
      (bytevector-copy! (bytevector-word-count? byte-count))
    ;;not a fixnum
    (doit (bytevector-copy! #vu8() 0 #vu8() 0 #\a) #\a)
    ;;negative
    (doit (bytevector-copy! #vu8() 0 #vu8() 0 -2)  -2)
    (void))

  (with-check-for-procedure-argument-validation
      (bytevector-copy! (bytevector-start-index-and-count-for-word8? src src.start byte-count))
    ;;too big for source
    (doit (bytevector-copy! #vu8(1 2) 0 #vu8() 0 3)  #vu8(1 2) 0 3))

  (with-check-for-procedure-argument-validation
      (bytevector-copy! (bytevector-start-index-and-count-for-word8? dst dst.start byte-count))
    ;;too big for dest
    (doit (bytevector-copy! #vu8(1 2) 0 #vu8(1) 0 2)  #vu8(1) 0 2))

  #t)


(parametrise ((check-test-name	'subbytevector-u8))

;;; argument validation, bytevector

  (with-check-for-procedure-argument-validation
      (subbytevector-u8 (bytevector? src.bv))
    (doit (subbytevector-u8 "ciao" 1) "ciao"))

;;; --------------------------------------------------------------------
;;; argument validation, start index

  (with-check-for-procedure-argument-validation
      (subbytevector-u8 (bytevector-index? src.start))
    ;;start index not an integer
    (doit (subbytevector-u8 '#vu8() #\a) #\a)
    ;;start index not an exact integer
    (doit (subbytevector-u8 '#vu8() 1.0) 1.0)
    ;;start index is negative
    (doit (subbytevector-u8 '#vu8() -1) -1))

  (with-check-for-procedure-argument-validation
      (subbytevector-u8 (bytevector-start-past-indexes? src.bv src.start src.end))
    ;;start index too big
    (doit (subbytevector-u8 '#vu8() 1) #vu8() 1 0))

;;; --------------------------------------------------------------------
;;; argument validation, end index

  (with-check-for-procedure-argument-validation
      (subbytevector-u8 (bytevector-index? src.end))
    ;;end index not an integer
    (doit (subbytevector-u8 '#vu8(1) 0 #\a) #\a)
    ;;end index not an exact integer
    (doit (subbytevector-u8 '#vu8(1) 0 1.0) 1.0)
    ;;end index is negative
    (doit (subbytevector-u8 '#vu8(1) 0 -1) -1))

  (with-check-for-procedure-argument-validation
      (subbytevector-u8 (bytevector-start-past-indexes? src.bv src.start src.end))
    ;;end index too big
    (doit (subbytevector-u8 '#vu8(1) 0 2) #vu8(1) 0 2))

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

  (with-check-for-procedure-argument-validation
      (subbytevector-u8/count (bytevector? src.bv))
    ;;argument is not a bytevector
    (doit (subbytevector-u8/count "ciao" 1 1) "ciao"))

;;; --------------------------------------------------------------------
;;; argument validation, start index

  (with-check-for-procedure-argument-validation
      (subbytevector-u8/count (bytevector-index? src.start))
    ;;start index not an integer
    (doit (subbytevector-u8/count '#vu8() #\a 1) #\a)
    ;;start index not an exact integer
    (doit (subbytevector-u8/count '#vu8() 1.0 1) 1.0)
    ;;start index is negative
    (doit (subbytevector-u8/count '#vu8() -1 1) -1))

  (with-check-for-procedure-argument-validation
      (subbytevector-u8/count (bytevector-start-index-and-count-for-word8? src.bv src.start dst.len))
    ;;start index too big
    (doit (subbytevector-u8/count '#vu8() 1 1) #vu8() 1 1))

;;; --------------------------------------------------------------------
;;; argument validation, word count

  (with-check-for-procedure-argument-validation
      (subbytevector-u8/count (bytevector-length? dst.len))
    ;;word count not an integer
    (doit (subbytevector-u8/count '#vu8(1) 0 #\a) #\a)
    ;;word count not an exact integer
    (doit (subbytevector-u8/count '#vu8(1) 0 1.0) 1.0)
    ;;word count is negative
    (doit (subbytevector-u8/count '#vu8(1) 0 -1) -1))

  (with-check-for-procedure-argument-validation
      (subbytevector-u8/count (bytevector-start-index-and-count-for-word8? src.bv src.start dst.len))
    ;;end index too big
    (doit (subbytevector-u8/count '#vu8(1) 0 2) #vu8(1) 0 2))

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

  (with-check-for-procedure-argument-validation
      (subbytevector-s8 (bytevector? src.bv))
    (doit (subbytevector-s8 "ciao" 1) "ciao"))

;;; --------------------------------------------------------------------
;;; argument validation, start index

  (with-check-for-procedure-argument-validation
      (subbytevector-s8 (bytevector-index? src.start))
    ;;start index not an integer
    (doit (subbytevector-s8 '#vs8() #\a) #\a)
    ;;start index not an exact integer
    (doit (subbytevector-s8 '#vs8() 1.0) 1.0)
    ;;start index is negative
    (doit (subbytevector-s8 '#vs8() -1) -1))

  (with-check-for-procedure-argument-validation
      (subbytevector-s8 (bytevector-start-past-indexes? src.bv src.start src.end))
    ;;start index too big
    (doit (subbytevector-s8 '#vs8() 1) #vs8() 1 0))

;;; --------------------------------------------------------------------
;;; argument validation, end index

  (with-check-for-procedure-argument-validation
      (subbytevector-s8 (bytevector-index? src.end))
    ;;end index not an integer
    (doit (subbytevector-s8 '#vs8(1) 0 #\a) #\a)
    ;;end index not an exact integer
    (doit (subbytevector-s8 '#vs8(1) 0 1.0) 1.0)
    ;;end index is negative
    (doit (subbytevector-s8 '#vs8(1) 0 -1) -1))

  (with-check-for-procedure-argument-validation
      (subbytevector-s8 (bytevector-start-past-indexes? src.bv src.start src.end))
    ;;end index too big
    (doit (subbytevector-s8 '#vs8(1) 0 2) #vs8(1) 0 2))

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

  (with-check-for-procedure-argument-validation
      (subbytevector-s8/count (bytevector? src.bv))
    ;;argument is not a bytevector
    (doit (subbytevector-s8/count "ciao" 1 1) "ciao"))

;;; --------------------------------------------------------------------
;;; argument validation, start index

  (with-check-for-procedure-argument-validation
      (subbytevector-s8/count (bytevector-index? src.start))
    ;;start index not an integer
    (doit (subbytevector-s8/count '#vs8() #\a 1) #\a)
    ;;start index not an exact integer
    (doit (subbytevector-s8/count '#vs8() 1.0 1) 1.0)
    ;;start index is negative
    (doit (subbytevector-s8/count '#vs8() -1 1) -1))

  (with-check-for-procedure-argument-validation
      (subbytevector-s8/count (bytevector-start-index-and-count-for-word8? src.bv src.start dst.len))
    ;;start index too big
    (doit (subbytevector-s8/count '#vs8() 1 1) #vs8() 1 1))

;;; --------------------------------------------------------------------
;;; argument validation, word count

  (with-check-for-procedure-argument-validation
      (subbytevector-s8/count (bytevector-length? dst.len))
    ;;word count not an integer
    (doit (subbytevector-s8/count '#vs8(1) 0 #\a) #\a)
    ;;word count not an exact integer
    (doit (subbytevector-s8/count '#vs8(1) 0 1.0) 1.0)
    ;;word count is negative
    (doit (subbytevector-s8/count '#vs8(1) 0 -1) -1))

  (with-check-for-procedure-argument-validation
      (subbytevector-s8/count (bytevector-start-index-and-count-for-word8? src.bv src.start dst.len))
    ;;end index too big
    (doit (subbytevector-s8/count '#vs8(1) 0 2) #vs8(1) 0 2))

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

  (with-check-for-procedure-argument-validation
      (bytevector-append (list-of-bytevectors? list-of-bytevectors))
    (doit (bytevector-append 123) (123))
    (doit (bytevector-append '#vu8() 123) (#vu8() 123)))

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


(parametrise ((check-test-name	'reverse-and-concatenate))

;;; arguments validation

  (with-check-for-procedure-argument-validation
      (bytevector-reverse-and-concatenate (list-of-bytevectors? list-of-bytevectors))
    (doit (bytevector-reverse-and-concatenate 123) 123)
    (doit (bytevector-reverse-and-concatenate '(123)) (123)))

;;; --------------------------------------------------------------------

  (check
      (bytevector-reverse-and-concatenate '())
    => '#vu8())

  (check
      (bytevector-reverse-and-concatenate '(#vu8()))
    => '#vu8())

  (check
      (bytevector-reverse-and-concatenate '(#vu8() #vu8()))
    => '#vu8())

;;; --------------------------------------------------------------------

  (check
      (bytevector-reverse-and-concatenate '(#vu8(1 2 3)))
    => '#vu8(1 2 3))

  (check
      (bytevector-reverse-and-concatenate '(#vu8(4 5 6) #vu8(1 2 3)))
    => '#vu8(1 2 3 4 5 6))

  (check
      (bytevector-reverse-and-concatenate '(#vu8(7 8 9) #vu8(4 5 6) #vu8(1 2 3)))
    => '#vu8(1 2 3 4 5 6 7 8 9))

  #t)


(parametrise ((check-test-name	'bytevector-hash))

  (with-check-for-procedure-argument-validation
      (bytevector-hash (bytevector? bv))
    (doit (bytevector-hash 123) 123))

;;; --------------------------------------------------------------------

  (check
      (fixnum? (bytevector-hash '#vu8()))
    => #t)

  (check
      (fixnum? (bytevector-hash '#vu8(1 2 3)))
    => #t)

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

  (with-check-for-procedure-argument-validation
      (bytevector-u8-set! (bytevector? bv))
    (doit (bytevector-u8-set! #\a 1 2) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-u8-set! (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-u8-set! #vu8(1 2 3) #\a 2) #\a)
    ;;negative
    (doit (bytevector-u8-set! #vu8(1 2 3) -1 2) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-u8-set! (bytevector-index-for-word8? bv index))
    ;;too high
    (doit (bytevector-u8-set! #vu8(1 2 3) 4 2) #vu8(1 2 3) 4)
    ;;too high
    (doit (bytevector-u8-set! #vu8(1 2 3) 3 2) #vu8(1 2 3) 3))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (with-check-for-procedure-argument-validation
      (bytevector-u8-set! (words.word-u8? octet))
    ;;not a fixnum
    (doit (bytevector-u8-set! #vu8(1 2 3) 1 #\a) #\a)
    ;;too low
    (doit (bytevector-u8-set! #vu8(1 2 3) 1 (words.least-u8*)) ,(words.least-u8*))
    ;;too high
    (doit (bytevector-u8-set! #vu8(1 2 3) 1 (words.greatest-u8*)) ,(words.greatest-u8*)))

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

  (with-check-for-procedure-argument-validation
      (bytevector-u8-ref (bytevector? bv))
    (doit (bytevector-u8-ref #\a 1) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-u8-ref (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-u8-ref #vu8(1 2 3) #\a) #\a)
    ;;negative
    (doit (bytevector-u8-ref #vu8(1 2 3) -1) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-u8-ref (bytevector-index-for-word8? bv index))
    ;;too high
    (doit (bytevector-u8-ref #vu8(1 2 3) 4) #vu8(1 2 3) 4)
    ;;too high
    (doit (bytevector-u8-ref #vu8(1 2 3) 3) #vu8(1 2 3) 3))

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

  (with-check-for-procedure-argument-validation
      (bytevector-s8-set! (bytevector? bv))
    (doit (bytevector-s8-set! #\a 1 2) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-s8-set! (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-s8-set! #vs8(1 2 3) #\a 2) #\a)
    ;;negative
    (doit (bytevector-s8-set! #vs8(1 2 3) -1 2) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-s8-set! (bytevector-index-for-word8? bv index))
    ;;too high
    (doit (bytevector-s8-set! #vs8(1 2 3) 4 2) #vs8(1 2 3) 4)
    ;;too high
    (doit (bytevector-s8-set! #vs8(1 2 3) 3 2) #vs8(1 2 3) 3))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (with-check-for-procedure-argument-validation
      (bytevector-s8-set! (words.word-s8? byte))
    ;;not a fixnum
    (doit (bytevector-s8-set! #vs8(1 2 3) 1 #\a) #\a)
    ;;too low
    (doit (bytevector-s8-set! #vs8(1 2 3) 1 (words.least-s8*)) ,(words.least-s8*))
    ;;too high
    (doit (bytevector-s8-set! #vs8(1 2 3) 1 (words.greatest-s8*)) ,(words.greatest-s8*)))

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

  (with-check-for-procedure-argument-validation
      (bytevector-s8-ref (bytevector? bv))
    (doit (bytevector-s8-ref #\a 1) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-s8-ref (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-s8-ref #vs8(1 2 3) #\a) #\a)
    ;;negative
    (doit (bytevector-s8-ref #vs8(1 2 3) -1) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-s8-ref (bytevector-index-for-word8? bv index))
    ;;too high
    (doit (bytevector-s8-ref #vs8(1 2 3) 4) #vs8(1 2 3) 4)
    ;;too high
    (doit (bytevector-s8-ref #vs8(1 2 3) 3) #vs8(1 2 3) 3))

  #t)


(parametrise ((check-test-name	'bytevector-u16-set-bang))

  (define bytes-per-word	2)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u16-set! bv 0 #x0AF0 (endianness little))
	bv)
    => #vu8(#xF0 #x0A))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u16-set! bv 0 #x0AF0 (endianness big))
	bv)
    => #vu8(#x0A #xF0))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u16-set! bv 0 #x0AF0 (native-endianness))
	bv)
    => (case (native-endianness)
	 ((big)		#vu8(#x0A #xF0))
	 ((little)	#vu8(#xF0 #x0A))))

  (check
      (let ((bv (make-bytevector (mult 3) 0)))
	(bytevector-u16-set! bv (mult 0) 10 (endianness little))
	(bytevector-u16-set! bv (mult 1) 20 (endianness little))
	(bytevector-u16-set! bv (mult 2) 30 (endianness little))
	bv)
    => #vu8(10 0 20 0 30 0))

  (check
      (let ((bv (make-bytevector (mult 3) 0)))
	(bytevector-u16-set! bv (mult 0) 10 (endianness big))
	(bytevector-u16-set! bv (mult 1) 20 (endianness big))
	(bytevector-u16-set! bv (mult 2) 30 (endianness big))
	bv)
    => #vu8(0 10 0 20 0 30))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (with-check-for-procedure-argument-validation
      (bytevector-u16-set! (bytevector? bv))
    (doit (bytevector-u16-set! #\a 1 2 (endianness little)) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-u16-set! (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-u16-set! #vu8(1 0 2 0 3 0) #\a 2 (endianness little)) #\a)
    ;;negative
    (doit (bytevector-u16-set! #vu8(1 0 2 0 3 0) -1 2 (endianness little)) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-u16-set! (bytevector-index-for-word? bv index 2))
    ;;too high
    (doit (bytevector-u16-set! #vu8(1 0 2 0 3 0) (mult 4) 2 (endianness little))  #vu8(1 0 2 0 3 0) ,(mult 4) 2)
    ;;too high
    (doit (bytevector-u16-set! #vu8(1 0 2 0 3 0) (mult 3) 2 (endianness little))  #vu8(1 0 2 0 3 0) ,(mult 3) 2))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (with-check-for-procedure-argument-validation
      (bytevector-u16-set! (words.word-u16? word))
    ;;not a fixnum
    (doit (bytevector-u16-set! #vu8(1 0 2 0 3 0) 1 #\a (endianness little)) #\a)
    ;;too low
    (doit (bytevector-u16-set! #vu8(1 0 2 0 3 0) 1 (words.least-u16*) (endianness little)) ,(words.least-u16*))
    ;;too high
    (doit (bytevector-u16-set! #vu8(1 0 2 0 3 0) 1 (words.greatest-u16*) (endianness little)) ,(words.greatest-u16*)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;not a fixnum
      (catch #f
	(bytevector-u16-set! #vu8(1 0 2 0 3 0) 1 0 'dummy))
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
      (let ((bv #vu8(1 0 2 0 3 0)))
	(list (bytevector-u16-ref bv (mult 0) (endianness little))
	      (bytevector-u16-ref bv (mult 1) (endianness little))
	      (bytevector-u16-ref bv (mult 2) (endianness little))))
    => '(1 2 3))

  (check
      (let ((bv #vu8(0 1 0 2 0 3)))
	(list (bytevector-u16-ref bv (mult 0) (endianness big))
	      (bytevector-u16-ref bv (mult 1) (endianness big))
	      (bytevector-u16-ref bv (mult 2) (endianness big))))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (with-check-for-procedure-argument-validation
      (bytevector-u16-ref (bytevector? bv))
    (doit (bytevector-u16-ref #\a 1 (endianness little)) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-u16-ref (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-u16-ref #vu8(1 0 2 0 3 0) #\a (endianness little)) #\a)
    ;;negative
    (doit (bytevector-u16-ref #vu8(1 0 2 0 3 0) -1 (endianness little)) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-u16-ref (bytevector-index-for-word? bv index 2))
    ;;too high
    (doit (bytevector-u16-ref #vu8(1 0 2 0 3 0) (mult 4) (endianness little))  #vu8(1 0 2 0 3 0) ,(mult 4) 2)
    ;;too high
    (doit (bytevector-u16-ref #vu8(1 0 2 0 3 0) (mult 3) (endianness little))  #vu8(1 0 2 0 3 0) ,(mult 3) 2))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check
      (catch #f
	(bytevector-u16-ref #vu8(1 0 2 0 3 0) 0 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-u16-native-set-bang))

  (define bytes-per-word	2)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u16-native-set! bv 0 #x0AF0)
	bv)
    => (case (native-endianness)
	 ((big)		#vu8(#x0A #xF0))
	 ((little)	#vu8(#xF0 #x0A))))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (with-check-for-procedure-argument-validation
      (bytevector-u16-native-set! (bytevector? bv))
    (doit (bytevector-u16-native-set! #\a 1 2) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-u16-native-set! (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-u16-native-set! #vu8(1 0 2 0 3 0) #\a 2) #\a)
    ;;negative
    (doit (bytevector-u16-native-set! #vu8(1 0 2 0 3 0) -1 2) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-u16-native-set! (words.fixnum-aligned-to-2? index))
    ;;not aligned to 2
    (doit (bytevector-u16-native-set! #vu8(1 0 2 0 3 0) 1 0) 1))

  (with-check-for-procedure-argument-validation
      (bytevector-u16-native-set! (bytevector-index-for-word? bv index 2))
    ;;too high
    (doit (bytevector-u16-native-set! #vu8(1 0 2 0 3 0) (mult 4) 2)  #vu8(1 0 2 0 3 0) ,(mult 4) 2)
    ;;too high
    (doit (bytevector-u16-native-set! #vu8(1 0 2 0 3 0) (mult 3) 2)  #vu8(1 0 2 0 3 0) ,(mult 3) 2))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (with-check-for-procedure-argument-validation
      (bytevector-u16-native-set! (words.word-u16? word))
    ;;not a fixnum
    (doit (bytevector-u16-native-set! #vu8(1 0 2 0 3 0) 1 #\a) #\a)
    ;;too low
    (doit (bytevector-u16-native-set! #vu8(1 0 2 0 3 0) 1 (words.least-u16*)) ,(words.least-u16*))
    ;;too high
    (doit (bytevector-u16-native-set! #vu8(1 0 2 0 3 0) 1 (words.greatest-u16*)) ,(words.greatest-u16*)))

  #t)


(parametrise ((check-test-name	'bytevector-u16-native-ref))

  (define bytes-per-word	2)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (bytevector-u16-native-ref #vu8(#xF0 #x0A) 0)
    => (case (native-endianness)
	 ((big)		#xF00A)
	 ((little)	#x0AF0)))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (with-check-for-procedure-argument-validation
      (bytevector-u16-native-ref (bytevector? bv))
    (doit (bytevector-u16-native-ref #\a 1) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-u16-native-ref (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-u16-native-ref #vu8(1 0 2 0 3 0) #\a) #\a)
    ;;negative
    (doit (bytevector-u16-native-ref #vu8(1 0 2 0 3 0) -1) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-u16-native-ref (words.fixnum-aligned-to-2? index))
    ;;not aligned to 2
    (doit (bytevector-u16-native-ref #vu8(1 0 2 0 3 0) 1) 1))

  (with-check-for-procedure-argument-validation
      (bytevector-u16-native-ref (bytevector-index-for-word? bv index 2))
    ;;too high
    (doit (bytevector-u16-native-ref #vu8(1 0 2 0 3 0) (mult 4))  #vu8(1 0 2 0 3 0) ,(mult 4) 2)
    ;;too high
    (doit (bytevector-u16-native-ref #vu8(1 0 2 0 3 0) (mult 3))  #vu8(1 0 2 0 3 0) ,(mult 3) 2))

  #t)


(parametrise ((check-test-name	'bytevector-s16-set-bang))

  (define bytes-per-word	2)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-set! bv 0 #x0AF0 (endianness little))
	bv)
    => #vu8(#xF0 #x0A))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-set! bv 0 #x0AF0 (endianness big))
	bv)
    => #vu8(#x0A #xF0))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-set! bv 0 #x0AF0 (native-endianness))
	bv)
    => (case (native-endianness)
	 ((big)		#vu8(#x0A #xF0))
	 ((little)	#vu8(#xF0 #x0A))))

  (check
      (let ((bv (make-bytevector (mult 3) 0)))
	(bytevector-s16-set! bv (mult 0) 10 (endianness little))
	(bytevector-s16-set! bv (mult 1) 20 (endianness little))
	(bytevector-s16-set! bv (mult 2) 30 (endianness little))
	bv)
    => #vu8(10 0 20 0 30 0))

  (check
      (let ((bv (make-bytevector (mult 3) 0)))
	(bytevector-s16-set! bv (mult 0) 10 (endianness big))
	(bytevector-s16-set! bv (mult 1) 20 (endianness big))
	(bytevector-s16-set! bv (mult 2) 30 (endianness big))
	bv)
    => #vu8(0 10 0 20 0 30))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (bytevector? bv))
    (doit (bytevector-s16-native-set! #\a 1 2) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) #\a 2) #\a)
    ;;negative
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) -1 2) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (words.fixnum-aligned-to-2? index))
    ;;not aligned to 2
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) 1 0) 1))

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (bytevector-index-for-word? bv index 2))
    ;;too high
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) (mult 4) 2)  #vu8(1 0 2 0 3 0) ,(mult 4) 2)
    ;;too high
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) (mult 3) 2)  #vu8(1 0 2 0 3 0) ,(mult 3) 2))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (words.word-s16? word))
    ;;not a fixnum
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) 1 #\a) #\a)
    ;;too low
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) 1 (words.least-s16*)) ,(words.least-s16*))
    ;;too high
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) 1 (words.greatest-s16*)) ,(words.greatest-s16*)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check
      (catch #f
	(bytevector-s16-set! #vu8(1 0 2 0 3 0) 1 0 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-s16-ref))

  (define bytes-per-word	2)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (bytevector-s16-ref #vu8(#x0F #x0A) 0 (endianness little))
    => #x0A0F)

  (check
      (bytevector-s16-ref #vu8(#x0F #x0A) 0 (endianness big))
    => #x0F0A)

  (check
      (bytevector-s16-ref #vu8(#x0F #x0A) 0 (native-endianness))
    => (case (native-endianness)
	 ((big)		#x0F0A)
	 ((little)	#x0A0F)))

  (check
      (let ((bv #vu8(1 0 2 0 3 0)))
	(list (bytevector-s16-ref bv (mult 0) (endianness little))
	      (bytevector-s16-ref bv (mult 1) (endianness little))
	      (bytevector-s16-ref bv (mult 2) (endianness little))))
    => '(1 2 3))

  (check
      (let ((bv #vu8(0 1 0 2 0 3)))
	(list (bytevector-s16-ref bv (mult 0) (endianness big))
	      (bytevector-s16-ref bv (mult 1) (endianness big))
	      (bytevector-s16-ref bv (mult 2) (endianness big))))
    => '(1 2 3))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-set! bv 0 (words.greatest-s16) (endianness little))
	bv)
    => #vu8(#xFF 127))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-set! bv 0 (words.greatest-s16) (endianness big))
	bv)
    => #vu8(127 #xFF))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-set! bv 0 (words.least-s16) (endianness little))
	bv)
    => #vu8(0 #x80))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-set! bv 0 (words.least-s16) (endianness big))
	bv)
    => #vu8(#x80 0))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (with-check-for-procedure-argument-validation
      (bytevector-s16-ref (bytevector? bv))
    (doit (bytevector-s16-ref #\a 1 (endianness little)) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-s16-ref (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-s16-ref #vu8(1 0 2 0 3 0) #\a (endianness little)) #\a)
    ;;negative
    (doit (bytevector-s16-ref #vu8(1 0 2 0 3 0) -1 (endianness little)) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-s16-ref (bytevector-index-for-word? bv index 2))
    ;;too high
    (doit (bytevector-s16-ref #vu8(1 0 2 0 3 0) (mult 4) (endianness little))  #vu8(1 0 2 0 3 0) ,(mult 4) 2)
    ;;too high
    (doit (bytevector-s16-ref #vu8(1 0 2 0 3 0) (mult 3) (endianness little))  #vu8(1 0 2 0 3 0) ,(mult 3) 2))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check
      (catch #f
	(bytevector-s16-ref #vu8(1 0 2 0 3 0) 0 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-s16-native-set-bang))

  (define bytes-per-word	2)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-native-set! bv 0 #x0AF0)
	bv)
    => (case (native-endianness)
	 ((big)		#vu8(#x0A #xF0))
	 ((little)	#vu8(#xF0 #x0A))))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (bytevector? bv))
    (doit (bytevector-s16-native-set! #\a 1 2) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) #\a 2) #\a)
    ;;negative
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) -1 2) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (words.fixnum-aligned-to-2? index))
    ;;not aligned to 2
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) 1 0) 1))

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (bytevector-index-for-word? bv index 2))
    ;;too high
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) (mult 4) 2)  #vu8(1 0 2 0 3 0) ,(mult 4) 2)
    ;;too high
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) (mult 3) 2)  #vu8(1 0 2 0 3 0) ,(mult 3) 2))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-set! (words.word-s16? word))
    ;;not a fixnum
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) 1 #\a) #\a)
    ;;too low
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) 1 (words.least-s16*)) ,(words.least-s16*))
    ;;too high
    (doit (bytevector-s16-native-set! #vu8(1 0 2 0 3 0) 1 (words.greatest-s16*)) ,(words.greatest-s16*)))

  #t)


(parametrise ((check-test-name	'bytevector-s16-native-ref))

  (define bytes-per-word	2)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (bytevector-s16-native-ref #vu8(#x0F #x0A) 0)
    => (case (native-endianness)
	 ((big)		#x0F0A)
	 ((little)	#x0A0F)))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-native-set! bv 0 (words.greatest-s16))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#xFF 127))
	 ((big)		#vu8(127 #xFF))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s16-native-set! bv 0 (words.least-s16))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(0 #x80))
	 ((big)		#vu8(#x80 0))))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-ref (bytevector? bv))
    (doit (bytevector-s16-native-ref #\a 1) #\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-ref (bytevector-index? index))
    ;;not a fixnum
    (doit (bytevector-s16-native-ref #vu8(1 0 2 0 3 0) #\a) #\a)
    ;;negative
    (doit (bytevector-s16-native-ref #vu8(1 0 2 0 3 0) -1) -1))

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-ref (words.fixnum-aligned-to-2? index))
    ;;not aligned to 2
    (doit (bytevector-s16-native-ref #vu8(1 0 2 0 3 0) 1) 1))

  (with-check-for-procedure-argument-validation
      (bytevector-s16-native-ref (bytevector-index-for-word? bv index 2))
    ;;too high
    (doit (bytevector-s16-native-ref #vu8(1 0 2 0 3 0) (mult 4))  #vu8(1 0 2 0 3 0) ,(mult 4) 2)
    ;;too high
    (doit (bytevector-s16-native-ref #vu8(1 0 2 0 3 0) (mult 3))  #vu8(1 0 2 0 3 0) ,(mult 3) 2))

  #t)


(parametrise ((check-test-name	'bytevector-u32-set-bang))

  (define bytes-per-word	4)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u32-set! bv 0 #x12345678 (endianness little))
	bv)
    => #vu8(#x78 #x56 #x34 #x12))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u32-set! bv 0 #x12345678 (endianness big))
	bv)
    => #vu8(#x12 #x34 #x56 #x78))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u32-set! bv 0 #x12345678 (native-endianness))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#x78 #x56 #x34 #x12))
	 ((big)		#vu8(#x12 #x34 #x56 #x78))))

  (check
      (let ((bv (make-bytevector (mult 4) 0)))
	(bytevector-u32-set! bv (mult 0) 10 (endianness little))
	(bytevector-u32-set! bv (mult 1) 20 (endianness little))
	(bytevector-u32-set! bv (mult 2) 30 (endianness little))
	(bytevector-u32-set! bv (mult 3) 40 (endianness little))
	bv)
    => #vu8(10 0 0 0 20 0 0 0 30 0 0 0 40 0 0 0))

  (check
      (let ((bv (make-bytevector (mult 4) 0)))
	(bytevector-u32-set! bv (mult 0) 10 (endianness big))
	(bytevector-u32-set! bv (mult 1) 20 (endianness big))
	(bytevector-u32-set! bv (mult 2) 30 (endianness big))
	(bytevector-u32-set! bv (mult 3) 40 (endianness big))
	bv)
    => #vu8(0 0 0 10
	      0 0 0 20
	      0 0 0 30
	      0 0 0 40))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u32-set! #\a 1 2 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) #\a 2 (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) -1 2 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 5) 2 (endianness little)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-u32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 4) 2 (endianness little)))
    => (list (mult 4)))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-u32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 #\a (endianness little)))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-u32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 (words.least-u32*) (endianness little)))
    => `(,(words.least-u32*)))

  (check	;too high
      (catch #f
	(bytevector-u32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 (words.greatest-u32*) (endianness little)))
    => `(,(words.greatest-u32*)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;not a fixnum
      (catch #f
	(bytevector-u32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 0 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-u32-ref))

  (define bytes-per-word	4)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u32-ref #vu8(#x78 #x56 #x34 #x12) 0 (endianness little)))
    => #x12345678)

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u32-ref #vu8(#x12 #x34 #x56 #x78) 0 (endianness big)))
    => #x12345678)

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u32-ref (case (native-endianness)
			      ((little)	#vu8(#x78 #x56 #x34 #x12))
			      ((big)	#vu8(#x12 #x34 #x56 #x78)))
			    0 (native-endianness)))
    => #x12345678)

  (check
      (let ((bv #vu8(10 0 0 0
		     20 0 0 0
		     30 0 0 0
		     40 0 0 0)))
	(list (bytevector-u32-ref bv (mult 0) (endianness little))
	      (bytevector-u32-ref bv (mult 1) (endianness little))
	      (bytevector-u32-ref bv (mult 2) (endianness little))
	      (bytevector-u32-ref bv (mult 3) (endianness little))))
    => '(10 20 30 40))

  (check
      (let ((bv #vu8(0 0 0 10
		       0 0 0 20
		       0 0 0 30
		       0 0 0 40)))
	(list (bytevector-u32-ref bv (mult 0) (endianness big))
	      (bytevector-u32-ref bv (mult 1) (endianness big))
	      (bytevector-u32-ref bv (mult 2) (endianness big))
	      (bytevector-u32-ref bv (mult 3) (endianness big))))
    => '(10 20 30 40))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u32-ref #\a 1 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) #\a (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) -1 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 5) (endianness little)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-u32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 4) (endianness little)))
    => (list (mult 4)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;not a fixnum
      (catch #f
	(bytevector-u32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-u32-native-set-bang))

  (define bytes-per-word	4)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u32-native-set! bv 0 #x12345678)
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#x78 #x56 #x34 #x12))
	 ((big)		#vu8(#x12 #x34 #x56 #x78))))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u32-native-set! #\a 1 2))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) #\a 2))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) -2 2))
    => '(-2))

  (check	;too high
      (catch #f
	(bytevector-u32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 5) 2))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-u32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 4) 2))
    => (list (mult 4)))

  (check	;not aligned
      (catch #f
	(bytevector-u32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 3 2))
    => '(3))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-u32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 0 #\a))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-u32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 0 (words.least-u32*)))
    => `(,(words.least-u32*)))

  (check	;too high
      (catch #f
	(bytevector-u32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 0 (words.greatest-u32*)))
    => `(,(words.greatest-u32*)))

  #t)


(parametrise ((check-test-name	'bytevector-u32-native-ref))

  (define bytes-per-word	4)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u32-native-ref (case (native-endianness)
			      ((little)	#vu8(#x78 #x56 #x34 #x12))
			      ((big)	#vu8(#x12 #x34 #x56 #x78)))
			    0))
    => #x12345678)

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u32-native-ref #\a 0))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) #\a))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) -1))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 5)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-u32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 4)))
    => (list (mult 4)))

  (check	;not aligned
      (catch #f
	(bytevector-u32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 3))
    => '(3))

  #t)


(parametrise ((check-test-name	'bytevector-s32-set-bang))

  (define bytes-per-word	4)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-set! bv 0 #x12345678 (endianness little))
	bv)
    => #vu8(#x78 #x56 #x34 #x12))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-set! bv 0 #x12345678 (endianness big))
	bv)
    => #vu8(#x12 #x34 #x56 #x78))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-set! bv 0 #x12345678 (native-endianness))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#x78 #x56 #x34 #x12))
	 ((big)		#vu8(#x12 #x34 #x56 #x78))))

  (check
      (let ((bv (make-bytevector (mult 4) 0)))
	(bytevector-s32-set! bv (mult 0) 10 (endianness little))
	(bytevector-s32-set! bv (mult 1) 20 (endianness little))
	(bytevector-s32-set! bv (mult 2) 30 (endianness little))
	(bytevector-s32-set! bv (mult 3) 40 (endianness little))
	bv)
    => #vu8(10 0 0 0
	    20 0 0 0
	    30 0 0 0
	    40 0 0 0))

  (check
      (let ((bv (make-bytevector (mult 4) 0)))
	(bytevector-s32-set! bv (mult 0) 10 (endianness big))
	(bytevector-s32-set! bv (mult 1) 20 (endianness big))
	(bytevector-s32-set! bv (mult 2) 30 (endianness big))
	(bytevector-s32-set! bv (mult 3) 40 (endianness big))
	bv)
    => #vu8(0 0 0 10
	      0 0 0 20
	      0 0 0 30
	      0 0 0 40))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s32-set! #\a 1 2 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) #\a 2 (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) -1 2 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-s32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 5) 2 (endianness little)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-s32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 4) 2 (endianness little)))
    => (list (mult 4)))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-s32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 #\a (endianness little)))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-s32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 (words.least-s32*) (endianness little)))
    => `(,(words.least-s32*)))

  (check	;too high
      (catch #f
	(bytevector-s32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 (words.greatest-s32*) (endianness little)))
    => `(,(words.greatest-s32*)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;not a fixnum
      (catch #f
	(bytevector-s32-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 0 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-s32-ref))

  (define bytes-per-word	4)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-ref #vu8(#x78 #x56 #x34 #x12) 0 (endianness little)))
    => #x12345678)

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-ref #vu8(#x12 #x34 #x56 #x78) 0 (endianness big)))
    => #x12345678)

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-ref (case (native-endianness)
			      ((little)	#vu8(#x78 #x56 #x34 #x12))
			      ((big)	#vu8(#x12 #x34 #x56 #x78)))
			    0 (native-endianness)))
    => #x12345678)

  (check
      (let ((bv #vu8(10 0 0 0 20 0 0 0 30 0 0 0 40 0 0 0)))
	(list (bytevector-s32-ref bv (mult 0) (endianness little))
	      (bytevector-s32-ref bv (mult 1) (endianness little))
	      (bytevector-s32-ref bv (mult 2) (endianness little))
	      (bytevector-s32-ref bv (mult 3) (endianness little))))
    => '(10 20 30 40))

  (check
      (let ((bv #vu8(0 0 0 10 0 0 0 20 0 0 0 30 0 0 0 40 0 0 0)))
	(list (bytevector-s32-ref bv (mult 0) (endianness big))
	      (bytevector-s32-ref bv (mult 1) (endianness big))
	      (bytevector-s32-ref bv (mult 2) (endianness big))
	      (bytevector-s32-ref bv (mult 3) (endianness big))))
    => '(10 20 30 40))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-set! bv 0 (words.greatest-s32) (endianness little))
	bv)
    => #vu8(#xFF #xFF #xFF 127))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-set! bv 0 (words.greatest-s32) (endianness big))
	bv)
    => #vu8(127 #xFF #xFF #xFF))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-set! bv 0 (words.least-s32) (endianness little))
	bv)
    => #vu8(0 0 0 #x80))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-set! bv 0 (words.least-s32) (endianness big))
	bv)
    => #vu8(#x80 0 0 0))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s32-ref #\a 1 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) #\a (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) -1 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-s32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 5) (endianness little)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-s32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 4) (endianness little)))
    => (list (mult 4)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;not a fixnum
      (catch #f
	(bytevector-s32-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 1 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-s32-native-set-bang))

  (define bytes-per-word	4)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-native-set! bv 0 #x12345678)
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#x78 #x56 #x34 #x12))
	 ((big)		#vu8(#x12 #x34 #x56 #x78))))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s32-native-set! #\a 0 2))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) #\a 2))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) -2 2))
    => '(-2))

  (check	;too high
      (catch #f
	(bytevector-s32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 5) 2))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-s32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 4) 2))
    => (list (mult 4)))

  (check	;not aligned
      (catch #f
	(bytevector-s32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 3 2))
    => '(3))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-s32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 0 #\a))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-s32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 0 (words.least-s32*)))
    => `(,(words.least-s32*)))

  (check	;too high
      (catch #f
	(bytevector-s32-native-set! #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 0 (words.greatest-s32*)))
    => `(,(words.greatest-s32*)))

  #t)


(parametrise ((check-test-name	'bytevector-s32-native-ref))

  (define bytes-per-word	4)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-native-ref (case (native-endianness)
				     ((little)	#vu8(#x78 #x56 #x34 #x12))
				     ((big)	#vu8(#x12 #x34 #x56 #x78)))
				   0))
    => #x12345678)

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-native-set! bv 0 (words.greatest-s32))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#xFF #xFF #xFF 127))
	 ((big)		#vu8(127 #xFF #xFF #xFF))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s32-native-set! bv 0 (words.least-s32))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(0 0 0 #x80))
	 ((big)		#vu8(#x80 0 0 0))))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s32-native-ref #\a 0))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) #\a))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) -2))
    => '(-2))

  (check	;too high
      (catch #f
	(bytevector-s32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 5)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-s32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) (mult 4)))
    => (list (mult 4)))

  (check	;not aligned
      (catch #f
	(bytevector-s32-native-ref #vu8(1 0 0 0 2 0 0 0 3 0 0 0) 3))
    => '(3))

  #t)


(parametrise ((check-test-name	'bytevector-u64-set-bang))

  (define bytes-per-word	8)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (define the-bv
    #vu8( ;;
	 0 0 0 0   0 0 0 10
	 0 0 0 0   0 0 0 20
	 0 0 0 0   0 0 0 30
	 0 0 0 0   0 0 0 40))

  (check	;zero
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-set! bv 0 0 (endianness little))
	bv)
    => #vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

  (check	;fixnum
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-set! bv 0 #xFF (endianness little))
	bv)
    => #vu8(#xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00))

  (check	;fixnum
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-set! bv 0 #xFF (endianness big))
	bv)
    => #vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF))

  (check	;recognisable u64
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-set! bv 0 #x0102030405060708 (endianness little))
	bv)
    => #vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))

  (check	;recognisable u64
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-set! bv 0 #x0102030405060708 (endianness big))
	bv)
    => #vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08))

  (check	;recognisable u64
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-set! bv 0 #x0102030405060708 (native-endianness))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))
	 ((big)		#vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08))))

  (check	;greatest u64
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-set! bv 0 (words.greatest-u64) (endianness big))
	bv)
    => #vu8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))

  (check	;greatest u64
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-set! bv 0 (words.greatest-u64) (endianness little))
	bv)
    => #vu8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))

  (check	;greatest u64
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-set! bv 0 (words.greatest-u64) (native-endianness))
	bv)
    => #vu8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))

;;; --------------------------------------------------------------------

  (check
      (let ((bv (make-bytevector (mult 4) 0)))
	(bytevector-u64-set! bv (mult 0) 10 (endianness little))
	(bytevector-u64-set! bv (mult 1) 20 (endianness little))
	(bytevector-u64-set! bv (mult 2) 30 (endianness little))
	(bytevector-u64-set! bv (mult 3) 40 (endianness little))
	bv)
    => #vu8( ;;
	    10 0 0 0   0 0 0 0
	    20 0 0 0   0 0 0 0
	    30 0 0 0   0 0 0 0
	    40 0 0 0   0 0 0 0))

  (check
      (let ((bv (make-bytevector (mult 4) 0)))
	(bytevector-u64-set! bv (mult 0) 10 (endianness big))
	(bytevector-u64-set! bv (mult 1) 20 (endianness big))
	(bytevector-u64-set! bv (mult 2) 30 (endianness big))
	(bytevector-u64-set! bv (mult 3) 40 (endianness big))
	bv)
    => #vu8( ;;
	    0 0 0 0   0 0 0 10
	    0 0 0 0   0 0 0 20
	    0 0 0 0   0 0 0 30
	    0 0 0 0   0 0 0 40))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u64-set! #\a 1 2 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u64-set! the-bv #\a 2 (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u64-set! the-bv -1 2 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u64-set! the-bv (mult 5) 2 (endianness little)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-u64-set! the-bv (mult 4) 2 (endianness little)))
    => (list (mult 4)))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-u64-set! the-bv 1 #\a (endianness little)))
    => '(#\a))

  (check	;negative fixnum
      (catch #f
	(bytevector-u64-set! the-bv 1 -1 (endianness little)))
    => '(-1))

  (check	;negative bignum
      (catch #f
	(bytevector-u64-set! the-bv 1 (words.least-u64*) (endianness little)))
    => `(,(words.least-u64*)))

  (check 	;too high
      (catch #f
	(bytevector-u64-set! the-bv 1 (words.greatest-u64*) (endianness little)))
    => `(,(words.greatest-u64*)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;invalid endianness symbol
      (catch #f
	(bytevector-u64-set! the-bv 1 0 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-u64-ref))

  (define bytes-per-word	8)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (define the-bv-be
    #vu8( ;;
	 0 0 0 0   0 0 0 10
	 0 0 0 0   0 0 0 20
	 0 0 0 0   0 0 0 30
	 0 0 0 0   0 0 0 40))

  (define the-bv-le
    #vu8( ;;
	 10 0 0 0   0 0 0 0
	 20 0 0 0   0 0 0 0
	 30 0 0 0   0 0 0 0
	 40 0 0 0   0 0 0 0))

;;; --------------------------------------------------------------------

  (check
      (bytevector-u64-ref #vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) 0 (endianness little))
    =>  #x0102030405060708)

  (check
      (bytevector-u64-ref #vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08) 0 (endianness big))
    => #x0102030405060708)

  (check
      (bytevector-u64-ref (case (native-endianness)
			    ((little)	#vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))
			    ((big)	#vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08)))
			  0 (native-endianness))
    => #x0102030405060708)

  (check
      (list (bytevector-u64-ref the-bv-le (mult 0) (endianness little))
	    (bytevector-u64-ref the-bv-le (mult 1) (endianness little))
	    (bytevector-u64-ref the-bv-le (mult 2) (endianness little))
	    (bytevector-u64-ref the-bv-le (mult 3) (endianness little)))
    => '(10 20 30 40))

  (check
      (list (bytevector-u64-ref the-bv-be (mult 0) (endianness big))
	    (bytevector-u64-ref the-bv-be (mult 1) (endianness big))
	    (bytevector-u64-ref the-bv-be (mult 2) (endianness big))
	    (bytevector-u64-ref the-bv-be (mult 3) (endianness big)))
    => '(10 20 30 40))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u64-ref #\a 1 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u64-ref the-bv-le #\a (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u64-ref the-bv-le -1 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-u64-ref the-bv-le (mult 5) (endianness little)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-u64-ref the-bv-le (mult 4) (endianness little)))
    => (list (mult 4)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;not a fixnum
      (catch #f
	(bytevector-u64-ref the-bv-le 1 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-u64-native-set-bang))

  (define bytes-per-word	8)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (define the-bv
    #vu8( ;;
	 0 0 0 0   0 0 0 10
	 0 0 0 0   0 0 0 20
	 0 0 0 0   0 0 0 30
	 0 0 0 0   0 0 0 40))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-u64-native-set! bv 0 #x0102030405060708)
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))
	 ((big)		#vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08))))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u64-native-set! #\a 0 2))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u64-native-set! the-bv #\a 2))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u64-native-set! the-bv -8 2))
    => '(-8))

  (check	;too high
      (catch #f
	(bytevector-u64-native-set! the-bv (mult 5) 2))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-u64-native-set! the-bv (mult 4) 2))
    => (list (mult 4)))

  (check	;not aligned
      (catch #f
	(bytevector-u64-native-set! the-bv 3 2))
    => '(3))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-u64-native-set! the-bv 0 #\a))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-u64-native-set! the-bv 0 (words.least-u64*)))
    => `(,(words.least-u64*)))

  (check	;too high
      (catch #f
	(bytevector-u64-native-set! the-bv 0 (words.greatest-u64*)))
    => `(,(words.greatest-u64*)))

  #t)


(parametrise ((check-test-name	'bytevector-u64-native-ref))

  (define bytes-per-word	8)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (define the-bv-be
    #vu8( ;;
	 0 0 0 0   0 0 0 10
	 0 0 0 0   0 0 0 20
	 0 0 0 0   0 0 0 30
	 0 0 0 0   0 0 0 40))

  (define the-bv-le
    #vu8( ;;
	 10 0 0 0   0 0 0 0
	 20 0 0 0   0 0 0 0
	 30 0 0 0   0 0 0 0
	 40 0 0 0   0 0 0 0))

;;; --------------------------------------------------------------------

  (check
      (bytevector-u64-native-ref (case (native-endianness)
				   ((little)	#vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))
				   ((big)	#vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08)))
				 0)
    => #x0102030405060708)

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-u64-native-ref #\a 0))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-u64-native-ref the-bv-le #\a))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-u64-native-ref the-bv-le -2))
    => '(-2))

  (check	;too high
      (catch #f
	(bytevector-u64-native-ref the-bv-le (mult 5)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-u64-native-ref the-bv-le (mult 4)))
    => (list (mult 4)))

  (check	;not aligned
      (catch #f
	(bytevector-u64-native-ref the-bv-le 3))
    => '(3))

  #t)


(parametrise ((check-test-name	'bytevector-s64-set-bang))

  (define bytes-per-word	8)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (define the-bv-le
    #vu8( ;;
	 10 0 0 0   0 0 0 0
	 20 0 0 0   0 0 0 0
	 30 0 0 0   0 0 0 0
	 40 0 0 0   0 0 0 0))

  (define the-bv-be
    #vu8( ;;
	 0 0 0 0   0 0 0 10
	 0 0 0 0   0 0 0 20
	 0 0 0 0   0 0 0 30
	 0 0 0 0   0 0 0 40))

;;; --------------------------------------------------------------------

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-set! bv 0 #x0102030405060708 (endianness little))
	bv)
    => #vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-set! bv 0 #x0102030405060708 (endianness big))
	bv)
    => #vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-set! bv 0 #x0102030405060708 (native-endianness))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))
	 ((big)		#vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08))))

  (check
      (let ((bv (make-bytevector (mult 4) 0)))
	(bytevector-s64-set! bv (mult 0) 10 (endianness little))
	(bytevector-s64-set! bv (mult 1) 20 (endianness little))
	(bytevector-s64-set! bv (mult 2) 30 (endianness little))
	(bytevector-s64-set! bv (mult 3) 40 (endianness little))
	bv)
    => the-bv-le)

  (check
      (let ((bv (make-bytevector (mult 4) 0)))
	(bytevector-s64-set! bv (mult 0) 10 (endianness big))
	(bytevector-s64-set! bv (mult 1) 20 (endianness big))
	(bytevector-s64-set! bv (mult 2) 30 (endianness big))
	(bytevector-s64-set! bv (mult 3) 40 (endianness big))
	bv)
    => the-bv-be)

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-set! bv 0 (words.greatest-s64) (endianness little))
	bv)
    => #vu8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF 127))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-set! bv 0 (words.greatest-s64) (endianness big))
	bv)
    => #vu8(127 #xFF #xFF #xFF #xFF #xFF #xFF #xFF))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-set! bv 0 (words.least-s64) (endianness little))
	bv)
    => #vu8(0 0 0 0 0 0 0 #x80))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-set! bv 0 (words.least-s64) (endianness big))
	bv)
    => #vu8(#x80 0 0 0 0 0 0 0))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s64-set! #\a 1 2 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s64-set! the-bv-le #\a 2 (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s64-set! the-bv-le -1 2 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-s64-set! the-bv-le (mult 5) 2 (endianness little)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-s64-set! the-bv-le (mult 4) 2 (endianness little)))
    => (list (mult 4)))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-s64-set! the-bv-le 1 #\a (endianness little)))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-s64-set! the-bv-le 1 (words.least-s64*) (endianness little)))
    => `(,(words.least-s64*)))

  (check	;too high
      (catch #f
	(bytevector-s64-set! the-bv-le 1 (words.greatest-s64*) (endianness little)))
    => `(,(words.greatest-s64*)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;not a fixnum
      (catch #f
	(bytevector-s64-set! the-bv-le 1 0 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-s64-ref))

  (define bytes-per-word	8)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (define the-bv-be
    #vu8( ;;
	 0 0 0 0   0 0 0 10
	 0 0 0 0   0 0 0 20
	 0 0 0 0   0 0 0 30
	 0 0 0 0   0 0 0 40))

  (define the-bv-le
    #vu8( ;;
	 10 0 0 0   0 0 0 0
	 20 0 0 0   0 0 0 0
	 30 0 0 0   0 0 0 0
	 40 0 0 0   0 0 0 0))

;;; --------------------------------------------------------------------

  (check
      (bytevector-s64-ref #vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) 0 (endianness little))
    =>  #x0102030405060708)

  (check
      (bytevector-s64-ref #vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08) 0 (endianness big))
    => #x0102030405060708)

  (check
      (bytevector-s64-ref (case (native-endianness)
			    ((little)	#vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))
			    ((big)	#vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08)))
			  0 (native-endianness))
    => #x0102030405060708)

  (check
      (list (bytevector-s64-ref the-bv-le (mult 0) (endianness little))
	    (bytevector-s64-ref the-bv-le (mult 1) (endianness little))
	    (bytevector-s64-ref the-bv-le (mult 2) (endianness little))
	    (bytevector-s64-ref the-bv-le (mult 3) (endianness little)))
    => '(10 20 30 40))

  (check
      (list (bytevector-s64-ref the-bv-be (mult 0) (endianness big))
	    (bytevector-s64-ref the-bv-be (mult 1) (endianness big))
	    (bytevector-s64-ref the-bv-be (mult 2) (endianness big))
	    (bytevector-s64-ref the-bv-be (mult 3) (endianness big)))
    => '(10 20 30 40))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s64-ref #\a 1 (endianness little)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s64-ref the-bv-le #\a (endianness little)))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s64-ref the-bv-le -1 (endianness little)))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-s64-ref the-bv-le (mult 5) (endianness little)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-s64-ref the-bv-le (mult 4) (endianness little)))
    => (list (mult 4)))

;;; --------------------------------------------------------------------
;;; arguments validation: endianness

  (check	;not a fixnum
      (catch #f
	(bytevector-s64-ref the-bv-le 1 'dummy))
    => '(dummy))

  #t)


(parametrise ((check-test-name	'bytevector-s64-native-set-bang))

  (define bytes-per-word	8)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (define the-bv-le
    #vu8( ;;
	 10 0 0 0   0 0 0 0
	 20 0 0 0   0 0 0 0
	 30 0 0 0   0 0 0 0
	 40 0 0 0   0 0 0 0))

  (define the-bv-be
    #vu8( ;;
	 0 0 0 0   0 0 0 10
	 0 0 0 0   0 0 0 20
	 0 0 0 0   0 0 0 30
	 0 0 0 0   0 0 0 40))

;;; --------------------------------------------------------------------

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-native-set! bv 0 #x0102030405060708)
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))
	 ((big)		#vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-native-set! bv 0 (words.greatest-s64))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(#xFF #xFF #xFF #xFF #xFF #xFF #xFF 127))
	 ((big)		#vu8(127 #xFF #xFF #xFF #xFF #xFF #xFF #xFF))))

  (check
      (let ((bv (make-bytevector bytes-per-word)))
	(bytevector-s64-native-set! bv 0 (words.least-s64))
	bv)
    => (case (native-endianness)
	 ((little)	#vu8(0 0 0 0 0 0 0 #x80))
	 ((big)		#vu8(#x80 0 0 0 0 0 0 0))))

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s64-native-set! #\a 0 2))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s64-native-set! the-bv-le #\a 2))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s64-native-set! the-bv-le -8 2))
    => '(-8))

  (check	;too high
      (catch #f
	(bytevector-s64-native-set! the-bv-le (mult 5) 2))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-s64-native-set! the-bv-le (mult 4) 2))
    => (list (mult 4)))

  (check	;not aligned
      (catch #f
	(bytevector-s64-native-set! the-bv-le 3 2))
    => '(3))

;;; --------------------------------------------------------------------
;;; arguments validation: value

  (check	;not a fixnum
      (catch #f
	(bytevector-s64-native-set! the-bv-le 0 #\a))
    => '(#\a))

  (check	;too low
      (catch #f
	(bytevector-s64-native-set! the-bv-le 0 (words.least-s64*)))
    => `(,(words.least-s64*)))

  (check	;too high
      (catch #f
	(bytevector-s64-native-set! the-bv-le 0 (words.greatest-s64*)))
    => `(,(words.greatest-s64*)))

  #t)


(parametrise ((check-test-name	'bytevector-s64-native-ref))

  (define bytes-per-word	8)
  (define-syntax mult
    (syntax-rules ()
      ((_ ?num)
       (* bytes-per-word ?num))))

  (define the-bv-be
    #vu8( ;;
	 0 0 0 0   0 0 0 10
	 0 0 0 0   0 0 0 20
	 0 0 0 0   0 0 0 30
	 0 0 0 0   0 0 0 40))

  (define the-bv-le
    #vu8( ;;
	 10 0 0 0   0 0 0 0
	 20 0 0 0   0 0 0 0
	 30 0 0 0   0 0 0 0
	 40 0 0 0   0 0 0 0))

;;; --------------------------------------------------------------------

  (check
      (bytevector-s64-native-ref (case (native-endianness)
				   ((little)	#vu8(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))
				   ((big)	#vu8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08)))
				 0)
    => #x0102030405060708)

;;; --------------------------------------------------------------------
;;; arguments validation: bytevector

  (check
      (catch #f
	(bytevector-s64-native-ref #\a 0))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check	;not a fixnum
      (catch #f
	(bytevector-s64-native-ref the-bv-le #\a))
    => '(#\a))

  (check	;negative
      (catch #f
	(bytevector-s64-native-ref the-bv-le -1))
    => '(-1))

  (check	;too high
      (catch #f
	(bytevector-s64-native-ref the-bv-le (mult 5)))
    => (list (mult 5)))

  (check	;too high
      (catch #f
	(bytevector-s64-native-ref the-bv-le (mult 4)))
    => (list (mult 4)))

  (check	;not aligned
      (catch #f
	(bytevector-s64-native-ref the-bv-le 3))
    => '(3))

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

;;; --------------------------------------------------------------------
;;; single precision flonums

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->f4l-list (f4l-list->bytevector ?ell))
			  (=> flonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2))
    (doit '(1.2 3.4))
    (doit '(1.2 3.4 5.6))
    (doit '(1.2 -3.4 5.6))
    #f)

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->f4b-list (f4b-list->bytevector ?ell))
			  (=> flonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2))
    (doit '(1.2 3.4))
    (doit '(1.2 3.4 5.6))
    (doit '(1.2 -3.4 5.6))
    #f)

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->f4n-list (f4n-list->bytevector ?ell))
			  (=> flonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2))
    (doit '(1.2 3.4))
    (doit '(1.2 3.4 5.6))
    (doit '(1.2 -3.4 5.6))
    #f)

;;; --------------------------------------------------------------------
;;; double precision flonums

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->f8l-list (f8l-list->bytevector ?ell))
			  (=> flonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2))
    (doit '(1.2 3.4))
    (doit '(1.2 3.4 5.6))
    (doit '(1.2 -3.4 5.6))
    #f)

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->f8b-list (f8b-list->bytevector ?ell))
			  (=> flonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2))
    (doit '(1.2 3.4))
    (doit '(1.2 3.4 5.6))
    (doit '(1.2 -3.4 5.6))
    #f)

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->f8n-list (f8n-list->bytevector ?ell))
			  (=> flonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2))
    (doit '(1.2 3.4))
    (doit '(1.2 3.4 5.6))
    (doit '(1.2 -3.4 5.6))
    #f)

;;; --------------------------------------------------------------------
;;; single precision cflonums

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->c4l-list (c4l-list->bytevector ?ell))
			  (=> cflonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2+3.4i))
    (doit '(1.2+3.4i 5.6+7.8i))
    #f)

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->c4b-list (c4b-list->bytevector ?ell))
			  (=> cflonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2+3.4i))
    (doit '(1.2+3.4i 5.6+7.8i))
    #f)

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->c4n-list (c4n-list->bytevector ?ell))
			  (=> cflonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2+3.4i))
    (doit '(1.2+3.4i 5.6+7.8i))
    #f)

;;; --------------------------------------------------------------------
;;; double precision cflonums

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->c8l-list (c8l-list->bytevector ?ell))
			  (=> cflonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2+3.4i))
    (doit '(1.2+3.4i 5.6+7.8i))
    #f)

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->c8b-list (c8b-list->bytevector ?ell))
			  (=> cflonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2+3.4i))
    (doit '(1.2+3.4i 5.6+7.8i))
    #f)

  (let-syntax ((doit (syntax-rules ()
		       ((_ ?ell)
			(check
			    (bytevector->c8n-list (c8n-list->bytevector ?ell))
			  (=> cflonums=?)
			  ?ell)))))
    (doit '())
    (doit '(1.2+3.4i))
    (doit '(1.2+3.4i 5.6+7.8i))
    #f)

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
;;eval: (put 'with-check-for-procedure-argument-validation 'scheme-indent-function 1)
;;End:
