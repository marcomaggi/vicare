;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for rounding flonums
;;;Date: Wed Jan 14, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: format rounding routines\n")

;;; --------------------------------------------------------------------

(define zero-char-integer	(char->integer #\0))
(define mantissa-max-length	400)
(define mantissa-buffer		#f)
(define mantissa-length		#f)
(define mantissa-dot-index	#f)

(define-syntax increment!
  (syntax-rules ()
    ((_ ?varname ?step)
     (set! ?varname (+ ?varname ?step)))
    ((_ ?varname)
     (set! ?varname (+ ?varname 1)))))

;;; --------------------------------------------------------------------

(define-syntax mantissa-set!
  (syntax-rules ()
    ((_ ?idx ?char)
     (string-set! mantissa-buffer ?idx ?char))))

(define-syntax mantissa-ref
  (syntax-rules ()
    ((_ ?idx)
     (string-ref mantissa-buffer ?idx))))

(define-syntax mantissa-digit-set!
  (syntax-rules ()
    ((_ ?idx ?digit)
     (mantissa-set! ?idx (integer->char (+ ?digit zero-char-integer))))))

(define-syntax mantissa-digit-ref
  (syntax-rules ()
    ((_ ?idx)
     (- (char->integer (mantissa-ref ?idx)) zero-char-integer))))

;;; --------------------------------------------------------------------

(define (mantissa-zfill left? n)
  (when (> (+ n mantissa-length) mantissa-max-length) ; from the left or right
    (error 'mantissa-zfill
      "number is too long to format (enlarge mantissa-max-length)"))
  (increment! mantissa-length n)
  (if left?
      (do ((i mantissa-length (- i 1))) ; fill n 0s to left
	  ((< i 0))
	(mantissa-set! i (if (< i n)
			     #\0
			   (mantissa-ref (- i n)))))
    (do ((i (- mantissa-length n) (+ i 1))) ; fill n 0s to the right
	((= i mantissa-length))
      (mantissa-set! i #\0))))

(define-syntax mantissa-prepend-zeros
  (syntax-rules ()
    ((_ ?number-of-zeros)
     (mantissa-zfill #t ?number-of-zeros))))

(define-syntax mantissa-append-zeros
  (syntax-rules ()
    ((_ ?number-of-zeros)
     (mantissa-zfill #f ?number-of-zeros))))




(define (mantissa-round-digits-after-dot number-of-digits)

  ;;Think of the mantissa buffer like this:
  ;;
  ;; I = integer digits		F = fractional digits
  ;; X = rounded digit		T = truncated digits
  ;;
  ;;     IIIIIIIIIIIIIIFFFFFFFFFFFXTTTTTTTTTTTTTTTTTTT
  ;;     ^             ^          ^^
  ;;     |             |          ||
  ;; index zero    index of dot   | --- index of first
  ;;                              |     truncated digit
  ;;                              |
  ;;                          index of
  ;;                          rounded digit

  (define (compute-rounded-digit-with-carry digit first-truncated-digit-idx)
    (let ((rounded (if (= first-truncated-digit-idx mantissa-length)
		       digit
		     (let ((d (mantissa-ref first-truncated-digit-idx)))
		       (cond ((char>? #\5 d)	digit)
			     ((char<? #\5 d)	(+ 1 digit))
			     (else
			      ;;Here D  is #\5, so  we scan the  rest of
			      ;;the  mantissa  buffer:   if  we  find  a
			      ;;non-zero char, we  round up; if we reach
			      ;;the end of the buffer we round to even.
			      (let loop ((i (+ 1 first-truncated-digit-idx)))
				(cond
				 ((= i mantissa-length)
				  (if (even? digit)
				      digit
				    (+ 1 digit)))
				 ((char=? #\0 (mantissa-ref i))
				  (loop (+ 1 i)))
				 (else
				  (+ 1 digit))))))))))
      (if (> 10 rounded)
	  (values rounded #f)
	(values 0 #t))))

  (define (propagate-carry idx)
    (let ((carry #t))
      (do ((i idx (- i 1)))
	  ((or (not carry) (< i 0))
	   (when carry
	     ;;This  body prepends  a  "1" to  the  mantissa buffer  and
	     ;;increments the  dot position.   This way it  performs the
	     ;;carry normalisation for the roundings like:
	     ;;
	     ;;	"9.9" -> "10.0"
	     ;;
	     (mantissa-prepend-zeros 1)
	     (mantissa-set! 0 #\1)
	     (increment! mantissa-dot-index)))
	(let ((digit (+ 1 (mantissa-digit-ref i))))
	  (set! carry (>= digit 10))
	  (mantissa-digit-set! i (if carry (- digit 10) digit))))))

  (when (and (= 0 mantissa-dot-index)
	     (= 0 number-of-digits))
    (mantissa-prepend-zeros 1)
    (increment! mantissa-dot-index))
  (let* ((i (+ mantissa-dot-index number-of-digits -1))
	 (j (+ 1 i)))
    (unless (= i mantissa-length)
      (receive (rounded-digit carry)
	  (compute-rounded-digit-with-carry (mantissa-digit-ref i) j)
	(mantissa-digit-set! i rounded-digit) ;;store the rounded digit
	(set! mantissa-length j)	      ;;truncate the tail digits
	(when carry (propagate-carry (- i 1)))))))




;;;; the round test

(define-syntax round-digits-after-dot
  (syntax-rules ()
    ((_ ?fn-str ?fn-len ?fn-dot ?digits)
     (begin
       (set! mantissa-buffer	(string-copy ?fn-str))
       (set! mantissa-length	?fn-len)
       (set! mantissa-dot-index	?fn-dot)
       (mantissa-round-digits-after-dot ?digits)
       (list (substring mantissa-buffer 0 mantissa-length)
	     mantissa-length mantissa-dot-index)))))


;;;; no rounding, just moving the dot position

(check (round-digits-after-dot "1234xxx" 4 0 4) => '("1234" 4 0))
(check (round-digits-after-dot "1234xxx" 4 1 3) => '("1234" 4 1))
(check (round-digits-after-dot "1234xxx" 4 2 2) => '("1234" 4 2))
(check (round-digits-after-dot "1234xxx" 4 3 1) => '("1234" 4 3))
(check (round-digits-after-dot "1234xxx" 4 4 0) => '("1234" 4 4))



;;;; rounding one digit, dot offset 0

(check (round-digits-after-dot "1230xxx" 4 0 3) => '("123" 3 0))
(check (round-digits-after-dot "1231xxx" 4 0 3) => '("123" 3 0))
(check (round-digits-after-dot "1232xxx" 4 0 3) => '("123" 3 0))
(check (round-digits-after-dot "1233xxx" 4 0 3) => '("123" 3 0))
(check (round-digits-after-dot "1234xxx" 4 0 3) => '("123" 3 0))
(check (round-digits-after-dot "1235xxx" 4 0 3) => '("124" 3 0)) ;; rounding to even
(check (round-digits-after-dot "1236xxx" 4 0 3) => '("124" 3 0))
(check (round-digits-after-dot "1237xxx" 4 0 3) => '("124" 3 0))
(check (round-digits-after-dot "1238xxx" 4 0 3) => '("124" 3 0))
(check (round-digits-after-dot "1239xxx" 4 0 3) => '("124" 3 0))

(check (round-digits-after-dot "1270xxx" 4 0 3) => '("127" 3 0))
(check (round-digits-after-dot "1271xxx" 4 0 3) => '("127" 3 0))
(check (round-digits-after-dot "1272xxx" 4 0 3) => '("127" 3 0))
(check (round-digits-after-dot "1273xxx" 4 0 3) => '("127" 3 0))
(check (round-digits-after-dot "1274xxx" 4 0 3) => '("127" 3 0))
(check (round-digits-after-dot "1275xxx" 4 0 3) => '("128" 3 0)) ;; rounding to even
(check (round-digits-after-dot "1276xxx" 4 0 3) => '("128" 3 0))
(check (round-digits-after-dot "1277xxx" 4 0 3) => '("128" 3 0))
(check (round-digits-after-dot "1278xxx" 4 0 3) => '("128" 3 0))
(check (round-digits-after-dot "1279xxx" 4 0 3) => '("128" 3 0))

(check (round-digits-after-dot "1240xxx" 4 0 3) => '("124" 3 0))
(check (round-digits-after-dot "1241xxx" 4 0 3) => '("124" 3 0))
(check (round-digits-after-dot "1242xxx" 4 0 3) => '("124" 3 0))
(check (round-digits-after-dot "1243xxx" 4 0 3) => '("124" 3 0))
(check (round-digits-after-dot "1244xxx" 4 0 3) => '("124" 3 0))
(check (round-digits-after-dot "1245xxx" 4 0 3) => '("124" 3 0)) ;; rounding to even
(check (round-digits-after-dot "1246xxx" 4 0 3) => '("125" 3 0))
(check (round-digits-after-dot "1247xxx" 4 0 3) => '("125" 3 0))
(check (round-digits-after-dot "1248xxx" 4 0 3) => '("125" 3 0))
(check (round-digits-after-dot "1249xxx" 4 0 3) => '("125" 3 0))

(check (round-digits-after-dot "1260xxx" 4 0 3) => '("126" 3 0))
(check (round-digits-after-dot "1261xxx" 4 0 3) => '("126" 3 0))
(check (round-digits-after-dot "1262xxx" 4 0 3) => '("126" 3 0))
(check (round-digits-after-dot "1263xxx" 4 0 3) => '("126" 3 0))
(check (round-digits-after-dot "1264xxx" 4 0 3) => '("126" 3 0))
(check (round-digits-after-dot "1265xxx" 4 0 3) => '("126" 3 0)) ;; rounding to even
(check (round-digits-after-dot "1266xxx" 4 0 3) => '("127" 3 0))
(check (round-digits-after-dot "1267xxx" 4 0 3) => '("127" 3 0))
(check (round-digits-after-dot "1268xxx" 4 0 3) => '("127" 3 0))
(check (round-digits-after-dot "1269xxx" 4 0 3) => '("127" 3 0))



;;;; rounding one digit, dot offset 1

(check (round-digits-after-dot "1230xxx" 4 1 2) => '("123" 3 1))
(check (round-digits-after-dot "1231xxx" 4 1 2) => '("123" 3 1))
(check (round-digits-after-dot "1232xxx" 4 1 2) => '("123" 3 1))
(check (round-digits-after-dot "1233xxx" 4 1 2) => '("123" 3 1))
(check (round-digits-after-dot "1234xxx" 4 1 2) => '("123" 3 1))
(check (round-digits-after-dot "1235xxx" 4 1 2) => '("124" 3 1)) ;; rounding to even
(check (round-digits-after-dot "1236xxx" 4 1 2) => '("124" 3 1))
(check (round-digits-after-dot "1237xxx" 4 1 2) => '("124" 3 1))
(check (round-digits-after-dot "1238xxx" 4 1 2) => '("124" 3 1))
(check (round-digits-after-dot "1239xxx" 4 1 2) => '("124" 3 1))

(check (round-digits-after-dot "1270xxx" 4 1 2) => '("127" 3 1))
(check (round-digits-after-dot "1271xxx" 4 1 2) => '("127" 3 1))
(check (round-digits-after-dot "1272xxx" 4 1 2) => '("127" 3 1))
(check (round-digits-after-dot "1273xxx" 4 1 2) => '("127" 3 1))
(check (round-digits-after-dot "1274xxx" 4 1 2) => '("127" 3 1))
(check (round-digits-after-dot "1275xxx" 4 1 2) => '("128" 3 1)) ;; rounding to even
(check (round-digits-after-dot "1276xxx" 4 1 2) => '("128" 3 1))
(check (round-digits-after-dot "1277xxx" 4 1 2) => '("128" 3 1))
(check (round-digits-after-dot "1278xxx" 4 1 2) => '("128" 3 1))
(check (round-digits-after-dot "1279xxx" 4 1 2) => '("128" 3 1))

(check (round-digits-after-dot "1240xxx" 4 1 2) => '("124" 3 1))
(check (round-digits-after-dot "1241xxx" 4 1 2) => '("124" 3 1))
(check (round-digits-after-dot "1242xxx" 4 1 2) => '("124" 3 1))
(check (round-digits-after-dot "1243xxx" 4 1 2) => '("124" 3 1))
(check (round-digits-after-dot "1244xxx" 4 1 2) => '("124" 3 1))
(check (round-digits-after-dot "1245xxx" 4 1 2) => '("124" 3 1)) ;; rounding to even
(check (round-digits-after-dot "1246xxx" 4 1 2) => '("125" 3 1))
(check (round-digits-after-dot "1247xxx" 4 1 2) => '("125" 3 1))
(check (round-digits-after-dot "1248xxx" 4 1 2) => '("125" 3 1))
(check (round-digits-after-dot "1249xxx" 4 1 2) => '("125" 3 1))

(check (round-digits-after-dot "1260xxx" 4 1 2) => '("126" 3 1))
(check (round-digits-after-dot "1261xxx" 4 1 2) => '("126" 3 1))
(check (round-digits-after-dot "1262xxx" 4 1 2) => '("126" 3 1))
(check (round-digits-after-dot "1263xxx" 4 1 2) => '("126" 3 1))
(check (round-digits-after-dot "1264xxx" 4 1 2) => '("126" 3 1))
(check (round-digits-after-dot "1265xxx" 4 1 2) => '("126" 3 1)) ;; rounding to even
(check (round-digits-after-dot "1266xxx" 4 1 2) => '("127" 3 1))
(check (round-digits-after-dot "1267xxx" 4 1 2) => '("127" 3 1))
(check (round-digits-after-dot "1268xxx" 4 1 2) => '("127" 3 1))
(check (round-digits-after-dot "1269xxx" 4 1 2) => '("127" 3 1))




;;;; rounding one digit, dot offset 2

(check (round-digits-after-dot "1230xxx" 4 2 1) => '("123" 3 2))
(check (round-digits-after-dot "1231xxx" 4 2 1) => '("123" 3 2))
(check (round-digits-after-dot "1232xxx" 4 2 1) => '("123" 3 2))
(check (round-digits-after-dot "1233xxx" 4 2 1) => '("123" 3 2))
(check (round-digits-after-dot "1234xxx" 4 2 1) => '("123" 3 2))
(check (round-digits-after-dot "1235xxx" 4 2 1) => '("124" 3 2)) ;; rounding to even
(check (round-digits-after-dot "1236xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1237xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1238xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1239xxx" 4 2 1) => '("124" 3 2))

(check (round-digits-after-dot "1270xxx" 4 2 1) => '("127" 3 2))
(check (round-digits-after-dot "1271xxx" 4 2 1) => '("127" 3 2))
(check (round-digits-after-dot "1272xxx" 4 2 1) => '("127" 3 2))
(check (round-digits-after-dot "1273xxx" 4 2 1) => '("127" 3 2))
(check (round-digits-after-dot "1274xxx" 4 2 1) => '("127" 3 2))
(check (round-digits-after-dot "1275xxx" 4 2 1) => '("128" 3 2)) ;; rounding to even
(check (round-digits-after-dot "1276xxx" 4 2 1) => '("128" 3 2))
(check (round-digits-after-dot "1277xxx" 4 2 1) => '("128" 3 2))
(check (round-digits-after-dot "1278xxx" 4 2 1) => '("128" 3 2))
(check (round-digits-after-dot "1279xxx" 4 2 1) => '("128" 3 2))

(check (round-digits-after-dot "1240xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1241xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1242xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1243xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1244xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1245xxx" 4 2 1) => '("124" 3 2)) ;; rounding to even
(check (round-digits-after-dot "1246xxx" 4 2 1) => '("125" 3 2))
(check (round-digits-after-dot "1247xxx" 4 2 1) => '("125" 3 2))
(check (round-digits-after-dot "1248xxx" 4 2 1) => '("125" 3 2))
(check (round-digits-after-dot "1249xxx" 4 2 1) => '("125" 3 2))

(check (round-digits-after-dot "1260xxx" 4 2 1) => '("126" 3 2))
(check (round-digits-after-dot "1261xxx" 4 2 1) => '("126" 3 2))
(check (round-digits-after-dot "1262xxx" 4 2 1) => '("126" 3 2))
(check (round-digits-after-dot "1263xxx" 4 2 1) => '("126" 3 2))
(check (round-digits-after-dot "1264xxx" 4 2 1) => '("126" 3 2))
(check (round-digits-after-dot "1265xxx" 4 2 1) => '("126" 3 2)) ;; rounding to even
(check (round-digits-after-dot "1266xxx" 4 2 1) => '("127" 3 2))
(check (round-digits-after-dot "1267xxx" 4 2 1) => '("127" 3 2))
(check (round-digits-after-dot "1268xxx" 4 2 1) => '("127" 3 2))
(check (round-digits-after-dot "1269xxx" 4 2 1) => '("127" 3 2))



;;;; rounding to even

(check (round-digits-after-dot "1205xxx" 4 2 1) => '("120" 3 2))
(check (round-digits-after-dot "1215xxx" 4 2 1) => '("122" 3 2))
(check (round-digits-after-dot "1225xxx" 4 2 1) => '("122" 3 2))
(check (round-digits-after-dot "1235xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1245xxx" 4 2 1) => '("124" 3 2))
(check (round-digits-after-dot "1255xxx" 4 2 1) => '("126" 3 2))
(check (round-digits-after-dot "1265xxx" 4 2 1) => '("126" 3 2))
(check (round-digits-after-dot "1275xxx" 4 2 1) => '("128" 3 2))
(check (round-digits-after-dot "1285xxx" 4 2 1) => '("128" 3 2))

(check (round-digits-after-dot "9985xxx" 4 2 1) => '("998"  3 2))

(check (round-digits-after-dot "1205000" 7 2 1) => '("120" 3 2))
(check (round-digits-after-dot "1205001" 7 2 1) => '("121" 3 2))
(check (round-digits-after-dot "1215000" 7 2 1) => '("122" 3 2))
(check (round-digits-after-dot "1215001" 7 2 1) => '("122" 3 2))

;;; --------------------------------------------------------------------
;;; rounding with carry

(check (round-digits-after-dot "1295xxx" 4 2 1) => '("130"  3 2))
(check (round-digits-after-dot "1995xxx" 4 2 1) => '("200"  3 2))
(check (round-digits-after-dot "2995xxx" 4 2 1) => '("300"  3 2))
(check (round-digits-after-dot "99955xx" 5 2 1) => '("1000" 4 3))
(check (round-digits-after-dot "9995xxx" 4 2 1) => '("1000" 4 3))
(check (round-digits-after-dot "99953xx" 5 2 1) => '("1000" 4 3))




;;;; done

(check-report)

;;; end of file
