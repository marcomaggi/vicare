;;;
;;;Part of: Vicare Scheme
;;;Contents: randomness related string functions
;;;Date: Thu Jul  2, 2009
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
(library (vicare crypto randomisations strings)
  (export
    random-string-unfold-chars
    %random-string-shuffle		random-string-shuffle
    %random-string-shuffle!		random-string-shuffle!

    %random-string-sample		random-string-sample
    %random-string-sample-population	random-string-sample-population)
  (import (rnrs)
    (vicare crypto randomisations)
    (vicare containers strings low)
    (rename (only (vicare containers strings) %string-unpack)
	    (%string-unpack unpack))
    (rnrs mutable-strings))


;;;; low level

(define (%random-string-shuffle! source str start past)
  (let* ((len		(- past start))
	 (integer-maker	(random-source-integers-maker source)))
    (do ((k len (- k 1)))
	((= k 1)
	 str)
      (let* ((i (+ start (- k 1)))
	     (j (+ start (integer-maker k)))
	     (xi (string-ref str i))
	     (xj (string-ref str j)))
	(string-set! str i xj)
	(string-set! str j xi)))))

(define (%random-string-shuffle source str start past)
  (let ((dst (substring str start past)))
    (%random-string-shuffle! source str start past)))

(define (%random-string-sample source str start past)
  (let ((index-maker	(random-source-integers-maker source))
	(max		(- past start)))
    (lambda ()
      (string-ref str (+ start (index-maker max))))))

(define (%random-string-sample-population source len str start past)
  (let ((sampler	(%random-string-sample source str start past)))
    (lambda ()
      (do ((i 0 (+ 1 i))
	   (individual (make-string len)))
	  ((= i len)
	   individual)
	(string-set! individual i (sampler))))))


;;;; high level

(define (random-string-unfold-chars integer-maker number-of-numbers)
  (let ((str (make-string number-of-numbers)))
    (do ((i 0 (+ 1 i)))
	((= i number-of-numbers)
	 str)
      (do ((n (integer-maker) (integer-maker)))
	  ((or (and (<= 0 n)     (< n #xD800))
	       (and (< #xDFFF n) (< n #x10FFFF)))
	   (string-set! str i (integer->char n)))))))

(define-syntax random-string-shuffle
  (syntax-rules ()
    ((_ ?S ?source)
     (let-values (((str start past) (unpack ?S)))
       (%random-string-shuffle ?source str start past)))))

(define-syntax random-string-shuffle!
  (syntax-rules ()
    ((_ ?S ?source)
     (let-values (((str start past) (unpack ?S)))
       (%random-string-shuffle! ?source str start past)))))

(define-syntax random-string-sample
  (syntax-rules ()
    ((_ ?S ?source)
     (let-values (((str start past) (unpack ?S)))
       (%random-string-sample ?source str start past)))))

(define-syntax random-string-sample-population
  (syntax-rules ()
    ((_ ?S ?len ?source)
     (let-values (((str start past) (unpack ?S)))
       (%random-string-sample-population ?source ?len str start past)))))


;; (define (string-permute! data permutation)
;;   (let ((len (string-length permutation)))
;;     (unless (= len (string-length data))
;;       (assertion-violation 'string-permute!
;; 	(string-append "expected strings with equal length, got data string of length"
;; 		       (number->string (string-length data))
;; 		       " and permutation string of length "
;; 		       (number->string len))
;; 	data permutation))
;;     (let ((x (string-ref data (string-ref permutation 0))))
;;       (do ((i 1 (+ 1 i)))
;; 	  ((= i len))
;; 	(let ((j (string-ref permutation i)))
;; 	  (string-set! data (string-ref data i)))
;; 	  (set! x (string-ref data j))
;; 	))))


;;;; done

)

;;; end of file
