;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Scheme
;;;Contents: Rabin-Karp string search
;;;Date: Wed May 11, 2011
;;;
;;;Abstract
;;;
;;;	The code is derived from the implementation at:
;;;
;;;	<http://algs4.cs.princeton.edu/53substring/RabinKarp.java.html>
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
(library (vicare containers strings rabin-karp)
  (export
    make-rabin-karp
    rabin-karp?
    rabin-karp.vicare-arguments-validation
    rabin-karp-search)
  (import (vicare)
    (vicare unsafe operations)
    (vicare arguments validation)
    (vicare language-extensions infix))


;;;; helpers

(define (%text-match? pattern pattern.len text text.start)
  ;;Does the pattern exactly match the text at given offset?
  ;;
  (let loop ((i 0) (j text.start))
    (cond ((= i pattern.len)
	   #t)
	  ((char=? (string-ref pattern i)
		   (string-ref text    j))
	   (loop (+ 1 i) (+ 1 j)))
	  (else #f))))



;;;; rolling hash

#| Given  a sequence of 5  integers: I1, I2,  I3, I4, I5 and  an integer
base B, we can compute the hash value of the first 3 with:

  H(I1,I2,I3) = (I1 * B + I2) * B + I3
              = I1 * B^2 + I2 * B + I3
              = I1 * B^(3-1) + I2 * B^(3-2) + I3^(3-3)

now  if we  want to  exclude  the leftmost  integer I1  and include  the
rightmost integer I4, we can recompute the hash value with:

  H(I2,I3,I4) = I2 * B^2 + I3 * B + I4

or  compute  H(I2,I3,I4) starting  from  H(I1,I2,I3)  using the  rolling
formula:

  H(I2,I3,I4) = (H(I1,I2,I3) - (I1 * B^(3-1))) * B + I4

now if we  want to exclude the leftmost I2 and  include the rightmost I5
we can use the rolling formula:

  H(I3,I4,I5) = (H(I2,I3,I4) - (I2 * B^(3-1))) * B + I5

  Notice  that the  coefficient B^(3-1)  is  a constant  in the  rolling
formula, so we can precompute it as B^(M-1) where M is the length of the
subsequence we include in the hash computation (3 in this example).

  We are not interested in the value of the polynomial, only in the hash
value; so we select an appropriate prime number Q and compute everything
using its modulo:

  H(I1,I2,I3) = ((((I1 mod Q) * B + I2) mod Q) * B + I3) mod Q
  H(I2,I3,I4) = ((H(I1,I2,I3) - (I1 * B^(3-1)) mod Q) * B + I4) mod Q

|#

(define (%full-hash key number-of-chars base prime)
  (define-inline (chi idx)
    (char->integer (string-ref key idx)))
  (let loop ((H (chi 0))
	     (i 1))
    (if (< i number-of-chars)
	(loop (infix (H * base + chi(i)) mod prime)
	      (+ 1 i))
      H)))

(define (%rolling-hash H old-chi new-chi B B^M-1 Q)
  ;;Compute and return the new hash value from the old one.
  ;;
  ;;H = the old hash value.
  ;;
  ;;old-chi  = fixnum representing  the leftmost  character in  the hash
  ;;formula for the old value.
  ;;
  ;;new-chi =  fixnum representing the  rightmost character in  the hash
  ;;formula for the new value.
  ;;
  ;;B, B^M-1 = the base value and the precomputed coefficient to be used
  ;;for old character removal.
  ;;
  ;;Q = the large prime fixnum.
  ;;
  (infix (((H + Q - (B^M-1 * old-chi) mod Q) mod Q) * B + new-chi) mod Q))


(define-record-type rabin-karp
  (nongenerative vicare:containers:strings:rabin-karp)
  (opaque #t)
  (sealed #t)
  (fields (immutable pattern)
	  (immutable pattern.hash)
	  (immutable prime)
	  (immutable base)
	  (immutable base^M-1))
  (protocol
   (lambda (make-record)
     (define (maker pattern base prime)
       (define who 'make-rabin-karp)
       (with-arguments-validation (who)
	   ((string		pattern)
	    (positive-fixnum	base)
	    (positive-fixnum	prime))
	 (let ((M ($string-length pattern)))
	   (make-record pattern (%full-hash pattern M base prime) prime base
			;;Precompute: base^(M-1) % prime.
			(let loop ((K 1) (i 1))
			  (if (>= i M)
			      K
			    (loop (infix (K * base) mod prime) (fx+ 1 i))))))))
     (define-constant DEFAULT-BASE  256)
     (define-constant DEFAULT-PRIME 472882049)
     (case-lambda
      ((pattern)
       (maker pattern DEFAULT-BASE DEFAULT-PRIME))
      ((pattern base)
       (maker pattern base DEFAULT-PRIME))
      ((pattern base prime)
       (maker pattern base prime))))))

(define-argument-validation (rabin-karp who obj)
  (rabin-karp? obj)
  (assertion-violation who "expected rabin-karp search object as argument" obj))

(define (rabin-karp-search S text)
  (define who 'rabin-karp-search)
  (with-arguments-validation (who)
      ((rabin-karp	S)
       (string		text))
    (let* ((M	($string-length ($rabin-karp-pattern S)))
	   (H	(%full-hash text M ($rabin-karp-base S) ($rabin-karp-prime S))))
      (define-inline (chi offset)
	(char->integer (string-ref text offset)))
      (define-inline (%compare hash offset)
	(and (= hash ($rabin-karp-pattern.hash S))
	     (%text-match? ($rabin-karp-pattern S) M text offset)))
      (cond ((< ($string-length text) M) ;impossible to match
	     #f)
	    ((%compare H 0)
	     0)	;match at offset 0
	    (else
	     (let loop ((H H) (i M))
	       (if (>= i ($string-length text))
		   #f ;no match
		 (let* ((H	(%rolling-hash H (chi (- i M)) (chi i)
					       ($rabin-karp-base S)
					       ($rabin-karp-base^M-1 S)
					       ($rabin-karp-prime S)))
			(offset	(infix i - M + 1)))
		   (if (%compare H offset)
		       offset
		     (loop H (fx+ 1 i)))))))))))


;;;; done

)

;;; end of file
