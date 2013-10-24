;;;
;;;Part of: Vicare Scheme
;;;Contents: low level functions for (randomisations) and auxiliary libraries
;;;Date: Sat Jul  4, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2008 Derick Eddington
;;;Copyright (c) 2002 Sebastian Egner
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.
;;;


#!r6rs
(library (vicare crypto randomisations low)
  (export

    const:2^32 const:2^32-1

    ;; randomness source
    :random-source-make random-source?
    :random-source-state-ref
    :random-source-state-set!
    :random-source-seed!
    :random-source-jumpahead!
    :random-source-required-seed-values
    :random-source-integers-maker
    :random-source-reals-maker
    :random-source-bytevectors-filler

    ;; random numbers generation
    make-random-integer		make-random-real
    random-bytevector-fill!)
  (import (rnrs))


;;;; helpers

(define const:2^32   (bitwise-copy-bit 0 32 1))
(define const:2^32-1 (- const:2^32 1))


;;;; randomness source

(define-record-type (:random-source :random-source-make random-source?)
  (nongenerative nausicaa:randomisations:low::random-source)
  (fields (immutable state-ref)
	  (immutable state-set!)
	  (immutable seed!)
	  (immutable jumpahead!)
	  (immutable required-seed-values)
	  (immutable integers-maker)
	  (immutable reals-maker)
	  (immutable bytevectors-filler)))


;;;; random numbers generation

(define (make-random-integer U M make-random-bits)
  (cond ((zero? U)
	 0)
	((<= U M)
	 (%make-random-integer/small U M make-random-bits))
	(else
	 (%make-random-integer/large U M make-random-bits))))

(define (%make-random-integer/small U M make-random-bits)
  ;;Read the  documentation of  Nausicaa/Scheme, node "random  prng", to
  ;;understand what this does.
  (if (= U M)
      (make-random-bits)
    (let* ((Q  (div M U))
	   (QU (* Q U)))
      (do ((N (make-random-bits) (make-random-bits)))
	  ((< N QU)
	   (div N Q))))))

(define (%make-random-integer/large U M make-random-bits)
  ;;Read the  documentation of  Nausicaa/Scheme, node "random  prng", to
  ;;understand what this does.
  (define (polynomial k)
    ;;Notice that this function returns  integers N' in a range 0<=N'<M'
    ;;with M^k<=M'; that is N' can be greater than M^k.
    (%polynomial k M make-random-bits))
  (do ((k 2 (+ k 1))
       (M^k (* M M) (* M^k M)))
      ((<= U M^k)
       (if (= U M^k)
	   (polynomial k)
	 (let* ((Q  (div M^k U))
		(QU (* Q U)))
	   (do ((N (polynomial k) (polynomial k)))
	       ((< N QU)
		(div N Q))))))))

;;The  version  of %POLYNOMIAL  commented  out  below  is an  equivalent
;;reorganisation of the original version in the reference implementation
;;of SRFI-42.  It literally computes:
;;
;;  N0 + (M * (N1 + (M * (N2 + (... (M * (N(k-3) + (M * (N(k-2) + (M * N(k-1)))))))))))
;;
;;which is  not tail  recursive, but uses  Horner's polynomial-rewriting
;;rule which has less operations.
;;
;; (define (%polynomial k U M make-random-bits)
;;   (let ((N (%make-random-integer/small U M make-random-bits)))
;;     (if (= k 1)
;; 	     N
;;       (+ N (* M (%polynomial (- k 1) U M make-random-bits))))))
;;
;;The polynomial can be rewritten:
;;
;;  N0 + M * N1 + M^2 * N2 + ... + M^(k-2) * N(k-2) + M^(k-1) * N(k-1)
;;
;;which is implemented by the tail recursive version below.  Notice that
;;the maximum value is realised when every N(k) equals M-1 and its value
;;is M^k.
;;
(define (%polynomial k M make-random-bits)
  (define (integer)
    (%make-random-integer/small M M make-random-bits))
  (let loop ((A   (integer))
	     (j   1)
	     (M^j M))
    (if (= j k)
	A
      (loop (+ A (* M^j (integer))) (+ 1 j) (* M M^j)))))

(define make-random-real
  (let ((%normalise (lambda (N M) (inexact (/ (+ 1 N) (+ 1 M))))))
    ;;Read the documentation of  Nausicaa/Scheme, node "random prng", to
    ;;understand what this does.
    (case-lambda
     ((M make-random-bits)
      (%normalise (make-random-bits) M))
     ((M make-random-bits unit)
      (if (<= 1 unit)
	  (%normalise (make-random-bits) M)
	(let ((C (- (/ unit) 1)))
	  (if (<= C M)
	      ;;This is like: (make-random-real M make-random-bits)
	      (begin
;;;	    (write 'avoiding-unit-because-too-big)(newline)
		(%normalise (make-random-bits) M))
	    (do ((k 1 (+ k 1))
		 (U C (/ U M)))
		((<= U 1)
;;;	     (write 'making-use-of-unit-here)(newline)
		 (%normalise (%polynomial k M make-random-bits)
			     (expt M k)))))))))))

(define (random-bytevector-fill! bv make-random-32bits)
  (let ((len  (bytevector-length bv)))
    (let-values (((len-32bits len-rest) (div-and-mod len 4)))
      (set! len-32bits (* 4 len-32bits))
      (do ((i 0 (+ 4 i)))
	  ((= i len-32bits))
	(bytevector-u32-native-set! bv i (make-random-32bits)))
      (let ((bits (make-bytevector 4)))
	(bytevector-u32-native-set! bits 0 (make-random-32bits))
	(do ((i (- len len-rest) (+ 1 i))
	     (j 0 (+ 1 j)))
	    ((= i len))
	  (bytevector-u8-set! bv i (bytevector-u8-ref bits j)))))))


;;;; done

)

;;; end of file
