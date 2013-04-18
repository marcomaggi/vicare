;;;The code here is extracted from
;;;
;;;  ``Printing Floating-Point Numbers Quickly and Accurately''
;;;   http://www.cs.indiana.edu/~burger/FP-Printing-PLDI96.pdf
;;;
;;;It is  believed to be in  the public domain.  The  copyright below is
;;;for the R6RS implementation not part of the original work.

;;;Copyright (c) 2009 Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!r6rs
(library (vicare flonum-parser)
  (export parse-flonum)
  (import (rnrs)
    (vicare language-extensions syntaxes))


;;;; arguments validation

(define-argument-validation (flonum who obj)
  (flonum? obj)
  (assertion-violation who "expected flonum as argument" obj))

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))


(define fxsll fxarithmetic-shift-left)
(define fxsra fxarithmetic-shift-right)

(define (flonum-bytes f k)
  (let ((bv (make-bytevector 8)))
    (bytevector-ieee-double-set! bv 0 f (endianness big))
    (k (bytevector-u8-ref bv 0)
       (bytevector-u8-ref bv 1)
       (bytevector-u8-ref bv 2)
       (bytevector-u8-ref bv 3)
       (bytevector-u8-ref bv 4)
       (bytevector-u8-ref bv 5)
       (bytevector-u8-ref bv 6)
       (bytevector-u8-ref bv 7))))

(define (flonum-parts x)
  (flonum-bytes x
		(lambda (b0 b1 b2 b3 b4 b5 b6 b7)
		  (values
		   (zero? (fxand b0 128))
		   (+ (fxsll (fxand b0 127) 4)
		      (fxsra b1 4))
		   (+ (+ b7 (fxsll b6 8) (fxsll b5 16))
		      (* (+ b4
			    (fxsll b3 8)
			    (fxsll b2 16)
			    (fxsll (fxand b1 #b1111) 24))
			 (expt 2 24)))))))

(define flonum->digits
  (lambda (f e min-e p b B)
      ;;; flonum v = f * b^e
      ;;; p = precision  (p >= 1)
    (let ((round? (even? f)))
      (if (>= e 0)
	  (if (not (= f (expt b (- p 1))))
	      (let ((be (expt b e)))
		(scale (* f be 2) 2 be be 0 B round? f e))
	    (let* ((be (expt b e)) (be1 (* be b)))
	      (scale (* f be1 2) (* b 2) be1 be 0 B round? f e)))
	(if (or (= e min-e) (not (= f (expt b (- p 1)))))
	    (scale (* f 2) (* (expt b (- e)) 2) 1 1 0 B round? f e)
	  (scale (* f b 2) (* (expt b (- 1 e)) 2) b 1 0 B round? f e))))))

(define (len n)
  (let f ((n n) (i 0))
    (cond
     ((zero? n) i)
     (else (f (div n 2) (+ i 1))))))

(define scale
  (lambda (r s m+ m- k B round? f e)
    (let ((est (exact
		(ceiling
		 (- (* (+ e (len f) -1) (invlog2of B))
		    1e-10)))))
      (if (>= est 0)
	  (fixup r (* s (exptt B est)) m+ m- est B round?)
	(let ((scale (exptt B (- est))))
	  (fixup (* r scale) s (* m+ scale) (* m- scale) est B round?))))))

(define fixup
  (lambda (r s m+ m- k B round?)
    (if ((if round? >= >) (+ r m+) s) ; too low?
	(values (+ k 1) (generate r s m+ m- B round?))
      (values k (generate (* r B) s (* m+ B) (* m- B) B round?)))))

(define (chr x)
  (vector-ref '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) x))

(define generate
  (lambda (r s m+ m- B round?)
    (let-values (((q r) (div-and-mod r s)))
      (let ((tc1 ((if round? <= <) r m-))
	    (tc2 ((if round? >= >) (+ r m+) s)))
	(if (not tc1)
	    (if (not tc2)
		(cons (chr q) (generate (* r B) s (* m+ B) (* m- B) B round?))
	      (list (chr (+ q 1))))
	  (if (not tc2)
	      (list (chr q))
	    (if (< (* r 2) s)
		(list (chr q))
	      (list (chr (+ q 1))))))))))

(define invlog2of
  (let ((table (make-vector 37))
	(log2 (log 2)))
    (do ((B 2 (+ B 1)))
	((= B 37))
      (vector-set! table B (/ log2 (log B))))
    (lambda (B)
      (if (<= 2 B 36)
	  (vector-ref table B)
	(/ log2 (log B))))))

(define exptt
  (let ((table (make-vector 326)))
    (do ((k 0 (+ k 1)) (v 1 (* v 10)))
	((= k 326))
      (vector-set! table k v))
    (lambda (B k)
      (if (and (= B 10) (<= 0 k 325))
	  (vector-ref table k)
	(expt B k)))))

(define (convert-real-flonum pos? m e p k)
  (let-values (((expt digits) (flonum->digits m e 10 p 2 10)))
    (k pos? digits expt)))

(define (parse-flonum x k0 k1)
  ;;  (parse-flonum <fl>
  ;;    (lambda (positive? digits:list-of-chars exponent:int)
  ;;      ---)
  ;;    (lambda (inf/nan:string)
  ;;      ---))
  ;;  calls one of the two procedures depending on whether the
  ;;  number has a real value or not.
  (define who 'parse-flonum)
  (with-arguments-validation (who)
      ((flonum		x)
       (procedure	k0)
       (procedure	k1))
    (let-values (((pos? be m) (flonum-parts x)))
      (cond ((<= 1 be 2046) ; normalized flonum
	     (convert-real-flonum pos? (+ m (expt 2 52)) (- be 1075) 53 k0))
	    ((= be 0)
	     (convert-real-flonum pos? m -1074 52 k0))
	    ((= be 2047)
	     (k1 (if (= m 0) (if pos? "+inf.0" "-inf.0") "+nan.0")))
	    (else
	     (error who "cannot happen"))))))


;;;; done

)

;;; end of file
