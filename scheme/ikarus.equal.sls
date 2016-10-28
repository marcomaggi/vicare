;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;
;;;The code has been copied from:
;;;
;;;   Michael  D.  Adams  and  R.  Kent  Dybvig.   Efficient nondestructive  equality
;;;   checking for trees and graphs.  In ICFP '08: Proceeding of the 13th ACM SIGPLAN
;;;   international conference  on Functional  programming, pages 179-188.   ACM, New
;;;   York, NY, USA, 2008.  ISBN 978-1-59593-919-7. doi: 10.1145/1411204.1411230.
;;;
;;;such code is also distributed on the Net under the following license.
;;;
;;;Copyright (c) 2008 Michael D. Adams and R. Kent Dybvig
;;;Modified in 2016 by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;


(library (ikarus.equal)
  (export equal?)
  (import (except (vicare)
		  equal?)
    (vicare system structs)
    (only (vicare system $records)
      	  $record-type-equality-predicate)
    (only (vicare system $pointers)
	  $pointer=)
    (only (vicare system $structs)
	  $struct-ref)
    (only (vicare system $keywords)
	  $keyword=?))

  (module UNSAFE
    (< <= > >= = + - vector-ref vector-length car cdr)
    (import
      (rename (vicare system $vectors)
        ($vector-length vector-length)
        ($vector-ref    vector-ref))
      (rename (vicare system $pairs)
        ($car   car)
        ($cdr   cdr))
      (rename (vicare system $fx)
        ($fx+      +)
        ($fx-      -)
        ($fx<      <)
        ($fx>      >)
        ($fx>=     >=)
        ($fx<=     <=)
        ($fx=      =))))


(define (equal? x y)
  (let ((k (pre? x y k0)))
    (and k (or (> k 0)
	       (interleave? x y 0)))))

;;;Commented out  because unused; it is  better to just  name EQUAL? the
;;;function  PRECHECK/INTERLEAVE-EQUAL?  rather  than  do the  following
;;;(Marco Maggi; Nov 2, 2011).
;;;
;;;(define (equal? x y)
;;;  (precheck/interleave-equal? x y))
;;;
;;;(define (precheck/interleave-equal? x y)
;;;  (let ((k (pre? x y k0)))
;;;    (and k (or (> k 0)
;;;               (interleave? x y 0)))))
;;;
;;;(define (interleave-equal? x y)
;;;  (interleave? x y k0))

;;; constants
(define k0 400)
(define kb -40)


(define (pre? x y k)
  ;;Perform  a precheck comparison  between X  and Y,  ignoring possible
  ;;cycles, using K as maximum  number of recursions.  Return false if X
  ;;and Y are different, or the updated number of recursions.
  ;;
  (import UNSAFE)
  (cond ((eq? x y)	;symbols and EQ? values in general
	 k)
	((pair? x)
	 (and (pair? y)
	      (if (<= k 0)
		  k
		(let ((k (pre? (car x) (car y) (- k 1))))
		  (and k (pre? (cdr x) (cdr y) k))))))
	((vector? x)
	 (and (vector? y)
	      (let ((n (vector-length x)))
		(and (= (vector-length y) n)
		     (let loop ((i 0) (k k))
		       (if (or (= i n) (<= k 0))
			   k
			 (let ((k (pre?
				   (vector-ref x i)
				   (vector-ref y i)
				   (- k 1))))
			   (and k (loop (+ i 1) k)))))))))
	((string? x)
	 (and (string? y)
	      (string=? x y)
	      k))
	((bytevector? x)
	 (and (bytevector? y)
	      (bytevector=? x y)
	      k))
	((pointer? x)
	 (and (pointer? y)
	      ($pointer= x y)
	      k))
	((keyword? x)
	 (and (keyword? y)
	      ($keyword=? x y)
	      k))
	((ipair? x)
	 ;;An  ipair is  a struct,  but  STRUCT=? uses  EQV? internally;  so here  we
	 ;;implement a branch that actually traverses the internals of the ipairs.
	 (and (ipair? y)
	      (if (<= k 0)
		  k
		(let ((k (pre? (icar x) (icar y) (- k 1))))
		  (and k (pre? (icdr x) (icdr y) k))))))
        ((record-object? x)
         (and (record-object? y)
              (let ((rtdx (struct-std x))
                    (rtdy (struct-std y)))
                (and (eq? rtdx rtdy)
                     (cond (($record-type-equality-predicate rtdx)
                            => (lambda (equal-pred)
                                 (equal-pred x y)))
                           (else
                            (record=? x y)))))
              k))
	((struct? x)
	 (and (struct? y)
	      (struct=? x y)
	      k))
	(else	;chars, numbers, booleans, other non-compound value
	 (and (eqv? x y) k))))


(define (interleave? x y k)
  (import UNSAFE)
  (let ((ht #f))
    (define (call-union-find x y)
      (unless ht (set! ht (make-eq-hashtable)))
      (union-find ht x y))
    (define (e? x y k)
      (if (<= k 0)
	  (if (= k kb) (fast? x y (random (* 2 k0))) (slow? x y k))
	(fast? x y k)))
    (define (slow? x y k)
      (cond
       ((eq? x y) k)
       ((pair? x)
	(and (pair? y)
	     (if (call-union-find x y)
		 0
	       (let ((k (e? (car x) (car y) (- k 1))))
		 (and k (e? (cdr x) (cdr y) k))))))
       ((vector? x)
	(and (vector? y)
	     (let ((n (vector-length x)))
	       (and (= (vector-length y) n)
		    (if (call-union-find x y)
			0
		      (let f ((i 0) (k (- k 1)))
			(if (= i n)
			    k
			  (let ((k (e? (vector-ref x i)
				       (vector-ref y i)
				       k)))
			    (and k (f (+ i 1) k))))))))))
       ((string? x)
	(and (string? y)
	     (string=? x y)
	     k))
       ((bytevector? x)
	(and (bytevector? y)
	     (bytevector=? x y)
	     k))
       ((pointer? x)
	(and (pointer? y)
	     ($pointer= x y)
	     k))
       ((keyword? x)
	(and (keyword? y)
	     ($keyword=? x y)
	     k))
       ((ipair? x)
	;;An  ipair is  a  struct, but  STRUCT=?  uses EQV?  internally;  so here  we
	;;implement a branch that actually traverses the internals of the ipairs.
	(and (ipair? y)
	     (if (call-union-find x y)
		 0
	       (let ((k (e? (icar x) (icar y) (- k 1))))
		 (and k (e? (icdr x) (icdr y) k))))))
       ((record-object? x)
        (and (record-object? y)
             (let ((rtdx (record-rtd x))
                   (rtdy (record-rtd y)))
               (and (eq? rtdx rtdy)
                    (cond (($record-type-equality-predicate rtdx)
                           => (lambda (equal-pred)
                                (equal-pred x y)))
                          (else
                           (record=? x y)))))
             k))
       ((struct? x)
	(and (struct? y)
	     (struct=? x y)
	     k))
       (else
	(and (eqv? x y) k))))
    (define (fast? x y k)
      (let ((k (- k 1)))
	(cond
	 ((eq? x y) k)
	 ((pair? x)
	  (and (pair? y)
	       (let ((k (e? (car x) (car y) k)))
		 (and k (e? (cdr x) (cdr y) k)))))
	 ((vector? x)
	  (and (vector? y)
	       (let ((n (vector-length x)))
		 (and (= (vector-length y) n)
		      (let f ((i 0) (k k))
			(if (= i n)
			    k
			  (let ((k (e? (vector-ref x i)
				       (vector-ref y i)
				       k)))
			    (and k (f (+ i 1) k)))))))))
	 ((string? x)
	  (and (string? y)
	       (string=? x y)
	       k))
	 ((bytevector? x)
	  (and (bytevector? y)
	       (bytevector=? x y)
	       k))
	 ((pointer? x)
	  (and (pointer? y)
	       ($pointer= x y)
	       k))
	 ((keyword? x)
	  (and (keyword? y)
	       ($keyword=? x y)
	       k))
	 ((ipair? x)
	  ;;An  ipair is  a struct,  but STRUCT=?  uses EQV?  internally; so  here we
	  ;;implement a branch that actually traverses the internals of the ipairs.
	  (and (ipair? y)
	       (let ((k (e? (icar x) (icar y) k)))
		 (and k (e? (icdr x) (icdr y) k)))))
	 ((record-object? x)
	  (and (record-object? y)
	       (let ((rtdx (record-rtd x))
	             (rtdy (record-rtd y)))
	         (and (eq? rtdx rtdy)
	              (cond (($record-type-equality-predicate rtdx)
	                     => (lambda (equal-pred)
	                          (equal-pred x y)))
	                    (else
	                     (record=? x y)))))
	       k))
	 ((struct? x)
	  (and (struct? y)
	       (struct=? x y)
	       k))
	 (else
	  (and (eqv? x y)
	       k)))))
    (and (e? x y k) #t)))


(define (union-find ht x y)
  (import UNSAFE)
  (define-struct box (content))
  (define (find b)
    (let ((n (box-content b)))
      (if (box? n)
	  (let loop ((b b) (n n))
	    (let ((nn (box-content n)))
	      (if (box? nn)
		  (begin
		    (set-box-content! b nn)
		    (loop n nn))
		n)))
	b)))
  (let ((bx (hashtable-ref ht x #f))
	(by (hashtable-ref ht y #f)))
    (if (not bx)
	(if (not by)
	    (let ((b (make-box 1)))
	      (hashtable-set! ht x b)
	      (hashtable-set! ht y b)
	      #f)
	  (let ((ry (find by)))
	    (hashtable-set! ht x ry)
	    #f))
      (if (not by)
	  (let ((rx (find bx)))
	    (hashtable-set! ht y rx)
	    #f)
	(let ((rx (find bx))
	      (ry (find by)))
	  (or (eq? rx ry)
	      (let ((nx (box-content rx))
		    (ny (box-content ry)))
		(if (> nx ny)
		    (begin
		      (set-box-content! ry rx)
		      (set-box-content! rx (+ nx ny))
		      #f)
		  (begin
		    (set-box-content! rx ry)
		    (set-box-content! ry (+ ny nx))
		    #f)))))))))


;;;; done

#| end of library |# )

;;; end of file
