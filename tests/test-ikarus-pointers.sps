;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests  from  the  file  "scheme/tests/pointers.ss" file  in  the
;;;	original Ikarus distribution.
;;;
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

#!ikarus
(import (ikarus)
  (ikarus system $foreign))

(define bits
  (if (<= (fixnum-width) 32) 32 64))

(define (test-pointer-values)
  (define mask (sub1 (sll 1 bits)))
  (define (test-pointer n)
    (let* ([np (integer->pointer n)]
	   [m (pointer->integer np)]
	   [mp (integer->pointer m)])
      (unless (= (bitwise-and n mask) (bitwise-and m mask))
	(error 'test "failed/got" n m
	       (bitwise-and n mask) (bitwise-and m mask)))))
  (test-pointer 0)
  (test-pointer 100)
  (test-pointer -100)
  (test-pointer (greatest-fixnum))
  (test-pointer (least-fixnum))
  (test-pointer (+ 1 (greatest-fixnum)))
  (test-pointer (+ 1 (least-fixnum)))
  (test-pointer (- 1 (greatest-fixnum)))
  (test-pointer (- 1 (least-fixnum)))
  (test-pointer (+ -1 (greatest-fixnum)))
  (test-pointer (+ -1 (least-fixnum)))
  (test-pointer (- -1 (greatest-fixnum)))
  (test-pointer (- -1 (least-fixnum)))
  (test-pointer (* 2 (greatest-fixnum)))
  (test-pointer (* 2 (least-fixnum)))
  (test-pointer (* 4 (greatest-fixnum)))
  (test-pointer (* 4 (least-fixnum)))
  (test-pointer (* 8 (greatest-fixnum)))
  (test-pointer (* 8 (least-fixnum)))
  (test-pointer (* 16 (greatest-fixnum)))
  (test-pointer (* 16 (least-fixnum))))

(define (combinations n)
  (define (one-bit-combinations n)
    (let ([n (- n 1)])
      (if (< n 0)
	  '()
	(cons (sll 1 n) (one-bit-combinations n)))))
  (define (or* ls1 ls2)
    (apply append
	   (map
	       (lambda (n1)
		 (map
		     (lambda (n2)
		       (bitwise-ior n1 n2))
		   ls2))
	     ls1)))
  (let ([n (min bits n)])
    (let* ([ls1 (one-bit-combinations n)]
	   [ls2 (or* ls1 ls1)]
	   [ls3 (or* ls2 ls1)])
      (append
       (list 0 (sub1 (sll 1 (- n 1))) (sub1 (sll 1 n)))
       ls1 ls2 ls3))))

(define (u* n)
  (let ([n (min n bits)])
    (combinations n)))

(define (s* n)
  (let ([n (min n bits)])
    (let ([mx (- (expt 2 (- n 1)) 1)])
      (map
          (lambda (x)
            (if (> x mx)
                (- x (expt 2 n))
	      x))
	(combinations n)))))


(define (t-ref/set type combinations getter setter)
  (printf "testing memory access (~s combination for type ~s)\n"
	  (length combinations)
	  type)
  (for-each
      (lambda (n)
        (let ([m
               (let ([p (malloc 8)])
                 (setter p 0 n)
                 (let ([m (getter p 0)])
                   (free p)
                   m))])
          (unless (= n m)
            (error 'test "failed" getter setter n m))))
    combinations))

(define (check-combinations n)
  (define (same-pattern? u s i)
    (cond
     [(= i 1)
      (cond
       [(= u 0) (= s 0)]
       [(= u 1) (= s -1)]
       [else #f])]
     [else
      (and (= (bitwise-and u 1) (bitwise-and s 1))
	   (same-pattern? (sra u 1) (sra s 1) (- i 1)))]))
  (define (check u s)
    (unless (same-pattern? u s (min n bits))
      (error 'check "failed" u s)))
  (for-each check (u* n) (s* n)))


(define (run-tests)
  (for-each check-combinations '(8 16 32 64))

  (test-pointer-values)
  (t-ref/set 'char   (s*  8) pointer-ref-c-signed-char    pointer-set-c-char!)
  (t-ref/set 'short  (s* 16) pointer-ref-c-signed-short   pointer-set-c-short!)
  (t-ref/set 'int    (s* 32) pointer-ref-c-signed-int     pointer-set-c-int!)
  (t-ref/set 'long   (s* 64) pointer-ref-c-signed-long    pointer-set-c-long!)
  (t-ref/set 'long-long
	     (s* 64)
	     pointer-ref-c-signed-long-long
	     pointer-set-c-long-long!)
  (t-ref/set 'uchar  (u*  8) pointer-ref-c-unsigned-char  pointer-set-c-char!)
  (t-ref/set 'ushort (u* 16) pointer-ref-c-unsigned-short pointer-set-c-short!)
  (t-ref/set 'uint   (u* 32) pointer-ref-c-unsigned-int   pointer-set-c-int!)
  (t-ref/set 'ulong  (u* 64) pointer-ref-c-unsigned-long  pointer-set-c-long!)
  (t-ref/set 'ulong-long
	     (u* 64)
	     pointer-ref-c-unsigned-long-long
	     pointer-set-c-long-long!)
  )

(display "*** testing pointers\n" (current-error-port))
(flush-output-port (current-error-port))
(run-tests)
(display "; *** done\n" (current-error-port))
(flush-output-port (current-error-port))


;;; end of file
