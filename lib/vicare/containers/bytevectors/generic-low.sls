;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: low level bytevector functions
;;;Date: Sat Jun 26, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;;Copyright (c) 2009 Derick Eddington
;;;
;;;Derived from the SRFI 13 reference implementation.
;;;
;;;Olin Shivers 7/2000
;;;
;;;Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;;Copyright (c) 1998, 1999, 2000 Olin Shivers.  All rights reserved.
;;;   The details of the copyrights appear at the end of the file. Short
;;;   summary: BSD-style open source.
;;;
;;;Copyright details
;;;=================
;;;
;;;The prefix/suffix and comparison routines in this code had (extremely
;;;distant) origins  in MIT Scheme's  string lib, and  was substantially
;;;reworked by  Olin Shivers (shivers@ai.mit.edu)  9/98. As such,  it is
;;;covered by MIT Scheme's open source copyright. See below for details.
;;;
;;;The KMP string-search code  was influenced by implementations written
;;;by Stephen  Bevan, Brian Dehneyer and Will  Fitzgerald. However, this
;;;version was written from scratch by myself.
;;;
;;;The remainder  of this  code was written  from scratch by  myself for
;;;scsh.  The scsh  copyright is a BSD-style open  source copyright. See
;;;below for details.
;;;
;;;-- Olin Shivers
;;;
;;;MIT Scheme copyright terms
;;;==========================
;;;
;;;This   material  was  developed   by  the   Scheme  project   at  the
;;;Massachusetts  Institute  of  Technology,  Department  of  Electrical
;;;Engineering and Computer Science.  Permission to copy and modify this
;;;software, to redistribute either  the original software or a modified
;;;version, and to use this software for any purpose is granted, subject
;;;to the following restrictions and understandings.
;;;
;;;1. Any copy made of  this software must include this copyright notice
;;;   in full.
;;;
;;;2. Users  of this software  agree to make  their best efforts  (a) to
;;;   return to  the MIT Scheme  project any improvements  or extensions
;;;   that they make, so that  these may be included in future releases;
;;;   and (b) to inform MIT of noteworthy uses of this software.
;;;
;;;3.  All materials  developed  as a  consequence  of the  use of  this
;;;   software shall  duly acknowledge such use, in  accordance with the
;;;   usual standards of acknowledging credit in academic research.
;;;
;;;4. MIT has made no  warrantee or representation that the operation of
;;;   this software will  be error-free, and MIT is  under no obligation
;;;   to  provide  any  services,  by  way of  maintenance,  update,  or
;;;   otherwise.
;;;
;;;5. In  conjunction  with  products  arising  from  the  use  of  this
;;;   material, there shall  be no use of the  name of the Massachusetts
;;;   Institute  of Technology  nor  of any  adaptation  thereof in  any
;;;   advertising,  promotional,  or   sales  literature  without  prior
;;;   written consent from MIT in each case.
;;;
;;;Scsh copyright terms
;;;====================
;;;
;;;All rights reserved.
;;;
;;;Redistribution and  use in source  and binary forms, with  or without
;;;modification,  are permitted provided  that the  following conditions
;;;are met:
;;;
;;;1.  Redistributions of source  code must  retain the  above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;;2. Redistributions in binary  form must reproduce the above copyright
;;;   notice, this  list of conditions  and the following  disclaimer in
;;;   the  documentation  and/or   other  materials  provided  with  the
;;;   distribution.
;;;
;;;3. The  name of  the authors may  not be  used to endorse  or promote
;;;   products derived from this software without specific prior written
;;;   permission.
;;;
;;;THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;;IMPLIED  WARRANTIES,  INCLUDING,  BUT  NOT LIMITED  TO,  THE  IMPLIED
;;;WARRANTIES OF  MERCHANTABILITY AND  FITNESS FOR A  PARTICULAR PURPOSE
;;;ARE  DISCLAIMED.  IN NO  EVENT SHALL  THE AUTHORS  BE LIABLE  FOR ANY
;;;DIRECT,  INDIRECT, INCIDENTAL,  SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL
;;;DAMAGES  (INCLUDING, BUT  NOT LIMITED  TO, PROCUREMENT  OF SUBSTITUTE
;;;GOODS  OR  SERVICES; LOSS  OF  USE,  DATA,  OR PROFITS;  OR  BUSINESS
;;;INTERRUPTION) HOWEVER CAUSED AND  ON ANY THEORY OF LIABILITY, WHETHER
;;;IN  CONTRACT,  STRICT LIABILITY,  OR  TORT  (INCLUDING NEGLIGENCE  OR
;;;OTHERWISE) ARISING IN  ANY WAY OUT OF THE USE  OF THIS SOFTWARE, EVEN
;;;IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;Other copyright terms
;;;=====================
;;;
;;;Copyright (c) 2008 Derick Eddington.  Ported to R6RS.


#!r6rs
(library (vicare containers bytevectors generic-low)
  (export instantiate-body)
  (import (only (vicare containers auxiliary-syntaxes)
		define-instantiable-body))

  (define-instantiable-body instantiate-body


;;;; helpers

(define $white-spaces-for-dictionary-comparison
  (map char->integer '(#\space #\tab #\vtab #\linefeed #\return #\page)))

(define (bvs-list-min-length sequences)
  (apply min (map bytevector-length sequences)))

(define (%total-length-of-bytevectors-list bvs)
  (let loop ((bvs bvs) (len 0))
    (if (null? bvs)
	len
      (loop (cdr bvs) (+ len (bytevector-length (car bvs)))))))

(define (%integer-char-ci=? a b)
  (char-ci=? (integer->char a) (integer->char b)))

(define (%integer-char-ci<? a b)
  (char-ci<? (integer->char a) (integer->char b)))

(define (%integer-char-ci<=? a b)
  (char-ci<=? (integer->char a) (integer->char b)))

(define (%assert-bytevectors-of-same-length who bvs)
  (unless (apply =* (map bytevector-length bvs))
    (assertion-violation who "expected list of bytevectors of the same length")))

(define (=* . args)
  ;;This exists because some implementations (Mosh) do not allow = to be
  ;;called with less than 2 arguments.
  (if (null? args)
      #t
    (let loop ((val  (car args))
	       (args (cdr args)))
      (or (null? args)
	  (let ((new-val (car args)))
	    (and (= val new-val)
		 (loop new-val (cdr args))))))))


;;;; constructors

(define (sequence-concatenate bvs)
  (let ((result (make-bytevector (%total-length-of-bytevectors-list bvs))))
    (let loop ((i 0) (bvs bvs))
      (if (null? bvs)
	  result
	(let* ((s    (car bvs))
	       (slen (bytevector-length s)))
	  (%sequence-copy*! result i s 0 slen)
	  (loop (+ i slen) (cdr bvs)))))))

(define (sequence-append bv0 . bvs)
  (let-values (((port getter) (open-bytevector-output-port)))
    (put-bytevector port bv0)
    (for-each (lambda (bv)
		(put-bytevector port bv))
      bvs)
    (getter)))

(define (%sequence-concatenate-reverse bvs tail-bv past)
  (let* ((result.len	(%total-length-of-bytevectors-list bvs))
	 (result	(make-bytevector (+ past result.len))))
    (%sequence-copy*! result result.len tail-bv 0 past)
    (let loop ((i   result.len)
	       (bvs bvs))
      (if (null? bvs)
	  result
	(let* ((s    (car bvs))
	       (bvs  (cdr bvs))
	       (slen (bytevector-length s))
	       (i    (- i slen)))
	  (%sequence-copy*! result i s 0 slen)
	  (loop i bvs))))))

(define (sequence-tabulate proc len)
  (let ((s (make-bytevector len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0)
	 s)
      (sequence-set! s i (proc i)))))


;;;; predicates

(define (sequence-null? bv)
  (zero? (bytevector-length bv)))

(define (%sequence-every criterion bv start past)
  (and (< start past)
       (cond ((and (integer? criterion) (exact? criterion))
	      (let loop ((i start))
		(or (<= past i)
		    (and (= criterion (sequence-ref bv i))
			 (loop (+ 1 i))))))

	     ((char? criterion)
	      (let ((criterion (char->integer criterion)))
		(let loop ((i start))
		  (or (<= past i)
		      (and (= criterion (sequence-ref bv i))
			   (loop (+ 1 i)))))))

	     ((char-set? criterion)
	      (let loop ((i start))
		(or (<= past i)
		    (and (char-set-contains? criterion (integer->char (sequence-ref bv i)))
			 (loop (+ 1 i))))))

	     ((procedure? criterion)
	      ;;Slightly funky loop so  that final (PRED S[PAST-1]) call
	      ;;is a tail call.
	      (let loop ((i start))
		(let ((c  (sequence-ref bv i))
		      (i1 (+ i 1)))
		  (if (= i1 past)
		      (criterion c) ;this has to be a tail call
		    (and (criterion c) (loop i1))))))

	     (else
	      (assertion-violation '%sequence-every
		"expected char-set, char, or predicate as second parameter"
		criterion)))))

(define (%sequence-any criterion bv start past)
  (and (< start past)
       (cond ((and (integer? criterion) (exact? criterion))
	      (let loop ((i start))
		(and (< i past)
		     (or (= criterion (sequence-ref bv i))
			 (loop (+ i 1))))))

	     ((char? criterion)
	      (let ((criterion (char->integer criterion)))
		(let loop ((i start))
		  (and (< i past)
		       (or (= criterion (sequence-ref bv i))
			   (loop (+ i 1)))))))

	     ((char-set? criterion)
	      (let loop ((i start))
		(and (< i past)
		     (or (char-set-contains? criterion (integer->char (sequence-ref bv i)))
			 (loop (+ i 1))))))

	     ((procedure? criterion)
	      ;;Slightly funky loop so  that final (PRED S[PAST-1]) call
	      ;;is a tail call.
	      (let loop ((i start))
		(let ((c (sequence-ref bv i))
		      (i1 (+ i 1)))
		  (if (= i1 past)
		      (criterion c) ; This has to be a tail call.
		    (or (criterion c) (loop i1))))))

	     (else
	      (assertion-violation '%sequence-any
		"expected char-set, char, or predicate as second parameter"
		criterion)))))


;;;; lexicographic comparison

(define (%true-sequence-compare sequence-prefix-length-proc less-proc
				     bv1 start1 past1 bv2 start2 past2 proc< proc= proc>)
  (let ((size1 (- past1 start1))
	(size2 (- past2 start2))
	(match (sequence-prefix-length-proc bv1 start1 past1 bv2 start2 past2)))
    (if (= match size1)
	((if (= match size2) proc= proc<) past1)
      ((if (= match size2)
	   proc>
	 (if (less-proc (sequence-ref bv1 (+ start1 match))
			(sequence-ref bv2 (+ start2 match)))
	     proc< proc>))
       (+ match start1)))))

(define (%sequence-compare bv1 start1 past1 bv2 start2 past2 proc< proc= proc>)
  (%true-sequence-compare %sequence-prefix-length <
			       bv1 start1 past1 bv2 start2 past2 proc< proc= proc>))

(define (%sequence-compare-ci bv1 start1 past1 bv2 start2 past2 proc< proc= proc>)
  (%true-sequence-compare %sequence-prefix-length-ci %integer-char-ci<?
			       bv1 start1 past1 bv2 start2 past2 proc< proc= proc>))

;;; --------------------------------------------------------------------

(define (%true-sequence= sequence-compare-proc bv1 start1 past1 bv2 start2 past2)
  (and (= (- past1 start1) (- past2 start2))	 ; Quick filter
       (or (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
	   (sequence-compare-proc bv1 start1 past1 bv2 start2 past2 ; Real test
				       (lambda (i) #f) values (lambda (i) #f)))))

(define (%sequence= bv1 start1 past1 bv2 start2 past2)
  (%true-sequence= %sequence-compare bv1 start1 past1 bv2 start2 past2))

(define (%sequence-ci= bv1 start1 past1 bv2 start2 past2)
  (%true-sequence= %sequence-compare-ci bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-sequence<> sequence-compare-proc bv1 start1 past1 bv2 start2 past2)
  (or (not (= (- past1 start1) (- past2 start2)))      ; Fast path
      (and (not (and (eq? bv1 bv2) (= start1 start2))) ; Quick filter
	   (sequence-compare-proc bv1 start1 past1 bv2 start2 past2 ; Real test
				       values (lambda (i) #f) values))))

(define (%sequence<> bv1 start1 past1 bv2 start2 past2)
  (%true-sequence<> %sequence-compare bv1 start1 past1 bv2 start2 past2))

(define (%sequence-ci<> bv1 start1 past1 bv2 start2 past2)
  (%true-sequence<> %sequence-compare-ci bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-sequence< sequence-prefix-proc pred
			      bv1 start1 past1 bv2 start2 past2)
  (if (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
      (< past1 past2)
    ;;Notice that PRED is always the less-than one.
    (%true-sequence-compare sequence-prefix-proc pred ; Real test
				 bv1 start1 past1 bv2 start2 past2
				 values (lambda (i) #f) (lambda (i) #f))))

(define (%sequence< bv1 start1 past1 bv2 start2 past2)
  (%true-sequence< %sequence-prefix-length <
			bv1 start1 past1 bv2 start2 past2))

(define (%sequence-ci< bv1 start1 past1 bv2 start2 past2)
  (%true-sequence< %sequence-prefix-length-ci %integer-char-ci<?
			bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-sequence<= sequence-prefix-proc pred
			       bv1 start1 past1 bv2 start2 past2)
  (if (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
      (<= past1 past2)
    ;;Notice that PRED is always the less-than one.
    (%true-sequence-compare sequence-prefix-proc pred ; Real test
				 bv1 start1 past1 bv2 start2 past2
				 values values (lambda (i) #f))))

(define (%sequence<= bv1 start1 past1 bv2 start2 past2)
  (%true-sequence<= %sequence-prefix-length <=
			 bv1 start1 past1 bv2 start2 past2))

(define (%sequence-ci<= bv1 start1 past1 bv2 start2 past2)
  (%true-sequence<= %sequence-prefix-length-ci %integer-char-ci<=?
			 bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-sequence> sequence-prefix-proc pred bv1 start1 past1 bv2 start2 past2)
  (if (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
      (> past1 past2)
    ;;Notice that PRED is always the less-than one.
    (%true-sequence-compare sequence-prefix-proc pred ; Real test
				 bv1 start1 past1 bv2 start2 past2
				 (lambda (i) #f) (lambda (i) #f) values)))

(define (%sequence> bv1 start1 past1 bv2 start2 past2)
  (%true-sequence> %sequence-prefix-length <
			bv1 start1 past1 bv2 start2 past2))

(define (%sequence-ci> bv1 start1 past1 bv2 start2 past2)
  (%true-sequence> %sequence-prefix-length-ci %integer-char-ci<?
			bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%true-sequence>= sequence-prefix-proc pred bv1 start1 past1 bv2 start2 past2)
  (if (and (eq? bv1 bv2) (= start1 start2)) ; Fast path
      (>= past1 past2)
    ;;Notice that PRED is always the less-than one.
    (%true-sequence-compare sequence-prefix-proc pred ; Real test
				 bv1 start1 past1 bv2 start2 past2
				 (lambda (i) #f) values values)))

(define (%sequence>= bv1 start1 past1 bv2 start2 past2)
  (%true-sequence>= %sequence-prefix-length <=
			 bv1 start1 past1 bv2 start2 past2))

(define (%sequence-ci>= bv1 start1 past1 bv2 start2 past2)
  (%true-sequence>= %sequence-prefix-length-ci %integer-char-ci<=?
			 bv1 start1 past1 bv2 start2 past2))

;;; --------------------------------------------------------------------

(define (%full-sequence= a b)
  (%sequence= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-sequence< a b)
  (%sequence< a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-sequence<= a b)
  (%sequence<= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-sequence> a b)
  (%sequence> a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-sequence>= a b)
  (%sequence>= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-sequence-ci= a b)
  (%sequence-ci= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-sequence-ci< a b)
  (%sequence-ci< a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-sequence-ci<= a b)
  (%sequence-ci<= a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-sequence-ci> a b)
  (%sequence-ci> a 0 (bytevector-length a) b 0 (bytevector-length b)))

(define (%full-sequence-ci>= a b)
  (%sequence-ci>= a 0 (bytevector-length a) b 0 (bytevector-length b)))


;;;; dictionary comparison

(define (%true-sequence-dictionary-compare a b pred=? pred<? $lesser $equal $greater)
  ;;Compare the bytes  from bytevectors A and B  using PRED=? and PRED<?
  ;;and return $LESSER, $EQUAL  or $GREATER.  The comparison skips bytes
  ;;which, interpreted as ASCII, are blanks.
  ;;
  (let ((lena	(bytevector-length a))
	(lenb	(bytevector-length b)))
    (let loop ((i 0) (j 0))
      (cond ((= i lena)
	     (if (= j lenb)
		 $equal
	       $lesser))

	    ((= j lenb)
	     $greater)

	    (else
	     (let ((byte-a (sequence-ref a i))
		   (byte-b (sequence-ref b j)))
	       (cond ((memv byte-a $white-spaces-for-dictionary-comparison)
		      (loop (+ 1 i) j))

		     ((memv byte-b $white-spaces-for-dictionary-comparison)
		      (loop i (+ 1 j)))

		     ((pred=? byte-a byte-b)
		      (loop (+ 1 i) (+ 1 j)))

		     ((pred<? byte-a byte-b)
		      $lesser)

		     (else
		      $greater))))))))

;;; --------------------------------------------------------------------

(define (%sequence-dictionary-compare a b)
  (%true-sequence-dictionary-compare a b = < -1 0 +1))

(define (%sequence-dictionary=? a b)
  (%true-sequence-dictionary-compare a b = < #f #t #f))

(define (%sequence-dictionary<>? a b)
  (%true-sequence-dictionary-compare a b = < #t #f #t))

(define (%sequence-dictionary<? a b)
  (%true-sequence-dictionary-compare a b = < #t #f #f))

(define (%sequence-dictionary<=? a b)
  (%true-sequence-dictionary-compare a b = < #t #t #f))

(define (%sequence-dictionary>? a b)
  (%true-sequence-dictionary-compare a b = < #f #f #t))

(define (%sequence-dictionary>=? a b)
  (%true-sequence-dictionary-compare a b = < #f #t #t))

;;; --------------------------------------------------------------------

(define (%sequence-dictionary-compare-ci a b)
  (%true-sequence-dictionary-compare a b %integer-char-ci=? %integer-char-ci<?))

(define (%sequence-dictionary-ci=? a b)
  (%true-sequence-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #f #t #f))

(define (%sequence-dictionary-ci<>? a b)
  (%true-sequence-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #t #f #t))

(define (%sequence-dictionary-ci<? a b)
  (%true-sequence-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #t #f #f))

(define (%sequence-dictionary-ci<=? a b)
  (%true-sequence-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #t #t #f))

(define (%sequence-dictionary-ci>? a b)
  (%true-sequence-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #f #f #t))

(define (%sequence-dictionary-ci>=? a b)
  (%true-sequence-dictionary-compare a b %integer-char-ci=? %integer-char-ci<? #f #t #t))


;;;; sequence/numbers lexicographic comparison

(define $list-of-digits
  (map char->integer '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(define (%sequence/numbers->parts input-bv)
  ;;Split a bytevector string into the list of its parts.  Example:
  ;;
  ;;	"foo4bar3zab10"
  ;;
  ;;becomes:
  ;;
  ;;	("foo" ("4" . 4) "bar" ("3" . 3) "zab" ("10" . 10))
  ;;
  (let loop ((chars	(reverse (sequence->list input-bv)))
	     (str	'())
	     (num	'())
	     (parts	'()))
    (define (%accumulate-sequence-part)
      (if (null? str)
	  parts
	(cons (list->sequence str) parts)))
    (define (%accumulate-number-part)
      (if (null? num)
	  parts
	(let ((s (list->sequence num)))
	  (cons `(,s . ,(string->number (utf8->string s))) parts))))
    (cond ((null? chars)
	   (cond ((not (null? str))
		  (assert (null? num))
		  (%accumulate-sequence-part))
		 ((not (null? num))
		  (assert (null? str))
		  (%accumulate-number-part))
		 (else parts)))
	  ((memv (car chars) $list-of-digits)
	   (loop (cdr chars)
		 '()
		 (cons (car chars) num)
		 (%accumulate-sequence-part)))
	  (else
	   (loop (cdr chars)
		 (cons (car chars) str)
		 '()
		 (%accumulate-number-part))))))

(define (%true-sequence/numbers-compare a b sequence->parts
					     bytevector=? bytevector<? $lesser $equal $greater)
  (let loop ((a (sequence->parts a))
             (b (sequence->parts b)))
    (cond ((null? a)
	   (if (null? b) $equal $lesser))

	  ((null? b) $greater)

	  ((and (bytevector? (car a)) ;both strings
		(bytevector? (car b)))
	   (if (bytevector=? (car a) (car b))
	       (loop (cdr a) (cdr b))
	     (if (bytevector<? (car a) (car b))
		 $lesser
	       $greater)))

	  ((and (pair? (car a))	;both numbers
		(pair? (car b)))
	   (let ((num-a (cdar a))
		 (num-b (cdar b)))
	     (cond ((= num-a num-b)
		    (loop (cdr a) (cdr b)))
		   ((< num-a num-b)
		    $lesser)
		   (else
		    $greater))))

	  ((bytevector? (car a)) ;first string, second number
	   (if (bytevector<? (car a) (caar b))
	       $lesser
	     $greater))

	  (else	;first number, second string
	   (if (bytevector<? (caar a) (car b))
	       $lesser
	     $greater)))))

;;; --------------------------------------------------------------------

(define (%sequence/numbers-compare a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       bytevector=? %full-sequence< -1 0 +1))

(define (%sequence/numbers=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       bytevector=? %full-sequence< #f #t #f))

(define (%sequence/numbers<>? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       bytevector=? %full-sequence< #t #f #t))

(define (%sequence/numbers<? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       bytevector=? %full-sequence< #t #f #f))

(define (%sequence/numbers<=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       bytevector=? %full-sequence< #t #t #f))

(define (%sequence/numbers>? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       bytevector=? %full-sequence< #f #f #t))

(define (%sequence/numbers>=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       bytevector=? %full-sequence< #f #t #t))

;;; --------------------------------------------------------------------

(define (%sequence/numbers-compare-ci a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       %full-sequence-ci= %full-sequence-ci<))

(define (%sequence/numbers-ci=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       %full-sequence-ci= %full-sequence-ci< #f #t #f))

(define (%sequence/numbers-ci<>? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       %full-sequence-ci= %full-sequence-ci< #t #f #t))

(define (%sequence/numbers-ci<? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       %full-sequence-ci= %full-sequence-ci< #t #f #f))

(define (%sequence/numbers-ci<=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       %full-sequence-ci= %full-sequence-ci< #t #t #f))

(define (%sequence/numbers-ci>? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       %full-sequence-ci= %full-sequence-ci< #f #f #t))

(define (%sequence/numbers-ci>=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers->parts
				       %full-sequence-ci= %full-sequence-ci< #f #t #t))


;;;; sequence/numbers dictionary comparison

(define (%sequence/numbers-dictionary->parts input-string)
  ;;Split a  string into  the list of  its parts, discard  white spaces.
  ;;Example:
  ;;
  ;;	"foo4 bar3   zab10"
  ;;
  ;;becomes:
  ;;
  ;;	("foo" ("4" . 4) "bar" ("3" . 3) "zab" ("10" . 10))
  ;;
  (let loop ((chars	(reverse (sequence->list input-string)))
	     (str	'())
	     (num	'())
	     (parts	'()))
    (define (%accumulate-sequence-part)
      (if (null? str)
	  parts
	(cons (list->sequence str) parts)))
    (define (%accumulate-number-part)
      (if (null? num)
	  parts
	(let ((s (list->sequence num)))
	  (cons `(,s . ,(string->number (utf8->string s))) parts))))
    (cond ((null? chars)
	   (cond ((not (null? str))
		  (assert (null? num))
		  (%accumulate-sequence-part))
		 ((not (null? num))
		  (assert (null? str))
		  (%accumulate-number-part))
		 (else parts)))
	  ((memv (car chars) $list-of-digits)
	   (loop (cdr chars)
		 '()
		 (cons (car chars) num)
		 (%accumulate-sequence-part)))
	  ((memv (car chars) $white-spaces-for-dictionary-comparison)
	   (loop (cdr chars) str num parts))
	  (else
	   (loop (cdr chars)
		 (cons (car chars) str)
		 '()
		 (%accumulate-number-part))))))

;;; --------------------------------------------------------------------

(define (%sequence/numbers-dictionary-compare a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       bytevector=? %full-sequence< -1 0 +1))

(define (%sequence/numbers-dictionary=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       bytevector=? %full-sequence< #f #t #f))

(define (%sequence/numbers-dictionary<>? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       bytevector=? %full-sequence< #t #f #t))

(define (%sequence/numbers-dictionary<? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       bytevector=? %full-sequence< #t #f #f))

(define (%sequence/numbers-dictionary<=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       bytevector=? %full-sequence< #t #t #f))

(define (%sequence/numbers-dictionary>? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       bytevector=? %full-sequence< #f #f #t))

(define (%sequence/numbers-dictionary>=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       bytevector=? %full-sequence< #f #t #t))

;;; --------------------------------------------------------------------

(define (%sequence/numbers-dictionary-compare-ci a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       %full-sequence-ci= %full-sequence-ci<))

(define (%sequence/numbers-dictionary-ci=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       %full-sequence-ci= %full-sequence-ci< #f #t #f))

(define (%sequence/numbers-dictionary-ci<>? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       %full-sequence-ci= %full-sequence-ci< #t #f #t))

(define (%sequence/numbers-dictionary-ci<? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       %full-sequence-ci= %full-sequence-ci< #t #f #f))

(define (%sequence/numbers-dictionary-ci<=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       %full-sequence-ci= %full-sequence-ci< #t #t #f))

(define (%sequence/numbers-dictionary-ci>? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       %full-sequence-ci= %full-sequence-ci< #f #f #t))

(define (%sequence/numbers-dictionary-ci>=? a b)
  (%true-sequence/numbers-compare a b %sequence/numbers-dictionary->parts
				       %full-sequence-ci= %full-sequence-ci< #f #t #t))


;;;; mapping

(define (sequence-map proc bv0 . bvs)
  (let ((bvs (cons bv0 bvs)))
    (%assert-bytevectors-of-same-length 'sequence-map bvs)
    (let* ((len     (bytevector-length bv0))
	   (result  (make-bytevector len)))
      (do ((i 0 (+ 1 i)))
	  ((= len i)
	   result)
	(sequence-set! result i
			    (apply proc i (map (lambda (bv) (sequence-ref bv i))
					    bvs)))))))

(define (sequence-map! proc bv0 . bvs)
  (let ((bvs (cons bv0 bvs)))
    (%assert-bytevectors-of-same-length 'sequence-map bvs)
    (let ((len (bytevector-length bv0)))
      (do ((i 0 (+ 1 i)))
	  ((= len i))
	(sequence-set! bv0 i
			    (apply proc i (map (lambda (bv) (sequence-ref bv i))
					    bvs)))))))

(define (sequence-map* proc bv0 . bvs)
  (let* ((bvs (cons bv0 bvs))
	 (len (bvs-list-min-length bvs)))
    (do ((i 0 (+ 1 i))
	 (result (make-bytevector len)))
	((= len i)
	 result)
      (sequence-set! result i
			  (apply proc i (map (lambda (bv) (sequence-ref bv i))
					  bvs))))))

(define (sequence-map*! proc bv0 . bvs)
  (let* ((bvs  (cons bv0 bvs))
	 (len      (bvs-list-min-length bvs)))
    (do ((i 0 (+ 1 i)))
	((= len i))
      (sequence-set! bv0 i
			  (apply proc i (map (lambda (bv) (sequence-ref bv i))
					  bvs))))))

(define (sequence-for-each* proc bv0 . bvs)
  (let* ((bvs  (cons bv0 bvs))
	 (len      (bvs-list-min-length bvs)))
    (do ((i 0 (+ 1 i)))
	((= len i))
      (apply proc i (map (lambda (bv) (sequence-ref bv i))
		      bvs)))))

;;; --------------------------------------------------------------------

(define (%subsequence-map proc bv start past)
  (do ((i start (+ 1 i))
       (j 0 (+ 1 j))
       (result (make-bytevector (- past start))))
      ((>= i past)
       result)
    (sequence-set! result j (proc (sequence-ref bv i)))))

(define (%subsequence-map! proc bv start past)
  (do ((i start (+ 1 i)))
      ((>= i past)
       bv)
    (sequence-set! bv i (proc (sequence-ref bv i)))))

(define (%subsequence-for-each proc bv start past)
  (let loop ((i start))
    (when (< i past)
      (proc (sequence-ref bv i))
      (loop (+ i 1)))))

(define (%subsequence-for-each-index proc bv start past)
  (let loop ((i start))
    (when (< i past)
      (proc i)
      (loop (+ i 1)))))


;;;; case hacking

(define (byte-cased? c)
  ;;This works  because CHAR-UPCASE returns  #f if the character  has no
  ;;upcase version.
  (char-upper-case? (char-upcase (integer->char c))))

(define (%sequence-titlecase*! bv start past)
  (let loop ((i start))
    (cond ((%sequence-index byte-cased? bv i past)
	   => (lambda (i)
		(sequence-set! bv i
				    (char->integer
				     (char-titlecase
				      (integer->char (sequence-ref bv i)))))
		(let ((i1 (+ i 1)))
		  (cond ((%sequence-skip byte-cased? bv i1 past)
			 => (lambda (j)
			      (%subsequence-map! (lambda (b)
							(char->integer
							 (char-downcase (integer->char b))))
						      bv i1 j)
			      (loop (+ j 1))))
			(else
			 (%subsequence-map! (lambda (b)
						   (char->integer (char-downcase (integer->char b))))
						 bv i1 past)))))))))


;;;; folding

(define (sequence-fold-left kons knil vec0 . bvs)
  (let ((bvs (cons vec0 bvs)))
    (%assert-bytevectors-of-same-length 'sequence-fold-left bvs)
    (let ((len (bytevector-length vec0)))
      (let loop ((i 0) (knil knil))
	(if (= i len)
	    knil
	  (loop (+ 1 i) (apply kons i knil
			       (map (lambda (vec)
				      (sequence-ref vec i))
				 bvs))))))))

(define (sequence-fold-right kons knil vec0 . bvs)
  (let* ((bvs (cons vec0 bvs)))
    (%assert-bytevectors-of-same-length 'sequence-fold-right bvs)
    (let ((len (bvs-list-min-length bvs)))
      (let loop ((i (- len 1)) (knil knil))
	(if (< i 0)
	    knil
	  (loop (- i 1) (apply kons i knil
			       (map (lambda (vec)
				      (sequence-ref vec i))
				 bvs))))))))

(define (sequence-fold-left* kons knil vec0 . bvs)
  (let* ((bvs (cons vec0 bvs))
	 (len (bvs-list-min-length bvs)))
    (let loop ((i 0) (knil knil))
      (if (= len i)
	  knil
	(loop (+ 1 i) (apply kons i knil
			     (map (lambda (vec)
				    (sequence-ref vec i))
			       bvs)))))))

(define (sequence-fold-right* kons knil vec0 . bvs)
  (let* ((bvs (cons vec0 bvs))
	 (len (bvs-list-min-length bvs)))
    (let loop ((i (- len 1)) (knil knil))
      (if (< i 0)
	  knil
	(loop (- i 1) (apply kons i knil
			     (map (lambda (vec)
				    (sequence-ref vec i))
			       bvs)))))))

(define (%subsequence-fold-left kons knil bv start past)
  (let loop ((v knil) (i start))
    (if (< i past)
	(loop (kons (sequence-ref bv i) v) (+ i 1))
      v)))

(define (%subsequence-fold-right kons knil bv start past)
  (let loop ((v knil) (i (- past 1)))
    (if (>= i start)
	(loop (kons (sequence-ref bv i) v) (- i 1))
      v)))

(define sequence-unfold
  (case-lambda
   ((p f g seed)
    (sequence-unfold p f g seed '#vu8() (lambda (x) '#vu8())))
   ((p f g seed base)
    (sequence-unfold p f g seed base (lambda (x) '#vu8())))
   ((p f g seed base make-final)
    ;;The strategy is to allocate a series of chunks into which we stash
    ;;the bytes as we generate them. Chunk size goes up in powers of two
    ;;beginning with 40 and levelling out at 4k, i.e.
    ;;
    ;;	40 40 80 160 320 640 1280 2560 4096 4096 4096 4096 4096...
    ;;
    ;;This  should work  pretty well  for short  bytevectors  and longer
    ;;ones.  When  done, we allocate  an answer bytevector and  copy the
    ;;chars over from the chunk buffers.
    (let lp ((chunks '())		  ;previously filled chunks
	     (nchars 0)			  ;number of chars in CHUNKS
	     (chunk (make-bytevector 40)) ;current chunk into which we write
	     (chunk-len 40)
	     (i 0) ;number of chars written into CHUNK
	     (seed seed))
      (let lp2 ((i i) (seed seed))
	(if (not (p seed))
	    (let ((c (f seed))
		  (seed (g seed)))
	      (if (< i chunk-len)
		  (begin (sequence-set! chunk i c)
			 (lp2 (+ i 1) seed))

		(let* ((nchars2 (+ chunk-len nchars))
		       (chunk-len2 (min 4096 nchars2))
		       (new-chunk (make-bytevector chunk-len2)))
		  (sequence-set! new-chunk 0 c)
		  (lp (cons chunk chunks) (+ nchars chunk-len)
		      new-chunk chunk-len2 1 seed))))

	  ;;Done, make the answer bytevector and install the bits.
	  (let* ((final    (make-final seed))
		 (flen     (bytevector-length final))
		 (base-len (bytevector-length base))
		 (j        (+ base-len nchars i))
		 (ans      (make-bytevector (+ j flen))))
	    (%sequence-copy*! ans j final 0 flen) ; Install FINAL.
	    (let ((j (- j i)))
	      (%sequence-copy*! ans j chunk 0 i) ; Install CHUNK[0,I).
	      (let lp ((j j) (chunks chunks))	      ; Install CHUNKS.
		(if (pair? chunks)
		    (let* ((chunk  (car chunks))
			   (chunks (cdr chunks))
			   (chunk-len (bytevector-length chunk))
			   (j (- j chunk-len)))
		      (%sequence-copy*! ans j chunk 0 chunk-len)
		      (lp j chunks)))))
	    (%sequence-copy*! ans 0 base 0 base-len) ; Install BASE.
	    ans)))))))

(define sequence-unfold-right
  (case-lambda
   ((p f g seed)
    (sequence-unfold-right p f g seed '#vu8() (lambda (x) '#vu8())))
   ((p f g seed base)
    (sequence-unfold-right p f g seed base (lambda (x) '#vu8())))
   ((p f g seed base make-final)
    (let lp ((chunks '())	      ; Previously filled chunks
	     (nchars 0)		      ; Number of chars in CHUNKS
	     (chunk (make-bytevector 40)) ; Current chunk into which we write
	     (chunk-len 40)
	     (i 40) ; Number of chars available in CHUNK
	     (seed seed))
      (let lp2 ((i i) (seed seed)) ; Fill up CHUNK from right
	(if (not (p seed))	   ; to left.
	    (let ((c (f seed))
		  (seed (g seed)))
	      (if (> i 0)
		  (let ((i (- i 1)))
		    (sequence-set! chunk i c)
		    (lp2 i seed))

		(let* ((nchars2 (+ chunk-len nchars))
		       (chunk-len2 (min 4096 nchars2))
		       (new-chunk (make-bytevector chunk-len2))
		       (i (- chunk-len2 1)))
		  (sequence-set! new-chunk i c)
		  (lp (cons chunk chunks) (+ nchars chunk-len)
		      new-chunk chunk-len2 i seed))))

	  ;; We're done. Make the answer sequence & install the bits.
	  (let* ((final (make-final seed))
		 (flen (bytevector-length final))
		 (base-len (bytevector-length base))
		 (chunk-used (- chunk-len i))
		 (j (+ base-len nchars chunk-used))
		 (ans (make-bytevector (+ j flen))))
	    (%sequence-copy*! ans 0 final 0 flen)	       ; Install FINAL.
	    (%sequence-copy*! ans flen chunk i chunk-len) ; Install CHUNK[I,).
	    (let lp ((j (+ flen chunk-used))	       ; Install CHUNKS.
		     (chunks chunks))
	      (if (pair? chunks)
		  (let* ((chunk  (car chunks))
			 (chunks (cdr chunks))
			 (chunk-len (bytevector-length chunk)))
		    (%sequence-copy*! ans j chunk 0 chunk-len)
		    (lp (+ j chunk-len) chunks))
		(%sequence-copy*! ans j base 0 base-len)))	; Install BASE.
	    ans)))))))


;;;; selecting

(define (%assert-enough-chars who nchars start past)
  (unless (<= nchars (- past start))
    (assertion-violation who
      "requested number of chars greater than length of bytevector" nchars start past)))

(define (%sequence-reverse-copy* bv start past)
  (let ((result (make-bytevector (- past start))))
    (do ((i (- past 1) (- i 1))
	 (j 0 (+ j 1)))
	((< i start)
	 result)
      (sequence-set! result j (sequence-ref bv i)))))

(define (%sequence-take nchars bv start past)
  (%assert-enough-chars '%sequence-take nchars start past)
  (subsequence bv start (+ start nchars)))

(define (%sequence-take-right nchars bv start past)
  (%assert-enough-chars '%sequence-take-right nchars start past)
  (subsequence bv (- past nchars) past))

(define (%sequence-drop nchars bv start past)
  (%assert-enough-chars '%sequence-drop nchars start past)
  (subsequence bv nchars past))

(define (%sequence-drop-right nchars bv start past)
  (%assert-enough-chars '%sequence-drop-right nchars start past)
  (subsequence bv start (+ start nchars)))

(define (%sequence-trim criterion bv start past)
  (cond ((%sequence-skip criterion bv start past)
	 => (lambda (i) (subsequence bv i past)))
	(else '#vu8())))

(define (%sequence-trim-right criterion bv start past)
  (cond ((%sequence-skip-right criterion bv start past)
	 => (lambda (i)
	      (subsequence bv start (+ 1 i))))
	(else '#vu8())))

(define (%sequence-trim-both criterion bv start past)
  (let ((bv (%sequence-trim-right criterion bv start past)))
    (%sequence-trim criterion bv start (bytevector-length bv))))

(define (%sequence-pad requested-len fill-char bv start past)
  (let ((len  (- past start))
	(fill-char (if (char? fill-char)
		       (char->integer fill-char)
		     fill-char)))
    (if (<= requested-len len)
	(subsequence bv (- past requested-len) past)
      (let ((result (make-bytevector requested-len fill-char)))
	(%sequence-copy*! result (- requested-len len) bv start past)
	result))))

(define (%sequence-pad-right requested-len fill-char bv start past)
  (let ((len (- past start))
	(fill-char (if (char? fill-char)
		       (char->integer fill-char)
		     fill-char)))
    (if (<= requested-len len)
	(subsequence bv start (+ start requested-len))
      (let ((result (make-bytevector requested-len fill-char)))
	(%sequence-copy*! result 0 bv start past)
	result))))


;;;; prefix and suffix

(define (%true-sequence-prefix-length byte-cmp? bv1 start1 past1 bv2 start2 past2)
  ;;Find the length  of the common prefix.  It is  not required that the
  ;;two subbytevector passed be of equal length.
  ;;
  (let* ((delta (min (- past1 start1) (- past2 start2)))
	 (past1 (+ start1 delta)))
    (if (and (eq? bv1 bv2) (= start1 start2)) ; EQ fast path
	delta
      (let lp ((i start1) (j start2)) ; Regular path
	(if (or (>= i past1)
		(not (byte-cmp? (sequence-ref bv1 i)
				(sequence-ref bv2 j))))
	    (- i start1)
	  (lp (+ i 1) (+ j 1)))))))

(define (%sequence-prefix-length bv1 start1 past1 bv2 start2 past2)
  (%true-sequence-prefix-length = bv1 start1 past1 bv2 start2 past2))

(define (%sequence-prefix-length-ci bv1 start1 past1 bv2 start2 past2)
  (%true-sequence-prefix-length %integer-char-ci=? bv1 start1 past1 bv2 start2 past2))

(define (%sequence-prefix? bv1 start1 past1 bv2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%sequence-prefix-length bv1 start1 past1
					       bv2 start2 past2)))))

(define (%sequence-prefix-ci? bv1 start1 past1 bv2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%sequence-prefix-length-ci bv1 start1 past1
						  bv2 start2 past2)))))

;;; --------------------------------------------------------------------

(define (%true-sequence-suffix-length byte-cmp? bv1 start1 past1 bv2 start2 past2)
  ;;Find the length  of the common suffix.  It is  not required that the
  ;;two subsequences passed be of equal length.
  ;;
  (let* ((delta (min (- past1 start1) (- past2 start2)))
	 (start1 (- past1 delta)))
    (if (and (eq? bv1 bv2) (= past1 past2)) ; EQ fast path
	delta
      (let lp ((i (- past1 1)) (j (- past2 1))) ; Regular path
	(if (or (< i start1)
		(not (byte-cmp? (sequence-ref bv1 i)
				(sequence-ref bv2 j))))
	    (- (- past1 i) 1)
	  (lp (- i 1) (- j 1)))))))

(define (%sequence-suffix-length bv1 start1 past1 bv2 start2 past2)
  (%true-sequence-suffix-length = bv1 start1 past1 bv2 start2 past2))

(define (%sequence-suffix-length-ci bv1 start1 past1 bv2 start2 past2)
  (%true-sequence-suffix-length %integer-char-ci=? bv1 start1 past1 bv2 start2 past2))

(define (%sequence-suffix? bv1 start1 past1 bv2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%sequence-suffix-length bv1 start1 past1
					       bv2 start2 past2)))))

(define (%sequence-suffix-ci? bv1 start1 past1 bv2 start2 past2)
  (let ((len1 (- past1 start1)))
    (and (<= len1 (- past2 start2)) ; Quick check
	 (= len1 (%sequence-suffix-length-ci bv1 start1 past1
						  bv2 start2 past2)))))


;;;; searching

(define (%sequence-index criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%sequence-index (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%sequence-index (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%sequence-index (lambda (byte)
				 (char-set-contains? criterion (integer->char byte)))
			       bv start past))
	((procedure? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (criterion (sequence-ref bv i))
		    i
		  (loop (+ 1 i))))))
	(else
	 (assertion-violation '%sequence-index
	   "expected char-set, integer, char or predicate as criterion" criterion))))

(define (%sequence-index-right criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%sequence-index-right (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%sequence-index-right (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%sequence-index-right (lambda (byte)
				       (char-set-contains? criterion (integer->char byte)))
			       bv start past))
	((procedure? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (criterion (sequence-ref bv i))
		    i
		  (loop (- i 1))))))
	(else
	 (assertion-violation '%sequence-index-right
	   "expected char-set, integer, char or predicate as criterion" criterion))))

(define (%sequence-skip criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%sequence-skip (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%sequence-skip (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%sequence-skip (lambda (byte)
				(char-set-contains? criterion (integer->char byte)))
			      bv start past))
	((procedure? criterion)
	 (let loop ((i start))
	   (and (< i past)
		(if (criterion (sequence-ref bv i))
		    (loop (+ i 1))
		  i))))
	(else
	 (assertion-violation '%sequence-skip
	   "expected char-set, integer, char or predicate as criterion" criterion))))

(define (%sequence-skip-right criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%sequence-skip-right (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%sequence-skip-right (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%sequence-skip-right (lambda (byte)
				      (char-set-contains? criterion (integer->char byte)))
				    bv start past))
	((procedure? criterion)
	 (let loop ((i (- past 1)))
	   (and (>= i start)
		(if (criterion (sequence-ref bv i))
		    (loop (- i 1))
		  i))))
	(else
	 (assertion-violation '%sequence-skip-right
	   "expected char-set, integer, char or predicate as criterion" criterion))))

(define (%sequence-count criterion bv start past)
  (cond ((and (integer? criterion) (exact? criterion))
	 (%sequence-count (lambda (byte) (= byte criterion)) bv start past))
	((char? criterion)
	 (%sequence-count (char->integer criterion) bv start past))
	((char-set? criterion)
	 (%sequence-count (lambda (byte)
				 (char-set-contains? criterion (integer->char byte)))
			       bv start past))
	((procedure? criterion)
	 (do ((i start (+ i 1))
	      (count 0 (if (criterion (sequence-ref bv i))
			   (+ 1 count)
			 count)))
	     ((>= i past)
	      count)))
	(else
	 (assertion-violation '%sequence-count
	   "expected char-set, char or predicate as criterion" criterion))))

(define (%sequence-contains bv bv-start bv-past pattern pattern-start pattern-past)
  (%kmp-search = sequence-ref
	       bv bv-start bv-past
	       pattern pattern-start pattern-past))

(define (%sequence-contains-ci bv bv-start bv-past pattern pattern-start pattern-past)
  (%kmp-search %integer-char-ci=?
	       sequence-ref
	       bv bv-start bv-past
	       pattern pattern-start pattern-past))


;;;; filtering

(define (%sequence-filter criterion bv start past)
  (let-values (((port getter) (open-bytevector-output-port)))
    (cond ((procedure? criterion)
	   (do ((i start (+ 1 i)))
	       ((= i past)
		(getter))
	     (let ((byte (sequence-ref bv i)))
	       (when (criterion byte)
		 (put-u8 port byte)))))
	  ((and (integer? criterion) (exact? criterion))
	   (%sequence-filter (lambda (byte)
				    (= byte criterion))
				  bv start past))
	  ((char? criterion)
	   (%sequence-filter (char->integer criterion) bv start past))
	  ((char-set? criterion)
	   (%sequence-filter (lambda (byte)
				    (char-set-contains? criterion (integer->char byte)))
				  bv start past))
	  (else
	   (assertion-violation '%sequence-filter
	     "expected predicate, integer, char or char-set as criterion" criterion)))))

(define (%sequence-delete criterion bv start past)
  (let-values (((port getter) (open-bytevector-output-port)))
    (cond ((procedure? criterion)
	   (do ((i start (+ 1 i)))
	       ((= i past)
		(getter))
	     (let ((byte (sequence-ref bv i)))
	       (unless (criterion byte)
		 (put-u8 port byte)))))
	  ((and (integer? criterion) (exact? criterion))
	   (%sequence-delete (lambda (byte)
				    (= byte criterion))
				  bv start past))
	  ((char? criterion)
	   (%sequence-delete (char->integer criterion) bv start past))
	  ((char-set? criterion)
	   (%sequence-delete (lambda (byte)
				    (char-set-contains? criterion (integer->char byte)))
				  bv start past))
	  (else
	   (assertion-violation '%sequence-delete
	     "expected predicate, integer, char or char-set as criterion" criterion)))))


;;;; bytevectors and lists

(define (reverse-list->sequence clist)
  (let* ((len (length clist))
	 (s   (make-bytevector len)))
    (do ((i (- len 1) (- i 1))
	 (clist clist (cdr clist)))
	((not (pair? clist))
	 s)
      (sequence-set! s i (car clist)))))

(define (%reverse-sequence->list bv start past)
  (let loop ((i start) (result '()))
    (if (= i past)
	result
      (loop (+ 1 i) (cons (sequence-ref bv i) result)))))

(define (%sequence->list* bv start past)
  (do ((i (- past 1) (- i 1))
       (result '() (cons (sequence-ref bv i) result)))
      ((< i start)
       result)))

(define (%sequence-join bvs delim grammar)
  (define (join-with-delim ell final)
    (let loop ((ell ell))
      (if (pair? ell)
	  (cons delim (cons (car ell) (loop (cdr ell))))
	final)))
  (cond ((pair? bvs)
	 (sequence-concatenate
	  (case grammar
	    ((infix strict-infix)
	     (cons (car bvs)
		   (join-with-delim (cdr bvs) '())))
	    ((prefix)
	     (join-with-delim bvs '()))
	    ((suffix)
	     (cons (car bvs)
		   (join-with-delim (cdr bvs) (list delim))))
	    (else
	     (assertion-violation '%sequence-join
	       "illegal join grammar" grammar)))))

	((not (null? bvs))
	 (assertion-violation '%sequence-join
	   "BVS parameter is not a list" bvs))

	;; here we know that BVS is the empty list
	((eq? grammar 'strict-infix)
	 (assertion-violation '%sequence-join
	   "empty list cannot be joined with STRICT-INFIX grammar."))

	(else '#vu8()))) ; Special-cased for infix grammar.

(define (%sequence-tokenize token-set bv start past)
  (let loop ((i past) (result '()))
    (cond ((and (< start i)
		(%sequence-index-right token-set bv start i))
	   => (lambda (tpast-1)
		(let ((tpast (+ 1 tpast-1)))
		  (cond ((%sequence-skip-right token-set bv start tpast-1)
			 => (lambda (tstart-1)
			      (loop tstart-1
				    (cons (subsequence bv (+ 1 tstart-1) tpast)
					  result))))
			(else
			 (cons (subsequence bv start tpast) result))))))
	  (else result))))


;;;; subvector functions

(define (subsequence src start past)
  (let ((src-len	(bytevector-length src))
	(dst-len	(- past start)))
    (cond ((zero? dst-len) '#vu8())
	  ((zero? src-len)
	   (assertion-violation 'subsequence "cannot replicate empty (sub)bytevector"))
	  (else
	   (let ((dst (make-bytevector dst-len)))
	     (do ((i 0 (+ 1 i))
		  (j start (+ 1 j)))
		 ((= j past)
		  dst)
	       (sequence-set! dst i (sequence-ref src j))))))))

(define (%xsubsequence from to bv start past)
  (let ((bv-len	(- past start))
	(result-len	(- to from)))
    (cond ((zero? result-len) '#vu8())
	  ((zero? bv-len)
	   (assertion-violation '%xsubsequence "cannot replicate empty (sub)sequence"))
	  ((= 1 bv-len)
	   (make-bytevector result-len (sequence-ref bv start)))

	  ;; Selected text falls entirely within one span.
	  ((= (floor (/ from bv-len)) (floor (/ to bv-len)))
	   (subsequence bv
		      (+ start (mod from bv-len))
		      (+ start (mod to   bv-len))))

	  ;; Selected text requires multiple spans.
	  (else
	   (let ((result (make-bytevector result-len)))
	     (%multispan-repcopy! from to result 0 bv start past)
	     result)))))

(define (%sequence-xcopy! from to
			dst-bv dst-start dst-past
			src-bv src-start src-past)
  (let* ((tocopy	(- to from))
	 (tend		(+ dst-start tocopy))
	 (bv-len	(- src-past src-start)))
    (cond ((zero? tocopy))
	  ((zero? bv-len)
	   (assertion-violation '%sequence-xcopy! "cannot replicate empty (sub)sequence"))

	  ((= 1 bv-len)
	   (%sequence-fill*! dst-bv (sequence-ref src-bv src-start) dst-start dst-past))

	  ;; Selected text falls entirely within one span.
	  ((= (floor (/ from bv-len)) (floor (/ to bv-len)))
	   (%sequence-copy*! dst-bv dst-start src-bv
			  (+ src-start (mod from bv-len))
			  (+ src-start (mod to   bv-len))))

	  (else
	   (%multispan-repcopy! from to dst-bv dst-start src-bv src-start src-past)))))

(define (%multispan-repcopy! from to dst-bv dst-start src-bv src-start src-past)
  ;;This  is the  core  copying loop  for  XSUBSEQUENCE and  SEQUENCE-XCOPY!
  ;;Internal -- not exported, no careful arg checking.
  (let* ((bv-len	(- src-past src-start))
	 (i0		(+ src-start (mod from bv-len)))
	 (total-chars	(- to from)))

    ;; Copy the partial span @ the beginning
    (%sequence-copy*! dst-bv dst-start src-bv i0 src-past)

    (let* ((ncopied (- src-past i0))	   ; We've copied this many.
	   (nleft (- total-chars ncopied)) ; # chars left to copy.
	   (nspans (div nleft bv-len)))   ; # whole spans to copy

      ;; Copy the whole spans in the middle.
      (do ((i (+ dst-start ncopied) (+ i bv-len)) ; Current target index.
	   (nspans nspans (- nspans 1)))	   ; # spans to copy
	  ((zero? nspans)
	   ;; Copy the partial-span @ the end & we're done.
	   (%sequence-copy*! dst-bv i src-bv src-start
				  (+ src-start (- total-chars (- i dst-start)))))

	(%sequence-copy*! dst-bv i src-bv src-start src-past))))) ; Copy a whole span.


;;;; reverse, replace

(define (%sequence-reverse bv start past)
  (let* ((len (- past start))
	 (result (make-bytevector len)))
    (do ((i start (+ i 1))
	 (j (- len 1) (- j 1)))
	((< j 0)
	 result)
      (sequence-set! result j (sequence-ref bv i)))))

(define (%sequence-replace bv1 start1 past1 bv2 start2 past2)
  (let* ((len1		(bytevector-length bv1))
	 (len2		(- past2 start2))
	 (result	(make-bytevector (+ len2 (- len1 (- past1 start1))))))
    (%sequence-copy*! result 0 bv1 0 start1)
    (%sequence-copy*! result start1 bv2 start2 past2)
    (%sequence-copy*! result (+ start1 len2) bv1 past1 len1)
    result))

(define (%sequence-reverse! bv start past)
  (do ((i (- past 1) (- i 1))
       (j start (+ j 1)))
      ((<= i j))
    (let ((ci (sequence-ref bv i)))
      (sequence-set! bv i (sequence-ref bv j))
      (sequence-set! bv j ci))))


;;;; mutating

(define (%sequence-copy*! dst-bv dst-start src-bv src-start src-past)
  (when (< (- (bytevector-length dst-bv) dst-start)
	   (- src-past src-start))
    (assertion-violation '%sequence-copy*!
      "not enough room in destination bytevector"))
  (if (> src-start dst-start)
      (do ((i src-start (+ i 1))
	   (j dst-start (+ j 1)))
	  ((>= i src-past))
	(sequence-set! dst-bv j (sequence-ref src-bv i)))
    (do ((i (- src-past 1)                    (- i 1))
	 (j (+ -1 dst-start (- src-past src-start)) (- j 1)))
	((< i src-start))
      (sequence-set! dst-bv j (sequence-ref src-bv i)))))

(define (%sequence-reverse-copy*! dst-bv dst-start src-bv src-start src-past)
  (when (< (- (bytevector-length dst-bv) dst-start)
	   (- src-past src-start))
    (assertion-violation '%sequence-reverse-copy*!
      "not enough room in destination sequence"))
  ;;We must handle  correctly copying over the same  bytevector.  If the
  ;;source and  destination bytevectors  are the same,  we copy  all the
  ;;elements  in a  temporary  buffer first;  this  should be  optimised
  ;;someway to reduce to the minimum the size of the buffer.
  (if (eq? src-bv dst-bv)
      (when (< src-start src-past)
	(let* ((buffer (%sequence-reverse-copy* src-bv src-start src-past)))
	  (%sequence-copy*! dst-bv dst-start buffer 0 (bytevector-length buffer))))
    (do ((i (- src-past 1) (- i 1))
	 (j dst-start (+ j 1)))
	((< i src-start))
      (sequence-set! dst-bv j (sequence-ref src-bv i)))))

(define (%sequence-fill*! fill-char bv start past)
  (let ((fill-char (if (char? fill-char)
		       (char->integer fill-char)
		     fill-char)))
    (do ((i (- past 1) (- i 1)))
	((< i start))
      (sequence-set! bv i fill-char))))

(define (sequence-swap! bv i j)
  (when (zero? (bytevector-length bv))
    (assertion-violation 'sequence-swap!
      "attempt to swap elements in an empty bytevector"))
  (when (not (= i j))
    (let ((x (sequence-ref bv i)))
      (sequence-set! bv i (sequence-ref bv j))
      (sequence-set! bv j x))))


;;;; done

))

;;; end of file
