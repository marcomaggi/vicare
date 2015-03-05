;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: bytevectors functions
;;;Date: Sat Jun 26, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers bytevectors generic)
  (export instantiate-body)
  (import (only (vicare containers auxiliary-syntaxes)
		define-instantiable-body))

  (define-instantiable-body instantiate-body


;;;; predicates

(define-syntax sequence-every
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-every ?proc str beg past)))))

(define-syntax sequence-any
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-any ?proc str beg past)))))


;;;; comparison

(define-syntax sequence-compare
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-compare str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))

(define-syntax sequence-compare-ci
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-compare-ci str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))

;;; --------------------------------------------------------------------

(define-syntax sequence=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence= str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence<>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence<> str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence<
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence< str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence> str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence<=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence<= str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence>=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence>= str1 start1 end1 str2 start2 end2)))))

;;; --------------------------------------------------------------------

(define-syntax sequence-ci=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-ci= str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence-ci<>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-ci<> str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence-ci<
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-ci< str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence-ci>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-ci> str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence-ci<=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-ci<= str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence-ci>=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-ci>= str1 start1 end1 str2 start2 end2)))))


;;;; mapping

(define-syntax subsequence-map
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-map ?proc str beg past)))))

(define-syntax subsequence-map!
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-map! ?proc str beg past)))))

(define-syntax subsequence-for-each
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-for-each ?proc str beg past)))))

(define-syntax subsequence-for-each-index
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-for-each-index ?proc str beg past)))))


;;;; case hacking

(define-syntax sequence-upcase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-map ascii-upcase str beg past)))))

(define-syntax sequence-upcase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-map! ascii-upcase str beg past)))))

(define-syntax sequence-downcase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-map ascii-downcase str beg past)))))

(define-syntax sequence-downcase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-map! ascii-downcase str beg past)))))

(define-syntax sequence-titlecase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (let ((ans (subsequence str beg past)))
	 (%sequence-titlecase*! ans 0 (- past beg))
	 ans)))))

(define-syntax sequence-titlecase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-titlecase*! str beg past)))))


;;;; folding

(define-syntax subsequence-fold-left
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-fold-left ?kons ?knil str beg past)))))

(define-syntax subsequence-fold-right
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%subsequence-fold-right ?kons ?knil str beg past)))))


;;;; selecting

(define-syntax subsequence*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (subsequence str beg past)))))

(define-syntax sequence-reverse-copy*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-reverse-copy* str beg past)))))

(define-syntax sequence-copy*!
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-copy*! str1 beg1 str2 beg2 past2)))))

(define-syntax sequence-reverse-copy*!
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-reverse-copy*! str1 beg1 str2 beg2 past2)))))

(define-syntax sequence-take
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-take nchars str beg past)))))

(define-syntax sequence-take-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-take-right nchars str beg past)))))

(define-syntax sequence-drop
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-drop nchars str beg past)))))

(define-syntax sequence-drop-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-drop-right nchars str beg past)))))

(define-syntax sequence-trim
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-trim criterion str beg past)))))

(define-syntax sequence-trim-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-trim-right criterion str beg past)))))

(define-syntax sequence-trim-both
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-trim-both criterion str beg past)))))

(define $int-space (char->integer #\space))

(define-syntax sequence-pad
  (syntax-rules ()
    ((_ ?S ?len)
     (sequence-pad ?S ?len $int-space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-pad ?len ?char str beg past)))))

(define-syntax sequence-pad-right
  (syntax-rules ()
    ((_ ?S ?len)
     (sequence-pad-right ?S ?len $int-space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-pad-right ?len ?char str beg past)))))


;;;; prefix and suffix

(define-syntax sequence-prefix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-prefix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax sequence-suffix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-suffix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax sequence-prefix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-prefix-length-ci str1 beg1 past1 str2 beg2 past2)))))

(define-syntax sequence-suffix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-suffix-length-ci str1 beg1 past1 str2 beg2 past2)))))

;;; --------------------------------------------------------------------

(define-syntax sequence-prefix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-prefix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax sequence-suffix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-suffix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax sequence-prefix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-prefix-ci? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax sequence-suffix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%sequence-suffix-ci? str1 beg1 past1 str2 beg2 past2)))))


;;;; searching

(define-syntax sequence-index
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-index criterion str beg past)))))

(define-syntax sequence-index-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-index-right criterion str beg past)))))

(define-syntax sequence-skip
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-skip criterion str beg past)))))

(define-syntax sequence-skip-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-skip-right criterion str beg past)))))

(define-syntax sequence-count
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-count criterion str beg past)))))

(define-syntax sequence-contains
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-contains str1 start1 end1 str2 start2 end2)))))

(define-syntax sequence-contains-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%sequence-contains-ci str1 start1 end1 str2 start2 end2)))))


;;;; filtering

(define-syntax sequence-delete
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-delete criterion str beg past)))))

(define-syntax sequence-filter
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-filter criterion str beg past)))))


;;;; sequences and lists

(define-syntax sequence->list*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence->list* str beg past)))))

(define-syntax reverse-sequence->list
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%reverse-sequence->list str beg past)))))

(define-syntax sequence-tokenize
  (syntax-rules ()
    ((_ ?S ?token-set)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-tokenize ?token-set str beg past)))))

(define sequence-join
  (case-lambda
   ((sequences)
    (%sequence-join sequences " " 'infix))
   ((sequences delim)
    (%sequence-join sequences delim 'infix))
   ((sequences delim grammar)
    (%sequence-join sequences delim grammar))))


;;; replicating

(define-syntax xsubsequence
  (syntax-rules ()
    ((_ ?S ?from ?to)
     (let-values (((str beg past) (unpack ?S)))
       (%xsubsequence ?from ?to str beg past)))))

(define-syntax sequence-xcopy!
  (syntax-rules ()
    ((_ ?T ?S ?from ?to)
     (let-values (((str1 beg1 past1) (unpack ?T))
		  ((str2 beg2 past2) (unpack ?S)))
       (%sequence-xcopy! ?from ?to str1 beg1 past1 str2 beg2 past2)))))


;;; concatenate, reverse, replace, fill

(define sequence-concatenate-reverse
  (case-lambda

   ((sequence-list)
    (%sequence-concatenate-reverse sequence-list '#vu8() 0))

   ((sequence-list final)
    (%sequence-concatenate-reverse sequence-list final (bytevector-length final)))

   ((sequence-list final past)
    (%sequence-concatenate-reverse sequence-list final past))))

(define-syntax sequence-replace
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 past1) (unpack ?S1))
		  ((str2 start2 past2) (unpack ?S2)))
       (%sequence-replace str1 start1 past1 str2 start2 past2)))))

(define-syntax sequence-reverse
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-reverse str beg past)))))

(define-syntax sequence-reverse!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-reverse! str beg past)))))

(define-syntax sequence-fill*!
  (syntax-rules ()
    ((_ ?S ?fill-char)
     (let-values (((str beg past) (unpack ?S)))
       (%sequence-fill*! ?fill-char str beg past)))))


;;;; done

))

;;; end of file
