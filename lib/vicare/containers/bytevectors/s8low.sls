;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: low level bytevector functions
;;;Date: Tue Jul  5, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers bytevectors s8low)
  (export

    bytevector->s8-list		s8-list->bytevector

    ;; constructors
    bytevector-s8-concatenate  %bytevector-s8-concatenate-reverse  bytevector-s8-tabulate
    subbytevector-s8 bytevector-s8-append

    ;; predicates
    bytevector-s8-null?  %bytevector-s8-every  %bytevector-s8-any

    ;; lexicographic comparison
    %bytevector-s8-compare  %bytevector-s8-compare-ci
    %bytevector-s8=  %bytevector-s8<>  %bytevector-s8-ci=  %bytevector-s8-ci<>
    %bytevector-s8<  %bytevector-s8<=  %bytevector-s8-ci<  %bytevector-s8-ci<=
    %bytevector-s8>  %bytevector-s8>=  %bytevector-s8-ci>  %bytevector-s8-ci>=

    ;; dictionary comparison
    %bytevector-s8-dictionary-compare
    %bytevector-s8-dictionary=?
    %bytevector-s8-dictionary<>?
    %bytevector-s8-dictionary<?
    %bytevector-s8-dictionary<=?
    %bytevector-s8-dictionary>?
    %bytevector-s8-dictionary>=?

    %bytevector-s8-dictionary-compare-ci
    %bytevector-s8-dictionary-ci=?
    %bytevector-s8-dictionary-ci<>?
    %bytevector-s8-dictionary-ci<?
    %bytevector-s8-dictionary-ci<=?
    %bytevector-s8-dictionary-ci>?
    %bytevector-s8-dictionary-ci>=?

    ;; bytevector-s8/numbers lexicographic comparison
    %bytevector-s8/numbers-compare	%bytevector-s8/numbers-compare-ci
    %bytevector-s8/numbers=?		%bytevector-s8/numbers<>?
    %bytevector-s8/numbers-ci=?		%bytevector-s8/numbers-ci<>?
    %bytevector-s8/numbers<?		%bytevector-s8/numbers<=?
    %bytevector-s8/numbers-ci<?		%bytevector-s8/numbers-ci>?
    %bytevector-s8/numbers>?		%bytevector-s8/numbers>=?
    %bytevector-s8/numbers-ci<=?	%bytevector-s8/numbers-ci>=?

    ;; bytevector-s8/numbers dictionary comparison
    %bytevector-s8/numbers-dictionary-compare
    %bytevector-s8/numbers-dictionary=?	%bytevector-s8/numbers-dictionary<>?
    %bytevector-s8/numbers-dictionary<?	%bytevector-s8/numbers-dictionary<=?
    %bytevector-s8/numbers-dictionary>?	%bytevector-s8/numbers-dictionary>=?

    %bytevector-s8/numbers-dictionary-compare-ci
    %bytevector-s8/numbers-dictionary-ci=?	%bytevector-s8/numbers-dictionary-ci<>?
    %bytevector-s8/numbers-dictionary-ci<?	%bytevector-s8/numbers-dictionary-ci>?
    %bytevector-s8/numbers-dictionary-ci<=?	%bytevector-s8/numbers-dictionary-ci>=?

    ;; mapping
    bytevector-s8-map      bytevector-s8-map!
    bytevector-s8-map*     bytevector-s8-map*!     bytevector-s8-for-each*
    %subbytevector-s8-map  %subbytevector-s8-map!  %subbytevector-s8-for-each
    %subbytevector-s8-for-each-index

    ;; case hacking
    %bytevector-s8-titlecase*!

    ;; folding and unfolding
    bytevector-s8-fold-left		bytevector-s8-fold-right
    bytevector-s8-fold-left*		bytevector-s8-fold-right*
    %subbytevector-s8-fold-left	%subbytevector-s8-fold-right
    bytevector-s8-unfold		bytevector-s8-unfold-right

    ;; selecting
    (rename (subbytevector-s8 %bytevector-s8-copy*)) %bytevector-s8-reverse-copy*
    %bytevector-s8-copy*!  %bytevector-s8-reverse-copy*!
    %bytevector-s8-take    %bytevector-s8-take-right
    %bytevector-s8-drop    %bytevector-s8-drop-right

    ;; padding and trimming
    %bytevector-s8-trim    %bytevector-s8-trim-right  %bytevector-s8-trim-both
    %bytevector-s8-pad     %bytevector-s8-pad-right

    ;; prefix and suffix
    %bytevector-s8-prefix-length  %bytevector-s8-prefix-length-ci
    %bytevector-s8-suffix-length  %bytevector-s8-suffix-length-ci
    %bytevector-s8-prefix?        %bytevector-s8-prefix-ci?
    %bytevector-s8-suffix?        %bytevector-s8-suffix-ci?

    ;; searching
    %bytevector-s8-index     %bytevector-s8-index-right
    %bytevector-s8-skip      %bytevector-s8-skip-right
    %bytevector-s8-contains  %bytevector-s8-contains-ci
    %bytevector-s8-count

    ;; filtering
    %bytevector-s8-delete  %bytevector-s8-filter

    ;; lists
    %bytevector->s8-list*   %reverse-bytevector->s8-list
    reverse-s8-list->bytevector
    %bytevector-s8-tokenize  %bytevector-s8-join
    (rename (%bytevector-s8-tokenize %bytevector-s8-tokenise))

    ;; replicating
    %xsubbytevector-s8  %bytevector-s8-xcopy!

    ;; mutating
    %bytevector-s8-fill*!  bytevector-s8-swap!

    ;; reverse and replace
    %bytevector-s8-reverse  %bytevector-s8-reverse!
    %bytevector-s8-replace)
  (import (rnrs)
    (vicare containers bytevectors generic-low)
    (vicare containers char-sets)
    (vicare containers knuth-morris-pratt))


;;;; helpers

(define (bytevector->s8-list bv)
  (bytevector->sint-list bv  (endianness big) 1))

(define (s8-list->bytevector ell)
  (sint-list->bytevector ell (endianness big) 1))


(instantiate-body
 ( ;;basic
  (sequence-set!			bytevector-s8-set!)
  (sequence-ref				bytevector-s8-ref)
  (sequence->list			bytevector->s8-list)
  (list->sequence			s8-list->bytevector)

  ;; constructors
  (sequence-concatenate			bytevector-s8-concatenate)
  (sequence-append			bytevector-s8-append)
  (%sequence-concatenate-reverse	%bytevector-s8-concatenate-reverse)
  (sequence-tabulate			bytevector-s8-tabulate)

  ;; predicates
  (sequence-null?			bytevector-s8-null?)
  (%sequence-every			%bytevector-s8-every)
  (%sequence-any			%bytevector-s8-any)

  ;; lexicographic comparison

  (%sequence-compare			%bytevector-s8-compare)
  (%sequence-compare-ci			%bytevector-s8-compare-ci)
  (%sequence=				%bytevector-s8=)
  (%sequence-ci=			%bytevector-s8-ci=)

  (%sequence<>				%bytevector-s8<>)
  (%sequence-ci<>			%bytevector-s8-ci<>)
  (%sequence<				%bytevector-s8<)
  (%sequence-ci<			%bytevector-s8-ci<)

  (%sequence<=				%bytevector-s8<=)
  (%sequence-ci<=			%bytevector-s8-ci<=)
  (%sequence>				%bytevector-s8>)
  (%sequence-ci>			%bytevector-s8-ci>)
  (%sequence>=				%bytevector-s8>=)
  (%sequence-ci>=			%bytevector-s8-ci>=)

  ;; dictionary comparison
  (%sequence-dictionary-compare		%bytevector-s8-dictionary-compare)
  (%sequence-dictionary=?		%bytevector-s8-dictionary=?)
  (%sequence-dictionary<>?		%bytevector-s8-dictionary<>?)
  (%sequence-dictionary<?		%bytevector-s8-dictionary<?)
  (%sequence-dictionary<=?		%bytevector-s8-dictionary<=?)
  (%sequence-dictionary>?		%bytevector-s8-dictionary>?)
  (%sequence-dictionary>=?		%bytevector-s8-dictionary>=?)

  (%sequence-dictionary-compare-ci	%bytevector-s8-dictionary-compare-ci)
  (%sequence-dictionary-ci=?		%bytevector-s8-dictionary-ci=?)
  (%sequence-dictionary-ci<>?		%bytevector-s8-dictionary-ci<>?)
  (%sequence-dictionary-ci<?		%bytevector-s8-dictionary-ci<?)
  (%sequence-dictionary-ci<=?		%bytevector-s8-dictionary-ci<=?)
  (%sequence-dictionary-ci>?		%bytevector-s8-dictionary-ci>?)
  (%sequence-dictionary-ci>=?		%bytevector-s8-dictionary-ci>=?)

  ;; bytevector-s8/numbers lexicographic comparison
  (%sequence/numbers-compare		%bytevector-s8/numbers-compare)
  (%sequence/numbers=?			%bytevector-s8/numbers=?)
  (%sequence/numbers<>?			%bytevector-s8/numbers<>?)
  (%sequence/numbers<?			%bytevector-s8/numbers<?)
  (%sequence/numbers<=?			%bytevector-s8/numbers<=?)
  (%sequence/numbers>?			%bytevector-s8/numbers>?)
  (%sequence/numbers>=?			%bytevector-s8/numbers>=?)

  (%sequence/numbers-compare-ci		%bytevector-s8/numbers-compare-ci)
  (%sequence/numbers-ci=?		%bytevector-s8/numbers-ci=?)
  (%sequence/numbers-ci<>?		%bytevector-s8/numbers-ci<>?)
  (%sequence/numbers-ci<?		%bytevector-s8/numbers-ci<?)
  (%sequence/numbers-ci<=?		%bytevector-s8/numbers-ci<=?)
  (%sequence/numbers-ci>?		%bytevector-s8/numbers-ci>?)
  (%sequence/numbers-ci>=?		%bytevector-s8/numbers-ci>=?)

  ;; bytevector-s8/numbers dictionary comparison
  (%sequence/numbers-dictionary-compare	%bytevector-s8/numbers-dictionary-compare)
  (%sequence/numbers-dictionary=?	%bytevector-s8/numbers-dictionary=?)
  (%sequence/numbers-dictionary<>?	%bytevector-s8/numbers-dictionary<>?)
  (%sequence/numbers-dictionary<?	%bytevector-s8/numbers-dictionary<?)
  (%sequence/numbers-dictionary<=?	%bytevector-s8/numbers-dictionary<=?)
  (%sequence/numbers-dictionary>?	%bytevector-s8/numbers-dictionary>?)
  (%sequence/numbers-dictionary>=?	%bytevector-s8/numbers-dictionary>=?)

  (%sequence/numbers-dictionary-compare-ci %bytevector-s8/numbers-dictionary-compare-ci)
  (%sequence/numbers-dictionary-ci=?	%bytevector-s8/numbers-dictionary-ci=?)
  (%sequence/numbers-dictionary-ci<>?	%bytevector-s8/numbers-dictionary-ci<>?)
  (%sequence/numbers-dictionary-ci<?	%bytevector-s8/numbers-dictionary-ci<?)
  (%sequence/numbers-dictionary-ci<=?	%bytevector-s8/numbers-dictionary-ci<=?)
  (%sequence/numbers-dictionary-ci>?	%bytevector-s8/numbers-dictionary-ci>?)
  (%sequence/numbers-dictionary-ci>=?	%bytevector-s8/numbers-dictionary-ci>=?)

  ;; mapping
  (sequence-map				bytevector-s8-map)
  (sequence-map!			bytevector-s8-map!)
  (sequence-map*			bytevector-s8-map*)
  (sequence-map*!			bytevector-s8-map*!)
  (sequence-for-each*			bytevector-s8-for-each*)

  (%subsequence-map			%subbytevector-s8-map)
  (%subsequence-map!			%subbytevector-s8-map!)
  (%subsequence-for-each		%subbytevector-s8-for-each)
  (%subsequence-for-each-index		%subbytevector-s8-for-each-index)

  ;; case hacking
  (%sequence-titlecase*!		%bytevector-s8-titlecase*!)

  ;; folding
  (sequence-fold-left			bytevector-s8-fold-left)
  (sequence-fold-right			bytevector-s8-fold-right)
  (sequence-fold-left*			bytevector-s8-fold-left*)
  (sequence-fold-right*			bytevector-s8-fold-right*)
  (%subsequence-fold-left		%subbytevector-s8-fold-left)
  (%subsequence-fold-right		%subbytevector-s8-fold-right)
  (sequence-unfold			bytevector-s8-unfold)
  (sequence-unfold-right		bytevector-s8-unfold-right)

  ;; selecting
  (%sequence-reverse-copy*		%bytevector-s8-reverse-copy*)
  (%sequence-take			%bytevector-s8-take)
  (%sequence-take-right			%bytevector-s8-take-right)
  (%sequence-drop			%bytevector-s8-drop)
  (%sequence-drop-right			%bytevector-s8-drop-right)
  (%sequence-trim			%bytevector-s8-trim)
  (%sequence-trim-right			%bytevector-s8-trim-right)
  (%sequence-trim-both			%bytevector-s8-trim-both)
  (%sequence-pad			%bytevector-s8-pad)
  (%sequence-pad-right			%bytevector-s8-pad-right)

  ;; prefix and suffix
  (%sequence-prefix-length		%bytevector-s8-prefix-length)
  (%sequence-prefix-length-ci		%bytevector-s8-prefix-length-ci)
  (%sequence-prefix?			%bytevector-s8-prefix?)
  (%sequence-prefix-ci?			%bytevector-s8-prefix-ci?)

  (%sequence-suffix-length		%bytevector-s8-suffix-length)
  (%sequence-suffix-length-ci		%bytevector-s8-suffix-length-ci)
  (%sequence-suffix?			%bytevector-s8-suffix?)
  (%sequence-suffix-ci?			%bytevector-s8-suffix-ci?)

  ;; searching
  (%sequence-index			%bytevector-s8-index)
  (%sequence-index-right		%bytevector-s8-index-right)
  (%sequence-skip			%bytevector-s8-skip)
  (%sequence-skip-right			%bytevector-s8-skip-right)
  (%sequence-count			%bytevector-s8-count)
  (%sequence-contains			%bytevector-s8-contains)
  (%sequence-contains-ci		%bytevector-s8-contains-ci)

  ;; filtering
  (%sequence-filter			%bytevector-s8-filter)
  (%sequence-delete			%bytevector-s8-delete)

  ;; bytevectors and lists
  (reverse-list->sequence		reverse-s8-list->bytevector)
  (%reverse-sequence->list		%reverse-bytevector->s8-list)
  (%sequence->list*			%bytevector->s8-list*)
  (%sequence-join			%bytevector-s8-join)
  (%sequence-tokenize			%bytevector-s8-tokenize)

  ;; subvector functions
  (subsequence				subbytevector-s8)
  (%xsubsequence			%xsubbytevector-s8)
  (%sequence-xcopy!			%bytevector-s8-xcopy!)

  ;; reverse, replace
  (%sequence-reverse			%bytevector-s8-reverse)
  (%sequence-replace			%bytevector-s8-replace)
  (%sequence-reverse!			%bytevector-s8-reverse!)

  ;; mutating
  (%sequence-copy*!			%bytevector-s8-copy*!)
  (%sequence-reverse-copy*!		%bytevector-s8-reverse-copy*!)
  (%sequence-fill*!			%bytevector-s8-fill*!)
  (sequence-swap!			bytevector-s8-swap!)))


;;;; done

)

;;; end of file
