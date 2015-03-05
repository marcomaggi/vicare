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
(library (vicare containers bytevectors u8low)
  (export

    ;; constructors
    bytevector-u8-concatenate  %bytevector-u8-concatenate-reverse  bytevector-u8-tabulate
    subbytevector-u8 bytevector-u8-append

    ;; predicates
    bytevector-u8-null?  %bytevector-u8-every  %bytevector-u8-any

    ;; lexicographic comparison
    %bytevector-u8-compare  %bytevector-u8-compare-ci
    %bytevector-u8=  %bytevector-u8<>  %bytevector-u8-ci=  %bytevector-u8-ci<>
    %bytevector-u8<  %bytevector-u8<=  %bytevector-u8-ci<  %bytevector-u8-ci<=
    %bytevector-u8>  %bytevector-u8>=  %bytevector-u8-ci>  %bytevector-u8-ci>=

    ;; dictionary comparison
    %bytevector-u8-dictionary-compare
    %bytevector-u8-dictionary=?
    %bytevector-u8-dictionary<>?
    %bytevector-u8-dictionary<?
    %bytevector-u8-dictionary<=?
    %bytevector-u8-dictionary>?
    %bytevector-u8-dictionary>=?

    %bytevector-u8-dictionary-compare-ci
    %bytevector-u8-dictionary-ci=?
    %bytevector-u8-dictionary-ci<>?
    %bytevector-u8-dictionary-ci<?
    %bytevector-u8-dictionary-ci<=?
    %bytevector-u8-dictionary-ci>?
    %bytevector-u8-dictionary-ci>=?

    ;; bytevector-u8/numbers lexicographic comparison
    %bytevector-u8/numbers-compare	%bytevector-u8/numbers-compare-ci
    %bytevector-u8/numbers=?		%bytevector-u8/numbers<>?
    %bytevector-u8/numbers-ci=?		%bytevector-u8/numbers-ci<>?
    %bytevector-u8/numbers<?		%bytevector-u8/numbers<=?
    %bytevector-u8/numbers-ci<?		%bytevector-u8/numbers-ci>?
    %bytevector-u8/numbers>?		%bytevector-u8/numbers>=?
    %bytevector-u8/numbers-ci<=?	%bytevector-u8/numbers-ci>=?

    ;; bytevector-u8/numbers dictionary comparison
    %bytevector-u8/numbers-dictionary-compare
    %bytevector-u8/numbers-dictionary=?	%bytevector-u8/numbers-dictionary<>?
    %bytevector-u8/numbers-dictionary<?	%bytevector-u8/numbers-dictionary<=?
    %bytevector-u8/numbers-dictionary>?	%bytevector-u8/numbers-dictionary>=?

    %bytevector-u8/numbers-dictionary-compare-ci
    %bytevector-u8/numbers-dictionary-ci=?	%bytevector-u8/numbers-dictionary-ci<>?
    %bytevector-u8/numbers-dictionary-ci<?	%bytevector-u8/numbers-dictionary-ci>?
    %bytevector-u8/numbers-dictionary-ci<=?	%bytevector-u8/numbers-dictionary-ci>=?

    ;; mapping
    bytevector-u8-map      bytevector-u8-map!
    bytevector-u8-map*     bytevector-u8-map*!     bytevector-u8-for-each*
    %subbytevector-u8-map  %subbytevector-u8-map!  %subbytevector-u8-for-each
    %subbytevector-u8-for-each-index

    ;; case hacking
    %bytevector-u8-titlecase*!

    ;; folding and unfolding
    bytevector-u8-fold-left		bytevector-u8-fold-right
    bytevector-u8-fold-left*		bytevector-u8-fold-right*
    %subbytevector-u8-fold-left	%subbytevector-u8-fold-right
    bytevector-u8-unfold		bytevector-u8-unfold-right

    ;; selecting
    (rename (subbytevector-u8 %bytevector-u8-copy*)) %bytevector-u8-reverse-copy*
    %bytevector-u8-copy*!  %bytevector-u8-reverse-copy*!
    %bytevector-u8-take    %bytevector-u8-take-right
    %bytevector-u8-drop    %bytevector-u8-drop-right

    ;; padding and trimming
    %bytevector-u8-trim    %bytevector-u8-trim-right  %bytevector-u8-trim-both
    %bytevector-u8-pad     %bytevector-u8-pad-right

    ;; prefix and suffix
    %bytevector-u8-prefix-length  %bytevector-u8-prefix-length-ci
    %bytevector-u8-suffix-length  %bytevector-u8-suffix-length-ci
    %bytevector-u8-prefix?        %bytevector-u8-prefix-ci?
    %bytevector-u8-suffix?        %bytevector-u8-suffix-ci?

    ;; searching
    %bytevector-u8-index     %bytevector-u8-index-right
    %bytevector-u8-skip      %bytevector-u8-skip-right
    %bytevector-u8-contains  %bytevector-u8-contains-ci
    %bytevector-u8-count

    ;; filtering
    %bytevector-u8-delete  %bytevector-u8-filter

    ;; lists
    %bytevector->u8-list*   %reverse-bytevector->u8-list
    reverse-u8-list->bytevector
    %bytevector-u8-tokenize  %bytevector-u8-join
    (rename (%bytevector-u8-tokenize %bytevector-u8-tokenise))

    ;; replicating
    %xsubbytevector-u8  %bytevector-u8-xcopy!

    ;; mutating
    %bytevector-u8-fill*!  bytevector-u8-swap!

    ;; reverse and replace
    %bytevector-u8-reverse  %bytevector-u8-reverse!
    %bytevector-u8-replace)
  (import (rnrs)
    (vicare containers bytevectors generic-low)
    (vicare containers char-sets)
    (vicare containers knuth-morris-pratt))


(instantiate-body
 ( ;;basic
  (sequence-set!			bytevector-u8-set!)
  (sequence-ref				bytevector-u8-ref)
  (sequence->list			bytevector->u8-list)
  (list->sequence			u8-list->bytevector)

  ;; constructors
  (sequence-concatenate			bytevector-u8-concatenate)
  (sequence-append			bytevector-u8-append)
  (%sequence-concatenate-reverse	%bytevector-u8-concatenate-reverse)
  (sequence-tabulate			bytevector-u8-tabulate)

  ;; predicates
  (sequence-null?			bytevector-u8-null?)
  (%sequence-every			%bytevector-u8-every)
  (%sequence-any			%bytevector-u8-any)

  ;; lexicographic comparison

  (%sequence-compare			%bytevector-u8-compare)
  (%sequence-compare-ci			%bytevector-u8-compare-ci)
  (%sequence=				%bytevector-u8=)
  (%sequence-ci=			%bytevector-u8-ci=)

  (%sequence<>				%bytevector-u8<>)
  (%sequence-ci<>			%bytevector-u8-ci<>)
  (%sequence<				%bytevector-u8<)
  (%sequence-ci<			%bytevector-u8-ci<)

  (%sequence<=				%bytevector-u8<=)
  (%sequence-ci<=			%bytevector-u8-ci<=)
  (%sequence>				%bytevector-u8>)
  (%sequence-ci>			%bytevector-u8-ci>)
  (%sequence>=				%bytevector-u8>=)
  (%sequence-ci>=			%bytevector-u8-ci>=)

  ;; dictionary comparison
  (%sequence-dictionary-compare		%bytevector-u8-dictionary-compare)
  (%sequence-dictionary=?		%bytevector-u8-dictionary=?)
  (%sequence-dictionary<>?		%bytevector-u8-dictionary<>?)
  (%sequence-dictionary<?		%bytevector-u8-dictionary<?)
  (%sequence-dictionary<=?		%bytevector-u8-dictionary<=?)
  (%sequence-dictionary>?		%bytevector-u8-dictionary>?)
  (%sequence-dictionary>=?		%bytevector-u8-dictionary>=?)

  (%sequence-dictionary-compare-ci	%bytevector-u8-dictionary-compare-ci)
  (%sequence-dictionary-ci=?		%bytevector-u8-dictionary-ci=?)
  (%sequence-dictionary-ci<>?		%bytevector-u8-dictionary-ci<>?)
  (%sequence-dictionary-ci<?		%bytevector-u8-dictionary-ci<?)
  (%sequence-dictionary-ci<=?		%bytevector-u8-dictionary-ci<=?)
  (%sequence-dictionary-ci>?		%bytevector-u8-dictionary-ci>?)
  (%sequence-dictionary-ci>=?		%bytevector-u8-dictionary-ci>=?)

  ;; bytevector-u8/numbers lexicographic comparison
  (%sequence/numbers-compare		%bytevector-u8/numbers-compare)
  (%sequence/numbers=?			%bytevector-u8/numbers=?)
  (%sequence/numbers<>?			%bytevector-u8/numbers<>?)
  (%sequence/numbers<?			%bytevector-u8/numbers<?)
  (%sequence/numbers<=?			%bytevector-u8/numbers<=?)
  (%sequence/numbers>?			%bytevector-u8/numbers>?)
  (%sequence/numbers>=?			%bytevector-u8/numbers>=?)

  (%sequence/numbers-compare-ci		%bytevector-u8/numbers-compare-ci)
  (%sequence/numbers-ci=?		%bytevector-u8/numbers-ci=?)
  (%sequence/numbers-ci<>?		%bytevector-u8/numbers-ci<>?)
  (%sequence/numbers-ci<?		%bytevector-u8/numbers-ci<?)
  (%sequence/numbers-ci<=?		%bytevector-u8/numbers-ci<=?)
  (%sequence/numbers-ci>?		%bytevector-u8/numbers-ci>?)
  (%sequence/numbers-ci>=?		%bytevector-u8/numbers-ci>=?)

  ;; bytevector-u8/numbers dictionary comparison
  (%sequence/numbers-dictionary-compare	%bytevector-u8/numbers-dictionary-compare)
  (%sequence/numbers-dictionary=?	%bytevector-u8/numbers-dictionary=?)
  (%sequence/numbers-dictionary<>?	%bytevector-u8/numbers-dictionary<>?)
  (%sequence/numbers-dictionary<?	%bytevector-u8/numbers-dictionary<?)
  (%sequence/numbers-dictionary<=?	%bytevector-u8/numbers-dictionary<=?)
  (%sequence/numbers-dictionary>?	%bytevector-u8/numbers-dictionary>?)
  (%sequence/numbers-dictionary>=?	%bytevector-u8/numbers-dictionary>=?)

  (%sequence/numbers-dictionary-compare-ci %bytevector-u8/numbers-dictionary-compare-ci)
  (%sequence/numbers-dictionary-ci=?	%bytevector-u8/numbers-dictionary-ci=?)
  (%sequence/numbers-dictionary-ci<>?	%bytevector-u8/numbers-dictionary-ci<>?)
  (%sequence/numbers-dictionary-ci<?	%bytevector-u8/numbers-dictionary-ci<?)
  (%sequence/numbers-dictionary-ci<=?	%bytevector-u8/numbers-dictionary-ci<=?)
  (%sequence/numbers-dictionary-ci>?	%bytevector-u8/numbers-dictionary-ci>?)
  (%sequence/numbers-dictionary-ci>=?	%bytevector-u8/numbers-dictionary-ci>=?)

  ;; mapping
  (sequence-map				bytevector-u8-map)
  (sequence-map!			bytevector-u8-map!)
  (sequence-map*			bytevector-u8-map*)
  (sequence-map*!			bytevector-u8-map*!)
  (sequence-for-each*			bytevector-u8-for-each*)

  (%subsequence-map			%subbytevector-u8-map)
  (%subsequence-map!			%subbytevector-u8-map!)
  (%subsequence-for-each		%subbytevector-u8-for-each)
  (%subsequence-for-each-index		%subbytevector-u8-for-each-index)

  ;; case hacking
  (%sequence-titlecase*!		%bytevector-u8-titlecase*!)

  ;; folding
  (sequence-fold-left			bytevector-u8-fold-left)
  (sequence-fold-right			bytevector-u8-fold-right)
  (sequence-fold-left*			bytevector-u8-fold-left*)
  (sequence-fold-right*			bytevector-u8-fold-right*)
  (%subsequence-fold-left		%subbytevector-u8-fold-left)
  (%subsequence-fold-right		%subbytevector-u8-fold-right)
  (sequence-unfold			bytevector-u8-unfold)
  (sequence-unfold-right		bytevector-u8-unfold-right)

  ;; selecting
  (%sequence-reverse-copy*		%bytevector-u8-reverse-copy*)
  (%sequence-take			%bytevector-u8-take)
  (%sequence-take-right			%bytevector-u8-take-right)
  (%sequence-drop			%bytevector-u8-drop)
  (%sequence-drop-right			%bytevector-u8-drop-right)
  (%sequence-trim			%bytevector-u8-trim)
  (%sequence-trim-right			%bytevector-u8-trim-right)
  (%sequence-trim-both			%bytevector-u8-trim-both)
  (%sequence-pad			%bytevector-u8-pad)
  (%sequence-pad-right			%bytevector-u8-pad-right)

  ;; prefix and suffix
  (%sequence-prefix-length		%bytevector-u8-prefix-length)
  (%sequence-prefix-length-ci		%bytevector-u8-prefix-length-ci)
  (%sequence-prefix?			%bytevector-u8-prefix?)
  (%sequence-prefix-ci?			%bytevector-u8-prefix-ci?)

  (%sequence-suffix-length		%bytevector-u8-suffix-length)
  (%sequence-suffix-length-ci		%bytevector-u8-suffix-length-ci)
  (%sequence-suffix?			%bytevector-u8-suffix?)
  (%sequence-suffix-ci?			%bytevector-u8-suffix-ci?)

  ;; searching
  (%sequence-index			%bytevector-u8-index)
  (%sequence-index-right		%bytevector-u8-index-right)
  (%sequence-skip			%bytevector-u8-skip)
  (%sequence-skip-right			%bytevector-u8-skip-right)
  (%sequence-count			%bytevector-u8-count)
  (%sequence-contains			%bytevector-u8-contains)
  (%sequence-contains-ci		%bytevector-u8-contains-ci)

  ;; filtering
  (%sequence-filter			%bytevector-u8-filter)
  (%sequence-delete			%bytevector-u8-delete)

  ;; bytevectors and lists
  (reverse-list->sequence		reverse-u8-list->bytevector)
  (%reverse-sequence->list		%reverse-bytevector->u8-list)
  (%sequence->list*			%bytevector->u8-list*)
  (%sequence-join			%bytevector-u8-join)
  (%sequence-tokenize			%bytevector-u8-tokenize)

  ;; subvector functions
  (subsequence				subbytevector-u8)
  (%xsubsequence			%xsubbytevector-u8)
  (%sequence-xcopy!			%bytevector-u8-xcopy!)

  ;; reverse, replace
  (%sequence-reverse			%bytevector-u8-reverse)
  (%sequence-replace			%bytevector-u8-replace)
  (%sequence-reverse!			%bytevector-u8-reverse!)

  ;; mutating
  (%sequence-copy*!			%bytevector-u8-copy*!)
  (%sequence-reverse-copy*!		%bytevector-u8-reverse-copy*!)
  (%sequence-fill*!			%bytevector-u8-fill*!)
  (sequence-swap!			bytevector-u8-swap!)))


;;;; done

)

;;; end of file
