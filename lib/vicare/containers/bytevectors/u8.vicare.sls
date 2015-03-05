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
;;;Copyright (c) 2010, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers bytevectors u8)
  (export

    ;; auxiliary syntaxes
    view start past

    ;; constructors
    bytevector-u8-concatenate bytevector-u8-concatenate-reverse
    bytevector-u8-tabulate subbytevector-u8 bytevector-u8-append

    ;; predicates
    bytevector-u8-null?  bytevector-u8-every  bytevector-u8-any

    ;; comparison
    bytevector-u8-compare  bytevector-u8-compare-ci
    bytevector-u8=  bytevector-u8<>  bytevector-u8-ci=  bytevector-u8-ci<>
    bytevector-u8<  bytevector-u8<=  bytevector-u8-ci<  bytevector-u8-ci<=
    bytevector-u8>  bytevector-u8>=  bytevector-u8-ci>  bytevector-u8-ci>=

    ;; mapping
    bytevector-u8-map     bytevector-u8-map!
    bytevector-u8-map*    bytevector-u8-map*!    bytevector-u8-for-each*
    subbytevector-u8-map  subbytevector-u8-map!  subbytevector-u8-for-each  subbytevector-u8-for-each-index

    ;; case
    bytevector-u8-downcase*   bytevector-u8-upcase*   bytevector-u8-titlecase*
    bytevector-u8-downcase*!  bytevector-u8-upcase*!  bytevector-u8-titlecase*!

    ;; folding
    bytevector-u8-fold-left		bytevector-u8-fold-right
    bytevector-u8-fold-left*		bytevector-u8-fold-right*
    subbytevector-u8-fold-left		subbytevector-u8-fold-right
    bytevector-u8-unfold		bytevector-u8-unfold-right

    ;; selecting
    subbytevector-u8*
    (rename (subbytevector-u8* bytevector-u8-copy*)) bytevector-u8-reverse-copy*
    bytevector-u8-copy*!  bytevector-u8-reverse-copy*!
    bytevector-u8-take    bytevector-u8-take-right
    bytevector-u8-drop    bytevector-u8-drop-right

    ;; padding and trimming
    bytevector-u8-trim  bytevector-u8-trim-right  bytevector-u8-trim-both
    bytevector-u8-pad   bytevector-u8-pad-right

    ;; prefix and suffix
    bytevector-u8-prefix-length  bytevector-u8-prefix-length-ci
    bytevector-u8-suffix-length  bytevector-u8-suffix-length-ci
    bytevector-u8-prefix?        bytevector-u8-prefix-ci?
    bytevector-u8-suffix?        bytevector-u8-suffix-ci?

    ;; searching
    bytevector-u8-index     bytevector-u8-index-right
    bytevector-u8-skip      bytevector-u8-skip-right
    bytevector-u8-contains  bytevector-u8-contains-ci
    bytevector-u8-count

    ;; filtering
    bytevector-u8-filter bytevector-u8-delete

    ;; lists
    bytevector->u8-list*  reverse-u8-list->bytevector
    bytevector-u8-join    bytevector-u8-tokenize
    (rename (bytevector-u8-tokenize bytevector-u8-tokenise))

    ;; replicating
    xsubbytevector-u8  bytevector-u8-xcopy!

    ;; mutating
    bytevector-u8-fill*! bytevector-u8-swap!

    ;; reverse and replace
    bytevector-u8-reverse  bytevector-u8-reverse!
    bytevector-u8-replace

    (rename (unpack %bytevector-u8-unpack))
    )
  (import (rnrs)
    (vicare language-extensions ascii-chars)
    (vicare containers bytevectors u8low)
    (vicare containers bytevectors generic)
    (only (vicare containers auxiliary-syntaxes)
	  view start past))


(define-syntax unpack
  (syntax-rules (view start past)

    ((_ (view ?str))
     (let ((str ?str))
       (values str 0 (bytevector-length str))))

    ((_ (view ?str (start ?start)))
     (let ((str ?str))
       (values str ?start (bytevector-length str))))

    ((_ (view ?str (past ?past)))
     (values ?str 0 ?past))

    ((_ (view ?str (start ?start) (past ?past)))
     (values ?str ?start ?past))

    ((?F ?str)
     (let ((str ?str))
       (values str 0 (bytevector-length str))))

    ((?F ?stuff ...)
     (syntax-violation #f "invalid parameters" (?stuff ...)))))


(instantiate-body
 ( ;;constructors
  (sequence->list			bytevector->u8-list)
  (list->sequence			u8-list->bytevector)

  ;;predicates
  (sequence-every			bytevector-u8-every)
  (%sequence-every			%bytevector-u8-every)
  (sequence-any				bytevector-u8-any)
  (%sequence-any			%bytevector-u8-any)

  ;; comparison
  (sequence-compare			bytevector-u8-compare)
  (%sequence-compare			%bytevector-u8-compare)
  (sequence-compare-ci			bytevector-u8-compare-ci)
  (%sequence-compare-ci			%bytevector-u8-compare-ci)

  (sequence=				bytevector-u8=)
  (%sequence=				%bytevector-u8=)
  (sequence<>				bytevector-u8<>)
  (%sequence<>				%bytevector-u8<>)
  (sequence<				bytevector-u8<)
  (%sequence<				%bytevector-u8<)
  (sequence>				bytevector-u8>)
  (%sequence>				%bytevector-u8>)
  (sequence<=				bytevector-u8<=)
  (%sequence<=				%bytevector-u8<=)
  (sequence>=				bytevector-u8>=)
  (%sequence>=				%bytevector-u8>=)

  (sequence-ci=				bytevector-u8-ci=)
  (%sequence-ci=			%bytevector-u8-ci=)
  (sequence-ci<>			bytevector-u8-ci<>)
  (%sequence-ci<>			%bytevector-u8-ci<>)
  (sequence-ci<				bytevector-u8-ci<)
  (%sequence-ci<			%bytevector-u8-ci<)
  (sequence-ci>				bytevector-u8-ci>)
  (%sequence-ci>			%bytevector-u8-ci>)
  (sequence-ci<=			bytevector-u8-ci<=)
  (%sequence-ci<=			%bytevector-u8-ci<=)
  (sequence-ci>=			bytevector-u8-ci>=)
  (%sequence-ci>=			%bytevector-u8-ci>=)

  ;; mapping
  (subsequence-map			subbytevector-u8-map)
  (%subsequence-map			%subbytevector-u8-map)
  (subsequence-map!			subbytevector-u8-map!)
  (%subsequence-map!			%subbytevector-u8-map!)
  (subsequence-for-each			subbytevector-u8-for-each)
  (%subsequence-for-each		%subbytevector-u8-for-each)
  (subsequence-for-each-index		subbytevector-u8-for-each-index)
  (%subsequence-for-each-index		%subbytevector-u8-for-each-index)

  ;; case hacking
  (sequence-upcase*			bytevector-u8-upcase*)
  (sequence-upcase*!			bytevector-u8-upcase*!)
  (sequence-downcase*			bytevector-u8-downcase*)
  (sequence-downcase*!			bytevector-u8-downcase*!)
  (sequence-titlecase*			bytevector-u8-titlecase*)
  (sequence-titlecase*!			bytevector-u8-titlecase*!)
  (%sequence-titlecase*!		%bytevector-u8-titlecase*!)

  ;; folding
  (subsequence-fold-left		subbytevector-u8-fold-left)
  (%subsequence-fold-left		%subbytevector-u8-fold-left)
  (subsequence-fold-right		subbytevector-u8-fold-right)
  (%subsequence-fold-right		%subbytevector-u8-fold-right)

  ;; selecting
  (subsequence				subbytevector-u8)
  (subsequence*				subbytevector-u8*)
  (sequence-reverse-copy*		bytevector-u8-reverse-copy*)
  (%sequence-reverse-copy*		%bytevector-u8-reverse-copy*)
  (sequence-copy*!			bytevector-u8-copy*!)
  (%sequence-copy*!			%bytevector-u8-copy*!)
  (sequence-reverse-copy*!		bytevector-u8-reverse-copy*!)
  (%sequence-reverse-copy*!		%bytevector-u8-reverse-copy*!)
  (sequence-take			bytevector-u8-take)
  (%sequence-take			%bytevector-u8-take)
  (sequence-take-right			bytevector-u8-take-right)
  (%sequence-take-right			%bytevector-u8-take-right)
  (sequence-drop			bytevector-u8-drop)
  (%sequence-drop			%bytevector-u8-drop )
  (sequence-drop-right			bytevector-u8-drop-right)
  (%sequence-drop-right			%bytevector-u8-drop-right )
  (sequence-trim			bytevector-u8-trim)
  (%sequence-trim			%bytevector-u8-trim)
  (sequence-trim-right			bytevector-u8-trim-right)
  (%sequence-trim-right			%bytevector-u8-trim-right)
  (sequence-trim-both			bytevector-u8-trim-both)
  (%sequence-trim-both			%bytevector-u8-trim-both)
  (sequence-pad				bytevector-u8-pad)
  (%sequence-pad			%bytevector-u8-pad)
  (sequence-pad-right			bytevector-u8-pad-right)
  (%sequence-pad-right			%bytevector-u8-pad-right)

  ;; prefix and suffix

  (sequence-prefix-length		bytevector-u8-prefix-length)
  (%sequence-prefix-length		%bytevector-u8-prefix-length)
  (sequence-suffix-length		bytevector-u8-suffix-length)
  (%sequence-suffix-length		%bytevector-u8-suffix-length)
  (sequence-prefix-length-ci		bytevector-u8-prefix-length-ci)
  (%sequence-prefix-length-ci		%bytevector-u8-prefix-length-ci)
  (sequence-suffix-length-ci		bytevector-u8-suffix-length-ci)
  (%sequence-suffix-length-ci		%bytevector-u8-suffix-length-ci)

  (sequence-prefix?			bytevector-u8-prefix?)
  (%sequence-prefix?			%bytevector-u8-prefix?)
  (sequence-suffix?			bytevector-u8-suffix?)
  (%sequence-suffix?			%bytevector-u8-suffix?)
  (sequence-prefix-ci?			bytevector-u8-prefix-ci?)
  (%sequence-prefix-ci?			%bytevector-u8-prefix-ci?)
  (sequence-suffix-ci?			bytevector-u8-suffix-ci?)
  (%sequence-suffix-ci?			%bytevector-u8-suffix-ci?)

  ;; searching
  (sequence-index			bytevector-u8-index)
  (%sequence-index			%bytevector-u8-index)
  (sequence-index-right			bytevector-u8-index-right)
  (%sequence-index-right		%bytevector-u8-index-right)
  (sequence-skip			bytevector-u8-skip)
  (%sequence-skip			%bytevector-u8-skip)
  (sequence-skip-right			bytevector-u8-skip-right)
  (%sequence-skip-right			%bytevector-u8-skip-right)
  (sequence-count			bytevector-u8-count)
  (%sequence-count			%bytevector-u8-count)
  (sequence-contains			bytevector-u8-contains)
  (%sequence-contains			%bytevector-u8-contains)
  (sequence-contains-ci			bytevector-u8-contains-ci)
  (%sequence-contains-ci		%bytevector-u8-contains-ci)

  ;; filtering
  (sequence-delete			bytevector-u8-delete)
  (%sequence-delete			%bytevector-u8-delete)
  (sequence-filter			bytevector-u8-filter)
  (%sequence-filter			%bytevector-u8-filter)

  ;; sequences and lists
  (sequence->list*			bytevector->u8-list*)
  (%sequence->list*			%bytevector->u8-list*)
  (reverse-sequence->list		reverse-bytevector->u8-list)
  (%reverse-sequence->list		%reverse-bytevector->u8-list)
  (sequence-tokenize			bytevector-u8-tokenize)
  (%sequence-tokenize			%bytevector-u8-tokenize)
  (sequence-join			bytevector-u8-join)
  (%sequence-join			%bytevector-u8-join)

  ;; replicating
  (xsubsequence				xsubbytevector-u8)
  (%xsubsequence			%xsubbytevector-u8)
  (sequence-xcopy!			bytevector-u8-xcopy!)
  (%sequence-xcopy!			%bytevector-u8-xcopy!)

  ;; concatenate, reverse, replace, fill
  (sequence-concatenate-reverse		bytevector-u8-concatenate-reverse)
  (%sequence-concatenate-reverse	%bytevector-u8-concatenate-reverse)
  (sequence-replace			bytevector-u8-replace)
  (%sequence-replace			%bytevector-u8-replace)
  (sequence-reverse			bytevector-u8-reverse)
  (%sequence-reverse			%bytevector-u8-reverse)
  (sequence-reverse!			bytevector-u8-reverse!)
  (%sequence-reverse!			%bytevector-u8-reverse!)
  (sequence-fill*!			bytevector-u8-fill*!)
  (%sequence-fill*!			%bytevector-u8-fill*!)))


;;;; done

)

;;; end of file
