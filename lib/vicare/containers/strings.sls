;;;
;;;Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers strings)
  (export

    ;; auxiliary syntaxes
    view start past

    ;; constructors
    string-concatenate  string-concatenate-reverse  string-tabulate

    ;; predicates
    string-null?  string-every  string-any

    ;; comparison
    string-compare  string-compare-ci
    string=  string<>  string-ci=  string-ci<>
    string<  string<=  string-ci<  string-ci<=
    string>  string>=  string-ci>  string-ci>=

    ;; dictionary comparison
    (rename (%string-dictionary-compare		string-dictionary-compare)
	    (%string-dictionary=?		string-dictionary=?)
	    (%string-dictionary<>?		string-dictionary<>?)
	    (%string-dictionary<?		string-dictionary<?)
	    (%string-dictionary<=?		string-dictionary<=?)
	    (%string-dictionary>?		string-dictionary>?)
	    (%string-dictionary>=?		string-dictionary>=?)

	    (%string-dictionary-compare-ci	string-dictionary-compare-ci)
	    (%string-dictionary-ci=?		string-dictionary-ci=?)
	    (%string-dictionary-ci<>?		string-dictionary-ci<>?)
	    (%string-dictionary-ci<?		string-dictionary-ci<?)
	    (%string-dictionary-ci<=?		string-dictionary-ci<=?)
	    (%string-dictionary-ci>?		string-dictionary-ci>?)
	    (%string-dictionary-ci>=?		string-dictionary-ci>=?))

    ;; string/numbers lexicographic comparison
    (rename (%string/numbers-compare		string/numbers-compare)
	    (%string/numbers=?			string/numbers=?)
	    (%string/numbers<>?			string/numbers<>?)
	    (%string/numbers<?			string/numbers<?)
	    (%string/numbers<=?			string/numbers<=?)
	    (%string/numbers>?			string/numbers>?)
	    (%string/numbers>=?			string/numbers>=?)
	    (%string/numbers-compare-ci		string/numbers-compare-ci)
	    (%string/numbers-ci=?		string/numbers-ci=?)
	    (%string/numbers-ci<>?		string/numbers-ci<>?)
	    (%string/numbers-ci<?		string/numbers-ci<?)
	    (%string/numbers-ci>?		string/numbers-ci>?)
	    (%string/numbers-ci<=?		string/numbers-ci<=?)
	    (%string/numbers-ci>=?		string/numbers-ci>=?))

    ;; string/numbers dictionary comparison
    (rename (%string/numbers-dictionary-compare		string/numbers-dictionary-compare)
	    (%string/numbers-dictionary=?		string/numbers-dictionary=?)
	    (%string/numbers-dictionary<>?		string/numbers-dictionary<>?)
	    (%string/numbers-dictionary<?		string/numbers-dictionary<?)
	    (%string/numbers-dictionary<=?		string/numbers-dictionary<=?)
	    (%string/numbers-dictionary>?		string/numbers-dictionary>?)
	    (%string/numbers-dictionary>=?		string/numbers-dictionary>=?)
	    (%string/numbers-dictionary-compare-ci	string/numbers-dictionary-compare-ci)
	    (%string/numbers-dictionary-ci=?		string/numbers-dictionary-ci=?)
	    (%string/numbers-dictionary-ci<>?		string/numbers-dictionary-ci<>?)
	    (%string/numbers-dictionary-ci<?		string/numbers-dictionary-ci<?)
	    (%string/numbers-dictionary-ci>?		string/numbers-dictionary-ci>?)
	    (%string/numbers-dictionary-ci<=?		string/numbers-dictionary-ci<=?)
	    (%string/numbers-dictionary-ci>=?		string/numbers-dictionary-ci>=?))

    ;; mapping
    string-map     string-map!
    string-map*    string-map*!    string-for-each*
    substring-map  substring-map!  substring-for-each  substring-for-each-index

    ;; case
    string-downcase*   string-upcase*   string-titlecase*
    string-downcase*!  string-upcase*!  string-titlecase*!

    ;; folding
    string-fold-left		string-fold-right
    string-fold-left*		string-fold-right*
    substring-fold-left		substring-fold-right
    string-unfold		string-unfold-right

    ;; selecting
    substring*
    (rename (substring* string-copy*)) string-reverse-copy*
    string-copy*!  string-reverse-copy*!
    string-take    string-take-right
    string-drop    string-drop-right

    ;; padding and trimming
    string-trim  string-trim-right  string-trim-both
    string-pad   string-pad-right

    ;; prefix and suffix
    string-prefix-length  string-prefix-length-ci
    string-suffix-length  string-suffix-length-ci
    string-prefix?        string-prefix-ci?
    string-suffix?        string-suffix-ci?

    ;; searching
    string-index		string-index-right
    string-skip			string-skip-right
    string-contains		string-contains-ci
    string-count
    string-search-and-replace	string-search-and-replace-ci

    ;; filtering
    string-filter string-delete

    ;; lists
    string->list*  reverse-list->string
    string-join    string-tokenize
    (rename (string-tokenize string-tokenise))

    ;; replicating
    xsubstring  string-xcopy!

    ;; mutating
    string-fill*! string-swap!

    ;; reverse and replace
    string-reverse  string-reverse!
    string-replace

    ;; misc
    word-frequency

    (rename (unpack %string-unpack)))
  (import (vicare)
    (vicare containers strings low)
    (vicare containers auxiliary-syntaxes))


(define-syntax unpack
  (syntax-rules (view start past)

    ((_ (view ?str))
     (let ((str ?str))
       (values str 0 (string-length str))))

    ((_ (view ?str (start ?start)))
     (let* ((S   ?str)
	    (sta ?start)
	    (len (string-length S)))
       (values S (normalise-index sta len) len)))

    ((_ (view ?str (past ?past)))
     (let* ((S   ?str)
	    (pas ?past))
       (values S 0 (normalise-index pas (string-length S)))))

    ((_ (view ?str (start ?start) (past ?past)))
     (let* ((S   ?str)
	    (sta ?start)
	    (pas ?past)
	    (len (string-length S)))
       (values S (normalise-index sta len) (normalise-index pas len))))

    ((?F ?str)
     (let ((S ?str))
       (values S 0 (string-length S))))

    ((?F ?stuff ...)
     (syntax-violation #f "invalid parameters" (?stuff ...)))))

(define-syntax normalise-index
  (syntax-rules ()
    ((_ ?idx ?len)
     (if (negative? ?idx)
	 (+ ?idx ?len)
       ?idx))))


;;;; predicates

(define-syntax string-every
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-every ?proc str beg past)))))

(define-syntax string-any
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-any ?proc str beg past)))))


;;;; comparison

(define-syntax string-compare
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-compare str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))

(define-syntax string-compare-ci
  (syntax-rules ()
    ((_ ?S1 ?S2 ?proc< ?proc= ?proc>)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-compare-ci str1 start1 end1 str2 start2 end2 ?proc< ?proc= ?proc>)))))

;;; --------------------------------------------------------------------

(define-syntax string=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string= str1 start1 end1 str2 start2 end2)))))

(define-syntax string<>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string<> str1 start1 end1 str2 start2 end2)))))

(define-syntax string<
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string< str1 start1 end1 str2 start2 end2)))))

(define-syntax string>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string> str1 start1 end1 str2 start2 end2)))))

(define-syntax string<=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string<= str1 start1 end1 str2 start2 end2)))))

(define-syntax string>=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string>= str1 start1 end1 str2 start2 end2)))))

;;; --------------------------------------------------------------------

(define-syntax string-ci=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-ci= str1 start1 end1 str2 start2 end2)))))

(define-syntax string-ci<>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-ci<> str1 start1 end1 str2 start2 end2)))))

(define-syntax string-ci<
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-ci< str1 start1 end1 str2 start2 end2)))))

(define-syntax string-ci>
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-ci> str1 start1 end1 str2 start2 end2)))))

(define-syntax string-ci<=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-ci<= str1 start1 end1 str2 start2 end2)))))

(define-syntax string-ci>=
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-ci>= str1 start1 end1 str2 start2 end2)))))


;;;; mapping

(define-syntax substring-map
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-map ?proc str beg past)))))

(define-syntax substring-map!
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-map! ?proc str beg past)))))

(define-syntax substring-for-each
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-for-each ?proc str beg past)))))

(define-syntax substring-for-each-index
  (syntax-rules ()
    ((_ ?proc ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-for-each-index ?proc str beg past)))))


;;;; case hacking

(define-syntax string-upcase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-map char-upcase str beg past)))))

(define-syntax string-upcase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-map! char-upcase str beg past)))))

(define-syntax string-downcase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-map char-downcase str beg past)))))

(define-syntax string-downcase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-map! char-downcase str beg past)))))

(define-syntax string-titlecase*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (let ((ans (substring str beg past)))
	 (%string-titlecase*! ans 0 (- past beg))
	 ans)))))

(define-syntax string-titlecase*!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-titlecase*! str beg past)))))


;;;; folding

(define-syntax substring-fold-left
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-fold-left ?kons ?knil str beg past)))))

(define-syntax substring-fold-right
  (syntax-rules ()
    ((?F ?kons ?knil ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%substring-fold-right ?kons ?knil str beg past)))))


;;;; selecting

(define-syntax substring*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (substring str beg past)))))

(define-syntax string-reverse-copy*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-reverse-copy* str beg past)))))

(define-syntax string-copy*!
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-copy*! str1 beg1 str2 beg2 past2)))))

(define-syntax string-reverse-copy*!
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-reverse-copy*! str1 beg1 str2 beg2 past2)))))

(define-syntax string-take
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%string-take nchars str beg past)))))

(define-syntax string-take-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%string-take-right nchars str beg past)))))

(define-syntax string-drop
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%string-drop nchars str beg past)))))

(define-syntax string-drop-right
  (syntax-rules ()
    ((_ ?S nchars)
     (let-values (((str beg past) (unpack ?S)))
       (%string-drop-right nchars str beg past)))))

(define-syntax string-trim
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-trim criterion str beg past)))))

(define-syntax string-trim-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-trim-right criterion str beg past)))))

(define-syntax string-trim-both
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-trim-both criterion str beg past)))))

(define-syntax string-pad
  (syntax-rules ()
    ((_ ?S ?len)
     (string-pad ?S ?len #\space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%string-pad ?len ?char str beg past)))))

(define-syntax string-pad-right
  (syntax-rules ()
    ((_ ?S ?len)
     (string-pad-right ?S ?len #\space))
    ((_ ?S ?len ?char)
     (let-values (((str beg past) (unpack ?S)))
       (%string-pad-right ?len ?char str beg past)))))


;;;; prefix and suffix

(define-syntax string-prefix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-prefix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-suffix-length
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-suffix-length str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-prefix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-prefix-length-ci str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-suffix-length-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-suffix-length-ci str1 beg1 past1 str2 beg2 past2)))))

;;; --------------------------------------------------------------------

(define-syntax string-prefix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-prefix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-suffix?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-suffix? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-prefix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-prefix-ci? str1 beg1 past1 str2 beg2 past2)))))

(define-syntax string-suffix-ci?
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 beg1 past1) (unpack ?S1))
		  ((str2 beg2 past2) (unpack ?S2)))
       (%string-suffix-ci? str1 beg1 past1 str2 beg2 past2)))))


;;;; searching

(define-syntax string-index
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-index criterion str beg past)))))

(define-syntax string-index-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-index-right criterion str beg past)))))

(define-syntax string-skip
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-skip criterion str beg past)))))

(define-syntax string-skip-right
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-skip-right criterion str beg past)))))

(define-syntax string-count
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-count criterion str beg past)))))

(define-syntax string-contains
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-contains str1 start1 end1 str2 start2 end2)))))

(define-syntax string-contains-ci
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2)))
       (%string-contains-ci str1 start1 end1 str2 start2 end2)))))

(define-syntax string-search-and-replace
  (syntax-rules ()
    ((_ ?S1 ?S2 ?S3 ?max-replacement-count)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2))
		  ((str3 start3 end3) (unpack ?S3)))
       (%string-search-and-replace str1 start1 end1
				   str2 start2 end2
				   str3 start3 end3
				   ?max-replacement-count)))))

(define-syntax string-search-and-replace-ci
  (syntax-rules ()
    ((_ ?S1 ?S2 ?S3 ?max-replacement-count)
     (let-values (((str1 start1 end1) (unpack ?S1))
		  ((str2 start2 end2) (unpack ?S2))
		  ((str3 start3 end3) (unpack ?S3)))
       (%string-search-and-replace-ci str1 start1 end1
				      str2 start2 end2
				      str3 start3 end3
				      ?max-replacement-count)))))


;;;; filtering

(define-syntax string-delete
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-delete criterion str beg past)))))

(define-syntax string-filter
  (syntax-rules ()
    ((_ ?S criterion)
     (let-values (((str beg past) (unpack ?S)))
       (%string-filter criterion str beg past)))))


;;;; strings and lists

(define-syntax string->list*
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string->list* str beg past)))))

(define-syntax reverse-string->list
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%reverse-string->list str beg past)))))

(define-syntax string-tokenize
  (syntax-rules ()
    ((_ ?S ?token-set)
     (let-values (((str beg past) (unpack ?S)))
       (%string-tokenize ?token-set str beg past)))))

(define string-join
  (case-lambda
   ((strings)
    (%string-join strings " " 'infix))
   ((strings delim)
    (%string-join strings delim 'infix))
   ((strings delim grammar)
    (%string-join strings delim grammar))))


;;; replicating

(define-syntax xsubstring
  (syntax-rules ()
    ((_ ?S ?from ?to)
     (let-values (((str beg past) (unpack ?S)))
       (%xsubstring ?from ?to str beg past)))))

(define-syntax string-xcopy!
  (syntax-rules ()
    ((_ ?T ?S ?from ?to)
     (let-values (((str1 beg1 past1) (unpack ?T))
		  ((str2 beg2 past2) (unpack ?S)))
       (%string-xcopy! ?from ?to str1 beg1 past1 str2 beg2 past2)))))


;;; concatenate, reverse, replace, fill

(define string-concatenate-reverse
  (case-lambda

   ((string-list)
    (%string-concatenate-reverse string-list "" 0))

   ((string-list final)
    (%string-concatenate-reverse string-list final (string-length final)))

   ((string-list final past)
    (%string-concatenate-reverse string-list final past))))

(define-syntax string-replace
  (syntax-rules ()
    ((_ ?S1 ?S2)
     (let-values (((str1 start1 past1) (unpack ?S1))
		  ((str2 start2 past2) (unpack ?S2)))
       (%string-replace str1 start1 past1 str2 start2 past2)))))

(define-syntax string-reverse
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-reverse str beg past)))))

(define-syntax string-reverse!
  (syntax-rules ()
    ((_ ?S)
     (let-values (((str beg past) (unpack ?S)))
       (%string-reverse! str beg past)))))

(define-syntax string-fill*!
  (syntax-rules ()
    ((_ ?S ?fill-char)
     (let-values (((str beg past) (unpack ?S)))
       (%string-fill*! ?fill-char str beg past)))))


;;;; miscellaneous

(define (word-frequency getter)
  (let ((table (make-hashtable string-hash string=?)))
    (let loop ((word (getter)))
      (if word
	  (begin
	    (hashtable-set! table word (+ 1 (hashtable-ref table word 0)))
	    (loop (getter)))
	table))))


;;;; done

)

;;; end of file
