;;;
;;;Part of: Vicare Scheme
;;;Contents: knuth-morris-pratt search algorithm
;;;Date: Thu Jun 18, 2009
;;;
;;;Knuth-Morris-Pratt string searching. See:
;;;
;;;  "Fast pattern matching in strings"
;;;  SIAM J. Computing 6(2):323-350 1977
;;;  D. E. Knuth, J. H. Morris and V. R. Pratt
;;;
;;;also described in:
;;;
;;;  "Pattern matching in strings"
;;;  Alfred V. Aho
;;;  Formal Language Theory - Perspectives and Open Problems
;;;  Ronald V. Brook (editor)
;;;
;;;and of course:
;;;
;;;  <http://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm>
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare containers knuth-morris-pratt)
  (export
    %kmp-search %kmp-make-restart-vector
    %kmp-step %kmp-partial-search)
  (import (rnrs))


(define (%kmp-search item= item-ref
		     text text-start text-past
		     pattern pattern-start pattern-past)
  (let ((plen (- pattern-past pattern-start))
	(restart-vector (%kmp-make-restart-vector item= item-ref
						  pattern pattern-start pattern-past)))
    ;; The search loop. TJ & PJ are redundant state.
    (let loop ((ti text-start) (pi 0)
	       (tj (- text-past text-start)) ; (- tlen ti) -- how many chars left.
	       (pj plen)) ; (- plen pi) -- how many chars left.
      (if (= pi plen)
	  (- ti plen)			     ; Win.
	(and (<= pj tj)			     ; Lose.
	     (if (item= (item-ref text ti) ; Search.
			(item-ref pattern (+ pattern-start pi)))
		 (loop (+ 1 ti) (+ 1 pi) (- tj 1) (- pj 1)) ; Advance.
	       (let ((pi (vector-ref restart-vector pi))) ; Retreat.
		 (if (= pi -1)
		     (loop (+ ti 1) 0  (- tj 1) plen) ; Punt.
		   (loop ti       pi tj       (- plen pi))))))))))

(define (%kmp-make-restart-vector item= item-ref
				  pattern pattern-start pattern-past)
  (let* ((rvlen (- pattern-past pattern-start))
	 (restart-vector (make-vector rvlen -1)))
    (when (> rvlen 0)
      (let ((rvlen-1 (- rvlen 1))
	    (c0 (item-ref pattern pattern-start)))
	;;Here's the main loop.  We have  set RV[0] ...  RV[i].  K = I
	;;+ START -- it is the corresponding index into PATTERN.
	(let loop1 ((i 0) (j -1) (k pattern-start))
	  (when (< i rvlen-1)
	    ;; loop2 invariant:
	    ;;   pat[(k-j) .. k-1] matches pat[start .. start+j-1]
	    ;;   or j = -1.
	    (let loop2 ((j j))
	      (cond ((= j -1)
		     (let ((i1 (+ 1 i)))
		       (when (not (item= (item-ref pattern (+ k 1)) c0))
			 (vector-set! restart-vector i1 0))
		       (loop1 i1 0 (+ k 1))))
		    ;; pat[(k-j) .. k] matches pat[start..start+j].
		    ((item= (item-ref pattern k) (item-ref pattern (+ j pattern-start)))
		     (let* ((i1 (+ 1 i))
			    (j1 (+ 1 j)))
		       (vector-set! restart-vector i1 j1)
		       (loop1 i1 j1 (+ k 1))))

		    (else (loop2 (vector-ref restart-vector j)))))))))
    restart-vector))

(define (%kmp-step item= item-ref
		   restart-vector next-item-from-text
		   next-index-in-pattern pattern pattern-start)
  (let loop ((i next-index-in-pattern))
    (if (item= next-item-from-text (item-ref pattern (+ i pattern-start)))
	(+ i 1)				       ; Done.
      (let ((i (vector-ref restart-vector i))) ; Back up in PATTERN.
	(if (= i -1)
	    0		;;Can't back up  further, return the first index
			;;in pattern from the start.
	  (loop i))))))	;Keep trying for match.

(define (%kmp-partial-search item= item-ref
			     restart-vector
			     next-index-in-pattern
			     text text-start text-end
			     pattern pattern-start)
  (let ((patlen (vector-length restart-vector)))
    (let loop ((ti text-start)
	       (pi next-index-in-pattern))
      (cond ((= pi patlen) (- ti)) ; found
	    ((= ti text-end) pi)   ; consumed all text
	    (else
	     (let ((c (item-ref text ti)))
	       (loop (+ ti 1)
		     ;;The  following  loop  is  an inlined  version  of
		     ;;%KMP-STEP.
		     (let loop2 ((pi pi))
		       (if (item= c (item-ref pattern (+ pi pattern-start)))
			   (+ pi 1)
			 (let ((pi (vector-ref restart-vector pi)))
			   (if (= pi -1) 0
			     (loop2 pi))))))))))))


;;;; done

)

;;; end of file
