;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;copyright notices for the implementations of:
;;;
;;;   vector-fold-left
;;;   vector-fold-right
;;;
;;;Copyright (c) 2008-2010, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Taylor Campbell wrote this code; he places it in the public domain.
;;;Modified by Derick Eddington to be included into an R6RS library.
;;;Modified by Marco Maggi to be included in Nausicaa.
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
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.

;;;Many  functions  are  derived  from  the SRFI  13  (strings  library)
;;;reference implementation.  Its copyright notices are below.
;;;
;;;Olin Shivers 7/2000
;;;
;;;Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;;Copyright (c) 1998, 1999, 2000 Olin Shivers.  All rights reserved.
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


(library (ikarus vectors)
  (export
    list-of-vectors?
    make-vector		vector
    subvector		vector-length
    vector-empty?	non-empty-vector?
    vectors-of-same-length?
    list-of-vectors-of-same-length?
    vector-ref		vector-set!
    vector->list	list->vector
    vector-map		vector-for-each
    vector-find
    vector-for-all	vector-exists
    vector-fold-left	vector-fold-right
    vector-fill!	vector-append
    vector-copy		vector-copy!
    vector-resize	vector-reset!

    ;; unsafe operations
    $make-clean-vector
    $vector-empty?
    $vector-map1
    $vector-for-each1
    $vector-for-all1
    $vector-exists1
    $subvector
    $vectors-of-same-length?
    $vector-self-copy-forwards!
    $vector-self-copy-backwards!
    $vector-copy-source-range!
    $vector-copy-source-count!
    $vector-set-void!
    $fill-vector-from-list!)
  (import (except (vicare)
		  list-of-vectors?
		  make-vector		vector
		  subvector		vector-length
		  vector-empty?		non-empty-vector?
		  vectors-of-same-length?
		  list-of-vectors-of-same-length?
		  vector-ref		vector-set!
		  vector->list		list->vector
		  vector-map		vector-for-each
		  vector-find
		  vector-for-all	vector-exists
		  vector-fold-left	vector-fold-right
		  vector-fill!		vector-append
		  vector-copy		vector-copy!
		  vector-resize		vector-reset!)
    (vicare system $fx)
    (vicare system $pairs)
    (except (vicare system $vectors)
	    $make-clean-vector
	    $subvector
	    $vector-empty?
	    $vector-map1
	    $vector-for-each1
	    $vector-for-all1
	    $vector-exists1
	    $vectors-of-same-length?
	    $vector-self-copy-forwards!
	    $vector-self-copy-backwards!
	    $vector-copy-source-range!
	    $vector-copy-source-count!
	    $fill-vector-from-list!)
    (only (vicare language-extensions syntaxes)
	  define-list-of-type-predicate))


;;;; arguments validation

(define-syntax (preconditions stx)
  (module (vicare-built-with-arguments-validation-enabled)
    (module (arguments-validation)
      (include "ikarus.config.scm" #t))
    (define (vicare-built-with-arguments-validation-enabled)
      arguments-validation)
    #| end of module |# )
  (syntax-case stx ()
    ;;Single precondition.
    ;;
    ((_ (?predicate ?arg ...))
     (identifier? #'?who)
     (if (vicare-built-with-arguments-validation-enabled)
	 #'(unless (?predicate ?arg ...)
	     (procedure-arguments-consistency-violation __who__
	       "failed precondition"
	       '(?predicate ?arg ...) ?arg ...))
       #'(void)))

    ;;Multiple preconditions.
    ;;
    ((_ (?predicate ?arg ...) ...)
     (identifier? #'?who)
     (if (vicare-built-with-arguments-validation-enabled)
	 #'(begin
	     (preconditions (?predicate ?arg ...))
	     ...)
       #'(void)))
    ))

;;; --------------------------------------------------------------------

(define-syntax-rule (valid-index-for-vector-slot vec idx)
  ;;Assume VEC and IDX have been already validated as vector and non-negative fixnum.
  ;;
  (unless ($fx< idx ($vector-length vec))
    (procedure-arguments-consistency-violation __who__
      (format "index ~a out of range for vector of length ~a"
	idx ($vector-length vec))
      vec idx)))

(define-syntax-rule (total-vector-length-is-a-fixnum total-length)
  (unless (fixnum? total-length)
    (procedure-arguments-consistency-violation __who__
      "total vector length greater than maximum vector length" total-length)))

(define-syntax-rule (valid-start-index-for-vector-slot vec idx)
  ;;Assume VEC and IDX have been already validated as vector and non-negative fixnum.
  ;;
  (unless ($fx<= idx ($vector-length vec))
    (procedure-arguments-consistency-violation __who__
      (format "start index ~a out of range for vector of length ~a" idx ($vector-length vec))
      vec idx)))

(define-syntax-rule (valid-end-index-for-vector-slot vec idx)
  ;;Assume VEC and IDX have been already validated as vector and non-negative fixnum.
  ;;
  (unless ($fx<= idx ($vector-length vec))
    (procedure-arguments-consistency-violation __who__
      (format "end index ~a out of range for vector of length ~a" idx ($vector-length vec))
      vec idx)))

(define-syntax-rule (start-and-end-indexes-in-correct-order start end)
  ;;Assume START and END have been already validated as non-negative fixnums.
  ;;
  (unless ($fx<= start end)
    (procedure-arguments-consistency-violation __who__
      (format "start index ~a and end index ~a in wrong order" start end)
      start end)))

(define-syntax-rule (valid-start-index-and-slot-count-for-vector vec idx count)
  ;;Assume  VEC  and  IDX  and  COUNT  have been  already  validated  as  vector  and
  ;;non-negative fixnums.
  ;;
  (let ((end (+ idx count)))
    (unless (fixnum? end)
      (procedure-arguments-consistency-violation __who__
	(format "start index ~a and slot count ~a out of range for maximum vector length" idx count)
	idx count))
    (unless ($fx<= end ($vector-length vec))
      (procedure-arguments-consistency-violation __who__
	(format "start index ~a and slot count ~a out of range for vector of length ~a" idx count ($vector-length vec))
	vec idx count))))

(define-syntax vectors-of-same-length
  (syntax-rules ()
    ((_ ?v0 ?v1)
     (unless ($vectors-of-same-length? ?v0 ?v1)
       (procedure-arguments-consistency-violation __who__
	 "expected vectors of same length" ?v0 ?v1)))
    ((_ ?v0 ?v1 ?v*)
     (unless (apply $vectors-of-same-length? ?v0 ?v1 ?v*)
       (procedure-arguments-consistency-violation __who__
	 "expected vectors of same length" ?v0 ?v1 ?v*)))
    ))


;;;; helpers

(define-syntax-rule (with-who ?name . ?body)
  (fluid-let-syntax
      ((__who__ (identifier-syntax (quote ?name))))
    . ?body))


;;;; common unsafe operations

(define ($make-clean-vector len)
  (foreign-call "ikrt_make_vector1" len))

(define-syntax-rule ($vector-clean! ?vec)
  (foreign-call "ikrt_vector_clean" ?vec))

(case-define $vectors-of-same-length?
  ((v0 v1)
   ($fx= ($vector-length v0)
	 ($vector-length v1)))
  ((v0 v1 v2)
   (let ((len0 ($vector-length v0)))
     (and ($fx= len0 ($vector-length v1))
	  ($fx= len0 ($vector-length v2)))))
  ((v0 . v*)
   (let ((len ($vector-length v0)))
     (for-all (lambda (V)
		($fx= len ($vector-length V)))
       v*)))
  #| end of CASE-DEFINE |# )

(define ($vector-copy-source-range! src.vec src.start src.end
				    dst.vec dst.start)
  ;;Tail-recursive function.   Copy items  from SRC.VEC  to DST.VEC;  return DST.VEC.
  ;;Copy the items from the source  slots from index SRC.START (inclusive) to SRC.END
  ;;(exclusive), in the destination slots starting at index DST.START.
  ;;
  (if ($fx< src.start src.end)
      (begin
	($vector-set! dst.vec dst.start ($vector-ref src.vec src.start))
	($vector-copy-source-range! src.vec ($fxadd1 src.start) src.end
				    dst.vec ($fxadd1 dst.start)))
    dst.vec))

(define ($vector-copy-source-count! src.vec src.start dst.vec dst.start count)
  ;;Tail-recursive  function.   Copy COUNT  items  from  SRC.VEC to  DST.VEC;  return
  ;;DST.VEC.  Copy  the items the source  slots at index SRC.START  inclusive, to the
  ;;destination slots at index DST.START inclusive.
  ;;
  (if ($fxpositive? count)
      (begin
	($vector-set! dst.vec dst.start ($vector-ref src.vec src.start))
	($vector-copy-source-count! src.vec ($fxadd1 src.start) dst.vec ($fxadd1 dst.start) ($fxsub1 count)))
    dst.vec))

(define ($vector-self-copy-forwards! vec src.start dst.start count)
  ;;Copy  COUNT items  of VEC  from  SRC.START inclusive  to VEC  itself starting  at
  ;;DST.START inclusive.  The  copy happens forwards, so it is  suitable for the case
  ;;SRC.START > DST.START.
  ;;
  (if ($fxpositive? count)
      (begin
	($vector-set! vec dst.start ($vector-ref vec src.start))
	($vector-self-copy-forwards! vec ($fxadd1 src.start) ($fxadd1 dst.start) ($fxsub1 count)))
    vec))

(define ($vector-self-copy-backwards! vec src.end dst.end count)
  ;;Copy COUNT items of VEC from SRC.END  exclusive to VEC itself starting at DST.END
  ;;exclusive.  The copy happens backwards, so it  is suitable for the case SRC.END <
  ;;DST.END.
  ;;
  (if ($fxpositive? count)
      (let ((src.idx ($fxsub1 src.end))
	    (dst.idx ($fxsub1 dst.end)))
	;;FIXME I tried using $VECTOR-SET!  and  $VECTOR-REF here, but it appears the
	;;unsafe operations trigger a compiler bug that causes wrong code generation.
	;;Everything works fine using the safe  operations.  (Marco Maggi; Thu May 7,
	;;2015)
	(vector-set! vec dst.idx (vector-ref vec src.idx))
	($vector-self-copy-backwards! vec src.idx dst.idx ($fxsub1 count)))
    vec))

(define ($subvector src.vec src.start src.end)
  ;;Return  a new  vector  holding items  from SRC.VEC  from  SRC.START inclusive  to
  ;;SRC.END exclusive.
  ;;
  (let ((dst.len ($fx- src.end src.start)))
    (if ($fxpositive? dst.len)
	(receive-and-return (dst.vec)
	    ($make-clean-vector dst.len)
	  ($vector-copy-source-range! src.vec src.start src.end dst.vec 0))
      ($make-vector 0))))

(define ($fill-vector-from-list! v i ls)
  ;;Fill the vector V with items from the proper list LS, starting at index I.
  ;;
  (if (pair? ls)
      (begin
	($vector-set! v i (car ls))
	($fill-vector-from-list! v ($fxadd1 i) (cdr ls)))
    v))


;;;; predicates and vector length

(define-list-of-type-predicate list-of-vectors? vector?)

(define* (vector-length {vec vector?})
  ;;Defined by R6RS.   Return the number of elements in  VEC as an exact
  ;;integer object.
  ;;
  ($vector-length vec))

(case-define* vectors-of-same-length?
  (({v0 vector?} {v1 vector?})
   ($fx= ($vector-length v0)
	 ($vector-length v1)))
  (({v0 vector?} {v1 vector?} {v2 vector?})
   (let ((len0 ($vector-length v0)))
     (and ($fx= len0 ($vector-length v1))
	  ($fx= len0 ($vector-length v2)))))
  (({v0 vector?} . {v* vector?})
   (let ((len ($vector-length v0)))
     (for-all (lambda (V)
		($fx= len ($vector-length V)))
       v*)))
  #| end of CASE-DEFINE |# )

(define (list-of-vectors-of-same-length? obj)
  (and (pair? obj)
       (and (vector? (car obj))
	    (let ((len ($vector-length (car obj))))
	      (let loop ((ell (cdr obj)))
		(if (pair? ell)
		    (and (vector? (car ell))
			 ($fx= len ($vector-length (car ell)))
			 (loop (cdr ell)))
		  (null? ell)))))))


;;;; constructors

(case-define* make-vector
  ;;Defined by R6RS.  Return  a newly allocated vector of LEN  elements.  If a second
  ;;argument  is given,  then each  element is  initialized to  FILL.  Otherwise  the
  ;;initial contents of each element is unspecified.
  ;;
  ((len)
   ($vector-fill-range! ($make-clean-vector len) 0 len (void)))
  (({len non-negative-fixnum?} fill)
   ($vector-fill-range! ($make-clean-vector len) 0 len fill))
  #| end of CASE-DEFINE* |# )

(case-define* vector
  ;;Defined by  R6RS.  Return  a newly  allocated vector  whose elements  contain the
  ;;given arguments.  Analogous to LIST.
  ;;
  (()
   ($make-vector 0))

  ((one)
   (receive-and-return (vec)
       ($make-clean-vector 1)
     ($vector-set! vec 0 one)))

  ((one two)
   (receive-and-return (vec)
       ($make-clean-vector 2)
     ($vector-set! vec 0 one)
     ($vector-set! vec 1 two)))

  ((one two three)
   (receive-and-return (vec)
       ($make-clean-vector 3)
     ($vector-set! vec 0 one)
     ($vector-set! vec 1 two)
     ($vector-set! vec 2 three)))

  ((one two three four)
   (receive-and-return (vec)
       ($make-clean-vector 4)
     ($vector-set! vec 0 one)
     ($vector-set! vec 1 two)
     ($vector-set! vec 2 three)
     ($vector-set! vec 3 four)))

  (arg*
   (let ((len (let loop ((ls arg*) (len 0))
		(if (pair? ls)
		    (loop ($cdr ls) ($fxadd1 len))
		  len))))
     (preconditions
      (total-vector-length-is-a-fixnum len))
     ($fill-vector-from-list! ($make-clean-vector len) 0 arg*)))

  #| end of CASE-DEFINE* |# )


;;;; filling

(define* (vector-fill! {vec vector?} fill)
  ;;Defined by R6RS.  Store FILL in every element of VEC and returns unspecified.
  ;;
  ($vector-fill-range! vec 0 ($vector-length vec) fill))

(define ($vector-fill-range! vec start end fill)
  ;;Set to  FILL all  the slots in  VEC in  the range from  START (inclusive)  to END
  ;;(exclusive).  Return VEC.
  ;;
  (if ($fx< start end)
      (begin
	($vector-set! vec start fill)
	($vector-fill-range! vec ($fxadd1 start) end fill))
    vec))


(define* (vector-ref {vec vector?} {idx non-negative-fixnum?})
  ;;Defined by  R6RS.  IDX  must be  a valid index  of VEC.   Return the  contents of
  ;;element IDX of VEC.
  ;;
  (preconditions
   (valid-index-for-vector-slot vec idx))
  ($vector-ref vec idx))

(define* (vector-set! {vec vector?} {idx non-negative-fixnum?} new-item)
  ;;Defined by R6RS.  IDX must be  a valid index of VEC.  Store NEW-ITEM
  ;;in element IDX of VEC, and return unspecified values.
  ;;
  ;;Passing an immutable vector to VECTOR-SET! should cause an exception
  ;;with condition type "&assertion" to be raised.
  ;;
  (preconditions
   (valid-index-for-vector-slot vec idx))
  ($vector-set! vec idx new-item))

(define ($vector-set-void! vec idx)
  ($vector-set! vec idx (void)))


(define* (vector->list {vec vector?})
  ;;Defined by R6RS.  Return  a newly allocated list of the  objects contained in the
  ;;elements of VEC.
  ;;
  (define (f vec idx ls)
    (if ($fx< idx 0)
	ls
      (f vec ($fxsub1 idx) (cons ($vector-ref vec idx) ls))))
  (let ((len ($vector-length vec)))
    (if ($fxzero? len)
	'()
      (f vec ($fxsub1 len) '()))))

(define* (list->vector ls)
  ;;Defined by  R6RS.  Return a newly  created vector initialized to  the elements of
  ;;the list LS.
  ;;
  (define (race h t ls n)
    (with-who list->vector
      (cond ((pair? h)
	     (let ((h ($cdr h)))
	       (cond ((pair? h)
		      (if (not (eq? h t))
			  (race ($cdr h) ($cdr t) ls ($fx+ n 2))
			(procedure-signature-argument-violation __who__ "circular list is invalid as argument" 1 '(list? ls) ls)))
		     ((null? h)
		      ($fx+ n 1))
		     (else
		      (procedure-signature-argument-violation __who__ "expected proper list as argument" 1 '(list? ls) ls)))))
	    ((null? h)
	     n)
	    (else
	     (procedure-signature-argument-violation __who__ "expected proper list as argument" 1 '(list? ls) ls)))))

  (let ((len (race ls ls ls 0)))
    (preconditions
     (total-vector-length-is-a-fixnum len))
    ($fill-vector-from-list! ($make-clean-vector len) 0 ls)))


(module (vector-map)
  ;;Defined  by R6RS.   The  vector  arguments must  all  have the  same
  ;;length.  The procedure should accept  as many arguments as there are
  ;;vector arguments and return a single value.
  ;;
  ;;The VECTOR-MAP  procedure applies P element-wise to  the elements of
  ;;the vectors  and returns a  vector of the  results, in order.   P is
  ;;always called in the  same dynamic environment as VECTOR-MAP itself.
  ;;The order  in which P is applied  to the elements of  the vectors is
  ;;unspecified.  If multiple returns  occur from VECTOR-MAP, the return
  ;;values returned by earlier returns are not mutated.
  ;;
  ;;Analogous to MAP.
  ;;
  ;;IMPLEMENTATION  RESPONSIBILITIES The  implementation must  check the
  ;;restrictions  on  P  to  the  extent performed  by  applying  it  as
  ;;described.  An implementation may  check whether P is an appropriate
  ;;argument before applying it.
  ;;

  (define (ls->vec ls n)
    (let f ((v  ($make-clean-vector n))
	    (n  n)
	    (ls ls))
      (if (null? ls)
	  v
	(let ((n ($fxsub1 n)))
	  ($vector-set! v n ($car ls))
	  (f v n ($cdr ls))))))

  (case-define* vector-map
    ;;Notice that R6RS states:
    ;;
    ;;  "If  multiple returns occur  from VECTOR-MAP,  the return values  returned by
    ;;  earlier returns are not mutated."
    ;;
    ;;so if we jump back into  the mapping procedure using a continuation, VECTOR-MAP
    ;;must  return  a new  vector.   This  behaviour  can  be demonstrated  with  the
    ;;following program:
    ;;
    ;;#!r6rs
    ;;(import (rnrs))
    ;;(let ((only-once #t)
    ;;      (v0 (vector 1 2 3 4 5 6))
    ;;      (cl '())
    ;;      (old-v1 #f))
    ;;  (let ((v1 (vector-map (lambda (elt)
    ;;                          (call/cc
    ;;                              (lambda (c)
    ;;                                (set! cl (cons c cl))
    ;;                                (* elt elt))))
    ;;                        v0)))
    ;;    (when only-once
    ;;      (set! only-once #f)
    ;;      (set! old-v1 v1)
    ;;      ((car (reverse cl)) 'x))
    ;;
    ;;    (write v0)(newline)
    ;;    (write old-v1)(newline)
    ;;    (write v1)(newline)))
    ;;
    ;;which must print:
    ;;
    ;;#(1 2 3 4 5 6)
    ;;#(1 4 9 16 25 36)
    ;;#(x 4 9 16 25 36)
    ;;
    ;;rather than:
    ;;
    ;;#(1 2 3 4 5 6)
    ;;#(x 4 9 16 25 36)  ;; wrong!!!
    ;;#(x 4 9 16 25 36)
    ;;
    (({p procedure?} {v vector?})
     (let f ((p  p)
	     (v  v)
	     (i  0)
	     (n  (vector-length v))
	     (ac '()))
       (if ($fx= i n)
	   (ls->vec ac n)
	 (f p v ($fxadd1 i) n (cons (p (vector-ref v i)) ac)))))

    (({p procedure?} {v0 vector?} {v1 vector?})
     (preconditions
      (vectors-of-same-length v0 v1))
     (let f ((p  p)
	     (v0 v0)
	     (v1 v1)
	     (i  0)
	     (n  ($vector-length v0))
	     (ac '()))
       (if ($fx= i n)
	   (ls->vec ac n)
	 (f p v0 v1 ($fxadd1 i) n
	    (cons (p ($vector-ref v0 i) ($vector-ref v1 i)) ac)))))

    (({p procedure?} {v0 vector?} {v1 vector?} . {v* vector?})
     (preconditions
      (vectors-of-same-length v0 v1 v*))
     (let loop ((i 0) (len (vector-length v0)) (ac '()))
       (if ($fx= i len)
	   (ls->vec ac len)
	 (loop ($fxadd1 i) len
	       (cons (apply p ($vector-ref v0 i) ($vector-ref v1 i)
			    (let inner-loop ((i i) (v* v*))
			      (if (pair? v*)
				  (cons ($vector-ref ($car v*) i)
					(inner-loop i ($cdr v*)))
				'())))
		     ac)))))

    #| end of CASE-DEFINE* |# )

  #| end of module|# )


(case-define* vector-for-each
  ;;Defined  by R6RS.   The vector  arguments  must all  have the  same length.   The
  ;;procedure  P  should  accept  as  many  arguments  as  there  are  vectors.   The
  ;;VECTOR-FOR-EACH procedure applies  P element-wise to the elements  of the vectors
  ;;for its side effects, in order from the  first elements to the last.  P is always
  ;;called in  the same  dynamic environment as  VECTOR-FOR-EACH itself.   The return
  ;;values of VECTOR-FOR-EACH are unspecified.
  ;;
  ;;Analogous to FOR-EACH.
  ;;
  ;;IMPLEMENTATION RESPONSIBILITIES The implementation must check the restrictions on
  ;;P to  the extent performed  by applying it  as described.  An  implementation may
  ;;check whether P is an appropriate argument before applying it.
  ;;
  (({p procedure?} {v vector?})
   (let loop ((i 0) (len ($vector-length v)))
     (if ($fx= i len)
	 ;;We want to make sure that the return value is void.
	 (void)
       (begin
	 (p ($vector-ref v i))
	 (loop ($fxadd1 i) len)))))

  (({p procedure?} {v0 vector?} {v1 vector?})
   (preconditions
    (vectors-of-same-length v0 v1))
   (let loop ((i 0) (len ($vector-length v0)))
     (if ($fx= i len)
	 (void)
       (begin
	 (p ($vector-ref v0 i)
	    ($vector-ref v1 i))
	 (loop ($fxadd1 i) len)))))

  (({p procedure?} {v0 vector?} {v1 vector?} . {v* vector?})
   (preconditions
    (vectors-of-same-length v0 v1 v*))
   (let loop ((i 0) (len ($vector-length v0)))
     (if ($fx= i len)
	 (void)
       (begin
	 (apply p ($vector-ref v0 i) ($vector-ref v1 i)
		(let inner-loop ((i i) (v* v*))
		  (if (pair? v*)
		      (cons ($vector-ref ($car v*) i)
			    (inner-loop i ($cdr v*)))
		    '())))
	 (loop ($fxadd1 i) len)))))

  #| end of CASE-DEFINE* |# )


;;;; iterations

(define* (vector-find {proc procedure?} {vec vector?})
  (define-constant LEN
    (vector-length vec))
  (let loop ((i 0))
    (if (fx=? i LEN)
	#f
      (let ((rv (vector-ref vec i)))
	(if (proc rv)
	    rv
	  (loop (fxadd1 i)))))))

(define-syntax define-vector-iterator
  (syntax-rules ()
    ((_ ?name ?logic-combine)
     (case-define* ?name

       (({proc procedure?} {v0 vector?})
	(let ((len (vector-length v0)))
	  (if ($fxzero? len)
	      (?logic-combine)
	    (let ((len-1 ($fxsub1 len)))
	      (let loop ((i 0))
		(if ($fx= i len-1)
		    (proc ($vector-ref v0 i)) ;tail call deciding the return value
		  (?logic-combine (proc ($vector-ref v0 i))
				  (loop ($fxadd1 i)))))))))

       (({proc procedure?} {v0 vector?} {v1 vector?})
	(preconditions
	 (vectors-of-same-length v0 v1))
	(let ((len (vector-length v0)))
	  (if (zero? len)
	      (?logic-combine)
	    (let ((len-1 ($fxsub1 len)))
	      (let loop ((i 0))
		(if ($fx= i len-1)
		    (proc ($vector-ref v0 i)
			  ($vector-ref v1 i)) ;tail call deciding the return value
		  (?logic-combine (proc ($vector-ref v0 i)
					($vector-ref v1 i))
				  (loop ($fxadd1 i)))))))))

       (({proc procedure?} {v0 vector?} {v1 vector?} . {v* vector?})
	(preconditions
	 (vectors-of-same-length v0 v1 v*))
	(let ((len ($vector-length v0)))
	  (if ($fxzero? len)
	      (?logic-combine)
	    (let ((len-1 (- len 1)))
	      (let loop ((i 0))
		(if ($fx= i len-1)
		    (apply proc v0 v0
			   (map (lambda (vec)
				  ($vector-ref vec i))
			     v*)) ;tail call deciding the return value
		  (?logic-combine (apply proc v0 v1
					 (map (lambda (vec)
						($vector-ref vec i))
					   v*))
				  (loop ($fxadd1 i)))))))))

       #| end of CASE-DEFINE* |# ))
    ))

(define-vector-iterator vector-for-all and)
(define-vector-iterator vector-exists  or)


;;;; folding

(case-define* vector-fold-left

  (({combine procedure?} knil {v0 vector?})
   (let ((len (vector-length v0)))
     (if (fxzero? len)
	 knil
       (let loop ((i     0)
		  (imax  (fxsub1 len))
		  (state knil))
	 (define-syntax-rule (doit ?idx)
	   (combine state (vector-ref v0 ?idx)))
	 (if (fx=? i imax)
	     ;;Tail-call as last COMBINE application.
	     (doit i)
	   (loop (fxadd1 i) imax (doit i)))))))

  (({combine procedure?} knil {v0 vector?} {v1 vector?})
   (preconditions
    (vectors-of-same-length v0 v1))
   (let ((len (vector-length v0)))
     (if (fxzero? len)
	 knil
       (let loop ((i     0)
		  (imax  (fxsub1 len))
		  (state knil))
	 (define-syntax-rule (doit ?idx)
	   (combine state (vector-ref v0 ?idx) (vector-ref v1 ?idx)))
	 (if (fx=? i imax)
	     ;;Tail-call as last COMBINE application.
	     (doit i)
	   (loop (fxadd1 i) imax (doit i)))))))

  (({combine procedure?} knil {v0 vector?} {v1 vector?} . {v* vector?})
   (preconditions
    (vectors-of-same-length v0 v1 v*))
   (let ((len (vector-length v0)))
     (if (fxzero? len)
	 knil
       (let loop ((i     0)
		  (imax  (fxsub1 len))
		  (state knil))
	 (define (doit idx)
	   (apply combine state
		  (vector-ref v0 idx)
		  (vector-ref v1 idx)
		  (map (lambda (vec)
			 (vector-ref vec idx))
		    v*)))
	 (if (fx=? i imax)
	     ;;Tail-call as last COMBINE application.
	     (doit i)
	   (loop (fxadd1 i) imax (doit i)))))))

  #| end of CASE-DEFINE* |# )

;;; --------------------------------------------------------------------


(case-define* vector-fold-right
  (({combine procedure?} knil {v0 vector?})
   (let ((len (vector-length v0)))
     (if (fxzero? len)
	 knil
       (let loop ((i     (fxsub1 len))
		  (imin  0)
		  (state knil))
	 (define-syntax-rule (doit ?idx)
	   (combine (vector-ref v0 ?idx) state))
	 (if (fx=? i imin)
	     ;;Tail-call as last COMBINE application.
	     (doit i)
	   (loop (fxsub1 i) imin (doit i)))))))

  (({combine procedure?} knil {v0 vector?} {v1 vector?})
   (preconditions
    (vectors-of-same-length v0 v1))
   (let ((len (vector-length v0)))
     (if (fxzero? len)
	 knil
       (let loop ((i     (fxsub1 len))
		  (imin  0)
		  (state knil))
	 (define-syntax-rule (doit ?idx)
	   (combine (vector-ref v0 ?idx) (vector-ref v1 ?idx) state))
	 (if (fx=? i imin)
	     ;;Tail-call as last COMBINE application.
	     (doit i)
	   (loop (fxsub1 i) imin (doit i)))))))

  (({combine procedure?} knil {v0 vector?} {v1 vector?} . {v* vector?})
   (preconditions
    (vectors-of-same-length v0 v1 v*))
   (let ((len (vector-length v0)))
     (if (fxzero? len)
	 knil
       (let loop ((i     (fxsub1 len))
		  (imin  0)
		  (state knil))
	 (define (doit idx)
	   (apply combine
		  (vector-ref v0 idx)
		  (vector-ref v1 idx)
		  (fold-right (lambda (vec tail)
				(cons (vector-ref vec idx)
				      tail))
		    (list state) v*)))
	 (if (fx=? i imin)
	     ;;Tail-call as last COMBINE application.
	     (doit i)
	   (loop (fxsub1 i) imin (doit i)))))))

  #| end of CASE-DEFINE* |# )


(case-define* vector-append
  ;;Defined  by  Vicare.  Return  a  newly  allocated  vector  whose items  form  the
  ;;concatenation of the given vectors.
  ;;
  (() '#())

  (({vec vector?})
   vec)

  (({vec1 vector?} {vec2 vector?})
   (let* ((len1		($vector-length vec1))
	  (len2		($vector-length vec2))
	  (dst.len	(+ len1 len2)))
     (preconditions
      (total-vector-length-is-a-fixnum dst.len))
     (receive-and-return (dst.vec)
	 ($make-clean-vector dst.len)
       ($vector-copy-source-range! vec1 0 len1 dst.vec 0)
       ($vector-copy-source-range! vec2 0 len2 dst.vec len1))))

  (({vec1 vector?} {vec2 vector?} {vec3 vector?})
   (let* ((len1		($vector-length vec1))
	  (len2		($vector-length vec2))
	  (len3		($vector-length vec3))
	  (dst.len	(+ len1 len2 len3)))
     (preconditions
      (total-vector-length-is-a-fixnum dst.len))
     (receive-and-return (dst.vec)
	 ($make-clean-vector dst.len)
       ($vector-copy-source-range! vec1 0 len1 dst.vec 0)
       ($vector-copy-source-range! vec2 0 len2 dst.vec len1)
       ($vector-copy-source-range! vec3 0 len3 dst.vec ($fx+ len1 len2)))))

  (({vec1 vector?} {vec2 vector?} {vec3 vector?} {vec4 vector?})
   (let* ((len1		($vector-length vec1))
	  (len2		($vector-length vec2))
	  (len3		($vector-length vec3))
	  (len4		($vector-length vec4))
	  (dst.len	(+ len1 len2 len3 len4)))
     (preconditions
      (total-vector-length-is-a-fixnum dst.len))
     (receive-and-return (dst.vec)
	 ($make-clean-vector dst.len)
       ($vector-copy-source-range! vec1 0 len1 dst.vec 0)
       ($vector-copy-source-range! vec2 0 len2 dst.vec len1)
       (let ((dst.start ($fx+ len1 len2)))
	 ($vector-copy-source-range! vec3 0 len3 dst.vec dst.start)
	 (let ((dst.start ($fx+ dst.start len3)))
	   ($vector-copy-source-range! vec4 0 len4 dst.vec dst.start))))))

  (v*
   (for-each (lambda (item)
	       (unless (vector? item)
		 (procedure-argument-violation __who__ "failed argument validation" item)))
     v*)
   (let ((dst.len (fold-left (lambda (accum-len V)
			       (+ accum-len ($vector-length V)))
		    0 v*)))
     (preconditions
      (total-vector-length-is-a-fixnum dst.len))
     (receive-and-return (dst.vec)
	 ($make-clean-vector dst.len)
       (fold-left (lambda (dst.start V)
		    (let ((V.len ($vector-length V)))
		      ($vector-copy-source-range! V 0 V.len dst.vec dst.start)
		      ($fx+ dst.start V.len)))
	 0 v*))))

  #| end of CASE-DEFINE* |# )


(define* (subvector {vec vector?} {start non-negative-fixnum?} {end non-negative-fixnum?})
  ;;Defined by Vicare.  VEC must be a vector, and START and END must be exact integer
  ;;objects satisfying:
  ;;
  ;;   0 <= START <= END <= (vector-length VEC)
  ;;
  ;;Return a newly allocated vector formed from the items of VEC beginning with index
  ;;START (inclusive) and ending with index END (exclusive).
  ;;
  (preconditions
   (valid-start-index-for-vector-slot vec start)
   (valid-end-index-for-vector-slot   vec end)
   (start-and-end-indexes-in-correct-order start end))
  ($subvector vec start end))

(define* (vector-copy {vec vector?})
  ;;Defined by Vicare.  Return a newly allocated copy of the given VEC.
  ;;
  ($subvector vec 0 ($vector-length vec)))

(case-define* vector-resize
  (({vec vector?} {new-len non-negative-fixnum?})
   ($vector-resize vec new-len #f))
  (({vec vector?} {new-len non-negative-fixnum?} fill)
   ($vector-resize vec new-len fill))
  #| end of CASE-DEFINE* |# )

(define ($vector-resize src.vec dst.len fill)
  ;;Build and return a new vector  object of length DST.LEN representing the resizing
  ;;of SRC.VEC.
  ;;
  ;;* If  DST.LEN is less  than the  length of SRC.VEC:  the new vector  represents a
  ;;subvector of SRC.VEC.
  ;;
  ;;* If DST.LEN is  greater than the length of SRC.VEC: the new  vector has the head
  ;;slots holding the same items of SRC.VEC and the tail slots initialised to FILL.
  ;;
  ;;Examples:
  ;;
  ;;   ($vector-resize '#(1 2 3 4) 2 #f)  => #(1 2)
  ;;   ($vector-resize '#(1 2 3 4) 6 #f)  => #(1 2 3 4 #f #f)
  ;;
  (receive-and-return (dst.vec)
      ($make-clean-vector dst.len)
    (let ((src.len ($vector-length src.vec)))
      ($vector-copy-source-range! src.vec 0 src.len dst.vec 0)
      (when ($fx< src.len dst.len)
	($vector-fill-range! dst.vec src.len dst.len fill)))))

(case-define* vector-reset!
  (({vec vector?})
   (do ((i 0 (fxadd1 i)))
       ((fx=? i (vector-length vec))
	(void))
     ($vector-set! vec i (void))))
  (({vec vector?} {start non-negative-fixnum?} {end non-negative-fixnum?})
   ;;Defined  by Vicare.   VEC must  be a  vector, and  START and  END must  be exact
   ;;integer objects satisfying:
   ;;
   ;;   0 <= START <= END < (vector-length VEC)
   ;;
   ;;Reset to void the selected range of slots beginning with index START (inclusive)
   ;;and ending with index END (exclusive).
   ;;
   (preconditions
    (valid-start-index-for-vector-slot vec start)
    (valid-end-index-for-vector-slot   vec end)
    (start-and-end-indexes-in-correct-order start end))
   (do ((i start (fxadd1 i)))
       ((fx=? i end)
	(void))
     ($vector-set! vec i (void)))))

(define* (vector-copy! {src.vec vector?} {src.start non-negative-fixnum?}
		       {dst.vec vector?} {dst.start non-negative-fixnum?}
		       {count non-negative-fixnum?})
  ;;Defined  by  Vicare.   Copy  COUNT  items  from  SRC.VEC  starting  at  SRC.START
  ;;(inclusive) to DST.VEC starting at DST.START.  Return unspecified values.
  ;;
  (preconditions
   (valid-start-index-for-vector-slot src.vec src.start)
   (valid-start-index-for-vector-slot dst.vec dst.start)
   (valid-start-index-and-slot-count-for-vector src.vec src.start count)
   (valid-start-index-and-slot-count-for-vector dst.vec dst.start count))
  (cond (($fxzero? count)
	 (void))
	((eq? src.vec dst.vec)
	 (cond (($fx< dst.start src.start)
		($vector-self-copy-forwards!  src.vec src.start dst.start count))
	       (($fx> dst.start src.start)
		($vector-self-copy-backwards! src.vec
					      ($fx+ src.start count)
					      ($fx+ dst.start count)
					      count))
	       (else (void))))
	(else
	 ($vector-copy-source-count! src.vec src.start dst.vec dst.start count))))


(define* (vector-empty? {vec vector?})
  ;;Defined by Vicare.  Return true if VEC is empty, otherwise return false.
  ;;
  ($vector-empty? vec))

;;FIXME This  should become  a true  primitive operation.  (Marco  Maggi; Tue  Oct 8,
;;2013)
(define ($vector-empty? vec)
  ($fxzero? ($vector-length vec)))

(define (non-empty-vector? obj)
  ;;Does not raise an exception if OBJ is not a vector object.
  ;;
  (and (vector? obj)
       (not ($vector-empty? obj))))


;;;; simplified unsafe operations

(define ($vector-map1 func src)
  ;;Like VECTOR-MAP, but for only  one vector argument: build and return
  ;;a new  vector having  the same size  of VEC and  items equal  to the
  ;;result of applying FUNC to the items of VEC.
  ;;
  (let loop ((i   0)
	     (dst ($make-clean-vector ($vector-length src))))
    (if ($fx= i ($vector-length src))
	src
      (begin
	($vector-set! dst i (func ($vector-ref src i)))
	(loop ($fxadd1 i) dst)))))

(define ($vector-for-each1 func vec)
  ;;Like VECTOR-FOR-EACH, but  for only one vector  argument: apply FUNC
  ;;to all the items of VEC and discard the return values.
  ;;
  (let loop ((i 0))
    (or ($fx= i ($vector-length vec))
	(begin
	  (func ($vector-ref vec i))
	  (loop ($fxadd1 i))))))

(define ($vector-for-all1 func vec)
  ;;Like VECTOR-FOR-ALL, but  for only one vector  argument: return true
  ;;if FUNC returns  true for all the items in  VEC.  If the application
  ;;of FUNC to the items of VEC returns true up to the penultimate item,
  ;;the last application is performed as tail call.
  ;;
  (or ($fxzero? ($vector-length vec))
      (let loop ((i     0)
		 (len-1 ($fxsub1 ($vector-length vec))))
	(if ($fx= i len-1)
	    ;;Tail call.
	    (func ($vector-ref vec i))
	  (and (func ($vector-ref vec i))
	       (loop ($fxadd1 i) len-1))))))

(define ($vector-exists1 func vec)
  ;;Like VECTOR-EXISTS, but  for only one vector  argument: return false
  ;;if FUNC returns false for all  the items in VEC.  If the application
  ;;of FUNC  to the  items of  VEC returns false  up to  the penultimate
  ;;item, the last application is performed as tail call.
  ;;
  (if ($fxzero? ($vector-length vec))
      #f
    (let loop ((i     0)
	       (len-1 ($fxsub1 ($vector-length vec))))
      (if ($fx= i len-1)
	  ;;Tail call.
	  (func ($vector-ref vec i))
	(or (func ($vector-ref vec i))
	    (loop ($fxadd1 i) len-1))))))


;;;; done

#| end of library |# )


(library (vicare system vectors)
  (export $vector-ref $vector-length)
  (import (vicare))
  (define $vector-ref vector-ref)
  (define $vector-length vector-length))

;;; end of file
