;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified in 2015 by Marco Maggi <marco-maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (ikarus.sort)
  (export
    list-sort
    vector-sort vector-sort!)
  (import (except (vicare)
		  list-sort
		  vector-sort vector-sort!)
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $vectors))


(module UNSAFE
  (fx<? fx<=? fx>? fx>=? fx=?
	fx+ fx- fxarithmetic-shift-right
	vector-ref vector-set! car cdr)
  (import
      (rename (vicare system $pairs)
	      ($car    car)
	      ($cdr    cdr))
    (rename (vicare system $vectors)
	    ($vector-ref    vector-ref)
	    ($vector-set!   vector-set!))
    (rename (vicare system $fx)
	    ($fxsra    fxarithmetic-shift-right)
	    ($fx+      fx+)
	    ($fx-      fx-)
	    ($fx<      fx<?)
	    ($fx>      fx>?)
	    ($fx>=     fx>=?)
	    ($fx<=     fx<=?)
	    ($fx=      fx=?))))


(define* (list-sort {proc procedure?} ls)
  (import UNSAFE)

  (define (race hare turtle ls accum-length)
    ;;Perform the  "turtle and  hare" race to  compute the length  of the  list HARE,
    ;;validating it as proper list.  Raise an error if HARE is circular or improper.
    ;;
    ;;LS is the original full list: it is used only for error reporting.
    ;;
    ;;At every function invocation: the hare does 2 steps forwards, the turtle does 1
    ;;step forwards.
    ;;
    (fluid-let-syntax ((__who__ (identifier-syntax 'list-sort)))
      (cond ((pair? hare)
	     (let ((hare (cdr hare)))
	       (cond ((pair? hare)
		      (if (eq? hare turtle)
			  (error __who__ "circular list" ls)
			(race (cdr hare) (cdr turtle) ls ($fx+ accum-length 2))))
		     ((null? hare)
		      ($fxadd1 accum-length))
		     (else
		      (error __who__ "not a proper list" ls)))))
	    ((null? hare)
	     accum-length)
	    (else
	     (error __who__ "not a proper list" ls)))))

  (let ((len (race ls ls ls 0)))
    (if (fixnum? len)
	(if ($fx< len 2)
	    ls
	  (let loop ((v (make-vector len))
		     (ls ls)
		     (i 0))
	    (if (pair? ls)
		(begin
		  ($vector-set! v i ($car ls))
		  (loop v ($cdr ls) ($fxadd1 i)))
	      (begin
		(vector-sort! proc v)
		(vector->list v)))))
      (procedure-argument-violation __who__ "list too long to be sorted" ls))))


(module (vector-sort vector-sort!)
  (import UNSAFE)

  (define* (vector-sort {proc procedure?} {src vector?})
    (receive-and-return (dst)
	(vector-copy src)
      (%do-sort! proc dst (vector-copy dst) 0 ($fxsub1 ($vector-length src)))))

  (define* (vector-sort! {proc procedure?} {src vector?})
    (%do-sort! proc src (vector-copy src) 0 ($fxsub1 ($vector-length src)))
    src)

  (define (%do-sort! proc src skr i k)
    ;; sort src(i .. k) inclusive in place
    (when (fx<? i k)
      (let ((j (fxarithmetic-shift-right (fx+ i k) 1)))
	(%do-sort!  proc skr src i j)
	(%do-sort!  proc skr src ($fxadd1 j) k)
	(%do-merge! proc src skr i k i j ($fxadd1 j) k))))

  (define (%copy-subrange! src dst src.idx dst.idx dst.last)
    (vector-set! dst dst.idx (vector-ref src src.idx))
    (let ((dst.idx ($fxadd1 dst.idx)))
      (when (fx<=? dst.idx dst.last)
	(%copy-subrange! src dst ($fxadd1 src.idx) dst.idx dst.last))))

  (define (%do-merge-a! proc src skr ri rj ai aj bi bj b0)
    (let ((a0 (vector-ref skr ai))
	  (ai ($fxadd1 ai)))
      (if (proc b0 a0)
	  (begin
	    (vector-set! src ri b0)
	    (let ((ri ($fxadd1 ri)))
	      (if (fx<=? bi bj)
		  (%do-merge-b! proc src skr ri rj ai aj bi bj a0)
		(begin
		  (vector-set! src ri a0)
		  (let ((ri ($fxadd1 ri)))
		    (when (fx<=? ri rj)
		      (%copy-subrange! skr src ai ri rj)))))))
	(begin
	  (vector-set! src ri a0)
	  (let ((ri ($fxadd1 ri)))
	    (if (fx<=? ai aj)
		(%do-merge-a! proc src skr ri rj ai aj bi bj b0)
	      (begin
		(vector-set! src ri b0)
		(let ((ri ($fxadd1 ri)))
		  (when (fx<=? ri rj)
		    (%copy-subrange! skr src bi ri rj))))))))))

  (define (%do-merge-b! proc src skr ri rj ai aj bi bj a0)
    (let ((b0 (vector-ref skr bi))
	  (bi ($fxadd1 bi)))
      (if (proc b0 a0)
	  (begin
	    (vector-set! src ri b0)
	    (let ((ri ($fxadd1 ri)))
	      (if (fx<=? bi bj)
		  (%do-merge-b! proc src skr ri rj ai aj bi bj a0)
		(begin
		  (vector-set! src ri a0)
		  (let ((ri ($fxadd1 ri)))
		    (when (fx<=? ri rj)
		      (%copy-subrange! skr src ai ri rj)))))))
	(begin
	  (vector-set! src ri a0)
	  (let ((ri ($fxadd1 ri)))
	    (if (fx<=? ai aj)
		(%do-merge-a! proc src skr ri rj ai aj bi bj b0)
	      (begin
		(vector-set! src ri b0)
		(let ((ri ($fxadd1 ri)))
		  (when (fx<=? ri rj)
		    (%copy-subrange! skr src bi ri rj))))))))))

  (define (%do-merge! proc src skr ri rj ai aj bi bj)
    (let ((a0 (vector-ref skr ai))
	  (b0 (vector-ref skr bi))
	  (ai ($fxadd1 ai))
	  (bi ($fxadd1 bi)))
      (if (proc b0 a0)
	  (begin
	    (vector-set! src ri b0)
	    (let ((ri ($fxadd1 ri)))
	      (if (fx<=? bi bj)
		  (%do-merge-b! proc src skr ri rj ai aj bi bj a0)
		(begin
		  (vector-set! src ri a0)
		  (let ((ri ($fxadd1 ri)))
		    (when (fx<=? ri rj)
		      (%copy-subrange! skr src ai ri rj)))))))
	(begin
	  (vector-set! src ri a0)
	  (let ((ri ($fxadd1 ri)))
	    (if (fx<=? ai aj)
		(%do-merge-a! proc src skr ri rj ai aj bi bj b0)
	      (begin
		(vector-set! src ri b0)
		(let ((ri ($fxadd1 ri)))
		  (when (fx<=? ri rj)
		    (%copy-subrange! skr src bi ri rj))))))))))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
