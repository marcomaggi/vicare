;;;
;;;Part of: Vicare Scheme
;;;Contents: sorting dynamic arrays
;;;Date: Fri Aug 14, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare containers dynamic-arrays sort)
  (export
    dynamic-array-sort		$dynamic-array-sort
    dynamic-array-sort!		$dynamic-array-sort!)
  (import (vicare)
    (vicare containers dynamic-arrays)
    (vicare system $fx))


(define* (dynamic-array-sort {item< procedure?} {arry dynamic-array?})
  ($dynamic-array-sort item< arry))

(define* (dynamic-array-sort! {item< procedure?} {arry dynamic-array?})
  ($dynamic-array-sort! item< arry))

(module ($dynamic-array-sort $dynamic-array-sort!)

  (define ($dynamic-array-sort item< src)
    (define len
      ($dynamic-array-length src))
    (receive-and-return (dst)
	($dynamic-array-copy! (make-dynamic-array len) src)
      (%do-sort! item< dst ($dynamic-array-copy! (make-dynamic-array len) dst) 0 ($fxsub1 len))))

  (define ($dynamic-array-sort! item< src)
    (define len
      ($dynamic-array-length src))
    (%do-sort! item< src ($dynamic-array-copy! (make-dynamic-array len) src) 0 ($fxsub1 len))
    src)

  (define (%do-sort! item< src skr i k)
    ;; sort src(i .. k) inclusive in place
    (when ($fx< i k)
      (let ((j ($fxsra ($fx+ i k) 1)))
	(%do-sort!  item< skr src i j)
	(%do-sort!  item< skr src ($fxadd1 j) k)
	(%do-merge! item< src skr i k i j ($fxadd1 j) k))))

  (define (%copy-subrange! src dst src.idx dst.idx dst.last)
    ($dynamic-array-set! dst dst.idx ($dynamic-array-ref src src.idx))
    (let ((dst.idx ($fxadd1 dst.idx)))
      (when ($fx<= dst.idx dst.last)
	(%copy-subrange! src dst ($fxadd1 src.idx) dst.idx dst.last))))

  (define (%do-merge-a! item< src skr ri rj ai aj bi bj b0)
    (let ((a0 ($dynamic-array-ref skr ai))
	  (ai ($fxadd1 ai)))
      (if (item< b0 a0)
	  (begin
	    ($dynamic-array-set! src ri b0)
	    (let ((ri ($fxadd1 ri)))
	      (if ($fx<= bi bj)
		  (%do-merge-b! item< src skr ri rj ai aj bi bj a0)
		(begin
		  ($dynamic-array-set! src ri a0)
		  (let ((ri ($fxadd1 ri)))
		    (when ($fx<= ri rj)
		      (%copy-subrange! skr src ai ri rj)))))))
	(begin
	  ($dynamic-array-set! src ri a0)
	  (let ((ri ($fxadd1 ri)))
	    (if ($fx<= ai aj)
		(%do-merge-a! item< src skr ri rj ai aj bi bj b0)
	      (begin
		($dynamic-array-set! src ri b0)
		(let ((ri ($fxadd1 ri)))
		  (when ($fx<= ri rj)
		    (%copy-subrange! skr src bi ri rj))))))))))

  (define (%do-merge-b! item< src skr ri rj ai aj bi bj a0)
    (let ((b0 ($dynamic-array-ref skr bi))
	  (bi ($fxadd1 bi)))
      (if (item< b0 a0)
	  (begin
	    ($dynamic-array-set! src ri b0)
	    (let ((ri ($fxadd1 ri)))
	      (if ($fx<= bi bj)
		  (%do-merge-b! item< src skr ri rj ai aj bi bj a0)
		(begin
		  ($dynamic-array-set! src ri a0)
		  (let ((ri ($fxadd1 ri)))
		    (when ($fx<= ri rj)
		      (%copy-subrange! skr src ai ri rj)))))))
	(begin
	  ($dynamic-array-set! src ri a0)
	  (let ((ri ($fxadd1 ri)))
	    (if ($fx<= ai aj)
		(%do-merge-a! item< src skr ri rj ai aj bi bj b0)
	      (begin
		($dynamic-array-set! src ri b0)
		(let ((ri ($fxadd1 ri)))
		  (when ($fx<= ri rj)
		    (%copy-subrange! skr src bi ri rj))))))))))

  (define (%do-merge! item< src skr ri rj ai aj bi bj)
    (let ((a0 ($dynamic-array-ref skr ai))
	  (b0 ($dynamic-array-ref skr bi))
	  (ai ($fxadd1 ai))
	  (bi ($fxadd1 bi)))
      (if (item< b0 a0)
	  (begin
	    ($dynamic-array-set! src ri b0)
	    (let ((ri ($fxadd1 ri)))
	      (if ($fx<= bi bj)
		  (%do-merge-b! item< src skr ri rj ai aj bi bj a0)
		(begin
		  ($dynamic-array-set! src ri a0)
		  (let ((ri ($fxadd1 ri)))
		    (when ($fx<= ri rj)
		      (%copy-subrange! skr src ai ri rj)))))))
	(begin
	  ($dynamic-array-set! src ri a0)
	  (let ((ri ($fxadd1 ri)))
	    (if ($fx<= ai aj)
		(%do-merge-a! item< src skr ri rj ai aj bi bj b0)
	      (begin
		($dynamic-array-set! src ri b0)
		(let ((ri ($fxadd1 ri)))
		  (when ($fx<= ri rj)
		    (%copy-subrange! skr src bi ri rj))))))))))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
