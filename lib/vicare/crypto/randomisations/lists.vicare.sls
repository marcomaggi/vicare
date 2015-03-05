;;;
;;;Part of: Vicare Scheme
;;;Contents: randomness related list functions
;;;Date: Thu Jul  2, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (vicare crypto randomisations lists)
  (export
    random-list-unfold-numbers
    random-list-shuffle
    random-list-sample
    random-list-sample-population)
  (import (rnrs)
    (vicare crypto randomisations))



(define (random-list-unfold-numbers number-maker number-of-numbers)
  (do ((i 0 (+ 1 i))
       (ell '() (cons (number-maker) ell)))
      ((= i number-of-numbers)
       ell)))

(define (random-list-shuffle ell source)
  (let* ((len  (length ell))
	 (perm ((random-permutations-maker source) len)))
    (let loop ((i 0)
	       (result '()))
      (if (= i len)
	  result
	(loop (+ 1 i) (cons (list-ref ell (vector-ref perm i)) result))))))

(define (random-list-sample ell source)
  (let ((index-maker	(random-source-integers-maker source))
	(len		(length ell)))
    (lambda ()
      (list-ref ell (index-maker len)))))

(define (random-list-sample-population ell len source)
  (let ((sampler (random-list-sample ell source)))
    (lambda ()
      (do ((i 0 (+ 1 i))
	   (individual '() (cons (sampler) individual)))
	  ((= i len)
	   individual)))))


;;;; done

)

;;; end of file
