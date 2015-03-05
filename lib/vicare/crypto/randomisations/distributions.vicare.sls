;;;
;;;Part of: Vicare Scheme
;;;Contents: random numbers from distributions
;;;Date: Sat Jul  4, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2002 Sebastian Egner
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
(library (vicare crypto randomisations distributions)
  (export
    random-exponentials-maker
    random-normals-maker)
  (import (rnrs)
    (vicare crypto randomisations))



(define (random-exponentials-maker source)
  ;;Refer to Knuth's  ``The Art of Computer Programming'',  Vol. II, 2nd
  ;;ed., Section 3.4.1.D.
  (let ((real-maker (random-source-reals-maker source)))
    (lambda (mu)
      (- (* mu (log (real-maker)))))))

(define (random-normals-maker source)
  ;;For  the   algorithm  refer  to   Knuth's  ``The  Art   of  Computer
  ;;Programming'', Vol. II, 2nd ed., Algorithm P of Section 3.4.1.C.
  (let ((next #f)
	(real-maker (random-source-reals-maker source)))
    (lambda (mu sigma)
      (if next
          (let ((result next))
            (set! next #f)
            (+ mu (* sigma result)))
        (let loop ()
          (let* ((v1 (- (* 2 (real-maker)) 1))
                 (v2 (- (* 2 (real-maker)) 1))
                 (s (+ (* v1 v1) (* v2 v2))))
            (if (>= s 1)
                (loop)
              (let ((scale (sqrt (/ (* -2 (log s)) s))))
                (set! next (* scale v2))
                (+ mu (* sigma scale v1))))))))))



;;;; done

)

;;; end of file
