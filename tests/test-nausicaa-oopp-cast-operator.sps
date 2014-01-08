;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for Nausicaa's cast operator
;;;Date: Sun Dec 29, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (nausicaa)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa: cast operator\n")


(parametrise ((check-test-name	'labels))

  (check ((<top>) 123)				=> 123)
  (check ((<procedure>) 123)			=> 123)

  (check ((<fixnum>) 123)			=> 123)
  (check (((<fixnum>) 123) add1)		=> 124)
  (check (((<fixnum>) 123) <= 200)		=> #t)
  (check (((<fixnum>) 123) <= 100)		=> #f)

  (check (((<vector>) '#(1 2 3)) [1])		=> 2)

  (check
      (guard (E ((syntax-violation? E)
		 (condition-message E))
		(else E))
	(eval '(((<fixnum>) 123) map)
	      (environment '(nausicaa))))
    => "invalid tag member")

  #t)


(parametrise ((check-test-name	'classes))

  (define-class <my-fixnum>
    (fields N)
    (method (add1 (O <my-fixnum>))
      (<my-fixnum> ((add1 (O N))))))

  (check ((<my-fixnum>) (<my-fixnum> (123)))		=> (<my-fixnum> (123)))
  (check (((<my-fixnum>) (<my-fixnum> (123))) add1)	=> (<my-fixnum> (124)))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
