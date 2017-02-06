;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test for issue 94
;;;Date: Feb  5, 2017
;;;
;;;Abstract
;;;
;;;	Test for a compiler error when using unsafe primitives.
;;;
;;;Copyright (C) 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (vicare)
  (vicare checks))

(check-set-mode! 'report)
(check-display "*** testing issue 94, compiler error when using fxxor\n")



(check
    (internal-body
      (define f
	(eval '(lambda (x)
		 (let* ((t0 x)
			(t1 18)
			(tmp (fx- t0 t1))
			(fl-OF (lambda ()
				 (fxand (fxxor t0 t1)
					(fxxor t0 tmp)))))
		   (fl-OF)))
	      (environment '(rnrs))))
      (f 0))
  => 2)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
