;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for instantiable bodies
;;;Date: Wed Jun 29, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-language-extensions-instantiable-bodies)
  (import (vicare)
    (vicare language-extensions instantiable-bodies)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: instantiable bodies\n")


(parametrise ((check-test-name	'base))

  (check
      (internal-body
	(define-instantiable-body definer
	  (define (fun a)
	    a))

        (definer ((fun doit)))

	(doit 123))
    => 123)

  (check
      (internal-body
	(define-instantiable-body definer
	  (define (fun)
	    const))

        (definer ((fun   doit)
		  (const 123)))

	(doit))
    => 123)

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
