;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for typed language syntaxes
;;;Date: Sat Oct 10, 2015
;;;
;;;Abstract
;;;
;;;
;;;
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
(program (test-typed-language-syntaxes)
  (options typed-language)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: type-related syntaxes\n")


;;;; helpers




(parametrise ((check-test-name	'type-super-and-sub))

  (check-for-true	(type-super-and-sub? <number> <fixnum>))
  (check-for-false	(type-super-and-sub? <number> <string>))

  (check
      (expansion-of (type-super-and-sub? <number> <fixnum>))
    => '(quote #t))

  (check
      (expansion-of (type-super-and-sub? <number> <string>))
    => '(quote #f))

  (check-for-true	(type-super-and-sub? <top> <number>))
  (check-for-false	(type-super-and-sub? <number> <top>))

  (internal-body
    (define-record-type alpha)

    (define-record-type beta
      (parent alpha))

    (define-record-type gamma
      (parent beta))

    (check-for-true	(type-super-and-sub? alpha beta))
    (check-for-false	(type-super-and-sub? beta alpha))

    (check-for-true	(type-super-and-sub? alpha gamma))
    (check-for-false	(type-super-and-sub? gamma alpha))

    (check-for-true	(type-super-and-sub? beta gamma))
    (check-for-false	(type-super-and-sub? gamma beta))

    (check-for-true	(type-super-and-sub? <top> alpha))
    (check-for-false	(type-super-and-sub? alpha <top>))

    (check-for-true	(type-super-and-sub? <top> beta))
    (check-for-false	(type-super-and-sub? beta <top>))

    (check-for-true	(type-super-and-sub? <top> gamma))
    (check-for-false	(type-super-and-sub? gamma <top>))

    (void))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
