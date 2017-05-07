;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test for issue 96
;;;Date: May  7, 2017
;;;
;;;Abstract
;;;
;;;	Test for EXPT with typed language enabled.
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


#!vicare
(program (test-issue-096-error-in-expt-tables)
  (options typed-language)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report)
(check-display "*** testing issue 96, error in expt tables\n")


;;;; tests

(check (expt 1.1 2.0)		=> (* 1.1 1.1))

(check (expt 1.1 2)		=> (* 1.1 1.1))
(check (expt 2/3 2)		=> (* 2/3 2/3))
(check (expt (least-positive-bignum) 2)		=> (* (least-positive-bignum) (least-positive-bignum)))
(check (expt 2+3.0i 2)		=> (* 2+3.0i 2+3.0i))
(check (expt 2.0+3.0i 2)	=> (* 2.0+3.0i 2.0+3.0i))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
