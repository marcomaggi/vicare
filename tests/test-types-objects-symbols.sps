;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <symbol> type
;;;Date: Mon Oct 19, 2015
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
(program (test-types-symbol-objects)
  (options typed-language)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for <symbol> objects\n")


(parametrise ((check-test-name		'predicate))

  (check-for-true	(is-a? 'ciao <symbol>))
  (check-for-true	(is-a? (gensym) <symbol>))
  (check-for-false	(is-a? 123 <symbol>))

  (check-for-true	(let (({O <symbol>} 'ciao))
			  (is-a? O <symbol>)))

  (check-for-true	(let (({O <top>} 'ciao))
			  (is-a? O <symbol>)))

  (check-for-false	(let (({O <top>} "ciao"))
			  (is-a? O <symbol>)))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
