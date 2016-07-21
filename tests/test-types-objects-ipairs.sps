;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <ipair> type
;;;Date: Fri Mar 25, 2016
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
(program (test-types-ipair-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: <ipair> objects\n")


(parametrise ((check-test-name	'predicate))

  (check-for-true	(is-a? (ipair 1 2) <ipair>))
  (check-for-false	(is-a? 123 <ipair>))

  (check-for-false	(is-a? '() <ipair>))

  (check-for-true	(let (({O <ipair>} (ipair 1 2)))
			  (is-a? O <ipair>)))

  (check-for-true	(let (({O <top>} (ipair 1 2)))
			  (is-a? O <ipair>)))

  (check-for-false	(let (({O <top>} "ciao"))
			  (is-a? O <ipair>)))

  (void))


(parametrise ((check-test-name	'constructor))

  (check
      (new <ipair> 1 2)
    => (ipair 1 2))

  (check
      (.syntax-object (type-of (new <ipair> (read) (read))))
    (=> expander::syntax=?)
    #'(<ipair>))

  (void))


(parametrise ((check-test-name	'methods))

  (check
      (.car (new <ipair> 1 2))
    => 1)

  (check
      (.cdr (new <ipair> 1 2))
    => 2)

  (void))


(parametrise ((check-test-name	'late-binding))

  (check
      (method-call-late-binding 'car #f (new <ipair> 1 2))
    => 1)

  (check
      (method-call-late-binding 'cdr #f (new <ipair> 1 2))
    => 2)

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
