;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <keyword> type
;;;Date: Mon Oct 19, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-types-objects-keywords)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for <keyword> objects\n")


(parametrise ((check-test-name	'predicate))

  (check-for-true	(is-a? #:ciao <keyword>))
  (check-for-false	(is-a? 123 <keyword>))

  (check-for-true	(let (({O <keyword>} #:ciao))
			  (is-a? O <keyword>)))

  (check-for-true	(let (({O <top>} #:ciao))
			  (is-a? O <keyword>)))

  (check-for-false	(let (({O <top>} "ciao"))
			  (is-a? O <keyword>)))

  #t)


(parametrise ((check-test-name		'constructor))

  (check
      (new <keyword> 'ciao)
    => #:ciao)

  (check
      (expander::type-signature.syntax-object (type-of (new <keyword> (read))))
    (=> expander::syntax=?)
    (list #'<keyword>))

  (void))


(parametrise ((check-test-name		'methods))

  (check
      (let (({O <keyword>} #:ciao))
	(.string O))
    => "#:ciao")

;;; --------------------------------------------------------------------

  (check
      (let (({O <keyword>} #:ciao))
	(fixnum? (.hash O)))
    => #t)

  (void))


(parametrise ((check-test-name		'late-binding))

  (check
      (let (({O <keyword>} #:ciao))
	(method-call-late-binding 'string #f O))
    => "#:ciao")

;;; --------------------------------------------------------------------

  (check
      (let (({O <keyword>} #:ciao))
	(fixnum? (method-call-late-binding 'hash #f O)))
    => #t)

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
