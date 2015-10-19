;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <char> type
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
(program (test-types-char-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) xp.)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for <char> objects\n")


(parametrise ((check-test-name		'predicate))

  (check-for-true	(is-a? #\A <char>))
  (check-for-false	(is-a? 123 <char>))

  (check-for-true	(let (({O <char>} #\A))
			  (is-a? O <char>)))

  (check-for-true	(let (({O <top>} #\A))
			  (is-a? O <char>)))

  (check-for-false	(let (({O <top>} "ciao"))
			  (is-a? O <char>)))

  (void))


(parametrise ((check-test-name		'constructor))

  (check
      (new <char> 65)
    => #\A)

  (check
      (xp.type-signature-tags (type-of (new <char> (read))))
    (=> syntax=?)
    (list #'<char>))

  (void))


(parametrise ((check-test-name		'methods))

  (check
      (let (({O <char>} #\A))
	(.string O))
    => "A")

;;; --------------------------------------------------------------------

  (check
      (let (({O <char>} #\A))
	(fixnum? (.hash O)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let (({O <char>} #\A))
	(.integer O))
    => 65)

  (check
      (let (({O <char>} #\A))
	(.fixnum O))
    => 65)

  (void))


(parametrise ((check-test-name		'late-binding))

  (check
      (let (({O <char>} #\A))
	(method-call-late-binding 'string O))
    => "A")

;;; --------------------------------------------------------------------

  (check
      (let (({O <char>} #\A))
	(fixnum? (method-call-late-binding 'hash O)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let (({O <char>} #\A))
	(method-call-late-binding 'integer O))
    => 65)

  (check
      (let (({O <char>} #\A))
	(method-call-late-binding 'fixnum O))
    => 65)

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
