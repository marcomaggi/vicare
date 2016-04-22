;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the enumeration type annotation
;;;Date: Fri Apr 22, 2016
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
(program (test-types-enumeration-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: enumeration type annotations\n")


;;;; helpers

(define (%type-signature->sexp sig)
  (syntax->datum (expander::type-signature.syntax-object sig)))


(parametrise ((check-test-name	'predicate))

  (define-type <greetings>
    (enumeration hello ciao salut ohayo))

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? 'ciao (enumeration hello ciao salut ohayo)))
  (check-for-false	(is-a? 'hell (enumeration hello ciao salut ohayo)))

  (check-for-true	(is-a? 'hello <greetings>))
  (check-for-true	(is-a? 'ciao  <greetings>))
  (check-for-true	(is-a? 'salut <greetings>))
  (check-for-true	(is-a? 'ohayo <greetings>))
  (check-for-false	(is-a? 'hell  <greetings>))

  (void))


(parametrise ((check-test-name	'super-and-sub))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?super ?sub => ?expected)
       (check
	   (type-annotation-super-and-sub? ?super ?sub)
	 => ?expected))
      ))

;;; --------------------------------------------------------------------

  (doit (enumeration hello ciao salut ohayo)
	(enumeration hello ohayo)
	=> #t)

  (doit (enumeration hello ohayo)
	(enumeration hello ciao salut ohayo)
	=> #f)

  (void))


(parametrise ((check-test-name	'union))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?super ?sub => ?expected)
       (check
	   (type-signature-union (?super) (?sub))
	 (=> syntax=?)
	 (syntax (?expected))))
      ))

;;; --------------------------------------------------------------------

  (doit (enumeration hello ciao salut ohayo)
	(enumeration hello ohayo)
	=> (enumeration hello ciao salut ohayo))

  (doit (enumeration hello ohayo)
	(enumeration salut ciao)
	=> (enumeration hello ohayo salut ciao))

  (doit (enumeration hello ciao)
	(enumeration salut ciao)
	=> (enumeration hello ciao salut))

  (void))


(parametrise ((check-test-name	'validator))

  (define-type greeting
    (enumeration hello ciao salut ohayo))

;;; --------------------------------------------------------------------

  (check (greeting ciao)	=> 'ciao)
  (check (greeting salut)	=> 'salut)
  (check (greeting ohayo)	=> 'ohayo)
  (check (greeting hello)	=> 'hello)

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
