;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <port> types
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
(program (test-types-port-objects)
  (options typed-language)
  (import (vicare)
    (only (vicare expander)
	  type-annotation=?
	  type-annotation-super-and-sub?
	  type-annotation-common-ancestor
	  type-annotation-ancestors
	  type-annotation-syntax
	  type-annotation-matching
	  type-signature-super-and-sub?
	  type-signature-common-ancestor
	  type-signature-matching
	  type-signature-union)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: tests for <port> types\n")


(parametrise ((check-test-name	'ports))

;;; type predicates

  (check-for-true	(is-a? (current-input-port)	<port>))
  (check-for-true	(is-a? (current-output-port)	<port>))
  (check-for-true	(is-a? (current-error-port)	<port>))

  (check-for-true
   (let (({P <textual-output-port>} (current-error-port)))
     (is-a? P <port>)))

  (check-for-true
   (let (({P <textual-output-port>} (current-error-port)))
     (is-a? P <output-port>)))

  (check-for-true
   (let (({P <textual-output-port>} (current-error-port)))
     (is-a? P <textual-output-port>)))

  (check-for-false
   (let (({P <textual-output-port>} (current-error-port)))
     (is-a? P <input-port>)))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
