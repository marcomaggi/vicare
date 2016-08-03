;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <boolean> type
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
(program (test-types-boolean-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
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
(check-display "*** testing Vicare typed language: <boolean> objects\n")


(parametrise ((check-test-name	'predicate))

;;; type predicate

  (check-for-true
   (is-a? #t <boolean>))

  (check-for-true
   (is-a? #f <boolean>))

  (check-for-true
   (let ((O '#t))
     (is-a? O <boolean>)))

  (check-for-true
   (let ((O '#f))
     (is-a? O <boolean>)))

  (check-for-true
   (let (({O <boolean>} '#t))
     (is-a? O <boolean>)))

  (check-for-false
   (is-a? 123 <boolean>))

  (void))


(parametrise ((check-test-name	'constructor))

  (check-for-true	(new <boolean> 123))
  (check-for-true	(new <boolean> #t))
  (check-for-false	(new <boolean> #f))

  (check
      (expander::type-signature.syntax-object (type-of (new <boolean> (read))))
    (=> expander::syntax=?)
    (list #'<boolean>))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
