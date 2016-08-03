;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for union object types under typed language
;;;Date: test-types-unions.sps
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
(program (test-vicare-unions-typed)
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
(check-display "*** testing Vicare libraries: tests for union object-types under typed language\n")


(parametrise ((check-test-name	'type-of))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expression ?expected-tags)
       (check
	   ;;The  return value  of  a  TYPE-OF use  expansion  and  evaluation is  an
	   ;;instance of "<type-signature>".
	   (.syntax-object (type-of ?expression))
	 (=> expander::syntax=?)
	 ;;When the expression is a CONDITION application: the expected tags value is
	 ;;a list with a single item.
	 ?expected-tags))
      ))

;;; --------------------------------------------------------------------

  (define ({fun-1 (or <flonum> <string>)})
    1.2)

  (define-type <fixnum/flonum>
    (or <fixnum> <flonum>))

  (define ({fun-2 <fixnum/flonum>})
    1.2)

;;; --------------------------------------------------------------------

  (doit (fun-1)
	#'((or <flonum> <string>)))

  (doit (fun-2)
	#'((or <fixnum> <flonum>)))

  (void))


(parametrise ((check-test-name	'type-tags))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?type-annotation ?expected-tags)
       ;;Here we test only type signature describing a single value.
       (check
	   (type-annotation-syntax ?type-annotation)
	 (=> expander::syntax=?)
	 #'?expected-tags))
      ))

;;; --------------------------------------------------------------------

  (doit (or <fixnum> <string>)
	(or <fixnum> <string>))

  (doit (or <fixnum> (or <string>))
	(or <fixnum> <string>))

  (doit (or <fixnum> (or <string> <symbol>))
	(or <fixnum> <string> <symbol>))

  (doit (or <fixnum> (or (or <string>) <symbol>))
	(or <fixnum> <string> <symbol>))

  (doit (or <void> <top>)
	<void>)

  (doit (or <top> <void>)
	<void>)

  (doit (or <top> <fixnum> <void>)
	<void>)

  (void))


(parametrise ((check-test-name	'is-a))

  (define-type <stuff>
    (or <fixnum> <string> <symbol>))

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? 1	(or <fixnum> <flonum>)))
  (check-for-true	(is-a? 1.2	(or <fixnum> <flonum>)))
  (check-for-false	(is-a? "ciao"	(or <fixnum> <flonum>)))

  (check-for-true	(is-a? 1	<stuff>))
  (check-for-true	(is-a? "ciao"	<stuff>))
  (check-for-true	(is-a? 'ciao	<stuff>))
  (check-for-false	(is-a? 1.2	<stuff>))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
