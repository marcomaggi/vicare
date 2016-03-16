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
	   (.tags (type-of ?expression))
	 (=> syntax=?)
	 ;;When the expression is a CONDITION application: the expected tags value is
	 ;;a list with a single item.
	 ?expected-tags))
      ))

;;; --------------------------------------------------------------------

  (define ({fun-1 (union <flonum> <string>)})
    1.2)

  (define-type <fixnum/flonum>
    (union <fixnum> <flonum>))

  (define ({fun-2 <fixnum/flonum>})
    1.2)

;;; --------------------------------------------------------------------

  (doit (fun-1)
	#'((union <flonum> <string>)))

  (doit (fun-2)
	#'((union <fixnum> <flonum>)))

  (void))


(parametrise ((check-test-name	'type-tags))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?type-annotation ?expected-tags)
       ;;Here we test only type signature describing a single value.
       (check
	   (.tags (new expander::<type-signature> #'(?type-annotation)))
	 (=> syntax=?)
	 #'(?expected-tags)))
      ))

;;; --------------------------------------------------------------------

  (doit (union <fixnum> <string>)
	(union <fixnum> <string>))

  (doit (union <fixnum> (union <string>))
	(union <fixnum> <string>))

  (doit (union <fixnum> (union <string> <symbol>))
	(union <fixnum> <string> <symbol>))

  (doit (union <fixnum> (union (union <string>) <symbol>))
	(union <fixnum> <string> <symbol>))

  (void))


(parametrise ((check-test-name	'is-a))

  (define-type <stuff>
    (union <fixnum> <string> <symbol>))

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? 1	(union <fixnum> <flonum>)))
  (check-for-true	(is-a? 1.2	(union <fixnum> <flonum>)))
  (check-for-false	(is-a? "ciao"	(union <fixnum> <flonum>)))

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
