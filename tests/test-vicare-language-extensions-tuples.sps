;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for tuples
;;;Date: Thu Aug 25, 2016
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
(program (test-vicare-tuples)
  (options typed-language)
  (import (vicare)
    (vicare language-extensions tuples)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: tuples\n")


(parametrise ((check-test-name	'list-tuples))

  ;;Untyped fields.
  ;;
  (check
      (internal-body
	(define-list-tuple-type <stuff>
	  (fields a b c))
	(define T
	  (new <stuff> 1 2 3))
	(values (.a T)
		(.b T)
		(.c T)))
    => 1 2 3)

  ;;Typed fields.
  ;;
  (check
      (internal-body
	(define-list-tuple-type <stuff>
	  (fields {a <fixnum>}
		  {b <flonum>}
		  {c <string>}))
	(let ((T (new <stuff> 1 2.3 "ciao")))
	  (values T
		  (.a T)
		  (.b T)
		  (.c T)
		  (.length (.c T)))))
    => '(1 2.3 "ciao") 1 2.3 "ciao" 4)

  ;;Predicate.
  ;;
  (check
      (internal-body
	(define-list-tuple-type <stuff>
	  (fields {a <fixnum>}
		  {b <flonum>}
		  {c <string>}))
	(is-a? '(4 5.6 "hello") <stuff>))
    => #t)

  #t)


(parametrise ((check-test-name	'vector-tuples))

  ;;Untyped fields.
  ;;
  (check
      (internal-body
	(define-vector-tuple-type <stuff>
	  (fields a b c))
	(let ((T (new <stuff> 1 2 3)))
	  (values (.a T)
		  (.b T)
		  (.c T))))
    => 1 2 3)

  ;;Typed fields.
  ;;
  (check
      (internal-body
	(define-vector-tuple-type <stuff>
	  (fields {a <fixnum>}
		  {b <flonum>}
		  {c <string>}))
	(let ((T (new <stuff> 1 2.3 "ciao")))
	  (values T
		  (.a T)
		  (.b T)
		  (.c T)
		  (.length (.c T)))))
    => '#(1 2.3 "ciao") 1 2.3 "ciao" 4)

  ;;Predicate.
  ;;
  (check
      (internal-body
	(define-vector-tuple-type <stuff>
	  (fields {a <fixnum>}
		  {b <flonum>}
		  {c <string>}))
	(is-a? '#(4 5.6 "hello") <stuff>))
    => #t)

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
