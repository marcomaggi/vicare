;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <pair> type
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
(program (test-types-pair-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: <pair> objects\n")


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

  (doit (cons 1 2)
	#'((pair <positive-fixnum> <positive-fixnum>)))

  (doit (cons 1 "ciao")
	#'((pair <positive-fixnum> <string>)))

  (doit (cons 1 (read))
	#'((pair <positive-fixnum> <top>)))

  (doit (cons 1 (cons 2 3))
	#'((pair <positive-fixnum> (pair <positive-fixnum> <positive-fixnum>))))

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

  (doit <pair>
  	<pair>)

  (doit (pair <fixnum> <flonum>)
  	(pair <fixnum> <flonum>))

  (doit (pair <fixnum> <fixnum>)
  	(pair <fixnum> <fixnum>))

  (doit (pair-of <fixnum>)
  	(pair-of <fixnum>))

  (void))


(parametrise ((check-test-name	'predicate))

  (check-for-true	(is-a? '(1 . 2) <pair>))
  (check-for-false	(is-a? 123 <pair>))

  (check-for-false	(is-a? '() <pair>))
  (check-for-true	(is-a? '(1) <pair>))
  (check-for-true	(is-a? '(1 2 . 3) <pair>))

  (check-for-true	(let (({O <pair>} '(1 . 2)))
			  (is-a? O <pair>)))

  (check-for-true	(let (({O <top>} '(1 . 2)))
			  (is-a? O <pair>)))

  (check-for-false	(let (({O <top>} "ciao"))
			  (is-a? O <pair>)))

  (void))


(parametrise ((check-test-name	'constructor))

  (check
      (new <pair> 1 2)
    => '(1 . 2))

  (check
      (.tags (type-of (new <pair> (read) (read))))
    (=> syntax=?)
    #'((pair <top> <top>)))

  #t)


(parametrise ((check-test-name	'methods))

  (check
      (.car (new <pair> 1 2))
    => 1)

  (check
      (.cdr (new <pair> 1 2))
    => 2)

  (void))


(parametrise ((check-test-name	'late-binding))

  (check
      (method-call-late-binding 'car (new <pair> 1 2))
    => 1)

  (check
      (method-call-late-binding 'cdr (new <pair> 1 2))
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
