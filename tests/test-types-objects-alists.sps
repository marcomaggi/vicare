;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the <alist-type-spec> type
;;;Date: Mon Apr  4, 2016
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
(program (test-types-alist-objects)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: <alist-type-spec> objects\n")


(parametrise ((check-test-name	'predicate))

  (check-for-true	(is-a? '()
			       (alist <symbol> <fixnum>)))

  (check-for-true	(is-a? '((a . 1))
			       (alist <symbol> <fixnum>)))

  (check-for-true	(is-a? '((a . 1) (b . 2) (c . 3))
			       (alist <symbol> <fixnum>)))

  (check-for-true	(let ((O '((a . 1) (b . 2) (c . 3))))
			  (is-a? O
				 (alist <symbol> <fixnum>))))

  (check-for-true	(let (({O (alist <symbol> <fixnum>)} '((a . 1) (b . 2) (c . 3))))
			  (is-a? O
				 (alist <symbol> <fixnum>))))

;;;

  (check-for-false	(is-a? '(("a" . 1) (b . 2) (c . 3))	(alist <symbol> <fixnum>)))
  (check-for-false	(is-a? '((a . "1") (b . 2) (c . 3))	(alist <symbol> <fixnum>)))
  (check-for-false	(is-a? '((a . 1) ("b" . 2) (c . 3))	(alist <symbol> <fixnum>)))
  (check-for-false	(is-a? '((a . 1) (b . "2") (c . 3))	(alist <symbol> <fixnum>)))
  (check-for-false	(is-a? '((a . 1) (b . 2) ("c" . 3))	(alist <symbol> <fixnum>)))
  (check-for-false	(is-a? '((a . 1) (b . 2) (c . "3"))	(alist <symbol> <fixnum>)))

  (void))


(parametrise ((check-test-name	'super-and-sub))

;;; alist is super

  (check-for-true	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list-of (pair <symbol> <number>))))

  (check-for-true	(type-annotation-super-and-sub? (alist <real> <number>)
							(list-of (pair-of (or <fixnum> <flonum>)))))

  (check-for-true	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list (pair <symbol> <number>)
							      (pair <symbol> <number>))))

  (check-for-true	(type-annotation-super-and-sub? (alist <real> <number>)
							(list (pair-of (or <fixnum> <flonum>))
							      (pair-of (or <fixnum> <flonum>)))))

;;; no match

  (check-for-false	(type-annotation-super-and-sub? (alist <symbol> <number>)
							<list>))

  (check-for-false	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list-of (or <symbol> <number>))))

  (check-for-false	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list-of (pair <symbol> <boolean>))))

  (check-for-false	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list-of (pair <boolean> <number>))))

  (check-for-false	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list (pair <boolean> <number>)
							      (pair <symbol> <number>))))

  (check-for-false	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list (pair-of (or <symbol> <number>))
							      (pair-of (or <symbol> <boolean>)))))

;;; --------------------------------------------------------------------
;;; alist is sub

  (check-for-true	(type-annotation-super-and-sub? (list-of (pair <symbol> <number>))
							(alist <symbol> <number>)))

  (check-for-true	(type-annotation-super-and-sub? (list-of (pair-of <real>))
							(alist <fixnum> <flonum>)))

  (check-for-true	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list (pair <symbol> <number>)
							      (pair <symbol> <flonum>))))

  (check-for-true	(type-annotation-super-and-sub? (alist <real> <number>)
							(list (pair-of (or <fixnum> <flonum>))
							      (pair-of (or <fixnum> <flonum>)))))

;;; no match

  (check-for-true	(type-annotation-super-and-sub? <list>
							(alist <symbol> <number>)))

  (check-for-false	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list-of (or <symbol> <number>))))

  (check-for-false	(type-annotation-super-and-sub? (list-of (pair <symbol> <number>))
							(alist <keyword> <number>)))

  (check-for-false	(type-annotation-super-and-sub? (list-of (pair-of (or <symbol> <number>)))
							(alist <symbol> <keyword>)))

  (check-for-false	(type-annotation-super-and-sub? (list (pair <symbol> <number>)
							      (pair <symbol> <number>))
							(alist <keyword> <number>)))

  (check-for-false	(type-annotation-super-and-sub? (alist <symbol> <number>)
							(list (pair-of (or <symbol> <keyword>))
							      (pair-of (or <symbol> <keyword>)))))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
