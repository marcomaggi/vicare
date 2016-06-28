;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for DEFINE-INTERFACE
;;;Date: Sat Jun 25, 2016
;;;
;;;Abstract
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
(program (test-types-interfaces)
  (options typed-language)
  (import (vicare)
    (vicare language-extensions interfaces)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** test Vicare typed language: DEFINE-INTERFACE\n")


;;;; helpers

(define-type <all-fixnums>
  (or <non-negative-fixnum> <negative-fixnum>))

(define-syntax matching
  (syntax-rules (=>)
    ((_ ?one ?two => ?expected)
     (check
	 (type-annotation-matching ?one ?two)
       => (quote ?expected)))
    ))


(parametrise ((check-test-name	'type-inspection))

  (define-interface <Sequence>
    (method-prototype length	(lambda (<bottom>) => (<non-negative-exact-integer>)))
    (method-prototype first	(lambda (<bottom>) => (<char>)))
    (method-prototype ref	(lambda (<bottom> <non-negative-exact-integer>) => (<char>)))
    (method ({just-length <non-negative-exact-integer>})
      (.length this))
    (case-method just-item
      (({_ <char>})
       (.ref this 0))
      (({_ <char>} {idx <non-negative-exact-integer>})
       (.ref this idx)))
    (method/overload (doit)
      this))

  (define-record-type <str>
    (implements <Sequence>)
    (fields {str <string>})
    (method ({length <non-negative-fixnum>} {O <str>})
      (string-length (.str O)))
    (method ({first  <char>} {O <str>})
      (string-ref    (.str O) 0))
    (method ({ref    <char>} {O <str>} {idx <non-negative-exact-integer>})
      (string-ref    (.str O) idx)))

  (define (fun {O <Sequence>})
    (.length O))

;;; --------------------------------------------------------------------

  (check (type-annotation-matching <Sequence> <Sequence>)	=> 'exact-match)
  (check (type-annotation-matching <Sequence> <str>)		=> 'exact-match)
  (check (type-annotation-matching <str> <Sequence>)		=> 'no-match)
  (check (type-annotation-matching <Sequence> <fixnum>)		=> 'no-match)
  (check (type-annotation-matching <fixnum> <Sequence>)		=> 'no-match)

  (check-for-true	(type-annotation-super-and-sub? <Sequence> <Sequence>))
  (check-for-true	(type-annotation-super-and-sub? <Sequence> <str>))
  (check-for-false	(type-annotation-super-and-sub? <str> <Sequence>))
  (check-for-false	(type-annotation-super-and-sub? <Sequence> <fixnum>))
  (check-for-false	(type-annotation-super-and-sub? <fixnum> <Sequence>))

;;; --------------------------------------------------------------------

  (check
      (let ((O (new <str> "ciao")))
	(fun O))
    => 4)

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
