;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 114
;;;Date: Sun Mar  8, 2015
;;;
;;;Abstract
;;;
;;;	There is no test suite in  the reference implementation.  So this program has
;;;	been written specifically for Vicare.
;;;
;;;     NOTE It is  unlikely that I have gotten everything  right.  (Marco Maggi; Sun
;;;     Mar 8, 2015)
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


#!r6rs
(import (vicare)
  (vicare checks)
  (srfi :114))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: SRFI 114, comparators\n")


(parametrise ((check-test-name	'predicates))

  (check
      (comparator? 123)
    => #f)

  (check-for-true (comparator? default-comparator))

  (check-for-true (comparator? boolean-comparator))
  (check-for-true (comparator? char-comparator))
  (check-for-true (comparator? char-ci-comparator))
  (check-for-true (comparator? string-comparator))
  (check-for-true (comparator? string-ci-comparator))
  (check-for-true (comparator? symbol-comparator))

  (check-for-true (comparator? exact-integer-comparator))
  (check-for-true (comparator? integer-comparator))
  (check-for-true (comparator? rational-comparator))
  (check-for-true (comparator? real-comparator))
  (check-for-true (comparator? complex-comparator))
  (check-for-true (comparator? number-comparator))
  (check-for-true (comparator? pair-comparator))
  (check-for-true (comparator? list-comparator))
  (check-for-true (comparator? vector-comparator))
  (check-for-true (comparator? bytevector-comparator))

;;; --------------------------------------------------------------------

  (check-for-procedure-argument-violation
      (comparator-comparison-procedure? 123)
    => '(comparator (123)))

  (check-for-true (comparator-comparison-procedure? default-comparator))

  (check-for-true (comparator-comparison-procedure? boolean-comparator))
  (check-for-true (comparator-comparison-procedure? char-comparator))
  (check-for-true (comparator-comparison-procedure? char-ci-comparator))
  (check-for-true (comparator-comparison-procedure? string-comparator))
  (check-for-true (comparator-comparison-procedure? string-ci-comparator))
  (check-for-true (comparator-comparison-procedure? symbol-comparator))

  (check-for-true (comparator-comparison-procedure? exact-integer-comparator))
  (check-for-true (comparator-comparison-procedure? integer-comparator))
  (check-for-true (comparator-comparison-procedure? rational-comparator))
  (check-for-true (comparator-comparison-procedure? real-comparator))
  (check-for-true (comparator-comparison-procedure? complex-comparator))
  (check-for-true (comparator-comparison-procedure? number-comparator))
  (check-for-true (comparator-comparison-procedure? pair-comparator))
  (check-for-true (comparator-comparison-procedure? list-comparator))
  (check-for-true (comparator-comparison-procedure? vector-comparator))
  (check-for-true (comparator-comparison-procedure? bytevector-comparator))

;;; --------------------------------------------------------------------

  (check-for-procedure-argument-violation
      (comparator-hash-function? 123)
    => '(comparator (123)))

  (check-for-true (comparator-hash-function? default-comparator))

  (check-for-true (comparator-hash-function? boolean-comparator))
  (check-for-true (comparator-hash-function? char-comparator))
  (check-for-true (comparator-hash-function? char-ci-comparator))
  (check-for-true (comparator-hash-function? string-comparator))
  (check-for-true (comparator-hash-function? string-ci-comparator))
  (check-for-true (comparator-hash-function? symbol-comparator))

  (check-for-true (comparator-hash-function? exact-integer-comparator))
  (check-for-true (comparator-hash-function? integer-comparator))
  (check-for-true (comparator-hash-function? rational-comparator))
  (check-for-true (comparator-hash-function? real-comparator))
  (check-for-true (comparator-hash-function? complex-comparator))
  (check-for-true (comparator-hash-function? number-comparator))
  (check-for-true (comparator-hash-function? pair-comparator))
  (check-for-true (comparator-hash-function? list-comparator))
  (check-for-true (comparator-hash-function? vector-comparator))
  (check-for-true (comparator-hash-function? bytevector-comparator))

  #t)


(parametrise ((check-test-name	'standard))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?C ?a ?b)
       (begin
	 (check-for-true  ((comparator-type-test-procedure ?C) ?a))
	 (check-for-true  ((comparator-type-test-procedure ?C) ?b))
	 (check-for-false ((comparator-type-test-procedure ?C) (void)))
	 (check-for-true  ((comparator-equality-predicate ?C) ?a ?a))
	 (check-for-false ((comparator-equality-predicate ?C) ?a ?b))
	 (check
	     ((comparator-comparison-procedure ?C) ?a ?a)
	   => 0)
	 (check
	     ((comparator-comparison-procedure ?C) ?a ?b)
	   => -1)
	 (check
	     ((comparator-comparison-procedure ?C) ?b ?a)
	   => +1)
	 (check-for-true
	  (non-negative-exact-integer? ((comparator-hash-function ?C) ?a)))
	 (check-for-true
	  (non-negative-exact-integer? ((comparator-hash-function ?C) ?b)))
	 ))
      ))

;;; --------------------------------------------------------------------

  (doit default-comparator #f #t)

  (doit boolean-comparator #f #t)
  (doit char-comparator #\a #\b)
  (doit char-ci-comparator #\A #\b)
  (doit string-comparator "a" "b")
  (doit string-ci-comparator "A" "b")
  (doit symbol-comparator 'a 'b)

  (doit exact-integer-comparator 1 2)
  (doit integer-comparator 1 2.0)
  (doit rational-comparator 1/3 2/3)
  (doit real-comparator 1.1 2.2)
  (doit complex-comparator 1.1+3.3i 2.2+4.4i)
  (doit number-comparator 1 2.0+3i)
  (doit pair-comparator '(1 . 2) '(3 . 4))
  (doit list-comparator '(1 2) '(3 4))
  (doit vector-comparator '#(1 2) '#(3 4))
  (doit bytevector-comparator '#vu8(1 2) '#vu8(3 4))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
