;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of SRFI 114
;;;Date: Sun Mar  8, 2015
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

#!r6rs
(library (srfi :114)
  (export
    ;;
    comparator? comparator-comparison-procedure?
    comparator-hash-function?
    ;;
    boolean-comparator char-comparator char-ci-comparator
    string-comparator string-ci-comparator symbol-comparator
    exact-integer-comparator integer-comparator rational-comparator
    real-comparator complex-comparator number-comparator
    pair-comparator list-comparator vector-comparator
    bytevector-comparator
    ;;
    default-comparator
    ;;
    make-comparator make-inexact-real-comparator make-vector-comparator
    make-bytevector-comparator make-list-comparator
    make-vectorwise-comparator make-listwise-comparator
    make-car-comparator make-cdr-comparator make-pair-comparator
    make-improper-list-comparator make-selecting-comparator
    make-refining-comparator make-reverse-comparator
    make-debug-comparator
    ;;
    eq-comparator eqv-comparator equal-comparator
    ;;
    comparator-type-test-procedure comparator-check-type-procedure
    comparator-equality-predicate comparator-comparison-procedure
    comparator-hash-function
    ;;
    comparator-test-type comparator-check-type comparator-equal?
    comparator-compare comparator-hash
    ;;
    make-comparison< make-comparison> make-comparison<=
    make-comparison>= make-comparison=/< make-comparison=/>
    ;;
    if3 if=? if<? if>? if<=? if>=? if-not=?
    ;;
    =? <? >? <=? >=?
    ;;
    make= make<  make> make<= make>=
    ;;
    in-open-interval? in-closed-interval? in-open-closed-interval?
    in-closed-open-interval?
    ;;
    comparator-min comparator-max

    comparator-register-default!

    ;; condition objects
    &comparator-error make-comparator-error comparator-error?

    &comparator-type-error make-comparator-type-error comparator-type-error?
    comparator-type-error.comparator comparator-type-error.objects
    raise-comparator-type-error

    &comparator-nan-comparison-error
    make-comparator-nan-comparison-error-condition
    condition-comparator-nan-comparison-error?
    comparator-nan-comparison-error.comparator
    raise-comparator-nan-comparison-error

    &inexact-real-comparator-with-ignored-epsilon
    make-inexact-real-comparator-with-ignored-epsilon-condition
    condition-inexact-real-comparator-with-ignored-epsilon?
    inexact-real-comparator-with-ignored-epsilon.epsilon
    inexact-real-comparator-with-ignored-epsilon.rounding)
  (import (srfi :114 comparators)))

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
