;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of SRFI 114
;;;Date: Sun Mar  8, 2015
;;;
;;;Abstract
;;;
;;;	This library is derived from SRFI 114 reference implementation.
;;;
;;;Copyright (C) John Cowan 2013.  All Rights Reserved.
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software  and associated documentation  files (the ``Software''), to  deal in
;;;the Software without restriction, including  without limitation the rights to use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED ``AS  IS'', WITHOUT  WARRANTY OF  ANY KIND,  EXPRESS OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;


#!vicare
(library (srfi :114 comparators)
  (export
    ;;
    comparator? comparator-comparison-procedure?
    comparator-hash-function?
    ;;
    boolean-comparator char-comparator char-ci-comparator
    string-comparator string-ci-comparator symbol-comparator
    fixnum-comparator
    exact-integer-comparator integer-comparator rational-comparator
    real-comparator complex-comparator number-comparator
    pair-comparator list-comparator vector-comparator
    bytevector-comparator
    ;;
    default-comparator comparator-register-default!
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
    comparator-type-test-procedure
    (rename (comparator-type-test-procedure
	     comparator-test-type-procedure))
    comparator-check-type-procedure
    (rename (comparator-check-type-procedure
	     comparator-type-check-procedure))
    comparator-equality-predicate
    comparator-comparison-procedure comparator-hash-function
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
    ;;
    make-comparator-hashtable
    ;;
    ;; condition objects
    &comparator-error make-comparator-error comparator-error?

    &unsupported-comparator-operation-error
    make-unsupported-comparator-operation-error
    unsupported-comparator-operation-error?
    unsupported-comparator-operation-error.comparator
    raise-unsupported-comparator-operation-error

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
    inexact-real-comparator-with-ignored-epsilon.rounding

    &comparator-debug-error
    make-comparator-debug-error
    comparator-debug-error?
    comparator-debug-error.debug-comparator
    comparator-debug-error.comparator
    raise-comparator-debug-error)
  (import (vicare containers comparators)))

;;; end of file
