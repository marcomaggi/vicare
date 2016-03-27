;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for bignums core primitives
;;Date: Tue Dec 22, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;

#!vicare
(library (typed-core-primitives bignums)
  (export typed-core-primitives.bignums)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.bignums)


;;;; bignums, safe operations

(section

(declare-core-primitive least-positive-bignum
    (safe)
  (signatures
   (()				=> (<positive-bignum>)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive greatest-negative-bignum
    (safe)
  (signatures
   (()				=> (<negative-bignum>)))
  (attributes
   (()				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-type-predicate bignum? <bignum>)

(declare-bignum-predicate bignum-even?		(replacements $bignum-even?))
(declare-bignum-predicate bignum-odd?		(replacements $bignum-odd?))

(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?true-tag ?false-tag)
			  (declare-core-primitive ?who
			      (safe)
			    (signatures
			     ((?true-tag)	=> (<true>))
			     ((?false-tag)	=> (<false>))
			     ((<bignum>)	=> (<boolean>)))))
			 )))
  (define-predicate bignum-positive?		<positive-bignum>	<negative-bignum>)
  (define-predicate bignum-negative?		<negative-bignum>	<positive-bignum>)
  (define-predicate bignum-non-positive?	<negative-bignum>	<positive-bignum>)
  (define-predicate bignum-non-negative?	<positive-bignum>	<negative-bignum>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?true-tag ?false-tag)
			  (declare-core-primitive ?who
			      (safe)
			    (signatures
			     ((?true-tag)	=> (<true>))
			     ((?false-tag)	=> (<false>))
			     ((<bignum>)	=> (<boolean>))
			     ((<top>)		=> (<boolean>)))))
			 )))
  (define-predicate positive-bignum?		<positive-bignum>	<negative-bignum>)
  (define-predicate negative-bignum?		<negative-bignum>	<positive-bignum>)
  (define-predicate non-positive-bignum?	<negative-bignum>	<positive-bignum>)
  (define-predicate non-negative-bignum?	<positive-bignum>	<negative-bignum>)
  #| end of LET-SYNTAX |# )


;;;; bignums, unsafe operations

;;; predicates

(declare-bignum-predicate $bignum-positive? unsafe)
(declare-bignum-predicate $bignum-negative? unsafe)
(declare-bignum-predicate $bignum-non-positive? unsafe)
(declare-bignum-predicate $bignum-non-negative? unsafe)

(declare-bignum-predicate $bignum-even? unsafe)
(declare-bignum-predicate $bignum-odd? unsafe)

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $bignum-byte-ref
    (unsafe)
  (signatures
   ((<bignum> <fixnum>)		=> (<fixnum>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive $bignum-size
    (unsafe)
  (signatures
   ((<bignum>)			=> (<fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $bignum->flonum
    (unsafe)
  (signatures
   ((<bignum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
