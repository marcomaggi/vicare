;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for ratnums core primitives
;;Date: Sat Jan  2, 2016
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (typed-core-primitives ratnums)
  (export typed-core-primitives.ratnums)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.ratnums)


;;;; ratnums, safe operations

(section

(declare-type-predicate ratnum? <ratnum>)

(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?true-tag ?false-tag)
			  (declare-core-primitive ?who
			      (safe)
			    (signatures
			     ((?true-tag)	=> (<true>))
			     ((?false-tag)	=> (<false>))
			     ((<ratnum>)	=> (<boolean>)))))
			 )))
  (define-predicate ratnum-positive?		<positive-ratnum>	<negative-ratnum>)
  (define-predicate ratnum-negative?		<negative-ratnum>	<positive-ratnum>)
  (define-predicate ratnum-non-positive?	<negative-ratnum>	<positive-ratnum>)
  (define-predicate ratnum-non-negative?	<positive-ratnum>	<negative-ratnum>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?true-tag ?false-tag)
			  (declare-core-primitive ?who
			      (safe)
			    (signatures
			     ((?true-tag)	=> (<true>))
			     ((?false-tag)	=> (<false>))
			     ((<ratnum>)	=> (<boolean>))
			     ((<top>)		=> (<boolean>)))))
			 )))
  (define-predicate positive-ratnum?		<positive-ratnum>	<negative-ratnum>)
  (define-predicate negative-ratnum?		<negative-ratnum>	<positive-ratnum>)
  (define-predicate non-positive-ratnum?	<negative-ratnum>	<positive-ratnum>)
  (define-predicate non-negative-ratnum?	<positive-ratnum>	<negative-ratnum>)
  #| end of LET-SYNTAX |# )

/section)


;;;; ratnums, unsafe operations

(section

(declare-core-primitive $make-ratnum
    (unsafe)
  (signatures
   ((<exact-integer> <exact-integer>)		=> (<ratnum>)))
  (attributes
   ((_ _)					foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive $ratnum-num
    (unsafe)
  (signatures
   ((<ratnum>)					=> (<non-zero-exact-integer>)))
  (attributes
   ((_)						foldable effect-free result-true)))

(declare-core-primitive $ratnum-den
    (unsafe)
  (signatures
   ((<ratnum>)					=> (<positive-exact-integer>)))
  (attributes
   ((_)						foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-ratnum-predicate $ratnum-positive? unsafe)
(declare-ratnum-predicate $ratnum-negative? unsafe)
(declare-ratnum-predicate $ratnum-non-positive? unsafe)
(declare-ratnum-predicate $ratnum-non-negative? unsafe)

;;; --------------------------------------------------------------------

(declare-core-primitive $ratnum->flonum
    (unsafe)
  (signatures
   ((<ratnum>)			=> (<flonum>)))
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
