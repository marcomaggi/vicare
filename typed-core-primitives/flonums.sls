;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for flonums core primitives
;;Date: Tue Dec 31, 2015
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
(library (typed-core-primitives flonums)
  (export typed-core-primitives.flonums)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.flonums)


;;;; flonums, safe functions

(section

;;; predicates

 (declare-type-predicate flonum? <flonum>)
 (declare-list-of-type-predicate list-of-flonums?	<flonum>)

;;; --------------------------------------------------------------------
;;; range predicates raising error if the argument is not a flonum

;;;NOTE Compliance with IEEE 754 imposes constraints  on how to handle +0.0 and -0.0:
;;;flonum zero is separeted from positive flonums and negative flonums; +0.0 is *not*
;;;positive; -0.0 is *not* negative.

 (declare-core-primitive flzero?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<true>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

 (declare-core-primitive flzero?/positive
     (safe)
   (signatures
    ((<positive-zero-flonum>)		=> (<true>))
    ((<negative-zero-flonum>)		=> (<false>))
    ((<zero-flonum>)			=> (<boolean>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

 (declare-core-primitive flzero?/negative
     (safe)
   (signatures
    ((<positive-zero-flonum>)		=> (<false>))
    ((<negative-zero-flonum>)		=> (<true>))
    ((<zero-flonum>)			=> (<boolean>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

;;;

 (declare-core-primitive flpositive?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<false>))
    ;;
    ((<positive-flonum>)		=> (<true>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<false>))
    ;;
    ((<flonum>)				=> (<boolean>))))

 (declare-core-primitive flnonnegative?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<true>))
    ;;
    ((<positive-flonum>)		=> (<true>))
    ((<non-negative-flonum>)		=> (<true>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

;;;

 (declare-core-primitive flnegative?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<false>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<false>))
    ;;
    ((<negative-flonum>)		=> (<true>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

 (declare-core-primitive flnonpositive?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<true>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<true>))
    ((<non-positive-flonum>)		=> (<true>))
    ;;
    ((<flonum>)				=> (<boolean>))))

;;; --------------------------------------------------------------------
;;; range predicates returning false if the argument is not a flonum

;;;NOTE Compliance with IEEE 754 imposes constraints  on how to handle +0.0 and -0.0:
;;;flonum zero is separeted from positive flonums and negative flonums; +0.0 is *not*
;;;positive; -0.0 is *not* negative.

 (declare-core-primitive zero-flonum?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<true>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))
    ;;
    (((ancestor-of <fixnum>))		=> (<boolean>))
    (((not <flonum>))			=> (<false>))))

 (declare-core-primitive positive-zero-flonum?
     (safe)
   (signatures
    ((<positive-zero-flonum>)		=> (<true>))
    ((<negative-zero-flonum>)		=> (<false>))
    ((<zero-flonum>)			=> (<boolean>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))
    ;;
    (((ancestor-of <fixnum>))		=> (<boolean>))
    (((not <flonum>))			=> (<false>))))

 (declare-core-primitive negative-zero-flonum?
     (safe)
   (signatures
    ((<positive-zero-flonum>)		=> (<false>))
    ((<negative-zero-flonum>)		=> (<true>))
    ((<zero-flonum>)			=> (<boolean>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))
    ;;
    (((ancestor-of <fixnum>))		=> (<boolean>))
    (((not <flonum>))			=> (<false>))))

;;;

 (declare-core-primitive positive-flonum?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<false>))
    ;;
    ((<positive-flonum>)		=> (<true>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<false>))
    ;;
    ((<flonum>)				=> (<boolean>))
    ;;
    (((ancestor-of <fixnum>))		=> (<boolean>))
    (((not <flonum>))			=> (<false>))))

 (declare-core-primitive non-negative-flonum?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<true>))
    ;;
    ((<positive-flonum>)		=> (<true>))
    ((<non-negative-flonum>)		=> (<true>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))
    ;;
    (((ancestor-of <fixnum>))		=> (<boolean>))
    (((not <flonum>))			=> (<false>))))

;;;

 (declare-core-primitive negative-flonum?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<false>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<false>))
    ;;
    ((<negative-flonum>)		=> (<true>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))
    ;;
    (((ancestor-of <fixnum>))		=> (<boolean>))
    (((not <flonum>))			=> (<false>))))

 (declare-core-primitive non-positive-flonum?
     (safe)
   (signatures
    ((<zero-flonum>)			=> (<true>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<false>))
    ;;
    ((<negative-flonum>)		=> (<true>))
    ((<non-positive-flonum>)		=> (<true>))
    ;;
    ((<flonum>)				=> (<boolean>))
    ;;
    (((ancestor-of <fixnum>))		=> (<boolean>))
    (((not <flonum>))			=> (<false>))))

;;; --------------------------------------------------------------------

 (declare-flonum-predicate fleven?		(replacements $fleven?))
 (declare-flonum-predicate flodd?		(replacements $flodd?))

 (declare-core-primitive flinteger?
     (safe)
   (signatures
    ((<flonum>)			=> (<boolean>)))
   (attributes
    ((_)			foldable effect-free))
   (replacements $flonum-integer?))

 (declare-core-primitive flfinite?
     (safe)
   (signatures
    ((<flonum>)			=> (<boolean>)))
   (attributes
    ((_)			foldable effect-free))
   (replacements $flonum-rational?))

 (declare-core-primitive flinfinite?
     (safe)
   (signatures
    ((<flonum>)			=> (<boolean>)))
   (attributes
    ((_)			foldable effect-free))
   (replacements $flinfinite?))

 (declare-core-primitive flnan?
     (safe)
   (signatures
    ((<flonum>)			=> (<boolean>)))
   (attributes
    ((_)			foldable effect-free))
   (replacements $flnan?))

;;; --------------------------------------------------------------------
;;; rounding

 (let-syntax
     ((declare-flonum-rounding (syntax-rules ()
				 ((_ ?who ?replacement)
				  (declare-core-primitive ?who
				      (safe)
				    (signatures
				     ((<flonum>)	=> (<flonum>)))
				    (attributes
				     ((_)		foldable effect-free result-true))
				    (replacements ?replacement))))))
   (declare-flonum-rounding flround	 $flround)
   (declare-flonum-rounding flfloor	 $flfloor)
   (declare-flonum-rounding flceiling	 $flceiling)
   (declare-flonum-rounding fltruncate	 $fltruncate)
   #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; parts

 (declare-flonum-unary flnumerator	(replacements $flnumerator))
 (declare-flonum-unary fldenominator	(replacements $fldenominator))
 (declare-flonum-unary flabs		(replacements $flabs))

 (declare-core-primitive flonum-bytes
     (safe)
   (signatures
    ((<flonum>)				=> (<flonum> <flonum> <flonum> <flonum>
						     <flonum> <flonum> <flonum> <flonum>)))
   (attributes
    ((_)				effect-free)))

 (declare-core-primitive flonum-parts
     (safe)
   (signatures
    ((<flonum>)				=> (<boolean> <fixnum> <exact-integer>)))
   (attributes
    ((_)				effect-free)))

;;; --------------------------------------------------------------------
;;; trigonometric

 (declare-flonum-unary flsin		(replacements $flsin))
 (declare-flonum-unary flcos		(replacements $flcos))
 (declare-flonum-unary fltan		(replacements $fltan))
 (declare-flonum-unary flasin		(replacements $flasin))
 (declare-flonum-unary flacos		(replacements $flacos))

 (declare-core-primitive flatan
     (safe)
   (signatures
    ((<flonum>)			=> (<flonum>))
    ((<flonum> <flonum>)	=> (<flonum>)))
   (attributes
    ((_)			foldable effect-free result-true)
    ((_ _)			foldable effect-free result-true))
   (replacements $flatan $flatan2))

;;; --------------------------------------------------------------------
;;; hyperbolic

 (declare-flonum-unary flsinh		(replacements $flsinh))
 (declare-flonum-unary flcosh		(replacements $flcosh))
 (declare-flonum-unary fltanh		(replacements $fltanh))
 (declare-flonum-unary flasinh		(replacements $flasinh))
 (declare-flonum-unary flacosh		(replacements $flacosh))
 (declare-flonum-unary flatanh		(replacements $flatanh))

;;; --------------------------------------------------------------------
;;; exponentiation, exponentials, logarithms

 (declare-flonum-unary flexp		(replacements $flexp))
 (declare-flonum-unary/binary fllog	(replacements $fllog $fllog2))
 (declare-flonum-unary flexpm1		(replacements $flexpm1))
 (declare-flonum-unary fllog1p		(replacements $fllog1p))
 (declare-flonum-binary flexpt		(replacements $flexpt))
 (declare-flonum-unary flsqrt		(replacements $flsqrt))
 (declare-flonum-unary flsquare		(replacements $flsquare))
 (declare-flonum-unary flcube		(replacements $flcube))
 (declare-flonum-unary flcbrt		(replacements $flcbrt))
 (declare-flonum-binary flhypot		(replacements $flhypot))

;;; --------------------------------------------------------------------
;;; comparison

 (declare-flonum-unary/multi-comparison fl=?	(replacements $fl=))
 (declare-flonum-unary/multi-comparison fl!=?	(replacements $fl!=))
 (declare-flonum-unary/multi-comparison fl<?	(replacements $fl<))
 (declare-flonum-unary/multi-comparison fl>?	(replacements $fl>))
 (declare-flonum-unary/multi-comparison fl<=?	(replacements $fl<=))
 (declare-flonum-unary/multi-comparison fl>=?	(replacements $fl>=))

;;; --------------------------------------------------------------------
;;; arithmetics

 (declare-flonum-unary/multi fl+		(replacements $fl+))
 (declare-flonum-unary/multi fl-		(replacements $fl-))
 (declare-flonum-unary/multi fl*		(replacements $fl*))
 (declare-flonum-unary/multi fl/		(replacements $fl/))

 (declare-flonum-unary/multi flmin	(replacements $flmin))
 (declare-flonum-unary/multi flmax	(replacements $flmax))

 (declare-flonum-binary fldiv		(replacements $fldiv))
 (declare-flonum-binary fldiv0		(replacements $fldiv0))
 (declare-flonum-binary flmod		(replacements $flmod))
 (declare-flonum-binary flmod0		(replacements $flmod0))

 (declare-core-primitive fldiv-and-mod
     (safe)
   (signatures
    ((<flonum> <flonum>)		=> (<flonum> <flonum>)))
   (attributes
    ((_ _)				effect-free)))

 (declare-core-primitive fldiv0-and-mod0
     (safe)
   (signatures
    ((<flonum> <flonum>)		=> (<flonum> <flonum>)))
   (attributes
    ((_ _)				effect-free)))

;;; --------------------------------------------------------------------
;;; conversion

 (declare-core-primitive flonum->string
     (safe)
   (signatures
    ((<flonum>)		=> (<string>)))
   (attributes
    ((_)			foldable effect-free result-true)))

 (declare-core-primitive real->flonum
     (safe)
   (signatures
    ((<real>)		=> (<flonum>)))
   (attributes
    ((_)			foldable effect-free result-true)))

 /section)


;;;; flonums, unsafe functions

(section

(declare-core-primitive $make-flonum
    (unsafe)
  (signatures
   (()				=> (<flonum>)))
  ;;Not foldable because $MAKE-FLONUM must return a new flonum every time.
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive $flonum->maybe-exact
    (unsafe)
  (signatures
   ((<flonum>)			=> ((or <false> <exact-rational>))))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $flexact
    (unsafe)
  (signatures
   ((<flonum>)			=> (<exact-rational>)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

 (declare-core-primitive $flzero?
     (unsafe)
   (signatures
    ((<zero-flonum>)			=> (<true>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

 (declare-core-primitive $flzero?/positive
     (unsafe)
   (signatures
    ((<positive-zero-flonum>)		=> (<true>))
    ((<negative-zero-flonum>)		=> (<false>))
    ((<zero-flonum>)			=> (<boolean>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

 (declare-core-primitive $flzero?/negative
     (unsafe)
   (signatures
    ((<positive-zero-flonum>)		=> (<false>))
    ((<negative-zero-flonum>)		=> (<true>))
    ((<zero-flonum>)			=> (<boolean>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

;;;

 (declare-core-primitive $flpositive?
     (unsafe)
   (signatures
    ((<zero-flonum>)			=> (<false>))
    ;;
    ((<positive-flonum>)		=> (<true>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<false>))
    ;;
    ((<flonum>)				=> (<boolean>))))

 (declare-core-primitive $flnonnegative?
     (unsafe)
   (signatures
    ((<zero-flonum>)			=> (<true>))
    ;;
    ((<positive-flonum>)		=> (<true>))
    ((<non-negative-flonum>)		=> (<true>))
    ;;
    ((<negative-flonum>)		=> (<false>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

;;;

 (declare-core-primitive $flnegative?
     (unsafe)
   (signatures
    ((<zero-flonum>)			=> (<false>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<false>))
    ;;
    ((<negative-flonum>)		=> (<true>))
    ((<non-positive-flonum>)		=> (<boolean>))
    ;;
    ((<flonum>)				=> (<boolean>))))

 (declare-core-primitive $flnonpositive?
     (unsafe)
   (signatures
    ((<zero-flonum>)			=> (<true>))
    ;;
    ((<positive-flonum>)		=> (<false>))
    ((<non-negative-flonum>)		=> (<boolean>))
    ;;
    ((<negative-flonum>)		=> (<true>))
    ((<non-positive-flonum>)		=> (<true>))
    ;;
    ((<flonum>)				=> (<boolean>))))

;;; --------------------------------------------------------------------
;;; other predicates

 (declare-flonum-predicate $fleven? unsafe)
 (declare-flonum-predicate $flodd? unsafe)

 (declare-core-primitive $flonum-integer?
   (unsafe)
   (signatures
    ((<flonum>)			=> (<boolean>)))
   (attributes
    ((_)			foldable effect-free)))

 (declare-core-primitive $flonum-rational?
   (unsafe)
   (signatures
    ((<flonum>)			=> (<boolean>)))
   (attributes
    ((_)			foldable effect-free)))

 (declare-core-primitive $flinfinite?
   (unsafe)
   (signatures
    ((<flonum>)			=> (<boolean>)))
   (attributes
    ((_)			foldable effect-free)))

 (declare-core-primitive $flnan?
   (unsafe)
   (signatures
    ((<flonum>)			=> (<boolean>)))
   (attributes
    ((_)			foldable effect-free)))

;;; --------------------------------------------------------------------
;;; rounding

 (let-syntax
     ((declare-flonum-rounding (syntax-rules ()
				 ((_ ?who)
				  (declare-core-primitive ?who
				    (unsafe)
				    (signatures
				     ((<flonum>)	=> (<flonum>)))
				    (attributes
				     ((_)		foldable effect-free result-true)))))))
   (declare-flonum-rounding $flround)
   (declare-flonum-rounding $flfloor)
   (declare-flonum-rounding $flceiling)
   (declare-flonum-rounding $fltruncate)
   #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; parts

 (declare-flonum-unary $flnumerator unsafe)
 (declare-flonum-unary $fldenominator unsafe)
 (declare-flonum-unary $flabs unsafe)

 (declare-core-primitive $flonum-u8-ref
   (unsafe)
   (signatures
    ((<flonum> <fixnum>)		=> (<fixnum>)))
   (attributes
    ((_ _)			effect-free result-true)))

 (declare-core-primitive $flonum-sbe
   (unsafe)
   (signatures
    ((<flonum>)			=> (<fixnum>)))
   (attributes
    ((_)				effect-free result-true)))

 (declare-core-primitive $flonum-set!
   (unsafe)
   (signatures
    ((<flonum> <fixnum> <fixnum>)	=> ())))

;;; --------------------------------------------------------------------
;;; trigonometric

 (declare-flonum-unary $flsin unsafe)
 (declare-flonum-unary $flcos unsafe)
 (declare-flonum-unary $fltan unsafe)
 (declare-flonum-unary $flasin unsafe)
 (declare-flonum-unary $flacos unsafe)
 (declare-flonum-unary/binary $flatan unsafe)
 (declare-flonum-binary $flatan2 unsafe)

;;; --------------------------------------------------------------------
;;; hyperbolic

 (declare-flonum-unary $flsinh unsafe)
 (declare-flonum-unary $flcosh unsafe)
 (declare-flonum-unary $fltanh unsafe)
 (declare-flonum-unary $flasinh unsafe)
 (declare-flonum-unary $flacosh unsafe)
 (declare-flonum-unary $flatanh unsafe)

;;; --------------------------------------------------------------------
;;; exponentiation, exponentials, logarithms

 (declare-flonum-unary $flexp unsafe)
 (declare-flonum-unary/binary $fllog unsafe)
 (declare-flonum-binary $fllog2 unsafe)
 (declare-flonum-unary $flexpm1 unsafe)
 (declare-flonum-unary $fllog1p unsafe)
 (declare-flonum-binary $flexpt unsafe)
 (declare-flonum-unary $flsqrt unsafe)
 (declare-flonum-unary $flsquare unsafe)
 (declare-flonum-unary $flcube unsafe)
 (declare-flonum-unary $flcbrt unsafe)
 (declare-flonum-binary $flhypot unsafe)

;;; --------------------------------------------------------------------
;;; comparison

 (declare-flonum-unary/multi-comparison $fl=  unsafe)
 (declare-flonum-unary/multi-comparison $fl!= unsafe)
 (declare-flonum-unary/multi-comparison $fl<  unsafe)
 (declare-flonum-unary/multi-comparison $fl>  unsafe)
 (declare-flonum-unary/multi-comparison $fl<= unsafe)
 (declare-flonum-unary/multi-comparison $fl>= unsafe)

;;; --------------------------------------------------------------------
;;; arithmetics

 (declare-flonum-multi $fl+ unsafe)
 (declare-flonum-multi $fl- unsafe)
 (declare-flonum-multi $fl* unsafe)
 (declare-flonum-multi $fl/ unsafe)

 (declare-flonum-binary $fldiv unsafe)
 (declare-flonum-binary $flmod unsafe)
 (declare-flonum-binary $fldiv0 unsafe)
 (declare-flonum-binary $flmod0 unsafe)

 (declare-flonum-multi $flmax unsafe)
 (declare-flonum-multi $flmin unsafe)

 (declare-core-primitive $fldiv-and-mod
   (unsafe)
   (signatures
    ((<flonum> <flonum>)	=> (<flonum> <flonum>)))
   (attributes
    ((_ _)			effect-free)))

 (declare-core-primitive $fldiv0-and-mod0
   (unsafe)
   (signatures
    ((<flonum> <flonum>)	=> (<flonum> <flonum>)))
   (attributes
    ((_ _)			effect-free)))

 /section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
