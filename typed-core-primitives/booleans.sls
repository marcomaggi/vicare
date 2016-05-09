;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for booleans core primitives
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
(library (typed-core-primitives booleans)
  (export typed-core-primitives.booleans)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.booleans)


;;;; booleans, safe procedures

(section

(define-object-unary/multi-comparison-declarer declare-boolean-unary/multi-comparison <boolean>)

(declare-type-predicate boolean?	<boolean>)
(declare-type-predicate true?		<true>)
(declare-type-predicate false?		<false>)

(declare-boolean-unary/multi-comparison boolean=?)
(declare-boolean-unary/multi-comparison boolean!=?)
(declare-boolean-unary/multi-comparison boolean<=?)
(declare-boolean-unary/multi-comparison boolean<?)
(declare-boolean-unary/multi-comparison boolean>=?)
(declare-boolean-unary/multi-comparison boolean>?)

(declare-core-primitive boolean-max
    (safe)
  (signatures
   ((<boolean> . (list-of <boolean>))	=> (<boolean>))))

(declare-core-primitive boolean-min
    (safe)
  (signatures
   ((<boolean> . (list-of <boolean>))	=> (<boolean>))))

/section)


;;;; booleans, unsafe procedures

(section

(declare-boolean-unary/multi-comparison $boolean=	unsafe)
(declare-boolean-unary/multi-comparison $boolean!=	unsafe)
(declare-boolean-unary/multi-comparison $boolean<=	unsafe)
(declare-boolean-unary/multi-comparison $boolean<	unsafe)
(declare-boolean-unary/multi-comparison $boolean>=	unsafe)
(declare-boolean-unary/multi-comparison $boolean>	unsafe)

(declare-core-primitive $boolean-max
    (unsafe)
  (signatures
   ((<boolean> <boolean>)		=> (<boolean>))))

(declare-core-primitive $boolean-min
    (unsafe)
  (signatures
   ((<boolean> <boolean>)		=> (<boolean>))))

/section)


;;;; core syntactic binding descriptors, typed safe OOP core primitives: booleans

(section

(declare-core-primitive <boolean>-constructor
    (safe)
  (signatures
   ((<top>)		=> (<boolean>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
