;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for keywords core primitives
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
(library (typed-core-primitives keywords)
  (export typed-core-primitives.keywords)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.keywords)


;;;; core syntactic binding descriptors, typed core primitives: safe keyword primitives

(section

(declare-type-predicate keyword? <keyword>)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive symbol->keyword
    (safe)
  (signatures
   ((<symbol>)			=> (<keyword>)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-keyword-binary-comparison keyword=?)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive keyword->symbol
    (safe)
  (signatures
   ((<keyword>)			=> (<symbol>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive keyword->string
    (safe)
  (signatures
   ((<keyword>)			=> (<string>)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-hash-function keyword-hash <keyword> safe)

/section)


;;;; keywords, unsafe functions

(section

;;; constructors

(declare-core-primitive $symbol->keyword
    (unsafe)
  (signatures
   ((<symbol>)			=> (<keyword>)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-keyword-binary-comparison $keyword=? unsafe)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $keyword->symbol
    (unsafe)
  (signatures
   ((<keyword>)			=> (<symbol>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive $keyword->string
    (unsafe)
  (signatures
   ((<keyword>)			=> (<string>)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-hash-function $keyword-hash <keyword> unsafe)

/section)


;;;; done

#| end of define |# )

#| end of library |# )


;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
