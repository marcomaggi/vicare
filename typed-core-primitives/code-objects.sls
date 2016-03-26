;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for code objects core primitives
;;Date: Tue Dec 24, 2015
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
(library (typed-core-primitives code-objects)
  (export typed-core-primitives.code-objects)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.code-objects)


;;;; codes and closures, unsafe primitives

(section

(declare-core-primitive $closure-code
    (safe)
  (signatures
   ((<procedure>)	=> (<code>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $cpref
    (safe)
  (signatures
   ((<procedure> <non-negative-fixnum>)		=> (<top>)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive $make-annotated-procedure
    (safe)
  (signatures
   ((<top> <procedure>)	=> (<procedure>)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive $annotated-procedure-annotation
    (safe)
  (signatures
   ((<procedure>)	=> (<top>)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive $code->closure
    (safe)
  (signatures
   ((<code>)		=> (<procedure>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $code-reloc-vector
    (safe)
  (signatures
   ((<code>)		=> (<top>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $code-freevars
    (safe)
  (signatures
   ((<code>)		=> (<non-negative-fixnum>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $code-size
    (safe)
  (signatures
   ((<code>)		=> (<non-negative-exact-integer>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $code-annotation
    (safe)
  (signatures
   ((<code>)		=> (<top>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive $set-code-annotation!
    (safe)
  (signatures
   ((<code> <top>)	=> (<void>)))
  (attributes
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $code-ref
    (safe)
  (signatures
   ((<code> <non-negative-fixnum>)	=> (<non-negative-fixnum>)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive $code-set!
    (safe)
  (signatures
   ((<code> <non-negative-exact-integer> <non-negative-fixnum>)	=> (<void>)))
  (attributes
   ((_ _ _)		result-true)))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
