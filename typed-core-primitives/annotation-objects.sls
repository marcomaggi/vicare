;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for annotation objects core primitives
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
(library (typed-core-primitives annotation-objects)
  (export typed-core-primitives.annotation-objects)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.annotation-objects)


;;;; core syntactic binding descriptors, typed safe core primitives: annotation objects

(section

(declare-core-primitive reader-annotation?
    (safe)
  (signatures
   ((<reader-annotation>)		=> (<boolean>)))
  (attributes
   ((#f)			foldable effect-free result-false)
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive get-annotated-datum
    (safe)
  (signatures
   ((<textual-input-port>)		=> (<reader-annotation>))))

(declare-core-primitive reader-annotation-expression
    (safe)
  (signatures
   ((<reader-annotation>)		=> (_))))

(declare-core-primitive reader-annotation-stripped
    (safe)
  (signatures
   ((<reader-annotation>)		=> (_)))
  (attributes
   ((#f)			foldable effect-free result-false)))

(declare-core-primitive reader-annotation-textual-position
    (safe)
  (signatures
   ((<reader-annotation>)		=> (&source-position))))

(declare-core-primitive reader-annotation-source
    (safe)
  (signatures
   ((<reader-annotation>)		=> (_))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
