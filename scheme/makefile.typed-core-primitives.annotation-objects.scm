;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for annotation objects core primitives
;;Date: Tue Dec 22, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; core syntactic binding descriptors, typed safe core primitives: annotation objects

(section

(declare-core-primitive annotation?
    (safe)
  (signatures
   ((<annotation>)		=> (<boolean>)))
  (attributes
   ((#f)			foldable effect-free result-false)
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive get-annotated-datum
    (safe)
  (signatures
   ((<textual-input-port>)	=> (_))))

(declare-core-primitive annotation-expression
    (safe)
  (signatures
   ((<annotation>)		=> (_))))

(declare-core-primitive annotation-stripped
    (safe)
  (signatures
   ((<annotation>)		=> (_)))
  (attributes
   ((#f)			foldable effect-free result-false)))

(declare-core-primitive annotation-textual-position
    (safe)
  (signatures
   ((<annotation>)		=> (_))))

(declare-core-primitive annotation-source
    (safe)
  (signatures
   ((<annotation>)		=> (_))))

/section)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 1)
;; End:
