;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for compnums core primitives
;;Date: Tue Dec 24, 2015
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


;;;; compnums, safe functions

(section

(declare-type-predicate compnum? <compnum>)

/section)


;;;; compnums, unsafe functions

(section

;;; constructors

(declare-core-primitive $make-compnum
    (unsafe)
  (signatures
   ((<rational> <rational>)		=> (<compnum>))
   ((<rational> <flonum>)		=> (<compnum>))
   ((<flonum> <rational>)		=> (<compnum>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive $compnum-real
    (unsafe)
  (signatures
   ((<compnum>)			=> (<real>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $compnum-imag
    (unsafe)
  (signatures
   ((<compnum>)			=> (<real>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $compnum->cflonum
    (unsafe)
  (signatures
   ((<compnum>)			=> (<cflonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

/section)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 1)
;; End:
