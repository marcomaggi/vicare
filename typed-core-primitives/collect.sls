;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for GC-related core primitives
;;Date: Mon Mar 28, 2016
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
(library (typed-core-primitives collect)
  (export typed-core-primitives.collect)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.collect)


;;;; calling procedures and returning

(section

(declare-core-primitive collect
    (safe)
  (signatures
   (()						=> (<void>))
   (((or <symbol> <non-negative-fixnum>))	=> (<void>))))

(declare-core-primitive automatic-collect
    (safe)
  (signatures
   (()						=> (<void>))
   (((or <symbol> <non-negative-fixnum>))	=> (<void>))))

(declare-parameter automatic-garbage-collection)

(declare-parameter post-gc-hooks	(list-of <procedure>))

/section)


;;;; collection avoidance

(section

(declare-core-primitive register-to-avoid-collecting
    (safe)
  (signatures
   ((<top>)				=> (<pointer>))))

(declare-core-primitive forget-to-avoid-collecting
    (safe)
  (signatures
   ((<pointer>)				=> (<top>))))

(declare-core-primitive replace-to-avoid-collecting
    (safe)
  (signatures
   ((<pointer> <top>)			=> (<top>))))

(declare-core-primitive retrieve-to-avoid-collecting
    (safe)
  (signatures
   ((<pointer>)				=> (<top>))))

(declare-core-primitive collection-avoidance-list
    (safe)
  (signatures
   (()					=> <list>)))

(declare-core-primitive purge-collection-avoidance-list
    (safe)
  (signatures
   (()					=> (<void>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
