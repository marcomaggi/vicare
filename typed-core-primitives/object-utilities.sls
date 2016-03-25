;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for object utilities core primitives
;;Date: Thu Mar 24, 2016
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
(library (typed-core-primitives object-utilities)
  (export typed-core-primitives.object-utilities)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.object-utilities)


;;;; booleans, safe procedures

(section

(declare-type-predicate scheme-type-descriptor?		<scheme-type-descriptor>)

(declare-core-primitive scheme-type-descriptor.name
    (safe)
  (signatures
   ((<scheme-type-descriptor>)		=> (<symbol>))))

(declare-core-primitive scheme-type-descriptor.parent
    (safe)
  (signatures
   ((<scheme-type-descriptor>)		=> ((union <false> <scheme-type-descriptor>)))))

(declare-core-primitive scheme-type-descriptor.uids-list
    (safe)
  (signatures
   ((<scheme-type-descriptor>)		=> ((list-of <symbol>)))))

(declare-core-primitive scheme-type-descriptor.method-retriever
    (safe)
  (signatures
   ((<scheme-type-descriptor>)		=> (<procedure>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 1)
;; End:

