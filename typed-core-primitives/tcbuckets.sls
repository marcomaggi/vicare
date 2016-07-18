;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for tcbuckets core primitives
;;Date: Mon Jul 18, 2016
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
(library (typed-core-primitives tcbuckets)
  (export typed-core-primitives.tcbuckets)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.tcbuckets)


;;;; tcbuckets, safe primitives

(section

(declare-type-predicate		tcbucket?		<tcbucket>)

/section)


;;;; tcbuckets, unsafe primitives

(section

(declare-core-primitive $make-tcbucket
    (unsafe)
  (signatures
   ((<pair> <top> <top> (or <tcbucket> <non-negative-fixnum>))
    => (<tcbucket>))))

;;; --------------------------------------------------------------------

(declare-core-primitive $tcbucket-tconc
    (unsafe)
  (signatures
   ((<tcbucket>)		=> (<pair>))))

(declare-core-primitive $tcbucket-key
    (unsafe)
  (signatures
   ((<tcbucket>)		=> (<top>))))

(declare-core-primitive $tcbucket-val
    (unsafe)
  (signatures
   ((<tcbucket>)		=> (<top>))))

(declare-core-primitive $tcbucket-next
    (unsafe)
  (signatures
   ((<tcbucket>)		=> ((or <tcbucket> <non-negative-fixnum>)))))

;;; --------------------------------------------------------------------

(declare-core-primitive $set-tcbucket-tconc!
    (unsafe)
  (signatures
   ((<tcbucket> <pair>)		=> (<void>))))

(declare-core-primitive $set-tcbucket-key!
    (unsafe)
  (signatures
   ((<tcbucket> <top>)		=> (<void>))))

(declare-core-primitive $set-tcbucket-val!
    (unsafe)
  (signatures
   ((<tcbucket> <top>)		=> (<void>))))

(declare-core-primitive $set-tcbucket-next!
    (unsafe)
  (signatures
   ((<tcbucket> (or <tcbucket> <non-negative-fixnum>))		=> (<void>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
