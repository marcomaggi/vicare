;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: foldable core primitive variants
;;;Date: Fri Dec  5, 2014
;;;
;;;Abstract
;;;
;;;	There are  core primitive functions that,  as defined by R6RS,  must return a
;;;	newly allocated  Scheme object  at every  application; examples:  LIST, CONS,
;;;	VECTOR,  STRING.  Such  primitives cannot  be classified  as foldable  by the
;;;	source optimiser.
;;;
;;;       This library defines aliases for  such primitives that the source optimiser
;;;     can classify as foldable; their use allows more constant Scheme objects to be
;;;     computed at compile-time, rather than created at run-time.
;;;
;;;	  This foldable primitives are, mostly, for internal use in QUASIQUOTE forms,
;;;	which are may generate constant objects as defined by R6RS.
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (ikarus.foldable)
  (export
    foldable-cons
    foldable-list
    foldable-vector
    foldable-string
    foldable-list->vector
    foldable-append)
  (import (except (vicare)
		  foldable-cons
		  foldable-list
		  foldable-vector
		  foldable-string
		  foldable-list->vector
		  foldable-append))
  (define foldable-cons		cons)
  (define foldable-list		list)
  (define foldable-vector	vector)
  (define foldable-string	string)
  (define foldable-list->vector	list->vector)
  (define foldable-append	append)
  #| end of library |# )

;;; end of file
;; Local Variables:
;; End:
