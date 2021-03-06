;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: auxiliary library for testing compiler internals
;;;Date: Mon Aug 25, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (libtest compiler-internals)
  (export a-func a-thunk a-const)
  (import (rnrs (6)))
  (define (a-func a b)
    (+ a b))
  (define (a-thunk)
    "ciao")
  (define a-const 123)
  #| end of library |# )

;;; end of file
