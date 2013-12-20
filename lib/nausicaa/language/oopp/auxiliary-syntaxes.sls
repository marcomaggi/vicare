;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: internal auxiliary syntaxes definitions
;;;Date: Sun May  6, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa language oopp auxiliary-syntaxes)
  (export
    :flat-oopp-syntax
    :define
    :let
    :bind-and-call
    :dispatch
    :accessor
    :mutator
    :setter
    :getter
    :make
    :make-from-fields
    :is-a?
    :assert-type-and-return
    :assert-procedure-argument
    :assert-expression-return-value
    :insert-parent-clause
    :insert-constructor-fields
    :super-constructor-descriptor
    :append-unique-id
    :list-of-unique-ids
    :predicate-function
    :accessor-function
    :mutator-function
    :process-shadowed-identifier
    :insert-mixin-clauses)
  (import (only (vicare)
		define-auxiliary-syntaxes))
  (define-auxiliary-syntaxes
    :flat-oopp-syntax
    :define
    :let
    :bind-and-call
    :make
    :make-from-fields
    :is-a?
    :dispatch
    :accessor
    :mutator
    :setter
    :getter
    :assert-type-and-return
    :assert-procedure-argument
    :assert-expression-return-value
    :insert-parent-clause
    :insert-constructor-fields
    :super-constructor-descriptor
    :append-unique-id
    :list-of-unique-ids
    :predicate-function
    :accessor-function
    :mutator-function
    :process-shadowed-identifier
    :insert-mixin-clauses))

;;; end of file
