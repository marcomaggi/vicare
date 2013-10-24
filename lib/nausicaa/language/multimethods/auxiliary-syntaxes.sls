;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: auxiliary syntaxes for generics macros
;;;Date: Thu Apr  7, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language multimethods auxiliary-syntaxes)
  (export
    :number-of-arguments

    :primary-methods-alist
    :before-methods-alist
    :after-methods-alist
    :around-methods-alist

    :primary-cache
    :before-cache
    :after-cache
    :around-cache

    :primary-method-add
    :before-method-add
    :after-method-add
    :around-method-add)
  (import (only (vicare)
		define-auxiliary-syntaxes))
  (define-auxiliary-syntaxes
    :number-of-arguments

    :primary-methods-alist
    :before-methods-alist
    :after-methods-alist
    :around-methods-alist

    :primary-cache
    :before-cache
    :after-cache
    :around-cache

    :primary-method-add
    :before-method-add
    :after-method-add
    :around-method-add))

;;; end of file
