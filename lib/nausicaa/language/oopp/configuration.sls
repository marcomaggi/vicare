;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: configuration options
;;;Date: Thu May 17, 2012
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
(library (nausicaa language oopp configuration)
  (export
    validate-tagged-values?
    enable-satisfactions)
  (import (only (rnrs base (6))
		define))

  (define validate-tagged-values?
    ;;When true: the expansion of syntaxes creating tagged bindings will
    ;;include an  assertion to validate  for the value according  to the
    ;;tag predicate.
    ;;
    #t)

  (define enable-satisfactions
    ;;When  true: the  expansion of  DEFINE-CLASS and  DEFINE-LABEL will
    ;;include the processing of SATISFIES clauses.
    #t)

  )

;;; end of file
