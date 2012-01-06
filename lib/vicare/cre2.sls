;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: binding to CRE2
;;;Date: Fri Jan  6, 2012
;;;
;;;Abstract
;;;
;;;	Built in  binding to the CRE2  library: a C wrapper  for the RE2
;;;	regular expressions library from Google.
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare cre2)
  (export
    cre2-enabled?)
  (import (vicare)
    (vicare syntactic-extensions))


;;;; C API

(define-inline (capi.cre2-enabled?)
  (foreign-call "ikrt_cre2_enabled"))


;;;; version functions

(define (cre2-enabled?)
  (capi.cre2-enabled?))


;;;; done

)

;;; end of file
