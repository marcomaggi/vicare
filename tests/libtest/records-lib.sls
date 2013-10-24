;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: R6RS record type definitions for testing purposes
;;;Date: Wed Oct  2, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (libtest records-lib)
  (export
    <alpha>	make-<alpha>
    <beta>	make-<beta>
    <gamma>	make-<gamma>
    )
  (import (vicare))


(define-record-type <alpha>
  (fields (mutable   one)
	  (immutable two)))

(define-record-type <beta>
  (fields (mutable   one the-beta-one the-beta-one-set!)
	  (immutable two the-beta-two)))

(define-record-type <gamma>
  (parent <beta>)
  (fields (mutable   three the-gamma-one the-gamma-one-set!)
	  (immutable four  the-gamma-two)))


;;;; done

)

;;; end of file
