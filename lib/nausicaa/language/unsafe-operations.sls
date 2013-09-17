;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: unsafe opertions
;;;Date: Wed May  2, 2012
;;;
;;;Abstract
;;;
;;;	This library is meant to export the unsafe version of the common
;;;	R6RS  functions acting  on  Scheme objects.   If the  underlying
;;;	Scheme implementation  does not allow access  to such functions:
;;;	the bindings from (rnrs (6)) are exported.
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
(library (nausicaa language unsafe-operations)
  (export
    ;; lists
    car			cdr
    caar		cadr
    cdar		cddr
    set-car!		set-cdr!

    ;; vectors
    make-vector		vector-length
    vector-ref		vector-set!

    ;; strings
    make-string		string-length
    string-ref		string-set!
    )
  (import (except (rnrs)
		  make-vector
		  make-string)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (prefix (only (rnrs)
		  make-vector
		  make-string)
	    rnrs.))


;;;; special operations

(define-syntax make-vector
  (syntax-rules ()
    ((_ ?length)
     (rnrs.make-vector ?length))))

(define-syntax make-string
  (syntax-rules ()
    ((_ ?length)
     (rnrs.make-string ?length))))


;;;; done

)

;;; end of file
