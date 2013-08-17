;;;
;;;Part of: Vicare/Scheme
;;;Contents: sentinel library
;;;Date: Tue Jul  7, 2009
;;;
;;;Abstract
;;;
;;;	This library was originally part of Nausicaa.
;;;
;;;Copyright (c) 2009, 2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare language-extensions sentinels)
  (export sentinel sentinel? make-sentinel
	  undefined undefined?
	  unspecified unspecified?)
  (import (rnrs))
  (define-record-type (:sentinel make-sentinel sentinel?)
    (opaque #t)
    (sealed #t)
    (nongenerative nausicaa:sentinel::sentinel))
  (define sentinel    (make-sentinel))
  (define undefined   (make-sentinel))
  (define unspecified (make-sentinel))
  (define (undefined? obj)
    (eq? obj undefined))
  (define (unspecified? obj)
    (eq? obj unspecified)))

;;; end of file
