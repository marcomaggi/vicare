;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: definition of unique objects
;;;Date: Thu Apr 14, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus unique-objects)
  (export
    would-block-object	would-block-object?
    sentinel		sentinel?
    ;;This is needed in "ikarus.io.sls".
    WOULD-BLOCK-OBJECT
    ;;This is needed in "ikarus.hash-tables.sls".
    SENTINEL)
  (import (except (vicare)
		  would-block-object	would-block-object?
		  sentinel		sentinel?)
    (only (vicare system $structs)
	  $struct)
    (vicare system structs))


(define-struct unique-object
  ())

(define-constant WOULD-BLOCK-OBJECT
  ($struct (type-descriptor unique-object)))

(define-constant SENTINEL
  ($struct (type-descriptor unique-object)))

(define (would-block-object)
  WOULD-BLOCK-OBJECT)

(define (would-block-object? obj)
  (eq? obj WOULD-BLOCK-OBJECT))

(define (sentinel)
  SENTINEL)

(define (sentinel? obj)
  (eq? obj SENTINEL))


;;;; done

#| end of library |# )

;;; end of file
