;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: byte as ASCII characters
;;;Date: Tue Jun 29, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare language-extensions ascii-chars)
  (export
    fixnum-in-ascii-range?
    ascii-cased?		$ascii-cased?
    ascii-upper-case?		$ascii-upper-case?
    ascii-lower-case?		$ascii-lower-case?
    ascii-numeric?		$ascii-numeric?
    (rename (ascii-upper-case?	ascii-title-case?)
	    (ascii-cased?	ascii-alphabetic?)
	    ($ascii-upper-case?	$ascii-title-case?)
	    ($ascii-cased?	$ascii-alphabetic?))

    ascii-upcase		$ascii-upcase
    ascii-downcase		$ascii-downcase
    (rename (ascii-upcase	ascii-titlecase)
	    ($ascii-upcase	$ascii-titlecase)))
  (import (vicare)
    (vicare arguments validation)
    (vicare system $fx))


;;;; helpers

(define-argument-validation (fixnum-in-ascii-range who obj)
  (fixnum-in-ascii-range? obj)
  (assertion-violation who "expected fixnum in ASCII range as argument" obj))

(define-constant FIXNUM-a (char->integer #\a))
(define-constant FIXNUM-z (char->integer #\z))
(define-constant FIXNUM-A (char->integer #\A))
(define-constant FIXNUM-Z (char->integer #\Z))
(define-constant FIXNUM-0 (char->integer #\0))
(define-constant FIXNUM-9 (char->integer #\9))


;;;; utilities

(define (fixnum-in-ascii-range? obj)
  (and (fixnum? obj)
       (or ($ascii-upper-case? obj)
	   ($ascii-lower-case? obj)
	   ($ascii-numeric?    obj))))

;;; --------------------------------------------------------------------

(define (ascii-upper-case? fx)
  (and (fixnum? fx)
       ($ascii-upper-case? fx)))

(define ($ascii-upper-case? fx)
  (and ($fx>= fx FIXNUM-A)
       ($fx<= fx FIXNUM-Z)))

;;; --------------------------------------------------------------------

(define (ascii-lower-case? fx)
  (and (fixnum? fx)
       ($ascii-lower-case? fx)))

(define ($ascii-lower-case? fx)
  (and ($fx>= fx FIXNUM-a)
       ($fx<= fx FIXNUM-z)))

;;; --------------------------------------------------------------------

(define (ascii-cased? fx)
  (and (fixnum? fx)
       ($ascii-cased? fx)))

(define ($ascii-cased? fx)
  (or ($ascii-upper-case? fx)
      ($ascii-lower-case? fx)))

;;; --------------------------------------------------------------------

(define (ascii-numeric? fx)
  (and (fixnum? fx)
       ($ascii-numeric? fx)))

(define ($ascii-numeric? fx)
  (and ($fx>= fx FIXNUM-0)
       ($fx<= fx FIXNUM-9)))

;;; --------------------------------------------------------------------

(define (ascii-upcase fx)
  (define who 'ascii-upcase)
  (with-arguments-validation (who)
      ((fixnum-in-ascii-range	fx))
    ($ascii-upcase fx)))

(define ($ascii-upcase fx)
  (if ($ascii-lower-case? fx)
      ($fx- fx 32)
    fx))

;;; --------------------------------------------------------------------

(define (ascii-downcase fx)
  (define who 'ascii-downcase)
  (with-arguments-validation (who)
      ((fixnum-in-ascii-range	fx))
    ($ascii-downcase fx)))

(define ($ascii-downcase fx)
  (if ($ascii-upper-case? fx)
      ($fx+ 32 fx)
    fx))


;;;; done

)

;;; end of file
