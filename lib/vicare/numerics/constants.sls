;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: numerics constants
;;;Date: Mon Dec 17, 2012
;;;
;;;Abstract
;;;
;;;
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare numerics constants)
  (export
    greek-pi  greek-pi/2  greek-pi/3  greek-pi/4  greek-pi/6  greek-pi*2  greek-pi*2/3  greek-pi*3/4
    +greek-pi +greek-pi/2 +greek-pi/3 +greek-pi/4 +greek-pi/6 +greek-pi*2 +greek-pi*2/3 +greek-pi*3/4
    -greek-pi -greek-pi/2 -greek-pi/3 -greek-pi/4 -greek-pi/6 -greek-pi*2 -greek-pi*2/3 -greek-pi*3/4
    )
  (import (vicare))


;;;; helpers

;; (define-syntax define-inline-constant
;;   (syntax-rules ()
;;     ((_ ?name ?value)
;;      (define-syntax ?name (identifier-syntax ?value)))))


;;;; pi

#;(define greek-pi (acos -1))

;;From Wikipedia.
(define greek-pi	3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679)

(define greek-pi/2	(/ greek-pi 2.0))
(define greek-pi/3	(/ greek-pi 3.0))
(define greek-pi/4	(/ greek-pi 4.0))
(define greek-pi/6	(/ greek-pi 6.0))

(define greek-pi*2	(* greek-pi 2.0))
(define greek-pi*2/3	(* greek-pi (/ 2.0 3.0)))
(define greek-pi*3/4	(* greek-pi (/ 3.0 4.0)))

;;; --------------------------------------------------------------------

(define +greek-pi	greek-pi)

(define +greek-pi/2	greek-pi/2)
(define +greek-pi/3	greek-pi/3)
(define +greek-pi/4	greek-pi/4)
(define +greek-pi/6	greek-pi/6)

(define +greek-pi*2	greek-pi*2)
(define +greek-pi*2/3	greek-pi*2/3)
(define +greek-pi*3/4	greek-pi*3/4)

;;; --------------------------------------------------------------------

(define -greek-pi	(- greek-pi))

(define -greek-pi/2	(- greek-pi/2))
(define -greek-pi/3	(- greek-pi/3))
(define -greek-pi/4	(- greek-pi/4))
(define -greek-pi/6	(- greek-pi/6))

(define -greek-pi*2	(- greek-pi*2))
(define -greek-pi*2/3	(- greek-pi*2/3))
(define -greek-pi*3/4	(- greek-pi*3/4))


;;;; done

)

;;; end of file
