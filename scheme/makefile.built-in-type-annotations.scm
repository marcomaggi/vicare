;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: definition of hard-coded built-in type annotations
;;;Date: Mon Mar 21, 2016
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

(define-type-annotation <&who-value>
  (union <false> <symbol> <string>))

(define-type-annotation <non-negative-exact-integer>
  (union <non-negative-fixnum> <positive-bignum>))

(define-type-annotation <exact-rational>
  (union <exact-integer> <ratnum>))

;;; --------------------------------------------------------------------

(define-type-annotation <exact>
  (union <exact-rational> <exact-compnum>))

(define-type-annotation <inexact>
  (union <flonum> <cflonum> <inexact-compnum>))

;;; --------------------------------------------------------------------

(define-type-annotation <positive>
  (union <positive-fixnum> <positive-bignum> <positive-ratnum>
	 <positive-flonum> <positive-zero-flonum>))

(define-type-annotation <negative>
  (union <negative-fixnum> <negative-bignum> <negative-ratnum>
	 <negative-flonum> <negative-zero-flonum>))

;;; --------------------------------------------------------------------

(define-type-annotation <file-descriptor>
  <non-negative-fixnum>)

;; (define-type <syntax-object>
;;   (or <syntactic-identifier>
;;       <boolean>
;;       <char>
;;       <number>
;;       <string>
;;       <bytevector>
;;       <vector-of-syntax-objects>
;;       <list-of-syntax-objects>
;;       ))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
