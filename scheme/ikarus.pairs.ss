;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

(library (ikarus pairs)
  (export
    cons weak-cons set-car! set-cdr!  car cdr caar cdar cadr cddr
    caaar cdaar cadar cddar caadr cdadr caddr cdddr caaaar cdaaar
    cadaar cddaar caadar cdadar caddar cdddar caaadr cdaadr cadadr
    cddadr caaddr cdaddr cadddr cddddr)
  (import
    (except (ikarus) cons weak-cons set-car! set-cdr! car cdr caar
            cdar cadr cddr caaar cdaar cadar cddar caadr cdadr caddr
            cdddr caaaar cdaaar cadaar cddaar caadar cdadar caddar
            cdddar caaadr cdaadr cadadr cddadr caaddr cdaddr cadddr
            cddddr)
    (rename (only (ikarus)
		  cons)
	    (cons sys:cons))
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (ikarus system $pairs))


;;;; arguments validation

(define-argument-validation (pair who obj)
  (pair? obj)
  (assertion-violation who "argument does not have required pair structure" obj))


(define (cons x y)
  (sys:cons x y))

(define (weak-cons a d)
  (foreign-call "ikrt_weak_cons" a d))

(define (set-car! x y)
  (with-arguments-validation (set-car!)
      ((pair x))
    (unsafe.set-car! x y)))

(define (set-cdr! x y)
  (with-arguments-validation (set-cdr!)
      ((pair x))
    (unsafe.set-cdr! x y)))

(define-syntax cxr
  (syntax-rules ()
    ((_ ?who ?obj $car/$cdr)
     (let ((x ?obj))
       (with-arguments-validation (?who)
	   ((pair x))
	 ($car/$cdr x))))
    ((_ ?who ?obj ?op ... $car/$cdr)
     (let ((x ?obj))
       (with-arguments-validation (?who)
	   ((pair x))
	 (cxr ?who ($car/$cdr x) ?op ...))))))

(define-syntax define-cxr*
  (syntax-rules ()
    ((_ (?name ?operation ...) ...)
     (begin
       (define (?name x)
	 (cxr ?name x ?operation ...))
       ...))))

(define-cxr*
  (car      unsafe.car)
  (cdr      unsafe.cdr)
  (caar     unsafe.car unsafe.car)
  (cdar     unsafe.cdr unsafe.car)
  (cadr     unsafe.car unsafe.cdr)
  (cddr     unsafe.cdr unsafe.cdr)
  (caaar    unsafe.car unsafe.car unsafe.car)
  (cdaar    unsafe.cdr unsafe.car unsafe.car)
  (cadar    unsafe.car unsafe.cdr unsafe.car)
  (cddar    unsafe.cdr unsafe.cdr unsafe.car)
  (caadr    unsafe.car unsafe.car unsafe.cdr)
  (cdadr    unsafe.cdr unsafe.car unsafe.cdr)
  (caddr    unsafe.car unsafe.cdr unsafe.cdr)
  (cdddr    unsafe.cdr unsafe.cdr unsafe.cdr)
  (caaaar   unsafe.car unsafe.car unsafe.car unsafe.car)
  (cdaaar   unsafe.cdr unsafe.car unsafe.car unsafe.car)
  (cadaar   unsafe.car unsafe.cdr unsafe.car unsafe.car)
  (cddaar   unsafe.cdr unsafe.cdr unsafe.car unsafe.car)
  (caadar   unsafe.car unsafe.car unsafe.cdr unsafe.car)
  (cdadar   unsafe.cdr unsafe.car unsafe.cdr unsafe.car)
  (caddar   unsafe.car unsafe.cdr unsafe.cdr unsafe.car)
  (cdddar   unsafe.cdr unsafe.cdr unsafe.cdr unsafe.car)
  (caaadr   unsafe.car unsafe.car unsafe.car unsafe.cdr)
  (cdaadr   unsafe.cdr unsafe.car unsafe.car unsafe.cdr)
  (cadadr   unsafe.car unsafe.cdr unsafe.car unsafe.cdr)
  (cddadr   unsafe.cdr unsafe.cdr unsafe.car unsafe.cdr)
  (caaddr   unsafe.car unsafe.car unsafe.cdr unsafe.cdr)
  (cdaddr   unsafe.cdr unsafe.car unsafe.cdr unsafe.cdr)
  (cadddr   unsafe.car unsafe.cdr unsafe.cdr unsafe.cdr)
  (cddddr   unsafe.cdr unsafe.cdr unsafe.cdr unsafe.cdr)))


(library (ikarus system pairs)
  (export $car $cdr)
  (import (ikarus))
  (define $car car)
  (define $cdr cdr))

;;; end of file
