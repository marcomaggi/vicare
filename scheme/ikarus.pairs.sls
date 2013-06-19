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
    (vicare language-extensions syntaxes)
    (vicare arguments validation)
    (ikarus system $pairs))


(define (cons x y)
  (sys:cons x y))

(define (weak-cons a d)
  (foreign-call "ikrt_weak_cons" a d))

(define (set-car! x y)
  (define who 'set-car!)
  (with-arguments-validation (who)
      ((pair x))
    ($set-car! x y)))

(define (set-cdr! x y)
  (define who 'set-cdr!)
  (with-arguments-validation (who)
      ((pair x))
    ($set-cdr! x y)))

(define-syntax cxr
  (syntax-rules ()
    ((_ ?who ?obj $car/$cdr)
     (let ((x   ?obj)
	   (who '?who))
       (with-arguments-validation (who)
	   ((pair x))
	 ($car/$cdr x))))
    ((_ ?who ?obj ?op ... $car/$cdr)
     (let ((x   ?obj)
	   (who '?who))
       (with-arguments-validation (who)
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
  (car      $car)
  (cdr      $cdr)
  (caar     $car $car)
  (cdar     $cdr $car)
  (cadr     $car $cdr)
  (cddr     $cdr $cdr)
  (caaar    $car $car $car)
  (cdaar    $cdr $car $car)
  (cadar    $car $cdr $car)
  (cddar    $cdr $cdr $car)
  (caadr    $car $car $cdr)
  (cdadr    $cdr $car $cdr)
  (caddr    $car $cdr $cdr)
  (cdddr    $cdr $cdr $cdr)
  (caaaar   $car $car $car $car)
  (cdaaar   $cdr $car $car $car)
  (cadaar   $car $cdr $car $car)
  (cddaar   $cdr $cdr $car $car)
  (caadar   $car $car $cdr $car)
  (cdadar   $cdr $car $cdr $car)
  (caddar   $car $cdr $cdr $car)
  (cdddar   $cdr $cdr $cdr $car)
  (caaadr   $car $car $car $cdr)
  (cdaadr   $cdr $car $car $cdr)
  (cadadr   $car $cdr $car $cdr)
  (cddadr   $cdr $cdr $car $cdr)
  (caaddr   $car $car $cdr $cdr)
  (cdaddr   $cdr $car $cdr $cdr)
  (cadddr   $car $cdr $cdr $cdr)
  (cddddr   $cdr $cdr $cdr $cdr))


;;;; end of library (ikarus pairs)

)


(library (ikarus system pairs)
  (export $car $cdr)
  (import (ikarus))
  (define $car car)
  (define $cdr cdr))

;;; end of file
