;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



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
    (rename (only (ikarus) cons) (cons sys:cons))
    (ikarus system $pairs))

  (define cons (lambda (x y) (sys:cons x y)))

  (define weak-cons
    (lambda (a d)
      (foreign-call "ikrt_weak_cons" a d)))

  (define set-car!
    (lambda (x y) 
      (unless (pair? x)
        (die 'set-car! "not a pair" x))
      ($set-car! x y)))
  
  (define set-cdr! 
    (lambda (x y)
      (unless (pair? x)
        (die 'set-cdr! "not a pair" x))
      ($set-cdr! x y)))

  (define-syntax cxr
    (syntax-rules ()
      [(_ err $car/$cdr) 
       (lambda (x) 
         (if (pair? x) ($car/$cdr x) err))]
      [(_ err rest ... $car/$cdr)
       (lambda (x)
         (if (pair? x)
             ((cxr err rest ...) ($car/$cdr x))
             err))]))

  (define-syntax define-cxr*
    (syntax-rules ()
      [(_ [name* ops** ...] ...)
       (begin
         (define name*
           (lambda (x) 
             ((cxr (die 'name* 
                     "argument does not have required pair structure" x) 
                   ops** ...)
              x)))
         ...)]))

  (define-cxr*
    [car      $car]
    [cdr      $cdr]
    [caar     $car $car]
    [cdar     $cdr $car]
    [cadr     $car $cdr]
    [cddr     $cdr $cdr]
    [caaar    $car $car $car]
    [cdaar    $cdr $car $car]
    [cadar    $car $cdr $car]
    [cddar    $cdr $cdr $car]
    [caadr    $car $car $cdr]
    [cdadr    $cdr $car $cdr]
    [caddr    $car $cdr $cdr]
    [cdddr    $cdr $cdr $cdr]
    [caaaar   $car $car $car $car]
    [cdaaar   $cdr $car $car $car]
    [cadaar   $car $cdr $car $car]
    [cddaar   $cdr $cdr $car $car]
    [caadar   $car $car $cdr $car]
    [cdadar   $cdr $car $cdr $car]
    [caddar   $car $cdr $cdr $car]
    [cdddar   $cdr $cdr $cdr $car]
    [caaadr   $car $car $car $cdr]
    [cdaadr   $cdr $car $car $cdr]
    [cadadr   $car $cdr $car $cdr]
    [cddadr   $cdr $cdr $car $cdr]
    [caaddr   $car $car $cdr $cdr]
    [cdaddr   $cdr $car $cdr $cdr]
    [cadddr   $car $cdr $cdr $cdr]
    [cddddr   $cdr $cdr $cdr $cdr]))


(library (ikarus system pairs)
  (export $car $cdr)
  (import (ikarus))
  (define $car car)
  (define $cdr cdr))

