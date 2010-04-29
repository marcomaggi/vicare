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


(library (ikarus predicates)

  (export fixnum? flonum? bignum? ratnum? compnum? cflonum?
          number? complex? real? rational?
          integer? exact? inexact? eof-object? bwp-object? immediate?
          boolean? char? vector? bytevector? string? procedure? null? pair?
          symbol? code? not weak-pair? eq? eqv? boolean=?
          symbol=? finite? infinite? nan? real-valued?
          rational-valued? integer-valued? transcoder?)
  (import 
    (except (ikarus) fixnum? flonum? bignum? ratnum? compnum?  cflonum?
            number? complex? real?
            rational? integer? exact? inexact? eof-object? bwp-object?
            immediate? boolean? char? vector? bytevector? string? procedure?
            null? pair? weak-pair? symbol? code? not eq? eqv? 
            transcoder? boolean=? symbol=?
            finite? infinite? nan? real-valued? rational-valued? 
            integer-valued?)
    (ikarus system $fx)
    (ikarus system $flonums)
    (ikarus system $pairs)
    (ikarus system $chars)
    (ikarus system $strings)
    (ikarus system $vectors)
    (only (ikarus system $compnums) $cflonum-real $cflonum-imag)
    (rename (only (ikarus) fixnum? flonum? bignum? ratnum? compnum? cflonum?
                  eof-object?
                  bwp-object? immediate? boolean? char? vector? string?
                  bytevector? procedure? null? pair? symbol? code? eq?
                  transcoder?)
            (fixnum? sys:fixnum?)
            (flonum? sys:flonum?)
            (bignum? sys:bignum?)
            (ratnum? sys:ratnum?)
            (compnum? sys:compnum?)
            (cflonum? sys:cflonum?)
            (eof-object? sys:eof-object?)
            (bwp-object? sys:bwp-object?)
            (immediate? sys:immediate?)
            (boolean? sys:boolean?)
            (char? sys:char?)
            (vector? sys:vector?)
            (bytevector? sys:bytevector?)
            (string? sys:string?)
            (procedure? sys:procedure?)
            (null? sys:null?)
            (pair? sys:pair?)
            (symbol? sys:symbol?)
            (code? sys:code?)
            (eq? sys:eq?)
            (transcoder? sys:transcoder?)
            ))

  (define fixnum?
    (lambda (x) (sys:fixnum? x)))
  
  (define bignum? 
    (lambda (x) (sys:bignum? x)))
  
  (define ratnum? 
    (lambda (x) (sys:ratnum? x)))
  
  (define flonum? 
    (lambda (x) (sys:flonum? x)))
  
  (define compnum? 
    (lambda (x) (sys:compnum? x)))

  (define cflonum? 
    (lambda (x) (sys:cflonum? x)))

  (define number?
    (lambda (x)
      (or (sys:fixnum? x)
          (sys:bignum? x)
          (sys:flonum? x)
          (sys:ratnum? x)
          (sys:compnum? x)
          (sys:cflonum? x))))
  
  (define complex?
    (lambda (x) (number? x)))
  
  (define real?
    (lambda (x) 
      (or (sys:fixnum? x)
          (sys:bignum? x)
          (sys:flonum? x)
          (sys:ratnum? x))))

  (define real-valued?
    (lambda (x) 
      (cond
        [(real? x) #t]
        [(cflonum? x) (fl=? ($cflonum-imag x) 0.0)]
        [else #f])))

  (define rational?
    (lambda (x) 
      (cond
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:ratnum? x) #t]
        [(sys:flonum? x) ($flonum-rational? x)]
        [else #f])))

  (define rational-valued? 
    (lambda (x) 
      (cond
        [(rational? x) #t]
        [(cflonum? x) 
         (and (fl=? ($cflonum-imag x) 0.0) 
              ($flonum-rational? ($cflonum-real x)))]
        [else #f])))

  (define integer? 
    (lambda (x) 
      (cond
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:ratnum? x) #f]
        [(sys:flonum? x) ($flonum-integer? x)]
        [else #f])))

  (define integer-valued? 
    (lambda (x) 
      (cond
        [(integer? x) #t]
        [(cflonum? x)
         (and (fl=? ($cflonum-imag x) 0.0) 
              ($flonum-integer? ($cflonum-real x)))]
        [else #f])))


  (define exact?
    (lambda (x) 
      (cond
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:ratnum? x) #t]
        [(sys:flonum? x) #f]
        [(sys:compnum? x) #t]
        [(sys:cflonum? x) #f]
        [else 
         (die 'exact? "not a number" x)])))
  

  (define inexact?
    (lambda (x) 
      (cond
        [(sys:flonum? x) #t]
        [(sys:fixnum? x) #f]
        [(sys:bignum? x) #f]
        [(sys:ratnum? x) #f]
        [(sys:compnum? x) #f]
        [(sys:cflonum? x) #t]
        [else 
         (die 'inexact? "not a number" x)])))

  (define finite?
    (lambda (x)
      (cond
        [(sys:flonum? x) (flfinite? x)]
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:ratnum? x) #t]
        [(sys:compnum? x) #t]
        [(sys:cflonum? x) 
         (and 
           (flfinite? ($cflonum-real x)) 
           (flfinite? ($cflonum-imag x)))]
        [else 
         (die 'finite? "not a number" x)])))

  (define infinite?
    (lambda (x)
      (cond
        [(sys:flonum? x) (flinfinite? x)]
        [(sys:fixnum? x) #f]
        [(sys:bignum? x) #f]
        [(sys:ratnum? x) #f]
        [(sys:compnum? x) #f]
        [(sys:cflonum? x) 
         (or 
           (flinfinite? ($cflonum-real x)) 
           (flinfinite? ($cflonum-imag x)))]
        [else 
         (die 'infinite? "not a number" x)])))

  (define nan?
    (lambda (x)
      (cond
        [(sys:flonum? x) (flnan? x)]
        [(sys:fixnum? x) #f]
        [(sys:bignum? x) #f]
        [(sys:ratnum? x) #f]
        [(sys:compnum? x) #f]
        [(sys:cflonum? x) 
         (or 
           (nan? ($cflonum-real x)) 
           (nan? ($cflonum-imag x)))]
        [else 
         (die 'nan? "not a number" x)])))




  (define eof-object? (lambda (x) (sys:eof-object? x)))
  (define bwp-object? (lambda (x) (sys:bwp-object? x)))
  (define transcoder? (lambda (x) (sys:transcoder? x)))
  (define immediate? (lambda (x) (sys:immediate? x)))
  (define boolean? (lambda (x) (sys:boolean? x)))
  (define char? (lambda (x) (sys:char? x)))
  (define vector? (lambda (x) (sys:vector? x)))
  (define bytevector? (lambda (x) (sys:bytevector? x)))
  (define string? (lambda (x) (sys:string? x)))
  (define procedure? (lambda (x) (sys:procedure? x)))
  (define null? (lambda (x) (sys:null? x)))
  (define pair? (lambda (x) (sys:pair? x)))
  (define symbol? (lambda (x) (sys:symbol? x)))
  (define code? (lambda (x) (sys:code? x)))


  (define weak-pair?
    (lambda (x)
      (and (pair? x)
           (foreign-call "ikrt_is_weak_pair" x))))

  (define not (lambda (x) (if x #f #t)))

  (define eq? (lambda (x y) (sys:eq? x y)))

  (define eqv?
    (lambda (x y)
      (import (ikarus))
      (cond
        [(eq? x y) #t]
        [(flonum? x) 
         (and (flonum? y)
              (if ($fl< x y)
                  #f
                  (if ($fl> x y)
                      #f
                      (if ($fl= x 0.0)
                          ($fl= ($fl/ 1.0 x) ($fl/ 1.0 y))
                          #t))))]
        [(bignum? x) (and (bignum? y) (= x y))]
        [(ratnum? x) (and (ratnum? y) (= x y))]
        [(compnum? x)
         (and (compnum? y) 
              (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))]
        [(cflonum? x)
         (and (cflonum? y)
              (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))]
        [else #f])))

  
  (define-syntax define-pred
    (syntax-rules ()
      [(_ name pred? msg)
       (begin
         (define (err x) (die 'name msg x))
         (define (g rest)
           (if (sys:pair? rest)
               (let ([a (car rest)])
                 (if (pred? a)
                     (g (cdr rest))
                     (err a)))
               #f))
         (define (f x rest)
           (if (sys:pair? rest)
               (let ([a (car rest)])
                 (if (sys:eq? x a)
                     (f x (cdr rest))
                     (if (pred? a)
                         (g (cdr rest))
                         (err a))))
               #t))
         (define name
           (case-lambda
             [(x y) 
              (if (pred? x)
                  (if (sys:eq? x y) 
                      #t
                      (if (pred? y) 
                          #f
                          (err y)))
                  (err x))]
             [(x y z . rest)
              (if (pred? x)
                  (if (sys:eq? x y)
                      (if (sys:eq? x z)
                          (f x rest)
                          (if (pred? z) #f (err z)))
                      (if (pred? y) #f (err y)))
                  (err x))])))]))
  (define-pred symbol=? sys:symbol? "not a symbol")
  (define-pred boolean=? sys:boolean? "not a boolean")



  )
