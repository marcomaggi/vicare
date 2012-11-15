;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Implementation  of  FXREVERSE-BIT-FIELD from:
;;;
;;;  Original patch by Göran Weinholt, posted on the Ikarus bug tracker.
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


(library (ikarus numerics div-and-mod)
  (export div mod div-and-mod div0 mod0 div0-and-mod0)
  (import
    (except (ikarus)
      div mod div-and-mod div0 mod0 div0-and-mod0))

  (define (div-and-mod* n m who)
    (import (ikarus system $fx)
            (only (ikarus system $flonums) $fl=)
            (ikarus flonums))
    (define (int-div-and-mod n m)
      (let ((d0 (quotient n m)))
        (let ((m0 (- n (* d0 m))))
          (if (>= m0 0)
              (values d0 m0)
              (if (>= m 0)
                  (values (- d0 1) (+ m0 m))
                  (values (+ d0 1) (- m0 m)))))))
    (define (rat-div-and-mod n m)
      (let ((x (/ n m)))
        (cond
          ((or (fixnum? x) (bignum? x))
           (values x 0))
          (else
           (let ((n0 (numerator x)) (d0 (denominator x)))
             (let ((q (quotient n0 d0)))
               (let ((r (- n (* q m))))
                 (if (>= r 0)
                     (values q r)
                     (if (> m 0)
                         (values (- q 1) (+ r m))
                         (values (+ q 1) (- r m)))))))))))
    (cond
      ((fixnum? m)
       (cond
         (($fx= m 0)
          (die who "division by 0"))
         ((or (fixnum? n) (bignum? n))
          (int-div-and-mod n m))
         ((flonum? n)
          (fldiv-and-mod n (fixnum->flonum m)))
         ((ratnum? n)
          (rat-div-and-mod n m))
         (else (die who "not a number" n))))
      ((bignum? m)
       (cond
         ((or (fixnum? n) (bignum? n))
          (int-div-and-mod n m))
         ((flonum? n)
          (let ((v ($flonum->exact n)))
            (unless v
              (die who "invalid argument" n))
            (let-values (((a b) (div-and-mod* v m who)))
              (values (inexact a) (inexact b)))))
         ((ratnum? n)
          (rat-div-and-mod n m))
         (else (die who "not a number" n))))
      ((ratnum? m)
       (cond
         ((or (fixnum? n) (bignum? n) (ratnum? n))
          (rat-div-and-mod n m))
         ((flonum? n)
          (let ((v ($flonum->exact n)))
            (unless v
              (die who "invalid argument" n))
            (let-values (((a b) (div-and-mod* v m who)))
              (values (inexact a) (inexact b)))))
         (else (die who "not a number" n))))
      ((flonum? m)
       (cond
         (($fl= m 0.0)
          (die who "division by 0.0"))
         ((flonum? n) (fldiv-and-mod n m))
         ((fixnum? n)
          (fldiv-and-mod (fixnum->flonum n) m))
         ((or (bignum? n) (ratnum? n))
          (let ((v ($flonum->exact m)))
            (unless v
              (die who "invalid argument" m))
            (let-values (((a b) (div-and-mod* n v who)))
              (values (inexact a) (inexact b)))))
         (else (die who "not a number" n))))
      (else (die who "not a number" m))))

  (define (div-and-mod n m)
    (div-and-mod* n m 'div-and-mod))

  (define (div n m)
    (import (ikarus system $fx))
    (cond
      ((and (fixnum? n) (fixnum? m))
       (cond
         ((eq? m 0) (die 'div "division by 0"))
         ((eq? m -1) (- n))
         (else
          (let ((d0 ($fxquotient n m)))
            (if ($fx>= n ($fx* d0 m))
                d0
                (if ($fx>= m 0)
                    ($fx- d0 1)
                    ($fx+ d0 1)))))))
      (else
       (let-values (((a b) (div-and-mod* n m 'div)))
         a))))

  (define (mod n m)
    (import (ikarus system $fx))
    (cond
      ((and (fixnum? n) (fixnum? m))
       (cond
         ((eq? m 0) (die 'mod "division by 0"))
         (else
          (let ((d0 ($fxquotient n m)))
            (let ((m0 ($fx- n ($fx* d0 m))))
              (if ($fx>= m0 0)
                  m0
                  (if ($fx>= m 0)
                      ($fx+ m0 m)
                      ($fx- m0 m))))))))
      (else
       (let-values (((a b) (div-and-mod* n m 'mod)))
         b))))

  (define (div0-and-mod0 x y)
    (let-values (((d m) (div-and-mod* x y 'div0-and-mod0)))
      (if (> y 0)
          (if (< m (/ y 2))
              (values d m)
              (values (+ d 1) (- m y)))
          (if (>= m (/ y -2))
              (values (- d 1) (+ m y))
              (values d m)))))

  (define (div0 x y)
    (let-values (((d m) (div-and-mod* x y 'div0)))
      (if (> y 0)
          (if (< m (/ y 2))
              d
              (+ d 1))
          (if (>= m (/ y -2))
              (- d 1)
              d))))

  (define (mod0 x y)
    (let-values (((d m) (div-and-mod* x y 'mod0)))
      (if (> y 0)
          (if (< m (/ y 2))
              m
              (- m y))
          (if (>= m (/ y -2))
              (+ m y)
              m))))


;;;; done

)

;;; end of file
