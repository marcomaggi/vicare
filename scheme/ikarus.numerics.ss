;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;; Implementation  of FXREVERSE-BIT-FIELD  from the  original  patch by
;;; Göran Weinholt, posted on the Ikarus bug tracker.
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

(library (ikarus flonums)
  (export $flonum->exact flonum-parts
          inexact->exact exact $flonum-rational? $flonum-integer? $flzero?
          $flnegative? flpositive? flabs fixnum->flonum
          flsin flcos fltan flasin flacos flatan fleven? flodd?
          flfloor flceiling flnumerator fldenominator flexp fllog
          flinteger? flonum-bytes flnan? flfinite? flinfinite?
          flexpt $flround flround)
  (import
    (ikarus system $bytevectors)
    (ikarus system $fx)
    (only (ikarus system $flonums) $fl>= $flonum-sbe)
    (ikarus system $bignums)
    (except (ikarus system $flonums) $flonum-rational?
            $flonum-integer? $flround)
    (except (ikarus) inexact->exact exact flpositive? flabs fixnum->flonum
            flsin flcos fltan flasin flacos flatan fleven? flodd?
            flfloor flceiling flnumerator fldenominator flexp fllog
            flexpt flinteger? flonum-parts flonum-bytes flnan? flfinite?
            flinfinite? flround))

  (define (flonum-bytes f)
    (unless (flonum? f)
      (die 'flonum-bytes "not a flonum" f))
    (values
      ($flonum-u8-ref f 0)
      ($flonum-u8-ref f 1)
      ($flonum-u8-ref f 2)
      ($flonum-u8-ref f 3)
      ($flonum-u8-ref f 4)
      ($flonum-u8-ref f 5)
      ($flonum-u8-ref f 6)
      ($flonum-u8-ref f 7)))
  (define (flonum-parts x)
    (unless (flonum? x)
      (die 'flonum-parts "not a flonum" x))
    (let-values ([(b0 b1 b2 b3 b4 b5 b6 b7) (flonum-bytes x)])
      (values
        (zero? (fxlogand b0 128))
        (+ (fxsll (fxlogand b0 127) 4)
           (fxsra b1 4))
        (+ (+ b7 (fxsll b6 8) (fxsll b5 16))
           (* (+ b4
                 (fxsll b3 8)
                 (fxsll b2 16)
                 (fxsll (fxlogand b1 #b1111) 24))
              (expt 2 24))))))
  (define ($zero-m? f)
    (and ($fxzero? ($flonum-u8-ref f 7))
         ($fxzero? ($flonum-u8-ref f 6))
         ($fxzero? ($flonum-u8-ref f 5))
         ($fxzero? ($flonum-u8-ref f 4))
         ($fxzero? ($flonum-u8-ref f 3))
         ($fxzero? ($flonum-u8-ref f 2))
         ($fxzero? ($fxlogand ($flonum-u8-ref f 1) #b1111))))

  (define ($flonum-rational? x)
    (let ([be ($fxlogand ($flonum-sbe x)
                ($fxsub1 ($fxsll 1 11)))])
      ($fx< be 2047)))

  (define ($flonum-integer? x)
    (let ([be ($fxlogand ($flonum-sbe x)
                ($fxsub1 ($fxsll 1 11)))])
      (cond
        [($fx= be 2047)  ;;; nans and infs
         #f]
        [($fx>= be 1075) ;;; magnitue large enough
         #t]
        [($fx= be 0) ;;; denormalized double, only +/-0.0 is integer
         (and ($fx= ($flonum-u8-ref x 7) 0)
              ($fx= ($flonum-u8-ref x 6) 0)
              ($fx= ($flonum-u8-ref x 5) 0)
              ($fx= ($flonum-u8-ref x 4) 0)
              ($fx= ($flonum-u8-ref x 3) 0)
              ($fx= ($flonum-u8-ref x 2) 0)
              ($fx= ($flonum-u8-ref x 1) 0))]
        [($fx< be ($fx+ 1075 -52)) ;;; too small to be an integer
         #f]
        [else ($fl= x ($$flround x))])))


  (define ($$flround x)
    (foreign-call "ikrt_fl_round" x ($make-flonum)))

  (define ($flround x)
    ;;; optimize for integer flonums case
    (define (ratnum-round n nbe)
      (let ([d (sll 1 nbe)])
        (let ([q (sra n nbe)]
              [r (bitwise-and n (sub1 d))])
          (let ([r2 (+ r r)])
            (cond
              [(< r2 d) q]
              [(> r2 d) (+ q 1)]
              [else (if (even? q) q (+ q 1))])))))
    (let ([sbe ($flonum-sbe x)])
      (let ([be ($fxlogand sbe #x7FF)])
        (cond
          ;;; nans/infs/magnitude large enough to be an integer
          [($fx>= be 1075) x]
          [else
           ;;; this really needs to get optimized.
           (let-values ([(pos? be m) (flonum-parts x)])
             (cond
               [(= be 0) ;;; denormalized
                (if pos? +0.0 -0.0)]
               [else ; normalized flonum
                (let ([r
                       (inexact
                         (ratnum-round (+ m (expt 2 52)) (- 1075 be)))])
                  (if pos? r ($fl* r -1.0)))]))]))))

  (define (flround x)
    (if (flonum? x)
        ($flround x)
        (die 'flround "not a flonum" x)))

  (module ($flonum->exact)
    (define ($flonum-signed-mantissa x)
      (let ([b0 ($flonum-u8-ref x 0)])
        (let ([m0 ($fx+ ($flonum-u8-ref x 7)
                        ($fx+ ($fxsll ($flonum-u8-ref x 6) 8)
                              ($fxsll ($flonum-u8-ref x 5) 16)))]
              [m1 ($fx+ ($flonum-u8-ref x 4)
                        ($fx+ ($fxsll ($flonum-u8-ref x 3) 8)
                              ($fxsll ($flonum-u8-ref x 2) 16)))]
              [m2 (let ([b1 ($flonum-u8-ref x 1)])
                    (if (and ($fx= ($fxlogand b0 #x7F) 0)
                             ($fx= ($fxsra b1 4) 0))
                        ($fxlogand b1 #xF)
                        ($fxlogor ($fxlogand b1 #xF) #x10)))])
          (if ($fx= 0 ($fxlogand #x80 b0))
              (+ (bitwise-arithmetic-shift-left ($fxlogor m1 ($fxsll m2 24)) 24) m0)
              (+ (bitwise-arithmetic-shift-left
                   ($fx- 0 ($fxlogor m1 ($fxsll m2 24))) 24)
                 ($fx- 0 m0))))))
    (define ($flonum->exact x)
      (import (ikarus))
      (let ([sbe ($flonum-sbe x)])
        (let ([be ($fxlogand sbe #x7FF)])
          (cond
            [($fx= be 2047) #f] ;;; nans/infs
            [($fx>= be 1075)    ;;; magnitude large enough to be an integer
             (bitwise-arithmetic-shift-left
               ($flonum-signed-mantissa x)
               (- be 1075))]
            [else
             ;;; this really needs to get optimized.
             (let-values ([(pos? be m) (flonum-parts x)])
               (cond
                 [(= be 0) ;;; denormalized
                  (if (= m 0)
                      0
                      (* (if pos? 1 -1)
                         (/ m (expt 2 1074))))]
                 [else ; normalized flonum
                  (/ (+ m (expt 2 52))
                     (bitwise-arithmetic-shift-left
                       (if pos? 1 -1)
                       (- 1075 be)))]))])))))

  (define (flnumerator x)
    (unless (flonum? x)
      (die 'flnumerator "not a flonum" x))
    (cond
      [($flonum-integer? x) x]
      [($flonum-rational? x)
       (exact->inexact (numerator ($flonum->exact x)))]
      [else x]))

  (define (fldenominator x)
    (unless (flonum? x)
      (die 'fldenominator "not a flonum" x))
    (cond
      [($flonum-integer? x) 1.0]
      [($flonum-rational? x)
       (exact->inexact (denominator ($flonum->exact x)))]
      [(flnan? x) x]
      [else 1.0]))

  (define (fleven? x)
    ;;; FIXME: optimize
    (unless (flonum? x)
      (die 'fleven? "not a flonum" x))
    (let ([v ($flonum->exact x)])
      (cond
        [(fixnum? v) ($fx= ($fxlogand v 1) 0)]
        [(bignum? v)
         (foreign-call "ikrt_even_bn" v)]
        [else (die 'fleven? "not an integer flonum" x)])))

  (define (flodd? x)
    (unless (flonum? x)
      (die 'flodd? "not a flonum" x))
    ;;; FIXME: optimize
    (let ([v ($flonum->exact x)])
      (cond
        [(fixnum? v) ($fx= ($fxlogand v 1) 1)]
        [(bignum? v)
         (not (foreign-call "ikrt_even_bn" v))]
        [else (die 'flodd? "not an integer flonum" x)])))

  (define (flinteger? x)
    (if (flonum? x)
        ($flonum-integer? x)
        (die 'flinteger? "not a flonum" x)))

  (define (flinfinite? x)
    (if (flonum? x)
        (let ([be (fxlogand ($flonum-sbe x) (sub1 (fxsll 1 11)))])
          (and (fx= be 2047)  ;;; nans and infs
               ($zero-m? x)))
        (die 'flinfinite? "not a flonum" x)))

  (define (flnan? x)
    (if (flonum? x)
        (let ([be (fxlogand ($flonum-sbe x) (sub1 (fxsll 1 11)))])
          (and (fx= be 2047)  ;;; nans and infs
               (not ($zero-m? x))))
        (die 'flnan? "not a flonum" x)))

  (define (flfinite? x)
    (if (flonum? x)
        (let ([be (fxlogand ($flonum-sbe x) (sub1 (fxsll 1 11)))])
          (not (fx= be 2047)))
        (die 'flfinite? "not a flonum" x)))

  (define ($flzero? x)
    (let ([be (fxlogand ($flonum-sbe x) (sub1 (fxsll 1 11)))])
       (and
         (fx= be 0) ;;; denormalized double, only +/-0.0 is integer
         (and (fx= ($flonum-u8-ref x 7) 0)
              (fx= ($flonum-u8-ref x 6) 0)
              (fx= ($flonum-u8-ref x 5) 0)
              (fx= ($flonum-u8-ref x 4) 0)
              (fx= ($flonum-u8-ref x 3) 0)
              (fx= ($flonum-u8-ref x 2) 0)
              (fx= ($flonum-u8-ref x 1) 0)))))

  (define ($flnegative? x)
    (let ([b0 ($flonum-u8-ref x 0)])
      (fx> b0 127)))

  (define ($exact x who)
    (import (ikarus system $compnums))
    (cond
      [(flonum? x)
       (or ($flonum->exact x)
           (die who "number has no real value" x))]
      [(cflonum? x)
       (make-rectangular
         (or ($flonum->exact ($cflonum-real x))
             (die who "number has no real value" x))
         (or ($flonum->exact ($cflonum-imag x))
             (die who "number has no real value" x)))]
      [(or (fixnum? x) (ratnum? x) (bignum? x) (compnum? x)) x]
      [else (die who "not a number" x)]))

  (define (inexact->exact x) ($exact x 'inexact->exact))
  (define (exact x) ($exact x 'exact))


  (define (flpositive? x)
    (if (flonum? x)
        ($fl> x 0.0)
        (die 'flpositive? "not a flonum" x)))

  (define (flabs x)
    (if (flonum? x)
        (if ($fx> ($flonum-u8-ref x 0) 127)
            ($fl* x -1.0)
            x)
        (die 'flabs "not a flonum" x)))

  (define (fixnum->flonum x)
    (if (fixnum? x)
        ($fixnum->flonum x)
        (die 'fixnum->flonum "not a fixnum")))

  (define (flsin x)
    (if (flonum? x)
        (foreign-call "ikrt_fl_sin" x)
        (die 'flsin "not a flonum" x)))

  (define (flcos x)
    (if (flonum? x)
        (foreign-call "ikrt_fl_cos" x)
        (die 'flcos "not a flonum" x)))

  (define (fltan x)
    (if (flonum? x)
        (foreign-call "ikrt_fl_tan" x)
        (die 'fltan "not a flonum" x)))

  (define (flasin x)
    (if (flonum? x)
        (foreign-call "ikrt_fl_asin" x)
        (die 'flasin "not a flonum" x)))

  (define (flacos x)
    (if (flonum? x)
        (foreign-call "ikrt_fl_acos" x)
        (die 'flacos "not a flonum" x)))

  (define flatan
    (case-lambda
      [(x)
       (if (flonum? x)
           (foreign-call "ikrt_fl_atan" x)
           (die 'flatan "not a flonum" x))]
      [(x y)
       (if (flonum? x)
           (if (flonum? y)
               (foreign-call "ikrt_atan2" x y)
               (die 'flatan "not a flonum" y))
           (die 'flatan "not a flonum" x))]))

  (define (flfloor x)
    (define (ratnum-floor x)
      (let ([n (numerator x)] [d (denominator x)])
        (let ([q (quotient n d)])
          (if (>= n 0) q (- q 1)))))
    (cond
      [(flonum? x)
       ;;; optimize for integer flonums case
       (let ([e ($flonum->exact x)])
         (cond
           [(ratnum? e)
            (exact->inexact (ratnum-floor e))]
           [else x]))]
      [else (die 'flfloor "not a flonum" x)]))

  (define (flceiling x)
    (cond
      [(flonum? x)
       ;;; optimize for integer flonums case
       (let ([e ($flonum->exact x)])
         (cond
           [(ratnum? e)
            (exact->inexact (ceiling e))]
           [else x]))]
      [else (die 'flceiling "not a flonum" x)]))

  (define (flexp x)
    (if (flonum? x)
        (foreign-call "ikrt_fl_exp" x ($make-flonum))
        (die 'flexp "not a flonum" x)))

  (define fllog
    (case-lambda
      [(x)
       (if (flonum? x)
           (foreign-call "ikrt_fl_log" x)
           (die 'fllog "not a flonum" x))]
      [(x y)
       (if (flonum? x)
           (if (flonum? y)
               (fl/ (foreign-call "ikrt_fl_log" x)
                    (foreign-call "ikrt_fl_log" y))
               (die 'fllog "not a flonum" y))
           (die 'fllog "not a flonum" x))]))

  (define (flexpt x y)
    (if (flonum? x)
        (if (flonum? y)
            (let ([y^ ($flonum->exact y)])
              ;;; FIXME: performance bottleneck?
              (cond
                [(fixnum? y^) (inexact (expt x y^))]
                [(bignum? y^) (inexact (expt x y^))]
                [else
                 (foreign-call "ikrt_flfl_expt" x y ($make-flonum))]))
            (die 'flexpt "not a flonum" y))
        (die 'fllog "not a flonum" x)))
)


(library (ikarus generic-arithmetic)
  (export + - * / zero? = < <= > >= add1 sub1 quotient remainder
          modulo even? odd? bitwise-and bitwise-not bitwise-ior
          bitwise-xor bitwise-if
          bitwise-arithmetic-shift-right bitwise-arithmetic-shift-left
          bitwise-arithmetic-shift
          bitwise-length bitwise-copy-bit-field
          bitwise-copy-bit bitwise-bit-field
          positive? negative? expt gcd lcm numerator denominator
          exact-integer-sqrt
          quotient+remainder number->string min max
          abs truncate fltruncate sra sll real->flonum
          exact->inexact inexact floor ceiling round log fl=? fl<? fl<=? fl>?
          fl>=? fl+ fl- fl* fl/ flsqrt flmin flzero? flnegative?
          sin cos tan asin acos atan sqrt exp
          sinh cosh tanh asinh acosh atanh
          flmax random
          error@add1 error@sub1)
  (import
    (ikarus system $fx)
    (ikarus system $flonums)
    (ikarus system $ratnums)
    (ikarus system $bignums)
    (ikarus system $compnums)
    (ikarus system $chars)
    (ikarus system $strings)
    (only (ikarus flonums) $flonum->exact $flzero? $flnegative?
          $flround)
    (except (ikarus) + - * / zero? = < <= > >= add1 sub1 quotient
            remainder modulo even? odd? quotient+remainder number->string
            bitwise-arithmetic-shift-right bitwise-arithmetic-shift-left
            bitwise-arithmetic-shift
            bitwise-length bitwise-copy-bit-field
            bitwise-copy-bit bitwise-bit-field
            positive? negative? bitwise-and bitwise-not bitwise-ior
            bitwise-xor bitwise-if
            expt gcd lcm numerator denominator
            exact->inexact inexact floor ceiling round log
            exact-integer-sqrt min max abs real->flonum
            fl=? fl<? fl<=? fl>? fl>=? fl+ fl- fl* fl/ flsqrt flmin
            flzero? flnegative? sra sll exp
            sin cos tan asin acos atan sqrt truncate fltruncate
            sinh cosh tanh asinh acosh atanh
            flmax random))

  (define (bignum->flonum x)
    (foreign-call "ikrt_bignum_to_flonum" x 0 ($make-flonum)))


  ;;; (define (ratnum->flonum x)
  ;;;   (define (->flonum n d)
  ;;;     (let-values ([(q r) (quotient+remainder n d)])
  ;;;       (if (= r 0)
  ;;;           (inexact q)
  ;;;           (if (= q 0)
  ;;;               (/ (->flonum d n))
  ;;;               (+ q (->flonum r d))))))
  ;;;   (let ([n (numerator x)] [d (denominator x)])
  ;;;     (let ([b (bitwise-first-bit-set n)])
  ;;;       (if (eqv? b 0)
  ;;;           (let ([b (bitwise-first-bit-set d)])
  ;;;             (if (eqv? b 0)
  ;;;                 (->flonum n d)
  ;;;                 (/ (->flonum n (bitwise-arithmetic-shift-right d b))
  ;;;                    (expt 2.0 b))))
  ;;;           (* (->flonum (bitwise-arithmetic-shift-right n b) d)
  ;;;              (expt 2.0 b))))))

  ;;; (define (ratnum->flonum x)
  ;;;   (let f ([n ($ratnum-n x)] [d ($ratnum-d x)])
  ;;;     (let-values ([(q r) (quotient+remainder n d)])
  ;;;       (if (= q 0)
  ;;;           (/ 1.0 (f d n))
  ;;;           (if (= r 0)
  ;;;               (inexact q)
  ;;;               (+ q (f r d)))))))

  ;;; (define (ratnum->flonum num)
  ;;;   (define (rat n m)
  ;;;     (let-values ([(q r) (quotient+remainder n m)])
  ;;;        (if (= r 0)
  ;;;            (inexact q)
  ;;;            (fl+ (inexact q) (fl/ 1.0 (rat  m r))))))
  ;;;   (define (pos n d)
  ;;;     (cond
  ;;;       [(even? n)
  ;;;        (* (pos (sra n 1) d) 2.0)]
  ;;;       [(even? d)
  ;;;        (/ (pos n (sra d 1)) 2.0)]
  ;;;       [(> n d) (rat n d)]
  ;;;       [else
  ;;;        (/ (rat d n))]))
  ;;;   (let ([n ($ratnum-n num)] [d ($ratnum-d num)])
  ;;;     (if (> n 0)
  ;;;         (pos n d)
  ;;;         (- (pos (- n) d)))))

  (define (ratnum->flonum num)
    (define *precision* 53)
    (define (long-div1 n d)
      (let-values ([(q r) (quotient+remainder n d)])
        (cond
          [(< (* r 2) d) (inexact q)]
          [else (inexact (+ q 1))]
          ;[else (error #f "invalid" n d q r)]
          )))
    (define (long-div2 n d bits)
      (let f ([bits bits] [ac (long-div1 n d)])
        (cond
          [(= bits 0) ac]
          [else (f (- bits 1) (/ ac 2.0))])))
    (define (pos n d)
      (let ([nbits (bitwise-length n)]
            [dbits (bitwise-length d)])
        (let ([diff-bits (- nbits dbits)])
          (if (>= diff-bits *precision*)
              (long-div1 n d)
              (let ([extra-bits (- *precision* diff-bits)])
                (long-div2 (sll n extra-bits) d extra-bits))))))
    (let ([n ($ratnum-n num)] [d ($ratnum-d num)])
      (if (> n 0)
          (pos n d)
          (- (pos (- n) d)))))

  (define (err who x)
    (die who (if (number? x) "invalid argument" "not a number") x))


  (define binary+
    (lambda (x y)
      (cond
        [(fixnum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxfxplus" x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnplus" x y)]
           [(flonum? y)
            ($fl+ ($fixnum->flonum x) y)]
           [(ratnum? y)
            ($make-ratnum
              (+ (* x ($ratnum-d y)) ($ratnum-n y))
              ($ratnum-d y))]
           [(compnum? y)
            ($make-compnum
              (binary+ x ($compnum-real y))
              ($compnum-imag y))]
           [(cflonum? y)
            ($make-cflonum
              (binary+ x ($cflonum-real y))
              ($cflonum-imag y))]
           [else (err '+ y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxbnplus" y x)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnplus" x y)]
           [(flonum? y)
            ($fl+ (bignum->flonum x) y)]
           [(ratnum? y)
            ($make-ratnum
              (+ (* x ($ratnum-d y)) ($ratnum-n y))
              ($ratnum-d y))]
           [(compnum? y)
            ($make-compnum
              (binary+ x ($compnum-real y))
              ($compnum-imag y))]
           [(cflonum? y)
            ($make-cflonum
              (binary+ x ($cflonum-real y))
              ($cflonum-imag y))]
           [else (err '+ y)])]
        [(flonum? x)
         (cond
           [(fixnum? y)
            ($fl+ x ($fixnum->flonum y))]
           [(bignum? y)
            ($fl+ x (bignum->flonum y))]
           [(flonum? y)
            ($fl+ x y)]
           [(ratnum? y)
            ($fl+ x (ratnum->flonum y))]
           [(cflonum? y)
            ($make-cflonum
              ($fl+ x ($cflonum-real y))
              ($cflonum-imag y))]
           [(compnum? y)
            ($make-cflonum
              (binary+ x ($compnum-real y))
              (inexact ($compnum-imag y)))]
           [else (err '+ y)])]
        [(ratnum? x)
         (cond
           [(or (fixnum? y) (bignum? y))
            ($make-ratnum
              (+ (* y ($ratnum-d x)) ($ratnum-n x))
              ($ratnum-d x))]
           [(flonum? y)
            ($fl+ y (ratnum->flonum x))]
           [(ratnum? y)
            (let ([n0 ($ratnum-n x)] [n1 ($ratnum-n y)]
                  [d0 ($ratnum-d x)] [d1 ($ratnum-d y)])
              ;;; FIXME: inefficient
              (/ (+ (* n0 d1) (* n1 d0)) (* d0 d1)))]
           [(compnum? y)
            ($make-compnum
              (binary+ x ($compnum-real y))
              ($compnum-imag y))]
           [(cflonum? y)
            ($make-cflonum
              (binary+ x ($cflonum-real y))
              ($cflonum-imag y))]
           [else (err '+ y)])]
        [(compnum? x)
         (cond
           [(or (fixnum? y) (bignum? y) (ratnum? y))
            ($make-compnum
              (binary+ ($compnum-real x) y)
              ($compnum-imag x))]
           [(compnum? y)
            ($make-rectangular
              (binary+ ($compnum-real x) ($compnum-real y))
              (binary+ ($compnum-imag x) ($compnum-imag y)))]
           [(flonum? y)
            ($make-cflonum
              (binary+ y ($compnum-real x))
              (inexact ($compnum-imag x)))]
           [(cflonum? y)
            ($make-cflonum
              (binary+ ($compnum-real x) ($cflonum-real y))
              (binary+ ($compnum-imag x) ($cflonum-imag y)))]
           [else (err '+ y)])]
        [(cflonum? x)
         (cond
           [(cflonum? y)
            ($make-cflonum
              (binary+ ($cflonum-real x) ($cflonum-real y))
              (binary+ ($cflonum-imag x) ($cflonum-imag y)))]
           [(flonum? y)
            ($make-cflonum
              ($fl+ ($cflonum-real x) y)
              ($cflonum-imag x))]
           [(or (fixnum? y) (bignum? y) (ratnum? y))
            ($make-compnum
              (binary+ ($compnum-real x) y)
              ($compnum-imag x))]
           [(compnum? y)
            ($make-cflonum
              (binary+ ($cflonum-real x) ($compnum-real y))
              (binary+ ($cflonum-imag x) ($compnum-imag y)))]
           [else (err '+ y)])]
        [else (err '+ x)])))

  (define binary-bitwise-and
    (lambda (x y)
      (cond
        [(fixnum? x)
         (cond
           [(fixnum? y) ($fxlogand x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnlogand" x y)]
           [else
            (die 'bitwise-and "not an exact integer" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxbnlogand" y x)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnlogand" x y)]
           [else
            (die 'bitwise-and "not an exact integer" y)])]
        [else (die 'bitwise-and "not an exact integer" x)])))

  (define binary-bitwise-ior
    (lambda (x y)
      (cond
        [(fixnum? x)
         (cond
           [(fixnum? y) ($fxlogor x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnlogor" x y)]
           [else
            (die 'bitwise-ior "not an exact integer" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxbnlogor" y x)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnlogor" x y)]
           [else
            (die 'bitwise-ior "not an exact integer" y)])]
        [else (die 'bitwise-ior "not an exact integer" x)])))


  (define binary-bitwise-xor
    (lambda (x y)
      (define (fxbn x y)
        (let ([y0 (bitwise-and y (greatest-fixnum))]
              [y1 (bitwise-arithmetic-shift-right y (- (fixnum-width) 1))])
          (bitwise-ior
            ($fxlogand ($fxlogxor x y0) (greatest-fixnum))
            (bitwise-arithmetic-shift-left
              (bitwise-arithmetic-shift-right
                (if ($fx>= x 0) y (bitwise-not y))
                (- (fixnum-width) 1))
              (- (fixnum-width) 1)))))
      (define (bnbn x y)
        (let ([x0 (bitwise-and x (greatest-fixnum))]
              [x1 (bitwise-arithmetic-shift-right x (- (fixnum-width) 1))]
              [y0 (bitwise-and y (greatest-fixnum))]
              [y1 (bitwise-arithmetic-shift-right y (- (fixnum-width) 1))])
          (bitwise-ior
            ($fxlogand ($fxlogxor x0 y0) (greatest-fixnum))
            (bitwise-arithmetic-shift-left
              (binary-bitwise-xor x1 y1)
              (- (fixnum-width) 1)))))
      (cond
        [(fixnum? x)
         (cond
           [(fixnum? y) ($fxlogxor x y)]
           [(bignum? y) (fxbn x y)]
           [else
            (die 'bitwise-xor "not an exact integer" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y) (fxbn y x)]
           [(bignum? y) (bnbn x y)]
           [else
            (die 'bitwise-xor "not an exact integer" y)])]
        [else (die 'bitwise-xor "not an exact integer" x)])))


  (define binary-
    (lambda (x y)
      (cond
        [(fixnum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxfxminus" x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnminus" x y)]
           [(flonum? y)
            (if ($fx= x 0)
                ($fl* y -1.0)
                ($fl- ($fixnum->flonum x) y))]
           [(ratnum? y)
            (let ([n ($ratnum-n y)] [d ($ratnum-d y)])
              (binary/ (binary- (binary* d x) n) d))]
           [(compnum? y)
            ($make-compnum
              (binary- x ($compnum-real y))
              (binary- 0 ($compnum-imag y)))]
           [(cflonum? y)
            ($make-cflonum
              (binary- x ($cflonum-real y))
              ($fl- 0.0 ($cflonum-imag y)))]
           [else (err '- y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_bnfxminus" x y)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnminus" x y)]
           [(flonum? y)
            ($fl- (bignum->flonum x) y)]
           [(ratnum? y)
            (let ([n ($ratnum-n y)] [d ($ratnum-d y)])
              (binary/ (binary- (binary* d x) n) d))]
           [(compnum? y)
            ($make-compnum
              (binary- x ($compnum-real y))
              (binary- 0 ($compnum-imag y)))]
           [(cflonum? y)
            ($make-cflonum
              (binary- x ($cflonum-real y))
              ($fl- 0.0 ($cflonum-imag y)))]
           [else (err '- y)])]
        [(flonum? x)
         (cond
           [(flonum? y)
            ($fl- x y)]
           [(cflonum? y)
            ($make-cflonum
              ($fl- x ($cflonum-real y))
              ($fl- 0.0 ($cflonum-imag y)))]
           [(fixnum? y)
            ($fl- x ($fixnum->flonum y))]
           [(bignum? y)
            ($fl- x (bignum->flonum y))]
           [(ratnum? y)
            (let ([n ($ratnum-n y)] [d ($ratnum-d y)])
              (binary/ (binary- (binary* d x) n) d))]
           [(compnum? y)
            ($make-cflonum
              (binary- x ($compnum-real y))
              (binary- 0.0 ($compnum-imag y)))]
           [else (err '- y)])]
        [(ratnum? x)
         (let ([nx ($ratnum-n x)] [dx ($ratnum-d x)])
           (cond
             [(or (fixnum? y) (bignum? y) (flonum? y))
              (binary/ (binary- nx (binary* dx y)) dx)]
             [(ratnum? y)
              (let ([ny ($ratnum-n y)] [dy ($ratnum-d y)])
                (binary/ (binary- (binary* nx dy) (binary* ny dx))
                         (binary* dx dy)))]
             [(compnum? y)
              ($make-compnum
                (binary- x ($compnum-real y))
                (binary- 0 ($compnum-imag y)))]
             [(cflonum? y)
              ($make-cflonum
                (binary- x ($cflonum-real y))
                ($fl- 0.0 ($cflonum-imag y)))]
             [else (err '- y)]))]
        [(compnum? x)
         (cond
           [(or (fixnum? y) (bignum? y) (ratnum? y))
            ($make-compnum
               (binary- ($compnum-real x) y)
               ($compnum-imag x))]
           [(compnum? y)
            ($make-rectangular
              (binary- ($compnum-real x) ($compnum-real y))
              (binary- ($compnum-imag x) ($compnum-imag y)))]
           [(flonum? y)
            ($make-cflonum
              (binary- ($compnum-real x) y)
              (binary- ($compnum-imag x) 0.0))]
           [(cflonum? y)
            ($make-cflonum
              (binary- ($compnum-real x) ($cflonum-real y))
              (binary- ($compnum-imag x) ($cflonum-imag y)))]
           [else
            (err '- y)])]
        [(cflonum? x)
         (cond
           [(flonum? y)
            ($make-cflonum
               ($fl- ($cflonum-real x) y)
               ($cflonum-imag x))]
           [(cflonum? y)
            ($make-cflonum
              (binary- ($cflonum-real x) ($cflonum-real y))
              (binary- ($cflonum-imag x) ($cflonum-imag y)))]
           [(or (fixnum? y) (bignum? y) (ratnum? y))
            ($make-cflonum
               (binary- ($cflonum-real x) y)
               ($cflonum-imag x))]
           [(compnum? y)
            ($make-cflonum
              (binary- ($cflonum-real x) ($compnum-real y))
              (binary- ($cflonum-imag x) ($compnum-imag y)))]
           [else
            (err '- y)])]
        [else (err '- x)])))

  (define binary*
    (lambda (x y)
      (cond
        [(fixnum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxfxmult" x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnmult" x y)]
           [(flonum? y)
            ($fl* ($fixnum->flonum x) y)]
           [(ratnum? y)
            (binary/ (binary* x ($ratnum-n y)) ($ratnum-d y))]
           [(compnum? y)
            ($make-rectangular
              (binary* x ($compnum-real y))
              (binary* x ($compnum-imag y)))]
           [(cflonum? y)
            ($make-cflonum
              (binary* x ($cflonum-real y))
              (binary* x ($cflonum-imag y)))]
           [else (err '* y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxbnmult" y x)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnmult" x y)]
           [(flonum? y)
            ($fl* (bignum->flonum x) y)]
           [(ratnum? y)
            (binary/ (binary* x ($ratnum-n y)) ($ratnum-d y))]
           [(compnum? y)
            ($make-rectangular
              (binary* x ($compnum-real y))
              (binary* x ($compnum-imag y)))]
           [(cflonum? y)
            ($make-cflonum
              (binary* x ($cflonum-real y))
              (binary* x ($cflonum-imag y)))]
           [else (err '* y)])]
        [(flonum? x)
         (cond
           [(flonum? y)
            ($fl* x y)]
           [(cflonum? y)
            ($make-cflonum
              ($fl* x ($cflonum-real y))
              ($fl* x ($cflonum-imag y)))]
           [(fixnum? y)
            ($fl* x ($fixnum->flonum y))]
           [(bignum? y)
            ($fl* x (bignum->flonum y))]
           [(ratnum? y)
            (binary/ (binary* x ($ratnum-n y)) ($ratnum-d y))]
           [(compnum? y)
            ($make-cflonum
              (binary* x ($compnum-real y))
              (binary* x ($compnum-imag y)))]
           [else (err '* y)])]
        [(ratnum? x)
         (cond
           [(ratnum? y)
            (binary/ (binary* ($ratnum-n x) ($ratnum-n y))
                     (binary* ($ratnum-d x) ($ratnum-d y)))]
           [(compnum? y)
            ($make-rectangular
              (binary* x ($compnum-real y))
              (binary* x ($compnum-imag y)))]
           [(cflonum? y)
            ($make-cflonum
              (binary* x ($cflonum-real y))
              (binary* x ($cflonum-imag y)))]
           [else (binary* y x)])]
        [(compnum? x)
         (cond
           [(or (fixnum? y) (bignum? y) (ratnum? y))
            ($make-rectangular
              (binary* ($compnum-real x) y)
              (binary* ($compnum-imag x) y))]
           [(flonum? y)
            ($make-cflonum
              (binary* ($compnum-real x) y)
              (binary* ($compnum-imag x) y))]
           [(compnum? y)
            (let ([r0 ($compnum-real x)]
                  [r1 ($compnum-real y)]
                  [i0 ($compnum-imag x)]
                  [i1 ($compnum-imag y)])
              (make-rectangular
                (- (* r0 r1) (* i0 i1))
                (+ (* r0 i1) (* i0 r1))))]
           [(cflonum? y)
            (let ([r0 ($compnum-real x)]
                  [r1 ($cflonum-real y)]
                  [i0 ($compnum-imag x)]
                  [i1 ($cflonum-imag y)])
              (make-rectangular
                (- (* r0 r1) (* i0 i1))
                (+ (* r0 i1) (* i0 r1))))]
           [else (err '* y)])]
        [(cflonum? x)
         (cond
           [(flonum? y)
            ($make-cflonum
              ($fl* ($cflonum-real x) y)
              ($fl* ($cflonum-imag x) y))]
           [(cflonum? y)
            (let ([r0 ($cflonum-real x)]
                  [r1 ($cflonum-real y)]
                  [i0 ($cflonum-imag x)]
                  [i1 ($cflonum-imag y)])
              ($make-cflonum
                ($fl- ($fl* r0 r1) ($fl* i0 i1))
                ($fl+ ($fl* r0 i1) ($fl* i0 r1))))]
           [(or (fixnum? y) (bignum? y) (ratnum? y))
            ($make-cflonum
              (binary* ($compnum-real x) y)
              (binary* ($compnum-imag x) y))]
           [(compnum? y)
            (let ([r0 ($compnum-real x)]
                  [r1 ($compnum-real y)]
                  [i0 ($compnum-imag x)]
                  [i1 ($compnum-imag y)])
              (make-rectangular
                (- (* r0 r1) (* i0 i1))
                (+ (* r0 i1) (* i0 r1))))]
           [else (err '* y)])]
        [else (err '* x)])))

  (define +
    (case-lambda
      [(x y) (binary+ x y)]
      [(x y z) (binary+ (binary+ x y) z)]
      [(a)
       (cond
         [(fixnum? a) a]
         [(number? a) a]
         [else (die '+ "not a number" a)])]
      [() 0]
      [(a b c d . e*)
       (let f ([ac (binary+ (binary+ (binary+ a b) c) d)]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary+ ac (car e*)) (cdr e*))]))]))

  (define bitwise-and
    (case-lambda
      [(x y) (binary-bitwise-and x y)]
      [(x y z) (binary-bitwise-and (binary-bitwise-and x y) z)]
      [(a)
       (cond
         [(fixnum? a) a]
         [(bignum? a) a]
         [else (die 'bitwise-and "not a number" a)])]
      [() -1]
      [(a b c d . e*)
       (let f ([ac (binary-bitwise-and a
                     (binary-bitwise-and b
                       (binary-bitwise-and c d)))]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary-bitwise-and ac (car e*)) (cdr e*))]))]))

  (define bitwise-ior
    (case-lambda
      [(x y) (binary-bitwise-ior x y)]
      [(x y z) (binary-bitwise-ior (binary-bitwise-ior x y) z)]
      [(a)
       (cond
         [(fixnum? a) a]
         [(bignum? a) a]
         [else (die 'bitwise-ior "not a number" a)])]
      [() 0]
      [(a b c d . e*)
       (let f ([ac (binary-bitwise-ior a
                     (binary-bitwise-ior b
                       (binary-bitwise-ior c d)))]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary-bitwise-ior ac (car e*)) (cdr e*))]))]))

  (define bitwise-xor
    (case-lambda
      [(x y) (binary-bitwise-xor x y)]
      [(x y z) (binary-bitwise-xor (binary-bitwise-xor x y) z)]
      [(a)
       (cond
         [(fixnum? a) a]
         [(bignum? a) a]
         [else (die 'bitwise-xor "not a number" a)])]
      [() 0]
      [(a b c d . e*)
       (let f ([ac (binary-bitwise-xor a
                     (binary-bitwise-xor b
                       (binary-bitwise-xor c d)))]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary-bitwise-xor ac (car e*)) (cdr e*))]))]))

  (define (bitwise-not x)
    (cond
      [(fixnum? x) ($fxlognot x)]
      [(bignum? x) (foreign-call "ikrt_bnlognot" x)]
      [else (die 'bitwise-not "invalid argument" x)]))

  (define (bitwise-if x y z)
    (define who 'bitwise-if)
    (define (err x) (die who "not an exact integer" x))
    (unless (or (fixnum? x) (bignum? x)) (err x))
    (unless (or (fixnum? y) (bignum? y)) (err y))
    (unless (or (fixnum? z) (bignum? z)) (err z))
    (bitwise-ior
      (bitwise-and x y)
      (bitwise-and (bitwise-not x) z)))

  (define (bitwise-copy-bit-field x i j n)
    (define who 'bitwise-copy-bit-field)
    (define (err x) (die who "not an exact integer" x))
    (define (err2 x) (die who "index must be nonnegative" x))
    (define (err3 x y) (die who "indices must be in nondescending order" x y))
    (unless (or (fixnum? x) (bignum? x)) (err x))
    (unless (or (fixnum? i) (bignum? i)) (err i))
    (unless (or (fixnum? j) (bignum? j)) (err j))
    (unless (or (fixnum? n) (bignum? n)) (err n))
    (when (< i 0) (err2 i))
    (when (< j i) (err3 i j))
    (bitwise-if (sll (sub1 (sll 1 (- j i))) i) (sll n i) x))

  (define -
    (case-lambda
      [(x y) (binary- x y)]
      [(x y z) (binary- (binary- x y) z)]
      [(a) (binary- 0 a)]
      [(a b c d . e*)
       (let f ([ac (binary- (binary- (binary- a b) c) d)]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary- ac (car e*)) (cdr e*))]))]))

  (define *
    (case-lambda
      [(x y) (binary* x y)]
      [(x y z) (binary* (binary* x y) z)]
      [(a)
       (cond
         [(fixnum? a) a]
         [(number? a) a]
         [else (die '* "not a number" a)])]
      [() 1]
      [(a b c d . e*)
       (let f ([ac (binary* (binary* (binary* a b) c) d)]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary* ac (car e*)) (cdr e*))]))]))

  (define (binary-gcd x y)
    (define (gcd x y)
      (cond
        [($fx= y 0) x]
        [else (gcd y (remainder x y))]))
    (let ([x (if (< x 0) (- x) x)]
          [y (if (< y 0) (- y) y)])
      (cond
        [(> x y) (gcd x y)]
        [(< x y) (gcd y x)]
        [else x])))

  (define gcd
    (case-lambda
      [(x y)
       (cond
         [(or (fixnum? x) (bignum? x))
          (cond
            [(or (fixnum? y) (bignum? y))
             (binary-gcd x y)]
            [(number? y)
             (die 'gcd "not an exact integer" y)]
            [else
             (die 'gcd "not a number" y)])]
         [(number? x)
          (die 'gcd "not an exact integer" x)]
         [else
          (die 'gcd "not a number" x)])]
      [(x)
       (cond
         [(or (fixnum? x) (bignum? x)) x]
         [(number? x)
          (die 'gcd "not an exact integer" x)]
         [else
          (die 'gcd "not a number" x)])]
      [() 0]
      [(x y z . ls)
       (let f ([g (gcd (gcd x y) z)] [ls ls])
         (cond
           [(null? ls) g]
           [else (f (gcd g (car ls)) (cdr ls))]))]))


  (define lcm
    (case-lambda
      [(x y)
       (cond
         [(or (fixnum? x) (bignum? x))
          (cond
            [(or (fixnum? y) (bignum? y))
             (let ([x (if (< x 0) (- x) x)]
                   [y (if (< y 0) (- y) y)])
               (let ([g (binary-gcd x y)])
                 (binary* y (quotient x g))))]
            [(flonum? y)
             (let ([v ($flonum->exact y)])
               (cond
                 [(or (fixnum? v) (bignum? v))
                  (inexact (lcm x v))]
                 [else (die 'lcm "not an integer" y)]))]
            [else
             (die 'lcm "not an integer" y)])]
         [(flonum? x)
          (let ([v ($flonum->exact x)])
            (cond
              [(or (fixnum? v) (bignum? v))
               (inexact (lcm v y))]
              [else (die 'lcm "not an integer" x)]))]
         [else
          (die 'lcm "not an integer" x)])]
      [(x)
       (cond
         [(or (fixnum? x) (bignum? x)) x]
         [(flonum? x)
          (let ([v ($flonum->exact x)])
            (cond
              [(or (fixnum? v) (bignum? v)) x]
              [else (die 'lcm "not an integer" x)]))]
         [else
          (die 'lcm "not an integer" x)])]
      [() 1]
      [(x y z . ls)
       ;;; FIXME: incorrect for multiple roundings
       (let f ([g (lcm (lcm x y) z)] [ls ls])
         (cond
           [(null? ls) g]
           [else (f (lcm g (car ls)) (cdr ls))]))]))


  (define binary/
    (lambda (x y)
      (define (x/compy x y)
        (let ([yr (real-part y)]
              [yi (imag-part y)])
          (let ([denom (+ (* yr yr) (* yi yi))])
            (make-rectangular
              (binary/ (* x yr) denom)
              (binary/ (* (- x) yi) denom)))))
      (define (compx/y x y)
        (let ([xr (real-part x)]
              [xi (imag-part x)])
          (make-rectangular
            (binary/ xr y)
            (binary/ xi y))))
      (define (compx/compy x y)
        (let ([xr (real-part x)]
              [xi (imag-part x)]
              [yr (real-part y)]
              [yi (imag-part y)])
          (let ([denom (+ (* yr yr) (* yi yi))])
            (make-rectangular
              (binary/ (+ (* xr yr) (* xi yi)) denom)
              (binary/ (- (* xi yr) (* xr yi)) denom)))))
      (cond
        [(flonum? x)
         (cond
           [(flonum? y) ($fl/ x y)]
           [(fixnum? y) ($fl/ x ($fixnum->flonum y))]
           [(bignum? y) ($fl/ x (bignum->flonum y))]
           [(ratnum? y) ($fl/ x (ratnum->flonum y))]
           [(or (cflonum? y) (compnum? y)) (x/compy x y)]
           [else (err '/ y)])]
        [(fixnum? x)
         (cond
           [(flonum? y) ($fl/ ($fixnum->flonum x) y)]
           [(fixnum? y)
            (cond
              [($fx= y 0) (die '/ "division by 0")]
              [($fx> y 0)
               (if ($fx= y 1)
                   x
                   (let ([g (binary-gcd x y)])
                     (cond
                       [($fx= g y) (fxquotient x g)]
                       [($fx= g 1) ($make-ratnum x y)]
                       [else
                        ($make-ratnum (fxquotient x g) (fxquotient y g))])))]
              [else
               (if ($fx= y -1)
                   (binary- 0 x)
                   (let ([g (binary-gcd x y)])
                     (cond
                       [($fx= ($fx- 0 g) y) (binary- 0 (fxquotient x g))]
                       [($fx= g 1) ($make-ratnum (binary- 0 x) (binary- 0 y))]
                       [else
                        ($make-ratnum
                          (binary- 0 (fxquotient x g))
                          (binary- 0 (fxquotient y g)))])))])]
           [(bignum? y)
            (if ($fx= x 0)
                0
                (let ([g (binary-gcd x y)])
                  (cond
                    [(= g y) (quotient x g)]
                    [($bignum-positive? y)
                     (if ($fx= g 1)
                         ($make-ratnum x y)
                         ($make-ratnum (fxquotient x g) (quotient y g)))]
                    [else
                     (if ($fx= g 1)
                         ($make-ratnum (binary- 0 x) (binary- 0 y))
                         ($make-ratnum
                            (binary- 0 (fxquotient x g))
                            (binary- 0 (quotient y g))))])))]
           [(ratnum? y)
            (/ (* x ($ratnum-d y)) ($ratnum-n y))]
           [(or (compnum? y) (cflonum? y)) (x/compy x y)]
           [else (err '/ y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (cond
              [($fx= y 0) (die '/ "division by 0")]
              [($fx> y 0)
               (if ($fx= y 1)
                   x
                   (let ([g (binary-gcd x y)])
                     (cond
                       [($fx= g 1) ($make-ratnum x y)]
                       [($fx= g y) (quotient x g)]
                       [else
                        ($make-ratnum (quotient x g) (quotient y g))])))]
              [else
               (if ($fx= y -1)
                   (- x)
                   (let ([g (binary-gcd x y)])
                     (cond
                       [(= (- g) y) (- (quotient x g))]
                       [else
                        ($make-ratnum
                          (- (quotient x g))
                          (- (quotient y g)))])))])]
           [(bignum? y)
            (let ([g (binary-gcd x y)])
              (cond
                [($fx= g 1)
                 (if ($bignum-positive? y)
                     ($make-ratnum x y)
                     ($make-ratnum
                       (binary- 0 x)
                       (binary- 0 y)))]
                [($bignum-positive? y)
                 (if (= g y)
                     (quotient x g)
                     ($make-ratnum (quotient x g) (quotient y g)))]
                [else
                 (let ([y (binary- 0 y)])
                   (if (= g y)
                       (binary- 0 (quotient x g))
                       ($make-ratnum
                         (binary- 0 (quotient x g))
                         (quotient y g))))]))]
           [(flonum? y) ($fl/ (bignum->flonum x) y)]
           [(ratnum? y)
            (binary/ (binary* x ($ratnum-d y)) ($ratnum-n y))]
           [(or (compnum? y) (cflonum? y)) (x/compy x y)]
           [else (err '/ y)])]
        [(ratnum? x)
         (cond
           [(ratnum? y)
            (binary/
              (binary* ($ratnum-n x) ($ratnum-d y))
              (binary* ($ratnum-n y) ($ratnum-d x)))]
           [(or (compnum? y) (cflonum? y)) (x/compy x y)]
           [else (binary/ 1 (binary/ y x))])]
        [(or (compnum? x) (cflonum? x))
         (cond
           [(or (compnum? y) (cflonum? y)) (compx/compy x y)]
           [(or (fixnum? y) (bignum? y) (ratnum? y) (flonum? y)) (compx/y x y)]
           [else (err '/ y)])]
        [else (err '/ x)])))


  (define /
    (case-lambda
      [(x y) (binary/ x y)]
      [(x)
       (cond
         [(fixnum? x)
          (cond
            [($fxzero? x) (die '/ "division by 0")]
            [($fx> x 0)
             (if ($fx= x 1)
                 1
                 ($make-ratnum 1 x))]
            [else
             (if ($fx= x -1)
                 -1
                 ($make-ratnum -1 (- x)))])]
         [(bignum? x)
          (if ($bignum-positive? x)
              ($make-ratnum 1 x)
              ($make-ratnum -1 (- x)))]
         [(flonum? x) (foreign-call "ikrt_fl_invert" x)]
         [(ratnum? x)
          (let ([n ($ratnum-n x)] [d ($ratnum-d x)])
            (cond
              [($fx= n 1) d]
              [($fx= n -1) (- d)]
              [else ($make-ratnum d n)]))]
         [(compnum? x) (binary/ 1 x)]
         [else (die '/ "not a number" x)])]
      [(x y z . ls)
       (let f ([a (binary/ x y)] [b z] [ls ls])
         (cond
           [(null? ls) (binary/ a b)]
           [else (f (binary/ a b) (car ls) (cdr ls))]))]))


  (define flmax
    (case-lambda
      [(x y)
       (if (flonum? x)
           (if (flonum? y)
               (if ($fl< x y)
                   y
                   x)
               (die 'flmax "not a flonum" y))
           (die 'flmax "not a flonum" x))]
      [(x y z . rest)
       (let f ([a (flmax x y)] [b z] [ls rest])
         (cond
           [(null? ls) (flmax a b)]
           [else
            (f (flmax a b) (car ls) (cdr ls))]))]
      [(x)
       (if (flonum? x)
           x
           (die 'flmax "not a number" x))]))

  (define max
    (case-lambda
      [(x y)
       (cond
         [(fixnum? x)
          (cond
            [(fixnum? y)
             (if ($fx> x y) x y)]
            [(bignum? y)
             (if (positive-bignum? y) y x)]
            [(flonum? y)
             (let ([x ($fixnum->flonum x)])
               (if ($fl>= y x) y x))]
            [(ratnum? y) ;;; FIXME: optimize
             (if (>= x y) x y)]
            [else (die 'max "not a number" y)])]
         [(bignum? x)
          (cond
            [(fixnum? y)
             (if (positive-bignum? x) x y)]
            [(bignum? y)
             (if (bnbn> x y) x y)]
            [(flonum? y)
             (let ([x (bignum->flonum x)])
               (if ($fl>= y x) y x))]
            [(ratnum? y) ;;; FIXME: optimize
             (if (>= x y) x y)]
            [else (die 'max "not a number" y)])]
         [(flonum? x)
          (cond
            [(flonum? y)
             (if ($fl>= x y) x y)]
            [(fixnum? y)
             (let ([y ($fixnum->flonum y)])
               (if ($fl>= y x) y x))]
            [(bignum? y)
             (let ([y (bignum->flonum y)])
               (if ($fl>= y x) y x))]
            [(ratnum? y)
             ;;; FIXME: may be incorrect
             (let ([y (ratnum->flonum y)])
               (if ($fl>= y x) y x))]
            [else (die 'max "not a number" y)])]
         [(ratnum? x)
          (cond
            [(or (fixnum? y) (bignum? y) (ratnum? y))
             (if (>= x y) x y)]
            [(flonum? y)
             (let ([x (ratnum->flonum x)])
               (if ($fl>= x y) x y))]
            [else (die 'max "not a number" y)])]
         [else (die 'max "not a number" x)])]
      [(x y z . rest)
       (let f ([a (max x y)] [b z] [ls rest])
         (cond
           [(null? ls) (max a b)]
           [else
            (f (max a b) (car ls) (cdr ls))]))]
      [(x)
       (cond
         [(or (fixnum? x) (bignum? x) (ratnum? x) (flonum? x)) x]
         [else (die 'max "not a number" x)])]))

  (define min
    (case-lambda
      [(x y)
       (cond
         [(fixnum? x)
          (cond
            [(fixnum? y)
             (if ($fx> x y) y x)]
            [(bignum? y)
             (if (positive-bignum? y) x y)]
            [(flonum? y)
             (let ([x ($fixnum->flonum x)])
               (if ($fl>= y x) x y))]
            [(ratnum? y) ;;; FIXME: optimize
             (if (>= x y) y x)]
            [else (die 'min "not a number" y)])]
         [(bignum? x)
          (cond
            [(fixnum? y)
             (if (positive-bignum? x) y x)]
            [(bignum? y)
             (if (bnbn> x y) y x)]
            [(flonum? y)
             (let ([x (bignum->flonum x)])
               (if ($fl>= y x) x y))]
            [(ratnum? y) ;;; FIXME: optimize
             (if (>= x y) y x)]
            [else (die 'min "not a number" y)])]
         [(flonum? x)
          (cond
            [(flonum? y)
             (if ($fl>= x y) y x)]
            [(fixnum? y)
             (let ([y ($fixnum->flonum y)])
               (if ($fl>= y x) x y))]
            [(bignum? y)
             (let ([y (bignum->flonum y)])
               (if ($fl>= y x) x y))]
            [(ratnum? y)
             ;;; FIXME: may be incorrect
             (let ([y (ratnum->flonum y)])
               (if ($fl>= y x) x y))]
            [else (die 'min "not a number" y)])]
         [(ratnum? x)
          (cond
            [(or (fixnum? y) (bignum? y) (ratnum? y))
             (if (>= x y) y x)]
            [(flonum? y)
             (let ([x (ratnum->flonum x)])
               (if ($fl>= x y) y x))]
            [else (die 'min "not a number" y)])]
         [else (die 'min "not a number" x)])]
      [(x y z . rest)
       (let f ([a (min x y)] [b z] [ls rest])
         (cond
           [(null? ls) (min a b)]
           [else
            (f (min a b) (car ls) (cdr ls))]))]
      [(x)
       (cond
         [(or (fixnum? x) (bignum? x) (ratnum? x) (flonum? x)) x]
         [else (die 'min "not a number" x)])]))

  (define (abs x)
    (cond
      [(fixnum? x)
       (if ($fx< x 0) (- x) x)]
      [(bignum? x)
       (if ($bignum-positive? x) x (- x))]
      [(flonum? x)
       (if ($fx> ($flonum-u8-ref x 0) 127)
           ($fl* x -1.0)
           x)]
      [(ratnum? x)
       (let ([n ($ratnum-n x)])
         (if (< n 0)
             ($make-ratnum (- n) ($ratnum-d x))
             x))]
      [else (die 'abs "not a real number" x)]))

  (define flmin
    (case-lambda
      [(x y)
       (if (flonum? x)
           (if (flonum? y)
               (if ($fl< x y) x y)
               (die 'flmin "not a flonum" y))
           (die 'flmin "not a flonum" x))]
      [(x y z . rest)
       (let f ([a (flmin x y)] [b z] [ls rest])
         (cond
           [(null? ls) (flmin a b)]
           [else
            (f (flmin a b) (car ls) (cdr ls))]))]
      [(x)
       (if (flonum? x)
           x
           (die 'flmin "not a flonum" x))]))

  (define (->inexact x who)
    (cond
      [(fixnum? x) ($fixnum->flonum x)]
      [(bignum? x) (bignum->flonum x)]
      [(ratnum? x) (ratnum->flonum x)]
      [(flonum? x) x]
      [(compnum? x)
       (make-rectangular
         (->inexact (real-part x) who)
         (->inexact (imag-part x) who))]
      [(cflonum? x) x]
      [else
       (die who "not a number" x)]))

  (define (exact->inexact x)
    (->inexact x 'exact->inexact))

  (define (inexact x)
    (->inexact x 'inexact))

  (define real->flonum
    (lambda (x)
      (cond
        [(fixnum? x) ($fixnum->flonum x)]
        [(bignum? x) (bignum->flonum x)]
        [(ratnum? x) (ratnum->flonum x)]
        [(flonum? x) x]
        [else
         (die 'real->flonum "not a real number" x)])))

  (define positive-bignum?
    (lambda (x)
      (foreign-call "ikrt_positive_bn" x)))

  (define even-bignum?
    (lambda (x)
      (foreign-call "ikrt_even_bn" x)))

  (define ($fxeven? x)
    ($fxzero? ($fxlogand x 1)))

  (define (even? x)
    (cond
      [(fixnum? x) ($fxeven? x)]
      [(bignum? x) (even-bignum? x)]
      [(flonum? x)
       (let ([v ($flonum->exact x)])
         (cond
           [(fixnum? v) ($fxeven? v)]
           [(bignum? v) (even-bignum? v)]
           [else (die 'even? "not an integer" x)]))]
      [else (die 'even? "not an integer" x)]))

  (define (odd? x)
    (cond
      [(fixnum? x) (not ($fxeven? x))]
      [(bignum? x) (not (even-bignum? x))]
      [(flonum? x)
       (let ([v ($flonum->exact x)])
         (cond
           [(fixnum? v) (not ($fxeven? v))]
           [(bignum? v) (not (even-bignum? v))]
           [else (die 'odd? "not an integer" x)]))]
      [else (die 'odd? "not an integer" x)]))

  (module (number->string)
    (module (bignum->string)
      (define (bignum->decimal-string x)
        (utf8->string (foreign-call "ikrt_bignum_to_bytevector" x)))
      (module (bignum->power-string)
        (define string-map "0123456789ABCDEF")
        (define (init-string x chars)
          (if ($bignum-positive? x)
              (make-string chars)
              (let ([s (make-string ($fxadd1 chars))])
                (string-set! s 0 #\-)
                s)))
        (define (bignum-bits x)
          (define (add-bits b n)
            (cond
              [($fxzero? b) n]
              [else (add-bits ($fxsra b 1) ($fx+ n 1))]))
          (let f ([i ($fxsub1 ($bignum-size x))])
            (let ([b ($bignum-byte-ref x i)])
              (cond
                [($fxzero? b) (f ($fxsub1 i))]
                [else (add-bits b ($fxsll i 3))]))))
        (define (bignum->power-string x mask shift)
          (let ([bits (bignum-bits x)])
            (let ([chars (fxquotient (fx+ bits (fx- shift 1)) shift)])
              (let* ([s (init-string x chars)]
                     [n ($fx- (string-length s) 1)])
                (let f ([i 0] [j 0] [k 0] [b 0])
                  (cond
                    [($fx= i chars) s]
                    [($fx< k 8)
                     (f i ($fxadd1 j) ($fx+ k 8)
                        ($fxlogor b
                          ($fxsll ($bignum-byte-ref x j) k)))]
                    [else
                     (string-set! s ($fx- n i)
                       (string-ref string-map
                         ($fxlogand mask b)))
                     (f ($fxadd1 i) j ($fx- k shift) ($fxsra b shift))])))))))
      (define (bignum->string x r)
        (case r
          [(10) (bignum->decimal-string x)]
          [(2)  (bignum->power-string x  1 1)]
          [(8)  (bignum->power-string x  7 3)]
          [(16) (bignum->power-string x 15 4)]
          [else (die 'number->string "BUG")])))
    (define ratnum->string
      (lambda (x r)
        (string-append
          ($number->string ($ratnum-n x) r)
          "/"
          ($number->string ($ratnum-d x) r))))
    (define (imag x r)
      (cond
        [(eqv? x 1) "+"]
        [(eqv? x -1) "-"]
        [(or (< x 0) (eqv? x -0.0))
         ($number->string x r)]
        [else (string-append "+" ($number->string x r))]))
    (define $number->string
      (lambda (x r)
        (import (ikarus system $compnums))
        (cond
          [(fixnum? x) (fixnum->string x r)]
          [(bignum? x) (bignum->string x r)]
          [(flonum? x)
           (unless (eqv? r 10)
             (die 'number->string
                "invalid radix for inexact number"
                r x))
           (flonum->string x)]
          [(ratnum? x) (ratnum->string x r)]
          [(compnum? x)
           (let ([xr ($compnum-real x)]
                 [xi ($compnum-imag x)])
             (if (eqv? xr 0)
                 (string-append (imag xi r) "i")
                 (string-append
                   ($number->string xr r)
                   (imag xi r)
                   "i")))]
          [(cflonum? x)
           (let ([xr ($cflonum-real x)]
                 [xi ($cflonum-imag x)])
             (cond
               [(flnan? xi)
                (string-append ($number->string xr r) "+nan.0i")]
               [(flinfinite? xi)
                (string-append ($number->string xr r)
                  (if ($fl> xi 0.0) "+inf.0i" "-inf.0i"))]
               [else
                (string-append
                  ($number->string xr r) (imag xi r) "i")]))]
          [else (die 'number->string "not a number" x)])))
    (define do-warn
      (lambda ()
        (set! do-warn values)
        (raise-continuable
          (condition
            (make-warning)
            (make-who-condition 'number->string)
            (make-message-condition
              "precision argument is not supported")))))
    (define number->string
      (case-lambda
        [(x) ($number->string x 10)]
        [(x r)
         (unless (memv r '(2 8 10 16))
           (die 'number->string "invalid radix" r))
         ($number->string x r)]
        [(x r precision)
         ;(do-warn)
         (number->string x r)])))

  (define modulo
    (lambda (n m)
      (cond
        [(fixnum? n)
         (cond
           [(fixnum? m)
            (if (eqv? m 0)
                (die 'modulo "division by zero" n m)
                ($fxmodulo n m))]
           [(bignum? m)
            (if ($fx< n 0)
                (if ($bignum-positive? m)
                    (foreign-call "ikrt_fxbnplus" n m)
                    n)
                (if ($bignum-positive? m)
                    n
                    (foreign-call "ikrt_fxbnplus" n m)))]
           [(flonum? m)
            (let ([v ($flonum->exact m)])
              (cond
                [(or (fixnum? v) (bignum? v))
                 (inexact (modulo n v))]
                [else
                 (die 'modulo "not an integer" m)]))]
           [(ratnum? m) (die 'modulo "not an integer" m)]
           [else (die 'modulo "not a number" m)])]
        [(bignum? n)
         (cond
           [(fixnum? m)
            (if (eqv? m 0)
                (die 'modulo "division by zero" n m)
                (foreign-call "ikrt_bnfx_modulo" n m))]
           [(bignum? m)
            (if ($bignum-positive? n)
                (if ($bignum-positive? m)
                    (remainder n m)
                    (+ m (remainder n m)))
                (if ($bignum-positive? m)
                    (+ m (remainder n m))
                    (remainder n m)))]
           [(flonum? m)
            (let ([v ($flonum->exact m)])
              (cond
                [(or (fixnum? v) (bignum? v))
                 (inexact (modulo n v))]
                [else
                 (die 'modulo "not an integer" m)]))]
           [(ratnum? m) (die 'modulo "not an integer" m)]
           [else (die 'modulo "not a number" m)])]
        [(flonum? n)
         (let ([v ($flonum->exact n)])
           (cond
             [(or (fixnum? v) (bignum? v))
              (inexact (modulo v m))]
             [else
              (die 'modulo "not an integer" n)]))]
        [(ratnum? n) (die 'modulo "not an integer" n)]
        [else (die 'modulo "not a number" n)])))

  (define-syntax mk<
    (syntax-rules ()
      [(_ name fxfx< fxbn< bnfx< bnbn<
               fxfl< flfx< bnfl< flbn< flfl<
               fxrt< rtfx< bnrt< rtbn< flrt< rtfl< rtrt<)
       (let ()
         (define err
           (lambda (x) (die 'name "not a real number" x)))
         (define fxloopt
           (lambda (x y ls)
             (cond
               [(fixnum? y)
                (if (null? ls)
                    (fxfx< x y)
                    (if (fxfx< x y)
                        (fxloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(bignum? y)
                (if (null? ls)
                    (fxbn< x y)
                    (if (fxbn< x y)
                        (bnloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(flonum? y)
                (if (null? ls)
                    (fxfl< x y)
                    (if (fxfl< x y)
                        (flloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(ratnum? y)
                (if (null? ls)
                    (fxrt< x y)
                    (if (fxrt< x y)
                        (rtloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [else (err y)])))
         (define bnloopt
           (lambda (x y ls)
             (cond
               [(fixnum? y)
                (if (null? ls)
                    (bnfx< x y)
                    (if (bnfx< x y)
                        (fxloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(bignum? y)
                (if (null? ls)
                    (bnbn< x y)
                    (if (bnbn< x y)
                        (bnloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(flonum? y)
                (if (null? ls)
                    (bnfl< x y)
                    (if (bnfl< x y)
                        (flloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(ratnum? y)
                (if (null? ls)
                    (bnrt< x y)
                    (if (bnrt< x y)
                        (rtloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [else (err y)])))
         (define flloopt
           (lambda (x y ls)
             (cond
               [(fixnum? y)
                (if (null? ls)
                    (flfx< x y)
                    (if (flfx< x y)
                        (fxloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(bignum? y)
                (if (null? ls)
                    (flbn< x y)
                    (if (flbn< x y)
                        (bnloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(flonum? y)
                (if (null? ls)
                    (flfl< x y)
                    (if (flfl< x y)
                        (flloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(ratnum? y)
                (if (null? ls)
                    (flrt< x y)
                    (if (flrt< x y)
                        (rtloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [else (err y)])))
         (define rtloopt
           (lambda (x y ls)
             (cond
               [(fixnum? y)
                (if (null? ls)
                    (rtfx< x y)
                    (if (rtfx< x y)
                        (fxloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(bignum? y)
                (if (null? ls)
                    (rtbn< x y)
                    (if (rtbn< x y)
                        (bnloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(flonum? y)
                (if (null? ls)
                    (rtfl< x y)
                    (if (rtfl< x y)
                        (flloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(ratnum? y)
                (if (null? ls)
                    (rtrt< x y)
                    (if (rtrt< x y)
                        (rtloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [else (err y)])))
         (define loopf
           (lambda (x ls)
             (cond
               [(number? x)
                (if (null? ls)
                    #f
                    (loopf (car ls) (cdr ls)))]
               [else (err x)])))
         (define name
           (case-lambda
             [(x y)
              (cond
                [(fixnum? x)
                 (cond
                   [(fixnum? y) (fxfx< x y)]
                   [(bignum? y) (fxbn< x y)]
                   [(flonum? y) (fxfl< x y)]
                   [(ratnum? y) (fxrt< x y)]
                   [else (err y)])]
                [(bignum? x)
                 (cond
                   [(fixnum? y) (bnfx< x y)]
                   [(bignum? y) (bnbn< x y)]
                   [(flonum? y) (bnfl< x y)]
                   [(ratnum? y) (bnrt< x y)]
                   [else (err y)])]
                [(flonum? x)
                 (cond
                   [(fixnum? y) (flfx< x y)]
                   [(bignum? y) (flbn< x y)]
                   [(flonum? y) (flfl< x y)]
                   [(ratnum? y) (flrt< x y)]
                   [else (err y)])]
                [(ratnum? x)
                 (cond
                   [(fixnum? y) (rtfx< x y)]
                   [(bignum? y) (rtbn< x y)]
                   [(flonum? y) (rtfl< x y)]
                   [(ratnum? y) (rtrt< x y)]
                   [else (err y)])]
                [else (err x)])]
             [(x y z) (and (name x y) (name y z))]
             [(x) (if (number? x) #t (err x))]
             [(x y . ls)
              (cond
                [(fixnum? x) (fxloopt x y ls)]
                [(bignum? x) (bnloopt x y ls)]
                [(flonum? x) (flloopt x y ls)]
                [(ratnum? x) (rtloopt x y ls)]
                [else (err x)])]))
         name)]))


  (define-syntax false (syntax-rules () [(_ x y) #f]))
  (define-syntax bnbncmp
    (syntax-rules ()
      [(_ x y cmp)
       (cmp (foreign-call "ikrt_bnbncomp" x y) 0)]))
  (define-syntax bnbn= (syntax-rules () [(_ x y) (bnbncmp x y $fx=)]))
  (define-syntax bnbn< (syntax-rules () [(_ x y) (bnbncmp x y $fx<)]))
  (define-syntax bnbn> (syntax-rules () [(_ x y) (bnbncmp x y $fx>)]))
  (define-syntax bnbn<= (syntax-rules () [(_ x y) (bnbncmp x y $fx<=)]))
  (define-syntax bnbn>= (syntax-rules () [(_ x y) (bnbncmp x y $fx>=)]))
  (define-syntax fxbn< (syntax-rules () [(_ x y) (positive-bignum? y)]))
  (define-syntax bnfx< (syntax-rules () [(_ x y) (not (positive-bignum? x))]))
  (define-syntax fxbn> (syntax-rules () [(_ x y) (not (positive-bignum? y))]))
  (define-syntax bnfx> (syntax-rules () [(_ x y) (positive-bignum? x)]))

  (define-syntax flcmp
    (syntax-rules ()
      [(_ flfl? flfx? fxfl? flbn? bnfl? fl?)
       (begin
         (define-syntax flfl?
           (syntax-rules () [(_ x y) (fl? x y)]))
         (define-syntax flfx?
           (syntax-rules () [(_ x y) (fl? x ($fixnum->flonum y))]))
         (define-syntax flbn?
           (syntax-rules () [(_ x y) (fl? x (bignum->flonum y))]))
         (define-syntax fxfl?
           (syntax-rules () [(_ x y) (fl? ($fixnum->flonum x) y)]))
         (define-syntax bnfl?
           (syntax-rules () [(_ x y) (fl? (bignum->flonum x) y)])))]))

 ;;;  #;
 ;;; (begin
 ;;;   (define-syntax $fl=
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_equal" x y)]))
 ;;;   (define-syntax $fl<
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less" x y)]))
 ;;;   (define-syntax $fl<=
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less_or_equal" x y)]))
 ;;;   (define-syntax $fl>
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less" y x)]))
 ;;;   (define-syntax $fl>=
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less_or_equal" y x)])))

  (define-syntax define-flcmp
    (syntax-rules ()
      [(_ fl<? $fl<)
       (define fl<?
         (case-lambda
           [(x y)
            (if (flonum? x)
                (if (flonum? y)
                    ($fl< x y)
                    (die 'fl<? "not a flonum" y))
                (die 'fl<? "not a flonum" x))]
           [(x y z)
            (if (flonum? x)
                (if (flonum? y)
                    (if (flonum? z)
                        (and ($fl< x y) ($fl< y z))
                        (die 'fl<? "not a flonum" z))
                    (die 'fl<? "not a flonum" y))
                (die 'fl<? "not a flonum" x))]
           [(x)
            (or (flonum? x)
                (die 'fl<? "not a flonum" x))]
           [(x y . rest)
            (let ()
              (define (loopf a ls)
                (unless (flonum? a)
                  (die 'fl<? "not a flonum" a))
                (if (null? ls)
                    #f
                    (loopf (car ls) (cdr ls))))
              (if (flonum? x)
                  (if (flonum? y)
                      (if ($fl< x y)
                          (let f ([x y] [y (car rest)] [ls (cdr rest)])
                            (if (flonum? y)
                                (if (null? ls)
                                    ($fl< x y)
                                    (if ($fl< x y)
                                        (f y (car ls) (cdr ls))
                                        (loopf (car ls) (cdr ls))))
                                (die 'fl<? "not a flonum" y)))
                          (loopf (car rest) (cdr rest)))
                      (die 'fl<? "not a flonum" y))
                  (die 'fl<? "not a flonum" x)))]))]))
  (define-flcmp fl=? $fl=)
  (define-flcmp fl<? $fl<)
  (define-flcmp fl<=? $fl<=)
  (define-flcmp fl>? $fl>)
  (define-flcmp fl>=? $fl>=)

  (define fl+
    (case-lambda
      [(x y)
       (if (flonum? x)
           (if (flonum? y)
               ($fl+ x y)
               (die 'fl+ "not a flonum" y))
           (die 'fl+ "not a flonum" x))]
      [(x y z)
       (fl+ (fl+ x y) z)]
      [(x y z q . rest)
       (let f ([ac (fl+ (fl+ (fl+ x y) z) q)] [rest rest])
         (if (null? rest)
             ac
             (f (fl+ ac (car rest)) (cdr rest))))]
      [(x)
       (if (flonum? x)
           x
           (die 'fl+ "not a flonum" x))]
      [() (exact->inexact 0)]))


  (define fl-
    (case-lambda
      [(x y)
       (if (flonum? x)
           (if (flonum? y)
               ($fl- x y)
               (die 'fl- "not a flonum" y))
           (die 'fl- "not a flonum" x))]
      [(x y z)
       (fl- (fl- x y) z)]
      [(x y z q . rest)
       (let f ([ac (fl- (fl- (fl- x y) z) q)] [rest rest])
         (if (null? rest)
             ac
             (f (fl- ac (car rest)) (cdr rest))))]
      [(x)
       (if (flonum? x)
           ($fl* -1.0 x)
           (die 'fl+ "not a flonum" x))]))

  (define fl*
    (case-lambda
      [(x y)
       (if (flonum? x)
           (if (flonum? y)
               ($fl* x y)
               (die 'fl* "not a flonum" y))
           (die 'fl* "not a flonum" x))]
      [(x y z)
       (fl* (fl* x y) z)]
      [(x y z q . rest)
       (let f ([ac (fl* (fl* (fl* x y) z) q)] [rest rest])
         (if (null? rest)
             ac
             (f (fl* ac (car rest)) (cdr rest))))]
      [(x)
       (if (flonum? x)
           x
           (die 'fl* "not a flonum" x))]
      [() 1.0]))

  (define fl/
    (case-lambda
      [(x y)
       (if (flonum? x)
           (if (flonum? y)
               ($fl/ x y)
               (die 'fl/ "not a flonum" y))
           (die 'fl/ "not a flonum" x))]
      [(x y z)
       (fl/ (fl/ x y) z)]
      [(x y z q . rest)
       (let f ([ac (fl/ (fl/ (fl/ x y) z) q)] [rest rest])
         (if (null? rest)
             ac
             (f (fl/ ac (car rest)) (cdr rest))))]
      [(x)
       (if (flonum? x)
           ($fl/ 1.0 x)
           (die 'fl/ "not a flonum" x))]))

  (flcmp flfl= flfx= fxfl= flbn= bnfl= $fl=)
  (flcmp flfl< flfx< fxfl< flbn< bnfl< $fl<)
  (flcmp flfl> flfx> fxfl> flbn> bnfl> $fl>)
  (flcmp flfl<= flfx<= fxfl<= flbn<= bnfl<= $fl<=)
  (flcmp flfl>= flfx>= fxfl>= flbn>= bnfl>= $fl>=)

  (define-syntax cmp-ex/in
    (syntax-rules ()
      [(_ pred)
       (syntax-rules ()
         [(_ ex in)
          (let ([x ex] [y in])
            (if ($flonum-rational? y)
                (pred x (exact y))
                (pred (inexact x) y)))])]))
  (define-syntax cmp-in/ex
    (syntax-rules ()
      [(_ pred)
       (syntax-rules ()
         [(_ in ex)
          (let ([x in] [y ex])
            (if ($flonum-rational? x)
                (pred (exact x) y)
                (pred x (inexact y))))])]))

  (define-syntax flrt=  (cmp-in/ex =))
  (define-syntax rtfl=  (cmp-ex/in =))
  (define-syntax flrt<  (cmp-in/ex <))
  (define-syntax rtfl<  (cmp-ex/in <))
  (define-syntax flrt<= (cmp-in/ex <=))
  (define-syntax rtfl<= (cmp-ex/in <=))
  (define-syntax flrt>  (cmp-in/ex >))
  (define-syntax rtfl>  (cmp-ex/in >))
  (define-syntax flrt>= (cmp-in/ex >=))
  (define-syntax rtfl>= (cmp-ex/in >=))

  (define (exrt< x y) (< (* x ($ratnum-d y)) ($ratnum-n y)))
  (define (rtex< x y) (< ($ratnum-n x) (* y ($ratnum-d x))))
  (define (rtrt< x y) (< (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
  (define (rtrt<= x y) (<= (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
  (define (exrt> x y) (> (* x ($ratnum-d y)) ($ratnum-n y)))
  (define (rtex> x y) (> ($ratnum-n x) (* y ($ratnum-d x))))
  (define (rtrt> x y) (> (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
  (define (rtrt>= x y) (>= (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
  (define (rtrt= x y)
    (and (= ($ratnum-n x) ($ratnum-n y)) (= ($ratnum-d x) ($ratnum-d y))))



  (define =
    (let ()
      (define err
        (lambda (x) (die '= "not a number" x)))
      (define (fx? x y)
        (cond
          [(fixnum? y) ($fx= x y)]
          [(flonum? y) (fxfl= x y)]
          [(or (bignum? y) (ratnum? y) (compnum? y)) #f]
          [(cflonum? y)
           (and (flfl= 0.0 ($cflonum-imag y)) (fxfl= x ($cflonum-real y)))]
          [else (err y)]))
      (define (bn? x y)
        (cond
          [(bignum? y) (bnbn= x y)]
          [(flonum? y) (bnfl= x y)]
          [(or (fixnum? y) (ratnum? y) (compnum? y)) #f]
          [(cflonum? y)
           (and (flfl= 0.0 ($cflonum-imag y)) (bnfl= x ($cflonum-real y)))]
          [else (err y)]))
      (define (fl? x y)
        (cond
          [(flonum? y) (flfl= x y)]
          [(fixnum? y) (flfx= x y)]
          [(bignum? y) (flbn= x y)]
          [(ratnum? y) (flrt= x y)]
          [(compnum? y) #f]
          [(cflonum? y)
           (and (flfl= 0.0 ($cflonum-imag y)) (flfl= x ($cflonum-real y)))]
          [else (err y)]))
      (define (rn? x y)
        (cond
          [(flonum? y) (rtfl= x y)]
          [(ratnum? y) (rtrt= x y)]
          [(or (fixnum? y) (bignum? y) (compnum? y)) #f]
          [(cflonum? y)
           (and (flfl= 0.0 ($cflonum-imag y)) (rtfl= x ($cflonum-real y)))]
          [else (err y)]))
      (define (cn? x y)
        (cond
          [(compnum? y) (cncn= x y)]
          [(cflonum? y) (cncf= x y)]
          [(or (fixnum? y) (bignum? y) (flonum? y) (ratnum? y)) #f]
          [else (err y)]))
      (define (cf? x y)
        (cond
          [(cflonum? y) (cfcf= x y)]
          [(compnum? y) (cncf= y x)]
          [(or (fixnum? y) (bignum? y) (flonum? y) (ratnum? y))
           (and (flfl= 0.0 ($cflonum-imag x)) (= ($cflonum-real x) y))]
          [else (err y)]))
      (define-syntax doloop
        (syntax-rules ()
          [(_ cmp x0 y0 ls0)
           (let loop ([x x0] [y y0] [ls ls0])
             (if (cmp x y)
                 (if (null? ls) #t (loop x (car ls) (cdr ls)))
                 (if (null? ls) #f (loopf (car ls) (cdr ls)))))]))
      (define loopf
        (lambda (x ls)
          (if (number? x)
              (if (null? ls)
                  #f
                 (loopf (car ls) (cdr ls)))
              (err x))))
      (define (cncn= x y)
        (and
          (= ($compnum-real x) ($compnum-real y))
          (= ($compnum-imag x) ($compnum-imag y))))
      (define (cncf= x y)
        (and
          (= ($compnum-real x) ($cflonum-real y))
          (= ($compnum-imag x) ($cflonum-imag y))))
      (define (cfcf= x y)
        (and
          (= ($cflonum-real x) ($cflonum-real y))
          (= ($cflonum-imag x) ($cflonum-imag y))))
      (define =
        (case-lambda
          [(x y)
           (cond
             [(fixnum? x)  (fx? x y)]
             [(bignum? x)  (bn? x y)]
             [(flonum? x)  (fl? x y)]
             [(ratnum? x)  (rn? x y)]
             [(compnum? x) (cn? x y)]
             [(cflonum? x) (cf? x y)]
             [else (err x)])]
          [(x y z) (if (= x y) (= y z) (if (number? z) #f (err z)))]
          [(x) (if (number? x) #t (err x))]
          [(x y . ls)
           (cond
             [(fixnum? x) (doloop fx? x y ls)]
             [(bignum? x) (doloop bn? x y ls)]
             [(flonum? x) (doloop fl? x y ls)]
             [(ratnum? x) (doloop rn? x y ls)]
             [(compnum? x) (doloop cn? x y ls)]
             [(cflonum? x) (doloop cf? x y ls)]
             [else (err x)])]))
      =))

  ;(define =
  ;  (mk< = $fx= false false bnbn= fxfl= flfx= bnfl= flbn= flfl=
  ;             false false false false flrt= rtfl= rtrt=))

  (define <
    (mk< < $fx< fxbn< bnfx< bnbn< fxfl< flfx< bnfl< flbn< flfl<
               exrt< rtex< exrt< rtex< flrt< rtfl< rtrt<))
  (define >
    (mk< > $fx> fxbn> bnfx> bnbn> fxfl> flfx> bnfl> flbn> flfl>
               exrt> rtex> exrt> rtex> flrt> rtfl> rtrt>))
  (define <=
    (mk< <= $fx<= fxbn< bnfx< bnbn<= fxfl<= flfx<= bnfl<= flbn<= flfl<=
               exrt< rtex< exrt< rtex< flrt<= rtfl<= rtrt<=))
  (define >=
    (mk< >= $fx>= fxbn> bnfx> bnbn>= fxfl>= flfx>= bnfl>= flbn>= flfl>=
               exrt> rtex> exrt> rtex> flrt>= rtfl>= rtrt>=))

  (define error@add1
    (lambda (x)
      (import (ikarus))
      (cond
        [(fixnum? x) (+ (greatest-fixnum) 1)]
        [(number? x) (+ x 1)]
        [else (die 'add1 "not a number" x)])))

  (define add1
    (lambda (x)
      (import (ikarus))
      (add1 x)))

  (define error@sub1
    (lambda (x)
      (import (ikarus))
      (cond
        [(fixnum? x) (- (least-fixnum) 1)]
        [(number? x) (- x 1)]
        [else (die 'sub1 "not a number" x)])))

  (define sub1
    (lambda (x)
      (import (ikarus))
      (sub1 x)))

  (define zero?
    (lambda (x)
      (cond
        [(fixnum? x) (eq? x 0)]
        [(bignum? x) #f]
        [(ratnum? x) #f]
        [(flonum? x)
         (or ($fl= x 0.0) ($fl= x -0.0))]
        [(cflonum? x)
         (and ($fl= ($cflonum-real x) 0.0) ($fl= ($cflonum-imag x) 0.0))]
        [(compnum? x) #f]
        [else
         (die 'zero? "not a number" x)])))

(define (expt n m)
  ;;Return N raised to the power  M.  For non--zero N, this is:
  ;;
  ;;    (expt N M) === (exp (log (expt N M)))
  ;;               === (exp (* M (log N)))
  ;;
  ;;for N equal to zero:
  ;;
  ;;    (expt 0.0 Z) = 1.0 if Z = 0.0
  ;;                 = 0.0 if (real-part Z) is positive
  ;;
  ;;for  other cases  in which  the first  argument is  zero,  either an
  ;;exception       is       raised       with      condition       type
  ;;"&implementation-restriction",  or an  unspecified number  object is
  ;;returned.
  ;;
  ;;For an  exact real number  object N and  an exact integer  object M,
  ;;(expt N M)  must return an exact result.  For all  other values of N
  ;;and M, (expt N M) may return an inexact result, even when both N and
  ;;M are exact.
  ;;
  ;;Notice that this definition can  lead to unintuitive results; from a
  ;;discussion with Kent Dybvig: It does seem like:
  ;;
  ;;   (expt +inf.0+2.i 2)
  ;;
  ;;would be equivalent to:
  ;;
  ;;   (* +inf.0+2.i +inf.0+2.i)
  ;;
  ;;which evaluates  to +inf.0+inf.0i.   Nevertheless, I'm not  sure the
  ;;R6RS supports this interpretation.   According to the description of
  ;;expt, when z1 is not zero,
  ;;
  ;;   (expt z1 z2) => e^{z2 log z1}
  ;;
  ;;so,
  ;;
  ;;   (expt +inf.0+2.i 2) => (exp (* 2 (log +inf.0+2.i)))
  ;;
  ;;Meanwhile, the Section 11.7.3 subsection on transcendental functions
  ;;defines log as follows:
  ;;
  ;;   (log z) => log |z| + (angle z)i
  ;;
  ;;so,
  ;;
  ;;   (log +inf.0+2.i) =>
  ;;     (make-rectangular
  ;;       (log (magnitude +inf.0+2.0i))
  ;;       (angle +inf.0+2.0i))
  ;;
  ;;Since:
  ;;
  ;;   (magnitude +inf.0+2.i) => +inf.0,
  ;;   (log +inf.0) => +inf.0, and
  ;;   (angle +inf.0+2.i) => 0.0,
  ;;
  ;;we have:
  ;;
  ;;   (log +inf.0+2.i) => +inf.0+0.0i
  ;;
  ;;and finally:
  ;;
  ;;   (expt +inf.0+2.i 2) => (exp (* 2 +inf.0+0.0i))
  ;;                       => (exp +inf.0+0.0i)
  ;;                       => +inf.0+0.0i
  ;;

  (define (%expt-fx n m)
    ;;Recursive function computing N^M when M is a fixnum and N is:
    ;;
    ;;* A real number, infinite included, NaN excluded.
    ;;
    ;;* A finite complex number.
    ;;
    ;;This function recurses a number of  times equal to the bits in the
    ;;representation of M.
    ;;
    ;;Notes about used procedures:
    ;;
    ;;* ($fxlogand  m 1) is 1  for even m and  0 for odd m,  or in other
    ;;words: the return value is the rightmost bit of M.
    ;;
    ;;* $fxsra means "fixnum shift right arithmetic".
    ;;
    ;;* binary* is the multiplication with two arguments.
    ;;
    (cond (($fxzero? m)
	   1)
	  (($fxzero? ($fxlogand m 1)) ;the rightmost bit in M is zero
	   (%expt-fx (binary* n n) ($fxsra m 1)))
	  (else ;the rightmost bit in M is one
	   (binary* n (%expt-fx (binary* n n) ($fxsra m 1))))))

  (unless (number? n)
    (die 'expt "not a numebr" n))
  (cond ((fixnum? m)
	 (cond (($fxzero? m)
		(cond ((nan? n)		+nan.0)
		      ((exact? n)	1)
		      (else		1.)))
	       (($fx> m 0)
		(cond ((integer? n)
		       (%expt-fx n m))
		      ((ratnum? n)
		       ($make-ratnum (%expt-fx ($ratnum-n n) m)
				     (%expt-fx ($ratnum-d n) m)))
		      ((real? n) ;this includes the real infinite +inf.0
		       (if (nan? n)
			   +nan.0
			 (%expt-fx n m)))
		      ;;In the following clauses N is a non-real.
		      ((or (nan? (real-part n))
			   (nan? (imag-part n)))
		       +nan.0+nan.0i)
		      ((infinite? n) ;this handles correctly some special cases
		       (exp (* m (log n))))
		      (else
		       (%expt-fx n m))))
	       (else ;M is negative
		(let ((v (expt n (- m))))
		  (if (eq? v 0)
		      0
		    (/ 1 v))))))
	((bignum? m)
	 (cond ((eq? n 0)	0)
	       ((eq? n 1)	1)
	       ((eq? n -1)	(if (even-bignum? m) 1 -1))
	       ((nan? n)	+nan.0)
	       (else
		(die 'expt "result is too big to compute" n m))))
	((flonum? m)
	 (cond ((real? n)
		(cond ((nan? n)
		       +nan.0)
		      ((integer? m)
		       ;;N^M  when M  is an  integer always  has  a real
		       ;;number as result.
		       (flexpt (inexact n) m))
		      ((negative? n)
		       (exp (* m (log n))))
		      (else
		       (flexpt (inexact n) m))))
	       ((or (nan? (real-part n))
		    (nan? (imag-part n)))
		+nan.0+nan.0i)
	       (else
		(exp (* m (log n))))))
	((ratnum? m)
;;; (expt (expt n ($ratnum-n m))
;;;       (inexact ($make-ratnum 1 ($ratnum-d m))))
	 (expt n (inexact m)))
	((or (compnum? m) (cflonum? m))
	 (cond ((eq? n 0)
		0)
	       ((or (nan? (real-part n))
		    (nan? (imag-part n)))
		+nan.0+nan.0i)
	       ((zero? n)
		(if (flonum? n) 0.0 0.0+0.0i))
	       (else
		(exp (* m (log n))))))
	(else
	 (die 'expt "not a number" m))))

  (define quotient
    (lambda (x y)
      (let-values ([(q r) (quotient+remainder x y)])
        q)))

  (define remainder
    (lambda (x y)
      (let-values ([(q r) (quotient+remainder x y)])
        r)))

  (define quotient+remainder
    (lambda (x y)
      (cond
        [(eq? y 0)
         (die 'quotient+remainder
                "second argument must be non-zero")]
        [(fixnum? x)
         (cond
           [(fixnum? y)
            (if (eq? y -1)
                (values (- x) 0)
                (values (fxquotient x y) (fxremainder x y)))]
           [(bignum? y) (values 0 x)]
           [(flonum? y)
            (let ([v ($flonum->exact y)])
              (cond
                [(or (fixnum? v) (bignum? v))
                 (let-values ([(q r) (quotient+remainder x v)])
                   (values (inexact q) (inexact r)))]
                [else
                 (die 'quotient+remainder "not an integer" y)]))]
           [else (die 'quotient+remainder "not an integer" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (let ([p (foreign-call "ikrt_bnfxdivrem" x y)])
              (values (car p) (cdr p)))]
           [(bignum? y)
            (let ([p (foreign-call "ikrt_bnbndivrem" x y)])
              (values (car p) (cdr p)))]
           [(flonum? y)
            (let ([v ($flonum->exact y)])
              (cond
                [(or (fixnum? v) (bignum? v))
                 (let-values ([(q r) (quotient+remainder x v)])
                   (values (inexact q) (inexact r)))]
                [else
                 (die 'quotient+remainder "not an integer" y)]))]
           [else (die 'quotient+remainder "not an integer" y)])]
        [(flonum? x)
         (let ([v ($flonum->exact x)])
           (cond
             [(or (fixnum? v) (bignum? v))
              (let-values ([(q r) (quotient+remainder v y)])
                (values (inexact q) (inexact r)))]
             [else (die 'quotient+remainder "not an integer" x)]))]
        [else (die 'quotient+remainder "not an integer" x)])))

  (define positive?
    (lambda (x)
      (cond
        [(fixnum? x) ($fx> x 0)]
        [(flonum? x) ($fl> x 0.0)]
        [(bignum? x) (positive-bignum? x)]
        [(ratnum? x) (positive? ($ratnum-n x))]
        [else (die 'positive? "not a real number" x)])))

  (define negative?
    (lambda (x)
      (cond
        [(fixnum? x) ($fx< x 0)]
        [(flonum? x) ($fl< x 0.0)]
        [(bignum? x) (not (positive-bignum? x))]
        [(ratnum? x) (negative? ($ratnum-n x))]
        [else (die 'negative? "not a real number" x)])))

  (define sinh
    (lambda (x)
      (define who 'sinh)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_sinh" x)]
        [(or (fixnum? x) (bignum? x) (ratnum? x))
         (sinh (inexact x))]
        [(or (compnum? x) (cflonum? x))
         (let ([r (real-part x)] [i (imag-part x)])
           (make-rectangular
             (* (sinh r) (cos i))
             (* (cosh r) (sin i))))]
        [else (die who "not a number" x)])))

  (define cosh
    (lambda (x)
      (define who 'cosh)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_cosh" x)]
        [(or (fixnum? x) (bignum? x) (ratnum? x))
         (cosh (inexact x))]
        [(or (compnum? x) (cflonum? x))
         (let ([r (real-part x)] [i (imag-part x)])
           (make-rectangular
             (* (cosh r) (cos i))
             (* (sinh r) (sin i))))]
        [else (die who "not a number" x)])))

  (define tanh
    (lambda (x)
      (define who 'tanh)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_tanh" x)]
        [(or (fixnum? x) (bignum? x) (ratnum? x))
         (tanh (inexact x))]
        [(or (compnum? x) (cflonum? x))
         (let ([r (real-part x)] [i (imag-part x)])
           (let ([rr (* 2 r)] [ii (* 2 i)])
             (let ([cos2i (cos ii)] [cosh2r (cosh rr)])
               (make-rectangular
                 (/ (tanh rr) (+ 1 (/ cos2i cosh2r)))
                 (/ (sin ii) (+ cosh2r cos2i))))))]
        [else (die who "not a number" x)])))

  (define asinh
    (lambda (x)
      (define who 'asinh)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_asinh" x)]
        [(or (fixnum? x) (bignum? x) (ratnum? x))
         (asinh (inexact x))]
        [(or (cflonum? x) (compnum? x))
         (let ([x (real-part x)] [y (imag-part x)])
           (cond
             [(= x 0)
              (let ([v (asin y)])
                (make-rectangular (imag-part v) (real-part v)))]
             [else
              (let* ([z^2 (+ (* x x) (* y y))]
                     [z^2-1 (- z^2 1)]
                     [z^2-1^2 (* z^2-1 z^2-1)]
                     [y^2 (* y y)]
                     [q (sqrt (+ z^2-1^2 (* 4 y^2)))])
                (define (sgn x) (if (< x 0) -1 1))
                (make-rectangular
                  (* 0.5 (sgn x) (acosh (+ q z^2)))
                  (* 0.5 (sgn y) (acos (- q z^2)))))]))]
        [else (die who "not a number" x)])))

  (define acosh
    (lambda (x)
      (define who 'acosh)
      (cond
        [(flonum? x)
         (cond
           [($fl>= x 1.0) (foreign-call "ikrt_fl_acosh" x)]
           [($fl>= x -1.0)
            (make-rectangular 0 (atan (sqrt (- 1 (* x x))) x))]
           [($fl< x -1.0)
            (make-rectangular (acosh (- x)) PI)]
           [else +nan.0])]
        [(or (fixnum? x) (bignum? x) (ratnum? x))
         (acosh (inexact x))]
        [(or (cflonum? x) (compnum? x))
         (let ([x (real-part x)] [y (imag-part x)])
           (cond
             [(= x 0) (+ (asinh y) (make-rectangular 0 PI/2))]
             [else
              (let* ([z^2 (+ (* x x) (* y y))]
                     [z^2-1 (- z^2 1)]
                     [z^2-1^2 (* z^2-1 z^2-1)]
                     [y^2 (* y y)]
                     [q (sqrt (+ z^2-1^2 (* 4 y^2)))])
                (define (sgn x) (if (< x 0) -1 1))
                (+ (* 0.5 (sgn x) (acosh (+ q z^2)))
                   (* 0.5i (sgn y)
                      (- PI (* (sgn x) (acos (- q z^2)))))))]))]
        [else (die who "not a number" x)])))

  (define atanh
    (lambda (x)
      (define who 'atanh)
      (cond
        [(flonum? x)
         (cond
           [(and (fl<=? x 1.0) (fl>=? x -1.0))
            (foreign-call "ikrt_fl_atanh" x)]
           [else
            (- (atanh (fl/ 1.0 x))
               (if (fl<? x 0.0) (* -i PI/2) (* +i PI/2)))])]
        [(or (fixnum? x) (bignum? x) (ratnum? x))
         (atanh (inexact x))]
        [(number? x) (error who "not implemented" x)]
        [else (die who "not a number" x)])))

  (define sin
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_sin" x)]
        [(fixnum? x)
         (if (fx=? x 0)
             0
             (foreign-call "ikrt_fx_sin" x))]
        [(or (cflonum? x) (compnum? x))
         (let ([r (real-part x)] [i (imag-part x)])
           (make-rectangular
             (* (sin r) (cosh i))
             (* (cos r) (sinh i))))]
        [(number? x) (sin (inexact x))]
        [else (die 'sin "not a number" x)])))

  (define cos
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_cos" x)]
        [(fixnum? x)
         (if (fx=? x 0)
             1
             (foreign-call "ikrt_fx_cos" x))]
        [(or (cflonum? x) (compnum? x))
         (let ([r (real-part x)] [i (imag-part x)])
           (make-rectangular
             (* (cos r) (cosh i))
             (* (sin r) (sinh i))))]
        [(number? x) (cos (inexact x))]
        [else (die 'cos "not a number" x)])))

  (define tan
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_tan" x)]
        [(fixnum? x)
         (if (fx=? x 0)
             0
             (foreign-call "ikrt_fx_tan" x))]
        [(or (cflonum? x) (compnum? x))
         (let ([r (real-part x)] [i (imag-part x)])
           (make-rectangular
             (/ (sin (* 2 r))
                (+ (cos (* 2 r)) (cosh (* 2 i))))
             (/ (tanh (* 2 i))
                (+ 1 (/ (cos (* 2 r)) (cosh (* 2 i)))))))]
        [(number? x) (tan (inexact x))]
        [else (die 'tan "not a number" x)])))

  (module (PI PI/2)
    (import (ikarus))
    (define PI (acos -1))
    (define PI/2 (/ PI 2)))

  (define asin
    (lambda (x)
      (cond
        [(flonum? x)
         (cond
           [($fl> x 1.0)
            (make-rectangular PI/2 (acosh x))]
           [($fl< x -1.0)
            (make-rectangular (- PI/2) (- (acosh (- x))))]
           [else
            (foreign-call "ikrt_fl_asin" x)])]
        [(or (cflonum? x) (compnum? x))
         (let ([x (real-part x)] [y (imag-part x)])
           (cond
             [(= x 0) (make-rectangular 0 (asinh y))]
             [else
              (let* ([z^2 (+ (* x x) (* y y))]
                     [z^2-1 (- z^2 1.0)]
                     [z^2-1^2 (* z^2-1 z^2-1)]
                     [y^2 (* y y)]
                     [q (sqrt (+ z^2-1^2 (* 4.0 y^2)))])
                (define (sgn x) (if (< x 0) -1.0 1.0))
                (make-rectangular
                  (* 0.5 (sgn x) (acos (- q z^2)))
                  (* 0.5 (sgn y) (acosh (+ q z^2)))))]))]
        [(number? x) (asin (inexact x))]
        [else (die 'asin "not a number" x)])))

  (define acos
    (lambda (x)
      (cond
        [(flonum? x)
         (cond
           [($fl> x 1.0)
            (make-rectangular 0 (acosh x))]
           [($fl< x -1.0)
            (make-rectangular PI (- (acosh (- x))))]
           [else
            (foreign-call "ikrt_fl_acos" x)])]
        [(or (cflonum? x) (compnum? x))
         (- PI/2 (asin x))]
        [(number? x) (acos (inexact x))]
        [else (die 'acos "not a number" x)])))

  (define atan
    (case-lambda
      [(x)
       (cond
         [(flonum? x) (foreign-call "ikrt_fl_atan" x)]
         [(fixnum? x) (foreign-call "ikrt_fx_atan" x)]
         [(or (ratnum? x) (bignum? x)) (atan (inexact x))]
         [else (die 'atan "not a number" x)])]
      [(y x)
       (unless (real? x) (die 'atan "not a real number" x))
       (unless (real? y) (die 'atan "not a real number" y))
       (foreign-call "ikrt_atan2" (inexact y) (inexact x))]))

  (define sqrt
    (lambda (x)
      (cond
        [(flonum? x)
         (if ($fl< x 0.0)
             (make-rectangular 0.0
               (foreign-call "ikrt_fl_sqrt" ($fl- 0.0 x)))
             (foreign-call "ikrt_fl_sqrt" x))]
        [(fixnum? x)
         (cond
           [($fx< x 0)
            (make-rectangular 0 (sqrt (- x)))]
           [else
            (let-values ([(s r) (exact-integer-sqrt x)])
              (cond
                [(eq? r 0) s]
                [else (foreign-call "ikrt_fx_sqrt" x)]))])]
        [(bignum? x)
         (cond
           [($bignum-positive? x)
            (let-values ([(s r) (exact-integer-sqrt x)])
              (cond
                [(eq? r 0) s]
                [else
                 (let ([v (sqrt (inexact x))])
                   ;;; could the [dropped] residual ever affect the answer?
                   (cond
                     [(infinite? v)
                      (if (bignum? s)
                          (foreign-call "ikrt_bignum_to_flonum"
                             s
                             1 ;;; round up in case of a tie
                             ($make-flonum))
                          (inexact s))]
                     [else v]))]))]
           [else
            (make-rectangular 0 (sqrt (- x)))])]
        [(ratnum? x)
         ;;; FIXME: incorrect as per bug 180170
         (/ (sqrt ($ratnum-n x)) (sqrt ($ratnum-d x)))]
        [(or (compnum? x) (cflonum? x))
         (let ([xr (real-part x)] [xi (imag-part x)])
           (let ([m (sqrt (+ (* xr xr) (* xi xi)))]
                 [s (if (> xi 0) 1 -1)])
             (make-rectangular
               (sqrt (/ (+ m xr) 2))
               (* s (sqrt (/ (- m xr) 2))))))]
        [else (die 'sqrt "not a number" x)])))

  (define flsqrt
    (lambda (x)
      (if (flonum? x)
          (foreign-call "ikrt_fl_sqrt" x)
          (die 'flsqrt "not a flonum" x))))

  (define flzero?
    (lambda (x)
      (if (flonum? x)
          ($flzero? x)
          (die 'flzero? "not a flonum" x))))

  (define flnegative?
    (lambda (x)
      (if (flonum? x)
          ($fl< x 0.0)
          (die 'flnegative? "not a flonum" x))))

  (define exact-integer-sqrt
    (lambda (x)
      (define who 'exact-integer-sqrt)
      (cond
        [(fixnum? x)
         (cond
           [($fx= x 0) (values 0 0)]
           [($fx< x 0) (die who "invalid argument" x)]
           [else
            (let ([s (foreign-call "ikrt_exact_fixnum_sqrt" x)])
              (values s ($fx- x ($fx* s s))))])]
        [(bignum? x)
         (cond
           [($bignum-positive? x)
            (let ([r (foreign-call "ikrt_exact_bignum_sqrt" x)])
              (values (car r) (cdr r)))]
           [else (die who "invalid argument" x)])]
        [else (die who "invalid argument" x)])))


  (define numerator
    (lambda (x)
      (cond
        [(ratnum? x) ($ratnum-n x)]
        [(or (fixnum? x) (bignum? x)) x]
        [(flonum? x) (flnumerator x)]
        [else (die 'numerator "not an exact integer" x)])))

  (define denominator
    (lambda (x)
      (cond
        [(ratnum? x) ($ratnum-d x)]
        [(or (fixnum? x) (bignum? x)) 1]
        [(flonum? x) (fldenominator x)]
        [else (die 'denominator "not an exact integer" x)])))


  (define (floor x)
    (define (ratnum-floor x)
      (let ([n (numerator x)] [d (denominator x)])
        (let ([q (quotient n d)])
          (if (>= n 0) q (- q 1)))))
    (cond
      [(flonum? x)
       ;;; optimize for integer flonums
       (let ([e ($flonum->exact x)])
         (cond
           [(ratnum? e)
            (exact->inexact (ratnum-floor e))]
           [else x]))]
      [(ratnum? x) (ratnum-floor x)]
      [(or (fixnum? x) (bignum? x)) x]
      [else (die 'floor "not a number" x)]))

  (define (ceiling x)
    (define (ratnum-ceiling x)
      (let ([n (numerator x)] [d (denominator x)])
        (let ([q (quotient n d)])
          (if (< n 0) q (+ q 1)))))
    (cond
      [(flonum? x)
       ;;; optimize for integer flonums
       (let ([e ($flonum->exact x)])
         (cond
           [(ratnum? e) (exact->inexact (ratnum-ceiling e))]
           [else x]))]
      [(ratnum? x) (ratnum-ceiling x)]
      [(or (fixnum? x) (bignum? x)) x]
      [else (die 'ceiling "not a number" x)]))


  (define ($ratnum-round x)
    (let ([n ($ratnum-n x)] [d ($ratnum-d x)])
      (let-values ([(q r) (div-and-mod n d)])
        (let ([r2 (+ r r)])
          (cond
            [(< r2 d) q]
            [(> r2 d) (+ q 1)]
            [else (if (even? q) q (+ q 1))])))))

  (define ($ratnum-truncate x)
    (let ([n ($ratnum-n x)] [d ($ratnum-d x)])
      (quotient n d)))


  (define (round x)
    (cond
      [(flonum? x) ($flround x)]
      [(ratnum? x) ($ratnum-round x)]
      [(or (fixnum? x) (bignum? x)) x]
      [else (die 'round "not a number" x)]))

  (define (truncate x)
    ;;; FIXME: fltruncate should preserve the sign of -0.0.
    ;;;
    (cond
      [(flonum? x)
       (let ([e ($flonum->exact x)])
         (cond
           [(ratnum? e) (exact->inexact ($ratnum-truncate e))]
           [else x]))]
      [(ratnum? x) ($ratnum-truncate x)]
      [(or (fixnum? x) (bignum? x)) x]
      [else (die 'truncate "not a number" x)]))


  (define (fltruncate x)
    ;;; FIXME: fltruncate should preserve the sign of -0.0.
    (unless (flonum? x)
      (die 'fltruncate "not a flonum" x))
    (let ([v ($flonum->exact x)])
      (cond
        [(ratnum? v) (exact->inexact ($ratnum-truncate v))]
        [else x])))

  (define log
    (case-lambda
      [(x)
       (cond
         [(fixnum? x)
          (cond
            [($fx= x 1) 0]
            [($fx= x 0) (die 'log "undefined around 0")]
            [($fx> x 0) (foreign-call "ikrt_fx_log" x)]
            [else (make-rectangular (log (- x)) (acos -1))])]
         [(flonum? x)
          (cond
	   ((nan? x)	  +nan.0)
	   [(fl>=? x 0.0) (foreign-call "ikrt_fl_log" x)]
	   [else
	    (make-rectangular
	     (log (fl- 0.0 x))
	     (acos -1))])]
         [(bignum? x)
          (if ($bignum-positive? x)
              (let ([v (log (inexact x))])
                (cond
                  [(infinite? v)
                   (let-values ([(s r) (exact-integer-sqrt x)])
                     ;;; could the [dropped] residual ever affect the answer?
                     (fl* 2.0 (log s)))]
                  [else v]))
              (make-rectangular (log (- x)) (acos -1)))]
         [(ratnum? x)
          ;;; FIXME: incorrect as per bug 180170
          (- (log (numerator x)) (log (denominator x)))]
         [(or (compnum? x) (cflonum? x))
	  (let ([xr (real-part x)] [xi (imag-part x)])
	    (make-rectangular
	     (/ (log (+ (* xr xr) (* xi xi))) 2)
	     (atan xi xr)))]
         [else (die 'log "not a number" x)])]
      [(x y)
       (let ([ly (log y)])
         (if (eqv? ly 0)
             (die 'log "invalid arguments" x y)
             (/ (log x) ly)))]))

  (define (random n)
    (if (fixnum? n)
        (if (fx> n 1)
            (foreign-call "ikrt_fxrandom" n)
            (if (fx= n 1)
                0
                (die 'random "incorrect argument" n)))
        (die 'random "not a fixnum" n)))


  (define (shift-right-arithmetic n m who)
    (cond
      [(fixnum? m)
       (cond
         [(fixnum? n)
          (cond
            [($fx>= m 0) ($fxsra n m)]
            [else (die who "offset must be non-negative" m)])]
         [(bignum? n)
          (cond
            [($fx> m 0)
             (foreign-call "ikrt_bignum_shift_right" n m)]
            [($fx= m 0) n]
            [else (die who "offset must be non-negative" m)])]
         [else (die who "not an exact integer" n)])]
      [(bignum? m)
       (cond
         [(fixnum? n) (if ($fx>= n 0) 0 -1)]
         [(bignum? n) (if ($bignum-positive? n) 0 -1)]
         [else (die who "not an exact integer" n)])]
      [else (die who "not an exact integer offset" m)]))

  (define (sra n m)
    (shift-right-arithmetic n m 'sra))

  (define (shift-left-logical n m who)
    (unless (fixnum? m)
      (die who "shift amount is not a fixnum"))
    (cond
      [(fixnum? n)
       (cond
         [($fx> m 0)
          (foreign-call "ikrt_fixnum_shift_left" n m)]
         [($fx= m 0) n]
         [else (die who "offset must be non-negative" m)])]
      [(bignum? n)
       (cond
         [($fx> m 0)
          (foreign-call "ikrt_bignum_shift_left" n m)]
         [($fx= m 0) n]
         [else (die who "offset must be non-negative" m)])]
      [else (die who "not an exact integer" n)]))

  (define (sll n m)
    (shift-left-logical n m 'sll))

  (define (bitwise-arithmetic-shift-right n m)
    (shift-right-arithmetic n m 'bitwise-arithmetic-shift-right))
  (define (bitwise-arithmetic-shift-left n m)
    (shift-left-logical n m 'bitwise-arithmetic-shift-left))
  (define (bitwise-arithmetic-shift n m)
    (define who 'bitwise-arithmetic-shift)
    (unless (fixnum? m)
      (die who "shift amount is not a fixnum"))
    (cond
      [(fixnum? n)
       (cond
         [($fx> m 0)
          (foreign-call "ikrt_fixnum_shift_left" n m)]
         [($fx= m 0) n]
         [else
          (let ([m^ (- m)])
            (unless (fixnum? m^)
              (die who "shift amount is too big" m))
            ($fxsra n m^))])]
      [(bignum? n)
       (cond
         [($fx> m 0)
          (foreign-call "ikrt_bignum_shift_left" n m)]
         [($fx= m 0) n]
         [else
          (let ([m^ (- m)])
            (unless (fixnum? m^)
              (die who "shift amount is too big" m))
            (foreign-call "ikrt_bignum_shift_right" n m^))])]
      [else (die who "not an exact integer" n)]))

  (define (exp x)
    (cond
      [(flonum? x) (flexp x)]
      [(fixnum? x)
       (if ($fx= x 0) 1 (flexp (fixnum->flonum x)))]
      [(bignum? x) (flexp (bignum->flonum x))]
      [(ratnum? x) (flexp (ratnum->flonum x))]
      [(or (compnum? x) (cflonum? x))
       ;;In general:
       ;;
       ;;   e^x = e^(xr + xi i)
       ;;       = e^xr cos(xi) + e^xr sin(xi) i
       ;;
       ;;but the special case xi=0.0 is handled as:
       ;;
       ;;   e^(xr+0.0i) = e^xr * e^(0.0 i) = e^xr * 1.0+0.0i
       ;;
       ;;else,  in  the  special  case  xr=+inf.0,  the  imaginary  part
       ;;becomes:
       ;;
       ;;   e^xr * sin(xi) * i = +inf.0 * 0.0 * i = +nan.0 * i
       ;;
       (let ([xr (real-part x)] [xi (imag-part x)])
	 (if (zero? xi)
	     (make-rectangular (inexact (exp xr)) 0.0) ;equivalent to: (* (exp xr) 1.0+0.0i)
	   (let ([e^xr (exp xr)])
	     (make-rectangular
	      (* e^xr (cos xi))
	      (* e^xr (sin xi))))))]
      [else (die 'exp "not a number" x)]))

  (define (bitwise-length n)
    (cond
      [(fixnum? n) (fxlength n)]
      [(bignum? n) (foreign-call "ikrt_bignum_length" n)]
      [else (die 'bitwise-length "not an exact integer" n)]))

  (define (bitwise-copy-bit n idx bit)
    (define who 'bitwise-copy-bit)
    (define (do-copy-bit n idx bit)
      (case bit
        [(0)
         (cond
           [(bitwise-bit-set? n idx)
            (bitwise-and n (bitwise-not (sll 1 idx)))]
           [else n])]
        [(1)
         (cond
           [(bitwise-bit-set? n idx) n]
           [(>= n 0) (+ n (sll 1 idx))]
           [else
            (bitwise-not
              (bitwise-and
                (bitwise-not n)
                (bitwise-not (sll 1 idx))))])]
        [else (die who "bit must be either 0 or 1" bit)]))
    (cond
      [(fixnum? idx)
       (cond
         [(fx< idx 0)
          (die who "negative bit index" idx)]
         [(or (fixnum? n) (bignum? n))
          (do-copy-bit n idx bit)]
         [else (die who "not an exact integer" n)])]
      [(bignum? idx)
       (unless (or (fixnum? n) (bignum? n))
         (die who "not an exact integer" n))
       (if ($bignum-positive? idx)
           (case bit
             [(0)
              (if (>= n 0)
                  n
                  (die who "unrepresentable result"))]
             [(1)
              (if (< n 0)
                  n
                  (die who "unrepresentable result"))]
             [else (die who "bit must be either 0 or 1" bit)])
           (die who "negative bit index" idx))]
      [else (die who "index is not an exact integer" idx)]))

  (define (bitwise-bit-field n idx1 idx2)
    (define who 'bitwise-bit-field)
    (cond
      [(and (fixnum? idx1) (fx>= idx1 0))
       (cond
         [(and (fixnum? idx2) (fx>= idx2 0))
          (cond
            [(fx<= idx1 idx2)
             (cond
               [(or (fixnum? n) (bignum? n))
                (bitwise-and
                   (sra n idx1)
                   (- (sll 1 (- idx2 idx1)) 1))]
               [else (die who "not an exact integer" n)])]
            [else (die who "invalid order for indices" idx1 idx2)])]
         [else
          (if (not (fixnum? idx2))
              (die who "invalid index" idx2)
              (die who "negative index" idx2))])]
      [else
       (if (not (fixnum? idx1))
           (die who "invalid index" idx1)
           (die who "negative index" idx1))]))

  )




(library (ikarus flonum-conversion)
  (export string->flonum flonum->string)
  (import
    (rnrs bytevectors)
    (ikarus system $bytevectors)
    (ikarus system $flonums)
    (except (ikarus) flonum->string string->flonum ))

  (module (flonum->string)
    (module (flonum->digits)
      (define flonum->digits
        (lambda (f e min-e p b B)
          ;;; flonum v = f * b^e
          ;;; p = precision  (p >= 1)
          (let ([round? (even? f)])
            (if (>= e 0)
                (if (not (= f (expt b (- p 1))))
                    (let ([be (expt b e)])
                      (scale (* f be 2) 2 be be 0 B round? f e))
                    (let* ([be (expt b e)] [be1 (* be b)])
                      (scale (* f be1 2) (* b 2) be1 be 0 B round? f e)))
                (if (or (= e min-e) (not (= f (expt b (- p 1)))))
                    (scale (* f 2) (* (expt b (- e)) 2) 1 1 0 B round? f e)
                    (scale (* f b 2) (* (expt b (- 1 e)) 2) b 1 0 B round? f e))))))
      (define (len n)
        (let f ([n n] [i 0])
          (cond
            [(zero? n) i]
            [else (f (quotient n 2) (+ i 1))])))
      (define scale
        (lambda (r s m+ m- k B round? f e)
          (let ([est (inexact->exact
                       (ceiling
                         (- (* (+ e (len f) -1) (invlog2of B))
                            1e-10)))])
            (if (>= est 0)
                (fixup r (* s (exptt B est)) m+ m- est B round?)
                (let ([scale (exptt B (- est))])
                  (fixup (* r scale) s (* m+ scale) (* m- scale) est B round?))))))
      (define fixup
        (lambda (r s m+ m- k B round?)
          (if ((if round? >= >) (+ r m+) s) ; too low?
              (values (+ k 1) (generate r s m+ m- B round?))
              (values k (generate (* r B) s (* m+ B) (* m- B) B round?)))))
      (define (chr x)
        (vector-ref '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) x))
      (define generate
        (lambda (r s m+ m- B round?)
          (let-values ([(q r) (quotient+remainder r s)])
            (let ([tc1 ((if round? <= <) r m-)]
                  [tc2 ((if round? >= >) (+ r m+) s)])
              (if (not tc1)
                  (if (not tc2)
                      (cons (chr q) (generate (* r B) s (* m+ B) (* m- B) B round?))
                      (list (chr (+ q 1))))
                  (if (not tc2)
                      (list (chr q))
                      (if (< (* r 2) s)
                          (list (chr q))
                          (list (chr (+ q 1))))))))))
      (define invlog2of
        (let ([table (make-vector 37)]
              [log2 (log 2)])
          (do ([B 2 (+ B 1)])
              ((= B 37))
            (vector-set! table B (/ log2 (log B))))
          (lambda (B)
            (if (<= 2 B 36)
                (vector-ref table B)
                (/ log2 (log B))))))
      (define exptt
        (let ([table (make-vector 326)])
          (do ([k 0 (+ k 1)] [v 1 (* v 10)])
              ((= k 326))
              (vector-set! table k v))
          (lambda (B k)
            (if (and (= B 10) (<= 0 k 325))
                (vector-ref table k)
                (expt B k))))))
    (define (format-flonum pos? expt digits)
      (define (next x)
        (if (null? x)
            (values #\0 '())
            (values (car x) (cdr x))))
      (define (format-flonum-no-expt expt d0 d*)
        (cond
          [(= expt 1)
           (cons d0 (if (null? d*) '(#\. #\0) (cons #\. d*)))]
          [else
           (cons d0
             (let-values ([(d0 d*) (next d*)])
               (format-flonum-no-expt (- expt 1) d0 d*)))]))
      (define (format-flonum-no-expt/neg expt d*)
        (cond
          [(= expt 0) d*]
          [else (cons #\0 (format-flonum-no-expt/neg (+ expt 1) d*))]))
      (define (sign pos? ls)
        (if pos?
           (list->string ls)
           (list->string (cons #\- ls))))
      (let ([d0 (car digits)] [d* (cdr digits)])
        (cond
          [(null? d*)
           (if (char=? d0 #\0)
               (if pos? "0.0" "-0.0")
               (if (= expt 1)
                   (if pos?
                       (string d0 #\. #\0)
                       (string #\- d0 #\. #\0))
                   (if (= expt 0)
                       (if pos?
                           (string #\0 #\. d0)
                           (string #\- #\0 #\. d0))
                       (string-append
                         (if pos? "" "-")
                         (string d0) "e" (fixnum->string (- expt 1))))))]
          [(and (null? d*) (char=? d0 #\0)) (if pos? "0.0" "-0.0")]
          [(<= 1 expt 9)
           (sign pos? (format-flonum-no-expt expt d0 d*))]
          [(<= -3 expt 0)
           (sign pos? (cons* #\0 #\. (format-flonum-no-expt/neg expt digits)))]
          [else
           (string-append
             (if pos? "" "-")
             (string d0) "." (list->string d*)
             "e" (fixnum->string (- expt 1)))])))
    (define (flo->string pos? m e p)
      (let-values ([(expt digits) (flonum->digits m e 10 p 2 10)])
         (format-flonum pos? expt digits)))
    (define (flonum->string x)
      (let-values ([(pos? be m) (flonum-parts x)])
        (cond
          [(<= 1 be 2046) ; normalized flonum
           (flo->string pos? (+ m (expt 2 52)) (- be 1075) 53)]
          [(= be 0)
           (flo->string pos? m -1074 52)]
          [(= be 2047)
           (if (= m 0)
               (if pos? "+inf.0" "-inf.0")
               ;;; Gee!  nans have no sign!
               "+nan.0")]
          [else (die 'flonum->string "cannot happen")]))))
  ;;;
  (define (string->flonum x)
    (cond
      [(string? x)
       (foreign-call "ikrt_bytevector_to_flonum"
         (string->utf8 x))]
      [else
       (die 'string->flonum "not a string" x)])) )

(library (ikarus rationalize)
  (export rationalize)
  (import
    (except (ikarus) rationalize))

  (define (rationalize x eps)
    (define who 'rationalize)
    (define (simplest x y)
      (cond
        [(< y x) (simplest y x)]
        [(= x y) x]
        [(> x 0)
         (let ([n (numerator x)] [d (denominator x)]
               [n^ (numerator y)] [d^ (denominator y)])
           (simplest^ n d n^ d^))]
        [(< y 0)
         (let ([n (numerator x)] [d (denominator x)]
               [n^ (numerator y)] [d^ (denominator y)])
           (- (simplest^ (- n^) d^ (- n) d)))]
        [else 0]))
    (define (simplest^ n d n^ d^)
      (let-values ([(q r) (div-and-mod n d)])
        (if (= r 0)
            q
            (let-values ([(q^ r^) (div-and-mod n^ d^)])
              (if (= q q^)
                  (let ([v (simplest^ d^ r^ d r)])
                    (let ([n^^ (numerator v)] [d^^ (denominator v)])
                      (/ (+ (* q n^^) d^^) n^^)))
                  (+ q 1))))))
    (define (go x eps)
      (simplest (- x eps) (+ x eps)))
    (cond
      [(flonum? x)
       (if (flfinite? x)
           (cond
             [(flonum? eps)
              (if (flfinite? eps) (go x eps) +0.0)]
             [(or (fixnum? eps) (bignum? eps) (ratnum? eps))
              (go x eps)]
             [else (die who "not a number" eps)])
           (cond
             [(flonum? eps)
              (if (flfinite? eps) x +nan.0)]
             [(or (fixnum? eps) (bignum? eps) (ratnum? eps))
              x]
             [else (die who "not a number" eps)]))]
      [(or (fixnum? x) (bignum? x) (ratnum? x))
       (cond
         [(flonum? eps)
          (if (flfinite? eps) (go x eps) +0.0)]
         [(or (fixnum? eps) (bignum? eps) (ratnum? eps))
          (go x eps)]
         [else (die who "not a number" eps)])]
      [else (die who "not a number" x)])))


(library (ikarus r6rs-fu div/mod)
  (export div mod div-and-mod div0 mod0 div0-and-mod0)
  (import
    (except (ikarus)
      div mod div-and-mod div0 mod0 div0-and-mod0))

  (define (div-and-mod* n m who)
    (import (ikarus system $fx)
            (only (ikarus system $flonums) $fl=)
            (ikarus flonums))
    (define (int-div-and-mod n m)
      (let ([d0 (quotient n m)])
        (let ([m0 (- n (* d0 m))])
          (if (>= m0 0)
              (values d0 m0)
              (if (>= m 0)
                  (values (- d0 1) (+ m0 m))
                  (values (+ d0 1) (- m0 m)))))))
    (define (rat-div-and-mod n m)
      (let ([x (/ n m)])
        (cond
          [(or (fixnum? x) (bignum? x))
           (values x 0)]
          [else
           (let ([n0 (numerator x)] [d0 (denominator x)])
             (let ([q (quotient n0 d0)])
               (let ([r (- n (* q m))])
                 (if (>= r 0)
                     (values q r)
                     (if (> m 0)
                         (values (- q 1) (+ r m))
                         (values (+ q 1) (- r m)))))))])))
    (cond
      [(fixnum? m)
       (cond
         [($fx= m 0)
          (die who "division by 0")]
         [(or (fixnum? n) (bignum? n))
          (int-div-and-mod n m)]
         [(flonum? n)
          (fldiv-and-mod n (fixnum->flonum m))]
         [(ratnum? n)
          (rat-div-and-mod n m)]
         [else (die who "not a number" n)])]
      [(bignum? m)
       (cond
         [(or (fixnum? n) (bignum? n))
          (int-div-and-mod n m)]
         [(flonum? n)
          (let ([v ($flonum->exact n)])
            (unless v
              (die who "invalid argument" n))
            (let-values ([(a b) (div-and-mod* v m who)])
              (values (inexact a) (inexact b))))]
         [(ratnum? n)
          (rat-div-and-mod n m)]
         [else (die who "not a number" n)])]
      [(ratnum? m)
       (cond
         [(or (fixnum? n) (bignum? n) (ratnum? n))
          (rat-div-and-mod n m)]
         [(flonum? n)
          (let ([v ($flonum->exact n)])
            (unless v
              (die who "invalid argument" n))
            (let-values ([(a b) (div-and-mod* v m who)])
              (values (inexact a) (inexact b))))]
         [else (die who "not a number" n)])]
      [(flonum? m)
       (cond
         [($fl= m 0.0)
          (die who "division by 0.0")]
         [(flonum? n) (fldiv-and-mod n m)]
         [(fixnum? n)
          (fldiv-and-mod (fixnum->flonum n) m)]
         [(or (bignum? n) (ratnum? n))
          (let ([v ($flonum->exact m)])
            (unless v
              (die who "invalid argument" m))
            (let-values ([(a b) (div-and-mod* n v who)])
              (values (inexact a) (inexact b))))]
         [else (die who "not a number" n)])]
      [else (die who "not a number" m)]))

  (define (div-and-mod n m)
    (div-and-mod* n m 'div-and-mod))

  (define (div n m)
    (import (ikarus system $fx))
    (cond
      [(and (fixnum? n) (fixnum? m))
       (cond
         [(eq? m 0) (die 'div "division by 0")]
         [(eq? m -1) (- n)]
         [else
          (let ([d0 ($fxquotient n m)])
            (if ($fx>= n ($fx* d0 m))
                d0
                (if ($fx>= m 0)
                    ($fx- d0 1)
                    ($fx+ d0 1))))])]
      [else
       (let-values ([(a b) (div-and-mod* n m 'div)])
         a)]))

  (define (mod n m)
    (import (ikarus system $fx))
    (cond
      [(and (fixnum? n) (fixnum? m))
       (cond
         [(eq? m 0) (die 'mod "division by 0")]
         [else
          (let ([d0 ($fxquotient n m)])
            (let ([m0 ($fx- n ($fx* d0 m))])
              (if ($fx>= m0 0)
                  m0
                  (if ($fx>= m 0)
                      ($fx+ m0 m)
                      ($fx- m0 m)))))])]
      [else
       (let-values ([(a b) (div-and-mod* n m 'mod)])
         b)]))

  (define (div0-and-mod0 x y)
    (let-values ([(d m) (div-and-mod* x y 'div0-and-mod0)])
      (if (> y 0)
          (if (< m (/ y 2))
              (values d m)
              (values (+ d 1) (- m y)))
          (if (>= m (/ y -2))
              (values (- d 1) (+ m y))
              (values d m)))))

  (define (div0 x y)
    (let-values ([(d m) (div-and-mod* x y 'div0)])
      (if (> y 0)
          (if (< m (/ y 2))
              d
              (+ d 1))
          (if (>= m (/ y -2))
              (- d 1)
              d))))

  (define (mod0 x y)
    (let-values ([(d m) (div-and-mod* x y 'mod0)])
      (if (> y 0)
          (if (< m (/ y 2))
              m
              (- m y))
          (if (>= m (/ y -2))
              (+ m y)
              m))))
  )

(library (ikarus flonums div-and-mod)
  (export fldiv flmod fldiv-and-mod fldiv0 flmod0 fldiv0-and-mod0)
  (import
    (ikarus system $flonums)
    (ikarus system $fx)
    (except (ikarus)
      fldiv flmod fldiv-and-mod fldiv0 flmod0 fldiv0-and-mod0))

  (define ($flmod n m)
    (let ([d0 (fltruncate ($fl/ n m))])
      (let ([m0 ($fl- n ($fl* d0 m))])
        (if ($fl>= m0 0.0)
            m0
            (if ($fl>= m 0.0)
                ($fl+ m0 m)
                ($fl- m0 m))))))

  (define ($fldiv n m)
    (let ([d0 (fltruncate ($fl/ n m))])
      (if ($fl>= n ($fl* d0 m))
          d0
          (if ($fl>= m 0.0)
              ($fl- d0 1.0)
              ($fl+ d0 1.0)))))

  (define ($fldiv-and-mod n m)
    (let ([d0 (fltruncate ($fl/ n m))])
      (let ([m0 ($fl- n ($fl* d0 m))])
        (if ($fl>= m0 0.0)
            (values d0 m0)
            (if ($fl>= m 0.0)
                (values ($fl- d0 1.0) ($fl+ m0 m))
                (values ($fl+ d0 1.0) ($fl- m0 m)))))))

  (define (fldiv n m)
    (if (flonum? n)
        (if (flonum? m)
            ($fldiv n m)
            (die 'fldiv "not a flonum" m))
        (die 'fldiv "not a flonum" n)))

  (define (flmod n m)
    (if (flonum? n)
        (if (flonum? m)
            ($flmod n m)
            (die 'flmod "not a flonum" m))
        (die 'flmod "not a flonum" n)))

  (define (fldiv-and-mod n m)
    (if (flonum? n)
        (if (flonum? m)
            ($fldiv-and-mod n m)
            (die 'fldiv-and-mod "not a flonum" m))
        (die 'fldiv-and-mod "not a flonum" n)))

  (define ($fldiv0-and-mod0 n m)
    (let ([d0 (fltruncate ($fl/ n m))])
      (let ([m0 ($fl- n ($fl* d0 m))])
        (if ($fl>= m 0.0)
            (if ($fl< m0 ($fl/ m 2.0))
                (if ($fl>= m0 ($fl/ m -2.0))
                    (values d0 m0)
                    (values ($fl- d0 1.0) ($fl+ m0 m)))
                (values ($fl+ d0 1.0) ($fl- m0 m)))
            (if ($fl< m0 ($fl/ m -2.0))
                (if ($fl>= m0 ($fl/ m 2.0))
                    (values d0 m0)
                    (values ($fl+ d0 1.0) ($fl- m0 m)))
                (values ($fl- d0 1.0) ($fl+ m0 m)))))))

  (define ($fldiv0 n m)
    (let ([d0 (fltruncate ($fl/ n m))])
      (let ([m0 ($fl- n ($fl* d0 m))])
        (if ($fl>= m 0.0)
            (if ($fl< m0 ($fl/ m 2.0))
                (if ($fl>= m0 ($fl/ m -2.0))
                    d0
                    ($fl- d0 1.0))
                ($fl+ d0 1.0))
            (if ($fl< m0 ($fl/ m -2.0))
                (if ($fl>= m0 ($fl/ m 2.0))
                    d0
                    ($fl+ d0 1.0))
                ($fl- d0 1.0))))))

  (define ($flmod0 n m)
    (let ([d0 (fltruncate ($fl/ n m))])
      (let ([m0 ($fl- n ($fl* d0 m))])
        (if ($fl>= m 0.0)
            (if ($fl< m0 ($fl/ m 2.0))
                (if ($fl>= m0 ($fl/ m -2.0))
                    m0
                    ($fl+ m0 m))
                ($fl- m0 m))
            (if ($fl< m0 ($fl/ m -2.0))
                (if ($fl>= m0 ($fl/ m 2.0))
                    m0
                    ($fl- m0 m))
                ($fl+ m0 m))))))

  (define (fldiv0 n m)
    (if (flonum? n)
        (if (flonum? m)
            ($fldiv0 n m)
            (die 'fldiv0 "not a flonum" m))
        (die 'fldiv0 "not a flonum" n)))

  (define (flmod0 n m)
    (if (flonum? n)
        (if (flonum? m)
            ($flmod0 n m)
            (die 'flmod0 "not a flonum" m))
        (die 'flmod0 "not a flonum" n)))

  (define (fldiv0-and-mod0 n m)
    (if (flonum? n)
        (if (flonum? m)
            ($fldiv0-and-mod0 n m)
            (die 'fldiv0-and-mod0 "not a flonum" m))
        (die 'fldiv0-and-mod0 "not a flonum" n))))

(library (ikarus bitwise misc)
  (export fxfirst-bit-set bitwise-bit-set? bitwise-first-bit-set
          fxbit-count bitwise-bit-count
          fxlength
          fxbit-set?
          fxcopy-bit
          fxcopy-bit-field fxrotate-bit-field fxreverse-bit-field
          fxbit-field)
  (import
    (ikarus system $fx)
    (ikarus system $bignums)
    (ikarus system $flonums)
    (except (ikarus)
      fxfirst-bit-set bitwise-bit-set? bitwise-first-bit-set
      fxbit-count bitwise-bit-count
      fxlength
      fxbit-set?
      fxcopy-bit
      fxcopy-bit-field fxrotate-bit-field fxreverse-bit-field
      fxbit-field))

  (module (bitwise-first-bit-set fxfirst-bit-set)
    (define (byte-first-bit-set x i)
      (import (ikarus system $bytevectors))
      (define-syntax make-first-bit-set-bytevector
        (lambda (x)
          (define (fst n)
            (cond
              [(odd? n) 0]
              [else (+ 1 (fst (bitwise-arithmetic-shift-right n 1)))]))
          (u8-list->bytevector
            (cons 0 #| not used |#
              (let f ([i 1])
                (cond
                  [(= i 256) '()]
                  [else (cons (fst i) (f (+ i 1)))]))))))
      (define bv (make-first-bit-set-bytevector))
      ($fx+ i ($bytevector-u8-ref bv x)))
    (define ($fxloop x i)
      (let ([y ($fxlogand x 255)])
        (if ($fx= y 0)
            ($fxloop ($fxsra x 8) ($fx+ i 8))
            (byte-first-bit-set y i))))
    (define ($bnloop x i idx)
      (let ([b ($bignum-byte-ref x idx)])
        (if ($fxzero? b)
            ($bnloop x ($fx+ i 8) ($fx+ idx 1))
            (byte-first-bit-set b i))))
    (define ($fxfirst-bit-set x)
      (if ($fx> x 0)
          ($fxloop x 0)
          (if ($fx= x 0)
              -1
              (if ($fx> x (least-fixnum))
                  ($fxloop ($fx- 0 x) 0)
                  ($bnloop (- x) 0 0)))))
    (define (fxfirst-bit-set x)
      (cond
        [(fixnum? x)
         ($fxfirst-bit-set x)]
        [else (die 'fxfirst-bit-set "not a fixnum" x)]))
    (define (bitwise-first-bit-set x)
      (cond
        [(fixnum? x)
         ($fxfirst-bit-set x)]
        [(bignum? x) ($bnloop x 0 0)]
        [else (die 'bitwise-first-bit-set "not an exact integer" x)])))

  (module (fxbit-count bitwise-bit-count)
    (define (pos-fxbitcount n)
      ;;; nifty parrallel count from:
      ;;; http://infolab.stanford.edu/~manku/bitcount/bitcount.html
      (case (fixnum-width)
        [(30)
         (let ([m0 #x15555555]
               [m1 #x13333333]
               [m2 #x0f0f0f0f])
           (let* ([n ($fx+ ($fxlogand n m0) ($fxlogand ($fxsra n 1) m0))]
                  [n ($fx+ ($fxlogand n m1) ($fxlogand ($fxsra n 2) m1))]
                  [n ($fx+ ($fxlogand n m2) ($fxlogand ($fxsra n 4) m2))])
             ($fxmodulo n 255)))]
        [else
         (let ([m0 #x0555555555555555]
               [m1 #x0333333333333333]
               [m2 #x0f0f0f0f0f0f0f0f]
               [m3 #x00ff00ff00ff00ff])
           (let* ([n ($fx+ ($fxlogand n m0) ($fxlogand ($fxsra n 1) m0))]
                  [n ($fx+ ($fxlogand n m1) ($fxlogand ($fxsra n 2) m1))]
                  [n ($fx+ ($fxlogand n m2) ($fxlogand ($fxsra n 4) m2))]
                  [n ($fx+ ($fxlogand n m3) ($fxlogand ($fxsra n 8) m3))])
             ($fxmodulo n 255)))]))
    (define ($fxbitcount n)
      (if ($fx< n 0)
          (fxlognot (pos-fxbitcount (fxlognot n)))
          (pos-fxbitcount n)))
    (define (bnbitcount n)
      (define (poscount x idx c)
        (let ([c (+ c
                    ($fx+ (pos-fxbitcount
                            ($fxlogor
                              ($fxsll ($bignum-byte-ref x ($fx+ idx 3)) 8)
                              ($bignum-byte-ref x ($fx+ idx 2))))
                          (pos-fxbitcount
                            ($fxlogor
                              ($fxsll ($bignum-byte-ref x ($fxadd1 idx)) 8)
                              ($bignum-byte-ref x idx)))))])
          (if ($fx= idx 0)
              c
              (poscount x ($fx- idx 4) c))))
      (if ($bignum-positive? n)
          (poscount n ($fx- ($bignum-size n) 4) 0)
          (let ([n (bitwise-not n)])
            (bitwise-not (poscount n ($fx- ($bignum-size n) 4) 0)))))
    (define (fxbit-count n)
      (cond
        [(fixnum? n) ($fxbitcount n)]
        [else (die 'fxbit-count "not a fixnum" n)]))
    (define (bitwise-bit-count n)
      (cond
        [(fixnum? n) ($fxbitcount n)]
        [(bignum? n) (bnbitcount n)]
        [else (die 'bitwise-bit-count "not an exact integer" n)])))

  (define (fxlength x)
    (define (fxlength32 x)
      (let ([fl ($fixnum->flonum x)])
         (let ([sbe ($fxlogor
                      ($fxsll ($flonum-u8-ref fl 0) 4)
                      ($fxsra ($flonum-u8-ref fl 1) 4))])
           (cond
             [($fx= sbe 0) 0]
             [else ($fx- sbe 1022)]))))
    (define (fxlength64 x)
      (if ($fx> x #x7FFFFFFF)
          ($fx+ 31 (fxlength32 ($fxsra x 31)))
          (fxlength32 x)))
    (if (fixnum? x)
        (case (fixnum-width)
          [(30)
           (fxlength32 (if ($fx< x 0) ($fxlognot x) x))]
          [else
           (fxlength64 (if ($fx< x 0) ($fxlognot x) x))])
        (die 'fxlength "not a fixnum" x)))

  (define (fxbit-set? x i)
    (define who 'fxbit-set?)
    (if (fixnum? x)
        (if (fixnum? i)
            (if (and ($fx<= 0 i) ($fx< i (fixnum-width)))
                (not ($fxzero? ($fxlogand ($fxsra x i) 1)))
                (die who "index out of range" i))
            (die who "index is not a fixnum" i))
        (die who "not a fixnum" x)))

  (define (bitwise-bit-set? x i)
    (define who 'bitwise-bit-set?)
    (cond
      [(fixnum? i)
       (when ($fx< i 0)
         (die who "index must be non-negative" i))
       (cond
         [(fixnum? x)
          (if ($fx< i (fixnum-width))
              ($fx= ($fxlogand ($fxsra x i) 1) 1)
              ($fx< x 0))]
         [(bignum? x)
          (let ([n ($bignum-size x)])
            (let ([m ($fx* n 8)])
              (if ($fx< m i)
                  (not ($bignum-positive? x))
                  (if ($bignum-positive? x)
                      (let ([b ($bignum-byte-ref x ($fxsra i 3))])
                        ($fx= ($fxlogand ($fxsra b ($fxlogand i 7)) 1) 1))
                      (= 1 (bitwise-and
                             (bitwise-arithmetic-shift-right x i)
                             1))))))]
         [else (die who "not an exact integer" x)])]
      [(bignum? i)
       (unless ($bignum-positive? i)
         (die who "index must be non-negative"))
       (cond
         [(fixnum? x) ($fx< x 0)]
         [(bignum? x)
          (= 1 (bitwise-and (bitwise-arithmetic-shift-right x i) 1))]
         [else (die who "not an exact integer" x)])]
      [else
       (die who "index is not an exact integer" i)]))


  (define (fxcopy-bit x i b)
    (define who 'fxcopy-bit)
    (if (fixnum? x)
        (if (fixnum? i)
            (if (and ($fx<= 0 i) ($fx< i (fixnum-width)))
                (case b
                  [(0) ($fxlogand x ($fxlognot ($fxsll 1 i)))]
                  [(1) ($fxlogor x ($fxsll 1 i))]
                  [else (die who "invalid bit value" b)])
                (die who "index out of range" i))
            (die who "index is not a fixnum" i))
        (die who "not a fixnum" x)))

  (define (fxcopy-bit-field x i j b)
    (define who 'fxcopy-bit-field)
    (if (fixnum? x)
        (if (fixnum? i)
            (if ($fx<= 0 i)
                (if (fixnum? j)
                    (if ($fx< j (fixnum-width))
                        (if ($fx<= i j)
                            (if (fixnum? b)
                                (let ([m
                                       ($fxlogxor
                                         ($fxsub1 ($fxsll 1 i))
                                         ($fxsub1 ($fxsll 1 j)))])
                                  ($fxlogor
                                    ($fxlogand m ($fxsll b i))
                                    ($fxlogand ($fxlognot m) x)))
                                (die who "not a fixnum" b))
                            (if ($fx<= 0 j)
                                (die who "index out of range" j)
                                (die who "indices not in order" i j)))
                        (die who "index out of range" j))
                    (die who "not a fixnum" j))
                (die who "index out of range" i))
            (die who "not a fixnum" i))
        (die who "not a fixnum" x)))

  (define ($fxrotate-bit-field x i j c w)
    (let ([m ($fxsll ($fxsub1 ($fxsll 1 w)) i)])
      (let ([x0 ($fxlogand x m)])
        (let ([lt ($fxsll x0 c)] [rt ($fxsra x0 ($fx- w c))])
          (let ([x0 ($fxlogand ($fxlogor lt rt) m)])
            ($fxlogor x0 ($fxlogand x ($fxlognot m))))))))

  (define (fxrotate-bit-field x i j c)
    (define who 'fxrotate-bit-field)
    (if (fixnum? x)
        (if (fixnum? i)
            (if ($fx>= i 0)
                (if (fixnum? j)
                    (if ($fx< j (fixnum-width))
                        (let ([w ($fx- j i)])
                          (if ($fx>= w 0)
                              (if (fixnum? c)
                                  (if (and ($fx>= c 0) ($fx< c w))
                                      ($fxrotate-bit-field x i j c w)
                                      (die who "count is invalid" c))
                                  (die who "count is not a fixnum" c))
                              (die who "field width is negative" i j)))
                        (die who "end index is out of range" j))
                    (die who "end index is not a fixnum" j))
                (die who "start index is out of range" i))
            (die who "start index is not a fixnum" i))
        (die who "not a fixnum" x)))

  (define (fxreverse-bit-field v start end)

    ;;This is from  the original patch by Göran  Weinholt, posted on the
    ;;Ikarus bug tracker.
    ;;
    ;; (define (%fxreverse-bit-field61 v)
    ;;   ;; Based on <http://aggregate.org/MAGIC/#Bit Reversal>.
    ;;   (assert (= (fixnum-width) 61))
    ;;   (let* ( ;; Swap pairs of bits
    ;; 	     (v (fxior (fxarithmetic-shift-right
    ;; 			(fxand v #b101010101010101010101010101010101010101010101010101010101010) 1)
    ;; 		       (fxarithmetic-shift-left
    ;; 			(fxand v #b010101010101010101010101010101010101010101010101010101010101) 1)))
    ;; 	     ;; Swap 2-bit fields
    ;; 	     (v (fxior (fxarithmetic-shift-right
    ;; 			(fxand v #b110011001100110011001100110011001100110011001100110011001100) 2)
    ;; 		       (fxarithmetic-shift-left
    ;; 			(fxand v #b001100110011001100110011001100110011001100110011001100110011) 2)))
    ;; 	     ;; Swap 4-bit fields
    ;; 	     (tmp1     (fxarithmetic-shift-right
    ;; 			(fxand v #b111100000000000000000000000000000000000000000000000000000000) 56))
    ;; 	     (v (fxior (fxarithmetic-shift-right
    ;; 			(fxand v #b000011110000111100001111000011110000111100001111000011110000) 4)
    ;; 		       (fxarithmetic-shift-left
    ;; 			(fxand v #b000000001111000011110000111100001111000011110000111100001111) 4)))
    ;; 	     ;; Swap bytes
    ;; 	     (tmp2     (fxarithmetic-shift-right
    ;; 			(fxand v #b000011111111000000000000000000000000000000000000000000000000) 44))
    ;; 	     (v (fxior (fxarithmetic-shift-right
    ;; 			(fxand v #b111100000000111111110000000011111111000000001111111100000000) 8)
    ;; 		       (fxarithmetic-shift-left
    ;; 			(fxand v #b000000000000000000001111111100000000111111110000000011111111) 8)))
    ;; 	     ;; Swap 16-bit fields
    ;; 	     (tmp3     (fxarithmetic-shift-right
    ;; 			(fxand v #b000000000000111111111111111100000000000000000000000000000000) 20))
    ;; 	     (v (fxior (fxarithmetic-shift-right
    ;; 			(fxand v #b111111111111000000000000000011111111111111110000000000000000) 16)
    ;; 		       (fxarithmetic-shift-left
    ;; 			(fxand v #b000000000000000000000000000000000000000000001111111111111111) 16))))
    ;; 	;; Put together the pieces
    ;; 	(fxior (fxarithmetic-shift-left v 28)
    ;; 	       tmp1 tmp2 tmp3)))

    (define (%fxreverse-bit-field30 v)
      (assert (= (fixnum-width) 30))
      (let* ( ;; Swap pairs of bits
	     (tmp1     (fxarithmetic-shift-right (fxand v #b10000000000000000000000000000) 28))
	     (v (fxior (fxarithmetic-shift-right (fxand v #b01010101010101010101010101010) 1)
		       (fxarithmetic-shift-left  (fxand v #b00101010101010101010101010101) 1)))
	     ;; Swap 2-bit fields
	     (v (fxior (fxarithmetic-shift-right (fxand v #b01100110011001100110011001100) 2)
		       (fxarithmetic-shift-left  (fxand v #b10011001100110011001100110011) 2)))
	     ;; Swap 4-bit fields
	     (tmp2     (fxarithmetic-shift-right (fxand v #b01111000000000000000000000000) 23))
	     (v (fxior (fxarithmetic-shift-right (fxand v #b10000111100001111000011110000) 4)
		       (fxarithmetic-shift-left  (fxand v #b00000000011110000111100001111) 4)))
	     ;; Swap bytes
	     (tmp3     (fxarithmetic-shift-right (fxand v #b00000111111110000000000000000) 11))
	     (v (fxior (fxarithmetic-shift-right (fxand v #b11111000000001111111100000000) 8)
		       (fxarithmetic-shift-left  (fxand v #b00000000000000000000011111111) 8))))
	;; Put together the pieces
	(fxior (fxarithmetic-shift-left v 13)
	       tmp1 tmp2 tmp3)))

    (define who 'fxreverse-bit-field)
    (unless (fixnum? v)
      (assertion-violation who "expected fixnum as first argument" v))
    (unless (and (integer? start) (exact? start))
      (assertion-violation who "expected exact integer as second argument" start))
    (unless (and (integer? end) (exact? end))
      (assertion-violation who "expected exact integer as third argument" end))
    (unless (< -1 start (fixnum-width))
      (assertion-violation who
	(string-append "expected second argument between zero (included) and fixnum width "
		       (number->string (fixnum-width)))
	start))
    (unless (< -1 end (fixnum-width))
      (assertion-violation who
	(string-append "expected third argument between zero (included) and fixnum width "
		       (number->string (fixnum-width)))
	end))
    (unless (<= 0 start end)
      (assertion-violation who
	"expected second argument between zero (included) and third argument"
	start end))

    ;;This is from  the original patch by Göran  Weinholt, posted on the
    ;;Ikarus bug tracker.
    ;;
    ;; (cond ((= (fixnum-width) 61)
    ;;        (fxior (fxarithmetic-shift-right
    ;;                (%fxreverse-bit-field61 (fxbit-field v start end))
    ;;                (fx- 60 end))
    ;;               (fxcopy-bit-field v start end 0)))
    ;;       ((= (fixnum-width) 30)
    ;;        (fxior (fxarithmetic-shift-right
    ;;                (%fxreverse-bit-field30 (fxbit-field v start end))
    ;;                (fx- 29 end))
    ;;               (fxcopy-bit-field v start end 0)))
    ;;       (else
    ;;        (do ((i start (fx+ i 1))
    ;;             (ret 0 (if (fxbit-set? v i)
    ;;                        (fxior ret (fxarithmetic-shift-left 1 (fx- (fx- end i) 1)))
    ;; 			 ret)))
    ;;            ((fx=? i end)
    ;;             (fxior (fxarithmetic-shift-left ret start)
    ;;                    (fxcopy-bit-field v start end 0))))))

    ;;We have FIXNUM-WIDTH equal to 30.
    (fxior (fxarithmetic-shift-right
	    (%fxreverse-bit-field30 (fxbit-field v start end))
	    (fx- 29 end))
	   (fxcopy-bit-field v start end 0)))


  (define (fxbit-field x i j)
    (define who 'fxbit-field)
    (if (fixnum? x)
        (if (fixnum? i)
            (if ($fx<= 0 i)
                (if (fixnum? j)
                    (if ($fx< j (fixnum-width))
                        (if ($fx<= i j)
                            ($fxsra
                              ($fxlogand x ($fxsub1 ($fxsll 1 j)))
                              i)
                            (if ($fx<= 0 j)
                                (die who "index out of range" j)
                                (die who "indices not in order" i j)))
                        (die who "index out of range" j))
                    (die who "not a fixnum" j))
                (die who "index out of range" i))
            (die who "not a fixnum" i))
        (die who "not a fixnum" x)))

  )


(library (ikarus complex-numbers)
  (export make-rectangular $make-rectangular make-polar
          real-part imag-part angle magnitude)
  (import
    (except (ikarus) make-rectangular make-polar
          real-part imag-part angle magnitude)
    (except (ikarus system $compnums) $make-rectangular))

  (define ($make-rectangular r i)
    ;;; should be called with 2 exacts
    (if (eqv? i 0)
        r
        ($make-compnum r i)))

  (define (make-rectangular r i)
    (define who 'make-rectangular)
    (define (err x)
      (die who "invalid argument" x))
    (cond
      [(flonum? i)
       (cond
         [(flonum? r) ($make-cflonum r i)]
         [(or (fixnum? r) (bignum? r) (ratnum? r))
          ($make-cflonum (inexact r) i)]
         [else (err r)])]
      [(eqv? i 0) (if (number? r) r (err r))]
      [(or (fixnum? i) (bignum? i) (ratnum? i))
       (cond
         [(or (fixnum? r) (bignum? r) (ratnum? r))
          ($make-rectangular r i)]
         [(flonum? r)
          ($make-cflonum r (inexact i))]
         [else (err r)])]
      [else (err i)]))

  (define (make-polar mag angle)
    (define who 'make-polar)
    (unless (real? mag)
      (die who "not a real number" mag))
    (unless (real? angle)
      (die who "not a real number" angle))
    (make-rectangular
      (* mag (cos angle))
      (* mag (sin angle))))

  (define magnitude
    (lambda (x)
      (cond
        [(or (fixnum? x) (bignum? x) (ratnum? x) (flonum? x))
         (abs x)]
        [(compnum? x)
         (let ([r ($compnum-real x)]
               [i ($compnum-imag x)])
           (sqrt (+ (* r r) (* i i))))]
        [(cflonum? x)
         (let ([r ($cflonum-real x)]
               [i ($cflonum-imag x)])
           (sqrt (+ (* r r) (* i i))))]
        [else
         (die 'magnitude "not a number" x)])))

  (define angle
    (lambda (x)
      (import (ikarus system $bignums) (ikarus system $ratnums))
      (define PI (acos -1))
      (cond
        [(fixnum? x)
         (if (fx>? x 0)
             0
             (if (fx<? x 0)
                 PI
                 (die 'angle "undefined for 0")))]
        [(bignum? x)
         (if ($bignum-positive? x) 0 PI)]
        [(ratnum? x)
         (let ([n ($ratnum-n x)])
           (if (> n 0) 0 PI))]
        [(flonum? x)
         (atan 0.0 x)]
        [(compnum? x)
         (let ([r ($compnum-real x)]
               [i ($compnum-imag x)])
           (atan i r))]
        [(cflonum? x)
         (let ([r ($cflonum-real x)]
               [i ($cflonum-imag x)])
           (atan i r))]
        [else
         (die 'angle "not a number" x)])))

  (define real-part
    (lambda (x)
      (cond
        [(fixnum? x) x]
        [(bignum? x) x]
        [(ratnum? x) x]
        [(flonum? x) x]
        [(compnum? x) ($compnum-real x)]
        [(cflonum? x) ($cflonum-real x)]
        [else
         (die 'real-part "not a number" x)])))

  (define imag-part
    (lambda (x)
      (cond
        [(fixnum? x) 0]
        [(bignum? x) 0]
        [(ratnum? x) 0]
        [(flonum? x) 0]
        [(compnum? x) ($compnum-imag x)]
        [(cflonum? x) ($cflonum-imag x)]
        [else
         (die 'imag-part "not a number" x)])))
)

(library (ikarus system flonums)
  (export $fixnum->flonum)
  (import (ikarus))
  (define $fixnum->flonum fixnum->flonum))
