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
          (let ((round? (even? f)))
            (if (>= e 0)
                (if (not (= f (expt b (- p 1))))
                    (let ((be (expt b e)))
                      (scale (* f be 2) 2 be be 0 B round? f e))
                    (let* ((be (expt b e)) (be1 (* be b)))
                      (scale (* f be1 2) (* b 2) be1 be 0 B round? f e)))
                (if (or (= e min-e) (not (= f (expt b (- p 1)))))
                    (scale (* f 2) (* (expt b (- e)) 2) 1 1 0 B round? f e)
                    (scale (* f b 2) (* (expt b (- 1 e)) 2) b 1 0 B round? f e))))))
      (define (len n)
        (let f ((n n) (i 0))
          (cond
            ((zero? n) i)
            (else (f (quotient n 2) (+ i 1))))))
      (define scale
        (lambda (r s m+ m- k B round? f e)
          (let ((est (inexact->exact
                       (ceiling
                         (- (* (+ e (len f) -1) (invlog2of B))
                            1e-10)))))
            (if (>= est 0)
                (fixup r (* s (exptt B est)) m+ m- est B round?)
                (let ((scale (exptt B (- est))))
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
          (let-values (((q r) (quotient+remainder r s)))
            (let ((tc1 ((if round? <= <) r m-))
                  (tc2 ((if round? >= >) (+ r m+) s)))
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
        (let ((table (make-vector 37))
              (log2 (log 2)))
          (do ((B 2 (+ B 1)))
              ((= B 37))
            (vector-set! table B (/ log2 (log B))))
          (lambda (B)
            (if (<= 2 B 36)
                (vector-ref table B)
                (/ log2 (log B))))))
      (define exptt
        (let ((table (make-vector 326)))
          (do ((k 0 (+ k 1)) (v 1 (* v 10)))
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
          ((= expt 1)
           (cons d0 (if (null? d*) '(#\. #\0) (cons #\. d*))))
          (else
           (cons d0
             (let-values (((d0 d*) (next d*)))
               (format-flonum-no-expt (- expt 1) d0 d*))))))
      (define (format-flonum-no-expt/neg expt d*)
        (cond
          ((= expt 0) d*)
          (else (cons #\0 (format-flonum-no-expt/neg (+ expt 1) d*)))))
      (define (sign pos? ls)
        (if pos?
           (list->string ls)
           (list->string (cons #\- ls))))
      (let ((d0 (car digits)) (d* (cdr digits)))
        (cond
          ((null? d*)
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
                         (string d0) "e" (fixnum->string (- expt 1)))))))
          ((and (null? d*) (char=? d0 #\0)) (if pos? "0.0" "-0.0"))
          ((<= 1 expt 9)
           (sign pos? (format-flonum-no-expt expt d0 d*)))
          ((<= -3 expt 0)
           (sign pos? (cons* #\0 #\. (format-flonum-no-expt/neg expt digits))))
          (else
           (string-append
             (if pos? "" "-")
             (string d0) "." (list->string d*)
             "e" (fixnum->string (- expt 1)))))))
    (define (flo->string pos? m e p)
      (let-values (((expt digits) (flonum->digits m e 10 p 2 10)))
         (format-flonum pos? expt digits)))
    (define (flonum->string x)
      (let-values (((pos? be m) (flonum-parts x)))
        (cond
          ((<= 1 be 2046) ; normalized flonum
           (flo->string pos? (+ m (expt 2 52)) (- be 1075) 53))
          ((= be 0)
           (flo->string pos? m -1074 52))
          ((= be 2047)
           (if (= m 0)
               (if pos? "+inf.0" "-inf.0")
               ;;; Gee!  nans have no sign!
               "+nan.0"))
          (else (die 'flonum->string "cannot happen"))))))
  ;;;
  (define (string->flonum x)
    (cond
      ((string? x)
       (foreign-call "ikrt_bytevector_to_flonum"
         (string->utf8 x)))
      (else
       (die 'string->flonum "not a string" x))))


;;;; done

)

;;; end of file
