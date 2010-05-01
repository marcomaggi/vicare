;;; Copyright (c) 2009 Abdulaziz Ghuloum
;;; Modified by Marco Maggi.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(library (vicare flonum-formatter)
  (export ikarus-format-flonum)
  (import (rnrs))

  (define (ikarus-format-flonum pos? digits expt)
    (define fixnum->string number->string)
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
           "e" (fixnum->string (- expt 1)))]))))
