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


;;; FASL 
;;;
;;; A fasl object is a header followed by one or more objects followed by an
;;; end-of-fasl marker
;;;
;;; The header is the string "#@IK01"
;;; The end of fasl marker is "@"
;;;
;;; An object is either:
;;;   "N" : denoting the empty list
;;;   "T" : denoting #t
;;;   "F" : denoting #f
;;;   "E" : denoting the end of file object
;;;   "U" : denoting the unspecified value
;;;   "I" + 4-bytes : denoting a fixnum (in host byte order)
;;;   "c" + 1-byte : denoting a small character (<= 255)
;;;   "C" + 4-byte word: big char.
;;;   "P" + object1 + object2 : a pair
;;;   "V" + 4-bytes(n) + object ... : a vector of length n followed by n objects
;;;   "v" + 4-byte(n) + octet ... : a bytevector of length n followed by n octets
;;;   "s" + 4-bytes(n) + octet ... : an ascii string
;;;   "S" + 4-bytes(n) + int ... : a unicode string
;;;   "M" + symbol-name : a symbol
;;;   "G" + pretty-name + unique-name : a gensym
;;;   "R" + rtd-name + rtd-symbol + field-count + field-names
;;;   "{" + field-count + rtd + fields
;;;   ">" + 4-bytes(i) : mark the next object with index i
;;;   "<" + 4-bytes(i) : dereference the object marked with index i
;;;   "x" : denotes code
;;;   "T" : Thunk; followed by code.
;;;   "r" + numerator + denominator : ratnum
;;;   "f" + 8-byte : IEEE flonum
;;;   "b" + 4-byte(n) + n-bytes denotes a bignum (sign is sign of n).






(library (ikarus fasl read)
  (export fasl-read)
  (import (except (ikarus) fasl-read)
          (except (ikarus.code-objects) procedure-annotation)
          (ikarus system $codes)
          (ikarus system $structs))

  (define who 'fasl-read)
  
  (define (make-struct rtd n)
    (import (ikarus system $fx))
    (let f ([i 0] [n n] [s ($make-struct rtd n)])
      (cond
        [($fx= i n) s]
        [else 
         ($struct-set! s i 0)
         (f ($fx+ i 1) n s)])))

  (define (read-u8 p)
    (let ([b (get-u8 p)])
      (when (eof-object? b) 
        (error who "invalid eof encountered" p))
      b))

  (define (read-u8-as-char p)
    (integer->char (read-u8 p)))

  (define (assert-eq? x y)
    (unless (eq? x y)
      (die who 
        (format "Expected ~s, got ~s\n" y x))))

  (define (char->int x)
    (if (char? x)
        (char->integer x)
        (die who "unexpected eof inside a fasl object")))

  (define (read-u32 p)
    (let* ([c0 (read-u8 p)]
           [c1 (read-u8 p)]
           [c2 (read-u8 p)]
           [c3 (read-u8 p)])
      (bitwise-ior c0 (sll c1 8) (sll c2 16) (sll c3 24))))
    
  (define (read-fixnum p)
    (case (fixnum-width)
      [(30)
       (let* ([c0 (read-u8 p)]
              [c1 (read-u8 p)]
              [c2 (read-u8 p)]
              [c3 (read-u8 p)])
         (cond
           [(fx<= c3 127)
            (fxlogor (fxlogor (fxsra c0 2) (fxsll c1 6))
                     (fxlogor (fxsll c2 14) (fxsll c3 22)))]
           [else
            (let ([c0 (fxlogand #xFF (fxlognot c0))]
                  [c1 (fxlogand #xFF (fxlognot c1))]
                  [c2 (fxlogand #xFF (fxlognot c2))]
                  [c3 (fxlogand #xFF (fxlognot c3))])
              (fx- -1 
                (fxlogor (fxlogor (fxsra c0 2) 
                                  (fxsll c1 6))
                         (fxlogor (fxsll c2 14) 
                                  (fxsll c3 22)))))]))]
      [else
       (let* ([u0 (read-u32 p)]
              [u1 (read-u32 p)])
         (if (<= u1 #x7FFFFFF)
             (sra (bitwise-ior u0 (sll u1 32)) 3)
             (let ([u0 (fxlogand #xFFFFFFFF (fxlognot u0))]
                   [u1 (fxlogand #xFFFFFFFF (fxlognot u1))])
               (fx- -1 
                  (fxlogor (fxsra u0 3) (fxsll u1 29))))))]))

  (define (read-int p)
    (case (fixnum-width)
      [(30)
       (let* ([c0 (char->int (read-u8-as-char p))]
              [c1 (char->int (read-u8-as-char p))]
              [c2 (char->int (read-u8-as-char p))]
              [c3 (char->int (read-u8-as-char p))])
         (cond
           [(fx<= c3 127)
            (fxlogor (fxlogor c0 (fxsll c1 8))
                     (fxlogor (fxsll c2 16) (fxsll c3 24)))]
           [else
            (let ([c0 (fxlogand #xFF (fxlognot c0))]
                  [c1 (fxlogand #xFF (fxlognot c1))]
                  [c2 (fxlogand #xFF (fxlognot c2))]
                  [c3 (fxlogand #xFF (fxlognot c3))])
              (fx- -1 
                (fxlogor (fxlogor c0 
                                  (fxsll c1 8))
                         (fxlogor (fxsll c2 16) 
                                  (fxsll c3 24)))))]))]
      [else
       (let* ([u0 (read-u32 p)]
              [u1 (read-u32 p)])
         (if (<= u1 #x7FFFFFF)
             (bitwise-ior u0 (sll u1 32))
             (let ([u0 (fxlogand #xFFFFFFFF (fxlognot u0))]
                   [u1 (fxlogand #xFFFFFFFF (fxlognot u1))])
               (- -1 (bitwise-ior u0 (sll u1 32))))))]))

  (define (do-read p)
    (define marks (make-vector 1 #f))
    (define (max x y)
      (if (fx> x y) x y))
    (define (put-mark m obj)
      (cond
        [(fx< m (vector-length marks))
         (when (vector-ref marks m)
           (die 'fasl-read "mark set twice" m))
         (vector-set! marks m obj)]
        [else
         (let ([n (vector-length marks)])
           (let ([v (make-vector 
                      (max (fx* n 2) (fx+ m 1))
                      #f)])
             (let f ([i 0])
               (cond
                 [(fx= i n) 
                  (set! marks v)
                  (vector-set! marks m obj)]
                 [else
                  (vector-set! v i (vector-ref marks i))
                  (f (fxadd1 i))]))))]))
    (define (read) (read/mark #f))
    (define (read-code code-m clos-m)
      (let* ([code-size (read-int p)]
             [freevars (read-fixnum p)])
        (let ([code (make-code code-size freevars)])
          (when code-m (put-mark code-m code))
          (let ([annotation (read)])
            (set-code-annotation! code annotation))
          (let f ([i 0])
            (unless (fx= i code-size)
              (code-set! code i (char->int (read-u8-as-char p)))
              (f (fxadd1 i))))
          (cond
            [clos-m
             (let ([clos ($code->closure code)])
               (put-mark clos-m clos)
               (set-code-reloc-vector! code (read))
               code)]
            [else
             (set-code-reloc-vector! code (read))
             code]))))
    (define (read-procedure m)
      (let ([c (read-u8-as-char p)])
        (case c
          [(#\x)
           (let ([code (read-code #f m)])
             (if m (vector-ref marks m) ($code->closure code)))]
          [(#\<) 
           (let ([cm (read-u32 p)])
             (unless (fx< cm (vector-length marks))
               (die who "invalid mark" m))
             (let ([code (vector-ref marks cm)])
               (let ([proc ($code->closure code)])
                 (when m (put-mark m proc))
                 proc)))]
          [(#\>)
           (let ([cm (read-u32 p)])
             (assert-eq? (read-u8-as-char p) #\x)
             (let ([code (read-code cm m)])
               (if m (vector-ref marks m) ($code->closure code))))]
          [else (die who "invalid code header" c)])))

    (define (read-list len m)
      (let ([ls (make-list (+ len 1))])
        (when m (put-mark m ls))
        (let f ([ls ls])
          (set-car! ls (read))
          (let ([d (cdr ls)])
            (if (null? d)
                (set-cdr! ls (read))
                (f d))))
        ls))
 
    (define (read/mark m)
      (define (nom)
        (when m (die who "unhandled mark")))
      (let ([h (read-u8-as-char p)])
        (case h
          [(#\I) 
           (read-fixnum p)]
          [(#\P)
           (if m
               (let ([x (cons #f #f)])
                 (put-mark m x)
                 (set-car! x (read))
                 (set-cdr! x (read))
                 x)
               (let ([a (read)])
                 (cons a (read))))]
          [(#\N) '()]
          [(#\T) #t]
          [(#\F) #f]
          [(#\E) (eof-object)]
          [(#\U) (void)]
          [(#\s) ;;; string
           (let ([n (read-int p)])
             (let ([str (make-string n)])
               (let f ([i 0])
                 (unless (fx= i n)
                   (let ([c (read-u8-as-char p)])
                     (string-set! str i c)
                     (f (fxadd1 i)))))
               (when m (put-mark m str))
               str))]
          [(#\S) ;;; unicode string
           (let ([n (read-int p)])
             (let ([str (make-string n)])
               (let f ([i 0])
                 (unless (fx= i n)
                   (let ([c (integer->char (read-u32 p))])
                     (string-set! str i c)
                     (f (fxadd1 i)))))
               (when m (put-mark m str))
               str))]
          [(#\M) ;;; symbol
           (let ([str (read)])
             (let ([sym (string->symbol str)])
               (when m (put-mark m sym))
               sym))]
          [(#\G)
           (let* ([pretty (read)]
                  [unique (read)])
             (let ([g (foreign-call "ikrt_strings_to_gensym" pretty unique)])
               (when m (put-mark m g))
               g))]
          [(#\V) ;;; vector
           (let ([n (read-int p)])
             (let ([v (make-vector n)])
               (when m (put-mark m v))
               (let f ([i 0])
                 (unless (fx= i n)
                   (vector-set! v i (read))
                   (f (fxadd1 i))))
               v))]
          [(#\v) ;;; bytevector
           (let ([n (read-int p)])
             (let ([v (make-bytevector n)])
               (when m (put-mark m v))
               (let f ([i 0])
                 (unless (fx= i n)
                   (bytevector-u8-set! v i (read-u8 p))
                   (f (fxadd1 i))))
               v))]
          [(#\x) ;;; code
           (read-code m #f)]
          [(#\Q) ;;; procedure
           (read-procedure m)]
          [(#\R)
           (let* ([rtd-name (read)]
                  [rtd-symbol (read)]
                  [field-count (read-int p)])
             (let ([fields
                    (let f ([i 0])
                      (cond
                        [(fx= i field-count) '()]
                        [else 
                         (let ([a (read)])
                           (cons a (f (fxadd1 i))))]))])
               (let ([rtd (make-struct-type 
                            rtd-name fields rtd-symbol)])
                 (when m (put-mark m rtd))
                 rtd)))]
          [(#\{)
           (let ([n (read-int p)])
             (let ([rtd (read)])
               (let ([x (make-struct rtd n)])
                 (when m (put-mark m x))
                 (let f ([i 0])
                   (unless (fx= i n)
                     ($struct-set! x i (read))
                     (f (fxadd1 i))))
                 x)))]
          [(#\C) (integer->char (read-u32 p))]
          [(#\c) (read-u8-as-char p)]
          [(#\>)
           (let ([m (read-u32 p)])
             (read/mark m))]
          [(#\<)
           (let ([m (read-u32 p)])
             (unless (fx< m (vector-length marks))
               (die who "invalid mark" m))
             (or (vector-ref marks m)
                 (error who "uninitialized mark" m)))]
          [(#\l) ;;; list of length <= 255
           (read-list (read-u8 p) m)]
          [(#\L) ;;; list of length > 255
           (read-list (read-int p) m)]
          [(#\W) ;;; r6rs record type descriptor
           (let* ([name    (read)]
                  [parent  (read)]
                  [uid     (read)]
                  [sealed? (read)]
                  [opaque? (read)]
                  [n       (read)]
                  [fields  (make-vector n)])
             (let f ([i 0])
               (cond
                 [(= i n) 
                  (let ([rtd (make-record-type-descriptor
                               name parent uid sealed? opaque?
                               fields)])
                    (when m (put-mark m rtd))
                    rtd)]
                 [else
                  (let* ([field-mutable? (read)]
                         [field-name (read)])
                    (vector-set! fields i 
                      (list (if field-mutable? 'mutable 'immutable) field-name))
                    (f (+ i 1)))])))]
          [(#\b) ;;; bignum
           (let ([i (read-int p)]) 
             (let ([bytes (if (< i 0) (- i) i)])
               (let ([bv (get-bytevector-n p bytes)])
                 (let ([n (bytevector-uint-ref bv 0 'little bytes)])
                   (let ([n (if (< i 0) (- n) n)])
                     (when m (put-mark m n))
                     n)))))]
          [(#\f) ;;; flonum
           (let ()
             (import (ikarus system $flonums))
             (let ([x ($make-flonum)])
               ($flonum-set! x 7 (get-u8 p))
               ($flonum-set! x 6 (get-u8 p))
               ($flonum-set! x 5 (get-u8 p))
               ($flonum-set! x 4 (get-u8 p))
               ($flonum-set! x 3 (get-u8 p))
               ($flonum-set! x 2 (get-u8 p))
               ($flonum-set! x 1 (get-u8 p))
               ($flonum-set! x 0 (get-u8 p))
               (when m (put-mark m x))
               x))]
          [(#\r) ;;; ratnum
           (let* ([den (read)]
                  [num (read)])
             (let ([x (/ num den)])
               (when m (put-mark m x))
               x))]
          [(#\i) ;;; compnum
           (let* ([real (read)]
                  [imag (read)])
             (let ([x (make-rectangular real imag)])
               (when m (put-mark m x))
               x))]
          [(#\h #\H) 
           (let ([x (if (eqv? h #\h) (make-eq-hashtable) (make-eqv-hashtable))])
             (when m (put-mark m x))
             (let* ([keys (read)] [vals (read)])
               (vector-for-each
                 (lambda (k v) (hashtable-set! x k v))
                 keys vals))
             x)]
          [else
           (die who "Unexpected char as a fasl object header" h p)])))
    (read))
  (define $fasl-read
    (lambda (p)
      (assert-eq? (read-u8-as-char p) #\#)
      (assert-eq? (read-u8-as-char p) #\@)
      (assert-eq? (read-u8-as-char p) #\I)
      (assert-eq? (read-u8-as-char p) #\K)
      (assert-eq? (read-u8-as-char p) #\0)
      (case (fixnum-width)
        [(30) (assert-eq? (read-u8-as-char p) #\1)]
        [else (assert-eq? (read-u8-as-char p) #\2)])
      (let ([v (do-read p)])
        (unless (port-eof? p)
          (printf "port did not reach eof\n"))
        v)))
  
  (define fasl-read
    (case-lambda
      [(p) 
       (if (input-port? p) 
           ($fasl-read p)
           (die 'fasl-read "not an input port" p))]))

  )

