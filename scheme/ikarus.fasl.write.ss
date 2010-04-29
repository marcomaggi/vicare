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


(library (ikarus.fasl.write)
  (export fasl-write)
  (import
    (rnrs hashtables)
    (ikarus system $codes)
    (ikarus system $pairs)
    (ikarus system $structs)
    (ikarus system $bytevectors)
    (ikarus system $fx)
    (ikarus system $chars)
    (ikarus system $strings)
    (ikarus system $flonums)
    (ikarus system $bignums)
    (except (ikarus.code-objects) procedure-annotation)
    (except (ikarus) fasl-write write-byte))
 
  (module (wordsize)
    (import (ikarus include))
    (include "ikarus.config.ss"))

  ;;; (define-syntax fxshift 
  ;;;   (identifier-syntax
  ;;;     (case wordsize
  ;;;       [(4) 2]
  ;;;       [(8) 3]
  ;;;       [else (error 'fxshift "invalid wordsize" wordsize)])))

  ;;; (define-syntax intbits (identifier-syntax (* wordsize 8)))

  ;;; (define-syntax fxbits (identifier-syntax (- intbits fxshift)))

  (define fxshift 
    (case wordsize
      [(4) 2]
      [(8) 3]
      [else (error 'fxshift "invalid wordsize" wordsize)]))

  (define intbits (* wordsize 8))

  (define fxbits (- intbits fxshift))

  (define (fx? x)
    (and (or (fixnum? x) (bignum? x)) 
         (<= (- (expt 2 (- fxbits 1)))
             x
             (- (expt 2 (- fxbits 1)) 1))))
 
  (define (int? x) 
    (and (or (fixnum? x) (bignum? x)) 
         (<= (- (expt 2 (- intbits 1))) 
             x
             (- (expt 2 (- intbits 1)) 1))))

  (define-syntax write-byte
    (syntax-rules ()
      [(_ byte port)
       (put-u8 port byte)]))

  (define (put-tag c p)
    (write-byte (char->integer c) p))
  
  (define write-int32 
    (lambda (x p)
      (write-byte (bitwise-and x #xFF) p)
      (write-byte (bitwise-and (sra x 8) #xFF) p)
      (write-byte (bitwise-and (sra x 16) #xFF) p)
      (write-byte (bitwise-and (sra x 24) #xFF) p)))
 
  (define write-int 
    (lambda (x p)
      (unless (int? x) (die 'write-int "not a int" x))
      (write-int32 x p)
      (when (eqv? wordsize 8)
        (write-int32 (sra x 32) p))))

  (define fasl-write-immediate
    (lambda (x p)
      (cond
        [(null? x) (put-tag #\N p)]
        [(fx? x)
         (put-tag #\I p)
         (write-int (bitwise-arithmetic-shift-left x fxshift) p)]
        [(char? x)
         (let ([n ($char->fixnum x)])
           (if ($fx<= n 255)
               (begin
                 (put-tag #\c p)
                 (write-byte n p))
               (begin
                 (put-tag #\C p)
                 (write-int32 n p))))]
        [(boolean? x)
         (put-tag (if x #\T #\F) p)]
        [(eof-object? x) (put-tag #\E p)]
        [(eq? x (void)) (put-tag #\U p)]
        [else (die 'fasl-write "not a fasl-writable immediate" x)])))
  
  (define (ascii-string? s)
    (let f ([s s] [i 0] [n (string-length s)])
      (or ($fx= i n)
          (and ($char<= ($string-ref s i) ($fixnum->char 127))
               (f s ($fxadd1 i) n)))))
  
  (define (count-unshared-cdrs x h n)
    (cond
      [(and (pair? x) (eq? (hashtable-ref h x #f) 0))
       (count-unshared-cdrs ($cdr x) h ($fxadd1 n))]
      [else n]))
  
  (define (write-pairs x p h m n)
    (cond
      [($fx= n 0) (fasl-write-object x p h m)]
      [else 
       (write-pairs (cdr x) p h 
         (fasl-write-object (car x) p h m)
         ($fxsub1 n))]))
       
  (define do-write
    (lambda (x p h m)
      (cond
        [(pair? x)
         (let ([d ($cdr x)])
           (let ([n (count-unshared-cdrs d h 0)])
             (cond
               [($fx= n 0)
                (put-tag #\P p)
                (fasl-write-object d p h
                  (fasl-write-object (car x) p h m))]
               [else 
                (cond
                  [($fx<= n 255) 
                   (put-tag #\l p)
                   (write-byte n p)]
                  [else
                   (put-tag #\L p)
                   (write-int n p)])
                (write-pairs d p h 
                  (fasl-write-object (car x) p h m)
                  n)])))]
        [(vector? x)
         (put-tag #\V p)
         (write-int (vector-length x) p)
         (let f ([x x] [i 0] [n (vector-length x)] [m m])
           (cond
             [(fx= i n) m]
             [else
              (f x (fxadd1 i) n
                 (fasl-write-object (vector-ref x i) p h m))]))]
        [(string? x) 
         (cond
           [(ascii-string? x)
            (put-tag #\s p)
            (write-int (string-length x) p)
            (let f ([x x] [i 0] [n (string-length x)])
              (unless (fx= i n)
                (write-byte (char->integer (string-ref x i)) p)
                (f x (fxadd1 i) n)))]
           [else
            (put-tag #\S p)
            (write-int (string-length x) p)
            (let f ([x x] [i 0] [n (string-length x)])
              (unless (= i n)
                (write-int32 (char->integer (string-ref x i)) p)
                (f x (fxadd1 i) n)))])
         m]
        [(gensym? x)
         (put-tag #\G p)
         (fasl-write-object (gensym->unique-string x) p h
           (fasl-write-object (symbol->string x) p h m))]
        [(symbol? x) 
         (put-tag #\M p)
         (fasl-write-object (symbol->string x) p h m)]
        [(code? x)
         (put-tag #\x p)
         (write-int (code-size x) p)
         (write-int (bitwise-arithmetic-shift-left
                      (code-freevars x)
                      fxshift)
                    p)
         (let ([m (fasl-write-object ($code-annotation x) p h m)])
           (let f ([i 0] [n (code-size x)])
             (unless (fx= i n)
               (write-byte (code-ref x i) p)
               (f (fxadd1 i) n)))
           (fasl-write-object (code-reloc-vector x) p h m))]
        [(hashtable? x)
         (let ([v (hashtable-ref h x #f)])
           (if (eq? eq? (hashtable-equivalence-function x))
               (put-tag #\h p)
               (put-tag #\H p))
           (fasl-write-object (vector-ref v 2) p h
             (fasl-write-object (vector-ref v 1) p h m)))]
        [(struct? x)
         (cond
           [(record-type-descriptor? x)
            (put-tag #\W p)
            (let* ([m (fasl-write-object (record-type-name x) p h m)]
                   [m (fasl-write-object (record-type-parent x) p h m)]
                   [m (fasl-write-object (record-type-uid x) p h m)])
              (fasl-write-immediate (record-type-sealed? x) p)
              (fasl-write-immediate (record-type-opaque? x) p)
              (let* ([fields (record-type-field-names x)]
                     [n (vector-length fields)])
                (fasl-write-immediate n p)
                (let f ([i 0] [m m])
                  (cond
                    [(= i n) m]
                    [else
                     (fasl-write-immediate (record-field-mutable? x i) p)
                     (f (+ i 1) 
                        (fasl-write-object (vector-ref fields i) p h m))]))))]
           [else
            (let ([rtd (struct-type-descriptor x)])
              (cond
                [(eq? rtd (base-rtd))
                 ;;; rtd record
                 (put-tag #\R p)
                 (let ([names (struct-type-field-names x)]
                       [m 
                        (fasl-write-object (struct-type-symbol x) p h
                          (fasl-write-object (struct-type-name x) p h m))])
                   (write-int (length names) p)
                   (let f ([names names] [m m])
                     (cond
                       [(null? names) m]
                       [else
                        (f (cdr names)
                           (fasl-write-object (car names) p h m))])))]
                [else
                 ;;; non-rtd record
                 (put-tag #\{ p)
                 (let ([n (struct-length x)])
                   (write-int n p)
                   (let f ([i 0] 
                           [m (fasl-write-object rtd p h m)])
                     (cond
                       [(= i n) m]
                       [else
                        (f (+ i 1)
                           (fasl-write-object 
                              (struct-ref x i)
                              p h m))])))]))])]
        [(procedure? x)
         (put-tag #\Q p)
         (fasl-write-object ($closure-code x) p h m)]
        [(bytevector? x) 
         (put-tag #\v p)
         (let ([n ($bytevector-length x)])
           (write-int n p)
           (write-bytevector x 0 n p))
         m]
        [(flonum? x) 
         (put-tag #\f p)
         (write-byte ($flonum-u8-ref x 7) p)
         (write-byte ($flonum-u8-ref x 6) p)
         (write-byte ($flonum-u8-ref x 5) p)
         (write-byte ($flonum-u8-ref x 4) p)
         (write-byte ($flonum-u8-ref x 3) p)
         (write-byte ($flonum-u8-ref x 2) p)
         (write-byte ($flonum-u8-ref x 1) p)
         (write-byte ($flonum-u8-ref x 0) p)
         m]
        [(ratnum? x)
         (put-tag #\r p)
         (fasl-write-object (numerator x) p h
           (fasl-write-object (denominator x) p h m))]
        [(bignum? x) 
         (put-tag #\b p) 
         (let ([sz ($bignum-size x)])
           (write-int (if ($bignum-positive? x) sz (- sz)) p)
           (let f ([i 0])
             (unless (fx= i sz)
               (write-byte ($bignum-byte-ref x i) p)
               (f (fxadd1 i)))))
         m]
        [(or (compnum? x) (cflonum? x))
         (put-tag #\i p)
         (fasl-write-object (imag-part x) p h
           (fasl-write-object (real-part x) p h m))]
        [else (die 'fasl-write "not fasl-writable" x)])))
  (define (write-bytevector x i j p)
    (unless ($fx= i j)
      (write-byte ($bytevector-u8-ref x i) p)
      (write-bytevector x ($fxadd1 i) j p)))
  (define fasl-write-object 
    (lambda (x p h m)
      (cond
        [(immediate? x) (fasl-write-immediate x p) m]
        [(hashtable-ref h x #f) =>
         (lambda (mk)
           (let ([mark (if (fixnum? mk) mk (vector-ref mk 0))])
             (cond
               [(fx= mark 0) ; singly referenced
                (do-write x p h m)]
               [(fx> mark 0) ; marked but not written
                (if (fixnum? mk)
                    (hashtable-set! h x (fx- 0 m))
                    (vector-set! mk 0 (fx- 0 m)))
                (put-tag #\> p)
                (write-int32 m p)
                (do-write x p h (fxadd1 m))]
               [else
                (put-tag #\< p)
                (write-int32 (fx- 0 mark) p)
                m])))]
        [else (die 'fasl-write "BUG: not in hash table" x)]))) 
  (define make-graph
    (lambda (x h)
      (unless (immediate? x)
        (cond
          [(hashtable-ref h x #f) =>
           (lambda (i) 
             (if (vector? i)
                 (vector-set! i 0 (fxadd1 (vector-ref i 0)))
                 (hashtable-set! h x (fxadd1 i))))]
          [else
           (hashtable-set! h x 0)
           (cond
             [(pair? x) 
              (make-graph (car x) h)
              (make-graph (cdr x) h)]
             [(vector? x)
              (let f ([x x] [i 0] [n (vector-length x)])
                (unless (fx= i n) 
                  (make-graph (vector-ref x i) h)
                  (f x (fxadd1 i) n)))]
             [(symbol? x) 
              (make-graph (symbol->string x) h)
              (when (gensym? x) (make-graph (gensym->unique-string x) h))]
             [(string? x) (void)]
             [(code? x) 
              (make-graph ($code-annotation x) h)
              (make-graph (code-reloc-vector x) h)]
             [(hashtable? x) 
              (when (hashtable-hash-function x) 
                (die 'fasl-write "not fasl-writable" x))
              (let-values ([(keys vals) (hashtable-entries x)])
                (make-graph keys h)
                (make-graph vals h)
                (hashtable-set! h x (vector 0 keys vals)))]
             [(struct? x)
              (cond
                [(eq? x (base-rtd))
                 (die 'fasl-write "base-rtd is not writable")]
                [(record-type-descriptor? x) 
                 (make-graph (record-type-name x) h)
                 (make-graph (record-type-parent x) h)
                 (make-graph (record-type-uid x) h)
                 (vector-for-each 
                   (lambda (x) (make-graph x h))
                   (record-type-field-names x))]
                [else
                 (let ([rtd (struct-type-descriptor x)])
                   (cond
                     [(eq? rtd (base-rtd))
                      ;;; this is a struct rtd
                      (make-graph (struct-type-name x) h)
                      (make-graph (struct-type-symbol x) h)
                      (for-each (lambda (x) (make-graph x h))
                        (struct-type-field-names x))]
                     [else
                      ;;; this is a struct
                      (make-graph rtd h)
                      (let f ([i 0] [n (struct-length x)])
                        (unless (= i n) 
                          (make-graph (struct-ref x i) h)
                          (f (+ i 1) n)))]))])]
             [(procedure? x)
              (let ([code ($closure-code x)])
                (unless (fxzero? (code-freevars code))
                  (die 'fasl-write
                    "Cannot write a non-thunk procedure; \
                     the one given has free vars"
                    (code-freevars code)))
                (make-graph code h))]
             [(bytevector? x) (void)]
             [(flonum? x)     (void)]
             [(bignum? x)     (void)]
             [(ratnum? x) 
              (make-graph (numerator x) h)
              (make-graph (denominator x) h)]
             [(or (compnum? x) (cflonum? x))
              (make-graph (real-part x) h)
              (make-graph (imag-part x) h)]
             [else (die 'fasl-write "not fasl-writable" x)])]))))
  (define fasl-write-to-port
    (lambda (x port)
      (let ([h (make-eq-hashtable)])
         (make-graph x h)
         (put-tag #\# port)
         (put-tag #\@ port)
         (put-tag #\I port)
         (put-tag #\K port)
         (put-tag #\0 port)
         (put-tag (if (= wordsize 4) #\1 #\2) port)
         (fasl-write-object x port h 1)
         (void))))
  (define fasl-write
    (case-lambda 
      [(x p)
       (cond
         [(not (output-port? p)) 
          (die 'fasl-write "not an output port" p)]
         [(not (binary-port? p))
          (die 'fasl-write "not a binary port" p)]
         [else (fasl-write-to-port x p)])])))
