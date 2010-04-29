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

(library (ikarus enumerations)
  (export make-enumeration enum-set-universe enum-set-indexer
    enum-set-constructor enum-set->list enum-set-member?
    enum-set-subset? enum-set=? enum-set-union enum-set-difference
    enum-set-intersection enum-set-complement enum-set-projection
    make-file-options enum-set?)
  (import 
    (except (ikarus)
      make-enumeration enum-set-universe enum-set-indexer
      enum-set-constructor enum-set->list enum-set-member?
      enum-set-subset? enum-set=? enum-set-union enum-set-difference
      enum-set-intersection enum-set-complement
      enum-set-projection
      make-file-options enum-set?))

  (define-record-type enum-type
    (fields id mask symbol->index-hashtable index->symbol-vector)
    (sealed #t)
    (opaque #t)
    (nongenerative))

  (define-record-type enum-set
    (fields type bits)
    (sealed #t)
    (nongenerative))

  (define (make-enumeration ls) 
    (unless (and (list? ls) (for-all symbol? ls))
      (die 'make-enumeration "not a list of symbols" ls))
    (let ([h (make-eq-hashtable)] [v (list->vector ls)])
      (let f ([i 0] [n (vector-length v)])
        (cond
          [(= i n) 
           (let ([mask (sub1 (sll 1 n))])
             (let ([t (make-enum-type (gensym) mask h v)])
               (make-enum-set t mask)))]
          [else 
           (hashtable-set! h (vector-ref v i) i)
           (f (+ i 1) n)]))))
  
  (define (enum-set-universe x)
    (unless (enum-set? x) 
      (die 'enum-set-universe "not an enum set" x))
    (let ([t (enum-set-type x)])
      (make-enum-set (enum-set-type x) (enum-type-mask t))))
  
  (define (enum-set-indexer x)
    (unless (enum-set? x) 
      (die 'enum-set-indexer "not an enum set" x))
    (let ([h (enum-type-symbol->index-hashtable (enum-set-type x))])
      (lambda (s)
        (unless (symbol? s) 
          (die 'enum-set-indexer "not a symbol" s))
        (hashtable-ref h s #f))))
  
  (define (enum-set-constructor x)
    (unless (enum-set? x)
      (die 'enum-set-constructor "not an enum set" x))
    (let ([t (enum-set-type x)])
      (let ([h (enum-type-symbol->index-hashtable t)])
        (lambda (ls)
          (unless (list? ls) (die 'enum-set-constructor "not a list" ls))
          (let f ([ls ls] [n 0])
            (cond
              [(null? ls) (make-enum-set t n)]
              [else
               (f (cdr ls) 
                  (bitwise-ior n
                    (sll 1
                      (or (hashtable-ref h (car ls) #f)
                          (die 'enum-set-constructor 
                               "not in universe" 
                               (car ls) t)))))]))))))
   
  (define (enum-set->list x)
    (unless (enum-set? x) 
      (die 'enum-set->list "not an enum set" x))
    (let ([v (enum-type-index->symbol-vector (enum-set-type x))])
      (let ([n (vector-length v)])
        (let f ([bits (enum-set-bits x)] [i 0])
          (if (eqv? bits 0)
              '()
              (if (even? bits)
                  (f (sra bits 1) (+ i 1))
                  (cons (vector-ref v i)
                    (f (sra bits 1) (+ i 1)))))))))


  (define (enum-set-andmap proc x)
    (let ([v (enum-type-index->symbol-vector (enum-set-type x))])
      (let ([n (vector-length v)])
        (let f ([bits (enum-set-bits x)] [i 0])
          (if (= bits 0)
              #t
              (if (even? bits)
                  (f (sra bits 1) (+ i 1))
                  (and (proc (vector-ref v i))
                       (f (sra bits 1) (+ i 1)))))))))

  (define (enum-set-member? s x) 
    (unless (enum-set? x) 
      (die 'enum-set-member? "not an enum set" x))
    (let ([h (enum-type-symbol->index-hashtable (enum-set-type x))])
      (let ([idx (hashtable-ref h s #f)])
        (cond
          [idx (bitwise-bit-set? (enum-set-bits x) idx)]
          [(symbol? s) #f]
          [else (die 'enum-set-member? "not a symbol" s)]))))
  
  (define (enum-set-subset? x1 x2) 
    (unless (enum-set? x1) 
      (die 'enum-set-subset? "not an enum set" x1))
    (unless (enum-set? x2) 
      (die 'enum-set-subset? "not an enum set" x2))
    (let ([t1 (enum-set-type x1)] [t2 (enum-set-type x2)])
      (if (or (eq? t1 t2) (eq? (enum-type-id t1) (enum-type-id t2)))
          (let ([b1 (enum-set-bits x1)] [b2 (enum-set-bits x2)])
            (= (bitwise-and b1 b2) b1))
          (and (enum-set-andmap (lambda (s) (enum-set-member? s x2)) x1)
               (let ([u2 (enum-set-universe x2)])
                 (enum-set-andmap (lambda (s) (enum-set-member? s u2))
                   (enum-set-universe x1)))))))
  

  (define (enum-set=? x1 x2) 
    (unless (enum-set? x1) 
      (die 'enum-set=? "not an enum set" x1))
    (unless (enum-set? x2) 
      (die 'enum-set=? "not an enum set" x2))
    (let ([t1 (enum-set-type x1)] [t2 (enum-set-type x2)])
      (if (or (eq? t1 t2) (eq? (enum-type-id t1) (enum-type-id t2)))
          (= (enum-set-bits x1) (enum-set-bits x2))
          (and (enum-set-andmap (lambda (s) (enum-set-member? s x2)) x1)
               (enum-set-andmap (lambda (s) (enum-set-member? s x1)) x2)
               (let ([u1 (enum-set-universe x1)] [u2 (enum-set-universe x2)])
                 (and 
                   (enum-set-andmap (lambda (s) (enum-set-member? s u2)) u1)
                   (enum-set-andmap (lambda (s) (enum-set-member? s u1)) u2)))))))
  
  (define (enum-set-op x1 x2 who combine)
    (unless (enum-set? x1) 
      (die who "not an enum set" x1))
    (unless (enum-set? x2) 
      (die who "not an enum set" x2))
    (let ([t1 (enum-set-type x1)] [t2 (enum-set-type x2)])
      (if (or (eq? t1 t2) (eq? (enum-type-id t1) (enum-type-id t2)))
          (make-enum-set t1 (combine (enum-set-bits x1) (enum-set-bits x2)))
          (die who "enum sets have different enumeration types" x1 x2))))
  
  (define (enum-set-union x1 x2)
    (enum-set-op x1 x2 'enum-set-union bitwise-ior))
  
  (define (enum-set-intersection x1 x2)
    (enum-set-op x1 x2 'enum-set-intersection bitwise-and))
  
  (define (enum-set-difference x1 x2)
    (enum-set-op x1 x2 'enum-set-difference 
      (lambda (n1 n2) (bitwise-and n1 (bitwise-not n2)))))
  
  (define (enum-set-complement x)
    (define who 'enum-set-complement)
    (unless (enum-set? x) 
      (die who "not an enum set" x))
    (let ([t (enum-set-type x)])
      (make-enum-set t
        (bitwise-and 
          (enum-type-mask t)
          (bitwise-not (enum-set-bits x))))))
  
  (define (enum-set-projection x1 x2)
    (define who 'enum-set-projection)
    (unless (enum-set? x1) (die who "not an enum set" x1))
    (unless (enum-set? x2) (die who "not an enum set" x2))
    (let ([t1 (enum-set-type x1)] [t2 (enum-set-type x2)])
      (let ([h (enum-type-symbol->index-hashtable t2)]
            [v (enum-type-index->symbol-vector t1)])
        (let f ([in-bits (enum-set-bits x1)] [i 0] [out-bits 0])
          (if (= in-bits 0)
              (make-enum-set t2 out-bits)
              (if (even? in-bits)
                  (f (sra in-bits 1) (+ i 1) out-bits)
                  (let ([idx (hashtable-ref h (vector-ref v i) #f)])
                    (if idx
                        (f (sra in-bits 1) (+ i 1) 
                           (bitwise-ior out-bits (sll 1 idx)))
                        (f (sra in-bits 1) (+ i 1)
                           out-bits)))))))))

  (define make-file-options
    (enum-set-constructor
      (make-enumeration 
        '(no-create no-fail no-truncate))))

)

