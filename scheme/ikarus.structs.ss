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



(library (ikarus structs)
  (export
    make-struct-type struct-type-name struct-type-symbol
    struct-type-field-names struct-constructor struct-predicate
    struct-field-accessor struct-field-mutator struct? struct-rtd
    set-rtd-printer!
    (rename (struct-rtd struct-type-descriptor))
    struct-name struct-printer struct-length struct-ref struct-set!)

  (import
    (ikarus system $structs)
    (ikarus system $pairs)
    (ikarus system $fx)
    (except (ikarus)
      make-struct-type struct-type-name struct-type-symbol
      struct-type-field-names struct-constructor struct-predicate
      struct-field-accessor struct-field-mutator struct? struct-rtd
      struct-type-descriptor struct-name struct-printer struct-length
      struct-ref struct-set! set-rtd-printer!))



  (define rtd?
    (lambda (x)
      (and ($struct? x)
           (eq? ($struct-rtd x) (base-rtd)))))

  (define rtd-name
    (lambda (rtd)
      ($struct-ref rtd 0)))

  (define rtd-length
    (lambda (rtd)
      ($struct-ref rtd 1)))

  (define rtd-fields
    (lambda (rtd)
      ($struct-ref rtd 2)))

  (define rtd-printer
    (lambda (rtd)
      ($struct-ref rtd 3)))

  (define rtd-symbol
    (lambda (rtd)
      ($struct-ref rtd 4)))

  (define set-rtd-name!
    (lambda (rtd name)
      ($struct-set! rtd 0 name)))
 
  (define set-rtd-length!
    (lambda (rtd n)
      ($struct-set! rtd 1 n)))

  (define set-rtd-fields!
    (lambda (rtd fields)
      ($struct-set! rtd 2 fields)))

  (define $set-rtd-printer!
    (lambda (rtd printer)
      ($struct-set! rtd 3 printer)))
   
  (define set-rtd-symbol!
    (lambda (rtd symbol)
      ($struct-set! rtd 4 symbol)))

  (define make-rtd
    (lambda (name fields printer symbol)
      ($struct (base-rtd) name (length fields) fields printer symbol)))

  (define verify-field
    (lambda (x)
      (unless (symbol? x) 
        (die 'make-struct-type "not a valid field name" x))))
  
  (define set-fields
    (lambda (r f* i n)
      (cond
        [(null? f*)
         (if ($fx= i n)
             r
             #f)]
        [($fx< i n)
         (if (null? f*)
             #f
             (begin
               ($struct-set! r i ($car f*))
               (set-fields r ($cdr f*) ($fxadd1 i) n)))]
        [else #f])))

  (define make-struct-type
    (case-lambda
      [(name fields)
       (unless (string? name)
         (die 'make-struct-type "name must be a string" name))
       (unless (list? fields)
         (die 'make-struct-type "fields must be a list" fields))
       (for-each verify-field fields)
       (let ([g (gensym name)])
         (let ([rtd (make-rtd name fields #f g)])
           (set-symbol-value! g rtd)
           rtd))]
      [(name fields g)
       (unless (string? name)
         (die 'make-struct-type "name must be a string" name))
       (unless (list? fields)
         (die 'make-struct-type "fields must be a list" fields))
       (for-each verify-field fields)
       (cond
         [(symbol-bound? g)
          (let ([rtd (symbol-value g)])
            (unless (and (string=? name (struct-type-name rtd))
                         (equal? fields (struct-type-field-names rtd)))
              (die 'make-struct-type "definition mismatch"))
            rtd)]
         [else
          (let ([rtd (make-rtd name fields #f g)])
            (set-symbol-value! g rtd)
            rtd)])]))

  (define struct-type-name
    (lambda (rtd)
      (unless (rtd? rtd)
        (die 'struct-type-name "not an rtd" rtd))
      (rtd-name rtd)))

  (define struct-type-symbol
    (lambda (rtd)
      (unless (rtd? rtd)
        (die 'struct-type-symbol "not an rtd" rtd))
      (rtd-symbol rtd)))
  
  (define struct-type-field-names
    (lambda (rtd)
      (unless (rtd? rtd)
        (die 'struct-type-field-names "not an rtd" rtd))
      (rtd-fields rtd)))
 

  (define struct-constructor
    (lambda (rtd)
      (unless (rtd? rtd)
        (die 'struct-constructor "not an rtd"))
      (lambda args
        (let ([n (rtd-length rtd)])
          (let ([r ($make-struct rtd n)])
            (or (set-fields r args 0 n)
                (die 'struct-constructor 
                  "incorrect number of arguments to the constructor" 
                  rtd)))))))
  
  (define struct-predicate
    (lambda (rtd)
      (unless (rtd? rtd)
        (die 'struct-predicate "not an rtd"))
      (lambda (x)
        (and ($struct? x)
             (eq? ($struct-rtd x) rtd)))))

  (define field-index 
    (lambda (i rtd who)
      (cond
        [(fixnum? i)
         (unless (and ($fx>= i 0) ($fx< i (rtd-length rtd)))
           (die who "out of range for rtd" i rtd))
         i]
        [(symbol? i)
         (letrec ([lookup
                   (lambda (n ls)
                     (cond
                       [(null? ls) 
                        (die who "not a field" rtd)]
                       [(eq? i ($car ls)) n]
                       [else (lookup ($fx+ n 1) ($cdr ls))]))])
           (lookup 0 (rtd-fields rtd)))]
        [else (die who "not a valid index" i)])))

  (define struct-field-accessor
    (lambda (rtd i)
      (unless (rtd? rtd)
        (die 'struct-field-accessor "not an rtd" rtd))
      (let ([i (field-index i rtd 'struct-field-accessor)])
        (lambda (x)
          (unless (and ($struct? x) 
                       (eq? ($struct-rtd x) rtd))
            (die 'struct-field-accessor "not of correct type" x rtd))
          ($struct-ref x i)))))

  (define struct-field-mutator
    (lambda (rtd i)
      (unless (rtd? rtd)
        (die 'struct-field-mutator "not an rtd" rtd))
      (let ([i (field-index i rtd 'struct-field-mutator)])
        (lambda (x v)
          (unless (and ($struct? x) 
                       (eq? ($struct-rtd x) rtd))
            (die 'struct-field-mutator "not of correct type" x rtd))
          ($struct-set! x i v)))))

  (define struct?
    (lambda (x . rest)
      (if (null? rest)
          ($struct? x)
          (let ([rtd ($car rest)])
            (unless (null? ($cdr rest))
              (die 'struct? "too many arguments"))
            (unless (rtd? rtd)
              (die 'struct? "not an rtd"))
            (and ($struct? x)
                 (eq? ($struct-rtd x) rtd))))))

  (define struct-rtd
    (lambda (x)
      (if ($struct? x)
          ($struct-rtd x)
          (die 'struct-rtd "not a struct" x))))

  (define struct-length
    (lambda (x)
      (if ($struct? x)
          (rtd-length ($struct-rtd x))
          (die 'struct-length "not a struct" x))))
            
  (define struct-name
    (lambda (x)
      (if ($struct? x)
          (rtd-name ($struct-rtd x))
          (die 'struct-name "not a struct" x))))

  (define struct-printer
    (lambda (x)
      (if ($struct? x)
          (rtd-printer ($struct-rtd x))
          (die 'struct-printer "not a struct" x))))

  (define struct-ref
    (lambda (x i)
      (unless ($struct? x) (die 'struct-ref "not a struct" x))
      (unless (fixnum? i) (die 'struct-ref "not a valid index" i))
      (let ([n (rtd-length ($struct-rtd x))])
        (unless (and ($fx>= i 0) ($fx< i n))
          (die 'struct-ref "index is out of range" i x))
        ($struct-ref x i))))

  (define struct-set!
    (lambda (x i v)
      (unless ($struct? x) (die 'struct-set! "not a struct" x))
      (unless (fixnum? i) (die 'struct-set! "not a valid index" i))
      (let ([n (rtd-length ($struct-rtd x))])
        (unless (and ($fx>= i 0) ($fx< i n))
          (die 'struct-set! "index is out of range" i x))
        ($struct-set! x i v))))

  (define (set-rtd-printer! x p)
    (unless (rtd? x)
      (die 'set-rtd-printer! "not an rtd" x))
    (unless (procedure? p)
      (die 'set-rtd-printer! "not a procedure" p))
    ($set-rtd-printer! x p))

  (set-rtd-fields! (base-rtd) '(name fields length printer symbol))
  (set-rtd-name! (base-rtd) "base-rtd")
  ($set-rtd-printer! (base-rtd)
    (lambda (x p wr)
      (unless (rtd? x)
        (die 'struct-type-printer "not an rtd"))
      (display "#<" p)
      (display (rtd-name x) p)
      (display " rtd>" p)))
  )


(library (ikarus systems structs)
  (export $struct-ref $struct/rtd?)
  (import (ikarus))
  (define $struct-ref struct-ref)
  (define ($struct/rtd? x rtd)
    (import (ikarus system $structs))
    ($struct/rtd? x rtd)))

