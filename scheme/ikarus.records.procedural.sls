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


(library (ikarus records procedural)
  (export 
    make-record-type-descriptor record-type-descriptor?
    make-record-constructor-descriptor record-accessor
    record-mutator record-constructor record-predicate record?
    record-rtd record-type-name record-type-parent record-type-uid
    record-type-generative?  record-type-sealed? record-type-opaque?
    record-type-field-names record-field-mutable? rtd-subtype? rtd?)
  (import 
    (except (ikarus)
      record-constructor record-predicate record?  record-type-name
      record-type-parent record-type-descriptor? record-rtd
      record-type-uid record-type-sealed? record-type-opaque?
      record-type-generative? make-record-type-descriptor
      make-record-constructor-descriptor record-accessor
      record-mutator
      record-type-field-names record-field-mutable?
      rtd? rtd-subtype?)
    (ikarus system $structs))

  (define-struct rtd 
    (name size old-fields printer-proc symbol parent sealed? opaque? uid fields))

  (define rtd-alist '())
  (define (intern-rtd! uid rtd)
    (set! rtd-alist (cons (cons uid rtd) rtd-alist)))
  (define (lookup-rtd uid)
    (cond
      [(assq uid rtd-alist) => cdr]
      [else #f]))

  (define (record-type-descriptor? x) (rtd? x))

  (define (record? x)
    (and ($struct? x) 
         (let ([rtd ($struct-rtd x)])
           (and (rtd? rtd) 
                (not (rtd-opaque? rtd))))))

  (define (record-rtd x)
    (if ($struct? x)
        (let ([rtd ($struct-rtd x)])
          (if (rtd? rtd)
              (if (not (rtd-opaque? rtd))
                  rtd
                  (die 'record-rtd "record is opaque"))
              (die 'record-rtd "not a record" x)))
        (die 'record-rtd "not a record" x)))

  (define (record-type-name x)
    (if (rtd? x)
        (rtd-name x)
        (die 'record-type-name "not an rtd" x)))

  (define (record-type-parent x)
    (if (rtd? x)
        (rtd-parent x)
        (die 'record-type-parent "not an rtd" x)))

  (define (record-type-uid x)
    (if (rtd? x)
        (or (rtd-uid x)
            (let ([g (gensym)])
              (set-rtd-uid! x g) 
              (intern-rtd! g x)
              g))
        (die 'record-type-uid "not an rtd" x)))

  (define (record-type-sealed? x)
    (if (rtd? x)
        (rtd-sealed? x)
        (die 'record-type-sealed? "not an rtd" x)))

  (define (record-type-opaque? x)
    (if (rtd? x)
        (rtd-opaque? x)
        (die 'record-type-opaque? "not an rtd" x)))

  (define (record-type-generative? x)
    (if (rtd? x)
        (not (rtd-sealed? x)) ;;; FIXME: bogus?
        (die 'record-type-generative? "not an rtd" x)))

  (define (record-type-field-names x)
    (if (rtd? x)
        (let ([v (rtd-fields x)])
          (let ([n (vector-length v)])
            (let f ([x (make-vector n)] [v v] [n n] [i 0])
              (if (= i n) 
                  x
                  (begin
                    (vector-set! x i (cdr (vector-ref v i)))
                    (f x v n (fxadd1 i)))))))
        (die 'record-type-field-names "not an rtd" x)))


  (module (make-record-type-descriptor)
    (define who 'make-record-type-descriptor)
    (define (make-rtd-aux name parent uid sealed? opaque?
                          parent-size fields)
      (make-rtd name (+ parent-size (vector-length fields))
          #f #f #f parent sealed? opaque? uid fields))
    (define (convert-fields sv)
      (unless (vector? sv) 
        (die who "invalid fields argument" sv))
      (let ([n2 (vector-length sv)])
        (let ([v (make-vector n2)])
          (let f ([i 0])
            (unless (= i n2)
              (let ([x (vector-ref sv i)])
                (if (pair? x) 
                    (let ([m/u (car x)] [x (cdr x)])
                      (if (pair? x) 
                          (let ([name (car x)])
                            (unless (and (null? (cdr x)) (symbol? name))
                              (die who "invalid fields argument" sv))
                            (vector-set! v i
                              (cons (case m/u
                                      [(mutable)   #t]
                                      [(immutable) #f]
                                      [else 
                                       (die who "invalid fields argument" sv)]) 
                                    name)))
                          (die who "invalid fields argument" sv)))
                    (die who "invalid fields argument" sv)))
              (f (add1 i))))
          v)))
    (define generate-rtd
      (lambda (name parent uid sealed? opaque? fields)
        (cond
          [(rtd? parent)
           (when (rtd-sealed? parent) 
             (die who "cannot extend sealed parent" parent))
           (make-rtd-aux name parent uid sealed? 
             (or opaque? (rtd-opaque? parent))
             (rtd-size parent)
             (convert-fields fields))]
          [(eqv? parent #f) 
           (make-rtd-aux name parent uid sealed? opaque? 0
             (convert-fields fields))]
          [else (die who "not a valid parent" parent)])))
    (define (same-fields-as-rtd? fields rtd)
      (let* ([fv (rtd-fields rtd)]
             [n (vector-length fv)])
        (and (vector? fields)
             (= (vector-length fields) n)
             (let f ([i 0])
               (or (= i n) 
                   (let ([a (vector-ref fields i)]
                         [b (vector-ref fv i)])
                     (and
                       (pair? a)
                       (case (car a) 
                         [(mutable) (eqv? (car b) #t)]
                         [(immutable) (eqv? (car b) #f)]
                         [else #f])
                       (let ([a (cdr a)])
                         (and (pair? a)
                              (null? (cdr a))
                              (eq? (car a) (cdr b))))
                       (f (+ i 1)))))))))
    (define make-nongenerative-rtd 
      (lambda (name parent uid sealed? opaque? fields)
        (cond
          [(lookup-rtd uid) =>
           (lambda (rtd) 
             (unless
               (and ; must not check name!
                    ; (eqv? name (rtd-name rtd)) 
                    (eqv? parent (rtd-parent rtd))
                    (eqv? sealed? (rtd-sealed? rtd))
                    (eqv? opaque? (rtd-opaque? rtd))
                    (same-fields-as-rtd? fields rtd))
               (die who "arguments not equivalent to those in an existing rtd"
                    parent sealed? opaque? fields))
             rtd)]
          [else
           (let ([rtd (generate-rtd name parent uid sealed? opaque? fields)])
             (intern-rtd! uid rtd)
             rtd)])))
    (define make-record-type-descriptor
      (lambda (name parent uid sealed? opaque? fields)
        (unless (symbol? name)
          (die who "not a valid record type name" name))
        (unless (boolean? sealed?)
          (die who "not a valid sealed? argument" sealed?))
        (unless (boolean? opaque?)
          (die who "not a valid opaque? argument" opaque?))
        (cond
          [(symbol? uid) 
           (make-nongenerative-rtd name parent uid sealed? opaque? fields)]
          [(eqv? uid #f) 
           (generate-rtd name parent uid sealed? opaque? fields)]
          [else (die who "not a valid uid" uid)]))))

  (define-struct rcd (rtd prcd proc))

  (define (is-parent-of? prtd rtd)
    (let ([p (rtd-parent rtd)])
      (cond
        [(eq? p prtd) #t]
        [(not p) #f]
        [else (is-parent-of? prtd p)])))

  (define (rtd-subtype? rtd parent-rtd) 
    (unless (rtd? rtd) 
      (die 'rtd-subtype? "not an rtd" rtd))
    (unless (rtd? parent-rtd) 
      (die 'rtd-substype? "not an rtd" parent-rtd))
    (or (eq? rtd parent-rtd)
        (is-parent-of? parent-rtd rtd)))
        
  (define make-record-constructor-descriptor
    (lambda (rtd prcd protocol)
      (define who 'make-record-constructor-descriptor)
      (unless (rtd? rtd)
        (die who "not a record type descriptor" rtd))
      (unless (or (not protocol) (procedure? protocol))
        (die who "invalid protocol" protocol))
      (let ([prtd (rtd-parent rtd)])
        (cond
          [(not prcd) 
           (make-rcd rtd #f protocol)]
          [(rcd? prcd) 
           (unless (is-parent-of? (rcd-rtd prcd) rtd)
             (die who "descriptor does not apply" 
                    prcd rtd))
           (make-rcd rtd prcd protocol)]
          [else
           (die who "not a valid record constructor descriptor" prcd)]))))

  (define (record-constructor rcd)
    (define who 'record-constructor)

    (define (split all-fields n)
      (let f ([ls all-fields] [n n])
        (if (zero? n)
            (values '() ls) 
            (if (pair? ls) 
                (let-values ([(m p) (f (cdr ls) (- n 1))]) 
                  (values (cons (car ls) m) p))
                (die 'record-constructor "insufficient arguments"
                       all-fields)))))

    (define (constructor main-rtd size prcd proto)
      (define (fill i r flds f*) 
        (cond
          [(null? flds)
           (if (null? f*)
               r
               (fill i r (car f*) (cdr f*)))]
          [else
           ($struct-set! r i (car flds))
           (fill (add1 i) r (cdr flds) f*)]))
      (if (not prcd) ;;; base
          (let ([n (rtd-size main-rtd)])
            (define-syntax expand-setters
              (syntax-rules ()
                [(_ r idx) #f]
                [(_ r idx a0 a* ...)
                 (begin
                   ($struct-set! r idx a0)
                   (expand-setters r (+ idx 1) a* ...))]))
            (define-syntax expand-constructor
              (syntax-rules (default)
                [(_ f* default) 
                 (lambda flds
                   (unless (= (length flds) size)
                     (apply die
                        'a-record-constructor
                        (format 
                          "expected ~a args, got ~a instead" 
                          n (length flds))
                        flds))
                   (let ([r ($make-struct main-rtd n)])
                     (fill 0 r flds f*)))]
                [(_ f* (args ...)) 
                 (lambda (args ...) 
                   (let ([r ($make-struct main-rtd n)])
                     (expand-setters r 0 args ...)
                     (if (null? f*) 
                         r
                         (fill (length '(args ...)) r (car f*) (cdr f*)))))]))
            (define-syntax expand-one-case
              (syntax-rules ()
                [(_ arg-case) 
                 (if proto
                     (lambda (f*)
                       (let ([a-record-constructor 
                               (expand-constructor f* arg-case)])
                          (proto a-record-constructor)))
                     (lambda (f*)
                       (let ([a-record-constructor
                               (expand-constructor f* arg-case)])
                         a-record-constructor)))]))
            (case size 
              [(0)  (expand-one-case ())]
              [(1)  (expand-one-case (f0))]
              [(2)  (expand-one-case (f0 f1))]
              [(3)  (expand-one-case (f0 f1 f2))]
              [(4)  (expand-one-case (f0 f1 f2 f3))]
              [else (expand-one-case default)]))
          (let ([pprcd (rcd-prcd prcd)]
                [sz (rtd-size (rcd-rtd prcd))])
            (let ([p (constructor main-rtd sz pprcd (rcd-proc prcd))]
                  [n (- size sz)]
                  [protocol
                   (if proto
                       proto
                       (lambda (new)
                         (let ([a-record-constructor
                                (lambda all-fields
                                  (let-values ([(parent-fields myfields) 
                                                (split all-fields 
                                                  (- (length all-fields)
                                                     (- size sz)))])
                                     (apply (apply new parent-fields)
                                            myfields)))])
                           a-record-constructor)))])
              (lambda (f*)
                (protocol
                  (lambda fmls
                    (lambda flds
                      (unless (= (length flds) n) 
                        (apply die
                           'a-record-constructor
                           (format 
                             "expected ~a args, got ~a instead" 
                             n (length flds))
                           flds))
                      (apply (p (cons flds f*)) fmls)))))))))
    (unless (rcd? rcd)
      (die who "not a record constructor descriptor" rcd))
    (let ([rtd (rcd-rtd rcd)]
          [prcd (rcd-prcd rcd)]
          [proto (rcd-proc rcd)])
      ((constructor rtd (rtd-size rtd) prcd proto) '())))
  

  (define (record-accessor rtd k) 
    (define who 'record-accessor)
    (unless (rtd? rtd)
      (die who "not an rtd" rtd))
    (unless (and (fixnum? k) (fx>= k 0)) 
      (die who "not a valid index" k))
    (let ([sz (rtd-size rtd)]
          [p (rtd-parent rtd)])
      (let ([i (if p (+ k (rtd-size p)) k)])
        (unless (fx< i sz) 
          (die who "not a valid index" k))
        (let ([a-record-accessor
               (lambda (x) 
                 (cond
                   [($struct/rtd? x rtd) ($struct-ref x i)]
                   [($struct? x)
                    (let ([xrtd ($struct-rtd x)])
                      (unless (rtd? xrtd) 
                        (die who "invalid type" x rtd))
                      (let f ([prtd (rtd-parent xrtd)] [rtd rtd] [x x] [i i])
                        (cond
                          [(eq? prtd rtd) ($struct-ref x i)]
                          [(not prtd) 
                           (die who "invalid type" x rtd)]
                          [else (f (rtd-parent prtd) rtd x i)])))]
                   [else (die who "invalid type" x rtd)]))])
          a-record-accessor))))

  (define (record-mutator rtd k) 
    (define who 'record-mutator)
    (unless (rtd? rtd)
      (die who "not an rtd" rtd))
    (unless (and (fixnum? k) (fx>= k 0)) 
      (die who "not a valid index" k))
    (let ([sz (rtd-size rtd)]
          [p (rtd-parent rtd)])
      (let ([i (if p (+ k (rtd-size p)) k)])
        (unless (fx< i sz) 
          (die who "not a valid index" k))
        (unless (car (vector-ref (rtd-fields rtd) k))
          (die who "field is not mutable" k rtd))
        (let ([a-record-mutator
               (lambda (x v) 
                 (cond
                   [($struct/rtd? x rtd) ($struct-set! x i v)]
                   [($struct? x)
                    (let ([xrtd ($struct-rtd x)])
                      (unless (rtd? xrtd) 
                        (die who "invalid type" x rtd))
                      (let f ([prtd (rtd-parent xrtd)] [rtd rtd] [x x] [i i] [v v])
                        (cond
                          [(eq? prtd rtd) ($struct-set! x i v)]
                          [(not prtd) 
                           (die who "invalid type" x rtd)]
                          [else (f (rtd-parent prtd) rtd x i v)])))]
                   [else (die who "invalid type" x rtd)]))])
          a-record-mutator))))

  (define (record-predicate rtd) 
    (define who 'record-predicate)
    (unless (rtd? rtd)
      (die who "not an rtd" rtd))
    (let ([sz (rtd-size rtd)]
          [p (rtd-parent rtd)])
      (let ([a-record-predicate
             (lambda (x) 
               (cond
                 [($struct/rtd? x rtd) #t]
                 [($struct? x)
                  (let ([xrtd ($struct-rtd x)])
                    (and (rtd? xrtd) 
                         (let f ([prtd (rtd-parent xrtd)] [rtd rtd])
                           (cond
                             [(eq? prtd rtd) #t]
                             [(not prtd)     #f]
                             [else (f (rtd-parent prtd) rtd)]))))]
                 [else #f]))])
        a-record-predicate)))


  (define (record-field-mutable? rtd k) 
    (define who 'record-field-mutable?)
    (unless (rtd? rtd)
      (die who "not an rtd" rtd))
    (unless (and (fixnum? k) (fx>= k 0)) 
      (die who "not a valid index" k))
    (let ([sz (rtd-size rtd)]
          [p (rtd-parent rtd)])
      (let ([i (if p (+ k (rtd-size p)) k)])
        (unless (fx< i sz) 
          (die who "not a valid index" k))
        (car (vector-ref (rtd-fields rtd) k)))))

  (set-rtd-printer! (type-descriptor rtd)
    (lambda (x p wr) 
      (display (format "#<record-type-descriptor ~s>" (rtd-name x)) p)))

  (set-rtd-printer! (type-descriptor rcd)
    (lambda (x p wr) 
      (display (format "#<record-constructor-descriptor ~s>"
                       (rtd-name (rcd-rtd x))) p)))
                  
)


#!eof

rtd0  fields=4
proto0 = 
  (lambda (n) 
    (lambda (p0-fmls ...) 
      (n f0 f1 f2 f3)))

rtd1  fields=2
proto1 = 
  (lambda (n)
    (lambda (p1-fmls ...)
      ((n p0-acts ...) f4 f5)))

rtd2  fields=1
proto2 =
  (lambda (n)
    (lambda (p2-fmls ...)
      ((n p1-acts ...) f6)))


(record-constructor rcd2) 
==
(proto2 (lambda p1-fml*
          (lambda (f6) 
            (apply (proto1 (lambda p0-fml*
                             (lambda (f4 f5) 
                               (apply (proto0 (lambda (f0 f1 f2 f3) 
                                                ($record rtd2 f0 f1 f2 f3 f4 f5 f6)))
                                      p0-fml*))))
                   p1-fml*))))

new0 = (lambda (f0 f1 f2 f3 f4 f5 f6)
         ($record rtd2 f0 f1 f2 f3 f4 f5 f6))

(record-constructor rcd2) 
==
(proto2 (lambda p1-fml*
          (lambda (f6) 
            (apply (proto1 (lambda p0-fml*
                             (lambda (f4 f5) 
                               (apply (proto0 (lambda (f0 f1 f2 f3) 
                                                (new0 f0 f1 f2 f3 f4 f5 f6)))
                                      p0-fml*))))
                   p1-fml*))))















