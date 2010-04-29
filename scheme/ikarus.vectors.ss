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


(library (ikarus vectors)
  (export make-vector vector vector-length vector-ref vector-set!
          vector->list list->vector vector-map vector-for-each
          vector-fill!)
  (import 
    (except (ikarus) make-vector vector 
            vector-length vector-ref vector-set!
            vector->list list->vector vector-map vector-for-each
            vector-fill!)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $vectors))


  (define vector-length
    (lambda (x)
      (unless (vector? x) 
        (die 'vector-length "not a vector" x))
      ($vector-length x)))

  (module (make-vector)
    (define fill!
      (lambda (v i n fill)
        (cond
          [($fx= i n) v]
          [else
           ($vector-set! v i fill)
           (fill! v ($fx+ i 1) n fill)])))
    (define make-vector
      (case-lambda
        [(n) (make-vector n (void))]
        [(n fill)
         (unless (and (fixnum? n) ($fx>= n 0))
           (die 'make-vector "not a valid length" n))
         (fill! ($make-vector n) 0 n fill)])))


  (define vector
    ;;; FIXME: add case-lambda
    (letrec ([length
              (lambda (ls n)
                (cond
                 [(null? ls) n]
                 [else (length ($cdr ls) ($fx+ n 1))]))]
             [loop 
              (lambda (v ls i n)
                (cond
                 [($fx= i n) v]
                 [else 
                  ($vector-set! v i ($car ls))
                  (loop v ($cdr ls) ($fx+ i 1) n)]))])
       (lambda ls
         (let ([n (length ls 0)])
           (let ([v (make-vector n)])
             (loop v ls 0 n))))))


  (define vector-ref 
    (lambda (v i)
      (unless (vector? v)
        (die 'vector-ref "not a vector" v))
      (unless (fixnum? i)
        (die 'vector-ref "not a valid index" i))
      (unless (and ($fx< i ($vector-length v))
                   ($fx<= 0 i))
        (die 'vector-ref "index is out of range" i v))
      ($vector-ref v i)))
  
  (define vector-set! 
    (lambda (v i c) 
      (unless (vector? v) 
        (die 'vector-set! "not a vector" v))
      (unless (fixnum? i)
        (die 'vector-set! "not a valid index" i))
      (unless (and ($fx< i ($vector-length v))
                   ($fx<= 0 i))
        (die 'vector-set! "index is out of range" i v))
      ($vector-set! v i c)))

  (define vector->list
    (lambda (v)
      (define f
        (lambda (v i ls)
          (cond
            [($fx< i 0) ls]
            [else
             (f v ($fxsub1 i) (cons ($vector-ref v i) ls))])))
      (if (vector? v)
          (let ([n ($vector-length v)])
            (if ($fxzero? n)
                '()
                (f v ($fxsub1 n) '())))
          (die 'vector->list "not a vector" v))))

  (define list->vector
    (letrec ([race
              (lambda (h t ls n)
               (if (pair? h)
                   (let ([h ($cdr h)])
                      (if (pair? h)
                          (if (not (eq? h t))
                              (race ($cdr h) ($cdr t) ls ($fx+ n 2))
                              (die 'list->vector "circular list" ls))
                          (if (null? h)
                              ($fx+ n 1)
                              (die 'list->vector "not a proper list" ls))))
                   (if (null? h)
                       n
                       (die 'list->vector "not a proper list" ls))))]
              [fill
               (lambda (v i ls)
                 (cond
                   [(null? ls) v]
                   [else
                    (let ([c ($car ls)])
                      ($vector-set! v i c)
                      (fill v ($fxadd1 i) (cdr ls)))]))])
       (lambda (ls)
         (let ([n (race ls ls ls 0)])
           (let ([v (make-vector n)])
             (fill v 0 ls))))))

  (module (vector-map)
    (define who 'vector-map)
    (define (ls->vec ls n) 
      (let f ([v (make-vector n)]
              [n n]
              [ls ls])
        (cond
          [(null? ls) v]
          [else
           (let ([n ($fxsub1 n)])
             ($vector-set! v n ($car ls))
             (f v n ($cdr ls)))])))
    (define vector-map
      (case-lambda
        [(p v) 
         (unless (procedure? p) 
           (die who "not a procedure" p))
         (unless (vector? v) 
           (die who "not a vector" v))
         (let f ([p p] [v v] [i 0] [n (vector-length v)] [ac '()])
           (cond
             [($fx= i n) (ls->vec ac n)]
             [else 
              (f p v ($fxadd1 i) n (cons (p (vector-ref v i)) ac))]))]
        [(p v0 v1) 
         (unless (procedure? p) 
           (die who "not a procedure" p))
         (unless (vector? v0) 
           (die who "not a vector" v0))
         (unless (vector? v1) 
           (die who "not a vector" v1))
         (let ([n (vector-length v0)])
           (unless ($fx= n ($vector-length v1))
             (die who "length mismatch" v0 v1))
           (let f ([p p] [v0 v0] [v1 v1] [i 0] [n n] [ac '()])
             (cond
               [($fx= i n) (ls->vec ac n)]
               [else 
                (f p v0 v1 ($fxadd1 i) n 
                   (cons (p ($vector-ref v0 i) ($vector-ref v1 i)) ac))])))]
        [(p v0 v1 . v*) 
         (unless (procedure? p) 
           (die who "not a procedure" p))
         (unless (vector? v0) 
           (die who "not a vector" v0))
         (unless (vector? v1) 
           (die who "not a vector" v1))
         (let ([n (vector-length v0)])
           (unless ($fx= n ($vector-length v1))
             (die who "length mismatch" v0 v1))
           (let f ([v* v*] [n n])
             (unless (null? v*) 
               (let ([a ($car v*)])
                 (unless (vector? a) 
                   (die who "not a vector" a))
                 (unless ($fx= ($vector-length a) n) 
                   (die who "length mismatch")))
               (f ($cdr v*) n)))
           (let f ([p p] [v0 v0] [v1 v1] [v* v*] [i 0] [n n] [ac '()])
             (cond
               [($fx= i n) (ls->vec ac n)] 
               [else 
                (f p v0 v1 v* ($fxadd1 i) n 
                   (cons 
                     (apply p ($vector-ref v0 i) ($vector-ref v1 i)
                       (let f ([i i] [v* v*]) 
                         (if (null? v*) 
                             '()
                             (cons ($vector-ref ($car v*) i) 
                                   (f i ($cdr v*))))))
                     ac))])))])))


  (module (vector-for-each)
    (define who 'vector-for-each)
    (define vector-for-each
      (case-lambda
        [(p v) 
         (unless (procedure? p) 
           (die who "not a procedure" p))
         (unless (vector? v) 
           (die who "not a vector" v))
         (let f ([p p] [v v] [i 0] [n (vector-length v)])
           (cond
             [($fx= i n) (void)]
             [else 
              (p (vector-ref v i))
              (f p v ($fxadd1 i) n)]))]
        [(p v0 v1) 
         (unless (procedure? p) 
           (die who "not a procedure" p))
         (unless (vector? v0) 
           (die who "not a vector" v0))
         (unless (vector? v1) 
           (die who "not a vector" v1))
         (let ([n (vector-length v0)])
           (unless ($fx= n ($vector-length v1))
             (die who "length mismatch" v0 v1))
           (let f ([p p] [v0 v0] [v1 v1] [i 0] [n n])
             (cond
               [($fx= i n) (void)]
               [else 
                (p ($vector-ref v0 i) ($vector-ref v1 i))
                (f p v0 v1 ($fxadd1 i) n)])))]
        [(p v0 v1 . v*) 
         (unless (procedure? p) 
           (die who "not a procedure" p))
         (unless (vector? v0) 
           (die who "not a vector" v0))
         (unless (vector? v1) 
           (die who "not a vector" v1))
         (let ([n (vector-length v0)])
           (unless ($fx= n ($vector-length v1))
             (die who "length mismatch" v0 v1))
           (let f ([v* v*] [n n])
             (unless (null? v*) 
               (let ([a ($car v*)])
                 (unless (vector? a) 
                   (die who "not a vector" a))
                 (unless ($fx= ($vector-length a) n) 
                   (die who "length mismatch")))
               (f ($cdr v*) n)))
           (let f ([p p] [v0 v0] [v1 v1] [v* v*] [i 0] [n n])
             (cond
               [($fx= i n) (void)] 
               [else 
                (apply p ($vector-ref v0 i) ($vector-ref v1 i)
                  (let f ([i i] [v* v*]) 
                    (if (null? v*) 
                        '()
                        (cons ($vector-ref ($car v*) i) 
                              (f i ($cdr v*))))))
                (f p v0 v1 v* ($fxadd1 i) n)])))])))

  (define (vector-fill! v fill)
    (unless (vector? v) 
      (die 'vector-fill! "not a vector" v))
    (let f ([v v] [i 0] [n ($vector-length v)] [fill fill])
      (unless ($fx= i n) 
        ($vector-set! v i fill)
        (f v ($fxadd1 i) n fill))))

  )


(library (ikarus system vectors)
  (export $vector-ref $vector-length)
  (import (ikarus))
  (define $vector-ref vector-ref)
  (define $vector-length vector-length))

