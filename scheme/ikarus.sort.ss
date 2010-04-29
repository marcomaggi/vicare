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


(library (ikarus.sort)
  (export list-sort vector-sort vector-sort!)
  (import 
    (except (ikarus) list-sort vector-sort vector-sort!))

    
  (module UNSAFE  
    (fx<? fx<=? fx>? fx>=? fx=? 
     fx+ fx- fxarithmetic-shift-right
     vector-ref vector-set! car cdr)
    (import 
      (rename (ikarus system $pairs)
        ($car    car)
        ($cdr    cdr))
      (rename (ikarus system $vectors) 
        ($vector-ref    vector-ref)
        ($vector-set!   vector-set!))
      (rename (ikarus system $fx) 
        ($fxsra    fxarithmetic-shift-right)
        ($fx+      fx+)
        ($fx-      fx-)
        ($fx<      fx<?)
        ($fx>      fx>?)
        ($fx>=     fx>=?)
        ($fx<=     fx<=?)
        ($fx=      fx=?))))

  (define (list-sort proc ls) 
    (import UNSAFE)
    (define race
      (lambda (h t ls n)
        (if (pair? h)
            (let ([h (cdr h)])
               (if (pair? h)
                   (if (not (eq? h t))
                       (race (cdr h) (cdr t) ls (fx+ n 2))
                       (die 'list-sort "circular list" ls))
                   (if (null? h)
                       (fx+ n 1)
                       (die 'list-sort "not a proper list" ls))))
            (if (null? h)
                n
                (die 'list-sort "not a proper list" ls)))))
    (unless (procedure? proc)
      (die 'list-sort "not a procedure" proc))
    (let ([n (race ls ls ls 0)])
      (cond
        [(fx< n 2) ls]
        [else
         (let f ([v (make-vector n)] [ls ls] [i 0])
           (cond
             [(null? ls) 
              (vector-sort! proc v)
              (vector->list v)]
             [else 
              (vector-set! v i (car ls))
              (f v (cdr ls) (fx+ i 1))]))])))

  (module (vector-sort vector-sort!)
   
    (import UNSAFE) 

    (define (copy-subrange! src dst si di dj) 
      (vector-set! dst di (vector-ref src si)) 
      (let ([di (fx+ di 1)])
        (when (fx<=? di dj)
          (copy-subrange! src dst (fx+ si 1) di dj))))
    
    (define (do-merge-a! proc src skr ri rj ai aj bi bj b0)
      (let ([a0 (vector-ref skr ai)]
            [ai (fx+ ai 1)])
        (cond
          [(proc b0 a0)
           (vector-set! src ri b0)
           (let ([ri (fx+ ri 1)])
             (cond
               [(fx<=? bi bj)
                (do-merge-b! proc src skr ri rj ai aj bi bj a0)]
               [else
                (vector-set! src ri a0)
                (let ([ri (fx+ ri 1)])
                  (cond
                    [(fx<=? ri rj)
                     (copy-subrange! skr src ai ri rj)]))]))]
          [else
           (vector-set! src ri a0)
           (let ([ri (fx+ ri 1)])
             (cond
               [(fx<=? ai aj)
                (do-merge-a! proc src skr ri rj ai aj bi bj b0)]
               [else
                (vector-set! src ri b0)
                (let ([ri (fx+ ri 1)])
                  (cond
                    [(fx<=? ri rj)
                     (copy-subrange! skr src bi ri rj)]))]))])))
    
    
    (define (do-merge-b! proc src skr ri rj ai aj bi bj a0)
      (let ([b0 (vector-ref skr bi)]
            [bi (fx+ bi 1)])
        (cond
          [(proc b0 a0)
           (vector-set! src ri b0)
           (let ([ri (fx+ ri 1)])
             (cond
               [(fx<=? bi bj)
                (do-merge-b! proc src skr ri rj ai aj bi bj a0)]
               [else
                (vector-set! src ri a0)
                (let ([ri (fx+ ri 1)])
                  (cond
                    [(fx<=? ri rj)
                     (copy-subrange! skr src ai ri rj)]))]))]
          [else
           (vector-set! src ri a0)
           (let ([ri (fx+ ri 1)])
             (cond
               [(fx<=? ai aj)
                (do-merge-a! proc src skr ri rj ai aj bi bj b0)]
               [else
                (vector-set! src ri b0)
                (let ([ri (fx+ ri 1)])
                  (cond
                    [(fx<=? ri rj)
                     (copy-subrange! skr src bi ri rj)]))]))])))
    
    (define (do-merge! proc src skr ri rj ai aj bi bj)
      (let ([a0 (vector-ref skr ai)]
            [b0 (vector-ref skr bi)]
            [ai (fx+ ai 1)]
            [bi (fx+ bi 1)])
        (cond
          [(proc b0 a0)
           (vector-set! src ri b0)
           (let ([ri (fx+ ri 1)])
             (cond
               [(fx<=? bi bj)
                (do-merge-b! proc src skr ri rj ai aj bi bj a0)]
               [else
                (vector-set! src ri a0) 
                (let ([ri (fx+ ri 1)])
                  (cond
                    [(fx<=? ri rj) 
                     (copy-subrange! skr src ai ri rj)]))]))]
          [else
           (vector-set! src ri a0)
           (let ([ri (fx+ ri 1)])
             (cond
               [(fx<=? ai aj)
                (do-merge-a! proc src skr ri rj ai aj bi bj b0)]
               [else
                (vector-set! src ri b0)
                (let ([ri (fx+ ri 1)])
                  (cond
                    [(fx<=? ri rj)
                     (copy-subrange! skr src bi ri rj)]))]))])))
    
    (define (do-sort! proc src skr i k)
      ; sort src[i .. k] inclusive in place
      (cond
        [(fx<? i k)
         (let ([j (fxarithmetic-shift-right (fx+ i k) 1)])
           (do-sort! proc skr src i j)
           (do-sort! proc skr src (fx+ j 1) k)
           (do-merge! proc src skr i k i j (fx+ j 1) k))]))
    
    (define (vector-copy v)
      (let ([n (vector-length v)])
        (let f ([v v] [r (make-vector n)] [n n] [i 0])
          (cond
            [(fx=? i n) r]
            [else 
             (vector-set! r i (vector-ref v i))
             (f v r n (fx+ i 1))]))))
    
    (define (vector-sort proc src)
      (unless (procedure? proc) 
        (die 'vector-sort "not a procedure" proc))
      (unless (vector? src) 
        (die 'vector-sort "not a vector" src))
      (let ([src (vector-copy src)]
            [skr (vector-copy src)])
        (do-sort! proc src skr 0 (fx- (vector-length src) 1))
        src))
    
    (define (vector-sort! proc src)
      (unless (procedure? proc) 
        (die 'vector-sort! "not a procedure" proc))
      (unless (vector? src) 
        (die 'vector-sort! "not a vector" src))
      (let ([skr (vector-copy src)])
        (do-sort! proc src skr 0 (fx- (vector-length src) 1))
        src)))
    
  )


