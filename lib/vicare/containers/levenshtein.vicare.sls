;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: Levenshtein distance metric
;;;Date: Mon May  9, 2011
;;;
;;;Abstract
;;;
;;;	The original  version of  this file is  from the  PLaneT package
;;;	version 1.3 (2009-03-14).
;;;
;;;Copyright (C) 2004-2009 Neil Van Dyke
;;;Port to R6RS by Marco Maggi
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;For other  licenses and consulting,  please contact the  author (Neil
;;;Van Dyke).
;;;


#!r6rs
(library (vicare containers levenshtein)
  (export
    levenshtein
    levenshtein/predicate
    list-levenshtein
    list-levenshtein/eq
    list-levenshtein/equal
    list-levenshtein/eqv
    list-levenshtein/predicate
    string-levenshtein
    vector-levenshtein
    vector-levenshtein/eq
    vector-levenshtein/equal
    vector-levenshtein/eqv
    vector-levenshtein/predicate
    vector-levenshtein/predicate/get-scratch)
  (import (rnrs))


;;;; helpers

(define (%identity x) x)

(define (%string-empty? v) (zero? (string-length v)))
(define (%vector-empty? v) (zero? (vector-length v)))

(define (%string->vector s)
  (list->vector (string->list s)))


;;;; basic comparisons

(define (vector-levenshtein/predicate/get-scratch a b pred get-scratch)
  (let ((a-len (vector-length a))
        (b-len (vector-length b)))
    (cond ((zero? a-len) b-len)
          ((zero? b-len) a-len)
          (else
           (let ((w    (get-scratch (+ 1 b-len)))
                 (next #f))
             (let fill ((k b-len))
               (vector-set! w k k)
               (or (zero? k) (fill (- k 1))))
             (let loop-i ((i 0))
               (if (= i a-len)
                   next
		 (let ((a-i (vector-ref a i)))
		   (let loop-j ((j   0)
				(cur (+ 1 i)))
		     (if (= j b-len)
			 (begin (vector-set! w b-len next)
				(loop-i (+ 1 i)))
		       ;; TODO: Make these costs parameters.
		       (begin (set! next (min (+ 1 (vector-ref w (+ 1 j)))
					      (+ 1 cur)
					      (if (pred a-i
							(vector-ref b j))
						  (vector-ref w j)
						(+ 1 (vector-ref w j)))))
			      (vector-set! w j cur)
			      (loop-j (+ 1 j) next))))))))))))

(define (vector-levenshtein/predicate a b pred)
  (vector-levenshtein/predicate/get-scratch a b pred make-vector))

(define (vector-levenshtein/eq    a b)
  (vector-levenshtein/predicate a b eq?))
(define (vector-levenshtein/eqv   a b)
  (vector-levenshtein/predicate a b eqv?))
(define (vector-levenshtein/equal a b)
  (vector-levenshtein/predicate a b equal?))

(define (vector-levenshtein a b)
  (vector-levenshtein/equal a b))

(define (list-levenshtein/predicate a b pred)
  (cond ((null? a) (length b))
        ((null? b) (length a))
        (else (vector-levenshtein/predicate (list->vector a)
                                            (list->vector b)
                                            pred))))

(define (list-levenshtein/eq    a b) (list-levenshtein/predicate a b eq?))
(define (list-levenshtein/eqv   a b) (list-levenshtein/predicate a b eqv?))
(define (list-levenshtein/equal a b) (list-levenshtein/predicate a b equal?))

(define (list-levenshtein       a b) (list-levenshtein/equal     a b))

;; TODO:  Maybe make  a version that  does the  O(n) access to  the list
;;       elements in exchange for not allocating a vector.

(define (string-levenshtein a b)
  ;; TODO: Maybe make a version that doesn't convert to vectors but also
  ;;       doesn't do lots of string-refs.
  (cond ((zero? (string-length a)) (string-length b))
        ((zero? (string-length b)) (string-length a))
        (else (vector-levenshtein/eqv
               (%string->vector a)
               (%string->vector b)))))

(define (%string-levenshtein/predicate a b pred)
  (cond ((zero? (string-length a)) (string-length b))
        ((zero? (string-length b)) (string-length a))
        (else (vector-levenshtein/predicate
               (%string->vector a)
               (%string->vector b)
               pred))))


;;;; type-coercing comparisons

(define levenshtein/predicate
  ;; TODO: Change this to a let-syntax.
  (let ((foo (lambda (a b pred a-emp a-len a-vec)
               (let ((bar (lambda (b-emp b-len b-vec)
                            (if (b-emp b)
                                (a-len a)
                                (vector-levenshtein/predicate (a-vec a)
                                                              (b-vec b)
                                                              pred)))))
                 (cond ((vector? b) (bar %vector-empty?
                                         vector-length
                                         %identity))
                       ((string? b) (bar %string-empty?
                                         string-length
                                         %string->vector))
                       ((list?   b) (bar null? length list->vector))
                       (else (error "term 2 must be vector, list, or string:"
                                    b)))))))
    (lambda (a b pred)
      (cond ((vector? a) (if (vector? b)
                             (vector-levenshtein/predicate a b pred)
                             (foo a b pred
                                  %vector-empty?
                                  vector-length
                                  %identity)))
            ((string? a) (if (string? b)
                             (%string-levenshtein/predicate
                              a b pred)
                             (foo a b pred
                                  %string-empty?
                                  string-length
                                  %string->vector)))
            ((list?   a) (if (list? b)
                             (list-levenshtein/predicate a b pred)
                             (foo a b pred null? length list->vector)))
            (else (error "term 1 must be vector, list, or string:" a))))))

(define (levenshtein a b)
  (if (and (string? a) (string? b))
      (string-levenshtein a b)
      (levenshtein/predicate a b equal?)))


;; @appendix Trullenque Perl Implementation
;;
;; For reference, the implementation from [Trullenque] is reproduced here.
;;
;; @verbatim
;; sub levenshtein($$){
;;   my @A=split //, lc shift;
;;   my @B=split //, lc shift;
;;   my @W=(0..@B);
;;   my ($i, $j, $cur, $next);
;;   for $i (0..$#A){
;;     $cur=$i+1;
;;     for $j (0..$#B){
;;             $next=min(
;;                     $W[$j+1]+1,
;;                     $cur+1,
;;                     ($A[$i] ne $B[$j])+$W[$j]
;;             );
;;             $W[$j]=$cur;
;;             $cur=$next;
;;     }
;;     $W[@B]=$next;
;;   }
;;   return $next;
;; }
;;
;; sub min($$$){
;;   if ($_[0] < $_[2]){ pop @_; } else { shift @_; }
;;   return $_[0] < $_[1]? $_[0]:$_[1];
;; }
;; @end verbatim


;;;; done

)

;;; end of file
