;;;
;;;Part of: Vicare Scheme
;;;Contents: randomness related vector functions
;;;Date: Thu Jul  2, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (vicare crypto randomisations vectors)
  (export
    %random-vector-shuffle		random-vector-shuffle
    %random-vector-shuffle!		random-vector-shuffle!

    %random-vector-sample		random-vector-sample
    %random-vector-sample-population	random-vector-sample-population

    random-integers-with-sum		random-reals-with-sum
    random-vector-unfold-numbers)
  (import (rnrs)
    (vicare crypto randomisations)
    (vicare containers vectors low)
    (rename (only (vicare containers vectors) %vector-unpack)
	    (%vector-unpack unpack)))


;;;; helpers

(define-syntax set-incr!
  (syntax-rules ()
    ((_ ?name ?delta)
     (set! ?name (+ ?name ?delta)))))


;;;; low level

(define (%random-vector-shuffle! source vec start past)
  (let* ((len		(- past start))
	 (integer-maker	(random-source-integers-maker source)))
    (do ((k len (- k 1)))
	((= k 1)
	 vec)
      (let* ((i (+ start (- k 1)))
	     (j (+ start (integer-maker k)))
	     (xi (vector-ref vec i))
	     (xj (vector-ref vec j)))
	(vector-set! vec i xj)
	(vector-set! vec j xi)))))

(define (%random-vector-shuffle source vec start past)
  (let ((dst (%vector-copy vec start past)))
    (%random-vector-shuffle! source vec start past)))

(define (%random-vector-sample source vec start past)
  (let ((index-maker	(random-source-integers-maker source))
	(max		(- past start)))
    (lambda ()
      (vector-ref vec (+ start (index-maker max))))))

(define (%random-vector-sample-population source len vec start past)
  (let ((sampler	(%random-vector-sample source vec start past)))
    (lambda ()
      (do ((i 0 (+ 1 i))
	   (individual (make-vector len)))
	  ((= i len)
	   individual)
	(vector-set! individual i (sampler))))))


;;;; high level

(define (random-vector-unfold-numbers number-maker number-of-numbers)
  (do ((i 0 (+ 1 i))
       (vec (make-vector number-of-numbers) (begin (vector-set! vec i (number-maker)) vec)))
      ((= i number-of-numbers)
       vec)))

(define-syntax random-vector-shuffle
  (syntax-rules ()
    ((_ ?V ?source)
     (let-values (((vec start past) (unpack ?V)))
       (%random-vector-shuffle ?source vec start past)))))

(define-syntax random-vector-shuffle!
  (syntax-rules ()
    ((_ ?V ?source)
     (let-values (((vec start past) (unpack ?V)))
       (%random-vector-shuffle! ?source vec start past)))))

(define-syntax random-vector-sample
  (syntax-rules ()
    ((_ ?V ?source)
     (let-values (((vec start past) (unpack ?V)))
       (%random-vector-sample ?source vec start past)))))

(define-syntax random-vector-sample-population
  (syntax-rules ()
    ((_ ?V ?len ?source)
     (let-values (((vec start past) (unpack ?V)))
       (%random-vector-sample-population ?source ?len vec start past)))))

(define (random-integers-with-sum requested-sum number-of-numbers
				  range-min ; inclusive
				  range-max ; inclusive
				  source)
  (assert (<= (* range-min number-of-numbers)
	      requested-sum
	      (* range-max number-of-numbers)))
  (let* ((integers-maker (random-source-integers-maker-from-range source range-min range-max))
	 (vec		 (random-vector-unfold-numbers integers-maker number-of-numbers))
	 (delta		 (- requested-sum (vector-fold-left + 0 vec)))
	 (step		 (if (positive? delta) +1 -1))
	 (index-maker	 (random-source-integers-maker source)))
    (let loop ((delta delta))
      (if (zero? delta)
	  vec
	(let* ((idx (index-maker number-of-numbers))
	       (val (+ step (vector-ref vec idx))))
	  (if (<= range-min val range-max)
	      (begin
		(vector-set! vec idx val)
		(loop (- delta step)))
	    (loop delta)))))))

(define (random-reals-with-sum requested-sum epsilon
			       number-of-numbers
			       range-min    ; exclusive
			       range-max    ; exclusive
			       source)
  (assert (<= (* range-min number-of-numbers)
	      requested-sum
	      (* range-max number-of-numbers)))
  (let* ((reals-maker	(random-source-reals-maker-from-range source range-min range-max))
  	 (vec		(random-vector-unfold-numbers reals-maker number-of-numbers))
	 (index-maker	(random-source-integers-maker source))
	 (epsilon+	(abs epsilon))
	 (epsilon-	(- epsilon+)))
    (let loop ()
      (let* ((delta (- requested-sum (vector-fold-left + 0 vec)))
	     (step  (/ delta number-of-numbers)))
	(if (<= epsilon- (abs delta) epsilon+)
	    vec
	  (do ((i 0 (+ 1 i)))
	      ((= i number-of-numbers)
	       (loop))
	    (let* ((idx (index-maker number-of-numbers))
		   (val (+ step (vector-ref vec idx))))
	      (when (< range-min val range-max)
		(vector-set! vec idx val)))))))))


;;;; done

)

;;; end of file
