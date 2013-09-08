;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for random
;;;Date: Thu Jun 25, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare checks)
  (except (vicare containers lists)
	  break)
  (vicare containers strings)
  (vicare containers char-sets)
  (except (vicare containers vectors)
	  vector-copy!
	  vector-copy)

  (vicare crypto randomisations)

  (vicare crypto randomisations distributions)
  (vicare crypto randomisations lists)
  (vicare crypto randomisations vectors)
  (vicare crypto randomisations strings)

  (vicare crypto randomisations marsaglia)
  (vicare crypto randomisations mersenne)
  (vicare crypto randomisations blum-blum-shub)
  (vicare crypto randomisations borosh)
  (vicare crypto randomisations cmrg))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraires: random numbers\n")

(define const:2^32 (expt 2 32))
(define const:2^32-1 (- const:2^32 1))


(parameterise ((check-test-name 'no-fuss))

  (check-for-true (integer? (random-integer 10)))
  (check-for-true (real? (random-real)))

  )


(parameterise ((check-test-name 'default-source))

  (let* ((make-integer	(random-source-integers-maker default-random-source)))

    (define (integer) (make-integer 100))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((make-real	(random-source-reals-maker default-random-source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((make-real	(random-source-reals-maker default-random-source 1e-30)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((make-real	(random-source-reals-maker default-random-source 1e-5)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-b	((random-source-maker)))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! default-random-source make-integer)
     (let* ((make-real	(random-source-reals-maker default-random-source))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! default-random-source 10)
	(let ((make-integer (random-source-integers-maker default-random-source)))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((bytevector-maker (random-source-bytevectors-maker default-random-source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((state	(random-source-state-ref default-random-source)))
     (random-source-state-set! default-random-source state)
     (let ((make-integer (random-source-integers-maker default-random-source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'default-source-parameter))

  (let* ((source	((random-source-maker)))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((source	((random-source-maker)))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))


  (let* ((source	((random-source-maker)))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	((random-source-maker)))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	((random-source-maker)))
	  (source-b	((random-source-maker)))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! ((random-source-maker)) 10)
	(let ((make-integer (random-source-integers-maker ((random-source-maker)))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		((random-source-maker)))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	((random-source-maker)))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'mrg32k3a))

  (let* ((source	(make-random-source/mrg32k3a))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/mrg32k3a))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/mrg32k3a))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/mrg32k3a))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/mrg32k3a))
	  (source-b	(make-random-source/mrg32k3a))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/mrg32k3a) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/mrg32k3a))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/mrg32k3a))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/mrg32k3a))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'device))

  (let* ((source	(make-random-source/device))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/device))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/device))
	  (source-b	(make-random-source/device))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a (let ((count 0))
				     (lambda (U)
				       (and (< count 100)
					    (begin
					      (set! count (+ 1 count))
					      (make-integer U))))))
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/device) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/device))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/device))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/device))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'device-low-level))

;;;This will block if "/dev/random" has not enough random bytes.
;;   (check
;;       (parameterise ((random-device-cache-length 5))
;; 	(let* ((len 1)
;; 	       (bv  (random-device-bytevector len)))
;; 	  (and (bytevector? bv)
;; 	       (= len (bytevector-length bv)))))
;;     => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((len 10)
	     (bv  (urandom-device-bytevector len)))
	(and (bytevector? bv)
	     (= len (bytevector-length bv))))
    => #t)

  (check
      (let* ((len 5000)
	     (bv  (urandom-device-bytevector len)))
	(and (bytevector? bv)
	     (= len (bytevector-length bv))))
    => #t)

  )


(parameterise ((check-test-name 'utils))

  (define (make-integer) (random-integer 10))

  (let ((obj (random-list-unfold-numbers make-integer 10)))
    (check-for-true (list? obj))
    (check-for-true (every integer? obj))
    (check-for-true (every non-negative? obj)))

  (let ((obj (random-vector-unfold-numbers make-integer 10)))
    (check-for-true (vector? obj))
    (check-for-true (vector-every integer? obj))
    (check-for-true (vector-every non-negative? obj)))

  (let ((obj (random-string-unfold-chars make-integer 10)))
    (check-for-true (string? obj)))

;;; --------------------------------------------------------------------

  (let* ((perm-maker	(random-permutations-maker default-random-source))
	 (obj		(perm-maker 10)))

    (check (vector? obj) => #t)
    (check (vector-length obj) => 10)
    (check (vector-every (lambda (n)
			   (and (integer? n) (<= 0 n) (< n 10)))
			 obj)
      => #t)
    )

  (let* ((exp-maker	(random-exponentials-maker default-random-source))
	 (norm-maker	(random-normals-maker default-random-source)))
    (check (real? (exp-maker 1)) => #t)
    (check (real? (norm-maker 1 2)) => #t))

;;; --------------------------------------------------------------------

  (let ((sampler (random-source-integers-maker-from-range default-random-source 0 10)))

    (check (integer? (sampler)) => #t)
    (check (let ((n (sampler))) (and (<= 0 n) (<= n 10))) => #t))

  (let ((sampler (random-source-integers-maker-from-range default-random-source 0 10 2)))

    (check (integer? (sampler)) => #t)
    (check (let ((n (sampler))) (and (<= 0 n) (<= n 10))) => #t)
    (check (mod (sampler) 2) => 0)
    (check (mod (sampler) 2) => 0)
    (check (mod (sampler) 2) => 0)
    (check (mod (sampler) 2) => 0)
    (check (mod (sampler) 2) => 0)
    (check (mod (sampler) 2) => 0)
    )

;;; --------------------------------------------------------------------

  (let ((sampler (random-source-reals-maker-from-range default-random-source 0 10)))

    (check (real? (sampler)) => #t)
    (check (let ((n (sampler))) (and (<= 0 n) (<= n 10))) => #t))

  (let ((sampler (random-source-reals-maker-from-range default-random-source 0 10 1.2)))

    (check (real? (sampler)) => #t)
    (check (let ((n (sampler))) (and (<= 0 n) (<= n 10))) => #t))

  )


(parameterise ((check-test-name 'list))

  (check
      (list? (random-list-shuffle '(0 1 2 3 4 5 6 7 8 9) default-random-source))
    => #t)

  (check
      (length (random-list-shuffle '(0 1 2 3 4 5 6 7 8 9) default-random-source))
    => 10)

  (check
      (every (lambda (n)
	       (and (integer? n) (<= 0 n) (< n 10)))
	(random-list-shuffle '(0 1 2 3 4 5 6 7 8 9) default-random-source))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((sampler (random-list-sample '(0 1 2 3 4 5 6 7 8 9) default-random-source))
	 (obj	(sampler)))

    (check (integer? obj) => #t)
    (check (and (<= 0 obj) (< obj 10)) => #t))

;;; --------------------------------------------------------------------

  (let ((sampler (random-list-sample-population '(0 1 2 3 4 5 6 7 8 9) 5 default-random-source)))

;;     (write (sampler))(newline)
;;     (write (sampler))(newline)
;;     (write (sampler))(newline)
;;     (write (sampler))(newline)

    (check
	(list? (sampler))
      => #t)

    (check
	(length (sampler))
      => 5)

    (check
	(every (lambda (n)
		 (and (integer? n) (<= 0 n) (< n 10)))
	  (sampler))
      => #t))

  )


(parameterise ((check-test-name 'vector))

  (let* ((perm-maker (random-permutations-maker default-random-source)))

    (check
	(vector? (random-vector-shuffle (perm-maker 10) default-random-source))
      => #t)

    (check
	(vector-length (random-vector-shuffle (perm-maker 10) default-random-source))
      => 10)

    (check
	(vector-every (lambda (n)
			(and (integer? n) (<= 0 n) (< n 10)))
		      (random-vector-shuffle (perm-maker 10) default-random-source))
      => #t)

;;; --------------------------------------------------------------------

    (let* ((sampler (random-vector-sample (perm-maker 10) default-random-source))
	   (obj	(sampler)))

      (check (integer? obj) => #t)
      (check (and (<= 0 obj) (< obj 10)) => #t))

;;; --------------------------------------------------------------------

    (let ((sampler (random-vector-sample-population (perm-maker 10) 5 default-random-source)))

      (check
	  (vector? (sampler))
	=> #t)

      (check
	  (vector-length (sampler))
	=> 5)

      (check
	  (vector-every (lambda (n)
			  (and (integer? n) (<= 0 n) (< n 10)))
			(sampler))
	=> #t))

    )

;;; --------------------------------------------------------------------

  (let ()
    (define (test-random-integers-with-sum requested-sum number-of-numbers
					   range-min-inclusive range-max-inclusive)
      (let ((obj (random-integers-with-sum requested-sum number-of-numbers
					   range-min-inclusive range-max-inclusive
					   default-random-source)))
	;; (write (list 'doing requested-sum number-of-numbers
	;; 	     range-min-inclusive range-max-inclusive))(newline)
	;; (write obj)(newline)
	(check-for-true (vector? obj))
	(check-for-true (= number-of-numbers (vector-length obj)))
	(check-for-true (vector-every integer? obj))
	(do ((i 0 (+ 1 i)))
	    ((= i number-of-numbers))
	  (check-for-true (let ((n (vector-ref obj i)))
;;;			    (write (list range-min-inclusive n range-max-inclusive))(newline)
			    (<= range-min-inclusive n range-max-inclusive))))
	(check (vector-fold-left + 0 obj) => requested-sum)))

    (test-random-integers-with-sum 25 8 0 10)
    (test-random-integers-with-sum 25 8 0 5)
    (test-random-integers-with-sum 50 8 0 20)
    (test-random-integers-with-sum 50 8 3 10)
    (test-random-integers-with-sum 0 8 -10 2)
    (test-random-integers-with-sum 50 8 -10 20)
    (test-random-integers-with-sum -400 8 -50 -20)
    #f)

  (let ()
    (define (test-random-reals-with-sum requested-sum number-of-numbers
					range-min-exclusive range-max-exclusive)
      (let* ((epsilon	1e-6)
	     (obj	(random-reals-with-sum requested-sum epsilon
					       number-of-numbers
					       range-min-exclusive range-max-exclusive
					       default-random-source)))
;;;	(write obj)(newline)
	(check-for-true (vector? obj))
	(check-for-true (= number-of-numbers (vector-length obj)))
	(check-for-true (vector-every real? obj))
	(do ((i 0 (+ 1 i)))
	    ((= i number-of-numbers))
	  (check-for-true (< range-min-exclusive (vector-ref obj i) range-max-exclusive)))
	(check
	    (vector-fold-left + 0 obj)
	  (=> (lambda (a b)
		(> 1e-6 (abs (- a b)))))
	  requested-sum)
    #f))

    (test-random-reals-with-sum 25 8 0 10)
    (test-random-reals-with-sum 25 8 0 5)
    (test-random-reals-with-sum 50 8 0 20)
    (test-random-reals-with-sum 50 8 3 10)
    (test-random-reals-with-sum 0 8 -10 2)
    (test-random-reals-with-sum 50 8 -10 20)
    (test-random-reals-with-sum -400 8 -50 -20)
    #f)

  #t)


(parameterise ((check-test-name 'string))

  (check
      (string? (random-string-shuffle "abcdefghlm" default-random-source))
    => #t)

  (check
      (string-length (random-string-shuffle "abcdefghlm" default-random-source))
    => 10)

  (check
      (string-every (lambda (n) (and (char<=? #\a n) (char<? n #\n)))
		    (random-string-shuffle "abcdefghlm" default-random-source))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((sampler (random-string-sample "abcdefghlm" default-random-source))
	 (obj	(sampler)))

    (check (char? obj) => #t)
    (check (and (char<=? #\a obj) (char<? obj #\n)) => #t))

;;; --------------------------------------------------------------------

  (let ((sampler (random-string-sample-population "abcdefghlm" 5 default-random-source)))

    (check
	(string? (sampler))
      => #t)

    (check
	(string-length (sampler))
      => 5)

    (check
	(string-every (lambda (n) (and (char<=? #\a n) (char<? n #\n)))
		      (sampler))
      => #t))


  )


(parameterise ((check-test-name 'marsaglia-cong))

;;; This is commented out because it takes a lot of time
;;;
;;; (let* ((source	(make-random-source/marsaglia/cong))
;;; 	 (make-integer	(random-source-integers-maker source))
;;; 	 (uint32	(lambda () (make-integer const:2^32))))
;;;   (check
;;; 	(do ((i 1 (+ 1 i))
;;; 	     (k (uint32) (uint32)))
;;; 	    ((= i 1000000)
;;; 	     k))
;;;     => 1529210297))

  (let* ((source	(make-random-source/marsaglia/cong))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100))))
    )

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/marsaglia/cong))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/cong))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/cong))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/marsaglia/cong))
	  (source-b	(make-random-source/marsaglia/cong))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/marsaglia/cong) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/marsaglia/cong))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/marsaglia/cong))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/marsaglia/cong))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'marsaglia-fib))

;;; This is commented out because it takes a lot of time
;;;
;;; (let* ((source	(make-random-source/marsaglia/fib))
;;; 	 (make-integer	(random-source-integers-maker source))
;;; 	 (uint32	(lambda () (make-integer const:2^32))))
;;;   (check
;;; 	(do ((i 1 (+ 1 i))
;;; 	     (k (uint32) (uint32)))
;;; 	    ((= i 1000000)
;;; 	     k))
;;;     => 3519793928))

  (let* ((source	(make-random-source/marsaglia/fib))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100))))
    )

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/marsaglia/fib))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/fib))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/fib))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/marsaglia/fib))
	  (source-b	(make-random-source/marsaglia/fib))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/marsaglia/fib) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/marsaglia/fib))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/marsaglia/fib))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/marsaglia/fib))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'marsaglia-kiss))

;;; This is commented out because it takes a lot of time
;;;
;;;   (let* ((source	(make-random-source/marsaglia/kiss))
;;; 	 (make-integer	(random-source-integers-maker source))
;;; 	 (uint32	(lambda () (make-integer const:2^32))))
;;;     (check
;;; 	(do ((i 1 (+ 1 i))
;;; 	     (k (uint32) (uint32)))
;;; 	    ((= i 1000000)
;;; 	     k))
;;;       => 1372460312))

  (let* ((source	(make-random-source/marsaglia/kiss))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100))))
    )

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/marsaglia/kiss))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/kiss))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/kiss))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/marsaglia/kiss))
	  (source-b	(make-random-source/marsaglia/kiss))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/marsaglia/kiss) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/marsaglia/kiss))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/marsaglia/kiss))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/marsaglia/kiss))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'marsaglia-lfib4))

;;; This is commented out because it takes a lot of time
;;;
;;;   (let* ((source	(make-random-source/marsaglia/lfib4))
;;; 	 (make-integer	(random-source-integers-maker source))
;;; 	 (uint32	(lambda () (make-integer const:2^32))))
;;;     (check
;;; 	(do ((i 1 (+ 1 i))
;;; 	     (k (uint32) (uint32)))
;;; 	    ((= i 1000000)
;;; 	     k))
;;;       => 1064612766))

  (let* ((source	(make-random-source/marsaglia/lfib4))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100))))
    )

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/marsaglia/lfib4))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/lfib4))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/lfib4))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/marsaglia/lfib4))
	  (source-b	(make-random-source/marsaglia/lfib4))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/marsaglia/lfib4) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/marsaglia/lfib4))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/marsaglia/lfib4))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/marsaglia/lfib4))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'marsaglia-mwc))

;;; This is commented out because it takes a lot of time
;;;
;;;   (let* ((source	(make-random-source/marsaglia/mwc))
;;; 	 (make-integer	(random-source-integers-maker source))
;;; 	 (uint32	(lambda () (make-integer const:2^32))))
;;;     (check
;;; 	(do ((i 1 (+ 1 i))
;;; 	     (k (uint32) (uint32)))
;;; 	    ((= i 1000000)
;;; 	     k))
;;;       => 904977562))

  (let* ((source	(make-random-source/marsaglia/mwc))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100))))
    )

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/marsaglia/mwc))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/mwc))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/mwc))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/marsaglia/mwc))
	  (source-b	(make-random-source/marsaglia/mwc))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/marsaglia/mwc) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/marsaglia/mwc))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/marsaglia/mwc))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/marsaglia/mwc))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'marsaglia-shr3))

;;; This is commented out because it takes a lot of time
;;;
;;;   (let* ((source	(make-random-source/marsaglia/shr3))
;;; 	 (make-integer	(random-source-integers-maker source))
;;; 	 (uint32	(lambda () (make-integer const:2^32))))
;;;     (check
;;; 	(do ((i 1 (+ 1 i))
;;; 	     (k (uint32) (uint32)))
;;; 	    ((= i 1000000)
;;; 	     k))
;;;       => 2642725982))

  (let* ((source	(make-random-source/marsaglia/shr3))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100))))
    )

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/marsaglia/shr3))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/shr3))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/shr3))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/marsaglia/shr3))
	  (source-b	(make-random-source/marsaglia/shr3))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/marsaglia/shr3) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/marsaglia/shr3))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/marsaglia/shr3))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/marsaglia/shr3))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'marsaglia-swb))

;;; This is commented out because it takes a lot of time
;;;
;;;   (let* ((source	(make-random-source/marsaglia/swb))
;;; 	 (make-integer	(random-source-integers-maker source))
;;; 	 (uint32	(lambda () (make-integer const:2^32))))
;;;     (check
;;; 	(do ((i 1 (+ 1 i))
;;; 	     (k (uint32) (uint32)))
;;; 	    ((= i 1000000)
;;; 	     k))
;;;       => 627749721))

  (let* ((source	(make-random-source/marsaglia/swb))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100))))
    )

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/marsaglia/swb))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/swb))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/marsaglia/swb))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/marsaglia/swb))
	  (source-b	(make-random-source/marsaglia/swb))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/marsaglia/swb) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/marsaglia/swb))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/marsaglia/swb))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/marsaglia/swb))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'mersenne))

  (let* ((source	(make-random-source/mersenne))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/mersenne))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/mersenne))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/mersenne))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/mersenne))
	  (source-b	(make-random-source/mersenne))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/mersenne) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/mersenne))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/mersenne))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/mersenne))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'borosh))

  (let* ((source	(make-random-source/borosh))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/borosh))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/borosh))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/borosh))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/borosh))
	  (source-b	(make-random-source/borosh))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/borosh) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/borosh))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/borosh))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/borosh))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'cmrg))

  (let* ((source	(make-random-source/cmrg))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/cmrg))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/cmrg))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/cmrg))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/cmrg))
	  (source-b	(make-random-source/cmrg))
	  (make-integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a make-integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

  (check
      (begin
	(random-source-jumpahead! (make-random-source/cmrg) 10)
	(let ((make-integer (random-source-integers-maker (make-random-source/cmrg))))
	  (integer? (make-integer 10))))
    => #t)

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/cmrg))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/cmrg))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'blum-blum-shub))

  (define (seed-it source)
    (random-source-seed! source (let ((ell '(1856429723 162301391051)))
				  (lambda (U)
				    (if (null? ell)
					(random-integer (bitwise-copy-bit 0 32 1))
				      (begin0
					  (car ell)
					(set! ell (cdr ell))))))))

  (let* ((source	(make-random-source/blum-blum-shub))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))
    (seed-it source)

;;     (do ((i 0 (+ 1 i)))
;; 	((= i 100))
;;       (write (integer))
;;       (newline))

    (check-for-true (integer? (integer)))
    (check-for-true (non-negative? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100))))
    )

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/blum-blum-shub))
	 (make-real	(random-source-reals-maker source)))

    (seed-it source)
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/blum-blum-shub))
	 (make-real	(random-source-reals-maker source 1e-30)))
    (seed-it source)
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

  (let* ((source	(make-random-source/blum-blum-shub))
	 (make-real	(random-source-reals-maker source 1e-5)))
    (seed-it source)
    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (let* ((source		(let ((s (make-random-source/blum-blum-shub)))
				  (seed-it s)
				  s))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj			(bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(let ((s (make-random-source/blum-blum-shub)))
			  (seed-it s)
			  s))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


;;;; examples

(when #f

  (display "Example of random lists:\n")
  (do ((i 0 (+ 1 i)))
      ((= i 5))
    (write (random-list-unfold-numbers (lambda ()
					 (random-integer 10))
				       10))
    (newline))


  (display "Example of random vectors:\n")
  (do ((i 0 (+ 1 i)))
      ((= i 5))
    (write (random-vector-unfold-numbers (lambda ()
					   (random-integer 10))
					 10))
    (newline))

  (display "Example of random strings:\n")
  (do ((i 0 (+ 1 i)))
      ((= i 5))
    (write (random-string-unfold-chars (lambda ()
					   (+ 65 (random-integer 21)))
					 10))
    (newline))

  (display "Example of random passwords of printable characters:\n")
  (do ((i 0 (+ 1 i)))
      ((= i 5))
    (display (random-string-unfold-chars
	      (lambda ()
		(do ((ch (random-integer 127) (random-integer 127)))
		    ((char-set-contains? char-set:ascii/graphic (integer->char ch))
		     ch)))
	      10))
    (newline))

  (let ((sampler (random-source-integers-maker-from-range default-random-source 0 10)))
    (display "integer samples from [0, 10]:\n")
    (do ((i 0 (+ 1 i)))
	((= i 10))
      (write (sampler))(newline)))

  (let ((sampler (random-source-integers-maker-from-range default-random-source 0 10 2)))
    (display "integer samples from [0, 10], step 2:\n")
    (do ((i 0 (+ 1 i)))
	((= i 10))
      (write (sampler))(newline)))

  (let ((sampler (random-source-reals-maker-from-range default-random-source 0 10)))
    (display "real samples from (0, 10):\n")
    (do ((i 0 (+ 1 i)))
	((= i 10))
      (write (sampler))(newline)))

  (let ((sampler (random-source-reals-maker-from-range default-random-source 0 10 1.2)))
    (display "real samples from [0, 10), step 1.2:\n")
    (do ((i 0 (+ 1 i)))
	((= i 10))
      (write (sampler))(newline)))

  )


;;;; done

(check-report)

;;; end of file
