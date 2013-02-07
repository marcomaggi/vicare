;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 43
;;;Date: Thu Feb  7, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.
;;;


#!r6rs
(import (except (rnrs base)
                vector-fill!
                vector->list
                list->vector
                vector-map
                vector-for-each)
  (rnrs lists)
  (rnrs io simple)
  (srfi :6 basic-string-ports)
  (srfi :43 vectors)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI 43: vectors\n")


(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(check
    (vector? (make-vector 0))
  => #t)

(check
    (vector-length (make-vector 10))
  => 10)

(check
    (vector-ref (make-vector 500 97) 499)
  => 97)

(check
    (vector)
  => '#())

(check
    (vector 'a 'b 97)
  => '#(a b 97))

(check
    (vector-unfold (lambda (i x) (values x (- x 1)))
		   10 0)
  => '#(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)) ; but SRFI 43 says -8 -8 at end

(check
    (vector-unfold values 10)
  => '#(0 1 2 3 4 5 6 7 8 9))

(let ((V '#(a b 97)))
  (check
      (vector-unfold (lambda (i) (vector-ref V i))
		     (vector-length V))
    => V))

(check
    (vector-unfold-right (lambda (i x) (values x (+ x 1))) 8 0)
  => '#(7 6 5 4 3 2 1 0))

(let ((vector '#(3 1 4 5 9)))
  (check
      (vector-unfold-right (lambda (i x)
			     (values (vector-ref vector x) (+ x 1)))
			   (vector-length vector)
			   0)
    => '#(9 5 4 1 3)))

(check
    (vector-copy '#(a b c d e f g h i))
  => '#(a b c d e f g h i))

(check
    (vector-copy '#(a b c d e f g h i) 6)
  => '#(g h i))

(check
    (vector-copy '#(a b c d e f g h i) 3 6)
  => '#(d e f))

(check
    (vector-copy '#(a b c d e f g h i) 6 12 'x)
  => '#(g h i x x x))

(check
    (vector-reverse-copy '#(5 4 3 2 1 0) 1 5)
  => '#(1 2 3 4))

(check
    (vector-append '#(x) '#(y))
  => '#(x y))

(check
    (vector-append '#(a) '#(b c d))
  => '#(a b c d))

(check
    (vector-append '#(a #(b)) '#(#(c)))
  => '#(a #(b) #(c)))

(check
    (vector-concatenate '(#(a b) #(c d)))
  => '#(a b c d))

(check
    (and (eq? (vector? '#(a b c)) #t)
         (eq? (vector? '(a b c)) #f)
         (eq? (vector? #t) #f)
         (eq? (vector? '#()) #t)
         (eq? (vector? '()) #f))
  => #t)

(check
    (and (eq? (vector-empty? '#(a)) #f)
         (eq? (vector-empty? '#(())) #f)
         (eq? (vector-empty? '#(#())) #f)
         (eq? (vector-empty? '#()) #t))
  => #t)

(check
    (and (eq? (vector= eq? '#(a b c d) '#(a b c d)) #t)
         (eq? (vector= eq? '#(a b c d) '#(a b d c)) #f)
         (eq? (vector= = '#(1 2 3 4 5) '#(1 2 3 4)) #f)
         (eq? (vector= = '#(1 2 3 4) '#(1 2 3 4))   #t)
         (eq? (vector= eq?) #t)
         (eq? (vector= eq? '#(a)) #t)
         (eq? (vector= eq? (vector (vector 'a)) (vector (vector 'a))) #f)
         (eq? (vector= equal? (vector (vector 'a)) (vector (vector 'a))) #t))
  => #t)

(check
    (eq? (vector-ref '#(a b c d) 2)
	 'c)
  => #t)

(check
    (eq? (vector-length '#(a b c)) 3)
  => #t)

(check
    (vector-fold (lambda (index len str) (max (string-length str) len))
		 0 '#("a" "b" "" "dd" "e"))
  => 2)

(check
    (vector-fold (lambda (index tail elt) (cons elt tail))
		 '() '#(0 1 2 3 4))
  => '(4 3 2 1 0))

(check
    (vector-fold (lambda (index counter n)
		   (if (even? n) (+ counter 1) counter))
		 0 '#(0 1 2 3 4 4 4 5 6 7))
  => 6)

(check
    (vector-fold-right (lambda (index tail elt) (cons elt tail))
      '() '#(a b c d))
  => '(a b c d))

(check
    (vector-map (lambda (i x) (* x x))
      (vector-unfold (lambda (i x) (values x (+ x 1))) 4 1))
  => '#(1 4 9 16))

(check
    (vector-map (lambda (i x y) (* x y))
      (vector-unfold (lambda (i x) (values x (+ x 1))) 5 1)
      (vector-unfold (lambda (i x) (values x (- x 1))) 5 5))
  => '#(5 8 9 8 5))

(check
    (and (member (let ((count 0))
		   (vector-map (lambda (ignored-index ignored-elt)
				 (set! count (+ count 1))
				 count)
		     '#(a b)))
		 '(#(1 2) #(2 1)))
	 #t)
  => #t)

(check
    (let ((v (vector 1 2 3 4)))
      (vector-map! (lambda (i elt) (+ i elt)) v)
      v)
  => '#(1 3 5 7))

(check
    (let ((p (open-output-string)))
      (vector-for-each (lambda (i x) (display x p) (newline p))
	'#("foo" "bar" "baz" "quux" "zot"))
      (get-output-string p))
  => "foo\nbar\nbaz\nquux\nzot\n")

(check
    (vector-count (lambda (i elt) (even? elt)) '#(3 1 4 1 5 9 2 5 6))
  => 3)

(check
    (vector-count (lambda (i x y) (< x y))
		  '#(1 3 6 9)
		  '#(2 4 6 8 10 12))
  => 2)

(check
    (vector-index even? '#(3 1 4 1 5 9))
  => 2)

(check
    (vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
  => 1)

(check
    (vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
  => #f)

(check
    (vector-index-right even? '#(3 1 4 1 5 9))
  => 2)

(check
    (vector-index-right < '#(3 1 4 1 5) '#(2 7 1 8 2))
  => 3)

(check
    (vector-index-right = '#(3 1 4 1 5) '#(2 7 1 8 2))
  => #f)

(check
    (vector-skip even? '#(3 1 4 1 5 9))
  => 0)

(check
    (vector-skip < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
  => 0)

(check
    (vector-skip = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
  => 0)

(check
    (vector-skip > '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
  => 1)

(check
    (vector-skip-right even? '#(3 1 4 1 5 9))
  => 5)

(check
    (vector-skip-right < '#(3 1 4 1 5) '#(2 7 1 8 2))
  => 4)

(check
    (vector-skip-right = '#(3 1 4 1 5) '#(2 7 1 8 2))
  => 4)

(check
    (vector-skip-right > '#(3 1 4 1 5) '#(2 7 1 8 2))
  => 3)

(define (string-comparator s1 s2)
  (cond ((< (string-length s1) (string-length s2))
         -1)
        ((> (string-length s1) (string-length s2))
         +1)
        ((string<? s1 s2)
         -1)
        ((string>? s1 s2)
         +1)
        (else
         0)))

(check
    (vector-binary-search '#()
			  "bad"
			  string-comparator)
  => #f)

(check
    (vector-binary-search '#("ab" "cd" "ef" "bcd" "cde" "aaaa")
			  "bad"
			  string-comparator)
  => #f)

(check
    (vector-binary-search '#("ab" "cd" "ef" "bcd" "cde" "aaaa")
			  ""
			  string-comparator)
  => #f)

(check
    (vector-binary-search '#("ab" "cd" "ef" "bcd" "cde" "aaaa")
			  "hello"
			  string-comparator)
  => #f)

(check
    (vector-binary-search '#("ab" "cd" "ef" "bcd" "cde" "aaaa")
			  "ab"
			  string-comparator)
  => 0)

(check
    (vector-binary-search '#("ab" "cd" "ef" "bcd" "cde" "aaaa")
			  "aaaa"
			  string-comparator)
  => 5)

(check
    (vector-binary-search '#("ab" "cd" "ef" "bcd" "cde" "aaaa")
			  "bcd"
			  string-comparator)
  => 3)

(check
    (vector-any list '#() '#(a b c))
  => #f)

(check
    (vector-any list '#(a b c) '#())
  => #f)

(check
    (vector-any list '#(a b c) '#(d))
  => '(a d))

(check
    (vector-any memq '#(a b c) '#(() (c d e) (b c 97)))
  => '(c 97))

(check
    (vector-every list '#() '#(a b c))
  => #t)

(check
    (vector-every list '#(a b c) '#())
  => #t)

(check
    (vector-every list '#(a b c) '#(d))
  => '(a d))

(check
    (vector-every memq '#(a b c) '#(() (c d e) (b c 97)))
  => #f)

(check
    (let ((v (vector 0 1 2 3)))
      (vector-set! v 1 11)
      v)
  => '#(0 11 2 3))

(check
    (let ((v (vector 0 1 2 3)))
      (vector-swap! v 1 3)
      v)
  => '#(0 3 2 1))

(check
    (let ((v (vector)))
      (vector-fill! v 97)
      v)
  => '#())

(check
    (let ((v (vector 0 1 2 3)))
      (vector-fill! v 97)
      v)
  => '#(97 97 97 97))

(check
    (let ((v (vector 0 1 2 3)))
      (vector-fill! v 97 1)
      v)
  => '#(0 97 97 97))

(check
    (let ((v (vector 0 1 2 3)))
      (vector-fill! v 97 1 2)
      v)
  => '#(0 97 2 3))

(check
    (let ((v (vector)))
      (vector-reverse! v)
      v)
  => '#())

(check
    (let ((v (vector 0 1 2 3)))
      (vector-reverse! v)
      v)
  => '#(3 2 1 0))

(check
    (let ((v (vector 0 1 2 3)))
      (vector-reverse! v 1)
      v)
  => '#(0 3 2 1))

(check
    (let ((v (vector 0 1 2 3)))
      (vector-reverse! v 1 3)
      v)
  => '#(0 2 1 3))

(check
    (let ((v (vector))
	  (src '#(100 101 102 103 104 105)))
      (vector-copy! v 0 v)
      v)
  => '#())

(check
    (let ((v (vector 0 1 2 3 4 5))
	  (src '#(100 101 102 103 104 105)))
      (vector-copy! v 0 src)
      v)
  => '#(100 101 102 103 104 105))

(check
    (let ((v (vector 0 1 2 3))
	  (src '#(100 101 102 103 104 105)))
      (vector-copy! v 1 src 4)
      v)
  => '#(0 104 105 3))

(check
    (let ((v (vector 0 1 2 3))
	  (src '#(100 101 102 103 104 105)))
      (vector-copy! v 1 src 2 4)
      v)
  => '#(0 102 103 3))

(check
    (let ((v (vector))
	  (src '#(100 101 102 103 104 105)))
      (vector-reverse-copy! v 0 v)
      v)
  => '#())

(check
    (let ((v (vector 0 1 2 3 4 5))
	  (src '#(100 101 102 103 104 105)))
      (vector-reverse-copy! v 0 src)
      v)
  => '#(105 104 103 102 101 100))

(check
    (let ((v (vector 0 1 2 3))
	  (src '#(100 101 102 103 104 105)))
      (vector-reverse-copy! v 1 src 4)
      v)
  => '#(0 105 104 3))

(check
    (let ((v (vector 0 1 2 3))
	  (src '#(100 101 102 103 104 105)))
      (vector-reverse-copy! v 1 src 2 4)
      v)
  => '#(0 103 102 3))

(check
    (vector->list '#())
  => '())

(check
    (vector->list '#(a b c))
  => '(a b c))

(check
    (vector->list '#(a b c d e) 1)
  => '(b c d e))

(check
    (vector->list '#(a b c d e) 1 4)
  => '(b c d))

(check
    (reverse-vector->list '#())
  => '())

(check
    (reverse-vector->list '#(a b c))
  => '(c b a))

(check
    (reverse-vector->list '#(a b c d e) 1)
  => '(e d c b))

(check
    (reverse-vector->list '#(a b c d e) 1 3)
  => '(c b))

(check
    (list->vector '())
  => '#())

(check
    (list->vector '(a b c))
  => '#(a b c))

(check
    (reverse-list->vector '())
  => '#())

(check
    (reverse-list->vector '(a b c))
  => '#(c b a))


;;;; done

(check-report)

;;; end of file
