;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for (vicare language-extensions streams)
;;;Date: Sun Sep 20, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2007 by Philip L. Bewig of Saint Louis, Missouri, USA.
;;;All rights reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the  following  conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or  substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!r6rs
(import (vicare)
  (vicare language-extensions streams)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare language extensions: streams\n")


;;;; helpers

(define-syntax check-error-message
  (syntax-rules (=>)
    ((_ ?expr => ?result)
     (check (guard (E (else (condition-message E))) ?expr) => ?result))))

(define (lsec proc . args)
  (lambda x
    (apply proc (append args x))))

(define (rsec proc . args)
  (lambda x
    (apply proc (reverse (append (reverse args) (reverse x))))))

(define-stream (qsort lt? strm)
  (if (stream-null? strm)
      stream-null
      (let ((x (stream-car strm))
            (xs (stream-cdr strm)))
        (stream-append
          (qsort lt?
            (stream-filter
              (lambda (u) (lt? u x))
              xs))
          (stream x)
          (qsort lt?
            (stream-filter
              (lambda (u) (not (lt? u x)))
              xs))))))

(define-stream (isort lt? strm)
    (define-stream (insert strm x)
      (stream-match strm
        (() (stream x))
        ((y . ys)
          (if (lt? y x)
              (stream-cons y (insert ys x))
              (stream-cons x strm)))))
    (stream-fold insert stream-null strm))

(define-stream (stream-merge lt? . strms)
  (define-stream (merge xx yy)
    (stream-match xx (() yy) ((x . xs)
      (stream-match yy (() xx) ((y . ys)
        (if (lt? y x)
            (stream-cons y (merge xx ys))
            (stream-cons x (merge xs yy))))))))
  (stream-let loop ((strms strms))
    (cond ((null? strms) stream-null)
          ((null? (cdr strms)) (car strms))
          (else (merge (car strms)
                       (apply stream-merge lt?
                         (cdr strms)))))))

(define-stream (msort lt? strm)
  (let* ((n (div (stream-length strm) 2))
         (ts (stream-take n strm))
         (ds (stream-drop n strm)))
    (if (zero? n)
        strm
        (stream-merge lt?
          (msort < ts) (msort < ds)))))

(define-stream (stream-unique eql? strm)
  (if (stream-null? strm)
      stream-null
      (stream-cons (stream-car strm)
        (stream-unique eql?
          (stream-drop-while
            (lambda (x)
              (eql? (stream-car strm) x))
            strm)))))

(define nats
  (stream-cons 0
    (stream-map add1 nats)))

(define hamming
  (stream-unique =
    (stream-cons 1
      (stream-merge <
        (stream-map (lsec * 2) hamming)
        (stream-merge <
          (stream-map (lsec * 3) hamming)
          (stream-map (lsec * 5) hamming))))))

(define primes
  (let ()
    (define-stream (next base mult strm)
      (let ((first (stream-car strm))
	    (rest (stream-cdr strm)))
	(cond ((< first mult)
	       (stream-cons first
			    (next base mult rest)))
	      ((< mult first)
	       (next base (+ base mult) strm))
	      (else (next base
			  (+ base mult) rest)))))
    (define-stream (sift base strm)
      (next base (+ base base) strm))
    (define-stream (sieve strm)
      (let ((first (stream-car strm))
	    (rest (stream-cdr strm)))
	(stream-cons first
		     (sieve (sift first rest)))))
    (sieve (stream-from 2))))


(parametrise ((check-test-name	'stream-null))
  (check (stream? stream-null) => #t)
  (check (stream-null? stream-null) => #t)
  (check (stream-pair? stream-null) => #f)
  #t)


(parametrise ((check-test-name	'stream-cons))
  (check (stream?      (stream-cons 1 stream-null)) => #t)
  (check (stream-null? (stream-cons 1 stream-null)) => #f)
  (check (stream-pair? (stream-cons 1 stream-null)) => #t)
  #t)


(parametrise ((check-test-name	'stream?))
  (check (stream? stream-null) => #t)
  (check (stream? (stream-cons 1 stream-null)) => #t)
  (check (stream? "four") => #f)
  #t)


(parametrise ((check-test-name	'stream-null?))
  (check (stream-null? stream-null) => #t)
  (check (stream-null? (stream-cons 1 stream-null)) => #f)
  (check (stream-null? "four") => #f)
  #t)


(parametrise ((check-test-name	'stream-pair?))
  (check (stream-pair? stream-null) => #f)
  (check (stream-pair? (stream-cons 1 stream-null)) => #t)
  (check (stream-pair? "four") => #f)
  #t)


(parametrise ((check-test-name	'stream-car))

  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-car "four") => "non stream")
  (check-error-message (stream-car stream-null) => "null stream")
  (check (stream-car strm123) => 1)
  (check (stream-car (stream-cdr strm123)) => 2)
  (check (stream-car (stream-cdr (stream-cdr strm123))) => 3)

  #t)


(parametrise ((check-test-name	'stream-cdr))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-cdr "four") => "non stream")
  (check-error-message (stream-cdr stream-null) => "null stream")
  (check (stream-car (stream-cdr strm123)) => 2)
  #t)


(parametrise ((check-test-name	'stream-lambda))
  (define strm123
    (stream 1 2 3))

  (check
      (stream->list
       (letrec ((double
		 (stream-lambda (strm)
				(if (stream-null? strm)
				    stream-null
				  (stream-cons
				   (* 2 (stream-car strm))
				   (double (stream-cdr strm)))))))
	 (double strm123)))
    => '(2 4 6))
  #t)


(parametrise ((check-test-name	'define-stream))
  (define strm123
    (stream 1 2 3))

  (check
      (stream->list
       (let ()
	 (define-stream (double strm)
	   (if (stream-null? strm)
	       stream-null
	     (stream-cons
	      (* 2 (stream-car strm))
	      (double (stream-cdr strm)))))
	 (double strm123)))
    => '(2 4 6))
  #t)


(parametrise ((check-test-name	'list->stream))
  (check-error-message (list->stream "four") => "non-list argument")
  (check (stream->list (list->stream '())) => '())
  (check (stream->list (list->stream '(1 2 3))) => '(1 2 3))
  #t)


(parametrise ((check-test-name	'port->stream))
  ;; (let* ((p (open-input-file "streams.ss"))
  ;;        (s (port->stream p)))
  ;;   (check (port->stream "four") => "port->stream: non-input-port argument")
  ;;   (check (string=? (list->string (stream->list s 11)) "; Copyright") => #t)
  ;;   (close-input-port p))
  #t)


(parametrise ((check-test-name	'the-stream))
  (check (stream->list (stream)) => '())
  (check (stream->list (stream 1)) => '(1))
  (check (stream->list (stream 1 2 3)) => '(1 2 3))
  #t)


(parametrise ((check-test-name	'stream->list))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream->list '()) => "non-stream argument")
  (check-error-message (stream->list strm123 "four") => "non-integer count")
  (check-error-message (stream->list strm123 -1) => "negative count")
  (check (stream->list (stream)) => '())
  (check (stream->list strm123) => '(1 2 3))
  (check (stream->list strm123 5) => '(1 2 3))
  (check (stream->list (stream-from 1) 3) => '(1 2 3))
  #t)


(parametrise ((check-test-name	'stream-append))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-append "four") => "non-stream argument")
  (check (stream->list (stream-append strm123)) => '(1 2 3))
  (check (stream->list (stream-append strm123 strm123)) => '(1 2 3 1 2 3))
  (check (stream->list (stream-append strm123 strm123 strm123)) => '(1 2 3 1 2 3 1 2 3))
  (check (stream->list (stream-append strm123 stream-null)) => '(1 2 3))
  (check (stream->list (stream-append stream-null strm123)) => '(1 2 3))
  #t)


(parametrise ((check-test-name	'stream-concat))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-concat "four") => "non-stream argument")
  (check (stream->list (stream-concat (stream strm123))) => '(1 2 3))
  (check (stream->list (stream-concat (stream strm123 strm123))) => '(1 2 3 1 2 3))
  #t)


(parametrise ((check-test-name	'stream-constant))
  (check (stream-ref (stream-constant 1) 100) => 1)
  (check (stream-ref (stream-constant 1 2) 100) => 1)
  (check (stream-ref (stream-constant 1 2 3) 3) => 1)
  #t)


(parametrise ((check-test-name	'stream-drop))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-drop "four" strm123) => "non-integer argument")
  (check-error-message (stream-drop -1 strm123) => "negative argument")
  (check-error-message (stream-drop 2 "four") => "non-stream argument")
  (check (stream->list (stream-drop 0 stream-null)) => '())
  (check (stream->list (stream-drop 0 strm123)) => '(1 2 3))
  (check (stream->list (stream-drop 1 strm123)) => '(2 3))
  (check (stream->list (stream-drop 5 strm123)) => '())
  #t)


(parametrise ((check-test-name	'stream-drop-while))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-drop-while "four" strm123) => "non-procedural argument")
  (check-error-message (stream-drop-while odd? "four") => "non-stream argument")
  (check (stream->list (stream-drop-while odd? stream-null)) => '())
  (check (stream->list (stream-drop-while odd? strm123)) => '(2 3))
  (check (stream->list (stream-drop-while even? strm123)) => '(1 2 3))
  (check (stream->list (stream-drop-while positive? strm123)) => '())
  (check (stream->list (stream-drop-while negative? strm123)) => '(1 2 3))
  #t)


(parametrise ((check-test-name 'stream-filter))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-filter "four" strm123) => "non-procedural argument")
  (check-error-message (stream-filter odd? '()) => "non-stream argument")
  (check (stream-null? (stream-filter odd? (stream))) => #t)
  (check (stream->list (stream-filter odd? strm123)) => '(1 3))
  (check (stream->list (stream-filter even? strm123)) => '(2))
  (check (stream->list (stream-filter positive? strm123)) => '(1 2 3))
  (check (stream->list (stream-filter negative? strm123)) => '())
  (let loop ((n 10))
    (check (odd? (stream-ref (stream-filter odd? (stream-from 0)) n)) => #t)
    (if (positive? n) (loop (- n 1))))
  (let loop ((n 10))
    (check (even? (stream-ref (stream-filter odd? (stream-from 0)) n)) => #f)
    (if (positive? n) (loop (- n 1))))
  #t)


(parametrise ((check-test-name	'stream-fold))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-fold "four" 0 strm123) => "non-procedural argument")
  (check-error-message (stream-fold + 0 '()) => "non-stream argument")
  (check (stream-fold + 0 strm123) => 6)
  #t)


(parametrise ((check-test-name	'stream-for-each))
 (define strm123
   (stream 1 2 3))

  (check-error-message (stream-for-each "four" strm123) => "non-procedural argument")
  (check-error-message (stream-for-each display) => "no stream arguments")
  (check-error-message (stream-for-each display "four") => "non-stream argument")
  (check
      (let ((sum 0))
	(stream-for-each (lambda (x)
			   (set! sum (+ sum x)))
			 strm123)
	sum)
    => 6)
  #t)


(parametrise ((check-test-name	'stream-from))
  (check-error-message (stream-from "four") => "non-numeric starting number")
  (check-error-message (stream-from 1 "four") => "non-numeric step size")
  (check (stream-ref (stream-from 0) 100) => 100)
  (check (stream-ref (stream-from 1 2) 100) => 201)
  (check (stream-ref (stream-from 0 -1) 100) => -100)
  #t)


(parametrise ((check-test-name	'stream-iterate))
  (check-error-message (stream-iterate "four" 0) => "non-procedural argument")

  (check (stream->list (stream-iterate (lsec + 1) 1) 3) => '(1 2 3))
  #t)


(parametrise ((check-test-name	'stream-length))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-length "four") => "non-stream argument")
  (check (stream-length (stream)) => 0)
  (check (stream-length strm123) => 3)
  #t)


(parametrise ((check-test-name	'stream-let))
  (define strm123
    (stream 1 2 3))

  (check
      (stream->list
       (stream-let loop ((strm strm123))
		   (if (stream-null? strm)
		       stream-null
		     (stream-cons
		      (* 2 (stream-car strm))
		      (loop (stream-cdr strm))))))
    => '(2 4 6))
  #t)


(parametrise ((check-test-name	'stream-map))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-map "four" strm123) => "non-procedural argument")
  (check-error-message (stream-map odd?) => "no stream arguments")
  (check-error-message (stream-map odd? "four") => "non-stream argument")
  (check (stream->list (stream-map - strm123)) => '(-1 -2 -3))
  (check (stream->list (stream-map + strm123 strm123)) => '(2 4 6))
  (check (stream->list (stream-map + strm123 (stream-from 1))) => '(2 4 6))
  (check (stream->list (stream-map + (stream-from 1) strm123)) => '(2 4 6))
  (check (stream->list (stream-map + strm123 strm123 strm123)) => '(3 6 9))
  #t)


(parametrise ((check-test-name	'stream-match))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-match '(1 2 3) (_ 'ok)) => "non-stream argument")
  (check-error-message (stream-match strm123 (() 42)) => "pattern failure")
  (check (stream-match stream-null (() 'ok)) => 'ok)
  (check (stream-match strm123 (() 'no) (else 'ok)) => 'ok)
  (check (stream-match (stream 1) (() 'no) ((a) a)) => 1)
  (check (stream-match (stream 1) (() 'no) ((_) 'ok)) => 'ok)
  (check (stream-match strm123 ((a b c) (list a b c))) => '(1 2 3))
  (check (stream-match strm123 ((a . _) a)) => 1)
  (check (stream-match strm123 ((a b . _) (list a b))) => '(1 2))
  (check (stream-match strm123 ((a b . c) (list a b (stream-car c)))) => '(1 2 3))
  (check (stream-match strm123 (s (stream->list s))) => '(1 2 3))
  (check (stream-match strm123 ((a . _) (= a 1) 'ok)) => 'ok)
  (check (stream-match strm123 ((a . _) (= a 2) 'yes) (_ 'no)) => 'no)
  (check (stream-match strm123 ((a b c) (= a b) 'yes) (_ 'no)) => 'no)
  (check (stream-match (stream 1 1 2) ((a b c) (= a b) 'yes) (_ 'no)) => 'yes)
  #t)


(parametrise ((check-test-name	'stream-of))
  (check
      (stream->list
       (stream-of (+ y 6)
		  (x in (stream-range 1 6))
		  (odd? x)
		  (y is (* x x))))
    => '(7 15 31))

  (check
      (stream->list
       (stream-of (* x y)
		  (x in (stream-range 1 4))
		  (y in (stream-range 1 5))))
    => '(1 2 3 4 2 4 6 8 3 6 9 12))

  (check (stream-car (stream-of 1)) => 1)
  #t)


(parametrise ((check-test-name	'stream-range))
  (check-error-message (stream-range "four" 0) => "non-numeric starting number")
  (check-error-message (stream-range 0 "four") => "non-numeric ending number")
;;;FIXME  This is commented  out because  it seems  to fails  during the
;;;expansion phase, causing all the program execution to be aborted.  It
;;;should not happen...
;;;
;;;  (check (stream-range 1 2 "three") => "non-numeric step size")
;;;
  (check (stream->list (stream-range 0 5)) => '(0 1 2 3 4))
  (check (stream->list (stream-range 5 0)) => '(5 4 3 2 1))
  (check (stream->list (stream-range 0 5 2)) => '(0 2 4))
  (check (stream->list (stream-range 5 0 -2)) => '(5 3 1))
  (check (stream->list (stream-range 0 1 -1)) => '())
  #t)


(parametrise ((check-test-name	'stream-ref))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-ref '() 4) => "non-stream argument")
  (check-error-message (stream-ref nats 3.5) => "non-integer argument")
  (check-error-message (stream-ref nats -3) => "negative argument")
  (check-error-message (stream-ref strm123 5) => "beyond end of stream")
  (check (stream-ref strm123 0) => 1)
  (check (stream-ref strm123 1) => 2)
  (check (stream-ref strm123 2) => 3)
  #t)


(parametrise ((check-test-name	'stream-reverse))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-reverse '()) => "non-stream argument")
  (check (stream->list (stream-reverse (stream))) => '())
  (check (stream->list (stream-reverse strm123)) => '(3 2 1))
  #t)


(parametrise ((check-test-name	'stream-scan))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-scan "four" 0 strm123) => "non-procedural argument")
  (check-error-message (stream-scan + 0 '()) => "non-stream argument")
  (check (stream->list (stream-scan + 0 strm123)) => '(0 1 3 6))
  #t)


(parametrise ((check-test-name	'stream-take))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-take 5 "four") => "non-stream argument")
  (check-error-message (stream-take "four" strm123) => "non-integer argument")
  (check-error-message (stream-take -4 strm123) => "negative argument")
  (check (stream->list (stream-take 5 stream-null)) => '())
  (check (stream->list (stream-take 0 stream-null)) => '())
  (check (stream->list (stream-take 0 strm123)) => '())
  (check (stream->list (stream-take 2 strm123)) => '(1 2))
  (check (stream->list (stream-take 3 strm123)) => '(1 2 3))
  (check (stream->list (stream-take 5 strm123)) => '(1 2 3))
  #t)


(parametrise ((check-test-name	'stream-take-while))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-take-while odd? "four") => "non-stream argument")
  (check-error-message (stream-take-while "four" strm123) => "non-procedural argument")
  (check (stream->list (stream-take-while odd? strm123)) => '(1))
  (check (stream->list (stream-take-while even? strm123)) => '())
  (check (stream->list (stream-take-while positive? strm123)) => '(1 2 3))
  (check (stream->list (stream-take-while negative? strm123)) => '())
  #t)


(parametrise ((check-test-name	'stream-unfold))
  (check-error-message (stream-unfold "four" odd? + 0) => "non-procedural mapper")
  (check-error-message (stream-unfold + "four" + 0) => "non-procedural pred?")
  (check-error-message (stream-unfold + odd? "four" 0) => "non-procedural generator")

  (check
      (stream->list (stream-unfold (rsec expt 2) (rsec < 10) (rsec + 1) 0))
    => '(0 1 4 9 16 25 36 49 64 81))
  #t)


(parametrise ((check-test-name	'stream-unfolds))
  (check
      (stream->list
       (stream-unfolds
	(lambda (x)
	  (let ((n (car x)) (s (cdr x)))
	    (if (zero? n)
		(values 'dummy '())
	      (values
	       (cons (- n 1) (stream-cdr s))
	       (list (stream-car s))))))
	(cons 5 (stream-from 0))))
    => '(0 1 2 3 4))
  #t)


(parametrise ((check-test-name	'stream-zip))
  (define strm123
    (stream 1 2 3))

  (check-error-message (stream-zip) => "no stream arguments")
  (check-error-message (stream-zip "four") => "non-stream argument")
  (check-error-message (stream-zip strm123 "four") => "non-stream argument")
  (check (stream->list (stream-zip strm123 stream-null)) => '())
  (check (stream->list (stream-zip strm123)) => '((1) (2) (3)))
  (check (stream->list (stream-zip strm123 strm123)) => '((1 1) (2 2) (3 3)))
  (check (stream->list (stream-zip strm123 (stream-from 1))) => '((1 1) (2 2) (3 3)))
  (check (stream->list (stream-zip strm123 strm123 strm123)) => '((1 1 1) (2 2 2) (3 3 3)))
  #t)


(parametrise ((check-test-name	'other-tests))

  (check
      (stream-car
       (stream-reverse
	(stream-take-while
	 (rsec < 1000)
	 primes)))
    => 997)

  (check
      (equal?
       (stream->list (qsort < (stream 3 1 5 2 4)))
       (stream->list (isort < (stream 2 5 1 4 3))))
    => #t)

  (check
      (equal?
       (stream->list (msort < (stream 3 1 5 2 4)))
       (stream->list (isort < (stream 2 5 1 4 3))))
    => #t)

  ;;  http://www.research.att.com/~njas/sequences/A051037
  (check (stream-ref hamming 999) => 51200000)

  #t)


;;;; done

(check-report)

;;; end of file
