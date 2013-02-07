;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 45, lazy
;;;Date: Thu Feb  7, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright AndrÃ© van Tonder.  All Rights Reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission notice  shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN NO  EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER  IN AN
;;;ACTION OF  CONTRACT, TORT OR  OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION WITH  THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.

;;;Modified by Andreas Rottmann to be an R6RS program.


#!r6rs
(import (vicare)
  (prefix (srfi :45) srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI 45: lazy\n")


(parametrise ((check-test-name	'memoization))

  (check
      (with-result
       (call-with-string-output-port
	   (lambda (port)
	     (let ((s (srfi.delay (begin
				    (display 'hello port)
				    1))))
	       (add-result (srfi.force s))
	       (add-result (srfi.force s))))))
    => '("hello" (1 1)))

  (check
      (with-result
       (call-with-string-output-port
	   (lambda (port)
	     (let ((s (srfi.delay (begin
				    (display 'bonjour port)
				    2))))
	       (add-result (+ (srfi.force s) (srfi.force s)))))))
    => '("bonjour" (4)))

  ;;Pointed out by Alejandro Forero Cuervo.
  (check
      (with-result
       (call-with-string-output-port
	   (lambda (port)
	     (let* ((r (srfi.delay (begin
				     (display 'hi port)
				     1)))
		    (s (srfi.lazy r))
		    (t (srfi.lazy s)))
	       (add-result (srfi.force t))
	       (add-result (srfi.force r))))))
    => '("hi" (1 1)))

;;; --------------------------------------------------------------------

  (let ()
    (define (stream-drop s index)
      (srfi.lazy
       (if (zero? index)
	   s
	 (stream-drop (cdr (srfi.force s)) (- index 1)))))

    (define (ones port)
      (srfi.delay (begin
		    (display 'ho port)
		    (cons 1 (ones port)))))

    (check
	(with-result
	 (call-with-string-output-port
	     (lambda (port)
	       (define s (ones port))
	       (add-result (car (srfi.force (stream-drop s 4))))
	       (add-result (car (srfi.force (stream-drop s 4)))))))
      => '("hohohohoho" (1 1)))
    #f)

  #t)


(parametrise ((check-test-name	'reentrancy))

  ;;From R5RS.
  (check
      (with-result
       (letrec ((count 0)
		(p (srfi.delay (begin
				 (set! count (+ count 1))
				 (if (> count x)
				     count
				   (srfi.force p)))))
		(x 5))
	 (add-result (srfi.force p))
	 (set! x 10)
	 (add-result (srfi.force p))))
    => `(,(void) (6 6)))

  ;;From SRFI 40.
  (check
      (with-result
       (letrec ((f (let ((first? #t))
		     (srfi.delay
		      (if first?
			  (begin
			    (set! first? #f)
			    (srfi.force f))
			'second)))))
	 (add-result (srfi.force f))))
    => `(,(void) (second)))

  ;;Due to John Shutt.
  (check
      (with-result
       (let* ((q (let ((count 5))
		   (define (get-count) count)
		   (define p (srfi.delay (if (<= count 0)
					     count
					   (begin
					     (set! count (- count 1))
					     (srfi.force p)
					     (set! count (+ count 2))
					     count))))
		   (list get-count p)))
	      (get-count (car q))
	      (p (cadr q)))
	 (add-result (get-count))
	 (add-result (srfi.force p))
	 (add-result (get-count))))
    => `(,(void) (5 0 10)))

  #t)


;;;; done

(check-report)

;;; end of file
