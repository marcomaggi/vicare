;;;Lightweight testing (reference implementation)
;;;
;;;Copyright (c) 2009, 2010, 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2005-2006 Sebastian Egner <Sebastian.Egner@philips.com>
;;;Modified by Derick Eddington for R6RS Scheme.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;``Software''), to deal in the Software without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE  IS PROVIDED  ``AS IS'', WITHOUT  WARRANTY OF  ANY KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!r6rs
(library (vicare checks)
  (export
    ;; bindings from the SRFI
    check
    check-report
    check-set-mode!
    check-reset!
    check-passed?

    ;; result handling
    with-result add-result get-result

    ;; more macros
    false-if-exception check-for-true check-for-false

    ;; selecting tests
    check-test-name

    ;; output
    check-quiet-tests?
    check-display
    check-write
    check-newline
    check-pretty-print)
  (import (rename (ikarus)
		  (display	ikarus:display)
		  (write	ikarus:write)
		  (newline	ikarus:newline)
		  (pretty-print	ikarus:pretty-print)))


;;; utilities

(define check-quiet-tests?
  (let ((S (getenv "VICARE_CHECK_QUIET")))
    (and S
	 (or (and (fixnum? S)
		  (not (fxzero? S)))
	     (string=? S "yes")))))

(define (check-display thing)
  (unless check-quiet-tests?
    (ikarus:display thing (current-error-port))))

(define (check-write thing)
  (unless check-quiet-tests?
    (ikarus:write thing (current-error-port))))

(define (check-newline)
  (unless check-quiet-tests?
    (ikarus:newline (current-error-port))))

(define (check-pretty-print thing)
  (unless check-quiet-tests?
    (ikarus:pretty-print thing (current-error-port))))

(define (flush-checks-port)
  (unless check-quiet-tests?
    (flush-output-port (current-error-port))))

(define check-pretty-print/no-trailing-newline
  (case-lambda
   ((datum output-port)
    (unless check-quiet-tests?
      (let* ((os	(call-with-string-output-port
			    (lambda (sop) (check-pretty-print datum sop))))
	     (len	(string-length os))
	     (os	(if (and (positive? len)
				 (char=? #\newline
					 (string-ref os (- len 1))))
			    (substring os 0 (- len 1))
			  os)))
	(check-display os output-port))))
   ((datum)
    (check-pretty-print/no-trailing-newline datum (current-error-port)))))

(define (string-prefix? prefix the-string)
  (or (eq? prefix the-string)
      (let ((prelen (string-length prefix)))
	(and (<= prelen (string-length the-string))
	     (string=? prefix (substring the-string 0 prelen))))))

(define (string-suffix? suffix the-string)
  (or (eq? suffix the-string)
      (let ((strlen (string-length the-string))
	    (suflen (string-length suffix)))
	(and (<= suflen strlen)
	     (string=? suffix (substring the-string suflen strlen))))))



;;; mode handling

(define check:mode
  (make-parameter 'report
    (lambda (v)
      (case v
	((off)           0)
	((summary)       1)
	((report-failed) 10)
	((report)        100)
	(else (error 'check:mode
		"unrecognized mode" v))))))

(define (check-set-mode! mode)
  (check:mode mode))


;;; state handling

(define check:correct 0)
(define check:failed '())

(define (check-reset!)
  (set! check:correct 0)
  (set! check:failed '()))

(define (check:add-correct!)
  (set! check:correct (+ check:correct 1)))

(define (check:add-failed! expression actual-result expected-result)
  (set! check:failed
	(cons (list expression actual-result expected-result)
	      check:failed)))


;;; reporting

(define (check:report-expression expression)
  (check-newline)
  (check-pretty-print expression)
  (check-display " => "))

(define (check:report-actual-result actual-result)
  (check-pretty-print actual-result)
  (check-display " ; "))

(define (check:report-correct cases)
  (check-display "correct")
  (if (not (= cases 1))
      (begin (check-display " (")
	     (check-display cases)
	     (check-display " cases checked)")))
  (check-newline))

(define (check:report-failed expected-result)
  (check-display "*** failed ***")
  (check-newline)
  (check-display " ; expected result: ")
  (check-pretty-print expected-result)
  (check-newline)
  (flush-checks-port))

(define (check-report)
  (when (>= (check:mode) 1)
    (check-newline)
    (check-display "; *** checks *** : ")
    (check-display check:correct)
    (check-display " correct, ")
    (check-display (length check:failed))
    (check-display " failed.")
    (if (or (null? check:failed) (<= (check:mode) 1))
	(begin
	  (check-newline)
	  (check-newline)
	  (check-newline))
      (let* ((w (car (reverse check:failed)))
	     (expression (car w))
	     (actual-result (cadr w))
	     (expected-result (caddr w)))
	(check-display " First failed example:")
	(check-newline)
	(check:report-expression expression)
	(check:report-actual-result actual-result)
	(check:report-failed expected-result)
	(check-newline)
	(check-newline)))
    (flush-output-port (current-error-port)))
  (cond ((null? check:failed)
	 ;;Success for GNU Automake.
	 (exit 0))
	((null? check:failed)
	 ;;Success for GNU Automake.
	 (exit 0))
	((not (null? check:failed))
	 ;;Test failure for GNU Automake.
	 (exit 1))
;;;	((something)
;;;	 ;;Skipped test for GNU Automake.
;;;	 (exit 77))
	(else
	 ;;Hard error for GNU Automake.
	 (exit 99))))

(define (check-passed? expected-total-count)
  (and (= (length check:failed) 0)
       (= check:correct expected-total-count)))


;;; simple checks

(define (check:proc expression thunk equal expected-result)
  (case (check:mode)
    ((0) #f)
    ((1)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
	   (check:add-correct!)
	 (check:add-failed! expression actual-result expected-result))))
    ((10)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
	   (check:add-correct!)
	 (begin
	   (check:report-expression expression)
	   (check:report-actual-result actual-result)
	   (check:report-failed expected-result)
	   (check:add-failed! expression actual-result expected-result)))))
    ((100)
     (check:report-expression expression)
     (let ((actual-result (thunk)))
       (check:report-actual-result actual-result)
       (if (equal actual-result expected-result)
	   (begin (check:report-correct 1)
		  (check:add-correct!))
	 (begin (check:report-failed expected-result)
		(check:add-failed! expression
				   actual-result
				   expected-result)))))
    (else (error 'check:proc
	    "unrecognized check:mode" (check:mode))))
  (if #f #f))

(define-syntax srfi:check
  (syntax-rules (=>)
    ((_ ?expr => ?expected)
     (srfi:check ?expr (=> equal?) ?expected))
    ((_ ?expr (=> ?equal) ?expected)
     (when (>= (check:mode) 1)
       (check:proc (quote ?expr) (lambda () ?expr) ?equal ?expected)))))


;;;; handling results

(define result
  (make-parameter #f))

(define-syntax with-result
  (syntax-rules ()
    ((_ ?form ... ?last-form)
     (parameterize ((result '()))
       ?form ...
       (let ((last-result ?last-form))
		;We  need  to make  sure  that  ?LAST-FORM is  evaluated
		;before (GET-RESULT).
	 (list last-result (get-result)))))))

(define (add-result value)
  (result (cons value (result))))

(define (get-result)
  (reverse (result)))


;;;; more macros

(define-syntax false-if-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else #f))
       ?form0 ?form ...))))

(define-syntax check-for-true
  (syntax-rules ()
    ((_ ?form)
     (check (if ?form #t #f) => #t))
    ((_ (quote ?name) ?form)
     (check (quote ?name) (if ?form #t #f) => #t))))

(define-syntax check-for-false
  (syntax-rules ()
    ((_ ?form)
     (check (if ?form #t #f) => #f))
    ((_ (quote ?name) ?form)
     (check (quote ?name) (if ?form #t #f) => #f))))


;;;; selecting tests

(define check-test-name
  (make-parameter #f
    (lambda (value)
      (unless (or (not value) (string? value) (symbol? value))
	(assertion-violation 'check-test-name
	  "expected #f or string as parameter value" value))
      (if (symbol? value)
	  (symbol->string value)
	value))))

(define selected-test
  (getenv "CHECK_TEST_NAME"))

(define (eval-this-test?)
  (or (not selected-test)
      (zero? (string-length selected-test))
      (let ((name (check-test-name)))
	(if name
	    (or (string-prefix? selected-test name)
		(string-suffix? selected-test name))
	  #f))))

(define-syntax check
  (syntax-rules (=>)
    ((_ ?expr => ?expected-result)
     (check ?expr (=> equal?) ?expected-result))

    ((_ ?expr (=> ?equal) ?expected-result)
     (when (eval-this-test?)
       (srfi:check ?expr (=> ?equal) ?expected-result)))

    ((_ ?name ?expr => ?expected-result)
     (check ?name ?expr (=> equal?) ?expected-result))

    ((_ ?name ?expr (=> ?equal) ?expected-result)
     (parameterize ((check-test-name ?name))
       (when (eval-this-test?)
	 (srfi:check ?expr (=> ?equal) ?expected-result))))))


;;;; done

)

;;; end of file
