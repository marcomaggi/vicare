;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the compiler internals
;;;Date: Mon Jul 28, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (vicare checks)
  (prefix (vicare system $compiler)
	  compiler.))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare compiler internals\n")


;;;; helpers

(define (gensyms->symbols sexp)
  (cond ((pair? sexp)
	 (cons (gensyms->symbols (car sexp))
	       (gensyms->symbols (cdr sexp))))
	((vector? sexp)
	 (vector-map gensyms->symbols sexp))
	((gensym? sexp)
	 (string->symbol (symbol->string sexp)))
	(else sexp)))

;;; --------------------------------------------------------------------
;;; expansion helpers

(define-constant THE-ENVIRONMENT
  (environment '(rnrs)
	       ;;We  import this  library for  $SYMBOL-STRING, which  is a  primitive
	       ;;operation bot not a primitive function.
	       '(vicare system $symbols)))

(define (%expand standard-language-form)
  (receive (code libs)
      (expand-form-to-core-language standard-language-form THE-ENVIRONMENT)
    code))

(define (%expand-library standard-language-form)
  (cdr (assq 'invoke-code (expand-library->sexp standard-language-form))))


(parametrise ((check-test-name		'expansion))

  (define-syntax-rule (doit* ?standard-language-form ?expected-result)
    (check
	(%expand (quasiquote ?standard-language-form))
      => (quasiquote ?expected-result)))

  (define-syntax-rule (libdoit* ?standard-language-form ?expected-result)
    (doit ,(%expand-library (quasiquote ?standard-language-form)) ?expected-result))

;;; --------------------------------------------------------------------

  (doit* (fx+ 1 2)
	 (annotated-call (fx+ 1 2)
			 (primitive fx+) '1 '2))

  (doit* ($symbol-string 'ciao)
	 (annotated-call ($symbol-string 'ciao)
			 (primitive $symbol-string) 'ciao))

  #t)


(parametrise ((check-test-name		'recordisation))

  (define (%recordise core-language-form)
    (let* ((D (compiler.$recordize core-language-form))
	   (S (compiler.$unparse-recordized-code/sexp D)))
      S))

  (define-syntax-rule (doit ?core-language-form ?expected-result)
    (check
	(%recordise (quasiquote ?core-language-form))
      => (quasiquote ?expected-result)))

  (define-syntax-rule (doit* ?standard-language-form ?expected-result)
    (doit ,(%expand (quasiquote ?standard-language-form)) ?expected-result))

  (define-syntax-rule (libdoit* ?standard-language-form ?expected-result)
    (doit ,(%expand-library (quasiquote ?standard-language-form)) ?expected-result))

;;; --------------------------------------------------------------------

  (doit* (fx+ 1 2)
	 (funcall (primref fx+) (constant 1) (constant 2)))
  (doit ((primitive fx+) '1 '2)
	(funcall (primref fx+) (constant 1) (constant 2)))
  (doit (annotated-call (fx+ 1 2)
			(primitive fx+) '1 '2)
	(funcall (primref fx+) (constant 1) (constant 2)))

  (doit* ($symbol-string 'ciao)
	 (funcall (primref $symbol-string) (constant ciao)))
  (doit ((primitive $symbol-string) 'ciao)
	 (funcall (primref $symbol-string) (constant ciao)))

  #t)


(parametrise ((check-test-name			'optimize-letrec-basic)
	      (compiler.$current-letrec-pass	'basic))

  (define-syntax-rule (doit ?core-language-form ?expected-result)
    (check
	(%optimize-letrec (quasiquote ?core-language-form))
      => (quasiquote ?expected-result)))

  (define-syntax-rule (doit* ?standard-language-form ?expected-result)
    (doit ,(%expand-and-optimize-letrec (quasiquote ?standard-language-form))
	  ?expected-result))

  (define-syntax-rule (libdoit* ?standard-language-form ?expected-result)
    (doit ,(%expand-library-and-optimize-letrec (quasiquote ?standard-language-form))
	  ?expected-result))

;;; --------------------------------------------------------------------

  (doit (letrec ((a (quote 1)))
	  a)
	(bind ((a_0 (constant #!void)))
	  (bind ((a_1 (constant 1)))
	    (seq
	      (set! a_0 a_1)
	      a_0))))

  (doit (letrec ((a (lambda () (quote 1))))
	  a)
	(bind ((a_0 (constant #!void)))
	  (bind ((a_1 (lambda () (constant 1))))
	    (seq
	      (set! a_0 a_1)
	      a_0))))

  (doit* (letrec ((a (lambda () (quote 1)))
		  (b (lambda () (quote 2))))
	   (list (a) (b)))
	 (bind ((a_0 (constant #!void))
		(b_0 (constant #!void)))
	   (bind ((a_1 (lambda () (constant 1)))
		  (b_1 (lambda () (constant 2))))
	     (seq
	       (set! a_0 a_1)
	       (set! b_0 b_1)
	       (funcall (primref list) (funcall a_0) (funcall b_0))))))

;;; --------------------------------------------------------------------

  (doit (letrec* ((a (quote 1)))
	  a)
	(bind ((a_0 (constant #!void)))
	  (seq
	    (set! a_0 (constant 1))
	    a_0)))

  (doit (letrec* ((a (lambda () (quote 1))))
	  a)
	(bind ((a_0 (constant #!void)))
	  (seq
	    (set! a_0 (lambda () (constant 1)))
	    a_0)))

  (doit* (letrec* ((a (lambda () (quote 1)))
		   (b (lambda () (quote 2))))
	   (list (a) (b)))
	 (bind ((a_0 (constant #!void))
		(b_0 (constant #!void)))
	   (seq
	     (set! a_0 (lambda () (constant 1)))
	     (set! b_0 (lambda () (constant 2)))
	     (funcall (primref list) (funcall a_0) (funcall b_0)))))

;;; --------------------------------------------------------------------
;;; special

  (libdoit* (library (optimize-letrec-basic-demo)
	      (export a b c)
	      (import (rnrs))
	      (define (a) 1)
	      (define (b) (a) 2)
	      (define (c) (b) 3))
	    (bind ((a_0 (constant #!void))
		   (b_0 (constant #!void))
		   (c_0 (constant #!void)))
	      (seq
		(set! a_0 (lambda () (constant 1)))
		(set! b_0 (lambda () (seq (funcall a_0) (constant 2))))
		(set! c_0 (lambda () (seq (funcall b_0) (constant 3))))
		(funcall (primref void)))))

  #t)


(parametrise ((check-test-name			'optimize-letrec-waddell)
	      (compiler.$current-letrec-pass	'waddell))

  (define (%optimize-letrec core-language-form)
    (let* ((D (compiler.$recordize core-language-form))
	   (D (compiler.$optimize-direct-calls D))
	   (D (compiler.$optimize-letrec D))
	   (S (compiler.$unparse-recordized-code/sexp D)))
      S))

  (define-syntax-rule (doit ?core-language-form ?expected-result)
    (check
	(%optimize-letrec (quasiquote ?core-language-form))
      => (quasiquote ?expected-result)))

  (define-syntax-rule (doit* ?standard-language-form ?expected-result)
    (doit ,(%expand (%optimize-letrec (quasiquote ?standard-language-form)))
	  ?expected-result))

  (define-syntax-rule (libdoit* ?standard-language-form ?expected-result)
    (doit ,(%expand-library (%optimize-letrec (quasiquote ?standard-language-form)))
	  ?expected-result))

;;; --------------------------------------------------------------------
;;; single bindings, simple RHS expressions

  ;;Single, unused binding.
  ;;
  (doit (letrec ((a '1))
	  '#f)
	(bind ((a_0 (constant 1))) ;simple
	  (constant #f)))
  (doit (letrec* ((a '1))
	  '#f)
	(bind ((a_0 (constant 1))) ;simple
	  (constant #f)))

  ;;Single, referenced, unassigned binding.
  ;;
  (doit (letrec ((a '1))
	  a)
	(bind ((a_0 (constant 1))) ;simple
	  a_0))
  (doit (letrec* ((a '1))
	  a)
	(bind ((a_0 (constant 1))) ;simple
	  a_0))

  ;;Single, unreferenced, assigned binding.
  ;;
  (doit (letrec ((a '1))
	  (begin
	    (set! a (quote 2))
	    '#f))
	(bind ((a_0 (constant #!void)))	;complex
	  (bind ((a_1 (constant 1)))	;tmp
	    (seq
	      (set! a_0 a_1)
	      (set! a_0 (constant 2))
	      (constant #f)))))
  (doit (letrec* ((a '1))
	  (begin
	    (set! a (quote 2))
	    '#f))
	(bind ((a_0 (constant #!void))) ;complex
	  (seq
	    (set! a_0 (constant 1))
	    (set! a_0 (constant 2))
	    (constant #f))))

  ;;Single, referenced, assigned binding.
  ;;
  (doit (letrec ((a '1))
	  (begin
	    (set! a (quote 2))
	    a))
	(bind ((a_0 (constant #!void))) ;complex
	  (bind ((a_1 (constant 1)))    ;tmp
	    (seq
	      (set! a_0 a_1)
	      (set! a_0 (constant 2))
	      a_0))))
  (doit (letrec* ((a '1))
	  (begin
	    (set! a (quote 2))
	    a))
	(bind ((a_0 (constant #!void))) ;complex
	  (seq
	    (set! a_0 (constant 1))
	    (set! a_0 (constant 2))
	    a_0)))

;;; --------------------------------------------------------------------
;;; single bindings, lambda RHS expressions

  ;;Single, unreferenced, unassigned, lambda binding is classified as "lambda", so it
  ;;ends in a FIX form.
  ;;
  (doit (letrec ((a (lambda () '1)))
	  '#f)
	(fix ((a_0 (lambda () (constant 1))))
	  (constant #f)))
  (doit (letrec* ((a (lambda () '1)))
	  '#f)
	(fix ((a_0 (lambda () (constant 1))))
	  (constant #f)))

  ;;Single, referenced, unassigned,  lambda binding is classified as  "lambda", so it
  ;;ends in a FIX form.
  ;;
  (doit (letrec ((a (lambda () '1)))
	  a)
	(fix ((a_0 (lambda () (constant 1))))
	  a_0))
  (doit (letrec* ((a (lambda () '1)))
	  a)
	(fix ((a_0 (lambda () (constant 1))))
	  a_0))

  ;;Single, unreferenced, assigned, lambda binding is classified as "complex".
  ;;
  (doit (letrec ((a (lambda () '1)))
	  (begin
	    (set! a '1)
	    '#f))
	(bind ((a_0 (constant #!void)))
	  (bind ((a_1 (lambda () (constant 1))))
	    (seq
	      (set! a_0 a_1)
	      (set! a_0 (constant 1))
	      (constant #f)))))
  (doit (letrec* ((a (lambda () '1)))
	  (begin
	    (set! a '1)
	    '#f))
	(bind ((a_0 (constant #!void)))
	  (seq
	    (set! a_0 (lambda () (constant 1)))
	    (set! a_0 (constant 1))
	    (constant #f))))

  ;;Single, referenced, assigned, lambda binding is classified as "complex".
  ;;
  (doit (letrec ((a (lambda () '1)))
	  (begin
	    (set! a '1)
	    a))
	(bind ((a_0 (constant #!void)))
	  (bind ((a_1 (lambda () (constant 1))))
	    (seq
	      (set! a_0 a_1)
	      (set! a_0 (constant 1))
	      a_0))))
  (doit (letrec* ((a (lambda () '1)))
	  (begin
	    (set! a '1)
	    a))
	(bind ((a_0 (constant #!void)))
	  (seq
	    (set! a_0 (lambda () (constant 1)))
	    (set! a_0 (constant 1))
	    a_0)))

;;; --------------------------------------------------------------------

  ;;Multiple unassigned  lambda bindings are classified  as "lambda", so they  end in
  ;;the same FIX form.
  ;;
  (doit (letrec ((a (lambda () '1))
		 (b (lambda () '2)))
	  '#f)
	(fix ((a_0 (lambda () (constant 1)))
	      (b_0 (lambda () (constant 2))))
	  (constant #f)))
  (doit (letrec* ((a (lambda () '1))
		  (b (lambda () '2)))
	  '#f)
	(fix ((a_0 (lambda () (constant 1)))
	      (b_0 (lambda () (constant 2))))
	  (constant #f)))

  ;;Multiple unassigned  lambda bindings are classified  as "lambda", so they  end in
  ;;the same FIX form.
  ;;
  (doit (letrec ((a (lambda () (b)))
		 (b (lambda () (a))))
	  '#f)
	(fix ((a_0 (lambda () (funcall b_0)))
	      (b_0 (lambda () (funcall a_0))))
	  (constant #f)))
  (doit (letrec ((a (lambda () (b)))
		 (b (lambda () (a))))
	  a)
	(fix ((a_0 (lambda () (funcall b_0)))
	      (b_0 (lambda () (funcall a_0))))
	  a_0))
  (doit* (letrec ((a (lambda () (b)))
		  (b (lambda () (a))))
	   (list a b))
	 (fix ((a_0 (lambda () (funcall b_0)))
	       (b_0 (lambda () (funcall a_0))))
	   (funcall (primref list) a_0 b_0)))
  (doit (letrec* ((a (lambda () (b)))
		  (b (lambda () (a))))
	  '#f)
	(fix ((a_0 (lambda () (funcall b_0)))
	      (b_0 (lambda () (funcall a_0))))
	  (constant #f)))
  (doit (letrec* ((a (lambda () (b)))
		  (b (lambda () (a))))
	  a)
	(fix ((a_0 (lambda () (funcall b_0)))
	      (b_0 (lambda () (funcall a_0))))
	  a_0))
  (doit* (letrec* ((a (lambda () (b)))
		   (b (lambda () (a))))
	   (list a b))
	 (fix ((a_0 (lambda () (funcall b_0)))
	       (b_0 (lambda () (funcall a_0))))
	   (funcall (primref list) a_0 b_0)))

  ;;Here the  binding B is assigned,  even though in the  body of a LAMBDA;  for this

  ;;reason it cannot be classified as "lambda", rather it is classified as "complex".
  ;;
  (doit (letrec ((a (lambda () (b)))
		 (b (lambda () (set! a '123))))
	  '#f)
	(bind ((a_0 (constant #!void)))			;complex
	  (fix ((b_0 (lambda () (set! a_0 (constant 123)))))	;lambda
	    (bind ((a_1 (lambda () (funcall b_0))))		;tmp
	      (seq
		(set! a_0 a_1)
		(constant #f))))))

  (doit* (let ((a '1))
	   (let ((a a))
	     a))
	 (bind ((a_0 (constant 1)))
	   (bind ((a_1 a_0))
	     a_1)))

  (doit* (let ((a '1))
	   (let ((a '2))
	     (let ((a '3))
	       a)))
	 (bind ((a_0 (constant 1)))
	   (bind ((a_1 (constant 2)))
	     (bind ((a_2 (constant 3)))
	       a_2))))

  (doit* (letrec ((a '1)
		  (b '2))
	   (list a b))
	 (bind ((a_0 (constant 1))
		(b_0 (constant 2)))
	   (funcall (primref list) a_0 b_0)))

  ;;The binding C is not "simple" because it is assigned in the RHS of D.
  ;;
  (doit* (letrec* ((a (lambda (x)
			(when x
			  (a '#f))))
		   (b '123)
		   (c '456)
		   (d (begin
			(set! c '789)
			'9)))
	   a)
	 (bind ((b_0 (constant 123)))
	   (bind ((c_0 (constant #!void))
		  (d_0 (constant #!void)))
	     (fix ((a_0 (lambda (x_0)
			  (conditional x_0
			      (funcall a_0 (constant #f))
			    (funcall (primref void))))))
	       (seq
		 (set! c_0 (constant 456))
		 (set! d_0 (seq
			     (set! c_0 (constant 789))
			     (constant 9)))
		 a_0)))))

  ;;The binding A is referenced in a RHS, so it cannot be "simple".
  ;;
  (doit* (letrec* ((a '1)
		   (b a))
	   b)
	 (bind ((a_0 (constant #!void))
		(b_0 (constant #!void)))
	   (seq
	     (set! a_0 (constant 1))
	     (set! b_0 a_0)
	     b_0)))

  ;;Here  the binding  D is  initialised  by a  LAMBDA  expression, but  later it  is
  ;;assigned; for this reason  D cannot end into a FIX form,  rather it is classified
  ;;as "complex".
  ;;
  (doit* (letrec ((a '123)
		  (d (lambda () '123)))
	   (set! d '123)
	   a)
	 (bind ((a_0 (constant 123)))
	   (bind ((d_0 (constant #!void)))
	     (bind ((d_1 (lambda () (constant 123))))
	       (seq
		 (set! d_0 d_1)
		 (set! d_0 (constant 123))
		 a_0)))))
  (doit* (letrec* ((a '123)
		   (d (lambda () '123)))
	   (set! d '123)
	   a)
	 (bind ((a_0 (constant 123)))
	   (bind ((d_0 (constant #!void)))
	     (seq
	       (set! d_0 (lambda () (constant 123)))
	       (set! d_0 (constant 123))
	       a_0))))

;;; --------------------------------------------------------------------
;;; special cases

  ;;References and assignments of LETREC and LETREC* bindings in LAMBDA bodies do not
  ;;make the binding "complex"

  ;;This example shows the difference between waddell and scc.
  (doit (letrec ((a '1)
                 (b (lambda () a)))
          '#f)
	(bind ((a_0 (constant #!void)))
	  (fix ((b_0 (lambda () a_0)))
	    (bind ((a_1 (constant 1)))
	      (seq
		(set! a_0 a_1)
		(constant #f))))))

  ;;This example shows the difference between waddell and scc.
  (doit (letrec ((a '1)
                 (b (lambda () (set! a '2))))
          '#f)
	(bind ((a_0 (constant #!void)))
	  (fix ((b_0 (lambda () (set! a_0 (constant 2)))))
	    (bind ((a_1 (constant 1)))
	      (seq
		(set! a_0 a_1)
		(constant #f))))))

  (doit* (letrec ((a '1))
	   (let ((b (lambda () a)))
	     '#f))
	 (bind ((a_0 (constant 1)))
	   (bind ((b_0 (lambda () a_0)))
	     (constant #f))))

  ;;This example shows the difference between  WDC and SCC.  WDC does not distinguish
  ;;between an assignment in the RHS and an assignment in the body.
  (doit* (letrec ((a '1))
	   (let ((b (lambda () (set! a '2))))
	     '#f))
	 (bind ((a_0 (constant #!void)))
	   (bind ((a_1 (constant 1)))
	     (seq
	       (set! a_0 a_1)
	       (bind ((b_0 (lambda () (set! a_0 (constant 2)))))
		 (constant #f))))))

  (doit (letrec* ((a '1)
                  (b (lambda () a))
                  (c (b)))
          '#f)
        (bind ((a_0 (constant #!void))	;complex
	       (c_0 (constant #!void)))	;complex
	  (fix ((b_0 (lambda () a_0)))
	    (seq
	      (set! a_0 (constant 1))
	      (set! c_0 (funcall b_0))
	      (constant #f)))))

;;; --------------------------------------------------------------------
;;; special cases

  (doit* (letrec* ((a '1)
		   (b (write a)))
	   a)
	 (bind ((a_0 (constant #!void))
		(b_0 (constant #!void)))
	   (seq
	     (set! a_0 (constant 1))
	     (set! b_0 (funcall (primref write) a_0))
	     a_0)))

  (libdoit* (library (optimize-letrec-waddell-demo)
	      (export a b c)
	      (import (rnrs))
	      (define (a) 1)
	      (define (b) (a) 2)
	      (define (c) (b) 3))
	    (fix ((a_0 (lambda () (constant 1)))
		  (b_0 (lambda () (seq (funcall a_0) (constant 2))))
		  (c_0 (lambda () (seq (funcall b_0) (constant 3)))))
	      (funcall (primref void))))

  ;; (doit (letrec ((a '1)
  ;; 		 (b (letrec ((d (lambda () '4)))
  ;; 		      (d)))
  ;; 		 (c '2))
  ;; 	  b)
  ;; 	(bind ((a (constant 1))
  ;; 	       (c (constant 2)))
  ;; 	  (fix ((d (lambda () (constant 4))))
  ;; 	    (bind ((b (funcall d)))
  ;; 	      b))))

  #t)


(parametrise ((check-test-name			'optimize-letrec-scc)
	      (compiler.$current-letrec-pass	'scc))

  (define-syntax-rule (doit ?core-language-form ?expected-result)
    (check
	(%optimize-letrec (quasiquote ?core-language-form))
      => (quasiquote ?expected-result)))

  (define-syntax-rule (doit* ?standard-language-form ?expected-result)
    (doit ,(%expand-and-optimize-letrec (quasiquote ?standard-language-form))
	  ?expected-result))

  (define-syntax-rule (libdoit* ?standard-language-form ?expected-result)
    (doit ,(%expand-library-and-optimize-letrec (quasiquote ?standard-language-form))
	  ?expected-result))

;;; --------------------------------------------------------------------

  (doit (letrec ((a (quote 1)))
	  a)
	(bind ((a_0 (constant 1)))
	  a_0))

  (doit (letrec ((a (lambda () (quote 1))))
	  a)
	(fix ((a_0 (lambda () (constant 1))))
	  a_0))

  (doit (letrec ((a (lambda () a)))
	  a)
	(fix ((a_0 (lambda () a_0)))
	  a_0))

  (doit (letrec ((a (lambda () (set! a '1))))
	  a)
	(bind ((a_0 (constant #!void)))
	  (seq
	    (set! a_0 (lambda () (set! a_0 (constant 1))))
	    a_0)))

  (doit* (letrec* ((a '123)
		   (b '2)
		   (c b)
		   (d (lambda () '123)))
	   (set! d '123)
	   b)
	 (bind ((a_0 (constant 123)))
	   (bind ((b_0 (constant 2)))
	     (bind ((c_0 b_0))
	       (bind ((d_0 (lambda () (constant 123))))
		 (seq
		   (set! d_0 (constant 123))
		   b_0))))))

  ;;The binding A  is referenced in a  RHS.  This tests shows the  superiority of the
  ;;SCC transformation with respect to the Waddell one.
  ;;
  (doit* (letrec* ((a '1)
		   (b a))
	   b)
	 (bind ((a_0 (constant 1)))
	   (bind ((b_0 a_0))
	     b_0)))

;;; --------------------------------------------------------------------
;;; special cases

  ;;References and assignments of LETREC and LETREC* bindings in LAMBDA bodies do not
  ;;make the binding "complex"

  (doit (letrec ((a '1)
                 (b (lambda () a)))
          '#f)
	(bind ((a_0 (constant 1)))
	  (fix ((b_0 (lambda () a_0)))
	    (constant #f))))

  (doit (letrec ((a '1)
                 (b (lambda () (set! a '2))))
          '#f)
	(bind ((a_0 (constant 1)))
	  (fix ((b_0 (lambda () (set! a_0 (constant 2)))))
	    (constant #f))))

  (doit* (letrec ((a '1))
	   (let ((b (lambda () a)))
	     '#f))
	 (bind ((a_0 (constant 1)))
	   (bind ((b_0 (lambda () a_0)))
	     (constant #f))))

  (doit* (letrec ((a '1))
	   (let ((b (lambda () (set! a '2))))
	     '#f))
	 (bind ((a_0 (constant 1)))
	   (bind ((b_0 (lambda () (set! a_0 (constant 2)))))
	     (constant #f))))

  (doit (letrec* ((a '1)
                  (b (lambda () a))
                  (c (b)))
          '#f)
        (bind ((a_0 (constant 1)))
          (bind ((c_0 (constant #!void)))
            (fix ((b_0 (lambda () a_0)))
              (seq
		(set! c_0 (funcall b_0))
		(constant #f))))))

;;; --------------------------------------------------------------------
;;; specials

  (libdoit* (library (optimize-letrec-scc-demo)
	      (export a b c)
	      (import (rnrs))
	      (define (a) 1)
	      (define (b) (a) 2)
	      (define (c) (b) 3))
	    (fix ((a_0 (lambda () (constant 1)))
		  (b_0 (lambda () (seq (funcall a_0) (constant 2))))
		  (c_0 (lambda () (seq (funcall b_0) (constant 3)))))
	      (funcall (primref void))))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'seq 'scheme-indent-function 0)
;; eval: (put 'fix 'scheme-indent-function 1)
;; eval: (put 'bind 'scheme-indent-function 1)
;; eval: (put 'conditional 'scheme-indent-function 2)
;; End:
