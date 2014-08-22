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
  (only (vicare libraries)
	uninstall-library)
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
	       '(vicare system $fx)
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
;;; primitive function calls and primitive operation calls

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

;;; --------------------------------------------------------------------
;;; let bindings

  (doit* (let ((a '1))
	   (let ((a '2))
	     (let ((a '3))
	       a)))
	 (bind ((a_0 (constant 1)))
	   (bind ((a_1 (constant 2)))
	     (bind ((a_2 (constant 3)))
	       a_2))))

  (doit* (let ((a '1))
	   (let ((a a))
	     (let ((a a))
	       a)))
	 (bind ((a_0 (constant 1)))
	   (bind ((a_1 a_0))
	     (bind ((a_2 a_1))
	       a_2))))

  #t)


(parametrise ((check-test-name	'basic-optimize-letrec))

;;;We test the "basic"  here because it is very simple and does  not need many tests.
;;;We do the "waddell" and "scc" tests below.

  (define (%optimize-letrec core-language-form)
    (let* ((D (compiler.$recordize core-language-form))
	   (D (compiler.$optimize-direct-calls D))
	   (D (compiler.$optimize-letrec D))
	   (S (compiler.$unparse-recordized-code/sexp D)))
      S))

  (define-auxiliary-syntaxes basic waddell scc)

  (define-syntax doit
    (syntax-rules (basic waddell scc)
      ((_ ?core-language-form ?expected-result/basic)
       (check
	   (parametrise ((compiler.$current-letrec-pass 'basic))
	     (%optimize-letrec (quasiquote ?core-language-form)))
	 => (quasiquote ?expected-result/basic)))
      ))

  (define-syntax doit*
    (syntax-rules (basic waddell scc)
      ((_ ?standard-language-form ?expected-result/basic)
       (doit ,(%expand (quasiquote ?standard-language-form)) ?expected-result/basic))
      ))

  (define-syntax libdoit*
    (syntax-rules (basic waddell scc)
      ((_ ?standard-language-form ?expected-result/basic)
       (doit ,(%expand-library (quasiquote ?standard-language-form)) ?expected-result/basic))
      ))

;;; --------------------------------------------------------------------

  (doit (letrec ((a '1))
	  '#f)
	(bind ((a_0 (constant #!void)))
	  (bind ((a_1 (constant 1)))
	    (seq
	      (assign a_0 a_1)
	      (constant #f)))))

  (doit (letrec* ((a '1))
	  '#f)
	(bind ((a_0 (constant #!void)))
	  (seq
	    (assign a_0 (constant 1))
	    (constant #f))))

  (doit (letrec ((a (lambda () (quote 1))))
	  a)
	(bind ((a_0 (constant #!void)))
	  (bind ((a_1 (lambda () (constant 1))))
	    (seq
	      (assign a_0 a_1)
	      a_0))))

  (doit* (letrec ((a (lambda () (quote 1)))
		  (b (lambda () (quote 2))))
	   (list (a) (b)))
	 (bind ((a_0 (constant #!void))
		(b_0 (constant #!void)))
	   (bind ((a_1 (lambda () (constant 1)))
		  (b_1 (lambda () (constant 2))))
	     (seq
	       (assign a_0 a_1)
	       (assign b_0 b_1)
	       (funcall (primref list) (funcall a_0) (funcall b_0))))))

;;; --------------------------------------------------------------------

  (doit (letrec* ((a (quote 1)))
	  a)
	(bind ((a_0 (constant #!void)))
	  (seq
	    (assign a_0 (constant 1))
	    a_0)))

  (doit (letrec* ((a (lambda () (quote 1))))
	  a)
	(bind ((a_0 (constant #!void)))
	  (seq
	    (assign a_0 (lambda () (constant 1)))
	    a_0)))

  (doit* (letrec* ((a (lambda () (quote 1)))
		   (b (lambda () (quote 2))))
	   (list (a) (b)))
	 (bind ((a_0 (constant #!void))
		(b_0 (constant #!void)))
	   (seq
	     (assign a_0 (lambda () (constant 1)))
	     (assign b_0 (lambda () (constant 2)))
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
		(assign a_0 (lambda () (constant 1)))
		(assign b_0 (lambda () (seq (funcall a_0) (constant 2))))
		(assign c_0 (lambda () (seq (funcall b_0) (constant 3))))
		(funcall (primref void)))))

  #t)


(parametrise ((check-test-name	'optimize-letrec))

;;;Test the "waddell" and  "scc" transformations here, side by side,  to make it easy
;;;to  compare  the results.   Every  test  that is  interesting  for  "scc" is  also
;;;interesting for "waddell".

  (define (%optimize-letrec core-language-form)
    (let* ((D (compiler.$recordize core-language-form))
	   (D (compiler.$optimize-direct-calls D))
	   (D (compiler.$optimize-letrec D))
	   (S (compiler.$unparse-recordized-code/sexp D)))
      S))

  (define-auxiliary-syntaxes waddell scc)

  (define-syntax doit
    (syntax-rules (basic waddell scc)
      ((_ ?core-language-form
	  (waddell	?expected-result/waddell)
	  (scc		?expected-result/scc))
       (begin
	 (check
	     (parametrise ((compiler.$current-letrec-pass 'waddell))
	       (%optimize-letrec (quasiquote ?core-language-form)))
	   => (quasiquote ?expected-result/waddell))
	 (check
	     (parametrise ((compiler.$current-letrec-pass 'scc))
	       (%optimize-letrec (quasiquote ?core-language-form)))
	   => (quasiquote ?expected-result/scc))
	 ))
      ))

  (define-syntax doit*
    (syntax-rules (basic waddell scc)
      ((_ ?standard-language-form
	  (waddell	?expected-result/waddell)
	  (scc		?expected-result/scc))
       ;;We want the ?STANDARD-LANGUAGE-FORM to appear  in the output of CHECK when a
       ;;test fails.
       (doit ,(%expand (quasiquote ?standard-language-form))
	     (waddell		?expected-result/waddell)
	     (scc		?expected-result/scc)))
      ))

;;; --------------------------------------------------------------------
;;; single bindings, simple RHS expressions

  ;;Single, unused binding.
  ;;
  (doit (letrec ((a '1))
	  '#f)
	(waddell
	 (bind ((a_0 (constant 1))) ;simple
	   (constant #f)))
	(scc
	 (bind ((a_0 (constant 1)))
	   (constant #f))))

  (doit (letrec* ((a '1))
	  '#f)
	(waddell
	 (bind ((a_0 (constant 1))) ;simple
	   (constant #f)))
	(scc
	 (bind ((a_0 (constant 1)))
	   (constant #f))))

  ;;Single, referenced, unassigned binding.
  ;;
  (doit (letrec ((a '1))
	  a)
	(waddell
	 (bind ((a_0 (constant 1))) ;simple
	   a_0))
	(scc
	 (bind ((a_0 (constant 1)))
	   a_0)))

  (doit (letrec* ((a '1))
	  a)
	(waddell
	 (bind ((a_0 (constant 1))) ;simple
	   a_0))
	(scc
	 (bind ((a_0 (constant 1)))
	   a_0)))

  (doit* (letrec ((a '1))
	   (let ((a a))
	     a))
	 (waddell
	  (bind ((a_0 (constant 1)))
	    (bind ((a_1 a_0))
	      a_1)))
	 (scc
	  (bind ((a_0 (constant 1)))
	    (bind ((a_1 a_0))
	      a_1))))

  (doit* (letrec* ((a '1))
	   (let ((a a))
	     a))
	 (waddell
	  (bind ((a_0 (constant 1)))
	    (bind ((a_1 a_0))
	      a_1)))
	 (scc
	  (bind ((a_0 (constant 1)))
	    (bind ((a_1 a_0))
	      a_1))))

  ;;Single, unreferenced, assigned binding.
  ;;
  (doit (letrec ((a '1))
	  (begin
	    (set! a (quote 2))
	    '#f))
	(waddell
	 (bind ((a_0 (constant #!void))) ;complex
	   (bind ((a_1 (constant 1)))	 ;tmp
	     (seq
	       (assign a_0 a_1)
	       (assign a_0 (constant 2))
	       (constant #f)))))
	(scc
	 (bind ((a_0 (constant 1)))
	   (seq
	     (assign a_0 (constant 2))
	     (constant #f)))))
  (doit (letrec* ((a '1))
	  (begin
	    (set! a (quote 2))
	    '#f))
	(waddell
	 (bind ((a_0 (constant #!void))) ;complex
	   (seq
	     (assign a_0 (constant 1))
	     (assign a_0 (constant 2))
	     (constant #f))))
	(scc
	 (bind ((a_0 (constant 1)))
	   (seq
	     (assign a_0 (constant 2))
	     (constant #f)))))


  ;;Single, referenced, assigned binding.
  ;;
  (doit (letrec ((a '1))
	  (begin
	    (set! a (quote 2))
	    a))
	(waddell
	 (bind ((a_0 (constant #!void))) ;complex
	   (bind ((a_1 (constant 1)))    ;tmp
	     (seq
	       (assign a_0 a_1)
	       (assign a_0 (constant 2))
	       a_0))))
	(scc
	 (bind ((a_0 (constant 1))) ;complex
	   (seq
	     (assign a_0 (constant 2))
	     a_0))))
  (doit (letrec* ((a '1))
	  (begin
	    (set! a (quote 2))
	    a))
	(waddell
	 (bind ((a_0 (constant #!void))) ;complex
	   (seq
	     (assign a_0 (constant 1))
	     (assign a_0 (constant 2))
	     a_0)))
	(scc
	 (bind ((a_0 (constant 1))) ;complex
	   (seq
	     (assign a_0 (constant 2))
	     a_0))))

;;; --------------------------------------------------------------------
;;; single bindings, lambda RHS expressions


  ;;Single, unreferenced, unassigned,  lambda binding is classified  as "fixable", so
  ;;it ends in a FIX form.
  ;;
  (doit (letrec ((a (lambda () '1)))
	  '#f)
	(waddell
	 (fix ((a_0 (lambda () (constant 1))))
	   (constant #f)))
	(scc
	 (fix ((a_0 (lambda () (constant 1))))
	   (constant #f))))
  (doit (letrec* ((a (lambda () '1)))
	  '#f)
	(waddell
	 (fix ((a_0 (lambda () (constant 1))))
	   (constant #f)))
	(scc
	 (fix ((a_0 (lambda () (constant 1))))
	   (constant #f))))


  ;;Single, referenced, unassigned,  lambda binding is classified as  "fixable", so it
  ;;ends in a FIX form.
  ;;
  (doit (letrec ((a (lambda () '1)))
	  a)
	(waddell
	 (fix ((a_0 (lambda () (constant 1))))
	   a_0))
	(scc
	 (fix ((a_0 (lambda () (constant 1))))
	   a_0)))
  (doit (letrec* ((a (lambda () '1)))
	  a)
	(waddell
	 (fix ((a_0 (lambda () (constant 1))))
	   a_0))
	(scc
	 (fix ((a_0 (lambda () (constant 1))))
	   a_0)))

  ;;Single, unreferenced, assigned, lambda binding is classified as "complex".
  ;;
  (doit (letrec ((a (lambda () '1)))
	  (begin
	    (set! a '1)
	    '#f))
	(waddell
	 (bind ((a_0 (constant #!void)))
	   (bind ((a_1 (lambda () (constant 1))))
	     (seq
	       (assign a_0 a_1)
	       (assign a_0 (constant 1))
	       (constant #f)))))
	(scc
	 (bind ((a_0 (lambda () (constant 1))))
	   (seq
	     (assign a_0 (constant 1))
	     (constant #f)))))
  (doit (letrec* ((a (lambda () '1)))
	  (begin
	    (set! a '1)
	    '#f))
	(waddell
	 (bind ((a_0 (constant #!void)))
	   (seq
	     (assign a_0 (lambda () (constant 1)))
	     (assign a_0 (constant 1))
	     (constant #f))))
	(scc
	 (bind ((a_0 (lambda () (constant 1))))
	   (seq
	     (assign a_0 (constant 1))
	     (constant #f)))))

  ;;Single, referenced, assigned, lambda binding is classified as "complex".
  ;;
  (doit (letrec ((a (lambda () '1)))
	  (begin
	    (set! a '1)
	    a))
	(waddell
	 (bind ((a_0 (constant #!void)))
	   (bind ((a_1 (lambda () (constant 1))))
	     (seq
	       (assign a_0 a_1)
	       (assign a_0 (constant 1))
	       a_0))))
	(scc
	 (bind ((a_0 (lambda () (constant 1))))
	   (seq
	     (assign a_0 (constant 1))
	     a_0))))
  (doit (letrec* ((a (lambda () '1)))
	  (begin
	    (set! a '1)
	    a))
	(waddell
	 (bind ((a_0 (constant #!void)))
	   (seq
	     (assign a_0 (lambda () (constant 1)))
	     (assign a_0 (constant 1))
	     a_0)))
	(scc
	 (bind ((a_0 (lambda () (constant 1))))
	   (seq
	     (assign a_0 (constant 1))
	     a_0))))

;;; --------------------------------------------------------------------
;;; multiple simple RHS expressions

  ;;Multiple referenced, unassigned bindings.
  ;;
  (doit* (letrec ((a '1)
		  (b '2))
	   (list a b))
	 (waddell
	  (bind ((a_0 (constant 1))
		 (b_0 (constant 2)))
	    (funcall (primref list) a_0 b_0)))
	 (scc
	  (bind ((a_0 (constant 1)))
	    (bind ((b_0 (constant 2)))
	      (funcall (primref list) a_0 b_0)))))
  (doit* (letrec* ((a '1)
		   (b '2))
	   (list a b))
	 (waddell
	  (bind ((a_0 (constant 1))
		 (b_0 (constant 2)))
	    (funcall (primref list) a_0 b_0)))
	 (scc
	  (bind ((a_0 (constant 1)))
	    (bind ((b_0 (constant 2)))
	      (funcall (primref list) a_0 b_0)))))


;;; --------------------------------------------------------------------
;;; multiple lambda RHS expressions

  ;;Multiple unreferenced, unassigned lambda bindings are classified as "fixable", so
  ;;they end in the same FIX form.
  ;;
  (doit (letrec ((a (lambda () '1))
		 (b (lambda () '2)))
	  '#f)
	(waddell
	 (fix ((a_0 (lambda () (constant 1)))
	       (b_0 (lambda () (constant 2))))
	   (constant #f)))
	(scc
	 (fix ((a_0 (lambda () (constant 1)))
	       (b_0 (lambda () (constant 2))))
	   (constant #f))))
  (doit (letrec* ((a (lambda () '1))
		  (b (lambda () '2)))
	  '#f)
	(waddell
	 (fix ((a_0 (lambda () (constant 1)))
	       (b_0 (lambda () (constant 2))))
	   (constant #f)))
	(scc
	 (fix ((a_0 (lambda () (constant 1)))
	       (b_0 (lambda () (constant 2))))
	   (constant #f))))

  ;;Multiple referenced, unassigned  lambda bindings are classified  as "fixable", so
  ;;they end in the same FIX form.
  ;;
  (doit (letrec ((a (lambda () (b)))
		 (b (lambda () (a))))
	  '#f)
	(waddell
	 (fix ((a_0 (lambda () (funcall b_0)))
	       (b_0 (lambda () (funcall a_0))))
	   (constant #f)))
	(scc
	 (fix ((b_0 (lambda () (funcall a_0)))
	       (a_0 (lambda () (funcall b_0))))
	   (constant #f))))
  (doit (letrec ((a (lambda () (b)))
		 (b (lambda () (a))))
	  a)
	(waddell
	 (fix ((a_0 (lambda () (funcall b_0)))
	       (b_0 (lambda () (funcall a_0))))
	   a_0))
	(scc
	 (fix ((b_0 (lambda () (funcall a_0)))
	       (a_0 (lambda () (funcall b_0))))
	   a_0)))
  (doit* (letrec ((a (lambda () (b)))
		  (b (lambda () (a))))
	   (list a b))
	 (waddell
	  (fix ((a_0 (lambda () (funcall b_0)))
		(b_0 (lambda () (funcall a_0))))
	    (funcall (primref list) a_0 b_0)))
	 (scc
	  (fix ((b_0 (lambda () (funcall a_0)))
		(a_0 (lambda () (funcall b_0))))
	    (funcall (primref list) a_0 b_0))))
  (doit (letrec* ((a (lambda () (b)))
		  (b (lambda () (a))))
	  '#f)
	(waddell
	 (fix ((a_0 (lambda () (funcall b_0)))
	       (b_0 (lambda () (funcall a_0))))
	   (constant #f)))
	(scc
	 (fix ((b_0 (lambda () (funcall a_0)))
	       (a_0 (lambda () (funcall b_0))))
	   (constant #f))))
  (doit (letrec* ((a (lambda () (b)))
		  (b (lambda () (a))))
	  a)
	(waddell
	 (fix ((a_0 (lambda () (funcall b_0)))
	       (b_0 (lambda () (funcall a_0))))
	   a_0))
	(scc
	 (fix ((b_0 (lambda () (funcall a_0)))
	       (a_0 (lambda () (funcall b_0))))
	   a_0)))
  (doit* (letrec* ((a (lambda () (b)))
		   (b (lambda () (a))))
	   (list a b))
	 (waddell
	  (fix ((a_0 (lambda () (funcall b_0)))
		(b_0 (lambda () (funcall a_0))))
	    (funcall (primref list) a_0 b_0)))
	 (scc
	  (fix ((b_0 (lambda () (funcall a_0)))
		(a_0 (lambda () (funcall b_0))))
	    (funcall (primref list) a_0 b_0))))

  ;;Here the  binding B is assigned,  even though in the  body of a LAMBDA;  for this
  ;;reason it cannot be classified as "fixable", rather it is classified as "complex".
  ;;
  (doit (letrec ((a (lambda () (b)))
		 (b (lambda () (set! a '123))))
	  '#f)
	(waddell
	 (bind ((a_0 (constant #!void)))		   ;complex
	   (fix ((b_0 (lambda () (assign a_0 (constant 123))))) ;lambda
	     (bind ((a_1 (lambda () (funcall b_0))))		   ;tmp
	       (seq
		 (assign a_0 a_1)
		 (constant #f))))))
	(scc
	 (bind ((a_0 (constant #!void)))		   ;complex
	   (fix ((b_0 (lambda () (assign a_0 (constant 123))))) ;lambda
	     (seq
	       (assign a_0 (lambda () (funcall b_0)))
	       (constant #f))))))


;;; --------------------------------------------------------------------
;;; misc RHS expressions

  ;;With waddell: the binding C is not "simple"  because it is assigned in the RHS of
  ;;D.
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
	 (waddell
	  (bind ((b_0 (constant 123)))
	    (bind ((c_0 (constant #!void))
		   (d_0 (constant #!void)))
	      (fix ((a_0 (lambda (x_0)
			   (conditional x_0
			       (funcall a_0 (constant #f))
			     (funcall (primref void))))))
		(seq
		  (assign c_0 (constant 456))
		  (assign d_0 (seq
				(assign c_0 (constant 789))
				(constant 9)))
		  a_0)))))
	 (scc
	  (fix ((a_0 (lambda (x_0)
		       (conditional x_0
			   (funcall a_0 (constant #f))
			 (funcall (primref void))))))
	    (bind ((b_0 (constant 123)))
	      (bind ((c_0 (constant 456)))
		(bind ((d_0 (seq
			      (assign c_0 (constant 789))
			      (constant 9))))
		  a_0))))))


  ;;The binding  A is  referenced in  a RHS, so  it cannot  be "simple"  according to
  ;;waddell.
  ;;
  (doit* (letrec* ((a '1)
		   (b a))
	   b)
	 (waddell
	  (bind ((a_0 (constant #!void))
		 (b_0 (constant #!void)))
	    (seq
	      (assign a_0 (constant 1))
	      (assign b_0 a_0)
	      b_0)))
	 (scc
	  (bind ((a_0 (constant 1)))
	    (bind ((b_0 a_0))
	      b_0))))

  ;;waddell: here the binding D is initialised  by a LAMBDA RHS expression, but later
  ;;it is  assigned; for  this reason  D cannot  end into  a FIX  form, rather  it is
  ;;classified as "complex".
  ;;
  (doit* (letrec ((a '123)
		  (d (lambda () '123)))
	   (set! d '123)
	   a)
	 (waddell
	  (bind ((a_0 (constant 123)))
	    (bind ((d_0 (constant #!void)))
	      (bind ((d_1 (lambda () (constant 123))))
		(seq
		  (assign d_0 d_1)
		  (assign d_0 (constant 123))
		  a_0)))))
	 (scc
	  (bind ((a_0 (constant 123)))
	    (bind ((d_0 (lambda () (constant 123))))
	      (seq
		(assign d_0 (constant 123))
		a_0)))))
  (doit* (letrec* ((a '123)
		   (d (lambda () '123)))
	   (set! d '123)
	   a)
	 (waddell
	  (bind ((a_0 (constant 123)))
	    (bind ((d_0 (constant #!void)))
	      (seq
		(assign d_0 (lambda () (constant 123)))
		(assign d_0 (constant 123))
		a_0))))
	 (scc
	  (bind ((a_0 (constant 123)))
	    (bind ((d_0 (lambda () (constant 123))))
	      (seq
		(assign d_0 (constant 123))
		a_0)))))

;;; --------------------------------------------------------------------
;;; special cases

  ;;References and assignments of LETREC and LETREC* bindings in LAMBDA bodies do not
  ;;make the binding "complex".

  ;;Binding referenced in lambda RHS.
  (doit (letrec ((a '1)
                 (b (lambda () a)))
          '#f)
	(waddell
	 (bind ((a_0 (constant #!void)))
	   (fix ((b_0 (lambda () a_0)))
	     (bind ((a_1 (constant 1)))
	       (seq
		 (assign a_0 a_1)
		 (constant #f))))))
	(scc
	 (bind ((a_0 (constant 1)))
	   (fix ((b_0 (lambda () a_0)))
	     (constant #f)))))
  (doit (letrec* ((a '1)
		  (b (lambda () a)))
          '#f)
	(waddell
	 (bind ((a_0 (constant #!void)))
	   (fix ((b_0 (lambda () a_0)))
	     (seq
	       (assign a_0 (constant 1))
	       (constant #f)))))
	(scc
	 (bind ((a_0 (constant 1)))
	   (fix ((b_0 (lambda () a_0)))
	     (constant #f)))))

  ;;Binding assigned in lambda RHS.
  (doit (letrec ((a '1)
                 (b (lambda () (set! a '2))))
          '#f)
	(waddell
	 (bind ((a_0 (constant #!void)))
	   (fix ((b_0 (lambda () (assign a_0 (constant 2)))))
	     (bind ((a_1 (constant 1)))
	       (seq
		 (assign a_0 a_1)
		 (constant #f))))))
	(scc
	 (bind ((a_0 (constant 1)))
	   (fix ((b_0 (lambda () (assign a_0 (constant 2)))))
	     (constant #f)))))

  (doit* (letrec ((a '1))
	   (let ((b (lambda () a)))
	     '#f))
	 (waddell
	  (bind ((a_0 (constant 1)))
	    (bind ((b_0 (lambda () a_0)))
	      (constant #f))))
	 (scc
	  (bind ((a_0 (constant 1)))
	    (bind ((b_0 (lambda () a_0)))
	      (constant #f)))))


  ;;waddell does not  distinguish between an assignment in the  RHS and an assignment
  ;;in the body.
  (doit* (letrec ((a '1))
	   (let ((b (lambda () (set! a '2))))
	     '#f))
	 (waddell
	  (bind ((a_0 (constant #!void)))
	    (bind ((a_1 (constant 1)))
	      (seq
		(assign a_0 a_1)
		(bind ((b_0 (lambda () (assign a_0 (constant 2)))))
		  (constant #f))))))
	 (scc
	  (bind ((a_0 (constant 1)))
	    (bind ((b_0 (lambda () (assign a_0 (constant 2)))))
	      (constant #f)))))

  (doit (letrec* ((a '1)
                  (b (lambda () a))
                  (c (b)))
          '#f)
        (waddell
	 (bind ((a_0 (constant #!void))	 ;complex
		(c_0 (constant #!void))) ;complex
	   (fix ((b_0 (lambda () a_0)))
	     (seq
	       (assign a_0 (constant 1))
	       (assign c_0 (funcall b_0))
	       (constant #f)))))
        (scc
	 (bind ((a_0 (constant 1)))
	   (fix ((b_0 (lambda () a_0)))
	     (bind ((c_0 (funcall b_0)))
	       (constant #f))))))

;;; --------------------------------------------------------------------
;;; special cases

  ;;RHS with side effects.
  (doit* (letrec* ((a '1)
		   (b (write a)))
	   a)
	 (waddell
	  (bind ((a_0 (constant #!void))
		 (b_0 (constant #!void)))
	    (seq
	      (assign a_0 (constant 1))
	      (assign b_0 (funcall (primref write) a_0))
	      a_0)))
	 (scc
	  (bind ((a_0 (constant 1)))
	    (bind ((b_0 (funcall (primref write) a_0)))
	      a_0))))

  ;;Function  call in  RHS expression  that does  not make  the enclosing  expression
  ;;"complex".
  (doit* (letrec* ((a '1)
		   (b ($fx+ '1 '2)))
	   a)
	 (waddell
	  (bind ((a_0 (constant 1))
		 (b_0 (funcall (primref $fx+) (constant 1) (constant 2))))
	    a_0))
	 (scc
	  (bind ((a_0 (constant 1)))
	    (bind ((b_0 (funcall (primref $fx+) (constant 1) (constant 2))))
	      a_0))))

  ;;Nested RHS LETREC.
  ;; (doit (letrec ((a '1)
  ;; 		 (b (letrec ((d (lambda () '4)))
  ;; 		      (d)))
  ;; 		 (c '2))
  ;; 	  b)
  ;; 	(waddell
  ;; 	 (bind ((a (constant 1))
  ;; 		(c (constant 2)))
  ;; 	   (fix ((d (lambda () (constant 4))))
  ;; 	     (bind ((b (funcall d)))
  ;; 	       b))))
  ;; 	(scc
  ;; 	 (bind ((a (constant 1))
  ;; 		(c (constant 2)))
  ;; 	   (fix ((d (lambda () (constant 4))))
  ;; 	     (bind ((b (funcall d)))
  ;; 	       b)))))

;;; --------------------------------------------------------------------
;;; libraries

  ;;This test will install the library!!!
  (check
      (parametrise ((compiler.$current-letrec-pass 'waddell))
	(let* ((form1 '(library (optimize-letrec-waddell-demo-1)
			 (export a b c)
			 (import (rnrs))
			 (define (a) 1)
			 (define (b) (a) 2)
			 (define (c) (b) 3)))
	       (form2 (%expand-library form1)))
	  (%optimize-letrec form2)))
    => '(fix ((a_0 (lambda () (constant 1)))
	      (b_0 (lambda () (seq (funcall a_0) (constant 2))))
	      (c_0 (lambda () (seq (funcall b_0) (constant 3)))))
	  (funcall (primref void))))

  ;;This test will install the library!!!
  (check
      (parametrise ((compiler.$current-letrec-pass 'scc))
	(let* ((form1 '(library (optimize-letrec-scc-demo-1)
			 (export a b c)
			 (import (rnrs))
			 (define (a) 1)
			 (define (b) (a) 2)
			 (define (c) (b) 3)))
	       (form2 (%expand-library form1)))
	  (%optimize-letrec form2)))
    => '(fix ((a_0 (lambda () (constant 1)))
	      (b_0 (lambda () (seq (funcall a_0) (constant 2))))
	      (c_0 (lambda () (seq (funcall b_0) (constant 3)))))
	  (funcall (primref void))))

  #t)


(parametrise ((check-test-name	'rewrite-references-and-assignments))

  (define (%rewrite-references-and-assignments core-language-form)
    (let* ((D (compiler.$recordize core-language-form))
	   (D (compiler.$optimize-direct-calls D))
	   (D (compiler.$optimize-letrec D))
	   (D (compiler.$source-optimize D))
	   (D (compiler.$rewrite-references-and-assignments D))
	   (S (compiler.$unparse-recordized-code/sexp D)))
      S))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?core-language-form ?expected-result)
       (check
	   (%rewrite-references-and-assignments (quasiquote ?core-language-form))
	 => (quasiquote ?expected-result)))
      ))

  (define-syntax doit*
    (syntax-rules ()
      ((_ ?standard-language-form ?expected-result)
       ;;We want the ?STANDARD-LANGUAGE-FORM to appear  in the output of CHECK when a
       ;;test fails.
       (doit ,(%expand (quasiquote ?standard-language-form))
	     ?expected-result/waddell))
      ))

;;; --------------------------------------------------------------------

;;; --------------------------------------------------------------------
;;; libraries

;;;These tests will install the libraries!!!

  ;;All the bindings end in a FIX structure.
  (check
      (let* ((form1 '(library (rewrite-references-and-assignments-demo-1)
		       (export a b c)
		       (import (rnrs))
		       (define (a) 1)
		       (define (b) (read))
		       (define (c) (b))))
	     (form2 (%expand-library form1)))
	(%rewrite-references-and-assignments form2))
    => `(fix ((a_0 (lambda () (constant 1)))
	      (b_0 (lambda () (funcall (primref read))))
	      (c_0 (lambda () (funcall (primref read)))))
	  (constant ,(void))))

  #;(check
      (let* ((form1 '(library (rewrite-references-and-assignments-demo-2)
		       (export a)
		       (import (rnrs)
			 (vicare containers stacks))
		       (define a
			 (make-stack 1))
		       (define (b)
			 a)))
	     (form2 (%expand-library form1)))
	(%rewrite-references-and-assignments form2))
    => `(fix ((a_0 (lambda () (constant 1)))
	      (b_0 (lambda () (seq (funcall a_0) (constant 2))))
	      (c_0 (lambda () (seq (funcall b_0) (constant 3)))))
	  (constant ,(void))))

  #f)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'seq 'scheme-indent-function 0)
;; eval: (put 'fix 'scheme-indent-function 1)
;; eval: (put 'bind 'scheme-indent-function 1)
;; eval: (put 'conditional 'scheme-indent-function 2)
;; End:
