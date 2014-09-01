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

(compiler.optimize-level 2)
(compiler.$source-optimizer-passes-count 2)
;;(compiler.$cp0-effort-limit 50)
;;(compiler.$cp0-size-limit   8)


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
	       '(vicare system $symbols)
	       ;;We import this library to  inspect how imported bindings are handled
	       ;;by the compiler.
	       '(libtest compiler-internals)
	       ;;This is to build annotated forms and test debug-calls.
	       '(only (vicare)
		      get-annotated-datum)))

(define (%expand standard-language-form)
  (receive (code libs)
      (expand-form-to-core-language standard-language-form THE-ENVIRONMENT)
    code))

(define (%expand-library standard-language-form)
  (cdr (assq 'invoke-code (expand-library->sexp standard-language-form))))

(define (%make-annotated-form form)
  (let* ((form.str (receive (port extract)
		       (open-string-output-port)
		     (unwind-protect
			 (begin
			   (display form port)
			   (extract))
		       (close-port port))))
	 (port     (open-string-input-port form.str)))
    (unwind-protect
	(get-annotated-datum port)
      (close-port port))))


(parametrise ((check-test-name		'expansion))

  (define-syntax-rule (doit* ?standard-language-form ?expected-result)
    (check
	(%expand (quasiquote ?standard-language-form))
      => (quasiquote ?expected-result)))

  (define-syntax-rule (libdoit* ?standard-language-form ?expected-result)
    (check
	(gensyms->symbols (%expand-library (quasiquote ?standard-language-form)))
      => (quasiquote ?expected-result)))

;;; --------------------------------------------------------------------

  (doit* (fx+ 1 2)
	 (annotated-call (fx+ 1 2)
			 (primitive fx+) '1 '2))

  (doit* ($symbol-string 'ciao)
	 (annotated-call ($symbol-string 'ciao)
			 (primitive $symbol-string) 'ciao))

;;; --------------------------------------------------------------------

  (libdoit* (library (expansion-demo-1)
	      (export a b)
	      (import (rnrs)
		(libtest compiler-internals))
	      (define a
		(a-func 1 2))
	      (define (b)
		1))
	    (library-letrec* ((a a (annotated-call (a-func 1 2) a-func (quote 1) (quote 2)))
			      (b b (annotated-case-lambda b (() (quote 1)))))
	      (quote #!void)))

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

;;; --------------------------------------------------------------------
;;; sequences

  (doit* (begin '1 '2 '3)
	 (seq (constant 1) (constant 2) (constant 3)))

;;; --------------------------------------------------------------------
;;; libraries

  ;;This is the (libtest compiler-internals) library.
  (libdoit* (library (recordize-demo-0)
	      (export a-func a-thunk a-const)
	      (import (rnrs (6)))
	      (define (a-func a b)
		(+ a b))
	      (define (a-thunk)
		"ciao")
	      (define a-const 123))
	    (rec*bind ((a-func_0  (lambda (a_0 b_0) (funcall (primref +) a_0 b_0)))
		       (a-thunk_0 (lambda () (constant "ciao")))
		       (a-const_0 (constant 123)))
	      (constant #!void)))

  ;;How reference imported bindings are recordised.
  (libdoit* (library (recordize-demo-1)
	      (export)
	      (import (rnrs)
		(libtest compiler-internals))
	      (list a-const
		    (a-thunk)
		    (a-func 1 2)))
	    (rec*bind ()
	      (funcall (primref list)
		(funcall (primref top-level-value) (constant a-const))
		(funcall (funcall (primref top-level-value) (constant a-thunk)))
		(funcall (funcall (primref top-level-value) (constant a-func))
		  (constant 1) (constant 2)))))

  ;;Record type definition.
  (libdoit* (library (recordize-demo-2)
	      (export make-a a?)
	      (import (rnrs))
	      (define-record-type a))
	    (rec*bind ((a-rtd_0      (funcall (primref make-record-type-descriptor)
				       (constant a) (constant #f) (constant #f)
				       (constant #f) (constant #f) (constant #())))
		       (a-protocol_0 (constant #f))
		       (a-rcd_0      (funcall (primref make-record-constructor-descriptor)
				       a-rtd_0 (constant #f) a-protocol_0))
		       (a?_0         (funcall (primref record-predicate) a-rtd_0))
		       (make-a_0     (funcall (primref record-constructor) a-rcd_0))
		       (a-first-field-offset_0
			(funcall (primref $struct-ref) a-rtd_0 (constant 3))))
	      (constant #!void)))

;;; --------------------------------------------------------------------
;;; debug calls, no annotation

  (parametrise ((compiler.$generate-debug-calls #t))

    (doit* (list '1 '2)
	   (funcall (primref debug-call) (constant (#f . (list '1 '2)))
		    (primref list) (constant 1) (constant 2)))

    (doit* ((lambda (x y) (list x y)) '1 '2)
	   (funcall (primref debug-call)
	     (constant (#f . ((lambda (x y) (list x y)) '1 '2)))
	     (lambda (x_0 y_0)
	       (funcall (primref debug-call)
		 (constant (#f . (list x y)))
		 (primref list)
		 x_0 y_0))
	     (constant 1) (constant 2)))

    (doit* ((lambda (x) x) '1)
	   (funcall (primref debug-call)
	     (constant (#f . ((lambda (x) x) '1)))
	     (lambda (x_0) x_0)
	     (constant 1)))

    (doit* (let ((x '1)
		 (y '2))
	     (list x y))
	   (bind ((x_0 (constant 1))
		  (y_0 (constant 2)))
	     (funcall (primref debug-call) (constant (#f . (list x y)))
		      (primref list) x_0 y_0)))

    (doit* (let ((f (lambda (x) x)))
	     (f '1))
	   (bind ((f_0 (lambda (x_0) x_0)))
	     (funcall (primref debug-call) (constant (#f . (f '1)))
		      f_0 (constant 1))))

    #f)

;;; --------------------------------------------------------------------
;;; debug calls, annotation

;;;The format of the annotation is:
;;;
;;;   (constant (?annotation-source . ?source-form))
;;;
;;;where ?ANNOTATION-SOURCE has one of the formats:
;;;
;;;   #f
;;;   (?port-identifier . ?first-character-offset)
;;;

  (parametrise ((compiler.$generate-debug-calls #t))

    (doit* ,(%make-annotated-form '(list 1 2))
	   (funcall (primref debug-call)
	     (constant (("*string-input-port*" . 0) . (list 1 2)))
	     (primref list)
	     (constant 1) (constant 2)))

    (doit* ,(%make-annotated-form '((lambda (x) x) 1))
	   (funcall (primref debug-call)
	     (constant (("*string-input-port*" . 0) . ((lambda (x) x) 1)))
	     (lambda (x_0) x_0)
	     (constant 1)))

    (doit* ,(%make-annotated-form '(let ((x '1)
                           		 (y '2))
                           	     (list x y)))
	   (bind ((x_0 (constant 1))
		  (y_0 (constant 2)))
	     (funcall (primref debug-call)
	       (constant (("*string-input-port*" . 21) . (list x y)))
	       (primref list)
	       x_0 y_0)))

    (doit* ,(%make-annotated-form '(let ((f (lambda (x) x)))
				     (f '1)))
	   (bind ((f_0 (lambda (x_0) x_0)))
	     (funcall (primref debug-call)
	       (constant (("*string-input-port*". 26) . (f '1)))
	       f_0
	       (constant 1))))

    (doit* ,(%make-annotated-form '((let ((f (lambda (x) x)))
				      f)
				    1))
	   (funcall (primref debug-call)
	     (constant (("*string-input-port*" . 0) . ((let ((f (lambda (x) x))) f) 1)))
	     (bind ((f_0 (lambda (x_0) x_0))) f_0)
	     (constant 1)))

    #f)

  #t)


(parametrise ((check-test-name		'direct-calls-optimisation))

  (define (%optimize-direct-calls core-language-form)
    (let* ((D (compiler.$recordize core-language-form))
	   (D (compiler.$optimize-direct-calls D))
	   (S (compiler.$unparse-recordized-code/sexp D)))
      S))

  (define-syntax-rule (doit ?core-language-form ?expected-result)
    (check
	(%optimize-direct-calls (quasiquote ?core-language-form))
      => (quasiquote ?expected-result)))

  (define-syntax-rule (doit* ?standard-language-form ?expected-result)
    (doit ,(%expand (quasiquote ?standard-language-form)) ?expected-result))

  (define-syntax-rule (libdoit* ?standard-language-form ?expected-result)
    (doit ,(%expand-library (quasiquote ?standard-language-form)) ?expected-result))

;;; --------------------------------------------------------------------
;;; simple demos

  (doit* (fx+ 1 2)
	 (funcall (primref fx+) (constant 1) (constant 2)))

;;; --------------------------------------------------------------------
;;; CLAMBDA integration

  (doit* ((lambda (x) x) '1)
	 (bind ((x_0 (constant 1)))
	   x_0))

  (doit* ((case-lambda
	   ((x y) y)
	   ((x) x)
	   (args args))
	  '1)
	 (bind ((x_0 (constant 1)))
	   x_0))

;;; --------------------------------------------------------------------
;;; binding forms integration

  (doit* ((let ((x '1))
	    (lambda (y) (list x y)))
	  '2)
	 (bind ((x_0 (constant 1)))
	   (bind ((y_0 (constant 2)))
	     (funcall (primref list) x_0 y_0))))

  ;;CLAMBDA body
  (doit ((let ((x '1))
	   (lambda (y) ((primitive list) x y)))
	 '2)
	(bind ((x_0 (constant 1)))
	  (bind ((y_0 (constant 2)))
	    (funcall (primref list) x_0 y_0))))

  ;;CLAMBDA body
  (doit ((letrec ((x '1))
	   (lambda (y) ((primitive list) x y)))
	 '2)
	(recbind ((x_0 (constant 1)))
	  (bind ((y_0 (constant 2)))
	    (funcall (primref list) x_0 y_0))))

  ;;CLAMBDA body
  (doit ((letrec* ((x '1))
	   (lambda (y) ((primitive list) x y)))
	 '2)
	(rec*bind ((x_0 (constant 1)))
	  (bind ((y_0 (constant 2)))
	    (funcall (primref list) x_0 y_0))))

  ;;prelex body
  (doit ((let ((f (lambda (y) y)))
	   f)
	 '1)
	(bind ((f_0 (lambda (y_0) y_0)))
	  (funcall f_0 (constant 1))))

  ;;prelex body
  (doit ((letrec ((f (lambda (y) y)))
	   f)
	 '1)
	(recbind ((f_0 (lambda (y_0) y_0)))
	  (funcall f_0 (constant 1))))

  ;;prelex body
  (doit ((letrec* ((f (lambda (y) y)))
	   f)
	 '1)
	(rec*bind ((f_0 (lambda (y_0) y_0)))
	  (funcall f_0 (constant 1))))

  ;;multiple expressions body
  (doit ((let ((x '1))
	   (begin
	     ((primitive display) '1)
	     (lambda (y) ((primitive list) x y))))
	 '2)
	(bind ((x_0 (constant 1)))
	  (bind ((tmp_0 (seq
			  (funcall (primref display) (constant 1))
			  (lambda (y_0)
			    (funcall (primref list) x_0 y_0)))))
	    (funcall tmp_0 (constant 2)))))

;;; --------------------------------------------------------------------
;;; call-with-values

  ;;No possible integration.
  (doit* (call-with-values
	     (lambda ()
	       (values (read) (read)))
	   (lambda (x y)
	     (list x y)))
	 (funcall (primref call-with-values)
	   (lambda ()
	     (funcall (primref values) (funcall (primref read)) (funcall (primref read))))
	   (lambda (x_0 y_0)
	     (funcall (primref list) x_0 y_0))))

  ;;Consumer is a CLAMBDA with single argument.
  (doit* (call-with-values
	     (lambda ()
	       (read))
	   (lambda (x)
	     (list x)))
	 (bind ((x_0 (bind ()
		       (funcall (primref read)))))
	   (funcall (primref list) x_0)))

;;; --------------------------------------------------------------------
;;; special cases

  (doit* (cond ((read)
		=> (lambda (Y)
		     (write Y)))
	       (else
		(read)))
	 (bind ((t_0 (funcall (primref read))))
	   (conditional t_0
	       (bind ((Y_0 t_0))
		 (funcall (primref write) Y_0))
	     (funcall (primref read)))))

;;; --------------------------------------------------------------------
;;; debugging calls

  (parametrise ((compiler.$generate-debug-calls #t))

    (doit* ,(%make-annotated-form '((lambda (x) x) '1))
	   (bind ((x_0 (constant 1)))
	     x_0))

    (doit* ,(%make-annotated-form '((let ((f (lambda (y) y)))
				      f)
				    1))
	   (bind ((f_0 (lambda (y_0) y_0)))
	     (funcall (primref debug-call)
	       (constant (("*string-input-port*" . 0) . ((let ((f (lambda (y) y))) f) 1)))
	       f_0 (constant 1))))

    #f)

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
		(assign-init a_0 (lambda () (constant 1)))
		(assign-init b_0 (lambda () (seq (funcall a_0) (constant 2))))
		(assign-init c_0 (lambda () (seq (funcall b_0) (constant 3))))
		(constant #!void))))

;;; --------------------------------------------------------------------
;;; nested binding forms: nested RHS

  ;;nested LET RHS
  (doit (letrec* ((a (lambda () '1))
		  (c (let ((b '2))
		       ((primitive +) a b)))
		  (d (lambda () '4)))
	  ((primitive list) a c d))
	(bind ((a_0 (constant #!void))
	       (b_0 (constant #!void))
	       (b_1 (constant #!void))
	       (c_0 (constant #!void))
	       (d_0 (constant #!void)))
	  (seq
	    (assign a_0 (lambda () (constant 1)))
	    (assign b_0 (constant 2))
	    (assign b_1 b_0)
	    (assign c_0 (funcall (primref +) a_0 b_1))
	    (assign d_0 (lambda () (constant 4)))
	    (funcall (primref list) a_0 c_0 d_0))))

  ;;nested LETREC RHS
  (doit (letrec* ((a (lambda () '1))
		  (c (letrec ((b '2))
		       ((primitive +) a b)))
		  (d (lambda () '4)))
	  ((primitive list) a c d))
	(bind ((a_0 (constant #!void))
	       (b_0 (constant #!void))
	       (b_1 (constant #!void))
	       (c_0 (constant #!void))
	       (d_0 (constant #!void)))
	  (seq
	    (assign a_0 (lambda () (constant 1)))
	    (assign b_0 (constant 2))
	    (assign b_1 b_0)
	    (assign c_0 (funcall (primref +) a_0 b_1))
	    (assign d_0 (lambda () (constant 4)))
	    (funcall (primref list) a_0 c_0 d_0))))

  ;;nested LETREC* RHS
  (doit (letrec* ((a (lambda () '1))
		  (c (letrec* ((b '2))
		       ((primitive +) a b)))
		  (d (lambda () '4)))
	  ((primitive list) a c d))
	(bind ((a_0 (constant #!void))
	       (b_0 (constant #!void))
	       (b_1 (constant #!void))
	       (c_0 (constant #!void))
	       (d_0 (constant #!void)))
	  (seq
	    (assign a_0 (lambda () (constant 1)))
	    (assign b_0 (constant 2))
	    (assign b_1 b_0)
	    (assign c_0 (funcall (primref +) a_0 b_1))
	    (assign d_0 (lambda () (constant 4)))
	    (funcall (primref list) a_0 c_0 d_0))))

  ;;preparatory exapmle for the double nested LETREC* and LET RHS below
  (doit (letrec* ((d '3)
		  (e (let ((g '5))
		       ((primitive list) d g)))
		  (f '4))
	  ((primitive list) d e f))
	(bind ((d_0 (constant #!void))
	       (g_0 (constant #!void))
	       (g_1 (constant #!void))
	       (e_0 (constant #!void))
	       (f_0 (constant #!void)))
	  (seq
	    (assign d_0 (constant 3))
	    (assign g_0 (constant 5))
	    (assign g_1 g_0)
	    (assign e_0 (funcall (primref list) d_0 g_1))
	    (assign f_0 (constant 4))
	    (funcall (primref list) d_0 e_0 f_0))))

  ;;double nested LETREC* and LET RHS
  (doit (letrec* ((a (lambda () '1))
		  (b (letrec* ((d '3)
			       (e (let ((g '5))
				    ((primitive list) d g)))
			       (f '4))
		       ((primitive list) d e f)))
		  (c (lambda () '2)))
	  ((primitive list) a b c))
	(bind ((a_0 (constant #!void))
	         (d_0 (constant #!void))
	         (g_0 (constant #!void))
	         (g_1 (constant #!void))
	         (e_0 (constant #!void))
	         (f_0 (constant #!void))
	       (d_1 (constant #!void))
	       (g_2 (constant #!void))
	       (g_3 (constant #!void))
	       (e_1 (constant #!void))
	       (f_1 (constant #!void))
	       (b_0 (constant #!void))
	       (c_0 (constant #!void)))
	  (seq (assign a_0 (lambda () (constant 1)))
	       (assign d_0 (constant 3)) (assign g_0 (constant 5))
	       (assign g_1 g_2)
	       (assign e_0 (funcall (primref list) d_1 g_3))
	       (assign f_0 (constant 4)) (assign d_1 d_0)
	       (assign g_2 g_0) (assign g_3 g_1) (assign e_1 e_0)
	       (assign f_1 f_0)
	       (assign b_0 (funcall (primref list) d_1 e_1 f_1))
	       (assign c_0 (lambda () (constant 2)))
	       (funcall (primref list) a_0 b_0 c_0))))

;;; --------------------------------------------------------------------
;;; nested binding forms: nested body

  ;;nested LET body
  (doit (letrec* ((a (lambda () '1)))
	  (let ((b (lambda () '2)))
	    ((primitive list) a b)))
	(bind ((a_0 (constant #!void))
	       (b_0 (constant #!void)))
	  (seq
	    (assign a_0 (lambda () (constant 1)))
	    (assign b_0 (lambda () (constant 2)))
	    (funcall (primref list) a_0 b_0))))

  ;;nested LETREC body
  (doit (letrec* ((a (lambda () '1)))
	  (letrec ((b (lambda () '2)))
	    ((primitive list) a b)))
	(bind ((a_0 (constant #!void))
	       (b_0 (constant #!void)))
	  (seq
	    (assign a_0 (lambda () (constant 1)))
	    (assign b_0 (lambda () (constant 2)))
	    (funcall (primref list) a_0 b_0))))

  ;;nested LETREC* body
  (doit (letrec* ((a (lambda () '1)))
	  (letrec* ((b (lambda () '2)))
	    ((primitive list) a b)))
	(bind ((a_0 (constant #!void))
	       (b_0 (constant #!void)))
	  (seq
	    (assign a_0 (lambda () (constant 1)))
	    (assign b_0 (lambda () (constant 2)))
	    (funcall (primref list) a_0 b_0))))

  ;;double nested LETREC* body
  (doit (letrec* ((a (lambda () '1)))
	  (letrec* ((b (lambda () '2)))
	    (letrec* ((c (lambda () '3)))
	      ((primitive list) a b c))))
	(bind ((a_0 (constant #!void))
	       (b_0 (constant #!void))
	       (c_0 (constant #!void)))
	  (seq
	    (assign a_0 (lambda () (constant 1)))
	    (assign b_0 (lambda () (constant 2)))
	    (assign c_0 (lambda () (constant 3)))
	    (funcall (primref list) a_0 b_0 c_0))))

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
	  (bind ((a_0 (constant #!void))
		 (a_1 (constant #!void)))
	    (seq
	      (assign a_0 (constant 1))
	      (assign a_1 a_0)
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

  ;;Weird case number 1.
  (doit (letrec* ((b (lambda () a))
		  (a (b)))
	  '#!void)
	(waddell
	 (bind ((a_0 (constant #!void)))
	   (fix ((b_0 (lambda () a_0)))
	     (seq
	       (assign a_0 (funcall b_0))
	       (constant #!void)))))
	(scc
	 (bind ((a_0 (constant #!void)))
	   (fix ((b_0 (lambda () a_0)))
	     (seq
	       (assign a_0 (funcall b_0))
	       (constant #!void))))))

  ;;Weird case number 2.
  (doit (letrec* ((b (lambda () ((primitive list) a)))
		  (a (b)))
	  '#!void)
	(waddell
	 (bind ((a_0 (constant #!void)))
	   (fix ((b_0 (lambda () (funcall (primref list) a_0))))
	     (seq
	       (assign a_0 (funcall b_0))
	       (constant #!void)))))
	(scc
	 (bind ((a_0 (constant #!void)))
	   (fix ((b_0 (lambda () (funcall (primref list) a_0))))
	     (seq
	       (assign a_0 (funcall b_0))
	       (constant #!void))))))

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
			     (constant #!void)))))
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
			 (constant #!void)))))
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
  (doit (letrec* ((a '1)
		  (b (letrec* ((d (lambda () '4)))
		       (d)))
		  (c '2))
  	  b)
  	(waddell
  	 (bind ((a_0 (constant 1))
  		(c_0 (constant 2)))
	   (bind ((d_0 (constant #!void))
		  (b_0 (constant #!void)))
	     (fix ((d_1 (lambda () (constant 4))))
	       (seq
		 (assign d_0 d_1)
		 (assign b_0 (funcall d_0))
		 b_0)))))
  	(scc
  	 (bind ((a_0 (constant 1)))
  	   (fix ((d_0 (lambda () (constant 4))))
	     (bind ((d_1 d_0))
	       (bind ((b_0 (funcall d_1)))
		 (bind ((c_0 (constant 2)))
		   b_0)))))))

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
	  (constant #!void)))

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
	  (constant #!void)))

  ;;Record type definition.
  (check
      (parametrise ((compiler.$current-letrec-pass 'scc))
	(let* ((form1 '(library (optimize-letrec-scc-demo-2)
			 (export make-a a?)
			 (import (rnrs))
			 (define-record-type a)))
	       (form2 (%expand-library form1)))
	  (%optimize-letrec form2)))
    => '(bind ((a-rtd_0 (funcall (primref make-record-type-descriptor)
			  (constant a) (constant #f) (constant #f)
			  (constant #f) (constant #f) (constant #()))))
	  (bind ((a-protocol_0 (constant #f)))
	    (bind ((a-rcd_0 (funcall (primref make-record-constructor-descriptor)
			      a-rtd_0 (constant #f) a-protocol_0)))
	      (bind ((a?_0 (funcall (primref record-predicate) a-rtd_0)))
		(bind ((make-a_0 (funcall (primref record-constructor) a-rcd_0)))
		  (bind ((a-first-field-offset_0 (funcall (primref $struct-ref) a-rtd_0 (constant 3))))
		    (constant #!void))))))))

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
	     ?expected-result))
      ))

;;; --------------------------------------------------------------------
;;; read-only bindings

  ;;Weird case  number 1.   Originally read-only bindings  that are  transformed into
  ;;read-write  bindings by  the SCC  letrec optimiser,  but then  retransformed into
  ;;read-only by the source optimiser.
  (doit (letrec* ((b (lambda () a))
		  (a (b)))
	  '#!void)
	(constant #!void))

;;; --------------------------------------------------------------------
;;; read-write bindings

  (doit* (let ((a '1))
	   (set! a '2)
	   a)
	 (bind ((a_0 (constant 1)))
	   (bind ((a_1 (funcall (primref vector) a_0)))
	     (seq
	       (funcall (primref $vector-set!) a_1 (constant 0) (constant 2))
	       (funcall (primref $vector-ref)  a_1 (constant 0))))))

  (doit* (let ((a '1)
	       (b '2))
	   (set! a '3)
	   (set! b '4)
	   (list a b))
	 (bind ((a_0 (constant 1))
		(b_0 (constant 2)))
	   (bind ((a_1 (funcall (primref vector) a_0))
		  (b_1 (funcall (primref vector) b_0)))
	     (seq
	       (funcall (primref $vector-set!) a_1 (constant 0) (constant 3))
	       (funcall (primref $vector-set!) b_1 (constant 0) (constant 4))
	       (funcall (primref list)
		 (funcall (primref $vector-ref) a_1 (constant 0))
		 (funcall (primref $vector-ref) b_1 (constant 0)))))))

  ;;Lambda form
  (doit* (lambda (a)
	   (display a)
	   (set! a 1)
	   a)
	 (lambda (a_0)
	   (bind ((a_1 (funcall (primref vector) a_0)))
	     (seq
	       (funcall (primref display)
		 (funcall (primref $vector-ref) a_1 (constant 0)))
	       (funcall (primref $vector-set!) a_1 (constant 0) (constant 1))
	       (funcall (primref $vector-ref)  a_1 (constant 0))))))

;;; --------------------------------------------------------------------
;;; LIBRARY-LETREC* forms

  ;;Simple unassigned bindings.
  (doit (library-letrec* ((a.lex a.loc '1)
			  (b.lex b.loc '2))
	  ((primitive display) a.lex b.lex))
	(bind ((a.lex_0 (constant 1)))
	  (bind ((b.lex_0 (constant 2)))
	    (funcall (primref display) (constant 1) (constant 2)))))

  ;;Simple assigned bindings.
  (doit (library-letrec* ((a.lex a.loc '1)
			  (b.lex b.loc '2))
	  (begin
	    (set! a.lex '11)
	    (set! b.lex '22)
	    ((primitive display) a.lex b.lex)))
	(bind ((a.lex_0 (constant 1)))
	  (bind ((b.lex_0 (constant 2)))
	    (seq
	      (funcall (primref $set-symbol-value!)
		(constant a.loc)
		(constant 11))
	      (funcall (primref $set-symbol-value!)
		(constant b.loc)
		(constant 22))
	      (funcall (primref display)
		(funcall (primref $symbol-value) (constant a.loc))
		(funcall (primref $symbol-value) (constant b.loc)))))))

  ;;Recursive top level binding with single init assignment.
  (doit (library-letrec* ((b.lex b.loc (lambda () a.lex))
			  (a.lex a.loc (b.lex)))
	  '#!void)
	(bind ((a.lex_0 (constant #!void)))
	  (fix ((b.lex_0 (lambda ()
			   (funcall (primref $symbol-value)
			     (constant a.loc)))))
	    (constant #!void))))

  ;;Recursive top level binding with assignment.
  (doit (library-letrec* ((a.lex a.loc (lambda () a.lex))
			  (b.lex b.loc (lambda () (set! a.lex '123))))
	  '#!void)
	(bind ((a.lex_0 (constant #!void)))
	  (seq
	    (funcall (primref $set-symbol-value!)
	      (constant a.loc)
	      (lambda ()
		(funcall (primref $symbol-value)
		  (constant a.loc))))
	    (fix ((b.lex_0 (lambda ()
			     (seq
			       (funcall (primref $set-symbol-value!)
				 (constant a.loc)
				 (constant 123))
			       (constant #!void)))))
	      (constant #!void)))))

  ;;All bindings in FIX.
  (doit (library-letrec*
	    ((a.lex a.loc (lambda () '1))
	     (b.lex a.loc (lambda () '2)))
	  ((primitive display) (a.lex)))
	(fix ((a.lex_0 (lambda () (constant 1)))
	      (b.lex_0 (lambda () (constant 2))))
	  (funcall (primref display) (constant 1))))

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

  (check
      (let* ((form1 '(library (rewrite-references-and-assignments-demo-2)
		       (export a b)
		       (import (rnrs)
			 (libtest compiler-internals))
		       (define a
			 (a-func 1 2))
		       (define (b)
			 1)))
	     (form2 (%expand-library form1)))
	(%rewrite-references-and-assignments form2))
    => '(bind ((a_0 (funcall (funcall (primref top-level-value) (constant a-func))
		      (constant 1)
		      (constant 2))))
	  (fix ((b_0 (lambda () (constant 1))))
	    (constant #!void))))

  ;;Record type definition.
  (check
      (let* ((form1 '(library (rewrite-references-and-assignments-demo-3)
		       (export make-a a?)
		       (import (rnrs))
		       (define-record-type a)))
	     (form2 (%expand-library form1)))
	(%rewrite-references-and-assignments form2))
    => '(bind ((a-rtd_0 (funcall (primref make-record-type-descriptor)
			  (constant a) (constant #f) (constant #f)
			  (constant #f) (constant #f) (constant #()))))
	  (bind ((a-protocol_0 (constant #f)))
	    (bind ((a-rcd_0 (funcall (primref make-record-constructor-descriptor)
			      (funcall (primref $symbol-value) (constant a-rtd))
			      (constant #f) (constant #f))))
	      (bind ((a?_0 (funcall (primref record-predicate)
			     (funcall (primref $symbol-value) (constant a-rtd)))))
		(bind ((make-a_0 (funcall (primref record-constructor)
				   (funcall (primref $symbol-value) (constant a-rcd)))))
		  (bind ((a-first-field-offset_0 (funcall (primref $struct-ref)
						   (funcall (primref $symbol-value) (constant a-rtd))
						   (constant 3))))
		    (constant #!void))))))))

  #f)


(parametrise ((check-test-name	'sanitise-bindings))

  (define (%sanitize-bindings core-language-form)
    (let* ((D (compiler.$recordize core-language-form))
	   (D (compiler.$optimize-direct-calls D))
	   (D (compiler.$optimize-letrec D))
	   (D (compiler.$source-optimize D))
	   (D (compiler.$rewrite-references-and-assignments D))
	   (D (compiler.$sanitize-bindings D))
	   (S (compiler.$unparse-recordized-code/sexp D)))
      S))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?core-language-form ?expected-result)
       (check
	   (%sanitize-bindings (quasiquote ?core-language-form))
	 => (quasiquote ?expected-result)))
      ))

  (define-syntax doit*
    (syntax-rules ()
      ((_ ?standard-language-form ?expected-result)
       ;;We want the ?STANDARD-LANGUAGE-FORM to appear  in the output of CHECK when a
       ;;test fails.
       (doit ,(%expand (quasiquote ?standard-language-form))
	     ?expected-result))
      ))

;;; --------------------------------------------------------------------

  (doit (lambda () '1)
	(fix ((tmp_0 (lambda () (constant 1))))
	  tmp_0))

  (doit (let ((a (lambda () '1)))
	  a)
	(fix ((a_0 (lambda () (constant 1))))
	  a_0))

  (doit (let ((a (lambda () '1))
	      (b ((primitive read))))
	  ((primitive list) a b))
	(bind ((b_0 (funcall (primref read))))
	  (fix ((a_0 (lambda () (constant 1))))
	    (funcall (primref list) a_0 b_0))))

  (doit (let ((a (lambda () '1)))
	  (begin
	    (set! a '2)
	    a))
	(fix ((a_0 (lambda () (constant 1))))
	  (bind ((a_1 (funcall (primref vector) a_0)))
	    (seq
	      (funcall (primref $vector-set!) a_1 (constant 0) (constant 2))
	      (funcall (primref $vector-ref)  a_1 (constant 0))))))

  #t)


(parametrise ((check-test-name	'insert-global-assignments))

  (define (%insert-global-assignments core-language-form)
    (let* ((D (compiler.$recordize core-language-form))
	   (D (compiler.$optimize-direct-calls D))
	   (D (compiler.$optimize-letrec D))
	   (D (compiler.$source-optimize D))
	   (D (compiler.$rewrite-references-and-assignments D))
	   (D (compiler.$sanitize-bindings D))
	   (D (compiler.$optimize-for-direct-jumps D))
	   (D (compiler.$insert-global-assignments D))
	   (S (compiler.$unparse-recordized-code/sexp D)))
      S))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?core-language-form ?expected-result)
       (check
	   (%insert-global-assignments (quasiquote ?core-language-form))
	 => (quasiquote ?expected-result)))
      ))

  (define-syntax doit*
    (syntax-rules ()
      ((_ ?standard-language-form ?expected-result)
       ;;We want the ?STANDARD-LANGUAGE-FORM to appear  in the output of CHECK when a
       ;;test fails.
       (doit ,(%expand (quasiquote ?standard-language-form))
	     ?expected-result))
      ))

  (define-syntax libdoit*
    (syntax-rules (basic waddell scc)
      ((_ ?standard-language-form ?expected-result/basic)
       (doit ,(%expand-library (quasiquote ?standard-language-form)) ?expected-result/basic))
      ))

;;; --------------------------------------------------------------------

  (doit (library-letrec*
	    ((a.lex a.loc (lambda () '1))
	     (b.lex b.loc (lambda () '2))
	     (c.lex c.loc (lambda () '3))
	     (d.lex d.loc '4))
	  (quote #!void))
	(fix ((a.lex_0 (lambda () (constant 1)))
	      (b.lex_0 (lambda () (constant 2)))
	      (c.lex_0 (lambda () (constant 3))))
	  (seq
	    (funcall (primref $set-symbol-value/proc!) (constant a.loc) a.lex_0)
	    (funcall (primref $init-symbol-value!)     (constant b.loc) b.lex_0)
	    (funcall (primref $init-symbol-value!)     (constant c.loc) c.lex_0)
	    (bind ((d.lex_0 (constant 4)))
	      (seq
		(funcall (primref $init-symbol-value!) (constant d.loc) d.lex_0)
		(constant #!void))))))

;;; --------------------------------------------------------------------
;;; libraries

  (libdoit* (library (insert-global-assignments-demo-1)
	      (export a b c d)
	      (import (rnrs))
	      (define (a) '1)
	      (define (b) '2)
	      (define (c) '3)
	      (define d '4))
	    (fix ((a_0 (lambda () (constant 1)))
		  (b_0 (lambda () (constant 2)))
		  (c_0 (lambda () (constant 3))))
	      (seq
		(funcall (primref $set-symbol-value/proc!) (constant a) a_0)
		(funcall (primref $init-symbol-value!)     (constant b) b_0)
		(funcall (primref $init-symbol-value!)     (constant c) c_0)
		(bind ((d_0 (constant 4)))
		  (seq
		    (funcall (primref $init-symbol-value!) (constant d) d_0)
		    (constant #!void))))))
  #t)


(parametrise ((check-test-name	'lift-codes))

  (define (%lift-codes core-language-form)
    (let* ((D (compiler.$recordize core-language-form))
	   (D (compiler.$optimize-direct-calls D))
	   (D (compiler.$optimize-letrec D))
	   (D (compiler.$source-optimize D))
	   (D (compiler.$rewrite-references-and-assignments D))
	   (D (compiler.$sanitize-bindings D))
	   (D (compiler.$optimize-for-direct-jumps D))
	   (D (compiler.$insert-global-assignments D))
	   (D (compiler.$introduce-vars D))
	   (D (compiler.$convert-closures D))
	   (D (compiler.$optimize-closures/lift-codes D))
	   (S (compiler.$unparse-recordized-code/sexp D)))
      S))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?core-language-form ?expected-result)
       (check
	   (%lift-codes (quasiquote ?core-language-form))
	 => (quasiquote ?expected-result)))
      ))

  (define-syntax doit*
    (syntax-rules ()
      ((_ ?standard-language-form ?expected-result)
       ;;We want the ?STANDARD-LANGUAGE-FORM to appear  in the output of CHECK when a
       ;;test fails.
       (doit ,(%expand (quasiquote ?standard-language-form))
	     ?expected-result))
      ))

  (define-syntax libdoit*
    (syntax-rules (basic waddell scc)
      ((_ ?standard-language-form ?expected-result/basic)
       (doit ,(%expand-library (quasiquote ?standard-language-form)) ?expected-result/basic))
      ))

;;; --------------------------------------------------------------------

  (doit (library-letrec*
	    ((a.lex a.loc (lambda () '1))
	     (b.lex b.loc (lambda () '2))
	     (c.lex c.loc (lambda () '3))
	     (d.lex d.loc '4))
	  (quote #!void))
	(codes
	 ((lambda () (constant 3))
	  (lambda () (constant 2))
	  (lambda () (constant 1)))
	 (seq
	   (funcall (primref $set-symbol-value/proc!)
	     (constant a.loc)
	     (closure (code-loc a.lex) () #f))
	   (funcall (primref $init-symbol-value!)
	     (constant b.loc)
	     (closure (code-loc b.lex) () #f))
	   (funcall (primref $init-symbol-value!)
	     (constant c.loc)
	     (closure (code-loc c.lex) () #f))
	   (bind ((d.lex_0 (constant 4)))
	     (seq
	       (funcall (primref $init-symbol-value!)
		 (constant d.loc) d.lex_0)
	       (constant #!void))))))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'bind			'scheme-indent-function 1)
;; eval: (put 'fix			'scheme-indent-function 1)
;; eval: (put 'recbind			'scheme-indent-function 1)
;; eval: (put 'rec*bind			'scheme-indent-function 1)
;; eval: (put 'seq			'scheme-indent-function 0)
;; eval: (put 'conditional		'scheme-indent-function 2)
;; eval: (put 'funcall			'scheme-indent-function 1)
;; eval: (put 'library-letrec*		'scheme-indent-function 1)
;; End:
