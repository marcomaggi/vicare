;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the compiler internals, core type inference pass
;;;Date: Tue Nov 11, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (only (vicare expander)
	expand-form-to-core-language)
  (only (vicare libraries)
	expand-library->sexp)
  (prefix (vicare compiler)
	  compiler.))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare compiler: core type inference pass\n")

(compiler.optimize-level 2)
(compiler.source-optimizer-passes-count 2)
;;(compiler.cp0-effort-limit 50)
;;(compiler.cp0-size-limit   8)


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
  (environment '(vicare)
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

;;; --------------------------------------------------------------------

(define (%core-type-inference core-language-form)
  (let* ((D (compiler.recordize core-language-form))
	 (D (compiler.optimize-direct-calls D))
	 (D (compiler.optimize-letrec D))
	 ;;By skipping source optimisation we  make it simpler to write inspectable
	 ;;test cases.
	 #;(D (compiler.source-optimize D))
	 (D (compiler.rewrite-references-and-assignments D))
	 (D (compiler.core-type-inference D))
	 (S (compiler.unparse-recordized-code/sexp D)))
    S))

(define-syntax doit
  (syntax-rules ()
    ((_ ?core-language-form ?expected-result)
     (check
	 (%core-type-inference (quasiquote ?core-language-form))
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


(parametrise ((check-test-name	'numbers))

  ;;fixnum predicate
  (doit* (let ((x (read)))
	   (cond ((fixnum? x)
		  (display x))))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref fixnum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:fixnum T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object)))
	     (funcall (primref void)))))

  ;;bignum predicate
  (doit* (let ((x (read)))
	   (cond ((bignum? x)
		  (display x))))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref bignum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:bignum T:non-false T:nonimmediate T:exact-real T:real T:exact-integer T:exact T:number T:object)))
	     (funcall (primref void)))))

  ;;ratnum predicate
  (doit* (let ((x (read)))
	   (cond ((ratnum? x)
		  (display x))))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref ratnum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:ratnum T:non-false T:nonimmediate T:exact-real T:real T:exact T:number T:object)))
	     (funcall (primref void)))))

  ;;flonum predicate
  (doit* (let ((x (read)))
	   (cond ((flonum? x)
		  (display x))))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref flonum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:non-false T:nonimmediate T:real T:inexact T:flonum T:number T:object)))
	     (funcall (primref void)))))

  ;;cflonum predicate
  (doit* (let ((x (read)))
	   (cond ((cflonum? x)
		  (display x))))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref cflonum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:cflonum T:non-false T:nonimmediate T:non-real T:inexact T:number T:object)))
	     (funcall (primref void)))))

  ;;compnum predicate
  (doit* (let ((x (read)))
	   (cond ((compnum? x)
		  (display x))))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref compnum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:compnum T:non-false T:nonimmediate T:non-real T:number T:object)))
	     (funcall (primref void)))))

;;; --------------------------------------------------------------------

  ;;Test for numbers.
  #;(doit* (let ((x (read)))
	   (cond ((fixnum? x)
		  (display x))
		 ((flonum? x)
		  (display x))
		 ((bignum? x)
		  (display x))
		 ((ratnum? x)
		  (display x))
		 ((compnum? x)
		  (display x))
		 ((cflonum? x)
		  (display x))
		 ((number? x)
		  (display x))
		 ((complex? x)
		  (display x))
		 ((real? x)
		  (display x))
		 ((rational? x)
		  (display x))
		 ((exact? x)
		  (display x))
		 ((inexact? x)
		  (display x))
		 (else
		  (display x))))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref fixnum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:fixnum T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object)))
	     (conditional (funcall (primref flonum?) x_0)
		 (funcall (primref display)
		   (known x_0 (T:non-false T:real T:nonimmediate T:inexact T:flonum T:number T:object)))
	       (conditional (funcall (primref bignum?) x_0)
		   (funcall (primref display)
		     (known x_0 (T:bignum T:non-false T:nonimmediate T:exact-real T:real T:exact-integer T:exact T:number T:object)))
		 (conditional (funcall (primref ratnum?) x_0)
		     (funcall (primref display)
		       (known x_0 (T:ratnum T:non-false T:nonimmediate T:exact-real T:real T:exact T:number T:object)))
		   (conditional (funcall (primref compnum?) x_0)
		       (funcall (primref display)
			 (known x_0 ())
			 #;(known x_0 (T:compnum T:non-false T:nonimmediate T:number T:object))
			 )
		     (conditional (funcall (primref cflonum?) x_0)
			 (funcall (primref display)
			   (known x_0 (T:cflonum T:non-false T:nonimmediate T:non-real T:inexact T:number T:object)))
		       (conditional (funcall (primref number?) x_0)
			   (funcall (primref display)
			     (known x_0 (T:non-false T:number T:object)))
			 (conditional (funcall (primref complex?) x_0)
			     (funcall (primref display)
			       (known x_0 (T:cflonum T:non-false T:nonimmediate T:non-real T:inexact T:number T:object)))
			   (conditional (funcall (primref real?) x_0)
			       (funcall (primref display)
				 (known x_0 (T:non-false T:real T:number T:object)))
			     (conditional (funcall (primref rational?) x_0)
				 (funcall (primref display)
				   (known x_0 (T:non-false T:number T:object)))
			       (conditional (funcall (primref exact?) x_0)
				   (funcall (primref display)
				     (known x_0 (T:non-false T:real T:exact T:number T:object)))
				 (conditional (funcall (primref inexact?) x_0)
				     (funcall (primref display)
				       (known x_0 (T:non-false T:nonimmediate T:inexact T:number T:object)))
				   (funcall (primref display) x_0)))))))))))))))

  #t)


(parametrise ((check-test-name	'fixnums))

  (doit (let ((f (lambda (x) x)))
	  (f '1))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant 1)
		   (T:fixnum T:positive T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object)))))

  #t)


(parametrise ((check-test-name	'bignums))

  (doit (let ((f (lambda (x) x)))
	  (f '1152921504606846976))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant 1152921504606846976)
		   (T:bignum T:positive T:non-false T:nonimmediate T:exact-real T:real T:exact-integer T:exact T:number T:object)))))

  (doit (let ((f (lambda (x) x)))
	  (f '-1152921504606846977))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant -1152921504606846977)
		   (T:bignum T:negative T:non-false T:nonimmediate T:exact-real T:real T:exact-integer T:exact T:number T:object)))))

  #t)


(parametrise ((check-test-name	'ratnums))

  (doit (let ((f (lambda (x) x)))
	  (f (quote +2/3)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant +2/3)
		   (T:ratnum T:positive T:non-false T:nonimmediate T:exact-real T:real T:exact T:number T:object)))))

  (doit (let ((f (lambda (x) x)))
	  (f (quote -2/3)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant -2/3)
		   (T:ratnum T:negative T:non-false T:nonimmediate T:exact-real T:real T:exact T:number T:object)))))

  #t)


(parametrise ((check-test-name	'flonums))

  (doit (let ((f (lambda (x) x)))
	  (f (quote +2.3)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant +2.3)
		   (T:flonum-fractional T:positive T:non-false T:nonimmediate T:real T:inexact T:flonum T:flonum-finite T:number T:object)))))

  (doit (let ((f (lambda (x) x)))
	  (f (quote -2.3)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant -2.3)
		   (T:flonum-fractional T:negative T:non-false T:nonimmediate T:real T:inexact T:flonum T:flonum-finite T:number T:object)))))

  (doit (let ((f (lambda (x) x)))
	  (f (quote -0.0)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant -0.0)
		   (T:flonum-integer T:zero T:non-false T:nonimmediate T:real T:inexact T:flonum T:flonum-finite T:number T:object)))))

  #t)


(parametrise ((check-test-name	'cflonums))

  (doit (let ((f (lambda (x) x)))
	  (f (quote +2.3+4.5i)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant +2.3+4.5i)
		   (T:cflonum T:non-false T:nonimmediate T:non-real T:inexact T:number T:object)))))

  (doit (let ((f (lambda (x) x)))
	  (f (quote -2.3+4.5i)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant -2.3+4.5i)
		   (T:cflonum T:non-false T:nonimmediate T:non-real T:inexact T:number T:object)))))

  (doit (let ((f (lambda (x) x)))
	  (f (quote -0.0+0.0i)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant -0.0+0.0i)
		   (T:cflonum T:zero T:non-false T:nonimmediate T:non-real T:inexact T:number T:object)))))

  #t)


(parametrise ((check-test-name	'compnums))

  (doit (let ((f (lambda (x) x)))
	  (f (quote +2/3+4/5i)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0 (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant +2/3+4/5i)
		   (T:compnum T:non-false T:nonimmediate T:non-real T:number T:object)))))

  (doit (let ((f (lambda (x) x)))
	  (f (quote -2/3+4/5i)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant -2/3+4/5i)
		   (T:compnum T:non-false T:nonimmediate T:non-real T:number T:object)))))

  (doit (let ((f (lambda (x) x)))
	  (f (quote 0+0.0i)))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant 0+0.0i)
		   (T:compnum T:zero T:non-false T:nonimmediate T:non-real T:number T:object)))))

  #t)


(parametrise ((check-test-name	'strings))

  (doit (let ((f (lambda (x) x)))
	  (f '"1"))
	(bind ((f_0 (lambda (x_0) x_0)))
	  (funcall (known f_0
			  (T:procedure T:non-false T:nonimmediate T:object))
	    (known (constant "1")
		   (T:string T:non-false T:nonimmediate T:object)))))

  #t)


(parametrise ((check-test-name	'pairs))

  ;;Test for NULL?, PAIR? and LIST?.
  (doit* (let ((x (read)))
	   (cond ((null? x)
		  (display x))
		 ((list? x)
		  (display x))
		 ((pair? x)
		  (display x))
		 (else
		  (display x))))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref null?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:null T:non-false T:proper-list T:immediate T:object)))
	     (conditional (funcall (primref list?) x_0)
		 (funcall (primref display)
		   (known x_0 (T:non-false T:proper-list T:object)))
	       (conditional (funcall (primref pair?) x_0)
		   (funcall (primref display)
		     (known x_0 (T:non-false T:nonimmediate T:pair T:object)))
		 (funcall (primref display) x_0))))))

  #t)


(parametrise ((check-test-name	'vectors))

  (doit* (vector-length '#(1 2))
	 (funcall (primref vector-length)
	   (known (constant #(1 2))
		  (T:vector T:non-false T:nonimmediate T:object))))

  #t)


(parametrise ((check-test-name	'conditionals))

  ;;Inference of  type after  successful variable  test: if X  is non-false,  its tag
  ;;contains T:non-false; if X is false, its tag contains T:false.
  (doit* (let ((x (read)))
	   (if x
	       (display x)
	     (display x)))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional x_0
	       (funcall (primref display)
		 (known x_0 (T:non-false T:object)))
	     (funcall (primref display)
	       (known x_0 (T:false T:boolean T:immediate T:object))))))

  ;;Test for NOT in conditional's test position:  if X is non-false, its tag contains
  ;;T:non-false; if X is false, its tag contains T:false.
  (doit* (let ((x (read)))
	   (if (not x)
	       (display x)
	     (display x)))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref not) x_0)
	       (funcall (primref display)
		 (known x_0 (T:false T:boolean T:immediate T:object)))
	     (funcall (primref display)
	       (known x_0 (T:non-false T:object))))))

  ;;Test for AND syntax type propagation.  AND expands to IF.
  (doit* (let ((x (read)))
	   (and (pair? x)
		(car   x)))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref pair?) x_0)
	       (funcall (primref car)
		 (known x_0 (T:non-false T:nonimmediate T:pair T:object)))
	     (constant #f))))

;;; --------------------------------------------------------------------
;;; inference of type after successful type predicate application

  (doit* (let ((x (read)))
	   (if (fixnum? x)
	       (display x)
	     (display x)))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref fixnum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:fixnum T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object)))
	     (funcall (primref display)
	       x_0))))

  (doit* (let ((x (read)))
	   (and (number? x)
		(nan?    x)))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref number?) x_0)
	       (funcall (primref nan?) (known x_0 (T:non-false T:number T:object)))
	     (constant #f))))

  ;;Miscellaneous predicates.
  (doit* (let ((x (read)))
	   (cond ((fixnum? x)
		  (display x))
		 ((string? x)
		  (display x))
		 (else
		  (display x))))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref fixnum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:fixnum T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object)))
	     (conditional (funcall (primref string?) x_0)
		 (funcall (primref display)
		   (known x_0 (T:string T:non-false T:nonimmediate T:object)))
	       (funcall (primref display)
		 x_0)))))

;;; --------------------------------------------------------------------
;;; flonum predicates

  ;;FLONUM?
  (doit* (let ((x (read)))
	   (and (flonum? x)
		(display x)))
	 (bind ((x_0 (funcall (primref read))))
	   (conditional (funcall (primref flonum?) x_0)
	       (funcall (primref display)
		 (known x_0 (T:non-false T:nonimmediate T:real T:inexact T:flonum T:number T:object)))
	     (constant #f))))

  ;;FLINFINITE?
  (doit (let ((x ((primitive read))))
	  (if ((primitive flinfinite?) x)
	      ((primitive display) x)
	    ((primitive display) x)))
	(bind ((x_0 (funcall (primref read))))
	  (conditional (funcall (primref flinfinite?) x_0)
	      (funcall (primref display)
		(known x_0 (T:flonum-infinite T:non-false T:nonimmediate T:real T:inexact T:flonum T:number T:object)))
	    (funcall (primref display)
	      (known x_0 (T:non-false T:nonimmediate T:real T:inexact T:flonum T:number T:object))))))

  #t)


(parametrise ((check-test-name	'arguments))

  ;; propagation of type after successful primitive argument validation

  ;;The following tests are related.  We  test what happens when a variable reference
  ;;is used as operand for CDR; the primitive CDR accepts a pair as operand.
  (begin
    ;;Here  the variable  X  is of  known  type from  the start:  it  is tagged  with
    ;;"T:pair".
    (doit* (let ((f (lambda (y) y))
		 (x '(1 . 2)))
	     (f (cdr x)))
	   (bind ((f_0 (lambda (y_0) y_0))
		  (x_0 (constant (1 . 2))))
	     (funcall (known f_0 (T:procedure T:non-false T:nonimmediate T:object))
	       (funcall (primref cdr)
		 (known x_0 (T:standalone-pair T:non-false T:nonimmediate T:pair T:object))))))

    ;;Here the variable X is of unknown type: it is left untagged.
    (doit* (let ((f (lambda (y) y))
		 (x (read)))
	     (f (cdr x)))
	   (bind ((f_0 (lambda (y_0) y_0))
		  (x_0 (funcall (primref read))))
	     (funcall (known f_0 (T:procedure T:non-false T:nonimmediate T:object))
	       (funcall (primref cdr) x_0))))

    ;;Here the variable X is of unknown type  when its binding is created: it is left
    ;;untagged.   But, after  being  used  as argument  for  CDR  without raising  an
    ;;exception: it is known that its type  is "T:pair"; so the second reference to X
    ;;is tagged with "T:pair".
    ;;
    ;;We know  that a "wrong  operand type" exception  is non-continuable; so  if CDR
    ;;raises an exception because X is not a pair: the control flow cannot come back.
    (doit* (let ((f (lambda (y) y))
		 (x (read)))
	     (f (cdr x))
	     (f x))
	   (bind ((f_0 (lambda (y_0) y_0))
		  (x_0 (funcall (primref read))))
	     (seq
	       (funcall (known f_0 (T:procedure T:non-false T:nonimmediate T:object))
		 (funcall (primref cdr) x_0))
	       (funcall (known f_0 (T:procedure T:non-false T:nonimmediate T:object))
		 (known x_0 (T:non-false T:nonimmediate T:pair T:object))))))

    #| end of BEGIN |# )

  #t)


(parametrise ((check-test-name	'return-values))

  ;;Test for NOT return value type descriptor.  NOT has a T:boolean as return value.
  (doit (let ((x ((primitive not) ((primitive read)))))
	  ((primitive display) x))
	(bind ((x_0 (funcall (primref not)
		      (funcall (primref read)))))
	  (funcall (primref display)
	    (known x_0 (T:boolean T:immediate T:object)))))

  #t)


(parametrise ((check-test-name	'typed-expressions))

;;;Tests  for core  types introduced  by the  code that  generates the  core language
;;;expression.

  (doit (let ((x (typed-expr ((primitive read))
			     T:fixnum)))
	  ((primitive display) x))
	(bind ((x_0 (funcall (primref read))))
	  (funcall (primref display)
	    (known x_0 (T:fixnum T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object)))))

  ;;Here  "+" is  declared to  return a  "T:number"; the  TYPED-EXPR refines  this to
  ;;"T:fixnum".  The core types "T:number" and "T:fixnum" are compatible.
  ;;
  (doit (let ((x (typed-expr ((primitive +) '1 '2)
			     T:fixnum)))
	  ((primitive display) x))
	(bind ((x_0 (funcall (primref +)
		      (known (constant 1)
			     (T:fixnum T:positive T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object))
		      (known (constant 2)
			     (T:fixnum T:positive T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object)))))
	  (funcall (primref display)
	    (known x_0 (T:fixnum T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object)))))

  ;;This correctly  fails with  specified core type  incompatible with  inferred core
  ;;type.
  ;;
  ;; (doit (typed-expr (quote (1 . 2)) T:fixnum) #f)

;;; --------------------------------------------------------------------
;;; typing LAMBDA arguments

  (doit (lambda (a b c)
	  (begin
	    (typed-expr a T:fixnum)
	    (typed-expr b T:symbol)
	    (typed-expr c T:string)
	    ((primitive list) a b c)))
	(lambda (a_0 b_0 c_0)
	  (funcall (primref list)
	    (known a_0 (T:fixnum T:non-false T:exact-real T:real T:exact-integer T:exact T:number T:immediate T:object))
	    (known b_0 (T:symbol T:non-false T:nonimmediate T:object))
	    (known c_0 (T:string T:non-false T:nonimmediate T:object)))))

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
;; eval: (put 'shortcut			'scheme-indent-function 1)
;; End:
