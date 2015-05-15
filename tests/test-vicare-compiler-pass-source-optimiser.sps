;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for the source code optimiser
;;;Date: Mon Jul 14, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (vicare checks)
  (prefix (only (vicare expander)
		expand-form-to-core-language
		generate-descriptive-gensyms?)
	  expander.)
  (only (vicare libraries)
	expand-library->sexp)
  (prefix (vicare compiler)
	  compiler.)
  (prefix (vicare platform words) words.)
  (vicare unsafe operations))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare compiler: source optimiser pass\n")

(begin-for-syntax
  (expander.generate-descriptive-gensyms? #t))
(compiler.generate-descriptive-labels?	#t)
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
      (expander.expand-form-to-core-language standard-language-form THE-ENVIRONMENT)
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

(define (%source-optimize core-language-form)
  (let* ((D (compiler.pass-recordize core-language-form))
	 (D (compiler.pass-optimize-direct-calls D))
	 (D (compiler.pass-optimize-letrec D))
	 (D (compiler.pass-source-optimize D))
	 (S (compiler.unparse-recordized-code/sexp D)))
    S))

(define-syntax doit
  (syntax-rules ()
    ((_ ?core-language-form ?expected-result)
     (check
	 (%source-optimize (quasiquote ?core-language-form))
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


(parametrise ((check-test-name	'variable-references))

  (check
      (optimisation-of
       (let ((x (read)))
	 ;;A variable reference evaluated for side effects only is removed.
	 x
	 (write x)))
    => '(let ((lex.x_0 (read)))
	  (write lex.x_0)))

  (check
      (optimisation-of
       (let ((x (read)))
	 ;;A variable reference evaluated for side effects only is removed.
	 x x x x
	 (write x)))
    => '(let ((lex.x_0 (read)))
	  (write lex.x_0)))

;;; --------------------------------------------------------------------

  (check
      (optimisation-of
       (let ((x (read)))
	 ;;An assigment to a variable that is never referenced is useless.
	 (set! x 1)
	 123))
    => '(begin
	  (read)
	  (quote 123)))

  (check
      (optimisation-of
       (let ((x (read)))
	 ;;An assigment to a variable that is never referenced is useless.
	 (set! x 1)
	 (set! x 1)
	 (set! x 1)
	 (set! x 1)
	 123))
    => '(begin
	  (read)
	  (quote 123)))

;;; --------------------------------------------------------------------

  (check
      (optimisation-of
       (let ((x (read)))
	 ;;A variable reference evaluated for side effects only is removed.
	 x
	 ;;An assigment to a variable that is never referenced is useless.
	 (set! x 1)
	 123))
    => '(begin
	  (read)
	  (quote 123)))

  (check
      (optimisation-of
       (let ((x (read)))
	 ;;A variable reference evaluated for side effects only is removed.
	 x x x x
	 ;;An assigment to a variable that is never referenced is useless.
	 (set! x 1)
	 123))
    => '(begin
	  (read)
	  (quote 123)))

  (check
      (optimisation-of
       (let ((x (read)))
	 ;;An assigment to a variable that is never referenced is useless.
	 (set! x 1)
	 ;;A variable reference evaluated for side effects only is removed.
	 x
	 123))
    => '(begin
	  (read)
	  (quote 123)))

  (check
      (optimisation-of
       (let ((x (read)))
	 ;;An assigment to a variable that is never referenced is useless.
	 (set! x 1)
	 ;;A variable reference evaluated for side effects only is removed.
	 x x x x
	 123))
    => '(begin
	  (read)
	  (quote 123)))

  #t)


(parametrise ((check-test-name	'fixnums))

  (check
      (optimisation-of (greatest-fixnum))
    => (words.case-word-size
	((32)		'(quote +536870911))
	((64)		'(quote +1152921504606846975))))

  (check
      (optimisation-of (least-fixnum))
    => (words.case-word-size
	((32)		'(quote -536870912))
	((64)		'(quote -1152921504606846976))))

  (doit ((primitive greatest-fixnum))
	,(words.case-word-size
	  ((32)		'(constant +536870911))
	  ((64)		'(constant +1152921504606846975))))

  (doit ((primitive least-fixnum))
	,(words.case-word-size
	  ((32)		'(constant -536870912))
	  ((64)		'(constant -1152921504606846976))))

  (doit ((primitive fixnum-width))
	,(words.case-word-size
	  ((32)		'(constant 20))
	  ((64)		'(constant 61))))

;;; --------------------------------------------------------------------

  (check
      (optimisation-of (fx+ 1 2))
    => '(quote 3))

  ;;Not precomputed because it is an overflow error.
  (check
      (optimisation-of (fx+ 1 (greatest-fixnum)))
    => `(fx+ '1 ',(greatest-fixnum)))

  ;;Not precomputed because it is an overflow error.
  (check
      (optimisation-of (fx+ -1 (least-fixnum)))
    => `(fx+ '-1 ',(least-fixnum)))

;;; --------------------------------------------------------------------

  (check
      (optimisation-of ($fx+ 1 2))
    => '(quote 3))

;;; --------------------------------------------------------------------

  (check
      (optimisation-of (fx- 1 2))
    => '(quote -1))

;;; --------------------------------------------------------------------

  (check
      (optimisation-of ($fx- 1 2))
    => '(quote -1))

;;; --------------------------------------------------------------------

  (check
      (optimisation-of (fx* 11 22))
    => '(quote 242))

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
