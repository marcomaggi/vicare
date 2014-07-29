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


(parametrise ((check-test-name			'optimize-letrec-waddell)
	      (compiler.$current-letrec-pass	'waddell))

  (define-syntax-rule (doit ?core-language-form ?expected-result)
    (check
	(let* ((D (compiler.$recordize (quasiquote ?core-language-form)))
	       (D (compiler.$optimize-direct-calls D))
	       (D (compiler.$optimize-letrec D))
	       (S (compiler.$unparse-recordized-code/pretty D)))
	  S)
      => (quasiquote ?expected-result)))

  (define-syntax-rule (doit* ?standard-language-form ?expected-result)
    (doit ,(receive (code libs)
	       (expand-form-to-core-language (quasiquote ?standard-language-form)
					     (environment '(rnrs)))
	     code)
	  ?expected-result))

;;; --------------------------------------------------------------------

  (doit (letrec ((a (quote 1)))
	  a)
	(let ()
	  (let ((a_0 (quote #!void)))
	    (fix ()
	      (let ((a_1 (quote 1)))
		(begin
		  (set! a_0 a_1)
		  a_0))))))

  (doit (letrec ((a (lambda () (quote 1))))
	  a)
	(let ()
	  (let ()
	    (fix ((a_0 (lambda () (quote 1))))
	      (let ()
		a_0)))))

  (doit* (let ((a (quote 1)))
	   (let ((a a))
	     a))
	 (let* ((a_0 '1)
		(a_1 a_0))
	   a_1))

  (doit* (let ((a '1))
	   (let ((a '2))
	     (let ((a '3))
	       a)))
	 (let* ((a_0 '1)
		(a_1 '2)
		(a_2 '3))
	   a_2))

  (doit* (letrec ((a '1)
		  (b '2))
	   (list a b))
	 (let ()
	   (let ((a_0 '#!void)
		 (b_0 '#!void))
	     (fix ()
	       (let ((a_1 '1)
		     (b_1 '2))
		 (begin
		   (set! a_0 a_1)
		   (set! b_0 b_1)
		   (list a_0 b_0)))))))

  (doit* (letrec* ((a (lambda (x)
			(when x
			  (a '#f))))
		   (b '123)
		   (c '456)
		   (d (begin
			(set! c '789)
			'9)))
	   a)
	 (let ()
	   (let ((b_0 '#!void)
		 (c_0 '#!void)
		 (d_0 '#!void))
	     (fix ((a_0 (lambda (x_0)
			  (if x_0
			      (a_0 '#f)
			    (void)))))
	       (begin
		 (set! b_0 '123)
		 (set! c_0 '456)
		 (set! d_0 (begin
			     (set! c_0 '789)
			     '9))
		 a_0)))))

  (doit* (letrec* ((a '123)
		   (b '2)
		   (c b)
		   (d (lambda () '123)))
	   b)
	 (let ()
	   (let ((a_0 '#!void)
		 (b_0 '#!void)
		 (c_0 '#!void))
	     (fix ((d_0 (lambda () '123)))
	       (begin
		 (set! a_0 '123)
		 (set! b_0 '2)
		 (set! c_0 b_0)
		 b_0)))))

  (doit* (letrec* ((a '123)
		   (b '2)
		   (c b)
		   (d (lambda () '123)))
	   (set! d '123)
	   b)
	 (let ()
	   (let ((a_0 '#!void)
		 (b_0 '#!void)
		 (c_0 '#!void)
		 (d_0 '#!void))
	     (fix ()
	       (begin
		 (set! a_0 '123)
		 (set! b_0 '2)
		 (set! c_0 b_0)
		 (set! d_0 (lambda () '123))
		 (set! d_0 '123)
		 b_0)))))

  #t)


(parametrise ((check-test-name			'optimize-letrec-waddell)
	      (compiler.$current-letrec-pass	'scc))

  (define-syntax-rule (doit ?core-language-form ?expected-result)
    (check
	(let* ((D (compiler.$recordize ?core-language-form))
	       (D (compiler.$optimize-direct-calls D))
	       (D (compiler.$optimize-letrec D))
	       (S (compiler.$unparse-recordized-code/pretty D)))
	  S)
      => ?expected-result))

;;; --------------------------------------------------------------------

  (doit '(letrec ((a (quote 1)))
	   a)
	'(let ((a_0 (quote 1)))
	   a_0))

  (doit '(letrec ((a (lambda () (quote 1))))
	   a)
	'(fix ((a_0 (lambda () (quote 1))))
	   a_0))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'fix 'scheme-indent-function 1)
;; End:
