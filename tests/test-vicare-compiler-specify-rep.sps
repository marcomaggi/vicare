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
(check-display "*** testing Vicare compiler pass: specify representation\n")

(compiler.optimize-level 2)
(compiler.$source-optimizer-passes-count 2)
;;(compiler.$cp0-effort-limit 50)
;;(compiler.$cp0-size-limit   8)
(compiler.$descriptive-labels #t)


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

;;; --------------------------------------------------------------------

(define (%specify-representation core-language-form)
  (let* ((D (compiler.$recordize core-language-form))
	 (D (compiler.$optimize-direct-calls D))
	 (D (compiler.$optimize-letrec D))
	 ;;Source optimisation is skipped here to  make it easier to write meaningful
	 ;;code for debugging and inspection.
	 #;(D (compiler.$source-optimize D))
	 (D (compiler.$rewrite-references-and-assignments D))
	 (D (compiler.$sanitize-bindings D))
	 (D (compiler.$optimize-for-direct-jumps D))
	 (D (compiler.$insert-global-assignments D))
	 (D (compiler.$introduce-vars D))
	 (D (compiler.$introduce-closure-makers D))
	 (D (compiler.$optimize-combinator-calls/lift-clambdas D))
	 (D (compiler.$introduce-primcalls D))
	 (D (compiler.$rewrite-freevar-references D))
	 (D (compiler.$insert-engine-checks D))
	 (D (compiler.$insert-stack-overflow-check D))
	 (D (compiler.$specify-representation D))
	 (S (compiler.$unparse-recordized-code/sexp D)))
    S))

(define-syntax doit
  (syntax-rules ()
    ((_ ?core-language-form ?expected-result)
     (check
	 (%specify-representation (quasiquote ?core-language-form))
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
  (syntax-rules ()
    ((_ ?standard-language-form ?expected-result/basic)
     (doit ,(%expand-library (quasiquote ?standard-language-form)) ?expected-result/basic))
    ))


(parametrise ((check-test-name	'arithmetics))

  ;;The  arguments of  +  are known  fixnums:  first attempt  a  sub between  fixnums
  ;;yielding  a fixnum;  if  an overflow  occurs,  resort to  a call  to  the full  +
  ;;primitive function.  The  primitive function + is accessed by  retrieving it from
  ;;its loc gensym.
  ;;
  ;;19 is  the offset of the  VALUE slot in the  loc gensym, taking into  account the
  ;;type tag in the tagged pointer.
  ;;
  ;;8 and 16 are, respectively, the machine word representations of the fixnums 1 and
  ;;2 on 64-bit platforms.
  (doit ((primitive +) '1 '2)
	(codes
	 ()
	 (shortcut
	     (seq
	       (primcall nop)
	       (primcall int+/overflow (constant 8) (constant 16)))
	   (funcall (primcall mref (constant (object +)) (constant 19))
	     (constant 8) (constant 16)))))

  ;;Notice how the return value of the SHORTCUT becomes the operand of DISPLAY.
  (doit* (display (+ 1 2))
	 (codes
	  ()
	  (funcall (primcall mref (constant (object display)) (constant 19))
	    (shortcut
		(seq
		  (primcall nop)
		  (primcall int+/overflow (constant 8) (constant 16)))
	      (funcall (primcall mref (constant (object +)) (constant 19))
		(constant 8) (constant 16))))))

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
