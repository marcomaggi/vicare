;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the compiler internals
;;;Date: Thu Oct  9, 2014
;;;
;;;Abstract
;;;
;;;	Test the compiler pass "assign frame sizes".
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
  (prefix (vicare compiler)
	  compiler.))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare compiler pass: assign frame sizes\n")

(compiler.descriptive-labels   #t)
(compiler.generate-debug-calls #f)


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
	       '(vicare unsafe operations)))

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
  (let* ((D (compiler.recordize core-language-form))
	 (D (compiler.optimize-direct-calls D))
	 (D (compiler.optimize-letrec D))
	 ;;Source optimisation is skipped here to  make it easier to write meaningful
	 ;;code for debugging and inspection.
	 #;(D (compiler.source-optimize D))
	 (D (compiler.rewrite-references-and-assignments D))
	 (D (compiler.core-type-inference D))
	 (D (compiler.sanitize-bindings D))
	 (D (compiler.optimize-for-direct-jumps D))
	 (D (compiler.insert-global-assignments D))
	 (D (compiler.introduce-vars D))
	 (D (compiler.introduce-closure-makers D))
	 (D (compiler.optimize-combinator-calls/lift-clambdas D))
	 (D (compiler.introduce-primitive-operation-calls D))
	 (D (compiler.rewrite-freevar-references D))
	 (D (compiler.insert-engine-checks D))
	 (D (compiler.insert-stack-overflow-check D))
	 (D (compiler.specify-representation D))
	 (S (compiler.unparse-recordized-code/sexp D)))
    S))

(define (%impose-eval-order core-language-form)
  (let* ((D (compiler.recordize core-language-form))
	 (D (compiler.optimize-direct-calls D))
	 (D (compiler.optimize-letrec D))
	 ;;Source optimisation is skipped here to  make it easier to write meaningful
	 ;;code for debugging and inspection.
	 #;(D (compiler.source-optimize D))
	 (D (compiler.rewrite-references-and-assignments D))
	 (D (compiler.core-type-inference D))
	 (D (compiler.sanitize-bindings D))
	 (D (compiler.optimize-for-direct-jumps D))
	 (D (compiler.insert-global-assignments D))
	 (D (compiler.introduce-vars D))
	 (D (compiler.introduce-closure-makers D))
	 (D (compiler.optimize-combinator-calls/lift-clambdas D))
	 (D (compiler.introduce-primitive-operation-calls D))
	 (D (compiler.rewrite-freevar-references D))
	 (D (compiler.insert-engine-checks D))
	 (D (compiler.insert-stack-overflow-check D))
	 (D (compiler.specify-representation D))
	 (D (compiler.impose-calling-convention/evaluation-order D))
	 (S (compiler.unparse-recordized-code/sexp D)))
    S))

(define (%assign-frame-sizes core-language-form)
  (let* ((D (compiler.recordize core-language-form))
	 (D (compiler.optimize-direct-calls D))
	 (D (compiler.optimize-letrec D))
	 ;;Source optimisation is skipped here to  make it easier to write meaningful
	 ;;code for debugging and inspection.
	 #;(D (compiler.source-optimize D))
	 (D (compiler.rewrite-references-and-assignments D))
	 (D (compiler.core-type-inference D))
	 (D (compiler.sanitize-bindings D))
	 (D (compiler.optimize-for-direct-jumps D))
	 (D (compiler.insert-global-assignments D))
	 (D (compiler.introduce-vars D))
	 (D (compiler.introduce-closure-makers D))
	 (D (compiler.optimize-combinator-calls/lift-clambdas D))
	 (D (compiler.introduce-primitive-operation-calls D))
	 (D (compiler.rewrite-freevar-references D))
	 (D (compiler.insert-engine-checks D))
	 (D (compiler.insert-stack-overflow-check D))
	 (D (compiler.specify-representation D))
	 (D (compiler.impose-calling-convention/evaluation-order D))
	 (D (compiler.assign-frame-sizes D))
	 (S (compiler.unparse-recordized-code/sexp D)))
    S))

;;; --------------------------------------------------------------------

(define-syntax doit
  (syntax-rules ()
    ((_ ?core-language-form ?expected-result)
     (check
	 (%assign-frame-sizes (quasiquote ?core-language-form))
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


(parametrise ((check-test-name	'nested-non-tail-calls))

;;;We want to show  what happens when we perform a non-tail  call while preparing the
;;;stack operands for a non-tail call.

  (check
      (%specify-representation '(let ((f (lambda (x) (+ '1 x)))
				      (g (lambda (y) (+ '2 y))))
				  (begin
				    (f (g '3))
				    '4)))
    => '(codes
	 ((lambda (label: asmlabel:g:clambda) (cp_0 y_0)
	     (seq
	       ;;Core primitive operation $DO-EVENT.
	       (shortcut
		   (asmcall incr/zero? %esi (constant 72) (constant 8))
		 (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	       ;;Tail call to the core primitive function "+".
	       (funcall (asmcall mref (constant (object +)) (constant 27))
		 (constant 16) y_0)))
	  (lambda (label: asmlabel:f:clambda) (cp_1 x_0)
	     (seq
	       ;;Core primitive operation $DO-EVENT.
	       (shortcut
		   (asmcall incr/zero? %esi (constant 72) (constant 8))
		 (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	       ;;Tail call to the core primitive function "+".
	       (funcall (asmcall mref (constant (object +)) (constant 27))
		 (constant 8) x_0))))
	 (seq
	   ;;Core primitive operation $STACK-OVERFLOW-CHECK.
	   (shortcut
	       (conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		   (asmcall interrupt)
		 (asmcall nop))
	     (foreign-call "ik_stack_overflow"))
	   ;;Core primitive operation $DO-EVENT.
	   (shortcut
	       (asmcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	   (jmpcall asmlabel:f:clambda:case-1
		    (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:f:clambda) no-freevars))))
		      tmp_0)
		    (jmpcall asmlabel:g:clambda:case-1
			     (bind ((tmp_1 (constant (closure-maker (code-loc asmlabel:g:clambda) no-freevars))))
			       tmp_1)
			     (constant 24)))
	   (constant 32))))

;;; --------------------------------------------------------------------

  (check
      (%impose-eval-order '(let ((f (lambda (x) (+ '1 x)))
				 (g (lambda (y) (+ '2 y))))
			     (begin
			       (f (g '3))
			       '4)))
    => '(codes
	 ((lambda (label: asmlabel:g:clambda) (%edi fvar.1)
	     (locals
	      (local-vars: tmp_0 tmp_1 tmp_2 cp_0)
	      (seq
		(asm-instr move cp_0 %edi)
		;;Core primitive operation $DO-EVENT.
		(shortcut (asmcall incr/zero? %esi (constant 72) (constant 8))
		  (non-tail-call-frame
		    (rand*: #f)
		    (live: #f)
		    (seq
		      (asm-instr move tmp_0 (disp (constant (object $do-event)) (constant 19)))
		      (asm-instr move %edi tmp_0)
		      (asm-instr move %eax (constant 0))
		      (non-tail-call
			(target: #f)
			(retval-var: #f)
			(all-rand*: %eax %ebp %edi %esp %esi)
			(mask: #f)
			(size: #f)))))
		;;Tail call to the core primitive function "+".
		(asm-instr move tmp_1 fvar.1)
		(asm-instr move tmp_2 (disp (constant (object +)) (constant 27)))
		(asm-instr move fvar.1 (constant 16))
		(asm-instr move fvar.2 tmp_1)
		(asm-instr move %edi tmp_2)
		(asm-instr move %eax (constant -16))
		(asmcall indirect-jump %eax %ebp %edi %esp %esi fvar.1 fvar.2))))
	  (lambda (label: asmlabel:f:clambda) (%edi fvar.1)
	     (locals
	      (local-vars: tmp_3 tmp_4 tmp_5 cp_1)
	      (seq
		(asm-instr move cp_1 %edi)
		;;Core primitive operation $DO-EVENT.
		(shortcut
		    (asmcall incr/zero? %esi (constant 72) (constant 8))
		  (non-tail-call-frame
		    (rand*: #f)
		    (live: #f)
		    (seq
		      (asm-instr move tmp_3 (disp (constant (object $do-event)) (constant 19)))
		      (asm-instr move %edi tmp_3)
		      (asm-instr move %eax (constant 0))
		      (non-tail-call
			(target: #f)
			(retval-var: #f)
			(all-rand*: %eax %ebp %edi %esp %esi)
			(mask: #f)
			(size: #f)))))
		;;Tail call to the core primitive function "+".
		(asm-instr move tmp_4 fvar.1)
		(asm-instr move tmp_5 (disp (constant (object +)) (constant 27)))
		(asm-instr move fvar.1 (constant 8))
		(asm-instr move fvar.2 tmp_4)
		(asm-instr move %edi tmp_5)
		(asm-instr move %eax (constant -16))
		(asmcall indirect-jump %eax %ebp %edi %esp %esi fvar.1 fvar.2)))))
	 (locals
	  (local-vars: tmp_6 tmp_7 tmp_8 tmp_9 tmp_10)
	  (seq
	    ;;Core primitive operation $STACK-OVERFLOW-CHECK.
	    (shortcut
		(conditional (asm-instr u< %esp (disp %esi (constant 32)))
		    (asmcall interrupt)
		  (asmcall nop))
	      (non-tail-call-frame (rand*: #f) (live: #f)
				   (seq
				     (asm-instr move %edi
						(constant (foreign-label "ik_stack_overflow")))
				     (asm-instr move %eax (constant 0))
				     (non-tail-call (target: "ik_stack_overflow")
						    (retval-var: #f)
						    (all-rand*: %eax %ebp %edi %esp %esi)
						    (mask: #f) (size: #f)))))
	    ;;Core primitive operation $DO-EVENT.
	    (shortcut
		(asmcall incr/zero? %esi (constant 72) (constant 8))
	      (non-tail-call-frame
		(rand*: #f)
		(live: #f)
		(seq
		  (asm-instr move tmp_6 (disp (constant (object $do-event)) (constant 19)))
		  (asm-instr move %edi tmp_6)
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target: #f)
		    (retval-var: #f)
		    (all-rand*: %eax %ebp %edi %esp %esi)
		    (mask: #f)
		    (size: #f)))))
	    ;;Non-tail call frame to the function F.
	    (non-tail-call-frame
	      (rand*: nfv.1_0)
	      (live: #f)
	      (seq
		;;Non-tail call frame to the function G.
		(non-tail-call-frame
		  (rand*: nfv.1_1)
		  (live: #f)
		  (seq
		    (asm-instr move nfv.1_1 (constant 24))
		    (asm-instr move tmp_7 (constant (closure-maker (code-loc asmlabel:g:clambda) no-freevars)))
		    (asm-instr move tmp_8 tmp_7)
		    (asm-instr move %edi tmp_8)
		    (asm-instr move %eax (constant -8))
		    (non-tail-call
		      (target: asmlabel:g:clambda:case-1)
		      (retval-var: nfv.1_0)
		      (all-rand*: %eax %ebp %edi %esp %esi nfv.1_1)
		      (mask: #f)
		      (size: #f))))
		(asm-instr move nfv.1_0 %eax)
		(asm-instr move tmp_9 (constant (closure-maker (code-loc asmlabel:f:clambda) no-freevars)))
		(asm-instr move tmp_10 tmp_9)
		(asm-instr move %edi tmp_10)
		(asm-instr move %eax (constant -8))
		(non-tail-call
		  (target: asmlabel:f:clambda:case-1)
		  (retval-var: #f)
		  (all-rand*: %eax %ebp %edi %esp %esi nfv.1_0)
		  (mask: #f)
		  (size: #f))))
	    (asm-instr move %eax (constant 32))
	    (asmcall return %eax %ebp %esp %esi)))))

;;; --------------------------------------------------------------------

  (doit (let ((f (lambda (x) (+ '1 x)))
	      (g (lambda (y) (+ '2 y))))
	  (begin
	    (f (g '3))
	    '4))
	(codes
	 ((lambda (label: asmlabel:g:clambda) (%edi fvar.1)
	     (locals
	      (local-vars: #(tmp_0 tmp_1 tmp_2 cp_0)
			   tmp_0 tmp_1 tmp_2 cp_0)
	      (seq
		(asmcall nop)
		(shortcut
		    (asmcall incr/zero? %esi (constant 72) (constant 8))
		  (seq
		    (asm-instr move tmp_0 (disp (constant (object $do-event)) (constant 19)))
		    (asm-instr move %edi tmp_0)
		    (asm-instr move %eax (constant 0))
		    (non-tail-call
		      (target: #f)
		      (retval-var: #f)
		      (all-rand*: %eax %ebp %edi %esp %esi)
		      (mask: #(2))
		      (size: 2))))
		(asm-instr move tmp_1 fvar.1)
		(asm-instr move tmp_2 (disp (constant (object +)) (constant 27)))
		(asm-instr move fvar.1 (constant 16))
		(asm-instr move fvar.2 tmp_1)
		(asm-instr move %edi tmp_2)
		(asm-instr move %eax (constant -16))
		(asmcall indirect-jump %eax %ebp %edi %esp %esi fvar.1 fvar.2))))
	  (lambda (label: asmlabel:f:clambda) (%edi fvar.1)
	    (locals
	     (local-vars: #(tmp_3 tmp_4 tmp_5 cp_1)
			  tmp_3 tmp_4 tmp_5 cp_1)
	     (seq
	       (asmcall nop)
	       (shortcut
		   (asmcall incr/zero? %esi (constant 72) (constant 8))
		 (seq
		   (asm-instr move tmp_3 (disp (constant (object $do-event)) (constant 19)))
		   (asm-instr move %edi tmp_3)
		   (asm-instr move %eax (constant 0))
		   (non-tail-call
		     (target: #f)
		     (retval-var: #f)
		     (all-rand*: %eax %ebp %edi %esp %esi)
		     (mask: #(2))
		     (size: 2))))
	       (asm-instr move tmp_4 fvar.1)
	       (asm-instr move tmp_5 (disp (constant (object +)) (constant 27)))
	       (asm-instr move fvar.1 (constant 8))
	       (asm-instr move fvar.2 tmp_4)
	       (asm-instr move %edi tmp_5)
	       (asm-instr move %eax (constant -16))
	       (asmcall indirect-jump %eax %ebp %edi %esp %esi fvar.1 fvar.2)))))
	 (locals
	  (local-vars: #(tmp_6 tmp_7 tmp_8 tmp_9 tmp_10)
		       tmp_6 tmp_7 tmp_8 tmp_9 tmp_10)
	  (seq
	    (shortcut
		(conditional (asm-instr u< %esp (disp %esi (constant 32)))
		    (asmcall interrupt)
		  (asmcall nop))
	      (seq
		(asm-instr move %edi (constant (foreign-label "ik_stack_overflow")))
		(asm-instr move %eax (constant 0))
		(non-tail-call
		  (target: "ik_stack_overflow")
		  (retval-var: #f)
		  (all-rand*: %eax %ebp %edi %esp %esi)
		  (mask: #(0))
		  (size: 1))))
	    (shortcut
		(asmcall incr/zero? %esi (constant 72) (constant 8))
	      (seq
		(asm-instr move tmp_6 (disp (constant (object $do-event)) (constant 19)))
		(asm-instr move %edi tmp_6)
		(asm-instr move %eax (constant 0))
		(non-tail-call
		  (target: #f)
		  (retval-var: #f)
		  (all-rand*: %eax %ebp %edi %esp %esi)
		  (mask: #(0))
		  (size: 1))))
	    (asm-instr move fvar.2 (constant 24))
	    (asm-instr move tmp_7 (constant (closure-maker (code-loc asmlabel:g:clambda) no-freevars)))
	    (asm-instr move tmp_8 tmp_7)
	    (asm-instr move %edi tmp_8)
	    (asm-instr move %eax (constant -8))
	    (non-tail-call
	      (target: asmlabel:g:clambda:case-1)
	      (retval-var: (nfv.1_0 . fvar.2))
	      (all-rand*: %eax %ebp %edi %esp %esi fvar.2)
	      (mask: #(0))
	      (size: 1))
	    (asm-instr move fvar.2 %eax)
	    (asm-instr move tmp_9 (constant (closure-maker (code-loc asmlabel:f:clambda) no-freevars)))
	    (asm-instr move tmp_10 tmp_9)
	    (asm-instr move %edi tmp_10)
	    (asm-instr move %eax (constant -8))
	    (non-tail-call
	      (target: asmlabel:f:clambda:case-1)
	      (retval-var: #f)
	      (all-rand*: %eax %ebp %edi %esp %esi fvar.2)
	      (mask: #(0))
	      (size: 1))
	    (asm-instr move %eax (constant 32))
	    (asmcall return %eax %ebp %esp %esi)))))

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
;; eval: (put 'non-tail-call		'scheme-indent-function 0)
;; eval: (put 'non-tail-call-frame	'scheme-indent-function 0)
;; End:
