;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the compiler internals
;;;Date: Mon Jul 28, 2014
;;;
;;;Abstract
;;;
;;;	Test the compiler pass "impose evaluation order".
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
(check-display "*** testing Vicare compiler pass: impose evaluation order\n")

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

(define (%before-impose-eval-order core-language-form)
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

;;; --------------------------------------------------------------------

(define-syntax doit
  (syntax-rules ()
    ((_ ?core-language-form ?expected-result)
     (check
	 (%impose-eval-order (quasiquote ?core-language-form))
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


(parametrise ((check-test-name	'clambdas))

  (begin
    ;;Let's see first the previous compiler pass output.
    (check
	(%before-impose-eval-order '(let ((F (lambda (x) ((primitive +) '1 x))))
				      (F '2)))
      => '(codes
	   ((lambda (label: asmlabel:F:clambda) (cp_0 x_0)
	       (shortcut
		   (seq
		     ;;Check if X_0 is a fixnum.
		     (conditional (asmcall =
					   (asmcall logand x_0 (constant 7))
					   (constant 0))
			 (asmcall nop)
		       (asmcall interrupt))
		     ;;Attempt to sum the fixnums.
		     (asmcall int+/overflow (constant 8) x_0))
		 (funcall (asmcall mref (constant (object +)) (constant 19))
		   (constant 8)
		   x_0))))
	   (seq
	     (shortcut
		 (asmcall incr/zero? %esi (constant 72) (constant 8))
	       (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	     (jmpcall asmlabel:F:clambda:case-1
		      (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars))))
			tmp_0)
		      (constant 16)))))

    ;;Let's see then the output of this compiler pass.
    (doit (let ((F (lambda (x) ((primitive +) '1 x))))
	    (F '2))
	  (codes
	   ((lambda (label: asmlabel:F:clambda) (%edi fvar.1)
	       (locals
		;;The list  of machine  words we  need in this  function to  hold the
		;;temporary local values.  They will be CPU registers or Scheme stack
		;;words.
		(local-vars: tmp_0 tmp_1 tmp_2 tmp_3 cp_0)
		(seq
		  ;;Load the  reference to closure  object from the  CP-REGISTER into
		  ;;CP_0.
		  (asm-instr move cp_0 %edi)
		  (shortcut
		      ;;This is the  integrated body of the  core primitive operation
		      ;;"+".
		      (seq
			;;Check  if the  argument in  the Scheme  stack machine  word
			;;FVAR.1 is a fixnum.
			(conditional (seq
				       (asm-instr move tmp_0 fvar.1)
				       (asm-instr logand tmp_0 (constant 7))
				       (asm-instr = tmp_0 (constant 0)))
			    (asmcall nop)
			  (asmcall interrupt))
			;;Attempt to sum the fixnums.
			(asm-instr move tmp_1 (constant 8))
			(asm-instr int+/overflow tmp_1 fvar.1)
			;;If successful: return the result in AA-REGISTER.
			(asm-instr move %eax tmp_1)
			(asmcall return %esi %esp %ebp %eax))
		    ;;The sum  between fixnums  failed: tail-call the  core primitive
		    ;;function "+".
		    (seq
		      ;;To  prepare for  the  tail call  we need  to  remove the  old
		      ;;operands from the  Scheme stack: we need  the location FVAR.1
		      ;;for the tail call's operands.
		      (asm-instr move tmp_3 fvar.1)
		      ;;Load the reference to the function "+" from the loc gensym in
		      ;;the relocation vector.
		      (asm-instr move tmp_2 (disp (constant (object +))
						  (constant 19)))
		      ;;Store the reference to function "+" in the CP-REGISTER.
		      (asm-instr move %edi tmp_2)
		      ;;Put the operands of "+" on the Scheme stack.
		      (asm-instr move fvar.1 (constant 8))
		      (asm-instr move fvar.2 tmp_3)
		      ;;Load the negated number of operands in the AA-REGISTER.
		      (asm-instr move %eax (constant -16))
		      ;;Perform the  tail-call as indirect  jump: the address  of the
		      ;;entry  point of  the function  "+" is  in the  closure object
		      ;;referenced by CP-REGISTER.
		      (asmcall indirect-jump %eax %esi %esp %ebp %edi fvar.1 fvar.2)))))))
	   (locals
	    ;;The  list of  machine words  we  need in  this expression  to hold  the
	    ;;temporary local  values.  They  will be CPU  registers or  Scheme stack
	    ;;words.
	    (local-vars: tmp_4 tmp_5 tmp_6)
	    (seq
	      (shortcut
		  ;;This  is the  integrated  body of  the  core primitive  operation
		  ;;"$do-event": check if the PCB's engine counter has been set.
		  (asmcall incr/zero? %esi (constant 72) (constant 8))
		(nframe
		 (vars: #f)
		 (live: #f)
		 (seq
		   ;;Load  the reference  to the  function "$do-event"  from the  loc
		   ;;gensym in the relocation vector.
		   (asm-instr move tmp_4 (disp (constant (object $do-event))
					       (constant 19)))
		   ;;Store the reference to function "$do-event" in the CP-REGISTER.
		   (asm-instr move %edi tmp_4)
		   ;;Load the  negated number of  arguments in the  AA-REGISTER; zero
		   ;;for "$do-event".
		   (asm-instr move %eax (constant 0))
		   (ntcall #f #f))))
	      ;;Retrieve from  the relocation vector  of the code object  the closure
	      ;;object of the combinator F.
	      (asm-instr move tmp_5 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
	      (asm-instr move tmp_6 tmp_5)
	      ;;Store a reference to the combinator F in the CP-REGISTER.
	      (asm-instr move %edi tmp_6)
	      ;;Put on the Scheme stack the operand of F.
	      (asm-instr move fvar.1 (constant 16))
	      ;;Load the negated number of arguments in the AA-REGISTER; -1 for F.
	      (asm-instr move %eax (constant -8))
	      ;;Tail-call the operator F.
	      (asmcall direct-jump
		       (code-loc asmlabel:F:clambda:case-1)
		       %eax %esi %esp %ebp %edi fvar.1)))))
    #| end of BEGIN |# )

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
