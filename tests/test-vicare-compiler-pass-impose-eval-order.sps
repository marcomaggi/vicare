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
(check-display "*** testing Vicare compiler pass: impose evaluation order\n")

(compiler.generate-descriptive-labels?   #t)
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


(parametrise ((check-test-name	'tail-calling-clambda-combinator))

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

;;; --------------------------------------------------------------------

  ;;Let's see then the output of this compiler pass.
  (doit (let ((F (lambda (x) ((primitive +) '1 x))))
	  (F '2))
	(codes
	 ((lambda (label: asmlabel:F:clambda) (%edi fvar.1)
	     (locals
	      (local-vars: tmp_0 tmp_1 tmp_2 tmp_3 cp_0)
	      (seq
		;;Load in CP_0 the reference to closure object from the CP-REGISTER.
		(asm-instr move cp_0 %edi)
		;;This shortcut is the implementation of the core primitive operation
		;;"+".
		(shortcut
		    (seq
		      ;;Check if the argument FVAR.1 is a fixnum.
		      (conditional (seq (asm-instr move tmp_0 fvar.1)
					(asm-instr logand tmp_0 (constant 7))
					(asm-instr = tmp_0 (constant 0)))
			  (asmcall nop)
			(asmcall interrupt))
		      ;;Load the constant operand in a temporary location.
		      (asm-instr move tmp_1 (constant 8))
		      ;;Attempt to sum the fixnums.
		      (asm-instr int+/overflow tmp_1 fvar.1)
		      ;;Load in AA-REGISTER the result.
		      (asm-instr move %eax tmp_1)
		      ;;Return to the caller.
		      (asmcall return %eax %ebp %esp %esi))
		  ;;This is the interrupt handler: a  full call to the closure object
		  ;;implementing "+".
		  (seq
		    ;;Move the old operand in a temporary location.
		    (asm-instr move tmp_2 fvar.1)
		    ;;Load in a  temporary location, from the  relocation vector, the
		    ;;reference to closure object.
		    (asm-instr move tmp_3 (disp (constant (object +)) (constant 19)))
		    ;;Stack operands: load them in the stack slots.
		    (asm-instr move fvar.1 (constant 8))
		    (asm-instr move fvar.2 tmp_2)
		    ;;Register operand: load in  CP-REGISTER the reference to closure
		    ;;object.
		    (asm-instr move %edi tmp_3)
		    ;;Register operand: load in AA-REGISTER a fixnum representing the
		    ;;negated number of operands, -2.
		    (asm-instr move %eax (constant -16))
		    ;;Tail-call the  closure object.   We jump  directly to  the code
		    ;;object's entry  point, retrieving the address  from the closure
		    ;;object in CP-REGISTER.
		    (asmcall indirect-jump
			     %eax %ebp %edi %esp %esi fvar.1 fvar.2)))))))
	 (locals
	  (local-vars: tmp_4 tmp_5 tmp_6)
	  (seq
	    ;;This SHORTCUT  is the  implementation of  the core  primitive operation
	    ;;$DO-EVENT.
	    (shortcut
		;;Check if the PCB engine counter is set.
		(asmcall incr/zero? %esi (constant 72) (constant 8))
	      (non-tail-call-frame
		(rand*: #f)
		(live: #f)
		(seq
		  ;;Load in  a temporary  location, from  the relocation  vector, the
		  ;;reference to closure object implementing $DO-EVENT.
		  (asm-instr move tmp_4 (disp (constant (object $do-event)) (constant 19)))
		  ;;Register parameter: load in  CP-REGISTER the reference to closure
		  ;;object.
		  (asm-instr move %edi tmp_4)
		  ;;Register parameter: load in AA-REGISTER a fixnum representing the
		  ;;negated number of operands: 0.
		  (asm-instr move %eax (constant 0))
		  ;;Non tail-call to the closure object in CP-REGISTER.
		  (non-tail-call
		    (target: #f)
		    (retval-var: #f)
		    (all-rand*: %eax %ebp %edi %esp %esi)
		    (mask: #f)
		    (size: #f)))))
	    ;;Load in a  temporary location, from the relocation  vector, a reference
	    ;;to the the closure object implementing the combinator function.
	    (asm-instr move tmp_5 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
	    (asm-instr move tmp_6 tmp_5)
	    ;;Stack operand: put the operand in the stack slot.
	    (asm-instr move fvar.1 (constant 16))
	    ;;Register  parameter:  load  in  CP-REGISTER the  reference  to  closure
	    ;;object.
	    (asm-instr move %edi tmp_6)
	    ;;Register  parameter:  load in  AA-REGISTER  a  fixnum representing  the
	    ;;negated number of operands: -1.
	    (asm-instr move %eax (constant -8))
	    ;;Tail-call  to the  combinator function.   We jump  direcly to  the code
	    ;;object's entry point.
	    (asmcall direct-jump
		     (code-loc asmlabel:F:clambda:case-1)
		     %eax %ebp %edi %esp %esi fvar.1)))))

  #t)


(parametrise ((check-test-name		'non-tail-calling-clambda-combinator))

  ;;Let's see first the previous compiler pass output.
  (check
      (%before-impose-eval-order '(let ((F (lambda (x) ((primitive +) '1 x))))
				    (begin
				      (F '2)
				      '3)))
    => '(codes
	 ((lambda (label: asmlabel:F:clambda) (cp_0 x_0)
	     (shortcut
		 ;;This is the integrated body of the core primitive operation "+".
		 (seq
		   ;;Check if the argument X_0 is a fixnum.
		   (conditional (asmcall = (asmcall logand x_0 (constant 7)) (constant 0))
		       (asmcall nop)
		     (asmcall interrupt))
		   ;;Attempt to sum the fixnum argument X_0 and the fixnum constant.
		   (asmcall int+/overflow (constant 8) x_0))
	       ;;This is the interrupt handler of the core primitive operation "+".
	       (funcall (asmcall mref (constant (object +)) (constant 19))
		 (constant 8)
		 x_0))))
	 (seq
	   ;;Check the use of the Scheme stack.
	   (shortcut
	       ;;This   is   the  integrated   body   of   the  primitive   operation
	       ;;$STACK-OVERFLOW-CHECK.
	       (conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		   (asmcall interrupt)
		 (asmcall nop))
	     ;;This   is   the  interrupt   handler   of   the  primitive   operation
	     ;;$STACK-OVERFLOW-CHECK.
	     (foreign-call "ik_stack_overflow"))
	   ;;Check if the PCB's engine counter is set.
	   (shortcut
	       (asmcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	   ;;Perfor the non-tail call to the combinator function.
	   (jmpcall asmlabel:F:clambda:case-1
		    (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars))))
		      tmp_0)
		    (constant 16))
	   ;;Return the fixnum 3.
	   (constant 24))))

;;; --------------------------------------------------------------------

  ;;Let's see then the output of this compiler pass.
  (doit (let ((F (lambda (x) ((primitive +) '1 x))))
	  (begin
	    (F '2)
	    '3))
	(codes
	 ((lambda (label: asmlabel:F:clambda) (%edi fvar.1)
	     (locals
	      (local-vars: tmp_0 tmp_1 tmp_2 tmp_3 cp_0)
	      (seq
		;;Load in CP_0 the self reference to closure object from CP-REGISTER.
		(asm-instr move cp_0 %edi)
		;;This SHORTCUT is the implementation of the core primitive operation
		;;"+".
		(shortcut
		    (seq
		      ;;Check if the operand is a fixnum.
		      (conditional (seq (asm-instr move tmp_0 fvar.1)
					(asm-instr logand tmp_0 (constant 7))
					(asm-instr = tmp_0 (constant 0)))
			  (asmcall nop)
			(asmcall interrupt))
		      ;;Load the constant operand.
		      (asm-instr move tmp_1 (constant 8))
		      ;;Attempt to sum the operands.
		      (asm-instr int+/overflow tmp_1 fvar.1)
		      ;;Load the result in AA-REGISTER.
		      (asm-instr move %eax tmp_1)
		      ;;Return to the caller.
		      (asmcall return %eax %ebp %esp %esi))
		  ;;This is the interrupt handler: a  full call to the closure object
		  ;;implementing "+".
		  (seq
		    ;;Move the old operand in a temporary location.
		    (asm-instr move tmp_2 fvar.1)
		    ;;Load the reference to closure object.
		    (asm-instr move tmp_3 (disp (constant (object +)) (constant 19)))
		    ;;Stack operands: put them in the stack slots.
		    (asm-instr move fvar.1 (constant 8))
		    (asm-instr move fvar.2 tmp_2)
		    ;;Parameter  operand: load  the  reference to  closure object  in
		    ;;CP-REGISTER.
		    (asm-instr move %edi tmp_3)
		    ;;Parameter operand:  load in  AA-REGISTER a  fixnum representing
		    ;;the negated number of operands: -2.
		    (asm-instr move %eax (constant -16))
		    ;;Tail-call to the  closure object; we jump directly  to the code
		    ;;object's entry point, retrieving it  from the closure object in
		    ;;CP-REGISTER.
		    (asmcall indirect-jump
			     %eax %ebp %edi %esp %esi fvar.1 fvar.2)))))))
	 (locals
	  (local-vars: tmp_4 tmp_5 tmp_6)
	  (seq
	    ;;This SHORTCUT  is the  implementation of  the core  primitive operation
	    ;;$STACK-OVERFLOW-CHECK.
	    (shortcut
		(conditional (asm-instr u< %esp (disp %esi (constant 32)))
		    (asmcall interrupt)
		  (asmcall nop))
	      (non-tail-call-frame
		(rand*: #f)
		(live: #f)
		(seq
		  (asm-instr move %edi (constant (foreign-label "ik_stack_overflow")))
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target: "ik_stack_overflow")
		    (retval-var: #f)
		    (all-rand*: %eax %ebp %edi %esp %esi)
		    (mask: #f)
		    (size: #f)))))
	    ;;This SHORTCUT  is the  implementation of  the core  primitive operation
	    ;;$DO-EVENT.
	    (shortcut
		(asmcall incr/zero? %esi (constant 72) (constant 8))
	      (non-tail-call-frame
		(rand*: #f)
		(live: #f)
		(seq
		  (asm-instr move tmp_4 (disp (constant (object $do-event)) (constant 19)))
		  (asm-instr move %edi tmp_4)
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target: #f)
		    (retval-var: #f)
		    (all-rand*: %eax %ebp %edi %esp %esi)
		    (mask: #f)
		    (size: #f)))))
	    ;;This is the call to combinator F.
	    (non-tail-call-frame
	      (rand*: nfv.1_0)
	      (live: #f)
	      (seq
		;;Stack operand: load the operand in the stack slot.
		(asm-instr move nfv.1_0 (constant 16))
		;;Load in a temporary location the Retrieve from the
		(asm-instr move tmp_5 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
		(asm-instr move tmp_6 tmp_5)
		;;Register operand: load in CP-REGISTER the reference to closure object.
		(asm-instr move %edi tmp_6)
		;;Register  operand: load  in AA-REGISTER  a fixnum  representing the
		;;negated number of operands, -1.
		(asm-instr move %eax (constant -8))
		;;Non-tail call to the closure object in CP-REGISTER.
		(non-tail-call
		  (target: asmlabel:F:clambda:case-1)
		  (retval-var: #f)
		  (all-rand*: %eax %ebp %edi %esp %esi nfv.1_0)
		  (mask: #f)
		  (size: #f))))
	    ;;Load in AA-REGISTER the fixnum 3.
	    (asm-instr move %eax (constant 24))
	    ;;Return to the caller.
	    (asmcall return %eax %ebp %esp %esi)))))

  #t)


(parametrise ((check-test-name		'non-tail-calling-clambda-combinator-2))

;;;In this case there are 3 stack operands.

  ;;Let's see first the previous compiler pass output.
  (check
      (%before-impose-eval-order '(let ((F (lambda (x y z) ((primitive +) '1 x y z))))
				    (begin
				      (F '2 '3 '4)
				      '5)))
    => '(codes
	 ((lambda (label: asmlabel:F:clambda) (cp_0 x_0 y_0 z_0)
	     (shortcut
		 (seq
		   (conditional (asmcall =
					 (asmcall logand
						  (asmcall logor (asmcall logor x_0 y_0) z_0)
						  (constant 7))
					 (constant 0))
		       (asmcall nop)
		     (asmcall interrupt))
		   (asmcall int+/overflow
			    (asmcall int+/overflow
				     (asmcall int+/overflow (constant 8) x_0)
				     y_0)
			    z_0))
	       (funcall (asmcall mref (constant (object +)) (constant 19))
		 (constant 8) x_0 y_0 z_0))))
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
	   (jmpcall asmlabel:F:clambda:case-3
		    (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars))))
		      tmp_0)
		    (constant 16) (constant 24) (constant 32))
	   (constant 40))))

;;; --------------------------------------------------------------------

  ;;Let's see then the output of this compiler pass.
  (doit (let ((F (lambda (x y z) ((primitive +) '1 x y z))))
	  (begin
	    (F '2 '3 '4)
	    '5))
	(codes
	 ((lambda (label: asmlabel:F:clambda) (%edi fvar.1 fvar.2 fvar.3)
	     (locals
	      (local-vars: tmp_0 tmp_1 tmp_2 tmp_3 tmp_4 tmp_5 cp_0)
	      (seq (asm-instr move cp_0 %edi)
		   (shortcut
		       (seq
			 (conditional
			     (seq (asm-instr move tmp_0 fvar.1)
				  (asm-instr logor tmp_0 fvar.2)
				  (asm-instr logor tmp_0 fvar.3)
				  (asm-instr logand tmp_0 (constant 7))
				  (asm-instr = tmp_0 (constant 0)))
			     (asmcall nop)
			   (asmcall interrupt))
			 (asm-instr move tmp_1 (constant 8))
			 (asm-instr int+/overflow tmp_1 fvar.1)
			 (asm-instr int+/overflow tmp_1 fvar.2)
			 (asm-instr int+/overflow tmp_1 fvar.3)
			 (asm-instr move %eax tmp_1)
			 (asmcall return %eax %ebp %esp %esi))
		     (seq
		       (asm-instr move tmp_2 fvar.3)
		       (asm-instr move tmp_3 fvar.2)
		       (asm-instr move tmp_4 fvar.1)
		       (asm-instr move tmp_5 (disp (constant (object +)) (constant 19)))
		       (asm-instr move fvar.1 (constant 8))
		       (asm-instr move fvar.2 tmp_4)
		       (asm-instr move fvar.3 tmp_3)
		       (asm-instr move fvar.4 tmp_2)
		       (asm-instr move %edi tmp_5)
		       (asm-instr move %eax (constant -32))
		       (asmcall indirect-jump
				%eax %ebp %edi %esp %esi
				fvar.1 fvar.2 fvar.3 fvar.4)))))))
	 (locals
	  (local-vars: tmp_6 tmp_7 tmp_8)
	  (seq
	    ;;Core primitive operation $STACK-OVERFLOW-CHECK.
	    (shortcut (conditional (asm-instr u< %esp (disp %esi (constant 32)))
			  (asmcall interrupt)
			(asmcall nop))
	      (non-tail-call-frame
		(rand*: #f)
		(live: #f)
		(seq
		  (asm-instr move %edi (constant (foreign-label "ik_stack_overflow")))
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target: "ik_stack_overflow")
		    (retval-var: #f)
		    (all-rand*: %eax %ebp %edi %esp %esi)
		    (mask: #f)
		    (size: #f)))))
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
	    (non-tail-call-frame
	      (rand*: nfv.1_0 nfv.2_0 nfv.3_0)
	      (live: #f)
	      (seq
		(asm-instr move nfv.1_0 (constant 16))
		(asm-instr move nfv.2_0 (constant 24))
		(asm-instr move nfv.3_0 (constant 32))
		(asm-instr move tmp_7 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
		(asm-instr move tmp_8 tmp_7)
		(asm-instr move %edi tmp_8)
		(asm-instr move %eax (constant -24))
		(non-tail-call
		  (target: asmlabel:F:clambda:case-3)
		  (retval-var: #f)
		  (all-rand*: %eax %ebp %edi %esp %esi nfv.1_0 nfv.2_0 nfv.3_0)
		  (mask: #f)
		  (size: #f))))
	    (asm-instr move %eax (constant 40))
	    (asmcall return %eax %ebp %esp %esi)))))

  #t)


(parametrise ((check-test-name	'tail-calling-clambda-non-combinator))

  ;;Let's see first the previous compiler pass output.
  (check
      (%before-impose-eval-order '(let ((x ((primitive read))))
				    (let ((F (lambda (y) ((primitive +) x y))))
				      (F '2))))
    => '(codes
	 ((lambda (label: asmlabel:F:clambda) (cp_0 y_0)
	     ;;Retrieve the value of X from the closure object.
	     (bind ((tmp_0 (asmcall mref cp_0 (constant 5))))
	       (shortcut
		   ;;This is the integrated body of the core primitive operation "+".
		   (seq
		     ;;Check  in a  single step  if both  the captured  value of  the
		     ;;variable X=TMP_0 and the operand Y_0 are fixnums.
		     ;;
		     ;;   ((X | Y) & fx-mask) == fx-tag
		     ;;
		     (conditional (asmcall = (asmcall logand
						      (asmcall logor tmp_0 y_0)
						      (constant 7))
					   (constant 0))
			 (asmcall nop)
		       (asmcall interrupt))
		     ;;Attempt to sum the operands.
		     (asmcall int+/overflow tmp_0 y_0))
		 ;;This is the interrupt handler of the core primitive operation "+".
		 (funcall (asmcall mref (constant (object +)) (constant 19))
		   tmp_0 y_0)))))
	 (seq
	   ;;Check the use of the Scheme stack.
	   (shortcut
	       ;;This   is   the  integrated   body   of   the  primitive   operation
	       ;;$STACK-OVERFLOW-CHECK.
	       (conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		   (asmcall interrupt)
		 (asmcall nop))
	     ;;This   is   the   interrupt    body   of   the   primitive   operation
	     ;;$STACK-OVERFLOW-CHECK.
	     (foreign-call "ik_stack_overflow"))
	   ;;Check if the PCB engine counter is set.
	   (shortcut
	       (asmcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	   ;;Here is the actual expression.
	   (bind ((x_0 (funcall (asmcall mref (constant (object read)) (constant 19)))))
	     ;;Allocate the run-time closure object.
	     (bind ((F_0 (asmcall alloc (constant 16) (constant 3))))
	       (seq
		 ;;Store in the  closure object the address of the  binary code entry
		 ;;point.
		 (asmcall mset F_0 (constant -3) (constant (code-loc asmlabel:F:clambda)))
		 ;;Store  in the  closure object  the current  value of  the captured
		 ;;variable.
		 (asmcall mset F_0 (constant 5) x_0)
		 ;;Perform the tail call.
		 (jmpcall asmlabel:F:clambda:case-1 F_0 (constant 16))))))))

;;; --------------------------------------------------------------------

  ;;Let's see then the output of this compiler pass.
  (doit (let ((x ((primitive read))))
	  (let ((F (lambda (y) ((primitive +) x y))))
	    (F '2)))
	(codes
	 ((lambda (label: asmlabel:F:clambda) (%edi fvar.1)
	     (locals
	      (local-vars: tmp_0 tmp_1 tmp_2 tmp_3 tmp_4 tmp_5 cp_0)
	      (seq
		;;Load  in  CP_0  the  reference  to this  closure  object  from  the
		;;CP-REGISTER.
		(asm-instr move cp_0 %edi)
		;;Load the value of the free variable X from the closure object.
		(asm-instr move tmp_0 (disp cp_0 (constant 5)))
		;;This SHORTCUT is the implementation  of the core primitive function
		;;"+".
		(shortcut
		    ;;This is the integrated body.
		    (seq
		      ;;With a single step: check if  the captured value of X (TMP_0)
		      ;;and the argument Y (fvar.1) are both fixnums.
		      (conditional (seq (asm-instr move tmp_1 tmp_0)
					(asm-instr logor tmp_1 fvar.1)
					(asm-instr logand tmp_1 (constant 7))
					(asm-instr = tmp_1 (constant 0)))
			  (asmcall nop)
			(asmcall interrupt))
		      ;;Attempt to add the two fixnums.
		      (asm-instr move tmp_2 tmp_0)
		      (asm-instr int+/overflow tmp_2 fvar.1)
		      ;;Load the result in AA-REGISTER.
		      (asm-instr move %eax tmp_2)
		      ;;Return to the caller.
		      (asmcall return %eax %ebp %esp %esi))
		  ;;This  is the  interrupt  handler:  a full  call  to the  function
		  ;;implementing "+".
		  (seq
		    ;;Put in temporary locations the stack operands of the call.
		    (asm-instr move tmp_3 fvar.1)
		    (asm-instr move tmp_4 tmp_0)
		    ;;Load in a  temporary location, from the  relocation vector, the
		    ;;reference to the closure object implementing "+".
		    (asm-instr move tmp_5 (disp (constant (object +)) (constant 19)))
		    ;;Stack operands: put them on the stack in the correct order.
		    (asm-instr move fvar.1 tmp_4)
		    (asm-instr move fvar.2 tmp_3)
		    ;;Register operand: load in  CP-REGISTER the reference to closure
		    ;;object.
		    (asm-instr move %edi tmp_5)
		    ;;Register operand: load in AA-REGISTER a fixnum representing the
		    ;;negated number of operands: -2.
		    (asm-instr move %eax (constant -16))
		    ;;Call "+" by jumping to the code object entry point; the address
		    ;;of  the  entry  point  is  taken from  the  closure  object  in
		    ;;CP-REGISTER.
		    (asmcall indirect-jump %eax %ebp %edi %esp %esi fvar.1 fvar.2)))))))
	 (locals
	  (local-vars: tmp_6 tmp_7 x_0 tmp_8 F_0)
	  (seq
	    ;;This SHORTCUT  is the  implementation of  the core  primitive operation
	    ;;$STACK-OVERFLOW-CHECK.
	    (shortcut
		;;Check if the stack usage has crossed the red line.
		(conditional (asm-instr u< %esp (disp %esi (constant 32)))
		    (asmcall interrupt)
		  (asmcall nop))
	      (non-tail-call-frame
		(rand*: #f)
		(live: #f)
		(seq
		  ;;Load in CP-REGISTER the address of the C language function.
		  (asm-instr move %edi (constant (foreign-label "ik_stack_overflow")))
		  ;;Load in AA-REGISTER  a fixnum representing the  negated number of
		  ;;operands: 0.
		  (asm-instr move %eax (constant 0))
		  ;;Non-tail call  to the  C language  function through  the Assembly
		  ;;routine "ik_foreign_call".
		  (non-tail-call
		    (target: "ik_stack_overflow")
		    (retval-var: #f)
		    (all-rand*: %eax %ebp %edi %esp %esi)
		    (mask: #f)
		    (size: #f)))))
	    ;;This SHORTCUT is the implementation of the core primitive $DO-EVENT.
	    (shortcut
		;;Check if the PCB engine counter is set.
		(asmcall incr/zero? %esi (constant 72) (constant 8))
	      (non-tail-call-frame
		(rand*: #f)
		(live: #f)
		(seq
		  ;;Load in  a temporary  location, from  the relocation  vector, the
		  ;;reference to closure object implementing $DO-EVENT.
		  (asm-instr move tmp_6 (disp (constant (object $do-event)) (constant 19)))
		  ;;Register operand:  load in  CP-REGISTER the reference  to closure
		  ;;object.
		  (asm-instr move %edi tmp_6)
		  ;;Register operand:  load in AA-REGISTER a  fixnum representing the
		  ;;negated number of operands: 0.
		  (asm-instr move %eax (constant 0))
		  ;;Non-tail call to the closure object in CP-REGISTER.
		  (non-tail-call
		    (target: #f)
		    (retval-var: #f)
		    (all-rand*: %eax %ebp %edi %esp %esi)
		    (mask: #f)
		    (size: #f)))))
	    ;;This is the call to the core primitive function READ.
	    (non-tail-call-frame
	      (rand*: #f)
	      (live: #f)
	      (seq
		;;Load  in a  temporary  location, from  the  relocation vector,  the
		;;reference to closure object implementing READ.
		(asm-instr move tmp_7 (disp (constant (object read)) (constant 19)))
		;;Register  operand: load  in  CP-REGISTER the  reference to  closure
		;;object.
		(asm-instr move %edi tmp_7)
		;;Register  operand: load  in AA-REGISTER  a fixnum  representing the
		;;negated number of operands: 0.
		(asm-instr move %eax (constant 0))
		;;Non-tail call to the closure object in CP-REGISTER.
		(non-tail-call
		  (target: #f)
		  (retval-var: x_0)
		  (all-rand*: %eax %ebp %edi %esp %esi)
		  (mask: #f)
		  (size: #f))))
	    ;;Store in X_0, from AA-REGISTER, the single value returned by READ.
	    (asm-instr move x_0 %eax)
	    ;;Now we have to allocate a closure object.
	    ;;
	    ;;This  shortcut  is  the   implementation  of  the  high-level  Assembly
	    ;;instruction ALLOC.
	    (shortcut
		;;The closure object's  aligned size is less than a  page size; check
		;;if the AP-REGISTER has crossed the heap nursery red line.
		(conditional (asm-instr <= %ebp (disp %esi (constant 8)))
		    (asmcall nop)
		  (asmcall interrupt))
	      ;;This is the interrupt handler:  call the function DO-OVERFLOW to make
	      ;;room for the closure object.
	      (non-tail-call-frame
		(rand*: nfv.1_0)
		(live: #f)
		(seq
		  ;;Stack  operand:  load in  the  stack  slot the  closure  object's
		  ;;aligned size.
		  (asm-instr move nfv.1_0 (constant 16))
		  ;;Load in  a temporary  location, from  the relocation  vector, the
		  ;;reference to the closure object implementing DO-OVERFLOW.
		  (asm-instr move tmp_8 (disp (constant (object do-overflow)) (constant 27)))
		  ;;Register operand:  load in  CP-REGISTER the reference  to closure
		  ;;object.
		  (asm-instr move %edi tmp_8)
		  ;;Register operand:  load in AA-REGISTER a  fixnum representing the
		  ;;negated number of arguments: -1.
		  (asm-instr move %eax (constant -8))
		  ;;Non-tail call to the closure object in CP-REGISTER.
		  (non-tail-call
		    (target: #f)
		    (retval-var: #f)
		    (all-rand*: %eax %ebp %edi %esp %esi nfv.1_0)
		    (mask: #f)
		    (size: #f)))))
	    ;;Now the value of AP-REGISTER is a  pointer to the first machine word of
	    ;;the closure object on the heap nursery.
	    ;;
	    ;;Load the raw pointer to the closure object.
	    (asm-instr move F_0 %ebp)
	    ;;Tag the pointer with the primary tag of closure objects.
	    (asm-instr logor F_0 (constant 3))
	    ;;Increment the AP-REGISTER by the aligned size of the closure object.
	    (asm-instr int+ %ebp (constant 16))
	    ;;Store in  the closure  object the  address of  the code  object's entry
	    ;;point.
	    (asm-instr mset
		       (disp F_0 (constant -3))
		       (constant (code-loc asmlabel:F:clambda)))
	    ;;Store in  the closure  object the  current value  of the  variable this
	    ;;closure is closed upon.
	    (asm-instr mset (disp F_0 (constant 5)) x_0)
	    ;;Stack operand: put in the stack slot the operand for the call to F.
	    (asm-instr move fvar.1 (constant 16))
	    ;;Register operand: load in CP-REGISTER the reference to closure F.
	    (asm-instr move %edi F_0)
	    ;;Register operand: load in AA-REGISTER a fixnum representing the negated
	    ;;number of arguments: -1.
	    (asm-instr move %eax (constant -8))
	    ;;Tail call the closure object F.   We jump directly to the code object's
	    ;;entry point.
	    (asmcall direct-jump
		     (code-loc asmlabel:F:clambda:case-1)
		     %eax %ebp %edi %esp %esi fvar.1)))))

  #t)


(parametrise ((check-test-name	'call-display-for-side-effects))

;;;DISPLAY is  called for  its side effects;  this means its  return value  (which is
;;;#<void>) is discarded.  Let's look at the contents of the structs NON-TAIL-CALL.

  (check
      (%before-impose-eval-order '(begin
				    ((primitive display) '1)
				    '2))
    => '(codes
	 ()
	 (seq
	   (shortcut
	       (conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		   (asmcall interrupt)
		 (asmcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (funcall (asmcall mref (constant (object display)) (constant 19))
	     (constant 8))
	   (constant 16))))

  (doit (begin
	  ((primitive display) '1)
	  '2)
	(codes
	 ()
	 (locals
	  (local-vars: tmp_0)
	  (seq
	    ;;This SHORTCUT  is the  implementation of  the core  primitive operation
	    ;;$STACK-OVERFLOW-CHECK.
	    (shortcut
		(conditional (asm-instr u< %esp (disp %esi (constant 32)))
		    (asmcall interrupt)
		  (asmcall nop))
	      (non-tail-call-frame
		(rand*: #f)
		(live: #f)
		(seq
		  (asm-instr move %edi (constant (foreign-label "ik_stack_overflow")))
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target: "ik_stack_overflow")
		    (retval-var: #f)
		    (all-rand*: %eax %ebp %edi %esp %esi)
		    (mask: #f)
		    (size: #f)))))
	    (non-tail-call-frame
	      (rand*: nfv.1_0)
	      (live: #f)
	      (seq
		;;Stack operands: put the operand in the stack slot.
		(asm-instr move nfv.1_0 (constant 8))
		;;Load  in a  temporary  location, from  the  relocation vector,  the
		;;reference to closure object implementing DISPLAY.
		(asm-instr move tmp_0 (disp (constant (object display)) (constant 19)))
		;;Register  operand: load  in  CP-REGISTER the  reference to  closure
		;;object.
		(asm-instr move %edi tmp_0)
		;;Register  operand: load  in AA-REGISTER  a fixnum  representing the
		;;negated number of operands: -1.
		(asm-instr move %eax (constant -8))
		(non-tail-call
		  (target: #f)
		  (retval-var: #f)
		  (all-rand*: %eax %ebp %edi %esp %esi nfv.1_0)
		  (mask: #f)
		  (size: #f))))
	    ;;Load in AA-REGISTER the fixnum 2.
	    (asm-instr move %eax (constant 16))
	    ;;Return to the caller.
	    (asmcall return %eax %ebp %esp %esi)))))

  #t)


(parametrise ((check-test-name	'recursive-function-same-operands))

;;;Check  what happens  when a  function tail-calls  itself with  the same  operands.
;;;Before the recursive  tail-call: no operands movement should  be inserted, because
;;;all the operands are already in the correctl place on the Scheme stack.

  (check
      (%before-impose-eval-order '(let ((F (lambda (a b) (F a b))))
				    (F '1 '2)))
    => '(codes
	 ((lambda (label: asmlabel:F:clambda) (cp_0 a_0 b_0)
	     (seq
	       ;;Check if the PCB's engine counter has changed.
	       (shortcut
		   (asmcall incr/zero? %esi (constant 72) (constant 8))
		 (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	       ;;Perform the recursive tail-call.
	       (jmpcall asmlabel:F:clambda:case-2
			(bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars))))
			  tmp_0)
			a_0 b_0))))
	 (seq
	   ;;Check if the PCB's engine counter has changed.
	   (shortcut
	       (asmcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	   ;;Perform the tail-call to F.
	   (jmpcall asmlabel:F:clambda:case-2
		    (bind ((tmp_1 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars))))
		      tmp_1)
		    (constant 8)
		    (constant 16)))))

;;; --------------------------------------------------------------------

  (doit (let ((F (lambda (a b) (F a b))))
	  (F '1 '2))
	(codes
	 ((lambda (label: asmlabel:F:clambda) (%edi fvar.1 fvar.2)
	     (locals
	      (local-vars: tmp_0 tmp_1 tmp_2 cp_0)
	      (seq
		(asm-instr move cp_0 %edi)
		(shortcut
		    (asmcall incr/zero? %esi (constant 72) (constant 8))
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
		(asm-instr move tmp_1 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
		(asm-instr move tmp_2 tmp_1)
		(asm-instr move %edi tmp_2)
		(asm-instr move %eax (constant -16))
		(asmcall direct-jump
			 (code-loc asmlabel:F:clambda:case-2)
			 %eax %ebp %edi %esp %esi fvar.1 fvar.2)))))
	 (locals
	  (local-vars: tmp_3 tmp_4 tmp_5)
	  (seq
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
	    (asm-instr move tmp_4 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
	    (asm-instr move tmp_5 tmp_4)
	    (asm-instr move fvar.1 (constant 8))
	    (asm-instr move fvar.2 (constant 16))
	    (asm-instr move %edi tmp_5)
	    (asm-instr move %eax (constant -16))
	    (asmcall direct-jump
		     (code-loc asmlabel:F:clambda:case-2)
		     %eax %ebp %edi %esp %esi fvar.1 fvar.2)))))

  #t)


(parametrise ((check-test-name	'nested-non-tail-calls))

;;;We want to show  what happens when we perform a non-tail  call while preparing the
;;;stack operands for a non-tail call.

  (check
      (%before-impose-eval-order '(let ((f (lambda (x) (+ '1 x)))
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

  (doit (let ((f (lambda (x) (+ '1 x)))
	      (g (lambda (y) (+ '2 y))))
	  (begin
	    (f (g '3))
	    '4))
	(codes
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

  #t)


(parametrise ((check-test-name	'seal-frame-and-call))

  (check
      (%before-impose-eval-order
       (receive (code libs)
	   (expand-form-to-core-language '(lambda (receiver-func)
					    ($seal-frame-and-call receiver-func))
					 (environment '(vicare)
						      '(vicare system $stack)))
	 code))
    => '(codes
	 ((lambda (label: asmlabel:anonymous:clambda) (cp_0 lex.receiver-func_0)
	     ;;Allocate a continuation object.
	     (bind ((kont-obj_0 (asmcall alloc-no-hooks (constant 32) (constant 5))))
	       ;;Compute the  address of  the word  on the  stack holding  the return
	       ;;address to the underflow handler.
	       (bind ((base_0 (asmcall int+ (asmcall mref %esi (constant 24)) (constant -8))))
		 ;;Load  the return  address to  the underflow  handler into  a local
		 ;;variable.
		 (bind ((underflow-handler_0 (asmcall mref base_0 (constant 0))))
		   (seq
		     ;;Store the continuation secondary tag  in the first word of the
		     ;;continuation object.
		     (asmcall mset kont-obj_0 (constant -5) (constant 31))
		     ;;Store  in the  continuation object  the current  Frame Pointer
		     ;;Register  as  address   to  go  back  to   when  resuming  the
		     ;;continuation.
		     (asmcall mset kont-obj_0 (constant 3) %esp)
		     ;;Store  in   the  continuation  object  the   number  of  bytes
		     ;;representing the total size of the freezed stack frames.
		     (asmcall mset kont-obj_0 (constant 11) (asmcall int- base_0 %esp))
		     ;;Prepend  the new  continuation object  to the  linked list  of
		     ;;"next process continuations" in the PCB.
		     ;;
		     ;;   kont_obj->next = pcb->next_k;
		     ;;   pcb->next_k    = kont_obj;
		     ;;
		     (asmcall mset kont-obj_0 (constant 19) (asmcall mref %esi (constant 40)))
		     (asmcall mset %esi (constant 40) kont-obj_0)
		     ;;The machine word  referenced by the FPR is the  new frame base
		     ;;for subsequent  code execution;  store the FPR  in the  PCB as
		     ;;frame base.
		     (asmcall mset %esi (constant 24) %esp)
		     (asmcall call-with-underflow-handler underflow-handler_0 kont-obj_0 lex.receiver-func_0)))))))
	 (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:anonymous:clambda) no-freevars))))
	   tmp_0)))

;;; --------------------------------------------------------------------

  (check
      (%impose-eval-order
       (receive (code libs)
	   (expand-form-to-core-language '(lambda (receiver-func)
					    ($seal-frame-and-call receiver-func))
					 (environment '(vicare)
						      '(vicare system $stack)))
	 code))
    => '(codes
	 ((lambda (label: asmlabel:anonymous:clambda) (%edi fvar.1)
	     (locals
	      (local-vars: kont-obj_0 base_0 underflow-handler_0
			   tmp_0 tmp_1 tmp-underfow-handler_0
			   tmp-kont-object_0 tmp-receiver-func_0 cp_0)
	      (seq
		(asm-instr move cp_0 %edi)
		(shortcut
		    (conditional (asm-instr <= %ebp (disp %esi (constant 8)))
			(asmcall nop)
		      (asmcall interrupt))
		  (non-tail-call-frame
		    (rand*: nfv.1_0)
		    (live: #f)
		    (seq
		      (asm-instr move nfv.1_0 (constant 32))
		      (asm-instr move %edi (constant (foreign-label "ik_collect")))
		      (asm-instr move %eax (constant -8))
		      (non-tail-call
			(target: "ik_collect")
			(retval-var: #f)
			(all-rand*: %eax %ebp %edi %esp %esi nfv.1_0)
			(mask: #f)
			(size: #f)))))
		(asm-instr move kont-obj_0 %ebp)
		(asm-instr logor kont-obj_0 (constant 5))
		(asm-instr int+ %ebp (constant 32))
		(asm-instr move base_0 (disp %esi (constant 24)))
		(asm-instr int+ base_0 (constant -8))
		(asm-instr move underflow-handler_0 (disp base_0 (constant 0)))
		(asm-instr mset (disp kont-obj_0 (constant -5)) (constant 31))
		(asm-instr mset (disp kont-obj_0 (constant 3)) %esp)
		(asm-instr move tmp_0 base_0)
		(asm-instr int- tmp_0 %esp)
		(asm-instr mset (disp kont-obj_0 (constant 11)) tmp_0)
		(asm-instr move tmp_1 (disp %esi (constant 40)))
		(asm-instr mset (disp kont-obj_0 (constant 19)) tmp_1)
		(asm-instr mset (disp %esi (constant 40)) kont-obj_0)
		(asm-instr mset (disp %esi (constant 24)) %esp)
		(asm-instr move tmp-underfow-handler_0 underflow-handler_0)
		(asm-instr move tmp-kont-object_0 kont-obj_0)
		(asm-instr move tmp-receiver-func_0 fvar.1)
		(asm-instr move fvar.1 tmp-underfow-handler_0)
		(asm-instr move fvar.2 tmp-kont-object_0)
		(asm-instr move %edi tmp-receiver-func_0)
		(asm-instr move %eax (constant -8))
		(asm-instr int- %esp (constant 8))
		(asmcall indirect-jump %eax %ebp %edi %esp %esi fvar.1 fvar.2)))))
	 (locals
	  (local-vars: tmp_2)
	  (seq
	    (asm-instr move tmp_2 (constant (closure-maker (code-loc asmlabel:anonymous:clambda) no-freevars)))
	    (asm-instr move %eax tmp_2)
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
