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
	      ;;The  list of  machine words  we  need in  this function  to hold  the
	      ;;temporary local values.   They will be CPU registers  or Scheme stack
	      ;;words.
	      (local-vars: tmp_0 tmp_1 tmp_2 tmp_3 cp_0)
	      (seq
		;;Load  the reference  to closure  object from  the CP-REGISTER  into
		;;CP_0.
		(asm-instr move cp_0 %edi)
		(shortcut
		    ;;This is  the integrated  body of  the core  primitive operation
		    ;;"+".
		    (seq
		      ;;Check if the argument in the Scheme stack machine word FVAR.1
		      ;;is a fixnum.
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
		  ;;The  sum between  fixnums  failed: tail-call  the core  primitive
		  ;;function "+".
		  (seq
		    ;;To prepare for the tail call we need to remove the old operands
		    ;;from the Scheme stack: we need the location FVAR.1 for the tail
		    ;;call's operands.
		    (asm-instr move tmp_3 fvar.1)
		    ;;Load the reference  to the function "+" from the  loc gensym in
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
		    ;;Perform  the tail-call  as indirect  jump: the  address of  the
		    ;;entry  point of  the  function  "+" is  in  the closure  object
		    ;;referenced by CP-REGISTER.
		    (asmcall indirect-jump %eax %esi %esp %ebp %edi fvar.1 fvar.2)))))))
	 (locals
	  ;;The  list  of machine  words  we  need in  this  expression  to hold  the
	  ;;temporary  local values.   They will  be  CPU registers  or Scheme  stack
	  ;;words.
	  (local-vars: tmp_4 tmp_5)
	  (seq
	    ;;Check if the PCB engine counter is set.
	    (shortcut
		;;This  is  the  integrated  body of  the  core  primitive  operation
		;;"$do-event": check if the PCB's engine counter has been set.
		(asmcall incr/zero? %esi (constant 72) (constant 8))
	      (non-tail-call-frame
		(vars: #f)
		(live: #f)
		(seq
		  ;;Load the reference to the function "$do-event" from the loc gensym
		  ;;in the relocation vector.
		  (asm-instr move tmp_4 (disp (constant (object $do-event))
					      (constant 19)))
		  ;;Store the reference to function "$do-event" in the CP-REGISTER.
		  (asm-instr move %edi tmp_4)
		  ;;Load the negated number of  arguments in the AA-REGISTER; zero for
		  ;;"$do-event".
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target:      #f)
		    (retval-var:  #f)
		    (args:        %eax %ebp %edi %esp %esi)
		    (mask:        #f)
		    (size:        #f)))))
	    ;;Retrieve  from the  relocation vector  of the  code object  the closure
	    ;;object of the combinator F.
	    (asm-instr move tmp_5 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
	    ;;Store  a  reference  to  the  combinator  F's  closure  object  in  the
	    ;;CP-REGISTER.
	    (asm-instr move %edi tmp_5)
	    ;;Put on the Scheme stack the operand of F.
	    (asm-instr move fvar.1 (constant 16))
	    ;;Load the negated number of arguments in the AA-REGISTER; -1 for F.
	    (asm-instr move %eax (constant -8))
	    ;;Tail-call the operator F.
	    (asmcall direct-jump
		     (code-loc asmlabel:F:clambda:case-1)
		     %eax %ebp %esp %esi %edi fvar.1)))))

  #t)


(parametrise ((check-test-name	'nontail-calling-clambda-combinator))

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
		;;Load  in  CP_0  the  reference  to  the  closure  object  from  the
		;;CP-REGISTER.
		(asm-instr move cp_0 %edi)
		(shortcut
		    ;;This is  the integrated  body of  the core  primitive operation
		    ;;"+".
		    (seq
		      ;;Check if the argument FVAR.1 is a fixnum
		      (conditional (seq (asm-instr move tmp_0 fvar.1)
					(asm-instr logand tmp_0 (constant 7))
					(asm-instr = tmp_0 (constant 0)))
			  (asmcall nop)
			(asmcall interrupt))
		      ;;Attempt  to add  the fixnum  argument FVAR.1  and the  fixnum
		      ;;constant.
		      (asm-instr move tmp_1 (constant 8))
		      (asm-instr int+/overflow tmp_1 fvar.1)
		      ;;Store the result in the AA-REGISTER.
		      (asm-instr move %eax tmp_1)
		      ;;Return to the caller.
		      (asmcall return %esi %esp %ebp %eax))
		  ;;This is  the interrupt  handler of  the core  primitive operation
		  ;;"+".
		  (seq
		    ;;Store in TMP_3 the argument.
		    (asm-instr move tmp_3 fvar.1)
		    ;;Load  in TMP_2  the reference  to closure  object "+"  from the
		    ;;relocation vector.
		    (asm-instr move tmp_2 (disp (constant (object +))
						(constant 19)))
		    ;;Store in the CP-REGISTER the reference to the closure object to
		    ;;call.
		    (asm-instr move %edi tmp_2)
		    ;;Put on the stack the operands of the call.
		    (asm-instr move fvar.1 (constant 8))
		    (asm-instr move fvar.2 tmp_3)
		    ;;Load in AA-REGISTER a fixnum representing the negated number of
		    ;;operands.
		    (asm-instr move %eax (constant -16))
		    ;;Tail-call the closure object in the CP-REGISTER.
		    (asmcall indirect-jump %eax %esi %esp %ebp %edi fvar.1 fvar.2)))))))
	 (locals
	  (local-vars: tmp_4 tmp_5 tmp_6 tmp_7)
	  (seq
	    ;;Check the use of the Scheme stack.
	    (shortcut
		;;This   is  the   integrated   body  of   the  primitive   operation
		;;$STACK-OVERFLOW-CHECK.
		(conditional (asm-instr u< %esp (disp %esi (constant 32)))
		    (asmcall interrupt)
		  (asmcall nop))
	      ;;This   is   the  interrupt   hanler   of   the  primitive   operation
	      ;;$STACK-OVERFLOW-CHECK.
	      (non-tail-call-frame
		(vars: #f)
		(live: #f)
		(seq
		  (asm-instr move tmp_4 (constant (foreign-label "ik_stack_overflow")))
		  (asm-instr move %edi tmp_4)
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target:      "ik_stack_overflow")
		    (retval-var:  #f)
		    (args:        %eax %esi %esp %ebp %edi)
		    (mask:        #f)
		    (size:        #f)))))
	    ;;Check if the PCB's engine counter is set.
	    (shortcut
		(asmcall incr/zero? %esi (constant 72) (constant 8))
	      (non-tail-call-frame
		(vars: #f)
		(live: #f)
		(seq
		  (asm-instr move tmp_5 (disp (constant (object $do-event))
					      (constant 19)))
		  (asm-instr move %edi tmp_5)
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target:      #f)
		    (retval-var:  #f)
		    (args:        %eax %esi %esp %ebp %edi)
		    (mask:        #f)
		    (size:        #f)))))
	    ;;Non-tail call the operator function.
	    (non-tail-call-frame
	      (vars: (nfv unset-conflicts))
	      (live: #f)
	      (seq
		;;Load in the stack operand location the operand fixnum 2.
		(asm-instr move (nfv unset-conflicts) (constant 16))
		;;Load in TMP_6 the reference to closure object implementing F.
		(asm-instr move tmp_6 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
		(asm-instr move tmp_7 tmp_6)
		;;Store in CP-REGISTER the reference to closure object.
		(asm-instr move %edi tmp_7)
		;;Load in  AA-REGISTER a  fixnum representing  the negated  number of
		;;arguments: -1.
		(asm-instr move %eax (constant -8))
		(non-tail-call
		  (target:      asmlabel:F:clambda:case-1)
		  (retval-var:  #f)
		  (args:        %eax %esi %esp %ebp %edi (nfv unset-conflicts))
		  (mask:        #f)
		  (size:        #f))))
	    ;;Load in AA-REGISTER the fixnum 3.
	    (asm-instr move %eax (constant 24))
	    ;;Return to the caller.
	    (asmcall return %esi %esp %ebp %eax)))))

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
		;;Load in CP_0 the reference to closure object from the CP-REGISTER.
		(asm-instr move cp_0 %edi)
		;;Retrieve the value of X from the closure object.
		(asm-instr move tmp_0 (disp cp_0 (constant 5)))
		(shortcut
		    ;;This is the integrated body of the core primitive operation "+".
		    (seq
		      ;;Check in  a single  step if  both the  captured value  of the
		      ;;variable X=TMP_0 and the operand Y_0 are fixnums.
		      ;;
		      ;;   ((X | Y) & fx-mask) == fx-tag
		      ;;
		      (conditional (seq (asm-instr move tmp_1 tmp_0)
					(asm-instr logor tmp_1 fvar.1)
					(asm-instr logand tmp_1 (constant 7))
					(asm-instr = tmp_1 (constant 0)))
			  (asmcall nop)
			(asmcall interrupt))
		      ;;Attempt to sum the operands.
		      (asm-instr move tmp_2 tmp_0)
		      (asm-instr int+/overflow tmp_2 fvar.1)
		      ;;Load the result in AA-REGISTER.
		      (asm-instr move %eax tmp_2)
		      ;;Return to the caller.
		      (asmcall return %esi %esp %ebp %eax))
		  ;;This is  the interrupt  handler of  the core  primitive operation
		  ;;"+".
		  (seq
		    ;;Move the operands in temporary locations.
		    (asm-instr move tmp_5 fvar.1)
		    (asm-instr move tmp_4 tmp_0)
		    ;;Retrieve form  the relocation  vector a  reference to  the core
		    ;;primitive "+".
		    (asm-instr move tmp_3 (disp (constant (object +)) (constant 19)))
		    ;;Store in CP-REGISTER the reference to "+".
		    (asm-instr move %edi tmp_3)
		    ;;Put the operands on the stack, in the correct order.
		    (asm-instr move fvar.1 tmp_4)
		    (asm-instr move fvar.2 tmp_5)
		    ;;Store in  AA-REGISTER a fixnum representing  the negated number
		    ;;of operands: -2.
		    (asm-instr move %eax (constant -16))
		    ;;Perform the tail call.
		    (asmcall indirect-jump %eax %esi %esp %ebp %edi fvar.1 fvar.2)))))))
	 (locals
	  (local-vars: tmp_6 tmp_7 tmp_8 x_0 tmp_9 F_0 tmp_10)
	  (seq
	    ;;Check the use of the Scheme stack.
	    (shortcut
		(conditional (asm-instr u< %esp (disp %esi (constant 32)))
		    (asmcall interrupt)
		  (asmcall nop))
	      (non-tail-call-frame
		(vars: #f)
		(live: #f)
		(seq
		  (asm-instr move tmp_6 (constant (foreign-label "ik_stack_overflow")))
		  (asm-instr move %edi tmp_6)
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target:      "ik_stack_overflow")
		    (retval-var:  #f)
		    (args:        %eax %esi %esp %ebp %edi)
		    (mask:        #f)
		    (size:        #f)))))
	    ;;Check if the PCB engine counter is set.
	    (shortcut
		(asmcall incr/zero? %esi (constant 72) (constant 8))
	      (non-tail-call-frame
		(vars: #f)
		(live: #f)
		(seq
		  (asm-instr move tmp_7 (disp (constant (object $do-event))
					      (constant 19)))
		  (asm-instr move %edi tmp_7)
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target:      #f)
		    (retval-var:  #f)
		    (args:        %eax %esi %esp %ebp %edi)
		    (mask:        #f)
		    (size:        #f)))))
	    ;;Here is the actual expression.  First we call READ and store the result
	    ;;in the location X_0.
	    (non-tail-call-frame
	      (vars: #f)
	      (live: #f)
	      (seq
		;;Retrieve,  from the  relocation  vector, the  reference to  closure
		;;object implementing READ.
		(asm-instr move tmp_8 (disp (constant (object read))
					    (constant 19)))
		;;Store in CP-REGISTER the reference to READ.
		(asm-instr move %edi tmp_8)
		;;Store in  AA-REGISTER a fixnum  representing the negated  number of
		;;operands for READ: zero.
		(asm-instr move %eax (constant 0))
		;;Perform  the  non-tail  call  to   READ;  the  result  is  left  in
		;;AA-REGISTER.
		(non-tail-call
		  (target:      #f)
		  (retval-var:  x_0)
		  (args:        %eax %esi %esp %ebp %edi)
		  (mask:        #f)
		  (size:        #f))))
	    ;;Load in X_0 the result of READ from AA-REGISTER.
	    (asm-instr move x_0 %eax)
	    ;;Here we begin what is needed to allocate the run-time closure object.
	    ;;
	    ;;First make sure that there is enough room for the closure object in the
	    ;;heap nursery.
	    (shortcut
		;;Compare the  AP-REGISTER with the  nursery red line pointer  in the
		;;PCB.
		(conditional (asm-instr <= %ebp (disp %esi (constant 8)))
		    (asmcall nop)
		  (asmcall interrupt))
	      ;;If there is no room: call  DO-OVERFLOW to perform a garbage collector
	      ;;run.
	      (non-tail-call-frame
		(vars: (nfv unset-conflicts))
		(live: #f)
		(seq
		  ;;Store in the stack operand  location the operand for the non-tail
		  ;;call to DO-OVERFLOW.
		  (asm-instr move (nfv unset-conflicts) (constant 16))
		  ;;Retrieve from  the relocation vector  a reference to  the closure
		  ;;object implementing DO-OVERFLOW.
		  (asm-instr move tmp_9 (disp (constant (object do-overflow))
					      (constant 27)))
		  ;;Store in CP-REGISTER the reference to DO-OVERFLOW.
		  (asm-instr move %edi tmp_9)
		  ;;Store in AA-REGISTER a fixnum  representing the negated number of
		  ;;operands: -1.
		  (asm-instr move %eax (constant -8))
		  ;;Perform the non-tail call; the returned value is discarded.
		  (non-tail-call
		    (target:      #f)
		    (retval-var:  #f)
		    (args:        %eax %esi %esp %ebp %edi (nfv unset-conflicts))
		    (mask:        #f)
		    (size:        #f)))))
	    ;;The  pointer to  the new  closure object  is the  current value  of the
	    ;;AP-REGISTER.
	    (asm-instr move F_0 %ebp)
	    ;;Tag the pointer with the CLOSURE-TAG.
	    (asm-instr logor F_0 (constant 3))
	    ;;Increment the AP-REGISTER by the closure object's aligned size.
	    (asm-instr int+ %ebp (constant 16))
	    ;;Store in the closure object the address of the binary code entry point.
	    (asm-instr mset
		       (disp F_0 (constant -3))
		       (constant (code-loc asmlabel:F:clambda)))
	    ;;Store in the closure object the current value of the captured variable.
	    (asm-instr mset
		       (disp F_0 (constant 5))
		       x_0)
	    ;;Store in the CP-REGSITER the reference to closure object.
	    (asm-instr move tmp_10 F_0)
	    (asm-instr move %edi tmp_10)
	    ;;Put on the stack the operand to the closure call.
	    (asm-instr move fvar.1 (constant 16))
	    ;;Store  in  AA-REGISTER a  fixnum  representing  the negated  number  of
	    ;;operands: -1.
	    (asm-instr move %eax (constant -8))
	    ;;Perform the tail call.
	    (asmcall direct-jump
		     (code-loc asmlabel:F:clambda:case-1)
		     %eax %esi %esp %ebp %edi fvar.1)))))

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
	  (local-vars: tmp_0 tmp_1)
	  (seq
	    (shortcut
		;;Have we crossed the red line of Scheme stack usage?
		(conditional (asm-instr u< %esp (disp %esi (constant 32)))
		    (asmcall interrupt)
		  (asmcall nop))
	      ;;A stack  reallocation is needed.   Perform a  call to the  C function
	      ;;"ik_stack_overflow()",   through   calling   the   Assembly   routine
	      ;;"ik_foreign_call".
	      (non-tail-call-frame
		(vars: #f)
		(live: #f)
		(seq
		  ;;Retrieve  the address  of  the  C function  "ik_stack_overflow()";
		  ;;store it in TMP_0.
		  (asm-instr move tmp_0 (constant (foreign-label "ik_stack_overflow")))
		  ;;Load in  CP-REGISTER the address  of the C function;  the Assembly
		  ;;routine "ik_foreign_call" expects it there.
		  (asm-instr move %edi tmp_0)
		  ;;Load in  AA-REGISTER a fixnum  representing the negated  number of
		  ;;aruments; 0 for this call.
		  (asm-instr move %eax (constant 0))
		  ;;Do the call.  Discard the return value.
		  (non-tail-call
		    (target:      "ik_stack_overflow")
		    (retval-var:  #f)
		    (args:        %eax %esi %esp %ebp %edi)
		    (mask:        #f)
		    (size:        #f)))))
	    ;;Perform the non-tail call to DISPLAY.
	    (non-tail-call-frame
	      (vars: (nfv unset-conflicts))
	      (live: #f)
	      (seq
		;;Load in the stack operand location the operand of DISPLAY.
		(asm-instr move (nfv unset-conflicts) (constant 8))
		;;Retrieve  from  the relocation  vector  the  reference to  the  core
		;;primitive function DISPLAY.
		(asm-instr move tmp_1 (disp (constant (object display))
					    (constant 19)))
		;;Store the reference to DISPLAY in the CP-REGISTER.
		(asm-instr move %edi tmp_1)
		;;Load in  AA-REGISTER the fixnum  representing the negated  number of
		;;operands; -1 for this call to DISPLAY.
		(asm-instr move %eax (constant -8))
		;;Do the call.  The return value is discarded.
		(non-tail-call
		  (target:      #f)
		  (retval-var:  #f)
		  (args:        %eax %esi %esp %ebp %edi (nfv unset-conflicts))
		  (mask:        #f)
		  (size:        #f))))
	    ;;Load in AA-REGISTER the return value of this standalone expression.
	    (asm-instr move %eax (constant 16))
	    ;;Return to the caller.
	    (asmcall return %esi %esp %ebp %eax)))))

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
		;;Load in CP_0 the reference to closure object from the CP-REGISTER.
		(asm-instr move cp_0 %edi)
		;;Check if the PCB's engine counter has changed.
		(shortcut
		    (asmcall incr/zero? %esi (constant 72) (constant 8))
		  (non-tail-call-frame
		    (vars: #f)
		    (live: #f)
		    (seq
		      (asm-instr move tmp_0 (disp (constant (object $do-event))
						  (constant 19)))
		      (asm-instr move %edi tmp_0)
		      (asm-instr move %eax (constant 0))
		      (non-tail-call
			(target:      #f)
			(retval-var:  #f)
			(args:        %eax %esi %esp %ebp %edi)
			(mask:        #f)
			(size:        #f)))))
		;;Load the reference to closure object.
		(asm-instr move tmp_1 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
		(asm-instr move tmp_2 tmp_1)
		;;Store the reference to closure object into CP-REGISTER.
		(asm-instr move %edi tmp_2)
		;;Store in  AA-REGISTER a fixnum  representing the negated  number of
		;;arguments: -2.
		(asm-instr move %eax (constant -16))
		;;Perform the tail-call.
		(asmcall direct-jump
			 (code-loc asmlabel:F:clambda:case-2)
			 %eax %esi %esp %ebp %edi fvar.1 fvar.2)))))
	 (locals
	  (local-vars: tmp_3 tmp_4 tmp_5)
	  (seq
	    ;;Check if the PCB's engine counter has changed.
	    (shortcut
		(asmcall incr/zero? %esi (constant 72) (constant 8))
	      (non-tail-call-frame
		(vars: #f)
		(live: #f)
		(seq
		  (asm-instr move tmp_3 (disp (constant (object $do-event))
					      (constant 19)))
		  (asm-instr move %edi tmp_3)
		  (asm-instr move %eax (constant 0))
		  (non-tail-call
		    (target:      #f)
		    (retval-var:  #f)
		    (args:        %eax %esi %esp %ebp %edi)
		    (mask:        #f)
		    (size:        #f)))))
	    ;;Load the reference to closure object.
	    (asm-instr move tmp_4 (constant (closure-maker (code-loc asmlabel:F:clambda) no-freevars)))
	    (asm-instr move tmp_5 tmp_4)
	    ;;Store the reference to closure object into CP-REGISTER.
	    (asm-instr move %edi tmp_5)
	    ;;Put on the stack the operands, in the correct order.
	    (asm-instr move fvar.1 (constant 8))
	    (asm-instr move fvar.2 (constant 16))
	    ;;Store  in  AA-REGISTER a  fixnum  representing  the negated  number  of
	    ;;arguments: -2.
	    (asm-instr move %eax (constant -16))
	    ;;Perform the tail-call.
	    (asmcall direct-jump
		     (code-loc asmlabel:F:clambda:case-2)
		     %eax %esi %esp %ebp %edi fvar.1 fvar.2)))))

  #t)


(parametrise ((check-test-name	'seal-frame-and-call))

  (check
      (%before-impose-eval-order
       (receive (code libs)
	   (expand-form-to-core-language '($seal-frame-and-call void)
					 (environment '(vicare)
						      '(vicare system $stack)))
	 code))
    => '(codes
	 ()
	 ;;Retrieve a reference to the closure object implementing VOID.
	 (bind ((tmp_0 (asmcall mref (constant (object void)) (constant 19))))
	   ;;Allocate a continuation object.
	   (bind ((kont_0 (asmcall alloc-no-hooks (constant 32) (constant 5))))
	     ;;Compute  the address  of  the word  on the  stack  holding the  return
	     ;;address to the underflow handler.
	     (bind ((base_0 (asmcall int+
				     (asmcall mref %esi (constant 24))
				     (constant -8))))
	       ;;Load  the return  address  to  the underflow  handler  into a  local
	       ;;variable.
	       (bind ((underflow-handler_0 (asmcall mref base_0 (constant 0))))
		 (seq
		   ;;Store the  continuation secondary tag  in the first word  of the
		   ;;continuation object.
		   (asmcall mset kont_0 (constant -5) (constant 31))
		   ;;Store  in  the continuation  object  the  current Frame  Pointer
		   ;;Register  as   address  to   go  back   to  when   resuming  the
		   ;;continuation.
		   (asmcall mset kont_0 (constant 3) %esp)
		   ;;Store   in  the   continuation  object   the  number   of  bytes
		   ;;representing the total size of the freezed stack frames.
		   (asmcall mset kont_0 (constant 11) (asmcall int- base_0 %esp))
		   ;;Prepend the new continuation object  to the linked list of "next
		   ;;process continuations" in the PCB.
		   (asmcall mset kont_0 (constant 19)
			    (asmcall mref %esi (constant 40)))
		   (asmcall mset %esi (constant 40) kont_0)
		   ;;The machine word referenced by the FPR is the new frame base for
		   ;;subsequent code  execution; store  the FPR in  the PCB  as frame
		   ;;base.
		   (asmcall mset %esi (constant 24) %esp)
		   (asmcall call-with-underflow-handler
			    underflow-handler_0 tmp_0 kont_0))))))))

;;; --------------------------------------------------------------------

  (check
      (%impose-eval-order
       (receive (code libs)
	   (expand-form-to-core-language '($seal-frame-and-call void)
					 (environment '(vicare)
						      '(vicare system $stack)))
	 code))
    => '(codes
	 ()
	 (locals
	  (local-vars: tmp_0 tmp_1 kont_0 base_0
		       underflow-handler_0 tmp_2 tmp_3 tmp-underfow-handler_0
		       tmp-kont-object_0 tmp-func_0)
	  (seq
	    ;;Retrieve a reference to the closure object implementing VOID.
	    (asm-instr move tmp_0 (disp (constant (object void)) (constant 19)))
	    ;;Make  sure  there  is  enough  room  in the  heap  nursery  for  a  new
	    ;;continuation object.
	    (shortcut
		(conditional
		    (asm-instr <= %ebp (disp %esi (constant 8)))
		    (asmcall nop) (asmcall interrupt))
	      (non-tail-call-frame
		(vars: (nfv unset-conflicts))
		(live: #f)
		(seq
		  (asm-instr move (nfv unset-conflicts) (constant 32))
		  (asm-instr move tmp_1 (constant (foreign-label "ik_collect")))
		  (asm-instr move %edi tmp_1)
		  (asm-instr move %eax (constant -8))
		  (non-tail-call
		    (target:      "ik_collect")
		    (retval-var:  #f)
		    (args:        %eax %esi %esp %ebp %edi (nfv unset-conflicts))
		    (mask:        #f)
		    (size:        #f)))))
	    ;;Store  in  a local  variable  the  Allocation Pointer  Register,  which
	    ;;represents the untagged pointer to the continuation object.
	    (asm-instr move kont_0 %ebp)
	    ;;Tag the pointer as continuation object.
	    (asm-instr logor kont_0 (constant 5))
	    ;;Increment the  Allocation Pointer Register  by the aligned size  of the
	    ;;continuation object.
	    (asm-instr int+ %ebp (constant 32))
	    ;;Compute the address of the word on the stack holding the return address
	    ;;to the underflow handler.
	    (asm-instr move base_0 (disp %esi (constant 24)))
	    (asm-instr int+ base_0 (constant -8))
	    ;;Load the return address to the underflow handler into a local variable.
	    (asm-instr move underflow-handler_0 (disp base_0 (constant 0)))
	    ;;Store  the  continuation  secondary  tag  in  the  first  word  of  the
	    ;;continuation object.
	    (asm-instr mset (disp kont_0 (constant -5)) (constant 31))
	    ;;Store in the continuation object  the current Frame Pointer Register as
	    ;;address to go back to when resuming the continuation.
	    (asm-instr mset (disp kont_0 (constant 3)) %esp)
	    ;;Store in the  continuation object the number of  bytes representing the
	    ;;total size of the freezed stack frames.
	    (asm-instr move tmp_2 base_0)
	    (asm-instr int- tmp_2 %esp)
	    (asm-instr mset (disp kont_0 (constant 11)) tmp_2)
	    ;;Prepend the new continuation object to the linked list of "next process
	    ;;continuations" in the PCB.
	    (asm-instr move tmp_3 (disp %esi (constant 40)))
	    (asm-instr mset (disp kont_0 (constant 19)) tmp_3)
	    (asm-instr mset (disp %esi (constant 40)) kont_0)
	    ;;The  machine word  referenced by  the  FPR is  the new  frame base  for
	    ;;subsequent code execution; store the FPR in the PCB as frame base.
	    (asm-instr mset (disp %esi (constant 24)) %esp)
	    ;;Store in a local variable the return address to the underflow handler.
	    (asm-instr move tmp-underfow-handler_0 underflow-handler_0)
	    ;;Store in a local variable the reference to continuation object.
	    (asm-instr move tmp-kont-object_0 kont_0)
	    ;;Store in a local variable the reference to closure object to call.
	    (asm-instr move tmp-func_0 tmp_0)
	    ;;Move IK_UNDERFLOW_HANDLER in its reserved slot the on the Scheme stack.
	    (asm-instr move fvar.1 tmp-underfow-handler_0)
	    ;;Move the the  reference to continuation object in its  reserved slot on
	    ;;the Scheme stack, as operand to the closure referenced by TMP-FUNC_0.
	    (asm-instr move fvar.2 tmp-kont-object_0)
	    ;;Load the reference to closure object in the CP-REGISTER.
	    (asm-instr move %edi tmp-func_0)
	    ;;Load in  the AA-REGISTER  a fixnum representing  the negated  number of
	    ;;operands: -1.
	    (asm-instr move %eax (constant -8))
	    ;;Decrement the FP-REGISTER so that it points to the underflow handler as
	    ;;return address.
	    (asm-instr int- %esp (constant 8))
	    ;;Jump to  the binary code entry  point by retrieving it  from the cloure
	    ;;object referenced by CP-REGISTER.
	    (asmcall indirect-jump %eax %edi %esi %esp %ebp fvar.1 fvar.2)))))

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
