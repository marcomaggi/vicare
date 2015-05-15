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
(check-display "*** testing Vicare compiler pass: specify representation\n")

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

(define (%core-type-inference core-language-form)
  (let* ((D (compiler.pass-recordize core-language-form))
	 (D (compiler.pass-optimize-direct-calls D))
	 (D (compiler.pass-optimize-letrec D))
	 ;;Source optimisation is skipped here to  make it easier to write meaningful
	 ;;code for debugging and inspection.
	 #;(D (compiler.pass-source-optimize D))
	 (D (compiler.pass-rewrite-references-and-assignments D))
	 (D (compiler.pass-core-type-inference D))
	 (S (compiler.unparse-recordized-code/sexp D)))
    S))

(define (%before-specify-representation core-language-form)
  (let* ((D (compiler.pass-recordize core-language-form))
	 (D (compiler.pass-optimize-direct-calls D))
	 (D (compiler.pass-optimize-letrec D))
	 ;;Source optimisation is skipped here to  make it easier to write meaningful
	 ;;code for debugging and inspection.
	 #;(D (compiler.pass-source-optimize D))
	 (D (compiler.pass-rewrite-references-and-assignments D))
	 (D (compiler.pass-core-type-inference D))
	 (D (compiler.pass-sanitize-bindings D))
	 (D (compiler.pass-optimize-for-direct-jumps D))
	 (D (compiler.pass-insert-global-assignments D))
	 (D (compiler.pass-introduce-vars D))
	 (D (compiler.pass-introduce-closure-makers D))
	 (D (compiler.pass-optimize-combinator-calls/lift-clambdas D))
	 (D (compiler.pass-introduce-primitive-operation-calls D))
	 (D (compiler.pass-rewrite-freevar-references D))
	 (D (compiler.pass-insert-engine-checks D))
	 (D (compiler.pass-insert-stack-overflow-check D))
	 (S (compiler.unparse-recordized-code/sexp D)))
    S))

(define (%specify-representation core-language-form)
  (let* ((D (compiler.pass-recordize core-language-form))
	 (D (compiler.pass-optimize-direct-calls D))
	 (D (compiler.pass-optimize-letrec D))
	 ;;Source optimisation is skipped here to  make it easier to write meaningful
	 ;;code for debugging and inspection.
	 #;(D (compiler.pass-source-optimize D))
	 (D (compiler.pass-rewrite-references-and-assignments D))
	 (D (compiler.pass-core-type-inference D))
	 (D (compiler.pass-sanitize-bindings D))
	 (D (compiler.pass-optimize-for-direct-jumps D))
	 (D (compiler.pass-insert-global-assignments D))
	 (D (compiler.pass-introduce-vars D))
	 (D (compiler.pass-introduce-closure-makers D))
	 (D (compiler.pass-optimize-combinator-calls/lift-clambdas D))
	 (D (compiler.pass-introduce-primitive-operation-calls D))
	 (D (compiler.pass-rewrite-freevar-references D))
	 (D (compiler.pass-insert-engine-checks D))
	 (D (compiler.pass-insert-stack-overflow-check D))
	 (D (compiler.pass-specify-representation D))
	 (S (compiler.unparse-recordized-code/sexp D)))
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


(parametrise ((check-test-name	'fixnums))

  (doit* (fx+ 1 2)
	 (codes
	  ()
	  (shortcut
	      (seq
		(asmcall nop)
		(asmcall int+/overflow (constant 8) (constant 16)))
	    (funcall (asmcall mref (constant (object error@fx+)) (constant 19))
	      (constant 8) (constant 16)))))

  ;;Notice how the return value of the SHORTCUT becomes the operand of DISPLAY.
  (doit* (display (fx+ 1 2))
	 (codes
	  ()
	  (funcall (asmcall mref (constant (object display)) (constant 19))
	    (shortcut
		(seq
		  (asmcall nop)
		  (asmcall int+/overflow (constant 8) (constant 16)))
	      (funcall (asmcall mref (constant (object error@fx+)) (constant 19))
		(constant 8) (constant 16))))))

;;; --------------------------------------------------------------------

  (doit* (fx- 1 2)
	 (codes
	  ()
	  (shortcut
	      (seq
		(asmcall nop)
		(asmcall int-/overflow (constant 8) (constant 16)))
	    (funcall (asmcall mref (constant (object error@fx-)) (constant 19))
	      (constant 8) (constant 16)))))

;;; --------------------------------------------------------------------

  ;;This is an unsafe operation: it means we  do not check for overflow; we trust the
  ;;code writer to  have already verified that  overflow is not possible  or does not
  ;;matter.  So we use the assembly instruction "int*" rather than "int*/overflow".
  ;;
  ;;How do we multiply two fixnums A and B?  Naively we could:
  ;;
  ;;1. Untag A right shifting by 3  bits (on 64-bit platforms), obtaining the machine
  ;;   word A1.
  ;;
  ;;2. Untag B right shifting by 3  bits (on 64-bit platforms), obtaining the machine
  ;;   word B1.
  ;;
  ;;3. Multiply the machine words A1 and B1, obtaining the machine word C1.
  ;;
  ;;4.   Tag the  machine word  C1 by  left shifting  3 bits  (on 64-bit  platforms),
  ;;   obtaining as result the fixnum C.
  ;;
  ;;As expression:
  ;;
  ;;   C = ((A >> 3) * (B >> 3)) << 3
  ;;
  ;;which,  using exact  integer operations  and  remembering that  8 =  2^3, can  be
  ;;written:
  ;;
  ;;   C = ((A / 8) * (B / 8)) * 8 = A * (B / 8) = (A / 8) * B
  ;;
  ;;so the following expressions are equivalent and save some operations:
  ;;
  ;;   C = A * (B >> 3) = (A >> 3) * B
  ;;
  (doit* ($fx* 2 4)
	 (codes
	  ()
	  (asmcall int* (constant 32) (constant 2))))

  ;;Here we  do care about  the overflow.   Here we do  now at compile-time  that the
  ;;operands are both fixnum, so there is no need to introduce type validation code.
  ;;
  ;;The internal CONDITIONAL  validates "b_0" as a fixnum; "7"  is the representation
  ;;of the machine  word #b111 which is the  bitmask used to isolate the  type tag of
  ;;fixnums  on  64-bit  platforms; the  type  tag  of  fixnums  is #b000  on  64-bit
  ;;platforms.  So:
  ;;
  ;;   (asmcall = (asmcall logand b_0 (constant 7)) (constant 0))
  ;;
  ;;is true if  "b_0" has the 3  least significant bits set  to zero, and so  it is a
  ;;64-bit fixnum.
  ;;
  (doit* (fx* 2 4)
	 (codes
	  ()
	  (shortcut
	      (bind ((b_0 (constant 32)))
		(seq
		  (conditional (asmcall = (asmcall logand b_0 (constant 7)) (constant 0))
		      (asmcall nop)
		    (asmcall interrupt))
		  (asmcall int*/overflow (constant 2) b_0)))
	    (funcall
		(asmcall mref (constant (object error@fx*)) (constant 19))
	      (constant 16) (constant 32)))))

  ;;Here the type of the operands is unknown.
  ;;
  (doit* (fx* (read) (read))
	 (codes
	  ()
	  (seq
	    ;;Here we check if the use of Scheme stack has crossed the red line and a
	    ;;stack  enlargement  is  needed.   This is  the  implementation  of  the
	    ;;primitive operation "$stack-overflow-check".
	    (shortcut
		(conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		    (asmcall interrupt)
		  (asmcall nop))
	      (foreign-call "ik_stack_overflow"))
	    ;;Here we start the actual form implementation.
	    (bind ((tmp_0 (funcall (asmcall mref (constant (object read)) (constant 19))))
		   (tmp_1 (funcall (asmcall mref (constant (object read)) (constant 19)))))
	      (shortcut
		  (bind ((a_0 tmp_0)
			 (b_0 tmp_1))
		    (seq
		      ;;Validate a_0 as 64-bit fixnum.
		      (conditional (asmcall = (asmcall logand a_0 (constant 7)) (constant 0))
			  (asmcall nop)
			(asmcall interrupt))
		      ;;Validate b_0 as 64-bit fixnum.
		      (conditional (asmcall = (asmcall logand b_0 (constant 7)) (constant 0))
			  (asmcall nop)
			(asmcall interrupt))
		      ;;Perform the  product, by untagging (right-shifting)  only one
		      ;;operand.
		      (asmcall int*/overflow a_0 (asmcall sra b_0 (constant 3)))))
		;;If  an operand  is not  a finxum  or an  overflow occurs:  raise an
		;;exception.  The single primitive function "error@fx*" is called for
		;;both the causes  or error: first it validates  (again) the operands
		;;as fixnums, and if they are: it means the error is an overflow.
		(funcall (asmcall mref (constant (object error@fx*)) (constant 19))
		  tmp_0 tmp_1))))))

;;; --------------------------------------------------------------------

  (doit* (fxdiv 6 3)
	 (codes
	  ()
	  (funcall (asmcall mref (constant (object fxdiv)) (constant 19))
	    (constant 48) (constant 24))))

  #t)


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
	       (asmcall nop)
	       (asmcall int+/overflow (constant 8) (constant 16)))
	   (funcall (asmcall mref (constant (object +)) (constant 19))
	     (constant 8) (constant 16)))))

  ;;Notice how the return value of the SHORTCUT becomes the operand of DISPLAY.
  (doit* (display (+ 1 2))
	 (codes
	  ()
	  (funcall (asmcall mref (constant (object display)) (constant 19))
	    (shortcut
		(seq
		  (asmcall nop)
		  (asmcall int+/overflow (constant 8) (constant 16)))
	      (funcall (asmcall mref (constant (object +)) (constant 19))
		(constant 8) (constant 16))))))

  #t)


(parametrise ((check-test-name	'pairs))

  ;;Predicate application in V context.
  (doit ((primitive pair?) '(1 . 2))
	(codes ()
	       (constant 63)))

  ;;Predicate application in V context.
  (doit ((primitive pair?) '())
	(codes
	 ()
	 (constant 47)))

  ;;Predicate application in V context.
  (doit ((primitive pair?) ((primitive read)))
	(codes
	 ()
	 (seq
	   (shortcut
	       (conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		   (asmcall interrupt)
		 (asmcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (bind ((tmp_0 (funcall (asmcall mref (constant (object read)) (constant 19)))))
	     ;;If the operand is a pair...
	     (conditional (asmcall =
				   (asmcall logand tmp_0 (constant 7))
				   (constant 1))
		 ;;... return true.
		 (constant 63)
	       ;;... otherwise return false.
	       (constant 47))))))

  ;;Predicate application in P context.
  (doit (if ((primitive pair?) '(1 . 2))
	    '"yes"
	  '"no")
	(codes
	 ()
	 (conditional (constant #t)
	     (constant (object "yes"))
	   (constant (object "no")))))

  ;;Predicate application in P context.
  (doit (if ((primitive pair?) '())
	    '"yes"
	  '"no")
	(codes
	 ()
	 (conditional (constant #f)
	     (constant (object "yes"))
	   (constant (object "no")))))

;;; --------------------------------------------------------------------

  ;;NOTE The  second operand to  MREF is the  "offset" of the  car with respect  to a
  ;;tagged pointer referencing the pair; this offset untags the pointer.
  (doit ((primitive $car) '(1 . 2))
	(codes
	 ()
	 (asmcall mref (constant (object (1 . 2))) (constant -1))))

  ;;Here it is known that the operand is a "T:pair".
  (doit ((primitive car) '(1 . 2))
	(codes
	 ()
	 (asmcall mref (constant (object (1 . 2))) (constant -1))))

  ;;Here the operand is of unknown type.
  (doit ((primitive car) ((primitive read)))
	(codes
	 ()
	 (seq
	   (shortcut
	       (conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		   (asmcall interrupt)
		 (asmcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (bind ((tmp_0 (funcall (asmcall mref (constant (object read)) (constant 19)))))
	     (shortcut
		 (seq
		   ;;If the primary tag is the pair tag...
		   (conditional (asmcall =
					 (asmcall logand tmp_0 (constant 7))
					 (constant 1))
		       ;;... fine.
		       (asmcall nop)
		     ;;... otherwise jump to the interrupt handler.
		     (asmcall interrupt))
		   ;;Extract the car.
		   (asmcall mref tmp_0 (constant -1)))
	       (funcall (asmcall mref (constant (object car)) (constant 19))
		 tmp_0))))))

  #t)


(parametrise ((check-test-name	'vectors))

  (check
      (%core-type-inference '((primitive vector-length) '#(1 2)))
    => '(funcall (primref vector-length)
	  (known (constant #(1 2)) (T:vector T:non-false T:nonimmediate T:object))))

  (check
      (%before-specify-representation '((primitive vector-length) '#(1 2)))
    => '(codes
	 ()
	 (primopcall vector-length
		     (known (constant #(1 2)) (T:vector T:non-false T:nonimmediate T:object)))))

  (doit ((primitive vector-length) '#(1 2))
	(codes
	 ()
	 (asmcall mref (constant (object #(1 2))) (constant -5))))

  (doit ((primitive vector-length) ((primitive read)))
	(codes
	 ()
	 (seq
	   (shortcut
	       (conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		   (asmcall interrupt)
		 (asmcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (bind ((tmp_0 (funcall (asmcall mref (constant (object read)) (constant 19)))))
	     (shortcut
		 (seq
		   ;;If the operand is a tagged pointer tagged as vector...
		   (conditional (asmcall = (asmcall logand tmp_0 (constant 7)) (constant 5))
		       ;;... fine.
		       (asmcall nop)
		     ;;... otherwise call the full core primitive function.
		     (asmcall interrupt))
		   ;;Retrieve the first word.
		   (bind ((vec.len_0 (asmcall mref tmp_0 (constant -5))))
		     (seq
		       ;;If the first word is a fixnum...
		       (conditional (asmcall = (asmcall logand vec.len_0 (constant 7)) (constant 0))
			   ;;... fine.
			   (asmcall nop)
			 ;;... otherwise call the full core primitive function.
			 (asmcall interrupt))
		       vec.len_0)))
	       ;;Interrupt handler: perform a full call to the primitive function and
	       ;;let it raise an exception if there is the need.
	       (funcall (asmcall mref (constant (object vector-length)) (constant 19))
		 tmp_0))))))

  #t)


(parametrise ((check-test-name	'fix))

  (doit (let ((f (lambda () '1)))
	  f)
	(codes
	 ((lambda (label: asmlabel:f:clambda) (cp_0) (constant 8)))
	 (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:f:clambda) no-freevars))))
	   tmp_0)))

  (doit (let ((f (lambda () '1)))
	  (f))
	(codes
	 ((lambda (label: asmlabel:f:clambda) (cp_0) (constant 8)))
	 (seq
	   (shortcut
	       (asmcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	   (jmpcall asmlabel:f:clambda:case-0
		    (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:f:clambda) no-freevars))))
		      tmp_0)))))

  ;;All combinator bindings.
  (doit (let ((a (lambda () '1))
	      (b (lambda () '2))
	      (c (lambda () '3)))
	  (list a b c))
	(codes
	 ((lambda (label: asmlabel:c:clambda) (cp_0) (constant 24))
	  (lambda (label: asmlabel:b:clambda) (cp_1) (constant 16))
	  (lambda (label: asmlabel:a:clambda) (cp_2) (constant 8)))
	 (seq
	   (shortcut
	       (asmcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall
		 (asmcall mref (constant (object $do-event)) (constant 19))))
	   (funcall (asmcall mref (constant (object list)) (constant 27))
	     (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:a:clambda) no-freevars))))
	       tmp_0)
	     (bind ((tmp_1 (constant (closure-maker (code-loc asmlabel:b:clambda) no-freevars))))
	       tmp_1)
	     (bind ((tmp_2 (constant (closure-maker (code-loc asmlabel:c:clambda) no-freevars))))
	       tmp_2)))))

;;; --------------------------------------------------------------------

  ;;All  non-combinator  bindings.  Every  non-combinator  has  1 free  variable;  we
  ;;allocate a single memory block for the 3 closure objects:
  ;;
  ;;    code entry  freevar   code entry  freevar    code entry  freevar
  ;;    point C_0   slot C_0  point B_0   slot B_0  point A_0   slot A_0
  ;;   |----------|----------|----------|----------|----------|----------|
  ;;
  ;;
  (doit (let ((x (read)))
	  (let ((a (lambda () x))
		(b (lambda () x))
		(c (lambda () x)))
	    (list a b c)))
	(codes
	 ((lambda (label: asmlabel:c:clambda) (cp_0) (asmcall mref cp_0 (constant 5)))
	  (lambda (label: asmlabel:b:clambda) (cp_1) (asmcall mref cp_1 (constant 5)))
	  (lambda (label: asmlabel:a:clambda) (cp_2) (asmcall mref cp_2 (constant 5))))
	 (seq
	   (shortcut
	       (conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		   (asmcall interrupt)
		 (asmcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (shortcut
	       (asmcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall (asmcall mref (constant (object $do-event)) (constant 19))))
	   (bind ((x_0 (funcall (asmcall mref (constant (object read)) (constant 27)))))
	     (bind ((c_0 (asmcall alloc (constant 48) (constant 3))))
	       (bind ((b_0 (asmcall int+ c_0 (constant 16)))
		      (a_0 (asmcall int+ c_0 (constant 32))))
		 (seq
		   (asmcall mset c_0 (constant -3) (constant (code-loc asmlabel:c:clambda)))
		   (asmcall mset c_0 (constant  5) x_0)
		   (asmcall mset b_0 (constant -3) (constant (code-loc asmlabel:b:clambda)))
		   (asmcall mset b_0 (constant  5) x_0)
		   (asmcall mset a_0 (constant -3) (constant (code-loc asmlabel:a:clambda)))
		   (asmcall mset a_0 (constant  5) x_0)
		   (funcall (asmcall mref (constant (object list)) (constant 27))
		     a_0 b_0 c_0))))))))

  ;;Mixed combinator/non-combinator bindings.
  (doit (let ((x (read)))
	  (let ((a (lambda () '1))
		(b (lambda () '2))
		(c (lambda () x))
		(d (lambda () x)))
	    (list a b c)))
	(codes
	 ((lambda (label: asmlabel:d:clambda) (cp_0) (asmcall mref cp_0 (constant 5)))
	  (lambda (label: asmlabel:c:clambda) (cp_1) (asmcall mref cp_1 (constant 5)))
	  (lambda (label: asmlabel:b:clambda) (cp_2) (constant 16))
	  (lambda (label: asmlabel:a:clambda) (cp_3) (constant 8)))
	 (seq
	   (shortcut
	       (conditional (asmcall u< %esp (asmcall mref %esi (constant 32)))
		   (asmcall interrupt)
		 (asmcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (shortcut
	       (asmcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall
		 (asmcall mref (constant (object $do-event)) (constant 19))))
	   (bind ((x_0 (funcall (asmcall mref (constant (object read)) (constant 27)))))
	     (bind ((d_0 (asmcall alloc (constant 32) (constant 3))))
	       (bind ((c_0 (asmcall int+ d_0 (constant 16))))
		 (seq
		   (asmcall mset d_0 (constant -3) (constant (code-loc asmlabel:d:clambda)))
		   (asmcall mset d_0 (constant  5) x_0)
		   (asmcall mset c_0 (constant -3) (constant (code-loc asmlabel:c:clambda)))
		   (asmcall mset c_0 (constant  5) x_0)
		   (funcall (asmcall mref (constant (object list)) (constant 27))
		     (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:a:clambda) no-freevars))))
		       tmp_0)
		     (bind ((tmp_1 (constant (closure-maker (code-loc asmlabel:b:clambda) no-freevars))))
		       tmp_1)
		     c_0))))))))

  #t)


(parametrise ((check-test-name			'debug-call)
	      (compiler.generate-debug-calls	#t))

  ;;The core  primitive function READ  is called with  an ordinary function  call, no
  ;;SHORTCUT.
  (doit (annotated-call (read) (primitive read))
	(codes
	 ()
	 (funcall (asmcall mref (constant (object debug-call)) (constant 19))
	   (constant (object (#f . (read))))
	   (asmcall mref (constant (object read)) (constant 19)))))

  ;;The core primitive operation/function LIST cannot fail.
  (doit (annotated-call (list 1 2) (primitive list) '1 '2)
	(codes
	 ()
	 (bind ((first-pair_0 (asmcall alloc (constant 32) (constant 1))))
	   (seq
	     (asmcall mset first-pair_0 (constant -1) (constant 8))
	     (asmcall mset first-pair_0 (constant 23) (constant 79))
	     (bind ((tmp_0 (asmcall int+ first-pair_0 (constant 16))))
	       (seq
		 (asmcall mset tmp_0 (constant -1) (constant 16))
		 (asmcall mset tmp_0 (constant -9) tmp_0)
		 first-pair_0))))))

  ;;The  core  primitive  operation/function  FX+ can  fail  with  special  interrupt
  ;;handler.   When  debugging mode  is  enabled:  rather  than calling  the  special
  ;;interrupt handler, we call the full core primitive function.
  (doit (annotated-call (fx+ 1 2) (primitive fx+) '1 '2)
	(codes
	 ()
	 (shortcut
	     (seq
	       (asmcall nop)
	       (asmcall int+/overflow (constant 8) (constant 16)))
	   (funcall (asmcall mref (constant (object debug-call)) (constant 19))
	     (constant (object (#f . (fx+ 1 2))))
	     (asmcall mref (constant (object fx+)) (constant 19))
	     (constant 8)
	     (constant 16)))))

  ;;The core primitive  operation/function + can fail with  "function call" interrupt
  ;;handler.
  (doit (annotated-call (+ 1 2) (primitive +) '1 '2)
	(codes
	 ()
	 (shortcut
	     (seq
	       (asmcall nop)
	       (asmcall int+/overflow (constant 8) (constant 16)))
	   (funcall (asmcall mref (constant (object debug-call)) (constant 19))
	     (constant (object (#f . (+ 1 2))))
	     (asmcall mref (constant (object +)) (constant 19))
	     (constant 8)
	     (constant 16)))))

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
