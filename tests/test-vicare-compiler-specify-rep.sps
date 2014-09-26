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
  (prefix (vicare compiler)
	  compiler.))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare compiler pass: specify representation\n")

(compiler.optimize-level 2)
(compiler.source-optimizer-passes-count 2)
;;(compiler.cp0-effort-limit 50)
;;(compiler.cp0-size-limit   8)
(compiler.descriptive-labels #t)


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
  (let* ((D (compiler.recordize core-language-form))
	 (D (compiler.optimize-direct-calls D))
	 (D (compiler.optimize-letrec D))
	 ;;Source optimisation is skipped here to  make it easier to write meaningful
	 ;;code for debugging and inspection.
	 #;(D (compiler.source-optimize D))
	 (D (compiler.rewrite-references-and-assignments D))
	 (D (compiler.core-type-inference D))
	 (S (compiler.unparse-recordized-code/sexp D)))
    S))

(define (%before-specify-representation core-language-form)
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
	 (D (compiler.introduce-primcalls D))
	 (D (compiler.rewrite-freevar-references D))
	 (D (compiler.insert-engine-checks D))
	 (D (compiler.insert-stack-overflow-check D))
	 (S (compiler.unparse-recordized-code/sexp D)))
    S))

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
	 (D (compiler.introduce-primcalls D))
	 (D (compiler.rewrite-freevar-references D))
	 (D (compiler.insert-engine-checks D))
	 (D (compiler.insert-stack-overflow-check D))
	 (D (compiler.specify-representation D))
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
		(primcall nop)
		(primcall int+/overflow (constant 8) (constant 16)))
	    (funcall (primcall mref (constant (object error@fx+)) (constant 19))
	      (constant 8) (constant 16)))))

  ;;Notice how the return value of the SHORTCUT becomes the operand of DISPLAY.
  (doit* (display (fx+ 1 2))
	 (codes
	  ()
	  (funcall (primcall mref (constant (object display)) (constant 19))
	    (shortcut
		(seq
		  (primcall nop)
		  (primcall int+/overflow (constant 8) (constant 16)))
	      (funcall (primcall mref (constant (object error@fx+)) (constant 19))
		(constant 8) (constant 16))))))

;;; --------------------------------------------------------------------

  (doit* (fx- 1 2)
	 (codes
	  ()
	  (shortcut
	      (seq
		(primcall nop)
		(primcall int-/overflow (constant 8) (constant 16)))
	    (funcall (primcall mref (constant (object error@fx-)) (constant 19))
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
	  (primcall int* (constant 32) (constant 2))))

  ;;Here we  do care about  the overflow.   Here we do  now at compile-time  that the
  ;;operands are both fixnum, so there is no need to introduce type validation code.
  ;;
  ;;The internal CONDITIONAL  validates "b_0" as a fixnum; "7"  is the representation
  ;;of the machine  word #b111 which is the  bitmask used to isolate the  type tag of
  ;;fixnums  on  64-bit  platforms; the  type  tag  of  fixnums  is #b000  on  64-bit
  ;;platforms.  So:
  ;;
  ;;   (primcall = (primcall logand b_0 (constant 7)) (constant 0))
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
		  (conditional (primcall = (primcall logand b_0 (constant 7)) (constant 0))
		      (primcall nop)
		    (primcall interrupt))
		  (primcall int*/overflow (constant 2) b_0)))
	    (funcall
		(primcall mref (constant (object error@fx*)) (constant 19))
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
		(conditional (primcall u< %esp (primcall mref %esi (constant 32)))
		    (primcall interrupt)
		  (primcall nop))
	      (foreign-call "ik_stack_overflow"))
	    ;;Here we start the actual form implementation.
	    (bind ((tmp_0 (funcall (primcall mref (constant (object read)) (constant 19))))
		   (tmp_1 (funcall (primcall mref (constant (object read)) (constant 19)))))
	      (shortcut
		  (bind ((a_0 tmp_0)
			 (b_0 tmp_1))
		    (seq
		      ;;Validate a_0 as 64-bit fixnum.
		      (conditional (primcall = (primcall logand a_0 (constant 7)) (constant 0))
			  (primcall nop)
			(primcall interrupt))
		      ;;Validate b_0 as 64-bit fixnum.
		      (conditional (primcall = (primcall logand b_0 (constant 7)) (constant 0))
			  (primcall nop)
			(primcall interrupt))
		      ;;Perform the  product, by untagging (right-shifting)  only one
		      ;;operand.
		      (primcall int*/overflow a_0 (primcall sra b_0 (constant 3)))))
		;;If  an operand  is not  a finxum  or an  overflow occurs:  raise an
		;;exception.  The single primitive function "error@fx*" is called for
		;;both the causes  or error: first it validates  (again) the operands
		;;as fixnums, and if they are: it means the error is an overflow.
		(funcall (primcall mref (constant (object error@fx*)) (constant 19))
		  tmp_0 tmp_1))))))

;;; --------------------------------------------------------------------

  (doit* (fxdiv 6 3)
	 (codes
	  ()
	  (funcall (primcall mref (constant (object fxdiv)) (constant 19))
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
	       (conditional (primcall u< %esp (primcall mref %esi (constant 32)))
		   (primcall interrupt)
		 (primcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (bind ((tmp_0 (funcall (primcall mref (constant (object read)) (constant 19)))))
	     ;;If the operand is a pair...
	     (conditional (primcall =
				    (primcall logand tmp_0 (constant 7))
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
	 (primcall mref (constant (object (1 . 2))) (constant -1))))

  ;;Here it is known that the operand is a "T:pair".
  (doit ((primitive car) '(1 . 2))
	(codes
	 ()
	 (primcall mref (constant (object (1 . 2))) (constant -1))))

  ;;Here the operand is of unknown type.
  (doit ((primitive car) ((primitive read)))
	(codes
	 ()
	 (seq
	   (shortcut
	       (conditional (primcall u< %esp (primcall mref %esi (constant 32)))
		   (primcall interrupt)
		 (primcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (bind ((tmp_0 (funcall (primcall mref (constant (object read)) (constant 19)))))
	     (shortcut
		 (seq
		   ;;If the primary tag is the pair tag...
		   (conditional (primcall =
					  (primcall logand tmp_0 (constant 7))
					  (constant 1))
		       ;;... fine.
		       (primcall nop)
		     ;;... otherwise jump to the interrupt handler.
		     (primcall interrupt))
		   ;;Extract the car.
		   (primcall mref tmp_0 (constant -1)))
	       (funcall (primcall mref (constant (object car)) (constant 19))
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
	 (primcall vector-length
		   (known (constant #(1 2)) (T:vector T:non-false T:nonimmediate T:object)))))

  (doit ((primitive vector-length) '#(1 2))
	(codes
	 ()
	 (primcall mref (constant (object #(1 2))) (constant -5))))

  (doit ((primitive vector-length) ((primitive read)))
	(codes
	 ()
	 (seq
	   (shortcut
	       (conditional (primcall u< %esp (primcall mref %esi (constant 32)))
		   (primcall interrupt)
		 (primcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (bind ((tmp_0 (funcall (primcall mref (constant (object read)) (constant 19)))))
	     (shortcut
		 (seq
		   ;;If the operand is a tagged pointer tagged as vector...
		   (conditional (primcall = (primcall logand tmp_0 (constant 7)) (constant 5))
		       ;;... fine.
		       (primcall nop)
		     ;;... otherwise call the full core primitive function.
		     (primcall interrupt))
		   ;;Retrieve the first word.
		   (bind ((vec.len_0 (primcall mref tmp_0 (constant -5))))
		     (seq
		       ;;If the first word is a fixnum...
		       (conditional (primcall = (primcall logand vec.len_0 (constant 7)) (constant 0))
			   ;;... fine.
			   (primcall nop)
			 ;;... otherwise call the full core primitive function.
			 (primcall interrupt))
		       vec.len_0)))
	       ;;Interrupt handler: perform a full call to the primitive function and
	       ;;let it raise an exception if there is the need.
	       (funcall (primcall mref (constant (object vector-length)) (constant 19))
		 tmp_0))))))

  #t)


(parametrise ((check-test-name	'fix))

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
	       (primcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall
		 (primcall mref (constant (object $do-event)) (constant 19))))
	   (funcall (primcall mref (constant (object list)) (constant 27))
	     (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:a:clambda) no-freevars))))
	       tmp_0)
	     (bind ((tmp_1 (constant (closure-maker (code-loc asmlabel:b:clambda) no-freevars))))
	       tmp_1)
	     (bind ((tmp_2 (constant (closure-maker (code-loc asmlabel:c:clambda) no-freevars))))
	       tmp_2)))))

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
	 ((lambda (label: asmlabel:c:clambda) (cp_0) (primcall mref cp_0 (constant 5)))
	  (lambda (label: asmlabel:b:clambda) (cp_1) (primcall mref cp_1 (constant 5)))
	  (lambda (label: asmlabel:a:clambda) (cp_2) (primcall mref cp_2 (constant 5))))
	 (seq
	   (shortcut
	       (conditional (primcall u< %esp (primcall mref %esi (constant 32)))
		   (primcall interrupt)
		 (primcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (shortcut
	       (primcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall (primcall mref (constant (object $do-event)) (constant 19))))
	   (bind ((x_0 (funcall (primcall mref (constant (object read)) (constant 27)))))
	     (bind ((c_0 (primcall alloc (constant 48) (constant 3))))
	       (bind ((b_0 (primcall int+ c_0 (constant 16)))
		      (a_0 (primcall int+ c_0 (constant 32))))
		 (seq
		   (primcall mset c_0 (constant -3) (constant (code-loc asmlabel:c:clambda)))
		   (primcall mset c_0 (constant  5) x_0)
		   (primcall mset b_0 (constant -3) (constant (code-loc asmlabel:b:clambda)))
		   (primcall mset b_0 (constant  5) x_0)
		   (primcall mset a_0 (constant -3) (constant (code-loc asmlabel:a:clambda)))
		   (primcall mset a_0 (constant  5) x_0)
		   (funcall (primcall mref (constant (object list)) (constant 27))
		     a_0 b_0 c_0))))))))

  ;;Mixed combinator/non-combinator bindings.
  (doit (let ((x (read)))
	  (let ((a (lambda () '1))
		(b (lambda () '2))
		(c (lambda () x))
		(d (lambda () x)))
	    (list a b c)))
	(codes
	 ((lambda (label: asmlabel:d:clambda) (cp_0) (primcall mref cp_0 (constant 5)))
	  (lambda (label: asmlabel:c:clambda) (cp_1) (primcall mref cp_1 (constant 5)))
	  (lambda (label: asmlabel:b:clambda) (cp_2) (constant 16))
	  (lambda (label: asmlabel:a:clambda) (cp_3) (constant 8)))
	 (seq
	   (shortcut
	       (conditional (primcall u< %esp (primcall mref %esi (constant 32)))
		   (primcall interrupt)
		 (primcall nop))
	     (foreign-call "ik_stack_overflow"))
	   (shortcut
	       (primcall incr/zero? %esi (constant 72) (constant 8))
	     (funcall
		 (primcall mref (constant (object $do-event)) (constant 19))))
	   (bind ((x_0 (funcall (primcall mref (constant (object read)) (constant 27)))))
	     (bind ((d_0 (primcall alloc (constant 32) (constant 3))))
	       (bind ((c_0 (primcall int+ d_0 (constant 16))))
		 (seq
		   (primcall mset d_0 (constant -3) (constant (code-loc asmlabel:d:clambda)))
		   (primcall mset d_0 (constant  5) x_0)
		   (primcall mset c_0 (constant -3) (constant (code-loc asmlabel:c:clambda)))
		   (primcall mset c_0 (constant  5) x_0)
		   (funcall (primcall mref (constant (object list)) (constant 27))
		     (bind ((tmp_0 (constant (closure-maker (code-loc asmlabel:a:clambda) no-freevars))))
		       tmp_0)
		     (bind ((tmp_1 (constant (closure-maker (code-loc asmlabel:b:clambda) no-freevars))))
		       tmp_1)
		     c_0))))))))

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
