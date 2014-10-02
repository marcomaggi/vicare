;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(module (impose-calling-convention/evaluation-order)
  ;;This module does stuff:
  ;;
  ;;*  All the  BIND  struct instances  in  the input  expression  are processed  and
  ;;substituted with code that evaluates the  RHS expressions and stores their single
  ;;return value into appropriately allocated Scheme stack machine words.  Here it is
  ;;decided in which order the RHS expressions are computed.
  ;;
  ;;*  All the  FUNCALL  struct  instances in  the  input  expression representing  a
  ;;function call:
  ;;
  ;;   (funcall (asmcall mref
  ;;                    (constant (object ?loc))
  ;;                    (constant ?off-symbol-record-proc))
  ;;            (?rand ...))
  ;;
  ;;are converted to:
  ;;
  ;;   (bind ((tmp ?rand) ...)
  ;;     (funcall (asmcall mref
  ;;                      (constant (object ?loc))
  ;;                      (constant ?off-symbol-record-proc))
  ;;              (tmp ...)))
  ;;
  ;;so that the order of evaluation of the operands' expressions is decided.
  ;;
  ;;*  All the  ASMCALL  struct  instances in  the  input  expression representing  a
  ;;high-level Assembly instruction:
  ;;
  ;;   (asmcall ?instr (?rand ...))
  ;;
  ;;are converted to the equivalent of:
  ;;
  ;;   (bind ((tmp ?rand) ...)
  ;;     (asmcall ?instr (tmp ...)))
  ;;
  ;;so that the order of evaluation of the operands' expressions is decided.
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   bind		conditional		constant
  ;;   forcall		funcall			jmpcall
  ;;   asmcall		seq
  ;;   shortcut		var
  ;;
  ;;in addition CLOSURE-MAKER structs can appear in side CONSTANT structs.
  ;;
  (import INTEL-ASSEMBLY-CODE-GENERATION)

  (define-syntax __module_who__
    (identifier-syntax 'impose-calling-convention/evaluation-order))

  (define (impose-calling-convention/evaluation-order codes)
    (V-codes codes))


;;;; helpers

(define (var/nfv? x)
  (or (var? x)
      (nfv? x)))

(define-syntax-rule (%move-dst<-src ?dst ?src)
  (make-asm-instr 'move ?dst ?src))


;;;; local values
;;
;;Some Assembly instructions generate result values  that must be stored somewhere to
;;be consumed  later; to represent  such machine words  we use VAR  structs.  Ideally
;;such temporary values are all stored in  CPU registers, but sometimes there are not
;;enough registers and we need to spill them on the Scheme stack.
;;
;;We usually  use the BIND  struct to represent the  need to allocate  such temporary
;;location.
;;
;;For every standalone expression  and body of CLAMBDA clause we  collect the list of
;;VAR structs representing such locations and store  it in the VARS field of a LOCALS
;;struct; we use the parameter LOCAL-VALUES to accumulate the list.
;;

(define local-values
  (make-parameter #f))

(define-syntax-rule (%local-value-cons ?A)
  (local-values (cons ?A (local-values))))

(define-syntax-rule (%local-value-cons* ?A0 ?A ...)
  (local-values (cons* ?A0 ?A ... (local-values))))


;;;; processing CODES structs

(module (V-codes)

  (define (V-codes x)
    ;;X must be a CODES struct:
    ;;
    ;;   (codes
    ;;     ((clambda ?label
    ;;        (?clause ...)
    ;;        ---)
    ;;      ...)
    ;;     ?body)
    ;;
    ;;Return a CODES struct  in which the init body and each  CLAMBDA clause body are
    ;;wrapped into a LOCALS struct:
    ;;
    ;;   (codes
    ;;     ((clambda ?label
    ;;        ((locals ?local-vars ?clause) ...)
    ;;        ---)
    ;;      ...)
    ;;     (locals ?local-vars ?body))
    ;;
    (struct-case x
      ((codes x.clambda* x.body)
       (make-codes (map V-clambda x.clambda*) (V-body x.body)))))

  (define (V-body x)
    (parametrise ((local-values '()))
      (let ((y (V-tail x)))
	(make-locals (local-values) y))))

  (module (V-clambda)

    (define (V-clambda x)
      (struct-case x
	((clambda x.label x.clause* x.cp x.freevar* x.name)
	 (make-clambda x.label (map V-clambda-clause x.clause*) x.cp x.freevar* x.name))))

    (define (V-clambda-clause cas)
      ;;This function has two purposes: apply "V-tail" to the body of the clause;
      ;;
      (struct-case cas
	((clambda-case cas.info cas.body)
	 (struct-case cas.info
	   ((case-info cas.info.label cas.info.args cas.info.proper)
	    ;;Remember that  CAS.INFO.ARGS is a proper  list of VAR structs  with the
	    ;;format:
	    ;;
	    ;;   (?cpvar ?arg ...)
	    ;;
	    ;;where: ?CPVAR represents a machine word that must hold a pointer to the
	    ;;closure object;  each ?ARG represents a  machine word that must  hold a
	    ;;CLAMBDA clause's argument.
	    (receive (register-args register-names stack-args stack-locations)
		(%partition-formals PARAMETER-REGISTERS cas.info.args)
	      ;;The arguments listed in REGISTER-ARGS will be stored in the registers
	      ;;listed in REGISTER-NAMES.  The arguments listed in STACK-ARGS will be
	      ;;stored in the Scheme stack machine words listed in STACK-LOCATIONS.
	      (parametrise ((local-values register-args))
		($for-each/stx set-var-loc! stack-args stack-locations)
		(let ((body (let recur ((args register-args)
					(locs register-names))
			      (if (pair? args)
				  (make-seq
				    ;;Load a special parameter  from the CPU register
				    ;;into the locally allocated VAR.
				    (%move-dst<-src (car args) (car locs))
				    (recur          (cdr args) (cdr locs)))
				(V-tail cas.body)))))
		  (make-clambda-case
		   (make-case-info cas.info.label (append register-names stack-locations) cas.info.proper)
		   (make-locals (local-values) body))))))))))

    (define (%partition-formals available-registers formals)
      ;;Recursive function.  Associate  the formals of a CLAMBDA  clause to available
      ;;CPU registers.
      ;;
      ;;The  argument AVAILABLE-REGISTERS  must  be a  list  of symbols  representing
      ;;available CPU  registers.  The  argument FORMALS  must be  a list  of CLAMBDA
      ;;clause's formals.
      ;;
      ;;Return 4 values:
      ;;
      ;;1.   The list  of lex  gensyms  representing formal  arguments associated  to
      ;;   available registers.
      ;;
      ;;2.   The list  of symbols  representing register  names associated  to formal
      ;;   arguments.
      ;;
      ;;3. The list  of lex gensyms representing formal arguments  associated to FVAR
      ;;   structures.
      ;;
      ;;4. The list of FVAR structures associated to formals.
      ;;
      (cond ((null? available-registers)
	     ;;If  the number  of formals  is <=  of the  number of  registers: the
	     ;;left-over  registers  are  associated   to  FVAR  structures,  which
	     ;;represent Scheme stack machine words.
	     (let ((stack-locations (%one-fvar-for-each-left-over-formal 1 formals)))
	       (values '() '() formals stack-locations)))
	    ((null? formals)
	     ;;If there are more registers than formals: fine.
	     (values '() '() '() '()))
	    (else
	     ;;If there is a register for the next formal: associate them.
	     (receive (register-args register-names stack-args stack-locations)
		 (%partition-formals (cdr available-registers) (cdr formals))
	       (values (cons (car formals)             register-args)
		       (cons (car available-registers) register-names)
		       stack-args stack-locations)))))

    (define (%one-fvar-for-each-left-over-formal i leftover-formal)
      (if (pair? leftover-formal)
	  (cons (mkfvar i)
		(%one-fvar-for-each-left-over-formal (fxadd1 i) (cdr leftover-formal)))
	'()))

    #| end of module: V-clambda |# )

  #| end of module: V-codes |# )


(module (V-tail)

  (define (V-tail x)
    (struct-case x

      ((constant)
       (VT x))

      ((var)
       (VT x))

      ((asmcall op rands)
       (case op
	 ((call-with-underflow-handler)
	  ;;This    primitive    is    used   by    the    primitive    operation
	  ;;$SEAL-FRAME-AND-CALL  to implement  the heart  of CALL/CC  (call with
	  ;;current  continuation) and  CALL/CF (call  with current  frame), file
	  ;;"ikarus.control.sls".   Let's super  simplify  and  comment the  code
	  ;;starting with  the call to  %PRIMITIVE-CALL/CF which is the  heart of
	  ;;both CALL/CC and CALL/CF.
	  ;;
	  ;;Remember that:
	  ;;
	  ;;* FPR stands for Frame Pointer Register;
	  ;;
	  ;;*  PCR stands  for Process  Control  Register and  it references  the
	  ;;structure PCB defined at the C language level;
	  ;;
	  ;;*  CPR stands  for Closure  Pointer Register  and it  must contain  a
	  ;;reference to the closure object being executed.
	  ;;
	  ;;* ARGC-REGISTER stands for Argument Count Register.
	  ;;
	  ;;When arriving here  the scenario of the Scheme stack  is the one left
	  ;;by $SEAL-FRAME-AND-CALL:
	  ;;
	  ;;         high memory
	  ;;   |                      |
	  ;;   |----------------------|
	  ;;   | ik_underflow_handler |
	  ;;   |----------------------|                           --
	  ;;     ... other frames ...                             .
	  ;;   |----------------------|                           .
	  ;;   |      local value     |                           . freezed
	  ;;   |----------------------|                           . frames
	  ;;   |      local value     |                           .
	  ;;   |----------------------|                           .
	  ;;   |     return address   | <- FPR = pcb->frame_base  .
	  ;;   |----------------------|                           --
	  ;;   |         func         | -> closure object
	  ;;   |----------------------|
	  ;;             ...
	  ;;   |----------------------|
	  ;;   |      free word       | <- pcb->stack_base
	  ;;   |----------------------|
	  ;;   |                      |
	  ;;          low memory
	  ;;
	  ;;ARGC-REGISTER contains the encoded  number of arguments, counting the
	  ;;single  argument FUNC  to %PRIMITIVE-CALL/CF.   The reference  to the
	  ;;just created  continuation object is  in some CPU register.   The raw
	  ;;memory pointer UNDERFLOW-HANDLER is in some CPU register.
	  ;;
	  ;;There are 3 operands in RANDS:
	  ;;
	  ;;*  A representation  of  the CPU  register  containing the  underflow
	  ;;handler:  a   raw  memory  address   equal  to  the   assembly  label
	  ;;"ik_underflow_handler".
	  ;;
	  ;;* A representation of the stack location containing FUNC.
	  ;;
	  ;;* A representation of the CPU  register containing a reference to the
	  ;;continuation   object   referencing   the   freezed   frames.    Such
	  ;;continuation object  is also the  "next process continuation"  in the
	  ;;PCB, that is: it is the value of the field "pcb->next_k".
	  ;;
	  (let ((t0			(make-unique-var 't))
		(t1			(make-unique-var 't))
		(t2			(make-unique-var 't))
		(underflow-handler	(car rands))
		(func		(cadr rands))
		(kont-object	(caddr rands)))
	    (%local-value-cons* t0 t1 t2)
	    (multiple-forms-sequence
	     ;;Copy the arguments in CPU registers.
	     (V t0 underflow-handler)
	     (V t1 kont-object)
	     (V t2 func)
	     ;;Move IK_UNDERFLOW_HANDLER in  its reserved slot the  on the Scheme
	     ;;stack.
	     (%move-dst<-src (mkfvar 1) t0)
	     ;;Move the the reference to continuation object in its reserved slog
	     ;;on the Scheme stack, as argument to THE-FUNC.
	     (%move-dst<-src (mkfvar 2) t1)
	     ;;When we arrive here the situation on the Scheme stack is:
	     ;;
	     ;;         high memory
	     ;;   |                      |
	     ;;   |----------------------|
	     ;;   | ik_underflow_handler |
	     ;;   |----------------------|                           --
	     ;;     ... other frames ...                             .
	     ;;   |----------------------|                           .
	     ;;   |      local value     |                           . freezed
	     ;;   |----------------------|                           . frames
	     ;;   |      local value     |                           .
	     ;;   |----------------------|                           .
	     ;;   |     return address   | <- FPR = pcb->frame_base  .
	     ;;   |----------------------|                           --
	     ;;   | ik_underflow_handler |
	     ;;   |----------------------|
	     ;;   |         kont         | -> continuation object
	     ;;   |----------------------|
	     ;;             ...
	     ;;   |----------------------|
	     ;;   |      free word       | <- pcb->stack_base
	     ;;   |----------------------|
	     ;;   |                      |
	     ;;          low memory
	     ;;
	     ;;Load the reference to closure object FUNC in the CPR.
	     (%move-dst<-src cpr t2)
	     ;;Load in  ARGC-REGISTER the encoded  number of arguments,  counting the
	     ;;continuation object.
	     (%notify-number-of-operands 1)
	     ;;Decrement the FPR so that it points to the underflow handler.
	     (make-asm-instr 'int- fpr (make-constant wordsize))
	     ;;When we arrive here the situation on the Scheme stack is:
	     ;;
	     ;;         high memory
	     ;;   |                      |
	     ;;   |----------------------|
	     ;;   | ik_underflow_handler |
	     ;;   |----------------------|                     --
	     ;;     ... other frames ...                       .
	     ;;   |----------------------|                     .
	     ;;   |      local value     |                     . freezed
	     ;;   |----------------------|                     . frames
	     ;;   |      local value     |                     .
	     ;;   |----------------------|                     .
	     ;;   |     return address   | <- pcb->frame_base  .
	     ;;   |----------------------|                     --
	     ;;   | ik_underflow_handler | <- FPR
	     ;;   |----------------------|
	     ;;   |         kont         | -> continuation object
	     ;;   |----------------------|
	     ;;             ...
	     ;;   |----------------------|
	     ;;   |      free word       | <- pcb->stack_base
	     ;;   |----------------------|
	     ;;   |                      |
	     ;;          low memory
	     ;;
	     ;;The following INDIRECT-JUMP compiles to a single "jmp" instruction
	     ;;that  jumps  to  the  machine  code entry  point  in  the  closure
	     ;;referenced by  the CPR, which is  FUNC.  By doing a  "jmp", rather
	     ;;than a  "call", we avoid  pushing a  return address on  the Scheme
	     ;;stack.
	     ;;
	     ;;Notice that the stack frame of FUNC starts with the argument KONT.
	     ;;The  IK_UNDERFLOW_HANDLER we  have  put on  the  stack does  *not*
	     ;;belong to any stack frame.
	     ;;
	     ;;If the closure FUNC returns  without calling a continuation escape
	     ;;function: it will return to  the underflow handler; such underflow
	     ;;handler must  pop the  continuation object from  "pcb->next_k" and
	     ;;process it as explained in the documentation.
	     ;;
	     (make-asmcall 'indirect-jump
	       (list ARGC-REGISTER cpr pcr esp apr (mkfvar 1) (mkfvar 2))))))
	 (else
	  (VT x))))

      ((bind lhs* rhs* e)
       (%do-bind lhs* rhs* (V-tail e)))

      ((seq e0 e1)
       (make-seq (E e0) (V-tail e1)))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (V-tail e1) (V-tail e2)))

      ((funcall rator rands)
       (%handle-tail-call #f rator rands))

      ((jmpcall label rator rands)
       (%handle-tail-call (make-code-loc label) rator rands))

      ((forcall)
       (VT x))

      ((shortcut body handler)
       (make-shortcut (V-tail body) (V-tail handler)))

      (else
       (compiler-internal-error __module_who__ "invalid tail" x))))

  (define (VT x)
    ;;X is a struct of type: CONSTANT, VAR, ASMCALL, FORCALL.
    ;;
    (import OPERANDS-SIMPLIFICATION)
    (S x
      (lambda (x)
	(make-seq
	  (%move-dst<-src RETURN-VALUE-REGISTER x)
	  (make-asmcall 'return (list pcr esp apr RETURN-VALUE-REGISTER))))))

  #| end of module: V-tail |# )


;;;; helpers

(define (%notify-number-of-operands num-of-rands)
  ;;Store in the AA-REGISTER a fixnum representing the negated number of operands.
  (%move-dst<-src ARGC-REGISTER (make-constant (argc-convention num-of-rands))))

(define (%assign* lhs* rhs* tail-body)
  ;;Non-tail recursive  function.  Given a list  of destination locations LHS*  and a
  ;;list of source expressions RHS*, build and return a struct instance representing:
  ;;
  ;;   (seq
  ;;     (asm-instr move ?lhs ?rhs)
  ;;     ...
  ;;     ?tail-body)
  ;;
  (if (pair? lhs*)
      (begin
	;; (assert (let ((lhs (car lhs*)))
	;; 	  (or (and (symbol? lhs)
	;; 		   (memq lhs PARAMETER-REGISTERS))
	;; 	      (fvar? lhs))))
	(make-seq
	  (%move-dst<-src (car lhs*) (car rhs*))
	  (%assign*       (cdr lhs*) (cdr rhs*) tail-body)))
    tail-body))

(define (%do-bind lhs* rhs* tail-body)
  ;;Non-tail recursive function.   This function is like %ASSIGN*,  but, in addition:
  ;;it registers the LHS* as local variables and filters the RHS* through V.
  ;;
  (if (pair? lhs*)
      (begin
	(%local-value-cons (car lhs*))
	(make-seq
	  ;;Generate assembly instructions  to compute a value from  "(car rhs*)" and
	  ;;store the result in destination "(car lhs*)".
	  (V        (car lhs*) (car rhs*))
	  (%do-bind (cdr lhs*) (cdr rhs*)
	    tail-body)))
    tail-body))


(module OPERANDS-SIMPLIFICATION
  (S S*)

  (define (S* x* kont)
    (if (pair? x*)
	(S (car x*)
	   (lambda (a)
	     (S* (cdr x*)
		 (lambda (d)
		   (kont (cons a d))))))
      (kont '())))

  (define (S x kont)
    (struct-case x
      ((bind lhs* rhs* body)
       (%do-bind lhs* rhs* (S body kont)))
      ((seq e0 e1)
       (make-seq (E e0) (S e1 kont)))
      (else
       (cond ((or (constant? x)
		  (symbol?   x))
	      (kont x))
	     ((var? x)
	      (cond ((var-loc x)
		     => kont)
		    (else
		     (kont x))))
	     ((or (funcall? x) (asmcall? x) (jmpcall? x)
		  (forcall? x) (shortcut? x) (conditional? x))
	      (let ((t (make-unique-var 'tmp)))
		(%do-bind (list t) (list x) (kont t))))
	     (else
	      (compiler-internal-error __module_who__ "invalid S" x))))))

  #| end of module: OPERANDS-SIMPLIFICATION |# )


(module (V)

  (define* (V {dst var/nfv?} x)
    ;;Generate assembly instructions  to compute a value from struct  X and store the
    ;;result in destination DST.
    ;;
    (struct-case x
      ((constant)
       (%move-dst<-src dst x))

      ((var)
       (cond ((var-loc x)
	      ;;X is an argument to function call  already stored on the stack in the
	      ;;FVAR stored in the LOC field.
	      => (lambda (loc)
		   ;;(assert (fvar? loc))
		   (%move-dst<-src dst loc)))
	     (else
	      (%move-dst<-src dst x))))

      ((bind lhs* rhs* body)
       (%do-bind lhs* rhs* (V dst body)))

      ((seq e0 e1)
       (make-seq (E e0) (V dst e1)))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (V dst e1) (V dst e2)))

      ((asmcall op rands)
       (V-asmcall x dst op rands))

      ((funcall rator rand*)
       (let ((target #f))
	 (%handle-non-tail-call rator rand* dst target)))

      ((jmpcall asmlabel rator rand*)
       (%handle-non-tail-call rator rand* dst asmlabel))

      ((forcall cfunc-name.str rand*)
       (let ((rator (make-constant (make-foreign-label cfunc-name.str))))
	 (%handle-non-tail-call rator rand* dst cfunc-name.str)))

      ((shortcut body handler)
       (make-shortcut
	   (V dst body)
	 (V dst handler)))

      (else
       (if (symbol? x)
	   (%move-dst<-src dst x)
	 (compiler-internal-error __module_who__
	   "invalid recordised code in V context" (unparse-recordized-code/sexp x))))))

;;; --------------------------------------------------------------------

  (define (V-asmcall x dst op rand*)
    (import OPERANDS-SIMPLIFICATION)
    (case op
      ((alloc)
       ;;Allocate a Scheme object on the heap.  We expect X to have the format:
       ;;
       ;;   (asmcall alloc (?aligned-memory-block-size ?scheme-object-primary-tag))
       ;;
       ;;First check if there is enough room on the heap segment:
       ;;
       ;;* If  there is:  just increment  the Allocation  Pointer Register  (APR) and
       ;;  return the old APR value.
       ;;
       ;;* If  there is  not: run  a garbage collection  (complete with  execution of
       ;;  post-GC hooks) by calling the function DO-OVERFLOW, then increment the APR
       ;;  and return the old APR after the GC.
       ;;
       (S (car rand*)
	 (lambda (aligned-size)
	   (make-seq
	     (alloc-check aligned-size)
	     (S (cadr rand*)
	       (lambda (primary-tag)
		 (multiple-forms-sequence
		   ;;Load in DST the value in the Allocation Pointer Register: this
		   ;;value is  a pointer to  a usable block  of memory on  the heap
		   ;;nursery.
		   (%move-dst<-src dst apr)
		   ;;Add the tag to the pointer.
		   (make-asm-instr 'logor dst primary-tag)
		   ;;Increment the Allocation Pointer  Register by the aligned size
		   ;;of the block.
		   (make-asm-instr 'int+ apr aligned-size))))))))

      ((alloc-no-hooks)
       ;;This is  like ALLOC,  but, if there  is the need,  run a  garbage collection
       ;;without executing the post-GC hooks.
       ;;
       ;;This simpler GC run does not touch the Scheme stack, avoiding the generation
       ;;of    corrupt   continuation    objects   by    the   primitive    operation
       ;;$SEAL-FRAME-AND-CALL (which was a cause of issue #35).
       ;;
       ;;$SEAL-FRAME-AND-CALL should  be the only  operation making use of  this heap
       ;;allocation method.
       ;;
       (S (car rand*)
	 (lambda (aligned-size)
	   (make-seq
	     (alloc-check/no-hooks aligned-size)
	     (S (cadr rand*)
	       (lambda (primary-tag)
		 (multiple-forms-sequence
		   (%move-dst<-src dst apr)
		   (make-asm-instr 'logor dst primary-tag)
		   (make-asm-instr 'int+  apr aligned-size))))))))

      ((mref)
       ;;We expect X to have the format:
       ;;
       ;;   (asmcall mref (?operand-referencing-scheme-object ?offset))
       ;;
       (S* rand*
	 (lambda (rand*)
	   (%move-dst<-src dst (make-disp (car rand*) (cadr rand*))))))

      ((mref32)
       ;;We expect X to have the format:
       ;;
       ;;   (asmcall mref32 (?operand-referencing-scheme-object ?offset))
       ;;
       ;;MREF32 is used, for example, to extract single characters from a string.
       (S* rand*
	 (lambda (rand*)
	   (make-asm-instr 'load32 dst (make-disp (car rand*) (cadr rand*))))))

      ((bref)
       ;;We expect X to have the format:
       ;;
       ;;   (asmcall bref (?operand-referencing-scheme-objet ?offset))
       ;;
       ;;BREF is used, for example, to extract single bytes from a bytevector.
       (S* rand*
	 (lambda (rand*)
	   (make-asm-instr 'load8 dst (make-disp (car rand*) (cadr rand*))))))

      ((logand logxor logor int+ int- int* int-/overflow int+/overflow int*/overflow)
       ;;We expect X to have the format:
       ;;
       ;;   (asmcall ?op (?first-operand ?second-operand))
       ;;
       ;;representing a high-level Assembly instruction that must store the resulting
       ;;value in ?FIRST-OPERAND.
       (make-seq
	 ;;Load the first operand in DST.
	 ;;
	 ;;NOTE We do not filter "(car rand*)" through S here: we hand it to V, which
	 ;;takes care of filtering it.
	 (V dst (car rand*))
	 (S (cadr rand*)
	   (lambda (src)
	     ;;Perform the  operation OP between  the first  operand in DST  and the
	     ;;second operand in SRC; store the resulting value in DST.
	     (make-asm-instr op dst src)))))

      ((int-quotient)
       ;;We expect X to have the format:
       ;;
       ;;   (asmcall int-quotient (?first-operand ?second-operand))
       ;;
       (S* rand*
	 (lambda (rand*)
	   (multiple-forms-sequence
	     (%move-dst<-src eax (car rand*))
	     (make-asm-instr 'cltd edx eax)
	     (make-asm-instr 'idiv eax (cadr rand*))
	     (%move-dst<-src dst eax)))))

      ((int-remainder)
       ;;We expect X to have the format:
       ;;
       ;;   (asmcall int-remainder (?first-operand ?second-operand))
       ;;
       (S* rand*
	 (lambda (rand*)
	   (multiple-forms-sequence
	     (%move-dst<-src eax (car rand*))
	     (make-asm-instr 'cltd edx eax)
	     (make-asm-instr 'idiv edx (cadr rand*))
	     (%move-dst<-src dst edx)))))

      ((sll sra srl sll/overflow)
       ;;We expect X to have the format:
       ;;
       ;;   (asmcall ?op (?operand ?shift-amount))
       ;;
       ;;If the  ?SHIFT-AMOUNT is a  constant: the  Assembly instruction can  load it
       ;;directly.
       ;;
       ;;If the ?SHIFT-AMOUNT must be  computed at run-time: the Assembly instruction
       ;;expects it to be computed and the result loaded into ECX.
       (let ((operand      (car  rand*))
	     (shift-amount (cadr rand*)))
	 ;;NOTE We do not filter OPERAND through  S here: we hand OPERAND to V, which
	 ;;takes care of filtering it.
	 (if (constant? shift-amount)
	     (make-seq
	       (V dst operand)
	       (make-asm-instr op dst shift-amount))
	   (S shift-amount
	     (lambda (shift-amount)
	       (multiple-forms-sequence
		 (V dst operand)
		 (%move-dst<-src ecx shift-amount)
		 (make-asm-instr op dst ecx)))))))

      (else
       (compiler-internal-error __module_who__
	 "invalid ASMCALL operator in return value context"
	 (unparse-recordized-code/sexp x)))))

;;; --------------------------------------------------------------------

  (module (alloc-check alloc-check/no-hooks)

    (define (alloc-check aligned-size)
      (E (make-shortcut
	     (make-conditional (%test aligned-size)
		 (nop)
	       (interrupt))
	   (make-funcall
	    ;;From the relocation  vector of this code object:  retrieve the location
	    ;;gensym associated to DO-OVERFLOW, then retrieve the value of its "proc"
	    ;;slot.  The "proc"  slot of such loc gensym contains  a reference to the
	    ;;closure object implementing DO-OVERFLOW.
	    (make-asmcall 'mref
	      (list (make-constant (make-object (primitive-public-function-name->location-gensym 'do-overflow)))
		    (make-constant off-symbol-record-proc)))
	    (list aligned-size)))))

    (define (alloc-check/no-hooks aligned-size)
      (E (make-shortcut
	     (make-conditional (%test aligned-size)
		 (nop)
	       (interrupt))
	   (make-forcall "ik_collect" (list aligned-size)))))

    (define (%test aligned-size)
      ;;There is a page  between the heap nursery allocation red line  and the end of
      ;;the nursery.  How does the garbage collector handle big objects?
      ;;
      ;;* If  the requested  size is less  than, or  equal to, a  page size:  we just
      ;;  compare the Allocation Pointer Register with the red line pointer.
      ;;
      ;;* If the requested size is greater than  a page size: we check that the whole
      ;;  allocated memory block fits the nursery area before the red line.
      ;;
      (if (struct-case aligned-size
	    ((constant i)
	     (<= i PAGE-SIZE))
	    (else #f))
	  (make-asmcall '<= (list apr RED-LINE-POINTER))
	;;(RED-LINE-POINTER - apr) >= aligned-size
	(make-asmcall '>=
	  (list (make-asmcall 'int- (list RED-LINE-POINTER apr))
		aligned-size))))

    (define-constant RED-LINE-POINTER
      (make-asmcall 'mref
	(list pcr (make-constant pcb-allocation-redline))))

    (define-inline-constant PAGE-SIZE
      4096)

    #| end of module: ALLOC-CHECK, ALLOC-CHECK/NO-HOOKS |# )

  #| end of module: V |# )


(module (E)

  (define (E x)
    (struct-case x
      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (E e1) (E e2)))

      ((bind lhs* rhs* e)
       (%do-bind lhs* rhs* (E e)))

      ((asmcall op rand*)
       (E-asmcall x op rand*))

      ((funcall rator rand*)
       ;;For side effects the return value is discarded, so there is no DST location.
       (let ((dst       #f)
	     (asmlabel  #f))
	 (%handle-non-tail-call rator rand* dst asmlabel)))

      ((jmpcall asmlabel rator rand*)
       ;;For side effects the return value is discarded, so there is no DST location.
       (let ((dst #f))
	 (%handle-non-tail-call rator rand* dst asmlabel)))

      ((forcall op rand*)
       ;;For side effects the return value is discarded, so there is no DST location.
       (let ((rator  (make-constant (make-foreign-label op)))
	     (dst    #f))
	 (%handle-non-tail-call rator rand* dst op)))

      ((shortcut body handler)
       (make-shortcut (E body) (E handler)))

      (else
       (compiler-internal-error __module_who__
	 "invalid recordised code in E context"
	 (unparse-recordized-code/sexp x)))))

  (define (E-asmcall x op rand*)
    (import OPERANDS-SIMPLIFICATION)
    (case op
      ((mset bset mset32)
       ;;We expect X to have one of the formats:
       ;;
       ;;   (asmcall mset   (?operand-referencing-scheme-object ?offset ?new-val))
       ;;   (asmcall bset   (?operand-referencing-scheme-object ?offset ?new-val))
       ;;   (asmcall mset32 (?operand-referencing-scheme-object ?offset ?new-val))
       ;;
       ;;MSET is  used, for example, to  store objects in  the car and cdr  of pairs.
       ;;MSET32 is used,  for example, to store single characters  in a string.  BSET
       ;;is used, for example, to store single bytes in a bytevector.
       (S* rand*
	 (lambda (simple-rand*)
	   (let ((objref  (make-disp (car simple-rand*) (cadr simple-rand*)))
		 (new-val (caddr simple-rand*)))
	     (make-asm-instr op objref new-val)))))

      ((fl:load fl:store
		fl:add! fl:sub! fl:mul! fl:div!
		fl:from-int fl:shuffle
		fl:store-single fl:load-single
		bswap!)
       ;;Remembering that the floating point operations are performed on the stack of
       ;;the CPU's floating point unit, we expect X to have one of the formats:
       ;;
       ;;   (asmcall fl:load  (?flonum-operand ?offset))
       ;;   (asmcall fl:store (?flonum-operand ?offset))
       ;;
       ;;   (asmcall fl:add!  (?flonum-operand ?offset))
       ;;   (asmcall fl:sub!  (?flonum-operand ?offset))
       ;;   (asmcall fl:mul!  (?flonum-operand ?offset))
       ;;   (asmcall fl:div!  (?flonum-operand ?offset))
       ;;
       ;;   (asmcall fl:from-int (?int-operand ?int-operand))
       ;;   (asmcall fl:shuffle  (?bv-operand ?offset))
       ;;
       ;;   (asmcall fl:store-single (?pointer ?offset))
       ;;   (asmcall fl:load-single  (?flonum-operand ?offset))
       ;;
       ;;   (asmcall bswap! (?int-operand ?int-operand))
       ;;
       (S* rand*
	 (lambda (simple-rand*)
	   (make-asm-instr op (car simple-rand*) (cadr simple-rand*)))))

      ((nop interrupt incr/zero? fl:double->single fl:single->double)
       ;;Remembering that the floating point operations are performed on the stack of
       ;;the CPU's floating point unit, we expect X to have the format:
       ;;
       ;;   (asmcall nop       ())
       ;;   (asmcall interrupt ())
       ;;
       ;;   (asmcall incr/zero? (?pointer ?offset ?incr-step))
       ;;
       ;;   (asmcall fl:double->single ())
       ;;   (asmcall fl:single->double ())
       ;;
       x)

      (else
       (compiler-internal-error __module_who__
	 "invalid ASMCALL operator in E context"
	 (unparse-recordized-code/sexp x)))))

  #| end of module: E |# )


(module (P)

  (define (P x)
    (struct-case x
      ((constant)
       x)

      ((seq e0 e1)
       (make-seq (E e0) (P e1)))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (P e1) (P e2)))

      ((bind lhs* rhs* e)
       (%do-bind lhs* rhs* (P e)))

      ((asmcall op rand*)
       (P-asmcall op rand*))

      ((shortcut body handler)
       (make-shortcut (P body) (P handler)))

      (else
       (compiler-internal-error __module_who__
	 "invalid recordised code in P context"
	 (unparse-recordized-code/sexp x)))))

  (module (P-asmcall)

    (define (P-asmcall op rand*)
      ;;We expect an input ASMCALL struct with the format:
      ;;
      ;;   (asmcall ?op (?rand1 ?rand2))
      ;;
      ;;If both  the operands are  simple constants,  we transform the  input ASMCALL
      ;;into:
      ;;
      ;;   (asm-instr move (tmp1 ?rand1))
      ;;   (asmcall   ?op  (tmp1 ?rand2))
      ;;
      ;;otherwise we  assume the operands may  be complex, so we  transform the input
      ;;ASMCALL into  code that evaluates the  operands and loads the  results in CPU
      ;;registers; for example:
      ;;
      ;;   (asm-instr move (tmp1 ?rand1))
      ;;   (asm-instr move (tmp2 ?rand2))
      ;;   (asm-instr ?op  (tmp1 tmp2))
      ;;
      (let ((rand1 (car  rand*))
	    (rand2 (cadr rand*)))
	(if (and (constant? rand1)
		 (constant? rand2))
	    (let ((t (make-unique-var 'tmp)))
	      (P (make-bind (list t) (list rand1)
			    (make-asmcall op (list t rand2)))))
	  (%simplify-rand rand1
	    (lambda (simple-rand1)
	      (%simplify-rand rand2
		(lambda (simple-rand2)
		  (make-asm-instr op simple-rand1 simple-rand2))))))))

    (define (%simplify-rand x.rand kont)
      (import OPERANDS-SIMPLIFICATION)
      (struct-case x.rand
	((asmcall x.rand.op x.rand.rand*)
	 (if (eq? x.rand.op 'mref)
	     ;;We expect x.rand to have the format:
	     ;;
	     ;;   (asmcall mref (?operand-referencing-scheme-object ?offset))
	     ;;
	     (S* x.rand.rand*
	       (lambda (simple-x.rand.rand*)
		 (kont (make-disp (car simple-x.rand.rand*) (cadr simple-x.rand.rand*)))))
	   (S x.rand kont)))
	(else
	 (S x.rand kont))))

    #| end of module: P-asmcall |# )

  #| end of module: P |# )


(module (%handle-tail-call)
  ;;Here we build the Scheme stack layout needed  to perform a tail call to a closure
  ;;object.  Let's consider the common case in which we are already inside a function
  ;;call and we perform another function call; the old, uplevel call frame is already
  ;;on the stack.  We build the following layout:
  ;;
  ;;           high memory
  ;;   |                          |         --
  ;;               ...                      .
  ;;   |--------------------------|         . uplevel stack frame
  ;;   | uplevel return address   | <-- FPR .
  ;;   |--------------------------|         --
  ;;   | uplevel stack operand 0  |         .
  ;;   |--------------------------|         .
  ;;   | uplevel stack operand 1  |         . uplevel operands and local variables
  ;;   |--------------------------|         . represented by FVAR structs
  ;;   |       local var 0        |         .
  ;;   |--------------------------|         .
  ;;   |       local var 1        |         .
  ;;   |--------------------------|         .--
  ;;   |  stack operand 0 in tmp  |         .
  ;;   |--------------------------|         . operands to the call
  ;;   |  stack operand 1 in tmp  |         . in temporary locations
  ;;   |--------------------------|         . represented by FVAR structs
  ;;   |  stack operand 2 in tmp  |         .
  ;;   |--------------------------|         --
  ;;   |                          |
  ;;           low memory
  ;;
  ;;The stack  local variables  are represented  by FVAR  structs.  These  locals are
  ;;allocated by  the BIND structs  that survived  all the previous  compiler passes;
  ;;this compiler pass creates additional locals with the function %DO-BIND.
  ;;
  ;;The stack operands variables are represented by FVAR structs; these operands must
  ;;match the  actual function  arguments.  This compiler  pass computes  the operand
  ;;values and allocates temporary stack locations for them.
  ;;
  ;;In  addition to  the stack  operands: some  register operands  are handed  to the
  ;;callee  closure object;  such values  are computed  and stored  in dedicated  CPU
  ;;registers, where  the binary code of  the callee expects them.   At present, only
  ;;the reference  to closure  object is  handled this way,  passing it  throught the
  ;;CP-REGISTER.
  ;;
  ;;When all the operand values have been computed and stored in temporary locations:
  ;;they are moved  to the actual stack  locations for the call,  overwriting the old
  ;;operands and the local variables.  The resulting scenario follows:
  ;;
  ;;           high memory
  ;;   |                          |         --
  ;;               ...                      .
  ;;   |--------------------------|         . uplevel stack frame
  ;;   | uplevel return address   | <-- FPR .
  ;;   |--------------------------|         --
  ;;   |     stack operand 0      |         .
  ;;   |--------------------------|         . operands to the call
  ;;   |     stack operand 1      |         . in correct locations
  ;;   |--------------------------|         . represented by FVAR structs
  ;;   |     stack operand 2      |         .
  ;;   |--------------------------|         --
  ;;   |       local var 1        |
  ;;   |--------------------------|
  ;;   |                          |
  ;;           low memory
  ;;
  ;;The uplevel  return address is left  untouched because it becomes  the new return
  ;;address.  If  there are  leftover operands  or local  variables from  the uplevel
  ;;function execution:  they are left alone  and simply overwritten by  further code
  ;;execution.
  ;;
  (define (%handle-tail-call target rator rand*)
    ;;Handle FUNCALL and JMPCALL structures in tail position.
    ;;
    ;;If the  argument TARGET  is true:  the tail call  is for  a JMPCALL  struct and
    ;;TARGET is a CODE-LOC wrapping the name of the target Assembly label.  If TARGET
    ;;is false: the tail call is for a FUNCALL struct.
    ;;
    ;;We build and return a struct instance to represent:
    ;;
    ;;1.  For the operator and the operands: a sequence of assignments to store the
    ;;values in registers or memory locations.
    ;;
    ;;2. Loading the number of  arguments in the appropriate register.
    ;;
    ;;3. The actual call.
    ;;
    (let* ((args (cons rator rand*))
	   (locs (%formals-locations PARAMETER-REGISTERS args)))
      ;;We want  to determine which ARGS  are complex expressions for  which there is
      ;;the need to allocate a temporary location in which to store the result.
      (let recur ((input-arg*  (reverse args))
		  (input-dst*  (reverse locs))
		  (output-arg* '())
		  (output-dst* '()))
	(define-syntax-rule (%do-recur ?output-arg* ?output-dst*)
	  (recur (cdr input-arg*) (cdr input-dst*) ?output-arg* ?output-dst*))
	(if (pair? input-arg*)
	    (let ((arg (car input-arg*))
		  (dst (car input-dst*)))
	      (cond ((constant? arg)
		     ;;The operand is  either a register operand or  a stack operand;
		     ;;the operand's expression is a CONSTANT: we can just load it in
		     ;;the associated location.
		     (%do-recur (cons arg output-arg*)
				(cons dst output-dst*)))

		    ((and (fvar? dst)
			  (var?  arg)
			  (eq? dst (var-loc arg)))
		     ;;The operand is a stack  operand; the operand's expression is a
		     ;;VAR; the destination  location is an FVAR; the  operand VAR is
		     ;;already  allocated  to  the  location's FVAR.   We  skip  this
		     ;;operand: there is nothing to be done.
		     ;;
		     ;;This is the case, for example,  in which a function tail calls
		     ;;itself with the same operands:
		     ;;
		     ;;   (define (f a b)
		     ;;     (f a b))
		     ;;   (f 1 2)
		     ;;
		     (%do-recur output-arg* output-dst*))

		    (else
		     (let ((tmp (make-unique-var 'tmp)))
		       (%local-value-cons tmp)
		       (make-seq
			 ;;Generate assembly instructions to compute a value from ARG
			 ;;and store the result in destination DST.
			 (V tmp arg)
			 (%do-recur (cons tmp output-arg*)
				    (cons dst output-dst*)))))))
	  (%assign*
	      output-dst*
	      output-arg*
	    (make-seq
	      ;;Store in the AA-REGISTER a  fixnum representing the negated number of
	      ;;operands.
	      (%notify-number-of-operands (length rand*))
	      (if target
		  ;;This is was a JMPCALL: we  jump directly to the binary code entry
		  ;;point represented  by the Assembly  label in the  CODE-LOC struct
		  ;;TARGET.
		  (make-asmcall 'direct-jump (cons target (cons* ARGC-REGISTER pcr esp apr locs)))
		;;This was  a FUNCALL: we  jump indirectly  to the binary  code entry
		;;point by retrieving it, at run-time, from the closure object.
		(make-asmcall 'indirect-jump (cons* ARGC-REGISTER pcr esp apr locs)))))))))

  (define (%formals-locations parameter-registers regparm*+rand*)
    ;;Non-tail recursive function.  Return a list  of items having the same length of
    ;;REGPARM*+RAND*; the first items are  taken from PARAMETER-REGISTERS, the others
    ;;are newly constructed FVAR structs.
    ;;
    (if (pair? regparm*+rand*)
	(if (pair? parameter-registers)
	    (cons (car parameter-registers)
		  (%formals-locations (cdr parameter-registers) (cdr regparm*+rand*)))
	  ;;Here we have consumed all the REGPARM* and only the RAND* are left.
	  (%one-fvar-for-each-rand 1 regparm*+rand*))
      '()))

  (define (%one-fvar-for-each-rand i rand*)
    ;;Non-tail recursive  function.  Build and return  a list of FVAR  structs having
    ;;the same length of RAND*.
    ;;
    (if (pair? rand*)
	(cons (mkfvar i)
	      (%one-fvar-for-each-rand (fxadd1 i) (cdr rand*)))
      '()))

  #| end of module: %HANDLE-TAIL-CALL |# )


(module (%handle-non-tail-call)
  ;;Here we  build the Scheme  stack layout  needed to perform  a non-tail call  to a
  ;;closure object.  Let's consider the common case  in which we are already inside a
  ;;function call and  we perform another function call; the  old, uplevel call frame
  ;;is already on the stack.  We build the following layout:
  ;;
  ;;           high memory
  ;;   |                          |         --
  ;;               ...                      .
  ;;   |--------------------------|         . uplevel stack frame
  ;;   | uplevel return address   | <-- FPR .
  ;;   |--------------------------|         --
  ;;   | uplevel stack operand 0  |         .
  ;;   |--------------------------|         .
  ;;   | uplevel stack operand 1  |         .
  ;;   |--------------------------|         . stack frame described
  ;;   |       local var 0        |         . by this call's call table,
  ;;   |--------------------------|         . represented by FVAR structs
  ;;   |       local var 1        |         .
  ;;   |--------------------------|         .
  ;;   |        empty word        |         .
  ;;   |--------------------------|         --
  ;;   |      stack operand 0     |         .
  ;;   |--------------------------|         . operands to the call
  ;;   |      stack operand 1     |         . represented by NFV structs
  ;;   |--------------------------|         --
  ;;   |                          |
  ;;           low memory
  ;;
  ;;The stack  local variables  are represented  by FVAR  structs.  These  locals are
  ;;allocated by  the BIND structs  that survived  all the previous  compiler passes;
  ;;this compiler pass creates additional locals with the function %DO-BIND.
  ;;
  ;;The stack operands variables are represented  by NFV structs; these operands must
  ;;match the  actual function  arguments.  This compiler  pass allocates  such stack
  ;;operands with the function %DO-OPERANDS-BIND.
  ;;
  ;;In  addition to  the stack  operands: some  register operands  are handed  to the
  ;;callee  closure object;  such values  are computed  and stored  in dedicated  CPU
  ;;registers, where  the binary code of  the callee expects them.   At present, only
  ;;the reference  to closure  object is  handled this way,  passing it  throught the
  ;;CP-REGISTER.
  ;;
  ;;The "empty  word" will be  filled by  the return address  of the function  we are
  ;;about to call.
  ;;
  (define (%handle-non-tail-call rator rand* dst-local call-target)
    ;;Build a NON-TAIL-CALL-FRAME  including everything needed to  perform a non-tail
    ;;call to a closure object.  If the return  value of the call is requested by the
    ;;caller, the return value is recordised code representing:
    ;;
    ;;   (seq
    ;;     (non-tail-call-frame
    ;;       --- preparation of call operands ---
    ;;       (non-tail-call ---))
    ;;     (asm-instr move ?DST-LOCAL AA-REGISTER)))
    ;;
    ;;otherwise, if the return value of the call is to be discarded, the return value
    ;;is recordised code representing:
    ;;
    ;;   (non-tail-call-frame
    ;;     --- preparation of call operands ---
    ;;     (non-tail-call ---))
    ;;
    ;;The argument DST-LOCAL must be false a VAR or NFV struct:
    ;;
    ;;* When  false: it means  the return value of  this function call  is discarded;
    ;;  this function call is performed for its side effects.
    ;;
    ;;* When non-false: it  represents the location to which the  return value of the
    ;;  function  call must be  stored: first the  callee function stores  its return
    ;;  value into the RETURN-VALUE-REGISTER, then caller moves it into DST-LOCAL.
    ;;
    ;;When the function returns a single  value: the return value stored in DST-LOCAL
    ;;is the  actually returned  Scheme object.  When  the function  returns multiple
    ;;values: the return  value stored in DST-LOCAL is the  number of returned Scheme
    ;;objects (0, 2 or more) and the Scheme objects are on the Scheme stack.
    ;;
    ;;The argument CALL-TARGET is false, a string or a gensym:
    ;;
    ;;* When false: this call is to a core primitive function.
    ;;
    ;;* When a string:  this call is to a foreign C language  function and the string
    ;;  is its name.
    ;;
    ;;*  When a  gensym: this  call is  a jump  to the  entry point  of a  combinator
    ;;  function.
    ;;
    (receive (register-name* register-arg* stack-arg*)
	(%nontail-locations PARAMETER-REGISTERS (cons rator rand*))
      ;;REGISTER-ARG* and  STACK-ARG* may be  complex operands; so first  we evaluate
      ;;the proper function operands, which are  stack operands, and load them in the
      ;;appropriate stack locations:
      ;;
      ;;   (asm-instr move ?stack-arg.nfv    ?stack-arg)
      ;;   ...
      ;;
      ;;then we  evaluate the register operands'  values and store them  in temporary
      ;;locations:
      ;;
      ;;   (asm-instr move ?register-arg.var ?register-arg)
      ;;   ...
      ;;
      ;;finally  we load  the  values of  the  register operands  in  the actual  CPU
      ;;registers:
      ;;
      ;;   (asm-instr move ?register-name ?register-arg.var)
      ;;   ...
      ;;
      ;;the  ?RATOR  is always  loaded  in  the CP-REGISTER,  so  the  first item  of
      ;;PARAMETER-REGISTER must be CP-REGISTER.
      (let ((register-arg*.var ($map/stx (lambda (x)
					   (make-unique-var 'tmp))
				 register-arg*))
	    (stack-arg*.nfv    ($map/stx (lambda (x)
					   (make-nfv 'unset-conflicts #f #f #f #f))
				 stack-arg*)))
	(define body
	  (let ((live #f)
		(body (let ((ntcall (let ((args (cons* ARGC-REGISTER pcr esp apr
						       (append register-name* stack-arg*.nfv)))
					  (mask #f)
					  (size #f))
				      (make-non-tail-call call-target dst-local args mask size))))
			(%make-call-frame-body stack-arg*.nfv stack-arg*
					       register-arg*.var register-arg*
					       register-name* rand* ntcall))))
	    (make-non-tail-call-frame stack-arg*.nfv live body)))
	(if dst-local
	    (make-seq
	      body
	      (%move-dst<-src dst-local RETURN-VALUE-REGISTER))
	  body))))

  (define (%nontail-locations regs args)
    ;;Non-tail recursive function.
    ;;
    (if (pair? args)
	(if (pair? regs)
	    (receive (r* rl* f*)
		(%nontail-locations (cdr regs) (cdr args))
	      (values (cons (car regs) r*)
		      (cons (car args) rl*)
		      f*))
	  (values '() '() args))
      (values '() '() '())))

  (define (%make-call-frame-body stack-arg*.nfv stack-arg*
				 register-arg*.var register-arg*
				 register-name* rand* ntcall)
    ;;Load  on the  stack  the stack  operands  of the  function  call.  These  stack
    ;;locations are below the location that will hold the return address of the call.
    (%do-operands-bind*
	stack-arg*.nfv
	stack-arg*
      ;;Load in temporary  locations the values of the register  parameters; skip the
      ;;RATOR.
      (%do-bind
	  (cdr register-arg*.var)
	  (cdr register-arg*)
	;;Load in a temporary location the value of the RATOR register parameter.
	(%do-bind
	    (list (car register-arg*.var))
	    (list (car register-arg*))
	  ;;Store  in the  actual CPU  registers the  register parameter  values from
	  ;;their temporary locations.
	  (%assign*
	      register-name*
	      register-arg*.var
	    (make-seq
	      ;;Load  in AA-REGISTER  a  fixnum representing  the  negated number  of
	      ;;operands.
	      (%notify-number-of-operands (length rand*))
	      ntcall))))))

  (define (%do-operands-bind* nfv* rhs* tail-body)
    ;;Non-tail recursive function.  Given a list  of destination locations NFV* and a
    ;;list  of  source   expressions  RHS*,  build  and  return   a  struct  instance
    ;;representing:
    ;;
    ;;   (seq
    ;;     (asm-instr move ?nfv ?rhs)
    ;;     ...
    ;;     ?tail-body)
    ;;
    ;;in addition: filter the RHS* through V.
    ;;
    (if (pair? nfv*)
	(begin
	  ;;(assert (nfv? (car nfv*)))
	  (make-seq
	    ;;Generate assembly instructions to compute a value from "(car rhs*)" and
	    ;;store the result in destination "(car nfv*)".
	    (V                  (car nfv*) (car rhs*))
	    (%do-operands-bind* (cdr nfv*) (cdr rhs*) tail-body)))
      tail-body))

  #| end of module: %HANDLE-NON-TAIL-CALL |# )


;;;; done

#| end of module: IMPOSE-CALLING-CONVENTION/EVALUATION-ORDER |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-asmcall		'scheme-indent-function 1)
;; eval: (put 'assemble-sources		'scheme-indent-function 1)
;; eval: (put 'make-conditional		'scheme-indent-function 2)
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; eval: (put 'make-seq			'scheme-indent-function 0)
;; eval: (put 'multiple-forms-sequence	'scheme-indent-function 0)
;; eval: (put 'S			'scheme-indent-function 1)
;; eval: (put 'S*			'scheme-indent-function 1)
;; eval: (put '%do-operands-bind*	'scheme-indent-function 2)
;; eval: (put '%do-bind			'scheme-indent-function 2)
;; eval: (put '%assign*			'scheme-indent-function 2)
;; End:
