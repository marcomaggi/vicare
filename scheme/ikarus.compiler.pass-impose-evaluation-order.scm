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
  ;;are converted to the equivalent of:
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
  ;;* Function call  instruction blocks are inserted to represent  non-tail calls and
  ;;tail-calls.
  ;;
  ;;* High-level Assembly instructions requiring temporary locations to store partial
  ;;results are expanded into more basic instructions.
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   bind		conditional		constant
  ;;   forcall		funcall			jmpcall
  ;;   asmcall		seq
  ;;   shortcut		var
  ;;
  ;;in  addition CLOSURE-MAKER  and  CODE-LOC  structs can  appear  in side  CONSTANT
  ;;structs.
  ;;
  ;;NOTE In this module we create FVAR structs and NFV structs in the recordised code
  ;;returned to  the caller; such  structures will  be processed in  further compiler
  ;;passes.  But *no* FVAR and NFV structs are present in the input recordised code.
  ;;
  (module (argc-convention
	   eax ecx edx
	   AA-REGISTER AP-REGISTER CP-REGISTER FP-REGISTER PC-REGISTER)
    (import INTEL-ASSEMBLY-CODE-GENERATION))

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
      (let ((y (V-and-return x)))
	(make-locals (local-values) y))))

  (module (V-clambda)
    ;;Upon entering the  execution, the binary code of a  callee function expects the
    ;;following "register operands" to be present:
    ;;
    ;;AAR:  the  Accumulator  and  Arguments   count  Register  must  hold  a  fixnum
    ;;representing the negated number of operands on the stack.
    ;;
    ;;APR: the Allocation Pointer Register must hold an untagged pointer to the first
    ;;free  word on  the Scheme  heap nursery.   This value  is mutated  only if  the
    ;;function allocates new Scheme objects on the heap.
    ;;
    ;;FPR: the  Frame Pointer Register  must hold an  untagged pointer to  the return
    ;;address for  this function call.   This value is  mutated only if  the function
    ;;performs nested function calls, either tail calls or non-tail calls.
    ;;
    ;;PCR:  the Process  Control Register  must  hold an  untagged pointer  to the  C
    ;;language structure PCB.  This value is never mutated.
    ;;
    ;;CPR: the  Closure Pointer Register  must hold a  tagged pointer to  the closure
    ;;object  implementing this  function.   This  reference is  used  to access  the
    ;;variables this  function is  closed upon.   This value is  mutated only  if the
    ;;function performs nested function calls.
    ;;
    ;;The proper operands to the function call, the "stack operands", if any, must be
    ;;on the Scheme stack at negative offsets from the pointer in the FPR.
    ;;
    (define (V-clambda x)
      (struct-case x
	((clambda x.label x.clause* x.cp x.freevar* x.name)
	 (make-clambda x.label ($map/stx V-clambda-clause x.clause*) x.cp x.freevar* x.name))
	))

    (define (V-clambda-clause clause)
      ;;This  function has  two purposes:  apply "V-and-return"  to the  body of  the
      ;;clause; to  include what is needed  to handle clause's register  operands and
      ;;stack operands.
      ;;
      (struct-case clause
	((clambda-case clause.info clause.body)
	 (struct-case clause.info
	   ((case-info clause.info.label clause.info.args clause.info.proper)
	    ;;Remember that CLAUSE.INFO.ARGS is a proper list of VAR structs with the
	    ;;format:
	    ;;
	    ;;   (?cpvar ?arg ...)
	    ;;
	    ;;where: ?CPVAR represents a machine word that must hold a pointer to the
	    ;;closure object;  each ?ARG represents a  machine word that must  hold a
	    ;;CLAMBDA clause's argument.
	    ;;
	    ;;RAND*.VAR is a list of VAR structs,  one for each stack operand, in the
	    ;;order in which they must appear on  the stack; the VAR structs are used
	    ;;in the body  of the function to access the  operands.  Here we allocate
	    ;;an  FVAR  for  each VAR:  wherever  a  VAR  appears  in the  code,  its
	    ;;associated FVAR will be inserted in a later compiler pass.
	    (let* ((cpvar       (car clause.info.args))
		   (rand*.var   (cdr clause.info.args))
		   (rand*.fvar  (%one-fvar-for-each-stack-operand 1 rand*.var)))
	      ($for-each/stx set-var-loc! rand*.var rand*.fvar)
	      (parametrise ((local-values (list cpvar)))
		(define body
		  (make-seq
		    ;;Load in CPVAR  the reference to closure object  receiving it as
		    ;;register operand.
		    (%move-dst<-src cpvar CP-REGISTER)
		    (V-and-return clause.body)))
		(make-clambda-case
		 (make-case-info clause.info.label (cons CP-REGISTER rand*.fvar) clause.info.proper)
		 (make-locals (local-values) body)))))))))

    (define (%one-fvar-for-each-stack-operand i rand*.var)
      (if (pair? rand*.var)
	  (cons (mkfvar i)
		(%one-fvar-for-each-stack-operand (fxadd1 i) (cdr rand*.var)))
	'()))

    #| end of module: V-clambda |# )

  #| end of module: V-codes |# )


(module (V-and-return)
  ;;Process  a recordised  code struct  for  its return  value, and  append a  RETURN
  ;;high-level Assembly instruction that returns the value to the caller.
  ;;
  (define* (V-and-return x)
    (struct-case x

      ((constant)
       (V-tail x))

      ((var)
       (V-tail x))

      ((asmcall op rand*)
       (case op
	 ((call-with-underflow-handler)
	  (V-call-with-underflow-handler op rand*))
	 (else
	  (V-tail x))))

      ((bind lhs* rhs* e)
       (%assign-complex-rhs-to-local-lhs*
	   lhs* rhs*
	 (V-and-return e)))

      ((seq e0 e1)
       (make-seq (E e0) (V-and-return e1)))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (V-and-return e1) (V-and-return e2)))

      ((funcall rator rand*)
       (%handle-tail-call #f rator rand*))

      ((jmpcall label rator rand*)
       (%handle-tail-call (make-code-loc label) rator rand*))

      ((forcall)
       (V-tail x))

      ((shortcut body handler)
       (make-shortcut (V-and-return body) (V-and-return handler)))

      (else
       (compiler-internal-error __module_who__ __who__ "invalid tail" x))))

  (define (V-tail x)
    ;;X is a struct of type: CONSTANT, VAR, ASMCALL, FORCALL.
    ;;
    (import OPERANDS-SIMPLIFICATION)
    (S x
      (lambda (x)
	(make-seq
	  (%move-dst<-src AA-REGISTER x)
	  (asm 'return AA-REGISTER AP-REGISTER FP-REGISTER PC-REGISTER)))))

  (define (V-call-with-underflow-handler op rand*)
    ;;This  high-level  Assembly instruction  is  used  only  by the  core  primitive
    ;;operation $SEAL-FRAME-AND-CALL  to implement  the heart  of CALL/CC  (call with
    ;;current   continuation)   and  CALL/CF   (call   with   current  frame),   file
    ;;"ikarus.control.sls".  Let's super simplify and  comment the code starting with
    ;;the call to %PRIMITIVE-CALL/CF which is the heart of both CALL/CC and CALL/CF.
    ;;
    ;;Remember that:
    ;;
    ;;* FPR stands for Frame Pointer Register;
    ;;
    ;;* PCR stands  for Process Control Register and it  references the structure PCB
    ;;  defined at the C language level;
    ;;
    ;;* CPR stands  for Closure Pointer Register  and it must contain  a reference to
    ;;  the closure object being executed.
    ;;
    ;;* AA-REGISTER stands for Argument Count Register.
    ;;
    ;;When  arriving here  the  scenario of  the  Scheme  stack is  the  one left  by
    ;;$SEAL-FRAME-AND-CALL:
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
    ;;   |     receiver-func    | -> closure object
    ;;   |----------------------|
    ;;             ...
    ;;   |----------------------|
    ;;   |      free word       | <- pcb->stack_base
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;AA-REGISTER  contains the  encoded  number of  arguments,  counting the  single
    ;;argument  RECEIVER-FUNC  to  %PRIMITIVE-CALL/CF.   The reference  to  the  just
    ;;created continuation  object is in some  CPU register.  The raw  memory pointer
    ;;UNDERFLOW-HANDLER is in some CPU register.
    ;;
    ;;There are 3 operands in RAND*:
    ;;
    ;;* A  VAR struct representing  a location holding  the underflow handler:  a raw
    ;;  memory address equal to the assembly label "ik_underflow_handler".
    ;;
    ;;* A VAR struct representing a  location holding a reference to the continuation
    ;;  object describing  the freezed frames.  Such continuation object  is also the
    ;;  "next process continuation" in the PCB, that is: it is the value of the field
    ;;  "pcb->next_k".
    ;;
    ;;* A  VAR struct  representing a  location holding a  reference to  the receiver
    ;;  closure object: the function accepting the continuation object as argument.
    ;;
    (let ((underflow-handler  (car rand*))
	  (kont-object        (cadr rand*))
	  (receiver-func      (caddr rand*)))
      (assert (var? underflow-handler))
      (assert (var? kont-object))
      (assert (var? receiver-func))
      (assert (fvar? (var-loc receiver-func)))
      ;;Fine.  Here we know that  the operands to CALL-WITH-UNDERFLOW-HANDLER are VAR
      ;;structs, and  that RECEIVER-FUNC is on  the Scheme stack as  stack operand to
      ;;the call to %PRIMITIVE-CALL/CF.
      ;;
      ;;I would  like to spare  additional temporary  locations when possible,  and I
      ;;think it should be possible to spare the one of RECEVEIR-FUNC, but instead it
      ;;appears they are all needed.  (Marco Maggi; Sat Oct 4, 2014)
      (let ((tmp-underflow-handler  (make-unique-var 'tmp-underfow-handler))
	    (tmp-kont-object        (make-unique-var 'tmp-kont-object))
	    (tmp-receiver-func      (make-unique-var 'tmp-receiver-func)))
	(%assign-complex-rhs-to-local-lhs*
	    (list tmp-underflow-handler tmp-kont-object tmp-receiver-func)
	    (list     underflow-handler     kont-object     receiver-func)
	  (multiple-forms-sequence
	    ;;Move IK_UNDERFLOW_HANDLER in its reserved slot the on the Scheme stack.
	    (%move-dst<-src (mkfvar 1) tmp-underflow-handler)
	    ;;Move the the reference to continuation object in its reserved slot on the
	    ;;Scheme stack, as argument to RECEIVER-FUNC.
	    (%move-dst<-src (mkfvar 2) tmp-kont-object)
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
	    ;;Load the reference to closure object RECEIVER-FUNC in the CP-REGISTER.
	    (%load-register-operand/closure-object-reference tmp-receiver-func)
	    ;;Load  in  AA-REGISTER  the  encoded number  of  arguments,  counting  the
	    ;;continuation object.
	    (%load-register-operand/number-of-stack-operands 1)
	    ;;Decrement the FPR so that it points to the underflow handler.
	    (make-asm-instr 'int- FP-REGISTER (make-constant wordsize))
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
	    ;;The following INDIRECT-JUMP  compiles to a single  "jmp" instruction that
	    ;;jumps to  the machine code entry  point in the closure  referenced by the
	    ;;CPR, which is RECEIVER-FUNC.  By doing  a "jmp", rather than a "call", we
	    ;;avoid pushing a return address on the Scheme stack.
	    ;;
	    ;;Notice that  the stack  frame of RECEIVER-FUNC  starts with  the argument
	    ;;KONT.   The IK_UNDERFLOW_HANDLER  we have  put  on the  stack does  *not*
	    ;;belong to any stack frame.
	    ;;
	    ;;If  the  closure RECEIVER-FUNC  returns  without  calling a  continuation
	    ;;escape function: it will return  to the underflow handler; such underflow
	    ;;handler must pop  the continuation object from  "pcb->next_k" and process
	    ;;it as explained in the documentation.
	    ;;
	    (make-asmcall 'indirect-jump
	      (list AA-REGISTER AP-REGISTER CP-REGISTER FP-REGISTER PC-REGISTER (mkfvar 1) (mkfvar 2))))))))

  #| end of module: V-and-return |# )


;;;; helpers

(define (%load-register-operand/number-of-stack-operands num-of-rands)
  ;;Store in the AA-REGISTER a fixnum representing the negated number of operands.
  (%move-dst<-src AA-REGISTER (make-constant (argc-convention num-of-rands))))

(define (%load-register-operand/closure-object-reference closure-object-ref)
  (%move-dst<-src CP-REGISTER closure-object-ref))

(define (%assign-complex-rhs-to-local-lhs* lhs* rhs* tail-body)
  ;;Non-tail recursive  function.  Given a list  of destination locations LHS*  and a
  ;;list of source expressions RHS*, build and return a struct instance representing:
  ;;
  ;;   (seq
  ;;     (asm-instr move ?lhs ?rhs)
  ;;     ...
  ;;     ?tail-body)
  ;;
  ;;The  LHS* are  local  variables:  they must  be  registered  in the  LOCAL-VALUES
  ;;parameter.  The RHS* are complex: they must be filtered through V.
  ;;
  (if (pair? lhs*)
      (%assign-complex-rhs-to-local-lhs
	  (car lhs*)
	  (car rhs*)
	(%assign-complex-rhs-to-local-lhs*
	    (cdr lhs*) (cdr rhs*)
	  tail-body))
    tail-body))

(define (%assign-complex-rhs-to-local-lhs lhs rhs tail-body)
  (%local-value-cons lhs)
  (make-seq
    ;;Generate assembly instructions to compute a value from RHS and store the result
    ;;in destination LHS.
    (V lhs rhs)
    tail-body))


(module OPERANDS-SIMPLIFICATION
  (S S*)

  (define (S* x* kont)
    ;;Simplify the  list of structs  X then  apply the function  KONT to the  list of
    ;;simplified structs.
    ;;
    (if (pair? x*)
	(S (car x*)
	   (lambda (A)
	     (S* (cdr x*)
		 (lambda (D)
		   (kont (cons A D))))))
      (kont '())))

  (define* (S x kont)
    ;;Simplify the struct X then apply the function KONT to the simplified struct.  X
    ;;must be an operand for a high-level Assembly instruction in an ASMCALL struct.
    ;;
    (struct-case x
      ((bind lhs* rhs* body)
       (%assign-complex-rhs-to-local-lhs*
	   lhs* rhs*
	 (S body kont)))
      ((seq e0 e1)
       (make-seq (E e0) (S e1 kont)))
      ((constant)
       (kont x))
      (else
       (cond ((symbol? x)
	      ;;When X is a symbol: it is the name of a CPU register.
	      #;(assert (memq x '(%esi %esp %ebp)))
	      (kont x))
	     ((var? x)
	      (cond ((var-loc x)
		     ;;X is an argument to function  call already stored on the stack
		     ;;in the FVAR from the LOC field.
		     => kont)
		    (else
		     (kont x))))
	     ((or (funcall? x) (asmcall?  x) (jmpcall?     x)
		  (forcall? x) (shortcut? x) (conditional? x))
	      (let ((t (make-unique-var)))
		(%assign-complex-rhs-to-local-lhs
		    t x
		  (kont t))))
	     (else
	      (compiler-internal-error __module_who__ __who__
		"invalid ASMCALL operand to be simpliefied"
		(unparse-recordized-code/sexp x)))))))

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
	      ;;
	      ;;We may  rightfully need to  move it into  another place: this  is not
	      ;;necessarily a useless move from VAR to VAR.
	      => (lambda (loc)
		   ;;(assert (fvar? loc))
		   (%move-dst<-src dst loc)))
	     (else
	      ;;This may be a useless move from VAR to VAR.
	      (%move-dst<-src dst x))))

      ((bind lhs* rhs* body)
       (%assign-complex-rhs-to-local-lhs*
	   lhs* rhs*
	 (V dst body)))

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
	 (compiler-internal-error __module_who__ __who__
	   "invalid recordised code in V context" (unparse-recordized-code/sexp x))))))

;;; --------------------------------------------------------------------

  (define* (V-asmcall x dst op rand*)
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
		   (%move-dst<-src dst AP-REGISTER)
		   ;;Add the tag to the pointer.
		   (make-asm-instr 'logor dst primary-tag)
		   ;;Increment the Allocation Pointer  Register by the aligned size
		   ;;of the block.
		   (make-asm-instr 'int+ AP-REGISTER aligned-size))))))))

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
		   (%move-dst<-src dst AP-REGISTER)
		   (make-asm-instr 'logor dst primary-tag)
		   (make-asm-instr 'int+  AP-REGISTER aligned-size))))))))

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
       (compiler-internal-error __module_who__ __who__
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
	  (make-asmcall '<= (list AP-REGISTER RED-LINE-POINTER))
	;;(RED-LINE-POINTER - AP-REGISTER) >= aligned-size
	(make-asmcall '>=
	  (list (make-asmcall 'int- (list RED-LINE-POINTER AP-REGISTER))
		aligned-size))))

    (define-constant RED-LINE-POINTER
      (make-asmcall 'mref
	(list PC-REGISTER (make-constant pcb-allocation-redline))))

    (define-inline-constant PAGE-SIZE
      4096)

    #| end of module: ALLOC-CHECK, ALLOC-CHECK/NO-HOOKS |# )

  #| end of module: V |# )


(module (E)

  (define* (E x)
    (struct-case x
      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (E e1) (E e2)))

      ((bind lhs* rhs* e)
       (%assign-complex-rhs-to-local-lhs*
	   lhs* rhs*
	 (E e)))

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
       (compiler-internal-error __module_who__ __who__
	 "invalid recordised code in E context"
	 (unparse-recordized-code/sexp x)))))

  (define* (E-asmcall x op rand*)
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
       (compiler-internal-error __module_who__ __who__
	 "invalid ASMCALL operator in E context"
	 (unparse-recordized-code/sexp x)))))

  #| end of module: E |# )


(module (P)

  (define* (P x)
    (struct-case x
      ((constant)
       x)

      ((seq e0 e1)
       (make-seq (E e0) (P e1)))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (P e1) (P e2)))

      ((bind lhs* rhs* e)
       (%assign-complex-rhs-to-local-lhs*
	   lhs* rhs*
	 (P e)))

      ((asmcall op rand*)
       (P-asmcall op rand*))

      ((shortcut body handler)
       (make-shortcut (P body) (P handler)))

      (else
       (compiler-internal-error __module_who__ __who__
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
	    (let ((t (make-unique-var)))
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
  ;;object.
  ;;
  ;;Upon entering  the execution, the  binary code of  a callee function  expects the
  ;;following "register operands" to be present:
  ;;
  ;;AAR: the Accumulator and Arguments count Register must hold a fixnum representing
  ;;the negated number of operands on the stack.
  ;;
  ;;APR: the Allocation  Pointer Register must hold an untagged  pointer to the first
  ;;free word on the Scheme heap nursery.
  ;;
  ;;FPR:  the Frame  Pointer Register  must hold  an untagged  pointer to  the return
  ;;address for this function call.
  ;;
  ;;PCR: the Process Control Register must hold an untagged pointer to the C language
  ;;structure PCB.  This value is never mutated.
  ;;
  ;;CPR:  the Closure  Pointer Register  must hold  a tagged  pointer to  the closure
  ;;object implementing this function.
  ;;
  ;;With  the exception  of the  CPR: all  these register  operands already  have the
  ;;correct value  when the code  generated here is executed.   Here we only  need to
  ;;store the correct value in the CPR.
  ;;
  ;;Let's consider the common case in which we are already inside a function call and
  ;;we perform another function  call; the old, uplevel call frame  is already on the
  ;;stack.  We build the following layout:
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
  ;;The stack local variables are represented  by VAR structs in the input recordised
  ;;code and by FVAR structs in the output recordised code.  Some of these locals are
  ;;allocated by  the BIND structs  that survived  all the previous  compiler passes;
  ;;this compiler pass creates additional locals.
  ;;
  ;;The  stack operands  variables  are represented  by FVAR  structs  in the  output
  ;;recordised code; these  operands must match the actual  function arguments.  This
  ;;compiler pass computes the operand values and allocates temporary stack locations
  ;;for them, when needed.
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
  ;;address.  If there are leftover operands  and/or local variables from the uplevel
  ;;function execution:  they are left alone  and simply overwritten by  further code
  ;;execution.
  ;;
  (define (%handle-tail-call target rator rand*)
    ;;Handle  FUNCALL and  JMPCALL  structures  in tail  position.   Since the  stack
    ;;operands are  placed in stack locations  already occupied by the  uplevel stack
    ;;operands and local  variables: here we must take care  of preserving the values
    ;;already on the stack while computing the operands.
    ;;
    ;;If the  argument TARGET  is true:  the tail call  is for  a JMPCALL  struct and
    ;;TARGET is a CODE-LOC wrapping the name of the target Assembly label.  If TARGET
    ;;is false: the tail call is for a FUNCALL struct.
    ;;
    ;;These  RAND*.FVAR represent  the  location  on the  stack  in  which the  stack
    ;;operands must be put.
    (define rand*.fvar
      (%one-fvar-for-each-stack-operand 1 rand*))
    ;;We want to determine which RAND* are complex expressions for which there is the
    ;;need  to  allocate  temporary  locations,  before  putting  the  value  in  the
    ;;operands's FVAR.
    (let recur ((input-arg*  (reverse rand*))
		(input-dst*  (reverse rand*.fvar))
		(output-arg* '())
		(output-dst* '()))
      (define-syntax-rule (%do-recur ?output-arg* ?output-dst*)
	(recur (cdr input-arg*) (cdr input-dst*) ?output-arg* ?output-dst*))
      (if (pair? input-arg*)
	  (let ((arg (car input-arg*))
		(dst (car input-dst*)))
	    (cond ((constant? arg)
		   ;;The operand's expression  is a CONSTANT: we can just  load it in
		   ;;the associated location.
		   (%do-recur (cons arg output-arg*)
			      (cons dst output-dst*)))

		  ((and (fvar? dst)
			(var?  arg)
			(eq? dst (var-loc arg)))
		   ;;The operand's expression  is a VAR; the  destination location is
		   ;;an FVAR; the operand VAR  is already allocated to the location's
		   ;;FVAR.  We skip this operand: there is nothing to be done.
		   ;;
		   ;;This is  the case, for example,  in which a function  tail calls
		   ;;itself with the same operands:
		   ;;
		   ;;   (define (f a b)
		   ;;     (f a b))
		   ;;   (f 1 2)
		   ;;
		   (%do-recur output-arg* output-dst*))

		  (else
		   (let ((tmp (make-unique-var)))
		     (%assign-complex-rhs-to-local-lhs
			 tmp arg
		       (%do-recur (cons tmp output-arg*)
				  (cons dst output-dst*)))))))
	;;Here OUTPUT-DST* is  a subset of RAND*.FVAR and OUTPUT-ARG*  is a subset of
	;;RAND*.
	(%make-tail-call-sequence target rator rand*.fvar output-dst* output-arg*))))

  (define (%make-tail-call-sequence target rator rand*.fvar rand*.dst rand*.src)
    ;;Build and return  recordised code that puts stack operands  on the stack, loads
    ;;register operands  in the register, and  finally performs the call.   Since the
    ;;stack operands  are placed in stack  locations already occupied by  the uplevel
    ;;stack operands  and local variables: here  we must take care  of preserving the
    ;;values already on the stack while computing the operands.
    ;;
    ;;The RATOR is supposed to be a struct representing recordised code evaluating to
    ;;a closure object.
    ;;
    ;;* If RATOR is a "complex" struct that needs temporary locations allocation (CPU
    ;;  registers  and stack locals) to  produce the result: we  allocate a temporary
    ;;  location here, RATOR.VAR, and use %ASSIGN-COMPLEX-RHS-TO-LOCAL-LHS* to filter
    ;;  RATOR through V and store the result in it.
    ;;
    ;;* If  the RATOR is a  "simple" struct, we do  not need to allocate  a temporary
    ;;  location: we  just load the value in the  CP-REGISTER.  Simple operators are:
    ;;   VAR structs  that  do not  represent uplevel  stack  operands, because  they
    ;;  already are  references to closure objects stored in  memory locations or CPU
    ;;  registers;  CONSTANT structs, because  they can  be directly loaded  into the
    ;;  CP-REGISTER.
    ;;
    ;;NOTE If a VAR  struct represents an uplevel stack operand:  its LOC field holds
    ;;an  FVAR; otherwise  its LOC  field holds  false.  VAR  structs that  represent
    ;;uplevel  stack operands  are not  simple operators:  they are  stored in  stack
    ;;locations that  may be  overwritten by  the new operands,  so we  must preserve
    ;;their values until  all the stack operands and the  reference to closure object
    ;;have been computed.
    (receive (rator.simple already-simple?)
	(struct-case rator
	  ((var)
	   (if (var-loc rator)
	       (values (make-unique-var) #f)
	     (values rator #t)))
	  ((constant)
	   (values rator #t))
	  (else
	   (values (make-unique-var) #f)))
      ;;If  there  is the  need:  compute  the  reference  to closure  object  before
      ;;overwriting the stack locations with the new stack operands.
      (%assign-complex-rhs-to-local-lhs*
	  (if already-simple? '() (list rator.simple))
	  (if already-simple? '() (list rator))
	;;Put the stack operands  on the stack in the correct  positions for the tail
	;;call.
	(%assign-simple-rhs-to-non-local-lhs*
	    rand*.dst
	    rand*.src
	  (multiple-forms-sequence
	    ;;Load in the actual CPU registers the register operands.
	    (%load-register-operand/closure-object-reference rator.simple)
	    (%load-register-operand/number-of-stack-operands (length rand*.fvar))
	    (if target
		;;This is  was a JMPCALL: we  jump directly to the  binary code entry
		;;point  represented by  the Assembly  label in  the CODE-LOC  struct
		;;TARGET.
		;;
		;;NOTE When the  ASMCALL has DIRECT-JUMP as operator:  the first item
		;;in the  operands must be  the CODE-LOC representing  target!!!  The
		;;order of the other operands does not matter.
		(make-asmcall 'direct-jump (cons* target
						  AA-REGISTER AP-REGISTER CP-REGISTER FP-REGISTER PC-REGISTER
						  rand*.fvar))
	      ;;This was a FUNCALL: we jump indirectly to the binary code entry point
	      ;;by retrieving it, at run-time, from the closure object.
	      (make-asmcall 'indirect-jump (cons* AA-REGISTER AP-REGISTER CP-REGISTER FP-REGISTER PC-REGISTER
						  rand*.fvar))))))))

  (define (%one-fvar-for-each-stack-operand i rand*)
    ;;Non-tail recursive  function.  Build and return  a list of FVAR  structs having
    ;;the same length of RAND*.
    ;;
    (if (pair? rand*)
	(cons (mkfvar i)
	      (%one-fvar-for-each-stack-operand (fxadd1 i) (cdr rand*)))
      '()))

  (define (%assign-simple-rhs-to-non-local-lhs* lhs* rhs* tail-body)
    ;;Non-tail recursive function.  Given a list  of destination locations LHS* and a
    ;;list  of  source   expressions  RHS*,  build  and  return   a  struct  instance
    ;;representing:
    ;;
    ;;   (seq
    ;;     (asm-instr move ?lhs ?rhs)
    ;;     ...
    ;;     ?tail-body)
    ;;
    ;;The LHS*  are not local  variables: they  do not need  to be registered  in the
    ;;LOCAL-VALUES parameter.  The  RHS* are simple: they do not  need to be filtered
    ;;through V.
    ;;
    (if (pair? lhs*)
	(begin
	  ;; (assert (let ((lhs (car lhs*)))
	  ;; 	  (or (and (symbol? lhs)
	  ;; 		   (eq? lhs CP-REGISTER))
	  ;; 	      (fvar? lhs))))
	  (make-seq
	    (%move-dst<-src (car lhs*) (car rhs*))
	    (%assign-simple-rhs-to-non-local-lhs*
		(cdr lhs*) (cdr rhs*)
	      tail-body)))
      tail-body))

  #| end of module: %HANDLE-TAIL-CALL |# )


(module (%handle-non-tail-call)
  ;;Here we  build the Scheme  stack layout  needed to perform  a non-tail call  to a
  ;;closure object.
  ;;
  ;;Upon entering  the execution, the  binary code of  a callee function  expects the
  ;;following "register operands" to be present:
  ;;
  ;;AAR: the Accumulator and Arguments count Register must hold a fixnum representing
  ;;the negated number of operands on the stack.
  ;;
  ;;APR: the Allocation  Pointer Register must hold an untagged  pointer to the first
  ;;free word on the Scheme heap nursery.
  ;;
  ;;FPR:  the Frame  Pointer Register  must hold  an untagged  pointer to  the return
  ;;address for this function call.
  ;;
  ;;PCR: the Process Control Register must hold an untagged pointer to the C language
  ;;structure PCB.  This value is never mutated.
  ;;
  ;;CPR:  the Closure  Pointer Register  must hold  a tagged  pointer to  the closure
  ;;object implementing this function.
  ;;
  ;;With the exception of  AAR and CPR: all these register  operands already have the
  ;;correct value  when the code  generated here is executed.   Here we only  need to
  ;;store the correct values in AAR and CPR.
  ;;
  ;;Let's consider the common case in which we are already inside a function call and
  ;;we perform another function  call; the old, uplevel call frame  is already on the
  ;;stack.  We build the following layout:
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
  ;;The stack local variables are represented  by VAR structs in the input recordised
  ;;code and by FVAR structs in the output recordised code.  Some of these locals are
  ;;allocated by  the BIND structs  that survived  all the previous  compiler passes;
  ;;this compiler pass creates additional locals.
  ;;
  ;;The  stack operands  variables  are  represented by  NFV  structs  in the  output
  ;;recordised code; these operands must match the actual function arguments.
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
    ;;  value into the AA-REGISTER, then caller moves it into DST-LOCAL.
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
    (let* ((rand*.nfv (%one-nfv-for-each-stack-operand 1 rand*))
	   (ntcall    (let ((all-rand* (cons* AA-REGISTER AP-REGISTER CP-REGISTER FP-REGISTER PC-REGISTER rand*.nfv))
			    (mask      #f)
			    (size      #f))
			(make-non-tail-call call-target dst-local all-rand* mask size)))
	   (ntframe   (let ((live   #f)
			    (ntbody (%make-non-tail-call-frame-body rator rand* rand*.nfv ntcall)))
			(make-non-tail-call-frame rand*.nfv live ntbody))))
      (if dst-local
	  (make-seq
	    ntframe
	    (%move-dst<-src dst-local AA-REGISTER))
	ntframe)))

  (define (%make-non-tail-call-frame-body rator rand* rand*.nfv ntcall)
    ;;Build and  return recordised code to  be used as body  in a NON-TAIL-CALL-FRAME
    ;;struct.   Since  the  stack  operands  are  placed  in  fresh  stack  locations
    ;;(described  by the  NFV structs):  here we  do not  worry about  preserving the
    ;;values already on the stack while computing the operands.
    ;;
    ;;Load on the  stack operands of the  function call in the  "next frame variable"
    ;;slots.
    (%do-operands-bind*
	rand*.nfv
	rand*
      ;;The RATOR is supposed to be  a struct representing recordised code evaluating
      ;;to a closure object.
      ;;
      ;;* If  RATOR is a "complex"  struct that needs temporary  locations allocation
      ;;   (CPU registers  and stack  locals) to  produce the  result: we  allocate a
      ;;       temporary       location      here,      RATOR.VAR,       and      use
      ;;  %ASSIGN-COMPLEX-RHS-TO-LOCAL-LHS* to  filter RATOR through V  and store the
      ;;  result in it.
      ;;
      ;;* If the RATOR  is a "simple" struct, we do not need  to allocate a temporary
      ;;  location: we just load the value in the CP-REGISTER.  Simple operators are:
      ;;  VAR structs, because they already  are references to closure objects stored
      ;;  in memory locations or CPU registers; CONSTANT structs, because they can be
      ;;  directly loaded into the CP-REGISTER.
      ;;
      (receive (rator.simple already-simple?)
	  (struct-case rator
	    ((var)
	     (values rator #t))
	    ((constant)
	     (values rator #t))
	    (else
	     (values (make-unique-var) #f)))
	(%assign-complex-rhs-to-local-lhs*
	    (if already-simple? '() (list rator.simple))
	    (if already-simple? '() (list rator))
	  ;;Load in the  actual CPU registers the register operand  values from their
	  ;;temporary locations.
	  (multiple-forms-sequence
	    (%load-register-operand/closure-object-reference rator.simple)
	    (%load-register-operand/number-of-stack-operands (length rand*))
	    ntcall)))))

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

  (define (%one-nfv-for-each-stack-operand i rand*)
    ;;Non-tail recursive function.  Build and return a list of NFV structs having the
    ;;same length of RAND*.
    ;;
    (if (pair? rand*)
	(cons (make-nfv i #f #f #f #f)
	      (%one-nfv-for-each-stack-operand (fxadd1 i) (cdr rand*)))
      '()))

  #| end of module: %HANDLE-NON-TAIL-CALL |# )


;;;; done

#| end of module: IMPOSE-CALLING-CONVENTION/EVALUATION-ORDER |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-asmcall		'scheme-indent-function 1)
;; eval: (put 'make-conditional		'scheme-indent-function 2)
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; eval: (put 'make-seq			'scheme-indent-function 0)
;; eval: (put 'multiple-forms-sequence	'scheme-indent-function 0)
;; eval: (put 'S			'scheme-indent-function 1)
;; eval: (put 'S*			'scheme-indent-function 1)
;; eval: (put '%do-operands-bind*	'scheme-indent-function 2)
;; eval: (put '%assign-simple-rhs-to-non-local-lhs*	'scheme-indent-function 2)
;; eval: (put '%assign-complex-rhs-to-local-lhs*	'scheme-indent-function 2)
;; eval: (put '%assign-complex-rhs-to-local-lhs		'scheme-indent-function 2)
;; End:
