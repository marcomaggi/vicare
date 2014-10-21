;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
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


;;;; assembly labels for common subroutines

(module (refresh-common-assembly-subroutines-cached-labels!
	 sl-annotated-procedure-label
	 sl-apply-label
	 sl-continuation-code-label
	 sl-invalid-args-label
	 sl-mv-ignore-rp-label
	 sl-mv-error-rp-label
	 sl-values-label
	 sl-cwv-label)
  (import INTEL-ASSEMBLY-CODE-GENERATION)


(define-syntax (define-cached x)
  (syntax-case x (public-function
		  entry-point-label number-of-free-variables
		  code-annotation definitions local-labels
		  assembly)
    ((_ ?refresh
	((public-function		?func-name)
	 (entry-point-label		?entry-point-assembly-label)
	 (number-of-free-variables	?num-of-freevars)
	 (code-annotation		?annotation)
	 (definitions			?def ...)
	 (local-labels			?lab ...)
	 (assembly			?body0 ?body ...))
	...)
     (with-syntax (((LABEL-GENSYM ...) (generate-temporaries #'(?func-name ...))))
       #'(begin
	   (define LABEL-GENSYM #f)
	   ...
	   (define (?func-name)
	     (or LABEL-GENSYM (error '?func-name "uninitialized label")))
	   ...
	   (define (?refresh)
	     (define-syntax ?func-name
	       (lambda (stx)
		 (syntax-violation '?func-name "cannot use label before it is defined" stx #f)))
	     ...
	     (let* ((?func-name (let ((label (receive-and-return (?entry-point-assembly-label)
						 (gensym (symbol->string '?entry-point-assembly-label))
					       ?def ...
					       (define ?lab (gensym (symbol->string (quote ?lab))))
					       ...
					       ;;We  know  that  ?BODY0 is  always  a
					       ;;symbolic expression:
					       ;;
					       ;;   (label ?entry-point-assembly-label)
					       ;;
					       ;;We  discard  the   return  value  of
					       ;;ASSEMBLE-SOURCES.
					       (assemble-sources thunk?-label
						 ;;This   must    be   a    list   of
						 ;;CODE-OBJECT-SEXP          symbolic
						 ;;expressions.
						 `((code-object-sexp
						    (number-of-free-vars:  ,?num-of-freevars)
						    (annotation:           ,?annotation)
						    . ,(list ?body0 ?body ...)))))))
				  (set! LABEL-GENSYM label)
				  (lambda () label)))
		    ...)
	       (void))))
       ))
    ))

(define (thunk?-label x)
  #f)

(define-auxiliary-syntaxes
  public-function
  entry-point-label
  number-of-free-variables
  code-annotation
  definitions
  local-labels
  assembly)


(define-cached refresh-common-assembly-subroutines-cached-labels!

  ;;SL-ANNOTATED-PROCEDURE-LABEL Given a reference to a closure object
  ;;stored  in the  Closure  Pointer Register  (CPR), representing  an
  ;;annotated closure, retrieve the actual closure and call it.
  ;;
  ;;Notice that we do not touch the stack here.
  ;;
  ;;NOTE This is for debugging purposes, it is not used by Vicare; see
  ;;the    primitive    operations    $MAKE-ANNOTATED-PROCEDURE    and
  ;;$ANNOTATED-PROCEDURE-ANNOTATION  for  more  details  on  annotated
  ;;procedures.
  ;;
  ((public-function		sl-annotated-procedure-label)
   (entry-point-label		SL_annotated)
   (number-of-free-variables	2)
   ;;ANNOTATION-INDIRECT is a  struct type without fields;  it is used
   ;;to generate unique values of disjoint type.  This will end in the
   ;;code object's annotation field.
   (code-annotation		(make-annotation-indirect))
   (definitions
     (import (only (ikarus.code-objects)
		   make-annotation-indirect)))
   (local-labels)
   (assembly
    (label SL_annotated)
    ;;Load into CPR (Closure Pointer  Register) a reference to closure
    ;;object retrieving it  from the second free variable  slot in the
    ;;closure object actually referenced by the CPR itself.
    ;;
    ;;     ---
    ;;  CPR | -------------> |---|---|---| closure object
    ;;     ---                         |
    ;;      ^                          |
    ;;      |                          |
    ;;       --------------------------
    ;;
    (movl (mem (fx- (fx+ disp-closure-data wordsize) closure-tag)
	       CP-REGISTER)
	  CP-REGISTER)
    ;;Fetch a binary  code address from the  closure object referenced
    ;;by the CPR (Closure Pointer Register) and call it.
    (tail-indirect-cpr-call)
    ))

;;; --------------------------------------------------------------------

  ;;SL-APPLY-LABEL.  In the context of a function application like:
  ;;
  ;;   (apply ?function ?arg0 ?arg1 '(?arg2 ?arg3 ?arg4))
  ;;
  ;;this  routine iterates  the list  of arguments  and pushes  on the
  ;;stack the Scheme objects: ?ARG2, ?ARG3, ?ARG4.
  ;;
  ;;Before:
  ;;                  ---
  ;;           FPR --> | ?return-address
  ;;                  ---
  ;;                   | ?arg0
  ;;                  ---
  ;;                   | ?arg1
  ;;                  ---
  ;;   [FPR + EAX] --> | --> (?arg2 ?arg3 ?arg4)
  ;;                  ---
  ;;
  ;;after:
  ;;                  ---
  ;;           FPR --> | ?return-address
  ;;                  ---
  ;;                   | ?arg0
  ;;                  ---
  ;;                   | ?arg1
  ;;                  ---
  ;;                   | ?arg2
  ;;                  ---
  ;;                   | ?arg3
  ;;                  ---
  ;;   [FPR + EAX] --> | ?arg4
  ;;                  ---
  ;;
  ((public-function		sl-apply-label)
   (entry-point-label		SL_apply)
   (number-of-free-variables	0)
   (code-annotation		'sl-apply-label)
   (definitions)
   (local-labels L_apply_done
		 L_apply_loop)
   ;;We suppose that:  at (descending) offset EAX from  the address in
   ;;FPR (Frame  Pointer Register)  there is a  reference to  a Scheme
   ;;list.
   (assembly
    (label SL_apply)
    ;;Load in EBX the word at offset EAX from the frame pointer.
    (movl (mem FP-REGISTER eax) ebx)
    ;;If EBX holds the Scheme null object ...
    (cmpl (int NULL-OBJECT) ebx)
    ;;... there are no further arguments to push on the stack.
    (je (label L_apply_done))

    (label L_apply_loop)
    ;;Load in ECX the car.
    (movl (mem off-car ebx) ecx)
    ;;Load in EBX the cdr.
    (movl (mem off-cdr ebx) ebx)
    ;;Move the car at offset EAX from the frame pointer.
    (movl ecx (mem FP-REGISTER eax))
    ;;Decrement  EAX  by the  word  size:  stack  offset of  the  next
    ;;argument, if any.
    (subl (int wordsize) eax)
    ;;If EBX does not hold the Scheme null object ...
    (cmpl (int NULL-OBJECT) ebx)
    ;;... there are more function call arguments to push on the stack.
    (jne (label L_apply_loop))

    (label L_apply_done)
    ;;Undo the  previous increment of EAX,  so that EAX is  the offset
    ;;from the address in FPR of the last function call argument.
    (addl (int wordsize) eax)
    ;;Fetch a binary  code address from the  closure object referenced
    ;;by the CPR (Closure Pointer Register) and jump directly there.
    (tail-indirect-cpr-call)
    ))

;;; --------------------------------------------------------------------

  ;;SL-CONTINUATION-CODE-LABEL  This  subroutine  is  used  to  resume
  ;;execution  of  a previously  freezed  stack  frame from  a  Scheme
  ;;continuation  object; it  is  used to  implement the  continuation
  ;;escape  function  generated by  CALL/CC;  it  is the  entry  point
  ;;associated  to  the  closure  object  returned  by  the  primitive
  ;;operation $FRAME->CONTINUATION.
  ;;
  ;;Upon entering this subroutine:
  ;;
  ;;*  The  Process Control  Register  (PCR)  must reference  the  PCB
  ;;  structure.
  ;;
  ;;* The Frame  Pointer Register (FPR) must reference the  top of the
  ;;  Scheme stack.
  ;;
  ;;* The Closure Pointer Register (CPR)  must hold a reference to the
  ;;   actual  escape  closure  object,  whose  first  slot  for  free
  ;;  variables  contains a  reference to  the continuation  object to
  ;;   resume.
  ;;
  ;;*  The AA-REGISTER  must  contain a  fixnum representing  the  negated number  of
  ;;  arguments handed to the escape function; such arguments are the values returned
  ;;  to the topmost function in the continuation.
  ;;
  ;;Before  resuming  execution: we  want  to  set the  Frame  Pointer
  ;;Register to the address of the  highest machine word on the Scheme
  ;;segment,  right  below  the  "frame_base";  such  memory  location
  ;;contains the address of the  underflow handler: the assembly label
  ;;"ik_underflow_handler" defined in the file "ikarus-enter.S".
  ;;
  ;;Calling the  escape closure throws  away the stack  frames between
  ;;"pcb->frame_base"  and the  FPR.  Before  rewinding the  stack the
  ;;scenario is:
  ;;
  ;;         high memory
  ;;   |                      |
  ;;   |----------------------|
  ;;   |                      | <-- pcb->frame_base
  ;;   |----------------------|
  ;;   | ik_underflow_handler |
  ;;   |----------------------|         --
  ;;             ...                    .
  ;;   |----------------------|         . frames that will be thrown away
  ;;   |  old return address  | <- FPR  .
  ;;   |----------------------|         --
  ;;   |      argument 0      |         .
  ;;   |----------------------|         . frame of the escape function
  ;;   |      argument 1      |         .
  ;;   |----------------------|         --
  ;;   |                      |
  ;;          low memory
  ;;
  ;;after rewinding the stack the scenario is:
  ;;
  ;;         high memory
  ;;   |                      |
  ;;   |----------------------|
  ;;   |                      | <-- pcb->frame_base
  ;;   |----------------------|
  ;;   | ik_underflow_handler | <- FPR
  ;;   |----------------------|         --
  ;;   |      argument 0      |         .
  ;;   |----------------------|         . frame of the escape function
  ;;   |      argument 1      |         .
  ;;   |----------------------|         --
  ;;   |                      |
  ;;          low memory
  ;;
  ((public-function		sl-continuation-code-label)
   (entry-point-label		SL_continuation_code)
   (number-of-free-variables	1)
   (code-annotation		'sl-continuation-code-label)
   (definitions)
   (local-labels L_cont_zero_args
		 L_cont_mult_args
		 L_cont_one_arg
		 L_cont_mult_move_args
		 L_cont_mult_copy_loop)
   (assembly
    (label SL_continuation_code)
    ;;Move in EBX  the reference to the  continuation object contained
    ;;in the first data slot in the closure object.
    (movl (mem off-closure-data CP-REGISTER) ebx)
    ;;Move  the   reference  to  continuation  object   in  the  field
    ;;"pcb->next_k" (overwriting the old value!!!).
    (movl ebx (mem pcb-next-continuation PC-REGISTER))
    ;;Move in EBX the field  "pcb->frame_base".  Notice that we do not
    ;;touch "pcb->frame_pointer" here.
    (movl (mem pcb-frame-base PC-REGISTER) ebx)
    ;;Dispatch according to  the number of arguments to  return to the
    ;;continuation.
    (cmpl (int (argc-convention 1)) eax)
    (jg (label L_cont_zero_args))   ;jump if -1 > EAX: less than one arg
    (jl (label L_cont_mult_args))   ;jump if -1 < EAX: more than one arg

    ;;We give one argument to the continuation.  The typical situation
    ;;on the stack when arriving here is:
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base = EBX
    ;;   |----------------------|
    ;;   | ik_underflow_handler | = highest machine word on the stack
    ;;   |----------------------|          --
    ;;             ...                     .
    ;;   |----------------------|          . frames that will be thrown away
    ;;   |  old return address  | <-- FPR  .
    ;;   |----------------------|          --
    ;;   |     return value     | <-- FPR - wordisze
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;where "old return  address" is the return address  to the caller
    ;;of the  escape function and EAX  contains the fixnum -1;  but we
    ;;can already be  at the base of the stack  if the escape function
    ;;was called in tail position while reinstating previously freezed
    ;;frames:
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base = EBX
    ;;   |----------------------|
    ;;   | ik_underflow_handler | <-- FPR
    ;;   |----------------------|
    ;;   |     return value     | <-- FPR - wordisze
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    (label L_cont_one_arg)
    ;;Load in EAX the the return value.
    (movl (mem (fx- wordsize) FP-REGISTER) eax)
    ;;Load   in   the   Frame    Pointer   Register   the   value   of
    ;;"pcb->frame_base".
    (movl ebx FP-REGISTER)
    ;;Decrement  Frame Pointer  Register  by a  wordsize,  so that  it
    ;;contains the address of the  highest machine word in the current
    ;;Scheme stack segment.
    (subl (int wordsize) FP-REGISTER)
    ;;Jump  to  the  underflow   handler  to  actually  reinstate  the
    ;;continuation.  The situation on the stack when arriving here is:
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base
    ;;   |----------------------|
    ;;   | ik_underflow_handler | <-- Frame Pointer Register
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;EAX  contains   the  single   return  value   and  "pcb->next_k"
    ;;references the continuation object we must go back to.
    ;;
    (ret)

    ;;We  give  zero  arguments  to  the  continuation.   The  typical
    ;;situation on the stack when arriving here is:
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base = EBX
    ;;   |----------------------|
    ;;   | ik_underflow_handler | = highest machine word on the stack
    ;;   |----------------------|          --
    ;;             ...                     .
    ;;   |----------------------|          . frames that will be thrown away
    ;;   |  old return address  | <-- FPR  .
    ;;   |----------------------|          --
    ;;   |                      |
    ;;          low memory
    ;;
    ;;where "old return  address" is the return address  to the caller
    ;;of the escape  function and EAX contains the fixnum  0 as number
    ;;of arguments; but we can already be  at the base of the stack if
    ;;the  escape   function  was   called  in  tail   position  while
    ;;reinstating previously freezed frames:
    ;;
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base = EBX
    ;;   |----------------------|
    ;;   | ik_underflow_handler | <-- FPR
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    (label L_cont_zero_args)
    ;;Decrement EBX by a wordsize, so  that it contains the address of
    ;;the highest machine word in the current Scheme stack segment.
    (subl (int wordsize) ebx)
    ;;Store EBX in  the Frame Pointer Register, so  that it references
    ;;the underflow handler.
    (movl ebx FP-REGISTER)
    ;;Load in EBX  the machine word from the address  in EBX.  This is
    ;;like the C language code:
    ;;
    ;;   EBX = *EBX;
    ;;
    ;;Now EBX = ik_underflow_handler.
    ;;
    (movl (mem 0 ebx) ebx)
    ;;Jump to the multivalue underflow handler, retrieving its address
    ;;by adding its offset  to the address "ik_underflow_handler"; see
    ;;the  file "ikarus-enter.S"  for details.   The situation  on the
    ;;stack when arriving here is:
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base
    ;;   |----------------------|
    ;;   | ik_underflow_handler | <-- Frame Pointer Register
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;EAX  contains  zero as  number  of  arguments and  "pcb->next_k"
    ;;references the continuation object we must go back to.
    ;;
    (jmp (mem DISP-MULTIVALUE-RP ebx))

    ;;We give more than one argument to the continuation.  The typical
    ;;situation on the stack when arriving here is:
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base = EBX
    ;;   |----------------------|
    ;;   | ik_underflow_handler | = highest machine word on the stack
    ;;   |----------------------|          --
    ;;             ...                     .
    ;;   |----------------------|          . frames that will be thrown away
    ;;   |  old return address  | <-- FPR  .
    ;;   |----------------------|          --
    ;;   |    return value 0    |
    ;;   |----------------------|
    ;;   |    return value 1    |
    ;;   |----------------------|
    ;;   |    return value 2    |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;where "old return  address" is the return address  to the caller
    ;;of the  escape function and  EAX contains the encoded  number of
    ;;arguments; but we can already be at the base of the stack if the
    ;;escape function  was called  in tail position  while reinstating
    ;;previously freezed frames:
    ;;
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base = EBX
    ;;   |----------------------|
    ;;   | ik_underflow_handler | <-- FPR
    ;;   |----------------------|
    ;;   |    return value 0    |
    ;;   |----------------------|
    ;;   |    return value 1    |
    ;;   |----------------------|
    ;;   |    return value 2    |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    (label L_cont_mult_args)
    ;;Decrement EBX by a wordsize, so  that it contains the address of
    ;;the highest machine word in the current Scheme stack segment.
    (subl (int wordsize) ebx)
    ;;If the current frame pointer is already at the base of the stack
    ;;(FPR == pcb->frame_base - wordsize): we  do not need to copy the
    ;;return values.
    (cmpl ebx FP-REGISTER)
    (jne (label L_cont_mult_move_args))
    ;;Load in EBX  the machine word from the address  in EBX.  This is
    ;;like the C language code:
    ;;
    ;;   EBX = *EBX;
    ;;
    ;;Now EBX = ik_underflow_handler.
    ;;
    (movl (mem 0 ebx) ebx)
    ;;Jump to the multivalue underflow handler, retrieving its address
    ;;by adding its offset  to the address "ik_underflow_handler"; see
    ;;the  file "ikarus-enter.S"  for details.   The situation  on the
    ;;stack when arriving here is:
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base
    ;;   |----------------------|
    ;;   | ik_underflow_handler | <-- Frame Pointer Register (%esp)
    ;;   |----------------------|
    ;;   |    return value 0    |
    ;;   |----------------------|
    ;;   |    return value 1    |
    ;;   |----------------------|
    ;;   |    return value 2    |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;EAX contains  the encoded number of  arguments and "pcb->next_k"
    ;;references the continuation object we must go back to.
    ;;
    (jmp (mem DISP-MULTIVALUE-RP ebx))

    ;;FPR does not reference the highest word on the stack segment; we
    ;;need to copy the arguments from the current frame to the highest
    ;;words of the stack, as follows:
    ;;
    ;;                            --- hi mem
    ;;                             |
    ;;         pcb->frame_base --> |
    ;;                     EBX --> | = ik_underflow_handler
    ;;                      arg0 = |<--
    ;;                      arg1 = |<--+-        dest slots
    ;;                             |   | |
    ;;                             |   | |
    ;;  Frame Pointer Register --> |   | |
    ;;                      arg0 = | --  |     source slots
    ;;                      arg1 = | ----
    ;;                             |
    ;;                            --- lo mem
    ;;
    (label L_cont_mult_move_args)
    (movl (int 0) ecx) ;initialise argument offset
    (label L_cont_mult_copy_loop)
    (subl (int wordsize) ecx)  ;decrement ECX
    (movl (mem FP-REGISTER ecx) edx)   ;load arg in EDX from source slot
    (movl edx (mem ebx ecx))   ;store arg from EDX to dest slot
    (cmpl ecx eax)	       ;moved all?
    (jne (label L_cont_mult_copy_loop))
    ;;Store  "pcb->frame_base   -  wordsize"  in  the   Frame  Pointer
    ;;Register.
    (movl ebx FP-REGISTER)
    ;;Load in EBX  the machine word from the address  in EBX.  This is
    ;;like the C language code:
    ;;
    ;;   EBX = *EBX;
    ;;
    ;;Now EBX = ik_underflow_handler.
    ;;
    (movl (mem 0 ebx) ebx)
    ;;Jump to the multivalue underflow handler, retrieving its address
    ;;by adding its offset  to the address "ik_underflow_handler"; see
    ;;the  file "ikarus-enter.S"  for details.   The situation  on the
    ;;stack when arriving here is:
    ;;
    ;;         high memory
    ;;   |                      | <-- pcb->frame_base
    ;;   |----------------------|
    ;;   | ik_underflow_handler | <-- Frame Pointer Register (%esp)
    ;;   |----------------------|
    ;;   |    return value 0    |
    ;;   |----------------------|
    ;;   |    return value 1    |
    ;;   |----------------------|
    ;;   |    return value 2    |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;EAX contains  the encoded number of  arguments and "pcb->next_k"
    ;;references the continuation object we must go back to.
    (jmp (mem DISP-MULTIVALUE-RP ebx))
    ))

;;; --------------------------------------------------------------------

  ;;SL-INVALID-ARGS-LABEL  This subroutine  handles calls  to function
  ;;with the wrong number of arguments.  We just want to tail call the
  ;;primitive function $INCORRECT-ARGS-ERROR-HANDLER.
  ;;
  ;;Upon entering this label:
  ;;
  ;;**The Frame Pointer  Register (FPR) must reference the  top of the
  ;;  Scheme stack.
  ;;
  ;;**The Closure  Pointer Register (CPR)  must hold a reference  to a
  ;;  closure  object; such closure  is the  one that has  been called
  ;;  with the wrong number of arguments.
  ;;
  ;;The situation on the Scheme stack when arriving here is:
  ;;
  ;;         high memory
  ;;   |                      |
  ;;   |----------------------|
  ;;   |     return address   | <-- Frame Pointer Register
  ;;   |----------------------|
  ;;   |      argument 0      |
  ;;   |----------------------|
  ;;   |      argument 1      |
  ;;   |----------------------|
  ;;   |      argument 3      |
  ;;   |----------------------|
  ;;   |                      |
  ;;          low memory
  ;;
  ;;and EAX contains the encoded  number of arguments.  We just ignore
  ;;the arguments.
  ;;
  ;;FIXME It would be  good to call $INCORRECT-ARGS-ERROR-HANDLER with
  ;;the arguments that are already on the stack.  (Marco Maggi; Nov 6,
  ;;2012)
  ;;
  ((public-function		sl-invalid-args-label)
   (entry-point-label		SL_invalid_args)
   (number-of-free-variables	0)
   (code-annotation		'sl-invalid-args-label)
   (definitions)
   (local-labels)
   (assembly
    (label SL_invalid_args)
    ;;Store on the  stack a reference to the closure  object (from the
    ;;Closure  Pointer Register)  as  first argument  to  the call  to
    ;;$INCORRECT-ARGS-ERROR-HANDLER.
    (movl CP-REGISTER (mem (fx- wordsize) FP-REGISTER))
    ;;Decode  the incorrect  number  of  arguments, so  that  it is  a
    ;;non-negative fixnum.
    (negl eax)
    ;;Store on the  stack the incorrect number of  arguments as second
    ;;argument to the call to $INCORRECT-ARGS-ERROR-HANDLER.
    (movl eax (mem (fx- (fx* 2 wordsize)) FP-REGISTER))
    ;;From the  relocation vector  of this  code object:  retrieve the
    ;;location gensym associated  to $INCORRECT-ARGS-ERROR-HANDLER and
    ;;load it in the Closure Pointer Register (CPR).
    ;;
    ;;The "proc" slot  of such loc gensym contains a  reference to the
    ;;closure object implementing $INCORRECT-ARGS-ERROR-HANDLER.
    (movl (obj (primitive-public-function-name->location-gensym '$incorrect-args-error-handler)) CP-REGISTER)
    ;;Load in the Closure Pointer  Register a reference to the closure
    ;;object implementing the function $INCORRECT-ARGS-ERROR-HANDLER.
    (movl (mem off-symbol-record-proc CP-REGISTER) CP-REGISTER)
    ;;Load in  EAX the  encoded number  of arguments  for the  call to
    ;;$INCORRECT-ARGS-ERROR-HANDLER.
    (movl (int (argc-convention 2)) eax)
    ;;Fetch a binary  code address from the  closure object referenced
    ;;by the Closure Pointer Register and jump directly there.
    (tail-indirect-cpr-call)
    ))

;;; --------------------------------------------------------------------

  ;;SL-MV-IGNORE-RP-LABEL  This  subroutine   is  called  whenever  an
  ;;attempt  to return  zero or  2 or  more values  to a  single value
  ;;context is  performed, and  the receiving  function just  wants to
  ;;ignore the error.
  ;;
  ((public-function		sl-mv-ignore-rp-label)
   (entry-point-label		SL_multiple_values_ignore_rp)
   (number-of-free-variables	0)
   (code-annotation		'sl-mv-ignore-rp-label)
   (definitions)
   (local-labels)
   (assembly
    (label SL_multiple_values_ignore_rp)
    (ret)
    ))

;;; --------------------------------------------------------------------

  ;;SL-MV-ERROR-RP-LABEL This subroutine is called whenever an attempt
  ;;to return zero  or 2 or more  values to a single  value context is
  ;;performed, as in:
  ;;
  ;;   (let ((x (values 1 2)))
  ;;     x)
  ;;
  ;;or:
  ;;
  ;;   (let ((x (values)))
  ;;     x)
  ;;
  ;;This happens *only*  when VALUES is used.  We just  call the error
  ;;handler $MULTIPLE-VALUES-ERROR.
  ;;
  ;;The  label "SL_multiple_values_error_rp"  defines  a return  point
  ;;(rp) for functions  accepting only a single return  value; so this
  ;;label should be used when generating all the call chunks for those
  ;;function calls.
  ;;
  ((public-function		sl-mv-error-rp-label)
   (entry-point-label		SL_multiple_values_error_rp)
   (number-of-free-variables	0)
   (code-annotation		'sl-mv-error-rp-label)
   (definitions)
   (local-labels)
   (assembly
    (label SL_multiple_values_error_rp)
    ;;From the  relocation vector  of this  code object:  retrieve the
    ;;location gensym associated to $MULTIPLE-VALUES-ERROR and load it
    ;;in the Closure Pointer Register  (CPR).  The "proc" slot of such
    ;;loc  gensym   contains  a   reference  to  the   closure  object
    ;;implementing $MULTIPLE-VALUES-ERROR.
    (movl (obj (primitive-public-function-name->location-gensym '$multiple-values-error)) CP-REGISTER)
    ;;Load in the Closure Pointer  Register a reference to the closure
    ;;object implementing the function $MULTIPLE-VALUES-ERROR.
    (movl (mem off-symbol-record-proc CP-REGISTER) CP-REGISTER)
    ;;Fetch a binary  code address from the  closure object referenced
    ;;by the CPR (Closure Pointer Register) and jump directly there.
    (tail-indirect-cpr-call)
    ))

;;; --------------------------------------------------------------------

  ;;Implementation of  the function  VALUES.  The arguments  to VALUES
  ;;are the return values of VALUES.  When arriving here from a call:
  ;;
  ;;   (values ret-val-0 ret-val-1 ret-val-2)
  ;;
  ;;the situation on the Scheme stack is:
  ;;
  ;;         high memory
  ;;   |                      |
  ;;   |----------------------|
  ;;   |    return address    | <-- Frame Pointer Register (FPR)
  ;;   |----------------------|
  ;;   |    return value 0    |
  ;;   |----------------------|
  ;;   |    return value 1    |
  ;;   |----------------------|
  ;;   |    return value 2    |
  ;;   |----------------------|
  ;;   |                      |
  ;;          low memory
  ;;
  ;;and EAX  holds a fixnum being  the negated number of  arguments to
  ;;VALUES, which is the negated number of return values of VALUES.
  ;;
  ;;* When  only one argument  is given:  this routine just  puts that
  ;;   single  value  in  EAX,  then  it  executes  a  "ret"  assembly
  ;;  instruction to plainly go back to "return address".
  ;;
  ;;* When  multiple values are  given: this routine leaves  the stack
  ;;  and  EAX untouched  and jumps to  the multivalue  assembly label
  ;;  associated to "return address".
  ;;
  ;;Notice that if the return address is ik_underflow_handler: nothing
  ;;special needs to be done.
  ;;
  ((public-function sl-values-label)
   (entry-point-label		SL_values)
   (number-of-free-variables	0)
   (code-annotation		'values)
   (definitions)
   (local-labels L_values_one_value
		 L_values_many_values)
   (assembly
    (label SL_values)
    ;;Dispatch according  to the number  of arguments.  Jump  when one
    ;;argument (slower)  because, usually, when  we use VALUES  we are
    ;;returning multiple values.
    (cmpl (int (argc-convention 1)) eax)
    (je (label L_values_one_value))

    ;;Return  0,  2 or  more  values.   Retrieve  the address  of  the
    ;;multivalue  assembly  label   by  adding  DISP-MULTIVALUE-RP  to
    ;;"return address".
    (label L_values_many_values)
    (movl (mem 0 FP-REGISTER) ebx)
    (jmp (mem DISP-MULTIVALUE-RP ebx))

    ;;Return a  single value.  Store  in EAX the single  return value,
    ;;then "ret".
    (label L_values_one_value)
    (movl (mem (fx- wordsize) FP-REGISTER) eax)
    (ret)
    ))

;;; --------------------------------------------------------------------

  ;;Implementation  of the  function  CALL-WITH-VALUES (shortly  named
  ;;CWV).  When arriving here, the situation on the Scheme stack is:
  ;;
  ;;         high memory
  ;;   |                      |
  ;;   |----------------------|
  ;;   |  CWV return address  | <-- Frame Pointer Register (FPR)
  ;;   |----------------------|
  ;;   |   closure reference  | --> producer closure object
  ;;   |----------------------|
  ;;   |   closure reference  | --> consumer closure object
  ;;   |----------------------|
  ;;   |                      |
  ;;          low memory
  ;;
  ;;the  register EAX  holds  a  fixnum being  the  negated number  of
  ;;arguments: as fixnum it is -2, as  machine word it is -8 on 32-bit
  ;;platforms  and  -16  on  64-bit  platforms;  the  Closure  Pointer
  ;;Register  (CPR)  holds a  reference  to  the closure  implementing
  ;;CALL-WITH-VALUES itself.
  ;;
  ((public-function		sl-cwv-label)
   (entry-point-label		SL_call_with_values)
   (number-of-free-variables	0)
   (code-annotation		'call-with-values)
   (definitions)
   (local-labels L_cwv_done
		 L_cwv_loop
		 L_cwv_multi_rp
		 L_cwv_call
		 SL_nonprocedure
		 SL_invalid_args)
   (assembly
    (label SL_call_with_values)

    ;;Validate the number of arguments.
    (cmpl (int (argc-convention 2)) eax)
    (jne (label SL_invalid_args))

    ;;Calling the producer:
    ;;
    ;;1..Fetch from  the stack the  reference to the  producer closure
    ;;   object and store it in EBX.
    ;;
    ;;2..Store in the Continuation  Pointer Register (CPR) a reference
    ;;   to the producer closure object.
    ;;
    ;;3..Check that EBX actually contains a reference to object tagged
    ;;   as closure; else jump to the appropriate error handler.
    ;;
    (movl (mem (fx- wordsize) FP-REGISTER) ebx)
    (movl ebx CP-REGISTER)
    (andl (int closure-mask) ebx)
    (cmpl (int closure-tag) ebx)
    (jne (label SL_nonprocedure))
    ;;
    ;;4..The producer is  called with zero arguments: load  in EAX the
    ;;   fixnum zero.
    ;;
    ;;5..Call the producer closure in  CPR executing a "call" assembly
    ;;   instruction.
    ;;
    ;;The situation  on the stack  right before entering  the assembly
    ;;chunk generated by COMPILE-CALL-TABLE is:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |  producer reference  |
    ;;   |----------------------|
    ;;   |  consumer reference  |
    ;;   |----------------------|
    ;;   |     empty word       |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;and such chunk adjusts the FPR to:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  |
    ;;   |----------------------|
    ;;   |  producer reference  |
    ;;   |----------------------|
    ;;   |  consumer reference  | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |     empty word       |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;before performing the "call"  instruction.  After returning from
    ;;the call: the  assembly chunk adjusts back the  FPR to reference
    ;;the CWV return address.
    ;;
    (movl (int (argc-convention 0)) eax)
    (compile-call-table
     3		 ;The  frame-words-count is  3  because on  the stack  of
		;CALL-WITH-VALUES  there  are  3 machine  words:  return
		;address, producer closure, consumer closure.
     '#(#b110)	;This livemask tells the  garbage collector that on the
		;stack: the highest word (return address) is not a live
		;object, the  middle word (producer closure)  is a live
		;object, the  lowest word  (consumer object) is  a live
		;object.
     (label-address L_cwv_multi_rp)
     (indirect-cpr-call))

    ;;If we are here it means that the producer returned one value; we
    ;;want  to  hand  such  single  value  to  the  consumer  closure,
    ;;performing a tail call to it.
    ;;
    ;;The call  chunk above adjusts the  FPR, so the situation  on the
    ;;stack when we arrive here is:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |  producer reference  |
    ;;   |----------------------|
    ;;   |  consumer reference  |
    ;;   |----------------------|
    ;;   | producer return addr |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;and the returned value  is in EAX.  We want to  set the stack as
    ;;follows:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |     return value     |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;then perform a "jmp" to the entry point of the consumer code.
    ;;
    ;;Store in EBX a reference to the consumer closure object.
    (movl (mem (fx* -2 wordsize) FP-REGISTER) ebx)
    ;;Store in the Continuation Pointer  Register (CPR) a reference to
    ;;the consumer closure object.
    (movl ebx CP-REGISTER)
    ;;Check that EBX actually contains  a reference to closure object;
    ;;else jump to the appropriate error handler.
    (andl (int closure-mask) ebx)
    (cmpl (int closure-tag) ebx)
    (jne (label SL_nonprocedure))
    ;;Store the  returned value on  the stack, right below  the return
    ;;address.
    (movl eax (mem (fx- wordsize) FP-REGISTER))
    ;;We will call the consumer closure with one argument.
    (movl (int (argc-convention 1)) eax)
    ;;Fetch a binary  code address from the  closure object referenced
    ;;by the Closure Pointer Register and jump directly there.
    (tail-indirect-cpr-call)

    ;;If we  are here it means  that the producer returned  zero, 2 or
    ;;more values (not one value) performing a tail call to VALUES; we
    ;;want  to hand  such  multiple values  to  the consumer  closure,
    ;;performing a tail call to it.
    ;;
    ;;When returning  0, 2  or more values:  VALUES performs  a direct
    ;;"jmp" to the label "L_cwv_multi_rp"; the FPR is not adjusted, so
    ;;the situation on the stack when we arrive here is:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  |
    ;;   |----------------------|
    ;;   |  producer reference  |
    ;;   |----------------------|
    ;;   |  consumer reference  |
    ;;   |----------------------|
    ;;   | producer return addr | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |    return value 0    |
    ;;   |----------------------|
    ;;   |    return value 1    |
    ;;   |----------------------|
    ;;   |    return value 2    |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;and the number of returned values is encoded in EAX.  We want to
    ;;set the stack as follows:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |    return value 0    |
    ;;   |----------------------|
    ;;   |    return value 1    |
    ;;   |----------------------|
    ;;   |    return value 2    |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;then perform a "jmp" to the entry point of the consumer code.
    ;;
    (label L_cwv_multi_rp)
    ;;Adjust the  Frame Pointer Register  to reference the  CWV return
    ;;address.
    (addl (int (fx* wordsize 3)) FP-REGISTER)
    ;;Store  in  the  Closure  Pointer Register  a  reference  to  the
    ;;consumer  closure  object.  We  will  check  below that  CPR  is
    ;;actually a  reference to closure  object (to avoid  moving stuff
    ;;into registers twice).
    (movl (mem (fx* -2 wordsize) FP-REGISTER) CP-REGISTER)
    ;;Check if the number of returned values is zero.
    (cmpl (int (argc-convention 0)) eax)
    (je (label L_cwv_done))
    ;;Make EBX reference the first return value on the stack.
    (movl (int (fx* -4 wordsize)) ebx)
    (addl FP-REGISTER ebx)
    ;;Make ECX reference the last return value on the stack.
    (movl ebx ecx)
    (addl eax ecx)
    ;;Copy the return  values in the correct position  right below the
    ;;CWV return address.
    (label L_cwv_loop)
    (movl (mem 0 ebx) edx)
    (movl edx (mem (fx* 3 wordsize) ebx))
    (subl (int wordsize) ebx)
    (cmpl ecx ebx)
    (jge (label L_cwv_loop))

    (label L_cwv_done)
    ;;Check that CPR actually contains  a reference to closure object;
    ;;else jump to the appropriate error handler.
    (movl CP-REGISTER ebx)
    (andl (int closure-mask) ebx)
    (cmpl (int closure-tag) ebx)
    (jne (label SL_nonprocedure))
    ;;Fetch a binary  code address from the  closure object referenced
    ;;by the CPR (Closure Pointer Register) and jump directly there.
    (tail-indirect-cpr-call)

    ;;We come here if either the  producer or the consumer argument is
    ;;not  a closure  object.  The  offending  object must  be in  the
    ;;Closure Pointer Register (CPR).
    ;;
    ;;We    want    to    tail    call    the    primitive    function
    ;;$APPLY-NONPROCEDURE-ERROR-HANDLER,  which accepts  the offending
    ;;value as single argument.  We reset the stack from:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |   closure reference  | --> producer closure object
    ;;   |----------------------|
    ;;   |   closure reference  | --> consumer closure object
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;to:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |   offending object   |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    (label SL_nonprocedure)
    ;;Put on the stack the offending object.
    (movl CP-REGISTER (mem (fx- wordsize) FP-REGISTER))
    ;;From the  relocation vector  of this  code object:  retrieve the
    ;;location gensym  associated to $APPLY-NONPROCEDURE-ERROR-HANDLER
    ;;and load it  in the Closure Pointer Register  (CPR).  The "proc"
    ;;slot  of such  loc gensym  contains a  reference to  the closure
    ;;object implementing $APPLY-NONPROCEDURE-ERROR-HANDLER.
    (movl (obj (primitive-public-function-name->location-gensym '$apply-nonprocedure-error-handler)) CP-REGISTER)
    ;;Load in the Closure Pointer  Register a reference to the closure
    ;;object implementing $APPLY-NONPROCEDURE-ERROR-HANDLER.
    (movl (mem off-symbol-record-proc CP-REGISTER) CP-REGISTER)
    ;;Put in EAX the encoded number of arguments, which is 1.
    (movl (int (argc-convention 1)) eax)
    ;;Fetch a binary  code address from the  closure object referenced
    ;;by the CPR (Closure Pointer Register) and jump directly there.
    (tail-indirect-cpr-call)

    ;;We come here if CALL-WITH-VALUES was applied to the wrong number
    ;;of arguments.  A reference  to the CALL-WITH-VALUES closure must
    ;;be in the Closure Pointer Register (CPR).
    ;;
    ;;We    want    to    tail    call    the    primitive    function
    ;;$INCORRECT-ARGS-ERROR-HANDLER,  which  accepts  2  arguments:  a
    ;;reference to the CALL-WITH-VALUES closure, a non-negative fixnum
    ;;representing the  incorrect number  of arguments.  We  reset the
    ;;stack from:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |   closure reference  | --> producer closure object
    ;;   |----------------------|
    ;;   |   closure reference  | --> consumer closure object
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    ;;to:
    ;;
    ;;         high memory
    ;;   |                      |
    ;;   |----------------------|
    ;;   |  CWV return address  | <-- Frame Pointer Register (FPR)
    ;;   |----------------------|
    ;;   |   closure reference  | --> CALL-WITH-VALUES
    ;;   |----------------------|
    ;;   |  encoded num of args |
    ;;   |----------------------|
    ;;   |                      |
    ;;          low memory
    ;;
    (label SL_invalid_args)
    ;;Put on the stack a  reference to the closure object implementing
    ;;CALL-WITH-VALUES,        as       first        argument       to
    ;;$INCORRECT-ARGS-ERROR-HANDLER.
    (movl CP-REGISTER (mem (fx- wordsize) FP-REGISTER))
    ;;Decode the  number of  arguments, so that  it is  a non-negative
    ;;fixnum.
    (negl eax)
    ;;Put on the stack the incorrect number of arguments as fixnum, as
    ;;second argument to $INCORRECT-ARGS-ERROR-HANDLER.
    (movl eax (mem (fx- 0 (fx* 2 wordsize)) FP-REGISTER))
    ;;From the  relocation vector  of this  code object:  retrieve the
    ;;location gensym associated  to $INCORRECT-ARGS-ERROR-HANDLER and
    ;;load it in the Closure  Pointer Register (CPR).  The "proc" slot
    ;;of such  loc gensym contains  a reference to the  closure object
    ;;implementing $INCORRECT-ARGS-ERROR-HANDLER.
    (movl (obj (primitive-public-function-name->location-gensym '$incorrect-args-error-handler)) CP-REGISTER)
    ;;Load in the Closure Pointer  Register a reference to the closure
    ;;object implementing $INCORRECT-ARGS-ERROR-HANDLER.
    (movl (mem off-symbol-record-proc CP-REGISTER) CP-REGISTER)
    ;;Load in  EAX the  encoded number  of arguments  for the  call to
    ;;$INCORRECT-ARGS-ERROR-HANDLER.
    (movl (int (argc-convention 2)) eax)
    ;;Fetch a binary  code address from the  closure object referenced
    ;;by the CPR (Closure Pointer Register) and jump directly there.
    (tail-indirect-cpr-call)
    )))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
