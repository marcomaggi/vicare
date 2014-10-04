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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;; Introduction
;;
;;

(module CORE-PRIMITIVE-OPERATION-DEFINITIONS
  ()
  (import CODE-GENERATION-FOR-CORE-PRIMITIVE-OPERATION-CALLS)
  (module (PC-REGISTER FP-REGISTER)
    (import INTEL-ASSEMBLY-CODE-GENERATION))


(define-auxiliary-syntaxes safe unsafe)

(define-syntax (define-core-primitive-operation stx)
  ;;Transform a declaration like:
  ;;
  ;;  (define-core-primitive-operation $vector-length unsafe
  ;;    ((P x) body-P)	;when used as "conditional test expression"
  ;;    ((E x) body-E)	;when used as "for side-effects expression"
  ;;    ((V x) body-V))	;when used as "for return value expression"
  ;;
  ;;into (a bit simplified):
  ;;
  ;;  (begin
  ;;    (define cogen-pred-$vector-length
  ;;      (case-lambda
  ;;       ((x)		body-P)
  ;;       (args	(interrupt))))
  ;;
  ;;    (define cogen-effect-$vector-length
  ;;      (case-lambda
  ;;       ((x)		body-E))
  ;;       (args	(interrupt))))
  ;;
  ;;    (define cogen-value-$vector-length
  ;;      (case-lambda
  ;;       ((x)		body-V)
  ;;       (args	(interrupt))))
  ;;
  ;;    (module ()
  ;;      (set-primop! '$vector-length
  ;;                   (make-primitive-handler #f
  ;;                     cogen-pred-$vector-length    #t
  ;;                     cogen-value-$vector-length   #t
  ;;                     cogen-effect-$vector-length  #t))))
  ;;
  ;;If a  core primitive is  defined as  "safe": it is  interruptible and it  will be
  ;;implemented by a SHORTCUT struct:
  ;;
  ;;   (shortcut ?integrated-fast-implementation ?call-to-function-implementation)
  ;;
  ;;If  it is  defined as  "unsafe": it  will  not be  interruptible and  it will  be
  ;;implemented with simple body struct.
  ;;
  ;;The P, V  and E clauses are optional  and there can be multiple  clauses for each
  ;;type: they are like CASE-LAMBDA clauses.  Multiple clauses like:
  ;;
  ;;   (define-core-primitive-operation doit safe
  ;;     ((E x)   body0)
  ;;     ((E x y) body1))
  ;;
  ;;will become the core primitive implementation handler:
  ;;
  ;;   (define cogen-effect-doit
  ;;     (case-lambda
  ;;       ((x)   body0)
  ;;       ((x y) body1)))
  ;;
  ;;The P implementation handler is used to generate a test for a CONDITIONAL struct;
  ;;this handler must  return recordised code with specific  characteristics: it will
  ;;generate Assembly code that performs a comparison and then a jump to a "label for
  ;;conseq" or to a "label for altern".
  ;;
  (define (main stx)
    (syntax-case stx ()
      ((?stx ?name ?interruptable ?clause* ...)
       (let ((cases #'(?clause* ...)))
	 (with-syntax
	     ((COGEN-P		(%cogen-name #'?stx "pred"   #'?name))
	      (COGEN-V		(%cogen-name #'?stx "value"  #'?name))
	      (COGEN-E		(%cogen-name #'?stx "effect" #'?name))
	      (INTERRUPTABLE?	(syntax-case #'?interruptable (safe unsafe)
				  (safe   #t)
				  (unsafe #f))))
	   (let-values
	       (((P-handler P-handled?) (%generate-handler #'COGEN-P #'P cases))
		((V-handler V-handled?) (%generate-handler #'COGEN-V #'V cases))
		((E-handler E-handled?) (%generate-handler #'COGEN-E #'E cases)))
	     #`(module (COGEN-P COGEN-V COGEN-E)
		 (define COGEN-P
		   (fluid-let-syntax ((__who__ (identifier-syntax (quote ?name))))
		     (lambda args
		       (cond-expand
			(#t
			 (%validate-P-implementation-handler-retval (quote COGEN-P) (apply #,P-handler args)))
			(else
			 (apply #,P-handler args))))))
		 (define COGEN-V
		   (fluid-let-syntax ((__who__ (identifier-syntax (quote ?name))))
		     #,V-handler))
		 (define COGEN-E
		   (fluid-let-syntax ((__who__ (identifier-syntax (quote ?name))))
		     #,E-handler))
		 (module ()
		   ;;Import this module for INTERRUPT.
		   (import CODE-GENERATION-FOR-CORE-PRIMITIVE-OPERATION-CALLS)
		   (set-primop! '?name
				(make-primitive-handler INTERRUPTABLE?
							COGEN-P #,P-handled?
							COGEN-V #,V-handled?
							COGEN-E #,E-handled?))))
	     ))))
      ))

  (define (%generate-handler who.id execution-context clause*)
    ;;Return  2  values:  a  CASE-LAMBDA syntax  object  representing  the  primitive
    ;;operation handler for EXECUTION-CONTEXT; a boolean value, true if the primitive
    ;;operation is implemented for EXECUTION-CONTEXT.
    ;;
    (let ((clause* (%filter-cases execution-context clause*)))
      (with-syntax
	  (((CLAUSE ...) clause*))
	(values #`(case-lambda
		   CLAUSE ...
		   ;;This case with any number  of arguments will cause the primitive
		   ;;operation to jump  to the interrupt handler which  will call the
		   ;;primitive  function.  This  way we  can integrate  the primitive
		   ;;calls with few and simple arguments, while reverting to the full
		   ;;function for applications to more arguments.
		   (args
		    (interrupt)))
		(not (null? clause*))))))

  (define (%filter-cases execution-context clause*)
    ;;Extract from CLAUSE* the cases matching EXECUTION-CONTEXT among the possible P,
    ;;V, E.  Return a list of CASE-LAMBDA clauses.
    ;;
    (syntax-case clause* ()
      (() '())
      ((((?PVE . ?arg*) ?body0 ?body ...) . ?rest)
       (free-identifier=? #'?PVE execution-context)
       (cons #'(?arg* ?body0 ?body ...)
	     (%filter-cases execution-context #'?rest)))
      ((?case . ?rest)
       (%filter-cases execution-context #'?rest))))

  (define (%cogen-name stx infix name)
    (let* ((name.str  (symbol->string (syntax->datum name)))
	   (cogen.str (string-append "cogen-" infix "-"  name.str)))
      (datum->syntax stx (string->symbol cogen.str))))

  (main stx))


;;;; primitive operation definition helpers

(define (%validate-P-implementation-handler-retval who rv)
  ;;Called by all the  "for predicate" primitive-operation implementation-handlers to
  ;;validate the return value.  If the return  value RV is correct: return RV itself,
  ;;otherwise raise an exception.
  ;;
  ;;An  implementation handler  for the  P context  can return:
  ;;
  ;;*  The constants  (K #t)  or  (K #f)  as special  values  to be  recognised in  a
  ;;  subsequent compiler pass.
  ;;
  ;;* An ASMCALL with operand among: =, !=, <,  <=, >, >=, u<, u<=, u>, u>=.  This is
  ;;  the "true" return value; it is the proper operation that generates, as Assembly
  ;;  instructions, a jump to the "label for consequent" or "label for alternate".
  ;;
  ;;* A  variety of recordised  code which has  one of the  above as last  form.  For
  ;;   example:  a  call  to  SEC-TAG-TEST  used  to  implement  SYMBOL?   returns  a
  ;;  CONDITIONAL; a call to $FLCMP-AUX used to implement "fl=?" returns a SEQ.
  ;;
  ;;Here we do what we can to validate such return value.
  ;;
  (define (%error msg)
    (compiler-internal-error who msg (unparse-recordized-code/sexp rv)))
  (struct-case rv
    ((constant rv.const)
     (if (boolean? rv.const)
	 rv
       (%error "wrong CONSTANT recordised code returned from \
                primitive-operation implementation handler, expected (K #t) or (K #f)")))
    ((asmcall op args)
     (case op
       ((interrupt)
	rv)
       ((= != < <= > >= u< u<= u> u>=)
	rv)
       (else
	(%error "invalid ASMCALL operand in recordised code returned from primitive-operation implementation handler,\
                 expected an operand among: =, !=, <, <=, >, >=, u<, u<=, u>, u>=, interrupt"))))
    (else
     rv)))


;;;; syntax helpers

(define-syntax /section
  (syntax-rules ()))

(define-syntax section
  (syntax-rules (/section)
    ((section ?body ... /section)
     (begin ?body ...))
    ))

;;; --------------------------------------------------------------------

(define-syntax-rule (interrupt)
  (flagged-interrupt))

;;; --------------------------------------------------------------------

(define-syntax-rule (prm-tag-as-fixnum ?machine-word)
  ;;Given the struct ?MACHINE-WORD representing recordized code that will evaluate to
  ;;an integer:  return a  struct instance representing  recordized code  which, when
  ;;evaluated, will tag the result of ?MACHINE-WORD as fixnum.
  ;;
  (asm 'sll ?machine-word (K fx-shift)))

(define-syntax-rule (prm-UNtag-as-fixnum ?machine-word)
  ;;Given the struct ?MACHINE-WORD representing recordized code that will evaluate to
  ;;an  integer  representing  a  fixnum:   return  a  struct  instance  representing
  ;;recordized code which, when evaluated, will  untag the result of ?MACHINE-WORD as
  ;;fixnum.
  ;;
  ;;Notice that untagging *must* be performed with *arithmetic* right-shift.
  ;;
  (asm 'sra ?machine-word (K fx-shift)))

;;; --------------------------------------------------------------------

(define-syntax-rule (prm-isolate-least-significant-byte ?machine-word)
  ;;Given  a struct  instance ?MACHINE-WORD  representing recordized  code: return  a
  ;;struct instance representing recordized code  which, when evaluated, will isolate
  ;;the least significant byte of the result of ?MACHINE-WORD.
  ;;
  (asm 'logand ?machine-word (K 255)))

;;; --------------------------------------------------------------------
;;; predefined checks

(define (interrupt-unless recordized-code)
  (make-conditional recordized-code
      (nop)
    (interrupt)))

(define (interrupt-when recordized-code)
  (make-conditional recordized-code
      (interrupt)
    (nop)))

(define (interrupt-unless-fixnum recordized-code)
  (interrupt-unless
   (tag-test recordized-code fx-mask fx-tag)))

(define (interrupt-unless-fx binary-representation)
  (if (target-platform-fixnum? binary-representation)
      (nop)
    (interrupt)))


;;;; helpers

(section

 (define-constant NUM-OF-BITS-IN-WORD
   (* wordsize 8))

 (define (tag-test x mask tag)
   ;;Primary tag test: test if X is a word of type TAG; use MASK to extract bits from
   ;;X, then verify if the bits are equal to TAG.
   ;;
   ;;X must be recordised code representing a simplified operand; at run-time it will
   ;;evaluate to an immediate Scheme object  or a tagged pointer referencing a Scheme
   ;;object.
   ;;
   ;;This  function  can be  used  to  in  primop  implementation handlers  for  type
   ;;predicates, like FIXNUM? and PAIR?; example:
   ;;
   ;;   (define-core-primitive-operation fixnum? safe
   ;;     ((P x)
   ;;      (tag-test (V-simple-operand x) fx-mask fx-tag))
   ;;     ((E x)
   ;;      (nop)))
   ;;
   ;;However, notice that the code returned by this function does not generate a full
   ;;predicate that returns  a boolean Scheme object; rather  it generates recordised
   ;;code representing a test that will end with an assembly instruction like "je" or
   ;;"jne".  So it must be further processed  by a successive compiler pass that will
   ;;generate and use  the appropriate assembly labels  to be used as  target for the
   ;;jump.
   ;;
   (if mask
       (asm '= (asm 'logand x (KN mask)) (KN tag))
     (asm '= x (KN tag))))

 (define (sec-tag-test x primary-mask primary-tag secondary-mask secondary-tag)
   ;;Primary and  secondary tag  test: test if  X is a  tagged pointer  referencing a
   ;;Scheme object in which:  the tagged pointer holds a primary  tag; the first word
   ;;of the  referenced memory block holds  the secondary tag.  Example:  port values
   ;;are referenced by pointers tagged as vectors.
   ;;
   ;;X must be recordised code representing a simplified operand; at run-time it will
   ;;evaluate to an immediate Scheme object  or a tagged pointer referencing a Scheme
   ;;object.
   ;;
   ;;This is how it goes:
   ;;
   ;;* We use PRIMARY-MASK to extract bits from  X, then verify if the bits are equal
   ;;  to PRIMARY-TAG.
   ;;
   ;;* If the  primary test is successful:  extract the first word W  from the memory
   ;;  block referenced by X, use SECONDARY-MASK  to extract bits from W, then verify
   ;;  if the bits are equal to SECONDARY-TAG.
   ;;
   ;;This function can be used in primop implementation handlers for type predicates,
   ;;like VECTOR? and SYMBOL?; example:
   ;;
   ;;   (define-core-primitive-operation vector? safe
   ;;     ((P x)
   ;;      (sec-tag-test (V-simple-operand x) vector-mask vector-tag fx-mask fx-tag))
   ;;     ((E x)
   ;;      (nop)))
   ;;
   ;;However, notice that the code returned by this function does not generate a full
   ;;predicate that returns  a boolean Scheme object; rather  it generates recordised
   ;;code representing a test that will end with an assembly instruction like "je" or
   ;;"jne".  So it must be further processed  by a successive compiler pass that will
   ;;generate and use  the appropriate assembly labels  to be used as  target for the
   ;;jump.
   ;;
   ;;There are two TAG-TEST  calls each of which will be  transformed into a sequence
   ;;of Assembly instructions terminating with a  jump; it means a system of Assembly
   ;;labels  must be  generated to  handle this  conditional.  The  alternate of  the
   ;;CONDITIONAL is a constant #f, not a native constant BOOL-FALSE-OBJECT; this case
   ;;will be recognised and specially handled by a subsequent compiler pass.
   ;;
   (make-conditional (tag-test x primary-mask primary-tag)
       (tag-test (asm 'mref x (KN (fx- primary-tag))) secondary-mask secondary-tag)
     (K #f)))

 (define-inline-constant DIRTY-WORD
   -1)

 (define (dirty-vector-set address)
   (define shift-bits 2)
   (asm 'mset32
	(asm 'mref PC-REGISTER (K pcb-dirty-vector))
	(asm 'sll (asm 'srl address (K pageshift)) (K shift-bits))
	(K DIRTY-WORD)))

 (define (smart-dirty-vector-set addr what)
   (struct-case what
     ((constant t)
      (if (or (target-platform-fixnum? t) (immediate? t))
	  (nop)
	(dirty-vector-set addr)))
     ((known expr type)
      (cond ((eq? (T:immediate? type) 'yes)
	     (nop))
	    (else
	     (smart-dirty-vector-set addr expr))))
     (else
      (dirty-vector-set addr))))

 (module (mem-assign)

   (define (mem-assign v base offset)
     ;;Generate low level recordized code  needed to store the result of
     ;;evaluating V at OFFSET from the base heap address BASE.
     ;;
     ;;V must be  a struct instance representing recordized  code.  If V
     ;;is recognised as fixnum or immediate  value (a value that fits in
     ;;a single machine word): there is  no need to signal this mutation
     ;;in the dirty vector.
     ;;
     ;;BASE must be  a struct instance representing  a (possibly tagged)
     ;;base address.
     ;;
     ;;OFFSET must be an exact  integer representing an offset in number
     ;;of bytes.
     ;;
     (struct-case v
       ((constant value)
	(if (or (target-platform-fixnum? value)
		(immediate? value))
	    (asm 'mset base (K offset) (V-simple-operand v))
	  (%slow-mem-assign v base offset)))
       ((known expr type)
	(cond ((eq? (T:immediate? type) 'yes)
	       (asm 'mset base (K offset) (V-simple-operand expr)))
	      (else
	       (%slow-mem-assign expr base offset))))
       (else
	(%slow-mem-assign v base offset))))

   (define (%slow-mem-assign v base offset)
     ;;Generate recordised code to  perform the memory location mutation
     ;;and to signal dirt in the corresponding slot of the dirty vector.
     ;;
     (with-tmp ((t (asm 'int+ base (K offset))))
       (make-seq (asm 'mset t (K 0) (V-simple-operand v))
		 (dirty-vector-set t))))

   #| end of module: mem-assign |# )

 (define (align-code UNknown-amount known-amount)
   ;;Given  a compile-time  known  amount of  bytes  and a  compile-time
   ;;UNknown amount of bytes, which must be summed to obtain the size of
   ;;a memory block to be allocated: compute and return the actual total
   ;;amount of  bytes to be  allocated on  the heap to  keep subsequence
   ;;memory blocks allocated.
   ;;
   ;;See details about memory allocation in the documentation.  See also
   ;;the ALIGN function defined in the library (ikarus.compiler).
   ;;
   (define-inline (%shift-left  ?expr)	(asm 'sll ?expr (K align-shift)))
   (define-inline (%shift-right ?expr)	(asm 'sra ?expr (K align-shift)))
   (%shift-left
    (%shift-right
     (asm 'int+ UNknown-amount (K (+ known-amount (sub1 object-alignment)))))))

 /section)


;;;; values validation

(section

 (define (assert-fixnum x)
   (struct-case x
     ((constant x.val)
      (if (target-platform-fixnum? x.val)
	  (nop)
	(interrupt)))
     ((known x.expr x.type)
      (case (T:fixnum? x.type)
	((yes) (nop))
	((no)  (interrupt))
	(else  (assert-fixnum x.expr))))
     (else
      (interrupt-unless
       (cogen-pred-fixnum? x)))))

 (module (assert-fixnums)

   (define (assert-fixnums a a*)
     ;;Generate and return recordized code that validates, at run time, the arguments
     ;;as fixnums.
     ;;
     (let*-values (((fx* others)  (partition %known-fixnum?     (cons a a*)))
		   ((nfx* others) (partition %known-non-fixnum? others)))
       (cond ((not (null? nfx*))
	      (interrupt))
	     ((null? others)
	      (nop))
	     (else
	      (interrupt-unless
	       (tag-test (%or* (V-simple-operand (car others))
			       (cdr others))
			 fx-mask fx-tag))))))

   (define (%or* a a*)
     (if (pair? a*)
	 (%or* (asm 'logor a (V-simple-operand (car a*)))
	       (cdr a*))
       a))

   (define (%known-fixnum? x)
     (struct-case x
       ((constant x.val)
	(target-platform-fixnum? x.val))
       ((known x.expr x.type)
	(case (T:fixnum? x.type)
	  ((yes)  #t)
	  (else   #f)))
       (else #f)))

   (define (%known-non-fixnum? x)
     (struct-case x
       ((constant x.val)
	(not (target-platform-fixnum? x.val)))
       ((known x.expr x.type)
	(eq? (T:fixnum? x.type) 'no))
       (else
	#f)))

   #| end of module: assert-fixnums |# )

;;; --------------------------------------------------------------------

 (define* (assert-pair x who)
   ;;Return recordised code  that validates X as  a Scheme pair object.  X  must be a
   ;;struct instance representing recordized code not yet filtered through T.
   ;;
   (define (%compile-time-error)
     (compile-time-operand-core-type-error __who__
       "expected pair as primitive operation argument"
       (cons who (unparse-recordized-code/sexp x))))
   (struct-case x
     ((known x.expr x.type)
      (case (T:pair? x.type)
	((yes)
	 (nop))
	((no)
	 (if (option.strict-r6rs)
	     (interrupt)
	   (%compile-time-error)))
	(else
	 (assert-pair x.expr who))))
     ((constant x.const)
      (cond ((pair? x.const)
	     (nop))
	    ((option.strict-r6rs)
	     (interrupt))
	    (else
	     (%compile-time-error))))
     (else
      (interrupt-unless-pair (V-simple-operand x)))))

 (define (interrupt-unless-pair x)
   ;;Return recordised code  that validates X as  a Scheme pair object.  X  must be a
   ;;struct instance representing recordized code that does *not* need to be filtered
   ;;through T.
   ;;
   ;;NOTE The primitive implementation  handler COGEN-PRED-PAIR?  expects an argument
   ;;that still needs to be filtered through T, so we cannot use it here.
   ;;
   (interrupt-unless (tag-test x pair-mask pair-tag)))

;;; --------------------------------------------------------------------

 (define (assert-string x)
   (struct-case x
     ((constant s)
      (if (string? s)
	  (nop)
	(interrupt)))
     ((known expr type)
      (case (T:string? type)
	((yes)
	 (nop))
	((no)
	 (interrupt))
	(else
	 (assert-string expr))))
     (else
      (interrupt-unless (cogen-pred-string? x)))))

 /section)


(section

 (define-core-primitive-operation base-rtd safe
   ;;The base RTD  of all the struct  types is stored in  the C language
   ;;structure PCB.
   ;;
   ((V) (asm 'mref PC-REGISTER (K pcb-base-rtd)))
   ((P) (K #t))
   ((E) (nop)))

 (define-core-primitive-operation nop unsafe
   ;;This is the definition of the primitive operation NOP.
   ;;
   ((E) (nop)))

 (define-core-primitive-operation void safe
   ;;This is the  definition of the Scheme  function VOID.  It just  returns the void
   ;;object, which is true.
   ;;
   ((V) (KN VOID-OBJECT))
   ((P) (K #t))
   ((E) (nop)))

 (define-core-primitive-operation void-object? safe
   ((P x) (asm '= (V-simple-operand x) (KN VOID-OBJECT)))
   ((E x) (nop)))

 (define-core-primitive-operation bwp-object? safe
   ;;This is the implementation of the Scheme function BWP-OBJECT?.
   ;;
   ((P x) (asm '= (V-simple-operand x) (K BWP-OBJECT)))
   ((E x) (nop)))

 (define-core-primitive-operation neq? unsafe
   ;;This is the implementation of the Scheme function NEQ?.
   ;;
   ((P x y) (asm '!= (V-simple-operand x) (V-simple-operand y)))
   ((E x y) (nop)))

 (define-core-primitive-operation eq? safe
   ;;This is the implementation of the Scheme function EQ?.
   ;;
   ((P x y) (asm '= (V-simple-operand x) (V-simple-operand y)))
   ((E x y) (nop)))

 (define (equable? x)
   (or (target-platform-fixnum? x)
       (not (number? x))))

 (define (equable-constant? x)
   (struct-case x
     ((constant xv)
      (equable? xv))
     ((known expr)
      (equable-constant? expr))
     (else
      #f)))

 (define-core-primitive-operation eqv? safe
   ;;This is the implementation of the primitive operation EQV?.
   ;;
   ;;Notice  that at  the Scheme  level the  EQV? predicate  is the  one
   ;;exported by (ikarus predicates).
   ;;
   ((P x y)
    (if (or (equable-constant? x)
	    (equable-constant? y))
	(asm '= (V-simple-operand x) (V-simple-operand y))
      (interrupt)))
   ((E x y) (nop)))

 (define-core-primitive-operation null? safe
   ;;This is the implementation of the Scheme function NULL?.
   ;;
   ((P x) (asm '= (V-simple-operand x) (KN NULL-OBJECT)))
   ((E x) (nop)))

 (define-core-primitive-operation not safe
   ;;This is the implementation of the Scheme function NOT.
   ;;
   ((P x) (asm '= (V-simple-operand x) (KN BOOL-FALSE-OBJECT)))
   ((E x) (nop)))

 (define-core-primitive-operation eof-object safe
   ;;This is the implementation of the Scheme function EOF-OBJECT.
   ;;
   ((V) (KN EOF-OBJECT))
   ((P) (K #t))
   ((E) (nop)))

 (define-core-primitive-operation eof-object? safe
   ;;This is the implementation of the Scheme function EOF-OBJECT?.
   ;;
   ((P x) (asm '= (V-simple-operand x) (KN EOF-OBJECT)))
   ((E x) (nop)))

 (define-core-primitive-operation $unbound-object? unsafe
   ;;This   is   the   implementation   of   the   primitive   operation
   ;;$UNBOUND-OBJECT?.
   ;;
   ((P x) (asm '= (V-simple-operand x) (KN UNBOUND-OBJECT)))
   ((E x) (nop)))

 (define-core-primitive-operation immediate? safe
   ;;This is the implementation of the Scheme function IMMEDIATE?.
   ;;
   ((P x)
    (make-conditional
	(tag-test (V-simple-operand x) fx-mask fx-tag)
      (K #t)
      (tag-test (V-simple-operand x) 7 7)))
   ((E x) (nop)))

 (define-core-primitive-operation boolean? safe
   ;;This is the implementation of the Scheme function BOOLEAN?.
   ;;
   ((P x)
    (tag-test (V-simple-operand x) bool-mask bool-tag))
   ((E x) (nop)))

 (define-core-primitive-operation $forward-ptr? unsafe
   ;;Primitive operation.  When a Scheme  object's memory block is moved
   ;;by the garbage collector: the first word of the old memory block is
   ;;overwritten with a  special value, the "forward  pointer".  See the
   ;;garbage collector for details.
   ;;
   ;;This predicate evaluates to true if X is such a machine word.
   ;;
   ;;This  definition  must  be  kept   in  sync  with  the  C  language
   ;;preprocessor constant IK_FORWARD_PTR.
   ;;
   ;;FIXME  At present  this operation  is  not exported  by any  Scheme
   ;;library, should it be?  (Marco Maggi; Oct 17, 2012)
   ;;
   ((P x)
    (asm '= (V-simple-operand x) (K -1)))
   ((E x)
    (nop)))

 (define-core-primitive-operation pointer-value unsafe
   ;;FIXME What is this for?  (Marco Maggi; Oct 17, 2012)
   ;;
   ((V x) (asm 'logand
	       (asm 'srl (V-simple-operand x) (K 1))
	       (K (* -1 fx-scale))))
   ((P x) (K #t))
   ((E x) (nop)))

 (define-core-primitive-operation $arg-list unsafe
   ;;Return  the  value  of  the  field "arg_list"  in  the  C  language
   ;;structure PCB.
   ;;
   ((V) (asm 'mref PC-REGISTER (K pcb-arg-list)))
   ((P) (K #t))
   ((E) (nop)))

 (define-core-primitive-operation $collect-key unsafe
   ;;Return  the value  of the  field  "collect_key" in  the C  language
   ;;structure PCB.
   ;;
   ((V)   (asm 'mref PC-REGISTER (K pcb-collect-key)))
   ((E x) (asm 'mset PC-REGISTER (K pcb-collect-key) (V-simple-operand x))))

 (define-core-primitive-operation $memq safe
   ((P x ls)
    (struct-case ls
      ((constant ls)
       (if (list? ls)
	   ;;We assume that a list hard-coded in the source is not "very
	   ;;long",   so  we   unroll  the   search  into   sequence  of
	   ;;conditionals.
	   (with-tmp ((x (V-simple-operand x)))
	     (let loop ((ls ls))
	       (cond ((null? ls)
		      (K #f))
		     ((null? (cdr ls))
		      (asm '= x (V-simple-operand (K (car ls)))))
		     (else
		      (make-conditional
			  (asm '= x (V-simple-operand (K (car ls))))
			(K #t) ;return a boolean
			(loop (cdr ls)))))))
	 (interrupt)))
      ((known expr)
       (cogen-pred-$memq x expr))
      (else
       (interrupt))))
   ((V x ls)
    (struct-case ls
      ((constant ls)
       (if (list? ls)
	   ;;We assume that a list hard-coded in the source is not "very
	   ;;long",   so  we   unroll  the   search  into   sequence  of
	   ;;conditionals.
	   (with-tmp ((x (V-simple-operand x)))
	     (let loop ((ls ls))
	       (cond ((null? ls)
		      (KN BOOL-FALSE-OBJECT))
		     (else
		      (make-conditional
			  (asm '= x (V-simple-operand (K (car ls))))
			(V-simple-operand (K ls)) ;return the value
			(loop (cdr ls)))))))
	 (interrupt)))
      ((known expr)
       (cogen-value-$memq x expr))
      (else
       (interrupt))))
   ((E x ls)
    (nop)))

 (define-core-primitive-operation memq safe
   ((P x ls)
    (cogen-pred-$memq x ls))
   ((V x ls)
    (cogen-value-$memq x ls))
   ((E x ls)
    (struct-case ls
      ((constant ls)
       (if (list? ls)
	   (nop)
	 (interrupt)))
      ((known expr)
       (cogen-effect-memq x expr))
      (else
       (interrupt)))))

 (define-core-primitive-operation memv safe
   ((V x ls)
    (struct-case ls
      ((constant lsv)
       (if (and (list? lsv)
		(andmap equable? lsv))
	   (cogen-value-$memq x ls)
	 (interrupt)))
      ((known expr)
       (cogen-value-memv x expr))
      (else
       (interrupt))))
   ((P x ls)
    (struct-case ls
      ((constant lsv)
       (if (and (list? lsv)
		(andmap equable? lsv))
	   (cogen-pred-$memq x ls)
	 (interrupt)))
      ((known expr)
       (cogen-pred-memv x expr))
      (else
       (interrupt))))
   ((E x ls)
    (struct-case ls
      ((constant val)
       (if (list? val)
	   (nop)
	 (interrupt)))
      ((known expr)
       (cogen-effect-memv x expr))
      (else
       (interrupt)))))

 /section)


;;;; pairs
;;
;;A pair is a  fixed-length block of memory composed of two  machine words; the least
;;significant bits of a reference to a pair are the pair tag.
;;
;;  |------------------------|-------------| reference to pair
;;        heap offset           pair tag
;;
(section

 (define-core-primitive-operation pair? safe
   ((P x)
    (struct-case x
      ((known x.expr x.type)
       (case (T:pair? x.type)
	 ((yes)
	  ;;We know the  result of the predicate;  we can discard X  because, being a
	  ;;simplified operand, we know it has no side effects.
	  (K #t))
	 ((no)
	  ;;We know the  result of the predicate;  we can discard X  because, being a
	  ;;simplified operand, we know it has no side effects.
	  (K #f))
	 (else
	  (cogen-pred-pair? x.expr))))
      ((constant x.const)
       (if (pair? x.const)
	   (K #t)
	 (K #f)))
      (else
       (tag-test (V-simple-operand x) pair-mask pair-tag))))
   ((E x)
    (nop)))

 (define-core-primitive-operation cons safe
   ((V a d)
    (with-tmp ((t (asm 'alloc (K pair-size) (K pair-tag))))
      (asm 'mset t (KN off-car) (V-simple-operand a))
      (asm 'mset t (KN off-cdr) (V-simple-operand d))
      t))
   ((P a d)
    (K #t))
   ((E a d)
    (nop)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $car unsafe
   ((V x)
    (asm 'mref  (V-simple-operand x) (KN off-car)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $cdr unsafe
   ((V x)
    (asm 'mref  (V-simple-operand x) (KN off-cdr)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $set-car! unsafe
   ((E x v)
    (with-tmp ((tx (V-simple-operand x)))
      (asm 'mset tx (KN off-car) (V-simple-operand v))
      (smart-dirty-vector-set tx v))))

 (define-core-primitive-operation $set-cdr! unsafe
   ((E x v)
    (with-tmp ((tx (V-simple-operand x)))
      (asm 'mset tx (KN off-cdr) (V-simple-operand v))
      (smart-dirty-vector-set tx v))))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation car safe
   ((V x)
    (define (%error-wrong-operand)
      (if (option.strict-r6rs)
	  (interrupt)
	(compile-time-operand-core-type-error __who__
	  "expected pair as primitive operation argument" (unparse-recordized-code/sexp x))))
    (struct-case x
      ((known x.expr x.type)
       (case (T:pair? x.type)
      	 ((yes)
      	  (cogen-value-$car x.expr))
      	 ((no)
      	  (%error-wrong-operand))
      	 (else
      	  (cogen-value-car x.expr))))
      ((constant x.const)
       (if (pair? x.const)
      	   (cogen-value-$car x)
      	 (%error-wrong-operand)))
      ((var)
       ;;This is a special case in which we  know "(V-simple-operand x)" would just return X itself;
       ;;so we avoid creating a temporary location.
       (multiple-forms-sequence
      	(interrupt-unless-pair x)
      	(asm 'mref x (KN off-car))))
      (else
       (with-tmp ((tx (V-simple-operand x)))
	 (interrupt-unless-pair tx)
	 (asm 'mref tx (KN off-car))))))
   ((E x)
    (assert-pair x 'car)))

 (define-core-primitive-operation cdr safe
   ((V x)
    (define (%error-wrong-operand)
      (if (option.strict-r6rs)
	  (interrupt)
	(compile-time-operand-core-type-error __who__
	  "expected pair as primitive operation argument" (unparse-recordized-code/sexp x))))
    (struct-case x
      ((known x.expr x.type)
       (case (T:pair? x.type)
      	 ((yes)
      	  (cogen-value-$cdr x.expr))
      	 ((no)
      	  (%error-wrong-operand))
      	 (else
      	  (cogen-value-cdr x.expr))))
      ((constant x.const)
       (if (pair? x.const)
      	   (cogen-value-$cdr x)
      	 (%error-wrong-operand)))
      ((var)
       ;;This is a special case in which we  know "(V-simple-operand x)" would just return X itself;
       ;;so we avoid creating a temporary location.
       (multiple-forms-sequence
      	(interrupt-unless-pair x)
      	(asm 'mref x (KN off-cdr))))
      (else
       (with-tmp ((tx (V-simple-operand x)))
	 (interrupt-unless-pair tx)
	 (asm 'mref tx (KN off-cdr))))))
   ((E x)
    (assert-pair x 'cdr)))

 (define-core-primitive-operation set-car! safe
   ((E x v)
    (define (%error-wrong-operand)
      (if (option.strict-r6rs)
	  (interrupt)
	(compile-time-operand-core-type-error __who__
	  "expected pair as primitive operation argument" (unparse-recordized-code/sexp x))))
    (struct-case x
      ((known x.expr x.type)
       (case (T:pair? x.type)
	 ((yes)
	  (cogen-value-$set-car! x.expr))
	 ((no)
	  (%error-wrong-operand))
	 (else
	  (cogen-value-set-car! x.expr))))
      ((constant x.const)
       (if (pair? x.const)
	   (if (option.strict-r6rs)
	       (cogen-value-$set-car! x)
	     (compile-time-error 'set-car!
	       "invalid mutation of pair datum" (unparse-recordized-code/sexp x)))
	 (%error-wrong-operand)))
      (else
       (with-tmp ((tx (V-simple-operand x)))
	 (interrupt-unless-pair tx)
	 (asm 'mset tx (KN off-car) (V-simple-operand v))
	 (smart-dirty-vector-set tx v))))))

 (define-core-primitive-operation set-cdr! safe
   ((E x v)
    (define (%error-wrong-operand)
      (if (option.strict-r6rs)
	  (interrupt)
	(compile-time-operand-core-type-error __who__
	  "expected pair as primitive operation argument" (unparse-recordized-code/sexp x))))
    (struct-case x
      ((known x.expr x.type)
       (case (T:pair? x.type)
	 ((yes)
	  (cogen-value-$set-cdr! x.expr))
	 ((no)
	  (%error-wrong-operand))
	 (else
	  (cogen-value-set-cdr! x.expr))))
      ((constant x.const)
       (if (pair? x.const)
	   (if (option.strict-r6rs)
	       (cogen-value-$set-cdr! x)
	     (compile-time-error 'set-cdr!
	       "invalid mutation of pair datum" (unparse-recordized-code/sexp x)))
	 (%error-wrong-operand)))
      (else
       (with-tmp ((tx (V-simple-operand x)))
	 (interrupt-unless-pair tx)
	 (asm 'mset tx (KN off-cdr) (V-simple-operand v))
	 (smart-dirty-vector-set tx v))))))

;;; --------------------------------------------------------------------

 (define (%expand-cxr val ls)
   ;;Return a struct  instance representing recordized code for a  nested sequence of
   ;;car and  cdr calls.  LS  must be a  list of symbols  "a" and "d"  representing a
   ;;nested sequence of car and cdr operations.
   ;;
   ;;For example:
   ;;
   ;;   (%expand-cxr x '(a d))
   ;;
   ;;returns recordized code representing internal CDR and external CAR:
   ;;
   ;;   (with-tmp ((tmp (asm 'mref x off-cdr)))
   ;;     (interrupt-unless-pair tmp)
   ;;     (asm 'mref tmp off-car))
   ;;
   ;;and so it implements CADR.
   ;;
   (if (pair? ls)
       (with-tmp ((item (%expand-cxr val (cdr ls))))
	 (interrupt-unless-pair item)
	 (asm 'mref item (if (eq? 'a (car ls))
			     (KN off-car)
			   (KN off-cdr))))
     (V-simple-operand val)))

 (define-core-primitive-operation caar   safe ((V x) (%expand-cxr x '(a a))))
 (define-core-primitive-operation cadr   safe ((V x) (%expand-cxr x '(a d))))
 (define-core-primitive-operation cdar   safe ((V x) (%expand-cxr x '(d a))))
 (define-core-primitive-operation cddr   safe ((V x) (%expand-cxr x '(d d))))

 (define-core-primitive-operation caaar  safe ((V x) (%expand-cxr x '(a a a))))
 (define-core-primitive-operation caadr  safe ((V x) (%expand-cxr x '(a a d))))
 (define-core-primitive-operation cadar  safe ((V x) (%expand-cxr x '(a d a))))
 (define-core-primitive-operation caddr  safe ((V x) (%expand-cxr x '(a d d))))
 (define-core-primitive-operation cdaar  safe ((V x) (%expand-cxr x '(d a a))))
 (define-core-primitive-operation cdadr  safe ((V x) (%expand-cxr x '(d a d))))
 (define-core-primitive-operation cddar  safe ((V x) (%expand-cxr x '(d d a))))
 (define-core-primitive-operation cdddr  safe ((V x) (%expand-cxr x '(d d d))))

 (define-core-primitive-operation caaaar safe ((V x) (%expand-cxr x '(a a a a))))
 (define-core-primitive-operation caaadr safe ((V x) (%expand-cxr x '(a a a d))))
 (define-core-primitive-operation caadar safe ((V x) (%expand-cxr x '(a a d a))))
 (define-core-primitive-operation caaddr safe ((V x) (%expand-cxr x '(a a d d))))
 (define-core-primitive-operation cadaar safe ((V x) (%expand-cxr x '(a d a a))))
 (define-core-primitive-operation cadadr safe ((V x) (%expand-cxr x '(a d a d))))
 (define-core-primitive-operation caddar safe ((V x) (%expand-cxr x '(a d d a))))
 (define-core-primitive-operation cadddr safe ((V x) (%expand-cxr x '(a d d d))))
 (define-core-primitive-operation cdaaar safe ((V x) (%expand-cxr x '(d a a a))))
 (define-core-primitive-operation cdaadr safe ((V x) (%expand-cxr x '(d a a d))))
 (define-core-primitive-operation cdadar safe ((V x) (%expand-cxr x '(d a d a))))
 (define-core-primitive-operation cdaddr safe ((V x) (%expand-cxr x '(d a d d))))
 (define-core-primitive-operation cddaar safe ((V x) (%expand-cxr x '(d d a a))))
 (define-core-primitive-operation cddadr safe ((V x) (%expand-cxr x '(d d a d))))
 (define-core-primitive-operation cdddar safe ((V x) (%expand-cxr x '(d d d a))))
 (define-core-primitive-operation cddddr safe ((V x) (%expand-cxr x '(d d d d))))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation list safe
   ((V)		;this is the case of: (list)
    (KN NULL-OBJECT))
   ((V . arg*)	;this is the case of: (list 1 2 3)
    (let ((len   (length arg*))	;number of pairs
	  (arg*^ (map V-simple-operand arg*)))
      ;;Allocate on the heap enough room for all the pairs.
      (with-tmp ((first-pair (asm 'alloc
				  (K (align (* len pair-size)))
				  (K pair-tag))))
	;;Store the first value in the car of the first pair.
	(asm 'mset first-pair (KN off-car) (car arg*^))
	;;Store null in the cdr of the last pair.
	(asm 'mset first-pair
	     (K (+ off-cdr (* (sub1 len) pair-size)))
	     (KN NULL-OBJECT))
	(let loop ((arg*^  (cdr arg*^))
		   (offset pair-size)) ;offset in bytes of the next pair
	  (if (pair? arg*^)
	      (with-tmp ((tmp (asm 'int+ first-pair (K offset))))
		;;Store a value in the car of this pair.
		(asm 'mset tmp (KN off-car) (car arg*^))
		;;Store a reference to this pair in the cdr of the previous pair.
		(asm 'mset tmp (K (fx- off-cdr pair-size)) tmp)
		(loop (cdr arg*^) (+ offset pair-size)))
	    first-pair))
	)))
   ((P . arg*)
    (K #t))
   ((E . arg*)
    (nop)))

 (define-core-primitive-operation cons* safe
   ((V)		;this is the invalid case: (cons*)
    (interrupt))
   ((V x)	;this is the case: (cons* '(1 2 3)) = (1 2 3)
    (V-simple-operand x))
   ((V a . a*)
    (let ((arg*^ (map V-simple-operand a*))
	  (len   (length a*)))	;number of pairs
      ;;Allocate on the heap enough room for all the pairs.  Notice that
      ;;a multiple of the PAIR-SIZE is automatically aligned.
      (with-tmp ((first-pair (asm 'alloc
				  (K (* len pair-size))
				  (K pair-tag))))
	(asm 'mset first-pair (KN off-car) (V-simple-operand a))
	(let loop ((arg*^  arg*^)
		   (offset pair-size)) ;offset in bytes of the next pair
	  (if (null? (cdr arg*^))
	      ;;Store the last argument (which  should be a list) in the
	      ;;cdr of the last pair; return the first pair.
	      ;;
	      ;;Notice  that, here,  OFFSET  references  the first  byte
	      ;;*after* the last pair.
	      (multiple-forms-sequence
	       (asm 'mset first-pair
		    (K (+ (- offset pair-size) off-cdr))
		    (car arg*^))
	       first-pair)
	    (with-tmp ((tmp (asm 'int+ first-pair (K offset))))
	      ;;Store a value in the car of this pair.
	      (asm 'mset tmp (KN off-car) (car arg*^))
	      ;;Store  a  reference to  this  pair  in  the cdr  of  the
	      ;;previous pair.
	      (asm 'mset tmp (K (fx- off-cdr pair-size)) tmp)
	      (loop (cdr arg*^) (+ offset pair-size)))))
	)))
   ((P)
    (interrupt))
   ((P x)
    (P x))
   ((P a . a*)
    (K #t))
   ((E)
    (interrupt))
   ((E . a*)
    (nop)))

 /section)


;;;; vectors
;;
;;Vectors are  variable length  blocks of  memory referenced  by machine
;;words tagged  as vectors.  The  first machine  word of a  vector block
;;contains a fixnum representing the  vector length; this means that the
;;first word of a vector is tagged as a fixnum.
;;
;; |------------------------|-------------| reference to vector
;;       heap pointer         vector tag
;;
;; |------------------------|-------------| vector first word
;;      number of words       fixnum tag
;;
;;After the length machine word comes the data area: an array of machine
;;words, one for each vector slot; slot indexes are zero--based.
;;
;;       0   1   2   3   4   5   6   7
;; |---|---|---|---|---|---|---|---|---| vector memory block
;;   ^ |...............................|
;;   |               slots
;; length
;; fixnum
;;
;;A vector is capable of holding at most a number of values equal to the
;;return value  of GREATEST-FIXNUM.  The fixnum  representing the vector
;;length, interpreted as raw signed  integer, also represents the number
;;of bytes in the data area.
;;
;;A fixnum representing  the index of slot N, interpreted  as raw signed
;;integer, also represents the offset in  bytes of the firts byte of the
;;slot with respect the beginning of the data area.
;;
(section

 (module (vector-range-check)

   (define (vector-range-check maybe-vector maybe-idx)
     ;;THE-VECTOR must be a struct instance representing recordized code
     ;;which, once evaluated, it is known to return a Scheme vector.
     ;;
     ;;MAYBE-IDX must be a  struct instance representing recordized code
     ;;which, once  evaluated, *should*  return a fixnum  representing a
     ;;valid vector index, but we are not sure about it.
     ;;
     ;;Generate recordized code to check  at run time that: MAYBE-IDX is
     ;;a fixnum and also it is in the correct range for a slot index.
     ;;
     (struct-case maybe-vector
       ((known expr type)
	(case (T:vector? type)
	  ((yes)
	   (%check-vector expr maybe-idx))
	  ((no)
	   (interrupt))
	  (else
	   (%check-non-vector expr maybe-idx))))
       (else
	(%check-non-vector maybe-vector maybe-idx))))

   (module (%check-non-vector)

     (define (%check-non-vector maybe-vector maybe-idx)
       ;;MAYBE-VECTOR must be a  struct instance representing recordized
       ;;code which,  once evaluated,  *should* return a  Scheme vector,
       ;;but we are not sure about it.
       ;;
       ;;MAYBE-IDX  must be  a struct  instance representing  recordized
       ;;code   which,  once   evaluated,  *should*   return  a   fixnum
       ;;representing a valid slot index, but we are not sure about it.
       ;;
       ;;Generate   recordized  code   to  check   at  run   time  that:
       ;;MAYBE-VECTOR is  actually a vector;  MAYBE-IDX is a  fixnum and
       ;;also it is in the correct range for a slot index.
       ;;
       (struct-case maybe-idx
	 ((constant val)
	  (if (and (target-platform-fixnum? val)
		   (>= val 0))
	      (%check-fx maybe-vector maybe-idx)
	    (%check-? maybe-vector maybe-idx)))
	 ((known expr type)
	  (case (T:fixnum? type)
	    ((yes)
	     (%check-fx maybe-vector expr))
	    ((maybe)
	     (vector-range-check maybe-vector expr))
	    (else
	     (fprintf (current-error-port)
		      "*** Vicare warning: vector check with mismatch index tag ~s\n"
		      type)
	     (vector-range-check maybe-vector expr))))
	 (else
	  (%check-? maybe-vector maybe-idx))))

     (define (%check-fx maybe-vector idx)
       ;;MAYBE-VECTOR must be a  struct instance representing recordized
       ;;code which,  once evaluated,  *should* return a  Scheme vector,
       ;;but we are not sure about it.
       ;;
       ;;IDX  must be  a  struct instance  representing recordized  code
       ;;which, once evaluated, it is known to return a fixnum.
       ;;
       ;;Generate   recordized  code   to  check   at  run   time  that:
       ;;MAYBE-VECTOR is actually a vector;  IDX is in the correct range
       ;;for a slot index.
       ;;
       (multiple-forms-sequence
	(interrupt-unless
	 (tag-test (V-simple-operand maybe-vector) vector-mask vector-tag))
	(with-tmp ((len (cogen-value-$vector-length maybe-vector)))
	  ;;FIXME Should not  the two forms below be  in reversed order?
	  ;;Is this the  order because the first check is  faster and it
	  ;;will  work with  any machine  word?  (Marco  Maggi; Oct  18,
	  ;;2012)
	  (interrupt-unless
	   (asm 'u< (V-simple-operand idx) len))
	  (interrupt-unless-fixnum len))))

     (define (%check-? maybe-vector maybe-idx)
       ;;MAYBE-VECTOR must be a  struct instance representing recordized
       ;;code which,  once evaluated,  *should* return a  Scheme vector,
       ;;but we are not sure about it.
       ;;
       ;;MAYBE-IDX  must be  a struct  instance representing  recordized
       ;;code   which,  once   evaluated,  *should*   return  a   fixnum
       ;;representing a  valid vector index,  but we are not  sure about
       ;;it.
       ;;
       ;;Generate   recordized  code   to  check   at  run   time  that:
       ;;MAYBE-VECTOR is  actually a vector;  MAYBE-IDX is a  fixnum and
       ;;also it is in the correct range for a slot index.
       ;;
       (multiple-forms-sequence
	(interrupt-unless
	 (tag-test (V-simple-operand maybe-vector) vector-mask vector-tag))
	(with-tmp ((len (cogen-value-$vector-length maybe-vector)))
	  ;;FIXME Should not  the two forms below be  in reversed order?
	  ;;Is this the  order because the first check is  faster and it
	  ;;will work with any machine word?   What kind of check is the
	  ;;second one?  (Marco Maggi; Oct 18, 2012)
	  (interrupt-unless
	   (asm 'u< (V-simple-operand maybe-idx) len))
	  (with-tmp ((t (asm 'logor len (V-simple-operand maybe-idx))))
	    (interrupt-unless-fixnum t)))))

     #| end of module: %check-non-vector |#)

   (module (%check-vector)

     (define (%check-vector the-vector maybe-idx)
       ;;THE-VECTOR must  be a  struct instance  representing recordized
       ;;code  which, once  evaluated, it  is known  to return  a Scheme
       ;;vector.
       ;;
       ;;MAYBE-IDX  must be  a struct  instance representing  recordized
       ;;code   which,  once   evaluated,  *should*   return  a   fixnum
       ;;representing a  valid vector index,  but we are not  sure about
       ;;it.
       ;;
       ;;Generate recordized code  to check at run  time that: MAYBE-IDX
       ;;is a  fixnum and  also it is  in the correct  range for  a slot
       ;;index.
       ;;
       (struct-case maybe-idx
	 ((constant val)
	  (if (and (target-platform-fixnum? val)
		   (>= val 0))
	      (%check-fx the-vector maybe-idx)
	    (interrupt)))
	 ((known expr type)
	  (case (T:fixnum? type)
	    ((yes)
	     (%check-fx the-vector expr))
	    ((no)
	     (interrupt))
	    (else
	     (%check-vector the-vector expr))))
	 (else
	  (%check-? the-vector maybe-idx))))

     (define (%check-fx the-vector idx)
       ;;THE-VECTOR must  be a  struct instance  representing recordized
       ;;code  which, once  evaluated, it  is known  to return  a Scheme
       ;;vector.
       ;;
       ;;IDX  must be  a  struct instance  representing recordized  code
       ;;which, once evaluated, it is known to return a fixnum.
       ;;
       ;;Generate recordized code  to check at run time that:  IDX is in
       ;;the correct range for a slot index.
       ;;
       (with-tmp ((len (cogen-value-$vector-length the-vector)))
	 (interrupt-unless
	  (asm 'u< (V-simple-operand idx) len))))

     (define (%check-? the-vector maybe-idx)
       ;;THE-VECTOR must  be a  struct instance  representing recordized
       ;;code  which, once  evaluated, it  is known  to return  a Scheme
       ;;vector.
       ;;
       ;;MAYBE-IDX  must be  a struct  instance representing  recordized
       ;;code   which,  once   evaluated,  *should*   return  a   fixnum
       ;;representing a valid slot index, but we are not sure about it.
       ;;
       ;;Generate recordized code  to check at run  time that: MAYBE-IDX
       ;;is a  fixnum and  also it is  in the correct  range for  a slot
       ;;index.
       ;;
       (multiple-forms-sequence
	(interrupt-unless-fixnum (V-simple-operand maybe-idx))
	(with-tmp ((len (cogen-value-$vector-length the-vector)))
	  ;;FIXME Should not  the two forms below be  in reversed order?
	  ;;Is this the  order because the first check is  faster and it
	  ;;will work with any machine word?   What kind of check is the
	  ;;second one?  (Marco Maggi; Oct 18, 2012)
	  (interrupt-unless
	   (asm 'u< (V-simple-operand maybe-idx) len))
	  (with-tmp ((t (asm 'logor len (V-simple-operand maybe-idx))))
	    (interrupt-unless-fixnum t)))))

     #| end of module: %check-vector |# )

   #| end of module: vector-range-check |# )

;;; --------------------------------------------------------------------

 (define-core-primitive-operation vector? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag fx-mask fx-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $make-vector unsafe
   ;;Notice that  the code  below does not  initialise the  vector's data
   ;;area leaving the items set to  whatever is there on the Scheme heap;
   ;;this can be bad for garbage  collection if the newly built vector is
   ;;moved before the items are  set to some correct Scheme object.  This
   ;;is why  the unsafe  operations library defines  a $MAKE-CLEAN-VECTOR
   ;;macro which builds a new vector  and clears the data area filling it
   ;;with zero fixnums (which is  fast from C language using "memset()").
   ;;(Marco Maggi; Jan 18, 2012)
   ;;
   ((V len)
    (struct-case len
      ((constant len.val)
       ;;LEN.VAL is  an exact  integer (possibly a  bignum) representing
       ;;the binary representation of the number of slots.
       (if (and (target-platform-fixnum? len.val) #f)
	   (interrupt)
	 (with-tmp ((vec (asm 'alloc
			      (K (align (+ (* len.val wordsize) disp-vector-data)))
			      (K vector-tag))))
	   (asm 'mset vec
		(K off-vector-length)
		(K (* len.val fx-scale)))
	   vec)))
      ((known len.expr)
       (cogen-value-$make-vector len.expr))
      (else
       ;;Here LEN is recordized code  which, when evaluated, must return
       ;;a finxum representing the number of slots.
       (with-tmp ((alen (align-code (V-simple-operand len) disp-vector-data)))
	 (with-tmp ((vec (asm 'alloc alen (K vector-tag))))
	   (asm 'mset vec (K off-vector-length) (V-simple-operand len))
	   vec)))))
   ((P len)
    (K #t))
   ((E len)
    (nop)))

 (define-core-primitive-operation make-vector safe
   ((V len)
    (with-tmp ((vec (make-forcall "ikrt_make_vector1" (list (V-simple-operand len)))))
      (interrupt-when
       (asm '= vec (K 0)))
      vec)))

 (define-core-primitive-operation $vector-ref unsafe
   ((V vec idx)
    (or (struct-case idx
	  ((constant idx.val)
	   ;;LEN.VAL   is  an   exact   integer   (possibly  a   bignum)
	   ;;representing  the binary  representation of  the number  of
	   ;;slots.
	   (and (target-platform-fixnum? idx.val)
		(fx>= idx.val 0)
		(asm 'mref (V-simple-operand vec) (K (+ (* idx.val wordsize) off-vector-data)))))
	  ((known idx.expr)
	   (cogen-value-$vector-ref vec idx.expr))
	  (else
	   #f))
	;;Notice that  IDX is  not multiplied by  the WORDSIZE;  this is
	;;because  IDX is  recordized  code that,  once evaluated,  must
	;;return a fixnum representing the index of the IDX-th slot in a
	;;vector; also,  taken as  a "long",  such value  represents the
	;;offset in bytes of the word in the IDX-th slot.
	(asm 'mref (V-simple-operand vec) (asm 'int+ (V-simple-operand idx) (K off-vector-data)))))
   ((E vec idx)
    (nop)))

 (define-core-primitive-operation $vector-length unsafe
   ((V vec)
    (asm 'mref (V-simple-operand vec) (K off-vector-length)))
   ((E vec)
    (nop))
   ((P vec)
    (K #t)))

 (define-core-primitive-operation vector-length safe
   ((V vec)
    (struct-case vec
      ((known vec.expr vec.type)
       (case (T:vector? vec.type)
	 ((yes)
	  (cogen-value-$vector-length vec.expr))
	 ((no)
	  (interrupt))
	 (else
	  (cogen-value-vector-length vec.expr))))
      ((constant vec.const)
       ;;Here the operand has no type description, but still we can check the operand
       ;;at compile-time.
       (cond ((vector? vec.const)
	      (cogen-value-$vector-length vec))
	     ((option.strict-r6rs)
	      (interrupt))
	     (else
	      (compile-time-operand-core-type-error __who__
		"expected vector as operand" (unparse-recordized-code vec)))))
      (else
       (multiple-forms-sequence
	(interrupt-unless
	 (tag-test (V-simple-operand vec) vector-mask vector-tag))
	(with-tmp ((vec.len (cogen-value-$vector-length vec)))
	  (interrupt-unless-fixnum vec.len)
	  vec.len)))))
   ((E vec)
    (struct-case vec
      ((known vec.expr vec.type)
       (case (T:vector? vec.type)
	 ((yes)
	  (nop))
	 ((no)
	  (interrupt))
	 (else
	  (cogen-effect-vector-length vec.expr))))
      ((constant vec.const)
       ;;Here the operand has no type description, but still we can check the operand
       ;;at compile-time.
       (cond ((vector? vec.const)
	      (nop))
	     ((option.strict-r6rs)
	      (interrupt))
	     (else
	      (compile-time-operand-core-type-error __who__
		"expected vector as operand" (unparse-recordized-code vec)))))
      (else
       (multiple-forms-sequence
	(interrupt-unless
	 (tag-test (V-simple-operand vec) vector-mask vector-tag))
	(with-tmp ((vec.len (cogen-value-$vector-length vec)))
	  (interrupt-unless-fixnum vec.len))))))
   ((P vec)
    (multiple-forms-sequence
     (cogen-effect-vector-length vec)
     (K #t))))

 (define-core-primitive-operation vector-ref safe
   ((V vec idx)
    (multiple-forms-sequence
     (vector-range-check vec idx)
     (cogen-value-$vector-ref vec idx)))
   ((E vec idx)
    (vector-range-check vec idx)))

 (define-core-primitive-operation $vector-set! unsafe
   ((E vec idx item)
    (struct-case idx
      ((constant idx.val)
       ;;IDX.VAL  is an  exact  integer (possibly  a  bignum) being  the
       ;;binary representation of a fixnum slot index.
       (if (not (target-platform-fixnum? idx.val))
	   (interrupt)
	 (mem-assign item (V-simple-operand vec) (+ (* idx.val wordsize) off-vector-data))))
      ((known idx.expr)
       (cogen-effect-$vector-set! vec idx.expr item))
      (else
       ;;Here IDX is recordized code  which, when evaluated, must return
       ;;a fixnum representing a slot index.
       ;;
       ;;Notice  that I  is  not  multiplied by  the  WORDSIZE; this  is
       ;;because I is  a fixnum representing the index of  the I-th slot
       ;;in a vector; also, taken as  a "long", it represents the offset
       ;;in bytes of the word in the I-th slot.
       (mem-assign item (asm 'int+ (V-simple-operand vec) (V-simple-operand idx)) off-vector-data)))))

 (define-core-primitive-operation vector-set! safe
   ((E vec idx item)
    (multiple-forms-sequence
     (vector-range-check vec idx)
     (cogen-effect-$vector-set! vec idx item))))

 (define-core-primitive-operation vector safe
   ((V . arg*)
    ;;This is the case:
    ;;
    ;;   (vector 1 2 3)
    ;;
    ;;here we  know the number  of arguments  and we assume  that vector
    ;;constructor calls hard-coded  in the source have  a "small" number
    ;;of  arguments;   so  we  generate  unrolled   recordized  code  to
    ;;initialise the items.
    ;;
    (with-tmp ((vec (asm 'alloc
			 (K (align (fx+ disp-vector-data (* (length arg*) wordsize))))
			 (K vector-tag))))
      (multiple-forms-sequence
       ;;Store the vector length in the first word.
       (asm 'mset vec (K off-vector-length) (K (* (length arg*) wordsize)))
       (let recur ((arg*^  (map V-simple-operand arg*))
		   (offset off-vector-data))
	 (if (pair? arg*^)
	     (make-seq (asm 'mset vec (K offset) (car arg*^))
		       (recur (cdr arg*^) (+ offset wordsize)))
	   vec)))))
   ((E . arg*)
    (nop))
   ((P . arg*)
    (K #t)))

 /section)


;;;; symbols
;;
;;A symbol  is a fixed length  memory block referenced  by machine words
;;tagged as vectors.  The first machine word of a symbol block is tagged
;;has  symbol  in  its  least  significant  bits and  it  has  the  most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to symbol
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| symbol first word
;;     all set to zero         symbol tag
;;
;;A symbol  memory block  is 6  words wide  being the  following fields:
;;string, ustring, value, proc, plist.
;;
(section

 (define-core-primitive-operation symbol? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask symbol-primary-tag #f symbol-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $make-symbol unsafe
   ((V str)
    (with-tmp ((sym (asm 'alloc
			 (K (align symbol-record-size))
			 (K symbol-primary-tag))))
      (asm 'mset sym (K off-symbol-record-tag)     (K symbol-tag))
      (asm 'mset sym (K off-symbol-record-string)  (V-simple-operand str))
      (asm 'mset sym (K off-symbol-record-ustring) (K 0))
      (asm 'mset sym (K off-symbol-record-value)   (KN UNBOUND-OBJECT))
      (asm 'mset sym (K off-symbol-record-proc)    (KN UNBOUND-OBJECT))
      (asm 'mset sym (K off-symbol-record-plist)   (KN NULL-OBJECT))
      sym))
   ((P str)
    (K #t))
   ((E str)
    (nop)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $symbol-string unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-symbol-record-string)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $set-symbol-string! unsafe
   ((E x v)
    (mem-assign v (V-simple-operand x) off-symbol-record-string)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $symbol-unique-string unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-symbol-record-ustring)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $set-symbol-unique-string! unsafe
   ((E x v)
    (mem-assign v (V-simple-operand x) off-symbol-record-ustring)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $symbol-plist unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-symbol-record-plist)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $set-symbol-plist! unsafe
   ((E x v)
    (mem-assign v (V-simple-operand x) off-symbol-record-plist)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $symbol-value unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-symbol-record-value)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $set-symbol-value! unsafe
   ((E x v)
    (with-tmp ((x^ (V-simple-operand x)))
      (asm 'mset x^ (K off-symbol-record-value) (V-simple-operand v))
      (dirty-vector-set x^))))

;;; --------------------------------------------------------------------

 ;;NOTE Whenever binary code performs a call to a global closure object,
 ;;it does the following:
 ;;
 ;;* From the relocation vector of the current code object: retrieve the
 ;;  loc gensym of the procedure to call.
 ;;
 ;;* From the loc gensym: extract the value of the "proc" slot, which is
 ;;  meant to be a closure object.
 ;;
 ;;* Actually call the closure object.
 ;;
 (define-core-primitive-operation $symbol-proc unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-symbol-record-proc)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $set-symbol-proc! unsafe
   ((E x v)
    (with-tmp ((x^ (V-simple-operand x)))
      (asm 'mset x^ (K off-symbol-record-proc) (V-simple-operand v))
      (dirty-vector-set x^))))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $set-symbol-value/proc! unsafe
   ((E x v)
    (with-tmp ((x^ (V-simple-operand x))
	       (v^ (V-simple-operand v)))
      (asm 'mset x^ (K off-symbol-record-value) v^)
      (asm 'mset x^ (K off-symbol-record-proc)  v^)
      (dirty-vector-set x^))))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation top-level-value safe
   ;;Expect the single argument to be a  loc gensym associated to a top level lexical
   ;;binding; extract the value from the slot "value" of the symbol object and return
   ;;it.  If the value is the unbound object: raise an exception.
   ;;
   ;;For a full  explanation of this operation: see the  description of the primitive
   ;;function TOP-LEVEL-VALUE.
   ;;
   ;;NOTE For the common case in which  the argument is known, at compile-time, to be
   ;;a symbol: this  primitive operation will *not* generate code  that validates the
   ;;argument at run-time.
   ;;
   ((V sym)
    (struct-case sym
      ((constant sym.val)
       (cond ((symbol? sym.val)
	      (with-tmp ((val (cogen-value-$symbol-value sym)))
		;;Raise an exception if the slot "value" is set to the unbound object.
		(interrupt-when
		 (cogen-pred-$unbound-object? val))
		val))
	     ((option.strict-r6rs)
	      ;;Report error at run-time.
	      (interrupt))
	     (else
	      ;;Report error at compile-time.
	      (compile-time-operand-core-type-error __who__
		"expected symbol as loc gensym argument" sym.val))))
      ((known sym.expr)
       ;;The argument is an  expression whose return value type is  known.  Act as if
       ;;sym is a CONSTANT.
       (cogen-value-top-level-value sym.expr))
      (else
       ;;Here SYM is  recordized code which, when evaluated, should  return a symbol;
       ;;but here we do not know which value will be returned.
       (with-tmp ((sym^ (V-simple-operand sym)))
	 ;;Test at run-time if it is a symbol.
	 (interrupt-unless
	  (cogen-pred-symbol? sym^))
	 (with-tmp ((val (cogen-value-$symbol-value sym^)))
	   ;;Raise an exception if the slot "value" is set to the unbound object.
	   (interrupt-when
	    (cogen-pred-$unbound-object? val))
	   val)))))
   ((E sym)
    ;;The difference between  the V execution context and the  E execution context is
    ;;that: here we do *not* return the  object in the symbol's field "value", rather
    ;;we raise an error if the "value" field is set to the unbound object.
    ;;
    (struct-case sym
      ((constant sym.val)
       (cond ((symbol? sym.val)
	      (with-tmp ((val (cogen-value-$symbol-value sym)))
		(interrupt-when
		 (cogen-pred-$unbound-object? val))))
	     ((option.strict-r6rs)
	      ;;Report error at run-time.
	      (interrupt))
	     (else
	      ;;Report error at compile-time.
	      (compile-time-operand-core-type-error __who__
		"expected symbol as loc gensym argument" sym.val))))
      ((known sym.expr)
       ;;The argument is an  expression whose return value type is  known.  Act as if
       ;;sym is a CONSTANT.
       (cogen-effect-top-level-value sym.expr))
      (else
       ;;Here SYM is  recordized code which, when evaluated, should  return a symbol;
       ;;but here we do not know which value will be returned.
       (with-tmp ((sym^ (V-simple-operand sym)))
	 ;;Test at run-time if it is a symbol.
	 (interrupt-unless
	  (cogen-pred-symbol? sym^))
	 (with-tmp ((val (cogen-value-$symbol-value sym^)))
	   ;;Raise an exception if the slot "value" is set to the unbound object.
	   (interrupt-when
	    (cogen-pred-$unbound-object? val))))))))

 ;;Commented out because unused.
 ;;
 ;; (define-core-primitive-operation $init-symbol-function! unsafe
 ;;   ((E sym v)
 ;;    (with-tmp ((sym^ (V-simple-operand sym))
 ;; 	       (v^   (V-simple-operand v)))
 ;;      (asm 'mset sym^ (K off-symbol-record-proc) v^)
 ;;      (dirty-vector-set sym^))))

 /section)


;;;; fixnums
;;
;;A fixnum  is a machine  word whose least  significant bits are  set to
;;zero.  R6RS states that  a fixnum must have at least  24 bits in which
;;to store  the number; on a  32-bit platform, 30 bits  are available to
;;store the number:
;;
;; (greatest-fixnum)       => +536870911
;; (expt 2 29)             => +536870912
;; (- (expt 2 29) 1)       => +536870911
;;
;; (least-fixnum)          => -536870912
;; (- (expt 2 29))         => -536870912
;;
(section

 (define-core-primitive-operation fixnum? safe
   ((P x)
    (tag-test (V-simple-operand x) fx-mask fx-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation fixnum-width safe
   ((V)
    (K (fxsll NUMBER-OF-BITS-IN-FIXNUM-REPRESENTATION fx-shift)))
   ((E)
    (nop))
   ((P)
    (K #t)))

 (define-core-primitive-operation least-fixnum safe
   ((V)
    (K (sll (- (expt 2 (- NUMBER-OF-BITS-IN-FIXNUM-REPRESENTATION 1))) fx-shift)))
   ((E)
    (nop))
   ((P)
    (K #t)))

 (define-core-primitive-operation greatest-fixnum safe
   ((V)
    (K (sll (- (expt 2 (- NUMBER-OF-BITS-IN-FIXNUM-REPRESENTATION 1)) 1) fx-shift)))
   ((E)
    (nop))
   ((P)
    (K #t)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $fxzero? unsafe
   ((P x)
    (asm '= (V-simple-operand x) (K 0)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $fxnegative? unsafe
   ((P x)
    (asm '< (V-simple-operand x) (K 0)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $fxpositive? unsafe
   ((P x)
    (asm '> (V-simple-operand x) (K 0)))
   ((E x)
    (nop)))

 ;; (define-core-primitive-operation $fxeven? unsafe
 ;;   ((P x)
 ;;    (asm '= (asm 'logand (V-simple-operand x) (K 1)) (K 0)))
 ;;   ((E x)
 ;;    (nop)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $fx= unsafe
   ((P x y)
    (asm '= (V-simple-operand x) (V-simple-operand y)))
   ((E x y)
    (nop)))

 (define-core-primitive-operation $fx< unsafe
   ((P x y)
    (asm '< (V-simple-operand x) (V-simple-operand y)))
   ((E x y)
    (nop)))

 (define-core-primitive-operation $fx<= unsafe
   ((P x y)
    (asm '<= (V-simple-operand x) (V-simple-operand y)))
   ((E x y)
    (nop)))

 (define-core-primitive-operation $fx> unsafe
   ((P x y)
    (asm '> (V-simple-operand x) (V-simple-operand y)))
   ((E x y)
    (nop)))

 (define-core-primitive-operation $fx>= unsafe
   ((P x y)
    (asm '>= (V-simple-operand x) (V-simple-operand y)))
   ((E x y)
    (nop)))

 (define-core-primitive-operation $fxadd1 unsafe
   ((V x)
    (cogen-value-$fx+ x (K 1)))
   ((P x)
    (K #t))
   ((E x)
    (nop)))

 (define-core-primitive-operation $fxsub1 unsafe
   ((V x)
    (cogen-value-$fx+ x (K -1)))
   ((P x)
    (K #t))
   ((E x)
    (nop)))

 (define-core-primitive-operation $fx+ unsafe
   ((V x y)
    (asm 'int+ (V-simple-operand x) (V-simple-operand y)))
   ((P x y)
    (K #t))
   ((E x y)
    (nop)))

 ;; (define-core-primitive-operation $fx+/overflow safe
 ;;   ((V x y)
 ;;    (asm 'int+/overflow (V-simple-operand x) (V-simple-operand y))))

 (define-core-primitive-operation $fx* unsafe
   ((V a b)
    (struct-case a
      ((constant a.val)
       ;;A.VAL is  an exact  integer (possibly  a bignum)  whose payload
       ;;bits are the binary representation of the fixnum A.
       (interrupt-unless-fx a.val)
       ;;Since we want the  result to be a fixnum, there  is no need to:
       ;;untag A, multiply A, retag A; we just multiply the tagged A.
       (asm 'int* (V-simple-operand b) (K a.val)))
      ((known a)
       (cogen-value-$fx* a b))
      (else
       ;;Here A is recordized code  which, when evaluated, must return a
       ;;fixnum.
       (struct-case b
	 ((constant b.val)
	  ;;B.VAL is an exact integer  (possibly a bignum) whose payload
	  ;;bits are the binary representation of the fixnum B.
	  (interrupt-unless-fx b.val)
	  (asm 'int* (V-simple-operand a) (K b.val)))
	 ((known b.expr)
	  (cogen-value-$fx* a b.expr))
	 (else
	  ;;Here B is recordized code which, when evaluated, must return
	  ;;a fixnum.  By right-shifting B we untag it.
	  ;;
	  ;;Since we  want the result to  be a fixnum, there  is no need
	  ;;to:  untag A,  multiply A,  retag  A; we  just multiply  the
	  ;;tagged A.
	  (asm 'int* (V-simple-operand a) (prm-UNtag-as-fixnum (V-simple-operand b))))))))
   ((P x y)
    (K #t))
   ((E x y)
    (nop)))

 (define-core-primitive-operation $fxlognot unsafe
   ((V x)
    (cogen-value-$fxlogxor x (K -1)))
   ((P x)
    (K #t))
   ((E x)
    (nop)))

 (define-core-primitive-operation $fxlogand unsafe
   ((V x y) (asm 'logand (V-simple-operand x) (V-simple-operand y)))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-core-primitive-operation $fxlogor unsafe
   ((V x y) (asm 'logor (V-simple-operand x) (V-simple-operand y)))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-core-primitive-operation $fxlogxor unsafe
   ((V x y) (asm 'logxor (V-simple-operand x) (V-simple-operand y)))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-core-primitive-operation $fx- unsafe
   ((V x)   (asm 'int- (K 0) (V-simple-operand x)))
   ((V x y) (asm 'int- (V-simple-operand x) (V-simple-operand y)))
   ((P x y) (K #t))
   ((E x y) (nop)))

 (define-core-primitive-operation $fxsll unsafe
   ;;Shift-left logic.
   ;;
   ((V x numbits)
    ;;Both X and NUMBITS must be fixnums.
    (struct-case numbits
      ((constant numbits.val)
       ;;Here NUMBITS.VAL  must be an  exact integer whose  payload bits
       ;;are the  binary representation of  the shift amount  as machine
       ;;word.
       ;;
       ;;Question:  Should we  not check  also that  NUMBITS.VAL is  not
       ;;bigger than the number of bits in  a fixnum?  We could as it is
       ;;done by FXARITHMETIC-SHIFT,  but we decide not  to because this
       ;;is a low level operation.  (Marco Maggi; Oct 18, 2012)
       (interrupt-unless-fx numbits.val)
       (asm 'sll (V-simple-operand x) (K numbits.val)))
      ((known numbits.expr)
       (cogen-value-$fxsll x numbits.expr))
      (else
       ;;Here NUMBITS is recordized code that must return a fixnum.
       ;;
       ;;By right-shifting NUMBITS we untag  it.  Since we want a fixnum
       ;;as result: there is no need  to untag X, left-shift X, retag X;
       ;;we just left-shift the tagged X.
       (asm 'sll (V-simple-operand x) (prm-UNtag-as-fixnum (V-simple-operand numbits))))))
   ((P x i)
    (K #t))
   ((E x i)
    (nop)))

 (define-core-primitive-operation $fxsra unsafe
   ;;Shift-right arithmetic: right-shifts the bits  of the operand by an
   ;;amount of positions; if the most  significant bit of the operand is
   ;;set to:
   ;;
   ;;0 - arithmetic right-shifting introduces bits  set to 0 on the left
   ;;    of the machine word;
   ;;
   ;;1 - arithmetic right-shifting introduces bits  set to 1 on the left
   ;;    of the machine word.
   ;;
   ;;On a 32-bit platform, let's say  we have the following machine word
   ;;as operand (representing the positive  fixnum 63161283) and we want
   ;;to right-shift it by 10:
   ;;
   ;;      byte4    byte3    byte1    byte0
   ;;    00001111 00001111 00001111 00001100
   ;;   |................................|..|
   ;;           payload bits              fixnum tag
   ;;                         |.............|
   ;;                          shift amount
   ;;
   ;;after arithmetic right-shifting, we get the machine word:
   ;;
   ;;      byte4    byte3    byte1    byte0
   ;;    00000000 00000011 11000011 11000011
   ;;
   ;;and to make  the resulting fixnum, we  have to set to  0 the least
   ;;significant bits  corresponding to the  fixnum tag; we do  it with
   ;;the following bitwise logic AND operation:
   ;;
   ;;      byte4    byte3    byte1    byte0
   ;;    00000000 00000011 11000011 11000011 AND
   ;;    11111111 11111111 11111111 11111100 =
   ;;    -------------------------------------
   ;;    00000000 00000011 11000011 11000000
   ;;    |...............................|..|
   ;;             payload bits            fixnum tag
   ;;
   ;;whose result is the fixnum:
   ;;
   ;;   (integer->machine-word #b00000000000000111100001111000000)
   ;;   => 61680
   ;;
   ;;On a 32-bit platform, let's say  we have the following machine word
   ;;as  operand (representing  the negative  fixnum -473709629)  and we
   ;;want to right-shift it by 10:
   ;;
   ;;      byte4    byte3    byte1    byte0
   ;;    10001111 00001111 00001111 00001100
   ;;   |................................|..|
   ;;           payload bits              fixnum tag
   ;;                         |.............|
   ;;                          shift amount
   ;;
   ;;after arithmetic right-shifting, we get the machine word:
   ;;
   ;;      byte4    byte3    byte1    byte0
   ;;    11111111 11100011 11000011 11000011
   ;;               ^
   ;;        original sign bit
   ;;
   ;;and to make  the resulting fixnum, we  have to set to  0 the least
   ;;significant bits  corresponding to the  fixnum tag; we do  it with
   ;;the following bitwise logic AND operation:
   ;;
   ;;      byte4    byte3    byte1    byte0
   ;;    11111111 11100011 11000011 11000011 AND
   ;;    11111111 11111111 11111111 11111100 =
   ;;    -------------------------------------
   ;;    11111111 11100011 11000011 11000000
   ;;    |...............................|..|
   ;;             payload bits            fixnum tag
   ;;
   ;;whose result is the fixnum:
   ;;
   ;;   (integer->machine-word #b11111111111000111100001111000000)
   ;;   => -462608
   ;;
   ;;QUESTION: How do we make the  machine word that acts as AND-mask to
   ;;set to 0 the fixnum tag bits?
   ;;
   ;;ANSWER:  It is  a  constant value  known at  compile  time, so  the
   ;;compiler computes it  as struct instance of type  CONSTANT; at this
   ;;stage in  the compilation process,  such constant must be  an exact
   ;;integer whose  payload bits  are the  binary representation  of the
   ;;mask word.
   ;;
   ;;On a 32-bit platform, the payload bits of such exact integer are:
   ;;
   ;;   11111111 11111111 11111111 11111100   machine word mask
   ;;
   ;;we can represent this integer as the fixnum:
   ;;
   ;;   11111111 11111111 11111111 11110000   mask fixnum representation
   ;;
   ;;knowing that,  when the final  compilation step will  be performed,
   ;;such fixnum will be artithmetically right-shifted by 2.
   ;;
   ;;Then, how do we compute the  fixnum representation of the mask?  We
   ;;take the fixnum  with all the payload  bits set to 1,  which is -1,
   ;;and left-shift  it by the number  of bits in the  fixnum tag; there
   ;;are two ways to do it, on a 32-bit platform:
   ;;
   ;;   (define fx-shift 2)
   ;;   (define fx-scale 4)
   ;;
   ;;   (fxarithmetic-shift-left -1 fx-shift)	=> -4
   ;;   (* -1 fx-scale)				=> -4
   ;;
   ;;and notice that:
   ;;
   ;;   (machine-word->integer -4)
   ;;   => #b11111111111111111111111111110000
   ;;
   ((V x numbits)
    ;;Both X and  NUMBITS must be fixnums.
    (struct-case numbits
      ((constant numbits.val)
       ;;Here NUMBITS.VAL  must be an  exact integer whose  payload bits
       ;;are the  binary representation of  the shift amount  as machine
       ;;word.
       (interrupt-unless-fx numbits.val)
       (asm 'logand
	    (asm 'sra (V-simple-operand x) (K (let ((word-numbits NUM-OF-BITS-IN-WORD))
				 (if (< numbits.val word-numbits)
				     numbits.val
				   (- word-numbits 1)))))
	    (K (* -1 fx-scale))))
      ((known numbits.expr)
       (cogen-value-$fxsra x numbits.expr))
      (else
       ;;Here NUMBITS is recordized code that must return a fixnum.
       ;;
       (with-tmp ((numbits.val (prm-UNtag-as-fixnum (V-simple-operand numbits))))
	 (with-tmp ((numbits.val (let ((word-numbits NUM-OF-BITS-IN-WORD))
				   (make-conditional (asm '< numbits.val (K word-numbits))
				       numbits.val
				     (K (- word-numbits 1))))))
	   (asm 'logand
		(asm 'sra (V-simple-operand x) numbits.val)
		(K (* -1 fx-scale))))))))
   ((P x i)
    (K #t))
   ((E x i)
    (nop)))

 (define-core-primitive-operation $fxquotient unsafe
   ((V a b)
    (with-tmp ((b (V-simple-operand b)))
      (prm-tag-as-fixnum (asm 'int-quotient (V-simple-operand a) b))))
   ((P a b)
    (K #t))
   ((E a b)
    (nop)))

 ;;FIXME This  is used nowhere, is  it finished?  (Marco Maggi;  Oct 19,
 ;;2012)
 (define-core-primitive-operation $int-quotient unsafe
   ((V a b)
    (prm-tag-as-fixnum (asm 'int-quotient (V-simple-operand a) (V-simple-operand b)))))

 ;;FIXME This is  used nowhere, and it looks  unfinished?  (Marco Maggi;
 ;;Oct 19, 2012)
 (define-core-primitive-operation $int-remainder unsafe
   ((V a b)
    (asm 'int-remainder (V-simple-operand a))))

 ;;This  implementation is  wrong as  documented in  issue 9:  incorrect
 ;;results for negative numbers.  It is replaced with another version in
 ;;"ikarus.fixnums.sls".  (Marco Maggi)
 ;;
 ;; (define-core-primitive-operation $fxmodulo unsafe
 ;;   ((V a b)
 ;;    (with-tmp ((b (V-simple-operand b)))
 ;;      (with-tmp ((c (asm 'logand b
 ;;                       (asm 'sra (asm 'logxor b (V-simple-operand a))
 ;;                          (K (sub1 (* 8 wordsize)))))))
 ;;        (asm 'int+ c (asm 'int-remainder (V-simple-operand a) b)))))
 ;;   ((P a b) (K #t))
 ;;   ((E a b) (nop)))

 (define-core-primitive-operation $fxabs unsafe
   ((V x)
    (with-tmp ((n (V-simple-operand x)))
      (make-conditional (asm '< n (K 0))
	  (asm 'int- (K 0) n)
	n)))
   ((P x)
    (K #t))
   ((E x)
    (nop)))

 ;;FIXME This looks  to be used nowhere; is it  finished?  (Marco Maggi;
 ;;Oct 19, 2012)
 (define-core-primitive-operation $fxinthash unsafe
   ((V key)
    (with-tmp ((k (V-simple-operand key)))
      (with-tmp ((k (asm 'int+ k (asm 'logxor (asm 'sll k (K 15)) (K -1)))))
	(with-tmp ((k (asm 'logxor k (asm 'sra k (K 10)))))
	  (with-tmp ((k (asm 'int+ k (asm 'sll k (K 3)))))
	    (with-tmp ((k (asm 'logxor k (asm 'sra k (K 6)))))
	      (with-tmp ((k (asm 'int+ k (asm 'logxor (asm 'sll k (K 11)) (K -1)))))
		(with-tmp ((k (asm 'logxor k (asm 'sra k (K 16)))))
		  (asm 'sll k (K fx-shift)))))))))))
;;;(define inthash
;;;    (lambda (key)
;;;      ;static int inthash(int key) { /* from Bob Jenkin's */
;;;      ;  /* http://burtleburtle.net/bob/hash/doobs.html */
;;;      ;  key += ~(key << 15);
;;;      ;  key ^=  (key >> 10);
;;;      ;  key +=  (key << 3);
;;;      ;  key ^=  (key >> 6);
;;;      ;  key += ~(key << 11);
;;;      ;  key ^=  (key >> 16);
;;;      ;  return key;
;;;      ;}
;;;      (let* ((key (fx+ key (fxlognot (fxsll key 15))))
;;;             (key (fxlogxor key (fxsra key 10)))
;;;             (key (fx+ key (fxsll key 3)))
;;;             (key (fxlogxor key (fxsra key 6)))
;;;             (key (fx+ key (fxlognot (fxsll key 11))))
;;;             (key (fxlogxor key (fxsra key 16))))
;;;        key)))

 /section)


;;;; bignums
;;
;;A bignum is a variable length memory block referenced by machine words
;;tagged as vectors.  The first machine word of a bignum block is tagged
;;has bignum in its least significant  bits; then comes a sign bit, zero
;;if positive; the remaining  most significant bits represent the number
;;of words in the memory block after the first one.
;;
;;  |------------------------|-------------| reference to bignum
;;        heap offset          vector tag
;;
;;                       sign bit
;;  |----------------------|-|-------------| bignum first word
;;     number of words         bignum tag
;;
;;   |----|-----|-----|-----|-----| bignum memory block
;;    1st  limb0 limb1 limb2 limb3
;;
(section

 (define-core-primitive-operation bignum? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag bignum-mask bignum-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $bignum-positive? unsafe
   ;;Extract the  sign bit from the  first word and evaluate  to true if
   ;;the sign bit is set to zero.
   ;;
   ((P x)
    (asm '= (asm 'logand
		 (asm 'mref (V-simple-operand x) (K off-bignum-tag))
		 (K bignum-sign-mask))
	 (K 0)))
   ((E x)
    (nop)))

 (define-core-primitive-operation $bignum-size unsafe
   ;;Extract the  number of limbs from  the first word and  return it as
   ;;fixnum.
   ;;
   ((V x)
    (asm 'sll  ;In one step:  multiply the number  of limbs by  the word
		;size and tag the result as fixnum.
	 (asm 'sra ;extract the number of limbs
	      (asm 'mref (V-simple-operand x) (K off-bignum-tag))
	      (K bignum-length-shift))
	 ;;This constant must be an exact integer whose payload bits are
	 ;;the binary representation of the left-shift amount.
	 (K (* 2 fx-shift)))))

 (define-core-primitive-operation $bignum-byte-ref unsafe
   ;;Return a fixnum representing a byte  from the array of limbs in the
   ;;data area.  On a *little*  endian 32-bit platform, the byte indexes
   ;;are as follows:
   ;;
   ;;      limb0         limb1         limb2         limb3
   ;;  |--|--|--|--| |--|--|--|--| |--|--|--|--| |--|--|--|--| ...
   ;;    0  1  2  3    4  5  6  7    8  9 10 11   12 13 14 15
   ;;
   ;;on a *big* endian 32-bit platform, the byte indexes are as follows:
   ;;
   ;;      limb0         limb1         limb2         limb3
   ;;  |--|--|--|--| |--|--|--|--| |--|--|--|--| |--|--|--|--| ...
   ;;    3  2  1  0    7  6  5  4   11 10  9  8   15 14 13 12
   ;;
   ;;Remember that i686 is little endian.
   ;;
   ((V bigN byte-idx)
    (struct-case byte-idx
      ((constant byte-idx.val)
       ;;BYTE-IDX.VAL is  an exact  integer whose  payload bits  are the
       ;;binary representataion of the selected byte index.
       ;;
       ;;FIXME  Endianness dependency!!!   Works only  on little  endian
       ;;platforms.  (Marco Maggi; Oct 19, 2012)
       (interrupt-unless-fx byte-idx.val)
       (prm-tag-as-fixnum
	(prm-isolate-least-significant-byte
	 (asm 'mref (V-simple-operand bigN) (K (+ byte-idx.val off-bignum-data))))))
      ((known byte-idx.expr)
       (cogen-value-$bignum-byte-ref bigN byte-idx.expr))
      (else
       ;;Here BYTE-IDX  is recordized  code which, when  evaluated, must
       ;;return a fixnum.
       ;;
       ;;FIXME  Endianness dependency!!!   Works only  on little  endian
       ;;platforms.  (Marco Maggi; Oct 19, 2012)
       (prm-tag-as-fixnum
	(prm-isolate-least-significant-byte
	 (asm 'mref (V-simple-operand bigN)
	      (asm 'int+
		   (prm-UNtag-as-fixnum (V-simple-operand byte-idx))
		   (K off-bignum-data)))))
       ;;
       ;;The one below  is the original Ikarus version; I  do not really
       ;;know why it was written this way,  it may well be that I am too
       ;;stupid and/or  ignorant to  understand.  (Marco Maggi;  Oct 19,
       ;;2012)
       ;;
       ;; (prm-tag-as-fixnum
       ;;      (asm 'srl ;shift-right logic.  FIXME bref.  (Abdulaziz Ghuloum)
       ;;           (asm 'mref (V-simple-operand bigN)
       ;;                (asm 'int+
       ;;                     (prm-UNtag-as-fixnum (V-simple-operand byte-idx))
       ;;                     (K (- off-bignum-data (- wordsize 1)))))
       ;;           (K (* (- wordsize 1) 8))))
       ;;
       )))
   ((P bigN byte-idx)
    (K #t))
   ((E bigN byte-idx)
    (nop)))

 /section)


;;;; flonums
;;
;;A flonum  is a fixed length  memory block referenced  by machine words
;;tagged as vectors.  The first machine word of a flonum block is tagged
;;has  flonum  in  its  least  significant  bits and  it  has  the  most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to flonum
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| flonum first word
;;     all set to zero         flonum tag
;;
;;A  flonum memory  block is  16 bytes  wide on  both 32-bit  and 64-bit
;;platforms; the data area of the memory  block is 8 bytes wide, on both
;;32-bit and 64-bit  platforms: it contains a  double-precision IEEE 754
;;floating-point number as specified by the hosting platform.
;;
;;To allow  for the same  binary layout on  both platforms, on  a 32-bit
;;platform the actual number is stored in the last two words:
;;
;;    1st word     2nd word     3rd word     4th word
;;  |------------|------------|------------|------------|
;;   tagged word     unused           data words
;;                            |.........................|
;;                                      flonum
;;
;;on a 64-bit platform the actual number is stored in the second word:
;;
;;            1st word                 2nd word
;;  |-------------------------|-------------------------|
;;           tagged word               data word
;;                            |.........................|
;;                                      flonum
;;
;;We assume  that the endianness  in which the floating-point  number is
;;stored in the data area is the same as the machine words.  On the i686
;;platform: little endian.
;;
(section

 (define ($flop-aux op fl0 fl1)
   ;;Flonum operation between two operands.
   ;;
   (with-tmp ((x (asm 'alloc
		      (K (align flonum-size))
		      (K vector-tag))))
     ;;Tag the first word of the result as flonum.
     (asm 'mset x (K off-flonum-tag) (K flonum-tag))
     ;;Load the first operand in a register for flonums.
     (asm 'fl:load  (V-simple-operand fl0) (K off-flonum-data))
     ;;Perform the operation between the register and FL1.
     (asm op        (V-simple-operand fl1) (K off-flonum-data))
     ;;Store the result from the register into memory referenced by X.
     (asm 'fl:store x       (K off-flonum-data))
     x))

 (define ($flop-aux* op fl fl*)
   ;;Flonum operation  between three or  more operands (but also  upon a
   ;;single operand).
   ;;
   (with-tmp ((x (asm 'alloc
		      (K (align flonum-size))
		      (K vector-tag))))
     ;;Tag the first word of the result as flonum.
     (asm 'mset x (K off-flonum-tag) (K flonum-tag))
     ;;Load the first operand in a register for flonums.
     (asm 'fl:load (V-simple-operand fl) (K off-flonum-data))
     (let recur ((fl* fl*))
       (if (pair? fl*)
	   (make-seq
	    ;;Perform  the operation  between  the register  and the  next
	    ;;operand.
	    (asm op (V-simple-operand (car fl*)) (K off-flonum-data))
	    (recur (cdr fl*)))
	 (nop)))
     ;;Store the result from the register into memory referenced by X.
     (asm 'fl:store x (K off-flonum-data))
     x))

 (define ($flcmp-aux op fl0 fl1)
   ;;Flonum comparison operation.
   ;;
   (make-seq
    ;;Load the first operand in a register for flonums.
    (asm 'fl:load (V-simple-operand fl0) (K off-flonum-data))
    ;;Perform the operation between the register and FL1.
    (asm op       (V-simple-operand fl1) (K off-flonum-data))))

 (define (check-flonums ls code)
   ;;CODE must be a struct instance representing recordized code.
   ;;
   ;;Do what is possible at compilation time to validate LS as a list of
   ;;flonum  objects, then  return  recordized code  that performs  with
   ;;valid arguments CODE.
   ;;
   (if (pair? ls)
       (struct-case (car ls)
	 ((constant v)
	  (if (flonum? v)
	      (check-flonums (cdr ls) code)
	    (interrupt)))
	 ((known expr type)
	  (case (T:flonum? type)
	    ((yes)
	     (check-flonums (cdr ls) code))
	    ((no)
	     (interrupt))
	    (else
	     (check-flonums (cons expr (cdr ls)) code))))
	 (else
	  (check-flonums (cdr ls)
	    (with-tmp ((x (V-simple-operand (car ls))))
	      (interrupt-unless
	       (tag-test x vector-mask vector-tag))
	      (interrupt-unless
	       (asm '=
		    (asm 'mref x (K off-flonum-tag))
		    (K flonum-tag)))
	      code))))
     code))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation flonum? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag #f flonum-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $make-flonum unsafe
   ((V)
    (with-tmp ((flo (asm 'alloc
			 (K (align flonum-size))
			 (K vector-tag))))
      (asm 'mset flo (K off-flonum-tag) (K flonum-tag))
      flo))
   ((P str)
    (K #t))
   ((E str)
    (nop)))

 (define-core-primitive-operation $flonum-u8-ref unsafe
   ;;Return a fixnum representing the octect at OFFSET (a fixnum) in the
   ;;data area of FLO (a flonum).
   ;;
   ;;Notice that  the OFFSET  is positive, but  it represents  an offset
   ;;from the end of the data area; on a 32-bit platform, the indexes of
   ;;the bytes are:
   ;;
   ;;     1st word    2nd word    3rd word    4th word
   ;;  |-----------|-----------|-----------|-----------|
   ;;                          |--|--|--|--|--|--|--|--| bytes
   ;;                            7  6  5  4  3  2  1  0  OFFSETs
   ;;
   ((V flo offset)
    (struct-case offset
      ((constant offset.val)
       ;;OFFSET.VAL  is an  exact  integer whose  payload  bits are  the
       ;;binary representation of the offset as machine word.
       (unless (and (target-platform-fixnum? offset.val)
		    ;;The data area is 8 bytes wide.
		    (fx>= offset.val 0)
		    (fx<= offset.val 7))
	 (interrupt))
       (prm-tag-as-fixnum
	(prm-isolate-least-significant-byte
	 (asm 'bref (V-simple-operand flo)
	      (K (fx+ (fx- 7 offset.val) off-flonum-data))))))
      ((known offset.expr)
       (cogen-value-$flonum-u8-ref flo offset.expr))
      (else
       ;;Here OFFSET is  recordized code; this case  is not implemented.
       ;;This means that the  following will fail with "uninterruptible"
       ;;exception:
       ;;
       ;;   ($flonum-u8-ref 123.456 (read))
       ;;
       ;;after we have put an offset into the current input port.
       ;;
       ;;FIXME Why is this not implemented?  (Marco Maggi; Oct 20, 2012)
       (interrupt))))
   ((P s i)
    (K #t))
   ((E s i)
    (nop)))

 (define-core-primitive-operation $flonum-set! unsafe
   ;;Store the  octet represented by the  fixnum OCTET at OFFSET  in the
   ;;data area of the flonum FLO.
   ;;
   ;;Notice that  the OFFSET  is positive, but  it represents  an offset
   ;;from the end of the data area; on a 32-bit platform, the indexes of
   ;;the bytes are:
   ;;
   ;;     1st word    2nd word    3rd word    4th word
   ;;  |-----------|-----------|-----------|-----------|
   ;;                          |--|--|--|--|--|--|--|--| bytes
   ;;                            7  6  5  4  3  2  1  0  OFFSETs
   ;;
   ((E flo offset octet)
    (struct-case offset
      ((constant offset.val)
       ;;OFFSET.VAL  is an  exact  integer whose  payload  bits are  the
       ;;binary representation of the offset as machine word.
       (unless (and (target-platform-fixnum? offset.val)
		    ;;The data area is 8 bytes wide.
		    (fx>= offset.val 0)
		    (fx<= offset.val 7))
	 (interrupt))
       ;;store the byte
       (asm 'bset (V-simple-operand flo)
	    (K (fx+ (fx- 7 offset.val) off-flonum-data))
	    (prm-UNtag-as-fixnum (V-simple-operand octet))))
      ((known offset.expr)
       (cogen-effect-$flonum-set! flo offset.expr octet))
      (else
       ;;Here OFFSET is  recordized code; this case  is not implemented.
       ;;This means that the  following will fail with "uninterruptible"
       ;;exception:
       ;;
       ;;   ($flonum-set! 123.456 (read) 10)
       ;;
       ;;after we have put an offset into the current input port.
       ;;
       ;;FIXME Why is this not implemented?  (Marco Maggi; Oct 20, 2012)
       (interrupt)))))

 (define-core-primitive-operation $fixnum->flonum unsafe
   ((V fx)
    (boot.case-word-size
      ((32)
       (with-tmp ((flo (asm 'alloc
			    (K (align flonum-size))
			    (K vector-tag))))
	 ;;Tag the first word of the result as flonum.
	 (asm 'mset flo (K off-flonum-tag) (K flonum-tag))
	 ;;Perform the operation storing the  result in a floating point
	 ;;register.
	 (asm 'fl:from-int
	      (K 0) ; dummy
	      (prm-UNtag-as-fixnum (V-simple-operand fx)))
	 ;;Store the result from the register into memory referenced by X
	 (asm 'fl:store flo (K off-flonum-data))
	 flo))
      ((64)
       (with-tmp ((flo (cogen-value-$make-flonum)))
	 (make-forcall "ikrt_fixnum_to_flonum" (list (V-simple-operand fx) flo)))))))

;;; --------------------------------------------------------------------
;;; UNsafe arithmetic primitive operations

 (define-core-primitive-operation $fl+ unsafe
   ((V x y)
    ($flop-aux 'fl:add! x y)))

 (define-core-primitive-operation $fl- unsafe
   ((V x)
    ;;Notice that we cannot do this as: +0.0 - x, because such operation
    ;;does not handle correctly the case: +0.0 - +0.0 = -0.0.
    ($flop-aux 'fl:mul! (K -1.0) x))
   ((V x y)
    ($flop-aux 'fl:sub! x y)))

 (define-core-primitive-operation $fl* unsafe
   ((V x y)
    ($flop-aux 'fl:mul! x y)))

 (define-core-primitive-operation $fl/ unsafe
   ((V x y)
    ($flop-aux 'fl:div! x y)))

;;; --------------------------------------------------------------------
;;; safe arithmetic primitive operations

 (define-core-primitive-operation fl+ safe
   ((V)
    (K (make-object 0.0)))
   ((V x)
    (check-flonums (list x) (V-simple-operand x)))
   ((V x . x*)
    (check-flonums (cons x x*)
      ($flop-aux* 'fl:add! x x*)))
   ((P . x*)
    (check-flonums x* (K #t)))
   ((E . x*)
    (check-flonums x* (nop))))

 (define-core-primitive-operation fl* safe
   ((V)
    (K (make-object 1.0)))
   ((V x)
    (check-flonums (list x)
      (V-simple-operand x)))
   ((V x . x*)
    (check-flonums (cons x x*)
      ($flop-aux* 'fl:mul! x x*)))
   ((P . x*)
    (check-flonums x*
      (K #t)))
   ((E . x*)
    (check-flonums x*
      (nop))))

 (define-core-primitive-operation fl- safe
   ((V x)
    ;;Notice that we cannot do this as: +0.0 - x, because such operation
    ;;does not handle correctly the case: +0.0 - +0.0 = -0.0.
    (check-flonums (list x)
      ($flop-aux 'fl:mul! (K -1.0) x)))
   ((V x . x*)
    (check-flonums (cons x x*)
      ($flop-aux* 'fl:sub! x x*)))
   ((P x . x*)
    (check-flonums (cons x x*)
      (K #t)))
   ((E x . x*)
    (check-flonums (cons x x*)
      (nop))))

 (define-core-primitive-operation fl/ safe
   ((V x)
    (check-flonums (list x)
      ($flop-aux 'fl:div! (K 1.0) x)))
   ((V x . x*)
    (check-flonums (cons x x*)
      ($flop-aux* 'fl:div! x x*)))
   ((P x . x*)
    (check-flonums (cons x x*)
      (K #t)))
   ((E x . x*)
    (check-flonums (cons x x*)
      (nop))))

;;; --------------------------------------------------------------------
;;; UNsafe comparison primitive operations

 (define-core-primitive-operation $fl= unsafe
   ;;Notice that  this predicate does  not distinguish between  +0.0 and
   ;;-0.0 and this is compliant with what R6RS states.
   ;;
   ((P x y)
    ($flcmp-aux 'fl:= x y)))

 (define-core-primitive-operation $fl< unsafe
   ((P x y)
    ($flcmp-aux 'fl:< x y)))

 (define-core-primitive-operation $fl<= unsafe
   ((P x y)
    ($flcmp-aux 'fl:<= x y)))

 (define-core-primitive-operation $fl> unsafe
   ((P x y)
    ($flcmp-aux 'fl:> x y)))

 (define-core-primitive-operation $fl>= unsafe
   ((P x y)
    ($flcmp-aux 'fl:>= x y)))

;;; --------------------------------------------------------------------

;;NOTE $flzero?  is more complicated because we must check for both +0.0
;;and -0.0; so  it is not a primitive operation.   (Marco Maggi; Nov 17,
;;2012)

 (define-core-primitive-operation $flpositive? unsafe
   ((P x)
    ($flcmp-aux 'fl:> x (K +0.0))))

 (define-core-primitive-operation $flnegative? unsafe
   ((P x)
    ($flcmp-aux 'fl:< x (K -0.0))))

;;; --------------------------------------------------------------------
;;; safe comparison primitive operations

 (define-core-primitive-operation fl=? safe
   ((P x y)
    (check-flonums (list x y)
      ($flcmp-aux 'fl:= x y)))
   ((E x y)
    (check-flonums (list x y)
      (nop))))

 (define-core-primitive-operation fl<? safe
   ((P x y)
    (check-flonums (list x y)
      ($flcmp-aux 'fl:< x y)))
   ((E x y)
    (check-flonums (list x y)
      (nop))))

 (define-core-primitive-operation fl<=? safe
   ((P x y)
    (check-flonums (list x y)
      ($flcmp-aux 'fl:<= x y)))
   ((E x y)
    (check-flonums (list x y)
      (nop))))

 (define-core-primitive-operation fl>? safe
   ((P x y)
    (check-flonums (list x y)
      ($flcmp-aux 'fl:> x y)))
   ((E x y)
    (check-flonums (list x y)
      (nop))))

 (define-core-primitive-operation fl>=? safe
   ((P x y)
    (check-flonums (list x y)
      ($flcmp-aux 'fl:>= x y)))
   ((E x y)
    (check-flonums (list x y)
      (nop))))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $flonum-sbe unsafe
   ;;Given a  flonum object  FLO, inspects the  IEEE 754  floating point
   ;;number; on a 32-bit platform:
   ;;
   ;;      1st     2nd     3rd     4th
   ;;   |-------|-------|-------|-------| flonum memory block
   ;;                   |...............| floating point number
   ;;                   |-|-|-|-|-|-|-|-| 8 bytes
   ;;                           |.| BE
   ;;
   ;;on a 64-bit platform:
   ;;
   ;;        1st word        2nd word
   ;;   |---------------|---------------| flonum memory block
   ;;                   |...............| floating point number
   ;;                   |-|-|-|-|-|-|-|-| 8 bytes
   ;;                           |.| BE
   ;;
   ;;extract the most  significant 32-bit word, called BE,  and apply to
   ;;it a  logic right-shift of  20 bit-positions, so extracting  its 12
   ;;most significant bits.  Return the result encoded as fixnum.
   ;;
   ;;FIXME Endianness dependency.  (Marco Maggi; Oct 21, 2012)
   ;;
   ((V flo)
    (prm-tag-as-fixnum
     ;;extract the 12 most significant bits
     (asm 'srl
	  ;;retrieve the second data word
	  (asm 'mref32 (V-simple-operand flo)
	       (K (+ off-flonum-data 4)))
	  (K 20)))))

 /section)


;;;; ratnums
;;
;;A ratnum  is a fixed length  memory block referenced  by machine words
;;tagged as vectors.  The first machine word of a ratnum block is tagged
;;has  ratnum  in  its  least  significant  bits and  it  has  the  most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to ratnum
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| ratnum first word
;;     all set to zero         ratnum tag
;;
;;A ratnum memory block is 4 words wide; a reference to the numerator is
;;stored in the second word and a reference to the denominator is stored
;;in the third word
;;
;;     1st word     2nd word     3rd word     4th word
;;  |------------|------------|------------|------------|
;;   tagged word   numerator   denominator     unused
;;
(section

 (define-core-primitive-operation ratnum? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag #f ratnum-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $make-ratnum unsafe
   ((V num den)
    (with-tmp ((rat (asm 'alloc
			 (K (align ratnum-size))
			 (K vector-tag))))
      (asm 'mset rat (K off-ratnum-tag) (K ratnum-tag))
      (asm 'mset rat (K off-ratnum-num) (V-simple-operand num))
      (asm 'mset rat (K off-ratnum-den) (V-simple-operand den))
      rat))
   ((P str)
    (K #t))
   ((E str)
    (nop)))


 (define-core-primitive-operation $ratnum-n unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-ratnum-num))))

 (define-core-primitive-operation $ratnum-num unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-ratnum-num))))

 (define-core-primitive-operation $ratnum-d unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-ratnum-den))))

 (define-core-primitive-operation $ratnum-den unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-ratnum-den))))

 /section)


;;;; compnum
;;
;;A compnum is  a fixed length memory block  referenced by machine words
;;tagged  as vectors.   The first  machine word  of a  compnum  block is
;;tagged has compnum  in its least significant bits and  it has the most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to compnum
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| compnum first word
;;     all set to zero         compnum tag
;;
;;A compnum memory  block is 4 words wide; a reference  to the real part
;;is stored in the second word  and a reference to the imaginary part is
;;stored in the third word
;;
;;     1st word     2nd word     3rd word     4th word
;;  |------------|------------|------------|------------|
;;   tagged word   real part    imag part     unused
;;
(section

 (define-core-primitive-operation compnum? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag #f compnum-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $make-compnum unsafe
   ((V real imag)
    (with-tmp ((comp (asm 'alloc
			  (K (align compnum-size))
			  (K vector-tag))))
      (asm 'mset comp (K off-compnum-tag)  (K compnum-tag))
      (asm 'mset comp (K off-compnum-real) (V-simple-operand real))
      (asm 'mset comp (K off-compnum-imag) (V-simple-operand imag))
      comp))
   ((P str)
    (K #t))
   ((E str)
    (nop)))

 (define-core-primitive-operation $compnum-real unsafe
   ((V comp)
    (asm 'mref (V-simple-operand comp) (K off-compnum-real))))

 (define-core-primitive-operation $compnum-imag unsafe
   ((V comp)
    (asm 'mref (V-simple-operand comp) (K off-compnum-imag))))

 /section)


;;;; cflonum
;;
;;A cflonum is  a fixed length memory block  referenced by machine words
;;tagged  as vectors.   The first  machine word  of a  cflonum  block is
;;tagged has cflonum  in its least significant bits and  it has the most
;;significant bits set to zero.
;;
;;  |------------------------|-------------| reference to cflonum
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| cflonum first word
;;     all set to zero         cflonum tag
;;
;;A cflonum memory  block is 4 words wide; a reference  to the real part
;;is stored in the second word  and a reference to the imaginary part is
;;stored in the third word
;;
;;     1st word     2nd word     3rd word     4th word
;;  |------------|------------|------------|------------|
;;   tagged word   real part    imag part     unused
;;
(section

 (define-core-primitive-operation cflonum? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag #f cflonum-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $make-cflonum unsafe
   ((V real imag)
    (with-tmp ((cflo (asm 'alloc
			  (K (align cflonum-size))
			  (K vector-tag))))
      (asm 'mset cflo (K off-cflonum-tag)  (K cflonum-tag))
      (asm 'mset cflo (K off-cflonum-real) (V-simple-operand real))
      (asm 'mset cflo (K off-cflonum-imag) (V-simple-operand imag))
      cflo))
   ((P str)
    (K #t))
   ((E str)
    (nop)))

 (define-core-primitive-operation $cflonum-real unsafe
   ((V cflo)
    (asm 'mref (V-simple-operand cflo) (K off-cflonum-real))))

 (define-core-primitive-operation $cflonum-imag unsafe
   ((V cflo)
    (asm 'mref (V-simple-operand cflo) (K off-cflonum-imag))))

 /section)


;;;; generic arithmetic

(section

 (define (fixnum-fold-p op a a*)
   ;;Generate and return  recordized code that: first  validates all the
   ;;arguments as  fixnums, then  applies the operation  OP to  pairs of
   ;;arguments  and evaluates  to true  if the  predicate always  return
   ;;true.
   ;;
   (multiple-forms-sequence
    (assert-fixnums a a*)
    (let recur ((a  a)
		(a* a*))
      (if (pair? a*)
	  (let ((b (car a*)))
	    (make-conditional (asm op (V-simple-operand a) (V-simple-operand b))
		(recur b (cdr a*))
	      (K #f)))
	(K #t)))))

 (module (cogen-binary-*)

   (define (cogen-binary-* a b)
     (or (cogen-*-constant a b)
	 (cogen-*-constant b a)
	 (cogen-*-non-constants a b)))

   (define (cogen-*-constant a b)
     (struct-case a
       ((constant a.val)
	(if (target-platform-fixnum? a.val)
	    (begin
	      (interrupt)
	      (with-tmp ((b (V-simple-operand b)))
		(assert-fixnum b)
		(asm 'int*/overflow a b)))
	  (interrupt)))
       ((known a.expr)
	(cogen-*-constant a.expr b))
       (else
	#f)))

   (define (cogen-*-non-constants a b)
     (interrupt)
     (with-tmp ((a (V-simple-operand a))
		(b (V-simple-operand b)))
       (assert-fixnum a)
       (assert-fixnum b)
       (asm 'int*/overflow a (prm-UNtag-as-fixnum b))))

   #| end of module: cogen-binary-* |# )

;;; --------------------------------------------------------------------
;;; generic comparison

 (define-core-primitive-operation = safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '= a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation < safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '< a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation <= safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '<= a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation > safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '> a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation >= safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '>= a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

;;; --------------------------------------------------------------------
;;; safe fixnum comparison

 (define-core-primitive-operation fx= safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '= a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation fx< safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '< a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation fx<= safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '<= a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation fx> safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '> a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation fx>= safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '>= a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation fx=? safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '= a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation fx<? safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '< a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation fx<=? safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '<= a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation fx>? safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '> a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation fx>=? safe
   ((P)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((P a . a*)
    (fixnum-fold-p '>= a a*))
   ((E)
    ;;According R6RS: it is an error to call this without arguments.
    (interrupt))
   ((E a . a*)
    (assert-fixnums a a*)))

;;; --------------------------------------------------------------------
;;; safe fixnum arithmetic

 (define-core-primitive-operation fx+ safe
   ((V x y)
    (cogen-value-+ x y)))

 (define-core-primitive-operation fx- safe
   ((V x)
    (cogen-value-- (K 0) x))
   ((V x y)
    (cogen-value-- x y)))

 (define-core-primitive-operation fx* safe
   ((V a b)
    (cogen-binary-* a b)))

 (define-core-primitive-operation fxadd1 safe
   ((V x)
    (cogen-value-+ x (K 1))))

 (define-core-primitive-operation fxsub1 safe
   ((V x)
    (cogen-value-+ x (K -1))))

;;; --------------------------------------------------------------------
;;; safe fixnum bitwise

 (define-core-primitive-operation fxarithmetic-shift-left safe
   ((V x bitcount)
    (struct-case bitcount
      ((constant bitcount.val)
       ;;BITCOUNT.VAL is  an exact  integer whose  payload bits  are the
       ;;binary representation of the shift amount as a fixnum.
       (cond ((and (target-platform-fixnum? bitcount.val)
		   (>= bitcount.val 0)
		   (<  bitcount.val NUMBER-OF-BITS-IN-FIXNUM-REPRESENTATION))
	      (with-tmp ((x (V-simple-operand x)))
		(assert-fixnum x)
		(if (< bitcount.val 6)
		    (let recur ((i bitcount.val))
		      (if (zero? i)
			  x
			(begin
			  (interrupt)
			  (asm 'sll/overflow
			       (recur (- i 1))
			       (K 1)))))
		  (with-tmp ((x2 (asm 'sll x (K bitcount.val))))
		    (interrupt-unless
		     (asm '=
			  (asm 'sra x2 (K bitcount.val))
			  x))
		    x2))))
	     (else
	      (interrupt))))
      (else
       (with-tmp ((x (V-simple-operand x))
		  (n (V-simple-operand bitcount)))
	 (assert-fixnums x (list n))
	 (with-tmp ((n (prm-UNtag-as-fixnum n)))
	   (interrupt-when
	    (asm '< n (K 0)))
	   (interrupt-when
	    (asm '>= n (K NUMBER-OF-BITS-IN-FIXNUM-REPRESENTATION)))
	   (with-tmp ((x2 (asm 'sll x n)))
	     (interrupt-unless
	      (asm '= (asm 'sra x2 n) x))
	     x2)))))))

 (define-core-primitive-operation fxarithmetic-shift-right safe
   ((V x bitcount)
    (struct-case bitcount
      ;;FIXME Check for known types.  (Abdulaziz Ghuloum)
      ((constant bitcount.val)
       (if (and (target-platform-fixnum? bitcount.val)
		(>= bitcount.val 0)
		(<  bitcount.val NUMBER-OF-BITS-IN-FIXNUM-REPRESENTATION))
	   (prm-tag-as-fixnum
	    (asm 'sra (V-simple-operand x) (K (+ bitcount.val fx-shift))))
	 (interrupt)))
      (else
       (with-tmp ((x (V-simple-operand x))
		  (n (V-simple-operand bitcount)))
	 (assert-fixnums x (list n))
	 (with-tmp ((n (prm-UNtag-as-fixnum n)))
	   (interrupt-when
	    (asm '<  n (K 0)))
	   (interrupt-when
	    (asm '>= n (K NUMBER-OF-BITS-IN-FIXNUM-REPRESENTATION)))
	   ;;Untagging and  then tagging as  fixnum makes sure  that the
	   ;;bits of the fixnum tag are zero.
	   (prm-tag-as-fixnum
	    (prm-UNtag-as-fixnum
	     (asm 'sra x n)))))))))

;;; --------------------------------------------------------------------
;;; safe generic arithmetic

 (define-core-primitive-operation - safe
   ((V a)
    ;;FIXME Why  do we interrupt  here?  If I  remove the interrupt:  this integrated
    ;;body uncovers an error in a subsequent compiler pass.
    ;;
    ;;My current understanding is: by interrupting  here we jump directly to the full
    ;;primitive function  call, skipping the  integrated body.   But this is  not the
    ;;whole story: by  examining the generated assembly code (possibly  by turning of
    ;;the source optimiser), I see a  correct SHORTCUT generated, without direct jump
    ;;to the function call.
    ;;
    ;;I need to understand and fix this!!! (Marco Maggi; Fri Sep 19, 2014)
    (interrupt)
    (multiple-forms-sequence
     (assert-fixnums a '())
     (asm 'int-/overflow (K 0) (V-simple-operand a))))
   ((V a . a*)
    ;;FIXME Why  do we interrupt  here?  If I  remove the interrupt:  this integrated
    ;;body uncovers an error in a subsequent compiler pass.
    ;;
    ;;My current understanding is: by interrupting  here we jump directly to the full
    ;;primitive function  call, skipping the  integrated body.   But this is  not the
    ;;whole story: by  examining the generated assembly code (possibly  by turning of
    ;;the source optimiser), I see a  correct SHORTCUT generated, without direct jump
    ;;to the function call.
    ;;
    ;;I need to understand and fix this!!! (Marco Maggi; Fri Sep 19, 2014)
    (interrupt)
    (multiple-forms-sequence
     (assert-fixnums a a*)
     (let recur ((a  (V-simple-operand a))
    		 (a* a*))
       (if (pair? a*)
    	   (recur (asm 'int-/overflow a (V-simple-operand (car a*)))
    		  (cdr a*))
    	 a))))
   ((P a . a*)
    (multiple-forms-sequence
     (assert-fixnums a a*)
     (K #t)))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation + safe
   ((V)
    (K 0))
   ((V a . a*)
    ;;FIXME Why  do we interrupt  here?  If I  remove the interrupt:  this integrated
    ;;body uncovers an error in a subsequent compiler pass.
    ;;
    ;;My current understanding is: by interrupting  here we jump directly to the full
    ;;primitive function  call, skipping the  integrated body.   But this is  not the
    ;;whole story: by  examining the generated assembly code (possibly  by turning of
    ;;the source optimiser), I see a  correct SHORTCUT generated, without direct jump
    ;;to the function call.
    ;;
    ;;I need to understand and fix this!!! (Marco Maggi; Fri Sep 19, 2014)
    (interrupt)
    (multiple-forms-sequence
     (assert-fixnums a a*)
     (let recur ((a  (V-simple-operand a))
    		 (a* a*))
       (if (pair? a*)
    	   (recur (asm 'int+/overflow a (V-simple-operand (car a*)))
    		  (cdr a*))
    	 a))))
   ((P)
    (K #t))
   ((P a . a*)
    (multiple-forms-sequence
     (assert-fixnums a a*)
     (K #t)))
   ((E)
    (nop))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation add1 safe
   ((V x)
    (cogen-value-+ x (K 1))))

 (define-core-primitive-operation sub1 safe
   ((V x)
    (cogen-value-+ x (K -1))))

 (define-core-primitive-operation * safe
   ((V)
    (K (fxsll 1 fx-shift)))
   ((V a b)
    (cogen-binary-* a b))
   ((P)
    (K #t))
   ((P a . a*)
    (multiple-forms-sequence
     (assert-fixnums a a*)
     (K #t)))
   ((E)
    (nop))
   ((E a . a*)
    (assert-fixnums a a*)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation bitwise-and safe
   ((V)
    (K (fxsll -1 fx-shift)))
   ((V a . a*)
    (interrupt)
    (multiple-forms-sequence
     (assert-fixnums a a*)
     (let loop ((a  (V-simple-operand a))
		(a* a*))
       (if (pair? a*)
	   (loop (asm 'logand a (V-simple-operand (car a*))) (cdr a*))
	 a))))
   ((P)
    (K #t))
   ((P a . a*)
    (multiple-forms-sequence
     (assert-fixnums a a*)
     (K #t)))
   ((E)
    (nop))
   ((E a . a*)
    (assert-fixnums a a*)))

 (define-core-primitive-operation zero? safe
   ((P x)
    (multiple-forms-sequence
     (assert-fixnum x)
     (cogen-pred-$fxzero? x)))
   ((E x) (assert-fixnum x)))

 (module (cogen-pred-div
	  cogen-effect-div
	  cogen-value-div)

   (define-core-primitive-operation div safe
     ((V x n)
      (struct-case n
	((constant i)
	 (cond ((and (target-platform-fixnum? i)
		     (> i 0)
		     (log2 i))
		=> (lambda (bits)
		     (multiple-forms-sequence
		      (interrupt-unless
		       (cogen-pred-fixnum? x))
		      (prm-tag-as-fixnum
		       (asm 'sra (V-simple-operand x) (K (+ bits fx-shift)))))))
	       (else
		(interrupt))))
	((known expr)
	 (cogen-value-div x expr))
	(else
	 (interrupt)))))

   (define (log2 n)
     (let recur ((n n)
		 (i 0))
       (cond ((fxzero? (fxand n 1))
	      (recur (fxsra n 1)
		     (+ i 1)))
	     ((= n 1)
	      i)
	     (else
	      #f))))

   #| end of module |# )

 (define-core-primitive-operation quotient safe
   ((V num den)
    (struct-case den
      ((constant den.val)
       (if (eqv? den.val 2)
	   (multiple-forms-sequence
	    (interrupt-unless
	     (cogen-pred-fixnum? num))
	    (make-conditional
		(asm '< (V-simple-operand num) (K 0))
		(asm 'logand
		     (asm 'int+
			  (asm 'sra (V-simple-operand num) (K 1))
			  (K (fxsll 1 (sub1 fx-shift))))
		     (K (fxsll -1 fx-shift)))
	      (asm 'logand
		   (asm 'sra (V-simple-operand num) (K 1))
		   (K (fxsll -1 fx-shift)))))
	 (interrupt)))
      ((known expr)
       (cogen-value-quotient num expr))
      (else
       (interrupt)))))

 /section)


;;;; structs
;;
;;A data  structure is a variable  length block of memory  referenced by
;;machine  words  tagged as  vectors;  the  first  machine word  of  the
;;structure is a reference to the type descriptor (STD), which is itself
;;a data structure; the subsequent words,  if any, are the fields of the
;;structure.
;;
;;A block of memory  is a data structure if and only  if: a reference to
;;it is tagged as vector and its first word is tagged as vector.
;;
;; |----------------|----------| reference to structure
;;   heap offset     vector tag
;;
;; |----------------|----------| first word of structure
;;   heap offset     vector tag    = reference to STD
;;                                 = reference to structure
;;
;;The whole memory block layout of a struct with 5 fields is as follows:
;;
;; |-----|------|------|------|------|------|
;;   STD  field0 field1 field2 field3 field4
;;
;;fields are indexed starting at zero.
;;
;;The struct type descriptor (std) of the type descriptors is the return
;;value of BASE-RTD.
;;
(section

 (define-core-primitive-operation $struct? unsafe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag vector-mask vector-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $struct/rtd? unsafe
   ;;Evaluate to true if X is a structure and its type is STD.
   ;;
   ((P x std)
    (make-conditional (tag-test (V-simple-operand x) vector-mask vector-tag)
	(asm '=
	     (asm 'mref (V-simple-operand x) (K off-struct-std))
	     (V-simple-operand std))
      (K #f)))
   ((E x std)
    (nop)))

 (define-core-primitive-operation $make-struct unsafe
   ;;Allocate a  new data structure of  type STD capable of  holding LEN
   ;;words/fields.
   ;;
   ;;NOTE This operation initialises only the std field, leaving all the
   ;;other fields set to a machine word  with all the bits set to 1.  We
   ;;must be  careful not to  trigger a garbage collection  before those
   ;;fields are  initialised with  valid values,  else the  behaviour is
   ;;undefined.
   ;;
   ((V std len)
    (struct-case len
      ((constant len.val)
       ;;LEN.VAL must  be an  exact integer whose  payload bits  are the
       ;;binary representation of a fixnum.
       (unless (target-platform-fixnum? len.val)
	 (interrupt))
       (with-tmp ((stru (asm 'alloc
			     (K (align (+ (* len.val wordsize) disp-struct-data)))
			     (K vector-tag))))
	 ;;Store the STD in the first word.
	 (asm 'mset stru (K off-struct-std) (V-simple-operand std))
	 stru))
      ((known len.expr)
       (cogen-value-$make-struct std len.expr))
      (else
       ;;Here LEN is recordized code  which, when evaluated, must return
       ;;a fixnum representing the number of fields.
       (with-tmp ((size (align-code len disp-struct-data)))
	 (with-tmp ((stru (asm 'alloc size (K vector-tag))))
	   (asm 'mset stru (K off-struct-std) (V-simple-operand std))
	   stru)))))
   ((P std len)
    (K #t))
   ((E std len)
    (nop)))

 (define-core-primitive-operation $struct-rtd unsafe
   ((V stru)
    (asm 'mref (V-simple-operand stru) (K off-struct-std)))
   ((E stru)
    (nop))
   ((P stru)
    (K #t)))

;;; --------------------------------------------------------------------
;;; struct type descriptor accessor

 (define-core-primitive-operation $std-std unsafe
   ((V stru)
    (asm 'mref (V-simple-operand stru) (K off-std-std)))
   ((E stru)
    (nop))
   ((P stru)
    (K #t)))

 (define-core-primitive-operation $std-name unsafe
   ((V stru)
    (asm 'mref (V-simple-operand stru) (K off-std-name)))
   ((E stru)
    (nop))
   ((P stru)
    (K #t)))

 (define-core-primitive-operation $std-length unsafe
   ((V stru)
    (asm 'mref (V-simple-operand stru) (K off-std-length)))
   ((E stru)
    (nop))
   ((P stru)
    (K #t)))

 (define-core-primitive-operation $std-fields unsafe
   ((V stru)
    (asm 'mref (V-simple-operand stru) (K off-std-fields)))
   ((E stru)
    (nop))
   ((P stru)
    (K #t)))

 (define-core-primitive-operation $std-printer unsafe
   ((V stru)
    (asm 'mref (V-simple-operand stru) (K off-std-printer)))
   ((E stru)
    (nop)))

 (define-core-primitive-operation $std-symbol unsafe
   ((V stru)
    (asm 'mref (V-simple-operand stru) (K off-std-symbol)))
   ((E stru)
    (nop))
   ((P stru)
    ;;The UID is always set.
    (K #t)))

 (define-core-primitive-operation $std-destructor unsafe
   ((V stru)
    (asm 'mref (V-simple-operand stru) (K off-std-destructor)))
   ((E stru)
    (nop)))

;;; --------------------------------------------------------------------
;;; struct type descriptor mutators

 ;;NOTE Remember that when mutating a storage location we have to update
 ;;the dirty  vector; for this reason  we use the MEM-ASSIGN  which does
 ;;the right thing.  (Marco Maggi; Fri Feb 28, 2014)
 (let-syntax
     ((define-std-mutator (syntax-rules ()
			    ((_ ?who ?off)
			     (define-core-primitive-operation ?who unsafe
			       ((V stru v)
				(multiple-forms-sequence
				 (mem-assign v (V-simple-operand stru) ?off)
				 (KN VOID-OBJECT)))
			       ((E stru v)
				(mem-assign v (V-simple-operand stru) ?off))
			       ((P stru v)
				(multiple-forms-sequence
				 (mem-assign v (V-simple-operand stru) ?off)
				 (K #t)))
			       ))
			    )))
   (define-std-mutator $set-std-std!		off-std-std)
   (define-std-mutator $set-std-name!		off-std-name)
   (define-std-mutator $set-std-length!		off-std-length)
   (define-std-mutator $set-std-fields!		off-std-fields)
   (define-std-mutator $set-std-printer!	off-std-printer)
   (define-std-mutator $set-std-symbol!		off-std-symbol)
   (define-std-mutator $set-std-destructor!	off-std-destructor)
   #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $struct-ref unsafe
   ;;Return the  word in the  field at index  I.  Accessing a  struct is
   ;;like accessing a vector.
   ;;
   ((V stru idx)
    (cogen-value-$vector-ref stru idx))
   ((E stru idx)
    (nop)))

 (define-core-primitive-operation $struct-set! unsafe
   ;;Store a  new word in  the field at index  I.  Mutating a  struct is
   ;;like mutating a vector.
   ;;
   ((V stru idx v)
    (multiple-forms-sequence
     (cogen-effect-$vector-set! stru idx v)
     (KN VOID-OBJECT)))
   ((E stru idx v)
    (cogen-effect-$vector-set! stru idx v))
   ((P stru idx v)
    (multiple-forms-sequence
     (cogen-effect-$vector-set! stru idx v)
     (K #t))))

 (define-core-primitive-operation $struct unsafe
   ((V std . field*)
    (with-tmp ((stru (asm 'alloc
			  (K (align (+ disp-struct-data (* (length field*) wordsize))))
			  (K vector-tag))))
      ;;Store a reference to the STD in the first word.
      (asm 'mset stru (K off-struct-std) (V-simple-operand std))
      ;;Store the fields.
      (let recur ((field* field*)
		  (offset off-struct-data)) ;offset in bytes
	(if (pair? field*)
	    (make-seq (asm 'mset stru (K offset) (V-simple-operand (car field*)))
		      (recur (cdr field*) (+ offset wordsize)))
	  stru))))
   ((P std . field*)
    (K #t))
   ((E std . field*)
    (nop)))

 /section)


;;;; characters
;;
;;A standalone character is a  machine word whose least significant bits
;;are set  to the character tag.   When stored in a  string: the machine
;;word is trimmed to its least significant 32 bits.
;;
;;The most significant bits, interpreted as integer, represent a Unicode
;;code point in the range [0, #x10FFFF] but excluding the range [#xD800,
;;#xDFFF].
;;
(section

 (module (assert-chars)

   (define (assert-chars a a*)
     ;;Return  a  struct  instance  representing  recordized  code  that
     ;;validates A and A* as code returning characters.
     ;;
     ;;A must be a struct intance representing recordized code.  A* must
     ;;be a list of struct instances representing recordized code.
     ;;
     (let*-values (((ch*  others) (partition known-char?     (cons a a*)))
		   ((nch* others) (partition known-non-char? others)))
       ;;Here  OTHERS  is  a   list  of  struct  instances  representing
       ;;recordized code which  are neither known to  evaluate to chars,
       ;;nor known to evaluate to non-chars.
       (cond ((not (null? nch*))
	      ;;If there are some non-chars: it is an error.
	      (interrupt))
	     ((null? others)
	      ;;If everything is known to evaluate to chars: fine.
	      (nop))
	     (else
	      ;;If  there  is some  code  of  unknown return  value:  OR
	      ;;together  all the  resulting machine  words, then  check
	      ;;that the result is tagged as char.
	      (interrupt-unless
	       (tag-test (or* (V-simple-operand (car others))
			      (cdr others))
			 char-mask char-tag))))))

   (define (or* a a*)
     ;;Return  a  struct  instance  representing  recordized  code  that
     ;;applies bitwise  logic OR  to all the  results of  the arguments.
     ;;Examples:
     ;;
     ;;  (or* arg0 '())
     ;;  => arg0
     ;;
     ;;  (or* arg0 (list arg1 arg2 arg3))
     ;;  => (asm 'logor
     ;;          (asm 'logor
     ;;               (asm 'logor arg0 arg1)
     ;;               arg2)
     ;;          arg3)
     ;;
     (if (pair? a*)
	 (or* (asm 'logor a (V-simple-operand (car a*)))
	      (cdr a*))
       a))

   (define (known-char? x)
     (struct-case x
       ((constant x.val)
	(char? x.val))
       ((known x.expr x.type)
	(eq? (T:char? x.type) 'yes))
       (else
	#f)))

   (define (known-non-char? x)
     (struct-case x
       ((constant x.val)
	(not (char? x.val)))
       ((known x.expr x.type)
	(eq? (T:char? x.type) 'no))
       (else
	#f)))

   #| end of module: assert-chars |# )

 (define (char-fold-p op a a*)
   ;;This  is used  to  apply  predicates to  pairs  of  arguments in  a
   ;;multi-argument primitive call.
   ;;
   ;;A must be  a struct intance representing recordized  code.  A* must
   ;;be a list of struct instances representing recordized code.
   ;;
   ;;The returned recordized code is as follows:
   ;;
   ;;   (char-fold-p op arg0 (list arg1 arg2 arg3))
   ;;   => (make-conditional (asm op arg0 arg1)
   ;;          (make-conditional (asm op arg1 arg2)
   ;;              (make-conditional (asm arg2 arg3)
   ;;                  (K #t)
   ;;                (K #f))
   ;;            (K #f))
   ;;        (K #f))
   ;;
   (multiple-forms-sequence
    (assert-chars a a*)
    (let recur ((a  a)
		(a* a*))
      (if (pair? a*)
	  (let ((b (car a*)))
	    (make-conditional (asm op (V-simple-operand a) (V-simple-operand b))
		(recur b (cdr a*))
	      (K #f)))
	(K #t)))))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation char? safe
   ((P x)
    (tag-test (V-simple-operand x) char-mask char-tag))
   ((E x)
    (nop)))

;;; --------------------------------------------------------------------
;;; fixnum to and from character

 (define-core-primitive-operation $fixnum->char unsafe
   ;;Given  a machine  word representing  a fixnum,  whose payload  bits
   ;;represent a Unicode code point,  return a machine word representing
   ;;the corresponding standalone character.
   ;;
   ;;We know that the character tag  (#x0F) is wider than the fixnum tag
   ;;(#b00);  on a  32-bit platform,  the fixnum  representing the  code
   ;;point of #\A:
   ;;
   ;;   00000000 00000000 00000001 00000100
   ;;                                    ** fixnum tag
   ;;
   ;;can become the character #\A by left-shifting by 6 positions
   ;;
   ;;   00000000 00000000 01000001 00000000
   ;;
   ;;and OR-ing the char tag:
   ;;
   ;;   00000000 00000000 01000001 00001111
   ;;                              ******** char tag
   ;;
   ((V x)
    (asm 'logor
	 (asm 'sll (V-simple-operand x) (K (fx- char-shift fx-shift)))
	 (K char-tag)))
   ((P x)
    (K #t))
   ((E x)
    (nop)))

 (define-core-primitive-operation $char->fixnum unsafe
   ;;Given a machine word representing  a standalone character: return a
   ;;machine word  representing a  fixnum, whose payload  bits represent
   ;;the corresponding Unicode code point.
   ;;
   ;;We know that the character tag  (#x0F) is wider than the fixnum tag
   ;;(#b00), and also the most significant bits of the character tag are
   ;;zero like  the bits of  the fixnum tag;  on a 32-bit  platform, the
   ;;machine word representing the character #\A:
   ;;
   ;;   00000000 00000000 01000001 00001111
   ;;                              ******** char tag
   ;;
   ;;can become  the fixnum representing the  code point of #\A  by just
   ;;right-shifting by 6:
   ;;
   ;;   00000000 00000000 00000001 00000100
   ;;                                    ** fixnum tag
   ;;
   ;;FIXME  We  do  an  arithmetic  shift-right,  rather  than  a  logic
   ;;shift-right, is there  a reason for this?  Anyway,  notice that the
   ;;biggest code  point #x10FFFF  has the most  significant bit  set to
   ;;zero both when represented as fixnum and character.
   ;;
   ((V x)
    (asm 'sra (V-simple-operand x) (K (fx- char-shift fx-shift))))
   ((P x)
    (K #t))
   ((E x)
    (nop)))

;;; --------------------------------------------------------------------
;;; UNsafe character comparisons

 (let-syntax
     ((define-$char-comparison (syntax-rules ()
				 ((_ ?who ?prim)
				  (define-core-primitive-operation ?who unsafe
				    ((P x y)
				     (asm (quote ?prim) (V-simple-operand x) (V-simple-operand y)))
				    ((E x y)
				     (nop)))
				  ))))
   (define-$char-comparison $char=	=)
   (define-$char-comparison $char<	<)
   (define-$char-comparison $char<=	<=)
   (define-$char-comparison $char>	>)
   (define-$char-comparison $char>=	>=))

;;; --------------------------------------------------------------------
;;; safe character comparisons

 (define-core-primitive-operation char=? safe
   ((P)
    (interrupt))
   ((P a . a*)
    (char-fold-p '= a a*))
   ((E)
    (interrupt))
   ((E a . a*)
    (assert-chars a a*)))

 (define-core-primitive-operation char<? safe
   ((P)
    (interrupt))
   ((P a . a*)
    (char-fold-p '< a a*))
   ((E)
    (interrupt))
   ((E a . a*)
    (assert-chars a a*)))

 (define-core-primitive-operation char<=? safe
   ((P)
    (interrupt))
   ((P a . a*)
    (char-fold-p '<= a a*))
   ((E)
    (interrupt))
   ((E a . a*)
    (assert-chars a a*)))

 (define-core-primitive-operation char>? safe
   ((P)
    (interrupt))
   ((P a . a*)
    (char-fold-p '> a a*))
   ((E)
    (interrupt))
   ((E a . a*)
    (assert-chars a a*)))

 (define-core-primitive-operation char>=? safe
   ((P)
    (interrupt))
   ((P a . a*)
    (char-fold-p '>= a a*))
   ((E)
    (interrupt))
   ((E a . a*)
    (assert-chars a a*)))

 /section)


;;;; bytevectors
;;
;;Bytevectors are blocks of memory referenced by machine words tagged as
;;bytevectors.   The  first  word  in  the  memory  block  is  a  fixnum
;;representing the  number of  bytes in the  data area; a  bytevector is
;;capable of  holding at  most a  number of values  equal to  the return
;;value of GREATEST-FIXNUM.
;;
;;When allocating  a bytevector capable  of holding N bytes,  the actual
;;size of the  allocated data area is at least  N+1; the additional last
;;byte is not part of the data area and is perpetually set to zero.
;;
;;To  allow  for  the same  binary  layout  on  both 32-bit  and  64-bit
;;platforms,  the data area  starts 8  bytes after  the beginning;  on a
;;32-bit platform the layout is:
;;
;;    1st word   2nd word                       last byte
;;  |----------|----------|-------------------|-----------|
;;     length     unused        data area      set to zero
;;
;;on a 64-bit platform the layout is:
;;
;;         1st word                             last byte
;;  |---------------------|-------------------|-----------|
;;         length               data area      set to zero
;;
(section

 (define-core-primitive-operation bytevector? safe
   ((P x)
    (tag-test (V-simple-operand x) bytevector-mask bytevector-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $make-bytevector unsafe
   ((V num-of-bytes)
    (struct-case num-of-bytes
      ((constant num-of-bytes.val)
       ;;NUM-OF-BYTES.VAL is an exact integer whose payload bits are the
       ;;binary representation of a fixnum.
       (unless (target-platform-fixnum? num-of-bytes.val)
	 (interrupt))
       (with-tmp ((bv (asm 'alloc
			   (K (align (+ num-of-bytes.val 1 disp-bytevector-data)))
			   (K bytevector-tag))))
	 ;;Store the length in the first word.
	 (asm 'mset bv
	      (K off-bytevector-length)
	      ;;Tag as fixnum.
	      (K (* num-of-bytes.val fx-scale)))
	 ;;Set to zero the one-off byte.
	 (asm 'bset bv
	      (K (+ num-of-bytes.val off-bytevector-data))
	      (K 0))
	 bv))
      ((known num-of-bytes.expr)
       (cogen-value-$make-bytevector num-of-bytes.expr))
      (else
       ;;Here NUM-OF-BYTES is a  struct instance representing recordized
       ;;code which, when evaluated, must return a fixnum.
       (with-tmp ((bv (asm 'alloc
			   (align-code (prm-UNtag-as-fixnum (V-simple-operand num-of-bytes))
				       (+ 1 disp-bytevector-data))
			   (K bytevector-tag))))
	 ;;Store the length in the first word.
	 (asm 'mset bv (K off-bytevector-length) (V-simple-operand num-of-bytes))
	 ;;Set to zero the one-off byte.
	 (asm 'bset bv
	      (asm 'int+
		   (prm-UNtag-as-fixnum (V-simple-operand num-of-bytes))
		   (K off-bytevector-data))
	      (K 0))
	 bv))))
   ((P num-of-bytes)
    (K #t))
   ((E num-of-bytes)
    (nop)))

 (define-core-primitive-operation $bytevector-length unsafe
   ((V bv)
    (asm 'mref (V-simple-operand bv) (K off-bytevector-length)))
   ((P bv)
    (K #t))
   ((E bv)
    (nop)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $bytevector-u8-ref unsafe
   ((V bv idx)
    (struct-case idx
      ((constant idx.val)
       ;;IDX.VAL is an  exact integer whose payload bits  are the binary
       ;;representation of a fixnum.
       (unless (target-platform-fixnum? idx.val)
	 (interrupt))
       (prm-tag-as-fixnum
	(prm-isolate-least-significant-byte
	 (asm 'bref (V-simple-operand bv) (K (+ idx.val off-bytevector-data))))))
      (else
       ;;Here  IDX is  a  struct instance  representing recordized  code
       ;;which, when evaluated, must return a fixnum.
       (prm-tag-as-fixnum
	(prm-isolate-least-significant-byte
	 (asm 'bref (V-simple-operand bv) (asm 'int+
				(prm-UNtag-as-fixnum (V-simple-operand idx))
				(K off-bytevector-data))))))))
   ((P bv idx)
    (K #t))
   ((E bv idx)
    (nop)))

 (define-core-primitive-operation $bytevector-s8-ref unsafe
   ((V bv idx)
    (let-syntax
	((%extend-sign (syntax-rules ()
			 ((_ ?machine-word)
			  (asm 'sra ;this extends the sign
			       (asm 'sll ?machine-word
				    (K (fx- NUM-OF-BITS-IN-WORD 8)))
			       (K (fx- NUM-OF-BITS-IN-WORD (fx+ 8 fx-shift))))))))
      (struct-case idx
	((constant idx.val)
	 ;;IDX.VAL is an exact integer whose payload bits are the binary
	 ;;representation of a fixnum.
	 (unless (target-platform-fixnum? idx.val)
	   (interrupt))
	 ;;Retrieve the  requested byte than left-shift  and right-shift
	 ;;so that  the most  significant bit  is extended  to correctly
	 ;;represent the sign in the returned fixnum.
	 (%extend-sign
	  (asm 'bref (V-simple-operand bv) (K (+ idx.val off-bytevector-data))))
	 ;;
	 ;;The one below  is the original code from  Ikarus; in addition
	 ;;to the code  above, it contains a byte  isolation that, IMHO,
	 ;;it is useless here.  (Marco Maggi; Oct 23, 2012)
	 ;;
         ;; (%extend-sign
         ;;  (prm-isolate-least-significant-byte
         ;;   (asm 'bref (V-simple-operand bv) (K (+ idx.val off-bytevector-data)))))
	 ;;
	 )
	(else
	 ;;Here IDX  is a  struct instance representing  recordized code
	 ;;which, when evaluated, must return a fixnum.
	 ;;
	 ;;Retrieve the  requested byte than left-shift  and right-shift
	 ;;so that  the most  significant bit  is extended  to correctly
	 ;;represent the sign in the returned fixnum.
	 (%extend-sign
	  (asm 'bref (V-simple-operand bv) (asm 'int+
				 (prm-UNtag-as-fixnum (V-simple-operand idx))
				 (K off-bytevector-data))))
	 ))))
   ((P bv idx)
    (K #t))
   ((E bv idx)
    (nop)))

 (define-core-primitive-operation $bytevector-set! unsafe
   ((E bv idx byte)
    (define (%check-byte byte.val)
      (cond ((<= -128 byte.val 127)
	     (K byte.val))
	    ((<= +128 byte.val 255)
	     (K (fx- byte.val 256)))
	    (else
	     (interrupt))))
    (struct-case idx
      ((constant idx.val)
       (unless (target-platform-fixnum? idx.val)
	 (interrupt))
       ;;IDX.VAL is an  exact integer whose payload bits  are the binary
       ;;representation of a fixnum.
       (let ((byte-offset (+ idx.val off-bytevector-data)))
	 (struct-case byte
	   ((constant byte.val)
	    (unless (target-platform-fixnum? byte.val)
	      (interrupt))
	    ;;BYTE.VAL is  an exact integer  whose payload bits  are the
	    ;;binary representation of a fixnum.
	    (asm 'bset (V-simple-operand bv) (K byte-offset) (%check-byte byte.val)))
	   (else
	    ;;BYTE  is a  struct instance  representing recordized  code
	    ;;which, when evaluate, must return a fixnum.
	    (asm 'bset (V-simple-operand bv) (K byte-offset) (prm-UNtag-as-fixnum (V-simple-operand byte)))))))
      (else
       ;;IDX is  a struct  instance representing recordized  code which,
       ;;when evaluate, must return a fixnum.
       (define byte-offset
	 (asm 'int+ (prm-UNtag-as-fixnum (V-simple-operand idx)) (K off-bytevector-data)))
       (struct-case byte
	 ((constant byte.val)
	  (unless (target-platform-fixnum? byte.val)
	    (interrupt))
	  ;;BYTE.VAL is  an exact integer  whose payload bits  are the
	  ;;binary representation of a fixnum.
	  (asm 'bset (V-simple-operand bv) byte-offset (%check-byte byte.val)))
	 (else
	  ;;BYTE  is a  struct instance  representing recordized  code
	  ;;which, when evaluate, must return a fixnum.
	  (asm 'bset (V-simple-operand bv) byte-offset (prm-UNtag-as-fixnum (V-simple-operand byte)))))))))

;;; --------------------------------------------------------------------
;;; double flonum ref

 (define-core-primitive-operation $bytevector-ieee-double-native-ref unsafe
   ((V bv idx)
    (with-tmp ((flo (asm 'alloc
			 (K (align flonum-size))
			 (K vector-tag))))
      ;;Tag the first word as flonum.
      (asm 'mset flo (K off-flonum-tag) (K flonum-tag))
      ;;Load the number in a floating point register.
      (asm 'fl:load
	   (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand idx)))
	   (K off-bytevector-data))
      ;;Store the number in the data area of the flonum.
      (asm 'fl:store flo (K off-flonum-data))
      flo)))

 (define-core-primitive-operation $bytevector-ieee-double-nonnative-ref unsafe
   ((V bv i)
    (boot.case-word-size
     ((32)
      (with-tmp ((flo (asm 'alloc
			   (K (align flonum-size))
			   (K vector-tag))))
	;;Tag the first word as flonum.
	(asm 'mset flo (K off-flonum-tag) (K flonum-tag))
	(with-tmp ((t (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand i)))))
	  (with-tmp ((x0 (asm 'mref t (K off-bytevector-data))))
	    (asm 'bswap! x0 x0)
	    (asm 'mset flo (K (+ off-flonum-data wordsize)) x0))
	  (with-tmp ((x0 (asm 'mref t (K (+ off-bytevector-data wordsize)))))
	    (asm 'bswap! x0 x0)
	    (asm 'mset flo (K off-flonum-data) x0)))
	flo))
     ((64)
      (with-tmp ((flo (asm 'alloc
			   (K (align flonum-size))
			   (K vector-tag))))
	;;Tag the first word as flonum.
	(asm 'mset flo (K off-flonum-tag) (K flonum-tag))
	(with-tmp ((t  (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand i)))))
	  (with-tmp ((x0 (asm 'mref t (K off-bytevector-data))))
	    (asm 'bswap! x0 x0)
	    (asm 'mset flo (K off-flonum-data) x0)))
	flo)))))

;;;The  following   uses  unsupported  SSE3   instructions.   (Abdulaziz
;;;Ghuloum)
;;;
;;;(define-core-primitive-operation $bytevector-ieee-double-nonnative-ref unsafe
;;;  ((V bv i)
;;;   (with-tmp ((x (asm 'alloc (K (align flonum-size)) (K vector-tag))))
;;;     (asm 'mset x (K off-flonum-tag) (K flonum-tag))
;;;     (asm 'fl:load
;;;       (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand i)))
;;;       (K off-bytevector-data))
;;;     (asm 'fl:shuffle
;;;       (K (make-object '#vu8(7 6 2 3 4 5 1 0)))
;;;       (K off-bytevector-data))
;;;     (asm 'fl:store x (K off-flonum-data))
;;;     x)))

;;; --------------------------------------------------------------------
;;; double flonum set

 (define-core-primitive-operation $bytevector-ieee-double-native-set! unsafe
   ((E bv idx flo)
    (multiple-forms-sequence
     ;;Load the double from the data  area of the flonum into a floating
     ;;point register.
     (asm 'fl:load (V-simple-operand flo) (K off-flonum-data))
     ;;Store the  double from  the register  into the  data area  of the
     ;;bytevector.
     (asm 'fl:store
	  (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand idx)))
	  (K off-bytevector-data)))))

 (define-core-primitive-operation $bytevector-ieee-double-nonnative-set! unsafe
   ((E bv idx flo)
    (boot.case-word-size
     ((32)
      (with-tmp ((t (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand idx)))))
	(with-tmp ((x0 (asm 'mref (V-simple-operand flo) (K off-flonum-data))))
	  (asm 'bswap! x0 x0)
	  (asm 'mset t (K (+ off-bytevector-data wordsize)) x0))
	(with-tmp ((x0 (asm 'mref (V-simple-operand flo) (K (+ off-flonum-data wordsize)))))
	  (asm 'bswap! x0 x0)
	  (asm 'mset t (K off-bytevector-data) x0))))
     ((64)
      (with-tmp ((t (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand idx)))))
	(with-tmp ((x0 (asm 'mref (V-simple-operand flo) (K off-flonum-data))))
	  (asm 'bswap! x0 x0)
	  (asm 'mset t (K off-bytevector-data) x0)))))))

;;;The following uses unsupported SSE3 instructions.  (Abdulaziz Ghuloum)
;;;
;;;(define-core-primitive-operation $bytevector-ieee-double-nonnative-set! unsafe
;;;  ((E bv i x)
;;;   (multiple-forms-sequence
;;;     (asm 'fl:load (V-simple-operand x) (K off-flonum-data))
;;;     (asm 'fl:shuffle
;;;       (K (make-object '#vu8(7 6 2 3 4 5 1 0)))
;;;       (K off-bytevector-data))
;;;     (asm 'fl:store
;;;       (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand i)))
;;;       (K off-bytevector-data)))))

;;; --------------------------------------------------------------------
;;; single flonum ref

 (define-core-primitive-operation $bytevector-ieee-single-native-ref unsafe
   ((V bv idx)
    (with-tmp ((flo (asm 'alloc
			 (K (align flonum-size))
			 (K vector-tag))))
      ;;Tag the first word of the flonum memory block.
      (asm 'mset flo (K off-flonum-tag) (K flonum-tag))
      ;;Load  the  single from  the  bytevector  into a  floating  point
      ;;register.
      (asm 'fl:load-single
	   (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand idx)))
	   (K off-bytevector-data))
      ;;Convert the single into a double.
      (asm 'fl:single->double)
      ;;Store the double into the data area of the flonum.
      (asm 'fl:store flo (K off-flonum-data))
      flo)))

 (define-core-primitive-operation $bytevector-ieee-single-nonnative-ref unsafe
   ((V bv idx)
    (with-tmp ((flo (asm 'alloc
			 (K (align flonum-size))
			 (K vector-tag))))
      ;;Tag the first word of the flonum memory block.
      (asm 'mset flo (K off-flonum-tag) (K flonum-tag))
      ;;Copy the single  from the bytevector data area  into a register;
      ;;reverse  its bytes;  copy the  reversed single  into the  flonum
      ;;data.
      (with-tmp ((t (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand idx)))))
	(with-tmp ((x0 (asm 'mref t (K off-bytevector-data))))
	  (asm 'bswap! x0 x0)
	  (asm 'mset flo (K off-flonum-data) x0)))
      ;;Load the reversed single into a floating point register.
      (asm 'fl:load-single flo (K (+ off-flonum-data (- wordsize 4))))
      ;;Convert the single into a double.
      (asm 'fl:single->double)
      ;;Store the double into the data area of the flonum.
      (asm 'fl:store flo (K off-flonum-data))
      flo)))

;;; --------------------------------------------------------------------
;;; single flonum set

 (define-core-primitive-operation $bytevector-ieee-single-native-set! unsafe
   ((E bv idx flo)
    (multiple-forms-sequence
     ;;Load the single into a floating point register.
     (asm 'fl:load (V-simple-operand flo) (K off-flonum-data))
     ;;Convert the double into a single.
     (asm 'fl:double->single)
     ;;Store the double into the bytevector.
     (asm 'fl:store-single
	  (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand idx)))
	  (K off-bytevector-data)))))

 (define-core-primitive-operation $bytevector-ieee-single-nonnative-set! unsafe
   ((E bv i flo)
    (multiple-forms-sequence
     ;;Load the single into a floating point register.
     (asm 'fl:load (V-simple-operand flo) (K off-flonum-data))
     ;;Convert the double into a single.
     (asm 'fl:double->single)
     (with-tmp ((t (asm 'int+ (V-simple-operand bv) (prm-UNtag-as-fixnum (V-simple-operand i)))))
       ;;Store the single into the bytevector data area.
       (asm 'fl:store-single t (K off-bytevector-data))
       (boot.case-word-size
	((32)
	 ;;Load the single into a register.
	 (with-tmp ((x0 (asm 'mref t (K off-bytevector-data))))
	   ;;Reverse the bytes.
	   (asm 'bswap! x0 x0)
	   ;;Store the reversed single in the bytevector.
	   (asm 'mset   t (K off-bytevector-data) x0)))
	((64)
	 ;;Load the single into a register.
	 (with-tmp ((x0 (asm 'mref32 t (K off-bytevector-data))))
	   ;;Reverse the bytes.
	   (asm 'bswap! x0 x0)
	   ;;Store the reversed single in the bytevector.
	   (asm 'mset32 t (K off-bytevector-data) (asm 'sra x0 (K 32))))))))))

 /section)


;;;; strings
;;
;;Strings are  blocks of  memory referenced by  machine words  tagged as
;;strings; the first  word in the memory block  is a fixnum representing
;;the number  of characters  in the  data area; a  string is  capable of
;;holding at  most a number of  characters equal to the  return value of
;;GREATEST-FIXNUM.
;;
;;  |------------------------|-------------| reference to string
;;        heap offset          string tag
;;
;;  |------------------------|-------------| string first word
;;       number of words       fixnum tag
;;
;;All the  remaining space  in the  memory block  is filled  with 32-bit
;;unsigned  integers  whose  least  significant  bits  are  set  to  the
;;character  tag  and  whose  most  significant  bits  are  set  to  the
;;character's Unicode code point:
;;
;;   tag ch0 ch1 ch2 ch3 ch4 ch5 ch6 ch7
;;  |---|---|---|---|---|---|---|---|---| string memory block
;;
;;Character indexes are zero-based.
;;
(section

 (define-core-primitive-operation string? safe
   ((P x)
    (tag-test (V-simple-operand x) string-mask string-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $make-string unsafe
   ((V num-of-chars)
    (struct-case num-of-chars
      ((constant num-of-chars.val)
       (unless (target-platform-fixnum? num-of-chars.val)
	 (interrupt))
       ;;NUM-OF-CHARS.VAL is an exact integer whose payload bits are the
       ;;binary representation of a fixnum.
       (with-tmp ((str (asm 'alloc
			    (K (align (+ (* num-of-chars.val char-size) disp-string-data)))
			    (K string-tag))))
	 ;;Store the string length in the first word.
	 (asm 'mset str (K off-string-length) (K (* num-of-chars.val fx-scale)))
	 str))
      ((known num-of-chars.expr)
       (cogen-value-$make-string num-of-chars.expr))
      (else
       ;;NUM-OF-CHARS is a struct  instance representing recordized code
       ;;which, when evaluated, must return a fixnum.
       (boot.case-word-size
	((32)
	 (with-tmp ((str (asm 'alloc
			      (align-code (V-simple-operand num-of-chars) disp-string-data)
			      (K string-tag))))
	   ;;Store the string length in the first word.
	   (asm 'mset str (K off-string-length) (V-simple-operand num-of-chars))
	   str))
	((64)
	 ;;FIXME  CHECK In  the  original Ikarus  sources  there was  no
	 ;;difference between  32-bit and 64-bit code:  both cases where
	 ;;like the  above branch for  32-bit.  But on 64-bit  such code
	 ;;causes the data  area of the string to be  (* 8 NUM-OF-CHARS)
	 ;;bytes wide,  not (* 4  NUM-OF-CHARS) bytes wide as  it should
	 ;;be.  So I have introduced the right-shift.  (Marco Maggi; Oct
	 ;;24, 2012)
	 (with-tmp ((str (asm 'alloc
			      (align-code (asm 'sra (V-simple-operand num-of-chars) (K 1))
					  disp-string-data)
			      (K string-tag))))
	   ;;Store the string length in the first word.
	   (asm 'mset str (K off-string-length) (V-simple-operand num-of-chars))
	   str))))))
   ((P n)
    (K #t))
   ((E n)
    (nop)))

 (define-core-primitive-operation $string-length unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-string-length)))
   ((P x)
    (K #t))
   ((E x)
    (nop)))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $string-ref unsafe
   ((V str idx)
    (struct-case idx
      ((constant idx.val)
       (interrupt-unless-fx idx.val)
       ;;IDX.VAL is an  exact integer whose payload bits  are the binary
       ;;representation of a fixnum.
       (asm 'mref32 (V-simple-operand str) (K (+ (* idx.val char-size) off-string-data))))
      (else
       ;;IDX is  a struct  instance representing recordized  code which,
       ;;when evaluated, must return a fixnum.
       (asm 'mref32 (V-simple-operand str) (asm 'int+ (boot.case-word-size
					;;IDX is a fixnum representing a
					;;character  index  and its  raw
					;;value  is also  the offset  in
					;;bytes.
					((32)	(V-simple-operand idx))
					;;IDX is a fixnum representing a
					;;character  index   and,  after
					;;shifting  one   bit,  its  raw
					;;value  is also  the offset  in
					;;bytes.
					((64)	(asm 'sra (V-simple-operand idx) (K 1))))
				 (K off-string-data))))))
   ((P str idx)
    (K #t))
   ((E str idx)
    (nop)))

 (define-core-primitive-operation $string-set! unsafe
   ((E str idx ch)
    (struct-case idx
      ((constant idx.val)
       (interrupt-unless-fx idx.val)
       ;;IDX.VAL is an  exact integer whose payload bits  are the binary
       ;;representation of a fixnum.
       (asm 'mset32 (V-simple-operand str) (K (+ (* idx.val char-size) off-string-data))
	    (V-simple-operand ch)))
      (else
       ;;IDX is  a struct  instance representing recordized  code which,
       ;;when evaluated, must return a fixnum.
       (asm 'mset32 (V-simple-operand str) (asm 'int+ (boot.case-word-size
					;;IDX is a fixnum representing a
					;;character  index  and its  raw
					;;value  is also  the offset  in
					;;bytes.
					((32)	(V-simple-operand idx))
					;;IDX is a fixnum representing a
					;;character  index   and,  after
					;;shifting  one   bit,  its  raw
					;;value  is also  the offset  in
					;;bytes.
					((64)	(asm 'sra (V-simple-operand idx) (K 1))))
				 (K off-string-data))
	    (V-simple-operand ch))))))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation string-length safe
   ((V str)
    (struct-case str
      ((known str.expr str.type)
       (case (T:string? str.type)
	 ((yes)
	  (cogen-value-$string-length str.expr))
	 ((no)
	  (if (option.strict-r6rs)
	      (interrupt)
	    (compile-time-operand-core-type-error __who__
	      "expected string as operand" (unparse-recordized-code str))))
	 (else
	  (cogen-value-string-length str.expr))))
      ((constant str.const)
       ;;Here the operand has no type description, but still we can check the operand
       ;;at compile-time.
       (cond ((string? str.const)
	      (cogen-value-$string-length str))
	     ((option.strict-r6rs)
	      (interrupt))
	     (else
	      (compile-time-operand-core-type-error __who__
		"expected string as operand" (unparse-recordized-code str)))))
      (else
       (multiple-forms-sequence
	(assert-string str)
	(with-tmp ((str.len (cogen-value-$string-length str)))
	  (interrupt-unless-fixnum str.len)
	  str.len)))))
   ((E str)
    (struct-case str
      ((known str.expr str.type)
       (case (T:string? str.type)
	 ((yes)
	  (nop))
	 ((no)
	  (if (option.strict-r6rs)
	      (interrupt)
	    (compile-time-operand-core-type-error __who__
	      "expected string as operand" (unparse-recordized-code str))))
	 (else
	  (cogen-effect-string-length str.expr))))
      ((constant str.const)
       ;;Here the operand has no type description, but still we can check the operand
       ;;at compile-time.
       (cond ((string? str.const)
	      (nop))
	     ((option.strict-r6rs)
	      (interrupt))
	     (else
	      (compile-time-operand-core-type-error __who__
		"expected string as operand" (unparse-recordized-code str)))))
      (else
       (assert-string str))))
   ((P str)
    (multiple-forms-sequence
     (cogen-effect-string-length str)
     (K #t))))

 (define-core-primitive-operation string-ref safe
   ((V str idx)
    (struct-case idx
      ((constant idx.val)
       (unless (and (target-platform-fixnum? idx.val)
		    (<= 0 idx.val))
	 (error 'cogen/string-ref
	   "expected non-negative fixnum as constant character index" idx.val))
       ;;IDX.VAL is an  exact integer whose payload bits  are the binary
       ;;representation of a fixnum.
       (multiple-forms-sequence
	(assert-string str)
	(interrupt-unless
	 (asm 'u< (V-simple-operand idx) (cogen-value-$string-length str)))
	(cogen-value-$string-ref str idx)))
      ((known idx.expr idx.type)
       (case (T:fixnum? idx.type)
	 ((yes)
	  (multiple-forms-sequence
	   (assert-string str)
	   (interrupt-unless
	    (asm 'u< (V-simple-operand idx.expr) (cogen-value-$string-length str)))
	   (cogen-value-$string-ref str idx)))
	 ((no)
	  (interrupt))
	 (else
	  (cogen-value-string-ref str idx.expr))))
      (else
       ;;IDX is  a struct  instance representing recordized  code which,
       ;;when evaluated, must return a fixnum.
       (multiple-forms-sequence
	(assert-fixnum idx)
	(assert-string str)
	(interrupt-unless
	 (asm 'u< (V-simple-operand idx) (cogen-value-$string-length str)))
	(cogen-value-$string-ref str idx)))))
   ((P str idx)
    (multiple-forms-sequence
     (assert-fixnum idx)
     (assert-string str)
     (interrupt-unless
      (asm 'u< (V-simple-operand idx) (cogen-value-$string-length str)))
     (K #t)))
   ((E str idx)
    (multiple-forms-sequence
     (assert-fixnum idx)
     (assert-string str)
     (interrupt-unless
      (asm 'u< (V-simple-operand idx) (cogen-value-$string-length str))))))

 /section)


;;;; ports
;;
;;A port  object is a  fixed--length memory block referenced  by machine
;;words tagged as  vectors; the first word of the  vector is the bitwise
;;OR between a port tag and a bitvector of port attributes:
;;
;;  |----------------|----------| port value
;;    heap offset     vector tag
;;
;;  |----------------|----------| 1st word of port's memory block
;;   port attributes   port tag
;;
;;At present the port tag is 6 bits wide, so we have:
;;
;;- on 32 bits platforms: 32 - 6 = 24
;;- on 64 bits platforms: 64 - 6 = 58
;;
;;bits available for attributes.
;;
(section

 (define-core-primitive-operation port? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag port-mask port-tag))
   ((E x)
    (nop)))

 ;;FIXME These  primitive operations should  work, IMO, but they  do not
 ;;and I do not understand why.  (Marco Maggi; Oct 24, 2012)
 ;;
 ;; (let-syntax
 ;;     ((define-predicate-operation
 ;;        (syntax-rules ()
 ;;          ((_ ?who ?tag)
 ;;           (define-core-primitive-operation ?who safe
 ;;             ((P x)
 ;;              (define bits
 ;;                (bitwise-ior port-tag
 ;;                             (bitwise-arithmetic-shift-left ?tag port-attrs-shift)))
 ;;              (sec-tag-test (V-simple-operand x) vector-mask vector-tag bits bits))
 ;;             ((E x)
 ;;              (nop)))
 ;;           ))))
 ;;   (define-predicate-operation input-port?      INPUT-PORT-TAG)
 ;;   (define-predicate-operation output-port?     OUTPUT-PORT-TAG)
 ;;   (define-predicate-operation textual-port?    TEXTUAL-PORT-TAG)
 ;;   (define-predicate-operation binary-port?     BINARY-PORT-TAG))

;;; --------------------------------------------------------------------

 (define-core-primitive-operation $make-port unsafe
   ((V attrs idx sz buf tr id read write getp setp cl cookie)
    (with-tmp ((p (asm 'alloc
		       (K (align port-size))
		       (K vector-tag))))
      ;;Store  in  the  first  word  a machine  word  holding  the  port
      ;;attributes and the port tag.
      (asm 'mset p (K off-port-attrs)
	   (asm 'logor
		(asm 'sll (V-simple-operand attrs) (K port-attrs-shift))
		(K port-tag)))
      (asm 'mset p (K off-port-index)		(V-simple-operand idx))
      (asm 'mset p (K off-port-size)		(V-simple-operand sz))
      (asm 'mset p (K off-port-buffer)		(V-simple-operand buf))
      (asm 'mset p (K off-port-transcoder)	(V-simple-operand tr))
      (asm 'mset p (K off-port-id)		(V-simple-operand id))
      (asm 'mset p (K off-port-read!)		(V-simple-operand read))
      (asm 'mset p (K off-port-write!)		(V-simple-operand write))
      (asm 'mset p (K off-port-get-position)	(V-simple-operand getp))
      (asm 'mset p (K off-port-set-position!)	(V-simple-operand setp))
      (asm 'mset p (K off-port-close)		(V-simple-operand cl))
      (asm 'mset p (K off-port-cookie)		(V-simple-operand cookie))
      (asm 'mset p (K off-port-unused1)		(K 0))
      (asm 'mset p (K off-port-unused2)		(K 0))
      p)))

 (let-syntax
     ((define-port-accessor (syntax-rules ()
			      ((_ ?who ?offset)
			       (define-core-primitive-operation ?who unsafe
				 ((V port)
				  (asm 'mref (V-simple-operand port) (K ?offset))))
			       ))))
   (define-port-accessor $port-index		off-port-index)
   (define-port-accessor $port-size		off-port-size)
   (define-port-accessor $port-buffer		off-port-buffer)
   (define-port-accessor $port-transcoder	off-port-transcoder)
   (define-port-accessor $port-id		off-port-id)
   (define-port-accessor $port-read!		off-port-read!)
   (define-port-accessor $port-write!		off-port-write!)
   (define-port-accessor $port-get-position	off-port-get-position)
   (define-port-accessor $port-set-position!	off-port-set-position!)
   (define-port-accessor $port-close		off-port-close)
   (define-port-accessor $port-cookie		off-port-cookie))

 (define-core-primitive-operation $port-attrs unsafe
   ;;Given  a  port value  X:  return  a  fixnum representing  the  port
   ;;attributes.   To  be  used  when  the  argument  has  already  been
   ;;validated as port value.
   ((V port)
    (asm 'sra
	 (asm 'mref (V-simple-operand port) (K off-port-attrs))
	 (K port-attrs-shift))))

 (define-core-primitive-operation $port-tag unsafe
   ;;Extract  from  a port  reference  a  fixnum  representing the  port
   ;;attributes.  If  the argument  is not a  port reference  the return
   ;;value is zero.
   ((V port)
    (make-conditional (tag-test (V-simple-operand port) vector-mask vector-tag)
	(with-tmp ((first-word (asm 'mref (V-simple-operand port) (K off-port-attrs))))
	  (make-conditional (tag-test first-word port-mask port-tag)
	      (asm 'sra first-word (K port-attrs-shift))
	    (K 0)))
      (K 0))))

 (let-syntax
     ((define-port-mutator (syntax-rules ()
			     ((_ ?who ?offset)
			      (define-core-primitive-operation ?who unsafe
				((E port val)
				 ;;We do  not need  to update  the dirty
				 ;;vector because VAL is always a fixnum
				 ;;here.
				 (asm 'mset (V-simple-operand port) (K ?offset) (V-simple-operand val))))
			      ))))
   (define-port-mutator $set-port-index!	off-port-index)
   (define-port-mutator $set-port-size!		off-port-size))

 (define-core-primitive-operation $set-port-attrs! unsafe
   ;;Store  in the  first word  of  a port  memory  block a  new set  of
   ;;attributes.
   ;;
   ;;ATTRS must be a struct instance representing recordized code which,
   ;;when  evaluated, must  return a  fixnum representing  the attribute
   ;;bits.  We  do not need to  update the dirty vector  because the new
   ;;value is always a fixnum.
   ;;
   ((E port attrs)
    (asm 'mset (V-simple-operand port) (K off-port-attrs)
	 (asm 'logor
	      (asm 'sll (V-simple-operand attrs) (K port-attrs-shift))
	      (K port-tag)))))

 /section)


;;;; port transcoders
;;
;;A transcoder is a  machine word tagged to make it  of a disjoint type.
;;The transcoder data  (codec, EOL style, error handling)  is encoded in
;;the most significant bits of this word.
;;
;;  |---------------------------|------------| transcoder
;;         payload bits          transcoder-tag
;;
(section

 (define-core-primitive-operation transcoder? unsafe
   ((P x)
    (tag-test (V-simple-operand x) transcoder-mask transcoder-tag)))

 (define-core-primitive-operation $data->transcoder unsafe
   ;;Given a fixnum FX: encode it  as payload bits of a transcoder word.
   ;;In one step untag it as fixnum and tag it as transcoder.
   ;;
   ((V fx)
    (asm 'logor
	 (asm 'sll (V-simple-operand fx) (K (fx- transcoder-payload-shift fx-shift)))
	 (K transcoder-tag))))

 (define-core-primitive-operation $transcoder->data unsafe
   ;;Given a transcoder  word TRAN: extract the payload  bits and return
   ;;them as fixnum.
   ;;
   ((V tran)
    (asm 'sra (V-simple-operand tran) (K (fx- transcoder-payload-shift fx-shift)))))

 /section)


;;;; pointers
;;
;;A pointer is  a fixed length memory block,  two words wide, referenced
;;by  machine words  tagged as  vectors.  The  first machine  word  of a
;;pointer block is tagged has  pointer in its least significant bits and
;;it has the most significant bits set to zero.  The second machine word
;;of a pointer block holds the actual pointer value.
;;
;;  |------------------------|-------------| reference to pointer
;;        heap offset          vector tag
;;
;;  |------------------------|-------------| pointer first word
;;     all set to zero         pointer tag
;;
;;  |--------------------------------------| pointer second word
;;              pointer value
;;
(section

 (define-core-primitive-operation pointer? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag #f pointer-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $pointer? safe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag #f pointer-tag))
   ((E x)
    (nop)))

 (define-core-primitive-operation $pointer= unsafe
   ((V x y)
    ;;FIXME This is a predicate but a forcall is currently not supported
    ;;by the P function.  (Marco Maggi; Nov 30, 2011)
    (with-tmp ((arg1 (V-simple-operand x)))
      (with-tmp ((arg2 (V-simple-operand y)))
	(make-forcall "ikrt_pointer_eq" (list arg1 arg2)))))
   ((E x y)
    (nop)))

 /section)


;;;; closures
;;
;;A closure object is a fixed  length memory block referenced by machine
;;words tagged as closures; each closure  object is associated to a code
;;object that implements the procedure.   The memory layout of a closure
;;object is as follows:
;;
;; |------------------------|-------------| reference to closure
;;       heap pointer         closure tag
;;
;;                        0   1   2   3   4   5
;; |--------------------|---|---|---|---|---|---| memory block
;;   raw memory pointer    one slot for every
;;   to binary code        free variable
;;
;;the  first  word in  the  memory  block  holds  a raw  memory  pointer
;;referencing  the  first  byte  in the  code  object  implementing  the
;;closure; the  subsequent words  (if any) are  slots associated  to the
;;free variables referenced by the closure's code.
;;
(section

 (define-core-primitive-operation procedure? safe
   ;;Evaluate to true if X is a closure object.
   ;;
   ((P x)
    (tag-test (V-simple-operand x) closure-mask closure-tag)))

 (define-core-primitive-operation $cpref unsafe
   ;;Whenever  the body  of a  closure  references a  free variable  the
   ;;closure is closed upon...
   ;;
   ((V clo freevar-idx)
    (struct-case freevar-idx
      ((constant freevar-idx.val)
       ;;FREEVAR-IDX.VAL is an exact integer  whose payload bits are the
       ;;binary representation  of the index  of a free variable  in the
       ;;closure's data area; such index is zero-based.
       (unless (target-platform-fixnum? freevar-idx.val)
	 (interrupt))
       (asm 'mref (V-simple-operand clo) (K (+ off-closure-data (* freevar-idx.val wordsize)))))
      ((known freevar-idx.expr)
       (cogen-value-$cpref clo freevar-idx.expr))
      (else
       ;;Free variable indexes are always generated by the compiler.
       (compiler-internal-error '$cpref
	 "invalid free variable index, expected compile-time value"
	 freevar-idx)))))

 (define-core-primitive-operation $closure-code unsafe
   ;;First  extract  from the  closure  object  the raw  memory  pointer
   ;;referencing the first  byte of binary code in a  code object's data
   ;;area:
   ;;
   ;;  memory pointer = (asm 'mref (V-simple-operand closure) (K off-closure-code))
   ;;
   ;;we know that such memory  pointer is DISP-CODE-DATA bytes after the
   ;;pointer  to the  first  byte of  the code  object,  so we  subtract
   ;;DISP-CODE-DATA  and add  the vector  tag:  the result  is a  tagged
   ;;reference to the code object.
   ;;
   ;;    |----------| closure object
   ;;      p_memory
   ;;
   ;;      meta data        binary code
   ;;    |-----------|-----------------------| code object
   ;;    ^           ^
   ;; s_code      p_memory
   ;;
   ;;    |...........| disp-code-data
   ;;
   ((V clo)
    (asm 'int+
	 (asm 'mref (V-simple-operand clo) (K off-closure-code))
	 (K (fx- vector-tag disp-code-data)))))

 /section)


;;;; code objects
;;
;;A code object  is a fixed length memory block,  of variable size equal
;;to a  multiple of the  host's page  size, referenced by  machine words
;;tagged as  vectors; the memory is  allocated with "mmap" and  is given
;;read, write  and execution  protection.  The memory  layout of  a code
;;object is as follows:
;;
;;        page size          page size          page size
;;   |..................|..................|..................|
;;   |-----------|-----------------------------------|++++++++|
;;     meta data                 data area             unused
;;
;;if the  requested memory  is less  than the exact  multiple of  a page
;;size,  the  last  allocated  bytes  are unused  and  set  to  zero  at
;;allocation time.
;;
;;The first machine  word of a code  memory block is tagged  has code in
;;its least significant bits and it has the most significant bits set to
;;zero:
;;
;;   |------------------------|-------------| reference to code
;;       memory pointer         vector tag
;;
;;   |------------------------|-------------| code first word
;;      all set to zero          code tag
;;
(section

 ;;An object is a code object if the reference to it is tagged as vector
 ;;and if the first word is tagged as code.
 (define-core-primitive-operation code? unsafe
   ((P x)
    (sec-tag-test (V-simple-operand x) vector-mask vector-tag #f code-tag)))

 (define-core-primitive-operation $code-freevars unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-code-freevars))))

 (define-core-primitive-operation $code-reloc-vector unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-code-relocsize))))

 (define-core-primitive-operation $code-size unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-code-instrsize))))

 (define-core-primitive-operation $code-annotation unsafe
   ((V x)
    (asm 'mref (V-simple-operand x) (K off-code-annotation))))

 (define-core-primitive-operation $code->closure unsafe
   ((V code)
    (with-tmp
	;;Allocate a closure's memory block  and tag the reference to it
	;;as closure.
	((clo (asm 'alloc
		   (K (align (+ 0 disp-closure-data)))
		   (K closure-tag))))
      ;;Store in the  closure's memory block a raw pointer  to the first
      ;;byte of the code object's data area.
      (asm 'mset clo (K off-closure-code)
	   (asm 'int+ (V-simple-operand code) (K off-code-data)))
      clo)))

 (define-core-primitive-operation $code-ref unsafe
   ((V code idx)
    (prm-tag-as-fixnum
     (prm-isolate-least-significant-byte
      (asm 'bref (V-simple-operand code)
	   (asm 'int+ (prm-UNtag-as-fixnum (V-simple-operand idx))
		(K off-code-data)))))))

 (define-core-primitive-operation $code-set! unsafe
   ((E code idx val)
    (asm 'bset (V-simple-operand code)
	 (asm 'int+ (prm-UNtag-as-fixnum (V-simple-operand idx))
	      (K off-code-data))
	 (prm-UNtag-as-fixnum (V-simple-operand val)))))

 (define-core-primitive-operation $set-code-annotation! unsafe
   ((E code ann)
    (mem-assign ann (V-simple-operand code) off-code-annotation)))

 /section)


;;;; hash table tcbuckets
;;
;;A tcbucket is a fixed-length  memory block referenced by machine words
;;tagged as  vectors; a  tcbucket is  not a Scheme  object like  all the
;;others; it is used only to store values in a hashtable.
;;
;;   |------------------------|-------------| reference to tcbucket
;;         heap pointer         vector tag
;;
;;The memory layout of a tcbucket is as follows:
;;
;;   |-----|-----|-----|-----| tcbucket memory block
;;    tconc  key   val  next
;;
;;For details on such objects, see the paper:
;;
;;   Ghuloum,   Dybvig.    ``Generation--Friendly  Eq   Hash   Tables''.
;;   Proceedings  of   the  2007  Workshop  on   Scheme  and  Functional
;;   Programming.
;;
(section

 (define-core-primitive-operation $make-tcbucket unsafe
   ((V tconc key val next)
    (with-tmp ((buck (asm 'alloc
			  (K (align tcbucket-size))
			  (K vector-tag))))
      (asm 'mset buck (K off-tcbucket-tconc) (V-simple-operand tconc))
      (asm 'mset buck (K off-tcbucket-key)   (V-simple-operand key))
      (asm 'mset buck (K off-tcbucket-val)   (V-simple-operand val))
      (asm 'mset buck (K off-tcbucket-next)  (V-simple-operand next))
      buck)))

;;; --------------------------------------------------------------------
;;; accessors

 (define-core-primitive-operation $tcbucket-key unsafe
   ((V buck)
    (asm 'mref (V-simple-operand buck) (K off-tcbucket-key))))

 (define-core-primitive-operation $tcbucket-val unsafe
   ((V buck)
    (asm 'mref (V-simple-operand buck) (K off-tcbucket-val))))

 (define-core-primitive-operation $tcbucket-next unsafe
   ((V buck)
    (asm 'mref (V-simple-operand buck) (K off-tcbucket-next))))

;;; --------------------------------------------------------------------
;;; mutators

 (define-core-primitive-operation $set-tcbucket-key! unsafe
   ((E buck val)
    (mem-assign val (V-simple-operand buck) off-tcbucket-key)))

 (define-core-primitive-operation $set-tcbucket-val! unsafe
   ((E buck val)
    (mem-assign val (V-simple-operand buck) off-tcbucket-val)))

 (define-core-primitive-operation $set-tcbucket-next! unsafe
   ((E buck val)
    (mem-assign val (V-simple-operand buck) off-tcbucket-next)))

 (define-core-primitive-operation $set-tcbucket-tconc! unsafe
   ((E buck val)
    (mem-assign val (V-simple-operand buck) off-tcbucket-tconc)))

 /section)


;;;; interrupts-and-engines
;;
;;These  primitive operations  make  use of  some fields  of  the PCB  C
;;language data  structure; a pointer to  the PCB is meant  to be always
;;stored in the process control register (PC-REGISTER).
;;
(section

 (define-core-primitive-operation $interrupted? unsafe
   ;;Evaluate  to true  if the  field  "interrupted" of  the C  language
   ;;struct PCB is not zero.
   ;;
   ((P)
    (asm '!= (asm 'mref PC-REGISTER (K pcb-interrupted)) (K 0))))

 (define-core-primitive-operation $unset-interrupted! unsafe
   ;;Set to zero the field "interrupted" of the C language struct PCB.
   ;;
   ((E)
    (asm 'mset PC-REGISTER (K pcb-interrupted) (K 0))))

 (define-core-primitive-operation $do-event safe
   ;;Set to 1 the field "engine_counter" of the C language struct PCB.
   ;;
   ((E)
    (begin
      (interrupt)
      (asm 'incr/zero? PC-REGISTER (K pcb-engine-counter)
	   (K (fxsll 1 fx-shift))))))

 (define-core-primitive-operation $swap-engine-counter! unsafe
   ;;Set to X  the field "engine_counter" of the C  language struct PCB;
   ;;return the previous field value.
   ;;
   ((V x)
    ;;FIXME: should be atomic swap  instead of load and set!  (Abdulaziz
    ;;Ghuloum)
    ;;
    (with-tmp ((x0 (V-simple-operand x)))
      (with-tmp ((t (asm 'mref PC-REGISTER (K pcb-engine-counter))))
	(asm 'mset PC-REGISTER (K pcb-engine-counter) x0)
	t))))

 /section)


;;;; control operations
;;
;;These primitives are mostly used by "ikarus.control.sls".
;;
;;Recall that a Scheme stack segment is laid out as follows:
;;
;;         high memory
;;   |                      |
;;   |----------------------|
;;   |                      | <-- pcb->frame_base
;;   |----------------------|
;;   | ik_underflow_handler | <-- pcb->frame_base - wordsize
;;   |----------------------|
;;   |      used word       |
;;   |----------------------|
;;   |           .          |
;;   |           .          |
;;   |           .          |
;;   |----------------------|
;;   |     return address   | <-- Frame Pointer Register
;;   |----------------------|
;;   |           .          |
;;   |           .          |
;;   |           .          |
;;   |----------------------|
;;   |      free word       | <-- pcb->stack_base
;;   |----------------------|
;;   |                      |
;;          low memory
;;
;;and remember that "pcb->frame_base" references  a word that is one-off
;;the end of the stack segment; so the first word in the stack is:
;;
;;   pcb->frame_base - wordsize
;;
;;"ik_underflow_handler"  is  an  assembly  label defined  in  the  file
;;"ikarus-enter.S", to which  the execution flow returns  after the last
;;Scheme code execution using this stack segment completes.
;;
(section

 (define-core-primitive-operation $fp-at-base unsafe
   ;;Evaluate  to true  if the  Frame Pointer  Register (FP-REGISTER)  references the
   ;;highest  machine word  in  the Scheme  stack  segment as  described  by the  PCB
   ;;structure,  the highest  machine word  is  the one  holding the  address of  the
   ;;assembly label "ik_underflow_handler".
   ;;
   ;;          high memory
   ;;   |                      | <-- pcb->frame_base
   ;;   |----------------------|
   ;;   | ik_underflow_handler | <-- FP-REGISTER
   ;;   |----------------------|
   ;;   |                      |
   ;;          low memory
   ;;
   ;;The Process  Control Register (PC-REGISTER)  contains the memory address  of the
   ;;PCB structure.
   ;;
   ;;FP-REGISTER is at the frame base:
   ;;
   ;;1..Right after  a new Scheme  stack segment has been  allocated and
   ;;   initialised.
   ;;
   ;;2..After the  execution flow  has returned  from a  Scheme function
   ;;   call whose stack frame is the last on the stack segment.
   ;;
   ((P)
    (asm '= (asm 'int+ (asm 'mref PC-REGISTER (K pcb-frame-base))
		 (K (- wordsize)))
	 FP-REGISTER)))

 (define-core-primitive-operation $current-frame unsafe
   ;;Extract from  the PCB  structure a reference  to the  "next process
   ;;continuation" and  return it.   In C  language terms:  the returned
   ;;value is "pcb->next_k".
   ;;
   ((V)
    (asm 'mref PC-REGISTER (K pcb-next-continuation))))

 (define-core-primitive-operation $seal-frame-and-call unsafe
   ;;This  primitive  operation is  used  to  implement  CALL/CC (call  with  current
   ;;continuation) and CALL/CF (call  with current frame), file "ikarus.control.sls".
   ;;Let's  super  simplify   and  comment  the  code  starting  with   the  call  to
   ;;%PRIMITIVE-CALL/CF which is the heart of  both CALL/CC and CALL/CF; the function
   ;;%PRIMITIVE-CALL/CF contains the code:
   ;;
   ;;   ($seal-frame-and-call receiver-func)
   ;;
   ;;where RECEIVER-FUNC  is a  local binding  evaluating to  a closure  object: this
   ;;closure accepts a  single argument being the continuation  object describing the
   ;;freezed frames.
   ;;
   ;;Remember that:
   ;;
   ;;* FPR stands for Frame Pointer Register.
   ;;
   ;;* PC-REGISTER  stands for Process Control  Register and it references  the structure PCB
   ;;  defined at the C language level.
   ;;
   ;;* AA-REGISTER stands for Argument Count Register.
   ;;
   ;;Upon   entering   this  primitive   operation   it   has  been   determined   by
   ;;%PRIMITIVE-CALL/CF that the scenario on the Scheme stack is:
   ;;
   ;;         high memory
   ;;   |                      | <-- pcb->frame_base
   ;;   |----------------------|
   ;;   | ik_underflow_handler |
   ;;   |----------------------|
   ;;     ... other frames ...
   ;;   |----------------------|          --
   ;;   |     local value 1    |          .
   ;;   |----------------------|          .
   ;;   |     local value 1    |          . frame 1
   ;;   |----------------------|          .
   ;;   |   return address 1   |          .
   ;;   |----------------------|          --
   ;;   |     local value 0    |          .
   ;;   |----------------------|          .
   ;;   |     local value 0    |          . frame 0
   ;;   |----------------------|          .
   ;;   |   return address 0   | <-- FPR  .
   ;;   |----------------------|          --
   ;;   |     receiver-func    | --> closure object
   ;;   |----------------------|
   ;;             ...
   ;;   |----------------------|
   ;;   |      free word       | <-- pcb->stack_base
   ;;   |----------------------|
   ;;   |                      |
   ;;          low memory
   ;;
   ;;where RECEIVER-FUNC (the  argument to this primitive operation) is  a VAR struct
   ;;representing the memory location on the Scheme stack containing a reference to a
   ;;closure object.
   ;;
   ;;The value "return address 0" leads back to the caller of %PRIMITIVE-CALL/CF.
   ;;
   ;;It goes like this:
   ;;
   ;;1. We freeze the used portion of  the current Scheme stack segment, as described
   ;;   by the PCB structure, into a new continuation object KONT-OBJ.
   ;;
   ;;2. We  push the  new continuation  object to  the PCB's  stack of  "next process
   ;;   continuations".
   ;;
   ;;3.  We apply the closure object RECEIVER-FUNC to the object KONT-OBJ.  This step
   ;;   is  actually performed by CALL-WITH-UNDERFLOW-HANDLER,  a high-level Assembly
   ;;   instruction.
   ;;
   ((V receiver-func)
    (assert (var? receiver-func))
    ;;Here we perform the allocation using ALLOC-NO-HOOKS, which does not execute the
    ;;post-GC hooks.  When  we come here we  have already determined that  the FPR is
    ;;not at  the base  of the  Scheme stack;  running the  post-GC could  change the
    ;;scenario leaving  the FPR at  base and so causing  the generation of  a corrupt
    ;;continuation object (with  size 0 and the underflow handler  as return point of
    ;;the topmost stack frame).
    (with-tmp ((kont-obj (asm 'alloc-no-hooks (K continuation-size) (K vector-tag))))
      ;;BASE references the underflow handler:
      ;;
      ;;        high memory
      ;; |                      | <-- pcb->frame_base
      ;; |----------------------|
      ;; | ik_underflow_handler | <-- BASE = pcb->frame_base - wordsize
      ;; |----------------------|
      ;; |                      |
      ;;       low memory
      ;;
      (with-tmp ((base (asm 'int+
			    (asm 'mref PC-REGISTER (K pcb-frame-base))
			    (K (- wordsize)))))
	(with-tmp ((underflow-handler (asm 'mref base (K 0))))
	  ;;Store the continuation tag in the first word.
	  (asm 'mset kont-obj (K off-continuation-tag)  (K continuation-tag))
	  ;;Set the  current Frame  Pointer Register  as address to  go back  to when
	  ;;resuming the continuation.
	  (asm 'mset kont-obj (K off-continuation-top)  FP-REGISTER)
	  ;;Set the number of bytes representing  the total size of the freezed stack
	  ;;frames.
	  (asm 'mset kont-obj (K off-continuation-size) (asm 'int- base FP-REGISTER))
	  ;;Prepend the new  continuation object to the linked list  of "next process
	  ;;continuations" in the PCB.
	  ;;
	  ;;   s_kont->next  = pcb->next_k;
	  ;;   pcb->next_k   = s_kont->next;
	  ;;
	  (asm 'mset kont-obj (K off-continuation-next) (asm 'mref PC-REGISTER (K pcb-next-continuation)))
	  (asm 'mset PC-REGISTER (K pcb-next-continuation) kont-obj)
	  ;;The machine word containing "return address 0" (the one referenced by the
	  ;;FPR) is the  new frame base for subsequent code  execution; store the FPR
	  ;;in the PCB as frame base.
	  (asm 'mset PC-REGISTER (K pcb-frame-base) FP-REGISTER)
	  ;;When arriving here the situation of the Scheme stack is:
	  ;;
	  ;;         high memory
	  ;;   |                      |
	  ;;   |----------------------|
	  ;;   | ik_underflow_handler |
	  ;;   |----------------------|                           --
	  ;;     ... other frames ...                             .
	  ;;   |----------------------|                           .
	  ;;   |     local value 1    |                           .
	  ;;   |----------------------|                           .
	  ;;   |     local value 1    |                           .
	  ;;   |----------------------|                           . freezed
	  ;;   |   return address 1   |                           . frames
	  ;;   |----------------------|                           .
	  ;;   |     local value 0    |                           .
	  ;;   |----------------------|                           .
	  ;;   |     local value 0    |                           .
	  ;;   |----------------------|                           .
	  ;;   |   return address 0   | <- FPR = pcb->frame_base  .
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
	  ;;AA-REGISTER still contains the encoded  number of arguments, counting the
	  ;;single  argument RECEIVER-FUNC  to %PRIMITIVE-CALL/CF;  the reference  to
	  ;;continuation  object KONT-OBJ  is in  some VAR  location; the  raw memory
	  ;;pointer UNDERFLOW-HANDLER is in some VAR register.
	  ;;
	  ;;NOTE Here  we know  that UNDERFLOW-HANDLER and  KONT-OBJ are  already VAR
	  ;;structs.
	  (asm 'call-with-underflow-handler underflow-handler kont-obj (V-simple-operand receiver-func))))))
   ((E . args)
    (interrupt))
   ((P . args)
    (interrupt)))

 (define-core-primitive-operation $frame->continuation unsafe
   ;;Build  and return  a  new  closure object.   When  such closure  is
   ;;invoked:    it   makes    use    of    the   assembly    subroutine
   ;;SL-CONTINUATION-CODE to  resume the  execution of  the continuation
   ;;object X.
   ;;
   ;;The continuation  object X  is stored  in the  first slot  for free
   ;;variables.
   ;;
   ((V x)
    (with-tmp ((clo (asm 'alloc
			 (K (align (+ disp-closure-data wordsize)))
			 (K closure-tag))))
      (asm 'mset clo (K off-closure-code) (K (make-code-loc (sl-continuation-code-label))))
      (asm 'mset clo (K off-closure-data) (V-simple-operand x))
      clo))
   ((P x)
    (K #t))
   ((E x)
    (nop)))

 (define-core-primitive-operation $stack-overflow-check unsafe
   ;;Check if  the topmost Scheme stack  call frame has crossed  the red
   ;;line of  stack usage; this  condition triggers a new  stack segment
   ;;allocation.   The   scenario  on   the  stack  that   triggers  the
   ;;reallocation is:
   ;;
   ;;         high memory
   ;;   |                      | <- pcb->frame_base
   ;;   |----------------------|
   ;;   | ik_underflow_handler |
   ;;   |----------------------|
   ;;            ...
   ;;   |----------------------|                        --
   ;;   |     local value      |                        .
   ;;   |----------------------|                        .
   ;;   |     local value      | <- pcb->frame_redline  .
   ;;   |----------------------|                        . framesize
   ;;   |     local value      |                        .
   ;;   |----------------------|                        .
   ;;   |    return address    | <- FPR                 .
   ;;   |----------------------|                        --
   ;;   |   function argument  |
   ;;   |----------------------|
   ;;   |   function argument  |
   ;;   |----------------------|
   ;;             ...
   ;;   |----------------------|
   ;;   |                      | <- pcb->stack_base
   ;;   |----------------------|
   ;;   |                      |
   ;;         low memory
   ;;
   ;;we see that: FPR < pcb->frame_redline.
   ;;
   ;;This operation is inserted right after  the entry point of the body
   ;;of *every* Scheme function that does enlarge the stack; it is *not*
   ;;inserted in the body of functions that only perform tail calls.
   ;;
   ;;See  the  comments  in  the C  function  "ik_stack_overflow()"  for
   ;;details of what happens in case of stack overflow.
   ;;
   ;;The generated code looks somewhat like this i686 pseudo-Assembly:
   ;;
   ;;   (label function_entry_point)
   ;;     (cmpl FPR pcb->frame_redline)
   ;;     (jb L0)
   ;;
   ;;   (label L1)
   ;;     ;; ... the function body ...
   ;;     (ret)
   ;;
   ;;   (label L0)
   ;;     (forcall "ik_stack_overflow")
   ;;     (jmp L1)
   ;;
   ;;NOTE The Assembly instruction JB is for comparison between UNsigned
   ;;integers, while JL is for comparison between signed integers.
   ;;
   ((E)
    (make-shortcut
	(make-conditional (asm 'u< FP-REGISTER (asm 'mref PC-REGISTER (K pcb-frame-redline)))
	    (asm 'interrupt)
	  (nop))
      (make-forcall "ik_stack_overflow" '()))))

 /section)


;;;; multiple values operations
;;
(section

 (define-core-primitive-operation $make-call-with-values-procedure unsafe
   ;;Return a CLOSURE-MAKER struct  representing code that, evaluated at
   ;;run-time,    returns   a    closure    object   implementing    the
   ;;CALL-WITH-VALUES  primitive function  through the  assembly routine
   ;;"SL_call_with_values".
   ;;
   ((V)
    (K (make-closure-maker (make-code-loc (sl-cwv-label)) '())))
   ((P)
    (interrupt))
   ((E)
    (interrupt)))

 (define-core-primitive-operation $make-values-procedure unsafe
   ;;Return a CLOSURE-MAKER struct  representing code that, evaluated at
   ;;run-time,  returns   a  closure  object  implementing   the  VALUES
   ;;primitive function through the assembly routine "SL_values".
   ;;
   ((V)
    (K (make-closure-maker (make-code-loc (sl-values-label)) '())))
   ((P)
    (interrupt))
   ((E)
    (interrupt)))

 /section)


;;;; annotated procedures
;;
;;Annotated  procedures  are  closure  objects  wrapping  other  closure
;;objects;  the outer  closure just  hands  its arguments  to the  inner
;;closure.  An annotated procedure contains an annotation object.
;;
(section

 (define-core-primitive-operation $make-annotated-procedure unsafe
   ;;Build and return  a new closure object wrapping  the closure object
   ;;PROC  and  just adding  an  annotation  object  to it.   When  such
   ;;annotated closure is applied: it  makes use of the assembly routine
   ;;"SL_annotated" to call the closure PROC.
   ;;
   ;;FIXME  It appears  that  this is  never used:  it  is available  as
   ;;primitive operation for the brave user.  Notice, though, that it is
   ;;interesting to annotate  a function with whatever  Scheme object we
   ;;want.  (Marco Maggi; Oct 25, 2012)
   ;;
   ((V annotation proc)
    (with-tmp ((clo (asm 'alloc
			 (K (align (+ disp-closure-data (* 2 wordsize))))
			 (K closure-tag))))
      (asm 'mset clo (K off-closure-code)
	   (K (make-code-loc (sl-annotated-procedure-label))))
      ;;Store the annotation in the first slot for free variables.
      (asm 'mset clo (K off-closure-data)              (V-simple-operand annotation))
      ;;Store the wrapped closure in the second slot for free variables.
      (asm 'mset clo (K (+ off-closure-data wordsize)) (V-simple-operand proc))
      clo))
   ((P)
    (interrupt))
   ((E)
    (interrupt)))

 (define-core-primitive-operation $annotated-procedure-annotation unsafe
   ;;Given a closure object PROC being an annotated procedure created by
   ;;$MAKE-ANNOTATED-PROCEDURE: return the annotation object.
   ;;
   ((V proc)
    (asm 'mref (V-simple-operand proc) (K off-closure-data))))

 /section)


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;eval: (put 'make-conditional	'scheme-indent-function 2)
;;eval: (put 'make-shortcut	'scheme-indent-function 1)
;;eval: (put 'with-tmp		'scheme-indent-function 1)
;;eval: (put 'struct-case	'scheme-indent-function 1)
;;eval: (put 'check-flonums	'scheme-indent-function 1)
;;End:
