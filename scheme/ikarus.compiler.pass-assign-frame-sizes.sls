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


#!vicare
(library (ikarus.compiler.pass-assign-frame-sizes)
  (export
    pass-assign-frame-sizes
    FRAME-CONFLICT-SETS)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code)
    (ikarus.compiler.intel-assembly))


;;;; recapitulation
;;
;;Let's  clarify where  we are  when  this compiler  pass  is applied.   There are  3
;;entities that we can compile: libraries, programs, standalone EVAL expressions.
;;
;;* After the  previous compiler passes: the fact that  a standalone expression might
;;  reference bindings  defined by  previous standalone  expressions does  not matter
;;  anymore; those  references and assignments  have been converted  into appropriate
;;  function calls.
;;
;;* After the previous compiler passes: all  the code has been reorganised into a set
;;  of CLAMBDA structs and an initialisation expression.
;;
;;*  Every clause  in the  CLAMBDA structs  can be  processed independently,  without
;;  informations from other clauses.
;;
;;* There is no  significant difference between the body of a  CLAMBDA clause and the
;;  body of the  init expression: the body  of the init expression is  like a CLAMBDA
;;  clause's body with no stack operands and no closure variables.
;;
;;So here we  can just consider the  bodies: if we understand how  a CLAMBDA clause's
;;body is  processed we get everything.
;;
;;
;;Note about NON-TAIL-CALL-FRAME and NON-TAIL-CALL structs
;;--------------------------------------------------------
;;
;;The previous  compiler pass "impose  evaluation order" is  the only place  in which
;;structs  of  type  NON-TAIL-CALL-FRAME   and  NON-TAIL-CALL  are  introduced.   The
;;ALL-RAND* field of a NON-TAIL-CALL struct holds a list of items:
;;
;;  (AAR APR CPR FPR PCR . rand*.nfv)
;;
;;first  come  all the  register  operands  (in alphabetical  order,  but  it is  not
;;important), then come the NFV structs representing the stack locations in which the
;;stack   operands  for   the  call   must  be   placed.   The   RAND*  field   of  a
;;NON-TAIL-CALL-FRAME contains the  list RAND*.NFV that is the tail  of the ALL-RAND*
;;field in the associated NON-TAIL-CALL struct.
;;
;;This  compiler  pass  consumes  structs of  type  NON-TAIL-CALL-FRAME  and  returns
;;recordised code that contains them no more.
;;
;;
;;Note about ASMCALL structs
;;--------------------------
;;
;;The previous  compiler pass "impose  evaluation order" is  the only place  in which
;;ASMCALL structs  with operator  RETURN, DIRECT-JUMP, INDIRECT-JUMP  are introduced.
;;We expect them to have the format:
;;
;;   (asmcall direct-jump   (?target AAR APR CPR FPR PCR . rand*.fvar))
;;   (asmcall indirect-jump         (AAR APR CPR FPR PCR . rand*.fvar))
;;   (asmcall return                (AAR APR     FPR PCR))
;;
;;where: RAND*.FVAR is a list of FVAR  structs representing the location on the stack
;;in which  the stack operands of  the tail-call must  be put; ?TARGET is  a CODE-LOC
;;wrapping the name of the tail-call target Assembly label.
;;
;;
;;Note about register operands
;;----------------------------
;;
;;The register operands AAR,  APR, CPR, FPR, PCR are not the  only CPU registers that
;;may  appear in  the input  form.   Some Assembly  instructions require  the use  of
;;specific registers; namely ECX and EDX.
;;
;;
;;Local variables
;;---------------
;;
;;Every body is  an expression resulting from the composition  of subexpressions; the
;;subexpressions form a  tree-like hierarchy.  In a previous compiler  pass: for each
;;body  a list  of  local variables  has been  gathered,  representing the  temporary
;;locations needed to hold data and partial results from computations; such lists are
;;stored in  LOCALS structs.
;;
;;Some local variables exists only in branches of the tree, for example:
;;
;;   (conditional ?test
;;       (bind ((a ?rhs-a))
;;         ?conseq)
;;     (bind ((b ?rhs-b))
;;       ?altern))
;;
;;the variable A exists only in the ?CONSEQ,  while the variable B exists only in the
;;?ALTERN.
;;
;;To minimise  the use of  resources: local  variables, including stack  and register
;;operands,  are stored  in/mapped to  the CPU  registers and  the minimum  amount of
;;Scheme stack locations.  Allocating CPU registers to local variables is preferable,
;;when possible, because  read and write operations to registers  are faster than the
;;equivalent to  the stack.   Finding the  minimum set  of stack  location is  a hard
;;problem.
;;
;;
;;Local variables liveness
;;------------------------
;;
;;At every point in the computation: the set  of local variables that are read in the
;;continuation  is  called the  "live  set".   Knowing which  locals  are  live in  a
;;subexpression and  which are shared between  subexpressions is needed to  map local
;;variables to available CPU registers.
;;
;;


;;;; CPU registers and stack space allocation
;;
;;For an introduction to register allocation see:
;;
;;   <http://en.wikipedia.org/wiki/Register_allocation>
;;
;;For an introduction to live variables analysis see:
;;
;;   <http://en.wikipedia.org/wiki/Data-flow_analysis>
;;   <http://en.wikipedia.org/wiki/Liveness_analysis>
;;
;;A paper on register allocation:
;;
;;   G. J.   Chaitin.  "Register  allocation and spilling  via graph  coloring".  IBM
;;   Research.
;;
;;another paper:
;;
;;   R. Burger, O.  Waddell, R.  K.  Dybvig.  "Register Allocation  Using Lazy Saves,
;;   Eager  Restores, and  Greedy  Shuffling".  Indiana  University Computer  Science
;;   Department.
;;


;;;; introduction
;;
;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
;;recordized code must be composed by struct instances of the following types:
;;
;;   asm-instr	conditional	constant
;;   asmcall		seq		shortcut
;;   locals		non-tail-call	non-tail-call-frame
;;
;;in  addition CLOSURE-MAKER  and  CODE-LOC  structs can  appear  in side  CONSTANT
;;structs.  The  only ASMCALL operators  still accepted  as input in  this compiler
;;pass are:
;;
;;   return			indirect-jump		direct-jump
;;   nop			interrupt		incr/zero?
;;   fl:double->single	fl:single->double
;;
;;NOTE In the  input and output recordised code: the  ASM-INSTR structs contain, as
;;operands: DISP structs, FVAR structs,  CONSTANT structs, symbols representing CPU
;;register names,  VAR structs  with LOC field  set to #f.   Among these,  the DISP
;;structs  have:  as  OBJREF  field,  a   CONSTANT,  FVAR,  VAR  struct  or  symbol
;;representing a CPU register name; as OFFSET field, a CONSTANT or VAR struct.  The
;;VAR structs in the DISP have LOC field set to #f.
;;
;;NOTE The returned code does not contain NFV structs anymore.
;;

(define-syntax __module_who__
  (identifier-syntax 'assign-frame-sizes))

(define (pass-assign-frame-sizes x)
  (E-codes x))


;;;; non-tail calls and nested non-tail calls
;;
;;We want  to make clear how  the stack locations  are allocated and used  to perform
;;non-tail function calls.   This reasoning will make us understand  some of the work
;;this compiler pass does,  and what is the use of the livemask  in the non-tail call
;;table.  We  will focus  on the  stack operands, leaving  out the  register operands
;;(which are described elsewhere).
;;
;;Let's consider the common  case in which we are already inside  a function call and
;;we perform another function call: a non-tail  call.  The old, uplevel call frame is
;;already on the stack along with the local  variables used so far by the function we
;;are executing:
;;
;;           high memory
;;   |                          |         --
;;               ...                      .
;;   |--------------------------|         . uplevel stack frame
;;   | uplevel return address   | <-- FPR .
;;   |--------------------------|         --
;;   | uplevel stack operand 0  | fvar.1
;;   |--------------------------|
;;   | uplevel stack operand 1  | fvar.2
;;   |--------------------------|
;;   |       local var 0        | fvar.3 <- var.1
;;   |--------------------------|
;;   |       local var 1        | fvar.4 <- var.2
;;   |--------------------------|
;;   |                          |
;;           low memory
;;
;;the  uplevel stack  operands  are  represented by  FVAR  structs,  while the  local
;;variables are represented by VAR structs having FVAR structs in their LOC field.
;;
;;The case in which  we are not already in a function call  is equivalent; instead of
;;the uplevel return address  we have the address of the  underflow handler and there
;;are no uplevel stack operands:
;;
;;           high memory
;;   |                          |
;;   |--------------------------|
;;   |   ik_underflow_handler   | <-- FPR
;;   |--------------------------|
;;   |       local var 0        |
;;   |--------------------------|
;;   |       local var 1        |
;;   |--------------------------|
;;   |                          |
;;           low memory
;;
;;A non-tail call  is described, in recordised code, by  a struct NON-TAIL-CALL-FRAME
;;which includes everything needed to: compute  the stack operands, perform the call,
;;accept  the returned  values.  The  NON-TAIL-CALL-FRAME marks  the point  where the
;;stack locations in which stack operand will be stored are allocated; so right after
;;we enter the code described by a NON-TAIL-CALL-FRAME, the scenario is:
;;
;;           high memory
;;   |                          |                  --
;;               ...                               .
;;   |--------------------------|                  . uplevel stack frame
;;   | uplevel return address   | <-- FPR          .
;;   |--------------------------|                  --
;;   | uplevel stack operand 0  | fvar.1           .
;;   |--------------------------|                  .
;;   | uplevel stack operand 1  | fvar.2           .
;;   |--------------------------|                  . stack frame described
;;   |       local var 0        | fvar.3 <- var.1  . by this call's call table
;;   |--------------------------|                  .
;;   |       local var 1        | fvar.4 <- var.2  .
;;   |--------------------------|                  .
;;   |        empty word        | fvar.5           .
;;   |--------------------------|                  --
;;   |       still unused       | fvar.6 <- nfv.1
;;   |--------------------------|
;;   |       still unused       | fvar.7 <- nfv.2
;;   |--------------------------|
;;   |                          |
;;           low memory
;;
;;all the stack locations will be represented, sooner or later, by FVAR structs whose
;;index is  the number  of machine  words from  the location  referenced by  FPR; the
;;"empty word"  FVAR.5 will be filled  by the return  address of the function  we are
;;about to call; the stack operands locations are represented by NFV structs.
;;
;;Right after we enter the code  described by a NON-TAIL-CALL-FRAME struct: the stack
;;locations  for the  NFV structs  are  (conceptually) already  allocated, but  still
;;unused.
;;
;;The  recordised  code  represented  by  the BODY  field  of  a  NON-TAIL-CALL-FRAME
;;describes: how the values of the stack  operands are computed; when such values are
;;actually stored in the NFV locations; how  the non-tail call is performed through a
;;NON-TAIL-CALL struct describing the call table.   While computing the values of the
;;stack operands: temporary locations may be needed, and they are allocated below the
;;NFV locations:
;;
;;           high memory
;;   |                          |                    --
;;               ...                                 .
;;   |--------------------------|                    . uplevel stack frame
;;   | uplevel return address   | <-- FPR            .
;;   |--------------------------|                    --
;;   | uplevel stack operand 0  | fvar.1             .
;;   |--------------------------|                    .
;;   | uplevel stack operand 1  | fvar.2             .
;;   |--------------------------|                    . stack frame described
;;   |       local var 0        | fvar.3 <- var.1    . by this call's call table
;;   |--------------------------|                    .
;;   |       local var 1        | fvar.4 <- var.2    .
;;   |--------------------------|                    .
;;   |        empty word        | fvar.5             .
;;   |--------------------------|                    --
;;   |       still unused       | fvar.6 <- nfv.1
;;   |--------------------------|
;;   |       still unused       | fvar.7 <- nfv.2
;;   |--------------------------|
;;   |     temporary var 0      | fvar.8 <- var.3
;;   |--------------------------|
;;   |     temporary var 1      | fvar.9 <- var.4
;;   |--------------------------|
;;   |                          |
;;           low memory
;;
;;such temporary  locations are represented  by VAR structs  having a FVAR  struct in
;;their LOC field.  While computing the stack operands: it is possible to temporarily
;;use the stack locations allocated to  the NFV structs; for example, while computing
;;the value of NFV.1, we can temporarily use the location FVAR.7.
;;
;;Right before  the code  represented by  the NON-TAIL-CALL  struct is  executed: the
;;values of the stack operands are in place, and the scenario is:
;;
;;           high memory
;;   |                          |                    --
;;               ...                                 .
;;   |--------------------------|                    . uplevel stack frame
;;   | uplevel return address   | <-- FPR            .
;;   |--------------------------|                    --
;;   | uplevel stack operand 0  | fvar.1             .
;;   |--------------------------|                    .
;;   | uplevel stack operand 1  | fvar.2             .
;;   |--------------------------|                    . stack frame described
;;   |       local var 0        | fvar.3 <- var.1    . by this call's call table
;;   |--------------------------|                    .
;;   |       local var 1        | fvar.4 <- var.2    .
;;   |--------------------------|                    .
;;   |        empty word        | fvar.5             .
;;   |--------------------------|                    --
;;   |     stack operand 0      | fvar.6 <- nfv.1
;;   |--------------------------|
;;   |     stack operand 1      | fvar.7 <- nfv.2
;;   |--------------------------|                    --
;;   |     temporary var 0      | fvar.8 <- var.3    .
;;   |--------------------------|                    . no more needed
;;   |     temporary var 1      | fvar.9 <- var.4    .
;;   |--------------------------|                    --
;;   |                          |
;;           low memory
;;
;;the temporary  vars are no  more needed, they will  be overwritten by  whatever the
;;callee function does.
;;
;;Good.
;;
;;Now let's suppose that,  while computing the values of the  stack operands, we need
;;to perform another non-tail function call;  this happens for example in expressions
;;like:
;;
;;   (let ((f (lambda (a b) (_  a b)))
;;         (g (lambda (y)   (_ '1 y)))
;;         (h (lambda (z)   (_ '2 z))))
;;     (begin
;;       (f (g '3) (h '4))
;;       '5))
;;
;;to compute the stack operands  of the call to F, we need to call  G and then H.  So
;;let's consider  the recordised code (which  is an excerpt  from a test in  the test
;;suite):
;;
;;   (non-tail-call-frame
;;     (rand*: nfv.1_0 nfv.2_0)
;;     (live: ---)
;;     (seq
;;
;;       (non-tail-call-frame
;;         (rand*: nfv.1_1)
;;         (live: ---)
;;         (seq
;;           (asm-instr move nfv.1_1 (constant 24))
;;           ---
;;           (non-tail-call
;;             (target: asmlabel:g:clambda:case-1)
;;             (retval-var: nfv.1_0)
;;             (all-rand*: AAR APR CPR FPR PCR nfv.1_1)
;;             (mask: #f)
;;             (size: #f))))
;;       (asm-instr move nfv.1_0 AAR)
;;
;;       (non-tail-call-frame
;;         (rand*: nfv.1_2)
;;         (live: #f)
;;         (seq
;;           (asm-instr move nfv.1_2 (constant 32))
;;           ---
;;           (non-tail-call
;;             (target: asmlabel:h:clambda:case-1)
;;             (retval-var: nfv.2_0)
;;             (all-rand*: AAR APR CPR FPR PCR nfv.1_2)
;;             (mask: #f)
;;             (size: #f))))
;;       (asm-instr move nfv.2_0 AAR)
;;
;;       (non-tail-call
;;         (target: asmlabel:f:clambda:case-2)
;;         (retval-var: #f)
;;         (all-rand*: AAR APR CPR FPR PCR nfv.1_0 nfv.2_0)
;;         (mask: #f)
;;         (size: #f))))
;;
;;what's the scenario on the stack?
;;
;;Right before the first function call the stack is empty:
;;
;;           high memory
;;   |                          |
;;   |--------------------------|
;;   |     ik_stack_overflow    | <-- FPR
;;   |--------------------------|
;;   |                          |
;;           low memory
;;
;;For the non-tail call to G we prepare the Scheme stack layout:
;;
;;           high memory
;;   |                          |
;;   |--------------------------|
;;   |     ik_stack_overflow    | <-- FPR
;;   |--------------------------|               --
;;   |       empty word         | <- fvar.1     . stack frame of the call to G
;;   |--------------------------|               --
;;   | stack operand = fixnum 3 | <- fvar.2
;;   |--------------------------|
;;   |                          |
;;           low memory
;;
;;where the empty word  at FVAR.1 will be filled by the  return address.  Right after
;;the call to G has returned, the return value is moved from AAR on the stack:
;;
;;           high memory
;;   |                          |
;;   |--------------------------|
;;   |     ik_stack_overflow    | <-- FPR
;;   |--------------------------|
;;   |       empty word         | <- fvar.1
;;   |--------------------------|
;;   |      G return value      | <- fvar.2
;;   |--------------------------|
;;   |                          |
;;           low memory
;;
;;For the non-tail call to H we prepare the Scheme stack layout:
;;
;;           high memory
;;   |                          |
;;   |--------------------------|
;;   |     ik_stack_overflow    | <-- FPR
;;   |--------------------------|               --
;;   |       empty word         | <- fvar.1     .
;;   |--------------------------|               .
;;   |      G return value      | <- fvar.2     . stack frame of the call to H
;;   |--------------------------|               .
;;   |       empty word         | <- fvar.3     .
;;   |--------------------------|               --
;;   | stack operand = fixnum 4 | <- fvar.4
;;   |--------------------------|
;;   |                          |
;;           low memory
;;
;;where the empty word at FVAR.3 will be filled by the return address; the empty word
;;at FVAR.1 is left empty and untouched.  Right after the call to H has returned, the
;;return value is moved from AAR on the stack:
;;
;;           high memory
;;   |                          |
;;   |--------------------------|
;;   |     ik_stack_overflow    | <-- FPR
;;   |--------------------------|               --
;;   |       empty word         | <- fvar.1     . stack frame of the call to F
;;   |--------------------------|               --
;;   |      G return value      | <- fvar.2
;;   |--------------------------|
;;   |      H return value      | <- fvar.3
;;   |--------------------------|
;;   |                          |
;;           low memory
;;
;;and we are ready to  call F; the empty word at FVAR.1 will  be filled by the return
;;address.
;;
;;So we see that  the stack frame of the inner function call  to H includes the empty
;;word that will  be used to store the return  address in the call to F.   So, at the
;;time we perform  the inner function call to  H: some of the stack  locations in the
;;stack frame hold "live" objects, while others are unused.  The livemask in the call
;;table has the purpose of tracing which  is which and to allow the garbage collector
;;to do the right thing.
;;


;;;; data structures

(define-struct non-tail-call-frame-sets
  ;;Structs of this type are stored in the LIVE field of NON-TAIL-CALL-FRAME structs.
  ;;
  (vs fs ns))


(module INTEGER-SET
  ;;This module  implements sets of  bits; each set is  a nested hierarchy  of lists,
  ;;pairs and fixnums  interpreted as a tree; fixnums are  interpreted as bitvectors.
  ;;The empty set is the fixnum zero.
  ;;
  ;;To search for  a bit: we compute a  "bit index", then start from the  root of the
  ;;tree and: if  the index is even we go  left (the car), if the index  is odd we go
  ;;right (the cdr).
  ;;
  ;;This module has the same API of the module LISTY-SET.
  ;;
  (make-empty-set
   singleton
   set-member?		empty-set?
   set-add		set-rem
   set-difference	set-union
   set->list		list->set)

;;; --------------------------------------------------------------------

  (define-inline-constant BITS
    28)

  (define-syntax-rule (make-empty-set)
    0)

  (define-syntax-rule ($index-of N)
    ;;Given a set element N to be added  to, or searched into, a set: return a fixnum
    ;;representing the "index" of the fixnum in which N should be stored.
    ;;
    (fxquotient N BITS))

  (define ($mask-of n)
    ;;Given a set element N to be added  to, or searched into, a set: return a fixnum
    ;;representing the bitmask of N for the fixnum in which N should be stored.
    ;;
    (fxsll 1 (fxremainder n BITS)))

  (define (singleton N)
    ;;Return a set containing only N.
    ;;
    (set-add N (make-empty-set)))

;;; --------------------------------------------------------------------

  (define (empty-set? S)
    (eqv? S 0))

  (define* (set-member? {N fixnum?} SET)
    (let loop ((SET SET)
	       (idx ($index-of N))
	       (msk ($mask-of  N))) ;this never changes in the loop
      (cond ((pair? SET)
	     (if (fxeven? idx)
		 (loop (car SET) (fxsra idx 1) msk)
	       (loop (cdr SET) (fxsra idx 1) msk)))
	    ((fxzero? idx)
	     (fx=? msk (fxand SET msk)))
	    (else #f))))

  (define* (set-add {N fixnum?} SET)
    (let recur ((SET SET)
		(idx ($index-of N))
		(msk ($mask-of  N))) ;this never changes in the loop
      (cond ((pair? SET)
	     (if (fxeven? idx)
		 (let* ((a0 (car SET))
			(a1 (recur a0 (fxsra idx 1) msk)))
		   (if (eq? a0 a1)
		       SET
		     (cons a1 (cdr SET))))
	       (let* ((d0 (cdr SET))
		      (d1 (recur d0 (fxsra idx 1) msk)))
		 (if (eq? d0 d1)
		     SET
		   (cons (car SET) d1)))))
	    ((fxzero? idx)
	     (fxior SET msk))
	    (else
	     (if (fxeven? idx)
		 (cons (recur SET (fxsra idx 1) msk) 0)
	       (cons SET (recur 0 (fxsra idx 1) msk)))))))

  (define (cons^ A D)
    (if (and (eq? D 0)
	     (fixnum? A))
        A
      (cons A D)))

  (define* (set-rem {N fixnum?} SET)
    (let recur ((SET SET)
		(idx ($index-of N))
		(msk ($mask-of  N)))	;this never changes in the loop
      (cond ((pair? SET)
	     (if (fxeven? idx)
		 (let* ((a0 (car SET))
			(a1 (recur a0 (fxsra idx 1) msk)))
		   (if (eq? a0 a1)
		       SET
		     (cons^ a1 (cdr SET))))
	       (let* ((d0 (cdr SET))
		      (d1 (recur d0 (fxsra idx 1) msk)))
		 (if (eq? d0 d1)
		     SET
		   (cons^ (car SET) d1)))))
	    ((fxzero? idx)
	     (fxand SET (fxnot msk)))
	    (else
	     SET))))

  (module (set-union)

    (define (set-union S1 S2)
      (if (pair? S1)
	  (if (pair? S2)
	      (if (eq? S1 S2)
		  S1
		(cons (set-union (car S1) (car S2))
		      (set-union (cdr S1) (cdr S2))))
	    (let ((a0 (car S1)))
	      (let ((a1 (set-union^ a0 S2)))
		(if (eq? a0 a1) S1 (cons a1 (cdr S1))))))
	(if (pair? S2)
	    (let ((a0 (car S2)))
	      (let ((a1 (set-union^ a0 S1)))
		(if (eq? a0 a1) S2 (cons a1 (cdr S2)))))
	  (fxior S1 S2))))

    (define (set-union^ S1 M2)
      (if (pair? S1)
	  (let* ((a0 (car S1))
		 (a1 (set-union^ a0 M2)))
	    (if (eq? a0 a1)
		S1
	      (cons a1 (cdr S1))))
	(fxior S1 M2)))

    #| end of module: set-union |# )

  (module (set-difference)

    (define (set-difference s1 s2)
      (cond ((pair? s1)
	     (if (pair? s2)
		 (if (eq? s1 s2)
		     0
		   (cons^ (set-difference (car s1) (car s2))
			  (set-difference (cdr s1) (cdr s2))))
	       (let* ((a0 (car s1))
		      (a1 (set-difference^ a0 s2)))
		 (if (eq? a0 a1)
		     s1
		   (cons^ a1 (cdr s1))))))
	    ((pair? s2)
	     (set-difference^^ s1 (car s2)))
	    (else
	     (fxand s1 (fxnot s2)))))

    (define (set-difference^ S1 M2)
      (if (pair? S1)
	  (let* ((a0 (car S1))
		 (a1 (set-difference^ a0 M2)))
	    (if (eq? a0 a1)
		S1
	      (cons^ a1 (cdr S1))))
	(fxand S1 (fxnot M2))))

    (define (set-difference^^ M1 S2)
      (if (pair? S2)
	  (set-difference^^ M1 (car S2))
	(fxand M1 (fxnot S2))))

    #| end of module: set-difference |# )

  (module (list->set)

    (define* (list->set {ls list-of-fixnums?})
      (let recur ((ls ls)
		  (S  0))
	(if (pair? ls)
	    (recur (cdr ls) (set-add (car ls) S))
	  S)))

    (define (list-of-fixnums? obj)
      (and (list? obj)
	   (for-all fixnum? obj)))

    #| end of module: list->set |# )

  (define (set->list S)
    (let outer ((i  0)
		(j  1)
		(S  S)
		(ac '()))
      (if (pair? S)
	  (outer i (fxsll j 1) (car S)
		 (outer (fxior i j) (fxsll j 1) (cdr S) ac))
	(let inner ((i  (fx* i BITS))
		    (m  S)
		    (ac ac))
	  (if (fxeven? m)
	      (if (fxzero? m)
		  ac
		(inner (fxadd1 i) (fxsra m 1) ac))
	    (inner (fxadd1 i) (fxsra m 1) (cons i ac)))))))

  #| end of module: INTEGER-SET |# )


(module FRAME-CONFLICT-SETS
  (init-var*!
   init-nfv!
   empty-var-set rem-var add-var union-vars mem-var? for-each-var
   empty-nfv-set rem-nfv add-nfv union-nfvs mem-nfv? for-each-nfv
   empty-frm-set rem-frm add-frm union-frms mem-frm?
   empty-reg-set rem-reg add-reg union-regs mem-reg?

   add-interference-edge!/var->fvar
   add-interference-edge!/var->reg
   add-interference-edge!/var->var

   add-interference-edge!/nfv->fvar
   add-interference-edge!/nfv->nfv
   add-interference-edge!/nfv->var

   merge-interference-edges!/var->fvar
   merge-interference-edges!/var->reg
   merge-interference-edges!/var->var
   merge-interference-edges!/nfv->fvar
   merge-interference-edges!/nfv->var)
  (import INTEGER-SET)
  (module (register?)
    (import INTEL-ASSEMBLY-CODE-GENERATION))

  (module (init-var*!)

    (case-define init-var*!
      ((ls)
       (init-var*! ls 0))
      ((ls idx)
       (when (pair? ls)
	 (init-var!  (car ls) idx)
	 (init-var*! (cdr ls) (fxadd1 idx)))))

    (define (init-var! x i)
      ($set-var-index! x i)
      ;;Sets representing preference  edges.  If a struct is added  to these sets: it
      ;;means it is connected to all the structs already in the set.
      ($set-var-var-move! x (empty-var-set))
      ($set-var-reg-move! x (empty-reg-set))
      ($set-var-frm-move! x (empty-frm-set))
      ;;Sets representing interference edges.  If a struct is added to these sets: it
      ;;means it is connected to all the structs already in the set.
      ($set-var-var-conf! x (empty-var-set))
      ($set-var-reg-conf! x (empty-reg-set))
      ($set-var-frm-conf! x (empty-frm-set)))

    #| end of module |# )

  (define (init-nfv! x)
    ;;Sets representing  preference edges.  If  a struct is  added to these  sets: it
    ;;means it is connected to all the structs already in the set.
    ($set-nfv-frm-conf! x (empty-frm-set))
    ($set-nfv-nfv-conf! x (empty-nfv-set))
		;We set  this field to  an empty set,  even though, at  present, this
		;field is unused.
    ($set-nfv-var-conf! x (empty-var-set)))

;;; --------------------------------------------------------------------
;;; temporary locations, VAR structs

  (define-syntax-rule (empty-var-set)
    ;;Build and return a new, empty VS set.
    ;;
    (make-empty-set))

  (define (add-var x vs)
    ;;Add the VAR struct X to the set VS.  Return the new set.
    ;;
    (set-add (var-index x) vs))

  (define (rem-var x vs)
    ;;Remove the VAR struct X from the set VS.  Return the new set.
    ;;
    (set-rem (var-index x) vs))

  (define (mem-var? x vs)
    ;;Return true if the VAR struct X is a member of te set VS.
    ;;
    (set-member? (var-index x) vs))

  (define-syntax-rule (union-vars ?vs1 ?vs2)
    ;;Build  and return  a new  VS set  holding  all the  members of  ?VS1 and  ?VS2;
    ;;duplicate members are included only once.
    ;;
    (set-union ?vs1 ?vs2))

  (define (for-each-var vs locals.vars func)
    ;;Apply FUNC to every VAR struct in the set VS; return unspecified values.
    ;;
    ;;The argument  LOCALS.VARS is meant to  be a vector of  VAR structs representing
    ;;the local variables in  a LOCALS struct's body.  The slot index  of each VAR in
    ;;LOCALS.VARS equals the value of the INDEX of the VAR struct itself.
    ;;
    ;;The set VS contains (encoded) only the indexes of the VAR structs in the vector
    ;;LOCALS.VARS: this  function exists to  allow the  iteration over VS  by mapping
    ;;indexes to VAR structs.
    ;;
    (for-each (lambda (i)
		(func (vector-ref locals.vars i)))
      (set->list vs)))

;;; --------------------------------------------------------------------
;;; current frame stack operands, FVAR structs

  (define-syntax-rule (empty-frm-set)
    ;;Build and return a new, empty FS set.
    ;;
    (make-empty-set))

  (define (add-frm x fs)
    ;;Add the FVAR struct X to the set FS.  Return the new set.
    ;;
    (set-add (fvar-idx x) fs))

  (define (rem-frm x fs)
    ;;Remove the FVAR struct X from the set FS.  Return the new set.
    ;;
    (set-rem (fvar-idx x) fs))

  (define (mem-frm? x fs)
    ;;Return true if the FVAR struct X is a member of the set FS.
    ;;
    (set-member? (fvar-idx x) fs))

  (define-syntax-rule (union-frms ?fs1 ?fs2)
    ;;Build  and return  a new  FS set  holding  all the  members of  ?FS1 and  ?FS2;
    ;;duplicate members are included only once.
    ;;
    (set-union ?fs1 ?fs2))

;;; --------------------------------------------------------------------
;;; CPU registers, REG symbols

  (define-syntax-rule (empty-reg-set)
    ;;Build and return a new, empty RS set.
    ;;
    (make-empty-set))

  (define (add-reg x rs)
    ;;Add the CPU register symbol name X to the set RS.  Return the new set.
    ;;
    (module (%cpu-register-name->index)
      (import INTEL-ASSEMBLY-CODE-GENERATION))
    (set-add (%cpu-register-name->index x) rs))

  (define (rem-reg x rs)
    ;;Remove the CPU register symbol name X from the set RS.  Return the new set.
    ;;
    (module (%cpu-register-name->index)
      (import INTEL-ASSEMBLY-CODE-GENERATION))
    (set-rem (%cpu-register-name->index x) rs))

  (define (mem-reg? x rs)
    ;;Return true if the CPU register symbol name X is a member of the set RS.
    ;;
    (module (%cpu-register-name->index)
      (import INTEL-ASSEMBLY-CODE-GENERATION))
    (set-member? (%cpu-register-name->index x) rs))

  (define-syntax-rule (union-regs ?rs1 ?rs2)
    ;;Build  and return  a new  RS set  holding  all the  members of  ?RS1 and  ?RS2;
    ;;duplicate members are included only once.
    ;;
    (set-union ?rs1 ?rs2))

;;; --------------------------------------------------------------------
;;; next frame stack operands, NFV structs

  (define-syntax-rule (empty-nfv-set)
    ;;Build and return a new, empty NS set.
    ;;
    '())

  (define (add-nfv x ns)
    ;;Add the NFV struct X to the set NS.  Return the new set.
    ;;
    (if (memq x ns)
	ns
      (cons x ns)))

  (define-syntax-rule (rem-nfv ?x ?ns)
    ;;Remove the NFV struct ?X from the set ?NS.  Return the new set.
    ;;
    (remq1 ?x ?ns))

  (define-syntax-rule (mem-nfv? ?x ?ns)
    ;;Return true if the NFV struct ?X is a member of the set ?NS.
    ;;
    (memq ?x ?ns))

  (define (union-nfvs ns1 ns2)
    ;;Build and return a new NS set holding all the members of NS1 and NS2; duplicate
    ;;members are included only once.
    ;;
    (let recur ((ns1 ns1)
		(ns2 ns2))
      (cond ((null? ns1)
	     ns2)
	    ((memq (car ns1) ns2)
	     (recur (cdr ns1) ns2))
	    (else
	     (cons (car ns1)
		   (recur (cdr ns1) ns2))))))

  (define-syntax-rule (for-each-nfv ?ns ?func)
    (for-each ?func ?ns))

;;; --------------------------------------------------------------------
;;; interference edges

  (define* (add-interference-edge!/var->reg  {src.var var?} {dst.reg register?})
    ($set-var-reg-conf! src.var (add-reg dst.reg  ($var-reg-conf src.var))))

  (define* (add-interference-edge!/var->fvar {src.var var?} {dst.fvar fvar?})
    ($set-var-frm-conf! src.var (add-frm dst.fvar ($var-frm-conf src.var))))

  (define* (add-interference-edge!/var->var  {src.var var?} {dst.var var?})
    ($set-var-var-conf! src.var (add-var dst.var  ($var-var-conf src.var))))

;;;

  (define* (add-interference-edge!/nfv->nfv  {src.nfv nfv?} {dst.nfv nfv?})
    ($set-nfv-nfv-conf! src.nfv (add-reg dst.nfv  ($nfv-nfv-conf src.nfv))))

  (define* (add-interference-edge!/nfv->fvar {src.nfv nfv?} {dst.fvar fvar?})
    ($set-nfv-frm-conf! src.nfv (add-frm dst.fvar ($nfv-frm-conf src.nfv))))

  (define* (add-interference-edge!/nfv->var  {src.nfv nfv?} {dst.var var?})
    ($set-nfv-var-conf! src.nfv (add-var dst.var  ($nfv-var-conf src.nfv))))

;;;

  (define* (merge-interference-edges!/var->fvar {src.var var?} set.fvar)
    ($set-var-frm-conf! src.var (union-frms set.fvar ($var-frm-conf src.var))))

  (define* (merge-interference-edges!/var->reg  {src.var var?} set.reg)
    ($set-var-reg-conf! src.var (union-regs set.reg  ($var-reg-conf src.var))))

  (define* (merge-interference-edges!/var->var  {src.var var?} set.var)
    ($set-var-var-conf! src.var (union-vars set.var  ($var-var-conf src.var))))

  (define* (merge-interference-edges!/nfv->var  {src.nfv nfv?} set.var)
    ($set-nfv-var-conf! src.nfv (union-vars set.var  ($nfv-var-conf src.nfv))))

  (define* (merge-interference-edges!/nfv->fvar {src.nfv nfv?} set.fvar)
    ($set-nfv-frm-conf! src.nfv (union-frms set.fvar ($nfv-frm-conf src.nfv))))

  #| end of module: FRAME-CONFLICT-SETS |# )


(module (E-codes)

  (define (E-codes x)
    (struct-case x
      ((codes clam* body)
       (make-codes (map E-clambda clam*) (E-locals body)))))

  (define (E-clambda x)
    (struct-case x
      ((clambda label clause* cp freevar* name)
       (make-clambda label (map E-clambda-clause clause*) cp freevar* name))))

  (define (E-clambda-clause x)
    (struct-case x
      ((clambda-case info body)
       (make-clambda-case info (E-locals body)))))

  (define* (E-locals x)
    ;;X must  be a struct instance  of type LOCALS.  Update  the field VARS of  X and
    ;;return a new struct instance of type LOCALS which is meant to replace X.
    ;;
    (module (init-var*!)
      (import FRAME-CONFLICT-SETS))
    (struct-case x
      ((locals x.vars x.body)
       ;;X.VARS is  a possibly empty  proper list  of VAR structs  representing local
       ;;variables  used in  the code  represented by  X.BODY.  Such  local variables
       ;;represent  machine-word storage  locations  that must  be  allocated to  CPU
       ;;registers or Scheme stack words.
       (init-var*! x.vars)
       (let* ((x.vars.vec        (list->vector x.vars))
	      (spill-set         (%uncover-frame-conflicts x.body x.vars.vec))
	      (x.vars.spillable* (%discard-vars-being-stack-operands x.vars))
	      (x.body^           (%rewrite x.body x.vars.vec)))
	 ;;X.VARS.VEC is a vector of VAR structs representing all the local variables
	 ;;in X.BODY.   Some of  these VAR structs  have a FVAR  struct in  their LOC
	 ;;field: they have been allocated to  stack locations; the other VAR structs
	 ;;have #f in their LOC field: they are not yet allocated.
	 ;;
	 ;;X.VARS.SPILLABLE* is a list of VAR  structs representing the subset of VAR
	 ;;structs in X.VARS.VEC that have #f  in their LOC field.  These VAR structs
	 ;;can be allocated to CPU registers or to stack locations (spilled).
	 (make-locals (cons x.vars.vec x.vars.spillable*) x.body^)))
      (else
       (compiler-internal-error __module_who__ __who__
	 "expected LOCALS struct as body form" x))))

  (define (%discard-vars-being-stack-operands x.vars)
    ;;Tail-recursive function.  Given a list of  struct instances of type VAR, return
    ;;a new list containing only those having #f in the LOC field.
    ;;
    ;;The VAR  with a non-false LOC  fields have a  FVAR struct in it,  and represent
    ;;stack operands in closure object bodies.
    ;;
    (if (pair? x.vars)
	(if ($var-loc (car x.vars))
	    (begin
	      #;(assert (fvar? (var-loc (car x.vars))))
	      (%discard-vars-being-stack-operands (cdr x.vars)))
	  (cons (car x.vars) (%discard-vars-being-stack-operands (cdr x.vars))))
      '()))

  #| end of module: E-codes |# )


(define (%uncover-frame-conflicts locals.body locals.vars)
  ;;
  ;;The argument  LOCALS.BODY is the  body of a LOCALS  struct; the LOCALS  struct is
  ;;either the body of a CLAMBDA clause or the init expression of a CODES struct.
  ;;
  ;;The  argument LOCALS.VARS  is  a vector  of VAR  structs  representing the  local
  ;;variables in LOCALS.BODY.   The slot index of each VAR  in LOCALS.VARS equals the
  ;;value of the INDEX of the VAR struct itself.
  ;;
  ;;Throughout this function the arguments VS, RS,  FS, NS are sets as defined by the
  ;;module FRAME-CONFLICT-SETS (they are not all instances of the same type):
  ;;
  ;;VS -  A collection of VAR  structs; it is always  a non-strict subset of  the VAR
  ;;     structs listed in LOCALS.VARS.
  ;;
  ;;RS -  A collection  of register  name symbols, REGs  for short.   It is  always a
  ;;     non-strict subset of: AAR, APR, CPR, FPR, PCR, %ecx, %edx.
  ;;
  ;;FS - A collection of FVAR structs (current stack frame operands).  It is always a
  ;;     non-strict subset of the FVAR structs  listed in the ARGS field of CASE-INFO
  ;;     structs.
  ;;
  ;;NS -  A collection of NFV  structs (next stack  frame operands).  It is  always a
  ;;      non-strict  subset  of  the  NFV  structs listed  in  the  RAND*  field  of
  ;;     NON-TAIL-CALL-FRAME structs.
  ;;
  ;;We know that, after  being processed by the previous compiler  pass, the body has
  ;;as last form of every branch a struct like:
  ;;
  ;;   (seq
  ;;     (asm-instr move (AAR ?result))
  ;;     (asmcall return (AAR APR FPR PCR)))
  ;;
  ;;or a function tail-call:
  ;;
  ;;   (asmcall direct-jump   (?target AAR APR CPR FPR PCR . rand*.fvar))
  ;;   (asmcall indirect-jump         (AAR APR CPR FPR PCR . rand*.fvar))
  ;;
  ;;This module begins its  work by applying T to LOCALS.BODY.   The function T first
  ;;processes subforms of BODY in tail position with a depth-first visit, recursively
  ;;applying itself to such forms; until, in the true tail position a struct among:
  ;;
  ;;   (asmcall direct-jump   (?target AAR APR CPR FPR PCR . rand*.fvar))
  ;;   (asmcall indirect-jump         (AAR APR CPR FPR PCR . rand*.fvar))
  ;;   (asmcall return                (AAR APR     FPR PCR))
  ;;
  ;;is found.  Then, while rewinding, this function  applies the functions E and P to
  ;;the  non-tail subforms  of X.   When  an ASMCALL  struct  with one  of the  above
  ;;operators is found new, empty sets VS, RS,  FS, NS are created and used as return
  ;;values while the nested function calls rewind.
  ;;
  ;;VS, RS,  FS, NS are filled  while rewinding the  visit and, at any  instant, they
  ;;represent  the sets  of VARs,  registers, FVARs  and NFVs  that are  used in  the
  ;;continuation of the current subexpression.
  ;;
  ;;The true work is done in the functions "R" and "E-asm-instr".
  ;;
  ;;NOTE Whenever a  SHORTCUT is processed: first the interrupt  handler is processed
  ;;and the resulting VS,  RS, FS, NS are stored (as vector  object) in the parameter
  ;;EXCEPTION-LIVE-SET; then the body is processed, in the dynamic environment having
  ;;the parameter set.
  ;;
  ;;NOTE A  lot of  functions are  nested here because  they need  to close  upon the
  ;;argument LOCALS.VARS.
  ;;
  (import INTEGER-SET)
  (import FRAME-CONFLICT-SETS)
  (module (register?
	   eax ecx edx
	   AA-REGISTER CP-REGISTER AP-REGISTER FP-REGISTER PC-REGISTER)
    (import INTEL-ASSEMBLY-CODE-GENERATION))

  (define spill-set
    ;;Whenever, at some point in the LOCALS.BODY, we perform a non-tail call: all the
    ;;temporary locations (VS) active right before  the a non-tail call must be saved
    ;;on the stack and restored before calling and restored right after the return.
    ;;
    ;;Such locations are collected in this set.
    (make-empty-set))

  (define exception-live-set
    (make-parameter #f))

  (define (main body)
    ;;Return the spill set.  The VS, RS, FS, NS gathered up to here are discarded.
    ;;
    #;(debug-print* 'locals.body (unparse-recordised-code/sexp body))
    (receive (vs rs fs ns)
	(T body)
      #;(debug-print* 'VARs vs 'REGs rs 'FVARs fs 'NFVs  ns)
      (void))
    #;(debug-print* 'spill-set spill-set)
    spill-set)

;;; --------------------------------------------------------------------

  (define* (R x vs rs fs ns)
    ;;Recursive function, tail and non-tail.  The argument X can be one among:
    ;;
    ;;* An operand among RAND* in a ASMCALL struct.
    ;;
    ;;* An operand among DST and SRC in a ASM-INSTR struct.
    ;;
    ;;* An operand among ARGS in a NON-TAIL-CALL struct.
    ;;
    ;;If  the argument  X  is a  VAR,  REG, FVAR,  NFV  struct: it  is  added to  the
    ;;appropriate set among VS, RS, FS, NS.
    ;;
    ;;Return  4  values being  the  sets  VS, RS,  FS,  NS  updated with  information
    ;;representing the operand in X.
    ;;
    (if (register? x)
	;;X is a symbol representing the name of a CPU register.
	(begin
	  #;(assert (memq x (list AA-REGISTER CP-REGISTER AP-REGISTER FP-REGISTER PC-REGISTER ecx edx)))
	  (values vs (add-reg x rs) fs ns))
      (struct-case x
	((fvar)
	 (values vs rs (add-frm x fs) ns))
	((var)
	 (values (add-var x vs) rs fs ns))
	((nfv)
	 (values vs rs fs (add-nfv x ns)))
	((disp objref offset)
	 (receive (vs rs fs ns)
	     (R objref vs rs fs ns)
	   (R offset vs rs fs ns)))
	((constant)
	 (values vs rs fs ns))
	((code-loc)
	 (values vs rs fs ns))
	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid recordised code processed by R"
	   (unparse-recordised-code/sexp x))))))

  (define (R* ls vs rs fs ns)
    ;;Recursive function,  tail and non-tail.   Apply R to every  item in LS  and the
    ;;other arguments.  Return the final VS, RS, FS, NS updated sets.
    ;;
    (if (pair? ls)
	(receive (vs rs fs ns)
	    (R (car ls) vs rs fs ns)
	  (R* (cdr ls) vs rs fs ns))
      (values vs rs fs ns)))

;;; --------------------------------------------------------------------

  (define* (T x)
    ;;Process the  recordised code X  as a form in  tail position.  In  tail position
    ;;there can be only structs of  type: SEQ, CONDITIONAL, SHORTCUT and ASMCALL with
    ;;operator among: RETURN, INDIRECT-JUMP, DIRECT-JUMP.
    ;;
    ;;Return 4 values being the sets VS, RS, FS, NS updated with information gathered
    ;;in X.
    ;;
    (struct-case x
      ((seq e0 e1)
       (receive (vs rs fs ns)
	   (T e1)
         (E e0 vs rs fs ns)))

      ((conditional test conseq altern)
       (let-values
	   (((vs.conseq rs.conseq fs.conseq ns.conseq) (T conseq))
	    ((vs.altern rs.altern fs.altern ns.altern) (T altern)))
         (P test
            vs.conseq rs.conseq fs.conseq ns.conseq
            vs.altern rs.altern fs.altern ns.altern
            (union-vars vs.conseq vs.altern)
            (union-regs rs.conseq rs.altern)
            (union-frms fs.conseq fs.altern)
            (union-nfvs ns.conseq ns.altern))))

      ((asmcall rator rand*)
       (case rator
         ((return indirect-jump direct-jump)
	  ;;This is a form in tail position in the original input body.  We expect it
	  ;;to be one among:
	  ;;
	  ;;   (asmcall direct-jump   (?target AAR APR CPR FPR PCR . rand*.fvar))
	  ;;   (asmcall indirect-jump         (AAR APR CPR FPR PCR . rand*.fvar))
	  ;;   (asmcall return                (AAR APR     FPR PCR))
	  ;;
          (R* rand*
	      (empty-var-set)
              (empty-reg-set)
              (empty-frm-set)
              (empty-nfv-set)))
         (else
	  (compiler-internal-error __module_who__ __who__
	    "invalid ASMCALL operator in tail position"
	    (unparse-recordized-code/sexp x)))))

      ((shortcut body handler)
       (receive (vs.handler rs.handler fs.handler ns.handler)
	   (T handler)
	 (parameterize
	     ((exception-live-set (vector vs.handler rs.handler fs.handler ns.handler)))
	   (T body))))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid tail"
	 (unparse-recordized-code/sexp x)))))

;;; --------------------------------------------------------------------

  (define* (P x
	      vs.conseq rs.conseq fs.conseq ns.conseq
	      vs.altern rs.altern fs.altern ns.altern
	      vs.union  rs.union  fs.union  ns.union)
    ;;Process  the recordised  code  X as  a  form in  predicate  position.  In  tail
    ;;position  there  can be  only  structs  of  type: SEQ,  CONDITIONAL,  SHORTCUT,
    ;;CONSTANT and ASM-INSTR with operator among: RETURN, INDIRECT-JUMP, DIRECT-JUMP.
    ;;
    ;;Return 4 values being the sets VS, RS, FS, NS updated with information gathered
    ;;in X.
    ;;
    (struct-case x
      ((seq e0 e1)
       (receive (vs rs fs ns)
	   (P e1
	      vs.conseq rs.conseq fs.conseq ns.conseq
	      vs.altern rs.altern fs.altern ns.altern
	      vs.union  rs.union  fs.union  ns.union)
         (E e0 vs rs fs ns)))

      ((conditional e0 e1 e2)
       (let-values
	   (((vs1 rs1 fs1 ns1)
	     (P e1
		vs.conseq rs.conseq fs.conseq ns.conseq
		vs.altern rs.altern fs.altern ns.altern
		vs.union  rs.union  fs.union  ns.union))
	    ((vs2 rs2 fs2 ns2)
	     (P e2
		vs.conseq rs.conseq fs.conseq ns.conseq
		vs.altern rs.altern fs.altern ns.altern
		vs.union  rs.union  fs.union  ns.union)))
         (P e0
            vs1 rs1 fs1 ns1
            vs2 rs2 fs2 ns2
            (union-vars vs1 vs2)
            (union-regs rs1 rs2)
            (union-frms fs1 fs2)
            (union-nfvs ns1 ns2))))

      ((constant x.const)
       (if x.const
           (values vs.conseq rs.conseq fs.conseq ns.conseq)
	 (values vs.altern rs.altern fs.altern ns.altern)))

      ((asm-instr op dst src)
       (R* (list dst src) vs.union  rs.union  fs.union  ns.union))

      ((shortcut body handler)
       (receive (vs.handler rs.handler fs.handler ns.handler)
	   (P handler
	      vs.conseq rs.conseq fs.conseq ns.conseq
	      vs.altern rs.altern fs.altern ns.altern
	      vs.union  rs.union  fs.union  ns.union)
	 (parameterize ((exception-live-set (vector vs.handler rs.handler fs.handler ns.handler)))
	   (P body
	      vs.conseq rs.conseq fs.conseq ns.conseq
	      vs.altern rs.altern fs.altern ns.altern
	      vs.union  rs.union  fs.union  ns.union))))

      (else
       (compiler-internal-error __module_who__ __who__
				"invalid pred" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define* (E x vs rs fs ns)
    ;;Process the recordised code X as a form in side effects position.
    ;;
    ;;Return 4 values being the sets VS, RS, FS, NS updated with information gathered
    ;;in X.
    ;;
    (struct-case x

      ((seq e0 e1)
       (receive (vs rs fs ns)
	   (E e1 vs rs fs ns)
         (E e0 vs rs fs ns)))

      ((conditional test conseq altern)
       (let-values
	   (((vs.conseq rs.conseq fs.conseq ns.conseq)  (E conseq vs rs fs ns))
	    ((vs.altern rs.altern fs.altern ns.altern)  (E altern vs rs fs ns)))
         (P test
            vs.conseq rs.conseq fs.conseq ns.conseq
            vs.altern rs.altern fs.altern ns.altern
            (union-vars vs.conseq vs.altern)
            (union-regs rs.conseq rs.altern)
            (union-frms fs.conseq fs.altern)
            (union-nfvs ns.conseq ns.altern))))

      ((asm-instr op dst src)
       (E-asm-instr x op dst src vs rs fs ns))

      ((non-tail-call target.unused value.unused all-rand*)
       ;;A non-tail call is not a "true" tail form, but it both consumes and produces
       ;;the values  in the  register operands;  it also consumes  values in  the NFV
       ;;structs.
       ;;
       ;;All  the temporary  location VAR  structs  in VS,  alive right  befor the  a
       ;;non-tail call, must be saved on  the stack before calling and restored right
       ;;after the return.
       (set! spill-set (union-vars vs spill-set))
       ;;Set to #t  the LOC field of every  VAR struct which is a member  of VS; this
       ;;way we signal that this VAR is alive right before the non-tail call.
       (for-each-var
	   vs locals.vars
	 (lambda (x)
	   ($set-var-loc! x #t)))
       ;;We expect ALL-RAND* to be:
       ;;
       ;;  (AAR APR CPR FPR PCR . rand*.nfv)
       ;;
       (R* all-rand* vs (empty-reg-set) fs ns))

      ((non-tail-call-frame nfv* live body)
       (for-each init-nfv! nfv*)
       (set-non-tail-call-frame-live! x (make-non-tail-call-frame-sets vs fs ns))
       (E body vs rs fs ns))

      ((asmcall op)
       (case op
         ((nop fl:double->single fl:single->double)
	  (values vs rs fs ns))
         ((interrupt incr/zero?)
          (let ((v (exception-live-set)))
            (if (vector? v)
		(values (vector-ref v 0)
			(vector-ref v 1)
			(vector-ref v 2)
			(vector-ref v 3))
              (compiler-internal-error __module_who__ __who__ "unbound exception2"))))
         (else
	  (compiler-internal-error __module_who__ __who__
	    "invalid ASMCALL operator in for effect form" op))))

      ((shortcut body handler)
       (receive (vs.handler rs.handler fs.handler ns.handler)
	   (E handler vs rs fs ns)
	 (parameterize
	     ((exception-live-set (vector vs.handler rs.handler fs.handler ns.handler)))
	   (E body vs rs fs ns))))

      (else
       (compiler-internal-error __module_who__ __who__ "invalid effect" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (module (E-asm-instr)

    (define* (E-asm-instr x op dst src vs rs fs ns)
      ;;Return  4 values  being the  sets  VS, RS,  FS, NS  updated with  information
      ;;representing the operands DST and SRC.
      ;;
      (case op
	((move bref mref32)
	 (E-asm-instr/move x op dst src vs rs fs ns))

	((int-/overflow int+/overflow int*/overflow)
	 (E-asm-instr/int-overflow x op dst src vs rs fs ns))

	((nop)
	 (values vs rs fs ns))

	((logand logor logxor sll sra srl int+ int- int* bswap! sll/overflow)
	 (E-asm-instr/bitwise x op dst src vs rs fs ns))

	((idiv)
	 ;;Here we know that DST is either  the register EAX or the register EDX; SRC
	 ;;is an operand, we do not know which  one here.  We know that CLTD and IDIV
	 ;;always come together.
	 #;(assert (or (eq? dst eax) (eq? dst edx)))
	 (mark-reg/vars-conf! eax vs)
	 (mark-reg/vars-conf! edx vs)
	 (R src vs (add-reg eax (add-reg edx rs)) fs ns))

	((cltd)
	 ;;Here we know that DST is the register EDX and SRC is the register EAX.  We
	 ;;know that CLTD and IDIV always come together.
	 #;(assert (eq? dst edx))
	 #;(assert (eq? src eax))
	 (mark-reg/vars-conf! edx vs)
	 (R src vs (rem-reg edx rs) fs ns))

	((mset mset32 bset
	       fl:load fl:store fl:add! fl:sub! fl:mul! fl:div! fl:from-int
	       fl:shuffle fl:load-single fl:store-single)
	 (R* (list src dst) vs rs fs ns))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid ASM-INSTR operator in recordised code for effect"
	   (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

    (define* (E-asm-instr/move x op dst src vs rs fs ns)
      ;;We expect the ASM-INSTR struct to have one of the formats:
      ;;
      ;;   (asm-instr move   ?reg ?src)
      ;;   (asm-instr move   ?var ?src)
      ;;   (asm-instr move   ?nfv ?src)
      ;;   (asm-instr bref  ?var (disp ?objref ?offset))
      ;;   (asm-instr bref  ?nfv (disp ?objref ?offset))
      ;;   (asm-instr mref32 ?var (disp ?objref ?offset))
      ;;   (asm-instr mref32 ?nfv (disp ?objref ?offset))
      ;;
      ;;Return  4 values  being the  sets  VS, RS,  FS, NS  updated with  information
      ;;representing the operands DST and SRC.
      ;;
      (cond ((register? dst)
	     (E-asm-instr/move/reg-dst  x op dst src vs rs fs ns))
	    ((fvar? dst)
	     (E-asm-instr/move/fvar-dst x op dst src vs rs fs ns))
	    ((var? dst)
	     (E-asm-instr/move/var-dst  x op dst src vs rs fs ns))
	    ((nfv? dst)
	     (E-asm-instr/move/nfv-dst  x op dst src vs rs fs ns))
	    (else
	     (compiler-internal-error __module_who__ __who__
	       "invalid d" dst))))

    (define* (E-asm-instr/move/reg-dst x op dst src vs rs fs ns)
      (cond ((not (mem-reg? dst rs))
	     ;;If  a REG  is  the destination:  it  must  be that  such  REG is  used
	     ;;somewhere in the continuation, so the  REG must already be a member of
	     ;;RS.   If it  is not:  we  have made  a mistake  while generating  this
	     ;;ASM-INSTR struct.
	     ;;
	     ;;NOTE  For reasons  unknown  to me,  in the  original  Ikarus code  the
	     ;;ASM-INSTR was transformed  in a NOP, as below.  (Marco  Maggi; Wed Oct
	     ;;8, 2014)
	     #;(set-asm-instr-op! x 'nop)
	     #;(values vs rs fs ns)
	     (compiler-internal-error __module_who__ __who__
	       "register name used as destination operand in Assembly instruction, \
                but never consumed in the continuation"
	       (unparse-recordised-code/sexp x)))
	    ((or (constant? src)
		 (code-loc? src)
		 (disp?     src)
		 (register? src))
	     (let ((rs (rem-reg dst rs)))
	       (mark-reg/vars-conf! dst vs)
	       (R src vs rs fs ns)))
	    ((var? src)
	     (let ((rs (rem-reg dst rs))
		   (vs (rem-var src vs)))
	       (mark-var/reg-move! src dst)
	       (mark-reg/vars-conf! dst vs)
	       (values (add-var src vs) rs fs ns)))
	    ((fvar? src)
	     (let ((rs (rem-reg dst rs)))
	       (mark-reg/vars-conf! dst vs)
	       (values vs rs (add-frm src fs) ns)))
	    (else
	     (compiler-internal-error __module_who__ __who__
	       "invalid rs"
	       (unparse-recordized-code x)))))

    (define* (E-asm-instr/move/fvar-dst x op dst src vs rs fs ns)
      (cond ((not (mem-frm? dst fs))
	     (set-asm-instr-op! x 'nop)
	     (values vs rs fs ns))
	    ((or (constant? src)
		 (code-loc? src)
		 (disp?     src)
		 (register? src))
	     (let ((fs (rem-frm dst fs)))
	       (mark-frm/vars-conf! dst vs)
	       (mark-frm/nfvs-conf! dst ns)
	       (R src vs rs fs ns)))
	    ((var? src)
	     (let ((fs (rem-frm dst fs))
		   (vs (rem-var src vs)))
	       (mark-var/frm-move! src dst)
	       (mark-frm/vars-conf! dst vs)
	       (mark-frm/nfvs-conf! dst ns)
	       (values (add-var src vs) rs fs ns)))
	    (else
	     (compiler-internal-error __module_who__ __who__ "invalid fs" src))))

    (define* (E-asm-instr/move/var-dst x op dst src vs rs fs ns)
      (cond ((not (mem-var? dst vs))
	     (set-asm-instr-op! x 'nop)
	     (values vs rs fs ns))
	    ((or (disp? src) (constant? src))
	     (let ((vs (rem-var dst vs)))
	       (mark-var/vars-conf! dst vs)
	       (mark-var/frms-conf! dst fs)
	       (mark-var/regs-conf! dst rs)
	       (mark-var/nfvs-conf! dst ns)
	       (R src vs rs fs ns)))
	    ((register? src)
	     (let ((vs (rem-var dst vs))
		   (rs (rem-reg src rs)))
	       (mark-var/reg-move! dst src)
	       (mark-var/vars-conf! dst vs)
	       (mark-var/frms-conf! dst fs)
	       (mark-var/regs-conf! dst rs)
	       (mark-var/nfvs-conf! dst ns)
	       (values vs (add-reg src rs) fs ns)))
	    ((var? src)
	     (let ((vs (rem-var dst (rem-var src vs))))
	       (mark-var/var-move! dst src)
	       (mark-var/vars-conf! dst vs)
	       (mark-var/frms-conf! dst fs)
	       (mark-var/regs-conf! dst rs)
	       (mark-var/nfvs-conf! dst ns)
	       (values (add-var src vs) rs fs ns)))
	    ((fvar? src)
	     (let ((vs (rem-var dst vs))
		   (fs (rem-frm src fs)))
	       (mark-var/frm-move! dst src)
	       (mark-var/vars-conf! dst vs)
	       (mark-var/frms-conf! dst fs)
	       (mark-var/regs-conf! dst rs)
	       (mark-var/nfvs-conf! dst ns)
	       (values vs rs (add-frm src fs) ns)))
	    (else
	     (compiler-internal-error __module_who__ __who__ "invalid vs" src))))

    (define* (E-asm-instr/move/nfv-dst x op dst src vs rs fs ns)
      (cond ((not (mem-nfv? dst ns))
	     (compiler-internal-error __module_who__ __who__ "dead nfv"))

	    ((or (disp?     src)
		 (constant? src)
		 (register? src))
	     (let ((ns (rem-nfv dst ns)))
	       (mark-nfv/vars-conf! dst vs)
	       (mark-nfv/frms-conf! dst fs)
	       (R src vs rs fs ns)))

	    ((var? src)
	     (let ((ns (rem-nfv dst ns))
		   (vs (rem-var src vs)))
	       (mark-nfv/vars-conf! dst vs)
	       (mark-nfv/frms-conf! dst fs)
	       (values (add-var src vs) rs fs ns)))

	    ((fvar? src)
	     (let ((ns (rem-nfv dst ns))
		   (fs (rem-frm src fs)))
	       (mark-nfv/vars-conf! dst vs)
	       (mark-nfv/frms-conf! dst fs)
	       (values vs rs (add-frm src fs) ns)))

	    (else
	     (compiler-internal-error __module_who__ __who__
	       "invalid ns" src))))

    (define* (E-asm-instr/int-overflow x op dst src vs rs fs ns)
      ;;We expect the ASM-INSTR struct to have one of the formats:
      ;;
      ;;   (asm-instr int-/overflow ?dst ?src)
      ;;   (asm-instr int+/overflow ?dst ?src)
      ;;   (asm-instr int*/overflow ?dst ?src)
      ;;
      ;;Return  4 values  being the  sets  VS, RS,  FS, NS  updated with  information
      ;;representing the operands DST and SRC.
      ;;
      (let ((v (exception-live-set)))
	(unless (vector? v)
	  (compiler-internal-error __module_who__ __who__
	    "unbound exception" x v))
	(let ((vs (union-vars vs (vector-ref v 0)))
	      (rs (union-regs rs (vector-ref v 1)))
	      (fs (union-frms fs (vector-ref v 2)))
	      (ns (union-nfvs ns (vector-ref v 3))))
	  (cond ((var? dst)
		 (cond ((not (mem-var? dst vs))
			(set-asm-instr-op! x 'nop)
			(values vs rs fs ns))
		       (else
			(let ((vs (rem-var dst vs)))
			  (mark-var/vars-conf! dst vs)
			  (mark-var/frms-conf! dst fs)
			  (mark-var/nfvs-conf! dst ns)
			  (mark-var/regs-conf! dst rs)
			  (R src (add-var dst vs) rs fs ns)))))

		((register? dst)
		 (if (not (mem-reg? dst rs))
		     (values vs rs fs ns)
		   (let ((rs (rem-reg dst rs)))
		     (mark-reg/vars-conf! dst vs)
		     (R src vs (add-reg dst rs) fs ns))))

		((nfv? dst)
		 (if (not (mem-nfv? dst ns))
		     (compiler-internal-error __module_who__ __who__ "dead nfv")
		   (let ((ns (rem-nfv dst ns)))
		     (mark-nfv/vars-conf! dst vs)
		     (mark-nfv/frms-conf! dst fs)
		     (R src vs rs fs (add-nfv dst ns)))))

		(else
		 (compiler-internal-error __module_who__ __who__
		   "invalid op dst"
		   (unparse-recordized-code x)))))))

    (define* (E-asm-instr/bitwise x op dst src vs rs fs ns)
      ;;We expect the ASM-INSTR struct to have one of the formats:
      ;;
      ;;   (asm-instr logand ?dst ?src)
      ;;   (asm-instr logor  ?dst ?src)
      ;;   (asm-instr logxor ?dst ?src)
      ;;   (asm-instr sll    ?dst ?src)
      ;;   (asm-instr sra    ?dst ?src)
      ;;   (asm-instr srl    ?dst ?src)
      ;;   (asm-instr int+   ?dst ?src)
      ;;   (asm-instr int-   ?dst ?src)
      ;;   (asm-instr int*   ?dst ?src)
      ;;   (asm-instr bswap! ?dst ?src)
      ;;   (asm-instr sll/overflow (?dst ?src))
      ;;
      ;;Return  4 values  being the  sets  VS, RS,  FS, NS  updated with  information
      ;;representing the operands DST and SRC.
      ;;
      (cond ((var? dst)
	     (cond ((not (mem-var? dst vs))
		    (set-asm-instr-op! x 'nop)
		    (values vs rs fs ns))
		   (else
		    (let ((vs (rem-var dst vs)))
		      (mark-var/vars-conf! dst vs)
		      (mark-var/frms-conf! dst fs)
		      (mark-var/nfvs-conf! dst ns)
		      (mark-var/regs-conf! dst rs)
		      (R src (add-var dst vs) rs fs ns)))))

	    ((register? dst)
	     (cond ((not (mem-reg? dst rs))
		    (set-asm-instr-op! x 'nop)
		    (values vs rs fs ns))
		   (else
		    (let ((rs (rem-reg dst rs)))
		      (mark-reg/vars-conf! dst vs)
		      (R src vs (add-reg dst rs) fs ns)))))

	    ((nfv? dst)
	     (if (not (mem-nfv? dst ns))
		 (compiler-internal-error __module_who__ __who__ "dead nfv")
	       (let ((ns (rem-nfv dst ns)))
		 (mark-nfv/vars-conf! dst vs)
		 (mark-nfv/frms-conf! dst fs)
		 (R src vs rs fs (add-nfv dst ns)))))

	    (else
	     (compiler-internal-error __module_who__ __who__
	       "invalid op dst" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

    (define (mark-reg/vars-conf! r vs)
      ;;Add the register symbol name R to the REG-CONF set of every VAR struct in VS.
      ;;
      (for-each-var
	  vs locals.vars
	(lambda (v)
	  (add-interference-edge!/var->reg v r))))

    (define (mark-frm/vars-conf! f vs)
      ;;Add the FVAR struct F to the FRM-CONF set of every VAR struct in VS.
      ;;
      (for-each-var
	  vs locals.vars
	(lambda (v)
	  (add-interference-edge!/var->fvar v f))))

    (define (mark-var/vars-conf! v vs)
      ;;Add the VAR struct V to the VAR-CONF set of every VAR struct in VS.
      ;;
      ;;Add to the VAR-CONF set of V the VAR structs in VS.
      ;;
      (for-each-var
	  vs locals.vars
	(lambda (w)
	  (add-interference-edge!/var->var w v)))
      #;($set-var-var-conf! v (union-vars vs ($var-var-conf v)))
      (merge-interference-edges!/var->var v vs))

    (define (mark-nfv/vars-conf! n vs)
      #;($set-nfv-var-conf! n (union-vars vs ($nfv-var-conf n)))
      (merge-interference-edges!/nfv->var n vs))

;;; --------------------------------------------------------------------

    (define (mark-frm/nfvs-conf! f ns)
      (for-each-nfv
	  ns
	(lambda (n)
	  (add-interference-edge!/nfv->fvar n f))))

    (define (mark-var/nfvs-conf! v ns)
      (for-each-nfv
	  ns
	(lambda (n)
	  (add-interference-edge!/nfv->var n v))))

;;; --------------------------------------------------------------------

    (define (mark-var/frms-conf! v fs)
      (merge-interference-edges!/var->fvar v fs))

    (define (mark-nfv/frms-conf! n fs)
      (merge-interference-edges!/nfv->fvar n fs))

;;; --------------------------------------------------------------------

    (define (mark-var/regs-conf! v rs)
      (merge-interference-edges!/var->reg v rs))

;;; --------------------------------------------------------------------

    ;;At present we never generate Assembly code that moves a value from a NFV struct
    ;;into a NFV struct:
    ;;
    ;;   (asm-instr move (?nfv-dst ?nfv-src))
    ;;
    ;;neither between NFV structs of the  same non-tail call, nor between NFV structs
    ;;of different (nested) non-tail calls.  We could, because in some case it may be
    ;;efficient.
    ;;
    ;;When two stack operands have the  same values, we allocate a temporary location
    ;;and move the values from there:
    ;;
    ;;   (asm-instr move ?tmp-var ?some-value)
    ;;   (asm-instr move ?nfv.1   ?tmp-var)
    ;;   (asm-instr move ?nfv.2   ?tmp-var)
    ;;
    ;;For this reason  this function is commented out because  unused.  (Marco Maggi;
    ;;Thu Oct 9, 2014)
    ;;
    ;;(define (mark-nfv/nfvs-conf! n ns)
    ;;  ($set-nfv-nfv-conf! n (union-nfvs ns ($nfv-nfv-conf n)))
    ;;  (for-each-nfv
    ;;      ns
    ;;    (lambda (m)
    ;;      ($set-nfv-nfv-conf! m (add-nfv n ($nfv-nfv-conf m))))))

;;; --------------------------------------------------------------------

    (define (mark-var/var-move! x y)
      ;;Add preferende edges.
      ($set-var-var-move! x (add-var y ($var-var-move x)))
      ($set-var-var-move! y (add-var x ($var-var-move y))))

    (define (mark-var/frm-move! x y)
      ;;Add preferende edge.
      ($set-var-frm-move! x (add-frm y ($var-frm-move x))))

    (define (mark-var/reg-move! x y)
      ;;Add preferende edge.
      ($set-var-reg-move! x (add-reg y ($var-reg-move x))))

    #| end of module: E-asm-instr |# )

;;; --------------------------------------------------------------------

  (main locals.body))


(define (%rewrite locals.body locals.vars)
  ;;
  ;;
  ;;The argument  LOCALS.BODY is the  body of a LOCALS  struct; the LOCALS  struct is
  ;;either the body of a CLAMBDA clause or the init expression of a CODES struct.
  ;;
  ;;The  argument LOCALS.VARS  is  a vector  of VAR  structs  representing the  local
  ;;variables in LOCALS.BODY.   The slot index of each VAR  in LOCALS.VARS equals the
  ;;value of the INDEX of the VAR struct itself.
  ;;
  ;;NOTE The returned code does not contain NFV structs anymore.
  ;;
  ;;NOTE A  lot of  functions are  nested here because  they need  to close  upon the
  ;;argument LOCALS.VARS.
  ;;
  (module (set-member? set-difference set->list)
    (import INTEGER-SET))
  (module (for-each-var rem-nfv add-frm add-interference-edge!/var->fvar)
    (import FRAME-CONFLICT-SETS))
  (module (register?)
    (import INTEL-ASSEMBLY-CODE-GENERATION))

  (define (main body)
    (T locals.body))

;;; --------------------------------------------------------------------

  (define* (T x)
    ;;Process the struct instance X representing recordized  code as if it is in tail
    ;;position.
    ;;
    (struct-case x
      ((seq e0 e1)
       (let ((e0^ (E e0)))
	 (make-seq e0^ (T e1))))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (T e1) (T e2)))

      ((asmcall op args)
       x)

      ((shortcut body handler)
       (make-shortcut (T body) (T handler)))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid tail expression"
	 (unparse-recordized-code/sexp x)))))

;;; --------------------------------------------------------------------

  (define* (P x)
    (struct-case x
      ((seq e0 e1)
       (let ((e0^ (E e0)))
	 (make-seq e0^ (P e1))))

      ((conditional e0 e1 e2)
       (make-conditional (P e0) (P e1) (P e2)))

      ((asm-instr op dst src)
       (make-asm-instr op (R dst) (R src)))

      ((constant)
       x)

      ((shortcut body handler)
       (make-shortcut (P body) (P handler)))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid expression in predicate context"
	 (unparse-recordized-code/sexp x)))))

;;; --------------------------------------------------------------------

  (module (E)

    (define* (E x)
      (struct-case x
	((seq e0 e1)
	 (let ((e0^ (E e0)))
	   (make-seq e0^ (E e1))))

	((conditional e0 e1 e2)
	 (make-conditional (P e0) (E e1) (E e2)))

	((asm-instr op dst src)
	 (E-asm-instr x op dst src))

	((non-tail-call-frame nfv* live body)
	 (E-non-tail-call-frame nfv* live body))

	((asmcall op args)
	 (case op
	   ((nop interrupt incr/zero? fl:double->single fl:single->double)
	    x)
	   (else
	    (compiler-internal-error __module_who__ __who__
	      "invalid ASMCALL operator in recordised code for side effects"
	      (unparse-recordised-code/sexp x)))))

	((shortcut body handler)
	 (make-shortcut (E body) (E handler)))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid recordised code for effects"
	   (unparse-recordized-code x)))))

    (define* (E-asm-instr x op dst src)
      (case op
	((move bref mref32)
	 ;;If the destination equals the source: convert this instruction into a NOP.
	 (let ((dst (R dst))
	       (src (R src)))
	   (if (eq? dst src)
	       (nop)
	     (make-asm-instr op dst src))))

	(( ;;some assembly instructions
	  logand		logor		logxor
	  int+			int-		int*
	  mset			mset32
	  bset			bswap!
	  sll			sll/overflow
	  sra			srl
	  cltd			idiv
	  int-/overflow		int+/overflow	int*/overflow
	  fl:load		fl:store
	  fl:add!		fl:sub!		fl:mul!		fl:div!
	  fl:from-int		fl:shuffle	fl:load-single	fl:store-single)
	 (make-asm-instr op (R dst) (R src)))

	((nop)
	 (nop))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid ASM-INSTR operator in recordised code for side effects"
	   (unparse-recordised-code/sexp x)))))

    (module (E-non-tail-call-frame)

      (define (E-non-tail-call-frame nfv* live body)
	;;Process the  fields of a  NON-TAIL-CODE-FRAME struct and  return recordised
	;;code that does *not* contain NON-TAIL-CODE-FRAME structs anymore.
	;;
	;;NOTE We  have to remember that:  while computing the stack  operands for an
	;;upcoming non-tail call, we might  perform other non-tail calls; so multiple
	;;groups of NFV structs are present contemporarily.  This is why we also have
	;;a LIVE-NFVS set to take care of.
	;;
	(let ((live-frms1 (map (lambda (i)
				 (R-var (vector-ref locals.vars i)))
			    (set->list (non-tail-call-frame-sets-vs live))))
	      (live-frms2 (set->list (non-tail-call-frame-sets-fs live)))
	      (live-nfvs  (non-tail-call-frame-sets-ns live)))
	  (let ((idx (%actual-frame-size nfv* (fx+ 2 (max-frm live-frms1
							      (max-nfv live-nfvs
								       (max-ls live-frms2 0)))))))
	    (%assign-frame-locations-to-stack-operands! nfv* idx)
	    (NFE (fxsub1 idx)
		 (%make-livemask-vec (fxsub1 idx) live-frms1 live-frms2 live-nfvs)
		 body))))

;;; --------------------------------------------------------------------

      (define (max-frm ls i)
	(if (pair? ls)
	    (max-frm (cdr ls) (max i ($fvar-idx (car ls))))
	  i))

      (define (max-ls ls i)
	(if (pair? ls)
	    (max-ls  (cdr ls) (max i (car ls)))
	  i))

      (define* (max-nfv ls i)
	(if (pair? ls)
	    (let ((loc ($nfv-loc (car ls))))
	      (if (fvar? loc)
		  (max-nfv (cdr ls) (max i ($fvar-idx loc)))
		(compiler-internal-error __module_who__ __who__
		  "FVAR not assigned to location in MAX-NFV"
		  loc)))
	  i))

;;; --------------------------------------------------------------------

      (module (%make-livemask-vec)
	;;Build and return the livemask vector used in the non-tail call table.
	;;
	(define (%make-livemask-vec n live-frms1 live-frms2 live-nfvs)
	  (receive-and-return (mask)
	      (make-vector (fxsra (fx+ n 7) 3) 0)
	    ($for-each/stx (lambda (fvar)
			     (%set-bit! mask ($fvar-idx fvar)))
	      live-frms1)
	    ($for-each/stx (lambda (idx)
			     (%set-bit! mask idx))
	      live-frms2)
	    ($for-each/stx (lambda (nfv)
			     (cond (($nfv-loc nfv)
				    => (lambda (loc)
					 (%set-bit! mask ($fvar-idx loc))))))
	      live-nfvs)))

	(define (%set-bit! mask idx)
	  (let ((q (fxsra    idx 3))
		(r (fxand idx 7)))
	    (vector-set! mask q (fxior (vector-ref mask q) (fxsll 1 r)))))

	#| end of module: %make-livemask-vec |# )

;;; --------------------------------------------------------------------

      (module (%actual-frame-size)

	(define (%actual-frame-size nfv* i)
	  (if (%frame-size-ok? i nfv*)
	      i
	    (%actual-frame-size nfv* (fxadd1 i))))

	(define (%frame-size-ok? i nfv*)
	  ;;Tail recursive function.
	  ;;
	  (or (null? nfv*)
	      (let ((x (car nfv*)))
		(and (not (set-member?    i ($nfv-frm-conf x)))
		     (not (%var-conflict? i ($nfv-var-conf x)))
		     (%frame-size-ok? (fxadd1 i) (cdr nfv*))))))

	(define (%var-conflict? i vs)
	  (ormap (lambda (xi)
		   (let ((loc ($var-loc (vector-ref locals.vars xi))))
		     (and (fvar? loc)
			  (fx=? i ($fvar-idx loc)))))
		 (set->list vs)))

	#| end of module: %actual-frame-size |# )

      (define* (%assign-frame-locations-to-stack-operands! rand*.nfv idx)
	;;Tail recursive  function.  For each  NFV struct in the  argument RAND*.NFV,
	;;representing  a  stack operand  in  a  soon-to-be-performed non-tail  call:
	;;allocate a  FVAR struct  to serve  as actual stack  location for  the stack
	;;operand.
	;;
	;;The argument  IDX represents the  index of the  next stack machine  word to
	;;allocate; in the following scenario, the value of IDX for the first call to
	;;this function is IDX=6:
	;;
	;;           high memory
        ;;   |                          |               --
        ;;               ...                            .
        ;;   |--------------------------|               . uplevel stack frame
        ;;   | uplevel return address   | <-- FPR       .
        ;;   |--------------------------|               --
        ;;   | uplevel stack operand 0  | idx=1 fvar.1  .
        ;;   |--------------------------|               .
        ;;   | uplevel stack operand 1  | idx=2 fvar.2  .
        ;;   |--------------------------|               . stack frame described
        ;;   |       local var 0        | idx=3 fvar.3  . by this call's call table,
        ;;   |--------------------------|               . represented by FVAR structs
        ;;   |       local var 1        | idx=4 fvar.4  .
        ;;   |--------------------------|               .
        ;;   |        empty word        | idx=5 fvar.5  .
        ;;   |--------------------------|               --
        ;;   |  stack operand 0, nfv.1  | idx=6 fvar.6  .
        ;;   |--------------------------|               . operands to the call
        ;;   |  stack operand 1, nfv.2  | idx=7 fvar.7  . represented by NFV structs
        ;;   |--------------------------|               --
        ;;   |                          |
        ;;           low memory
	;;
	;;here we want to allocate stack machine word as follows:
	;;
	;;   NFV.1 == FVAR.6
	;;   NFV.2 == FVAR.7
	;;
	(when (pair? rand*.nfv)
	  (let ((rand.nfv  (car rand*.nfv))
		(rand.fvar (mkfvar idx)))
	    ($set-nfv-loc! rand.nfv rand.fvar)
	    ;;At present we are not using the field NFV-CONF of NFV structs.
	    #;(assert (null? (nfv-nfv-conf rand.nfv)))
	    ;;If, in  future, we start  using the field  NFV-CONF of NFV  structs: we
	    ;;will consider this code.
	    ;;
            ;; (for-each (lambda (x)
            ;;             ;;X is a NFV struct that is alive while we are putting the value
	    ;;             ;;of RAND.NFV on the stack.
            ;;             (let ((loc ($nfv-loc x)))
            ;;               (if loc
            ;;                   (when (fx=? ($fvar-idx loc) idx)
            ;;                     (compiler-internal-error __module_who__ __who__ "invalid assignment"))
            ;;                 (begin
            ;;                   ($set-nfv-nfv-conf! x (rem-nfv rand.nfv  ($nfv-nfv-conf x)))
            ;;                   ($set-nfv-frm-conf! x (add-frm rand.fvar ($nfv-frm-conf x)))))))
            ;;   ($nfv-nfv-conf rand.nfv))
	    ;;
	    (for-each-var
		($nfv-var-conf rand.nfv)
		locals.vars
	      (lambda (live-var)
		;;LIVE-VAR is  a VAR struct from  LOCALS.VARS that is alive  while we
		;;are putting the value of RAND.NFV on the stack.
		(let ((loc ($var-loc live-var)))
		  (if (fvar? loc)
		      ;;If the LIVE-VAR is assigned to  the same FVAR of RAND.NFV: we
		      ;;have made a mistake.
		      (when (fx=? (fvar-idx loc) idx)
			(compiler-internal-error __module_who__ __who__ "invalid assignment"))
		    (begin
		      ;;The  VAR is  already has  in its  interference edges  the NFV
		      ;;struct; now  we add an  interference edge for  the associated
		      ;;FVAR struct.
		      (add-interference-edge!/var->fvar live-var rand.fvar)))))))
	  (%assign-frame-locations-to-stack-operands! (cdr rand*.nfv) (fxadd1 idx))))

;;; --------------------------------------------------------------------

      (define* (NFE stack-frame-size mask x)
	;;Non-tail recursive function.
	;;
	(struct-case x
	  ((seq e0 e1)
	   (let ((e0^ (E e0)))
	     (make-seq e0^ (NFE stack-frame-size mask e1))))
	  ((non-tail-call target retval-location all-rand*)
	   (make-non-tail-call target
			       ;;If  the  return  value  location is  a  NFV  struct:
			       ;;replace it with the associated FVAR struct.
			       (cond ((nfv? retval-location)
				      (assert (nfv-loc retval-location))
				      ($nfv-loc retval-location))
				     ((or (var?      retval-location)
					  (register? retval-location))
				      retval-location)
				     ((not retval-location)
				      retval-location)
				     (else
				      (compiler-internal-error __module_who__ __who__
					"invalid return value location in NON-TAIL-CALL struct"
					(unparse-recordised-code/sexp retval-location))))
			       ;;Replace all the NFV  structs in ALL-RAND* with their
			       ;;assigned location.
			       (map (lambda (x)
				      (cond ((register? x)
					     x)
					    ((nfv? x)
					     (assert (nfv-loc x))
					     ($nfv-loc x))
					    (else
					     (compiler-internal-error __module_who__ __who__
					       "invalid operand"
					       (unparse-recordised-code/sexp x)))))
				 all-rand*)
			       mask stack-frame-size))
	  (else
	   (compiler-internal-error __module_who__ __who__ "invalid NF effect" x))))

      #| end of module: E-non-tail-call-frame |# )

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (module (R)

    (define* (R x)
      (if (register? x)
	  x
	(struct-case x
	  ((constant)
	   x)
	  ((fvar)
	   x)
	  ((nfv)
	   (or ($nfv-loc x)
	       (compiler-internal-error __module_who__ __who__
		 "invali NFV struct without assigned LOC")))
	  ((var)
	   (R-var x))
	  ((disp objref offset)
	   (%mk-disp (R objref) (R offset)))
	  (else
	   (compiler-internal-error __module_who__ __who__
	     "invalid R" (unparse-recordized-code x))))))

    (module (%mk-disp)

      (define* (%mk-disp {objref %disp-objref?} {offset %disp-offset?})
	(make-disp objref offset))

      (define (%disp-objref? obj)
	(or (constant? obj)
	    (and (var? obj)
		 (not (var-loc obj)))
	    (fvar?     obj)
	    (register? obj)))

      (define (%disp-offset? obj)
	(or (constant? obj)
	    (and (var? obj)
		 (not (var-loc obj)))))

      #| end of module: %mk-disp |# )

    #| end of module: R |# )

;;; --------------------------------------------------------------------

  (module (R-var)

    (define (R-var x)
      ;;X is a VAR struct.
      ;;
      (cond (($var-loc x)
	     ;;This VAR has a non-false LOC field, the value is either:
	     ;;
	     ;;* A FVAR  struct, meaning this VAR  is a stack operand  for a previous
	     ;;  call.
	     ;;
	     ;;* The  boolean #t, meaning we  are preparing a non-tail  call and this
	     ;;  VAR is alive right before this non-tail call.
	     => (lambda (loc)
		  (if (fvar? loc)
		      loc
		    (begin
		      (assert (boolean? loc))
		      (%assign x locals.vars)))))
	    ;;This VAR has no location assigned to it yet.
	    (else x)))

    (module (%assign)
      (import FRAME-CONFLICT-SETS)

      (define (%assign x.var locals.vars)
	(or (%assign-move x.var locals.vars)
	    (%assign-any  x.var locals.vars)))

      (define (%assign-move x.var locals.vars)
	;;Assign a FVAR struct to the VAR struct X.VAR.
	;;
	(let ((mr (set->list (set-difference ($var-frm-move x.var) ($var-frm-conf x.var)))))
	  (and (pair? mr)
	       (receive-and-return (x.fvar)
		   (mkfvar (car mr))
		 ($set-var-loc! x.var x.fvar)
		 ;;Register X.FVAR as  FVAR struct that is alive in  the state of all
		 ;;the live VARs.
		 (for-each-var
		     ($var-var-conf x.var)
		     locals.vars
		   (lambda (y.var)
		     ($set-var-frm-conf! y.var (add-frm x.fvar ($var-frm-conf y.var)))))
		 (for-each-var
		     ($var-var-move x.var)
		     locals.vars
		   (lambda (y.var)
		     ($set-var-frm-move! y.var (add-frm x.fvar ($var-frm-move y.var)))))))))

      (define (%assign-any x.var locals.vars)
	;;Scan the stack locations and find the first one in upper memory that is not
	;;alive; assign that location to X.VAR and return the associated FVAR struct.
	;;
	(let ((frms ($var-frm-conf x.var)))
	  (let search-lowest-index-of-unused-fvar ((i 1))
	    (if (set-member? i frms)
		(search-lowest-index-of-unused-fvar (fxadd1 i))
	      (receive-and-return (x.fvar)
		  (mkfvar i)
		($set-var-loc! x.var x.fvar)
		;;Register X.FVAR  as FVAR struct that  is alive in the  state of all
		;;the live VARs.
		(for-each-var
		    ($var-var-conf x.var)
		    locals.vars
		  (lambda (V)
		    ($set-var-frm-conf! V (add-frm x.fvar ($var-frm-conf V))))))))))

      #| end of module: %assign |# )

    #| end of module: R-var |# )

;;; --------------------------------------------------------------------

  (main locals.body))


;;;; done

#| end of LIBRARY |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-asmcall		'scheme-indent-function 1)
;; eval: (put 'make-conditional		'scheme-indent-function 2)
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; eval: (put 'for-each-var		'scheme-indent-function 2)
;; eval: (put 'for-each-nfv		'scheme-indent-function 1)
;; End:
