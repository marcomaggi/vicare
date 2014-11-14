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


;;;; compiler pass preconditions

(module (preconditions-for-color-by-chaitin)
  (module (register? eax ecx edx)
    (import INTEL-ASSEMBLY-CODE-GENERATION))

  (define-syntax __module_who__
    (identifier-syntax 'preconditions-for-color-by-chaitin))

  (define (preconditions-for-color-by-chaitin x)
    (struct-case x
      ((codes x.clambda* x.locals)
       (make-codes (map A-clambda x.clambda*) (A-locals x.locals))))
    x)

  (define (A-clambda x)
    (struct-case x
      ((clambda label clause* cp freevar* name)
       (make-clambda label (map A-clambda-clause clause*) cp freevar* name))))

  (define (A-clambda-clause x)
    (struct-case x
      ((clambda-case x.info x.locals)
       (make-clambda-case x.info (A-locals x.locals)))))

  (define (A-locals x)
    (struct-case x
      ((locals x.vars x.body)
       (let ((x.vars.vec        (car x.vars))
	     (x.vars.spillable* (cdr x.vars)))
	 (assert (vector-for-all var? x.vars.vec))
	 (assert (for-all var? x.vars.spillable*)))
       (A-body x.body))))

  (define (A-body x)
    (T x))

;;; --------------------------------------------------------------------

  (define-syntax (%compiler-internal-error/src-operand stx)
    (syntax-case stx ()
      ((?ctx ?asm-instr)
       (identifier? #'?asm-instr)
       (with-syntax
	   ((__MODULE_WHO__ (datum->syntax #'?ctx '__module_who__))
	    (__WHO__        (datum->syntax #'?ctx '__who__)))
	 #'(compiler-internal-error __MODULE_WHO__ __WHO__
	     "invalid SRC operand in ASM-INSTR struct"
	     (unparse-recordised-code/sexp ?asm-instr))))
      ))

  (define-syntax (%compiler-internal-error/dst-operand stx)
    (syntax-case stx ()
      ((?ctx ?asm-instr)
       (identifier? #'?asm-instr)
       (with-syntax
	   ((__MODULE_WHO__ (datum->syntax #'?ctx '__module_who__))
	    (__WHO__        (datum->syntax #'?ctx '__who__)))
	 #'(compiler-internal-error __MODULE_WHO__ __WHO__
	     "invalid DST operand in ASM-INSTR struct"
	     (unparse-recordised-code/sexp ?asm-instr))))
      ))

  (define-syntax (A-operand stx)
    (syntax-case stx ()
      ((?ctx ?operand ?asm-instr)
       (and (identifier? #'?operand)
	    (identifier? #'?asm-instr))
       (with-syntax
	   ((__MODULE_WHO__ (datum->syntax #'?ctx '__module_who__))
	    (__WHO__        (datum->syntax #'?ctx '__who__)))
	 #'(unless (or (register? ?operand)
		       (fvar?     ?operand)
		       (and (constant? ?operand)
			    (A-constant ?operand ?asm-instr))
		       (and (disp?  ?operand)
			    (A-disp ?operand ?asm-instr))
		       (and (var?   ?operand)
			    (A-var  ?operand ?asm-instr)))
	     (compiler-internal-error __MODULE_WHO__ __WHO__
	       "invalid ASM-INSTR operand"
	       (unparse-recordised-code/sexp ?asm-instr)
	       (unparse-recordised-code/sexp ?operand)))))
      ))

  (define-syntax (A-var stx)
    (syntax-case stx ()
      ((?ctx ?var ?asm-instr)
       (and (identifier? #'?var)
	    (identifier? #'?asm-instr))
       (with-syntax
	   ((__MODULE_WHO__ (datum->syntax #'?ctx '__module_who__))
	    (__WHO__        (datum->syntax #'?ctx '__who__)))
	 #'(unless (and (var? ?var)
			(not (var-loc ?var)))
	     (compiler-internal-error __MODULE_WHO__ __WHO__
	       "invalid VAR struct as ASM-INSTR operand"
	       (unparse-recordised-code/sexp ?asm-instr)
	       (unparse-recordised-code/sexp ?var)))))
      ))

  (module (A-disp)

    (define-syntax (A-disp stx)
      (syntax-case stx ()
	((?ctx ?disp ?asm-instr)
	 (and (identifier? #'?disp)
	      (identifier? #'?asm-instr))
	 #'(struct-case ?disp
	     ((disp objref offset)
	      (A-disp-objref objref ?asm-instr)
	      (A-disp-offset offset ?asm-instr))))
	))

    (define-syntax (A-disp-objref stx)
      (syntax-case stx ()
	((?ctx ?objref ?asm-instr)
	 (and (identifier? #'?objref)
	      (identifier? #'?asm-instr))
	 (with-syntax
	     ((__MODULE_WHO__ (datum->syntax #'?ctx '__module_who__))
	      (__WHO__        (datum->syntax #'?ctx '__who__)))
	   #'(unless (or (and (constant? ?objref)
			      (A-constant ?objref ?asm-instr))
			 (fvar?     ?objref)
			 (register? ?objref)
			 (and (var?  ?objref)
			      (A-var ?objref ?asm-instr)))
	       (compiler-internal-error __MODULE_WHO__ __WHO__
		 "invalid OBJREF field in DISP struct as ASM-INSTR operand"
		 (unparse-recordised-code/sexp ?asm-instr)
		 (unparse-recordised-code/sexp ?objref)))))
	))

    (define-syntax (A-disp-offset stx)
      (syntax-case stx ()
	((?ctx ?offset ?asm-instr)
	 (and (identifier? #'?offset)
	      (identifier? #'?asm-instr))
	 (with-syntax
	     ((__MODULE_WHO__ (datum->syntax #'?ctx '__module_who__))
	      (__WHO__        (datum->syntax #'?ctx '__who__)))
	   #'(unless (or (and (constant? ?offset)
			      (A-constant ?offset ?asm-instr))
			 (register? ?offset)
			 (and (var? ?offset)
			      (A-var ?offset ?asm-instr)))
	       (compiler-internal-error __MODULE_WHO__ __WHO__
		 "invalid OFFSET field in DISP struct as ASM-INSTR operand"
		 (unparse-recordised-code/sexp ?asm-instr)
		 (unparse-recordised-code/sexp ?offset)))))
	))

    #| end of module: A-disp |# )

  (define-syntax (A-constant stx)
    (syntax-case stx ()
      ((?ctx ?constant ?asm-instr)
       (and (identifier? #'?constant)
	    (identifier? #'?asm-instr))
       (with-syntax
	   ((__MODULE_WHO__ (datum->syntax #'?ctx '__module_who__))
	    (__WHO__        (datum->syntax #'?ctx '__who__)))
	 #'(struct-case ?constant
	     ((constant obj)
	      (unless (or (fixnum?        obj)
			  (object?        obj)
			  (code-loc?      obj)
			  (foreign-label? obj)
			  (closure-maker? obj)
			  (bignum?        obj))
		(compiler-internal-error __MODULE_WHO__ __WHO__
		  "invalid Scheme object in CONSTANT struct as ASM-INSTR operand"
		  (unparse-recordised-code/sexp ?asm-instr)
		  (unparse-recordised-code/sexp ?constant)))))))
      ))

  (define-syntax (A-constant/offset stx)
    (syntax-case stx ()
      ((?ctx ?constant ?asm-instr)
       (and (identifier? #'?constant)
	    (identifier? #'?asm-instr))
       (with-syntax
	   ((__MODULE_WHO__ (datum->syntax #'?ctx '__module_who__))
	    (__WHO__        (datum->syntax #'?ctx '__who__)))
	 #'(struct-case ?constant
	     ((constant obj)
	      (unless (or (fixnum? obj)
			  (bignum? obj))
		(compiler-internal-error __MODULE_WHO__ __WHO__
		  "invalid Scheme object in CONSTANT struct as ASM-INSTR operand representing offset"
		  (unparse-recordised-code/sexp ?asm-instr)
		  (unparse-recordised-code/sexp ?constant)))))))
      ))

;;; --------------------------------------------------------------------

  (define* (T x)
    ;;Validate expressions in tail position.
    ;;
    (struct-case x
      ((asmcall op rands)
       (unless (or (eq? op 'return)
		   (eq? op 'direct-jump)
		   (eq? op 'indirect-jump))
	 (compiler-internal-error __module_who__ __who__
	   "invalid operator in ASMCALL struct in tail position"
	   (unparse-recordised-code/sexp x))))

      ((conditional test conseq altern)
       (P test)
       (T conseq)
       (T altern))

      ((seq e0 e1)
       (E e0)
       (T e1))

      ((shortcut body handler)
       (T body)
       (T handler))

      (else
       (compiler-internal-error __module_who__  __who__
	 "invalid code in T context" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (module (E)

    (define* (E x)
      ;;Validate expressions in "for side effects" context.
      ;;
      (struct-case x
	((seq e0 e1)
	 (E e0)
	 (E e1))

	((conditional test conseq altern)
	 (P test)
	 (E conseq)
	 (E altern))

	((asm-instr op dst src)
	 (A-asm-instr op dst src x))

	((asmcall op rands)
	 (case op
	   ((nop interrupt incr/zero? fl:single->double fl:double->single)
	    (void))
	   (else
	    (compiler-internal-error __module_who__  __who__
	      "ASMCALL struct with invalid operator in E context"
	      (unparse-recordized-code x)))))

	((non-tail-call)
	 (void))

	((shortcut body handler)
	 (E body)
	 (E handler))

	(else
	 (compiler-internal-error __module_who__  __who__
	   "invalid code in E context"
	   (unparse-recordized-code/sexp x)))))

    (define* (A-asm-instr op dst src x)
      (case op
	((bref mref32)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr bref  ?var  (disp ?objref ?offset))
	 ;;   (asm-instr bref  ?fvar (disp ?objref ?offset))
	 ;;   (asm-instr mref32 ?var  (disp ?objref ?offset))
	 ;;   (asm-instr mref32 ?fvar (disp ?objref ?offset))
	 ;;
	 (unless (or (var?  dst)
		     (fvar? dst))
	   (compiler-internal-error __module_who__ __who__
	     "invalid DST operand in ASM-INSTR struct"
	     (unparse-recordised-code/sexp x)))
	 (A-disp src x))

	((move
	  logor		logxor			logand
	  int+		int-			int*
	  int+/overflow	int-/overflow		int*/overflow)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr move ?reg  ?src)
	 ;;   (asm-instr move ?var  ?src)
	 ;;   (asm-instr move ?fvar ?src)
	 ;;
	 (A-operand dst x)
	 (A-operand src x))

	((bswap!)
	 (A-operand dst x)
	 (A-operand src x))

	((cltd)
	 ;;Here we know that DST is the register EDX and SRC is the register EAX.  We
	 ;;know that CLTD and IDIV always come together.
	 (unless (eq? dst edx)
	   (%compiler-internal-error/dst-operand x))
	 (unless (eq? src eax)
	   (%compiler-internal-error/src-operand x)))

	((idiv)
	 ;;Here we know that DST is either  the register EAX or the register EDX; SRC
	 ;;is an operand, we do not know which  one here.  We know that CLTD and IDIV
	 ;;always come together.
	 (unless (or (eq? dst eax)
		     (eq? dst edx))
	   (%compiler-internal-error/dst-operand x))
	 (A-operand src x))

	((sll sra srl sll/overflow)
	 (unless (or (and (constant? src)
			  (A-constant src x))
		     (eq? src ecx))
	   (%compiler-internal-error/src-operand x))
	 (A-operand dst x))

	((mset mset32 bset)
	 ;;We expect X to have one of the formats:
	 ;;
	 ;;   (asm-instr mset   (disp ?objref ?offset) ?new-val)
	 ;;   (asm-instr bset   (disp ?objref ?offset) ?new-val)
	 ;;   (asm-instr mset32 (disp ?objref ?offset) ?new-val)
	 ;;
	 (A-disp    dst x)
	 (A-operand src x))

	((fl:load fl:store fl:add! fl:sub! fl:mul! fl:div! fl:load-single fl:store-single)
	 (A-operand dst x)
	 (A-constant/offset src x))

	((fl:from-int fl:shuffle)
	 (A-operand dst x)
	 (A-operand src x))

	(else
	 (compiler-internal-error __module_who__  __who__
	   "invalid ASM-INSTR operator in E context"
	   (unparse-recordised-code/sexp x)))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (module (P)

    (define* (P x)
      ;;Validate expressions in predicate position.
      ;;
      (struct-case x
	((constant)
	 (void))

	((conditional test conseq altern)
	 (P test)
	 (P conseq)
	 (P altern))

	((seq e0 e1)
	 (E e0)
	 (P e1))

	((asm-instr op dst src)
	 (P-asm-instr op dst src x))

	((shortcut body handler)
	 (P body)
	 (P handler))

	(else
	 (compiler-internal-error __module_who__  __who__
	   "invalid code in P context" (unparse-recordized-code/sexp x)))))

    (define* (P-asm-instr op dst src x)
      (case op
	((= != <  <= > >= u< u<= u> u>=)
	 ;;We expect X to have the format, for signed integer comparison:
	 ;;
	 ;;   (asm-instr =  ?dst ?src)
	 ;;   (asm-instr != ?dst ?src)
	 ;;   (asm-instr <  ?dst ?src)
	 ;;   (asm-instr <= ?dst ?src)
	 ;;   (asm-instr >  ?dst ?src)
	 ;;   (asm-instr >= ?dst ?src)
	 ;;
	 ;;for unsigned integer comparison:
	 ;;
	 ;;   (asm-instr u<  ?dst ?src)
	 ;;   (asm-instr u<= ?dst ?src)
	 ;;   (asm-instr u>  ?dst ?src)
	 ;;   (asm-instr u>= ?dst ?src)
	 ;;
	 ;;all  these instruction,  in truth,  will  be implemented  by the  Assembly
	 ;;instruction "cmp".
	 (A-operand dst x)
	 (A-operand src x))

	((fl:= fl:!= fl:< fl:<= fl:> fl:>=)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr fl:=  ?dst ?src)
	 ;;   (asm-instr fl:!=  ?dst ?src)
	 ;;   (asm-instr fl:<  ?dst ?src)
	 ;;   (asm-instr fl:<= ?dst ?src)
	 ;;   (asm-instr fl:>  ?dst ?src)
	 ;;   (asm-instr fl:>= ?dst ?src)
	 ;;
	 ;;These instructions always come in sequence:
	 ;;
	 ;;   (asm-instr fl:load ?reference-to-flonum1 (KN ?off-flonum-data))
	 ;;   (asm-instr fl:=    ?reference-to-flonum2 (KN ?off-flonum-data))
	 ;;
	 ;;where:  "fl:load"  loads the  first  flonum  operand  in the  CPU's  float
	 ;;register XMM0; "fl:=" performs the  comparison between the operand in XMM0
	 ;;and  the flonum  referenced in  the instruction.   So ?SRC  is always  the
	 ;;constant:
	 ;;
	 ;;   (KN ?off-flonum-data)
	 ;;
	 ;;and ?DST is a simple operand referencing a Scheme object of type flonum.
	 ;;
	 (unless (or (register? dst)
		     (fvar?     dst)
		     (and (constant? dst)
			  (A-constant dst x))
		     (and (var? dst)
			  (A-var dst x)))
	   (%compiler-internal-error/dst-operand x))
	 (unless (struct-case src
		   ((constant src.const)
		    (eq? src.const off-flonum-data))
		   (else #f))
	   (%compiler-internal-error/src-operand x)))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid ASM-INSTR operator in P context"
	   (unparse-recordised-code/sexp x)))))

    #| end of module: P |# )

  #| end of module: PRECONDITIONS-FOR-COLOR-BY-CHAITIN |# )


(module (color-by-chaitin)
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   asm-instr	code-loc	conditional
  ;;   constant		disp		fvar
  ;;   locals		non-tail-call	var
  ;;   asmcall		seq		shortcut
  ;;
  ;;in addition CLOSURE-MAKER structs can appear in side CONSTANT structs.
  ;;
  ;;NOTE In the *input* recordised code:  the ASM-INSTR structs contain, as operands:
  ;;DISP structs, FVAR  structs, CONSTANT structs, symbols  representing CPU register
  ;;names, VAR structs with LOC field set to #f.  Among these, the DISP structs have:
  ;;as  OBJREF field,  a CONSTANT,  FVAR,  VAR struct  or symbol  representing a  CPU
  ;;register name; as OFFSET field, a CONSTANT or VAR struct.  The VAR structs in the
  ;;DISP have LOC field set to #f.
  ;;
  ;;NOTE In the *output* recordised code: the ASM-INSTR structs contain, as operands:
  ;;DISP structs, FVAR  structs, CONSTANT structs, symbols  representing CPU register
  ;;names.  Among these, the  DISP structs have: as OBJREF field,  a CONSTANT or FVAR
  ;;struct or a symbol representing a CPU  register name; as OFFSET field, a CONSTANT
  ;;or symbol representing a CPU register name.
  ;;
  (import INTEL-ASSEMBLY-CODE-GENERATION)

  (define-syntax __module_who__
    (identifier-syntax 'color-by-chaitin))

  (module (color-by-chaitin)
    ;;The purpose of this module is to apply the function %COLOR-PROGRAM below to all
    ;;the bodies.
    ;;
    (define (color-by-chaitin x)
      (struct-case x
	((codes x.clambda* x.locals)
	 (make-codes (map E-clambda x.clambda*) (%color-program x.locals)))))

    (define (E-clambda x)
      (struct-case x
	((clambda label clause* cp freevar* name)
	 (make-clambda label (map E-clambda-clause clause*) cp freevar* name))))

    (define (E-clambda-clause x)
      (struct-case x
	((clambda-case x.info x.locals)
	 (make-clambda-case x.info (%color-program x.locals)))))

    (module (%color-program)

      (define (%color-program x)
	;;The argument X must  be a LOCALS struct representing the  body of a CLAMBDA
	;;clause or the  body of an initialisation expression.  Return  the return of
	;;the call to %SUBSTITUTE-VARS-WITH-ASSOCIATED-LOCATIONS.
	;;
	(module (list->set make-empty-set)
	  (import LISTY-SET))
	(struct-case x
	  ((locals x.vars x.body)
	   ;;X.VARS.VEC  is  a vector  of  VAR  structs  representing all  the  local
	   ;;variables in  X.BODY.  Some of these  VAR structs have a  FVAR struct in
	   ;;their LOC  field: they have  already been allocated to  stack locations;
	   ;;the other  VAR structs  have #f  in their  LOC field:  they are  not yet
	   ;;allocated.
	   ;;
	   ;;X.VARS.SPILLABLE* is  a list of  VAR structs representing the  subset of
	   ;;VAR structs  in X.VARS.VEC that have  #f in their LOC  field.  These VAR
	   ;;structs  can  be  allocated  to  CPU registers  or  to  stack  locations
	   ;;(spilled).
	   (let ((x.vars.vec        (car x.vars))
		 (x.vars.spillable* (cdr x.vars)))
	     (let loop ((spillable.set    (list->set x.vars.spillable*))
			(unspillable.set  (make-empty-set))
			(body             x.body))
	       (receive (unspillable.set^ body^)
		   ;;FIXME This really needs to be inside the loop.  But why?  Insert
		   ;;explanation here.  (Marco Maggi; Wed Oct 22, 2014)
		   (%add-unspillables unspillable.set body)
		 (let ((G (%build-interference-graph body^)))
		   #;(print-graph G)
		   (receive (spilled* spillable.set^ env)
		       (%color-graph spillable.set unspillable.set^ G)
		     (if (null? spilled*)
			 ;;Finished!
			 (%substitute-vars-with-associated-locations env body^)
		       ;;Another iteration is needed.
		       (let* ((env^   (%assign-stack-locations-to-spilled-vars spilled* x.vars.vec))
			      (body^^ (%substitute-vars-with-associated-locations env^ body^)))
			 (loop spillable.set^ unspillable.set^ body^^)))))))))))

      (define (%assign-stack-locations-to-spilled-vars spilled* x.vars.vec)
	;;The argument  SPILLED* is the  list of  the VAR structs  representing local
	;;variables spilled on the stack: VAR structs  whose LOC field must be set to
	;;a newly allocated FVAR struct.
	;;
	;;The argument  X.VARS.VEC is a  vector of  VAR structs representing  all the
	;;local variables in X.BODY.
	;;
	;;For every VAR struct in SPILLED*: select a stack frame location that is not
	;;already used by  other VAR structs and  allocated it to the  VAR by storing
	;;the  associated FVAR  struct in  its  LOC field;  updated the  interference
	;;graphs accordingly.
	;;
	;;Return an ENV  value: an alist whose  keys are the spilled  VAR structs and
	;;whose values are the associated FVAR structs.
	;;
	;;FIXME Why  in hell the  FVAR allocated to spilled  vars here do  not always
	;;have  increasing  index in  a  single  function execution?   Uncomment  the
	;;FPRINTFs  and see  for yourself.   Almost  always they  are incresing,  but
	;;sometimes they are not.  (Marco Maggi; Thu Nov 6, 2014)
	;;
	#;(fprintf (current-error-port) "start\n")
	($map/stx
	    (lambda (spilled)
	      (module (for-each-var add-frm rem-var mem-frm?)
		(import FRAME-CONFLICT-SETS))
	      (let ((spilled.fvar (let loop ((stack-frame-conflicts ($var-frm-conf spilled))
					     (stack-offset          1))
				    (let ((stack-frame-loc (mkfvar stack-offset)))
				      (if (mem-frm? stack-frame-loc stack-frame-conflicts)
					  ;;The  stack frame  location referenced  by
					  ;;STACK-FRAME-LOC   is   already  used   by
					  ;;another  local variable,  check the  next
					  ;;one.
					  (begin
					    #;(fprintf (current-error-port) "~a, " stack-offset)
					    (loop stack-frame-conflicts (fxadd1 stack-offset)))
					;;The  stack  frame  location  referended  by
					;;STACK-FRAME-LOC is unused: choose it.
					stack-frame-loc)))))
		#;(fprintf (current-error-port) " i=~a\n" (fvar-idx spilled.fvar))
		;;For  each local  variable in  X.VARS.VEC: remove  SPILLED from  the
		;;interference  graph of  VAR structs  (VAR-CONF) and  add it  to the
		;;interference graph of FVAR structs (FRM-CONF).
		(for-each-var
		    ($var-var-conf spilled)
		    x.vars.vec
		  (lambda (x.var)
		    ($set-var-var-conf! x.var (rem-var spilled      ($var-var-conf x.var)))
		    ($set-var-frm-conf! x.var (add-frm spilled.fvar ($var-frm-conf x.var)))))
		($set-var-loc! spilled spilled.fvar)
		(cons spilled spilled.fvar)))
	  spilled*))

      #| end of module: %COLOR-PROGRAM |# )

    #| end of module: COLOR-BY-CHAITIN |# )


(module LISTY-SET
  ;;This module implements sets of bits; each  set is a proper list of items, wrapped
  ;;by a struct.
  ;;
  ;;NOTE This  module has the  same API  of the module  INTEGER-SET, but this  is not
  ;;actually important.
  ;;
  (make-empty-set
   element->set
   set-member?		empty-set?
   set-add		set-rem
   set-difference	set-union	set-union*
   set->list		list->set
   set-for-each)

  (define-struct set
    ;;Wrapper for a list of elements used as a set.
    ;;
    (element*
		;The  list of  elements in  the set:  VAR structs,  FVAR structs  and
		;symbols representing CPU register names.  The included registers are
		;only  the ones  being  full  machine word  registers  and *not*  for
		;special purpose (not APR, not CPR, not FPR, not PCR).
     ))

  (define (element? x)
    (or (var? x) (register? x) (fvar? x)))

;;; --------------------------------------------------------------------
;;; constructors

  (define-syntax-rule (make-empty-set)
    (make-set '()))

  (define (element->set x)
    #;(assert (element? x))
    (make-set (list x)))

  (define (list->set element*)
    #;(assert (for-all element? element*))
    (make-set element*))

;;; --------------------------------------------------------------------

  (define* (set-member? x {S set?})
    (memq x ($set-element* S)))

  (define* (empty-set? {S set?})
    (null? ($set-element* S)))

  (define* (set->list {S set?})
    ($set-element* S))

  (define* (set-add x {S set?})
    ;;Add X to S, but only if it is not already contained.
    ;;
    #;(assert (element? x))
    (if (memq x ($set-element* S))
	S
      (make-set (cons x ($set-element* S)))))

  (define* (set-rem x {S set?})
    #;(assert (element? x))
    (make-set ($remq x ($set-element* S))))

  (define* (set-difference {S1 set?} {S2 set?})
    (make-set (let $difference ((element1* ($set-element* S1))
				(element2* ($set-element* S2)))
		;;Remove  from  the list  ELEMENT1*  all  the  elements of  the  list
		;;ELEMENT2*.  Use EQ? for comparison.
		(if (pair? element2*)
		    ($difference ($remq (car element2*) element1*)
				 (cdr element2*))
		  element1*))))

  (define* (set-union {S1 set?} {S2 set?})
    (make-set (let $union ((element1* ($set-element* S1))
			   (element2* ($set-element* S2)))
		(cond ((pair? element1*)
		       (cond ((memq (car element1*) element2*)
			      ($union (cdr element1*) element2*))
			     (else
			      (cons (car element1*)
				    (union (cdr element1*) element2*)))))
		      (else
		       ;;(assert (null? element1*))
		       element2*)))))

  (define-syntax set-union*
    (syntax-rules ()
      ((_ ?expr)
       ?expr)
      ((_ ?expr ... ?last-expr)
       (set-union (set-union* ?expr ...) ?last-expr))))

  (define-syntax-rule (set-for-each ?func ?set)
    ($for-each/stx ?func (set->list ?set)))

;;; --------------------------------------------------------------------

  (define ($remq x element*)
    ;;Remove X from the list ELEMENT*.
    ;;
    (cond ((pair? element*)
	   (cond ((eq? x (car element*))
		  (cdr element*))
		 (else
		  (cons (car element*)
			($remq x (cdr element*))))))
	  (else
	   ;;(assert (null? element*))
	   '())))

  #| end of module: LISTY-SET |# )


(module GRAPHS
  ;;This module is like INTEGER-GRAPHS, but it makes use of LISTY-SET.
  ;;
  (empty-graph
   add-edge!
   empty-graph?
   print-graph
   node-neighbors
   delete-node!)
  (import LISTY-SET)

  (define-struct graph
    ;;Represent the interference graph.  The graph is undirected: for every edge from
    ;;NODE1 to NODE2 there is an edge from NODE2 to NODE1.
    ;;
    (entry*
		;An alist  representing the nodes and  edges.  The keys of  the alist
		;represent graph's  nodes: structs  of type VAR  or FVAR,  or symbols
		;representing register  names.  The values  of the alist are  sets as
		;defined  by the  module LISTY-SET,  representing the  edges outgoing
		;from the node.
		;
		;The destination  nodes of the edges  outgoing from node N  are alive
		;when node N is alive.
     ))

  (define-syntax-rule (empty-graph)
    (make-graph '()))

  (define (empty-graph? G)
    (andmap (lambda (x)
	      (empty-set? (cdr x)))
	    ($graph-entry* G)))

  (module (add-edge!)

    (define (add-edge! G node1 node2)
      ;;Add an undirected edge to graph G between NODE1 and NODE2.
      ;;
      (let ((old-entry* ($graph-entry* G)))
	(cond ((assq node1 old-entry*)
	       ;;NODE1 is already in the graph.
	       => (lambda (node1.entry)
		    ;;Is NODE2 already in the set of nodes connected to NODE1?
		    (unless (set-member? node2 (cdr node1.entry))
		      ;;Not it is not, so add it.
		      (%add-directed-edge-to-entry! node1.entry node2)
		      (cond ((assq node2 old-entry*)
			     ;;NODE2 is already in the graph.  Add an edge from NODE2
			     ;;to NODE1.
			     => (lambda (node2.entry)
				  (%add-directed-edge-to-entry! node2.entry node1)))
			    (else
			     ;;NODE2 is  not in  the graph.  Add  NODE2 to  the graph
			     ;;with an edge from NODE2 to NODE1.
			     ($set-graph-entry*! G (cons (%make-singleton-entry node2 node1)
							 old-entry*)))))))
	      ((assq node2 old-entry*)
	       => (lambda (node2.entry)
		    (%add-directed-edge-to-entry! node2.entry node1)
		    ($set-graph-entry*! G (cons (%make-singleton-entry node1 node2)
						old-entry*))))
	      (else
	       ;;Neither NODE1 nor NODE2 are already nodes in the graph.  Add both of
	       ;;them.
	       ($set-graph-entry*! G (cons* (%make-singleton-entry node1 node2)
					    (%make-singleton-entry node2 node1)
					    old-entry*))))))

    (define-syntax-rule (%make-singleton-entry ?src-node ?dst-node)
      ;;Build and  return a new  entry representing  node ?SRC-NODE with  an outgoing
      ;;edge from ?SRC-NODE to ?DST-NODE.
      ;;
      (cons ?src-node (element->set ?dst-node)))

    (define-syntax (%add-directed-edge-to-entry! stx)
      ;;Given a node's entry: add an edge from the entry's node to ?DST-NODE.
      ;;
      (syntax-case stx ()
	((_ ?src-node-entry ?dst-node)
	 (and (identifier? #'?src-node-entry)
	      (identifier? #'?dst-noe))
	 #'(set-cdr! ?src-node-entry (set-add ?dst-node (cdr ?src-node-entry))))
	))

    #| end of module: ADD-EDGE! |# )

  (define (node-neighbors x G)
    ;;Return a set containing the nodes connected to X.  Such nodes are the locations
    ;;alive when X is written.
    ;;
    (cond ((assq x ($graph-entry* G))
	   => cdr)
	  (else
	   (make-empty-set))))

  (define* (delete-node! x G)
    ;;Removing a  node means removing  all the  edges to and  from X; the  location X
    ;;still has an entry in G.  If X is not a node: nothing happens.
    ;;
    (let ((entry* ($graph-entry* G)))
      (cond ((assq x entry*)
	     => (lambda (x.entry)
		  ;;For every node Y connected to X: remove the edge from Y to X.
		  (set-for-each (lambda (y)
				  (let ((y.entry (assq y entry*)))
				    (set-cdr! y.entry (set-rem x (cdr y.entry)))))
		    (cdr x.entry))
		  ;;Remove all the edges from X to other nodes.
		  (set-cdr! x.entry (make-empty-set)))))))

;;; --------------------------------------------------------------------

  (define (print-graph G)
    (printf "G={\n")
    (parameterize ((print-gensym 'pretty))
      (for-each (lambda (x)
                  (let ((lhs  (car x))
			(rhs* (cdr x)))
                    (printf "  ~s => ~s\n"
                            (unparse-recordized-code lhs)
                            (map unparse-recordized-code (set->list rhs*)))))
        ($graph-entry* G)))
    (printf "}\n"))

  #| end of module: GRAPHS |# )


(module (%add-unspillables)

  (define-syntax __who__
    (identifier-syntax '%add-unspillables))

;;; --------------------------------------------------------------------

  (module ASM-INSTR-OPERANDS-HELPERS
    (disp/fvar? long-immediate? small-operand?)
    ;;All the  function exported  by this  module are  applied to  the ?SRC  and ?DST
    ;;operands of ASM-INSTR structs.
    ;;

    (module SIGNED-32-BIT-INTEGER-LIMITS
      (MIN-SIGNED-32-BIT-INTEGER MAX-SIGNED-32-BIT-INTEGER)
      (define-inline-constant MIN-SIGNED-32-BIT-INTEGER
	(- (expt 2 31)))
      (define-inline-constant MAX-SIGNED-32-BIT-INTEGER
	(- (expt 2 31) 1))
      #| end of module: SIGNED-32-BIT-INTEGER-LIMITS |# )

    (define (disp/fvar? x)
      (or (disp? x) (fvar? x)))

    (define (long-immediate? x)
      ;;Return true  if X  represents a  constant signed  integer too  big to  fit in
      ;;32-bit; otherwise return false.
      ;;
      (import SIGNED-32-BIT-INTEGER-LIMITS)
      (struct-case x
	((constant n)
	 (cond ((integer? n)
		(not (<= MIN-SIGNED-32-BIT-INTEGER
			 n
			 MAX-SIGNED-32-BIT-INTEGER)))
	       (else #t)))
	(else #f)))

    (define (small-operand? x)
      (import SIGNED-32-BIT-INTEGER-LIMITS)
      ;; (assert (or (disp? x)
      ;;             (fvar? x)
      ;;             (and (var? x)
      ;;                  (not (var-loc x)))
      ;;             (register? x)
      ;;             (constant? x)))
      (boot.case-word-size
       ((32)
	(not (disp/fvar? x)))
       ((64)
	(struct-case x
	  ((constant n)
	   (if (integer? n)
	       (<= MIN-SIGNED-32-BIT-INTEGER
		   n
		   MAX-SIGNED-32-BIT-INTEGER)
	     #f))
	  (else
	   (or (register? x)
	       (var?      x)))))))

    #| end of module: ASM-INSTR-OPERANDS-HELPERS |# )

;;; --------------------------------------------------------------------

  (define (%add-unspillables unspillable.set body)
    ;;The argument UNSPILLABLE.SET  is a set (as defined by  the LISTY-SET module) of
    ;;VAR structs representing  unspillable local variables in BODY;  it starts empty
    ;;and we  fill it  in this  function.  Return 2  values: a  new set  derived from
    ;;UNSPILLABLE.SET by adding elements; a  struct representing recordised code that
    ;;must replace BODY.
    ;;
    ;;NOTE  A lot  of functions  are nested  here because  they call  the subfunction
    ;;%MAKE-UNSPILLABLE-VAR, and  the %MAKE-UNSPILLABLE-VAR  needs to close  upon the
    ;;argument UNSPILLABLE.SET.
    ;;

    (define (main body)
      ;;Here we want to return the value of the mutated UNSPILLABLE.SET binding.
      (values unspillable.set (T body)))

    (define (%make-unspillable-var)
      (module (set-add)
	(import LISTY-SET))
      (receive-and-return (unspillable)
	  (make-unique-var 'unspillable-tmp)
	(set! unspillable.set (set-add unspillable unspillable.set))))

;;; --------------------------------------------------------------------

    (define (T x)
      (struct-case x
	((asmcall op rands)
	 #;(assert (or (eq? op 'return) (eq? op 'direct-jump) (eq? op 'indirect-jump)))
	 ;;We assume the input code is correct; this means in tail position there are
	 ;;only ASMCALL  structs with  operand RETURN, DIRECT-JUMP  or INDIRECT-JUMP.
	 ;;We know that the operands of  such ASMCALL structs are CPU register symbol
	 ;;names and FVAR structs; so there is  nothing to be done here.  Just return
	 ;;X itself.
	 x)

	((conditional e0 e1 e2)
	 (make-conditional (P e0) (T e1) (T e2)))

	((seq e0 e1)
	 (make-seq (E e0) (T e1)))

	((shortcut body handler)
	 (make-shortcut (T body) (T handler)))

	(else
	 (compiler-internal-error __module_who__  __who__
	   "invalid code in T context" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

    (module (E)

      (define (E x)
	(struct-case x
	  ((seq e0 e1)
	   (make-seq (E e0) (E e1)))

	  ((conditional e0 e1 e2)
	   (make-conditional (P e0) (E e1) (E e2)))

	  ((asm-instr op dst src)
	   (E-asm-instr op dst src x))

	  ((asmcall op rands)
	   (case op
	     ((nop interrupt incr/zero? fl:single->double fl:double->single)
	      x)
	     (else
	      (compiler-internal-error __module_who__  __who__
		"ASMCALL struct with invalid operator in E context"
		(unparse-recordized-code x)))))

	  ((non-tail-call)
	   x)

	  ((shortcut body handler)
	   ;;Do BODY first, then HANDLER.
	   (let ((body^ (E body)))
	     (make-shortcut body^ (E handler))))

	  (else
	   (compiler-internal-error __module_who__  __who__
	     "invalid code in E context"
	     (unparse-recordized-code x)))))

      (define (E-asm-instr op dst src x)
	;;On any host the CPU has limited addressing capabilities.  When two operands
	;;require unsupported  addressing: the  instruction has to  be split  in two,
	;;with the introduction of a temporary variable.  The temporary variable must
	;;be allocated to a CPU register, and so it is not spillable on the stack.
	;;
	(import ASM-INSTR-OPERANDS-HELPERS)
	(case op
	  ((bref mref32)
	   (E-asm-instr/load op dst src x))

	  ((move
	    logor		logxor			logand
	    int+		int-			int*
	    int+/overflow	int-/overflow		int*/overflow)
	   ;;We expect X to have the format:
	   ;;
	   ;;   (asm-instr move ?reg  ?src)
	   ;;   (asm-instr move ?var  ?src)
	   ;;   (asm-instr move ?fvar ?src)
	   ;;
	   (cond ((and (eq? op 'move)
		       (eq? dst src))
		  ;;Source and dest are the same: do nothing.
		  (nop))
		 ((and (= wordsize 8)
		       (not (eq? op 'move))
		       (long-immediate? src))
		  (let ((unspillable (%make-unspillable-var)))
		    (make-seq
		      (E (make-asm-instr 'move unspillable src))
		      (E (make-asm-instr op dst unspillable)))))
		 ((and (memq op '(int* int*/overflow))
		       (disp/fvar? dst))
		  (let ((unspillable (%make-unspillable-var)))
		    (multiple-forms-sequence
		      (E (make-asm-instr 'move unspillable dst))
		      (E (make-asm-instr op unspillable src))
		      (E (make-asm-instr 'move dst unspillable)))))
		 ((and (disp/fvar? dst)
		       (not (small-operand? src)))
		  (let ((unspillable (%make-unspillable-var)))
		    (make-seq
		      (E (make-asm-instr 'move unspillable src))
		      (E (make-asm-instr op dst unspillable)))))
		 ((disp? dst)
		  (let ((s0 (disp-objref dst))
			(s1 (disp-offset dst)))
		    (cond ((not (small-operand? s0))
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable s0))
			       (E (make-asm-instr op (make-disp unspillable s1) src)))))
			  ((not (small-operand? s1))
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable s1))
			       (E (make-asm-instr op (make-disp s0 unspillable) src)))))
			  ((small-operand? src)
			   x)
			  (else
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable src))
			       (E (make-asm-instr op dst unspillable))))))))
		 ((disp? src)
		  (let ((s0 (disp-objref src))
			(s1 (disp-offset src)))
		    (cond ((not (small-operand? s0))
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable s0))
			       (E (make-asm-instr op dst (make-disp unspillable s1))))))
			  ((not (small-operand? s1))
			   (let ((unspillable (%make-unspillable-var)))
			     (make-seq
			       (E (make-asm-instr 'move unspillable s1))
			       (E (make-asm-instr op dst (make-disp s0 unspillable))))))
			  (else x))))
		 (else x)))

	  ((bswap!)
	   (if (disp/fvar? src)
	       (let ((unspillable (%make-unspillable-var)))
		 (multiple-forms-sequence
		   (E (make-asm-instr 'move   unspillable dst))
		   (E (make-asm-instr 'bswap! unspillable unspillable))
		   (E (make-asm-instr 'move   src         unspillable))))
	     x))

	  ((cltd)
	   ;;Here we know that  DST is the register EDX and SRC  is the register EAX.
	   ;;We know that CLTD and IDIV always come together.
	   (assert (eq? dst edx))
	   (assert (eq? src eax))
	   x)

	  ((idiv)
	   ;;Here we know  that DST is either  the register EAX or  the register EDX;
	   ;;SRC is an operand, we do not know which one here.  We know that CLTD and
	   ;;IDIV always come together.
	   (assert (or (eq? dst eax) (eq? dst edx)))
	   (if (or (var?      src)
		   (register? src))
	       x
	     (let ((unspillable (%make-unspillable-var)))
	       (make-seq
		 (E (make-asm-instr 'move unspillable src))
		 (E (make-asm-instr 'idiv dst unspillable))))))

	  ((sll sra srl sll/overflow)
	   (unless (or (constant? src)
		       (eq? src ecx))
	     (compiler-internal-error __module_who__  __who__ "invalid shift" src))
	   x)

	  ((mset mset32 bset)
	   ;;We expect X to have one of the formats:
	   ;;
	   ;;   (asm-instr mset   (disp ?objref ?offset) ?new-val)
	   ;;   (asm-instr bset   (disp ?objref ?offset) ?new-val)
	   ;;   (asm-instr mset32 (disp ?objref ?offset) ?new-val)
	   ;;
	   ;;MSET is used, for example, to store objects in the car and cdr of pairs.
	   ;;MSET32 is  used, for example,  to store  single characters in  a string.
	   ;;BSET is used, for example, to store single bytes in a bytevector.
	   (if (not (small-operand? src))
	       (let ((unspillable (%make-unspillable-var)))
		 (make-seq
		   (E (make-asm-instr 'move unspillable src))
		   (E (make-asm-instr op dst unspillable))))
	     (check-disp dst
			 (lambda (dst)
			   (let ((s0 (disp-objref dst))
				 (s1 (disp-offset dst)))
			     (if (and (constant? s0)
				      (constant? s1))
				 (let ((unspillable (%make-unspillable-var)))
				   (multiple-forms-sequence
				     (E (make-asm-instr 'move unspillable s0))
				     (E (make-asm-instr 'int+ unspillable s1))
				     (let ((disp (make-disp unspillable (make-constant 0))))
				       (make-asm-instr op disp src))))
			       (make-asm-instr op dst src)))))))

	  ((fl:load fl:store fl:add! fl:sub! fl:mul! fl:div! fl:load-single fl:store-single)
	   ;;We expect X to have the format:
	   ;;
	   ;;   (asm-instr fl:load         ?reference ?offset)
	   ;;   (asm-instr fl:store        ?reference ?offset)
	   ;;   (asm-instr fl:load-single  ?reference ?offset)
	   ;;   (asm-instr fl:store-single ?reference ?offset)
	   ;;
	   ;;where:  ?REFERENCE  is   a  simple  operand,  often   (but  not  always)
	   ;;representing  a tagged  pointer to  flonum object;  ?OFFSET is  a simple
	   ;;operand, often (but not always) the constant:
	   ;;
	   ;;   (KN off-flonum-data)
	   ;;
	   ;;but in any case it is a CONSTANT.
	   ;;
	   ;;Otherwise, we expect X to have the format:
	   ;;
	   ;;   (asm-instr fl:add!  ?reference-to-flonum (KN off-flonum-data))
	   ;;   (asm-instr fl:sub!  ?reference-to-flonum (KN off-flonum-data))
	   ;;   (asm-instr fl:mul!  ?reference-to-flonum (KN off-flonum-data))
	   ;;   (asm-instr fl:div!  ?reference-to-flonum (KN off-flonum-data))
	   ;;
	   ;;these arithmetic instructions always come in sequence:
	   ;;
	   ;;   (asm-instr fl:load ?reference-to-flonum1 (KN ?off-flonum-data))
	   ;;   (asm-instr fl:add! ?reference-to-flonum2 (KN ?off-flonum-data))
	   ;;
	   ;;where:  "fl:load" loads  the first  flonum  operand in  the CPU's  float
	   ;;register XMM0; "fl:add!"  performs the  operation between the operand in
	   ;;XMM0  and  the  flonum  referenced  in the  instruction.   So,  for  the
	   ;;arithmetic operators, ?SRC is always the constant:
	   ;;
	   ;;   (KN ?off-flonum-data)
	   ;;
	   ;;and ?DST is a simple  operand representing a tagged pointer, referencing
	   ;;a Scheme object of type flonum.
	   ;;
	   ;;We have to remember that a  core primitive operation "simple operand" is
	   ;;either a VAR struct or a CONSTANT; here we know that ?SRC will always be
	   ;;a constant.   ?DST and  ?SRC are  used here to  compose a  DISP symbolic
	   ;;expression for Assembler consumption:
	   ;;
	   ;;   (disp ?DST ?SRC)
	   ;;
	   ;;in such expressions  we have to make  sure that ?SRC fits  into 32 bits;
	   ;;?DST can be either a register or FVAR.
	   ;;
	   (check-disp-arg dst
			   (lambda (dst)
			     (check-disp-arg src
					     (lambda (src)
					       (make-asm-instr op dst src))))))

	  ((fl:from-int fl:shuffle)
	   x)

	  (else
	   (compiler-internal-error __module_who__  __who__
	     "invalid ASM-INSTR operator" op))))

      (module (E-asm-instr/load)
	;;We expect X to have the format:
	;;
	;;   (asm-instr bref  ?var  (disp ?objref ?offset))
	;;   (asm-instr bref  ?fvar (disp ?objref ?offset))
	;;   (asm-instr mref32 ?var  (disp ?objref ?offset))
	;;   (asm-instr mref32 ?fvar (disp ?objref ?offset))
	;;
	;;There are cases for which we cannot directly generate machine code:
	;;
	;;* In the case of destination operand being a stack slot:
	;;
	;;     (asm-instr bref ?fvar (disp ?objref ?offset))
	;;
	;;  both the operands would be memory  references and this is not a supported
	;;  operation; so we split such ASM-INSTR into:
	;;
	;;     (asm-instr bref ?var.tmp (disp ?objref ?offset))
	;;     (asm-instr move  ?fvar ?var.tmp)
	;;
	;;  where ?VAR.TMP is a VAR  struct, representing a temporary location, which
	;;  must be allocated to a CPU register and so it is unspillable.  We do this
	;;  kind of processing recursively, introducing all the needed temporary VAR,
	;;  until every generated instruction is simple enough.
	;;
	;;* If ?objref and/or ?offset are not "small operands" we split the ASM-INSTR
	;;  into:
	;;
	;;     (asm-instr move  ?var.tmp ?objref)
	;;     (asm-instr bref ?dst     (disp ?var.tmp ?offset))
	;;
	;;  or:
	;;
	;;     (asm-instr move  ?var.tmp ?offset)
	;;     (asm-instr bref ?dst     (disp ?objref ?var.tmp))
	;;
	;;  or:
	;;
	;;     (asm-instr move  ?var1.tmp ?objref)
	;;     (asm-instr move  ?var2.tmp ?offset)
	;;     (asm-instr bref ?dst      (disp ?var1.tmp ?var2.tmp))
	;;
	;;
	(import ASM-INSTR-OPERANDS-HELPERS)

	(define (E-asm-instr/load op dst src x)
	  #;(assert (or (var? dst) (fvar? dst)))
	  #;(assert (disp? src))
	  (%fix-disp-address src
			     (lambda (src)
			       (cond ((var?  dst)
				      (make-asm-instr op dst src))
				     ((fvar? dst)
				      (let ((unspillable (%make-unspillable-var)))
					(make-seq
					  (make-asm-instr op unspillable src)
					  (E (make-asm-instr 'move dst unspillable)))))
				     (else
				      (compiler-internal-error __module_who__ __who__
					"invalid destination operand, expected VAR or FVAR"
					(unparse-recordised-code/sexp x)))))))

	(define (%fix-disp-address src kont)
	  ;;Non-tail recursive  function.  Introduce the MOVE  instructions needed to
	  ;;transform a complex SRC argument into a simple one.
	  ;;
	  (struct-case src
	    ((disp objref offset)
	     (cond ((not (small-operand? objref))
		    (let ((unspillable (%make-unspillable-var)))
		      (make-seq
			(E (make-asm-instr 'move unspillable objref))
			(%fix-disp-address (make-disp unspillable offset) kont))))
		   ((not (small-operand? offset))
		    (let ((unspillable (%make-unspillable-var)))
		      (make-seq
			(E (make-asm-instr 'move unspillable offset))
			(%fix-disp-address (make-disp objref unspillable) kont))))
		   (else
		    (kont src))))
	    (else
	     (kont src))))

	#| end of module: E-asm-instr/load |# )

      #| end of module: E |# )

;;; --------------------------------------------------------------------

    (module (P)

      (define (P x)
	(struct-case x
	  ((constant)
	   x)

	  ((conditional e0 e1 e2)
	   (make-conditional (P e0) (P e1) (P e2)))

	  ((seq e0 e1)
	   (make-seq (E e0) (P e1)))

	  ((asm-instr op dst src)
	   (P-asm-instr op dst src x))

	  ((shortcut body handler)
	   ;;Do BODY first, then HANDLER.
	   (let ((body (P body)))
	     (make-shortcut body (P handler))))

	  (else
	   (compiler-internal-error __module_who__  __who__
	     "invalid code in P context" (unparse-recordized-code x)))))

      (define (P-asm-instr op dst src x)
	(case op
	  ((= != <  <= > >= u< u<= u> u>=)
	   (P-asm-instr/integer-comparison      op dst src x))
	  ((fl:= fl:!= fl:< fl:<= fl:> fl:>=)
	   (P-asm-instr/float-number-comparison op dst src x))
	  (else
	   (compiler-internal-error __module_who__ __who__
	     "invalid ASM-INSTR operator in P context"
	     (unparse-recordised-code/sexp x)))))

      (define (P-asm-instr/integer-comparison op dst src x)
	;;We expect X to have the format, for signed integer comparison:
	;;
	;;   (asm-instr =  ?dst ?src)
	;;   (asm-instr != ?dst ?src)
	;;   (asm-instr <  ?dst ?src)
	;;   (asm-instr <= ?dst ?src)
	;;   (asm-instr >  ?dst ?src)
	;;   (asm-instr >= ?dst ?src)
	;;
	;;for unsigned integer comparison:
	;;
	;;   (asm-instr u<  ?dst ?src)
	;;   (asm-instr u<= ?dst ?src)
	;;   (asm-instr u>  ?dst ?src)
	;;   (asm-instr u>= ?dst ?src)
	;;
	;;all  these instruction,  in  truth,  will be  implemented  by the  Assembly
	;;instruction  "cmp".
	;;
	;;FIXME With this Assembly instruction an  integer in a CONSTANT, as operand,
	;;can be  at most 32-bit?  Or  can it be  extended to 64-bit in  64-bit mode?
	;;See the reference manual.  (Marco Maggi; Tue Oct 28, 2014)
	;;
	(import ASM-INSTR-OPERANDS-HELPERS)
	(cond ((and (not (disp/fvar? dst))
		    (not (small-operand?  dst)))
	       (let ((unspillable (%make-unspillable-var)))
		 (make-seq
		   (E (make-asm-instr 'move unspillable dst))
		   (P (make-asm-instr op unspillable src)))))

	      ((and (not (disp/fvar? src))
		    (not (small-operand?  src)))
	       (let ((unspillable (%make-unspillable-var)))
		 (make-seq
		   (E (make-asm-instr 'move unspillable src))
		   (P (make-asm-instr op dst unspillable)))))

	      ((and (disp/fvar? dst)
		    (disp/fvar? src))
	       (let ((unspillable (%make-unspillable-var)))
		 (make-seq
		   (E (make-asm-instr 'move unspillable src))
		   (P (make-asm-instr op    dst unspillable)))))

	      (else
	       (check-disp dst
			   (lambda (dst)
			     (check-disp src
					 (lambda (src)
					   (make-asm-instr op dst src))))))))

      (define (P-asm-instr/float-number-comparison op dst src x)
	;;We expect X to have the format:
	;;
	;;   (asm-instr fl:=  ?dst ?src)
	;;   (asm-instr fl:!= ?dst ?src)
	;;   (asm-instr fl:<  ?dst ?src)
	;;   (asm-instr fl:<= ?dst ?src)
	;;   (asm-instr fl:>  ?dst ?src)
	;;   (asm-instr fl:>= ?dst ?src)
	;;
	;;These instructions always come in sequence:
	;;
	;;   (asm-instr fl:load ?reference-to-flonum1 (KN ?off-flonum-data))
	;;   (asm-instr fl:=    ?reference-to-flonum2 (KN ?off-flonum-data))
	;;
	;;where: "fl:load" loads the first flonum operand in the CPU's float register
	;;XMM0; "fl:="  performs the comparison between  the operand in XMM0  and the
	;;flonum referenced in the instruction.  So ?SRC is always the constant:
	;;
	;;   (KN ?off-flonum-data)
	;;
	;;and ?DST is a simple operand referencing a Scheme object of type flonum.
	;;
	;;The actually generated  Intel Assembly looks (more or less)  like this (for
	;;"fl:="):
	;;
	;;   (ucomisd (disp ?ref-to-flonum2 ?off-flonum-data) xmm0)
	;;   (je ?consequent-label)
	;;
	;;where   UCOMISD   is   the   mnemonic   for   "Unordered   Compare   Scalar
	;;Double-Precision Floating-Point Values and  Set EFLAGS".  The DISP Assembly
	;;sexp  must  represent   the  address  of  a  64-bit   memory  location,  as
	;;register+offset.
	;;
	;;So ?DST *cannot* be an FVAR or a DISP, because, in that case, it would be a
	;;reference to memory  location, which in turn holds the  reference to double
	;;float; ?DSP must  be itself register.  For this reason  here we introduce a
	;;temporary, unspillable variable when needed, so that the code becomes:
	;;
	;;   (move ?register ?ref-to-flonum2)
	;;   (ucomisd (disp ?register ?off-flonum-data) xmm0)
	;;   (je ?consequent-label)
	;;
	(import ASM-INSTR-OPERANDS-HELPERS)
	(assert (struct-case src
		  ((constant src.const)
		   (eq? src.const off-flonum-data))
		  (else #f)))
	(cond ((or (disp? dst)
		   (fvar? dst))
	       (let ((unspillable (%make-unspillable-var)))
		 (make-seq
		   (E (make-asm-instr 'move unspillable dst))
		   (make-asm-instr op unspillable src))))
	      ((var? dst)
	       ;;This VAR struct is not yet allocated.
	       (assert (not (var-loc dst)))
	       x)
	      ((register? dst)
	       x)
	      (else
	       (compiler-internal-error __module_who__ __who__
		 "invalid DST field in ASM-INSTR with float comparison operator"
		 (unparse-recordised-code/sexp x)))))

      #| end of module: P |# )

;;; --------------------------------------------------------------------

    (define (check-disp-arg x kont)
      (import ASM-INSTR-OPERANDS-HELPERS)
      (if (small-operand? x)
	  (kont x)
	(let ((unspillable (%make-unspillable-var)))
	  (make-seq
	    (E (make-asm-instr 'move unspillable x))
	    (kont unspillable)))))

    (define (check-disp x kont)
      (struct-case x
	((disp objref offset)
	 (check-disp-arg objref
			 (lambda (objref^)
			   (check-disp-arg offset
					   (lambda (offset^)
					     (kont (make-disp objref^ offset^)))))))
	(else
	 (kont x))))

    (main body)) ;;end of function %ADD-UNSPILLABLES

  #| end of module: %ADD-UNSPILLABLES |# )


(define* (%build-interference-graph body)
  ;;Process BODY with a depth-first traversal, visiting the tail branches first, in a
  ;;post-order fashion; while rewinding:
  ;;
  ;;* Build the live  set: upon entering a code struct the live  set is a SET struct,
  ;;   as defined  by the  LISTY-SET  module, containing  all the  VAR structs,  FVAR
  ;;  structs  and CPU register  symbol names  that are *read*  at least once  in the
  ;;  continuation of the code struct.
  ;;
  ;;* Build the interference graph: a GRAPH  struct, as defined by the GRAPHS module,
  ;;  containing one node for every VAR  struct, FVAR struct and CPU register that is
  ;;  *written* at least  once.  The edges of each node  represent the locations that
  ;;  are  alive when the  node is written:  all the locations  that will be  read at
  ;;  least once in the continuation of the writing instruction.
  ;;
  ;;Return the GRAPH struct representing the interference graph.
  ;;
  ;;NOTE Locations are added  to a live set only by the  function "R" which processes
  ;;operands.  "R" processes the leaves of the  BODY and creates either an empty live
  ;;set or  a live set  containing a single  location; other functions  take multiple
  ;;live sets and compose new live sets with the union operation.
  ;;
  ;;NOTE  Edges   are  added  to  the   interference  graph  only  by   the  function
  ;;"E-asm-instr",  which  is the  only  one  that  processes  code which  writes  to
  ;;locations.
  ;;
  ;;NOTE A lot  of functions are nested  here because they need to  close upon GRAPH,
  ;;which is mutated as  the code traversal progresses and finally  it is returned to
  ;;the caller.
  ;;
  (import LISTY-SET)
  (import GRAPHS)

  (define (main body)
    ;;We discard the return value from T.
    (T body)
    #;(pretty-print (unparse-recordized-code/sexp x))
    #;(print-graph THE-GRAPH)
    THE-GRAPH)

  (define THE-GRAPH
    (empty-graph))

  (define exception-live-set
    ;;Whenever we enter a SHORTCUT, we.
    ;;
    ;;1. Process the handler first.
    ;;
    ;;2. Set this parameter to the handler's set.
    ;;
    ;;3. Process the body.
    ;;
    ;;The handler's  set becomes the set  of ASMCALL structs with  operands INTERRUPT
    ;;and  INCR/ZERO?,  and  it  is  consumed by  ASM-INSTR  structs  with  operands:
    ;;INT-/OVERFLOW, INT+/OVERFLOW, INT*/OVERFLOW.
    ;;
    (make-parameter #f))

;;; --------------------------------------------------------------------

  (define (T x)
    ;;Process the  struct instance X,  representing recordized code,  as if it  is in
    ;;tail position.
    ;;
    (struct-case x
      ((conditional test conseq altern)
       (let ((conseq.set (T conseq))
	     (altern.set (T altern)))
	 (P test conseq.set altern.set (set-union conseq.set altern.set))))

      ((asmcall op rand*)
       #;(assert (or (eq? op 'return) (eq? op 'direct-jump) (eq? op 'indirect-jump)))
       ;;We assume the input  code is correct; this means in  tail position there are
       ;;only ASMCALL structs with operand  RETURN, DIRECT-JUMP or INDIRECT-JUMP.  We
       ;;know that the operands of such ASMCALL structs are CPU register symbol names
       ;;and FVAR structs.
       (R* rand*))

      ((seq e0 e1)
       (E e0 (T e1)))

      ((shortcut body handler)
       ;;Do the handler first, then the body.
       (let ((handler.set (T handler)))
	 (parameterize ((exception-live-set handler.set))
	   (T body))))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid code in T context" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define (R* rand*)
    ;;Process the  operands of ASMCALL  and NON-TAIL-CALL structs.  Build  and return
    ;;the union between the live sets of all the RAND*.
    ;;
    (if (pair? rand*)
	(set-union (R  (car rand*))
		   (R* (cdr rand*)))
      (make-empty-set)))

  (define (R x)
    ;;Process X which must be an operand in a struct of type: ASMCALL, NON-TAIL-CALL,
    ;;ASM-INSTR, DISP.  Return the live set of X.
    ;;
    (struct-case x
      ((constant)
       (make-empty-set))
      ((var)
       (element->set x))
      ((disp objref offset)
       (set-union (R objref) (R offset)))
      ((fvar)
       (make-empty-set))
      ((code-loc)
       (make-empty-set))
      (else
       (if (register? x)
	   ;;Build a live  set containing the register  only if it is  a full machine
	   ;;word register and *not* a register  with special purpose (APR, CPR, FPR,
	   ;;PCR).
	   (if (memq x ALL-REGISTERS)
	       (element->set x)
	     (make-empty-set))
	 (compiler-internal-error __module_who__ __who__
	   "invalid code in R context" (unparse-recordised-code/sexp x))))))

;;; --------------------------------------------------------------------

  (define (P x tail-conseq.set tail-altern.set tail-union.set)
    ;;If we are in "for predicate" context:  there is other code in tail context that
    ;;has been  processed before X by  the backwards code traversal.   Such tail code
    ;;has produced the sets:
    ;;
    ;;TAIL-CONSEQ.SET -
    ;;  From the uplevel CONSEQ branch, it is to be used for test-true processing.
    ;;
    ;;TAIL-ALTERN.SET -
    ;;  From the uplevel ALTERN branch, it is to be used for test-false processing.
    ;;
    ;;TAIL-UNION.SET -
    ;;  The union  between the uplevel TAIL-CONSEQ.SET and TAIL-ALTERN.SET;  it is to
    ;;  be  joined with  the sets  produced here and  the result  is returned  to the
    ;;  caller.
    ;;
    (struct-case x
      ((constant x.const)
       (if x.const tail-conseq.set tail-altern.set))

      ((seq e0 e1)
       (E e0 (P e1 tail-conseq.set tail-altern.set tail-union.set)))

      ((conditional test conseq altern)
       (let ((conseq.set (P conseq tail-conseq.set tail-altern.set tail-union.set))
	     (altern.set (P altern tail-conseq.set tail-altern.set tail-union.set)))
	 (P test conseq.set altern.set (set-union conseq.set altern.set))))

      ((asm-instr op dst src)
       (set-union* (R dst) (R src) tail-union.set))

      ((shortcut body handler)
       (let ((handler.set (P handler tail-conseq.set tail-altern.set tail-union.set)))
	 (parameterize ((exception-live-set handler.set))
	   (P body tail-conseq.set tail-altern.set tail-union.set))))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid code in P context" (unparse-recordized-code/sexp x)))))

;;; --------------------------------------------------------------------

  (module (E)

    (define (E x tail.set)
      ;;If we are in  "for effect" context: there is other code  in tail context that
      ;;has been processed before X by  the backwards code traversal.  Such tail code
      ;;has produced the live set TAIL.SET used as argument here.
      ;;
      (struct-case x
	((asm-instr op dst src)
	 (E-asm-instr op dst src tail.set x))

	((seq e0 e1)
	 (E e0 (E e1 tail.set)))

	((conditional test conseq altern)
	 (let ((conseq.set (E conseq tail.set))
	       (altern.set (E altern tail.set)))
	   (P test conseq.set altern.set (set-union conseq.set altern.set))))

	((non-tail-call unused.target unused.retval-var all-rand*)
	 (set-union (R* all-rand*) tail.set))

	((asmcall op arg*)
	 (case op
	   ((nop fl:single->double fl:double->single)
	    tail.set)
	   ((interrupt incr/zero?)
	    (or (exception-live-set)
		(compiler-internal-error __module_who__ __who__
		  "missing live set for SHORTCUT's handler while processing body")))
	   (else
	    (compiler-internal-error __module_who__ __who__
	      "invalid ASMCALL operator in E context" op))))

	((shortcut body handler)
	 (let ((handler.set (E handler tail.set)))
	   (parameterize ((exception-live-set handler.set))
	     (E body tail.set))))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid code in E context" (unparse-recordized-code/sexp x)))))

    (define (E-asm-instr op dst src tail.set x)
      (case op
	((move mref32)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr move   ?dst ?src)
	 ;;   (asm-instr mref32 ?dst (disp ?objref ?offset))
	 ;;
	 ;;here ?DST is written  and ?SRC is read: in the uplevel  code ?DST is dead,
	 ;;in the  tail code ?DST is  alive; in both  the uplevel code and  tail code
	 ;;?SRC is alive.
	 ;;
	 ;;Here ?DST changes  its status from live  to dead: it is the  moment to add
	 ;;?DST as  node of  the interference graph.   The interference  sub-graph of
	 ;;?DST is:
	 ;;
	 ;;   ?dst <--> (tail.set - ?dst)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + (tail.set - ?dst)
	 ;;
	 (let ((S (set-rem dst tail.set)))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   ?dst <--> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH dst live-loc))
	     S)
	   (set-union (R src) S)))

	((bref)
	 ;;We expect X ot have the format:
	 ;;
	 ;;   (asm-instr bref ?dst (disp ?objref ?offset))
	 ;;
	 ;;here ?DST  is written, ?OBJREF and  ?OFFSET are read: in  the uplevel code
	 ;;?DST is dead, in the tail code ?DST is alive; in both the uplevel code and
	 ;;tail code ?OBJREF and ?OFFSET are alive.
	 ;;
	 ;;While rewinding the  BODY traversal: ?DST changes its status  from live to
	 ;;dead, it is the moment to add ?DST as node of the interference graph.  The
	 ;;interference sub-graph of ?DST is:
	 ;;
	 ;;   ?dst <--> (tail.set - ?dst)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + (tail.set - ?dst)
	 ;;
	 #;(assert (disp? src))
	 (let ((S (set-rem dst tail.set)))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   ?dst <--> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH dst live-loc))
	     S)
	   ;;The destination of an 8-bit load cannot  be any register: it can be only
	   ;;a register among the ones that support 8-bit operations; for example EAX
	   ;;is fine, because we can use the 8-bit register AL as operand.  So, here,
	   ;;we add interference edges between DST  and all the CPU registers that do
	   ;;*not* support 8-bit operations; this makes sure that no such register is
	   ;;allocated to DST.
	   (when (var? dst)
	     (for-each (lambda (register)
			 (add-edge! THE-GRAPH dst register))
	       NON-8BIT-REGISTERS))
	   (set-union (R src) S)))

	((int-/overflow int+/overflow int*/overflow)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr int+/overflow ?dst ?src)
	 ;;   (asm-instr int-/overflow ?dst ?src)
	 ;;   (asm-instr int*/overflow ?dst ?src)
	 ;;
	 ;;and  these  instructions  might  jump  to the  handler  of  the  enclosing
	 ;;SHORTCUT; they will become the Intel Assembly instructions:
	 ;;
	 ;;   (addl ?src ?dst)
	 ;;   (jo ?handler-label)
	 ;;
	 ;;   (subl ?src ?dst)
	 ;;   (jo ?handler-label)
	 ;;
	 ;;   (imull ?src ?dst)
	 ;;   (jo ?handler-label)
	 ;;
	 ;;so ?DST is both  read and written and ?SRC is  read.  The continuation can
	 ;;be either the subsequent instructions  in the enclosing SHORTCUT's body or
	 ;;the enclosing SHORTCUT's interrupt handler.
	 ;;
	 ;;While rewinding the  BODY traversal: ?DST changes its status  from live to
	 ;;dead to  live, it is the  moment to add  ?DST as node of  the interference
	 ;;graph.  The interference sub-graph of ?DST is:
	 ;;
	 ;;   ?dst <--> (tail.set + handler.set - ?dst)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + (tail.set + handler.set)
	 ;;
	 (unless (exception-live-set)
	   (compiler-internal-error __module_who__ __who__
	     "missing live set for SHORTCUT's handler while processing body"))
	 (let ((S (set-rem dst (set-union tail.set (exception-live-set)))))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   ?dst -> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH dst live-loc))
	     S)
	   (set-union* (R src) (R dst) S)))

	((logand logor logxor sll sra srl int+ int- int* bswap! sll/overflow)
	 ;;We expect X to have the format:
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
	 ;;where ?DST is both read and written and ?SRC is read.
	 ;;
	 ;;While rewinding the  BODY traversal: ?DST changes its status  from live to
	 ;;dead to  live, it is the  moment to add  ?DST as node of  the interference
	 ;;graph.  The interference sub-graph of ?DST is:
	 ;;
	 ;;   ?dst <--> (tail.set - ?dst)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + (tail.set)
	 ;;
	 (let ((S (set-rem dst tail.set)))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   ?dst -> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH dst live-loc))
	     S)
	   (set-union (R src) tail.set)))

	((bset)
	 ;;We expect X ot have the format:
	 ;;
	 ;;   (asm-instr bref (disp ?objref ?offset) ?src)
	 ;;
	 ;;here  ?SRC is  read, the  machine word  referenced by  the DISP  struct is
	 ;;written but ?OBJREF and ?OFFSET themselves are read.
	 ;;
	 ;;There is no true node created here because no location is written, but see
	 ;;below.  The live set returned to the caller is:
	 ;;
	 ;;   ?src + ?objref + ?offset + (tail.set)
	 ;;
	 (assert (disp? dst))
	 ;;The source  of an 8-bit  store cannot  be any register:  it can be  only a
	 ;;register among the ones that support  8-bit operations; for example EAX is
	 ;;fine, because we can  use the 8-bit register AL as  operand.  So, here, we
	 ;;add interference edges between SRC and all the CPU registers that do *not*
	 ;;support  8-bit  operations; this  makes  sure  that  no such  register  is
	 ;;allocated to SRC.
	 (when (var? src)
	   (for-each (lambda (reg)
		       (add-edge! THE-GRAPH src reg))
	     NON-8BIT-REGISTERS))
	 (set-union* (R src) (R dst) tail.set))

	((cltd)
	 ;;NOTE  CLTD (Convert  Long To  Double)  is the  AT&T conventional  mnemonic
	 ;;corresponding to the Intel  conventional mnemonic CDQ (Convert Double-word
	 ;;to Quad-word); it takes the value of EAX and sign-extends it to EDX:EAX.
	 ;;
	 ;;Here we know that DST is the register EDX and SRC is the register EAX.  We
	 ;;know that CLTD and IDIV always come together.
	 (assert (eq? dst edx))
	 (assert (eq? src eax))
	 ;;We expect X ot have the format:
	 ;;
	 ;;   (asm-instr cltd EDX EAX)
	 ;;
	 ;;here EAX is read and EDX is written.
	 ;;
	 ;;The interference sub-graph of EDX is:
	 ;;
	 ;;   EDX <--> (tail.set - EDX)
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   EAX + (tail.set - EDX)
	 ;;
	 (let ((S (set-rem edx tail.set)))
	   ;;Extract the live locations from S and add interference edges:
	   ;;
	   ;;   EDX -> ?live-loc
	   ;;
	   (set-for-each (lambda (live-loc)
			   (add-edge! THE-GRAPH edx live-loc))
	     S)
	   (set-union (R eax) S)))

	((idiv)
	 ;;NOTE On  32-bit platforms, as  used by  Vicare, IDIV divides  (signed) the
	 ;;value EDX:EAX  (where EDX must contain  the sign-extension of EAX)  by the
	 ;;?SRC operand;  the results  are: EAX=quotient, EDX=remainder.   The source
	 ;;operand ?SRC can  be a general-purpose register or a  memory location.  As
	 ;;used by Vicare, the IDIV instruction always comes after a CLTD instruction
	 ;;that sign-extends the machine word in EAX to EDX:EAX.
	 ;;
	 ;;Here we know that DST is either  the register EAX or the register EDX; SRC
	 ;;is an operand, we do not know which  one here.  We know that CLTD and IDIV
	 ;;always come together.
	 (assert (or (eq? dst eax) (eq? dst edx)))
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr idiv EAX ?src)
	 ;;   (asm-instr idiv EDX ?src)
	 ;;
	 ;;here both  EAX and EDX are  written and read  and ?SRC is read.   The ?DST
	 ;;operand,  here, is  just a  place-holder and  it is  not used  to generate
	 ;;machine code.
	 ;;
	 ;;The interference sub-graphs of EAX and EDX are:
	 ;;
	 ;;   EAX <--> (tail.set - EAX - EDX)
	 ;;   EDX <--> (tail.set - EAX - EDX)
	 ;;
	 ;;and the live set returned to the caller is:
	 ;;
	 ;;   ?src + EAX + EDX + (tail.set)
	 ;;
	 (let ((S (set-rem eax (set-rem edx tail.set))))
	   (when (register? eax)
	     (set-for-each (lambda (live-loc)
			     (add-edge! THE-GRAPH eax live-loc)
			     (add-edge! THE-GRAPH edx live-loc))
	       S))
	   (set-union* (R eax) (R edx) (R src) S)))

	(( ;;some assembly instructions
	  mset			mset32
	  fl:load		fl:store
	  fl:add!		fl:sub!
	  fl:mul!		fl:div!
	  fl:from-int		fl:shuffle
	  fl:store-single	fl:load-single)
	 ;;We expect X to have the format:
	 ;;
	 ;;   (asm-instr mset     (disp ?objref ?offset) ?src)
	 ;;   (asm-instr mset32   (disp ?objref ?offset) ?src)
	 ;;
	 ;;   (asm-instr fl:load  ?flonum-operand ?offset)
	 ;;   (asm-instr fl:store ?flonum-operand ?offset)
	 ;;
	 ;;   (asm-instr fl:add!  ?flonum-operand ?offset)
	 ;;   (asm-instr fl:sub!  ?flonum-operand ?offset)
	 ;;   (asm-instr fl:mul!  ?flonum-operand ?offset)
	 ;;   (asm-instr fl:div!  ?flonum-operand ?offset)
	 ;;
	 ;;   (asm-instr fl:from-int  (KN 0) ?int-operand)
	 ;;   (asm-instr fl:shuffle   ?pointer ?offset)
	 ;;
	 ;;   (asm-instr fl:store-single  ?pointer ?offset)
	 ;;   (asm-instr fl:load-single   ?pointer ?offset)
	 ;;
	 ;;where ?FLONUM-OPERAND is a tagged pointer to Scheme object of type flonum.
	 ;;Here  both ?DST  and ?SRC  are read  and neither  stack locations  nor CPU
	 ;;registers are written; so no nodes are added to the graph.
	 ;;
	 ;;The live set returned to the caller is:
	 ;;
	 ;;   ?src + ?dst + (tail.set)
	 ;;
	 (set-union* (R src) (R dst) tail.set))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid ASM-INSTR operator in E context" (unparse-recordised-code/sexp x)))))

    #| end of module: E |# )

  (main body))


(module (%color-graph)
  (import LISTY-SET)
  (import GRAPHS)

  (define* (%color-graph spillable.set unspillable.set G)
    ;;Non-tail recursive function.
    ;;
    ;;The  argument SPILLABLE.SET  must be  a  set including  the VAR  structs that  are
    ;;spillable.  The argument  UNSPILLABLE.SET must be a set including  the VAR structs
    ;;that are unspillable.  The argument G is a graph.
    ;;
    ;;Return 3 values:
    ;;
    ;;1. A list of VAR structs representing the spillED local variables.
    ;;
    ;;2. A set holding the VAR structs representing the spillABLE local variables.
    ;;
    ;;3.  An  ENV  value:  an  possibly  empty  alist  whose  keys  are  VAR  structs
    ;;   representing  local variables in BODY,  and whose values are  the associated
    ;;   locations: FVAR structs or CPU register symbol names.
    ;;
    (cond ((and (empty-set? spillable.set)
		(empty-set? unspillable.set))
	   (values '() (make-empty-set) '()))

	  ((find-low-degree (set->list unspillable.set) G)
	   => (lambda (unspillable)
		(let ((neighbor* (node-neighbors unspillable G)))
		  (delete-node! unspillable G)
		  (receive (spilled* spillable.set env)
		      (%color-graph spillable.set (set-rem unspillable unspillable.set) G)
		    (let ((register (find-color unspillable neighbor* env)))
		      (values spilled*
			      spillable.set
			      (cons (cons unspillable register) env)))))))

	  ((find-low-degree (set->list spillable.set) G)
	   => (lambda (spillable)
		(let ((neighbor* (node-neighbors spillable G)))
		  (delete-node! spillable G)
		  (receive (spilled* spillable.set env)
		      (%color-graph (set-rem spillable spillable.set) unspillable.set G)
		    (let ((register (find-color spillable neighbor* env)))
		      (values spilled*
			      (set-add spillable spillable.set)
			      (cons (cons spillable register) env)))))))

	  ((pair? (set->list spillable.set))
	   (let* ((spillable (car (set->list spillable.set)))
		  (neighbor* (node-neighbors spillable G)))
	     (delete-node! spillable G)
	     (receive (spilled* spillable.set env)
		 (%color-graph (set-rem spillable spillable.set) unspillable.set G)
	       (let ((register (find-color/maybe spillable neighbor* env)))
		 (if register
		     (values spilled*
			     (set-add spillable spillable.set)
			     (cons (cons spillable register) env))
		   (values (cons spillable spilled*)
			   spillable.set
			   env))))))

	  (else
	   (compiler-internal-error __module_who__ __who__ "this should never happen"))))

  (define (find-low-degree ls G)
    (and (pair? ls)
	 (if (fx< (length (set->list (node-neighbors (car ls) G)))
		  (length ALL-REGISTERS))
	     (car ls)
	   (find-low-degree (cdr ls) G))))

  (define* (find-color x confs env)
    ;;The argument X is a VAR struct for which we want to allocate a register.
    ;;
    ;;The argument  ENV is  an alist  whose keys are  VAR structs  representing local
    ;;variables in BODY, and whose values  are the associated locations: FVAR structs
    ;;or CPU register symbol names.
    ;;
    (or (find-color/maybe x confs env)
	(compiler-internal-error __module_who__ __who__
	  "cannot find color local variable" x)))

  (module (find-color/maybe)

    (define (find-color/maybe x confs env)
      ;;The argument X is a VAR struct for which we want to allocate a register.
      ;;
      ;;The argument  ENV is an alist  whose keys are VAR  structs representing local
      ;;variables  in BODY,  and  whose  values are  the  associated locations:  FVAR
      ;;structs or CPU register symbol names.
      ;;
      (define available-register*
	;;Compose a  set containing all the  registers that are alive  here; then and
	;;remove them from  the set of all  the registers.  The result is  the set of
	;;registers that are *not* alive here, which can be allocated for to hold the
	;;VAR struct X.
	(let ((cr ($fold-right/stx (lambda (x knil)
				     (cond ((register? x)
					    (cons x knil))
					   ((assq x env)
					    => (lambda (entry)
						 (let ((loc (cdr entry)))
						   (if (register? loc)
						       (cons loc knil)
						     knil))))
					   (else
					    ;;If  we are  here:  X is  either a  FVAR
					    ;;struct or a VAR struct not in ENV.
					    (assert (or (var? x) (fvar? x)))
					    knil)))
		    '()
		    (set->list confs))))
	  (set->list (set-difference ALL-REGISTERS-SET (list->set cr)))))
      (and (pair? available-register*)
	   (car   available-register*)))

    (define-constant ALL-REGISTERS-SET
      (list->set ALL-REGISTERS))

    #| end of module |# )

  #| end of module: %COLOR-GRAPH |# )


(define* (%substitute-vars-with-associated-locations env body)
  ;;The argument BODY  must represent recordised code.  The argument  ENV is an alist
  ;;whose keys are VAR structs representing local variables in BODY, and whose values
  ;;are the associated locations: FVAR structs or CPU register symbol names.
  ;;
  ;;This function  builds and returns  a new struct instance  representing recordised
  ;;code, which is meant  to replace BODY.  The purpose of this  function is to apply
  ;;the  subfunction R  to  the operands  in  the structures  of  type ASM-INSTR  and
  ;;ASMCALL; as a consequence: we apply the subfunction R-var to all the VAR structs.
  ;;The subfunction R-var replaces its VAR argument with the associated location.
  ;;
  ;;A lot  of functions  are nested  here because  they make  use of  the subfunction
  ;;"R-var", and "R-var" needs to close upon the argument ENV.
  ;;
  (module (R)

    (define (R x)
      (struct-case x
	((constant)
	 x)
	((var)
	 (R-var x))
	((fvar)
	 x)
	((disp objref offset)
	 (%mk-disp (R-disp objref) (R-disp offset)))
	(else
	 (if (register? x)
	     x
	   (compiler-internal-error __module_who__  __who__
	     "invalid operand struct" x)))))

    (define (R-disp x)
      (struct-case x
	((constant)
	 x)
	((var)
	 (R-var x))
	((fvar)
	 x)
	(else
	 (if (register? x)
	     x
	   (compiler-internal-error __module_who__  __who__
	     "invalid DISP field value" x)))))

    (define (R-var x)
      (cond ((assq x env)
	     ;;This VAR struct  is in the list of spilled  variables: replace it with
	     ;;its associated FVAR struct.
      	     => cdr)
	    ;;This VAR struct is not in the  list of spilled variables: just leave it
	    ;;alone.
      	    (else x)))

    (module (%mk-disp)

      (define* (%mk-disp {objref %disp-objref?} {offset %disp-offset?})
	(make-disp objref offset))

      (define (%disp-objref? obj)
	(or (constant? obj)
	    (fvar?     obj)
	    (register? obj)
	    ;;This function  is called also  when not all  the VAR structs  have been
	    ;;mapped to a location.
	    (and (var? obj)
		 (not (var-loc obj)))))

      (define (%disp-offset? obj)
	(or (constant? obj)
	    (register? obj)
	    ;;This function  is called also  when not all  the VAR structs  have been
	    ;;mapped to a location.
	    (and (var? obj)
		 (not (var-loc obj)))))

      #| end of module: %mk-disp |# )

    #| end of module: R |# )

  (define (E x)
    ;;substitute effect
    (struct-case x
      ((seq e0 e1)
       (make-seq (E e0) (E e1)))
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (E e1) (E e2)))
      ((asm-instr op x v)
       (make-asm-instr op (R x) (R v)))
      ((asmcall op rands)
       (make-asmcall op (map R rands)))
      ((non-tail-call)
       x)
      ((shortcut body handler)
       (make-shortcut (E body) (E handler)))
      (else
       (compiler-internal-error __module_who__  __who__ "invalid effect" (unparse-recordized-code x)))))

  (define (P x)
    (struct-case x
      ((constant)
       x)
      ((asm-instr op x v)
       (make-asm-instr op (R x) (R v)))
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (P e1) (P e2)))
      ((seq e0 e1)
       (make-seq (E e0) (P e1)))
      ((shortcut body handler)
       (make-shortcut (P body) (P handler)))
      (else
       (compiler-internal-error __module_who__  __who__ "invalid pred" (unparse-recordized-code x)))))

  (define (T x)
    (struct-case x
      ((asmcall op)
       #;(assert (or (eq? op 'return) (eq? op 'direct-jump) (eq? op 'indirect-jump)))
       ;;We assume the input  code is correct; this means in  tail position there are
       ;;only ASMCALL structs with operand  RETURN, DIRECT-JUMP or INDIRECT-JUMP.  We
       ;;know that the operands of such ASMCALL structs are CPU register symbol names
       ;;and FVAR  structs; so there  are no VAR  structs to substitute  there.  Just
       ;;return X itself.
       x)
      ((conditional e0 e1 e2)
       (make-conditional (P e0) (T e1) (T e2)))
      ((seq e0 e1)
       (make-seq (E e0) (T e1)))
      ((shortcut body handler)
       (make-shortcut (T body) (T handler)))
      (else
       (compiler-internal-error __module_who__  __who__ "invalid tail" (unparse-recordized-code x)))))

  (T body))


;;;; done

#| end of module: color-by-chaitin |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'make-asmcall		'scheme-indent-function 1)
;; eval: (put 'make-conditional		'scheme-indent-function 2)
;; eval: (put 'struct-case		'scheme-indent-function 1)
;; eval: (put 'set-for-each		'scheme-indent-function 1)
;; End:
