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


;;;; Introduction
;;
;;NOTE This library is loaded by "makefile.sps" to build the boot image.
;;


#!vicare
(library (ikarus.compiler)
  (export
    current-primitive-locations		eval-core
    compile-core-expr-to-port		compile-core-expr
    core-expr->optimized-code		core-expr->assembly-code
    core-expr->optimisation-and-core-type-inference-code

    system-value			system-value-gensym

    ;; these go in (vicare compiler)
    optimize-level

    ;; configuration parameters
    current-letrec-pass
    check-for-illegal-letrec
    source-optimizer-passes-count
    perform-core-type-inference
    perform-unsafe-primrefs-introduction
    cp0-effort-limit
    cp0-size-limit
    strip-source-info
    generate-debug-calls
    check-compiler-pass-preconditions
    enabled-function-application-integration?
    generate-descriptive-labels?

    ;; middle pass inspection
    assembler-output
    optimizer-output

    compile-core-expr->code
    recordize
    optimize-direct-calls
    optimize-letrec
    source-optimize
    rewrite-references-and-assignments
    core-type-inference
    introduce-unsafe-primrefs
    sanitize-bindings
    optimize-for-direct-jumps
    insert-global-assignments
    introduce-vars
    introduce-closure-makers
    optimize-combinator-calls/lift-clambdas
    introduce-primitive-operation-calls
    rewrite-freevar-references
    insert-engine-checks
    insert-stack-overflow-check
    alt-cogen
    assemble-sources

    specify-representation
    impose-calling-convention/evaluation-order
    assign-frame-sizes
    color-by-chaitin
    flatten-codes

    unparse-recordized-code
    unparse-recordized-code/sexp
    unparse-recordized-code/pretty)
  ;;NOTE  This library  is  needed  to build  a  new boot  image.   Let's  try to  do
  ;;everything  here  *not*  loading  external  libraries.  Also,  let's  try  to  do
  ;;everything without  importing libraries in  the hierarchy (vicare system  --); we
  ;;should rely on the expander's type tagging and the compiler's core type inference
  ;;to introduce unsafe calls when appropriate.  (Marco Maggi; Fri Sep 19, 2014)
  (import (except (rnrs)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.system-value)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.scheme-objects-ontology)
    (ikarus.compiler.core-primitive-properties)
    (ikarus.compiler.unparse-recordised-code)
    (ikarus.compiler.pass-recordise)
    (ikarus.compiler.pass-optimize-direct-calls)
    (ikarus.compiler.pass-letrec-optimizer)
    (ikarus.compiler.pass-source-optimizer)
    (ikarus.compiler.pass-rewrite-references-and-assignments)
    (ikarus.compiler.pass-core-type-inference)
    (ikarus.compiler.pass-introduce-unsafe-primrefs)
    ;;When building a new  boot image: the FASL write library  is loaded from source.
    ;;This needs  to be  loaded here  so that  it evaluates  with the  freshly loaded
    ;;"ikarus.config.scm", including the correct value for WORDSIZE.
    (only (ikarus.fasl.write)
	  fasl-write)
    (only (ikarus.intel-assembler)
	  assemble-sources))

  (include "ikarus.wordsize.scm" #t)


;;;; mapping primitive symbol names to location gensyms

(define current-primitive-locations
  ;;Closure upon a function capable of  retrieving a core primitive's location gensym
  ;;given its symbol name.   Notice that this is not a  parameter because: whenever a
  ;;new  procedure is  set, some  initialisation must  be performed;  also: there  is
  ;;really no need to set this value with the features of the dynamic environment.
  ;;
  ;;The referenced function will allow this computation:
  ;;
  ;;   ((current-primitive-locations) 'display)
  ;;   => ?display-loc-gensym
  ;;
  ;;When  Vicare  is  running  normally: the  built-in  library  "(ikarus  primlocs)"
  ;;initialises  this parameter  to  a function  that queries  the  property list  of
  ;;symbols, looking for an entry having  SYSTEM-VALUE-GENSYM as key and the location
  ;;gensym  as  value (similarly  to  what  SYSTEM-VALUE  does, but  without  raising
  ;;exceptions).
  ;;
  ;;When  building  the  boot  image: the  program  "makefile.sps"  initialises  this
  ;;parameter  to a  function that  extracts  the loc  gensyms from  tables built  by
  ;;"makefile.sps" itself.
  ;;
  (let ((plocs (lambda (x) #f)))
    (case-lambda*
      (()
       plocs)
      (({p procedure?})
       (set! plocs p)
       (refresh-common-assembly-subroutines-cached-labels!)))))

(define* (primitive-public-function-name->location-gensym {name symbol?})
  ;;Given the  symbol NAME,  which must be  the public name  of a  primitive function
  ;;exported by the boot image (like "do-overflow", "display", "write", etc.), return
  ;;its associated location gensym.
  ;;
  ;;If the primitive is a procedure: the  location gensym has in both its "value" and
  ;;"proc" slots a reference to the closure object implementing the primitive.
  ;;
  ;;If the  primitive is  a non-procedure  variable: the location  gensym has  in its
  ;;"value" slot a reference to the actual Scheme object.
  ;;
  ;;NOTE We  have to  remember that the  location gensym of  a primitive  function is
  ;;generated by the expander while processing the boot image sources, and hard-coded
  ;;in the  boot image file  itself.  From that point  on: such location  gensyms are
  ;;universal constants.
  ;;
  (cond (((current-primitive-locations) name)
	 => (lambda (obj)
	      (if (symbol? obj)
		  obj
		(expression-return-value-violation __who__
		  "expected symbol as return value from CURRENT-PRIMITIVE-LOCATIONS procedure"
		  obj))))
	(else
	 (compiler-internal-error #f __who__
	   "while building boot image: primitive missing from makefile.sps" name))))


;;;; compiler entry point

(module COMPILER-SINGLE-ENTRY-POINT
  (compile-core-language-expression)

  (define compiler-initialised? #f)

  (define (initialise-compiler)
    (unless compiler-initialised?
      (initialise-core-primitive-properties)
      (set! compiler-initialised? #t)))

;;; --------------------------------------------------------------------

  (define (compile-core-language-expression core-language-sexp
					    perform-core-type-inference?
					    introduce-unsafe-primitives?
					    stop-after-optimisation?
					    stop-after-core-type-inference?
					    stop-after-assembly-generation?)
    ;;This is *the* commpiler function.  Transform a symbolic expression representing
    ;;a Scheme program in core language; return a code object.
    ;;
    (initialise-compiler)
    (%parse-compilation-options core-language-sexp
      (lambda (core-language-sexp)
	(let* ((p (recordize core-language-sexp))
	       (p (optimize-direct-calls p))
	       (p (optimize-letrec p))
	       (p (source-optimize p)))
	  (%print-optimiser-output p)
	  (let ((p (rewrite-references-and-assignments p)))
	    (if stop-after-optimisation?
		p
	      (let* ((p (if perform-core-type-inference?
			    (core-type-inference p)
			  p))
		     (p (if introduce-unsafe-primitives?
			    (introduce-unsafe-primrefs p)
			  p)))
		(if stop-after-core-type-inference?
		    p
		  (let* ((p (sanitize-bindings p))
			 (p (optimize-for-direct-jumps p))
			 (p (insert-global-assignments p))
			 (p (introduce-vars p))
			 (p (introduce-closure-makers p))
			 (p (optimize-combinator-calls/lift-clambdas p))
			 (p (introduce-primitive-operation-calls p))
			 (p (rewrite-freevar-references p))
			 (p (insert-engine-checks p))
			 (p (insert-stack-overflow-check p))
			 (code-object-sexp* (alt-cogen p)))
		    (%print-assembly code-object-sexp*)
		    (if stop-after-assembly-generation?
			code-object-sexp*
		      (let ((code* (assemble-sources thunk?-label code-object-sexp*)))
			;;CODE*  is a  list of  code objects;  the first  is the  one
			;;representing the initialisation  expression, the others are
			;;the ones representing the CLAMBDAs.
			;;
			;;The  initialisation   expression's  code   object  contains
			;;references to  all the CLAMBDA code  objects.  By returning
			;;the initialisation expression: we return the root of a tree
			;;hierarchy of code objects.  By recursively serialising from
			;;the root: we serialise all the code objects.
			(car code*))))))))))))

;;; --------------------------------------------------------------------

  (define* (thunk?-label x)
    ;;If X is a struct instance of  type CLOSURE-MAKER with no free variables: return
    ;;the associated label.
    ;;
    (and (closure-maker? x)
	 (if (null? (closure-maker-freevar* x))
	     (code-loc-label (closure-maker-code x))
	   (compiler-internal-error #f __who__ "non-thunk escaped" x))))

  (define (%parse-compilation-options core-language-sexp kont)
    ;;Parse  the given  core language  expression; extract  the optional  compilation
    ;;options  and the  body;  apply KONT  to  the body  in  the dynamic  environment
    ;;configured by the options.
    ;;
    ;;If the input expression selects compilation options, it has the format:
    ;;
    ;;   (with-compilation-options (?option ...) ?body)
    ;;
    ;;and we want to  apply KONT to the body; otherwise it is  a normal core language
    ;;expression.
    ;;
    ;;NOTE We  have to remember  that the CORE-LANGUAGE-SEXP may  not be a  pair, for
    ;;example:
    ;;
    ;;   (eval 123     (environment '(vicare)))
    ;;   (eval display (environment '(vicare)))
    ;;
    ;;will generate perfectly valid, non-pair, core language expressions.
    ;;
    (if (and (pair? core-language-sexp)
	     (eq? 'with-compilation-options (car core-language-sexp)))
	(let ((option* (cadr  core-language-sexp))
	      (body    (caddr core-language-sexp)))
	  (parametrise ((option.strict-r6rs (or (memq 'strict-r6rs option*)
						(option.strict-r6rs))))
	    (when (option.strict-r6rs)
	      (print-compiler-warning-message "enabling compiler's strict R6RS support"))
	    (kont body)))
      (kont core-language-sexp)))

;;; --------------------------------------------------------------------

  (define (%print-optimiser-output p)
    (when (optimizer-output)
      (pretty-print (unparse-recordized-code/pretty p) (current-error-port))))

;;; --------------------------------------------------------------------

  (module (%print-assembly)

    (define (%print-assembly code-object-sexp*)
      ;;Print nicely the assembly labels.
      ;;
      ;;CODE-OBJECT-SEXP* is a list of symbolic expressions:
      ;;
      ;;   (?code-object-sexp ...)
      ;;
      ;;each of which has the format:
      ;;
      ;;   (code-object-sexp
      ;;     (number-of-free-vars:	?num)
      ;;     (annotation:		?annotation)
      ;;     (label			?label)
      ;;     ?asm-instr-sexp ...)
      ;;
      (when (assembler-output)
	(parametrise ((gensym-prefix "L")
		      (print-gensym  #f))
	  (for-each (lambda (sexp)
		      (let ((port (current-error-port)))
			(newline port)
			(fprintf port "(code-object-sexp\n  (number-of-free-variables: ~a)\n  (annotation: ~s)\n"
				 (%sexp.number-of-free-vars sexp)
				 (%sexp.annotation          sexp))
			($for-each/stx %print-assembly-instr (%sexp.asm-instr-sexp* sexp))
			(fprintf port ")\n")))
	    code-object-sexp*))))

    (define (%sexp.number-of-free-vars sexp)
      ;;Given as argument a CODE-OBJECT-SEXP  symbolic expression: extract and return
      ;;the value of the NUMBER-OF-FREE-VARS: field.
      ;;
      (let ((field-sexp (cadr sexp)))
	(assert (eq? (car field-sexp) 'number-of-free-vars:))
	(cadr field-sexp)))

    (define (%sexp.annotation sexp)
      ;;Given as argument a CODE-OBJECT-SEXP  symbolic expression: extract and return
      ;;the value of the ANNOTATION: field.
      ;;
      (let ((field-sexp (caddr sexp)))
	(assert (eq? (car field-sexp) 'annotation:))
	(cadr field-sexp)))

    (define (%sexp.asm-instr-sexp* sexp)
      ;;Given as argument a CODE-OBJECT-SEXP  symbolic expression: extract and return
      ;;the  list of  Assembly  instructions.  We  know  that the  first  is a  label
      ;;definition.
      ;;
      (receive-and-return (asm-instr-sexp*)
	  (cdddr sexp)
	(assert (eq? 'label (caar asm-instr-sexp*)))))

    (define (%print-assembly-instr x)
      ;;Print  to the  current error  port the  symbolic expression  representing the
      ;;assembly  instruction X.   To be  used to  log generated  assembly for  human
      ;;inspection.
      ;;
      (if (and (pair? x)
	       (eq? (car x) 'seq))
	  ($for-each/stx %print-assembly-instr (cdr x))
	(let ((port (current-error-port)))
	  (display "   " port)
	  (write x port)
	  (newline port))))

    #| end of module: %PRINT-ASSEMBLY |# )

  #| end of module: COMPILER-SINGLE-ENTRY-POINT |# )


;;;; compiler public API

(define (eval-core x)
  ;;This function is used to compile  fully expanded R6RS programs, invoke libraries,
  ;;implement R6RS's eval function, compile right-hand sides of syntax definitions.
  ;;
  ((compile-core-expr x)))

(module (compile-core-expr-to-port
	 compile-core-expr
	 compile-core-expr->code
	 core-expr->optimized-code
	 core-expr->optimisation-and-core-type-inference-code
	 core-expr->assembly-code)
  (import COMPILER-SINGLE-ENTRY-POINT)

  (define (compile-core-expr-to-port expr port)
    ;;This function is used to write binary code into the boot image.
    ;;
    (fasl-write (compile-core-expr->code expr) port))

  (define (compile-core-expr x)
    ;;This function is used to compile  libraries' source code for serialisation into
    ;;FASL files.
    ;;
    (import (only (vicare system $codes)
		  $code->closure))
    (let ((code (compile-core-expr->code x)))
      ($code->closure code)))

  (define (compile-core-expr->code core-language-sexp)
    ;;Transform a core language symbolic expression into a code object.
    ;;
    (let* ((perform-core-type-inference?	(perform-core-type-inference))
	   (introduce-unsafe-primitives?	(and perform-core-type-inference? (perform-unsafe-primrefs-introduction)))
	   (stop-after-optimisation?		#f)
	   (stop-after-core-type-inference?	#f)
	   (stop-after-assembly-generation?	#f))
      (compile-core-language-expression core-language-sexp
					perform-core-type-inference?
					introduce-unsafe-primitives?
					stop-after-optimisation?
					stop-after-core-type-inference?
					stop-after-assembly-generation?)))

  (define (core-expr->optimized-code core-language-sexp)
    ;;This is a utility function used for debugging and inspection purposes; it is to
    ;;be used to inspect the result of optimisation.
    ;;
    (let* ((perform-core-type-inference?	(perform-core-type-inference))
	   (introduce-unsafe-primitives?	(and perform-core-type-inference? (perform-unsafe-primrefs-introduction)))
	   (stop-after-optimisation?		#t)
	   (stop-after-core-type-inference?	#t)
	   (stop-after-assembly-generation?	#f))
      (unparse-recordized-code/pretty
       (compile-core-language-expression core-language-sexp
					 perform-core-type-inference?
					 introduce-unsafe-primitives?
					 stop-after-optimisation?
					 stop-after-core-type-inference?
					 stop-after-assembly-generation?))))

  (define (core-expr->optimisation-and-core-type-inference-code core-language-sexp)
    ;;This is a utility function used for debugging and inspection purposes; it is to
    ;;be used to inspect the result of optimisation.
    ;;
    (let* ((perform-core-type-inference?	(perform-core-type-inference))
	   (introduce-unsafe-primitives?	(and perform-core-type-inference? (perform-unsafe-primrefs-introduction)))
	   (stop-after-optimisation?		#f)
	   (stop-after-core-type-inference?	#t)
	   (stop-after-assembly-generation?	#f))
      (unparse-recordized-code/pretty
       (compile-core-language-expression core-language-sexp
					 perform-core-type-inference?
					 introduce-unsafe-primitives?
					 stop-after-optimisation?
					 stop-after-core-type-inference?
					 stop-after-assembly-generation?))))

  (define (core-expr->assembly-code core-language-sexp)
    ;;This is  a utility  function used  for debugging  and inspection  purposes.  It
    ;;transforms  a symbolic  expression representing  core language  into a  list of
    ;;sublists, each sublist  representing assembly language instructions  for a code
    ;;object.
    ;;
    (let* ((perform-core-type-inference?	(perform-core-type-inference))
	   (introduce-unsafe-primitives?	(and perform-core-type-inference? (perform-unsafe-primrefs-introduction)))
	   (stop-after-optimisation?		#f)
	   (stop-after-core-type-inference?	#f)
	   (stop-after-assembly-generation?	#t))
      (compile-core-language-expression core-language-sexp
					perform-core-type-inference?
					introduce-unsafe-primitives?
					stop-after-optimisation?
					stop-after-core-type-inference?
					stop-after-assembly-generation?)))

  #| end of module: compile-core-expr |# )


(module (sanitize-bindings)
  ;;In this module  we want to make  sure that every CLAMBA struct  appears as direct
  ;;RHS expression for a FIX struct:
  ;;
  ;;   (fix ((?lhs ?clambda)) ?body)
  ;;
  ;;so:
  ;;
  ;;* CLAMBDA structs that already appear as RHS of FIX structs are left alone.
  ;;
  ;;* CLAMBDA structs appearing as RHS in  single binding BIND structs cause the BIND
  ;;  struct to be replaced by a FIX struct:
  ;;
  ;;     (bind ((?lhs ?clambda)) ?body) ==> (fix ((?lhs ?clambda)) ?body)
  ;;
  ;;* CLAMBDA  structs appearing as  RHS in multiple  binding BIND structs  cause the
  ;;  BIND  struct to  be split  into a  BIND struct and  a FIX  struct in  which the
  ;;  bindings are partitioned:
  ;;
  ;;     (bind ((?lhs0 ?clambda)
  ;;            (?lhs1 ?rhs))
  ;;       ?body)
  ;;     ==> (bind ((?lhs1 ?rhs))
  ;;           (fix ((?lhs0 ?clambda))
  ;;             ?body))
  ;;
  ;;* CLAMBDA structs  appearing as standalone expressions (that is:  not directly as
  ;;  RHS of a BIND or FIX struct) are "lifted" as follows:
  ;;
  ;;     (clambda (?formals ?body) ...)
  ;;     ==> (fix ((tmp (clambda (?formals ?body) ...)))
  ;;           tmp)
  ;;
  ;;After  this  pass is  complete:  all  the BIND  structs  have  a non-CLAMBDA  RHS
  ;;expression; all CLAMBDA structs appear as RHS of a FIX struct.
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		prelex		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		known
  ;;   forcall		funcall		typed-expr
  ;;
  ;;Examples
  ;;--------
  ;;
  ;;The form:
  ;;
  ;;   (bind ((a 123)
  ;;          (b (lambda (x) (this))))
  ;;     (that))
  ;;
  ;;is transformed into:
  ;;
  ;;   (bind ((a 123))
  ;;     (fix ((b (lambda (x) (this))))
  ;;       (that)))
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'sanitize-bindings))

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax sanitize-bindings))

  (define* (sanitize-bindings x)
    ;;Perform code transformation traversing the whole  hierarchy in X, which must be
    ;;a  struct instance  representing  recordized  code in  the  core language,  and
    ;;building  a new  hierarchy  of  transformed, recordized  code;  return the  new
    ;;hierarchy.
    ;;
    (struct-case x
      ((constant)
       x)

      ;;If we are performing this compiler  pass without first having performed "core
      ;;type inference":  there may be  TYPED-EXPR structs  in the input.   We remove
      ;;them.
      ((typed-expr expr)
       (E expr))

      ((prelex)
       x)

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (receive (fixable* bindable*)
	   (partition (lambda (x)
			(clambda? (cdr x)))
	     ($map/stx cons lhs* rhs*))
	 ;;FIXABLE* is  a list  of pairs  (?LHS . ?RHS)  in which  ?RHS is  a CLAMBDA
	 ;;struct.  BINDABLE*  is a  list of pairs  (?LHS .  ?RHS)  in which  ?RHS is
	 ;;*not* a CLAMBDA struct.
	 (%mk-bind ($map/stx car bindable*)
		   ($map/stx (lambda (bindable)
			       (E (cdr bindable)))
		     bindable*)
		   (E-fix ($map/stx car fixable*)
			  ($map/stx cdr fixable*)
			  body))))

      ((fix lhs* rhs* body)
       (E-fix lhs* rhs* body))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda)
       ;;This is a standalone CLAMBDA struct.
       (let ((tmp (make-prelex-for-tmp-binding)))
         (make-fix (list tmp) (list (E-clambda-rhs x)) tmp)))

      ((forcall op rand*)
       (make-forcall op ($map/stx E rand*)))

      ((funcall rator rand*)
       (make-funcall (E-known rator) ($map/stx E-known rand*)))

      (else
       (compile-time-error __module_who__ __who__
	 "invalid expression" (unparse-recordized-code x)))))

  (define (%mk-bind lhs* rhs* body)
    (if (null? lhs*)
	body
      (make-bind lhs* rhs* body)))

  (define (E-fix lhs* rhs* body)
    (if (null? lhs*)
        (E body)
      (make-fix lhs* ($map/stx E-clambda-rhs rhs*) (E body))))

  (define (E-clambda-rhs x)
    ;;The argument X  must be a struct  instance of type CLAMBDA  appearing as direct
    ;;RHS of a FIX struct.  The purpose of this function is to apply E to the body of
    ;;each CLAMBDA clause.
    ;;
    (struct-case x
      ((clambda label clause* cp freevar* name)
       (let ((clause*^ ($map/stx
			   (lambda (cls)
			     (struct-case cls
			       ((clambda-case info body)
				(struct-case info
				  ((case-info label fml* proper)
				   (let ((info^ (make-case-info label fml* proper)))
				     (make-clambda-case info^ (E body))))))))
			 clause*)))
	 (make-clambda label clause*^ cp freevar* name)))))

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

  #| end of module: sanitize-bindings |# )


(module (optimize-for-direct-jumps)
  ;;This  module transforms  FUNCALL structs  into  JMPCALL structs  whenever in  the
  ;;application form:
  ;;
  ;;   (funcall ?operator ?operand ...)
  ;;
  ;;the ?OPERATOR is a binding reference  known to reference a CLAMBDA struct.  There
  ;;is a technique that allows the  implementation of this "full closure object call"
  ;;as a faster "direct jump call" into the closure clause with the correct number of
  ;;arguments.
  ;;
  ;;As example, let's  consider the following code  in which the lambda  sexp has not
  ;;been integrated at the call site:
  ;;
  ;;   (let ((f (lambda (x) x)))
  ;;     (f 123))
  ;;
  ;;it is  known that F  references a  CLAMBDA, so the  application "(f 123)"  can be
  ;;implemented as direct jump call.  Another  example, when the CLAMBDA has multiple
  ;;clauses:
  ;;
  ;;   (let ((f (case-lambda
  ;;              ((x)   x)
  ;;              ((x y) (list x y)))))
  ;;     (f 1 2))
  ;;
  ;;it is known that  F references a CLAMBDA and that it is  called with 2 arguments:
  ;;there is technique that allows to implement the application "(f 1 2)" as a direct
  ;;jump to the clause with 2 arguments.
  ;;
  ;;Upon entering  this transformation: all  the CLAMBDA  structs must appear  in the
  ;;input as RHS  init expressions of FIX  structs; all the BIND structs  must have a
  ;;non-CLAMBDA struct as RHS init expression.
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		prelex		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		known
  ;;   forcall		funcall
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'optimize-for-direct-jumps))

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax optimize-for-direct-jumps))

  (define* (optimize-for-direct-jumps x)
    ;;Perform code optimisation traversing the whole  hierarchy in X, which must be a
    ;;struct instance representing recordized code in the core language, and building
    ;;a new hierarchy of optimised, recordized code; return the new hierarchy.
    ;;
    (struct-case x
      ((constant)
       x)

      ((prelex)
       x)

      ((primref)
       x)

      ((bind lhs* rhs* body)
       #;(assert (for-all (lambda (rhs) (not (clambda? rhs))) rhs*))
       (E-bind lhs* rhs* body))

      ((fix lhs* rhs* body)
       #;(assert (for-all (lambda (rhs) (clambda? rhs)) rhs*))
       ;;Here we  know that  RHS* is  a list of  CLAMBDA structs,  because it  is the
       ;;result  of  previous compiler  passes.   We  mark  each  PRELEX in  LHS*  as
       ;;referencing a CLAMBDA  struct, so that later they can  be used for jump-call
       ;;optimisation if they appear in operator position.
       ($for-each/stx $set-prelex-referenced-clambda! lhs* rhs*)
       (make-fix lhs* ($map/stx E-clambda rhs*) (E body)))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((forcall op rand*)
       (make-forcall op ($map/stx E rand*)))

      ((funcall rator rand*)
       (E-funcall x (E-known rator) ($map/stx E-known rand*)))

      (else
       (compile-time-error __module_who__ __who__
	 "invalid expression" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define (E-bind lhs* rhs* body)
    ;;Process LHS*  marking, when  appropriate, the PRELEX  structs as  referencing a
    ;;CLAMBDA struct,  so that  later the PRELEX  in LHS* can  be used  for jump-call
    ;;optimisation if they appear in operator position.
    ;;
    ;;Here we know  that RHS* is *not* a  list of CLAMBDA structs, because  it is the
    ;;result  of  previous  compiler passes;  so  we  try  to  determine if  the  RHS
    ;;expressions will return a CLAMBDA struct.
    ;;
    ;;By default  the PRELEX structs are  marked as *not* referencing  a CLAMBDA upon
    ;;creation,  so if  a PRELEX  does not  reference a  CLAMBDA here  we need  to do
    ;;nothing.
    ;;
    (let ((rhs*^ ($map/stx E rhs*)))
      ($for-each/stx
	  (lambda (lhs rhs)
	    (struct-case rhs
	      ((prelex)
	       (cond (($prelex-referenced-clambda rhs)
		      ;;RHS is  a PRELEX  struct referencing  a CLAMBDA  struct; this
		      ;;means LHS  references the same  CLAMBDA struct.  CLAM  is the
		      ;;referenced CLAMBDA struct.
		      => (lambda (clam)
			   ($set-prelex-referenced-clambda! lhs clam)))))
	      (else
	       ;;LHS does not reference a CLAMBDA struct.
	       (void))))
	lhs* rhs*^)
      (make-bind lhs* rhs*^ (E body))))

  (define (E-clambda x)
    ;;The argument X must be a struct  instance of type CLAMBDA.  The purpose of this
    ;;function is to apply E to the body of each CLAMBDA clause.
    ;;
    (struct-case x
      ((clambda label clause* cp freevar* name)
       (let ((clause*^ ($map/stx (lambda (clause)
				   (struct-case clause
				     ((clambda-case info body)
				      (make-clambda-case info (E body)))))
			 clause*)))
	 (make-clambda label clause*^ cp freevar* name)))))

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

;;; --------------------------------------------------------------------

  (module (E-funcall)

    (define (E-funcall appform rator rand*)
      ;;RATOR and RAND* have already been processed by E.
      ;;
      (let ((unwrapped-rator (%unwrap-known rator)))
	(cond
	 ;;Is UNWRAPPED-RATOR a prelex known to reference a closure?  In this case we
	 ;;can attempt an optimization.  CLAM is the referenced CLAMBDA.
	 ((and (prelex? unwrapped-rator)
	       ($prelex-referenced-clambda unwrapped-rator))
	  => (lambda (clam)
	       (%optimize-funcall appform clam unwrapped-rator rand*)))

	 ;;Is UNWRAPPED-RATOR the low level APPLY operation?  In this case: the first
	 ;;RAND* should be a struct  instance representing recordized code which will
	 ;;evaluate to a closure.
	 ;;
	 ;;$$APPLY is  used only  in the  body of the  procedure APPLY,  after having
	 ;;validated the  first argument as a  closure object; so, here,  we are sure
	 ;;that "(car rand*)" will evaluate to a closure object.
	 ((and (primref? unwrapped-rator)
	       (eq? ($primref-name unwrapped-rator) '$$apply))
	  ;;JMPCALL does not want KNOWN structs as rator and rands.
	  (make-jmpcall (sl-apply-label)
			(%unwrap-known (car rand*))
			($map/stx %unwrap-known (cdr rand*))))

	 ;;If  we are  here: UNWRAPPED-RATOR  is  just some  unknown struct  instance
	 ;;representing recordized code which, when  evaluated, will return a closure
	 ;;object.
	 (else
	  (make-funcall rator rand*)))))

    (define* (%optimize-funcall appform clam prelex-rator rand*)
      ;;Attempt to optimize the function application:
      ;;
      ;;   (PRELEX-RATOR . RAND*)
      ;;
      ;;CLAM is a struct instance of type CLAMBDA.  PRELEX-RATOR is a struct instance
      ;;of type PRELEX which  is known to reference the CLAMBDA in  CLAM.  RAND* is a
      ;;list of struct instances representing  recordized code which, when evaluated,
      ;;will return the operands for the function application.
      ;;
      ;;This function searches for a clause in CLAM which matches the arguments given
      ;;in RAND*:
      ;;
      ;;* If found: return a struct instance of type JMPCALL representing a jump call
      ;;  to the matching clause.
      ;;
      ;;* If not found: just return a  struct instance of type FUNCALL representing a
      ;;  normal function call.
      ;;
      (define num-of-rand* (length rand*))
      (let recur ((clause* ($clambda-cases clam)))
	(define-syntax-rule (%recur-to-next-clause)
	  (recur (cdr clause*)))
	(if (pair? clause*)
	    (struct-case ($clambda-case-info (car clause*))
	      ((case-info label fml* proper?)
	       (if proper?
		   ;;This clause has a fixed number of arguments.
		   (if (fx=? num-of-rand* (length fml*))
		       (make-jmpcall label prelex-rator ($map/stx %unwrap-known rand*))
		     (%recur-to-next-clause))
		 ;;This clause has a variable number of arguments.
		 (if (fx<=? (length (cdr fml*)) num-of-rand*)
		     (make-jmpcall label prelex-rator (%prepare-rand* (cdr fml*) rand*))
		   (%recur-to-next-clause)))))
	  ;;No matching clause found.
	  (if (option.strict-r6rs)
	      ;;Just call the closure as always.  A "wrong num args" exception will
	      ;;be raised at run-time as mandated by R6RS.
	      (begin
		(print-compiler-warning-message "wrong number of arguments in closure object application: ~a"
						(unparse-recordized-code/pretty appform))
		(make-funcall prelex-rator rand*))
	    (compile-time-arity-error __module_who__ __who__
	      "wrong number of arguments in closure object application"
	      (unparse-recordized-code/pretty appform))))))

    (define (%prepare-rand* fml* rand*)
      ;;Recursive function.
      ;;
      ;;FML* is a list of structs of type PRELEX representing the formal arguments of
      ;;the CASE-LAMBDA clause.
      ;;
      ;;RAND* is a  list of structs representing the arguments  to the closure object
      ;;application.
      ;;
      ;;This function  processes RAND* and  builds a new list  representing arguments
      ;;that can be assigned to the formals of the clause.  If the clause is:
      ;;
      ;;   ((a b . args) ?body)
      ;;
      ;;and RAND* is:
      ;;
      ;;   (#[constant 1] #[constant 2]
      ;;    #[constant 3] #[constant 4])
      ;;
      ;;this function must prepare a recordized list like:
      ;;
      ;;   (#[constant 1] #[constant 2]
      ;;    #[funcall #[primref list] (#[constant 3] #[constant 4])])
      ;;
      ;;so that the application:
      ;;
      ;;   (?rator ?rand0 ?rand1 ?rand2 ?rand3)
      ;;
      ;;is converted to:
      ;;
      ;;   (?rator ?rand1 ?rand2 (list ?rand3 ?rand4))
      ;;
      ;;where ?RAND1  and ?RAND2 are unwrapped  from KNOWN structs, while  ?RAND3 and
      ;;?RAND4 are not.
      ;;
      (if (pair? fml*)
	  (cons (%unwrap-known (car rand*))
		(%prepare-rand* (cdr fml*) (cdr rand*)))
	;;FIXME Construct list afterwards.  (Abdulaziz Ghuloum)
	(list (make-funcall (mk-primref 'list) rand*))))

    (define (%unwrap-known x)
      (struct-case x
	((known expr)
	 expr)
	(else x)))

    #| end of module: E-funcall |# )

  #| end of module: OPTIMIZE-FOR-DIRECT-JUMPS |# )


(module (insert-global-assignments)
  ;;This module  inserts global assignments forms  that put the value  of lexical top
  ;;level bindings in the slot "value"  of the corresponding loc gensym; for bindings
  ;;whose value is a  closure object: the value is also stored in  the "proc" slot of
  ;;the loc gensym.
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		prelex		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		known
  ;;   forcall		funcall		jmpcall
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'insert-global-assignments))

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax insert-global-assignments))

  (define* (insert-global-assignments x)
    ;;Perform code transformations traversing the whole hierarchy in X, which must be
    ;;a struct instance representing recordized code, and building a new hierarchy of
    ;;transformed, recordized code; return the new hierarchy.
    ;;
    (struct-case x
      ((constant)
       x)

      ((prelex)
       x)

      ((primref)
       x)

      ((bind lhs* rhs* body)
       #;(assert (for-all (lambda (rhs) (not (clambda? rhs))) rhs*))
       (make-bind lhs* ($map/stx E rhs*)
		  (%process-bind lhs* (E body))))

      ((fix lhs* rhs* body)
       #;(assert (for-all (lambda (rhs) (clambda? rhs)) rhs*))
       (make-fix lhs* ($map/stx E-clambda rhs*)
		 (%process-fix lhs* (E body))))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((forcall op rand*)
       (make-forcall op ($map/stx E rand*)))

      ((funcall rator rand*)
       (make-funcall (E-known rator) ($map/stx E-known rand*)))

      ((jmpcall label rator rand*)
       ;;JMPCALL's  rator and  rand* are  not,  by construction,  wrapped into  KNOWN
       ;;structs.
       (make-jmpcall label (E rator) ($map/stx E rand*)))

      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid expression" (unparse-recordized-code x)))))

  (define (E-clambda rhs)
    (struct-case rhs
      ((clambda label clause* cp freevar* name)
       ;;Apply E to every body of every CASE-LAMBDA clause.
       (let ((clause*^ ($map/stx (lambda (clause)
				   (struct-case clause
				     ((clambda-case info body)
				      (make-clambda-case info (E body)))))
			 clause*)))
	 (make-clambda label clause*^ cp freevar* name)))))

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

;;; --------------------------------------------------------------------

  (module (%process-bind %process-fix)

    (define-constant INIT-PRIMREF
      (mk-primref '$init-symbol-value!))

    (define-constant SET-PRIMREF
      (mk-primref '$set-symbol-value/proc!))

    (define (%process-bind lhs* body)
      ;;Prepend to the body of a BIND  struct a call to $INIT-SYMBOL-VALUE!  for each
      ;;of the PRELEX structs in LHS* representing top level bindings.
      ;;
      ;;$INIT-SYMBOL-VALUE! stores in the "value" field  of the loc gensym the actual
      ;;binding value; if, at run-time, such binding value is recognised as a closure
      ;;object: it is also stored in the "proc" field.
      ;;
      (%insert-assignments lhs* body INIT-PRIMREF))

    (define (%process-fix lhs* body)
      ;;Prepend to the  body of a FIX  struct a call to  $INIT-SYMBOL-VALUE! for each
      ;;PRELEX struct in  LHS* representing top level bindings;  for efficiency, only
      ;;for the first binding: the call is to $SET-SYMBOL-VALUE/PROC!.
      ;;
      ;;$INIT-SYMBOL-VALUE! stores in the "value" field  of the loc gensym the actual
      ;;binding value; if, at run-time, such binding value is recognised as a closure
      ;;object: it is also stored in the "proc" field.
      ;;
      ;;$SET-SYMBOL-VALUE/PROC!  stores in both the  "value" and "proc" fields of the
      ;;loc gensym  the actual binding value;  it is known at  compile-time that such
      ;;value is a closure object resulting from the evaluation of a CLAMBDA struct.
      ;;
      ;;FIXME   Why   in   hell   only   the  first   binding   can   be   set   with
      ;;$SET-SYMBOL-VALUE/PROC!   and  not  all  of   them?   I  have  tried  to  use
      ;;$SET-SYMBOL-VALUE/PROC! for all the bindings  and the result is some infinite
      ;;loop while compiling the boot image.  I do not understand.  (Marco Maggi; Sun
      ;;Aug 31, 2014)
      ;;
      (cond ((null? lhs*)
	     body)
	    ((prelex-global-location (car lhs*))
	     => (lambda (loc)
		  (make-seq (make-funcall SET-PRIMREF (list (make-constant loc) (car lhs*)))
			    (%insert-assignments (cdr lhs*) body INIT-PRIMREF))))
	    (else
	     (%process-fix (cdr lhs*) body))))

    (define (%insert-assignments lhs* body pref)
      ($fold-right/stx (lambda (lhs tail)
			 (cond ((prelex-global-location lhs)
				=> (lambda (loc)
				     (make-seq
				      (make-funcall pref (list (make-constant loc) lhs))
				      tail)))
			       (else tail)))
		       body lhs*))

    #| end of module |# )

  #| end of module: INSERT-GLOBAL-ASSIGNMENTS |# )


(module (introduce-vars)
  ;;This module replaces all the PRELEX  structs in recordised code with VAR structs;
  ;;this is  because from  now on  we need a  different set  of properties  to handle
  ;;variable bindings.
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		prelex		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		known
  ;;   funcall		forcall		jmpcall
  ;;
  ;;NOTE  This module  stores  generated VAR  structs  in the  field  OPERAND of  the
  ;;associated PRELEX structs.   We do not care about resetting  such field of PRELEX
  ;;structs, because in subsequent compiler passes the PRELEX structs will be no more
  ;;used: they will be garbage collected.
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'introduce-vars))

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax introduce-vars))

  (define* (introduce-vars x)
    ;;Perform code transformation traversing the whole  hierarchy in X, which must be
    ;;a  struct instance  representing  recordized  code in  the  core language,  and
    ;;building  a new  hierarchy  of  transformed, recordized  code;  return the  new
    ;;hierarchy.
    ;;
    (struct-case x
      ((constant)
       x)

      ((prelex)
       (%lookup-already-processed-prelex x))

      ((primref)
       x)

      ((bind lhs* rhs* body)
       #;(assert (for-all (lambda (rhs) (not (clambda? rhs))) rhs*))
       ;;Process the LHS* before everything else!
       (let ((lhs* ($map/stx %prelex->var lhs*)))
         (make-bind lhs* ($map/stx E rhs*) (E body))))

      ((fix lhs* rhs* body)
       #;(assert (for-all (lambda (rhs) (clambda? rhs)) rhs*))
       ;;Process the LHS* before everything else!
       (let ((lhs* ($map/stx %prelex->var lhs*)))
         (make-fix lhs* ($map/stx E-clambda lhs* rhs*) (E body))))

      ((conditional e0 e1 e2)
       (make-conditional (E e0) (E e1) (E e2)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((funcall rator rand*)
       (make-funcall (E-known rator) ($map/stx E-known rand*)))

      ((forcall rator rand*)
       (make-forcall rator ($map/stx E rand*)))

      ((jmpcall label rator rand*)
       ;;JMPCALL's  rator and  rand* are  not,  by construction,  wrapped into  KNOWN
       ;;structs.
       (make-jmpcall label (E rator) ($map/stx E rand*)))

      (else
       (compile-time-error __module_who__ __who__
	 "invalid expression" (unparse-recordized-code x)))))

  (define (E-clambda lhs rhs)
    ;;Process a FIX-defined binding.
    ;;
    ;;LHS is the struct instance of type VAR to which the function generated from RHS
    ;;will be bound.  This VAR struct is present in reference position in the body of
    ;;the  CLAMBDA clauses;  after  CLAMBDA  lifting, a  further  compiler pass  will
    ;;process these references.
    ;;
    (struct-case rhs
      ((clambda label clause* cp.unset freevar* name)
       (assert (not cp.unset))
       ;;The purpose of  this form is to  apply %PRELEX->VAR to all the  items in the
       ;;ARGS field of all the CASE-INFO structs.  Also we apply E to each body.
       (let ((cp       lhs)
	     (clause*^ ($map/stx (lambda (clause)
				   (struct-case clause
				     ((clambda-case info body)
				      (struct-case info
					((case-info label args proper)
					 ;;Process the LHS* before everything else!
					 (let ((info (make-case-info label
								     ($map/stx %prelex->var args)
								     proper)))
					   (make-clambda-case info (E body))))))))
			 clause*)))
	 (make-clambda label clause*^ cp freevar* name)))))

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

;;; --------------------------------------------------------------------

  (define (%lookup-already-processed-prelex prel)
    ;;Given a struct  instance of type PRELEX: return the  associated struct instance
    ;;of type VAR.   It is a very bad  error if this function finds a  PRELEX not yet
    ;;processed by %PRELEX->VAR.
    ;;
    (receive-and-return (V)
	($prelex-operand prel)
      (assert (var? V))))

  (define (%prelex->var prel)
    ;;Convert the PRELEX struct PREL into a VAR struct; return the VAR struct.
    ;;
    ;;The generated VAR struct is stored in the field OPERAND of the PRELEX, so that,
    ;;later, references to the PRELEX in  the recordized code can be substituted with
    ;;the VAR.
    ;;
    (assert (not (var? ($prelex-operand prel))))
    (receive-and-return (V)
	(make-unique-var ($prelex-name prel))
      ($set-var-global-location! V ($prelex-global-location    prel))
      ($set-prelex-operand! prel V)))

  #| end of module: INTRODUCE-VARS |# )


(module (introduce-closure-makers)
  ;;This  module  wraps  each  CLAMBDA  struct in  the  input  recordised  code  into
  ;;CLOSURE-MAKER structures, compiling  a list of free variables  referenced by each
  ;;CLAMBDA.  Each CLOSURE-MAKER struct represents  code that, evaluated at run-time,
  ;;will build and return a closure object.
  ;;
  ;;The *true*  purpose of this  compiler pass is to  gather lists of  free variables
  ;;referenced by CLAMBDA  bodies.  We might introduce the closure  makers at a later
  ;;pass and store the  lists of free variables in the  CLAMBDA structs; we introduce
  ;;the  closure  makers  here  because  it  helps  a  bit  in  reasoning  about  the
  ;;transformations.
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		var		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		known
  ;;   forcall		funcall		jmpcall
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'introduce-closure-makers))

  (define* (introduce-closure-makers X)
    ;;Perform code transformation traversing the whole  hierarchy in X, which must be
    ;;a  struct instance  representing  recordised  code in  the  core language,  and
    ;;building  a new  hierarchy  of  transformed, recordised  code;  return the  new
    ;;hierarchy.
    ;;
    (receive (X^ freevar*)
	(E X)
      (if (null? freevar*)
	  X^
	(compiler-internal-error __module_who__ __who__
	  "free vars encountered in program" (map unparse-recordized-code freevar*)))))

  (define* (E X)
    ;;Traverse  the recordized  code X  and return  2 values:  a new  recordized code
    ;;hierarchy, a list of VAR structs representing the free variables in X.
    ;;
    ;;The collected  freevar* are consumed by  E-clambda by storing them  in the free
    ;;field of the CLAMBDA struct.
    ;;
    (struct-case X
      ((constant)
       (values X '()))

      ((var)
       (values X (list X)))

      ((primref)
       (values X '()))

      ((bind lhs* rhs* body)
       ;;This is  a BIND struct,  so, assuming the  recordised input is  correct: the
       ;;RHS* are non-CLAMBDA structs;  the VARs in LHS* do *not*  appear in the RHS*
       ;;expressions.
       #;(assert (for-all (lambda (rhs) (not (clambda? rhs))) rhs*))
       (let-values
	   (((rhs*^ freevar*.rhs)  (E* rhs*))
	    ((body^ freevar*.body) (E  body)))
	 (values (make-bind lhs* rhs*^ body^)
		 ;;If  a VAR  struct is  a binding  in this  BIND: it  is not  a free
		 ;;variable; so remove it.
		 (union freevar*.rhs (difference freevar*.body lhs*)))))

      ((fix lhs* rhs* body)
       ;;This is a FIX struct, so, assuming the recordised input is correct: the RHS*
       ;;are CLAMBDA structs; the VARs in LHS* can appear in the RHS* expressions.
       #;(assert (for-all (lambda (rhs) (clambda? rhs)) rhs*))
       (let-values
	   (((rhs*^ freevar*.rhs)  (E-clambda* rhs*))
	    ((body^ freevar*.body) (E body)))
	 ;;Here RHS*^ is a list of CLOSURE-MAKER structs.
	 (values (make-fix lhs* rhs*^ body^)
		 ;;If  a VAR  struct is  a binding  in  this FIX:  it is  not a  free
		 ;;variable; so remove it.
		 (difference (union freevar*.body freevar*.rhs) lhs*))))

      ((conditional test conseq altern)
       (let-values
	   (((test^   freevar*.test)   (E test))
	    ((conseq^ freevar*.conseq) (E conseq))
	    ((altern^ freevar*.altern) (E altern)))
         (values (make-conditional test^ conseq^ altern^)
                 (union freevar*.test (union freevar*.conseq freevar*.altern)))))

      ((seq e0 e1)
       (let-values
	   (((e0^ freevar*.e0) (E e0))
	    ((e1^ freevar*.e1) (E e1)))
         (values (make-seq e0^ e1^) (union freevar*.e0 freevar*.e1))))

      ((forcall op rand*)
       (receive (rand*^ freevar*.rand*)
	   (E* rand*)
         (values (make-forcall op rand*^) freevar*.rand*)))

      ((funcall rator rand*)
       (let-values
	   (((rator^ freevar*.rator) (E-known  rator))
	    ((rand*^ freevar*.rand*) (E-known* rand*)))
         (values (make-funcall rator^ rand*^) (union freevar*.rator freevar*.rand*))))

      ((jmpcall label rator rand*)
       ;;JMPCALL's  rator and  rand* are  not,  by construction,  wrapped into  KNOWN
       ;;structs.
       (let-values
	   (((rator^ freevar*.rator) (E  rator))
	    ((rand*^ freevar*.rand*) (E* rand*)))
         (values (make-jmpcall label rator^ rand*^)
                 (union freevar*.rator freevar*.rand*))))

      (else
       (compile-time-error __module_who__ __who__
	 "invalid expression" X))))

;;; --------------------------------------------------------------------

  (define (E* X*)
    ;;Map  E over  each element  of X*,  which  must be  a list  of struct  instances
    ;;representing recordized code; return 2 values:  the processed X*, a list of VAR
    ;;structs representing the free variables referenced by X*.
    ;;
    (if (pair? X*)
	(let-values
	    (((a freevar*.a) (E  (car X*)))
	     ((d freevar*.d) (E* (cdr X*))))
	  (values (cons a d) (union freevar*.a freevar*.d)))
      (values '() '())))

  (define (E-known x)
    ;;Apply E  to X, which  must be a  struct instance representing  recordized code;
    ;;return 2 values: the  processed X, a list of VAR  structs representing the free
    ;;variables referenced by X.
    ;;
    (struct-case x
      ((known expr type)
       (receive (expr^ freevar*)
	   (E expr)
         (values (make-known expr^ type) freevar*)))
      (else
       (E x))))

  (define (E-known* X*)
    ;;Map  E-known  over  each element  of  X*,  which  must  be a  list  of  structs
    ;;representing  code; return  2  values: the  processed X*,  a  list VAR  structs
    ;;representing the free variables referenced by X*.
    ;;
    (if (pair? X*)
	(let-values
	    (((a freevar*.a) (E-known  (car X*)))
	     ((d freevar*.d) (E-known* (cdr X*))))
	  (values (cons a d) (union freevar*.a freevar*.d)))
      (values '() '())))

;;; --------------------------------------------------------------------

  (module (E-clambda*)

    (define (E-clambda* rhs*)
      ;;Non-tail recursive  function.  Apply  "E-clambda" to  every CLAMBDA  in RHS*.
      ;;Return 2 values:
      ;;
      ;;1. A list of CLOSURE-MAKER structs which must replace the original RHS*.
      ;;
      ;;2. A  list of VAR structs  representing the free variables  referenced by the
      ;;   CLOSURE-MAKER structs.
      ;;
      (if (pair? rhs*)
	  (let-values
	      (((a freevar*.a) (E-clambda  (car rhs*)))
	       ((d freevar*.d) (E-clambda* (cdr rhs*))))
	    (values (cons a d) (union freevar*.a freevar*.d)))
	(values '() '())))

    (define (E-clambda rhs)
      ;;Build a struct instance of type CLOSURE-MAKER which must replace the original
      ;;RHS.   Return 2  values:  the CLOSURE-MAKER  struct, a  list  of VAR  structs
      ;;representing the free variables referenced by the CLOSURE-MAKER.
      ;;
      ;;NOTE The free  variables collected here are stored in  the CLOSURE-MAKER, but
      ;;not in the new CLAMBDA; further compiler passes will process the freevars* in
      ;;the CLOSURE-MAKER,  performing a  cleanup to  determine the  actual freevars*
      ;;that will end in the CLAMBDA.
      ;;
      (struct-case rhs
	((clambda label clause* cp freevar*.unset name)
	 #;(assert (not freevar*.unset))
	 (receive (clause*^ freevar*)
	     (E-clambda-case* clause*)
	   (values (let ((clam (make-clambda label clause*^ cp freevar*.unset name)))
		     (make-closure-maker clam freevar*))
		   freevar*)))))

    (define (E-clambda-case* clause*)
      ;;Non-tail recursive function.   Process all the clauses in  CLAUSE* which must
      ;;be a list of CLAMBDA-CASE structs.  Return 2 values:
      ;;
      ;;1. A list of CLAMBDA-CASE structs which must replace the original CLAUSE*.
      ;;
      ;;2. A  list of VAR structs  representing the free variables  referenced by the
      ;;   bodies of the clauses.
      ;;
      (if (pair? clause*)
	  (struct-case (car clause*)
	    ((clambda-case info body)
	     (let-values
		 (((body^    freevar*.body)    (E body))
		  ((clause*^ freevar*.clause*) (E-clambda-case* (cdr clause*))))
	       (values (cons (make-clambda-case info body^) clause*^)
		       ;;If a  VAR struct is  a clause's formal  argument: it is  not a
		       ;;free variable; so remove it.
		       (union (difference freevar*.body (case-info-args info))
			      freevar*.clause*)))))
	(values '() '())))

    #| end of module: do-clambda* |# )

  #| end of module: convert closures |# )


(module (optimize-combinator-calls/lift-clambdas)
  ;;This  module performs  CLAMBDA lifting  and  optimisation of  calls to  functions
  ;;having no free variables (combinators).
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		var		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		closure-maker
  ;;   forcall		funcall		jmpcall
  ;;   known
  ;;
  ;;NOTE This module  makes use of the field  "index" of structs of type  VAR used to
  ;;reference bindings.   The value of such  fields from previous compiler  passes is
  ;;ignored, because the fields are reset to #f before being used in this module.
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'optimize-combinator-calls/lift-clambdas))

  (define (optimize-combinator-calls/lift-clambdas X)
    ;;Perform code transformation traversing the whole  hierarchy in X, which must be
    ;;a  struct instance  representing  recordised  code in  the  core language,  and
    ;;building  a new  hierarchy  of  transformed, recordised  code;  return a  CODES
    ;;struct.
    ;;
    (parametrise ((all-clambdas '()))
      ;;First apply E to X...
      (let ((X^ (E X)))
	;;... then gather ALL-CLAMBDAS.
	(make-codes (all-clambdas) X^))))

  (module (E)

    (define* (E x)
      (struct-case x
	((constant)
	 x)

	((var)
	 ;;X is a VAR  struct.  If this VAR is a node in  the graph of substitutions:
	 ;;start  a  visit to  the  graph  starting at  X  and  return the  resulting
	 ;;substitution.   The  result  can  be:  X itself,  another  VAR  struct,  a
	 ;;CLOSURE-MAKER struct.
	 (%find-var-substitution! x))

	((primref)
	 x)

	((bind lhs* rhs* body)
	 #;(assert (for-all (lambda (rhs) (and (not (clambda? rhs)) (not (closure-maker? rhs)))) rhs*))
	 ;;Clear the field "index" of the VAR structs in LHS* from whatever value the
	 ;;previous compiler passes have left in.
	 ($for-each/stx %var-reset-subst! lhs*)
	 (E-bind lhs* rhs* body))

	((fix lhs* rhs* body)
	 #;(assert (for-all (lambda (rhs) (closure-maker? rhs)) rhs*))
	 ;;Clear the field "index" of the VAR structs in LHS* from whatever value the
	 ;;previous compiler passes have left in.
	 ($for-each/stx %var-reset-node! lhs*)
	 (E-fix lhs* rhs* body))

	((conditional test conseq altern)
	 (make-conditional (E test) (E conseq) (E altern)))

	((seq e0 e1)
	 (make-seq (E e0) (E e1)))

	((forcall op rand*)
	 (make-forcall op ($map/stx E rand*)))

	((funcall rator rand*)
	 (make-funcall (E-known rator) ($map/stx E-known rand*)))

	((jmpcall label rator rand*)
	 ;;JMPCALL's rator  and rand*  are not, by  construction, wrapped  into KNOWN
	 ;;structs.
	 (make-jmpcall label (E rator) ($map/stx E rand*)))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid expression" (unparse-recordized-code x)))))

    (define (E-bind lhs* rhs* body)
      ;;Bindings defined by BIND have left-hand side VAR structs; such VARs:
      ;;
      ;;* Are without substitution.
      ;;
      ;;* Have another VAR struct as substitution.
      ;;
      ;;* Have a CLOSURE-MAKER struct as substitution.
      ;;
      (let ((rhs*^ ($map/stx E rhs*)))
	;;Make sure  that each LHS*  has the  same substitution of  the corresponding
	;;RHS*^:
	;;
	;;* If RHS^ is a VAR struct with non-false substitution object in its "index"
	;;  field: store such  object in the "index" field of LHS,  so that they have
	;;  the same substitution.
	;;
	;;* Otherwise RHS is not a VAR  struct or it has no substitution: store false
	;;  in the LHS "index" field, so that LHS also has no substitution.
	;;
	;;For example, given:
	;;
	;;   (bind ((a b))
	;;     ?body)
	;;
	;;we want the VAR structs A and B to have the same substitution.
	($for-each/stx (lambda (lhs rhs^)
			 (%var-set-subst! lhs (and (var? rhs^)
						   (%var-get-subst rhs^))))
	  lhs* rhs*^)
	(let ((body^ (E body)))
	  ;;Once the body has been processed: we do not need the substitutions in the
	  ;;LHS* anymore, so reset them.
	  ($for-each/stx %var-reset-subst! lhs*)
	  (make-bind lhs* rhs*^ body^))))

    (define (E-known x)
      (struct-case x
	((known expr type)
	 (make-known (E expr) type))
	(else
	 (E x))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (define all-clambdas
    ;;While processing  an input expression:  this parameter  holds a proper  list of
    ;;CLAMBDA structs representing  the set of all the CLAMBDAs  defined in the input
    ;;expression.  These CLAMBDA structs will be compiled to machine code and used to
    ;;build the code objects.
    ;;
    (make-parameter #f))

  (define-syntax-rule (%gather-clambda! ?obj)
    (all-clambdas (cons ?obj (all-clambdas))))

;;; --------------------------------------------------------------------

  (module (E-fix node?)

    (define (E-fix lhs* rhs* body)
      ;;Here we know that RHS* is a list of CLOSURE-MAKER structs in which the VAR in
      ;;LHS* might  appear.  Here the  VAR structs  in LHS* are  not yet part  of the
      ;;graph of substitutions.
      ;;
      ;;In the dynamic extent of this function  we create a directed graph having the
      ;;VAR structs in LHS*  as nodes; such graph contains only  such VAR structs and
      ;;it is  destroyed before  returning.  The  graph is  used to  determine which,
      ;;among  the   CLAMBDA  structs  in   RHS*,  is   a  combinator  and   which  a
      ;;non-combinator.
      ;;
      (define-constant extern-clean-freevar**
	;;We  clean up  the  lists  of free  variables  performing the  substitutions
	;;defined for the outer bindings:
	;;
	;;   (bind ((?lhs1 ?rhs1) ...)    ;clean for these bindings
	;;     (fix ((?lhs2 ?rhs2) ...))  ;clean for these bindings
	;;       (fix ((?lhs ?rhs) ...)   ;this is the FIX we are processing
	;;         ?body))
	;;
	;;We also  remove VAR self references  in recursive functions because  a self
	;;reference (by itself) does not cause  a function to be a non-combinator; we
	;;do not want self-loop edges in the graph.
	($map/stx %filter-and-substitute-binding-freevars lhs* rhs*))
      ;;Now we have  to clean and substitute the free  variables referencing bindings
      ;;defined in  this very  FIX.  Among  the VAR  structs in  LHS* that  appear in
      ;;EXTERN-CLEAN-FREEVAR**: we can remove those referencing a combinator, we must
      ;;include those referencing a non-combinator.
      ;;
      ;;Build a NODE struct  for every binding defined by this  FIX; the NODE structs
      ;;start with  an empty  list of free  variables and an  empty list  of outgoing
      ;;edges.
      (define-constant node*
	($map/stx (lambda (lhs rhs)
		    (receive-and-return (N)
			(mk-node lhs ($closure-maker-code rhs))
		      (%var-set-node! lhs N)))
	  lhs* rhs*))
      ;;Build a (possibly cyclic) directed  graph representing the dependencies among
      ;;the NODE of this FIX.  Example:
      ;;
      ;;   (fix ((A (closure-maker ?clambda no-freevars))
      ;;         (B (closure-maker ?clambda (freevars: A)))
      ;;         (C (closure-maker ?clambda (freevars: A B))))
      ;;     ?body)
      ;;
      ;;is represented with the graph:
      ;;
      ;;   node[A] ---> node[B]
      ;;     |            |
      ;;     |            v
      ;;      --------> node[C]
      ;;
      ;;the outgoing links are stored in the list DEPS of each node; for this example
      ;;it means:
      ;;
      ;;   node[A].deps := (node[B] node[C])
      ;;   node[B].deps := (node[C])
      ;;   node[C].deps := ()
      ;;
      ;;which in turn means:
      ;;
      ;;* If A is a non-combinator: both B and C are non-combinators.
      ;;
      ;;* If B is a non-combinator: C is a non-combinator.
      ;;
      ;;* If C is a non-combinator: fine.
      ;;
      ;;In the  following loop: we  add edges  among nodes representing  citations in
      ;;free variables list.   We begin the iteration with each  NODE having an empty
      ;;list  of free  variables; we  end  the iteration  with each  NODE having  the
      ;;external free variables added to the lists.
      ($for-each/stx
	  (lambda (this-node extern-clean-freevar*)
	    ($for-each/stx
		(lambda (freevar)
		  (cond ((%var-get-node freevar)
			 ;;The FREEVAR references a binding created by this very FIX;
			 ;;its associated NODE is  PREDECESSOR-NODE.  We register the
			 ;;dependency among nodes; but we leave FREEVAR out for now.
			 => (lambda (predecessor-node)
			      (node-add-edge-from/to! predecessor-node this-node)))
			(else
			 ;;The FREEVAR references a  binding created by some external
			 ;;binding form.  This FREEVAR does  not create an edge among
			 ;;nodes;  this  FREEVAR  is  inclued in  the  list  of  free
			 ;;variables of this node.
			 (node-push-freevar! this-node freevar))))
	      extern-clean-freevar*))
	node* extern-clean-freevar**)
      ;;Now  we need  to add  to  each NODE  its free  variables referencing  binding
      ;;defined by this  FIX, but only if the added  VAR references a non-combinator;
      ;;VARs referencing a combinator are left out.
      ;;
      ;;We perform a depth-first visit of the graph of NODE structs, visiting all the
      ;;nodes that  have not already  been marked as  "done" and following  the paths
      ;;formed by outgoing edges.
      ;;
      ;;*  If a  NODE has  empty list  of free  variables: we  leave it  alone, *not*
      ;;  marking it as done.  This node  is a non-combinator only if it references a
      ;;  non-combinator.  We might come back to it later following another path.
      ;;
      ;;*  If  the visited  NODE  has  non-empty list  of  free  variables: it  is  a
      ;;   non-combinator; we  visit all  its  successors, adding  the free  variable
      ;;   referencing the  visited  node to  the  list of  its  successors: all  the
      ;;  successors a non-cobinators.
      ;;
      (letrec ((%depth-first-visit
		(lambda (visited-node)
		  ;;Non-tail recursive function.
		  (unless (or ($node-done? visited-node)
			      (null? ($node-freevar* visited-node)))
		    ($set-node-done?! visited-node #t)
		    ;;If we are here, in the graph of nodes we have:
		    ;;
		    ;;   visited-node ---> successor-node
		    ;;
		    ;;we know  that VISITED-NODE  represents a non-combinator  and we
		    ;;know  that  VISITED-NODE  was  originally in  the  freevar*  of
		    ;;SUCCESSOR-NODE.  So we must put VISITED-NODE in the freevar* of
		    ;;SUCCESSOR-NODE    to    make   SUCCESSOR-NODE    represent    a
		    ;;non-combinator too.
		    ($for-each/stx (lambda (successor-node)
				     (node-push-freevar! successor-node ($node-var visited-node))
				     (%depth-first-visit successor-node))
		      ($node-deps visited-node))))))
	($for-each/stx %depth-first-visit node*))
      ;;Here we scan  the list of NODEs  and convert it into a  list of CLOSURE-MAKER
      ;;structs that will be candidate for inclusion  in the output FIX struct.  If a
      ;;NODE arrives here with no free  variables: it represents a combinator and its
      ;;VAR can  be included in  the graph of substitution.   If a NODE  arrives here
      ;;with some free  variables: it represents a non-combinator and  its VAR has no
      ;;substitution.
      ;;
      ;;If  a  binding   is  defined  by  a  FIX:  either   it  has  no  substitution
      ;;(non-combinator) or it has its CLOSURE-MAKER as substitution (combinator).
      (let ((new-rhs* ($map/stx
			  (lambda (node)
			    ;;NOTE  Upon  entering  this function  extent:  the  NODE
			    ;;struct  is  stored in  the  "index"  field of  its  VAR
			    ;;struct.  Here  we either reset  the field to  false, or
			    ;;replace   the   NODE   with  a   proper   CLOSURE-MAKER
			    ;;substitution.
			    (let ((true-freevar* ($node-freevar* node)))
			      (receive-and-return (clmaker)
				  ;;Make the new CLOSURE-MAKER using the list of free
				  ;;variables we have cleaned before.
				  (make-closure-maker ($node-clambda node) true-freevar*)
				;;Is the  binding of CLOSURE-MAKER to  be included in
				;;the graph of substitutions?
				(let ((lhs ($node-var node)))
				  (if (null? true-freevar*)
				      ;;This   CLOSURE-MAKER  struct   has  no   free
				      ;;variables:  it  will   return  a  combinator.
				      ;;Let's add it to the graph of substitutions.
				      (%var-reset-node/set-subst! lhs clmaker)
				    ;;This   CLOSURE-MAKER  struct   has  true,   not
				    ;;removable,  free variables:  it  will return  a
				    ;;non-combinator.  Let's  leave it  alone without
				    ;;substitution.
				    (%var-reset-node! lhs))))))
			node*)))
	;;Done with setting up substitutions.  Now  we perform the lambda lifting: we
	;;replace  the  CLAMBDA  in  the CLOSURE-MAKER  structs  with  an  associated
	;;CODE-LOC struct  and enqueue the  CLAMBDA in the  list of all  the CLAMBDAs
	;;defined by the input expression.  Before:
	;;
	;;   (fix ((?lhs (closure-maker ?clambda)) ...)
	;;     ?body)
	;;
	;;after:
	;;
	;;   (fix ((?lhs (closure-maker (code-loc ?asmlabel)) ...))
	;;     ?body)
	;;
	;;   all-clambdas := (?clambda ...)
	;;
	($for-each/stx %lift-clambda! new-rhs*)
	;;Process  the BODY  substituting  VAR structs  from  LHS* when  appropriate.
	;;Build and return the output FIX struct including only the non-combinators.
	(%mk-fix '() '() lhs* new-rhs* (E body))))

;;; --------------------------------------------------------------------

    (define-struct node
      ;;Each binding defined by a FIX has a NODE associated to it.
      ;;
      (var
		;The VAR struct appearing as LHS in this binding's definition.
       clambda
		;The CLAMBDA struct appearing as RHS in this binding's definition.
       deps
		;Null or  a list of successor  NODE structs; the list  represents the
		;destination nodes of  outgoing edges.  If a node is  listed in DEPS:
		;it has a free variable referencing this node.
       done?
		;Boolean.  If  true: this NODE  struct has already been  processed to
		;establish its non-combinator dependencies.
       freevar*
		;Null or  a list of  VAR structs  representing free variables  in the
		;CLAMBDA.  This  list is progressively  built to include  the "true",
		;non-removable, substituted free variables from the original list.
       ))

    (define (mk-node lhs code)
      (make-node lhs code '() #f '()))

    (define (node-push-freevar! node freevar)
      ($set-node-freevar*! node (cons freevar ($node-freevar* node))))

    (define (node-add-edge-from/to! node dep)
      ($set-node-deps! node (cons dep ($node-deps node))))

;;; --------------------------------------------------------------------

    (define (%mk-fix output-lhs* output-rhs* input-lhs* input-rhs* body)
      ;;Tail-recursive function.  Build and return the output FIX struct.  Of all the
      ;;bindings described by  INPUT-LHS* and INPUT-RHS*: only  those whose INPUT-LHS
      ;;has  no substitutions  will  be included  in  the output;  if  a binding  has
      ;;INPUT-LHS with substitution it is filtered out.
      ;;
      (cond ((pair? input-lhs*)
	     (let ((input-lhs (car input-lhs*))
		   (input-rhs (car input-rhs*)))
	       (if (%var-get-subst input-lhs)
		   (begin
		     ;;This INPUT-LHS  has a substitution,  it is a  combinator: skip
		     ;;its binding.
		     (%var-reset-subst! input-lhs)
		     (%mk-fix output-lhs* output-rhs*
			      (cdr input-lhs*) (cdr input-rhs*)
			      body))
		 ;;This LHS has no substitution,  it is a non-combinator: include its
		 ;;binding.
		 (%mk-fix (cons input-lhs output-lhs*) (cons input-rhs output-rhs*)
			  (cdr input-lhs*) (cdr input-rhs*)
			  body))))
	    ((pair? output-lhs*)
	     (make-fix output-lhs* output-rhs* body))
	    (else body)))

    (module (%filter-and-substitute-binding-freevars)

      (define (%filter-and-substitute-binding-freevars self-var clmaker)
	;;Build  and  return a  list  of  VAR  structs  representing the  "true  free
	;;variables" of the CLOSURE-MAKER struct in  CLMAKER.  Taken the list of free
	;;variables from CLMAKER:
	;;
	;;*  Include   the  VAR  structs   having  no  substitution   (references  to
	;;  non-combinators and non-removable external bindings).
	;;
	;;* Remove  the VAR structs that  have a CLOSURE-MAKER struct  as replacement
	;;  (references to combinators).
	;;
	;;*  Substitute the  VAR  structs  having a  VAR  substitution  with the  VAR
	;;  substitution itself (references to special external bindings).
	;;
	;;* If  SELF-VAR is a  VAR struct and  it is in  the list of  free variables:
	;;  remove  it.  This is  meant to  remove self references  for CLOSURE-MAKER
	;;  structs representing recursive functions:
	;;
	;;     (fix ((f (lambda () f)))
	;;       ?body)
	;;
	;;  which have a  reference to themselves in the body of  the function and so
	;;  in the list of free variables.
	;;
	(let ((substituted-freevar* (%filter-freevar* ($closure-maker-freevar* clmaker))))
	  (if (var? self-var)
	      (remq self-var substituted-freevar*)
	    substituted-freevar*)))

      (define* (%filter-freevar* freevar*)
	;;Non-tail recursive function.  Given a list of VAR structs representing free
	;;variables in  the body of a  function: build and  return a new list  of VAR
	;;structs representing the actual free vars we care about.
	;;
	(if (pair? freevar*)
	    (let ((A (car freevar*))
		  (D (cdr freevar*)))
	      (let ((what (%find-var-substitution! A))
		    (rest (%filter-freevar* D)))
		;;Here WHAT is the  substitution of A; it is possible  that WHAT is A
		;;itself.
		(struct-case what
		  ((closure-maker)
		   ;;The substitution is a CLOSURE-MAKER struct: filter it out.
		   rest)
		  ((var)
		   ;;Either WHAT  is A itself or  the substitution of A  is a VAR
		   ;;struct: include it, but only once.
		   (if (memq what rest)
		       rest
		     (cons what rest)))
		  (else
		   (compiler-internal-error __module_who__ __who__
		     "invalid VAR substitution value" what)))))
	  '()))

      #| end of module: %ORIGINAL-FREEVAR*->FILTERED-AND-SUBSTITUTED-FREEVAR *|# )

    (define (%lift-clambda! clmaker)
      ;;Given data  from a CLOSURE-MAKER  struct: build a new  CLAMBDA to be  used to
      ;;generate  the actual  code object;  build  a CODE-LOC  struct to  be used  to
      ;;generate the actual closure object; return the CODE-LOC; push the new CLAMBDA
      ;;on the parameter ALL-CLAMBDAS.
      ;;
      (let ((original-clam ($closure-maker-code     clmaker))
	    (new-freevar*  ($closure-maker-freevar* clmaker)))
	#;(assert (clambda? original-clam))
	(struct-case original-clam
	  ((clambda label clause* cp freevar*.unset name)
	   #;(assert (var? cp))
	   #;(assert (not freevar*.unset))
	   (let ((clause*^ ($map/stx (lambda (clause)
				       (struct-case clause
					 ((clambda-case info body)
					  ;;Clear  the  field   "index"  of  the  VAR
					  ;;structs in  ARGS from whatever  value the
					  ;;previous compiler passes have left in.
					  ($for-each/stx %var-reset-subst! (case-info-args info))
					  (make-clambda-case info (E body)))))
			     clause*)))
	     (%gather-clambda! (make-clambda label clause*^ cp new-freevar* name))
	     ($set-closure-maker-code! clmaker (make-code-loc label)))))))

    #| end of module: E-fix |# )

;;; --------------------------------------------------------------------

  (define* (%find-var-substitution! x)
    ;;Non-tail  recursive function.   X  is  a VAR  struct.   Traverse  the graph  of
    ;;substitutions starting from X and return:
    ;;
    ;;* X itself if X has no substitution.
    ;;
    ;;* If the substitution  of X is a CLOSURE-MAKER struct:  return such struct.  In
    ;;   this case  the CLOSURE-MAKER  represents code  that, evaluated  at run-time,
    ;;  returns a "combinator" function.
    ;;
    ;;* If the substitution  of X is another VAR struct Y:  recurse searching for the
    ;;   substitution of  Y,  which will  become  the substitution  of  X; cache  the
    ;;  substitution of Y as substitution of X.
    ;;
    (when (eq? x 'q)
      (compile-time-error __module_who__ __who__
	"circular dependency searching for VAR substitution"))
    (let ((x.subst (%var-get-subst x)))
      (cond ((not x.subst)
	     ;;The VAR  struct X has  no substitution,  so it cannot  be substituted:
	     ;;just use it.  This  might be a substitution: if X is  not the start of
	     ;;the graph traversal,  the VAR from which the traversal  was started is
	     ;;substituted with X.
	     x)

	    ((var? x.subst)
	     ;;The VAR struct X has another  VAR as substitution: step forward in the
	     ;;graph of substitutions.
	     ;;
	     ;;By  temporarily  setting the  subst  to  "q"  we can  detect  circular
	     ;;references while recursing into %FIND-VAR-SUBSTITUTION!.
	     (%var-set-subst! x 'q)
	     (receive-and-return (new-subst)
		 (%find-var-substitution! x.subst)
	       ;;Down the graph traversal we  have retrieved a substitution: cache it
	       ;;so that further  traversals reaching X will just use  it rather than
	       ;;go deeper again.
	       (%var-set-subst! x new-subst)))

	    ((closure-maker? x.subst)
	     ;;The  VAR X  has a  CLOSURE-MAKER  as substitution.   The original  VAR
	     ;;struct,  at the  beginning of  the substitutions  graph traversal,  is
	     ;;substituted by this  CLOSURE-MAKER struct which has  no free variables
	     ;;and so will return a "combinator" closure object.
	     #;(assert (null? (closure-maker-freevar* x.subst)))
	     x.subst)

	    (else
	     (compiler-internal-error __module_who__ __who__
	       "invalid VAR substitution" x x.subst)))))

;;; --------------------------------------------------------------------
;;; VAR structs and associated NODE structs

  ;;Whenever bindings defined by  a FIX struct are processed: a  NODE struct for each
  ;;binding is  created to represent  a graph of  dependencies; such graph  allows to
  ;;establish which bindings define a "combinator function" and which bindings define
  ;;a "non-combinator function".
  ;;
  ;;The  field "index"  of the  VAR structs  representing the  left-hand side  of FIX
  ;;bindings, is used to hold the NODE struct associated to the VAR struct itself.
  ;;
  ;;The fields  "index" are reset  to false before the  VAR structs are  processed to
  ;;enter the graph of substitutions.
  ;;

  (define (%var-reset-node! V)
    #;(assert (var? V))
    ($set-var-index! V #f))

  (define (%var-set-node! V N)
    #;(assert (var?  V))
    #;(assert (node? N))
    ($set-var-index! V N))

  (define (%var-get-node V)
    #;(assert (var? x))
    ($var-index V))

;;; --------------------------------------------------------------------
;;; VAR structs and associated substitutions

  ;;The field  "index" of VAR structs  is used to  hold the substitution for  the VAR
  ;;struct itself.  If "index" holds false: the VAR struct cannot be substituted with
  ;;anything; if the "index" holds another  VAR struct or a CLOSURE-MAKER struct: the
  ;;VAR struct can be substituted with such object.
  ;;

  (define (%var-reset-subst! x)
    #;(assert (var? x))
    ($set-var-index! x #f))

  (define (%var-set-subst! x v)
    #;(assert (var? x))
    #;(assert (or (node? v) (closure-maker? v) (var? v) (eq? v 'q)))
    ($set-var-index! x v))

  (define (%var-reset-node/set-subst! x v)
    (%var-set-subst! x v))

  (define (%var-get-subst x)
    #;(assert (var? x))
    ($var-index x))

  #| end of module: OPTIMIZE-COMBINATOR-CALLS/LIFT-CLAMBDAS |# )


(module (introduce-primitive-operation-calls)
  ;;The purpose of this module is to examine all the function calls:
  ;;
  ;;   (?operator ?arg ...)
  ;;
  ;;which, in recordized  code, are represented by struct instances  of type FUNCALL;
  ;;everything else is left untouched.  If the ?OPERATOR is a struct instance of type
  ;;PRIMREF  representing  a primitive  operation:  such  struct  is replaced  by  an
  ;;appropriate struct instance of type PRIMOPCALL; recordized code like:
  ;;
  ;;   #[funcall  #[primref ?name] (?arg ...)]
  ;;
  ;;is converted to:
  ;;
  ;;   #[primopcall ?name (?arg ...)]
  ;;
  ;;If  the FUNCALL  struct represents  a call  to a  proper primitive  function (not
  ;;operation): it is left untouched as FUNCALL struct.
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   bind		closure-maker	conditional
  ;;   constant		fix		forcall
  ;;   funcall		jmpcall		known
  ;;   primref		seq		var
  ;;
  ;;NOTE  Not  all  the  struct  instances of  type  PRIMREF  reference  a  primitive
  ;;operation: the struct  type PRIMREF is used  to represent a reference  to all the
  ;;lexical core bindings defined by the boot image.  Only those for which ?NAME is a
  ;;symbol   satisfying  the   predicate  CORE-PRIMITIVE-OPERATION?    are  primitive
  ;;operations;  in  other   words,  only  the  operations  defined   by  the  syntax
  ;;DEFINE-PRIMITIVE-OPERATION  are  primitive  operations.   Examples:  $CAR,  $CDR,
  ;;FIXNUM?  are  primitive operations;  LIST, NUMBER?,  DISPLAY are  *not* primitive
  ;;operations.
  ;;
  ;;NOTE Not all  the instances of struct PRIMOPCALL are  generated from instances of
  ;;FUNCALL; so not all the instances of PRIMOPCALL are generated here.
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'introduce-primitive-operation-calls))

  (define* (introduce-primitive-operation-calls Program)
    (struct-case Program
      ((codes code* body)
       (make-codes ($map/stx E-clambda code*)
		   (E body)))
      (else
       (compiler-internal-error __module_who__ __who__
	 "invalid input expression" (unparse-recordized-code Program)))))

;;; --------------------------------------------------------------------

  (module (E)

    (define* (E x)
      ;;Perform code transformation  traversing the whole hierarchy in  X, which must
      ;;be a struct instance representing recordized  code, and build a new hierarchy
      ;;of transformed, recordized code; return the new hierarchy.
      ;;
      ;;The purpose of this recordized code  traversal is to process struct instances
      ;;of  type  FUNCALL  with  the  module  MK-FUNCALL;  everything  else  is  left
      ;;untouched.
      ;;
      (struct-case x
	((constant)
	 x)

	((var)
	 x)

	((primref)
	 x)

	((bind lhs* rhs* body)
	 (make-bind lhs* ($map/stx E rhs*) (E body)))

	((fix lhs* rhs* body)
	 (make-fix lhs* rhs* (E body)))

	((conditional e0 e1 e2)
	 (make-conditional (E e0) (E e1) (E e2)))

	((seq e0 e1)
	 (make-seq (E e0) (E e1)))

	((closure-maker)
	 x)

	((forcall op arg*)
	 (make-forcall op ($map/stx E arg*)))

	((funcall rator arg*)
	 (mk-funcall (E-known rator) ($map/stx E-known arg*)))

	((jmpcall label rator arg*)
	 (make-jmpcall label (E rator) ($map/stx E arg*)))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid input expression" (unparse-recordized-code x)))))

    (define (E-known x)
      (struct-case x
	((known expr type)
	 (make-known (E expr) type))
	(else
	 (E x))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (module (E-clambda)
    ;;The purpose  of this  module is  to apply E  to the  body of  every CASE-LAMBDA
    ;;clause.
    ;;
    (define* (E-clambda x)
      (struct-case x
	((clambda label case* cp freevar* name)
	 (make-clambda label ($map/stx E-clambda-case case*) cp freevar* name))
	(else
	 (compiler-internal-error __module_who__ __who__ "invalid clambda" x))))

    (define* (E-clambda-case x)
      (struct-case x
	((clambda-case info body)
	 (make-clambda-case info (E body)))
	(else
	 (compiler-internal-error __module_who__ __who__ "invalid clambda-case" x))))

    #| end of module: E-clambda |# )

;;; --------------------------------------------------------------------

  (module (mk-funcall)

    (define (mk-funcall op arg*)
      ;;OP is a struct instance representing the operator in a function application.
      ;;
      ;;ARG* is a  list of struct instances representing the  arguments of a function
      ;;application.
      ;;
      ;;If the  operator is  a struct  instance of type  PRIMREF representing  a core
      ;;primitive  operation:  such  struct  is replaced  by  an  appropriate  struct
      ;;instance of type PRIMOPCALL.
      ;;
      (struct-case op
	((known expr)
	 (mk-funcall expr arg*))

	((primref name)
	 (if (%primitive-operation? name)
	     (make-primopcall name arg*)
	   (make-funcall op arg*)))

	(else
	 (make-funcall op arg*))))

    (define (%primitive-operation? x)
      ;;Import  the  function CORE-PRIMITIVE-OPERATION?   from  a  module defined  in
      ;;"pass-specify-rep.ss".  (Marco Maggi; Oct 14, 2012)
      (module (core-primitive-operation?)
	(import CORE-PRIMITIVE-OPERATION-NAMES))
      (or (eq? x 'debug-call)
	  (core-primitive-operation? x)))

    #| end of module: MK-FUNCALL |# )

  #| end of module: INTRODUCE-PRIMITIVE-OPERATION-CALLS |# )


(module (rewrite-freevar-references)
  ;;This module  rewrites references to  free variables  in closure objects  to forms
  ;;actually accessing the values from the run-time closure object.
  ;;
  ;;We know that  a Scheme closure object (satisfiying the  predicate PROCEDURE?) has
  ;;memory layout:
  ;;
  ;;                  0   1   2   3   4   5
  ;;   |------------|---|---|---|---|---|---| closure object
  ;;         ^
  ;;         |      |.......................|
  ;;    pointer to     one slot for every
  ;;    binary code    free variable
  ;;
  ;;in which a slot for every free  variable is allocated to: directly hold the value
  ;;for unassigned  variables; hold a  reference to  the mutable vector  for assigned
  ;;variables.  The purpose of this module is to:
  ;;
  ;;1. For  every closure's  clause make  a new  struct instance  of type  VAR called
  ;;   CPVAR.
  ;;
  ;;2.  In every  closure  clause's body:  find  every struct  instance  of type  VAR
  ;;   referencing the closure itself, and replace it with CPVAR.
  ;;
  ;;3.  In every  closure  clause's body:  find  every struct  instance  of type  VAR
  ;;    referencing  a closure's  free  variable  and  replace  it with  a  primitive
  ;;   operation $CPREF retrieving the referenced  object from the associated slot in
  ;;   the data area of the closure built in object.
  ;;
  ;;4. Transform every ?CLOSURE-MAKER not appearing as RHS of a FIX into:
  ;;
  ;;      (fix ((tmp ?closure-maker))
  ;;        tmp)
  ;;
  ;;   This is  an independent additional task that must  be performed somewhere, and
  ;;   we do it here.
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   bind		closure-maker	conditional
  ;;   constant		fix		forcall
  ;;   funcall		jmpcall		known
  ;;   primref		seq		var
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'rewrite-freevar-references))

  (define* (rewrite-freevar-references Program)
    (struct-case Program
      ((codes code* body)
       ;;First traverse  the bodies of  the lifted  CLAMBDAs, then traverse  the init
       ;;expression.
       (let* ((code*^ ($map/stx E-clambda code*))
	      (E      (let ((main-cpvar #f)
			    (cpvar      #f)
			    (freevar*   '()))
			(make-E main-cpvar cpvar freevar*)))
	      (body^  (E body)))
	 (make-codes code*^ body^)))
      (else
       (compiler-internal-error __module_who__ __who__ "invalid program" Program))))

;;; --------------------------------------------------------------------

  (module (E-clambda)

    (define* (E-clambda x)
      ;;X must be a struct instance of type CLAMBDA.
      ;;
      (struct-case x
	((clambda label clause* cp freevar* name)
	 (let ((case-mapper (make-E-clambda-case cp freevar*))
	       (cp^         #f))
	   (make-clambda label ($map/stx case-mapper clause*) cp^ freevar* name)))
	(else
	 (compiler-internal-error __module_who__ __who__ "invalid clambda" x))))

    (define* (make-E-clambda-case main-cp freevar*)
      ;;MAIN-CP must  be a  struct instance  of type VAR  to which  the CLOSURE-MAKER
      ;;referencing this CLAMBDA is bound; it  represents the machine word from which
      ;;a pointer to the closure object can be acquired.
      ;;
      ;;FREEVAR* must be a list of struct instances of type VAR representing the free
      ;;variables referenced by the clauses of this CASE-LAMBDA.
      ;;
      ;;Return a function to be mapped over all the CLAMBDA clauses.
      ;;
      ;;Notice that CPVAR is prepended to the list of arguments for this clause.
      ;;
      (lambda (x)
	;;X must be a struct instance of type CLAMBDA-CASE.
	;;
	(struct-case x
	  ((clambda-case info body)
	   (struct-case info
	     ((case-info label args proper?)
	      ;;The VAR  struct CPVAR  represents the machine  word (CPU  register or
	      ;;memory location)  from which  the code  can load  a reference  to the
	      ;;closure  to be  stored in  the CPR  (Closure Pointer  Register); such
	      ;;reference allows  the body of  a run-time  code object to  access the
	      ;;free variables upon which it is closed.
	      (let* ((cpvar (make-unique-var 'cp))
		     ;;Prepend to the properised list of formals the VAR representing
		     ;;the machine word holding the current closure pointer.
		     (info^ (make-case-info label (cons cpvar args) proper?))
		     (E     (make-E main-cp cpvar freevar*))
		     (body^ (E body)))
		(make-clambda-case info^ body^)))))
	  (else
	   (compiler-internal-error __module_who__ __who__ "invalid clambda-case" x)))))

    #| end of module: Clambda |# )

;;; --------------------------------------------------------------------

  (define (make-E main-cpvar cpvar freevar*)
    ;;Create and return the function E.
    ;;
    (define* (E x)
      ;;Perform code transformation  traversing the whole hierarchy in  X, which must
      ;;be  a  struct instance  representing  recordized  code,  and building  a  new
      ;;hierarchy of transformed, recordized code; return the new hierarchy.
      ;;
      ;;The purposes of this code traversal are:
      ;;
      ;;1. Map %DO-VAR over every struct instance of type VAR in reference position.
      ;;
      ;;2. Map %DO-FIX to every struct instance of type FIX.
      ;;
      ;;3.  Convert every  standalone struct  instance of  type CLOSURE-MAKER  into a
      ;;   struct instance of type FIX representing this form:
      ;;
      ;;      (let ((T ?closure-maker))
      ;;        T)
      ;;
      ;;   where T is a unique variable.
      ;;
      (struct-case x
	((constant)
	 x)

	((var)
	 (%do-var x))

	((primref)
	 x)

	((bind lhs* rhs* body)
	 (make-bind lhs* ($map/stx E rhs*) (E body)))

	((fix lhs* rhs* body)
	 (%do-fix lhs* rhs* (E body)))

	((conditional e0 e1 e2)
	 (make-conditional (E e0) (E e1) (E e2)))

	((seq e0 e1)
	 (make-seq (E e0) (E e1)))

	((closure-maker)
	 (let ((t (make-unique-var)))
	   (E (make-fix (list t) (list x) t))))

	((primopcall op arg*)
	 (make-primopcall op ($map/stx E-known arg*)))

	((forcall op arg*)
	 (make-forcall op ($map/stx E arg*)))

	((funcall rator arg*)
	 (make-funcall (E-known rator) ($map/stx E-known arg*)))

	((jmpcall label rator arg*)
	 (make-jmpcall label (E rator) ($map/stx E arg*)))

	(else
	 (compiler-internal-error __module_who__ __who__ "invalid expr" x))))

    (define (E-known x)
      (struct-case x
	((known expr type)
	 (make-known (E expr) type))
	(else
	 (E x))))

    (module (%do-fix)
      ;;The purpose of this module is to map %DO-VAR over all the struct instances of
      ;;type VAR  being free variables  referenced by  the closures.  We  cannot move
      ;;this module out  of MAKE-E because %DO-VAR  is a closure on  the arguments of
      ;;MAKE-E itself.
      ;;
      (define (%do-fix lhs* rhs* body)
	(make-fix lhs* ($map/stx %handle-closure rhs*) body))

      (define (%handle-closure rhs)
	;;RHS must be a struct instance of type CLOSURE-MAKER.
	;;
	(struct-case rhs
	  ((closure-maker code freevar*)
	   (make-closure-maker code ($map/stx %do-var freevar*)))))

      #| end of module: %do-fix |# )

    (define (%do-var x)
      ;;This function is a closure  upon the arguments of MAKE-E:
      ;;
      ;;MAIN-CPVAR: false  or a struct instance  of type VAR referencing  the closure
      ;;whose body we are traversing.
      ;;
      ;;CPVAR: false  or a struct  instance of type  VAR associated to  the closure's
      ;;clause whose body we are traversing.
      ;;
      ;;FREEVAR*:  a list  of  struct instances  of type  VAR  representing the  free
      ;;variables referenced by the closure whose body we are traversing.
      ;;
      ;;X must  be a struct instance  of type VAR.
      ;;
      ;;If X references the closure itself: replace it with CPVAR.
      ;;
      ;;If  X references  a  closure's free  variable: replace  it  with a  primitive
      ;;operation $CPREF retrieving the referenced object from the associated slot in
      ;;the data area of the closure built in object.
      ;;
      (if (eq? x main-cpvar)
	  cpvar
	(let loop ((freevar* freevar*)
		   (i        0))
	  (if (pair? freevar*)
	      (if (eq? x (car freevar*))
		  ;;Replace a  reference to free  variable with the  appropriate slot
		  ;;accessor.
		  (make-primopcall '$cpref (list cpvar (make-constant i)))
		(loop (cdr freevar*) (fxadd1 i)))
	    x))))

    E)

  #| end of module: REWRITE-FREEVAR-REFERENCES |# )


(module (insert-engine-checks)
  ;;This module traverses all the function bodies  and, if the body contains at least
  ;;one  JMPCALL struct  or one  FUNCALL struct  (in which  the operator  is *not*  a
  ;;PRIMREF), it transforms the ?BODY into:
  ;;
  ;;   (begin
  ;;     (primopcall '$do-event '())
  ;;     ?body)
  ;;
  ;;the call  to the primitive operation  $DO-EVENT suspends the execution  of Scheme
  ;;code  for the  current process  and enters  a subprocess  which can  take actions
  ;;asynchronously.
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   bind		closure-maker	conditional
  ;;   constant		fix		forcall
  ;;   funcall		jmpcall		known
  ;;   primref		seq		var
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'insert-engine-checks))

  (module (insert-engine-checks)

    (define (insert-engine-checks x)
      (struct-case x
	((codes list body)
	 (make-codes ($map/stx E-clambda list)
		     (%introduce-check-maybe body)))))

    (define (E-clambda x)
      (struct-case x
	((clambda label cases cp freevar* name)
	 (make-clambda label ($map/stx E-clambda-clause cases) cp freevar* name))))

    (define (E-clambda-clause x)
      (struct-case x
	((clambda-case info body)
	 (make-clambda-case info (%introduce-check-maybe body)))))

    (define (%introduce-check-maybe body)
      (if (E body)
	  (make-seq EVENT-PRIMOPCALL body)
	body))

    (define-constant EVENT-PRIMOPCALL
      (make-primopcall '$do-event '()))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (E)

    (define* (E x)
      ;;The purpose of this recordized code traversal is to return true if:
      ;;
      ;;* At least one of the nested structs is an instance of JMPCALL.
      ;;
      ;;* At least one  of the nested structs is an instance of  FUNCALL in which the
      ;;  operator is *not* a struct instance of type PRIMREF.
      ;;
      ;;else the return value is false.
      ;;
      (struct-case x
	((constant)
	 #f)

	((var)
	 #f)

	((primref)
	 #f)

	((jmpcall label rator arg*)
	 #t)

	((funcall rator arg*)
	 (if (%known-primref? rator)
	     (ormap E-known arg*)
	   #t))

	((bind lhs* rhs* body)
	 (or (ormap E rhs*) (E body)))

	((fix lhs* rhs* body)
	 (E body))

	((conditional e0 e1 e2)
	 (or (E e0) (E e1) (E e2)))

	((seq e0 e1)
	 (or (E e0) (E e1)))

	((primopcall op arg*)
	 (ormap E-known arg*))

	((forcall op arg*)
	 (ormap E arg*))

	(else
	 (compiler-internal-error __module_who__ __who__
	   "invalid input expression" (unparse-recordized-code x)))))

    (define (E-known x)
      (struct-case x
	((known expr)
	 (E expr))
	(else
	 (E x))))

    (define (%known-primref? x)
      ;;Return true if X is a struct  instance of type PRIMREF, possibly wrapped into
      ;;a struct instance of type KNOWN.
      ;;
      (struct-case x
	((known expr)
	 (%known-primref? expr))
	((primref)
	 #t)
	(else
	 #f)))

    #| end of module: E |# )

  #| end of file: INSERT-ENGINE-CHECKS |# )


(module (insert-stack-overflow-check)
  ;;This module traverses all the function bodies  and: if a ?BODY contains code that
  ;;will cause further use of the stack, it transforms it as follows:
  ;;
  ;;   (begin
  ;;     (primopcall '$stack-overflow-check '())
  ;;     ?body)
  ;;
  ;;so  that, right  after entering  the execution  of a  function, the  call to  the
  ;;primitive operation $STACK-OVERFLOW-CHECK  checks if the current  Scheme stack is
  ;;about to be  exhausted.  If a ?BODY does  not make further use of  the stack: its
  ;;function execution is a "stack tail".
  ;;
  ;;This module  accepts as  input a  struct instance of  type CODES,  whose internal
  ;;recordized code must be composed by struct instances of the following types:
  ;;
  ;;   bind		closure-maker	conditional
  ;;   constant		fix		forcall
  ;;   funcall		jmpcall		known
  ;;   primref		seq		var
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'insert-stack-overflow-check))

  (module (insert-stack-overflow-check)

    (define (insert-stack-overflow-check x)
      (struct-case x
	((codes code* body)
	 (make-codes (map E-clambda code*)
		     (%process-body body)))))

    (module (E-clambda)
      ;;The purpose of this module is  to apply %PROCESS-BODY to all the
      ;;bodies of closure's clauses.
      ;;
      (define (E-clambda x)
	(struct-case x
	  ((clambda label case* cp freevar* name)
	   (make-clambda label (map E-clambda-clause case*) cp freevar* name))))

      (define (E-clambda-clause x)
	(struct-case x
	  ((clambda-case info body)
	   (make-clambda-case info (%process-body body)))))

      #| end of module: E-clambda |# )

    (define (%process-body body)
      (if (%tail? body)
	  (make-seq CHECK-PRIMOPCALL body)
	body))

    (define-constant CHECK-PRIMOPCALL
      (make-primopcall '$stack-overflow-check '()))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (%tail?)

    (define* (%tail? body)
      ;;Return true if  the recordized code BODY  contains only function
      ;;calls in tail position.
      ;;
      (struct-case body
	((constant)		#f)
	((var)			#f)
	((primref)		#f)

	((bind lhs* rhs* body)
	 (or (ormap %non-tail? rhs*)
	     (%tail? body)))

	((fix lhs* rhs* body)
	 (%tail? body))

	((conditional e0 e1 e2)
	 (or (%non-tail? e0)
	     (%tail? e1)
	     (%tail? e2)))

	((seq e0 e1)
	 (or (%non-tail? e0)
	     (%tail? e1)))

	((primopcall op arg*)
	 (ormap %non-tail? arg*))

	((forcall op arg*)
	 (ormap %non-tail? arg*))

	((funcall rator arg*)
	 (or (%non-tail? rator)
	     (ormap %non-tail? arg*)))

	((jmpcall label rator arg*)
	 (or (%non-tail? rator)
	     (ormap %non-tail? arg*)))

	(else
	 (compiler-internal-error __module_who__ __who__ "invalid expr" body))))

    (module (%non-tail?)

      (define* (%non-tail? x)
	;;Notice that this function never  calls %TAIL?.  Return true if
	;;the recordized code X contains any type of function call.
	;;
	(struct-case x
	  ((constant)			#f)
	  ((var)			#f)
	  ((primref)			#f)

	  ((funcall rator arg*)		#t)
	  ((jmpcall label rator arg*)	#t)

	  ;;FIXME!  (Abdulaziz Ghuloum)
	  ((primopcall op arg*)
	   (ormap %non-tail?-known arg*))

	  ((bind lhs* rhs* body)
	   (or (ormap %non-tail? rhs*)
	       (%non-tail? body)))

	  ((fix lhs* rhs* body)
	   (%non-tail? body))

	  ((conditional e0 e1 e2)
	   (or (%non-tail? e0)
	       (%non-tail? e1)
	       (%non-tail? e2)))

	  ((seq e0 e1)
	   (or (%non-tail? e0)
	       (%non-tail? e1)))

	  ((forcall op arg*)
	   (ormap %non-tail? arg*))

	  ((known expr)
	   (%non-tail? expr))

	  (else
	   (compiler-internal-error __module_who__ __who__ "invalid expr" x))))

      (define (%non-tail?-known x)
	(struct-case x
	  ((known expr)
	   (%non-tail? expr))
	  (else
	   (%non-tail? x))))

      #| end of module: %non-tail? |# )

    #| end of module: %tail? |# )

  #| end of module: insert-stack-overflow-check |# )


(module CORE-PRIMITIVE-OPERATION-NAMES
  (core-primitive-operation? get-primop set-primop!)
  ;;This module  has the only  purpose of making the  binding COOKIE visible  only to
  ;;CORE-PRIMITIVE-OPERATION?, GET-PRIMOP and SET-PRIMOP!.
  ;;
  (define-constant COOKIE
    (expand-time-gensym "core-primitive-operation/integration-handler"))

  (define* (core-primitive-operation? {core-primitive-symbol-name symbol?})
    ;;Return  true  if  CORE-PRIMITIVE-SYMBOL-NAME  is  the public  name  of  a  core
    ;;primitive operation; otherwise return false.
    ;;
    (and (getprop core-primitive-symbol-name COOKIE) #t))

  (define* (get-primop {core-primitive-symbol-name symbol?})
    ;;If CORE-PRIMITIVE-SYMBOL-NAME is the public name of a core primitive operation:
    ;;return a PRIMITIVE-HANDLER struct describind  the operation; otherwise raise an
    ;;exception.
    ;;
    (or (getprop core-primitive-symbol-name COOKIE)
	(compiler-internal-error 'CORE-PRIMITIVE-OPERATION-NAMES __who__
	  "not a core primitive operation" core-primitive-symbol-name)))

  (define* (set-primop! {symbol symbol?} primitive-handler)
    ;;Associate to  SYMBOL the struct  PRIMITIVE-HANDLER, turning SYMBOL into  a core
    ;;primitive's symbol name.
    ;;
    (putprop symbol COOKIE primitive-handler))

  #| end of module CORE-PRIMITIVE-OPERATION-NAMES |# )


;;;; Assembly code generation

(module (alt-cogen
	 refresh-common-assembly-subroutines-cached-labels!
	 sl-apply-label
	 specify-representation
	 impose-calling-convention/evaluation-order
	 assign-frame-sizes
	 color-by-chaitin
	 flatten-codes)

  (define (alt-cogen x)
    (let* ((x  (specify-representation x))
	   (x  (impose-calling-convention/evaluation-order x))
	   (x  (assign-frame-sizes x))
	   (x  (if (check-compiler-pass-preconditions)
		   (preconditions-for-color-by-chaitin x)
		 x))
	   (x  (color-by-chaitin x))
	   (code-object-sexp* (flatten-codes x)))
      code-object-sexp*))

;;; --------------------------------------------------------------------
;;; high-level assembly instructions

  (define (asm op . rand*)
    ;;Build  and  return  recordised  call which  performs  the  high-level  Assembly
    ;;instruction OP applying it to the arguments RAND*.
    ;;
    (make-asmcall op rand*))

  (define (nop)
    ;;Build  and  return  recordised  call representing  the  dummy  instruction  "no
    ;;operation".
    ;;
    (asm 'nop))

  (define (interrupt)
    ;;Build and  return recordised call representing  a jump to a  SHORTCUT interrupt
    ;;handler.
    ;;
    ;;NOTE This function is shadowed in  the pass "specify representation" by a local
    ;;INTERRUPT function.
    ;;
    (asm 'interrupt))

;;; --------------------------------------------------------------------
;;; include some external code for compiler passes and modules

  (include "ikarus.compiler.scheme-objects-layout.scm"		#t)
  (include "ikarus.compiler.intel-assembly.scm"			#t)
  (include "ikarus.compiler.common-assembly-subroutines.scm"	#t)

  (include "ikarus.compiler.pass-specify-representation.scm"	#t)
  (include "ikarus.compiler.pass-impose-evaluation-order.scm"	#t)
  (include "ikarus.compiler.pass-assign-frame-sizes.scm"	#t)
  (include "ikarus.compiler.pass-color-by-chaitin.scm"		#t)
  (include "ikarus.compiler.pass-flatten-codes.scm"		#t)

  #| end of module: alt-cogen |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'define-structure			'scheme-indent-function 1)
;; eval: (put 'make-conditional			'scheme-indent-function 2)
;; eval: (put 'struct-case			'scheme-indent-function 1)
;; eval: (put '$map/stx				'scheme-indent-function 1)
;; eval: (put '$for-each/stx			'scheme-indent-function 1)
;; eval: (put '$fold-right/stx			'scheme-indent-function 1)
;; eval: (put 'with-prelex-structs-in-plists	'scheme-indent-function 1)
;; eval: (put 'compile-time-error		'scheme-indent-function 2)
;; eval: (put 'compiler-internal-error		'scheme-indent-function 2)
;; eval: (put 'compile-time-arity-error		'scheme-indent-function 2)
;; eval: (put 'compile-time-operand-core-type-error 'scheme-indent-function 2)
;; eval: (put '%parse-compilation-options	'scheme-indent-function 1)
;; End:
