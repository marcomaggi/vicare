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
    (ikarus.compiler.pass-sanitize-bindings)
    (ikarus.compiler.pass-optimize-for-direct-jumps)
    (ikarus.compiler.pass-insert-global-assignments)
    (ikarus.compiler.pass-introduce-vars)
    (ikarus.compiler.pass-introduce-closure-makers)
    (ikarus.compiler.pass-optimize-combinator-calls-lift-clambdas)
    (ikarus.compiler.pass-introduce-primitive-operation-calls)
    (ikarus.compiler.pass-rewrite-freevar-references)
    (ikarus.compiler.pass-insert-engine-checks)
    (ikarus.compiler.pass-insert-stack-overflow-check)
    (ikarus.compiler.code-generation)
    ;;When building a new  boot image: the FASL write library  is loaded from source.
    ;;This needs  to be  loaded here  so that  it evaluates  with the  freshly loaded
    ;;"ikarus.config.scm", including the correct value for WORDSIZE.
    (only (ikarus.fasl.write)
	  fasl-write)
    (only (ikarus.intel-assembler)
	  assemble-sources))

  (include "ikarus.wordsize.scm" #t)


;;;; compiler entry point

(module COMPILER-SINGLE-ENTRY-POINT
  (compile-core-language-expression)

  (define compiler-initialised? #f)

  (define (initialise-compiler)
    (unless compiler-initialised?
      (initialise-core-primitive-properties)
      (initialise-core-primitive-operations)
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
