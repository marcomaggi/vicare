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
    current-primitive-locations
    eval-core				current-core-eval
    compile-core-expr-to-port		compile-core-expr
    core-expr->optimized-code		core-expr->assembly-code
    core-expr->optimisation-and-core-type-inference-code

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
  (import (except (vicare)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Wed Dec 10, 2014)
		  void-object?

		  ;;We need a SYSTEM-VALUE procedure, but the correct one.  See below
		  ;;the appropriately commented import spec.
		  #;system-value

		  return
		  current-primitive-locations
		  eval-core			current-core-eval
		  compile-core-expr-to-port	compile-core-expr

		  assembler-output		optimizer-output

		  cp0-effort-limit		cp0-size-limit
		  current-letrec-pass		generate-debug-calls
		  optimize-level
		  perform-core-type-inference	strip-source-info
		  fasl-write)
    (ikarus.compiler.config)
    ;;Here we *truly* want to use the SYSTEM-VALUE provided by the library (vicare).
    ;;
    ;;* During normal execution: this binding is the one from the boot image.
    ;;
    ;;* While building  a new boot image: this  binding is the one from  the old boot
    ;;  image, *not* the one from loading "(ikarus.symbols)" in source form.
    ;;
    ;;We really need it this way for the use we do of such procedure.
    (only (vicare) system-value)
    ;;When building a new  boot image: the FASL write library  is loaded from source.
    ;;This needs  to be  loaded here  so that  it evaluates  with the  freshly loaded
    ;;"ikarus.config.scm", including the correct value for WORDSIZE.
    (only (ikarus.fasl.write)
	  fasl-write)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.scheme-objects-ontology)
    (ikarus.compiler.core-primitive-properties)
    (only (ikarus.intel-assembler)
	  assemble-sources)
    (prefix (only (ikarus.options)
		  strict-r6rs
		  verbose?)
	    option.))

  (include "ikarus.wordsize.scm" #t)


;;;; configuration parameters

(define generate-debug-calls
  ;;Set  to true  when  the option  "--debug"  is used  on the  command  line of  the
  ;;executable "vicare"; else set to #f.
  ;;
  (make-parameter #f))

(define strip-source-info
  ;;When true:  while processing  the core  language form  ANNOTATED-CASE-LAMBDA, the
  ;;annotation about the source code location of the expression is removed; otherwise
  ;;it is left in to be used by the debugger.
  ;;
  ;;We should strip source annotations when building the boot image.
  ;;
  (make-parameter #f))

(define optimizer-output
  (make-parameter #f))

(define perform-core-type-inference
  ;;When true: the pass CORE-TYPE-INFERENCE is performed, else it is skipped.
  ;;
  (make-parameter #t))

(define perform-unsafe-primrefs-introduction
  ;;When true: the pass INTRODUCE-UNSAFE-PRIMREFS  is performed, else it is skipped.
  ;;It makes sense to perform such compiler  pass only if we have first performed the
  ;;core type inference.
  ;;
  (make-parameter #t))

(define assembler-output
  (make-parameter #f))

(define enabled-function-application-integration?
  ;;When  true:   the  source   optimiser  will   attempt  integration   of  function
  ;;applications.
  ;;
  (make-parameter #t
    (lambda (obj)
      (and obj #t))))

(define check-compiler-pass-preconditions
  ;;When true:  perform additional  compiler code-validation  passes to  validate the
  ;;recordised code between true compiler passes.
  ;;
  (make-parameter #f
    (lambda (obj)
      (and obj #t))))


;;;; helper syntaxes

(define-syntax (cond-expand stx)
  ;;A  simple  implementation of  COND-EXPAND  in  which  the tests  are  expressions
  ;;evaluated at expand time.
  ;;
  (syntax-case stx (else)
    ((?ctx (?test0 . ?clause0*) (?test . ?clause*) ... (else . ?else*))
     (with-syntax
	 ((OUTPUT (datum->syntax #'?ctx 'output)))
       #'(let-syntax ((OUTPUT (lambda (stx)
				(syntax-case stx ()
				  ((??ctx)
				   (datum->syntax #'??ctx
						  (cond (?test0 '(begin . ?clause0*))
							(?test  '(begin . ?clause*))
							...
							(else   '(begin . ?else*)))))
				  ))))
	   (OUTPUT))))
    ((?ctx (?test0 . ?clause0*) (?test . ?clause*) ...)
     (with-syntax
	 ((OUTPUT (datum->syntax #'?ctx 'output)))
       #'(let-syntax ((OUTPUT (lambda (stx)
				(syntax-case stx ()
				  ((??ctx)
				   (datum->syntax #'??ctx
						  (cond (?test0 '(begin . ?clause0*))
							(?test  '(begin . ?clause*))
							...
							(else   '(void)))))))))
	   (OUTPUT))))
    ))

;;; --------------------------------------------------------------------

(define-syntax-rule (%list-of-one-item? ?ell)
  (let ((ell ?ell))
    (and (pair? ell)
	 (null? (cdr ell)))))

(define-syntax-rule (fxincr! ?var)
  (set! ?var (fxadd1 ?var)))

(define-syntax (compile-time-gensym stx)
  ;;Generate a gensym at expand time and expand to the quoted symbol.
  ;;
  (syntax-case stx ()
    ((_ ?template)
     (let* ((tmp (syntax->datum #'?template))
	    (fxs (vector->list (foreign-call "ikrt_current_time_fixnums_2")))
	    (str (apply string-append tmp (map (lambda (N)
						 (string-append "." (number->string N)))
					    fxs)))
	    (sym (gensym str)))
       (with-syntax
	   ((SYM (datum->syntax #'here sym)))
	 (fprintf (current-error-port) "expand-time gensym ~a\n" sym)
	 #'(quote SYM))))))

;;; --------------------------------------------------------------------

(define-syntax ($map/stx stx)
  ;;Like  MAP, but  expand the  loop inline.   The "function"  to be  mapped must  be
  ;;specified by an identifier or lambda form because it is evaluated multiple times.
  (syntax-case stx ()
    ((_ ?proc ?ell0 ?ell ...)
     ;;This implementation  is: tail recursive,  loops in order, assumes  proper list
     ;;arguments of equal length.
     (with-syntax
	 (((ELL0 ELL ...) (generate-temporaries #'(?ell0 ?ell ...))))
       #'(letrec ((loop (lambda (result.head result.last-pair ELL0 ELL ...)
			  (if (pair? ELL0)
			      (let* ((result.last-pair^ (let ((new-last-pair (cons (?proc (car ELL0)
											  (car ELL)
											  ...)
										   '())))
							  (if result.last-pair
							      (begin
								(set-cdr! result.last-pair new-last-pair)
								new-last-pair)
							    new-last-pair)))
				     (result.head^       (or result.head result.last-pair^)))
				(loop result.head^ result.last-pair^ (cdr ELL0) (cdr ELL) ...))
			    (or result.head '())))))
	   (loop #f #f ?ell0 ?ell ...)))
     ;;This alternative  implementation: is non-tail recursive,  loops in unspecified
     ;;order, assumes proper list arguments of equal length.
     ;;
     ;; (with-syntax (((T ...) (generate-temporaries #'(?ell ...))))
     ;;   #'(let recur ((t ?ell0) (T ?ell) ...)
     ;; 	   (if (null? t)
     ;; 	       '()
     ;; 	     (cons (?proc (car t) (car T) ...)
     ;; 		   (recur (cdr t) (cdr T) ...))))))
     )))

(define-syntax ($for-each/stx stx)
  ;;Like FOR-HEACH, but expand the loop inline.   The "function" to be mapped must be
  ;;specified by an identifier or lambda form because it is evaluated multiple times.
  ;;
  ;;This implementation:  is tail recursive,  assumes proper list arguments  of equal
  ;;length.
  ;;
  (syntax-case stx ()
    ((_ ?func ?ell0 ?ell ...)
     (with-syntax (((T ...) (generate-temporaries #'(?ell ...))))
       #'(let loop ((t ?ell0) (T ?ell) ...)
	   (when (pair? t)
	     (?func (car t) (car T) ...)
	     (loop  (cdr t) (cdr T) ...)))))
    ))

(define-syntax ($fold-right/stx stx)
  ;;Like FOLD-RIGHT, but expand the loop inline.  The "function" to be folded must be
  ;;specified by an identifier or lambda form because it is evaluated multiple times.
  ;;
  ;;This  implementation: is  non-tail recursive,  assumes proper  list arguments  of
  ;;equal length.
  ;;
  (syntax-case stx ()
    ((_ ?combine ?knil ?ell0 ?ell ...)
     (with-syntax (((ELL ...) (generate-temporaries #'(?ell ...))))
       #'(let recur ((knil ?knil)
		     (ell0 ?ell0)
		     (ELL  ?ell)
		     ...)
	   (if (pair? ell0)
	       ;;This is FOLD-RIGHT so: first we recur, then we combine.
	       (?combine (car ell0) (car ELL) ... (recur knil (cdr ell0) (cdr ELL) ...))
	     knil))))
    ))

;;; --------------------------------------------------------------------

(define-syntax struct-case
  ;;Specialised  CASE syntax  for data  structures.  Notice  that we  could use  this
  ;;syntax for  any set of struct  types, not only  the struct types defined  in this
  ;;library.
  ;;
  ;;Given:
  ;;
  ;;  (define-struct alpha (a b c))
  ;;  (define-struct beta  (d e f))
  ;;
  ;;we want to expand:
  ;;
  ;;  (struct-case ?expr
  ;;    ((alpha a b)
  ;;     (do-this))
  ;;    ((beta d e f)
  ;;     (do-that))
  ;;    (else
  ;;     (do-other)))
  ;;
  ;;into:
  ;;
  ;;  (let ((v ?expr))
  ;;    (if ($struct/rtd? v (type-descriptor alpha))
  ;;        (let ((a ($struct-ref v 0))
  ;;              (b ($struct-ref v 1)))
  ;;          (do-this))
  ;;      (if ($struct/rtd? v (type-descriptor beta))
  ;;          (let ((d ($struct-ref v 0))
  ;;                (e ($struct-ref v 1))
  ;;                (f ($struct-ref v 2)))
  ;;            (do-that))
  ;;        (begin
  ;;          (do-other)))))
  ;;
  ;;notice that: in the clauses the pattern "(alpha  a b)" must list the fields A and
  ;;B in the same order in which they appear in the struct type definition.
  ;;
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ?expr ?clause ...)
	 (with-syntax ((BODY (%generate-body #'(?clause ...))))
	   #'(let ((v ?expr))
	       (import (only (vicare system $structs)
			     $struct-ref $struct/rtd?))
	       BODY)))))

    (define (%generate-body clauses-stx)
      (syntax-case clauses-stx (else)
        (()
	 (with-syntax ((INPUT-FORM stx))
	   #'(error 'compiler "unknown struct type" v 'INPUT-FORM)))

        (((else ?body0 ?body ...))
	 #'(let () ?body0 ?body ...))

        ((((?struct-name ?field-name ...) ?body0 ?body ...) . ?other-clauses)
	 (identifier? #'?struct-name)
         (with-syntax
	     ((RTD		#'(type-descriptor ?struct-name))
	      ((FIELD-NAM ...)  (%filter-field-names #'(?field-name ...)))
	      ((FIELD-IDX ...)	(%enumerate #'(?field-name ...) 0))
	      (ALTERN		(%generate-body #'?other-clauses)))
	   #'(if ($struct/rtd? v RTD)
		 (let ((FIELD-NAM ($struct-ref v FIELD-IDX))
		       ...)
		   ?body0 ?body ...)
	       ALTERN)))))

    (define (%filter-field-names field*.stx)
      ;;FIELD*.STX  must be  a  syntax object  holding a  list  of identifiers  being
      ;;underscores or struct  field names.  Filter out the underscores  and return a
      ;;list of identifiers representing the true field names.
      ;;
      (syntax-case field*.stx ()
        (() '())
        ((?field-name . ?other-names)
	 (eq? '_ (syntax->datum #'?field-name))
	 (%filter-field-names #'?other-names))
        ((?field-name . ?other-names)
	 (cons #'?field-name (%filter-field-names #'?other-names)))
	))

    (define (%enumerate field*.stx next-field-idx)
      ;;FIELD*.STX  must be  a  syntax object  holding a  list  of identifiers  being
      ;;underscores  or  struct  field  names.    NEXT-FIELD-IDX  must  be  a  fixnum
      ;;representing the index of the first field in FIELD*.STX.
      ;;
      ;;Return  a  list  of  fixnums  representing  the  indexes  of  the  fields  in
      ;;FIELD*.STX, discarding the fixnums matching underscores.
      ;;
      (syntax-case field*.stx ()
        (() '())
        ((?field-name . ?other-names)
	 (eq? '_ (syntax->datum #'?field-name))
	 (%enumerate #'?other-names (fxadd1 next-field-idx)))
        ((?field-name . ?other-names)
	 (cons next-field-idx (%enumerate #'?other-names (fxadd1 next-field-idx))))
	))

    (main stx)))

(define-syntax define-structure
  ;;A syntax to define struct types for compatibility with the notation used in Oscar
  ;;Waddell's thesis; it allows  the definition of struct types in  which some of the
  ;;fields are initialised  by the maker with default values,  while other fields are
  ;;initialised with arguments handed to the maker.
  ;;
  ;;Synopsis:
  ;;
  ;;  (define-structure ?name
  ;;    (?field-without ...)
  ;;    ((?field-with ?default)
  ;;	 ...))
  ;;
  ;;where: ?NAME is the struct type name, ?FIELD-WITHOUT are identifier names for the
  ;;fields  without default,  ?FIELD-WITH are  identifier names  for the  fields with
  ;;default, ?DEFAULT are the default values.
  ;;
  ;;The maker accepts a number of arguments equal to the number of ?FIELD-WITHOUT, in
  ;;the same order in which they appear in the struct definition.
  ;;
  ;;(It is a bit ugly...  Marco Maggi; Oct 10, 2012)
  ;;
  (lambda (stx)
    (define (%format-id ctxt template-str . args)
      (datum->syntax ctxt (string->symbol
			   (apply format template-str (map syntax->datum args)))))
    (syntax-case stx ()
      ((_ ?name (?field-without-default ...) ((?field-with-default ?default) ...))
       (identifier? #'?name)
       (let ((name.id #'?name))
	 (with-syntax
	     ((PRED			(%format-id name.id "~s?" name.id))
	      (MAKER			(%format-id name.id "make-~s" name.id))
	      ((GETTER ...)		(map (lambda (x)
					       (%format-id name.id "~s-~s" name.id x))
					  #'(?field-without-default ... ?field-with-default ...)))
	      ((UNSAFE-GETTER ...)	(map (lambda (x)
					       (%format-id name.id "$~s-~s" name.id x))
					  #'(?field-without-default ... ?field-with-default ...)))
	      ((SETTER ...)		(map (lambda (x)
					       (%format-id name.id "set-~s-~s!" name.id x))
					  #'(?field-without-default ... ?field-with-default ...)))
	      ((UNSAFE-SETTER ...)	(map (lambda (x)
					       (%format-id name.id "$set-~s-~s!" name.id x))
					  #'(?field-without-default ... ?field-with-default ...))))
	   #'(module (?name PRED
			    GETTER ... UNSAFE-GETTER ...
			    SETTER ... UNSAFE-SETTER ...
			    MAKER)
	       (module private
		 (?name PRED
			GETTER ... UNSAFE-GETTER ...
			SETTER ... UNSAFE-SETTER ...
			MAKER)
		 (define-struct ?name
		   (?field-without-default ... ?field-with-default ...)))
	       (module (MAKER)
		 (define (MAKER ?field-without-default ...)
		   (import private)
		   (MAKER ?field-without-default ... ?default ...)))
	       (module (?name PRED
			      GETTER ... UNSAFE-GETTER ...
			      SETTER ... UNSAFE-SETTER ...)
		 (import private))))))

      ((_ ?name (?field ...))
       (identifier? #'?name)
       #'(define-struct ?name (?field ...)))
      )))


;;;; helper functions

(define (print-compiler-warning-message template . args)
  (when (option.verbose?)
    (let ((P (current-error-port)))
      (display "vicare: compiler warning: " P)
      (apply fprintf P template args)
      (newline P))))

;;; --------------------------------------------------------------------

;;FIXME To  be removed at the  next boot image  rotation.  (Marco Maggi; Tue  Sep 30,
;;2014)
(define (void-object? x)
  (eq? x (void)))

;;; --------------------------------------------------------------------

(define (remq1 x ls)
  ;;Scan the list  LS and remove only the  first instance of object X,  using EQ?  as
  ;;comparison function; return the resulting list which may share its tail with LS.
  ;;
  (if (pair? ls)
      (if (eq? x (car ls))
	  (cdr ls)
	(let ((t (remq1 x (cdr ls))))
	  (cond ((eq? t (cdr ls))
		 ls)
		(else
		 (cons (car ls) t)))))
    '()))

(define (union s1 s2)
  ;;Return a  list which  is the  union between  the lists  S1 and  S2, with  all the
  ;;duplicates removed.
  ;;
  (define (add* s1 s2)
    (if (pair? s1)
	(add (car s1)
	     (add* (cdr s1) s2))
      s2))
  (define (add x s)
    (if (memq x s)
	s
      (cons x s)))
  (cond ((null? s1) s2)
	((null? s2) s1)
	(else
	 (add* s1 s2))))

(define (difference s1 s2)
  ;;Return a list holding  all the elements from the list S1 not  present in the list
  ;;S2.
  ;;
  (define (rem* s2 s1)
    (if (pair? s2)
	(remq1 (car s2)
	       (rem* (cdr s2) s1))
      s1))
  (cond ((null? s1) '())
	((null? s2) s1)
	(else
	 (rem* s2 s1))))


(define current-core-eval
  (make-parameter (lambda (x)
		    ((compile-core-expr x)))
    (lambda (obj)
      (if (procedure? obj)
	  obj
	(procedure-argument-violation 'current-core-eval
	  "expected procedure as parameter value" obj)))))

(define (eval-core x)
  ;;This function is used to compile  fully expanded R6RS programs, invoke libraries,
  ;;implement R6RS's eval function, compile right-hand sides of syntax definitions.
  ;;
  ((current-core-eval) x))


;;;; mapping primitive symbol names to location gensyms

(define current-primitive-locations
  ;;Closure upon a function capable of  retrieving a core primitive's location gensym
  ;;given its symbol name.   Notice that this is not a  parameter because: whenever a
  ;;new procedure is set some initialisation must be performed.
  ;;
  ;;The referenced function will allow this computation:
  ;;
  ;;   ((current-primitive-locations) 'display)
  ;;   => ?display-loc-gensym
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
		(expression-return-value-violation 'current-primitive-locations
		  "expected symbol as return value from CURRENT-PRIMITIVE-LOCATIONS procedure"
		  obj))))
	(else
	 (compiler-internal-error #f __who__
	   "while building boot image: primitive missing from makefile.sps" name))))


(module (compile-core-expr-to-port
	 compile-core-expr
	 compile-core-expr->code
	 core-expr->optimized-code
	 core-expr->optimisation-and-core-type-inference-code
	 core-expr->assembly-code)

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
    ;;This  is *the*  commpiler function.   It  transforms a  core language  symbolic
    ;;expression into a code object.
    ;;
    (%parse-compilation-options core-language-sexp
      (lambda (core-language-sexp)
	(let* ((p (recordize core-language-sexp))
	       (p (optimize-direct-calls p))
	       (p (optimize-letrec p))
	       (p (source-optimize p)))
	  (when (optimizer-output)
	    (pretty-print (unparse-recordized-code/pretty p) (current-error-port)))
	  (let* ((p (rewrite-references-and-assignments p))
		 (p (if (perform-core-type-inference)
			(core-type-inference p)
		      p))
		 (p (if (and (perform-core-type-inference)
			     (perform-unsafe-primrefs-introduction))
			(introduce-unsafe-primrefs p)
		      p))
		 (p (sanitize-bindings p))
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
	    (let ((code* (assemble-sources thunk?-label code-object-sexp*)))
	      ;;CODE* is  a list of code  objects; the first is  the one representing
	      ;;the initialisation  expression, the others are  the ones representing
	      ;;the CLAMBDAs.   The initialisation expression's code  object contains
	      ;;references to all the CLAMBDA code objects.
	      (car code*)))))))

  (define (core-expr->optimized-code core-language-sexp)
    ;;This is a utility function used for debugging and inspection purposes; it is to
    ;;be used to inspect the result of optimisation.
    ;;
    (%parse-compilation-options core-language-sexp
      (lambda (core-language-sexp)
	(let* ((p (recordize core-language-sexp))
	       (p (optimize-direct-calls p))
	       (p (optimize-letrec p))
	       (p (source-optimize p))
	       (p (rewrite-references-and-assignments p))
	       (p (if (perform-core-type-inference)
	       	      (core-type-inference p)
	       	    p))
	       (p (if (and (perform-core-type-inference)
	       		   (perform-unsafe-primrefs-introduction))
	       	      (introduce-unsafe-primrefs p)
	       	    p)))
	  (unparse-recordized-code/pretty p)))))

  (define (core-expr->optimisation-and-core-type-inference-code core-language-sexp)
    ;;This is a utility function used for debugging and inspection purposes; it is to
    ;;be used to inspect the result of optimisation.
    ;;
    (%parse-compilation-options core-language-sexp
      (lambda (core-language-sexp)
	(let* ((p (recordize core-language-sexp))
	       (p (optimize-direct-calls p))
	       (p (optimize-letrec p))
	       (p (source-optimize p))
	       (p (rewrite-references-and-assignments p))
	       (p (if (perform-core-type-inference)
	       	      (core-type-inference p)
	       	    p))
	       (p (if (and (perform-core-type-inference)
	       		   (perform-unsafe-primrefs-introduction))
	       	      (introduce-unsafe-primrefs p)
	       	    p)))
	  (unparse-recordized-code/pretty p)))))

  (define (core-expr->assembly-code core-language-sexp)
    ;;This is  a utility  function used  for debugging  and inspection  purposes.  It
    ;;transforms  a symbolic  expression representing  core language  into a  list of
    ;;sublists, each sublist  representing assembly language instructions  for a code
    ;;object.
    ;;
    (%parse-compilation-options core-language-sexp
      (lambda (core-language-sexp)
	(let* ((p (recordize core-language-sexp))
	       (p (optimize-direct-calls p))
	       (p (optimize-letrec p))
	       (p (source-optimize p)))
	  (let* ((p (rewrite-references-and-assignments p))
		 (p (if (perform-core-type-inference)
			(core-type-inference p)
		      p))
		 (p (if (and (perform-core-type-inference)
			     (perform-unsafe-primrefs-introduction))
			(introduce-unsafe-primrefs p)
		      p))
		 (p (sanitize-bindings p))
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
	    code-object-sexp*)))))

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
	(parameterize ((gensym-prefix "L")
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

  #| end of module: compile-core-expr |# )


;;;; struct types used to represent code in the core language
;;
;;The struct types  defined in this code  page are used by the  function RECORDIZE to
;;represent  code in  the core  language in  a way  which is  better inspectable  and
;;optimizable.
;;

(define-structure prelex
  ;;Whenever in  core language  forms a  lex gensym appears  to define,  reference or
  ;;assign a variable  binding: an instance of this type  appears in recordised code.
  ;;This is to allow properties to be attached to bindings.
  ;;
  (name
		;The lex gensym  representing the binding name in  the core language;
		;in practice useful only for humans when debugging.
   )
  ((operand		#f)
		;Multipurpose  field.   When   unused  it  is  set   to  false.   The
		;documentation of  uses is long:  see the individual  compiler passes
		;for details.
   (source-referenced?	#f)
		;Boolean.  True  if this PRELEX  is referenced  at least once  in the
		;code.  See also the field RESIDUAL-REFERENCED?.
   (source-assigned?	#f)
		;Boolean or loc gensym.
		;
		;When a  boolean: true when  the binding  has been used  as left-hand
		;side in a SET!  form; false when the binding is UNassigned.
		;
		;When  a loc  gensym: this  PRELEX structure  represents a  top level
		;binding defined  by a BIND struct,  whose value is contained  in the
		;VALUE slot of the loc gensym itself.
		;
		;In  the following  example  of standard  language  form, the  PRELEX
		;replacing X will have this field set to #f:
		;
		;   (let ((x 1))
		;     (display x))
		;
		;In  the following  example  of standard  language  form, the  PRELEX
		;replacing X will have this field set to #t:
		;
		;   (let ((x 1))
		;     (set! x (do-something))
		;     x)
		;
		;In  the following  example  of standard  language  form, the  PRELEX
		;replacing X will have this field set to the loc gensym:
		;
		;   (library (demo)
		;     (export x)
		;     (import (rnrs))
		;     (define x 1))
		;

   (residual-referenced? #f)
		;Boolean,  this field  is used  only by  the source  optimizer.  When
		;optimizing source code, an attempt  will be made to substitute every
		;reference to the binding this struct represents with an expansion of
		;the referenced value.
		;
		;For example, an attempt will be made to simplify:
		;
		;   (let ((x 1) (y 2)) (list x y))
		;   ==> (let ((x 1) (y 2)) (list 1 2))
		;
		;When such optimization  attempts fail, the reference  to variable is
		;just left in place and this field is set to true.  So the meaning of
		;this field is: still referenced after optimization attempt.
   (residual-assigned?   #f)
		;Boolean,  this field  is used  only by  the source  optimizer.  When
		;optimizing  source code,  an attempt  will be  made to  remove every
		;assignment to the binding this struct represents if the new value is
		;never used.
		;
		;For example, an attempt will be made to simplify:
		;
		;   (let ((x 1))
		;     (set! x (do-something))
		;     1)
		;   ==> (let ((x 1))
		;         (do-something)
		;         1)
		;
		;because X is never referenced.
		;
		;When such optimization attempts fail,  the assignment to variable is
		;just left in place and this field is set to true.  So the meaning of
		;this field is: still assigned after optimization attempt.
   (referenced-clambda	#f)
		;False  of a  CLAMBDA  struct.   False if  this  struct represents  a
		;binding  referencing  a   non-CLAMBDA  right-hand  side  expression.
		;Otherwise this  variable represents a binding  whose right-hand side
		;is  a  CLAMBDA expression,  and  the  value  of  this field  is  the
		;referenced CLAMBDA.
		;
		;After the compiler pass "sanitise  bindings" has been performed: all
		;the PRELEX  structs defined in  BIND structs have a  non-CLAMBDA RHS
		;expression; all  the PRELEX  structs defined in  FIX structs  have a
		;CLAMBDA RHS expression.
		;
		;This field  is used only in  the compiler pass "optimise  for direct
		;jumps".
   (global-location      #f)
		;When the  binding described by  this struct  is a top  level binding
		;(defined by  the form  LIBRARY-LETREC* in  the core  language): this
		;field is set to the loc  gensym associated to the binding.  When the
		;binding described by this struct is a local one: this field is #f.
		;
		;At run-time: the value of a top level binding is stored in the VALUE
		;field of its loc gensym.  Such  field is accessed with the operation
		;$SYMBOL-VALUE and mutated with the operation $SET-SYMBOL-VALUE!.
		;
		;NOTE Bindings defined by a  previously evaluated REPL expression are
		;not described by  a PRELEX struct, so this field  has no relation to
		;them.
   ))

(case-define* make-prelex-for-tmp-binding
  ;;Build and return a unique PRELEX struct meant to be used for a compiler-generated
  ;;binding, which  will be referenced but  not assigned.  Since we  know the binding
  ;;will be  referenced (otherwise the compiler  would not generate it):  we mark the
  ;;PRELEX as source referenced.
  ;;
  (()
   (receive-and-return (tmp)
       (make-prelex (gensym "tmp"))
     (set-prelex-source-referenced?! tmp #t)))
  (({prel prelex?})
   ;;The  init value  of the  binding will  be, in  some way,  related to  the PRELEX
   ;;argument PREL; so we reuse the name of PREL as name of the returned PRELEX.
   (receive-and-return (tmp)
       (make-prelex (prelex-name prel))
     (set-prelex-source-referenced?! tmp #t))))

;;; --------------------------------------------------------------------

;;An  instance of  this type  represents a  SET!  form  in which  the left-hand  side
;;references a lexical binding represented by a struct instance of type PRELEX.
;;
(define-struct assign
  (lhs
		;A struct instance of type PRELEX representing the binding.
   rhs
		;A struct instance  representing an expression evaluating  to the new
		;variable's value.
   ))

;;; --------------------------------------------------------------------

;;An instance  of this type represents  a form in a  sequence of forms inside  a core
;;language BEGIN.
;;
(define-struct seq
  (e0
		;A struct instance  representing the first form in  the sequence.  It
		;can be a nested SEQ struct.
   e1
		;A struct  instance representing the  last form in the  sequence.  It
		;can be a nested SEQ struct.
   ))

(define-syntax multiple-forms-sequence
  (syntax-rules ()
    ((_ ?expr)
     ?expr)
    ((_ ?expr ... ?last-expr)
     (make-seq (multiple-forms-sequence ?expr ...) ?last-expr))))

;;An instance of this type represents an IF form.
;;
(define-struct conditional
  (test
		;A struct instance representing the test form.
   conseq
		;A struct instance representing the consequent form.
   altern
		;A struct instance representing the alternate form.
   ))

;;An instance of this type represents a function call; there are special
;;cases of this:
;;
;;* When this FUNCALL represents a plain function application:
;;
;;     (funcall ?op ?rand*)
;;
;;  the field OP is a struct  representing an expression that supposedly evaluates to
;;  a closure object; the field RAND* is  set to a list of structs representing forms
;;  that evaluate to the arguments.
;;
;;* When this FUNCALL represents an annotated function application and debugging mode
;;  is active:
;;
;;     (funcall (primref debug-call) (?src/expr ?rator ?rand ...))
;;
;;  the  field OP  is a  struct instance  of type  PRIMREF referencing  the primitive
;;  DEBUG-CALL; the field RAND* is a list in which:
;;
;;  - the  1st item is a struct  instance of type CONSTANT whose field  holds a pair:
;;    its  car is  #f or  the source  input port  identifier; its  cdr is  the source
;;    expression;
;;
;;  - the 2nd item a struct instance representing a form that supposedly evaluates to
;;    a closure;
;;
;;  - the tail is a list of  struct instances representing forms that evaluate to the
;;    arguments.
;;
;;* When the FUNCALL represents a SET! on a top level binding:
;;
;;     (funcall (primref $init-symbol-value!) ?rand*)
;;
;;  the   field   OP   is   a   struct   of   type   PRIMREF   holding   the   symbol
;;  "$init-symbol-value!"; the field RAND* is a list of 2 elements: a struct instance
;;  of  type CONSTANT  holding  the loc  gensym  of the  binding;  a struct  instance
;;  representing an expression evaluating to the new value.
;;
(define-struct funcall
  (op rand*))

;;An instance  of this  type represents  a call to  a foreign  function, usually  a C
;;language function.
;;
(define-struct forcall
  (op
		;A string representing the name of the foreign function.
   rand*
		;A list of struct instances representing the arguments.
   ))

;;; --------------------------------------------------------------------

;;Instances of this struct type represent  bindings at the lowest level in recordized
;;code.  We can  think of BIND forms as LET  core language forms: a BIND is  a set of
;;non-recursive bindings in which the order of evaluation of the right-hand sides can
;;be freely changed.
;;
;;Down below, a compiler pass will process the BIND struct instances and convert them
;;to  code that  evaluates the  RHS  expressions and  store their  return value  into
;;appropriately allocated  Scheme stack machine words;  a machine word for  every LHS
;;will be allocated, each LHS will represent an actual "local variable".
;;
(define-struct bind
  (lhs*
		;When  code is  first  recordized  and optimized:  a  list of  struct
		;instances  of type  PRELEX representing  the binding  lexical names.
		;Later: a list of struct instances of type VAR representing some kind
		;of memory location.
   rhs*
		;A list of struct instances  representing recordized code which, when
		;evaluated, will return the binding's initial values.
   body
		;A struct  instance representing recordized  code to be  evaluated in
		;the region of the bindings.
   ))

;;Like BIND, but the  RHS* field holds struct instances of type  CLAMBDA and the LHS*
;;field  holds  PRELEX structures  representing  bindings  that are  never  assigned,
;;neither by the  BODY nor by the RHS* themselves.   We can think of a FIX  form as a
;;LETREC form in which the right-hand sides are functions.
;;
;;For details on the meaning of FIX, see the paper (available on the Net):
;;
;;   Oscar Waddell, Dipanwita Sarkar, R. Kent Dybvig.  "Fixing Letrec: A Faithful Yet
;;   Efficient Implementation of Scheme's Recursive Binding Construct"
;;
(define-struct fix
  (lhs*
		;A list  of PRELEX  structs representing  bindings with  CLAMBDA init
		;expressions.
   rhs*
		;A list of CLAMBDA structures.
   body
		;A struct  instance representing recordized  code to be  evaluated in
		;the region of the bindings.
   ))

;;An instance of this type represents a LETREC form.
;;
(define-struct recbind
  (lhs*
		;A list of struct instances of type PRELEX describing the bindings.
   rhs*
		;A  list  of  struct   instances  representing  the  right-hand  side
		;expressions   of  the   bindings,   that   is:  the   initialisation
		;expressions.
   body
		;A struct instance representing the sequence of body forms.
   ))

;;An instance of this type represents a LETREC* or LIBRARY-LETREC* form.
;;
;;The difference between a struct representing  a LETREC* and a struct representing a
;;LIBRARY-LETREC* form is only in the PRELEX structures.
;;
(define-struct rec*bind
  (lhs*
		;A list of struct instances of type PRELEX describing the bindings.
   rhs*
		;A  list  of  struct   instances  representing  the  right-hand  side
		;expressions   of  the   bindings,   that   is:  the   initialisation
		;expressions.
   body
		;A struct instance representing the sequence of body forms.
   ))

;;An  instance  of this  type  represents  a reference  to  a  primitive function  or
;;primitive operation.   A "primitive function"  is a  function exported by  the boot
;;image.  A  "primitive operation" is like  a macro for high-level  assembly language
;;and  it  is  implemented  by  the  boot image.   Given  a  standard  language  form
;;representing a primitive function application:
;;
;;   (list 1 2 3)
;;
;;the expander generates the core language form:
;;
;;   ((primitive list) '1 '2 '3)
;;
;;and its recordised representation is:
;;
;;   (funcall (primref 'list) (constant 1) (constant 2) (constant 3))
;;
;;NOTE We  have to  remember that:  the location  gensym of  a primitive  function is
;;generated by the  expander while processing the boot image  sources, and hard-coded
;;in  the boot  image file  itself; from  that point  on: such  location gensyms  are
;;universal  constants.  Given  the  public  name of  a  primitive  function: we  can
;;retrieve its location gensym with:
;;
;;   (getprop 'list system-value-gensym) => ?loc-gensym-of-list
;;
(define-struct primref
  (name
		;A symbol being the public name of the primitive function.
   ))

(module (mk-primref)
  ;;NOTE We might cache the PRIMREF structs in an attempt to allocate less memory and
  ;;cause less garbage  collections.  I have tried this and,  while building the boot
  ;;image,  the result  is that  there  are 10  more garbage  collections, not  less.
  ;;(Marco Maggi; Mon Aug 25, 2014)
  ;;
  (define mk-primref make-primref)
  ;; (define-constant CACHE
  ;;   (make-eq-hashtable))
  ;; (define (mk-primref name)
  ;;   (or (hashtable-ref CACHE name #f)
  ;; 	(receive-and-return (ref)
  ;; 	    (make-primref name)
  ;; 	  (hashtable-set! CACHE name ref))))
  #| end of module |# )

;;An instance of this type represents a  LAMBDA or CASE-LAMBDA form.  Such forms have
;;the syntax:
;;
;;   (lambda ?formals ?body0 ?body ...)
;;   (case-lambda (?formals ?body0 ?body ...) ...)
;;   (annotated-case-lambda ?annotation (?formals ?body0 ?body ...))
;;
;;but LAMBDA forms are converted to CASE-LAMBDA forms:
;;
;;   (lambda ?formals ?body0 ?body ...)
;;   ===> (case-lambda (?formals ?body0 ?body ...))
;;
(define-struct clambda
  (label
		;A unique  gensym associated to  the code object that  will implememt
		;this  CLAMBDA.   It will  become  the  name  of the  assembly  label
		;representing the  entry point  to the CLAMBDA  machine code.   It is
		;used to generate, when possible,  direct jumps to this CLAMBDA entry
		;point  (rather than  performing a  full call  to a  run-time closure
		;object).
   cases
		;A list  of struct  instances of  type CLAMBDA-CASE  representing the
		;clauses.
   cp
		;Initialised to #f, it  is set to the struct instance  of type VAR to
		;which the CLOSURE-MAKER  wrapping this CLAMBDA is  bound.  CP stands
		;for "Closure Pointer".
   freevar*
		;Initialised to #f,  it is set to a list  of VAR structs representing
		;the free variables referenced by this CLAMBDA.
   name
		;An annotation representing the name of the closure if available.  It
		;can be:
		;
		;* #f when no information is available.
		;
		;* A symbol representing the closure name itself.
		;
		;* A  pair whose car  is #f or a  name representing the  closure name
		;itself, and whose cdr is #f or the content of the SOURCE field in an
		;ANNOTATION struct.
   ))

;;An  instance of  this  type  represents a  CASE-LAMBDA  clause.   Given a  symbolic
;;expression representing a CASE-LAMBDA:
;;
;;   (case-lambda (?formals ?body0 ?body ...) ...)
;;
;;and knowing that a LAMBDA sexp is converted to CASE-LAMBDA:
;;
;;   (lambda ?formals ?body0 ?body ...)
;;   ===> (case-lambda (?formals ?body0 ?body ...))
;;
;;an instance of this type is used to represent a single clause:
;;
;;   (?formals ?body0 ?body ...)
;;
;;holding informations needed to select it among the multiple choices of the original
;;form.
;;
(define-struct clambda-case
  (info
		;A struct instance of type CASE-INFO.
   body
		;A struct instance representing the sequence of ?BODY forms.
   ))

;;An instance of  this type represents easily accessible informations  on the formals
;;of a single clause of CASE-LAMBDA form:
;;
;;   (?formals ?body0 ?body ...)
;;
;;We know that the following cases for ?FORMALS are possible:
;;
;;   (()           ?body0 ?body ...)
;;   ((a b c)      ?body0 ?body ...)
;;   ((a b c . d)  ?body0 ?body ...)
;;   (args         ?body0 ?body ...)
;;
;;information is encoded in the fields as follows:
;;
;;* If ?FORMALS  is null or a proper list:  ARGS is null or a proper  list, PROPER is
;;#t.
;;
;;* If ?FORMALS is  a symbol: ARGS is a proper list holding  a single item, PROPER is
;;#f.
;;
;;* If ?FORMALS is an IMproper list: ARGS is a proper list holding multiple elements,
;;PROPER is #f.
;;
(define-struct case-info
  (label
		;A unique  gensym for  this CASE-LAMBDA clause.   It will  become the
		;name  of the  assembly label  representing the  entry point  to this
		;CLAMBDA clause.  It is used to generate, when possible, direct jumps
		;to this  clause (rather than  performing a  full call to  a run-time
		;closure object).
   args
		;In the earliest compiler passes: a  list of struct instances of type
		;PRELEX representing the ?FORMALS as follows:
		;
		;   (() ?body0 ?body ...)
		;   => ()
		;
		;   ((a b c) ?body0 ?body ...)
		;   => (prelex-a prelex-b prelex-c)
		;
		;   ((a b c . d) ?body0 ?body ...)
		;   => (prelex-a prelex-b prelex-c prelex-d)
		;
		;   (args ?body0 ?body ...)
		;   => (prelex-args)
		;
		;After  the compiler  pass  INTRODUCE-VARS is  performed:  a list  of
		;struct instances of type VAR  representing the formals with the same
		;format.
		;
		;In  the  latest compiler  passes:  a  pair  whose  car is  a  symbol
		;representing the  CPU register  holding the  pointer to  the current
		;closure object,  and whose cdr is  the list of formals  as described
		;above.
   proper
		;A boolean: true if ?FORMALS is a proper list, false if ?FORMALS is a
		;symbol or improper list.
   ))

;;Instances of this type represent datums in the core language.
;;
(define-struct constant
  (value
		;The datum from  the source, for example:  numbers, vectors, strings,
		;quoted lists...
   ))

(define-constant VOID-CONSTANT
  (make-constant (void)))

;;Structs of this  type are introduced by  the expander and consumed  by the compiler
;;pass  "core  type  inference".   These  structs  represents  type  informations  of
;;arbitrary expressions.
;;
(define-struct typed-expr
  (expr
		;A struct representing an expression as recordised code.
   type
		;A struct  of type CORE-TYPE-TAG representing  core type informations
		;of EXPR.
   ))


;;;; struct types used in middle-level code representation

;;Instances of this struct type represent  variables in recordized code.  An instance
;;of this struct  is created for each  lexical binding that has  survived the various
;;optimisation passes.
;;
(define-struct var
   (name
		;Lex gensym uniquely identifying this lexical binding in the original
		;core language expression.  For  bindings introduced by the compiler:
		;this is just a symbol.
    reg-conf
    frm-conf
    var-conf
		;The suffix "-conf" stands for "conflicts; "reg-conf" stands for "CPU
		;register conflicts"; "frm-conf" stands for "stack frame conflicts".
		;
		;Falses or sets,  as defined by the  module INTEGER-SET, representing
		;interference edges  between, respectively: live CPU  registers, live
		;FVAR structs, live VAR structs.  If  two structs are connected by an
		;interference edge: they  are alive at the same time,  so they cannot
		;be stored in the same CPU register or the same FVAR stack location.
    reg-move
    frm-move
    var-move
		;"reg-move"  stand for  "CPU  register move";  "frm-move" stands  for
		;"stack frame move"; "var-move" stands for "VAR struct move".
		;
		;Falses or sets,  as defined by the  module INTEGER-SET, representing
		;preference  edges between,  respectively: live  CPU registers,  live
		;FVAR structs, live  VAR structs.  If two structs are  connected by a
		;preference  edge:  somewhere  an Assembly  instruction  MOVE  exists
		;moving  a  value  between  the two  structs.
		;
		;While allocating CPU registers: it is more efficient to allocate the
		;same CPU register to two structs  connected by a preference edge, so
		;that the MOVE instruction can be discarded.
    loc
		;False or  FVAR struct or  CPU register symbol name  representing the
		;storage location of this variable.
    index
		;False or fixnum.  This is a  multipurpose field used in the compiler
		;passes "optimize  combinator calls/lift clambdas" and  "assign frame
		;sizes"; see the documentation of the compiler passes for details.
    global-location
		;False  or loc  gensym.   When  false: this  VAR  represents a  local
		;lexical binding.  When a loc gensym: this VAR represents a top level
		;lexical binding  and the loc  gensym is the one  to use to  hold its
		;current value in the "value" slot.
    ))

(case-define make-unique-var
  (()
   (make-unique-var 'tmp))
  ((name)
   (make-var name #f #f #f #f #f #f #f #f #f)))

;;; --------------------------------------------------------------------

;;Instances of  this struct type  are substitute  instances of FUNCALL  in recordized
;;code when it is possible to perform a faster "direct jump call" to a CLAMBDA clause
;;with the correct number of arguments, rather than a "full closure object call" with
;;number of arguments that must be validated.
;;
;;Example, given the definition:
;;
;;   (define func
;;     (case-lambda
;;      ((a)
;;       (do-this a))
;;      ((a b)
;;       (do-that a b))))
;;
;;the closure application:
;;
;;   (func 1)
;;
;;can be implemented as a direct jump to the first branch of the CASE-LAMBDA, because
;;we know at compile-time that there is only one operand.
;;
(define-struct jmpcall
  (label
		;A gensym.  It is the gensym stored  in the LABEL field of the target
		;CASE-INFO struct.
   op
		;A  struct   instance  representing  the  operator   of  the  closure
		;application.
   rand*
		;A list of struct instances  representing the operands of the closure
		;application.
   ))

;;; --------------------------------------------------------------------

;;Instances of this  type represent form that, evaluated at  run-time, will build and
;;return closure objects.
;;
(define-struct closure-maker
  (code
		;Before  the   lambda  lifting   compiler  pass:  a   CLAMBDA  struct
		;representing a function defined by  the input expression.  After the
		;lambda  lifting:  a CODE-LOC  struct  to  be  used to  generate  the
		;run-time closure object representing the function.
   freevar*
		;Before  the VAR  structs  representing captured  free variables  are
		;rewritten to  use the CP-REGISTER: null  or a proper list  of struct
		;instances of type VAR representing  the free variables referenced by
		;the generated closure object.
		;
		;After  the  VAR structs  representing  captured  free variables  are
		;rewritten to use the CP-REGISTER: null  or a proper list of structs.
		;Each struct can be:
		;
		;* A VAR struct representing  a self-reference: when a closure object
		;  references itself.  Example:
		;
		;   (let ((a ((primitive read))))
		;     (letrec ((f (lambda () (list a f))))
		;       f))
		;
		;  the LAMBDA is a non-combinator and it references itself.
		;
		;*  A  VAR struct  representing  a  free  variable defined  by  BIND.
		;  Example:
		;
		;     (let ((a ---))
		;       (lambda () a))
		;
		;* A PRIMOPCALL with the format:
		;
		;     (primopcall $cpref (?cpvar (constant ?index)))
		;
		;  which represents access to a free variable that is a free variable
		;  of an enclosing closure object; for example:
		;
		;     (let ((a ((primitive read))))
		;       (lambda () (lambda () a)))
		;
		;  the variable A is free for  both the outer and inner LAMBDA forms,
		;  so the inner  must access it from the closure  object slots of the
		;  outer.
		;
   ))

;;Instances of this type represent primitive operation applications.
;;
;;A "primitive operation" is assembly code that is integrated at the call site of the
;;primitive operation application; a primitive  operation application is similar to a
;;Scheme function application.   There are:
;;
;;* Primitive functions (bindings exported by  the boot image, implemented as closure
;;objects) which also have an alternative implementation as primitive operations.
;;
;;* System  primitive operations  that are  only primitive  operations, but  are also
;;bindings  exported  by   the  boot  image  via  a  system   library.   For  example
;;"$symbol-string", which  retrieves the  pretty string  name of a  symbol and  it is
;;exported by the library "(vicare system $symbols)".
;;
;;* Low-level primitive  operations that are only primitive  operations and represent
;;high-level assembly  instructions.  For example  "mref", which retrieves  a machine
;;word from a memory location.
;;
;;EXAMPLE  Let's consider  FX+ which  is both  a primitive  function and  a primitive
;;operation.  Given the standard Scheme code:
;;
;;   (fx+ 1 2)
;;
;;the result of its full expansion is:
;;
;;   (fx+ 1 2)
;;
;;and the result of its recordisation is:
;;
;;   (funcall (primref fx+) (constant 1) (constant 2))
;;
;;In a  low-level compiler  pass: FX+  is recognised as  primitive operation  and the
;;recordised representation becomes:
;;
;;   (primopcall fx+ (constant 1) (constant 2))
;;
;;an even lower compiler pass with transform PRIMOPCALL into the actuall asssembly code
;;implementing the primitive operation.
;;
;;NOTE Given the  symbol representing the public name of  a primitive operation which
;;is also  a binding exported  by some library (like  FX+ and $SYMBOL-STRING):  it is
;;possible  to  retrieve  informations  about the  implementation  of  the  primitive
;;operation by inspecting its property list.
;;
;;NOTE  Given the  symbol  representing  the public  name  of  a low-level  primitive
;;operation  representing a  high-level  assembly  instruction: there  is  no way  to
;;recognise it as such.
;;
(define-struct primopcall
  (op
		;A symbol representing the public name of a core primitive operation.
   rand*
		;A  list  of  struct instances  representing  recordized  expressions
		;which, when evaluated,  will return the arguments  of this primitive
		;call.
   ))

;;Instances  of  this  type  represent  the  application  of  a  high-level  Assembly
;;instruction.  High-level Assembly instructions are:
;;
;;   nop
;;   interrupt		return
;;   alloc		alloc-no-hooks
;;   !=			<		<=
;;   =			>		>=
;;   u<			u<=
;;   u>			u>=
;;   bset		bref		bswap!
;;   direct-jump	indirect-jump
;;   call-with-underflow-handler
;;   fl:!=		fl:<		fl:<=
;;   fl:=		fl:>		fl:>=
;;   fl:add!		fl:sub!
;;   fl:div!		fl:mul!
;;   fl:from-int	fl:load		fl:load-single
;;   fl:o!=		fl:o<		fl:o<=
;;   fl:o=		fl:o>		fl:o>=
;;   fl:shuffle		fl:store	fl:store-single
;;   fl:double->single	fl:single->double
;;   int+		int+/overflow
;;   int-		int-/overflow
;;   int*		int*/overflow
;;   int-quotient	int-remainder
;;   incr/zero?
;;   logand		logor		logxor
;;   move
;;   mset		mset32
;;   mref		mref32
;;   sll		sll/overflow
;;   sra		srl
;;
;;other high-level Assembly operations (like function calls) are represented by other
;;structs.
;;
;;High-level  Assembly  instructions have  structs  representing  recordised code  as
;;operands.   Each high-level  Assembly instruction  is transformed  into a  group of
;;target CPU Assembly instructions that actually implement the with CPU registers and
;;memory  locations as  operands;  some high-level  Assembly  instructions are  first
;;transformed into other, simpler, high-level Assembly instructions.
;;
(define-struct asmcall
  (instr
		;A symbol representing the name of a high-level Assembly instruction.
   rand*
		;A list of struct instances  representing recordised code which, when
		;evaluated, will result in the operands of this Assembly instruction.
   ))

(define-struct codes
  (list
		;A list of struct instances of type CLAMBDA.
   body
		;A struct instance representing recordized code.
   ))

;;Instances of  this type  represent constant  objects to be  hard coded  in compiled
;;code, whose binary representation is *not*  an exact integer that can be integrated
;;in  the generated  machine code.   For example:  the binary  representation of  the
;;boolean #t is  the integer #x3F, so  it can be integrated; a  literal Scheme vector
;;from the source code has a more complex representation, so it cannot be integrated.
;;
;;Scheme objects  wrapped in OBJECT  structures are  stored in the  relocation vector
;;associated to code objects.
;;
(define-struct object
  (val))

;;; --------------------------------------------------------------------

;;A lambda sexp in the source code:
;;
;;   (lambda ?formals ?body)
;;
;;is converted first into a CLAMBDA  struct, then into a CLOSURE-MAKER struct holding
;;the CLAMBDA:
;;
;;   (closure-maker ?clambda ?freevar* ?recursive)
;;
;;and then into a CLOSURE-MAKER struct holding a CODE-LOC:
;;
;;   (closure-maker ?code-loc ?freevar* ?recursive)
;;
;;A CODE-LOC  struct represents  a reference  to the  code object  that is  needed to
;;construct a  run-time closure object.   We need to  remember that a  closure object
;;contains a  reference to  a code  object, which in  turn contains  compiled machine
;;code.
;;
(define-struct code-loc
  (label
		;The gensym  of the referenced CLAMBDA.   It will become the  name of
		;the  assembly label  representing  the entry  point  to the  CLAMBDA
		;machine code.
   ))

(define-struct foreign-label
  ;;This struct represents the address of a  C language foreign function that must be
  ;;called.  It is  used in a low-level representation of  the FOREIGN-CALL macro and
  ;;FORCALL struct.
  ;;
  (label
		;A string representing the name of the C language foreign function.
   ))

;;Structs of this type are introduced by  the compiler pass "core type inference" and
;;consumed  by  the   compiler  passes  "introduce  unsafe   primrefs"  and  "specify
;;representation".  These structs represents type informations of operands in FUNCALL
;;structs and  derived "call" structs;  these informations  are consumed by  the core
;;primitive operation's implementation handlers.
;;
(define-struct known
  (expr
		;A struct representing an expression as recordised code.
   type
		;A struct  of type CORE-TYPE-TAG representing  core type informations
		;of EXPR.
   ))

(define-struct shortcut
  (body
   handler
   ))

;;; --------------------------------------------------------------------

;;Represent a machine word  on the Scheme stack, below the  address referenced by the
;;Frame Pointer Register (FPR).  The index is interpreted as follows:
;;
;;       high memory
;;   |                |
;;   |----------------|
;;   | return address | <-- frame pointer register (FPR)
;;   |----------------|
;;   |                | <-- idx = 1
;;   |----------------|
;;   |                | <-- idx = 2
;;   |----------------|
;;   |                | <-- idx = 3
;;   |----------------|
;;   |                |
;;       low memory
;;
;;It is used to  represent memory locations used as local  variables in the execution
;;of a Scheme function.
;;
(define-struct fvar
  (idx
		;A fixnum  representing the  index of  a machine  word on  the Scheme
		;stack.
   ))

(module (mkfvar)
  ;;Maker  function for  structs of  type FVAR.   It caches  structures based  on the
  ;;values of the argument, so that calling:
  ;;
  ;;   (mkfvar 123)
  ;;
  ;;multiple times always returns the same FVAR instance holding 123.
  ;;
  ;;NOTE When compiling the  boot image this index can go above  30, even though most
  ;;uses are below 10.  (Marco Maggi; Sat Aug 23, 2014)
  ;;
  ;;NOTE I have changed the implementation from  alist to hashtable, but, in truth, I
  ;;have done  no profiling  whatsoever to  verify that the  table version  is faster
  ;;(shame on me!).  (Marco Maggi; Sun Aug 24, 2014)
  ;;
  (define* (mkfvar {i fixnum?})
    (or (%cache-ref i)
	(receive-and-return (fv)
	    (make-fvar i)
	  (%cache-set! i fv))))

  (begin
    (define-constant CACHE
      (make-eq-hashtable))
    (define-syntax-rule (%cache-ref key)
      (hashtable-ref CACHE key #f))
    (define-syntax-rule (%cache-set! key val)
      (hashtable-set! CACHE key val)))

  ;; (begin
  ;;   (define CACHE '())
  ;;   (define-syntax-rule (%cache-ref key)
  ;;     (cond ((assv key CACHE)
  ;; 	     => cdr)
  ;; 	    (else #f)))
  ;;   (define-syntax-rule (%cache-set! key val)
  ;;     (set! CACHE (cons (cons key val) CACHE))))

  #| end of module: MKFVAR |# )

;;; --------------------------------------------------------------------

(define-struct locals
  ;;Used to wrap top level expressions and bodies of CLAMBDA clauses.
  ;;
  (vars
		;This  field  contains  values  of different  types  after  different
		;compiler passes.
		;
		;After the pass "impose evaluation order"
		;----------------------------------------
		;
		;A  possibly empty  proper  list of  VAR  structs representing  local
		;variables  used in  the code  represented  by the  BODY field;  such
		;locations will be allocated to CPU registers or Scheme stack words.
		;
		;After the pass "assign frame sizes"
		;-----------------------------------
		;
		;A  pair  whose  car  is  named  VARS.VEC  and  whose  cdr  is  named
		;VARS.SPILLABLE*.
		;
		;VARS.VEC  is a  vector of  VAR  structs representing  all the  local
		;variables in BODY.  Some of these  VAR structs have a FVAR struct in
		;their LOC  field: they have  been allocated to stack  locations; the
		;other  VAR structs  have #f  in their  LOC field:  they are  not yet
		;allocated.
		;
		;VARS.SPILLABLE* is a list of  VAR structs representing the subset of
		;VAR structs in VARS.VEC that have  #f in their LOC field.  These VAR
		;structs  can be  allocated to  CPU registers  or to  stack locations
		;(spilled).
   body
		;A struct representing recordised code.
   ))

(define-struct nfv
  ;;Represent a Scheme stack location used to hold an operand to an upcoming non-tail
  ;;function call.   Such locations are below  the return address to  the caller; the
  ;;scenario on the Scheme stack right after the "call" Assembly instruction is:
  ;;
  ;;      high memory
  ;;   |                |
  ;;   |----------------|
  ;;   | return address | <- Frame Pointer Register (FPR)
  ;;   |----------------|
  ;;   |  1st operand   | <- FPR - 1 * wordsize
  ;;   |----------------|
  ;;   |  2nd operand   | <- FPR - 2 * wordsize
  ;;   |----------------|
  ;;   |  3rd operand   | <- FPR - 3 * wordsize
  ;;   |----------------|
  ;;   |                | <- FPR - AA-REGISTER
  ;;   |----------------|
  ;;   |                |
  ;;       low memory
  ;;
  ;;so the  offset of the operands  with respect to  the FPR is negative.   While the
  ;;non-tail call  is prepared:  these stack  locations are not  part of  the current
  ;;stack frame,  rather they will be  part of the  next stack frame; hence  the name
  ;;"Next Frame Variables" (F* Yeah!).
  ;;
  (idx
		;A 1-based  non-negative fixnum  representing the  index of  the this
		;stack operand.
   loc
		;False or  a struct of type  FVAR representing the stack  location in
		;which this operand is stored.
   var-conf
   frm-conf
   nfv-conf
		;Falses or sets,  as defined by the  module INTEGER-SET, representing
		;interference  edges between,  respectively: live  VAR structs,  live
		;FVAR structs, live NFV structs.  If  two structs are connected by an
		;interference edge: they  are alive at the same time,  so they cannot
		;be stored in the same CPU register.
		;
		;The suffix "-conf" stands for conflict.
   ))

(define-struct non-tail-call-frame
  (rand*
		;Null or a proper list of NFV structs representing the stack operands
		;of the tail-call.
   live
		;This field is  used only in the compiler pass  "assign frame sizes";
		;it is set to false or  to a struct of type NON-TAIL-CALL-FRAME-SETS.
		;See the documentation of the compiler pass for details.
   body
		;A  struct  representing  recordised code.   It  contains  everything
		;needed to  compute values  for the register  operands and  the stack
		;operands.   The last  form of  the BODY  is a  NON-TAIL-CALL struct,
		;representing the  actual non-tail function call,  including the call
		;table describing the stack frame.
   ))

(define-struct non-tail-call
  ;;Represent a non-tail function call.
  ;;
  (target
		;False, a string or a gensym:
		;
		;* When false: this call is to a core primitive function.
		;
		;* When a string:  this call is to a foreign  C language function and
		;  the string is its name.
		;
		;*  When a  gensym: this  call is  a  jump to  the entry  point of  a
		;  combinator function.
   retval-var
		;False, VAR, NFV or FVAR struct:
		;
		;* When  false: it means  the return value  of this function  call is
		;  discarded; this function call is performed for its side effects.
		;
		;* When  non-false: it  represents the location  to which  the return
		;   value of  the  function call  must be  stored:  first the  callee
		;  function  stores its return  value into the AA-REGISTER,  then the
		;  caller moves it into RETVAL-VAR.
		;
		;When the function returns a single value: the return value stored in
		;RETVAL-VAR  is  the  actually  returned  Scheme  object.   When  the
		;function  returns  multiple  values:  the  return  value  stored  in
		;RETVAL-VAR is the  number of returned Scheme objects (0,  2 or more)
		;and the Scheme objects are on the Scheme stack.
   all-rand*
		;A list  comprising the register  operand and the stack  operands the
		;non-tail call:
		;
		;   (AAR APR CPR FPR PCR . rand*.nfv)
		;
		;where:  AAR, APR,  CPR, FPR,  PCR are  symbols representing  the CPU
		;register names being  the register operands; RAND*.NFV is  a list of
		;NFV structs representing locations on the Scheme stack that hold the
		;stack operands.
		;
		;After  the  pass "assign  frame  sizes":  the  list of  NFV  structs
		;RAND*.NFV is replaced  by the list of FVAR  structs representing the
		;actual stack locations:
		;
		;   (AAR APR CPR FPR PCR . rand*.fvar)
		;
   mask
		;False or the  livemask vector used to build the  non-tail call table
		;describing the stack frame.
   size
		;False or a positive fixnum representing  the size of the stack frame
		;(expressed in number  of machine words) of this  non-tail call.  Its
		;value is 1 or greater, because a call frame always contains at least
		;the return address.
   ))

(define-struct asm-instr
  ;;Represent an assembly instruction.
  ;;
  (op
		;Operand.
   dst
		;Destination machine word.
   src
		;Source machine word.
   ))

(define-struct disp
  ;;Represent the  displacement of a  machine word, 32-bit word,  byte or octet  in a
  ;;Scheme object.  The  Scheme object can either be stored  in the relocation vector
  ;;of the current code object, in a CPU register or on the Scheme stack.
  ;;
  (objref
		;A  struct  representing  recordised  code, for  example  a  CONSTANT
		;holding  an OBJECT  struct.  It  represents an  object from  which a
		;value must be extracted.
   offset
		;A CONSTANT struct  representing the offset of the  machine word that
		;must be extracted from the Scheme object referenced by OBJREF.
   ))


(define* (recordize input-expr)
  ;;Given a symbolic expression INPUT-EXPR representing  a form in the core language,
  ;;convert it into  a nested hierarchy of struct instances;  return the outer struct
  ;;instance.
  ;;
  ;;This function expects a symbolic expression with perfect syntax: no syntax errors
  ;;are  checked.   We  expect  this  function to  be  executed  without  errors,  no
  ;;exceptions should be raised unless an internal bug makes it happen.
  ;;
  ;;Recognise the following core language forms:
  ;;
  ;;   (library-letrec* ((?lhs ?loc ?rhs) ...) ?body)
  ;;   (quote ?datum)
  ;;   (if ?test ?consequent ?alternate)
  ;;   (set! ?lhs ?rhs)
  ;;   (begin ?body0 ?body ...)
  ;;   (let     ((?lhs ?rhs) ...) ?body)
  ;;   (letrec  ((?lhs ?rhs) ...) ?body)
  ;;   (letrec* ((?lhs ?rhs) ...) ?body)
  ;;   (case-lambda (?formals ?body) ...)
  ;;   (annotated-case-lambda ?annotation (?formals ?body) ...)
  ;;   (lambda ?formals ?body)
  ;;   (foreign-call "?function-name" ?arg ...)
  ;;   (primitive ?prim)
  ;;   (annotated-call ?annotation ?fun ?arg ...)
  ;;   ?lex
  ;;   (?func ?arg ...)
  ;;   (typed-expr ?expr ?core-type)
  ;;
  ;;where:  a standalone  ?LEX atom  is  a lex  gensym, interpreted  as reference  to
  ;;binding; ?LHS stands for "left-hand side" and it is a lex gensym; ?RHS stands for
  ;;"right-hand side"; ?LOC is a loc gensym;  ?PRIM is a symbol representing the name
  ;;of a primitive function.
  ;;
  ;;About the argument CTXT
  ;;-----------------------
  ;;
  ;;Whenever possible we want closure objects to  be annotated with their name in the
  ;;original source  code; the name  of a closure  is the lex  gensym to which  it is
  ;;bound.  Examples:
  ;;
  ;;   (define a
  ;;     (lambda () ;annotated: a
  ;;       ---))
  ;;
  ;;   (define a
  ;;     (begin
  ;;       (do-something)
  ;;       (lambda () ;annotated: a
  ;;         ---))
  ;;
  ;;   (define a
  ;;     (let ((a 1) (b 2))
  ;;       (do-something)
  ;;       (lambda () ;annotated: a
  ;;         ---))
  ;;
  ;;   (set! a (lambda () ;annotated: a
  ;;             ---))
  ;;
  ;;   (set! a (begin
  ;;             (do-something)
  ;;             (lambda () ;annotated: a
  ;;               ---))
  ;;
  ;;   ((lambda (x) x)
  ;;      (lambda (y) y)) ;annotated: x
  ;;
  ;;This is what the CTXT argument in the subfunctions is for; it is carefully handed
  ;;to the functions that process forms that  might evaluate to a closure and finally
  ;;used to annotate struct instances of type CLAMBDA.
  ;;
  ;;The CTXT argument is handled as follows:
  ;;
  ;;* Upon entering the input expression, no name is defined in the code: CTXT is set
  ;;to #f.
  ;;
  ;;* Upon entering the right-hand side expression of a lexical binding definition or
  ;;assignment: CTXT is  set to ?LEX-GENSYM, where ?LEX-GENSYM is  the left-hand side
  ;;of the definition or assignment.
  ;;
  ;;* When processing ?RATOR in an application form:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;the current value  of CTXT is wrapped in  a list of a single  item.  For example,
  ;;while processing:
  ;;
  ;;   (let ((x ((lambda (y) y) 1)))
  ;;     ?body)
  ;;
  ;;upon entering the  operator "(lambda (y) y)",  CTXT is set to "(x)".   Notice that the
  ;;operator "(lambda (y) y)" itself has no name.
  ;;
  ;;* When processing a ?RAND in an application form:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;the old value of CTXT is discarded and a new value is selected if the ?RATOR is a
  ;;lambda sexp.  For example, while processing:
  ;;
  ;;   ((lambda (x) x)
  ;;      (lambda (y) y)) ;annotated: x
  ;;
  ;;upon entering the operand "(lambda (y) y)", CTXT is set to "x".
  ;;
  ;;* When calling MAKE-CLAMBDA  the value of CTXT is consumed: if  CTXT is a symbol,
  ;;it becomes the value of the field NAME of the CLAMBDA struct.
  ;;
  ;;* Upon entering the  body of a CLAMBDA-CASE: if CTXT is a  list of a single item,
  ;;the item is unwrapped  and becomes the current value of CTXT.   This is to handle
  ;;the case in which  the last form of a CLAMBDA-CASE body  returns a CLAMBDA, which
  ;;in turn is bound to a lex gensym.  Example:
  ;;
  ;;   (let ((x ((lambda (y) (lambda () y))
  ;;             1)))
  ;;     x)
  ;;
  ;;here we have an application form:
  ;;
  ;;   ((lambda (y) (lambda () y)) 1)
  ;;
  ;;in which the operator is:
  ;;
  ;;   (lambda (y) (lambda () y))
  ;;
  ;;and its  return value is the  result of "(lambda ()  y)" which ends up  being bound to
  ;;"x"; so we want "(lambda () y)" to be annotated as "x".
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'recordize))

  (case-define E
    ;;Convert the symbolic expression X representing code in the core language into a
    ;;nested hierarchy of struct instances.
    ;;
    ;;When  X is  recordised  code  representing the  right-hand  side  of a  binding
    ;;definition: CTXT is the corresponding lex gensym.
    ;;
    ((X)
     (E X #f))
    ((X ctxt)
     (cond ((pair? X)
	    (%recordize-pair-sexp X ctxt))

	   ((symbol? X)
	    (cond ((lexical X)
		   ;;X  is a  lex gensym  referencing a  binding defined  inside this
		   ;;INPUT-EXPR.  For  now, the recordised reference  to such binding
		   ;;is simply its PRELEX struct; we  will decide in a later compiler
		   ;;pass if this reference must extract a value from a loc gensym or
		   ;;simply reference a memory location on the Scheme stack.
		   => (lambda (prel)
			(set-prelex-source-referenced?! prel #t)
			prel))
		  (else
		   ;;X is  *not* a  lex gensym referencing  a binding  defined inside
		   ;;this INPUT-EXPR.  We  default to interpreting X as  a loc gensym
		   ;;referencing a top level lexical variable defined by:
		   ;;
		   ;;*  A  previously  processed  input  expression;  for  example  a
		   ;;previous  expression  read  at  the REPL  and  expanded  in  the
		   ;;interaction environment.
		   ;;
		   ;;* A library that was imported in the environment.
		   ;;
		   ;;To reference  such binding we  have to generate  recordised code
		   ;;that extracts the  value from the binding's loc  gensym; this is
		   ;;what the primitive TOP-LEVEL-VALUE does.
		   ;;
		   ;;NOTE When the binding was added to an interaction environment by
		   ;;a  previously evaluated  expression: we  expect the  expander to
		   ;;have generated a  single gensym to serve both as  lex gensym and
		   ;;loc  gensym; so,  here, X  is both  the lex  gensym and  the loc
		   ;;gensym.
		   ;;
		   ;;If instead X is an unbound variable: the call to TOP-LEVEL-VALUE
		   ;;will fail at run-time.
		   ;;
		   ;;NOTE  TOP-LEVEL-VALUE  is  both   a  primitive  function  and  a
		   ;;primitive operation.
		   (make-funcall (mk-primref 'top-level-value)
				 (list (make-constant X))))))

	   (else
	    (compile-time-error __module_who__ __who__
	      "invalid core language expression" X)))))

  (define-syntax-rule (%recordize-pair-sexp X ctxt)
    (case (car X)

      ;;Synopsis: (quote ?datum)
      ;;
      ;;Return a struct instance of type CONSTANT.
      ;;
      ((quote)
       (make-constant (cadr X)))

      ;;Synopsis: (typed-expr ?expr ?core-type-name)
      ;;
      ;;Return a struct instance of type TYPED-EXPR.
      ;;
      ((typed-expr)
       (let ((expr      (E (cadr X)))
	     (core-type (let ()
			  (module (name->core-type-tag)
			    (import SCHEME-OBJECTS-ONTOLOGY))
			  (name->core-type-tag (caddr X)))))
	 (make-typed-expr expr core-type)))

      ;;Synopsis: (if ?test ?consequent ?alternate)
      ;;
      ;;Return a struct instance of type CONDITIONAL.
      ;;
      ((if)
       (make-conditional
	   (E (cadr X))
	   (E (caddr X) ctxt)
	 (E (cadddr X) ctxt)))

      ;;Synopsis: (set! ?lhs ?rhs)
      ;;
      ;;If the  left-hand side  references a lexical  binding defined  by INPUT-EXPR:
      ;;return a struct instance of type ASSIGN.  If the left-hand side references an
      ;;imported binding or  a binding defined in a  previously processed expression:
      ;;return a  new struct instance of  type FUNCALL representing a  SET! operation
      ;;for a variable  whose value is stored  in the "value" field of  a loc gensym.
      ;;For more details: see the documentation of the primitive TOP-LEVEL-VALUE.
      ;;
      ((set!)
       (let* ((lhs.sexp (cadr  X)) ;left-hand side
	      (rhs.sexp (caddr X)) ;right-hand side
	      ;;We recordize the right-hand side in the context of LHS.
	      (rhs.reco (E rhs.sexp lhs.sexp)))
	 (cond ((lexical lhs.sexp)
		=> (lambda (prel)
		     (set-prelex-source-assigned?! prel #t)
		     (make-assign prel rhs.reco)))
	       (else
		;;Here we assume LHS.SEXP is a loc gensym.
		(make-funcall (mk-primref '$init-symbol-value!)
			      (list (make-constant lhs.sexp) rhs.reco))))))

      ;;Synopsis: (begin ?body0 ?body ...)
      ;;
      ;;Build and return nested hierarchy of SEQ structures:
      ;;
      ;;   #[seq ?body0 #[seq ?body ...]]
      ;;
      ((begin)
       (let recur ((A (cadr X))
      		   (D (cddr X)))
      	 (if (pair? D)
	     (make-seq (E A) (recur (car D) (cdr D)))
	   (E A ctxt))))

      ;;Synopsis: (let     ((?lhs ?rhs) ...) ?body)
      ;;Synopsis: (letrec  ((?lhs ?rhs) ...) ?body)
      ;;Synopsis: (letrec* ((?lhs ?rhs) ...) ?body)
      ;;
      ;;Each ?LHS is a  lex gensym representing the name of  the binding; this gensym
      ;;is unique for this binding in the whole history of the Universe.
      ;;
      ;;Return, respectively, a struct instance of type: BIND, RECBIND, REC*BIND.
      ;;
      ((let letrec letrec*)
       (let ((bind* (cadr  X))		      ;list of bindings
	     (body  (caddr X)))		      ;list of body forms
	 (let ((lex* ($map/stx car  bind*))  ;list of bindings left-hand sides
	       (rhs* ($map/stx cadr bind*))) ;list of bindings right-hand sides
	   (with-prelex-structs-in-plists (prel* lex*)
	     (let* ((rhs*^ ($map/stx E rhs* lex*))
		    (body^ (E body ctxt)))
	       (case (car X)
		 ((let)
		  (make-bind     prel* rhs*^ body^))
		 ((letrec)
		  (make-recbind  prel* rhs*^ body^))
		 ((letrec*)
		  (make-rec*bind prel* rhs*^ body^))))))))

      ;;Synopsis: (library-letrec* ((?lex ?loc ?rhs) ...) ?body)
      ;;
      ;;A LIBRARY form like:
      ;;
      ;;   (library (the-lib)
      ;;     (export ---)
      ;;     (import ---)
      ;;     (define ?lhs ?rhs)
      ;;     ...
      ;;     ?expr ...)
      ;;
      ;;is converted by the expander into:
      ;;
      ;;   (library-letrec* ((?lex ?loc ?rhs) ...) ?expr ...)
      ;;
      ;;where:
      ;;
      ;;* ?LEX is the lex gensym representing the name of the binding; this gensym is
      ;;  unique for this binding in the whole history of the Universe.
      ;;
      ;;* ?LOC is the loc gensym used to  hold the value of the binding (in the VALUE
      ;;  field of the symbol's memory block); this gensym is unique for this binding
      ;;  in the whole history of the Universe.
      ;;
      ;;* ?RHS is a symbolic expression which evaluates to the binding's value.
      ;;
      ;;Return a struct instance of type REC*BIND.  The difference between a REC*BIND
      ;;representing a  LETREC* and a  REC*BIND representing a LIBRARY-LETREC*  is in
      ;;the PRELEX structs.
      ;;
      ((library-letrec*)
       (let ((bind* (cadr  X))		       ;list of bindings
	     (body  (caddr X)))		       ;list of body forms
	 (let ((lex* ($map/stx car   bind*))  ;list of lex gensyms
	       (loc* ($map/stx cadr  bind*))  ;list of loc gensyms
	       (rhs* ($map/stx caddr bind*))) ;list of bindings right-hand sides
	   (with-prelex-structs-in-plists (prel* lex*)
	     ($for-each/stx set-prelex-global-location! prel* loc*)
	     (let* ((rhs*^ ($map/stx E rhs* lex*))
		    (body^ (E body ctxt)))
	       (make-rec*bind prel* rhs*^ body^))))))

      ;;Synopsis: (case-lambda (?formals ?body) ...)
      ;;
      ;;Return a struct instance of type CLAMBDA.
      ;;
      ((case-lambda)
       (let* ((name     (and (symbol? ctxt) ctxt))
	      (asmlabel (%name->asmlabel name))
	      (cases    (E-clambda-case* asmlabel (cdr X) ctxt)))
	 (let ((cp       #f)
	       (freevar* #f))
	   (make-clambda asmlabel cases cp freevar* name))))

      ;;Synopsis: (annotated-case-lambda ?annotation (?formals ?body))
      ;;
      ;;Return a struct instance of type CLAMBDA.
      ;;
      ((annotated-case-lambda)
       (let* ((name     (cons (and (symbol? ctxt) ctxt)
			      ;;This annotation  is excluded  only when  building the
			      ;;boot image.
			      (and (not (strip-source-info))
				   (let ((annotated-expr (cadr X)))
				     (and (annotation?       annotated-expr)
					  (annotation-source annotated-expr))))))
	      (asmlabel (%name->asmlabel name))
	      (cases    (E-clambda-case* asmlabel (cddr X) ctxt)))
	 (let ((cp       #f)
	       (freevar* #f))
	   (make-clambda asmlabel cases cp freevar* name))))

      ;;Synopsis: (lambda ?formals ?body)
      ;;
      ;;LAMBDA functions are handled as special cases of CASE-LAMBDA functions.
      ;;
      ;;   (lambda ?formals ?body)
      ;;   ===> (case-lambda (?formals ?body))
      ;;
      ((lambda)
       (E `(case-lambda ,(cdr X)) ctxt))

      ;;Synopsis: (foreign-call "?function-name" ?arg ...)
      ;;
      ;;Return a struct instance of type FORCALL.
      ;;
      ((foreign-call)
       (let ((name (quoted-string (cadr X)))
	     (arg* (cddr X)))
	 (make-forcall name ($map/stx E arg*))))

      ;;Synopsis: (primitive ?prim)
      ;;
      ;;Return a struct instance of type PRIMREF.  ?PRIM is a symbol representing the
      ;;public name of the primitive function or primitive operation.
      ;;
      ;;NOTE Every time  the expander recognises an identifier  in reference position
      ;;captured by a lexical  binding (not a syntax) exported by  the boot image: it
      ;;generates this symbolic expression as core language form.  For example:
      ;;
      ;;   (fx+ 1 2)
      ;;
      ;;is expanded into:
      ;;
      ;;   ((primitive fx+) '1 '2)
      ;;
      ;;and recordised to:
      ;;
      ;;   (funcall (primref fx+) (constant 1) (constant 2))
      ;;
      ((primitive)
       (let ((name (cadr X)))
	 (mk-primref name)))

      ;;Synopsis: (annotated-call ?annotation ?fun ?arg ...)
      ;;
      ;;Return a struct instance of type FUNCALL.
      ;;
      ((annotated-call)
       (E-annotated-call X ctxt))

      (else	;if X is a pair here, it is a function call
       ;;Synopsis: (?func ?rand ...)
       ;;
       ;;Return a struct instance of type FUNCALL.
       ;;
       (let ((func  (car X))
	     (rand* (cdr X)))
	 (E-app make-funcall func rand* ctxt)))))

;;; --------------------------------------------------------------------

  (define (%name->asmlabel name)
    (if (generate-descriptive-labels?)
	(gensym (string-append "asmlabel:"
			       (cond ((symbol? name)
				      (symbol->string name))
				     ((and (pair? name)
					   (symbol? (car name)))
				      (symbol->string (car name)))
				     (else
				      "anonymous"))
			       ":clambda"))
      (gensym)))

  (module (quoted-sym)

    (define (quoted-sym? obj)
      ;;Return true if OBJ is a sexp with the format:
      ;;
      ;;   (quote ?symbol)
      ;;
      (and (list? obj)
	   (null? (cddr obj))
	   (eq? 'quote (car obj))
	   (symbol? (cadr obj))))

    (define* (quoted-sym {x quoted-sym?})
      ;;Check that X has the format:
      ;;
      ;;   (quote ?symbol)
      ;;
      ;;and return ?SYMBOL.
      ;;
      (cadr x))

    #| end of module: quoted-sym |# )

  (module (quoted-string)

    (define (quoted-string? obj)
      ;;Check that X has the format:
      ;;
      ;;   (quote ?string)
      ;;
      (and (list? obj)
	   (null? (cddr obj))
	   (eq? 'quote (car obj))
	   (string? (cadr obj))))

    (define* (quoted-string {x quoted-string?})
      ;;Check that X has the format:
      ;;
      ;;  (quote ?string)
      ;;
      ;;and return ?string.
      ;;
      (cadr x))

    #| end of module: quoted-string |# )

;;; --------------------------------------------------------------------

  (module (E-clambda-case*)

    (define (E-clambda-case* asmlabel clause* ctxt)
      ;;Given a symbolic expression representing a lambda:
      ;;
      ;;   (lambda ?formals ?body)
      ;;   (case-lambda (?formals ?body) ...)
      ;;   (annotated-case-lambda ?annotation (?formals ?body))
      ;;
      ;;this function is called with CLAUSE* set to the list of clauses:
      ;;
      ;;   ((?formals ?body) ...)
      ;;
      ;;Return a list holding new struct instances of type CLAMBDA-CASE, one for each
      ;;clause.
      ;;
      ;;ASMLABEL is  the label identifying the  machine code entry point  of the full
      ;;CLAMBDA; it is used to generate descriptive labels for each clause.
      ;;
      (let ((ctxt (and (pair? ctxt) (car ctxt))))
	($map/stx
	    (lambda (clause)
	      ;;We expect clause to have the format:
	      ;;
	      ;;   (?formals ?body)
	      ;;
	      (let ((fml* (car  clause))	 ;the formals
		    (body (cadr clause)))	 ;the body sequence
		(let ((lex* (%properize-clambda-formals fml*)))
		  (with-prelex-structs-in-plists (prel* lex*)
		    (let* ((body^ (E body ctxt))
			   ;;PROPER? is: true  if FML* is a proper list;  false if it
			   ;;is an improper list, including a standalone lex gensym.
			   (info  (let* ((proper?       (list? fml*))
					 (asmlabel-case (%asmlabel->asmlabel-case asmlabel proper? fml*)))
				    (make-case-info asmlabel-case prel* proper?))))
		      (make-clambda-case info body^))))))
	  clause*)))

    (define (%asmlabel->asmlabel-case asmlabel proper? fml*)
      (if (generate-descriptive-labels?)
	  (gensym (string-append (symbol->string asmlabel)
				 ":case-"
				 (if proper?
				     (number->string (length fml*))
				   "*")))
	(gensym)))

    (define (%properize-clambda-formals fml*)
      ;;Convert the  formals FML*  of a  CASE-LAMBDA clause into  a proper  list.  We
      ;;expect FML* to be a valid CASE-LAMBDA formals specification, one among:
      ;;
      ;;   (?arg-symbol ...)
      ;;   (?arg-symbol ... . ?rest-symbol)
      ;;   ?args-symbol
      ;;
      ;;where the symbols are lex gensyms; here we do *not* validate the items in the
      ;;list as symbols.  If FML* is:
      ;;
      ;;* null or a proper list: return a new list holding the same values:
      ;;
      ;;     (%properize-clambda-formals '())			=> ()
      ;;     (%properize-clambda-formals '(arg1 arg2 arg3))	=> (arg1 arg2 arg3)
      ;;
      ;;* an improper list: return a new proper list holding the same values:
      ;;
      ;;     (%properize-clambda-formals '(arg1 arg2 . rest-arg))
      ;;     => (arg1 arg2 rest-arg)
      ;;
      ;;* not a list: return a list wrapping it:
      ;;
      ;;     (%properize-clambda-formals args) => (args)
      ;;
      (cond ((pair? fml*)
	     (cons (car fml*)
		   (%properize-clambda-formals (cdr fml*))))
	    ((null? fml*)
	     '())
	    (else
	     (list fml*))))

    #| end of module: E-clambda-case* |# )

;;; --------------------------------------------------------------------

  (module (E-annotated-call)

    (define (E-annotated-call X ctxt)
      ;;This  function is  a wrapper  for "E-app",  with the  purpose of  building an
      ;;appropriate  MK-CALL  argument for  it.   We  expect  X  to be  the  symbolic
      ;;expression:
      ;;
      ;;   (annotated-call ?annotation ?fun ?arg ...)
      ;;
      ;;where ?ANNOTATION is either a struct  instance of type ANNOTATION, defined by
      ;;the reader, or a syntax object constructed by the expander.
      ;;
      ;;NOTE At present, this  function is the only place in  the compiler that makes
      ;;use of the parameter GENERATE-DEBUG-CALLS.  (Marco Maggi; Oct 11, 2012)
      ;;
      (let ((anno (cadr  X))  ;annotation
	    (func (caddr X))  ;expression evaluating to the function
	    (args (cdddr X))) ;arguments
	(let ((mk-call (if (generate-debug-calls)
			   (%make-funcall-maker anno)
			 make-funcall)))
	  (E-app mk-call func args ctxt))))

    (module (%make-funcall-maker)

      (define (%make-funcall-maker anno)
	(let ((src/expr (make-constant (if (annotation? anno)
					   (cons (annotation-source   anno)
						 (annotation-stripped anno))
					 (cons #f (syntax->datum anno))))))
	  (lambda (op rands)
	    (if (%core-primitive-reference? op)
		;;This is an annotated call to a core primitive function or primitive
		;;operation: ignore debugging mode, handle it as a normal call.
		(make-funcall op rands)
	      ;;This is an annotated call to a user-defined function: honor debugging
	      ;;mode and generate a DEBUG-CALL.
	      (make-funcall (mk-primref 'debug-call)
			    (cons* src/expr op rands))))))

      (define (%core-primitive-reference? op)
	;;Evaluate to true if OP references  a lexical core primitive exported by the
	;;boot image.
	;;
	;;The SYSTEM-VALUE call  below will fail with an assertion  violation if NAME
	;;is not a symbol associated to a lexical core primitive exported by the boot
	;;image.  See the documentation of SYSTEM-VALUE for more details.
	;;
	;;NOTE When compiling  a library: SYSTEM-VALUE will return  with no exception
	;;if OP is a core primitive exported by the boot image.  When compiling a new
	;;boot image:  SYSTEM-VALUE will  return with  no exception if  OP is  a core
	;;primitive exported by the *old* boot image; so SYSTEM-VALUE must be the one
	;;exported by the old boot image.
	;;
	(struct-case op
	  ((primref name)
	   (guard (C ((assertion-violation? C)
		      #t))
	     (system-value name)
	     #f))
	  (else #f)))

      #| end of module: %MAKE-FUNCALL-MAKER |# )

    #| end of module: E-annotated-call |# )

;;; --------------------------------------------------------------------

  (module (E-app)

    (define (E-app mk-call rator rand* ctxt)
      ;;Process a form representing a function call; return a struct instance of type
      ;;FUNCALL.   The final  purpose of  this function  is to  apply MK-CALL  to the
      ;;recordised version of RATOR and RAND*.
      ;;
      ;;MK-CALL is either MAKE-FUNCALL or a wrapper for it.  RATOR is a core language
      ;;expression evaluating to the  operator of the call.  RAND* is  a list of core
      ;;language expressions evaluating to the call operands.  When the function call
      ;;form is:
      ;;
      ;;   (?func ?arg ...)
      ;;
      ;;the argument RATOR is ?FUNC and the argument RAND* is (?ARG ...).
      ;;
      ;;We handle specially the cases in which RATOR is one of the sexps:
      ;;
      ;;   (primitive make-parameter)
      ;;
      ;;by generating  a core language  expression to  be integrated in  the original
      ;;source.
      ;;
      (define-syntax-rule (%common-function-application)
	(E-function-application mk-call rator rand* ctxt))
      (if (and (pair? rator)
	       (eq? 'primitive (car rator)))
	  (case (cadr rator)
	    ((make-parameter)
	     (E-integration-make-parameter mk-call rand* ctxt))
	    ;;NOTE  With this  function  written as  it is,  everything  is ready  to
	    ;;introduce the integration of further lexical core primitives as in:
	    ;;
	    ;;((map)
	    ;; (E-integration-map mk-call rand* ctxt))
	    ;;
	    ;;But  we  should  consider  this with  care,  because  introducing  such
	    ;;integrations  here  does  no  allow   us  to  take  advantage  of  type
	    ;;informations;  it is  most  likely better  to do  it  in the  expander.
	    ;;(Marco Maggi; Wed Aug 27, 2014)
	    (else
	     (%common-function-application)))
	(%common-function-application)))

    (module (E-function-application)
      ;;NOTE In case RATOR is a lambda sexp with one of the formats:
      ;;
      ;;   (case-lambda                       (?formals ?body) ...)
      ;;   (annotated-case-lambda ?annotation (?formals ?body) ...)
      ;;
      ;;the function application looks like:
      ;;
      ;;   ((lambda (x) x) 123)
      ;;
      ;;In this case, if one of the RAND* evaluates to a closure as in:
      ;;
      ;;   ((lambda (x) x) (lambda (y) y))
      ;;
      ;;we  want the  operand "(lambda  (y)  y)" to  be annotated  with the  corresponding
      ;;operator's formal name "x".
      ;;
      (define (E-function-application mk-call rator rand* ctxt)
	(let ((op    (E rator (list ctxt)))
	      (rand* (E-rand* rator rand*)))
	  (mk-call op rand*)))

      (define (E-rand* rator rand*)
	(let ((fmls (%get-matching-formals rator rand*)))
	  (if (null? fmls)
	      ($map/stx E rand*)
	    ;;FMLS  is a  proper or  improper list  of lex  gensyms representing  the
	    ;;formals of a lambda sexp case.  Here we want to apply E to the RAND* in
	    ;;the  context  of  the  corresponding  formal.   Example,  the  function
	    ;;application:
	    ;;
	    ;;   ((lambda (lex.a lex.b lex.c) ?body)
	    ;;      ?rand0 ?rand1 ?rand2)
	    ;;
	    ;;has matching FMLS:
	    ;;
	    ;;   (lex.a lex.b lex.c)
	    ;;
	    ;;and will cause the following applications:
	    ;;
	    ;;   (E ?rand0 lex.a)
	    ;;   (E ?rand1 lex.b)
	    ;;   (E ?rand2 lex.c)
	    ;;
	    ;;Example, the function application:
	    ;;
	    ;;   ((lambda (lex.a . lex.rest) ?body)
	    ;;      ?rand0 ?rand1 ?rand2)
	    ;;
	    ;;has matching FMLS:
	    ;;
	    ;;   (lex.a . lex.rest)
	    ;;
	    ;;and will cause the following applications:
	    ;;
	    ;;   (E ?rand0 lex.a)
	    ;;   (E ?rand1)
	    ;;   (E ?rand2)
	    ;;
	    (let recur ((rand* rand*)
			(fmls  fmls))
	      (if (pair? fmls)
		  (cons (let ((ctxt (car fmls)))
			  (E (car rand*) ctxt))
			(recur (cdr rand*) (cdr fmls)))
		($map/stx E rand*))))))

      (module (%get-matching-formals)

	(define (%get-matching-formals rator rand*)
	  ;;RATOR  must be  a  core  language expression  representing  code that  will
	  ;;evaluate to  a function call operator.   RAND* must be a  list of arguments
	  ;;for such function.
	  ;;
	  ;;If RATOR is a lambda sexp, with one of the formats:
	  ;;
	  ;;   (case-lambda (?formals ?body) ...)
	  ;;   (annotated-case-lambda ?annotation (?formals ?body) ...)
	  ;;
	  ;;scan the cases  in RATOR looking for  a set of formals  that matches RAND*:
	  ;;when found, return the list of formals ?FORMALS.
	  ;;
	  ;;When no  matching formals  are found  or the  RATOR is  not a  lambda sexp:
	  ;;return null.
	  ;;
	  (let loop ((case* (%extract-lambda-cases rator)))
	    (cond ((null? case*)
		   ;;The RATOR is not a lambda sexp, or it is but no case matched.
		   '())
		  ((let ((fmls (caar case*)))
		     (%matching? fmls rand*))
		   ;;The RATOR is a lambda sexp and the first case in CASE* matches the
		   ;;RAND*: return the formals.
		   (caar case*))
		  (else
		   ;;The RATOR is  a lambda sexp and  the first case in  CASE* does not
		   ;;match: try the next.
		   (loop (cdr case*))))))

	(define (%matching? fmls rand*)
	  ;;FMLS is a  proper or improper list of lex  gensyms representing the formals
	  ;;of  a lambda  case;  RAND* is  a  proper list  of  recordised code  structs
	  ;;representing funcation application operands.  Return true if FMLS and RAND*
	  ;;match each other, otherwise return false.
	  ;;
	  (cond ((null? fmls)
		 (null? rand*))
		((pair? fmls)
		 (and (pair? rand*)
		      (%matching? (cdr fmls) (cdr rand*))))
		(else #t)))

	(define (%extract-lambda-cases rator)
	  ;;Given the sexp RATOR with one of the formats:
	  ;;
	  ;;   (case-lambda                       (?formals ?body) ...)
	  ;;   (annotated-case-lambda ?annotation (?formals ?body) ...)
	  ;;
	  ;;return the list of cases:
	  ;;
	  ;;   ((?formals ?body) ...)
	  ;;
	  ;;return null if RATOR is not a lambda sexp.
	  ;;
	  (if (pair? rator)
	      (case (car rator)
		((case-lambda)
		 (cdr rator))
		((annotated-case-lambda)
		 (cddr rator))
		(else '()))
	    '()))

	#| end of module: GET-MATCHING-FORMALS |# )

      #| end of module: E-function-application |# )

    (define (E-integration-make-parameter mk-call rand* ctxt)
      ;;If the  number of  operands is  correct for  MAKE-PARAMETER, generate  a core
      ;;language expression to be integrated in place of the function application:
      ;;
      ;;   ((primitive make-parameter) ?rand ...)
      ;;
      ;;otherwise we raise an assertion violation:  at run-time if "strict R6RS" mode
      ;;is enabled, otherwise at compile-time.
      ;;
      ;;NOTE The one below is the original Ikarus implementation; it was applying the
      ;;guard function  every time and also  applying the guard function  to the init
      ;;value (Marco Maggi; Feb 3, 2012).
      ;;
      ;;   ((case-lambda
      ;;     ((,t0)
      ;;      (case-lambda
      ;;       (() ,t0)
      ;;       ((,x) (set! ,t0 (,f ,x))))
      ;;      (,f ,t))))
      ;;
      (case (length rand*)
	((1)	;MAKE-PARAMETER called with one argument.
	 (let ((val-expr	(car rand*))
	       (t		(gensym 't))
	       (x		(gensym 'x))
	       (bool		(gensym 'bool)))
	   (E `((lambda (,t)
		  (case-lambda
		   (() ,t)
		   ((,x) (set! ,t ,x))
		   ((,x ,bool)
		    (set! ,t ,x))))
		,val-expr)
	      ctxt)))
	((2)	;MAKE-PARAMETER called with two arguments.
	 (let ((val-expr	(car rand*))
	       (guard-expr	(cadr rand*))
	       (f		(gensym 'f))
	       (t		(gensym 't))
	       (t0		(gensym 't))
	       (x		(gensym 'x))
	       (bool		(gensym 'bool)))
	   (E `((case-lambda
		 ((,t ,f)
		  (if ((primitive procedure?) ,f)
		      ((case-lambda
			((,t0)
			 (case-lambda
			  (() ,t0)
			  ((,x) (set! ,t0 (,f ,x)))
			  ((,x ,bool)
			   (if ,bool
			       (set! ,t0 (,f ,x))
			     (set! ,t0 ,x))))))
		       ,t)
		    ((primitive procedure-argument-violation) 'make-parameter
		     '"expected procedure as guard function argument" ,f))))
		,val-expr
		,guard-expr)
	      ctxt)))
	(else
	 (if (option.strict-r6rs)
	     (begin
	       (print-compiler-warning-message
		"invalid number of operands to core language function integration: ~a"
		rand*)
	       (mk-call (make-primref 'make-parameter) ($map/stx E rand*)))
	   (assertion-violation 'make-parameter
	     "invalid number of operands to core language function integration"
	     rand*)))))

    #| end of module: E-app |# )

;;; --------------------------------------------------------------------

  (module (with-prelex-structs-in-plists
	   lexical lex*->prelex* %remove-prelex-from-proplist-of-lex)
    ;;This module  takes care of  generating a PRELEX  structure for each  lex gensym
    ;;associated to a lexical binding.
    ;;
    ;;Remember that the  function RECORDIZE is called to process:  full LIBRARY forms
    ;;the expander  has transformed  into LIBRARY-LETREC*  core language  forms; full
    ;;R6RS programs  the expander has  transformed into LIBRARY-LETREC  core language
    ;;forms; standalone expressions from invocations of R6RS's EVAL, for example read
    ;;by the REPL.
    ;;
    ;;This is how bindings are handled:
    ;;
    ;;*  When RECORDIZE  enters  a LAMBDA,  ANNOTATED-CASE-LAMBDA, CASE-LAMBDA,  LET,
    ;;  LETREC, LETREC*, LIBRARY-LETREC* core language form: for each defined binding
    ;;  a PRELEX struct is built and stored in the property list of the binding's lex
    ;;  gensym.
    ;;
    ;;* While  RECORDIZE processes the body  of the binding core  language form: each
    ;;  lex gensym  associated to a local  binding contains a PRELEX  in its property
    ;;  list.
    ;;
    ;;* When RECORDIZE  exits the binding core language form:  all the PRELEX structs
    ;;  are removed from the lex gensyms property lists.
    ;;
    ;;So:
    ;;
    ;;* While  processing a LIBRARY-LETREC* form:  all the lex gensyms  associated to
    ;;  top level  bindings defined inside the  form have a PRELEX  in their property
    ;;  list.
    ;;
    ;;* While  processing a standalone expression:  the lex gensyms associated  to an
    ;;  internally defined binding  do have a PRELEX in their  property list; the lex
    ;;  gensyms associated to a previously defined  binding do *not* have a PRELEX in
    ;;  their property list.
    ;;
    ;;For example, let's say we are  evaluating expressions at the REPL; new bindings
    ;;created  by  DEFINE  are  added  to the  interaction  environment  returned  by
    ;;INTERACTION-ENVIRONMENT.  So if we do:
    ;;
    ;;   vicare> (define a 1)
    ;;
    ;;the expander converts this  DEFINE form into a SET! form and  adds a binding to
    ;;the interaction environment;  while recordizing this expression  the lex gensym
    ;;*does*  have a  PRELEX  in its  property  list; lexical  bindings  added to  an
    ;;interaction environment have a single gensym to  be used as both lex gensym and
    ;;loc gensym.  If later we do:
    ;;
    ;;   vicare> a
    ;;
    ;;the expander finds the binding in the interaction environment and converts this
    ;;variable  reference  into  a  standalone lex  gensym;  while  recordizing  this
    ;;expression the lex gensym *does not* have a PRELEX in its property list.
    ;;
    (import (vicare system $symbols))

    (define-syntax (with-prelex-structs-in-plists stx)
      (syntax-case stx ()
	((_ (?prel* ?lex*) ?body0 ?body ...)
	 (and (identifier? #'?prel*)
	      (identifier? #'?lex*))
	 #'(let ((?prel* (lex*->prelex* ?lex*)))
	     (begin0
	       (begin ?body0 ?body ...)
	       (%remove-prelex-from-proplist-of-lex ?lex*))))
	))

    ;;FIXME Do we  need a new cookie  at each call to the  RECORDIZE function?  Maybe
    ;;not,   because  we   always  call   LEX*->PRELEX*  and   then  clean   up  with
    ;;%REMOVE-PRELEX-FROM-PROPLIST-OF-LEX.  (Marco Maggi; Oct 10, 2012)
    (define-constant *COOKIE*
      (gensym "prelex-for-lex"))

    (module (lexical)

      (define-syntax-rule (lexical ?X)
	;;If the  lex gensym ?X  has been defined  in the expression  currently being
	;;recordised: return the associated PRELEX struct.   If the lex gensym ?X has
	;;been defined in a previously processed expression: return false.
	;;
	($getprop ?X *COOKIE*))

      (define ($getprop x k)
	($assq+cdr k ($symbol-plist x)))

      (define-syntax ($assq+cdr stx)
	;;The expansion of this macro is equivalent to:
	;;
	;;   (cond ((assq key ell)
	;;          => cdr)
	;;         (else #f))
	;;
	(syntax-case stx ()
	  ((_ ?key ?ell)
	   #'(let loop ((key ?key)
			(ell ?ell))
	       (and (pair? ell)
		    (if (eq? key (caar ell))
			(cdar ell)
		      (loop key (cdr ell))))))
	  ))

      #| end of module: LEXICAL |# )

    (module (lex*->prelex*)

      (define (lex*->prelex* lex*)
	;;Process the formals and left-hand sides  of the core language forms LAMBDA,
	;;CASE-LAMBDA, ANNOTATED-CASE-LAMBDA, LET,  LETREC, LETREC*, LIBRARY-LETREC*.
	;;Expect LEX*  to be a list  of lex gensyms;  for each LEX generate  a PRELEX
	;;structure and store it in the property list of the LEX.  Return the list of
	;;PRELEX structures.
	;;
	;;The property list keyword is the gensym bound to *COOKIE*.
	;;
	($map/stx (lambda (lex)
		    (receive-and-return (prel)
			(make-prelex lex)
		      ($putprop lex *COOKIE* prel)))
	  lex*))

      (define-syntax ($putprop stx)
	;;The expansion of this syntax is equivalent to:
	;;
	;;   (putprop symbol key value)
	;;
	(syntax-case stx ()
	  ((_ ?symbol ?key ?value)
	   #'(let ((symbol  ?symbol)
		   (key     ?key)
		   (value   ?value))
	       (let loop ((plist ($symbol-plist symbol)))
		 (if (pair? plist)
		     (if (eq? key (caar plist))
			 (set-cdr! (car plist) value)
		       (loop (cdr plist)))
		   ($set-symbol-plist! symbol (cons (cons key value) plist))))))
	  ))

      #| end of module: LEX*->PRELEX* |# )

    (module (%remove-prelex-from-proplist-of-lex)

      (define (%remove-prelex-from-proplist-of-lex lex*)
	;;Process the formals and left-hand sides  of the core language forms LAMBDA,
	;;CASE-LAMBDA, ANNOTATED-CASE-LAMBDA, LET,  LETREC, LETREC*, LIBRARY-LETREC*.
	;;Expect  LEX*  to be  a  list  of  lex  gensyms previsously  processed  with
	;;LEX*->PRELEX*; for each  LEX remove the PRELEX structure  from its property
	;;list.  Return unspecified values.
	;;
	;;The property list keyword is the gensym bound to *COOKIE*.
	;;
	($for-each/stx (lambda (lex)
			 ($remprop lex *COOKIE*))
	  lex*))

      (define-syntax-rule ($remprop ?symbol ?key)
	(let* ((symbol ?symbol)
	       (key    ?key)
	       (plist  ($symbol-plist symbol)))
	  (when (pair? plist)
	    (let ((a (car plist)))
	      (if (eq? (car a) key)
		  ($set-symbol-plist! symbol (cdr plist))
		(let loop ((q     plist)
			   (plist (cdr plist)))
		  (when (pair? plist)
		    (let ((a (car plist)))
		      (if (eq? (car a) key)
			  (set-cdr! q (cdr plist))
			(loop plist (cdr plist)))))))))))

      #| end of module: %REMOVE-PRELEX-FROM-PROPLIST-OF-LEX |# )

    #| end of module |# )

  (E input-expr))


(module (optimize-direct-calls)
  ;;This module inspects application forms:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;and attempts to integrate the operator ?RATOR when possible.
  ;;
  ;;By definition, a "direct closure application" like:
  ;;
  ;;   ((lambda (x) x) 123)
  ;;
  ;;can be integrated to:
  ;;
  ;;   (let ((x 123)) x)
  ;;
  ;;and  so  it can  be  converted  to low  level  operations  that more  efficiently
  ;;implement the binding; this module  attempts to perform such integration.  Notice
  ;;that in the case:
  ;;
  ;;   ((case-lambda
  ;;     ((x) x)
  ;;     ((x y) y))
  ;;    123)
  ;;
  ;;the integration yields:
  ;;
  ;;   (let ((x 123)) x)
  ;;
  ;;and the clause with two arguments is just discarded and never compiled.
  ;;
  ;;There are  other integration  possibilities when the  operator of  an application
  ;;form is a complex expression:
  ;;
  ;;  ((let ((?lhs ?rhs) ...) ?body) ?rand ...)
  ;;  ===> (let ((?lhs ?rhs) ...)
  ;;         (?body ?rand ...))
  ;;
  ;;  ((letrec ((?lhs ?rhs) ...) ?body) ?rand ...)
  ;;  ===> (letrec ((?lhs ?rhs) ...)
  ;;         (?body ?rand ...))
  ;;
  ;;  ((letrec ((?lhs ?rhs) ...) ?body) ?rand ...)
  ;;  ===> (letrec* ((?lhs ?rhs) ...)
  ;;         (?body ?rand ...))
  ;;
  ;;Accept as input a nested hierarchy of the following structures:
  ;;
  ;;   constant		prelex		primref
  ;;   bind		recbind		rec*bind
  ;;   conditional	seq		clambda
  ;;   funcall		forcall		assign
  ;;   typed-expr
  ;;
  ;;Example: COND syntaxes
  ;;----------------------
  ;;
  ;;COND syntaxes are expanded as follows:
  ;;
  ;;   (cond ((this X)
  ;;          => (lambda (Y)
  ;;               (that Y)))
  ;;         (else
  ;;          (those)))
  ;;
  ;;becomes:
  ;;
  ;;   (let ((t (this X)))
  ;;     (if t
  ;;         ((lambda (Y) (that Y)) t)
  ;;       (those)))
  ;;
  ;;which contains a direct call, which will be optimised to:
  ;;
  ;;   (let ((t (this X)))
  ;;     (if t
  ;;         (let ((Y t)) (that Y))
  ;;       (those)))
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'optimize-direct-calls))

  (define* (optimize-direct-calls x)
    ;;Perform code optimisation traversing the whole  hierarchy in X, which must be a
    ;;struct instance representing recordized code in the core language, and building
    ;;a new hierarchy of optimised, recordized code; return the new hierarchy.
    ;;
    ;;The only  recordized code  that may actually  need inlining  transformation are
    ;;FUNCALL instances.
    ;;
    (define-syntax E ;make the code more readable
      (identifier-syntax optimize-direct-calls))
    (struct-case x
      ((constant)
       x)

      ((typed-expr expr core-type)
       (make-typed-expr (E expr) core-type))

      ((prelex)
       #;(assert (prelex-source-referenced? x))
       x)

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (make-bind lhs* ($map/stx E rhs*) (E body)))

      ((recbind lhs* rhs* body)
       (make-recbind lhs* ($map/stx E rhs*) (E body)))

      ((rec*bind lhs* rhs* body)
       (make-rec*bind lhs* ($map/stx E rhs*) (E body)))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda label clause* cp freevar* name)
       (make-clambda label
		     ;;Apply E to the body of each clause.
		     ($map/stx (lambda (clause)
				 (struct-case clause
				   ((clambda-case info body)
				    (make-clambda-case info (E body)))))
		       clause*)
		     cp freevar* name))

      ((funcall rator rand*)
       (%attempt-integration make-funcall (E rator) ($map/stx E rand*)))

      ((forcall rator rand*)
       (make-forcall rator ($map/stx E rand*)))

      ((assign lhs rhs)
       #;(assert (prelex-source-assigned? lhs))
       (make-assign lhs (E rhs)))

      (else
       (compile-time-error __module_who__ __who__
	 "invalid expression" (unparse-recordized-code x)))))

  (module (%attempt-integration)

    (define (%attempt-integration mk rator rand*)
      ;;Attempt to integrate the operator of an application form.
      ;;
      ;;MK  is MAKE-FUNCALL  or a  wrapper for  it.  RATOR  is the  already processed
      ;;operator of  the application form.   RAND* is  the list of  already processed
      ;;operands of the application form.
      ;;
      (struct-case rator
	((clambda label.unused clause*)
	 (%attempt-integration/clambda clause* rand* (mk rator rand*)))

	((primref op)
	 (case op
	   ((call-with-values)
	    (%attempt-integration/call-with-values mk rator rand*))
	   ((debug-call)
	    (%attempt-integration/debug-call mk rator rand*))
	   (else
	    ;;Other primitive operations need no special handling.
	    (mk rator rand*))))

	((bind lhs* rhs* body)
	 ;;  ((bind ((?lhs ?rhs) ...) ?body) ?rand ...)
	 ;;  ===> (bind ((?lhs ?rhs) ...) (?body ?rand ...))
	 (if (null? lhs*)
	     (%attempt-integration mk body rand*)
	   (make-bind lhs* rhs* (%attempt-integration/binding-form-body mk body rand*))))

	((recbind lhs* rhs* body)
	 ;;  ((recbind ((?lhs ?rhs) ...) ?body) ?rand ...)
	 ;;  ===> (recbind ((?lhs ?rhs) ...) (?body ?rand ...))
	 (if (null? lhs*)
	     (%attempt-integration mk body rand*)
	   (make-recbind lhs* rhs* (%attempt-integration/binding-form-body mk body rand*))))

	((rec*bind lhs* rhs* body)
	 ;;  ((rec*bind ((?lhs ?rhs) ...) ?body) ?rand ...)
	 ;;  ===> (rec*bind ((?lhs ?rhs) ...) (?body ?rand ...))
	 (if (null? lhs*)
	     (%attempt-integration mk body rand*)
	   (make-rec*bind lhs* rhs* (%attempt-integration/binding-form-body mk body rand*))))

	(else
	 ;;Nothing to be inlined.
	 (mk rator rand*))))

    (define (%attempt-integration/binding-form-body mk body rand*)
      (cond ((clambda? body)
	     (%attempt-integration mk body rand*))
	    ((and (prelex? body)
		  (not (prelex-source-assigned? body)))
	     ;;The body is an UNassigned reference to lexical binding.  Build:
	     ;;
	     ;;   (funcall body rand*)
	     ;;
	     (mk body rand*))
	    (else
	     ;;The body is a generic expression.  Build:
	     ;;
	     ;;   (bind ((tmp body))
	     ;;     (funcall tmp rand*))
	     ;;
	     (let ((t (make-prelex 'tmp)))
	       (set-prelex-source-referenced?! t #t)
	       (make-bind (list t) (list body) (mk t rand*))))))

    (define (%attempt-integration/debug-call mk debug-call-rator rand*)
      ;;Given the original application form in standard language:
      ;;
      ;;   (?func ?arg ...)
      ;;
      ;;if debugging mode is enabled, its recordisation is:
      ;;
      ;;   (funcall (primref debug-call)
      ;;            ?annotation
      ;;            ?rator ?rand ...)
      ;;
      ;;where: ?RATOR  is the recordised  version of  ?FUNC; ?RAND is  the recordised
      ;;version of ?ARG; ?ANNOTATION is a debugging annotation:
      ;;
      ;;   (constant (?annotation-source . (?func ?arg ...)))
      ;;
      ;;in which ?ANNOTATION-SOURCE has one of the formats:
      ;;
      ;;   #f
      ;;   (?port-identifier . ?first-character-offset)
      ;;
      ;;The introducion  of DEBUG-CALL is  performed no matter what  expression ?FUNC
      ;;is.
      ;;
      ;;In this function call:  the argument MK is MAKE-FUNCALL or  a wrapper for it;
      ;;DEBUG-CALL-RATOR is the operator of the debugging application form:
      ;;
      ;;   (primref debug-call)
      ;;
      ;;RAND* is the list of already  processed operands of the application form: the
      ;;first operand is the annotation; the  second operand is the original operator
      ;;expression; the other operands are the arguments for the original operator.
      ;;
      ;;As example of integration, the standard language form:
      ;;
      ;;   ((lambda (x) x) 1)
      ;;
      ;;is expanded into the core language form:
      ;;
      ;;   (annotated-call ?annotation-struct
      ;;                   (annotated-case-lambda #'(lambda (x) x) ((x x)))
      ;;                   (quote 1))
      ;;
      ;;which is recordised as:
      ;;
      ;;   (funcall (primref debug-call)
      ;;            (constant (?annotation-source . ((lambda (x) x) '1)))
      ;;            (lambda (x_0) x_0)
      ;;            (constant 1))
      ;;
      ;;and integrated here as:
      ;;
      ;;   (bind ((x_0 (constant 1)))
      ;;     x_0)
      ;;
      ;;where we can see  there is no more a function application.   In this case the
      ;;argument MK is never used.
      ;;
      ;;Another example, the standard language form:
      ;;
      ;;   ((let ((f (lambda (y) y)))
      ;;      f)
      ;;    '1)
      ;;
      ;;is expanded and recordised into:
      ;;
      ;;   (funcall (primref debug-call)
      ;;            (constant (?annotation-source . ((let ((f (lambda (x) x))) f) 1)))
      ;;            (bind ((f_0 (lambda (x_0) x_0))) f_0)
      ;;            (constant 1))
      ;;
      ;;and integrated as:
      ;;
      ;;   (bind ((f_0 (lambda (y_0) y_0)))
      ;;     (funcall (primref debug-call)
      ;;              (constant (?annotation-source . ((let ((f (lambda (y) y))) f) 1)))
      ;;              f_0 (constant 1)))
      ;;
      ;;where we can  understand how the MK  wrapper we generate here is  used in the
      ;;internal function call.
      ;;
      (let ((annotation (car rand*))
	    (orig-rator (cadr rand*))
	    (orig-rand* (cddr rand*)))
	(%attempt-integration (lambda (op^ rand*^)
				(mk debug-call-rator (cons* annotation op^ rand*^)))
			      orig-rator
			      orig-rand*)))

    #| end of module: %attempt-integration |# )

;;; --------------------------------------------------------------------

  (module (%attempt-integration/call-with-values)

    (define (%attempt-integration/call-with-values mk rator rand*)
      ;;MK  is MAKE-FUNCALL  or a  wrapper for  it.  RATOR  is the  already processed
      ;;operator   of   the   application   form,   representing   a   reference   to
      ;;CALL-WITH-VALUES.  RAND*  is the  list of already  processed operands  of the
      ;;application form.
      ;;
      (if (null? (cddr rand*))
	  ;;Here we know that the source code is:
	  ;;
	  ;;   (call-with-values ?producer ?consumer)
	  ;;
	  ;;with a correct number of arguments.
	  (let ((producer (%attempt-integration mk (car rand*) '()))
		(consumer (cadr rand*)))
	    (cond ((%single-value-consumer? consumer)
		   ;;The consumer expects a single argument, so we can transform:
		   ;;
		   ;;   (call-with-values
		   ;;         (lambda () ?body1)
		   ;;     (lambda (x) ?body2))
		   ;;
		   ;;into:
		   ;;
		   ;;   (bind ((x_0 (bind ()
		   ;;                 ?body1)))
		   ;;     ?body2)
		   ;;
		   (%attempt-integration mk consumer (list producer)))
		  ;;NOTE Are there other special  cases of producer and consumer that
		  ;;allow  the removal  of  the CALL-WITH-VALUES  call?  Most  likely
		  ;;there  are, but  none  are  implemented right  now.   If some  is
		  ;;implemented it has to be placed  here.  (Marco Maggi; Wed Aug 27,
		  ;;2014)
		  (else
		   ;;Just perform the call to CALL-WITH-VALUES.
		   (mk rator rand*))))
	;;Wrong number of arguments to CALL-WITH-VALUES!!!
	(mk rator rand*)))

    (define (%single-value-consumer? consumer)
      ;;Return true if CONSUMER is a struct instance of type CLAMBDA, having a single
      ;;clause which accepts a single argument; else return false.
      ;;
      ;;In other words, return true if CONSUMER represents a lambda sexp like:
      ;;
      ;;   (lambda (a) ?body)
      ;;   (case-lambda ((a) ?body))
      ;;   (annotated-case-lambda ?annotation (?formals ?body))
      ;;
      (struct-case consumer
	((clambda label.unused clause*)
	 (and (%list-of-one-item? clause*)
	      (struct-case (car clause*)
		((clambda-case info)
		 (struct-case info
		   ((case-info label.unused args proper?)
		    (and proper? (%list-of-one-item? args))))))))
	(else #f)))

    #| end of module: %ATTEMPT-INTEGRATION/CALL-WITH-VALUES |# )

;;; --------------------------------------------------------------------

  (module (%attempt-integration/clambda)

    (define (%attempt-integration/clambda clause* rand* default)
      ;;Iterate the CLAMBDA clauses in CLAUSE*  searching for one whose formals match
      ;;the RAND* operands; if found  generate appropriate local bindings that inline
      ;;the  closure application.   If successful  return a  struct instance  of type
      ;;BIND, else return DEFAULT.
      ;;
      (cond ((null? clause*)
	     default)
	    ((%attempt-integration/clambda-clause (car clause*) rand*))
	    (else
	     (%attempt-integration/clambda (cdr clause*) rand* default))))

    (define (%attempt-integration/clambda-clause clause rand*)
      ;;Try to convert the CLAMBDA clause in  CLAUSE into a set of local bindings for
      ;;the operands in  RAND*; if successful return a struct  instance of type BIND,
      ;;else return #f.
      ;;
      (struct-case clause
	((clambda-case info body)
	 (struct-case info
	   ((case-info label fml* proper?)
	    (if proper?
		;;The  formals of  the  CLAMBDA  clause is  a  proper  list, make  an
		;;appropriate local binding; convert:
		;;
		;;   ((case-lambda ((a b c) ?body)) 1 2 3)
		;;
		;;into:
		;;
		;;   (let ((a 1) (b 2) (c 3)) ?body)
		;;
		(and (fx=? (length fml*)
			   (length rand*))
		     (make-bind fml* rand* body))
	      ;;The formals of the CLAMBDA clause  is an improper list (including the
	      ;;case  of  standalone  symbol),  make an  appropriate  local  binding;
	      ;;convert:
	      ;;
	      ;;   ((case-lambda (args ?body)) 1 2 3)
	      ;;
	      ;;into:
	      ;;
	      ;;   (let ((args (list 1 2 3))) ?body)
	      ;;
	      ;;and convert:
	      ;;
	      ;;   ((case-lambda ((a b . args) ?body)) 1 2 3 4)
	      ;;
	      ;;into:
	      ;;
	      ;;   (let ((a 1) (b 2) (args (list 3 4))) ?body)
	      ;;
	      (and (fx<=? (length fml*)
			  (length rand*))
		   (make-bind fml* (%properize-operands fml* rand*) body))))))))

    (define* (%properize-operands lhs* rhs*)
      ;;LHS* must be a list of PRELEX  structures representing the binding names of a
      ;;CLAMBDA clause, for the cases of formals  being a symbol or an improper list;
      ;;RHS* must be a list of struct instances representing the values to be bound.
      ;;
      ;;Build and return a new list out of RHS* that matches the list in LHS*.
      ;;
      ;;If LHS* holds a single item: it means the CLAMBDA application is like:
      ;;
      ;;   ((case-lambda (args ?body0 ?body ...)) 1 2 3)
      ;;
      ;;so this function is called with:
      ;;
      ;;   LHS* = (#[prelex args])
      ;;   RHS* = (#[constant 1] #[constant 2] #[constant 3])
      ;;
      ;;and we need to return the list:
      ;;
      ;;   (#[funcall cons
      ;;              (#[constant 1]
      ;;               #[funcall cons
      ;;                         (#[constant 2]
      ;;                          #[funcall cons
      ;;                                    (#[constant 3]
      ;;                                     #[constant ()])])])])
      ;;
      ;;If LHS*  holds multiple items: it  means that the CASE-LAMBDA  application is
      ;;like:
      ;;
      ;;   ((case-lambda ((a b . args) ?body0 ?body ...)) 1 2 3 4)
      ;;
      ;;so this function is called with:
      ;;
      ;;   LHS* = (#[prelex a] #[prelex b] #[prelex args])
      ;;   RHS* = (#[constant 1] #[constant 2]
      ;;           #[constant 3] #[constant 4])
      ;;
      ;;and we need to return the list:
      ;;
      ;;   (#[constant 1] #[constant 2]
      ;;    #[funcall cons
      ;;              (#[constant 3]
      ;;               #[funcall cons
      ;;                         (#[constant 4]
      ;;                          #[constant ()])])])
      ;;
      (cond ((null? lhs*)
	     (compile-time-error __module_who__ __who__ "improper improper"))
	    ((null? (cdr lhs*))
	     (list (%make-conses rhs*)))
	    (else
	     (cons (car rhs*)
		   (%properize-operands (cdr lhs*)
					(cdr rhs*))))))

    (define (%make-conses ls)
      (if (pair? ls)
	  (make-funcall (mk-primref 'cons)
			(list (car ls) (%make-conses (cdr ls))))
	(make-constant '())))

    #| end of module: %ATTEMPT-INTEGRATION/CLAMBDA |# )

  #| end of module: OPTIMIZE-DIRECT-CALLS |# )


;;;; let's include some external code

(include "ikarus.compiler.pass-letrec-optimizer.scm" #t)
(include "ikarus.compiler.pass-source-optimizer.scm" #t)


(module (rewrite-references-and-assignments)
  ;;We distinguish between bindings that  are only referenced (unassigned, read-only,
  ;;constant) and  bindings that  are also assigned  (read-write, mutated).   We also
  ;;distinguish between  lexical top  level bindings originally  defined by  the core
  ;;language form  LIBRARY-LETREC* and lexical  local bindings originally  defined by
  ;;the   core   language   forms   LET,  LETREC,   LETREC*,   LAMBDA,   CASE-LAMBDA,
  ;;ANNOTATED-CASE-LAMBDA.
  ;;
  ;;Remembering that the actual value of a top level binding is stored in the "value"
  ;;field of a loc gensym, this function performs the following transformations:
  ;;
  ;;* References to top level bindings are transformed into:
  ;;
  ;;     (funcall (primref $symbol-value) (constant ?loc))
  ;;
  ;;  which extracts the value from slot "value" of the loc gensym ?LOC.
  ;;
  ;;* Common assignments to top level bindings are transformed into:
  ;;
  ;;     (funcall (primref $set-symbol-value!) (constant ?loc) ?rhs)
  ;;
  ;;  which stores a new value in the slot @code{value} of the log gensym ?LOC.
  ;;
  ;;*  Single  assignments  to  top  level  bindings  which  also  serve  as  binding
  ;;  initialisations are transformed into:
  ;;
  ;;     (funcall (primref $init-symbol-value!) (constant ?loc) ?rhs)
  ;;
  ;;  which stores a new value in the slot  "value" of ?LOC and, only if the value is
  ;;  recognised at run-time  as being closure object, also stores  value in the slot
  ;;  "proc".
  ;;
  ;;* Definitions of assigned local bindings are transformed as follows:
  ;;
  ;;     (bind ((?prel ?init)) ?body)
  ;;     ===> (bind ((?tmp-prel ?init))
  ;;            (bind ((?prel (funcall (primref vector) ?tmp-prel)))
  ;;              ?body))
  ;;
  ;;  NOTE Assigned local bindings whose RHS  expression is a CLAMBDA struct are also
  ;;  transformed this way.  After this compiler  pass: there are no more BIND struct
  ;;  whose RHS is a CLAMBDA struct.
  ;;
  ;;* References  to assigned local  bindings are transformed from  standalone PRELEX
  ;;  structs to:
  ;;
  ;;     (funcall (primref $vector-ref) ?prel (constant 0))
  ;;
  ;;* Assignments to assigned local bindings are transformed as follows:
  ;;
  ;;     (assign ?prel ?rhs)
  ;;     ===> (funcall (primref $vector-set!) ?prel (constant 0) ?rhs)
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		prelex		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		assign
  ;;   forcall		funcall		typed-expr
  ;;
  ;;After  this compiler  pass: there  are  no more  ASSIGN structs  in the  returned
  ;;recordised code.
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'rewrite-references-and-assignments))

  (define* (rewrite-references-and-assignments x)
    ;;Perform code transformation traversing the whole  hierarchy in X, which must be
    ;;a  struct instance  representing  recordized  code in  the  core language,  and
    ;;building  a new  hierarchy  of  transformed, recordized  code;  return the  new
    ;;hierarchy.
    ;;
    (define-syntax E ;make the code more readable
      (identifier-syntax rewrite-references-and-assignments))
    (struct-case x
      ((constant)
       x)

      ((typed-expr expr core-type)
       (make-typed-expr (E expr) core-type))

      ((prelex)
       (if (prelex-source-assigned? x)
	   ;;X is a reference to a lexical read-write binding.
	   (cond ((prelex-global-location x)
		  ;;Reference to a  lexical top level binding; LOC is  the loc gensym
		  ;;used to hold the value at run-time.
		  => (lambda (loc)
		       (%top-level-binding-reference loc)))
		 (else
		  ;;Reference to a lexical local binding.
		  (%assigned-local-binding-reference x)))
	 ;;X is a reference to a lexical read-only binding.
	 (cond ((prelex-global-location x)
		=> (lambda (loc)
		     ;;Reference  to a  lexical top  level  binding; LOC  is the  loc
		     ;;gensym used to hold the value at run-time.
		     (%top-level-binding-reference loc)))
	       (else
		;;Reference to a lexical local binding.
		x))))

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (receive (outer-lhs* assigned-lhs* vector-prel*)
	   (%process-assigned-lhs* lhs*)
         (make-bind outer-lhs* ($map/stx E rhs*)
		    (%bind-assigned assigned-lhs* vector-prel* (E body)))))

      ((fix lhs* rhs* body)
       (make-fix lhs* ($map/stx E rhs*) (E body)))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda label clause* cp freevar* name)
       (let ((clause*^ ($map/stx (lambda (clause)
				   ;;Process the formals of every clause to introduce
				   ;;transformations  for  assigned formal  bindings.
				   ;;Also apply E to each body.
				   (struct-case clause
				     ((clambda-case info body)
				      (struct-case info
					((case-info label fml* proper)
					 (receive (fml* assigned-lhs* vector-prel*)
					     (%process-assigned-lhs* fml*)
					   (make-clambda-case
					    (make-case-info label fml* proper)
					    (%bind-assigned assigned-lhs* vector-prel* (E body)))))))))
			 clause*)))
	 (make-clambda label clause*^ cp freevar* name)))

      ((forcall op rand*)
       (make-forcall op ($map/stx E rand*)))

      ((funcall rator rand*)
       (make-funcall (E rator) ($map/stx E rand*)))

      ((assign lhs rhs)
       (cond ((prelex-source-assigned? lhs)
	      => (lambda (where)
		   (cond ((symbol? where)
			  ;;Initialisation  single-assignment  of lexical  top  level
			  ;;binding; WHERE is the loc gensym used to hold the value.
			  (%top-level-binding-init where (E rhs)))
			 ((prelex-global-location lhs)
			  ;;Common assignment  of lexical  top level binding;  LOC is
			  ;;the loc gensym used to hold the value.
			  => (lambda (loc)
			       (%top-level-binding-assignment loc (E rhs))))
			 (else
			  (%assigned-local-binding-assignment lhs (E rhs))))))
	     (else
	      (compiler-internal-error __module_who__ __who__
		"assigned PRELEX has non-assigned state" lhs x))))

      (else
       (compile-time-error __module_who__ __who__
	 "invalid recordised expression" (unparse-recordized-code x)))))

;;; --------------------------------------------------------------------

  (define-syntax-rule (%top-level-binding-init ?loc ?init)
    ;;Single initialisation assignment of recursive  lexical top level binding.  This
    ;;binding is defined with:
    ;;
    ;;   (bind ((?prel (constant #!void)))
    ;;     (assign ?prel ?init) ;<-- this assignment
    ;;     ?body)
    ;;
    ;;to allow  the initialisation  expression ?INIT  to access  the machine  word in
    ;;which the  value is  stored; this  binding has no  other assignments,  and this
    ;;assignment is the operation that initialises it.
    (make-funcall (mk-primref '$init-symbol-value!) (list (make-constant ?loc) ?init)))

  (define-syntax-rule (%top-level-binding-assignment ?loc ?rhs)
    ;;Assignment of lexical top level binding.
    (make-funcall (mk-primref '$set-symbol-value!)  (list (make-constant ?loc) ?rhs)))

  (define-syntax-rule (%top-level-binding-reference ?loc)
    ;;Reference to lexical top level binding.
    (make-funcall (mk-primref '$symbol-value)       (list (make-constant ?loc))))

;;; --------------------------------------------------------------------

  (define-syntax-rule (%assigned-local-binding-reference ?prel)
    ;;Reference to lexical local mutable binding.
    (make-funcall (mk-primref '$vector-ref)  (list ?prel (make-constant 0))))

  (define-syntax-rule (%assigned-local-binding-assignment ?prel ?rhs)
    ;;Assignment of lexical local binding stored on the Scheme stack.
    (make-funcall (mk-primref '$vector-set!) (list ?prel (make-constant 0) ?rhs)))

;;; --------------------------------------------------------------------

  (define (%process-assigned-lhs* lhs*)
    ;;Recursive  function.   LHS* is  a  list  of  struct  instances of  type  PRELEX
    ;;representing bindings.  Return 3 values:
    ;;
    ;;1. The  list of PRELEX  structs to which the  original RHS expressions  must be
    ;;bound.
    ;;
    ;;2. A list of PRELEX structs to which the vector expressions must be bound.
    ;;
    ;;3. A list of PRELEX structs used in the vector creation RHS expressions.
    ;;
    ;;In a trasformation from:
    ;;
    ;;   (let ((X 1)
    ;;         (Y 2))
    ;;     (set! X 3)
    ;;     (list X Y))
    ;;
    ;;to:
    ;;
    ;;   (let ((T 1)
    ;;         (Y 2))
    ;;     (let ((X (vector T)))
    ;;       ($vector-set! X 0 456)
    ;;       (list ($vector-ref X 0) Y)))
    ;;
    ;;the argument LHS* is a list of PRELEX structs: "(X Y)", the return values are:
    ;;
    ;;1. A list holding the PRELEX structs  "(T Y)" for the outer binding definitions
    ;;evaluating the original RHS expressions.
    ;;
    ;;2.  A  list holding  the PRELEX  for X  for the  inner, true,  assigned binding
    ;;definition.
    ;;
    ;;3. A list holding the PRELEX for T for the RHS vector creation expression.
    ;;
    (if (pair? lhs*)
	(let ((prel (car lhs*)))
	  (receive (tmp* assigned-lhs* vector-prel*)
	      (%process-assigned-lhs* (cdr lhs*))
	    (if (and (prelex-source-assigned? prel)
		     (not (prelex-global-location prel)))
		;;PREL is an assigned lexical local binding.  We process it.
		(let ((tmp (make-prelex-for-tmp-binding prel)))
		  (values (cons tmp  tmp*)
			  (cons prel assigned-lhs*)
			  (cons tmp  vector-prel*)))
	      ;;PREL is  an unassigned  lexical local  binding or  a lexical  top level
	      ;;binding.  We skip it.
	      (values (cons prel tmp*) assigned-lhs* vector-prel*))))
      (values '() '() '())))

  (define (%bind-assigned assigned-lhs* vector-prel* body)
    ;;ASSIGNED-LHS* must be a list of PRELEX structs representing the true read-write
    ;;bindings.
    ;;
    ;;VECTOR-PREL*  must be  a  list  of PRELEX  structs  representing references  to
    ;;temporary bindings holding the binding's values.
    ;;
    ;;BODY must be a struct instance representing  code to be evaluated in the region
    ;;of the bindings.
    ;;
    ;;In a trasformation from:
    ;;
    ;;   (let ((X 123))
    ;;     (set! X 456)
    ;;     x)
    ;;
    ;;to:
    ;;
    ;;   (let ((T 123))
    ;;     (let ((X (vector T)))
    ;;       ($vector-set! X 0 456)
    ;;       ($vector-ref X 0)))
    ;;
    ;;this function generates the recordised code representing:
    ;;
    ;;   (let ((X (vector T)))
    ;;     ?body)
    ;;
    ;;in  this case  ASSIGNED-LHS* is  the list  "(X)" and  VECTOR-PREL* is  the list
    ;;"(T)".
    ;;
    (if (null? assigned-lhs*)
	body
      (make-bind assigned-lhs*
		 ($map/stx (lambda (rhs)
			     (make-funcall (mk-primref 'vector) (list rhs)))
		   vector-prel*)
		 body)))

  #| end of module: rewrite-references-and-assignments |# )


;;;; some other external code

(include "ikarus.compiler.pass-core-type-inference.scm" #t)
(include "ikarus.compiler.pass-introduce-unsafe-primrefs.scm" #t)


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
    (compile-time-gensym "core-primitive-operation/integration-handler"))

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


(module (unparse-recordized-code
	 unparse-recordized-code/sexp
	 unparse-recordized-code/pretty
	 unparse-recordised-code
	 unparse-recordised-code/sexp
	 unparse-recordised-code/pretty)

  (define (unparse-recordised-code x)
    ;;Unparse  the  struct instance  X  (representing  recordized  code in  the  core
    ;;language  already processed  by the  compiler) into  a human  readable symbolic
    ;;expression to be used when raising errors.
    ;;
    ;;Being that this function is used only  when signaling errors: it makes no sense
    ;;to use unsafe operations: let's keep it safe!!!
    ;;
    (import SCHEME-OBJECTS-ONTOLOGY)
    (define E unparse-recordised-code)
    (define E-nfv (make-E-nfv E))
    (struct-case x
      ((constant)
       (E-constant 'constant x E))

      ((known expr type)
       `(known ,(E expr) ,(core-type-tag-description type)))

      ((code-loc x)
       `(code-loc ,x))

      ((var x)
       (string->symbol (format ":~a" x)))

      ((prelex name)
       (string->symbol (format ":~a" name)))

      ((primref x)
       x)

      ((conditional test conseq altern)
       (E-conditional 'conditional test conseq altern E))

      ((primopcall op arg*)
       `(,op . ,(map E arg*)))

      ((asmcall op arg*)
       `(asmcall ,op . ,(map E arg*)))

      ((bind lhs* rhs* body)
       `(let ,(map (lambda (lhs rhs)
		     (list (E lhs) (E rhs)))
		lhs* rhs*)
	  ,(E body)))

      ((recbind lhs* rhs* body)
       `(letrec ,(map (lambda (lhs rhs)
			(list (E lhs) (E rhs)))
		   lhs* rhs*)
	  ,(E body)))

      ((rec*bind lhs* rhs* body)
       `(letrec* ,(map (lambda (lhs rhs)
			 (list (E lhs) (E rhs)))
		    lhs* rhs*)
	  ,(E body)))

      ((fix lhs* rhs* body)
       `(fix ,(map (lambda (lhs rhs)
		     (list (E lhs) (E rhs)))
		lhs* rhs*)
	  ,(E body)))

      ((seq e0 e1)
       (letrec ((recur (lambda (x ac)
			 (struct-case x
			   ((seq e0 e1)
			    (recur e0 (recur e1 ac)))
			   (else
			    (cons (E x) ac))))))
	 (cons 'seq (recur e0 (recur e1 '())))))

      ((clambda-case info body)
       `(,(if (case-info-proper info)
	      (map E (case-info-args info))
	    ;;The loop  below is like MAP  but for improper  lists: it maps E  over the
	    ;;improper list X.
	    (let ((X (case-info-args info)))
	      (let recur ((A (car X))
			  (D (cdr X)))
		(if (pair? D)
		    (cons (E A) (recur (car D) (cdr D)))
		  (E A)))))
	 ,(E body)))

      ((clambda label cls* cp freevar*)
       ;;FIXME Should we print more fields?  (Marco Maggi; Oct 11, 2012)
       `(clambda (label: ,(%pretty-symbol label))
		 (cp:    ,(E cp))
		 (free:  ,(and freevar* (map E freevar*)))
		 ,@(map E cls*)))

      ((closure-maker code freevar*)
       `(closure (freevars: ,(map E freevar*))
		 ,(E code)))

      ((codes list body)
       `(codes ,(map E list)
	       ,(E body)))

      ((funcall rator rand*)
       `(funcall ,(E rator) . ,(map E rand*)))

      ((jmpcall label rator rand*)
       `(jmpcall ,(%pretty-symbol label) ,(E rator) . ,(map E rand*)))

      ((forcall rator rand*)
       `(foreign-call ,rator . ,(map E rand*)))

      ((assign lhs rhs)
       `(set! ,(E lhs) ,(E rhs)))

      ((foreign-label x)
       `(foreign-label ,x))

      ((fvar idx)
       (E-fvar idx))

      ((nfv)
       (E-nfv x))

      ((locals vars body)
       (E-locals vars body E))

      ((asm-instr op d s)
       `(asm ,op ,(E d) ,(E s)))

      ((disp s0 s1)
       `(disp ,(E s0) ,(E s1)))

      ((non-tail-call-frame rand* live body)
       (E-non-tail-call-frame rand* live body E))

      ((shortcut body handler)
       `(shortcut
	    ,(E body)
	  ,(E handler)))

      ((non-tail-call)
       (E-non-tail-call x E))

      (else x)))

;;; --------------------------------------------------------------------

  (define (unparse-recordised-code/sexp input-expr)
    ;;Unparse the  struct instance  INPUT-EXPR (representing  recordized code  in the
    ;;core language already processed by the compiler) into a human readable symbolic
    ;;expression to  be used when printing  to some port for  miscellaneous debugging
    ;;purposes.
    ;;
    ;;This  module attempts  to  unparse  recordized code  and  construct a  symbolic
    ;;expression that still represents the struct types in the recordized code.
    ;;
    ;;This function recognises only structures of the following type:
    ;;
    ;;   assign		bind		clambda
    ;;   conditional	constant	fix
    ;;   forcall	foreign-label	funcall
    ;;   known		prelex		primopcall
    ;;   primref	rec*bind	recbind
    ;;   seq		var		asmcall
    ;;
    ;;other values are not processed and are returned as they are.
    ;;
    ;;*NOTE* Being that this function is used  only when debugging: it makes no sense
    ;;to use unsafe operations: LET'S KEEP IT SAFE!!!
    ;;
    (import SCHEME-OBJECTS-ONTOLOGY)
    (define (E x)
      (struct-case x
	((constant)
	 (E-constant 'constant x E))

	((prelex)
	 (E-var x))

	((var)
	 (E-var x))

	((assign lhs rhs)
	 (if (symbol? (prelex-source-assigned? lhs))
	     `(assign-init ,(E lhs) ,(E rhs))
	   `(assign ,(E lhs) ,(E rhs))))

	((primref x)
	 `(primref ,x))

	((known expr type)
	 `(known ,(E expr) ,(core-type-tag-description type)))

	((clambda)
	 (E-clambda x E E-var))

	((closure-maker code freevar*)
	 `(closure-maker ,(E code)
			 ,(let ((freevar* (map E freevar*)))
			    (if (null? freevar*)
				'no-freevars
			      `(freevars: . ,freevar*)))))

	((primopcall op arg*)
	 (cons* 'primopcall op (%map-in-order E arg*)))

	((asmcall op arg*)
	 (cons* 'asmcall    op (%map-in-order E arg*)))

	((funcall rator rand*)
	 (let ((rator (E rator)))
	   (cons* 'funcall rator (%map-in-order E rand*))))

	((forcall rator rand*)
	 `(foreign-call ,rator . ,(%map-in-order E rand*)))

	((jmpcall label op rand*)
	 `(jmpcall ,(%pretty-symbol label) ,(E op) . ,(map E rand*)))

	((seq e0 e1)
	 (E-seq 'seq e0 e1 E))

	((conditional test conseq altern)
	 (E-conditional 'conditional test conseq altern E))

	((bind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order E-var lhs*))
		(rhs* (%map-in-order E     rhs*))
		(body (E body)))
	   (list 'bind (map list lhs* rhs*) body)))

	((fix lhs* rhs* body)
	 (let* ((lhs* (%map-in-order E-var lhs*))
		(rhs* (%map-in-order E     rhs*))
		(body (E body)))
	   (list 'fix (map list lhs* rhs*) body)))

	((recbind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order E-var lhs*))
		(rhs* (%map-in-order E     rhs*))
		(body (E body)))
	   (list 'recbind (map list lhs* rhs*) body)))

	((rec*bind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order E-var lhs*))
		(rhs* (%map-in-order E     rhs*))
		(body (E body)))
	   (list 'rec*bind (map list lhs* rhs*) body)))

	((codes clambda* body)
	 `(codes ,(map (lambda (clam)
			 (let ((sexp (E clam)))
			   (cons* (car sexp)
				  ;;Print the pretty gensym name.
				  `(label: ,(%pretty-symbol (clambda-label clam)))
				  (cdr sexp))))
		    clambda*)
		 ,(E body)))

	((code-loc label)
	 ;;Print the pretty gensym name.
	 `(code-loc ,(%pretty-symbol label)))

	((shortcut body handler)
	 `(shortcut ,(E body) ,(E handler)))

	((locals vars body)
	 (E-locals vars body E))

	((object obj)
	 `(object ,(cond ((symbol? obj)
			  (%pretty-symbol obj))
			 (else
			  (E obj)))))

	;; ------------------------------

	((foreign-label x)
	 `(foreign-label ,x))

	((fvar idx)
	 (E-fvar idx))

	((nfv)
	 (E-nfv x))

	((asm-instr op d s)
	 `(asm-instr ,op ,(E d) ,(E s)))

	((disp s0 s1)
	 `(disp ,(E s0) ,(E s1)))

	((non-tail-call-frame rand* live body)
	 (E-non-tail-call-frame rand* live body E))

	((shortcut body handler)
	 `(shortcut ,(E body) ,(E handler)))

	((non-tail-call target value args mask size)
	 (E-non-tail-call x E))

	(else x)))

      (define E-var (make-E-var E))
      (define E-nfv (make-E-nfv E))

      (E input-expr))

;;; --------------------------------------------------------------------

  (define (unparse-recordised-code/pretty input-expr)
    ;;Unparse the  struct instance  INPUT-EXPR (representing  recordized code  in the
    ;;core language already processed by the compiler) into a human readable symbolic
    ;;expression to  be used when printing  to some port for  miscellaneous debugging
    ;;purposes.
    ;;
    ;;This module attempts  to unparse recordized code and  reconstruct a Scheme-like
    ;;symbolic expression; the returned sexp does *not* exactly represent the input.
    ;;
    ;;This function recognises only structures of the following type:
    ;;
    ;;   assign		bind		clambda
    ;;   conditional	constant	fix
    ;;   forcall	foreign-label	funcall
    ;;   known		prelex		primopcall
    ;;   primref	rec*bind	recbind
    ;;   seq		var		asmcall
    ;;
    ;;other values are not processed and are returned as they are.
    ;;
    ;;*NOTE* Being that this function is used  only when debugging: it makes no sense
    ;;to use unsafe operations: LET'S KEEP IT SAFE!!!
    ;;
    (import SCHEME-OBJECTS-ONTOLOGY)
    (define (E x)
      (struct-case x
	((constant)
	 (E-constant 'quote x E))

	((prelex)
	 (E-var x))

	((var)
	 (E-var x))

	((assign lhs rhs)
	 `(set! ,(E lhs) ,(E rhs)))

	((primref x)
	 x)

	((known expr type)
	 `(known ,(E expr) ,(core-type-tag-description type)))

	((clambda)
	 (E-clambda x E E-var))

	((closure-maker code freevar*)
	 `(closure-maker ,(E code)
			 ,(let ((freevar* (map E freevar*)))
			    (if (null? freevar*)
				'no-freevars
			      `(freevars: . ,freevar*)))))

	((primopcall op arg*)
	 (cons op (%map-in-order E arg*)))

	((asmcall op arg*)
	 (cons* 'asmcall op (%map-in-order E arg*)))

	((funcall rator rand*)
	 (let ((rator (E rator)))
	   (cons rator (%map-in-order E rand*))))

	((forcall rator rand*)
	 `(foreign-call ,rator . ,(%map-in-order E rand*)))

	((jmpcall label op rand*)
	 `(jmpcall ,(%pretty-symbol label) ,(E op) . ,(map E rand*)))

	((foreign-label x)
	 `(foreign-label ,x))

	((seq e0 e1)
	 (E-seq 'begin e0 e1 E))

	((conditional test conseq altern)
	 (E-conditional 'if test conseq altern E))

	((bind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order E-var lhs*))
		(rhs* (%map-in-order E     rhs*))
		(body (E body)))
	   (E-let (map list lhs* rhs*) body)))

	((fix lhs* rhs* body)
	 (let* ((lhs* (%map-in-order E-var lhs*))
		(rhs* (%map-in-order E     rhs*))
		(body (E body)))
	   (list 'fix (map list lhs* rhs*) body)))

	((recbind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order E-var lhs*))
		(rhs* (%map-in-order E     rhs*))
		(body (E body)))
	   (list 'letrec (map list lhs* rhs*) body)))

	((rec*bind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order E-var lhs*))
		(rhs* (%map-in-order E     rhs*))
		(body (E body)))
	   (list 'letrec* (map list lhs* rhs*) body)))

	((codes clambda* body)
	 `(codes ,(map (lambda (clam)
			 (let ((sexp (E clam)))
			   (cons* (car sexp)
				  ;;Print the pretty gensym name.
				  `(label: (%pretty-symbol (clambda-label clam)))
				  (cdr sexp))))
		    clambda*)
		 ,(E body)))

	((code-loc label)
	 ;;Print the pretty gensym name.
	 `(code-loc ,(%pretty-symbol label)))

	((shortcut body handler)
	 `(shortcut ,(E body) ,(E handler)))

	((locals vars body)
	 (E-locals vars body E))

	((object obj)
	 `(object ,(cond ((symbol? obj)
			  (%pretty-symbol obj))
			 (else
			  (E obj)))))

	;; ------------------------------

	((foreign-label x)
	 `(foreign-label ,x))

	((fvar idx)
	 (E-fvar idx))

	((nfv)
	 (E-nfv x))

	((asm-instr op d s)
	 `(asm-instr ,op ,(E d) ,(E s)))

	((disp s0 s1)
	 `(disp ,(E s0) ,(E s1)))

	((non-tail-call-frame rand* live body)
	 (E-non-tail-call-frame rand* live body E))

	((shortcut body handler)
	 `(shortcut ,(E body) ,(E handler)))

	((non-tail-call target value args mask size)
	 (E-non-tail-call x E))

	(else x)))

    (define E-var (make-E-var E))
    (define E-nfv (make-E-nfv E))

    (E input-expr))

;;; --------------------------------------------------------------------

  (define (E-let b* body)
    ;;B*  must be  a list  of already  unparsed LET-like  bindings; BODY  must be  an
    ;;already unparsed symbolic expression representing a body.
    ;;
    ;;If B* represents  a single binding: compress  nested LET and LET*  forms into a
    ;;single LET* form.   If B* represents multiple bindings: just  return a LET-like
    ;;form.
    ;;
    ;;Example:
    ;;
    ;;   (let ((a 1))
    ;;     (let ((b 2))
    ;;       (let ((a 3))
    ;;         (list a b))))
    ;;
    ;;is compressed into:
    ;;
    ;;   (let* ((a 1)
    ;;          (b 2)
    ;;          (a 3))
    ;;     (list a b))
    ;;
    ;;while:
    ;;
    ;;   (let ((a 1)
    ;;         (b 2))
    ;;     (let ((c 3))
    ;;       (list a b c)))
    ;;
    ;;is returned as is.
    ;;
    (cond ((and (%list-of-one-item? b*)
		(pair? body)
		(or (eq? (car body) 'let*)
		    (and (eq? (car body) 'let)
			 (%list-of-one-item? (cadr body)))))
	   (list 'let* (append b* (cadr body)) (caddr body)))
	  (else
	   (list 'let b* body))))

;;; --------------------------------------------------------------------

  (module (E-clambda)

    (define (E-clambda x E E-var)
      (struct-case x
	((clambda label.unused cls*)
	 (let ((cls* (%map-in-order (lambda (clause)
				      (E-clambda-clause clause E E-var))
				    cls*)))
	   (if (%list-of-one-item? cls*)
	       (cons 'lambda (car cls*))
	     (cons 'case-lambda cls*))))))

    (define (E-clambda-clause x E E-var)
      (struct-case x
	((clambda-case info body)
	 (let ((args (E-args (case-info-proper info) (case-info-args info) E E-var)))
	   (list args (E body))))))

    (define (E-args proper? x E E-var)
      (if proper?
	  (%map-in-order E-var x)
	;;The loop below is  like MAP but for improper lists: it  maps E-var over the
	;;improper list X.
	(let recur ((A (car x))
		    (D (cdr x)))
	  (if (pair? D)
	      (let ((A (E-var A)))
		(cons A (recur (car D) (cdr D))))
	    (E-var A)))))

    #| end of module: E-clambda |# )

;;; --------------------------------------------------------------------

  (define (E-constant sym x E)
    (list sym
	  (struct-case x
	    ((constant x.const)
	     (cond ((symbol? x.const)
		    ;;Extract the pretty name; this is useful when X.CONST is a loc gensym.
		    (%pretty-symbol x.const))
		   ((object? x.const)
		    (E x.const))
		   ((closure-maker? x.const)
		    (E x.const))
		   ((code-loc? x.const)
		    (E x.const))
		   ((foreign-label? x.const)
		    (struct-case x.const
		      ((foreign-label name)
		       `(foreign-label ,name))))
		   (else
		    x.const))))))

;;; --------------------------------------------------------------------

  (module (make-E-var)
    ;;Given a struct instance X of type  PRELEX or VAR, identifying the location of a
    ;;binding: return a  symbol representing a unique name for  the binding.  The map
    ;;between structures and symbols is cached in a hash table.
    ;;
    ;;This function acts in such a way that the input:
    ;;
    ;;   (let ((a 1))
    ;;     (let ((a a))
    ;;       a))
    ;;
    ;;is transformed into:
    ;;
    ;;   (let ((a_0 1))
    ;;     (let ((a_1 a_0))
    ;;       a_1))
    ;;
    (define (make-E-var E)
      (define H
	;;Map PRELEX and VAR structures to already built binding name symbols.
	(make-eq-hashtable))
      (define T
	;;Map binding  pretty string names  to number of  times this string  name has
	;;already been used.
	(make-hashtable string-hash string=?))
      (lambda (x)
	(or (hashtable-ref H x #f)
	    (struct-case x
	      ((prelex x.name)
	       (%build-name x x.name T H))
	      ((var x.name)
	       (let ((rep (%build-name x x.name T H)))
		 (cond ((var-loc x)
			=> (lambda (loc)
			     (cons rep (E loc))))
		       (else rep))))
	      ((fvar)
	       (E x))
	      (else x)))))

    (define (%build-name x x.name T H)
      (let* ((name (symbol->string x.name))
	     (N    (hashtable-ref T name 0)))
	(hashtable-set! T name (+ N 1))
	(receive-and-return (sym)
	    (string->symbol (string-append name "_" (number->string N)))
	  (hashtable-set! H x sym))))

    #| end of module: make-E-var |# )

;;; --------------------------------------------------------------------

  (define (make-E-nfv E)
    ;;Given a struct instance  X of type NFV, identifying the  location of a non-tail
    ;;call stack operand: return a symbol representing a unique name for the operand.
    ;;The map between structures and symbols is cached in a hash table.
    ;;
    (define H
      ;;Map NFV structures to already built operand name symbols.
      (make-eq-hashtable))
    (define T
      ;;Map  operand pretty  string names  to number  of times  this string  name has
      ;;already been used.
      (make-hashtable string-hash string=?))
    (lambda (x)
      (or (hashtable-ref H x #f)
	  (struct-case x
	    ((nfv idx loc)
	     (let ((sym (let* ((name (format "nfv.~a" idx))
			       (N    (hashtable-ref T name 0)))
			  (hashtable-set! T name (+ N 1))
			  (receive-and-return (sym)
			      (string->symbol (string-append name "_" (number->string N)))
			    (hashtable-set! H x sym)))))
	       (if loc
		   (cons sym (E loc))
		 sym)))))))

;;; --------------------------------------------------------------------

  (define (E-seq sym e0 e1 E)
    (cons sym
	  ;;Here we flatten nested SEQ instances into a unique output SEQ form.
	  (let recur ((expr  e0)
		      (expr* (list e1)))
	    (struct-case expr
	      ((seq expr.e0 expr.e1)
	       (recur expr.e0 (cons expr.e1 expr*)))
	      (else
	       (let ((expr^ (E expr)))
		 (if (pair? expr*)
		     (cons expr^ (recur (car expr*) (cdr expr*)))
		   (list expr^))))))))


  (define (E-conditional symbol test conseq altern E)
    `(,symbol ,(E test)
	      ,(E conseq)
	      ,(E altern)))

  (define (E-non-tail-call x E)
    (struct-case x
      ((non-tail-call target retval-var all-rand* mask size)
       `(non-tail-call
	 (target: ,(and target
			(cond ((symbol? target)
			       (%pretty-symbol target))
			      (else target))))
	 (retval-var:  ,(and retval-var (E retval-var)))
	 ,(if (and all-rand* (pair? all-rand*))
	      `(all-rand*: . ,(map (lambda (arg)
				     (cond ((symbol? arg)
					    arg)
					   ((nfv? arg)
					    (E arg))
					   ((fvar? arg)
					    (E arg))
					   (else arg)))
				all-rand*))
	    '(all-rand*: #f))
	 (mask:   ,mask)
	 (size:   ,size)))))

  (define (E-non-tail-call-frame rand* live body E)
    `(non-tail-call-frame
      ,(if (pair? rand*)
	   `(rand*: . ,(map E rand*))
	 '(rand*: #f))
      ,(if live
	   `(live: . ,(map E live))
	 '(live: #f))
      ,(E body)))

  (define (E-fvar idx)
    (string->symbol (format "fvar.~a" idx)))

  (define (E-locals vars body E)
    `(locals ,(cond ((null? vars)
		     '(local-vars: . #f))
		    ((list? vars)
		     `(local-vars: . ,(let ((A (car vars)))
					(if (vector? A)
					    (cons (vector-map E A)
						  (map E (cdr vars)))
					  (map E vars)))))
		    (else
		     ;;This includes the case of VARS being #f.
		     `(local-vars: ,vars)))
	     ,(E body)))

;;; --------------------------------------------------------------------

  (define (%pretty-symbol sym)
    (string->symbol (symbol->string sym)))

  (define (%map-in-order f ls)
    ;;This version of  MAP imposes an order to  the application of F to  the items in
    ;;LS.
    ;;
    (if (pair? ls)
	(let ((a (f (car ls))))
	  (cons a (%map-in-order f (cdr ls))))
      '()))

;;; --------------------------------------------------------------------

  (define unparse-recordized-code        unparse-recordised-code)
  (define unparse-recordized-code/sexp   unparse-recordised-code/sexp)
  (define unparse-recordized-code/pretty unparse-recordised-code/pretty)

  #| end of module |# )


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
