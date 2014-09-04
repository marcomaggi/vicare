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

    ;; these go in (vicare system $compiler)
    optimize-level
    (rename
     ;; configuration parameters
     (current-letrec-pass			$current-letrec-pass)
     (check-for-illegal-letrec			$check-for-illegal-letrec)
     (source-optimizer-passes-count		$source-optimizer-passes-count)
     (perform-tag-analysis			$perform-tag-analysis)
     (cp0-effort-limit				$cp0-effort-limit)
     (cp0-size-limit				$cp0-size-limit)
     (strip-source-info				$strip-source-info)
     (generate-debug-calls			$generate-debug-calls)
     (enabled-function-application-integration?	$enabled-function-application-integration?)
     (option.descriptive-labels			$descriptive-labels)

     ;; middle pass inspection
     (assembler-output				$assembler-output)
     (optimizer-output				$optimizer-output)
     (tag-analysis-output			$tag-analysis-output)

     (compile-core-expr->code			$compile-core-expr->code)
     (recordize					$recordize)
     (optimize-direct-calls			$optimize-direct-calls)
     (optimize-letrec				$optimize-letrec)
     (source-optimize				$source-optimize)
     (rewrite-references-and-assignments	$rewrite-references-and-assignments)
     (introduce-tags				$introduce-tags)
     (sanitize-bindings				$sanitize-bindings)
     (optimize-for-direct-jumps			$optimize-for-direct-jumps)
     (insert-global-assignments			$insert-global-assignments)
     (introduce-vars				$introduce-vars)
     (introduce-closure-makers			$introduce-closure-makers)
     (optimize-closures/lift-codes		$optimize-closures/lift-codes)
     (alt-cogen					$alt-cogen)
     (assemble-sources				$assemble-sources)

     (alt-cogen.introduce-primcalls		$introduce-primcalls)
     (alt-cogen.eliminate-fix			$eliminate-fix)
     (alt-cogen.insert-engine-checks		$insert-engine-checks)
     (alt-cogen.insert-stack-overflow-check	$insert-stack-overflow-check)
     (alt-cogen.specify-representation		$specify-representation)
     (alt-cogen.impose-calling-convention/evaluation-order
      $impose-calling-convention/evaluation-order)
     (alt-cogen.assign-frame-sizes		$assign-frame-sizes)
     (alt-cogen.color-by-chaitin		$color-by-chaitin)
     (alt-cogen.flatten-codes			$flatten-codes)

     (unparse-recordized-code			$unparse-recordized-code)
     (unparse-recordized-code/sexp		$unparse-recordized-code/sexp)
     (unparse-recordized-code/pretty		$unparse-recordized-code/pretty)))
  ;;NOTE  This library  is  needed  to build  a  new boot  image.   Let's  try to  do
  ;;everything here  using the system  libraries and not loading  external libraries.
  ;;(Marco Maggi; Fri May 23, 2014)
  (import (except (vicare)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  ;;We need a SYSTEM-VALUE procedure, but the correct one.  See below
		  ;;the appropriately commented import spec.
		  #;system-value

		  return
		  current-primitive-locations
		  eval-core			current-core-eval
		  compile-core-expr-to-port	compile-core-expr

		  assembler-output
		  optimizer-output
		  tag-analysis-output

		  cp0-effort-limit		cp0-size-limit
		  current-letrec-pass		generate-debug-calls
		  optimize-level
		  perform-tag-analysis		strip-source-info
		  fasl-write)
    ;;Here we *truly* want to use the SYSTEM-VALUE provided by the library (vicare).
    ;;
    ;;* During normal execution: this binding is the one from the boot image.
    ;;
    ;;* While building  a new boot image: this  binding is the one from  the old boot
    ;;  image, *not* the one from loading "(ikarus.symbols)" in source form.
    ;;
    ;;We really need it this way for the use we do of such procedure.
    (only (vicare) system-value)
    ;;When building a new  boot image: the hashtables library is  loaded from the old
    ;;boot image.
    (rnrs hashtables)
    (only (vicare system $codes)
	  $code->closure)
    (only (vicare system $structs)
	  $struct-ref $struct/rtd?)
    ;;When building a new boot image: the FASL write library is loaded from source.
    ;;This needs to be loaded here so that it evaluates with the freshly loaded
    ;;"ikarus.config.ss", including the correct value for WORDSIZE.
    (only (ikarus.fasl.write)
	  fasl-write)
    (ikarus.intel-assembler)
    (prefix (only (ikarus.options)
		  strict-r6rs
		  descriptive-labels)
	    option.))


;;;; helper modules

(include "ikarus.wordsize.scm" #t)

(module UNSAFE
  ;;Remember that this file defines the primitive operations.
  ($car $cdr $set-car! $set-cdr!
	$fx= $fx< $fx> $fx<= $fx>=
	$fxadd1 $fxsub1 $fx+ $fx- $fx* $fxdiv
	$fxlogand $fxlogor $fxlognot $fxsra $fxsll
	$fxzero?)

  #;(vicare system $pairs)
  (begin
    (define $car car)
    (define $cdr cdr)
    (define-syntax $set-car!
      (syntax-rules ()
	((_ ?var ?val)
	 (set-car! ?var ?val))))
    (define-syntax $set-cdr!
      (syntax-rules ()
	((_ ?var ?val)
	 (set-cdr! ?var ?val))))
    #| end of begin |# )

  #;(vicare system $fx)
  (begin
    (define $fxzero?	 fxzero?)
    (define $fx=	fx=?)
    (define $fx<	fx<?)
    (define $fx>	fx>?)
    (define $fx<=	fx<=?)
    (define $fx>=	fx>=?)
    (define $fx+	fx+)
    (define $fx-	fx-)
    (define $fx*	fx*)
    (define $fxdiv	fxdiv)
    (define $fxlogand	fxand)
    (define $fxlogor	fxior)
    (define $fxlognot	fxnot)
    (define ($fxadd1 x)
      (fx+ x 1))
    (define ($fxsub1 x)
      (fx- x 1))
    (define ($fxsra x count)
      (import (prefix (vicare system $fx) unsafe.))
      (assert (fixnum? x))
      (assert (fixnum? count))
      (unsafe.$fxsra x count))
    (define ($fxsll x count)
      (import (prefix (vicare system $fx) unsafe.))
      (assert (fixnum? x))
      (assert (fixnum? count))
      (unsafe.$fxsll x count))
    #| end of begin |# )
  #| end of module |# )

(import UNSAFE)


;;;; configuration parameters

(define generate-debug-calls
  ;;Set  to true  when  the option  "--debug"  is used  on the  command  line of  the
  ;;executable "vicare"; else set to #f.
  ;;
  (make-parameter #f))

(define strip-source-info
  (make-parameter #f))

(define optimizer-output
  (make-parameter #f))

(define perform-tag-analysis
  ;;When true the pass INTRODUCE-TAGS is performed, else it is skipped.
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


;;;; helper syntaxes

(define-inline ($caar x)	($car ($car x)))
(define-inline ($cadr x)	($car ($cdr x)))
(define-inline ($cdar x)	($cdr ($car x)))
(define-inline ($cddr x)	($cdr ($cdr x)))

(define-inline ($caaar x)	($car ($car ($car x))))
(define-inline ($caadr x)	($car ($car ($cdr x))))
(define-inline ($cadar x)	($car ($cdr ($car x))))
(define-inline ($cdaar x)	($cdr ($car ($car x))))
(define-inline ($cdadr x)	($cdr ($car ($cdr x))))
(define-inline ($cddar x)	($cdr ($cdr ($car x))))
(define-inline ($cdddr x)	($cdr ($cdr ($cdr x))))
(define-inline ($caddr x)	($car ($cdr ($cdr x))))

(define-inline ($cadddr x)	($car ($cdddr x)))

(define-syntax-rule (%list-of-one-item? ?ell)
  (let ((ell ?ell))
    (and (pair? ell)
	 (null? ($cdr ell)))))

(define-syntax-rule (fxincr! ?var)
  (set! ?var (fxadd1 ?var)))

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
			      (let* ((result.last-pair^ (let ((new-last-pair (cons (?proc ($car ELL0)
											  ($car ELL)
											  ...)
										   '())))
							  (if result.last-pair
							      (begin
								($set-cdr! result.last-pair new-last-pair)
								new-last-pair)
							    new-last-pair)))
				     (result.head^       (or result.head result.last-pair^)))
				(loop result.head^ result.last-pair^ ($cdr ELL0) ($cdr ELL) ...))
			    (or result.head '())))))
	   (loop #f #f ?ell0 ?ell ...)))
     ;;This alternative  implementation: is non-tail recursive,  loops in unspecified
     ;;order, assumes proper list arguments of equal length.
     ;;
     ;; (with-syntax (((T ...) (generate-temporaries #'(?ell ...))))
     ;;   #'(let recur ((t ?ell0) (T ?ell) ...)
     ;; 	   (if (null? t)
     ;; 	       '()
     ;; 	     (cons (?proc ($car t) ($car T) ...)
     ;; 		   (recur ($cdr t) ($cdr T) ...))))))
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
	   (unless (null? t)
	     (?func ($car t) ($car T) ...)
	     (loop  ($cdr t) ($cdr T) ...)))))
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
	       (?combine ($car ell0) ($car ELL) ... (recur knil ($cdr ell0) ($cdr ELL) ...))
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

(define (remq1 x ls)
  ;;Scan the list  LS and remove only the  first instance of object X,  using EQ?  as
  ;;comparison function; return the resulting list which may share its tail with LS.
  ;;
  (cond ((null? ls)
	 '())
	((eq? x ($car ls))
	 ($cdr ls))
	(else
	 (let ((t (remq1 x ($cdr ls))))
	   (cond ((eq? t ($cdr ls))
		  ls)
		 (else
		  (cons ($car ls) t)))))))

(define (union s1 s2)
  ;;Return a  list which  is the  union between  the lists  S1 and  S2, with  all the
  ;;duplicates removed.
  ;;
  (define (add* s1 s2)
    (if (null? s1)
	s2
      (add ($car s1)
	   (add* ($cdr s1) s2))))
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
    (if (null? s2)
	s1
      (remq1 ($car s2)
	     (rem* ($cdr s2) s1))))
  (cond ((null? s1) '())
	((null? s2) s1)
	(else
	 (rem* s2 s1))))


;;;; condition objects

(define-condition-type &compile-time-error &assertion
  make-compile-time-error compile-time-error?)

(define-condition-type &compiler-internal-error &compile-time-error
  make-compiler-internal-error compiler-internal-error?)

(define (compile-time-error who message . irritants)
  (raise
   (condition (make-compile-time-error)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants))))

(define (compiler-internal-error who message . irritants)
  (raise
   (condition (make-compiler-internal-error)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants))))


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
       (refresh-cached-labels!)))))

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
	 (compiler-internal-error __who__
	   "while building boot image: primitive missing from makefile.sps" name))))


(module (compile-core-expr-to-port
	 compile-core-expr
	 compile-core-expr->code
	 core-expr->optimized-code
	 core-expr->assembly-code)

  (define (compile-core-expr-to-port expr port)
    ;;This function is used to write binary code into the boot image.
    ;;
    (fasl-write (compile-core-expr->code expr) port))

  (define (compile-core-expr x)
    ;;This function is used to compile  libraries' source code for serialisation into
    ;;FASL files.
    ;;
    (let ((code (compile-core-expr->code x)))
      ($code->closure code)))

  (define (compile-core-expr->code core-language-sexp)
    ;;This  is *the*  commpiler function.   It  transforms a  core language  symbolic
    ;;expression into a code object.
    ;;
    (let* ((p (recordize core-language-sexp))
	   (p (optimize-direct-calls p))
	   (p (optimize-letrec p))
	   (p (source-optimize p)))
      (when (optimizer-output)
	(pretty-print (unparse-recordized-code/pretty p) (current-error-port)))
      (let* ((p (rewrite-references-and-assignments p))
	     (p (if (perform-tag-analysis)
		    (introduce-tags p)
		  p))
	     (p (sanitize-bindings p))
	     (p (optimize-for-direct-jumps p))
	     (p (insert-global-assignments p))
	     (p (introduce-vars p))
	     (p (introduce-closure-makers p))
	     (p (optimize-closures/lift-codes p))
	     (ls* (alt-cogen p)))
	(when (assembler-output)
	  ;;Print nicely the assembly labels.
	  (parameterize ((gensym-prefix "L")
			 (print-gensym  #f))
	    (for-each (lambda (ls)
			(newline (current-error-port))
			($for-each/stx print-instr ls))
	      ls*)))
	(let ((code* (assemble-sources thunk?-label ls*)))
	  (car code*)))))

  (define (core-expr->optimized-code core-language-sexp)
    ;;This is a utility function used for debugging and inspection purposes; it is to
    ;;be used to inspect the result of optimisation.
    ;;
    (let* ((p (recordize core-language-sexp))
	   (p (optimize-direct-calls p))
	   (p (optimize-letrec p))
	   (p (source-optimize p)))
      (unparse-recordized-code/pretty p)))

  (define (core-expr->assembly-code core-language-sexp)
    ;;This is  a utility  function used  for debugging  and inspection  purposes.  It
    ;;transforms  a symbolic  expression representing  core language  into a  list of
    ;;sublists, each sublist  representing assembly language instructions  for a code
    ;;object.
    ;;
    (let* ((p (recordize core-language-sexp))
	   (p (optimize-direct-calls p))
	   (p (optimize-letrec p))
	   (p (source-optimize p)))
      (let* ((p (rewrite-references-and-assignments p))
	     (p (if (perform-tag-analysis)
		    (introduce-tags p)
		  p))
	     (p (sanitize-bindings p))
	     (p (optimize-for-direct-jumps p))
	     (p (insert-global-assignments p))
	     (p (introduce-vars p))
	     (p (introduce-closure-makers p))
	     (p (optimize-closures/lift-codes p))
	     (ls* (alt-cogen p)))
	#;(gensym-prefix "L")
	ls*)))

  (define (thunk?-label x)
    ;;If X is a struct instance of  type CLOSURE-MAKER with no free variables: return
    ;;the associated label.
    ;;
    (and (closure-maker? x)
	 (if (null? (closure-maker-freevar* x))
	     (code-loc-label (closure-maker-code x))
	   (compiler-internal-error #f "non-thunk escaped" x))))

  (define (print-instr x)
    ;;Print  to the  current  error  port the  symbolic  expression representing  the
    ;;assembly  instruction X.   To  be  used to  log  generated  assembly for  human
    ;;inspection.
    ;;
    (if (and (pair? x)
	     (eq? ($car x) 'seq))
	($for-each/stx print-instr ($cdr x))
      (let ((port (current-error-port)))
	(display "   " port)
	(write x port)
	(newline port))))

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
   (source-reference-count 0)
		;Fixnum,  represents  the  number  of times  a  lexical  variable  is
		;referenced in the source code.
		;
		;See also the field RESIDUAL-REFERENCED?.
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

;;Implement operations on the field SOURCE-REFERENCE-COUNT for the PRELEX structure.
;;
(begin
  (define* (prelex-incr-source-reference-count! {prel prelex?})
    ($set-prelex-source-reference-count! prel (fxadd1 ($prelex-source-reference-count prel))))

  (define* (prelex-decr-source-reference-count! {prel prelex?})
    ($set-prelex-source-reference-count! prel (fxsub1 ($prelex-source-reference-count prel)))))

;;Implement the virtual field SOURCE-REFERENCED? for the PRELEX structure.
;;
(begin
  (define (prelex-source-referenced? prel)
    (fxpositive? (prelex-source-reference-count prel)))

  (define (set-prelex-source-referenced?! prel bool)
    (set-prelex-source-reference-count! prel (if bool 1 0)))

  (define ($prelex-source-referenced? prel)
    (fxpositive? ($prelex-source-reference-count prel)))

  (define ($set-prelex-source-referenced?! prel bool)
    ($set-prelex-source-reference-count! prel (if bool 1 0))))

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
		;False or
    frm-conf
		;False or
    var-conf
		;False or
    reg-move
		;False or
    frm-move
		;False or
    var-move
		;False or
    loc
		;False or
    index
		;False or
    global-location
		;False  or loc  gensym.   When  false: this  VAR  represents a  local
		;lexical binding.  When a loc gensym: this VAR represents a top level
		;lexical binding  and the loc  gensym is the one  to use to  hold its
		;current value in the "value" slot.
    ))

(define (make-unique-var name)
  (make-var name #f #f #f #f #f #f #f #f #f))

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
		;First a CLAMBDA struct representing  a function defined by the input
		;expression; then, after  code lifting, a CODE-LOC struct  to be used
		;to generate the run-time closure object representing the function.
   freevar*
		;Null or a  proper list of struct instances of  type VAR representing
		;the free variables referenced by the generated closure object.
   recursive?
		;Boolean.  True if the body of  the CLAMBDA references itself, and so
		;the function is recursive; false otherwise.
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
;;   (primcall fx+ (constant 1) (constant 2))
;;
;;an even lower compiler pass with transform PRIMCALL into the actuall asssembly code
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
(define-struct primcall
  (op
		;A symbol representing the public name of a primitive operation.
   arg*
		;A  list  of  struct instances  representing  recordized  expressions
		;which, when evaluated,  will return the arguments  of this primitive
		;call.
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
  (label))

(define-struct cp-var
  (idx))

(define-struct frame-var
  (idx))

(define-struct new-frame
  (base-idx size body))

(define-struct save-cp
  (loc))

(define-struct eval-cp
  (check body))

(define-struct return
  (value))

(define-struct call-cp
  (call-convention
   label
   save-cp?
   rp-convention
   base-idx
   arg-count
   live-mask))

(define-struct tailcall-cp
  (convention
   label
   arg-count
   ))

(define-struct interrupt-call
  (test handler))

(define-struct known
  (expr
   type
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
  (vars
   body
   ))

(define-struct nframe
  (vars
   live
   body
   ))

(define-struct nfv
  (conf
   loc
   var-conf
   frm-conf
   nfv-conf
   ))

(define-struct ntcall
  (target
   value
   args
   mask
   size
   ))

;;Represent an assembly instruction.
;;
(define-struct asm-instr
  (op
		;Operand.
   dst
		;Destination machine word.
   src
		;Source machine word.
   ))

(define-struct disp
  (s0
   s1
   ))


(define (recordize input-expr)
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
  (define-fluid-override __who__
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
			(prelex-incr-source-reference-count! prel)
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
	    (compile-time-error __who__ "invalid core language expression" X)))))

  (define-syntax-rule (%recordize-pair-sexp X ctxt)
    (case ($car X)

      ;;Synopsis: (quote ?datum)
      ;;
      ;;Return a struct instance of type CONSTANT.
      ;;
      ((quote)
       (make-constant ($cadr X)))

      ;;Synopsis: (if ?test ?consequent ?alternate)
      ;;
      ;;Return a struct instance of type CONDITIONAL.
      ;;
      ((if)
       (make-conditional
	   (E ($cadr X))
	   (E ($caddr  X) ctxt)
	 (E ($cadddr X) ctxt)))

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
       (let* ((lhs.sexp ($cadr  X)) ;left-hand side
	      (rhs.sexp ($caddr X)) ;right-hand side
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
       (let recur ((A ($cadr X))
      		   (D ($cddr X)))
      	 (if (null? D)
      	     (E A ctxt)
      	   (make-seq (E A) (recur ($car D) ($cdr D))))))

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
       (let ((bind* ($cadr  X))		      ;list of bindings
	     (body  ($caddr X)))	      ;list of body forms
	 (let ((lex* ($map/stx $car  bind*))  ;list of bindings left-hand sides
	       (rhs* ($map/stx $cadr bind*))) ;list of bindings right-hand sides
	   (with-prelex-structs-in-plists (prel* lex*)
	     (let* ((rhs*^ ($map/stx E rhs* lex*))
		    (body^ (E body ctxt)))
	       (case ($car X)
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
       (let ((bind* ($cadr  X))		       ;list of bindings
	     (body  ($caddr X)))	       ;list of body forms
	 (let ((lex* ($map/stx $car   bind*))  ;list of lex gensyms
	       (loc* ($map/stx $cadr  bind*))  ;list of loc gensyms
	       (rhs* ($map/stx $caddr bind*))) ;list of bindings right-hand sides
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
	      (cases    (E-clambda-case* asmlabel ($cdr X) ctxt)))
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
				   (let ((annotated-expr ($cadr X)))
				     (and (annotation?       annotated-expr)
					  (annotation-source annotated-expr))))))
	      (asmlabel (%name->asmlabel name))
	      (cases    (E-clambda-case* asmlabel ($cddr X) ctxt)))
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
       (E `(case-lambda ,($cdr X)) ctxt))

      ;;Synopsis: (foreign-call "?function-name" ?arg ...)
      ;;
      ;;Return a struct instance of type FORCALL.
      ;;
      ((foreign-call)
       (let ((name (quoted-string ($cadr X)))
	     (arg* ($cddr X)))
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
       (let ((name ($cadr X)))
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
       (let ((func  ($car X))
	     (rand* ($cdr X)))
	 (E-app make-funcall func rand* ctxt)))))

;;; --------------------------------------------------------------------

  (define (%name->asmlabel name)
    (if (option.descriptive-labels)
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
	   (null? ($cddr obj))
	   (eq? 'quote ($car obj))
	   (symbol? ($cadr obj))))

    (define* (quoted-sym {x quoted-sym?})
      ;;Check that X has the format:
      ;;
      ;;   (quote ?symbol)
      ;;
      ;;and return ?SYMBOL.
      ;;
      ($cadr x))

    #| end of module: quoted-sym |# )

  (module (quoted-string)

    (define (quoted-string? obj)
      ;;Check that X has the format:
      ;;
      ;;   (quote ?string)
      ;;
      (and (list? obj)
	   (null? ($cddr obj))
	   (eq? 'quote ($car obj))
	   (string? ($cadr obj))))

    (define* (quoted-string {x quoted-string?})
      ;;Check that X has the format:
      ;;
      ;;  (quote ?string)
      ;;
      ;;and return ?string.
      ;;
      ($cadr x))

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
      (let ((ctxt (and (pair? ctxt) ($car ctxt))))
	($map/stx
	    (lambda (clause)
	      ;;We expect clause to have the format:
	      ;;
	      ;;   (?formals ?body)
	      ;;
	      (let ((fml* ($car  clause))	 ;the formals
		    (body ($cadr clause)))	 ;the body sequence
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
      (if (option.descriptive-labels)
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
	     (cons ($car fml*)
		   (%properize-clambda-formals ($cdr fml*))))
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
      (let ((anno ($cadr  X))  ;annotation
	    (func ($caddr X))  ;expression evaluating to the function
	    (args ($cdddr X))) ;arguments
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
	       (eq? 'primitive ($car rator)))
	  (case ($cadr rator)
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
		  (cons (let ((ctxt ($car fmls)))
			  (E ($car rand*) ctxt))
			(recur ($cdr rand*) ($cdr fmls)))
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
		  ((let ((fmls ($caar case*)))
		     (%matching? fmls rand*))
		   ;;The RATOR is a lambda sexp and the first case in CASE* matches the
		   ;;RAND*: return the formals.
		   ($caar case*))
		  (else
		   ;;The RATOR is  a lambda sexp and  the first case in  CASE* does not
		   ;;match: try the next.
		   (loop ($cdr case*))))))

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
		      (%matching? ($cdr fmls) ($cdr rand*))))
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
	      (case ($car rator)
		((case-lambda)
		 ($cdr rator))
		((annotated-case-lambda)
		 ($cddr rator))
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
	     (mk-call (make-primref 'make-parameter) ($map/stx E rand*))
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
		    (if (eq? key ($caar ell))
			($cdar ell)
		      (loop key ($cdr ell))))))
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
		     (if (eq? key ($caar plist))
			 ($set-cdr! ($car plist) value)
		       (loop ($cdr plist)))
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
	  (unless (null? plist)
	    (let ((a ($car plist)))
	      (if (eq? ($car a) key)
		  ($set-symbol-plist! symbol ($cdr plist))
		(let loop ((q     plist)
			   (plist ($cdr plist)))
		  (unless (null? plist)
		    (let ((a ($car plist)))
		      (if (eq? ($car a) key)
			  ($set-cdr! q ($cdr plist))
			(loop plist ($cdr plist)))))))))))

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
  (define-fluid-override __who__
    (identifier-syntax 'optimize-direct-calls))

  (define (optimize-direct-calls x)
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
       (compile-time-error __who__ "invalid expression" (unparse-recordized-code x)))))

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
      (let ((annotation ($car rand*))
	    (orig-rator ($cadr rand*))
	    (orig-rand* ($cddr rand*)))
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
	      (struct-case ($car clause*)
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
	    ((%attempt-integration/clambda-clause ($car clause*) rand*))
	    (else
	     (%attempt-integration/clambda ($cdr clause*) rand* default))))

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
		(and ($fx= (length fml*)
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
	      (and ($fx<= (length fml*)
			  (length rand*))
		   (make-bind fml* (%properize-operands fml* rand*) body))))))))

    (define (%properize-operands lhs* rhs*)
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
	     (compile-time-error __who__ "improper improper"))
	    ((null? ($cdr lhs*))
	     (list (%make-conses rhs*)))
	    (else
	     (cons ($car rhs*)
		   (%properize-operands ($cdr lhs*)
					($cdr rhs*))))))

    (define (%make-conses ls)
      (if (null? ls)
	  (make-constant '())
	(make-funcall (mk-primref 'cons)
		      (list ($car ls) (%make-conses ($cdr ls))))))

    #| end of module: %ATTEMPT-INTEGRATION/CLAMBDA |# )

  #| end of module: OPTIMIZE-DIRECT-CALLS |# )


;;;; let's include some external code

(include "ikarus.compiler.letrec-optimizer.scm" #t)
(include "ikarus.compiler.source-optimizer.scm" #t)


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
  ;;   forcall		funcall
  ;;
  ;;After  this compiler  pass: there  are  no more  ASSIGN structs  in the  returned
  ;;recordised code.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'rewrite-references-and-assignments))

  (define (rewrite-references-and-assignments x)
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
	      (compiler-internal-error __who__
		"assigned PRELEX has non-assigned state" lhs x))))

      (else
       (compile-time-error __who__
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
    (if (null? lhs*)
	(values '() '() '())
      (let ((prel ($car lhs*)))
	(receive (tmp* assigned-lhs* vector-prel*)
	    (%process-assigned-lhs* ($cdr lhs*))
	  (if (and (prelex-source-assigned? prel)
		   (not (prelex-global-location prel)))
	      ;;PREL is an assigned lexical local binding.  We process it.
	      (let ((tmp (make-prelex-for-tmp-binding prel)))
		(values (cons tmp  tmp*)
			(cons prel assigned-lhs*)
			(cons tmp  vector-prel*)))
	    ;;PREL is  an unassigned  lexical local  binding or  a lexical  top level
	    ;;binding.  We skip it.
	    (values (cons prel tmp*) assigned-lhs* vector-prel*))))))

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

(include "ikarus.compiler.tag-annotation-analysis.ss" #t)


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
  ;;After  this  pass is  complete,  all  the BIND  structs  have  a non-CLAMBDA  RHS
  ;;expression.
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		prelex		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		known
  ;;   forcall		funcall
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
  (define-fluid-override __who__
    (identifier-syntax 'sanitize-bindings))

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax sanitize-bindings))

  (define (sanitize-bindings x)
    ;;Perform code transformation traversing the whole  hierarchy in X, which must be
    ;;a  struct instance  representing  recordized  code in  the  core language,  and
    ;;building  a new  hierarchy  of  transformed, recordized  code;  return the  new
    ;;hierarchy.
    ;;
    (struct-case x
      ((constant)
       x)

      ((prelex)
       x)

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (receive (fixable* bindable*)
	   (partition (lambda (x)
			(clambda? ($cdr x)))
	     ($map/stx cons lhs* rhs*))
	 ;;FIXABLE* is  a list  of pairs  (?LHS . ?RHS)  in which  ?RHS is  a CLAMBDA
	 ;;struct.  BINDABLE*  is a  list of pairs  (?LHS .  ?RHS)  in which  ?RHS is
	 ;;*not* a CLAMBDA struct.
	 (%mk-bind ($map/stx $car bindable*)
		   ($map/stx (lambda (bindable)
			       (E ($cdr bindable)))
		     bindable*)
		   (E-fix ($map/stx $car fixable*)
			  ($map/stx $cdr fixable*)
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
       (compile-time-error __who__
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
  (define-fluid-override __who__
    (identifier-syntax 'optimize-for-direct-jumps))

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax optimize-for-direct-jumps))

  (define (optimize-for-direct-jumps x)
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
       (E-bind lhs* rhs* body))

      ((fix lhs* rhs* body)
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
       (compile-time-error __who__
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
	 ;;that "($car rand*)" will evaluate to a closure object.
	 ((and (primref? unwrapped-rator)
	       (eq? ($primref-name unwrapped-rator) '$$apply))
	  ;;JMPCALL does not want KNOWN structs as rator and rands.
	  (make-jmpcall (sl-apply-label)
			(%unwrap-known ($car rand*))
			($map/stx %unwrap-known ($cdr rand*))))

	 ;;If  we are  here: UNWRAPPED-RATOR  is  just some  unknown struct  instance
	 ;;representing recordized code which, when  evaluated, will return a closure
	 ;;object.
	 (else
	  (make-funcall rator rand*)))))

    (define (%optimize-funcall appform clam prelex-rator rand*)
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
	  (recur ($cdr clause*)))
	(if (null? clause*)
	    ;;No matching clause found.
	    (if (option.strict-r6rs)
		;;Just call the closure as always.  A "wrong num args" exception will
		;;be raised at run-time as mandated by R6RS.
		(make-funcall prelex-rator rand*)
	      (compile-time-error __who__
		"wrong number of arguments in closure object application"
		(unparse-recordized-code/pretty appform)))
	  (struct-case ($clambda-case-info ($car clause*))
	    ((case-info label fml* proper?)
	     (if proper?
		 ;;This clause has a fixed number of arguments.
		 (if ($fx= num-of-rand* (length fml*))
		     (make-jmpcall label prelex-rator ($map/stx %unwrap-known rand*))
		   (%recur-to-next-clause))
	       ;;This clause has a variable number of arguments.
	       (if ($fx<= (length ($cdr fml*)) num-of-rand*)
		   (make-jmpcall label prelex-rator (%prepare-rand* ($cdr fml*) rand*))
		 (%recur-to-next-clause))))))))

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
      (if (null? fml*)
	  ;;FIXME Construct list afterwards.  (Abdulaziz Ghuloum)
	  (list (make-funcall (mk-primref 'list) rand*))
	(cons (%unwrap-known ($car rand*))
	      (%prepare-rand* ($cdr fml*) ($cdr rand*)))))

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
  (define-fluid-override __who__
    (identifier-syntax 'insert-global-assignments))

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax insert-global-assignments))

  (define (insert-global-assignments x)
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
       (make-bind lhs* ($map/stx E rhs*)
		  (%process-bind lhs* (E body))))

      ((fix lhs* rhs* body)
       (make-fix lhs* ($map/stx E rhs*)
		 (%process-fix   lhs* (E body))))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda label clause* cp freevar* name)
       ;;Apply E to every body of every CASE-LAMBDA clause.
       (let ((clause*^ ($map/stx (lambda (clause)
				   (struct-case clause
				     ((clambda-case info body)
				      (make-clambda-case info (E body)))))
			 clause*)))
	 (make-clambda label clause*^ cp freevar* name)))

      ((forcall op rand*)
       (make-forcall op ($map/stx E rand*)))

      ((funcall rator rand*)
       (make-funcall (E-known rator) ($map/stx E-known rand*)))

      ((jmpcall label rator rand*)
       ;;JMPCALL's  rator and  rand* are  not,  by construction,  wrapped into  KNOWN
       ;;structs.
       (make-jmpcall label (E rator) ($map/stx E rand*)))

      (else
       (compiler-internal-error __who__
	 "invalid expression" (unparse-recordized-code x)))))

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
	    ((prelex-global-location ($car lhs*))
	     => (lambda (loc)
		  (make-seq (make-funcall SET-PRIMREF (list (make-constant loc) ($car lhs*)))
			    (%insert-assignments ($cdr lhs*) body INIT-PRIMREF))))
	    (else
	     (%process-fix ($cdr lhs*) body))))

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
  (define-fluid-override __who__
    (identifier-syntax 'introduce-vars))

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax introduce-vars))

  (define (introduce-vars x)
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
       ;;Process the LHS* before everything else!
       (let ((lhs* ($map/stx %prelex->var lhs*)))
         (make-bind lhs* ($map/stx E rhs*) (E body))))

      ((fix lhs* rhs* body)
       ;;Process the LHS* before everything else!
       (let ((lhs* ($map/stx %prelex->var lhs*)))
         (make-fix lhs* ($map/stx E rhs*) (E body))))

      ((conditional e0 e1 e2)
       (make-conditional (E e0) (E e1) (E e2)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda label clause* cp freevar* name)
       ;;The purpose of  this form is to  apply %PRELEX->VAR to all the  items in the
       ;;ARGS field of all the CASE-INFO structs.  Also we apply E to each body.
       (let ((clause*^ ($map/stx (lambda (clause)
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
	 (make-clambda label clause*^ cp freevar* name)))

      ((funcall rator rand*)
       (make-funcall (E-known rator) ($map/stx E-known rand*)))

      ((forcall rator rand*)
       (make-forcall rator ($map/stx E rand*)))

      ((jmpcall label rator rand*)
       ;;JMPCALL's  rator and  rand* are  not,  by construction,  wrapped into  KNOWN
       ;;structs.
       (make-jmpcall label (E rator) ($map/stx E rand*)))

      (else
       (compile-time-error __who__
	 "invalid expression" (unparse-recordized-code x)))))

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

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

  #| end of module: introduce-vars |# )


(module (introduce-closure-makers)
  ;;This  module  wraps  each  CLAMBDA  struct in  the  input  recordised  code  into
  ;;CLOSURE-MAKER structures, compiling  a list of free variables  referenced by each
  ;;CLAMBDA.  Each CLOSURE-MAKER struct represents  code that, evaluated at run-time,
  ;;will build and return a closure object.
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		var		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		known
  ;;   forcall		funcall		jmpcall
  ;;
  ;;NOTE This module  makes use of the field  "index" of structs of type  VAR used to
  ;;reference bindings.   The value of such  fields from previous compiler  passes is
  ;;expected  to be  #f.   The purpose  of  the field  in this  compiler  pass is  to
  ;;determine which CLAMBDA structs represent a recursive function.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'introduce-closure-makers))

  (define (introduce-closure-makers X)
    ;;Perform code transformation traversing the whole  hierarchy in X, which must be
    ;;a  struct instance  representing  recordised  code in  the  core language,  and
    ;;building  a new  hierarchy  of  transformed, recordised  code;  return the  new
    ;;hierarchy.
    ;;
    (receive (X^ freevar*)
	(E X)
      (if (null? freevar*)
	  X^
	(compiler-internal-error __who__
	  "free vars encountered in program" (map unparse-recordized-code freevar*)))))

  (define (E X)
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
       ($set-var-index! X #f)
       (values X (list X)))

      ((primref)
       (values X '()))

      ((bind lhs* rhs* body)
       ;;This is  a BIND struct,  so, assuming the  recordised input is  correct: the
       ;;RHS* are non-CLAMBDA structs;  the VARs in LHS* do *not*  appear in the RHS*
       ;;expressions.
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
       ($for-each/stx (lambda (lhs)
			($set-var-index! lhs #t))
	 lhs*)
       (let-values
	   (((rhs*^ freevar*.rhs)  (E-clambda* lhs* rhs*))
	    ((body^ freevar*.body) (E body)))
	 ;;Here RHS*^ is a list of CLOSURE-MAKER structs.
	 ($for-each/stx (lambda (lhs rhs^)
			  ;;LHS is the  VAR struct referencing the  CLAMBDA RHS^.  If
			  ;;the field  "index" of LHS  is true: LHS appears  at least
			  ;;once  in the  body of  RHS^; this  means the  function is
			  ;;recursive.  If  the field  "index" of  LHS is  false: LHS
			  ;;does not appear in the body of RHS^.
			  (when ($var-index lhs)
			    (set-closure-maker-recursive?! rhs^ #t)
			    ($set-var-index! lhs #f)))
	   lhs* rhs*^)
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
       (compile-time-error __who__ "invalid expression" X))))

;;; --------------------------------------------------------------------

  (define (E* X*)
    ;;Map  E over  each element  of X*,  which  must be  a list  of struct  instances
    ;;representing recordized code; return 2 values:  the processed X*, a list of VAR
    ;;structs representing the free variables referenced by X*.
    ;;
    (if (null? X*)
	(values '() '())
      (let-values
	  (((a freevar*.a) (E  ($car X*)))
	   ((d freevar*.d) (E* ($cdr X*))))
	(values (cons a d) (union freevar*.a freevar*.d)))))

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
    (if (null? X*)
	(values '() '())
      (let-values
	  (((a freevar*.a) (E-known  ($car X*)))
	   ((d freevar*.d) (E-known* ($cdr X*))))
	(values (cons a d) (union freevar*.a freevar*.d)))))

;;; --------------------------------------------------------------------

  (module (E-clambda*)

    (define (E-clambda* lhs* rhs*)
      ;;Non-tail recursive  function.  Apply  E-CLAMBDA to every  associated couple
      ;;from LHS* and RHS*.  LHS* is a list of VAR structs; RHS* is a list of CLAMBDA
      ;;structs.  Return 2 values:
      ;;
      ;;1. A list of CLOSURE-MAKER structs which must replace the original RHS*.
      ;;
      ;;2. A  list of VAR structs  representing the free variables  referenced by the
      ;;   CLOSURE-MAKER structs.
      ;;
      (if (null? rhs*)
	  (values '() '())
	(let-values
	    (((a freevar*.a) (E-clambda  ($car lhs*) ($car rhs*)))
	     ((d freevar*.d) (E-clambda* ($cdr lhs*) ($cdr rhs*))))
	  (values (cons a d) (union freevar*.a freevar*.d)))))

    (define (E-clambda lhs rhs)
      ;;LHS is a VAR struct; RHS is a CLAMBDA struct; LHS is the binding of RHS.
      ;;
      ;;Build a struct instance of type CLOSURE-MAKER which must replace the original
      ;;RHS.   Return 2  values:  the CLOSURE-MAKER  struct, a  list  of VAR  structs
      ;;representing the free variables referenced by the CLOSURE-MAKER.
      ;;
      (struct-case rhs
	((clambda label clause* cp.unused freevar*.unused name)
	 #;(assert (not freevar*.unused))
	 (receive (clause*^ freevar*)
	     (E-clambda-case* clause*)
	   (values (let ((clam       (make-clambda label clause*^ lhs freevar* name))
			 (recursive? #f))
		     (make-closure-maker clam freevar* recursive?))
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
      (if (null? clause*)
	  (values '() '())
	(struct-case ($car clause*)
	  ((clambda-case info body)
	   (let-values
	       (((body^    freevar*.body)    (E body))
		((clause*^ freevar*.clause*) (E-clambda-case* ($cdr clause*))))
	     (values (cons (make-clambda-case info body^) clause*^)
		     ;;If a  VAR struct is  a clause's formal  argument: it is  not a
		     ;;free variable; so remove it.
		     (union (difference freevar*.body (case-info-args info))
			    freevar*.clause*)))))))

    #| end of module: do-clambda* |# )

  #| end of module: convert closures |# )


(module (optimize-closures/lift-codes)
  ;;This module
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
  (define-fluid-override __who__
    (identifier-syntax 'optimize-closures/lift-codes))

  (define all-codes
    ;;While  processing  an  input  expression:  a proper  list  of  CLAMBDA  structs
    ;;representing the  set of  all the  functions defined  in the  input expression.
    ;;These CLAMBDA structs  will be compiled to  machine code and used  to build the
    ;;code objects.
    ;;
    (make-parameter #f))

  (define-syntax-rule (%prepend-to-all-codes! ?obj)
    (all-codes (cons ?obj (all-codes))))

  (define (optimize-closures/lift-codes X)
    ;;Perform code transformation traversing the whole  hierarchy in X, which must be
    ;;a  struct instance  representing  recordised  code in  the  core language,  and
    ;;building  a new  hierarchy  of  transformed, recordised  code;  return a  CODES
    ;;struct.
    ;;
    (parametrise ((all-codes '()))
      (let* ((X^ (E X))
	     (C (make-codes (all-codes) X^)))
	C)))

  (module (E)

    (define (E x)
      (struct-case x
	((constant)
	 x)

	((var)
	 (%find-var-substitution! x))

	((primref)
	 x)

	((bind lhs* rhs* body)
	 ;;Clear the field "index" of the VAR structs in LHS* from whatever value the
	 ;;previous compiler passes have left in.
	 ($for-each/stx %var-reset-subst! lhs*)
	 (E-bind lhs* rhs* body))

	((fix lhs* rhs* body)
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
	 (compiler-internal-error __who__
	   "invalid expression" (unparse-recordized-code x)))))

    (define (E-bind lhs* rhs* body)
      ;;Here we know that RHS* is a  list of non-CLAMBDA expressions in which the VAR
      ;;in LHS* do *not* appear.
      ;;
      (let ((rhs*^ ($map/stx E rhs*)))
	;;If  an  RHS^  is  a  VAR  struct  with a  subst:  copy  the  subst  to  the
	;;corresponding LHS.   In other  words: if an  RHS^ is part  of the  graph of
	;;substitutions, we make LHS part of the graph too.
	($for-each/stx %var-copy-subst! lhs* rhs*^)
	(let ((body^ (E body)))
	  ;;Once the body has  been processed: we do not need the  substs in the LHS*
	  ;;anymore, so reset them.
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

  (module (E-fix node?)

    (define (E-fix lhs* rhs* body)
      ;;Here we know that RHS* is a list of CLOSURE-MAKER structs in which the VAR in
      ;;LHS* might appear.
      ;;
      (define cleaned-freevar**
	;;Here  the  VAR  structs  in  LHS*  are   not  yet  part  of  the  graph  of
	;;substitutions.   We clean  up the  lists of  free variables  performing the
	;;substitutions defined for the outer bindings:
	;;
	;;   (bind ((?lhs1 ?rhs1) ...)    ;clean for these bindings
	;;     (fix ((?lhs2 ?rhs2) ...))  ;clean for these bindings
	;;       (fix ((?lhs ?rhs) ...)   ;this is the FIX we are processing
	;;         ?body))
	;;
	($map/stx %filter-and-substitute-binding-freevars lhs* rhs*))
      (define node*
	;;Define the  NODE structs  required to establish  the graph  of dependencies
	;;among "true  closures".  Temporarily set  the NODE as substitution  for the
	;;VAR.
	($map/stx (lambda (lhs rhs)
		    (receive-and-return (N)
			(mk-node lhs ($closure-maker-code rhs) ($closure-maker-recursive? rhs))
		      (%var-set-node! lhs N)))
	  lhs* rhs*))
      ;;If X is free in Y, then whenever X becomes a non-combinator, Y also becomes a
      ;;non-combinator.  Here, we mark these dependencies.
      ($for-each/stx
	  (lambda (my-node freevar*)
	    ($for-each/stx
		(lambda (freevar)
		  (cond ((%var-get-node freevar)
			 ;;One of ours.
			 => (lambda (her-node)
			      (node-push-dep! her-node my-node)))
			(else
			 ;;Not one of ours.
			 (node-push-freevar! my-node freevar))))
	      freevar*))
	node* cleaned-freevar**)
      ;;Next, we  go over the  list of nodes,  and if we find  one that has  any free
      ;;variables, we know it's a non-combinator, so we whack it and add it to all of
      ;;its dependents.
      ;;
      (let ()
	(define (%process-node-successors x)
	  ;;Non-tail recursive function.
	  (unless (or ($node-done? x)
		      (null? ($node-freevar* x)))
	    ($set-node-done?! x #t)
	    ($for-each/stx (lambda (successor)
			     (node-push-freevar! successor ($node-name x))
			     (%process-node-successors successor))
	      ($node-deps x))))
	($for-each/stx %process-node-successors node*))
      (let ((rhs* (%assign-substitutions-to-closures node*)))
	;;Clean the lists of free variables  for the substitutions of the bindings in
	;;this very FIX struct.  Introduce the  CODE-LOC structs and push the CLAMBDA
	;;structs to ALL-CODES.
	(%final-freevars-cleanup-and-code-lifting lhs* rhs*)
	;;Process  the BODY  substituting  VAR structs  from  LHS* when  appropriate.
	;;Build and return the output FIX struct.
	(%mk-fix '() '() lhs* rhs* (E body))))

;;; --------------------------------------------------------------------

    (define-struct node
      ;;Each CLOSURE-MAKER struct appears as RHS in a FIX struct.  Each CLOSURE-MAKER
      ;;struct has a NODE associated to it.
      ;;
      (name
		;The VAR struct appearing as LHS in the binding definition associated
		;to this NODE.
       code
		;The CLAMBDA or CODE-LOC associated to the CLOSURE-MAKER.
       deps
		;Null or a list of NODE structs.
       done?
		;Boolean.  If  true this  NODE struct has  already been  processed to
		;establish its "true closure" dependencies.
       freevar*
		;Null or the  list of VAR structs representing free  variables in the
		;CLAMBDA.   This   list  of  free   variables  is  cleaned   up  from
		;substitutions in outer binding forms  and from the possible self VAR
		;reference (in recursive functions).
       recursive?
		;Boolean.   True  if  the  function  returned  by  the  CLOSURE-MAKER
		;associated to this NODE is recursive.
       ))

    (define (mk-node lhs code recursive?)
      (make-node lhs code '() #f '() recursive?))

    (define (node-push-freevar! node freevar)
      ($set-node-freevar*! node (cons freevar ($node-freevar* node))))

    (define (node-push-dep! node dep)
      ($set-node-deps! node (cons dep ($node-deps node))))

;;; --------------------------------------------------------------------

    (define (%assign-substitutions-to-closures node*)
      ;;The CLOSURE-MAKER  structs that still  have non-removable free  variables are
      ;;"true closures":  they will become  run-time closure objects  actually closed
      ;;upon free  variables.  The  VAR struct  referencing such  true-closure object
      ;;must  be left  alone: it  cannot be  subsituted by  the CLOSURE-MAKER  struct
      ;;itself.
      ;;
      ;;The CLOSURE-MAKER structs with no free  variables or whose free variables are
      ;;all removable are "combinators": they  will beome run-time closure object not
      ;;closed upon free variables.  The  VAR struct referencing such combinator must
      ;;be substituted with the CLOSURE-MAKER struct itself.
      ;;
      ;;Here we scan the list of NODEs  and convert it into the list of CLOSURE-MAKER
      ;;structs that  will be candidate  to inclusion in  the output FIX  struct.  We
      ;;determine here if a binding must be included in the graph of substitutions or
      ;;not.  If a binding  is defined by a FIX: either it has  no substitution or it
      ;;has its CLOSURE-MAKER as substitution.
      ;;
      ;;NOTE Upon entering this loop all the VAR  in the LHS* list have a NODE struct
      ;;as substitution.  Here we either reset  the substitution to false, or replace
      ;;the NODE with a proper CLOSURE-MAKER substitution.
      ;;
      ($map/stx (lambda (node)
		  (let ((freevar*   ($node-freevar*   node))
			(recursive? ($node-recursive? node)))
		    (receive-and-return (clmaker)
			;;Make the new CLOSURE-MAKER using the list of free variables
			;;we have cleaned before.
			(make-closure-maker ($node-code node) freevar* recursive?)
		      ;;Is the binding  of CLOSURE-MAKER to be included  in the graph
		      ;;of substitutions?
		      (let ((lhs ($node-name node)))
			(cond ((null? freevar*)
			       ;;This CLOSURE-MAKER struct has  no free variables: it
			       ;;will return a combinator.  Let's add it to the graph
			       ;;of substitutions.
			       (%var-reset-node/set-subst! lhs clmaker))
			      ((and recursive? (null? ($cdr freevar*)))
			       ;;This CLOSURE-MAKER struct has 1 free variable and it
			       ;;is recursive;  this means the only  free variable is
			       ;;the VAR self referencing the closure itself:
			       ;;
			       ;;  (fix ((f (lambda () f)))
			       ;;    ?body)
			       ;;
			       #;(assert (eq? lhs (car freevar*)))
			       ;;This CLOSURE-MAKER will  return a combinator.  Let's
			       ;;add it to the graph of substitutions.
			       (%var-reset-node/set-subst! lhs closure-maker))
			      (else
			       ;;This CLOSURE-MAKER  struct has true,  not removable,
			       ;;free  variables: it  will return  a "true  closure".
			       ;;Let's leave it alone.
			       (%var-reset-node! lhs)))))))
	node*))

    (define (%final-freevars-cleanup-and-code-lifting lhs* rhs*)
      ;;Perform  the final  cleanup  of the  list  of free  variables  in each  RHS's
      ;;CLOSURE-MAKER  struct;  this  cleanup processes  free  variables  referencing
      ;;bindings defined this very FIX struct:
      ;;
      ;;
      ;;   (fix ((?lhs ?rhs) ...) ;cleanup between these bindings
      ;;     ?body)
      ;;
      ;;Then  replace the  CLAMBDA in  the  CLOSURE-MAKER struct  with an  associated
      ;;CODE-LOC  struct and  enqueue the  CLAMBDA in  the list  of all  the CLAMBDAs
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
      ;;   all-codes := (?clambda ...)
      ;;
      ($for-each/stx
	  (lambda (lhs clmaker)
	    (let ((substituted-freevar* (%filter-and-substitute-binding-freevars lhs clmaker)))
	      ;; (assert (let ((lhs-replacement (%find-var-substitution! lhs)))
	      ;; 		(or (eq? lhs lhs-replacement)
	      ;; 		    (clmaker? lhs-replacement))))
	      ($set-closure-maker-freevar*! clmaker substituted-freevar*)
	      ;;Replace  the CLAMBDA  struct in  the "code"  field with  a CODE-LOC
	      ;;struct.
	      ($set-closure-maker-code! clmaker (%lift-code lhs
							    ($closure-maker-code     clmaker)
							    ($closure-maker-freevar* clmaker)))))
	lhs* rhs*))

    (define (%mk-fix output-lhs* output-rhs* input-lhs* input-rhs* body)
      ;;Tail-recursive function.  Build and return the output FIX struct.  Of all the
      ;;bindings described by  INPUT-LHS* and INPUT-RHS*: only  those whose INPUT-LHS
      ;;has  no substitutions  will  be included  in  the output;  if  a binding  has
      ;;INPUT-LHS with substitution it is filtered out.
      ;;
      (if (null? input-lhs*)
	  (if (null? output-lhs*)
	      body
	    (make-fix output-lhs* output-rhs* body))
	(let ((input-lhs ($car input-lhs*))
	      (input-rhs ($car input-rhs*)))
	  (if (%var-get-subst input-lhs)
	      (begin
		;;This INPUT-LHS has a subst: skip its binding.
		(%var-reset-subst! input-lhs)
		(%mk-fix output-lhs* output-rhs*
			 ($cdr input-lhs*) ($cdr input-rhs*)
			 body))
	    ;;This LHS has no subst: include its binding.
	    (%mk-fix (cons input-lhs output-lhs*) (cons input-rhs output-rhs*)
		     ($cdr input-lhs*) ($cdr input-rhs*)
		     body)))))

    (module (%filter-and-substitute-binding-freevars)

      (define (%filter-and-substitute-binding-freevars self-var clmaker)
	;;Build  and  return a  list  of  VAR  structs  representing the  "true  free
	;;variables" of the CLOSURE-MAKER struct in  CLMAKER.  Taken the list of free
	;;variables from CLMAKER
	;;
	;;* Include the VAR structs having no substitution.
	;;
	;;* Remove the VAR structs that have a CLOSURE-MAKER struct as replacement.
	;;
	;;*  Substitute the  VAR  structs  having a  VAR  substitution  with the  VAR
	;;  substitution itself.
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

      (define (%filter-freevar* freevar*)
	;;Non-tail recursive function.  Given a list of VAR structs representing free
	;;variables in  the body of a  function: build and  return a new list  of VAR
	;;structs representing the actual free vars we care about.
	;;
	(if (pair? freevar*)
	    (let ((A ($car freevar*))
		  (D ($cdr freevar*)))
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
		   (compiler-internal-error __who__
		     "invalid VAR substitution value" what)))))
	  '()))

      #| end of module: %ORIGINAL-FREEVAR*->FILTERED-AND-SUBSTITUTED-FREEVAR *|# )

    (define (%lift-code cp original-clam new-freevar*)
      ;;Given data  from a CLOSURE-MAKER  struct: build a new  CLAMBDA to be  used to
      ;;generated  the actual  code object;  build a  CODE-LOC struct  to be  used to
      ;;generate the actual closure object; return the CODE-LOC; push the new CLAMBDA
      ;;on the parameter ALL-CODES.
      ;;
      ;;CP is the  struct instance of type  VAR to which the closure  is bound.  This
      ;;VAR struct represents the machine word (CPU register or memory location) from
      ;;which the code  can load a reference to  the closure to be stored  in the CPR
      ;;(Closure Pointer Register); such reference allows the body of a run-time code
      ;;object to access the free variables upon which it is closed.
      ;;
      ;;CLAM is the CLAMBDA struct representing the closure's implementation.
      ;;
      ;;NEW-FREEVAR* is the list of VAR  structs representing the free variables that
      ;;will actually be  stored in the run-time closure object.   This list replaces
      ;;the one in the input CLAMBDA struct.
      ;;
      (struct-case original-clam
	((clambda label clause* cp.unused freevar*.dropped name)
	 (let ((clause*^ ($map/stx (lambda (clause)
				     (struct-case clause
				       ((clambda-case info body)
					;;Clear the field "index"  of the VAR structs
					;;in  ARGS from  whatever value  the previous
					;;compiler passes have left in.
					($for-each/stx %var-reset-subst! (case-info-args info))
					(make-clambda-case info (E body)))))
			   clause*)))
	   (%prepend-to-all-codes! (make-clambda label clause*^ cp new-freevar* name))
	   (make-code-loc label)))))

    #| end of module: E-fix |# )

;;; --------------------------------------------------------------------

  (define (%find-var-substitution! x)
    ;;Non-tail  recursive function.   X  is  a VAR  struct.   Traverse  the graph  of
    ;;substitutions starting from X and return:
    ;;
    ;;* X itself if X has no substitution.
    ;;
    ;;* If the substitution  of X is a CLOSURE-MAKER struct:  return such struct.  In
    ;;   this case  the CLOSURE-MAKER  represents code  that, evaluated  at run-time,
    ;;  returns a "combinator" function.
    ;;
    ;;* If  the substitution of  X is another VAR  struct: recurse searching  for its
    ;;  substitution, which will become the substitution of X.
    ;;
    (when (eq? x 'q)
      (compile-time-error __who__
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
	       ;;Down the graph traversal we  have retrieved a substitution: store it
	       ;;so that  further traversals reaching  ORIGINAL-VAR will just  use it
	       ;;rather than go deeper again.
	       (%var-set-subst! x new-subst)))

	    ((closure-maker? x.subst)
	     ;;The  VAR X  has a  CLOSURE-MAKER  as substitution.   The original  VAR
	     ;;struct,  at the  beginning of  the substitutions  graph traversal,  is
	     ;;substituted by this  CLOSURE-MAKER struct which has  no free variables
	     ;;and so will return a "combinator" closure object.
	     #;(assert (null? (closure-maker-freevar* x.subst)))
	     x.subst)

	    (else
	     (compiler-internal-error __who__
	       "invalid VAR substitution" x x.subst)))))

;;; --------------------------------------------------------------------
;;; VAR structs and associated NODE structs

  ;;Whenever bindings defined by  a FIX struct are processed: a  NODE struct for each
  ;;binding is  created to represent  a graph of  dependencies; such graph  allows to
  ;;establish which bindings define a "combinator function" and which bindings define
  ;;a "true closure function".
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
  ;;Bindings defined by BIND have left-hand side VAR structs:
  ;;
  ;;* Are without substitution.
  ;;
  ;;* Have another VAR struct as substitution.
  ;;
  ;;* Have a CLOSURE-MAKER struct as substitution.
  ;;
  ;;Upon entering  this compiler pass, bindings  defined by FIX have  a CLOSURE-MAKER
  ;;struct as right-hand side expression; their left-hand side VAR structs:
  ;;
  ;;* Are without substitution, when their RHS expression returns a "true closure".
  ;;
  ;;* Have  the right-hand side CLOSURE-MAKER  struct as substitution, when  such RHS
  ;;  expression returns a "combinator".
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

  (define (%var-copy-subst! lhs rhs)
    ;;LHS and RHS are, respectively, the left-hand side VAR struct and the right-hand
    ;;side expression of a binding defined  by a BIND struct.  Conditionally copy the
    ;;substitution object from RHS to LHS:
    ;;
    ;;* If  RHS is  a VAR struct  with non-false substitution  object in  its "index"
    ;;  field: store such  object in the "index" field of LHS, so  that they have the
    ;;  same substitution.
    ;;
    ;;* Otherwise RHS is  not a VAR struct or it has no  substitution: store false in
    ;;  the LHS "index" field, so that LHS also has no substitution.
    ;;
    ;;For example, given:
    ;;
    ;;   (bind ((a b))
    ;;     ?body)
    ;;
    ;;we want the VAR structs A and B to have the same property.
    ;;
    #;(assert (var? lhs))
    (set-var-index! lhs (and (var? rhs)
			     (var-index rhs))))

  (define (%var-get-subst x)
    #;(assert (var? x))
    ($var-index x))

  #| end of module: optimize-closures/lift-codes |# )


;;;; definitions for assembly code generation
;;
;;The folowing constants definitions must be kept in sync with the definitions in the
;;C language header files "internals.h" and "vicare.h".
;;

(define wordshift
  (boot.case-word-size
   ((32) 2)
   ((64) 3)))

(define object-alignment		(* 2 wordsize))
(define align-shift			(+ wordshift 1))
(define pagesize			4096)
(define pageshift			12)

(define fx-scale			wordsize)
(define fx-shift			wordshift)
(define fx-mask				(- wordsize 1))
(define fx-tag				0)

;;; --------------------------------------------------------------------
;;; built in Scheme values

(define bool-f				#x2F)	;the constant #f
(define bool-t				#x3F)	;the constant #t
(define bool-mask			#xEF)
(define bool-tag			#x2F)
(define bool-shift			4)

(define nil				#x4F)
(define eof				#x5F)
(define unbound				#x6F)
(define void-object			#x7F)
(define BWP-OBJECT			#x8F)

;;; --------------------------------------------------------------------
;;; characters

;;Characters are 32-bit integers, on any platform.
(define char-size			4)
(define char-shift			8)
(define char-tag			#x0F)
(define char-mask			#xFF)

;;; --------------------------------------------------------------------
;;; pairs

(define pair-mask			7)
(define pair-tag			1)

(define disp-car			0)
(define disp-cdr			wordsize)
(define pair-size			(fx* 2 wordsize))

(define off-car				(fx- disp-car pair-tag))
(define off-cdr				(fx- disp-cdr pair-tag))

;;; --------------------------------------------------------------------
;;; vectors

(define vector-tag			5)
(define vector-mask			7)

(define disp-vector-length		0)
(define disp-vector-data		wordsize)

(define off-vector-length		(fx- disp-vector-length vector-tag))
(define off-vector-data			(fx- disp-vector-data   vector-tag))

;;; --------------------------------------------------------------------
;;; flonums

(define flonum-tag			#x17)
(define flonum-size			16)

(define disp-flonum-tag			0)
(define disp-flonum-data		8)

(define off-flonum-tag			(fx- disp-flonum-tag  vector-tag))
(define off-flonum-data			(fx- disp-flonum-data vector-tag))

;;; --------------------------------------------------------------------
;;; ratnums

(define ratnum-tag			#x27)
(define disp-ratnum-tag			0)
(define disp-ratnum-num			(fx* 1 wordsize))
(define disp-ratnum-den			(fx* 2 wordsize))
(define ratnum-size			(fx* 4 wordsize))

(define off-ratnum-tag			(fx- disp-ratnum-tag vector-tag))
(define off-ratnum-num			(fx- disp-ratnum-num vector-tag))
(define off-ratnum-den			(fx- disp-ratnum-den vector-tag))

;;; --------------------------------------------------------------------
;;; compnums

(define compnum-tag			#x37)
(define disp-compnum-tag		0)
(define disp-compnum-real		(* 1 wordsize))
(define disp-compnum-imag		(* 2 wordsize))
(define compnum-size			(* 4 wordsize))

(define off-compnum-tag			(fx- disp-compnum-tag  vector-tag))
(define off-compnum-real		(fx- disp-compnum-real vector-tag))
(define off-compnum-imag		(fx- disp-compnum-imag vector-tag))

;;; --------------------------------------------------------------------
;;; cflonums

(define cflonum-tag			#x47)
(define disp-cflonum-tag		0)
(define disp-cflonum-real		(* 1 wordsize))
(define disp-cflonum-imag		(* 2 wordsize))
(define cflonum-size			(* 4 wordsize))

(define off-cflonum-tag			(fx- disp-cflonum-tag  vector-tag))
(define off-cflonum-real		(fx- disp-cflonum-real vector-tag))
(define off-cflonum-imag		(fx- disp-cflonum-imag vector-tag))

;;; --------------------------------------------------------------------
;;; bignums

(define bignum-mask			#b111)
(define bignum-tag			#b011)

;;Bit mask to be AND-ed to the  first word in a bignum's memory block to
;;isolate the sign bit.
(define bignum-sign-mask		#b1000)

;;Given the  first word in  a bignum's memory  block, with the  sign bit
;;already isolated: we have to right-shift it by this amount to get 0 if
;;positive and 1 if negative.
(define bignum-sign-shift		3)

;;Given  the  first  word  in  a  bignum's  memory  block:  we  have  to
;;right-shift it by  this amount to get a machine  word (*not* a fixnum)
;;representing the  number of limbs  in the data  area.  Each limb  is a
;;machine word.
(define bignum-length-shift		4)

(define disp-bignum-tag			0)
(define disp-bignum-data		wordsize)

(define off-bignum-tag			(fx- disp-bignum-tag  vector-tag))
(define off-bignum-data			(fx- disp-bignum-data vector-tag))

;;; --------------------------------------------------------------------
;;; bytevectors

(define bytevector-mask			7)
(define bytevector-tag			2)
(define disp-bytevector-length		0)
(define disp-bytevector-data		8)
		;To  allow  the same  displacement  on  both 32-bit  and
		;64-bit platforms.

(define off-bytevector-length		(fx- disp-bytevector-length bytevector-tag))
(define off-bytevector-data		(fx- disp-bytevector-data   bytevector-tag))

;;; --------------------------------------------------------------------
;;; symbols

(define symbol-primary-tag		vector-tag)
(define symbol-tag			#x5F)
(define symbol-record-tag		#x5F)

(define disp-symbol-record-tag		0 #;(fx* 0 wordsize))
(define disp-symbol-record-string	(fx* 1 wordsize))
(define disp-symbol-record-ustring	(fx* 2 wordsize))
(define disp-symbol-record-value	(fx* 3 wordsize))
(define disp-symbol-record-proc		(fx* 4 wordsize))
(define disp-symbol-record-plist	(fx* 5 wordsize))
(define symbol-record-size		(fx* 6 wordsize))

(define off-symbol-record-tag		(fx- disp-symbol-record-tag     symbol-primary-tag))
(define off-symbol-record-string	(fx- disp-symbol-record-string  symbol-primary-tag))
(define off-symbol-record-ustring	(fx- disp-symbol-record-ustring symbol-primary-tag))
(define off-symbol-record-value		(fx- disp-symbol-record-value   symbol-primary-tag))
(define off-symbol-record-proc		(fx- disp-symbol-record-proc    symbol-primary-tag))
(define off-symbol-record-plist		(fx- disp-symbol-record-plist   symbol-primary-tag))

;;; --------------------------------------------------------------------
;;; structs

(define record-tag			vector-tag)
(define disp-struct-rtd			0)
(define disp-struct-std			0)
(define disp-struct-data		wordsize)

(define off-struct-rtd			(fx- disp-struct-rtd  vector-tag))
(define off-struct-std			(fx- disp-struct-std  vector-tag))
(define off-struct-data			(fx- disp-struct-data vector-tag))

(define idx-std-std			0)
(define idx-std-name			1)
(define idx-std-length			2)
(define idx-std-fields			3)
(define idx-std-printer			4)
(define idx-std-symbol			5)
(define idx-std-destructor		6)

;;Struct type descriptor fields.
(define disp-std-rtd			idx-std-std)
(define disp-std-std			idx-std-std)
(define disp-std-name			(fx* wordsize idx-std-name))
(define disp-std-length			(fx* wordsize idx-std-length))
(define disp-std-fields			(fx* wordsize idx-std-fields))
(define disp-std-printer		(fx* wordsize idx-std-printer))
(define disp-std-symbol			(fx* wordsize idx-std-symbol))
(define disp-std-destructor		(fx* wordsize idx-std-destructor))

(define off-std-rtd			(fx- disp-std-rtd	 vector-tag))
(define off-std-std			(fx- disp-std-std	 vector-tag))
(define off-std-name			(fx- disp-std-name	 vector-tag))
(define off-std-length			(fx- disp-std-length	 vector-tag))
(define off-std-fields			(fx- disp-std-fields	 vector-tag))
(define off-std-printer			(fx- disp-std-printer	 vector-tag))
(define off-std-symbol			(fx- disp-std-symbol	 vector-tag))
(define off-std-destructor		(fx- disp-std-destructor vector-tag))

;;; --------------------------------------------------------------------
;;; strings

(define string-mask			#b111)
(define string-tag			6)

(define disp-string-length		0)
(define disp-string-data		wordsize)

(define off-string-length		(fx- disp-string-length string-tag))
(define off-string-data			(fx- disp-string-data   string-tag))

;;; --------------------------------------------------------------------
;;; code objects

(define code-tag			#x2F)
(define disp-code-tag			0)
(define disp-code-instrsize		(* 1 wordsize))
(define disp-code-relocsize		(* 2 wordsize))
(define disp-code-freevars		(* 3 wordsize))
(define disp-code-annotation		(* 4 wordsize))
(define disp-code-unused		(* 5 wordsize))
(define disp-code-data			(* 6 wordsize))

(define off-code-tag			(fx- disp-code-tag vector-tag))
(define off-code-instrsize		(fx- disp-code-instrsize vector-tag))
(define off-code-relocsize		(fx- disp-code-relocsize vector-tag))
(define off-code-freevars		(fx- disp-code-freevars vector-tag))
(define off-code-annotation		(fx- disp-code-annotation vector-tag))
(define off-code-unused			(fx- disp-code-unused vector-tag))
(define off-code-data			(fx- disp-code-data vector-tag))

;;; --------------------------------------------------------------------
;;; closure objects

(define closure-mask			7)
(define closure-tag			3)

(define disp-closure-code		0)
(define disp-closure-data		wordsize)

(define off-closure-code		(fx- disp-closure-code closure-tag))
(define off-closure-data		(fx- disp-closure-data closure-tag))

;;; --------------------------------------------------------------------
;;; continuations

(define continuation-tag		#x1F)

(define disp-continuation-tag		0)
(define disp-continuation-top		(* 1 wordsize))
(define disp-continuation-size		(* 2 wordsize))
(define disp-continuation-next		(* 3 wordsize))
(define continuation-size		(* 4 wordsize))

(define off-continuation-tag		(fx- disp-continuation-tag  vector-tag))
(define off-continuation-top		(fx- disp-continuation-top  vector-tag))
(define off-continuation-size		(fx- disp-continuation-size vector-tag))
(define off-continuation-next		(fx- disp-continuation-next vector-tag))

;;; --------------------------------------------------------------------
;;; input/output ports

(define port-tag			#x3F)
(define port-mask			#x3F)

;;How many bits to right-shift the first  word in a port memory block to
;;isolate the port attributes.
(define port-attrs-shift		6)

;;These  values  must  be  kept  in   sync  with  the  ones  defined  in
;;"ikarus.io.sls".
(define INPUT-PORT-TAG			#b00000000000001)
(define OUTPUT-PORT-TAG			#b00000000000010)
(define TEXTUAL-PORT-TAG		#b00000000000100)
(define BINARY-PORT-TAG			#b00000000001000)

(define disp-port-attrs			0)
(define disp-port-index			(fx*  1 wordsize))
(define disp-port-size			(fx*  2 wordsize))
(define disp-port-buffer		(fx*  3 wordsize))
(define disp-port-transcoder		(fx*  4 wordsize))
(define disp-port-id			(fx*  5 wordsize))
(define disp-port-read!			(fx*  6 wordsize))
(define disp-port-write!		(fx*  7 wordsize))
(define disp-port-get-position		(fx*  8 wordsize))
(define disp-port-set-position!		(fx*  9 wordsize))
(define disp-port-close			(fx* 10 wordsize))
(define disp-port-cookie		(fx* 11 wordsize))
(define disp-port-unused1		(fx* 12 wordsize))
(define disp-port-unused2		(fx* 13 wordsize))
(define port-size			(fx* 14 wordsize))

(define off-port-attrs			(fx- disp-port-attrs		vector-tag))
(define off-port-index			(fx- disp-port-index		vector-tag))
(define off-port-size			(fx- disp-port-size		vector-tag))
(define off-port-buffer			(fx- disp-port-buffer		vector-tag))
(define off-port-transcoder		(fx- disp-port-transcoder	vector-tag))
(define off-port-id			(fx- disp-port-id		vector-tag))
(define off-port-read!			(fx- disp-port-read!		vector-tag))
(define off-port-write!			(fx- disp-port-write!		vector-tag))
(define off-port-get-position		(fx- disp-port-get-position	vector-tag))
(define off-port-set-position!		(fx- disp-port-set-position!	vector-tag))
(define off-port-close			(fx- disp-port-close		vector-tag))
(define off-port-cookie			(fx- disp-port-cookie		vector-tag))
(define off-port-unused1		(fx- disp-port-unused1		vector-tag))
(define off-port-unused2		(fx- disp-port-unused2		vector-tag))

;;; --------------------------------------------------------------------
;;; transcoders

(define transcoder-mask			#xFF) ;;; 0011
(define transcoder-tag			#x7F) ;;; 0011
(define transcoder-payload-shift	10)

(define transcoder-write-utf8-mask	#x1000)
(define transcoder-write-byte-mask	#x2000)
(define transcoder-read-utf8-mask	#x4000)
(define transcoder-read-byte-mask	#x8000)
(define transcoder-handling-mode-shift	16)
(define transcoder-handling-mode-bits	2)
(define transcoder-eol-style-shift	18)
(define transcoder-eol-style-bits	3)
(define transcoder-codec-shift		21)
(define transcoder-codec-bits		3)

(define transcoder-handling-mode:none	#b00)
(define transcoder-handling-mode:ignore	#b01)
(define transcoder-handling-mode:raise	#b10)
(define transcoder-handling-mode:replace #b11)

(define transcoder-eol-style:none	#b000)
(define transcoder-eol-style:lf		#b001)
(define transcoder-eol-style:cr		#b010)
(define transcoder-eol-style:crlf	#b011)
(define transcoder-eol-style:nel	#b100)
(define transcoder-eol-style:crnel	#b101)
(define transcoder-eol-style:ls		#b110)

(define transcoder-codec:none		#b000)
(define transcoder-codec:latin-1	#b001)
(define transcoder-codec:utf-8		#b010)
(define transcoder-codec:utf-16		#b011)

;;; --------------------------------------------------------------------
;;; pointer objects

(define pointer-tag			#x107)
(define disp-pointer-data		wordsize)
(define pointer-size			(* 2 wordsize))

(define off-pointer-data		(- disp-pointer-data vector-tag))

;;; --------------------------------------------------------------------
;;; tcbuckets

(define disp-tcbucket-tconc		0)
(define disp-tcbucket-key		(* 1 wordsize))
(define disp-tcbucket-val		(* 2 wordsize))
(define disp-tcbucket-next		(* 3 wordsize))
(define tcbucket-size			(* 4 wordsize))

(define off-tcbucket-tconc		(fx- disp-tcbucket-tconc vector-tag))
(define off-tcbucket-key		(fx- disp-tcbucket-key   vector-tag))
(define off-tcbucket-val		(fx- disp-tcbucket-val   vector-tag))
(define off-tcbucket-next		(fx- disp-tcbucket-next  vector-tag))

;;; --------------------------------------------------------------------

;;Whenever a "call" assembly instruction  is generated: the compiler, in
;;truth, generates this sequence:
;;
;;     jmp L0
;;     livemask-bytes		;array of bytes             |
;;     framesize		;data word, a "long"        | call
;;     rp_offset		;data word, a fixnum        | table
;;     multi-value-rp		;data word, assembly label  |
;;     pad-bytes
;;   L0:
;;     call function-address
;;   single-value-rp:		;single value return point
;;     ... instructions...
;;   multi-value-rp:		;multi value return point
;;     ... instructions...
;;
;;and remember that  the "call" pushes on the stack  the return address,
;;which is the label SINGLE-VALUE-RP.
;;
;;If the called function wants to  return a single argument: it can just
;;put it in EAX  and perform a "ret"; this will  make the execution flow
;;jump back to the entry point SINGLE-VALUE-RP.
;;
;;If the called function wants to return zero or 2 or more arguments: it
;;retrieves  the address  SINGLE-VALUE-RP  from the  stack,  adds to  it
;;DISP-MULTIVALUE-RP  as  defined  below  and  it  obtains  the  address
;;MULTI-VALUE-RP, then performs a "jmp" directly to MULTI-VALUE-RP.

;;Refer  to  the picture  in  src/ikarus-collect.c  for details  on  how
;;call-frames are laid out (search for livemask).
;;
(define call-instruction-size
  (boot.case-word-size
   ((32) 5)
   ((64) 10)))

;;Commented out because unused.
;;
;;(define disp-frame-size	(- (+ call-instruction-size (* 3 wordsize))))

;;Commented out because unused.
;;
;;(define disp-frame-offset	(- (+ call-instruction-size (* 2 wordsize))))

;;Multivalue return point.
;;
(define disp-multivalue-rp	(- (+ call-instruction-size (* 1 wordsize))))

(define dirty-word		-1)

;;; --------------------------------------------------------------------

;;(define pcb-allocation-pointer(*  0 wordsize)) NOT USED
(define pcb-allocation-redline	(*  1 wordsize))
;;;(define pcb-frame-pointer	(*  2 wordsize)) NOT USED
(define pcb-frame-base		(*  3 wordsize))
(define pcb-frame-redline	(*  4 wordsize))
(define pcb-next-continuation	(*  5 wordsize)) ;this is the C field "next_k"
;;(define pcb-system-stack	(*  6 wordsize)) NOT USED
(define pcb-dirty-vector	(*  7 wordsize))
(define pcb-arg-list		(*  8 wordsize))
(define pcb-engine-counter	(*  9 wordsize))
(define pcb-interrupted		(* 10 wordsize))
(define pcb-base-rtd		(* 11 wordsize))
(define pcb-collect-key		(* 12 wordsize))


;;;; utility functions for assembly code generation

(module (fx? max-bitcount-in-fixnum-binary-representation)

  (define-constant max-bitcount-in-fixnum-binary-representation
    (- (* wordsize 8) fx-shift))

  (define-constant intbits
    ($fx* wordsize 8))
  (define-constant fxbits
    ($fx- intbits fx-shift))
  (define-constant t
    (expt 2 ($fx- fxbits 1)))
  (define-constant least-signed-machine-word
    (- t))
  (define-constant greatest-signed-machine-word
    (- t 1))

  (define (fx? x)
    ;;Return true if X is an exact signed integer that fits in a machine word.
    ;;
    (and (or (fixnum? x)
	     (bignum? x))
	 (<= least-signed-machine-word x greatest-signed-machine-word)))

  #| end od module |# )


;;;; more assembly code helpers

(module ()
  ;;Initialize the cogen.  This parameter is used by the assembler.
  (code-entry-adjustment (- disp-code-data vector-tag)))

;;; --------------------------------------------------------------------

(define (align n)
  ($fxsll ($fxsra ($fx+ n ($fxsub1 object-alignment))
		  align-shift)
	  align-shift))

(define (mem off val)
  (cond ((fixnum? off)
	 (list 'disp (int off) val))
	((register? off)
	 (list 'disp off val))
	(else
	 (error 'mem "invalid disp" off))))

(define-syntax int
  (syntax-rules ()
    ((_ x) x)))

(define (obj x)		(list 'obj x))
(define (byte x)	(list 'byte x))
(define (byte-vector x) (list 'byte-vector x))
(define (movzbl src targ) (list 'movzbl src targ))
(define (sall src targ)	(list 'sall src targ))
(define (sarl src targ) (list 'sarl src targ))
(define (shrl src targ) (list 'shrl src targ))
(define (notl src)	(list 'notl src))
(define (pushl src)	(list 'pushl src))
(define (popl src)	(list 'popl src))
(define (orl src targ)	(list 'orl src targ))
(define (xorl src targ) (list 'xorl src targ))
(define (andl src targ) (list 'andl src targ))
(define (movl src targ) (list 'movl src targ))
(define (leal src targ) (list 'leal src targ))
(define (movb src targ) (list 'movb src targ))
(define (addl src targ) (list 'addl src targ))
(define (imull src targ) (list 'imull src targ))
(define (idivl src)	(list 'idivl src))
(define (subl src targ) (list 'subl src targ))
(define (push src)	(list 'push src))
(define (pop targ)	(list 'pop targ))
(define (sete targ)	(list 'sete targ))
(define (call targ)	(list 'call targ))

(define (tail-indirect-cpr-call)
  ;;Fetch a  binary code address  from the closure object  referenced by
  ;;the CPR (Closure Pointer Register) and jump directly there.
  ;;
  (jmp  (mem off-closure-code cpr)))

(define (indirect-cpr-call)
  ;;Fetch a  binary code address  from the closure object  referenced by
  ;;the CPR (Closure Pointer Register) and perform a call to there.
  ;;
  (call (mem off-closure-code cpr)))

(define (negl targ)	(list 'negl targ))
(define (label x)	(list 'label x))
(define (label-address x) (list 'label-address x))
(define (ret)		'(ret))
(define (cltd)		'(cltd))
(define (cmpl arg1 arg2) (list 'cmpl arg1 arg2))
(define (je label)	(list 'je label))
(define (jne label)	(list 'jne label))
(define (jle label)	(list 'jle label))
(define (jge label)	(list 'jge label))
(define (jg label)	(list 'jg label))
(define (jl label)	(list 'jl label))
(define (jb label)	(list 'jb label))
(define (ja label)	(list 'ja label))
(define (jo label)	(list 'jo label))
(define (jmp label)	(list 'jmp label))

(define esp		'%esp) ; stack frame pointer
(define al		'%al)
(define ah		'%ah)
(define bh		'%bh)
(define cl		'%cl)
(define eax		'%eax)
(define ebx		'%ebx)
(define ecx		'%ecx)
(define edx		'%edx)
(define apr		'%ebp) ; allocation pointer
(define fpr		'%esp) ; frame pointer
(define cpr		'%edi) ; closure pointer
(define pcr		'%esi) ; pcb pointer
(define register?	symbol?)

(define (argc-convention n)
  ;;At  run  time:  the  number  of arguments  in  a  function  call  is
  ;;represented by  a negative fixnum  which is the number  of arguments
  ;;negated.  Example: -2 <-> 2 arguments.
  ;;
  ($fx- 0 ($fxsll n fx-shift)))


;;;; assembly labels for common subroutines

(module (refresh-cached-labels!
	 sl-annotated-procedure-label
	 sl-apply-label
	 sl-continuation-code-label
	 sl-invalid-args-label
	 sl-mv-ignore-rp-label
	 sl-mv-error-rp-label
	 sl-values-label
	 sl-cwv-label)

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

  (define-syntax define-cached
    (lambda (x)
      (syntax-case x (public-function
		      entry-point-label number-of-free-variables
		      code-annotation definitions local-labels
		      assembly)
        ((_ ?refresh
	    ((public-function		?func-name)
	     (entry-point-label		?label-name)
	     (number-of-free-variables	?freevars)
	     (code-annotation		?annotation)
	     (definitions		?def ...)
	     (local-labels		?lab ...)
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
		 (let* ((?func-name
			 (let ((label
				(let ((?label-name (gensym (symbol->string '?label-name))))
				  ?def ...
				  (define ?lab (gensym (symbol->string (quote ?lab))))
				  ...
				  (assemble-sources thunk?-label
				    (list (cons* ?freevars ?annotation
						 (list ?body0 ?body ...))))
				  ?label-name)))
			   (set! LABEL-GENSYM label)
			   (lambda () label)))
			...)
		   (void))))
	   ))
	)))

  (define-cached refresh-cached-labels!

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
     (code-annotation		`(name ,(make-annotation-indirect)))
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
      (movl (mem ($fx- ($fx+ disp-closure-data wordsize) closure-tag)
		 cpr)
	    cpr)
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
    ((public-function	sl-apply-label)
     (entry-point-label		SL_apply)
     (number-of-free-variables	0)
     (code-annotation		(label SL_apply))
     (definitions)
     (local-labels L_apply_done
		   L_apply_loop)
     ;;We suppose that:  at (descending) offset EAX from  the address in
     ;;FPR (Frame  Pointer Register)  there is a  reference to  a Scheme
     ;;list.
     (assembly
      ;;Load in EBX the word at offset EAX from the frame pointer.
      (movl (mem fpr eax) ebx)
      ;;If EBX holds the Scheme null object ...
      (cmpl (int nil) ebx)
      ;;... there are no further arguments to push on the stack.
      (je (label L_apply_done))

      (label L_apply_loop)
      ;;Load in ECX the car.
      (movl (mem off-car ebx) ecx)
      ;;Load in EBX the cdr.
      (movl (mem off-cdr ebx) ebx)
      ;;Move the car at offset EAX from the frame pointer.
      (movl ecx (mem fpr eax))
      ;;Decrement  EAX  by the  word  size:  stack  offset of  the  next
      ;;argument, if any.
      (subl (int wordsize) eax)
      ;;If EBX does not hold the Scheme null object ...
      (cmpl (int nil) ebx)
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
    ;;* The ARGC-REGISTER must contain a fixnum representing the negated
    ;;   number  of  arguments  handed  to  the  escape  function;  such
    ;;  arguments are the values returned to the topmost function in the
    ;;  continuation.
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
     (code-annotation		(label SL_continuation_code))
     (definitions)
     (local-labels L_cont_zero_args
		   L_cont_mult_args
		   L_cont_one_arg
		   L_cont_mult_move_args
		   L_cont_mult_copy_loop)
     (assembly
      ;;Move in EBX  the reference to the  continuation object contained
      ;;in the first data slot in the closure object.
      (movl (mem off-closure-data cpr) ebx)
      ;;Move  the   reference  to  continuation  object   in  the  field
      ;;"pcb->next_k" (overwriting the old value!!!).
      (movl ebx (mem pcb-next-continuation pcr))
      ;;Move in EBX the field  "pcb->frame_base".  Notice that we do not
      ;;touch "pcb->frame_pointer" here.
      (movl (mem pcb-frame-base pcr) ebx)
      ;;Dispatch according to  the number of arguments to  return to the
      ;;continuation.
      (cmpl (int (argc-convention 1)) eax)
      (jg (label L_cont_zero_args)) ;jump if -1 > EAX: less than one arg
      (jl (label L_cont_mult_args)) ;jump if -1 < EAX: more than one arg

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
      (movl (mem (fx- wordsize) fpr) eax)
      ;;Load   in   the   Frame    Pointer   Register   the   value   of
      ;;"pcb->frame_base".
      (movl ebx fpr)
      ;;Decrement  Frame Pointer  Register  by a  wordsize,  so that  it
      ;;contains the address of the  highest machine word in the current
      ;;Scheme stack segment.
      (subl (int wordsize) fpr)
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
      (movl ebx fpr)
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
      (jmp (mem disp-multivalue-rp ebx))

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
      (cmpl ebx fpr)
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
      (jmp (mem disp-multivalue-rp ebx))

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
      (subl (int wordsize) ecx)	;decrement ECX
      (movl (mem fpr ecx) edx)	;load arg in EDX from source slot
      (movl edx (mem ebx ecx))	;store arg from EDX to dest slot
      (cmpl ecx eax)		;moved all?
      (jne (label L_cont_mult_copy_loop))
      ;;Store  "pcb->frame_base   -  wordsize"  in  the   Frame  Pointer
      ;;Register.
      (movl ebx fpr)
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
      (jmp (mem disp-multivalue-rp ebx))
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
     (code-annotation		(label SL_invalid_args))
     (definitions)
     (local-labels)
     (assembly
      ;;Store on the  stack a reference to the closure  object (from the
      ;;Closure  Pointer Register)  as  first argument  to  the call  to
      ;;$INCORRECT-ARGS-ERROR-HANDLER.
      (movl cpr (mem (fx- wordsize) fpr))
      ;;Decode  the incorrect  number  of  arguments, so  that  it is  a
      ;;non-negative fixnum.
      (negl eax)
      ;;Store on the  stack the incorrect number of  arguments as second
      ;;argument to the call to $INCORRECT-ARGS-ERROR-HANDLER.
      (movl eax (mem (fx- (fx* 2 wordsize)) fpr))
      ;;From the  relocation vector  of this  code object:  retrieve the
      ;;location gensym associated  to $INCORRECT-ARGS-ERROR-HANDLER and
      ;;load it in the Closure Pointer Register (CPR).
      ;;
      ;;The "proc" slot  of such loc gensym contains a  reference to the
      ;;closure object implementing $INCORRECT-ARGS-ERROR-HANDLER.
      (movl (obj (primitive-public-function-name->location-gensym '$incorrect-args-error-handler)) cpr)
      ;;Load in the Closure Pointer  Register a reference to the closure
      ;;object implementing the function $INCORRECT-ARGS-ERROR-HANDLER.
      (movl (mem off-symbol-record-proc cpr) cpr)
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
     (code-annotation		(label SL_multiple_values_ignore_rp))
     (definitions)
     (local-labels)
     (assembly
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
     (code-annotation		(label SL_multiple_values_error_rp))
     (definitions)
     (local-labels)
     (assembly
      ;;From the  relocation vector  of this  code object:  retrieve the
      ;;location gensym associated to $MULTIPLE-VALUES-ERROR and load it
      ;;in the Closure Pointer Register  (CPR).  The "proc" slot of such
      ;;loc  gensym   contains  a   reference  to  the   closure  object
      ;;implementing $MULTIPLE-VALUES-ERROR.
      (movl (obj (primitive-public-function-name->location-gensym '$multiple-values-error)) cpr)
      ;;Load in the Closure Pointer  Register a reference to the closure
      ;;object implementing the function $MULTIPLE-VALUES-ERROR.
      (movl (mem off-symbol-record-proc cpr) cpr)
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
     (code-annotation		'(name values))
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
      (movl (mem 0 fpr) ebx)
      (jmp (mem disp-multivalue-rp ebx))

      ;;Return a  single value.  Store  in EAX the single  return value,
      ;;then "ret".
      (label L_values_one_value)
      (movl (mem (fx- wordsize) fpr) eax)
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
     (code-annotation		'(name call-with-values))
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
      (movl (mem (fx- wordsize) fpr) ebx)
      (movl ebx cpr)
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
       3	;The  frame-words-count is  3  because on  the stack  of
		;CALL-WITH-VALUES  there  are  3 machine  words:  return
		;address, producer closure, consumer closure.
       '#(#b110) ;This livemask tells the  garbage collector that on the
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
      (movl (mem (fx* -2 wordsize) fpr) ebx)
      ;;Store in the Continuation Pointer  Register (CPR) a reference to
      ;;the consumer closure object.
      (movl ebx cpr)
      ;;Check that EBX actually contains  a reference to closure object;
      ;;else jump to the appropriate error handler.
      (andl (int closure-mask) ebx)
      (cmpl (int closure-tag) ebx)
      (jne (label SL_nonprocedure))
      ;;Store the  returned value on  the stack, right below  the return
      ;;address.
      (movl eax (mem (fx- wordsize) fpr))
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
      (addl (int (fx* wordsize 3)) fpr)
      ;;Store  in  the  Closure  Pointer Register  a  reference  to  the
      ;;consumer  closure  object.  We  will  check  below that  CPR  is
      ;;actually a  reference to closure  object (to avoid  moving stuff
      ;;into registers twice).
      (movl (mem (fx* -2 wordsize) fpr) cpr)
      ;;Check if the number of returned values is zero.
      (cmpl (int (argc-convention 0)) eax)
      (je (label L_cwv_done))
      ;;Make EBX reference the first return value on the stack.
      (movl (int (fx* -4 wordsize)) ebx)
      (addl fpr ebx)
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
      (movl cpr ebx)
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
      (movl cpr (mem (fx- wordsize) fpr))
      ;;From the  relocation vector  of this  code object:  retrieve the
      ;;location gensym  associated to $APPLY-NONPROCEDURE-ERROR-HANDLER
      ;;and load it  in the Closure Pointer Register  (CPR).  The "proc"
      ;;slot  of such  loc gensym  contains a  reference to  the closure
      ;;object implementing $APPLY-NONPROCEDURE-ERROR-HANDLER.
      (movl (obj (primitive-public-function-name->location-gensym '$apply-nonprocedure-error-handler)) cpr)
      ;;Load in the Closure Pointer  Register a reference to the closure
      ;;object implementing $APPLY-NONPROCEDURE-ERROR-HANDLER.
      (movl (mem off-symbol-record-proc cpr) cpr)
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
      (movl cpr (mem (fx- wordsize) fpr))
      ;;Decode the  number of  arguments, so that  it is  a non-negative
      ;;fixnum.
      (negl eax)
      ;;Put on the stack the incorrect number of arguments as fixnum, as
      ;;second argument to $INCORRECT-ARGS-ERROR-HANDLER.
      (movl eax (mem (fx- 0 (fx* 2 wordsize)) fpr))
      ;;From the  relocation vector  of this  code object:  retrieve the
      ;;location gensym associated  to $INCORRECT-ARGS-ERROR-HANDLER and
      ;;load it in the Closure  Pointer Register (CPR).  The "proc" slot
      ;;of such  loc gensym contains  a reference to the  closure object
      ;;implementing $INCORRECT-ARGS-ERROR-HANDLER.
      (movl (obj (primitive-public-function-name->location-gensym '$incorrect-args-error-handler)) cpr)
      ;;Load in the Closure Pointer  Register a reference to the closure
      ;;object implementing $INCORRECT-ARGS-ERROR-HANDLER.
      (movl (mem off-symbol-record-proc cpr) cpr)
      ;;Load in  EAX the  encoded number  of arguments  for the  call to
      ;;$INCORRECT-ARGS-ERROR-HANDLER.
      (movl (int (argc-convention 2)) eax)
      ;;Fetch a binary  code address from the  closure object referenced
      ;;by the CPR (Closure Pointer Register) and jump directly there.
      (tail-indirect-cpr-call)
      )))

  #| end of module |# )


;;;; external code for actual code generation

(include "ikarus.compiler.altcogen.ss" #t)


(define (unparse-recordized-code x)
  ;;Unparse the struct  instance X (representing recordized  code in the
  ;;core  language  already processed  by  the  compiler) into  a  human
  ;;readable symbolic expression to be used when raising errors.
  ;;
  ;;Being  that this  function is  used only  when signaling  errors: it
  ;;makes no sense to use unsafe operations: let's keep it safe!!!
  ;;
  (struct-case x
    ((constant c)
     `(quote ,c))

    ((known expr type)
     `(known ,(unparse-recordized-code expr) ,(T:description type)))

    ((code-loc x)
     `(code-loc ,x))

    ((var x)
     (string->symbol (format ":~a" x)))

    ((prelex name)
     (string->symbol (format ":~a" name)))

    ((primref x)
     x)

    ((conditional test conseq altern)
     `(conditional ,(unparse-recordized-code test)
		   ,(unparse-recordized-code conseq)
		   ,(unparse-recordized-code altern)))

    ((interrupt-call e0 e1)
     `(interrupt-call ,(unparse-recordized-code e0) ,(unparse-recordized-code e1)))

    ((primcall op arg*)
     `(,op . ,(map unparse-recordized-code arg*)))

    ((bind lhs* rhs* body)
     `(let ,(map (lambda (lhs rhs)
		   (list (unparse-recordized-code lhs) (unparse-recordized-code rhs)))
	      lhs* rhs*)
	,(unparse-recordized-code body)))

    ((recbind lhs* rhs* body)
     `(letrec ,(map (lambda (lhs rhs)
		      (list (unparse-recordized-code lhs) (unparse-recordized-code rhs)))
		 lhs* rhs*)
	,(unparse-recordized-code body)))

    ((rec*bind lhs* rhs* body)
     `(letrec* ,(map (lambda (lhs rhs)
		       (list (unparse-recordized-code lhs) (unparse-recordized-code rhs)))
		  lhs* rhs*)
	,(unparse-recordized-code body)))

    ;;Commented out because unused;  notice that LIBRARY-LETREC* forms
    ;;are  represented by  structures of  type REC*BIND;  there is  no
    ;;structure of type LIBRARY-RECBIND.  (Marco Maggi; Oct 11, 2012)
    ;;
    ;; ((library-recbind lhs* loc* rhs* body)
    ;;  `(letrec ,(map (lambda (lhs loc rhs)
    ;; 			(list (unparse-recordized-code lhs) loc (unparse-recordized-code rhs)))
    ;; 		   lhs* loc* rhs*)
    ;; 	  ,(unparse-recordized-code body)))

    ((fix lhs* rhs* body)
     `(fix ,(map (lambda (lhs rhs)
		   (list (unparse-recordized-code lhs) (unparse-recordized-code rhs)))
	      lhs* rhs*)
	   ,(unparse-recordized-code body)))

    ((seq e0 e1)
     (letrec ((recur (lambda (x ac)
		       (struct-case x
			 ((seq e0 e1)
			  (recur e0 (recur e1 ac)))
			 (else
			  (cons (unparse-recordized-code x) ac))))))
       (cons 'seq (recur e0 (recur e1 '())))))

    ((clambda-case info body)
     `(,(if (case-info-proper info)
	    (map unparse-recordized-code (case-info-args info))
	  ;;The loop below  is like MAP but for improper  lists: it maps
	  ;;UNPARSE-RECORDIZED-CODE over the improper list X.
	  (let ((X (case-info-args info)))
	    (let recur ((A (car X))
			(D (cdr X)))
	      (if (null? D)
		  (unparse-recordized-code A)
		(cons (unparse-recordized-code A) (recur (car D) (cdr D)))))))
       ,(unparse-recordized-code body)))

    ((clambda label cls* cp freevar*)
     ;;FIXME Should we print more fields?  (Marco Maggi; Oct 11, 2012)
     `(clambda (label: ,label)
	       (cp:    ,(unparse-recordized-code cp))
	       (free:  ,(and freevar* (map unparse-recordized-code freevar*)))
	       ,@(map unparse-recordized-code cls*)))

    ((closure-maker code freevar* recursive?)
     `(closure ,(if recursive? '(recursive: #t) '(recursive: #f))
	       (freevars: ,(map unparse-recordized-code freevar*))
	       ,(unparse-recordized-code code)))

    ((codes list body)
     `(codes ,(map unparse-recordized-code list)
	     ,(unparse-recordized-code body)))

    ((funcall rator rand*)
     `(funcall ,(unparse-recordized-code rator) . ,(map unparse-recordized-code rand*)))

    ((jmpcall label rator rand*)
     `(jmpcall ,label ,(unparse-recordized-code rator) . ,(map unparse-recordized-code rand*)))

    ((forcall rator rand*)
     `(foreign-call ,rator . ,(map unparse-recordized-code rand*)))

    ((assign lhs rhs)
     `(set! ,(unparse-recordized-code lhs) ,(unparse-recordized-code rhs)))

    ((return x)
     `(return ,(unparse-recordized-code x)))

    ((new-frame base-idx size body)
     `(new-frame (base: ,base-idx)
		 (size: ,size)
		 ,(unparse-recordized-code body)))

    ((frame-var idx)
     (string->symbol (format "fv.~a" idx)))

    ((cp-var idx)
     (string->symbol (format "cp.~a" idx)))

    ((save-cp expr)
     `(save-cp ,(unparse-recordized-code expr)))

    ((eval-cp check body)
     `(eval-cp ,check ,(unparse-recordized-code body)))

    ((call-cp call-convention label save-cp? rp-convention base-idx arg-count live-mask)
     `(call-cp (conv:		,call-convention)
	       (label:	,label)
	       (rpconv:	,(if (symbol? rp-convention)
			     rp-convention
			   (unparse-recordized-code rp-convention)))
	       (base-idx:	,base-idx)
	       (arg-count:	,arg-count)
	       (live-mask:	,live-mask)))

    ((tailcall-cp convention label arg-count)
     `(tailcall-cp ,convention ,label ,arg-count))

    ((foreign-label x)
     `(foreign-label ,x))

    ((fvar idx)
     (string->symbol (format "fv.~a" idx)))

    ((nfv idx)
     'nfv)

    ((locals vars body)
     `(locals ,(map unparse-recordized-code vars) ,(unparse-recordized-code body)))

    ((asm-instr op d s)
     `(asm ,op ,(unparse-recordized-code d) ,(unparse-recordized-code s)))

    ((disp s0 s1)
     `(disp ,(unparse-recordized-code s0) ,(unparse-recordized-code s1)))

    ((nframe vars live body)
     `(nframe
       #;(vars: ,(map unparse-recordized-code vars))
       #;(live: ,(map unparse-recordized-code live))
       ,(unparse-recordized-code body)))

    ((shortcut body handler)
     `(shortcut ,(unparse-recordized-code body) ,(unparse-recordized-code handler)))

    ((ntcall target valuw args mask size)
     `(ntcall ,target ,size))

    (else x)))


(module (unparse-recordized-code/sexp)
  ;;Unparse the struct instance INPUT-EXPR  (representing recordized code in the core
  ;;language  already processed  by  the  compiler) into  a  human readable  symbolic
  ;;expression to  be used  when printing  to some  port for  miscellaneous debugging
  ;;purposes.
  ;;
  ;;This  module  attempts  to  unparse  recordized code  and  construct  a  symbolic
  ;;expression that still represents the struct types in the recordized code.
  ;;
  ;;This function recognises only structures of the following type:
  ;;
  ;;   assign		bind		clambda
  ;;   conditional	constant	fix
  ;;   forcall		foreign-label	funcall
  ;;   known		prelex		primcall
  ;;   primref		rec*bind	recbind
  ;;   seq		var
  ;;
  ;;other values are not processed and are returned as they are.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'unparse-recordized-code/sexp))

  (define (unparse-recordized-code/sexp input-expr)
    ;;
    ;;A lot of functions are nested here  because they make use of the closure "Var",
    ;;which has internal state.
    ;;
    ;;*NOTE* Being that this function is used  only when debugging: it makes no sense
    ;;to use unsafe operations: LET'S KEEP IT SAFE!!!
    ;;
    (define (E x)
      (struct-case x
	((constant c)
	 (if (symbol? c)
	     ;;Extract the pretty name; this is useful when C is a loc gensym.
	     `(constant ,(%pretty-symbol c))
	   `(constant ,c)))

	((prelex)
	 (Var x))

	((var)
	 (Var x))

	((assign lhs rhs)
	 (if (symbol? (prelex-source-assigned? lhs))
	     `(assign-init ,(E lhs) ,(E rhs))
	   `(assign ,(E lhs) ,(E rhs))))

	((primref x)
	 `(primref ,x))

	((known expr type)
	 `(known ,(E expr) ,(T:description type)))

	((clambda)
	 (E-clambda x))

	((closure-maker code freevar* recursive?)
	 `(closure-maker ,(E code)
			 ,(let ((freevar* (map E freevar*)))
			    (if (null? freevar*)
				'no-freevars
			      `(freevars: . ,freevar*)))))

	((primcall op arg*)
	 (cons* 'primcall op (%map-in-order E arg*)))

	((funcall rator rand*)
	 (let ((rator (E rator)))
	   (cons* 'funcall rator (%map-in-order E rand*))))

	((forcall rator rand*)
	 `(foreign-call ,rator . ,(%map-in-order E rand*)))

	((jmpcall label op rand*)
	 `(jmpcall ,(%pretty-symbol label) ,(E op) . ,(map E rand*)))

	((foreign-label x)
	 `(foreign-label ,x))

	((seq e0 e1)
	 (%do-seq e0 e1))

	((conditional test conseq altern)
	 (let ((test^   (E test))
	       (conseq^ (E conseq))
	       (altern^ (E altern)))
	   (list 'conditional test^ conseq^ altern^)))

	((bind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order Var lhs*))
		(rhs* (%map-in-order E   rhs*))
		(body (E body)))
	   (list 'bind (map list lhs* rhs*) body)))

	((fix lhs* rhs* body)
	 (let* ((lhs* (%map-in-order Var lhs*))
		(rhs* (%map-in-order E   rhs*))
		(body (E body)))
	   (list 'fix (map list lhs* rhs*) body)))

	((recbind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order Var lhs*))
		(rhs* (%map-in-order E   rhs*))
		(body (E body)))
	   (list 'recbind (map list lhs* rhs*) body)))

	((rec*bind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order Var lhs*))
		(rhs* (%map-in-order E   rhs*))
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

	(else x)))

    (module (Var)
      ;;Given a struct instance X of type  PRELEX or VAR, identifying the location of
      ;;a binding: return  a symbol representing a unique name  for the binding.  The
      ;;map between structures and symbols is cached in a hash table.
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
      (define H
	;;Map PRELEX and VAR structures to already built binding name symbols.
	(make-eq-hashtable))
      (define T
	;;Map binding  pretty string names  to number of  times this string  name has
	;;already been used.
	(make-hashtable string-hash string=?))
      (define (Var x)
	(or (hashtable-ref H x #f)
	    (struct-case x
	      ((prelex x.name)
	       (%build-name x x.name))
	      ((var x.name)
	       (%build-name x x.name))
	      (else x))))

      (define (%build-name x x.name)
	(let* ((name (symbol->string x.name))
	       (N    (hashtable-ref T name 0)))
	  (hashtable-set! T name (+ N 1))
	  (receive-and-return (sym)
	      (string->symbol (string-append name "_" (number->string N)))
	    (hashtable-set! H x sym))))

      #| end of module: Var |# )

   (define (%do-seq e0 e1)
      (cons 'seq
	    ;;Here we flatten nested SEQ instances into a unique output SEQ form.
	    (let recur ((expr  e0)
			(expr* (list e1)))
	      (struct-case expr
		((seq expr.e0 expr.e1)
		 (recur expr.e0 (cons expr.e1 expr*)))
		(else
		 (let ((expr^ (E expr)))
		   (if (null? expr*)
		       (list expr^)
		     (cons expr^ (recur (car expr*) (cdr expr*))))))))))

    (module (E-clambda)

      (define (E-clambda x)
	(struct-case x
	  ((clambda label.unused cls*)
	   (let ((cls* (%map-in-order E-clambda-clause cls*)))
	     (if (= (length cls*) 1)
		 (cons 'lambda (car cls*))
	       (cons 'case-lambda cls*))))))

      (define (E-clambda-clause x)
	(struct-case x
	  ((clambda-case info body)
	   (let ((args (E-args (case-info-proper info) (case-info-args info))))
	     (list args (E body))))))

      (define (E-args proper x)
	(if proper
	    (%map-in-order Var x)
	  ;;The loop below is  like MAP but for improper lists: it  maps Var over the
	  ;;improper list X.
	  (let recur ((A (car x))
		      (D (cdr x)))
	    (if (null? D)
		(Var A)
	      (let ((A (Var A)))
		(cons A (recur (car D) (cdr D))))))))

      #| end of module: E-clambda |# )

    (E input-expr))

;;; --------------------------------------------------------------------

  (define (%map-in-order f ls)
    ;;This version of  MAP imposes an order to  the application of F to  the items in
    ;;LS.
    ;;
    (if (null? ls)
	'()
      (let ((a (f (car ls))))
	(cons a (%map-in-order f (cdr ls))))))

  (define (%pretty-symbol sym)
    (string->symbol (symbol->string sym)))

  #| end of module: unparse-recordized-code/sexp |# )


(module (unparse-recordized-code/pretty)
  ;;Unparse the struct instance INPUT-EXPR  (representing recordized code in the core
  ;;language  already processed  by  the  compiler) into  a  human readable  symbolic
  ;;expression to  be used  when printing  to some  port for  miscellaneous debugging
  ;;purposes.
  ;;
  ;;This module  attempts to  unparse recordized code  and reconstruct  a Scheme-like
  ;;symbolic expression; the returned sexp does *not* exactly represent the input.
  ;;
  ;;This function recognises only structures of the following type:
  ;;
  ;;   assign		bind		clambda
  ;;   conditional	constant	fix
  ;;   forcall		foreign-label	funcall
  ;;   known		prelex		primcall
  ;;   primref		rec*bind	recbind
  ;;   seq		var
  ;;
  ;;other values are not processed and are returned as they are.
  ;;
  (define who 'unparse-recordized-code/pretty)

  (define (unparse-recordized-code/pretty input-expr)
    ;;
    ;;A lot of functions are nested here  because they make use of the closure "Var",
    ;;which has internal state.
    ;;
    ;;*NOTE* Being that this function is used  only when debugging: it makes no sense
    ;;to use unsafe operations: LET'S KEEP IT SAFE!!!
    ;;
    (define (E x)
      (struct-case x
	((constant c)
	 (if (symbol? c)
	     ;;Extract the pretty name.
	     `(quote ,(%pretty-symbol c))
	   `(quote ,c)))

	((prelex)
	 (Var x))

	((var)
	 (Var x))

	((assign lhs rhs)
	 `(set! ,(E lhs) ,(E rhs)))

	((primref x)
	 x)

	((known expr type)
	 `(known ,(E expr) ,(T:description type)))

	((clambda)
	 (E-clambda x))

	((closure-maker code freevar* recursive?)
	 `(closure-maker ,(E code)
			 ,(let ((freevar* (map E freevar*)))
			    (if (null? freevar*)
				'no-freevars
			      `(freevars: . ,freevar*)))))

	((primcall op arg*)
	 (cons op (%map-in-order E arg*)))

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
	 (%do-seq e0 e1))

	((conditional test conseq altern)
	 (let ((test^   (E test))
	       (conseq^ (E conseq))
	       (altern^ (E altern)))
	   (list 'if test^ conseq^ altern^)))

	((bind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order Var lhs*))
		(rhs* (%map-in-order E   rhs*))
		(body (E body)))
	   (%build-let (map list lhs* rhs*) body)))

	((fix lhs* rhs* body)
	 (let* ((lhs* (%map-in-order Var lhs*))
		(rhs* (%map-in-order E   rhs*))
		(body (E body)))
	   (list 'fix (map list lhs* rhs*) body)))

	((recbind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order Var lhs*))
		(rhs* (%map-in-order E   rhs*))
		(body (E body)))
	   (list 'letrec (map list lhs* rhs*) body)))

	((rec*bind lhs* rhs* body)
	 (let* ((lhs* (%map-in-order Var lhs*))
		(rhs* (%map-in-order E   rhs*))
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

	(else x)))

    (module (Var)
      ;;Given a struct instance X of type  PRELEX or VAR, identifying the location of
      ;;a binding: return  a symbol representing a unique name  for the binding.  The
      ;;map between structures and symbols is cached in a hash table.
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
      (define H
	;;Map PRELEX and VAR structures to already built binding name symbols.
	(make-eq-hashtable))
      (define T
	;;Map binding  pretty string names  to number of  times this string  name has
	;;already been used.
	(make-hashtable string-hash string=?))
      (define (Var x)
	(or (hashtable-ref H x #f)
	    (struct-case x
	      ((prelex x.name)
	       (%build-name x x.name))
	      ((var x.name)
	       (%build-name x x.name))
	      (else x))))

      (define (%build-name x x.name)
	(let* ((name (symbol->string x.name))
	       (N    (hashtable-ref T name 0)))
	  (hashtable-set! T name (+ N 1))
	  (receive-and-return (sym)
	      (string->symbol (string-append name "_" (number->string N)))
	    (hashtable-set! H x sym))))

      #| end of module: Var |# )

    (define (%do-seq e0 e1)
      (cons 'begin
	    ;;Here we flatten nested SEQ instances into a unique output SEQ form.
	    (let recur ((expr  e0)
			(expr* (list e1)))
	      (struct-case expr
		((seq expr.e0 expr.e1)
		 (recur expr.e0 (cons expr.e1 expr*)))
		(else
		 (let ((expr^ (E expr)))
		   (if (null? expr*)
		       (list expr^)
		     (cons expr^ (recur (car expr*) (cdr expr*))))))))))

    (module (E-clambda)

      (define (E-clambda x)
	(struct-case x
	  ((clambda label.unused cls*)
	   (let ((cls* (%map-in-order E-clambda-clause cls*)))
	     (if (= (length cls*) 1)
		 (cons 'lambda (car cls*))
	       (cons 'case-lambda cls*))))))

      (define (E-clambda-clause x)
	(struct-case x
	  ((clambda-case info body)
	   (let ((args (E-args (case-info-proper info) (case-info-args info))))
	     (list args (E body))))))

      (define (E-args proper x)
	(if proper
	    (%map-in-order Var x)
	  ;;The loop below is  like MAP but for improper lists: it  maps Var over the
	  ;;improper list X.
	  (let recur ((A (car x))
		      (D (cdr x)))
	    (if (null? D)
		(Var A)
	      (let ((A (Var A)))
		(cons A (recur (car D) (cdr D))))))))

      #| end of module: E-clambda |# )

    (E input-expr))

;;; --------------------------------------------------------------------

  (define (%map-in-order f ls)
    ;;This version of  MAP imposes an order to  the application of F to  the items in
    ;;LS.
    ;;
    (if (null? ls)
	'()
      (let ((a (f (car ls))))
	(cons a (%map-in-order f (cdr ls))))))

  (define (%build-let b* body)
    ;;B* must be a list of already unparsed LET-like bindings; BODY must
    ;;be an already unparsed symbolic expression representing a body.
    ;;
    ;;If B*  represents a single  binding: compress nested LET  and LET*
    ;;forms  into  a  single  LET*  form.   If  B*  represents  multiple
    ;;bindings: just return a LET-like form.
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
    (cond ((and (= (length b*) 1)
		(pair? body)
		(or (eq? (car body) 'let*)
		    (and (eq? (car body) 'let)
			 (= (length (cadr body)) 1))))
	   (list 'let* (append b* (cadr body)) (caddr body)))
	  (else
	   (list 'let b* body))))

  (define (%pretty-symbol sym)
    (string->symbol (symbol->string sym)))

  #| end of module: unparse-recordized-code/pretty |# )


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'assemble-sources			'scheme-indent-function 1)
;; eval: (put 'define-structure			'scheme-indent-function 1)
;; eval: (put 'make-conditional			'scheme-indent-function 2)
;; eval: (put 'struct-case			'scheme-indent-function 1)
;; eval: (put '$map/stx				'scheme-indent-function 1)
;; eval: (put '$for-each/stx			'scheme-indent-function 1)
;; eval: (put '$fold-right/stx			'scheme-indent-function 1)
;; eval: (put 'with-prelex-structs-in-plists	'scheme-indent-function 1)
;; eval: (put 'compile-time-error		'scheme-indent-function 1)
;; eval: (put 'compiler-internal-error		'scheme-indent-function 1)
;; End:
