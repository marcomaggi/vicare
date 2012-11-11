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
    compile-core-expr-to-port		assembler-output
    optimize-cp				optimizer-output
    current-primitive-locations
    eval-core				current-core-eval
    compile-core-expr
    cp0-effort-limit			cp0-size-limit
    optimize-level
    perform-tag-analysis		tag-analysis-output
    strip-source-info			generate-debug-calls
    current-letrec-pass

    ;; these go in (ikarus system $compiler)
    (rename
     (current-letrec-pass			$current-letrec-pass)
     (check-for-illegal-letrec			$check-for-illegal-letrec)
     (source-optimizer-passes-count		$source-optimizer-passes-count)

     (compile-core-expr->code			$compile-core-expr->code)
     (recordize					$recordize)
     (optimize-direct-calls			$optimize-direct-calls)
     (optimize-letrec				$optimize-letrec)
     (source-optimize				$source-optimize)
     (rewrite-references-and-assignments	$rewrite-references-and-assignments)
     (introduce-tags				$introduce-tags)
     (introduce-vars				$introduce-vars)
     (sanitize-bindings				$sanitize-bindings)
     (optimize-for-direct-jumps			$optimize-for-direct-jumps)
     (insert-global-assignments			$insert-global-assignments)
     (convert-closures				$convert-closures)
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
     (unparse-recordized-code/pretty		$unparse-recordized-code/pretty)))
  (import (except (ikarus)
		  compile-core-expr-to-port		assembler-output
		  optimize-cp				optimizer-output
		  current-primitive-locations
		  eval-core
		  optimize-level
		  cp0-size-limit			cp0-effort-limit
		  tag-analysis-output			perform-tag-analysis
		  current-core-eval			current-letrec-pass
		  bind)
    ;;Remember that this file defines the primitive operations.
    (ikarus system $fx)
    (ikarus system $pairs)
    (only (ikarus system $codes)
	  $code->closure)
    (only (ikarus system $structs)
	  $struct-ref $struct/rtd?)
    (vicare include)
    (ikarus.intel-assembler)
    (except (vicare syntactic-extensions)
	    begin0)
    (vicare arguments validation)
    (for (prefix (only (vicare installation-configuration)
		       wordsize)
		 config.)
	 run expand))


;;;; configuration parameters

(define generate-debug-calls
  ;;Set to true when the option "--debug" is used on the command line of
  ;;the executable "vicare"; else set to #f.
  ;;
  (make-parameter #f))

(define strip-source-info
  (make-parameter #f))

(define open-mvcalls
  ;;When  set to  true: an  attempt is  made to  expand inline  calls to
  ;;CALL-WITH-VALUES by inserting local bindings.
  ;;
  (make-parameter #f))

(define optimize-cp
  (make-parameter #t))

(define optimizer-output
  (make-parameter #f))

(define perform-tag-analysis
  (make-parameter #t))

(define assembler-output
  (make-parameter #f))


;;;; helper syntaxes

(define-inline (%debug-print ?obj)
  (pretty-print ?obj (current-error-port)))

(define-inline (%debug-write ?obj)
  (begin
    (write ?obj (current-error-port))
    (newline (current-error-port))))

;;; --------------------------------------------------------------------

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

;;; --------------------------------------------------------------------

(define-syntax $map/stx
  ;;Like MAP, but  expand the loop inline.  The "function"  to be mapped
  ;;must be specified by an identifier.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?func ?ell0 ?ell ...)
       (identifier? #'?func)
       (with-syntax (((T ...) (generate-temporaries #'(?ell ...))))
	 #'(let recur ((t ?ell0) (T ?ell) ...)
	     (if (null? t)
		 '()
	       ;;MAP does  not specify the  order in which the  ?FUNC is
	       ;;applied to the items.
	       (cons (?func ($car t) ($car T) ...)
		     (recur ($cdr t) ($cdr T) ...))))))
      )))

(define-syntax $for-each/stx
  ;;Like FOR-HEACH,  but expand the  loop inline.  The "function"  to be
  ;;mapped must be specified by an identifier.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?func ?ell0 ?ell ...)
       (identifier? #'?func)
       (with-syntax (((T ...) (generate-temporaries #'(?ell ...))))
	 #'(let loop ((t ?ell0) (T ?ell) ...)
	     (unless (null? t)
	       (?func ($car t) ($car T) ...)
	       (loop  ($cdr t) ($cdr T) ...)))))
      )))

(define-syntax begin0
  ;;A version of BEGIN0 usable only with single values.
  ;;
  (syntax-rules ()
    ((_ ?form ?body0 ?body ...)
     (let ((t ?form))
       ?body0 ?body ...
       t))))

;;; --------------------------------------------------------------------

(define-syntax struct-case
  ;;Specialised CASE syntax  for data structures.  Notice  that we could
  ;;use this  syntax for any  set of struct  types, not only  the struct
  ;;types defined in this library.
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
  ;;notice that: in the clauses the  pattern "(alpha a b)" must list the
  ;;fields A and B in the same  order in which they appear in the struct
  ;;type definition.
  ;;
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ?expr ?clause ...)
	 (with-syntax ((BODY (%generate-body #'_ #'(?clause ...))))
	   #'(let ((v ?expr))
	       BODY)))))

    (define (%generate-body ctxt clauses-stx)
      (syntax-case clauses-stx (else)
        (()
	 (with-syntax ((INPUT-FORM stx))
	   #'(error 'compiler "unknown struct type" v 'INPUT-FORM)))

        (((else ?body0 ?body ...))
	 #'(let () ?body0 ?body ...))

        ((((?struct-name ?field-name ...) ?body0 ?body ...) . ?other-clauses)
	 (identifier? #'?struct-name)
         (with-syntax ((RTD		#'(type-descriptor ?struct-name))
                       ((FIELD-IDX ...)	(%enumerate #'(?field-name ...) 0))
		       (ALTERN		(%generate-body ctxt #'?other-clauses)))
	   #'(if ($struct/rtd? v RTD)
		 (let ((?field-name ($struct-ref v FIELD-IDX))
		       ...)
		   ?body0 ?body ...)
	       ALTERN)))))

    (define (%enumerate fields-stx next-field-idx)
      ;;FIELDS-STX must be a syntax object holding a list of identifiers
      ;;being  struct field  names.   NEXT-FIELD-IDX must  be a  fixnums
      ;;representing the index of the first field in FIELDS-STX.
      ;;
      ;;Return  a syntax  object holding  a  list of  fixnums being  the
      ;;indexes of the fields in FIELDS-STX.
      ;;
      (syntax-case fields-stx ()
        (() #'())
        ((?field-name . ?other-names)
         (with-syntax
	     ((FIELD-IDX        next-field-idx)
	      (OTHER-FIELD-IDXS (%enumerate #'?other-names (fxadd1 next-field-idx))))
           #'(FIELD-IDX . OTHER-FIELD-IDXS)))))

    (main stx)))

(define-syntax define-structure
  ;;A syntax to define struct  types for compatibility with the notation
  ;;used in Oscar  Waddell's thesis; it allows the  definition of struct
  ;;types in which some of the  fields are initialised by the maker with
  ;;default values,  while other  fields are initialised  with arguments
  ;;handed to the maker.
  ;;
  ;;Synopsis:
  ;;
  ;;  (define-structure (?name ?field-without ...)
  ;;    ((?field-with ?default)
  ;;	 ...))
  ;;
  ;;where: ?NAME is the struct  type name, ?FIELD-WITHOUT are identifier
  ;;names  for the  fields without  default, ?FIELD-WITH  are identifier
  ;;names for the fields with default, ?DEFAULT are the default values.
  ;;
  ;;The  maker accepts  a number  of arguments  equal to  the number  of
  ;;?FIELD-WITHOUT, in the same order in which they appear in the struct
  ;;definition.
  ;;
  ;;(It is a bit ugly...  Marco Maggi; Oct 10, 2012)
  ;;
  (lambda (stx)
    (define (%format-id ctxt template-str . args)
      (datum->syntax ctxt (string->symbol
			   (apply format template-str (map syntax->datum args)))))
    (syntax-case stx ()
      ((_ (?name ?field ...))
       #'(define-struct ?name (?field ...)))

      ((_ (?name ?field-without-default ...)
	  ((?field-with-default ?default)
	   ...))
       (with-syntax
	   ((PRED		(%format-id #'?name "~s?" #'?name))
	    (MAKER		(%format-id #'?name "make-~s" #'?name))
	    ((GETTER ...)	(map (lambda (x)
				       (%format-id #'?name "~s-~s" #'?name x))
				  #'(?field-without-default ... ?field-with-default ...)))
	    ((UNSAFE-GETTER ...)(map (lambda (x)
				       (%format-id #'?name "$~s-~s" #'?name x))
				  #'(?field-without-default ... ?field-with-default ...)))
	    ((SETTER ...)	(map (lambda (x)
				       (%format-id #'?name "set-~s-~s!" #'?name x))
				  #'(?field-without-default ... ?field-with-default ...)))
	    ((UNSAFE-SETTER ...)(map (lambda (x)
				       (%format-id #'?name "$set-~s-~s!" #'?name x))
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
               (import private)))))
      )))


;;;; helper functions

(define (remq1 x ls)
  ;;Scan the  list LS and  remove only the  first instance of  object X,
  ;;using EQ?  as  comparison function; return the  resulting list which
  ;;may share its tail with LS.
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
  ;;Return a list which  is the union between the lists  S1 and S2, with
  ;;all the duplicates removed.
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
  ;;Return a list holding all the  elements from the list S1 not present
  ;;in the list S2.
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


(define current-core-eval
  (make-parameter (lambda (x)
		    ((compile-core-expr x)))
    (lambda (x)
      (define who 'current-core-eval)
      (with-arguments-validation (who)
	  ((procedure	x))
	x))))

(define (eval-core x)
  ;;This  function is  used  to compile  fully  expanded R6RS  programs,
  ;;invoke libraries, implement R6RS's eval function, compile right-hand
  ;;sides of syntax definitions.
  ;;
  ((current-core-eval) x))

(define current-primitive-locations
  (let ((plocs (lambda (x) #f)))
    (case-lambda
     (()
      plocs)
     ((p)
      (define who 'current-primitive-locations)
      (with-arguments-validation (who)
	  ((procedure	p))
	(set! plocs p)
	(refresh-cached-labels!))))))


(module (compile-core-expr-to-port
	 compile-core-expr
	 compile-core-expr->code)
  ;;The list of compiler passes is:
  ;;
  ;;   recordize
  ;;   optimize-direct-calls
  ;;   optimize-letrec
  ;;   source-optimize
  ;;   rewrite-references-and-assignments
  ;;   introduce-tags (optional)
  ;;   introduce-vars
  ;;   sanitize-bindings
  ;;   optimize-for-direct-jumps
  ;;   insert-global-assignments
  ;;   convert-closures
  ;;   optimize-closures/lift-codes
  ;;

  (define (compile-core-expr-to-port expr port)
    ;;This function is used to write binary code into the boot image.
    ;;
    (fasl-write (compile-core-expr->code expr) port))

  (define (compile-core-expr x)
    ;;This  function  is used  to  compile  libraries' source  code  for
    ;;serialisation into FASL files.
    ;;
    (let ((code (compile-core-expr->code x)))
      ($code->closure code)))

  (define who 'compile-core-expr->code)

  (define (compile-core-expr->code p)
    (let* ((p (recordize p))
	   (p (parameterize ((open-mvcalls #f))
		(optimize-direct-calls p)))
	   (p (optimize-letrec p))
	   (p (source-optimize p)))
      (when (optimizer-output)
	(pretty-print (unparse-recordized-code/pretty p) (current-error-port)))
      (let* ((p (rewrite-references-and-assignments p))
	     (p (if (perform-tag-analysis)
		    (introduce-tags p)
		  p))
	     (p (introduce-vars p))
	     (p (sanitize-bindings p))
	     (p (optimize-for-direct-jumps p))
	     (p (insert-global-assignments p))
	     (p (convert-closures p))
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

  (define (thunk?-label x)
    (if (closure? x)
	(if (null? (closure-free* x))
	    (code-loc-label (closure-code x))
	  (error who "BUG: non-thunk escaped" x))
      #f))

  (define (print-instr x)
    ;;Print  to   the  current   error  port  the   symbolic  expression
    ;;representing  the  assembly instruction  X.   To  be used  to  log
    ;;generated assembly for human inspection.
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
;;The struct  types defined in this  code page are used  by the function
;;RECORDIZE to  represent code in  the core language  in a way  which is
;;better inspectable and optimizable.
;;

;;Instances of  this type are  stored in  the property lists  of symbols
;;representing  binding names;  this way  we  can just  send around  the
;;binding name symbol to represent some lexical context informations.
;;
(define-structure
    (prelex
     name
		;A  symbol representing  the binding  name; in  practice
		;useful only for humans when debugging.
     operand
		;Multipurpose field.  One use: false or a struct of type
		;VAR associated to this instance.
     )
  ((source-referenced?   #f)
		;Boolean, true when the region  in which this binding is
		;defined  in   the  source  code  (before   source  code
		;optimization) has at least one reference to it.
		;
		;See also the field RESIDUAL-REFERENCED?.
   (source-assigned?     #f)
		;Boolean or symbol.
		;
		;When a boolean: true when  the binding has been used as
		;left-hand side in a SET! form.
		;
		;When a symbol: who knows?  (Marco Maggi; Oct 12, 2012)
   (residual-referenced? #f)
		;Boolean,  this  field  is   used  only  by  the  source
		;optimizer.   When optimizing  source  code, an  attempt
		;will  be  made to  substitute  every  reference to  the
		;binding this struct represents with an expansion of the
		;referenced value.
		;
		;For example, an attempt will be made to simplify:
		;
		;   (let ((x 1) (y 2)) (list x y))
		;   ==> (let ((x 1) (y 2)) (list 1 2))
		;
		;When such optimization attempts  fail, the reference to
		;variable is just left in place and this field is set to
		;true.   So   the  meaning  of  this   field  is:  still
		;referenced after optimization attempt.
   (residual-assigned?   #f)
		;Boolean,  this  field  is   used  only  by  the  source
		;optimizer.   When optimizing  source  code, an  attempt
		;will be made to remove  every assignment to the binding
		;this struct represents if the new value is never used.
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
		;When such optimization attempts fail, the assignment to
		;variable is just left in place and this field is set to
		;true.  So the meaning of  this field is: still assigned
		;after optimization attempt.
   (global-location      #f)
		;When this  binding describes a top  level binding, this
		;field  is set  to  a unique  gensym  associated to  the
		;binding; else this field is #f.
		;
		;The value of a top level binding is stored in the VALUE
		;field of a gensym memory block.  Such field is accessed
		;with the  operation $SYMBOL-VALUE and mutated  with the
		;operation $SET-SYMBOL-VALUE!.
   ))

;;; --------------------------------------------------------------------

;;Instances of this type represent datums in the core language.
;;
(define-struct constant
  (value
		;The  datum  from  the  source,  for  example:  numbers,
		;vectors, strings, quoted lists...
   ))

;;An instance  of this  type represents  a form in  a sequence  of forms
;;inside a  core language BEGIN.
;;
;;*NOTE* We  want the SEQ structure  to be built everywhere  with nested
;;structures in  the first field and  the last expression in  the second
;;field; this allows easy extraction of  the last form, which is useful,
;;for example, in the source optimizer.
;;
;;We can think of the form:
;;
;;   (begin ?b0 ?b1 ?last)
;;
;;as:
;;
;;   (begin (begin ?b0 ?b1) ?last)
;;
;;and it becomes the nested hierarchy:
;;
;;   (make-seq (make-eq (recordize ?b0) (recordize ?b1))
;;             (recordize ?last))
;;
(define-struct seq
  (e0
		;A struct  instance representing  the first form  in the
		;sequence.  It can be a nested SEQ struct.
   e1
		;A  struct instance  representing the  last form  in the
		;sequence.  It *should not* be a SEQ struct.
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

;;An instance of this type represents a SET! form in which the left-hand
;;side references a lexical binding  represented by a struct instance of
;;type PRELEX.
;;
(define-struct assign
  (lhs
		;A  struct  instance  of type  PRELEX  representing  the
		;binding.
   rhs
		;A struct instance representing the new binding value.
   ))

;;An instance of this type represents a function call; there are special
;;cases of this:
;;
;;**When this FUNCALL represents a plain function application:
;;
;;  - the field OP is set to  a struct instance representing a form that
;;    supposedly evaluates to a closure;
;;
;;  - the field RAND* is set  to a list of struct instances representing
;;    forms that evaluate to the arguments.
;;
;;**When this  FUNCALL represents an annotated  function application and
;;  debugging mode is active:
;;
;;  - the field OP is a  struct instance of type PRIMREF referencing the
;;    primitive DEBUG-CALL;
;;
;;  - the field RAND* is a list in which:
;;
;;    + the 1st  item is a struct instance of  type CONSTANT whose field
;;      holds a pair: its car is #f or the source input port identifier;
;;      its cdr is the source epxression;
;;
;;    + the  2nd  item  a  struct  instance  representing  a  form  that
;;      supposedly evaluates to a closure;
;;
;;    + the tail is  a list of  of struct  instances  representing forms
;;      that evaluate to the arguments.
;;
;;  Notice that a call to DEBUG-CALL looks like this:
;;
;;     (debug-call ?src/expr ?rator ?arg ...)
;;
;;**When the FUNCALL represents a SET!  on a top level binding:
;;
;;  -  the field  OP is a  struct instance of  type PRIMREF  holding the
;;    symbol "$init-symbol-value!".
;;
;;  - the field RAND* is a list of 2 elements: a struct instance of type
;;    CONSTANT holding the symbol name of the binding; a struct instance
;;    representing the new value.
;;
(define-struct funcall
  (op rand*))

;;An instance of this type represents a LETREC form.
;;
(define-struct recbind
  (lhs*
		;A list  of struct  instances of type  PRELEX describing
		;the bindings.
   rhs*
		;A list of struct  instances representing the right-hand
		;sides of the bindings.
   body
		;A  struct instance  representing the  sequence of  body
		;forms.
   ))

;;An instance of this type represents a LETREC* or LIBRARY-LETREC* form.
;;
;;The difference  between a struct  representing a LETREC* and  a struct
;;representing a LIBRARY-LETREC* form is only in the PRELEX structures.
;;
(define-struct rec*bind
  (lhs*
		;A list  of struct  instances of type  PRELEX describing
		;the bindings.
   rhs*
		;A list of struct  instances representing the right-hand
		;sides of the bindings.
   body
		;A  struct instance  representing the  sequence of  body
		;forms.
   ))

;;An  instance  of this  type  represents  a  reference to  a  primitive
;;function.
;;
(define-struct primref
  (name
		;A symbol being the name of the primitive.
   ))

;;An instance  of this  type represents  a call  to a  foreign function,
;;usually a C language function.
;;
(define-struct forcall
  (op
		;A string representing the name of the foreign function.
   rand*
		;A list of struct instances representing the arguments.
   ))

;;An  instance of  this type  represents a  LAMBDA or  CASE-LAMBDA form.
;;Such forms have the syntax:
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
		;A unique gensym associated to this closure.
   cases
		;A  list  of  struct   instances  of  type  CLAMBDA-CASE
		;representing the clauses.
   cp
		;Initialised to #f, it is  set to the struct instance of
		;type VAR to which the  CLOSURE wrapping this CLAMBDA is
		;bound.
   free
		;Initialised  to #f,  it  is  set to  a  list of  struct
		;instances of  type VAR representing the  free variables
		;referenced by this CLAMBDA.
   name
		;An annotation  representing the name of  the closure if
		;available.  It  can be:
		;
		;* #f when no information is available.
		;
		;* A symbol representing the closure name itself.
		;
		;* A  pair whose car  is #f  or a name  representing the
		;closure name itself, and whose cdr is #f or the content
		;of the SOURCE field in an ANNOTATION struct.
   ))

;;An instance  of this  type represents a  CASE-LAMBDA clause.   Given a
;;symbolic expression representing a CASE-LAMBDA:
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
;;holding informations needed to select it among the multiple choices of
;;the original form.
;;
(define-struct clambda-case
  (info
		;A struct instance of type CASE-INFO.
   body
		;A struct  instance representing  the sequence  of ?BODY
		;forms.
   ))

;;An instance of this type  represents easily accessible informations on
;;the formals of a single clause of CASE-LAMBDA form:
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
;;**If ?FORMALS is null or a proper list: ARGS is null or a proper list,
;;  PROPER is #t.
;;
;;**If ?FORMALS  is a  symbol: ARGS  is a proper  list holding  a single
;;  item, PROPER is #f.
;;
;;**If  ?FORMALS is  an improper  list: ARGS  is a  proper list  holding
;;  multiple elements, PROPER is #f.
;;
(define-struct case-info
  (label
		;A  unique gensym  for this  CASE-LAMBDA clause.   It is
		;used to  generate, when possible, direct  jumps to this
		;clause rather than calling the whole closure.
   args
		;In  the  earlier  compiler  passes: a  list  of  struct
		;instances of  type PRELEX representing the  ?FORMALS as
		;follows:
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
		;In  the  latest  compiler  passes:  a  list  of  struct
		;instances of type VAR representing the formals.  In the
		;very latest passes.
   proper
		;A boolean: true if ?FORMALS  is a proper list, false if
		;?FORMALS is a symbol or improper list.
   ))


;;;; struct types

;;Instances of this  struct type represent bindings at  the lowest level
;;in recordized code.
;;
;;A BIND is  a set of bindings  in which the order of  evaluation of the
;;right-hand sides can be freely changed; it is like a LET syntax.
;;
(define-struct bind
  (lhs*
		;When code is first recordized  and optimized: a list of
		;struct  instances  of   type  PRELEX  representing  the
		;binding names.   Later: a  list of struct  instances of
		;type VAR representing some kind of memory location.
   rhs*
		;A list of struct instances representing recordized code
		;which,  when  evaluated,   will  return  the  binding's
		;values.
   body
		;A struct  instance representing  recordized code  to be
		;evaluated in the region of the bindings.
   ))

;;Like BIND, but  the RHS* field holds struct instances  of type CLAMBDA
;;and the  LHS* field  holds locations  that are  never assigned  by the
;;BODY.
;;
;;For details  on the meaning  of FIX, see  the paper (available  on the
;;Net):
;;
;;   Oscar Waddell, Dipanwita Sarkar, R. Kent Dybvig.  "Fixing Letrec: A
;;   Faithful Yet Efficient Implementation of Scheme's Recursive Binding
;;   Construct"
;;
(define-struct fix
  (lhs*
		;A list of locations in which references to CLAMBDA must
		;be stored.
   rhs*
		;A list of CLAMBDA structures.
   body
		;A struct instance representing recordized code.
   ))

;;Instances of this struct type represent variables in recordized code.
;;
(define-struct var
   (name
    reg-conf
    frm-conf
    var-conf
    reg-move
    frm-move
    var-move
    loc
    index
    referenced
    global-loc
    ))

;;Instances of this  struct type are substitute instances  of FUNCALL in
;;recordized code  when it  is possible  to perform a  direct jump  to a
;;CASE-LAMBDA clause, rather than call the whole closure.
;;
(define-struct jmpcall
  (label
		;A gensym, it is the gensym stored in the LABEL field of
		;the target CASE-INFO struct.
   op
		;A  struct instance  representing  the  operator of  the
		;closure application.
   rand*
		;A list  of struct instances representing  the arguments
		;of the closure application.
   ))

;;Instances of this type represent closures.
;;
;;NOTE We  must not confuse  struct instances  of type CLOSURE,  used to
;;represent recordized code,  with instances of Scheme  built in objects
;;of type closure, which represent closures at run time.
;;
(define-struct closure
  (code
		;A  struct instance  of  type  CLAMBDA representing  the
		;closure's implementation, or a  struct instance of type
		;CODE-LOC.
   free*
		;A list of struct instances of type VAR representing the
		;free variables referenced by this CLOSURE.
   well-known?
   ))

;;Instances of this type represent function calls to primitives.
;;
(define-struct primcall
  (op
		;A symbol being the name of the primitive.
   arg*
		;A  list  of  struct instances  representing  recordized
		;expressions  which,  when  evaluated, will  return  the
		;arguments of this primitive call.
   ))

(define-struct codes
  (list
		;A list of struct instances of type CLAMBDA.
   body
		;A struct instance representing recordized code.
   ))

;;Instances of this type represent constant  objects to be hard coded in
;;compiled code, whose binary representation  is *not* an exact integer.
;;For  example: the  binary  representation  of the  boolean  #t is  the
;;integer #x3F, while a Scheme vector has a more complex representation.
;;
(define-struct object
  (val))

;;; --------------------------------------------------------------------

(define-struct code-loc
  (label))

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

(define-struct mvcall
  (producer
   consumer
   ))

(define-struct known
  (expr
   type
   ))

(define-struct shortcut
  (body
   handler
   ))

;;Represent  a machine  word  on  the Scheme  stack,  below the  address
;;referenced  by  the  Frame  Pointer  Register  (FPR).   The  index  is
;;interpreted as follows:
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
;;It is  used to represent memory  locations used as local  variables in
;;the execution of a Scheme function.
;;
(define-struct fvar
  (idx
		;A fixnum  representing the index  of a machine  word on
		;the Scheme stack.
   ))

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

(define-struct asm-instr
  (op
   dst
   src
   ))

(define-struct disp
  (s0
   s1
   ))


;;;; special struct makers

(define mkfvar
  ;;Maker function for structs of type FVAR.  It caches structures based
  ;;on the values  of the argument, so that calling:
  ;;
  ;;   (mkfvar 123)
  ;;
  ;;always returns the same FVAR instance holding 123.
  ;;
  ;;FIXME Should  a hashtable be used  as cache?  (Marco Maggi;  Oct 10,
  ;;2012)
  ;;
  (let ((cache '()))
    (lambda (i)
      (define who 'mkfvar)
      (with-arguments-validation (who)
	  ((fixnum	i))
	(cond ((assv i cache)
	       => cdr)
	      (else
	       (let ((fv (make-fvar i)))
		 (set! cache (cons (cons i fv) cache))
		 fv)))))))

(define (unique-var name)
  (make-var name #f #f #f #f #f #f #f #f #f #f))


(define (recordize x)
  ;;Given  a symbolic  expression  X  representing a  form  in the  core
  ;;language, convert  it into a  nested hierarchy of  struct instances;
  ;;return the outer struct instance.
  ;;
  ;;An expression in  the core language is code fully  expanded in which
  ;;all  the bindings  have a  unique variable  name; the  core language
  ;;would be evaluated  in an environment composed by  all the functions
  ;;exported by the boot image and the loaded libraries.
  ;;
  ;;This function expects a symbolic  expression with perfect syntax: no
  ;;syntax errors are  checked.  We expect this function  to be executed
  ;;without errors,  no exceptions should  be raised unless  an internal
  ;;bug makes it happen.
  ;;
  ;;Recognise the following core language:
  ;;
  ;;   (quote ?datum)
  ;;   (if ?test ?consequent ?alternate)
  ;;   (set! ?lhs ?rhs)
  ;;   (begin ?body0 ?body ...)
  ;;   (letrec  ((?lhs ?rhs) ...) ?body0 ?body ..)
  ;;   (letrec* ((?lhs ?rhs) ...) ?body0 ?body ..)
  ;;   (library-letrec* ((?lhs ?loc ?rhs) ...) ?body0 ?body ..)
  ;;   (case-lambda (?formals ?body0 ?body ...) ...)
  ;;   (annotated-case-lambda ?annotation (?formals ?body0 ?body ...) ...)
  ;;   (lambda ?formals ?body0 ?body ...)
  ;;   (foreign-call "?function-name" ?arg ...)
  ;;   (primitive ?prim)
  ;;   (annotated-call ?annotation ?fun ?arg ...)
  ;;   ?symbol
  ;;   (?func ?arg ...)
  ;;
  ;;where: ?SYMBOL is interpreted as  reference to variable; ?LHS stands
  ;;for "left-hand side"; ?RHS stands for "right-hand side".
  ;;
  ;;The bulk of the work is performed by the recursive function E.
  ;;
  ;;Whenever possible we  want closures to be annotated  with their name
  ;;in the original source code; example:
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
  ;;   ((lambda (x) x) (lambda (y) ;annotated: x
  ;;                y))
  ;;
  ;;this is what  the CLOSURE-NAME argument in the  subfunctions is for;
  ;;it is carefully handed to the  functions that process forms that may
  ;;evaluate to a closure and  finally used to annotate struct instances
  ;;of type CLAMBDA.
  ;;

  (define-syntax E
    (syntax-rules ()
      ((_ ?x)
       (%E ?x #f))
      ((_ ?x ?ctxt)
       (%E ?x ?ctxt))
      ))

  (define (%E X ctxt)
    ;;Convert the  symbolic expression X  representing code in  the core
    ;;language into a nested hierarchy of struct instances.
    ;;
    (cond ((pair? X)
	   (%recordize-pair-sexp X ctxt))

	  ((symbol? X)
	   (cond ((lexical X)
		  ;;It is a reference to local variable.
		  => (lambda (var)
		       (set-prelex-source-referenced?! var #t)
		       var))
		 (else
		  ;;It is a reference to top level variable.
		  (make-funcall (make-primref 'top-level-value)
				(list (make-constant X))))))

	  (else
	   (error 'recordize "invalid core language expression" X))))

  (define-inline (%recordize-pair-sexp X ctxt)
    (case-symbols ($car X)

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
      ;;If the  left-hand side  references a  lexical binding:  return a
      ;;struct  instance   of  type  ASSIGN.   If   the  left-hand  side
      ;;references a top level binding: return a struct instance of type
      ;;FUNCALL.
      ;;
      ((set!)
       (let ((lhs ($cadr  X))  ;left-hand side
	     (rhs ($caddr X))) ;right-hand side
	 (cond ((lexical lhs)
		=> (lambda (var)
		     (set-prelex-source-assigned?! var #t)
		     (make-assign var (E rhs lhs))))
	       (else
		;;We recordize the right-hand size in the context
		;;of LHS.
		(make-global-set! lhs (E rhs lhs))))))

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

      ;;Synopsis: (letrec ((?lhs ?rhs) ...) ?body0 ?body ..)
      ;;
      ;;Return a struct instance of type RECBIND.
      ;;
      ((letrec)
       (let ((bind* ($cadr  X))		     ;list of bindings
	     (body  ($caddr X)))	     ;list of body forms
	 (let ((lhs* ($map/stx $car  bind*)) ;list of bindings left-hand sides
	       (rhs* ($map/stx $cadr bind*))) ;list of bindings right-hand sides
	   ;;Make sure that LHS* is processed first!!!
	   (let* ((lhs*^ (gen-fml* lhs*))
		  (rhs*^ ($map/stx E rhs* lhs*))
		  (body^ (E body ctxt)))
	     (begin0
		 (make-recbind lhs*^ rhs*^ body^)
	       (ungen-fml* lhs*))))))

      ;;Synopsis: (letrec* ((?lhs ?rhs) ...) ?body0 ?body ..)
      ;;
      ;;Return a struct instance of type REC*BIND.
      ;;
      ((letrec*)
       (let ((bind* ($cadr X))		     ;list of bindings
	     (body  ($caddr X)))	     ;list of body forms
	 (let ((lhs* ($map/stx $car  bind*)) ;list of bindings left-hand sides
	       (rhs* ($map/stx $cadr bind*))) ;list of bindings right-hand sides
	   ;;Make sure that LHS* is processed first!!!
	   (let* ((lhs*^ (gen-fml* lhs*))
		  (rhs*^ ($map/stx E rhs* lhs*))
		  (body^ (E body ctxt)))
	     (begin0
		 (make-rec*bind lhs*^ rhs*^ body^)
	       (ungen-fml* lhs*))))))

      ;;Synopsis: (library-letrec* ((?lhs ?loc ?rhs) ...) ?body0 ?body ..)
      ;;
      ;;Notice that a LIBRARY form like:
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
      ;;   (library-letrec* ((?lhs ?loc ?rhs) ...) ?expr ...)
      ;;
      ;;where:
      ;;
      ;;* ?LHS is a gensym representing the name of the binding;
      ;;
      ;;* ?LOC is a gensym used to hold the value of the binding (in the
      ;;  VALUE field of the sybmol memory block);
      ;;
      ;;* ?RHS is a symbolic expression which evaluates to the binding's
      ;;  value.
      ;;
      ;;Return a struct instance of type REC*BIND.
      ;;
      ((library-letrec*)
       (let ((bind* ($cadr  X))		      ;list of bindings
	     (body  ($caddr X)))	      ;list of body forms
	 (let ((lhs* ($map/stx $car   bind*)) ;list of bindings left-hand sides
	       (loc* ($map/stx $cadr  bind*)) ;list of unique gensyms
	       (rhs* ($map/stx $caddr bind*))) ;list of bindings right-hand sides
	   ;;Make sure that LHS* is processed first!!!
	   (let* ((lhs*^ (let ((lhs*^ (gen-fml* lhs*)))
			   ($for-each/stx set-prelex-global-location! lhs*^ loc*)
			   lhs*^))
		  (rhs*^ ($map/stx E rhs* lhs*))
		  (body^ (E body ctxt)))
	     (begin0
		 (make-rec*bind lhs*^ rhs*^ body^)
	       (ungen-fml* lhs*))))))

      ;;Synopsis: (case-lambda (?formals ?body0 ?body ...) ...)
      ;;
      ;;Return a struct instance of type CLAMBDA.
      ;;
      ((case-lambda)
       (let ((clause* (E-clambda-clause* ($cdr X) ctxt)))
	 (make-clambda (gensym) clause* #f #f
		       (and (symbol? ctxt) ctxt))))

      ;;Synopsis: (annotated-case-lambda ?annotation (?formals ?body0 ?body ...))
      ;;
      ;;Return a struct instance of type CLAMBDA.
      ;;
      ((annotated-case-lambda)
       (let ((annotated-expr ($cadr X))
	     (clause*        (E-clambda-clause* ($cddr X) ctxt)))
	 (make-clambda (gensym) clause* #f #f
		       (cons (and (symbol? ctxt) ctxt)
			     ;;This  annotation  is excluded  only  when
			     ;;building the boot image.
			     (and (not (strip-source-info))
				  (annotation? annotated-expr)
				  (annotation-source annotated-expr))))))

      ;;Synopsis: (lambda ?formals ?body0 ?body ...)
      ;;
      ;;LAMBDA  functions   are  handled  as  special   cases  of
      ;;CASE-LAMBDA functions.
      ;;
      ;;   (lambda ?formals ?body0 ?body ...)
      ;;   ===> (case-lambda (?formals ?body0 ?body ...))
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
      ;;Return a struct instance of type PRIMREF.
      ;;
      ((primitive)
       (let ((var ($cadr X)))
	 (make-primref var)))

      ;;Synopsis: (annotated-call ?annotation ?fun ?arg ...)
      ;;
      ;;Return a struct instance of type FUNCALL.
      ;;
      ((annotated-call)
       (E-annotated-call X ctxt))

      (else	;if X is a pair here, it is a function call
       ;;Synopsis: (?func ?arg ...)
       ;;
       ;;Return a struct instance of type FUNCALL.
       ;;
       (let ((func ($car X))
	     (args ($cdr X)))
	 (E-app make-funcall func args ctxt)))))

  (module (quoted-sym)

    (define-argument-validation (quoted-sym who obj)
      (and (list? obj)
	   ($fx= (length obj) 2)
	   (eq? 'quote ($car obj))
	   (symbol? ($cadr obj)))
      (assertion-violation who "expected quoted symbol sexp as argument" obj))

    (define (quoted-sym x)
      ;;Check that X has the format:
      ;;
      ;;  (quote ?symbol)
      ;;
      ;;and return ?SYMBOL.
      ;;
      (define who 'quoted-sym)
      (with-arguments-validation (who)
	  ((quoted-sym	x))
	($cadr x)))

    #| end of module: quoted-sym |# )

  (module (quoted-string)

    (define-argument-validation (quoted-string who obj)
      ;;Check that X has the format:
      ;;
      ;;  (quote ?string)
      ;;
      (and (list? obj)
	   ($fx= (length obj) 2)
	   (eq? 'quote ($car obj))
	   (string? ($cadr obj)))
      (error who "expected quoted string sexp as argument" obj))

    (define (quoted-string x)
      ;;Check that X has the format:
      ;;
      ;;  (quote ?string)
      ;;
      ;;and return ?string.
      ;;
      (define who 'quoted-string)
      (with-arguments-validation (who)
	  ((quoted-string	x))
	($cadr x)))

    #| end of module: quoted-string |# )

  (define (make-global-set! lhs recordised-rhs)
    ;;Return a new  struct instance of type FUNCALL  representing a SET!
    ;;for a variable at the top level. (?)
    ;;
    (make-funcall (make-primref '$init-symbol-value!)
		  (list (make-constant lhs) recordised-rhs)))

  (module (E-clambda-clause*)

    (define (E-clambda-clause* clause* ctxt)
      ;;Given a symbolic expression representing a CASE-LAMBDA:
      ;;
      ;;   (case-lambda (?formals ?body0 ?body ...) ...)
      ;;
      ;;and knowing that a LAMBDA sexp is converted to CASE-LAMBDA, this
      ;;function is called with CLAUSE* set to the list of clauses:
      ;;
      ;;   ((?formals ?body0 ?body ...) ...)
      ;;
      ;;Return a list holding new struct instances of type CLAMBDA-CASE,
      ;;one for each clause.
      ;;
      (map (let ((ctxt (and (pair? ctxt) ($car ctxt))))
	     (lambda (clause)
	       (let ((fml* ($car  clause))  ;the formals
		     (body ($cadr clause))) ;the body sequence
		 ;;Make sure that FML* is processed first!!!
		 (let* ((fml*^ (gen-fml* fml*))
			(body^ (E body ctxt))
			;;True if FML* is a  proper list; false if it is
			;;a symbol or improper list.
			(proper? (list? fml*)))
		   (ungen-fml* fml*)
		   (make-clambda-case (make-case-info (gensym) (properize fml*^) proper?)
				      body^)))))
	clause*))

    (define (properize fml*)
      ;;If FML*  is a proper  list: return a  new list holding  the same
      ;;values.
      ;;
      ;;If FML*  is an improper list:  return a new proper  list holding
      ;;the same values:
      ;;
      ;;   (properize '(1 2 . 3)) => (1 2 3)
      ;;
      ;;If FML* is not a list: return a list wrapping it:
      ;;
      ;;   (properize 123) => (123)
      ;;
      (cond ((pair? fml*)
	     (cons ($car fml*)
		   (properize ($cdr fml*))))
	    ((null? fml*)
	     '())
	    (else
	     (list fml*))))

    #| end of module: E-clambda-clause* |# )

  (module (E-annotated-call)

    (define (E-annotated-call X ctxt)
      ;;We expect X to be the symbolic expression:
      ;;
      ;;   (annotated-call ?annotation ?fun ?arg ...)
      ;;
      ;;where  ?ANNOTATION  is a  struct  instance  of type  ANNOTATION,
      ;;defined by the reader.
      ;;
      ;;At present  (Oct 11, 2012), this  function is the only  place in
      ;;the    compiler    that    makes   use    of    the    parameter
      ;;GENERATE-DEBUG-CALLS.
      ;;
      (let ((anno ($cadr  X))  ;annotation
	    (func ($caddr X))  ;expression evaluating to the function
	    (args ($cdddr X))) ;arguments
	(E-app (if (generate-debug-calls)
		   (%make-funcall-maker anno)
		 make-funcall)
	       func args ctxt)))

    (define (%make-funcall-maker anno)
      (let ((src/expr (make-constant (if (annotation? anno)
					 (cons (annotation-source   anno)
					       (annotation-stripped anno))
				       (cons #f (syntax->datum anno))))))
	(lambda (op rands)
	  ;;Only non-operators get special  handling when debugging mode
	  ;;is active.
	  ;;
	  (if (%operator? op)
	      (make-funcall op rands)
	    (make-funcall (make-primref 'debug-call) (cons* src/expr op rands))))))

    (define (%operator? op)
      ;;Evaluate to true if OP references a primitive operation exported
      ;;by the boot image.
      ;;
      ;;The  SYSTEM-VALUE  call  below   will  fail  with  an  assertion
      ;;violation  if NAME  is not  a  symbol associated  to a  function
      ;;exported by the boot image.
      ;;
      ;;See the documentation of SYSTEM-VALUE for more details.
      ;;
      (struct-case op
	((primref name)
	 (guard (C ((assertion-violation? C)
		    #t))
	   (system-value name)
	   #f))
	(else #f)))

    #| end of module: E-annotated-call |# )

  (module (E-app)

    (define (E-app mk-call rator args ctxt)
      ;;Process a  form representing a  function call.  Return  a struct
      ;;instance of type FUNCALL.
      ;;
      ;;MK-CALL is either MAKE-FUNCALL or a wrapper for it.
      ;;
      ;;When the function call form is:
      ;;
      ;;   (?func ?arg ...)
      ;;
      ;;the argument RATOR is ?FUNC and the argument ARGS is (?ARG ...).
      ;;
      ;;In case RATOR is itself a LAMBDA or CASE-LAMBDA form as in:
      ;;
      ;;   ((lambda (x) x) 123)
      ;;
      ;;and one of the ARGS evaluates to a closure as in:
      ;;
      ;;   ((lambda (x) x) (lambda (y) ;annotated: x
      ;;                y))
      ;;
      ;;we  want   the  argument  LAMBDA   to  be  annotated   with  the
      ;;corresponding  formal name;  most of  the times  the list  NAMES
      ;;below will be null.
      ;;
      (if (equal? rator '(primitive make-parameter))
	  (E-make-parameter mk-call args ctxt)
	(let ((op    (E rator (list ctxt)))
	      (rand* (let ((names (get-fmls rator args)))
		       (if (null? names)
			   ($map/stx E args)
			 (let recur ((args  args)
				     (names names))
			   (if (pair? names)
			       (cons (E     ($car args) ($car names))
				     (recur ($cdr args) ($cdr names)))
			     ($map/stx E args)))))))
	  (mk-call op rand*))))

    (define (E-make-parameter mk-call args ctxt)
      (case-fixnums (length args)
	((1)	;MAKE-PARAMETER called with one argument.
	 (let ((val-expr	(car args))
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
	 (let ((val-expr	(car args))
	       (guard-expr	(cadr args))
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
		    ;;The one below is the original Ikarus implementation;
		    ;;it was  applying the  guard function every  time and
		    ;;also applying  the guard function to  the init value
		    ;;(Marco Maggi; Feb 3, 2012).
		    ;;
		    ;; ((case-lambda
		    ;;   ((,t0)
		    ;;    (case-lambda
		    ;;     (() ,t0)
		    ;;     ((,x) (set! ,t0 (,f ,x))))
		    ;;    (,f ,t))))
		    ;;
		    ((primitive die) 'make-parameter '"not a procedure" ,f))))
		,val-expr
		,guard-expr)
	      ctxt)))
	(else	;Error, incorrect number of arguments.
	 (mk-call (make-primref 'make-parameter) ($map/stx E args)))))

    (module (get-fmls)

      (define (get-fmls x args)
	;;Expect X to  be a sexp representing a CASE-LAMBDA  and ARGS to
	;;be a list of arguments for such function.  Scan the clauses in
	;;X looking for a set of  formals that matches ARGS: when found,
	;;return the list of formals; when not found, return null.
	;;
	(let loop ((clause* (get-clause* x)))
	  (cond ((null? clause*)
		 '())
		((matching? ($caar clause*) args)
		 ($caar clause*))
		(else
		 (loop ($cdr clause*))))))

      (define (matching? fmls args)
	;;Return true if FMLS and ARGS are lists with the same number of
	;;items.
	(cond ((null? fmls)
	       (null? args))
	      ((pair? fmls)
	       (and (pair? args)
		    (matching? ($cdr fmls) ($cdr args))))
	      (else #t)))

      (define (get-clause* x)
	;;Given a  sexp representing a  CASE-LAMBDA, return its  list of
	;;clauses.  Return null if X is not a CASE-LAMBDA sexp.
	(if (pair? x)
	    (case-symbols ($car x)
	      ((case-lambda)
	       ($cdr x))
	      ((annotated-case-lambda)
	       ($cddr x))
	      (else '()))
	  '()))

      #| end of module: get-fmls |# )

    #| end of module: E-app |# )

  (module (lexical gen-fml* ungen-fml*)

    ;;FIXME  Do we  need a  new  cookie at  each call  to the  RECORDIZE
    ;;function?  Maybe  not, because  we always  call GEN-FML*  and then
    ;;clean up with UNGEN-FML*.  (Marco Maggi; Oct 10, 2012)
    (define *cookie*
      (gensym))

    (define-inline (lexical x)
      (getprop x *cookie*))

    (define (gen-fml* fml*)
      ;;Expect FML* to be a symbol  or a list of symbols.  This function
      ;;is used to  process the formals of  LAMBDA, CASE-LAMBDA, LETREC,
      ;;LETREC*, LIBRARY-LETREC.
      ;;
      ;;When FML*  is a symbol: build  a struct instance of  type PRELEX
      ;;and  store it  in the  property list  of FML*,  then return  the
      ;;PRELEX instance.
      ;;
      ;;When FML* is  a list of symbols: for each  symbol build a struct
      ;;instance of type PRELEX and store it in the property list of the
      ;;symbol; return the list of PRELEX instances.
      ;;
      ;;The property list keyword is the gensym bound to *COOKIE*.
      ;;
      (cond ((pair? fml*)
	     (let ((v (make-prelex ($car fml*) #f)))
	       (putprop ($car fml*) *cookie* v)
	       (cons v (gen-fml* ($cdr fml*)))))
	    ((symbol? fml*)
	     (let ((v (make-prelex fml* #f)))
	       (putprop fml* *cookie* v)
	       v))
	    (else '())))

    (define (ungen-fml* fml*)
      ;;Clean up  function associated to the  function GEN-FML*.  Expect
      ;;FML* to be a symbol or a list of symbols.
      ;;
      ;;When FML* is  a symbol: remove the instance of  type PRELEX from
      ;;the property list of the symbol; return unspecified values.
      ;;
      ;;When  FML* is  a list  of symbols:  for each  symbol remove  the
      ;;struct instance  of type  PRELEX from the  property list  of the
      ;;symbol; return unspecified values.
      ;;
      ;;The property list keyword is the gensym bound to *COOKIE*.
      ;;
      (cond ((pair? fml*)
	     (remprop ($car fml*) *cookie*)
	     (ungen-fml* ($cdr fml*)))
	    ((symbol? fml*)
	     (remprop fml* *cookie*))
	    ;;When FML* is null: do nothing.
	    ))

    #| end of module |# )

  (E x))


(module (optimize-direct-calls)
  ;;By definition, a "direct closure application" like:
  ;;
  ;;   ((lambda (x) x) 123)
  ;;
  ;;can be expanded to:
  ;;
  ;;   (let ((x 123)) x)
  ;;
  ;;and  so it  can  be  converted to  low  level  operations that  more
  ;;efficiently implement  the binding; this module  attempts to perform
  ;;such inlining.
  ;;
  ;;Examples:
  ;;
  ;;* Notice that COND syntaxes are expanded as follows:
  ;;
  ;;    (cond ((this X)
  ;;           => (lambda (Y)
  ;;                (that Y)))
  ;;          (else
  ;;           (that)))
  ;;
  ;;  becomes:
  ;;
  ;;    (let ((t (this X)))
  ;;      (if t
  ;;          ((lambda (Y) (that Y)) t)
  ;;        (that)))
  ;;
  ;;  which contains a direct call, which will be optimised to:
  ;;
  ;;    (let ((t (this X)))
  ;;      (if t
  ;;          (let ((Y t)) (that Y))
  ;;        (that)))
  ;;
  (define who 'optimize-direct-calls)

  (define (optimize-direct-calls x)
    ;;Perform  code optimisation  traversing the  whole hierarchy  in X,
    ;;which must  be a struct  instance representing recordized  code in
    ;;the  core language,  and building  a new  hierarchy of  optimised,
    ;;recordized code; return the new hierarchy.
    ;;
    ;;The  only   recordized  code  that  may   actually  need  inlining
    ;;transformation are FUNCALL instances.
    ;;
    (define-syntax E ;make the code more readable
      (identifier-syntax optimize-direct-calls))
    (struct-case x
      ((constant)
       x)

      ((prelex)
       (assert (prelex-source-referenced? x))
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

      ((clambda label clause* cp free name)
       (make-clambda label
		     (map (lambda (clause)
			    (struct-case clause
			      ((clambda-case info body)
			       (make-clambda-case info (E body)))))
		       clause*)
		     cp free name))

      ((funcall rator rand*)
       (inline make-funcall (E rator) ($map/stx E rand*)))

      ((forcall rator rand*)
       (make-forcall rator ($map/stx E rand*)))

      ((assign lhs rhs)
       (assert (prelex-source-assigned? lhs))
       (make-assign lhs (E rhs)))

      (else
       (error who "invalid expression" (unparse-recordized-code x)))))

  (module (inline)

    (define (inline mk rator rand*)
      (struct-case rator
	((clambda label.unused clause*)
	 (try-inline clause* rand* (mk rator rand*)))

	((primref op)
	 (case op
	   ;;FIXME Here.  (Abdulaziz Ghuloum)
	   ((call-with-values)
	    (cond ((and (open-mvcalls)
			($fx= (length rand*) 2))
		   ;;Here we know that the source code is:
		   ;;
		   ;;   (call-with-values ?producer ?consumer)
		   ;;
		   (let ((producer (inline ($car rand*) '()))
			 (consumer ($cadr rand*)))
		     (cond ((single-value-consumer? consumer)
			    (inline consumer (list producer)))
			   ((and (valid-mv-consumer? consumer)
				 (valid-mv-producer? producer))
			    (make-mvcall producer consumer))
			   (else
			    (make-funcall rator rand*)))))
		  (else
		   ;;Here we do not know what the source code is, it could
		   ;;be something like:
		   ;;
		   ;;   (apply call-with-values ?form)
		   ;;
		   (mk rator rand*))))
	   ((debug-call)
	    (inline (lambda (op^ rand*^)
		      (mk rator (cons* ($car rand*) op^ rand*^)))
		    ($cadr rand*)
		    ($cddr rand*)))
	   (else
	    ;;Other primitive operations need no special handling.
	    (mk rator rand*))))

	((bind lhs* rhs* body)
	 (if (null? lhs*)
	     (inline mk body rand*)
	   (make-bind lhs* rhs* (call-expr mk body rand*))))

	((recbind lhs* rhs* body)
	 (if (null? lhs*)
	     (inline mk body rand*)
	   (make-recbind lhs* rhs* (call-expr mk body rand*))))

	((rec*bind lhs* rhs* body)
	 (if (null? lhs*)
	     (inline mk body rand*)
	   (make-rec*bind lhs* rhs* (call-expr mk body rand*))))

	(else
	 (mk rator rand*))))

    (define (valid-mv-consumer? x)
      ;;Return true if X is a  struct instance of type CLAMBDA, having a
      ;;single clause which accepts a  fixed number of arguments, one or
      ;;more it does not matter; else return false.
      ;;
      ;;In  other  words,  return  true  if X  represents  a  LAMBDA  or
      ;;CASE-LAMBDA like:
      ;;
      ;;   (lambda (a) ?body0 ?body ...)
      ;;   (lambda (a b c) ?body0 ?body ...)
      ;;   (case-lambda ((a) ?body0 ?body ...))
      ;;   (case-lambda ((a b c) ?body0 ?body ...))
      ;;
      (struct-case x
	((clambda label.unused clause*)
	 (and ($fx= (length clause*) 1) ;single clause?
	      (struct-case ($car clause*)
		((clambda-case info)
		 (struct-case info
		   ((case-info label.unused args.unused proper?)
		    proper?))))))
	(else #f)))

    (define (single-value-consumer? x)
      ;;Return true if X is a  struct instance of type CLAMBDA, having a
      ;;single  clause  which accepts  a  single  argument; else  return
      ;;false.
      ;;
      ;;In  other  words,  return  true  if X  represents  a  LAMBDA  or
      ;;CASE-LAMBDA like:
      ;;
      ;;   (lambda (a) ?body0 ?body ...)
      ;;   (case-lambda ((a) ?body0 ?body ...))
      ;;
      (struct-case x
	((clambda label.unused clause*)
	 (and ($fx= (length clause*) 1) ;single clause?
	      (struct-case ($car clause*)
		((clambda-case info)
		 (struct-case info
		   ((case-info label.unused args proper?)
		    (and proper?
			 ($fx= (length args) 1))))))))
	(else #f)))

    (define (valid-mv-producer? x)
      (struct-case x
	((funcall)
	 #t)
	((conditional)
	 #f)
	((bind lhs* rhs* body)
	 (valid-mv-producer? body))
	;;FIXME Bug.  (Abdulaziz Ghuloum)
	;;
	;;FIXME Why is it a bug?  (Marco Maggi; Oct 12, 2012)
	(else #f)))

    (define (call-expr mk x rand*)
      (cond ((clambda? x)
	     (inline mk x rand*))
	    ((and (prelex? x)
		  (not (prelex-source-assigned? x)))
	     ;;FIXME Did we do the analysis yet?  (Abdulaziz Ghuloum)
	     (mk x rand*))
	    (else
	     (let ((t (make-prelex 'tmp #f)))
	       (set-prelex-source-referenced?! t #t)
	       (make-bind (list t) (list x) (mk t rand*))))))

    #| end of module: inline |# )

  (module (try-inline)

    (define (try-inline clause* rand* default)
      ;;Iterate  the CASE-LAMBDA  clauses in  CLAUSE* searching  for one
      ;;whose  formals  match the  RAND*  arguments;  if found  generate
      ;;appropriate local bindings that  inline the closure application.
      ;;If successful return a struct instance of type BIND, else return
      ;;DEFAULT.
      ;;
      (cond ((null? clause*)
	     default)
	    ((inline-case ($car clause*) rand*))
	    (else
	     (try-inline ($cdr clause*) rand* default))))

    (define (inline-case clause rand*)
      ;;Try to  convert the CASE-LAMBDA clause  in CLAUSE into a  set of
      ;;local bindings for the arguments  in RAND*; if successful return
      ;;a struct instance of type BIND, else return #f.
      ;;
      (struct-case clause
	((clambda-case info body)
	 (struct-case info
	   ((case-info label fml* proper?)
	    (if proper?
		;;If the formals  of the CASE-LAMBDA clause  is a proper
		;;list, make an appropriate local binding; convert:
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
	      ;;If the formals of the  CASE-LAMBDA clause is a symbol or
	      ;;a  proper  list,  make  an  appropriate  local  binding;
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
	      ;;
	      (and ($fx<= (length fml*)
			  (length rand*))
		   (make-bind fml* (%properize fml* rand*) body))))))))

    (define (%properize lhs* rhs*)
      ;;LHS*  must  be a  list  of  PRELEX structures  representing  the
      ;;binding names of a CASE-LAMBDA  clause, for the cases of formals
      ;;being  a symbol  or an  improper list;  RHS* must  be a  list of
      ;;struct instances representing the values to be bound.
      ;;
      ;;Build and return a new list out of RHS* that matches the list in
      ;;LHS*.
      ;;
      ;;If  LHS* holds  a single  item:  it means  that the  CASE-LAMBDA
      ;;application is like:
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
      ;;If LHS*  holds a multiple  items: it means that  the CASE-LAMBDA
      ;;application is like:
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
	     (error who "improper improper"))
	    ((null? ($cdr lhs*))
	     (list (%make-conses rhs*)))
	    (else
	     (cons ($car rhs*)
		   (%properize ($cdr lhs*)
			       ($cdr rhs*))))))

    (define (%make-conses ls)
      (if (null? ls)
	  (make-constant '())
	(make-funcall (make-primref 'cons)
		      (list ($car ls) (%make-conses ($cdr ls))))))

    #| end of module: try-inline |# )

  #| end of module: optimize-direct-calls |# )


;;;; some external code

#|
(letrec* (bi ...
          (x (let ((lhs* rhs*) ...) body))
          bj ...)
  body)
===?
(letrec* (bi ...
            (tmp* rhs*) ...
            (lhs* tmp*) ...
            (x body)
          bj ...)
  body)
|#

(include/verbose "ikarus.compiler.optimize-letrec.ss")
(include/verbose "ikarus.compiler.source-optimizer.ss")


(module (rewrite-references-and-assignments)
  ;;We distinguish between bindings that are only referenced (read-only)
  ;;and bindings that are also mutated (read-write).  Example of code in
  ;;which the binding X is only referenced:
  ;;
  ;;  (let ((x 123)) (display x))
  ;;
  ;;example of code in which the binding X is mutated and referenced:
  ;;
  ;;  (let ((x 123)) (set! x 456) (display x))
  ;;
  ;;A binding in  reference position or in the left-hand  side of a SET!
  ;;syntax, is  represented by  a struct instance  of type  PRELEX; this
  ;;module must be used only on  recordized code in which referenced and
  ;;mutated  bindings  have already  been  marked  appropriately in  the
  ;;PRELEX structures.
  ;;
  ;;* Top level bindings are  implemented with gensyms holding the value
  ;;  in an internal field.  References and assignments to such bindings
  ;;  must be substituted with appropriate function calls.
  ;;
  ;;*  Read-write local  bindings  are implemented  with Scheme  vectors
  ;;  providing mutable  memory locations: one vector  for each binding.
  ;;  References  and assignments to  such bindings must  be substituted
  ;;  with appropriate vector operations.
  ;;
  ;;Code representing usage of a read-write binding like:
  ;;
  ;;   (let ((x 123))
  ;;     (set! x 456)
  ;;     x)
  ;;
  ;;is transformed into:
  ;;
  ;;   (let ((t 123))
  ;;     (let ((x (vector t)))
  ;;       ($vector-set! x 0 456)
  ;;       ($vector-ref x 0)))
  ;;
  ;;Code representing usage of multiple a bindings like:
  ;;
  ;;   (let ((x 1)
  ;;         (y 2))
  ;;     (set! x 3)
  ;;     (list x y))
  ;;
  ;;is transformed into:
  ;;
  ;;   (let ((t 1)
  ;;         (y 2))
  ;;     (let ((x (vector t)))
  ;;       ($vector-set! x 0 3)
  ;;       (list ($vector-ref x 0) y)))
  ;;
  (define who 'rewrite-references-and-assignments)

  (define (rewrite-references-and-assignments x)
    ;;Perform code  transformation traversing the whole  hierarchy in X,
    ;;which must  be a struct  instance representing recordized  code in
    ;;the core  language, and building  a new hierarchy  of transformed,
    ;;recordized code; return the new hierarchy.
    ;;
    (define-syntax E ;make the code more readable
      (identifier-syntax rewrite-references-and-assignments))
    (struct-case x
      ((constant)
       x)

      ((prelex)
       (if (prelex-source-assigned? x)
	   ;;Reference to a read-write binding.
	   (cond ((prelex-global-location x)
		  ;;Reference to a top level binding.  LOC is the symbol
		  ;;used to hold the value.
		  => (lambda (loc)
		       (make-funcall (make-primref '$symbol-value)
				     (list (make-constant loc)))))
		 (else
		  ;;Reference to local  mutable binding: substitute with
		  ;;appropriate reference to the vector location.
		  (make-funcall (make-primref '$vector-ref)
				(list x (make-constant 0)))))
	 ;;Reference to a read-only binding.
	 x))

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (let-values (((lhs* a-lhs* a-rhs*) (%fix-lhs* lhs*)))
         (make-bind lhs* ($map/stx E rhs*)
		    (%bind-assigned a-lhs* a-rhs* (E body)))))

      ((fix lhs* rhs* body)
       (make-fix lhs* ($map/stx E rhs*) (E body)))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda label clause* cp free name)
       (make-clambda label
		     (map (lambda (cls)
			    (struct-case cls
			      ((clambda-case info body)
			       (struct-case info
				 ((case-info label fml* proper)
				  (let-values (((fml* a-lhs* a-rhs*) (%fix-lhs* fml*)))
				    (make-clambda-case
				     (make-case-info label fml* proper)
				     (%bind-assigned a-lhs* a-rhs* (E body)))))))))
		       clause*)
		     cp free name))

      ((forcall op rand*)
       (make-forcall op ($map/stx E rand*)))

      ((funcall rator rand*)
       (make-funcall (E rator) ($map/stx E rand*)))

      ((assign lhs rhs)
       (cond ((prelex-source-assigned? lhs)
	      => (lambda (where)
		   (cond ((symbol? where)
			  ;;FIXME What is this  case?  (Marco Maggi; Oct
			  ;;12, 2012)
			  (make-funcall (make-primref '$init-symbol-value!)
					(list (make-constant where) (E rhs))))
			 ((prelex-global-location lhs)
			  ;;Mutation of  top level binding.  LOC  is the
			  ;;symbol used to hold the value.
			  => (lambda (loc)
			       (make-funcall (make-primref '$set-symbol-value!)
					     (list (make-constant loc) (E rhs)))))
			 (else
			  ;;Mutation of local  binding.  Substitute with
			  ;;the appropriate vector operation.
			  (make-funcall (make-primref '$vector-set!)
					(list lhs (make-constant 0) (E rhs)))))))
	     (else
	      (error who "not assigned" lhs x))))

      ((mvcall p c)
       (make-mvcall (E p) (E c)))

      (else
       (error who "invalid expression" (unparse-recordized-code x)))))

  (define (%fix-lhs* lhs*)
    ;;LHS* is  a list  of struct instances  of type  PRELEX representing
    ;;bindings.  Return 3 values:
    ;;
    ;;**A  list of  struct  instances of  type  PRELEX representing  the
    ;;  bindings to be created to hold the right-hand side values.
    ;;
    ;;**A  list of  struct  instances of  type  PRELEX representing  the
    ;;  read-write bindings referencing the mutable vectors.
    ;;
    ;;**A  list of  struct  instances of  type  PRELEX representing  the
    ;;  temporary bindings used to hold the right-hand side values.
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
    ;;the argument LHS*  is a list holding the PRELEX  for X; the return
    ;;values are:
    ;;
    ;;1. A list holding the PRELEX for T.
    ;;
    ;;2. A list holding the PRELEX for X.
    ;;
    ;;3. A list holding the PRELEX for T.
    ;;
    (if (null? lhs*)
	(values '() '() '())
      (let ((x ($car lhs*)))
	(let-values (((lhs* a-lhs* a-rhs*) (%fix-lhs* ($cdr lhs*))))
	  (if (and (prelex-source-assigned? x)
		   (not (prelex-global-location x)))
	      ;;We always  use the same  PRELEX name because it  is used
	      ;;only for debugging purposes.
	      (let ((t (make-prelex 'assignment-tmp #f)))
		(set-prelex-source-referenced?! t #t)
		(values (cons t lhs*)
			(cons x a-lhs*)
			(cons t a-rhs*)))
	    (values (cons x lhs*) a-lhs* a-rhs*))))))

  (define (%bind-assigned lhs* rhs* body)
    ;;LHS* must be a list of struct instances of type PRELEX represening
    ;;read-write bindings.
    ;;
    ;;RHS* must be a list  of struct instance representing references to
    ;;temporary bindings holding the binding's values.
    ;;
    ;;BODY must be  a struct instance representing code  to be evaluated
    ;;in the region of the bindings.
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
    ;;in this case LHS* is a list holding the PRELEX for X and RHS* is a
    ;;list holding the PRELEX for T.
    ;;
    (if (null? lhs*)
	body
      (make-bind lhs*
		 (map (lambda (rhs)
			(make-funcall (make-primref 'vector) (list rhs)))
		   rhs*)
		 body)))

  #| end of module: rewrite-references-and-assignments |# )


;;;; some other external code

(include/verbose "ikarus.compiler.tag-annotation-analysis.ss")


(module (introduce-vars)
  ;;This module  operates on  recordized code  representing code  in the
  ;;core  language; it  substitutes  all the  struct  instances of  type
  ;;PRELEX with struct instances of type VAR.
  ;;
  (define who 'introduce-vars)

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax introduce-vars))

  (define (introduce-vars x)
    ;;Perform code  transformation traversing the whole  hierarchy in X,
    ;;which must  be a struct  instance representing recordized  code in
    ;;the core  language, and building  a new hierarchy  of transformed,
    ;;recordized code; return the new hierarchy.
    ;;
    (struct-case x
      ((constant)
       x)

      ((prelex)
       (lookup x))

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (let ((lhs* (map %convert-prelex lhs*)))
         (make-bind lhs* (map E rhs*) (E body))))

      ((fix lhs* rhs* body)
       (let ((lhs* (map %convert-prelex lhs*)))
         (make-fix lhs* (map E rhs*) (E body))))

      ((conditional e0 e1 e2)
       (make-conditional (E e0) (E e1) (E e2)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda label clause* cp free name)
       ;;The purpose of this form is to apply %CONVERT-PRELEX to all the
       ;;items in the ARGS field of all the CASE-INFO structs.
       (make-clambda label
		     (map (lambda (cls)
			    (struct-case cls
			      ((clambda-case info body)
			       (struct-case info
				 ((case-info label args proper)
				  (let ((args (map %convert-prelex args)))
				    (make-clambda-case (make-case-info label args proper)
						       (E body))))))))
		       clause*)
		     cp free name))

      ((primcall rator rand*)
       (make-primcall rator (map E-known rand*)))

      ((funcall rator rand*)
       (make-funcall (E-known rator) (map E-known rand*)))

      ((forcall rator rand*)
       (make-forcall rator (map E rand*)))

      ((assign lhs rhs)
       (make-assign (lookup lhs) (E rhs)))

      (else
       (error who "invalid expression" (unparse-recordized-code x)))))

  (define (lookup prel)
    ;;Given  a struct  instance of  type PRELEX  in reference  position,
    ;;return the associated struct instance of type VAR.
    ;;
    ;;It  is  a very  bad  error  if this  function  finds  a PRELEX  in
    ;;reference position not yet processed by %CONVERT-PRELEX.
    ;;
    (let ((v ($prelex-operand prel)))
      (assert (var? v))
      v))

  (define (%convert-prelex prel)
    ;;Convert the PRELEX  struct PREL into a VAR struct;  return the VAR
    ;;struct.
    ;;
    ;;The generated  VAR struct is  stored in  the field OPERAND  of the
    ;;PRELEX, so that, later, references to the PRELEX in the recordized
    ;;code can be substituted with the VAR.
    ;;
    (assert (not (var? ($prelex-operand prel))))
    (let ((v (unique-var ($prelex-name prel))))
      ($set-var-referenced! v ($prelex-source-referenced? prel))
      ($set-var-global-loc! v ($prelex-global-location prel))
      ($set-prelex-operand! prel v)
      v))

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

  #| end of module: introduce-vars |# )


(module (sanitize-bindings)
  ;;Separates bindings having  a CLAMBDA struct as  right-hand side into
  ;;struct instances of type FIX.
  ;;
  ;;This module  must be used with  recordized code in which  the PRELEX
  ;;instances have been already substituted by VAR instances.
  ;;
  ;;Code like:
  ;;
  ;;   (let ((a 123)
  ;;         (b (lambda (x) (this))))
  ;;     (that))
  ;;
  ;;is transformed into:
  ;;
  ;;   (let ((a 123))
  ;;     (let ((b (lambda (x) (this))))
  ;;       (that)))
  ;;
  ;;Code like:
  ;;
  ;;   (case-lambda (?formal ?body0 ?body ...))
  ;;
  ;;is transformed into:
  ;;
  ;;   (let ((t (case-lambda (?formal ?body0 ?body ...))))
  ;;     t)
  ;;
  ;;this is useful for later transformations.
  ;;
  (define who 'sanitize-bindings)

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax sanitize-bindings))

  (define (sanitize-bindings x)
    ;;Perform code  transformation traversing the whole  hierarchy in X,
    ;;which must  be a struct  instance representing recordized  code in
    ;;the core  language, and building  a new hierarchy  of transformed,
    ;;recordized code; return the new hierarchy.
    ;;
    (struct-case x
      ((constant)
       x)

      ((var)
       x)

      ((primref)
       x)

      ((bind lhs* rhs* body)
       ;;Here we want to transform:
       ;;
       ;;   (let ((a 123)
       ;;         (b (lambda (x) (this))))
       ;;     (that))
       ;;
       ;;into:
       ;;
       ;;   (let ((a 123))
       ;;     (let ((b (lambda (x) (this))))
       ;;       (that)))
       ;;
       ;;in which the  inner LET is represented by a  struct instance of
       ;;type FIX.
       (let-values (((lambda* other*) (partition (lambda (x)
						   (clambda? ($cdr x)))
					($map/stx cons lhs* rhs*))))
	 ;;LAMBDA* is  a list  of pairs (LHS  . RHS) in  which RHS  is a
	 ;;CLAMBDA struct.
	 ;;
	 ;;OTHER* is a list of pairs (LHS . RHS) in which RHS is *not* a
	 ;;CLAMBDA struct.
	 ;;
         (make-bind ($map/stx $car other*)
                    ($map/stx E ($map/stx $cdr other*))
		    (%do-fix ($map/stx $car lambda*)
			     ($map/stx $cdr lambda*)
			     body))))

      ((fix lhs* rhs* body)
       (%do-fix lhs* rhs* body))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda)
       ;;Here we want to transform:
       ;;
       ;;   (case-lambda (?formal ?body0 ?body ...))
       ;;
       ;;into:
       ;;
       ;;   (let ((t (case-lambda (?formal ?body0 ?body ...))))
       ;;     t)
       ;;
       ;;in which  the LET is represented  by a struct instance  of type
       ;;FIX.
       (let ((t (unique-var 'anon)))
         (make-fix (list t) (list (CLambda x)) t)))

      ((forcall op rand*)
       (make-forcall op ($map/stx E rand*)))

      ((funcall rator rand*)
       (make-funcall (E-known rator) ($map/stx E-known rand*)))

      ((mvcall p c)
       (make-mvcall (E p) (E c)))

      (else
       (error who "invalid expression" (unparse-recordized-code x)))))

  (define (CLambda x)
    ;;The argument  X must be  a struct  instance of type  CLAMBDA.  The
    ;;purpose of  this function  is to  apply E to  every body  of every
    ;;CASE-LAMBDA clause.
    ;;
    (struct-case x
      ((clambda label clause* cp free name)
       (make-clambda label
		     (map (lambda (cls)
			    (struct-case cls
			      ((clambda-case info body)
			       (struct-case info
				 ((case-info label fml* proper)
				  (make-clambda-case (make-case-info label fml* proper)
						     (E body)))))))
		       clause*)
		     cp free name))))

  (define (%do-fix lhs* rhs* body)
    (if (null? lhs*)
        (E body)
      (make-fix lhs* ($map/stx CLambda rhs*) (E body))))

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

  #| end of module: sanitize-bindings |# )


(module (optimize-for-direct-jumps)
  ;;We known that direct function applications like:
  ;;
  ;;   ((lambda (x) x) 123)
  ;;
  ;;can be optimized to:
  ;;
  ;;   (let ((x 123)) x)
  ;;
  ;;and also:
  ;;
  ;;   ((case-lambda ((x) x)
  ;;                 ((x y) (list x y)))
  ;;    1 2)
  ;;
  ;;can be optimized to:
  ;;
  ;;   ((case-lambda ((x y) (list x y))) 1 2)
  ;;
  ;;and so to:
  ;;
  ;;   (let ((x 1) (y 2)) (list x y))
  ;;
  ;;this is what the module OPTIMIZE-DIRECT-CALLS does.  Fine.
  ;;
  ;;This  module   is,  in  a   way,  a  generalisation  of   the  above
  ;;optimisation; let's consider the following code:
  ;;
  ;;   (let ((f (lambda (x) x)))
  ;;     (f 123))
  ;;
  ;;while we  cannot, in general,  replace the LAMBDA definition  with a
  ;;low level binding, there is a way to jump directly from the function
  ;;application to the function  implementation without executing a full
  ;;closure call.  When the closure definition has multiple clauses:
  ;;
  ;;   (let ((f (case-lambda
  ;;              ((x)		x)
  ;;              ((x y)	(list x y)))))
  ;;     (f 1 2))
  ;;
  ;;there is a way to jump directly from the function application to the
  ;;clause that matches the number of arguments.
  ;;
  ;;This optimisation is possible only  when: a preliminary iteration of
  ;;the recordized  code let's  us find  bindings whose  right-hand size
  ;;evaluates to  a CASE-LAMBDA,  that is to  a closure  definition with
  ;;known clauses.
  ;;
  ;;This module  must be used with  recordized code in which  the PRELEX
  ;;instances have been already substituted  by VAR instances.  It is to
  ;;be used after using SANITIZE-BINDINGS.
  ;;
  (define who 'optimize-for-direct-jumps)

  ;;Make the code more readable.
  (define-syntax E
    (identifier-syntax optimize-for-direct-jumps))

  (define (optimize-for-direct-jumps x)
    ;;Perform  code optimisation  traversing the  whole hierarchy  in X,
    ;;which must  be a struct  instance representing recordized  code in
    ;;the  core language,  and building  a new  hierarchy of  optimised,
    ;;recordized code; return the new hierarchy.
    ;;
    (struct-case x
      ((constant)
       x)

      ((var)
       x)

      ((primref)
       x)

      ((bind lhs* rhs* body)
       ($for-each/stx %mark-var-as-non-referenced lhs*)
       (let ((rhs* ($map/stx E rhs*)))
	 ($for-each/stx %maybe-mark-var-as-referencing-clambda lhs* rhs*)
	 (make-bind lhs* rhs* (E body))))

      ((fix lhs* rhs* body)
       ($for-each/stx %maybe-mark-var-as-referencing-clambda lhs* rhs*)
       (make-fix lhs* ($map/stx CLambda rhs*) (E body)))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((forcall op rand*)
       (make-forcall op ($map/stx E rand*)))

      ((funcall rator rand*)
       (%optimize-funcall rator rand*))

      (else
       (error who "invalid expression" (unparse-recordized-code x)))))

  (define-inline (%mark-var-as-non-referenced x)
    ($set-var-referenced! x #f))

  (define (%maybe-mark-var-as-referencing-clambda lhs rhs)
    (struct-case rhs
      ((clambda)
       ($set-var-referenced! lhs rhs))
      ((var)
       (cond ((%bound-var rhs)
	      => (lambda (v)
		   ($set-var-referenced! lhs v)))
	     (else
	      (void))))
      (else
       (void))))

  (define-inline (%bound-var x)
    ($var-referenced x))

  (module (%optimize-funcall)

    (define (%optimize-funcall rator rand*)
      (let-values (((rator type) (untag (E-known rator))))
	(cond
	 ;;Is RATOR a  variable known to reference a  closure?  In this
	 ;;case we can attempt an optimization.
	 ((and (var? rator)
	       (%bound-var rator))
	  => (lambda (c)
	       (%optimize c rator ($map/stx E-known rand*))))

	 ;;Is RATOR the  low level APPLY operation?  In  this case: the
	 ;;first  RAND*  should  be   a  struct  instance  representing
	 ;;recordized code which will evaluate to a closure.
	 ((and (primref? rator)
	       (eq? (primref-name rator) '$$apply))
	  (make-jmpcall (sl-apply-label)
			(E-unpack-known ($car rand*))
			($map/stx E-unpack-known ($cdr rand*))))

	 ;;If we are  here: RATOR is just some  unknown struct instance
	 ;;representing  recordized code  which,  when evaluated,  will
	 ;;return a closure.
	 (else
	  (make-funcall (tag rator type) ($map/stx E-known rand*))))))

    (define (%optimize clam rator rand*)
      ;;Attempt to optimize the function application:
      ;;
      ;;   (RATOR . RAND*)
      ;;
      ;;CLAM is a struct instance of type CLAMBDA.
      ;;
      ;;RATOR  is a  struct  instance  of type  VAR  which  is known  to
      ;;reference the CLAMBDA in CLAM.
      ;;
      ;;RAND* is a list of struct instances representing recordized code
      ;;which,  when  evaluated,  will  return  the  arguments  for  the
      ;;function application.
      ;;
      ;;This function  searches for a  clause in CLAM which  matches the
      ;;arguments given in RAND*:
      ;;
      ;;*  If   found:  return  a   struct  instance  of   type  JMPCALL
      ;;  representing a jump call to the matching clause.
      ;;
      ;;* If  not found: just return  a struct instance of  type FUNCALL
      ;;  representing a normal function call.
      ;;
      (struct-case clam
	((clambda label.unused clause*)
	 ;;Number of arguments in this function application.
	 (define num-of-rand* (length rand*))
	 (let recur ((clause* clause*))
	   (define-inline (%recur-to-next-clause)
	     (recur ($cdr clause*)))
	   (if (null? clause*)
	       ;;No  matching clause  found.  Just  call the  closure as
	       ;;always.
	       (make-funcall rator rand*)
	     (struct-case ($clambda-case-info ($car clause*))
	       ((case-info label fml* proper?)
		(if proper?
		    ;;This clause has a fixed number of arguments.
		    (if ($fx= num-of-rand* (length fml*))
			(make-jmpcall label (strip rator) ($map/stx strip rand*))
		      (%recur-to-next-clause))
		  ;;This clause has a variable number of arguments.
		  (if ($fx<= (length ($cdr fml*)) num-of-rand*)
		      (make-jmpcall label (strip rator) (%prepare-rand* ($cdr fml*) rand*))
		    (%recur-to-next-clause))))))))))

    (define (%prepare-rand* fml* rand*)
      ;;FML* is a  list of struct instances of  type PRELEX representing
      ;;the formal arguments of the CASE-LAMBDA clause.
      ;;
      ;;RAND* is a  list os struct instances  representing the arguments
      ;;to the closure application.
      ;;
      ;;This function processes RAND* and builds a new list representing
      ;;arguments that can be assigned to the formals of the clause.  If
      ;;the clause is:
      ;;
      ;;   ((a b . args) ?body0 ?body ...)
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
      (if (null? fml*)
	  ;;FIXME Construct list afterwards.  (Abdulaziz Ghuloum)
	  (list (make-funcall (make-primref 'list) rand*))
	(cons (strip ($car rand*))
	      (%prepare-rand* ($cdr fml*) ($cdr rand*)))))

    (define (strip x)
      (struct-case x
	((known expr)
	 expr)
	(else x)))

    #| end of module: %optimize-funcall |# )

  (define (CLambda x)
    ;;The argument  X must be  a struct  instance of type  CLAMBDA.  The
    ;;purpose  of this  function  is to  apply  E to  the  body of  each
    ;;CASE-LAMBDA  clause, after  having  marked  as non-referenced  the
    ;;corresponding formals.
    ;;
    (struct-case x
      ((clambda label clause* cp free name)
       (make-clambda label
		     (map (lambda (cls)
			    (struct-case cls
			      ((clambda-case info body)
			       ($for-each/stx %mark-var-as-non-referenced
					      (case-info-args info))
			       (make-clambda-case info (E body)))))
		       clause*)
		     cp free name))))

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

  (define (E-unpack-known x)
    (struct-case x
      ((known expr)
       (E expr))
      (else
       (E x))))

  (define (tag expr type)
    (if type
	(make-known expr type)
      expr))

  (define (untag x)
    (struct-case x
      ((known expr type)
       (values expr type))
      (else
       (values x #f))))

  #| end of module: optimize-for-direct-jumps |# )


(module (insert-global-assignments)

  (define who 'insert-global-assignments)

  ;;Make the code more readable.
  (define-syntax M
    (identifier-syntax insert-global-assignments))

  (define (insert-global-assignments x)
    ;;Perform code  transformation traversing the whole  hierarchy in X,
    ;;which must  be a struct  instance representing recordized  code in
    ;;the core  language, and building  a new hierarchy  of transformed,
    ;;recordized code; return the new hierarchy.
    ;;
    ;;This  function  traverses  top  level  forms  only;  forms  inside
    ;;CASE-LAMBDA are traversed by the function E.
    ;;
    (struct-case x
      ((constant)
       x)

      ((var)
       x)

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (make-bind lhs* ($map/stx M rhs*)
		  (%global-assign lhs* (M body))))

      ((fix lhs* rhs* body)
       (make-fix lhs* ($map/stx M rhs*)
		 (%global-fix lhs* (M body))))

      ((conditional test conseq altern)
       (make-conditional (M test) (M conseq) (M altern)))

      ((seq e0 e1)
       (make-seq (M e0) (M e1)))

      ((clambda label clause* cp free name)
       ;;Apply E to every body of every CASE-LAMBDA clause.
       (make-clambda label
		     (map (lambda (clause)
			    (struct-case clause
			      ((clambda-case info body)
			       (make-clambda-case info (E body)))))
		       clause*)
		     cp free name))

      ((forcall op rand*)
       (make-forcall op ($map/stx M rand*)))

      ((funcall rator rand*)
       (make-funcall (M-known rator) ($map/stx M-known rand*)))

      ((jmpcall label rator rand*)
       (make-jmpcall label (M rator) ($map/stx M rand*)))

      (else
       (error who "invalid expression" (unparse-recordized-code x)))))

  (define (E x)
    ;;Traverses  the  recordized code  inside  the  body of  CASE-LAMBDA
    ;;forms.
    ;;
    (struct-case x
      ((constant)
       x)

      ((var)
       (cond ((var-global-loc x)
	      => (lambda (loc)
		   (make-funcall (make-primref '$symbol-value)
				 (list (make-constant loc)))))
	     (else x)))

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (make-bind lhs* ($map/stx E rhs*)
		  (%global-assign lhs* (E body))))

      ((fix lhs* rhs* body)
       (make-fix lhs* ($map/stx E rhs*)
		 (%global-fix lhs* (E body))))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda label clause* cp free name)
       ;;Apply E to every body of every CASE-LAMBDA clause.
       (make-clambda label
		     (map (lambda (clause)
			    (struct-case clause
			      ((clambda-case info body)
			       (make-clambda-case info (E body)))))
		       clause*)
		     cp free name))

      ((forcall op rand*)
       (make-forcall op ($map/stx E rand*)))

      ((funcall rator rand*)
       (make-funcall (E-known rator) ($map/stx E-known rand*)))

      ((jmpcall label rator rand*)
       (make-jmpcall label (E rator) ($map/stx E rand*)))

      (else
       (error who "invalid expression" (unparse-recordized-code x)))))

  (define (%global-assign lhs* body.already-processed)
    ;;Prepend to the body of a BIND struct a call to $INIT-SYMBOL-VALUE!
    ;;for each of the VAR structs in LHS*.
    ;;
    (cond ((null? lhs*)
	   body.already-processed)
	  ((var-global-loc ($car lhs*))
	   => (lambda (loc)
		(make-seq (make-funcall (make-primref '$init-symbol-value!)
					(list (make-constant loc) ($car lhs*)))
			  (%global-assign ($cdr lhs*) body.already-processed))))
	  (else
	   (%global-assign ($cdr lhs*) body.already-processed))))

  (define (%global-fix lhs* body.already-processed)
    ;;Prepend   to   the   body   of    a   FIX   struct   a   call   to
    ;;$SET-SYMBOL-VALUE/PROC! for the first VAR struct in LHS*; the rest
    ;;of the VAR structs are handed to %GLOBAL-ASSIGN.
    ;;
    ;;FIXME Why is this?  (Marco Maggi; Oct 13, 2012)
    ;;
    (cond ((null? lhs*)
	   body.already-processed)
	  ((var-global-loc ($car lhs*))
	   => (lambda (loc)
		(make-seq (make-funcall (make-primref '$set-symbol-value/proc!)
					(list (make-constant loc) ($car lhs*)))
			  (%global-assign ($cdr lhs*) body.already-processed))))
	  (else
	   (%global-assign ($cdr lhs*) body.already-processed))))

  (define (E-known x)
    (struct-case x
      ((known expr type)
       (make-known (E expr) type))
      (else
       (E x))))

  (define (M-known x)
    (struct-case x
      ((known expr type)
       (make-known (M expr) type))
      (else
       (M x))))

  #| end of module: insert-global-assignments |# )


(module (convert-closures)
  ;;This  module  converts  all  the  CLAMBDA  structures  into  CLOSURE
  ;;structures, compiling  a list of  free variables referenced  by each
  ;;CLOSURE.
  ;;
  (define who 'convert-closures)

  (define (convert-closures X)
    ;;Perform code  transformation traversing the whole  hierarchy in X,
    ;;which must  be a struct  instance representing recordized  code in
    ;;the core  language, and building  a new hierarchy  of transformed,
    ;;recordized code; return the new hierarchy.
    ;;
    ;;The bulk of the work is performed by the recursive function E.
    ;;
    (let-values (((X^ free) (E X)))
      (if (null? free)
	  X^
	(error who "free vars encountered in program"
	       (map unparse-recordized-code free)))))

  (define (E X)
    ;;Traverse  the  recordized  code  X  and return  2  values:  a  new
    ;;recordized code hierarchy, a list  of struct instances of type VAR
    ;;representing the free variables in X.
    ;;
    (struct-case X
      ((constant)
       (values X '()))

      ((var)
       (set-var-index! X #f)
       (values X (list X)))

      ((primref)
       (values X '()))

      ((bind lhs* rhs* body)
       (let-values (((rhs*^ rhs-free)  (E* rhs*))
                    ((body^ body-free) (E  body)))
	 (values (make-bind lhs* rhs*^ body^)
		 ;;If a VAR struct is a  binding in this BIND: it is not
		 ;;a free variable; so remove it.
		 (union rhs-free (difference body-free lhs*)))))

      ((fix lhs* rhs* body)
       (for-each (lambda (x)
		   (set-var-index! x #t))
	 lhs*)
       (let-values (((rhs*^ rhs-free)  (%do-clambda* lhs* rhs*))
                    ((body^ body-free) (E body)))
	 ;;RHS*^ is a list of struct instances of type CLOSURE.
	 (for-each (lambda (lhs rhs)
		     (when (var-index lhs)
		       (set-closure-well-known?! rhs #t)
		       (set-var-index! lhs #f)))
	   lhs* rhs*^)
	 (values (make-fix lhs* rhs*^ body^)
		 ;;If a VAR struct is a binding in this FIX: it is not a
		 ;;free variable; so remove it.
		 (difference (union body-free rhs-free) lhs*))))

      ((conditional test conseq altern)
       (let-values (((test^     test-free) (E test))
                    ((conseq^ conseq-free) (E conseq))
                    ((altern^ altern-free) (E altern)))
         (values (make-conditional test^ conseq^ altern^)
                 (union test-free (union conseq-free altern-free)))))

      ((seq e0 e1)
       (let-values (((e0^ e0-free) (E e0))
                    ((e1^ e1-free) (E e1)))
         (values (make-seq e0^ e1^)
		 (union e0-free e1-free))))

      ((forcall op rand*)
       (let-values (((rand*^ rand*-free) (E* rand*)))
         (values (make-forcall op rand*^)
		 rand*-free)))

      ((funcall rator rand*)
       (let-values (((rator^ rat-free)   (E-known  rator))
                    ((rand*^ rand*-free) (E-known* rand*)))
         (values (make-funcall rator^ rand*^)
                 (union rat-free rand*-free))))

      ((jmpcall label rator rand*)
       (let-values (((rator^ rat-free)   (if (optimize-cp)
					    (Rator rator)
					  (E rator)))
                    ((rand*^ rand*-free) (E-known* rand*)))
         (values (make-jmpcall label rator^ rand*^)
                 (union rat-free rand*-free))))

      (else
       (error who "invalid expression" X))))

  (define (E* X*)
    ;;Map E  over each  element of X*,  which must be  a list  of struct
    ;;instances  representing  recordized  code; return  2  values:  the
    ;;processed X*, a list struct instances of type VAR representing the
    ;;free variables referenced by X*.
    ;;
    (if (null? X*)
	(values '() '())
      (let-values (((a a-free) (E  ($car X*)))
		   ((d d-free) (E* ($cdr X*))))
	(values (cons a d)
		(union a-free d-free)))))

  (module (%do-clambda*)

    (define (%do-clambda* lhs* rhs*)
      ;;LHS* is a list  of struct instances of type VAR;  RHS* is a list
      ;;of struct instances of type CLAMBDA.
      ;;
      ;;Apply %DO-CLAMBDA to every associated couple from LHS* and RHS*;
      ;;return 2 values:
      ;;
      ;;1. A list of struct instances of type CLOSURE which must replace
      ;;   the original RHS*.
      ;;
      ;;2. A list of struct instances  of type VAR representing the free
      ;;   variables referenced by the CLOSURE structs.
      ;;
      (if (null? rhs*)
	  (values '() '())
	(let-values (((a a-free) (%do-clambda  ($car lhs*) ($car rhs*)))
		     ((d d-free) (%do-clambda* ($cdr lhs*) ($cdr rhs*))))
	  (values (cons a d)
		  (union a-free d-free)))))

    (define (%do-clambda lhs rhs)
      ;;LHS is a  struct instance of type VAR; RHS  is a struct instance
      ;;of type CLAMBDA; LHS is the binding of RHS.
      ;;
      ;;Build a struct  instance of type CLOSURE which  must replace the
      ;;original RHS.
      ;;
      ;;Return 2 values: the CLOSURE  struct, a list of struct instances
      ;;of type  VAR representing the  free variables referenced  by the
      ;;CLOSURE.
      ;;
      (struct-case rhs
	((clambda label clause* cp.unused free.unused name)
	 (let-values (((clause*^ free) (%process-clauses clause*)))
	   (values (make-closure (make-clambda label clause*^ lhs free name)
				 free #f)
		   free)))))

    (define (%process-clauses clause*)
      ;;CLAUSE* is a list of struct instances of type CLAMBDA-CASE.
      ;;
      ;;Process all the clauses in CLAUSE*; return 2 values:
      ;;
      ;;1. A  list of struct  instances of type CLAMBDA-CASE  which must
      ;;   replace the original CLAUSE*.
      ;;
      ;;2. A list of struct instances  of type VAR representing the free
      ;;   variables referenced by the bodies of the clauses.
      ;;
      (if (null? clause*)
	  (values '() '())
	(struct-case (car clause*)
	  ((clambda-case info body)
	   (let-values (((body^    body-free)    (E body))
			((clause*^ clause*-free) (%process-clauses (cdr clause*))))
	     (values (cons (make-clambda-case info body^) clause*^)
		     ;;If a VAR  struct is a clause  formal argument: it
		     ;;is not a free variable; so remove it.
		     (union (difference body-free (case-info-args info))
			    clause*-free)))))))

    #| end of module: do-clambda* |# )

  (define (E-known x)
    ;;Apply  E  to X,  which  must  be  a struct  instance  representing
    ;;recordized  code; return  2 values:  the  processed X,  a list  of
    ;;struct  instances  of type  VAR  representing  the free  variables
    ;;referenced by X.
    ;;
    (struct-case x
      ((known expr type)
       (let-values (((expr^ free) (E expr)))
         (values (make-known expr^ type)
		 free)))
      (else
       (E x))))

  (define (E-known* X*)
    ;;Map  E-known over  each element  of X*,  which must  be a  list of
    ;;struct instances  representing recordized  code; return  2 values:
    ;;the processed X*, a list struct instances of type VAR representing
    ;;the free variables referenced by X*.
    ;;
    (if (null? X*)
	(values '() '())
      (let-values (((a a-free) (E-known  ($car X*)))
		   ((d d-free) (E-known* ($cdr X*))))
	(values (cons a d)
		(union a-free d-free)))))

  (define (Rator x)
    ;;Invoked only when the parameter OPTIMIZE-CP is set to true.
    ;;
    ;;FIXME Does this look as  an optimization to anyone?  (Marco Maggi;
    ;;Oct 13, 2012)
    ;;
    (struct-case x
      ((var)
       (values x (list x)))
      ;;((known x t)
      ;; (let-values (((x free) (Rator x)))
      ;;   (values (make-known x t) free)))
      (else
       (E x))))

  #| end of module: convert closures |# )


(module (optimize-closures/lift-codes)

  (define who 'optimize-closures/lift-codes)

  (define all-codes
    (make-parameter #f))

  (define (optimize-closures/lift-codes X)
    (parametrise ((all-codes '()))
      ;;(when (optimize-cp)
      ;;  (printf "BEFORE\n")
      ;;  (parameterize ((pretty-width 200))
      ;;    (pretty-print (unparse-recordized-code X))))
      (let* ((X^ (E X))
	     (V  (make-codes (all-codes) X^)))
	;;(when (optimize-cp)
	;;  (printf "AFTER\n")
	;;  (parameterize ((pretty-width 200))
	;;    (pretty-print (unparse-recordized-code V))))
	V)))

  (module (E)

    (define (E x)
      (struct-case x
	((constant)
	 x)

	((var)
	 (get-forward! x))

	((primref)
	 x)

	((bind lhs* rhs* body)
	 ($for-each/stx unset! lhs*)
	 (let ((rhs*^ ($map/stx E rhs*)))
	   ($for-each/stx copy-subst! lhs* rhs*^)
	   (let ((body^ (E body)))
	     ($for-each/stx unset! lhs*)
	     (make-bind lhs* rhs*^ body^))))

	((fix lhs* rhs* body)
	 (%do-fix lhs* rhs* body))

	((conditional test conseq altern)
	 (make-conditional (E test) (E conseq) (E altern)))

	((seq e0 e1)
	 (make-seq (E e0) (E e1)))

	((forcall op rand*)
	 (make-forcall op ($map/stx E rand*)))

	((funcall rator rand*)
	 (make-funcall (E-known rator) ($map/stx E-known rand*)))

	((jmpcall label rator rand*)
	 (make-jmpcall label (E rator) ($map/stx E rand*)))

	(else
	 (error who "invalid expression" (unparse-recordized-code x)))))

    (define (E-known x)
      (struct-case x
	((known expr type)
	 (make-known (E expr) type))
	(else
	 (E x))))

    #| end of module: E |# )

  (module (%do-fix)

    (define (%do-fix lhs* rhs* body)
      ($for-each/stx unset! lhs*)
      ;;Trim the free lists first; after init.
      (let ((free** (map (lambda (lhs rhs)
			   ;;Remove self also.
			   (remq lhs (%trim-free (closure-free* rhs))))
		      lhs* rhs*)))
	(define-struct node
	  (name code deps whacked free wk?))
	(let ((node* (map (lambda (lhs rhs)
			    (let ((n (make-node lhs (closure-code rhs)
						'() #f '()
						(closure-well-known? rhs))))
			      (set-subst! lhs n)
			      n))
		       lhs* rhs*)))
	  ;;If X is free in Y, then whenever X becomes a non-combinator, Y
	  ;;also   becomes  a   non-combinator.   Here,   we  mark   these
	  ;;dependencies.
	  (for-each
	      (lambda (my-node free*)
		(for-each (lambda (fvar)
			    (cond ((get-subst fvar)
				   ;;One of ours.
				   => (lambda (her-node)
					(set-node-deps! her-node
							(cons my-node (node-deps her-node)))))
				  (else ;;; not one of ours
				   (set-node-free! my-node
						   (cons fvar (node-free my-node))))))
		  free*))
	    node* free**)
	  ;;Next, we go  over the list of  nodes, and if we  find one that
	  ;;has any free  variables, we know it's a  non-combinator, so we
	  ;;whack it and add it to all of its dependents.
	  (let ()
	    (define (%process-node x)
	      (when (cond ((null? (node-free x))
			   #f)
			  ;; ((and (node-wk? x)
			  ;;       (null? (cdr (node-free x))))
			  ;;  #f)
			  (else
			   #t))
		(unless (node-whacked x)
		  (set-node-whacked! x #t)
		  (for-each (lambda (y)
			      (set-node-free! y (cons (node-name x) (node-free y)))
			      (%process-node y))
		    (node-deps x)))))
	    ($for-each/stx %process-node node*))
	  ;;Now those that have free variables are actual closures.  Those
	  ;;with no free variables are actual combinators.
	  (let ((rhs* (map (lambda (node)
			     (let ((wk?  (node-wk?  node))
				   (name (node-name node))
				   (free (node-free node)))
			       (let ((closure (make-closure (node-code node) free wk?)))
				 (cond ((null? free)
					(set-subst! name closure))
				       ((and (null? (cdr free)) wk?)
					(set-subst! name closure))
				       (else
					(unset! name)))
				 closure)))
			node*)))
	    (for-each (lambda (lhs^ closure)
			(let* ((lhs  (get-forward! lhs^))
			       (free (filter var?
				       (remq lhs (%trim-free (closure-free* closure))))))
			  (set-closure-free*! closure free)
			  (set-closure-code!  closure (lift-code lhs (closure-code  closure)
								 (closure-free* closure)))))
	      lhs* rhs*)
	    (let ((body^ (E body)))
	      (let loop ((lhs* lhs*)
			 (rhs* rhs*)
			 (l*   '())
			 (r*   '()))
		(if (null? lhs*)
		    (if (null? l*)
			body^
		      (make-fix l* r* body^))
		  (let ((lhs (car lhs*))
			(rhs (car rhs*)))
		    (if (get-subst lhs)
			(begin
			  (unset! lhs)
			  (loop (cdr lhs*) (cdr rhs*) l* r*))
		      (loop (cdr lhs*) (cdr rhs*) (cons lhs l*) (cons rhs r*)))))))))))

    (define (%trim-free ls)
      (cond ((null? ls)
	     '())
	    ((get-forward! (car ls))
	     => (lambda (what)
		  (let ((rest (%trim-free (cdr ls))))
		    (struct-case what
		      ((closure)
		       rest)
		      ((var)
		       (if (memq what rest)
			   rest
			 (cons what rest)))
		      (else
		       (error who "invalid value in %trim-free" what))))))
	    (else
	     (cons (car ls) (%trim-free (cdr ls))))))

    #| end of module: %do-fix |# )

  (define (get-forward! x)
    (when (eq? x 'q)
      (error who "BUG: circular dep"))
    (let ((y (get-subst x)))
      (cond ((not y)
	     x)
	    ((var? y)
	     ;;By  temporarily setting  the subst  to 'Q  we can  detect
	     ;;circular references while recursing into GET-FORWARD!.
	     (set-subst! x 'q)
	     (let ((y (get-forward! y)))
	       ;;Restore the subst to its proper value.
	       (set-subst! x y)
	       y))
	    ((closure? y)
	     (let ((free* (closure-free* y)))
	       (cond ((null? free*)
		      y)
		     ((null? (cdr free*))
		      ;;By temporarily  setting the  subst to 'Q  we can
		      ;;detect circular references  while recursing into
		      ;;GET-FORWARD!.
		      (set-subst! x 'q)
		      (let ((y (get-forward! ($car free*))))
			;;Restore the subst to its proper value.
			(set-subst! x y)
			y))
		     (else
		      y))))
	    (else
	     x))))

  (define (lift-code cp code free*)
    ;;CP is a struct instance of type VAR to which a closure is bound.
    ;;
    ;;CODE  is a  a struct  instance  of type  CLAMBDA representing  the
    ;;closure's implementation.
    ;;
    ;;FREE* is a  list of struct instances of type  VAR representing the
    ;;free variables referenced by the closure.
    ;;
    ;;Return a struct instance of type CODE-LOC holding the label of the
    ;;argument CODE.
    ;;
    (define-inline (prepend-to-all-codes obj)
      (all-codes (cons obj (all-codes))))
    (struct-case code
      ((clambda label clause* cp.dropped free*.dropped name)
       (let ((clause* (map (lambda (clause)
			     (struct-case clause
			       ((clambda-case info body)
				($for-each/stx unset! (case-info-args info))
				(make-clambda-case info (E body)))))
			clause*)))
	 (begin0
	     (make-code-loc label)
	   (prepend-to-all-codes (make-clambda label clause* cp free* name)))))))

  (module (unset! set-subst! get-subst copy-subst!)
    (define-struct prop
      (val))

    (define (unset! x)
      #;(assert (var? x))
      (set-var-index! x #f))

    (define (set-subst! x v)
      #;(assert (var? x))
      (set-var-index! x (make-prop v)))

    (define (copy-subst! lhs rhs)
      #;(assert (var? lhs))
      (cond ((and (var? rhs)
		  (var-index rhs))
	     => (lambda (v)
		  (set-var-index! lhs (if (prop? v) v #f))))
	    (else
	     (set-var-index! lhs #f))))

    (define (get-subst x)
      #;(assert (var? x))
      (struct-case (var-index x)
	((prop v)	v)
	(else		#f)))

    #| end of module |# )

  ;;Commented out because unused.  (Marco Maggi; Oct 13, 2012)
  ;;
  ;; (define (combinator? x)
  ;;   (struct-case x
  ;;     ((closure code free*)
  ;;      (null? free*))
  ;;     (else
  ;;      #f)))

  #| end of module: optimize-closures/lift-codes |# )


;;;; definitions for assembly code generation
;;
;;The  folowing constants  definitions must  be  kept in  sync with  the
;;definitions  in   the  C  language  header   files  "internals.h"  and
;;"vicare.h".
;;

(module (wordsize)
  (import (vicare include))
  (include "ikarus.config.ss"))

(define wordshift
  (case-word-size
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
(define bwp-object			#x8F)

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
;;; closures

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
;;     livemask-bytes		;array of bytes
;;     framesize		;data word, a "long"
;;     rp_offset		;data word, a fixnum
;;     multi-value-rp		;data word, assembly label
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
  (case-word-size
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

  (define max-bitcount-in-fixnum-binary-representation
    (- (* wordsize 8) fx-shift))

  (define intbits
    ($fx* wordsize 8))
  (define fxbits
    ($fx- intbits fx-shift))
  (define t
    (expt 2 ($fx- fxbits 1)))
  (define least-signed-machine-word
    (- t))
  (define greatest-signed-machine-word
    (- t 1))

  (define (fx? x)
    ;;Return true if X is an exact signed integer that fits in a machine
    ;;word.
    (and (or (fixnum? x)
	     (bignum? x))
	 (<= least-signed-machine-word x greatest-signed-machine-word)))

  #| end od module |# )

(define (primref->symbol op)
  ;;Given the  symbol, which must  be the  name of a  primitive function
  ;;exported by the boot image,
  ;;
  (define who 'primref->symbol)
  (with-arguments-validation (who)
      ((symbol	op))
    (cond (((current-primitive-locations) op)
	   => (lambda (x)
		(with-arguments-validation (who)
		    ((symbol	x))
		  x)))
	  (else
	   (error who "BUG: primitive missing from makefile.sps" op)))))

;;(define (primref-loc op)
;;  (mem off-symbol-record-proc (obj (primref->symbol op))))


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
  ;;negated.  Example: -2 = 2 arguments.
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

  (define-auxiliary-syntaxes public-function)
  (define-auxiliary-syntaxes entry-point-label)
  (define-auxiliary-syntaxes number-of-free-variables)
  (define-auxiliary-syntaxes code-annotation)
  (define-auxiliary-syntaxes definitions)
  (define-auxiliary-syntaxes local-labels)
  (define-auxiliary-syntaxes assembly)

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
		     (syntax-error stx "cannot use label before it is defined")))
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
    ;;NOTE This is for debugging purposes,  they are not used by Vicare;
    ;;see   the  primitive   operations  $MAKE-ANNOTATED-PROCEDURE   and
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
    ;;this routine  iterates the list  of arguments pushes on  the stack
    ;;the Scheme objects: ?ARG2, ?ARG3, ?ARG4.
    ;;
    ;;Before:
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
      ;;... there are no function call arguments to push on the stack.
      (je (label L_apply_done))

      (label L_apply_loop)
      ;;Load in ECX the car.
      (movl (mem off-car ebx) ecx)
      ;;Load in EBX the cdr.
      (movl (mem off-cdr ebx) ebx)
      ;;Move the car at offset EAX from the frame pointer.
      (movl ecx (mem fpr eax))
      ;;Decrement EAX  by the  word size:  offset of  the next
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
    ;;execution of a previously saved Scheme continuation; it is used to
    ;;implement the  continuation function  generated by CALL/CC;  it is
    ;;the entry point  associated to the closure object  returned by the
    ;;primitive operation $FRAME->CONTINUATION.
    ;;
    ;;Upon entering this subroutine:
    ;;
    ;;**The  Process  Control  Register  (PCR) must  reference  the  PCB
    ;;  structure.
    ;;
    ;;**The Frame Pointer  Register (FPR) must reference the  top of the
    ;;  Scheme stack.
    ;;
    ;;**The Closure  Pointer Register (CPR)  must hold a reference  to a
    ;;  closure object,  whose first slot for free  variables contains a
    ;;  reference to the continuation object to resume.  Such closure is
    ;;  an  internal component of  the continuation function  created by
    ;;  CALL/CC.
    ;;
    ;;Before  resuming  execution: we  want  to  set the  Frame  Pointer
    ;;Register to the address of the  highest machine word on the Scheme
    ;;stack; such memory location contains  the address of the underflow
    ;;handler: the assembly label  "ik_underflow_handler" defined in the
    ;;file "ikarus-enter.S".
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
      ;;Move in EBX the reference to the continuation object.
      (movl (mem off-closure-data cpr) ebx)
      ;;Move  the   reference  to  continuation  object   in  the  field
      ;;"pcb->next_k" (overwriting the old value!!!).
      (movl ebx (mem pcb-next-continuation pcr))
      ;;Move in EBX the field "pcb->frame_base".
      (movl (mem pcb-frame-base pcr) ebx)
      ;;Dispatch according to  the number of arguments to  return to the
      ;;continuation.
      (cmpl (int (argc-convention 1)) eax)
      (jg (label L_cont_zero_args)) ;jump if greater, more than one arg
      (jl (label L_cont_mult_args)) ;jump if less, less than one arg

      ;;We give one argument to  the continuation.  The situation on the
      ;;stack when arriving here is:
      ;;
      ;;         high memory
      ;;   |                      | <-- pcb->frame_base = EBX
      ;;   |----------------------|
      ;;   | ik_underflow_handler | = highest machine word on the stack
      ;;   |----------------------|
      ;;   |         ...          |
      ;;   |----------------------|
      ;;   |  old return address  | <-- Frame Pointer Register (FPR)
      ;;   |----------------------|
      ;;   |     return value     |
      ;;   |----------------------|
      ;;   |                      |
      ;;          low memory
      ;;
      ;;and "pcb->next_k" references the  continuation object we must go
      ;;back to.
      ;;
      (label L_cont_one_arg)
      ;;Load in EAX the the return value.
      (movl (mem (fx- 0 wordsize) fpr) eax)
      ;;Load   in   the   Frame    Pointer   Register   the   value   of
      ;;"pcb->frame_base".
      (movl ebx fpr)
      ;;Decrement  Frame Pointer  Register  by a  wordsize,  so that  it
      ;;contains the address of the  highest machine word in the current
      ;;Scheme stack segment.
      (subl (int wordsize) fpr)
      ;;Jump to the underflow handler.   The situation on the stack when
      ;;arriving here is:
      ;;
      ;;         high memory
      ;;   |                      | <-- pcb->frame_base
      ;;   |----------------------|
      ;;   | ik_underflow_handler | <-- Frame Pointer Register (%esp)
      ;;   |----------------------|
      ;;   |                      |
      ;;          low memory
      ;;
      ;;EAX  still contains  the encoded  1 as  number of  arguments and
      ;;"pcb->next_k" references the continuation object we must go back
      ;;to.
      ;;
      (ret)

      ;;We give  zero arguments to  the continuation.  The  situation on
      ;;the stack when arriving here is:
      ;;
      ;;         high memory
      ;;   |                      | <-- pcb->frame_base = EBX
      ;;   |----------------------|
      ;;   | ik_underflow_handler | = highest machine word on the stack
      ;;   |----------------------|
      ;;   |         ...          |
      ;;   |----------------------|
      ;;   |  old return address  | <-- Frame Pointer Register (FPR)
      ;;   |----------------------|
      ;;   |                      |
      ;;          low memory
      ;;
      ;;EAX  still contains  the encoded  0 as  number of  arguments and
      ;;"pcb->next_k" references the continuation object we must go back
      ;;to.
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
      ;;   | ik_underflow_handler | <-- Frame Pointer Register (%esp)
      ;;   |----------------------|
      ;;   |                      |
      ;;          low memory
      ;;
      ;;EAX  still  contains  the  zero   as  number  of  arguments  and
      ;;"pcb->next_k" references the continuation object we must go back
      ;;to.
      ;;
      (jmp (mem disp-multivalue-rp ebx))

      ;;We  give  more  than  one argument  to  the  continuation.   The
      ;;situation on the stack when arriving here is:
      ;;
      ;;         high memory
      ;;   |                      | <-- pcb->frame_base = EBX
      ;;   |----------------------|
      ;;   | ik_underflow_handler | = highest machine word on the stack
      ;;   |----------------------|
      ;;   |         ...          |
      ;;   |----------------------|
      ;;   |  old return address  | <-- Frame Pointer Register (%esp)
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
      ;;and EAX still contains the  encoded number of arguments (greater
      ;;than one).
      ;;
      (label L_cont_mult_args)
      ;;Decrement EBX by a wordsize, so  that it contains the address of
      ;;the highest machine word in the current Scheme stack segment.
      (subl (int wordsize) ebx)
      ;;If the current frame pointer is already at the base of the stack
      ;;(FPR == pcb->farme_base - wordsize): we  do not need to copy the
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
      ;;EAX  still   contains  the  encoded  number   of  arguments  and
      ;;"pcb->next_k" references the continuation object we must go back
      ;;to.
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
      ;;Store "pcb->frame_base" in the Frame Pointer Register.
      (movl ebx fpr)
      ;;Load in EBX  the machine word from the address  in EBX.  Now EBX
      ;;== ik_underflow_handler.
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
      ;;   |                      |
      ;;          low memory
      ;;
      ;;EAX  still   contains  the  encoded  number   of  arguments  and
      ;;"pcb->next_k" references the continuation object we must go back
      ;;to.
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
    ;;   |     return address   | <-- Frame Pointer Register (%esp)
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
      (movl cpr (mem ($fx- 0 wordsize) fpr))
      ;;Decode  the incorrect  number  of  arguments, so  that  it is  a
      ;;non-negative fixnum.
      (negl eax)
      ;;Store on the  stack the incorrect number of  arguments as second
      ;;argument to the call to $INCORRECT-ARGS-ERROR-HANDLER.
      (movl eax (mem ($fx- 0 ($fx* 2 wordsize)) fpr))
      ;;Load in  the Closure Pointer  Register (CPR) a reference  to the
      ;;symbol  object  containing a  reference  to  the closure  object
      ;;implementing the function $INCORRECT-ARGS-ERROR-HANDLER.
      (movl (obj (primref->symbol '$incorrect-args-error-handler)) cpr)
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

    ;;SL-MV-ERROR-RP-LABEL This subroutine is called whenever an attempt
    ;;to return zero  or 2 or more  values to a single  value context is
    ;;performed, and  the receiving  function just  wants to  ignore the
    ;;error.
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
      ;;Load in  the Closure Pointer  Register (CPR) a reference  to the
      ;;symbol  object  containing a  reference  to  the closure  object
      ;;implementing the function $MULTIPLE-VALUES-ERROR.
      (movl (obj (primref->symbol '$multiple-values-error)) cpr)
      ;;Load in the Closure Pointer  Register a reference to the closure
      ;;object implementing the function $MULTIPLE-VALUES-ERROR.
      (movl (mem off-symbol-record-proc cpr) cpr)
      ;;Fetch a binary  code address from the  closure object referenced
      ;;by the CPR (Closure Pointer Register) and jump directly there.
      (tail-indirect-cpr-call)
      ))

;;; --------------------------------------------------------------------

    ;;Implementation of  the function  VALUES.  The arguments  to VALUES
    ;;are the return values of VALUES.  When arriving from a call:
    ;;
    ;;   (values ret-val-0 ret-val-1 ret-val-2)
    ;;
    ;;here the situation on the Scheme stack is:
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

      ;;Return  many values.   Retrieve  the address  of the  multivalue
      ;;assembly label by adding DISP-MULTIVALUE-RP to "return address".
      (label L_values_many_values)
      (movl (mem 0 fpr) ebx)
      (jmp (mem disp-multivalue-rp ebx))

      ;;Return a  single value.  Store  in EAX the single  return value,
      ;;then "ret".
      (label L_values_one_value)
      (movl (mem (fx- 0 wordsize) fpr) eax)
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
      (movl (mem (fx- 0 wordsize) fpr) ebx)
      (movl ebx cpr)
      (andl (int closure-mask) ebx)
      (cmpl (int closure-tag) ebx)
      (jne (label SL_nonprocedure))
      ;;
      ;;4..The producer is  called with zero arguments: load  in EAX the
      ;;   fixnum zero.
      ;;
      ;;5..Call the producer closure in  CPR executing a "call" assembly
      ;;   instruction.   The situation  on the  stack right  before the
      ;;   "call" instruction is:
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
      (movl (int (argc-convention 0)) eax)
      (compile-call-frame
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
      ;;Store in EBX a reference to the consumer closure object.
      (movl ebx cpr)
      ;;Check that EBX actually contains  a reference to closure object;
      ;;else jump to the appropriate error handler.
      (andl (int closure-mask) ebx)
      (cmpl (int closure-tag) ebx)
      (jne (label SL_nonprocedure))
      ;;Store the  returned value on  the stack, right below  the return
      ;;address.
      (movl eax (mem (fx- 0 wordsize) fpr))
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
      (movl cpr (mem (fx- 0 wordsize) fpr))
      ;;Retrieve the reference  of the error handler closure  and put it
      ;;into CPR.
      (movl (obj (primref->symbol '$apply-nonprocedure-error-handler)) cpr)
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
      (movl cpr (mem (fx- 0 wordsize) fpr))
      ;;Decode the  number of  arguments, so that  it is  a non-negative
      ;;fixnum.
      (negl eax)
      ;;Put on the stack the incorrect number of arguments as fixnum, as
      ;;second argument to $INCORRECT-ARGS-ERROR-HANDLER.
      (movl eax (mem (fx- 0 (fx* 2 wordsize)) fpr))
      ;;Load in  the Closure Pointer  Register (CPR) a reference  to the
      ;;symbol  object  containing a  reference  to  the closure  object
      ;;implementing the function $INCORRECT-ARGS-ERROR-HANDLER.
      (movl (obj (primref->symbol '$incorrect-args-error-handler)) cpr)
      ;;Load in the Closure Pointer  Register a reference to the closure
      ;;object implementing the function $INCORRECT-ARGS-ERROR-HANDLER.
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

(include/verbose "ikarus.compiler.altcogen.ss")


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

    ((clambda label cls* cp free)
     ;;FIXME Should we print more fields?  (Marco Maggi; Oct 11, 2012)
     `(clambda (label: ,label)
	       (cp:    ,(unparse-recordized-code cp))
	       (free:  ,(if free
			    (map unparse-recordized-code free)
			  free))
	       ,@(map unparse-recordized-code cls*)))

    ((closure code free* wk?)
     `(closure ,(if wk? '(well-known: #t) '(well-known: #f))
	       (freevars: ,(map unparse-recordized-code free*))
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

    ((mvcall prod cons)
     `(mvcall ,(unparse-recordized-code prod) ,(unparse-recordized-code cons)))

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


(module (unparse-recordized-code/pretty)
  ;;Unparse the struct  instance X (representing recordized  code in the
  ;;core  language  already processed  by  the  compiler) into  a  human
  ;;readable symbolic expression  to be used when printing  to some port
  ;;for miscellaneous debugging purposes.
  ;;
  ;;This module  attempts to unparse  recordized code and  reconstruct a
  ;;Scheme-like  symbolic  expression;  the  returned  sexp  does  *not*
  ;;exactly represent the input.
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
  (import (only (ikarus system $symbols)
		$symbol-string))

  (define (unparse-recordized-code/pretty x)
    ;;
    ;;A lot  of functions are nested  here because they make  use of the
    ;;closure "Var", which has internal state.
    ;;
    ;;*NOTE* Being  that this function  is used only when  debugging: it
    ;;makes no sense to use unsafe operations: LET'S KEEP IT SAFE!!!
    ;;
    (define (E x)
      (struct-case x
	((constant c)
	 `(quote ,c))

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

	((closure code free* well-known?)
	 `(closure ,(E code)
		   ,(map E free*)
		   ,well-known?))

	((primcall op arg*)
	 (cons op (%map-in-order E arg*)))

	((funcall rator rand*)
	 (let ((rator (E rator)))
	   (cons rator (%map-in-order E rand*))))

	((forcall rator rand*)
	 `(foreign-call ,rator . ,(%map-in-order E rand*)))

	((jmpcall label op rand*)
	 `(jmpcall ,label
		   ,(E op)
		   ,(map E rand*)))

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

	((codes clambdas body)
	 `(codes ,(map E clambdas)
		 ,(E body)))

	(else x)))

    (module (Var)
      ;;Given a struct instance X of type PRELEX or VAR, identifying the
      ;;location of  a binding:  return a  symbol representing  a unique
      ;;name for the binding.  The map between structures and symbols is
      ;;cached in a hash table.
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
	;;Map PRELEX  and VAR structures  to already built  binding name
	;;symbols.
	(make-eq-hashtable))
      (define T
	;;Map binding pretty string names to indexes.
	(make-hashtable string-hash string=?))
      (define (Var x)
	(or (hashtable-ref H x #f)
	    (struct-case x
	      ((prelex x.name)
	       (%build-name x.name))
	      ((var x.name)
	       (%build-name x.name))
	      (else
	       x
	       #;(error who "expected struct of type PRELEX or VAR" x)))))

      (define (%build-name x.name)
	(let* ((name ($symbol-string x.name))
	       (N    (hashtable-ref T name 0)))
	  (hashtable-set! T name (+ N 1))
	  (let ((sym (string->symbol (string-append name "_" (number->string N)))))
	    (hashtable-set! H x sym)
	    sym)))

      #| end of module: Var |# )

    (define(%do-seq e0 e1)
      (cons 'begin
	    ;;Here we flatten nested SEQ  instances into a unique output
	    ;;SEQ form.
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
	  ;;The loop below  is like MAP but for improper  lists: it maps Var
	  ;;over the improper list X.
	  (let recur ((A (car x))
		      (D (cdr x)))
	    (if (null? D)
		(Var A)
	      (let ((A (Var A)))
		(cons A (recur (car D) (cdr D))))))))

      #| end of module: E-clambda |# )

    (E x))

;;; --------------------------------------------------------------------

  (define (%map-in-order f ls)
    ;;This version  of MAP imposes an  order to the application  of F to
    ;;the items in LS.
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

  #| end of module: unparse-recordized-code/pretty |# )


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'assemble-sources 'scheme-indent-function 1)
;; eval: (put 'define-structure 'scheme-indent-function 1)
;; eval: (put 'make-conditional 'scheme-indent-function 2)
;; eval: (put 'struct-case 'scheme-indent-function 1)
;; End:
