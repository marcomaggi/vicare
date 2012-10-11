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


#!r6rs
(library (ikarus.compiler)
  (export
    compile-core-expr-to-port		assembler-output
    optimize-cp				optimizer-output
    current-primitive-locations
    eval-core				current-core-eval
    compile-core-expr
    expand				expand/optimize
    expand/scc-letrec
    cp0-effort-limit			cp0-size-limit
    optimize-level
    perform-tag-analysis		tag-analysis-output
    strip-source-info			generate-debug-calls
    current-letrec-pass)
  (import (except (ikarus)
		  compile-core-expr-to-port		assembler-output
		  optimize-cp				optimizer-output
		  current-primitive-locations
		  eval-core
		  optimize-level
		  debug-optimizer
		  fasl-write
		  cp0-size-limit			cp0-effort-limit
		  expand				expand/optimize
		  expand/scc-letrec
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
    (ikarus.fasl.write)
    (ikarus.intel-assembler)
    (only (vicare syntactic-extensions)
	  define-inline
	  case-symbols
	  case-fixnums
	  case-word-size)
    (vicare arguments validation))


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
	 #'(begin ?body0 ?body ...))

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
    #;(define (%make-fmt ctxt)
      (lambda (template-str . args)
        (datum->syntax ctxt
		       (string->symbol
			(apply format template-str (map syntax->datum args))))))
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
	   #;(((PRED MAKER (GETTER ...) (SETTER ...))
	     (let ((fmt (%make-fmt #'?name)))
	       (list (fmt "~s?" #'?name)
		     (fmt "make-~s" #'?name)
		     (map (lambda (x)
			    (fmt "~s-~s" #'?name x))
		       #'(?field-without-default ... ?field-with-default ...))
		     (map (lambda (x)
			    (fmt "set-~s-~s!" #'?name x))
		       #'(?field-without-default ... ?field-with-default ...))))))
	   ((PRED		(%format-id #'?name "~s?" #'?name))
	    (MAKER		(%format-id #'?name "make-~s" #'?name))
	    ((GETTER ...)	(map (lambda (x)
				       (%format-id #'?name "~s-~s" #'?name x))
				  #'(?field-without-default ... ?field-with-default ...)))
	    ((SETTER ...)	(map (lambda (x)
				       (%format-id #'?name "set-~s-~s!" #'?name x))
				  #'(?field-without-default ... ?field-with-default ...))))
         #'(module (?name PRED GETTER ... SETTER ... MAKER)
             (module private
	       (?name PRED GETTER ... SETTER ... MAKER)
	       (define-struct ?name
		 (?field-without-default ... ?field-with-default ...)))
             (module (MAKER)
               (define (MAKER ?field-without-default ...)
                 (import private)
                 (MAKER ?field-without-default ... ?default ...)))
             (module (?name PRED GETTER ... SETTER ...)
               (import private)))))
      )))


;;;; helper functions

(define strip-source-info
  (make-parameter #f))

(define generate-debug-calls
  ;;Set to true when the option "--debug" is used on the command line of
  ;;the executable "vicare"; else set to #f.
  ;;
  (make-parameter #f))

(define-inline (singleton x)
  ;;Wrap  the object  X  into a  unique  container, so  that  it can  be
  ;;compared with EQ?.
  ;;
  (list x))

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


;;;; struct types used to represent code in the core language

;;Instances of  this type are  stored in  the property lists  of symbols
;;representing  binding names;  this way  we  can just  send around  the
;;binding name symbol to represent some lexical context informations.
;;
(define-structure (prelex name operand)
  ((source-referenced?   #f)
   (source-assigned?     #f)
		;Boolean,  true  when  the  binding  has  been  used  as
		;left-hand side in a SET! form.
   (residual-referenced? #f)
   (residual-assigned?   #f)
   (global-location      #f)
		;When this  binding describes a top  level binding, this
		;field  is set  to  a unique  gensym  associated to  the
		;binding; else this field is #f.
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
;;inside a core language BEGIN.  We can think of the form:
;;
;;   (begin ?b0 ?b1 ?last)
;;
;;as:
;;
;;   (begin ?b0 (begin ?b1 (begin ?last)))
;;
;;and it becomes the nested hierarchy:
;;
;;   (make-seq (recordize ?b0)
;;             (make-seq (recordize ?b1) (recordize ?last)))
;;
(define-struct seq
  (e0
		;A struct  instance representing  the first form  in the
		;sequence.
   e1
		;A  struct instance  representing the  last form  in the
		;sequence or a struct  instance of type SEQ representing
		;a subsequence.
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
;;side references a local binding.
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
;;  - the field OP is the result of calling:
;;
;;      (make-primref '$init-symbol-value!)
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
		;Initialised to #f.
   free
		;Initialised to #f.
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
		;A unique gensym for this CASE-LAMBDA clause.
   args
		;A list of struct  instances of type PRELEX representing
		;the ?FORMALS as follows:
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
   proper
		;A boolean: true if ?FORMALS  is a proper list, false if
		;?FORMALS is a symbol or improper list.
   ))


;;;; struct types

(define-struct code-loc
  (label))

(define-struct foreign-label
  (label))

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

(define-struct primcall
  (op arg*))

(define-struct interrupt-call
  (test handler))

(define-struct bind
  (lhs*
   rhs*
   body
   ))

(define-struct fix
  (lhs*
   rhs*
   body
   ))

(define-struct closure
  (code
   free*
   well-known?
   ))

(define-struct jmpcall
  (label
   op
   rand*
   ))

(define-struct codes
  (list
   body
   ))

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

(define-struct fvar
  (idx))

(define-struct object
  (val))

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
      ;;If  the left-hand  side  references a  local  binding: return  a
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
      ;;by the boot image?
      ;;
      ;;Not sure: the SYSTEM-NAME call below will fail with an assertion
      ;;violation  if  NAME is  not  a  unique  symbol associated  to  a
      ;;function  exported by  the boot  image.  (Marco  Maggi; Oct  11,
      ;;2012)
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


(define (unparse x)
  (define (E-args proper x)
    (if proper
        (map E x)
        (let f ((a (car x)) (d (cdr x)))
          (cond
            ((null? d) (E a))
            (else (cons (E a) (f (car d) (cdr d))))))))
  (define (E x)
    (struct-case x
      ((constant c) `(quote ,c))
      ((known x t) `(known ,(E x) ,(T:description t)))
      ((code-loc x) `(code-loc ,x))
      ((var x) (string->symbol (format ":~a" x)))
      ((prelex name) (string->symbol (format ":~a" name)))
      ((primref x) x)
      ((conditional test conseq altern)
       `(if ,(E test) ,(E conseq) ,(E altern)))
      ((interrupt-call e0 e1)
       `(interrupt-call ,(E e0) ,(E e1)))
      ((primcall op arg*) `(,op . ,(map E arg*)))
      ((bind lhs* rhs* body)
       `(let ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body)))
      ((recbind lhs* rhs* body)
       `(letrec ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body)))
      ((rec*bind lhs* rhs* body)
       `(letrec* ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body)))
      ;((library-recbind lhs* loc* rhs* body)
      ; `(letrec ,(map (lambda (lhs loc rhs) (list (E lhs) loc (E rhs)))
      ;                lhs* loc* rhs*)
      ;    ,(E body)))
      ((fix lhs* rhs* body)
       `(fix ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body)))
      ((seq e0 e1)
       (let ()
         (define (f x ac)
           (struct-case x
             ((seq e0 e1) (f e0 (f e1 ac)))
             (else (cons (E x) ac))))
         (cons 'begin (f e0 (f e1 '())))))
      ((clambda-case info body)
       `( ;   label: ,(case-info-label info)
         ,(E-args (case-info-proper info) (case-info-args info))
         ,(E body)))
      ((clambda g cls* cp free)
       `(clambda (label: ,g) ; cp: ,(E cp) ) ;free: ,(map E free))
           ,@(map E cls*)))
      ((clambda label clauses free)
       `(code ,label . ,(map E clauses)))
      ((closure code free* wk?)
       `(closure ,@(if wk? '(wk) '()) ,(E code) ,(map E free*)))
      ((codes list body)
       `(codes ,(map E list)
          ,(E body)))
      ((funcall rator rand*) `(funcall ,(E rator) . ,(map E rand*)))
      ((jmpcall label rator rand*)
       `(jmpcall ,label ,(E rator) . ,(map E rand*)))
      ((forcall rator rand*) `(foreign-call ,rator . ,(map E rand*)))
      ((assign lhs rhs) `(set! ,(E lhs) ,(E rhs)))
      ((return x) `(return ,(E x)))
      ((new-frame base-idx size body)
       `(new-frame (base: ,base-idx)
                   (size: ,size)
          ,(E body)))
      ((frame-var idx)
       (string->symbol (format "fv.~a" idx)))
      ((cp-var idx)
       (string->symbol (format "cp.~a" idx)))
      ((save-cp expr)
       `(save-cp ,(E expr)))
      ((eval-cp check body)
       `(eval-cp ,check ,(E body)))
      ((call-cp call-convention label save-cp? rp-convention base-idx arg-count live-mask)
       `(call-cp (conv: ,call-convention)
                 (label: ,label)
                 (rpconv: ,(if (symbol? rp-convention)
                               rp-convention
                               (E rp-convention)))
                 (base-idx: ,base-idx)
                 (arg-count: ,arg-count)
                 (live-mask: ,live-mask)))
      ((tailcall-cp convention label arg-count)
       `(tailcall-cp ,convention ,label ,arg-count))
      ((foreign-label x) `(foreign-label ,x))
      ((mvcall prod cons) `(mvcall ,(E prod) ,(E cons)))
      ((fvar idx) (string->symbol (format "fv.~a" idx)))
      ((nfv idx) 'nfv)
      ((locals vars body) `(locals ,(map E vars) ,(E body)))
      ((asm-instr op d s)
       `(asm ,op ,(E d) ,(E s)))
      ((disp s0 s1)
       `(disp ,(E s0) ,(E s1)))
      ((nframe vars live body) `(nframe ;(vars: ,(map E vars))
                                        ;(live: ,(map E live))
                                  ,(E body)))
      ((shortcut body handler)
       `(shortcut ,(E body) ,(E handler)))
      ((ntcall target valuw args mask size)
       `(ntcall ,target ,size))
      (else
       (if (symbol? x)
           x
           "#<unknown>"))))
  (E x))


(define (unparse-pretty x)
  (define n 0)
  (define h (make-eq-hashtable))
  (define (Var x)
    (or (hashtable-ref h x #f)
        (let ((v (string->symbol (format "~a_~a" (prelex-name x) n))))
          (hashtable-set! h x v)
          (set! n (+ n 1))
          v)))
  (define (map f ls)
    (cond
      ((null? ls) '())
      (else
       (let ((a (f (car ls))))
         (cons a (map f (cdr ls)))))))
  (define (E-args proper x)
    (if proper
        (map Var x)
        (let f ((a (car x)) (d (cdr x)))
          (cond
            ((null? d) (Var a))
            (else
             (let ((a (Var a)))
               (cons a (f (car d) (cdr d)))))))))
  (define (clambda-clause x)
    (struct-case x
      ((clambda-case info body)
       (let ((args (E-args (case-info-proper info) (case-info-args info)) ))
         (list args (E body))))))
  (define (build-let b* body)
    (cond
      ((and (= (length b*) 1)
            (pair? body)
            (or (eq? (car body) 'let*)
                (and (eq? (car body) 'let)
                     (= (length (cadr body)) 1))))
       (list 'let* (append b* (cadr body)) (caddr body)))
      (else
       (list 'let b* body))))
  (define (E x)
    (struct-case x
      ((constant c) `(quote ,c))
      ((prelex) (Var x))
      ((primref x) x)
      ((known x t) `(known ,(E x) ,(T:description t)))
      ((conditional test conseq altern)
       (cons 'if (map E (list test conseq altern))))
      ((primcall op arg*) (cons op (map E arg*)))
      ((bind lhs* rhs* body)
       (let* ((lhs* (map Var lhs*))
              (rhs* (map E rhs*))
              (body (E body)))
         (import (only (ikarus) map))
         (build-let (map list lhs* rhs*) body)))
      ((fix lhs* rhs* body)
       (let* ((lhs* (map Var lhs*))
              (rhs* (map E rhs*))
              (body (E body)))
         (import (only (ikarus) map))
         (list 'letrec (map list lhs* rhs*) body)))
      ((recbind lhs* rhs* body)
       (let* ((lhs* (map Var lhs*))
              (rhs* (map E rhs*))
              (body (E body)))
         (import (only (ikarus) map))
         (list 'letrec (map list lhs* rhs*) body)))
      ((rec*bind lhs* rhs* body)
       (let* ((lhs* (map Var lhs*))
              (rhs* (map E rhs*))
              (body (E body)))
         (import (only (ikarus) map))
         (list 'letrec* (map list lhs* rhs*) body)))
      ((seq e0 e1)
       (cons 'begin
          (let f ((e0 e0) (e* (list e1)))
            (struct-case e0
              ((seq e00 e01)
               (f e00 (cons e01 e*)))
              (else
               (let ((x (E e0)))
                 (if (null? e*)
                     (list x)
                     (cons x (f (car e*) (cdr e*))))))))))
      ((clambda g cls* cp free)
       (let ((cls* (map clambda-clause cls*)))
         (cond
           ((= (length cls*) 1) (cons 'lambda (car cls*)))
           (else (cons 'case-lambda cls*)))))
      ((funcall rator rand*)
       (let ((rator (E rator)))
         (cons rator (map E rand*))))
      ((forcall rator rand*) `(foreign-call ,rator . ,(map E rand*)))
      ((assign lhs rhs) `(set! ,(E lhs) ,(E rhs)))
      ((foreign-label x) `(foreign-label ,x))
      (else x)))
  (E x))


(define open-mvcalls (make-parameter #t))

(define (optimize-direct-calls x)
  (define who 'optimize-direct-calls)
  (define (make-conses ls)
    (cond
      ((null? ls) (make-constant '()))
      (else
       (make-funcall (make-primref 'cons)
         (list (car ls) (make-conses (cdr ls)))))))
  (define (properize lhs* rhs*)
    (cond
      ((null? lhs*) (error who "improper improper"))
      ((null? (cdr lhs*))
       (list (make-conses rhs*)))
      (else (cons (car rhs*) (properize (cdr lhs*) (cdr rhs*))))))
  (define (inline-case cls rand*)
    (struct-case cls
      ((clambda-case info body)
       (struct-case info
         ((case-info label fml* proper)
          (if proper
              (and (fx= (length fml*) (length rand*))
                   (make-bind fml* rand* body))
              (and (fx<= (length fml*) (length rand*))
                   (make-bind fml* (properize fml* rand*) body))))))))
  (define (try-inline cls* rand* default)
    (cond
      ((null? cls*) default)
      ((inline-case (car cls*) rand*))
      (else (try-inline (cdr cls*) rand* default))))
  (define (inline mk rator rand*)
    (define (valid-mv-consumer? x)
      (struct-case x
        ((clambda L cases F)
         (and (fx= (length cases) 1)
              (struct-case (car cases)
                ((clambda-case info body)
                 (struct-case info
                   ((case-info L args proper) proper))))))
        (else #f)))
    (define (single-value-consumer? x)
      (struct-case x
        ((clambda L cases F)
         (and (fx= (length cases) 1)
              (struct-case (car cases)
                ((clambda-case info body)
                 (struct-case info
                   ((case-info L args proper)
                    (and proper (fx= (length args) 1))))))))
        (else #f)))
    (define (valid-mv-producer? x)
      (struct-case x
        ((funcall) #t)
        ((conditional) #f)
        ((bind lhs* rhs* body) (valid-mv-producer? body))
        (else #f) ;; FIXME BUG
        ))
    (struct-case rator
      ((clambda g cls*)
       (try-inline cls* rand*
          (mk rator rand*)))
      ((primref op)
       (case op
         ;;; FIXME HERE
         ((call-with-values)
          (cond
            ((and (open-mvcalls) (fx= (length rand*) 2))
             (let ((producer (inline (car rand*) '()))
                   (consumer (cadr rand*)))
               (cond
                 ((single-value-consumer? consumer)
                  (inline consumer (list producer)))
                 ((and (valid-mv-consumer? consumer)
                       (valid-mv-producer? producer))
                  (make-mvcall producer consumer))
                 (else
                  (make-funcall rator rand*)))))
            (else
             (mk rator rand*))))
         ((debug-call)
          (inline
            (lambda (op^ rand*^)
              (mk rator (cons* (car rand*) op^ rand*^)))
            (cadr rand*)
            (cddr rand*)))
         (else
          (mk rator rand*))))
      ((bind lhs* rhs* body)
       (if (null? lhs*)
           (inline mk body rand*)
           (make-bind lhs* rhs*
             (call-expr mk body rand*))))
      ((recbind lhs* rhs* body)
       (if (null? lhs*)
           (inline mk body rand*)
           (make-recbind lhs* rhs*
             (call-expr mk body rand*))))
      ((rec*bind lhs* rhs* body)
       (if (null? lhs*)
           (inline mk body rand*)
           (make-rec*bind lhs* rhs*
             (call-expr mk body rand*))))
      (else (mk rator rand*))))
  (define (call-expr mk x rand*)
    (cond
      ((clambda? x) (inline mk x rand*))
      ((and (prelex? x) (not (prelex-source-assigned? x)))
       ;;; FIXME: did we do the analysis yet?
       (mk x rand*))
      (else
       (let ((t (make-prelex 'tmp #f)))
         (set-prelex-source-referenced?! t #t)
         (make-bind (list t) (list x)
           (mk t rand*))))))
  (define (Expr x)
    (struct-case x
      ((constant) x)
      ((prelex) (assert (prelex-source-referenced? x)) x)
      ((primref) x)
      ((bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body)))
      ((recbind lhs* rhs* body)
       (make-recbind lhs* (map Expr rhs*) (Expr body)))
      ((rec*bind lhs* rhs* body)
       (make-rec*bind lhs* (map Expr rhs*) (Expr body)))
      ((conditional test conseq altern)
       (make-conditional
         (Expr test)
         (Expr conseq)
         (Expr altern)))
      ((seq e0 e1)
       (make-seq (Expr e0) (Expr e1)))
      ((clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (x)
                (struct-case x
                  ((clambda-case info body)
                   (make-clambda-case info (Expr body)))))
              cls*)
         cp free name))
      ((funcall rator rand*)
       (inline make-funcall (Expr rator) (map Expr rand*)))
      ((forcall rator rand*)
       (make-forcall rator (map Expr rand*)))
      ((assign lhs rhs)
       (assert (prelex-source-assigned? lhs))
       (make-assign lhs (Expr rhs)))
      (else (error who "invalid expression" (unparse x)))))
  (Expr x))


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


(include "ikarus.compiler.optimize-letrec.ss")
(include "ikarus.compiler.source-optimizer.ss")


(define (rewrite-assignments x)
  (define who 'rewrite-assignments)
  (define (fix-lhs* lhs*)
    (cond
      ((null? lhs*) (values '() '() '()))
      (else
       (let ((x (car lhs*)))
         (let-values (((lhs* a-lhs* a-rhs*) (fix-lhs* (cdr lhs*))))
           (cond
             ((and (prelex-source-assigned? x) (not (prelex-global-location x)))
              (let ((t (make-prelex 'assignment-tmp #f)))
                (set-prelex-source-referenced?! t #t)
                (values (cons t lhs*) (cons x a-lhs*) (cons t a-rhs*))))
             (else
              (values (cons x lhs*) a-lhs* a-rhs*))))))))
  (define (bind-assigned lhs* rhs* body)
    (cond
      ((null? lhs*) body)
      (else
       (make-bind lhs*
         (map (lambda (rhs) (make-funcall (make-primref 'vector) (list rhs))) rhs*)
         body))))
  (define (Expr x)
    (struct-case x
      ((constant) x)
      ((prelex)
       (cond
         ((prelex-source-assigned? x)
          (cond
            ((prelex-global-location x) =>
             (lambda (loc)
               (make-funcall
                 (make-primref '$symbol-value)
                 (list (make-constant loc)))))
            (else
             (make-funcall (make-primref '$vector-ref)
               (list x (make-constant 0))))))
         (else x)))
      ((primref) x)
      ((bind lhs* rhs* body)
       (let-values (((lhs* a-lhs* a-rhs*) (fix-lhs* lhs*)))
         (make-bind lhs* (map Expr rhs*)
           (bind-assigned a-lhs* a-rhs* (Expr body)))))
      ((fix lhs* rhs* body)
       (make-fix lhs* (map Expr rhs*) (Expr body)))
      ((conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern)))
      ((seq e0 e1) (make-seq (Expr e0) (Expr e1)))
      ((clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (cls)
                (struct-case cls
                  ((clambda-case info body)
                   (struct-case info
                     ((case-info label fml* proper)
                      (let-values (((fml* a-lhs* a-rhs*) (fix-lhs* fml*)))
                        (make-clambda-case
                          (make-case-info label fml* proper)
                          (bind-assigned a-lhs* a-rhs* (Expr body)))))))))
              cls*)
         cp free name))
      ((forcall op rand*)
       (make-forcall op (map Expr rand*)))
      ((funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*)))
      ((assign lhs rhs)
       (cond
         ((prelex-source-assigned? lhs) =>
          (lambda (where)
            (cond
              ((symbol? where)
               (make-funcall (make-primref '$init-symbol-value!)
                 (list (make-constant where) (Expr rhs))))
              ((prelex-global-location lhs) =>
               (lambda (loc)
                 (make-funcall (make-primref '$set-symbol-value!)
                   (list (make-constant loc) (Expr rhs)))))
              (else
               (make-funcall (make-primref '$vector-set!)
                 (list lhs (make-constant 0) (Expr rhs)))))))
         (else
          (error 'rewrite-assignments "not assigned" lhs x))))
      ((mvcall p c) (make-mvcall (Expr p) (Expr c)))
      (else (error who "invalid expression" (unparse x)))))
  (Expr x))



(include "ikarus.compiler.tag-annotation-analysis.ss")


(define (introduce-vars x)
  (define who 'introduce-vars)
  (define (lookup x)
    (let ((v (prelex-operand x)))
      (assert (var? v))
      v))
  (define (convert-prelex x)
    (assert (not (var? (prelex-operand x))))
    (let ((v (unique-var (prelex-name x))))
      (set-var-referenced! v (prelex-source-referenced? x))
      (set-var-global-loc! v (prelex-global-location x))
      (set-prelex-operand! x v)
      v))
  (define (A x)
    (struct-case x
      ((known x t) (make-known (E x) t))
      (else (E x))))
  (define (E x)
    (struct-case x
      ((constant) x)
      ((prelex) (lookup x))
      ((primref) x)
      ((bind lhs* rhs* body)
       (let ((lhs* (map convert-prelex lhs*)))
         (make-bind lhs* (map E rhs*) (E body))))
      ((fix lhs* rhs* body)
       (let ((lhs* (map convert-prelex lhs*)))
         (make-fix lhs* (map E rhs*) (E body))))
      ((conditional e0 e1 e2)
       (make-conditional (E e0) (E e1) (E e2)))
      ((seq e0 e1) (make-seq (E e0) (E e1)))
      ((clambda g cls* cp free name)
       (make-clambda g
         (map
           (lambda (cls)
             (struct-case cls
               ((clambda-case info body)
                (struct-case info
                  ((case-info label args proper)
                   (let ((args (map convert-prelex args)))
                     (make-clambda-case
                       (make-case-info label args proper)
                       (E body))))))))
           cls*)
         cp free name))
      ((primcall rator rand*)
       (make-primcall rator (map A rand*)))
      ((funcall rator rand*)
       (make-funcall (A rator) (map A rand*)))
      ((forcall rator rand*) (make-forcall rator (map E rand*)))
      ((assign lhs rhs)
       (make-assign (lookup lhs) (E rhs)))
      (else (error who "invalid expression" (unparse x)))))
  (E x))


(define (sanitize-bindings x)
  (define who 'sanitize-bindings)
  (define (CLambda x)
    (struct-case x
      ((clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (cls)
                (struct-case cls
                  ((clambda-case info body)
                   (struct-case info
                     ((case-info label fml* proper)
                      (make-clambda-case
                        (make-case-info label fml* proper)
                        (Expr body)))))))
              cls*)
         cp free name))))
  (define (do-fix lhs* rhs* body)
    (if (null? lhs*)
        (Expr body)
        (make-fix lhs* (map CLambda rhs*) (Expr body))))
  (define (A x)
    (struct-case x
      ((known x t) (make-known (Expr x) t))
      (else (Expr x))))
  (define (Expr x)
    (struct-case x
      ((constant) x)
      ((var)      x)
      ((primref) x)
      ((bind lhs* rhs* body)
       (let-values (((lambda* other*)
                     (partition
                       (lambda (x) (clambda? (cdr x)))
                       (map cons lhs* rhs*))))
         (make-bind (map car other*)
                    (map Expr (map cdr other*))
           (do-fix (map car lambda*) (map cdr lambda*)
             body))))
      ((fix lhs* rhs* body)
       (do-fix lhs* rhs* body))
      ((conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern)))
      ((seq e0 e1) (make-seq (Expr e0) (Expr e1)))
      ((clambda g cls* cp free name)
       (let ((t (unique-var 'anon)))
         (make-fix (list t) (list (CLambda x)) t)))
      ((forcall op rand*)
       (make-forcall op (map Expr rand*)))
      ((funcall rator rand*)
       (make-funcall (A rator) (map A rand*)))
      ((mvcall p c) (make-mvcall (Expr p) (Expr c)))
      (else (error who "invalid expression" (unparse x)))))
  (Expr x))


(define (untag x)
  (struct-case x
    ((known x t) (values x t))
    (else        (values x #f))))

(define (tag x t)
  (if t
      (make-known x t)
    x))

(define (optimize-for-direct-jumps x)
  (define who 'optimize-for-direct-jumps)
  (define (init-var x)
    (set-var-referenced! x #f))
  (define (set-var x v)
    (struct-case v
      ((clambda) (set-var-referenced! x v))
      ((var)
       (cond
         ((bound-var v) => (lambda (v) (set-var-referenced! x v)))
         (else (void))))
      (else (void))))
  (define (bound-var x)
    (var-referenced x))
  (define (optimize c rator rand*)
    (let ((n (length rand*)))
      (struct-case c
        ((clambda main-label cls*)
         (let f ((cls* cls*))
           (cond
             ((null? cls*)
              ;;; none matching?
              (make-funcall rator rand*))
             (else
              (struct-case (clambda-case-info (car cls*))
                ((case-info label fml* proper)
                 (cond
                   (proper
                    (if (fx= n (length fml*))
                        (make-jmpcall label (strip rator) (map strip rand*))
                        (f (cdr cls*))))
                   (else
                    (if (fx<= (length (cdr fml*)) n)
                        (make-jmpcall label (strip rator)
                           (let f ((fml* (cdr fml*)) (rand* rand*))
                             (cond
                               ((null? fml*)
                                ;;; FIXME: construct list afterwards
                                (list (make-funcall (make-primref 'list) rand*)))
                               (else
                                (cons (strip (car rand*))
                                      (f (cdr fml*) (cdr rand*)))))))
                        (f (cdr cls*))))))))))))))
  (define (strip x)
    (struct-case x
      ((known x t) x)
      (else x)))
  (define (CLambda x)
    (struct-case x
      ((clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (cls)
                (struct-case cls
                  ((clambda-case info body)
                   ($for-each/stx init-var (case-info-args info))
                   (make-clambda-case info (Expr body)))))
              cls*)
         cp free name))))
  (define (A x)
    (struct-case x
      ((known x t) (make-known (Expr x) t))
      (else (Expr x))))
  (define (A- x)
    (struct-case x
      ((known x t) (Expr x))
      (else (Expr x))))
  (define (Expr x)
    (struct-case x
      ((constant) x)
      ((var)      x)
      ((primref)  x)
      ((bind lhs* rhs* body)
       ($for-each/stx init-var lhs*)
       (let ((rhs* (map Expr rhs*)))
         ($for-each/stx set-var lhs* rhs*)
         (make-bind lhs* rhs* (Expr body))))
      ((fix lhs* rhs* body)
       ($for-each/stx set-var lhs* rhs*)
       (make-fix lhs* (map CLambda rhs*) (Expr body)))
      ((conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern)))
      ((seq e0 e1) (make-seq (Expr e0) (Expr e1)))
      ((forcall op rand*)
       (make-forcall op (map Expr rand*)))
      ((funcall rator rand*)
       (let-values (((rator t) (untag (A rator))))
         (cond
           ((and (var? rator) (bound-var rator)) =>
            (lambda (c)
              (optimize c rator (map A rand*))))
           ((and (primref? rator)
                 (eq? (primref-name rator) '$$apply))
            (make-jmpcall (sl-apply-label)
                          (A- (car rand*))
                          (map A- (cdr rand*))))
           (else
            (make-funcall (tag rator t) (map A rand*))))))
      (else (error who "invalid expression" (unparse x)))))
  (Expr x))


(define (insert-global-assignments x)
  (define who 'insert-global-assignments)
  (define (global-assign lhs* body)
    (cond
      ((null? lhs*) body)
      ((var-global-loc (car lhs*)) =>
       (lambda (loc)
         (make-seq
           (make-funcall (make-primref '$init-symbol-value!)
             (list (make-constant loc) (car lhs*)))
           (global-assign (cdr lhs*) body))))
      (else (global-assign (cdr lhs*) body))))
  (define (global-fix lhs* body)
    (cond
      ((null? lhs*) body)
      ((var-global-loc (car lhs*)) =>
       (lambda (loc)
         (make-seq
           (make-funcall (make-primref '$set-symbol-value/proc!)
             (list (make-constant loc) (car lhs*)))
           (global-assign (cdr lhs*) body))))
      (else (global-assign (cdr lhs*) body))))
  (define (A x)
    (struct-case x
      ((known x t) (make-known (Expr x) t))
      (else (Expr x))))
  (define (Expr x)
    (struct-case x
      ((constant) x)
      ((var)
       (cond
         ((var-global-loc x) =>
          (lambda (loc)
            (make-funcall
              (make-primref '$symbol-value)
              (list (make-constant loc)))))
         (else x)))
      ((primref)  x)
      ((bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*)
         (global-assign lhs* (Expr body))))
      ((fix lhs* rhs* body)
       (make-fix lhs* (map Expr rhs*)
         (global-fix lhs* (Expr body))))
      ((conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern)))
      ((seq e0 e1) (make-seq (Expr e0) (Expr e1)))
      ((clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (cls)
                (struct-case cls
                  ((clambda-case info body)
                   (make-clambda-case info (Expr body)))))
              cls*)
         cp free name))
      ((forcall op rand*)
       (make-forcall op (map Expr rand*)))
      ((funcall rator rand*)
       (make-funcall (A rator) (map A rand*)))
      ((jmpcall label rator rand*)
       (make-jmpcall label (Expr rator) (map Expr rand*)))
      (else (error who "invalid expression" (unparse x)))))
  (define (AM x)
    (struct-case x
      ((known x t) (make-known (Main x) t))
      (else (Main x))))
  (define (Main x)
    (struct-case x
      ((constant) x)
      ((var)      x)
      ((primref)  x)
      ((bind lhs* rhs* body)
       (make-bind lhs* (map Main rhs*)
         (global-assign lhs* (Main body))))
      ((fix lhs* rhs* body)
       (make-fix lhs* (map Main rhs*)
         (global-fix lhs* (Main body))))
      ((conditional test conseq altern)
       (make-conditional (Main test) (Main conseq) (Main altern)))
      ((seq e0 e1) (make-seq (Main e0) (Main e1)))
      ((clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (cls)
                (struct-case cls
                  ((clambda-case info body)
                   (make-clambda-case info (Expr body)))))
              cls*)
         cp free name))
      ((forcall op rand*)
       (make-forcall op (map Main rand*)))
      ((funcall rator rand*)
       (make-funcall (AM rator) (map AM rand*)))
      ((jmpcall label rator rand*)
       (make-jmpcall label (Main rator) (map Main rand*)))
      (else (error who "invalid expression" (unparse x)))))
  (let ((x (Main x)))
    ;(pretty-print x)
    x))


(define optimize-cp (make-parameter #t))

(define (convert-closures prog)
  (define who 'convert-closures)
  (define (Expr* x*)
    (cond
      ((null? x*) (values '() '()))
      (else
       (let-values (((a a-free) (Expr (car x*)))
                    ((d d-free) (Expr* (cdr x*))))
         (values (cons a d) (union a-free d-free))))))
  (define (do-clambda* lhs* x*)
   (cond
     ((null? x*) (values '() '()))
     (else
      (let-values (((a a-free) (do-clambda (car lhs*) (car x*)))
                   ((d d-free) (do-clambda* (cdr lhs*) (cdr x*))))
        (values (cons a d) (union a-free d-free))))))
  (define (do-clambda lhs x)
    (struct-case x
      ((clambda g cls* _cp _free name)
       (let-values (((cls* free)
                     (let f ((cls* cls*))
                       (cond
                         ((null? cls*) (values '() '()))
                         (else
                          (struct-case (car cls*)
                            ((clambda-case info body)
                             (let-values (((body body-free) (Expr body))
                                          ((cls* cls*-free) (f (cdr cls*))))
                               (values
                                 (cons (make-clambda-case info body) cls*)
                                 (union (difference body-free (case-info-args info))
                                        cls*-free))))))))))
          (values
            (make-closure
              (make-clambda g cls* lhs free name)
              free
              #f)
            free)))))
  (define (A x)
    (struct-case x
      ((known x t)
       (let-values (((x free) (Expr x)))
         (values (make-known x t) free)))
      (else (Expr x))))
  (define (A* x*)
    (cond
      ((null? x*) (values '() '()))
      (else
       (let-values (((a a-free) (A (car x*)))
                    ((d d-free) (A* (cdr x*))))
         (values (cons a d) (union a-free d-free))))))
  (define (Expr ex)
    (struct-case ex
      ((constant) (values ex '()))
      ((var)
       (set-var-index! ex #f)
       (values ex (singleton ex)))
      ((primref) (values ex '()))
      ((bind lhs* rhs* body)
       (let-values (((rhs* rhs-free) (Expr* rhs*))
                    ((body body-free) (Expr body)))
          (values (make-bind lhs* rhs* body)
                  (union rhs-free (difference body-free lhs*)))))
      ((fix lhs* rhs* body)
       (for-each (lambda (x) (set-var-index! x #t)) lhs*)
       (let-values (((rhs* rfree) (do-clambda* lhs* rhs*))
                    ((body bfree) (Expr body)))
          (for-each
            (lambda (lhs rhs)
              (when (var-index lhs)
                (set-closure-well-known?! rhs #t)
                (set-var-index! lhs #f)))
            lhs* rhs*)
          (values (make-fix lhs* rhs* body)
                  (difference (union bfree rfree) lhs*))))
      ((conditional test conseq altern)
       (let-values (((test test-free) (Expr test))
                    ((conseq conseq-free) (Expr conseq))
                    ((altern altern-free) (Expr altern)))
         (values (make-conditional test conseq altern)
                 (union test-free (union conseq-free altern-free)))))
      ((seq e0 e1)
       (let-values (((e0 e0-free) (Expr e0))
                    ((e1 e1-free) (Expr e1)))
         (values (make-seq e0 e1) (union e0-free e1-free))))
      ((forcall op rand*)
       (let-values (((rand* rand*-free) (Expr* rand*)))
         (values (make-forcall op rand*)  rand*-free)))
      ((funcall rator rand*)
       (let-values (((rator rat-free) (A rator))
                    ((rand* rand*-free) (A* rand*)))
         (values (make-funcall rator rand*)
                 (union rat-free rand*-free))))
      ((jmpcall label rator rand*)
       (let-values (((rator rat-free)
                     (if (optimize-cp) (Rator rator) (Expr rator)))
                    ((rand* rand*-free)
                     (A* rand*)))
         (values (make-jmpcall label rator rand*)
                 (union rat-free rand*-free))))
      (else (error who "invalid expression" ex))))
  (define (Rator x)
    (struct-case x
      ((var) (values x (singleton x)))
      ;((known x t)
      ; (let-values (((x free) (Rator x)))
      ;   (values (make-known x t) free)))
      (else (Expr x))))
  (let-values (((prog free) (Expr prog)))
    (unless (null? free)
      (error 'convert-closures "free vars encountered in program"
          (map unparse free)))
   prog))


(define (optimize-closures/lift-codes x)
  (define who 'optimize-closures/lift-codes)
  (define all-codes '())
  (module (unset! set-subst! get-subst copy-subst!)
    (define-struct prop (val))
    (define (unset! x)
      (unless (var? x) (error 'unset! "not a var" x))
      (set-var-index! x #f))
    (define (set-subst! x v)
      (unless (var? x) (error 'set-subst! "not a var" x))
      (set-var-index! x (make-prop v)))
    (define (copy-subst! lhs rhs)
      (unless (var? lhs) (error 'copy-subst! "not a var" lhs))
      (cond
        ((and (var? rhs) (var-index rhs)) =>
         (lambda (v)
           (cond
             ((prop? v) (set-var-index! lhs v))
             (else (set-var-index! lhs #f)))))
        (else (set-var-index! lhs #f))))
    (define (get-subst x)
      (unless (var? x) (error 'get-subst "not a var" x))
      (struct-case (var-index x)
        ((prop v) v)
        (else #f))))
  (define (combinator? x)
    (struct-case x
      ((closure code free*)
       (null? free*))
      (else #f)))
  (define (lift-code cp code free*)
    (struct-case code
      ((clambda label cls* cp/dropped free*/dropped name)
       (let ((cls* (map
                     (lambda (x)
                       (struct-case x
                         ((clambda-case info body)
                          ($for-each/stx unset! (case-info-args info))
                          (make-clambda-case info (E body)))))
                     cls*)))
         (let ((g (make-code-loc label)))
           (set! all-codes
             (cons (make-clambda label cls* cp free* name)
                   all-codes))
           g)))))
  (define (trim p? ls)
    (cond
      ((null? ls) '())
      ((p? (car ls)) (trim p? (cdr ls)))
      (else
       (cons (car ls) (trim p? (cdr ls))))))
  (define (do-bind lhs* rhs* body)
    ($for-each/stx unset! lhs*)
    (let ((rhs* (map E rhs*)))
      ($for-each/stx copy-subst! lhs* rhs*)
      (let ((body (E body)))
        ($for-each/stx unset! lhs*)
        (make-bind lhs* rhs* body))))
  (define (trim-free ls)
    (cond
      ((null? ls) '())
      ((get-forward! (car ls)) =>
       (lambda (what)
         (let ((rest (trim-free (cdr ls))))
           (struct-case what
             ((closure) rest)
             ((var) (if (memq what rest) rest (cons what rest)))
             (else (error who "invalid value in trim-free" what))))))
      (else (cons (car ls) (trim-free (cdr ls))))))
  (define (do-fix lhs* rhs* body)
    ($for-each/stx unset! lhs*)
    (let ((free** ;;; trim the free lists first; after init.
           (map (lambda (lhs rhs)
                  ;;; remove self also
                  (remq lhs (trim-free (closure-free* rhs))))
                lhs* rhs*)))
      (define-struct node (name code deps whacked free wk?))
      (let ((node*
             (map (lambda (lhs rhs)
                    (let ((n (make-node lhs (closure-code rhs) '() #f '()
                               (closure-well-known? rhs))))
                      (set-subst! lhs n)
                      n))
                   lhs* rhs*)))
        ;;; if x is free in y, then whenever x becomes a non-combinator,
        ;;; y also becomes a non-combinator.  Here, we mark these
        ;;; dependencies.
        (for-each
          (lambda (my-node free*)
            (for-each (lambda (fvar)
                        (cond
                          ((get-subst fvar) => ;;; one of ours
                           (lambda (her-node)
                             (set-node-deps! her-node
                               (cons my-node (node-deps her-node)))))
                          (else ;;; not one of ours
                           (set-node-free! my-node
                             (cons fvar (node-free my-node))))))
                      free*))
           node* free**)
        ;;; Next, we go over the list of nodes, and if we find one
        ;;; that has any free variables, we know it's a non-combinator,
        ;;; so we whack it and add it to all of its dependents.
        (let ()
          (define (process-node x)
            (when (cond
                    ((null? (node-free x)) #f)
                    ;((and (node-wk? x) (null? (cdr (node-free x)))) #f)
                    (else #t))
              (unless (node-whacked x)
                (set-node-whacked! x #t)
                (for-each
                  (lambda (y)
                    (set-node-free! y
                      (cons (node-name x) (node-free y)))
                    (process-node y))
                  (node-deps x)))))
          ($for-each/stx process-node node*))
        ;;; Now those that have free variables are actual closures.
        ;;; Those with no free variables are actual combinators.
        (let ((rhs*
               (map
                 (lambda (node)
                   (let ((wk? (node-wk? node))
                         (name (node-name node))
                         (free (node-free node)))
                     (let ((closure
                            (make-closure (node-code node) free wk?)))
                       (cond
                         ((null? free)
                          (set-subst! name closure))
                         ((and (null? (cdr free)) wk?)
                          (set-subst! name closure))
                         (else
                          (unset! name)))
                       closure)))
                 node*)))
          (for-each
            (lambda (lhs^ closure)
              (let* ((lhs (get-forward! lhs^))
                     (free
                      (filter var?
                        (remq lhs (trim-free (closure-free* closure))))))
                (set-closure-free*! closure free)
                (set-closure-code! closure
                  (lift-code
                    lhs
                    (closure-code closure)
                    (closure-free* closure)))))
            lhs*
            rhs*)
          (let ((body (E body)))
            (let f ((lhs* lhs*) (rhs* rhs*) (l* '()) (r* '()))
              (cond
                ((null? lhs*)
                 (if (null? l*)
                     body
                     (make-fix l* r* body)))
                (else
                 (let ((lhs (car lhs*)) (rhs (car rhs*)))
                   (cond
                     ((get-subst lhs)
                      (unset! lhs)
                      (f (cdr lhs*) (cdr rhs*) l* r*))
                     (else
                      (f (cdr lhs*) (cdr rhs*)
                         (cons lhs l*) (cons rhs r*)))))))))))))
  (define (get-forward! x)
    (when (eq? x 'q)
      (error who "BUG: circular dep"))
    (let ((y (get-subst x)))
      (cond
        ((not y) x)
        ((var? y)
         (set-subst! x 'q)
         (let ((y (get-forward! y)))
           (set-subst! x y)
           y))
        ((closure? y)
         (let ((free (closure-free* y)))
           (cond
             ((null? free) y)
             ((null? (cdr free))
              (set-subst! x 'q)
              (let ((y (get-forward! (car free))))
                (set-subst! x y)
                y))
             (else y))))
        (else x))))
  (define (A x)
    (struct-case x
      ((known x t) (make-known (E x) t))
      (else (E x))))
  (define (E x)
    (struct-case x
      ((constant) x)
      ((var)      (get-forward! x))
      ((primref)  x)
      ((bind lhs* rhs* body) (do-bind lhs* rhs* body))
      ((fix lhs* rhs* body) (do-fix lhs* rhs* body))
      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))
      ((seq e0 e1)           (make-seq (E e0) (E e1)))
      ((forcall op rand*)    (make-forcall op (map E rand*)))
      ((funcall rator rand*) (make-funcall (A rator) (map A rand*)))
      ((jmpcall label rator rand*)
       (make-jmpcall label (E rator) (map E rand*)))
      (else (error who "invalid expression" (unparse x)))))
  ;(when (optimize-cp)
  ;  (printf "BEFORE\n")
  ;  (parameterize ((pretty-width 200))
  ;    (pretty-print (unparse x))))
  (let ((x (E x)))
    (let ((v (make-codes all-codes x)))
      ;(when (optimize-cp)
      ;  (printf "AFTER\n")
      ;  (parameterize ((pretty-width 200))
      ;    (pretty-print (unparse v))))
      v)))


;;;; definitions
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

(define fx-scale				wordsize)
(define object-alignment			(* 2 wordsize))
(define align-shift				(+ wordshift 1))
(define pagesize				4096)
(define pageshift				12)

(define fx-shift				wordshift)
(define fx-mask					(- wordsize 1))
(define fx-tag					0)

(define bool-f					#x2F)	;the constant #f
(define bool-t					#x3F)	;the constant #t
(define bool-mask				#xEF)
(define bool-tag				#x2F)
(define bool-shift				4)

(define nil					#x4F)
(define eof					#x5F)
(define unbound					#x6F)
(define void-object				#x7F)
(define bwp-object				#x8F)

(define char-size				4)
(define char-shift				8)
(define char-tag				#x0F)
(define char-mask				#xFF)

(define pair-mask				7)
(define pair-tag				1)
(define disp-car				0)
(define disp-cdr				wordsize)
(define pair-size				(* 2 wordsize))

(define flonum-tag				#x17)
(define flonum-size				16)
(define disp-flonum-data			8)

(define ratnum-tag				#x27)
(define disp-ratnum-num				(* 1 wordsize))
(define disp-ratnum-den				(* 2 wordsize))
(define ratnum-size				(* 4 wordsize))

(define compnum-tag				#x37)
(define disp-compnum-real			(* 1 wordsize))
(define disp-compnum-imag			(* 2 wordsize))
(define compnum-size				(* 4 wordsize))

(define cflonum-tag				#x47)
(define disp-cflonum-real			(* 1 wordsize))
(define disp-cflonum-imag			(* 2 wordsize))
(define cflonum-size				(* 4 wordsize))

(define bignum-mask				#b111)
(define bignum-tag				#b011)
(define bignum-sign-mask			#b1000)
(define bignum-sign-shift			3)
(define bignum-length-shift			4)
(define disp-bignum-data			wordsize)

(define bytevector-mask				7)
(define bytevector-tag				2)
(define disp-bytevector-length			0)
(define disp-bytevector-data			8)
		;To  allow  the same  displacement  on  both 32-bit  and
		;64-bit platforms.

(define vector-tag				5)
(define vector-mask				7)
(define disp-vector-length			0)
(define disp-vector-data			wordsize)

(define symbol-primary-tag			vector-tag)
(define symbol-tag				#x5F)
(define symbol-record-tag			#x5F)
(define disp-symbol-record-string		(* 1 wordsize))
(define disp-symbol-record-ustring		(* 2 wordsize))
(define disp-symbol-record-value		(* 3 wordsize))
(define disp-symbol-record-proc			(* 4 wordsize))
(define disp-symbol-record-plist		(* 5 wordsize))
(define symbol-record-size			(* 6 wordsize))

(define record-tag				vector-tag)
(define disp-struct-rtd				0)
(define disp-struct-data			wordsize)

(define string-mask				#b111)
(define string-tag				6)
(define disp-string-length			0)
(define disp-string-data			wordsize)

(define closure-mask				7)
(define closure-tag				3)
(define disp-closure-code			0)
(define disp-closure-data			wordsize)

(define continuation-tag			#x1F)
(define disp-continuation-top			(* 1 wordsize))
(define disp-continuation-size			(* 2 wordsize))
(define disp-continuation-next			(* 3 wordsize))
(define continuation-size			(* 4 wordsize))

(define code-tag				#x2F)
(define disp-code-instrsize			(* 1 wordsize))
(define disp-code-relocsize			(* 2 wordsize))
(define disp-code-freevars			(* 3 wordsize))
(define disp-code-annotation			(* 4 wordsize))
(define disp-code-unused			(* 5 wordsize))
(define disp-code-data				(* 6 wordsize))

(define transcoder-mask				#xFF) ;;; 0011
(define transcoder-tag				#x7F) ;;; 0011
(define transcoder-payload-shift		10)

(define transcoder-write-utf8-mask		#x1000)
(define transcoder-write-byte-mask		#x2000)
(define transcoder-read-utf8-mask		#x4000)
(define transcoder-read-byte-mask		#x8000)
(define transcoder-handling-mode-shift		16)
(define transcoder-handling-mode-bits		2)
(define transcoder-eol-style-shift		18)
(define transcoder-eol-style-bits		3)
(define transcoder-codec-shift			21)
(define transcoder-codec-bits			3)

(define transcoder-handling-mode:none		#b00)
(define transcoder-handling-mode:ignore		#b01)
(define transcoder-handling-mode:raise		#b10)
(define transcoder-handling-mode:replace	#b11)

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

(define port-tag			#x3F)
(define port-mask			#x3F)
(define disp-port-attrs			0)
(define disp-port-index			(*  1 wordsize))
(define disp-port-size			(*  2 wordsize))
(define disp-port-buffer		(*  3 wordsize))
(define disp-port-transcoder		(*  4 wordsize))
(define disp-port-id			(*  5 wordsize))
(define disp-port-read!			(*  6 wordsize))
(define disp-port-write!		(*  7 wordsize))
(define disp-port-get-position		(*  8 wordsize))
(define disp-port-set-position!		(*  9 wordsize))
(define disp-port-close			(* 10 wordsize))
(define disp-port-cookie		(* 11 wordsize))
(define disp-port-unused1		(* 12 wordsize))
(define disp-port-unused2		(* 13 wordsize))
(define port-size			(* 14 wordsize))

(define pointer-tag			#x107)
(define disp-pointer-data		wordsize)
(define pointer-size			(* 2 wordsize))
(define off-pointer-data		(- disp-pointer-data vector-tag))

(define disp-tcbucket-tconc		0)
(define disp-tcbucket-key		(* 1 wordsize))
(define disp-tcbucket-val		(* 2 wordsize))
(define disp-tcbucket-next		(* 3 wordsize))
(define tcbucket-size			(* 4 wordsize))

;;Refer  to  the picture  in  src/ikarus-collect.c  for details  on  how
;;call-frames are laid out (search for livemask).
;;
(define call-instruction-size
  (case-word-size
   ((32) 5)
   ((64) 10)))
(define disp-frame-size		(- (+ call-instruction-size (* 3 wordsize))))
(define disp-frame-offset	(- (+ call-instruction-size (* 2 wordsize))))
(define disp-multivalue-rp	(- (+ call-instruction-size (* 1 wordsize))))

(define dirty-word		-1)

;;; --------------------------------------------------------------------

;;;(define pcb-allocation-pointer	(*  0 wordsize)) NOT USED
(define pcb-allocation-redline		(*  1 wordsize))
;;;(define pcb-frame-pointer		(*  2 wordsize)) NOT USED
(define pcb-frame-base			(*  3 wordsize))
(define pcb-frame-redline		(*  4 wordsize))
(define pcb-next-continuation		(*  5 wordsize))
;;;(define pcb-system-stack		(*  6 wordsize)) NOT USED
(define pcb-dirty-vector		(*  7 wordsize))
(define pcb-arg-list			(*  8 wordsize))
(define pcb-engine-counter		(*  9 wordsize))
(define pcb-interrupted			(* 10 wordsize))
(define pcb-base-rtd			(* 11 wordsize))
(define pcb-collect-key			(* 12 wordsize))


(define (fx? x)
  (let* ((intbits (* wordsize 8))
         (fxbits (- intbits fx-shift)))
    (and (or (fixnum? x) (bignum? x))
         (<= (- (expt 2 (- fxbits 1)))
             x
             (- (expt 2 (- fxbits 1)) 1)))))


(module ()
  ;;; initialize the cogen
  (code-entry-adjustment (- disp-code-data vector-tag)))

(begin ;;; COGEN HELERS
  (define (align n)
    (fxsll (fxsra (fx+ n (fxsub1 object-alignment)) align-shift) align-shift))
  (define (mem off val)
    (cond
      ((fixnum? off) (list 'disp (int off) val))
      ((register? off) (list 'disp off val))
      (else (error 'mem "invalid disp" off))))
  (define-syntax int
    (syntax-rules ()
      ((_ x) x)))
  (define (obj x) (list 'obj x))
  (define (byte x) (list 'byte x))
  (define (byte-vector x) (list 'byte-vector x))
  (define (movzbl src targ) (list 'movzbl src targ))
  (define (sall src targ) (list 'sall src targ))
  (define (sarl src targ) (list 'sarl src targ))
  (define (shrl src targ) (list 'shrl src targ))
  (define (notl src) (list 'notl src))
  (define (pushl src) (list 'pushl src))
  (define (popl src) (list 'popl src))
  (define (orl src targ) (list 'orl src targ))
  (define (xorl src targ) (list 'xorl src targ))
  (define (andl src targ) (list 'andl src targ))
  (define (movl src targ) (list 'movl src targ))
  (define (leal src targ) (list 'leal src targ))
  (define (movb src targ) (list 'movb src targ))
  (define (addl src targ) (list 'addl src targ))
  (define (imull src targ) (list 'imull src targ))
  (define (idivl src) (list 'idivl src))
  (define (subl src targ) (list 'subl src targ))
  (define (push src) (list 'push src))
  (define (pop targ) (list 'pop targ))
  (define (sete targ) (list 'sete targ))
  (define (call targ) (list 'call targ))
  (define (tail-indirect-cpr-call)
    (jmp (mem (fx- disp-closure-code closure-tag) cpr)))
  (define (indirect-cpr-call)
    (call (mem (fx- disp-closure-code closure-tag) cpr)))
  (define (negl targ) (list 'negl targ))
  (define (label x) (list 'label x))
  (define (label-address x) (list 'label-address x))
  (define (ret) '(ret))
  (define (cltd) '(cltd))
  (define (cmpl arg1 arg2) (list 'cmpl arg1 arg2))
  (define (je label) (list 'je label))
  (define (jne label) (list 'jne label))
  (define (jle label) (list 'jle label))
  (define (jge label) (list 'jge label))
  (define (jg label) (list 'jg label))
  (define (jl label) (list 'jl label))
  (define (jb label) (list 'jb label))
  (define (ja label) (list 'ja label))
  (define (jo label) (list 'jo label))
  (define (jmp label) (list 'jmp label))
  (define esp '%esp) ; stack base pointer
  (define al '%al)
  (define ah '%ah)
  (define bh '%bh)
  (define cl '%cl)
  (define eax '%eax)
  (define ebx '%ebx)
  (define ecx '%ecx)
  (define edx '%edx)
  (define apr '%ebp) ; allocation pointer
  (define fpr '%esp) ; frame pointer
  (define cpr '%edi) ; closure pointer
  (define pcr '%esi) ; pcb pointer
  (define register? symbol?)
  (define (argc-convention n)
    (fx- 0 (fxsll n fx-shift))))


(define (primref->symbol op)
  (unless (symbol? op)
    (error 'primref->symbol "not a symbol" op))
  (cond (((current-primitive-locations) op)
	 => (lambda (x)
	      (unless (symbol? x)
		(error 'primitive-location "not a valid location" x op))
	      x))
	(else
	 (error 'vicare "primitive missing from makefile.sps" op))))

;(define (primref-loc op)
;  (mem (fx- disp-symbol-record-proc record-tag)
;       (obj (primref->symbol op))))



(module ;assembly-labels
  (refresh-cached-labels!
   sl-annotated-procedure-label
   sl-apply-label
   sl-continuation-code-label
   sl-invalid-args-label
   sl-mv-ignore-rp-label
   sl-mv-error-rp-label
   sl-values-label
   sl-cwv-label)
  (define-syntax define-cached
    (lambda (x)
      (syntax-case x ()
        ((_ refresh ((name*) b* b** ...) ...)
         (with-syntax (((v* ...) (generate-temporaries #'(name* ...))))
           #'(begin
               (define v* #f) ...
               (define (name*)
                 (or v* (error 'name* "uninitialized label"))) ...
               (define (refresh)
                 (define-syntax name*
                   (lambda (stx)
                     (syntax-error stx
                        "cannot use label before it is defined")))
                 ...
                 (let* ((name* (let ((label (let () b* b** ...)))
                                 (set! v* label)
                                 (lambda () label))) ...)
                   (void)))))))))
  (define-cached refresh-cached-labels!
   ((sl-annotated-procedure-label)
    (import (ikarus.code-objects))
    (define SL_annotated (gensym "SL_annotated"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 2
          `(name ,(make-annotation-indirect))
          (label SL_annotated)
          (movl (mem (fx- (fx+ disp-closure-data wordsize) closure-tag) cpr) cpr)
          (tail-indirect-cpr-call))))
    SL_annotated)
   ((sl-apply-label)
    (let ((SL_apply (gensym "SL_apply"))
          (L_apply_done (gensym))
          (L_apply_loop (gensym)))
      (assemble-sources (lambda (x) #f)
        (list
          (list 0
              (label SL_apply)
              (movl (mem fpr eax) ebx)
              (cmpl (int nil) ebx)
              (je (label L_apply_done))
              (label L_apply_loop)
              (movl (mem (fx- disp-car pair-tag) ebx) ecx)
              (movl (mem (fx- disp-cdr pair-tag) ebx) ebx)
              (movl ecx (mem fpr eax))
              (subl (int wordsize) eax)
              (cmpl (int nil) ebx)
              (jne (label L_apply_loop))
              (label L_apply_done)
              (addl (int wordsize) eax)
              (tail-indirect-cpr-call))))
      SL_apply))
   ((sl-continuation-code-label)
    (define SL_continuation_code (gensym "SL_continuation_code"))
    (assemble-sources (lambda (x) #f)
      (list
        (let ((L_cont_zero_args      (gensym))
              (L_cont_mult_args      (gensym))
              (L_cont_one_arg        (gensym))
              (L_cont_mult_move_args (gensym))
              (L_cont_mult_copy_loop (gensym)))
          (list  1 ; freevars
              (label SL_continuation_code)
              (movl (mem (fx- disp-closure-data closure-tag) cpr) ebx) ; captured-k
              (movl ebx (mem pcb-next-continuation pcr)) ; set
              (movl (mem pcb-frame-base pcr) ebx)
              (cmpl (int (argc-convention 1)) eax)
              (jg (label L_cont_zero_args))
              (jl (label L_cont_mult_args))
              (label L_cont_one_arg)
              (movl (mem (fx- 0 wordsize) fpr) eax)
              (movl ebx fpr)
              (subl (int wordsize) fpr)
              (ret)
              (label L_cont_zero_args)
              (subl (int wordsize) ebx)
              (movl ebx fpr)
              (movl (mem 0 ebx) ebx) ; return point
              (jmp (mem disp-multivalue-rp ebx))  ; go
              (label L_cont_mult_args)
              (subl (int wordsize) ebx)
              (cmpl ebx fpr)
              (jne (label L_cont_mult_move_args))
              (movl (mem 0 ebx) ebx)
              (jmp (mem disp-multivalue-rp ebx))
              (label L_cont_mult_move_args)
              ; move args from fpr to ebx
              (movl (int 0) ecx)
              (label L_cont_mult_copy_loop)
              (subl (int wordsize) ecx)
              (movl (mem fpr ecx) edx)
              (movl edx (mem ebx ecx))
              (cmpl ecx eax)
              (jne (label L_cont_mult_copy_loop))
              (movl ebx fpr)
              (movl (mem 0 ebx) ebx)
              (jmp (mem disp-multivalue-rp ebx))))))
    SL_continuation_code)
   ((sl-invalid-args-label)
    (define SL_invalid_args (gensym "SL_invalid_args"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_invalid_args)
          ;;;
          (movl cpr (mem (fx- 0 wordsize) fpr)) ; first arg
          (negl eax)
          (movl eax (mem (fx- 0 (fx* 2 wordsize)) fpr))
          (movl (obj (primref->symbol '$incorrect-args-error-handler)) cpr)
          (movl (mem (- disp-symbol-record-proc record-tag) cpr) cpr)
          ;(movl (primref-loc '$incorrect-args-error-handler) cpr)
          (movl (int (argc-convention 2)) eax)
          (tail-indirect-cpr-call))))
    SL_invalid_args)
   ((sl-mv-ignore-rp-label)
    (define SL_multiple_values_ignore_rp (gensym "SL_multiple_ignore_error_rp"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
           (label SL_multiple_values_ignore_rp)
           (ret))))
    SL_multiple_values_ignore_rp)
   ((sl-mv-error-rp-label)
    (define SL_multiple_values_error_rp (gensym "SL_multiple_values_error_rp"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_multiple_values_error_rp)
          (movl (obj (primref->symbol '$multiple-values-error)) cpr)
          (movl (mem (- disp-symbol-record-proc record-tag) cpr) cpr)
          ;(movl (primref-loc '$multiple-values-error) cpr)
          (tail-indirect-cpr-call))))
    SL_multiple_values_error_rp)
   ((sl-values-label)
    (define SL_values (gensym "SL_values"))
    (assemble-sources (lambda (x) #f)
      (list
        (let ((L_values_one_value (gensym))
              (L_values_many_values (gensym)))
          (list 0 ; no freevars
              '(name values)
              (label SL_values)
              (cmpl (int (argc-convention 1)) eax)
              (je (label L_values_one_value))
              (label L_values_many_values)
              (movl (mem 0 fpr) ebx) ; return point
              (jmp (mem disp-multivalue-rp ebx))     ; go
              (label L_values_one_value)
              (movl (mem (fx- 0 wordsize) fpr) eax)
              (ret)))))
    SL_values)
   ((sl-cwv-label)
    (define SL_call_with_values (gensym "SL_call_with_values"))
    (assemble-sources (lambda (x) #f)
      (list
        (let ((L_cwv_done (gensym))
              (L_cwv_loop (gensym))
              (L_cwv_multi_rp (gensym))
              (L_cwv_call (gensym))
              (SL_nonprocedure (gensym "SL_nonprocedure"))
              (SL_invalid_args (gensym "SL_invalid_args")))
          (list
              0 ; no free vars
              '(name call-with-values)
              (label SL_call_with_values)
              (cmpl (int (argc-convention 2)) eax)
              (jne (label SL_invalid_args))
              (movl (mem (fx- 0 wordsize) fpr) ebx) ; producer
              (movl ebx cpr)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label SL_nonprocedure))
              (movl (int (argc-convention 0)) eax)
              (compile-call-frame
                 3
                 '#(#b110)
                 (label-address L_cwv_multi_rp)
                 (indirect-cpr-call))
              ;;; one value returned
              (movl (mem (fx* -2 wordsize) fpr) ebx) ; consumer
              (movl ebx cpr)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl (int (argc-convention 1)) eax)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label SL_nonprocedure))
              (tail-indirect-cpr-call)
              ;;; multiple values returned
              (label L_cwv_multi_rp)
              ; because values does not pop the return point
              ; we have to adjust fp one more word here
              (addl (int (fx* wordsize 3)) fpr)
              (movl (mem (fx* -2 wordsize) fpr) cpr) ; consumer
              (cmpl (int (argc-convention 0)) eax)
              (je (label L_cwv_done))
              (movl (int (fx* -4 wordsize)) ebx)
              (addl fpr ebx)  ; ebx points to first value
              (movl ebx ecx)
              (addl eax ecx)  ; ecx points to the last value
              (label L_cwv_loop)
              (movl (mem 0 ebx) edx)
              (movl edx (mem (fx* 3 wordsize) ebx))
              (subl (int wordsize) ebx)
              (cmpl ecx ebx)
              (jge (label L_cwv_loop))
              (label L_cwv_done)
              (movl cpr ebx)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label SL_nonprocedure))
              (tail-indirect-cpr-call)

              (label SL_nonprocedure)
              (movl cpr (mem (fx- 0 wordsize) fpr)) ; first arg
              (movl (obj (primref->symbol '$apply-nonprocedure-error-handler)) cpr)
              (movl (mem (- disp-symbol-record-proc record-tag) cpr) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call)

              (label SL_invalid_args)
              ;;;
              (movl cpr (mem (fx- 0 wordsize) fpr)) ; first arg
              (negl eax)
              (movl eax (mem (fx- 0 (fx* 2 wordsize)) fpr))
              (movl (obj (primref->symbol '$incorrect-args-error-handler)) cpr)
              (movl (mem (- disp-symbol-record-proc record-tag) cpr) cpr)
              (movl (int (argc-convention 2)) eax)
              (tail-indirect-cpr-call)

              ))))
    SL_call_with_values)
   ))

(define (print-instr x)
  ;;Print to the current error port the symbolic expression representing
  ;;the assembly  instruction X.  To  be used to log  generated assembly
  ;;for human inspection.
  ;;
  (if (and (pair? x)
	   (eq? ($car x) 'seq))
      ($for-each/stx print-instr ($cdr x))
    (let ((port (current-error-port)))
      (display "   " port)
      (write x port)
      (newline port))))

(define optimizer-output (make-parameter #f))
(define perform-tag-analysis (make-parameter #t))

(define (compile-core-expr->code p)
  (let* ((p (recordize p))
         (p (parameterize ((open-mvcalls #f))
              (optimize-direct-calls p)))
         (p (optimize-letrec p))
         (p (source-optimize p))
         (dummy
          (begin
            (when (optimizer-output)
               (pretty-print (unparse-pretty p) (current-error-port)))
            #f))
         (p (rewrite-assignments p))
         (p (if (perform-tag-analysis)
                (introduce-tags p)
                p))
         (p (introduce-vars p))
         (p (sanitize-bindings p))
         (p (optimize-for-direct-jumps p))
         (p (insert-global-assignments p))
         (p (convert-closures p))
         (p (optimize-closures/lift-codes p)))
    (let ((ls* (alt-cogen p)))
      (when (assembler-output)
        (parameterize ((gensym-prefix "L")
                       (print-gensym  #f))
          (for-each (lambda (ls)
		      (newline)
		      ($for-each/stx print-instr ls))
	    ls*)))
      (let ((code* (assemble-sources
		    (lambda (x)
		      (if (closure? x)
			  (if (null? (closure-free* x))
			      (code-loc-label (closure-code x))
			    (error 'compile "BUG: non-thunk escaped" x))
			#f))
		    ls*)))
        (car code*)))))

(define compile-core-expr-to-port
  (lambda (expr port)
    (fasl-write (compile-core-expr->code expr) port)))


(define (compile-core-expr x)
  (let ((code (compile-core-expr->code x)))
    ($code->closure code)))

(define assembler-output (make-parameter #f))

(define current-core-eval
  (make-parameter
    (lambda (x) ((compile-core-expr x)))
    (lambda (x)
      (if (procedure? x)
          x
          (die 'current-core-eval "not a procedure" x)))))

(define eval-core
  (lambda (x)
    ((current-core-eval) x)))

(include "ikarus.compiler.altcogen.ss")

(define current-primitive-locations
  (let ((plocs (lambda (x) #f)))
    (case-lambda
      (() plocs)
      ((p)
       (if (procedure? p)
           (begin
             (set! plocs p)
             (refresh-cached-labels!))
           (error 'current-primitive-locations "not a procedure" p))))))

(define (expand/pretty x env who . passes)
  (unless (environment? env)
    (die who "not an environment" env))
  (let-values (((x libs) (core-expand x env)))
    (let f ((x (recordize x)) (passes passes))
      (if (null? passes)
          (unparse-pretty x)
          (f ((car passes) x) (cdr passes))))))


(define expand/scc-letrec
  (case-lambda
    ((x) (expand/scc-letrec x (interaction-environment)))
    ((x env)
     (expand/pretty x env 'expand/scc-letrec
       (lambda (x)
         (parameterize ((open-mvcalls #f))
            (optimize-direct-calls x)))
       (lambda (x)
         (parameterize ((debug-scc #t))
            (optimize-letrec x)))))))

(define expand/optimize
  (case-lambda
    ((x) (expand/optimize x (interaction-environment)))
    ((x env)
     (expand/pretty x env 'expand/optimize
       (lambda (x)
         (parameterize ((open-mvcalls #f))
            (optimize-direct-calls x)))
       optimize-letrec
       source-optimize))))

(define expand
  (case-lambda
    ((x) (expand x (interaction-environment)))
    ((x env) (expand/pretty x env 'expand))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'struct-case 'scheme-indent-function 1)
;; eval: (put 'make-conditional 'scheme-indent-function 1)
;; End:
