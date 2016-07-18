;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.compiler.typedefs)
  (export
    prelex				prelex?
    make-prelex				make-prelex-for-tmp-binding
    prelex-name
    prelex-operand			set-prelex-operand!
    prelex-source-referenced?		set-prelex-source-referenced?!
    prelex-source-assigned?		set-prelex-source-assigned?!
    prelex-residual-referenced?		set-prelex-residual-referenced?!
    prelex-residual-assigned?		set-prelex-residual-assigned?!
    prelex-referenced-clambda		set-prelex-referenced-clambda!
    prelex-global-location		set-prelex-global-location!

    $prelex-name
    $prelex-operand			$set-prelex-operand!
    $prelex-source-referenced?		$set-prelex-source-referenced?!
    $prelex-source-assigned?		$set-prelex-source-assigned?!
    $prelex-residual-referenced?	$set-prelex-residual-referenced?!
    $prelex-residual-assigned?		$set-prelex-residual-assigned?!
    $prelex-referenced-clambda		$set-prelex-referenced-clambda!
    $prelex-global-location		$set-prelex-global-location!

    assign				assign?
    make-assign
    assign-lhs
    assign-rhs

    seq					seq?
    make-seq				multiple-forms-sequence
    seq-e0
    seq-e1

    conditional				conditional?
    make-conditional
    conditional-test
    conditional-conseq
    conditional-altern

    funcall				funcall?
    make-funcall
    funcall-op
    funcall-rand*

    forcall				forcall?
    make-forcall
    forcall-op
    forcall-rand*

    bind				bind?
    make-bind
    bind-lhs*
    bind-rhs*
    bind-body

    fix					fix?
    make-fix
    fix-lhs*
    fix-rhs*
    fix-body

    recbind				recbind?
    make-recbind
    recbind-lhs*
    recbind-rhs*
    recbind-body

    rec*bind				rec*bind?
    make-rec*bind
    rec*bind-lhs*
    rec*bind-rhs*
    rec*bind-body

    primref				primref?
    make-primref			mk-primref
    primref-name
    $primref-name

    clambda				clambda?
    make-clambda
    clambda-label
    clambda-cases
    clambda-cp
    clambda-freevar*
    clambda-name

    $clambda-cases

    clambda-case			clambda-case?
    make-clambda-case
    clambda-case-info
    clambda-case-body
    $clambda-case-info
    $clambda-case-body

    case-info				case-info?
    make-case-info
    case-info-label
    case-info-args
    case-info-proper

    constant				constant?
    make-constant
    constant-value
    VOID-CONSTANT

    typed-expr				typed-expr?
    make-typed-expr
    typed-expr-expr
    typed-expr-type

    var					var?
    make-var				make-unique-var
    var-name
    var-reg-conf			set-var-reg-conf!
    var-frm-conf			set-var-frm-conf!
    var-var-conf			set-var-var-conf!
    var-reg-move			set-var-reg-move!
    var-frm-move			set-var-frm-move!
    var-var-move			set-var-var-move!
    var-loc				set-var-loc!
    var-index				set-var-index!
    var-global-location			set-var-global-location!
    $var-reg-conf			$set-var-reg-conf!
    $var-frm-conf			$set-var-frm-conf!
    $var-var-conf			$set-var-var-conf!
    $var-reg-move			$set-var-reg-move!
    $var-frm-move			$set-var-frm-move!
    $var-var-move			$set-var-var-move!
    $var-loc				$set-var-loc!
    $var-index				$set-var-index!
    $var-global-location		$set-var-global-location!

    jmpcall				jmpcall?
    make-jmpcall
    jmpcall-label
    jmpcall-op
    jmpcall-rand*

    closure-maker			closure-maker?
    make-closure-maker
    closure-maker-code			set-closure-maker-code!
    closure-maker-freevar*		set-closure-maker-freevar*!
    $closure-maker-code			$set-closure-maker-code!
    $closure-maker-freevar*		$set-closure-maker-freevar*!

    primopcall				primopcall?
    make-primopcall
    primopcall-op
    primopcall-rand*

    asmcall				asmcall?
    make-asmcall
    asmcall-instr
    asmcall-rand*

    codes				codes?
    make-codes
    codes-list
    codes-body

    object				object?
    make-object
    object-val

    code-loc				code-loc?
    make-code-loc
    code-loc-label

    foreign-label			foreign-label?
    make-foreign-label
    foreign-label-label

    known				known?
    make-known
    known-expr
    known-type

    shortcut				shortcut?
    make-shortcut
    shortcut-body
    shortcut-handler

    fvar				fvar?
    make-fvar				mkfvar
    fvar-idx
    $fvar-idx

    locals				locals?
    make-locals
    locals-vars
    locals-body

    nfv					nfv?
    make-nfv
    nfv-idx				set-nfv-idx!
    nfv-loc				set-nfv-loc!
    nfv-var-conf			set-nfv-var-conf!
    nfv-frm-conf			set-nfv-frm-conf!
    nfv-nfv-conf			set-nfv-nfv-conf!
    $nfv-idx				$set-nfv-idx!
    $nfv-loc				$set-nfv-loc!
    $nfv-var-conf			$set-nfv-var-conf!
    $nfv-frm-conf			$set-nfv-frm-conf!
    $nfv-nfv-conf			$set-nfv-nfv-conf!

    non-tail-call-frame			non-tail-call-frame?
    make-non-tail-call-frame
    non-tail-call-frame-rand*		set-non-tail-call-frame-rand*!
    non-tail-call-frame-live		set-non-tail-call-frame-live!
    non-tail-call-frame-body		set-non-tail-call-frame-body!

    non-tail-call			non-tail-call?
    make-non-tail-call
    non-tail-call-target
    non-tail-call-retval-var
    non-tail-call-all-rand*
    non-tail-call-mask
    non-tail-call-size

    asm-instr				asm-instr?
    make-asm-instr
    asm-instr-op			set-asm-instr-op!
    asm-instr-dst			set-asm-instr-dst!
    asm-instr-src			set-asm-instr-src!

    disp				disp?
    make-disp
    disp-objref
    disp-offset

    ;; high-level assembly instructions
    asm
    nop
    interrupt)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers))


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
;;   (getprop 'list (system-value-gensym)) => ?loc-gensym-of-list
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


;;;; high-level assembly instructions

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


;;;; done

#| end of library |# )

;;; end of file
