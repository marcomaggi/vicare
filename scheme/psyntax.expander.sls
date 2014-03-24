;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;; copyright notice for the original code of the XOR macro
;;;
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;;copyright notice for the original code of RECEIVE
;;;
;;;Copyright (C) John David Stone (1999). All Rights Reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;; introduction: bibliography
;;
;;There  are   multiple  documents  we   can  study  to  get   a  better
;;understanding of  the expander's code.   Most likely the  top document
;;is:
;;
;;[1] Abdulaziz  Ghuloum.  "Implicit Phasing for  Library Dependencies".
;;    Ph.D. thesis.  Department of Computer Science, Indiana University.
;;    December 2008.
;;
;;from the very author of this library; it is quite long.  Here are some
;;papers on syntax-case macros and R6RS libraries:
;;
;;[2]  R.   Kent  Dybvig.   "Writing  Hygienic  Macros  in  Scheme  with
;;    Syntax-Case".  Department of Computer Science, Indiana University.
;;    Technical Report 356.  June 1992.
;;
;;[3] Abdulaziz  Ghuloum, R.  Kent  Dybvig.  "Implicit Phasing  for R6RS
;;    Libraries".  Department of Computer Science, Indiana University.
;;
;;and the very paper that introduced hygienic macros:
;;
;;[4] Eugene  Kohlbecker, Daniel P. Friedman,  Matthias Felleisen, Bruce
;;    Duba.   "Hygienic   Macro  Expansion".   Department   of  Computer
;;    Science, Indiana University.  1986.
;;
;;Here  is another  long document  covering muliple  components of  this
;;Scheme implementation:
;;
;;[5] Oscar  Waddell.  "Extending  the Scope of  Syntactic Abstraction".
;;    Ph.D. thesis.  Department of Computer Science, Indiana University.
;;    August 1999.
;;
;;A discussion of  syntax parameters as implemented in Racket  is in the
;;paper:
;;
;;[6] Eli  Barzilay, Ryan  Culpepper, Matthew  Flatt. "Keeping  it clean
;;    with syntax parameters".  2011.
;;


;;;; introduction: evaluation times
;;
;;Throughout the  code there  are several  references to  different code
;;evaluation  times.  The  following  libraries from  [1]  allows us  to
;;explore what those times mean.
;;
;; (library (times-demo)
;;   (export
;;     call-time invoke-time expand-time visit-time compile-time)
;;   (import (vicare)
;;     (prefix (srfi :19)
;;             srfi.))
;;
;;   (define call-time
;;     (lambda ()
;;       (receive-and-return (S)
;;           (srfi.date->string (srfi.current-date))
;;         (printf "call time: ~a\n" S))))
;;
;;   (define invoke-time
;;     (let ((t (receive-and-return (S)
;;                  (srfi.date->string (srfi.current-date))
;;                (printf "invoke time: ~a\n" S))))
;;       (lambda () t)))
;;
;;   (define-syntax expand-time
;;     (lambda (stx)
;;       (receive-and-return (S)
;;           (srfi.date->string (srfi.current-date))
;;         (printf "expand time: ~a\n" S))))
;;
;;   (define-syntax visit-time
;;     (let ((t (receive-and-return (S)
;;                  (srfi.date->string (srfi.current-date))
;;                (printf "visit time: ~a\n" S))))
;;       (lambda (stx) t)))
;;
;;   (define-syntax compile-time
;;     (lambda (stx)
;;       (let-syntax ((t (lambda (stx)
;;                         (receive-and-return (S)
;;                             (srfi.date->string (srfi.current-date))
;;                           (printf "compile time: ~a\n" S)))))
;;         (t))))
;;
;;   #| end of library |# )
;;


;;;; introduction: lexical variables, labels, location gensyms
;;
;;Let's consider the example library:
;;
;;   (library (demo)
;;     (export this)
;;     (import (vicare))
;;     (define this 8)
;;     (define that 9)
;;     (let ((a 1))
;;       (let ((a 2))
;;         (list a this that))))
;;
;;and concentrate on the body:
;;
;;   (define this 8)
;;   (define that 9)
;;   (let ((a 1))
;;     (let ((a 2))
;;       (list a this that)))
;;
;;This   code  defines   4  syntactic   bindings:  THIS   and  THAT   as
;;library-scoped  lexical variables,  of  which THIS  is also  exported;
;;outer  A as  internal lexical  variable; inner  A as  internal lexical
;;variable.
;;
;;NOTE We indicate  as "global variables" the lexical  bindings that are
;;imported from another  library into the current scope;  we indicate as
;;"local variables"  the lexical bindings  that are defined in  the code
;;being expanded.
;;
;;After the expansion process every syntactic binding is renamed so that
;;its name is unique in the whole library body.  For example:
;;
;;  (define lex.this 8)
;;  (define lex.that 9)
;;  (let ((lex.a.1 1))
;;    (let ((lex.a.2 2))
;;      (list lex.a.2 lex.this lex.that)))
;;
;;notice that  LIST is still there  because it is part  of the top-level
;;environment (it  is a binding  exported by  the boot image);  the code
;;undergoes the following lexical variable name substitutions:
;;
;;  original name | lexical variable name
;;  --------------+----------------------
;;          this  | lex.this
;;          that  | lex.that
;;        outer a | lex.a.1
;;        inner a | lex.a.2
;;
;;where the "lex.*"  symbols are gensyms; such gensyms  are indicated as
;;"lex".
;;
;;Renaming  bindings  is one  of  the  core  purposes of  the  expansion
;;process; it is  performed while visiting the source code  as a tree in
;;breadth-first order.
;;
;;The expansion  process is complex  and can it  be described only  by a
;;complex  data structure:  the  lexical environment,  a composite  data
;;structure  resulting from  the conceptual  union among  component data
;;structures.
;;
;;Lexical contours and ribs
;;-------------------------
;;
;;To distinguish among  different bindings with the same  name, like the
;;two local  bindings both named A  in the example, we  must distinguish
;;among  different  "lexical contours"  that  is:  different regions  of
;;visibility for a set of bindings.  Every LET-like syntax defines a new
;;lexical  contour;  lexical  contours  can be  nested  by  nesting  LET
;;syntaxes; the library namespace is a lexical contour itself.
;;
;;    -------------------------------------------------
;;   | (define this 8)              ;top-level contour |
;;   | (define that 9)                                 |
;;   | (let ((a 1))                                    |
;;   |  -----------------------------------------      |
;;   | |                            ;contour 1   |     |
;;   | | (let ((a 2))                            |     |
;;   | |  -------------------------------------  |     |
;;   | | |                          ;contour 2 | |     |
;;   | | | (list a this that)                  | |     |
;;   | |  -------------------------------------  |     |
;;   | |   )                                     |     |
;;   |  -----------------------------------------      |
;;   |   )                                             |
;;    -------------------------------------------------
;;
;;An EQ?-unique object is assigned to each lexical contour; such objects
;;are called "marks".  In practice  each syntactic binding is associated
;;to the mark representing its  visibility region.  So the original code
;;is accompanied by the associations:
;;
;;  original name | lexical contour mark
;;  --------------+---------------------
;;          this  | top-mark
;;          that  | top-mark
;;        outer a |   1-mark
;;        inner a |   2-mark
;;
;;which  are registered  in a  component of  the lexical  environment: a
;;record of  type <RIB>.  Every lexical  contour is described by  a rib;
;;the rib for the top-level contour holds the associations:
;;
;;  original name | lexical contour mark
;;  --------------+---------------------
;;          this  | top-mark
;;          that  | top-mark
;;
;;the rib of the outer LET holds the associations:
;;
;;  original name | lexical contour mark
;;  --------------+---------------------
;;        outer a |   1-mark
;;
;;the rib of the inner LET holds the associations:
;;
;;  original name | lexical contour mark
;;  --------------+---------------------
;;        inner a |   2-mark
;;
;;While the  code is being visited  by the expander: syntax  objects are
;;created to  represent all the  binding names; such syntax  objects are
;;called "identifiers".  Each identifier is a data structure holding the
;;mark of its definition contour among its fields.
;;
;;
;;Label gensyms and ribs
;;----------------------
;;
;;An EQ?-unique object  is assigned to each syntactic  binding: a gensym
;;indicated as  "label"; such  associations are also  stored in  the rib
;;representing a lexical contour:
;;
;;  original name | lexical contour mark | label
;;  --------------+----------------------+---------
;;          this  | top-mark             | lab.this
;;          that  | top-mark             | lab.that
;;        outer a |   1-mark             | lab.a.1
;;        inner a |   2-mark             | lab.a.2
;;
;;where the symbols "lab.*" are gensyms.
;;
;;
;;Lexical variable gensyms and LEXENV
;;-----------------------------------
;;
;;The fact that  the "lex.*" symbols in the expanded  code are syntactic
;;bindings representing lexical variables is  registered in a portion of
;;the lexical environment indicated LEXENV.RUN or LEXENV.EXPAND.  So the
;;expanded code is accompanied by the association:
;;
;;    label  | lexical variables
;;  ---------+------------------
;;  lab.this | lex.this
;;  lab.that | lex.that
;;  lab.a.1  | lex.a.1
;;  lab.a.2  | lex.a.2
;;
;;Notice that, after  the expansion: the original names  of the internal
;;bindings (those  defined by LET)  do not matter anymore;  the original
;;names  of  the  non-exported  library-scoped bindings  do  not  matter
;;anymore;  only  the  original  name  of  the  exported  library-scoped
;;bindings is still important.
;;
;;Storage location gensyms and EXPORT-ENV
;;---------------------------------------
;;
;;About the value of lexical variables:
;;
;;* The  value of internal bindings  (those created by LET)  goes on the
;;  Scheme stack, and it exists only while the code is being evaluated.
;;
;;* The value of library-scoped  bindings (those created by DEFINE) must
;;  be stored in some persistent location, because it must exist for the
;;  whole time the library is loaded in a running Vicare process.
;;
;;But where is a library-scoped variable's value stored?  The answer is:
;;gensyms  are  created  for  the  sole purpose  of  acting  as  storage
;;locations  for  library-scoped  lexical variables,  such  gensyms  are
;;indicated as "loc".  Under Vicare,  symbols are data structures having
;;a  "value"   slot:  such  slot   has  SYMBOL-VALUE  as   accessor  and
;;SET-SYMBOL-VALUE! as mutator and it is used as storage location.
;;
;;So the expanded code is accompanied by the following association:
;;
;;    label  | location gensym
;;  ---------+----------------
;;  lab.this | loc.this
;;  lab.that | loc.that
;;
;;where the "loc.*"  are gensyms.  To represent  the association between
;;the library-scoped lexical variable labels (both the exported ones and
;;the  non-exported  ones)  and  their  storage  location  gensyms,  the
;;expander builds a data structure indicated as EXPORT-ENV.
;;
;;
;;Exported bindings and EXPORT-SUBST
;;----------------------------------
;;
;;Not all  the global lexical variables  are exported by a  library.  To
;;list  those that  are,  a data  structure is  built  and indicated  as
;;EXPORT-SUBST;  such data  structure  associates the  external name  of
;;exported bindings to their label  gensym.  For the example library the
;;EXPORT-SUBST represents the association:
;;
;;    label  | external name
;;  ---------+--------------
;;  lab.this | this
;;
;;If the EXPORT specification renames a bindings as in:
;;
;;   (export (rename this external-this))
;;
;;then the EXPORT-SUBST represents the association:
;;
;;    label  | external name
;;  ---------+--------------
;;  lab.this | external-this
;;


;;;; introduction: lexical environment, syntactic binding descriptors
;;
;;A syntactic  binding is the  association between  a name and  a value.
;;Syntactic bindings are created by LAMBDA and its variants, LET and its
;;variants,  DEFINE and  its variants,  DEFINE-SYNTAX and  its variants,
;;LET-SYNTAX and its variants.
;;
;;A syntactic binding descriptor has the following format:
;;
;;   (?binding-type . ?binding-value)
;;
;;where  ?BINDING-TYPE is  a  symbol and  the  format of  ?BINDING-VALUE
;;depends on the binding type.
;;
;;A syntactic  binding is  uniquely associated to  a label  gensym; such
;;associations are registered in:
;;
;;* A  set of label gensyms  associated to the bindings  exported by the
;;  boot image.
;;
;;* The interactive and non-interactive environment records.
;;
;;* The LEXENV data structures.
;;


;;;; introduction: lexical environments, the boot image component
;;
;;The  syntactic bindings  exported  by  the boot  image  are all  those
;;exported by the librarys "(psyntax system $all)".
;;
;;Under Vicare:  Scheme symbols  are data structures  with a  slot named
;;"value",  accessor  "symbol-value"  and  mutator  "set-symbol-value!",
;;usually    initialised     to    the    special     built-in    object
;;"#<unbound-object>".   When  a  label  is associated  to  a  syntactic
;;binding exported by the boot image, the binding's descriptor is stored
;;in the "value" slot.
;;
;;We can easily inspect such situation as follows:
;;
;;   (import (vicare))
;;   (define env (environment '(psyntax system $all)))
;;
;;   (define-values (label1 syntactic-binding-descriptor1)
;;     (environment-binding 'display env))
;;   syntactic-binding-descriptor1	=> (core-prim . display)
;;   (symbol-value label1)		=> (core-prim . display)
;;   (eq? syntactic-binding-descriptor1
;;        (symbol-value label1))	=> #t
;;
;;   (define-values (label2 syntactic-binding-descriptor2)
;;     (environment-binding 'lambda env))
;;   syntactic-binding-descriptor2	=> (core-macro . lambda)
;;   (symbol-value label2)		=> (core-macro . lambda)
;;   (eq? syntactic-binding-descriptor2
;;        (symbol-value label2))	=> #t
;;
;;   (define-values (label3 syntactic-binding-descriptor3)
;;     (environment-binding 'let env))
;;   syntactic-binding-descriptor3	=> (macro . let)
;;   (symbol-value label3)		=> (macro . let)
;;   (eq? syntactic-binding-descriptor3
;;        (symbol-value label3))	=> #t
;;
;;   (define-values (label4 syntactic-binding-descriptor4)
;;     (environment-binding '&condition env))
;;   syntactic-binding-descriptor4	=> ($core-rtd $condition-rtd &condition-rcd)
;;   (symbol-value label4)		=> ($core-rtd $condition-rtd &condition-rcd)
;;   (eq? syntactic-binding-descriptor4
;;        (symbol-value label4))	=> #t
;;
;;The  syntactic binding  exported by  the boot  image have  descriptors
;;defined in, and only in, "makefile.sps".
;;
;;
;;Boot image entry types
;;======================
;;
;;Core language syntax
;;--------------------
;;
;;Some syntaxes  are built-in  in the  expander and do  not even  have a
;;transformer function: they  are part of the core  language.  A binding
;;representing a core language syntax has the format:
;;
;;   (?name . ())
;;
;;where ?NAME is a symbol representing the name of the syntax.
;;
;;
;;Core macro
;;----------
;;
;;A binding representing a core macro integrated in the expander has the
;;format:
;;
;;   (core-macro . ?name)
;;
;;where ?NAME is  a symbol representing the macro name.   The core macro
;;transformer functions  are implemented by  the expander in  the module
;;exporting the  function CORE-MACRO-TRANSFORMER,  which is used  to map
;;core macro names to transformer functions.
;;
;;
;;Non-core macro
;;--------------
;;
;;A binding representing a non-core macro integrated in the expander has
;;the format:
;;
;;   (macro . ?name)
;;
;;where ?NAME  is a  symbol representing the  macro name.   The non-core
;;macro transformer  functions are  implemented by  the expander  in the
;;module  exporting the  function  NON-CORE-MACRO-TRANSFORMER, which  is
;;used to map non-core macro names to transformer functions.
;;
;;
;;Core primitive
;;--------------
;;
;;A syntactic binding representing a core primitive exported by the boot
;;image has the format:
;;
;;   (core-prim . ?name)
;;
;;where:  "core-prim"  is the  symbol  "core-prim";  ?NAME is  a  symbol
;;representing the name of the core primitive.
;;
;;
;;Core R6RS record type descriptor
;;--------------------------------
;;
;;A binding representing  R6RS's record type descriptor  exported by the
;;boot image has the format:
;;
;;   ($core-rtd . (?rtd-sym ?rcd-sym))
;;
;;where: "$core-rtd"  is the  symbol "$core-rtd";  ?RTD-SYM is  a symbol
;;representing  the name  of the  identifier  to which  the record  type
;;descriptor was originally bound; ?RCD-SYM is a symbol representing the
;;name  of  the  identifier  to which  the  default  record  constructor
;;descriptor was originally bound.
;;
;;For example: these  entries are used to represent  the predefined R6RS
;;condition object types.
;;
;;
;;Fluid syntax
;;------------
;;
;;A binding representing a fluid syntax has the format:
;;
;;   ($fluid . ?name-sym)
;;
;;where  ?NAME-SYM is  the symbol  representing  the name  of the  fluid
;;syntax and  it is used  as fluid label  to re-define the  binding.  At
;;present (Thu  Jan 30,  2014) the  boot image does  not export  a fully
;;defined fluid syntax.
;;
;;Let's draw  the picture.  When  a fluid  syntax binding is  created by
;;DEFINE-FLUID-SYNTAX in the source code of a library or program:
;;
;;   (define-fluid-syntax ?lhs ?rhs)
;;
;;an identifier ?LHS is associated to  a main label ?LABEL, and an entry
;;is pushed on the lexical environment:
;;
;;   (?label . ($fluid . ?fluid-label))
;;
;;at the same time another entry is pushed on the lexical environment:
;;
;;   (?fluid-label . ?syntactic-binding)
;;
;;where ?SYNTACTIC-BINDING is the concrete binding descriptor created by
;;expanding  and evaluating  ?RHS  then interpreting  its return  value.
;;Given the identifier  ?LHS: we can retrieve the  associated ?LABEL and
;;so  the ?FLUID-LABEL;  then we  can "follow  through" ?FLUID-LABEL  to
;;retrieve the actual binding descriptor.
;;
;;The  fluid syntax  can  be re-defined  any number  of  times by  using
;;FLUID-LET-SYNTAX:
;;
;;   (fluid-let-syntax ((?lhs ?inner-rhs)) . ?body)
;;
;;causing other entries  associated to ?FLUID-LABEL to be  pushed on the
;;LEXENV:
;;
;;   (?fluid-label . ?inner-syntactic-binding)
;;
;;where  ?INNER-SYNTACTIC-BINDING is  the  binding descriptor  resulting
;;from expanding and evaluating  ?INNER-RHS then interpreting its return
;;value.
;;
;;Fine.  This is  *not* what happens for the fluid  syntaxes exported by
;;the boot  image like RETURN,  BREAK and CONTINUE.  For  these syntaxes
;;the lexical environment  of the boot image includes  only entries with
;;binding descriptor:
;;
;;   ($fluid . return)
;;   ($fluid . break)
;;   ($fluid . continue)
;;
;;and  there are  no  entries for  the fluid  labels  RETURN, BREAK  and
;;CONTINUE.  The identifiers  for these fluid syntaxes are  bound in the
;;environment, but they are bound to "nothing"; trying to follow through
;;the fluid labels to the actual  binding descriptors will result in the
;;binding descriptor:
;;
;;   (displaced-lexical . #f)
;;
;;Such  half-defined  fluid  syntaxes  are  fully  usable  as  auxiliary
;;syntaxes and can be re-defined  with FLUID-LET-SYNTAX.  There are *no*
;;transformer  functions   for  RETURN,  BREAK,  CONTINUE   and  similar
;;syntaxes.
;;
;;
;;Some words about core primitive values
;;======================================
;;
;;Every  core primitive  value has  a name  that is  considered part  of
;;Vicare's core language; such primitive  name is the actual public name
;;of the binding  exported by the boot image.  The  name of the function
;;DISPLAY  is  the  symbol  "display";   the  name  of  the  record-type
;;descriptor &CONDITION-RTD is the symbol "&condition-rtd".
;;
;;The property list of the name  contains a special entry whose value is
;;the storage location gensym of the core primitive value; we can easily
;;inspect such situation as follows:
;;
;;   (import (vicare)
;;     (only (vicare system $symbols)
;;           system-value-gensym))
;;
;;   (getprop 'display system-value-gensym)
;;   => loc.display
;;
;;   (symbol-value (getprop 'display system-value-gensym))
;;   => #<procedure display>
;;
;;   (system-value 'display)
;;   => #<procedure display>
;;
;;   (system-value '&condition-rtd)
;;   => #[rtd name=&condition ...]
;;
;;Notice that the gensym bound to SYSTEM-VALUE-GENSYM is different every
;;time we rebuild the boot image.
;;


;;;; introduction: lexical environments, the LEXENV component
;;
;;A  LEXENV  is an  alist  managed  somewhat  like  a stack;  while  the
;;expansion  proceeds, visiting  the  code in  breadth-first order:  the
;;LEXENV is updated by pushing new  entries on the stack.  Each entry is
;;a  pair,  list  or improper  list  and  maps  a  label gensym  to  its
;;associated syntactic binding descriptor.
;;
;;A LEXENV entry has the following format:
;;
;;   (?label . ?syntactic-binding)
;;
;;where: ?LABEL  is a label  gensym uniquely associated to  the binding;
;;?SYNTACTIC-BINDING is a syntactic binding descriptor.
;;
;;
;;LEXENV entry types
;;==================
;;
;;Library lexical variables
;;-------------------------
;;
;;A syntactic binding representing a lexical variable, as created by LET
;;and similar syntaxes, LAMBDA, CASE-LAMBDA or DEFINE, has the format:
;;
;;   (lexical . (?lexvar . ?mutated))
;;
;;where  "lexical"  is  the  symbol   "lexical";  ?LEXVAR  is  a  gensym
;;representing the name of the  lexical variable binding in the expanded
;;code; ?MUTATED is  a boolean, true if somewhere in  the code the value
;;of this binding is mutated.
;;
;;We want to keep  track of mutated variables because we  do not want to
;;export from a library a mutable variable.
;;
;;
;;Imported lexical variables
;;--------------------------
;;
;;A  syntactic binding  representing  a lexical  variable imported  from
;;another library has the format:
;;
;;   (global . (?library . ?loc))
;;
;;where:  ?LIBRARY represents  the  library from  which  the binding  is
;;exported, ?LOC  is the gensym  containing the variable's value  in its
;;"value" field.
;;
;;When the  variable is defined  by an  imported library: ?LIBRARY  is a
;;record of type  LIBRARY.  When the variable was defined  by a previous
;;REPL expression: ?LIBRARY is the symbol "*interaction*".
;;
;;Labels   associated  to   these  imported   bindings  have   the  list
;;representing the binding itself stored in their "value" fields.
;;
;;
;;Library non-identifier macro
;;----------------------------
;;
;;A binding  representing a macro with  non-variable transformer defined
;;by the code being expanded has the format:
;;
;;   (local-macro . (?transformer . ?expanded-expr))
;;
;;where: ?TRANSFORMER is a  function implementing the macro transformer;
;;?EXPANDED-EXPR is  the expression in fully  expanded code representing
;;the right-hand side of the  syntax definition.
;;
;;?TRANSFORMER is the result of compiling and evaluating ?EXPANDED-EXPR.
;;
;;
;;Library identifier macro
;;------------------------
;;
;;A binding  representing a macro  with variable transformer  defined by
;;the code being expanded has the format:
;;
;;   (local-macro! . (?transformer . ?expanded-expr))
;;
;;where: ?TRANSFORMER is a  function implementing the macro transformer;
;;?EXPANDED-EXPR is  the expression in fully  expanded code representing
;;the right-hand side of the syntax definition.
;;
;;?TRANSFORMER is the result of compiling and evaluating ?EXPANDED-EXPR.
;;
;;
;;Imported non-identifier macro
;;-----------------------------
;;
;;A binding representing a macro with a non-variable transformer defined
;;by code in an imported library has the format:
;;
;;   (global-macro . (?library . ?loc))
;;
;;where: ?LIBRARY  is a  record of type  LIBRARY describing  the library
;;from which  the macro is exported;  ?LOC is the gensym  containing the
;;transformer function in its "value" field.
;;
;;Labels   associated  to   these  imported   bindings  have   the  list
;;representing the binding itself stored in their "value" fields.
;;
;;
;;Imported identifier macro
;;-------------------------
;;
;;A binding  representing a macro  with variable transformer  defined by
;;code in an imported library has the format:
;;
;;   (global-macro! . (?library . ?loc))
;;
;;where: ?LIBRARY  is a  record of type  LIBRARY describing  the library
;;from which  the macro is exported;  ?LOC is the gensym  containing the
;;transformer function in its "value" field.
;;
;;Labels   associated  to   these  imported   bindings  have   the  list
;;representing the binding itself stored in their "value" fields.
;;
;;
;;Library compile-time value
;;--------------------------
;;
;;A binding representing a compile-time  value defined by the code being
;;expanded has the format:
;;
;;   (local-ctv . (?object . ?expanded-expr))
;;
;;where:  ?OBJECT  is   the  actual  value  computed   at  expand  time;
;;?EXPANDED-EXPR is the result of fully expanding the right-hand side of
;;the syntax definition.
;;
;;?OBJECT is the result of compiling and evaluating ?EXPANDED-EXPR.
;;
;;
;;Imported compile-time value
;;---------------------------
;;
;;A  binding  representing  a  compile-time value  exported  by  another
;;library has the format:
;;
;;   (global-ctv . (?library . ?loc))
;;
;;where: ?LIBRARY  is a  record of type  LIBRARY describing  the library
;;from which the binding is exported;  ?LOC is the gensym containing the
;;actual object in its "value" field.
;;
;;Labels   associated  to   these  imported   bindings  have   the  list
;;representing the binding itself stored in their "value" fields.
;;
;;
;;Module interface
;;----------------
;;
;;A binding representing the interface of a MODULE syntax defined by the
;;code being expanded has the format:
;;
;;   ($module . ?iface)
;;
;;where ?IFACE is a record of type "module-interface".
;;
;;
;;Pattern variable
;;----------------
;;
;;A binding representing  a pattern variable, as  created by SYNTAX-CASE
;;and SYNTAX-RULES, has the format:
;;
;;   (syntax . (?name . ?level))
;;
;;where:  "syntax"  is   the  symbol  "syntax";  ?NAME   is  the  symbol
;;representing  the   name  of  the   pattern  variable;  ?LEVEL   is  a
;;non-negative exact integer representing the ellipsis nesting level.
;;
;;The  SYNTAX-CASE  patterns below  will  generate  the given  syntactic
;;bindings:
;;
;;   ?a				->  (syntax . (?a . 0))
;;   (?a)			->  (syntax . (?a . 0))
;;   (((?a)))			->  (syntax . (?a . 0))
;;   (?a ...)			->  (syntax . (?a . 1))
;;   ((?a) ...)			->  (syntax . (?a . 1))
;;   ((((?a))) ...)		->  (syntax . (?a . 1))
;;   ((?a ...) ...)		->  (syntax . (?a . 2))
;;   (((?a ...) ...) ...)	->  (syntax . (?a . 3))
;;
;;
;;Library Vicare struct descriptor
;;--------------------------------
;;
;;A binding  representing a Vicare's  struct type descriptor  defined by
;;the code being expanded has the format:
;;
;;   ($rtd . #<type-descriptor-struct>)
;;
;;where "$rtd" is the symbol "$rtd".
;;
;;
;;Library R6RS record type descriptor
;;-----------------------------------
;;
;;A  binding  representing an  R6RS's  record  type descriptor  and  the
;;default  record  constructor  descriptor  defined by  the  code  being
;;expanded has one of the formats:
;;
;;   ($rtd . (?rtd-id ?rcd-id))
;;   ($rtd . (?rtd-id ?rcd-id . ?spec))
;;
;;where: "$rtd" is the symbol "$rtd"; ?RTD-ID is the identifier to which
;;the  record type  descriptor is  bound; ?RCD-ID  is the  identifier to
;;which the default record constructor  descriptor is bound; ?SPEC is an
;;instance of record type R6RS-RECORD-TYPE-SPEC.
;;
;;
;;Fluid syntax
;;------------
;;
;;A binding descriptor representing a fluid syntax has the format:
;;
;;   ($fluid . ?fluid-label)
;;
;;where ?FLUID-LABEL is the label gensym associated to the fluid syntax.
;;
;;Let's draw  the picture.  When  a fluid  syntax binding is  created by
;;DEFINE-FLUID-SYNTAX:
;;
;;   (define-fluid-syntax ?lhs ?rhs)
;;
;;an identifier ?LHS is associated to  a main label ?LABEL, and an entry
;;is pushed on the lexical environment:
;;
;;   (?label . ($fluid . ?fluid-label))
;;
;;at the same time another entry is pushed on the lexical environment:
;;
;;   (?fluid-label . ?syntactic-binding)
;;
;;where ?SYNTACTIC-BINDING is the concrete binding descriptor created by
;;expanding  and evaluating  ?RHS  then interpreting  its return  value.
;;Given the identifier  ?LHS: we can retrieve the  associated ?LABEL and
;;so  the ?FLUID-LABEL;  then we  can "follow  through" ?FLUID-LABEL  to
;;retrieve the actual binding descriptor.
;;
;;The  fluid syntax  can  be re-defined  any number  of  times by  using
;;FLUID-LET-SYNTAX:
;;
;;   (fluid-let-syntax ((?lhs ?inner-rhs)) . ?body)
;;
;;causing other entries  associated to ?FLUID-LABEL to be  pushed on the
;;LEXENV:
;;
;;   (?fluid-label . ?inner-syntactic-binding)
;;
;;where  ?INNER-SYNTACTIC-BINDING is  the  binding descriptor  resulting
;;from expanding and evaluating  ?INNER-RHS then interpreting its return
;;value.
;;
;;
;;Synonym syntax
;;--------------
;;
;;A binding descriptor representing an synonym syntax has the format:
;;
;;   ($synonym . ?synonym-label)
;;
;;where ?SYNONYM-LABEL  is the  label gensym  associated to  the synonym
;;syntax.
;;
;;Let's draw the picture.  When an synonym syntax binding is created:
;;
;;   (define-syntax ?lhs (make-synonym-transformer ?id))
;;
;;an identifier ?LHS is associated to  a main label ?LABEL, and an entry
;;is pushed on the lexical environment:
;;
;;   (?label . ($synonym . ?synonym-label))
;;
;;where ?SYNONYM-LABEL is the label  bound to the identifier ?ID.  Given
;;the identifier ?LHS: we can retrieve  the associated ?LABEL and so the
;;?SYNONYM-LABEL;  then  we  can   "follow  through"  ?SYNONYM-LABEL  to
;;retrieve the actual binding descriptor.
;;
;;
;;Displaced lexical
;;-----------------
;;
;;These lists  have a format  similar to  a LEXENV entry  representing a
;;syntactic binding, but they are used to represent a failed search into
;;a LEXENV.
;;
;;The following special value represents an unbound label:
;;
;;     (displaced-lexical . #f)
;;
;;The  following  special  value  represents the  result  of  a  lexical
;;environment query with invalid label value (not a symbol):
;;
;;     (displaced-lexical . ())
;;


;;;; introduction: EXPORT-ENV
;;
;;The EXPORT-ENV  is a data structure  used to map the  label gensyms of
;;global syntactic  bindings, defined  by a library  or program,  to the
;;corresponding storage  location gensyms.   "Global bindings"  does not
;;mean "exported bindings": not all  the entries in EXPORT-ENV represent
;;exported bindings,  it is the role  of the EXPORT-SUBST to  select the
;;exported ones.
;;
;;An EXPORT-ENV is an alist whose entries have the format:
;;
;;   (?label . ?export-binding)
;;
;;and ?EXPORT-BINDING has the format:
;;
;;   (?type . ?loc)
;;
;;or shortly the entries have the format:
;;
;;   (?label ?type . ?loc)
;;
;;where: ?LABEL is the label gensym; ?TYPE is a type symbol; ?LOC is the
;;storage location gensym.
;;
;;The symbol ?TYPE is one among:
;;
;;GLOBAL -
;;   To denote  a lexical immutated  variable.  In this case  ?LOC holds
;;   the variable's value (immutable).  This binding can be exported.
;;
;;MUTABLE -
;;   To denote a lexical variable that is mutated somewhere in the code.
;;   In this case ?LOC holds  the variable's value.  This binding cannot
;;   be exported.
;;
;;GLOBAL-MACRO -
;;   To  denote a  non-identifier macro.   In this  case ?LOC  holds the
;;   macro transformer,  but only  after the  library has  been visited.
;;   This binding can be exported.
;;
;;GLOBAL-MACRO! -
;;   To denote an  identifier macro.  In this case ?LOC  holds the macro
;;   transformer, but  only after  the library  has been  visited.  This
;;   binding can be exported.
;;
;;GLOBAL-CTV -
;;   To denote  a compile-time value.   In this  case the loc  holds the
;;   actual compile-time  object, but  only after  the library  has been
;;   visited.  This binding can be exported.
;;


;;;; core macros, non-core macros, user-defined macros
;;
;;First some notes on "languages" Vicare jargon:
;;
;;* The "expanded language" is a low level Scheme-like language which is
;;  the result of  the expansion process: expanding a  Scheme library or
;;  program  means  to transform  the  input  language into  a  symbolic
;;  expression in the expanded language.
;;
;;* The "core language" is a low level Scheme-like language which is the
;;  input recognised by the compiler.
;;
;;at present the expanded language and  the core language are equal, but
;;this might change in the future.
;;
;;
;;Core macros
;;-----------
;;
;;These are basic syntaxes into which all the other macros are expanded;
;;despite being  called "core",  they are neither  part of  the expanded
;;language nor of  the core language.  Core macros are  split into three
;;groups, those that can appear in definition context only:
;;
;;   define				define-syntax
;;   define-alias			define-fluid-syntax
;;   module				library
;;   begin				import
;;   export				set!
;;   stale-when				begin-for-syntax
;;   eval-for-expand			define-fluid-override
;;
;;and those that can appear only in expression context only:
;;
;;   foreign-call			quote
;;   syntax-case			syntax
;;   letrec				letrec*
;;   if					lambda
;;   case-lambda			fluid-let-syntax
;;   struct-type-descriptor		struct-type-and-struct?
;;   struct-type-field-ref		struct-type-field-set!
;;   $struct-type-field-ref		$struct-type-field-set!
;;   record-type-descriptor		record-constructor-descriptor
;;   record-type-field-set!		record-type-field-ref
;;   $record-type-field-set!		$record-type-field-ref
;;   type-descriptor			is-a?
;;   slot-ref				slot-set!
;;   $slot-ref				$slot-set!
;;   splice-first-expand		unsafe
;;   predicate-procedure-argument-validation
;;   predicate-return-value-validation
;;
;;those  that  can appear  in  both  definition context  and  expression
;;context:
;;
;;   let-syntax				letrec-syntax
;;
;;The implementation  of core macros  that appear in  definition context
;;only is integrated in the function CHI-BODY*.
;;
;;The implementation  of core macros  that appear in  definition context
;;only consists of proper transformer functions selected by the function
;;CORE-MACRO-TRANSFORMER.  Such transformers are  applied to input forms
;;by the function CHI-EXPR.
;;
;;Macros  that can  appear  in both  definition  context and  expression
;;context have double implementation: one  in the function CHI-BODY* and
;;one in the function CHI-EXPR.
;;
;;Core macros  can introduce  bindings by direct  access to  the lexical
;;environment: their  transformer functions  create new rib  records and
;;push new entries on the LEXENV.
;;
;;
;;Non-core macros
;;---------------
;;
;;These are macros that expand themselves into uses of core macros; they
;;have a  proper transformer function  accepting as single  argument the
;;input form syntax object and returning as single value the output form
;;syntax object.   The only  difference between a  non-core macro  and a
;;user-defined macro is that the former is integrated in the expander.
;;
;;Non-core   macro   transformers   are   selected   by   the   function
;;NON-CORE-MACRO-TRANSFORMER.
;;
;;
;;User-defined macros
;;-------------------
;;
;;These are macros defined  by DEFINE-SYNTAX, LET-SYNTAX, LETREC-SYNTAX,
;;DEFINE-FLUID-SYNTAX  and  their  derivatives.   Such  syntaxes  expand
;;themselves into  uses of core  or non-core macros.   Their transformer
;;functions accept as  single argument the input form  syntax object and
;;return as single value the output form syntax object.
;;


#!vicare
(library (psyntax expander)
  (export
    eval
    environment				environment?
    null-environment			scheme-report-environment
    interaction-environment		new-interaction-environment

    ;; inspection of non-interaction environment objects
    environment-symbols			environment-libraries
    environment-labels			environment-binding

    expand-form-to-core-language
    expand-top-level			expand-top-level->sexp
    expand-library			expand-library->sexp
    expand-r6rs-top-level-make-evaluator boot-library-expand

    make-variable-transformer		variable-transformer?
    variable-transformer-procedure

    make-synonym-transformer		synonym-transformer?
    synonym-transformer-identifier

    make-compile-time-value		compile-time-value?
    compile-time-value-object		syntax-parameter-value

    generate-temporaries		identifier?
    free-identifier=?			bound-identifier=?
    identifier-bound?
    datum->syntax			syntax->datum

    syntax-violation			assertion-error

    ;;This must  be exported and that's  it.  I am unable  to remove it.
    ;;Sue me.  (Marco Maggi; Sun Nov 17, 2013)
    syntax-error

    syntax-dispatch			syntax-transpose
    ellipsis-map

    ;;syntactic binding properties
    syntactic-binding-putprop
    syntactic-binding-getprop
    syntactic-binding-remprop
    syntactic-binding-property-list

    set-identifier-unsafe-variant!
    set-predicate-procedure-argument-validation!
    set-predicate-return-value-validation!

    ;; expand-time object type specs: parsing tagged identifiers
    tagged-identifier-syntax?			parse-tagged-identifier-syntax
    list-of-tagged-bindings?			parse-list-of-tagged-bindings
    tagged-callable-spec-syntax?		parse-tagged-callable-spec-syntax
    tagged-formals-syntax?			parse-tagged-formals-syntax
    standard-formals-syntax?			formal-tags-syntax?
    return-values-tags-syntax?

    make-callable-signature		callable-signature?
    callable-signature-formals		callable-signature-return-values
    make-formals-signature		formals-signature?		formals-signature-tags
    make-return-values-signature	return-values-signature?	return-values-signature-tags
    callable-signature-formals-tags
    callable-signature-return-values-tags
    callable-signature=?		formals-signature=?		return-values-signature=?

    ;; expand-time object type specs: identifiers defining types
    top-id
    tag-identifier?				all-tag-identifiers?
    tag-super-and-sub?
    identifier-object-type-spec			set-identifier-object-type-spec!
    label-object-type-spec			set-label-object-type-spec!

    make-object-type-spec			object-type-spec?
    object-type-spec-parent-spec
    object-type-spec-type-id			object-type-spec-pred-stx
    object-type-spec-accessor-maker		object-type-spec-mutator-maker
    object-type-spec-setter-maker		object-type-spec-dispatcher

    ;; expand-time object type specs: tagged binding identifiers
    tagged-identifier?
    set-identifier-tag!				identifier-tag
    set-label-tag!				label-tag
    set-identifier-callable-signature!		identifier-callable-signature
    set-label-callable-signature!		label-callable-signature

    ;; expand-time type specs: callable specs
    identifier-callable-spec		set-identifier-callable-spec!
    make-callable-spec			callable-spec?
    callable-spec-name			callable-spec-min-arity
    callable-spec-max-arity		callable-spec-dispatcher

    ;; expant-time type specs: stuff for built-in tags
    initialise-type-spec-for-built-in-object-types

    ;;The following are inspection functions for debugging purposes.
    (rename (<stx>?		syntax-object?)
	    (<stx>-expr		syntax-object-expression)
	    (<stx>-mark*	syntax-object-marks)
	    (<stx>-rib*		syntax-object-ribs)
	    (<stx>-ae*		syntax-object-source-objects)))
  (import (except (rnrs)
		  eval
		  environment		environment?
		  null-environment	scheme-report-environment
		  identifier?
		  bound-identifier=?	free-identifier=?
		  generate-temporaries
		  datum->syntax		syntax->datum
		  syntax-violation	make-variable-transformer
		  syntax-error)
    (prefix (rnrs syntax-case) sys.)
    (rnrs mutable-pairs)
    (psyntax library-manager)
    (psyntax builders)
    (psyntax compat)
    (psyntax config)
    (psyntax internal))


;;; helpers

;;This syntax can be used as standalone identifier and it expands to #f.
;;It is used  as "annotated expression" argument in calls  to the BUILD-
;;functions when there is no annotated expression to be given.
;;
(define-syntax no-source
  (lambda (x) #f))

(define-syntax-rule (reverse-and-append ?item**)
  (apply append (reverse ?item**)))

(define (false-or-procedure? obj)
  (or (not obj)
      (procedure? obj)))

(define (improper-list->list-and-rest ell)
  (let loop ((ell   ell)
	     (item* '()))
    (syntax-match ell ()
      ((?car . ?cdr)
       (loop ?cdr (cons ?car item*)))
      (()
       (values (reverse item*) '()))
      (_
       (values (reverse item*) ell)))
    ;; (cond ((pair? ell)
    ;; 	   (loop (cdr ell) (cons (car ell) item*)))
    ;; 	  ((null? ell)
    ;; 	   (values (reverse item*) '()))
    ;; 	  (else
    ;; 	   (values (reverse item*) ell)))
    ))

(define* (proper-list->head-and-last ell)
  (let loop ((ell   ell)
	     (item* '()))
    (syntax-match ell ()
      (()
       (assertion-violation __who__ "expected non-empty list" ell))
      ((?last)
       (values (reverse item*) ?last))
      ((?car . ?cdr)
       (loop ?cdr (cons ?car item*))))))

(define-syntax-rule (trace-define (?name . ?formals) . ?body)
  (define (?name . ?formals)
    (debug-print (quote ?name) 'arguments . ?formals)
    (call-with-values
	(lambda () . ?body)
      (lambda retvals
	(debug-print (quote ?name) 'retvals retvals)
	(apply values retvals)))))

(define (self-evaluating? x)
  (or (number?			x)
      (string?			x)
      (char?			x)
      (boolean?			x)
      (bytevector?		x)
      (keyword?			x)
      (would-block-object?	x)
      (unbound-object?		x)
      (bwp-object?		x)))

(define (non-compound-sexp? obj)
  (or (null? obj)
      (self-evaluating? obj)
      ;;Notice that struct instances are not self evaluating.
      (struct? obj)))


;;;; library records collectors

(define (make-collector)
  (let ((ls '()))
    (case-lambda
     (()
      ls)
     ((x)
      (unless (eq? x '*interaction*)
	(assert (library? x))
	;;Prepend  X to  the  list LS  if it  is  not already  contained
	;;according to EQ?.
	(set! ls (if (memq x ls)
		     ls
		   (cons x ls))))))))

(define imp-collector
  ;;Imported  libraries  collector.   Holds a  collector  function  (see
  ;;MAKE-COLLECTOR)  filled with  the LIBRARY  records representing  the
  ;;libraries from an R6RS import specification: every time the expander
  ;;parses an IMPORT syntax, the selected libraries are represented by a
  ;;LIBRARY record and such record is added to this collection.
  ;;
  (make-parameter
      (lambda args
        (assertion-violation 'imp-collector "BUG: not initialized"))
    (lambda (x)
      (unless (procedure? x)
	(assertion-violation 'imp-collector "BUG: not a procedure" x))
      x)))

(define inv-collector
  ;;Invoked  libraries  collector.   Holds  a  collector  function  (see
  ;;MAKE-COLLECTOR)  filled with  the LIBRARY  records representing  the
  ;;libraries defining "global" entries  in the lexical environment that
  ;;are used in the code we are expanding.
  ;;
  ;;The library:
  ;;
  ;;   (library (subdemo)
  ;;     (export sub-var)
  ;;     (import (vicare))
  ;;     (define sub-var 456))
  ;;
  ;;is imported by the library:
  ;;
  ;;   (library (demo)
  ;;     (export var sub-var)
  ;;     (import (vicare) (subdemo))
  ;;     (define var 123))
  ;;
  ;;which is imported by the program:
  ;;
  ;;   (import (vicare) (demo))
  ;;   (define (doit)
  ;;     (list var sub-var))
  ;;   (doit)
  ;;
  ;;when the  body of the function  is expanded the identifiers  VAR and
  ;;SUB-VAR are captured by bindings in the lexical environment with the
  ;;format:
  ;;
  ;;   (global . (?library . ?gensym))
  ;;
  ;;where  ?LIBRARY  is the  record  of  type LIBRARY  representing  the
  ;;library that  defines the variable  and ?GENSYM is a  symbol holding
  ;;the variable's value in its  "value" slot.  Such LIBRARY records are
  ;;added to the INV-COLLECTOR.
  ;;
  ;;For the  identifier VAR:  ?LIBRARY represents the  library "(demo)";
  ;;for  the   identifier  SUB-VAR:  ?LIBRARY  represents   the  library
  ;;"(subdemo)".  Notice  that while "(demo)"  is present in  the IMPORT
  ;;specification, and  so it is  also registered in  the IMP-COLLECTOR,
  ;;"(subdemo)" is not and it is only present in the INV-COLLECTOR.
  ;;
  (make-parameter
      (lambda args
        (assertion-violation 'inv-collector "BUG: not initialized"))
    (lambda (x)
      (unless (procedure? x)
	(assertion-violation 'inv-collector "BUG: not a procedure" x))
      x)))

(define vis-collector
  ;;Visit  libraries   collector.   Holds  a  collector   function  (see
  ;;MAKE-COLLECTOR)  which  is  meant  to be  filled  with  the  LIBRARY
  ;;records.   This  collector  holds  the libraries  collected  by  the
  ;;INV-COLLECTOR  when  expanding  the  right-hand  side  of  a  syntax
  ;;definition.
  ;;
  ;;The library:
  ;;
  ;;  (library (demo)
  ;;    (export var)
  ;;    (import (vicare))
  ;;    (define var 123))
  ;;
  ;;is loaded by the program:
  ;;
  ;;  (import (vicare)
  ;;    (for (demo) expand))
  ;;  (define-syntax doit (lambda (stx) var))
  ;;  (doit)
  ;;
  ;;the right-hand side of the syntax definition is the expression:
  ;;
  ;;  (lambda (stx) var)
  ;;
  ;;when such expression is expanded: the  identifier VAR is found to be
  ;;captured by a binding in the lexical environment with the format:
  ;;
  ;;   (global . (?library . ?gensym))
  ;;
  ;;where  ?LIBRARY  is the  record  of  type LIBRARY  representing  the
  ;;library "(demo)" and ?GENSYM is a  symbol holding 123 in its "value"
  ;;slot.   The record  ?LIBRARY is  added first  to INV-COLLECTOR  and,
  ;;after finishing the expansion of the right-hand side, it is moved to
  ;;the INV-COLLECTOR.  See %EXPAND-MACRO-TRANSFORMER for details.
  ;;
  (make-parameter
      (lambda args
        (assertion-violation 'vis-collector "BUG: not initialized"))
    (lambda (x)
      (unless (procedure? x)
	(assertion-violation 'vis-collector "BUG: not a procedure" x))
      x)))

;;; --------------------------------------------------------------------

(define stale-when-collector
  ;;Collects  test  expressions  from STALE-WHEN  syntaxes  and  LIBRARY
  ;;records needed for such expressions.  This parameter holds a special
  ;;collector  function  (see  %MAKE-STALE-COLLECTOR)  which  handles  2
  ;;collections:  one for  expanded expressions  representing STALE-WHEN
  ;;test  expressions, one  for  LIBRARY records  defining the  imported
  ;;variables needed by the test expressions.
  ;;
  ;;The library:
  ;;
  ;;   (library (subsubdemo)
  ;;     (export sub-sub-var)
  ;;     (import (vicare))
  ;;     (define sub-sub-var 456))
  ;;
  ;;is imported by the library:
  ;;
  ;;   (library (subdemo)
  ;;     (export sub-var sub-sub-var)
  ;;     (import (vicare) (subsubdemo))
  ;;     (define sub-var 456))
  ;;
  ;;which is imported by the library:
  ;;
  ;;   (library (demo)
  ;;     (export var)
  ;;     (import (vicare) (subdemo))
  ;;     (define var
  ;;       (stale-when (< sub-var sub-sub-var)
  ;;         123)))
  ;;
  ;;which is imported by the program:
  ;;
  ;;   (import (vicare) (demo))
  ;;   (debug-print var)
  ;;
  ;;when the test  expression of the STALE-WHEN syntax  is expanded, the
  ;;identifiers SUB-VAR and SUB-SUB-VAR are  captured by bindings in the
  ;;lexical environment with the format:
  ;;
  ;;   (global . (?library . ?gensym))
  ;;
  ;;where  ?LIBRARY  is the  record  of  type LIBRARY  representing  the
  ;;library that  defines the variable  and ?GENSYM is a  symbol holding
  ;;the variable's value in its  "value" slot.  Such LIBRARY records are
  ;;added first to  INV-COLLECTOR and, after finishing  the expansion of
  ;;the  STALE-WHEN test,  they are  moved to  the STALE-WHEN-COLLECTOR.
  ;;See HANDLE-STALE-WHEN for details.
  ;;
  ;;For  the   identifier  SUB-VAR:  ?LIBRARY  represents   the  library
  ;;"(subdemo)"; for the identifier SUB-SUB-VAR: ?LIBRARY represents the
  ;;library "(subsubdemo)".  Notice that while "(subdemo)" is present in
  ;;the  IMPORT specification,  and  so  it is  also  registered in  the
  ;;IMP-COLLECTOR, "(subsubdemo)" is  not and it is only  present in the
  ;;STALE-WHEN-COLLECTOR.
  ;;
  ;;The  collector  function  referenced  by this  parameter  returns  2
  ;;values, which are usually named GUARD-CODE and GUARD-LIB*:
  ;;
  ;;GUARD-CODE  is  a single  core  language  expression representing  a
  ;;composition of  all the STALE-WHEN  test expressions present  in the
  ;;body  of  a library.   If  at  least  one  of the  test  expressions
  ;;evaluates to true: the whole composite expression evaluates to true.
  ;;
  ;;GUARD-LIB* is a  list of LIBRARY records  representing the libraries
  ;;needed to evaluate the composite test expression.
  ;;
  (make-parameter #f))


;;;; top-level environments
;;
;;The  result  of  parsing  a  set  of  import  specs  and  loading  the
;;corresponding libraries with the R6RS function ENVIRONMENT is a record
;;of  type  ENV;   ENV  records  represent  an   *immutable*  top  level
;;environment.
;;
;;Whenever  a REPL  is created  (Vicare can  launch multiple  REPLs), an
;;interaction environment is created to  serve as top level environment.
;;The  interaction  environment  is  initialised with  the  core  Vicare
;;library  "(vicare)";  an  interaction environment  is  *mutable*:  new
;;bindings can be added to it.  For this reason interaction environments
;;are  represented by  records of  type INTERACTION-ENV,  whose internal
;;format allows adding new bindings.
;;
;;Let's  step back:  how does  the  REPL work?   Every time  we type  an
;;expression  and press  "Return":  the expression  is  expanded in  the
;;context of  the current  interaction environment, compiled  to machine
;;code, executed.   Every REPL expression  is like a full  R6RS program,
;;with the  exception that  the interaction environment  "remembers" the
;;bindings we define.
;;
;;Notice that it is possible to use an interaction environment as second
;;argument  to   EVAL,  allowing  for  persistence   of  bindings  among
;;evaluations:
;;
;;   (eval '(begin
;;            (define a 1)
;;            (define b 2))
;;         (interaction-environment))
;;   (eval '(list a b)
;;         (interaction-environment))
;;   => (1 2)
;;

;;An ENV record encapsulates a substitution and a set of libraries.
;;
(define-record env
  (names
		;A vector  of symbols  representing the public  names of
		;bindings from a set of import specifications as defined
		;by  R6RS.   These  names  are from  the  subst  of  the
		;libraries, already processed with the directives in the
		;import sets (prefix, deprefix, only, except, rename).
   labels
		;A vector of gensyms representing the labels of bindings
		;from a set of import specifications as defined by R6RS.
		;These labels are from the subst of the libraries.
   itc
		;A collector  function (see MAKE-COLLECTOR)  holding the
		;LIBRARY records representing  the libraries selected by
		;the source IMPORT specifications.  These libraries have
		;already been installed.
   )
  (lambda (S port sub-printer)
    (display "#<environment>" port)))

(define-record interaction-env
  (rib
		;The top <RIB>  structure for the evaluation  of code in
		;this environment.  It maps bound identifiers to labels.
   lexenv
		;The LEXENV for both run  time and expand time.  It maps
		;labels to syntactic binding descriptors.
   lab.loc/lex*
		;An alist having  label gensyms as keys  and loc gensyms
		;as  values;  the  loc  gensyms are  also  used  as  lex
		;gensyms.  It  maps binding  labels to  storage location
		;gensyms.
   )
  (lambda (S port sub-printer)
    (display "#<interaction-environment>" port)))

(define (environment? obj)
  (or (env? obj)
      (interaction-env? obj)))

(define* (environment-symbols x)
  ;;Return a list of symbols representing the names of the bindings from
  ;;the given environment.
  ;;
  (cond ((env? x)
	 (vector->list ($env-names x)))
	((interaction-env? x)
	 (map values ($<rib>-name* ($interaction-env-rib x))))
	(else
	 (assertion-violation __who__ "not an environment" x))))

(define* (environment-labels x)
  ;;Return a  list of  symbols representing the  labels of  the bindings
  ;;from the given environment.
  ;;
  (unless (env? x)
    (assertion-violation __who__
      "expected non-interaction environment object as argument" x))
  (vector->list ($env-labels x)))

(define* (environment-libraries x)
  ;;Return  the  list  of  LIBRARY records  representing  the  libraries
  ;;forming the environment.
  ;;
  (unless (env? x)
    (assertion-violation __who__
      "expected non-interaction environment object as argument" x))
  (($env-itc x)))

(define* (environment-binding sym env)
  ;;Search the symbol SYM in the non-interaction environment ENV; if SYM
  ;;is the public  name of a binding  in ENV return 2  values: the label
  ;;associated  to the  binding,  the list  of  values representing  the
  ;;binding.  If SYM is not present in ENV return false and false.
  ;;
  (unless (env? env)
    (assertion-violation __who__
      "expected non-interaction environment object as argument" env))
  (let ((P (vector-exists (lambda (name label)
			    (import (vicare system $symbols))
			    (and (eq? sym name)
				 (cons label ($symbol-value label))))
	     ($env-names  env)
	     ($env-labels env))))
    (if P
	(values (car P) (cdr P))
      (values #f #f))))

;;; --------------------------------------------------------------------

(define (environment . import-spec*)
  ;;This  is  R6RS's  environment.   It  parses  the  import  specs  and
  ;;constructs  an env  record that  can be  used later  by eval  and/or
  ;;expand.
  ;;
  ;;IMPORT-SPEC*  must be  a list  of SYNTAX-MATCH  expression arguments
  ;;representing import  specifications as  defined by R6RS  plus Vicare
  ;;extensions.
  ;;
  (let ((itc (make-collector)))
    (parametrise ((imp-collector itc))
      ;;NAME-VEC is a vector of  symbols representing the external names
      ;;of  the  imported bindings.   LABEL-VEC  is  a vector  of  label
      ;;gensyms uniquely associated to the imported bindings.
      (receive (name-vec label-vec)
	  (begin
	    (import PARSE-IMPORT-SPEC)
	    (parse-import-spec* import-spec*))
	(make-env name-vec label-vec itc)))))

(define (null-environment n)
  ;;Defined  by R6RS.   The null  environment is  constructed using  the
  ;;corresponding library.
  ;;
  (unless (eqv? n 5)
    (assertion-violation 'null-environment
      "only report version 5 is supported" n))
  (environment '(psyntax null-environment-5)))

(define (scheme-report-environment n)
  ;;Defined  by R6RS.   The R5RS  environment is  constructed using  the
  ;;corresponding library.
  ;;
  (unless (eqv? n 5)
    (assertion-violation 'scheme-report-environment
      "only report version 5 is supported" n))
  (environment '(psyntax scheme-report-environment-5)))

(case-define new-interaction-environment
  ;;Build and return a new interaction environment.
  ;;
  (()
   (new-interaction-environment (base-of-interaction-library)))
  ((libname)
   (let* ((lib (find-library-by-name libname))
	  (rib (export-subst->rib (library-export-subst lib))))
     (make-interaction-env rib '() '()))))

(define interaction-environment
  ;;When  called  with  no   arguments:  return  an  environment  object
  ;;representing  the environment  active at  the  REPL; to  be used  as
  ;;argument for EVAL.
  ;;
  ;;When  called with  the argument  ENV, which  must be  an environment
  ;;object: set ENV as interaction environment.
  ;;
  (let ((current-env #f))
    (case-lambda
     (()
      (or current-env
	  (begin
	    (set! current-env (new-interaction-environment))
	    current-env)))
     ((env)
      (unless (environment? env)
	(assertion-violation 'interaction-environment
	  "expected environment object as argument" env))
      (set! current-env env)))))

;;; --------------------------------------------------------------------

;;Set to false  or a record of type INTERACTION-ENV.   This parameter is
;;meant to hold the top-level context  when running the REPL or invoking
;;EVAL with an INTERACTION-ENV as initial lexical environment.
;;
(define top-level-context
  (make-parameter #f))


;;;; public interface: variable transformer
;;
;;As  specified  by  R6RS:  we   can  define  identifier  syntaxes  with
;;IDENTIFIER-SYNTAX  and with  MAKE-VARIABLE-TRANSFORMER; both  of these
;;return  a "special"  value that,  when used  as right-hand  side of  a
;;syntax  definition,  is  recognised  by the  expander  as  a  variable
;;transformer  as opposed  to  a normal  transformer  or a  compile-time
;;value.
;;
;;Let's say we define an identifier syntax with:
;;
;;   (define-syntax ?kwd ?expression)
;;
;;where ?EXPRESSION is:
;;
;;   (identifier-syntax ?stuff)
;;
;;here is what happen:
;;
;;1..The DEFINE-SYNTAX form is expanded and a syntax object is created:
;;
;;      (syntax ?expression)
;;
;;2..The syntax object is  expanded by %EXPAND-MACRO-TRANSFORMER and the
;;   result is a core language sexp representing the transformer.
;;
;;3..The   sexp   is   compiled    and   evaluated   by   the   function
;;   %EVAL-MACRO-TRANSFORMER.   The  result  of   the  evaluation  is  a
;;   "special value" with format:
;;
;;      (identifier-macro! . ?transformer)
;;
;;   where ?TRANSFORMER is a transformer function.
;;
;;4..%EVAL-MACRO-TRANSFORMER  recognises  the  value  as  special  using
;;   VARIABLE-TRANSFORMER?  and   transforms  it  to   a  "local-macro!"
;;   syntactic binding.
;;

(define* (make-variable-transformer x)
  ;;R6RS's  make-variable-transformer.   Build  and return  a  "special"
  ;;value that, when used as right-hand  side of a syntax definition, is
  ;;recognised by the expander as a variable transformer as opposed to a
  ;;normal transformer or a compile-time value.
  ;;
  (if (procedure? x)
      (cons 'identifier-macro! x)
    (assertion-violation __who__ "not a procedure" x)))

(define (variable-transformer? x)
  ;;Return  true if  X  is  recognised by  the  expander  as a  variable
  ;;transformer as  opposed to  a normal  transformer or  a compile-time
  ;;value; otherwise return false.
  ;;
  (and (pair? x)
       (eq? (car x) 'identifier-macro!)
       (procedure? (cdr x))))

(define* (variable-transformer-procedure x)
  ;;If X is recognised by the expander as a variable transformer: return
  ;;the  actual  transformer  function,  otherwise  raise  an  assertion
  ;;violation.
  ;;
  (if (variable-transformer? x)
      (cdr x)
    (assertion-violation __who__ "not a variable transformer" x)))


;;;; public interface: synonym transformer
;;
;;

(define* (make-synonym-transformer {x identifier?})
  ;;Build and  return a  "special" value that,  when used  as right-hand
  ;;side of  a syntax  definition, is  recognised by  the expander  as a
  ;;synonym  transformer as  opposed to  a normal  transformer, variable
  ;;transformer or a compile-time value.
  ;;
  (cons 'synonym-transformer x))

(define (synonym-transformer? x)
  ;;Return  true  if X  is  recognised  by  the  expander as  a  synonym
  ;;transformer as opposed to a normal transformer, variable transformer
  ;;or a compile-time value; otherwise return false.
  ;;
  (and (pair? x)
       (eq? ($car x) 'synonym-transformer)
       (identifier? ($cdr x))))

(define* (synonym-transformer-identifier {x synonym-transformer?})
  ;;If X is recognised by the  expander as a synonym transformer: return
  ;;the source identifier, otherwise raise an exception.
  ;;
  ($cdr x))


;;;; public interface: compile-time values
;;
;;Compile-time values are objects computed  at expand-time and stored in
;;the lexical environment.  We can  define a compile-time value and push
;;it on the lexical environment with:
;;
;;   (define-syntax it
;;     (make-compile-time-value (+ 1 2)))
;;
;;later  we can  retrieve it  by  defining a  transformer function  that
;;returns a function:
;;
;;   (define-syntax get-it
;;     (lambda (stx)
;;       (lambda (ctv-retriever)
;;         (ctv-retriever #'it) => 3
;;         )))
;;
;;Let's say we define a compile-time value with:
;;
;;   (define-syntax ?kwd ?expression)
;;
;;where ?EXPRESSION is:
;;
;;   (make-compile-time-value ?stuff)
;;
;;here is what happen:
;;
;;1..The DEFINE-SYNTAX form is expanded and a syntax object is created:
;;
;;      (syntax ?expression)
;;
;;2..The syntax object is  expanded by %EXPAND-MACRO-TRANSFORMER and the
;;   result is a core language sexp representing the right-hand side.
;;
;;3..The   sexp   is   compiled    and   evaluated   by   the   function
;;   %EVAL-MACRO-TRANSFORMER.   The  result  of   the  evaluation  is  a
;;   "special value" with format:
;;
;;      (ctv! . ?obj)
;;
;;   where ?OBJ is the actual compile-time value.
;;
;;4..%EVAL-MACRO-TRANSFORMER  recognises  the  value  as  special  using
;;   COMPILE-TIME-VALUE?  and  transforms it to a  "local-ctv" syntactic
;;   binding.
;;

(define (make-compile-time-value obj)
  (cons 'ctv obj))

(define (compile-time-value? obj)
  (and (pair? obj)
       (eq? 'ctv (car obj))))

(define compile-time-value-object
  ;;Given  a compile-time  value datum:  return the  actual compile-time
  ;;object.
  ;;
  cdr)


(define* (eval x env)
  ;;This  is R6RS's  eval.   Take an  expression  and an  environment:
  ;;expand the  expression, invoke  its invoke-required  libraries and
  ;;evaluate  its  expanded  core  form.  Return  the  result  of  the
  ;;expansion.
  ;;
  (unless (environment? env)
    (error __who__ "not an environment" env))
  (receive (x invoke-req*)
      (expand-form-to-core-language x env)
    (for-each invoke-library invoke-req*)
    (eval-core (expanded->core x))))


(module (expand-form-to-core-language)
  (define-constant __who__ 'expand-form-to-core-language)

  (define (expand-form-to-core-language expr env)
    ;;Interface to the internal expression expander (chi-expr).  Take an
    ;;expression and  an environment.  Return two  values: the resulting
    ;;core-expression, a list  of libraries that must  be invoked before
    ;;evaluating the core expr.
    ;;
    (cond ((env? env)
	   (let ((rib (make-top-rib (env-names env) (env-labels env))))
	     (let ((expr.stx (make-<stx> expr TOP-MARK* (list rib) '()))
		   (rtc      (make-collector))
		   (vtc      (make-collector))
		   (itc      (env-itc env)))
	       (let ((expr.core (parametrise ((top-level-context #f)
					      (inv-collector rtc)
					      (vis-collector vtc)
					      (imp-collector itc))
				  (let ((lexenv.run	'())
					(lexenv.expand	'()))
				    (chi-expr expr.stx lexenv.run lexenv.expand)))))
		 (seal-rib! rib)
		 (values expr.core (rtc))))))

	  ((interaction-env? env)
	   (let ((rib         (interaction-env-rib env))
		 (lexenv.run  (interaction-env-lexenv env))
		 (rtc         (make-collector)))
	     (let ((expr.stx (make-<stx> expr TOP-MARK* (list rib) '())))
	       (receive (expr.core lexenv.run^)
		   (parametrise ((top-level-context env)
				 (inv-collector rtc)
				 (vis-collector (make-collector))
				 (imp-collector (make-collector)))
		     (%chi-interaction-expr expr.stx rib lexenv.run))
		 (set-interaction-env-lexenv! env lexenv.run^)
		 (values expr.core (rtc))))))

	  (else
	   (assertion-violation __who__ "not an environment" env))))

  (define (%chi-interaction-expr expr.stx rib lexenv.run)
    (receive (e* lexenv.run^ lexenv.expand^ lex* rhs* mod** _kwd* _exp*)
	(chi-body* (list expr.stx) lexenv.run lexenv.run
		   '() '() '() '() '() rib #t #f)
      (let ((expr.core* (%expand-interaction-rhs*/init*
			 (reverse lex*) (reverse rhs*)
			 (append (reverse-and-append mod**)
				 e*)
			 lexenv.run^ lexenv.expand^)))
	(let ((expr.core (cond ((null? expr.core*)
				(build-void))
			       ((null? (cdr expr.core*))
				(car expr.core*))
			       (else
				(build-sequence no-source expr.core*)))))
	  (values expr.core lexenv.run^)))))

  (define (%expand-interaction-rhs*/init* lhs* rhs* init* lexenv.run lexenv.expand)
    ;;Return a list of expressions in the core language.
    ;;
    (let recur ((lhs* lhs*)
		(rhs* rhs*))
      (if (null? lhs*)
	  (map (lambda (init)
		 (chi-expr init lexenv.run lexenv.expand))
	    init*)
	(let ((lhs (car lhs*))
	      (rhs (car rhs*)))
	  (define-inline (%recurse-and-cons ?core-expr)
	    (cons ?core-expr
		  (recur (cdr lhs*) (cdr rhs*))))
	  (case (car rhs)
	    ((defun)
	     (let ((rhs (chi-defun (cdr rhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons (build-global-assignment no-source lhs rhs))))
	    ((expr)
	     (let ((rhs (chi-expr (cdr rhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons (build-global-assignment no-source lhs rhs))))
	    ((top-expr)
	     (let ((core-expr (chi-expr (cdr rhs) lexenv.run lexenv.expand)))
	       (%recurse-and-cons core-expr)))
	    (else
	     (error __who__ "invalid" rhs)))))))

  #| end of module: EXPAND-FORM-TO-CORE-LANGUAGE |# )


(module (expand-r6rs-top-level-make-evaluator)

  (define (expand-r6rs-top-level-make-evaluator program-form*)
    ;;Given a list of  SYNTAX-MATCH expression arguments representing an
    ;;R6RS top level  program, expand it and return a  thunk which, when
    ;;evaluated,  compiles the  program and  returns an  INTERACTION-ENV
    ;;struct representing the environment after the program execution.
    ;;
    (receive (lib* invoke-code macro* export-subst export-env)
	(expand-top-level program-form*)
      (lambda ()
	;;Make  sure  that the  code  of  all  the needed  libraries  is
	;;compiled   and  evaluated.    The  storage   location  gensyms
	;;associated to  the exported bindings are  initialised with the
	;;global values.
	(for-each invoke-library lib*)
	;;Store  the  expanded  code  representing  the  macros  in  the
	;;associated location gensyms.
	(initial-visit! macro*)
	;;Convert  the expanded  language  code to  core language  code,
	;;compile it and evaluate it.
	(eval-core (expanded->core invoke-code))
	(make-interaction-env (export-subst->rib export-subst)
			      (map %export-env-entry->lexenv-entry export-env)
			      '()))))

  (define (%export-env-entry->lexenv-entry export-env-entry)
    (let* ((label           (car export-env-entry))
	   (export-binding  (cdr export-env-entry))
	   (type-sym        (export-binding-type export-binding))
	   (loc             (export-binding-loc  export-binding)))
      ;;Here we know  that TYPE-SYM is one  among: GLOBAL, GLOBAL-MACRO,
      ;;GLOBAL-MACRO!, GLOBAL-CTV.
      (cons* label type-sym '*interaction* loc)))

  #| end of module: EXPAND-R6RS-TOP-LEVEL-MAKE-EVALUATOR |# )


;;;; R6RS program expander

(module (expand-top-level)

  (define (expand-top-level program-form*)
    ;;Given a list of  SYNTAX-MATCH expression arguments representing an
    ;;R6RS top level program, expand it.
    ;;
    (receive (import-spec* body*)
	(%parse-top-level-program program-form*)
      (receive (import-spec* invoke-lib* visit-lib* invoke-code macro* export-subst export-env)
	  (let ()
	    (import CORE-BODY-EXPANDER)
	    (core-body-expander 'all import-spec* body* #t))
	(values invoke-lib* invoke-code macro* export-subst export-env))))

  (define (%parse-top-level-program program-form*)
    ;;Given a list of  SYNTAX-MATCH expression arguments representing an
    ;;R6RS top level program, parse it and return 2 values:
    ;;
    ;;1. A list of import specifications.
    ;;
    ;;2. A list of body forms.
    ;;
    (syntax-match program-form* ()
      (((?import ?import-spec* ...) body* ...)
       (eq? (syntax->datum ?import) 'import)
       (values ?import-spec* body*))

      (((?import . x) . y)
       (eq? (syntax->datum ?import) 'import)
       (syntax-violation 'expander
	 "invalid syntax of top-level program" (syntax-car program-form*)))

      (_
       (assertion-violation 'expander
	 "top-level program is missing an (import ---) clause"))))

  #| end of module: EXPAND-TOP-LEVEL |# )

(define (expand-top-level->sexp sexp)
  (receive (invoke-lib* invoke-code macro* export-subst export-env)
      (expand-top-level sexp)
    `((invoke-lib* . ,invoke-lib*)
      (invoke-code . ,invoke-code)
      (macro* . ,macro*)
      (export-subst . ,export-subst)
      (export-env . ,export-env))))


;;;; R6RS library expander

(define (boot-library-expand library-sexp)
  ;;This function  is used  to expand the  libraries composing  the boot
  ;;image; see "makefile.sps" for details on how it is used.
  ;;
  ;;When bootstrapping  the system,  visit-code is  not (and  cannot be)
  ;;used in the "next" system.  So, we drop it.
  ;;
  ;;The returned values are:
  ;;
  ;;LIBNAME -
  ;;   A R6RS library name.
  ;;
  ;;INVOKE-CODE -
  ;;    A list  of symbolic  expressions  representing the  body of  the
  ;;   library.
  ;;
  ;;EXPORT-SUBST -
  ;;   A  subst selecting the bindings  to be exported from  the ones in
  ;;   EXPORT-ENV.
  ;;
  ;;EXPORT-ENV -
  ;;   Represents the global bindings defined by the library body.
  ;;
  (receive (uid libname
		imp-libdesc* vis-libdesc* inv-libdesc*
		invoke-code visit-code
		export-subst export-env
		guard-code guard-libdesc*
		option*)
      (expand-library library-sexp)
    (values libname invoke-code export-subst export-env)))

(module (expand-library)
  ;;EXPAND-LIBRARY  is  the  default  library  expander;  it  expands  a
  ;;symbolic  expression representing  a LIBRARY  form to  core-form; it
  ;;registers it  with the library  manager, in other words  it installs
  ;;it.
  ;;
  ;;The argument LIBRARY-SEXP must be the symbolic expression:
  ;;
  ;;   (library . _)
  ;;
  ;;or an ANNOTATION struct representing such expression.
  ;;
  ;;The optional FILENAME must be #f or a string representing the source
  ;;file from which  the library was loaded; it is  used for information
  ;;purposes.
  ;;
  ;;The optional argument VERIFY-LIBNAME must be a procedure accepting a
  ;;R6RS  library  name  as  argument;  it  is  meant  to  perform  some
  ;;validation upon the library name components (especially the version)
  ;;and raise  an exception if  something is wrong; otherwise  it should
  ;;just return.
  ;;
  ;;The returned values are:
  ;;
  ;;UID -
  ;;  A gensym uniquely identifying this library.
  ;;
  ;;LIBNAME -
  ;;  A R6RS library name.  For the library:
  ;;
  ;;     (library (ciao (1 2))
  ;;       (export A)
  ;;       (import (rnrs))
  ;;       (define A 123))
  ;;
  ;;  LIBNAME is the list (ciao (1 2)).
  ;;
  ;;IMPORT-LIBDESC* -
  ;;  A list of library descriptors representing the libraries that need
  ;;  to be  imported for the invoke  code.  Each item in the  list is a
  ;;  "library descriptor" as built by the LIBRARY-DESCRIPTOR function.
  ;;
  ;;VISIT-LIBDESC* -
  ;;  A list of library descriptors representing the libraries that need
  ;;  to  be imported for the  visit code.  Each  item in the list  is a
  ;;  "library descriptor" as built by the LIBRARY-DESCRIPTOR function.
  ;;
  ;;INVOKE-LIBDESC* -
  ;;  A list of library descriptors representing the libraries that need
  ;;   to be  invoked  to  make available  the  values  of the  imported
  ;;  variables.   Each item in  the list  is a "library  descriptor" as
  ;;  built by the LIBRARY-DESCRIPTOR function.
  ;;
  ;;INVOKE-CODE -
  ;;  A  symbolic expression  representing the code  to be  evaluated to
  ;;  create  the top-level  DEFINE bindings  and evaluate  the trailing
  ;;  init expressions.
  ;;
  ;;VISIT-CODE -
  ;;  A  symbolic expression  representing the code  to be  evaluated to
  ;;  create the expand-time code.
  ;;
  ;;EXPORT-SUBST -
  ;;  A subst representing the bindings to export.
  ;;
  ;;EXPORT-ENV -
  ;;  A list representing the bindings exported by the library.
  ;;
  ;;GUARD-CODE -
  ;;   A predicate  expression  in the  core  language representing  the
  ;;  stale-when tests from the body of the library.
  ;;
  ;;GUARD-LIBDESC* -
  ;;  A list of library descriptors representing the libraries that need
  ;;  to  be invoked for  the STALE-WHEN  code; these are  the libraries
  ;;  accumulated  by the  INV-COLLECTOR while expanding  the STALE-WHEN
  ;;  test expressions.  Each item in the list is a "library descriptor"
  ;;  as built by the LIBRARY-DESCRIPTOR function.
  ;;
  ;;OPTION* -
  ;;   A list  of  symbolic expressions  representing  options from  the
  ;;  OPTIONS clause of the LIBRARY form.
  ;;
  ;;For example, expanding the library:
  ;;
  ;;   (library (ciao)
  ;;     (export var fun mac ctv)
  ;;     (import (vicare))
  ;;     (define var 1)
  ;;     (define (fun)
  ;;       2)
  ;;     (define-syntax (mac stx)
  ;;       3)
  ;;     (define-syntax ctv
  ;;       (make-compile-time-value
  ;;        (+ 4 5))))
  ;;
  ;;yields the INVOKE-CODE:
  ;;
  ;;   (library-letrec*
  ;;       ((lex.var loc.lex.var '1)
  ;;        (lex.fun loc.lex.fun (annotated-case-lambda fun (() '2))))
  ;;     ((primitive void)))
  ;;
  ;;the VISIT-CODE:
  ;;
  ;;   (begin
  ;;     (set! loc.lab.mac
  ;;           (annotated-case-lambda
  ;;               (#'lambda (#'stx) #'3)
  ;;             ((lex.stx) '3)))
  ;;     (set! loc.lab.ctv
  ;;           (annotated-call
  ;;               (make-compile-time-value (+ 4 5))
  ;;             (primitive make-compile-time-value)
  ;;             (annotated-call (+ 4 5) (primitive +) '4 '5))))
  ;;
  ;;the EXPORT-SUBST:
  ;;
  ;;   ((ctv . lab.ctv)
  ;;    (mac . lab.mac)
  ;;    (fun . lab.fun)
  ;;    (var . lab.var))
  ;;
  ;;the EXPORT-ENV
  ;;
  ;;   ((lab.var global		. loc.lex.var)
  ;;    (lab.fun global		. loc.lex.fun)
  ;;    (lab.mac global-macro	. loc.lab.mac)
  ;;    (lab.ctv global-ctv	. loc.lab.ctv))
  ;;
  ;;Another example, for the library:
  ;;
  ;;   (library (ciao (1 2))
  ;;     (export doit)
  ;;     (import (vicare))
  ;;     (stale-when (< 1 2)
  ;;       (define a 123))
  ;;     (stale-when (< 2 3)
  ;;       (define b 123))
  ;;     (define (doit)
  ;;       123))
  ;;
  ;;the GUARD-CODE is:
  ;;
  ;;   (if (if '#f
  ;;           '#t
  ;;          (annotated-call (< 1 2) (primitive <) '1 '2))
  ;;       '#t
  ;;     (annotated-call (< 2 3) (primitive <) '2 '3))
  ;;
  (case-define expand-library
    ((library-sexp)
     (expand-library library-sexp #f       (lambda (libname) (void))))
    ((library-sexp filename)
     (expand-library library-sexp filename (lambda (libname) (void))))
    ((library-sexp filename verify-libname)
     (receive (libname
	       import-lib* invoke-lib* visit-lib*
	       invoke-code macro*
	       export-subst export-env
	       guard-code guard-lib*
	       option*)
	 (parametrise ((source-code-location (or filename (source-code-location))))
	   (let ()
	     (import CORE-LIBRARY-EXPANDER)
	     (core-library-expander library-sexp verify-libname)))
       (let ((uid		(gensym)) ;library unique-symbol identifier
	     (import-libdesc*	(map library-descriptor import-lib*))
	     (visit-libdesc*	(map library-descriptor visit-lib*))
	     (invoke-libdesc*	(map library-descriptor invoke-lib*))
	     (guard-libdesc*	(map library-descriptor guard-lib*))
	     ;;Thunk to eval to visit the library.
	     (visit-proc	(lambda ()
				  (initial-visit! macro*)))
	     ;;Thunk to eval to invoke the library.
	     (invoke-proc	(lambda ()
				  (eval-core (expanded->core invoke-code))))
	     (visit-code	(%build-visit-code macro*))
	     (visible?		#t))
	 (install-library uid libname
			  import-libdesc* visit-libdesc* invoke-libdesc*
			  export-subst export-env
			  visit-proc invoke-proc
			  visit-code invoke-code
			  guard-code guard-libdesc*
			  visible? filename option*)
	 (values uid libname
		 import-libdesc* visit-libdesc* invoke-libdesc*
		 invoke-code visit-code
		 export-subst export-env
		 guard-code guard-libdesc*
		 option*)))))

  (define (%build-visit-code macro*)
    ;;Return a sexp  representing code that initialises  the bindings of
    ;;macro  definitions in  the  core language:  the  visit code;  code
    ;;evaluated whenever the library is visited; each library is visited
    ;;only once the first time an exported binding is used.  MACRO* is a
    ;;list of sublists, each having the format:
    ;;
    ;;   (?loc . (?obj . ?src-code))
    ;;
    ;;The returned sexp looks like this (one SET! for every macro):
    ;;
    ;;  (begin
    ;;    (set! G3
    ;;      (annotated-case-lambda
    ;;	      (#<syntax expr=lambda mark*=(top)>
    ;;	       (#<syntax expr=stx mark*=(top)>)
    ;;         #<syntax expr=3 mark*=(top)>)
    ;;	      ((stx) '3)))
    ;;    (set! G5
    ;;      (annotated-call
    ;;	      (make-compile-time-value (+ 4 5))
    ;;	      (primitive make-compile-time-value)
    ;;	      (annotated-call (+ 4 5)
    ;;          (primitive +) '4 '5))))
    ;;
    (if (null? macro*)
	(build-void)
      (build-sequence no-source
	(map (lambda (x)
	       (let ((loc (car x))
		     (src (cddr x)))
		 (build-global-assignment no-source loc src)))
	  macro*))))

  #| end of module: EXPAND-LIBRARY |# )

(define (expand-library->sexp libsexp)
  (receive (uid libname
	    import-libdesc* visit-libdesc* invoke-libdesc*
	    invoke-code visit-code
	    export-subst export-env
	    guard-code guard-libdesc*
	    option*)
      (expand-library libsexp)
    `((uid		. ,uid)
      (libname		. ,libname)
      (import-libdesc*	. ,import-libdesc*)
      (visit-libdesc*	. ,visit-libdesc*)
      (invoke-libdesc*	. ,invoke-libdesc*)
      (invoke-code	. ,invoke-code)
      (visit-code	. ,visit-code)
      (export-subst	. ,export-subst)
      (export-env	. ,export-env)
      (guard-code	. ,guard-code)
      (guard-libdesc*	. ,guard-libdesc*)
      (option*		. ,option*))))


(module CORE-LIBRARY-EXPANDER
  (core-library-expander)
  (define-constant __who__ 'core-library-expander)

  (define (core-library-expander library-sexp verify-libname)
    ;;Given a  SYNTAX-MATCH expression  argument representing  a LIBRARY
    ;;form:
    ;;
    ;;   (library . _)
    ;;
    ;;parse  it  and return  multiple  values  representing the  library
    ;;contents.
    ;;
    ;;The optional argument VERIFY-LIBNAME must be a procedure accepting
    ;;a  R6RS library  name as  argument; it  is meant  to perform  some
    ;;validation  upon  the  library  name  components  (especially  the
    ;;version) and raise  an exception if something  is wrong; otherwise
    ;;it should just return.
    ;;
    (receive (libname export-spec* import-spec* body* libopt*)
	(%parse-library library-sexp)
      (%validate-library-name libname verify-libname)
      (let* ((option*    (%parse-library-options libopt*))
	     (stale-clt  (%make-stale-collector)))
	(receive (import-lib* invoke-lib* visit-lib* invoke-code macro* export-subst export-env)
	    (parametrise ((stale-when-collector stale-clt))
	      (let ((mixed-definitions-and-expressions? #f))
		(import CORE-BODY-EXPANDER)
		(core-body-expander export-spec* import-spec* body*
				    mixed-definitions-and-expressions?)))
	  (receive (guard-code guard-lib*)
	      (stale-clt)
	    (values (syntax->datum libname)
		    import-lib* invoke-lib* visit-lib*
		    invoke-code macro* export-subst
		    export-env guard-code guard-lib*
		    option*))))))

  (define (%parse-library library-sexp)
    ;;Given an  ANNOTATION struct  representing a LIBRARY  form symbolic
    ;;expression, return 4 values:
    ;;
    ;;1..The name part.  A SYNTAX-MATCH expression argument representing
    ;;   parts of the library name.
    ;;
    ;;2..The   export  specs.    A   SYNTAX-MATCH  expression   argument
    ;;   representing the exports specification.
    ;;
    ;;3..The   import  specs.    A   SYNTAX-MATCH  expression   argument
    ;;   representing the imports specification.
    ;;
    ;;4..The body  of the  library.  A SYNTAX-MATCH  expression argument
    ;;   representing the body of the library.
    ;;
    ;;This function  performs no validation  of the returned  values, it
    ;;just validates the structure of the LIBRARY form.
    ;;
    (syntax-match library-sexp ()
      ((?library (?name* ...)
		 (?options ?libopt* ...)
		 (?export ?exp* ...)
		 (?import ?imp* ...)
		 ?body* ...)
       (and (eq? (syntax->datum ?library) 'library)
	    (eq? (syntax->datum ?options) 'options)
	    (eq? (syntax->datum ?export)  'export)
	    (eq? (syntax->datum ?import)  'import))
       (values ?name* ?exp* ?imp* ?body* ?libopt*))
      ((?library (?name* ...)
		 (?export ?exp* ...)
		 (?import ?imp* ...)
		 ?body* ...)
       (and (eq? (syntax->datum ?library) 'library)
	    (eq? (syntax->datum ?export)  'export)
	    (eq? (syntax->datum ?import)  'import))
       (values ?name* ?exp* ?imp* ?body* '()))
      (_
       (syntax-violation __who__ "malformed library" library-sexp))))

  (define (%validate-library-name libname verify-libname)
    ;;Given a SYNTAX-MATCH expression argument LIBNAME which is meant to
    ;;represent a R6RS library name: validate it and, if success, return
    ;;it; otherwise raise ane exception.
    ;;
    (receive (name* ver*)
	(let recur ((sexp libname))
	  (syntax-match sexp ()
	    (((?vers* ...))
	     (for-all library-version-number? (map syntax->datum ?vers*))
	     (values '() (map syntax->datum ?vers*)))

	    ((?id . ?rest)
	     (symbol? (syntax->datum ?id))
	     (receive (name* vers*)
		 (recur ?rest)
	       (values (cons (syntax->datum ?id) name*) vers*)))

	    (()
	     (values '() '()))

	    (_
	     (syntax-violation __who__ "invalid library name" libname))))
      (when (null? name*)
	(syntax-violation __who__ "empty library name" libname)))
    (verify-libname (syntax->datum libname)))

  (define (%parse-library-options libopt*)
    (syntax-match libopt* ()
      (() '())
      ((?opt . ?other*)
       (symbol? (syntax->datum ?opt))
       (let ((sym (syntax->datum ?opt)))
	 (if (eq? sym 'visit-upon-loading)
	     (cons sym (%parse-library-options ?other*))
	   (syntax-violation __who__
	     "invalid library option" ?opt))))
      ))

  (module (%make-stale-collector)
    ;;When a library has code like:
    ;;
    ;;   (stale-when (< 1 2) (define a 123))
    ;;   (stale-when (< 2 3) (define b 123))
    ;;
    ;;we build STALE-CODE as follows:
    ;;
    ;;   (if (if '#f
    ;;           '#t
    ;;         (annotated-call (< 1 2) (primitive <) '1 '2))
    ;;       '#t
    ;;     (annotated-call (< 2 3) (primitive <) '2 '3))
    ;;
    ;;The value GUARD-LIB* is the list of LIBRARY records accumulated by
    ;;the INV-COLLECTOR while expanding the STALE-WHEN test expressions.
    ;;
    (define (%make-stale-collector)
      (let ((accumulated-code           (build-data no-source #f))
	    (accumulated-requested-lib* '()))
	(case-lambda
	 (()
	  (values accumulated-code accumulated-requested-lib*))
	 ((new-test-code requested-lib*)
	  (set! accumulated-code
		(build-conditional no-source
		  accumulated-code	    ;test
		  (build-data no-source #t) ;consequent
		  new-test-code))	    ;alternate
	  (set! accumulated-requested-lib*
		(%set-union requested-lib* accumulated-requested-lib*))))))

    (define (%set-union ls1 ls2)
      ;;Build and return a new list holding elements from LS1 and LS2 with
      ;;duplicates removed.
      ;;
      (cond ((null? ls1)
	     ls2)
	    ((memq (car ls1) ls2)
	     (%set-union (cdr ls1) ls2))
	    (else
	     (cons (car ls1)
		   (%set-union (cdr ls1) ls2)))))

    #| end of module: %MAKE-STALE-COLLECTOR |# )

  #| end of module: CORE-LIBRARY-EXPANDER |# )


(module CORE-BODY-EXPANDER
  (core-body-expander)
  ;;Both the R6RS  programs expander and the R6RS  library expander make
  ;;use of this module to expand the body forms.
  ;;
  ;;Let's take this library as example:
  ;;
  ;;   (library (demo)
  ;;     (export var1
  ;;             (rename (var2 the-var2))
  ;;             mac)
  ;;     (import (vicare))
  ;;     (define var1 1)
  ;;     (define var2 2)
  ;;     (define-syntax (mac stx) 3))
  ;;
  ;;When expanding the body of a library: the argument EXPORT-SPEC* is a
  ;;SYNTAX-MATCH  input argument  representing a  set of  library export
  ;;specifications; when  expanding the body of  a program: EXPORT-SPEC*
  ;;is the symbol "all".
  ;;
  ;;IMPORT-SPEC* is a SYNTAX-MATCH input  argument representing a set of
  ;;library import specifications.
  ;;
  ;;BODY-SEXP* is  a SYNTAX-MATCH  input argument representing  the body
  ;;forms.
  ;;
  ;;MIXED-DEFINITIONS-AND-EXPRESSIONS? is true  when expanding a program
  ;;and  false when  expanding  a library;  when  true mixing  top-level
  ;;definitions and expressions is fine.
  ;;
  ;;Return multiple values:
  ;;
  ;;1. A list of LIBRARY records representing the collection accumulated
  ;;    by  the  IMP-COLLECTOR.   The records  represent  the  libraries
  ;;   imported by the IMPORT syntaxes.
  ;;
  ;;2. A list of LIBRARY records representing the collection accumulated
  ;;    by  the  INV-COLLECTOR.   The records  represent  the  libraries
  ;;   exporting the global variable bindings referenced in the run-time
  ;;   code.
  ;;
  ;;3. A list of LIBRARY records representing the collection accumulated
  ;;    by  the  VIS-COLLECTOR.   The records  represent  the  libraries
  ;;    exporting  the  global   variable  bindings  referenced  in  the
  ;;   right-hand sides of syntax definitions.
  ;;
  ;;4.  INVOKE-CODE  is  a   core  language  LIBRARY-LETREC*  expression
  ;;   representing the  result of expanding the input  source.  For the
  ;;   library in the example INVOKE-CODE is:
  ;;
  ;;      (library-letrec*
  ;;          ((lex.var1 loc.lex.var1 '1)
  ;;           (lex.var2 loc.lec.var2 '2))
  ;;        ((primitive void)))
  ;;
  ;;5. MACRO* is  a list of bindings representing the  macros defined in
  ;;   the code.  For the example library MACRO* is:
  ;;
  ;;      ((lab.mac #<procedure> .
  ;;         (annotated-case-lambda (#'lambda (#'stx) #'3)
  ;;           ((#'stx) '3)))
  ;;
  ;;6. EXPORT-SUBST is an alist with entries having the format:
  ;;
  ;;      (?name . ?label)
  ;;
  ;;   where:  ?NAME is a  symbol representing  the external name  of an
  ;;    exported   syntactic  binding;  ?LABEL  is   a  gensym  uniquely
  ;;    identifying such  syntactic  binding.  For  the  library in  the
  ;;   example, EXPORT-SUBST is:
  ;;
  ;;      ((mac      . lab.mac)
  ;;       (the-var2 . lab.var2)
  ;;       (var1     . lab.var1))
  ;;
  ;;7. EXPORT-ENV is the lexical environment of bindings exported by the
  ;;   library.   Its format is different  from the one of  the LEXENV.*
  ;;   values used throughout the expansion process.  For the library in
  ;;   the example, EXPORT-ENV is:
  ;;
  ;;      ((lab.var1 global       . loc.lex.var1)
  ;;       (lab.var2 global       . loc.lex.var2)
  ;;       (lab.mac  global-macro . loc.lab.mac))
  ;;
  (define (core-body-expander export-spec* import-spec* body-sexp* mixed-definitions-and-expressions?)
    (define itc (make-collector))
    (parametrise ((imp-collector      itc)
		  (top-level-context  #f))
      (define rib
	;;NAME-VEC  is a  vector  of symbols  representing the  external
	;;names  of the  imported bindings.   LABEL-VEC is  a vector  of
	;;label gensyms uniquely associated to the imported bindings.
	(receive (name-vec label-vec)
	    (let ()
	      (import PARSE-IMPORT-SPEC)
	      (parse-import-spec* import-spec*))
	  (make-top-rib name-vec label-vec)))
      (define (wrap x)
	(make-<stx> x TOP-MARK* (list rib) '()))
      (let ((body-stx*	(map wrap body-sexp*))
	    (rtc	(make-collector))
	    (vtc	(make-collector)))
	(parametrise ((inv-collector  rtc)
		      (vis-collector  vtc))
	  ;;INIT-FORM-STX* is a list  of syntax objects representing the
	  ;;trailing non-definition  forms from the body  of the library
	  ;;and the body of the internal modules.
	  ;;
	  ;;LEX* is a list of gensyms  to be used in binding definitions
	  ;;when  building core  language symbolic  expressions for  the
	  ;;glocal DEFINE forms  in the library.  There is  a gensym for
	  ;;every item in RHS*.
	  ;;
	  ;;QRHS* is  a list of qualified  right-hand sides representing
	  ;;the right-hand side expressions in the DEFINE forms from the
	  ;;body of the library.
	  ;;
	  ;;INTERNAL-EXPORT* is  a list of identifiers  exported through
	  ;;internal EXPORT syntaxes rather than  the export spec at the
	  ;;beginning of the library.
	  ;;
	  (receive (init-form-stx* lexenv.run lexenv.expand lex* qrhs* internal-export*)
	      (%chi-library-internal body-stx* rib mixed-definitions-and-expressions?)
	    (receive (export-name* export-id*)
		(let ()
		  (import PARSE-EXPORT-SPEC)
		  (parse-export-spec* (if (%expanding-program? export-spec*)
					  (map wrap (top-marked-symbols rib))
					(append (map wrap export-spec*)
						internal-export*))))
	      (seal-rib! rib)
	      ;;INIT-FORM-CORE*  is a  list  of  core language  symbolic
	      ;;expressions representing the trailing init forms.
	      ;;
	      ;;RHS-FORM-CORE*  is  a  list of  core  language  symbolic
	      ;;expressions representing the DEFINE right-hand sides.
	      ;;
	      ;;We want order here!?!
	      (let* ((init-form-core*  (chi-expr* init-form-stx* lexenv.run lexenv.expand))
		     (rhs-form-core*   (chi-qrhs*  qrhs*  lexenv.run lexenv.expand)))
		;;QUESTION Why do we unseal the  rib if we do not use it
		;;anymore?  Is  it an  additional check of  its internal
		;;integrity?  (Marco Maggi; Sun Mar 23, 2014)
		(unseal-rib! rib)
		(let ((loc*          (map gensym-for-storage-location lex*))
		      (export-subst  (%make-export-subst export-name* export-id*)))
		  (receive (export-env macro*)
		      (%make-export-env/macro* lex* loc* lexenv.run)
		    (%validate-exports export-spec* export-subst export-env)
		    (let ((invoke-code (build-library-letrec* no-source
					 mixed-definitions-and-expressions?
					 lex* loc* rhs-form-core*
					 (if (null? init-form-core*)
					     (build-void)
					   (build-sequence no-source init-form-core*)))))
		      (values (itc) (rtc) (vtc)
			      invoke-code macro* export-subst export-env)))))))))))

  (define-syntax-rule (%expanding-program? ?export-spec*)
    (eq? 'all ?export-spec*))

  (define-inline (%chi-library-internal body-stx* rib mixed-definitions-and-expressions?)
    ;;Perform  the expansion  of the  top-level forms  in the  body; the
    ;;right-hand sides  of DEFINE  syntaxes are  not expanded  here; the
    ;;trailing init forms are not expanded here.
    ;;
    (receive (trailing-init-form-stx*
	      lexenv.run lexenv.expand lex*
	      qrhs* module-init-form-stx** unused-kwd* internal-export*)
	(let ((shadowing-definitions? #t))
	  (chi-body* body-stx* '() '() '() '() '() '() '() rib
		     mixed-definitions-and-expressions?
		     shadowing-definitions?))
      ;;We build  a list of  init form  putting first the  trailing init
      ;;forms from the internal MODULE  syntaxes, then the trailing init
      ;;forms from the library body.
      (let ((init-form-stx* (append (reverse-and-append module-init-form-stx**)
				    trailing-init-form-stx*)))
	(values init-form-stx*
		lexenv.run lexenv.expand
		;;This  is a  list  of  gensyms to  be  used in  binding
		;;definitions  when  building   core  language  symbolic
		;;expressions  for  the  DEFINE forms  in  the  library.
		;;There is a gensym for every item in QRHS*.
		(reverse lex*)
		;;This  is   a  list   of  qualified   right-hand  sides
		;;representing  the right-hand  side expressions  in the
		;;DEFINE forms from the body of the library.
		(reverse qrhs*)
		;;This  is  a  list   of  identifiers  exported  through
		;;internal EXPORT  syntaxes rather than the  export spec
		;;at the beginning of the library.
		internal-export*))))

  (define (%make-export-subst export-name* export-id*)
    ;;For every  identifier in  ID: get  the rib of  ID and  extract the
    ;;lexical environment from it; search  the environment for a binding
    ;;associated  to ID  and acquire  its label  (a gensym).   Return an
    ;;alist with entries having the format:
    ;;
    ;;   (?export-name . ?label)
    ;;
    ;;where ?EXPORT-NAME is  a symbol representing the  external name of
    ;;an exported  binding, ?LABEL is the  corresponding gensym uniquely
    ;;identifying the binding.
    ;;
    (map (lambda (export-name export-id)
	   (let ((label (id->label export-id)))
	     (if label
		 (cons export-name label)
	       (stx-error export-id "cannot export unbound identifier"))))
      export-name* export-id*))

  (module (%make-export-env/macro*)
    ;;For  each entry  in LEXENV.RUN:  convert  the LEXENV  entry to  an
    ;;EXPORT-ENV  entry,  accumulating   EXPORT-ENV;  if  the  syntactic
    ;;binding is  a macro or  compile-time value: accumulate  the MACRO*
    ;;alist.
    ;;
    ;;Notice that EXPORT-ENV contains an  entry for every global lexical
    ;;variable, both the exported ones and the non-exported ones.  It is
    ;;responsibility  of   the  EXPORT-SUBST   to  select   the  entries
    ;;representing the exported bindings.
    ;;
    ;;LEX* must  be a  list of gensyms  representing the  global lexical
    ;;variables bindings.
    ;;
    ;;LOC* must  be a list  of storage  location gensyms for  the global
    ;;lexical variables: there must be a loc for every lex in LEX*.
    ;;
    (define (%make-export-env/macro* lex* loc* lexenv.run)
      (let loop ((lexenv.run		lexenv.run)
		 (export-env		'())
		 (macro*		'()))
	(if (null? lexenv.run)
	    (values export-env macro*)
	  (let* ((entry    (car lexenv.run))
		 (label    (lexenv-entry-label entry))
		 (binding  (lexenv-entry-binding-descriptor entry)))
	    (case (syntactic-binding-type binding)
	      ((lexical)
	       ;;This binding is  a lexical variable.  When  we import a
	       ;;lexical binding from another  library, we must see such
	       ;;entry as "global".
	       ;;
	       ;;The entry from the LEXENV looks like this:
	       ;;
	       ;;   (?label . (lexical . (?lexvar . ?mutable)))
	       ;;
	       ;;Add to the EXPORT-ENV an entry like:
	       ;;
	       ;;   (?label ?type . ?loc)
	       ;;
	       ;;where  ?TYPE  is the  symbol  "mutable"  or the  symbol
	       ;;"global"; notice that the entries of type "mutable" are
	       ;;forbidden to be exported.
	       ;;
	       (let* ((bind-val  (syntactic-binding-value binding))
		      (loc       (%lookup (lexical-var bind-val) lex* loc*))
		      (type      (if (lexical-var-mutated? bind-val)
				     'mutable
				   'global)))
		 (loop (cdr lexenv.run)
		       (cons (cons* label type loc) export-env)
		       macro*)))

	      ((local-macro)
	       ;;When we  define a binding for  a non-identifier syntax:
	       ;;the local code sees it  as "local-macro".  If we export
	       ;;such   binding:  the   importer  must   see  it   as  a
	       ;;"global-macro".
	       ;;
	       ;;The entry from the LEXENV looks like this:
	       ;;
	       ;;   (?label . (local-macro . (?transformer . ?expanded-expr)))
	       ;;
	       ;;Add to the EXPORT-ENV an entry like:
	       ;;
	       ;;   (?label global-macro . ?loc)
	       ;;
	       ;;and to the MACRO* an entry like:
	       ;;
	       ;;   (?loc . (?transformer . ?expanded-expr))
	       ;;
	       (let ((loc (gensym-for-storage-location label)))
		 (loop (cdr lexenv.run)
		       (cons (cons* label 'global-macro loc) export-env)
		       (cons (cons loc (syntactic-binding-value binding)) macro*))))

	      ((local-macro!)
	       ;;When we define a binding  for an identifier syntax: the
	       ;;local  code sees  it as  "local-macro!".  If  we export
	       ;;such   binding:  the   importer  must   see  it   as  a
	       ;;"global-macro!".
	       ;;
	       ;;The entry from the LEXENV looks like this:
	       ;;
	       ;;   (?label . (local-macro! . (?transformer . ?expanded-expr)))
	       ;;
	       ;;Add to the EXPORT-ENV an entry like:
	       ;;
	       ;;   (?label global-macro . ?loc)
	       ;;
	       ;;and to the MACRO* an entry like:
	       ;;
	       ;;   (?loc . (?transformer . ?expanded-expr))
	       ;;
	       (let ((loc (gensym-for-storage-location label)))
		 (loop (cdr lexenv.run)
		       (cons (cons* label 'global-macro! loc) export-env)
		       (cons (cons loc (syntactic-binding-value binding)) macro*))))

	      ((local-ctv)
	       ;;When  we  define a  binding  for  a compile-time  value
	       ;;(CTV): the  local code sees  it as "local-ctv".   If we
	       ;;export  such binding:  the importer  must see  it as  a
	       ;;"global-ctv".
	       ;;
	       ;;The entry from the LEXENV looks like this:
	       ;;
	       ;;   (?label . (local-ctv . (?object . ?expanded-expr)))
	       ;;
	       ;;Add to the EXPORT-ENV an entry like:
	       ;;
	       ;;   (?label global-ctv . ?loc)
	       ;;
	       ;;and to the MACRO* an entry like:
	       ;;
	       ;;   (?loc . (?object . ?expanded-expr))
	       ;;
	       (let ((loc (gensym-for-storage-location label)))
		 (loop (cdr lexenv.run)
		       (cons (cons* label 'global-ctv loc) export-env)
		       (cons (cons loc (syntactic-binding-value binding)) macro*))))

	      (($rtd $module $fluid $synonym)
	       ;;Just add the entry "as is" from the lexical environment
	       ;;to the EXPORT-ENV.
	       ;;
	       (loop (cdr lexenv.run)
		     (cons entry export-env)
		     macro*))

	      (else
	       (assertion-violation 'core-body-expander
		 "BUG: do not know how to export"
		 (syntactic-binding-type  binding)
		 (syntactic-binding-value binding))))))))

    (define (%lookup lexical-gensym lex* loc*)
      ;;Search for  LEXICAL-GENSYM in the  list LEX*: when  found return
      ;;the corresponding  gensym from LOC*.  LEXICAL-GENSYM  must be an
      ;;item in LEX*.
      ;;
      (if (pair? lex*)
	  (if (eq? lexical-gensym (car lex*))
	      (car loc*)
	    (%lookup lexical-gensym (cdr lex*) (cdr loc*)))
	(assertion-violation 'lookup-make-export "BUG")))

    #| end of module: %MAKE-EXPORT-ENV/MACRO* |# )

  (define (%validate-exports export-spec* export-subst export-env)
    ;;We want to forbid code like the following:
    ;;
    ;;    (library (proof)
    ;;      (export that doit)
    ;;      (import (vicare))
    ;;      (define that 123)
    ;;      (define (doit a)
    ;;	      (set! that a)))
    ;;
    ;;in which the mutable variable THAT is exported.
    ;;
    (define export-subst-entry-name  car)
    (define export-subst-entry-label cdr)
    (unless (%expanding-program? export-spec*)
      (for-each (lambda (subst)
		  (cond ((assq (export-subst-entry-label subst) export-env)
			 => (lambda (entry)
			      (when (eq? 'mutable (syntactic-binding-type
						   (lexenv-entry-binding-descriptor entry)))
				(syntax-violation 'export
				  "attempt to export mutated variable"
				  (export-subst-entry-name subst)))))))
	export-subst)))

  #| end of module: CORE-BODY-EXPANDER |# )


(module PARSE-EXPORT-SPEC
  (parse-export-spec*)
  ;;Given a  list of SYNTAX-MATCH expression  arguments representing the
  ;;exports specification from a LIBRARY form, return 2 values:
  ;;
  ;;1. A list of symbols representing the external names of the exported
  ;;   bindings.
  ;;
  ;;2.  A  list of  identifiers  (syntax  objects  holding a  symbol  as
  ;;    expression)  representing the  internal  names  of the  exported
  ;;   bindings.
  ;;
  ;;This function checks that none  of the identifiers is BOUND-ID=?  to
  ;;another: the library does not export the same external *name* twice.
  ;;It is instead possible to  export the same identifier multiple times
  ;;if we give it different external names.
  ;;
  ;;According to R6RS, an export specification has the following syntax:
  ;;
  ;;   (export ?export-spec ...)
  ;;
  ;;   ?export-spec
  ;;     == ?identifier
  ;;     == (rename (?internal-identifier ?external-identifier) ...)
  ;;
  ;;Vicare adds the following:
  ;;
  ;;     == (prefix   (?internal-identifier ...) the-prefix)
  ;;     == (deprefix (?internal-identifier ...) the-prefix)
  ;;     == (suffix   (?internal-identifier ...) the-suffix)
  ;;     == (desuffix (?internal-identifier ...) the-suffix)
  ;;
  (define-constant __who__ 'export)

  (define (parse-export-spec* export-spec*)
    (case-define %synner
      ((message)
       (syntax-violation __who__ message export-spec*))
      ((message subform)
       (syntax-violation __who__ message export-spec* subform)))
    (let loop ((export-spec*          export-spec*)
	       (internal-identifier*  '())
	       (external-identifier*  '()))
      (if (null? export-spec*)
	  (if (valid-bound-ids? external-identifier*)
	      (values (map syntax->datum external-identifier*)
		      internal-identifier*)
	    (%synner "invalid exports" (%find-dups external-identifier*)))
	(syntax-match (car export-spec*) ()
	  (?identifier
	   (identifier? ?identifier)
	   (loop (cdr export-spec*)
		 (cons ?identifier internal-identifier*)
		 (cons ?identifier external-identifier*)))

	  ((?rename (?internal* ?external*) ...)
	   (and (eq? (syntax->datum ?rename) 'rename)
		(for-all identifier? ?internal*)
		(for-all identifier? ?external*))
	   (loop (cdr export-spec*)
		 (append ?internal* internal-identifier*)
		 (append ?external* external-identifier*)))

	  ((?prefix (?internal* ...) ?the-prefix)
	   (and (eq? (syntax->datum ?prefix) 'prefix)
		(for-all identifier? ?internal*)
		(identifier? ?the-prefix))
	   (if (option.strict-r6rs)
	       (%synner "prefix export specification forbidden in strict R6RS mode")
	     (let* ((prefix.str (symbol->string (syntax->datum ?the-prefix)))
		    (external*  (map (lambda (id)
				       (datum->syntax
					id (string->symbol
					    (string-append
					     prefix.str
					     (symbol->string (syntax->datum id))))))
				  ?internal*)))
	       (loop (cdr export-spec*)
		     (append ?internal* internal-identifier*)
		     (append  external* external-identifier*)))))

	  ((?deprefix (?internal* ...) ?the-prefix)
	   (and (eq? (syntax->datum ?deprefix) 'deprefix)
		(for-all identifier? ?internal*)
		(identifier? ?the-prefix))
	   (if (option.strict-r6rs)
	       (%synner "deprefix export specification forbidden in strict R6RS mode")
	     (let* ((prefix.str (symbol->string (syntax->datum ?the-prefix)))
		    (prefix.len (string-length prefix.str))
		    (external*  (map (lambda (id)
				       (let* ((id.str  (symbol->string (syntax->datum id)))
					      (id.len  (string-length id.str)))
					 (if (and (< prefix.len id.len)
						  (string=? prefix.str
							    (substring id.str 0 prefix.len)))
					     (datum->syntax
					      id (string->symbol
						  (substring id.str prefix.len id.len)))
					   (%synner
					    (string-append "binding name \"" id.str
							   "\" cannot be deprefixed of \""
							   prefix.str "\"")))))
				  ?internal*)))
	       (loop (cdr export-spec*)
		     (append ?internal* internal-identifier*)
		     (append  external* external-identifier*)))))

	  ((?suffix (?internal* ...) ?the-suffix)
	   (and (eq? (syntax->datum ?suffix) 'suffix)
		(for-all identifier? ?internal*)
		(identifier? ?the-suffix))
	   (if (option.strict-r6rs)
	       (%synner "suffix export specification forbidden in strict R6RS mode")
	     (let* ((suffix.str (symbol->string (syntax->datum ?the-suffix)))
		    (external*  (map (lambda (id)
				       (datum->syntax
					id (string->symbol
					    (string-append
					     (symbol->string (syntax->datum id))
					     suffix.str))))
				  ?internal*)))
	       (loop (cdr export-spec*)
		     (append ?internal* internal-identifier*)
		     (append  external* external-identifier*)))))

	  ((?desuffix (?internal* ...) ?the-suffix)
	   (and (eq? (syntax->datum ?desuffix) 'desuffix)
		(for-all identifier? ?internal*)
		(identifier? ?the-suffix))
	   (if (option.strict-r6rs)
	       (%synner "desuffix export specification forbidden in strict R6RS mode")
	     (let* ((suffix.str (symbol->string (syntax->datum ?the-suffix)))
		    (suffix.len (string-length suffix.str))
		    (external*  (map (lambda (id)
				       (define id.str
					 (symbol->string (syntax->datum id)))
				       (define id.len
					 (string-length id.str))
				       (define prefix.len
					 (fx- id.len suffix.len))
				       (if (and (< suffix.len id.len)
						(string=? suffix.str
							  (substring id.str prefix.len id.len)))
					   (datum->syntax
					    id (string->symbol
						(substring id.str 0 prefix.len)))
					 (%synner
					  (string-append "binding name \"" id.str
							 "\" cannot be desuffixed of \""
							 suffix.str "\""))))
				  ?internal*)))
	       (loop (cdr export-spec*)
		     (append ?internal* internal-identifier*)
		     (append  external* external-identifier*)))))

	  (_
	   (%synner "invalid export specification" (car export-spec*)))))))

  (module (%find-dups)

    (define-inline (%find-dups ls)
      (let loop ((ls    ls)
		 (dups  '()))
	(cond ((null? ls)
	       dups)
	      ((%find-bound=? (car ls) (cdr ls) (cdr ls))
	       => (lambda (x)
		    (loop (cdr ls)
			  (cons (list (car ls) x)
				dups))))
	      (else
	       (loop (cdr ls) dups)))))

    (define (%find-bound=? x lhs* rhs*)
      (cond ((null? lhs*)
	     #f)
	    ((bound-id=? x (car lhs*))
	     (car rhs*))
	    (else
	     (%find-bound=? x (cdr lhs*) (cdr rhs*)))))

    #| end of module: %FIND-DUPS |# )

  #| end of module: PARSE-EXPORT-SPEC* |# )


(module PARSE-IMPORT-SPEC
  (parse-import-spec*)
  ;;Given  a  list  of SYNTAX-MATCH  expression  arguments  representing
  ;;import specifications from  a LIBRARY form, as defined  by R6RS plus
  ;;Vicare extensions (which can simply be  the raw sexp argument to the
  ;;ENVIRONMENT function):
  ;;
  ;;1. Parse and validate the import specs.
  ;;
  ;;2. For libraries not yet loaded: load the selected library files and
  ;;    add  them  to  the  current  collector  function  referenced  by
  ;;   IMP-COLLECTOR.
  ;;
  ;;3.   Apply to  library-exported  binding  names the  transformations
  ;;   described by the import spec, obtaining the external names.
  ;;
  ;;4. Check for name conflicts between imported bindings.
  ;;
  ;;Return 2  values which can  be used to build  a new top  level <RIB>
  ;;record:
  ;;
  ;;1. NAME-VEC, a vector of  symbols representing the external names of
  ;;   the  imported bindings.
  ;;
  ;;2. LABEL-VEC is a vector of label gensyms uniquely associated to the
  ;;   imported bindings.
  ;;
  ;;
  ;;A  quick  summary  of  R6RS syntax  definitions  along  with  Vicare
  ;;extensions:
  ;;
  ;;  (import ?import-spec ...)
  ;;
  ;;  ?import-spec
  ;;     == ?import-set
  ;;     == (for ?import-set ?import-level)
  ;;
  ;;  ?import-set
  ;;     == ?library-reference
  ;;     == (library ?library-reference)
  ;;     == (only ?import-set ?identifier ...)
  ;;     == (except ?import-set ?identifier)
  ;;     == (rename ?import-set (?identifier1 ?identifier2) ...)
  ;;
  ;;  ?library-reference
  ;;     == (?identifier0 ?identifier ...)
  ;;     == (?identifier0 ?identifier ... ?version-reference)
  ;;
  ;;  ?version-reference
  ;;     == (?sub-version-reference ...)
  ;;     == (and ?version-reference ...)
  ;;     == (or  ?version-reference ...)
  ;;     == (not ?version-reference)
  ;;
  ;;  ?sub-version-reference
  ;;     == ?sub-version
  ;;     == (>=  ?sub-version)
  ;;     == (<=  ?sub-version)
  ;;     == (and ?sub-version-reference ...)
  ;;     == (or  ?sub-version-reference ...)
  ;;     == (not ?sub-version-reference)
  ;;
  ;;  ?sub-version
  ;;     == #<non-negative fixnum>
  ;;
  ;;Vicare extends ?IMPORT-SET with:
  ;;
  ;;     == (prefix ?import-set ?identifier)
  ;;     == (deprefix ?import-set ?identifier)
  ;;     == (suffix ?import-set ?identifier)
  ;;     == (desuffix ?import-set ?identifier)
  ;;
  ;;Example, given:
  ;;
  ;;  ((rename (only (foo)
  ;;                 x z)
  ;;           (x y))
  ;;   (only (bar)
  ;;         q))
  ;;
  ;;this function returns the names and labels:
  ;;
  ;;   #(z y q)		#(lab.z lab.x lab.q)
  ;;
  ;;Externally   visible   imported   bindings  are   selected   by   an
  ;;EXPORT-SUBST: an alist  whose keys are the external  symbol names of
  ;;the bindings and whose values are the associated label gensyms.
  ;;
  (define-constant __who__ 'import)

  (module (parse-import-spec*)

    (define (parse-import-spec* import-spec*)
      (let loop ((import-spec*  import-spec*)
		 (export-table  (make-eq-hashtable)))
	;;EXPORT-TABLE has  EXPORT-SUBST names as keys  and EXPORT-SUBST
	;;labels as  values.  It  is used to  check for  duplicate names
	;;with different labels, which is an error.  Example:
	;;
	;;   (import (rename (french)
	;;                   (salut	ciao))	;ERROR!
	;;           (rename (british)
	;;                   (hello	ciao)))	;ERROR!
	;;
	(if (pair? import-spec*)
	    (begin
	      (for-each (lambda (name.label)
			  (%add-subst-entry! export-table name.label))
		(%import-spec->export-subst ($car import-spec*)))
	      (loop ($cdr import-spec*) export-table))
	  (hashtable-entries export-table))))

    (define-inline (%add-subst-entry! export-table name.label)
      ;;Add  the   given  NAME.LABEL   entry  to   EXPORT-TABLE;  return
      ;;unspecified values.  Raise a  syntax violation if NAME.LABEL has
      ;;the same name of an entry in EXPORT-TABLE, but different label.
      ;;
      (let ((name  ($car name.label))
	    (label ($cdr name.label)))
	(cond ((hashtable-ref export-table name #f)
	       => (lambda (already-existent-label)
		    (unless (eq? already-existent-label label)
		      (%error-two-import-with-different-bindings name))))
	      (else
	       (hashtable-set! export-table name label)))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (%import-spec->export-subst)

    (define-inline (%import-spec->export-subst import-spec)
      ;;Process the IMPORT-SPEC and return the corresponding subst.
      ;;
      ;;The IMPORT-SPEC is  parsed; the specified library  is loaded and
      ;;installed, if  not already  in the  library collection;  the raw
      ;;subst from the library definition  is processed according to the
      ;;rules in IMPORT-SPEC.
      ;;
      ;;If an  error is found, including  library version non-conforming
      ;;to the library reference, an exception is raised.
      ;;
      (syntax-match import-spec ()
	((?for ?import-set . ?import-levels)
	 ;;FIXME Here  we should validate  ?IMPORT-LEVELS even if  it is
	 ;;not used by Vicare.  (Marco Maggi; Tue Apr 23, 2013)
	 (eq? (syntax->datum ?for) 'for)
	 (%import-set->export-subst ?import-set import-spec))

	(?import-set
	 (%import-set->export-subst ?import-set import-spec))))

    (define (%import-set->export-subst import-set import-spec)
      ;;Recursive  function.   Process  the IMPORT-SET  and  return  the
      ;;corresponding  EXPORT-SUBST.   IMPORT-SPEC  is the  full  import
      ;;specification from the IMPORT clause: it is used for descriptive
      ;;error reporting.
      ;;
      (define (%recurse import-set)
	(%import-set->export-subst import-set import-spec))
      (define (%local-synner message)
	(%synner message import-spec import-set))
      (syntax-match import-set ()
	((?spec ?spec* ...)
	 ;;According to R6RS, the symbol LIBRARY  can be used to quote a
	 ;;library reference whose first  identifier is "for", "rename",
	 ;;etc.
	 (not (memq (syntax->datum ?spec)
		    '(rename except only prefix deprefix suffix desuffix library)))
	 (%import-library (cons ?spec ?spec*)))

	((?rename ?import-set (?old-name* ?new-name*) ...)
	 (and (eq? (syntax->datum ?rename) 'rename)
	      (for-all symbol-syntax? ?old-name*)
	      (for-all symbol-syntax? ?new-name*))
	 (let ((subst       (%recurse ?import-set))
	       (?old-name*  (map syntax->datum ?old-name*))
	       (?new-name*  (map syntax->datum ?new-name*)))
	   ;;FIXME Rewrite this  to eliminate find* and  rem* and merge.
	   ;;(Abdulaziz Ghuloum)
	   (let ((old-label* (find* ?old-name* subst ?import-set)))
	     (let ((subst (rem* ?old-name* subst)))
	       ;;FIXME Make sure map is valid. (Abdulaziz Ghuloum)
	       (%merge-export-subst* (map cons ?new-name* old-label*) subst)))))

	((?except ?import-set ?sym* ...)
	 (and (eq? (syntax->datum ?except) 'except)
	      (for-all symbol-syntax? ?sym*))
	 (let ((subst (%recurse ?import-set)))
	   (rem* (map syntax->datum ?sym*) subst)))

	((?only ?import-set ?name* ...)
	 (and (eq? (syntax->datum ?only) 'only)
	      (for-all symbol-syntax? ?name*))
	 (let* ((subst  (%recurse ?import-set))
		(name*  (map syntax->datum ?name*))
		(name*  (remove-dups name*))
		(lab*   (find* name* subst ?import-set)))
	   (map cons name* lab*)))

	((?prefix ?import-set ?the-prefix)
	 (and (eq? (syntax->datum ?prefix) 'prefix)
	      (symbol-syntax? ?prefix))
	 (let ((subst   (%recurse ?import-set))
	       (prefix  (symbol->string (syntax->datum ?the-prefix))))
	   (map (lambda (x)
		  (cons (string->symbol
			 (string-append prefix (symbol->string (car x))))
			(cdr x)))
	     subst)))

	((?deprefix ?import-set ?the-prefix)
	 (and (eq? (syntax->datum ?deprefix) 'deprefix)
	      (symbol-syntax? ?the-prefix))
	 (if (option.strict-r6rs)
	     (%local-synner "deprefix import specification forbidden in strict R6RS mode")
	   (let* ((subst       (%recurse ?import-set))
		  (prefix.str  (symbol->string (syntax->datum ?the-prefix)))
		  (prefix.len  (string-length prefix.str)))
	     ;;This should never happen.
	     (when (zero? prefix.len)
	       (%local-synner "null deprefix prefix"))
	     (map (lambda (subst.entry)
		    (let* ((orig.str  (symbol->string (car subst.entry)))
			   (orig.len  (string-length orig.str)))
		      (if (and (< prefix.len orig.len)
			       (string=? prefix.str (substring orig.str 0 prefix.len)))
			  (cons (string->symbol (substring orig.str prefix.len orig.len))
				(cdr subst.entry))
			(%local-synner
			 (string-append "binding name \"" orig.str
					"\" cannot be deprefixed of \"" prefix.str "\"")))))
	       subst))))

	((?suffix ?import-set ?the-suffix)
	 (and (eq? (syntax->datum ?suffix) 'suffix)
	      (symbol-syntax? ?suffix))
	 (if (option.strict-r6rs)
	     (%local-synner "suffix import specification forbidden in strict R6RS mode")
	   (let ((subst   (%recurse ?import-set))
		 (suffix  (symbol->string (syntax->datum ?the-suffix))))
	     (map (lambda (x)
		    (cons (string->symbol
			   (string-append (symbol->string (car x)) suffix))
			  (cdr x)))
	       subst))))

	((?desuffix ?import-set ?the-suffix)
	 (and (eq? (syntax->datum ?desuffix) 'desuffix)
	      (symbol-syntax? ?the-suffix))
	 (if (option.strict-r6rs)
	     (%local-synner "desuffix import specification forbidden in strict R6RS mode")
	   (let* ((subst       (%recurse ?import-set))
		  (suffix.str  (symbol->string (syntax->datum ?the-suffix)))
		  (suffix.len  (string-length suffix.str)))
	     ;;This should never happen.
	     (when (zero? suffix.len)
	       (%local-synner "null desuffix suffix"))
	     (map (lambda (subst.entry)
		    (let* ((orig.str    (symbol->string (car subst.entry)))
			   (orig.len    (string-length orig.str))
			   (prefix.len  (fx- orig.len suffix.len)))
		      (if (and (< suffix.len orig.len)
			       (string=? suffix.str
					 (substring orig.str prefix.len orig.len)))
			  (cons (string->symbol (substring orig.str 0 prefix.len))
				(cdr subst.entry))
			(%local-synner
			 (string-append "binding name \"" orig.str
					"\" cannot be desuffixed of \"" suffix.str "\"")))))
	       subst))))

	;;According to R6RS:  the symbol LIBRARY can be used  to quote a
	;;library reference  whose first identifier is  "for", "rename",
	;;etc.
	((?library (?spec* ...))
	 (eq? (syntax->datum ?library) 'library)
	 (%import-library ?spec*))

	(_
	 (%synner "invalid import set" import-spec import-set))))

    (define (%import-library libref)
      (receive (name version-conforms-to-reference?)
	  (%parse-library-reference libref)
	(when (null? name)
	  (%synner "empty library name" libref))
	;;Search  for the  library first  in the  collection of  already
	;;installed libraires, then on  the file system.  If successful:
	;;LIB is an instance of LIBRARY struct.
	(let ((lib (find-library-by-name (syntax->datum libref))))
	  (unless (version-conforms-to-reference? (library-name->version (library-name lib)))
	    (%synner "library does not satisfy version specification" libref lib))
	  ((imp-collector) lib)
	  (library-export-subst lib))))

    #| end of module: %IMPORT-SPEC->EXPORT-SUBST |# )

;;; --------------------------------------------------------------------

  (module (%parse-library-reference)

    (define (%parse-library-reference libref)
      ;;Given a  SYNTAX-MATCH expression argument LIBREF  representing a
      ;;library reference  as defined  by R6RS:  parse and  validate it.
      ;;Return 2 values:
      ;;
      ;;1. A list of symbols representing the library spec identifiers.
      ;;
      ;;2. A predicate function to be used to check if a library version
      ;;   conforms with the requirements of this library specification.
      ;;
      (let recur ((spec libref))
	(syntax-match spec ()

	  (((?version-spec* ...))
	   (values '() (%build-version-pred ?version-spec* libref)))

	  ((?id . ?rest*)
	   (symbol-syntax? ?id)
	   (receive (name pred)
	       (recur ?rest*)
	     (values (cons (syntax->datum ?id) name)
		     pred)))

	  (()
	   (values '() (lambda (x) #t)))

	  (_
	   (%synner "invalid library specification in import set" libref spec)))))

    (define (%build-version-pred version-reference libref)
      ;;Recursive function.  Given a  version reference: validate it and
      ;;build and return a predicate function that can be used to verify
      ;;if library versions do conform.
      ;;
      ;;LIBREF must be  the enclosing library reference, it  is used for
      ;;descriptive error reporting.
      ;;
      (define (%recurse X)
	(%build-version-pred X libref))
      (syntax-match version-reference ()
	(()
	 (lambda (x) #t))

	((?and ?version* ...)
	 (eq? (syntax->datum ?and) 'and)
	 (let ((predicate* (map %recurse ?version*)))
	   (lambda (x)
	     (for-all (lambda (pred)
			(pred x))
	       predicate*))))

	((?or ?version* ...)
	 (eq? (syntax->datum ?or) 'or)
	 (let ((predicate* (map %recurse ?version*)))
	   (lambda (x)
	     (exists (lambda (pred)
		       (pred x))
	       predicate*))))

	((?not ?version)
	 (eq? (syntax->datum ?not) 'not)
	 (let ((pred (%recurse ?version)))
	   (lambda (x)
	     (not (pred x)))))

	((?subversion* ...)
	 (let ((predicate* (map (lambda (subversion)
				  (%build-subversion-pred subversion libref))
			     ?subversion*)))
	   (lambda (x)
	     (let loop ((predicate* predicate*)
			(x          x))
	       (cond ((null? predicate*)
		      #t)
		     ((null? x)
		      #f)
		     (else
		      (and ((car predicate*) (car x))
			   (loop (cdr predicate*) (cdr x)))))))))

	(_
	 (%synner "invalid version reference" libref version-reference))))

    (define (%build-subversion-pred subversion* libref)
      ;;Recursive function.   Given a subversion reference:  validate it
      ;;and build  and return a predicate  function that can be  used to
      ;;verify if library versions do conform.
      ;;
      ;;LIBREF must be  the enclosing library reference, it  is used for
      ;;descriptive error reporting.
      ;;
      (define (%recurse X)
	(%build-subversion-pred X libref))
      (syntax-match subversion* ()
	(?subversion-number
	 (%subversion? ?subversion-number)
	 (let ((N (syntax->datum ?subversion-number)))
	   (lambda (x)
	     (= x N))))

	((?and ?subversion* ...)
	 (eq? (syntax->datum ?and) 'and)
	 (let ((predicate* (map %recurse ?subversion*)))
	   (lambda (x)
	     (for-all (lambda (pred)
			(pred x))
	       predicate*))))

	((?or ?subversion* ...)
	 (eq? (syntax->datum ?or) 'or)
	 (let ((predicate* (map %recurse ?subversion*)))
	   (lambda (x)
	     (exists (lambda (pred)
		       (pred x))
	       predicate*))))

	((?not ?subversion)
	 (eq? (syntax->datum ?not) 'not)
	 (let ((pred (%recurse ?subversion)))
	   (lambda (x)
	     (not (pred x)))))

        ((?<= ?subversion-number)
	 (and (eq? (syntax->datum ?<=) '<=)
	      (%subversion? ?subversion-number))
	 (let ((N (syntax->datum ?subversion-number)))
	   (lambda (x)
	     (<= x N))))

	((?>= ?subversion-number)
	 (and (eq? (syntax->datum ?>=) '>=)
	      (%subversion? ?subversion-number))
	 (let ((N (syntax->datum ?subversion-number)))
	   (lambda (x)
	     (>= x N))))

	(_
	 (%synner "invalid sub-version specification in library reference"
		  libref subversion*))))

    (define-inline (%subversion? stx)
      (library-version-number? (syntax->datum stx)))

    #| end of module: %PARSE-LIBRARY-REFERENCE |# )

;;; --------------------------------------------------------------------

  (module (%merge-export-subst*)

    (define (%merge-export-subst* subst1 subst2)
      ;;Recursive function.  Given two substs: merge them and return the
      ;;result.
      ;;
      ;;Assume that SUBST1  has unique entries in itself  and SUBST2 has
      ;;unique entrie in  itself.  If an entry from SUBST1  has the name
      ;;name but different label from an entry in SUBST2: raise a syntax
      ;;error.
      ;;
      (if (pair? subst1)
	  (%insert-to-subst ($car subst1)
			    (%merge-export-subst* ($cdr subst1) subst2))
	subst2))

    (define-inline (%insert-to-subst entry subst)
      ;;Given a subst  ENTRY and a SUBST: insert the  entry in the subst
      ;;if it is not already present  and return the result; else return
      ;;SUBST.
      ;;
      (let ((name  ($car entry))
	    (label ($cdr entry)))
	(cond ((assq name subst)
	       ;;An entry for NAME already exists.
	       => (lambda (x)
		    (if (eq? (cdr x) label)
			;;Same name and same label: OK.
			subst
		      ;;Same name but different label: ERROR.
		      (%error-two-import-with-different-bindings name))))
	      (else
	       ;;Prepend the new entry.
	       (cons entry subst)))))

    #| end of module: %MERGE-EXPORT-SUBST* |# )

;;; --------------------------------------------------------------------

  (define (find* sym* subst import-spec-stx)
    ;;Find all the entries in SUBST  having as name the symbols in SYM*;
    ;;return the  list of labels  from the  selected entries.  It  is an
    ;;error if a name in SYM* is not present in the SUBST.
    ;;
    ;;IMPORT-SPEC-STX must  be a  syntax object representing  the import
    ;;spec in which  we search for the SYM*; it  is used for descriptive
    ;;error reporting.
    ;;
    ;;This function is the one that raises  an error if we try to import
    ;;an unexistent binding, as in:
    ;;
    ;;   (import (only (vicare) this-does-not-exist))
    ;;
    (map (lambda (sym)
	   (cond ((assq sym subst)
		  => cdr)
		 (else
		  (%synner "cannot find identifier in export list of import spec"
			   import-spec-stx sym))))
      sym*))

  (define (rem* sym* subst)
    ;;Remove  from SUBST  all the  entries having  name in  the list  of
    ;;symbols SYM*.  Return the new  subst with the entries removed.  It
    ;;is fine if some names in SYM* are not present in SUBST.
    ;;
    (let recur ((subst subst))
      (cond ((null? subst)
	     '())
	    ((memq (caar subst) sym*)
	     (recur (cdr subst)))
	    (else
	     (cons (car subst) (recur (cdr subst)))))))

  (define (remove-dups ls)
    ;;Recursive  function.  Remove  duplicate  items from  the list  LS.
    ;;Compare items with EQ?.
    ;;
    (cond ((null? ls)
	   '())
	  ((memq (car ls) (cdr ls))
	   (remove-dups (cdr ls)))
	  (else
	   (cons (car ls) (remove-dups (cdr ls))))))

;;; --------------------------------------------------------------------

  (define (symbol-syntax? obj)
    (symbol? (syntax->datum obj)))

  (define (%error-two-import-with-different-bindings name)
    (%synner "two imports with different bindings" name))

  (case-define %synner
    ((message form)
     (syntax-violation __who__ message form))
    ((message form subform)
     (syntax-violation __who__ message form subform)))

  #| end of module: PARSE-IMPORT-SPEC* |# )


;;;; lexical environment: LEXENV entries and syntactic bindings helpers

;;Given the entry  from a lexical environment: return  the gensym acting
;;as label.
;;
(define lexenv-entry-label car)

;;Given the entry from a lexical environment: return the binding value.
;;
(define lexenv-entry-binding-descriptor cdr)

;;Build and return a new binding.
;;
(define-syntax-rule (make-binding ?bind-type ?bind-val)
  (cons ?bind-type ?bind-val))

;;Given a binding, return its type: a symbol.
;;
(define syntactic-binding-type car)

;;Given a binding, return its value: a pair.
;;
(define syntactic-binding-value cdr)

;;; --------------------------------------------------------------------
;;; core primitive bindings

(define (core-primitive-binding? binding)
  (and (pair? binding)
       (eq? 'core-prim (syntactic-binding-type binding))))

(define-syntax-rule (core-primitive-binding-symbol ?binding)
  (cdr ?binding))

;;; --------------------------------------------------------------------
;;; lexical variable bindings

(define (make-lexical-var-binding lex)
  ;;Build  and  return a  syntactic  binding  representing an  immutated
  ;;lexical variable.  LEX  must be a symbol representing the  name of a
  ;;lexical variable in the expanded language forms.
  ;;
  (cons* 'lexical lex #f))

;;Accessors for the value in a lexical variable binding.
;;
(define lexical-var		car)
(define lexical-var-mutated?	cdr)

(define (set-lexical-mutable! bind-val)
  ;;Mutator  for the  ?MUTATED  boolean in  a  lexical variable  binding
  ;;value.  This  function must  be applied to  the ?BINDING-VALUE  of a
  ;;lexical variable LEXENV  entry to signal that somewhere  in the code
  ;;this variable is mutated.
  ;;
  (set-cdr! bind-val #t))

(define (add-lexical-binding label lex lexenv)
  ;;Push on  the LEXENV  a new entry  representing an  immutated lexical
  ;;variable binding; return the resulting LEXENV.
  ;;
  ;;LABEL  must be  a syntactic  binding label.   LEX must  be a  symbol
  ;;representing the name of a lexical variable in the expanded language
  ;;forms.
  ;;
  (cons (cons label (make-lexical-var-binding lex))
	lexenv))

(define (add-lexical-bindings label* lex* lexenv)
  ;;Push  on the  given LEXENV  multiple entries  representing immutated
  ;;lexical variable bindings; return the resulting LEXENV.
  ;;
  ;;LABEL* must be  a list of syntactic binding labels.   LEX* must be a
  ;;list of symbols  representing the names of lexical  variables in the
  ;;expanded language forms.
  ;;
  (if (null? label*)
      lexenv
    (add-lexical-bindings ($cdr label*) ($cdr lex*)
			  (add-lexical-binding ($car label*) ($car lex*) lexenv))))

;;; --------------------------------------------------------------------
;;; local macro with non-variable transformer bindings

(define (make-local-macro-binding transformer expanded-expr)
  (cons* 'local-macro transformer expanded-expr))

;;; --------------------------------------------------------------------
;;; local macro with variable transformer bindings

(define (make-local-identifier-macro-binding transformer expanded-expr)
  (cons* 'local-macro! transformer expanded-expr))

;;; --------------------------------------------------------------------
;;; struct/record type descriptor bindings

(define-syntax-rule (make-struct-or-record-type-descriptor-binding ?bind-val)
  (cons '$rtd ?bind-val))

(define (struct-or-record-type-descriptor-binding? binding)
  (and (pair? binding)
       (eq? '$rtd (syntactic-binding-type binding))))

;;; --------------------------------------------------------------------
;;; Vicare struct type descriptor bindings

(define (struct-type-descriptor-binding? binding)
  (and (struct-or-record-type-descriptor-binding? binding)
       (struct-type-descriptor-bindval? (syntactic-binding-value binding))))

(define-syntax-rule (struct-type-descriptor-binding-std ?binding)
  (struct-type-descriptor-bindval-std (syntactic-binding-value ?binding)))

(define-syntax-rule (struct-type-descriptor-bindval? ?bind-val)
  (struct-type-descriptor? ?bind-val))

(define-syntax-rule (struct-type-descriptor-bindval-std ?bind-val)
  ?bind-val)

;;; --------------------------------------------------------------------
;;; R6RS record-type descriptor binding

(define (core-rtd-binding? binding)
  (and (pair? binding)
       (eq? '$core-rtd (syntactic-binding-type binding))))

;;;

(define (r6rs-record-type-descriptor-binding? binding)
  (and (struct-or-record-type-descriptor-binding? binding)
       (pair? (syntactic-binding-value binding))))

(define-syntax-rule (make-r6rs-record-type-descriptor-binding ?rtd-id ?rcd-id ?spec)
  (cons '$rtd (make-r6rs-record-type-descriptor-bindval ?rtd-id ?rcd-id ?spec)))

(define-syntax-rule (r6rs-record-type-descriptor-binding-rtd ?binding)
  (r6rs-record-type-descriptor-bindval-rtd (syntactic-binding-value ?binding)))

(define-syntax-rule (r6rs-record-type-descriptor-binding-rcd ?binding)
  (r6rs-record-type-descriptor-bindval-rcd (syntactic-binding-value ?binding)))

(define-syntax-rule (r6rs-record-type-descriptor-binding-spec ?binding)
  (r6rs-record-type-descriptor-bindval-spec (syntactic-binding-value ?binding)))

;;;

(define-syntax-rule (make-r6rs-record-type-descriptor-bindval ?rtd-id ?rcd-id ?spec)
  (cons ?rtd-id (cons ?rcd-id ?spec)))

(define-syntax-rule (r6rs-record-type-descriptor-bindval? ?bind-val)
  (pair? ?bind-val))

(define-syntax-rule (r6rs-record-type-descriptor-bindval-rtd ?bind-val)
  (car ?bind-val))

(define-syntax-rule (r6rs-record-type-descriptor-bindval-rcd ?bind-val)
  (cadr ?bind-val))

(define-syntax-rule (r6rs-record-type-descriptor-bindval-spec ?bind-val)
  (cddr ?bind-val))

;;;

(module R6RS-RECORD-TYPE-SPEC
  (make-r6rs-record-type-spec
   r6rs-record-type-spec?
   r6rs-record-type-descriptor-binding-safe-accessor
   r6rs-record-type-descriptor-binding-safe-mutator
   r6rs-record-type-descriptor-binding-unsafe-accessor
   r6rs-record-type-descriptor-binding-unsafe-mutator)

  (define-record r6rs-record-type-spec
    (safe-accessors-table
		;Alist  mapping all  field names  to the  identifiers to
		;which the safe accessors are bound.
     safe-mutators-table
		;Alist mapping mutable field names to the identifiers to
		;which safe mutators are bound.
     unsafe-accessors-table
		;False  or   alist  mapping  all  field   names  to  the
		;identifiers to which the unsafe accessors are bound.
     unsafe-mutators-table
		;False  or  alist mapping  mutable  field  names to  the
		;identifiers to which unsafe mutators are bound.
     ))

  (define (r6rs-record-type-descriptor-binding-safe-accessor binding field-name-id synner)
    (%spec-actor binding field-name-id r6rs-record-type-spec-safe-accessors-table
		 'record-accessor synner))

  (define (r6rs-record-type-descriptor-binding-safe-mutator binding field-name-id synner)
    (%spec-actor binding field-name-id r6rs-record-type-spec-safe-mutators-table
		 'record-mutator synner))

  (define (r6rs-record-type-descriptor-binding-unsafe-accessor binding field-name-id synner)
    (%spec-actor binding field-name-id r6rs-record-type-spec-unsafe-accessors-table
		 'unsafe-record-accessor synner))

  (define (r6rs-record-type-descriptor-binding-unsafe-mutator binding field-name-id synner)
    (%spec-actor binding field-name-id r6rs-record-type-spec-unsafe-mutators-table
		 'unsafe-record-mutator synner))

  (define (%spec-actor binding field-name-id table-getter actor-constructor synner)
    ;;Given an  R6RS record  type descriptor  binding and  an identifier
    ;;representing  a   record  field  name:  return   a  syntax  object
    ;;representing an expression which,  expanded and evaluated, returns
    ;;the accessor or mutator for the named field.
    ;;
    ;;TABLE-GETTER must be a function which, applied to the record spec,
    ;;returns the required association list.
    ;;
    ;;ACTOR-CONSTRUCTOR  must be  one of  the symbols:  record-accessor,
    ;;record-mutator.
    ;;
    (let ((field-name-sym (syntax->datum field-name-id))
	  (spec           (r6rs-record-type-descriptor-binding-spec binding)))
      (cond ((and (r6rs-record-type-spec? spec)
		  (assq field-name-sym (table-getter spec)))
	     => cdr)
	    (else
	     ;;Fallback  to   the  common  field  accessor   or  mutator
	     ;;constructor.
	     (let ((rtd-id (r6rs-record-type-descriptor-binding-rtd binding)))
	       (bless
		`(,actor-constructor ,rtd-id (quote ,field-name-sym))))))))

  #| end of module |# )

;;; --------------------------------------------------------------------
;;; fluid syntax bindings

(define-syntax-rule (make-fluid-syntax-binding ?label)
  (cons '$fluid ?label))

(define (fluid-syntax-binding? binding)
  (and (pair? binding)
       (eq? '$fluid (syntactic-binding-type binding))))

(define-syntax-rule (fluid-syntax-binding-fluid-label ?binding)
  (syntactic-binding-value ?binding))

;;; --------------------------------------------------------------------
;;; rename bindings

(define-syntax-rule (make-synonym-syntax-binding ?label)
  (cons '$synonym ?label))

(define (synonym-syntax-binding? binding)
  (and (pair? binding)
       (eq? '$synonym (syntactic-binding-type binding))))

(define-syntax-rule (synonym-syntax-binding-synonym-label ?binding)
  (syntactic-binding-value ?binding))

;;; --------------------------------------------------------------------
;;; compile-time values bindings

(define (make-local-compile-time-value-binding obj expanded-expr)
  ;;Given as arguments:  the actual object computed  from a compile-time
  ;;expression  and  a  core  language sexp  representing  the  original
  ;;expression already expanded, build and return a syntax binding.
  ;;
  (cons* 'local-ctv obj expanded-expr))

(define (local-compile-time-value-binding? binding)
  ;;Given a binding object: return  true if it represents a compile-time
  ;;value; otherwise return false.
  ;;
  (and (pair? binding)
       (eq? 'local-ctv (syntactic-binding-type binding))))

(define local-compile-time-value-binding-object
  ;;Given a binding representing a  local compile time value: return the
  ;;actual compile-time object.
  ;;
  cadr)

(define (global-compile-time-value-binding-object binding)
  (let ((lib (cadr binding))
	(loc (cddr binding)))
    ;;If this global binding use is the first time a binding from LIB is
    ;;used: visit the library.
    (unless (eq? lib '*interaction*)
      (visit-library lib))
    ;;FIXME The following form should really be just:
    ;;
    ;;   (symbol-value loc)
    ;;
    ;;because the value  in LOC should be the  actual compile-time value
    ;;object.  Instead  there is at least  a case in which  the value in
    ;;LOC is the full compile-time value:
    ;;
    ;;   (ctv . ?obj)
    ;;
    ;;It happens when the library:
    ;;
    ;;   (library (demo)
    ;;     (export obj)
    ;;     (import (vicare))
    ;;     (define-syntax obj (make-compile-time-value 123)))
    ;;
    ;;is precompiled and then loaded by the program:
    ;;
    ;;   (import (vicare) (demo))
    ;;   (define-syntax (doit stx)
    ;;     (lambda (ctv-retriever) (ctv-retriever #'obj)))
    ;;   (doit)
    ;;
    ;;the expansion  of "(doit)" fails  with an error because  the value
    ;;returned  by  the  transformer  is  the  CTV  special  value.   We
    ;;circumvent this problem  by testing below the nature  of the value
    ;;in LOC, but it is just  a temporary workaround.  (Marco Maggi; Sun
    ;;Jan 19, 2014)
    ;;
    (let ((ctv (symbol-value loc)))
      (if (compile-time-value? ctv)
	  (compile-time-value-object ctv)
	ctv))))

;;; --------------------------------------------------------------------
;;; module bindings

(define (make-module-binding iface)
  (cons '$module iface))

;;; --------------------------------------------------------------------
;;; pattern variable bindings

(define (pattern-variable-binding? binding)
  (and (pair? binding)
       (eq? 'syntax (syntactic-binding-type binding))))


;;;; lexical environment: mapping labels to syntactic binding descriptors

(module (label->syntactic-binding)

  (case-define label->syntactic-binding
    ;;Look up the  symbol LABEL in the  LEXENV as well as  in the global
    ;;environment.   If an  entry with  key LABEL  is found:  return the
    ;;associated syntactic  binding descriptor; if no  matching entry is
    ;;found, return one of the special descriptors:
    ;;
    ;;   (displaced-lexical . ())
    ;;   (displaced-lexical . #f)
    ;;
    ;;If the  binding descriptor  represents a  fluid syntax  or synonym
    ;;syntax: follow  through and return the  innermost re-definition of
    ;;the binding.
    ;;
    ((label lexenv)
     (label->syntactic-binding label lexenv '()))
    ((label lexenv accum-labels)
     (let ((binding (label->syntactic-binding/no-indirection label lexenv)))
       (cond ((fluid-syntax-binding? binding)
	      ;;Fluid syntax  bindings (created  by DEFINE-FLUID-SYNTAX)
	      ;;require  different   logic.   The   lexical  environment
	      ;;contains the main fluid  syntax definition entry and one
	      ;;or more subordinate fluid syntax re-definition entries.
	      ;;
	      ;;LABEL  is  associated  to the  main  binding  definition
	      ;;entry;  its syntactic  binding  descriptor contains  the
	      ;;fluid  label,  which  is   associated  to  one  or  more
	      ;;subordinate re-definitions.  We  extract the fluid label
	      ;;from  BINDING,  then  search  for  the  innermost  fluid
	      ;;binding associated to it.
	      ;;
	      ;;Such  search for  the fluid  re-definition binding  must
	      ;;begin from  LEXENV, and then in  the global environment.
	      ;;This  is because  we can  nest at  will FLUID-LET-SYNTAX
	      ;;forms  that   redefine  the   binding  by   pushing  new
	      ;;re-definition entries  on the LEXENV.  To  reach for the
	      ;;innermost we must query the LEXENV first.
	      ;;
	      ;;If there  is no binding descriptor  for FLUID-LABEL: the
	      ;;return value will be:
	      ;;
	      ;;   (displaced-lexical . #f)
	      ;;
	      (let* ((fluid-label   (fluid-syntax-binding-fluid-label binding))
		     (fluid-binding (cond ((assq fluid-label lexenv)
					   => lexenv-entry-binding-descriptor)
					  (else
					   (label->syntactic-binding/no-indirection fluid-label '())))))
		(if (synonym-syntax-binding? fluid-binding)
		    (%follow-through binding lexenv accum-labels)
		  fluid-binding)))

	     ((synonym-syntax-binding? binding)
	      (%follow-through binding lexenv accum-labels))

	     (else
	      binding)))))

  (define (%follow-through binding lexenv accum-labels)
    (let ((synonym-label (synonym-syntax-binding-synonym-label binding)))
      (if (memq synonym-label accum-labels)
	  (stx-error #f "circular reference detected while resolving synonym transformers")
	(label->syntactic-binding synonym-label lexenv (cons synonym-label accum-labels)))))

  #| end of module |# )

(define (label->syntactic-binding/no-indirection label lexenv)
  ;;Look up  the symbol  LABEL in the  LEXENV as well  as in  the global
  ;;environment.   If an  entry  with  key LABEL  is  found: return  the
  ;;associated  syntactic binding  descriptor; if  no matching  entry is
  ;;found, return one of the special descriptors:
  ;;
  ;;   (displaced-lexical . ())
  ;;   (displaced-lexical . #f)
  ;;
  ;;If the  binding descriptor  represents a fluid  syntax or  a synonym
  ;;syntax: *do not* follow through and return the binding descriptor of
  ;;the syntax definition.
  ;;
  ;;Since all labels are unique,  it doesn't matter which environment we
  ;;consult first; we  lookup the global environment  first because it's
  ;;faster.
  ;;
  (cond ((not label)
	 ;;If LABEL is the result of a previous call to ID->LABEL for an
	 ;;unbound  identifier: LABEL  is  false.  This  check makes  it
	 ;;possible to use the concise expression:
	 ;;
	 ;;   (define ?binding
	 ;;     (label->syntactic-binding (id->label ?id) ?lexenv))
	 ;;
	 ;;provided that later we check for the type of ?BINDING.
	 '(displaced-lexical))

	;;If a label is associated to  a binding from the the boot image
	;;environment or  to a binding  from a library's  EXPORT-ENV: it
	;;has the associated descriptor  in its "value" field; otherwise
	;;such field is set to #f.
	;;
	;;So,  if we  have a  label, we  can check  if it  references an
	;;imported binding simply by checking its "value" field; this is
	;;what IMPORTED-LABEL->SYNTACTIC-BINDING does.
	;;
	((imported-label->syntactic-binding label)
	 => (lambda (binding)
	      (if (core-rtd-binding? binding)
		  (make-struct-or-record-type-descriptor-binding
		   (map bless (syntactic-binding-value binding)))
		binding)))

	;;Search the given LEXENV.
	;;
	((assq label lexenv)
	 => lexenv-entry-binding-descriptor)

	;;Search the interaction top-level environment, if any.
	;;
	((top-level-context)
	 => (lambda (env)
	      (cond ((assq label (interaction-env-lab.loc/lex* env))
		     => (lambda (lab.loc/lex)
			  ;;Fabricate a  binding descriptor representing
			  ;;an immutated  lexical variable.  We  need to
			  ;;remember that  for interaction environments:
			  ;;we  reuse  the  storage location  gensym  as
			  ;;lexical gensym.
			  (make-lexical-var-binding (cdr lab.loc/lex))))
		    (else
		     ;;Unbound label.
		     '(displaced-lexical . #f)))))

	;;Unbound label.
	;;
	(else
	 '(displaced-lexical . #f))))


;;;; EXPORT-ENV helpers

(define-syntax-rule (make-export-env-entry ?label ?type ?loc)
  ;;Given a  label gensym, a  symbol representing an EXPORT-ENV  type, a
  ;;loc gensym: build and return an entry of EXPORT-ENV.
  ;;
  (cons* ?label ?type ?loc))

(define-syntax-rule (export-binding-type ?export-binding)
  ;;Given an export binding return the symbol representin its type.
  ;;
  (car ?export-binding))

(define-syntax-rule (export-binding-loc ?export-binding)
  ;;Given an export binding return the loc gensym holding its value.
  ;;
  (cdr ?export-binding))


;;;; marks of lexical contours

(define gen-mark
  ;;Generate a new unique mark.  We want a new string for every function
  ;;call.
  string)
;;The version below is useful for debugging.
;;
;; (define gen-mark
;;   (let ((i 0))
;;     (lambda ()
;;       (set! i (+ i 1))
;;       (string-append "m." (number->string i)))))

;;We use #f as the anti-mark.
(define-constant anti-mark #f)
(define anti-mark? not)

;;The body of  a library, when it  is first processed, gets  this set of
;;marks...
(define-constant TOP-MARK*
  '(top))

(define (list-of-marks? obj)
  (and (list? obj)
       (for-all (lambda (item)
		  (or (eq? 'top obj) ;proper top mark
		      (string? obj)  ;proper lexical contour mark
		      (not obj)))    ;anti-mark
	 obj)))

;;... consequently, every  syntax object that has a "top"  symbol in its
;;marks set was present in the program source.
(define-syntax-rule (top-marked? mark*)
  (memq 'top mark*))

(define* (symbol->top-marked-identifier {sym symbol?})
  ;;Given a  raw Scheme  symbol return a  syntax object  representing an
  ;;identifier with top marks.
  ;;
  (make-<stx> sym TOP-MARK* '() '()))

(define* (top-marked-symbols {rib rib?})
  ;;Scan the <rib> RIB and return a list of symbols representing binding
  ;;names and having the top mark.
  ;;
  (receive (sym* mark**)
      ;;If RIB is sealed the fields  hold vectors, else they hold lists;
      ;;we want lists here.
      (let ((sym*   ($<rib>-name*  rib))
	    (mark** ($<rib>-mark** rib)))
	(if ($<rib>-sealed/freq rib)
	    (values (vector->list sym*)
		    (vector->list mark**))
	  (values sym* mark**)))
    (let recur ((sym*   sym*)
		(mark** mark**))
      (cond ((null? sym*)
	     '())
	    ((equal? ($car mark**) TOP-MARK*)
	     (cons ($car sym*)
		   (recur ($cdr sym*) ($cdr mark**))))
	    (else
	     (recur ($cdr sym*) ($cdr mark**)))))))


;;;; rib record type definition
;;
;;A  <RIB> is  a  record constructed  at every  lexical  contour in  the
;;program to  hold informations about  the variables introduced  in that
;;contour; "lexical contours" are, for example, LET and similar syntaxes
;;that can introduce bindings.
;;
;;The purpose  of ribs is  to map original  binding names to  the labels
;;associated to those bindings; this map  is used when establishing if a
;;syntax  object identifier  in  reference position  is  captured by  an
;;existing sytactic binding.
;;
;;Sealing ribs
;;------------
;;
;;A non-empty  <RIB> can be sealed  once all bindings are  inserted.  To
;;seal a <RIB>, we convert the lists NAME*, MARK** and LABEL* to vectors
;;and insert a frequency vector in the SEALED/FREQ field.  The frequency
;;vector is a Scheme vector of exact integers.
;;
;;The  frequency vector  is an  optimization  that allows  the <RIB>  to
;;reorganize itself by  bubbling frequently used mappings to  the top of
;;the <RIB>.   This is possible because  the order in  which the binding
;;tuples appear in a <RIB> does not matter.
;;
;;The vector is  maintained in non-descending order  and an identifier's
;;entry in the <RIB> is incremented at every access.  If an identifier's
;;frequency  exceeds the  preceeding one,  the identifier's  position is
;;promoted  to the  top of  its  class (or  the bottom  of the  previous
;;class).
;;
;;An unsealed rib with 2 binings looks as follows:
;;
;;   name*       = (b       a)
;;   mark**      = ((top)   (top))
;;   label*      = (lab.b   lab.a)
;;   sealed/freq = #f
;;
;;and right after sealing it:
;;
;;   name*       = #(b       a)
;;   mark**      = #((top)   (top))
;;   label*      = #(lab.b   lab.a)
;;   sealed/freq = #(0       0)
;;
;;after  accessing once  the  binding  of "b"  in  the  sealed rib,  its
;;frequency is incremented:
;;
;;   name*       = #(b       a)
;;   mark**      = #((top)   (top))
;;   label*      = #(lab.b   lab.a)
;;   sealed/freq = #(1       0)
;;
;;and after  accessing twice the binding  of "a" in the  sealed rib, the
;;tuples are swapped:
;;
;;   name*       = #(a       b)
;;   mark**      = #((top)   (top))
;;   label*      = #(lab.a   lab.b)
;;   sealed/freq = #(2       1)
;;
(define-record (<rib> make-<rib> rib?)
  (name*
		;List of symbols representing the original binding names
		;in the source code.
		;
		;When the  <RIB> is sealed:  the list is converted  to a
		;vector.

   mark**
		;List of sublists of marks;  there is a sublist of marks
		;for every item in NAME*.
		;
		;When the  <RIB> is sealed:  the list is converted  to a
		;vector.

   label*
		;List   of  label   gensyms  uniquely   identifying  the
		;syntactic bindings; there  is a label for  each item in
		;NAME*.
		;
		;When the  <RIB> is sealed:  the list is converted  to a
		;vector.

   sealed/freq
		;False or  vector of  exact integers.  When  false: this
		;<RIB> is extensible, that is  new bindings can be added
		;to it.  When a vector: this <RIB> is selaed.
		;
		;See  below  the  code  section "sealing  ribs"  for  an
		;explanation of the frequency vector.
   )
  (lambda (S port subwriter) ;record printer function
    (define-syntax-rule (%display ?thing)
      (display ?thing port))
    (define-syntax-rule (%write ?thing)
      (write ?thing port))
    (define-syntax-rule (%pretty-print ?thing)
      (pretty-print* ?thing port 0 #f))
    (%display "#<rib")
    (%display " name*=")	(%pretty-print (<rib>-name*  S))
    (%display " mark**=")	(%pretty-print (<rib>-mark** S))
    (%display " label*=")	(%pretty-print (<rib>-label* S))
    (%display " sealed/freq=")	(%pretty-print (<rib>-sealed/freq S))
    (%display ">")))

(define (false-or-rib? obj)
  (or (not obj)
      (rib? obj)))

(define (list-of-ribs? obj)
  (and (list? obj)
       (for-all rib? obj)))

(define-inline (make-empty-rib)
  ;;Build and return a new empty <RIB> record.
  ;;
  ;;Empty ribs are used to represent freshly created lexical contours in
  ;;which no initial bindings are defined.  For example, internal bodies
  ;;might contain internal definitions:
  ;;
  ;;   (let ()
  ;;     (define a 1)
  ;;     (define b 2)
  ;;     (display a))
  ;;
  ;;but we  know about  them only  when we  begin their  expansion; when
  ;;creating the lexical contour for them we must create an empty rib.
  ;;
  ;;If STX  is a syntax object  representing an expression that  must be
  ;;expanded in a new lexical contour, we do:
  ;;
  ;;   (define stx  #'((define a 1)
  ;;                   (define b 2)
  ;;                   (display a)))
  ;;   (define rib  (make-empty-rib))
  ;;   (define stx^ (push-lexical-contour rib stx))
  ;;
  ;;and then  hand the resulting  syntax object STX^ to  the appropriate
  ;;"chi-*"  function  to  perform  the expansion.   Later  we  can  add
  ;;bindings to the rib with:
  ;;
  ;;   (extend-rib! rib id label shadowing-definitions?)
  ;;
  (make-<rib> '() '() '() #f))

(define (make-filled-rib id* label*)
  ;;Build and return a new <RIB> record initialised with bindings having
  ;;ID*  as   original  identifiers  and  LABEL*   as  associated  label
  ;;gensyms.
  ;;
  ;;ID* must  be a  list of syntax  object identifiers  representing the
  ;;original binding names.  LABEL* must be a list of label gensyms.
  ;;
  ;;For example, when creating a rib to represent the lexical context of
  ;;a LET syntax:
  ;;
  ;;   (let ((?lhs* ?rhs*) ...) ?body* ...)
  ;;
  ;;we do:
  ;;
  ;;   (define lhs-ids    ?lhs*)
  ;;   (define body-stx   ?body*)
  ;;   (define label*     (map gensym-for-label lhs-ids))
  ;;   (define rib        (make-filled-rib lhs-ids label*))
  ;;   (define body-stx^  (push-lexical-contour rib body-stx))
  ;;
  ;;and  then  hand  the  resulting   syntax  object  BODY-STX^  to  the
  ;;appropriate "chi-*" function to perform the expansion.
  ;;
  (let ((name*        (map identifier->symbol id*))
	(mark**       (map <stx>-mark* id*))
	(sealed/freq  #f))
    (make-<rib> name* mark** label* sealed/freq)))

(define* (make-top-rib name-vec label-vec)
  ;;Build  and  return a  new  <rib>  record initialised  with  bindings
  ;;imported  from a  set  or IMPORT  specifications  or an  environment
  ;;record.
  ;;
  ;;NAME-VEC is a  vector of symbols representing the  external names of
  ;;the  imported bindings.   LABEL-VEC  is a  vector  of label  gensyms
  ;;uniquely associated to the imported bindings.
  ;;
  ;;For example, when creating the  lexical contour for a top-level body
  ;;(either for a library or a program) we can do:
  ;;
  ;;   (define import-spec*
  ;;     '((vicare) (vicare ffi)))
  ;;
  ;;   (define rib
  ;;     (receive (name-vec label-vec)
  ;;         (let ()
  ;;           (import PARSE-IMPORT-SPEC)
  ;;           (parse-import-spec* import-spec*))
  ;;       (make-top-rib name-vec label-vec)))
  ;;
  ;;when  creating  the  lexical  contour  for  a  top-level  expression
  ;;evaluated by  EVAL in the  context of a  non-interactive environment
  ;;record we, cando:
  ;;
  ;;   (define env
  ;;     (environment '(vicare)))
  ;;
  ;;   (define rib
  ;;     (make-top-rib (env-names env) (env-labels env)))
  ;;
  (receive-and-return (rib)
      (make-empty-rib)
    (vector-for-each
        (lambda (name label)
          (if (symbol? name)
	      (let ((id                    (symbol->top-marked-identifier name))
		    (shadowing-definition? #t))
		(extend-rib! rib id label shadowing-definition?))
            (assertion-violation __who__
	      "Vicare bug: expected symbol as binding name" name)))
      name-vec label-vec)))

(define (export-subst->rib export-subst)
  ;;Build and return a new  <RIB> structure initialised with the entries
  ;;of EXPORT-SUBST.
  ;;
  ;;An EXPORT-SUBST  selects the  exported bindings among  the syntactic
  ;;bindings defined at the top-level of a library; it is an alist whose
  ;;keys are external symbol names of  the bindings and whose values are
  ;;the associated label gensyms.
  ;;
  (let ((name*        (map car export-subst)) ;exported binding names
	(mark**       (map (lambda (x) TOP-MARK*) export-subst))
	(label*       (map cdr export-subst))
	(sealed/freq  #f))
    (make-<rib> name* mark** label* sealed/freq)))

(module (extend-rib!)
  ;;A <RIB> can be extensible, or sealed.  Adding an identifier-to-label
  ;;mapping to  an extensible <RIB>  is achieved by prepending  items to
  ;;the field lists.
  ;;
  ;;For example, an empty extensible <RIB> has fields:
  ;;
  ;;   name*  = ()
  ;;   mark** = ()
  ;;   label* = ()
  ;;
  ;;adding a  binding to it  with name  "ciao", marks "(top)"  and label
  ;;"lab.ciao" means mutating the fields to:
  ;;
  ;;   name*  = (ciao)
  ;;   mark** = ((top))
  ;;   label* = (lab.ciao)
  ;;
  ;;pushing the  "binding tuple": ciao, (top),  lab.ciao; adding another
  ;;binding with name "hello", mark  "(top)" and label "lab.hello" means
  ;;mutating the fields to:
  ;;
  ;;   name*  = (hello     ciao)
  ;;   mark** = ((top)     (top))
  ;;   label* = (lab.hello lab.ciao)
  ;;
  ;;As further example, let's consider the form:
  ;;
  ;;   (lambda ()
  ;;     (define a 1)
  ;;     (define b 2)
  ;;     (list a b))
  ;;
  ;;when starting  to process the LAMBDA  internal body: a new  <RIB> is
  ;;created  and  is  added  to   the  metadata  of  the  syntax  object
  ;;representing  the  body itself;  when  each  internal definition  is
  ;;encountered,  a new  entry for  the  identifier is  added (via  side
  ;;effect) to the <RIB>:
  ;;
  ;;   name*  = (b       a)
  ;;   mark** = ((top)   (top))
  ;;   label* = (lab.b   lab.a)
  ;;
  ;;That the order in which the  binding tuples appear in the <RIB> does
  ;;not matter:  two tuples are different  when both the symbol  and the
  ;;marks are different and  it is an error to add twice  a tuple to the
  ;;same <RIB>.
  ;;
  ;;However, it  is possible to  redefine a  binding.  Let's say  we are
  ;;evaluating    forms   read    from    the    REPL:   the    argument
  ;;SHADOWING-DEFINITIONS? is true.  If we type:
  ;;
  ;;   vicare> (define a 1)
  ;;   vicare> (define a 2)
  ;;
  ;;after the first DEFINE is parsed the tuples are:
  ;;
  ;;   name*  = (a)
  ;;   mark** = ((top))
  ;;   label* = (lab.a.1)
  ;;
  ;;and after the secon DEFINE is parsed the tuples are:
  ;;
  ;;   name*  = (a)
  ;;   mark** = ((top))
  ;;   label* = (lab.a.2)
  ;;
  ;;we see that the label has changed.
  ;;
  (define* (extend-rib! {rib rib?} {id identifier?} label shadowing-definitions?)
    (when ($<rib>-sealed/freq rib)
      (assertion-violation __who__
	"Vicare: internal error: attempt to extend sealed RIB" rib))
    (let ((id.sym      (identifier->symbol id))
	  (id.mark*    ($<stx>-mark*  id))
	  (rib.name*   ($<rib>-name*  rib))
	  (rib.mark**  ($<rib>-mark** rib))
	  (rib.label*  ($<rib>-label* rib)))
      (cond ((%find-binding-with-same-marks id.sym id.mark* rib.name* rib.mark** rib.label*)
	     ;;A binding for ID already  exists in this lexical contour.
	     ;;For example, in an internal body we have:
	     ;;
	     ;;   (define a 1)
	     ;;   (define a 2)
	     ;;
	     ;;in an R6RS program or library we must raise an exception;
	     ;;at the REPL we just redefine the binding.
	     ;;
	     => (lambda (tail-of-rib.label*)
		  (unless (eq? label (car tail-of-rib.label*))
		    (if (not shadowing-definitions?)
			;;We  override the  already existent  label with
			;;the new label.
			(set-car! tail-of-rib.label* label)
		      ;;Signal an error if the identifier was already in
		      ;;the rib.
		      (syntax-violation 'expander
			"multiple definitions of identifier" id)))))
	    (else
	     ;;No binding exists for ID  in this lexical contour: create
	     ;;a new one.
	     ($set-<rib>-name*!  rib (cons id.sym   rib.name*))
	     ($set-<rib>-mark**! rib (cons id.mark* rib.mark**))
	     ($set-<rib>-label*! rib (cons label    rib.label*))))))

  (define-syntax-rule (%find-binding-with-same-marks id.sym id.mark*
						     rib.name* rib.mark** rib.label*)
    (and (memq id.sym rib.name*)
	 (%find id.sym id.mark* rib.name* rib.mark** rib.label*)))

  (define (%find id.sym id.mark* rib.name* rib.mark** rib.label*)
    ;;Here we know  that the list of symbols RIB.NAME*  has at least one
    ;;element equal to ID.SYM;  we iterate through RIB.NAME*, RIB.MARK**
    ;;and RIB.LABEL* looking for a tuple having name equal to ID.SYM and
    ;;marks equal to  ID.MARK* and return the tail  of RIB.LABEL* having
    ;;the associated label as car.  If  such binding is not found return
    ;;false.
    ;;
    (and (pair? rib.name*)
	 (if (and (eq? id.sym ($car rib.name*))
		  (same-marks? id.mark* ($car rib.mark**)))
	     rib.label*
	   (%find id.sym id.mark* ($cdr rib.name*) ($cdr rib.mark**) ($cdr rib.label*)))))

  #| end of module: EXTEND-RIB! |# )

(define* (seal-rib! {rib rib?})
  (let ((name* ($<rib>-name* rib)))
    (unless (null? name*) ;only seal if RIB is not empty
      (let ((name* (list->vector name*)))
	($set-<rib>-name*!       rib name*)
	($set-<rib>-mark**!      rib (list->vector ($<rib>-mark** rib)))
	($set-<rib>-label*!      rib (list->vector ($<rib>-label* rib)))
	($set-<rib>-sealed/freq! rib (make-vector (vector-length name*) 0))))))

(define* (unseal-rib! {rib rib?})
  (when ($<rib>-sealed/freq rib)
    ($set-<rib>-sealed/freq! rib #f)
    ($set-<rib>-name*!       rib (vector->list ($<rib>-name*  rib)))
    ($set-<rib>-mark**!      rib (vector->list ($<rib>-mark** rib)))
    ($set-<rib>-label*!      rib (vector->list ($<rib>-label* rib)))))


;;;; syntax object type definition
;;
;;First, let's  look at identifiers,  since they're the real  reason why
;;syntax objects are here to begin  with.  An identifier is an STX whose
;;EXPR is a symbol; in addition to the symbol naming the identifier, the
;;identifer has a list of marks and a list of ribs (and shifts).
;;
;;The idea  is that to get  the label of  an identifier, we look  up the
;;identifier's ribs for a mapping with the same name and same marks (see
;;SAME-MARKS? below).
;;
;;---
;;
;;A syntax  object may be wrapped  or unwrapped, so what  does that mean
;;exactly?
;;
;;A "wrapped syntax object" is just  a way of saying it's an STX record.
;;All identifiers are  STX records (with a symbol  in their EXPR field);
;;other objects such  as pairs and vectors may  be wrapped or unwrapped.
;;A wrapped pair is an STX whose EXPR is a pair.  An unwrapped pair is a
;;pair whose car  and cdr fields are themselves  syntax objects (wrapped
;;or unwrapped).
;;
;;We always  maintain the invariant  that we  do not double  wrap syntax
;;objects.  The  only way to  get a  doubly-wrapped syntax object  is by
;;doing  $DATUM->SYNTAX (above)  where  the datum  is  itself a  wrapped
;;syntax object  (R6RS may not  even consider wrapped syntax  objects as
;;datum, but let's not worry now).
;;
;;Syntax objects have, in addition  to the EXPR, a ribs-and-shifts field
;;RIB*: it is a list where each  element is either a <rib> or the symbol
;;"shift".  Normally,  a new  RIB is  added to an  STX at  every lexical
;;contour of the program in order  to capture the bindings introduced in
;;that contour.
;;
;;The MARK* field of an STX is  a list of marks; each of these marks can
;;be  either  a  generated mark  or  an  antimark.   Two marks  must  be
;;EQ?-comparable, so we use a string of one char (we assume that strings
;;are mutable in the underlying Scheme implementation).

(define-record <stx>
  (expr
		;A  symbolic   expression,  possibly   annotated,  whose
		;subexpressions can also be instances of <stx>.
   mark*
		;Null or  a proper list  of marks, including  the symbol
		;"top".
   rib*
		;Null or  a proper  list of  <rib> instances  or "shift"
		;symbols.   Every  <rib>  represents  a  nested  lexical
		;contour; a  "shift" represents the return  from a macro
		;transformer application.
		;
		;NOTE The  items in  the fields MARK*  and RIB*  are not
		;associated:  the two  lists can  grow independently  of
		;each other.   But, considering  the whole  structure of
		;nested  <stx> instances:  the  items in  all the  MARK*
		;fields are associated to the items in all the RIB*, see
		;the functions JOIN-WRAPS and ADD-MARK for details.
   ae*
		;List of  annotated expressions:  null or a  proper list
		;whose items are #f or  input forms of macro transformer
		;calls.  It is used to  trace the transformations a form
		;undergoes when it is processed as macro use.
		;
		;The  #f items  are inserted  when it  this instance  is
		;processed as input form of  a macro call, but are later
		;discarded.
   )
  (lambda (S port subwriter) ;record printer function
    (define-syntax-rule (%display ?thing)
      (display ?thing port))
    (define-syntax-rule (%write ?thing)
      (write ?thing port))
    (define-syntax-rule (%pretty-print ?thing)
      (pretty-print* ?thing port 0 #f))
    (%display "#<syntax")
    (%display " expr=")		(%pretty-print (syntax->datum S))
    (%display " mark*=")	(%pretty-print (<stx>-mark* S))
    (let ((expr (<stx>-expr S)))
      (when (annotation? expr)
	(let ((pos (annotation-textual-position expr)))
	  (when (source-position-condition? pos)
	    (%display " line=")		(%display (source-position-line    pos))
	    (%display " column=")	(%display (source-position-column  pos))
	    (%display " source=")	(%display (source-position-port-id pos))))))
    (%display ">")))

;;; --------------------------------------------------------------------

(define (syntax-object? obj)
  ;;Return #t if OBJ is a  wrapped or unwrapped syntax object; otherwise
  ;;return #f.  This is not a full validation, because a component <stx>
  ;;may contain a raw symbol.
  ;;
  (cond ((<stx>? obj)
	 obj)
	((pair? obj)
	 (cons (syntax-object? ($car obj)) (syntax-object? ($cdr obj))))
	((symbol? obj)
	 #f)
	((vector? obj)
	 (vector-map syntax-object? obj))
	(else
	 (non-compound-sexp? obj))))

(define (ribs-and-shifts? obj)
  ;;Return true  if OBJ is  a valid value for  the field RIB*  of <stx>;
  ;;otherwise return #f.
  ;;
  (and (list? obj)
       (for-all (lambda (item)
		  (or (rib? item)
		      (eq? item 'shift)))
	 obj)))

;;; --------------------------------------------------------------------

(define* (datum->syntax {id identifier?} datum)
  ($datum->syntax id datum))

(define ($datum->syntax id datum)
  ;;Since all the identifier->label bindings are encapsulated within the
  ;;identifier, converting a datum to a syntax object (non-hygienically)
  ;;is done simply by creating an <stx> that has the same marks and ribs
  ;;as the identifier.
  ;;
  ;;We  include also  the  annotated expression  from  ID because,  when
  ;;showing  an error  trace,  it  helps to  understand  from where  the
  ;;returned object comes.
  ;;
  (make-<stx> datum ($<stx>-mark* id) ($<stx>-rib* id) ($<stx>-ae* id)))

(define (syntax->datum S)
  (strip S '()))

(define* (mkstx expr-stx mark* {rib* ribs-and-shifts?} ae*)
  ;;This is the proper constructor for wrapped syntax objects.
  ;;
  ;;EXPR-STX can  be a raw sexp,  an instance of <STX>  or a (partially)
  ;;unwrapped syntax object.
  ;;
  ;;MARK* must be  null or a proper list of  marks, including the symbol
  ;;"top".
  ;;
  ;;RIB* must  be null or a  proper list of <rib>  instances and "shift"
  ;;symbols.
  ;;
  ;;AE* must be  null or a proper list of  annotated expressions: syntax
  ;;objects being input forms for macro transformer calls.
  ;;
  ;;When EXPR-STX  is a  raw sexp  or an  unwrapped syntax  object: just
  ;;build  and return  a  new  syntax object  with  the lexical  context
  ;;described by the given arguments.
  ;;
  ;;When EXPR-STX is a <stx> instance: join the wraps from EXPR-STX with
  ;;given wraps, making sure that marks and anti-marks and corresponding
  ;;shifts cancel properly.
  ;;
  (if (and (<stx>? expr-stx)
	   (not (top-marked? mark*)))
      (receive (mark* rib* ae*)
	  (join-wraps mark* rib* ae* expr-stx)
	(make-<stx> (<stx>-expr expr-stx) mark* rib* ae*))
    (make-<stx> expr-stx mark* rib* ae*)))

(define* (push-lexical-contour {rib rib?} expr-stx)
  ;;Add a rib to a syntax  object or expression and return the resulting
  ;;syntax object.   During the  expansion process  we visit  the nested
  ;;subexpressions  in a  syntax  object repesenting  source code:  this
  ;;procedure introduces a  lexical contour in the  context of EXPR-STX,
  ;;for example when we enter a LET syntax.
  ;;
  ;;RIB must be an instance of <RIB>.
  ;;
  ;;EXPR-STX can be a raw sexp, an instance of <STX> or a wrapped syntax
  ;;object.
  ;;
  ;;This function prepares  a computation that will  be lazily performed
  ;;later;  the  RIB will  be  pushed  on the  stack  of  ribs in  every
  ;;identifier in  the fully  unwrapped version  of the  returned syntax
  ;;object.
  ;;
  (let ((mark*	'())
	(ae*	'()))
    (mkstx expr-stx mark* (list rib) ae*)))

(define (expression-position x)
  (if (<stx>? x)
      (let ((x (<stx>-expr x)))
	(if (annotation? x)
	    (annotation-textual-position x)
	  (condition)))
    (condition)))

(define (syntax-annotation x)
  (if (<stx>? x)
      (<stx>-expr x)
    x))

;;; --------------------------------------------------------------------

(module (strip)
  ;;STRIP is used  to remove the wrap  of a syntax object.   It takes an
  ;;stx's expr  and marks.  If  the marks  contain a top-mark,  then the
  ;;expr is returned.
  ;;
  (define (strip x m*)
    (if (top-marked? m*)
	(if (or (annotation? x)
		(and (pair? x)
		     (annotation? ($car x)))
		(and (vector? x)
		     (not ($vector-empty? x))
		     (annotation? ($vector-ref x 0))))
	    ;;TODO Ask Kent  why this is a  sufficient test.  (Abdulaziz
	    ;;Ghuloum)
	    (%strip-annotations x)
	  x)
      (let f ((x x))
	(cond ((<stx>? x)
	       (strip ($<stx>-expr x) ($<stx>-mark* x)))
	      ((annotation? x)
	       (annotation-stripped x))
	      ((pair? x)
	       (let ((a (f ($car x)))
		     (d (f ($cdr x))))
		 (if (and (eq? a ($car x))
			  (eq? d ($cdr x)))
		     x
		   (cons a d))))
	      ((vector? x)
	       (let* ((old (vector->list x))
		      (new (map f old)))
		 (if (for-all eq? old new)
		     x
		   (list->vector new))))
	      (else x)))))

  (define (%strip-annotations x)
    (cond ((pair? x)
	   (cons (%strip-annotations ($car x))
		 (%strip-annotations ($cdr x))))
	  ((vector? x)
	   (vector-map %strip-annotations x))
	  ((annotation? x)
	   (annotation-stripped x))
	  (else x)))

  #| end of module: STRIP |# )


;;;; syntax objects: mapping identifiers to labels

(module (id->label)

  (define (id->label id)
    ;;Given the  identifier ID  search its ribs  for a  label associated
    ;;with the  same sym  and marks.   If found  return the  label, else
    ;;return false.
    ;;
    (let ((sym (identifier->symbol id)))
      (let search ((rib*  ($<stx>-rib*  id))
		   (mark* ($<stx>-mark* id)))
	(cond ((null? rib*)
	       #f)
	      ((eq? ($car rib*) 'shift)
	       ;;This is the  only place in the expander  where a symbol
	       ;;"shift" in a  RIB* makes some difference;  a "shift" is
	       ;;pushed on the RIB* when a mark is pushed on the MARK*.
	       ;;
	       ;;When  we   find  a  "shift"   in  RIB*:  we   skip  the
	       ;;corresponding mark in MARK*.
	       (search ($cdr rib*) ($cdr mark*)))
	      (else
	       (let ((rib ($car rib*)))
		 (define (next-search)
		   (search ($cdr rib*) mark*))
		 (if ($<rib>-sealed/freq rib)
		     (%search-in-sealed-rib rib sym mark* next-search)
		   (%search-in-rib rib sym mark* next-search))))))))

  (define-inline (%search-in-rib rib sym mark* next-search)
    (let loop ((name*   ($<rib>-name*  rib))
	       (mark**  ($<rib>-mark** rib))
	       (label*  ($<rib>-label* rib)))
      (cond ((null? name*)
	     (next-search))
	    ((and (eq? ($car name*) sym)
		  (same-marks? ($car mark**) mark*))
	     ($car label*))
	    (else
	     (loop ($cdr name*) ($cdr mark**) ($cdr label*))))))

  (module (%search-in-sealed-rib)

    (define (%search-in-sealed-rib rib sym mark* next-search)
      (define name* ($<rib>-name* rib))
      (let loop ((i       0)
		 (rib.len ($vector-length name*)))
	(cond (($fx= i rib.len)
	       (next-search))
	      ((and (eq? ($vector-ref name* i) sym)
		    (same-marks? mark* ($vector-ref ($<rib>-mark** rib) i)))
	       (receive-and-return (label)
		   ($vector-ref ($<rib>-label* rib) i)
		 (%increment-rib-frequency! rib i)))
	      (else
	       (loop ($fxadd1 i) rib.len)))))

    (define (%increment-rib-frequency! rib idx)
      (let* ((freq* (<rib>-sealed/freq rib))
	     (freq  (vector-ref freq* idx))
	     (i     (let loop ((i idx))
		      (if (zero? i)
			  0
			(let ((j (- i 1)))
			  (if (= freq (vector-ref freq* j))
			      (loop j)
			    i))))))
	($vector-set! freq* i (+ freq 1))
	(unless (= i idx)
	  (let ((name*  (<rib>-name*  rib))
		(mark** (<rib>-mark** rib))
		(label* (<rib>-label* rib)))
	    (let-syntax ((%vector-swap (syntax-rules ()
					 ((_ ?vec ?idx1 ?idx2)
					  (let ((V ($vector-ref ?vec ?idx1)))
					    ($vector-set! ?vec ?idx1 ($vector-ref ?vec ?idx2))
					    ($vector-set! ?vec ?idx2 V))))))
	      (%vector-swap name*  idx i)
	      (%vector-swap mark** idx i)
	      (%vector-swap label* idx i))))))

    #| end of module: %SEARCH-IN-SEALED-RIB |# )

  #| end of module: ID->LABEL |# )

(define (id->label/intern id)
  ;;Given the identifier ID search the lexical environment for a binding
  ;;that captures  it; if such  binding is found: return  the associated
  ;;label.
  ;;
  ;;If  no  capturing  binding  is found  but  a  top-level  interaction
  ;;environment  is  set:  we  fabricate   a  lexical  binding  in  such
  ;;environment so  that there exists a  lex gensym to name  the binding
  ;;and a loc gensym in which to store a value (actually the lex and the
  ;;loc are the same gensym).  This allows us to write "special" code on
  ;;the REPL, for example:
  ;;
  ;;   vicare> (set! a 1)
  ;;
  ;;when  A is  not defined  will not  fail, rather  it will  implicitly
  ;;define a binding as if we had typed:
  ;;
  ;;   vicare> (define a)
  ;;   vicare> (set! a 1)
  ;;
  ;;another example of weird code that will not fail at the REPL:
  ;;
  ;;   vicare> (let ()
  ;;             (set! a 1)
  ;;             (debug-print a))
  ;;
  ;;will just print A as if we had typed:
  ;;
  ;;   vicare> (let ()
  ;;             (define a)
  ;;             (set! a 1)
  ;;             (debug-print a))
  ;;
  ;;fabricating  the  lexical  binding  is  like  injecting  the  syntax
  ;;"(define id)".
  ;;
  ;;If neither a capturing binding  is found nor a top-level interaction
  ;;environment is set: return false.
  ;;
  (or (id->label id)
      (cond ((top-level-context)
	     => (lambda (env)
		  (let ((rib (interaction-env-rib env)))
		    (receive (lab unused-lex/loc)
			;;If  a binding  in the  interaction environment
			;;captures  ID: we  retrieve  its label.   Other
			;;wise a new binding is added to the interaction
			;;environment.
			(let ((shadowing-definition? #f))
			  (gen-define-label+lex id rib shadowing-definition?))
		      ;;FIXME (Abdulaziz Ghuloum)
		      (let ((shadowing-definition? #t))
			(extend-rib! rib id lab shadowing-definition?))
		      lab))))
	    (else #f))))

;;; --------------------------------------------------------------------

(define (id->label/or-error who form id)
  (or (id->label id)
      (%raise-unbound-error who form id)))

(define (id->r6rs-record-type-descriptor-binding who form type-name-id lexenv)
  ;;TYPE-NAME-ID is  meant to be an  identifier bound to an  R6RS record
  ;;type descriptor; retrieve  its label then its binding  in LEXENV and
  ;;return the binding descriptor.
  ;;
  ;;If TYPE-NAME-ID is unbound: raise an "unbound identifier" exception.
  ;;If  the syntactic  binding  descriptor does  not  represent an  R6RS
  ;;record type descriptor: raise a syntax violation exception.
  ;;
  (let* ((label   (id->label/or-error who form type-name-id))
	 (binding (label->syntactic-binding label lexenv)))
    (if (r6rs-record-type-descriptor-binding? binding)
	binding
      (syntax-violation who
	"identifier not bound to a record type descriptor"
	form type-name-id))))

;;; --------------------------------------------------------------------

(define-auxiliary-syntaxes r6rs-record-type vicare-struct-type)

(define-syntax (case-object-type-binding stx)
  ;;This syntax is meant to be used as follows:
  ;;
  ;;   (define-constant __who__ ...)
  ;;   (syntax-match input-stx ()
  ;;     ((_ ?type-id)
  ;;      (identifier? ?type-id)
  ;;      (case-object-type-binding __who__ input-stx ?type-id lexenv.run
  ;;        ((r6rs-record-type)
  ;;         ...)
  ;;        ((vicare-struct-type)
  ;;         ...)))
  ;;     )
  ;;
  ;;where  ?TYPE-ID  is meant  to  be  an  identifier  bound to  a  R6RS
  ;;record-type descriptor or Vicare's struct-type descriptor.
  ;;
  (sys.syntax-case stx (r6rs-record-type vicare-struct-type object-type-spec)
    ((_ (?who ?input-stx ?type-id ?lexenv)
	((r6rs-record-type)	?r6rs-body0   ?r6rs-body   ...)
	((vicare-struct-type)	?struct-body0 ?struct-body ...)
	((type-spec-type)	?spec-body0   ?spec-body   ...))
     (and (sys.identifier? (sys.syntax ?who))
	  (sys.identifier? (sys.syntax ?expr-stx))
	  (sys.identifier? (sys.syntax ?type-id))
	  (sys.identifier? (sys.syntax ?lexenv)))
     (sys.syntax
      (let* ((label    (id->label/or-error ?who ?input-stx ?type-id))
	     (binding  (label->syntactic-binding label ?lexenv)))
	(cond ((r6rs-record-type-descriptor-binding? binding)
	       ?r6rs-body0 ?r6rs-body ...)
	      ((struct-type-descriptor-binding? binding)
	       ?struct-body0 ?struct-body ...)
	      ((identifier-object-type-spec ?type-id)
	       ?spec-body0 ?spec-body ...)
	      (else
	       (syntax-violation ?who
		 "neither a struct type nor an R6RS record type nor a spec type"
		 ?input-stx ?type-id))))))
    ((_ (?who ?input-stx ?type-id ?lexenv ?binding)
	((r6rs-record-type)	?r6rs-body0   ?r6rs-body   ...)
	((vicare-struct-type)	?struct-body0 ?struct-body ...)
	((type-spec-type)	?spec-body0   ?spec-body   ...))
     (and (sys.identifier? (sys.syntax ?who))
	  (sys.identifier? (sys.syntax ?expr-stx))
	  (sys.identifier? (sys.syntax ?type-id))
	  (sys.identifier? (sys.syntax ?lexenv)))
     (sys.syntax
      (let* ((label    (id->label/or-error ?who ?input-stx ?type-id))
	     (?binding  (label->syntactic-binding label ?lexenv)))
	(cond ((r6rs-record-type-descriptor-binding? ?binding)
	       ?r6rs-body0 ?r6rs-body ...)
	      ((struct-type-descriptor-binding? ?binding)
	       ?struct-body0 ?struct-body ...)
	      ((identifier-object-type-spec ?type-id)
	       ?spec-body0 ?spec-body ...)
	      (else
	       (syntax-violation ?who
		 "neither a struct type nor an R6RS record type"
		 ?input-stx ?type-id))))))
    ))


;;So, what's an anti-mark and why is it there?
;;
;;The theory goes like this: when a macro call is encountered, the input
;;stx to the  macro transformer gets an extra  anti-mark, and the output
;;of the  transformer gets a fresh  mark.  When a mark  collides with an
;;anti-mark, they cancel one another.   Therefore, any part of the input
;;transformer that gets copied to  the output would have a mark followed
;;immediately by an  anti-mark, resulting in the same  syntax object (no
;;extra marks).  Parts of the output  that were not present in the input
;;(e.g. inserted by the macro  transformer) would have no anti-mark and,
;;therefore, the mark would stick to them.
;;
;;Every time  a mark is pushed  to an <stx>-mark* list,  a corresponding
;;'shift  is pushed  to  the  <stx>-rib* list.   Every  time  a mark  is
;;cancelled  by   an  anti-mark,  the  corresponding   shifts  are  also
;;cancelled.

;;The procedure join-wraps,  here, is used to compute the  new MARK* and
;;RIB* that  would result  when the m1*  and s1* are  added to  an stx's
;;MARK* and RIB*.
;;
;;The only tricky part here is that  e may have an anti-mark that should
;;cancel with the last mark in m1*.  So, if:
;;
;;  m1* = (mx* ... mx)
;;  m2* = (#f my* ...)
;;
;;then the resulting marks should be:
;;
;;  (mx* ... my* ...)
;;
;;since mx would cancel with the anti-mark.  The ribs would have to also
;;cancel since:
;;
;;    s1* = (sx* ... sx)
;;    s2* = (sy sy* ...)
;;
;;then the resulting ribs should be:
;;
;;    (sx* ... sy* ...)
;;
;;Notice that both SX and SY would be shift marks.
;;
;;All   this  work   is  performed   by  the   functions  ADD-MARK   and
;;%DO-MACRO-CALL.
;;

(module WRAPS-UTILITIES
  (%merge-annotated-expr*
   %append-cancel-facing)

  (define (%merge-annotated-expr* ls1 ls2)
    ;;Append LS1 and LS2 and return the result; if the car or LS2 is #f:
    ;;append LS1 and (cdr LS2).
    ;;
    ;;   (%merge-annotated-expr* '(a b c) '(d  e f))   => (a b c d e f)
    ;;   (%merge-annotated-expr* '(a b c) '(#f e f))   => (a b c e f)
    ;;
    (if (and (pair? ls1)
	     (pair? ls2)
	     (not ($car ls2)))
	(%append-cancel-facing ls1 ls2)
      (append ls1 ls2)))

  (define (%append-cancel-facing ls1 ls2)
    ;;Expect LS1 to be a proper list  of one or more elements and LS2 to
    ;;be a proper list  of one or more elements.  Append  the cdr of LS2
    ;;to LS1 and return the result:
    ;;
    ;;   (%append-cancel-facing '(1 2 3) '(4 5 6))	=> (1 2 5 6)
    ;;   (%append-cancel-facing '(1)     '(2 3 4))	=> (3 4)
    ;;   (%append-cancel-facing '(1)     '(2))		=> ()
    ;;
    ;;This function is like:
    ;;
    ;;   (append ls1 (cdr ls2))
    ;;
    ;;we just hope to be a bit more efficient.
    ;;
    (let recur ((A1 ($car ls1))
		(D1 ($cdr ls1)))
      (if (null? D1)
	  ($cdr ls2)
	(cons A1 (recur ($car D1) ($cdr D1))))))

  #| end of module: WRAPS-UTILITIES |# )

(define (same-marks? x y)
  ;;Two lists  of marks are  considered the same  if they have  the same
  ;;length and the corresponding marks on each are EQ?.
  ;;
  (or (eq? x y)
      (and (pair? x) (pair? y)
	   (eq? ($car x) ($car y))
	   (same-marks? ($cdr x) ($cdr y)))))

(define* (join-wraps {stx1.mark* list-of-marks?} {stx1.rib* ribs-and-shifts?} stx1.ae {stx2 <stx>?})
  ;;Join the given wraps with the  ones in STX2 and return the resulting
  ;;wraps; with "wraps" we mean the marks and ribs, with the addition of
  ;;the annotated  expressions for debugging purposes.   The scenario is
  ;;this:
  ;;
  ;;* A syntax object STX1 (wrapped or partially unwrapped) contains the
  ;;  syntax object STX2 (an instance of <stx>) as subexpression.
  ;;
  ;;* Whenever STX1 is fully unwrapped (for example by SYNTAX-MATCH) its
  ;;   marks  and  ribs  must   be  propagated  to  all  its  identifier
  ;;  subexpressions.
  ;;
  ;;* In practice: the  marks of STX1 must be prepended  to the marks of
  ;;  STX2, the ribs of STX1 must be prepended to the ribs of STX2.
  ;;
  ;;this is what this function does.  But: we must also handle anti-mark
  ;;annihilation and the associated removal of shifts from the ribs.
  ;;
  (import WRAPS-UTILITIES)
  (let ((stx2.mark* ($<stx>-mark* stx2))
	(stx2.rib*  ($<stx>-rib*  stx2))
	(stx2.ae*   ($<stx>-ae*   stx2)))
    ;;If the first item in stx2.mark* is an anti-mark...
    (if (and (not (null? stx1.mark*))
	     (not (null? stx2.mark*))
	     (anti-mark? ($car stx2.mark*)))
	;;...cancel mark, anti-mark, and corresponding shifts.
	(values (%append-cancel-facing stx1.mark* stx2.mark*)
		(%append-cancel-facing stx1.rib*  stx2.rib*)
		(%merge-annotated-expr* stx1.ae stx2.ae*))
      ;;..else no cancellation takes place.
      (values (append stx1.mark* stx2.mark*)
	      (append stx1.rib*  stx2.rib*)
	      (%merge-annotated-expr* stx1.ae stx2.ae*)))))

(module ADD-MARK
  (add-mark)
  (import WRAPS-UTILITIES)

  (define* (add-mark mark {rib false-or-rib?} expr ae)
    ;;Build and return a new syntax object wrapping EXPR and having MARK
    ;;pushed on its list of marks.  This function used only in 2 places:
    ;;
    ;;* It  is applied to  the input form  of a macro  transformer, with
    ;;  MARK being the anti-mark.
    ;;
    ;;* It  is applied to the  output form of a  macro transformer, with
    ;;  MARK being a proper mark.
    ;;
    ;;MARK is either the anti-mark or a new mark.
    ;;
    ;;RIB can be #f (when MARK is the anti-mark) or an instance of <rib>
    ;;(when MARK is a new mark).
    ;;
    ;;EXPR is either  the input form of a macro  transformer call or the
    ;;output  form of  a macro  transformer call;  it must  be a  syntax
    ;;object, either wrapped or unwrapped.
    ;;
    ;;AE is either #f (when MARK is  the anti-mark) or the input form of
    ;;a macro call (when MARK is a  new mark).  This argument is used to
    ;;keep track of  the transformation a form  undergoes when processed
    ;;as macro use.
    ;;
    ;;The return value  can be either a <stx> instance  or a (partially)
    ;;unwrapped syntax object.
    ;;
    (%find-meaningful-stx expr mark rib expr '() (list ae)))

  ;;NOTE The one  below was the original (modified,  but still original)
  ;;implementation of this function; it  is kept here for reference.  In
  ;;my  eternal ignorance,  I do  not understand  why the  syntax object
  ;;return value of the recursive  function is enclosed in another <stx>
  ;;with empty wraps; so I removed  such an envelope object.  It appears
  ;;that there  is no need  for this function  to return an  instance of
  ;;<stx>: the  code works fine when  the return value is  a (partially)
  ;;unwrapped syntax object.  (Marco Maggi; Tue Mar 18, 2014)
  ;;
  ;; (define* ({add-mark <stx>?} mark {rib false-or-rib?} expr ae)
  ;;   (let ((mark* '())
  ;;         (rib*  '())
  ;;         (ae*   (list ae)))
  ;;     (mkstx (%find-meaningful-stx expr mark rib expr '() '())
  ;;            mark* rib* ae*)))

  (define (%find-meaningful-stx top-expr new-mark rib expr accum-rib* ae*)
    ;;Recursively visit EXPR while EXPR is: a pair, a vector or an <stx>
    ;;with empty MARK*.  Stop the recursion  when EXPR is: an <stx> with
    ;;non-empty MARK* or a  non-compound datum (boolean, number, string,
    ;;..., struct, record,  null).  Raise an exception if EXPR  is a raw
    ;;symbol.
    ;;
    ;;When a  <stx> with non-empty  MARK* is found: perform  the action;
    ;;see below for details.
    ;;
    ;;Return a wrapped  or (partially) unwrapped syntax  object with the
    ;;mark added.
    ;;
    ;;ACCUM-RIB* is  the list of  RIB* (ribs and "shift"  symbols), from
    ;;outer to  inner, collected so  far while visiting  <stx> instances
    ;;with empty MARK*.
    ;;
    (define-syntax-rule (%recurse ?expr ?accum-rib* ?ae*)
      (%find-meaningful-stx top-expr new-mark rib ?expr ?accum-rib* ?ae*))
    (cond ((pair? expr)
	   ;;Visit the  items in the  pair.  If the visited  items equal
	   ;;the original items: keep EXPR as result.
	   (let ((A (%recurse (car expr) accum-rib* ae*))
		 (D (%recurse (cdr expr) accum-rib* ae*)))
	     (if (eq? A D)
		 expr
	       (cons A D))))

	  ((vector? expr)
	   ;;Visit all  the items in  the vector.  If the  visited items
	   ;;equal the original items: keep EXPR as result.
	   (let* ((ls1 (vector->list expr))
		  (ls2 (map (lambda (item)
			      (%recurse item accum-rib* ae*))
			 ls1)))
	     (if (for-all eq? ls1 ls2)
		 expr
	       (list->vector ls2))))

	  ((<stx>? expr)
	   (let ((expr.mark* ($<stx>-mark* expr))
		 (expr.rib*  ($<stx>-rib*  expr)))
	     (cond ((null? expr.mark*)
		    ;;EXPR  with  empty  MARK*: collect  its  RIB*  then
		    ;;recurse into its expression.
		    (%recurse ($<stx>-expr expr)
			      (append accum-rib* expr.rib*)
			      (%merge-annotated-expr* ae* ($<stx>-ae* expr))))

		   ((eq? (car expr.mark*) anti-mark)
		    ;;EXPR with non-empty MARK*  having the anti-mark as
		    ;;first mark; this means EXPR is the input form of a
		    ;;macro transformer call.
		    ;;
		    ;;Drop  both   NEW-MARK  and  the   anti-mark  (they
		    ;;annihilate each other) from the resulting MARK*.
		    ;;
		    ;;Join the collected  ACCUM-RIB* with the EXPR.RIB*;
		    ;;the first item in the resulting RIB* is associated
		    ;;to the  anti-mark (it is  a "shift" symbol)  so we
		    ;;drop it.
		    ;;
		    #;(assert (or (and (not (null? accum-rib*))
				     (eq? 'shift (car accum-rib*)))
				(and (not (null? expr.rib*))
				     (eq? 'shift (car expr.rib*)))))
		    (let* ((result.rib* (append accum-rib* expr.rib*))
			   (result.rib* (cdr result.rib*)))
		      (make-<stx> ($<stx>-expr expr) (cdr expr.mark*)
				  result.rib*
				  (%merge-annotated-expr* ae* ($<stx>-ae* expr)))))

		   (else
		    ;;EXPR with non-empty MARK*  having a proper mark as
		    ;;first  mark; this  means EXPR  is a  syntax object
		    ;;created by a macro transformer and inserted in its
		    ;;output form.
		    ;;
		    ;;Push NEW-MARK on the resulting MARK*.
		    ;;
		    ;;Join the  collected ACCUM-RIB* with  the EXPR.RIB*
		    ;;of  EXPR; push  a "shift"  on the  resulting RIB*,
		    ;;associated to NEW-MARK in MARK*.
		    ;;
		    (let* ((result.rib* (append accum-rib* expr.rib*))
			   (result.rib* (cons 'shift result.rib*))
			   (result.rib* (if rib
					    (cons rib result.rib*)
					  result.rib*)))
		      (make-<stx> ($<stx>-expr expr) (cons new-mark expr.mark*)
				  result.rib*
				  (%merge-annotated-expr* ae* ($<stx>-ae* expr))))))))

	  ((symbol? expr)
	   ;;A raw symbol is invalid.
	   (syntax-violation #f
	     "raw symbol encountered in output of macro"
	     top-expr expr))

	  (else
	   ;;If  we are  here EXPR  is a  non-compound datum  (booleans,
	   ;;numbers, strings, ..., structs, records).
	   #;(assert (non-compound-sexp? expr))
	   expr)))

  #| end of module: ADD-MARK |# )


;;;; stuff about labels, lexical variables, location gensyms

(define* (gensym-for-lexical-var seed)
  ;;Generate a unique symbol to represent the name of a lexical variable
  ;;in the core language forms.  Such  symbols have the purpose of being
  ;;unique in the core language  expressions representing a full library
  ;;or full program.
  ;;
  (if-wants-descriptive-gensyms
      (cond ((identifier? seed)
	     (gensym (string-append "lex." (symbol->string (identifier->symbol seed)))))
	    ((symbol? seed)
	     (gensym (string-append "lex." (symbol->string seed))))
	    (else
	     (assertion-violation __who__
	       "expected symbol or identifier as argument" seed)))
    (cond ((identifier? seed)
	   (gensym (symbol->string (identifier->symbol seed))))
	  ((symbol? seed)
	   (gensym (symbol->string seed)))
	  (else
	   (assertion-violation __who__
	     "expected symbol or identifier as argument" seed)))))

(define gensym-for-storage-location
  ;;Build  and return  a gensym  to be  used as  storage location  for a
  ;;global lexical variable.  The "value" slot of such gensym is used to
  ;;hold the value of the variable.
  ;;
  (if-wants-descriptive-gensyms
      (case-lambda
       (()
	(gensym "loc.anonymous"))
       ((seed)
	(cond ((identifier? seed)
	       (gensym (string-append "loc." (symbol->string (identifier->symbol seed)))))
	      ((symbol? seed)
	       (gensym (string-append "loc." (symbol->string seed))))
	      ((string? seed)
	       (gensym (string-append "loc." seed)))
	      (else
	       (gensym)))))
    ;;It is  really important to  use a  seeded gensym here,  because it
    ;;will show up in some error messages about unbound identifiers.
    (case-lambda
     (()
      (gensym))
     ((seed)
      (cond ((identifier? seed)
	     (gensym (symbol->string (identifier->symbol seed))))
	    ((symbol? seed)
	     (gensym (symbol->string seed)))
	    ((string? seed)
	     (gensym seed))
	    (else
	     (gensym)))))))

(define (gensym-for-label seed)
  ;;Every  syntactic binding  has a  label  associated to  it as  unique
  ;;identifier  in the  whole running  process; this  function generates
  ;;such labels as gensyms.
  ;;
  ;;Labels  must have  read/write  EQ?  invariance  to support  separate
  ;;compilation (when we write the expanded sexp to a file and then read
  ;;it back, the labels must not change and still be globally unique).
  ;;
  (if-wants-descriptive-gensyms
      (cond ((identifier? seed)
	     (gensym (string-append "lab." (symbol->string (identifier->symbol seed)))))
	    ((symbol? seed)
	     (gensym (string-append "lab." (symbol->string seed))))
	    ((string? seed)
	     (gensym (string-append "lab." seed)))
	    (else
	     (gensym)))
    (gensym)))

(module (gen-define-label+lex
	 gen-define-syntax-label)

  (define (gen-define-label+lex id rib shadowing-definition?)
    ;;Whenever a DEFINE syntax:
    ;;
    ;;   (define ?id ?rhs)
    ;;
    ;;is expanded  we need  to generate  for it: a  label gensym,  a lex
    ;;gensym  and a  loc gensym.   This function  returns 2  values: the
    ;;label and the lex; a loc might be generated too.
    ;;
    ;;ID  must  be an  identifier  representing  the  name of  a  DEFINE
    ;;binding.
    ;;
    ;;RIB  must be  the <RIB>  describing the  lexical contour  in which
    ;;DEFINE is present.
    ;;
    ;;SHADOWING-DEFINITION?  must  be a boolean,  true if it is  fine to
    ;;generate a binding that shadows an already existing binding:
    ;;
    ;;* When  this argument is true:  we always generate a  new label, a
    ;;  new lex and no loc.   Upon returning from this function, we have
    ;;  2 choices:
    ;;
    ;;  - Search the relevant LEXENV for the returned label and raise an
    ;;    error when a binding already exists.
    ;;
    ;;  - Create  a new binding by  pushing a new entry  on the relevant
    ;;    LEXENV.
    ;;
    ;;* When  this argument  is false:  we assume  there is  a top-level
    ;;  interaction  environment set.
    ;;
    ;;  - If  an existent binding in such top-level  env captures ID: we
    ;;    cause DEFINE  to mutate the existent binding  by returning the
    ;;    already existent label and lex.
    ;;
    ;;  -  Otherwise  we  fabricate  a  new  binding  in  the  top-level
    ;;    environment and return the new  label and lex; in this case we
    ;;    generate a loc gensym and also use it as lex gensym.
    ;;
    (if shadowing-definition?
	;;This DEFINE binding is *allowed* to shadow an existing lexical
	;;binding.
	(values (gensym-for-label id) (gensym-for-lexical-var id))
      ;;This DEFINE binding is *forbidden* to shadow an existing lexical
      ;;binding.
      (let* ((env       (top-level-context))
	     (label     (%gen-top-level-label id rib))
	     (lab.loc/lex*  (interaction-env-lab.loc/lex* env)))
	(values label
		(cond ((assq label lab.loc/lex*)
		       => cdr)
		      (else
		       (receive-and-return (loc)
			   (gensym-for-storage-location id)
			 (set-interaction-env-lab.loc/lex*! env
			   (cons (cons label loc)
				 lab.loc/lex*)))))))))

  (define (gen-define-syntax-label id rib shadowing-definition?)
    ;;Whenever a DEFINE syntax:
    ;;
    ;;   (define-syntax ?id ?rhs)
    ;;
    ;;is expanded we need to generate for it: a label gensym, and that's
    ;;all.  This function returns the label.
    ;;
    ;;ID must be an identifier  representing the name of a DEFINE-SYNTAX
    ;;binding.
    ;;
    ;;RIB  must be  the <RIB>  describing the  lexical contour  in which
    ;;DEFINE-SYNTAX is present.
    ;;
    ;;SHADOWING-DEFINITION?  must  be a boolean,  true if it is  fine to
    ;;generate a binding that shadows an already existing binding:
    ;;
    ;;* When this argument is true: we always generate a new label.
    ;;
    ;;* When this argument is false, we first check if RIB has a binding
    ;;  capturing ID: if it exists and it is not an imported binding, we
    ;;  return its  already existent label; otherwise we  generate a new
    ;;  label.
    ;;
    (if shadowing-definition?
        (gensym-for-label id)
      (%gen-top-level-label id rib)))

  (define (%gen-top-level-label id rib)
    (let ((sym   (identifier->symbol id))
	  (mark* (<stx>-mark* id))
	  (sym*  (<rib>-name* rib)))
      (cond ((and (memq sym (<rib>-name* rib))
		  (%find sym mark* sym* (<rib>-mark** rib) (<rib>-label* rib)))
	     => (lambda (label)
		  ;;If we are here RIB  contains a binding that captures
		  ;;ID and LABEL is its label.
		  ;;
		  ;;If the  LABEL is associated to  an imported binding:
		  ;;the  data structure  implementing the  symbol object
		  ;;holds informations about the  binding in an internal
		  ;;field; else such field is set to false.
		  (if (imported-label->syntactic-binding label)
		      ;;Create new label to shadow imported binding.
		      (gensym-for-label id)
		    ;;Recycle old label.
		    label)))
	    (else
	     ;;Create a new label for a new binding.
	     (gensym-for-label id)))))

  (define (%find sym mark* sym* mark** label*)
    ;;We know  that the list  of symbols SYM*  has at least  one element
    ;;equal to SYM; iterate through  SYM*, MARK** and LABEL* looking for
    ;;a  tuple having  marks equal  to MARK*  and return  the associated
    ;;label.  If such binding is not found return false.
    ;;
    (and (pair? sym*)
	 (if (and (eq? sym (car sym*))
		  (same-marks? mark* (car mark**)))
	     (car label*)
	   (%find sym mark* (cdr sym*) (cdr mark**) (cdr label*)))))

  #| end of module |# )


;;;; public interface: identifiers handling

(define (identifier? x)
  ;;Return true if X is an  identifier: a syntax object whose expression
  ;;is a symbol.
  ;;
  (and (<stx>? x)
       (let ((expr ($<stx>-expr x)))
	 (symbol? (if (annotation? expr)
		      (annotation-stripped expr)
		    expr)))))

(define (false-or-identifier? x)
  (or (not x)
      (identifier? x)))

;;; --------------------------------------------------------------------

(define* (bound-identifier=? {x identifier?} {y identifier?})
  (bound-id=? x y))

(define (bound-id=? id1 id2)
  ;;Two identifiers  are BOUND-ID=? if they  have the same name  and the
  ;;same set of marks.
  ;;
  (and (eq? (identifier->symbol id1) (identifier->symbol id2))
       (same-marks? ($<stx>-mark* id1) ($<stx>-mark* id2))))

(define* (free-identifier=? {x identifier?} {y identifier?})
  (free-id=? x y))

(define (free-id=? id1 id2)
  ;;Two identifiers are  FREE-ID=? if either both are bound  to the same
  ;;label or if both are unbound and they have the same name.
  ;;
  (let ((t1 (id->label id1))
	(t2 (id->label id2)))
    (if (or t1 t2)
	(eq? t1 t2)
      (eq? (identifier->symbol id1)
	   (identifier->symbol id2)))))

;;; --------------------------------------------------------------------

(define* (identifier-bound? {id identifier?})
  ($identifier-bound? id))

(define ($identifier-bound? id)
  (and (id->label id) #t))

(define (false-or-identifier-bound? id)
  (or (not id)
      (and (identifier? id)
	   ($identifier-bound? id))))

;;; --------------------------------------------------------------------

(define* (identifier->symbol x)
  ;;Given an identifier return its symbol expression.
  ;;
  (define (%error)
    (assertion-violation __who__
      "expected identifier as argument" x))
  (unless (<stx>? x)
    (%error))
  (let* ((expr ($<stx>-expr x))
	 (sym  (if (annotation? expr)
		   (annotation-stripped expr)
		 expr)))
    (if (symbol? sym)
	sym
      (%error))))

(define (generate-temporaries list-stx)
  (syntax-match list-stx ()
    ((?item* ...)
     (map (lambda (x)
	    (make-<stx> (let ((x (syntax->datum x)))
			  (if (or (symbol? x)
				  (string? x))
			      (gensym x)
			    (gensym 't)))
			TOP-MARK* '() '()))
       ?item*))
    (_
     (assertion-violation 'generate-temporaries
       "not a list" list-stx))))


;;;; utilities for identifiers

(define (valid-bound-ids? id*)
  ;;Given a list return #t if it  is made of identifers none of which is
  ;;BOUND-ID=? to another; else return #f.
  ;;
  ;;This function is called to validate  both list of LAMBDA formals and
  ;;list of LET binding identifiers.  The only guarantee about the input
  ;;is that it is a list.
  ;;
  (and (for-all identifier? id*)
       (distinct-bound-ids? id*)))

(define (distinct-bound-ids? id*)
  ;;Given a list of identifiers: return #t if none of the identifiers is
  ;;BOUND-ID=? to another; else return #f.
  ;;
  (or (null? id*)
      (and (not (bound-id-member? ($car id*) ($cdr id*)))
	   (distinct-bound-ids? ($cdr id*)))))

(define (duplicate-bound-formals? standard-formals-stx)
  ;;Given  a  syntax  object  representing a  list  of  UNtagged  LAMBDA
  ;;formals:  return #f  if none  of the  identifiers is  BOUND-ID=?  to
  ;;another; else return a duplicate identifier.
  ;;
  (let recur ((fmls          standard-formals-stx)
	      (collected-id* '()))
    (syntax-match fmls ()
      ;;STANDARD-FORMALS-STX is a proper list and it ends here.  Good.
      (() #f)
      ;;STANDARD-FORMALS-STX is an IMproper list and it ends here with a
      ;;rest argument.  Check it.
      (?rest
       (identifier? ?rest)
       (if (bound-id-member? ?rest collected-id*)
	   ?rest
	 #f))
      ((?id . ?rest)
       (identifier? ?id)
       (if (bound-id-member? ?id collected-id*)
	   ?id
	 (recur ?rest (cons ?id collected-id*))))
      (_
       (stx-error standard-formals-stx "invalid formals")))))

(define (bound-id-member? id id*)
  ;;Given an identifier  ID and a list of identifiers  ID*: return #t if
  ;;ID is BOUND-ID=? to one of the identifiers in ID*; else return #f.
  ;;
  (and (pair? id*)
       (or (bound-id=? id ($car id*))
	   (bound-id-member? id ($cdr id*)))))

(define* (identifier-append {ctxt identifier?} . str*)
  ;;Given  the identifier  CTXT  and a  list of  strings  or symbols  or
  ;;identifiers STR*: concatenate all the items in STR*, with the result
  ;;build and return a new identifier in the same context of CTXT.
  ;;
  ($datum->syntax ctxt
		  (string->symbol
		   (apply string-append
			  (map (lambda (x)
				 (cond ((symbol? x)
					(symbol->string x))
				       ((string? x)
					x)
				       ((identifier? x)
					(symbol->string (syntax->datum x)))
				       (else
					(assertion-violation __who__ "BUG"))))
			    str*)))))


;;;; identifiers: syntax parameters

(define current-run-lexenv
  ;;This parameter holds  a function which is meant to  return the value
  ;;of LEXENV.RUN while a macro is being expanded.
  ;;
  ;;The  default  value will  return  null,  which represents  an  empty
  ;;LEXENV; when  such value is used  with LABEL->SYNTACTIC-BINDING: the
  ;;mapping   label/binding  is   performed   only   in  the   top-level
  ;;environment.
  ;;
  ;;Another possibility we could think of is to use as default value the
  ;;function:
  ;;
  ;;   (lambda ()
  ;;     (syntax-violation 'current-run-lexenv
  ;; 	   "called outside the extent of a macro expansion"
  ;; 	   '(current-run-lexenv)))
  ;;
  ;;However there are  cases where we actually want  the returned LEXENV
  ;;to be null, for example: when evaluating the visit code of a library
  ;;just loaded in  FASL form; such visit code might  need, for example,
  ;;to access the  syntactic binding property lists, and it  would do it
  ;;outside any macro expansion.
  ;;
  (make-parameter
      (lambda () '())
    (lambda* ({obj procedure?})
      obj)))

(define* (syntax-parameter-value {id identifier?})
  (let ((label (id->label id)))
    (if label
	(let ((binding (label->syntactic-binding label ((current-run-lexenv)))))
	  (case (syntactic-binding-type binding)
	    ((local-ctv)
	     (local-compile-time-value-binding-object binding))

	    ((global-ctv)
	     (global-compile-time-value-binding-object binding))

	    (else
	     (procedure-argument-violation __who__
	       "expected identifier bound to compile-time value"
	       id))))
      (procedure-argument-violation __who__
	"unbound identifier" id))))


;;;; deconstructors and predicates for syntax objects

(module (syntax-pair?
	 syntax-vector?
	 syntax-null?)

  (define (syntax-pair? x)
    (syntax-kind? x pair?))

  (define (syntax-vector? x)
    (syntax-kind? x vector?))

  (define (syntax-null? x)
    (syntax-kind? x null?))

  (define (syntax-kind? x pred?)
    (cond ((<stx>? x)
	   (syntax-kind? (<stx>-expr x) pred?))
	  ((annotation? x)
	   (syntax-kind? (annotation-expression x) pred?))
	  (else
	   (pred? x))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define (syntax-list? x)
  ;;FIXME Should terminate on cyclic input.  (Abdulaziz Ghuloum)
  (or (syntax-null? x)
      (and (syntax-pair? x)
	   (syntax-list? (syntax-cdr x)))))

(define* (syntax-car x)
  (cond ((<stx>? x)
	 (mkstx (syntax-car ($<stx>-expr x))
		($<stx>-mark* x)
		($<stx>-rib*  x)
		($<stx>-ae*   x)))
	((annotation? x)
	 (syntax-car (annotation-expression x)))
	((pair? x)
	 ($car x))
	(else
	 (assertion-violation __who__ "not a pair" x))))

(define* (syntax-cdr x)
  (cond ((<stx>? x)
	 (mkstx (syntax-cdr ($<stx>-expr x))
		($<stx>-mark* x)
		($<stx>-rib*  x)
		($<stx>-ae*   x)))
	((annotation? x)
	 (syntax-cdr (annotation-expression x)))
	((pair? x)
	 ($cdr x))
	(else
	 (assertion-violation __who__ "not a pair" x))))

(define* (syntax->list x)
  (cond ((syntax-pair? x)
	 (cons (syntax-car x)
	       (syntax->list (syntax-cdr x))))
	((syntax-null? x)
	 '())
	(else
	 (assertion-violation __who__ "invalid argument" x))))

(define* (syntax-vector->list x)
  (cond ((<stx>? x)
	 (let ((ls     (syntax-vector->list ($<stx>-expr x)))
	       (mark*  ($<stx>-mark* x))
	       (rib*   ($<stx>-rib*  x))
	       (ae*    ($<stx>-ae*   x)))
	   (map (lambda (x)
		  (mkstx x mark* rib* ae*))
	     ls)))
	((annotation? x)
	 (syntax-vector->list (annotation-expression x)))
	((vector? x)
	 (vector->list x))
	(else
	 (assertion-violation __who__ "not a syntax vector" x))))

(define (syntax-unwrap stx)
  ;;Given a syntax object STX  decompose it and return the corresponding
  ;;S-expression holding datums and identifiers.  Take care of returning
  ;;a proper  list when the  input is a  syntax object holding  a proper
  ;;list.
  ;;
  (syntax-match stx ()
    (()
     '())
    ((?car . ?cdr)
     (cons (syntax-unwrap ?car)
	   (syntax-unwrap ?cdr)))
    (#(?item* ...)
     (list->vector (syntax-unwrap ?item*)))
    (?atom
     (identifier? ?atom)
     ?atom)
    (?atom
     (syntax->datum ?atom))))


;;;; various identifiers modules

(include "psyntax.expander.syntactic-binding-properties.scm" #t)
(include "psyntax.expander.tagged-identifiers.scm" #t)


(define-syntax syntax-match
  ;;The SYNTAX-MATCH macro is almost like SYNTAX-CASE macro.  Except that:
  ;;
  ;;*  The syntax  objects matched  are OUR  stx objects,  not the  host
  ;;  systems syntax objects (whatever they may be we don't care).
  ;;
  ;;*  The literals  are matched  against  those in  the system  library
  ;;  (psyntax system $all).  -- see scheme-stx
  ;;
  ;;* The variables  in the patters are bound to  ordinary variables not
  ;;  to special pattern variables.
  ;;
  ;;The actual matching between the input expression and the patterns is
  ;;performed  by   the  function   SYNTAX-DISPATCH;  the   patterns  in
  ;;SYNTAX-MATCH are converted to a  sexps and handed to SYNTAX-DISPATCH
  ;;along with the input expression.
  ;;
  (let ()
    (define (transformer stx)
      (syntax-case stx ()

	;;No  clauses.  Some  of  the  SYNTAX-MATCH clauses  recursively
	;;expand  to uses  of  SYNTAX-MATCH; when  no  more clauses  are
	;;available in the input  form, this SYNTAX-CASE clause matches.
	;;When this happens: we want to raise a syntax error.
	;;
	;;Notice that we  do not want to raise a  syntax error here, but
	;;in the expanded code.
	((_ ?expr (?literals ...))
	 (for-all sys.identifier? (syntax (?literals ...)))
	 (syntax (syntax-violation #f "invalid syntax" ?expr)))

	;;The next clause has a fender.
	((_ ?expr (?literals ...) (?pattern ?fender ?body) ?clause* ...)
	 (for-all sys.identifier? (syntax (?literals ...)))
	 (receive (pattern ptnvars/levels)
	     (%convert-single-pattern (syntax ?pattern) (syntax (?literals ...)))
	   (with-syntax
	       ((PATTERN                   (sys.datum->syntax (syntax here) pattern))
		(((PTNVARS . LEVELS) ...)  ptnvars/levels))
	     (syntax
	      (let ((T ?expr))
		;;If   the  input   expression   matches  the   symbolic
		;;expression PATTERN...
		(let ((ls/false (syntax-dispatch T 'PATTERN)))
		  (if (and ls/false
			   ;;...and  the pattern  variables satisfy  the
			   ;;fender...
			   (apply (lambda (PTNVARS ...) ?fender) ls/false))
		      ;;...evaluate the body  with the pattern variables
		      ;;assigned.
		      (apply (lambda (PTNVARS ...) ?body) ls/false)
		    ;;...else try to match the next clause.
		    (syntax-match T (?literals ...) ?clause* ...))))))))

	;;The next clause has NO fender.
	((_ ?expr (?literals ...) (?pattern ?body) clause* ...)
	 (for-all sys.identifier? (syntax (?literals ...)))
	 (receive (pattern ptnvars/levels)
	     (%convert-single-pattern (syntax ?pattern) (syntax (?literals ...)))
	   (with-syntax
	       ((PATTERN                   (sys.datum->syntax (syntax here) pattern))
		(((PTNVARS . LEVELS) ...)  ptnvars/levels))
	     (syntax
	      (let ((T ?expr))
		;;If   the  input   expression   matches  the   symbolic
		;;expression PATTERN...
		(let ((ls/false (syntax-dispatch T 'PATTERN)))
		  (if ls/false
		      ;;...evaluate the body  with the pattern variables
		      ;;assigned.
		      (apply (lambda (PTNVARS ...) ?body) ls/false)
		    ;;...else try to match the next clause.
		    (syntax-match T (?literals ...) clause* ...))))))))

	;;This is a true error in he use of SYNTAX-MATCH.  We still want
	;;the  expanded  code  to  raise  the  violation.   Notice  that
	;;SYNTAX-VIOLATION  is not  bound in  the expand  environment of
	;;SYNTAX-MATCH's transformer.
	;;
	(?stuff
	 (syntax (syntax-violation #f "invalid syntax" stx)))
	))

    (module (%convert-single-pattern)

      (case-define %convert-single-pattern
	;;Recursive function.  Transform the PATTERN-STX into a symbolic
	;;expression to be handed  to SYNTAX-DISPATCH.  PATTERN-STX must
	;;be  a  syntax  object  holding  the  SYNTAX-MATCH  pattern  to
	;;convert.  LITERALS must  be a syntax object holding  a list of
	;;identifiers being the literals in the PATTERN-STX.
	;;
	;;Return 2 values:
	;;
	;;1. The pattern as sexp.
	;;
	;;2.   An ordered  list of  pairs, each  representing a  pattern
	;;   variable that must be bound whenever the body associated to
	;;   the  pattern is  evaluated.  The  car of  each pair  is the
	;;   symbol  being the pattern  variable name.  The cdr  of each
	;;   pair is an exact  integer representing the nesting level of
	;;   the pattern variable.
	;;
	((pattern-stx literals)
	 (%convert-single-pattern pattern-stx literals 0 '()))

	((pattern-stx literals nesting-level pattern-vars)
	 (syntax-case pattern-stx ()

	   ;;A literal identifier is encoded as:
	   ;;
	   ;;   #(scheme-id ?identifier)
	   ;;
	   ;;the wildcard underscore identifier is encoded as:
	   ;;
	   ;;   _
	   ;;
	   ;;any other identifier will bind a variable and it is encoded
	   ;;as:
	   ;;
	   ;;   any
	   ;;
	   (?identifier
	    (sys.identifier? (syntax ?identifier))
	    (cond ((%bound-identifier-member? pattern-stx literals)
		   (values `#(scheme-id ,(sys.syntax->datum pattern-stx)) pattern-vars))
		  ((sys.free-identifier=? pattern-stx (syntax _))
		   (values '_ pattern-vars))
		  (else
		   (values 'any (cons (cons pattern-stx nesting-level)
				      pattern-vars)))))

	   ;;A  tail  pattern  with  ellipsis which  does  not  bind  a
	   ;;variable is encoded as:
	   ;;
	   ;;   #(each ?pattern)
	   ;;
	   ;;a tail pattern with ellipsis which does bind a variable is
	   ;;encoded as:
	   ;;
	   ;;   each-any
	   ;;
	   ((?pattern ?dots)
	    (%ellipsis? (syntax ?dots))
	    (receive (pattern^ pattern-vars^)
		(%convert-single-pattern (syntax ?pattern) literals
					 (+ nesting-level 1) pattern-vars)
	      (values (if (eq? pattern^ 'any)
			  'each-any
			`#(each ,pattern^))
		      pattern-vars^)))

	   ;;A non-tail pattern with ellipsis is encoded as:
	   ;;
	   ;;  #(each+ ?pattern-ellipsis (?pattern-following ...) . ?tail-pattern)
	   ;;
	   ((?pattern-x ?dots ?pattern-y ... . ?pattern-z)
	    (%ellipsis? (syntax ?dots))
	    (let*-values
		(((pattern-z pattern-vars)
		  (%convert-single-pattern (syntax ?pattern-z) literals
					   nesting-level pattern-vars))

		 ((pattern-y* pattern-vars)
		  (%convert-multi-pattern  (syntax (?pattern-y ...)) literals
					   nesting-level pattern-vars))

		 ((pattern-x pattern-vars)
		  (%convert-single-pattern (syntax ?pattern-x) literals
					   (+ nesting-level 1) pattern-vars)))
	      (values `#(each+ ,pattern-x ,(reverse pattern-y*) ,pattern-z)
		      pattern-vars)))

	   ;;A pair is encoded as pair.
	   ;;
	   ((?car . ?cdr)
	    (let*-values
		(((pattern-cdr pattern-vars)
		  (%convert-single-pattern (syntax ?cdr) literals
					   nesting-level pattern-vars))

		 ((pattern-car pattern-vars)
		  (%convert-single-pattern (syntax ?car) literals
					   nesting-level pattern-vars)))
	      (values (cons pattern-car pattern-cdr) pattern-vars)))

	   ;;Null is encoded as null.
	   ;;
	   (()
	    (values '() pattern-vars))

	   ;;A vector is encoded as:
	   ;;
	   ;;   #(vector ?datum)
	   ;;
	   (#(?item ...)
	    (receive (pattern-item* pattern-vars)
		(%convert-single-pattern (syntax (?item ...)) literals
					 nesting-level pattern-vars)
	      (values `#(vector ,pattern-item*) pattern-vars)))

	   ;;A datum is encoded as:
	   ;;
	   ;;   #(atom ?datum)
	   ;;
	   (?datum
	    (values `#(atom ,(sys.syntax->datum (syntax ?datum))) pattern-vars))
	   )))

      (define (%convert-multi-pattern pattern* literals nesting-level pattern-vars)
	;;Recursive function.
	;;
	(if (null? pattern*)
	    (values '() pattern-vars)
	  (let*-values
	      (((y pattern-vars^)
		(%convert-multi-pattern  (cdr pattern*) literals nesting-level pattern-vars))
	       ((x pattern-vars^^)
		(%convert-single-pattern (car pattern*) literals nesting-level pattern-vars^)))
	    (values (cons x y) pattern-vars^^))))

      (define (%bound-identifier-member? id list-of-ids)
	;;Return #t if  the identifier ID is  BOUND-IDENTIFIER=?  to one
	;;of the identifiers in LIST-OF-IDS.
	;;
	(and (pair? list-of-ids)
	     (or (sys.bound-identifier=? id (car list-of-ids))
		 (%bound-identifier-member? id (cdr list-of-ids)))))

      (define (%ellipsis? x)
	(and (sys.identifier? x)
	     (sys.free-identifier=? x (syntax (... ...)))))

      ;;Commented out because unused.  (Marco Maggi; Thu Apr 25, 2013)
      ;;
      ;; (define (%free-identifier-member? id1 list-of-ids)
      ;;   ;;Return #t if  the identifier ID1 is  FREE-IDENTIFIER=?  to one
      ;;   ;;of the identifiers in LIST-OF-IDS.
      ;;   ;;
      ;;   (and (exists (lambda (id2)
      ;; 		     (sys.free-identifier=? id1 id2))
      ;; 	     list-of-ids)
      ;; 	   #t))

      #| end of module: %CONVERT-SINGLE-PATTERN |# )

    transformer))


;;;; identifiers from the built-in environment

(define (bless input-stx)
  ;;Given a raw  sexp, a single syntax object, a  wrapped syntax object,
  ;;an unwrapped  syntax object or  a partly unwrapped syntax  object X:
  ;;return a syntax object representing the input, possibly X itself.
  ;;
  ;;When  X is  a sexp  or a  (partially) unwrapped  syntax object:  raw
  ;;symbols  in X  are considered  references  to bindings  in the  core
  ;;language:  they are  converted to  identifiers having  empty lexical
  ;;contexts.
  ;;
  (receive-and-return (output-stx)
      (mkstx (let recur ((input-stx input-stx))
	       (cond ((<stx>? input-stx)
		      input-stx)
		     ((pair? input-stx)
		      (cons (recur (car input-stx)) (recur (cdr input-stx))))
		     ((symbol? input-stx)
		      (scheme-stx input-stx))
		     ((vector? input-stx)
		      (list->vector (map recur (vector->list input-stx))))
		     (else
		      ;;If we are here INPUT-STX is a non-compound datum
		      ;;(boolean, number,  string, ...,  struct, record,
		      ;;null).
		      (assert (non-compound-sexp? input-stx))
		      input-stx)))
	     '()  ;mark*
	     '()  ;rib*
	     '()) ;ae*
    ;; (debug-print 'bless-input  (syntax->datum input-stx)
    ;; 		 'bless-output (syntax->datum output-stx))
    ;; (debug-print 'bless-input  input-stx
    ;; 		 'bless-output output-stx)
    (void)))

(define (trace-bless input-stx)
  (receive-and-return (output-stx)
      (bless input-stx)
    (debug-print 'bless-input  (syntax->datum input-stx)
		 'bless-output (syntax->datum output-stx))))

(define scheme-stx
  ;;Take a symbol  and if it's in the library:
  ;;
  ;;   (psyntax system $all)
  ;;
  ;;create a fresh identifier that maps  only the symbol to its label in
  ;;that library.  Symbols not in that library become fresh.
  ;;
  (let ((scheme-stx-hashtable (make-eq-hashtable)))
    (lambda (sym)
      (or (hashtable-ref scheme-stx-hashtable sym #f)
	  (let* ((subst  (library-export-subst (find-library-by-name '(psyntax system $all))))
		 (stx    (make-<stx> sym TOP-MARK* '() '()))
		 (stx    (cond ((assq sym subst)
				=> (lambda (subst.entry)
				     (let ((name  (car subst.entry))
					   (label (cdr subst.entry)))
				       (push-lexical-contour
					   (make-<rib> (list name)
						       (list TOP-MARK*)
						       (list label)
						       #f)
					 stx))))
			       (else stx))))
	    (hashtable-set! scheme-stx-hashtable sym stx)
	    stx)))))

;;Here  we define  identifier syntaxes  expanding to  the identifier  of
;;built-in tags.  We need to do all this mess rather than the simpler:
;;
;;   (define-constant <top>
;;     (scheme-stx '<top>))
;;
;;because  to use  SCHEME-STX successfully  we  need the  boot image  to
;;finish its initialisation; SCHEME-STX uses the library (psyntax system
;;$all), and this library is initialised last.
;;
(let-syntax
    ((define-tag-retriever (syntax-rules ()
			     ((_ ?tag)
			      (begin
				(define retriever
				  (let ((memoized-id #f))
				    (lambda ()
				      (or memoized-id
					  (receive-and-return (id)
					      (scheme-stx '?tag)
					    #;(assert-tag-identifier? id)
					    (set! memoized-id id))))))
				(define-syntax ?tag
				  (identifier-syntax (retriever))))))))
  (define-tag-retriever <top>)
  (define-tag-retriever <procedure>)
  #| end of let-syntax |# )


;;;; macro transformer modules

(module SPLICE-FIRST-ENVELOPE
  (make-splice-first-envelope
   splice-first-envelope?
   splice-first-envelope-form)

  (define-record splice-first-envelope
    (form))

  #| end of module |# )

(module NON-CORE-MACRO-TRANSFORMER
  (non-core-macro-transformer)
  (include "psyntax.expander.non-core-macro-transformers.scm" #t))

(module CORE-MACRO-TRANSFORMER
  (core-macro-transformer)
  (include "psyntax.expander.core-macro-transformers.scm" #t))


;;;; macro transformers helpers

(define (%expand-macro-transformer rhs-expr-stx lexenv.expand)
  ;;Given a syntax  object representing the right-hand side  of a syntax
  ;;definition      (DEFINE-SYNTAX,      LET-SYNTAX,      LETREC-SYNTAX,
  ;;DEFINE-FLUID-SYNTAX,   FLUID-LET-SYNTAX):    expand   it,   invoking
  ;;libraries as needed, and return  a core language sexp
  ;;representing transformer expression.
  ;;
  ;;Usually   the  return   value  of   this  function   is  handed   to
  ;;%EVAL-MACRO-TRANSFORMER.
  ;;
  ;;For:
  ;;
  ;;   (define-syntax ?lhs ?rhs)
  ;;
  ;;this function is called as:
  ;;
  ;;   (%expand-macro-transformer #'?rhs lexenv.expand)
  ;;
  ;;For:
  ;;
  ;;   (let-syntax ((?lhs ?rhs)) ?body0 ?body ...)
  ;;
  ;;this function is called as:
  ;;
  ;;   (%expand-macro-transformer #'?rhs lexenv.expand)
  ;;
  (let* ((rtc           (make-collector))
	 (expanded-rhs  (parametrise ((inv-collector rtc)
				      (vis-collector (lambda (x) (values))))
			  (chi-expr rhs-expr-stx lexenv.expand lexenv.expand))))
    ;;We  invoke all  the libraries  needed to  evaluate the  right-hand
    ;;side.
    (for-each
	(let ((register-visited-library (vis-collector)))
	  (lambda (lib)
	    ;;LIB is  a record  of type "library".   Here we  invoke the
	    ;;library, which means we  evaluate its run-time code.  Then
	    ;;we mark the library as visited.
	    (invoke-library lib)
	    (register-visited-library lib)))
      (rtc))
    expanded-rhs))

(define (%eval-macro-transformer expanded-expr lexenv.run)
  ;;Given a  core language sexp  representing the expression of  a macro
  ;;transformer: evaluate it  and return a proper  syntactic binding for
  ;;the resulting object.
  ;;
  ;;Usually  this   function  is   applied  to   the  return   value  of
  ;;%EXPAND-MACRO-TRANSFORMER.
  ;;
  ;;When  the RHS  of a  syntax  definition is  evaluated, the  returned
  ;;object   should  be   either  a   procedure,  an   identifier-syntax
  ;;transformer, a Vicare struct type  descriptor or an R6RS record type
  ;;descriptor.  If  the return value is  not of such type:  we raise an
  ;;assertion violation.
  ;;
  (let ((rv (parametrise ((current-run-lexenv (lambda () lexenv.run)))
	      (eval-core (expanded->core expanded-expr)))))
    (cond ((procedure? rv)
	   (make-local-macro-binding rv expanded-expr))
	  ((variable-transformer? rv)
	   (make-local-identifier-macro-binding (variable-transformer-procedure rv) expanded-expr))
	  ((struct-or-record-type-descriptor-binding? rv)
	   rv)
	  ((compile-time-value? rv)
	   (make-local-compile-time-value-binding (compile-time-value-object rv) expanded-expr))
	  ((synonym-transformer? rv)
	   (let ((id (synonym-transformer-identifier rv)))
	     (make-synonym-syntax-binding (id->label/or-error 'expander id id))))
	  (else
	   (raise
	    (condition
	     (make-assertion-violation)
	     (make-who-condition 'expand)
	     (make-message-condition "invalid return value from syntax transformer expression")
	     (make-macro-expanded-input-form-condition expanded-expr)
	     (make-irritants-condition (list rv))))))))


;;;; formals syntax validation

(define (%error-invalid-formals-syntax input-form-stx formals-stx)
  ;;Raise an error  for invalid formals of LAMBDA,  CASE-LAMBDA, LET and
  ;;similar.
  ;;
  ;;If no  invalid formals  are found:  return unspecified  values, else
  ;;raise a syntax violation.  This function  is called when it has been
  ;;already determined that the formals have something wrong.
  ;;
  ;;For a LAMBDA syntax:
  ;;
  ;;   (lambda ?formals . ?body)
  ;;
  ;;it is called as:
  ;;
  ;;   (%error-invalid-formals-syntax
  ;;      #'(lambda ?formals . ?body)
  ;;      #'?formals)
  ;;
  ;;For a LET syntax:
  ;;
  ;;   (let ((?lhs* ?rhs*) ...) . ?body)
  ;;
  ;;it is called as:
  ;;
  ;;   (%error-invalid-formals-syntax
  ;;      #'(let ((?lhs* ?rhs*) ...) . ?body)
  ;;      #'?lhs*)
  ;;
  ;;NOTE  Invalid LET-VALUES  and LET*-VALUES  formals are  processed by
  ;;this function  indirectly; LET-VALUES  and LET*-VALUES  syntaxes are
  ;;first  transformed into  CALL-WITH-VALUES syntaxes,  then it  is the
  ;;LAMBDA syntax that takes care of formals validation.
  ;;
  (define (%synner message subform)
    (syntax-violation #f message input-form-stx subform))
  (syntax-match formals-stx ()
    ((?id* ... . ?last)
     (let recur ((?id* (cond ((identifier? ?last)
			      (cons ?last ?id*))
			     ((syntax-null? ?last)
			      ?id*)
			     (else
			      (%synner "not an identifier" ?last)))))
       (cond ((null? ?id*)
	      (values))
	     ((not (identifier? (car ?id*)))
	      (%synner "not an identifier" (car ?id*)))
	     (else
	      (recur (cdr ?id*))
	      (when (bound-id-member? (car ?id*)
				      (cdr ?id*))
		(%synner "duplicate binding" (car ?id*)))))))

    (_
     (%synner "malformed binding form" formals-stx))))


;;;; pattern matching helpers

(define (convert-pattern pattern-stx literals)
  ;;This function is used both by  the transformer of the non-core macro
  ;;WITH-SYNTAX and  by the transformer  of the core  macro SYNTAX-CASE.
  ;;Transform the syntax object  PATTERN-STX, representing a SYNTAX-CASE
  ;;pattern, into a pattern in the format recognised by SYNTAX-DISPATCH.
  ;;
  ;;LITERALS is null or a  list of identifiers representing the literals
  ;;from a SYNTAX-CASE use.  Notice that the ellipsis and the underscore
  ;;identifiers cannot be literals.
  ;;
  ;;Return  2   values:  the  pattern  for   SYNTAX-DISPATCH,  an  alist
  ;;representing the pattern variables:
  ;;
  ;;* The  keys of the alist  are identifiers representing the  names of
  ;;  the pattern variables.
  ;;
  ;;*  The  values   of  the  alist  are   non-negative  exact  integers
  ;;   representing  the ellipsis  nesting  level  of the  corresponding
  ;;  pattern variable.  See SYNTAX-TRANSFORMER for details.
  ;;
  ;;The returned  pattern for  SYNTAX-DISPATCH is a  sexp
  ;;with the following format:
  ;;
  ;; P in pattern:                    |  matches:
  ;;----------------------------------+---------------------------
  ;;  ()                              |  empty list
  ;;  _                               |  anything (no binding created)
  ;;  any                             |  anything
  ;;  (p1 . p2)                       |  pair
  ;;  #(free-id <key>)                |  <key> with free-identifier=?
  ;;  each-any                        |  any proper list
  ;;  #(each p)                       |  (p*)
  ;;  #(each+ p1 (p2_1 ... p2_n) p3)  |   (p1* (p2_n ... p2_1) . p3)
  ;;  #(vector p)                     |  #(x ...) if p matches (x ...)
  ;;  #(atom <object>)                |  <object> with "equal?"
  ;;
  (define (%convert* pattern* ellipsis-nesting-level pvars.levels)
    (if (null? pattern*)
	(values '() pvars.levels)
      (receive (y pvars.levels)
	  (%convert* (cdr pattern*) ellipsis-nesting-level pvars.levels)
	(receive (x pvars.levels)
	    (%convert (car pattern*) ellipsis-nesting-level pvars.levels)
	  (values (cons x y) pvars.levels)))))

  (define (%convert p ellipsis-nesting-level pvars.levels)
    (syntax-match p ()
      (?id
       (identifier? ?id)
       (cond ((bound-id-member? ?id literals)
	      (values `#(free-id ,?id) pvars.levels))
	     ((free-id=? ?id (scheme-stx '_))
	      (values '_ pvars.levels))
	     (else
	      ;;It is a pattern variable.
	      (values 'any (cons (cons ?id ellipsis-nesting-level)
				 pvars.levels)))))

      ((p dots)
       (ellipsis? dots)
       (receive (p pvars.levels)
	   (%convert p (+ ellipsis-nesting-level 1) pvars.levels)
	 (values (if (eq? p 'any)
		     'each-any
		   `#(each ,p))
		 pvars.levels)))

      ((x dots ys ... . z)
       (ellipsis? dots)
       (receive (z pvars.levels)
	   (%convert z ellipsis-nesting-level pvars.levels)
	 (receive (ys pvars.levels)
	     (%convert* ys ellipsis-nesting-level pvars.levels)
	   (receive (x pvars.levels)
	       (%convert x (+ ellipsis-nesting-level 1) pvars.levels)
	     (values `#(each+ ,x ,(reverse ys) ,z)
		     pvars.levels)))))

      ((x . y)
       (receive (y pvars.levels)
	   (%convert y ellipsis-nesting-level pvars.levels)
	 (receive (x pvars.levels)
	     (%convert x ellipsis-nesting-level pvars.levels)
	   (values (cons x y) pvars.levels))))

      (()
       (values '() pvars.levels))

      (#(?item* ...)
       (not (<stx>? ?item*))
       (receive (item* pvars.levels)
	   (%convert ?item* ellipsis-nesting-level pvars.levels)
	 (values `#(vector ,item*) pvars.levels)))

      (?datum
       (values `#(atom ,(syntax->datum ?datum))
	       pvars.levels))))

  (%convert pattern-stx 0 '()))

(module (ellipsis? underscore?)

  (define (ellipsis? x)
    (%free-identifier-and-symbol? x '...))

  (define (underscore? x)
    (%free-identifier-and-symbol? x '_))

  (define (%free-identifier-and-symbol? x sym)
    (and (identifier? x)
	 (free-id=? x (scheme-stx sym))))

  #| end of module |# )

(define (%verify-literals literals use-stx)
  ;;Verify that  identifiers selected as literals  are: identifiers, not
  ;;ellipsisi, not usderscore.  If successful: return true, else raise a
  ;;syntax violation
  ;;
  ;;LITERALS is  a list  of literals  from SYNTAX-CASE  or SYNTAX-RULES.
  ;;USE-STX  is a  syntax  object  representing the  full  macro use  of
  ;;SYNTAX-CASE or SYNTAX-RULES:  it is used here  for descriptive error
  ;;reporting.
  ;;
  (for-each (lambda (x)
	      (when (or (not (identifier? x))
			(ellipsis? x)
			(underscore? x))
		(syntax-violation #f "invalid literal" use-stx x)))
    literals)
  #t)

(define (ellipsis-map proc ls . ls*)
  ;;This function  is used at  expand time  to generate the  portions of
  ;;macro  output  form  generated  by  templates  with  ellipsis.   See
  ;;SYNTAX-TRANSFORMER for details.
  ;;
  ;;For a syntax template:
  ;;
  ;;   (syntax ((?a ?b ...) ...))
  ;;
  ;;this function is called in the core language as:
  ;;
  ;;   ((primitive ellipsis-map) (primitive cons) ?a ?b)
  ;;
  (define-constant __who__ '...)
  (unless (list? ls)
    (assertion-violation __who__ "not a list" ls))
  ;;LS* must be a list of  sublists, each sublist having the same length
  ;;of LS.
  (unless (null? ls*)
    (let ((n (length ls)))
      (for-each
          (lambda (x)
            (unless (list? x)
              (assertion-violation __who__ "not a list" x))
            (unless (= (length x) n)
              (assertion-violation __who__ "length mismatch" ls x)))
	ls*)))
  (apply map proc ls ls*))

(module (syntax-transpose)
  ;;Mh... what  does this do?   Take BASE-ID  and NEW-ID, which  must be
  ;;FREE-IDENTIFIER=?, compute the difference  between their marks, push
  ;;such difference  on top of the  marks of OBJECT, return  the result.
  ;;What for?  (Marco Maggi; Sun May 5, 2013)
  ;;
  (define-constant __who__ 'syntax-transpose)

  (define (syntax-transpose object base-id new-id)
    (unless (identifier? base-id)
      (%synner "not an identifier" base-id))
    (unless (identifier? new-id)
      (%synner "not an identifier" new-id))
    (unless (free-identifier=? base-id new-id)
      (%synner "not the same identifier" base-id new-id))
    (receive (mark* rib* annotated-expr*)
	(diff (car ($<stx>-mark* base-id))
	      ($<stx>-mark* new-id)
	      ($<stx>-rib*  new-id)
	      ($<stx>-ae*   new-id)
	      (lambda ()
		(%synner "unmatched identifiers" base-id new-id)))
      (if (and (null? mark*)
	       (null? rib*))
	  object
	(mkstx object mark* rib* annotated-expr*))))

  (define (diff base.mark new.mark* new.rib* new.annotated-expr* error)
    (if (null? new.mark*)
	(error)
      (let ((new.mark1 (car new.mark*)))
	(if (eq? base.mark new.mark1)
	    (values '() (final new.rib*) '())
	  (receive (subst1* subst2*)
	      (split new.rib*)
	    (receive (nm* ns* nae*)
		(diff base.mark (cdr new.mark*) subst2* (cdr new.annotated-expr*) error)
	      (values (cons new.mark1 nm*)
		      (append subst1* ns*)
		      (cons (car new.annotated-expr*) nae*))))))))

  (define (split rib*)
    ;;Non-tail recursive function.  Split RIB*  and return 2 values: the
    ;;prefix of RIB* up to and  including the first shift, the suffix of
    ;;RIB* from the first shift excluded to the end.
    ;;
    (if (eq? (car rib*) 'shift)
	(values (list 'shift)
		(cdr rib*))
      (receive (rib1* rib2*)
	  (split (cdr rib*))
	(values (cons (car rib*) rib1*)
		rib2*))))

  (define (final rib*)
    ;;Non-tail recursive function.  Return the  prefix of RIB* up to and
    ;;not including the first shift.  The  returned prefix is a new list
    ;;spine sharing the cars with RIB*.
    ;;
    (if (or (null? rib*)
	    (eq? (car rib*) 'shift))
	'()
      (cons (car rib*)
	    (final (cdr rib*)))))

  (define-syntax-rule (%synner ?message ?irritant ...)
    (assertion-violation __who__ ?message ?irritant ...))

  #| end of module: SYNTAX-TRANSPOSE |# )


;;;; pattern matching

(module (syntax-dispatch)
  ;;Perform  the actual  matching between  an input  symbolic expression
  ;;being a  (wrapped, unwrapped  or partially unwrapped)  syntax object
  ;;and a  pattern symbolic expression.   If the expression  matches the
  ;;pattern return null or  a list of syntax objects to  be bound to the
  ;;pattern variables; else return false.
  ;;
  ;;The order of  syntax objects in the returned list  is established by
  ;;the pattern and it is the  same order in which the pattern variables
  ;;appear in the alist returned by CONVERT-PATTERN.
  ;;
  ;;The pattern  for SYNTAX-DISPATCH is  a symbolic expression  with the
  ;;following format:
  ;;
  ;; P in pattern:                    |  matches:
  ;;----------------------------------+---------------------------
  ;;  ()                              |  empty list
  ;;  _                               |  anything (no binding created)
  ;;  any                             |  anything
  ;;  (p1 . p2)                       |  pair
  ;;  #(free-id <key>)                |  <key> with free-identifier=?
  ;;  each-any                        |  any proper list
  ;;  #(each p)                       |  (p*)
  ;;  #(each+ p1 (p2_1 ... p2_n) p3)  |   (p1* (p2_n ... p2_1) . p3)
  ;;  #(vector p)                     |  #(x ...) if p matches (x ...)
  ;;  #(atom <object>)                |  <object> with "equal?"
  ;;
  (define (syntax-dispatch expr pattern)
    (%match expr pattern
	    '() #;mark*
	    '() #;rib*
	    '() #;annotated-expr*
	    '() #;pvar*
	    ))

  (define (%match expr pattern mark* rib* annotated-expr* pvar*)
    (cond ((not pvar*)
	   ;;No match.
	   #f)
	  ((eq? pattern '_)
	   ;;Match anything, bind nothing.
	   pvar*)
	  ((eq? pattern 'any)
	   ;;Match anything, bind a pattern variable.
	   (cons (%make-syntax-object expr mark* rib* annotated-expr*)
		 pvar*))
	  ((<stx>? expr)
	   ;;Visit the syntax object.
	   (and (not (top-marked? mark*))
		(receive (mark*^ rib*^ annotated-expr*^)
		    (join-wraps mark* rib* annotated-expr* expr)
		  (%match (<stx>-expr expr) pattern mark*^ rib*^ annotated-expr*^ pvar*))))
	  ((annotation? expr)
	   ;;Visit the ANNOTATION struct.
	   (%match (annotation-expression expr) pattern mark* rib* annotated-expr* pvar*))
	  (else
	   (%match* expr pattern mark* rib* annotated-expr* pvar*))))

  (define (%match* expr pattern mark* rib* annotated-expr* pvar*)
    (cond
     ;;End of list pattern: match the end of a list expression.
     ;;
     ((null? pattern)
      (and (null? expr)
	   pvar*))

     ;;Match a pair expression.
     ;;
     ((pair? pattern)
      (and (pair? expr)
	   (%match (car expr) (car pattern) mark* rib* annotated-expr*
		   (%match (cdr expr) (cdr pattern) mark* rib* annotated-expr* pvar*))))

     ;;Match any  proper list  expression and  bind a  pattern variable.
     ;;This happens when the original pattern symbolic expression is:
     ;;
     ;;   (?var ...)
     ;;
     ;;everything  in the  proper  list  must be  bound  to the  pattern
     ;;variable ?VAR.
     ;;
     ((eq? pattern 'each-any)
      (let ((l (%match-each-any expr mark* rib* annotated-expr*)))
	(and l (cons l pvar*))))

     (else
      ;;Here we expect the PATTERN to be a vector of the format:
      ;;
      ;;   #(?symbol ?stuff ...)
      ;;
      ;;where ?SYMBOL is a symbol.
      ;;
      (case (vector-ref pattern 0)

	;;The pattern is:
	;;
	;;   #(each ?sub-pattern)
	;;
	;;the expression  matches if it  is a  list in which  every item
	;;matches ?SUB-PATTERN.
	;;
	((each)
	 (if (null? expr)
	     (%match-empty (vector-ref pattern 1) pvar*)
	   (let ((pvar** (%match-each expr (vector-ref pattern 1)
				      mark* rib* annotated-expr*)))
	     (and pvar**
		  (%combine pvar** pvar*)))))

	;;The pattern is:
	;;
	;;   #(free-id ?literal)
	;;
	;;the  expression  matches  if  it is  an  identifier  equal  to
	;;?LITERAL according to FREE-IDENTIFIER=?.
	;;
	((free-id)
	 (and (symbol? expr)
	      (top-marked? mark*)
	      (free-id=? (%make-syntax-object expr mark* rib* annotated-expr*)
			 (vector-ref pattern 1))
	      pvar*))

	;;The pattern is:
	;;
	;;   #(scheme-id ?symbol)
	;;
	;;the  expression matches  if it  is an  identifier equal  to an
	;;identifier    having   ?SYMBOL    as    name   according    to
	;;FREE-IDENTIFIER=?.
	;;
	((scheme-id)
	 (and (symbol? expr)
	      (top-marked? mark*)
	      (free-id=? (%make-syntax-object expr mark* rib* annotated-expr*)
			 (scheme-stx (vector-ref pattern 1)))
	      pvar*))

	;;The pattern is:
	;;
	;;   #(each+ p1 (p2_1 ... p2_n) p3)
	;;
	;;which originally was:
	;;
	;;   (p1 ?ellipsis p2_1 ... p2_n . p3)
	;;
	;;the expression matches if ...
	;;
	((each+)
	 (receive (xr* y-pat pvar*)
	     (%match-each+ expr
			   (vector-ref pattern 1)
			   (vector-ref pattern 2)
			   (vector-ref pattern 3)
			   mark* rib* annotated-expr* pvar*)
	   (and pvar*
		(null? y-pat)
		(if (null? xr*)
		    (%match-empty (vector-ref pattern 1) pvar*)
		  (%combine xr* pvar*)))))

	;;The pattern is:
	;;
	;;  #(atom ?object)
	;;
	;;the  expression matches  if it  is  a single  object equal  to
	;;?OBJECT according to EQUAL?.
	;;
	((atom)
	 (and (equal? (vector-ref pattern 1)
		      (strip expr mark*))
	      pvar*))

	;;The pattern is:
	;;
	;;   #(vector ?sub-pattern)
	;;
	;;the expression matches if it is a vector whose items match the
	;;?SUB-PATTERN.
	;;
	((vector)
	 (and (vector? expr)
	      (%match (vector->list expr) (vector-ref pattern 1)
		      mark* rib* annotated-expr* pvar*)))

	(else
	 (assertion-violation 'syntax-dispatch "invalid pattern" pattern))))))

  (define (%match-each expr pattern mark* rib* annotated-expr*)
    ;;Recursive function.   The expression  matches if it  is a  list in
    ;;which  every item  matches  PATTERN.   Return null  or  a list  of
    ;;sublists, each sublist being a list of pattern variable values.
    ;;
    (cond ((pair? expr)
	   (let ((first (%match (car expr) pattern mark* rib* annotated-expr* '())))
	     (and first
		  (let ((rest (%match-each (cdr expr) pattern mark* rib* annotated-expr*)))
		    (and rest (cons first rest))))))
	  ((null? expr)
	   '())
	  ((<stx>? expr)
	   (and (not (top-marked? mark*))
		(receive (mark*^ rib*^ annotated-expr*^)
		    (join-wraps mark* rib* annotated-expr* expr)
		  (%match-each (<stx>-expr expr) pattern mark*^ rib*^ annotated-expr*^))))
	  ((annotation? expr)
	   (%match-each (annotation-expression expr) pattern mark* rib* annotated-expr*))
	  (else #f)))

  (define (%match-each+ e x-pat y-pat z-pat mark* rib* annotated-expr* pvar*)
    (let loop ((e e) (mark* mark*) (rib* rib*) (annotated-expr* annotated-expr*))
      (cond ((pair? e)
	     (receive (xr* y-pat pvar*)
		 (loop (cdr e) mark* rib* annotated-expr*)
	       (if pvar*
		   (if (null? y-pat)
		       (let ((xr (%match (car e) x-pat mark* rib* annotated-expr* '())))
			 (if xr
			     (values (cons xr xr*) y-pat pvar*)
			   (values #f #f #f)))
		     (values '()
			     (cdr y-pat)
			     (%match (car e) (car y-pat) mark* rib* annotated-expr* pvar*)))
		 (values #f #f #f))))
	    ((<stx>? e)
	     (if (top-marked? mark*)
		 (values '() y-pat (%match e z-pat mark* rib* annotated-expr* pvar*))
	       (receive (mark* rib* annotated-expr*)
		   (join-wraps mark* rib* annotated-expr* e)
		 (loop (<stx>-expr e) mark* rib* annotated-expr*))))
	    ((annotation? e)
	     (loop (annotation-expression e) mark* rib* annotated-expr*))
	    (else
	     (values '() y-pat (%match e z-pat mark* rib* annotated-expr* pvar*))))))

  (define (%match-each-any e mark* rib* annotated-expr*)
    (cond ((pair? e)
	   (let ((l (%match-each-any (cdr e) mark* rib* annotated-expr*)))
	     (and l (cons (%make-syntax-object (car e) mark* rib* annotated-expr*) l))))
	  ((null? e)
	   '())
	  ((<stx>? e)
	   (and (not (top-marked? mark*))
		(receive (mark* rib* annotated-expr*)
		    (join-wraps mark* rib* annotated-expr* e)
		  (%match-each-any (<stx>-expr e) mark* rib* annotated-expr*))))
	  ((annotation? e)
	   (%match-each-any (annotation-expression e) mark* rib* annotated-expr*))
	  (else #f)))

  (define (%match-empty p pvar*)
    (cond ((null? p)
	   pvar*)
	  ((eq? p '_)
	   pvar*)
	  ((eq? p 'any)
	   (cons '() pvar*))
	  ((pair? p)
	   (%match-empty (car p) (%match-empty (cdr p) pvar*)))
	  ((eq? p 'each-any)
	   (cons '() pvar*))
	  (else
	   (case (vector-ref p 0)
	     ((each)
	      (%match-empty (vector-ref p 1) pvar*))
	     ((each+)
	      (%match-empty (vector-ref p 1)
			    (%match-empty (reverse (vector-ref p 2))
					  (%match-empty (vector-ref p 3) pvar*))))
	     ((free-id atom)
	      pvar*)
	     ((scheme-id atom)
	      pvar*)
	     ((vector)
	      (%match-empty (vector-ref p 1) pvar*))
	     (else
	      (assertion-violation 'syntax-dispatch "invalid pattern" p))))))

  (define (%make-syntax-object stx mark* rib* annotated-expr*)
    (if (and (null? mark*)
	     (null? rib*)
	     (null? annotated-expr*))
	stx
      (mkstx stx mark* rib* annotated-expr*)))

  (define (%combine pvar** pvar*)
    (if (null? (car pvar**))
	pvar*
      (cons (map car pvar**)
	    (%combine (map cdr pvar**) pvar*))))

  #| end of module: SYNTAX-DISPATCH |# )


;;;; chi module
;;
;;The  "chi-*"  functions  are  the ones  visiting  syntax  objects  and
;;performing the expansion process.
;;
(module (chi-expr
	 chi-expr*
	 chi-body*
	 chi-internal-body
	 chi-qrhs*
	 chi-defun
	 chi-lambda-clause
	 chi-lambda-clause*)


;;;; chi procedures: syntax object type inspection

(define (expr-syntax-type expr-stx lexenv)
  ;;Determine  the syntax  type of  an expression.   EXPR-STX must  be a
  ;;syntax object representing an expression.  Return 2 values:
  ;;
  ;;1..A symbol representing the syntax type.
  ;;
  ;;2..If the syntax is a macro  application: the value of the syntactic
  ;;   binding associated to the macro identifier.  Otherwise false.
  ;;
  ;;3..If the syntax is a macro application: the identifier representing
  ;;   the macro keyword.  Otherwise false.
  ;;
  ;;The type of an expression is determined by two things:
  ;;
  ;;* The shape of the expression (identifier, pair, or datum).
  ;;
  ;;* The binding of  the identifier (for id-stx) or the  type of car of
  ;;  the pair.
  ;;
  (cond ((identifier? expr-stx)
	 (let* ((id    expr-stx)
		(label (id->label/intern id)))
	   (unless label
	     (%raise-unbound-error #f id id))
	   (let* ((binding (label->syntactic-binding label lexenv))
		  (type    (syntactic-binding-type binding)))
	     (case type
	       ((core-prim core-macro!
			   lexical global mutable
			   local-macro local-macro!
			   global-macro global-macro!
			   local-ctv global-ctv
			   macro macro! import export library $module syntax
			   displaced-lexical)
		(values type (syntactic-binding-value binding) id))
	       (($rtd)
		(values 'type-maker-reference (syntactic-binding-value binding) id))
	       (else
		(values 'other #f #f))))))

	((syntax-pair? expr-stx)
	 ;;Here we know that EXPR-STX has the format:
	 ;;
	 ;;   (?first-form ?form ...)
	 ;;
	 (let ((id (syntax-car expr-stx)))
	   (cond ((%tagged-identifier-with-dispatcher? id)
		  ;;Here we know that EXPR-STX has the format:
		  ;;
		  ;;   (?tagged-id ?form ...)
		  ;;
		  (values 'tagged-dispatching #f id))
		 ((identifier? id)
		  ;;Here we know that EXPR-STX has the format:
		  ;;
		  ;;   (?id ?form ...)
		  ;;
		  (let ((label (id->label/intern id)))
		    (unless label
		      (%raise-unbound-error #f id id))
		    (let* ((binding (label->syntactic-binding label lexenv))
			   (type    (syntactic-binding-type binding)))
		      (case type
			((core-macro
			  define define-syntax define-alias
			  define-fluid-syntax define-fluid-override
			  let-syntax letrec-syntax begin-for-syntax
			  begin set! stale-when
			  local-ctv global-ctv
			  local-macro local-macro!
			  global-macro global-macro!
			  macro import export library module)
			 (values type (syntactic-binding-value binding) id))
			(($rtd)
			 (values 'type-maker-application (syntactic-binding-value binding) id))
			(else
			 (values 'call #f #f))))))
		 (else
		  ;;Here we know that EXPR-STX has the format:
		  ;;
		  ;;   (?non-id ?form ...)
		  ;;
		  ;;where ?NON-ID can be anything but not an identifier.
		  ;;In practice the only valid syntax for this case is:
		  ;;
		  ;;   ((?first-subform ?subform ...) ?form ...)
		  ;;
		  ;;because ?NON-ID must be  an expression evaluating to
		  ;;a closure object.
		  ;;
		  (values 'call #f #f)))))

	(else
	 (let ((datum (syntax->datum expr-stx)))
	   (if (self-evaluating? datum)
	       (values 'constant datum #f)
	     (values 'other #f #f))))))


;;;; chi procedures: helpers for SPLICE-FIRST-EXPAND

;;Set to true  whenever we are expanding the first  suborm in a function
;;application.   This is  where the  syntax SPLICE-FIRST-EXPAND  must be
;;used; in every other place it must be discarded.
;;
(define expanding-application-first-subform?
  (make-parameter #f))

(define-syntax while-expanding-application-first-subform
  ;;Evaluate a body while the parameter is true.
  ;;
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (parametrise ((expanding-application-first-subform? #t))
       ?body0 ?body ...))))

(define-syntax while-not-expanding-application-first-subform
  ;;Evaluate a body while the parameter is false.
  ;;
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (parametrise ((expanding-application-first-subform? #f))
       ?body0 ?body ...))))

(define (chi-drop-splice-first-envelope-maybe expr lexenv.run lexenv.expand)
  ;;If we are expanding the first subform of an application: just return
  ;;EXPR;  otherwise if  EXPR is  a splice-first  envelope: extract  its
  ;;form, expand it and return the result.
  ;;
  (import SPLICE-FIRST-ENVELOPE)
  (if (splice-first-envelope? expr)
      (if (expanding-application-first-subform?)
	  expr
	(chi-drop-splice-first-envelope-maybe (chi-expr (splice-first-envelope-form expr) lexenv.run lexenv.expand)
					      lexenv.run lexenv.expand))
    expr))


;;;; chi procedures: macro calls

(module (chi-non-core-macro
	 chi-local-macro
	 chi-global-macro)

  (define* (chi-non-core-macro {procname symbol?} input-form-expr lexenv.run {rib false-or-rib?})
    ;;Expand an expression representing the use of a non-core macro; the
    ;;transformer function is integrated in the expander.
    ;;
    ;;PROCNAME is a symbol representing  the name of the non-core macro;
    ;;we can map  from such symbol to the transformer  function with the
    ;;module of NON-CORE-MACRO-TRANSFORMER.
    ;;
    ;;INPUT-FORM-EXPR is  the syntax object representing  the expression
    ;;to be expanded.
    ;;
    ;;LEXENV.RUN  is  the  run-time  lexical environment  in  which  the
    ;;expression must be expanded.
    ;;
    ;;RIB is false or a struct of type "<rib>".
    ;;
    (%do-macro-call (let ()
		      (import NON-CORE-MACRO-TRANSFORMER)
		      (non-core-macro-transformer procname))
		    input-form-expr lexenv.run rib))

  (define* (chi-local-macro bind-val input-form-expr lexenv.run {rib false-or-rib?})
    ;;This  function is  used  to  expand macro  uses  for macros  whose
    ;;transformer  is defined  by local  user code,  but not  identifier
    ;;syntaxes;  these are  the lexical  environment entries  with types
    ;;"local-macro" and "local-macro!".
    ;;
    ;;BIND-VAL is the binding value of  the global macro.  The format of
    ;;the bindings is:
    ;;
    ;;     (local-macro  . (?transformer . ?expanded-expr))
    ;;     (local-macro! . (?transformer . ?expanded-expr))
    ;;
    ;;and the argument BIND-VAL is:
    ;;
    ;;     (?transformer . ?expanded-expr)
    ;;
    ;;INPUT-FORM-EXPR is  the syntax object representing  the expression
    ;;to be expanded.
    ;;
    ;;LEXENV.RUN  is  the  run-time  lexical environment  in  which  the
    ;;expression must be expanded.
    ;;
    ;;RIB is false or a struct of type "<rib>".
    ;;
    (%do-macro-call (car bind-val) input-form-expr lexenv.run rib))

  (define* (chi-global-macro bind-val input-form-expr lexenv.run {rib false-or-rib?})
    ;;This  function is  used  to  expand macro  uses  for macros  whose
    ;;transformer is defined  by user code in  imported libraries; these
    ;;are the lexical environment  entries with types "global-macro" and
    ;;"global-macro!".
    ;;
    ;;BIND-VAL is the binding value of  the global macro.  The format of
    ;;the bindings is:
    ;;
    ;;     (global-macro  . (?library . ?gensym))
    ;;     (global-macro! . (?library . ?gensym))
    ;;
    ;;and the argument BIND-VAL is:
    ;;
    ;;     (?library . ?gensym)
    ;;
    ;;INPUT-FORM-EXPR is  the syntax object representing  the expression
    ;;to be expanded.
    ;;
    ;;LEXENV.RUN  is  the  run-time  lexical environment  in  which  the
    ;;expression must be expanded.
    ;;
    ;;RIB is false or a struct of type "<rib>".
    ;;
    (let ((lib (car bind-val))
	  (loc (cdr bind-val)))
      ;;If this global binding use is  the first time a binding from LIB
      ;;is used: visit the library.
      (unless (eq? lib '*interaction*)
	(visit-library lib))
      (let ((x (symbol-value loc)))
	(let ((transformer (cond ((procedure? x)
				  x)
				 ((variable-transformer? x)
				  (cdr x))
				 (else
				  (assertion-violation 'chi-global-macro
				    "Vicare: internal error: not a procedure" x)))))
	  (%do-macro-call transformer input-form-expr lexenv.run rib)))))

;;; --------------------------------------------------------------------

  (define* (%do-macro-call transformer input-form-expr lexenv.run {rib false-or-rib?})
    (import ADD-MARK)
    (define (main)
      ;;We parametrise here because we can never know which transformer,
      ;;for example, will query the syntactic binding properties.
      (parametrise ((current-run-lexenv (lambda () lexenv.run)))
	(let ((output-form-expr (transformer
				 ;;Put the anti-mark on the input form.
				 (add-mark anti-mark #f input-form-expr #f))))
	  ;;If the  transformer returns  a function:  we must  apply the
	  ;;returned function to a function acting as compile-time value
	  ;;retriever.   Such  application  must  return a  value  as  a
	  ;;transformer would do.
	  (if (procedure? output-form-expr)
	      (%return (output-form-expr %ctv-retriever))
	    (%return output-form-expr)))))

    (define (%return output-form-expr)
      ;;Check that there are no raw symbols in the value returned by the
      ;;macro transformer.
      (let recur ((x output-form-expr))
	;;Don't feed me cycles.
	(unless (<stx>? x)
	  (cond ((pair? x)
		 (recur (car x))
		 (recur (cdr x)))
		((vector? x)
		 (vector-for-each recur x))
		((symbol? x)
		 (syntax-violation #f
		   "raw symbol encountered in output of macro"
		   input-form-expr x)))))
      ;;Put a  new mark  on the  output form.   For all  the identifiers
      ;;already  present  in the  input  form:  this  new mark  will  be
      ;;annihilated  by  the  anti-mark  we put  before.   For  all  the
      ;;identifiers introduced  by the  transformer: this new  mark will
      ;;stay there.
      (add-mark (gen-mark) rib output-form-expr input-form-expr))

    (define (%ctv-retriever id)
      ;;This is  the compile-time  values retriever function.   Given an
      ;;identifier:  search an  entry in  the lexical  environment; when
      ;;found return its value, otherwise return false.
      ;;
      (unless (identifier? id)
	(assertion-violation 'rho "not an identifier" id))
      (let ((binding (label->syntactic-binding (id->label id) lexenv.run)))
	(case (syntactic-binding-type binding)
	  ;;The given identifier is bound to a local compile-time value.
	  ;;The actual object is stored in the binding itself.
	  ((local-ctv)
	   (local-compile-time-value-binding-object binding))

	  ;;The  given  identifier  is  bound to  a  compile-time  value
	  ;;imported from  a library or the  top-level environment.  The
	  ;;actual  object is  stored  in  the "value"  field  of a  loc
	  ;;gensym.
	  ((global-ctv)
	   (global-compile-time-value-binding-object binding))

	  ;;The given identifier is not bound to a compile-time value.
	  (else #f))))

    (main))

  #| end of module |# )


;;;; chi procedures: expressions

(module (chi-expr)

  (define (chi-expr expr-stx lexenv.run lexenv.expand)
    ;;Expand a single expression form.
    ;;
    (chi-drop-splice-first-envelope-maybe
     (receive (type bind-val kwd)
	 (expr-syntax-type expr-stx lexenv.run)
       (case type
	 ((core-macro)
	  (let ((transformer (let ()
			       (import CORE-MACRO-TRANSFORMER)
			       (core-macro-transformer bind-val))))
	    (transformer expr-stx lexenv.run lexenv.expand)))

	 ((global)
	  (let* ((lib (car bind-val))
		 (loc (cdr bind-val)))
	    ((inv-collector) lib)
	    (build-global-reference no-source loc)))

	 ((core-prim)
	  (let ((name bind-val))
	    (build-primref no-source name)))

	 ((call)
	  (chi-application expr-stx lexenv.run lexenv.expand))

	 ((lexical)
	  (let ((lex (lexical-var bind-val)))
	    (build-lexical-reference no-source lex)))

	 ((tagged-dispatching)
	  (chi-expr (tag-identifier-dispatch (identifier-tag kwd) expr-stx)
		    lexenv.run lexenv.expand))

	 ((global-macro global-macro!)
	  (let ((exp-e (while-not-expanding-application-first-subform
			(chi-global-macro bind-val expr-stx lexenv.run #f))))
	    (chi-expr exp-e lexenv.run lexenv.expand)))

	 ((local-macro local-macro!)
	  ;;Here  we expand  uses  of macros  that are  local  in a  non
	  ;;top-level region.
	  ;;
	  (let ((exp-e (while-not-expanding-application-first-subform
			(chi-local-macro bind-val expr-stx lexenv.run #f))))
	    (chi-expr exp-e lexenv.run lexenv.expand)))

	 ((macro macro!)
	  ;;Here we expand the use of a non-core macro.  Such macros are
	  ;;integrated in the expander.
	  ;;
	  (let ((exp-e (while-not-expanding-application-first-subform
			(chi-non-core-macro bind-val expr-stx lexenv.run #f))))
	    (chi-expr exp-e lexenv.run lexenv.expand)))

	 ((constant)
	  (let ((datum bind-val))
	    (build-data no-source datum)))

	 ((set!)
	  (chi-set! expr-stx lexenv.run lexenv.expand))

	 ((begin)
	  ;;Here we  expand the use of  the BEGIN core macro.   First we
	  ;;check with SYNTAX-MATCH that the  syntax is correct, then we
	  ;;build the expanded language expression.
	  ;;
	  (syntax-match expr-stx ()
	    ((_ x x* ...)
	     (build-sequence no-source
	       (while-not-expanding-application-first-subform
		(chi-expr* (cons x x*) lexenv.run lexenv.expand))))))

	 ((stale-when)
	  ;;STALE-WHEN  acts  like  BEGIN,  but  in  addition  causes  an
	  ;;expression  to  be  registered   in  the  current  stale-when
	  ;;collector.   When such  expression  evaluates  to false:  the
	  ;;compiled library is  stale with respect to  some source file.
	  ;;See for example the INCLUDE syntax.
	  (syntax-match expr-stx ()
	    ((_ ?guard ?x ?x* ...)
	     (begin
	       (handle-stale-when ?guard lexenv.expand)
	       (build-sequence no-source
		 (while-not-expanding-application-first-subform
		  (chi-expr* (cons ?x ?x*) lexenv.run lexenv.expand)))))))

	 ((let-syntax letrec-syntax)
	  (syntax-match expr-stx ()
	    ((_ ((xlhs* xrhs*) ...) xbody xbody* ...)
	     (unless (valid-bound-ids? xlhs*)
	       (stx-error expr-stx "invalid identifiers"))
	     (let* ((xlab* (map gensym-for-label xlhs*))
		    (xrib  (make-filled-rib xlhs* xlab*))
		    (xb*   (map (lambda (x)
				  (let ((in-form (if (eq? type 'let-syntax)
						     x
						   (push-lexical-contour xrib x))))
				    (with-exception-handler
					(lambda (E)
					  (raise
					   (condition E (make-macro-input-form-condition in-form))))
				      (lambda ()
					(%eval-macro-transformer
					 (%expand-macro-transformer in-form lexenv.expand)
					 lexenv.run)))))
			     xrhs*)))
	       (build-sequence no-source
		 (while-not-expanding-application-first-subform
		  (chi-expr* (map (lambda (x)
				    (push-lexical-contour xrib x))
			       (cons xbody xbody*))
			     (append (map cons xlab* xb*) lexenv.run)
			     (append (map cons xlab* xb*) lexenv.expand))))))))

	 ((displaced-lexical)
	  (stx-error expr-stx "identifier out of context"))

	 ((syntax)
	  (stx-error expr-stx "reference to pattern variable outside a syntax form"))

	 ((define define-syntax define-fluid-syntax define-fluid-override define-alias module import library)
	  (stx-error expr-stx (string-append
			       (case type
				 ((define)                 "a definition")
				 ((define-syntax)          "a define-syntax")
				 ((define-fluid-syntax)    "a define-fluid-syntax")
				 ((define-fluid-override)  "a define-fluid-override")
				 ((define-alias)           "a define-alias")
				 ((module)                 "a module definition")
				 ((library)                "a library definition")
				 ((import)                 "an import declaration")
				 ((export)                 "an export declaration")
				 (else                     "a non-expression"))
			       " was found where an expression was expected")))

	 ((mutable)
	  ;;Here we  expand an  identifier in reference  position, whose
	  ;;binding is a mutable variable.
	  ;;
	  (if (and (pair? bind-val)
		   (let ((lib (car bind-val)))
		     (eq? lib '*interaction*)))
	      (let ((loc (cdr bind-val)))
		(build-global-reference no-source loc))
	    (stx-error expr-stx "attempt to reference an unexportable variable")))

	 ((type-maker-reference)
	  ;;Here we  expand an  identifier in reference  position, whose
	  ;;binding is a  struct or record type name.  The  result is an
	  ;;expression that evaluates to the struct or record maker.
	  ;;
	  (%process-type-maker-reference expr-stx bind-val lexenv.run lexenv.expand))

	 ((type-maker-application)
	  ;;Here  we expand  a form  whose  car is  an identifier  whose
	  ;;binding is a  struct or record type name.  The  result is an
	  ;;expression that  evaluates to the application  of the struct
	  ;;or record maker.
	  ;;
	  (%process-type-maker-application expr-stx bind-val lexenv.run lexenv.expand))

	 (else
	  ;;(assertion-violation 'chi-expr "invalid type " type (strip expr-stx '()))
	  (stx-error expr-stx "invalid expression"))))
     lexenv.run lexenv.expand))

  (define (chi-application expr lexenv.run lexenv.expand)
    ;;Expand a function application form.   This is called when EXPR has
    ;;the format:
    ;;
    ;;   (?rator ?rand ...)
    ;;
    ;;and ?RATOR is a pair or a non-macro identifier.  For example it is
    ;;called when EXPR is:
    ;;
    ;;   (((?rator ?rand1 ...) ?rand2 ...) ?rand3 ...)
    ;;
    (define (%build-core-expression rator rands)
      (build-application (syntax-annotation expr)
	rator
	(while-not-expanding-application-first-subform
	 (chi-expr* rands lexenv.run lexenv.expand))))
    (syntax-match expr ()
      ((?rator ?rands* ...)
       (if (not (syntax-pair? ?rator))
       	   ;;This  is a  common function  application: ?RATOR  is not  a
       	   ;;syntax  keyword.  Let's  make  sure that  we expand  ?RATOR
       	   ;;first.
       	   (let ((rator (chi-expr ?rator lexenv.run lexenv.expand)))
	     (%build-core-expression rator ?rands*))
	 ;;This is a function application with the format:
	 ;;
	 ;;   ((?int-rator ?int-rand ...) ?rand ...)
	 ;;
	 ;;we  expand  it considering  the  case  of the  first  subform
	 ;;expanding to a SPLICE-FIRST-EXPAND form.
	 (let ((exp-rator (while-expanding-application-first-subform
			   (chi-expr ?rator lexenv.run lexenv.expand))))
	   (import SPLICE-FIRST-ENVELOPE)
	   (if (splice-first-envelope? exp-rator)
	       (syntax-match (splice-first-envelope-form exp-rator) ()
		 ((?int-rator ?int-rands* ...)
		  (chi-expr (cons ?int-rator (append ?int-rands* ?rands*))
			    lexenv.run lexenv.expand))
		 (_
		  (stx-error exp-rator
			     "expected list as argument of splice-first-expand"
			     'splice-first-expand)))
	     (%build-core-expression exp-rator ?rands*)))))
      ))

  (define (chi-set! expr-stx lexenv.run lexenv.expand)
    (syntax-match expr-stx ()
      ((_ x v)
       (identifier? x)
       (receive (type bind-val kwd)
	   (expr-syntax-type x lexenv.run)
	 (case type
	   ((lexical)
	    (set-lexical-mutable! bind-val)
	    (build-lexical-assignment no-source
	      (lexical-var bind-val)
	      (chi-expr v lexenv.run lexenv.expand)))
	   ((core-prim)
	    (stx-error expr-stx "cannot modify imported core primitive"))

	   ((global)
	    (stx-error expr-stx "attempt to modify an immutable binding"))

	   ((global-macro!)
	    (chi-expr (chi-global-macro bind-val expr-stx lexenv.run #f) lexenv.run lexenv.expand))

	   ((local-macro!)
	    (chi-expr (chi-local-macro bind-val expr-stx lexenv.run #f) lexenv.run lexenv.expand))

	   ((mutable)
	    (if (and (pair? bind-val)
		     (let ((lib (car bind-val)))
		       (eq? lib '*interaction*)))
		(let ((loc (cdr bind-val)))
		  (build-global-assignment no-source
		    loc (chi-expr v lexenv.run lexenv.expand)))
	      (stx-error expr-stx "attempt to modify an unexportable variable")))

	   (else
	    (stx-error expr-stx)))))))

  (define (%process-type-maker-reference expr-stx bind-val lexenv.run lexenv.expand)
    ;;BIND-VAL is the binding value of an R6RS record-type:
    ;;
    ;;   (?rtd-id ?rcd-id)
    ;;   (?rtd-id ?rcd-id . ?r6rs-record-type-spec)
    ;;
    ;;or the binding value of a Vicare struct type:
    ;;
    ;;   #<struct-type-descriptor>
    ;;
    (cond ((r6rs-record-type-descriptor-bindval? bind-val)
	   ;;The binding is for an R6RS record type.
	   (let ((rcd-id (r6rs-record-type-descriptor-bindval-rcd bind-val)))
	     (chi-expr (bless
			`(record-constructor ,rcd-id))
		       lexenv.run lexenv.expand)))

	  ((struct-type-descriptor-bindval? bind-val)
	   ;;The binding is for a Vicare struct type.
	   (let ((field-name* (struct-type-field-names
			       (struct-type-descriptor-bindval-std bind-val))))
	     (chi-expr (bless
			`(lambda ,field-name*
			   ($struct (quote ,bind-val) . ,field-name*)))
		       lexenv.run lexenv.expand)))

	  (else
	   (stx-error expr-stx "invalid binding for identifier"))))

  (define (%process-type-maker-application expr-stx bind-val lexenv.run lexenv.expand)
    ;;BIND-VAL is the binding value of an R6RS record-type:
    ;;
    ;;   (?rtd-id ?rcd-id)
    ;;   (?rtd-id ?rcd-id . ?r6rs-record-type-spec)
    ;;
    ;;or the binding value of a Vicare struct type:
    ;;
    ;;   #<struct-type-descriptor>
    ;;
    (cond ((r6rs-record-type-descriptor-bindval? bind-val)
	   ;;The binding is for an R6RS record type.
	   (syntax-match expr-stx ()
	     ((?rtd ?arg* ...)
	      (let ((rcd-id (r6rs-record-type-descriptor-bindval-rcd bind-val)))
		(chi-expr (bless
			   `((record-constructor ,rcd-id) . ,?arg*))
			  lexenv.run lexenv.expand)))
	     ))

	  ((struct-type-descriptor-bindval? bind-val)
	   ;;The binding is for a Vicare struct type.
	   (syntax-match expr-stx ()
	     ((?rtd ?arg* ...)
	      (let ((field-name* (struct-type-field-names
				  (struct-type-descriptor-bindval-std bind-val))))
		(chi-expr (bless
			   `((lambda ,field-name*
			       ($struct (quote ,bind-val) . ,field-name*))
			     . ,?arg*))
			  lexenv.run lexenv.expand)))
	     ))

	  (else
	   (stx-error expr-stx "invalid binding for identifier"))))

  #| end of module |# )

(define (chi-expr* expr* lexenv.run lexenv.expand)
  ;;Recursive function.  Expand the expressions in EXPR* left to right.
  ;;
  (if (null? expr*)
      '()
    ;;ORDER MATTERS!!!  Make sure  that first  we do  the car,  then the
    ;;rest.
    (let ((expr0 (chi-expr (car expr*) lexenv.run lexenv.expand)))
      (cons expr0
	    (chi-expr* (cdr expr*) lexenv.run lexenv.expand)))))


;;;; chi procedures: definitions and lambda clauses

(define (chi-lambda-clause input-form-stx formals-stx body-form-stx* lexenv.run lexenv.expand)
  ;;Expand  a LAMBDA  or CASE-LAMBDA  clause  body, return  2 values:  a
  ;;proper or improper list of  lex gensyms representing the formals; an
  ;;expanded language expression representing the body of the clause.
  ;;
  ;;A LAMBDA clause defines a lexical  contour, so: we build a new <RIB>
  ;;for it,  initialised with  the id/label  associations of  the LAMBDA
  ;;arguments; we push new lexical bindings on LEXENV.RUN.
  ;;
  ;;NOTE  The expander  for the  internal body  will create  yet another
  ;;lexical contour to hold the body's internal definitions.
  ;;
  (while-not-expanding-application-first-subform
   ;;Here  we support  both the  R6RS  standard LAMBDA  formals and  the
   ;;extended tagged formals, with or without rest argument.
   (receive (standard-formals-stx signature)
       (parse-tagged-callable-spec-syntax formals-stx input-form-stx)
     (syntax-match standard-formals-stx ()
       ;;Without rest argument.
       ((?arg* ...)
	(let ((lex* (map gensym-for-lexical-var ?arg*))
	      (lab* (map gensym-for-label       ?arg*)))
	  (define body-form-stx*^
	    (push-lexical-contour
		(make-filled-rib ?arg* lab*)
	      body-form-stx*))
	  (define lexenv.run^
	    (add-lexical-bindings lab* lex* lexenv.run))
	  ;;Here we know that the formals  signature is a proper list of
	  ;;tag identifiers with the same structure of FORMALS-STX.
	  (map set-label-tag!
	    lab* (callable-signature-formals-tags signature))
	  (values lex* (chi-internal-body body-form-stx*^ lexenv.run^ lexenv.expand))))
       ;;With rest argument.
       ((?arg* ... . ?rest-arg)
	(let ((lex*		(map gensym-for-lexical-var ?arg*))
	      (lab*		(map gensym-for-label       ?arg*))
	      (rest-lex	(gensym-for-lexical-var ?rest-arg))
	      (rest-lab	(gensym-for-label       ?rest-arg)))
	  (define body-form-stx*^
	    (push-lexical-contour
		(make-filled-rib (cons ?rest-arg ?arg*)
				 (cons rest-lab  lab*))
	      body-form-stx*))
	  (define lexenv.run^
	    (add-lexical-bindings (cons rest-lab lab*)
				  (cons rest-lex lex*)
				  lexenv.run))
	  ;;Here we know that the  formals signature is an improper list
	  ;;with the same structure of FORMALS-STX.
	  (let loop ((lab* lab*)
		     (tags (callable-signature-formals-tags signature)))
	    (if (pair? tags)
		(begin
		  (set-label-tag! (car lab*) (car tags))
		  (loop (cdr lab*) (cdr tags)))
	      (set-label-tag! rest-lab tags)))
	  (values (append lex* rest-lex) ;yes, this builds an improper list
		  (chi-internal-body body-form-stx*^ lexenv.run^ lexenv.expand))))
       (_
	(stx-error formals-stx "invalid syntax"))))))

(define (chi-lambda-clause* input-form-stx formals-stx* body-form-stx** lexenv.run lexenv.expand)
  ;;Expand all the clauses of a CASE-LAMBDA syntax, return 2 values:
  ;;
  ;;1. A list of subslist, each  sublist being a proper or improper list
  ;;   of lex gensyms representing the formals.
  ;;
  ;;2.  A list  of expanded language expressions,  each representing the
  ;;   expanded body of a clause.
  ;;
  (if (null? formals-stx*)
      (values '() '())
    (receive (formals-lex body-core)
	(chi-lambda-clause input-form-stx (car formals-stx*) (car body-form-stx**) lexenv.run lexenv.expand)
      (receive (formals-lex* body-core*)
	  (chi-lambda-clause* input-form-stx (cdr formals-stx*) (cdr body-form-stx**) lexenv.run lexenv.expand)
	(values (cons formals-lex formals-lex*)
		(cons body-core   body-core*))))))

(define (chi-defun input-form-stx lexenv.run lexenv.expand)
  ;;Expand a syntax object representing a  DEFINE syntax for the case of
  ;;function  definition.    Return  an  expanded   language  expression
  ;;representing the expanded definition.
  ;;
  ;;NOTE  This  function assumes  the  INPUT-FORM-STX  has already  been
  ;;parsed, and the  binding for ?CTXT has already been  added to LEXENV
  ;;by the caller.
  ;;
  (define (%expand ctxt-id formals-stx body-stx)
    (receive (formals-stx^ body-stx^)
	(chi-lambda-clause input-form-stx formals-stx body-stx lexenv.run lexenv.expand)
      (build-lambda (syntax-annotation ctxt-id) formals-stx^ body-stx^)))
  (syntax-match input-form-stx (brace)
    ((_ ((brace ?ctxt ?tag0 ?tag* ...) . ?fmls) . ?body-form*)
     (%expand ?ctxt (bless
		     `((brace _ ,?tag0 . ,?tag*) . ,?fmls))
	      ?body-form*))
    ((_ (?ctxt . ?fmls) . ?body-form*)
     (%expand ?ctxt ?fmls ?body-form*))
    ))


;;;; chi procedures: lexical bindings qualified right-hand sides
;;
;;A "qualified  right-hand side  expression" is  a pair  whose car  is a
;;symbol specifying the type of the expression and whose cdr is a syntax
;;object  representing  the  right-hand  side expression  of  a  lexical
;;binding definition.
;;
;;For  example,  when  CHI-BODY*  expands   a  body  that  allows  mixed
;;definitions and expressions:
;;
;;   (define (fun)
;;     1)
;;   (define var (+ 3 4))
;;   (display 5)
;;
;;all the forms are parsed and the following QRHS compounds are created:
;;
;;   (defun    . #'(define (fun) 1))
;;   (expr     . #'(+ 3 4))
;;   (top-expr . #'(display 5))
;;
;;The possible types are:
;;
;;DEFUN -
;;   For a function variable definition.  A syntax like:
;;
;;      (define (?id . ?formals) ?body ...)
;;
;;EXPR -
;;  For an non-function variable definition.  A syntax like:
;;
;;      (define ?id)
;;      (define ?id ?val)
;;
;;TOP-EXPR -
;;  For an  expression that is  not a  definition; this QRHS  is created
;;  only when mixed  definitions and expressions are  allowed.  A syntax
;;  like:
;;
;;     ?expr
;;
;;  in this case the caller implicitly handles such expression as:
;;
;;     (define dummy ?expr)
;;
;;It is responsibility  of the caller to create  the appropriate lexical
;;binding to represent the DEFINE syntax.
;;

(define (chi-qrhs qrhs lexenv.run lexenv.expand)
  ;;Expand a qualified right-hand side expression and return an expanded
  ;;language expression representing the result.
  ;;
  (case (car qrhs)
    ((defun)
     (chi-defun (cdr qrhs) lexenv.run lexenv.expand))

    ((expr)
     (let ((expr (cdr qrhs)))
       (chi-expr expr lexenv.run lexenv.expand)))

    ((top-expr)
     (let ((expr (cdr qrhs)))
       (build-sequence no-source
	 (list (chi-expr expr lexenv.run lexenv.expand)
	       (build-void)))))

    (else
     (assertion-violation 'chi-qrhs "BUG: invalid qrhs" qrhs))))

(define (chi-qrhs* qrhs* lexenv.run lexenv.expand)
  ;;Expand   the  qualified   right-hand  side   expressions  in   QRHS*,
  ;;left-to-right.
  ;;
  (let loop ((ls qrhs*))
    ;; chi-qrhs in order
    (if (null? ls)
	'()
      (let ((a (chi-qrhs (car ls) lexenv.run lexenv.expand)))
	(cons a (loop (cdr ls)))))))


;;;; chi procedures: internal body

(define (chi-internal-body body-form-stx* lexenv.run lexenv.expand)
  ;;This function is used to expand the internal bodies:
  ;;
  ;;*  The LET-like  syntaxes are  converted to  LETREC* syntaxes:  this
  ;;  function processes the internal bodies of LETREC*.
  ;;
  ;;* The  LAMBDA syntaxes  are processed  as CASE-LAMBDA  clauses: this
  ;;  function processes the internal body of CASE-LAMBDA clauses.
  ;;
  ;;Return  an expanded  language symbolic  expression representing  the
  ;;expanded body.
  ;;
  ;;An internal body must satisfy the following constraints:
  ;;
  ;;* There must be at least one trailing expression, otherwise an error
  ;;  is raised.
  ;;
  ;;*  Mixed  definitions  and   expressions  are  forbidden.   All  the
  ;;  definitions must come first and the expressions last.
  ;;
  ;;* It is impossible to export  an internal binding from the enclosing
  ;;  library.   The EXPORT  syntaxes present in  the internal  body are
  ;;  discarded.
  ;;
  ;;An internal body having internal definitions:
  ;;
  ;;   (define ?id ?rhs)
  ;;   ...
  ;;   ?trailing-expr
  ;;   ...
  ;;
  ;;is equivalent to:
  ;;
  ;;   (letrec* ((?id ?rhs) ...)
  ;;     ?trailing-expr)
  ;;
  ;;so we create a <RIB> to describe the lexical contour of the implicit
  ;;LETREC* and push it on the BODY-FORM-STX*.
  ;;
  (while-not-expanding-application-first-subform
   (let ((rib (make-empty-rib)))
     (receive (trailing-expr-stx*^
	       lexenv.run^ lexenv.expand^
	       lex*^ qrhs*^
	       trailing-mod-expr-stx**^
	       unused-kwd*^ unused-export-spec*^)
	 (let ((lex*                              '())
	       (qrhs*                             '())
	       (mod**                             '())
	       (kwd*                              '())
	       (export-spec*                      '())
	       (mix-definitions-and-expressions?  #f)
	       (shadowing-definitions?            #t))
	   (chi-body* (map (lambda (x)
			     (push-lexical-contour rib x))
			(syntax->list body-form-stx*))
		      lexenv.run lexenv.expand
		      lex* qrhs* mod** kwd* export-spec* rib
		      mix-definitions-and-expressions?
		      shadowing-definitions?))
       (when (null? trailing-expr-stx*^)
	 (stx-error body-form-stx* "no expression in body"))
       (let* ((all-expr-core*  (chi-expr* (append (reverse-and-append trailing-mod-expr-stx**^)
						  trailing-expr-stx*^)
					  lexenv.run^ lexenv.expand^))
	      (rhs-core*       (chi-qrhs* qrhs*^ lexenv.run^ lexenv.expand^)))
	 (build-letrec* no-source
	   (reverse lex*^)
	   (reverse rhs-core*)
	   (build-sequence no-source
	     all-expr-core*)))))))


;;;; chi procedures: body

(module (chi-body*)
  ;;The recursive function CHI-BODY* expands  the forms of a body.  Here
  ;;we  expand:
  ;;
  ;;* User-defined the macro definitions and uses.
  ;;
  ;;* Non-core macro uses.
  ;;
  ;;*  Basic  language  syntaxes: LIBRARY,  MODULE,  BEGIN,  STALE-WHEN,
  ;;  IMPORT, EXPORT.
  ;;
  ;;but  expand neither  the core  macros nor  the variable  definitions
  ;;(DEFINE forms)  and trailing  expressions: the  variable definitions
  ;;are accumulated  in the  argument and return  value QRHS*  for later
  ;;expansion; the trailing expressions  are accumulated in the argument
  ;;and return value BODY-FORM-STX* for later expansion.
  ;;
  ;;Here is a description of the arguments.
  ;;
  ;;BODY-FORM-STX* must be null or a list of syntax objects representing
  ;;the forms.
  ;;
  ;;LEXENV.RUN and LEXENV.EXPAND must  be lists representing the current
  ;;lexical environment for run and expand times.
  ;;
  ;;LEX* must be a list of gensyms and QRHS* must be a list of qualified
  ;;right-hand sides representing right-hand side expressions for DEFINE
  ;;syntax uses; they  are meant to be processed together  item by item.
  ;;Whenever the QRHS expressions are  expanded: a core language binding
  ;;will be  created with a LEX  gensym associate to a  QRHS expression.
  ;;The QRHS have the formats:
  ;;
  ;; (defun . ?full-form)
  ;;		Represents  a  DEFINE  form which  defines  a  function.
  ;;		?FULL-FORM is  the syntax  object representing  the full
  ;;		DEFINE form.
  ;;
  ;; (expr  . ?val)
  ;;		Represents a  DEFINE form  which defines  a non-function
  ;;		variable.   ?VAL is  a  syntax  object representing  the
  ;;		variable's value.
  ;;
  ;; (top-expr . ?body-form-stx)
  ;;		Represents   a  dummy   DEFINE   form  introduced   when
  ;;		processing an expression in a R6RS program.
  ;;
  ;;About the MOD** argument.  We  know that module definitions have the
  ;;syntax:
  ;;
  ;;   (module (?export-id ...) ?definition ... ?expression ...)
  ;;
  ;;and  the trailing  ?EXPRESSION  forms must  be  evaluated after  the
  ;;right-hand sides  of the DEFINE syntaxes  of the module but  also of
  ;;the  enclosing  lexical context.   So  when  expanding a  MODULE  we
  ;;accumulate such expression syntax objects in the MOD** argument as:
  ;;
  ;;   MOD** == ((?expression ...) ...)
  ;;
  ;;KWD* is a  list of identifiers representing the  syntaxes defined in
  ;;this body.  It is used to test for duplicate definitions.
  ;;
  ;;EXPORT-SPEC* is  null or a  list of syntax objects  representing the
  ;;export specifications from this body.  It is to be processed later.
  ;;
  ;;RIB is the current lexical environment's rib.
  ;;
  ;;MIX? is interpreted  as boolean.  When false:  the expansion process
  ;;visits all  the definition forms  and stops at the  first expression
  ;;form; the expression  forms are returned to the  caller.  When true:
  ;;the  expansion  process visits  all  the  definition and  expression
  ;;forms, accepting  a mixed  sequence of them;  an expression  form is
  ;;handled as a dummy definition form.
  ;;
  ;;When SD? is false this body  is allowed to redefine bindings created
  ;;by DEFINE;  this happens when expanding  for the Scheme REPL  in the
  ;;interaction environment.  When SD? is true: attempting to redefine a
  ;;DEFINE binding will raise an exception.
  ;;
  (define (chi-body* body-form-stx* lexenv.run lexenv.expand lex* qrhs* mod** kwd* export-spec* rib mix? sd?)
    (while-not-expanding-application-first-subform
     (if (null? body-form-stx*)
	 (values body-form-stx* lexenv.run lexenv.expand lex* qrhs* mod** kwd* export-spec*)
       (let ((body-form-stx (car body-form-stx*)))
	 (receive (type bind-val kwd)
	     (expr-syntax-type body-form-stx lexenv.run)
	   (let ((kwd* (if (identifier? kwd)
			   (cons kwd kwd*)
			 kwd*)))
	     (case type

	       ((define)
		;;The body form is a core language DEFINE macro use:
		;;
		;;   (define ?id ?rhs)
		;;
		;;We create a new lexical binding:
		;;
		;;* We  generate a  label gensym uniquely  associated to
		;;  the binding and a lex  gensym as name of the binding
		;;  in the expanded code.
		;;
		;;* We register the association id/label in the rib.
		;;
		;;*  We push  an entry  on LEXENV.RUN  to represent  the
		;;  association label/lex.
		;;
		;;* Finally we recurse on the rest of the body.
		;;
		;;Notice that:
		;;
		;;* The ?RHS will be expanded later.
		;;
		;;*  If the  binding is  at the  top-level of  a program
		;;  body:  we need a loc  gensym to store the  result of
		;;  evaluating ?RHS.  This loc will be generated later.
		;;
		;;*  If  the binding  is  at  the  top-level of  a  REPL
		;;  expression: we need a loc gensym to store the result
		;;   of  evaluating  ?RHS.   In this  case  the  loc  is
		;;  generated here by GEN-DEFINE-LABEL+LEX.
		;;
		(receive (id id-tag signature qrhs)
		    (%parse-define body-form-stx)
		  (when (bound-id-member? id kwd*)
		    (stx-error body-form-stx "cannot redefine keyword"))
		  (receive (lab lex)
		      (gen-define-label+lex id rib sd?)
		    (extend-rib! rib id lab sd?)
		    (set-label-tag! lab id-tag)
		    ;;If  the binding  is not  a function:  SIGNATURE is
		    ;;false.
		    (when signature
		      (set-label-callable-signature! lab signature))
		    (chi-body* (cdr body-form-stx*)
			       (add-lexical-binding lab lex lexenv.run) lexenv.expand
			       (cons lex lex*) (cons qrhs qrhs*)
			       mod** kwd* export-spec* rib mix? sd?))))

	       ((define-syntax)
		;;The body  form is a core  language DEFINE-SYNTAX macro
		;;use.    We  expand   and   evaluate  the   transformer
		;;expression, build a syntactic binding for it, register
		;;the label in the rib.   Finally we recurse on the rest
		;;of the body.
		;;
		(receive (id rhs-stx)
		    (%parse-define-syntax body-form-stx)
		  (when (bound-id-member? id kwd*)
		    (stx-error body-form-stx "cannot redefine keyword"))
		  ;;We want order here!?!
		  (let ((lab          (gen-define-syntax-label id rib sd?))
			(expanded-rhs (with-exception-handler
					  (lambda (E)
					    (raise
					     (condition E (make-macro-input-form-condition rhs-stx))))
					(lambda ()
					  (%expand-macro-transformer rhs-stx lexenv.expand)))))
		    ;;First map  the identifier  to the  label, creating
		    ;;the binding; then evaluate the macro transformer.
		    (extend-rib! rib id lab sd?)
		    (let ((entry (cons lab (with-exception-handler
					       (lambda (E)
						 (raise
						  (condition E (make-macro-input-form-condition rhs-stx))))
					     (lambda ()
					       (%eval-macro-transformer expanded-rhs lexenv.run))))))
		      (chi-body* (cdr body-form-stx*)
				 (cons entry lexenv.run)
				 (cons entry lexenv.expand)
				 lex* qrhs* mod** kwd* export-spec* rib
				 mix? sd?)))))

	       ((define-fluid-syntax)
		;;The body  form is a core  language DEFINE-FLUID-SYNTAX
		;;macro  use.  We  expand and  evaluate the  transformer
		;;expression, build syntactic  bindings for it, register
		;;the label in the rib.   Finally we recurse on the rest
		;;of the body.
		;;
		(receive (id rhs-stx)
		    (%parse-define-syntax body-form-stx)
		  (when (bound-id-member? id kwd*)
		    (stx-error body-form-stx "cannot redefine keyword"))
		  ;;We want order here!?!
		  (let* ((lab          (gen-define-syntax-label id rib sd?))
			 (flab         (gen-define-syntax-label id rib sd?))
			 (expanded-rhs (with-exception-handler
					   (lambda (E)
					     (raise
					      (condition E (make-macro-input-form-condition rhs-stx))))
					 (lambda ()
					   (%expand-macro-transformer rhs-stx lexenv.expand)))))
		    ;;First map the identifier to  the label, so that it
		    ;;is bound; then evaluate the macro transformer.
		    (extend-rib! rib id lab sd?)
		    (let* ((binding  (with-exception-handler
					 (lambda (E)
					   (raise
					    (condition E (make-macro-input-form-condition rhs-stx))))
					 (lambda ()
					   (%eval-macro-transformer expanded-rhs lexenv.run))))
			   ;;This LEXENV entry represents the definition
			   ;;of the fluid syntax.
			   (entry1   (cons lab (make-fluid-syntax-binding flab)))
			   ;;This  LEXENV entry  represents the  current
			   ;;binding  of the  fluid syntax;  the binding
			   ;;descriptor   is    of   type   LOCAL-MACRO,
			   ;;LOCAL-MACRO!  or  LOCAL-CTV.  Other entries
			   ;;like this  one can be pushed  to rebind the
			   ;;fluid syntax.
			   (entry2   (cons flab binding)))
		      (chi-body* (cdr body-form-stx*)
				 (cons* entry1 entry2 lexenv.run)
				 (cons* entry1 entry2 lexenv.expand)
				 lex* qrhs* mod** kwd* export-spec* rib
				 mix? sd?)))))

	       ((define-fluid-override)
		;;The body form is a core language DEFINE-FLUID-OVERRIDE
		;;macro use.   We push  new entries  on the  LEXENV then
		;;recurse on the rest of the body.
		;;
		;;For a  description of  how to re-bind  fluid syntaxes:
		;;see the transformer for FLUID-LET-SYNTAX.
		;;
		(receive (id rhs-stx)
		    (%parse-define-syntax body-form-stx)
		  (when (bound-id-member? id kwd*)
		    (stx-error body-form-stx "cannot redefine keyword"))
		  (let* ((fluid-label (let* ((label    (or (id->label id)
							   (stx-error id "unbound identifier")))
					     (binding  (label->syntactic-binding/no-indirection label lexenv.run)))
					(cond ((fluid-syntax-binding? binding)
					       (fluid-syntax-binding-fluid-label binding))
					      (else
					       (stx-error id "not a fluid identifier")))))
			 (binding     (with-exception-handler
					  (lambda (E)
					    (raise
					     (condition E (make-macro-input-form-condition rhs-stx))))
					(lambda ()
					  (%eval-macro-transformer
					   (%expand-macro-transformer rhs-stx lexenv.expand)
					   lexenv.run))))
			 (entry       (cons fluid-label binding)))
		    (chi-body* (cdr body-form-stx*)
			       (cons entry lexenv.run)
			       (cons entry lexenv.expand)
			       lex* qrhs* mod** kwd* export-spec* rib
			       mix? sd?))))

	       ((define-alias)
		;;The body  form is  a core language  DEFINE-ALIAS macro
		;;use.  We add a new association identifier/label to the
		;;current rib.   Finally we recurse  on the rest  of the
		;;body.
		;;
		(receive (alias-id old-id)
		    (%parse-define-alias body-form-stx)
		  (when (bound-id-member? old-id kwd*)
		    (stx-error body-form-stx "cannot redefine keyword"))
		  (cond ((id->label old-id)
			 => (lambda (label)
			      (extend-rib! rib alias-id label sd?)
			      (chi-body* (cdr body-form-stx*)
					 lexenv.run lexenv.expand
					 lex* qrhs* mod** kwd* export-spec* rib
					 mix? sd?)))
			(else
			 (stx-error body-form-stx "unbound source identifier")))))

	       ((let-syntax letrec-syntax)
		;;The  body  form  is  a  core  language  LET-SYNTAX  or
		;;LETREC-SYNTAX macro  use.  We expand and  evaluate the
		;;transformer expressions, build  syntactic bindings for
		;;them, register their labels in  a new rib because they
		;;are visible  only in the internal  body.  The internal
		;;forms are  spliced in the  external body but  with the
		;;rib added to them.
		;;
		(syntax-match body-form-stx ()
		  ((_ ((?xlhs* ?xrhs*) ...) ?xbody* ...)
		   (unless (valid-bound-ids? ?xlhs*)
		     (stx-error body-form-stx "invalid identifiers"))
		   (let* ((xlab*  (map gensym-for-label ?xlhs*))
			  (xrib   (make-filled-rib ?xlhs* xlab*))
			  ;;We evaluate the  transformers for LET-SYNTAX
			  ;;without   pushing  the   XRIB:  the   syntax
			  ;;bindings do not exist  in the environment in
			  ;;which the transformer is evaluated.
			  ;;
			  ;;We    evaluate    the    transformers    for
			  ;;LETREC-SYNTAX  after pushing  the XRIB:  the
			  ;;syntax bindings do  exist in the environment
			  ;;in which the transformer is evaluated.
			  (xbind* (map (lambda (x)
					 (let ((in-form (if (eq? type 'let-syntax)
							    x
							  (push-lexical-contour xrib x))))
					   (with-exception-handler
					       (lambda (E)
						 (raise
						  (condition E (make-macro-input-form-condition in-form))))
					     (lambda ()
					       (%eval-macro-transformer
						(%expand-macro-transformer in-form lexenv.expand)
						lexenv.run)))))
				    ?xrhs*)))
		     (chi-body*
		      ;;Splice the internal body forms but add a lexical
		      ;;contour to them.
		      (append (map (lambda (internal-body-form)
				     (push-lexical-contour xrib
				       internal-body-form))
				?xbody*)
			      (cdr body-form-stx*))
		      ;;Push   on   the  lexical   environment   entries
		      ;;corresponding  to  the defined  syntaxes.   Such
		      ;;entries  will  stay  there even  after  we  have
		      ;;processed the internal body forms; this is not a
		      ;;problem because the labels cannot be seen by the
		      ;;rest of the body.
		      (append (map cons xlab* xbind*) lexenv.run)
		      (append (map cons xlab* xbind*) lexenv.expand)
		      lex* qrhs* mod** kwd* export-spec* rib
		      mix? sd?)))))

	       ((begin-for-syntax)
		;;The body  form is  a BEGIN-FOR-SYNTAX syntax  use.  We
		;;expand the  expressions using LEXENV.EXPAND  as LEXENV
		;;for run-time, much like what we do when evaluating the
		;;right-hand side  of a DEFINE-SYNTAX, but  handling the
		;;sequence of  expressions as  a body;  then we  build a
		;;special   core   language   expression   with   global
		;;assignments; finally we evaluate result.
		;;
		(syntax-match body-form-stx ()
		  ((_ ?expr* ...)
		   (receive ( ;;
			     all-expr-core* rhs-core*
			     lexenv.expand^ lexenv.super^
			     lex*^ qrhs*^ mod**^ kwd*^ export-spec*^)
		       (let ((rtc (make-collector)))
			 (parametrise ((inv-collector rtc)
				       (vis-collector (lambda (x) (values))))
			   (receive (empty
				     lexenv.expand^ lexenv.super^
				     lex*^ qrhs*^ mod**^ kwd*^ export-spec*^)
			       ;;Expand  the  sequence  as  a  top-level
			       ;;body, accumulating the definitions.
			       (let ((lexenv.super                     lexenv.expand)
				     (mix-definitions-and-expressions? #t)
				     (shadowing-definitions?           #t))
				 (chi-body* (list (cons (bless 'begin) ?expr*))
					    lexenv.expand lexenv.super
					    '() '() '() '() '() rib
					    mix-definitions-and-expressions?
					    shadowing-definitions?))
			     ;;There should  be no  trailing expressions
			     ;;because we allowed mixing definitions and
			     ;;expressions as in a top-level program.
			     (assert (null? empty))
			     ;;Expand  the  definitions and  the  module
			     ;;trailing   expressions,  then   build  an
			     ;;expanded language expression.
			     (let* ((all-expr-core*  (chi-expr* (reverse-and-append mod**^)
								lexenv.expand^ lexenv.super^))
				    (rhs-core*       (chi-qrhs* qrhs*^ lexenv.expand^ lexenv.super^)))
			       ;;Now  that we  have  fully expanded  the
			       ;;forms:  we  invoke  all  the  libraries
			       ;;needed to evaluate them.
			       (for-each
				   (let ((register-visited-library (vis-collector)))
				     (lambda (lib)
				       (invoke-library lib)
				       (register-visited-library lib)))
				 (rtc))
			       ;;Let's  get   out  of   the  collectors'
			       ;;PARAMETRISE syntaxes.
			       (values all-expr-core* rhs-core*
				       lexenv.expand^ lexenv.super^
				       lex*^ qrhs*^ mod**^ kwd*^ export-spec*^)))))
		     ;;Build an expanded code expression and evaluate it.
		     (let ((code-core (build-sequence no-source
					(list (if (null? rhs-core*)
						  (build-void)
						(build-sequence no-source
						  (map (lambda (lhs rhs)
							 (build-global-assignment no-source lhs rhs))
						    (reverse lex*^)
						    (reverse rhs-core*))))
					      (if (null? all-expr-core*)
						  (build-void)
						(build-sequence no-source
						  all-expr-core*))))))
		       (parametrise ((current-run-lexenv (lambda () lexenv.run)))
			 (eval-core (expanded->core code-core))))
		     ;;Done!  Now go on with the next body forms.
		     (chi-body* (cdr body-form-stx*)
				lexenv.run lexenv.expand^
				lex* qrhs* mod** kwd* export-spec* rib
				mix? sd?)))
		  ))

	       ((begin)
		;;The body form is a  BEGIN syntax use.  Just splice the
		;;expressions and recurse on them.
		;;
		(syntax-match body-form-stx ()
		  ((_ ?expr* ...)
		   (chi-body* (append ?expr* (cdr body-form-stx*))
			      lexenv.run lexenv.expand
			      lex* qrhs* mod** kwd* export-spec* rib mix? sd?))))

	       ((stale-when)
		;;The body form is a STALE-WHEN syntax use.  Process the
		;;stale-when  guard  expression,  then just  splice  the
		;;internal expressions as we do for BEGIN and recurse.
		;;
		(syntax-match body-form-stx ()
		  ((_ ?guard ?expr* ...)
		   (begin
		     (handle-stale-when ?guard lexenv.expand)
		     (chi-body* (append ?expr* (cdr body-form-stx*))
				lexenv.run lexenv.expand
				lex* qrhs* mod** kwd* export-spec* rib mix? sd?)))))

	       ((tagged-dispatching)
		(chi-body* (cons (tag-identifier-dispatch (identifier-tag kwd) body-form-stx)
				 (cdr body-form-stx*))
			   lexenv.run lexenv.expand
			   lex* qrhs* mod** kwd* export-spec* rib mix? sd?))

	       ((global-macro global-macro!)
		;;The  body form  is a  macro  use, where  the macro  is
		;;imported  from  a  library.    We  perform  the  macro
		;;expansion,  then  recurse   on  the  resulting  syntax
		;;object.
		;;
		(let ((body-form-stx^ (chi-global-macro bind-val body-form-stx lexenv.run rib)))
		  (chi-body* (cons body-form-stx^ (cdr body-form-stx*))
			     lexenv.run lexenv.expand
			     lex* qrhs* mod** kwd* export-spec* rib mix? sd?)))

	       ((local-macro local-macro!)
		;;The  body form  is a  macro  use, where  the macro  is
		;;locally defined.  We perform the macro expansion, then
		;;recurse on the resulting syntax object.
		;;
		(let ((body-form-stx^ (chi-local-macro bind-val body-form-stx lexenv.run rib)))
		  (chi-body* (cons body-form-stx^ (cdr body-form-stx*))
			     lexenv.run lexenv.expand
			     lex* qrhs* mod** kwd* export-spec* rib mix? sd?)))

	       ((macro)
		;;The body  form is a  macro use,  where the macro  is a
		;;non-core macro integrated in the expander.  We perform
		;;the  macro expansion,  then recurse  on the  resulting
		;;syntax object.
		;;
		(let ((body-form-stx^ (chi-non-core-macro bind-val body-form-stx lexenv.run rib)))
		  (chi-body* (cons body-form-stx^ (cdr body-form-stx*))
			     lexenv.run lexenv.expand
			     lex* qrhs* mod** kwd* export-spec* rib mix? sd?)))

	       ((module)
		;;The body  form is  an internal module  definition.  We
		;;process the  module, then recurse  on the rest  of the
		;;body.
		;;
		(receive (lex* qrhs* m-exp-id* m-exp-lab* lexenv.run lexenv.expand mod** kwd*)
		    (chi-internal-module body-form-stx lexenv.run lexenv.expand lex* qrhs* mod** kwd*)
		  ;;Extend the rib with  the syntactic bindings exported
		  ;;by the module.
		  (vector-for-each (lambda (id lab)
				     (extend-rib! rib id lab sd?))
		    m-exp-id* m-exp-lab*)
		  (chi-body* (cdr body-form-stx*) lexenv.run lexenv.expand
			     lex* qrhs* mod** kwd* export-spec*
			     rib mix? sd?)))

	       ((library)
		;;The body form is a library definition.  We process the
		;;library, then recurse on the rest of the body.
		;;
		(expand-library (syntax->datum body-form-stx))
		(chi-body* (cdr body-form-stx*)
			   lexenv.run lexenv.expand
			   lex* qrhs* mod** kwd* export-spec*
			   rib mix? sd?))

	       ((export)
		;;The body form  is an EXPORT form.   We just accumulate
		;;the export specifications, to  be processed later, and
		;;we recurse on the rest of the body.
		;;
		(syntax-match body-form-stx ()
		  ((_ ?export-spec* ...)
		   (chi-body* (cdr body-form-stx*)
			      lexenv.run lexenv.expand
			      lex* qrhs* mod** kwd*
			      (append ?export-spec* export-spec*)
			      rib mix? sd?))))

	       ((import)
		;;The body form is an  IMPORT form.  We just process the
		;;form  which results  in  extending the  RIB with  more
		;;identifier-to-label associations.   Finally we recurse
		;;on the rest of the body.
		;;
		(%chi-import body-form-stx lexenv.run rib sd?)
		(chi-body* (cdr body-form-stx*) lexenv.run lexenv.expand
			   lex* qrhs* mod** kwd* export-spec* rib mix? sd?))

	       (else
		;;Any other expression.
		;;
		;;If mixed  definitions and expressions are  allowed, we
		;;handle this expression as an implicit definition:
		;;
		;;   (define dummy ?body-form)
		;;
		;;which is  not really part of  the lexical environment:
		;;we  only generate  a lex  and a  qrhs for  it, without
		;;adding entries to  LEXENV-RUN; then we move  on to the
		;;next body form.
		;;
		;;If mixed definitions and expressions are forbidden: we
		;;have reached  the end  of definitions and  expect this
		;;and  all  the  other  forms in  BODY-FORM-STX*  to  be
		;;expressions.  So BODY-FORM-STX*  becomes the "trailing
		;;expressions" return value.
		;;
		(if mix?
		    (chi-body* (cdr body-form-stx*)
			       lexenv.run lexenv.expand
			       (cons (gensym-for-lexical-var 'dummy) lex*)
			       (cons (cons 'top-expr body-form-stx) qrhs*)
			       mod** kwd* export-spec* rib #t sd?)
		  (values body-form-stx* lexenv.run lexenv.expand lex* qrhs* mod** kwd* export-spec*))))))))))

;;; --------------------------------------------------------------------

  (define (%parse-define input-form-stx)
    ;;Syntax  parser  for R6RS's  DEFINE  and  extended tagged  bindings
    ;;syntax.  Return 4 values:
    ;;
    ;;1..The identifier of the binding variable.
    ;;
    ;;2..An identifier representing the binding tag.   "<top>" is used when no tag is
    ;;   specified; "<procedure>" is used when a procedure is defined.
    ;;
    ;;3..False or an  object representing an instance  of "callable-signature"; false
    ;;   is returned when the binding is not a function.
    ;;
    ;;4..A qualified  right-hand side expression  (QRHS) representing the  binding to
    ;;   create.
    ;;
    (syntax-match input-form-stx (brace)
      ;;Function definition with tagged return values.
      ((_ ((brace ?id ?rv-tag0 ?rv-tag* ...) . ?fmls) ?b ?b* ...)
       (identifier? ?id)
       (receive (standard-formals-stx signature)
	   (parse-tagged-callable-spec-syntax (bless
					       `((brace _ ,?rv-tag0 . ,?rv-tag*) . ,?fmls))
					      input-form-stx)
	 (values ?id <procedure> signature (cons 'defun input-form-stx))))

      ;;Variable definition with tagged identifier.
      ((_ (brace ?id ?tag) ?val)
       (identifier? ?id)
       (values ?id ?tag #f (cons 'expr ?val)))

      ;;Variable definition with tagged identifier, no init.
      ((_ (brace ?id ?tag))
       (identifier? ?id)
       (values ?id ?tag #f (cons 'expr (bless '(void)))))

      ;;Function definition with possible tagged arguments.
      ((_ (?id . ?fmls) ?b ?b* ...)
       (identifier? ?id)
       (receive (standard-formals-stx signature)
	   (parse-tagged-callable-spec-syntax ?fmls input-form-stx)
	 (values ?id <procedure> signature (cons 'defun input-form-stx))))

      ;;R6RS variable definition.
      ((_ ?id ?val)
       (identifier? ?id)
       (values ?id <top> #f (cons 'expr ?val)))

      ;;R6RS variable definition, no init.
      ((_ ?id)
       (identifier? ?id)
       (values ?id <top> #f (cons 'expr (bless '(void)))))

      ))

  (define (%parse-define-syntax stx)
    ;;Syntax  parser for  R6RS's DEFINE-SYNTAX,  extended with  Vicare's
    ;;syntax.  Accept both:
    ;;
    ;;  (define-syntax ?name ?transformer-expr)
    ;;  (define-syntax (?name ?arg) ?body0 ?body ...)
    ;;
    (syntax-match stx ()
      ((_ ?id)
       (identifier? ?id)
       (values ?id (bless '(syntax-rules ()))))
      ((_ ?id ?transformer-expr)
       (identifier? ?id)
       (values ?id ?transformer-expr))
      ((_ (?id ?arg) ?body0 ?body* ...)
       (and (identifier? ?id)
	    (identifier? ?arg))
       (values ?id (bless `(lambda (,?arg) ,?body0 ,@?body*))))
      ))

  (define (%parse-define-alias body-form-stx)
    ;;Syntax parser for Vicares's DEFINE-ALIAS.
    ;;
    (syntax-match body-form-stx ()
      ((_ ?alias-id ?old-id)
       (and (identifier? ?alias-id)
	    (identifier? ?old-id))
       (values ?alias-id ?old-id))
      ))

;;; --------------------------------------------------------------------

  (module (%chi-import)
    ;;Process an IMPORT form.  The purpose of such forms is to push some
    ;;new identifier-to-label association on the current RIB.
    ;;
    (define (%chi-import body-form-stx lexenv.run rib sd?)
      (receive (id* lab*)
	  (%any-import*-checked body-form-stx lexenv.run)
	(vector-for-each (lambda (id lab)
			   (extend-rib! rib id lab sd?))
	  id* lab*)))

    (define (%any-import*-checked import-form lexenv.run)
      (syntax-match import-form ()
	((?ctxt ?import-spec* ...)
	 (%any-import* ?ctxt ?import-spec* lexenv.run))
	(_
	 (stx-error import-form "invalid import form"))))

    (define (%any-import* ctxt import-spec* lexenv.run)
      (if (null? import-spec*)
	  (values '#() '#())
	(let-values
	    (((t1 t2) (%any-import  ctxt (car import-spec*) lexenv.run))
	     ((t3 t4) (%any-import* ctxt (cdr import-spec*) lexenv.run)))
	  (values (vector-append t1 t3)
		  (vector-append t2 t4)))))

    (define (%any-import ctxt import-spec lexenv.run)
      (if (identifier? import-spec)
	  (%module-import (list ctxt import-spec) lexenv.run)
	(%library-import (list ctxt import-spec))))

    (define (%module-import import-form lexenv.run)
      (syntax-match import-form ()
	((_ ?id)
	 (identifier? ?id)
	 (receive (type bind-val kwd)
	     (expr-syntax-type ?id lexenv.run)
	   (case type
	     (($module)
	      (let ((iface bind-val))
		(values (module-interface-exp-id*     iface ?id)
			(module-interface-exp-lab-vec iface))))
	     (else
	      (stx-error import-form "invalid import")))))))

    (define (%library-import import-form)
      (syntax-match import-form ()
	((?ctxt ?imp* ...)
	 ;;NAME-VEC  is a  vector of  symbols representing  the external
	 ;;names of  the imported  bindings.  LABEL-VEC  is a  vector of
	 ;;label gensyms uniquely associated to the imported bindings.
	 (receive (name-vec label-vec)
	     (let ()
	       (import PARSE-IMPORT-SPEC)
	       (parse-import-spec* (syntax->datum ?imp*)))
	   (values (vector-map (lambda (name)
				 ($datum->syntax ?ctxt name))
		     name-vec)
		   label-vec)))
	(_
	 (stx-error import-form "invalid import form"))))

    #| end of module: %CHI-IMPORT |# )

  #| end of module: CHI-BODY* |# )


;;;; chi procedures: module processing

(module (chi-internal-module
	 module-interface-exp-id*
	 module-interface-exp-lab-vec)

  (define-record module-interface
    (first-mark
		;The first  mark in  the lexical  context of  the MODULE
		;form.
     exp-id-vec
		;A vector of identifiers exported by the module.
     exp-lab-vec
		;A  vector   of  gensyms   acting  as  labels   for  the
		;identifiers in the field EXP-ID-VEC.
     ))

  (define (chi-internal-module module-form-stx lexenv.run lexenv.expand lex* qrhs* mod** kwd*)
    ;;Expand the  syntax object MODULE-FORM-STX which  represents a core
    ;;langauge MODULE syntax use.
    ;;
    ;;LEXENV.RUN  and  LEXENV.EXPAND  must  be  lists  representing  the
    ;;current lexical environment for run and expand times.
    ;;
    (receive (name export-id* internal-body-form*)
	(%parse-module module-form-stx)
      (let* ((module-rib               (make-empty-rib))
	     (internal-body-form*/rib  (map (lambda (form)
					      (push-lexical-contour module-rib form))
					 (syntax->list internal-body-form*))))
	(receive (leftover-body-expr* lexenv.run lexenv.expand lex* qrhs* mod** kwd* _export-spec*)
	    ;;In a module: we do not want the trailing expressions to be
	    ;;converted to dummy definitions; rather  we want them to be
	    ;;accumulated in the MOD** argument, for later expansion and
	    ;;evaluation.  So we set MIX? to false.
	    (let ((empty-export-spec*      '())
		  (mix?                    #f)
		  (shadowing-definitions?  #t))
	      (chi-body* internal-body-form*/rib
			 lexenv.run lexenv.expand
			 lex* qrhs* mod** kwd* empty-export-spec*
			 module-rib mix? shadowing-definitions?))
	  ;;The list  of exported identifiers  is not only the  one from
	  ;;the MODULE  argument, but also  the one from all  the EXPORT
	  ;;forms in the MODULE's body.
	  (let* ((all-export-id*  (vector-append export-id* (list->vector _export-spec*)))
		 (all-export-lab* (vector-map
				      (lambda (id)
					;;For every  exported identifier
					;;there must be  a label already
					;;in the rib.
					(or (id->label (make-<stx> (identifier->symbol id)
								   (<stx>-mark* id)
								   (list module-rib)
								   '()))
					    (stx-error id "cannot find module export")))
				    all-export-id*))
		 (mod**           (cons leftover-body-expr* mod**)))
	    (if (not name)
		;;The module  has no name.  All  the exported identifier
		;;will go in the enclosing lexical environment.
		(values lex* qrhs* all-export-id* all-export-lab* lexenv.run lexenv.expand mod** kwd*)
	      ;;The module has a name.  Only  the name itself will go in
	      ;;the enclosing lexical environment.
	      (let* ((name-label (gensym-for-label 'module))
		     (iface      (make-module-interface
				  (car ($<stx>-mark* name))
				  (vector-map
				      (lambda (x)
					;;This   is   a  syntax   object
					;;holding an identifier.
					(let ((rib* '())
					      (ae*  '()))
					  (make-<stx> ($<stx>-expr x)
						      ($<stx>-mark* x)
						      rib*
						      ae*)))
				    all-export-id*)
				  all-export-lab*))
		     (binding    (make-module-binding iface))
		     (entry      (cons name-label binding)))
		(values lex* qrhs*
			;;FIXME:  module   cannot  export   itself  yet.
			;;Abdulaziz Ghuloum.
			(vector name)
			(vector name-label)
			(cons entry lexenv.run)
			(cons entry lexenv.expand)
			mod** kwd*))))))))

  (define (%parse-module module-form-stx)
    ;;Parse a  syntax object representing  a core language  MODULE form.
    ;;Return 3  values: false or  an identifier representing  the module
    ;;name; a list  of identifiers selecting the  exported bindings from
    ;;the first MODULE  argument; a list of  syntax objects representing
    ;;the internal body forms.
    ;;
    (syntax-match module-form-stx ()
      ((_ (?export* ...) ?body* ...)
       (begin
	 (unless (for-all identifier? ?export*)
	   (stx-error module-form-stx "module exports must be identifiers"))
	 (values #f (list->vector ?export*) ?body*)))
      ((_ ?name (?export* ...) ?body* ...)
       (begin
	 (unless (identifier? ?name)
	   (stx-error module-form-stx "module name must be an identifier"))
	 (unless (for-all identifier? ?export*)
	   (stx-error module-form-stx "module exports must be identifiers"))
	 (values ?name (list->vector ?export*) ?body*)))
      ))

  (module (module-interface-exp-id*)

    (define (module-interface-exp-id* iface id-for-marks)
      (let ((diff   (%diff-marks (<stx>-mark* id-for-marks)
				 (module-interface-first-mark iface)))
	    (id-vec (module-interface-exp-id-vec iface)))
	(if (null? diff)
	    id-vec
	  (vector-map
	      (lambda (x)
		(let ((rib* '())
		      (ae*  '()))
		  (make-<stx> ($<stx>-expr x)
			      (append diff ($<stx>-mark* x))
			      rib*
			      ae*)))
	    id-vec))))

    (define (%diff-marks mark* the-mark)
      ;;MARK* must be  a non-empty list of marks; THE-MARK  must be a mark
      ;;in MARK*.  Return  a list of the  elements of MARK* up  to and not
      ;;including THE-MARK.
      ;;
      (when (null? mark*)
	(error '%diff-marks "BUG: should not happen"))
      (let ((a (car mark*)))
	(if (eq? a the-mark)
	    '()
	  (cons a (%diff-marks (cdr mark*) the-mark)))))

    #| end of module: MODULE-INTERFACE-EXP-ID* |# )

  #| end of module |# )


;;;; chi procedures: stale-when handling

(define (handle-stale-when guard-expr lexenv.expand)
  (let* ((stc       (make-collector))
	 (core-expr (parametrise ((inv-collector stc))
		      (chi-expr guard-expr lexenv.expand lexenv.expand))))
    (cond ((stale-when-collector)
	   => (lambda (c)
		(c core-expr (stc)))))))


;;;; chi procedures: end of module

#| end of module |# )


;;;; errors

(define-condition-type &macro-input-form
    &condition
  make-macro-input-form-condition
  macro-input-form-condition?
  (form macro-input-form-condition-object))

(define-condition-type &macro-expanded-input-form
    &condition
  make-macro-expanded-input-form-condition
  macro-expanded-input-form-condition?
  (form macro-expanded-input-form-condition-object))

(define (assertion-error expr source-identifier
			 byte-offset character-offset
			 line-number column-number)
  ;;Invoked by the  expansion of the ASSERT macro to  raise an assertion
  ;;violation.
  ;;
  (raise
   (condition (make-assertion-violation)
	      (make-who-condition 'assert)
	      (make-message-condition "assertion failed")
	      (make-irritants-condition (list expr))
	      (make-source-position-condition source-identifier
					      byte-offset character-offset
					      line-number column-number))))

(define-syntax stx-error
  ;;Convenience wrapper for raising syntax violations.
  ;;
  (syntax-rules (quote)
    ((_ ?expr-stx)
     (syntax-violation #f "invalid syntax" ?expr-stx))
    ((_ ?expr-stx ?msg)
     (syntax-violation #f ?msg ?expr-stx))
    ((_ ?expr-stx ?msg ?who)
     (syntax-violation ?who ?msg ?expr-stx))
    ))

(module (syntax-error
	 syntax-violation
	 %raise-unbound-error)

  (define (syntax-error x . args)
    (unless (for-all string? args)
      (assertion-violation 'syntax-error "invalid argument" args))
    (raise
     (condition (make-message-condition (if (null? args)
					    "invalid syntax"
					  (apply string-append args)))
		(make-syntax-violation (syntax->datum x) #f)
		(%expression->source-position-condition x)
		(%extract-trace x))))

  (case-define syntax-violation
    ;;Defined  by R6RS.   WHO must  be false  or a  string or  a symbol.
    ;;MESSAGE must be a string.  FORM must be a syntax object or a datum
    ;;value.  SUBFORM must be a syntax object or a datum value.
    ;;
    ;;The SYNTAX-VIOLATION  procedure raises  an exception,  reporting a
    ;;syntax violation.  WHO should  describe the macro transformer that
    ;;detected the exception.  The  MESSAGE argument should describe the
    ;;violation.  FORM should be the erroneous source syntax object or a
    ;;datum value  representing a  form.  The optional  SUBFORM argument
    ;;should be a syntax object or  datum value representing a form that
    ;;more precisely locates the violation.
    ;;
    ;;If WHO is false, SYNTAX-VIOLATION attempts to infer an appropriate
    ;;value for the  condition object (see below) as  follows: when FORM
    ;;is  either  an  identifier  or  a  list-structured  syntax  object
    ;;containing an identifier  as its first element,  then the inferred
    ;;value is the identifier's symbol.   Otherwise, no value for WHO is
    ;;provided as part of the condition object.
    ;;
    ;;The condition object provided with the exception has the following
    ;;condition types:
    ;;
    ;;*  If WHO  is not  false  or can  be inferred,  the condition  has
    ;;condition type  "&who", with WHO  as the  value of its  field.  In
    ;;that  case,  WHO should  identify  the  procedure or  entity  that
    ;;detected the  exception.  If it  is false, the condition  does not
    ;;have condition type "&who".
    ;;
    ;;* The condition has condition type "&message", with MESSAGE as the
    ;;value of its field.
    ;;
    ;;* The condition has condition type "&syntax" with FORM and SUBFORM
    ;;as the value of its fields.  If SUBFORM is not provided, the value
    ;;of the subform field is false.
    ;;
    ((who msg form)
     (syntax-violation who msg form #f))
    ((who msg form subform)
     (%syntax-violation who msg form (make-syntax-violation form subform))))

  (define (%syntax-violation source-who msg form condition-object)
    (define-constant __who__ 'syntax-violation)
    (unless (string? msg)
      (assertion-violation __who__ "message is not a string" msg))
    (let ((source-who (cond ((or (string? source-who)
				 (symbol? source-who))
			     source-who)
			    ((not source-who)
			     (syntax-match form ()
			       (id
				(identifier? id)
				(syntax->datum id))
			       ((id . rest)
				(identifier? id)
				(syntax->datum id))
			       (_  #f)))
			    (else
			     (assertion-violation __who__ "invalid who argument" source-who)))))
      (raise
       (condition (if source-who
		      (make-who-condition source-who)
		    (condition))
		  (make-message-condition msg)
		  condition-object
		  (%expression->source-position-condition form)
		  (%extract-trace form)))))

  (define (%raise-unbound-error source-who form id)
    (raise
     (condition (if source-who
		    (make-who-condition source-who)
		  (condition))
		(make-message-condition "unbound identifier")
		(make-undefined-violation)
		(make-syntax-violation form id)
		(%expression->source-position-condition id)
		(%extract-trace id))))

  (define (%extract-trace x)
    (define-condition-type &trace &condition
      make-trace trace?
      (form trace-form))
    (let f ((x x))
      (cond ((<stx>? x)
	     (apply condition
		    (make-trace x)
		    (map f (<stx>-ae* x))))
	    ((annotation? x)
	     (make-trace (make-<stx> x '() '() '())))
	    (else
	     (condition)))))

  (define (%expression->source-position-condition x)
    (expression-position x))

  #| end of module |# )

(define (%raise-warning who message position . irritants)
  (raise-continuable
   (condition (make-warning)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants)
	      (if (source-position-condition? position)
		  (make-source-position-condition
		   (source-position-port-id   position)
		   (source-position-byte      position)
		   (source-position-character position)
		   (source-position-line      position)
		   (source-position-column    position))
		(condition)))))


;;;; R6RS programs and libraries helpers

(define (initial-visit! macro*)
  (for-each (lambda (x)
	      (let ((loc  (car  x))
		    (proc (cadr x)))
		(set-symbol-value! loc proc)))
    macro*))


;;;; done

;;Register the expander with the library manager.
(current-library-expander expand-library)

#| end of library |# )

;;; end of file
;;Local Variables:
;;eval: (put 'build-library-letrec*		'scheme-indent-function 1)
;;eval: (put 'build-application			'scheme-indent-function 1)
;;eval: (put 'build-conditional			'scheme-indent-function 1)
;;eval: (put 'build-case-lambda			'scheme-indent-function 1)
;;eval: (put 'build-lambda			'scheme-indent-function 1)
;;eval: (put 'build-foreign-call		'scheme-indent-function 1)
;;eval: (put 'build-sequence			'scheme-indent-function 1)
;;eval: (put 'build-global-assignment		'scheme-indent-function 1)
;;eval: (put 'build-lexical-assignment		'scheme-indent-function 1)
;;eval: (put 'build-letrec*			'scheme-indent-function 1)
;;eval: (put 'build-data			'scheme-indent-function 1)
;;eval: (put 'case-object-type-binding		'scheme-indent-function 1)
;;eval: (put 'if-wants-descriptive-gensyms	'scheme-indent-function 1)
;;eval: (put 'push-lexical-contour		'scheme-indent-function 1)
;;eval: (put 'set-interaction-env-lab.loc/lex*!	'scheme-indent-function 1)
;;eval: (put 'syntactic-binding-getprop		'scheme-indent-function 1)
;;eval: (put 'sys.syntax-case			'scheme-indent-function 2)
;;End:
