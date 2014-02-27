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

    ;; expand-time type specs: object specs
    identifier-object-spec		set-identifier-object-spec!
    (rename (public-make-object-spec make-object-spec))
    object-spec?
    object-spec-name
    object-spec-type-id			object-spec-pred-id

    ;; expand-time type specs: callable specs
    identifier-callable-spec		set-identifier-callable-spec!
    make-callable-spec			callable-spec?
    callable-spec-name			callable-spec-min-arity
    callable-spec-max-arity		callable-spec-dispatcher

    ;;The following are inspection functions for debugging purposes.
    (rename (<stx>?		syntax-object?)
	    (<stx>-expr		syntax-object-expression)
	    (<stx>-mark*	syntax-object-marks)
	    (<stx>-subst*	syntax-object-substs)
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

(define* (make-synonym-transformer (x identifier?))
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

(define* (synonym-transformer-identifier (x synonym-transformer?))
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
	   (if (options.strict-r6rs)
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
	   (if (options.strict-r6rs)
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
	   (if (options.strict-r6rs)
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
	   (if (options.strict-r6rs)
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
	      (for-all id-stx? ?old-name*)
	      (for-all id-stx? ?new-name*))
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
	      (for-all id-stx? ?sym*))
	 (let ((subst (%recurse ?import-set)))
	   (rem* (map syntax->datum ?sym*) subst)))

	((?only ?import-set ?name* ...)
	 (and (eq? (syntax->datum ?only) 'only)
	      (for-all id-stx? ?name*))
	 (let* ((subst  (%recurse ?import-set))
		(name*  (map syntax->datum ?name*))
		(name*  (remove-dups name*))
		(lab*   (find* name* subst ?import-set)))
	   (map cons name* lab*)))

	((?prefix ?import-set ?the-prefix)
	 (and (eq? (syntax->datum ?prefix) 'prefix)
	      (id-stx? ?prefix))
	 (let ((subst   (%recurse ?import-set))
	       (prefix  (symbol->string (syntax->datum ?the-prefix))))
	   (map (lambda (x)
		  (cons (string->symbol
			 (string-append prefix (symbol->string (car x))))
			(cdr x)))
	     subst)))

	((?deprefix ?import-set ?the-prefix)
	 (and (eq? (syntax->datum ?deprefix) 'deprefix)
	      (id-stx? ?the-prefix))
	 (if (options.strict-r6rs)
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
	      (id-stx? ?suffix))
	 (if (options.strict-r6rs)
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
	      (id-stx? ?the-suffix))
	 (if (options.strict-r6rs)
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
	   (id-stx? ?id)
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

  (define (id-stx? x)
    ;;Return true if X is an identifier.
    ;;
    (symbol? (syntax->datum x)))

;;; --------------------------------------------------------------------

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
(define-record <rib>
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
   ))

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
  (make-<rib> (map identifier->symbol id*)
	      (map <stx>-mark* id*)
	      label*
	      #f))

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
  (make-<rib> (map car export-subst)
	      (map (lambda (x) TOP-MARK*) export-subst)
	      (map cdr export-subst)
	      #f))

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
  (define* (extend-rib! (rib <rib>?) (id identifier?) label shadowing-definitions?)
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

(define* (seal-rib! (rib <rib>?))
  (let ((name* ($<rib>-name* rib)))
    (unless (null? name*) ;only seal if RIB is not empty
      (let ((name* (list->vector name*)))
	($set-<rib>-name*!       rib name*)
	($set-<rib>-mark**!      rib (list->vector ($<rib>-mark** rib)))
	($set-<rib>-label*!      rib (list->vector ($<rib>-label* rib)))
	($set-<rib>-sealed/freq! rib (make-vector (vector-length name*) 0))))))

(define* (unseal-rib! (rib <rib>?))
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
;;identifer has a list of marks and a list of substitutions.
;;
;;The idea  is that to get  the label of  an identifier, we look  up the
;;identifier's substitutions for  a mapping with the same  name and same
;;marks (see SAME-MARKS? below).
;;

(define-record <stx>
  (expr
   mark*
   subst*
   ae*)
  (lambda (S port subwriter) ;record printer function
    (define-inline (%display thing)
      (display thing port))
    (define-inline (%write thing)
      (write thing port))
    (define-inline (%pretty-print thing)
      (pretty-print* thing port 0 #f))
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

(define (datum->stx id datum)
  ;;Since all the identifier->label bindings are encapsulated within the
  ;;identifier, converting a datum to a syntax object (non-hygienically)
  ;;is  done simply  by creating  an  STX that  has the  same marks  and
  ;;substitutions as the identifier.
  ;;
  (make-<stx> datum
	      (<stx>-mark*  id)
	      (<stx>-subst* id)
	      (<stx>-ae*    id)))

(define (datum->syntax id datum)
  (if (identifier? id)
      (datum->stx id datum)
    (assertion-violation 'datum->syntax
      "expected identifier as context syntax object" id)))

(define (syntax->datum S)
  (strip S '()))

(define (mkstx stx/expr mark* subst* ae*)
  ;;This is the proper constructor for wrapped syntax objects.
  ;;
  ;;STX/EXPR can be a raw sexp, an instance of <STX> or a wrapped syntax
  ;;object.  MARK* is a list of marks.  SUBST* is a list of substs.
  ;;
  ;;AE* == annotated expressions???
  ;;
  ;;When STX/EXPR  is a  raw sexp:  just build and  return a  new syntax
  ;;object with the lexical context described by the given arguments.
  ;;
  ;;When STX/EXPR is a syntax object:  join the wraps from STX/EXPR with
  ;;given wraps, making sure that marks and anti-marks and corresponding
  ;;shifts cancel properly.
  ;;
  (if (and (<stx>? stx/expr)
	   (not (top-marked? mark*)))
      (receive (mark* subst* ae*)
	  (join-wraps mark* subst* ae* stx/expr)
	(make-<stx> (<stx>-expr stx/expr) mark* subst* ae*))
    (make-<stx> stx/expr mark* subst* ae*)))

(define (push-lexical-contour rib stx/expr)
  ;;Add a rib to a syntax  object or expression and return the resulting
  ;;syntax object.  This  procedure introduces a lexical  contour in the
  ;;context of the given syntax object or expression.
  ;;
  ;;RIB must be an instance of <RIB>.
  ;;
  ;;STX/EXPR can be a raw sexp, an instance of <STX> or a wrapped syntax
  ;;object.
  ;;
  ;;This function prepares  a computation that will  be lazily performed
  ;;later; the RIB will be pushed on the stack of substitutions in every
  ;;identifier in the fully unwrapped returned syntax object.
  ;;
  (mkstx stx/expr
	 '() #;mark*
	 (list rib)
	 '() #;ae*
	 ))

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
		     (> ($vector-length x) 0)
		     (annotation? ($vector-ref x 0))))
	    ;;TODO Ask Kent  why this is a  sufficient test.  (Abdulaziz
	    ;;Ghuloum)
	    (strip-annotations x)
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

  (define (strip-annotations x)
    (cond ((pair? x)
	   (cons (strip-annotations ($car x))
		 (strip-annotations ($cdr x))))
	  ((vector? x)
	   (vector-map strip-annotations x))
	  ((annotation? x)
	   (annotation-stripped x))
	  (else x)))

  #| end of module: STRIP |# )


;;;; syntax objects: mapping identifiers to labels

(module (id->label)

  (define (id->label id)
    ;;Given the identifier  ID search its substs for  a label associated
    ;;with the  same sym  and marks.   If found  return the  label, else
    ;;return false.
    ;;
    (let ((sym (identifier->symbol id)))
      (let search ((subst* ($<stx>-subst* id))
		   (mark*  ($<stx>-mark*  id)))
	(cond ((null? subst*)
	       #f)
	      ((eq? ($car subst*) 'shift)
	       ;;A shift is inserted when a  mark is added.  So, we search
	       ;;the rest of the substitution without the mark.
	       (search ($cdr subst*) ($cdr mark*)))
	      (else
	       (let ((rib ($car subst*)))
		 (define (next-search)
		   (search ($cdr subst*) mark*))
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

(define-auxiliary-syntaxes r6rs-record-type vicare-struct-type
  object-spec-type)

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
  (sys.syntax-case stx (r6rs-record-type vicare-struct-type object-spec-type)
    ((_ (?who ?input-stx ?type-id ?lexenv)
	((r6rs-record-type)	?r6rs-body0   ?r6rs-body   ...)
	((vicare-struct-type)	?struct-body0 ?struct-body ...)
	((object-spec-type)	?spec-body0   ?spec-body   ...))
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
	      ((identifier-object-spec ?type-id)
	       ?spec-body0 ?spec-body ...)
	      (else
	       (syntax-violation ?who
		 "neither a struct type nor an R6RS record type nor a spec type"
		 ?input-stx ?type-id))))))
    ((_ (?who ?input-stx ?type-id ?lexenv ?binding)
	((r6rs-record-type)	?r6rs-body0   ?r6rs-body   ...)
	((vicare-struct-type)	?struct-body0 ?struct-body ...)
	((object-spec-type)	?spec-body0   ?spec-body   ...))
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
	      ((identifier-object-spec ?type-id)
	       ?spec-body0 ?spec-body ...)
	      (else
	       (syntax-violation ?who
		 "neither a struct type nor an R6RS record type"
		 ?input-stx ?type-id))))))
    ))


;;;; marks

;;The body  of a library, when it  is first processed, gets  this set of
;;marks...
(define-constant TOP-MARK*
  '(top))

;;... consequently, every syntax object that  has a TOP in its marks set
;;was present in the program source.
(define-inline (top-marked? m*)
  (memq 'top m*))

(define (symbol->top-marked-identifier sym)
  ;;Given a  raw Scheme  symbol return a  syntax object  representing an
  ;;identifier with top marks.
  ;;
  (make-<stx> sym TOP-MARK* '() '()))

(define (top-marked-symbols rib)
  ;;Scan the <RIB> RIB and return a list of symbols representing binding
  ;;names and having the top mark.
  ;;
  (receive (sym* mark**)
      ;;If RIB is sealed the fields  hold vectors, else they hold lists;
      ;;we want lists here.
      (let ((sym*   (<rib>-name*  rib))
	    (mark** (<rib>-mark** rib)))
	(if (<rib>-sealed/freq rib)
	    (values (vector->list sym*)
		    (vector->list mark**))
	  (values sym* mark**)))
    (let recur ((sym*   sym*)
		(mark** mark**))
      (cond ((null? sym*)
	     '())
	    ((equal? (car mark**) TOP-MARK*)
	     (cons (car sym*)
		   (recur (cdr sym*) (cdr mark**))))
	    (else
	     (recur (cdr sym*) (cdr mark**)))))))

;;;; syntax objects and marks

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
;;We always  maintain the  invariant that we  do not double  wrap syntax
;;objects.  The  only way  to get a  doubly-wrapped syntax object  is by
;;doing DATUM->STX  (above) where the  datum is itself a  wrapped syntax
;;object (R6RS  may not even  consider wrapped syntax objects  as datum,
;;but let's not worry now).
;;
;;Syntax objects  have, in  addition to the  EXPR, a  substitution field
;;SUBST*: it is a list where each  element is either a RIB or the symbol
;;"shift".  Normally,  a new  RIB is  added to an  STX at  every lexical
;;contour of the program in  order to capture the bindings introduced in
;;that contour.
;;
;;The MARK* field of an STX is  a list of marks; each of these marks can
;;be  either  a  generated mark  or  an  antimark.   Two marks  must  be
;;EQ?-comparable, so we use a string of one char (we assume that strings
;;are mutable in the underlying Scheme implementation).

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
(define anti-mark #f)
(define anti-mark? not)

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
;;'shift  is pushed  to the  <stx>-subst* list.   Every time  a mark  is
;;cancelled  by   an  anti-mark,  the  corresponding   shifts  are  also
;;cancelled.

;;The procedure join-wraps,  here, is used to compute the  new mark* and
;;subst* that would  result when the m1*  and s1* are added  to an stx's
;;mark* and subst*.
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
;;since mx  would cancel with the  anti-mark.  The substs would  have to
;;also cancel since:
;;
;;    s1* = (sx* ... sx)
;;    s2* = (sy sy* ...)
;;
;;then the resulting substs should be:
;;
;;    (sx* ... sy* ...)
;;
;;Notice that both SX and SY would be shift marks.
;;
;;All   this  work   is  performed   by  the   functions  ADD-MARK   and
;;%DO-MACRO-CALL.
;;

(module (join-wraps)

  (define (join-wraps mark1* subst1* ae1* stx2)
    (let ((mark2*   ($<stx>-mark*  stx2))
	  (subst2*  ($<stx>-subst* stx2))
	  (ae2*     ($<stx>-ae*    stx2)))
      ;;If the first item in mark2* is an anti-mark...
      (if (and (not (null? mark1*))
	       (not (null? mark2*))
	       (anti-mark? ($car mark2*)))
	  ;;...cancel mark, anti-mark, and corresponding shifts.
	  (values (%append-cancel-facing mark1*  mark2*)
		  (%append-cancel-facing subst1* subst2*)
		  (%merge-ae* ae1* ae2*))
	;;..else no cancellation takes place.
	(values (append mark1*  mark2*)
		(append subst1* subst2*)
		(%merge-ae* ae1* ae2*)))))

  (define (%merge-ae* ls1 ls2)
    (if (and (pair? ls1)
	     (pair? ls2)
	     (not ($car ls2)))
	(%append-cancel-facing ls1 ls2)
      (append ls1 ls2)))

  (define (%append-cancel-facing ls1 ls2)
    ;;Given two non-empty lists: append them discarding the last item in
    ;;LS1 and the first item in LS2.  Examples:
    ;;
    ;;   (%append-cancel-facing '(1 2 3) '(4 5 6))	=> (1 2 5 6)
    ;;   (%append-cancel-facing '(1)     '(2 3 4))	=> (3 4)
    ;;   (%append-cancel-facing '(1)     '(2))		=> ()
    ;;
    (let recur ((x   ($car ls1))
		(ls1 ($cdr ls1)))
      (if (null? ls1)
	  ($cdr ls2)
	(cons x (recur ($car ls1) ($cdr ls1))))))

  #| end of module: JOIN-WRAPS |# )

(define (same-marks? x y)
  ;;Two lists  of marks are  considered the same  if they have  the same
  ;;length and the corresponding marks on each are EQ?.
  ;;
  (or (and (null? x) (null? y)) ;(eq? x y)
      (and (pair? x) (pair? y)
	   (eq? ($car x) ($car y))
	   (same-marks? ($cdr x) ($cdr y)))))

(define (add-mark mark subst expr ae)
  ;;Build and return  a new syntax object wrapping  EXPR and having MARK
  ;;pushed on its list of marks.
  ;;
  ;;SUBST can be #f or a list of substitutions.
  ;;
  (define (merge-ae* ls1 ls2)
    ;;Append LS1 and LS2 and return the result; if the car or LS2 is #f:
    ;;append LS1 and (cdr LS2).
    ;;
    ;;   (merge-ae* '(a b c) '(d  e f))   => (a b c d e f)
    ;;   (merge-ae* '(a b c) '(#f e f))   => (a b c e f)
    ;;
    (if (and (pair? ls1)
	     (pair? ls2)
	     (not (car ls2)))
	(cancel ls1 ls2)
      (append ls1 ls2)))
  (define (cancel ls1 ls2)
    ;;Expect LS1 to be a proper list  of one or more elements and LS2 to
    ;;be a proper  list of one or more elements.  Append  the cdr of LS2
    ;;to LS1 and return the result:
    ;;
    ;;   (cancel '(a b c) '(d e f))
    ;;   => (a b c e f)
    ;;
    ;;This function is like:
    ;;
    ;;   (append ls1 (cdr ls2))
    ;;
    ;;we just hope to be a bit more efficient.
    ;;
    (let recur ((A (car ls1))
		(D (cdr ls1)))
      (if (null? D)
	  (cdr ls2)
	(cons A (recur (car D) (cdr D))))))
  (define (f sub-expr mark subst1* ae*)
    (cond ((pair? sub-expr)
	   (let ((a (f (car sub-expr) mark subst1* ae*))
		 (d (f (cdr sub-expr) mark subst1* ae*)))
	     (if (eq? a d)
		 sub-expr
	       (cons a d))))
	  ((vector? sub-expr)
	   (let* ((ls1 (vector->list sub-expr))
		  (ls2 (map (lambda (x)
			      (f x mark subst1* ae*))
			 ls1)))
	     (if (for-all eq? ls1 ls2)
		 sub-expr
	       (list->vector ls2))))
	  ((<stx>? sub-expr)
	   (let ((mark*   (<stx>-mark*  sub-expr))
		 (subst2* (<stx>-subst* sub-expr)))
	     (cond ((null? mark*)
		    (f (<stx>-expr sub-expr)
		       mark
		       (append subst1* subst2*)
		       (merge-ae* ae* (<stx>-ae* sub-expr))))
		   ((eq? (car mark*) anti-mark)
		    (make-<stx> (<stx>-expr sub-expr) (cdr mark*)
				(cdr (append subst1* subst2*))
				(merge-ae* ae* (<stx>-ae* sub-expr))))
		   (else
		    (make-<stx> (<stx>-expr sub-expr)
				(cons mark mark*)
				(let ((s* (cons 'shift (append subst1* subst2*))))
				  (if subst
				      (cons subst s*)
				    s*))
				(merge-ae* ae* (<stx>-ae* sub-expr)))))))
	  ((symbol? sub-expr)
	   (syntax-violation #f
	     "raw symbol encountered in output of macro"
	     expr sub-expr))
	  (else
	   (make-<stx> sub-expr (list mark) subst1* ae*))))
  (mkstx (f expr mark '() '()) '() '() (list ae)))


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

(define (syntax-car x)
  (cond ((<stx>? x)
	 (mkstx (syntax-car ($<stx>-expr x))
		($<stx>-mark*  x)
		($<stx>-subst* x)
		($<stx>-ae*    x)))
	((annotation? x)
	 (syntax-car (annotation-expression x)))
	((pair? x)
	 ($car x))
	(else
	 (assertion-violation 'syntax-car "BUG: not a pair" x))))

(define (syntax-cdr x)
  (cond ((<stx>? x)
	 (mkstx (syntax-cdr ($<stx>-expr x))
		($<stx>-mark*  x)
		($<stx>-subst* x)
		($<stx>-ae*    x)))
	((annotation? x)
	 (syntax-cdr (annotation-expression x)))
	((pair? x)
	 ($cdr x))
	(else
	 (assertion-violation 'syntax-cdr "BUG: not a pair" x))))

(define (syntax->list x)
  (cond ((syntax-pair? x)
	 (cons (syntax-car x)
	       (syntax->list (syntax-cdr x))))
	((syntax-null? x)
	 '())
	(else
	 (assertion-violation 'syntax->list "BUG: invalid argument" x))))

(define (syntax-vector->list x)
  (cond ((<stx>? x)
	 (let ((ls (syntax-vector->list (<stx>-expr x)))
	       (m* (<stx>-mark* x))
	       (s* (<stx>-subst* x))
	       (ae* (<stx>-ae* x)))
	   (map (lambda (x)
		  (mkstx x m* s* ae*))
	     ls)))
	((annotation? x)
	 (syntax-vector->list (annotation-expression x)))
	((vector? x)
	 (vector->list x))
	(else
	 (assertion-violation 'syntax-vector->list "BUG: not a syntax vector" x))))


;;;; public interface identifiers handling

(define (identifier? x)
  ;;Return true if X is an  identifier: a syntax object whose expression
  ;;is a symbol.
  ;;
  (and (<stx>? x)
       (let ((expr ($<stx>-expr x)))
	 (symbol? (if (annotation? expr)
		      (annotation-stripped expr)
		    expr)))))

(define* (identifier->symbol x)
  ;;Given an identifier return its symbol expression.
  ;;
  (define (%error)
    (assertion-violation __who__
      "Vicare bug: expected identifier as argument" x))
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

(define (free-identifier=? x y)
  (if (identifier? x)
      (if (identifier? y)
	  (free-id=? x y)
	(assertion-violation 'free-identifier=? "not an identifier" y))
    (assertion-violation 'free-identifier=? "not an identifier" x)))

(define (bound-identifier=? x y)
  (if (identifier? x)
      (if (identifier? y)
	  (bound-id=? x y)
	(assertion-violation 'bound-identifier=? "not an identifier" y))
    (assertion-violation 'bound-identifier=? "not an identifier" x)))


;;;; identifiers: syntactic binding properties

(define* (syntactic-binding-putprop (id identifier?) (key symbol?) value)
  (putprop (id->label/or-error __who__ id id) key value))

(define* (syntactic-binding-getprop (id identifier?) (key symbol?))
  (getprop (id->label/or-error __who__ id id) key))

(define* (syntactic-binding-remprop (id identifier?) (key symbol?))
  (remprop (id->label/or-error __who__ id id) key))

(define* (syntactic-binding-property-list (id identifier?))
  (property-list (id->label/or-error __who__ id id)))


;;;; identifiers: unsafe variants API
;;
;;With the function SET-IDENTIFIER-UNSAFE-VARIANT! and the syntax UNSAFE
;;we allow the  user to select an  unsafe variant of a  function or (why
;;not?)  macro.
;;
;;The unsafe  variant of  a function  is a  function that  (not strictly
;;speaking) neither  validates its arguments  nor its return  value.  As
;;example, the function:
;;
;;   (define* ((string-ref-fx fixnum?) (str string?) (idx fixnum?))
;;     ($char->fixnum ($string-ref str idx)))
;;
;;can be split into:
;;
;;   (define* ((string-ref-fx fixnum?) (str string?) (idx fixnum?))
;;     ($string-ref-fx str idx))
;;
;;   (define ($string-ref-fx str idx)
;;     ($char->fixnum ($string-ref str idx)))
;;
;;where  $STRING-REF-FX  is the  unsafe  variant  of STRING-REF-FX;  the
;;unsafe variants  API allows us to  register this fact, for  use by the
;;expander, as follows:
;;
;;   (begin-for-syntax
;;     (set-identifier-unsafe-variant! #'string-ref-fx
;;       #'$string-ref-fx))
;;
;;so, having imported  the binding STRING-REF-FX, we can  use its unsafe
;;variant as follows:
;;
;;   ((unsafe string-ref-fx) "ciao" 2)
;;
;;without actually importing the binding $STRING-REF-FX.
;;
;;We  use  syntactic  binding  properties  to  implement  this  feature:
;;SET-IDENTIFIER-UNSAFE-VARIANT!   puts  a  property  in  the  syntactic
;;binding, which is later retrieved  by the UNSAFE syntax.  The property
;;value  must  be a  syntax  object  representing an  expression  which,
;;expanded and evaluated, returns the unsafe variant.
;;
;;When specifying  the unsafe variant  of a  function we should  build a
;;syntax  object   representing  an   expression  which,   expanded  and
;;evaluated, returns a  proper function; so that it can  be used in both
;;the expressions:
;;
;;  ((unsafe fx+) 1 2)
;;  (map (unsafe fx+) '(1 2) '(3 4))
;;
;;so for FX+ we should do:
;;
;;  (begin-for-syntax
;;    (set-identifier-unsafe-variant! #'fx+
;;      #'(lambda (a b) ($fx+ a b))))
;;
;;because $FX+ is not a function, rather it is a primitive operation.
;;

(define-constant *UNSAFE-VARIANT-COOKIE*
  'vicare:expander:unsafe-variant)

(define* (set-identifier-unsafe-variant! (safe-id identifier?) (unsafe-expr-stx <stx>?))
  (if (syntactic-binding-getprop safe-id *UNSAFE-VARIANT-COOKIE*)
      (syntax-violation __who__
	"unsafe variant already defined" safe-id unsafe-expr-stx)
    (syntactic-binding-putprop safe-id *UNSAFE-VARIANT-COOKIE* unsafe-expr-stx)))


;;;; identifiers: predicates axiliary functions API
;;
;;Predicate functions, both type  predicates and generic predicates, are
;;used to  validate procedure  arguments and  their return  values.  The
;;predicates  auxiliary  functions API  assumes  that  it is  useful  to
;;associate to certain predicate procedures:
;;
;;*  A procedure  that validates  a tuple  of values  and when  failing:
;;  raises     an    exception     with     condition    object     type
;;  &procedure-argument-violation.
;;
;;*  A procedure  that validates  a tuple  of values  and when  failing:
;;  raises     an    exception     with     condition    object     type
;;  &expression-return-value-violation.
;;
;;Such auxiliary functions are  made available through syntactic binding
;;properties  by  just importing  in  the  current lexical  contour  the
;;identifier bound to the predicate function.
;;
;;As example, we  can associate a procedure  argument auxiliary function
;;to LIST? as follows:
;;
;;   (define (list-procedure-argument who obj)
;;     (if (list? obj)
;;         obj
;;       (procedure-argument-violation who
;;         "expected list object as argument" obj)))
;;
;;   (begin-for-syntax
;;     (set-predicate-procedure-argument-validation! #'list?
;;       #'list-procedure-argument?))
;;
;;   ((predicate-procedure-argument-validation list?) 'hey '(1 2 3))
;;   => (1 2 3)
;;
;;   ((predicate-procedure-argument-validation list?) 'hey '#(1 2 3))
;;   error--> &procedure-argument-violation
;;
;;and we  can associate a return  value auxiliary function to  LIST?  as
;;follows:
;;
;;   (define (list-return-value? who obj)
;;     (if (list? obj)
;;         obj
;;       (expression-return-value-violation who
;;         "expected list object as argument" obj)))
;;
;;   (begin-for-syntax
;;     (set-predicate-return-value-validation! #'list?
;;       #'list-return-value?))
;;
;;    ((predicate-return-value-validation list?) 'hey '(1 2 3))
;;    => (1 2 3)
;;
;;    ((predicate-return-value-validation list?) 'hey '#(1 2 3))
;;    error--> &expression-return-value-violation
;;

(define-constant *PREDICATE-PROCEDURE-ARGUMENT-VALIDATION-COOKIE*
  'vicare:expander:predicate-procedure-argument-validation)

(define-constant *PREDICATE-RETURN-VALUE-VALIDATION-COOKIE*
  'vicare:expander:predicate-return-value-validation)

(define* (set-predicate-procedure-argument-validation! (pred-id identifier?) (validation-stx <stx>?))
  (if (syntactic-binding-getprop pred-id *PREDICATE-PROCEDURE-ARGUMENT-VALIDATION-COOKIE*)
      (syntax-violation __who__
	"predicate procedure argument validation already defined"
	pred-id validation-stx)
    (syntactic-binding-putprop pred-id *PREDICATE-PROCEDURE-ARGUMENT-VALIDATION-COOKIE*
			       validation-stx)))

(define* (set-predicate-return-value-validation! (pred-id identifier?) (validation-stx <stx>?))
  (if (syntactic-binding-getprop pred-id  *PREDICATE-RETURN-VALUE-VALIDATION-COOKIE*)
      (syntax-violation __who__
	"predicate procedure argument validation already defined"
	pred-id validation-stx)
    (syntactic-binding-putprop pred-id *PREDICATE-RETURN-VALUE-VALIDATION-COOKIE*
			       validation-stx)))


;;;; identifiers: expand-time type specification
;;
;;Examples of OBJECT-SPEC:
;;
;;   (define* (fixnum (obj fixnum?))
;;     obj)
;;
;;   (define* (exact-integer (obj exact-integer?))
;;     obj)
;;
;;   (eval-for-expand
;;
;;     (set-identifier-object-spec! #'fixnum
;;       (make-object-spec 'fixnum #'fixnum #'fixnum?))
;;
;;     (set-identifier-object-spec! #'exact-integer
;;       (make-object-spec 'exact-integer #'exact-integer #'exact-integer?))
;;
;;   (set-identifier-object-spec! #'cons
;;     (let ()
;;       (import (vicare system $pairs))
;;
;;       (define (accessor-maker slot-id safe?)
;;         (case-identifiers slot-id
;;           ((car) (if safe? #'car #'$car))
;;           ((cdr) (if safe? #'cdr #'$cdr))
;;           (else
;;            (syntax-violation 'pair
;;              "invalid slot name for accessor creation"
;;              slot-id))))
;;
;;       (define (mutator-maker slot-id safe?)
;;         (case-identifiers slot-id
;;           ((car) (if safe? #'set-car! #'$set-car!))
;;           ((cdr) (if safe? #'set-cdr! #'$set-cdr!))
;;           (else
;;            (syntax-violation 'pair
;;              "invalid slot name for mutation creation" slot-id))))
;;
;;       (make-object-spec 'pair #'cons #'pair? accessor-maker mutator-maker)))
;;
;;     #| end of begin-or-expand |# )
;;
;;Examples of CALLABLE-SPEC:
;;
;;   (define (func a b c)
;;     (* a (+ b c)))
;;
;;   (define (fxfunc a b c)
;;     (fx* a (fx+ b c)))
;;
;;   (eval-for-expand
;;
;;     (define fixnum-spec
;;       (object-spec #'fixnum))
;;
;;     (define exact-integer-spec
;;       (object-spec #'exact-integer))
;;
;;     (set-callable-object-spec! #'func
;;       (make-callable-spec 'func 3 3
;;         (lambda (type-a type-b type-c)
;;           (cond ((and (eq? type-a fixnum-spec)
;;                       (eq? type-b fixnum-spec)
;;                       (eq? type-c fixnum-spec))
;;                  (values #'fxfunc exact-integer-spec))
;;                 (else
;;                  (values #'func #f))))))
;;
;;     #| end of eval-for-expand |# )
;;

(define-constant *EXPAND-TIME-OBJECT-SPEC-COOKIE*
  'vicare:expander:object-spec)

(define-constant *EXPAND-TIME-CALLABLE-SPEC-COOKIE*
  'vicare:expander:callable-spec)

(define-record object-spec
  ;;A type representing  the object type to which  expressions in syntax
  ;;objects  will evaluate.   All the  Scheme  objects are  meant to  be
  ;;representable with this type.
  ;;
  ;;Instances of  this type are meant  to be compared with  EQ?, with #f
  ;;acting as wildcard: it represents any object type.
  ;;
  (name
		;A symbol representing  this type name.  To  be used for
		;meaningful error reporting.
   type-id
		;The  bound identifier  representing  the  name of  this
		;type.  This  identifier has  this very instance  in its
		;syntactic binding property list.
   pred-id
		;An identifier bound to the type predicate.
   accessor-maker
		;False  or  an  accessor  maker  procedure  accepting  2
		;arguments and returning 1 value.  The return value is a
		;syntax  object  evaluating  to a  slot  accessor.   The
		;arguments are: an identifier  representing a slot name;
		;a  boolean, true  if  the requested  accessor is  safe,
		;false if it is unsafe.
   mutator-maker
		;False  or   a  mutator  maker  procedure   accepting  2
		;arguments and returning 1 value.  The return value is a
		;syntax  object  evaluating  to  a  slot  mutator.   The
		;arguments are: an identifier  representing a slot name;
		;a boolean, true if the requested mutator is safe, false
		;if it is unsafe.
   ))

(case-define* public-make-object-spec
  (((name symbol?) (type-id identifier?) (pred-id identifier?))
   (make-object-spec name type-id pred-id #f #f))
  (((name symbol?) (type-id identifier?) (pred-id identifier?)
    (accessor-maker false-or-procedure?)
    (mutator-maker  false-or-procedure?))
   (make-object-spec name type-id pred-id accessor-maker mutator-maker)))

(define-record callable-spec
  ;;A struct type representing a callable form binding, either procedure
  ;;or macro.
  ;;
  (name
		;A symbol  representing this callable name.   To be used
		;for meaningful error reporting.
   min-arity
		;A fixnum  representing the minimum number  of arguments
		;this callable must be applied to.
   max-arity
		;A fixnum or positive  infinity representing the maximum
		;number of arguments this callable must be applied to.
   dispatcher
		;False  or  a  procedure  to be  called  with  the  type
		;signature of a specific callable application.  The type
		;signature  of  a tuple  of  arguments  is the  list  of
		;instances of type OBJECT-SPEC matching the arguments.
		;
		;It is meant  to return two values: the  identifier of a
		;specialised  version  of  this callable  that  is  more
		;suited to be  applied to a tuple of  arguments with the
		;given  type  signature;   an  instance  of  OBJECT-SPEC
		;representing the type of the return value.
   ))

;;; --------------------------------------------------------------------

(define* (set-identifier-object-spec! (type-id identifier?) (spec object-spec?))
  (if (syntactic-binding-getprop type-id *EXPAND-TIME-OBJECT-SPEC-COOKIE*)
      (syntax-violation __who__
	"object specification already defined" type-id spec)
    (syntactic-binding-putprop type-id *EXPAND-TIME-OBJECT-SPEC-COOKIE* spec)))

(define* (identifier-object-spec (type-id identifier?))
  (syntactic-binding-getprop type-id *EXPAND-TIME-OBJECT-SPEC-COOKIE*))

;;; --------------------------------------------------------------------

(define* (set-identifier-callable-spec! (type-id identifier?) (spec callable-spec?))
  (if (syntactic-binding-getprop type-id *EXPAND-TIME-CALLABLE-SPEC-COOKIE*)
      (syntax-violation __who__
	"callable specification already defined" type-id spec)
    (syntactic-binding-putprop type-id *EXPAND-TIME-CALLABLE-SPEC-COOKIE* spec)))

(define* (identifier-callable-spec (type-id identifier?))
  (syntactic-binding-getprop type-id *EXPAND-TIME-CALLABLE-SPEC-COOKIE*))


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
      (lambda () '())))

(define* (syntax-parameter-value (id identifier?))
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

(define* (identifier-bound? (id identifier?))
  (and (id->label id) #t))


;;;; utilities for identifiers

(define (bound-id=? id1 id2)
  ;;Two identifiers  are BOUND-ID=? if they  have the same name  and the
  ;;same set of marks.
  ;;
  (and (eq? (identifier->symbol id1) (identifier->symbol id2))
       (same-marks? ($<stx>-mark* id1) ($<stx>-mark* id2))))

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

(define (bound-id-member? id id*)
  ;;Given an identifier  ID and a list of identifiers  ID*: return #t if
  ;;ID is BOUND-ID=? to one of the identifiers in ID*; else return #f.
  ;;
  (and (pair? id*)
       (or (bound-id=? id ($car id*))
	   (bound-id-member? id ($cdr id*)))))

(define* (identifier-append ctxt . str*)
  ;;Given  the identifier  CTXT  and a  list of  strings  or symbols  or
  ;;identifiers STR*: concatenate all the items in STR*, with the result
  ;;build and return a new identifier in the same context of CTXT.
  ;;
  (datum->stx ctxt
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

(define (bless x)
  ;;Given a raw  sexp, a single syntax object, a  wrapped syntax object,
  ;;an unwrapped  syntax object or  a partly unwrapped syntax  object X:
  ;;return a syntax object representing the input, possibly X itself.
  ;;
  ;;When  X is  a sexp  or a  (partially) unwrapped  syntax object:  raw
  ;;symbols  in X  are considered  references  to bindings  in the  core
  ;;language:  they are  converted to  identifiers having  empty lexical
  ;;contexts.
  ;;
  (mkstx (let recur ((x x))
	   (cond ((<stx>? x)
		  x)
		 ((pair? x)
		  (cons (recur (car x)) (recur (cdr x))))
		 ((symbol? x)
		  (scheme-stx x))
		 ((vector? x)
		  (list->vector (map recur (vector->list x))))
		 ;;If we are here X is a self-evaluating datum.
		 (else x)))
	 '() #;mark*
	 '() #;subst*
	 '() #;ae*
	 ))

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


;;;; utilities for SPLICE-FIRST-EXPAND

(module SPLICE-FIRST-ENVELOPE
  (make-splice-first-envelope
   splice-first-envelope?
   splice-first-envelope-form)

  (define-record splice-first-envelope
    (form))

  #| end of module |# )


(module NON-CORE-MACRO-TRANSFORMER
  (non-core-macro-transformer)
  ;;The  function NON-CORE-MACRO-TRANSFORMER  maps symbols  representing
  ;;non-core macros to their macro transformers.
  ;;
  ;;NOTE This  module is very  long, so it  is split into  multiple code
  ;;pages.  (Marco Maggi; Sat Apr 27, 2013)
  ;;
  (define* (non-core-macro-transformer x)
    (define (%error-invalid-macro)
      (error __who__ "Vicare: internal error: invalid macro" x))
    (assert (symbol? x))
    (case x
      ((define-record-type)		define-record-type-macro)
      ((record-type-and-record?)	record-type-and-record?-macro)
      ((define-struct)			define-struct-macro)
      ((define-condition-type)		define-condition-type-macro)
      ((cond)				cond-macro)
      ((let)				let-macro)
      ((do)				do-macro)
      ((or)				or-macro)
      ((and)				and-macro)
      ((let*)				let*-macro)
      ((let-values)			let-values-macro)
      ((let*-values)			let*-values-macro)
      ((values->list)			values->list-macro)
      ((syntax-rules)			syntax-rules-macro)
      ((quasiquote)			quasiquote-macro)
      ((quasisyntax)			quasisyntax-macro)
      ((with-syntax)			with-syntax-macro)
      ((when)				when-macro)
      ((unless)				unless-macro)
      ((case)				case-macro)
      ((case-identifiers)		case-identifiers-macro)
      ((identifier-syntax)		identifier-syntax-macro)
      ((time)				time-macro)
      ((delay)				delay-macro)
      ((assert)				assert-macro)
      ((guard)				guard-macro)
      ((define-enumeration)		define-enumeration-macro)
      ((let*-syntax)			let*-syntax-macro)
      ((let-constants)			let-constants-macro)
      ((let*-constants)			let*-constants-macro)
      ((letrec-constants)		letrec-constants-macro)
      ((letrec*-constants)		letrec*-constants-macro)
      ((case-define)			case-define-macro)
      ((define*)			define*-macro)
      ((case-define*)			case-define*-macro)
      ((lambda*)			lambda*-macro)
      ((case-lambda*)			case-lambda*-macro)

      ((trace-lambda)			trace-lambda-macro)
      ((trace-define)			trace-define-macro)
      ((trace-let)			trace-let-macro)
      ((trace-define-syntax)		trace-define-syntax-macro)
      ((trace-let-syntax)		trace-let-syntax-macro)
      ((trace-letrec-syntax)		trace-letrec-syntax-macro)

      ((define-syntax-parameter)	define-syntax-parameter-macro)
      ((syntax-parametrise)		syntax-parametrise-macro)

      ((include)			include-macro)
      ((define-integrable)		define-integrable-macro)
      ((define-inline)			define-inline-macro)
      ((define-constant)		define-constant-macro)
      ((define-inline-constant)		define-inline-constant-macro)
      ((define-values)			define-values-macro)
      ((define-constant-values)		define-constant-values-macro)
      ((receive)			receive-macro)
      ((receive-and-return)		receive-and-return-macro)
      ((begin0)				begin0-macro)
      ((xor)				xor-macro)
      ((define-syntax-rule)		define-syntax-rule-macro)
      ((define-auxiliary-syntaxes)	define-auxiliary-syntaxes-macro)
      ((define-syntax*)			define-syntax*-macro)
      ((unwind-protect)			unwind-protect-macro)
      ((with-implicits)			with-implicits-macro)
      ((set-cons!)			set-cons!-macro)

      ;; non-Scheme style syntaxes
      ((while)				while-macro)
      ((until)				until-macro)
      ((for)				for-macro)
      ((define-returnable)		define-returnable-macro)
      ((lambda-returnable)		lambda-returnable-macro)
      ((begin-returnable)		begin-returnable-macro)

      ((parameterize)			parameterize-macro)
      ((parametrise)			parameterize-macro)

      ;; compensations
      ((with-compensations)		with-compensations-macro)
      ((with-compensations/on-error)	with-compensations/on-error-macro)
      ((compensate)			compensate-macro)
      ((push-compensation)		push-compensation-macro)

      ((eol-style)
       (lambda (x)
	 (%allowed-symbol-macro x '(none lf cr crlf nel crnel ls))))

      ((error-handling-mode)
       (lambda (x)
	 (%allowed-symbol-macro x '(ignore raise replace))))

      ((buffer-mode)
       (lambda (x)
	 (%allowed-symbol-macro x '(none line block))))

      ((endianness)
       endianness-macro)

      ((file-options)
       file-options-macro)

      ((... => _ <>
	    else unquote unquote-splicing
	    unsyntax unsyntax-splicing
	    fields mutable immutable parent protocol
	    sealed opaque nongenerative parent-rtd)
       (lambda (expr-stx)
	 (syntax-violation #f "incorrect usage of auxiliary keyword" expr-stx)))

      ((__file__)
       (lambda (stx)
	 (let ((expr (<stx>-expr stx)))
	   (if (annotation? expr)
	       (let ((pos (annotation-textual-position expr)))
		 (if (source-position-condition? pos)
		     (bless
		      `(quote ,(source-position-port-id pos)))
		   (bless
		    `(quote ,(source-code-location)))))
	     (bless
	      `(quote ,(source-code-location)))))))

      ((__line__)
       (lambda (stx)
	 (let ((expr (<stx>-expr stx)))
	   (if (annotation? expr)
	       (let ((pos (annotation-textual-position expr)))
		 (if (source-position-condition? pos)
		     (bless
		      `(quote ,(source-position-line pos)))
		   (bless '(quote #f))))
	     (bless '(quote #f))))))

      (else
       (%error-invalid-macro))))


;;;; module non-core-macro-transformer: DEFINE-AUXILIARY-SYNTAXES

(define (define-auxiliary-syntaxes-macro expr-stx)
  ;;Transformer      function      used     to      expand      Vicare's
  ;;DEFINE-AUXILIARY-SYNTAXES  macros   from  the  top-level   built  in
  ;;environment.   Expand  the contents  of  EXPR-STX;  return a  syntax
  ;;object that must be further expanded.
  ;;
  ;;Using an empty SYNTAX-RULES as  transformer function makes sure that
  ;;whenever an auxiliary syntax is referenced an error is raised.
  ;;
  (syntax-match expr-stx ()
    ((_ ?id* ...)
     (for-all identifier? ?id*)
     (bless
      `(begin
	 ,@(map (lambda (id)
		  `(define-syntax ,id (syntax-rules ())))
	     ?id*))))))


;;;; module non-core-macro-transformer: control structures macros

(define (when-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?test ?expr ?expr* ...)
     (bless `(if ,?test (begin ,?expr . ,?expr*))))))

(define (unless-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?test ?expr ?expr* ...)
     (bless `(if (not ,?test) (begin ,?expr . ,?expr*))))))


;;;; module non-core-macro-transformer: CASE, CASE-IDENTIFIERS

(module (case-macro)
  ;;Transformer  function used  to expand  R6RS's CASE  macros from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;FIXME This should be rewritten to support the model proposed in:
  ;;
  ;;    William   D.   Clinger.    "Rapid  case  dispatch   in  Scheme".
  ;;    Northeastern University.   Proceedings  of the  2006 Scheme  and
  ;;   Functional Programming Workshop.  University of Chicago Technical
  ;;   Report TR-2006-06.
  ;;
  (define (case-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?expr)
       (bless `(let ((t ,?expr))
		 (if #f #f))))
      ((_ ?expr ?clause ?clause* ...)
       (bless
	`(let ((t ,?expr))
	   ,(let recur ((clause  ?clause)
			(clause* ?clause*))
	      (if (null? clause*)
		  (%build-last clause)
		(%build-one clause (recur (car clause*) (cdr clause*))))))))))

  (define (%build-one clause-stx k)
    (syntax-match clause-stx (=>)
      (((?datum* ...) => ?expr)
       (if (options.strict-r6rs)
	   (syntax-violation 'case
	     "invalid usage of auxiliary keyword => in strict R6RS mode"
	     clause-stx)
	 `(if (memv t ',?datum*)
	      (,?expr t)
	    ,k)))
      (((?datum* ...) ?expr ?expr* ...)
       `(if (memv t ',?datum*)
	    (begin ,?expr . ,?expr*)
	  ,k))
      ))

  (define (%build-last clause)
    (syntax-match clause (else)
      ((else ?expr ?expr* ...)
       `(let () #f ,?expr . ,?expr*))
      (_
       (%build-one clause '(if #f #f)))))

  #| end of module: CASE-MACRO |# )

(module (case-identifiers-macro)
  (define-constant __who__
    'case-identifiers)

  (define (case-identifiers-macro expr-stx)
    ;;Transformer  function  used  to expand  Vicare's  CASE-IDENTIFIERS
    ;;macros  from  the  top-level  built in  environment.   Expand  the
    ;;contents of EXPR-STX; return a  syntax object that must be further
    ;;expanded.
    ;;
    (syntax-match expr-stx ()
      ((_ ?expr)
       (bless
	`(let ((t ,?expr))
	   (if #f #f))))
      ((_ ?expr ?clause ?clause* ...)
       (bless
	`(let* ((t ,?expr)
		(p (identifier? t)))
	   ,(let recur ((clause  ?clause)
			(clause* ?clause*))
	      (if (null? clause*)
		  (%build-last expr-stx clause)
		(%build-one expr-stx clause (recur (car clause*) (cdr clause*))))))))
      ))

  (define (%build-one expr-stx clause-stx kont)
    (syntax-match clause-stx (=>)
      (((?datum* ...) => ?proc)
       (if (options.strict-r6rs)
	   (syntax-violation __who__
	     "invalid usage of auxiliary keyword => in strict R6RS mode"
	     clause-stx)
	 `(if ,(%build-test expr-stx ?datum*)
	      (,?proc t)
	    ,kont)))

      (((?datum* ...) ?expr ?expr* ...)
       `(if ,(%build-test expr-stx ?datum*)
	    (begin ,?expr . ,?expr*)
	  ,kont))
      ))

  (define (%build-last expr-stx clause)
    (syntax-match clause (else)
      ((else ?expr ?expr* ...)
       `(let () #f ,?expr . ,?expr*))
      (_
       (%build-one expr-stx clause '(if #f #f)))))

  (define (%build-test expr-stx datum*)
    `(and p (or . ,(map (lambda (datum)
			  (if (identifier? datum)
			      `(free-identifier=? t (syntax ,datum))
			    (syntax-violation __who__
			      "expected identifiers as datums"
			      expr-stx datum)))
		     datum*))))

  #| end of module: CASE-IDENTIFIERS-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-RECORD-TYPE

(module (define-record-type-macro)
  ;;Transformer function used to expand R6RS's DEFINE-RECORD-TYPE macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (define-constant __who__ 'define-record-type)

  (define (define-record-type-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?namespec ?clause* ...)
       (begin
	 (%verify-clauses expr-stx ?clause*)
	 (%do-define-record expr-stx ?namespec ?clause*)))
      ))

;;; --------------------------------------------------------------------

  (define (%do-define-record expr-stx namespec clause*)
    (case-define synner
      ((message)
       (synner message #f))
      ((message subform)
       (syntax-violation __who__ message expr-stx subform)))

    (define-values (foo make-foo foo?)
      (%parse-full-name-spec namespec))
    (define foo-rtd		(%named-gensym foo "-rtd"))
    (define foo-rcd		(%named-gensym foo "-rcd"))
    (define foo-protocol	(gensym))
    (define-values
      (field-names
		;A list of identifiers representing all the field names.
       idx*
		;A list  of fixnums  representing all the  field indexes
		;(zero-based).
       foo-x*
		;A list  of identifiers  representing the  safe accessor
		;names.
       unsafe-foo-x*
		;A list of identifiers  representing the unsafe accessor
		;names.
       mutable-field-names
		;A list  of identifiers  representing the  mutable field
		;names.
       set-foo-idx*
		;A  list  of  fixnums  representing  the  mutable  field
		;indexes (zero-based).
       set-foo-x!*
		;A list of identifiers representing the mutator names.
       unsafe-set-foo-x!*
		;A list  of identifiers representing the  unsafe mutator
		;names.
       )
      (%parse-field-specs foo (%get-fields clause*) synner))

    (define binding-spec
      (%make-binding-spec field-names mutable-field-names
			  foo-x* set-foo-x!*
			  unsafe-foo-x* unsafe-set-foo-x!*))

    ;;Code  for parent  record-type  descriptor  and parent  record-type
    ;;constructor descriptor retrieval.
    (define-values (parent-rtd-code parent-rcd-code)
      (%make-parent-rtd+rcd-code clause* synner))

    ;;Code  for  record-type   descriptor  and  record-type  constructor
    ;;descriptor.
    (define foo-rtd-code
      (%make-rtd-code foo clause* parent-rtd-code synner))
    (define foo-rcd-code
      (%make-rcd-code clause* foo-rtd foo-protocol parent-rcd-code))

    ;;Code for protocol.
    (define protocol-code
      (%get-protocol-code clause* synner))

    (bless
     `(begin
	;;Record type descriptor.
	(define ,foo-rtd ,foo-rtd-code)
	;;Protocol function.
	(define ,foo-protocol ,protocol-code)
	;;Record constructor descriptor.
	(define ,foo-rcd ,foo-rcd-code)
	;;Record instance predicate.
	(define ,foo? (record-predicate ,foo-rtd))
	;;Record instance constructor.
	(define ,make-foo (record-constructor ,foo-rcd))
	;;Safe record fields accessors.
	,@(map (lambda (foo-x idx)
		 `(define ,foo-x (record-accessor ,foo-rtd ,idx (quote ,foo-x))))
	    foo-x* idx*)
	;;Safe record fields mutators (if any).
	,@(map (lambda (set-foo-x! idx)
		 `(define ,set-foo-x! (record-mutator ,foo-rtd ,idx (quote ,set-foo-x!))))
	    set-foo-x!* set-foo-idx*)

	;;Binding for record type name.
	(define-syntax ,foo
	  (cons '$rtd
		(cons (syntax ,foo-rtd)
		      (cons (syntax ,foo-rcd) (quote ,binding-spec)))))

	. ,(if (options.strict-r6rs)
	       '()
	     (%gen-unsafe-accessor+mutator-code foo foo-rtd foo-rcd
						unsafe-foo-x*      idx*
						unsafe-set-foo-x!* set-foo-idx*)))))

;;; --------------------------------------------------------------------

  (define (%gen-unsafe-accessor+mutator-code foo foo-rtd foo-rcd
					     unsafe-foo-x*      idx*
					     unsafe-set-foo-x!* set-foo-idx*)
    (define foo-first-field-offset
      (gensym))
    `((define ,foo-first-field-offset
	;;The field  at index 3  in the RTD is:  the index of  the first
	;;field of  this subtype in the  layout of instances; it  is the
	;;total number of fields of the parent type.
	($struct-ref ,foo-rtd 3))

      ;; Unsafe record fields accessors.
      ,@(map (lambda (unsafe-foo-x idx)
	       (let ((t (gensym)))
		 `(begin
		    (define ,t
		      (fx+ ,idx ,foo-first-field-offset))
		    (define-syntax-rule (,unsafe-foo-x x)
		      ($struct-ref x ,t)))))
	  unsafe-foo-x* idx*)

      ;; Unsafe record fields mutators.
      ,@(map (lambda (unsafe-set-foo-x! idx)
	       (let ((t (gensym)))
		 `(begin
		    (define ,t
		      (fx+ ,idx ,foo-first-field-offset))
		    (define-syntax-rule (,unsafe-set-foo-x! x v)
		      ($struct-set! x ,t v)))))
	  unsafe-set-foo-x!* set-foo-idx*)
      ))

;;; --------------------------------------------------------------------

  (define (%parse-full-name-spec spec)
    ;;Given  a  syntax  object  representing  a  full  record-type  name
    ;;specification: return the name identifier.
    ;;
    (syntax-match spec ()
      ((?foo ?make-foo ?foo?)
       (values ?foo ?make-foo ?foo?))
      (foo
       (identifier? foo)
       (values foo
	       (identifier-append  foo "make-" (syntax->datum foo))
	       (identifier-append  foo foo "?")))
      ))

  (define (%make-rtd-code name clause* parent-rtd-code synner)
    ;;Return a  sexp which,  when evaluated,  will return  a record-type
    ;;descriptor.
    ;;
    (define uid-code
      (let ((clause (%get-clause 'nongenerative clause*)))
	(syntax-match clause ()
	  ((_)
	   (quasiquote (quote (unquote (gensym)))))
	  ((_ ?uid)
	   (identifier? ?uid)
	   (quasiquote (quote (unquote ?uid))))
	  ;;No matching clause found.
	  (#f	#f)
	  (_
	   (synner "expected symbol or no argument in nongenerative clause" clause)))))
    (define sealed?
      (let ((clause (%get-clause 'sealed clause*)))
	(syntax-match clause ()
	  ((_ #t)	#t)
	  ((_ #f)	#f)
	  ;;No matching clause found.
	  (#f		#f)
	  (_
	   (synner "invalid argument in SEALED clause" clause)))))
    (define opaque?
      (let ((clause (%get-clause 'opaque clause*)))
	(syntax-match clause ()
	  ((_ #t)	#t)
	  ((_ #f)	#f)
	  ;;No matching clause found.
	  (#f		#f)
	  (_
	   (synner "invalid argument in OPAQUE clause" clause)))))
    (define fields
      (let ((clause (%get-clause 'fields clause*)))
	(syntax-match clause ()
	  ((_ field-spec* ...)
	   `(quote ,(list->vector
		     (map (lambda (field-spec)
			    (syntax-match field-spec (mutable immutable)
			      ((mutable ?name . ?rest)
			       `(mutable ,?name))
			      ((immutable ?name . ?rest)
			       `(immutable ,?name))
			      (?name
			       `(immutable ,?name))))
		       field-spec*))))
	  ;;No matching clause found.
	  (#f
	   (quote (quote #())))

	  (_
	   (synner "invalid syntax in FIELDS clause" clause)))))
    `(make-record-type-descriptor ',name ,parent-rtd-code
				  ,uid-code ,sealed? ,opaque? ,fields))

  (define (%make-rcd-code clause* foo-rtd foo-protocol parent-rcd-code)
    ;;Return a sexp  which, when evaluated, will  return the record-type
    ;;default constructor descriptor.
    ;;
    `(make-record-constructor-descriptor ,foo-rtd ,parent-rcd-code ,foo-protocol))

  (define (%make-parent-rtd+rcd-code clause* synner)
    ;;Return 2  values: a  sexp which, when  evaluated, will  return the
    ;;parent record-type descriptor; a  sexp which, when evaluated, will
    ;;return the parent record-type default constructor descriptor.
    ;;
    (let ((parent-clause (%get-clause 'parent clause*)))
      (syntax-match parent-clause ()
	;;If there is a PARENT clause insert code that retrieves the RTD
	;;from the parent type name.
	((_ ?name)
	 (identifier? ?name)
	 (values `(record-type-descriptor ,?name)
		 `(record-constructor-descriptor ,?name)))

	;;If there  is no PARENT  clause try to retrieve  the expression
	;;evaluating to the RTD.
	(#f
	 (let ((parent-rtd-clause (%get-clause 'parent-rtd clause*)))
	   (syntax-match parent-rtd-clause ()
	     ((_ ?rtd ?rcd)
	      (values ?rtd ?rcd))

	     ;;If  neither the  PARENT  nor the  PARENT-RTD clauses  are
	     ;;present: just return false.
	     (#f
	      (values #f #f))

	     (_
	      (synner "invalid syntax in PARENT-RTD clause" parent-rtd-clause)))))

	(_
	 (synner "invalid syntax in PARENT clause" parent-clause)))))

  (define (%get-protocol-code clause* synner)
    ;;Return  a  sexp  which,   when  evaluated,  returns  the  protocol
    ;;function.
    ;;
    (let ((clause (%get-clause 'protocol clause*)))
      (syntax-match clause ()
	((_ ?expr)
	 ?expr)

	;;No matching clause found.
	(#f	#f)

	(_
	 (synner "invalid syntax in PROTOCOL clause" clause)))))

  (define (%get-fields clause*)
    ;;Return   a  list   of  syntax   objects  representing   the  field
    ;;specifications.
    ;;
    (syntax-match clause* (fields)
      (()
       '())
      (((fields ?field-spec* ...) . _)
       ?field-spec*)
      ((_ . ?rest)
       (%get-fields ?rest))))

  (define (%parse-field-specs foo field-clause* synner)
    ;;Given the  arguments of the  fields specification clause  return 4
    ;;values:
    ;;
    ;;1..The list of identifiers representing all the field names.
    ;;
    ;;2..The  list  of  fixnums  representings all  the  field  relative
    ;;   indexes (zero-based).
    ;;
    ;;3..A list of identifiers representing the safe accessor names.
    ;;
    ;;4..A list of identifiers representing the unsafe accessor names.
    ;;
    ;;5..The list of identifiers representing the mutable field names.
    ;;
    ;;6..The list  of fixnums  representings the mutable  field relative
    ;;   indexes (zero-based).
    ;;
    ;;7..A list of identifiers representing the safe mutator names.
    ;;
    ;;8..A list of identifiers representing the unsafe mutator names.
    ;;
    ;;Here we assume that FIELD-CLAUSE* is null or a proper list.
    ;;
    (define (gen-safe-accessor-name x)
      (identifier-append  foo foo "-" x))
    (define (gen-unsafe-accessor-name x)
      (identifier-append  foo "$" foo "-" x))
    (define (gen-safe-mutator-name x)
      (identifier-append  foo foo "-" x "-set!"))
    (define (gen-unsafe-mutator-name x)
      (identifier-append  foo "$" foo "-" x "-set!"))
    (let loop ((field-clause*		field-clause*)
	       (i			0)
	       (field*			'())
	       (idx*			'())
	       (accessor*		'())
	       (unsafe-accessor*	'())
	       (mutable-field*		'())
	       (mutable-idx*		'())
	       (mutator*		'())
	       (unsafe-mutator*		'()))
      (syntax-match field-clause* (mutable immutable)
	(()
	 (values (reverse field*) (reverse idx*) (reverse accessor*) (reverse unsafe-accessor*)
		 (reverse mutable-field*) (reverse mutable-idx*) (reverse mutator*) (reverse unsafe-mutator*)))

	(((mutable   ?name ?accessor ?mutator) . ?rest)
	 (and (identifier? ?name)
	      (identifier? ?accessor)
	      (identifier? ?mutator))
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons ?accessor accessor*)	(cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       (cons ?name mutable-field*)	(cons i mutable-idx*)
	       (cons ?mutator mutator*)		(cons (gen-unsafe-mutator-name  ?name) unsafe-mutator*)))

	(((immutable ?name ?accessor) . ?rest)
	 (and (identifier? ?name)
	      (identifier? ?accessor))
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons ?accessor accessor*)	(cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       mutable-field*			mutable-idx*
	       mutator*				unsafe-mutator*))

	(((mutable   ?name) . ?rest)
	 (identifier? ?name)
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons (gen-safe-accessor-name   ?name) accessor*)
	       (cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       (cons ?name mutable-field*)	(cons i mutable-idx*)
	       (cons (gen-safe-mutator-name    ?name) mutator*)
	       (cons (gen-unsafe-mutator-name  ?name) unsafe-mutator*)))

	(((immutable ?name) . ?rest)
	 (identifier? ?name)
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons (gen-safe-accessor-name   ?name) accessor*)
	       (cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       mutable-field*			mutable-idx*
	       mutator*				unsafe-mutator*))

	((?name . ?rest)
	 (identifier? ?name)
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons (gen-safe-accessor-name   ?name) accessor*)
	       (cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       mutable-field*			mutable-idx*
	       mutator*				unsafe-mutator*))

	((?spec . ?rest)
	 (synner "invalid field specification in DEFINE-RECORD-TYPE syntax"
		 ?spec)))))

  (module (%make-binding-spec)
    (import R6RS-RECORD-TYPE-SPEC)

    (define (%make-binding-spec field-names mutable-field-names
				foo-x* set-foo-x!*
				unsafe-foo-x* unsafe-set-foo-x!*)

      ;;A sexp which will be BLESSed  in the output code.  The sexp will
      ;;evaluate to an alist in which: keys are symbols representing all
      ;;the  field  names; values  are  identifiers  bound to  the  safe
      ;;accessors.
      (define foo-fields-safe-accessors-table
	(%make-alist field-names foo-x*))

      ;;A sexp which will be BLESSed  in the output code.  The sexp will
      ;;evaluate to  an alist  in which:  keys are  symbols representing
      ;;mutable  field  names;  values  are identifiers  bound  to  safe
      ;;mutators.
      (define foo-fields-safe-mutators-table
	(%make-alist mutable-field-names set-foo-x!*))

      ;;A sexp which will be BLESSed  in the output code.  The sexp will
      ;;evaluate to an alist in which: keys are symbols representing all
      ;;the  field names;  values are  identifiers bound  to the  unsafe
      ;;accessors.
      (define foo-fields-unsafe-accessors-table
	(%make-alist field-names unsafe-foo-x*))

      ;;A sexp which will be BLESSed  in the output code.  The sexp will
      ;;evaluate to  an alist  in which:  keys are  symbols representing
      ;;mutable  field names;  values  are identifiers  bound to  unsafe
      ;;mutators.
      (define foo-fields-unsafe-mutators-table
	(%make-alist mutable-field-names unsafe-set-foo-x!*))

      (if (options.strict-r6rs)
	  (make-r6rs-record-type-spec foo-fields-safe-accessors-table
				      foo-fields-safe-mutators-table
				      #f #f)
	(make-r6rs-record-type-spec foo-fields-safe-accessors-table
				    foo-fields-safe-mutators-table
				    foo-fields-unsafe-accessors-table
				    foo-fields-unsafe-mutators-table)))

    (define (%make-alist key-id* operator-id*)
      (map (lambda (key-id operator-id)
	     (cons (syntax->datum key-id) operator-id))
	key-id* operator-id*))

    #| end of module: %MAKE-BINDING-SPEC |# )

;;; --------------------------------------------------------------------

  (module (%verify-clauses)

    (define (%verify-clauses expr-stx cls*)
      (define VALID-KEYWORDS
	(map bless
	  '(fields parent parent-rtd protocol sealed opaque nongenerative)))
      (let loop ((cls*  cls*)
		 (seen* '()))
	(unless (null? cls*)
	  (syntax-match (car cls*) ()
	    ((?kwd . ?rest)
	     (cond ((or (not (identifier? ?kwd))
			(not (%free-id-member? ?kwd VALID-KEYWORDS)))
		    (stx-error ?kwd "not a valid DEFINE-RECORD-TYPE keyword"))
		   ((bound-id-member? ?kwd seen*)
		    (syntax-violation __who__
		      "invalid duplicate clause in DEFINE-RECORD-TYPE"
		      expr-stx ?kwd))
		   (else
		    (loop (cdr cls*) (cons ?kwd seen*)))))
	    (?cls
	     (stx-error ?cls "malformed define-record-type clause"))
	    ))))

    (define (%free-id-member? x ls)
      (and (pair? ls)
	   (or (free-id=? x (car ls))
	       (%free-id-member? x (cdr ls)))))

    #| end of module: %VERIFY-CLAUSES |# )

  (define (%get-clause sym clause*)
    ;;Given a symbol SYM representing the  name of a clause and a syntax
    ;;object  CLAUSE*  representing  the clauses:  search  the  selected
    ;;clause and return it as syntax object.  When no matching clause is
    ;;found: return false.
    ;;
    (let next ((id       (bless sym))
	       (clause*  clause*))
      (syntax-match clause* ()
	(()
	 #f)
	(((?key . ?rest) . ?clause*)
	 (if (free-id=? id ?key)
	     `(,?key . ,?rest)
	   (next id ?clause*))))))

  (define (%named-gensym foo suffix)
    (gensym (string-append
	     (symbol->string (syntax->datum foo))
	     suffix)))

  #| end of module: DEFINE-RECORD-TYPE-MACRO |# )


;;;; module non-core-macro-transformer: RECORD-TYPE-AND-RECORD?

(define (record-type-and-record?-macro expr-stx)
  ;;Transformer function used to expand Vicare's RECORD-TYPE-AND-RECORD?
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?type-name ?record)
     (identifier? ?type-name)
     (bless
      `(record-and-rtd? ,?record (record-type-descriptor ,?type-name))))
    ))


;;;; module non-core-macro-transformer: DEFINE-CONDITION-TYPE

(define (define-condition-type-macro expr-stx)
  ;;Transformer  function  used  to  expand  R6RS  RECORD-CONDITION-TYPE
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((?ctxt ?name ?super ?constructor ?predicate (?field* ?accessor*) ...)
     (and (identifier? ?name)
	  (identifier? ?super)
	  (identifier? ?constructor)
	  (identifier? ?predicate)
	  (for-all identifier? ?field*)
	  (for-all identifier? ?accessor*))
     (let ((aux-accessor* (map (lambda (x)
				 (gensym))
			    ?accessor*)))
       (bless
	`(module (,?name ,?constructor ,?predicate . ,?accessor*)
	   (define-record-type (,?name ,?constructor ,(gensym))
	     (parent ,?super)
	     (fields ,@(map (lambda (field aux)
			      `(immutable ,field ,aux))
			 ?field* aux-accessor*))
	     (nongenerative)
	     (sealed #f)
	     (opaque #f))
	   (define ,?predicate
	     ;;Remember  that the  predicate has  to recognise  a simple
	     ;;condition object embedded in a compound condition object.
	     (condition-predicate (record-type-descriptor ,?name)))
	   ,@(map
		 (lambda (accessor aux)
		   `(define ,accessor
		      ;;Remember  that  the  accessor has  to  access  a
		      ;;simple condition  object embedded in  a compound
		      ;;condition object.
		      (condition-accessor (record-type-descriptor ,?name) ,aux)))
	       ?accessor* aux-accessor*)
	   #| end of module |# )
	)))
    ))


;;;; module non-core-macro-transformer: PARAMETERIZE and PARAMETRISE

(define (parameterize-macro expr-stx)
  ;;Transformer  function used  to expand  Vicare's PARAMETERIZE  macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  ;;Notice that  MAKE-PARAMETER is  a primitive function  implemented in
  ;;"ikarus.compiler.sls"  by   "E-make-parameter".   Under   Vicare,  a
  ;;parameter function  can be  called with  0, 1  or 2  arguments:
  ;;
  ;;* When called with 1 argument: it returns the parameter's value.
  ;;
  ;;* When called with 2 arguments:  it sets the parameter's value after
  ;;  checking the new value with the guard function (if any).
  ;;
  ;;*  When called  with  3  arguments: it  sets  the parameter's  value
  ;;   optionally checking  the new  value with  the guard  function (if
  ;;  any).
  ;;
  ;;Under Vicare,  PARAMETERIZE applies  the guard  function to  the new
  ;;value only the first  time it is set; if the  control flow exits and
  ;;returns multiple times beacuse  escaping continuations are used, the
  ;;guard function is  no more applied; this is achieved  by setting the
  ;;flag variable GUARD?.
  ;;
  (syntax-match expr-stx ()
    ((_ () ?body ?body* ...)
     (bless
      `(let () ,?body . ,?body*)))

    ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
     (let ((lhs* (generate-temporaries ?lhs*))
	   (rhs* (generate-temporaries ?rhs*)))
       (bless
	`((lambda ,(append lhs* rhs*)
	    (let* ((guard? #t) ;apply the guard function only the first time
		   (swap   (lambda ()
			     ,@(map (lambda (lhs rhs)
				      `(let ((t (,lhs)))
					 (,lhs ,rhs guard?)
					 (set! ,rhs t)))
				 lhs* rhs*)
			     (set! guard? #f))))
	      (dynamic-wind
		  swap
		  (lambda () ,?body . ,?body*)
		  swap)))
	  ,@(append ?lhs* ?rhs*)))))
    ))


;;;; module non-core-macro-transformer: UNWIND-PROTECT

(define (unwind-protect-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  UNWIND-PROTECT macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  ;;Not a  general UNWIND-PROTECT for Scheme,  but fine where we  do not
  ;;make the body return continuations to  the caller and then come back
  ;;again and again, calling CLEANUP multiple times.
  ;;
  (syntax-match expr-stx ()
    ((_ ?body ?cleanup0 ?cleanup* ...)
     (bless
      `(let ((cleanup (lambda () ,?cleanup0 ,@?cleanup*)))
	 (with-exception-handler
	     (lambda (E)
	       (cleanup)
	       (raise E))
	   (lambda ()
	     (begin0
		 ,?body
	       (cleanup)))))))
    ))


;;;; module non-core-macro-transformer: WITH-IMPLICITS

(define (with-implicits-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  WITH-IMPLICITS macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (define (%make-bindings ctx ids)
    (map (lambda (id)
	   `(,id (datum->syntax ,ctx (quote ,id))))
      ids))

  (syntax-match expr-stx ()

    ((_ () ?body0 ?body* ...)
     (bless
      `(begin ,?body0 . ,?body*)))

    ((_ ((?ctx ?symbol0 ?symbol* ...))
	?body0 ?body* ...)
     (let ((BINDINGS (%make-bindings ?ctx (cons ?symbol0 ?symbol*))))
       (bless
	`(with-syntax ,BINDINGS ,?body0 . ,?body*))))

    ((_ ((?ctx ?symbol0 ?symbol* ...) . ?other-clauses)
	?body0 ?body* ...)
     (let ((BINDINGS (%make-bindings ?ctx (cons ?symbol0 ?symbol*))))
       (bless
	`(with-syntax ,BINDINGS (with-implicits ,?other-clauses ,?body0 . ,?body*)))))

    ))


;;;; module non-core-macro-transformer: SET-CONS!

(define (set-cons!-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  SET-CONS! macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?id ?obj)
     (identifier? ?id)
     (bless `(set! ,?id (cons ,?obj ,?id))))
    ))


;;;; module non-core-macro-transformer: compensations

(module (with-compensations/on-error-macro
	 with-compensations-macro)

  (define (with-compensations/on-error-macro expr-stx)
    ;;Transformer     function      used     to      expand     Vicare's
    ;;WITH-COMPENSATIONS/ON-ERROR  macros from  the  top-level built  in
    ;;environment.   Expand the  contents of  EXPR-STX; return  a syntax
    ;;object that must be further expanded.
    ;;
    (syntax-match expr-stx ()
      ((_ ?body0 ?body* ...)
       (bless
	`(let ,(%make-store-binding)
	   (parametrise ((compensations store))
	     ,(%make-with-exception-handler ?body0 ?body*)))))
      ))

  (define (with-compensations-macro expr-stx)
    ;;Transformer  function used  to expand  Vicare's WITH-COMPENSATIONS
    ;;macros  from  the  top-level  built in  environment.   Expand  the
    ;;contents of EXPR-STX; return a  syntax object that must be further
    ;;expanded.
    ;;
    (syntax-match expr-stx ()
      ((_ ?body0 ?body* ...)
       (bless
	`(let ,(%make-store-binding)
	   (parametrise ((compensations store))
	     (begin0
		 ,(%make-with-exception-handler ?body0 ?body*)
	       ;;Better  run  the  cleanup   compensations  out  of  the
	       ;;WITH-EXCEPTION-HANDLER.
	       (run-compensations-store store))))))
      ))

  (define (%make-store-binding)
    '((store (let ((stack '()))
	       (case-lambda
		(()
		 stack)
		((false/thunk)
		 (if false/thunk
		     (set! stack (cons false/thunk stack))
		   (set! stack '()))))))))

  (define (%make-with-exception-handler body0 body*)
    ;;We really have to close the handler upon the STORE function, it is
    ;;wrong to access the COMPENSATIONS parameter from the handler.  The
    ;;dynamic environment  is synchronised with continuations:  when the
    ;;handler is called by  RAISE or RAISE-CONTINUABLE, the continuation
    ;;is the one of the RAISE or RAISE-CONTINUABLE forms.
    ;;
    `(with-exception-handler
	 (lambda (E)
	   (run-compensations-store store)
	   (raise E))
       (lambda ()
	 ,body0 ,@body*)))

  #| end of module |# )

(define (push-compensation-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  PUSH-COMPENSATION
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?release0 ?release* ...)
     (bless
      `(push-compensation-thunk (lambda () ,?release0 ,@?release*))))
    ))

(define (compensate-macro expr-stx)
  ;;Transformer function used to  expand Vicare's COMPENSATE macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (define-constant __who__ 'compensate)
  (define (%synner message subform)
    (syntax-violation __who__ message expr-stx subform))
  (syntax-match expr-stx ()
    ((_ ?alloc0 ?form* ...)
     (let ((free #f))
       (define alloc*
	 (let recur ((form-stx ?form*))
	   (syntax-match form-stx (with)
	     (((with ?release0 ?release* ...))
	      (begin
		(set! free `(push-compensation ,?release0 ,@?release*))
		'()))

	     (()
	      (%synner "invalid compensation syntax: missing WITH keyword"
		       form-stx))

	     (((with))
	      (%synner "invalid compensation syntax: empty WITH keyword"
		       (bless '(with))))

	     ((?alloc ?form* ...)
	      (cons ?alloc (recur ?form*)))
	     )))
       (bless
	`(begin0 (begin ,?alloc0 . ,alloc*) ,free))))
    ))


;;;; module non-core-macro-transformer: DEFINE-STRUCT

(module (define-struct-macro)
  ;;Transformer function  used to  expand Vicare's  DEFINE-STRUCT macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (define (define-struct-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ (?name ?maker ?predicate) (?field* ...))
       (%build-output-form ?name ?maker ?predicate ?field*))
      ((_ ?name (?field* ...))
       (%build-output-form ?name #f #f ?field*))
      ))

  (define (%build-output-form type-id maker-id predicate-id field-name-id*)
    (let* ((string->id		(lambda (str)
				  (datum->stx type-id (string->symbol str))))
	   (namestr		(symbol->string (identifier->symbol type-id)))
	   (field-sym*		(map identifier->symbol field-name-id*))
	   (field-str*		(map symbol->string field-sym*))
	   (rtd			(datum->stx type-id (make-struct-type namestr field-sym*)))
	   (constructor-id	(or maker-id     (string->id (string-append "make-" namestr))))
	   (predicate-id	(or predicate-id (string->id (string-append namestr "?"))))
	   (field-idx*		(enumerate field-name-id*)))

      (define getter-id*
	(map (lambda (x)
	       (string->id (string-append namestr "-" x)))
	  field-str*))

      (define setter-id*
	(map (lambda (x)
	       (string->id (string-append "set-" namestr "-" x "!")))
	  field-str*))

      (define unsafe-getter-id*
	(map (lambda (x)
	       (string->id (string-append "$" namestr "-" x)))
	  field-str*))

      (define unsafe-setter-id*
	(map (lambda (x)
	       (string->id (string-append "$set-" namestr "-" x "!")))
	  field-str*))

      (define getter-sexp*
	(map (lambda (getter-id unsafe-getter-id)
	       `(define (,getter-id stru)
		  (if ($struct/rtd? stru ',rtd)
		      (,unsafe-getter-id stru)
		    (assertion-violation ',getter-id
		      "not a struct of required type as struct getter argument"
		      stru ',rtd))))
	  getter-id* unsafe-getter-id*))

      (define setter-sexp*
	(map (lambda (setter-id unsafe-setter-id)
	       `(define (,setter-id stru val)
		  (if ($struct/rtd? stru ',rtd)
		      (,unsafe-setter-id stru val)
		    (assertion-violation ',setter-id
		      "not a struct of required type as struct setter argument"
		      stru ',rtd))))
	  setter-id* unsafe-setter-id*))

      (define unsafe-getter-sexp*
	(map (lambda (unsafe-getter-id field-idx)
	       `(define-syntax ,unsafe-getter-id
		  (syntax-rules ()
		    ((_ ?stru)
		     ($struct-ref ?stru ,field-idx)))))
	  unsafe-getter-id* field-idx*))

      (define unsafe-setter-sexp*
	(map (lambda (unsafe-setter-id field-idx)
	       `(define-syntax ,unsafe-setter-id
		  (syntax-rules ()
		    ((_ ?stru ?val)
		     ($struct-set! ?stru ,field-idx ?val)))))
	  unsafe-setter-id* field-idx*))

      (bless
       `(begin
	  (define-syntax ,type-id (cons '$rtd ',rtd))
	  (define (,constructor-id ,@field-name-id*)
	    (let ((S ($struct ',rtd ,@field-name-id*)))
	      (if ($struct-rtd-destructor ',rtd) ;destructor
		  ($struct-guardian S)
		S)))
	  (define (,predicate-id obj)
	    ($struct/rtd? obj ',rtd))
	  ,@getter-sexp*
	  ,@setter-sexp*
	  ,@unsafe-getter-sexp*
	  ,@unsafe-setter-sexp*)
       )))

  (define (enumerate ls)
    (let recur ((i 0) (ls ls))
      (if (null? ls)
	  '()
	(cons i (recur (+ i 1) (cdr ls))))))

  #| end of module: DEFINE-STRUCT-MACRO |# )


;;;; module non-core-macro-transformer: SYNTAX-RULES, DEFINE-SYNTAX-RULE

(define (syntax-rules-macro expr-stx)
  ;;Transformer function  used to  expand R6RS SYNTAX-RULES  macros from
  ;;the  top-level  built  in  environment.   Process  the  contents  of
  ;;EXPR-STX; return a syntax object that needs to be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?literal* ...)
	(?pattern* ?template*)
	...)
     (begin
       (%verify-literals ?literal* expr-stx)
       (bless
	`(lambda (x)
	   (syntax-case x ,?literal*
	     ,@(map (lambda (pattern template)
		      (syntax-match pattern ()
			((_ . ??rest)
			 `((g . ,??rest)
			   (syntax ,template)))
			(_
			 (syntax-violation #f
			   "invalid syntax-rules pattern"
			   expr-stx pattern))))
		 ?pattern* ?template*))))))))

(define (define-syntax-rule-macro expr-stx)
  ;;Transformer  function  used  to expand  Vicare's  DEFINE-SYNTAX-RULE
  ;;macros  from  the  top-level  built  in  environment.   Process  the
  ;;contents  of EXPR-STX;  return  a  syntax object  that  needs to  be
  ;;further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?name ?arg* ... . ?rest) ?body0 ?body* ...)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name
	 (syntax-rules ()
	   ((_ ,@?arg* . ,?rest)
	    (begin ,?body0 ,@?body*))))))
    ))


;;;; module non-core-macro-transformer: DEFINE-SYNTAX*

(define (define-syntax*-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  DEFINE-SYNTAX* macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name (syntax-rules ()))))

    ((_ ?name ?expr)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name ,?expr)))

    ((_ (?name ?stx) ?body0 ?body* ...)
     (and (identifier? ?name)
	  (identifier? ?stx))
     (let ((SYNNER (datum->syntax ?name 'synner)))
       (bless
	`(define-syntax ,?name
	   (lambda (,?stx)
	     (fluid-let-syntax
		 ((__who__ (identifier-syntax (quote ,?name))))
	       (letrec
		   ((,SYNNER (case-lambda
			      ((message)
			       (,SYNNER message #f))
			      ((message subform)
			       (syntax-violation __who__ message ,?stx subform)))))
		 ,?body0 ,@?body*)))))))
    ))


;;;; module non-core-macro-transformer: WITH-SYNTAX

(define (with-syntax-macro expr-stx)
  ;;Transformer function used to expand R6RS WITH-SYNTAX macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;A WITH-SYNTAX form:
  ;;
  ;;   (with-syntax ((?pat0 ?expr0)
  ;;                 (?pat1 ?expr1))
  ;;     ?body0 ?body ...)
  ;;
  ;;is expanded as follows:
  ;;
  ;;   (syntax-case ?expr0 ()
  ;;     (?pat0
  ;;      (syntax-case ?expr1 ()
  ;;       (?pat1
  ;;        (let () ?body0 ?body ...))
  ;;       (_
  ;;        (assertion-violation ---))))
  ;;     (_
  ;;      (assertion-violation ---)))
  ;;
  (syntax-match expr-stx ()
    ((_ ((?pat* ?expr*) ...) ?body ?body* ...)
     (let ((idn* (let recur ((pat* ?pat*))
		   (if (null? pat*)
		       '()
		     (receive (pat idn*)
			 (convert-pattern (car pat*) '())
		       (append idn* (recur (cdr pat*))))))))
       (%verify-formals-syntax (map car idn*) expr-stx)
       (let ((t* (generate-temporaries ?expr*)))
	 (bless
	  `(let ,(map list t* ?expr*)
	     ,(let recur ((pat* ?pat*)
			  (t*   t*))
		(if (null? pat*)
		    `(let () ,?body . ,?body*)
		  `(syntax-case ,(car t*) ()
		     (,(car pat*)
		      ,(recur (cdr pat*) (cdr t*)))
		     (_
		      (assertion-violation 'with-syntax
			"pattern does not match value"
			',(car pat*) ,(car t*)))))))))))
    ))


;;;; module non-core-macro-transformer: IDENTIFIER-SYNTAX

(define (identifier-syntax-macro stx)
  ;;Transformer function  used to  expand R6RS  IDENTIFIER-SYNTAX macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match stx (set!)
    ((_ ?expr)
     (bless
      `(lambda (x)
	 (syntax-case x ()
	   (??id
	    (identifier? (syntax ??id))
	    (syntax ,?expr))
	   ((??id ??expr* ...)
	    (identifier? (syntax ??id))
	    (cons (syntax ,?expr) (syntax (??expr* ...))))
	   ))))

    ((_ (?id1
	 ?expr1)
	((set! ?id2 ?expr2)
	 ?expr3))
     (and (identifier? ?id1)
	  (identifier? ?id2)
	  (identifier? ?expr2))
     (bless
      `(make-variable-transformer
	(lambda (x)
	  (syntax-case x (set!)
	    (??id
	     (identifier? (syntax ??id))
	     (syntax ,?expr1))
	    ((set! ??id ,?expr2)
	     (syntax ,?expr3))
	    ((??id ??expr* ...)
	     (identifier? (syntax ??id))
	     (syntax (,?expr1 ??expr* ...))))))))
    ))


;;;; module non-core-macro-transformer: LET, LET*, TRACE-LET

(define (let-macro expr-stx)
  ;;Transformer  function  used  to  expand R6RS  LET  macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
     (if (valid-bound-ids? ?lhs*)
	 (bless
	  `((lambda ,?lhs*
	      ,?body . ,?body*) . ,?rhs*))
       (%error-invalid-formals-syntax expr-stx ?lhs*)))

    ((_ ?recur ((?lhs* ?rhs*) ...) ?body ?body* ...)
     (identifier? ?recur)
     (if (valid-bound-ids? ?lhs*)
	 (bless
	  `((letrec ((,?recur (lambda ,?lhs*
				,?body . ,?body*)))
	      ,?recur) . ,?rhs*))
       (%error-invalid-formals-syntax expr-stx ?lhs*)))
    ))

(define (let*-macro expr-stx)
  ;;Transformer  function  used to  expand  R6RS  LET* macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
     (for-all identifier? ?lhs*)
     (bless
      (let recur ((x* (map list ?lhs* ?rhs*)))
	(if (null? x*)
	    `(let () ,?body . ,?body*)
	  `(let (,(car x*)) ,(recur (cdr x*)))))))
    ))

(define (trace-let-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  TRACE-LET macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?recur ((?lhs* ?rhs*) ...) ?body ?body* ...)
     (identifier? ?recur)
     (if (valid-bound-ids? ?lhs*)
	 (bless
	  `((letrec ((,?recur (trace-lambda ,?recur ,?lhs*
					    ,?body . ,?body*)))
	      ,?recur) . ,?rhs*))
       (%error-invalid-formals-syntax expr-stx ?lhs*)))
    ))


;;;; module non-core-macro-transformer: LET-VALUES

(module (let-values-macro)
  ;;Transformer function used to expand  R6RS LET-VALUES macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;A LET-VALUES syntax like:
  ;;
  ;;   (let-values (((a b c) rhs0)
  ;;                ((d e f) rhs1))
  ;;     ?body0 ?body ...)
  ;;
  ;;is expanded to:
  ;;
  ;;   (call-with-values
  ;;       (lambda () rhs0)
  ;;     (lambda (G.a G.b G.c)
  ;;       (call-with-values
  ;;           (lambda () rhs1)
  ;;         (lambda (G.d G.e G.f)
  ;;           (let ((a G.a) (b G.b) (c G.c)
  ;;                 (c G.c) (d G.d) (e G.e))
  ;;             ?body0 ?body)))))
  ;;
  (define-constant __who__ 'let-values)

  (define (let-values-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ () ?body ?body* ...)
       (cons* (bless 'let) '() ?body ?body*))

      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       (bless
	(let recur ((lhs*  ?lhs*)
		    (rhs*  ?rhs*)
		    (old*  '())
		    (new*  '()))
	  (if (null? lhs*)
	      `(let ,(map list old* new*)
		 ,?body . ,?body*)
	    (syntax-match (car lhs*) ()
	      ((?formal* ...)
	       (receive (y* old* new*)
		   (rename* ?formal* old* new* expr-stx)
		 `(call-with-values
		      (lambda () ,(car rhs*))
		    (lambda ,y*
		      ,(recur (cdr lhs*) (cdr rhs*) old* new*)))))

	      ((?formal* ... . ?rest-formal)
	       (let*-values
		   (((y  old* new*) (rename  ?rest-formal old* new* expr-stx))
		    ((y* old* new*) (rename* ?formal*     old* new* expr-stx)))
		 `(call-with-values
		      (lambda () ,(car rhs*))
		    (lambda ,(append y* y)
		      ,(recur (cdr lhs*) (cdr rhs*)
			      old* new*)))))
	      (others
	       (syntax-violation __who__ "malformed bindings" expr-stx others)))))))
      ))

  (define (rename formal old* new* expr-stx)
    (unless (identifier? formal)
      (syntax-violation __who__ "not an indentifier" expr-stx formal))
    (when (bound-id-member? formal old*)
      (syntax-violation __who__ "duplicate binding" expr-stx formal))
    (let ((y (gensym (syntax->datum formal))))
      (values y (cons formal old*) (cons y new*))))

  (define (rename* formal* old* new* expr-stx)
    (if (null? formal*)
	(values '() old* new*)
      (let*-values
	  (((formal  old* new*) (rename  (car formal*) old* new* expr-stx))
	   ((formal* old* new*) (rename* (cdr formal*) old* new* expr-stx)))
	(values (cons formal formal*) old* new*))))

  #| end of module: LET-VALUES-MACRO |# )


;;;; module non-core-macro-transformer: LET*-VALUES

(module (let*-values-macro)
  ;;Transformer function used to expand R6RS LET*-VALUES macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;A LET*-VALUES syntax like:
  ;;
  ;;   (let*-values (((a b c) rhs0)
  ;;                 ((d e f) rhs1))
  ;;     ?body0 ?body ...)
  ;;
  ;;is expanded to:
  ;;
  ;;   (call-with-values
  ;;       (lambda () rhs0)
  ;;     (lambda (a b c)
  ;;       (call-with-values
  ;;           (lambda () rhs1)
  ;;         (lambda (d e f)
  ;;           (begin ?body0 ?body)))))
  ;;
  (define-constant __who__ 'let*-values)

  (define (let*-values-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ () ?body ?body* ...)
       (cons* (bless 'let) '() ?body ?body*))

      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       (bless
	(let recur ((lhs* ?lhs*)
		    (rhs* ?rhs*))
	  (if (null? lhs*)
	      `(begin ,?body . ,?body*)
	    (syntax-match (car lhs*) ()
	      ((?formal* ...)
	       (begin
		 (check ?formal* expr-stx)
		 `(call-with-values
		      (lambda () ,(car rhs*))
		    (lambda ,?formal*
		      ,(recur (cdr lhs*) (cdr rhs*))))))

	      ((?formal* ... . ?rest-formal)
	       (begin
		 (check (cons ?rest-formal ?formal*) expr-stx)
		 `(call-with-values
		      (lambda () ,(car rhs*))
		    (lambda ,(append ?formal* ?rest-formal)
		      ,(recur (cdr lhs*) (cdr rhs*))))))

	      (others
	       (syntax-violation __who__ "malformed bindings" expr-stx others)))))))
      ))

  (define (check x* expr-stx)
    (unless (null? x*)
      (let ((x (car x*)))
	(unless (identifier? x)
	  (syntax-violation __who__ "not an identifier" expr-stx x))
	(check (cdr x*) expr-stx)
	(when (bound-id-member? x (cdr x*))
	  (syntax-violation __who__ "duplicate identifier" expr-stx x)))))

  #| end of module: LET*-VALUES-MACRO |# )


;;;; module non-core-macro-transformer: VALUES->LIST-MACRO

(define (values->list-macro expr-stx)
  ;;Transformer  function used  to expand  Vicare's VALUES->LIST  macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?expr)
     (bless
      `(call-with-values
	   (lambda () ,?expr)
	 list)))))


;;;; module non-core-macro-transformer: LET*-SYNTAX

(define (let*-syntax-macro expr-stx)
  ;;Transformer function used to expand Vicare's LET*-SYNTAX macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(begin ,?body . ,?body*)))
    ;;Single binding.
    ((_ ((?lhs ?rhs)) ?body ?body* ...)
     (bless
      `(let-syntax ((,?lhs ,?rhs))
	 ,?body . ,?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(let-syntax ((,?lhs ,?rhs))
	 (let*-syntax ,(map list ?lhs* ?rhs*)
	   ,?body . ,?body*))))
    ))


;;;; module non-core-macro-transformer: LET-CONSTANTS, LET*-CONSTANTS, LETREC-CONSTANTS, LETREC*-CONSTANTS

(define (let-constants-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  LET-CONSTANTS macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(let () ,?body . ,?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (let ((SHADOW* (generate-temporaries (cons ?lhs ?lhs*))))
       (bless
	`(let ,(map list SHADOW* (cons ?rhs ?rhs*))
	   (let-syntax ,(map (lambda (lhs shadow)
			       `(,lhs (identifier-syntax ,shadow)))
			  (cons ?lhs ?lhs*) SHADOW*)
	     ,?body . ,?body*)))))
    ))

(define (let*-constants-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  LET*-CONSTANTS macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(let () ,?body . ,?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(let-constants ((,?lhs ,?rhs))
	 (let*-constants ,(map list ?lhs* ?rhs*)
	   ,?body . ,?body*))))
    ))

(define (letrec-constants-macro expr-stx)
  ;;Transformer function used to expand Vicare's LETREC-CONSTANTS macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ () ?body0 ?body* ...)
     (bless
      `(let () ,?body0 . ,?body*)))

    ((_ ((?lhs* ?rhs*) ...) ?body0 ?body* ...)
     (let ((TMP* (generate-temporaries ?lhs*))
	   (VAR* (generate-temporaries ?lhs*)))
       (bless
	`(let ,(map (lambda (var)
		      `(,var (void)))
		 VAR*)
	   (let-syntax ,(map (lambda (lhs var)
			       `(,lhs (identifier-syntax ,var)))
			  ?lhs* VAR*)
	     ;;Do not enforce the order of evaluation of ?RHS.
	     (let ,(map list TMP* ?rhs*)
	       ,@(map (lambda (var tmp)
			`(set! ,var ,tmp))
		   VAR* TMP*)
	       (let () ,?body0 . ,?body*)))))))
    ))

(define (letrec*-constants-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  LETREC*-CONSTANTS
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ () ?body0 ?body* ...)
     (bless
      `(let () ,?body0 . ,?body*)))

    ((_ ((?lhs* ?rhs*) ...) ?body0 ?body* ...)
     (let ((TMP* (generate-temporaries ?lhs*))
	   (VAR* (generate-temporaries ?lhs*)))
       (bless
	`(let ,(map (lambda (var)
		      `(,var (void)))
		 VAR*)
	   (let-syntax ,(map (lambda (lhs var)
			       `(,lhs (identifier-syntax ,var)))
			  ?lhs* VAR*)
	     ;;Do enforce the order of evaluation of ?RHS.
	     (let* ,(map list TMP* ?rhs*)
	       ,@(map (lambda (var tmp)
			`(set! ,var ,tmp))
		   VAR* TMP*)
	       (let () ,?body0 . ,?body*)))))))
    ))


;;;; module non-core-macro-transformer: CASE-DEFINE

(define (case-define-macro expr-stx)
  ;;Transformer function used to expand Vicare's CASE-DEFINE macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?who ?cl-clause ?cl-clause* ...)
     (identifier? ?who)
     (bless
      `(define ,?who
	 (case-lambda ,?cl-clause . ,?cl-clause*))))
    ))


;;;; module non-core-macro-transformer: DEFINE*, LAMBDA*, CASE-DEFINE*, CASE-LAMBDA*

(module (lambda*-macro
	 define*-macro
	 case-lambda*-macro
	 case-define*-macro)

  (define-record argument-validation-spec
    (arg-id
		;Identifier representing the formal name of the argument
		;being validated.
     expr
		;Syntax  object  representing   an  argument  validation
		;expression.
     ))

  (define-record retval-validation-spec
    (rv-id
		;Identifier representing the internal formal name of the
		;return value being validated.
     pred
		;Identifier bound to the predicate  to be applied to the
		;return value.
     ))

;;; --------------------------------------------------------------------

  (module (define*-macro)
    ;;Transformer function  used to expand Vicare's  DEFINE* macros from
    ;;the  top-level  built  in  environment.  Expand  the  contents  of
    ;;EXPR-STX.  Return a syntax object that must be further expanded.
    ;;
    ;;We want to implement the following example expansions:
    ;;
    ;;  (define* ?id ?value)	==> (define ?id ?value)
    ;;  (define* ?id)		==> (define ?id)
    ;;
    ;;  (define* (?who . ?common-formals) . ?body)
    ;;  ==> (define (?who . ?common-formals)
    ;;        (fluid-let-syntax
    ;;            ((__who__ (identifier-syntax (quote ?who))))
    ;;          (let () . ?body)))
    ;;
    ;;  (define* (?who (?var ?pred)) . ?body)
    ;;  ==> (define (?who ?var)
    ;;        (fluid-let-syntax
    ;;            ((__who__ (identifier-syntax (quote ?who))))
    ;;          (unless (?pred ?var)
    ;; 	          (procedure-argument-violation __who__
    ;; 	            "failed argument validation" '(?pred ?var) ?var))
    ;;          (let () . ?body)))
    ;;
    ;;  (define* ((?who ?pred) ?var) . ?body)
    ;;  ==> (define (?who ?var)
    ;;        (fluid-let-syntax
    ;;            ((__who__ (identifier-syntax (quote ?who))))
    ;;          (receive-and-return (rv)
    ;;              (let () . ?body)
    ;;            (unless (?pred rv)
    ;;              (expression-return-value-violation __who__
    ;; 	              "failed return value validation" (list '?pred rv))))))
    ;;
    (define (define*-macro stx)
      (define (%synner message subform)
	(syntax-violation 'define* message stx subform))
      (syntax-match stx ()
	;;No ret-pred.
	((_ (?who . ?formals) ?body0 ?body* ...)
	 (identifier? ?who)
	 (%generate-define-output-form/without-ret-pred ?who ?formals (cons ?body0 ?body*) %synner))

	;;Ret-pred with list spec.
	((_ ((?who ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (identifier? ?who)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-define-output-form/with-ret-pred ?who (cons ?ret-pred0 ?ret-pred*) ?formals (cons ?body0 ?body*) %synner))

	;;Ret-pred with vector spec.
	((_ (#(?who ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (identifier? ?who)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-define-output-form/with-ret-pred ?who (cons ?ret-pred0 ?ret-pred*) ?formals (cons ?body0 ?body*) %synner))

	((_ ?who ?expr)
	 (identifier? ?who)
	 (bless
	  `(define ,?who ,?expr)))

	((_ ?who)
	 (identifier? ?who)
	 (bless
	  `(define ,?who (void))))

	))

    (define (%generate-define-output-form/without-ret-pred ?who ?predicate-formals ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner)))
	  (bless
	   `(define (,?who . ,?standard-formals)
	      (fluid-let-syntax
		  ((__who__ (identifier-syntax (quote ,?who))))
		,@ARG-VALIDATION*
		(let () . ,?body*)))))))

    (define (%generate-define-output-form/with-ret-pred ?who ?ret-pred* ?predicate-formals ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner))
	       (RET*            (generate-temporaries ?ret-pred*))
	       (RET-VALIDATION  (%make-ret-validation-form (map make-retval-validation-spec RET* ?ret-pred*)
							   synner)))
	  (bless
	   `(define (,?who . ,?standard-formals)
	      (fluid-let-syntax
		  ((__who__ (identifier-syntax (quote ,?who))))
		,@ARG-VALIDATION*
		(receive-and-return (,@RET*)
		    (let () . ,?body*)
		  ,RET-VALIDATION)))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (case-define*-macro)

    (define (case-define*-macro stx)
      ;;Transformer function used to expand Vicare's CASE-DEFINE* macros
      ;;from the top-level built in environment.  Expand the contents of
      ;;EXPR-STX.  Return a syntax object that must be further expanded.
      ;;
      (define (%synner message subform)
	(syntax-violation 'case-define* message stx subform))
      (syntax-match stx ()
	((_ ?who ?clause0 ?clause* ...)
	 (identifier? ?who)
	 (bless
	  `(define ,?who
	     (case-lambda
	      ,@(map (lambda (?clause)
		       (%generate-case-define-form ?who ?clause %synner))
		  (cons ?clause0 ?clause*))))))
	))

    (define (%generate-case-define-form ?who ?clause synner)
      (syntax-match ?clause ()
	;;Ret-pred with list spec.
	((((?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (%underscore? ?underscore)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-case-define-clause-form/with-ret-pred ?who (cons ?ret-pred0 ?ret-pred*) ?formals ?body0 ?body* synner))

	;;Ret-pred with vector spec.
	(((#(?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (%underscore? ?underscore)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-case-define-clause-form/with-ret-pred ?who (cons ?ret-pred0 ?ret-pred*) ?formals ?body0 ?body* synner))

	;;No ret-pred.
	((?formals ?body0 ?body* ...)
	 (%generate-case-define-clause-form/without-ret-pred ?who ?formals ?body0 ?body* synner))
	))

    (define (%generate-case-define-clause-form/without-ret-pred ?who ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner)))
	  `(,?standard-formals
	    (fluid-let-syntax
		((__who__ (identifier-syntax (quote ,?who))))
	      ,@ARG-VALIDATION*
	      (let () ,?body0 ,@?body*))))))

    (define (%generate-case-define-clause-form/with-ret-pred ?who ?ret-pred* ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner))
	       (RET*            (generate-temporaries ?ret-pred*))
	       (RET-VALIDATION  (%make-ret-validation-form (map make-retval-validation-spec RET* ?ret-pred*)
							   synner)))
	  `(,?standard-formals
	    (fluid-let-syntax
		((__who__ (identifier-syntax (quote ,?who))))
	      ,@ARG-VALIDATION*
	      (receive-and-return (,@RET*)
		  (let () ,?body0 ,@?body*)
		,RET-VALIDATION))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (lambda*-macro)

    (define (lambda*-macro stx)
      ;;Transformer function used to expand Vicare's LAMBDA* macros from
      ;;the  top-level built  in  environment.  Expand  the contents  of
      ;;EXPR-STX.  Return a syntax object that must be further expanded.
      ;;
      (define (%synner message subform)
	(syntax-violation 'lambda* message stx subform))
      (syntax-match stx ()
	;;Ret-pred with list spec.
	((?kwd ((?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (%underscore? ?underscore)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-lambda-output-form/with-ret-pred ?kwd (cons ?ret-pred0 ?ret-pred*) ?formals ?body0 ?body* %synner))

	;;Ret-pred with vector spec.
	((?kwd (#(?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (%underscore? ?underscore)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-lambda-output-form/with-ret-pred ?kwd (cons ?ret-pred0 ?ret-pred*) ?formals ?body0 ?body* %synner))

	;;No ret-pred.
	((?kwd ?formals ?body0 ?body* ...)
	 (%generate-lambda-output-form/without-ret-pred ?kwd ?formals ?body0 ?body* %synner))

	))

    (define (%generate-lambda-output-form/without-ret-pred ?ctx ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner)))
	  (bless
	   `(lambda ,?standard-formals
	      (fluid-let-syntax
		  ((__who__ (identifier-syntax (quote _))))
		,@ARG-VALIDATION*
		(let () ,?body0 ,@?body*)))))))

    (define (%generate-lambda-output-form/with-ret-pred ?ctx ?ret-pred* ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner))
	       (RET*            (generate-temporaries ?ret-pred*))
	       (RET-VALIDATION  (%make-ret-validation-form (map make-retval-validation-spec RET* ?ret-pred*)
							   synner)))
	  (bless
	   `(lambda ,?standard-formals
	      (fluid-let-syntax
		  ((__who__ (identifier-syntax (quote _))))
		,@ARG-VALIDATION*
		(receive-and-return (,@RET*)
		    (let () ,?body0 ,@?body*)
		  ,RET-VALIDATION)))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (case-lambda*-macro)

    (define (case-lambda*-macro stx)
      ;;Transformer function used to expand Vicare's CASE-LAMBDA* macros
      ;;from the top-level built in environment.  Expand the contents of
      ;;EXPR-STX.  Return a syntax object that must be further expanded.
      ;;
      (define (%synner message subform)
	(syntax-violation 'case-lambda* message stx subform))
      (syntax-match stx ()
	((?kwd ?clause0 ?clause* ...)
	 (bless
	  `(case-lambda
	    ,@(map (lambda (?clause)
		     (%generate-case-lambda-form ?kwd ?clause %synner))
		(cons ?clause0 ?clause*)))))
	))

    (define (%generate-case-lambda-form ?ctx ?clause synner)
      (syntax-match ?clause ()
	;;Ret-pred with list spec.
	((((?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (%underscore? ?underscore)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-case-lambda-clause-form/with-ret-pred ?ctx (cons ?ret-pred0 ?ret-pred*) ?formals ?body0 ?body* synner))

	;;Ret-pred with vector spec.
	(((#(?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (%underscore? ?underscore)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-case-lambda-clause-form/with-ret-pred ?ctx (cons ?ret-pred0 ?ret-pred*) ?formals ?body0 ?body* synner))

	;;No ret-pred.
	((?formals ?body0 ?body* ...)
	 (%generate-case-lambda-clause-form/without-ret-pred ?ctx ?formals ?body0 ?body* synner))
	))

    (define (%generate-case-lambda-clause-form/without-ret-pred ?ctx ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner)))
	  `(,?standard-formals
	    (fluid-let-syntax
		((__who__ (identifier-syntax (quote _))))
	      ,@ARG-VALIDATION*
	      (let () ,?body0 ,@?body*))))))

    (define (%generate-case-lambda-clause-form/with-ret-pred ?ctx ?ret-pred* ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner))
	       (RET*            (generate-temporaries ?ret-pred*))
	       (RET-VALIDATION  (%make-ret-validation-form (map make-retval-validation-spec RET* ?ret-pred*)
							   synner)))
	  `(,?standard-formals
	    (fluid-let-syntax
		((__who__ (identifier-syntax (quote _))))
	      ,@ARG-VALIDATION*
	      (receive-and-return (,@RET*)
		  (let () ,?body0 ,@?body*)
		,RET-VALIDATION))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define (%parse-predicate-formals ?predicate-formals synner)
    ;;Split  formals from  tags.   We  rely on  the  DEFINE, LAMBDA  and
    ;;CASE-LAMBDA syntaxes  in the output  form to further  validate the
    ;;formals against duplicate bindings.
    ;;
    ;;We use  the conventions: ?ID,  ?REST-ID and ?ARGS-ID  are argument
    ;;identifiers;  ?PRED   is  a  predicate  identifier;   ?EXPR  is  a
    ;;validation expression.
    ;;
    ;;We accept the following standard formals formats:
    ;;
    ;;   ?args-id
    ;;   (?id ...)
    ;;   (?id0 ?id ... . ?rest-id)
    ;;
    ;;and in addition the following predicate formals:
    ;;
    ;;   #(?args-id ?pred ?expr ...)
    ;;   (?pred-arg ...)
    ;;   (?pred-arg0 ?pred-arg ... . ?rest-id)
    ;;   (?pred-arg0 ?pred-arg ... . #(?rest ?pred ?expr ...))
    ;;
    ;;where ?PRED-ARG is a predicate argument with one of the formats:
    ;;
    ;;   ?id
    ;;   (?id ?pred ?expr ...)
    ;;
    ;;Return 3 values:
    ;;
    ;;* A list  of syntax objects representing the  standard formals for
    ;;the DEFINE, LAMBDA and CASE-LAMBDA syntaxes.
    ;;
    ;;* A list of  ARGUMENT-VALIDATION-SPEC structures each representing
    ;;a validation predicate.
    ;;
    (syntax-match ?predicate-formals ()

      ;;Untagged identifiers without rest argument.
      ;;
      ((?id* ...)
       (for-all identifier? ?id*)
       (values ?id* '()))

      ;;Untagged identifiers with rest argument.
      ;;
      ((?id* ... . ?rest-id)
       (and (for-all identifier? ?id*)
	    (identifier? ?rest-id))
       (values ?predicate-formals '()))

      ;;Possibly tagged identifiers without rest argument.
      ;;
      ((?pred-arg* ...)
       (let recur ((?pred-arg* ?pred-arg*))
	 (if (pair? ?pred-arg*)
	     (receive (?standard-formals arg-validation-spec*)
		 (recur (cdr ?pred-arg*))
	       (let ((?pred-arg (car ?pred-arg*)))
		 (syntax-match ?pred-arg ()
		   ;;Untagged argument.
		   (?id
		    (identifier? ?id)
		    (values (cons ?id ?standard-formals)
			    arg-validation-spec*))
		   ;;Tagged argument, list spec.
		   ((?id ?pred)
		    (and (identifier? ?id)
			 (identifier? ?pred))
		    (values (cons ?id ?standard-formals)
			    (cons (make-argument-validation-spec ?id (list ?pred ?id))
				  arg-validation-spec*)))
		   ;;Tagged argument, vector spec.
		   (#(?id ?pred)
		    (and (identifier? ?id)
			 (identifier? ?pred))
		    (values (cons ?id ?standard-formals)
			    (cons (make-argument-validation-spec ?id (list ?pred ?id))
				  arg-validation-spec*)))
		   (else
		    (synner "invalid argument specification" ?pred-arg)))))
	   (values '() '()))))

      ;;Possibly tagged identifiers with rest argument.
      ;;
      ((?pred-arg* ... . ?rest-var)
       (let recur ((?pred-arg* ?pred-arg*))
	 (if (pair? ?pred-arg*)
	     (receive (?standard-formals arg-validation-spec*)
		 (recur (cdr ?pred-arg*))
	       (let ((?pred-arg (car ?pred-arg*)))
		 (syntax-match ?pred-arg ()
		   ;;Untagged argument.
		   (?id
		    (identifier? ?id)
		    (values (cons ?id ?standard-formals)
			    arg-validation-spec*))
		   ;;Tagged argument, list spec.
		   ((?id ?pred)
		    (and (identifier? ?id)
			 (identifier? ?pred))
		    (values (cons ?id ?standard-formals)
			    (cons (make-argument-validation-spec ?id (list ?pred ?id))
				  arg-validation-spec*)))
		   ;;Tagged argument, vector spec.
		   (#(?id ?pred)
		    (and (identifier? ?id)
			 (identifier? ?pred))
		    (values (cons ?id ?standard-formals)
			    (cons (make-argument-validation-spec ?id (list ?pred ?id))
				  arg-validation-spec*)))
		   (else
		    (synner "invalid argument specification" ?pred-arg)))))
	   ;;Process rest argument.
	   (syntax-match ?rest-var ()
	     ;;Untagged rest argument.
	     (?rest-id
	      (identifier? ?rest-id)
	      (values ?rest-id '()))
	     ;;Tagged rest argument.
	     (#(?rest-id ?rest-pred)
	      (and (identifier? ?rest-id)
		   (identifier? ?rest-pred))
	      (values ?rest-id
		      (list (make-argument-validation-spec ?rest-id (list ?rest-pred ?rest-id)))))
	     (else
	      (synner "invalid argument specification" ?rest-var))))))
      ))

;;; --------------------------------------------------------------------

  (define (%make-arg-validation-forms arg-validation-spec* synner)
    (if (options.enable-arguments-validation?)
	(map (lambda (spec)
	       (let ((?arg-expr (argument-validation-spec-expr   spec))
		     (?arg-id   (argument-validation-spec-arg-id spec)))
		 `(unless ,?arg-expr
		    (procedure-argument-violation __who__
		      "failed argument validation"
		      (quote ,?arg-expr) ,?arg-id))))
	  arg-validation-spec*)
      '()))

  (define (%make-ret-validation-form retval-validation-spec* synner)
    (if (options.enable-arguments-validation?)
	`(begin
	   ,@(map (lambda (spec)
		    (let ((?pred (retval-validation-spec-pred  spec))
			  (?ret  (retval-validation-spec-rv-id spec)))
		      `(unless (,?pred ,?ret)
			 (expression-return-value-violation __who__
			   "failed return value validation"
			   ;;This list  represents the application  of the
			   ;;predicate to the offending value.
			   (list (quote ,?pred) ,?ret)))))
	       retval-validation-spec*))
      '(void)))

  (define (%underscore? stx)
    (and (identifier? stx)
	 (eq? '_ (syntax->datum stx))))

  #| end of module |# )


;;;; module non-core-macro-transformer: TRACE-LAMBDA, TRACE-DEFINE and TRACE-DEFINE-SYNTAX

(define (trace-lambda-macro expr-stx)
  ;;Transformer  function used  to expand  Vicare's TRACE-LAMBDA  macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?who (?formal* ...) ?body ?body* ...)
     (if (valid-bound-ids? ?formal*)
	 (bless
	  `(make-traced-procedure ',?who
				  (lambda ,?formal*
				    ,?body . ,?body*)))
       (%error-invalid-formals-syntax expr-stx ?formal*)))

    ((_  ?who (?formal* ... . ?rest-formal) ?body ?body* ...)
     (if (valid-bound-ids? (cons ?rest-formal ?formal*))
	 (bless
	  `(make-traced-procedure ',?who
				  (lambda (,@?formal* . ,?rest-formal)
				    ,?body . ,?body*)))
       (%error-invalid-formals-syntax expr-stx (append ?formal* ?rest-formal))))
    ))

(define (trace-define-macro expr-stx)
  ;;Transformer  function used  to expand  Vicare's TRACE-DEFINE  macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?who ?formal* ...) ?body ?body* ...)
     (if (valid-bound-ids? ?formal*)
	 (bless
	  `(define ,?who
	     (make-traced-procedure ',?who
				    (lambda ,?formal*
				      ,?body . ,?body*))))
       (%error-invalid-formals-syntax expr-stx ?formal*)))

    ((_ (?who ?formal* ... . ?rest-formal) ?body ?body* ...)
     (if (valid-bound-ids? (cons ?rest-formal ?formal*))
	 (bless
	  `(define ,?who
	     (make-traced-procedure ',?who
				    (lambda (,@?formal* . ,?rest-formal)
				      ,?body . ,?body*))))
       (%error-invalid-formals-syntax expr-stx (append ?formal* ?rest-formal))))

    ((_ ?who ?expr)
     (if (identifier? ?who)
	 (bless `(define ,?who
		   (let ((v ,?expr))
		     (if (procedure? v)
			 (make-traced-procedure ',?who v)
		       v))))
       (stx-error expr-stx "invalid name")))
    ))

(define (trace-define-syntax-macro expr-stx)
  ;;Transformer  function used  to  expand Vicare's  TRACE-DEFINE-SYNTAX
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?who ?expr)
     (if (identifier? ?who)
	 (bless
	  `(define-syntax ,?who
	     (make-traced-macro ',?who ,?expr)))
       (stx-error expr-stx "invalid name")))
    ))


;;;; module non-core-macro-transformer: TRACE-LET-SYNTAX, TRACE-LETREC-SYNTAX

(module (trace-let-syntax-macro
	 trace-letrec-syntax-macro)

  (define (%trace-let/rec-syntax who)
    (lambda (stx)
      (syntax-match stx ()
	((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
	 (if (valid-bound-ids? ?lhs*)
	     (let ((rhs* (map (lambda (lhs rhs)
				`(make-traced-macro ',lhs ,rhs))
			   ?lhs* ?rhs*)))
	       (bless
		`(,who ,(map list ?lhs* rhs*)
		       ,?body . ,?body*)))
	   (%error-invalid-formals-syntax stx ?lhs*)))
	)))

  (define trace-let-syntax-macro
    ;;Transformer  function  used  to expand  Vicare's  TRACE-LET-SYNTAX
    ;;macros  from  the  top-level  built in  environment.   Expand  the
    ;;contents of EXPR-STX; return a  syntax object that must be further
    ;;expanded.
    ;;
    (%trace-let/rec-syntax 'let-syntax))

  (define trace-letrec-syntax-macro
    ;;Transformer function  used to expand  Vicare's TRACE-LETREC-SYNTAX
    ;;macros  from  the  top-level  built in  environment.   Expand  the
    ;;contents of EXPR-STX; return a  syntax object that must be further
    ;;expanded.
    ;;
    (%trace-let/rec-syntax 'letrec-syntax))

  #| end of module |# )


;;;; module non-core-macro-transformer: GUARD

(module (guard-macro)

  (define (guard-macro x)
    ;;Transformer function  used to  expand R6RS  GUARD macros  from the
    ;;top-level built in environment.   Expand the contents of EXPR-STX;
    ;;return a syntax object that must be further expanded.
    ;;
    ;;A syntax without else clause like:
    ;;
    ;;   (guard (E
    ;;           (?test0 ?expr0)
    ;;           (?test1 ?expr1)))
    ;;     ?body0 ?body ...)
    ;;
    ;;is expanded to:
    ;;
    ;;   ((call/cc
    ;;        (lambda (outerk)
    ;;          (lambda ()
    ;;            (with-exception-handler
    ;; 	              (lambda (raised-obj)
    ;; 	                (let ((E raised-obj))
    ;;                    ((call/cc
    ;; 		               (lambda (raisek)
    ;; 		                 (outerk (lambda ()
    ;; 	                                   (if ?test0
    ;; 	                                       ?expr0
    ;;                  	             (if ?test1
    ;; 	                                         ?expr1
    ;; 	                                       (raisek (lambda ()
    ;;                                                   (raise-continuable raised-obj))))))))))))
    ;;              (lambda ()
    ;; 	              ?body0 ?body ...))))))
    ;;
    (syntax-match x ()
      ((_ (?variable ?clause* ...) ?body ?body* ...)
       (identifier? ?variable)
       (let ((outerk-id     (gensym))
	     (raised-obj-id (gensym)))
	 (bless
	  `((call/cc
		(lambda (,outerk-id)
		  (lambda ()
		    (with-exception-handler
			(lambda (,raised-obj-id)
			  (let ((,?variable ,raised-obj-id))
			    ,(gen-clauses raised-obj-id outerk-id ?clause*)))
		      (lambda ()
			,?body . ,?body*))))))
	  )))
      ))

  (define (gen-clauses raised-obj-id outerk-id clause*)

    (define (%process-single-cond-clause clause kont-code-stx)
      (syntax-match clause (=>)
	((?test => ?proc)
	 (let ((t (gensym)))
	   `(let ((,t ,?test))
	      (if ,t
		  (,?proc ,t)
		,kont-code-stx))))

	((?test)
	 (let ((t (gensym)))
	   `(let ((,t ,?test))
	      (if ,t ,t ,kont-code-stx))))

	((?test ?expr ?expr* ...)
	 `(if ,?test
	      (begin ,?expr . ,?expr*)
	    ,kont-code-stx))

	(_
	 (stx-error clause "invalid guard clause"))))

    (define (%process-multi-cond-clauses clause*)
      (syntax-match clause* (else)
	;;There is no ELSE clause: introduce the raise continuation that
	;;rethrows the exception.
	(()
	 (let ((raisek (gensym)))
	   (values `(,raisek (lambda ()
			       (raise-continuable ,raised-obj-id)))
		   raisek)))

	;;There  is an  ELSE  clause:  no need  to  introduce the  raise
	;;continuation.
	(((else ?else-body ?else-body* ...))
	 (values `(begin ,?else-body . ,?else-body*)
		 #f))

	((?clause . ?clause*)
	 (receive (code-stx raisek)
	     (%process-multi-cond-clauses ?clause*)
	   (values (%process-single-cond-clause ?clause code-stx)
		   raisek)))

	(others
	 (stx-error others "invalid guard clause"))))

    (receive (code-stx raisek)
	(%process-multi-cond-clauses clause*)
      (if raisek
	  `((call/cc
		(lambda (,raisek)
		  (,outerk-id (lambda () ,code-stx)))))
	`(,outerk-id (lambda () ,code-stx)))))

  #| end of module: GUARD-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-ENUMERATION

(define (define-enumeration-macro stx)
  ;;Transformer function  used to expand R6RS  DEFINE-ENUMERATION macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (define-constant __who__ 'define-enumeration)
  (define (set? x)
    (or (null? x)
	(and (not (memq (car x) (cdr x)))
	     (set? (cdr x)))))
  (define (remove-dups ls)
    (if (null? ls)
	'()
      (cons (car ls)
	    (remove-dups (remq (car ls) (cdr ls))))))
  (syntax-match stx ()
    ((_ ?name (?id* ...) ?maker)
     (begin
       (unless (identifier? ?name)
	 (syntax-violation __who__
	   "expected identifier as enumeration type name" stx ?name))
       (unless (for-all identifier? ?id*)
	 (syntax-violation __who__
	   "expected list of symbols as enumeration elements" stx ?id*))
       (unless (identifier? ?maker)
	 (syntax-violation __who__
	   "expected identifier as enumeration constructor syntax name" stx ?maker))
       (let ((symbol*		(remove-dups (syntax->datum ?id*)))
	     (the-constructor	(gensym)))
	 (bless
	  `(begin
	     (define ,the-constructor
	       (enum-set-constructor (make-enumeration ',symbol*)))

	     (define-syntax ,?name
	       ;;Check at macro-expansion time whether the symbol ?ARG
	       ;;is in  the universe associated with ?NAME.   If it is,
	       ;;the result  of the  expansion is equivalent  to ?ARG.
	       ;;It is a syntax violation if it is not.
	       ;;
	       (lambda (x)
		 (define universe-of-symbols ',symbol*)
		 (define (%synner message subform)
		   (syntax-violation ',?name message
				     (syntax->datum x) (syntax->datum subform)))
		 (syntax-case x ()
		   ((_ ?arg)
		    (not (identifier? (syntax ?arg)))
		    (%synner "expected symbol as argument to enumeration validator"
			     (syntax ?arg)))

		   ((_ ?arg)
		    (not (memq (syntax->datum (syntax ?arg)) universe-of-symbols))
		    (%synner "expected symbol in enumeration as argument to enumeration validator"
			     (syntax ?arg)))

		   ((_ ?arg)
		    (syntax (quote ?arg)))

		   (_
		    (%synner "invalid enumeration validator form" #f)))))

	     (define-syntax ,?maker
	       ;;Given  any  finite sequence  of  the  symbols in  the
	       ;;universe, possibly  with duplicates, expands  into an
	       ;;expression that  evaluates to the  enumeration set of
	       ;;those symbols.
	       ;;
	       ;;Check  at  macro-expansion  time  whether  every  input
	       ;;symbol is in the universe  associated with ?NAME; it is
	       ;;a syntax violation if one or more is not.
	       ;;
	       (lambda (x)
		 (define universe-of-symbols ',symbol*)
		 (define (%synner message subform-stx)
		   (syntax-violation ',?maker
		     message
		     (syntax->datum x) (syntax->datum subform-stx)))
		 (syntax-case x ()
		   ((_ . ?list-of-symbols)
		    ;;Check the input  symbols one by one partitioning
		    ;;the ones in the universe from the one not in the
		    ;;universe.
		    ;;
		    ;;If  an input element  is not  a symbol:  raise a
		    ;;syntax violation.
		    ;;
		    ;;After   all   the   input  symbols   have   been
		    ;;partitioned,  if the  list of  collected INvalid
		    ;;ones is not null:  raise a syntax violation with
		    ;;that list as  subform, else return syntax object
		    ;;expression   building  a  new   enumeration  set
		    ;;holding the list of valid symbols.
		    ;;
		    (let loop ((valid-symbols-stx	'())
			       (invalid-symbols-stx	'())
			       (input-symbols-stx	(syntax ?list-of-symbols)))
		      (syntax-case input-symbols-stx ()

			;;No more symbols to collect and non-null list
			;;of collected INvalid symbols.
			(()
			 (not (null? invalid-symbols-stx))
			 (%synner "expected symbols in enumeration as arguments \
                                     to enumeration constructor syntax"
				  (reverse invalid-symbols-stx)))

			;;No more symbols to  collect and null list of
			;;collected INvalid symbols.
			(()
			 (quasisyntax
			  (,the-constructor '(unsyntax (reverse valid-symbols-stx)))))

			;;Error if element is not a symbol.
			((?symbol0 . ?rest)
			 (not (identifier? (syntax ?symbol0)))
			 (%synner "expected symbols as arguments to enumeration constructor syntax"
				  (syntax ?symbol0)))

			;;Collect a symbol in the set.
			((?symbol0 . ?rest)
			 (memq (syntax->datum (syntax ?symbol0)) universe-of-symbols)
			 (loop (cons (syntax ?symbol0) valid-symbols-stx)
			       invalid-symbols-stx (syntax ?rest)))

			;;Collect a symbol not in the set.
			((?symbol0 . ?rest)
			 (loop valid-symbols-stx
			       (cons (syntax ?symbol0) invalid-symbols-stx)
			       (syntax ?rest)))

			))))))
	     )))))
    ))


;;;; module non-core-macro-transformer: DO

(define (do-macro stx)
  ;;Transformer  function  used  to  expand  R6RS  DO  macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (define (%normalise-binding binding-stx)
    (syntax-match binding-stx ()
      ((?var ?init)       `(,?var ,?init ,?var))
      ((?var ?init ?step) `(,?var ,?init ,?step))
      (_
       (stx-error stx "invalid binding"))))
  (syntax-match stx ()
    ((_ (?binding* ...)
	(?test ?expr* ...)
	?command* ...)
     (syntax-match (map %normalise-binding ?binding*) ()
       (((?var* ?init* ?step*) ...)
	(if (valid-bound-ids? ?var*)
	    (bless
	     `(letrec ((loop (lambda ,?var*
			       (if ,?test
				   ;;If ?EXPR* is  null: make sure there
				   ;;is  at  least   one  expression  in
				   ;;BEGIN.
				   (begin (if #f #f) . ,?expr*)
				 (begin
				   ,@?command*
				   (loop . ,?step*))))))
		(loop . ,?init*)))
	  (stx-error stx "invalid bindings")))
       ))
    ))


;;;; module non-core-macro-transformer: WHILE, UNTIL, FOR

(define (while-macro expr-stx)
  ;;Transformer function used  to expand Vicare's WHILE  macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?test ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ,?test
		     (begin ,@?body* (loop))
		   (escape))))))))
    ))

(define (until-macro expr-stx)
  ;;Transformer function used  to expand Vicare's UNTIL  macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?test ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ,?test
		     (escape)
		   (begin ,@?body* (loop)))))))))
    ))

(define (for-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  FOR macros  from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?init ?test ?incr) ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     ,?init
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ,?test
		     (begin
		       ,@?body* ,?incr
		       (loop))
		   (escape))))))))
    ))


;;;; module non-core-macro-transformer: DEFINE-RETURNABLE, LAMBDA-RETURNABLE

(define (define-returnable-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  DEFINE-RETURNABLE
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?name . ?formals) ?body0 ?body* ...)
     (bless
      `(define (,?name . ,?formals)
	 (call/cc
	     (lambda (escape)
	       (fluid-let-syntax ((return (syntax-rules ()
					    ((_ . ?args)
					     (escape . ?args)))))
		 ,?body0 . ,?body*))))))
    ))

(define (lambda-returnable-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  LAMBDA-RETURNABLE
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?body0 ?body* ...)
     (bless
      `(lambda ,?formals
	 (call/cc
	     (lambda (escape)
	       (fluid-let-syntax ((return (syntax-rules ()
					    ((_ . ?args)
					     (escape . ?args)))))
		 ,?body0 . ,?body*))))))
    ))

(define (begin-returnable-macro expr-stx)
  ;;Transformer function used to expand Vicare's BEGIN-RETURNABLE macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?body0 ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     (fluid-let-syntax ((return (syntax-rules ()
					  ((_ . ?args)
					   (escape . ?args)))))
	       ,?body0 . ,?body*)))))
    ))


;;;; module non-core-macro-transformer: OR, AND

(define (or-macro expr-stx)
  ;;Transformer  function  used  to  expand  R6RS  OR  macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_) #f)

    ((_ ?expr ?expr* ...)
     (bless
      (let recur ((e ?expr) (e* ?expr*))
	(if (null? e*)
	    `(begin #f ,e)
	  `(let ((t ,e))
	     (if t
		 t
	       ,(recur (car e*) (cdr e*))))))))
    ))

(define (and-macro expr-stx)
  ;;Transformer  function  used  to  expand R6RS  AND  macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_) #t)

    ((_ ?expr ?expr* ...)
     (bless
      (let recur ((e ?expr) (e* ?expr*))
	(if (null? e*)
	    `(begin #f ,e)
	  `(if ,e
	       ,(recur (car e*) (cdr e*))
	     #f)))))
    ))


;;;; module non-core-macro-transformer: COND

(define (cond-macro expr-stx)
  ;;Transformer  function  used to  expand  R6RS  COND macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?cls ?cls* ...)
     (bless
      (let recur ((cls ?cls) (cls* ?cls*))
	(if (null? cls*)
	    (syntax-match cls (else =>)
	      ((else ?expr ?expr* ...)
	       `(let () #f ,?expr . ,?expr*))

	      ((?test => ?proc)
	       `(let ((t ,?test))
		  (if t (,?proc t))))

	      ((?expr)
	       `(or ,?expr (if #f #f)))

	      ((?test ?expr* ...)
	       `(if ,?test
		    (begin . ,?expr*)))

	      (_
	       (stx-error expr-stx "invalid last clause")))

	  (syntax-match cls (else =>)
	    ((else ?expr ?expr* ...)
	     (stx-error expr-stx "incorrect position of keyword else"))

	    ((?test => ?proc)
	     `(let ((t ,?test))
		(if t
		    (,?proc t)
		  ,(recur (car cls*) (cdr cls*)))))

	    ((?expr)
	     `(or ,?expr
		  ,(recur (car cls*) (cdr cls*))))

	    ((?test ?expr* ...)
	     `(if ,?test
		  (begin . ,?expr*)
		,(recur (car cls*) (cdr cls*))))

	    (_
	     (stx-error expr-stx "invalid last clause")))))))
    ))


;;;; module non-core-macro-transformer: QUASIQUOTE

(module (quasiquote-macro)
  ;;Transformer function used to expand  R6RS QUASIQUOTE macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;Some example expansions:
  ;;
  ;;   (quasiquote (1 2 (unquote (+ 3 4))))
  ;;   ==> (list '1 '2 (+ '3 '4))
  ;;
  ;;   (quasiquote (1 2 (unquote (+ 3 4))))
  ;;   ==> (vector '1 '2 (+ '3 '4))
  ;;
  ;;NOTE We  can test  QUASIQUOTE expansions by  evaluating at  the REPL
  ;;expressions like:
  ;;
  ;;   (expand-form-to-core-language
  ;;     '(quasiquote ?pattern)
  ;;     (interaction-environment))
  ;;
  (define (quasiquote-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?expr)
       (%quasi ?expr 0))
      ))

  (define (%quasi p nesting-level)
    ;;Process a list of items representing the items from a list.
    ;;
    (syntax-match p (unquote unquote-splicing quasiquote)
      ((unquote p)
       (if (zero? nesting-level)
	   p
	 (%quasicons (%keyword 'unquote)
		     (%quasi (list p) (sub1 nesting-level)))))

      (((unquote p ...) . q)
       (if (zero? nesting-level)
	   (%quasicons* p (%quasi q nesting-level))
	 (%quasicons (%quasicons (%keyword 'unquote)
				 (%quasi p (sub1 nesting-level)))
		     (%quasi q nesting-level))))

      (((unquote-splicing p ...) . q)
       (if (zero? nesting-level)
	   (%quasiappend p (%quasi q nesting-level))
	 (%quasicons (%quasicons (%keyword 'unquote-splicing)
				 (%quasi p (sub1 nesting-level)))
		     (%quasi q nesting-level))))

      ((quasiquote p)
       (%quasicons (%keyword 'quasiquote)
		   (%quasi (list p) (add1 nesting-level))))

      ((p . q)
       (%quasicons (%quasi p nesting-level)
		   (%quasi q nesting-level)))

      (#(x ...)
       (not (<stx>? x))
       (%quasivector (%vector-quasi x nesting-level)))

      (p
       (%application 'quote p))
      ))

  (define (%vector-quasi p nesting-level)
    ;;Process a list of items representing the items from a vector.
    ;;
    (syntax-match p ()
      ((p . q)
       (syntax-match p (unquote unquote-splicing)
  	 ((unquote p ...)
  	  (if (zero? nesting-level)
  	      (%quasicons* p (%vector-quasi q nesting-level))
  	    (%quasicons (%quasicons (%keyword 'unquote)
  				    (%quasi p (sub1 nesting-level)))
  			(%vector-quasi q nesting-level))))

  	 ((unquote-splicing p ...)
  	  (if (zero? nesting-level)
  	      (%quasiappend p (%vector-quasi q nesting-level))
  	    (%quasicons (%quasicons (%keyword 'unquote-splicing)
  				    (%quasi p (sub1 nesting-level)))
  			(%vector-quasi q nesting-level))))

  	 (p
  	  (%quasicons (%quasi p nesting-level)
  		      (%vector-quasi q nesting-level)))
  	 ))

      (()
       (%application 'quote '()))
      ))

  (define (%keyword key)
    ;;Return a  top-marked syntax  object representing a  quoted symbol;
    ;;the  symbol  being the  name  of  a  syntax  from the  boot  image
    ;;EXPORT-ENV.  Expanding  and evaluating the returned  syntax object
    ;;is equivalent to evaluating:
    ;;
    ;;   (quote key)
    ;;
    ;;where KEY is one of: quasiquote, unquote, unquote-splicing.
    ;;
    (list (scheme-stx 'quote) (mkstx key TOP-MARK* '() '())))

  (define-syntax %application
    ;;Expand to an expression which, when evaluated, results in a syntax
    ;;object representing an expression.   Such syntax object expression
    ;;is the application of ?CONSTRUCTOR to the, possibly empty, list of
    ;;arguments ?ARG*.
    ;;
    ;;?CONSTRUCTOR must be a symbol  representing the name of a function
    ;;or syntax  from the boot  image EXPORT-ENV; candidates  are: list,
    ;;vector, list->vector, cons, quote.
    ;;
    (syntax-rules (quote)
      ((_ (quote ?constructor) ?arg* ...)
       (list (scheme-stx '?constructor) ?arg* ...))
      ))

  (define-syntax %application*
    ;;Expand to an expression which, when evaluated, results in a syntax
    ;;object representing an expression.   Such syntax object expression
    ;;is the application of ?CONSTRUCTOR to the, possibly empty, list of
    ;;arguments ?ARG* and the list of arguments ?TAIL-ARG*.
    ;;
    ;;?CONSTRUCTOR must be a symbol  representing the name of a function
    ;;or syntax  from the boot  image EXPORT-ENV; candidates  are: list,
    ;;append, vector.
    ;;
    (syntax-rules (quote)
      ((_ (quote ?constructor) ?arg* ... ?tail-arg*)
       (cons* (scheme-stx '?constructor) ?arg* ... ?tail-arg*))))

  (define (%quasicons* x y)
    (let recur ((x x))
      (if (null? x)
	  y
	(%quasicons (car x) (recur (cdr x))))))

  (define (%quasicons x y)
    (syntax-match y (quote list)
      ((quote ?dy)
       (syntax-match x (quote)
	 ((quote ?dx)
	  (%application 'quote (cons ?dx ?dy)))

	 (_
	  (syntax-match ?dy ()
	    (()
	     (%application 'list x))
	    (_
	     (%application 'cons x y))
	    ))
	 ))

      ((list ?stuff ...)
       (%application* 'list x ?stuff))

      (_
       (%application 'cons x y))
      ))

  (define (%quasiappend x y)
    (let ((ls (let recur ((x x))
		(if (null? x)
		    (syntax-match y (quote)
		      ((quote ())
		       '())
		      (_
		       (list y)))
		  (syntax-match (car x) (quote)
		    ((quote ())
		     (recur (cdr x)))
		    (_
		     (cons (car x) (recur (cdr x)))))))))
      (cond ((null? ls)
	     (%application 'quote '()))
	    ((null? (cdr ls))
	     (car ls))
	    (else
	     (%application* 'append ls)))))

  (define (%quasivector x)
    (let ((pat-x x))
      (syntax-match pat-x (quote)
  	((quote (x* ...))
  	 (%application 'quote (list->vector x*)))

  	(_
  	 (let loop ((x x)
  		    (k (lambda (ls)
  			 (%application* 'vector ls))))
  	   (syntax-match x (list cons quote)
  	     ((quote (x* ...))
  	      (k (map (lambda (x) (%application 'quote x)) x*)))

  	     ((list x* ...)
  	      (k x*))

  	     ((cons x y)
  	      (loop y (lambda (ls)
  			(k (cons x ls)))))

  	     (_
  	      (%application 'list->vector pat-x))
  	     )))
  	)))

  #| end of module: QUASIQUOTE-MACRO |# )


;;;; module non-core-macro-transformer: QUASISYNTAX

(module (quasisyntax-macro)
  ;;Transformer function used to expand R6RS QUASISYNTAX macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;FIXME: not really correct (Abdulaziz Ghuloum).
  ;;
  (define (quasisyntax-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ e)
       (receive (lhs* rhs* v)
	   (quasi e 0)
	 (bless
	  `(syntax-case (list ,@rhs*) ()
	     (,lhs*
	      (syntax ,v))))))
      ))

  (define (quasi p nesting-level)
    (syntax-match p (unsyntax unsyntax-splicing quasisyntax)
      ((unsyntax p)
       (if (zero? nesting-level)
	   (let ((g (gensym)))
	     (values (list g) (list p) g))
	 (receive (lhs* rhs* p)
	     (quasi p (sub1 nesting-level))
	   (values lhs* rhs* (list 'unsyntax p)))))

      (unsyntax
       (zero? nesting-level)
       (stx-error p "incorrect use of unsyntax"))

      (((unsyntax p* ...) . q)
       (receive (lhs* rhs* q)
	   (quasi q nesting-level)
	 (if (zero? nesting-level)
	     (let ((g* (map (lambda (x) (gensym)) p*)))
	       (values (append g* lhs*)
		       (append p* rhs*)
		       (append g* q)))
	   (receive (lhs2* rhs2* p*)
	       (quasi p* (sub1 nesting-level))
	     (values (append lhs2* lhs*)
		     (append rhs2* rhs*)
		     `((unsyntax . ,p*) . ,q))))))

      (((unsyntax-splicing p* ...) . q)
       (receive (lhs* rhs* q)
	   (quasi q nesting-level)
	 (if (zero? nesting-level)
	     (let ((g* (map (lambda (x) (gensym)) p*)))
	       (values (append (map (lambda (g) `(,g ...)) g*)
			       lhs*)
		       (append p* rhs*)
		       (append (apply append
				      (map (lambda (g) `(,g ...)) g*))
			       q)))
	   (receive (lhs2* rhs2* p*)
	       (quasi p* (sub1 nesting-level))
	     (values (append lhs2* lhs*)
		     (append rhs2* rhs*)
		     `((unsyntax-splicing . ,p*) . ,q))))))

      (unsyntax-splicing
       (zero? nesting-level)
       (stx-error p "incorrect use of unsyntax-splicing"))

      ((quasisyntax p)
       (receive (lhs* rhs* p)
	   (quasi p (add1 nesting-level))
	 (values lhs* rhs* `(quasisyntax ,p))))

      ((p . q)
       (let-values
	   (((lhs*  rhs*  p) (quasi p nesting-level))
	    ((lhs2* rhs2* q) (quasi q nesting-level)))
	 (values (append lhs2* lhs*)
		 (append rhs2* rhs*)
		 (cons p q))))

      (#(x* ...)
       (receive (lhs* rhs* x*)
	   (quasi x* nesting-level)
	 (values lhs* rhs* (list->vector x*))))

      (_
       (values '() '() p))
      ))

  #| end of module |# )


;;;; module non-core-macro-transformer: DEFINE-VALUES, DEFINE-CONSTANT-VALUES

(define (define-values-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  DEFINE-VALUES macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?var* ... ?var0) ?form* ... ?form0)
     (let ((TMP* (generate-temporaries ?var*)))
       (bless
	`(begin
	   ;;We must make sure that the ?FORMs do not capture the ?VARs.
	   (define (return-multiple-values)
	     ,@?form* ,?form0)
	   ,@(map (lambda (var)
		    `(define ,var #f))
	       ?var*)
	   (define ,?var0
	     (call-with-values
		 return-multiple-values
	       (lambda (,@TMP* T0)
		 ,@(map (lambda (var TMP)
			  `(set! ,var ,TMP))
		     ?var* TMP*)
		 T0)))
	   ))))
    ))

(define (define-constant-values-macro expr-stx)
  ;;Transformer function used  to expand Vicare's DEFINE-CONSTANT-VALUES
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?var* ... ?var0) ?form* ... ?form0)
     (let ((SHADOW* (generate-temporaries ?var*))
	   (TMP*    (generate-temporaries ?var*)))
       (bless
	`(begin
	   (define (return-multiple-values)
	     ,@?form* ,?form0)
	   ,@(map (lambda (SHADOW)
		    `(define ,SHADOW #f))
	       SHADOW*)
	   (define SHADOW0
	     (call-with-values
		 return-multiple-values
	       (lambda (,@TMP* T0)
		 ,@(map (lambda (SHADOW TMP)
			  `(set! ,SHADOW ,TMP))
		     SHADOW* TMP*)
		 T0)))
	   ,@(map (lambda (var SHADOW)
		    `(define-syntax ,var
		       (identifier-syntax ,SHADOW)))
	       ?var* SHADOW*)
	   (define-syntax ,?var0
	     (identifier-syntax SHADOW0))
	   ))))
    ))


;;;; module non-core-macro-transformer: RECEIVE, RECEIVE-AND-RETURN, BEGIN0, XOR

(define (receive-macro expr-stx)
  ;;Transformer function used to expand Vicare's RECEIVE macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?producer-expression ?form0 ?form* ...)
     (bless
      `(call-with-values
	   (lambda () ,?producer-expression)
	 (lambda ,?formals ,?form0 ,@?form*))))
    ))

(define (receive-and-return-macro expr-stx)
  ;;Transformer  function  used  to expand  Vicare's  RECEIVE-AND-RETURN
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?retval* ...) ?producer-expression ?body0 ?body* ...)
     (bless
      `(call-with-values
	   (lambda () ,?producer-expression)
	 (lambda ,?retval*
	   ,?body0 ,@?body*
	   (values ,@?retval*)))))
    ))

(define (begin0-macro expr-stx)
  ;;Transformer function used to expand  Vicare's BEGIN0 macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?form0 ?form* ...)
     (bless
      `(call-with-values
	   (lambda () ,?form0)
	 (lambda args
	   ,@?form*
	   (apply values args)))))
    ))

(module (xor-macro)
  ;;Transformer function  used to  expand Vicare's  XOR macros  from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (define (xor-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?expr* ...)
       (bless (%xor-aux #f ?expr*)))
      ))

  (define (%xor-aux bool/var expr*)
    (cond ((null? expr*)
	   bool/var)
	  ((null? (cdr expr*))
	   `(let ((x ,(car expr*)))
	      (if ,bool/var
		  (and (not x) ,bool/var)
		x)))
	  (else
	   `(let ((x ,(car expr*)))
	      (and (or (not ,bool/var)
		       (not x))
		   (let ((n (or ,bool/var x)))
		     ,(%xor-aux 'n (cdr expr*))))))))

  #| end of module: XOR-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-INLINE, DEFINE-CONSTANT

(define (define-constant-macro expr-stx)
  ;;Transformer function used to  expand Vicare's DEFINE-CONSTANT macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name ?expr)
     (bless
      `(begin
	 (define ghost ,?expr)
	 (define-syntax ,?name
	   (identifier-syntax ghost)))))
    ))

(define (define-inline-constant-macro expr-stx)
  ;;Transformer function used  to expand Vicare's DEFINE-INLINE-CONSTANT
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  ;;We want to allow a generic expression to generate the constant value
  ;;at expand time.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name ?expr)
     (bless
      `(define-syntax ,?name
	 (let ((const ,?expr))
	   (lambda (stx)
	     (syntax-case stx ()
	       (?id
		(identifier? #'?id)
		#`(quote #,const))))))))
    ))

(define (define-inline-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  DEFINE-INLINE macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?name ?arg* ... . ?rest) ?form0 ?form* ...)
     (and (identifier? ?name)
	  (for-all identifier? ?arg*)
	  (or (null? (syntax->datum ?rest))
	      (identifier? ?rest)))
     (let ((TMP* (generate-temporaries ?arg*)))
       (bless
	`(define-fluid-syntax ,?name
	   (syntax-rules ()
	     ((_ ,@TMP* . rest)
	      (fluid-let-syntax
		  ((,?name (lambda (stx)
			     (syntax-violation (quote ,?name)
			       "cannot recursively expand inline expression"
			       stx))))
		(let ,(append (map list ?arg* TMP*)
			      (if (null? (syntax->datum ?rest))
				  '()
				`((,?rest (list . rest)))))
		  ,?form0 ,@?form*))))))))
    ))


;;;; module non-core-macro-transformer: INCLUDE

(module (include-macro)
  ;;Transformer function used to expand Vicare's INCLUDE macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (define (include-macro expr-stx)
    (define (%synner message subform)
      (syntax-violation 'include message expr-stx subform))
    (syntax-match expr-stx ()
      ((?context ?filename)
       (%include-file ?filename ?context #f %synner))
      ((?context ?filename #t)
       (%include-file ?filename ?context #t %synner))
      ))

  (define (%include-file filename-stx context-id verbose? synner)
    (define filename.str
      (syntax->datum filename-stx))
    (unless (string? filename.str)
      (stx-error filename-stx "expected string as include file pathname"))
    (receive (pathname contents)
	;;FIXME Why in  fuck I cannot use the  parameter here?!?  (Marco
	;;Maggi; Tue Feb 11, 2014)
	(default-include-loader #;(current-include-loader)
	  filename.str verbose? synner)
      ;;We expect CONTENTS to be null or a list of annotated datums.
      (bless
       `(stale-when (let ()
		      (import (only (vicare language-extensions posix)
				    file-modification-time))
		      (or (not (file-exists? ,pathname))
			  (> (file-modification-time ,pathname)
			     ,(file-modification-time pathname))))
	  . ,(map (lambda (item)
		    (datum->syntax context-id item))
	       contents)))))

  #| end of module: INCLUDE-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-INTEGRABLE

(define (define-integrable-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  DEFINE-INTEGRABLE
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  ;;The original  syntax was  posted by "leppie"  on the  Ikarus mailing
  ;;list; subject "Macro Challenge of Last Year [Difficulty: *****]", 20
  ;;Oct 2009.
  ;;
  (syntax-match expr-stx (lambda)
    ((_ (?name . ?formals) ?form0 ?form* ...)
     (identifier? ?name)
     (bless
      `(define-integrable ,?name (lambda ,?formals ,?form0 ,@?form*))))

    ((_ ?name (lambda ?formals ?form0 ?form* ...))
     (identifier? ?name)
     (bless
      `(begin
	 (define-fluid-syntax ,?name
	   (lambda (x)
	     (syntax-case x ()
	       (_
		(identifier? x)
		#'xname)

	       ((_ arg ...)
		#'((fluid-let-syntax
		       ((,?name (identifier-syntax xname)))
		     (lambda ,?formals ,?form0 ,@?form*))
		   arg ...)))))
	 (define xname
	   (fluid-let-syntax ((,?name (identifier-syntax xname)))
	     (lambda ,?formals ,?form0 ,@?form*)))
	 )))
    ))


;;;; module non-core-macro-transformer: DEFINE-SYNTAX-PARAMETER, SYNTAX-PARAMETRISE

(define (define-syntax-parameter-macro expr-stx)
  ;;Transformer function used to expand Vicare's DEFINE-SYNTAX-PARAMETER
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?param-id ?param-expr)
     (identifier? ?param-id)
     (bless
      `(define-fluid-syntax ,?param-id
	 (make-compile-time-value ,?param-expr))))
    ))

(define (syntax-parametrise-macro expr-stx)
  ;;Transformer      function      used     to      expand      Vicare's
  ;;SYNTAX-PARAMETRISE-MACRO   macros  from   the  top-level   built  in
  ;;environment.   Expand  the contents  of  EXPR-STX;  return a  syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ((?lhs* ?rhs*) ...) ?body0 ?body* ...)
     (for-all identifier? ?lhs*)
     (bless
      `(fluid-let-syntax ,(map (lambda (lhs rhs)
				 (list lhs `(make-compile-time-value ,rhs)))
			    ?lhs* ?rhs*)
	 ,?body0 . ,?body*)))
    ))


;;;; module non-core-macro-transformer: miscellanea

(define (time-macro expr-stx)
  ;;Transformer function  used to expand  Vicare's TIME macros  from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?expr)
     (let ((str (receive (port getter)
		    (open-string-output-port)
		  (write (syntax->datum ?expr) port)
		  (getter))))
       (bless
	`(time-it ,str (lambda () ,?expr)))))))

(define (delay-macro expr-stx)
  ;;Transformer  function used  to  expand R6RS  DELAY  macros from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?expr)
     (bless
      `(make-promise (lambda ()
		       ,?expr))))))

(define (assert-macro expr-stx)
  ;;Defined by R6RS.  An ASSERT  form is evaluated by evaluating EXPR.
  ;;If  EXPR returns a  true value,  that value  is returned  from the
  ;;ASSERT  expression.   If EXPR  returns  false,  an exception  with
  ;;condition  types  "&assertion"  and  "&message"  is  raised.   The
  ;;message  provided  in   the  condition  object  is  implementation
  ;;dependent.
  ;;
  ;;NOTE  Implementations should  exploit the  fact that  ASSERT  is a
  ;;syntax  to  provide as  much  information  as  possible about  the
  ;;location of the assertion failure.
  ;;
  (syntax-match expr-stx ()
    ((_ ?expr)
     (let ((pos (or (expression-position expr-stx)
		    (expression-position ?expr))))
       (bless
	(if (source-position-condition? pos)
	    `(or ,?expr
		 (assertion-error
		  ',?expr ,(source-position-port-id pos)
		  ,(source-position-byte pos) ,(source-position-character pos)
		  ,(source-position-line pos) ,(source-position-column    pos)))
	  `(or ,?expr
	       (assertion-error ',?expr "unknown source" #f #f #f #f))))))))

(define (file-options-macro expr-stx)
  ;;Transformer for  the FILE-OPTIONS macro.  File  options selection is
  ;;implemented   as   an   enumeration  type   whose   constructor   is
  ;;MAKE-FILE-OPTIONS from the boot environment.
  ;;
  (define (valid-option? opt-stx)
    (and (identifier? opt-stx)
	 (memq (identifier->symbol opt-stx) '(no-fail no-create no-truncate))))
  (syntax-match expr-stx ()
    ((_ ?opt* ...)
     (for-all valid-option? ?opt*)
     (bless
      `(make-file-options ',?opt*)))))

(define (endianness-macro expr-stx)
  ;;Transformer of  ENDIANNESS.  Support  the symbols:  "big", "little",
  ;;"network", "native"; convert "network" to "big".
  ;;
  (syntax-match expr-stx ()
    ((_ ?name)
     (and (identifier? ?name)
	  (memq (identifier->symbol ?name) '(big little network native)))
     (case (identifier->symbol ?name)
       ((network)
	(bless '(quote big)))
       ((native)
	(bless '(native-endianness)))
       ((big little)
	(bless `(quote ,?name)))))))

(define (%allowed-symbol-macro expr-stx allowed-symbol-set)
  ;;Helper  function used  to  implement the  transformer of:  EOL-STYLE
  ;;ERROR-HANDLING-MODE, BUFFER-MODE,  ENDIANNESS.  All of  these macros
  ;;should expand to a quoted symbol among a list of allowed ones.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name)
     (and (identifier? ?name)
	  (memq (identifier->symbol ?name) allowed-symbol-set))
     (bless
      `(quote ,?name)))))


;;; end of module: NON-CORE-MACRO-TRANSFORMER

)


(module CORE-MACRO-TRANSFORMER
  (core-macro-transformer)
  ;;The  function   CORE-MACRO-TRANSFORMER  maps   symbols  representing
  ;;non-core macros to their macro transformers.
  ;;
  ;;We distinguish between "non-core macros" and "core macros".
  ;;
  ;;NOTE This  module is very  long, so it  is split into  multiple code
  ;;pages.  (Marco Maggi; Sat Apr 27, 2013)
  ;;
  (define* (core-macro-transformer name)
    (case name
      ((quote)				quote-transformer)
      ((lambda)				lambda-transformer)
      ((case-lambda)			case-lambda-transformer)
      ((letrec)				letrec-transformer)
      ((letrec*)			letrec*-transformer)
      ((if)				if-transformer)
      ((foreign-call)			foreign-call-transformer)
      ((syntax-case)			syntax-case-transformer)
      ((syntax)				syntax-transformer)
      ((fluid-let-syntax)		fluid-let-syntax-transformer)
      ((splice-first-expand)		splice-first-expand-transformer)
      ((unsafe)				unsafe-transformer)
      ((predicate-procedure-argument-validation)	predicate-procedure-argument-validation-transformer)
      ((predicate-return-value-validation)		predicate-return-value-validation-transformer)
      ((struct-type-descriptor)		struct-type-descriptor-transformer)
      ((struct-type-and-struct?)	struct-type-and-struct?-transformer)
      ((struct-type-field-ref)		struct-type-field-ref-transformer)
      ((struct-type-field-set!)		struct-type-field-set!-transformer)
      (($struct-type-field-ref)		$struct-type-field-ref-transformer)
      (($struct-type-field-set!)	$struct-type-field-set!-transformer)
      ((record-type-descriptor)		record-type-descriptor-transformer)
      ((record-constructor-descriptor)	record-constructor-descriptor-transformer)
      ((record-type-field-set!)		record-type-field-set!-transformer)
      ((record-type-field-ref)		record-type-field-ref-transformer)
      (($record-type-field-set!)	$record-type-field-set!-transformer)
      (($record-type-field-ref)		$record-type-field-ref-transformer)
      ((type-descriptor)		type-descriptor-transformer)
      ((is-a?)				is-a?-transformer)
      ((slot-ref)			slot-ref-transformer)
      ((slot-set!)			slot-set!-transformer)
      (($slot-ref)			$slot-ref-transformer)
      (($slot-set!)			$slot-set!-transformer)
      (else
       (assertion-violation __who__
	 "Vicare: internal error: cannot find transformer" name))))


;;;; module core-macro-transformer: IF

(define (if-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  R6RS  IF syntaxes  from  the
  ;;top-level built  in environment.  Expand the  syntax object EXPR-STX
  ;;in  the context  of the  given LEXENV;  return an  expanded language
  ;;symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ ?test ?consequent ?alternate)
     (build-conditional no-source
       (chi-expr ?test       lexenv.run lexenv.expand)
       (chi-expr ?consequent lexenv.run lexenv.expand)
       (chi-expr ?alternate  lexenv.run lexenv.expand)))
    ((_ ?test ?consequent)
     (build-conditional no-source
       (chi-expr ?test       lexenv.run lexenv.expand)
       (chi-expr ?consequent lexenv.run lexenv.expand)
       (build-void)))
    ))


;;;; module core-macro-transformer: QUOTE

(define (quote-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand R6RS  QUOTE syntaxes  from the
  ;;top-level built  in environment.  Expand the  syntax object EXPR-STX
  ;;in  the context  of the  given LEXENV;  return an  expanded language
  ;;symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ ?datum)
     (build-data no-source
       (syntax->datum ?datum)))))


;;;; module core-macro-transformer: LAMBDA and CASE-LAMBDA

(define (case-lambda-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand R6RS  CASE-LAMBDA syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ (?formals* ?body* ?body** ...) ...)
     (receive (formals* body*)
	 (chi-lambda-clause* expr-stx ?formals*
			     (map cons ?body* ?body**)
			     lexenv.run lexenv.expand)
       (build-case-lambda (syntax-annotation expr-stx)
	 formals* body*)))))

(define (lambda-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  R6RS LAMBDA syntaxes  from the
  ;;top-level built  in environment.  Expand the  syntax object EXPR-STX
  ;;in  the context  of the  given LEXENV;  return an  expanded language
  ;;symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?body ?body* ...)
     (receive (formals body)
	 (chi-lambda-clause expr-stx ?formals
			    (cons ?body ?body*)
			    lexenv.run lexenv.expand)
       (build-lambda (syntax-annotation expr-stx)
	 formals body)))))


;;;; module core-macro-transformer: LETREC and LETREC*

(module (letrec-transformer letrec*-transformer)

  (define (letrec-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer  function  used to  expand  LETREC  syntaxes from  the
    ;;top-level built in environment.  Expand the syntax object EXPR-STX
    ;;in the  context of the  given LEXENV; return an  expanded language
    ;;symbolic expression.
    ;;
    (%letrec-helper expr-stx lexenv.run lexenv.expand build-letrec))

  (define (letrec*-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer  function used  to  expand LETREC*  syntaxes from  the
    ;;top-level built in environment.  Expand the syntax object EXPR-STX
    ;;in the  context of the  given LEXENV; return an  expanded language
    ;;symbolic expression.
    ;;
    (%letrec-helper expr-stx lexenv.run lexenv.expand build-letrec*))

  (define (%letrec-helper expr-stx lexenv.run lexenv.expand core-lang-builder)
    (syntax-match expr-stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check  that  the  binding  names are  identifiers  and  without
       ;;duplicates.
       (unless (valid-bound-ids? ?lhs*)
	 (%error-invalid-formals-syntax expr-stx ?lhs*))
       ;;Generate  unique  variable  names  and labels  for  the  LETREC
       ;;bindings.
       (let ((lex* (map gensym-for-lexical-var ?lhs*))
	     (lab* (map gensym-for-label       ?lhs*)))
	 ;;Generate what is needed to  create a lexical contour: a <RIB>
	 ;;and an extended lexical environment in which to evaluate both
	 ;;the right-hand sides and the body.
	 ;;
	 ;;Notice that  the region of  all the LETREC  bindings includes
	 ;;all the right-hand sides.
	 (let ((rib        (make-filled-rib ?lhs* lab*))
	       (lexenv.run (add-lexical-bindings lab* lex* lexenv.run)))
	   ;;Create the lexical contour then process body and right-hand
	   ;;sides of bindings.
	   (let ((body (chi-internal-body (push-lexical-contour rib
					    (cons ?body ?body*))
					  lexenv.run lexenv.expand))
		 (rhs* (chi-expr*         (map (lambda (rhs)
						 (push-lexical-contour rib rhs))
					    ?rhs*)
					  lexenv.run lexenv.expand)))
	     ;;Build  the  LETREC  or  LETREC* expression  in  the  core
	     ;;language.
	     (core-lang-builder no-source lex* rhs* body)))))
      ))

  #| end of module |# )


;;;; module core-macro-transformer: FLUID-LET-SYNTAX

(define (fluid-let-syntax-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand FLUID-LET-SYNTAX  syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  ;;FLUID-LET-SYNTAX is  similar, but  not equal, to  LET-SYNTAX; rather
  ;;than defining new ?LHS bindings, it temporarily rebinds the keywords
  ;;to new transformers while expanding the ?BODY forms.  The given ?LHS
  ;;must   be    already   bound   to   fluid    syntaxes   defined   by
  ;;DEFINE-FLUID-SYNTAX.
  ;;
  ;;There are  two differences between FLUID-LET-SYNTAX  and LET-SYNTAX:
  ;;FLUID-LET-SYNTAX  must  appear  in   expression  context  only;  the
  ;;internal ?BODY forms are *not* spliced in the enclosing body.
  ;;
  ;;NOTE  We would  truly like  to splice  the inner  body forms  in the
  ;;surrounding body,  so that  this syntax  could act  like LET-SYNTAX,
  ;;which is useful; but we really cannot do it with this implementation
  ;;of the expander algorithm.  This  is because LET-SYNTAX both creates
  ;;a  new  rib  and  adds  new  id/label  entries  to  it,  and  pushes
  ;;label/descriptor  entries to  the  LEXENV; instead  FLUID-LET-SYNTAX
  ;;only pushes entries to the LEXENV: there is no way to keep the fluid
  ;;LEXENV entries  visible only to  a subsequence  of forms in  a body.
  ;;(Marco Maggi; Tue Feb 18, 2014)
  ;;
  (define (transformer expr-stx)
    (syntax-match expr-stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check that the ?LHS* are all identifiers with no duplicates.
       (unless (valid-bound-ids? ?lhs*)
	 (%error-invalid-formals-syntax expr-stx ?lhs*))
       (let* ((fluid-label* (map %lookup-binding-in-lexenv.run ?lhs*))
	      (binding*     (map (lambda (rhs)
				   (%eval-macro-transformer
				    (%expand-macro-transformer rhs lexenv.expand)
				    lexenv.run))
			      ?rhs*))
	      (entry*       (map cons fluid-label* binding*)))
	 (chi-internal-body (cons ?body ?body*)
			    (append entry* lexenv.run)
			    (append entry* lexenv.expand))))))

  (define (%lookup-binding-in-lexenv.run lhs)
    ;;Search the binding of the  identifier LHS retrieving its label; if
    ;;such  label  is  present  and  its  associated  syntactic  binding
    ;;descriptor from LEXENV.RUN  is of type "fluid  syntax": return the
    ;;associated fluid label that can be used to rebind the identifier.
    ;;
    (let* ((label    (or (id->label lhs)
			 (stx-error lhs "unbound identifier")))
	   (binding  (label->syntactic-binding/no-indirection label lexenv.run)))
      (cond ((fluid-syntax-binding? binding)
	     (fluid-syntax-binding-fluid-label binding))
	    (else
	     (stx-error lhs "not a fluid identifier")))))

  (transformer expr-stx))


;;;; module core-macro-transformer: FOREIGN-CALL

(define (foreign-call-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  FOREIGN-CALL syntaxes
  ;;from the top-level  built in environment.  Expand  the syntax object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name ?arg* ...)
     (build-foreign-call no-source
       (chi-expr  ?name lexenv.run lexenv.expand)
       (chi-expr* ?arg* lexenv.run lexenv.expand)))))


;;;; module core-macro-transformer: SYNTAX

(module (syntax-transformer)
  ;;Transformer function used to expand  R6RS's SYNTAX syntaxes from the
  ;;top-level built in environment.  Process  the contents of USE-STX in
  ;;the   context   of   the   lexical   environments   LEXENV.RUN   and
  ;;LEXENV.EXPAND.
  ;;
  ;;According to R6RS, the use of the SYNTAX macro must have the format:
  ;;
  ;;  (syntax ?template)
  ;;
  ;;where ?TEMPLATE is one among:
  ;;
  ;;  ?datum
  ;;  ?pattern-variable
  ;;  ?id
  ;;  (?subtemplate ...)
  ;;  (?subtemplate ... . ?template)
  ;;  #(?subtemplate ...)
  ;;
  ;;in  which:  ?DATUM  is  a literal  datum,  ?PATTERN-VARIABLE  is  an
  ;;identifier referencing  a pattern  variable created  by SYNTAX-CASE,
  ;;?ID   is  an   identifier  not   referencing  a   pattern  variable,
  ;;?SUBTEMPLATE  is  a  template  followed by  zero  or  more  ellipsis
  ;;identifiers.
  ;;
  ;;Return a sexp representing  code in the core language
  ;;which, when evaluated, returns a  wrapped or unwrapped syntax object
  ;;containing an expression in which:
  ;;
  ;;* All the template identifiers being references to pattern variables
  ;;  are substituted with the corresponding syntax objects.
  ;;
  ;;     (syntax-case #'123 (?obj (syntax ?obj)))
  ;;     => #<syntax expr=123>
  ;;
  ;;     (syntax-case #'(1 2) ((?a ?b) (syntax #(?a ?b))))
  ;;     => #(#<syntax expr=1> #<syntax expr=1>)
  ;;
  ;;* All the identifiers not  being references to pattern variables are
  ;;  left  alone to  be captured  by the lexical  context at  the level
  ;;  below the current,  in the context of the SYNTAX  macro use or the
  ;;  context of the output form.
  ;;
  ;;     (syntax-case #'(1) ((?a) (syntax (display ?b))))
  ;;     => (#<syntax expr=display>
  ;;         #<syntax expr=1> . #<syntax expr=()>)
  ;;
  ;;* All the sub-templates followed by ellipsis are replicated to match
  ;;  the input pattern.
  ;;
  ;;     (syntax-case #'(1 2 3) ((?a ...) (syntax #(?a ...))))
  ;;     => #(1 2 3)
  ;;
  ;;About pattern variables:  they are present in  a lexical environment
  ;;as entries with format:
  ;;
  ;;   (?label . (syntax . (?name . ?level)))
  ;;
  ;;where:  ?LABEL  is the  label  in  the identifier's  syntax  object,
  ;;"syntax" is  the symbol "syntax",  ?NAME is the  symbol representing
  ;;the  name  of the  pattern  variable,  ?LEVEL  is an  exact  integer
  ;;representing the  nesting ellipsis level.  The  SYNTAX-CASE patterns
  ;;below will generate the given entries:
  ;;
  ;;   ?a			->  (syntax . (?a . 0))
  ;;   (?a)			->  (syntax . (?a . 0))
  ;;   (((?a)))			->  (syntax . (?a . 0))
  ;;   (?a ...)			->  (syntax . (?a . 1))
  ;;   ((?a) ...)		->  (syntax . (?a . 1))
  ;;   ((((?a))) ...)		->  (syntax . (?a . 1))
  ;;   ((?a ...) ...)		->  (syntax . (?a . 2))
  ;;   (((?a ...) ...) ...)	->  (syntax . (?a . 3))
  ;;
  ;;The  input template  is  first visited  in  post-order, building  an
  ;;intermediate  symbolic  representation  of  it;  then  the  symbolic
  ;;representation is visited in post-order, building core language code
  ;;that  evaluates  to  the   resulting  syntax  object.   Examples  of
  ;;intermediate  representation  (-->)  and  expansion  (==>)  follows,
  ;;assuming identifiers starting with "?"  are pattern variables:
  #|
      (syntax display)
      --> (quote #<syntax expr=display>)
      ==> (quote #<syntax expr=display>)

      (syntax (display 123))
      --> (quote #<syntax expr=(display 123)>)
      ==> (quote #<syntax expr=(display 123)>)

      (syntax ?a)
      --> (ref ?a)
      ==> ?a

      (syntax (?a))
      --> (cons (ref ?a) (quote #<syntax expr=()>))
      ==> ((primitive cons) ?a (quote #<syntax expr=()>))

      (syntax (?a 1))
      --> (cons (ref ?a) (quote #<syntax expr=(1)>))
      ==> ((primitive cons) ?a (quote #<syntax expr=(1)>))

      (syntax (1 ?a 2))
      --> (cons (quote #<syntax expr=1>)
                (cons (ref ?a) (quote #<syntax expr=(2)>)))
      ==> ((primitive cons)
	   (quote #<syntax expr=1>)
	   ((primitive cons) ?a (quote #<syntax expr=(2)>)))

      (syntax (display ?a))
      ==> (cons
	   (quote #<syntax expr=display>)
	   (cons (ref ?a) (quote #<syntax expr=()>)))
      ==> ((primitive cons)
	   (quote #<syntax expr=display>)
	   ((primitive cons) ?a (quote #<syntax expr=()>)))

      (syntax #(?a))
      --> (vector (ref ?a))
      ==> ((primitive vector) ?a)

      (syntax (?a ...))
      --> (ref ?a)
      ==> ?a

      (syntax ((?a ...) ...))
      --> (ref ?a)
      ==> ?a

      (syntax ((?a ?b ...) ...))
      -- (map (primitive cons) (ref ?a) (ref ?b))
      ==> ((primitive ellipsis-map) (primitive cons) ?a ?b)

      (syntax (((?a ?b ...) ...) ...))
      --> (map (lambda (tmp2 tmp1)
                 (map (primitive cons) tmp1 tmp2))
	    (ref ?b) (ref ?a))
      ==> ((primitive ellipsis-map)
	   (case-lambda
	    ((tmp2 tmp1)
	     ((primitive ellipsis-map) (primitive cons) tmp1 tmp2)))
           ?b ?a)

      (syntax ((?a (?a ...)) ...))
      --> (map (lambda (tmp)
                 (cons (ref tmp)
		       (cons (ref ?a)
			     (quote #<syntax expr=()>))))
            (ref ?a))
      ==> ((primitive ellipsis-map)
           (case-lambda
            ((tmp)
             ((primitive cons) tmp
	                       ((primitive cons) ?a
                                        	 (quote #<syntax expr=()>)))))
           ?a)
  |#
  (define (syntax-transformer use-stx lexenv.run lexenv.expand)
    (syntax-match use-stx ()
      ((_ ?template)
       (receive (intermediate-sexp maps)
	   (%gen-syntax use-stx ?template lexenv.run '() ellipsis? #f)
	 (let ((code (%generate-output-code intermediate-sexp)))
	   #;(debug-print 'syntax (syntax->datum ?template) intermediate-sexp code)
	   code)))))

  (define (%gen-syntax use-stx template-stx lexenv maps ellipsis? vec?)
    ;;Recursive function.  Expand the contents of a SYNTAX use.
    ;;
    ;;USE-STX must be  the syntax object containing  the original SYNTAX
    ;;macro use; it is used for descriptive error reporting.
    ;;
    ;;TEMPLATE-STX must be the template from the SYNTAX macro use.
    ;;
    ;;LEXENV is  the lexical  environment in  which the  expansion takes
    ;;place;  it must  contain  the pattern  variables  visible by  this
    ;;SYNTAX use.
    ;;
    ;;MAPS is  a list  of alists,  one alist  for each  ellipsis nesting
    ;;level.  If the template has 3 nested ellipsis patterns:
    ;;
    ;;   (((?a ...) ...) ...)
    ;;
    ;;while  we are  processing the  inner "(?a  ...)"  MAPS  contains 3
    ;;alists.  The  alists are  used when processing  ellipsis templates
    ;;that recursively reference the same pattern variable, for example:
    ;;
    ;;   ((?a (?a ...)) ...)
    ;;
    ;;the inner  ?A is mapped  to a gensym which  is used to  generate a
    ;;binding in the output code.
    ;;
    ;;ELLIPSIS? must be a predicate function returning true when applied
    ;;to the  ellipsis identifier from  the built in  environment.  Such
    ;;function  is made  an argument,  so that  it can  be changed  to a
    ;;predicate  returning   always  false   when  we   are  recursively
    ;;processing a quoted template:
    ;;
    ;;   (... ?sub-template)
    ;;
    ;;in which the ellipses in ?SUB-TEMPLATE are to be handled as normal
    ;;identifiers.
    ;;
    ;;VEC? is a boolean: true when this function is processing the items
    ;;of a vector.
    ;;
    (syntax-match template-stx ()

      ;;Standalone ellipses are not allowed.
      ;;
      (?dots
       (ellipsis? ?dots)
       (stx-error use-stx "misplaced ellipsis in syntax form"))

      ;;Match  a standalone  identifier.   ?ID can  be:  a reference  to
      ;;pattern variable created by SYNTAX-CASE; an identifier that will
      ;;be captured by  some binding; an identifier that  will result to
      ;;be free,  in which  case an "unbound  identifier" error  will be
      ;;raised later.
      ;;
      (?id
       (identifier? ?id)
       (let ((binding (label->syntactic-binding (id->label ?id) lexenv)))
	 (if (pattern-variable-binding? binding)
	     ;;It is a reference to pattern variable.
	     (receive (var maps)
		 (let* ((name.level  (syntactic-binding-value binding))
			(name        (car name.level))
			(level       (cdr name.level)))
		   (%gen-ref use-stx name level maps))
	       (values (list 'ref var) maps))
	   ;;It is some other identifier.
	   (values (list 'quote ?id) maps))))

      ;;Ellipses starting a vector template are not allowed:
      ;;
      ;;   #(... 1 2 3)   ==> ERROR
      ;;
      ;;but ellipses  starting a list  template are allowed,  they quote
      ;;the subsequent sub-template:
      ;;
      ;;   (... ...)		==> quoted ellipsis
      ;;   (... ?sub-template)	==> quoted ?SUB-TEMPLATE
      ;;
      ;;so that the ellipses in  the ?SUB-TEMPLATE are treated as normal
      ;;identifiers.  We change the  ELLIPSIS? argument for recursion to
      ;;a predicate that always returns false.
      ;;
      ((?dots ?sub-template)
       (ellipsis? ?dots)
       (if vec?
	   (stx-error use-stx "misplaced ellipsis in syntax form")
	 (%gen-syntax use-stx ?sub-template lexenv maps (lambda (x) #f) #f)))

      ;;Match a template followed by ellipsis.
      ;;
      ((?template ?dots . ?rest)
       (ellipsis? ?dots)
       (let loop
	   ((rest.stx ?rest)
	    (kont     (lambda (maps)
			(receive (template^ maps)
			    (%gen-syntax use-stx ?template lexenv (cons '() maps) ellipsis? #f)
			  (if (null? (car maps))
			      (stx-error use-stx "extra ellipsis in syntax form")
			    (values (%gen-map template^ (car maps))
				    (cdr maps)))))))
	 (syntax-match rest.stx ()
	   (()
	    (kont maps))

	   ((?dots . ?tail)
	    (ellipsis? ?dots)
	    (loop ?tail (lambda (maps)
			  (receive (template^ maps)
			      (kont (cons '() maps))
			    (if (null? (car maps))
				(stx-error use-stx "extra ellipsis in syntax form")
			      (values (%gen-mappend template^ (car maps))
				      (cdr maps)))))))

	   (_
	    (receive (rest^ maps)
		(%gen-syntax use-stx rest.stx lexenv maps ellipsis? vec?)
	      (receive (template^ maps)
		  (kont maps)
		(values (%gen-append template^ rest^) maps))))
	   )))

      ;;Process pair templates.
      ;;
      ((?car . ?cdr)
       (receive (car.new maps)
	   (%gen-syntax use-stx ?car lexenv maps ellipsis? #f)
	 (receive (cdr.new maps)
	     (%gen-syntax use-stx ?cdr lexenv maps ellipsis? vec?)
	   (values (%gen-cons template-stx ?car ?cdr car.new cdr.new)
		   maps))))

      ;;Process a vector template.  We set to true the VEC? argument for
      ;;recursion.
      ;;
      (#(?item* ...)
       (receive (item*.new maps)
	   (%gen-syntax use-stx ?item* lexenv maps ellipsis? #t)
	 (values (%gen-vector template-stx ?item* item*.new)
		 maps)))

      ;;Everything else is just quoted in the output.  This includes all
      ;;the literal datums.
      ;;
      (_
       (values `(quote ,template-stx) maps))
      ))

  (define (%gen-ref use-stx var level maps)
    ;;Recursive function.
    ;;
    #;(debug-print 'gen-ref maps)
    (if (zero? level)
	(values var maps)
      (if (null? maps)
	  (stx-error use-stx "missing ellipsis in syntax form")
	(receive (outer-var outer-maps)
	    (%gen-ref use-stx var (- level 1) (cdr maps))
	  (cond ((assq outer-var (car maps))
		 => (lambda (b)
		      (values (cdr b) maps)))
		(else
		 (let ((inner-var (gensym-for-lexical-var 'tmp)))
		   (values inner-var
			   (cons (cons (cons outer-var inner-var)
				       (car maps))
				 outer-maps)))))))))

  (define (%gen-append x y)
    (if (equal? y '(quote ()))
	x
      `(append ,x ,y)))

  (define (%gen-mappend e map-env)
    `(apply (primitive append) ,(%gen-map e map-env)))

  (define (%gen-map e map-env)
    (let ((formals (map cdr map-env))
	  (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
      (cond
       ;; identity map equivalence:
       ;; (map (lambda (x) x) y) == y
       ((eq? (car e) 'ref)
	(car actuals))
       ;; eta map equivalence:
       ;; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
       ((for-all
	    (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
	  (cdr e))
	(let ((args (map (let ((r (map cons formals actuals)))
			   (lambda (x) (cdr (assq (cadr x) r))))
		      (cdr e))))
	  `(map (primitive ,(car e)) . ,args)))
       (else
	(cons* 'map (list 'lambda formals e) actuals)))))

  (define (%gen-cons e x y x.new y.new)
    (case (car y.new)
      ((quote)
       (cond ((eq? (car x.new) 'quote)
	      (let ((x.new (cadr x.new))
		    (y.new (cadr y.new)))
		(if (and (eq? x.new x)
			 (eq? y.new y))
		    `(quote ,e)
		  `(quote ,(cons x.new y.new)))))
	     ((null? (cadr y.new))
	      `(list ,x.new))
	     (else
	      `(cons ,x.new ,y.new))))
      ((list)
       `(list ,x.new . ,(cdr y.new)))
      (else
       `(cons ,x.new ,y.new))))

  (define (%gen-vector e ls lsnew)
    (cond ((eq? (car lsnew) 'quote)
	   (if (eq? (cadr lsnew) ls)
	       `(quote ,e)
	     `(quote #(,@(cadr lsnew)))))

	  ((eq? (car lsnew) 'list)
	   `(vector . ,(cdr lsnew)))

	  (else
	   `(list->vector ,lsnew))))

  (define (%generate-output-code x)
    ;;Recursive function.
    ;;
    (case (car x)
      ((ref)
       (build-lexical-reference no-source (cadr x)))
      ((primitive)
       (build-primref no-source (cadr x)))
      ((quote)
       (build-data no-source (cadr x)))
      ((lambda)
       (build-lambda no-source (cadr x) (%generate-output-code (caddr x))))
      ((map)
       (let ((ls (map %generate-output-code (cdr x))))
	 (build-application no-source
			    (build-primref no-source 'ellipsis-map)
			    ls)))
      (else
       (build-application no-source
			  (build-primref no-source (car x))
			  (map %generate-output-code (cdr x))))))

  #| end of module: syntax-transformer |# )


;;;; module core-macro-transformer: SYNTAX-CASE

(module (syntax-case-transformer)
  ;;Transformer function used to expand R6RS's SYNTAX-CASE syntaxes from
  ;;the top-level built in environment.  Process the contents of USE-STX
  ;;in  the   context  of   the  lexical  environments   LEXENV.RUN  and
  ;;LEXENV.EXPAND.
  ;;
  ;;Notice  that   the  parsing   of  the   patterns  is   performed  by
  ;;CONVERT-PATTERN at  expand time and  the actual pattern  matching is
  ;;performed by SYNTAX-DISPATCH at run time.
  ;;
  (define (syntax-case-transformer use-stx lexenv.run lexenv.expand)
    (syntax-match use-stx ()
      ((_ ?expr (?literal* ...) ?clauses* ...)
       (%verify-literals ?literal* use-stx)
       (let* ( ;;The identifier to  which the result of evaluating the
	      ;;?EXPR is bound.
	      (expr.id    (gensym-for-lexical-var 'tmp))
	      ;;The full SYNTAX-CASE  pattern matching code, generated
	      ;;and transformed to core language.
	      (body.core  (%gen-syntax-case expr.id ?literal* ?clauses*
					    lexenv.run lexenv.expand))
	      ;;The ?EXPR transformed to core language.
	      (expr.core  (chi-expr ?expr lexenv.run lexenv.expand)))
	 ;;Return a form like:
	 ;;
	 ;;   ((lambda (expr.id) body.core) expr.core)
	 ;;
	 (build-application no-source
	   (build-lambda no-source (list expr.id) body.core)
	   (list expr.core))))
      ))

  (define (%gen-syntax-case expr.id literals clauses lexenv.run lexenv.expand)
    ;;Recursive function.  Generate and return the full pattern matching
    ;;code in the core language to match the given CLAUSES.
    ;;
    (syntax-match clauses ()
      ;;No pattern matched the input  expression: return code to raise a
      ;;syntax error.
      ;;
      (()
       (build-application no-source
	 (build-primref no-source 'syntax-error)
	 (list (build-lexical-reference no-source expr.id))))

      ;;The pattern  is a standalone  identifier, neither a  literal nor
      ;;the ellipsis,  and it  has no  fender.  A  standalone identifier
      ;;with no fender matches everything,  so it is useless to generate
      ;;the code  for the next clauses:  the code generated here  is the
      ;;last one.
      ;;
      (((?pattern ?output-expr) . ?unused-clauses)
       (and (identifier? ?pattern)
	    (not (bound-id-member? ?pattern literals))
	    (not (ellipsis? ?pattern)))
       (if (free-id=? ?pattern (scheme-stx '_))
	   ;;The clause is:
	   ;;
	   ;;   (_ ?output-expr)
	   ;;
	   ;;the underscore  identifier matches everything and  binds no
	   ;;pattern variables.
	   (chi-expr ?output-expr lexenv.run lexenv.expand)
	 ;;The clause is:
	 ;;
	 ;;   (?id ?output-expr)
	 ;;
	 ;;a standalone identifier matches everything  and binds it to a
	 ;;pattern variable whose name is ?ID.
	 (let ((label (gensym-for-label ?pattern))
	       (lex   (gensym-for-lexical-var ?pattern)))
	   ;;The expression  must be  expanded in a  lexical environment
	   ;;augmented with the pattern variable.
	   (define output-expr^
	     (push-lexical-contour
		 (make-filled-rib (list ?pattern) (list label))
	       ?output-expr))
	   (define lexenv.run^
	     ;;Push a pattern variable entry to the lexical environment.
	     ;;The ellipsis nesting level is 0.
	     (cons (cons label (make-binding 'syntax (cons lex 0)))
		   lexenv.run))
	   (define output-expr.core
	     (chi-expr output-expr^ lexenv.run^ lexenv.expand))
	   (build-application no-source
	     (build-lambda no-source
	       (list lex)
	       output-expr.core)
	     (list (build-lexical-reference no-source expr.id))))))

      ;;The  pattern is  neither  a standalone  pattern  variable nor  a
      ;;standalone underscore.  It has no fender, which is equivalent to
      ;;having a "#t" as fender.
      ;;
      (((?pattern ?output-expr) . ?next-clauses)
       (%gen-clause expr.id literals
		    ?pattern #t #;fender
		    ?output-expr
		    lexenv.run lexenv.expand
		    ?next-clauses))

      ;;The pattern has a fender.
      ;;
      (((?pattern ?fender ?output-expr) . ?next-clauses)
       (%gen-clause expr.id literals
		    ?pattern ?fender ?output-expr
		    lexenv.run lexenv.expand
		    ?next-clauses))
      ))

  (define (%gen-clause expr.id literals
		       pattern.stx fender.stx output-expr.stx
		       lexenv.run lexenv.expand
		       next-clauses)
    ;;Generate  the  code needed  to  match  the clause  represented  by
    ;;PATTERN.STX, FENDER.STX and  OUTPUT-EXPR.STX; recursively generate
    ;;the code to match the other clauses in NEXT-CLAUSES.
    ;;
    ;;When there is a fender, we build the output form (pseudo-code):
    ;;
    ;;  ((lambda (y)
    ;;      (if (if y
    ;;              (fender-matches?)
    ;;            #f)
    ;;          (output-expr)
    ;;        (match-next-clauses))
    ;;   (syntax-dispatch expr.id pattern))
    ;;
    ;;when there is no fender, build the output form (pseudo-code):
    ;;
    ;;  ((lambda (tmp)
    ;;      (if tmp
    ;;          (output-expr)
    ;;        (match-next-clauses))
    ;;   (syntax-dispatch expr.id pattern))
    ;;
    ;;notice that the  return value of SYNTAX-DISPATCH is:  false if the
    ;;pattern did not match, otherwise the list of values to be bound to
    ;;the pattern variables.
    ;;
    (receive (pattern.dispatch pvars.levels)
	;;CONVERT-PATTERN  return 2  values: the  pattern in  the format
	;;accepted by SYNTAX-DISPATCH, an alist representing the pattern
	;;variables:
	;;
	;;* The keys of the alist are identifiers representing the names
	;;  of the pattern variables.
	;;
	;;*  The values  of the  alist are  non-negative exact  integers
	;;  representing the ellipsis nesting level of the corresponding
	;;  pattern variable.  See SYNTAX-TRANSFORMER for details.
	;;
	(convert-pattern pattern.stx literals)
      (let ((pvars (map car pvars.levels)))
	(unless (distinct-bound-ids? pvars)
	  (%invalid-ids-error pvars pattern.stx "pattern variable")))
      (unless (for-all (lambda (x)
			 (not (ellipsis? (car x))))
		pvars.levels)
	(stx-error pattern.stx "misplaced ellipsis in syntax-case pattern"))
      (let* ((tmp-sym      (gensym-for-lexical-var 'tmp))
	     (fender-cond  (%build-fender-conditional expr.id literals tmp-sym pvars.levels
						      fender.stx output-expr.stx
						      lexenv.run lexenv.expand
						      next-clauses)))
	(build-application no-source
	  (build-lambda no-source
	    (list tmp-sym)
	    fender-cond)
	  (list
	   (build-application no-source
	     (build-primref no-source 'syntax-dispatch)
	     (list (build-lexical-reference no-source expr.id)
		   (build-data no-source pattern.dispatch))))))))

  (define (%build-fender-conditional expr.id literals tmp-sym pvars.levels
				     fender.stx output-expr.stx
				     lexenv.run lexenv.expand
				     next-clauses)
    ;;Generate the  code that tests  the fender: if the  fender succeeds
    ;;run the output expression, else try to match the next clauses.
    ;;
    ;;When there is a fender, we build the output form (pseudo-code):
    ;;
    ;;   (if (if y
    ;;           (fender-matches?)
    ;;         #f)
    ;;       (output-expr)
    ;;     (match-next-clauses))
    ;;
    ;;when there is no fender, build the output form (pseudo-code):
    ;;
    ;;   (if tmp
    ;;       (output-expr)
    ;;     (match-next-clauses))
    ;;
    (define-inline (%build-call expr.stx)
      (%build-dispatch-call pvars.levels expr.stx tmp-sym lexenv.run lexenv.expand))
    (let ((test     (if (eq? fender.stx #t)
			;;There is no fender.
			tmp-sym
		      ;;There is a fender.
		      (build-conditional no-source
			(build-lexical-reference no-source tmp-sym)
			(%build-call fender.stx)
			(build-data no-source #f))))
	  (conseq    (%build-call output-expr.stx))
	  (altern    (%gen-syntax-case expr.id literals next-clauses lexenv.run lexenv.expand)))
      (build-conditional no-source
	test conseq altern)))

  (define (%build-dispatch-call pvars.levels expr.stx tmp-sym lexenv.run lexenv.expand)
    ;;Generate  code to  evaluate EXPR.STX  in an  environment augmented
    ;;with the pattern variables defined by PVARS.LEVELS.  Return a core
    ;;language expression representing the following pseudo-code:
    ;;
    ;;   (apply (lambda (pattern-var ...) expr) tmp)
    ;;
    (define ids
      ;;For each pattern variable: the identifier representing its name.
      (map car pvars.levels))
    (define labels
      ;;For each pattern variable: a gensym used as label in the lexical
      ;;environment.
      (map gensym-for-label ids))
    (define names
      ;;For each pattern variable: a gensym used as unique variable name
      ;;in the lexical environment.
      (map gensym-for-lexical-var ids))
    (define levels
      ;;For  each pattern  variable: an  exact integer  representing the
      ;;ellipsis nesting level.  See SYNTAX-TRANSFORMER for details.
      (map cdr pvars.levels))
    (define bindings
      ;;For each pattern variable: a binding to be pushed on the lexical
      ;;environment.
      (map (lambda (label name level)
	     (cons label (make-binding 'syntax (cons name level))))
	labels names levels))
    (define expr.core
      ;;Expand the  expression in  a lexical environment  augmented with
      ;;the pattern variables.
      ;;
      ;;NOTE We could have created a syntax object:
      ;;
      ;;  #`(lambda (pvar ...) #,expr.stx)
      ;;
      ;;and then  expanded it:  EXPR.STX would have  been expanded  in a
      ;;lexical environment augmented with the PVAR bindings.
      ;;
      ;;Instead we have chosen to push  the PVAR bindings on the lexical
      ;;environment "by hand", then to  expand EXPR.STX in the augmented
      ;;environment,  finally   to  put  the  resulting   core  language
      ;;expression in a core language LAMBDA syntax.
      ;;
      ;;The two methods are fully equivalent;  the one we have chosen is
      ;;a bit faster.
      ;;
      (chi-expr (push-lexical-contour
		    (make-filled-rib ids labels)
		  expr.stx)
		(append bindings lexenv.run)
		lexenv.expand))
    (build-application no-source
      (build-primref no-source 'apply)
      (list (build-lambda no-source names expr.core)
	    (build-lexical-reference no-source tmp-sym))))

  (define (%invalid-ids-error id* e class)
    (let find ((id* id*)
	       (ok* '()))
      (if (null? id*)
	  (stx-error e) ; shouldn't happen
	(if (identifier? (car id*))
	    (if (bound-id-member? (car id*) ok*)
		(syntax-error (car id*) "duplicate " class)
	      (find (cdr id*) (cons (car id*) ok*)))
	  (syntax-error (car id*) "invalid " class)))))

  #| end of module: SYNTAX-CASE-TRANSFORMER |# )


;;;; module core-macro-transformer: SPLICE-FIRST-EXPAND

(define (splice-first-expand-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  Vicare's SPLICE-FIRST-EXPAND
  ;;syntaxes  from the  top-level  built in  environment.  Expand  the
  ;;syntax object EXPR-STX in the  context of the given LEXENV; return
  ;;an expanded language symbolic expression.
  ;;
  (import SPLICE-FIRST-ENVELOPE)
  (syntax-match expr-stx ()
    ((_ ?form)
     (make-splice-first-envelope ?form))
    ))


;;;; module core-macro-transformer: UNSAFE

(define (unsafe-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  Vicare's UNSAFE macros from the
  ;;top-level built in environment.  Expand  the contents of EXPR-STX in
  ;;the  context  of  the  given LEXENV;  return  an  expanded  language
  ;;symbolic expression.
  ;;
  (define-constant __who__
    'unsafe)
  (syntax-match expr-stx ()
    ((_ ?id)
     (identifier? ?id)
     (chi-expr (cond ((parametrise ((current-run-lexenv (lambda () lexenv.run)))
			(syntactic-binding-getprop ?id *UNSAFE-VARIANT-COOKIE*)))
		     (else
		      ;;This warning will not abort the process.
		      (%raise-warning __who__ "requested unavailable unsafe variant"
				      (or (expression-position expr-stx)
					  (expression-position ?id))
				      ?id)
		      ?id))
	       lexenv.run lexenv.expand))
    ))


;;;; module core-macro-transformer: PREDICATE-PROCEDURE-ARGUMENT-VALIDATION, PREDICATE-RETURN-VALUE-VALIDATION

(define (predicate-procedure-argument-validation-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer      function      used     to      expand      Vicare's
  ;;PREDICATE-PROCEDURE-ARGUMENT-VALIDATION  macros  from the  top-level
  ;;built  in  environment.  Expand  the  contents  of EXPR-STX  in  the
  ;;context of  the given LEXENV;  return an expanded  language symbolic
  ;;expression.
  ;;
  (define-constant __who__
    'predicate-procedure-argument-validation)
  (syntax-match expr-stx ()
    ((_ ?id)
     (identifier? ?id)
     (chi-expr (cond ((parametrise ((current-run-lexenv (lambda () lexenv.run)))
			(syntactic-binding-getprop ?id
			  *PREDICATE-PROCEDURE-ARGUMENT-VALIDATION-COOKIE*)))
		     (else
		      (stx-error expr-stx "undefined procedure argument validation")))
	       lexenv.run lexenv.expand))
    ))

(define (predicate-return-value-validation-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer      function      used     to      expand      Vicare's
  ;;PREDICATE-RETURN-VALUE-VALIDATION macros from the top-level built in
  ;;environment.  Expand the contents of  EXPR-STX in the context of the
  ;;given LEXENV; return an expanded language symbolic expression.
  ;;
  (define-constant __who__
    'predicate-return-value-validation)
  (syntax-match expr-stx ()
    ((_ ?id)
     (identifier? ?id)
     (chi-expr (cond ((parametrise ((current-run-lexenv (lambda () lexenv.run)))
			(syntactic-binding-getprop ?id
			  *PREDICATE-RETURN-VALUE-VALIDATION-COOKIE*)))
		     (else
		      (stx-error expr-stx "undefined return value validation")))
	       lexenv.run lexenv.expand))
    ))


;;;; module core-macro-transformer: struct type descriptor, setter and getter

(module (struct-type-descriptor-transformer
	 struct-type-and-struct?-transformer
	 struct-type-field-ref-transformer
	 struct-type-field-set!-transformer
	 $struct-type-field-ref-transformer
	 $struct-type-field-set!-transformer)

  (define (struct-type-descriptor-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer   function  used   to  expand   STRUCT-TYPE-DESCRIPTOR
    ;;syntaxes  from the  top-level  built in  environment.  Expand  the
    ;;syntax object EXPR-STX in the  context of the given LEXENV; return
    ;;an expanded language symbolic expression.
    ;;
    ;;FIXME This transformer is  currently unused because the identifier
    ;;STRUCT-TYPE-DESCRIPTOR  is bound  to  a function.   In future  the
    ;;function  binding  will be  removed  and  replaced by  the  syntax
    ;;binding.  (Marco Maggi; Fri Jan 31, 2014)
    ;;
    (define-constant __who__ 'struct-type-descriptor)
    (syntax-match expr-stx ()
      ((_ ?type-id)
       (identifier? ?type-id)
       (build-data no-source
	 (%struct-type-id->rtd __who__ expr-stx ?type-id lexenv.run)))
      ))

  (define (struct-type-and-struct?-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer  function   used  to   expand  STRUCT-TYPE-AND-STRUCT?
    ;;syntaxes  from the  top-level  built in  environment.  Expand  the
    ;;syntax object EXPR-STX in the  context of the given LEXENV; return
    ;;an expanded language symbolic expression.
    ;;
    (define-constant __who__ 'struct-type-and-struct?)
    (syntax-match expr-stx ()
      ((_ ?type-id ?stru)
       (identifier? ?type-id)
       (let ((rtd (%struct-type-id->rtd __who__ expr-stx ?type-id lexenv.run)))
	 (chi-expr (bless
		    `($struct/rtd? ,?stru (quote ,rtd)))
		   lexenv.run lexenv.expand)))
      ))

;;; --------------------------------------------------------------------

  (module (struct-type-field-ref-transformer
	   $struct-type-field-ref-transformer)

    (define (struct-type-field-ref-transformer expr-stx lexenv.run lexenv.expand)
      ;;Transformer  function   used  to   expand  STRUCT-TYPE-FIELD-REF
      ;;syntaxes from  the top-level  built in environment.   Expand the
      ;;syntax  object EXPR-STX  in  the context  of  the given  LEXENV;
      ;;return an expanded language symbolic expression.
      ;;
      (%struct-type-field-ref-transformer 'struct-type-field-ref #t expr-stx lexenv.run lexenv.expand))

    (define ($struct-type-field-ref-transformer expr-stx lexenv.run lexenv.expand)
      ;;Transformer  function  used   to  expand  $STRUCT-TYPE-FIELD-REF
      ;;syntaxes from  the top-level  built in environment.   Expand the
      ;;syntax  object EXPR-STX  in  the context  of  the given  LEXENV;
      ;;return an expanded language symbolic expression.
      ;;
      (%struct-type-field-ref-transformer '$struct-type-field-ref #f expr-stx lexenv.run lexenv.expand))

    (define (%struct-type-field-ref-transformer who safe? expr-stx lexenv.run lexenv.expand)
      (syntax-match expr-stx ()
	((_ ?type-id ?field-id ?stru)
	 (and (identifier? ?type-id)
	      (identifier? ?field-id))
	 (let* ((rtd         (%struct-type-id->rtd who expr-stx ?type-id lexenv.run))
		(field-names (struct-type-field-names rtd))
		(field-idx   (%field-name->field-idx who expr-stx field-names ?field-id)))
	   (chi-expr (bless
		      (if safe?
			  `(struct-ref ,?stru ,field-idx)
			`($struct-ref ,?stru ,field-idx)))
		     lexenv.run lexenv.expand)))
	))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (struct-type-field-set!-transformer
	   $struct-type-field-set!-transformer)

    (define (struct-type-field-set!-transformer expr-stx lexenv.run lexenv.expand)
      ;;Transformer  function  used   to  expand  STRUCT-TYPE-FIELD-SET!
      ;;syntaxes from  the top-level  built in environment.   Expand the
      ;;syntax  object EXPR-STX  in  the context  of  the given  LEXENV;
      ;;return an expanded language symbolic expression.
      ;;
      (%struct-type-field-set!-transformer 'struct-type-field-ref #t expr-stx lexenv.run lexenv.expand))

    (define ($struct-type-field-set!-transformer expr-stx lexenv.run lexenv.expand)
      ;;Transformer  function  used  to  expand  $STRUCT-TYPE-FIELD-SET!
      ;;syntaxes from  the top-level  built in environment.   Expand the
      ;;syntax  object EXPR-STX  in  the context  of  the given  LEXENV;
      ;;return an expanded language symbolic expression.
      ;;
      (%struct-type-field-set!-transformer '$struct-type-field-ref #f expr-stx lexenv.run lexenv.expand))

    (define (%struct-type-field-set!-transformer who safe? expr-stx lexenv.run lexenv.expand)
      (syntax-match expr-stx ()
	((_ ?type-id ?field-id ?stru ?new-value)
	 (and (identifier? ?type-id)
	      (identifier? ?field-id))
	 (let* ((rtd         (%struct-type-id->rtd who expr-stx ?type-id lexenv.run))
		(field-names (struct-type-field-names rtd))
		(field-idx   (%field-name->field-idx who expr-stx field-names ?field-id)))
	   (chi-expr (bless
		      (if safe?
			  `(struct-set! ,?stru ,field-idx ,?new-value)
			`($struct-set! ,?stru ,field-idx ,?new-value)))
		     lexenv.run lexenv.expand)))
	))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define (%struct-type-id->rtd who expr-stx type-id lexenv.run)
    ;;Given the identifier  of the struct type: find its  label then its
    ;;syntactic binding  and return the  struct type descriptor.   If no
    ;;binding captures the identifier or the binding does not describe a
    ;;structure type descriptor: raise an exception.
    ;;
    (cond ((id->label type-id)
	   => (lambda (label)
		(let ((binding (label->syntactic-binding label lexenv.run)))
		  (if (struct-type-descriptor-binding? binding)
		      (syntactic-binding-value binding)
		    (syntax-violation who "not a struct type" expr-stx type-id)))))
	  (else
	   (%raise-unbound-error who expr-stx type-id))))

  (define (%field-name->field-idx who expr-stx field-names field-id)
    ;;Given a list of symbols  FIELD-NAMES representing a struct's field
    ;;names and an identifier FIELD-ID representing the name of a field:
    ;;return the index of the selected field in the list.
    ;;
    (define field-sym (identifier->symbol field-id))
    (let loop ((i 0) (ls field-names))
      (if (pair? ls)
	  (if (eq? field-sym ($car ls))
	      i
	    (loop ($fxadd1 i) ($cdr ls)))
	(syntax-violation who
	  "invalid struct type field name" expr-stx field-id))))

  #| end of module |# )


;;;; module core-macro-transformer: RECORD-{TYPE,CONSTRUCTOR}-DESCRIPTOR, field setter and getter

(module (record-type-descriptor-transformer
	 record-constructor-descriptor-transformer
	 record-type-field-set!-transformer
	 record-type-field-ref-transformer
	 $record-type-field-set!-transformer
	 $record-type-field-ref-transformer)
  ;;The syntactic  binding representing the R6RS  record type descriptor
  ;;and record constructor descriptor has one of the formats:
  ;;
  ;;   ($rtd . (?rtd-id ?rcd-id))
  ;;   ($rtd . (?rtd-id ?rcd-id . ?spec))
  ;;
  ;;where: "$rtd"  is the  symbol "$rtd"; ?RTD-ID  is the  identifier to
  ;;which the record type descriptor is bound; ?RCD-ID is the identifier
  ;;to which the  default record constructor descriptor  is bound; ?SPEC
  ;;is a record of type R6RS-RECORD-TYPE-SPEC.
  ;;
  (import R6RS-RECORD-TYPE-SPEC)

  (define (record-type-descriptor-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer   function  used   to  expand   RECORD-TYPE-DESCRIPTOR
    ;;syntaxes  from the  top-level  built in  environment.  Expand  the
    ;;syntax object EXPR-STX in the  context of the given LEXENV; return
    ;;an expanded language symbolic expression.
    ;;
    (define-constant __who__ 'record-type-descriptor)
    (syntax-match expr-stx ()
      ((_ ?type-name)
       (identifier? ?type-name)
       (chi-expr (r6rs-record-type-descriptor-binding-rtd
		  (id->r6rs-record-type-descriptor-binding __who__ expr-stx ?type-name lexenv.run))
		 lexenv.run lexenv.expand))
      ))

  (define (record-constructor-descriptor-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer function used  to expand RECORD-CONSTRUCTOR-DESCRIPTOR
    ;;syntaxes  from the  top-level  built in  environment.  Expand  the
    ;;syntax object EXPR-STX in the  context of the given LEXENV; return
    ;;an expanded language symbolic expression.
    ;;
    (define-constant __who__ 'record-constructor-descriptor)
    (syntax-match expr-stx ()
      ((_ ?type-name)
       (identifier? ?type-name)
       (chi-expr (r6rs-record-type-descriptor-binding-rcd
		  (id->r6rs-record-type-descriptor-binding __who__ expr-stx ?type-name lexenv.run))
		 lexenv.run lexenv.expand))
      ))

;;; --------------------------------------------------------------------

  (let-syntax
      ((define-getter-transformer
	 (syntax-rules ()
	   ((_ ?who ?transformer ?actor-getter)
	    (define (?transformer expr-stx lexenv.run lexenv.expand)
	      ;;Transformer function  used to expand ?who  syntaxes from
	      ;;the top-level  built in environment.  Expand  the syntax
	      ;;object  EXPR-STX in  the  context of  the given  LEXENV;
	      ;;return an expanded language symbolic expression.
	      ;;
	      (define-constant __who__ '?who)
	      (syntax-match expr-stx ()
		((_ ?type-name ?field-name ?record)
		 (and (identifier? ?type-name)
		      (identifier? ?field-name))
		 (let* ((synner   (lambda (message)
				    (syntax-violation __who__ message expr-stx ?type-name)))
			(binding  (id->r6rs-record-type-descriptor-binding __who__ expr-stx ?type-name lexenv.run))
			(accessor (?actor-getter binding ?field-name synner)))
		   (chi-expr (bless
			      (list accessor ?record))
			     lexenv.run lexenv.expand)))
		))
	    ))))
    (define-getter-transformer record-type-field-ref
      record-type-field-ref-transformer  r6rs-record-type-descriptor-binding-safe-accessor)
    (define-getter-transformer $record-type-field-ref
      $record-type-field-ref-transformer r6rs-record-type-descriptor-binding-unsafe-accessor))

;;; --------------------------------------------------------------------

  (let-syntax
      ((define-setter-transformer
	 (syntax-rules ()
	   ((_ ?who ?transformer ?actor-getter)
	    (define (?transformer expr-stx lexenv.run lexenv.expand)
	      ;;Transformer function  used to expand ?WHO  syntaxes from
	      ;;the top-level  built in environment.  Expand  the syntax
	      ;;object  EXPR-STX in  the  context of  the given  LEXENV;
	      ;;return an expanded language symbolic expression.
	      ;;
	      (define-constant __who__ '?who)
	      (syntax-match expr-stx ()
		((_ ?type-name ?field-name ?record ?new-value)
		 (and (identifier? ?type-name)
		      (identifier? ?field-name))
		 (let* ((synner  (lambda (message)
				   (syntax-violation __who__ message expr-stx ?type-name)))
			(binding (id->r6rs-record-type-descriptor-binding __who__ expr-stx ?type-name lexenv.run))
			(mutator (?actor-getter binding ?field-name synner)))
		   (chi-expr (bless
			      (list mutator ?record ?new-value))
			     lexenv.run lexenv.expand)))
		))
	    ))))
    (define-setter-transformer record-type-field-set!
      record-type-field-set!-transformer  r6rs-record-type-descriptor-binding-safe-mutator)
    (define-setter-transformer $record-type-field-set!
      $record-type-field-set!-transformer r6rs-record-type-descriptor-binding-unsafe-mutator))

  #| end of module |# )


;;;; module core-macro-transformer: TYPE-DESCRIPTOR, IS-A?

(define (type-descriptor-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand TYPE-DESCRIPTOR  syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  ;;The result must be an expression evaluating to:
  ;;
  ;;* A Vicare  struct type descriptor if the  given identifier argument
  ;;  is a struct type name.
  ;;
  ;;* A R6RS record type descriptor  if the given identifier argument is
  ;;  a record type name.
  ;;
  ;;* An expand-time OBJECT-SPEC instance.
  ;;
  (define-constant __who__ 'type-descriptor)
  (syntax-match expr-stx ()
    ((_ ?type-id)
     (identifier? ?type-id)
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run binding)
       ((r6rs-record-type)
	(chi-expr (r6rs-record-type-descriptor-binding-rtd binding)
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(build-data no-source
	  (syntactic-binding-value binding)))
       ((object-spec-type)
	(build-data no-source
	  (identifier-object-spec ?type-id)))
       ))
    ))

(define (is-a?-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's IS-A?   syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ 'is-a?)
  (syntax-match expr-stx (<>)
    ((_ <> ?type-id)
     (identifier? ?type-id)
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(lambda (obj)
		      (record-type-and-record? ,?type-id obj)))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(lambda (obj)
		      (struct-type-and-struct? ,?type-id obj)))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (chi-expr (object-spec-pred-id spec)
		    lexenv.run lexenv.expand)))
       ))

    ((_ ?expr ?type-id)
     (identifier? ?type-id)
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(record-type-and-record? ,?type-id ,?expr))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(struct-type-and-struct? ,?type-id ,?expr))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (chi-expr (bless
		     `(,(object-spec-pred-id spec) ,?expr))
		    lexenv.run lexenv.expand)))
       ))
    ))


;;;; module core-macro-transformer: SLOT-REF, SLOT-SET!, $SLOT-REF, $SLOT-SET!

(define (slot-ref-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to  expand Vicare's SLOT-REF syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ 'slot-ref)
  (syntax-match expr-stx (<>)
    ((_ <> ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(lambda (obj)
		      (record-type-field-ref ,?type-id ,?field-name-id obj)))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(lambda (obj)
		      (struct-type-field-ref ,?type-id ,?field-name-id obj)))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (cond ((object-spec-accessor-maker spec)
		 => (lambda (accessor-maker)
		      (chi-expr (bless
				 (accessor-maker ?field-name-id #t))
				lexenv.run lexenv.expand)))
		(else
		 (syntax-error expr-stx "object type does not provide accessors")))))
       ))

    ((_ ?expr ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(record-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(struct-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (cond ((object-spec-accessor-maker spec)
		 => (lambda (accessor-maker)
		      (chi-expr (bless
				 `(,(accessor-maker ?field-name-id #t) ,?expr))
				lexenv.run lexenv.expand)))
		(else
		 (syntax-error expr-stx "object type does not provide accessors")))))
       ))
    ))

(define (slot-set!-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's SLOT-SET! syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ 'slot-set!)
  (syntax-match expr-stx (<>)
    ((_ <> ?field-name-id ?type-id <>)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(lambda (obj new-value)
		      (record-type-field-set! ,?type-id ,?field-name-id obj new-value)))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(lambda (obj new-value)
		      (struct-type-field-set! ,?type-id ,?field-name-id obj new-value)))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (cond ((object-spec-mutator-maker spec)
		 => (lambda (mutator-maker)
		      (chi-expr (bless
				 (mutator-maker ?field-name-id #t))
				lexenv.run lexenv.expand)))
		(else
		 (syntax-error expr-stx "object type does not provide mutator")))))
       ))

    ((_ ?expr ?field-name-id ?type-id ?new-value)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(record-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(struct-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (cond ((object-spec-mutator-maker spec)
		 => (lambda (mutator-maker)
		      (chi-expr (bless
				 `(,(mutator-maker ?field-name-id #t) ,?expr ,?new-value))
				lexenv.run lexenv.expand)))
		(else
		 (syntax-error expr-stx "object type does not provide mutator")))))
       ))
    ))

(define ($slot-ref-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's $SLOT-REF syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ '$slot-ref)
  (syntax-match expr-stx (<>)
    ((_ <> ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(lambda (obj)
		      ($record-type-field-ref ,?type-id ,?field-name-id obj)))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(lambda (obj)
		      ($struct-type-field-ref ,?type-id ,?field-name-id obj)))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (cond ((object-spec-accessor-maker spec)
		 => (lambda (accessor-maker)
		      (chi-expr (bless
				 (accessor-maker ?field-name-id #f))
				lexenv.run lexenv.expand)))
		(else
		 (syntax-error expr-stx "object type does not provide accessors")))))
       ))

    ((_ ?expr ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `($record-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `($struct-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (cond ((object-spec-accessor-maker spec)
		 => (lambda (accessor-maker)
		      (chi-expr (bless
				 `(,(accessor-maker ?field-name-id #f) ,?expr))
				lexenv.run lexenv.expand)))
		(else
		 (syntax-error expr-stx "object type does not provide accessors")))))
       ))
    ))

(define ($slot-set!-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's $SLOT-SET!  syntaxes
  ;;from the top-level  built in environment.  Expand  the syntax object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ '$slot-set!)
  (syntax-match expr-stx (<>)
    ((_ <> ?field-name-id ?type-id <>)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(lambda (obj new-value)
		      ($record-type-field-set! ,?type-id ,?field-name-id obj new-value)))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(lambda (obj new-value)
		      ($struct-type-field-set! ,?type-id ,?field-name-id obj new-value)))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (cond ((object-spec-mutator-maker spec)
		 => (lambda (mutator-maker)
		      (chi-expr (bless
				 (mutator-maker ?field-name-id #f))
				lexenv.run lexenv.expand)))
		(else
		 (syntax-error expr-stx "object type does not provide mutators")))))
       ))

    ((_ ?expr ?field-name-id ?type-id ?new-value)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `($record-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `($struct-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((object-spec-type)
	(let ((spec (identifier-object-spec ?type-id)))
	  (cond ((object-spec-mutator-maker spec)
		 => (lambda (mutator-maker)
		      (chi-expr (bless
				 `(,(mutator-maker ?field-name-id #f) ,?expr ,?new-value))
				lexenv.run lexenv.expand)))
		(else
		 (syntax-error expr-stx "object type does not provide mutators")))))
       ))
    ))


;;;; module core-macro-transformer

#| end of module |# )


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
	   (assertion-violation 'expand
	     "invalid return value from syntax transformer expression"
	     rv)))))


;;;; formals syntax validation

(define (%verify-formals-syntax formals-stx input-form-stx)
  ;;Verify  that  FORMALS-STX  is  a syntax  object  representing  valid
  ;;formals for  LAMBDA and WITH-SYNTAX syntaxes.   If successful return
  ;;unspecified values, else raise a syntax violation.
  ;;
  (syntax-match formals-stx ()
    ((?id* ...)
     (unless (valid-bound-ids? ?id*)
       (%error-invalid-formals-syntax input-form-stx formals-stx)))

    ((?id* ... . ?rest-id)
     (unless (valid-bound-ids? (cons ?rest-id ?id*))
       (%error-invalid-formals-syntax input-form-stx formals-stx)))

    (_
     (stx-error input-form-stx "invalid syntax"))))

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
    (receive (mark* subst* annotated-expr*)
	(diff (car ($<stx>-mark* base-id))
	      ($<stx>-mark*   new-id)
	      ($<stx>-subst*  new-id)
	      ($<stx>-ae*     new-id)
	      (lambda ()
		(%synner "unmatched identifiers" base-id new-id)))
      (if (and (null? mark*)
	       (null? subst*))
	  object
	(mkstx object mark* subst* annotated-expr*))))

  (define (diff base.mark new.mark* new.subst* new.annotated-expr* error)
    (if (null? new.mark*)
	(error)
      (let ((new.mark1 (car new.mark*)))
	(if (eq? base.mark new.mark1)
	    (values '() (final new.subst*) '())
	  (receive (subst1* subst2*)
	      (split new.subst*)
	    (receive (nm* ns* nae*)
		(diff base.mark (cdr new.mark*) subst2* (cdr new.annotated-expr*) error)
	      (values (cons new.mark1 nm*)
		      (append subst1* ns*)
		      (cons (car new.annotated-expr*) nae*))))))))

  (define (split subst*)
    ;;Non-tail recursive  function.  Split  SUBST* and return  2 values:
    ;;the prefix  of SUBST*  up to  and including  the first  shift, the
    ;;suffix of SUBST* from the first shift excluded to the end.
    ;;
    (if (eq? (car subst*) 'shift)
	(values (list 'shift)
		(cdr subst*))
      (receive (subst1* subst2*)
	  (split (cdr subst*))
	(values (cons (car subst*) subst1*)
		subst2*))))

  (define (final subst*)
    ;;Non-tail recursive  function.  Return the  prefix of SUBST*  up to
    ;;and not including  the first shift.  The returned prefix  is a new
    ;;list spine sharing the cars with SUBST*.
    ;;
    (if (or (null? subst*)
	    (eq? (car subst*) 'shift))
	'()
      (cons (car subst*)
	    (final (cdr subst*)))))

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
	    '() #;subst*
	    '() #;annotated-expr*
	    '() #;pvar*
	    ))

  (define (%match expr pattern mark* subst* annotated-expr* pvar*)
    (cond ((not pvar*)
	   ;;No match.
	   #f)
	  ((eq? pattern '_)
	   ;;Match anything, bind nothing.
	   pvar*)
	  ((eq? pattern 'any)
	   ;;Match anything, bind a pattern variable.
	   (cons (%make-syntax-object expr mark* subst* annotated-expr*)
		 pvar*))
	  ((<stx>? expr)
	   ;;Visit the syntax object.
	   (and (not (top-marked? mark*))
		(receive (mark*^ subst*^ annotated-expr*^)
		    (join-wraps mark* subst* annotated-expr* expr)
		  (%match (<stx>-expr expr) pattern mark*^ subst*^ annotated-expr*^ pvar*))))
	  ((annotation? expr)
	   ;;Visit the ANNOTATION struct.
	   (%match (annotation-expression expr) pattern mark* subst* annotated-expr* pvar*))
	  (else
	   (%match* expr pattern mark* subst* annotated-expr* pvar*))))

  (define (%match* expr pattern mark* subst* annotated-expr* pvar*)
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
	   (%match (car expr) (car pattern) mark* subst* annotated-expr*
		   (%match (cdr expr) (cdr pattern) mark* subst* annotated-expr* pvar*))))

     ;;Match any  proper list  expression and  bind a  pattern variable.
     ;;This happens when the original pattern symbolic expression is:
     ;;
     ;;   (?var ...)
     ;;
     ;;everything  in the  proper  list  must be  bound  to the  pattern
     ;;variable ?VAR.
     ;;
     ((eq? pattern 'each-any)
      (let ((l (%match-each-any expr mark* subst* annotated-expr*)))
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
				      mark* subst* annotated-expr*)))
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
	      (free-id=? (%make-syntax-object expr mark* subst* annotated-expr*)
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
	      (free-id=? (%make-syntax-object expr mark* subst* annotated-expr*)
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
			   mark* subst* annotated-expr* pvar*)
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
		      mark* subst* annotated-expr* pvar*)))

	(else
	 (assertion-violation 'syntax-dispatch "invalid pattern" pattern))))))

  (define (%match-each expr pattern mark* subst* annotated-expr*)
    ;;Recursive function.   The expression  matches if it  is a  list in
    ;;which  every item  matches  PATTERN.   Return null  or  a list  of
    ;;sublists, each sublist being a list of pattern variable values.
    ;;
    (cond ((pair? expr)
	   (let ((first (%match (car expr) pattern mark* subst* annotated-expr* '())))
	     (and first
		  (let ((rest (%match-each (cdr expr) pattern mark* subst* annotated-expr*)))
		    (and rest (cons first rest))))))
	  ((null? expr)
	   '())
	  ((<stx>? expr)
	   (and (not (top-marked? mark*))
		(receive (mark*^ subst*^ annotated-expr*^)
		    (join-wraps mark* subst* annotated-expr* expr)
		  (%match-each (<stx>-expr expr) pattern mark*^ subst*^ annotated-expr*^))))
	  ((annotation? expr)
	   (%match-each (annotation-expression expr) pattern mark* subst* annotated-expr*))
	  (else #f)))

  (define (%match-each+ e x-pat y-pat z-pat mark* subst* annotated-expr* pvar*)
    (let loop ((e e) (mark* mark*) (subst* subst*) (annotated-expr* annotated-expr*))
      (cond ((pair? e)
	     (receive (xr* y-pat pvar*)
		 (loop (cdr e) mark* subst* annotated-expr*)
	       (if pvar*
		   (if (null? y-pat)
		       (let ((xr (%match (car e) x-pat mark* subst* annotated-expr* '())))
			 (if xr
			     (values (cons xr xr*) y-pat pvar*)
			   (values #f #f #f)))
		     (values '()
			     (cdr y-pat)
			     (%match (car e) (car y-pat) mark* subst* annotated-expr* pvar*)))
		 (values #f #f #f))))
	    ((<stx>? e)
	     (if (top-marked? mark*)
		 (values '() y-pat (%match e z-pat mark* subst* annotated-expr* pvar*))
	       (receive (mark* subst* annotated-expr*)
		   (join-wraps mark* subst* annotated-expr* e)
		 (loop (<stx>-expr e) mark* subst* annotated-expr*))))
	    ((annotation? e)
	     (loop (annotation-expression e) mark* subst* annotated-expr*))
	    (else
	     (values '() y-pat (%match e z-pat mark* subst* annotated-expr* pvar*))))))

  (define (%match-each-any e mark* subst* annotated-expr*)
    (cond ((pair? e)
	   (let ((l (%match-each-any (cdr e) mark* subst* annotated-expr*)))
	     (and l (cons (%make-syntax-object (car e) mark* subst* annotated-expr*) l))))
	  ((null? e)
	   '())
	  ((<stx>? e)
	   (and (not (top-marked? mark*))
		(receive (mark* subst* annotated-expr*)
		    (join-wraps mark* subst* annotated-expr* e)
		  (%match-each-any (<stx>-expr e) mark* subst* annotated-expr*))))
	  ((annotation? e)
	   (%match-each-any (annotation-expression e) mark* subst* annotated-expr*))
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

  (define (%make-syntax-object stx mark* subst* annotated-expr*)
    (if (and (null? mark*)
	     (null? subst*)
	     (null? annotated-expr*))
	stx
      (mkstx stx mark* subst* annotated-expr*)))

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

(module (expr-syntax-type)

  (define (expr-syntax-type expr-stx lexenv)
    ;;Determine the  syntax type of  an expression.  EXPR-STX must  be a
    ;;syntax object representing an expression.  Return 2 values:
    ;;
    ;;1..A symbol representing the syntax type.
    ;;
    ;;2..If  the  syntax  is  a  macro application:  the  value  of  the
    ;;    syntactic   binding  associated   to  the   macro  identifier.
    ;;   Otherwise false.
    ;;
    ;;3..If  the   syntax  is   a  macro  application:   the  identifier
    ;;   representing the macro keyword.  Otherwise false.
    ;;
    ;;The type of an expression is determined by two things:
    ;;
    ;;* The shape of the expression (identifier, pair, or datum).
    ;;
    ;;* The binding of the identifier (for id-stx) or the type of car of
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
	     (if (identifier? id)
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
			(values 'call #f #f)))))
	       ;;Here we know that EXPR-STX has the format:
	       ;;
	       ;;   (?non-id ?form ...)
	       ;;
	       ;;where ?NON-ID  can be  anything but not  an identifier.
	       ;;In practice the only valid syntax for this case is:
	       ;;
	       ;;   ((?first-subform ?subform ...) ?form ...)
	       ;;
	       ;;because ?NON-ID  must be an expression  evaluating to a
	       ;;closure object.
	       ;;
	       (values 'call #f #f))))

	  (else
	   (let ((datum (syntax->datum expr-stx)))
	     (if (self-evaluating? datum)
		 (values 'constant datum #f)
	       (values 'other #f #f))))))

  (define (self-evaluating? x)
    (or (number?		x)
	(string?		x)
	(char?			x)
	(boolean?		x)
	(bytevector?		x)
	(keyword?		x)
	(would-block-object?	x)))

  #| end of module: EXPR-SYNTAX-TYPE |# )


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

  (define* (chi-non-core-macro (procname symbol?) input-form-expr lexenv.run rib)
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

  (define (chi-local-macro bind-val input-form-expr lexenv.run rib)
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

  (define (chi-global-macro bind-val input-form-expr lexenv.run rib)
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

  (define (%do-macro-call transformer input-form-expr lexenv.run rib)
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
				  (%eval-macro-transformer
				   (%expand-macro-transformer (if (eq? type 'let-syntax)
								  x
								(push-lexical-contour xrib x))
							      lexenv.expand)
				   lexenv.run))
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
   (syntax-match formals-stx ()
     ((?arg* ...)
      (begin
	(%verify-formals-syntax formals-stx input-form-stx)
	(let ((lex* (map gensym-for-lexical-var ?arg*))
	      (lab* (map gensym-for-label       ?arg*)))
	  (values lex*
		  (chi-internal-body (push-lexical-contour
					 (make-filled-rib ?arg* lab*)
				       body-form-stx*)
				     (add-lexical-bindings lab* lex* lexenv.run)
				     lexenv.expand)))))
     ((?arg* ... . ?rest-arg)
      (begin
	(%verify-formals-syntax formals-stx input-form-stx)
	(let ((lex* (map gensym-for-lexical-var ?arg*))
	      (lab* (map gensym-for-label       ?arg*))
	      (lex  (gensym-for-lexical-var ?rest-arg))
	      (lab  (gensym-for-label       ?rest-arg)))
	  (values (append lex* lex) ;yes, this builds an improper list
		  (chi-internal-body (push-lexical-contour
					 (make-filled-rib (cons ?rest-arg ?arg*)
							(cons lab       lab*))
				       body-form-stx*)
				     (add-lexical-bindings (cons lab lab*)
							   (cons lex lex*)
							   lexenv.run)
				     lexenv.expand)))))
     (_
      (stx-error formals-stx "invalid syntax")))))

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
  (syntax-match input-form-stx ()
    ((_ (?ctxt . ?fmls) . ?body-form*)
     (receive (fmls body)
	 (chi-lambda-clause input-form-stx ?fmls ?body-form* lexenv.run lexenv.expand)
       (build-lambda (syntax-annotation ?ctxt) fmls body)))))


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
		;;  in the  expanded code.
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
		(receive (id qrhs)
		    (%parse-define body-form-stx)
		  (when (bound-id-member? id kwd*)
		    (stx-error body-form-stx "cannot redefine keyword"))
		  (receive (lab lex)
		      (gen-define-label+lex id rib sd?)
		    (extend-rib! rib id lab sd?)
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
		  (let* ((lab          (gen-define-syntax-label id rib sd?))
			 (expanded-rhs (%expand-macro-transformer rhs-stx lexenv.expand)))
		    ;;First map  the identifier  to the  label, creating
		    ;;the binding; then evaluate the macro transformer.
		    (extend-rib! rib id lab sd?)
		    (let ((entry (cons lab (%eval-macro-transformer expanded-rhs lexenv.run))))
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
			 (expanded-rhs (%expand-macro-transformer rhs-stx lexenv.expand)))
		    ;;First map the identifier to  the label, so that it
		    ;;is bound; then evaluate the macro transformer.
		    (extend-rib! rib id lab sd?)
		    (let* ((binding  (%eval-macro-transformer expanded-rhs lexenv.run))
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
			 (binding     (%eval-macro-transformer
				       (%expand-macro-transformer rhs-stx lexenv.expand)
				       lexenv.run))
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
					 (%eval-macro-transformer
					  (%expand-macro-transformer
					   (if (eq? type 'let-syntax)
					       x
					     (push-lexical-contour xrib x))
					   lexenv.expand)
					  lexenv.run))
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

  (define (%parse-define x)
    ;;Syntax parser for R6RS's DEFINE.
    ;;
    (syntax-match x ()
      ((_ (?id . ?fmls) ?b ?b* ...)
       (identifier? ?id)
       (begin
	 (%verify-formals-syntax ?fmls x)
	 (values ?id (cons 'defun x))))

      ((_ ?id ?val)
       (identifier? ?id)
       (values ?id (cons 'expr ?val)))

      ((_ ?id)
       (identifier? ?id)
       (values ?id (cons 'expr (bless '(void)))))
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
				 (datum->stx ?ctxt name))
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
				  (car (<stx>-mark* name))
				  (vector-map
				      (lambda (x)
					;;This   is   a  syntax   object
					;;holding an identifier.
					(make-<stx> (<stx>-expr x) ;expression
						    (<stx>-mark* x) ;list of marks
						    '() ;list of substs
						    '())) ;annotated expressions
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
		(make-<stx> (<stx>-expr x)		  ;expression
			    (append diff (<stx>-mark* x)) ;list of marks
			    '()	  ;list of substs
			    '())) ;annotated expressions
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
