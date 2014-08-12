;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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
;;;


;;;; introduction
;;
;;For an  introduction to processing LETREC  and LETREC* syntaxes, and  to understand
;;the code below, we *must* read the following paper:
;;
;;   [WSD]  Oscar Waddell,  Dipanwita  Sarkar,  R. Kent  Dybvig.   "Fixing Letrec:  A
;;   Faithful Yet Efficient Implementation of Scheme's Recursive Binding Construct"
;;
;;then we  can move to  the following paper,  which describes the  SCC transformation
;;used by Vicare:
;;
;;   [GD]  Abdulaziz  Ghuloum,  R.    Kent  Dybvig.   ``Fixing  Letrec  (reloaded)''.
;;   Workshop on Scheme and Functional Programming '09
;;
;;and finally the documentation of the pass functions in Texinfo format.
;;
;;
;;Input and output of all the LETREC optimisation alternatives
;;============================================================
;;
;;The input of  all the alternative pass functions is  a struct instance representing
;;an expression  as recordized  code; the  input is a  tree-like nested  hierarchy of
;;structures with the following struct types:
;;
;;	assign		bind		clambda
;;	conditional	constant	forcall
;;	funcall		mvcall		prelex
;;	primref		rec*bind	recbind
;;	seq
;;
;;in such  hierarchy: instances of the  struct type BIND represent  LET core language
;;forms; instances  of the  struct types  RECBIND and  REC*BIND represent  LETREC and
;;LETREC* core  language forms;  instances of  the struct  type PRELEX,  in reference
;;position, represent references to bindings defined by BIND, RECBIND or REC*BIND.
;;
;;The  output  of  all the  alternative  pass  functions  is  a new  struct  instance
;;representing  an  expression  as  recordized  code; the  hierarchy  of  the  output
;;expression is  the same as  that of the input  expression, except for  instances of
;;RECBIND and REC*BIND which are replaced by a composition of BIND, FIX and ASSIGN.
;;
;;
;;Notes for all the LETREC optimisation alternatives
;;==================================================
;;
;;NOTE We assume that the input expression  is correct.  All the PRELEX structures in
;;reference position are captured by a  binding defined by BIND, RECBIND or REC*BIND;
;;there are no references to free variables.
;;
;;NOTE We need to  remember that the LETREC-optimisation pass is  used to process the
;;result of fully expanding: libraries,  programs and standalone expressions given to
;;EVAL (either in stateless environments or stateful interactive environments).  This
;;means some  bindings are defined  in the input  expression, while others  have been
;;defined in  a previously processed  expression; assignments and references  to such
;;previously defined bindings have been  already processed and transformed into calls
;;to  the  primitive  functions:  they  are not  represented  by  ASSIGN  and  PRELEX
;;structures.
;;
;;NOTE Upon entering  this compiler pass, the PRELEX  structures representing defined
;;bindings already  have the  fields SOURCE-REFERENCED?,  SOURCE-ASSIGNED?  correctly
;;set; a previous pass has determined if a binding is assigned or not.
;;


(module (optimize-letrec
	 current-letrec-pass
	 check-for-illegal-letrec)

  (define check-for-illegal-letrec
    (make-parameter #t
      (lambda (obj)
	(and obj #t))))

  (define current-letrec-pass
    (make-parameter 'scc
      (lambda (obj)
	(if (memq obj '(scc waddell basic))
	    obj
	  (procedure-argument-violation 'current-letrec-pass
	    "invalid letrec optimization mode, expected a symbol among: scc, waddell, basic"
	    obj)))))

  (define* (optimize-letrec x)
    (when (check-for-illegal-letrec)
      (check-for-illegal-letrec-references x))
    (case (current-letrec-pass)
      ((scc)     (optimize-letrec/scc     x))
      ((waddell) (optimize-letrec/waddell x))
      ((basic)   (optimize-letrec/basic   x))
      (else
       (assertion-violation __who__
	 "invalid letrec optimization mode" (current-letrec-pass)))))


;;;; helpers

(define* (make-prelex-for-tmp-binding {prel prelex?})
  ;;Build and return a unique PRELEX struct meant to be used for a compiler-generated
  ;;binding, which will be referenced but not assigned.
  ;;
  ;;Since we  know the binding will  be referenced (otherwise the  compiler would not
  ;;generate it): we mark the PRELEX as source referenced.
  ;;
  ;;The  init value  of the  binding will  be,  in some  way, related  to the  PRELEX
  ;;argument PREL; so we reuse the name of PREL as name of the returned PRELEX.
  ;;
  (receive-and-return (tmp)
      (make-prelex (prelex-name prel))
    (set-prelex-source-referenced?! tmp #t)))

(module (%make-void-constants)
  ;;Build and  return a  list of  CONSTANT structs  representing #<void>  values, one
  ;;struct  for each  item in  LHS*.  They  are used,  for example,  to generate  the
  ;;undefined RHS expressions in the transformation from:
  ;;
  ;;   (letrec* ((?var ?init) ...)
  ;;     ?body0 ?body ...)
  ;;
  ;;to:
  ;;
  ;;   (let ((?var (void)) ...)
  ;;     (set! ?var ?init) ...
  ;;     ?body0 ?body ...)
  ;;
  (define (%make-void-constants lhs*)
    (map (lambda (x) THE-VOID) lhs*))

  (define-constant THE-VOID
    (make-constant (void)))

  #| end of module |# )

(module (build-assign*)

  (define (build-assign* lhs* rhs* body)
    ;;Build a sequence of assignments followed by a body.
    ;;
    ;;LHS*  must  be   a  list  of  struct  instances   of  type  PRELEX
    ;;representing left-hand sides in LET-like bindings.
    ;;
    ;;RHS* must  be a list  of struct instances  representing right-hand
    ;;sides in LET-like bindings, as recordized code.
    ;;
    ;;BODY must be a struct instance representing the body of a LET-like
    ;;body, as recordized code.
    ;;
    ;;Return a new struct instance representing the sequence:
    ;;
    ;;  (begin (set! ?lhs ?rhs) ... . ?body)
    ;;
    (for-each mark-assigned! lhs*)
    (let recur ((lhs* lhs*)
		(rhs* rhs*))
      (if (null? lhs*)
	  body
	(make-seq (make-assign ($car lhs*) ($car rhs*))
		  (recur ($cdr lhs*) ($cdr rhs*))))))

  (define (mark-assigned! lhs)
    ;;FIXME This is very fragile.  (Abdulaziz Ghuloum)
    (unless ($prelex-source-assigned? lhs)
      ($set-prelex-source-assigned?! lhs (or ($prelex-global-location lhs) #t))))

  #| end of module: build-assign* |# )


(module (check-for-illegal-letrec-references)
  ;;This module is  used to check for illegal references  to bindings in
  ;;the right-hand sides of LETREC and LETREC* syntaxes.
  ;;
  (define who 'check-for-illegal-letrec-references)

  (define (check-for-illegal-letrec-references x)
    (cond ((C x (%make-empty-illegal-set))
	   => (lambda (illegal)
		(%error illegal x)))))

  ;;In this  commented out  version we  use a  list to  hold the  set of
  ;;PRELEX structures that  is illegal to reference  in right-hand sides
  ;;of LETREC, LETREC* and LIBRARY-LETREC* syntaxes.
  ;;
  ;;Doing  a  linear search  is  usually  fine  for LETREC  and  LETREC*
  ;;syntaxes, because  the list of  bindings is most likely  small.  But
  ;;LIBRARY-LETREC*  syntaxes will  have "many"  bindings, one  for each
  ;;defined function.
  ;;
  ;; (begin
  ;;   (define-inline (%make-empty-illegal-set)
  ;;     '())
  ;;   (define (%illegal-reference-to? x illegals)
  ;;     (cond ((memq x illegals)
  ;; 	     => car)
  ;; 	    (else #f)))
  ;;   (define-inline (%illegal-augment more illegals)
  ;;     (append more illegals)))
  ;;
  ;;In this version we  use a closure on a hashtable to  hold the set of
  ;;PRELEX structures that  is illegal to reference  in right-hand sides
  ;;of LETREC, LETREC* and LIBRARY-LETREC* syntaxes.
  ;;
  (begin
    (define-inline (%make-empty-illegal-set)
      (lambda (x) #f))
    (define-inline (%illegal-reference-to? x illegals)
      ;;Must return #f if X is legal, and X itself if X is illegal.
      ;;
      (illegals x))
    (define (%illegal-augment more illegals)
      ;;MORE must be a list of  PRELEX structures to add to the illegals
      ;;set.
      ;;
      (if (null? more)
	  illegals
	(let ((H (make-eq-hashtable)))
	  (for-each (lambda (x)
		      ;;Yes, we want X as both key and value.
		      (hashtable-set! H x x))
	    more)
	  (lambda (x)
	    (or (hashtable-ref H x #f)
		(%illegal-reference-to? x illegals)))))))

  (define (C x illegals)
    ;;Recursively  visit the  recordized  code X  looking  for a  struct
    ;;instance of type  PRELEX which is EQ? to one  in the set ILLEGALS.
    ;;When found return such struct, else return #f.
    ;;
    (struct-case x
      ((constant)
       #f)

      ((prelex)
       (%illegal-reference-to? x illegals))

      ((assign lhs rhs)
       (or (%illegal-reference-to? lhs illegals)
	   (C rhs illegals)))

      ((primref)
       #f)

      ((bind lhs* rhs* body)
       (or (if (null? lhs*)
	       #f
	     (C*/error rhs* illegals))
	   (C body illegals)))

      ((recbind lhs* rhs* body)
       (or (if (null? lhs*)
	       #f
	     (C*/error rhs* (%illegal-augment lhs* illegals)))
	   (C body illegals)))

      ((rec*bind lhs* rhs* body)
       (or (if (null? lhs*)
	       #f
	     ;;Notice the difference between  LETREC and LETREC*: in the
	     ;;latter it  is fine for  a RHS to  reference the LHS  of a
	     ;;previous local binding.
	     (let loop ((lhs* lhs*)
			(rhs* rhs*))
	       (if (null? rhs*)
		   #f
		 (or (C/error ($car rhs*) (%illegal-augment lhs* illegals))
		     (loop ($cdr lhs*) ($cdr rhs*))))))
	   (C body illegals)))

      ((conditional test conseq altern)
       (or (C test   illegals)
	   (C conseq illegals)
	   (C altern illegals)))

      ((seq e0 e1)
       (or (C e0 illegals)
	   (C e1 illegals)))

      ((clambda)
       (C-clambda x))

      ((funcall rator rand*)
       (or (C  rator illegals)
	   (C* rand* illegals)))

      ((mvcall p c)
       (or (C p illegals)
	   (C c illegals)))

      ((forcall rator rand*)
       ;;Remember that RATOR is a string here.
       (C* rand* illegals))

      (else
       (error who "invalid expression" (unparse-recordized-code x)))))

  (define (C/error x illegals)
    ;;Like C, but  in case of error  make use of X as  enclosing form in
    ;;the raised exception.
    ;;
    (cond ((C x illegals)
	   => (lambda (illegal)
		(%error illegal x)))
	  (else #f)))

  (define (C* x* illegals)
    ;;Apply C to every item in the list X*.
    ;;
    (find (lambda (x)
	    (C x illegals))
      x*))

  (define (C*/error x* illegals)
    ;;Like C*, but in  case of error make use of the  culprit item of X*
    ;;as enclosing form in the raised exception.
    ;;
    (let loop ((x* x*))
      (cond ((null? x*)
	     #f)
	    ((C ($car x*) illegals)
	     => (lambda (illegal)
		  (%error illegal ($car x*))))
	    (else
	     (loop ($cdr x*))))))

;;; --------------------------------------------------------------------

  (module (C-clambda)
    ;;The purpose of this module is to apply C to every CASE-LAMBDA body
    ;;with an empty set of illegals.
    ;;
    (define (C-clambda x)
      (struct-case x
	((clambda label.unused cls*)
	 (for-each C-clambda-case cls*)
	 #f)))

    (define (C-clambda-case x)
      (struct-case x
	((clambda-case info body)
	 (C/error body (%make-empty-illegal-set)))))

    #| end of module: C-lambda |# )

;;; --------------------------------------------------------------------

  (define (%error illegal-prelex enclosing-code)
    ;;R6RS  requests  that  this  error is  of  type  "&assertion",  but
    ;;"&syntax" is not bad either.
    ;;
    (syntax-violation who
      "illegal binding reference in right-hand side of LETREC, LETREC* or LIBRARY syntax"
      (unparse-recordized-code/pretty enclosing-code)
      (unparse-recordized-code/pretty illegal-prelex)))

  #| end of module: check-for-illegal-letrec-references |# )


(module (optimize-letrec/basic)
  ;;Perform basic transformations to convert  the recordized representation of LETREC
  ;;and LETREC* forms into LET-like forms and assignments.
  ;;
  ;;The transformations performed by this module are equivalent to the following:
  ;;
  ;;   (letrec ((?var ?init) ...) . ?body)
  ;;   ==> (let ((?var (void)) ...)
  ;;         (let ((?tmp ?init) ...)
  ;;           (set! ?var ?tmp) ...
  ;;           . ?body))
  ;;
  ;;   (letrec* ((?var ?init) ...) . ?body)
  ;;   ==> (let ((?var (void)) ...)
  ;;         (set! ?var ?init) ...
  ;;         . ?body)
  ;;
  ;;   (library-letrec* ((?var ?loc ?init) ...) . ?body)
  ;;   ==> (let ((?var (void)) ...)
  ;;         (set! ?var ?init) ...
  ;;         . ?body)
  ;;
  ;;Notice that the transformation for LETREC is described also in the R5RS document.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'optimize-letrec/basic))

  ;;Make the code more readable.
  (define-syntax-rule (optimize-letrec/basic x)
    (E x))

  (define (E x)
    (struct-case x
      ((constant)
       x)

      ((prelex)
       (assert (prelex-source-referenced? x))
       x)

      ((assign lhs rhs)
       (assert (prelex-source-assigned? lhs))
       (make-assign lhs (E rhs)))

      ((primref)
       x)

      ((bind lhs* rhs* body)
       (if (null? lhs*)
	   (E body)
	 (make-bind lhs* (map E rhs*) (E body))))

      ((recbind lhs* rhs* body)
       (if (null? lhs*)
	   (E body)
	 (%do-recbind lhs* (map E rhs*) (E body))))

      ((rec*bind lhs* rhs* body)
       (if (null? lhs*)
	   (E body)
	 (%do-rec*bind lhs* (map E rhs*) (E body))))

      ((conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern)))

      ((seq e0 e1)
       (make-seq (E e0) (E e1)))

      ((clambda)
       (E-clambda x))

      ((funcall rator rand*)
       (make-funcall (E rator) (map E rand*)))

      ((mvcall p c)
       (make-mvcall (E p) (E c)))

      ((forcall rator rand*)
       (make-forcall rator (map E rand*)))

      (else
       (error __who__ "invalid expression" (unparse-recordized-code x)))))

  (define (E-clambda x)
    (struct-case x
      ((clambda label cls* cp free name)
       (make-clambda label (map E-clambda-case cls*) cp free name))))

  (define (E-clambda-case x)
    (struct-case x
      ((clambda-case info body)
       (make-clambda-case info (E body)))))

;;; --------------------------------------------------------------------

  (define (%do-rec*bind lhs* rhs* body)
    ;;A struct instance of type REC*BIND represents a form like:
    ;;
    ;;   (letrec* ((?var ?init) ...) ?body0 ?body ...)
    ;;
    ;;the transformation we do here is equivalent to constructing the following form:
    ;;
    ;;   (let ((?var (void)) ...)
    ;;     (set! ?var ?init) ...
    ;;     ?body0 ?body ...)
    ;;
    (make-bind lhs* (%make-void-constants lhs*)
      (build-assign* lhs* rhs* body)))

  (define (%do-recbind lhs* rhs* body)
    ;;A struct instance of type REC*BIND represents a form like:
    ;;
    ;;   (letrec ((?var ?init) ...) ?body0 ?body ...)
    ;;
    ;;the transformation we do here is equivalent to constructing the following form:
    ;;
    ;;   (let ((?var (void)) ...)
    ;;     (let ((?tmp ?init) ...)
    ;;       (set! ?var ?tmp) ...
    ;;       ?body0 ?body ...))
    ;;
    (let ((tmp* (map make-prelex-for-tmp-binding lhs*)))
      (make-bind lhs* (%make-void-constants lhs*)
	(make-bind tmp* rhs* (build-assign* lhs* tmp* body)))))

  #| end of module: optimize-letrec/basic |# )


(module (optimize-letrec/waddell)
  ;;Perform transformations  to convert the  recordized representation of  LETREC and
  ;;LETREC*  forms into  LET-like forms  and assignments.   This function  performs a
  ;;transformation similar (but not equal to) the one described in the [WSD] paper.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'optimize-letrec/waddell))

  (define (optimize-letrec/waddell x)
    (parametrise ((lhs-used-func  (%make-top-lhs-used-registrar-func))
		  (rhs-cplx-func  (%make-top-rhs-cplx-registrar-func)))
      (E x)))

  (module (E)

    (define (E x)
      ;;Recursively visit the recordized code X.
      ;;
      (struct-case x
	((constant)
	 x)

	((prelex)
	 ;;A reference to a lexical variable.
	 (register-lhs-usage! x)
	 x)

	((assign lhs rhs)
	 ;;X is  a binding assignment.  An  assignment is a reference  to the binding
	 ;;LHS and also it makes X a "complex" expression.
	 (register-lhs-usage! lhs)
	 (make-the-enclosing-rhs-complex!)
	 (make-assign lhs (E rhs)))

	((primref)
	 x)

	((bind lhs* rhs* body)
	 ;;X is a binding creation form like LET.  Do RHS* first, then BODY.
	 (if (null? lhs*)
	     (E body)
	   (let* ((rhs*^ (E* rhs*))
		  (body^ (parametrise ((lhs-used-func (%make-nonrec-lhs-used-registrar-func (lhs-used-func) lhs*)))
			   (E body))))
	     (make-bind lhs* rhs*^ body^))))

	((recbind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body)
	   (%do-recbind lhs* rhs* body)))

	((rec*bind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body)
	   (%do-rec*bind lhs* rhs* body)))

	((conditional test conseq altern)
	 (make-conditional (E test)
	     (E conseq)
	   (E altern)))

	((seq e0 e1)
	 (make-seq (E e0) (E e1)))

	((clambda)
	 (E-clambda x))

	((funcall)
	 (E-funcall x))

	((mvcall)
	 (E-mvcall x))

	((forcall rator rand*)
	 ;;This is a foreign function call.
	 (make-forcall rator (E* rand*)))

	(else
	 (error __who__ "invalid expression" (unparse-recordized-code x)))))

    (define (E* x*)
      (if (null? x*)
	  '()
	(cons (E  ($car x*))
	      (E* ($cdr x*)))))

    (module (E-clambda)
      ;;Process  a CLAMBDA  structure.  In  general  we just  process the  body of  a
      ;;CLAMBDA like all the  other forms, but we have to take  care of the following
      ;;cases.
      ;;
      ;;CLAMBDA arguments
      ;;-----------------
      ;;
      ;;The arguments of a CLAMBDA structure  are simple bindings, not different from
      ;;the ones defined by  a BIND structure.  So, upon entering  a CLAMBDA body, we
      ;;create  a  new  LHS-usage  registrar  function  that  avoids  inspecting  the
      ;;references to bindings defined by the arguments.
      ;;
      ;;Complexity of the enclosing expression
      ;;--------------------------------------
      ;;
      ;;We decide that nothing  that happens in the body of the  CLAMBDA can make the
      ;;enclosing RHS  expression complex; this  allows lambda RHS expressions  to be
      ;;classified as "fixable", even if they  reference or assign a binding in their
      ;;lexical contour.
      ;;
      ;;Noticing that "complex"  RHS expressions are evaluated  *after* "fixable" RHS
      ;;expressions, we can understand the following examples.
      ;;
      ;;* Here the  binding A is assigned, so  it is "complex"; but the  binding B is
      ;;  "fixable", it does not become itself complex.
      ;;
      ;;    (letrec ((a 1)
      ;;             (b (lambda () (set! a 1))))
      ;;      (b))
      ;;    ==> (bind ()
      ;;          (bind ((a_0 !#void))
      ;;            (fix ((b (lambda () (assign a_0 '1))))
      ;;              (bind ((a_1 '1))
      ;;                (seq
      ;;                  (assign a_0 a_1)
      ;;                  (funcall b))))))
      ;;
      (define (E-clambda x)
	(struct-case x
	  ((clambda label clause* cp free name)
	   (make-clambda label (map E-clambda-case clause*) cp free name))))

      (define (E-clambda-case clause)
	(struct-case clause
	  ((clambda-case info body)
	   (make-clambda-case info
			      (parametrise
				  ((lhs-used-func (%make-nonrec-lhs-used-registrar-func (lhs-used-func) (case-info-args info)))
				   (rhs-cplx-func (%make-top-rhs-cplx-registrar-func)))
				(E body))))))

      #| end of module: E-clambda |# )

    (module (E-funcall)

      (define (E-funcall x)
	(struct-case x
	  ((funcall rator rand*)
	   (let ((rator^ (E  rator))
		 (rand*^ (E* rand*)))
	     ;;This form  is a function  call.  In general:
	     ;;
	     ;;* We  must assume  it might  reference or assign  any of  the bindings
	     ;;whose region includes it.
	     ;;
	     ;;* We  must assume that it  causes side effects whose  evaluation order
	     ;;must  not  be changed.   To  avoid  changing  the order  (for  LETREC*
	     ;;bindings): we make all such RHS expressions complex.
	     ;;
	     ;;so we should call MAKE-THE-ENCLOSING-RHS-COMPLEX!.  For example:
	     ;;
	     ;;   (letrec* ((a (lambda () c))
	     ;;             (b (a))
	     ;;             (c 1))
	     ;;     #f)
	     ;;
	     ;;the  call to  A in  the RHS  of B  must cause  B to  be classified  as
	     ;;"complex".  Notice that with LETREC these bindings are illegal.
	     ;;
	     ;;As  special case:  if we  recognise  the function  call as  a call  to
	     ;;primitive function  with *no*  side effects, we  can avoid  making the
	     ;;enclosing expression complex.
	     (struct-case rator^
	       ((primref primitive-function-public-name)
		(unless (memq primitive-function-public-name SIMPLE-PRIMITIVES)
		  (make-the-enclosing-rhs-complex!)))
	       (else
		(make-the-enclosing-rhs-complex!)))
	     (make-funcall rator^ rand*^)))))

      (define-constant SIMPLE-PRIMITIVES
	;;NOTE There are  many, many simple primitives.  Maybe, one  day, I will list
	;;them here.  (Marco Maggi; Mon Aug 11, 2014)
	;;
	'(fx+ fx- fx* fxdiv))

      #| end of module: E-funcall |# )

    (define (E-mvcall x)
      (struct-case x
	((mvcall producer consumer)
	 ;;This form is a function call.
	 (make-the-enclosing-rhs-complex!)
	 (make-mvcall (E producer) (E consumer)))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (module LHS-USAGE-FLAGS
    (%make-lhs-usage-flags
     %lhs-usage-flags-set!
     %lhs-usage-flags-ref)
    ;;This module handles values called USED-LHS-FLAGS  in the code.  Such values are
    ;;associative containers representing  a property of the LHS  of bindings defined
    ;;by  RECBIND  (LETREC) and  REC*BIND  (LETREC*).   Each container  maps  binding
    ;;indexes to flags representing left-hand side usage: the flag is true if the LHS
    ;;has  been referenced  or assigned  at least  once in  the right-hand  side init
    ;;expressions of the same lexical contour.
    ;;
    (import (only (vicare system $vectors)
		  $vector-set!
		  $vector-ref))

    (define (%make-lhs-usage-flags lhs*)
      (make-vector (length lhs*) #f))

    (define-syntax-rule (%lhs-usage-flags-set! ?flags ?lhs-index)
      ($vector-set! ?flags ?lhs-index #t))

    (define-syntax-rule (%lhs-usage-flags-ref ?flags ?lhs-index)
      ($vector-ref ?flags ?lhs-index))

    #| end of module: LHS-USAGE-FLAGS |# )

;;; --------------------------------------------------------------------

  (module RHS-COMPLEXITY-FLAGS
    (%make-rhs-complexity-flags
     %rhs-complexity-flags-set!
     %rhs-complexity-flags-ref)
    ;;This module handles values called CPLX-RHS-FLAGS  in the code.  Such values are
    ;;associative containers representing  a property of the RHS  of bindings defined
    ;;by  RECBIND  (LETREC) and  REC*BIND  (LETREC*).   Each container  maps  binding
    ;;indexes to flags  representing right-hand side init  expression complexity: the
    ;;flag is true if the RHS expression is  "complex", that is: it may assign one of
    ;;the LHS in the same lexical contour (we  cannot be sure if it actually does it,
    ;;nor of which bindings are mutated), or it performs a complex function call.
    ;;
    (import (only (vicare system $vectors)
		  $vector-set!
		  $vector-ref))

    (define (%make-rhs-complexity-flags rhs*)
      (make-vector (length rhs*) #f))

    (define-syntax-rule (%rhs-complexity-flags-set! ?flags ?rhs-index)
      ($vector-set! ?flags ?rhs-index #t))

    (define-syntax-rule (%rhs-complexity-flags-ref ?flags ?rhs-index)
      ($vector-ref ?flags ?rhs-index))

    #| end of module: RHS-COMPLEXITY-FLAGS |# )

;;; --------------------------------------------------------------------

  (define (%make-top-lhs-used-registrar-func)
    ;;Build  and  return a  top  variable-usage  registrar function.   Top  registrar
    ;;functions are generated only when starting to process a whole input expression,
    ;;not when entering a nested subexpression representing a binding form.
    ;;
    ;;Given the  way the  registrar functions  are implemented:  if this  function is
    ;;actually applied to a PRELEX structure,  it means such PRELEX represents a free
    ;;variable in the whole expression; this is an error.  If this error is found: it
    ;;means  that the  original  input  expression is  incorrect;  this should  never
    ;;happen.
    ;;
    (lambda (prel)
      (assertion-violation __who__ "found free variable reference" prel)))

  (module (%make-nonrec-lhs-used-registrar-func
	   %make-recbind-lhs-used-registrar-func)

    (define (%make-nonrec-lhs-used-registrar-func outer-lhs-usage-registrar! prel*)
      ;;Build and return a new nonrecursive-binding variable-usage registrar function
      ;;wrapping the  one given  as argument.  The  returned registrar  function will
      ;;avoid attempting to mark as "used" the PRELEX structures in the list PREL*.
      ;;
      ;;The  nonrecursive-binding  registrar  functions  are used  to  avoid  marking
      ;;binding  references  created  by non-RECBIND  (non-LETREC)  and  non-REC*BIND
      ;;(non-LETREC*) binding forms; so it must be used when entering: a BIND form; a
      ;;CLAMBDA subexpression body; a RECBIND or REC*BIND body.
      ;;
      ;;For efficiency reasons: a  nonrecursive-binding variable-usage function marks
      ;;a PRELEX  structure as  "used" only  once; we  do this  by avoiding  to apply
      ;;OUTER-LHS-USAGE-REGISTRAR!  to PREL when PREL is already in the TABLE.
      ;;
      ;;NOTE At the call site of the  registrar function: we do know for which PRELEX
      ;;struct we call it.
      ;;
      (define-constant TABLE (make-eq-hashtable))
      (for-each (lambda (prel)
		  (hashtable-set! TABLE prel #t))
	prel*)
      (lambda (prel)
	(with-unseen-prel (prel TABLE)
	  (outer-lhs-usage-registrar! prel))))

    (define (%make-recbind-lhs-used-registrar-func outer-lhs-usage-registrar! prel* used-lhs-flags)
      ;;Build and  return a  new recursive-binding variable-usage  registrar function
      ;;wrapping the one given as argument.
      ;;
      ;;For efficiency reasons: a  recursive-binding variable-usage registrar marks a
      ;;PRELEX  structure as  "used"  only once;  we  do this  by  avoiding to  apply
      ;;OUTER-LHS-USAGE-REGISTRAR!  to PREL when PREL is already in the TABLE.
      ;;
      ;;We need  a registrar  function for  each recursive-binding  lexical countour;
      ;;*not* one for each binding.
      ;;
      ;;NOTE At the call site of the  registrar function: we do know for which PRELEX
      ;;struct  we  call it;  for  this  reason we  can  compute  at the  moment  the
      ;;LHS-INDEX.
      ;;
      (define-constant TABLE (make-eq-hashtable))
      (lambda (prel)
	(with-unseen-prel (prel TABLE)
	  ;;EFFICIENCY NOTE  Searching the PRELEX struct  PREL in the list  of PRELEX
	  ;;structs  PREL* is  not  very  efficient.  The  list  can  be "long";  for
	  ;;example, PREL*  can be the  list of top  level function definitions  in a
	  ;;LIBRARY form.  Can we make it better with a cheap data structure?  (Marco
	  ;;Maggi; Tue Aug 12, 2014)
	  (cond ((%find-index prel prel*)
		 => (lambda (lhs-index)
		      (import LHS-USAGE-FLAGS)
		      (%lhs-usage-flags-set! used-lhs-flags lhs-index)
		      ;;If we are here the  PRELEX struct PREL represents a reference
		      ;;to LETREC or LETREC* binding.  If  we are processing a RHS of
		      ;;the same  lexical contour: we want  this RHS to be  marked as
		      ;;"complex".
		      ;;
		      ;;Example (invalid for LETREC*):
		      ;;
		      ;;  (letrec ((a 1)
		      ;;           (b a))
		      ;;    #f)
		      ;;
		      ;;must be transformed into:
		      ;;
		      ;;  (bind ((a_0 '1))
		      ;;    (bind ((b_0 a_0))
		      ;;      (fix ()
		      ;;        (bind ()
		      ;;          '#f))))
		      ;;
		      ;;in which  the binding for  B is "complex".
		      (make-the-enclosing-rhs-complex!)))
		(else
		 (outer-lhs-usage-registrar! prel))))))

    (define-syntax (with-unseen-prel stx)
      (syntax-case stx ()
	;;Evaluate ?BODY if ?PREL is *not* already in ?TABLE.
	((_ (?prel ?table) . ?body)
	 (and (identifier? #'?prel)
	      (identifier? #'?table))
	 #'(unless (hashtable-ref ?table ?prel #f)
	     (hashtable-set! ?table ?prel #t)
	     . ?body))))

    (case-define %find-index
      ;;Search ITEM  in the proper list  ELL; when found return  its index, otherwise
      ;;return false.
      ;;
      ((item ell)
       (%find-index item ell 0))
      ((item ell counter)
       (cond ((null? ell)
	      #f)
	     ((eq? item ($car ell))
	      counter)
	     (else
	      (%find-index item ($cdr ell) ($fxadd1 counter))))))

    #| end of module |# )

  (define-constant lhs-used-func
    (make-parameter (%make-top-lhs-used-registrar-func)))

  (define-syntax-rule (register-lhs-usage! ?prel)
    ((lhs-used-func) ?prel))

;;; --------------------------------------------------------------------

  (define-syntax-rule (%make-top-rhs-cplx-registrar-func)
    ;;Return a top thunk to be used as expression complexity registrar.  The returned
    ;;thunk is  to be used  as outer thunk when  entering whole input  expressions or
    ;;CLAMBDA bodies.
    ;;
    void)

  (define (%make-recbind-rhs-cplx-registrar-func outer-rhs-cplx-registrar-func cplx-rhs-flags rhs-index)
    ;;Build and  return a new  thunk to be  used as expression  complexity registrar,
    ;;wrapping the  one given as  argument.  Called  to register that  the right-hand
    ;;side init expression with index RHS-INDEX in the container CPLX-RHS-FLAGS is to
    ;;be classified "complex"; also make the outer expression complex.
    ;;
    ;;We really need one of these thunks for each RHS in a recbind lexical contour.
    ;;
    ;;NOTE At the call site of the registrar function: we do *not* know for which RHS
    ;;expression struct we call  it; for this reason we need  to generate a registrar
    ;;function closed upon RHS-INDEX.
    ;;
    (lambda ()
      (import RHS-COMPLEXITY-FLAGS)
      (%rhs-complexity-flags-set! cplx-rhs-flags rhs-index)
      (outer-rhs-cplx-registrar-func)))

  (define-constant rhs-cplx-func
    (make-parameter (%make-top-lhs-used-registrar-func)))

  (define-syntax-rule (make-the-enclosing-rhs-complex!)
    ((rhs-cplx-func)))

;;; --------------------------------------------------------------------

  (module (%do-recbind %do-rec*bind)

    (define-syntax-rule (%do-recbind ?lhs* ?rhs* ?body)
      (%true-do-recbind ?lhs* ?rhs* ?body #t))

    (define-syntax-rule (%do-rec*bind ?lhs* ?rhs* ?body)
      (%true-do-recbind ?lhs* ?rhs* ?body #f))

    (define (%true-do-recbind lhs* rhs* body letrec?)
      ;;If  the core  language form  we are  processing is  a RECBIND  representing a
      ;;LETREC: the  argument LETREC?   is true.   If the core  language form  we are
      ;;processing is  a REC*BIND  representing a LETREC*:  the argument  LETREC?  is
      ;;false.
      ;;
      (import LHS-USAGE-FLAGS RHS-COMPLEXITY-FLAGS)
      (let* ((used-lhs-flags (%make-lhs-usage-flags      lhs*))
	     (cplx-rhs-flags (%make-rhs-complexity-flags rhs*))
	     (rhs*^          (parametrise
				 ((lhs-used-func (%make-recbind-lhs-used-registrar-func (lhs-used-func) lhs* used-lhs-flags)))
			       (E-rhs* rhs* cplx-rhs-flags)))
	     (body^          (parametrise
				 ((lhs-used-func (%make-nonrec-lhs-used-registrar-func  (lhs-used-func) lhs*)))
			       (E body))))
	(receive (simple.lhs* simple.rhs* fixable.lhs* fixable.rhs* complex.lhs* complex.rhs*)
	    (%partition-rhs* lhs* rhs*^ used-lhs-flags cplx-rhs-flags)
	  (%make-bind simple.lhs* simple.rhs*
	    (%make-bind complex.lhs* (%make-void-constants complex.lhs*)
	      (%make-fix fixable.lhs* fixable.rhs*
			 (if letrec?
			     ;;It is  a RECBIND  and LETREC:  no order  enforced when
			     ;;evaluating COMPLEX.RHS*.
			     (let ((tmp* (map make-prelex-for-tmp-binding complex.lhs*)))
			       (%make-bind tmp* complex.rhs*
				 (build-assign* complex.lhs* tmp* body^)))
			   ;;It  is  a  REC*BIND  and LETREC*:  order  enforced  when
			   ;;evaluating COMPLEX.RHS*.
			   (build-assign* complex.lhs* complex.rhs* body^))))))))

    (case-define E-rhs*
      ((rhs* cplx-rhs-flags)
       (E-rhs* rhs* cplx-rhs-flags 0))
      ((rhs* cplx-rhs-flags binding-index)
       ;;Recursively process  RHS* and  return a  list of  struct instances  which is
       ;;meant  to replace  the  original RHS*.   This function  has  the purpose  of
       ;;applying E to  each struct in RHS*;  in so doing it  will fill appropriately
       ;;the containers USED-LHS-FLAGS and CPLX-RHS-FLAGS.
       ;;
       ;;Given recordized code representing:
       ;;
       ;;   (letrec ((?lhs-0 ?rhs-0)
       ;;            (?lhs-1 ?rhs-1)
       ;;            (?lhs-2 ?rhs-2))
       ;;     . ?body)
       ;;
       ;;this function is recursively called with:
       ;;
       ;;   (E-rhs* '(?rhs-0 ?rhs-1 ?rhs-2) --- 0)
       ;;   (E-rhs* '(?rhs-1 ?rhs-2)        --- 1)
       ;;   (E-rhs* '(?rhs-2)               --- 2)
       ;;   (E-rhs* '()                     --- 3)
       ;;
       ;;NOTE The  reason we do not  simply map the function  E over RHS* is  that we
       ;;need to count the bindings and increment the index BINDING-INDEX.
       ;;
       (if (null? rhs*)
	   '()
	 (cons (parametrise
		   ((rhs-cplx-func (%make-recbind-rhs-cplx-registrar-func (rhs-cplx-func) cplx-rhs-flags binding-index)))
		 (E ($car rhs*)))
	       (E-rhs* ($cdr rhs*) cplx-rhs-flags ($fxadd1 binding-index))))))

    (case-define %partition-rhs*
      ((lhs* rhs* used-lhs-flags cplx-rhs-flags)
       (%partition-rhs* lhs* rhs* used-lhs-flags cplx-rhs-flags 0))
      ((lhs* rhs* used-lhs-flags cplx-rhs-flags binding-index)
       ;;Non-tail  recursive  function.  Make  use  of  the  data in  the  containers
       ;;USED-LHS-FLAGS and  CPLX-RHS-FLAGS to  partition the bindings  into: simple,
       ;;complex, fixable.  (RHS* is not visited here.)
       ;;
       ;;Return 6 values:
       ;;
       ;;SIMPLE.LHS*, SIMPLE.RHS*
       ;;   Simple  bindings.  SIMPLE.LHS is never  assigned in all the  RHS* and the
       ;;    associated SIMPLE.RHS  is a  simple expression:  it never  references an
       ;;   SIMPLE.LHS* and it does not call any function.
       ;;
       ;;COMPLEX.LHS*, COMPLEX.RHS*
       ;;   Complex bindings.  Lists of LHS and RHS for which either we know that the
       ;;   LHS has been assigned, or we know that the RHS may have assigned an LHS.
       ;;
       ;;FIXABLE.LHS*, FIXABLE.RHS*
       ;;    Fixable  bindings.   Lists  of LHS  and  RHS  representing  non-assigned
       ;;   bindings whose RHS is a CLAMBDA.
       ;;
       (import LHS-USAGE-FLAGS RHS-COMPLEXITY-FLAGS)
       (if (null? lhs*)
	   (values '() '() '() '() '() '())
	 (receive (simple.lhs* simple.rhs* fixable.lhs* fixable.rhs* complex.lhs* complex.rhs*)
	     (%partition-rhs* ($cdr lhs*) ($cdr rhs*) used-lhs-flags cplx-rhs-flags ($fxadd1 binding-index))
	   (let ((lhs ($car lhs*))
		 (rhs ($car rhs*)))
	     (cond ((prelex-source-assigned? lhs)
		    ;;This binding is "complex".  It does  not matter if the RHS is a
		    ;;CLAMBDA  structure:   the  fact  that  it   is  assigned  takes
		    ;;precedence when deciding how to classify it.
		    (values simple.lhs* simple.rhs*
			    fixable.lhs* fixable.rhs*
			    (cons lhs complex.lhs*) (cons rhs complex.rhs*)))
		   ((clambda? rhs)
		    ;;This binding is "fixable".
		    (values simple.lhs* simple.rhs*
			    (cons lhs fixable.lhs*) (cons rhs fixable.rhs*)
			    complex.lhs* complex.rhs*))
		   ((or (%lhs-usage-flags-ref      used-lhs-flags binding-index)
			(%rhs-complexity-flags-ref cplx-rhs-flags binding-index))
		    ;;This binding is "complex".
		    (values simple.lhs* simple.rhs*
			    fixable.lhs* fixable.rhs*
			    (cons lhs complex.lhs*) (cons rhs complex.rhs*)))
		   (else
		    ;;This binding is "simple".
		    (values (cons lhs simple.lhs*) (cons rhs simple.rhs*)
			    fixable.lhs* fixable.rhs*
			    complex.lhs* complex.rhs*))
		   ))))))

    #| end of module: %DO-RECBIND %DO-REC*BIND |# )

;;; --------------------------------------------------------------------

  (define (%make-bind lhs* rhs* body)
    (if (null? lhs*)
	body
      (make-bind lhs* rhs* body)))

  (define (%make-fix lhs* rhs* body)
    (if (null? lhs*)
	body
      (make-fix lhs* rhs* body)))

  #| end of module: OPTIMIZE-LETREC/WADDELL |# )


(module (optimize-letrec/scc)
  ;;Perform transformations  to convert the  recordized representation of  LETREC and
  ;;LETREC*  forms into  LET  forms  and assignments.   This  function  does what  is
  ;;described in the [GD] paper.
  ;;
  ;;Let's see some examples:
  ;;
  ;;   (let ((a 1))
  ;;     (let ((a a))
  ;;       a))
  ;;   ==> (let* ((a_0 '1)
  ;;              (a_1 a_0))
  ;;         a_1)
  ;;
  ;; (let ((a 1))
  ;;   (let ((a 2))
  ;;     (let ((a 3))
  ;; 	a)))
  ;; ==> (let* ((a_0 '1)
  ;; 	     (a_1 '2)
  ;; 	     (a_2 '3))
  ;; 	a_2)
  ;;
  ;; (letrec ((a 1)
  ;; 	   (b 2))
  ;;   (list a b))
  ;; ==> (let* ((a_0 '1)
  ;; 	     (b_0 '2))
  ;; 	(list a_0 b_0))
  ;;
  ;; (letrec* ((a (lambda (x)
  ;; 		 (when x
  ;; 		   (a #f))))
  ;;           (b 123)
  ;;           (c 456)
  ;;           (d (begin
  ;; 		 (set! c 789)
  ;; 		 9)))
  ;;   a)
  ;; ==> (fix ((a_0 (lambda (x_0)
  ;; 		   (if x_0
  ;; 		       (a_0 '#f)
  ;; 		     (void)))))
  ;; 	 (let* ((b_0 '123)
  ;; 		(c_0 '456)
  ;; 		(d_0 (begin
  ;; 		       (set! c_0 '789)
  ;; 		       '9)))
  ;; 	   a_0))
  ;;
  ;; (letrec* ((a 123)
  ;; 	    (b 2)
  ;; 	    (c b)
  ;; 	    (d (lambda () 123)))
  ;;   b)
  ;; ==> (let* ((a_0 '123)
  ;; 	     (b_0 '2)
  ;; 	     (c_0 b_0))
  ;; 	(fix ((d_0 (lambda () '123)))
  ;; 	  b_0))
  ;;
  ;; (letrec* ((a 123)
  ;; 	    (b 2)
  ;; 	    (c b)
  ;; 	    (d (lambda () 123)))
  ;;   (set! d 123)
  ;;   b)
  ;; ==> (let* ((a_0 '123)
  ;; 	     (b_0 '2)
  ;; 	     (c_0 b_0)
  ;; 	     (d_0 (lambda () '123)))
  ;; 	(begin
  ;; 	  (set! d_0 '123)
  ;; 	  b_0))
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'optimize-letrec/scc))

  (define-struct binding
    (serial
     lhs
     rhs
     complex
     prev
		;False or a struct instance of type BINDING being the previous value.
     free*))

  (define (optimize-letrec/scc x)
    (receive-and-return (x)
	(E x (make-binding #f #f #f #t #t '()))
      ;;(debug-print (unparse-recordized-code x))
      (void)))

;;; --------------------------------------------------------------------

  (module (E)

    (define (E x bc)
      (struct-case x
	((constant)
	 x)

	((prelex)
	 (assert (prelex-source-referenced? x))
	 (mark-free x bc)
	 (when (prelex-source-assigned? x)
	   (mark-complex! bc))
	 x)

	((assign lhs rhs)
	 (assert (prelex-source-assigned? lhs))
	 ;;(set-prelex-source-assigned?! lhs #t)
	 (mark-free lhs bc)
	 (mark-complex! bc)
	 (make-assign lhs (E rhs bc)))

	((primref)
	 x)

	((bind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body bc)
	   (make-bind lhs* (E* rhs* bc) (E body bc))))

	((recbind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body bc)
	   (%do-recbind lhs* rhs* body bc #f)))

	((rec*bind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body bc)
	   (%do-recbind lhs* rhs* body bc #t)))

	((conditional test conseq altern)
	 (make-conditional (E test bc) (E conseq bc) (E altern bc)))

	((seq e0 e1)
	 (make-seq (E e0 bc) (E e1 bc)))

	((clambda)
	 (E-clambda x bc))

	((funcall rator rand*)
	 (mark-complex! bc)
	 (make-funcall (E rator bc) (E* rand* bc)))

	((mvcall producer consumer)
	 (mark-complex! bc)
	 (make-mvcall (E producer bc) (E consumer bc)))

	((forcall rator rand*)
	 (mark-complex! bc)
	 (make-forcall rator (E* rand* bc)))

	(else
	 (error __who__ "invalid expression" (unparse-recordized-code x)))))

    (define (E* x* bc)
      (map (lambda (x)
	     (E x bc))
	x*))

    (define (E-clambda x bc)
      ;;Apply E to each clause's body.
      ;;
      (struct-case x
	((clambda label cls* cp free name)
	 (let ((bc (make-binding #f #f #f #t bc '())))
	   (make-clambda label (map (lambda (x)
				      (struct-case x
					((clambda-case info body)
					 (make-clambda-case info (E body bc)))))
				 cls*)
			 cp free name)))))

    (define (mark-complex! bc)
      ;;BC must be  a struct instance of type BINDING.   Mark as complex
      ;;BC and, recursively, its previous value in the field PREV.
      ;;
      (unless ($binding-complex bc)
	($set-binding-complex! bc #t)
	(mark-complex! ($binding-prev bc))))

    (define (mark-free var bc)
      ;;VAR must  be a  struct instance  of type PRELEX.   BC must  be a
      ;;struct instance of type BINDING.
      ;;
      (let ((rb (prelex-operand var)))
	(when rb
	  (let* ((lb    (let ((pr ($binding-prev rb)))
			  (let loop ((bc bc))
			    (let ((bcp ($binding-prev bc)))
			      (if (eq? bcp pr)
				  bc
				(loop bcp))))))
		 (free* ($binding-free* lb)))
	    (unless (memq rb free*)
	      ($set-binding-free*! lb (cons rb free*)))))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (module (%do-recbind)

    (define (%do-recbind lhs* rhs* body bc ordered?)
      (let ((b* (%make-bindings lhs* rhs* bc 0)))
	(for-each (lambda (b)
		    ($set-binding-rhs! b (E ($binding-rhs b) b)))
	  b*)
	(for-each (lambda (x)
		    (set-prelex-operand! x #f))
	  lhs*)
	(let ((body (E body bc)))
	  (when ordered?
	    (insert-order-edges b*))
	  (let ((scc* (get-sccs-in-order b* (map binding-free* b*) b*)))
	    (gen-letrecs scc* ordered? body)))))

    (define (%make-bindings lhs* rhs* bc i)
      (if (null? lhs*)
	  '()
	(let ((b (make-binding i ($car lhs*) ($car rhs*) #f bc '())))
	  (set-prelex-operand! ($car lhs*) b)
	  (cons b (%make-bindings ($cdr lhs*) ($cdr rhs*) bc (+ i 1))))))

    (define (complex? x)
      (or ($binding-complex x)
	  (prelex-source-assigned? ($binding-lhs x))))

    (module (insert-order-edges)

      (define (insert-order-edges b*)
	(unless (null? b*)
	  (let ((b ($car b*)))
	    (if (complex? b)
		(mark b ($cdr b*))
	      (insert-order-edges ($cdr b*))))))

      (define (mark pb b*)
	(unless (null? b*)
	  (let ((b ($car b*)))
	    (if (complex? b)
		(let ((free* ($binding-free* b)))
		  (unless (memq pb free*)
		    ($set-binding-free*! b (cons pb free*)))
		  (mark b ($cdr b*)))
	      (mark pb ($cdr b*))))))

      #| end of module: insert-order-edges |# )

    #| end of module: do-recbind |# )

;;; --------------------------------------------------------------------

  (module (gen-letrecs)

    (define (gen-letrecs scc* ordered? body)
      (let-values (((fix* body)
		    (let recur ((scc* scc*))
		      (if (null? scc*)
			  (values '() body)
			(let-values (((fix* body) (recur ($cdr scc*))))
			  (gen-single-letrec ($car scc*) fix* body ordered?))))))
	(mkfix fix* body)))

    (define (mkfix b* body)
      (if (null? b*)
	  body
	(make-fix (map binding-lhs b*) (map binding-rhs b*)
	  body)))

    (module (gen-single-letrec)

      (define (gen-single-letrec scc fix* body ordered?)
	(cond ((null? ($cdr scc))
	       (let ((b ($car scc)))
		 (cond ((lambda-binding? b)
			(values (cons b fix*) body))
		       ((not (memq b ($binding-free* b)))
			(values '() (mklet (list ($binding-lhs b))
					   (list ($binding-rhs b))
					   (mkfix fix* body))))
		       (else
			(values '() (mklet (list ($binding-lhs b))
					   (%make-void-constants '(#f))
					   (mkset!s scc (mkfix fix* body))))))))
	      (else
	       (receive (lambda* complex*)
		   (partition lambda-binding? scc)
		 (if (null? complex*)
		     (values (append lambda* fix*) body)
		   (let ((complex* (if ordered?
				       (sort-bindings complex*)
				     complex*)))
		     (values '()
			     (mklet (map binding-lhs complex*)
				    (%make-void-constants complex*)
				    (mkfix (append lambda* fix*)
					   (mkset!s complex* body))))))))))

      (define (lambda-binding? x)
	(and (not (prelex-source-assigned? ($binding-lhs x)))
	     (clambda? ($binding-rhs x))))

      (define (mklet lhs* rhs* body)
	(if (null? lhs*)
	    body
	  (make-bind lhs* rhs* body)))

      (define (mkset!s b* body)
	;;Recursive function.
	;;
	(if (null? b*)
	    body
	  (let* ((b   ($car b*))
		 (lhs ($binding-lhs b)))
	    (unless (prelex-source-assigned? lhs)
	      (set-prelex-source-assigned?! lhs (or (prelex-global-location lhs) #t)))
	    (make-seq (make-assign lhs ($binding-rhs b))
		      (mkset!s ($cdr b*) body)))))

      (define (sort-bindings ls)
	(list-sort (lambda (x y)
		     (< ($binding-serial x)
			($binding-serial y)))
		   ls))

      #| end of module: gen-single-letrec |# )

    #| end of module: gen-letrecs |# )

;;; --------------------------------------------------------------------

  (module (get-sccs-in-order)

    (define-struct node
      (data link* lowlink root done collection))

    (define (get-sccs-in-order n* e** data*)
      (let* ((G    (create-graph n* e** data*))
	     (sccs (compute-sccs G)))
	(map (lambda (scc)
	       (map node-data scc))
	  sccs)))

    (define (create-graph v* e** data*)
      (define h
	(make-eq-hashtable))
      (let ((v* (let f ((v*    v*)
			(data* data*))
		  (if (null? v*)
		      '()
		    (let ((node (make-node ($car data*) '() #f #f #f #f)))
		      (hashtable-set! h ($car v*) node)
		      (cons node (f ($cdr v*) ($cdr data*))))))))
	(for-each (lambda (v e*)
		    (set-node-link*! v (map (lambda (f)
					      (or (hashtable-ref h f #f)
						  (error __who__ "invalid node" f)))
					 e*)))
	  v* e**)
	v*))

    (define (compute-sccs v*)
      ;;Tarjan's algorithm.
      (define scc* '())
      (define (%compute-sccs v)
	(define index 0)
	(define stack '())
	(define (tarjan v)
	  (let ((v-index index))
	    (set-node-root! v v-index)
	    (set! stack (cons v stack))
	    (set! index (fx+ index 1))
	    (for-each (lambda (v^)
			(unless (node-done v^)
			  (unless (node-root v^)
			    (tarjan v^))
			  (set-node-root! v (fxmin (node-root v)
						   (node-root v^)))))
	      (node-link* v))
	    (when (fx= (node-root v) v-index)
	      (set! scc* (cons (let recur ((ls stack))
				 (let ((v^ ($car ls)))
				   (set-node-done! v^ #t)
				   (cons v^ (if (eq? v^ v)
						(begin (set! stack ($cdr ls)) '())
					      (recur ($cdr ls))))))
			       scc*)))))
	(tarjan v))
      (for-each (lambda (v)
		  (unless (node-done v)
		    (%compute-sccs v)))
	v*)
      (reverse scc*))

    #| end of module: get-sccs-in-order |# )

  #| end of module: optimize-letrec/scc |# )


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; eval: (put 'make-bind 'scheme-indent-function 2)
;; eval: (put 'make-fix 'scheme-indent-function 2)
;; eval: (put '%make-bind 'scheme-indent-function 2)
;; eval: (put '$make-fix 'scheme-indent-function 2)
;; eval: (put 'with-unseen-prel 'scheme-indent-function 1)
;; End:
