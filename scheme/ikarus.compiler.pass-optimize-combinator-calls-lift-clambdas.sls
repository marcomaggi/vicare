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
(library (ikarus.compiler.pass-optimize-combinator-calls-lift-clambdas)
  (export pass-optimize-combinator-calls/lift-clambdas)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
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
  (identifier-syntax 'pass-optimize-combinator-calls/lift-clambdas))

(define (pass-optimize-combinator-calls/lift-clambdas X)
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


;;;;

(define all-clambdas
  ;;While processing  an input expression:  this parameter  holds a proper  list of
  ;;CLAMBDA structs representing  the set of all the CLAMBDAs  defined in the input
  ;;expression.  These CLAMBDA structs will be compiled to machine code and used to
  ;;build the code objects.
  ;;
  (make-parameter #f))

(define-syntax-rule (%gather-clambda! ?obj)
  (all-clambdas (cons ?obj (all-clambdas))))


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


;;;; VAR structs and associated NODE structs

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


;;;; VAR structs and associated substitutions

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


;;;; done

#| end of library |# )

;;; end of file
