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
(library (ikarus.compiler.pass-introduce-closure-makers)
  (export introduce-closure-makers)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
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

  #| end of module: E-clambda* |# )


;;;; done

#| end of library |# )

;;; end of file
