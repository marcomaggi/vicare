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
(library (ikarus.compiler.pass-rewrite-freevar-references)
  (export rewrite-freevar-references)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
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


;;;; done

#| end of library |# )

;;; end of file
