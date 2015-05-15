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
(library (ikarus.compiler.pass-optimize-direct-calls)
  (export pass-optimize-direct-calls)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
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
;;   typed-expr
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


(define-syntax __module_who__
  (identifier-syntax 'pass-optimize-direct-calls))

(define-syntax E ;make the code more readable
  (identifier-syntax pass-optimize-direct-calls))

(define* (pass-optimize-direct-calls x)
  ;;Perform code  optimisation traversing the whole  hierarchy in X, which  must be a
  ;;struct instance representing recordized code in the core language, and building a
  ;;new hierarchy of optimised, recordized code; return the new hierarchy.
  ;;
  ;;The  only recordized  code that  may  actually need  inlining transformation  are
  ;;FUNCALL instances.
  ;;
  (struct-case x
    ((constant)
     x)

    ((typed-expr expr core-type)
     (make-typed-expr (E expr) core-type))

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
     (compile-time-error __module_who__ __who__
       "invalid expression" (unparse-recordized-code x)))))


(module (%attempt-integration)

  (define (%attempt-integration mk rator rand*)
    ;;Attempt to integrate the operator of an application form.
    ;;
    ;;MK  is MAKE-FUNCALL  or  a wrapper  for  it.  RATOR  is  the already  processed
    ;;operator  of the  application form.   RAND* is  the list  of already  processed
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
    (let ((annotation (car rand*))
	  (orig-rator (cadr rand*))
	  (orig-rand* (cddr rand*)))
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
	      (struct-case (car clause*)
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
	    ((%attempt-integration/clambda-clause (car clause*) rand*))
	    (else
	     (%attempt-integration/clambda (cdr clause*) rand* default))))

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
		(and (fx=? (length fml*)
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
	      (and (fx<=? (length fml*)
			  (length rand*))
		   (make-bind fml* (%properize-operands fml* rand*) body))))))))

    (define* (%properize-operands lhs* rhs*)
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
	     (compile-time-error __module_who__ __who__ "improper improper"))
	    ((null? (cdr lhs*))
	     (list (%make-conses rhs*)))
	    (else
	     (cons (car rhs*)
		   (%properize-operands (cdr lhs*)
					(cdr rhs*))))))

    (define (%make-conses ls)
      (if (pair? ls)
	  (make-funcall (mk-primref 'cons)
			(list (car ls) (%make-conses (cdr ls))))
	(make-constant '())))

    #| end of module: %ATTEMPT-INTEGRATION/CLAMBDA |# )


;;;; done

#| end of library |# )

;;; end of file
