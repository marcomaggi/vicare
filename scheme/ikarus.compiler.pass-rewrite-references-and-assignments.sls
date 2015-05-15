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
(library (ikarus.compiler.pass-rewrite-references-and-assignments)
  (export pass-rewrite-references-and-assignments)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
;;We distinguish  between bindings that  are only referenced  (unassigned, read-only,
;;constant)  and bindings  that are  also  assigned (read-write,  mutated).  We  also
;;distinguish  between lexical  top level  bindings  originally defined  by the  core
;;language form LIBRARY-LETREC* and lexical  local bindings originally defined by the
;;core    language    forms    LET,    LETREC,    LETREC*,    LAMBDA,    CASE-LAMBDA,
;;ANNOTATED-CASE-LAMBDA.
;;
;;Remembering that the actual  value of a top level binding is  stored in the "value"
;;field of a loc gensym, this function performs the following transformations:
;;
;;* References to top level bindings are transformed into:
;;
;;     (funcall (primref $symbol-value) (constant ?loc))
;;
;;  which extracts the value from slot "value" of the loc gensym ?LOC.
;;
;;* Common assignments to top level bindings are transformed into:
;;
;;     (funcall (primref $set-symbol-value!) (constant ?loc) ?rhs)
;;
;;  which stores a new value in the slot @code{value} of the log gensym ?LOC.
;;
;;*  Single  assignments   to  top  level  bindings  which  also   serve  as  binding
;;  initialisations are transformed into:
;;
;;     (funcall (primref $init-symbol-value!) (constant ?loc) ?rhs)
;;
;;  which stores a  new value in the slot  "value" of ?LOC and, only if  the value is
;;  recognised at  run-time as being  closure object, also  stores value in  the slot
;;  "proc".
;;
;;* Definitions of assigned local bindings are transformed as follows:
;;
;;     (bind ((?prel ?init)) ?body)
;;     ===> (bind ((?tmp-prel ?init))
;;            (bind ((?prel (funcall (primref vector) ?tmp-prel)))
;;              ?body))
;;
;;  NOTE Assigned  local bindings whose RHS  expression is a CLAMBDA  struct are also
;;  transformed this  way.  After this compiler  pass: there are no  more BIND struct
;;  whose RHS is a CLAMBDA struct.
;;
;;*  References to  assigned local  bindings are  transformed from  standalone PRELEX
;;  structs to:
;;
;;     (funcall (primref $vector-ref) ?prel (constant 0))
;;
;;* Assignments to assigned local bindings are transformed as follows:
;;
;;     (assign ?prel ?rhs)
;;     ===> (funcall (primref $vector-set!) ?prel (constant 0) ?rhs)
;;
;;Accept as input a nested hierarchy of the following structs:
;;
;;   constant		prelex		primref
;;   bind		fix		conditional
;;   seq		clambda		assign
;;   forcall		funcall		typed-expr
;;
;;After  this  compiler pass:  there  are  no more  ASSIGN  structs  in the  returned
;;recordised code.
;;


(define-syntax __module_who__
  (identifier-syntax 'pass-rewrite-references-and-assignments))

(define* (pass-rewrite-references-and-assignments x)
  ;;Perform code transformation traversing the whole  hierarchy in X, which must be a
  ;;struct instance representing recordized code in the core language, and building a
  ;;new hierarchy of transformed, recordized code; return the new hierarchy.
  ;;
  (define-syntax E ;make the code more readable
    (identifier-syntax pass-rewrite-references-and-assignments))
  (struct-case x
    ((constant)
     x)

    ((typed-expr expr core-type)
     (make-typed-expr (E expr) core-type))

    ((prelex)
     (if (prelex-source-assigned? x)
	 ;;X is a reference to a lexical read-write binding.
	 (cond ((prelex-global-location x)
		;;Reference to  a lexical top  level binding;  LOC is the  loc gensym
		;;used to hold the value at run-time.
		=> (lambda (loc)
		     (%top-level-binding-reference loc)))
	       (else
		;;Reference to a lexical local binding.
		(%assigned-local-binding-reference x)))
       ;;X is a reference to a lexical read-only binding.
       (cond ((prelex-global-location x)
	      => (lambda (loc)
		   ;;Reference  to a  lexical top  level  binding; LOC  is the  loc
		   ;;gensym used to hold the value at run-time.
		   (%top-level-binding-reference loc)))
	     (else
	      ;;Reference to a lexical local binding.
	      x))))

    ((primref)
     x)

    ((bind lhs* rhs* body)
     (receive (outer-lhs* assigned-lhs* vector-prel*)
	 (%process-assigned-lhs* lhs*)
       (make-bind outer-lhs* ($map/stx E rhs*)
	 (%bind-assigned assigned-lhs* vector-prel* (E body)))))

    ((fix lhs* rhs* body)
     (make-fix lhs* ($map/stx E rhs*) (E body)))

    ((conditional test conseq altern)
     (make-conditional (E test) (E conseq) (E altern)))

    ((seq e0 e1)
     (make-seq (E e0) (E e1)))

    ((clambda label clause* cp freevar* name)
     (let ((clause*^ ($map/stx (lambda (clause)
				 ;;Process the  formals of every clause  to introduce
				 ;;transformations  for   assigned  formal  bindings.
				 ;;Also apply E to each body.
				 (struct-case clause
				   ((clambda-case info body)
				    (struct-case info
				      ((case-info label fml* proper)
				       (receive (fml* assigned-lhs* vector-prel*)
					   (%process-assigned-lhs* fml*)
					 (make-clambda-case
					  (make-case-info label fml* proper)
					  (%bind-assigned assigned-lhs* vector-prel* (E body)))))))))
		       clause*)))
       (make-clambda label clause*^ cp freevar* name)))

    ((forcall op rand*)
     (make-forcall op ($map/stx E rand*)))

    ((funcall rator rand*)
     (make-funcall (E rator) ($map/stx E rand*)))

    ((assign lhs rhs)
     (cond ((prelex-source-assigned? lhs)
	    => (lambda (where)
		 (cond ((symbol? where)
			;;Initialisation  single-assignment  of lexical  top  level
			;;binding; WHERE is the loc gensym used to hold the value.
			(%top-level-binding-init where (E rhs)))
		       ((prelex-global-location lhs)
			;;Common assignment  of lexical  top level binding;  LOC is
			;;the loc gensym used to hold the value.
			=> (lambda (loc)
			     (%top-level-binding-assignment loc (E rhs))))
		       (else
			(%assigned-local-binding-assignment lhs (E rhs))))))
	   (else
	    (compiler-internal-error __module_who__ __who__
	      "assigned PRELEX has non-assigned state" lhs x))))

    (else
     (compile-time-error __module_who__ __who__
       "invalid recordised expression" (unparse-recordized-code x)))))


(define-syntax-rule (%top-level-binding-init ?loc ?init)
  ;;Single initialisation  assignment of recursive  lexical top level  binding.  This
  ;;binding is defined with:
  ;;
  ;;   (bind ((?prel (constant #!void)))
  ;;     (assign ?prel ?init) ;<-- this assignment
  ;;     ?body)
  ;;
  ;;to allow the initialisation expression ?INIT  to access the machine word in which
  ;;the value is  stored; this binding has no other  assignments, and this assignment
  ;;is the operation that initialises it.
  (make-funcall (mk-primref '$init-symbol-value!) (list (make-constant ?loc) ?init)))

(define-syntax-rule (%top-level-binding-assignment ?loc ?rhs)
  ;;Assignment of lexical top level binding.
  (make-funcall (mk-primref '$set-symbol-value!)  (list (make-constant ?loc) ?rhs)))

(define-syntax-rule (%top-level-binding-reference ?loc)
  ;;Reference to lexical top level binding.
  (make-funcall (mk-primref '$symbol-value)       (list (make-constant ?loc))))

;;; --------------------------------------------------------------------

(define-syntax-rule (%assigned-local-binding-reference ?prel)
  ;;Reference to lexical local mutable binding.
  (make-funcall (mk-primref '$vector-ref)  (list ?prel (make-constant 0))))

(define-syntax-rule (%assigned-local-binding-assignment ?prel ?rhs)
  ;;Assignment of lexical local binding stored on the Scheme stack.
  (make-funcall (mk-primref '$vector-set!) (list ?prel (make-constant 0) ?rhs)))


(define (%process-assigned-lhs* lhs*)
  ;;Recursive  function.   LHS* is  a  list  of  struct  instances of  type  PRELEX
  ;;representing bindings.  Return 3 values:
  ;;
  ;;1. The  list of PRELEX  structs to which the  original RHS expressions  must be
  ;;bound.
  ;;
  ;;2. A list of PRELEX structs to which the vector expressions must be bound.
  ;;
  ;;3. A list of PRELEX structs used in the vector creation RHS expressions.
  ;;
  ;;In a trasformation from:
  ;;
  ;;   (let ((X 1)
  ;;         (Y 2))
  ;;     (set! X 3)
  ;;     (list X Y))
  ;;
  ;;to:
  ;;
  ;;   (let ((T 1)
  ;;         (Y 2))
  ;;     (let ((X (vector T)))
  ;;       ($vector-set! X 0 456)
  ;;       (list ($vector-ref X 0) Y)))
  ;;
  ;;the argument LHS* is a list of PRELEX structs: "(X Y)", the return values are:
  ;;
  ;;1. A list holding the PRELEX structs  "(T Y)" for the outer binding definitions
  ;;evaluating the original RHS expressions.
  ;;
  ;;2.  A  list holding  the PRELEX  for X  for the  inner, true,  assigned binding
  ;;definition.
  ;;
  ;;3. A list holding the PRELEX for T for the RHS vector creation expression.
  ;;
  (if (pair? lhs*)
      (let ((prel (car lhs*)))
	(receive (tmp* assigned-lhs* vector-prel*)
	    (%process-assigned-lhs* (cdr lhs*))
	  (if (and (prelex-source-assigned? prel)
		   (not (prelex-global-location prel)))
	      ;;PREL is an assigned lexical local binding.  We process it.
	      (let ((tmp (make-prelex-for-tmp-binding prel)))
		(values (cons tmp  tmp*)
			(cons prel assigned-lhs*)
			(cons tmp  vector-prel*)))
	    ;;PREL is  an unassigned  lexical local  binding or  a lexical  top level
	    ;;binding.  We skip it.
	    (values (cons prel tmp*) assigned-lhs* vector-prel*))))
    (values '() '() '())))

(define (%bind-assigned assigned-lhs* vector-prel* body)
  ;;ASSIGNED-LHS* must be a list of PRELEX structs representing the true read-write
  ;;bindings.
  ;;
  ;;VECTOR-PREL*  must be  a  list  of PRELEX  structs  representing references  to
  ;;temporary bindings holding the binding's values.
  ;;
  ;;BODY must be a struct instance representing  code to be evaluated in the region
  ;;of the bindings.
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
  ;;in  this case  ASSIGNED-LHS* is  the list  "(X)" and  VECTOR-PREL* is  the list
  ;;"(T)".
  ;;
  (if (null? assigned-lhs*)
      body
    (make-bind assigned-lhs*
	($map/stx (lambda (rhs)
		    (make-funcall (mk-primref 'vector) (list rhs)))
	  vector-prel*)
      body)))


;;;; done

#| end of library |# )

;;; end of file
