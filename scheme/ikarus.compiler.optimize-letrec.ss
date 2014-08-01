;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;;; introduction
;;
;;Here  we  give only  a  short  context introduction;  for  a  real introduction  to
;;processing LETREC  and LETREC*  syntaxes, and specifically  to understand  the code
;;below, we *must* read the following paper:
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
;; ----------------------------------------------------------------------------------
;;
;;Let's consider the following program:
;;
;;   (import (rnrs))
;;   (let ((A B))
;;     #t)
;;
;;it  will fail  with "unbound  identifier B";  we are  *not* concerned  with unbound
;;identifiers here.  So let's move on to the following program:
;;
;;   (import (rnrs))
;;   (let ((A 123))
;;     (let ((A A))
;;       #t))
;;
;;no errors here: the identifier A in reference position is captured by the outer LET
;;binding for A.  Now this program:
;;
;;   (import (rnrs))
;;   (let* ((A 123)
;;          (B A))
;;     #t)
;;
;;everything is all right; now this program:
;;
;;   (import (rnrs))
;;   (let* ((A 123)
;;          (A A))
;;     #t)
;;
;;again no  error: the identifier  A in reference position  is captured by  the first
;;LET* binding for A; LET* allows us to create bindings with the same name.
;;
;; ----------------------------------------------------------------------------------
;;
;;Finally, let's move to the LETREC syntax.  This program is legal:
;;
;;   (import (rnrs))
;;   (letrec ((A (lambda () A)))
;;     #t)
;;
;;because LETREC defines recursive bindings, so we  are allowed to reference A in the
;;right-hand side of  the binding for A itself,  as long as we put  such reference in
;;the body of a LAMBDA.
;;
;;This program is also legal:
;;
;;   (import (rnrs))
;;   (letrec ((A (lambda () B))
;;            (B (lambda () A)))
;;     #t)
;;
;;because the cross references to A and B are in the body of LAMBDA syntaxes.
;;
;;This program is illegal:
;;
;;   (import (rnrs))
;;   (letrec ((A (list A)))
;;     #t)
;;
;;because the  identifier A  in reference  position is not  in the  body of  a LAMBDA
;;syntax: to  evaluate the right-hand side  of the binding  we need the value  of the
;;binding  itself.   Notice  that  A  in  reference  position  is  *not*  an  unbound
;;identifier: it is captured  by the A in binding position; it  is just "illegal" and
;;we must detect this situation, according to R6RS.
;;
;;This program is illegal:
;;
;;   (import (rnrs))
;;   (letrec ((A 123)
;;            (B (list A)))
;;     #t)
;;
;;because the  identifier A  in reference  position is not  in the  body of  a LAMBDA
;;syntax: LETREC does not impose an order  to the evaluation of the init expressions,
;;so to evaluate the right-hand side of the  binding we need the value of the binding
;;itself.
;;
;; ----------------------------------------------------------------------------------
;;
;;Let's move  to the LETREC*  syntax; it  is similar but  not equal to  LETREC.  This
;;program is legal:
;;
;;   (import (rnrs))
;;   (letrec* ((A (lambda () A)))
;;     #t)
;;
;;because LETREC* defines recursive bindings, so we are allowed to reference A in the
;;right-hand side of  the binding for A itself,  as long as we put  such reference in
;;the body of a LAMBDA.
;;
;;This program is also legal:
;;
;;   (import (rnrs))
;;   (letrec* ((A (lambda () B))
;;             (B (lambda () A)))
;;     #t)
;;
;;because the cross references to A and B are in the body of LAMBDA syntaxes.
;;
;;This program is illegal:
;;
;;   (import (rnrs))
;;   (letrec* ((A (list A)))
;;     #t)
;;
;;because the  identifier A  in reference  position is not  in the  body of  a LAMBDA
;;syntax: to  evaluate the right-hand side  of the binding  we need the value  of the
;;binding itself.   Again, notice that  A in reference  position is *not*  an unbound
;;identifier: it is captured  by the A in binding position; it  is just "illegal" and
;;we must detect this situation, according to R6RS.
;;
;;This program is legal:
;;
;;   (import (rnrs))
;;   (letrec* ((A 123)
;;             (B (list A)))
;;     #t)
;;
;;because  LETREC* imposes  a  left-to-right  order to  the  evaluation  of the  init
;;expressions.
;;
;; ----------------------------------------------------------------------------------
;;
;;R6RS mandates that illegal references to bindings established by LETREC and LETREC*
;;are detected  at run time  and cause an assertion  violation to be  raised.  Vicare
;;detects them at compile time, so some fully R6RS-compliant code will not work under
;;Vicare.
;;
;;The following code is illegal under both R6RS and Vicare:
;;
;;   (import (rnrs))
;;   (letrec ((x y)
;;            (y x))
;;     'should-not-get-here)
;;
;;The following program will run under a R6RS-compliant implementation:
;;
;;   (import (rnrs))
;;   (letrec ((x (if (eq? (cons 1 2)
;;                        (cons 1 2))
;;                   x
;;                 1)))
;;     x)
;;
;;because the form X  in reference position in the right-hand side  of the binding is
;;never evaluated;  under Vicare this  code will rather  raise a syntax  violation at
;;compile time.
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

(define* (unique-prelex {x prelex?})
  (receive-and-return (x)
      (make-prelex (prelex-name    x)
		   (prelex-operand x))
    (set-prelex-source-referenced?! x #t)))

(define (%make-void-constants lhs*)
  ;;Build and  return a  list of  CONSTANT structs  representing #<void>  values, one
  ;;struct for each item in LHS*.
  ;;
  (map (lambda (x)
	 (make-constant (void)))
    lhs*))

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
  ;;We  need  to remember  that  this  function is  used  to  process the  result  of
  ;;expanding: libraries,  programs, standalone  expressions given  to EVAL  (both in
  ;;stateless environments and stateful interactive environments).
  ;;
  ;;The transformations performed by this module are equivalent to the following:
  ;;
  ;;   (letrec* ((?var ?init) ...) . ?body)
  ;;   ==> (let ((?var (void)) ...) (set! ?var ?init) ... . ?body)
  ;;
  ;;   (library-letrec* ((?var ?loc ?init) ...) . ?body)
  ;;   ==> (let ((?var (void)) ...) (set! ?var ?init) ... . ?body)
  ;;
  ;;   (letrec ((?var ?init) ...) . ?body)
  ;;   ==> (let ((?var (void)) ...)
  ;;         (let ((?tmp ?init) ...) (set! ?var ?tmp) ... . ?body))
  ;;
  ;;Notice that the transformation for LETREC is described also in the R5RS document.
  ;;
  ;;This module accepts as input a  struct instance representing recordized code with
  ;;the following struct types:
  ;;
  ;;	assign		bind		clambda
  ;;	conditional	constant	forcall
  ;;	funcall		mvcall		prelex
  ;;	primref		rec*bind	recbind
  ;;	seq
  ;;
  ;;and returns  a new  struct instance  representing recordized  code with  the same
  ;;types except RECBIND and REC*BIND which are replaced by a composition of BIND and
  ;;ASSIGN structures.
  ;;
  (define who 'optimize-letrec/basic)

  ;;Make the code more readable.
  (define-inline (optimize-letrec/basic x)
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
       (error who "invalid expression" (unparse-recordized-code x)))))

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
    (make-bind lhs* (map (lambda (x)
			   (make-constant (void)))
		      lhs*)
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
    (let ((tmp* (map unique-prelex lhs*)))
      (make-bind lhs* (map (lambda (x)
			     (make-constant (void)))
			lhs*)
		 (make-bind tmp* rhs* (build-assign* lhs* tmp* body)))))

  #| end of module: optimize-letrec/basic |# )


(module (optimize-letrec/waddell)
  ;;Perform transformations  to convert the  recordized representation of  LETREC and
  ;;LETREC*  forms into  LET-like forms  and assignments.   This function  performs a
  ;;transformation similar (but not equal to) the one described in the [WSD] paper.
  ;;
  ;;We  need  to remember  that  this  function is  used  to  process the  result  of
  ;;expanding: libraries,  programs, standalone  expressions given  to EVAL  (both in
  ;;stateless environments and stateful interactive environments).
  ;;
  ;;This module accepts as input a  struct instance representing recordized code with
  ;;the following struct types:
  ;;
  ;;	assign		bind		clambda
  ;;	conditional	constant	forcall
  ;;	funcall		mvcall		prelex
  ;;	primref		rec*bind	recbind
  ;;	seq
  ;;
  ;;and returns  a new  struct instance  representing recordized  code with  the same
  ;;types except  RECBIND and REC*BIND which  are replaced by a  composition of BIND,
  ;;FIX and ASSIGN structures.
  ;;
  ;;How we process LETREC and LETREC*
  ;;---------------------------------
  ;;
  ;;When  processing   a  LETREC  or   LETREC*  core  language   forms  (represented,
  ;;respectively, by  a RECBIND and  a REC*BIND  structure) we classify  each binding
  ;;into: simple, complex, lambda.
  ;;
  ;;If the form  is a LETREC: we do  *not* care about the order of  evaluation of the
  ;;right-hand sides, so we produce recordized code like this:
  ;;
  ;;   (let ((?simple.lhs ?simple.rhs)
  ;;         ...)
  ;;     (let ((?complex.lhs (void))
  ;;           ...)
  ;;       (fix ((?lambda.lhs ?lambda.rhs)
  ;;             ...)
  ;;         (let ((?tmp ?complex.rhs)
  ;;               ...)
  ;;           (set! ?complex.lhs ?tmp)
  ;;           ...
  ;;           ?body))))
  ;;
  ;;in which the ?COMPLEX.RHS expressions are evaluated in unspecified order.
  ;;
  ;;If the  form is  a LETREC*: we  *do care*  about the order  of evaluation  of the
  ;;right-hand sides, so we produce recordized code like this:
  ;;
  ;;   (let ((?simple.lhs ?simple.rhs)
  ;;         ...)
  ;;     (let ((?complex.lhs (void))
  ;;           ...)
  ;;       (fix ((?lambda.lhs ?lambda.rhs)
  ;;             ...)
  ;;         (set! ?complex.lhs ?complex.rhs)
  ;;         ...
  ;;         ?body)))
  ;;
  ;;in which  the ?COMPLEX.RHS expressions are  evaluated in the same  order in which
  ;;they appear in the core language form.
  ;;
  ;;NOTE Upon entering this compiler pass, the PRELEX structures representing defined
  ;;bindings already have  the field SOURCE-ASSIGNED? correctly set;  a previous pass
  ;;has determined if a binding is assigned or not.
  ;;
  ;;Examples
  ;;--------
  ;;
  ;;Let's look at some examples:
  ;;
  ;; (let ((a 1))
  ;;   (let ((a a))
  ;;     a))
  ;; ==> (let* ((a_0 '1)
  ;;            (a_1 a_0))
  ;;       a_1)
  ;;
  ;; (let ((a 1))
  ;;   (let ((a 2))
  ;;     (let ((a 3))
  ;;       a)))
  ;; ==> (let* ((a_0 '1)
  ;;            (a_1 '2)
  ;;            (a_2 '3))
  ;;       a_2)
  ;;
  ;; (letrec ((a 1)
  ;;          (b 2))
  ;;   (list a b))
  ;; ==> (let ()
  ;;       (let ((a_0 '#<void>)
  ;;             (b_0 '#<void>))
  ;;         (fix ()
  ;;              (let ((a_1 '1)
  ;;                    (b_1 '2))
  ;;                (begin
  ;;                  (set! a_0 a_1)
  ;;                  (set! b_0 b_1)
  ;;                  (list a_0 b_0))))))
  ;;
  ;; (letrec* ((a (lambda (x)
  ;;                (when x
  ;;                  (a #f))))
  ;;           (b 123)
  ;;           (c 456)
  ;;           (d (begin
  ;;                (set! c 789)
  ;;                9)))
  ;;   a)
  ;; ==> (let ()
  ;;       (let ((b_0 '#<void>)
  ;;             (c_0 '#<void>)
  ;;             (d_0 '#<void>))
  ;;         (fix ((a_0 (lambda (x_0)
  ;;                      (if x_0
  ;;                          (a_0 '#f)
  ;;                        (void)))))
  ;;              (begin
  ;;                (set! b_0 '123)
  ;;                (set! c_0 '456)
  ;;                (set! d_0 (begin
  ;;                            (set! c_0 '789)
  ;;                            '9))
  ;;                a_0))))
  ;;
  ;; (letrec* ((a 123)
  ;;           (b 2)
  ;;           (c b)
  ;;           (d (lambda () 123)))
  ;;   b)
  ;; ==> (let ()
  ;;       (let ((a_0 '#<void>)
  ;;             (b_0 '#<void>)
  ;;             (c_0 '#<void>))
  ;;         (fix ((d_0 (lambda () '123)))
  ;;              (begin
  ;;                (set! a_0 '123)
  ;;                (set! b_0 '2)
  ;;                (set! c_0 b_0)
  ;;                b_0))))
  ;;
  ;; (letrec* ((a 123)
  ;;           (b 2)
  ;;           (c b)
  ;;           (d (lambda () 123)))
  ;;   (set! d 123)
  ;;   b)
  ;; ==> (let ()
  ;;       (let ((a_0 '#<void>)
  ;;             (b_0 '#<void>)
  ;;             (c_0 '#<void>)
  ;;             (d_0 '#<void>))
  ;;         (fix ()
  ;;              (begin
  ;;                (set! a_0 '123)
  ;;                (set! b_0 '2)
  ;;                (set! c_0 b_0)
  ;;                (set! d_0 (lambda () '123))
  ;;                (set! d_0 '123)
  ;;                b_0))))
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'optimize-letrec/waddell))

  (define (optimize-letrec/waddell x)
    (E x
       ;;This is the outer implementation of the function REGISTER-LHS-USAGE!.
       (%make-top-lhs-usage-register-func)
       ;;This is the outer implementation of the thunk MAKE-THE-ENCLOSING-RHS-COMPLEX!.
       (%make-top-rhs-complexity-register-func)))

  (module (E)

    (define (E x register-lhs-usage! make-the-enclosing-rhs-complex!)
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
	 (make-assign lhs (E rhs register-lhs-usage! make-the-enclosing-rhs-complex!)))

	((primref)
	 ;;X is a primitive function reference; for example as + appears in:
	 ;;
	 ;;   (map + '(1 2 3))
	 x)

	((bind lhs* rhs* body)
	 ;;X is a binding creation form like LET.  Do RHS* first, then BODY.
	 (let* ((rhs*^ (E* rhs* register-lhs-usage! make-the-enclosing-rhs-complex!))
		(body^ (E body
			  ;;Inside  a  binding  build  a   new  function  to  act  as
			  ;;REGISTER-LHS-USAGE!.
			  (%make-middle-lhs-usage-register-func lhs* register-lhs-usage!)
			  make-the-enclosing-rhs-complex!)))
	   (make-bind lhs* rhs*^ body^)))

	((recbind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body register-lhs-usage! make-the-enclosing-rhs-complex!)
	   (%do-recbind lhs* rhs* body register-lhs-usage! make-the-enclosing-rhs-complex!)))

	((rec*bind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body register-lhs-usage! make-the-enclosing-rhs-complex!)
	   (%do-rec*bind lhs* rhs* body register-lhs-usage! make-the-enclosing-rhs-complex!)))

	((conditional test conseq altern)
	 (make-conditional (E test register-lhs-usage! make-the-enclosing-rhs-complex!)
	     (E conseq register-lhs-usage! make-the-enclosing-rhs-complex!)
	   (E altern register-lhs-usage! make-the-enclosing-rhs-complex!)))

	((seq e0 e1)
	 (make-seq
	  (E e0 register-lhs-usage! make-the-enclosing-rhs-complex!)
	  (E e1 register-lhs-usage! make-the-enclosing-rhs-complex!)))

	((clambda)
	 (E-clambda x register-lhs-usage! make-the-enclosing-rhs-complex!))

	((funcall)
	 (E-funcall x register-lhs-usage! make-the-enclosing-rhs-complex!))

	((mvcall)
	 (E-mvcall x register-lhs-usage! make-the-enclosing-rhs-complex!))

	((forcall rator rand*)
	 ;;This is a foreign function call.
	 (make-forcall rator (E* rand* register-lhs-usage! make-the-enclosing-rhs-complex!)))

	(else
	 (error __who__ "invalid expression" (unparse-recordized-code x)))))

    (define (E* x* register-lhs-usage! make-the-enclosing-rhs-complex!)
      (if (null? x*)
	  '()
	(cons (E  ($car x*) register-lhs-usage! make-the-enclosing-rhs-complex!)
	      (E* ($cdr x*) register-lhs-usage! make-the-enclosing-rhs-complex!))))

    (module (E-clambda)

      (define (E-clambda x register-lhs-usage! make-the-enclosing-rhs-complex!)
	(struct-case x
	  ((clambda label clause* cp free name)
	   (make-clambda label (map (lambda (cls)
				      (E-clambda-case cls register-lhs-usage!))
				 clause*)
			 cp free name))))

      (define (E-clambda-case clause register-lhs-usage!)
	(struct-case clause
	  ((clambda-case info body)
	   (make-clambda-case info
			      (E body
				 (%make-middle-lhs-usage-register-func (case-info-args info)
								       register-lhs-usage!
								       #;(%make-top-lhs-usage-register-func)
								       ;; (lambda (prel)
								       ;; 	 (register-lhs-usage! prel #f))
								       )
				 (%make-top-rhs-complexity-register-func))))))

      #| end of module: E-clambda |# )

    (module (E-funcall)

      (define (E-funcall x register-lhs-usage! make-the-enclosing-rhs-complex!)
	(struct-case x
	  ((funcall rator rand*)
	   (let ((rator^ (E  rator register-lhs-usage! make-the-enclosing-rhs-complex!))
		 (rand*^ (E* rand* register-lhs-usage! make-the-enclosing-rhs-complex!)))
	     ;;This form  is a function  call.  In general,  we must assume  it might
	     ;;mutate any of the bindings whose region includes it: so we should call
	     ;;MAKE-THE-ENCLOSING-RHS-COMPLEX!.
	     (struct-case rator^
	       ((primref op)
		(if (eq? op 'top-level-value)
		    (struct-case (car rand*^)
		      ((constant name)
		       (unless (memq name SIMPLE-PRIMITIVES)
			 (make-the-enclosing-rhs-complex!)))
		      (else
		       (make-the-enclosing-rhs-complex!)))
		  (make-the-enclosing-rhs-complex!)))
	       (else
		(make-the-enclosing-rhs-complex!)))
	     (make-funcall rator^ rand*^)))))

      (define-constant SIMPLE-PRIMITIVES '(display))

      #| end of module: E-funcall |# )

    (define (E-mvcall x register-lhs-usage! make-the-enclosing-rhs-complex!)
      (struct-case x
	((mvcall producer consumer)
	 ;;This  form is  a function  call.  We  assume it  might mutate  any of  the
	 ;;bindings     whose     region     includes      it:     so     we     call
	 ;;MAKE-THE-ENCLOSING-RHS-COMPLEX!.
	 (make-the-enclosing-rhs-complex!)
	 (make-mvcall (E producer register-lhs-usage! make-the-enclosing-rhs-complex!)
		      (E consumer register-lhs-usage! make-the-enclosing-rhs-complex!)))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (module USAGE-LHS-FLAGS
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

    #| end of module: USAGE-LHS-FLAGS |# )

;;; --------------------------------------------------------------------

  (module RHS-COMPLEXITY-FLAGS
    (%make-rhs-complexity-flags
     %rhs-complexity-flags-set!
     %rhs-complexity-flags-ref)
    ;;This module handles values called CPLX-RHS-FLAGS  in the code.  Such values are
    ;;associative containers representing  a property of the RHS  of bindings defined
    ;;by  RECBIND  (LETREC) and  REC*BIND  (LETREC*).   Each container  maps  binding
    ;;indexes to flags  representing right-hand side init  expression complexity: the
    ;;flag is true if  the RHS expression is "complex", that is it  may assign one of
    ;;the LHS in the same lexical contour (we  cannot be sure if it actually does it,
    ;;nor of which bindings are mutated).
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

  (module (%make-top-lhs-usage-register-func
	   %make-middle-lhs-usage-register-func
	   %make-recbind-lhs-usage-register-func)

    (define (%make-top-lhs-usage-register-func)
      ;;Build  and  return a  top  variable-usage  register function.   Top  register
      ;;functions  are  generated  only  when  starting  to  process  a  whole  input
      ;;expression, not when  entering a nested subexpression  representing a binding
      ;;form.
      ;;
      ;;Given the  way the register  functions are  implemented: if this  function is
      ;;actually applied  to a PRELEX  structure, it  means such PRELEX  represents a
      ;;free variable in the whole expression; this is an error.
      ;;
      (lambda (prel)
	(error __who__ "found free variable reference" prel)))

    (define (%make-middle-lhs-usage-register-func prel* outer-register-lhs-usage!)
      ;;Build and return a new middle-level variable-usage register function wrapping
      ;;the  one  given as  argument.   The  returned  register function  will  avoid
      ;;attempting to mark as "used" the PRELEX structures in the list PREL*.
      ;;
      ;;The  middle-level  register  functions  are used  to  avoid  marking  binding
      ;;references created by non-RECBIND (non-LETREC) and non-REC*BIND (non-LETREC*)
      ;;binding  forms;  so  it  must  be  used when  entering  a  BIND  and  CLAMBDA
      ;;subexpression.
      ;;
      (define-constant TABLE (make-eq-hashtable))
      (for-each (lambda (prel)
		  (hashtable-set! TABLE prel #t))
	prel*)
      (lambda (prel)
	;;A middle-level variable-usage function attempts  to mark a PRELEX structure
	;;as "used"  only once.  So  avoid to  apply REGISTER-LHS-USAGE!  to  PREL if
	;;PREL is already in the TABLE.
	(with-unseen-prel (prel TABLE)
	  (outer-register-lhs-usage! prel))))

    (define (%make-recbind-lhs-usage-register-func prel* lhs-index used-lhs-flags
						   outer-register-lhs-usage!)
      ;;Build  and return  a new  recursive-binding variable-usage  register function
      ;;wrapping  the one  given as  argument.  The  returned register  function will
      ;;avoid attempting to mark as "used" the PRELEX structures in the list PREL*.
      ;;
      (define-constant TABLE (make-eq-hashtable))
      (lambda (prel)
	(import USAGE-LHS-FLAGS)
	(with-unseen-prel (prel TABLE)
	  (cond ((%find-index 0 prel prel*)
		 => (lambda (lhs-index)
		      (%lhs-usage-flags-set! used-lhs-flags lhs-index)))
		(else
		 (outer-register-lhs-usage! prel)))
	  ;; (if (memq prel prel*)
	  ;;     (begin
	  ;; 	(debug-print (%find-index 0 prel prel*) lhs-index)
	  ;; 	(%lhs-usage-flags-set! used-lhs-flags lhs-index))
	  ;;   (outer-register-lhs-usage! prel))
	  )))

    (define-syntax (with-unseen-prel stx)
      (syntax-case stx ()
	((_ (?prel ?table) . ?body)
	 (and (identifier? #'?prel)
	      (identifier? #'?table))
	 #'(unless (hashtable-ref ?table ?prel #f)
	     (hashtable-set! ?table ?prel #t)
	     . ?body))))

    (define (%find-index base item ell)
      (cond ((null? ell)
	     #f)
	    ((eq? item ($car ell))
	     base)
	    (else
	     (%find-index ($fxadd1 base) item ($cdr ell)))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define-syntax-rule (%make-top-rhs-complexity-register-func)
    ;;Return a top thunk to be used as expression complexity registrar.  The returned
    ;;thunk is  to be used  as outer thunk when  entering whole input  expressions or
    ;;CLAMBDA bodies.
    ;;
    void)

  (define (%make-recbind-rhs-complexity-register-func cplx-rhs-flags rhs-index make-the-enclosing-rhs-complex!)
    ;;Build and  return a new  thunk to be  used as expression  complexity registrar,
    ;;wrapping the  one given as  argument.  Called  to register that  the right-hand
    ;;side init expression with index RHS-INDEX in the container CPLX-RHS-FLAGS might
    ;;mutate a binding  among the ones in  the same lexical contour;  this means that
    ;;the outer expression enclosing this RHS is also complex.
    ;;
    ;;We really need one of these thunks for each RHS in a recbind lexical contour.
    ;;
    (lambda ()
      (import RHS-COMPLEXITY-FLAGS)
      (%rhs-complexity-flags-set! cplx-rhs-flags rhs-index)
      (make-the-enclosing-rhs-complex!)))

;;; --------------------------------------------------------------------

  (module (%do-recbind %do-rec*bind)

    (define-syntax-rule (%do-recbind ?lhs* ?rhs* ?body ?register-lhs-usage! ?make-the-enclosing-rhs-complex!)
      (%true-do-recbind ?lhs* ?rhs* ?body ?register-lhs-usage! ?make-the-enclosing-rhs-complex! #t))

    (define-syntax-rule (%do-rec*bind ?lhs* ?rhs* ?body ?register-lhs-usage! ?make-the-enclosing-rhs-complex!)
      (%true-do-recbind ?lhs* ?rhs* ?body ?register-lhs-usage! ?make-the-enclosing-rhs-complex! #f))

    (define (%true-do-recbind lhs* rhs* body register-lhs-usage! make-the-enclosing-rhs-complex! letrec?)
      ;;If  the core  language form  we are  processing is  a RECBIND  representing a
      ;;LETREC: the  argument LETREC?   is true.   If the core  language form  we are
      ;;processing is  a REC*BIND  representing a LETREC*:  the argument  LETREC?  is
      ;;false.
      ;;
      (import USAGE-LHS-FLAGS RHS-COMPLEXITY-FLAGS)
      (let ((used-lhs-flags	(%make-lhs-usage-flags      lhs*))
	    (cplx-rhs-flags	(%make-rhs-complexity-flags rhs*)))
	;;The lists  of bindings LHS*  and RHS* are  interpreted as tuples,  in which
	;;every item has an index: from 0 to "(sub1 (length lhs*))".
	;;
	(let ((rhs*^ (%do-rhs* 0 lhs* rhs*
			       register-lhs-usage! make-the-enclosing-rhs-complex!
			       used-lhs-flags cplx-rhs-flags))
	      (body^ (E body
			(%make-middle-lhs-usage-register-func lhs* register-lhs-usage!)
			make-the-enclosing-rhs-complex!)))
	  (receive (simple.lhs* simple.rhs* lambda.lhs* lambda.rhs* complex.lhs* complex.rhs*)
	      (%partition-rhs* 0 lhs* rhs*^ used-lhs-flags cplx-rhs-flags)
	    (make-bind simple.lhs* simple.rhs*
	      (make-bind complex.lhs* (%make-void-constants complex.lhs*)
		(make-fix lambda.lhs* lambda.rhs*
		  (if letrec?
		      ;;It is a RECBIND and LETREC: no order enforced when evaluating
		      ;;COMPLEX.RHS*.
		      (let ((tmp* (map unique-prelex complex.lhs*)))
			(make-bind tmp* complex.rhs*
			  (build-assign* complex.lhs* tmp* body^)))
		    ;;It is  a REC*BIND and  LETREC*: order enforced  when evaluating
		    ;;COMPLEX.RHS*.
		    (build-assign* complex.lhs* complex.rhs* body^)))))))))

    (define (%do-rhs* i lhs* rhs*
		      register-lhs-usage! make-the-enclosing-rhs-complex!
		      used-lhs-flags cplx-rhs-flags)
      ;;Recursively process RHS* and return a list of struct instances which is meant
      ;;to replace the original RHS*.
      ;;
      ;;This function has two purposes:
      ;;
      ;;1. Apply E to each struct in RHS*.
      ;;
      ;;2. Fill appropriately the vectors USED-LHS-FLAGS and CPLX-RHS-FLAGS.
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
      ;;   (%do-rhs* 0 '(?lhs-0 ?lhs-1 ?lhs-2) '(?rhs-0 ?rhs-1 ?rhs-2) ---)
      ;;   (%do-rhs* 1 '(?lhs-0 ?lhs-1 ?lhs-2)        '(?rhs-1 ?rhs-2) ---)
      ;;   (%do-rhs* 2 '(?lhs-0 ?lhs-1 ?lhs-2)               '(?rhs-2) ---)
      ;;   (%do-rhs* 3 '(?lhs-0 ?lhs-1 ?lhs-2)                     '() ---)
      ;;
      (if (null? rhs*)
	  '()
	(let ((rest (%do-rhs* (fxadd1 i) lhs* ($cdr rhs*)
			      register-lhs-usage! make-the-enclosing-rhs-complex!
			      used-lhs-flags cplx-rhs-flags)))
	  (cons (E ($car rhs*)
		   (%make-recbind-lhs-usage-register-func lhs* i used-lhs-flags register-lhs-usage!)
		   (%make-recbind-rhs-complexity-register-func cplx-rhs-flags i make-the-enclosing-rhs-complex!))
		rest))))

    (define (%partition-rhs* i lhs* rhs* used-lhs-flags cplx-rhs-flags)
      ;;Non-tail  recursive  function.   Make  use  of the  data  in  the  containers
      ;;USED-LHS-FLAGS  and CPLX-RHS-FLAGS  to partition  the bindings  into: simple,
      ;;lambda, complex.  (RHS* is not visited here.)
      ;;
      ;;Return 6 values:
      ;;
      ;;SIMPLE.LHS*, SIMPLE.RHS*
      ;;   Simple  bindings.  SIMPLE.LHS is  never assigned in  all the RHS*  and the
      ;;    associated SIMPLE.RHS  is a  simple  expression: it  never references  an
      ;;   SIMPLE.LHS* and it does not call any function.
      ;;
      ;;LAMBDA.LHS*, LAMBDA.RHS*
      ;;   Lambda bindings.   Lists of LHS and  RHS whose RHS is a  CLAMBDA and whose
      ;;   LHS is never assigned in the RHS*.
      ;;
      ;;COMPLEX.LHS*, COMPLEX.RHS*
      ;;   Complex bindings.  Lists of LHS and  RHS for which either we know that the
      ;;   LHS has been assigned, or we know that the RHS may have assigned an LHS.
      ;;
      (import USAGE-LHS-FLAGS RHS-COMPLEXITY-FLAGS)
      (if (null? lhs*)
	  (values '() '() '() '() '() '())
	(let-values
	    (((simple.lhs* simple.rhs* lambda.lhs* lambda.rhs* complex.lhs* complex.rhs*)
	      (%partition-rhs* (fxadd1 i) ($cdr lhs*) ($cdr rhs*) used-lhs-flags cplx-rhs-flags))
	     ((lhs rhs)
	      (values ($car lhs*) ($car rhs*))))
	  (cond ((prelex-source-assigned? lhs)
		 ;;This binding is "complex".  Notice that: even if the corresponding
		 ;;RHS is  a CLAMBDA, this  binding cannot be classified  as "lambda"
		 ;;and cannot  be defined by  a FIX struct.  Only  unassigned CLAMBDA
		 ;;bindings can  be classified as  "lambda"; it does *not*  matter if
		 ;;the assignment happens inside the body of a CLAMBDA or in the body
		 ;;of a LETREC or LETREC* form.
		 (values simple.lhs* simple.rhs*
			 lambda.lhs* lambda.rhs*
			 (cons lhs complex.lhs*) (cons rhs complex.rhs*)))
		((clambda? rhs)
		 ;;This binding is "lambda".  This classification has precedence over
		 ;;the binding being "complex".
		 (values simple.lhs* simple.rhs*
			 (cons lhs lambda.lhs*) (cons rhs lambda.rhs*)
			 complex.lhs* complex.rhs*))
                ((or (%lhs-usage-flags-ref      used-lhs-flags i)
		     (%rhs-complexity-flags-ref cplx-rhs-flags i))
		 ;;This binding is "complex".
		 (values simple.lhs* simple.rhs*
			 lambda.lhs* lambda.rhs*
			 (cons lhs complex.lhs*) (cons rhs complex.rhs*)))
		(else
		 ;;This binding is "simple".
		 (values (cons lhs simple.lhs*) (cons rhs simple.rhs*)
			 lambda.lhs* lambda.rhs*
			 complex.lhs* complex.rhs*))
		))))

    #| end of module: %DO-RECBIND %DO-REC*BIND |# )

  #| end of module: OPTIMIZE-LETREC/WADDELL |# )


(module (optimize-letrec/scc)
  ;;Perform transformations  to convert the  recordized representation of  LETREC and
  ;;LETREC*  forms into  LET  forms  and assignments.   This  function  does what  is
  ;;described in the [GD] paper.
  ;;
  ;;We  need  to remember  that  this  function is  used  to  process the  result  of
  ;;expanding: libraries,  programs, standalone  expressions given  to EVAL  (both in
  ;;stateless environments and stateful interactive environments).
  ;;
  ;;This  module  accepts  as   input  a  struct  instance  representing
  ;;recordized code with the following struct types:
  ;;
  ;;assign		bind		clambda
  ;;conditional		constant	forcall
  ;;funcall		mvcall		prelex
  ;;primref		rec*bind	recbind
  ;;seq
  ;;
  ;;and returns a new struct  instance representing recordized code with
  ;;the same types  except RECBIND and REC*BIND which are  replaced by a
  ;;composition of BIND, FIX and ASSIGN structures.
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
		;False or  a struct instance  of type BINDING  being the
		;previous value.
     free*))

  (define (optimize-letrec/scc x)
    (let ((x (E x (make-binding #f #f #f #t #t '()))))
      ;;(pretty-print (unparse-recordized-code x))
      x))

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
					   (list (make-funcall (make-primref 'void) '()))
					   (mkset!s scc (mkfix fix* body))))))))
	      (else
	       (let-values (((lambda* complex*) (partition lambda-binding? scc)))
		 (if (null? complex*)
		     (values (append lambda* fix*) body)
		   (let ((complex* (if ordered?
				       (sort-bindings complex*)
				     complex*)))
		     (values '()
			     (mklet (map binding-lhs complex*)
				    (map (lambda (x)
					   (make-funcall (make-primref 'void) '()))
				      complex*)
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
;; eval: (put 'with-unseen-prel 'scheme-indent-function 1)
;; End:
