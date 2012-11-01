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
;;Here  we  give   only  a  short  context  introduction;   for  a  real
;;introduction   to  processing   LETREC  and   LETREC*  syntaxes,   and
;;specifically  to  understand  the  code  below,  we  *must*  read  the
;;following paper:
;;
;;   [WSD]  Oscar Waddell,  Dipanwita Sarkar,  R. Kent  Dybvig.  "Fixing
;;   Letrec:  A  Faithful  Yet   Efficient  Implementation  of  Scheme's
;;   Recursive Binding Construct"
;;
;;then  we can  move to  the following  paper, which  describes the  SCC
;;transformation used by Vicare:
;;
;;   [GD]   Abdulaziz  Ghuloum,   R.  Kent   Dybvig.   ``Fixing   Letrec
;;   (reloaded)''.  Workshop on Scheme and Functional Programming '09
;;
;; ---------------------------------------------------------------------
;;
;;Let's consider the following program:
;;
;;   (import (rnrs))
;;   (let ((A B))
;;     #t)
;;
;;it will fail with "unbound identifier  B"; we are *not* concerned with
;;unbound identifiers here.  So let's move on to the following program:
;;
;;   (import (rnrs))
;;   (let ((A 123))
;;     (let ((A A))
;;       #t))
;;
;;no errors here: the identifier A  in reference position is captured by
;;the outer LET binding for A.  Now this program:
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
;;again no error: the identifier A  in reference position is captured by
;;the first LET*  binding for A; LET* allows us  to create bindings with
;;the same name.
;;
;; ---------------------------------------------------------------------
;;
;;Finally, let's move to the LETREC syntax.  This program is legal:
;;
;;   (import (rnrs))
;;   (letrec ((A (lambda () A)))
;;     #t)
;;
;;because  LETREC  defines recursive  bindings,  so  we are  allowed  to
;;reference A  in the right-hand  side of the  binding for A  itself, as
;;long as we put such reference in the body of a LAMBDA.
;;
;;This program is also legal:
;;
;;   (import (rnrs))
;;   (letrec ((A (lambda () B))
;;            (B (lambda () A)))
;;     #t)
;;
;;because the  cross references  to A and  B are in  the body  of LAMBDA
;;syntaxes.
;;
;;This program is illegal:
;;
;;   (import (rnrs))
;;   (letrec ((A (list A)))
;;     #t)
;;
;;because the identifier A in reference position is not in the body of a
;;LAMBDA syntax: to evaluate the right-hand  side of the binding we need
;;the value of the binding itself.   Notice that A in reference position
;;is *not*  an unbound identifier:  it is captured  by the A  in binding
;;position;  it is  just "illegal"  and we  must detect  this situation,
;;according to R6RS.
;;
;;This program is illegal:
;;
;;   (import (rnrs))
;;   (letrec ((A 123)
;;            (B (list A)))
;;     #t)
;;
;;because the identifier A in reference  position is not in the body off
;;a LAMBDA syntax: LETREC does not  impose an order to the evaluation of
;;the  init expressions,  so  to  evaluate the  right-hand  side of  the
;;binding we need the value of the binding itself.
;;
;; ---------------------------------------------------------------------
;;
;;Let's  move to  the LETREC*  syntax; it  is similar  but not  equal to
;;LETREC.  This program is legal:
;;
;;   (import (rnrs))
;;   (letrec* ((A (lambda () A)))
;;     #t)
;;
;;because  LETREC* defines  recursive  bindings, so  we  are allowed  to
;;reference A  in the right-hand  side of the  binding for A  itself, as
;;long as we put such reference in the body of a LAMBDA.
;;
;;This program is also legal:
;;
;;   (import (rnrs))
;;   (letrec* ((A (lambda () B))
;;             (B (lambda () A)))
;;     #t)
;;
;;because the  cross references  to A and  B are in  the body  of LAMBDA
;;syntaxes.
;;
;;This program is illegal:
;;
;;   (import (rnrs))
;;   (letrec* ((A (list A)))
;;     #t)
;;
;;because the identifier A in reference position is not in the body of a
;;LAMBDA syntax: to evaluate the right-hand  side of the binding we need
;;the value  of the binding itself.   Again, notice that A  in reference
;;position is  *not* an unbound identifier:  it is captured by  the A in
;;binding  position;  it is  just  "illegal"  and  we must  detect  this
;;situation, according to R6RS.
;;
;;This program is legal:
;;
;;   (import (rnrs))
;;   (letrec ((A 123)
;;            (B (list A)))
;;     #t)
;;
;;because LETREC* imposes a left-to-right order to the evaluation of the
;;init expressions.
;;
;; ---------------------------------------------------------------------
;;
;;R6RS  mandates  that illegal  references  to  bindings established  by
;;LETREC and  LETREC* are detected  at run  time and cause  an assertion
;;violation to be raised.  Vicare detects  them at compile time, so some
;;fully R6RS-compliant code will not work under Vicare.
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
;;because the form X in reference position in the right-hand side of the
;;binding is never  evaluated; under Vicare this code  will rather raise
;;an assertion violation and syntax violation at compile time.
;;


(module (debug-scc
	 optimize-letrec
	 current-letrec-pass
	 check-for-illegal-letrec)

  (define debug-scc
    (make-parameter #f))

  (define check-for-illegal-letrec
    (make-parameter #t
      (lambda (obj)
	(and obj #t))))

  (module (current-letrec-pass)

    (define current-letrec-pass
      (make-parameter 'scc
	(lambda (x)
	  (define who 'current-letrec-pass)
	  (with-arguments-validation (who)
	      ((letrec-pass x))
	    x))))

    (define-argument-validation (letrec-pass who obj)
      (memq obj '(scc waddell basic))
      (assertion-violation who
	"invalid letrec optimization mode, expected a symbol among: scc, waddell, basic"
	obj))

    #| end of module |# )

  (define (optimize-letrec x)
    (define who 'optimize-letrec)
    (when (check-for-illegal-letrec)
      (check-for-illegal-letrec-references x))
    (case-symbols (current-letrec-pass)
      ((scc)     (optimize-letrec/scc     x))
      ((waddell) (optimize-letrec/waddell x))
      ((basic)   (optimize-letrec/basic   x))
      (else
       (assertion-violation who
	 "invalid letrec optimization mode" (current-letrec-pass)))))


;;;; helpers

(define (unique-prelex x)
  (let ((x (make-prelex (prelex-name    x)
			(prelex-operand x))))
    ($set-prelex-source-referenced?! x #t)
    x))

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
    (cond ((C x '())
	   => (lambda (illegal)
		(%error illegal x)))))

  ;;FIXME Here we use  a list to hold the set  of PRELEX structures that
  ;;is illegal to  reference in right-hand sided of  LETREC, LETREC* and
  ;;LIBRARY-LETREC* syntaxes.
  ;;
  ;;Doing  a  linear search  is  usually  fine  for LETREC  and  LETREC*
  ;;syntaxes, because  the list of  bindings is most likely  small.  But
  ;;LIBRARY-LETREC*  syntaxes will  have "many"  bindings, one  for each
  ;;defined function.
  ;;
  ;;It might  be worthwhile  to change  the implementation  to something
  ;;better than  lists as it  is done by  other functions in  this file.
  ;;(Marco Maggi; Nov 1, 2012)
  ;;
  (begin
    (define (%illegal-reference-to? x illegals)
      (cond ((memq x illegals)
	     => car)
	    (else #f)))
    (define-inline (%illegal-augment more illegals)
      (append more illegals)))

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
       (or (%illegal-reference-to? x illegals)
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
	 (C/error body '()))))

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
  ;;Perform   basic   transformations    to   convert   the   recordized
  ;;representation of LETREC  and LETREC* forms into  LET-like forms and
  ;;assignments.
  ;;
  ;;The transformations performed  by this module are  equivalent to the
  ;;following:
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
  ;;Notice that the  transformation for LETREC is described  also in the
  ;;R5RS document.
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
  ;;composition of BIND and ASSIGN structures.
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
    ;;the transformation  we do here  is equivalent to  constructing the
    ;;following form:
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
    ;;the transformation  we do here  is equivalent to  constructing the
    ;;following form:
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
  ;;Perform transformations to convert  the recordized representation of
  ;;LETREC and LETREC* forms into  LET-like forms and assignments.  This
  ;;function performs  a transformation similar  (but not equal  to) the
  ;;one described in the [WSD] paper.
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
  ;;           (d (lambda ()
  ;;                123)))
  ;;   b)
  ;; ==> (let ()
  ;;       (let ((a_0 '#<void>)
  ;;             (b_0 '#<void>)
  ;;             (c_0 '#<void>))
  ;;         (fix ((d_0 (lambda ()
  ;;                      '123)))
  ;;              (begin
  ;;                (set! a_0 '123)
  ;;                (set! b_0 '2)
  ;;                (set! c_0 b_0)
  ;;                b_0))))
  ;;
  ;; (letrec* ((a 123)
  ;;           (b 2)
  ;;           (c b)
  ;;           (d (lambda ()
  ;;                123)))
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
  ;;                (set! d_0 (lambda ()
  ;;                            '123))
  ;;                (set! d_0 '123)
  ;;                b_0))))
  ;;
  (import (only (ikarus system $vectors)
		$vector-set!
		$vector-ref))
  (define who 'optimize-letrec/waddell)

  (define (optimize-letrec/waddell x)
    (E x (lambda (x)
	   (error who "free var found" x))
       void))

  (module (E)

    (define-constant SIMPLE-PRIMITIVES '())

    (define (E x ref comp)
      ;;Recursively visit the recordized code X.
      ;;
      ;;REF is a  function of single argument to be  applied to a struct
      ;;instance of type PRELEX, if  such instance represents a variable
      ;;that is referenced in X.
      ;;
      ;;COMP is a thunk to be called if X is a "complex" expression.  An
      ;;expression  is complex  if we  cannot establish  for sure  which
      ;;bindings of  an enclosing  LETREC or  LETREC* are  referenced or
      ;;assigned.
      ;;
      (struct-case x
	((constant)
	 x)

	((prelex)
	 (ref x)
	 x)

	((assign lhs rhs)
	 ;;An assignment is  a reference to the binding LHS  and also it
	 ;;makes X a "complex" expression.
	 (ref lhs)
	 (comp)
	 (make-assign lhs (E rhs ref comp)))

	((primref)
	 x)

	((bind lhs* rhs* body)
	 ;;Do RHS* first, then BODY.
	 (let* ((rhs*^ (E* rhs* ref comp))
		(body^ (E body (%extend-hash lhs* (make-eq-hashtable) ref) comp)))
	   (make-bind lhs* rhs*^ body^)))

	((recbind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body ref comp)
	   (%do-recbind lhs* rhs* body ref comp #t)))

	((rec*bind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body ref comp)
	   (%do-recbind lhs* rhs* body ref comp #f)))

	((conditional test conseq altern)
	 (make-conditional (E test ref comp)
	     (E conseq ref comp)
	   (E altern ref comp)))

	((seq e0 e1)
	 (make-seq (E e0 ref comp) (E e1 ref comp)))

	((clambda)
	 (E-clambda x ref comp))

	((funcall rator rand*)
	 (let ((rator (E  rator ref comp))
	       (rand* (E* rand* ref comp)))
	   ;;This form  is a function  call.  We assume it  might mutate
	   ;;any of  the bindings whose  region includes it: so  we call
	   ;;COMP.
	   (struct-case rator
	     ((primref op)
	      (unless (memq op SIMPLE-PRIMITIVES)
		(comp)))
	     (else
	      (comp)))
	   (make-funcall rator rand*)))

	((mvcall p c)
	 (let ((p (E p ref comp))
	       (c (E c ref comp)))
	   ;;This form  is a function  call.  We assume it  might mutate
	   ;;any of  the bindings whose  region includes it: so  we call
	   ;;COMP.
	   (comp)
	   (make-mvcall p c)))

	((forcall rator rand*)
	 (make-forcall rator (E* rand* ref comp)))

	(else
	 (error who "invalid expression" (unparse-recordized-code x)))))

    (define (E* x* ref comp)
      (if (null? x*)
	  '()
	(cons (E  ($car x*) ref comp)
	      (E* ($cdr x*) ref comp))))

    (define (E-clambda x ref comp)
      (struct-case x
	((clambda label cls* cp free name)
	 (make-clambda label (map (lambda (cls)
				    (E-clambda-case cls ref))
			       cls*)
		       cp free name))))

    (define (E-clambda-case x ref)
      (struct-case x
	((clambda-case info body)
	 (let ((h (make-eq-hashtable)))
	   (let ((body^ (E body
			   (%extend-hash (case-info-args info) h ref)
			   void)))
	     (make-clambda-case info body^))))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (define (%extend-hash lhs* table ref)
    (for-each (lambda (lhs)
		(hashtable-set! table lhs #t))
      lhs*)
    (lambda (x)
      (unless (hashtable-ref table x #f)
	(hashtable-set! table x #t)
	(ref x))))

  (module (%do-recbind)

    (define (%do-recbind lhs* rhs* body ref comp letrec?)
      ;;If this  form is  a LETREC, we  do not care  about the  order of
      ;;evaluation of the RHS*; we produce recordized code like this:
      ;;
      ;;   (let ((?slhs ?shrs) ...)		;simple
      ;;     (let ((?clhs (void)) ...)		;complex
      ;;       (fix ((?llhs ?lrhs) ...)		;lambda
      ;;         (let ((?tmp ?crhs) ...)
      ;;           (set! ?crhs ?tmp)
      ;;           ...
      ;;           ?body))))
      ;;
      ;;If  this form  is  a LETREC*,  we  do care  about  the order  of
      ;;evaluation of the RHS*;
      ;;
      ;;   (let ((?slhs ?shrs) ...)		;simple
      ;;     (let ((?clhs (void)) ...)		;complex
      ;;       (fix ((?llhs ?lrhs) ...)		;lambda
      ;;         (set! ?clhs ?crhs)
      ;;         ...
      ;;         ?body)))
      ;;
      ;;LETREC? is true if the form we are processing is a LETREC; it is
      ;;false if the form is a LETREC*.
      ;;
      (let ((h     (make-eq-hashtable))
	    (vref  (make-vector (length lhs*) #f))
	    (vcomp (make-vector (length lhs*) #f)))
	;;VREF  is a  vector of  booleans, one  for each  binding.  Such
	;;booleans  will be  set to  true  if the  corresponding LHS  is
	;;referenced or mutated.
	;;
	;;VCOMP is  a vector  of booleans, one  for each  binding.  Such
	;;booleans will be  set to #t if the corresponding  RHS may have
	;;assigned an LHS (we cannot be sure neither if it actually does
	;;it, nor of which bindings are mutated).
	;;
	(let* ((ref^  (%extend-hash lhs* h ref))
	       (body^ (E body ref^ comp))
	       (rhs*  (%do-rhs* 0 lhs* rhs* ref^ comp vref vcomp)))
	  (let-values (((slhs* srhs*	    ;simple bindings
			       llhs* lrhs*  ;lambda bindings
			       clhs* crhs*) ;complex bindings
			(%partition-rhs* 0 lhs* rhs* vref vcomp)))
	    (let ((void* (map (lambda (x)
				(make-constant (void)))
			   clhs*)))
	      (make-bind slhs* srhs*
		(make-bind clhs* void*
		  (make-fix llhs* lrhs*
		    (if letrec?
			(let ((tmp* (map unique-prelex clhs*)))
			  (make-bind tmp* crhs*
			    (build-assign* clhs* tmp* body^)))
		      (build-assign* clhs* crhs* body^))))))))))

    (define (%do-rhs* i lhs* rhs* ref comp vref vcomp)
      ;;Recursively process RHS*  and return a list  of struct instances
      ;;which is meant to replace the original RHS*.
      ;;
      ;;This function has two purposes:
      ;;
      ;;1. Apply E to each struct in RHS*.
      ;;
      ;;2. Fill appropriately the vectors VREF and VCOMP.
      ;;
      ;;Given recordized code representing:
      ;;
      ;;   (letrec ((?lhs-0 ?rhs-0)
      ;;            (?lhs-1 ?rhs-1)
      ;;            (?lhs-2 ?rhs-3))
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
	(let ((H    (make-eq-hashtable))
	      (rest (%do-rhs* (fxadd1 i) lhs* ($cdr rhs*) ref comp vref vcomp)))
	  (define (ref^ x)
	    ;;Called to signal that a form in RHS has accessed a binding
	    ;;among LHS*.
	    ;;
	    (unless (hashtable-ref H x #f)
	      (hashtable-set! H x #t)
	      (ref x)
	      (when (memq x lhs*)
		($vector-set! vref i #t))))
	  (define (comp^)
	    ;;Called to signal that a form in RHS might mutate a binding
	    ;;among LHS*.
	    ;;
	    ($vector-set! vcomp i #t)
	    (comp))
	  (cons (E ($car rhs*) ref^ comp^)
		rest))))

    (define (%partition-rhs* i lhs* rhs* vref vcomp)
      ;;Make use of the data in  the vectors VREF and VCOMP to partition
      ;;the  bindings  into:  simple,  lambda, complex.   (RHS*  is  not
      ;;visited here.)
      ;;
      ;;Return 6 values:
      ;;
      ;;SLHS*, SRHS*
      ;;   Simple bindings.  SLHS is never  assigned in all the RHS* and
      ;;    the  associated  SRHS  is  a  simple  expression:  it  never
      ;;   references SLHS and it does not call any function.
      ;;
      ;;LLHS*, LRHS*
      ;;   Lambda bindings.  Lists of LHS and RHS whose RHS is a CLAMBDA
      ;;   and  whose LHS  is never  assigned in both  the RHS*  and the
      ;;   body.
      ;;
      ;;CLHS*, CRHS*
      ;;   Complex bindings.   Lists of LHS and RHS for  which either we
      ;;   know that the LHS has been  assigned, or we know that the RHS
      ;;   may have assigned an LHS.
      ;;
      (if (null? lhs*)
	  (values '() '() '() '() '() '())
	(let-values
	    (((slhs* srhs* llhs* lrhs* clhs* crhs*)
	      (%partition-rhs* (fxadd1 i) ($cdr lhs*) ($cdr rhs*) vref vcomp))
	     ((lhs rhs)
	      (values ($car lhs*) ($car rhs*))))
	  (cond ((prelex-source-assigned? lhs) ;complex
		 (values slhs* srhs*
			 llhs* lrhs*
			 (cons lhs clhs*) (cons rhs crhs*)))
		((clambda? rhs)	;lambda
		 (values slhs* srhs*
			 (cons lhs llhs*) (cons rhs lrhs*)
			 clhs* crhs*))
		((or ($vector-ref vref  i) ;complex
		     ($vector-ref vcomp i))
		 (values slhs* srhs*
			 llhs* lrhs*
			 (cons lhs clhs*) (cons rhs crhs*)))
		(else ;simple
		 (values (cons lhs slhs*) (cons rhs srhs*)
			 llhs* lrhs*
			 clhs* crhs*))
		))))

    #| end of module: %do-recbind |# )

  #| end of module: optimize-letrec/waddell |# )


(module (optimize-letrec/scc)
  ;;Perform transformations to convert  the recordized representation of
  ;;LETREC  and LETREC*  forms  into LET  forms  and assignments.   This
  ;;function does what is described in the [GD] paper.
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
  (define who 'optimize-letrec/scc)

  (define-struct binding
    (serial lhs rhs complex prev free*))

  (define (optimize-letrec/scc x)
    (let ((x (E x (make-binding #f #f #f #t #t '()))))
      ;;(pretty-print (unparse-recordized-code x))
      x))

;;; --------------------------------------------------------------------

  (module (gen-letrecs)

    (define (gen-letrecs scc* ordered? body)
      (let-values (((fix* body)
		    (let recur ((scc* scc*))
		      (if (null? scc*)
			  (values '() body)
			(let-values (((fix* body) (recur (cdr scc*))))
			  (gen-letrec (car scc*) fix* body ordered?))))))
	(mkfix fix* body)))

    (define (mkfix b* body)
      (if (null? b*)
	  body
	(make-fix (map binding-lhs b*)
	    (map binding-rhs b*)
	  body)))

    (module (gen-letrec)

      (define (gen-letrec scc fix* body ordered?)
	(cond ((null? (cdr scc))
	       (let ((b (car scc)))
		 (cond ((lambda-binding? b)
			(values (cons b fix*) body))
		       ((not (memq b (binding-free* b)))
			(values '() (mklet (list (binding-lhs b))
					   (list (binding-rhs b))
					   (mkfix fix* body))))
		       (else
			(values '() (mklet (list (binding-lhs b))
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
	(and (not (prelex-source-assigned? (binding-lhs x)))
	     (clambda? (binding-rhs x))))

      (define (mklet lhs* rhs* body)
	(if (null? lhs*)
	    body
	  (make-bind lhs* rhs* body)))

      (define (mkset!s b* body)
	;;Recursive function.
	;;
	(if (null? b*)
	    body
	  (let* ((b   (car b*))
		 (lhs (binding-lhs b)))
	    (unless (prelex-source-assigned? lhs)
	      (when (debug-scc)
		(printf "MADE COMPLEX ~s\n" (unparse-recordized-code lhs)))
	      (set-prelex-source-assigned?! lhs (or (prelex-global-location lhs) #t)))
	    (make-seq (make-assign lhs (binding-rhs b))
		      (mkset!s (cdr b*) body)))))

      (define (sort-bindings ls)
	(list-sort (lambda (x y)
		     (< (binding-serial x)
			(binding-serial y)))
		   ls))

      #| end of module: gen-letrec |# )

    #| end of module: gen-letrecs |# )

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
	   (mark-complex bc))
	 x)

	((assign lhs rhs)
	 (assert (prelex-source-assigned? lhs))
	 ;;(set-prelex-source-assigned?! lhs #t)
	 (mark-free lhs bc)
	 (mark-complex bc)
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
	   (do-recbind lhs* rhs* body bc #f)))

	((rec*bind lhs* rhs* body)
	 (if (null? lhs*)
	     (E body bc)
	   (do-recbind lhs* rhs* body bc #t)))

	((conditional test conseq altern)
	 (make-conditional (E test bc) (E conseq bc) (E altern bc)))

	((seq e0 e1)
	 (make-seq (E e0 bc) (E e1 bc)))

	((clambda)
	 (E-clambda x bc))

	((funcall rator rand*)
	 (mark-complex bc)
	 (make-funcall (E rator bc) (E* rand* bc)))

	((mvcall p c)
	 (mark-complex bc)
	 (make-mvcall (E p bc) (E c bc)))

	((forcall rator rand*)
	 (mark-complex bc)
	 (make-forcall rator (E* rand* bc)))

	(else
	 (error who "invalid expression" (unparse-recordized-code x)))))

    (define (E* x* bc)
      (map (lambda (x)
	     (E x bc))
	x*))

    (define (E-clambda x bc)
      (struct-case x
	((clambda label cls* cp free name)
	 (let ((bc (make-binding #f #f #f #t bc '())))
	   (make-clambda label (map (lambda (x)
				      (struct-case x
					((clambda-case info body)
					 (make-clambda-case info (E body bc)))))
				 cls*)
			 cp free name)))))

    (define (mark-complex bc)
      (unless (binding-complex bc)
	(set-binding-complex! bc #t)
	(mark-complex (binding-prev bc))))

    (define (mark-free var bc)
      (let ((rb (prelex-operand var)))
	(when rb
	  (let* ((lb    (let ((pr (binding-prev rb)))
			  (let loop ((bc bc))
			    (let ((bcp (binding-prev bc)))
			      (if (eq? bcp pr)
				  bc
				(loop bcp))))))
		 (free* (binding-free* lb)))
	    (unless (memq rb free*)
	      (set-binding-free*! lb (cons rb free*)))))))

    #| end of module: E |# )

;;; --------------------------------------------------------------------

  (module (do-recbind)

    (define (do-recbind lhs* rhs* body bc ordered?)
      (let ((b* (%make-bindings lhs* rhs* bc 0)))
	(for-each (lambda (b)
		    (set-binding-rhs! b (E (binding-rhs b) b)))
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
	(let ((b (make-binding i (car lhs*) (car rhs*) #f bc '())))
	  (set-prelex-operand! (car lhs*) b)
	  (cons b (%make-bindings (cdr lhs*) (cdr rhs*) bc (+ i 1))))))

    (define (complex? x)
      (or (binding-complex x)
	  (prelex-source-assigned? (binding-lhs x))))

    (module (insert-order-edges)

      (define (insert-order-edges b*)
	(unless (null? b*)
	  (let ((b (car b*)))
	    (if (complex? b)
		(mark b (cdr b*))
	      (insert-order-edges (cdr b*))))))

      (define (mark pb b*)
	(unless (null? b*)
	  (let ((b (car b*)))
	    (if (complex? b)
		(let ((free* (binding-free* b)))
		  (unless (memq pb free*)
		    (set-binding-free*! b (cons pb free*)))
		  (mark b (cdr b*)))
	      (mark pb (cdr b*))))))

      #| end of module: insert-order-edges |# )

    #| end of module: do-recbind |# )

;;; --------------------------------------------------------------------


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
		    (let ((node (make-node (car data*) '() #f #f #f #f)))
		      (hashtable-set! h (car v*) node)
		      (cons node (f (cdr v*) (cdr data*))))))))
	(for-each (lambda (v e*)
		    (set-node-link*! v (map (lambda (f)
					      (or (hashtable-ref h f #f)
						  (error who "invalid node" f)))
					 e*)))
	  v* e**)
	v*))

    (define (compute-sccs v*)
      ;;Tarjan's algorithm.
      (define scc* '())
      (define (compute-sccs v)
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
	      (set! scc* (cons (let f ((ls stack))
				 (let ((v^ (car ls)))
				   (set-node-done! v^ #t)
				   (cons v^ (if (eq? v^ v)
						(begin (set! stack (cdr ls)) '())
					      (f (cdr ls))))))
			       scc*)))))
	(tarjan v))
      (for-each (lambda (v)
		  (unless (node-done v)
		    (compute-sccs v)))
	v*)
      (reverse scc*))

    #| end of module: get-sccs-in-order |# )

;;; --------------------------------------------------------------------

  #| end of module: optimize-letrec/scc |# )


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; eval: (put 'make-bind 'scheme-indent-function 2)
;; eval: (put 'make-fix 'scheme-indent-function 2)
;; End:
