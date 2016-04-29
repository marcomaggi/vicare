;;;Copyright (c) 2010-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(module (chi-lambda/std
	 chi-lambda/typed
	 chi-lambda/checked
	 chi-named-lambda/std
	 chi-named-lambda/typed
	 chi-named-lambda/checked
	 ;;
	 chi-case-lambda/std
	 chi-case-lambda/typed
	 chi-case-lambda/checked
	 chi-named-case-lambda/std
	 chi-named-case-lambda/typed
	 chi-named-case-lambda/checked
	 ;;
	 chi-defun/std
	 chi-defun/typed
	 chi-defun/checked
	 chi-case-defun/std
	 chi-case-defun/typed
	 chi-case-defun/checked)


;;;; helpers

(define (%sublists-of-same-length ell*)
  ;;Recursive function.  ELL*  must be a list  of items; each item being  a proper or
  ;;improper list.  This function searches for  items having the same length; if some
  ;;are found  they are returned  in a list;  if none are  found the return  value is
  ;;false.
  ;;
  (and (pair? ell*)
       (let ((item (car ell*)))
	 (let ((result (cond ((list? item)
			      ;;Proper list item.  Search  for proper list items with
			      ;;the same length in the rest of ELL*.
			      (let ((item.len (length item)))
				(filter (lambda (thing)
					  (and (list? thing)
					       (= item.len (length thing))))
				  (cdr ell*))))
			     ((pair? item)
			      ;;Improper list  item.  Search for improper  list items
			      ;;with the same length in the rest of ELL*.
			      (let ((item.len (length+ item)))
				(filter (lambda (thing)
					  (and (list? thing)
					       (= item.len (length+ thing))))
				  (cdr ell*))))
			     ((null? item)
			      ;;Null  item.  Search  for null  items in  the rest  of
			      ;;ELL*.
			      (filter null? (cdr ell*)))
			     (else
			      ;;Standalone item.  Search for  standalone items in the
			      ;;rest of ELL*.
			      (filter (lambda (thing)
					(not (or (pair? thing)
						 (null? thing))))
				(cdr ell*))))))
	   (if (null? result)
	       (%sublists-of-same-length (cdr ell*))
	     result)))))

(define (length+ x)
  ;;Return the length of an improper list.  This is from SRFI 1.
  ;;
  ;;Return #f if X is circular.
  ;;
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x   (cdr x))
	      (len (+ len 1)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag)) (lp x lag len)))
	    len))
      len)))


;;;; chi procedures: standard and typed single-clause function definition

(module (chi-defun/std chi-defun/typed chi-defun/checked)

  (define (chi-defun/std qdef lexenv.run lexenv.expand)
    ;;Expand a qualified RHS (QDEF) representing a DEFINE/STD syntax use for the
    ;;case of function definition; the original input form is something like:
    ;;
    ;;   (define/std (?lhs . ?formals) . ?body)
    ;;
    ;;Return a  PSI object holding a  lambda core language expression.   The returned
    ;;expression will be coupled (by the caller) with an already generated lex gensym
    ;;serving as lexical variable name; for this  reason we return a lambda core form
    ;;rather than a define core form.
    ;;
    ;;NOTE This  function assumes that:  the left-hand side (LHS)  variable syntactic
    ;;binding has already been added to LEXENV.
    ;;
    (%chi-defun 'standard qdef lexenv.run lexenv.expand))

  (define (chi-defun/typed qdef lexenv.run lexenv.expand)
    ;;Expand a  qualified RHS (QDEF) representing  a DEFINE/TYPED syntax use  for the
    ;;case of function definition; the original input form is something like:
    ;;
    ;;   (define/typed (?lhs . ?formals) . ?body)
    ;;   (define/typed ((brace ?lhs ?rv-type ... . ?rv-type-rest) . ?formals) . ?body)
    ;;
    ;;Return a  PSI object holding a  lambda core language expression.   The returned
    ;;expression will be coupled (by the caller) with an already generated lex gensym
    ;;serving as lexical variable name; for this  reason we return a lambda core form
    ;;rather than a define core form.
    ;;
    ;;NOTE This  function assumes that:  the left-hand side (LHS)  variable syntactic
    ;;binding has already been added to LEXENV.
    ;;
    (%chi-defun 'typed qdef lexenv.run lexenv.expand))

  (define (chi-defun/checked qdef lexenv.run lexenv.expand)
    ;;Expand a qualified RHS (QDEF) representing  a DEFINE/CHECKED syntax use for the
    ;;case of function definition; the original input form is something like:
    ;;
    ;;   (define/checked (?lhs . ?formals) . ?body)
    ;;   (define/checked ((brace ?lhs ?rv-type ... . ?rv-type-rest) . ?formals) . ?body)
    ;;
    ;;Return a  PSI object holding a  lambda core language expression.   The returned
    ;;expression will be coupled (by the caller) with an already generated lex gensym
    ;;serving as lexical variable name; for this  reason we return a lambda core form
    ;;rather than a define core form.
    ;;
    ;;NOTE This  function assumes that:  the left-hand side (LHS)  variable syntactic
    ;;binding has already been added to LEXENV.
    ;;
    (%chi-defun 'checked qdef lexenv.run lexenv.expand))

  (define* (%chi-defun type qdef lexenv.run lexenv.expand)
    (define-constant input-form.stx (qdef.input-form qdef))
    (parametrise ((current-run-lexenv (lambda () lexenv.run)))
      (receive (standard-formals.lex body.psi)
	  (case type
	    ((standard)
	     (chi-lambda-clause/std input-form.stx lexenv.run lexenv.expand
				    (qdef-defun.standard-formals qdef)
				    (car (qdef-closure.clause-signature* qdef))
				    (qdef-defun.body* qdef)))
	    ((typed)
	     (receive (lexenv.run lexenv.expand)
		 ;;We establish the syntactic binding for "__who__" before processing
		 ;;the body.  So the formals may shadow this binding.
		 (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand __who__ (qdef.var-id qdef))
	       (chi-lambda-clause/typed input-form.stx lexenv.run lexenv.expand
					(qdef-defun.standard-formals qdef)
					(car (qdef-closure.clause-signature* qdef))
					(qdef-defun.body* qdef))))
	    ((checked)
	     (receive (lexenv.run lexenv.expand)
		 ;;We establish the syntactic binding for "__who__" before processing
		 ;;the body.  So the formals may shadow this binding.
		 (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand __who__ (qdef.var-id qdef))
	       (chi-lambda-clause/checked input-form.stx lexenv.run lexenv.expand
					  (qdef-defun.standard-formals qdef)
					  (car (qdef-closure.clause-signature* qdef))
					  (qdef-defun.body* qdef)))))
	(make-psi input-form.stx
	  (build-lambda (identifier->symbol (qdef.var-id qdef))
	      standard-formals.lex
	    (psi.core-expr body.psi))
	  (make-type-signature/single-value (qdef-closure.ots qdef))))))

  #| end of module |# )


;;;; chi procedures: standard and typed multi-clause function definition

(module (chi-case-defun/std chi-case-defun/typed chi-case-defun/checked)

  (define (chi-case-defun/std qdef lexenv.run lexenv.expand)
    ;;Expand a  qualified RHS (QDEF)  representing a CASE-DEFINE/STD  syntax use
    ;;for the case of function definition; the original input form is something like:
    ;;
    ;;   (case-define/std ?lhs (?formals0 . ?body0) (?formals . ?body) ...)
    ;;
    ;;Return a  PSI object holding a  lambda core language expression.   The returned
    ;;expression will be coupled (by the caller) with an already generated lex gensym
    ;;serving as lexical variable name; for this  reason we return a lambda core form
    ;;rather than a define core form.
    ;;
    ;;NOTE This  function assumes that:  the left-hand side (LHS)  variable syntactic
    ;;binding has already been added to LEXENV.
    ;;
    (%chi-case-defun 'standard qdef lexenv.run lexenv.expand))

  (define (chi-case-defun/typed qdef lexenv.run lexenv.expand)
    ;;Expand a qualified  RHS (QDEF) representing a CASE-DEFINE/TYPED  syntax use for
    ;;the case of function definition; the original input form is something like:
    ;;
    ;;   (case-define/typed ?lhs (?formals0 . ?body0) (?formals . ?body) ...)
    ;;
    ;;Return a  PSI object holding a  lambda core language expression.   The returned
    ;;expression will be coupled (by the caller) with an already generated lex gensym
    ;;serving as lexical variable name; for this  reason we return a lambda core form
    ;;rather than a define core form.
    ;;
    ;;NOTE This  function assumes that:  the left-hand side (LHS)  variable syntactic
    ;;binding has already been added to LEXENV.
    ;;
    (%chi-case-defun 'typed qdef lexenv.run lexenv.expand))

  (define (chi-case-defun/checked qdef lexenv.run lexenv.expand)
    ;;Expand a qualified RHS (QDEF) representing a CASE-DEFINE/CHECKED syntax use for
    ;;the case of function definition; the original input form is something like:
    ;;
    ;;   (case-define/checked ?lhs (?formals0 . ?body0) (?formals . ?body) ...)
    ;;
    ;;Return a  PSI object holding a  lambda core language expression.   The returned
    ;;expression will be coupled (by the caller) with an already generated lex gensym
    ;;serving as lexical variable name; for this  reason we return a lambda core form
    ;;rather than a define core form.
    ;;
    ;;NOTE This  function assumes that:  the left-hand side (LHS)  variable syntactic
    ;;binding has already been added to LEXENV.
    ;;
    (%chi-case-defun 'checked qdef lexenv.run lexenv.expand))

  (define* (%chi-case-defun type qdef lexenv.run lexenv.expand)
    (define-constant input-form.stx		(qdef.input-form qdef))
    (define-constant standard-formals*.stx	(qdef-case-defun.standard-formals* qdef))
    (define-constant body**.stx			(qdef-case-defun.body** qdef))
    (define-constant clause-signature*		(qdef-closure.clause-signature* qdef))
    ;;We  do the  validation on  the standard  formals, so  that the  "_" element  is
    ;;excluded.
    (cond ((%sublists-of-same-length standard-formals*.stx)
	   => (lambda (same-length-formals*.stx)
		(syntax-violation #f
		  "invalid CASE-LAMBDA clause formals with the same length"
		  input-form.stx same-length-formals*.stx))))
    (receive (lexenv.run lexenv.expand)
	;;We  establish the  syntactic binding  for "__who__"  before processing  the
	;;body.  So the formals may shadow this binding.
	(fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand __who__ (qdef.var-id qdef))
      (parametrise ((current-run-lexenv (lambda () lexenv.run)))
	(receive (formals*.lex body*.psi)
	    (case type
	      ((standard)
	       (chi-case-lambda-clause*/std     input-form.stx lexenv.run lexenv.expand
						standard-formals*.stx clause-signature* body**.stx))
	      ((typed)
	       (chi-case-lambda-clause*/typed   input-form.stx lexenv.run lexenv.expand
						standard-formals*.stx clause-signature* body**.stx))
	      ((checked)
	       (chi-case-lambda-clause*/checked input-form.stx lexenv.run lexenv.expand
						standard-formals*.stx clause-signature* body**.stx)))
	  (make-psi input-form.stx
	    (build-case-lambda (syntax-annotation input-form.stx)
		formals*.lex
	      (map psi.core-expr body*.psi))
	    (make-type-signature/single-value (qdef-closure.ots qdef)))))))

  #| end of module |# )


;;;; standard LAMBDA expansion and variants

(define* (chi-lambda/std input-form.stx lexenv.run lexenv.expand
			 input-formals.stx body*.stx)
  ;;Expand the contents of a LAMBDA/STD syntax use and return a PSI object.
  ;;
  ;;The argument INPUT-FORM.STX  is a syntax object representing  the original LAMBDA
  ;;expression.  The argument  INPUT-FORMALS.STX is a syntax  object representing the
  ;;formals of the LAMBDA syntax.  The argument BODY*.STX is a list of syntax objects
  ;;representing the body expressions in the LAMBDA syntax.
  ;;
  (receive (standard-formals.stx clause-signature)
      (syntax-object.parse-standard-clambda-clause-formals input-formals.stx)
    ;;CLAUSE-SIGNATURE is an instance of "<clambda-clause-signature>".
    (receive (standard-formals.lex body.psi)
	(chi-lambda-clause/std input-form.stx lexenv.run lexenv.expand
			       standard-formals.stx clause-signature body*.stx)
      ;;STANDARD-FORMALS.LEX is a  proper or improper list of  lex gensyms representing
      ;;the lambda clause formals.
      (make-psi input-form.stx
	(build-lambda (syntax-annotation input-form.stx)
	    standard-formals.lex
	  (psi.core-expr body.psi))
	(make-type-signature/single-value
	 (make-closure-type-spec (make-clambda-signature (list clause-signature))))))))

(define* (chi-named-lambda/std input-form.stx lexenv.run lexenv.expand
			       who.id standard-formals.stx body*.stx)
  ;;Expand  the contents  of  a NAMED-LAMBDA/STD  syntax use  and  return a  PSI
  ;;object.
  ;;
  (receive (lexenv.run lexenv.expand)
      ;;We  establish  the syntactic  binding  for  "__who__" before  processing  the
      ;;formals and the body.  So the formals may shadow this binding.
      (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand __who__ who.id)
    (chi-lambda/std input-form.stx lexenv.run lexenv.expand
		    standard-formals.stx body*.stx)))


;;;; standard CASE-LAMBDA expansion and variants

(module (chi-case-lambda/std
	 chi-named-case-lambda/std)

  (define* (chi-case-lambda/std input-form.stx lexenv.run lexenv.expand
				input-formals*.stx body**.stx)
    ;;Expand  the contents  of a  CASE-LAMBDA/STD syntax  use and  return a  psi
    ;;object.
    ;;
    ;;The  argument  INPUT-FORM.STX is  a  syntax  object representing  the  original
    ;;CASE-LAMBDA/STD expression.  The argument  INPUT-FORMALS*.STX is a list of
    ;;syntax objects  whose items are  the formals  of the CASE-LAMBDA  clauses.  The
    ;;argument BODY**.STX is a  list of syntax objects whose items  are the bodies of
    ;;the CASE-LAMBDA clauses.
    ;;
    ;;Example, for the input form:
    ;;
    ;;   (case-lambda/std
    ;;     ((a b c) body1)
    ;;     ((d e f) body2))
    ;;
    ;;this function is invoked as:
    ;;
    ;;   (chi-case-lambda/std
    ;;      #'(case-lambda/std
    ;;          ((a b c) body1)
    ;;          ((d e f) body2))
    ;;      lexenv.run lexenv.expand
    ;;      (list #'(a b c) #'(d e f))
    ;;      (list #'(body1) #'(body2)))
    ;;
    (%chi-clambda input-form.stx lexenv.run lexenv.expand input-formals*.stx body**.stx))

  (define* (chi-named-case-lambda/std input-form.stx lexenv.run lexenv.expand
				      who.id input-formals*.stx body**.stx)
    ;;Expand the contents of a NAMED-CASE-LAMBDA/STD syntax use and return a psi
    ;;object.
    ;;
    (receive (lexenv.run lexenv.expand)
	;;We  establish the  syntactic binding  for "__who__"  before processing  the
	;;formals and the body.  So the formals may shadow this binding.
	(fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand __who__ who.id)
      (%chi-clambda input-form.stx lexenv.run lexenv.expand
		    input-formals*.stx body**.stx)))

  (define (%chi-clambda input-form.stx lexenv.run lexenv.expand input-formals*.stx body**.stx)
    (receive (standard-formals*.stx clause-signature*)
	(syntax-object.parse-standard-clambda-multi-clauses-formals input-formals*.stx)
      ;;We do  the validation  on the standard  formals, so that  the "_"  element is
      ;;excluded.
      (cond ((%sublists-of-same-length standard-formals*.stx)
	     => (lambda (same-length-formals*.stx)
		  (syntax-violation #f
		    "invalid CASE-LAMBDA clause formals with the same length"
		    input-form.stx same-length-formals*.stx))))
      (receive (formals*.lex body*.psi)
	  (chi-case-lambda-clause*/std input-form.stx lexenv.run lexenv.expand
				       standard-formals*.stx clause-signature* body**.stx)
	(make-psi input-form.stx
	  (build-case-lambda (syntax-annotation input-form.stx)
	      formals*.lex
	    (map psi.core-expr body*.psi))
	  (make-type-signature/single-value
	   (make-closure-type-spec (make-clambda-signature clause-signature*)))))))

  #| end of module |# )


;;;; typed LAMBDA expansion

(module (chi-lambda/typed chi-lambda/checked)

  (define* (chi-lambda/typed input-form.stx lexenv.run lexenv.expand
			     input-formals.stx body*.stx)
    ;;Expand the contents of a LAMBDA/TYPED syntax use and return a psi object.
    ;;
    ;;The argument INPUT-FORM.STX is a syntax object representing the original LAMBDA
    ;;expression.  The argument INPUT-FORMALS.STX is a syntax object representing the
    ;;formals  of the  LAMBDA syntax.   The argument  BODY*.STX is  a list  of syntax
    ;;objects representing the body expressions.
    ;;
    (%chi-lambda input-form.stx lexenv.run lexenv.expand 'typed   input-formals.stx body*.stx))

  (define* (chi-lambda/checked input-form.stx lexenv.run lexenv.expand
			       input-formals.stx body*.stx)
    ;;Expand the contents of a LAMBDA/CHECKED syntax use and return a psi object.
    ;;
    ;;The argument INPUT-FORM.STX is a syntax object representing the original LAMBDA
    ;;expression.  The argument INPUT-FORMALS.STX is a syntax object representing the
    ;;formals  of the  LAMBDA syntax.   The argument  BODY*.STX is  a list  of syntax
    ;;objects representing the body expressions.
    ;;
    (%chi-lambda input-form.stx lexenv.run lexenv.expand 'checked input-formals.stx body*.stx))

;;; --------------------------------------------------------------------

  (define (%chi-lambda input-form.stx lexenv.run lexenv.expand type input-formals.stx body*.stx)
    (receive (standard-formals.stx clause-signature)
	;;STANDARD-FORMALS.STX is  a syntax object representing  the formal arguments
	;;of the lambda clause as required  by R6RS.  CLAUSE-SIGNATURE is an instance
	;;of  "<clambda-clause-signature>"  representing  the types  of  formals  and
	;;retvals.
	(syntax-object.parse-typed-clambda-clause-formals input-formals.stx)
      (receive (standard-formals.lex body.psi)
	  (case type
	    ((typed)
	     (chi-lambda-clause/typed   input-form.stx lexenv.run lexenv.expand
					standard-formals.stx clause-signature body*.stx))
	    ((checked)
	     (chi-lambda-clause/checked input-form.stx lexenv.run lexenv.expand
					standard-formals.stx clause-signature body*.stx)))
	(make-psi input-form.stx
	  (build-lambda (syntax-annotation input-form.stx)
	      standard-formals.lex
	    (psi.core-expr body.psi))
	  (make-type-signature/single-value
	   (make-closure-type-spec (make-clambda-signature (list clause-signature))))))))

  #| end of module |# )


;;;; named and typed LAMBDA expansion

(module (chi-named-lambda/typed chi-named-lambda/checked)

  (define* (chi-named-lambda/typed input-form.stx lexenv.run lexenv.expand
				   who.id input-formals.stx body*.stx)
    ;;Expand the contents of a NAMED-LAMBDA/TYPED syntax use and return a psi object.
    ;;
    (%chi-lambda input-form.stx lexenv.run lexenv.expand
		 'typed   who.id input-formals.stx body*.stx))

  (define* (chi-named-lambda/checked input-form.stx lexenv.run lexenv.expand
				     who.id input-formals.stx body*.stx)
    ;;Expand the contents of a NAMED-LAMBDA/CHECKED syntax use and return a psi object.
    ;;
    (%chi-lambda input-form.stx lexenv.run lexenv.expand
		 'checked who.id input-formals.stx body*.stx))

;;; --------------------------------------------------------------------

  (define* (%chi-lambda input-form.stx lexenv.run lexenv.expand type who.id input-formals.stx body*.stx)
    (receive (standard-formals.stx clause-signature)
	;;STANDARD-FORMALS.STX is  a syntax object representing  the formal arguments
	;;of the lambda clause as required  by R6RS.  CLAUSE-SIGNATURE is an instance
	;;of  "<clambda-clause-signature>"  representing  the types  of  formals  and
	;;retvals.
	(syntax-object.parse-typed-clambda-clause-formals input-formals.stx)
      (receive (lexenv.run lexenv.expand)
	  ;;We establish  the syntactic binding  for "__who__" before  processing the
	  ;;formals and the body.  So the formals may shadow this binding.
	  (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand __who__ who.id)
	(receive (standard-formals.lex body.psi)
	    (case type
	      ((typed)
	       (chi-lambda-clause/typed   input-form.stx lexenv.run lexenv.expand
					  standard-formals.stx clause-signature body*.stx))
	      ((checked)
	       (chi-lambda-clause/checked input-form.stx lexenv.run lexenv.expand
					  standard-formals.stx clause-signature body*.stx)))
	  (make-psi input-form.stx
	    (build-lambda (syntax-annotation input-form.stx)
		standard-formals.lex
	      (psi.core-expr body.psi))
	    (make-type-signature/single-value
	     (make-closure-type-spec (make-clambda-signature (list clause-signature)))))))))

  #| end of module |# )


;;;; typed CASE-LAMBDA expansion

(module (chi-case-lambda/typed chi-case-lambda/checked)

  (define* (chi-case-lambda/typed input-form.stx lexenv.run lexenv.expand
				  input-formals*.stx body**.stx)
    ;;Expand the clauses of a CASE-LAMBDA/TYPED syntax use and return a psi object.
    ;;
    ;;The  argument  INPUT-FORM.STX is  a  syntax  object representing  the  original
    ;;CASE-LAMBDA/TYPED  expression.  The  argument INPUT-FORMALS*.STX  is a  list of
    ;;syntax objects  whose items are  the formals of the  CASE-LAMBDA/TYPED clauses.
    ;;The argument BODY**.STX is a list of  syntax objects whose items are the bodies
    ;;of the CASE-LAMBDA/TYPED clauses.
    ;;
    ;;Example, for the input form:
    ;;
    ;;   (case-lambda/typed
    ;;     ((a b c) body1)
    ;;     ((d e f) body2))
    ;;
    ;;this function is invoked as:
    ;;
    ;;   (chi-case-lambda/typed
    ;;     #'(case-lambda/typed
    ;;         ((a b c) body1)
    ;;         ((d e f) body2))
    ;;     lexenv.run lexenv.expand
    ;;     (list #'(a b c) #'(d e f))
    ;;     (list #'(body1) #'(body2)))
    ;;
    (%chi-clambda input-form.stx lexenv.run lexenv.expand 'typed input-formals*.stx body**.stx))

  (define* (chi-case-lambda/checked input-form.stx lexenv.run lexenv.expand
				    input-formals*.stx body**.stx)
    ;;Expand the clauses of a CASE-LAMBDA/CHECKED syntax use and return a psi object.
    ;;
    ;;The  argument  INPUT-FORM.STX is  a  syntax  object representing  the  original
    ;;CASE-LAMBDA/CHECKED expression.   The argument INPUT-FORMALS*.STX is  a list of
    ;;syntax objects whose items are  the formals of the CASE-LAMBDA/CHECKED clauses.
    ;;The argument BODY**.STX is a list of  syntax objects whose items are the bodies
    ;;of the CASE-LAMBDA/CHECKED clauses.
    ;;
    ;;Example, for the input form:
    ;;
    ;;   (case-lambda/checked
    ;;     ((a b c) body1)
    ;;     ((d e f) body2))
    ;;
    ;;this function is invoked as:
    ;;
    ;;   (chi-case-lambda/checked
    ;;     #'(case-lambda/checked
    ;;         ((a b c) body1)
    ;;         ((d e f) body2))
    ;;     lexenv.run lexenv.expand
    ;;     (list #'(a b c) #'(d e f))
    ;;     (list #'(body1) #'(body2)))
    ;;
    (%chi-clambda input-form.stx lexenv.run lexenv.expand 'checked input-formals*.stx body**.stx))

;;; --------------------------------------------------------------------

  (define (%chi-clambda input-form.stx lexenv.run lexenv.expand type input-formals*.stx body**.stx)
    (receive (standard-formals*.stx clause-signature*)
	(syntax-object.parse-typed-clambda-multi-clauses-formals input-formals*.stx)
      ;;We do  the validation  on the standard  formals, so that  the "_"  element is
      ;;excluded.
      (cond ((%sublists-of-same-length standard-formals*.stx)
	     => (lambda (same-length-formals*.stx)
		  (syntax-violation #f
		    "invalid CASE-LAMBDA clause formals with the same length"
		    input-form.stx same-length-formals*.stx))))
      (receive (formals*.lex body*.psi)
	  (case type
	    ((typed)
	     (chi-case-lambda-clause*/typed   input-form.stx lexenv.run lexenv.expand
					      standard-formals*.stx clause-signature* body**.stx))
	    ((checked)
	     (chi-case-lambda-clause*/checked input-form.stx lexenv.run lexenv.expand
					      standard-formals*.stx clause-signature* body**.stx)))
	(make-psi input-form.stx
	  (build-case-lambda (syntax-annotation input-form.stx)
	      formals*.lex
	    (map psi.core-expr body*.psi))
	  (make-type-signature/single-value
	   (make-closure-type-spec (make-clambda-signature clause-signature*)))))))

  #| end of module |# )


;;;; named and typed CASE-LAMBDA expansion

(module (chi-named-case-lambda/typed chi-named-case-lambda/checked)

  (define* (chi-named-case-lambda/typed input-form.stx lexenv.run lexenv.expand
					who.id input-formals*.stx body**.stx)
    ;;Expand the  clauses of a  NAMED-CASE-LAMBDA/TYPED syntax  use and return  a psi
    ;;object.
    ;;
    (%chi-clambda input-form.stx lexenv.run lexenv.expand
		  'typed who.id input-formals*.stx body**.stx))

  (define* (chi-named-case-lambda/checked input-form.stx lexenv.run lexenv.expand
					  who.id input-formals*.stx body**.stx)
    ;;Expand the clauses  of a NAMED-CASE-LAMBDA/checked syntax use and  return a psi
    ;;object.
    ;;
    (%chi-clambda input-form.stx lexenv.run lexenv.expand
		  'checked who.id input-formals*.stx body**.stx))

;;; --------------------------------------------------------------------

  (define* (%chi-clambda input-form.stx lexenv.run lexenv.expand type who.id input-formals*.stx body**.stx)
    (receive (standard-formals*.stx clause-signature*)
	(syntax-object.parse-typed-clambda-multi-clauses-formals input-formals*.stx)
      ;;We do  the validation  on the standard  formals, so that  the "_"  element is
      ;;excluded.
      (cond ((%sublists-of-same-length standard-formals*.stx)
	     => (lambda (same-length-formals*.stx)
		  (syntax-violation #f
		    "invalid CASE-LAMBDA clause formals with the same length"
		    input-form.stx same-length-formals*.stx))))
      (receive (lexenv.run lexenv.expand)
	  ;;We establish  the syntactic binding  for "__who__" before  processing the
	  ;;formals and the body.  So the formals may shadow this binding.
	  (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand __who__ who.id)
	(receive (formals*.lex body*.psi)
	    (case type
	      ((typed)
	       (chi-case-lambda-clause*/typed   input-form.stx lexenv.run lexenv.expand
					        standard-formals*.stx clause-signature* body**.stx))
	      ((checked)
	       (chi-case-lambda-clause*/checked input-form.stx lexenv.run lexenv.expand
						standard-formals*.stx clause-signature* body**.stx)))
	  (make-psi input-form.stx
	    (build-case-lambda (syntax-annotation input-form.stx)
		formals*.lex
	      (map psi.core-expr body*.psi))
	    (make-type-signature/single-value
	     (make-closure-type-spec (make-clambda-signature clause-signature*))))))))

  #| end of module |# )


;;;; case-lambda clauses expander: standard and typed CASE-LAMBDA

(define (chi-case-lambda-clause*/std input-form.stx lexenv.run lexenv.expand
				     standard-formals*.stx clause-signature* body**.stx)
  ;;Recursive function.  Expand a clause from a CASE-LAMBDA/STD syntax use.
  ;;
  ;;The argument INPUT-FORM.STX is the syntax object holding the original input form.
  ;;The argument  STANDARD-FORMALS*.STX is a list  of syntax objects, each  holding a
  ;;proper or improper list of formal arguments.  The argument CLAUSE-SIGNATURE* is a
  ;;list of "<clambda-clause-signature>" objects.  The  argument BODY**.STX is a list
  ;;of syntax objects each holding the body forms.
  ;;
  ;;Return the following values:
  ;;
  ;;1.  A  list of subslists,  each sublist  being a proper  or improper list  of lex
  ;;gensyms representing the formals.
  ;;
  ;;2.  A list of PSI objects each containing a core language expression representing
  ;;the body of a clause.
  ;;
  (if (pair? standard-formals*.stx)
      (receive (standard-formals.lex body.psi)
	  (chi-lambda-clause/std input-form.stx lexenv.run lexenv.expand
				 (car standard-formals*.stx) (car clause-signature*) (car body**.stx))
	(receive (standard-formals*.lex body*.psi)
	    (chi-case-lambda-clause*/std input-form.stx lexenv.run lexenv.expand
					 (cdr standard-formals*.stx) (cdr clause-signature*) (cdr body**.stx))
	  (values (cons standard-formals.lex standard-formals*.lex)
		  (cons body.psi body*.psi))))
    (values '() '())))

(define* (chi-case-lambda-clause*/typed input-form.stx lexenv.run lexenv.expand
					standard-formals*.stx clause-signature* body-form**.stx)
  ;;Recursive function.  Expand all the clauses of a CASE-LAMBDA/TYPED syntax.
  ;;
  ;;The argument INPUT-FORM.STX is the syntax object holding the original input form.
  ;;The argument  STANDARD-FORMALS*.STX is a list  of syntax objects, each  holding a
  ;;proper or improper list of formal arguments.  The argument CLAUSE-SIGNATURE* is a
  ;;list of "<clambda-clause-signature>" objects.  The  argument BODY**.STX is a list
  ;;of syntax objects each holding the body forms.
  ;;
  ;;Return the following values:
  ;;
  ;;1.  A  list of subslists,  each sublist  being a proper  or improper list  of lex
  ;;gensyms representing the formals.
  ;;
  ;;2.  A list of PSI objects each containing a core language expression representing
  ;;the body of a clause.
  ;;
  (if (pair? standard-formals*.stx)
      (receive (standard-formals.lex body.psi)
	  (chi-lambda-clause/typed input-form.stx lexenv.run lexenv.expand
				   (car standard-formals*.stx) (car clause-signature*) (car body-form**.stx))
	(receive (standard-formals*.lex body*.psi)
	    (chi-case-lambda-clause*/typed input-form.stx lexenv.run lexenv.expand
					   (cdr standard-formals*.stx) (cdr clause-signature*) (cdr body-form**.stx))
	  (values (cons standard-formals.lex standard-formals*.lex)
		  (cons body.psi body*.psi))))
    (values '() '())))

(define* (chi-case-lambda-clause*/checked input-form.stx lexenv.run lexenv.expand
					  standard-formals*.stx clause-signature* body-form**.stx)
  ;;Recursive function.  Expand all the clauses of a CASE-LAMBDA/CHECKED syntax.
  ;;
  ;;The argument INPUT-FORM.STX is the syntax object holding the original input form.
  ;;The argument  STANDARD-FORMALS*.STX is a list  of syntax objects, each  holding a
  ;;proper or improper list of formal arguments.  The argument CLAUSE-SIGNATURE* is a
  ;;list of "<clambda-clause-signature>" objects.  The  argument BODY**.STX is a list
  ;;of syntax objects each holding the body forms.
  ;;
  ;;Return the following values:
  ;;
  ;;1.  A  list of subslists,  each sublist  being a proper  or improper list  of lex
  ;;gensyms representing the formals.
  ;;
  ;;2.  A list of PSI objects each containing a core language expression representing
  ;;the body of a clause.
  ;;
  (if (pair? standard-formals*.stx)
      (receive (standard-formals.lex body.psi)
	  (chi-lambda-clause/checked input-form.stx lexenv.run lexenv.expand
				     (car standard-formals*.stx) (car clause-signature*) (car body-form**.stx))
	(receive (standard-formals*.lex body*.psi)
	    (chi-case-lambda-clause*/checked input-form.stx lexenv.run lexenv.expand
					     (cdr standard-formals*.stx) (cdr clause-signature*) (cdr body-form**.stx))
	  (values (cons standard-formals.lex standard-formals*.lex)
		  (cons body.psi body*.psi))))
    (values '() '())))


(module LAMBDA-CLAUSE-EXPANSION-HELPERS
  (%expand-guts-with-proper-list-formals
   %expand-guts-with-improper-list-formals)

  (define (%expand-guts-with-proper-list-formals input-form.stx lexenv.run lexenv.expand
						 standard-formals.stx clause-signature body*.stx)
    ;;Expand  the guts  of a  lambda  clause for  the  case of  formals without  rest
    ;;argument.  Here  we know that  STANDARD-FORMALS.STX and the  corresponding type
    ;;signature are proper lists with equal length.
    (receive (rib lexenv.run standard-formals*.lex)
	(%establish-typed-syntactic-bindings-lhs* standard-formals.stx (clambda-clause-signature.argvals.specs clause-signature) lexenv.run)
      (%expand-body input-form.stx lexenv.run lexenv.expand standard-formals*.lex body*.stx rib)))

  (define (%expand-guts-with-improper-list-formals input-form.stx lexenv.run lexenv.expand
						   standard-formals.stx clause-signature body*.stx)
    ;;Expand the guts of a lambda clause  for the case of formals with rest argument.
    ;;Here we know that STANDARD-FORMALS.STX and the corresponding type signature are
    ;;improper lists with equal length.
    (let*-values
	(((arg*.id  rest.id)
	  (improper-list->list-and-rest standard-formals.stx))
	 ((arg*.ots rest.ots)
	  (improper-list->list-and-rest (clambda-clause-signature.argvals.specs clause-signature)))
	 ((rib lexenv.run standard-formals.lex)
	  (receive (rib lexenv.run all*.lex)
	      (%establish-typed-syntactic-bindings-lhs* (cons rest.id arg*.id) (cons rest.ots arg*.ots) lexenv.run)
	    ;;Yes, this call to APPEND builds an improper list.
	    (values rib lexenv.run (append (cdr all*.lex) (car all*.lex))))))
      (%expand-body input-form.stx lexenv.run lexenv.expand standard-formals.lex body*.stx rib)))

  (define (%expand-body input-form.stx lexenv.run lexenv.expand standard-formals.lex body*.stx rib)
    (let* ((body*.stx (push-lexical-contour rib body*.stx))
	   (body.psi  (chi-internal-body body*.stx lexenv.run lexenv.expand)))
      (values standard-formals.lex body.psi)))

  #| end of module: LAMBDA-CLAUSE-EXPANSION-HELPERS |# )


;;;; lambda clause expander: standard lambda clause

(define (chi-lambda-clause/std input-form.stx lexenv.run lexenv.expand
			       standard-formals.stx clause-signature body*.stx)
  ;;Expand the clause of a LAMBDA/STD  or DEFINE/STD syntax use or a single
  ;;clause of a CASE-LAMBDA/STD or CASE-DEFINE/STD syntax use.
  ;;
  ;;The argument INPUT-FORM.STX is the syntax object holding the original input form.
  ;;The argument STANDARD-FORMALS.STX is a syntax object holding a proper or improper
  ;;list of standard formal arguments.   The argument CLAUSE-SIGNATURE is an instance
  ;;of  "<clambda-clause-signature>".  The  argument BODY*.STX  is a  list of  syntax
  ;;objects holding the body forms.
  ;;
  ;;Return the following values:
  ;;
  ;;1. STANDARD-FORMALS.LEX,  a proper or  improper list of lex  gensyms representing
  ;;the lambda clause formals.
  ;;
  ;;2. BODY.PSI, a PSI object representing the expanded body.
  ;;
  ;;This  function creates  a  new rib  object to  represent  the formals'  syntactic
  ;;bindings, then pushes the rib on the body forms before expanding them.
  ;;
  (import LAMBDA-CLAUSE-EXPANSION-HELPERS)
  (cond
   ((list? standard-formals.stx)
    (%expand-guts-with-proper-list-formals   input-form.stx lexenv.run lexenv.expand
					     standard-formals.stx clause-signature body*.stx))
   (else
    (%expand-guts-with-improper-list-formals input-form.stx lexenv.run lexenv.expand
					     standard-formals.stx clause-signature body*.stx))))


;;;; lambda clause expander: typed lambda clause

(module (chi-lambda-clause/typed chi-lambda-clause/checked)

  (define* (chi-lambda-clause/typed input-form.stx lexenv.run lexenv.expand
				    standard-formals.stx clause-signature body*.stx)
    ;;Expand the  clause of  a LAMBDA/TYPED  or DEFINE/TYPED syntax  use or  a single
    ;;clause of a CASE-LAMBDA/TYPED or CASE-DEFINE/TYPED syntax use.
    ;;
    ;;The argument  INPUT-FORM.STX is  the syntax object  holding the  original input
    ;;form.  The argument STANDARD-FORMALS.STX is a syntax object holding a proper or
    ;;improper list of  standard formal arguments.  The  argument CLAUSE-SIGNATURE is
    ;;an instance of "<clambda-clause-signature>".  The  argument BODY*.STX is a list
    ;;of syntax objects holding the body forms.
    ;;
    ;;Return the following values:
    ;;
    ;;1. STANDARD-FORMALS.LEX, a proper or  improper list of lex gensyms representing
    ;;the lambda clause formals.
    ;;
    ;;2. BODY.PSI, a PSI object representing the expanded body.
    ;;
    ;;NOTE The expander for the internal body will create yet another lexical contour
    ;;to hold the body's internal definitions.
    ;;
    (import LAMBDA-CLAUSE-EXPANSION-HELPERS)
    (let ((body*.stx (%insert-retvals-validation-form clause-signature body*.stx)))
      (cond
       ((list? standard-formals.stx)
	;;Without rest  argument.  Here  we know  that both  STANDARD-FORMALS.STX and
	;;ARGVALS-SIGNATURE.SPECS are proper lists with equal length.
	(%expand-guts-with-proper-list-formals input-form.stx lexenv.run lexenv.expand
					       standard-formals.stx clause-signature body*.stx))

       (else
	;;With  rest  argument.  Here  we  know  that both  STANDARD-FORMALS.STX  and
	;;ARGVALS-SIGNATURE.SPECS are improper lists with equal length.
	(%expand-guts-with-improper-list-formals input-form.stx lexenv.run lexenv.expand
						 standard-formals.stx clause-signature body*.stx)))))

  (define* (chi-lambda-clause/checked input-form.stx lexenv.run lexenv.expand
				      standard-formals.stx clause-signature body*.stx)
    ;;Expand the clause of a LAMBDA/CHECKED  or DEFINE/CHECKED syntax use or a single
    ;;clause of a CASE-LAMBDA/CHECKED or CASE-DEFINE/CHECKED syntax use.
    ;;
    ;;The argument  INPUT-FORM.STX is  the syntax object  holding the  original input
    ;;form.  The argument STANDARD-FORMALS.STX is a syntax object holding a proper or
    ;;improper list of  standard formal arguments.  The  argument CLAUSE-SIGNATURE is
    ;;an instance of "<clambda-clause-signature>".  The  argument BODY*.STX is a list
    ;;of syntax objects holding the body forms.
    ;;
    ;;Return the following values:
    ;;
    ;;1. STANDARD-FORMALS.LEX, a proper or  improper list of lex gensyms representing
    ;;the lambda clause formals.
    ;;
    ;;2. BODY.PSI, a PSI object representing the expanded body.
    ;;
    ;;NOTE The expander for the internal body will create yet another lexical contour
    ;;to hold the body's internal definitions.
    ;;
    (import LAMBDA-CLAUSE-EXPANSION-HELPERS)
    (define argvals-signature.specs
      (type-signature.object-type-specs (clambda-clause-signature.argvals clause-signature)))
    (define validation-who
      (let* ((id    (core-prim-id '__who__))
	     (label (id->label id))
	     (descr (label->syntactic-binding-descriptor label lexenv.run)))
	(case (syntactic-binding-descriptor.type descr)
	  ((displaced-lexical)
	   ;;__who__ is unbound.
	   '(quote _))
	  (else
	   ;;__who__ is bound.
	   '__who__))))
    (cond
     ((list? standard-formals.stx)
      ;;Without  rest argument.   Here  we know  that  both STANDARD-FORMALS.STX  and
      ;;ARGVALS-SIGNATURE.SPECS are proper lists with equal length.
      (let ((formals-validation-form*.stx (build-formals-validation-form* input-form.stx lexenv.run lexenv.expand validation-who
									  standard-formals.stx argvals-signature.specs #f #f)))
	(let* ((body*.stx (%insert-retvals-validation-form clause-signature body*.stx))
	       (body*.stx (if (pair? formals-validation-form*.stx)
			      (append formals-validation-form*.stx
				      ;;We introduce  an internal  body to  allow the
				      ;;correct expansion of  internal definitions in
				      ;;BODY*.STX.
				      `((,(core-prim-id 'internal-body) . ,body*.stx)))
			    body*.stx)))
	  (%expand-guts-with-proper-list-formals input-form.stx lexenv.run lexenv.expand
						 standard-formals.stx clause-signature body*.stx))))

     (else
      ;;With  rest  argument.   Here  we  know  that  both  STANDARD-FORMALS.STX  and
      ;;ARGVALS-SIGNATURE.SPECS are improper lists with equal length.
      (let ((formals-validation-form*.stx (let-values
					      (((arg*.id  rest.id)  (improper-list->list-and-rest standard-formals.stx))
					       ((arg*.ots rest.ots) (improper-list->list-and-rest argvals-signature.specs)))
					    (build-formals-validation-form* input-form.stx lexenv.run lexenv.expand validation-who
									    arg*.id arg*.ots rest.id rest.ots))))
	(let* ((body*.stx (%insert-retvals-validation-form clause-signature body*.stx))
	       (body*.stx (if (pair? formals-validation-form*.stx)
			      (append formals-validation-form*.stx
				      ;;We introduce  an internal  body to  allow the
				      ;;correct expansion of  internal definitions in
				      ;;BODY*.STX.
				      `((,(core-prim-id 'internal-body) . ,body*.stx)))
			    body*.stx)))
	  (%expand-guts-with-improper-list-formals input-form.stx lexenv.run lexenv.expand
						   standard-formals.stx clause-signature body*.stx))))))

;;; --------------------------------------------------------------------

  (module (build-formals-validation-form*)

    (define-constant MISMATCH-ERROR-MESSAGE
      "mismatch between type of operand and type of argument, failed run-time validation with type predicate")

    (define (build-formals-validation-form* input-form.stx lexenv.run lexenv.expand
					    validation-who arg*.id arg*.ots rest.id rest.ots)
      ;;When expanding a typed LAMBDA form like:
      ;;
      ;;   (lambda/checked ({a <fixnum>} {b <string>} . {rest <fixnum*>})
      ;;     ?body)
      ;;
      ;;we want to transform it into an equivalent of:
      ;;
      ;;   (lambda/std (a b)
      ;;     (unless (fixnum? a)
      ;;       (procedure-signature-argument-violation __who__
      ;;         "invalid object type" 1 '(is-a? _ <fixnum>) a)
      ;;
      ;;     (unless (string? b)
      ;;       (procedure-signature-argument-violation __who__
      ;;         "invalid object type" 2 '(is-a? _ <string>) b)
      ;;
      ;;     (fold-left
      ;;         (lambda/std (idx obj)
      ;;           (unless (fixnum? obj)
      ;;             (procedure-signature-argument-violation __who__
      ;;               "invalid object type" idx '(is-a? _ <fixnum>) obj))
      ;;           (fxadd1 idx))
      ;;       3 rest)
      ;;
      ;;     ?body)
      ;;
      ;;This  function builds  and  returns  a list  of  syntax objects  representing
      ;;expressions that validate (at run-time)  the arguments (excluding the formals
      ;;having type  "<top>", whose  arguments are  always valid).   If there  are no
      ;;arguments: return null.
      ;;
      ;;The argument  VALIDATION-WHO is a  symbolic expression representing  value of
      ;;the  "&who"  condition object  to  be  used  in  the output  expression  when
      ;;validating arguments.
      ;;
      ;;The arguments ARG*.ID and ARG*.OTS are  lists of identifiers and instances of
      ;;"<object-type-spec>":  the formers  representing the  names of  the mandatory
      ;;formal arguments,  the latters representing  the type specifications  of each
      ;;value in mandatory formal arguments.
      ;;
      ;;The arguments REST.ID  and REST.OTS must be an identifier  and an instance of
      ;;"<object-type-spec>": the  former representing the  name of the rest  or args
      ;;argument, the latter representing the type specification of the whole rest or
      ;;args argument (which must  be a list type).  When there  is no rest argument:
      ;;REST.ID and REST.OTS must be #f.
      ;;
      (let recur ((arg*.id	arg*.id)
		  (arg*.ots	arg*.ots)
		  (idx		1))
	(cond ((pair? arg*.id)
	       (%build-single-formal-validation-form input-form.stx lexenv.run lexenv.expand
						     validation-who (car arg*.id) (car arg*.ots) idx
						     (recur (cdr arg*.id) (cdr arg*.ots) (fxadd1 idx))))
	      ((or (not		rest.ots)
		   (<list>-ots?	rest.ots))
	       ;;There is  no rest argument  or it is  tagged as "<list>";  insert no
	       ;;validation.
	       '())
	      (else
	       (%build-rest-formal-validation-form input-form.stx lexenv.run lexenv.expand
						   validation-who rest.id rest.ots idx)))))

    (define* (%build-single-formal-validation-form input-form.stx lexenv.run lexenv.expand
						   validation-who arg.id arg.ots idx following-validations)
      (cond ((<top>-ots? arg.ots)
	     ;;Insert no validation for an argument typed "<top>".
	     following-validations)
	    (else
	     (cons (let ((type-pred.sexp	(object-type-spec.type-predicate-stx arg.ots))
			 (arg.name		(object-type-spec.name arg.ots)))
		     (bless
		      `(unless (,type-pred.sexp ,arg.id)
			 (procedure-signature-argument-violation ,validation-who
			   ,MISMATCH-ERROR-MESSAGE ,idx '(is-a? _ ,arg.name) ,arg.id))))
		   following-validations))))

    (define* (%build-rest-formal-validation-form input-form.stx lexenv.run lexenv.expand
						 validation-who rest.id rest.ots idx)
      (cond ((list-of-type-spec? rest.ots)
	     ;;Generate a validating expression that accepts  both null and a list of
	     ;;objects of the specified type.
	     (let* ((item.ots	(list-of-type-spec.item-ots rest.ots))
		    (item-pred	(object-type-spec.type-predicate-stx item.ots))
		    (item.name	(object-type-spec.name item.ots))
		    (obj.sym	(gensym "obj"))
		    (idx.sym	(gensym "idx")))
	       (bless
		`((fold-left (lambda (,idx.sym ,obj.sym)
			       (unless (,item-pred ,obj.sym)
				 (procedure-signature-argument-violation ,validation-who
				   ,MISMATCH-ERROR-MESSAGE ,idx.sym '(is-a? _ ,item.name) ,obj.sym))
			       (fxadd1 ,idx.sym))
		    ,idx ,rest.id)))))

	    (else
	     ;;REST.OTS is some other sub-type of  "<list>".  Just rely on the type's
	     ;;own predicate.
	     (let ((type-pred	(object-type-spec.type-predicate-stx rest.ots))
		   (rest.name	(object-type-spec.name rest.ots)))
	       (bless
		`(unless (,type-pred ,rest.id)
		   (procedure-signature-argument-violation __who__
		     ,MISMATCH-ERROR-MESSAGE #f '(is-a? _ ,rest.name) ,rest.id)))))))

    #| end of module: BUILD-FORMALS-VALIDATION-FORM* |# )

;;; --------------------------------------------------------------------

  (define* (%insert-retvals-validation-form clause-signature body-form*.stx)
    ;;When expanding a typed LAMBDA form like:
    ;;
    ;;   (lambda ({_ <symbol>} a b)
    ;;     ?body ... ?last-body)
    ;;
    ;;we want to transform it into an equivalent of:
    ;;
    ;;   (lambda (a b)
    ;;     ?body ...
    ;;     (assert-signature-and-return (<symbol>) ?last-body))
    ;;
    ;;Add the return values validation to the last form in the body; return a list of
    ;;body forms.
    ;;
    (let ((retvals-signature.sig (clambda-clause-signature.retvals clause-signature)))
      (if (type-signature.fully-untyped? retvals-signature.sig)
	  ;;The number and type of return values is unknown.
	  body-form*.stx
	(let ((retvals-signature.tags (type-signature.syntax-object retvals-signature.sig)))
	  (receive (head*.stx last.stx)
	      (proper-list->head-and-last body-form*.stx)
	    (bless
	     `(,@head*.stx
	       (assert-signature-and-return ,retvals-signature.tags ,last.stx))))))))

  #| end of module |# )


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;End:
