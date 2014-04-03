;;;Copyright (c) 2010-2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
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
    ((quote)					quote-transformer)
    ((lambda)					lambda-transformer)
    ((case-lambda)				case-lambda-transformer)
    ((letrec)					letrec-transformer)
    ((letrec*)					letrec*-transformer)
    ((if)					if-transformer)
    ((foreign-call)				foreign-call-transformer)
    ((syntax-case)				syntax-case-transformer)
    ((syntax)					syntax-transformer)
    ((fluid-let-syntax)				fluid-let-syntax-transformer)
    ((splice-first-expand)			splice-first-expand-transformer)
    ((internal-body)				internal-body-transformer)
    ((unsafe)					unsafe-transformer)
    ((predicate-procedure-argument-validation)	predicate-procedure-argument-validation-transformer)
    ((predicate-return-value-validation)	predicate-return-value-validation-transformer)

    ((struct-type-descriptor)			struct-type-descriptor-transformer)
    ((struct-type-and-struct?)			struct-type-and-struct?-transformer)
    ((struct-type-field-ref)			struct-type-field-ref-transformer)
    ((struct-type-field-set!)			struct-type-field-set!-transformer)
    (($struct-type-field-ref)			$struct-type-field-ref-transformer)
    (($struct-type-field-set!)			$struct-type-field-set!-transformer)

    ((record-type-descriptor)			record-type-descriptor-transformer)
    ((record-constructor-descriptor)		record-constructor-descriptor-transformer)
    ((record-type-field-set!)			record-type-field-set!-transformer)
    ((record-type-field-ref)			record-type-field-ref-transformer)
    (($record-type-field-set!)			$record-type-field-set!-transformer)
    (($record-type-field-ref)			$record-type-field-ref-transformer)

    ((type-descriptor)				type-descriptor-transformer)
    ((is-a?)					is-a?-transformer)
    ((slot-ref)					slot-ref-transformer)
    ((slot-set!)				slot-set!-transformer)
    (($slot-ref)				$slot-ref-transformer)
    (($slot-set!)				$slot-set!-transformer)

    ((tag-predicate)				tag-predicate-transformer)
    ((tag-procedure-argument-validator)		tag-procedure-argument-validator-transformer)
    ((tag-return-value-validator)		tag-return-value-validator-transformer)
    ((tag-assert)				tag-assert-transformer)
    ((tag-assert-and-return)			tag-assert-and-return-transformer)
    ((tag-accessor)				tag-accessor-transformer)
    ((tag-mutator)				tag-mutator-transformer)
    ((tag-getter)				tag-getter-transformer)
    ((tag-setter)				tag-setter-transformer)
    ((tag-dispatch)				tag-dispatch-transformer)
    ((tag-cast)					tag-cast-transformer)

    (else
     (assertion-violation __who__
       "Vicare: internal error: cannot find transformer" name))))


;;;; module core-macro-transformer: IF

(define (if-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS  IF syntaxes from the top-level built in
  ;;environment.   Expand the  syntax object  EXPR-STX in  the context  of the  given
  ;;LEXENV; return a PSI struct.
  ;;
  (syntax-match expr-stx ()
    ((_ ?test ?consequent ?alternate)
     (let ((test.psi       (chi-expr ?test       lexenv.run lexenv.expand))
	   (consequent.psi (chi-expr ?consequent lexenv.run lexenv.expand))
	   (alternate.psi  (chi-expr ?alternate  lexenv.run lexenv.expand)))
       (make-psi (build-conditional no-source
		   (psi-core-expr test.psi)
		   (psi-core-expr consequent.psi)
		   (psi-core-expr alternate.psi))
		 )))
    ((_ ?test ?consequent)
     (let ((test.psi       (chi-expr ?test       lexenv.run lexenv.expand))
	   (consequent.psi (chi-expr ?consequent lexenv.run lexenv.expand)))
       (make-psi (build-conditional no-source
		   (psi-core-expr test.psi)
		   (psi-core-expr consequent.psi)
		   (build-void))
		 )))
    ))


;;;; module core-macro-transformer: QUOTE

(define (quote-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS  QUOTE syntaxes from the top-level built
  ;;in environment.   Expand the syntax object  EXPR-STX in the context  of the given
  ;;LEXENV; return a PSI struct.
  ;;
  (syntax-match expr-stx ()
    ((_ ?datum)
     (let ((datum (syntax->datum ?datum)))
       (make-psi (build-data no-source
		   datum)
		 (retvals-signature-of-datum datum))))
    ))


;;;; module core-macro-transformer: LAMBDA and CASE-LAMBDA

(define (case-lambda-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  R6RS CASE-LAMBDA syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return an PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?formals* ?body* ?body** ...) ...)
     (chi-case-lambda input-form.stx ?formals* (map cons ?body* ?body**) lexenv.run lexenv.expand))
    ))

(define (lambda-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS LAMBDA syntaxes from the top-level built
  ;;in environment.   Expand the syntax object  INPUT-FORM.STX in the context  of the
  ;;given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?formals ?body ?body* ...)
     (chi-lambda input-form.stx ?formals (cons ?body ?body*) lexenv.run lexenv.expand))
    ))


;;;; module core-macro-transformer: LETREC and LETREC*

(module (letrec-transformer letrec*-transformer)

  (define (letrec-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand LETREC syntaxes from the top-level built in
    ;;environment.  Expand  the syntax object  INPUT-FORM.STX in  the context of  the given
    ;;LEXENV; return a PSI struct.
    ;;
    (%letrec-helper input-form.stx lexenv.run lexenv.expand build-letrec))

  (define (letrec*-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used  to expand LETREC* syntaxes from  the top-level built
    ;;in environment.  Expand the syntax object  INPUT-FORM.STX in the context of the given
    ;;LEXENV; return a PSI struct.
    ;;
    (%letrec-helper input-form.stx lexenv.run lexenv.expand build-letrec*))

  (define (%letrec-helper input-form.stx lexenv.run lexenv.expand core-lang-builder)
    (syntax-match input-form.stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check that the binding names are identifiers and without duplicates.
       (receive (lhs* tag*)
	   (parse-list-of-tagged-bindings ?lhs* input-form.stx)
	 ;;Generate unique variable names and labels for the LETREC bindings.
	 (let ((lex*     (map gensym-for-lexical-var lhs*))
	       (lab*     (map gensym-for-label       lhs*))
	       (rhs*.stx (map (lambda (rhs.stx tag)
				(if (free-id=? tag (untagged-tag-id))
				    rhs.stx
				  (bless
				   `(tag-assert-and-return (,tag) ,rhs.stx))))
			   ?rhs* tag*)))
	   (map (lambda (label tag)
		  (and tag (set-label-tag! label tag)))
	     lab* tag*)
	   ;;Generate what  is needed  to create  a lexical contour:  a <RIB>  and an
	   ;;extended lexical  environment in which  to evaluate both  the right-hand
	   ;;sides and the body.
	   ;;
	   ;;Notice  that the  region of  all the  LETREC bindings  includes all  the
	   ;;right-hand sides.
	   (let ((rib		(make-filled-rib lhs* lab*))
		 (lexenv.run	(add-lexical-bindings lab* lex* lexenv.run)))
	     ;;Create the lexical  contour then process body and  right-hand sides of
	     ;;bindings.
	     (let ((body.psi (chi-internal-body (push-lexical-contour rib
						  (cons ?body ?body*))
						lexenv.run lexenv.expand))
		   (rhs*.psi (chi-expr*         (map (lambda (rhs.stx)
						       (push-lexical-contour rib rhs.stx))
						  rhs*.stx)
						lexenv.run lexenv.expand)))
	       (let* ((rhs*.core (map psi-core-expr rhs*.psi))
		      (body.core (psi-core-expr body.psi))
		      ;;Build the LETREC or LETREC* expression in the core language.
		      (expr.core (core-lang-builder no-source lex* rhs*.core body.core)))
		 (make-psi expr.core
			   (psi-retvals-signature body.psi))))))))
      ))

  #| end of module |# )


;;;; module core-macro-transformer: FLUID-LET-SYNTAX

(define (fluid-let-syntax-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  FLUID-LET-SYNTAX syntaxes from the top-level
  ;;built in  environment.  Expand the syntax  object EXPR-STX in the  context of the
  ;;given LEXENV; return a PSI struct.
  ;;
  ;;FLUID-LET-SYNTAX is similar,  but not equal, to LET-SYNTAX;  rather than defining
  ;;new ?LHS bindings, it temporarily rebinds  the keywords to new transformers while
  ;;expanding  the ?BODY  forms.   The given  ?LHS  must be  already  bound to  fluid
  ;;syntaxes defined by DEFINE-FLUID-SYNTAX.
  ;;
  ;;There   are   two   differences    between   FLUID-LET-SYNTAX   and   LET-SYNTAX:
  ;;FLUID-LET-SYNTAX must appear in expression context only; the internal ?BODY forms
  ;;are *not* spliced in the enclosing body.
  ;;
  ;;NOTE We would truly like to splice  the inner body forms in the surrounding body,
  ;;so that  this syntax could  act like LET-SYNTAX, which  is useful; but  we really
  ;;cannot do it with this implementation of the expander algorithm.  This is because
  ;;LET-SYNTAX both creates a new rib and adds new id/label entries to it, and pushes
  ;;label/descriptor  entries to  the  LEXENV; instead  FLUID-LET-SYNTAX only  pushes
  ;;entries to the LEXENV:  there is no way to keep the  fluid LEXENV entries visible
  ;;only to a subsequence of forms in a body.  (Marco Maggi; Tue Feb 18, 2014)
  ;;
  (define (transformer expr-stx)
    (syntax-match expr-stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check that the ?LHS* are all identifiers with no duplicates.
       (unless (valid-bound-ids? ?lhs*)
	 (%error-invalid-formals-syntax expr-stx ?lhs*))
       (let* ((fluid-label* (map %lookup-binding-in-lexenv.run ?lhs*))
	      (binding*     (map (lambda (rhs)
				   (with-exception-handler
				       (lambda (E)
					 (raise
					  (condition
					   (make-macro-input-form-condition rhs)
					   E)))
				     (lambda ()
				       (%eval-macro-transformer
					(%expand-macro-transformer rhs lexenv.expand)
					lexenv.run))))
			      ?rhs*))
	      (entry*       (map cons fluid-label* binding*)))
	 (chi-internal-body (cons ?body ?body*)
			    (append entry* lexenv.run)
			    (append entry* lexenv.expand))))))

  (define (%lookup-binding-in-lexenv.run lhs)
    ;;Search the binding of the identifier LHS retrieving its label; if such label is
    ;;present and its  associated syntactic binding descriptor from  LEXENV.RUN is of
    ;;type "fluid  syntax": return  the associated  fluid label that  can be  used to
    ;;rebind the identifier.
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
  ;;Transformer  function used  to  expand Vicare's  FOREIGN-CALL  syntaxes from  the
  ;;top-level built in environment.  Expand the syntax object EXPR-STX in the context
  ;;of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name ?arg* ...)
     (let* ((name.psi  (chi-expr  ?name lexenv.run lexenv.expand))
	    (arg*.psi  (chi-expr* ?arg* lexenv.run lexenv.expand))
	    (expr.core (build-foreign-call no-source
			 (psi-core-expr name.psi)
			 (map psi-core-expr arg*.psi))))
       (make-psi expr.core)))
    ))


;;;; module core-macro-transformer: SYNTAX

(module (syntax-transformer)
  ;;Transformer function  used to  expand R6RS's SYNTAX  syntaxes from  the top-level
  ;;built in  environment.  Process  the contents  of USE-STX in  the context  of the
  ;;lexical environments LEXENV.RUN and LEXENV.EXPAND.  Return a PSI struct.
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
  ;;in  which:  ?DATUM  is  a  literal  datum,  ?PATTERN-VARIABLE  is  an  identifier
  ;;referencing a pattern  variable created by SYNTAX-CASE, ?ID is  an identifier not
  ;;referencing a  pattern variable, ?SUBTEMPLATE is  a template followed by  zero or
  ;;more ellipsis identifiers.
  ;;
  ;;Return  a sexp  representing code  in the  core language  which, when  evaluated,
  ;;returns a wrapped or unwrapped syntax object containing an expression in which:
  ;;
  ;;*  All  the  template  identifiers  being references  to  pattern  variables  are
  ;;  substituted with the corresponding syntax objects.
  ;;
  ;;     (syntax-case #'123 (?obj (syntax ?obj)))
  ;;     => #<syntax expr=123>
  ;;
  ;;     (syntax-case #'(1 2) ((?a ?b) (syntax #(?a ?b))))
  ;;     => #(#<syntax expr=1> #<syntax expr=1>)
  ;;
  ;;* All the identifiers not being references to pattern variables are left alone to
  ;;  be  captured by  the lexical  context at the  level below  the current,  in the
  ;;  context of the SYNTAX macro use or the context of the output form.
  ;;
  ;;     (syntax-case #'(1) ((?a) (syntax (display ?b))))
  ;;     => (#<syntax expr=display>
  ;;         #<syntax expr=1> . #<syntax expr=()>)
  ;;
  ;;* All  the sub-templates followed by  ellipsis are replicated to  match the input
  ;;  pattern.
  ;;
  ;;     (syntax-case #'(1 2 3) ((?a ...) (syntax #(?a ...))))
  ;;     => #(1 2 3)
  ;;
  ;;About pattern  variables: they are  present in  a lexical environment  as entries
  ;;with format:
  ;;
  ;;   (?label . (syntax . (?name . ?level)))
  ;;
  ;;where: ?LABEL  is the label  in the identifier's  syntax object, "syntax"  is the
  ;;symbol  "syntax", ?NAME  is  the  symbol representing  the  name  of the  pattern
  ;;variable, ?LEVEL  is an  exact integer representing  the nesting  ellipsis level.
  ;;The SYNTAX-CASE patterns below will generate the given entries:
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
  ;;The  input template  is first  visited  in post-order,  building an  intermediate
  ;;symbolic representation  of it;  then the symbolic  representation is  visited in
  ;;post-order, building  core language code  that evaluates to the  resulting syntax
  ;;object.   Examples  of  intermediate  representation (-->)  and  expansion  (==>)
  ;;follows, assuming identifiers starting with "?"  are pattern variables:
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
	   (make-psi code))))
      ))

  (define (%gen-syntax use-stx template-stx lexenv maps ellipsis? vec?)
    ;;Recursive function.  Expand the contents of a SYNTAX use.
    ;;
    ;;USE-STX must be the syntax object  containing the original SYNTAX macro use; it
    ;;is used for descriptive error reporting.
    ;;
    ;;TEMPLATE-STX must be the template from the SYNTAX macro use.
    ;;
    ;;LEXENV is the  lexical environment in which the expansion  takes place; it must
    ;;contain the pattern variables visible by this SYNTAX use.
    ;;
    ;;MAPS is a  list of alists, one  alist for each ellipsis nesting  level.  If the
    ;;template has 3 nested ellipsis patterns:
    ;;
    ;;   (((?a ...) ...) ...)
    ;;
    ;;while we  are processing  the inner  "(?a ...)"  MAPS  contains 3  alists.  The
    ;;alists are used  when processing ellipsis templates  that recursively reference
    ;;the same pattern variable, for example:
    ;;
    ;;   ((?a (?a ...)) ...)
    ;;
    ;;the inner ?A is  mapped to a gensym which is used to  generate a binding in the
    ;;output code.
    ;;
    ;;ELLIPSIS?  must be  a predicate  function returning  true when  applied to  the
    ;;ellipsis identifier  from the built in  environment.  Such function is  made an
    ;;argument, so that it can be changed  to a predicate returning always false when
    ;;we are recursively processing a quoted template:
    ;;
    ;;   (... ?sub-template)
    ;;
    ;;in which the ellipses in ?SUB-TEMPLATE are to be handled as normal identifiers.
    ;;
    ;;VEC? is a boolean: true when this function is processing the items of a vector.
    ;;
    (syntax-match template-stx ()

      ;;Standalone ellipses are not allowed.
      ;;
      (?dots
       (ellipsis? ?dots)
       (stx-error use-stx "misplaced ellipsis in syntax form"))

      ;;Match a standalone  identifier.  ?ID can be: a reference  to pattern variable
      ;;created by SYNTAX-CASE; an identifier that  will be captured by some binding;
      ;;an  identifier  that will  result  to  be free,  in  which  case an  "unbound
      ;;identifier" error will be raised later.
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
      ;;but ellipses starting a list template  are allowed, they quote the subsequent
      ;;sub-template:
      ;;
      ;;   (... ...)		==> quoted ellipsis
      ;;   (... ?sub-template)	==> quoted ?SUB-TEMPLATE
      ;;
      ;;so that the ellipses in the  ?SUB-TEMPLATE are treated as normal identifiers.
      ;;We change  the ELLIPSIS? argument  for recursion  to a predicate  that always
      ;;returns false.
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

      ;;Process a vector template.  We set to true the VEC? argument for recursion.
      ;;
      (#(?item* ...)
       (receive (item*.new maps)
	   (%gen-syntax use-stx ?item* lexenv maps ellipsis? #t)
	 (values (%gen-vector template-stx ?item* item*.new)
		 maps)))

      ;;Everything else is just quoted in  the output.  This includes all the literal
      ;;datums.
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
  ;;Transformer  function  used  to  expand  R6RS's  SYNTAX-CASE  syntaxes  from  the
  ;;top-level built in  environment.  Process the contents of USE-STX  in the context
  ;;of the lexical environments LEXENV.RUN and LEXENV.EXPAND.  Return a PSI struct.
  ;;
  ;;Notice that the parsing of the patterns is performed by CONVERT-PATTERN at expand
  ;;time and the actual pattern matching is performed by SYNTAX-DISPATCH at run time.
  ;;
  (define (syntax-case-transformer use-stx lexenv.run lexenv.expand)
    (syntax-match use-stx ()
      ((_ ?expr (?literal* ...) ?clauses* ...)
       (%verify-literals ?literal* use-stx)
       (let* ( ;;The identifier to which the result of evaluating the ?EXPR is bound.
	      (expr.id    (gensym-for-lexical-var 'tmp))
	      ;;The full SYNTAX-CASE pattern matching code, generated and transformed
	      ;;to core language.
	      (body.core  (%gen-syntax-case expr.id ?literal* ?clauses*
					    lexenv.run lexenv.expand))
	      ;;The ?EXPR transformed to core language.
	      (expr.core  (%chi-expr.core ?expr lexenv.run lexenv.expand)))
	 ;;Return a form like:
	 ;;
	 ;;   ((lambda (expr.id) body.core) expr.core)
	 ;;
	 (make-psi (build-application no-source
		     (build-lambda no-source (list expr.id) body.core)
		     (list expr.core)))))
      ))

  (define (%gen-syntax-case expr.id literals clauses lexenv.run lexenv.expand)
    ;;Recursive function.  Generate and return the  full pattern matching code in the
    ;;core language to match the given CLAUSES.
    ;;
    (syntax-match clauses ()
      ;;No pattern matched the input expression: return code to raise a syntax error.
      ;;
      (()
       (build-application no-source
	 (build-primref no-source 'syntax-error)
	 (list (build-lexical-reference no-source expr.id))))

      ;;The pattern is  a standalone identifier, neither a literal  nor the ellipsis,
      ;;and  it has  no  fender.   A standalone  identifier  with  no fender  matches
      ;;everything, so it is  useless to generate the code for  the next clauses: the
      ;;code generated here is the last one.
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
	   ;;the  underscore  identifier  matches  everything and  binds  no  pattern
	   ;;variables.
	   (%chi-expr.core ?output-expr lexenv.run lexenv.expand)
	 ;;The clause is:
	 ;;
	 ;;   (?id ?output-expr)
	 ;;
	 ;;a  standalone identifier  matches everything  and  binds it  to a  pattern
	 ;;variable whose name is ?ID.
	 (let ((label (gensym-for-label ?pattern))
	       (lex   (gensym-for-lexical-var ?pattern)))
	   ;;The expression must be expanded  in a lexical environment augmented with
	   ;;the pattern variable.
	   (define output-expr^
	     (push-lexical-contour
		 (make-filled-rib (list ?pattern) (list label))
	       ?output-expr))
	   (define lexenv.run^
	     ;;Push  a  pattern  variable  entry to  the  lexical  environment.   The
	     ;;ellipsis nesting level is 0.
	     (cons (cons label (make-binding 'syntax (cons lex 0)))
		   lexenv.run))
	   (define output-expr.core
	     (%chi-expr.core output-expr^ lexenv.run^ lexenv.expand))
	   (build-application no-source
	     (build-lambda no-source
	       (list lex)
	       output-expr.core)
	     (list (build-lexical-reference no-source expr.id))))))

      ;;The  pattern  is neither  a  standalone  pattern  variable nor  a  standalone
      ;;underscore.   It has  no fender,  which  is equivalent  to having  a "#t"  as
      ;;fender.
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
    ;;Generate  the code  needed  to  match the  clause  represented by  PATTERN.STX,
    ;;FENDER.STX  and OUTPUT-EXPR.STX;  recursively generate  the code  to match  the
    ;;other clauses in NEXT-CLAUSES.
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
    ;;notice that  the return value of  SYNTAX-DISPATCH is: false if  the pattern did
    ;;not match, otherwise the list of values to be bound to the pattern variables.
    ;;
    (receive (pattern.dispatch pvars.levels)
	;;CONVERT-PATTERN  return 2  values: the  pattern in  the format  accepted by
	;;SYNTAX-DISPATCH, an alist representing the pattern variables:
	;;
	;;*  The keys  of the  alist are  identifiers representing  the names  of the
	;;  pattern variables.
	;;
	;;* The values of the alist  are non-negative exact integers representing the
	;;   ellipsis  nesting level  of  the  corresponding pattern  variable.   See
	;;  SYNTAX-TRANSFORMER for details.
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
    ;;Generate the code that tests the fender:  if the fender succeeds run the output
    ;;expression, else try to match the next clauses.
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
    ;;Generate code to evaluate EXPR.STX in an environment augmented with the pattern
    ;;variables  defined   by  PVARS.LEVELS.   Return  a   core  language  expression
    ;;representing the following pseudo-code:
    ;;
    ;;   (apply (lambda (pattern-var ...) expr) tmp)
    ;;
    (define ids
      ;;For each pattern variable: the identifier representing its name.
      (map car pvars.levels))
    (define labels
      ;;For each pattern variable: a gensym used as label in the lexical environment.
      (map gensym-for-label ids))
    (define names
      ;;For  each pattern  variable: a  gensym used  as unique  variable name  in the
      ;;lexical environment.
      (map gensym-for-lexical-var ids))
    (define levels
      ;;For each pattern variable: an exact integer representing the ellipsis nesting
      ;;level.  See SYNTAX-TRANSFORMER for details.
      (map cdr pvars.levels))
    (define bindings
      ;;For each pattern variable: a binding to be pushed on the lexical environment.
      (map (lambda (label name level)
	     (cons label (make-binding 'syntax (cons name level))))
	labels names levels))
    (define expr.core
      ;;Expand the  expression in  a lexical environment  augmented with  the pattern
      ;;variables.
      ;;
      ;;NOTE We could have created a syntax object:
      ;;
      ;;  #`(lambda (pvar ...) #,expr.stx)
      ;;
      ;;and  then  expanded it:  EXPR.STX  would  have  been  expanded in  a  lexical
      ;;environment augmented with the PVAR bindings.
      ;;
      ;;Instead we have  chosen to push the PVAR bindings  on the lexical environment
      ;;"by hand", then  to expand EXPR.STX in the augmented  environment, finally to
      ;;put the resulting core language expression in a core language LAMBDA syntax.
      ;;
      ;;The two methods are fully equivalent; the one we have chosen is a bit faster.
      ;;
      (%chi-expr.core (push-lexical-contour
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

  (define (%chi-expr.core expr.stx lexenv.run lexenv.expand)
    (psi-core-expr (chi-expr expr.stx lexenv.run lexenv.expand)))

  #| end of module: SYNTAX-CASE-TRANSFORMER |# )


;;;; module core-macro-transformer: SPLICE-FIRST-EXPAND

(define (splice-first-expand-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's SPLICE-FIRST-EXPAND  syntaxes from
  ;;the top-level  built in environment.   Expand the  syntax object EXPR-STX  in the
  ;;context  of the  given LEXENV;  return  a PSI  struct containing  an instance  of
  ;;"splice-first-envelope".
  ;;
  (syntax-match expr-stx ()
    ((_ ?form)
     (make-psi (let ()
		 (import SPLICE-FIRST-ENVELOPE)
		 (make-splice-first-envelope ?form))))
    ))


;;;; module core-macro-transformer: INTERNAL-BODY

(define (internal-body-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's INTERNAL-BODY  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context  of the  given LEXENV;  return  a PSI  struct containing  an instance  of
  ;;"splice-first-envelope".
  ;;
  (syntax-match input-form.stx ()
    ((_ ?body ?body* ...)
     (chi-internal-body (cons ?body ?body*) lexenv.run lexenv.expand))
    ))


;;;; module core-macro-transformer: UNSAFE

(define (unsafe-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's UNSAFE  macros from  the top-level
  ;;built in  environment.  Expand  the contents  of EXPR-STX in  the context  of the
  ;;given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'unsafe))
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
  ;;Transformer        function         used        to         expand        Vicare's
  ;;PREDICATE-PROCEDURE-ARGUMENT-VALIDATION  macros  from   the  top-level  built  in
  ;;environment.  Expand the contents of EXPR-STX in the context of the given LEXENV;
  ;;return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'predicate-procedure-argument-validation))
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
  ;;Transformer  function used  to expand  Vicare's PREDICATE-RETURN-VALUE-VALIDATION
  ;;macros from the top-level built in  environment.  Expand the contents of EXPR-STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'predicate-return-value-validation))
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
    ;;Transformer function  used to  expand STRUCT-TYPE-DESCRIPTOR syntaxes  from the
    ;;top-level  built in  environment.  Expand  the  syntax object  EXPR-STX in  the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (define-fluid-override __who__
      (identifier-syntax 'struct-type-descriptor))
    (syntax-match expr-stx ()
      ((_ ?type-id)
       (identifier? ?type-id)
       (make-psi (build-data no-source
		   (%struct-type-id->rtd __who__ expr-stx ?type-id lexenv.run))
		 (make-retvals-signature (list (scheme-stx '<struct-type-descriptor>)))))
      ))

  (define (struct-type-and-struct?-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer function used to  expand STRUCT-TYPE-AND-STRUCT?  syntaxes from the
    ;;top-level  built in  environment.  Expand  the  syntax object  EXPR-STX in  the
    ;;context of the given LEXENV; return an PSI struct.
    ;;
    (define-fluid-override __who__
      (identifier-syntax 'struct-type-and-struct?))
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
      ;;Transformer function  used to expand STRUCT-TYPE-FIELD-REF  syntaxes from the
      ;;top-level built  in environment.   Expand the syntax  object EXPR-STX  in the
      ;;context of the given LEXENV; return a PSI struct.
      ;;
      (%struct-type-field-ref-transformer 'struct-type-field-ref #t expr-stx lexenv.run lexenv.expand))

    (define ($struct-type-field-ref-transformer expr-stx lexenv.run lexenv.expand)
      ;;Transformer function used to  expand $STRUCT-TYPE-FIELD-REF syntaxes from the
      ;;top-level built  in environment.   Expand the syntax  object EXPR-STX  in the
      ;;context of the given LEXENV; return a PSI struct.
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
      ;;Transformer function used to expand STRUCT-TYPE-FIELD-SET!  syntaxes from the
      ;;top-level built  in environment.   Expand the syntax  object EXPR-STX  in the
      ;;context of the given LEXENV; return a PSI struct.
      ;;
      (%struct-type-field-set!-transformer 'struct-type-field-ref #t expr-stx lexenv.run lexenv.expand))

    (define ($struct-type-field-set!-transformer expr-stx lexenv.run lexenv.expand)
      ;;Transformer function  used to  expand $STRUCT-TYPE-FIELD-SET!   syntaxes from
      ;;the top-level built in environment.  Expand the syntax object EXPR-STX in the
      ;;context of the given LEXENV; return a PSI struct.
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
    ;;Given the  identifier of  the struct  type: find its  label then  its syntactic
    ;;binding and  return the  struct type  descriptor.  If  no binding  captures the
    ;;identifier or the binding does not  describe a structure type descriptor: raise
    ;;an exception.
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
    ;;Given a list of symbols FIELD-NAMES  representing a struct's field names and an
    ;;identifier FIELD-ID representing  the name of a field: return  the index of the
    ;;selected field in the list.
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
  ;;The syntactic  binding representing  the R6RS record  type descriptor  and record
  ;;constructor descriptor has one of the formats:
  ;;
  ;;   ($rtd . (?rtd-id ?rcd-id))
  ;;   ($rtd . (?rtd-id ?rcd-id . ?spec))
  ;;
  ;;where: "$rtd" is the symbol "$rtd"; ?RTD-ID is the identifier to which the record
  ;;type descriptor is  bound; ?RCD-ID is the identifier to  which the default record
  ;;constructor descriptor is bound; ?SPEC is a record of type R6RS-RECORD-TYPE-SPEC.
  ;;
  (import R6RS-RECORD-TYPE-SPEC)

  (define (record-type-descriptor-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer function  used to  expand RECORD-TYPE-DESCRIPTOR syntaxes  from the
    ;;top-level  built in  environment.  Expand  the  syntax object  EXPR-STX in  the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (define-fluid-override __who__
      (identifier-syntax 'record-type-descriptor))
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
    (define-fluid-override __who__
      (identifier-syntax 'record-constructor-descriptor))
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
  ;;Transformer function used  to expand TYPE-DESCRIPTOR syntaxes  from the top-level
  ;;built in  environment.  Expand the syntax  object EXPR-STX in the  context of the
  ;;given LEXENV; return a PSI struct.
  ;;
  ;;The result must be an expression evaluating to:
  ;;
  ;;* A Vicare  struct type descriptor if  the given identifier argument  is a struct
  ;;  type name.
  ;;
  ;;* A R6RS record type descriptor if the given identifier argument is a record type
  ;;  name.
  ;;
  ;;* An expand-time OBJECT-TYPE-SPEC instance.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'type-descriptor))
  (syntax-match expr-stx ()
    ((_ ?type-id)
     (identifier? ?type-id)
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run binding)
       ((r6rs-record-type)
	(chi-expr (r6rs-record-type-descriptor-binding-rtd binding)
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(make-psi (build-data no-source
		    (syntactic-binding-value binding))
		  (make-retvals-signature (list (scheme-stx '<struct-type-descriptor>)))))
       ((object-type-spec)
	(make-psi (build-data no-source
		    (identifier-object-type-spec ?type-id))
		  (make-single-top-retvals-signature)))
       ))
    ))

(define (is-a?-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand Vicare's IS-A?  syntaxes  from the top-level
  ;;built in  environment.  Expand the syntax  object EXPR-STX in the  context of the
  ;;given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'is-a?))
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
       ((object-type-spec)
	(let ((spec (identifier-object-type-spec ?type-id)))
	  (chi-expr (object-type-spec-pred-stx spec)
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
       ((object-type-spec)
	(let ((spec (identifier-object-type-spec ?type-id)))
	  (chi-expr (bless
		     `(,(object-type-spec-pred-stx spec) ,?expr))
		    lexenv.run lexenv.expand)))
       ))
    ))


;;;; module core-macro-transformer: SLOT-REF, SLOT-SET!, $SLOT-REF, $SLOT-SET!

(define (slot-ref-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's SLOT-REF syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'slot-ref))
  (syntax-match input-form.stx (<>)
    ((_ <> ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run)
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
       ((object-type-spec)
	(chi-expr (tag-identifier-accessor ?type-id ?field-name-id input-form.stx)
		  lexenv.run lexenv.expand))
       ))

    ((_ ?expr ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(record-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(struct-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((object-type-spec)
	(let ((accessor-stx (tag-identifier-accessor ?type-id ?field-name-id input-form.stx)))
	  (chi-expr (bless
		     `(,accessor-stx ,?expr))
		    lexenv.run lexenv.expand)))
       ))

    ;;Missing type identifier.  Try to retrieve the type from the tag of
    ;;the subject.
    ((_ ?id ?field-name-id)
     (and (identifier? ?id)
	  (identifier? ?field-name-id))
     (cond ((identifier-tag ?id)
	    => (lambda (tag-id)
		 (let ((accessor-stx (tag-identifier-accessor tag-id ?field-name-id input-form.stx)))
		   (chi-expr (bless
			      `(,accessor-stx ,?id))
			     lexenv.run lexenv.expand))))
	   (else
	    (stx-error input-form.stx "unable to determine type tag of expression"))))
    ))

(define (slot-set!-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand  Vicare's  SLOT-SET!  syntaxes  from  the
  ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the context
  ;;of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'slot-set!))
  (syntax-match input-form.stx (<>)
    ((_ <> ?field-name-id ?type-id <>)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run)
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
       ((object-type-spec)
	(chi-expr (tag-identifier-mutator ?type-id ?field-name-id input-form.stx)
		  lexenv.run lexenv.expand))
       ))

    ((_ ?expr ?field-name-id ?type-id ?new-value)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(record-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(struct-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((object-type-spec)
	(let ((mutator-stx (tag-identifier-mutator ?type-id ?field-name-id input-form.stx)))
	  (chi-expr (bless
		     `(,mutator-stx ,?expr ,?new-value))
		    lexenv.run lexenv.expand)))
       ))

    ;;Missing type identifier.  Try to retrieve the type from the tag of
    ;;the subject.
    ((_ ?id ?field-name-id ?new-value)
     (and (identifier? ?id)
	  (identifier? ?field-name-id))
     (cond ((identifier-tag ?id)
	    => (lambda (tag-id)
		 (let ((mutator-stx (tag-identifier-mutator tag-id ?field-name-id input-form.stx)))
		   (chi-expr (bless
			      `(,mutator-stx ,?id ,?new-value))
			     lexenv.run lexenv.expand))))
	   (else
	    (stx-error input-form.stx "unable to determine type tag of expression"))))
    ))

(define ($slot-ref-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand  Vicare's  $SLOT-REF  syntaxes  from  the
  ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the context
  ;;of the given LEXENV; return a PSI struct.
  ;;
  (define-constant __who__ '$slot-ref)
  (syntax-match input-form.stx (<>)
    ((_ <> ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run)
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
       ((object-type-spec)
	(chi-expr (tag-identifier-accessor ?type-id ?field-name-id input-form.stx)
		  lexenv.run lexenv.expand))
       ))

    ((_ ?expr ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `($record-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `($struct-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((object-type-spec)
	(let ((accessor-stx (tag-identifier-accessor ?type-id ?field-name-id input-form.stx)))
	  (chi-expr (bless
		     `(,accessor-stx ,?expr))
		    lexenv.run lexenv.expand)))
       ))

    ;;Missing type identifier.  Try to retrieve the type from the tag of
    ;;the subject.
    ((_ ?id ?field-name-id)
     (and (identifier? ?id)
	  (identifier? ?field-name-id))
     (cond ((identifier-tag ?id)
	    => (lambda (tag-id)
		 (let ((accessor-stx (tag-identifier-accessor tag-id ?field-name-id input-form.stx)))
		   (chi-expr (bless
			      `(,accessor-stx ,?id))
			     lexenv.run lexenv.expand))))
	   (else
	    (stx-error input-form.stx "unable to determine type tag of expression"))))
    ))

(define ($slot-set!-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  Vicare's  $SLOT-SET!  syntaxes  from  the
  ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the context
  ;;of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax '$slot-set!))
  (syntax-match input-form.stx (<>)
    ((_ <> ?field-name-id ?type-id <>)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run)
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
       ((object-type-spec)
	(let ((mutator-stx (tag-identifier-mutator ?type-id ?field-name-id input-form.stx)))
	  (chi-expr mutator-stx lexenv.run lexenv.expand)))
       ))

    ((_ ?expr ?field-name-id ?type-id ?new-value)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `($record-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `($struct-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((object-type-spec)
	(let ((mutator-stx (tag-identifier-mutator ?type-id ?field-name-id input-form.stx)))
	  (chi-expr (bless
		     `(,mutator-stx ,?expr ,?new-value))
		    lexenv.run lexenv.expand)))
       ))

    ;;Missing type identifier.  Try to retrieve the type from the tag of
    ;;the subject.
    ((_ ?id ?field-name-id ?new-value)
     (and (identifier? ?id)
	  (identifier? ?field-name-id))
     (cond ((identifier-tag ?id)
	    => (lambda (tag-id)
		 (let ((mutator-stx (tag-identifier-mutator tag-id ?field-name-id input-form.stx)))
		   (chi-expr (bless
			      `(,mutator-stx ,?id ,?new-value))
			     lexenv.run lexenv.expand))))
	   (else
	    (stx-error input-form.stx "unable to determine type tag of expression"))))
    ))


;;;; module core-macro-transformer: TAG-PREDICATE

(define (tag-predicate-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's TAG-PREDICATE  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-predicate))
  (syntax-match input-form.stx ()
    ((_ ?tag)
     (tag-identifier? ?tag)
     (chi-expr (tag-identifier-predicate ?tag input-form.stx) lexenv.run lexenv.expand))
    ))


;;;; module core-macro-transformer: TAG-PROCEDURE-ARGUMENT-VALIDATOR, TAG-RETURN-VALUE-VALIDATOR

(define (tag-procedure-argument-validator-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  TAG-PROCEDURE-ARGUMENT-VALIDATOR
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-procedure-argument-validator))
  (syntax-match input-form.stx (<>)
    ((_ ?tag <>)
     (tag-identifier? ?tag)
     (chi-expr (bless
		`(lambda (obj)
		   (procedure-argument-validation-with-predicate (quote ,?tag) (tag-predicate ,?tag) obj)))
	       lexenv.run lexenv.expand))
    ((_ ?tag ?expr)
     (tag-identifier? ?tag)
     (chi-expr (bless
		`(procedure-argument-validation-with-predicate (quote ,?tag) (tag-predicate ,?tag) ,?expr))
	       lexenv.run lexenv.expand))
    ))

(define (tag-return-value-validator-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to  expand Vicare's TAG-RETURN-VALUE-VALIDATOR syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-return-value-validator))
  (syntax-match input-form.stx (<>)
    ((_ ?tag <>)
     (tag-identifier? ?tag)
     (chi-expr (bless
		`(lambda (obj)
		   (return-value-validation-with-predicate (quote ,?tag) (tag-predicate ,?tag) obj)))
	       lexenv.run lexenv.expand))
    ((_ ?tag ?expr)
     (tag-identifier? ?tag)
     (chi-expr (bless
		`(return-value-validation-with-predicate (quote ,?tag) (tag-predicate ,?tag) ,?expr))
	       lexenv.run lexenv.expand))
    ))


;;;; module core-macro-transformer: TAG-ASSERT

(define (tag-assert-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand Vicare's  TAG-ASSERT  syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-assert))

  (define (%just-evaluate-the-expression expr.psi)
    (let ((expr.core (psi-core-expr expr.psi)))
      (make-psi (build-sequence no-source
		  (list expr.core
			(build-void)))
		;;We know that we are returning a single void argument.
		(make-single-top-retvals-signature))))

  (define (%run-time-check-output-form expr.core checker.psi)
    ;;We build a core language expression as follows:
    ;;
    ;;   (call-with-values
    ;;        (lambda () ?expr)
    ;;     (lambda ?formals
    ;;       ?check-form ...
    ;;       (void)))
    ;;
    (let* ((call.psi     (chi-expr (bless 'call-with-values) lexenv.run lexenv.expand))
	   (call.core    (psi-core-expr call.psi))
	   (checker.core (psi-core-expr checker.psi)))
      (make-psi (build-application no-source
		  call.core
		  (list (build-lambda no-source '() expr.core)
			checker.core))
		;;We know that we are returning a single void argument.
		(make-single-top-retvals-signature))))

  (syntax-match input-form.stx ()
    ((_ ?retvals-signature ?expr)
     (retvals-signature-syntax? ?retvals-signature)
     (let* ((asserted.sign (make-retvals-signature ?retvals-signature))
	    (expr.psi      (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core     (psi-core-expr         expr.psi))
	    (expr.sign     (psi-retvals-signature expr.psi)))
       (cond ((and (identifier?       ?retvals-signature)
		   ($untagged-tag-id? ?retvals-signature)
		   ($top-tag-id?      ?retvals-signature))
	      ;;Any tuple of returned objects is of type "<top>" or "<untagged>".
	      (%just-evaluate-the-expression expr.psi))

	     ((retvals-signature-partially-unspecified? expr.sign)
	      ;;The  expression  has   no  type  specification  or   a  partial  type
	      ;;specification; we have to insert a run-time check.  Here we know that
	      ;;?RETVALS-SIGNATURE is  a valid formals  signature, so we can  be less
	      ;;strict in the patterns.
	      (syntax-match ?retvals-signature ()
		((?rv-tag* ...)
		 (let* ((TMP*         (generate-temporaries ?rv-tag*))
			(checker.psi  (chi-expr (bless
						 `(lambda ,TMP*
						    ,@(map (lambda (tmp tag)
							     `(tag-return-value-validator ,tag ,tmp))
							TMP* ?rv-tag*)
						    (void)))
						lexenv.run lexenv.expand)))
		   (%run-time-check-output-form expr.core checker.psi)))

		((?rv-tag* ... . ?rv-rest-tag)
		 (let* ((TMP*         (generate-temporaries ?rv-tag*))
			(checker.psi  (chi-expr (bless
						 `(lambda (,@TMP* . rest-tmp)
						    ,@(map (lambda (tmp tag)
							     `(tag-return-value-validator ,tag ,tmp))
							TMP* ?rv-tag*)
						    (tag-return-value-validator ,?rv-rest-tag rest-tmp)
						    (void)))
						lexenv.run lexenv.expand)))
		   (%run-time-check-output-form expr.core checker.psi)))

		(?rv-args-tag
		 (let ((checker.psi  (chi-expr (bless
						`(lambda args
						   (tag-return-value-validator ,?rv-args-tag args)
						   (void)))
					       lexenv.run lexenv.expand)))
		   (%run-time-check-output-form expr.core checker.psi)))
		))

	     ((retvals-signature-super-and-sub? asserted.sign expr.sign)
	      ;;Fine, we have established at expand time that the returned values are
	      ;;valid; assertion succeeded.  Just evaluate the expression.
	      (make-psi (build-sequence no-source
			  (list expr.core
				(build-void)))
			;;We know that we are returning a single void value.
			(make-single-top-retvals-signature)))

	     (else
	      ;;The horror!!!  We  have established at expand-time  that the returned
	      ;;values are of the wrong type; assertion failed.
	      (retvals-signature-violation __who__ ?expr asserted.sign expr.sign)))))

    ((_ ?retvals-signature ?expr)
     ;;Let's use a descriptive error message here.
     (syntax-violation __who__
       "invalid return values signature" input-form.stx ?retvals-signature))
    ))


;;;; module core-macro-transformer: TAG-ASSERT-AND-RETURN

(define (tag-assert-and-return-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to  expand Vicare's TAG-ASSERT-AND-RETURN syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-assert-and-return))

  (define* (%run-time-check-output-form {expr.psi psi?} {checker.psi psi?} asserted.sign)
    ;;We build a core language expression as follows:
    ;;
    ;;   (call-with-values
    ;;        (lambda () ?expr)
    ;;     (lambda ?formals
    ;;       ?check-form ...
    ;;       (apply values ?formals)))
    ;;
    ;;The returned PSI struct has the given retvals signature.
    ;;
    (let* ((cwv.psi      (chi-expr (scheme-stx 'call-with-values) lexenv.run lexenv.expand))
	   (cwv.core     (psi-core-expr cwv.psi))
	   (expr.core    (psi-core-expr expr.psi))
	   (checker.core (psi-core-expr checker.psi)))
      (make-psi (build-application no-source
		  cwv.core
		  (list (build-lambda no-source
			  '()
			  expr.core)
			checker.core))
		;;The type  of values  returned by ?EXPR  was unspecified,  but after
		;;asserting  the type  at  run-time: we  know that  the  type is  the
		;;asserted one.
		asserted.sign)))

  (syntax-match input-form.stx ()
    ((_ ?retvals-signature ?expr)
     (retvals-signature-syntax? ?retvals-signature)
     (let* ((asserted.sign (make-retvals-signature ?retvals-signature))
	    (expr.psi      (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core     (psi-core-expr  expr.psi))
	    (expr.sign     (psi-retvals-signature expr.psi)))
       (cond ((and (identifier?       ?retvals-signature)
		   ($untagged-tag-id? ?retvals-signature)
		   ($top-tag-id?      ?retvals-signature))
	      ;;Any tuple  of returned  objects is of  type "<top>"  or "<untagged>".
	      ;;Just return the expression.
	      expr.psi)

	     ((retvals-signature-partially-unspecified? expr.sign)
	      ;;The  expression  has no  type  specification;  we  have to  insert  a
	      ;;run-time check.  Here  we know that ASSERTED.SIGN is  a valid formals
	      ;;signature, so we can be less strict in the patterns.
	      (syntax-match ?retvals-signature ()
		((?rv-tag* ...)
		 (let* ((TMP*         (generate-temporaries ?rv-tag*))
			(checker.psi  (chi-expr (bless
						 `(lambda ,TMP*
						    ,@(map (lambda (tmp tag)
							     `(tag-return-value-validator ,tag ,tmp))
							TMP* ?rv-tag*)
						    (values . ,TMP*)))
						lexenv.run lexenv.expand)))
		   (%run-time-check-output-form expr.psi checker.psi asserted.sign)))

		((?rv-tag* ... . ?rv-rest-tag)
		 (let* ((TMP*         (generate-temporaries ?rv-tag*))
			(checker.psi  (chi-expr (bless
						 `(lambda (,@TMP* . rest-tmp)
						    ,@(map (lambda (tmp tag)
							     `(tag-return-value-validator ,tag ,tmp))
							TMP* ?rv-tag*)
						    (tag-return-value-validator ,?rv-rest-tag rest-tmp)
						    (apply values ,@TMP* rest-tmp)))
						lexenv.run lexenv.expand)))
		   (%run-time-check-output-form expr.psi checker.psi asserted.sign)))

		(?rv-args-tag
		 (let ((checker.psi  (chi-expr (bless
						`(lambda args
						   (tag-return-value-validator ,?rv-args-tag args)
						   (apply values args)))
					       lexenv.run lexenv.expand)))
		   (%run-time-check-output-form expr.psi checker.psi asserted.sign)))
		))

	     ((retvals-signature-super-and-sub? asserted.sign expr.sign)
	      ;;Fine, we have established at expand time that the returned values are
	      ;;valid; assertion succeeded.  Just evaluate the expression.
	      expr.psi)

	     (else
	      ;;The horror!!!  We  have established at expand-time  that the returned
	      ;;values are of the wrong type; assertion failed.
	      (retvals-signature-violation __who__ ?expr asserted.sign expr.sign)))))

    ((_ ?retvals-signature ?expr)
     ;;Let's use a descriptive error message here.
     (syntax-violation __who__
       "invalid return values signature" input-form.stx ?retvals-signature))
    ))


;;;; module core-macro-transformer: TAG-ACCESSOR, TAG-MUTATOR

(define (tag-accessor-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  TAG-ACCESSOR  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-accessor))
  (syntax-match input-form.stx ()
    ((_ ?expr ?field-name-id)
     (identifier? ?field-name-id)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sign (psi-retvals-signature expr.psi)))
       (if (retvals-signature-fully-unspecified? expr.sign)
	   (syntax-violation __who__ "unable to determine tag of expression" input-form.stx)
	 (syntax-match (retvals-signature-tags expr.sign) ()
	   ((?tag)
	    (let* ((accessor.stx  (tag-identifier-accessor ?tag ?field-name-id input-form.stx))
		   (accessor.psi  (chi-expr accessor.stx lexenv.run lexenv.expand))
		   (accessor.core (psi-core-expr accessor.psi)))
	      (make-psi (build-application (syntax-annotation input-form.stx)
			  accessor.core
			  (list expr.core))
			(psi-application-retvals-signature accessor.psi))))

	   (_
	    (syntax-violation __who__ "invalid expression retvals signature" input-form.stx expr.sign))
	   ))))
    ))

(define (tag-mutator-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  Vicare's  TAG-MUTATOR syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-mutator))
  (syntax-match input-form.stx ()
    ((_ ?expr ?field-name-id ?new-value)
     (identifier? ?field-name-id)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.sign (psi-retvals-signature expr.psi))
	    (nval.psi  (chi-expr ?new-value lexenv.run lexenv.expand)))
       (if (retvals-signature-fully-unspecified? expr.sign)
	   (syntax-violation __who__ "unable to determine tag of expression" input-form.stx)
	 (syntax-match (retvals-signature-tags expr.sign) ()
	   ((?tag)
	    (let* ((mutator.stx  (tag-identifier-mutator ?tag ?field-name-id input-form.stx))
		   (mutator.psi  (chi-expr mutator.stx lexenv.run lexenv.expand))
		   (mutator.core (psi-core-expr mutator.psi))
		   (expr.core    (psi-core-expr expr.psi))
		   (nval.core    (psi-core-expr nval.psi)))
	      (make-psi (build-application (syntax-annotation input-form.stx)
			  mutator.core
			  (list expr.core nval.core))
			(psi-application-retvals-signature mutator.psi))))

	   (_
	    (syntax-violation __who__ "invalid expression retvals signature" input-form.stx expr.sign))
	   ))))
    ))


;;;; module core-macro-transformer: TAG-GETTER, TAG-SETTER

(define (tag-getter-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand Vicare's  TAG-GETTER  syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-getter))
  (define (%generate-output-form expr.stx keys.stx)
    (let* ((expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand))
	   (expr.sign (psi-retvals-signature expr.psi)))
      (if (retvals-signature-fully-unspecified? expr.sign)
	  (syntax-violation __who__ "unable to determine tag of expression" input-form.stx)
	(syntax-match (retvals-signature-tags expr.sign) ()
	  ((?tag)
	   (let* ((getter.stx  (tag-identifier-getter ?tag keys.stx input-form.stx))
		  (getter.psi  (chi-expr getter.stx lexenv.run lexenv.expand))
		  (getter.core (psi-core-expr getter.psi))
		  (expr.core   (psi-core-expr expr.psi)))
	     (make-psi (build-application (syntax-annotation input-form.stx)
			 getter.core
			 (list expr.core))
		       (psi-application-retvals-signature getter.psi))))

	  (_
	   (syntax-violation __who__ "invalid expression retvals signature" input-form.stx expr.sign))
	  ))))
  (syntax-match input-form.stx ()
    ((_ ?expr ((?key00 ?key0* ...) (?key11* ?key1** ...) ...))
     (%generate-output-form ?expr (cons (cons ?key00 ?key0*) (map cons ?key11* ?key1**))))
    ((_ ?expr (?key00 ?key0* ...) (?key11* ?key1** ...) ...)
     (%generate-output-form ?expr (cons (cons ?key00 ?key0*) (map cons ?key11* ?key1**))))
    ))

(define (tag-setter-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand Vicare's  TAG-SETTER  syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-setter))
  (define (%generate-output-form expr.stx keys.stx new-value.stx)
    (let* ((expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand))
	   (expr.sign (psi-retvals-signature expr.psi)))
      (if (retvals-signature-fully-unspecified? expr.sign)
	  (syntax-violation __who__ "unable to determine tag of expression" input-form.stx)
	(syntax-match (retvals-signature-tags expr.sign) ()
	  ((?tag)
	   (let* ((setter.stx  (tag-identifier-setter ?tag keys.stx input-form.stx))
		  (setter.psi  (chi-expr setter.stx lexenv.run lexenv.expand))
		  (setter.core (psi-core-expr setter.psi))
		  (expr.core   (psi-core-expr expr.psi))
		  (nval.psi    (chi-expr new-value.stx lexenv.run lexenv.expand))
		  (nval.core   (psi-core-expr nval.psi)))
	     (make-psi (build-application (syntax-annotation input-form.stx)
			 setter.core
			 (list expr.core nval.core))
		       (psi-application-retvals-signature setter.psi))))

	  (_
	   (syntax-violation __who__ "invalid expression retvals signature" input-form.stx expr.sign))
	  ))))
  (syntax-match input-form.stx ()
    ((_ ?expr ((?key00 ?key0* ...) (?key11* ?key1** ...) ...) ?new-value)
     (%generate-output-form ?expr (cons (cons ?key00 ?key0*) (map cons ?key11* ?key1**)) ?new-value))
    ((_ ?expr (?key00 ?key0* ...) (?key11* ?key1** ...) ... ?new-value)
     (%generate-output-form ?expr (cons (cons ?key00 ?key0*) (map cons ?key11* ?key1**)) ?new-value))
    ))


;;;; module core-macro-transformer: TAG-DISPATCH

(define (tag-dispatch-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  TAG-DISPATCH  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-dispatch))
  (syntax-match input-form.stx ()
    ((_ ?expr ?member ?arg* ...)
     (identifier? ?member)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.sign (psi-retvals-signature expr.psi)))
      (if (retvals-signature-fully-unspecified? expr.sign)
	  (syntax-violation __who__ "unable to determine tag of expression" input-form.stx)
	(syntax-match (retvals-signature-tags expr.sign) ()
	  ((?tag)
	   (let* ((method.stx  (tag-identifier-dispatch ?tag ?member ?arg* input-form.stx))
		  (method.psi  (chi-expr method.stx lexenv.run lexenv.expand))
		  (method.core (psi-core-expr method.psi))
		  (expr.core   (psi-core-expr expr.psi)))
	     (make-psi (build-application (syntax-annotation input-form.stx)
			 method.core
			 (list expr.core))
		       (psi-application-retvals-signature method.psi))))

	  (_
	   (syntax-violation __who__ "invalid expression retvals signature" input-form.stx expr.sign))
	  ))))
    ))


;;;; module core-macro-transformer: TAG-CAST

(define (tag-cast-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's TAG-CAST syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-cast))

  (define (%retrieve-caster-maker target-tag)
    (cond ((identifier-object-type-spec target-tag)
	   => (lambda (spec)
		($object-type-spec-caster-maker spec)))
	  (else
	   (stx-internal-error input-form.stx "tag identifier without object type spec"))))

  (define (%cast-at-run-time-with-generic-transformer target-tag expr.psi)
    ;;When the  type of the expression  is unknown at expand-time:  insert a run-time
    ;;expression that will  try to convert whatever source value  into a target value
    ;;of the requested type.
    ;;
    (cond ((%retrieve-caster-maker target-tag)
	   => (lambda (target-caster-maker)
		(let* ((caster.stx   (target-caster-maker #f input-form.stx))
		       (caster.psi   (chi-expr caster.stx lexenv.run lexenv.expand))
		       (caster.core  (psi-core-expr caster.psi))
		       (expr.core    (psi-core-expr expr.psi)))
		  ;;This form  will either succeed or  raise an exception, so  we can
		  ;;tag this PSI with the target tag.
		  (make-psi (build-application no-source
			      caster.core
			      (list expr.core))
			    (make-retvals-signature (list target-tag))))))
	  (else
	   (%validate-and-return target-tag expr.psi))))

  (define (%validate-and-return target-tag expr.psi)
    ;;When the  source tag  is unknown  or incompatible  with the  target tag  and no
    ;;transformer  function was  found  for  the target  tag:  we  insert a  run-time
    ;;expression that validates and returns the value.
    ;;
    (let* ((expr.core       (psi-core-expr expr.psi))
  	   (type-name.core  (build-data no-source
			      (syntax->datum target-tag)))
  	   (predicate.psi   (chi-expr (tag-identifier-predicate target-tag input-form.stx)
				      lexenv.run lexenv.expand))
  	   (predicate.core  (psi-core-expr predicate.psi)))
      ;;This form will either  succeed or raise an exception, so we  can tag this PSI
      ;;with the target tag.
      (make-psi (build-application no-source
		  (build-primref no-source 'return-value-validation-with-predicate)
  		  (list type-name.core predicate.core expr.core))
  		(make-retvals-signature (list target-tag)))))

  (syntax-match input-form.stx ()
    ((_ ?target-tag ?expr)
     (tag-identifier? ?target-tag)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sign (psi-retvals-signature expr.psi)))
      (if (retvals-signature-fully-unspecified? expr.sign)
	  (%cast-at-run-time-with-generic-transformer ?target-tag expr.psi)
	(syntax-match (retvals-signature-tags expr.sign) ()
	  ((?source-tag)
	   (cond (($untagged-tag-id? ?source-tag)
		  (%cast-at-run-time-with-generic-transformer ?target-tag expr.psi))
		 ((tag-super-and-sub? ?target-tag ?source-tag)
		  ;;The expression  already has the  right type: nothing to  do, just
		  ;;return it.
		  expr.psi)
		 (else
		  ;;The tag  of expression  is incompatible  with the  requested tag.
		  ;;Try to select an appropriate caster operator.
		  (cond ((%retrieve-caster-maker ?target-tag)
			 => (lambda (target-caster-maker)
			      (let* ((caster.stx   (target-caster-maker ?source-tag input-form.stx))
				     (caster.psi   (chi-expr caster.stx lexenv.run lexenv.expand))
				     (caster.core  (psi-core-expr caster.psi)))
				(make-psi (build-application (syntax-annotation input-form.stx)
					    caster.core
					    (list expr.core))
					  (make-retvals-signature (list ?target-tag))))))
			(else
			 (%validate-and-return ?target-tag expr.psi))))))

	  (_
	   (syntax-violation __who__ "invalid expression retvals signature" input-form.stx expr.sign))
	  ))))
    ))


;;;; done

;;; end of file
;;Local Variables:
;;mode: vicare
;;fill-column: 85
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
