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

    ((tag-assert)				tag-assert-transformer)
    ((tag-assert-and-return)			tag-assert-and-return-transformer)

    (else
     (assertion-violation __who__
       "Vicare: internal error: cannot find transformer" name))))


;;;; module core-macro-transformer: IF

(define (if-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  R6RS  IF syntaxes  from  the
  ;;top-level built  in environment.  Expand the  syntax object EXPR-STX
  ;;in  the context  of the  given LEXENV;  return an  expanded language
  ;;symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ ?test ?consequent ?alternate)
     (build-conditional no-source
       (chi-expr ?test       lexenv.run lexenv.expand)
       (chi-expr ?consequent lexenv.run lexenv.expand)
       (chi-expr ?alternate  lexenv.run lexenv.expand)))
    ((_ ?test ?consequent)
     (build-conditional no-source
       (chi-expr ?test       lexenv.run lexenv.expand)
       (chi-expr ?consequent lexenv.run lexenv.expand)
       (build-void)))
    ))


;;;; module core-macro-transformer: QUOTE

(define (quote-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand R6RS  QUOTE syntaxes  from the
  ;;top-level built  in environment.  Expand the  syntax object EXPR-STX
  ;;in  the context  of the  given LEXENV;  return an  expanded language
  ;;symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ ?datum)
     (build-data no-source
       (syntax->datum ?datum)))))


;;;; module core-macro-transformer: LAMBDA and CASE-LAMBDA

(define (case-lambda-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand R6RS  CASE-LAMBDA syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ (?formals* ?body* ?body** ...) ...)
     (receive (formals* body*)
	 (chi-lambda-clause* expr-stx ?formals*
			     (map cons ?body* ?body**)
			     lexenv.run lexenv.expand)
       (build-case-lambda (syntax-annotation expr-stx)
	 formals* body*)))))

(define (lambda-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  R6RS LAMBDA syntaxes  from the
  ;;top-level built  in environment.  Expand the  syntax object EXPR-STX
  ;;in  the context  of the  given LEXENV;  return an  expanded language
  ;;symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?body ?body* ...)
     (receive (formals body)
	 (chi-lambda-clause expr-stx ?formals
			    (cons ?body ?body*)
			    lexenv.run lexenv.expand)
       (build-lambda (syntax-annotation expr-stx)
	 formals body)))))


;;;; module core-macro-transformer: LETREC and LETREC*

(module (letrec-transformer letrec*-transformer)

  (define (letrec-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer  function  used to  expand  LETREC  syntaxes from  the
    ;;top-level built in environment.  Expand the syntax object EXPR-STX
    ;;in the  context of the  given LEXENV; return an  expanded language
    ;;symbolic expression.
    ;;
    (%letrec-helper expr-stx lexenv.run lexenv.expand build-letrec))

  (define (letrec*-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer  function used  to  expand LETREC*  syntaxes from  the
    ;;top-level built in environment.  Expand the syntax object EXPR-STX
    ;;in the  context of the  given LEXENV; return an  expanded language
    ;;symbolic expression.
    ;;
    (%letrec-helper expr-stx lexenv.run lexenv.expand build-letrec*))

  (define (%letrec-helper expr-stx lexenv.run lexenv.expand core-lang-builder)
    (syntax-match expr-stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check  that  the  binding  names are  identifiers  and  without
       ;;duplicates.
       (receive (lhs* tag*)
	   (parse-tagged-bindings-syntax ?lhs* expr-stx)
	 ;;Generate  unique variable  names  and labels  for the  LETREC
	 ;;bindings.
	 (let ((lex* (map gensym-for-lexical-var lhs*))
	       (lab* (map gensym-for-label       lhs*)))
	   (map (lambda (label tag)
		  (and tag (set-label-type-tagging! label tag)))
	     lab* tag*)
	   ;;Generate  what is  needed to  create a  lexical contour:  a
	   ;;<RIB>  and  an extended  lexical  environment  in which  to
	   ;;evaluate both the right-hand sides and the body.
	   ;;
	   ;;Notice that the region of  all the LETREC bindings includes
	   ;;all the right-hand sides.
	   (let ((rib		(make-filled-rib lhs* lab*))
		 (lexenv.run	(add-lexical-bindings lab* lex* lexenv.run)))
	     ;;Create  the   lexical  contour  then  process   body  and
	     ;;right-hand sides of bindings.
	     (let ((body (chi-internal-body (push-lexical-contour rib
					      (cons ?body ?body*))
					    lexenv.run lexenv.expand))
		   (rhs* (chi-expr*         (map (lambda (rhs)
						   (push-lexical-contour rib rhs))
					      ?rhs*)
					    lexenv.run lexenv.expand)))
	       ;;Build  the LETREC  or  LETREC* expression  in the  core
	       ;;language.
	       (core-lang-builder no-source lex* rhs* body))))))
      ))

  #| end of module |# )


;;;; module core-macro-transformer: FLUID-LET-SYNTAX

(define (fluid-let-syntax-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand FLUID-LET-SYNTAX  syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  ;;FLUID-LET-SYNTAX is  similar, but  not equal, to  LET-SYNTAX; rather
  ;;than defining new ?LHS bindings, it temporarily rebinds the keywords
  ;;to new transformers while expanding the ?BODY forms.  The given ?LHS
  ;;must   be    already   bound   to   fluid    syntaxes   defined   by
  ;;DEFINE-FLUID-SYNTAX.
  ;;
  ;;There are  two differences between FLUID-LET-SYNTAX  and LET-SYNTAX:
  ;;FLUID-LET-SYNTAX  must  appear  in   expression  context  only;  the
  ;;internal ?BODY forms are *not* spliced in the enclosing body.
  ;;
  ;;NOTE  We would  truly like  to splice  the inner  body forms  in the
  ;;surrounding body,  so that  this syntax  could act  like LET-SYNTAX,
  ;;which is useful; but we really cannot do it with this implementation
  ;;of the expander algorithm.  This  is because LET-SYNTAX both creates
  ;;a  new  rib  and  adds  new  id/label  entries  to  it,  and  pushes
  ;;label/descriptor  entries to  the  LEXENV; instead  FLUID-LET-SYNTAX
  ;;only pushes entries to the LEXENV: there is no way to keep the fluid
  ;;LEXENV entries  visible only to  a subsequence  of forms in  a body.
  ;;(Marco Maggi; Tue Feb 18, 2014)
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
    ;;Search the binding of the  identifier LHS retrieving its label; if
    ;;such  label  is  present  and  its  associated  syntactic  binding
    ;;descriptor from LEXENV.RUN  is of type "fluid  syntax": return the
    ;;associated fluid label that can be used to rebind the identifier.
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
  ;;Transformer function  used to expand Vicare's  FOREIGN-CALL syntaxes
  ;;from the top-level  built in environment.  Expand  the syntax object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name ?arg* ...)
     (build-foreign-call no-source
       (chi-expr  ?name lexenv.run lexenv.expand)
       (chi-expr* ?arg* lexenv.run lexenv.expand)))))


;;;; module core-macro-transformer: SYNTAX

(module (syntax-transformer)
  ;;Transformer function used to expand  R6RS's SYNTAX syntaxes from the
  ;;top-level built in environment.  Process  the contents of USE-STX in
  ;;the   context   of   the   lexical   environments   LEXENV.RUN   and
  ;;LEXENV.EXPAND.
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
  ;;in  which:  ?DATUM  is  a literal  datum,  ?PATTERN-VARIABLE  is  an
  ;;identifier referencing  a pattern  variable created  by SYNTAX-CASE,
  ;;?ID   is  an   identifier  not   referencing  a   pattern  variable,
  ;;?SUBTEMPLATE  is  a  template  followed by  zero  or  more  ellipsis
  ;;identifiers.
  ;;
  ;;Return a sexp representing  code in the core language
  ;;which, when evaluated, returns a  wrapped or unwrapped syntax object
  ;;containing an expression in which:
  ;;
  ;;* All the template identifiers being references to pattern variables
  ;;  are substituted with the corresponding syntax objects.
  ;;
  ;;     (syntax-case #'123 (?obj (syntax ?obj)))
  ;;     => #<syntax expr=123>
  ;;
  ;;     (syntax-case #'(1 2) ((?a ?b) (syntax #(?a ?b))))
  ;;     => #(#<syntax expr=1> #<syntax expr=1>)
  ;;
  ;;* All the identifiers not  being references to pattern variables are
  ;;  left  alone to  be captured  by the lexical  context at  the level
  ;;  below the current,  in the context of the SYNTAX  macro use or the
  ;;  context of the output form.
  ;;
  ;;     (syntax-case #'(1) ((?a) (syntax (display ?b))))
  ;;     => (#<syntax expr=display>
  ;;         #<syntax expr=1> . #<syntax expr=()>)
  ;;
  ;;* All the sub-templates followed by ellipsis are replicated to match
  ;;  the input pattern.
  ;;
  ;;     (syntax-case #'(1 2 3) ((?a ...) (syntax #(?a ...))))
  ;;     => #(1 2 3)
  ;;
  ;;About pattern variables:  they are present in  a lexical environment
  ;;as entries with format:
  ;;
  ;;   (?label . (syntax . (?name . ?level)))
  ;;
  ;;where:  ?LABEL  is the  label  in  the identifier's  syntax  object,
  ;;"syntax" is  the symbol "syntax",  ?NAME is the  symbol representing
  ;;the  name  of the  pattern  variable,  ?LEVEL  is an  exact  integer
  ;;representing the  nesting ellipsis level.  The  SYNTAX-CASE patterns
  ;;below will generate the given entries:
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
  ;;The  input template  is  first visited  in  post-order, building  an
  ;;intermediate  symbolic  representation  of  it;  then  the  symbolic
  ;;representation is visited in post-order, building core language code
  ;;that  evaluates  to  the   resulting  syntax  object.   Examples  of
  ;;intermediate  representation  (-->)  and  expansion  (==>)  follows,
  ;;assuming identifiers starting with "?"  are pattern variables:
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
	   code)))))

  (define (%gen-syntax use-stx template-stx lexenv maps ellipsis? vec?)
    ;;Recursive function.  Expand the contents of a SYNTAX use.
    ;;
    ;;USE-STX must be  the syntax object containing  the original SYNTAX
    ;;macro use; it is used for descriptive error reporting.
    ;;
    ;;TEMPLATE-STX must be the template from the SYNTAX macro use.
    ;;
    ;;LEXENV is  the lexical  environment in  which the  expansion takes
    ;;place;  it must  contain  the pattern  variables  visible by  this
    ;;SYNTAX use.
    ;;
    ;;MAPS is  a list  of alists,  one alist  for each  ellipsis nesting
    ;;level.  If the template has 3 nested ellipsis patterns:
    ;;
    ;;   (((?a ...) ...) ...)
    ;;
    ;;while  we are  processing the  inner "(?a  ...)"  MAPS  contains 3
    ;;alists.  The  alists are  used when processing  ellipsis templates
    ;;that recursively reference the same pattern variable, for example:
    ;;
    ;;   ((?a (?a ...)) ...)
    ;;
    ;;the inner  ?A is mapped  to a gensym which  is used to  generate a
    ;;binding in the output code.
    ;;
    ;;ELLIPSIS? must be a predicate function returning true when applied
    ;;to the  ellipsis identifier from  the built in  environment.  Such
    ;;function  is made  an argument,  so that  it can  be changed  to a
    ;;predicate  returning   always  false   when  we   are  recursively
    ;;processing a quoted template:
    ;;
    ;;   (... ?sub-template)
    ;;
    ;;in which the ellipses in ?SUB-TEMPLATE are to be handled as normal
    ;;identifiers.
    ;;
    ;;VEC? is a boolean: true when this function is processing the items
    ;;of a vector.
    ;;
    (syntax-match template-stx ()

      ;;Standalone ellipses are not allowed.
      ;;
      (?dots
       (ellipsis? ?dots)
       (stx-error use-stx "misplaced ellipsis in syntax form"))

      ;;Match  a standalone  identifier.   ?ID can  be:  a reference  to
      ;;pattern variable created by SYNTAX-CASE; an identifier that will
      ;;be captured by  some binding; an identifier that  will result to
      ;;be free,  in which  case an "unbound  identifier" error  will be
      ;;raised later.
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
      ;;but ellipses  starting a list  template are allowed,  they quote
      ;;the subsequent sub-template:
      ;;
      ;;   (... ...)		==> quoted ellipsis
      ;;   (... ?sub-template)	==> quoted ?SUB-TEMPLATE
      ;;
      ;;so that the ellipses in  the ?SUB-TEMPLATE are treated as normal
      ;;identifiers.  We change the  ELLIPSIS? argument for recursion to
      ;;a predicate that always returns false.
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

      ;;Process a vector template.  We set to true the VEC? argument for
      ;;recursion.
      ;;
      (#(?item* ...)
       (receive (item*.new maps)
	   (%gen-syntax use-stx ?item* lexenv maps ellipsis? #t)
	 (values (%gen-vector template-stx ?item* item*.new)
		 maps)))

      ;;Everything else is just quoted in the output.  This includes all
      ;;the literal datums.
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
  ;;Transformer function used to expand R6RS's SYNTAX-CASE syntaxes from
  ;;the top-level built in environment.  Process the contents of USE-STX
  ;;in  the   context  of   the  lexical  environments   LEXENV.RUN  and
  ;;LEXENV.EXPAND.
  ;;
  ;;Notice  that   the  parsing   of  the   patterns  is   performed  by
  ;;CONVERT-PATTERN at  expand time and  the actual pattern  matching is
  ;;performed by SYNTAX-DISPATCH at run time.
  ;;
  (define (syntax-case-transformer use-stx lexenv.run lexenv.expand)
    (syntax-match use-stx ()
      ((_ ?expr (?literal* ...) ?clauses* ...)
       (%verify-literals ?literal* use-stx)
       (let* ( ;;The identifier to  which the result of evaluating the
	      ;;?EXPR is bound.
	      (expr.id    (gensym-for-lexical-var 'tmp))
	      ;;The full SYNTAX-CASE  pattern matching code, generated
	      ;;and transformed to core language.
	      (body.core  (%gen-syntax-case expr.id ?literal* ?clauses*
					    lexenv.run lexenv.expand))
	      ;;The ?EXPR transformed to core language.
	      (expr.core  (chi-expr ?expr lexenv.run lexenv.expand)))
	 ;;Return a form like:
	 ;;
	 ;;   ((lambda (expr.id) body.core) expr.core)
	 ;;
	 (build-application no-source
	   (build-lambda no-source (list expr.id) body.core)
	   (list expr.core))))
      ))

  (define (%gen-syntax-case expr.id literals clauses lexenv.run lexenv.expand)
    ;;Recursive function.  Generate and return the full pattern matching
    ;;code in the core language to match the given CLAUSES.
    ;;
    (syntax-match clauses ()
      ;;No pattern matched the input  expression: return code to raise a
      ;;syntax error.
      ;;
      (()
       (build-application no-source
	 (build-primref no-source 'syntax-error)
	 (list (build-lexical-reference no-source expr.id))))

      ;;The pattern  is a standalone  identifier, neither a  literal nor
      ;;the ellipsis,  and it  has no  fender.  A  standalone identifier
      ;;with no fender matches everything,  so it is useless to generate
      ;;the code  for the next clauses:  the code generated here  is the
      ;;last one.
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
	   ;;the underscore  identifier matches everything and  binds no
	   ;;pattern variables.
	   (chi-expr ?output-expr lexenv.run lexenv.expand)
	 ;;The clause is:
	 ;;
	 ;;   (?id ?output-expr)
	 ;;
	 ;;a standalone identifier matches everything  and binds it to a
	 ;;pattern variable whose name is ?ID.
	 (let ((label (gensym-for-label ?pattern))
	       (lex   (gensym-for-lexical-var ?pattern)))
	   ;;The expression  must be  expanded in a  lexical environment
	   ;;augmented with the pattern variable.
	   (define output-expr^
	     (push-lexical-contour
		 (make-filled-rib (list ?pattern) (list label))
	       ?output-expr))
	   (define lexenv.run^
	     ;;Push a pattern variable entry to the lexical environment.
	     ;;The ellipsis nesting level is 0.
	     (cons (cons label (make-binding 'syntax (cons lex 0)))
		   lexenv.run))
	   (define output-expr.core
	     (chi-expr output-expr^ lexenv.run^ lexenv.expand))
	   (build-application no-source
	     (build-lambda no-source
	       (list lex)
	       output-expr.core)
	     (list (build-lexical-reference no-source expr.id))))))

      ;;The  pattern is  neither  a standalone  pattern  variable nor  a
      ;;standalone underscore.  It has no fender, which is equivalent to
      ;;having a "#t" as fender.
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
    ;;Generate  the  code needed  to  match  the clause  represented  by
    ;;PATTERN.STX, FENDER.STX and  OUTPUT-EXPR.STX; recursively generate
    ;;the code to match the other clauses in NEXT-CLAUSES.
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
    ;;notice that the  return value of SYNTAX-DISPATCH is:  false if the
    ;;pattern did not match, otherwise the list of values to be bound to
    ;;the pattern variables.
    ;;
    (receive (pattern.dispatch pvars.levels)
	;;CONVERT-PATTERN  return 2  values: the  pattern in  the format
	;;accepted by SYNTAX-DISPATCH, an alist representing the pattern
	;;variables:
	;;
	;;* The keys of the alist are identifiers representing the names
	;;  of the pattern variables.
	;;
	;;*  The values  of the  alist are  non-negative exact  integers
	;;  representing the ellipsis nesting level of the corresponding
	;;  pattern variable.  See SYNTAX-TRANSFORMER for details.
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
    ;;Generate the  code that tests  the fender: if the  fender succeeds
    ;;run the output expression, else try to match the next clauses.
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
    ;;Generate  code to  evaluate EXPR.STX  in an  environment augmented
    ;;with the pattern variables defined by PVARS.LEVELS.  Return a core
    ;;language expression representing the following pseudo-code:
    ;;
    ;;   (apply (lambda (pattern-var ...) expr) tmp)
    ;;
    (define ids
      ;;For each pattern variable: the identifier representing its name.
      (map car pvars.levels))
    (define labels
      ;;For each pattern variable: a gensym used as label in the lexical
      ;;environment.
      (map gensym-for-label ids))
    (define names
      ;;For each pattern variable: a gensym used as unique variable name
      ;;in the lexical environment.
      (map gensym-for-lexical-var ids))
    (define levels
      ;;For  each pattern  variable: an  exact integer  representing the
      ;;ellipsis nesting level.  See SYNTAX-TRANSFORMER for details.
      (map cdr pvars.levels))
    (define bindings
      ;;For each pattern variable: a binding to be pushed on the lexical
      ;;environment.
      (map (lambda (label name level)
	     (cons label (make-binding 'syntax (cons name level))))
	labels names levels))
    (define expr.core
      ;;Expand the  expression in  a lexical environment  augmented with
      ;;the pattern variables.
      ;;
      ;;NOTE We could have created a syntax object:
      ;;
      ;;  #`(lambda (pvar ...) #,expr.stx)
      ;;
      ;;and then  expanded it:  EXPR.STX would have  been expanded  in a
      ;;lexical environment augmented with the PVAR bindings.
      ;;
      ;;Instead we have chosen to push  the PVAR bindings on the lexical
      ;;environment "by hand", then to  expand EXPR.STX in the augmented
      ;;environment,  finally   to  put  the  resulting   core  language
      ;;expression in a core language LAMBDA syntax.
      ;;
      ;;The two methods are fully equivalent;  the one we have chosen is
      ;;a bit faster.
      ;;
      (chi-expr (push-lexical-contour
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

  #| end of module: SYNTAX-CASE-TRANSFORMER |# )


;;;; module core-macro-transformer: SPLICE-FIRST-EXPAND

(define (splice-first-expand-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  Vicare's SPLICE-FIRST-EXPAND
  ;;syntaxes  from the  top-level  built in  environment.  Expand  the
  ;;syntax object EXPR-STX in the  context of the given LEXENV; return
  ;;an expanded language symbolic expression.
  ;;
  (import SPLICE-FIRST-ENVELOPE)
  (syntax-match expr-stx ()
    ((_ ?form)
     (make-splice-first-envelope ?form))
    ))


;;;; module core-macro-transformer: UNSAFE

(define (unsafe-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  Vicare's UNSAFE macros from the
  ;;top-level built in environment.  Expand  the contents of EXPR-STX in
  ;;the  context  of  the  given LEXENV;  return  an  expanded  language
  ;;symbolic expression.
  ;;
  (define-constant __who__
    'unsafe)
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
  ;;Transformer      function      used     to      expand      Vicare's
  ;;PREDICATE-PROCEDURE-ARGUMENT-VALIDATION  macros  from the  top-level
  ;;built  in  environment.  Expand  the  contents  of EXPR-STX  in  the
  ;;context of  the given LEXENV;  return an expanded  language symbolic
  ;;expression.
  ;;
  (define-constant __who__
    'predicate-procedure-argument-validation)
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
  ;;Transformer      function      used     to      expand      Vicare's
  ;;PREDICATE-RETURN-VALUE-VALIDATION macros from the top-level built in
  ;;environment.  Expand the contents of  EXPR-STX in the context of the
  ;;given LEXENV; return an expanded language symbolic expression.
  ;;
  (define-constant __who__
    'predicate-return-value-validation)
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
    ;;Transformer   function  used   to  expand   STRUCT-TYPE-DESCRIPTOR
    ;;syntaxes  from the  top-level  built in  environment.  Expand  the
    ;;syntax object EXPR-STX in the  context of the given LEXENV; return
    ;;an expanded language symbolic expression.
    ;;
    ;;FIXME This transformer is  currently unused because the identifier
    ;;STRUCT-TYPE-DESCRIPTOR  is bound  to  a function.   In future  the
    ;;function  binding  will be  removed  and  replaced by  the  syntax
    ;;binding.  (Marco Maggi; Fri Jan 31, 2014)
    ;;
    (define-constant __who__ 'struct-type-descriptor)
    (syntax-match expr-stx ()
      ((_ ?type-id)
       (identifier? ?type-id)
       (build-data no-source
	 (%struct-type-id->rtd __who__ expr-stx ?type-id lexenv.run)))
      ))

  (define (struct-type-and-struct?-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer  function   used  to   expand  STRUCT-TYPE-AND-STRUCT?
    ;;syntaxes  from the  top-level  built in  environment.  Expand  the
    ;;syntax object EXPR-STX in the  context of the given LEXENV; return
    ;;an expanded language symbolic expression.
    ;;
    (define-constant __who__ 'struct-type-and-struct?)
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
      ;;Transformer  function   used  to   expand  STRUCT-TYPE-FIELD-REF
      ;;syntaxes from  the top-level  built in environment.   Expand the
      ;;syntax  object EXPR-STX  in  the context  of  the given  LEXENV;
      ;;return an expanded language symbolic expression.
      ;;
      (%struct-type-field-ref-transformer 'struct-type-field-ref #t expr-stx lexenv.run lexenv.expand))

    (define ($struct-type-field-ref-transformer expr-stx lexenv.run lexenv.expand)
      ;;Transformer  function  used   to  expand  $STRUCT-TYPE-FIELD-REF
      ;;syntaxes from  the top-level  built in environment.   Expand the
      ;;syntax  object EXPR-STX  in  the context  of  the given  LEXENV;
      ;;return an expanded language symbolic expression.
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
      ;;Transformer  function  used   to  expand  STRUCT-TYPE-FIELD-SET!
      ;;syntaxes from  the top-level  built in environment.   Expand the
      ;;syntax  object EXPR-STX  in  the context  of  the given  LEXENV;
      ;;return an expanded language symbolic expression.
      ;;
      (%struct-type-field-set!-transformer 'struct-type-field-ref #t expr-stx lexenv.run lexenv.expand))

    (define ($struct-type-field-set!-transformer expr-stx lexenv.run lexenv.expand)
      ;;Transformer  function  used  to  expand  $STRUCT-TYPE-FIELD-SET!
      ;;syntaxes from  the top-level  built in environment.   Expand the
      ;;syntax  object EXPR-STX  in  the context  of  the given  LEXENV;
      ;;return an expanded language symbolic expression.
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
    ;;Given the identifier  of the struct type: find its  label then its
    ;;syntactic binding  and return the  struct type descriptor.   If no
    ;;binding captures the identifier or the binding does not describe a
    ;;structure type descriptor: raise an exception.
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
    ;;Given a list of symbols  FIELD-NAMES representing a struct's field
    ;;names and an identifier FIELD-ID representing the name of a field:
    ;;return the index of the selected field in the list.
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
  ;;The syntactic  binding representing the R6RS  record type descriptor
  ;;and record constructor descriptor has one of the formats:
  ;;
  ;;   ($rtd . (?rtd-id ?rcd-id))
  ;;   ($rtd . (?rtd-id ?rcd-id . ?spec))
  ;;
  ;;where: "$rtd"  is the  symbol "$rtd"; ?RTD-ID  is the  identifier to
  ;;which the record type descriptor is bound; ?RCD-ID is the identifier
  ;;to which the  default record constructor descriptor  is bound; ?SPEC
  ;;is a record of type R6RS-RECORD-TYPE-SPEC.
  ;;
  (import R6RS-RECORD-TYPE-SPEC)

  (define (record-type-descriptor-transformer expr-stx lexenv.run lexenv.expand)
    ;;Transformer   function  used   to  expand   RECORD-TYPE-DESCRIPTOR
    ;;syntaxes  from the  top-level  built in  environment.  Expand  the
    ;;syntax object EXPR-STX in the  context of the given LEXENV; return
    ;;an expanded language symbolic expression.
    ;;
    (define-constant __who__ 'record-type-descriptor)
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
    (define-constant __who__ 'record-constructor-descriptor)
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
  ;;Transformer function  used to  expand TYPE-DESCRIPTOR  syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  ;;The result must be an expression evaluating to:
  ;;
  ;;* A Vicare  struct type descriptor if the  given identifier argument
  ;;  is a struct type name.
  ;;
  ;;* A R6RS record type descriptor  if the given identifier argument is
  ;;  a record type name.
  ;;
  ;;* An expand-time OBJECT-TYPE-SPEC instance.
  ;;
  (define-constant __who__ 'type-descriptor)
  (syntax-match expr-stx ()
    ((_ ?type-id)
     (identifier? ?type-id)
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run binding)
       ((r6rs-record-type)
	(chi-expr (r6rs-record-type-descriptor-binding-rtd binding)
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(build-data no-source
	  (syntactic-binding-value binding)))
       ((object-type-spec)
	(build-data no-source
	  (identifier-object-type-spec ?type-id)))
       ))
    ))

(define (is-a?-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's IS-A?   syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ 'is-a?)
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
	  (chi-expr (object-type-spec-pred-id spec)
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
		     `(,(object-type-spec-pred-id spec) ,?expr))
		    lexenv.run lexenv.expand)))
       ))
    ))


;;;; module core-macro-transformer: SLOT-REF, SLOT-SET!, $SLOT-REF, $SLOT-SET!

(define (slot-ref-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to  expand Vicare's SLOT-REF syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ 'slot-ref)
  (syntax-match expr-stx (<>)
    ((_ <> ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
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
	(chi-expr (identifier-object-type-spec-accessor ?type-id ?field-name-id #t expr-stx)
		  lexenv.run lexenv.expand))
       ))

    ((_ ?expr ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(record-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(struct-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((object-type-spec)
	(chi-expr (bless
		   `(,(identifier-object-type-spec-accessor ?type-id ?field-name-id #t expr-stx) ,?expr))
		  lexenv.run lexenv.expand))
       ))

    ;;Missing type identifier.  Try to retrieve the type from the tag of
    ;;the subject.
    ((_ ?id ?field-name-id)
     (and (identifier? ?id)
	  (identifier? ?field-name-id))
     (cond ((identifier-type-tagging ?id)
	    => (lambda (tag-id)
		 (chi-expr (bless
			    `(,(identifier-object-type-spec-accessor tag-id ?field-name-id #t expr-stx) ,?id))
			   lexenv.run lexenv.expand)))
	   (else
	    (syntax-error expr-stx "unable to determine type tag of expression"))))
    ))

(define (slot-set!-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's SLOT-SET! syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ 'slot-set!)
  (syntax-match expr-stx (<>)
    ((_ <> ?field-name-id ?type-id <>)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
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
	(chi-expr (identifier-object-type-spec-mutator ?type-id ?field-name-id #t expr-stx)
		  lexenv.run lexenv.expand))
       ))

    ((_ ?expr ?field-name-id ?type-id ?new-value)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `(record-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `(struct-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((object-type-spec)
	(chi-expr (bless
		   `(,(identifier-object-type-spec-mutator ?type-id ?field-name-id #t expr-stx) ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ))

    ;;Missing type identifier.  Try to retrieve the type from the tag of
    ;;the subject.
    ((_ ?id ?field-name-id ?new-value)
     (and (identifier? ?id)
	  (identifier? ?field-name-id))
     (cond ((identifier-type-tagging ?id)
	    => (lambda (tag-id)
		 (chi-expr (bless
			    `(,(identifier-object-type-spec-mutator tag-id ?field-name-id #t expr-stx) ,?id ,?new-value))
			   lexenv.run lexenv.expand)))
	   (else
	    (syntax-error expr-stx "unable to determine type tag of expression"))))
    ))

(define ($slot-ref-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's $SLOT-REF syntaxes from
  ;;the  top-level  built  in  environment.  Expand  the  syntax  object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ '$slot-ref)
  (syntax-match expr-stx (<>)
    ((_ <> ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
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
	(chi-expr (identifier-object-type-spec-accessor ?type-id ?field-name-id #f expr-stx)
		  lexenv.run lexenv.expand))
       ))

    ((_ ?expr ?field-name-id ?type-id)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `($record-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `($struct-type-field-ref ,?type-id ,?field-name-id ,?expr))
		  lexenv.run lexenv.expand))
       ((object-type-spec)
	(chi-expr (bless
		   `(,(identifier-object-type-spec-accessor ?type-id ?field-name-id #f expr-stx) ,?expr))
		  lexenv.run lexenv.expand))
       ))

    ;;Missing type identifier.  Try to retrieve the type from the tag of
    ;;the subject.
    ((_ ?id ?field-name-id)
     (and (identifier? ?id)
	  (identifier? ?field-name-id))
     (cond ((identifier-type-tagging ?id)
	    => (lambda (tag-id)
		 (chi-expr (bless
			    `(,(identifier-object-type-spec-accessor tag-id ?field-name-id #t expr-stx) ,?id))
			   lexenv.run lexenv.expand)))
	   (else
	    (syntax-error expr-stx "unable to determine type tag of expression"))))
    ))

(define ($slot-set!-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's $SLOT-SET!  syntaxes
  ;;from the top-level  built in environment.  Expand  the syntax object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-constant __who__ '$slot-set!)
  (syntax-match expr-stx (<>)
    ((_ <> ?field-name-id ?type-id <>)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
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
	(chi-expr (identifier-object-type-spec-mutator ?type-id ?field-name-id #f expr-stx)
		  lexenv.run lexenv.expand))
       ))

    ((_ ?expr ?field-name-id ?type-id ?new-value)
     (and (identifier? ?type-id)
	  (identifier? ?field-name-id))
     (case-object-type-binding (__who__ expr-stx ?type-id lexenv.run)
       ((r6rs-record-type)
	(chi-expr (bless
		   `($record-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(chi-expr (bless
		   `($struct-type-field-set! ,?type-id ,?field-name-id ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ((object-type-spec)
	(chi-expr (bless
		   `(,(identifier-object-type-spec-mutator ?type-id ?field-name-id #f expr-stx) ,?expr ,?new-value))
		  lexenv.run lexenv.expand))
       ))

    ;;Missing type identifier.  Try to retrieve the type from the tag of
    ;;the subject.
    ((_ ?id ?field-name-id ?new-value)
     (and (identifier? ?id)
	  (identifier? ?field-name-id))
     (cond ((identifier-type-tagging ?id)
	    => (lambda (tag-id)
		 (chi-expr (bless
			    `(,(identifier-object-type-spec-mutator tag-id ?field-name-id #f expr-stx) ,?id ,?new-value))
			   lexenv.run lexenv.expand)))
	   (else
	    (syntax-error expr-stx "unable to determine type tag of expression"))))
    ))


;;;; module core-macro-transformer: TAG-ASSERT, TAG-ASSERT-AND-RETURN

(define (tag-assert-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's TAG-ASSERT  syntaxes
  ;;from the top-level  built in environment.  Expand  the syntax object
  ;;EXPR-STX  in the  context of  the given  LEXENV; return  an expanded
  ;;language symbolic expression.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-assert))
  (define (%output-expression tag-id expr-stx)
    (receive-and-return (V)
	(chi-expr (bless
		   `(let ((V ,expr-stx))
		      (unless (is-a? V ,tag-id)
			(assertion-violation (quote ,tag-id)
			  "expression with wrong result type" (quote ,expr-stx) V))))
		  lexenv.run lexenv.expand)
      (debug-print 'exit V)))
  (syntax-match expr-stx ()
    ((_ ?tag ?expr)
     (cond ((not (tag-identifier? ?tag))
	    (syntax-violation __who__
	      "expected tag identifier as first argument" expr-stx ?tag))

	   ((and (identifier? ?expr)
	      (identifier-with-tagging? ?expr))
	    (let ((tag-id (identifier-type-tagging ?expr)))
	      (cond ((free-identifier=? <top> tag-id)
		     (%output-expression ?tag ?expr))
		    ((tag-super-and-sub? ?tag tag-id)
		     ;;We know at expand time  that the type is correct, so
		     ;;we do not insert the validation.
		     (chi-expr ?expr lexenv.run lexenv.expand))
		    (else
		     (syntax-violation __who__
		       "expression with wrong type tagging" expr-stx tag-id)))))
	   (else
	    (%output-expression ?tag ?expr))))
    ))

(define (tag-assert-and-return-transformer expr-stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  Vicare's TAG-ASSERT-AND-RETURN
  ;;syntaxes from the top-level built in environment.  Expand the syntax
  ;;object  EXPR-STX in  the  context  of the  given  LEXENV; return  an
  ;;expanded language symbolic expression.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-assert-and-return))
  (define (%output-expression tag-id expr-stx)
    (chi-expr (bless
	       `(receive-and-return (V)
		    ,expr-stx
		  (unless (is-a? V ,tag-id)
		    (assertion-violation (quote ,tag-id)
		      "expression with wrong result type" (quote ,expr-stx) V))))
	      lexenv.run lexenv.expand))
  (syntax-match expr-stx ()
    ((_ ?tag ?expr)
     (cond ((not (tag-identifier? ?tag))
	    (syntax-violation __who__
	      "expected tag identifier as first argument" expr-stx ?tag))

	   ((and (identifier? ?expr)
		 (identifier-with-tagging? ?expr))
	    (let ((tag-id (identifier-type-tagging ?expr)))
	      (cond ((free-identifier=? <top> tag-id)
		     (%output-expression ?tag ?expr))
		    ((tag-super-and-sub? ?tag tag-id)
		     ;;We know at expand time  that the type is correct, so
		     ;;we do not insert the validation.
		     (chi-expr ?expr lexenv.run lexenv.expand))
		    (else
		     (syntax-violation __who__
		       "expression with wrong type tagging" expr-stx tag-id)))))
	   (else
	    (%output-expression ?tag ?expr))))
    ))


;;;; done

;;; end of file
;;Local Variables:
;;mode: vicare
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
