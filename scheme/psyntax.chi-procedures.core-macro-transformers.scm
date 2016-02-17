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

(module (core-macro-transformer)
  (import (prefix (rnrs syntax-case) sys::))

(import PSYNTAX-TYPE-SYNTAX-OBJECTS)


;;;; helpers

(define-syntax (define-core-transformer stx)
  (sys::syntax-case stx ()
    ((_ (?who ?input-form.stx ?lexenv.run ?lexenv.expand) ?body0 ?body ...)
     (let* ((who.sym (sys::syntax->datum (sys::syntax ?who)))
	    (who.str (symbol->string who.sym))
	    (who.out (string->symbol (string-append who.str "-transformer"))))
       (sys::with-syntax
	   ((WHO    (sys::datum->syntax (sys::syntax ?who) who.out))
	    (SYNNER (sys::datum->syntax (sys::syntax ?who) '%synner)))
	 (sys::syntax
	  (define (WHO ?input-form.stx ?lexenv.run ?lexenv.expand)
	    (with-who ?who
	      (define SYNNER
		(case-lambda
		 ((message)
		  (SYNNER message #f))
		 ((message subform)
		  (syntax-violation (quote ?who) message ?input-form.stx subform))))
	      ?body0 ?body ...))))))
    ))

(module ($map-in-order
	 $map-in-order1)

  (case-define $map-in-order
    ((func ell)
     ($map-in-order1 func ell))
    ((func . ells)
     (if (null? ells)
	 '()
       (let recur ((ells ells))
	 (if (pair? ($car ells))
	     (let* ((cars ($map-in-order1 $car ells))
		    (cdrs ($map-in-order1 $cdr ells))
		    (head (apply func cars)))
	       (cons head (recur cdrs)))
	   '())))))

  (define-syntax-rule ($map-in-order1 ?func ?ell)
    (let recur ((ell ?ell))
      (if (pair? ell)
	  (let ((head (?func ($car ell))))
	    (cons head (recur ($cdr ell))))
	ell)))

  #| end of module |# )

(define (%maybe-push-annotated-expr-on-lambda-input-form input-form.stx syntax.sym formals.stx body.stx body*.stx)
  ;;When expanding core  macros like LAMBDA and NAMED-LAMBDA  we internally transform
  ;;an INPUT-FORM.STX like:
  ;;
  ;;   (lambda ?formals ?body . ?body*)
  ;;
  ;;into one of the following output forms:
  ;;
  ;;   (lambda/standard ?formals ?body . ?body*)
  ;;   (lambda/typed    ?formals ?body . ?body*)
  ;;
  ;;When debugging mode  is enabled: we want to include  this internal transformation
  ;;in the stack of annotated expressions of INPUT-FORM.STX.
  ;;
  ;;NOTE  We push  the  annotated expression  only when  debugging  mode is  enabled,
  ;;because this operation may slow down significantly the expansion process.
  ;;
  (if (options::debug-mode-enabled?)
      (stx-push-annotated-expr input-form.stx (bless `(,syntax.sym ,formals.stx ,body.stx . ,body*.stx)))
    input-form.stx))

(define (%maybe-push-annotated-expr-on-case-lambda-input-form input-form.stx syntax.sym formals*.stx body**.stx)
  ;;When expanding core  macros like CASE-LAMBDA and  NAMED-CASE-LAMBDA we internally
  ;;transform an INPUT-FORM.STX like:
  ;;
  ;;   (case-lambda (?formals ?body . ?body*) ...)
  ;;
  ;;into one of the following output forms:
  ;;
  ;;   (case-lambda/standard (?formals ?body . ?body*) ...)
  ;;   (case-lambda/typed    (?formals ?body . ?body*) ...)
  ;;
  ;;When debugging mode  is enabled: we want to include  this internal transformation
  ;;in the stack of annotated expressions of INPUT-FORM.STX.
  ;;
  ;;NOTE  We push  the  annotated expression  only when  debugging  mode is  enabled,
  ;;because this operation may slow down significantly the expansion process.
  ;;
  (if (options::debug-mode-enabled?)
      (stx-push-annotated-expr input-form.stx (bless `(,syntax.sym ,(map cons formals*.stx body**.stx))))
    input-form.stx))


;;The function CORE-MACRO-TRANSFORMER maps symbols  representing core macros to their
;;macro transformers.
;;
;;We distinguish between "non-core macros" and "core macros".
;;
;;NOTE This  module is very long,  so it is  split into multiple code  pages.  (Marco
;;Maggi; Sat Apr 27, 2013)
;;
(define* (core-macro-transformer name)
  (case name
    ((quote)					quote-transformer)
    ;;
    ((lambda)					lambda-transformer)
    ((lambda/standard)				lambda/standard-transformer)
    ((lambda/typed)				lambda/typed-transformer)
    ((case-lambda)				case-lambda-transformer)
    ((case-lambda/standard)			case-lambda/standard-transformer)
    ((case-lambda/typed)			case-lambda/typed-transformer)
    ((named-lambda)				named-lambda-transformer)
    ((named-lambda/standard)			named-lambda/standard-transformer)
    ((named-lambda/typed)			named-lambda/typed-transformer)
    ((named-case-lambda)			named-case-lambda-transformer)
    ((named-case-lambda/standard)		named-case-lambda/standard-transformer)
    ((named-case-lambda/typed)			named-case-lambda/typed-transformer)
    ;;
    ((let)					let-transformer)
    ((letrec)					letrec-transformer)
    ((letrec*)					letrec*-transformer)
    ((if)					if-transformer)
    ((foreign-call)				foreign-call-transformer)
    ((syntax-case)				syntax-case-transformer)
    ((syntax)					syntax-transformer)
    ((fluid-let-syntax)				fluid-let-syntax-transformer)
    ((splice-first-expand)			splice-first-expand-transformer)
    ((internal-body)				internal-body-transformer)
    ((predicate-procedure-argument-validation)	predicate-procedure-argument-validation-transformer)
    ((predicate-return-value-validation)	predicate-return-value-validation-transformer)

    ((struct-type-descriptor)			struct-type-descriptor-transformer)

    ((record-type-descriptor)			record-type-descriptor-transformer)
    ((record-constructor-descriptor)		record-constructor-descriptor-transformer)

    ((type-descriptor)				type-descriptor-transformer)
    ((new)					new-transformer)
    ((delete)					delete-transformer)
    ((is-a?)					is-a?-transformer)
    ((slot-ref)					slot-ref-transformer)
    ((slot-set!)				slot-set!-transformer)
    ((method-call)				method-call-transformer)
    ((case-type)				case-type-transformer)

    ((internal-run-time-is-a?)			internal-run-time-is-a?-transformer)

    ((unsafe-cast-signature)			unsafe-cast-signature-transformer)
    ((validate-typed-procedure-argument)	validate-typed-procedure-argument-transformer)
    ((validate-typed-return-value)		validate-typed-return-value-transformer)
    ((assert-signature)				assert-signature-transformer)
    ((assert-signature-and-return)		assert-signature-and-return-transformer)

    ((type-of)					type-of-transformer)
    ((type-super-and-sub?)			type-super-and-sub?-transformer)
    ((signature-super-and-sub?)			signature-super-and-sub?-transformer)

    ((expansion-of)				expansion-of-transformer)
    ((expansion-of*)				expansion-of*-transformer)
    ((visit-code-of)				visit-code-of-transformer)
    ((optimisation-of)				optimisation-of-transformer)
    ((further-optimisation-of)			further-optimisation-of-transformer)
    ((optimisation-of*)				optimisation-of*-transformer)
    ((further-optimisation-of*)			further-optimisation-of*-transformer)
    ((assembly-of)				assembly-of-transformer)

    (else
     (assertion-violation/internal-error __who__
       "cannot find transformer" name))))


;;;; external modules

(include "psyntax.chi-procedures.type-macro-transformers.scm" #t)


;;;; module core-macro-transformer: LAMBDA and variants

(define-core-transformer (lambda/standard input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand LAMBDA/STANDARD syntaxes  from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI object.
  ;;
  ;;The syntax  LAMBDA/STANDARD is  strictly compatible with  the R6RS  definition of
  ;;LAMBDA; this syntax must be used in code that rejects typed language extensions.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?formals ?body ?body* ...)
     (chi-lambda/standard input-form.stx lexenv.run lexenv.expand
			  ?formals (cons ?body ?body*)))
    ))

(define-core-transformer (lambda/typed input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  LAMBDA/TYPED syntaxes  from the  top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI object.
  ;;
  ;;The syntax  LAMBDA/TYPED is  compatible with  the R6RS  definition of  LAMBDA and
  ;;extends it with  typed language features; this  syntax must be used  in code that
  ;;accepts typed language extensions.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?formals ?body ?body* ...)
     (chi-lambda/typed input-form.stx lexenv.run lexenv.expand
		       ?formals (cons ?body ?body*)))
    ))

(define-core-transformer (lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand LAMBDA syntaxes from the  top-level built in
  ;;environment.  Expand the syntax object INPUT-FORM.STX in the context of the given
  ;;LEXENV; return a PSI object.
  ;;
  ;;The expansion of the  syntax LAMBDA is influenced by state  of the typed language
  ;;option:  if typed  language  enabled, LAMBDA  is  transformed into  LAMBDA/TYPED;
  ;;otherwise it is transformed into LAMBDA/STANDARD.
  ;;
  ;;NOTE The LAMBDA  syntax as implemented here would be  more cleanly implemented as
  ;;non-core  macro.  But  implementing it  here as  core macro  makes the  expansion
  ;;process faster.  (Marco Maggi; Sun Jan 17, 2016)
  ;;
  (syntax-match input-form.stx ()
    ((_ ?formals ?body ?body* ...)
     (if (options::typed-language?)
	 (chi-lambda/typed (%maybe-push-annotated-expr-on-lambda-input-form input-form.stx
			     'lambda/typed ?formals ?body ?body*)
			   lexenv.run lexenv.expand
			   ?formals (cons ?body ?body*))
       (chi-lambda/standard (%maybe-push-annotated-expr-on-lambda-input-form input-form.stx
			      'lambda/standard ?formals ?body ?body*)
			    lexenv.run lexenv.expand
			    ?formals (cons ?body ?body*))))
    ))


;;;; module core-macro-transformer: NAMED-LAMBDA and variants

(define-core-transformer (named-lambda/standard input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to  expand Vicare's NAMED-LAMBDA/STANDARD syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI object.
  ;;
  ;;The syntax NAMED-LAMBDA/STANDARD is compatible with the R6RS definition of LAMBDA
  ;;and in addition accepts a name for the generated closure object; this syntax must
  ;;be used in code that rejects  typed language extensions.  This syntax establishes
  ;;a syntactic binding between the fluid syntax "__who__" and the quoted name of the
  ;;lambda.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?who ?formals ?body ?body* ...)
     (identifier? ?who)
     (chi-named-lambda/standard input-form.stx lexenv.run lexenv.expand
				?who ?formals (cons ?body ?body*)))
    ))

(define-core-transformer (named-lambda/typed input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's NAMED-LAMBDA/TYPED syntaxes from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?who ?formals ?body ?body* ...)
     (identifier? ?who)
     (chi-named-lambda/typed input-form.stx lexenv.run lexenv.expand
			     ?who ?formals (cons ?body ?body*)))
    ))

(define-core-transformer (named-lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  NAMED-LAMBDA  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  ;;NOTE  The  NAMED-LAMBDA  syntax  as   implemented  here  would  be  more  cleanly
  ;;implemented as non-core macro.  But implementing  it here as core macro makes the
  ;;expansion process faster.  (Marco Maggi; Sun Jan 17, 2016)
  ;;
  (syntax-match input-form.stx ()
    ((_ ?who ?formals ?body ?body* ...)
     (identifier? ?who)
     (if (options::typed-language?)
	 (chi-named-lambda/typed (%maybe-push-annotated-expr-on-lambda-input-form input-form.stx
				   'named-lambda/typed ?formals ?body ?body*)
				 lexenv.run lexenv.expand
				 ?who ?formals (cons ?body ?body*))
       (chi-named-lambda/standard (%maybe-push-annotated-expr-on-lambda-input-form input-form.stx
				    'named-lambda/standard ?formals ?body ?body*)
				  lexenv.run lexenv.expand
				  ?who ?formals (cons ?body ?body*))))
    ))


;;;; module core-macro-transformer: CASE-LAMBDA and variants

(define-core-transformer (case-lambda/standard input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand CASE-LAMBDA/STANDARD  syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return an PSI object.
  ;;
  ;;The syntax CASE-LAMBDA/STANDARD  is strictly compatible with  the R6RS definition
  ;;of CASE-LAMBDA;  this syntax  must be  used in code  that rejects  typed language
  ;;extensions.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?formals* ?body* ?body** ...) ...)
     (chi-case-lambda/standard input-form.stx lexenv.run lexenv.expand
			       ?formals* (map cons ?body* ?body**)))
    ))

(define-core-transformer (case-lambda/typed input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand CASE-LAMBDA/TYPED syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return an PSI object.
  ;;
  ;;The  syntax  CASE-LAMBDA/TYPED   is  compatible  with  the   R6RS  definition  of
  ;;CASE-LAMBDA and extends it with typed language features.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?formals* ?body* ?body** ...) ...)
     (chi-case-lambda/typed input-form.stx lexenv.run lexenv.expand
			    ?formals* (map cons ?body* ?body**)))
    ))

(define-core-transformer (case-lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand CASE-LAMBDA syntaxes from the top-level built
  ;;in environment.   Expand the syntax object  INPUT-FORM.STX in the context  of the
  ;;given LEXENV; return an PSI object.
  ;;
  ;;The  expansion of  the syntax  CASE-LAMBDA is  influenced by  state of  the typed
  ;;language  option: if  typed  language enabled,  CASE-LAMBDA  is transformed  into
  ;;CASE-LAMBDA/TYPED; otherwise it is transformed into CASE-LAMBDA/STANDARD.
  ;;
  ;;NOTE The CASE-LAMBDA syntax as implemented here would be more cleanly implemented
  ;;as non-core  macro.  But implementing it  here as core macro  makes the expansion
  ;;process faster.  (Marco Maggi; Sun Jan 17, 2016)
  ;;
  (syntax-match input-form.stx ()
    ((_ (?formals* ?body* ?body** ...) ...)
     (let ((body**.stx (map cons ?body* ?body**)))
       (if (options::typed-language?)
	   (chi-case-lambda/typed (%maybe-push-annotated-expr-on-case-lambda-input-form input-form.stx
				    'case-lambda/typed ?formals* body**.stx)
				  lexenv.run lexenv.expand
				  ?formals* body**.stx)
	 (chi-case-lambda/standard (%maybe-push-annotated-expr-on-case-lambda-input-form input-form.stx
				     'case-lambda/standard ?formals* body**.stx)
				   lexenv.run lexenv.expand
				   ?formals* body**.stx))))
    ))


;;;; module core-macro-transformer: NAMED-CASE-LAMBDA and variants

(define-core-transformer (named-case-lambda/standard input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to  expand Vicare's NAMED-CASE-LAMBDA/STANDARD syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return an PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?who (?formals* ?body* ?body** ...) ...)
     (identifier? ?who)
     (chi-named-case-lambda/standard input-form.stx lexenv.run lexenv.expand
				     ?who ?formals* (map cons ?body* ?body**)))
    ))

(define-core-transformer (named-case-lambda/typed input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's NAMED-CASE-LAMBDA/TYPED  syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return an PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?who (?formals* ?body* ?body** ...) ...)
     (identifier? ?who)
     (chi-named-case-lambda/typed input-form.stx lexenv.run lexenv.expand
				  ?who ?formals* (map cons ?body* ?body**)))
    ))

(define-core-transformer (named-case-lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  Vicare's NAMED-CASE-LAMBDA syntaxes from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return an PSI object.
  ;;
  ;;The expansion of the syntax NAMED-CASE-LAMBDA is influenced by state of the typed
  ;;language option: if typed language enabled, NAMED-CASE-LAMBDA is transformed into
  ;;NAMED-CASE-LAMBDA/TYPED;      otherwise      it     is      transformed      into
  ;;NAMED-CASE-LAMBDA/STANDARD.
  ;;
  ;;NOTE  The NAMED-CASE-LAMBDA  syntax as  implemented  here would  be more  cleanly
  ;;implemented as non-core macro.  But implementing  it here as core macro makes the
  ;;expansion process faster.  (Marco Maggi; Sun Jan 17, 2016)
  ;;
  (syntax-match input-form.stx ()
    ((_ ?who (?formals* ?body* ?body** ...) ...)
     (identifier? ?who)
     (let ((body**.stx (map cons ?body* ?body**)))
       (if (options::strict-r6rs)
	   (chi-named-case-lambda/typed (%maybe-push-annotated-expr-on-case-lambda-input-form input-form.stx
					  'named-case-lambda/typed ?formals* body**.stx)
					lexenv.run lexenv.expand
					?who ?formals* body**.stx)
	 (chi-named-case-lambda/standard (%maybe-push-annotated-expr-on-case-lambda-input-form input-form.stx
					   'named-case-lambda/standard ?formals* body**.stx)
					 lexenv.run lexenv.expand
					 ?who ?formals* body**.stx))))
    ))


;;;; module core-macro-transformer: IF

(define-core-transformer (if input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS  IF syntaxes from the top-level built in
  ;;environment.  Expand the syntax object INPUT-FORM.STX in the context of the given
  ;;LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?test ?consequent ?alternate)
     (let ((test.psi       (chi-expr ?test       lexenv.run lexenv.expand))
	   (consequent.psi (chi-expr ?consequent lexenv.run lexenv.expand))
	   (alternate.psi  (chi-expr ?alternate  lexenv.run lexenv.expand)))
       (make-psi input-form.stx
		 (build-conditional no-source
		     (psi.core-expr test.psi)
		   (psi.core-expr consequent.psi)
		   (psi.core-expr alternate.psi))
		 (type-signature.common-ancestor (psi.retvals-signature consequent.psi)
						 (psi.retvals-signature alternate.psi)))))
    ((_ ?test ?consequent)
     (let ((test.psi       (chi-expr ?test       lexenv.run lexenv.expand))
	   (consequent.psi (chi-expr ?consequent lexenv.run lexenv.expand)))
       ;;We build  code to  make the  one-armed IF  return void  if the  alternate is
       ;;unspecified; according  to R6RS:
       ;;
       ;;* If  the test succeeds: the  return value must  be the return value  of the
       ;;  consequent.
       ;;
       ;;* If the  test fails and there  *is* an alternate: the return  value must be
       ;;  the return value of the alternate.
       ;;
       ;;* If the test fails and there is *no* alternate: this syntax has unspecified
       ;;  return values.
       ;;
       ;;Notice that one-armed IF  is also used in the expansion  of WHEN and UNLESS;
       ;;R6RS states that, for those syntaxes, when the body *is* executed the return
       ;;value must be the return value of the last expression in the body.
       (make-psi input-form.stx
		 (build-conditional no-source
		     (psi.core-expr test.psi)
		   (psi.core-expr consequent.psi)
		   (build-void))
		 (make-type-signature/fully-untyped))))
    ))


;;;; module core-macro-transformer: QUOTE

(define-core-transformer (quote input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS  QUOTE syntaxes from the top-level built
  ;;in environment.   Expand the syntax object  INPUT-FORM.STX in the context  of the
  ;;given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?datum)
     (let ((datum (syntax->datum ?datum)))
       (make-psi input-form.stx
		 (build-data no-source datum)
		 (datum-type-signature datum))))
    ))


;;;; module core-macro-transformer: LET

(define* (let-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer functions  used to expand  LET syntaxes  from the top-level  built in
  ;;environment.  Expand the syntax object INPUT-FORM.STX in the context of the given
  ;;LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
     ;;Convert the UNnamed standard syntax:
     ;;
     ;;   (let ((?lhs ?rhs) ...) . ?body)
     ;;
     ;;into the core language syntax:
     ;;
     ;;   (let ((?lhs.lex ?rhs.core) ...) . ?body.core)
     ;;
     (receive (lhs*.lex rhs*.psi rib lexenv.run)
	 (if (options::typed-language?)
	     ;;Prepare extended, possibly typed syntactic bindings.
	     (let*-values
		 (((lhs*.id lhs*.tag)
		   (syntax-object.parse-typed-list-of-bindings ?lhs* input-form.stx))
		  ((rhs*.psi)
		   (map (lambda (rhs.stx lhs.tag)
			  ;;We  insert  a signature  validation  even  if LHS.TAG  is
			  ;;"<top>": this  way we  try to  check at  expand-time that
			  ;;there is a single return  value.  At run-time, we rely on
			  ;;the built-in run-time checking of single-value return.
			  (chi-expr (bless
				     `(assert-signature-and-return (,lhs.tag) ,rhs.stx))
				    lexenv.run lexenv.expand))
		     ?rhs* lhs*.tag))
		  ;;Prepare the untyped and typed lexical variables.
		  ((rib lexenv.run lhs*.lex)
		   (%process-typed-syntactic-bindings-lhs* lhs*.id lhs*.tag lexenv.run)))
	       (values lhs*.lex rhs*.psi rib lexenv.run))
	   ;;Prepare standard, untyped syntactic bindings.
	   (let* ((lhs*.id	(syntax-object.parse-standard-list-of-bindings ?lhs* input-form.stx))
		  (rhs*.psi	(map (lambda (rhs.stx)
				       (chi-expr rhs.stx lexenv.run lexenv.expand))
				  ?rhs*))
		  (lhs*.lab	(map generate-label-gensym   lhs*.id))
		  (lhs*.lex	(map generate-lexical-gensym lhs*.id))
		  (lexenv.run	(lexenv-add-lexical-var-bindings lhs*.lab lhs*.lex lexenv.run))
		  (rib	(make-rib/from-identifiers-and-labels lhs*.id lhs*.lab)))
	     (values lhs*.lex rhs*.psi rib lexenv.run)))
       ;;Prepare the body.
       (let* ((body*.stx  (push-lexical-contour rib (cons ?body ?body*)))
	      (body.psi   (chi-internal-body input-form.stx lexenv.run lexenv.expand body*.stx))
	      (body.core  (psi.core-expr body.psi))
	      (rhs*.core  (map psi.core-expr rhs*.psi)))
	 (make-psi input-form.stx
		   (build-let (syntax-annotation input-form.stx)
			      lhs*.lex rhs*.core
			      body.core)
		   (psi.retvals-signature body.psi)))))

    ((_ ?recur ((?lhs* ?rhs*) ...) ?body ?body* ...)
     (identifier? ?recur)
     (chi-expr (bless
		`(internal-body
		   ;;Here we  use DEFINE/TYPE so  that we  can easily define  a typed
		   ;;function.   Using  LETREC would  be  more  descriptive, but  not
		   ;;significantly better.
		   ;;
		   ;;FIXME We do not want "__who__"  to be bound here.  (Marco Maggi;
		   ;;Sat Feb 6, 2016)
		   (define/typed (,?recur . ,?lhs*) ,?body . ,?body*)
		   (,?recur . ,?rhs*)))
	       lexenv.run lexenv.expand))

    (_
     (syntax-violation __who__ "invalid syntax" input-form.stx))))


;;;; module core-macro-transformer: LET, LETREC and LETREC*

(module (letrec-transformer letrec*-transformer)
  ;;Transformer functions  used to expand LET,  LETREC and LETREC* syntaxes  from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  ;;In practice, below we convert the standard syntaxes:
  ;;
  ;;   (letrec  ((?lhs ?rhs) ...) . ?body)
  ;;   (letrec* ((?lhs ?rhs) ...) . ?body)
  ;;
  ;;into the core language syntaxes:
  ;;
  ;;   (letrec  ((?lhs ?rhs) ...) . ?body)
  ;;   (letrec* ((?lhs ?rhs) ...) . ?body)
  ;;

  (define (letrec-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand LETREC syntaxes from the top-level built in
    ;;environment.  Expand  the syntax  object INPUT-FORM.STX in  the context  of the
    ;;given LEXENV; return a PSI object.
    ;;
    (%letrec-helper input-form.stx lexenv.run lexenv.expand build-letrec))

  (define (letrec*-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used  to expand LETREC* syntaxes from  the top-level built
    ;;in environment.  Expand the syntax object  INPUT-FORM.STX in the context of the
    ;;given LEXENV; return a PSI object.
    ;;
    (%letrec-helper input-form.stx lexenv.run lexenv.expand build-letrec*))

  (define* (%letrec-helper input-form.stx lexenv.run lexenv.expand core-lang-builder)
    (syntax-match input-form.stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       (receive (lhs*.lex rhs*.psi rib lexenv.run)
	   (if (options::typed-language?)
	       ;;Prepare extended, possibly typed syntactic bindings.
	       (let*-values
		   (((lhs*.id lhs*.tag)
		     (syntax-object.parse-typed-list-of-bindings ?lhs* input-form.stx))
		    ;;Prepare the typed and untyped lexical variables.
		    ((rib lexenv.run lhs*.lex)
		     (%process-typed-syntactic-bindings-lhs* lhs*.id lhs*.tag lexenv.run))
		    ;;NOTE The region of all the LETREC and LETREC* bindings includes
		    ;;all the right-hand sides.  The new rib is pushed on all the RHS
		    ;;and the body.
		    ((rhs*.psi)
		     ($map-in-order
			 (lambda (rhs.stx lhs.tag)
			   (chi-expr (push-lexical-contour rib
				       (bless
					`(assert-signature-and-return (,lhs.tag) ,rhs.stx)))
				     lexenv.run lexenv.expand))
		       ?rhs* lhs*.tag)))
		 (values lhs*.lex rhs*.psi rib lexenv.run))
	     ;;Prepare standard, untyped syntactic bindings.
	     (let* ((lhs*.id		(syntax-object.parse-standard-list-of-bindings ?lhs* input-form.stx))
		    (lhs*.lab		(map generate-label-gensym   lhs*.id))
		    (lhs*.lex		(map generate-lexical-gensym lhs*.id))
		    (lexenv.run		(lexenv-add-lexical-var-bindings lhs*.lab lhs*.lex lexenv.run))
		    (rib		(make-rib/from-identifiers-and-labels lhs*.id lhs*.lab))
		    ;;NOTE The region of all the LETREC and LETREC* bindings includes
		    ;;all the right-hand sides.  The new rib is pushed on all the RHS
		    ;;and the body.
		    (rhs*.psi		($map-in-order (lambda (rhs.stx)
							 (chi-expr (push-lexical-contour rib rhs.stx) lexenv.run lexenv.expand))
					  ?rhs*)))
	       (values lhs*.lex rhs*.psi rib lexenv.run)))
	 ;;Prepare the body.
	 (let* ((body*.stx	(cons ?body ?body*))
		(body.psi	(chi-internal-body input-form.stx lexenv.run lexenv.expand
						   (push-lexical-contour rib body*.stx)))
		(body.core	(psi.core-expr body.psi)))
	   ;;Build the LETREC or LETREC* expression in the core language.
	   (let ((rhs*.core (map psi.core-expr rhs*.psi)))
	     (make-psi input-form.stx
		       (core-lang-builder (syntax-annotation input-form.stx)
			 lhs*.lex rhs*.core
			 body.core)
		       (psi.retvals-signature body.psi))))))
      ))

  #| end of module |# )


;;;; module core-macro-transformer: FLUID-LET-SYNTAX

(define-core-transformer (fluid-let-syntax input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand uses  of the syntax FLUID-LET-SYNTAX from the
  ;;top-level built-in environment.   Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  ;;FLUID-LET-SYNTAX is similar,  but not equal, to LET-SYNTAX;  rather than defining
  ;;new ?LHS keyword  syntactic bindings, it temporarily rebinds the  keywords to new
  ;;transformers while  expanding the ?BODY  forms.  The  given ?LHS must  be already
  ;;bound to fluid syntaxes defined by DEFINE-FLUID-SYNTAX.
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
  ;;only to a subsequence  of forms in a body (or there is?).   (Marco Maggi; Tue Feb
  ;;18, 2014)
  ;;
  (syntax-match input-form.stx ()
    ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
     ;;Check that the ?LHS* are all identifiers with no duplicates.
     (unless (valid-bound-ids? ?lhs*)
       (error-invalid-formals-syntax input-form.stx ?lhs*))
     (let loop ((lhs*.id	?lhs*)
		(rhs*.stx	?rhs*)
		(lexenv.run	lexenv.run)
		(lexenv.expand	lexenv.expand))
       (if (pair? lhs*.id)
	   (receive (lexenv.run lexenv.expand)
	       (fluid-syntax-push-redefinition-on-lexenvs input-form.stx lexenv.run lexenv.expand
							  __who__ (car lhs*.id) (car rhs*.stx))
	     (loop (cdr lhs*.id) (cdr rhs*.stx) lexenv.run lexenv.expand))
	 (chi-internal-body input-form.stx lexenv.run lexenv.expand (cons ?body ?body*)))))
    ))


;;;; module core-macro-transformer: FOREIGN-CALL

(define-core-transformer (foreign-call input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  FOREIGN-CALL  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?name ?arg* ...)
     (let* ((name.psi  (chi-expr  ?name lexenv.run lexenv.expand))
	    (arg*.psi  (chi-expr* ?arg* lexenv.run lexenv.expand))
	    (expr.core (build-foreign-call no-source
			   (psi.core-expr name.psi)
			 (map psi.core-expr arg*.psi))))
       (make-psi input-form.stx expr.core)))
    ))


;;;; module core-macro-transformer: SYNTAX

(module (syntax-transformer)
  ;;Transformer function  used to  expand R6RS's SYNTAX  syntaxes from  the top-level
  ;;built in  environment.  Process  the contents  of USE-STX in  the context  of the
  ;;lexical environments LEXENV.RUN and LEXENV.EXPAND.  Return a PSI object.
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
  ;;   (?label . (pattern-variable . (?name . ?level)))
  ;;
  ;;where: ?LABEL is the label in the identifier's syntax object, ?NAME is the symbol
  ;;representing  the name  of  the  pattern variable,  ?LEVEL  is  an exact  integer
  ;;representing the  nesting ellipsis  level.  The  SYNTAX-CASE patterns  below will
  ;;generate the given entries:
  ;;
  ;;   ?a			->  (pattern-variable . (?a . 0))
  ;;   (?a)			->  (pattern-variable . (?a . 0))
  ;;   (((?a)))			->  (pattern-variable . (?a . 0))
  ;;   (?a ...)			->  (pattern-variable . (?a . 1))
  ;;   ((?a) ...)		->  (pattern-variable . (?a . 1))
  ;;   ((((?a))) ...)		->  (pattern-variable . (?a . 1))
  ;;   ((?a ...) ...)		->  (pattern-variable . (?a . 2))
  ;;   (((?a ...) ...) ...)	->  (pattern-variable . (?a . 3))
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
  (define (syntax-transformer input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?template)
       (receive (intermediate-sexp maps)
	   (%gen-syntax input-form.stx ?template lexenv.run '() ellipsis-id? #f)
	 (let ((code (%generate-output-code intermediate-sexp)))
	   #;(debug-print 'syntax (syntax->datum ?template) intermediate-sexp code)
	   (make-psi input-form.stx code
		     (if (identifier? ?template)
			 (make-type-signature/single-syntactic-identifier)
		       ;;FIXME The  form #'(1  .  2) returns  an instance  of wrapped
		       ;;syntax object; if X is a  pattern variable, the form #'(X X)
		       ;;returns a list of wrapped syntax objects.  So here we should
		       ;;implement type  signature tracking  to be  able to  return a
		       ;;meaning ful signature.  (Marco Maggi; Mon Dec 28, 2015)
		       (make-type-signature/single-top))))))
      ))

  (define-module-who syntax)

  (define (%gen-syntax input-form.stx template-stx lexenv maps ellipsis-id? vec?)
    ;;Recursive function.  Expand the contents of a SYNTAX use.
    ;;
    ;;INPUT-FORM.STX must be the syntax object  containing the original SYNTAX macro use; it
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
    ;;ELLIPSIS-ID?  must be  a predicate  function returning  true when  applied to  the
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
       (ellipsis-id? ?dots)
       (syntax-violation __module_who__ "misplaced ellipsis in syntax form" input-form.stx))

      ;;Match a standalone  identifier.  ?ID can be: a reference  to pattern variable
      ;;created by SYNTAX-CASE; an identifier that  will be captured by some binding;
      ;;an  identifier  that will  result  to  be free,  in  which  case an  "unbound
      ;;identifier" error will be raised later.
      ;;
      (?id
       (identifier? ?id)
       (cond ((id->label ?id)
	      => (lambda (label)
		   (let ((descr (label->syntactic-binding-descriptor label lexenv)))
		     (case (syntactic-binding-descriptor.type descr)
		       ((pattern-variable)
			;;It is a reference to pattern variable.
			(receive (var maps)
			    (let* ((name.level  (syntactic-binding-descriptor.value descr))
				   (name        (car name.level))
				   (level       (cdr name.level)))
			      (%gen-ref input-form.stx name level maps))
			  (values (list 'ref var) maps)))
		       (else
			;;It is  some other identifier.  Here  we just put it  in the
			;;output form:  we do not want  to raise an exception,  we do
			;;not care if the descriptor is of type "displaced-lexical".
			(values (list 'quote ?id) maps))))))
	     (else
	      ;;It is an unbound identifier.  Here we just put it in the output form:
	      ;;we do not want to raise an exception.
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
      ;;We change  the ELLIPSIS-ID? argument  for recursion  to a predicate  that always
      ;;returns false.
      ;;
      ((?dots ?sub-template)
       (ellipsis-id? ?dots)
       (if vec?
	   (syntax-violation __module_who__ "misplaced ellipsis in syntax form" input-form.stx)
	 (%gen-syntax input-form.stx ?sub-template lexenv maps (lambda (x) #f) #f)))

      ;;Match a template followed by ellipsis.
      ;;
      ((?template ?dots . ?rest)
       (ellipsis-id? ?dots)
       (let loop
	   ((rest.stx ?rest)
	    (kont     (lambda (maps)
			(receive (template^ maps)
			    (%gen-syntax input-form.stx ?template lexenv (cons '() maps) ellipsis-id? #f)
			  (if (null? (car maps))
			      (syntax-violation __module_who__ "extra ellipsis in syntax form" input-form.stx)
			    (values (%gen-map template^ (car maps))
				    (cdr maps)))))))
	 (syntax-match rest.stx ()
	   (()
	    (kont maps))

	   ((?dots . ?tail)
	    (ellipsis-id? ?dots)
	    (loop ?tail (lambda (maps)
			  (receive (template^ maps)
			      (kont (cons '() maps))
			    (if (null? (car maps))
				(syntax-violation __module_who__ "extra ellipsis in syntax form" input-form.stx)
			      (values (%gen-mappend template^ (car maps))
				      (cdr maps)))))))

	   (_
	    (receive (rest^ maps)
		(%gen-syntax input-form.stx rest.stx lexenv maps ellipsis-id? vec?)
	      (receive (template^ maps)
		  (kont maps)
		(values (%gen-append template^ rest^) maps))))
	   )))

      ;;Process pair templates.
      ;;
      ((?car . ?cdr)
       (receive (car.new maps)
	   (%gen-syntax input-form.stx ?car lexenv maps ellipsis-id? #f)
	 (receive (cdr.new maps)
	     (%gen-syntax input-form.stx ?cdr lexenv maps ellipsis-id? vec?)
	   (values (%gen-cons template-stx ?car ?cdr car.new cdr.new)
		   maps))))

      ;;Process a vector template.  We set to true the VEC? argument for recursion.
      ;;
      (#(?item* ...)
       (receive (item*.new maps)
	   (%gen-syntax input-form.stx ?item* lexenv maps ellipsis-id? #t)
	 (values (%gen-vector template-stx ?item* item*.new)
		 maps)))

      ;;Everything else is just quoted in  the output.  This includes all the literal
      ;;datums.
      ;;
      (_
       (values `(quote ,template-stx) maps))
      ))

  (define (%gen-ref input-form.stx var level maps)
    ;;Recursive function.
    ;;
    #;(debug-print 'gen-ref maps)
    (if (zero? level)
	(values var maps)
      (if (null? maps)
	  (syntax-violation __module_who__ "missing ellipsis in syntax form" input-form.stx)
	(receive (outer-var outer-maps)
	    (%gen-ref input-form.stx var (- level 1) (cdr maps))
	  (cond ((assq outer-var (car maps))
		 => (lambda (b)
		      (values (cdr b) maps)))
		(else
		 (let ((inner-var (generate-lexical-gensym 'tmp)))
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
  ;;top-level built  in environment.  Process  the contents of INPUT-FORM.STX  in the
  ;;context of the  lexical environments LEXENV.RUN and LEXENV.EXPAND.   Return a PSI
  ;;struct.
  ;;
  ;;Notice that the parsing of the patterns is performed by CONVERT-PATTERN at expand
  ;;time and the actual pattern matching is performed by SYNTAX-DISPATCH at run time.
  ;;
  (define-module-who syntax-case)

  (define-syntax stx-error
    (syntax-rules ()
      ((_ ?stx ?msg)
       (syntax-violation __module_who__ ?msg ?stx))
      ))

  (define (syntax-case-transformer input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?expr (?literal* ...) ?clauses* ...)
       (verify-syntax-case-literals __module_who__ input-form.stx ?literal*)
       (let* ( ;;The lexical variable to which  the result of evaluating the ?EXPR is
	      ;;bound.
	      (expr.sym   (generate-lexical-gensym 'tmp))
	      ;;The full SYNTAX-CASE pattern matching code, generated and transformed
	      ;;to core language.
	      (body.core  (%gen-syntax-case expr.sym ?literal* ?clauses*
					    lexenv.run lexenv.expand))
	      ;;The ?EXPR transformed to core language.
	      (expr.core  (%chi-expr.core ?expr lexenv.run lexenv.expand)))
	 ;;Return a form like:
	 ;;
	 ;;   ((lambda (expr.sym) body.core) expr.core)
	 ;;
	 ;;where BODY.CORE is the SYNTAX-CASE matching code.
	 (make-psi input-form.stx
		   (build-application no-source
		     (build-lambda no-source
			 (list expr.sym)
		       body.core)
		     (list expr.core)))))
      ))

  (define (%gen-syntax-case expr.sym literals clauses lexenv.run lexenv.expand)
    ;;Recursive function.  Generate and return the  full pattern matching code in the
    ;;core language to match the given CLAUSES.
    ;;
    (syntax-match clauses ()
      ;;No pattern matched the input expression: return code to raise a syntax error.
      ;;
      (()
       (build-application no-source
	 (build-primref no-source 'syntax-violation)
	 (list (build-data no-source 'syntax-case)
	       (build-data no-source "no pattern matched the input expression")
	       (build-lexical-reference no-source expr.sym))))

      ;;The pattern is  a standalone identifier, neither a literal  nor the ellipsis,
      ;;and  it has  no  fender.   A standalone  identifier  with  no fender  matches
      ;;everything, so it is  useless to generate the code for  the next clauses: the
      ;;code generated here is the last one.
      ;;
      (((?pattern ?output-expr) . ?unused-clauses)
       (and (identifier? ?pattern)
	    (not (bound-id-member? ?pattern literals))
	    (not (ellipsis-id? ?pattern)))
       (if (underscore-id? ?pattern)
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
	 (let ((label (generate-label-gensym ?pattern))
	       (lex   (generate-lexical-gensym ?pattern)))
	   ;;The expression must be expanded  in a lexical environment augmented with
	   ;;the pattern variable.
	   (define output-expr^
	     (push-lexical-contour
		 (make-rib/from-identifiers-and-labels (list ?pattern) (list label))
	       ?output-expr))
	   (define lexenv.run^
	     ;;Push a  pattern variable  entry to the  lexenv.  The  ellipsis nesting
	     ;;level is 0.
	     (cons (cons label (make-syntactic-binding-descriptor/pattern-variable lex 0))
		   lexenv.run))
	   (define output-expr.core
	     (%chi-expr.core output-expr^ lexenv.run^ lexenv.expand))
	   (build-application no-source
	     (build-lambda no-source
		 (list lex)
	       output-expr.core)
	     (list (build-lexical-reference no-source expr.sym))))))

      ;;The  pattern  is neither  a  standalone  pattern  variable nor  a  standalone
      ;;underscore.   It has  no fender,  which  is equivalent  to having  a "#t"  as
      ;;fender.
      ;;
      (((?pattern ?output-expr) . ?next-clauses)
       (%gen-clause expr.sym literals
		    ?pattern #t #;fender
		    ?output-expr
		    lexenv.run lexenv.expand
		    ?next-clauses))

      ;;The pattern has a fender.
      ;;
      (((?pattern ?fender ?output-expr) . ?next-clauses)
       (%gen-clause expr.sym literals
		    ?pattern ?fender ?output-expr
		    lexenv.run lexenv.expand
		    ?next-clauses))
      ))

  (define (%gen-clause expr.sym literals
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
    ;;   (syntax-dispatch expr.sym pattern))
    ;;
    ;;when there is no fender, build the output form (pseudo-code):
    ;;
    ;;  ((lambda (tmp)
    ;;      (if tmp
    ;;          (output-expr)
    ;;        (match-next-clauses))
    ;;   (syntax-dispatch expr.sym pattern))
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
			 (not (ellipsis-id? (car x))))
		pvars.levels)
	(stx-error pattern.stx "misplaced ellipsis in syntax-case pattern"))
      (let* ((tmp-sym      (generate-lexical-gensym 'tmp))
	     (fender-cond  (%build-fender-conditional expr.sym literals tmp-sym pvars.levels
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
	     (list (build-lexical-reference no-source expr.sym)
		   (build-data no-source pattern.dispatch))))))))

  (define (%build-fender-conditional expr.sym literals tmp-sym pvars.levels
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
	  (altern    (%gen-syntax-case expr.sym literals next-clauses lexenv.run lexenv.expand)))
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
      (map generate-label-gensym ids))
    (define names
      ;;For  each pattern  variable: a  gensym used  as unique  variable name  in the
      ;;lexical environment.
      (map generate-lexical-gensym ids))
    (define levels
      ;;For each pattern variable: an exact integer representing the ellipsis nesting
      ;;level.  See SYNTAX-TRANSFORMER for details.
      (map cdr pvars.levels))
    (define bindings
      ;;For each pattern variable: a binding to be pushed on the lexical environment.
      (map (lambda (label name level)
	     (cons label (make-syntactic-binding-descriptor/pattern-variable name level)))
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
			  (make-rib/from-identifiers-and-labels ids labels)
			expr.stx)
		      (append bindings lexenv.run)
		      lexenv.expand))
    (build-application no-source
      (build-primref no-source 'apply)
      (list (build-lambda no-source names expr.core)
	    (build-lexical-reference no-source tmp-sym))))

  (define (%invalid-ids-error id* e description.str)
    (let find ((id* id*)
	       (ok* '()))
      (if (null? id*)
	  (stx-error e "syntax error") ; shouldn't happen
	(if (identifier? (car id*))
	    (if (bound-id-member? (car id*) ok*)
		(syntax-violation __module_who__
		  (string-append "duplicate " description.str) (car id*))
	      (find (cdr id*) (cons (car id*) ok*)))
	  (syntax-violation __module_who__
	    (string-append "invalid " description.str) (car id*))))))

  (define (%chi-expr.core expr.stx lexenv.run lexenv.expand)
    (psi.core-expr (chi-expr expr.stx lexenv.run lexenv.expand)))

  #| end of module: SYNTAX-CASE-TRANSFORMER |# )


;;;; module core-macro-transformer: SPLICE-FIRST-EXPAND

(define-core-transformer (splice-first-expand input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's SPLICE-FIRST-EXPAND  syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of  the given LEXENV; return  a PSI object containing  an instance of
  ;;"splice-first-envelope".
  ;;
  (syntax-match input-form.stx ()
    ((_ ?form)
     (make-psi input-form.stx
	       (internal-body
		 (import SPLICE-FIRST-ENVELOPE)
		 (make-splice-first-envelope ?form))))
    ))


;;;; module core-macro-transformer: INTERNAL-BODY

(define-core-transformer (internal-body input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's INTERNAL-BODY  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context  of the  given LEXENV;  return  a PSI  object containing  an instance  of
  ;;"splice-first-envelope".
  ;;
  (syntax-match input-form.stx ()
    ((_ ?body ?body* ...)
     (chi-internal-body input-form.stx lexenv.run lexenv.expand
			(cons ?body ?body*)))
    ))


;;;; module core-macro-transformer: PREDICATE-PROCEDURE-ARGUMENT-VALIDATION, PREDICATE-RETURN-VALUE-VALIDATION

(define-core-transformer (predicate-procedure-argument-validation input-form.stx lexenv.run lexenv.expand)
  ;;Transformer        function         used        to         expand        Vicare's
  ;;PREDICATE-PROCEDURE-ARGUMENT-VALIDATION  macros  from   the  top-level  built  in
  ;;environment.  Expand the  contents of INPUT-FORM.STX in the context  of the given
  ;;LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (chi-expr (cond ((parametrise ((current-run-lexenv (lambda () lexenv.run)))
			(predicate-assertion-procedure-argument-validation ?id)))
		     (else
		      (%synner "undefined procedure argument validation")))
	       lexenv.run lexenv.expand))
    ))

(define-core-transformer (predicate-return-value-validation input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's PREDICATE-RETURN-VALUE-VALIDATION
  ;;macros  from  the  top-level  built  in  environment.   Expand  the  contents  of
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (chi-expr (cond ((parametrise ((current-run-lexenv (lambda () lexenv.run)))
			(predicate-assertion-return-value-validation ?id)))
		     (else
		      (%synner "undefined return value validation")))
	       lexenv.run lexenv.expand))
    ))


;;;; module core-macro-transformer: STRUCT-TYPE-DESCRIPTOR, RECORD-{TYPE,CONSTRUCTOR}-DESCRIPTOR, TYPE-DESCRIPTOR

(module (struct-type-descriptor-transformer
	 record-type-descriptor-transformer
	 record-constructor-descriptor-transformer
	 type-descriptor-transformer)

  (define-core-transformer (struct-type-descriptor input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function  used to  expand STRUCT-TYPE-DESCRIPTOR syntaxes  from the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI object.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?type-id)
       (let ((sts (id->struct-type-specification __who__ input-form.stx ?type-id lexenv.run)))
	 (%make-struct-type-descriptor input-form.stx lexenv.run lexenv.expand sts)))))


  (define-core-transformer (record-type-descriptor input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function  used to  expand RECORD-TYPE-DESCRIPTOR syntaxes  from the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI object.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?type-id)
       (let ((rts (id->record-type-specification __who__ input-form.stx ?type-id lexenv.run)))
	 (%make-record-type-descriptor input-form.stx lexenv.run lexenv.expand rts)))))

  (define-core-transformer (record-constructor-descriptor input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand RECORD-CONSTRUCTOR-DESCRIPTOR syntaxes from
    ;;the top-level built in environment.  Expand the syntax object INPUT-FORM.STX in
    ;;the context of the given LEXENV; return a PSI object.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?type-id)
       (let* ((rts       (id->record-type-specification __who__ input-form.stx ?type-id lexenv.run))
	      (expr.stx  (record-type-spec.rcd-id rts))
	      (expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand)))
	 (make-psi input-form.stx
		   (psi.core-expr expr.psi)
		   (make-type-signature/single-value (core-prim-id '<record-constructor-descriptor>)))))
      ))

  (define-core-transformer (type-descriptor input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand TYPE-DESCRIPTOR syntaxes from the top-level
    ;;built in environment.   Expand the syntax object INPUT-FORM.STX  in the context
    ;;of the given LEXENV; return a PSI object.
    ;;
    ;;The result must be an expression evaluating to:
    ;;
    ;;* A  struct-type descriptor if the  given identifier argument is  a struct-type
    ;;  name.
    ;;
    ;;* A  record-type descriptor if the  given identifier argument is  a record-type
    ;;  name.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?type-id)
       (let ((ots (id->object-type-specification __who__ input-form.stx ?type-id lexenv.run)))
	 (cond ((record-type-spec? ots)
		(%make-record-type-descriptor input-form.stx lexenv.run lexenv.expand ots))
	       ((struct-type-spec? ots)
		(%make-struct-type-descriptor input-form.stx lexenv.run lexenv.expand ots))
	       (else
		(%synner "expected type identifier representing a struct-type name or a record-type name" ?type-id)))))
      ))

;;; --------------------------------------------------------------------

  (define (%make-struct-type-descriptor input-form.stx lexenv.run lexenv.expand
					sts)
    (let* ((std (struct-type-spec.std sts)))
      (make-psi input-form.stx
		(build-data no-source std)
		(make-type-signature/single-value (core-prim-id '<struct-type-descriptor>)))))

  (define (%make-record-type-descriptor input-form.stx lexenv.run lexenv.expand
					rts)
    (let* ((expr.stx  (record-type-spec.rtd-id rts))
	   (expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand)))
      (make-psi input-form.stx
		(psi.core-expr expr.psi)
		(make-type-signature/single-value (core-prim-id '<record-type-descriptor>)))))

  #| end of module |# )


;;;; module core-macro-transformer: EXPANSION-OF

(define-core-transformer (expansion-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  EXPANSION-OF  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ;;Special case to allow easy inspection of definitions.  We transform:
    ;;
    ;;   (define . ?stuff)
    ;;
    ;;into:
    ;;
    ;;   (internal-body (define . ?stuff) (void))
    ;;
    ((_ (?define . ?stuff))
     (and (identifier? ?define)
	  (or (~free-identifier=? ?define (core-prim-id 'define))
	      (~free-identifier=? ?define (core-prim-id 'define*))))
     (let* ((expr.stx `(,(core-prim-id 'internal-body)
			(,?define . ,?stuff)
			(,(core-prim-id 'void))))
	    (expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand))
	    (expr.core (psi.core-expr expr.psi))
	    (expr.sexp (core-language->sexp expr.core)))
       (let* ((out.sexp (map (lambda (bind*)
			       (list 'define (car bind*) (cadr bind*)))
			  (cadr expr.sexp)))
	      (out.sexp (if (= 1 (length out.sexp))
			    (car out.sexp)
			  (cons 'begin out.sexp))))
	 (make-psi input-form.stx
		   (build-data no-source
		     out.sexp)
		   (make-type-signature/single-top)))))

    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi.core-expr expr.psi))
	    (expr.sexp (core-language->sexp expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-type-signature/single-top))))
    ))

(define-core-transformer (expansion-of* input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's EXPANSION-OF*  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr0 ?expr* ...)
     (chi-expr (bless `(expansion-of (internal-body ,?expr0 ,@?expr* (void))))
	       lexenv.run lexenv.expand))
    ))


;;;; module core-macro-transformer: VISIT-CODE-OF

(define-core-transformer (visit-code-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's VISIT-CODE-OF  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (case-identifier-syntactic-binding-descriptor (__who__ input-form.stx ?id lexenv.run)
       ((local-macro local-macro!)
	(let* ((descr.value   (syntactic-binding-descriptor.value __descr__))
	       (expanded-expr (cdr descr.value)))
	  (make-psi input-form.stx
		    (build-data no-source
		      (core-language->sexp expanded-expr))
		    (make-type-signature/single-top))))
       (else
	(%synner "expected identifier of local macro" ?id))))
    ))


;;;; module core-macro-transformer: OPTIMISATION-OF, OPTIMISATION-OF*, FURTHER-OPTIMISATION-OF, FURTHER-OPTIMISATION-OF*

(define-core-transformer (optimisation-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  Vicare's OPTIMISATION-OF syntaxes  from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi.core-expr expr.psi))
	    (expr.sexp (compiler::core-expr->optimized-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-type-signature/single-top))))
    ))

(define-core-transformer (optimisation-of* input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand Vicare's OPTIMISATION-OF*  syntaxes from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr0 ?expr* ...)
     (let* ((expr.psi  (chi-expr (bless `(internal-body ,?expr0 ,@?expr* (void)))
				 lexenv.run lexenv.expand))
	    (expr.core (psi.core-expr expr.psi))
	    (expr.sexp (compiler::core-expr->optimized-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-type-signature/single-top))))
    ))

(define-core-transformer (further-optimisation-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's FURTHER-OPTIMISATION-OF  syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi.core-expr expr.psi))
	    (expr.sexp (compiler::core-expr->optimisation-and-core-type-inference-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-type-signature/single-top))))
    ))

(define-core-transformer (further-optimisation-of* input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's  FURTHER-OPTIMISATION-OF* syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr0 ?expr* ...)
     (let* ((expr.psi  (chi-expr (bless `(internal-body ,?expr0 ,@?expr* (void)))
				 lexenv.run lexenv.expand))
	    (expr.core (psi.core-expr expr.psi))
	    (expr.sexp (compiler::core-expr->optimisation-and-core-type-inference-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-type-signature/single-top))))
    ))


;;;; module core-macro-transformer: ASSEMBLY-OF

(define-core-transformer (assembly-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  Vicare's  ASSEMBLY-OF syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI object.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi.core-expr expr.psi))
	    (expr.sexp (compiler::core-expr->assembly-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-type-signature/single-top))))
    ))


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;fill-column: 85
;;eval: (put 'build-library-letrec*		'scheme-indent-function 1)
;;eval: (put 'build-application			'scheme-indent-function 1)
;;eval: (put 'build-conditional			'scheme-indent-function 2)
;;eval: (put 'build-lambda			'scheme-indent-function 2)
;;eval: (put 'build-foreign-call		'scheme-indent-function 2)
;;eval: (put 'build-global-assignment		'scheme-indent-function 1)
;;eval: (put 'build-lexical-assignment		'scheme-indent-function 1)
;;eval: (put 'build-letrec*			'scheme-indent-function 1)
;;eval: (put 'build-data			'scheme-indent-function 1)
;;eval: (put 'core-lang-builder			'scheme-indent-function 1)
;;eval: (put 'case-object-type-binding		'scheme-indent-function 1)
;;eval: (put 'push-lexical-contour		'scheme-indent-function 1)
;;eval: (put 'syntactic-binding-getprop		'scheme-indent-function 1)
;;eval: (put 'sys::syntax-case			'scheme-indent-function 2)
;;eval: (put '$map-in-order			'scheme-indent-function 1)
;;eval: (put '%maybe-push-annotated-expr-on-lambda-input-form		'scheme-indent-function 1)
;;eval: (put '%maybe-push-annotated-expr-on-case-lambda-input-form	'scheme-indent-function 1)
;;End:
