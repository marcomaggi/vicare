;;;Copyright (c) 2010-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; stuff

(import (prefix (rnrs syntax-case) sys.)
  (only (psyntax.syntax-utilities)
	generate-temporaries))

(define-syntax (define-core-transformer stx)
  (sys.syntax-case stx ()
    ((_ (?who ?input-form.stx ?lexenv.run ?lexenv.expand) ?body0 ?body ...)
     (let* ((who.sym (sys.syntax->datum (sys.syntax ?who)))
	    (who.str (symbol->string who.sym))
	    (who.out (string->symbol (string-append who.str "-transformer"))))
       (sys.with-syntax
	   ((WHO    (sys.datum->syntax (sys.syntax ?who) who.out))
	    (SYNNER (sys.datum->syntax (sys.syntax ?who) '%synner)))
	 (sys.syntax
	  (define (WHO ?input-form.stx ?lexenv.run ?lexenv.expand)
	    (with-who ?who
	      (define SYNNER
		(case-lambda
		 ((message)
		  (SYNNER message #f))
		 ((message subform)
		  (syntax-violation __who__ message ?input-form.stx subform))))
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


;;;; helpers

(define (%fxiota fx item*)
  (if (pair? item*)
      (cons fx (%fxiota (fxadd1 fx) (cdr item*)))
    '()))


;;;; helpers: struct types

(define (%struct-field-name->struct-field-idx who input-form.stx field-names field-id)
  ;;Given a list  of symbols FIELD-NAMES representing a struct's  list of field names
  ;;and an identifier FIELD-ID representing the name  of a field: return the index of
  ;;the selected field in the list.
  ;;
  (define field-sym (identifier->symbol field-id))
  (let loop ((i 0) (ls field-names))
    (if (pair? ls)
	(if (eq? field-sym ($car ls))
	    i
	  (loop ($fxadd1 i) ($cdr ls)))
      (syntax-violation who "invalid struct type field name" input-form.stx field-id))))

(define (%struct-type-id->std who input-form.stx type-id lexenv.run)
  ;;Given the syntactic identifier TYPE-ID  representing a struct-type name and bound
  ;;to a  struct-type descriptor (STD):  find its  label, then its  syntactic binding
  ;;descriptor,  finally return  the struct-type  descriptor itself.   If no  binding
  ;;captures the  identifier or  the binding  does not  describe a  struct-type name:
  ;;raise an exception.
  ;;
  (let* ((label (id->label/or-error who input-form.stx type-id))
	 (descr (label->syntactic-binding-descriptor label lexenv.run)))
    (if (struct-type-name-binding-descriptor? descr)
	(struct-type-name-binding-descriptor.type-descriptor descr)
      (syntax-violation who "not a struct type" input-form.stx type-id))))


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
    ((named-lambda)				named-lambda-transformer)
    ((named-case-lambda)			named-case-lambda-transformer)
    ((internal-lambda)				internal-lambda-transformer)
    ((internal-case-lambda)			internal-case-lambda-transformer)
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

    ((internal-run-time-is-a?)			internal-run-time-is-a?-transformer)

    ((unsafe-cast)				unsafe-cast-transformer)
    ((validate-typed-procedure-argument)	validate-typed-procedure-argument-transformer)
    ((validate-typed-return-value)		validate-typed-return-value-transformer)
    ((assert-retvals-signature)			assert-retvals-signature-transformer)
    ((assert-retvals-signature-and-return)	assert-retvals-signature-and-return-transformer)

    ((type-of)					type-of-transformer)
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

(include "psyntax.chi-procedures.generic-type-syntaxes.scm" #t)


(module PROCESSING-UTILITIES-FOR-LISTS-OF-BINDINGS
  (%expand-rhs*
   %select-lhs-declared-tag-or-rhs-inferred-tag
   %compose-lhs-specification)
  ;;In  this  module  we  assume  the  argument  INPUT-FORM.STX  can  be  matched  by
  ;;SYNTAX-MATCH patterns like:
  ;;
  ;;  (let            ((?lhs* ?rhs*) ...) . ?body*)
  ;;  (let     ?recur ((?lhs* ?rhs*) ...) . ?body*)
  ;;
  ;;where the ?LHS identifiers can be tagged or not; we have to remember that LET* is
  ;;just expanded in  a set of nested  LET syntaxes.  We assume that  the ?LHS syntax
  ;;objects have been processed with:
  ;;
  ;;   (receive (lhs*.id lhs*.tag)
  ;;       (syntax-object.parse-list-of-typed-bindings ?lhs* input-form.stx)
  ;;     ...)
  ;;
  ;;We want to provide helper functions to handle the following situations.
  ;;
  ;;
  ;;RHS tag propagation
  ;;-------------------
  ;;
  ;;When we write:
  ;;
  ;;   (let ((a 1)) . ?body)
  ;;
  ;;the identifier A is the left-hand side of the binding and the expression 1 is the
  ;;right-hand side  of the  binding; since  A is  untagged in  the source  code, the
  ;;expander will tag it, by default, with "<top>".
  ;;
  ;;* If  the option  RHS-TAG-PROPAGATION? is  turned OFF: the  identifier A  is left
  ;;  tagged with "<top>".  We are free to assign any object to A, mutating the bound
  ;;  value  multiple times with  objects of different  tag; this is  standard Scheme
  ;;  behaviour.
  ;;
  ;;* When the option RHS-TAG-PROPAGATION? is turned ON: the expander infers that the
  ;;  RHS has  signature "(<fixnum>)", so it  propagates the tag from the  RHS to the
  ;;  LHS overriding "<top>" with "<fixnum>".  This  will cause an error to be raised
  ;;  if we mutate the binding assigning to A an object whose tag is not "<fixnum>".
  ;;
  ;;
  ;;RHS signature validation
  ;;------------------------
  ;;
  ;;When we write:
  ;;
  ;;   (let (({a <fixnum>} 1)) . ?body)
  ;;
  ;;the identifier A is the left-hand side of the binding and the expression 1 is the
  ;;right-hand side of the binding; A is explicitly tagged with "<fixnum>".
  ;;
  ;;We want to make  sure that the RHS expression returns a  single return value with
  ;;signature "<fixnum>"; so  the RHS's retvals signature must  be "(<fixnum>)".  All
  ;;the work  is done  by the  macro ASSERT-RETVALS-SIGNATURE-AND-RETURN, so  we transform  the RHS
  ;;expression as if the input form is:
  ;;
  ;;   (let (({a <fixnum>} (assert-retvals-signature-and-return (<fixnum>) 1))) . ?body)
  ;;
  ;;and expand the new RHS:
  ;;
  ;;   (assert-retvals-signature-and-return (<fixnum>) 1)
  ;;
  ;;if the expander  determines that the signature  of 1 is "(<fixnum>)",  the RHS is
  ;;transformed at expand-time into just "1";  otherwise a run-time object type check
  ;;is inserted.  In  any case we can  be sure at both expand-time  and run-time that
  ;;the signature  of the  identifier A  is correct, otherwise  an exception  will be
  ;;raised before expanding or running the ?BODY.
  ;;

  (define* (%expand-rhs* input-form.stx lexenv.run lexenv.expand
			 lhs*.tag rhs*.stx)
    ;;Expand  a  list  of  right-hand  sides  from  bindings  in  the  syntax  object
    ;;INPUT-FORM.STX; the context  of the expansion is described by  the given LEXENV
    ;;arguments.
    ;;
    ;;LHS*.TAG must be a list of tag identifiers representing the tags resulting from
    ;;parsing the left-hand sides in the source code.
    ;;
    ;;RHS*.STX must be a list of syntax objects representing the expressions from the
    ;;right-hand sides.
    ;;
    ;;Return 2 values: a list of  PSI structures representing the expanded right-hand
    ;;sides; a list of tag identifiers  representing the signatures of the right-hand
    ;;sides.  If  the RHS  are found,  at expand-time,  to return  zero, two  or more
    ;;values: a synatx violation is raised.
    ;;
    ;;The tag identifiers  in the second returned  value can be used  to override the
    ;;ones in LHS*.TAG.
    ;;
    (define rhs*.psi
      (map (lambda (rhs.stx lhs.tag)
	     ;;If LHS.TAG is "<top>", we still want to use the assert and return form
	     ;;to  make sure  that  a single  value is  returned.   If the  signature
	     ;;validation  succeeds at  expand-time:  the returned  PSI  has the  RHS
	     ;;signature  inferred from  the original  RHS.STX, not  "(<top>)".  This
	     ;;allows us to propagate the tag from RHS to LHS.
	     (chi-expr (bless
			`(assert-retvals-signature-and-return (,lhs.tag) ,rhs.stx))
		       lexenv.run lexenv.expand))
	rhs*.stx lhs*.tag))
    (define rhs*.sig
      (map psi-retvals-signature rhs*.psi))
    (define rhs*.tag
      (map (lambda (sig)
	     (syntax-match (retvals-signature.tags sig) ()
	       ((?tag)
		;;Single return value: good.
		?tag)
	       (_
		;;If we  are here it  means that the ASSERT-RETVALS-SIGNATURE-AND-RETURN  above has
		;;misbehaved.
		(assertion-violation/internal-error __who__
		  "invalid retvals signature" (syntax->datum input-form.stx) sig))))
	rhs*.sig))
    (values rhs*.psi rhs*.tag))

  (define (%select-lhs-declared-tag-or-rhs-inferred-tag lhs*.tag rhs*.inferred-tag)
    ;;Given  a list  of  LHS tags  LHS*.tag  from the  source code  and  the list  of
    ;;corresponding RHS inferred tags RHS*.INFERRED-TAG: return a list of LHS tags to
    ;;replace LHS*.TAG.
    ;;
    (if (option.typed-language.rhs-tag-propagation?)
	(map (lambda (lhs.tag rhs.inferred-tag)
	       (if (top-tag-id? lhs.tag)
		   (if (top-tag-id? rhs.inferred-tag)
		       lhs.tag
		     rhs.inferred-tag)
		 lhs.tag))
	  lhs*.tag rhs*.inferred-tag)
      lhs*.tag))

  (define (%compose-lhs-specification lhs*.id lhs*.tag rhs*.inferred-tag)
    ;;For every LHS identifier build a tagged identifier syntax:
    ;;
    ;;   (brace ?lhs.id ?tag)
    ;;
    ;;in which ?TAG is either the original one specified in the LET syntax or the one
    ;;inferred by expanding the RHS.  If there  is no tag explicitly specified in the
    ;;LET syntax we put in the inferred one.
    ;;
    (if (option.typed-language.rhs-tag-propagation?)
	(map (lambda (lhs.id lhs.tag rhs.tag)
	       (bless
		`(brace ,lhs.id ,(if (top-tag-id? lhs.tag)
				     (if (top-tag-id? rhs.tag)
					 lhs.tag
				       rhs.tag)
				   lhs.tag))))
	  lhs*.id lhs*.tag rhs*.inferred-tag)
      (map (lambda (lhs.id lhs.tag)
	     (bless
	      `(brace ,lhs.id ,lhs.tag)))
	lhs*.id lhs*.tag)))

  #| end of module: PROCESSING-UTILITIES-FOR-LISTS-OF-BINDINGS |# )


;;;; module core-macro-transformer: IF

(define-core-transformer (if input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS  IF syntaxes from the top-level built in
  ;;environment.  Expand the syntax object INPUT-FORM.STX in the context of the given
  ;;LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?test ?consequent ?alternate)
     (let ((test.psi       (chi-expr ?test       lexenv.run lexenv.expand))
	   (consequent.psi (chi-expr ?consequent lexenv.run lexenv.expand))
	   (alternate.psi  (chi-expr ?alternate  lexenv.run lexenv.expand)))
       (make-psi input-form.stx
		 (build-conditional no-source
		   (psi-core-expr test.psi)
		   (psi-core-expr consequent.psi)
		   (psi-core-expr alternate.psi))
		 (retvals-signatures-common-ancestor (psi-retvals-signature consequent.psi)
						     (psi-retvals-signature alternate.psi)))))
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
		   (psi-core-expr test.psi)
		   (psi-core-expr consequent.psi)
		   (build-void))
		 (make-retvals-signature/single-top))))
    ))


;;;; module core-macro-transformer: QUOTE

(define-core-transformer (quote input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS  QUOTE syntaxes from the top-level built
  ;;in environment.   Expand the syntax object  INPUT-FORM.STX in the context  of the
  ;;given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?datum)
     (let ((datum (syntax->datum ?datum)))
       (make-psi input-form.stx
		 (build-data no-source datum)
		 (datum-retvals-signature datum))))
    ))


;;;; module core-macro-transformer: LAMBDA, CASE-LAMBDA, NAMED-LAMBDA, NAMED-CASE-LAMBDA, INTERNAL-LAMBDA, INTERNAL-CASE-LAMBDA

(define-core-transformer (case-lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  R6RS CASE-LAMBDA syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return an PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?formals* ?body* ?body** ...) ...)
     (chi-case-lambda input-form.stx lexenv.run lexenv.expand
		      '(safe) (underscore-id) ?formals* (map cons ?body* ?body**)))
    ))

(define-core-transformer (lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS LAMBDA syntaxes from the top-level built
  ;;in environment.   Expand the syntax object  INPUT-FORM.STX in the context  of the
  ;;given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?formals ?body ?body* ...)
     (chi-lambda input-form.stx lexenv.run lexenv.expand
		 '(safe) (underscore-id) ?formals (cons ?body ?body*)))
    ))

;;; --------------------------------------------------------------------

(define-core-transformer (named-case-lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  Vicare's NAMED-CASE-LAMBDA syntaxes from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return an PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?who (?formals* ?body* ?body** ...) ...)
     (identifier? ?who)
     (chi-case-lambda input-form.stx lexenv.run lexenv.expand
		      '(safe) ?who ?formals* (map cons ?body* ?body**)))
    ))

(define-core-transformer (named-lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  NAMED-LAMBDA  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?who ?formals ?body ?body* ...)
     (identifier? ?who)
     (chi-lambda input-form.stx lexenv.run lexenv.expand
		 '(safe) ?who ?formals (cons ?body ?body*)))
    ))

;;; --------------------------------------------------------------------

(define-core-transformer (internal-case-lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  INTERNAL-CASE-LAMBDA syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return an PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?attributes (?formals* ?body* ?body** ...) ...)
     (chi-case-lambda input-form.stx lexenv.run lexenv.expand
		      ?attributes #f ?formals* (map cons ?body* ?body**)))
    ))

(define-core-transformer (internal-lambda input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  Vicare's INTERNAL-LAMBDA syntaxes  from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?attributes ?formals ?body ?body* ...)
     (chi-lambda input-form.stx lexenv.run lexenv.expand
		 ?attributes #f ?formals (cons ?body ?body*)))
    ))


;;;; module core-macro-transformer: LET

(module (let-transformer)
  ;;Transformer function used  to expand R6RS LET macros with  Vicare extensions from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return an PSI struct.
  ;;
  ;;In practice, below we convert the UNnamed standard syntax:
  ;;
  ;;   (let ((?lhs ?rhs) ...) . ?body)
  ;;
  ;;into the core language syntax:
  ;;
  ;;   (let ((?lhs ?rhs) ...) . ?body)
  ;;
  ;;and the named standard syntax:
  ;;
  ;;   (let ?recur ((?lhs ?rhs) ...) . ?body)
  ;;
  ;;into the extended syntax:
  ;;
  ;;   (internal-body
  ;;     (define (?recur ?lhs ...) . ?body)
  ;;     (?recur ?rhs ...))
  ;;
  ;;for further expansion, notice that the latter allows ?RECUR to be tagged with the
  ;;return values of the LET form.
  ;;
  ;;When expanding UNnamed LET syntaxes:
  ;;
  ;;1. We parse the LHS tagged identifiers to acquire the declared tags.
  ;;
  ;;2. We expand the ?RHS expression to acquire their retvals signature.
  ;;
  ;;3. We  select the  more specific  LHS tag between  the declared  LHS one  and the
  ;;   inferred RHS one.
  ;;
  ;;3. We expand the body with the LHS identifiers correctly tagged.
  ;;
  ;;This way if we write:
  ;;
  ;;   (let ((a 1)) . ?body)
  ;;
  ;;this is what happens:
  ;;
  ;;1..The identifier A is first tagged with "<top>".
  ;;
  ;;2..The expander figures out that the RHS's signature is "(<fixnum>)".
  ;;
  ;;3..The expander overrides the tag of A to be "<fixnum>".
  ;;
  ;;4..In the ?BODY the identifier A is  tagged as "<fixnum>", so the extended syntax
  ;;   is available.
  ;;
  ;;On the other hand if we write:
  ;;
  ;;   (let (({a <exact-integer>} 1)) . ?body)
  ;;
  ;;we get an expansion that is equivalent to:
  ;;
  ;;   (let (({a <exact-integer>} (assert-retvals-signature-and-return (<exact-integer>) 1)))
  ;;     . ?body)
  ;;
  ;;so  the type  of the  RHS expression  is validated  either at  expand-time or  at
  ;;run-time.
  ;;
  ;;HISTORICAL NOTE In the original Ikarus code, the UNnamed LET syntax:
  ;;
  ;;   (let ((?lhs ?rhs) ...) . ?body)
  ;;
  ;;was transformed into:
  ;;
  ;;   ((lambda (?lhs ...) . ?body) ?rhs ...)
  ;;
  ;;and the named syntax:
  ;;
  ;;   (let ?recur ((?lhs ?rhs) ...) . ?body)
  ;;
  ;;into:
  ;;
  ;;   ((letrec ((?recur (lambda (?lhs ...) . ?body))) ?recur) ?rhs ...)
  ;;
  ;;such transformations are fine for an  UNtagged language.  In a tagged language we
  ;;want to use types  whenever possible, and this means to use  the DEFINE syntax to
  ;;define both a safe and an unsafe function.  (Marco Maggi; Sun Apr 27, 2014)
  ;;
  (define-syntax __who__
    (identifier-syntax 'let))

  (import PROCESSING-UTILITIES-FOR-LISTS-OF-BINDINGS)

  (define (let-transformer input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       (receive (lhs*.id lhs*.declared-tag)
	   (syntax-object.parse-list-of-typed-bindings ?lhs* input-form.stx)
	 (receive (rhs*.psi rhs*.inferred-tag)
	     (%expand-rhs* input-form.stx lexenv.run lexenv.expand lhs*.declared-tag ?rhs*)
	   (let ((lhs*.lex           (map generate-lexical-gensym lhs*.id))
		 (lhs*.lab           (map generate-label-gensym   lhs*.id))
		 (lhs*.inferred-tag  (%select-lhs-declared-tag-or-rhs-inferred-tag lhs*.declared-tag rhs*.inferred-tag)))
	     (let* ((body*.stx  (cons ?body ?body*))
		    (body.psi   (%expand-unnamed-let-body body*.stx lexenv.run lexenv.expand
							  lhs*.id lhs*.lab lhs*.lex lhs*.inferred-tag))
		    (body.core  (psi-core-expr body.psi))
		    (rhs*.core  (map psi-core-expr rhs*.psi)))
	       (make-psi input-form.stx
			 (build-let (syntax-annotation input-form.stx)
				    lhs*.lex rhs*.core
				    body.core)
			 (psi-retvals-signature body.psi)))))))

      ((_ ?recur ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;NOTE We want an implementation in which:  when BREAK is not used, the escape
       ;;function is  never referenced, so  the compiler can remove  CALL/CC.  Notice
       ;;that here binding  CONTINUE makes no sense, because calling  ?RECUR does the
       ;;job.
       (receive (recur.id recur.tag)
	   (syntax-object.parse-typed-argument ?recur)
	 (chi-expr (bless
		    `(internal-body
		       ;;We do not want "__who__" and RETURN to be bound here.
		       (internal-define () ,?recur
			   (internal-lambda (safe) ,?lhs* ,?body . ,?body*))
		       (,recur.id . ,?rhs*)))
		   lexenv.run lexenv.expand)))

      (_
       (syntax-violation __who__ "invalid syntax" input-form.stx))))

  (define (%expand-unnamed-let-body body*.stx lexenv.run lexenv.expand
				    lhs*.id lhs*.lab lhs*.lex lhs*.inferred-tag)
    ;;Generate what  is needed  to create a  lexical contour: a  RIB and  an extended
    ;;lexical environment in  which to evaluate the body.  Expand  the body forms and
    ;;return a single PSI struct representing the full body.
    (let ((body*.stx^   (push-lexical-contour
			    (make-rib/from-identifiers-and-labels lhs*.id lhs*.lab)
			  body*.stx))
	  (lexenv.run^  (lexenv-add-lexical-typed-var-bindings lhs*.lab lhs*.lex lhs*.inferred-tag lexenv.run)))
      (chi-internal-body #f lexenv.run^ lexenv.expand body*.stx^)))

  #| end of module: LET-TRANSFORMER |# )


;;;; module core-macro-transformer: LETREC and LETREC*

(module (letrec-transformer letrec*-transformer)
  ;;Transformer  functions  used to  expand  LETREC  and  LETREC* syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
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
  ;;NOTE Unfortunately, with recursive bindings we cannot implement coherent RHS type
  ;;propagation.  Let's think of:
  ;;
  ;;   (letrec ((a (some-stuff ... a ...)))
  ;;     ?body)
  ;;
  ;;we could infer  the returned type of the  RHS and use it while  expand the ?BODY,
  ;;but what about the RHS?  While expanding the RHS the identifier A would be tagged
  ;;as "<top>" and while expanding the ?BODY it would be tagged as "<whatever>"; this
  ;;is incoherent.
  ;;
  (define-syntax __who__
    (identifier-syntax 'letrec-transformer))

  (define (letrec-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand LETREC syntaxes from the top-level built in
    ;;environment.  Expand  the syntax  object INPUT-FORM.STX in  the context  of the
    ;;given LEXENV; return a PSI struct.
    ;;
    (%letrec-helper input-form.stx lexenv.run lexenv.expand build-letrec))

  (define (letrec*-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used  to expand LETREC* syntaxes from  the top-level built
    ;;in environment.  Expand the syntax object  INPUT-FORM.STX in the context of the
    ;;given LEXENV; return a PSI struct.
    ;;
    (%letrec-helper input-form.stx lexenv.run lexenv.expand build-letrec*))

  (define* (%letrec-helper input-form.stx lexenv.run lexenv.expand core-lang-builder)
    (import PROCESSING-UTILITIES-FOR-LISTS-OF-BINDINGS)
    (syntax-match input-form.stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check that the binding names are identifiers and without duplicates.
       (receive (lhs*.id lhs*.tag)
	   (syntax-object.parse-list-of-typed-bindings ?lhs* input-form.stx)
	 ;;Generate unique variable names and labels for the LETREC bindings.
	 (let ((lhs*.lex (map generate-lexical-gensym lhs*.id))
	       (lhs*.lab (map generate-label-gensym   lhs*.id)))
	   ;;Generate  what is  needed to  create  a lexical  contour: a  rib and  an
	   ;;extended lexical  environment in which  to evaluate both  the right-hand
	   ;;sides and the body.
	   ;;
	   ;;NOTE The region of all the  LETREC and LETREC* bindings includes all the
	   ;;right-hand sides.
	   (let* ((rib         (make-rib/from-identifiers-and-labels lhs*.id lhs*.lab))
		  (lexenv.run^ (lexenv-add-lexical-typed-var-bindings lhs*.lab lhs*.lex lhs*.tag lexenv.run))
		  (rhs*.psi    (%expand-rhs input-form.stx lexenv.run^ lexenv.expand
					    lhs*.lab lhs*.tag ?rhs* rib))
		  (body*.stx   (cons ?body ?body*))
		  (body.psi    (chi-internal-body #f lexenv.run^ lexenv.expand
						  (push-lexical-contour rib body*.stx))))
	     (let* ((rhs*.core (map psi-core-expr rhs*.psi))
		    (body.core (psi-core-expr body.psi))
		    ;;Build the LETREC or LETREC* expression in the core language.
		    (expr.core (core-lang-builder no-source
				 lhs*.lex
				 rhs*.core
				 body.core)))
	       (make-psi input-form.stx expr.core
			 (psi-retvals-signature body.psi)))))))
      ))

  (define (%expand-rhs input-form.stx lexenv.run lexenv.expand
		       lhs*.lab lhs*.tag rhs*.stx rib)
    ;;Expand  the  right  hand sides  in  RHS*.STX  and  return  a list  holding  the
    ;;corresponding PSI structures.
    ;;
    ($map-in-order
	(lambda (rhs.stx lhs.lab lhs.tag)
	  (receive-and-return (rhs.psi)
	      ;;The LHS*.ID and LHS*.LAB  have been added to the rib,  and the rib is
	      ;;pushed on  the RHS.STX.  So,  while the specific  identifiers LHS*.ID
	      ;;are unbound (because they do not  contain the rib), any occurrence of
	      ;;the binding identifiers in the RHS.STX  is captured by the binding in
	      ;;the rib.
	      ;;
	      ;;If LHS.TAG  is "<top>", we  still want to  use the assert  and return
	      ;;form to make sure that a  single value is returned.  If the signature
	      ;;validation succeeds at expand-time: the returned PSI has the original
	      ;;RHS signature,  not "(<top>)".  This  allows us to propagate  the tag
	      ;;from RHS to LHS.
	      (chi-expr (push-lexical-contour rib
			  (bless
			   `(assert-retvals-signature-and-return (,lhs.tag) ,rhs.stx)))
			lexenv.run lexenv.expand)
	    ;;If the  LHS is untagged:  perform tag propatation  from the RHS  to the
	    ;;LHS.
	    (when (and (option.typed-language.rhs-tag-propagation?)
		       (top-tag-id? lhs.tag))
	      (syntax-match (retvals-signature.tags (psi-retvals-signature rhs.psi)) ()
		((?tag)
		 ;;Single return value: good.
		 (let ((descr (label->syntactic-binding-descriptor lhs.lab lexenv.run)))
		   (lexical-var-binding-descriptor->lexical-typed-var-binding-descriptor! descr ?tag)))

		(_
		 ;;If we  are here it means  that the expansion of  the assertion and
		 ;;return syntax above has misbehaved.
		 (assertion-violation/internal-error __who__
		   "invalid retvals signature"
		   (syntax->datum input-form.stx)
		   (psi-retvals-signature rhs.psi)))))))
      rhs*.stx lhs*.lab lhs*.tag))

  #| end of module |# )


;;;; module core-macro-transformer: FLUID-LET-SYNTAX

(module (fluid-let-syntax-transformer push-fluid-syntax)

  (define-core-transformer (fluid-let-syntax input-form.stx lexenv.run lexenv.expand)
    ;;Transformer  function  used  to   expand  FLUID-LET-SYNTAX  syntaxes  from  the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    ;;FLUID-LET-SYNTAX is similar, but not equal, to LET-SYNTAX; rather than defining
    ;;new  ?LHS bindings,  it temporarily  rebinds the  keywords to  new transformers
    ;;while expanding the ?BODY forms.  The given ?LHS must be already bound to fluid
    ;;syntaxes defined by DEFINE-FLUID-SYNTAX.
    ;;
    ;;There   are   two   differences  between   FLUID-LET-SYNTAX   and   LET-SYNTAX:
    ;;FLUID-LET-SYNTAX must  appear in  expression context  only; the  internal ?BODY
    ;;forms are *not* spliced in the enclosing body.
    ;;
    ;;NOTE We  would truly  like to splice  the inner body  forms in  the surrounding
    ;;body, so that  this syntax could act  like LET-SYNTAX, which is  useful; but we
    ;;really cannot do  it with this implementation of the  expander algorithm.  This
    ;;is because LET-SYNTAX both  creates a new rib and adds  new id/label entries to
    ;;it, and pushes label/descriptor entries to the LEXENV; instead FLUID-LET-SYNTAX
    ;;only pushes  entries to the LEXENV:  there is no  way to keep the  fluid LEXENV
    ;;entries visible only  to a subsequence of  forms in a body.   (Marco Maggi; Tue
    ;;Feb 18, 2014)
    ;;
    (syntax-match input-form.stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check that the ?LHS* are all identifiers with no duplicates.
       (unless (valid-bound-ids? ?lhs*)
	 (error-invalid-formals-syntax input-form.stx ?lhs*))
       (let* ((fluid-label* (map (lambda (lhs)
				   (%lookup-binding-in-lexenv.run lhs lexenv.run %synner))
			      ?lhs*))
	      (binding*     (map (lambda (rhs.stx)
				   (with-exception-handler/input-form
				       rhs.stx
				     (eval-macro-transformer (expand-macro-transformer rhs.stx lexenv.expand)
							     lexenv.run)))
			      ?rhs*))
	      (entry*       (map cons fluid-label* binding*)))
	 (chi-internal-body #f
			    (append entry* lexenv.run)
			    (append entry* lexenv.expand)
			    (cons ?body ?body*))))))

  (define* (push-fluid-syntax lhs.id rhs.stx lexenv.run lexenv.expand synner)
    ;;Push on the LEXENVs the result of defining the single syntactic binding for the
    ;;identifier LHS.ID,  having RHS.STX as  right-hand side expression.   Return two
    ;;values: the updated LEXENV.RUN and LEXENV.EXPAND.
    ;;
    ;;This  function has  been especially  thought to  push new  definitions for  the
    ;;"__who__" fluid syntax.  As example, we can use it as follows:
    ;;
    ;;   (receive (lexenv.run^ lexenv.expand^)
    ;;       (push-fluid-syntax (core-prim-id '__who__)
    ;;                          (bless `(identifier-syntax (quote ,who.id)))
    ;;                          lexenv.run lexenv.expand synner)
    ;;     ---)
    ;;
    (let* ((fluid-label  (%lookup-binding-in-lexenv.run lhs.id lexenv.run synner))
	   (binding      (with-exception-handler/input-form
			     rhs.stx
			   (eval-macro-transformer (expand-macro-transformer rhs.stx lexenv.expand)
						   lexenv.run)))
	   (entry        (cons fluid-label binding)))
      (values (cons entry lexenv.run)
	      (cons entry lexenv.expand))))

  (define (%lookup-binding-in-lexenv.run lhs lexenv.run synner)
    ;;Search the binding of the identifier LHS retrieving its label; if such label is
    ;;present and its  associated syntactic binding descriptor from  LEXENV.RUN is of
    ;;type "fluid  syntax": return  the associated  fluid label that  can be  used to
    ;;rebind the identifier.
    ;;
    (let* ((label    (or (id->label lhs)
			 (synner "unbound identifier" lhs)))
	   (binding  (label->syntactic-binding-descriptor/no-indirection label lexenv.run)))
      (cond ((fluid-syntax-binding-descriptor? binding)
	     (fluid-syntax-binding-descriptor.fluid-label binding))
	    (else
	     (synner "not a fluid identifier" lhs)))))

  #| end of module |# )


;;;; module core-macro-transformer: FOREIGN-CALL

(define-core-transformer (foreign-call input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  FOREIGN-CALL  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?name ?arg* ...)
     (let* ((name.psi  (chi-expr  ?name lexenv.run lexenv.expand))
	    (arg*.psi  (chi-expr* ?arg* lexenv.run lexenv.expand))
	    (expr.core (build-foreign-call no-source
			 (psi-core-expr name.psi)
			 (map psi-core-expr arg*.psi))))
       (make-psi input-form.stx expr.core)))
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
  (define (syntax-transformer use-stx lexenv.run lexenv.expand)
    (syntax-match use-stx ()
      ((_ ?template)
       (receive (intermediate-sexp maps)
	   (%gen-syntax use-stx ?template lexenv.run '() ellipsis? #f)
	 (let ((code (%generate-output-code intermediate-sexp)))
	   #;(debug-print 'syntax (syntax->datum ?template) intermediate-sexp code)
	   (make-psi use-stx code))))
      ))

  (define-module-who syntax)

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
       (syntax-violation __module_who__ "misplaced ellipsis in syntax form" use-stx))

      ;;Match a standalone  identifier.  ?ID can be: a reference  to pattern variable
      ;;created by SYNTAX-CASE; an identifier that  will be captured by some binding;
      ;;an  identifier  that will  result  to  be free,  in  which  case an  "unbound
      ;;identifier" error will be raised later.
      ;;
      (?id
       (identifier? ?id)
       (let ((binding (label->syntactic-binding-descriptor (id->label ?id) lexenv)))
	 (if (pattern-variable-binding-descriptor? binding)
	     ;;It is a reference to pattern variable.
	     (receive (var maps)
		 (let* ((name.level  (syntactic-binding-descriptor.value binding))
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
	   (syntax-violation __module_who__ "misplaced ellipsis in syntax form" use-stx)
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
			      (syntax-violation __module_who__ "extra ellipsis in syntax form" use-stx)
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
				(syntax-violation __module_who__ "extra ellipsis in syntax form" use-stx)
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
	  (syntax-violation __module_who__ "missing ellipsis in syntax form" use-stx)
	(receive (outer-var outer-maps)
	    (%gen-ref use-stx var (- level 1) (cdr maps))
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
       (%verify-literals ?literal* input-form.stx)
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
	    (not (ellipsis? ?pattern)))
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
			 (not (ellipsis? (car x))))
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
    (psi-core-expr (chi-expr expr.stx lexenv.run lexenv.expand)))

  #| end of module: SYNTAX-CASE-TRANSFORMER |# )


;;;; module core-macro-transformer: SPLICE-FIRST-EXPAND

(define-core-transformer (splice-first-expand input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's SPLICE-FIRST-EXPAND  syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of  the given LEXENV; return  a PSI struct containing  an instance of
  ;;"splice-first-envelope".
  ;;
  (syntax-match input-form.stx ()
    ((_ ?form)
     (make-psi input-form.stx
	       (let ()
		 (import SPLICE-FIRST-ENVELOPE)
		 (make-splice-first-envelope ?form))))
    ))


;;;; module core-macro-transformer: INTERNAL-BODY

(define-core-transformer (internal-body input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's INTERNAL-BODY  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context  of the  given LEXENV;  return  a PSI  struct containing  an instance  of
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
  ;;LEXENV; return a PSI struct.
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
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
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
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?type-id)
       (identifier? ?type-id)
       (let ((descr (id->struct-type-name-binding-descriptor __who__ input-form.stx ?type-id lexenv.run)))
	 (%make-struct-type-descriptor descr input-form.stx lexenv.run lexenv.expand)))))


  (define-core-transformer (record-type-descriptor input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function  used to  expand RECORD-TYPE-DESCRIPTOR syntaxes  from the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?type-id)
       (identifier? ?type-id)
       (let ((descr (id->record-type-name-binding-descriptor __who__ input-form.stx ?type-id lexenv.run)))
	 (%make-record-type-descriptor descr input-form.stx lexenv.run lexenv.expand)))))

  (define-core-transformer (record-constructor-descriptor input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand RECORD-CONSTRUCTOR-DESCRIPTOR syntaxes from
    ;;the top-level built in environment.  Expand the syntax object INPUT-FORM.STX in
    ;;the context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?type-id)
       (identifier? ?type-id)
       (let* ((descr     (id->record-type-name-binding-descriptor __who__ input-form.stx ?type-id lexenv.run))
	      (rts       (syntactic-binding-descriptor.value descr))
	      (expr.stx  (record-type-spec.rcd-id rts))
	      (expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand)))
	 (make-psi input-form.stx
		   (psi-core-expr expr.psi)
		   (make-retvals-signature/single-value (core-prim-id '<record-constructor-descriptor>)))))
      ))

  (define-core-transformer (type-descriptor input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand TYPE-DESCRIPTOR syntaxes from the top-level
    ;;built in environment.   Expand the syntax object INPUT-FORM.STX  in the context
    ;;of the given LEXENV; return a PSI struct.
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
       (identifier? ?type-id)
       (begin
	 (visit-library-of-imported-syntactic-binding __who__ input-form.stx ?type-id lexenv.run)
	 (let* ((label (id->label/or-error __who__ input-form.stx ?type-id))
		(descr (label->syntactic-binding-descriptor label lexenv.run)))
	   (cond ((record-type-name-binding-descriptor? descr)
		  (%make-record-type-descriptor descr input-form.stx lexenv.run lexenv.expand))
		 ((struct-type-name-binding-descriptor? descr)
		  (%make-struct-type-descriptor descr input-form.stx lexenv.run lexenv.expand))
		 (else
		  (%synner "neither a struct type nor a object type" ?type-id))))))
      ))

;;; --------------------------------------------------------------------

  (define (%make-struct-type-descriptor descr input-form.stx lexenv.run lexenv.expand)
    (let* ((sts (syntactic-binding-descriptor.value descr))
	   (std (struct-type-spec.std sts)))
      (make-psi input-form.stx
		(build-data no-source std)
		(make-retvals-signature/single-value (core-prim-id '<struct-type-descriptor>)))))

  (define (%make-record-type-descriptor descr input-form.stx lexenv.run lexenv.expand)
    (let* ((rts       (syntactic-binding-descriptor.value descr))
	   (expr.stx  (record-type-spec.rtd-id rts))
	   (expr.psi  (chi-expr expr.stx lexenv.run lexenv.expand)))
      (make-psi input-form.stx
		(psi-core-expr expr.psi)
		(make-retvals-signature/single-value (core-prim-id '<record-type-descriptor>)))))

  #| end of module |# )


;;;; module core-macro-transformer: TYPE-OF

(define-core-transformer (type-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  Vicare's TYPE-OF syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.sig (psi-retvals-signature expr.psi)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sig)
		 (make-retvals-signature/single-top))))
    ))


;;;; module core-macro-transformer: EXPANSION-OF

(define-core-transformer (expansion-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  EXPANSION-OF  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
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
	    (expr.core (psi-core-expr expr.psi))
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
		   (make-retvals-signature/single-top)))))

    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sexp (core-language->sexp expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-retvals-signature/single-top))))
    ))

(define-core-transformer (expansion-of* input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's EXPANSION-OF*  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
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
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (let* ((label               (id->label/or-error __who__ input-form.stx ?id))
	    (binding-descriptor  (label->syntactic-binding-descriptor label lexenv.run))
	    (binding-value       (case (syntactic-binding-descriptor.type binding-descriptor)
				   ((local-macro local-macro!)
				    (syntactic-binding-descriptor.value binding-descriptor))
				   (else
				    (%synner "expected identifier of local macro" ?id)))))
       (make-psi input-form.stx
		 (build-data no-source
		   (core-language->sexp (cdr binding-value)))
		 (make-retvals-signature/single-top))))
    ))


;;;; module core-macro-transformer: OPTIMISATION-OF, OPTIMISATION-OF*, FURTHER-OPTIMISATION-OF, FURTHER-OPTIMISATION-OF*

(define-core-transformer (optimisation-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  Vicare's OPTIMISATION-OF syntaxes  from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sexp (compiler.core-expr->optimized-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-retvals-signature/single-top))))
    ))

(define-core-transformer (optimisation-of* input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand Vicare's OPTIMISATION-OF*  syntaxes from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr0 ?expr* ...)
     (let* ((expr.psi  (chi-expr (bless `(internal-body ,?expr0 ,@?expr* (void)))
				 lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sexp (compiler.core-expr->optimized-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-retvals-signature/single-top))))
    ))

(define-core-transformer (further-optimisation-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's FURTHER-OPTIMISATION-OF  syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sexp (compiler.core-expr->optimisation-and-core-type-inference-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-retvals-signature/single-top))))
    ))

(define-core-transformer (further-optimisation-of* input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's  FURTHER-OPTIMISATION-OF* syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr0 ?expr* ...)
     (let* ((expr.psi  (chi-expr (bless `(internal-body ,?expr0 ,@?expr* (void)))
				 lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sexp (compiler.core-expr->optimisation-and-core-type-inference-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-retvals-signature/single-top))))
    ))


;;;; module core-macro-transformer: ASSEMBLY-OF

(define-core-transformer (assembly-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  Vicare's  ASSEMBLY-OF syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sexp (compiler.core-expr->assembly-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-retvals-signature/single-top))))
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
;;eval: (put 'core-lang-builder			'scheme-indent-function 1)
;;eval: (put 'case-object-type-binding		'scheme-indent-function 1)
;;eval: (put 'push-lexical-contour		'scheme-indent-function 1)
;;eval: (put 'syntactic-binding-getprop		'scheme-indent-function 1)
;;eval: (put 'sys.syntax-case			'scheme-indent-function 2)
;;eval: (put 'with-exception-handler/input-form	'scheme-indent-function 1)
;;eval: (put '$map-in-order			'scheme-indent-function 1)
;;End:
