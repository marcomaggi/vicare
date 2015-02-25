;;;Copyright (c) 2010-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    ((condition-is-a?)				condition-is-a?-transformer)
    ((slot-ref)					slot-ref-transformer)
    ((slot-set!)				slot-set!-transformer)

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
    ((tag-unsafe-cast)				tag-unsafe-cast-transformer)

    ((type-of)					type-of-transformer)
    ((expansion-of)				expansion-of-transformer)
    ((visit-code-of)				visit-code-of-transformer)
    ((optimisation-of)				optimisation-of-transformer)
    ((assembly-of)				assembly-of-transformer)

    (else
     (assertion-violation/internal-error __who__
       "cannot find transformer" name))))


(module PROCESSING-UTILITIES-FOR-LISTS-OF-BINDINGS
  (%expand-rhs*
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
  ;;       (parse-list-of-tagged-bindings ?lhs* input-form.stx)
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
  ;;the work  is done  by the  macro TAG-ASSERT-AND-RETURN, so  we transform  the RHS
  ;;expression as if the input form is:
  ;;
  ;;   (let (({a <fixnum>} (tag-assert-and-return (<fixnum>) 1))) . ?body)
  ;;
  ;;and expand the new RHS:
  ;;
  ;;   (tag-assert-and-return (<fixnum>) 1)
  ;;
  ;;if the expander  determines that the signature  of 1 is "(<fixnum>)",  the RHS is
  ;;transformed at expand-time into just "1";  otherwise a run-time object type check
  ;;is inserted.  In any case we can be sure at bot expand-time and run-time that the
  ;;signature of the  identifier A is correct, otherwise an  exception will be raised
  ;;before expanding or running the ?BODY.
  ;;

  (define (%expand-rhs* input-form.stx lexenv.run lexenv.expand
			lhs*.tag rhs*.stx)
    ;;Expand  a  list  of  right-hand  sides  from  bindings  in  the  syntax  object
    ;;INPUT-FORM.STX; the context  of the expansion is described by  the given LEXENV
    ;;arguments.
    ;;
    ;;LHS*.TAG  must be  a list  of tag  identifiers representing  the tags  from the
    ;;left-hand sides.
    ;;
    ;;RHS*.STX must be a list of syntax objects representing the expressions from the
    ;;right-hand sides.
    ;;
    ;;Return 2 values: a list of  PSI structures representing the expanded right-hand
    ;;sides; a list of tag identifiers  representing the signatures of the right-hand
    ;;sides.  If  the RHS  are found,  at expand-time,  to return  zero, two  or more
    ;;values: a synatx violation is raised.
    ;;
    (define rhs*.psi
      (map (lambda (rhs.stx lhs.tag)
	     ;;If LHS.TAG is "<top>", we still want to use the assert and return form
	     ;;to  make sure  that  a single  value is  returned.   If the  signature
	     ;;validation succeeds at expand-time: the  returned PSI has the original
	     ;;RHS signature,  not "(<top>)".   This allows us  to propagate  the tag
	     ;;from RHS to LHS.
	     (chi-expr (bless
			`(tag-assert-and-return (,lhs.tag) ,rhs.stx))
		       lexenv.run lexenv.expand))
	rhs*.stx lhs*.tag))
    (define rhs*.sig
      (map psi-retvals-signature rhs*.psi))
    (define rhs*.tag
      (map (lambda (sig)
	     (syntax-match (retvals-signature-tags sig) ()
	       ((?tag)
		;;Single return value: good.
		?tag)
	       (_
		;;If we  are here it  means that the TAG-ASSERT-AND-RETURN  above has
		;;misbehaved.
		(assertion-violation/internal-error __who__
		  "invalid retvals signature" (syntax->datum input-form.stx) sig))))
	rhs*.sig))
    (values rhs*.psi rhs*.tag))

  (define (%compose-lhs-specification lhs*.id lhs*.tag rhs*.inferred-tag)
    ;;For every LHS identifier build a tagged identifier syntax:
    ;;
    ;;   (brace ?lhs.id ?tag)
    ;;
    ;;in which ?TAG is either the original one specified in the LET syntax or the one
    ;;inferred by expanding the RHS.  If there  is no tag explicitly specified in the
    ;;LET syntax we put in the inferred one.
    ;;
    (if (option.tagged-language.rhs-tag-propagation?)
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

(define (if-transformer input-form.stx lexenv.run lexenv.expand)
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
		 (retvals-signature-common-ancestor (psi-retvals-signature consequent.psi)
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
		 (make-retvals-signature-single-top))))
    ))


;;;; module core-macro-transformer: QUOTE

(define (quote-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS  QUOTE syntaxes from the top-level built
  ;;in environment.   Expand the syntax object  INPUT-FORM.STX in the context  of the
  ;;given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?datum)
     (let ((datum (syntax->datum ?datum)))
       (make-psi input-form.stx
		 (build-data no-source
		   datum)
		 (retvals-signature-of-datum datum))))
    ))


;;;; module core-macro-transformer: LAMBDA and CASE-LAMBDA, INTERNAL-LAMBDA and INTERNAL-CASE-LAMBDA

(define (case-lambda-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  R6RS CASE-LAMBDA syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return an PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?formals* ?body* ?body** ...) ...)
     (chi-case-lambda input-form.stx lexenv.run lexenv.expand
		      '(safe) ?formals* (map cons ?body* ?body**)))
    ))

(define (lambda-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand R6RS LAMBDA syntaxes from the top-level built
  ;;in environment.   Expand the syntax object  INPUT-FORM.STX in the context  of the
  ;;given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?formals ?body ?body* ...)
     (chi-lambda input-form.stx lexenv.run lexenv.expand
		 '(safe) ?formals (cons ?body ?body*)))
    ))

(define (internal-case-lambda-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  INTERNAL-CASE-LAMBDA syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return an PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?attributes (?formals* ?body* ?body** ...) ...)
     (chi-case-lambda input-form.stx lexenv.run lexenv.expand
		      ?attributes ?formals* (map cons ?body* ?body**)))
    ))

(define (internal-lambda-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  Vicare's INTERNAL-LAMBDA syntaxes  from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?attributes ?formals ?body ?body* ...)
     (chi-lambda input-form.stx lexenv.run lexenv.expand
		 ?attributes ?formals (cons ?body ?body*)))
    ))


;;;; module core-macro-transformer: LET

(module (let-transformer)
  ;;Transformer function used  to expand R6RS LET macros with  Vicare extensions from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return an PSI struct.
  ;;
  ;;In practice, below we convert the UNnamed syntax:
  ;;
  ;;   (let ((?lhs ?rhs) ...) . ?body)
  ;;
  ;;into:
  ;;
  ;;   ((lambda (?lhs ...) . ?body) ?rhs ...)
  ;;
  ;;and the named syntax:
  ;;
  ;;   (let ?recur ((?lhs ?rhs) ...) . ?body)
  ;;
  ;;into:
  ;;
  ;;   (internal-body
  ;;     (define (?recur ?lhs ...) . ?body)
  ;;     (?recur ?rhs ...))
  ;;
  ;;notice that  this allows ?RECUR to  be tagged with  the return values of  the LET
  ;;form.
  ;;
  ;;When expanding  UNnamed LET syntaxes: first  we expand the ?RHS  to acquire their
  ;;retvals signature, then  we expand the LAMBDA with the  formals correctly tagged.
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
  ;;   (let (({a <exact-integer>} (tag-assert-and-return (<exact-integer>) 1))) . ?body)
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
  (define-fluid-override __who__
    (identifier-syntax 'let))

  (define* (let-transformer input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       (%expander input-form.stx lexenv.run lexenv.expand
		  ?lhs* ?rhs* (cons ?body ?body*) %build-and-expand-common-lambda))


      ((_ ?recur ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;NOTE We want an implementation in which:  when BREAK is not used, the escape
       ;;function is  never referenced, so  the compiler can remove  CALL/CC.  Notice
       ;;that here binding  CONTINUE makes no sense, because calling  ?RECUR does the
       ;;job.
       (receive (recur.id recur.tag)
	   (parse-tagged-identifier-syntax ?recur)
	 (chi-expr (bless
		    `(internal-body
		       (define (,?recur . ,?lhs*)
			 ;;FIXME  We do  not want  "__who__" and  RETURN to  be bound
			 ;;here.  (Marco Maggi; Wed Jan 21, 2015)
			 ,?body . ,?body*)
		       (,recur.id . ,?rhs*)))
		   lexenv.run lexenv.expand)))

      (_
       (syntax-violation __who__ "invalid syntax" input-form.stx))))

;;; --------------------------------------------------------------------

  (define (%build-and-expand-common-lambda input-form.stx lexenv.run lexenv.expand
					   lhs*.stx body*.stx)
    (chi-expr (bless
	       `(lambda ,lhs*.stx . ,body*.stx))
	      lexenv.run lexenv.expand))

;;; --------------------------------------------------------------------

  (define (%expander input-form.stx lexenv.run lexenv.expand
		     lhs*.stx rhs*.stx body*.stx rator-expander)
    (import PROCESSING-UTILITIES-FOR-LISTS-OF-BINDINGS)
    (receive (lhs*.id lhs*.tag)
	(parse-list-of-tagged-bindings lhs*.stx input-form.stx)
      (receive (rhs*.psi rhs*.tag)
	  (%expand-rhs* input-form.stx lexenv.run lexenv.expand lhs*.tag rhs*.stx)
	(let* ((lhs*.stx    (%compose-lhs-specification lhs*.id lhs*.tag rhs*.tag))
	       (rator.psi   (rator-expander input-form.stx lexenv.run lexenv.expand lhs*.stx body*.stx))
	       (rator.core  (psi-core-expr rator.psi))
	       (rhs*.core   (map psi-core-expr rhs*.psi)))
	  (make-psi input-form.stx
		    (build-application (syntax-annotation input-form.stx)
		      rator.core
		      rhs*.core)
		    (psi-application-retvals-signature rator.psi))))))

  #| end of module: LET-TRANSFORMER |# )


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
    (import PROCESSING-UTILITIES-FOR-LISTS-OF-BINDINGS)
    (syntax-match input-form.stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check that the binding names are identifiers and without duplicates.
       (receive (lhs*.id lhs*.tag)
	   (parse-list-of-tagged-bindings ?lhs* input-form.stx)
	 ;;Generate unique variable names and labels for the LETREC bindings.
	 (let ((lhs*.lex (map gensym-for-lexical-var lhs*.id))
	       (lhs*.lab (map gensym-for-label       lhs*.id)))
	   (map set-label-tag! lhs*.lab lhs*.tag)
	   ;;Generate what  is needed  to create  a lexical contour:  a <RIB>  and an
	   ;;extended lexical  environment in which  to evaluate both  the right-hand
	   ;;sides and the body.
	   ;;
	   ;;Notice  that the  region of  all the  LETREC bindings  includes all  the
	   ;;right-hand sides.
	   (let* ((rib        (make-filled-rib lhs*.id lhs*.lab))
		  (lexenv.run (add-lexical-bindings lhs*.lab lhs*.lex lexenv.run))
		  (rhs*.psi   ($map-in-order
			       (lambda (rhs.stx lhs.lab lhs.tag)
				 (receive-and-return (rhs.psi)
				     ;;The LHS*.ID  and LHS*.LAB  have been  added to
				     ;;the rib, and the rib is pushed on the RHS.STX.
				     ;;So, while the specific identifiers LHS*.ID are
				     ;;unbound (because they do not contain the rib),
				     ;;any occurrence  of the binding  identifiers in
				     ;;the RHS.STX is captured  by the binding in the
				     ;;rib.
				     ;;
				     ;;If LHS.TAG  is "<top>",  we still want  to use
				     ;;the assert and return form to make sure that a
				     ;;single  value is  returned.  If  the signature
				     ;;validation   succeeds   at  expand-time:   the
				     ;;returned PSI  has the original  RHS signature,
				     ;;not  "(<top>)".  This  allows us  to propagate
				     ;;the tag from RHS to LHS.
				     (chi-expr (push-lexical-contour rib
						 (bless
						  `(tag-assert-and-return (,lhs.tag) ,rhs.stx)))
					       lexenv.run lexenv.expand)
				   ;;If the LHS is  untagged: perform tag propatation
				   ;;from the RHS to the LHS.
				   (when (and (option.tagged-language.rhs-tag-propagation?)
					      (top-tag-id? lhs.tag))
				     (syntax-match (retvals-signature-tags (psi-retvals-signature rhs.psi)) ()
				       ((?tag)
					;;Single return value: good.
					(override-label-tag! lhs.lab ?tag))
				       (_
					;;If   we  are   here   it   means  that   the
					;;TAG-ASSERT-AND-RETURN above has misbehaved.
					(assertion-violation/internal-error __who__
					  "invalid retvals signature"
					  (syntax->datum input-form.stx)
					  (psi-retvals-signature rhs.psi)))))))
			       ?rhs* lhs*.lab lhs*.tag))
		  (body.psi (chi-internal-body (push-lexical-contour rib
						 (cons ?body ?body*))
					       lexenv.run lexenv.expand)))
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

  #| end of module |# )


;;;; module core-macro-transformer: FLUID-LET-SYNTAX

(define (fluid-let-syntax-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  FLUID-LET-SYNTAX syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
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
  (define (transformer input-form.stx)
    (syntax-match input-form.stx ()
      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       ;;Check that the ?LHS* are all identifiers with no duplicates.
       (unless (valid-bound-ids? ?lhs*)
	 (%error-invalid-formals-syntax input-form.stx ?lhs*))
       (let* ((fluid-label* (map %lookup-binding-in-lexenv.run ?lhs*))
	      (binding*     (map (lambda (rhs.stx)
				   (with-exception-handler/input-form
				       rhs.stx
				     (%eval-macro-transformer
				      (%expand-macro-transformer rhs.stx lexenv.expand)
				      lexenv.run)))
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

  (transformer input-form.stx))


;;;; module core-macro-transformer: FOREIGN-CALL

(define (foreign-call-transformer input-form.stx lexenv.run lexenv.expand)
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
	   (make-psi use-stx code))))
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
  ;;top-level built  in environment.  Process  the contents of INPUT-FORM.STX  in the
  ;;context of the  lexical environments LEXENV.RUN and LEXENV.EXPAND.   Return a PSI
  ;;struct.
  ;;
  ;;Notice that the parsing of the patterns is performed by CONVERT-PATTERN at expand
  ;;time and the actual pattern matching is performed by SYNTAX-DISPATCH at run time.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'syntax-case))

  (define (syntax-case-transformer input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?expr (?literal* ...) ?clauses* ...)
       (%verify-literals ?literal* input-form.stx)
       (let* ( ;;The lexical variable to which  the result of evaluating the ?EXPR is
	      ;;bound.
	      (expr.sym   (gensym-for-lexical-var 'tmp))
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
      (let* ((tmp-sym      (gensym-for-lexical-var 'tmp))
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

  (define (%invalid-ids-error id* e description.str)
    (let find ((id* id*)
	       (ok* '()))
      (if (null? id*)
	  (stx-error e) ; shouldn't happen
	(if (identifier? (car id*))
	    (if (bound-id-member? (car id*) ok*)
		(syntax-violation __who__
		  (string-append "duplicate " description.str) (car id*))
	      (find (cdr id*) (cons (car id*) ok*)))
	  (syntax-violation __who__
	    (string-append "invalid " description.str) (car id*))))))

  (define (%chi-expr.core expr.stx lexenv.run lexenv.expand)
    (psi-core-expr (chi-expr expr.stx lexenv.run lexenv.expand)))

  #| end of module: SYNTAX-CASE-TRANSFORMER |# )


;;;; module core-macro-transformer: SPLICE-FIRST-EXPAND

(define (splice-first-expand-transformer input-form.stx lexenv.run lexenv.expand)
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


;;;; module core-macro-transformer: PREDICATE-PROCEDURE-ARGUMENT-VALIDATION, PREDICATE-RETURN-VALUE-VALIDATION

(define (predicate-procedure-argument-validation-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer        function         used        to         expand        Vicare's
  ;;PREDICATE-PROCEDURE-ARGUMENT-VALIDATION  macros  from   the  top-level  built  in
  ;;environment.  Expand the  contents of INPUT-FORM.STX in the context  of the given
  ;;LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'predicate-procedure-argument-validation))
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (chi-expr (cond ((parametrise ((current-run-lexenv (lambda () lexenv.run)))
			(syntactic-binding-getprop ?id
			  *PREDICATE-PROCEDURE-ARGUMENT-VALIDATION-COOKIE*)))
		     (else
		      (stx-error input-form.stx "undefined procedure argument validation")))
	       lexenv.run lexenv.expand))
    ))

(define (predicate-return-value-validation-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's PREDICATE-RETURN-VALUE-VALIDATION
  ;;macros  from  the  top-level  built  in  environment.   Expand  the  contents  of
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'predicate-return-value-validation))
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (chi-expr (cond ((parametrise ((current-run-lexenv (lambda () lexenv.run)))
			(syntactic-binding-getprop ?id
			  *PREDICATE-RETURN-VALUE-VALIDATION-COOKIE*)))
		     (else
		      (stx-error input-form.stx "undefined return value validation")))
	       lexenv.run lexenv.expand))
    ))


;;;; module core-macro-transformer: struct type descriptor, setter and getter

(module (struct-type-descriptor-transformer
	 struct-type-and-struct?-transformer
	 struct-type-field-ref-transformer
	 struct-type-field-set!-transformer
	 $struct-type-field-ref-transformer
	 $struct-type-field-set!-transformer)

  (define (struct-type-descriptor-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function  used to  expand STRUCT-TYPE-DESCRIPTOR syntaxes  from the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (define-fluid-override __who__
      (identifier-syntax 'struct-type-descriptor))
    (syntax-match input-form.stx ()
      ((_ ?type-id)
       (identifier? ?type-id)
       (make-psi input-form.stx
		 (build-data no-source
		   (%struct-type-id->rtd __who__ input-form.stx ?type-id lexenv.run))
		 (make-retvals-signature-single-value (core-prim-id '<struct-type-descriptor>))))
      ))

  (define (struct-type-and-struct?-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to  expand STRUCT-TYPE-AND-STRUCT?  syntaxes from the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return an PSI struct.
    ;;
    (define-fluid-override __who__
      (identifier-syntax 'struct-type-and-struct?))
    (syntax-match input-form.stx ()
      ((_ ?type-id ?stru)
       (identifier? ?type-id)
       (let ((rtd (%struct-type-id->rtd __who__ input-form.stx ?type-id lexenv.run)))
	 (chi-expr (bless
		    `($struct/rtd? ,?stru (quote ,rtd)))
		   lexenv.run lexenv.expand)))
      ))

;;; --------------------------------------------------------------------

  (module (struct-type-field-ref-transformer
	   $struct-type-field-ref-transformer)

    (define (struct-type-field-ref-transformer input-form.stx lexenv.run lexenv.expand)
      ;;Transformer function  used to expand STRUCT-TYPE-FIELD-REF  syntaxes from the
      ;;top-level built in  environment.  Expand the syntax  object INPUT-FORM.STX in
      ;;the context of the given LEXENV; return a PSI struct.
      ;;
      (%struct-type-field-ref-transformer 'struct-type-field-ref #t input-form.stx lexenv.run lexenv.expand))

    (define ($struct-type-field-ref-transformer input-form.stx lexenv.run lexenv.expand)
      ;;Transformer function used to  expand $STRUCT-TYPE-FIELD-REF syntaxes from the
      ;;top-level built in  environment.  Expand the syntax  object INPUT-FORM.STX in
      ;;the context of the given LEXENV; return a PSI struct.
      ;;
      (%struct-type-field-ref-transformer '$struct-type-field-ref #f input-form.stx lexenv.run lexenv.expand))

    (define (%struct-type-field-ref-transformer who safe? input-form.stx lexenv.run lexenv.expand)
      (syntax-match input-form.stx ()
	((_ ?type-id ?field-id ?stru)
	 (and (identifier? ?type-id)
	      (identifier? ?field-id))
	 (let* ((rtd         (%struct-type-id->rtd who input-form.stx ?type-id lexenv.run))
		(field-names (struct-type-field-names rtd))
		(field-idx   (%field-name->field-idx who input-form.stx field-names ?field-id)))
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

    (define (struct-type-field-set!-transformer input-form.stx lexenv.run lexenv.expand)
      ;;Transformer function used to expand STRUCT-TYPE-FIELD-SET!  syntaxes from the
      ;;top-level built in  environment.  Expand the syntax  object INPUT-FORM.STX in
      ;;the context of the given LEXENV; return a PSI struct.
      ;;
      (%struct-type-field-set!-transformer 'struct-type-field-ref #t input-form.stx lexenv.run lexenv.expand))

    (define ($struct-type-field-set!-transformer input-form.stx lexenv.run lexenv.expand)
      ;;Transformer function  used to  expand $STRUCT-TYPE-FIELD-SET!   syntaxes from
      ;;the top-level built in environment.   Expand the syntax object INPUT-FORM.STX
      ;;in the context of the given LEXENV; return a PSI struct.
      ;;
      (%struct-type-field-set!-transformer '$struct-type-field-ref #f input-form.stx lexenv.run lexenv.expand))

    (define (%struct-type-field-set!-transformer who safe? input-form.stx lexenv.run lexenv.expand)
      (syntax-match input-form.stx ()
	((_ ?type-id ?field-id ?stru ?new-value)
	 (and (identifier? ?type-id)
	      (identifier? ?field-id))
	 (let* ((rtd         (%struct-type-id->rtd who input-form.stx ?type-id lexenv.run))
		(field-names (struct-type-field-names rtd))
		(field-idx   (%field-name->field-idx who input-form.stx field-names ?field-id)))
	   (chi-expr (bless
		      (if safe?
			  `(struct-set! ,?stru ,field-idx ,?new-value)
			`($struct-set! ,?stru ,field-idx ,?new-value)))
		     lexenv.run lexenv.expand)))
	))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define (%struct-type-id->rtd who input-form.stx type-id lexenv.run)
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
		    (syntax-violation who "not a struct type" input-form.stx type-id)))))
	  (else
	   (%raise-unbound-error who input-form.stx type-id))))

  (define (%field-name->field-idx who input-form.stx field-names field-id)
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
	  "invalid struct type field name" input-form.stx field-id))))

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

  (define (record-type-descriptor-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function  used to  expand RECORD-TYPE-DESCRIPTOR syntaxes  from the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (define-fluid-override __who__
      (identifier-syntax 'record-type-descriptor))
    (syntax-match input-form.stx ()
      ((_ ?type-name)
       (identifier? ?type-name)
       (chi-expr (r6rs-record-type-descriptor-binding-rtd
		  (id->r6rs-record-type-descriptor-binding __who__ input-form.stx ?type-name lexenv.run))
		 lexenv.run lexenv.expand))
      ))

  (define (record-constructor-descriptor-transformer input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand RECORD-CONSTRUCTOR-DESCRIPTOR syntaxes from
    ;;the top-level built in environment.  Expand the syntax object INPUT-FORM.STX in
    ;;the  context  of  the  given  LEXENV;  return  an  expanded  language  symbolic
    ;;expression.
    ;;
    (define-fluid-override __who__
      (identifier-syntax 'record-constructor-descriptor))
    (syntax-match input-form.stx ()
      ((_ ?type-name)
       (identifier? ?type-name)
       (chi-expr (r6rs-record-type-descriptor-binding-rcd
		  (id->r6rs-record-type-descriptor-binding __who__ input-form.stx ?type-name lexenv.run))
		 lexenv.run lexenv.expand))
      ))

;;; --------------------------------------------------------------------

  (let-syntax
      ((define-getter-transformer
	 (syntax-rules ()
	   ((_ ?who ?transformer ?actor-getter)
	    (define (?transformer input-form.stx lexenv.run lexenv.expand)
	      ;;Transformer function used to expand  ?who syntaxes from the top-level
	      ;;built in environment.  Expand the syntax object INPUT-FORM.STX in the
	      ;;context of  the given  LEXENV; return  an expanded  language symbolic
	      ;;expression.
	      ;;
	      (define-constant __who__ '?who)
	      (syntax-match input-form.stx ()
		((_ ?type-name ?field-name ?record)
		 (and (identifier? ?type-name)
		      (identifier? ?field-name))
		 (let* ((synner   (lambda (message)
				    (syntax-violation __who__ message input-form.stx ?type-name)))
			(binding  (id->r6rs-record-type-descriptor-binding __who__ input-form.stx ?type-name lexenv.run))
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
	    (define (?transformer input-form.stx lexenv.run lexenv.expand)
	      ;;Transformer function used to expand  ?WHO syntaxes from the top-level
	      ;;built in environment.  Expand the syntax object INPUT-FORM.STX in the
	      ;;context of  the given  LEXENV; return  an expanded  language symbolic
	      ;;expression.
	      ;;
	      (define-constant __who__ '?who)
	      (syntax-match input-form.stx ()
		((_ ?type-name ?field-name ?record ?new-value)
		 (and (identifier? ?type-name)
		      (identifier? ?field-name))
		 (let* ((synner  (lambda (message)
				   (syntax-violation __who__ message input-form.stx ?type-name)))
			(binding (id->r6rs-record-type-descriptor-binding __who__ input-form.stx ?type-name lexenv.run))
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


;;;; module core-macro-transformer: TYPE-DESCRIPTOR

(define (type-descriptor-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand TYPE-DESCRIPTOR syntaxes  from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
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
  (syntax-match input-form.stx ()
    ((_ ?type-id)
     (identifier? ?type-id)
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run binding)
       ((r6rs-record-type)
	(chi-expr (r6rs-record-type-descriptor-binding-rtd binding)
		  lexenv.run lexenv.expand))
       ((vicare-struct-type)
	(make-psi input-form.stx
		  (build-data no-source
		    (syntactic-binding-value binding))
		  (make-retvals-signature-single-value (core-prim-id '<struct-type-descriptor>))))
       ((object-type-spec)
	(make-psi input-form.stx
		  (build-data no-source
		    (identifier-object-type-spec ?type-id))
		  (make-retvals-signature-single-top)))
       ))
    ))


;;;; module core-macro-transformer: IS-A?, CONDITION-IS-A?

(define (is-a?-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand Vicare's IS-A?  syntaxes  from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
  ;;
  (fluid-let-syntax ((__who__ (identifier-syntax 'is-a?)))
    (syntax-match input-form.stx ()
      ((_ ?jolly ?tag)
       (and (tag-identifier? ?tag)
	    (jolly-id? ?jolly))
       (let ((spec (identifier-object-type-spec ?tag)))
	 (chi-expr (object-type-spec-pred-stx spec)
		   lexenv.run lexenv.expand)))

      ((_ ?expr ?tag)
       (tag-identifier? ?tag)
       (let ((spec (identifier-object-type-spec ?tag)))
	 (chi-expr (bless
		    `(,(object-type-spec-pred-stx spec) ,?expr))
		   lexenv.run lexenv.expand)))
      )))

(define (condition-is-a?-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used  to expand Vicare's CONDITION-IS-A?   syntaxes from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (fluid-let-syntax ((__who__ (identifier-syntax 'condition-is-a?)))
    (syntax-match input-form.stx ()
      ((_ ?expr ?tag)
       (tag-identifier? ?tag)
       (chi-expr (bless
		  `(condition-and-rtd? ,?expr (record-type-descriptor ,?tag)))
		 lexenv.run lexenv.expand))
      )))


;;;; module core-macro-transformer: SLOT-REF, SLOT-SET!

(define (slot-ref-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's SLOT-REF syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'slot-ref))
  (syntax-match input-form.stx ()
    ((_ ?jolly ?field-name-id ?tag)
     (and (tag-identifier? ?tag)
	  (identifier? ?field-name-id)
	  (jolly-id? ?jolly))
     (chi-expr (tag-identifier-accessor ?tag ?field-name-id input-form.stx)
	       lexenv.run lexenv.expand))

    ((_ ?expr ?field-name-id ?tag)
     (and (tag-identifier? ?tag)
	  (identifier? ?field-name-id))
     (let ((accessor-stx (tag-identifier-accessor ?tag ?field-name-id input-form.stx)))
       (chi-expr (bless
		  `(,accessor-stx ,?expr))
		 lexenv.run lexenv.expand)))

    ;;Missing type identifier.  Try to retrieve the type from the tag of the subject.
    ((_ ?expr ?field-name-id)
     (identifier? ?field-name-id)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr         expr.psi))
	    (expr.sig  (psi-retvals-signature expr.psi)))
       (syntax-match (retvals-signature-tags expr.sig) ()
	 ((?tag)
	  (let* ((accessor.stx  (tag-identifier-accessor ?tag ?field-name-id input-form.stx))
		 (accessor.psi  (chi-expr accessor.stx lexenv.run lexenv.expand))
		 (accessor.core (psi-core-expr accessor.psi)))
	    (make-psi input-form.stx
		      (build-application input-form.stx
			accessor.core
			(list expr.core))
		      (psi-application-retvals-signature accessor.psi))))
	 (_
	  (syntax-violation __who__
	    "unable to determine type tag of expression, or invalid expression signature" input-form.stx))
	 )))
    ))

(define (slot-set!-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand  Vicare's  SLOT-SET!  syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'slot-set!))
  (syntax-match input-form.stx ()
    ((_ ?jolly1 ?field-name-id ?tag ?jolly2)
     (and (tag-identifier? ?tag)
	  (identifier? ?field-name-id)
	  (jolly-id? ?jolly1)
	  (jolly-id? ?jolly2))
     (chi-expr (tag-identifier-mutator ?tag ?field-name-id input-form.stx)
	       lexenv.run lexenv.expand))

    ((_ ?expr ?field-name-id ?tag ?new-value)
     (and (tag-identifier? ?tag)
	  (identifier? ?field-name-id))
     (let ((mutator-stx (tag-identifier-mutator ?tag ?field-name-id input-form.stx)))
       (chi-expr (bless
		  `(,mutator-stx ,?expr ,?new-value))
		 lexenv.run lexenv.expand)))

    ;;Missing type identifier.  Try to retrieve the type from the tag of the subject.
    ((_ ?expr ?field-name-id ?new-value)
     (identifier? ?field-name-id)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr         expr.psi))
	    (expr.sig  (psi-retvals-signature expr.psi)))
       (syntax-match (retvals-signature-tags expr.sig) ()
	 ((?tag)
	  (let* ((mutator.stx    (tag-identifier-mutator ?tag ?field-name-id input-form.stx))
		 (mutator.psi    (chi-expr mutator.stx lexenv.run lexenv.expand))
		 (mutator.core   (psi-core-expr mutator.psi))
		 (new-value.psi  (chi-expr ?new-value lexenv.run lexenv.expand))
		 (new-value.core (psi-core-expr new-value.psi)))
	    (make-psi input-form.stx
		      (build-application input-form.stx
			mutator.core
			(list expr.core new-value.core))
		      (psi-application-retvals-signature mutator.psi))))
	 (_
	  (syntax-violation __who__
	    "unable to determine type tag of expression, or invalid expression signature" input-form.stx))
	 )))
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

(module (tag-assert-transformer)
  ;;Transformer  function  used  to  expand Vicare's  TAG-ASSERT  syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define (tag-assert-transformer input-form.stx lexenv.run lexenv.expand)
    (define-fluid-override __who__
      (identifier-syntax 'tag-assert))
    (syntax-match input-form.stx ()
      ((_ ?retvals-signature ?expr)
       (retvals-signature-syntax? ?retvals-signature)
       (let* ((asserted.sig (make-retvals-signature ?retvals-signature))
	      (expr.psi     (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig     (psi-retvals-signature expr.psi)))
	 (cond ((list-tag-id? ?retvals-signature)
		;;If we are here the input form is:
		;;
		;;   (tag-assert <list> ?expr)
		;;
		;;and  any tuple  of returned  values returned  by ?EXPR  is of  type
		;;"<list>".
		(%just-evaluate-the-expression expr.psi))

	       ((retvals-signature-single-top-tag? asserted.sig)
		;;If we are here the input form is:
		;;
		;;   (tag-assert (<top>) ?expr)
		;;
		;;so it is  enough to make sure that the  expression returns a single
		;;value, whatever its type.
		(syntax-match (retvals-signature-tags expr.sig) ()
		  ((?tag)
		   ;;Success!!!   We   have  determined   at  expand-time   that  the
		   ;;expression returns a single value.
		   (%just-evaluate-the-expression expr.psi))
		  (?tag
		   (list-tag-id? ?tag)
		   ;;Damn   it!!!   The   expression's  return   values  have   fully
		   ;;unspecified signature; we need to insert a run-time check.
		   (%run-time-validation input-form.stx lexenv.run lexenv.expand
					 asserted.sig expr.psi))
		  (_
		   ;;The  horror!!!   We have  established  at  expand-time that  the
		   ;;expression returns multiple values; assertion failed.
		   (expand-time-retvals-signature-violation __who__ input-form.stx ?expr asserted.sig expr.sig))
		  ))

	       ((retvals-signature-partially-unspecified? expr.sig)
		;;Damn it!!!  The expression has  no type specification or  a partial
		;;type specification; we have to insert a run-time check.
		;;
		;;FIXME We can  do better here by inserting the  run-time checks only
		;;for  the "<top>"  return values,  rather than  for all  the values.
		;;(Marco Maggi; Fri Apr 4, 2014)
		(%run-time-validation input-form.stx lexenv.run lexenv.expand
				      asserted.sig expr.psi))

	       ((retvals-signature-super-and-sub? asserted.sig expr.sig)
		;;Success!!!  We  have established  at expand-time that  the returned
		;;values are valid; assertion succeeded.
		(%just-evaluate-the-expression expr.psi))

	       (else
		;;The horror!!!  We  have established at expand-time  that the returned
		;;values are of the wrong type; assertion failed.
		(expand-time-retvals-signature-violation __who__ input-form.stx ?expr asserted.sig expr.sig)))))

       ((_ ?retvals-signature ?expr)
	;;Let's use a descriptive error message here.
	(syntax-violation __who__
	  "invalid return values signature" input-form.stx ?retvals-signature))
       ))

  (define* (%run-time-validation input-form.stx lexenv.run lexenv.expand
				 {asserted.sig retvals-signature?} {expr.psi psi?})
    (define expr.core (psi-core-expr         expr.psi))
    (define expr.sig  (psi-retvals-signature expr.psi))
    ;;Here we know that ASSERTED.SIG is a  valid retvals signature, so we can be less
    ;;strict in the patterns.
    (syntax-match (retvals-signature-tags asserted.sig) ()
      ((?rv-tag* ...)
       (let* ((TMP*         (generate-temporaries ?rv-tag*))
	      (checker.psi  (chi-expr (bless
				       `(lambda ,TMP*
					  ,@(map (lambda (tmp tag)
						   `(tag-return-value-validator ,tag ,tmp))
					      TMP* ?rv-tag*)
					  (void)))
				      lexenv.run lexenv.expand)))
	 (%run-time-check-output-form input-form.stx lexenv.run lexenv.expand
				      expr.core checker.psi)))

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
	 (%run-time-check-output-form  input-form.stx lexenv.run lexenv.expand
				       expr.core checker.psi)))

      (?rv-args-tag
       (let ((checker.psi  (chi-expr (bless
				      `(lambda args
					 (tag-return-value-validator ,?rv-args-tag args)
					 (void)))
				     lexenv.run lexenv.expand)))
	 (%run-time-check-output-form  input-form.stx lexenv.run lexenv.expand
				       expr.core checker.psi)))
      ))

  (define (%just-evaluate-the-expression expr.psi)
    (make-psi (psi-stx expr.psi)
	      (build-sequence no-source
		(list (psi-core-expr expr.psi)
		      (build-void)))
	      ;;We know that we are returning a single void argument.
	      (make-retvals-signature-single-top)))

  (define (%run-time-check-output-form input-form.stx lexenv.run lexenv.expand
				       expr.core checker.psi)
    ;;We build a core language expression as follows:
    ;;
    ;;   (call-with-values
    ;;        (lambda () ?expr)
    ;;     (lambda ?formals
    ;;       ?check-form ...
    ;;       (void)))
    ;;
    (let* ((cwv.core     (build-primref no-source 'call-with-values))
	   (checker.core (psi-core-expr checker.psi)))
      (make-psi input-form.stx
		(build-application no-source
		  cwv.core
		  (list (build-lambda no-source '() expr.core)
			checker.core))
		;;We know that we are returning a single void argument.
		(make-retvals-signature-single-top))))

  #| end of module: TAG-ASSERT-TRANSFORMER |# )


;;;; module core-macro-transformer: TAG-ASSERT-AND-RETURN

(module (tag-assert-and-return-transformer)
  ;;Transformer function used to  expand Vicare's TAG-ASSERT-AND-RETURN syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI struct.
  ;;
  (define (tag-assert-and-return-transformer input-form.stx lexenv.run lexenv.expand)
    (define-fluid-override __who__
      (identifier-syntax 'tag-assert-and-return))
    (syntax-match input-form.stx ()
      ((_ ?retvals-signature ?expr)
       (retvals-signature-syntax? ?retvals-signature)
       (let* ((asserted.sig (make-retvals-signature ?retvals-signature))
	      (expr.psi     (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig     (psi-retvals-signature expr.psi)))
	 (cond ((list-tag-id? ?retvals-signature)
		;;If we are here the input form is:
		;;
		;;   (tag-assert-and-return <list> ?expr)
		;;
		;;and  any tuple  of returned  values returned  by ?EXPR  is of  type
		;;"<list>".  Just evaluate the expression.
		;;
		;;NOTE  The signature  validation has  succeeded at  expand-time: the
		;;returned PSI has the original  ?EXPR signature, not "<list>".  This
		;;just looks nicer.
		expr.psi)

	       ((retvals-signature-single-top-tag? asserted.sig)
		;;If we are here the input form is:
		;;
		;;   (tag-assert-and-return (<top>) ?expr)
		;;
		;;so it is  enough to make sure that the  expression returns a single
		;;value, whatever its type.
		(syntax-match (retvals-signature-tags expr.sig) ()
		  ((?tag)
		   ;;Success!!!   We   have  determined   at  expand-time   that  the
		   ;;expression returns a single  value.
		   ;;
		   ;;IMPORTANT  NOTE  The  signature   validation  has  succeeded  at
		   ;;expand-time:  the returned  PSI *must*  have the  original ?EXPR
		   ;;signature,  not ASSERTED.SIG;  this  even  when ASSERTED.SIG  is
		   ;;"(<top>)".   This  property is  used  in  binding syntaxes  when
		   ;;propagating a tag from the RHS to the LHS.
		   expr.psi)
		  (?tag
		   (list-tag-id? ?tag)
		   ;;Damn   it!!!   The   expression's  return   values  have   fully
		   ;;unspecified signature; we need to insert a run-time check.
		   (%run-time-validation input-form.stx lexenv.run lexenv.expand
					 asserted.sig expr.psi))
		  (_
		   ;;The  horror!!!   We have  established  at  expand-time that  the
		   ;;expression returns multiple values; assertion failed.
		   (expand-time-retvals-signature-violation __who__ input-form.stx ?expr asserted.sig expr.sig))
		  ))

	       ((retvals-signature-partially-unspecified? expr.sig)
		;;The  expression has  no type  specification;  we have  to insert  a
		;;run-time check.
		;;
		;;FIXME We can  do better here by inserting the  run-time checks only
		;;for  the "<top>"  return values,  rather than  for all  the values.
		;;(Marco Maggi; Fri Apr 4, 2014)
		(%run-time-validation input-form.stx lexenv.run lexenv.expand
				      asserted.sig expr.psi))

	       ((retvals-signature-super-and-sub? asserted.sig expr.sig)
		;;Fine, we have  established at expand-time that  the returned values
		;;are valid; assertion succeeded.  Just evaluate the expression.
		expr.psi)

	       (else
		;;The horror!!!  We have established at expand-time that the returned
		;;values are of the wrong type; assertion failed.
		(expand-time-retvals-signature-violation __who__ input-form.stx ?expr asserted.sig expr.sig)))))

      ((_ ?retvals-signature ?expr)
       ;;Let's use a descriptive error message here.
       (syntax-violation __who__
	 "invalid return values signature" input-form.stx ?retvals-signature))
      ))

  (define* (%run-time-validation input-form.stx lexenv.run lexenv.expand
				 {asserted.sig retvals-signature?} {expr.psi psi?})
    (define expr.core (psi-core-expr         expr.psi))
    (define expr.sig  (psi-retvals-signature expr.psi))
    ;;Here we know that ASSERTED.SIG is a  valid formals signature, so we can be less
    ;;strict in the patterns.
    (syntax-match (retvals-signature-tags asserted.sig) ()
      ;;Special handling for single value.
      ((?rv-tag)
       (let* ((checker.psi (chi-expr (bless
				      `(lambda (t)
					 (tag-return-value-validator ,?rv-tag t)
					 t))
				     lexenv.run lexenv.expand))
	      (checker.core (psi-core-expr checker.psi))
	      (expr.core    (psi-core-expr expr.psi)))
	 (make-psi input-form.stx
		   (build-application no-source
		     checker.core
		     (list expr.core))
		   ;;The type  of the  value returned by  ?EXPR was  unspecified, but
		   ;;after asserting the  type at run-time: we know that  the type is
		   ;;the asserted one.
		   asserted.sig)))

      ((?rv-tag* ...)
       (let* ((TMP*         (generate-temporaries ?rv-tag*))
	      (checker.psi  (chi-expr (bless
				       `(lambda ,TMP*
					  ,@(map (lambda (tmp tag)
						   `(tag-return-value-validator ,tag ,tmp))
					      TMP* ?rv-tag*)
					  (values . ,TMP*)))
				      lexenv.run lexenv.expand)))
	 (%run-time-check-multiple-values-output-form input-form.stx lexenv.run lexenv.expand
						      expr.psi checker.psi asserted.sig)))

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
	 (%run-time-check-multiple-values-output-form input-form.stx lexenv.run lexenv.expand
						      expr.psi checker.psi asserted.sig)))

      (?rv-args-tag
       (let ((checker.psi  (chi-expr (bless
				      `(lambda args
					 (tag-return-value-validator ,?rv-args-tag args)
					 (apply values args)))
				     lexenv.run lexenv.expand)))
	 (%run-time-check-multiple-values-output-form input-form.stx lexenv.run lexenv.expand
						      expr.psi checker.psi asserted.sig)))
      ))

  (define* (%run-time-check-multiple-values-output-form input-form.stx lexenv.run lexenv.expand
							{expr.psi psi?} {checker.psi psi?} {asserted.sig retvals-signature?})
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
    (let* ((cwv.core     (build-primref no-source 'call-with-values))
	   (expr.core    (psi-core-expr expr.psi))
	   (checker.core (psi-core-expr checker.psi)))
      (make-psi input-form.stx
		(build-application no-source
		  cwv.core
		  (list (build-lambda no-source
			  '()
			  expr.core)
			checker.core))
		;;The type  of values  returned by ?EXPR  was unspecified,  but after
		;;asserting  the type  at  run-time: we  know that  the  type is  the
		;;asserted one.
		asserted.sig)))

  #| end of module: TAG-ASSERT-AND-RETURN-TRANSFORMER |# )


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
	    (expr.sign (psi-retvals-signature expr.psi)))
       (if (retvals-signature-fully-unspecified? expr.sign)
	   (syntax-violation __who__ "unable to determine tag of expression" input-form.stx)
	 (syntax-match (retvals-signature-tags expr.sign) ()
	   ((?tag)
	    (let ((accessor.stx (tag-identifier-accessor ?tag ?field-name-id input-form.stx)))
	      (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						 accessor.stx expr.psi '())))
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
	    (expr.sign (psi-retvals-signature expr.psi)))
       (if (retvals-signature-fully-unspecified? expr.sign)
	   (syntax-violation __who__ "unable to determine tag of expression" input-form.stx)
	 (syntax-match (retvals-signature-tags expr.sign) ()
	   ((?tag)
	    (let ((mutator.stx (tag-identifier-mutator ?tag ?field-name-id input-form.stx)))
	      (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						 mutator.stx expr.psi (list ?new-value))))
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
	   (let ((getter.stx (tag-identifier-getter ?tag keys.stx input-form.stx)))
	     (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						getter.stx expr.psi '())))
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
	   (let ((setter.stx  (tag-identifier-setter ?tag keys.stx input-form.stx)))
	     (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						setter.stx expr.psi (list new-value.stx))))
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
	   (let ((method.stx (tag-identifier-dispatch ?tag ?member input-form.stx)))
	     (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						method.stx expr.psi ?arg*)))
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
	   => object-type-spec-caster-maker)
	  (else
	   (syntax-violation/internal-error __who__ "tag identifier without object type spec" input-form.stx))))

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
		  (make-psi (psi-stx expr.psi)
			    (build-application no-source
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
      (make-psi (psi-stx expr.psi)
		(build-application no-source
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
	    (cond ((top-tag-id? ?target-tag)
		   ;;The expression  already has the  right type: nothing to  do, just
		   ;;return it.
		   expr.psi)
		  ((top-tag-id? ?source-tag)
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
				 (make-psi input-form.stx
					   (build-application (syntax-annotation input-form.stx)
					     caster.core
					     (list expr.core))
					   (make-retvals-signature (list ?target-tag))))))
			 (else
			  (%validate-and-return ?target-tag expr.psi))))))

	   (_
	    (syntax-violation __who__ "invalid expression retvals signature" input-form.stx expr.sign))
	   ))))
    ))


;;;; module core-macro-transformer: TAG-UNSAFE-CAST

(define (tag-unsafe-cast-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  Vicare's TAG-UNSAFE-CAST syntaxes  from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'tag-unsafe-cast))
  (syntax-match input-form.stx ()
    ((_ ?target-tag ?expr)
     (tag-identifier? ?target-tag)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sign (psi-retvals-signature expr.psi)))
       (if (retvals-signature-fully-unspecified? expr.sign)
	   ;;The  expression has  non-specified values  type:  cast the  type to  the
	   ;;target one.  Hey!  It is UNSAFE cast!
	   (make-psi input-form.stx
		     expr.core
		     (make-retvals-signature-single-value ?target-tag))
	 (syntax-match (retvals-signature-tags expr.sign) ()
	   ((?source-tag)
	    (cond ((top-tag-id? ?target-tag)
		   ;;The expression already  has the right type: nothing  to do, just
		   ;;return it.
		   expr.psi)
		  ((top-tag-id? ?source-tag)
		   ;;The  expression has  non-specified single-value  type: cast  the
		   ;;type to the target one.
		   (make-psi input-form.stx
			     expr.core
			     (make-retvals-signature-single-value ?target-tag)))
		  ((tag-super-and-sub? ?target-tag ?source-tag)
		   ;;The expression already  has the right type: nothing  to do, just
		   ;;return it.
		   expr.psi)
		  (else
		   (syntax-violation __who__
		     "the tag of expression is incompatible with the requested tag"
		     input-form.stx expr.sign))))
	   (_
	    (syntax-violation __who__ "invalid expression retvals signature" input-form.stx expr.sign))
	   ))))
    ))


;;;; module core-macro-transformer: TYPE-OF

(define (type-of-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  Vicare's TYPE-OF syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'type-of))

  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.sig (psi-retvals-signature expr.psi)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sig)
		 (make-retvals-signature-single-top))))
    ))


;;;; module core-macro-transformer: EXPANSION-OF

(define (expansion-of-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to  expand Vicare's  EXPANSION-OF  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'expansion-of))
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
	  (or (free-id=? ?define (core-prim-id 'define))
	      (free-id=? ?define (core-prim-id 'define*))))
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
		   (make-retvals-signature-single-top)))))

    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sexp (core-language->sexp expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-retvals-signature-single-top))))
    ))


;;;; module core-macro-transformer: VISIT-CODE-OF

(define (visit-code-of-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's VISIT-CODE-OF  syntaxes from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'visit-code-of))
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (let* ((label               (id->label/or-error __who__ input-form.stx ?id))
	    (binding-descriptor  (label->syntactic-binding label lexenv.run))
	    (binding-value       (case (syntactic-binding-type binding-descriptor)
				   ((local-macro local-macro!)
				    (syntactic-binding-value binding-descriptor))
				   (else
				    (syntax-violation __who__
				      "expected identifier of local macro" input-form.stx ?id)))))
       (make-psi input-form.stx
		 (build-data no-source
		   (core-language->sexp (cdr binding-value)))
		 (make-retvals-signature-single-top))))
    ))


;;;; module core-macro-transformer: OPTIMISATION-OF

(define (optimisation-of-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand  Vicare's OPTIMISATION-OF syntaxes  from the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'optimisation-of))
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sexp (compiler.core-expr->optimized-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-retvals-signature-single-top))))
    ))


(define (assembly-of-transformer input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  Vicare's  ASSEMBLY-OF syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'assembly-of))
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core (psi-core-expr expr.psi))
	    (expr.sexp (compiler.core-expr->assembly-code expr.core)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sexp)
		 (make-retvals-signature-single-top))))
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
;;eval: (put 'if-wants-descriptive-gensyms	'scheme-indent-function 1)
;;eval: (put 'push-lexical-contour		'scheme-indent-function 1)
;;eval: (put 'set-interaction-env-lab.loc/lex*!	'scheme-indent-function 1)
;;eval: (put 'syntactic-binding-getprop		'scheme-indent-function 1)
;;eval: (put 'sys.syntax-case			'scheme-indent-function 2)
;;eval: (put 'with-exception-handler/input-form	'scheme-indent-function 1)
;;End:
