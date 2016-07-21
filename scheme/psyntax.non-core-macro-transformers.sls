;;; -*- coding: utf-8-unix -*-
;;;
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


;;;; copyright notice for the original code of the XOR macro
;;;
;;;Copyright (c) 2008 Derick Eddington
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
;;;Except as  contained in this  notice, the name(s)  of the above  copyright holders
;;;shall not be  used in advertising or  otherwise to promote the sale,  use or other
;;;dealings in this Software without prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(library (psyntax.non-core-macro-transformers)
  (export)
  (import (except (rnrs)
		  eval
		  environment		environment?
		  null-environment	scheme-report-environment
		  identifier?
		  bound-identifier=?	free-identifier=?
		  generate-temporaries
		  datum->syntax		syntax->datum
		  syntax-violation	make-variable-transformer)
    (psyntax.compat)
    (prefix (rnrs syntax-case) sys::)
    (prefix (only (psyntax.config)
		  typed-language-enabled?
		  strict-r6rs-enabled?)
	    options::)
    (psyntax.lexical-environment)
    (only (psyntax.library-manager)
	  current-include-loader
	  source-code-location)
    (only (psyntax.syntax-utilities)
	  duplicate-identifiers?))

  (include "psyntax.helpers.scm" #t)

;; module interfaces
(import PSYNTAX-SYNTAX-MATCH
  PSYNTAX-SYNTAX-UTILITIES
  PSYNTAX-TYPE-SIGNATURES
  PSYNTAX-TYPE-SYNTAX-OBJECTS
  PSYNTAX-LAMBDA-SIGNATURES)


;;;; helpers

(define-fluid-syntax __synner__
  (identifier-syntax (lambda (stx)
		       (syntax-violation '__synner__ "unset fluid syntax" stx))))
;; (define-fluid-syntax __synner__
;;   (lambda (stx)
;;     (syntax-violation '__synner__ "unset fluid syntax" stx)))

(define-syntax define-macro-transformer
  ;;We expect the table to be a proper list  and the entries in the table to have the
  ;;format:
  ;;
  ;;   (let-values (macro  . #{let-values . ?key}))
  ;;   (__file__   (macro! . #{__file__   . ?key}))
  ;;
  ;;where ?KEY is a gensym.
  ;;
  (let ((non-core-macro-table (with-input-from-file "non-core-macros-table.scm" read)))
    (lambda (stx)
      (sys::syntax-case stx ()
	((_ (?who ?input-form.stx) ?body0 ?body ...)
	 (and (sys::identifier? (sys::syntax ?who))
	      (sys::identifier? (sys::syntax ?input-form.stx)))
	 (let* ((who.sym	(sys::syntax->datum (sys::syntax ?who)))
		(key		(cdadr (assq who.sym non-core-macro-table)))
		(pretty-key	(symbol->string key))
		(funcname.sym	(string->symbol (string-append pretty-key "-macro"))))
	   (sys::with-syntax
	       ((FUNCNAME	(sys::datum->syntax (sys::syntax ?who) funcname.sym))
		(KEY		(sys::datum->syntax (sys::syntax ?who) key)))
	     (sys::syntax
	      (module (FUNCNAME)
		(define (FUNCNAME ?input-form.stx)
		  (with-who ?who
		    (define-synner synner (quote ?who) ?input-form.stx)
		    (fluid-let-syntax
			((__synner__ (identifier-syntax synner)))
		      ?body0 ?body ...)))
		($set-symbol-value! (quote KEY) FUNCNAME)
		#| end of module |# )))))
	))))

(define-syntax define-auxiliary-syntax-transformer
  ;;We expect the table to be a proper list  and the entries in the table to have the
  ;;format:
  ;;
  ;;   (let-values (macro  . #{let-values . ?key}))
  ;;   (__file__   (macro! . #{__file__   . ?key}))
  ;;
  ;;where ?KEY is a gensym.
  ;;
  (let ((non-core-macro-table (with-input-from-file "non-core-macros-table.scm" read)))
    (lambda (stx)
      (sys::syntax-case stx ()
	((_ (?who ?input-form.stx) ?body0 ?body ...)
	 (and (sys::identifier? (sys::syntax ?who))
	      (sys::identifier? (sys::syntax ?input-form.stx)))
	 (begin
	   ;; (let ((who.sym (sys::syntax->datum (sys::syntax ?who))))
	   ;;   (debug-print who.sym (assq who.sym non-core-macro-table))
	   ;;   (debug-print (cdadr (assq who.sym non-core-macro-table))))
	   (let* ((who.sym	(sys::syntax->datum (sys::syntax ?who)))
		  (key		(cdadr (assq who.sym non-core-macro-table)))
		  (pretty-key	(symbol->string key))
		  (funcname.sym	(string->symbol (string-append pretty-key "-macro"))))
	     (sys::with-syntax
		 ((FUNCNAME	(sys::datum->syntax (sys::syntax ?who) funcname.sym))
		  (KEY		(sys::datum->syntax (sys::syntax ?who) key)))
	       (sys::syntax
		(module (FUNCNAME)
		  (define (FUNCNAME ?input-form.stx) ?body0 ?body ...)
		  (set-symbol-value! (quote KEY) FUNCNAME)
		  #| end of module |# ))))))
	))))


;;;; auxiliary syntaxes

(let-syntax ((declare (syntax-rules ()
			((_ ?who)
			 (define-auxiliary-syntax-transformer (?who input-form.stx)
			   (syntax-violation (quote ?who)
			     "incorrect use of auxiliary syntactic keyword" input-form.stx))))))
  (declare ...)
  (declare =>)
  (declare _)
  (declare else)
  (declare unquote)
  (declare unquote-splicing)
  (declare unsyntax)
  (declare unsyntax-splicing)
  (declare fields)
  (declare mutable)
  (declare immutable)
  (declare parent)
  (declare protocol)
  (declare sealed)
  (declare opaque)
  (declare nongenerative)
  (declare parent-rtd)
  (declare constructor)
  (declare constructor-signature)
  (declare destructor)
  (declare super-protocol)
  (declare destructor-protocol)
  (declare custom-printer)
  (declare method)
  (declare virtual-method)
  (declare define-type-descriptors)
  (declare strip-angular-parentheses)
  (declare type-predicate)
  (declare mixins)
  (declare implements)
  (declare catch)
  (declare finally)
  (declare pair)
  (declare pair-of)
  (declare list-of)
  (declare nelist-of)
  (declare vector-of)
  (declare nevector-of)
  (declare hashtable)
  (declare alist)
  (declare parent-of)
  (declare ancestor-of)
  (declare enumeration)
  #| end of LET-SYNTAX |# )


;;;; external modules

(include "psyntax.non-core-macro-transformers.define-struct.scm"	#t)
(include "psyntax.non-core-macro-transformers.define-record-type.scm"	#t)
(include "psyntax.non-core-macro-transformers.infix-macro.scm"		#t)


;;;; non-core macro: DEFINE-TYPE, TYPE-ANNOTATION

(define-macro-transformer (define-type input-form.stx)
  ;;Transformer  function  used  to  expand  Vicare's  DEFINE-TYPE  macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-name ?type-annotation)
     (begin
       (unless (identifier? ?type-name)
	 (syntax-violation __who__
	   "expected identifier as type annotation name"
	   input-form.stx ?type-name))
       (let ((ots (type-annotation->object-type-spec ?type-annotation (current-inferior-lexenv) ?type-name)))
	 (bless
	  `(define-syntax ,?type-name (quote ,ots))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (type-annotation input-form.stx)
  ;;Transformer  function used  to expand  Vicare's TYPE-ANNOTATION  macros from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-annotation)
     (list (core-prim-id 'quote)
	   (type-annotation->object-type-spec ?type-annotation (current-inferior-lexenv))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: DEFINE-AUXILIARY-SYNTAXES

(define-macro-transformer (define-auxiliary-syntaxes input-form.stx)
  ;;Transformer  function used  to expand  Vicare's DEFINE-AUXILIARY-SYNTAXES  macros
  ;;from the top-level built in  environment.  Expand the contents of INPUT-FORM.STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;Using an empty  SYNTAX-RULES as transformer function makes sure  that whenever an
  ;;auxiliary syntax is referenced an error is raised.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id* ...)
     (for-all identifier? ?id*)
     (bless
      (let ((STX (make-syntactic-identifier-for-temporary-variable "stx")))
	`(begin
	   ,@(map (lambda (id)
		    `(define-syntax ,id
		       (lambda/std (,STX)
			 (syntax-violation (quote ,id)
			   "incorrect use of auxiliary syntactic keyword"
			   ,STX (quote ,id)))))
	       ?id*)))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: control structures macros

(define-macro-transformer (when input-form.stx)
  (syntax-match input-form.stx ()
    ((_ ?test ?expr ?expr* ...)
     (bless `(if ,?test (begin ,?expr . ,?expr*))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (unless input-form.stx)
  (syntax-match input-form.stx ()
    ((_ ?test ?expr ?expr* ...)
     (bless `(if (not ,?test) (begin ,?expr . ,?expr*))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: CASE

(module (case-macro)
  ;;Transformer function used to expand R6RS's CASE macros (with extensions) from the
  ;;top-level built in environment.  Expand the contents of EXPR-STX; return a syntax
  ;;object that must be further expanded.
  ;;
  ;;This implementation  supports 2 extensions with  respect to the one  specified by
  ;;R6RS:
  ;;
  ;;1. It supports arrow clauses like COND does.  Example:
  ;;
  ;;      (case 123
  ;;       ((123) => (lambda (num) ...)))
  ;;
  ;;2. When the  datums are strings, bytevectors, pairs or  vectors: it compares them
  ;;   using  STRING=?, BYTEVECTOR=?,  EQUAL? and  EQUAL? rather  than using  EQV? as
  ;;   specified by R6RS.
  ;;
  ;;An example expansion:
  ;;
  ;;   (case ?expr
  ;;     ((1 "a" c)
  ;;      (stuff1))
  ;;     ((2 #t #(1 2))
  ;;      (stuff2))
  ;;     (else
  ;;      (else-stuff)))
  ;;
  ;;is expanded to:
  ;;
  ;;   (letrec ((expr-result.tmp	?expr)
  ;;            (g1			(lambda () (stuff1)))
  ;;            (g2			(lambda () (stuff2)))
  ;;            (else-proc.tmp		(lambda () (else-stuff))))
  ;;     (cond ((number? expr-result.tmp)
  ;;            (cond ((= expr-result.tmp 1)
  ;;                   (g1))
  ;;                  ((= expr-result.tmp 2)
  ;;                   (g2))
  ;;                  (else
  ;;                   (else-proc.tmp)))
  ;;           ((string? expr-result.tmp)
  ;;            (cond ((string=? expr-result.tmp "a")
  ;;                   (g1))
  ;;                  (else
  ;;                   (else-proc.tmp)))
  ;;           ((symbol? expr-result.tmp)
  ;;            (cond ((eq? expr-result.tmp 'c)
  ;;                   (g1))
  ;;                  (else
  ;;                   (else-proc.tmp)))
  ;;           ((boolean? expr-result.tmp)
  ;;            (cond ((boolean=? expr-result.tmp #t)
  ;;                   (g2))
  ;;                  (else
  ;;                   (else-proc.tmp)))
  ;;           ((vector? expr-result.tmp)
  ;;            (cond ((equal? expr-result.tmp '#(1 2))
  ;;                   (g2))
  ;;                  (else
  ;;                   (else-proc.tmp)))
  ;;           (else
  ;;            (else-proc.tmp)))
  ;;
  ;;NOTE This implementation contains ideas from:
  ;;
  ;;    William  D.   Clinger.   "Rapid   case  dispatch  in  Scheme".   Northeastern
  ;;    University.   Proceedings  of  the 2006  Scheme  and  Functional  Programming
  ;;   Workshop.  University of Chicago Technical Report TR-2006-06.
  ;;
  ;;FIXME There is room for improvement.  (Marco Maggi; Thu Apr 17, 2014)
  ;;
  (define-module-who case)

  (define-macro-transformer (case input-form.stx)
    (syntax-match input-form.stx (else =>)
      ;;Without ELSE clause.
      ((_ ?expr ((?datum0* ?datum** ...) ?body0* ?body** ...) ...)
       (let* ((expr-result.id	(make-syntactic-identifier-for-temporary-variable "expr-result"))
	      (else-proc.id	(make-syntactic-identifier-for-temporary-variable "else-proc"))
	      (else-binding.stx	`(,else-proc.id (lambda/checked () (void)))))
	 (%build-output-form input-form.stx expr-result.id ?expr
			     (map cons
			       (map cons ?datum0* ?datum**)
			       (map cons ?body0*  ?body**))
			     else-proc.id else-binding.stx)))

      ;;With else clause, arrow case.
      ((_ ?expr ((?datum0* ?datum** ...) ?body0* ?body** ...) ... (else => ?else-proc))
       (not (options::strict-r6rs-enabled?))
       (let* ((expr-result.id	(make-syntactic-identifier-for-temporary-variable "expr-result"))
	      (else-proc.id	(make-syntactic-identifier-for-temporary-variable "else-proc"))
	      (else-binding.stx	`(,else-proc.id (lambda/checked ()
						  ((assert-signature-and-return (<procedure>) ,?else-proc) ,expr-result.id)))))
	 (%build-output-form input-form.stx expr-result.id ?expr
			     (map cons
			       (map cons ?datum0* ?datum**)
			       (map cons ?body0*  ?body**))
			     else-proc.id else-binding.stx)))

      ;;With else clause.
      ((_ ?expr ((?datum0* ?datum** ...) ?body0* ?body** ...) ... (else ?else-body0 ?else-body* ...))
       (let* ((expr-result.id	(make-syntactic-identifier-for-temporary-variable "expr-result"))
	      (else-proc.id	(make-syntactic-identifier-for-temporary-variable "else-proc"))
	      (else-binding.stx	`(,else-proc.id (lambda/checked () ,?else-body0 . ,?else-body*))))
	 (%build-output-form input-form.stx expr-result.id ?expr
			     (map cons
			       (map cons ?datum0* ?datum**)
			       (map cons ?body0*  ?body**))
			     else-proc.id else-binding.stx)))

      (_
       (syntax-violation __module_who__ "invalid syntax in macro use" input-form.stx))))

  (define (%build-output-form input-form.stx expr-result.id expr.stx datum-clause*.stx else-proc.id else-binding.stx)
    (receive (branch-binding*.stx cond-clause*.stx)
	(%process-clauses input-form.stx expr-result.id else-proc.id datum-clause*.stx)
      (bless
       `(letrec/checked ((,expr-result.id ,expr.stx)
			 ,@branch-binding*.stx
			 ,else-binding.stx)
	  (cond ,@cond-clause*.stx (else (,else-proc.id)))))))

  (define (%process-clauses input-form.stx expr-result.id else-proc.id clause*.stx)
    (receive (closure*.stx closure*.sym entry**)
	(%clauses->entries input-form.stx clause*.stx)
      (define boolean-entry*	'())
      (define char-entry*	'())
      (define null-entry*	'())
      (define symbol-entry*	'())
      (define number-entry*	'())
      (define string-entry*	'())
      (define bytevector-entry*	'())
      (define pair-entry*	'())
      (define vector-entry*	'())
      (define-syntax-rule (mk-datum-clause ?pred.sym ?compar.sym ?entry*)
	(%make-datum-clause input-form.stx expr-result.id else-proc.id (core-prim-id '?pred.sym) (core-prim-id '?compar.sym) ?entry*))
      (let loop ((entry* (apply append entry**)))
	(when (pair? entry*)
	  (let ((datum (syntax->datum (caar entry*))))
	    (cond ((boolean? datum)
		   (set-cons! boolean-entry* (car entry*))
		   (loop (cdr entry*)))

		  ((char? datum)
		   (set-cons! char-entry* (car entry*))
		   (loop (cdr entry*)))

		  ((symbol? datum)
		   (set-cons! symbol-entry* (car entry*))
		   (loop (cdr entry*)))

		  ((number? datum)
		   (set-cons! number-entry* (car entry*))
		   (loop (cdr entry*)))

		  ((string? datum)
		   (set-cons! string-entry* (car entry*))
		   (loop (cdr entry*)))

		  ((bytevector? datum)
		   (set-cons! bytevector-entry* (car entry*))
		   (loop (cdr entry*)))

		  ((pair? datum)
		   (set-cons! pair-entry* (car entry*))
		   (loop (cdr entry*)))

		  ((null? datum)
		   (set-cons! null-entry* (car entry*))
		   (loop (cdr entry*)))

		  ((vector? datum)
		   (set-cons! vector-entry* (car entry*))
		   (loop (cdr entry*)))

		  (else
		   (syntax-violation __module_who__ "invalid datum type" input-form.stx datum))))))
      (values (map list closure*.sym closure*.stx)
	      ($fold-left/stx (lambda (knil clause)
				(if (null? clause)
				    knil
				  (cons clause knil)))
		'()
		(list
		 (mk-datum-clause boolean?	boolean=?	boolean-entry*)
		 (mk-datum-clause char?		$char=		char-entry*)
		 (mk-datum-clause symbol?	eq?		symbol-entry*)
		 (%make-numbers-clause input-form.stx expr-result.id else-proc.id number-entry*)
		 (mk-datum-clause string?	$string=	string-entry*)
		 (mk-datum-clause bytevector?	$bytevector=	bytevector-entry*)
		 (mk-datum-clause pair?		equal?		pair-entry*)
		 (mk-datum-clause vector?	equal?		vector-entry*)
		 (%make-null-clause input-form.stx expr-result.id null-entry*)
		 )))))

  (define (%clauses->entries input-form.stx clause*.stx)
    (syntax-match clause*.stx ()
      (()
       (values '() '() '()))
      ((?clause . ?other-clause*)
       (let-values
	   (((closure.stx  closure.sym  entry*)		(%process-single-clause input-form.stx ?clause))
	    ((closure*.stx closure*.sym entry**)	(%clauses->entries      input-form.stx ?other-clause*)))
	 (values (cons closure.stx closure*.stx)
		 (cons closure.sym closure*.sym)
		 (cons entry*      entry**))))
      (_
       (syntax-violation __module_who__ "invalid syntax in macro use" input-form.stx))))

  (define (%process-single-clause input-form.stx clause.stx)
    (syntax-match clause.stx (=>)
      ((?datum* => ?closure)
       (not (options::strict-r6rs-enabled?))
       (let ((closure.sym	(make-syntactic-identifier-for-temporary-variable))
	     (obj.sym		(make-syntactic-identifier-for-temporary-variable)))
	 (define closure.stx
	   ;;Perform type propagation for some common idioms.
	   (let ((%make-annotation (lambda ()
				     `(or . ,(map (lambda (datum)
						    `(type-of ,datum))
					       ?datum*)))))
	     (syntax-match ?closure (lambda lambda/checked)
	       ((lambda (?arg) . ?body*)
		(and (options::typed-language-enabled?)
		     (identifier? ?arg))
		(bless
		 `(lambda ((brace ,?arg ,(%make-annotation))) . ,?body*)))
	       ((lambda/checked (?arg) . ?body*)
		(identifier? ?arg)
		(bless
		 `(lambda/checked ((brace ,?arg ,(%make-annotation))) . ,?body*)))
	       (_
		?closure))))
	 ;;We want ?CLOSURE to  be evaluated only if the test  of this clause returns
	 ;;true.  That is why we wrap ?CLOSURE in a further LAMBDA.
	 (values `(lambda/checked (,obj.sym)
		    ((assert-signature-and-return (<procedure>) ,closure.stx) ,obj.sym))
		 closure.sym
		 (let next-datum ((datums  ?datum*)
				  (entries '()))
		   (syntax-match datums ()
		     (()
		      entries)
		     ((?datum . ?datum*)
		      (next-datum ?datum*
				  (cons (cons* ?datum closure.sym #t) entries)))
		     )))))
      ((?datum* . ?body)
       (let ((closure.sym (make-syntactic-identifier-for-temporary-variable)))
	 (values `(lambda/checked () . ,?body)
		 closure.sym
		 (let next-datum ((datums  ?datum*)
				  (entries '()))
		   (syntax-match datums ()
		     (()
		      entries)
		     ((?datum . ?datum*)
		      (next-datum ?datum*
				  (cons (cons* ?datum closure.sym #f) entries)))
		     )))))
      (_
       (syntax-violation __module_who__ "invalid clause syntax" input-form.stx clause.stx))))

  (define (%make-datum-clause input-form.stx expr-result.id else-proc.id pred.id compar.id entry*)
    (if (pair? entry*)
	`((,pred.id ,expr-result.id)
	  (cond ,@(map (lambda (entry)
			 (let ((datum		(car entry))
			       (closure.sym	(cadr entry))
			       (arrow?		(cddr entry)))
			   `((,compar.id ,expr-result.id (quote ,datum))
			     ,(if arrow?
				  `(,closure.sym ,expr-result.id)
				`(,closure.sym)))))
		    entry*)
		(else
		 (,else-proc.id))))
      '()))

  (define (%make-null-clause input-form.stx expr-result.id entry*)
    (if (pair? entry*)
	(if (<= 2 (length entry*))
	    (syntax-violation __module_who__ "invalid datums, null is present multiple times" input-form.stx)
	  `((null? ,expr-result.id)
	    ,(let* ((entry		(car  entry*))
		    (closure.sym	(cadr entry))
		    (arrow?		(cddr entry)))
	       (if arrow?
		   `(,closure.sym ,expr-result.id)
		 `(,closure.sym)))))
      '()))

  (define (%make-numbers-clause input-form.stx expr-result.id else-proc.id entry*)
    ;;For generic  number objects we  use = as comparison  predicate and
    ;;NUMBER?  as type  predicate; but  if  all the  datums are  fixnums
    ;;(which is a common case): we use $FX= as comparison and FIXNUM? as
    ;;type predicate.
    ;;
    (define all-fixnums?
      (for-all (lambda (entry)
		 (let ((datum (car entry)))
		   (fixnum? (syntax->datum datum))))
	entry*))
    (if all-fixnums?
	(%make-datum-clause input-form.stx expr-result.id else-proc.id (core-prim-id 'fixnum?) (core-prim-id '$fx=) entry*)
      (%make-datum-clause input-form.stx expr-result.id else-proc.id (core-prim-id 'number?) (core-prim-id '=) entry*)))

  (define-syntax set-cons!
    (syntax-rules ()
      ((_ ?var ?expr)
       (set! ?var (cons ?expr ?var)))))

  #| end of module |# )


;;;; non-core macro: CASE-IDENTIFIERS

(module (case-identifiers-macro)
  (define-module-who case)

  (define-macro-transformer (case-identifiers input-form.stx)
    ;;Transformer function used  to expand Vicare's CASE-IDENTIFIERS  macros from the
    ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return
    ;;a syntax object that must be further expanded.
    ;;
    (syntax-match input-form.stx (else =>)
      ((_ ?expr)
       (bless
	`(begin
	   ,?expr
	   (quote #!void))))

      ;;Without ELSE clause.
      ((_ ?expr ((?datum0* ?datum** ...) ?body0* ?body** ...) ...)
       (let* ((expr-result.id	(make-syntactic-identifier-for-temporary-variable "expr-result"))
	      (else-proc.id	(make-syntactic-identifier-for-temporary-variable "else-proc"))
	      (else-binding.stx	`(,else-proc.id (lambda/checked () (void)))))
	 (%build-output-form input-form.stx expr-result.id ?expr
			     (map cons
			       (map cons ?datum0* ?datum**)
			       (map cons ?body0*  ?body**))
			     else-proc.id else-binding.stx)))

      ;;With else clause, arrow case.
      ((_ ?expr ((?datum0* ?datum** ...) ?body0* ?body** ...) ... (else => ?else-proc))
       (let* ((expr-result.id	(make-syntactic-identifier-for-temporary-variable "expr-result"))
	      (else-proc.id	(make-syntactic-identifier-for-temporary-variable "else-proc"))
	      (else-binding.stx	`(,else-proc.id (lambda/checked ()
						  ((assert-signature-and-return (<procedure>) ,?else-proc) ,expr-result.id)))))
	 (%build-output-form input-form.stx expr-result.id ?expr
			     (map cons
			       (map cons ?datum0* ?datum**)
			       (map cons ?body0*  ?body**))
			     else-proc.id else-binding.stx)))

      ;;With else clause.
      ((_ ?expr ((?datum0* ?datum** ...) ?body0* ?body** ...) ... (else ?else-body0 ?else-body* ...))
       (let* ((expr-result.id	(make-syntactic-identifier-for-temporary-variable "expr-result"))
	      (else-proc.id	(make-syntactic-identifier-for-temporary-variable "else-proc"))
	      (else-binding.stx	`(,else-proc.id (lambda/checked () ,?else-body0 . ,?else-body*))))
	 (%build-output-form input-form.stx expr-result.id ?expr
			     (map cons
			       (map cons ?datum0* ?datum**)
			       (map cons ?body0*  ?body**))
			     else-proc.id else-binding.stx)))

      (_
       (syntax-violation __module_who__ "invalid syntax in macro use" input-form.stx))))

  (define (%build-output-form input-form.stx expr-result.id expr.stx datum-clause*.stx else-proc.id else-binding.stx)
    (receive (branch-binding*.stx cond-clause*.stx)
	(%process-clauses input-form.stx expr-result.id datum-clause*.stx)
      (bless
       `(letrec/checked ((,expr-result.id ,expr.stx)
			 ,@branch-binding*.stx
			 ,else-binding.stx)
	  (if (identifier? ,expr-result.id)
	      (cond ,@cond-clause*.stx (else (,else-proc.id)))
	    (,else-proc.id))))))

  (define (%process-clauses input-form.stx expr-result.id datum-clause*.stx)
    (syntax-match datum-clause*.stx ()
      (()
       (values '() '()))
      ((?clause . ?other-clause*)
       (let-values
	   (((branch-binding* cond-clause*)	(%process-clauses       input-form.stx expr-result.id ?other-clause*))
	    ((branch-binding  cond-clause)	(%process-single-clause input-form.stx expr-result.id ?clause)))
	 (values (cons branch-binding branch-binding*)
		 (cons cond-clause    cond-clause*))))
      (_
       (syntax-violation __module_who__ "invalid syntax in macro use" input-form.stx))))

  (define (%process-single-clause input-form.stx expr-result.id clause.stx)
    (syntax-match clause.stx (=>)
      ((?datum* => ?closure)
       (let ((closure.sym	(make-syntactic-identifier-for-temporary-variable)))
	 ;;We want ?CLOSURE to  be evaluated only if the test  of this clause returns
	 ;;true.  That is why we wrap ?CLOSURE in a further LAMBDA.
	 (values (bless
		  `(,closure.sym (lambda/std ()
				   ((assert-signature-and-return (<procedure>) ,?closure) ,expr-result.id))))
		 `(,(%build-branch-test input-form.stx expr-result.id ?datum*)
		   (,closure.sym)))))

      ((?datum* . ?body)
       (let ((closure.sym	(make-syntactic-identifier-for-temporary-variable)))
	 (values `(,closure.sym (lambda/std () . ,?body))
		 `(,(%build-branch-test input-form.stx expr-result.id ?datum*)
		   (,closure.sym)))))

      (_
       (syntax-violation __module_who__ "invalid clause syntax" input-form.stx clause.stx))
      ))

  (define (%build-branch-test input-form.stx expr-result.id datum*)
    (syntax-match datum* ()
      ((?datum0 ?datum* ...)
       `(or . ,(map (lambda (datum.stx)
		      (if (identifier? datum.stx)
			  `(free-identifier=? ,expr-result.id (syntax ,datum.stx))
			(syntax-violation __module_who__ "expected identifiers as datums" input-form.stx datum.stx)))
		 (cons ?datum0 ?datum*))))
      (_
       (syntax-violation __module_who__ "invalid clause's data syntax" input-form.stx datum*))
      ))

  #| end of module: CASE-IDENTIFIERS-MACRO |# )


;;;; non-core macro: RECORD-TYPE-AND-RECORD?

(define-macro-transformer (record-type-and-record? input-form.stx)
  ;;Transformer function used to expand Vicare's RECORD-TYPE-AND-RECORD?  syntax uses
  ;;from the top-level built in  environment.  Expand the contents of INPUT-FORM.STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-name ?record)
     (begin
       (unless (identifier? ?type-name)
	 (syntax-violation 'record-type-and-record? "expected identifier as first argument" input-form.stx ?type-name))
       (bless
	`(record-and-rtd? ,?record (record-type-descriptor ,?type-name)))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: DEFINE-CONDITION-TYPE

(define-macro-transformer (define-condition-type input-form.stx)
  ;;Transformer function  used to expand  R6RS RECORD-CONDITION-TYPE macros  from the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (define (main)
    (syntax-match input-form.stx ()
      ((_ ?name ?parent-name ?constructor ?predicate (?field* ?accessor*) ...)
       (begin
	 (%validate-arguments ?name ?parent-name ?constructor ?predicate ?accessor*)
	 (receive (field-name*.id field-type*.id)
	     (map-for-two-retvals %parse-field-spec ?field*)
	   (let* ((UID				(make-syntactic-identifier-for-temporary-variable (syntax->datum ?name)))
		  (RTD				(mkname ?name "-rtd"))
		  (RCD				(mkname ?name "-rcd"))
		  (ACCESSOR-IDX*		(iota 0 ?accessor*))
		  (internal-constructor.sym	(make-syntactic-identifier-for-temporary-variable (syntax->datum ?constructor)))
		  (internal-predicate.sym	(make-syntactic-identifier-for-temporary-variable (syntax->datum ?predicate)))
		  (internal-accessor*.sym	(map (lambda (?accessor)
						       (make-syntactic-identifier-for-temporary-variable (syntax->datum ?accessor)))
						  ?accessor*))
		  (SEALED?			#f)
		  (OPAQUE?			#f)
		  (ARG				(make-syntactic-identifier-for-temporary-variable "arg"))
		  ;;The fields vector has the format:
		  ;;
		  ;;   #((immutable . ?field-name) ...)
		  ;;
		  (FIELDS-VECTOR		(list->vector (map (lambda (field-name.id)
								     (list 'immutable field-name.id))
								field-name*.id)))
		  ;;The normalised fields vector has the format:
		  ;;
		  ;;   #((#f . ?field-name) ...)
		  ;;
		  (NORMAL-FIELDS-VECTOR		(list->vector (map (lambda (field-name.id)
								     (cons #f field-name.id))
								field-name*.id)))
		  ;;This is  an alist having:  field name symbols as  keys; syntactic
		  ;;identifiers representing accessor names as values.
		  (METHOD-TABLE			`(list ,@(map (lambda (field-name.id ?accessor)
								`(cons (quote ,field-name.id) (syntax ,?accessor)))
							   field-name*.id ?accessor*)))
		  ;;This  is  a   closure  accepting  as  single   argument  a  symbol
		  ;;representing a field  name; the single return value is  false or a
		  ;;field accessor closure object.
		  (METHOD-RETRIEVER `(lambda/typed ({_ (or <false> <procedure>)} {,ARG <symbol>})
				       (case ,ARG
					 ,@(map (lambda (field-name.id accessor.id)
						  `((,field-name.id) ,accessor.id))
					     field-name*.id ?accessor*)
					 (else #f))))
		  ;;A, possibly  empty, list of symbolic  expressions representing the
		  ;;definitions of field accessor.  To be spliced in the output.
		  (ACCESSORS (map (lambda (?accessor field-type.id internal-accessor.sym accessor-idx)
				    `(begin
				       (define/checked ((brace ,?accessor ,field-type.id) (brace ,ARG ,?name))
					 (,internal-accessor.sym ,ARG))
				       (define ,internal-accessor.sym
					 ($condition-accessor ,RTD
							      ($record-accessor/index ,RTD ,accessor-idx (quote ,?accessor))
							      (quote ,?accessor)))))
			       ?accessor* field-type*.id internal-accessor*.sym ACCESSOR-IDX*)))
	     ;;We use the  records procedural layer and the unsafe  functions to make
	     ;;it easier to rotate the boot images.
	     (bless
	      `(module (,?name ,?constructor ,?predicate . ,?accessor*)
		 (define-syntax ,?name
		   (make-record-type-spec (syntax ,?name)
					  (quote ,UID)
					  (syntax ,RTD)
					  (syntax ,RCD)
					  #f #;super-rcd
					  (syntax ,?parent-name)
					  (syntax ,?constructor)
					  #f #;destructor
					  (syntax ,?predicate)
					  #f #;equality-predicate
					  #f #;comparison-procedure
					  #f #;hash-function
					  ,METHOD-TABLE
					  '() #;virtual-method-signatures
					  '() #;implemented-interfaces
					  ))
		 (define/std ,RTD
		   ($make-record-type-descriptor-ex (quote ,?name) (record-type-descriptor ,?parent-name)
						    (quote ,UID) #f ,SEALED? ,OPAQUE?
						    (quote ,FIELDS-VECTOR) (quote ,NORMAL-FIELDS-VECTOR)
						    #f ;destructor
						    #f ;printer
						    #f ;equality-predicate
						    #f ;comparison-procedure
						    #f ;hash-function
						    ,METHOD-RETRIEVER
						    #f ;implemented-interfaces
						    ))
		 (define/std ,RCD
		   ($make-record-constructor-descriptor ,RTD (record-constructor-descriptor ,?parent-name) #f))
		 ;;At present  we cannot know the  exact number of arguments  for the
		 ;;constructor: we should  take into account the  arguments needed by
		 ;;the parent constructor.
		 (define/checked ((brace ,?constructor ,?name) . ,ARG)
		   (unsafe-cast-signature (,?name) (apply ,internal-constructor.sym ,ARG)))
		 (define/std ,internal-constructor.sym
		   ($record-constructor ,RCD))
		 (define/checked ({,?predicate <boolean>} ,ARG)
		   (unsafe-cast-signature (<boolean>) (,internal-predicate.sym ,ARG)))
		 (define/std ,internal-predicate.sym
		   ($condition-predicate ,RTD))
		 ,@ACCESSORS
		 #| end of module |# ))))))
      (_
       (__synner__ "invalid syntax in DEFINE-CONDITION-TYPE macro use"))))

  (define (%validate-arguments ?name ?parent-name ?constructor ?predicate ?accessor*)
    (unless (identifier? ?name)
      (__synner__ "expected identifier as condition type name" ?name))
    (unless (identifier? ?parent-name)
      (__synner__ "expected identifier as condition type super-type name" ?parent-name))
    (unless (identifier? ?constructor)
      (__synner__ "expected identifier as condition type constructor name" ?constructor))
    (unless (identifier? ?predicate)
      (__synner__ "expected identifier as condition type predicate name" ?predicate))
    (for-each (lambda (accessor.id)
		(unless (identifier? accessor.id)
		  (__synner__ "expected identifier as condition type field accessor name" accessor.id)))
      ?accessor*))

  (define (%parse-field-spec field-spec.stx)
    (syntax-match field-spec.stx (brace)
      ((brace ?field-name ?field-type)
       (options::typed-language-enabled?)
       (begin
	 (unless (identifier? ?field-name)
	   (__synner__ "expected identifier as condition type field name" ?field-name))
	 (unless (identifier? ?field-type)
	   (__synner__ "expected identifier as condition type field type" ?field-type))
	 (values ?field-name ?field-type)))

      (?field-name
       (identifier? ?field-name)
       (values ?field-name (<top>-type-id)))

      (_
       (__synner__ (if (options::typed-language-enabled?)
		       "expected identifier or typed identifier as condition type field specification"
		     "expected identifier as condition type field name")
		   field-spec.stx))))

  (define (mkname name suffix)
    (datum->syntax name (string->symbol (string-append (symbol->string (syntax->datum name)) suffix))))

  (define (iota idx item*)
    (if (pair? item*)
	(cons idx (iota (fxadd1 idx) (cdr item*)))
      '()))

  (main))


;;;; non-core macro: PARAMETERIZE and PARAMETRISE

(define-macro-transformer (parameterize expr-stx)
  ;;Transformer  function  used  to  expand Vicare's  PARAMETERIZE  macros  from  the
  ;;top-level built in environment.  Expand the contents of EXPR-STX; return a syntax
  ;;object that must be further expanded.
  ;;
  ;;Notice   that   MAKE-PARAMETER   is   a   primitive   function   implemented   in
  ;;"ikarus.compiler.sls" by "E-make-parameter".  Under  Vicare, a parameter function
  ;;can be called with 0, 1 or 2 arguments:
  ;;
  ;;* When called with 1 argument: it returns the parameter's value.
  ;;
  ;;* When called with 2 arguments: it  sets the parameter's value after checking the
  ;;new value with the guard function (if any).
  ;;
  ;;* When called with 3 arguments: it sets the parameter's value optionally checking
  ;;the new value with the guard function (if any).
  ;;
  ;;Under Vicare, PARAMETERIZE  applies the guard function to the  new value only the
  ;;first  time it  is set;  if the  control flow  exits and  returns multiple  times
  ;;beacuse escaping continuations  are used, the guard function is  no more applied;
  ;;this is achieved by setting the flag variable GUARD?.
  ;;
  (syntax-match expr-stx ()
    ((_ () ?body ?body* ...)
     (bless
      `(internal-body ,?body . ,?body*)))

    ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
     (let ((lhs-arg*	(generate-temporaries ?lhs*))
	   (rhs-arg*	(generate-temporaries ?rhs*))
	   (guard?	(make-syntactic-identifier-for-temporary-variable "guard?"))
	   (swap	(make-syntactic-identifier-for-temporary-variable "swap"))
	   (tmp		(make-syntactic-identifier-for-temporary-variable "tmp")))
       (bless
	`((lambda/std ,(append lhs-arg* rhs-arg*)
	    (let*/checked
		(({,guard? <boolean>}	#t) ;apply the guard function only the first time
		 ({,swap   <thunk>}	(lambda/std ()
					  ,@(map (lambda (lhs rhs)
						   `(let/std ((,tmp (,lhs)))
						      (,lhs ,rhs ,guard?)
						      (set! ,rhs ,tmp)))
					      lhs-arg* rhs-arg*)
					  (set! ,guard? #f))))
	      (dynamic-wind
		  ,swap
		  (lambda/std () ,?body . ,?body*)
		  ,swap)))
	  ,@(append ?lhs* ?rhs*)))))

    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: WITH-UNWIND-PROTECTION, UNWIND-PROTECT

(define-macro-transformer (with-unwind-protection input-form.stx)
  ;;Transformer function  used to expand Vicare's  WITH-UNWIND-PROTECTION macros from
  ;;the  top-level built  in  environment.  Expand  the  contents of  INPUT-FORM.STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?unwind-handler ?thunk)
     (let ((terminated?  (make-syntactic-identifier-for-temporary-variable "terminated?"))
	   (normal-exit? (make-syntactic-identifier-for-temporary-variable "normal-exit?"))
	   (why          (make-syntactic-identifier-for-temporary-variable "why"))
	   (escape       (make-syntactic-identifier-for-temporary-variable "escape")))
       (bless
	;;Here we use LET/CHECKED to propagate the type of the last body form.
	`(let/checked
	     (({,terminated?  <boolean>}	#f)
		;True if the dynamic extent of the call to THUNK is terminated.
	      ({,normal-exit? <boolean>}	#f))
		;True  if the  dynamic extent  of the  call to  ?THUNK was  exited by
		;performing a normal return.
	   (dynamic-wind
	       (lambda/std ()
		 (when ,terminated?
		   (non-reinstatable-violation 'with-unwind-protection
		     "attempt to reenter thunk with terminated dynamic extent")))
	       ;;Here we  use LAMBDA/CHECKED to propagate  the type of the  last body
	       ;;form.
	       (lambda/checked ()
		 (begin0
		     (,?thunk)
		   (set! ,normal-exit? #t)))
	       (lambda/std ()
		 (unless ,terminated? ;be safe
		   (cond ((if ,normal-exit?
			      'return
			    ;;This parameter is set to:
			    ;;
			    ;;* The boolean #f if no unwind handler must be run.
			    ;;
			    ;;* The symbol "exception" if  the unwind handler must be
			    ;;run because an exception has been raised and catched by
			    ;;GUARD.
			    ;;
			    ;;* The symbol "escape" if the unwind handler must be run
			    ;;because an unwinding escape procedure has been called.
			    ;;
			    (run-unwind-protection-cleanup-upon-exit?))
			  => (lambda/std (,why)
			       (set! ,terminated? #t)
			       ;;We  want  to discard  any  exception  raised by  the
			       ;;cleanup thunk.
			       (call/cc
				   (lambda/std (,escape)
				     (with-exception-handler
					 ,escape
				       (lambda/std ()
					 (,?unwind-handler ,why)))))))))))))))

    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (unwind-protect input-form.stx)
  ;;Transformer  function used  to  expand Vicare's  UNWIND-PROTECT  macros from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  ;;Not a  general UNWIND-PROTECT  mechanism for  Scheme, but fine  when we  do *not*
  ;;create  continuations that  reenter the  ?BODY  again after  having executed  the
  ;;?CLEANUP forms once.
  ;;
  ;;NOTE This implementation works fine with coroutines.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?body ?cleanup0 ?cleanup* ...)
     (let ((why (make-syntactic-identifier-for-temporary-variable)))
       (bless
	`(with-unwind-protection
	     (lambda/std (,why) ,?cleanup0 . ,?cleanup*)
	   ;;Here we use LAMBDA/CHECKED to propagate the type of the last body form.
	   (lambda/checked () ,?body)))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: WITH-IMPLICITS

(define-macro-transformer (with-implicits input-form.stx)
  ;;Transformer  function used  to  expand Vicare's  WITH-IMPLICITS  macros from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  ;;This macro  is a wrapper  for WITH-SYNTAX  which defines the  identifiers ?SYMBOL
  ;;with the  same context  of ?CTX.   ?CTX must  be an  expression evaluating  to an
  ;;identifier; it  is evaluated  only once.   ?SYMBOL must  be Scheme  symbols.  For
  ;;example:
  ;;
  ;;   (syntax-case stx ()
  ;;     ((id)
  ;;      (identifier? #'id)
  ;;      (with-implicits ((#'id x y))
  ;;        #'(list x y))))
  ;;
  ;;is equivalent to:
  ;;
  ;;   (syntax-case stx ()
  ;;     ((id)
  ;;      (identifier? #'id)
  ;;      (with-syntax ((x (datum->syntax #'id 'x))
  ;;                    (y (datum->syntax #'id 'y)))
  ;;        #'(list x y))))
  ;;
  ;;NOTE This  macro is  derived from  WITH-IMPLICIT, documented  in the  Chez Scheme
  ;;User's Guide.   The two macros  have different API;  where we would  use Vicare's
  ;;variant as:
  ;;
  ;;   (with-implicits ((#'id x y))
  ;;     #'(list x y))
  ;;
  ;;we would use Chez's variant as:
  ;;
  ;;   (with-implicit ((id x y))
  ;;     #'(list x y))
  ;;
  (define (%make-bindings ctx ids)
    (map (lambda (id)
	   `(,id (datum->syntax ,ctx (quote ,id))))
      ids))
  (syntax-match input-form.stx ()

    ((_ () ?body0 ?body* ...)
     (bless
      `(begin ,?body0 . ,?body*)))

    ((_ ((?ctx ?symbol0 ?symbol* ...))
	?body0 ?body* ...)
     (let ((BINDINGS (%make-bindings ?ctx (cons ?symbol0 ?symbol*))))
       (bless
	`(with-syntax ,BINDINGS ,?body0 . ,?body*))))

    ((_ ((?ctx ?symbol0 ?symbol* ...) . ?other-clauses)
	?body0 ?body* ...)
     (let ((BINDINGS (%make-bindings ?ctx (cons ?symbol0 ?symbol*))))
       (bless
	`(with-syntax ,BINDINGS (with-implicits ,?other-clauses ,?body0 . ,?body*)))))

    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: SET-CONS!

(define-macro-transformer (set-cons! input-form.stx)
  ;;Transformer function used to expand  Vicare's SET-CONS! macros from the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id ?obj)
     (identifier? ?id)
     (bless `(set! ,?id (cons ,?obj ,?id))))

    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: compensations

(module (with-compensations/on-error-macro
	 with-compensations-macro)

  (define-macro-transformer (with-compensations/on-error input-form.stx)
    ;;Transformer function used to expand Vicare's WITH-COMPENSATIONS/ON-ERROR macros
    ;;from   the  top-level   built   in  environment.    Expand   the  contents   of
    ;;INPUT-FORM.STX; return a syntax object that must be further expanded.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?body0 ?body* ...)
       (let ((store (make-syntactic-identifier-for-temporary-variable))
	     (why   (make-syntactic-identifier-for-temporary-variable)))
	 (bless
	  ;;Here we use LET/CHECKED to propagate the type of the last body form.
	  `(let/checked ,(%make-store-binding store)
	     (parametrise ((compensations ,store))
	       (with-unwind-protection
		   (lambda/std (,why)
		     (when (eq? ,why 'exception)
		       (run-compensations-store ,store)))
		 ;;Here we use LAMBDA/CHECKED to propagate the type of the last body
		 ;;form.
		 (lambda/checked ()
		   ,?body0 . ,?body*)))))))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define-macro-transformer (with-compensations input-form.stx)
    ;;Transformer function used to expand Vicare's WITH-COMPENSATIONS macros from the
    ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return
    ;;a syntax object that must be further expanded.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?body0 ?body* ...)
       (let ((store (make-syntactic-identifier-for-temporary-variable))
	     (why   (make-syntactic-identifier-for-temporary-variable)))
	 (bless
	  ;;Here we use LET/CHECKED to propagate the type of the last body form.
	  `(let/checked ,(%make-store-binding store)
	     (parametrise ((compensations ,store))
	       (with-unwind-protection
		   (lambda/std (,why)
		     (run-compensations-store ,store))
		 ;;Here we use LAMBDA/CHECKED to propagate  the type of the last body
		 ;;form.
		 (lambda/checked ()
		   ,?body0 . ,?body*)))))))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define (%make-store-binding store)
    (let ((stack        (make-syntactic-identifier-for-temporary-variable))
	  (false/thunk  (make-syntactic-identifier-for-temporary-variable)))
      `((,store (let/std ((,stack '()))
		  (case-lambda/std
		   (()
		    ,stack)
		   ((,false/thunk)
		    (if ,false/thunk
			(set! ,stack (cons ,false/thunk ,stack))
		      (set! ,stack '())))))))))

  #| end of module |# )

(define-macro-transformer (push-compensation input-form.stx)
  ;;Transformer function  used to expand  Vicare's PUSH-COMPENSATION macros  from the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?release0 ?release* ...)
     (bless
      `(push-compensation-thunk (lambda/std () ,?release0 ,@?release*))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (with-compensation-handler input-form.stx)
  ;;Transformer  function used  to expand  Vicare's WITH-COMPENSATION-HANDLER  macros
  ;;from the top-level built in  environment.  Expand the contents of INPUT-FORM.STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?release-thunk ?alloc-thunk)
     (bless
      `(begin
	 (push-compensation-thunk ,?release-thunk)
	 (,?alloc-thunk))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (compensate input-form.stx)
  ;;Transformer function used to expand Vicare's COMPENSATE macros from the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?alloc0 ?form* ...)
     (let ((free #f))
       (define alloc*
	 (let recur ((form-stx ?form*))
	   (syntax-match form-stx (with)
	     (((with ?release0 ?release* ...))
	      (begin
		(set! free `(push-compensation ,?release0 ,@?release*))
		'()))

	     (()
	      (__synner__ "invalid compensation syntax: missing WITH keyword" form-stx))

	     (((with))
	      (__synner__ "invalid compensation syntax: empty WITH keyword" (bless '(with))))

	     ((?alloc ?form* ...)
	      (cons ?alloc (recur ?form*)))
	     )))
       (bless
	(let ((the-obj (make-syntactic-identifier-for-temporary-variable)))
	  `(receive-and-return (,the-obj)
	       (begin ,?alloc0 . ,alloc*)
	     (fluid-let-syntax ((<> (identifier-syntax ,the-obj)))
	       ,free))))
       ;; (bless
       ;;   `(begin0 (begin ,?alloc0 . ,alloc*) ,free))
       ))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: CONCURRENTLY, MONITOR

(define-macro-transformer (concurrently input-form.stx)
  ;;Transformer  function  used  to  expand Vicare's  CONCURRENTLY  macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?thunk0 ?thunk* ...)
     (let ((counter (make-syntactic-identifier-for-temporary-variable "counter")))
       (bless
	`(let/std ((,counter 0))
	   (begin
	     (set! ,counter (add1 ,counter))
	     (coroutine (lambda/std () (,?thunk0) (set! ,counter (sub1 ,counter)))))
	   ,@(map (lambda (thunk)
		    `(begin
		       (set! ,counter (add1 ,counter))
		       (coroutine (lambda/std () (,thunk)  (set! ,counter (sub1 ,counter))))))
	       ?thunk*)
	   (finish-coroutines (lambda/std ()
				(zero? ,counter)))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (monitor input-form.stx)
  ;;Transformer function  used to expand  Vicare's MONITOR macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  ;;Allow only ?CONCURRENT-COROUTINES-MAXIMUM to concurrently enter the monitor.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?concurrent-coroutines-maximum ?thunk)
     (let ((KEY (make-syntactic-identifier-for-temporary-variable)))
       (bless
	`(do-monitor (quote ,KEY) ,?concurrent-coroutines-maximum ,?thunk))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: SYNTAX-RULES, DEFINE-SYNTAX-RULE

(define-macro-transformer (syntax-rules input-form.stx)
  ;;Transformer function used  to expand R6RS SYNTAX-RULES macros  from the top-level
  ;;built in  environment.  Process the  contents of INPUT-FORM.STX; return  a syntax
  ;;object that needs to be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?literal* ...)
	(?pattern* ?template*)
	...)
     (begin
       (verify-syntax-case-literals 'syntax-rules input-form.stx ?literal*)
       (bless
	`(lambda/std (x)
	   (syntax-case x ,?literal*
	     ,@(map (lambda (pattern template)
		      (syntax-match pattern ()
			((_ . ??rest)
			 `((g . ,??rest)
			   (syntax ,template)))
			(_
			 (syntax-violation #f "invalid syntax-rules pattern" input-form.stx pattern))))
		 ?pattern* ?template*))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (define-syntax-rule input-form.stx)
  ;;Transformer function used  to expand Vicare's DEFINE-SYNTAX-RULE  macros from the
  ;;top-level built in environment.  Process the contents of INPUT-FORM.STX; return a
  ;;syntax object that needs to be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?name ?arg* ... . ?rest) ?body0 ?body* ...)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name
	 (syntax-rules ()
	   ((_ ,@?arg* . ,?rest)
	    (begin ,?body0 ,@?body*))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: WITH-SYNTAX

(define-macro-transformer (with-syntax input-form.stx)
  ;;Transformer function  used to expand  R6RS WITH-SYNTAX macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  ;;A WITH-SYNTAX form:
  ;;
  ;;   (with-syntax ((?pat0 ?expr0)
  ;;                 (?pat1 ?expr1))
  ;;     ?body0 ?body ...)
  ;;
  ;;is expanded as follows:
  ;;
  ;;   (syntax-case ?expr0 ()
  ;;     (?pat0
  ;;      (syntax-case ?expr1 ()
  ;;       (?pat1
  ;;        (internal-body ?body0 ?body ...))
  ;;       (_
  ;;        (assertion-violation ---))))
  ;;     (_
  ;;      (assertion-violation ---)))
  ;;
  (syntax-match input-form.stx ()
    ((_ ((?pat* ?expr*) ...) ?body ?body* ...)
     (let ((idn* (let recur ((pat* ?pat*))
		   (if (null? pat*)
		       '()
		     (receive (pat idn*)
			 (convert-pattern (car pat*) '())
		       (append idn* (recur (cdr pat*))))))))
       ;;We validate the  patterns as standard LAMBDA formals.   This function raises
       ;;an exception if a syntax violation is found.
       (syntax-object.parse-standard-formals (map car idn*))
       (let ((t* (generate-temporaries ?expr*)))
	 (bless
	  `(let/std ,(map list t* ?expr*)
	     ,(let recur ((pat* ?pat*)
			  (t*   t*))
		(if (null? pat*)
		    `(internal-body ,?body . ,?body*)
		  `(syntax-case ,(car t*) ()
		     (,(car pat*)
		      ,(recur (cdr pat*) (cdr t*)))
		     (_
		      (assertion-violation 'with-syntax
			"pattern does not match value"
			',(car pat*) ,(car t*)))))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: IDENTIFIER-SYNTAX

(define-macro-transformer (identifier-syntax input-form.stx)
  ;;Transformer  function  used to  expand  R6RS  IDENTIFIER-SYNTAX macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx (set!)
    ((_ ?expr)
     (bless
      `(lambda/std (x)
	 (syntax-case x ()
	   (??id
	    (identifier? (syntax ??id))
	    (syntax ,?expr))
	   ((??id . ??expr*)
	    (identifier? (syntax ??id))
	    (syntax (,?expr . ??expr*)))
	   ))))

    ((_ (?id1
	 ?expr1)
	((set! ?id2 ?expr2)
	 ?expr3))
     (and (identifier? ?id1)
	  (identifier? ?id2)
	  (identifier? ?expr2))
     (bless
      `(make-variable-transformer
	(lambda/std (x)
	  (syntax-case x (set!)
	    (??id
	     (identifier? (syntax ??id))
	     (syntax ,?expr1))
	    ((set! ??id ,?expr2)
	     (syntax ,?expr3))
	    ((??id . ??expr*)
	     (identifier? (syntax ??id))
	     (syntax (,?expr1 . ??expr*))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: LET-VALUES, LET-VALUES/STD, LET-VALUES/CHECKED

(module (let-values-macro let-values/std-macro let-values/checked-macro)
  ;;A LET-VALUES syntax like:
  ;;
  ;;   (let-values (((a b c) rhs0)
  ;;                ((d e f) rhs1))
  ;;     ?body0 ?body ...)
  ;;
  ;;is expanded to:
  ;;
  ;;   (call-with-values
  ;;       (lambda () rhs0)
  ;;     (lambda (G.a G.b G.c)
  ;;       (call-with-values
  ;;           (lambda () rhs1)
  ;;         (lambda (G.d G.e G.f)
  ;;           (let ((a G.a) (b G.b) (c G.c)
  ;;                 (c G.c) (d G.d) (e G.e))
  ;;             ?body0 ?body)))))
  ;;
  (define-module-who let-values)

  (define-macro-transformer (let-values input-form.stx)
    ;;Transformer function used  to expand R6RS LET-VALUES macros  from the top-level
    ;;built in environment.   Expand the contents of INPUT-FORM.STX;  return a syntax
    ;;object that must be further expanded.
    ;;
    (if (options::typed-language-enabled?)
	(let-values/checked-macro input-form.stx)
      (let-values/std-macro input-form.stx)))

;;; --------------------------------------------------------------------

  (module (let-values/std-macro)
    ;;NOTE Remember  that (as much  as I would like)  we *cannot* do  right-hand side
    ;;type propagation for  this standard language syntax: if a  syntactic binding is
    ;;assigned with SET!   the propagated type information  becomes meaningless.  For
    ;;this only reason we have to keep  all the syntactic bindings of type "<top>" or
    ;;"<list>".  (Marco Maggi; Wed May 18, 2016)

    (define-macro-transformer (let-values/std input-form.stx)
      ;;Transformer function used  to expand Vicare's LET-VALUES/STD  macros from the
      ;;top-level  built  in environment.   Expand  the  contents of  INPUT-FORM.STX;
      ;;return a syntax object that must be further expanded.
      ;;
      (syntax-match input-form.stx ()
	((_ () ?body ?body* ...)
	 (cons* (core-prim-id 'internal-body) ?body ?body*))

	((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
	 (receive (standard-formals*.stx formals*.sig)
	     ;;Here we make sure that all the syntactic bindings are declared without
	     ;;type annotations.
	     (%parse-lhs* ?lhs*)
	   (bless
	    (let recur ((standard-formals*.stx		standard-formals*.stx)
			(formals*.sig			formals*.sig)
			(input-formals*.stx		(syntax-unwrap ?lhs*))
			(rhs*.stx			?rhs*)
			(all-formal*.standard-id	'())
			(all-formal*.input-stx		'())
			(all-formal*.tmp-id		'()))
	      (if (null? standard-formals*.stx)
		  `(let/std ,(map list all-formal*.input-stx all-formal*.tmp-id)
		     ,?body . ,?body*)
		(syntax-match (car standard-formals*.stx) ()
		  ((?standard-formal* ...)
		   (receive (this-clause-formal*.tmp-id all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)
		       (%rename* ?standard-formal* (car input-formals*.stx)
				 all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
				 __synner__)
		     `(receive/std ,this-clause-formal*.tmp-id
			  ,(car rhs*.stx)
			,(recur (cdr standard-formals*.stx) (cdr formals*.sig) (cdr input-formals*.stx)
				(cdr rhs*.stx) all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id))))

		  ((?standard-formal* ... . ?standard-rest-formal)
		   (receive (arg-formal*.input-stx rest-formal.input-stx)
		       (improper-list->list-and-rest (car input-formals*.stx))
		     (receive (this-clause-formal.rest-tmp-id all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)
			 (%rename  ?standard-rest-formal rest-formal.input-stx
				   all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
				   __synner__)
		       (receive (this-clause-formal*.tmp-id all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)
			   (%rename* ?standard-formal* arg-formal*.input-stx
				     all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
				     __synner__)
			 `(receive/std ,(append this-clause-formal*.tmp-id this-clause-formal.rest-tmp-id)
			      ,(car rhs*.stx)
			    ,(recur (cdr standard-formals*.stx) (cdr formals*.sig) (cdr input-formals*.stx) (cdr rhs*.stx)
				    all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id))))))

		  (?others
		   (__synner__ "malformed syntactic bindings specification" ?others))))))))

	(_
	 (__synner__ "invalid syntax in macro use"))))

    (case-define %parse-lhs*
      ((input-formals*.stx)
       (%parse-lhs* input-formals*.stx '() '()))
      ((input-formals*.stx rev-standard-formals*.stx rev-formals*.sig)
       ;;Recursive function.  Parse the input formals.
       ;;
       ;;The argument INPUT-FORMALS*.STX is a  proper list of input syntactic binding
       ;;specifications; each item in the list may be a proper or improper list.
       ;;
       ;;The argument REV-STANDARD-FORMALS*.STX  is a proper list  of fully unwrapped
       ;;syntax objects  representing the standard syntactic  binding specifications;
       ;;each item  in the  list may  be a proper  or improper  list.  This  value is
       ;;accumulated at each recursive call.
       ;;
       ;;The  argument  REV-FORMALS*.SIG  is  a  proper  list  of  "<type-signature>"
       ;;instances representing  the type signatures  of the formals.  This  value is
       ;;accumulated at each recursive call.
       ;;
       (if (pair? input-formals*.stx)
	   ;;STANDARD-FORMALS.STX is a fully unwrapped syntax object representing the
	   ;;standard  formals in  a single  clause.  FORMALS.SIG  is an  instance of
	   ;;"<type-signature>".
	   (receive (standard-formals.stx formals.sig)
	       (syntax-object.parse-standard-formals (car input-formals*.stx))
	     (%parse-lhs* (cdr input-formals*.stx)
			  (cons standard-formals.stx	rev-standard-formals*.stx)
			  (cons formals.sig		rev-formals*.sig)))
	 (values (reverse rev-standard-formals*.stx)
		 (reverse rev-formals*.sig)))))

    #| end of module: LET-VALUES/STD-MACRO |# )

;;; --------------------------------------------------------------------

  (module (let-values/checked-macro)

    (define-macro-transformer (let-values/checked input-form.stx)
      ;;Transformer function  used to expand Vicare's  LET-VALUES/CHECKED macros from
      ;;the top-level built  in environment.  Expand the  contents of INPUT-FORM.STX;
      ;;return a syntax object that must be further expanded.
      ;;
      (syntax-match input-form.stx ()
	((_ () ?body ?body* ...)
	 (cons* (core-prim-id 'internal-body) ?body ?body*))

	((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
	 (receive (standard-formals*.stx formals*.sig)
	     (%parse-lhs* ?lhs*)
	   (bless
	    (let recur ((standard-formals*.stx		standard-formals*.stx)
			(formals*.sig			formals*.sig)
			(input-formals*.stx		(syntax-unwrap ?lhs*))
			(rhs*.stx			?rhs*)
			(all-formal*.standard-id	'())
			(all-formal*.input-stx		'())
			(all-formal*.tmp-id		'()))
	      (if (null? standard-formals*.stx)
		  `(let/checked ,(map list all-formal*.input-stx all-formal*.tmp-id)
		     ,?body . ,?body*)
		(syntax-match (car standard-formals*.stx) ()
		  ((?standard-formal* ...)
		   (receive (this-clause-formal*.tmp-id all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)
		       (%rename* ?standard-formal* (car input-formals*.stx)
				 all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
				 __synner__)
		     `(receive/checked ,this-clause-formal*.tmp-id
			  (assert-signature-and-return ,(type-signature.syntax-object (car formals*.sig)) ,(car rhs*.stx))
			,(recur (cdr standard-formals*.stx) (cdr formals*.sig) (cdr input-formals*.stx) (cdr rhs*.stx)
				all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id))))

		  ((?standard-formal* ... . ?standard-rest-formal)
		   (receive (arg-formal*.input-stx rest-formal.input-stx)
		       (input-formals->arg*-and-rest (car input-formals*.stx))
		     (receive (this-clause-formal.rest-tmp-id all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)
			 (%rename  ?standard-rest-formal rest-formal.input-stx
				   all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
				   __synner__)
		       (receive (this-clause-formal*.tmp-id all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)
			   (%rename* ?standard-formal* arg-formal*.input-stx
				     all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
				     __synner__)
			 `(receive/checked ,(append this-clause-formal*.tmp-id this-clause-formal.rest-tmp-id)
			      ,(car rhs*.stx)
			    ,(recur (cdr standard-formals*.stx) (cdr formals*.sig) (cdr input-formals*.stx) (cdr rhs*.stx)
				    all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id))))))

		  (?others
		   (__synner__ "malformed syntactic bindings specification" ?others))))))))

	(_
	 (__synner__ "invalid syntax in macro use"))))

    (case-define %parse-lhs*
      ((input-formals*.stx)
       (%parse-lhs* input-formals*.stx '() '()))
      ((input-formals*.stx rev-standard-formals*.stx rev-formals*.sig)
       ;;Recursive function.  Parse the input formals.
       ;;
       ;;The argument INPUT-FORMALS*.STX is a  proper list of input syntactic binding
       ;;specifications; each item in the list may be a proper or improper list.
       ;;
       ;;The argument REV-STANDARD-FORMALS*.STX  is a proper list  of fully unwrapped
       ;;syntax objects  representing the standard syntactic  binding specifications;
       ;;each item  in the  list may  be a proper  or improper  list.  This  value is
       ;;accumulated at each recursive call.
       ;;
       ;;The  argument  REV-FORMALS*.SIG  is  a  proper  list  of  "<type-signature>"
       ;;instances representing  the type signatures  of the formals.  This  value is
       ;;accumulated at each recursive call.
       ;;
       (if (pair? input-formals*.stx)
	   ;;STANDARD-FORMALS.STX is a fully unwrapped syntax object representing the
	   ;;standard  formals in  a single  clause.  FORMALS.SIG  is an  instance of
	   ;;"<type-signature>".
	   (receive (standard-formals.stx formals.sig)
	       ;;Here  we  use  "<untyped>"  for  syntactic  bindings  with  no  type
	       ;;annotation, because we want to perform RHS type propagation.
	       (syntax-object.parse-typed-formals (car input-formals*.stx) (<untyped>-ots))
	     (%parse-lhs* (cdr input-formals*.stx)
			  (cons standard-formals.stx	rev-standard-formals*.stx)
			  (cons formals.sig		rev-formals*.sig)))
	 (values (reverse rev-standard-formals*.stx)
		 (reverse rev-formals*.sig)))))

    (define* (input-formals->arg*-and-rest input-formals.stx)
      (let loop ((formals.stx		input-formals.stx)
		 (rev-arg*.stx		'()))
	(syntax-match formals.stx (brace)
	  ((brace ?id ?type)
	   (values (reverse rev-arg*.stx) formals.stx))
	  (?id
	   (identifier? ?id)
	   (values (reverse rev-arg*.stx) formals.stx))
	  (()
	   (assertion-violation __who__ "internal error: expected improper input formals" input-formals.stx))
	  ((?arg . ?rest)
	   (loop ?rest (cons ?arg rev-arg*.stx))))))

    #| end of module: LET-VALUES/CHECKED |# )

;;; --------------------------------------------------------------------

  (define (%rename formal.standard-id formal.input-stx
		   all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
		   synner)
    ;;This function operates on a single syntactic binding specification.
    ;;
    (when (bound-id-member? formal.standard-id all-formal*.standard-id)
      (synner "duplicate name for syntactic binding" formal.standard-id))
    (let ((this-clause-formal.tmp-id (make-syntactic-identifier-for-temporary-variable (syntax->datum formal.standard-id))))
      (values this-clause-formal.tmp-id
	      (cons formal.standard-id		all-formal*.standard-id)
	      (cons formal.input-stx		all-formal*.input-stx)
	      (cons this-clause-formal.tmp-id	all-formal*.tmp-id))))

  (define (%rename* arg-formal*.standard-id arg-formal*.input-stx
		    all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
		    synner)
    ;;This function operates on proper lists of syntactic binding specifications.
    ;;
    (if (pair? arg-formal*.standard-id)
	(receive (this-clause-formal.tmp-id all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)
	    (%rename (car arg-formal*.standard-id) (car arg-formal*.input-stx)
		     all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
		     synner)
	  (receive (this-clause-formal*.tmp-id all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)
	      (%rename* (cdr arg-formal*.standard-id) (cdr arg-formal*.input-stx)
			all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id
			synner)
	    (values (cons this-clause-formal.tmp-id this-clause-formal*.tmp-id)
		    all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)))
      (values '() all-formal*.standard-id all-formal*.input-stx all-formal*.tmp-id)))

  #| end of module |# )


;;;; non-core macro: LET*-VALUES, LET*-VALUES/STD, LET*-VALUES/CHECKED

(define-macro-transformer (let*-values input-form.stx)
  ;;Transformer function  used to expand  R6RS LET*-VALUES macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  ;;A LET*-VALUES syntax like:
  ;;
  ;;   (let*-values (((a b c) rhs0)
  ;;                 ((d e f) rhs1))
  ;;     ?body0 ?body ...)
  ;;
  ;;is expanded to:
  ;;
  ;;   (call-with-values
  ;;       (lambda () rhs0)
  ;;     (lambda (a b c)
  ;;       (call-with-values
  ;;           (lambda () rhs1)
  ;;         (lambda (d e f)
  ;;           (begin ?body0 ?body)))))
  ;;
  (if (options::typed-language-enabled?)
      (let*-values/checked-macro input-form.stx)
    (let*-values/std-macro input-form.stx)))

(define-macro-transformer (let*-values/std input-form.stx)
  ;;Transformer  function used  to expand  Vicare's LET*-VALUES/STD  macros from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ () ?body ?body* ...)
     (cons* (core-prim-id 'internal-body) ?body ?body*))

    ((_ ((?lhs ?rhs)) ?body ?body* ...)
     (bless
      `(receive/std ,?lhs
	   ,?rhs
	 ,?body . ,?body*)))

    ((_ ((?lhs0 ?rhs0) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(receive/std ,?lhs0
	   ,?rhs0
	 (let*-values/std ,(map list ?lhs* ?rhs*)
	   ,?body . ,?body*))))

    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (let*-values/checked input-form.stx)
  ;;Transformer function used to expand  Vicare's LET*-VALUES/CHECKED macros from the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ () ?body ?body* ...)
     (cons* (core-prim-id 'internal-body) ?body ?body*))

    ((_ ((?lhs ?rhs)) ?body ?body* ...)
     (bless
      `(receive/checked ,?lhs
	   ,?rhs
	 ,?body . ,?body*)))

    ((_ ((?lhs0 ?rhs0) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(receive/checked ,?lhs0
	   ,?rhs0
	 (let*-values/checked ,(map list ?lhs* ?rhs*)
	   ,?body . ,?body*))))

    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: VALUES->LIST-MACRO

(define-macro-transformer (values->list input-form.stx)
  ;;Transformer  function  used  to  expand Vicare's  VALUES->LIST  macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (bless
      (let ((ARGS (make-syntactic-identifier-for-temporary-variable "args")))
	`(receive ,ARGS
	     ,?expr
	   ,ARGS))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: LET*-SYNTAX

(define-macro-transformer (let*-syntax input-form.stx)
  ;;Transformer  function  used  to  expand  Vicare's  LET*-SYNTAX  macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(begin ,?body . ,?body*)))
    ;;Single binding.
    ((_ ((?lhs ?rhs)) ?body ?body* ...)
     (bless
      `(let-syntax ((,?lhs ,?rhs))
	 ,?body . ,?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(let-syntax ((,?lhs ,?rhs))
	 (let*-syntax ,(map list ?lhs* ?rhs*)
	   ,?body . ,?body*))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: LET-CONSTANTS, LET*-CONSTANTS, LETREC-CONSTANTS, LETREC*-CONSTANTS

(define-macro-transformer (let-constants input-form.stx)
  ;;Transformer  function  used to  expand  Vicare's  LET-CONSTANTS macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(internal-body ,?body . ,?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (let ((SHADOW* (generate-temporaries (cons ?lhs ?lhs*))))
       (bless
	`(let/std ,(map list SHADOW* (cons ?rhs ?rhs*))
	   (let-syntax ,(map (lambda (lhs shadow)
			       `(,lhs (identifier-syntax ,shadow)))
			  (cons ?lhs ?lhs*) SHADOW*)
	     ,?body . ,?body*)))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (let*-constants input-form.stx)
  ;;Transformer  function used  to  expand Vicare's  LET*-CONSTANTS  macros from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(internal-body ,?body . ,?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(let-constants ((,?lhs ,?rhs))
	 (let*-constants ,(map list ?lhs* ?rhs*)
	   ,?body . ,?body*))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (letrec-constants input-form.stx)
  ;;Transformer function  used to  expand Vicare's  LETREC-CONSTANTS macros  from the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ () ?body0 ?body* ...)
     (bless
      `(internal-body ,?body0 . ,?body*)))

    ((_ ((?lhs* ?rhs*) ...) ?body0 ?body* ...)
     (let ((TMP* (generate-temporaries ?lhs*))
	   (VAR* (generate-temporaries ?lhs*)))
       (bless
	`(let/std ,(map (lambda (var)
			  `(,var (void)))
		     VAR*)
	   (let-syntax ,(map (lambda (lhs var)
			       `(,lhs (identifier-syntax ,var)))
			  ?lhs* VAR*)
	     ;;Do not enforce the order of evaluation of ?RHS.
	     (let/std ,(map list TMP* ?rhs*)
	       ,@(map (lambda (var tmp)
			`(set! ,var ,tmp))
		   VAR* TMP*)
	       (internal-body ,?body0 . ,?body*)))))))

    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (letrec*-constants input-form.stx)
  ;;Transformer function  used to expand  Vicare's LETREC*-CONSTANTS macros  from the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ () ?body0 ?body* ...)
     (bless
      `(internal-body ,?body0 . ,?body*)))

    ((_ ((?lhs* ?rhs*) ...) ?body0 ?body* ...)
     (let ((TMP* (generate-temporaries ?lhs*))
	   (VAR* (generate-temporaries ?lhs*)))
       (bless
	`(let/std ,(map (lambda (var)
			  `(,var (void)))
		     VAR*)
	   (let-syntax ,(map (lambda (lhs var)
			       `(,lhs (identifier-syntax ,var)))
			  ?lhs* VAR*)
	     ;;Do enforce the order of evaluation of ?RHS.
	     (let*/std ,(map list TMP* ?rhs*)
	       ,@(map (lambda (var tmp)
			`(set! ,var ,tmp))
		   VAR* TMP*)
	       (internal-body ,?body0 . ,?body*)))))))

    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: DEFINE, CASE-DEFINE

(define-macro-transformer (define input-form.stx)
  ;;Transformer function  used to  expand Vicare's DEFINE  macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ . ?stuff)
     (cons (if (options::typed-language-enabled?)
	       (core-prim-id 'define/checked)
	     (core-prim-id 'define/std))
	   ?stuff))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (case-define input-form.stx)
  ;;Transformer  function  used  to  expand  Vicare's  CASE-DEFINE  macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ . ?stuff)
     (cons (if (options::typed-language-enabled?)
	       (core-prim-id 'case-define/checked)
	     (core-prim-id 'case-define/std))
	   ?stuff))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: DEFINE*, LAMBDA*, NAMED-LAMBDA*, CASE-DEFINE*, CASE-LAMBDA*, NAMED-CASE-LAMBDA*

(module (lambda*-macro
	 named-lambda*-macro
	 define*-macro
	 case-lambda*-macro
	 named-case-lambda*-macro
	 case-define*-macro)

  (define-core-record-type argument-validation-spec
    (nongenerative vicare:expander:argument-validation-spec)
    (fields arg-id
		;Identifier  representing  the  formal  name of  the  argument  being
		;validated.
	    pred
		;Syntax object representing the validation logic predicate.
	    expr
		;Syntax object representing an argument's validation expression.
	    list-arg?
		;Boolean.  True if this struct represents a rest or args argument.
	    ))

  (define-core-record-type retval-validation-spec
    (nongenerative vicare:expander:retval-validation-spec)
    (fields rv-id
		;Identifier representing the internal formal name of the return value
		;being validated.
	    pred
		;Syntax object representing the validation logic predicate.
	    expr
		;Syntax object representing a return value's validation expression.
	    ))

;;; --------------------------------------------------------------------

  (module (define*-macro)
    ;;Transformer function used to expand  Vicare's DEFINE* macros from the top-level
    ;;built in environment.  Expand the contents of EXPR.STX.  Return a syntax object
    ;;that must be further expanded.
    ;;
    (define-macro-transformer (define* expr.stx)
      (bless
       (syntax-match expr.stx (brace)
	 ;;No ret-pred.
	 ((_ (?who . ?formals) ?body0 ?body* ...)
	  (identifier? ?who)
	  (%generate-define-output-form/without-ret-pred ?who ?formals (cons ?body0 ?body*) __synner__))

	 ;;Return value predicates.
	 ((_ ((brace ?who ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	  (identifier? ?who)
	  (%generate-define-output-form/with-ret-pred ?who (cons ?ret-pred0 ?ret-pred*) ?formals (cons ?body0 ?body*) __synner__))

	 ((_ ?who ?expr)
	  (identifier? ?who)
	  `(define/std ,?who
	     (fluid-let-syntax ((__who__ (identifier-syntax (quote ,?who))))
	       ,?expr)))

	 ((_ ?who)
	  (identifier? ?who)
	  `(define/std ,?who))

	 (_
	  (__synner__ "invalid syntax in macro use")))))

    (define (%generate-define-output-form/without-ret-pred who.id predicate-formals.stx body*.stx synner)
      ;;Build and return a symbolic expression, to be BLESSed later, representing the
      ;;definition.
      ;;
      ;;STANDARD-FORMALS.STX  is an  improper  list of  identifiers representing  the
      ;;standard formals.  ARG-VALIDATION-SPEC* is a list of ARGUMENT-VALIDATION-SPEC
      ;;structures, each representing a validation predicate.
      (receive (standard-formals.stx arg-validation-spec*)
	  (%parse-predicate-formals predicate-formals.stx synner)
	`(define/std (,who.id . ,standard-formals.stx)
	   (fluid-let-syntax
	       ((__who__ (identifier-syntax (quote ,who.id))))
	     ,(if (options::enable-arguments-validation?)
		  ;;With validation.
		  `(begin
		     ,@(%make-arg-validation-forms who.id arg-validation-spec* synner)
		     (internal-body . ,body*.stx))
		;;Without validation
		`(begin . ,body*.stx))))))

    (define (%generate-define-output-form/with-ret-pred who.id ret-pred*.stx predicate-formals.stx body*.stx synner)
      ;;Build and return a symbolic expression, to be BLESSed later, representing the
      ;;definition.
      ;;
      ;;STANDARD-FORMALS.STX  is an  improper  list of  identifiers representing  the
      ;;standard formals.  ARG-VALIDATION-SPEC* is a list of ARGUMENT-VALIDATION-SPEC
      ;;structures, each representing a validation predicate.
      (receive (standard-formals.stx arg-validation-spec*)
	  (%parse-predicate-formals predicate-formals.stx synner)
	`(define/std (,who.id . ,standard-formals.stx)
	   (fluid-let-syntax
	       ((__who__ (identifier-syntax (quote ,who.id))))
	     ,(if (options::enable-arguments-validation?)
		  ;;With validation.
		  (let* ((RETVAL*            (generate-temporaries ret-pred*.stx))
			 (RETVAL-VALIDATION* (%make-ret-validation-forms
					      who.id
					      (map (lambda (rv.id pred.stx)
						     (make-retval-validation-spec rv.id pred.stx
										  (%parse-logic-predicate-syntax pred.stx rv.id synner)))
						RETVAL* ret-pred*.stx)
					      synner)))
		    `(begin
		       ,@(%make-arg-validation-forms who.id arg-validation-spec* synner)
		       (receive-and-return ,RETVAL*
			   (internal-body . ,body*.stx)
			 . ,RETVAL-VALIDATION*)))
		;;Without validation.
		`(begin . ,body*.stx))))))

    #| end of module: DEFINE*-MACRO |# )

;;; --------------------------------------------------------------------

  (module (case-define*-macro)

    (define-macro-transformer (case-define* expr.stx)
      ;;Transformer function  used to  expand Vicare's  CASE-DEFINE* macros  from the
      ;;top-level built in  environment.  Expand the contents of  EXPR.STX.  Return a
      ;;syntax object that must be further expanded.
      ;;
      (syntax-match expr.stx ()
	((_ ?who ?clause0 ?clause* ...)
	 (identifier? ?who)
	 (bless
	  `(case-define/std ,?who
	     ,@(map (lambda (clause.stx)
		      (%generate-case-define-form ?who clause.stx __synner__))
		 (cons ?clause0 ?clause*)))))
	(_
	 (__synner__ "invalid syntax in macro use"))))

    (define (%generate-case-define-form who.id clause.stx synner)
      (syntax-match clause.stx (brace)
	;;Return value predicates.
	((((brace ?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (underscore-id? ?underscore)
	 (%generate-case-define-clause-form/with-ret-pred who.id (cons ?ret-pred0 ?ret-pred*) ?formals (cons ?body0 ?body*) synner))

	;;No ret-pred.
	((?formals ?body0 ?body* ...)
	 (%generate-case-define-clause-form/without-ret-pred who.id ?formals (cons ?body0 ?body*) synner))
	))

    (define (%generate-case-define-clause-form/without-ret-pred who.id predicate-formals.stx body*.stx synner)
      ;;Build and return  a symbolic expression, to be BLESSed  later, representing a
      ;;definition clause.
      ;;
      ;;STANDARD-FORMALS.STX  is an  improper  list of  identifiers representing  the
      ;;standard formals.  ARG-VALIDATION-SPEC* is a list of ARGUMENT-VALIDATION-SPEC
      ;;structures, each representing a validation predicate.
      (receive (standard-formals.stx arg-validation-spec*)
	  (%parse-predicate-formals predicate-formals.stx synner)
	`(,standard-formals.stx
	  (fluid-let-syntax
	      ((__who__ (identifier-syntax (quote ,who.id))))
	    ,(if (options::enable-arguments-validation?)
		 ;;With validation.
		 `(begin
		    ,@(%make-arg-validation-forms who.id arg-validation-spec* synner)
		    (internal-body . ,body*.stx))
	       ;;Without validation.
	       `(begin . ,body*.stx))))))

    (define (%generate-case-define-clause-form/with-ret-pred who.id ret-pred*.stx predicate-formals.stx body*.stx synner)
      ;;Build and return  a symbolic expression, to be BLESSed  later, representing a
      ;;definition clause.
      ;;
      ;;STANDARD-FORMALS.STX  is an  improper  list of  identifiers representing  the
      ;;standard formals.  ARG-VALIDATION-SPEC* is a list of ARGUMENT-VALIDATION-SPEC
      ;;structures, each representing a validation predicate.
      (receive (standard-formals.stx arg-validation-spec*)
	  (%parse-predicate-formals predicate-formals.stx synner)
	`(,standard-formals.stx
	  (fluid-let-syntax
	      ((__who__ (identifier-syntax (quote ,who.id))))
	    ,(if (options::enable-arguments-validation?)
		 ;;With validation.
		 (let* ((RETVAL*            (generate-temporaries ret-pred*.stx))
			(RETVAL-VALIDATION* (%make-ret-validation-forms
					     who.id
					     (map (lambda (rv.id pred.stx)
						    (make-retval-validation-spec rv.id pred.stx
										 (%parse-logic-predicate-syntax pred.stx rv.id synner)))
					       RETVAL* ret-pred*.stx)
					     synner)))
		   `(begin
		      ,@(%make-arg-validation-forms who.id arg-validation-spec* synner)
		      (receive-and-return ,RETVAL*
			  (internal-body . ,body*.stx)
			. ,RETVAL-VALIDATION*)))
	       ;;Without validation.
	       `(begin . ,body*.stx))))))

    #| end of module: CASE-DEFINE*-MACRO |# )

;;; --------------------------------------------------------------------

  (module (lambda*-macro named-lambda*-macro)

    (define-macro-transformer (lambda* expr.stx)
      ;;Transformer  function  used  to  expand  Vicare's  LAMBDA*  macros  from  the
      ;;top-level built in  environment.  Expand the contents of  EXPR.STX.  Return a
      ;;syntax object that must be further expanded.
      ;;
      (define who.id
	(underscore-id))
      (bless
       (syntax-match expr.stx (brace)
	 ;;Ret-pred with list spec.
	 ((_ ((brace ?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	  (underscore-id? ?underscore)
	  (%generate-lambda-output-form/with-ret-pred who.id (cons ?ret-pred0 ?ret-pred*) ?formals (cons ?body0 ?body*) __synner__))

	 ;;No ret-pred.
	 ((_ ?formals ?body0 ?body* ...)
	  (%generate-lambda-output-form/without-ret-pred who.id ?formals (cons ?body0 ?body*) __synner__))

	 (_
	  (__synner__ "invalid syntax in macro use")))))

    (define-macro-transformer (named-lambda* expr.stx)
      ;;Transformer function  used to expand  Vicare's NAMED-LAMBDA* macros  from the
      ;;top-level built in  environment.  Expand the contents of  EXPR.STX.  Return a
      ;;syntax object that must be further expanded.
      ;;
      (bless
       (syntax-match expr.stx (brace)
	 ;;Ret-pred with list spec.
	 ((_ ?who ((brace ?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	  (and (underscore-id? ?underscore)
	       (identifier? ?who))
	  (%generate-lambda-output-form/with-ret-pred ?who (cons ?ret-pred0 ?ret-pred*) ?formals (cons ?body0 ?body*) __synner__))

	 ;;No ret-pred.
	 ((_ ?who ?formals ?body0 ?body* ...)
	  (identifier? ?who)
	  (%generate-lambda-output-form/without-ret-pred ?who ?formals (cons ?body0 ?body*) __synner__))

	 (_
	  (__synner__ "invalid syntax in macro use")))))

    (define (%generate-lambda-output-form/without-ret-pred who.id predicate-formals.stx body*.stx synner)
      ;;Build and return a symbolic expression, to be BLESSed later, representing the
      ;;LAMBDA syntax use.
      ;;
      ;;STANDARD-FORMALS.STX  is an  improper  list of  identifiers representing  the
      ;;standard formals.  ARG-VALIDATION-SPEC* is a list of ARGUMENT-VALIDATION-SPEC
      ;;structures, each representing a validation predicate.
      (receive (standard-formals.stx arg-validation-spec*)
	  (%parse-predicate-formals predicate-formals.stx synner)
        `(named-lambda/std ,who.id ,standard-formals.stx
	   ,(if (options::enable-arguments-validation?)
		;;With validation.
		`(begin
		   ,@(%make-arg-validation-forms who.id arg-validation-spec* synner)
		   (internal-body . ,body*.stx))
	      ;;Without validation.
	      `(begin . ,body*.stx)))))

    (define (%generate-lambda-output-form/with-ret-pred who.id ret-pred*.stx predicate-formals.stx body*.stx synner)
      ;;Build and return a symbolic expression, to be BLESSed later, representing the
      ;;LAMBDA syntax use.
      ;;
      ;;STANDARD-FORMALS.STX  is an  improper  list of  identifiers representing  the
      ;;standard formals.  ARG-VALIDATION-SPEC* is a list of ARGUMENT-VALIDATION-SPEC
      ;;structures, each representing a validation predicate.
      (receive (standard-formals.stx arg-validation-spec*)
	  (%parse-predicate-formals predicate-formals.stx synner)
	`(named-lambda/std ,who.id ,standard-formals.stx
	   ,(if (options::enable-arguments-validation?)
		;;With validation.
		(let* ((RETVAL*            (generate-temporaries ret-pred*.stx))
		       (RETVAL-VALIDATION* (%make-ret-validation-forms
					    who.id
					    (map (lambda (rv.id pred.stx)
						   (make-retval-validation-spec rv.id pred.stx
										(%parse-logic-predicate-syntax pred.stx rv.id synner)))
					      RETVAL* ret-pred*.stx)
					    synner)))
		  `(begin
		     ,@(%make-arg-validation-forms who.id arg-validation-spec* synner)
		     (receive-and-return ,RETVAL*
			 (internal-body . ,body*.stx)
		       . ,RETVAL-VALIDATION*)))
	      ;;Without validation
	      `(begin . ,body*.stx)))))

    #| end of module: LAMBDA*-MACRO |# )

;;; --------------------------------------------------------------------

  (module (case-lambda*-macro named-case-lambda*-macro)

    (define-macro-transformer (case-lambda* expr.stx)
      ;;Transformer function  used to  expand Vicare's  CASE-LAMBDA* macros  from the
      ;;top-level built in  environment.  Expand the contents of  EXPR.STX.  Return a
      ;;syntax object that must be further expanded.
      ;;
      (define who.id
	(underscore-id))
      (syntax-match expr.stx ()
	((_ ?clause0 ?clause* ...)
	 (bless
	  `(named-case-lambda/std _
	    ,@(map (lambda (clause.stx)
		     (%generate-case-lambda-form (quote _) clause.stx __synner__))
		(cons ?clause0 ?clause*)))))
	(_
	 (__synner__ "invalid syntax in macro use"))))

    (define-macro-transformer (named-case-lambda* expr.stx)
      ;;Transformer function  used to expand Vicare's  NAMED-CASE-LAMBDA* macros from
      ;;the top-level built in environment.  Expand the contents of EXPR.STX.  Return
      ;;a syntax object that must be further expanded.
      ;;
      (syntax-match expr.stx ()
	((_ ?who ?clause0 ?clause* ...)
	 (identifier? ?who)
	 (bless
	  `(named-case-lambda/std ,?who
	    ,@(map (lambda (clause.stx)
		     (%generate-case-lambda-form ?who clause.stx __synner__))
		(cons ?clause0 ?clause*)))))
	(_
	 (__synner__ "invalid syntax in macro use"))))

    (define (%generate-case-lambda-form who.id clause.stx synner)
      (syntax-match clause.stx (brace)
	;;Ret-pred with list spec.
	((((brace ?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (underscore-id? ?underscore)
	 (%generate-case-lambda-clause-form/with-ret-pred who.id (cons ?ret-pred0 ?ret-pred*) ?formals (cons ?body0 ?body*) synner))

	;;No ret-pred.
	((?formals ?body0 ?body* ...)
	 (%generate-case-lambda-clause-form/without-ret-pred who.id ?formals (cons ?body0 ?body*) synner))
	))

    (define (%generate-case-lambda-clause-form/without-ret-pred who.id predicate-formals.stx body*.stx synner)
      ;;Build and return a symbolic expression, to be BLESSed later, representing the
      ;;CASE-LAMBDA clause.
      ;;
      ;;STANDARD-FORMALS.STX  is an  improper  list of  identifiers representing  the
      ;;standard formals.  ARG-VALIDATION-SPEC* is a list of ARGUMENT-VALIDATION-SPEC
      ;;structures, each representing a validation predicate.
      (receive (standard-formals.stx arg-validation-spec*)
	  (%parse-predicate-formals predicate-formals.stx synner)
	`(,standard-formals.stx
	  ,(if (options::enable-arguments-validation?)
	       ;;With validation.
	       `(begin
		  ,@(%make-arg-validation-forms who.id arg-validation-spec* synner)
		  (internal-body . ,body*.stx))
	     ;;Without validation.
	     `(begin . ,body*.stx)))))

    (define (%generate-case-lambda-clause-form/with-ret-pred who.id ret-pred*.stx predicate-formals.stx body*.stx synner)
      ;;Build and return a symbolic expression, to be BLESSed later, representing the
      ;;CASE-LAMBDA clause.
      ;;
      ;;STANDARD-FORMALS.STX  is an  improper  list of  identifiers representing  the
      ;;standard formals.  ARG-VALIDATION-SPEC* is a list of ARGUMENT-VALIDATION-SPEC
      ;;structures, each representing a validation predicate.
      (receive (standard-formals.stx arg-validation-spec*)
	  (%parse-predicate-formals predicate-formals.stx synner)
	`(,standard-formals.stx
	  ,(if (options::enable-arguments-validation?)
	       ;;With validation
	       (let* ((RETVAL*            (generate-temporaries ret-pred*.stx))
		      (RETVAL-VALIDATION* (%make-ret-validation-forms
					   who.id
					   (map (lambda (rv.id pred.stx)
						  (make-retval-validation-spec rv.id pred.stx
									       (%parse-logic-predicate-syntax pred.stx rv.id synner)))
					     RETVAL* ret-pred*.stx)
					   synner)))
		 `(begin
		    ,@(%make-arg-validation-forms who.id arg-validation-spec* synner)
		    (receive-and-return ,RETVAL*
			(internal-body . ,body*.stx)
		      . ,RETVAL-VALIDATION*)))
	     ;;Without validation.
	     `(begin . ,body*.stx)))))

    #| end of module: CASE-LAMBDA*-MACRO |# )

;;; --------------------------------------------------------------------

  (define (%parse-predicate-formals predicate-formals.stx synner)
    ;;Split  formals from  tags.   We  rely on  the  DEFINE,  LAMBDA and  CASE-LAMBDA
    ;;syntaxes in the  output form to further validate the  formals against duplicate
    ;;bindings.
    ;;
    ;;We use  the conventions: ?ID,  ?REST-ID and ?ARGS-ID are  argument identifiers;
    ;;?PRED is a predicate identifier.
    ;;
    ;;We accept the following standard formals formats:
    ;;
    ;;   ?args-id
    ;;   (?id ...)
    ;;   (?id0 ?id ... . ?rest-id)
    ;;
    ;;and in addition the following predicate formals:
    ;;
    ;;   (brace ?args-id ?args-pred)
    ;;   (?arg ...)
    ;;   (?arg0 ?arg ... . ?rest-arg)
    ;;
    ;;where ?ARG is a predicate argument with one of the formats:
    ;;
    ;;   ?id
    ;;   (brace ?id ?pred)
    ;;
    ;;Return 2 values:
    ;;
    ;;* A  list of syntax objects  representing the standard formals  for the DEFINE,
    ;;  LAMBDA and CASE-LAMBDA syntaxes.
    ;;
    ;;*  A  list  of  false booleans  and  ARGUMENT-VALIDATION-SPEC  structures  each
    ;;  representing a validation predicate; when an argument has no logic predicate:
    ;;  the corresponding item in the list is the boolean false.
    ;;
    (syntax-match predicate-formals.stx (brace)

      ;;Tagged args.
      ;;
      ((brace ?args-id ?args-pred)
       (identifier? ?args-id)
       (values ?args-id
	       (list (make-argument-validation-spec ?args-id ?args-pred
						    (%parse-list-logic-predicate-syntax ?args-pred ?args-id synner) #t))))

      ;;Possibly tagged identifiers with tagged rest argument.
      ;;
      ((?pred-arg* ... . (brace ?rest-id ?rest-pred))
       (begin
	 (unless (identifier? ?rest-id)
	   (synner "invalid rest argument specification" (list 'brace ?rest-id ?rest-pred)))
	 (let recur ((?pred-arg* ?pred-arg*))
	   (if (pair? ?pred-arg*)
	       ;;STANDARD-FORMALS.STX is an improper list of identifiers representing
	       ;;the   standard  formals.    ARG-VALIDATION-SPEC*   is   a  list   of
	       ;;ARGUMENT-VALIDATION-SPEC structures, each  representing a validation
	       ;;predicate.
	       (receive (standard-formals.stx arg-validation-spec*)
		   (recur (cdr ?pred-arg*))
		 (let ((?pred-arg (car ?pred-arg*)))
		   (syntax-match ?pred-arg (brace)
		     ;;Untagged argument.
		     (?id
		      (identifier? ?id)
		      (values (cons ?id standard-formals.stx)
			      (cons #f  arg-validation-spec*)))
		     ;;Tagged argument.
		     ((brace ?id ?pred)
		      (identifier? ?id)
		      (values (cons ?id standard-formals.stx)
			      (cons (make-argument-validation-spec ?id ?pred (%parse-logic-predicate-syntax ?pred ?id synner) #f)
				    arg-validation-spec*)))
		     (else
		      (synner "invalid argument specification" ?pred-arg)))))
	     ;;Process rest argument.
	     (values ?rest-id
		     (list (make-argument-validation-spec ?rest-id ?rest-pred
							  (%parse-list-logic-predicate-syntax ?rest-pred ?rest-id synner) #t)))))))

      ;;Possibly tagged identifiers with UNtagged rest argument.
      ;;
      ((?pred-arg* ... . ?rest-id)
       (identifier? ?rest-id)
       (let recur ((?pred-arg* ?pred-arg*))
	 (if (pair? ?pred-arg*)
	     ;;STANDARD-FORMALS.STX is  an improper list of  identifiers representing
	     ;;the   standard   formals.    ARG-VALIDATION-SPEC*   is   a   list   of
	     ;;ARGUMENT-VALIDATION-SPEC  structures, each  representing a  validation
	     ;;predicate.
	     (receive (standard-formals.stx arg-validation-spec*)
		 (recur (cdr ?pred-arg*))
	       (let ((?pred-arg (car ?pred-arg*)))
		 (syntax-match ?pred-arg (brace)
		   ;;Untagged argument.
		   (?id
		    (identifier? ?id)
		    (values (cons ?id standard-formals.stx)
			    (cons #f  arg-validation-spec*)))
		   ;;Tagged argument.
		   ((brace ?id ?pred)
		    (identifier? ?id)
		    (values (cons ?id standard-formals.stx)
			    (cons (make-argument-validation-spec ?id ?pred (%parse-logic-predicate-syntax ?pred ?id synner) #f)
				  arg-validation-spec*)))
		   (else
		    (synner "invalid argument specification" ?pred-arg)))))
	   (values ?rest-id '()))))

      ;;Standard formals: untagged identifiers without rest argument.
      ;;
      ((?id* ...)
       (for-all identifier? ?id*)
       (values ?id* '()))

      ;;Standard formals: untagged identifiers with rest argument.
      ;;
      ((?id* ... . ?rest-id)
       (and (for-all identifier? ?id*)
	    (identifier? ?rest-id))
       (values predicate-formals.stx '()))

      ;;Standard formals: untagged args.
      ;;
      (?args-id
       (identifier? ?args-id)
       (values ?args-id '()))

      ;;Possibly tagged identifiers without rest argument.
      ;;
      ((?pred-arg* ...)
       (let recur ((?pred-arg* ?pred-arg*))
	 (if (pair? ?pred-arg*)
	     ;;STANDARD-FORMALS.STX is  an improper list of  identifiers representing
	     ;;the   standard   formals.    ARG-VALIDATION-SPEC*   is   a   list   of
	     ;;ARGUMENT-VALIDATION-SPEC  structures, each  representing a  validation
	     ;;predicate.
	     (receive (standard-formals.stx arg-validation-spec*)
		 (recur (cdr ?pred-arg*))
	       (let ((?pred-arg (car ?pred-arg*)))
		 (syntax-match ?pred-arg (brace)
		   ;;Untagged argument.
		   (?id
		    (identifier? ?id)
		    (values (cons ?id standard-formals.stx)
			    (cons #f  arg-validation-spec*)))
		   ;;Tagged argument.
		   ((brace ?id ?pred)
		    (identifier? ?id)
		    (values (cons ?id standard-formals.stx)
			    (cons (make-argument-validation-spec ?id ?pred (%parse-logic-predicate-syntax ?pred ?id synner) #f)
				  arg-validation-spec*)))
		   (else
		    (synner "invalid argument specification" ?pred-arg)))))
	   (values '() '()))))
      ))

;;; --------------------------------------------------------------------

  (define (%make-arg-validation-forms who.id arg-validation-spec* synner)
    (reverse
     (cdr ($fold-left/stx
	      (lambda (knil spec)
		(let ((arg-counter    (car knil))
		      (rev-head-forms (cdr knil)))
		  (if spec
		      ;;This argument HAS a logic predicate specification.
		      (let ((?expr   (argument-validation-spec-expr   spec))
			    (?pred   (argument-validation-spec-pred   spec))
			    (?arg-id (argument-validation-spec-arg-id spec)))
			(cons* (fxadd1 arg-counter)
			       (if (argument-validation-spec-list-arg? spec)
				   (let ((loop.sym		(make-syntactic-identifier-for-temporary-variable))
					 (pred.sym		(make-syntactic-identifier-for-temporary-variable))
					 (arg-counter.sym	(make-syntactic-identifier-for-temporary-variable))
					 (arg*.sym		(make-syntactic-identifier-for-temporary-variable)))
				     `(let/std ,loop.sym ((,pred.sym		,?expr)
							  (,arg-counter.sym	,arg-counter)
							  (,arg*.sym		,?arg-id))
					(when (pair? ,arg*.sym)
					  (if (,pred.sym (car ,arg*.sym))
					      (,loop.sym ,pred.sym ($fxadd1 ,arg-counter.sym) (cdr ,arg*.sym))
					    (procedure-signature-argument-violation (quote ,who.id)
					      "failed argument validation"
					      ,arg-counter.sym (quote ,?pred) (car ,arg*.sym))))))
				 `(unless ,?expr
				    (procedure-signature-argument-violation (quote ,who.id)
				      "failed argument validation"
				      ,arg-counter (quote ,?pred) ,?arg-id)))
			       rev-head-forms))
		    ;;This argument HAS NO logic predicate specification.
		    (cons (fxadd1 arg-counter) rev-head-forms))))
	    '(1 . ())
	    arg-validation-spec*))))

  (define (%make-ret-validation-forms who.id retval-validation-spec* synner)
    (reverse
     (cdr ($fold-left/stx (lambda (knil spec)
			    (let ((retval-counter (car knil))
				  (rev-head-forms (cdr knil)))
			      (let ((?expr (retval-validation-spec-expr  spec))
				    (?pred (retval-validation-spec-pred  spec))
				    (?ret  (retval-validation-spec-rv-id spec)))
				(if (and (identifier? ?pred)
					 (free-identifier=? ?pred (core-prim-id 'always-true)))
				    ;;This return value HAS NO logic predicate specification.
				    (cons (fxadd1 retval-counter) rev-head-forms)
				  ;;This return value HAS a logic predicate specification.
				  (cons* (fxadd1 retval-counter)
					 `(unless ,?expr
					    (procedure-signature-return-value-violation (quote ,who.id)
					      "failed return value validation"
					      ,retval-counter (quote ,?pred) ,?ret))
					 rev-head-forms)))))
	    '(1 . ())
	    retval-validation-spec*))))

  (define (%parse-logic-predicate-syntax pred.stx var.id synner)
    ;;This is used for normal arguments.
    ;;
    (parse-logic-predicate-syntax pred.stx
				  (lambda (pred.id)
				    (syntax-match pred.id ()
				      (?pred
				       (identifier? ?pred)
				       (list pred.id var.id))
				      (else
				       (synner "expected identifier as predicate name" pred.id))))))

  (define (%parse-list-logic-predicate-syntax pred.stx var.id synner)
    ;;This is used for rest and args arguments.
    ;;
    `(lambda/std (,var.id)
       ,(parse-logic-predicate-syntax pred.stx
				      (lambda (pred.id)
					(syntax-match pred.id ()
					  (?pred
					   (identifier? ?pred)
					   (list pred.id var.id))
					  (else
					   (synner "expected identifier as predicate name" pred.id)))))))

  #| end of module |# )


;;;; non-core macro: GUARD
;;
;;Vicare's implementation of the GUARD syntax  is really sophisticated because it has
;;to  deal with  both the  dynamic environment  requirements of  R6RS and  the unwind
;;protection mechanism defined  by Vicare.  For a through explanation  we should read
;;the documentation  in Texinfo  format, both  the one of  GUARD and  the one  of the
;;unwind protection mechanism.
;;
;;
;;About the dynamic environment
;;-----------------------------
;;
;;In a syntax use like:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   ?expr2))
;;     ?body0 ?body ...)
;;
;;if the  ?BODY raises an  exception: one of the  clauses will certainly  be executed
;;because there is  an ELSE clause.  The ?BODY might  mutate the dynamic environment;
;;all the ?TEST and ?EXPR expressions must be evaluated in the dynamic environment of
;;the use of GUARD.
;;
;;In a syntax use like:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1))
;;     ?body0 ?body ...)
;;
;;if all  the ?TEST  expressions evaluate  to false: we  must re-raise  the exception
;;using RAISE-CONTINUABLE; so the syntax is "almost" equivalent to:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   (raise-continuable E)))
;;     ?body0 ?body ...)
;;
;;but:  ?BODY  might  mutate  the  dynamic  environment;  all  the  ?TEST  and  ?EXPR
;;expressions must be evaluated  in the dynamic environment of the  use of GUARD; the
;;RAISE-CONTINUABLE in the  ELSE clause must be evaluated the  dynamic environment of
;;the ?BODY.
;;
;;We must remember that, when using:
;;
;;   (with-exception-handler ?handler ?thunk)
;;
;;the ?HANDLER procedure is evaluated in the dynamic environment of the ?THUNK, minus
;;the exception  handler itself.  So, in  pseudo-code, a syntax use  with ELSE clause
;;must be expanded as follows:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   ?expr2))
;;     ?body0 ?body ...)
;;   ==> (save-guard-continuation
;;        (with-exception-handler
;;            (lambda (E)
;;              (reinstate-guard-continuation
;;               (cond (?test0 ?expr0)
;;                     (?test1 ?expr1)
;;                     (else   ?expr2))))
;;          (lambda () ?body0 ?body ...)))
;;
;;and, also  in pseudo-code,  a syntax use  without ELSE clause  must be  expanded as
;;follows:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1))
;;     ?body0 ?body ...)
;;   ==> (save-guard-continuation
;;        (with-exception-handler
;;            (lambda (E)
;;              (save-exception-handler-continuation
;;               (reinstate-guard-continuation
;;                (cond (?test0 ?expr0)
;;                      (?test1 ?expr1)
;;                      (else   (reinstate-exception-handler-continuation
;;                               (raise-continuable E)))))))
;;          (lambda () ?body0 ?body ...)))
;;
;;notice  how, in  the exception  handler, we  have to  jump out  and in  the dynamic
;;environment of the exception handler itself.
;;
;;
;;About the unwind-protection mechanism
;;-------------------------------------
;;
;;There is some serious shit going on here to support the unwind-protection mechanism
;;as  defined by  Vicare; let's  focus  on unwind-proteciton  in the  case of  raised
;;exception.  When using:
;;
;;   (with-unwind-protection ?cleanup ?thunk)
;;
;;the ?CLEANUP is  associated to the dynamic  extent of the call to  ?THUNK: when the
;;dynamic extent is terminated (as defined by Vicare) the ?CLEANUP is called.  If the
;;value  RUN-UNWIND-PROTECTION-CLEANUP-UPON-EXIT?   is set  to  true  and the  dynamic
;;extent of a call  to ?THUNK is exited: the dynamic  extent is considered terminated
;;and ?CLEANUP is called.
;;
;;Vicare defines as termination  event of a GUARD's ?body the  execution of a GUARD's
;;clause that does not re-raise the exception.  For a GUARD use like:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   ?expr2))
;;     ?body0 ?body ...)
;;
;;we can imagine the pseudo-code:
;;
;;   (guard (E (?test0 (run-unwind-protection-cleanups) ?expr0)
;;             (?test1 (run-unwind-protection-cleanups) ?expr1)
;;             (else   (run-unwind-protection-cleanups) ?expr2))
;;     ?body0 ?body ...)
;;
;;and for a GUARD use like:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1))
;;     ?body0 ?body ...)
;;
;;we can imagine the pseudo-code:
;;
;;   (guard (E (?test0 (run-unwind-protection-cleanups) ?expr0)
;;             (?test1 (run-unwind-protection-cleanups) ?expr1)
;;             (else   (raise-continuable E)))
;;     ?body0 ?body ...)
;;
;;By doing  things this  way: an  exception raised by  an ?EXPR  does not  impede the
;;execution of the cleanups.  If a ?TEST raises an exception the cleanups will not be
;;run, and there is  nothing we can do about it; ?TEST  expressions are usually calls
;;to predicates  that recognise  the condition  type of E,  so the  risk of  error is
;;reduced.
;;
;;So, in pseudo-code, a syntax use with ELSE clause must be expanded as follows:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1)
;;             (else   ?expr2))
;;     ?body0 ?body ...)
;;   ==> (save-guard-continuation
;;        (with-exception-handler
;;            (lambda (E)
;;              (reinstate-guard-continuation
;;               (cond (?test0 (run-unwind-protection-cleanups) ?expr0)
;;                     (?test1 (run-unwind-protection-cleanups) ?expr1)
;;                     (else   (run-unwind-protection-cleanups) ?expr2))))
;;          (lambda () ?body0 ?body ...)))
;;
;;and, also  in pseudo-code,  a syntax use  without ELSE clause  must be  expanded as
;;follows:
;;
;;   (guard (E (?test0 ?expr0)
;;             (?test1 ?expr1))
;;     ?body0 ?body ...)
;;   ==> (save-guard-continuation
;;        (with-exception-handler
;;            (lambda (E)
;;              (save-exception-handler-continuation
;;               (reinstate-guard-continuation
;;                (cond (?test0 (run-unwind-protection-cleanups) ?expr0)
;;                      (?test1 (run-unwind-protection-cleanups) ?expr1)
;;                      (else   (reinstate-exception-handler-continuation
;;                               (raise-continuable E)))))))
;;          (lambda () ?body0 ?body ...)))
;;
;;But how is RUN-UNWIND-PROTECTION-CLEANUPS implemented?  To cause the cleanups to be
;;called we must set to  true the value RUN-UNWIND-PROTECTION-CLEANUP-UPON-EXIT?, then
;;cause  an  exit  from  the  dynamic  extent  of  the  ?THUNKs.   The  latter  is  a
;;sophisticated operation implemented as follows:
;;
;;   (define (run-unwind-protection-cleanups)
;;     (run-unwind-protection-cleanup-upon-exit? #t)
;;     (save-clause-expression-continuation
;;      (reinstate-exception-handler-continuation
;;       (reinstate-clause-expression-continuation))))
;;
;;we jump in GUARD's exception handler  dynamic environment then immediately jump out
;;in the GUARD's clause expression dynamic environment.  Fucking weird...
;;
;;
;;Expansion example: GUARD with no ELSE clause
;;--------------------------------------------
;;
;;A syntax without else clause like looks like this:
;;
;;   (guard (E
;;           (?test0 ?expr0)
;;           (?test1 ?expr1)))
;;     ?body0 ?body ...)
;;
;;is expanded to:
;;
;;   ((call/cc
;;        (lambda (reinstate-guard-continuation)
;;          (lambda ()
;;            (with-exception-handler
;;                (lambda (raised-obj)
;;                  (let ((E raised-obj))
;;                    ((call/cc
;;                         (lambda (reinstate-exception-handler-continuation)
;;                           (reinstate-guard-continuation
;;                            (lambda ()
;;                              (define (run-unwind-protect-cleanups)
;;                                (run-unwind-protection-cleanup-upon-exit? 'exception)
;;                                (call/cc
;;                                    (lambda (reinstate-clause-expression-continuation)
;;                                      (reinstate-exception-handler-continuation
;;                                       (lambda ()
;;                                         (reinstate-clause-expression-continuation)))))
;;                                (run-unwind-protection-cleanup-upon-exit? #f))
;;                              (if ?test0
;;                                  (begin
;;                                    (run-unwind-protect-cleanups)
;;                                    ?expr0)
;;                                (if ?test1
;;                                    (begin
;;                                      (run-unwind-protect-cleanups)
;;                                      ?expr1)
;;                                  (reinstate-exception-handler-continuation
;;                                   (lambda ()
;;                                     (raise-continuable raised-obj))))))))))))
;;              (lambda ()
;;                ?body0 ?body ...))))))
;;
(module (guard-macro)

  (define-module-who guard)

  (define-macro-transformer (guard input-form.stx)
    ;;Transformer function used to expand R6RS  GUARD macros from the top-level built
    ;;in environment.  Expand  the contents of INPUT-FORM.STX; return a  syntax object that
    ;;must be further expanded.
    ;;
    (syntax-match input-form.stx ()
      ((_ (?variable ?clause* ...) ?body ?body* ...)
       (identifier? ?variable)
       (let ((reinstate-guard-continuation-id  (make-syntactic-identifier-for-temporary-variable "reinstate-guard-continuation-id"))
	     (raised-obj-id                    (make-syntactic-identifier-for-temporary-variable "raised-obj")))
	 (bless
	  `((call/cc
		(lambda/checked ({,reinstate-guard-continuation-id <procedure>})
		  ;;Here we use LAMBDA/CHECKED to propagate the type of the last body
		  ;;form.
		  (lambda/checked ()
		    (with-exception-handler
			(lambda/std (,raised-obj-id)
			  ;;If we  raise an exception from  a DYNAMIC-WIND's in-guard
			  ;;or out-guard while trying to  call the cleanups: we reset
			  ;;it to avoid leaving it true.
			  (run-unwind-protection-cleanup-upon-exit? #f)
			  (let/std ((,?variable ,raised-obj-id))
			    ,(gen-clauses raised-obj-id reinstate-guard-continuation-id ?clause*)))
		      ;;Here we use LAMBDA/CHECKED to  propagate the type of the last
		      ;;body form.
		      (lambda/checked ()
			,?body . ,?body*)))))))))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (module (gen-clauses)

    (define (gen-clauses raised-obj-id reinstate-guard-continuation-id clause*)
      (define run-unwind-protect-cleanups-id               (make-syntactic-identifier-for-temporary-variable "run-unwind-protect-cleanups"))
      (define reinstate-clause-expression-continuation-id  (make-syntactic-identifier-for-temporary-variable "reinstate-clause-expression-continuation"))
      (receive (code-stx reinstate-exception-handler-continuation-id)
	  (%process-multi-cond-clauses raised-obj-id clause* run-unwind-protect-cleanups-id)
	`((call/cc
	      (lambda/std (,reinstate-exception-handler-continuation-id)
		(,reinstate-guard-continuation-id
		 (lambda/std ()
		   (define (,run-unwind-protect-cleanups-id)
		     ;;If we are  here: a test in the clauses  returned non-false and
		     ;;the execution  flow is at  the beginning of  the corresponding
		     ;;clause expression.
		     ;;
		     ;;Reinstate the  continuation of  the guard's  exception handler
		     ;;and then immediately reinstate this continuation.  This causes
		     ;;the  dynamic  environment  of  the  exception  handler  to  be
		     ;;reinstated, and the unwind-protection cleanups are called.
		     ;;
		     ;;Yes,  we  must   really  set  the  parameter   to  the  symbol
		     ;;"exception"; this  symbol is used  as argument for  the unwind
		     ;;handlers.
		     (run-unwind-protection-cleanup-upon-exit? 'exception)
		     (call/cc
			 (lambda/std (,reinstate-clause-expression-continuation-id)
			   (,reinstate-exception-handler-continuation-id
			    (lambda/std ()
			      (,reinstate-clause-expression-continuation-id)))))
		     (run-unwind-protection-cleanup-upon-exit? #f))
		   ,code-stx)))))))

    (define (%process-multi-cond-clauses raised-obj-id clause* run-unwind-protect-cleanups-id)
      (syntax-match clause* (else)
	;;There is  no ELSE clause: insert  code that reinstates the  continuation of
	;;the exception handler introduced by GUARD and re-raises the exception.
	(()
	 (let ((reinstate-exception-handler-continuation-id (make-syntactic-identifier-for-temporary-variable "reinstate-exception-handler-continuation")))
	   (values `(,reinstate-exception-handler-continuation-id
		     (lambda/std ()
		       (raise-continuable ,raised-obj-id)))
		   reinstate-exception-handler-continuation-id)))

	;;There is  an ELSE  clause: no need  to jump back  to the  exception handler
	;;introduced by GUARD.
	(((else ?else-body ?else-body* ...))
	 (let ((reinstate-exception-handler-continuation-id (make-syntactic-identifier-for-temporary-variable "reinstate-exception-handler-continuation")))
	   (values `(begin
		      (,run-unwind-protect-cleanups-id)
		      ,?else-body . ,?else-body*)
		   reinstate-exception-handler-continuation-id)))

	((?clause . ?clause*)
	 (receive (code-stx reinstate-exception-handler-continuation-id)
	     (%process-multi-cond-clauses raised-obj-id ?clause* run-unwind-protect-cleanups-id)
	   (values (%process-single-cond-clause ?clause code-stx run-unwind-protect-cleanups-id)
		   reinstate-exception-handler-continuation-id)))

	(others
	 (syntax-violation __module_who__ "invalid guard clause" others))))

    (define (%process-single-cond-clause clause kont-code-stx run-unwind-protect-cleanups-id)
      (syntax-match clause (=>)
	((?test => ?proc)
	 (let ((t (make-syntactic-identifier-for-temporary-variable)))
	   `(let/std ((,t ,?test))
	      (if ,t
		  (begin
		    (,run-unwind-protect-cleanups-id)
		    (,?proc ,t))
		,kont-code-stx))))

	((?test)
	 (let ((t (make-syntactic-identifier-for-temporary-variable)))
	   `(let/std ((,t ,?test))
	      (if ,t
		  (begin
		    (,run-unwind-protect-cleanups-id)
		    ,t)
		,kont-code-stx))))

	((?test ?expr ?expr* ...)
	 `(if ,?test
	      (begin
		(,run-unwind-protect-cleanups-id)
		,?expr . ,?expr*)
	    ,kont-code-stx))

	(_
	 (syntax-violation __module_who__ "invalid guard clause" clause))))

    #| end of module: GEN-CLAUSES |# )

  #| end of module: GUARD-MACRO |# )

;;; --------------------------------------------------------------------

;;NOTE The  one below is  the old  GUARD implementation.  It  worked fine but  had no
;;integration  with  the  unwind-protection  mechanism.   I am  keeping  it  here  as
;;reference.  Sue me.  (Marco Maggi; Mon Feb 2, 2015)
;;
(commented-out
 (module (guard-macro)

   (define-macro-transformer (guard x)
     ;;Transformer function used to expand R6RS GUARD macros from the top-level built
     ;;in environment.  Expand the contents of INPUT-FORM.STX; return a syntax object
     ;;that must be further expanded.
     ;;
     ;;NOTE If we need to reraise the continuation because no GUARD clause handles it
     ;;(and  there is  no  ELSE clause):  we  must  reraise it  in  the same  dynamic
     ;;environment  of the  ?BODY  minus  the exception  handler  installed by  GUARD
     ;;itself.  So,  to reraise the exception,  GUARD must jump back  in reevaluating
     ;;the in-guards of the DYNAMIC-WINDs.
     ;;
     ;;A syntax without else clause like:
     ;;
     ;;   (guard (E
     ;;           (?test0 ?expr0)
     ;;           (?test1 ?expr1)))
     ;;     ?body0 ?body ...)
     ;;
     ;;is expanded to:
     ;;
     ;;   ((call/cc
     ;;        (lambda (outerk)
     ;;          (lambda ()
     ;;            (with-exception-handler
     ;;                (lambda (raised-obj)
     ;;                  (let ((E raised-obj))
     ;;                    ((call/cc
     ;;                         (lambda (return-to-exception-handler-k)
     ;;                           (outerk (lambda ()
     ;;                                     (if ?test0
     ;;                                         ?expr0
     ;;                                       (if ?test1
     ;;                                           ?expr1
     ;;                                         (return-to-exception-handler-k
     ;;                                           (lambda ()
     ;;                                             (raise-continuable raised-obj))))))))))))
     ;;              (lambda ()
     ;;                ?body0 ?body ...))))))
     ;;
     (syntax-match x ()
       ((_ (?variable ?clause* ...) ?body ?body* ...)
	(identifier? ?variable)
	(let ((outerk-id     (make-syntactic-identifier-for-temporary-variable))
	      (raised-obj-id (make-syntactic-identifier-for-temporary-variable)))
	  (bless
	   `((call/cc
		 (lambda/std (,outerk-id)
		   (lambda/std ()
		     (with-exception-handler
			 (lambda/std (,raised-obj-id)
			   (let/std ((,?variable ,raised-obj-id))
			     ,(gen-clauses raised-obj-id outerk-id ?clause*)))
		       (lambda/std ()
			 ,?body . ,?body*))))))
	   )))
       ))

   (define (gen-clauses raised-obj-id outerk-id clause*)

     (define (%process-single-cond-clause clause kont-code-stx)
       (syntax-match clause (=>)
	 ((?test => ?proc)
	  (let ((t (make-syntactic-identifier-for-temporary-variable)))
	    `(let/std ((,t ,?test))
	       (if ,t
		   (,?proc ,t)
		 ,kont-code-stx))))

	 ((?test)
	  (let ((t (make-syntactic-identifier-for-temporary-variable)))
	    `(let/std ((,t ,?test))
	       (if ,t ,t ,kont-code-stx))))

	 ((?test ?expr ?expr* ...)
	  `(if ,?test
	       (begin ,?expr . ,?expr*)
	     ,kont-code-stx))

	 (_
	  (syntax-violation __module_who__ "invalid guard clause" clause))))

     (define (%process-multi-cond-clauses clause*)
       (syntax-match clause* (else)
	 ;;There is no ELSE clause: introduce the raise continuation that
	 ;;rethrows the exception.
	 (()
	  (let ((return-to-exception-handler-k (make-syntactic-identifier-for-temporary-variable)))
	    (values `(,return-to-exception-handler-k
		      (lambda/std ()
			(raise-continuable ,raised-obj-id)))
		    return-to-exception-handler-k)))

	 ;;There  is an  ELSE  clause:  no need  to  introduce the  raise
	 ;;continuation.
	 (((else ?else-body ?else-body* ...))
	  (values `(begin ,?else-body . ,?else-body*)
		  #f))

	 ((?clause . ?clause*)
	  (receive (code-stx return-to-exception-handler-k)
	      (%process-multi-cond-clauses ?clause*)
	    (values (%process-single-cond-clause ?clause code-stx)
		    return-to-exception-handler-k)))

	 (others
	  (syntax-violation __module_who__ "invalid guard clause" others))))

     (receive (code-stx return-to-exception-handler-k)
	 (%process-multi-cond-clauses clause*)
       (if return-to-exception-handler-k
	   `((call/cc
		 (lambda/std (,return-to-exception-handler-k)
		   (,outerk-id (lambda/std () ,code-stx)))))
	 `(,outerk-id (lambda/std () ,code-stx)))))

   #| end of module: GUARD-MACRO |# ))


;;;; non-core macro: DEFINE-ENUMERATION

(define-macro-transformer (define-enumeration input-form.stx)
  ;;Transformer  function used  to  expand R6RS  DEFINE-ENUMERATION  macros from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?name (?id* ...) ?maker)
     (begin
       (unless (identifier? ?name)
	 (__synner__ "expected identifier as enumeration type name" ?name))
       (unless (for-all identifier? ?id*)
	 (__synner__ "expected list of symbols as enumeration elements" ?id*))
       (unless (identifier? ?maker)
	 (__synner__ "expected identifier as enumeration constructor syntax name" ?maker))
       (let ((symbol*		(list-of-symbols.delete-duplicates (syntax->datum ?id*)))
	     (the-constructor	(make-syntactic-identifier-for-temporary-variable
				 (string->symbol (string-append "make-" (symbol->string (syntax->datum ?name)))))))
	 (bless
	  `(begin
	     (define/typed {,the-constructor (lambda ((list-of <symbol>)) => (<enum-set>))}
	       (enum-set-constructor (make-enumeration (quote ,symbol*))))
	     (define-type ,?name
	       (enumeration . ,symbol*))
	     (define-syntax ,?maker
	       ;;Given any finite  sequence of the symbols in  the universe, possibly
	       ;;with duplicates,  expands into an  expression that evaluates  to the
	       ;;enumeration set of those symbols.
	       ;;
	       ;;Check at macro-expansion  time whether every input symbol  is in the
	       ;;universe associated with  ?NAME; it is a syntax violation  if one or
	       ;;more is not.
	       ;;
	       (lambda/std (x)
		 (define universe-of-symbols (quote ,symbol*))
		 (define (%synner message subform-stx)
		   (syntax-violation ',?maker
		     message
		     (syntax->datum x) (syntax->datum subform-stx)))
		 (syntax-case x ()
		   ((_ . ??list-of-symbols)
		    ;;Check the input symbols one by one partitioning the ones in the
		    ;;universe from the one not in the universe.
		    ;;
		    ;;If an input element is not a symbol: raise a syntax violation.
		    ;;
		    ;;After all the input symbols  have been partitioned, if the list
		    ;;of collected INvalid ones is not null: raise a syntax violation
		    ;;with that list as subform, else return syntax object expression
		    ;;building  a  new enumeration  set  holding  the list  of  valid
		    ;;symbols.
		    ;;
		    (let loop ((valid-symbols-stx	'())
			       (invalid-symbols-stx	'())
			       (input-symbols-stx	(syntax ??list-of-symbols)))
		      (syntax-case input-symbols-stx ()

			;;No more symbols  to collect and non-null  list of collected
			;;INvalid symbols.
			(()
			 (not (null? invalid-symbols-stx))
			 (%synner "expected symbols in enumeration as arguments \
                                     to enumeration constructor syntax"
				  (reverse invalid-symbols-stx)))

			;;No  more symbols  to  collect and  null  list of  collected
			;;INvalid symbols.
			(()
			 (quasisyntax
			  (,the-constructor (quote (unsyntax (reverse valid-symbols-stx))))))

			;;Error if element is not a symbol.
			((??symbol0 . ??rest)
			 (not (identifier? (syntax ??symbol0)))
			 (%synner "expected symbols as arguments to enumeration constructor syntax"
				  (syntax ??symbol0)))

			;;Collect a symbol in the set.
			((??symbol0 . ??rest)
			 (memq (syntax->datum (syntax ??symbol0)) universe-of-symbols)
			 (loop (cons (syntax ??symbol0) valid-symbols-stx)
			       invalid-symbols-stx (syntax ??rest)))

			;;Collect a symbol not in the set.
			((??symbol0 . ??rest)
			 (loop valid-symbols-stx
			       (cons (syntax ??symbol0) invalid-symbols-stx)
			       (syntax ??rest)))

			))))))
	     )))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: DO, DO*, DOLIST, DOTIMES, WHILE, UNTIL, FOR

(module (do-macro do*-macro dolist-macro dotimes-macro while-macro until-macro for-macro)

(define (with-escape-fluids escape next-iteration body*)
  ;;NOTE We  define BREAK  as accepting  any number of  arguments and  returning zero
  ;;values  when given  zero arguments.   Returning  zero values  can be  meaningful,
  ;;example:
  ;;
  ;;   (receive args
  ;;       (values)
  ;;     args)
  ;;   => ()
  ;;
  ;;so we  do not want  force "(break)"  to return a  single value like  #<void> just
  ;;because it  is faster to  return one value rather  than return 0  values.  (Marco
  ;;Maggi; Sat Jan 31, 2015)
  ;;
  `(fluid-let-syntax
       ((break    (syntax-rules ()
		    ((_ . ?args)
		     (,escape . ?args))
		    ))
	(continue (syntax-rules ()
		    ((_)
		     (,next-iteration #t)))))
     . ,body*))

;;; --------------------------------------------------------------------

(define-macro-transformer (do input-form.stx)
  ;;Transformer function  used to expand R6RS  DO macros from the  top-level built in
  ;;environment;  we also  support extended  Vicare syntax.   Expand the  contents of
  ;;INPUT-FORM.STX; return a syntax object that must be further expanded.
  ;;
  (define (%normalise-binding binding-stx)
    (syntax-match binding-stx ()
      ((?var ?init)
       (receive (var.id var.ots)
	   (syntax-object.parse-typed-argument ?var)
	 `(,?var ,?init ,var.id)))
      ((?var ?init ?step)
       `(,?var ,?init ,?step))
      (_
       (__synner__ "invalid binding" input-form.stx))))
  (syntax-match input-form.stx (while until)
    ;;This is an extended Vicare syntax.
    ;;
    ;;NOTE We want an implementation in which:  when BREAK and CONTINUE are not used,
    ;;the escape functions are never referenced, so the compiler can remove CALL/CC.
    ;;
    ;;NOTE Using CONTINUE in the body causes a jump to the test.
    ((_ ?body (while ?test))
     (let ((escape         (make-syntactic-identifier-for-temporary-variable "escape"))
	   (next-iteration (make-syntactic-identifier-for-temporary-variable "next-iteration"))
	   (loop           (make-syntactic-identifier-for-temporary-variable "loop")))
       (bless
	`(unwinding-call/cc
	     (lambda/std (,escape)
	       (let/std ,loop ()
			(unwinding-call/cc
			    (lambda/std (,next-iteration)
			      ,(with-escape-fluids escape next-iteration (list ?body))))
			(when ,?test
			  (,loop))))))))

    ;;This is an extended Vicare syntax.
    ;;
    ;;NOTE We want an implementation in which:  when BREAK and CONTINUE are not used,
    ;;the escape functions are never referenced, so the compiler can remove CALL/CC.
    ;;
    ;;NOTE Using CONTINUE in the body causes a jump to the test.
    ((_ ?body (until ?test))
     (let ((escape         (make-syntactic-identifier-for-temporary-variable "escape"))
	   (next-iteration (make-syntactic-identifier-for-temporary-variable "next-iteration"))
	   (loop           (make-syntactic-identifier-for-temporary-variable "loop")))
       (bless
	`(unwinding-call/cc
	     (lambda/std (,escape)
	       (let/std ,loop ()
			(unwinding-call/cc
			    (lambda/std (,next-iteration)
			      ,(with-escape-fluids escape next-iteration (list ?body))))
			(until ,?test
			  (,loop))))))))

    ;;This is the R6RS syntax.
    ;;
    ;;NOTE We want an implementation in which:  when BREAK and CONTINUE are not used,
    ;;the escape functions are never referenced, so the compiler can remove CALL/CC.
    ((_ (?binding* ...)
	(?test ?expr* ...)
	?command* ...)
     (syntax-match (map %normalise-binding ?binding*) ()
       (((?var* ?init* ?step*) ...)
	(let ((escape         (make-syntactic-identifier-for-temporary-variable "escape"))
	      (next-iteration (make-syntactic-identifier-for-temporary-variable "next-iteration"))
	      (loop           (make-syntactic-identifier-for-temporary-variable "loop")))
	  (bless
	   `(unwinding-call/cc
		(lambda/std (,escape)
		  (letrec/std ((,loop (lambda ,?var*
					(if (unwinding-call/cc
						(lambda/std (,next-iteration)
						  (if ,?test
						      #f
						    ,(with-escape-fluids escape next-iteration `(,@?command* #t)))))
					    (,loop . ,?step*)
					  ,(if (null? ?expr*)
					       '(void)
					     `(begin . ,?expr*))))))
		    (,loop . ,?init*)))))))
       ))

    (_
     (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

(define-macro-transformer (do* input-form.stx)
  ;;Transformer function used to expand Vicare DO* macros from the top-level built in
  ;;environment;  we also  support extended  Vicare syntax.   Expand the  contents of
  ;;INPUT-FORM.STX; return a syntax object that must be further expanded.
  ;;
  ;;This is meant to be similar to the Common Lisp syntax of the same name.
  ;;
  ;;NOTE We want  an implementation in which:  when BREAK and CONTINUE  are not used,
  ;;the escape functions are never referenced, so the compiler can remove CALL/CC.
  ;;
  (define (%make-init-binding binding-stx)
    (syntax-match binding-stx ()
      ((?var ?init)
       (receive (var.id var.ots)
	   (syntax-object.parse-typed-argument ?var)
	 binding-stx))
      ((?var ?init ?step)
       (receive (var.id var.ots)
	   (syntax-object.parse-typed-argument ?var)
	 (list ?var ?init)))
      (_
       (__synner__ "invalid binding" binding-stx))))
  (define (%make-step-update binding-stx knil)
    (syntax-match binding-stx ()
      ((?var ?init)
       knil)
      ((?var ?init ?step)
       (receive (var.id var.ots)
	   (syntax-object.parse-typed-argument ?var)
	 (cons `(set! ,var.id ,?step)
	       knil)))
      (_
       (__synner__ "invalid binding" binding-stx))))
  (syntax-match input-form.stx ()
    ((_ (?binding* ...)
	(?test ?expr* ...)
	?command* ...)
     (let* ((escape         (make-syntactic-identifier-for-temporary-variable "escape"))
	    (next-iteration (make-syntactic-identifier-for-temporary-variable "next-iteration"))
	    (init-binding*  (map %make-init-binding ?binding*))
	    (step-update*   (fold-right %make-step-update '() ?binding*))
	    (loop           (make-syntactic-identifier-for-temporary-variable "loop")))
       (bless
	`(unwinding-call/cc
	     (lambda/std (,escape)
	       (let* ,init-binding*
		 (letrec/std ((,loop (lambda/std ()
				       (if (unwinding-call/cc
					       (lambda/std (,next-iteration)
						 (if ,?test
						     #f
						   ,(with-escape-fluids escape next-iteration `(,@?command* #t)))))
					   (begin
					     ,@step-update*
					     (,loop))
					 ,(if (null? ?expr*)
					      '(void)
					    `(begin . ,?expr*))))))
		   (,loop))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

(define-macro-transformer (dolist input-form.stx)
  ;;Transformer function used to expand Vicare DOLIST macros from the top-level built
  ;;in environment; we  also support extended Vicare syntax.  Expand  the contents of
  ;;INPUT-FORM.STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?var ?list-form)              ?body0 ?body* ...)
     (bless
      `(dolist (,?var ,?list-form (void))
	 ,?body0 . ,?body*)))
    ((_ (?var ?list-form ?result-form) ?body0 ?body* ...)
     (let ((ell  (make-syntactic-identifier-for-temporary-variable "ell"))
	   (loop (make-syntactic-identifier-for-temporary-variable "loop")))
       (bless
	`(let/std ,loop ((,ell ,?list-form))
		  (if (pair? ,ell)
		      (let ((,?var (car ,ell)))
			,?body0 ,@?body*
			(,loop (cdr ,ell)))
		    (let ((,?var '()))
		      ,?result-form))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

(define-macro-transformer (dotimes input-form.stx)
  ;;Transformer  function used  to expand  Vicare DOTIMES  macros from  the top-level
  ;;built  in  environment; we  also  support  extended  Vicare syntax.   Expand  the
  ;;contents of INPUT-FORM.STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?var ?count-form)              ?body0 ?body* ...)
     (let ((max-var (make-syntactic-identifier-for-temporary-variable)))
       (bless
	`(let/checked (({,max-var <top>} ,?count-form))
	   (do ((,?var 0 (add1 ,?var)))
	       ((>= ,?var ,max-var))
	     ,?body0 . ,?body*)))))
    ((_ (?var ?count-form ?result-form) ?body0 ?body* ...)
     (let ((max-var (make-syntactic-identifier-for-temporary-variable)))
       (bless
	`(let/checked (({,max-var <top>} ,?count-form))
	   (do ((,?var 0 (add1 ,?var)))
	       ((>= ,?var ,max-var)
		,?result-form)
	     ,?body0 . ,?body*)))))
    (_
     (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

(define-macro-transformer (while input-form.stx)
  ;;Transformer  function used  to expand  Vicare's WHILE  macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  ;;NOTE We want  an implementation in which:  when BREAK and CONTINUE  are not used,
  ;;the escape functions are never referenced, so the compiler can remove CALL/CC.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?test ?body* ...)
     (let ((escape         (make-syntactic-identifier-for-temporary-variable "escape"))
	   (next-iteration (make-syntactic-identifier-for-temporary-variable "next-iteration"))
	   (loop           (make-syntactic-identifier-for-temporary-variable "loop")))
       (bless
	`(unwinding-call/cc
	     (lambda/std (,escape)
	       (let/std ,loop ()
		 (when (unwinding-call/cc
			   (lambda/std (,next-iteration)
			     (if ,?test
				 ,(with-escape-fluids escape next-iteration `(,@?body* #t))
			       #f)))
		   (,loop))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (until input-form.stx)
  ;;Transformer  function used  to expand  Vicare's UNTIL  macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  ;;NOTE We want  an implementation in which:  when BREAK and CONTINUE  are not used,
  ;;the escape functions are never referenced, so the compiler can remove CALL/CC.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?test ?body* ...)
     (let ((escape         (make-syntactic-identifier-for-temporary-variable "escape"))
	   (next-iteration (make-syntactic-identifier-for-temporary-variable "next-iteration"))
	   (loop           (make-syntactic-identifier-for-temporary-variable "loop")))
       (bless
	`(unwinding-call/cc
	     (lambda/std (,escape)
	       (let/std ,loop ()
		 (when (unwinding-call/cc
			   (lambda/std (,next-iteration)
			     (if ,?test
				 #f
			       ,(with-escape-fluids escape next-iteration `(,@?body* #t)))))
		   (,loop))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (for input-form.stx)
  ;;Transformer function used to expand Vicare's  FOR macros from the top-level built
  ;;in environment.   Expand the contents  of INPUT-FORM.STX; return a  syntax object
  ;;that must be further expanded.
  ;;
  ;;NOTE We want  an implementation in which:  when BREAK and CONTINUE  are not used,
  ;;the escape functions are never referenced, so the compiler can remove CALL/CC.
  ;;
  ;;NOTE The CONTINUE must skip the rest of the body and jump to the increment.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?init ?test ?incr) ?body* ...)
     (let ((escape         (make-syntactic-identifier-for-temporary-variable "escape"))
	   (next-iteration (make-syntactic-identifier-for-temporary-variable "next-iteration"))
	   (loop           (make-syntactic-identifier-for-temporary-variable "loop")))
       (bless
	`(unwinding-call/cc
	     (lambda/std (,escape)
	       ,?init
	       (let/std ,loop ()
		 (when (unwinding-call/cc
			   (lambda/std (,next-iteration)
			     (if ,?test
				 ,(with-escape-fluids escape next-iteration `(,@?body* #t))
			       #f)))
		   ,?incr
		   (,loop))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

#| end of module |# )


;;;; non-core macro: RETURNABLE

(define-macro-transformer (returnable input-form.stx)
  ;;Transformer function used to expand Vicare's RETURNABLE macros from the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?body0 ?body* ...)
     (let ((escape (make-syntactic-identifier-for-temporary-variable "escape")))
       (bless
	`(unwinding-call/cc
	     (lambda/std (,escape)
	       (fluid-let-syntax ((return (syntax-rules ()
					    ((_ . ?args)
					     (,escape . ?args)))))
		 ,?body0 . ,?body*))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: TRY

(module (try-macro)
  (define-module-who try)

  (define-macro-transformer (try input-form.stx)
    ;;Transformer function used to expand Vicare's TRY ...  CATCH ...  FINALLY macros
    ;;from   the  top-level   built   in  environment.    Expand   the  contents   of
    ;;INPUT-FORM.STX; return a syntax object that must be further expanded.
    ;;
    (syntax-match input-form.stx (catch finally)
      ;;Full syntax.
      ((_ ?body (catch ?var ?catch-clause0 ?catch-clause* ...) (finally ?finally-body0 ?finally-body* ...))
       (begin
	 (validate-variable ?var __synner__)
	 (let ((GUARD-CLAUSE* (parse-multiple-catch-clauses ?var (cons ?catch-clause0 ?catch-clause*) __synner__))
	       (why           (make-syntactic-identifier-for-temporary-variable)))
	   (bless
	    `(with-unwind-protection
		 (lambda/std (,why)
		   ,?finally-body0 . ,?finally-body*)
	       (lambda/checked ()
		 (guard (,?var . ,GUARD-CLAUSE*)
		   ,?body)))))))

      ;;Only catch, no finally.
      ((_ ?body (catch ?var ?catch-clause0 ?catch-clause* ...))
       (begin
	 (validate-variable ?var __synner__)
	 (let ((GUARD-CLAUSE* (parse-multiple-catch-clauses ?var (cons ?catch-clause0 ?catch-clause*) __synner__)))
	   (bless
	    `(guard (,?var . ,GUARD-CLAUSE*) ,?body)))))

      ((_ ?body (finally ?finally-body0 ?finally-body* ...))
       (let ((why (make-syntactic-identifier-for-temporary-variable)))
	 (bless
	  `(with-unwind-protection
	       (lambda/std (,why)
		 ,?finally-body0 . ,?finally-body*)
	     (lambda/checked ()
	       ,?body)))))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define (parse-multiple-catch-clauses var-id clauses-stx synner)
    (syntax-match clauses-stx (else)
      ;;Match when  there is no  ELSE clause.  Remember that  GUARD will
      ;;reraise the exception when there is no ELSE clause.
      (()
       '())

      ;;This branch  with the ELSE  clause must come first!!!   The ELSE
      ;;clause is valid only if it is the last.
      (((else ?else-body0 ?else-body ...))
       clauses-stx)

      (((?pred ?tag-body0 ?tag-body* ...) . ?other-clauses)
       (cons (cons* (syntax-match ?pred ()
		      ((?tag)
		       (identifier? ?tag)
		       `(is-a? ,var-id ,?tag))
		      (_
		       (parse-logic-predicate-syntax ?pred
						     (lambda (tag-id)
						       (syntax-match tag-id ()
							 (?tag
							  (identifier? ?tag)
							  `(is-a? ,var-id ,?tag))
							 (else
							  (synner "expected identifier as condition type" tag-id)))))))
		    ?tag-body0 ?tag-body*)
	     (parse-multiple-catch-clauses var-id ?other-clauses synner)))

      ((?clause . ?other-clauses)
       (synner "invalid catch clause in try syntax" ?clause))))

  (define (validate-variable var-id synner)
    (unless (identifier? var-id)
      (synner "expected identifier as variable" var-id)))

  #| end of module |# )


;;;; non-core macro: WITH-BLOCKED-EXCEPTIONS, WITH-CURRENT-DYNAMIC-ENVIRONMENT

(define-macro-transformer (with-blocked-exceptions input-form.stx)
  ;;Transformer function used to  expand Vicare's WITH-BLOCKED-EXCEPTIONS macros from
  ;;the  top-level built  in  environment.  Expand  the  contents of  INPUT-FORM.STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?exception-retvals-maker ?thunk)
     (bless
      `(call/cc
	   (lambda/typed ({reinstate-with-blocked-exceptions-continuation <procedure>})
	     (with-exception-handler
		 (lambda/std (E)
		   (call-with-values
		       (lambda/std ()
			 (,?exception-retvals-maker E))
		     reinstate-with-blocked-exceptions-continuation))
	       ,?thunk)))))
    ((_ ?thunk)
     (bless
      `(call/cc
	   (lambda/typed ({reinstate-with-blocked-exceptions-continuation <procedure>})
	     (with-exception-handler
		 reinstate-with-blocked-exceptions-continuation
	       ,?thunk)))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (with-current-dynamic-environment input-form.stx)
  ;;Transformer  function used  to  expand Vicare's  WITH-CURRENT-DYNAMIC-ENVIRONMENT
  ;;macros  from  the  top-level  built  in  environment.   Expand  the  contents  of
  ;;INPUT-FORM.STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?exception-retvals-maker ?thunk)
     (bless
      `(call/cc
	   (lambda/typed ({return-thunk-with-packed-environment <procedure>})
	     ((call/cc
		  (lambda/typed ({reinstate-target-environment-continuation <procedure>})
		    (return-thunk-with-packed-environment
		     (lambda/std ()
		       (call/cc
			   (lambda/typed ({reinstate-thunk-call-continuation <procedure>})
			     (reinstate-target-environment-continuation
			      (lambda/std ()
				(call-with-values
				    (lambda/std ()
				      (with-blocked-exceptions
					  ,?exception-retvals-maker
					,?thunk))
				  reinstate-thunk-call-continuation))))))))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: SHIFT, RESET

(define-macro-transformer (reset input-form.stx)
  ;;Transformer  function used  to expand  Vicare's RESET  macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?body)
     (let ((mc.sym      (make-syntactic-identifier-for-temporary-variable "meta-continuation"))
	   (escape.sym  (make-syntactic-identifier-for-temporary-variable "escape"))
	   (value.sym   (make-syntactic-identifier-for-temporary-variable "value"))
	   (result.sym  (make-syntactic-identifier-for-temporary-variable "result")))
       (bless
	`(let/std ((,mc.sym (private-shift-meta-continuation)))
	   (call-with-current-continuation
	       (lambda (,escape.sym)
		 (parametrise ((private-shift-meta-continuation (lambda (,value.sym)
								  (private-shift-meta-continuation ,mc.sym)
								  (,escape.sym ,value.sym))))
		   (let/std ((,result.sym ,?body))
		     ((private-shift-meta-continuation) ,result.sym)))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (shift input-form.stx)
  ;;Transformer  function used  to expand  Vicare's SHIFT  macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?var ?body)
     (identifier? ?var)
     (let ((escape.sym  (make-syntactic-identifier-for-temporary-variable "escape"))
	   (value.sym   (make-syntactic-identifier-for-temporary-variable "value"))
	   (result.sym  (make-syntactic-identifier-for-temporary-variable "result")))
       (bless
	`(call-with-current-continuation
	     (lambda (,escape.sym)
	       (let/std ((,result.sym (let/std ((,?var (lambda (,value.sym)
							 (inner-reset (,escape.sym ,value.sym)))))
					,?body)))
		 ((private-shift-meta-continuation) ,result.sym)))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (inner-reset input-form.stx)
  ;;Transformer  function  used  to  expand  Vicare's  INNER-RESET  macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  ;;RESET and  INNER-RESET are almost equal;  the differenc is that  INNER-RESET does
  ;;not use PARAMETRISE to set PRIVATE-SHIFT-META-CONTINUATION.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?body)
     (let ((mc.sym      (make-syntactic-identifier-for-temporary-variable "meta-continuation"))
	   (escape.sym  (make-syntactic-identifier-for-temporary-variable "escape"))
	   (value.sym   (make-syntactic-identifier-for-temporary-variable "value"))
	   (result.sym  (make-syntactic-identifier-for-temporary-variable "result")))
       (bless
	`(let/std ((,mc.sym (private-shift-meta-continuation)))
	   (call-with-current-continuation
	       (lambda (,escape.sym)
		 (private-shift-meta-continuation (lambda (,value.sym)
						    (private-shift-meta-continuation ,mc.sym)
						    (,escape.sym ,value.sym)))
		 (let/std ((,result.sym ,?body))
		   ((private-shift-meta-continuation) ,result.sym))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: COND

(define-macro-transformer (cond input-form.stx)
  ;;Transformer function used to expand R6RS  COND macros from the top-level built in
  ;;environment.  Expand the contents of  INPUT-FORM.STX; return a syntax object that
  ;;must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?cls ?cls* ...)
     (bless
      (let recur ((cls ?cls) (cls* ?cls*))
	(if (null? cls*)
	    ;;Here we process the last clause.
	    (syntax-match cls (else =>)
	      ((else ?expr ?expr* ...)
	       `(internal-body ,?expr . ,?expr*))

	      ((?test => ?proc)
	       (let ((tmp (make-syntactic-identifier-for-temporary-variable "tmp")))
		 `(let/std ((,tmp ,?test))
		    (when ,tmp
		      (,?proc ,tmp)))))

	      ((?expr)
	       (let ((tmp (make-syntactic-identifier-for-temporary-variable "tmp")))
		 `(let/std ((,tmp ,?expr))
		    (when ,tmp ,tmp))))

	      ((?test ?expr* ...)
	       `(when ,?test
		  (internal-body . ,?expr*)))

	      (_
	       (__synner__ "invalid last clause" cls)))

	  (syntax-match cls (else =>)
	    ((else ?expr ?expr* ...)
	     (__synner__ "incorrect position of keyword ELSE" cls))

	    ((?test => ?proc)
	     (let ((tmp (make-syntactic-identifier-for-temporary-variable "tmp")))
	       `(let/std ((,tmp ,?test))
		  (if ,tmp
		      (,?proc ,tmp)
		    ,(recur (car cls*) (cdr cls*))))))

	    ((?expr)
	     (let ((tmp (make-syntactic-identifier-for-temporary-variable "tmp")))
	       `(let/std ((,tmp ,?expr))
		  (if ,tmp ,tmp ,(recur (car cls*) (cdr cls*))))))

	    ((?test ?expr* ...)
	     `(if ,?test
		  (internal-body . ,?expr*)
		,(recur (car cls*) (cdr cls*))))

	    (_
	     (__synner__ "invalid last clause" cls)))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: QUASIQUOTE

(define-macro-transformer (quasiquote input-form.stx)
  ;;Transformer function  used to  expand R6RS QUASIQUOTE  macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  ;;NOTE We  can test  QUASIQUOTE expansions  by evaluating  at the  REPL expressions
  ;;like:
  ;;
  ;;   (expansion-of '(quasiquote ?pattern))
  ;;
  (define (main input-form.stx)
    (syntax-match input-form.stx (unquote unquote-splicing)

      ;;According to  R6RS: a single-operand UNQUOTE  can appear outside of  list and
      ;;vector templates.  This happens when the input form is:
      ;;
      ;;   (quasiquote (unquote 1))	=> 1
      ;;
      ((_ (unquote ?expr))
       ?expr)

      ((_ (unquote ?expr0 ?expr* ...))
       (__synner__ "invalid multi-operand UNQUOTE form outside list and vector templates"
		   (bless
		    (cons* 'unquote ?expr0 ?expr*))))

      ((_ (unquote-splicing ?expr* ...))
       (__synner__ "invalid UNQUOTE-SPLICING form outside list and vector templates"
		   (bless
		    (cons 'unquote-splicing ?expr*))))

      ((_ (?car . ?cdr))
       (%quasi (cons ?car ?cdr) 0))

      ((_ #(?expr* ...))
       (%quasi (list->vector ?expr*) 0))

      ;;This happens when the input form is:
      ;;
      ;;   (quasiquote 1)	=> 1
      ;;
      ((_ ?expr)
       (bless
	`(quote ,?expr)))

      (_
       (__synner__ "invalid syntax in macro use"))))

  (define (%quasi stx nesting-level)
    (syntax-match stx (unquote unquote-splicing quasiquote)

      ;;This happens when STX appears in improper tail position:
      ;;
      ;;   (quasiquote (1 . (unquote (+ 2 3)))) => (cons 1 5)
      ;;
      ((unquote ?expr)
       (if (zero? nesting-level)
	   ?expr
	 (%quasicons (make-top-level-syntax-object/quoted-quoting 'unquote)
		     (%quasi (list ?expr) (sub1 nesting-level)))))

      ;;This happens when the input form is:
      ;;
      ;;   (quasiquote (1 . (unquote)))
      ;;
      ((unquote)
       (__synner__ "invalid UNQUOTE form in improper tail position" stx))

      (((unquote ?input-car-subexpr* ...) . ?input-cdr)
       ;;For  coherence  with what  R6RS  specifies  about UNQUOTE:  a  multi-operand
       ;;UNQUOTE must appear only inside a list or vector template.
       ;;
       ;;When the nesting level requires processing of unquoted expressions:
       ;;
       ;;* The expressions ?INPUT-CAR-SUBEXPR must be evaluated at run-time.
       ;;
       ;;* The input syntax object ?INPUT-CDR must be processed to produce the output
       ;;  syntax object ?OUTPUT-TAIL.
       ;;
       ;;* The returned syntax object must represent an expression that, at run-time,
       ;;  will construct the result as:
       ;;
       ;;     (cons* ?input-car-subexpr ... ?output-tail)
       ;;
       (let ((input-car-subexpr*.stx  ?input-car-subexpr*)
	     (output-tail.stx         (%quasi ?input-cdr nesting-level)))
	 (if (zero? nesting-level)
	     (%unquote-splice-cons* input-car-subexpr*.stx output-tail.stx)
	   (%quasicons (%quasicons (make-top-level-syntax-object/quoted-quoting 'unquote)
				   (%quasi input-car-subexpr*.stx (sub1 nesting-level)))
		       output-tail.stx))))

      (((unquote ?input-car-subexpr* ... . ?input-car-tail) . ?input-cdr)
       (__synner__ "invalid improper list as UNQUOTE form"
		   (bless
		    (append '(unquote) ?input-car-subexpr* ?input-car-tail))))

      ;;This happens when the input form is:
      ;;
      ;;   (quasiquote (1 . (unquote-splicing)))
      ;;
      ((unquote-splicing)
       (__synner__ "invalid UNQUOTE-SPLICING form in improper tail position" stx))

      ;;This happens when STX appears in improper tail position:
      ;;
      ;;   (quasiquote (1 . (unquote-splicing (list (+ 2 3))))) => (cons 1 5)
      ;;
      ((unquote-splicing ?expr)
       (if (zero? nesting-level)
	   ?expr
	 (%quasicons (make-top-level-syntax-object/quoted-quoting 'unquote-splicing)
		     (%quasi (list ?expr) (sub1 nesting-level)))))

      ((unquote-splicing ?input-car-subexpr0 ?input-car-subexpr* ...)
       (__synner__ "invalid multi-operand UNQUOTE-SPLICING form in improper tail position" stx))

      (((unquote-splicing ?input-car-subexpr* ...) . ?input-cdr)
       ;;For  coherence   with  what   R6RS  specifies  about   UNQUOTE-SPLICING:  an
       ;;UNQUOTE-SPLICING must appear only inside a list or vector template.
       ;;
       ;;When the nesting level requires processing of unquoted expressions:
       ;;
       ;;* The  subexpressions ?INPUT-CAR-SUBEXPR must  be evaluated at  run-time and
       ;;  their results must be lists:
       ;;
       ;;     ?input-car-subexpr => (?output-car-item ...)
       ;;
       ;;* The input syntax object ?INPUT-CDR must be processed to produce the output
       ;;  syntax object ?OUTPUT-TAIL.
       ;;
       ;;* The returned syntax object must represent an expression that, at run-time,
       ;;  will construct the result as:
       ;;
       ;;     (append ?input-car-subexpr ... ?output-tail)
       ;;
       (let ((input-car-subexpr*.stx  ?input-car-subexpr*)
	     (output-tail.stx         (%quasi ?input-cdr nesting-level)))
	 (if (zero? nesting-level)
	     (%unquote-splice-append input-car-subexpr*.stx output-tail.stx)
	   (%quasicons (%quasicons (make-top-level-syntax-object/quoted-quoting 'unquote-splicing)
				   (%quasi input-car-subexpr*.stx (sub1 nesting-level)))
		       output-tail.stx))))

      (((unquote-splicing ?input-car-subexpr* ... . ?input-car-tail) . ?input-cdr)
       (__synner__ "invalid improper list as UNQUOTE-SPLICING form"
		   (bless
		    (append '(unquote-splicing) ?input-car-subexpr* ?input-car-tail))))

      ((quasiquote ?nested-expr* ...)
       (%quasicons (make-top-level-syntax-object/quoted-quoting 'quasiquote)
		   (%quasi ?nested-expr* (add1 nesting-level))))

      ((?car . ?cdr)
       (%quasicons (%quasi ?car nesting-level)
		   (%quasi ?cdr nesting-level)))

      (#(?item* ...)
       (%quasivector (%vector-quasi ?item* nesting-level)))

      (?atom
       (bless
	`(quote ,?atom)))))

;;; --------------------------------------------------------------------

  (define (%quasicons output-car.stx output-cdr.stx)
    ;;Called to compose the output form resulting from processing:
    ;;
    ;;   (?car . ?cdr)
    ;;
    ;;return  a  syntax  object.   The  argument  OUTPUT-CAR.STX  is  the  result  of
    ;;processing  "(syntax ?car)".   The  argument OUTPUT-CDR.STX  is  the result  of
    ;;processing "(syntax ?cdr)".
    ;;
    (syntax-match output-cdr.stx (foldable-list quote)

      ;;When the result of  processing ?CDR is a quoted or iquoted  datum, we want to
      ;;return one among:
      ;;
      ;;   #'(cons (quote  ?car-input-datum) (quote  ?output-cdr-datum))
      ;;   #'(cons         ?car-input-datum  (quote  ?output-cdr-datum))
      ;;
      ;;and we know that we can simplify:
      ;;
      ;;   #'(cons (quote ?car-input-datum) (quote ()))
      ;;   ===> #'(list (quote ?car-input-datum))
      ;;
      ;;   #'(cons        ?car-input-datum  (quote ()))
      ;;   ===> #'(list ?car-input-datum)
      ;;
      ((quote ?cdr-datum)
       (syntax-match output-car.stx (quote)
	 ((quote ?car-datum)
	  (syntax-match ?cdr-datum ()
	    (()
	     (bless
	      `(foldable-list (quote ,?car-datum))))
	    (_
	     (bless
	      `(foldable-cons (quote ,?car-datum) (quote ,?cdr-datum))))))
	 (_
	  (syntax-match ?cdr-datum ()
	    (()
	     (bless
	      `(foldable-list ,output-car.stx)))
	    (_
	     (bless
	      `(foldable-cons ,output-car.stx (quote ,?cdr-datum))))))
	 ))

      ;;When  the result  of  processing  ?CDR is  a  syntax  object representing  an
      ;;expression that, at run-time, will build an immutable list: prepend the input
      ;;expression as first item of the list.
      ;;
      ((foldable-list ?cdr-expr* ...)
       (bless
	`(foldable-list ,output-car.stx . ,?cdr-expr*)))

      ;;When  the result  of processing  ?CDR is  a syntax  object representing  some
      ;;generic expression: return  a syntax object representing  an expression that,
      ;;at run-time, will build a pair.
      ;;
      (_
       (bless
	`(foldable-cons ,output-car.stx ,output-cdr.stx)))
      ))

  (define (%unquote-splice-cons* input-car-subexpr*.stx output-tail.stx)
    ;;Recursive function.  Called to build  the output form resulting from processing
    ;;the input form:
    ;;
    ;;   ((unquote ?input-car-subexpr ...) . ?input-cdr)
    ;;
    ;;return a syntax object.  At the first application:
    ;;
    ;;* The argument INPUT-CAR-SUBEXPR*.STX is the list of syntax objects:
    ;;
    ;;     ((syntax ?input-car-subexpr) ...)
    ;;
    ;;* The argument OUTPUT-TAIL.STX is the result of processing:
    ;;
    ;;     (syntax ?input-cdr)
    ;;
    ;;The returned  output form must  be a  syntax object representing  an expression
    ;;that, at run-time, constructs the result as:
    ;;
    ;;   (cons* ?input-car-subexpr0 ?input-car-subexpr ... ?output-tail)
    ;;
    ;;notice that the following expansion takes place:
    ;;
    ;;   ((unquote) . ?input-cdr) ==> ?output-tail
    ;;
    (if (null? input-car-subexpr*.stx)
	output-tail.stx
      (%quasicons (car input-car-subexpr*.stx)
		  (%unquote-splice-cons* (cdr input-car-subexpr*.stx) output-tail.stx))))

  (define (%unquote-splice-append input-car-subexpr*.stx output-tail.stx)
    ;;Called to build the result of processing the input form:
    ;;
    ;;   ((unquote-splicing ?input-car-subexpr ...) . ?input-cdr)
    ;;
    ;;return a syntax object.  At the first application:
    ;;
    ;;* The argument INPUT-CAR-SUBEXPR*.STX is the list of syntax objects:
    ;;
    ;;     ((syntax ?input-car-subexpr) ...)
    ;;
    ;;  where each expression ?INPUT-CAR-SUBEXPR is expected to return a list.
    ;;
    ;;* The argument OUTPUT-TAIL.STX is the result of processing:
    ;;
    ;;     (syntax ?input-cdr)
    ;;
    ;;The returned  output form must  be a  syntax object representing  an expression
    ;;that constructs the result as:
    ;;
    ;;   (append ?input-car-subexpr0 ?input-car-subexpr ... ?output-tail)
    ;;
    ;;notice that the following expansion takes place:
    ;;
    ;;   ((unquote-splicing) . ?input-cdr) ==> ?output-tail
    ;;
    (let ((ls (let recur ((stx* input-car-subexpr*.stx))
		(if (null? stx*)
		    (syntax-match output-tail.stx (quote)
		      ((quote ())
		       '())
		      (_
		       (list output-tail.stx)))
		  (syntax-match (car stx*) (quote)
		    ((quote ())
		     (recur (cdr stx*)))
		    (_
		     (cons (car stx*) (recur (cdr stx*)))))))))
      (cond ((null? ls)
	     (bless '(quote ())))
	    ((null? (cdr ls))
	     (car ls))
	    (else
	     (bless
	      `(foldable-append . ,ls))))))

;;; --------------------------------------------------------------------

  (define (%vector-quasi item*.stx nesting-level)
    ;;Recursive function.  Called to process an input syntax object with the format:
    ;;
    ;;   #(?item ...)
    ;;
    ;;At the first invocation, the argument ITEM*.STX is a syntax object representing
    ;;a proper list of items from the vector:
    ;;
    ;;   (syntax (?item ...))
    ;;
    ;;Return a syntax object representing an expression that, at run-time, will build
    ;;an list holding the vector items.
    ;;
    ;;NOTE  The difference  between  %QUASI  and %VECTOR-QUASI  is  that: the  former
    ;;accepts both  *proper* and *improper* lists  of items; the latter  accepts only
    ;;*proper* lists of items.
    ;;
    (syntax-match item*.stx ()
      ((?input-car . ?input-cdr)
       (let ((output-tail.stx (%vector-quasi ?input-cdr nesting-level)))
	 (syntax-match ?input-car (quasiquote unquote unquote-splicing)

	   ((unquote ?input-car-subexpr* ...)
	    ;;When the nesting level requires processing of unquoted expressions:
	    ;;
	    ;;* The expressions ?INPUT-CAR-SUBEXPR must be evaluated at run-time.
	    ;;
	    ;;* The input  syntax object ?INPUT-CDR must be processed  to produce the
	    ;;  output syntax object ?OUTPUT-TAIL.
	    ;;
	    ;;*  The returned  syntax object  must represent  an expression  that, at
	    ;;  run-time, will construct the result as:
	    ;;
	    ;;     (cons* ?input-car-subexpr ... ?output-tail)
	    ;;
	    ;;  notice that the following expansion takes place:
	    ;;
	    ;;     ((unquote) . ?input-cdr) ==> ?output-tail
	    ;;
	    (let ((input-car-subexpr*.stx ?input-car-subexpr*))
	      (if (zero? nesting-level)
		  (%unquote-splice-cons* input-car-subexpr*.stx output-tail.stx)
		(%quasicons (%quasicons (make-top-level-syntax-object/quoted-quoting 'unquote)
					(%quasi input-car-subexpr*.stx (sub1 nesting-level)))
			    output-tail.stx))))

	   ((unquote ?input-car-subexpr* ... . ?input-car-tail)
	    (__synner__ "invalid improper list as UNQUOTE form"
			(bless
			 (append '(unquote) ?input-car-subexpr* ?input-car-tail))))

	   ((unquote-splicing ?input-car-subexpr* ...)
	    ;;When the nesting level requires processing of unquoted expressions:
	    ;;
	    ;;* The  subexpressions ?INPUT-CAR-SUBEXPR must be  evaluated at run-time
	    ;;  and their results must be lists:
	    ;;
	    ;;     ?input-car-subexpr => (?output-car-item ...)
	    ;;
	    ;;* The input  syntax object ?INPUT-CDR must be processed  to produce the
	    ;;  output syntax object ?OUTPUT-TAIL.
	    ;;
	    ;;*  The returned  syntax object  must represent  an expression  that, at
	    ;;  run-time, will construct the result as:
	    ;;
	    ;;     (append ?input-car-subexpr ... ?output-tail)
	    ;;
	    ;;  notice that the following expansion takes place:
	    ;;
	    ;;     ((unquote-splicing) . ?input-cdr) ==> ?output-tail
	    ;;
	    (let ((input-car-subexpr*.stx ?input-car-subexpr*))
	      (if (zero? nesting-level)
		  (%unquote-splice-append input-car-subexpr*.stx output-tail.stx)
		(%quasicons (%quasicons (make-top-level-syntax-object/quoted-quoting 'unquote-splicing)
					(%quasi input-car-subexpr*.stx (sub1 nesting-level)))
			    output-tail.stx))))

	   ((unquote-splicing ?input-car-subexpr* ... . ?input-car-tail)
	    (__synner__ "invalid improper list as UNQUOTE-SPLICING form"
			(bless
			 (append '(unquote-splicing) ?input-car-subexpr* ?input-car-tail))))

	   ((quasiquote ?nested-expr* ...)
	    (%quasicons (%quasicons (make-top-level-syntax-object/quoted-quoting 'quasiquote)
				    (%quasi ?nested-expr* (add1 nesting-level)))
			output-tail.stx))

	   ((?nested-input-car . ?nested-input-cdr)
	    (%quasicons (%quasicons (%quasi ?nested-input-car nesting-level)
				    (%quasi ?nested-input-cdr nesting-level))
			output-tail.stx))

	   (#(?nested-input-item* ...)
	    (%quasicons (%quasivector (%vector-quasi ?nested-input-item* nesting-level))
			output-tail.stx))

	   (?input-atom
	    (%quasicons (bless
			 `(quote ,?input-atom))
			output-tail.stx)))))

      (()
       (bless '(quote ())))))

  (define (%quasivector output-list.stx)
    ;;Process to call the result of %QUASI-VECTOR.  The argument OUTPUT-LIST.STX is a
    ;;syntax object representing an expression that,  at run-time, will build an list
    ;;holding the vector items.
    ;;
    ;;Return  a syntax  object representing  an  expression that,  at run-time,  will
    ;;convert the list  to a vector.  In general applying  LIST->VECTOR always works,
    ;;but there are special cases where a more efficient processing is possible.
    ;;
    (syntax-match output-list.stx (foldable-list quote)
      ((foldable-list (quote ?datum*) ...)
       (bless
	`(quote #(,@?datum*))))
      (_
       (bless
	`(foldable-list->vector ,output-list.stx)))))

  (main input-form.stx))


;;;; non-core macro: QUASISYNTAX

(module (quasisyntax-macro)
  ;;Transformer function used to expand R6RS QUASISYNTAX macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;FIXME: not really correct (Abdulaziz Ghuloum).
  ;;
  (define-macro-transformer (quasisyntax input-form.stx)
    (syntax-match input-form.stx ()
      ((_ e)
       (receive (lhs* rhs* v)
	   (quasi e 0)
	 (bless
	  `(syntax-case (list ,@rhs*) ()
	     (,lhs*
	      (syntax ,v))))))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define-module-who quasisyntax)

  (define (quasi p nesting-level)
    (syntax-match p (unsyntax unsyntax-splicing quasisyntax)
      ((unsyntax p)
       (if (zero? nesting-level)
	   (let ((g (make-syntactic-identifier-for-temporary-variable)))
	     (values (list g) (list p) g))
	 (receive (lhs* rhs* p)
	     (quasi p (sub1 nesting-level))
	   (values lhs* rhs* (list 'unsyntax p)))))

      (unsyntax
       (zero? nesting-level)
       (syntax-violation __module_who__ "incorrect use of unsyntax" p))

      (((unsyntax p* ...) . q)
       (receive (lhs* rhs* q)
	   (quasi q nesting-level)
	 (if (zero? nesting-level)
	     (let ((g* (map (lambda (x) (make-syntactic-identifier-for-temporary-variable)) p*)))
	       (values (append g* lhs*)
		       (append p* rhs*)
		       (append g* q)))
	   (receive (lhs2* rhs2* p*)
	       (quasi p* (sub1 nesting-level))
	     (values (append lhs2* lhs*)
		     (append rhs2* rhs*)
		     `((unsyntax . ,p*) . ,q))))))

      (((unsyntax-splicing p* ...) . q)
       (receive (lhs* rhs* q)
	   (quasi q nesting-level)
	 (if (zero? nesting-level)
	     (let ((g* (map (lambda (x) (make-syntactic-identifier-for-temporary-variable)) p*)))
	       (values (append (map (lambda (g) `(,g ...)) g*)
			       lhs*)
		       (append p* rhs*)
		       (append (apply append
				      (map (lambda (g) `(,g ...)) g*))
			       q)))
	   (receive (lhs2* rhs2* p*)
	       (quasi p* (sub1 nesting-level))
	     (values (append lhs2* lhs*)
		     (append rhs2* rhs*)
		     `((unsyntax-splicing . ,p*) . ,q))))))

      (unsyntax-splicing
       (zero? nesting-level)
       (syntax-violation __module_who__ "incorrect use of unsyntax-splicing" p))

      ((quasisyntax p)
       (receive (lhs* rhs* p)
	   (quasi p (add1 nesting-level))
	 (values lhs* rhs* `(quasisyntax ,p))))

      ((p . q)
       (let-values
	   (((lhs*  rhs*  p) (quasi p nesting-level))
	    ((lhs2* rhs2* q) (quasi q nesting-level)))
	 (values (append lhs2* lhs*)
		 (append rhs2* rhs*)
		 (cons p q))))

      (#(x* ...)
       (receive (lhs* rhs* x*)
	   (quasi x* nesting-level)
	 (values lhs* rhs* (list->vector x*))))

      (_
       (values '() '() p))
      ))

  #| end of module |# )


;;;; non-core macro: DEFINE-VALUES

(define-macro-transformer (define-values input-form.stx)
  ;;Transformer  function  used to  expand  Vicare's  DEFINE-VALUES macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (if (options::typed-language-enabled?)
      (define-values/checked-macro input-form.stx)
    (define-values/std-macro input-form.stx)))

(define-macro-transformer (define-values/std input-form.stx)
  ;;Transformer function  used to expand  Vicare's DEFINE-VALUES/STD macros  from the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?formals ?body0 ?body* ...)
     (receive (standard-formals.stx formals.sig)
	 (syntax-object.parse-standard-formals ?formals)
       (syntax-match standard-formals.stx ()
	 ((?id* ... ?id0)
	  ;;We want this expansion:
	  ;;
	  ;;  (define/std ?id)
	  ;;  ...
	  ;;  (define/std ?id0
	  ;;    (call-with-values
	  ;;        (lambda/std () ?body0 . ?body*)
	  ;;      (lambda/std (TMP ... TMP0)
	  ;;        (set! ?id TMP)
	  ;;        ...
	  ;;        TMP0)))
	  ;;
	  (bless
	   (let ((TMP* (generate-temporaries ?id*)))
	     `(begin
		,@(map (lambda (var)
			 `(define/std ,var))
		    ?id*)
		(define/std ,?id0
		  (call-with-values
		      (lambda/std () ,?body0 . ,?body*)
		    (lambda/std (,@TMP* TMP0)
		      ,@(map (lambda (var TMP)
			       `(set! ,var ,TMP))
			  ?id* TMP*)
		      TMP0)))))))

	 ((?id ?id* ... . ?rest-id)
	  ;;We want this expansion:
	  ;;
	  ;;  (define/std ?id)
	  ;;  ...
	  ;;  (define/std ?rest-id
	  ;;    (call-with-values
	  ;;        (lambda/std () ?body0 . ?body*)
	  ;;      (lambda/std (TMP ... . TMP-REST)
	  ;;        (set! ?id TMP)
	  ;;        ...
	  ;;        TMP-REST)))
	  ;;
	  (bless
	   (let* ((id*.stx	(cons ?id ?id*))
		  (TMP*		(generate-temporaries id*.stx))
		  (rest.sym	(make-syntactic-identifier-for-temporary-variable "rest")))
	     `(begin
		,@(map (lambda (var)
			 `(define/std ,var))
		    id*.stx)
		(define/std ,?rest-id
		  (call-with-values
		      (lambda/std () ,?body0 . ,?body*)
		    (lambda/std (,@TMP* . ,rest.sym)
		      ,@(map (lambda (var TMP)
			       `(set! ,var ,TMP))
			  id*.stx TMP*)
		      ,rest.sym)))))))

	 (?args
	  (identifier? ?args)
	  (bless
	   (let ((args.sym (make-syntactic-identifier-for-temporary-variable "args")))
	     `(define/std ,?args
		(call-with-values
		    (lambda/std () ,?body0 . ,?body*)
		  (lambda/std ,args.sym ,args.sym))))))

	 (_
	  (__synner__ "invalid syntax in macro use")))))))

(define-macro-transformer (define-values/checked input-form.stx)
  ;;Transformer function  used to  expand Vicare's DEFINE-VALUES/CHECKED  macros from
  ;;the  top-level built  in  environment.  Expand  the  contents of  INPUT-FORM.STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?formals ?body0 ?body* ...)
     (receive (standard-formals.stx formals.sig)
	 ;;This call will use "<void>" for formals without type annotation.
	 (syntax-object.parse-typed-formals ?formals (<void>-ots))
       #;(debug-print __who__ standard-formals.stx formals.sig)
       (syntax-match standard-formals.stx ()
	 ((?id* ... ?id0)
	  ;;We want this expansion:
	  ;;
	  ;;  (define/checked {?id ?type})
	  ;;  ...
	  ;;  (define/checked {?id0 ?type0}
	  ;;    (call-with-values
	  ;;        (lambda/std () ?body0 . ?body*)
	  ;;      (lambda/typed ({_ ?type0} TMP ... TMP0)
	  ;;        (set! ?id TMP)
	  ;;        ...
	  ;;        TMP0))))
	  ;;
	  (receive (type* type0)
	      (proper-list->head-and-last (type-signature.syntax-object formals.sig))
	    (let ((TMP0 (make-syntactic-identifier-for-temporary-variable (syntax->datum ?id0)))
		  (TMP* (generate-temporaries ?id*)))
	      (bless
	       `(begin
		  ,@(map (lambda (var type)
			   (if (<void>-type-id? type)
			       `(define/typed {,var ,type})
			     `(define/checked {,var ,type})))
		      ?id* type*)
		  (define/checked ,(if (<void>-type-id? type0)
				       ?id0
				     `{,?id0 ,type0})

		    (receive/checked (,@TMP* ,TMP0)
			(begin ,?body0 . ,?body*)
		      ,@(map (lambda (var TMP)
			       `(set!/initialise ,var ,TMP))
			  ?id* TMP*)
		      ,TMP0)
		    ))))))

	 ((?id ?id* ... . ?rest-id)
	  ;;We want this expansion:
	  ;;
	  ;;  (define/checked {?id ?type})
	  ;;  ...
	  ;;  (define/checked {?rest-id ?rest-type}
	  ;;    (call-with-values
	  ;;        (lambda/std () ?body0 . ?body*)
	  ;;      (lambda/typed ({_ ?rest-type} TMP ... . TMP-REST)
	  ;;        (set! ?id TMP)
	  ;;        ...
	  ;;        TMP-REST))))
	  ;;
	  (receive (type* rest-type)
	      (type-signature.syntax-object-list-and-rest formals.sig)
	    (let* ((id*.stx	(cons ?id ?id*))
		   (TMP*	(generate-temporaries id*.stx))
		   (rest.sym	(make-syntactic-identifier-for-temporary-variable (syntax->datum ?rest-id))))
	      (bless
	       `(begin
		  ,@(map (lambda (var type)
			   (if (<void>-type-id? type)
			       `(define/typed ,var)
			     `(define/checked {,var ,type})))
		      id*.stx type*)
		  (define/checked {,?rest-id ,rest-type}
		    (receive/checked (,@TMP* . ,rest.sym)
			(begin ,?body0 . ,?body*)
		      ,@(map (lambda (var TMP)
			       `(set!/initialise ,var ,TMP))
			  id*.stx TMP*)
		      ,rest.sym)
		    ))))))

	 (?args
	  (identifier? ?args)
	  (let ((ARGS		(make-syntactic-identifier-for-temporary-variable (syntax->datum ?args)))
		(args.type	(type-signature.syntax-object formals.sig)))
	    (bless
	     `(define/checked {,?args ,args.type}
		(receive/checked {,ARGS ,args.type}
		    (begin ,?body0 . ,?body*)
		  ,ARGS)
		))))

	 (_
	  (__synner__ "invalid syntax in macro use")))))))


;;;; non-core macro: DEFINE-CONSTANT-VALUES

(module (define-constant-values-macro)

  (define-macro-transformer (define-constant-values input-form.stx)
    ;;Transformer function used to expand Vicare's DEFINE-CONSTANT-VALUES macros from
    ;;the top-level  built in  environment.  Expand  the contents  of INPUT-FORM.STX;
    ;;return a syntax object that must be further expanded.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?formals ?body0 ?body* ...)
       (if (options::typed-language-enabled?)
	   (define-constant-values/checked-macro input-form.stx ?formals (cons ?body0 ?body*))
	 (define-constant-values/std-macro input-form.stx ?formals (cons ?body0 ?body*))))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define (define-constant-values/std-macro input-form.stx formals.stx body*.stx)
    (receive (standard-formals.stx formals.sig)
	(syntax-object.parse-standard-formals formals.stx)
      (syntax-match standard-formals.stx ()
	((?id* ... ?id0)
	 (let ((SHADOW* (generate-temporaries ?id*))
	       (TMP*    (generate-temporaries ?id*)))
	   (bless
	    `(begin
	       ,@(map (lambda (var)
			`(define/std ,var))
		   SHADOW*)
	       (define/std SHADOW0
		 (call-with-values
		     (lambda/std () . ,body*.stx)
		   (lambda/std (,@TMP* TMP0)
		     ,@(map (lambda (var TMP)
			      `(set! ,var ,TMP))
			 SHADOW* TMP*)
		     TMP0)))
	       ,@(map (lambda (var SHADOW)
			`(define-syntax ,var (identifier-syntax ,SHADOW)))
		   ?id* SHADOW*)
	       (define-syntax ,?id0 (identifier-syntax SHADOW0))
	       #| end of BEGIN |# ))))

	((?id* ... . ?rest-id)
	 (let ((SHADOW* (generate-temporaries ?id*))
	       (TMP*    (generate-temporaries ?id*)))
	   (bless
	    `(begin
	       ,@(map (lambda (var)
			`(define/std ,var))
		   SHADOW*)
	       (define/std rest-shadow
		 (call-with-values
		     (lambda/std () . ,body*.stx)
		   (lambda/std (,@TMP* . rest)
		     ,@(map (lambda (var TMP)
			      `(set! ,var ,TMP))
			 SHADOW* TMP*)
		     rest)))
	       ,@(map (lambda (var SHADOW)
			`(define-syntax ,var (identifier-syntax ,SHADOW)))
		   ?id* SHADOW*)
	       (define-syntax ,?rest-id (identifier-syntax rest-shadow))
	       #| end of BEGIN |# ))))

	(?args
	 (identifier? ?args)
	 (bless
	  `(begin
	     (define/std shadow
	       (call-with-values
		   (lambda/std () . ,body*.stx)
		 (lambda/std args args)))
	     (define-syntax ,?args (identifier-syntax shadow)))))
	)))

  (define (define-constant-values/checked-macro input-form.stx formals.stx body*.stx)
    (receive (standard-formals.stx formals.sig)
	;;This call will use "<top>" for arguments without type annotation.
	(syntax-object.parse-typed-formals formals.stx)
      (syntax-match standard-formals.stx ()
	((?id* ... ?id0)
	 (let ((SHADOW* (generate-temporaries ?id*))
	       (TMP*    (generate-temporaries ?id*)))
	   (receive (tag* tag0)
	       (proper-list->head-and-last (type-signature.syntax-object formals.sig))
	     (bless
	      `(begin
		 ,@(map (lambda (var tag)
			  `(define/checked (brace ,var ,tag)))
		     SHADOW* tag*)
		 (define/checked (brace SHADOW0 ,tag0)
		   (call-with-values
		       (lambda/std () . ,body*.stx)
		     (lambda/std (,@TMP* TMP0)
		       ;;These SET! form do the type validation.
		       ,@(map (lambda (var TMP)
				`(set! ,var ,TMP))
			   SHADOW* TMP*)
		       (assert-signature-and-return (,tag0) TMP0))))
		 ,@(map (lambda (var SHADOW)
			  `(define-syntax ,var (identifier-syntax ,SHADOW)))
		     ?id* SHADOW*)
		 (define-syntax ,?id0 (identifier-syntax SHADOW0))
		 #| end of BEGIN |# )))))

	((?id* ... . ?rest-id)
	 (let ((SHADOW* (generate-temporaries ?id*))
	       (TMP*    (generate-temporaries ?id*)))
	   (receive (tag* rest-tag)
	       (improper-list->list-and-rest (type-signature.syntax-object formals.sig))
	     (bless
	      `(begin
		 ,@(map (lambda (var tag)
			  `(define/checked (brace ,var ,tag)))
		     SHADOW* tag*)
		 (define/checked (brace rest-shadow ,rest-tag)
		   (call-with-values
		       (lambda/std () . ,body*.stx)
		     (lambda/std (,@TMP* . rest)
		       ;;These SET! forms do the type validation.
		       ,@(map (lambda (var TMP)
				`(set! ,var ,TMP))
			   SHADOW* TMP*)
		       (assert-signature-and-return (,rest-tag) rest))))
		 ,@(map (lambda (var SHADOW)
			  `(define-syntax ,var (identifier-syntax ,SHADOW)))
		     ?id* SHADOW*)
		 (define-syntax ,?rest-id (identifier-syntax rest-shadow))
		 #| end of BEGIN |# )))))

	(?args
	 (identifier? ?args)
	 (let ((args-tag (type-signature.syntax-object formals.sig)))
	   (bless
	    `(begin
	       (define/checked (brace shadow ,args-tag)
		 (call-with-values
		     (lambda/std () . ,body*.stx)
		   (lambda/std args
		     (assert-signature-and-return (,args-tag) args))))
	       (define-syntax ,?args
		 (identifier-syntax shadow))))))
	)))

  #| end of module: DEFINE-CONSTANT-VALUES-MACRO |# )


;;;; non-core macro: XOR

(define-macro-transformer (xor input-form.stx)
  ;;Transformer function used to expand Vicare's  XOR macros from the top-level built
  ;;in environment.   Expand the contents  of INPUT-FORM.STX; return a  syntax object
  ;;that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr* ...)
     (bless
      ;;We use LET/CHECKED so we can exploit RHS type propagation.
      (let recur ((bool/var	#f)
		  (expr*	?expr*))
	(cond ((null? expr*)
	       bool/var)
	      ((null? (cdr expr*))
	       `(let/checked ((x ,(car expr*)))
		  (if ,bool/var
		      (and (not x) ,bool/var)
		    x)))
	      (else
	       `(let/checked ((x ,(car expr*)))
		  (and (or (not ,bool/var)
			   (not x))
		       (let/checked ((tmp (or ,bool/var x)))
			 ,(recur 'tmp (cdr expr*))))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: DEFINE-INLINE, DEFINE-CONSTANT

(define-macro-transformer (define-constant input-form.stx)
  ;;Transformer  function used  to expand  Vicare's DEFINE-CONSTANT  macros from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx (brace)
    ((_ (brace ?name ?tag) ?expr)
     (and (identifier? ?name)
	  (identifier? ?tag))
     (let ((ghost (make-syntactic-identifier-for-temporary-variable (syntax->datum ?name))))
       (bless
	`(begin
	   (define (brace ,ghost ?tag) ,?expr)
	   (define-syntax ,?name
	     (identifier-syntax ,ghost))))))
    ((_ ?name ?expr)
     (identifier? ?name)
     (let ((ghost (make-syntactic-identifier-for-temporary-variable (syntax->datum ?name))))
       (bless
	`(begin
	   (define ,ghost ,?expr)
	   (define-syntax ,?name
	     (identifier-syntax ,ghost))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (define-inline-constant input-form.stx)
  ;;Transformer function  used to expand Vicare's  DEFINE-INLINE-CONSTANT macros from
  ;;the  top-level built  in  environment.  Expand  the  contents of  INPUT-FORM.STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;We want to  allow a generic expression  to generate the constant  value at expand
  ;;time.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?name ?expr)
     (bless
      `(define-syntax ,?name
	 (let/std ((const ,?expr))
	   (lambda/std (stx)
	     (if (identifier? stx)
		 ;;By  using DATUM->SYNTAX  we avoid  the  "raw symbol  in output  of
		 ;;macro" error whenever the CONST is a symbol or contains a symbol.
		 #`(quote #,(datum->syntax stx const))
	       (syntax-violation (quote ?name)
		 "invalid use of identifier syntax" stx (syntax ?name))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (define-inline input-form.stx)
  ;;Transformer  function  used to  expand  Vicare's  DEFINE-INLINE macros  from  the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ (?name ?arg* ... . ?rest) ?body0 ?body* ...)
     (and (identifier? ?name)
	  (syntax-object.typed-clambda-clause-formals? (append ?arg* ?rest)))
     (let* ((TMP*	(generate-temporaries ?arg*))
	    (rest.datum	(syntax->datum ?rest))
	    (REST	(if (null? rest.datum) '() (make-syntactic-identifier-for-temporary-variable)))
	    (STX	(make-syntactic-identifier-for-temporary-variable))
	    (BINDING*	(append (map list ?arg* TMP*)
				(cond ((null? rest.datum)
				       '())
				      ((symbol? rest.datum)
				       ;;If the  rest argument is untagged,  we tag
				       ;;it by default with "<list>".
				       (if (options::typed-language-enabled?)
					   `(((brace ,?rest <list>) (list . ,REST)))
					 `((,?rest (list . ,REST)))))
				      (else
				       `((,?rest (list . ,REST))))))))
       (bless
	`(define-fluid-syntax ,?name
	   (syntax-rules ()
	     ((_ ,@TMP* . ,REST)
	      (fluid-let-syntax
		  ((,?name (lambda/std (,STX)
			     (syntax-violation (quote ,?name) "cannot recursively expand inline expression" ,STX)))
		   (__who__  (identifier-syntax (quote ,?name))))
		(let/std ,BINDING* ,?body0 . ,?body*))))))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: INCLUDE

(define-macro-transformer (include input-form.stx)
  ;;Transformer function  used to expand  Vicare's INCLUDE macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (define (main input-form.stx)
    (syntax-match input-form.stx ()
      ((?context ?filename)
       (%include-file ?filename ?context #f))
      ((?context ?filename #t)
       (%include-file ?filename ?context #t))
      (_
       (__synner__ "invalid syntax in macro use"))))

  (define (%include-file filename-stx context-id verbose?)
    (define filename.str
      (syntax->datum filename-stx))
    (unless (string? filename.str)
      (__synner__ "expected string as include file pathname" filename-stx))
    (receive (pathname contents)
	((current-include-loader) filename.str verbose? __synner__)
      ;;We expect CONTENTS to be null or a list of annotated datums.
      (bless
       `(stale-when (internal-body
		      (import (prefix (only (vicare system posix)
					    file-modification-time)
				      posix::))
		      (or (not (file-exists? ,pathname))
			  (> (posix::file-modification-time ,pathname)
			     ,(posix::file-modification-time pathname))))
	  . ,(map (lambda (item)
		    (datum->syntax context-id item))
	       contents)))))

  (main input-form.stx))


;;;; non-core macro: DEFINE-INTEGRABLE

(define-macro-transformer (define-integrable input-form.stx)
  ;;Transformer function  used to expand  Vicare's DEFINE-INTEGRABLE macros  from the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  ;;The original  syntax was posted by  "leppie" on the Ikarus  mailing list; subject
  ;;"Macro Challenge of Last Year [Difficulty: *****]", 20 Oct 2009.
  ;;
  (syntax-match input-form.stx (lambda)
    ((_ (?name . ?formals) ?form0 ?form* ...)
     (identifier? ?name)
     (let ((XNAME (make-syntactic-identifier-for-temporary-variable (syntax->datum ?name))))
       (bless
	`(begin
	   (define-fluid-syntax ,?name
	     (lambda/std (x)
	       (syntax-case x ()
		 (_
		  (identifier? x)
		  #',XNAME)

		 ((_ arg ...)
		  #'((fluid-let-syntax
			 ((,?name (identifier-syntax ,XNAME)))
		       (lambda/std ,?formals ,?form0 ,@?form*))
		     arg ...)))))
	   (define ,XNAME
	     (fluid-let-syntax ((,?name (identifier-syntax ,XNAME)))
	       (lambda/std ,?formals ,?form0 ,@?form*)))
	   ))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: DEFINE-SYNTAX-PARAMETER, SYNTAX-PARAMETRISE

(define-macro-transformer (define-syntax-parameter input-form.stx)
  ;;Transformer function used to  expand Vicare's DEFINE-SYNTAX-PARAMETER macros from
  ;;the  top-level built  in  environment.  Expand  the  contents of  INPUT-FORM.STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?param-id ?param-expr)
     (identifier? ?param-id)
     (bless
      `(define-fluid-syntax ,?param-id
	 (make-expand-time-value ,?param-expr))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (syntax-parametrise input-form.stx)
  ;;Transformer function used to expand Vicare's SYNTAX-PARAMETRISE-MACRO macros from
  ;;the  top-level built  in  environment.  Expand  the  contents of  INPUT-FORM.STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ((?lhs* ?rhs*) ...) ?body0 ?body* ...)
     (for-all identifier? ?lhs*)
     (bless
      `(fluid-let-syntax ,(map (lambda (lhs rhs)
				 (list lhs `(make-expand-time-value ,rhs)))
			    ?lhs* ?rhs*)
	 ,?body0 . ,?body*)))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; non-core macro: PRE-INCR!, PRE-DECR!, POST-INCR!, POST-DECR!

(define-macro-transformer (pre-incr! input-form.stx)
  ;;Transformer function used to expand  Vicare's PRE-INCR! macros from the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (bless
      `(begin
	 (set! ,?id (add1 ,?id))
	 ,?id)))
    ((_ ?id ?step)
     (identifier? ?id)
     (bless
      `(begin
	 (set! ,?id (+ ,?id ,?step))
	 ,?id)))
    ((_ ?expr)
     (bless
      `(add1 ,?expr)))
    ((_ ?expr ?step)
     (bless
      `(+ ,?expr ,?step)))
    (_
     (__synner__ "invalid pre-increment operation"))))

(define-macro-transformer (pre-decr! input-form.stx)
  ;;Transformer function used to expand  Vicare's PRE-DECR! macros from the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (bless
      `(begin
	 (set! ,?id (sub1 ,?id))
	 ,?id)))
    ((_ ?id ?step)
     (identifier? ?id)
     (bless
      `(begin
	 (set! ,?id (- ,?id ,?step))
	 ,?id)))
    ((_ ?expr)
     (bless
      `(sub1 ,?expr)))
    ((_ ?expr ?step)
     (bless
      `(- ,?expr ,?step)))
    (_
     (__synner__ "invalid pre-decrement operation"))))

;;; --------------------------------------------------------------------

(define-macro-transformer (post-incr! input-form.stx)
  ;;Transformer function used to expand Vicare's POST-INCR! macros from the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (bless
      `(receive-and-return (V)
	   ,?id
	 (set! ,?id (add1 ,?id)))))
    ((_ ?id ?step)
     (identifier? ?id)
     (bless
      `(receive-and-return (V)
	   ,?id
	 (set! ,?id (+ ,?id ,?step)))))
    ((_ ?expr)
     (bless
      `(add1 ,?expr)))
    ((_ ?expr ?step)
     (bless
      `(+ ,?expr ,?step)))
    (_
     (__synner__ "invalid post-increment operation"))))

(define-macro-transformer (post-decr! input-form.stx)
  ;;Transformer function used to expand Vicare's POST-DECR! macros from the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?id)
     (identifier? ?id)
     (bless
      `(receive-and-return (V)
	   ,?id
	 (set! ,?id (sub1 ,?id)))))
    ((_ ?id ?step)
     (identifier? ?id)
     (bless
      `(receive-and-return (V)
	   ,?id
	 (set! ,?id (- ,?id ,?step)))))
    ((_ ?expr)
     (bless
      `(sub1 ,?expr)))
    ((_ ?expr ?step)
     (bless
      `(- ,?expr ,?step)))
    (_
     (__synner__ "invalid post-decrement operation"))))


;;;; non-core macro: miscellanea

(define-macro-transformer (time input-form.stx)
  ;;Transformer function used to expand Vicare's TIME macros from the top-level built
  ;;in environment.   Expand the contents  of INPUT-FORM.STX; return a  syntax object
  ;;that must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let ((str (receive (port getter)
		    (open-string-output-port)
		  (write (syntax->datum ?expr) port)
		  (getter))))
       (bless
	`(time-it ,str (lambda/std () ,?expr)))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (delay input-form.stx)
  ;;Transformer function used to expand R6RS DELAY macros from the top-level built in
  ;;environment.  Expand the contents of  INPUT-FORM.STX; return a syntax object that
  ;;must be further expanded.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (bless
      `(make-promise (lambda/std () ,?expr))))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (assert input-form.stx)
  ;;Defined  by R6RS.   An ASSERT  form  is evaluated  by evaluating  EXPR.  If  EXPR
  ;;returns a true value, that value is returned from the ASSERT expression.  If EXPR
  ;;returns false, an  exception with condition types "&assertion"  and "&message" is
  ;;raised.   The  message  provided  in   the  condition  object  is  implementation
  ;;dependent.
  ;;
  ;;NOTE Implementations should  exploit the fact that ASSERT is  a syntax to provide
  ;;as much information as possible about the location of the assertion failure.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (if (options::drop-assertions?)
	 ?expr
       (let ((pos (or (expression-position input-form.stx)
		      (expression-position ?expr))))
	 (bless
	  (if (source-position-condition? pos)
	      `(or ,?expr
		   (assertion-error
		    ',?expr ,(source-position-port-id pos)
		    ,(source-position-byte pos) ,(source-position-character pos)
		    ,(source-position-line pos) ,(source-position-column    pos)))
	    `(or ,?expr
		 (assertion-error ',?expr "unknown source" #f #f #f #f)))))))
    (_
     (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

(define-macro-transformer (__file__ input-form.stx)
  (let ((expr (stx-expr input-form.stx)))
    (if (reader-annotation? expr)
	(let ((pos (reader-annotation-textual-position expr)))
	  (if (source-position-condition? pos)
	      (bless
	       `(quote ,(source-position-port-id pos)))
	    (bless
	     `(quote ,(source-code-location)))))
      (bless
       `(quote ,(source-code-location))))))

(define-macro-transformer (__line__ input-form.stx)
  (let ((expr (stx-expr input-form.stx)))
    (if (reader-annotation? expr)
	(let ((pos (reader-annotation-textual-position expr)))
	  (if (source-position-condition? pos)
	      (bless
	       `(quote ,(source-position-line pos)))
	    (bless '(quote #f))))
      (bless '(quote #f)))))

;;; --------------------------------------------------------------------

(define-macro-transformer (stdin input-form.stx)
  (bless '(console-input-port)))

(define-macro-transformer (stdout input-form.stx)
  (bless '(console-output-port)))

(define-macro-transformer (stderr input-form.stx)
  (bless '(console-error-port)))


;;;; symbol enumeration syntaxes

(define (%allowed-symbol-macro input-form.stx allowed-symbol-set)
  ;;Helper   function    used   to   implement   the    transformer   of:   EOL-STYLE
  ;;ERROR-HANDLING-MODE, BUFFER-MODE, ENDIANNESS.  All  of these macros should expand
  ;;to a quoted symbol among a list of allowed ones.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?name)
     (and (identifier? ?name)
	  (memq (identifier->symbol ?name) allowed-symbol-set))
     (bless
      `(quote ,?name)))
    (_
     (__synner__ "invalid syntax in macro use"))))

;;; --------------------------------------------------------------------

(define-macro-transformer (eol-style input-form.stx)
  (%allowed-symbol-macro input-form.stx '(none lf cr crlf nel crnel ls)))

(define-macro-transformer (error-handling-mode input-form.stx)
  (%allowed-symbol-macro input-form.stx '(ignore raise replace)))

(define-macro-transformer (buffer-mode input-form.stx)
  (%allowed-symbol-macro input-form.stx '(none line block)))

(define-macro-transformer (file-options input-form.stx)
  ;;Transformer for the FILE-OPTIONS macro.  File options selection is implemented as
  ;;an  enumeration  type  whose  constructor  is  MAKE-FILE-OPTIONS  from  the  boot
  ;;environment.
  ;;
  (define (valid-option? opt-stx)
    (and (identifier? opt-stx)
	 (memq (identifier->symbol opt-stx) '(no-fail no-create no-truncate executable))))
  (syntax-match input-form.stx ()
    ((_ ?opt* ...)
     (for-all valid-option? ?opt*)
     (bless
      `(make-file-options ',?opt*)))
    (_
     (__synner__ "invalid syntax in macro use"))))

(define-macro-transformer (expander-options input-form.stx)
  ;;Transformer  for the  EXPANDER-OPTIONS  macro.  Expander  options selection  that
  ;;expands to a quoted list of symbols.
  ;;
  (define (valid-option? opt-stx)
    (and (identifier? opt-stx)
	 (case (identifier->symbol opt-stx)
	   ((strict-r6rs typed-language default-language)
	    #t)
	   (else #f))))
  (syntax-match input-form.stx ()
    ((_ ?opt* ...)
     (for-all valid-option? ?opt*)
     (list (core-prim-id 'make-expander-options) (list (core-prim-id 'quote) ?opt*)))
    (_
     (__synner__ "invalid options"))))

(define-macro-transformer (compiler-options input-form.stx)
  ;;Transformer  for the  COMPILER-OPTIONS  macro.  Compiler  options selection  that
  ;;expands to a quoted list of symbols.
  ;;
  (define (valid-option? opt-stx)
    (and (identifier? opt-stx)
	 (memq (identifier->symbol opt-stx) '(strict-r6rs default-language))))
  (syntax-match input-form.stx ()
    ((_ ?opt* ...)
     (for-all valid-option? ?opt*)
     (list (core-prim-id 'make-compiler-options) (list (core-prim-id 'quote) ?opt*)))
    (_
     (__synner__ "invalid options"))))

(define-macro-transformer (endianness input-form.stx)
  ;;Transformer  of ENDIANNESS.   Support  the symbols:  "big", "little",  "network",
  ;;"native"; convert "network" to "big".
  ;;
  (syntax-match input-form.stx ()
    ((_ ?name)
     (and (identifier? ?name)
	  (memq (identifier->symbol ?name) '(big little network native)))
     (case (identifier->symbol ?name)
       ((network)
	(bless '(quote big)))
       ((native)
	(bless '(native-endianness)))
       ((big little)
	(bless `(quote ,?name)))))
    (_
     (__synner__ "invalid syntax in macro use"))))


;;;; done

#| end of library |# )

;;; end of file
