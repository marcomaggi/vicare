;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: generic functions
;;;Date: Mon Jul  5, 2010
;;;
;;;Abstract
;;;--------
;;;
;;;	The ancestor of this library is ScmObj by Dorai Sitaram.  In the
;;;	hierarchy of  libraries (nausicaa language multimethods  ---) we
;;;	accept  some  code duplication  with  libraries  outside of  the
;;;	hierarchy  with  the purpose  of  making  the implementation  of
;;;	generics independent.
;;;
;;;What is a method's signature
;;;----------------------------
;;;
;;;	The "signature"  of a  method is a  list of lists,  each sublist
;;;	being  a list of  record type  UIDs.  The  first sublist  is the
;;;	hierarchy  of UIDs of  the first  method's argument,  the second
;;;	sublist  is the  hierarchy  of the  second  argument, etc.   For
;;;	example, a method defined as:
;;;
;;;	   (define-method (doit (a <complex>) (b <string>) (c <char>))
;;;          ---)
;;;
;;;	has the following signature:
;;;
;;;	   ((nausicaa:builtin:<complex>
;;;	     nausicaa:builtin:<number>
;;;          nausicaa:builtin:<top>)    ;first argument
;;;
;;;         (nausicaa:builtin:<string>
;;;          nausicaa:builtin:<top>)    ;second argument
;;;
;;;         (nausicaa:builtin:<char>
;;;          nausicaa:builtin:<top>))   ;third argument
;;;
;;;
;;;Copyright (c) 2010-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 1996 Dorai Sitaram
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa language multimethods)
  (export
    define-generic-definer	define-generic*-definer
    define-method		add-method
    call-next-method		next-method?

    ;; auxiliary syntaxes
    (deprefix (aux.reverse-before-methods?
	       aux.argument-type-inspector
	       aux.merge-with-multimethods
	       aux.:primary
	       aux.:around
	       aux.:before
	       aux.:after)
	      aux.))
  (import (for (vicare)
	    run expand (meta 2))
    ;;See the source file for the customisable interface to types.
    (prefix (nausicaa language multimethods types) type.)
    (prefix (nausicaa language multimethods methods-table) mt.)
    (for (prefix (nausicaa language multimethods helpers-for-expand)
		 help.)
      expand)
    (prefix (only (nausicaa language auxiliary-syntaxes)
		  argument-type-inspector
		  reverse-before-methods?
		  merge-with-multimethods
		  :primary :before :after :around
		  <-)
	    aux.)
    (nausicaa language multimethods auxiliary-syntaxes))


;;;; next method implementation
;;
;;The   "traditional"  next-method  implementation   automatically  adds
;;arguments to the  lambdas implementing a method; if  a method's lambda
;;is defined as:
;;
;;   (lambda (a b c) ---)
;;
;;the traditional mechanism modifies it as:
;;
;;   (lambda (a b c call-next-method next-method?) ---)
;;
;;this has  the advantge of speed,  but it also has  some drawbacks, for
;;example  it is not  possible to  use any  existent function  as method
;;implementation.  This library makes  use of parameters instread, which
;;are slower but simpler in the opinion of the author.
;;

(define next-method-func-parm (make-parameter #f))
(define next-method-pred-parm (make-parameter #f))

(define-syntax call-next-method
  (syntax-rules ()
    ((_)
     (let ((f (next-method-func-parm)))
       (if f (f)
	 (assertion-violation 'call-next-method
	   "invoked call-next-method outside of a generic function"))))))

(define-syntax next-method?
  (syntax-rules ()
    ((_)
     (let ((f (next-method-pred-parm)))
       (if f (f)
	 (assertion-violation 'next-method?
	   "invoked next-method? outside of a generic function"))))))


;;;; ordinary generic functions definition

(define-syntax (define-generic-definer stx)
  (define who 'define-generic-definer)

  (define (%main stx)
    (syntax-case stx ()
      ((_ ?generic-definer ?clause ...)
       (identifier? #'?generic-definer)
       (let ((options (%parse-clauses #'(?clause ...))))
	 (unless (options-uid-list-function-id options)
	   (synner "missing UID list function specification"))
	 (with-syntax ((UID-LIST-OF (options-uid-list-function-id options)))
	   #`(define-syntax ?generic-definer
	       (syntax-rules (aux.merge-with-multimethods)
		 ((_ ?name (?formal (... ...)))
		  (%define-generic-implementation UID-LIST-OF ?name (?formal (... ...))))
		 ((_ ?name (?formal (... ...)) (aux.merge-with-multimethods ?merge-generic (... ...)))
		  (%define-generic-implementation UID-LIST-OF ?name (?formal (... ...))
						  ?merge-generic (... ...)))
		 )))))))

  (define-record-type options
    (nongenerative)
    (fields (mutable uid-list-function-id))
    (protocol (lambda (make-record)
		(lambda ()
		  (make-record #f)))))

  (define (%parse-clauses clauses)
    (let loop ((clauses clauses)
               (options (make-options)))
      (syntax-case clauses ()
	(()
	 options)
	((?clause . ?other-clauses)
	 (begin
	   (%parse-option-clause! options #'?clause)
	   (loop #'?other-clauses options))))))

  (define (%parse-option-clause! options clause)
    (syntax-case clause (aux.argument-type-inspector)
      ;;The value of  this clause must be an expression  evaluating to a
      ;;function.
      ((aux.argument-type-inspector ?uid-list-of)
       (options-uid-list-function-id-set! options #'?uid-list-of))
      (_
       (synner "unknown clause or invalid clause syntax" clause))))

  (define synner
    (case-lambda
     ((message)
      (synner message #f))
     ((message subform)
      (syntax-violation who message stx subform))))

  (%main stx))

(define-syntax (%define-generic-implementation stx)
  (define (%main stx)
    (syntax-case stx ()
      ((_ ?uid-list-of ?name (?formal ...) ?to-be-merged ...)
       (let ((uid-list-of-stx	#'?uid-list-of)
	     (name-id		#'?name)
	     (formals-stx	(syntax->list #'(?formal ...)))
	     (to-be-merged-stx	(syntax->list #'(?to-be-merged ...))))
	 ;;The value of UID-LIST-OF-STX must be an expression evaluating
	 ;;to a function.
	 (unless (identifier? name-id)
	   (synner "expected identifier as generic function name" name-id))
	 (unless (all-identifiers? formals-stx)
	   (synner "expected proper list of identifiers as generic function formals" formals-stx))
	 (unless (all-identifiers? to-be-merged-stx)
	   (synner "expected proper list of identifiers as names of multimethods to be merged" to-be-merged-stx))
	 (cond ((duplicate-identifiers? to-be-merged-stx free-identifier=?)
		=> (lambda (dup)
		     (synner "request to merge the same generic function more than once" dup))))
	 (generate-output-form name-id formals-stx uid-list-of-stx to-be-merged-stx)))
      (_
       (synner "invalid syntax in generic function definition"))))

  (define (generate-output-form name-id formals-stx uid-list-of-stx to-be-merged-stx)
    (with-syntax ((NAME                name-id)
		  (NUMBER-OF-ARGUMENTS (length (syntax->datum formals-stx)))
		  (UID-LIST-OF         uid-list-of-stx)
		  ((GENERIC ...)       to-be-merged-stx))
      #'(begin
	  (define number-of-arguments
	    (let ((N NUMBER-OF-ARGUMENTS))
	      (for-all
		  (lambda (M)
		    (unless (= N M)
		      (assertion-violation 'NAME
			(string-append
			 "attempt to merge generic function with wrong number of arguments, requested "
			 (number->string N) " got " (number->string M)))))
		(list (GENERIC :number-of-arguments) ...))
	      N))
	  (mt.define-methods-table NAME NUMBER-OF-ARGUMENTS
				   the-methods-alist the-method-add
				   the-cache the-cache-store the-cache-ref
				   (mt.merge-methods-alists (GENERIC :primary-methods-alist) ...))
	  (define (implementation . arguments)
	    (generic-function-implementation 'NAME the-methods-alist the-cache-store the-cache-ref
					     UID-LIST-OF number-of-arguments arguments))
	  (define-syntax NAME
	    (lambda (stx)
	      (define-syntax synner
		(syntax-rules ()
		  ((_ message)
		   (synner message #f))
		  ((_ message subform)
		   (syntax-violation 'NAME message (syntax->datum stx) (syntax->datum subform)))))
	      (syntax-case stx ( ;;
				:primary-method-add :primary-methods-alist
				:number-of-arguments :primary-cache)
		((_ :primary-method-add signature closure)
		 #'(the-method-add signature closure))
		((_ :primary-methods-alist)
		 #'the-methods-alist)
		((_ :primary-cache)
		 #'the-cache)
		((_ :number-of-arguments)
		 #'number-of-arguments)
		((_ key signature closure)
		 (and (identifier? #'key)
		      (identifier-memq #'key (list #':before-method-add
						   #':after-method-add
						   #':around-method-add)))
		 (synner "attempt to add method of invalid category \
                          to ordinary generic function" #'key))
		((_ key)
		 (and (identifier? #'key)
		      (identifier-memq #'key (list #':before-methods-alist
						   #':after-methods-alist
						   #':around-methods-alist)))
		 (synner "attempt to extract method table of invalid category \
                          from ordinary generic function" #'key))
		;;This  is the  reference to  the generic  function, for
		;;example when used with MAP.
		(?id
		 (identifier? #'?id)
		 #'implementation)
		;;This is the generic  function call with arguments in a
		;;form.
		((_ ?arg (... ...))
		 #'(implementation ?arg (... ...)))
		(_
		 (synner "invalid arguments to generic function")))))
	  )))

  (define who '%define-generic-implementation)

  (define synner
    (case-lambda
     ((message)
      (synner message #f))
     ((message subform)
      (syntax-violation who message stx subform))))

  (%main stx))


(define (generic-function-implementation who methods-alist cache-store cache-ref
					 uid-list-of expected-number-of-arguments arguments)

  (define signature
    (let ((given-number-of-arguments (length arguments)))
      (unless expected-number-of-arguments
	(assertion-violation who "called generic function with no methods"))
      (unless (= expected-number-of-arguments given-number-of-arguments)
	(assertion-violation who
	  (string-append "wrong number of arguments, expected "
			 (number->string expected-number-of-arguments)
			 " given " (number->string given-number-of-arguments))
	  arguments))
      (map uid-list-of arguments)))
  (define applicable-methods
    (or (cache-ref signature)
	(let ((methods (mt.compute-applicable-methods signature methods-alist)))
	  (cache-store signature methods)
	  methods)))
  (define method-called? #f)
  (define (is-a-next-method-available?)
    (null? applicable-methods))
  (define (call-methods)
    (cond ((not (null? applicable-methods))
	   (unless method-called?
	     (set! method-called? #t))
	   (let ((method-entry (car applicable-methods)))
	     (set! applicable-methods (cdr applicable-methods))
	     (apply (cdr method-entry) arguments)))
	  (method-called?
	   (assertion-violation who
	     "called next method but no more methods available"))
	  (else
	   (assertion-violation who
	     "no method defined for the argument's types"
	     signature))))
  (parametrise ((next-method-func-parm call-methods)
		(next-method-pred-parm is-a-next-method-available?))
    (call-methods)))


;;;; starred generic functions definition

(define-syntax (define-generic*-definer stx)
  (define who 'define-generic*-definer)

  (define (%main stx)
    (syntax-case stx ()
      ((_ ?generic*-definer ?clause ...)
       (identifier? #'?generic*-definer)
       (let ((options (%parse-clauses #'(?clause ...))))
	 (unless (options-uid-list-function-id options)
	   (synner "missing UID list function specification"))
	 (with-syntax
	     ((UID-LIST-OF              (options-uid-list-function-id options))
	      (REVERSE-BEFORE-METHODS?  (options-reverse-before-methods? options)))
	   #`(define-syntax ?generic*-definer
	       (syntax-rules (aux.merge-with-multimethods)
		 ((_ ?name (?formal (... ...)))
		  (%define-generic*-implementation UID-LIST-OF REVERSE-BEFORE-METHODS?
						   ?name (?formal (... ...))))
		 ((_ ?name (?formal (... ...)) (aux.merge-with-multimethods ?merge-generic (... ...)))
		  (%define-generic*-implementation UID-LIST-OF REVERSE-BEFORE-METHODS?
						   ?name (?formal (... ...))
						   ?merge-generic (... ...)))
		 )))))))

  (define-record-type options
    (nongenerative)
    (fields (mutable uid-list-function-id)
	    (mutable reverse-before-methods?))
    (protocol (lambda (make-record)
		(lambda ()
		  (make-record #f #f)))))

  (define (%parse-clauses clauses)
    (let loop ((clauses clauses)
               (options (make-options)))
      (syntax-case clauses ()
	(()
	 options)
	((?clause . ?other-clauses)
	 (begin
	   (%parse-option-clause! options #'?clause)
	   (loop #'?other-clauses options))))))

  (define (%parse-option-clause! options clause)
    (syntax-case clause (aux.argument-type-inspector aux.reverse-before-methods?)
      ;;The value of  this clause must be an expression  evaluating to a
      ;;function.
      ((aux.argument-type-inspector ?uid-list-of)
       (options-uid-list-function-id-set! options #'?uid-list-of))
      ((aux.reverse-before-methods? ?bool)
       (boolean? (syntax->datum #'?bool))
       (options-reverse-before-methods?-set! options (syntax->datum #'?bool)))
      (_
       (synner "unknown clause or invalid clause syntax" clause))))

  (define synner
    (case-lambda
     ((message)
      (synner message #f))
     ((message subform)
      (syntax-violation who message stx subform))))

  (%main stx))

(define-syntax (%define-generic*-implementation stx)
  (define (%main stx)
    (syntax-case stx ()
      ((_ ?uid-list-of ?reverse-before-methods ?name (?formal ...) ?to-be-merged ...)
       (let ((uid-list-of-stx		#'?uid-list-of)
	     (reverse-before-methods?	(syntax->datum #'?reverse-before-methods))
	     (name-id			#'?name)
	     (formals-stx		(syntax->list #'(?formal ...)))
	     (to-be-merged-stx		(syntax->list #'(?to-be-merged ...))))
	 ;;The value of UID-LIST-OF-STX must be an expression evaluating
	 ;;to a function.
	 (unless (boolean? reverse-before-methods?)
	   (synner "expected boolean as specification to reverse before methods" #'?reverse-before-methods))
	 (unless (identifier? name-id)
	   (synner "expected identifier as generic function name" name-id))
	 (unless (all-identifiers? formals-stx)
	   (synner "expected proper list of identifiers as generic function formals" formals-stx))
	 (unless (all-identifiers? to-be-merged-stx)
	   (synner "expected proper list of identifiers as names of multimethods to be merged" to-be-merged-stx))
	 (cond ((duplicate-identifiers? to-be-merged-stx free-identifier=?)
		=> (lambda (dup)
		     (synner "request to merge the same generic function more than once" dup))))
	 (generate-output-form name-id formals-stx uid-list-of-stx to-be-merged-stx
			       reverse-before-methods?)))
      (_
       (synner "invalid syntax in generic function definition"))))

  (define (generate-output-form name-id formals-stx uid-list-of-stx to-be-merged-stx reverse-before-methods?)
    (with-syntax ((NAME                   name-id)
		  (NUMBER-OF-ARGUMENTS    (length (syntax->datum formals-stx)))
		  (UID-LIST-OF            uid-list-of-stx)
		  ((GENERIC ...)          to-be-merged-stx)
		  (REVERSE-BEFORE-METHODS reverse-before-methods?))
      #'(begin
	  (define number-of-arguments
	    (let ((N NUMBER-OF-ARGUMENTS))
	      (for-all
		  (lambda (M)
		    (unless (= N M)
		      (assertion-violation 'NAME
			(string-append
			 "attempt to merge generic function with wrong number of arguments, requested "
			 (number->string N) " got " (number->string M)))))
		(list (GENERIC :number-of-arguments) ...))
	      N))
	  (mt.define-methods-table NAME NUMBER-OF-ARGUMENTS
				   primary-methods-alist primary-method-add
				   primary-cache primary-cache-store primary-cache-ref
				   (mt.merge-methods-alists (GENERIC :primary-methods-alist) ...))
	  (mt.define-methods-table NAME NUMBER-OF-ARGUMENTS
				   before-methods-alist before-method-add
				   before-cache before-cache-store before-cache-ref
				   (mt.merge-methods-alists (GENERIC :before-methods-alist) ...))
	  (mt.define-methods-table NAME NUMBER-OF-ARGUMENTS
				   after-methods-alist after-method-add
				   after-cache after-cache-store after-cache-ref
				   (mt.merge-methods-alists (GENERIC :after-methods-alist) ...))
	  (mt.define-methods-table NAME NUMBER-OF-ARGUMENTS
				   around-methods-alist around-method-add
				   around-cache around-cache-store around-cache-ref
				   (mt.merge-methods-alists (GENERIC :around-methods-alist) ...))
	  (define reverse-before-methods REVERSE-BEFORE-METHODS)
	  (define (implementation . arguments)
	    (generic*-function-implementation
	     'NAME
	     primary-methods-alist primary-cache-ref primary-cache-store
	     before-methods-alist  before-cache-ref  before-cache-store
	     after-methods-alist   after-cache-ref   after-cache-store
	     around-methods-alist  around-cache-ref  around-cache-store
	     UID-LIST-OF number-of-arguments reverse-before-methods arguments))
	  (define-syntax NAME
	    (lambda (stx)
	      (syntax-case stx ( ;;
				:primary-method-add :primary-methods-alist
				:after-method-add   :after-methods-alist
				:before-method-add  :before-methods-alist
				:around-method-add  :around-methods-alist
				:primary-cache      :before-cache
				:after-cache        :around-cache
				:number-of-arguments)
		((_ :primary-method-add	signature closure)
		 #'(primary-method-add	signature closure))
		((_ :after-method-add	signature closure)
		 #'(after-method-add	signature closure))
		((_ :before-method-add	signature closure)
		 #'(before-method-add	signature closure))
		((_ :around-method-add	signature closure)
		 #'(around-method-add	signature closure))

		((_ :primary-methods-alist)	#'primary-methods-alist)
		((_ :before-methods-alist)	#'before-methods-alist)
		((_ :after-methods-alist)	#'after-methods-alist)
		((_ :around-methods-alist)	#'around-methods-alist)

		((_ :primary-cache)		#'primary-cache)
		((_ :before-cache)		#'before-cache)
		((_ :after-cache)		#'after-cache)
		((_ :around-cache)		#'around-cache)

		((_ :number-of-arguments)
		 #'number-of-arguments)
		;;This is the reference  to the generic function, for
		;;example when used with MAP.
		(??id
		 (identifier? #'??id)
		 #'implementation)
		;;This is  the generic function  call with arguments
		;;in a form.
		((_ ??arg (... ...))
		 #'(implementation ??arg (... ...)))
		(_
		 (syntax-violation 'NAME
		   "invalid arguments to generic function"
		   (syntax->datum stx))))))
	  )))

  (define who '%define-generic*-implementation)

  (define synner
    (case-lambda
     ((message)
      (synner message #f))
     ((message subform)
      (syntax-violation who message stx subform))))

  (%main stx))


(define (generic*-function-implementation
	 who
	 primary-methods-alist primary-cache-ref primary-cache-store
	 before-methods-alist  before-cache-ref  before-cache-store
	 after-methods-alist   after-cache-ref   after-cache-store
	 around-methods-alist  around-cache-ref  around-cache-store
	 uid-list-of expected-number-of-arguments reverse-before-methods
	 arguments)
  (define signature
    (let ((given-number-of-arguments (length arguments)))
      (unless expected-number-of-arguments
	(assertion-violation who "called generic function with no methods"))
      (unless (= expected-number-of-arguments given-number-of-arguments)
	(assertion-violation who
	  (string-append "wrong number of arguments, expected "
			 (number->string expected-number-of-arguments) " given "
			 (number->string given-number-of-arguments))
	  arguments))
      (map uid-list-of arguments)))
  (define-syntax-rule (apply-function ?method)
    (apply ?method arguments))
  (define-syntax-rule (consume-method ?method-alist)
    (begin0
	(cdar ?method-alist)
      (set! ?method-alist (cdr ?method-alist))))
  (define-syntax define-applicable-methods
    (syntax-rules ()
      ((_ NAME ALIST STORE REF)
       (define NAME
	 (or (REF signature)
	     (let ((methods (mt.compute-applicable-methods signature ALIST)))
	       (STORE signature methods)
	       methods))))
      ((_ NAME ALIST STORE REF REVERSE?)
       (define NAME
	 (or (REF signature)
	     (let* ((ell     (mt.compute-applicable-methods signature ALIST))
		    (methods (if REVERSE? (reverse ell) ell)))
	       (STORE signature methods)
	       methods))))
      ))
  (define-applicable-methods applicable-around-methods
    around-methods-alist around-cache-store around-cache-ref)
  (define-applicable-methods applicable-primary-methods
    primary-methods-alist primary-cache-store primary-cache-ref)
  (define-applicable-methods applicable-before-methods
    before-methods-alist before-cache-store before-cache-ref reverse-before-methods)
  (define-applicable-methods applicable-after-methods
    after-methods-alist after-cache-store after-cache-ref #t)
  (define primary-method-called? #f)
  (define reject-recursive-calls? #f)
  (define (is-a-next-method-available?)
    (not (if primary-method-called?
	     (null? applicable-primary-methods)
	   (and (null? applicable-around-methods)
		(null? applicable-primary-methods)))))
  (define (call-methods)
    (cond (reject-recursive-calls?
	   ;;Raise an  error if a  ":before" or ":after"  method invokes
	   ;;the next method.
	   (assertion-violation who
	     ":before and :after methods are forbidden to call the next method"))

	  (primary-method-called?
	   ;;We enter here only if a primary method has been called and,
	   ;;in its body, a call to CALL-NEXT-METHOD is evaluated.
	   (when (null? applicable-primary-methods)
	     (assertion-violation who
	       "called next method but no more :primary methods available"))
	   (apply-function (consume-method applicable-primary-methods)))

	  ((null? applicable-primary-methods)
	   ;;Raise an error if no applicable methods.
	   (assertion-violation who "no method defined for argument tags" signature))

	  ((not (null? applicable-around-methods))
	   ;;If  around  methods exist:  we  apply  them  first.  It  is
	   ;;expected that an  around method invokes CALL-NEXT-METHOD to
	   ;;evaluate the  next around  methods and finally  the primary
	   ;;methods.
	   (apply-function (consume-method applicable-around-methods)))

	  (else
	   ;;Apply  the  methods: before,  primary,  after.  Return  the
	   ;;return value of the primary.
	   (begin ;run before methods
	     (set! reject-recursive-calls? #t)
	     (let loop ((applicable-before-methods applicable-before-methods))
	       (unless (null? applicable-before-methods)
		 (apply-function (cdar applicable-before-methods))
		 (loop (cdr applicable-before-methods))))
	     (set! reject-recursive-calls? #f))
	   (set! primary-method-called? #t)
	   (begin0
	       (apply-function (consume-method applicable-primary-methods))
	     (begin ;run after methods
	       (set! reject-recursive-calls? #t)
	       (let loop ((applicable-after-methods applicable-after-methods))
		 (unless (null? applicable-after-methods)
		   (apply-function (cdar applicable-after-methods))
		   (loop (cdr applicable-after-methods)))))
	     ))))

  (parametrise ((next-method-func-parm call-methods)
		(next-method-pred-parm is-a-next-method-available?))
    (call-methods)))


(define-syntax (define-method stx)
  ;;Define  a new  starred  method and  store  it in  the given  starred
  ;;generic function.
  ;;
  (define who 'define-method)
  (define (main generic-function-spec table-key formals-stx body-stx)
    (let loop ((formals		formals-stx)
	       (arg-ids		'())
	       (type-ids	'()))
      (syntax-case formals ()
	(()
	 (syntax-case generic-function-spec ()
	   (?generic-function-id
	    (identifier? #'?generic-function-id)
	    (with-syntax ((TABLE-KEY	table-key)
			  ((ARG ...)	(reverse arg-ids))
			  ((TYPE ...)	(reverse type-ids))
			  (BODY		body-stx))
	      #'(define dummy ;to make it a definition
		  (add-method ?generic-function-id TABLE-KEY (TYPE ...)
			      (type.method-lambda ((ARG TYPE) ...) . BODY)))))
	   ((?generic-function-id ?tag0 ?tag ...)
	    (all-identifiers? #'(?generic-function-id ?tag0 ?tag ...))
	    (with-syntax ((TABLE-KEY	table-key)
			  ((ARG ...)	(reverse arg-ids))
			  ((TYPE ...)	(reverse type-ids))
			  (BODY		body-stx))
	      #'(define dummy ;to make it a definition
		  (add-method ?generic-function-id TABLE-KEY (TYPE ...)
			      (type.method-lambda ((ARG TYPE) ...) (aux.<- ?tag0 ?tag ...) . BODY)))))
	   ))
	(((?arg ?type) . ?formals)
	 (loop #'?formals (cons #'?arg arg-ids) (cons #'?type    type-ids)))
	((?arg . ?formals)
	 (loop #'?formals (cons #'?arg arg-ids) (cons #'type.top type-ids)))
	(?stuff
	 (synner "invalid formal arguments in method definition" #'?stuff)))))
  (define synner
    (case-lambda
     ((message)
      (synner message #f))
     ((message subform)
      (syntax-violation who message stx subform))))
  (syntax-case stx (aux.:primary aux.:before aux.:after aux.:around)
    ((_ aux.:primary (?generic-function . ?formals) . ?body)
     (main #'?generic-function #'aux.:primary #'?formals #'?body))
    ((_ aux.:primary ?generic-function ?formals . ?body)
     (main #'?generic-function #'aux.:primary #'?formals #'?body))
    ((_ ?generic-function aux.:primary ?formals . ?body)
     (main #'?generic-function #'aux.:primary #'?formals #'?body))

    ((_ aux.:before (?generic-function . ?formals) . ?body)
     (main #'?generic-function #'aux.:before #'?formals #'?body))
    ((_ aux.:before ?generic-function ?formals . ?body)
     (main #'?generic-function #'aux.:before #'?formals #'?body))
    ((_ ?generic-function aux.:before ?formals . ?body)
     (main #'?generic-function #'aux.:before #'?formals #'?body))

    ((_ aux.:after (?generic-function . ?formals) . ?body)
     (main #'?generic-function #'aux.:after #'?formals #'?body))
    ((_ aux.:after ?generic-function ?formals . ?body)
     (main #'?generic-function #'aux.:after #'?formals #'?body))
    ((_ ?generic-function aux.:after ?formals . ?body)
     (main #'?generic-function #'aux.:after #'?formals #'?body))

    ((_ aux.:around (?generic-function . ?formals) . ?body)
     (main #'?generic-function #'aux.:around #'?formals #'?body))
    ((_ aux.:around ?generic-function ?formals . ?body)
     (main #'?generic-function #'aux.:around #'?formals #'?body))
    ((_ ?generic-function aux.:around ?formals . ?body)
     (main #'?generic-function #'aux.:around #'?formals #'?body))

    ((_ (?generic-function . ?formals) . ?body)
     (main #'?generic-function #'aux.:primary #'?formals #'?body))
    ((_ ?generic-function ?formals . ?body)
     (main #'?generic-function #'aux.:primary #'?formals #'?body))

    (_
     (synner "invalid syntax in method definition"))))


(define-syntax (add-method stx)
  (define who 'add-method)
  (define synner
    (case-lambda
     ((message)
      (synner message #f))
     ((message subform)
      (syntax-violation who message stx subform))))
  (syntax-case stx ()
    ((_ ?generic-function ?keyword (?type-id ...) ?closure)
     (all-identifiers? #'(?generic-function ?keyword ?type-id ...))
     #`(?generic-function #,(help.case-identifier #'?keyword
			      ((aux.:primary)	#':primary-method-add)
			      ((aux.:before)	#':before-method-add)
			      ((aux.:after)	#':after-method-add)
			      ((aux.:around)	#':around-method-add)
			      (else
			       (synner "invalid generic function kind" #'?keyword)))
			  (list (type.uid-list ?type-id) ...) ;this is the signature
			  ?closure))

    ((_ ?generic-function (?type-id ...) ?closure)
     (all-identifiers? #'(?generic-function ?type-id ...))
     #'(?generic-function :primary-method-add
			  (list (type.uid-list ?type-id) ...) ;this is the signature
			  ?closure))
    ))


;;;; done

)

;;; end of file
;;Local Variables:
;; coding: utf-8
;;eval: (put 'help.case-identifier 'scheme-indent-function 1)
;;End:
