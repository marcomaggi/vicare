;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: utility syntaxes
;;;Date: Fri Oct 21, 2011
;;;
;;;Abstract
;;;
;;;	This library is both  installed and used when expanding Vicare's
;;;	own source code.  For this  reason it must export only: bindings
;;;	imported  by Vicare itself,  syntaxes whose  expansion reference
;;;	only bindings imported by Vicare itself.
;;;
;;;Copyright (C) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;;copyright notice for RECEIVE
;;;
;;;Copyright (C) John David Stone (1999). All Rights Reserved.
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
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!r6rs
(library (vicare language-extensions syntaxes)
  (export
    ;; miscellaneous extensions
    define-inline		define-inline-constant
    define-constant		define-values
    define-integrable
    define-struct-extended	define-record-type-extended
    define-syntax*		define-auxiliary-syntaxes
    let-inline			let*-inline
    debug-assert		unwind-protect
    begin0			begin0-let
    with-pathnames
    with-bytevectors		with-bytevectors/or-false
    callet			callet*
    receive
    define-exact-integer->symbol-function

    ;; arguments validation
    define-argument-validation
    with-arguments-validation
    with-dangerous-arguments-validation
    arguments-validation-forms

    ;; specialised CASE
    case-word-size		case-endianness
    case-fixnums		$case-fixnums
    case-integers		$case-integers
    case-symbols		$case-symbols
    case-chars			$case-chars
    case-strings		$case-strings

    ;; miscellaneous dispatching
    cond-numeric-operand	cond-real-numeric-operand
    cond-exact-integer-operand	cond-inexact-integer-operand
    cond-exact-real-numeric-operand

    ;; auxiliary syntaxes
    big				little)
  (import (ikarus)
    (for (prefix (vicare platform configuration)
		 config.)
	 expand)
    (only (vicare arguments validation)
	  define-argument-validation
	  with-arguments-validation
	  with-dangerous-arguments-validation
	  arguments-validation-forms
	  exact-integer.vicare-arguments-validation)
    (prefix (vicare unsafe operations)
	    $))


;;;; some defining syntaxes

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))

(define-syntax define-integrable
  ;;Posted  by  "leppie" on  the  Ikarus  mailing list;  subject  "Macro
  ;;Challenge of Last Year [Difficulty: *****]", 20 Oct 2009.
  ;;
  (lambda (x)
    (define (make-residual-name name)
      (datum->syntax name
		     (string->symbol
		      (string-append "residual-"
				     (symbol->string (syntax->datum name))))))
    (syntax-case x (lambda)
      ((_ (?name . ?formals) ?form1 ?form2 ...)
       (identifier? #'?name)
       #'(define-integrable ?name (lambda ?formals ?form1 ?form2 ...)))

      ((_ ?name (lambda ?formals ?form1 ?form2 ...))
       (identifier? #'?name)
       (with-syntax ((XNAME (make-residual-name #'?name)))
	 #'(begin
	     (define-fluid-syntax ?name
	       (lambda (x)
		 (syntax-case x ()
		   (_
		    (identifier? x)
		    #'XNAME)

		   ((_ arg (... ...))
		    #'((fluid-let-syntax
			   ((?name (identifier-syntax XNAME)))
			 (lambda ?formals ?form1 ?form2 ...))
		       arg (... ...))))))

	     (define XNAME
	       (fluid-let-syntax ((?name (identifier-syntax XNAME)))
		 (lambda ?formals ?form1 ?form2 ...))))))
      )))

(define-syntax define-constant
  (syntax-rules ()
    ((_ ?name ?expr)
     (begin
       (define ghost ?expr)
       (define-syntax ?name
	 (identifier-syntax ghost))))))

#;(define-syntax define-inline-constant
  (syntax-rules ()
    ((_ ?name ?value)
     (define-syntax ?name (identifier-syntax ?value)))))
(define-syntax define-inline-constant
  ;;We want to allow a generic expression to generate the constant value
  ;;at expand time.
  ;;
  (syntax-rules ()
    ((_ ?name ?expr)
     (define-syntax ?name
       (let ((const ?expr))
	 (lambda (stx)
	   (syntax-case stx ()
	     (?id
	      (identifier? #'?id)
	      (with-syntax ((VALUE const))
		#'(quote VALUE))))))))))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (?who ?stx) . ?body)
     (define-syntax ?who (lambda (?stx) . ?body)))))

(define-syntax define-auxiliary-syntaxes
  (syntax-rules ()
    ((_ ?name)
     (define-syntax ?name (syntax-rules ())))
    ((_ ?name0 ?name ...)
     (begin
       (define-syntax ?name0 (syntax-rules ()))
       (define-auxiliary-syntaxes ?name ...)))
    ((_)	;allows this  syntax to be called with  no arguments and
		;still expand to a definition
     (define-syntax dummy (syntax-rules ())))
    ))

;;; --------------------------------------------------------------------

(define-syntax let-inline
  (syntax-rules ()
    ((_ ((?var ?expr) ...) ?body0 . ?body)
     (let-syntax ((?var (identifier-syntax ?expr)) ...)
       ?body0 . ?body))))

(define-syntax let*-inline
  (syntax-rules ()
    ((_ () ?body0 . ?body)
     (begin ?body0 . ?body))
    ((_ ((?var0 ?expr0) (?var ?expr) ...) ?body0 . ?body)
     (let-syntax ((?var0 (identifier-syntax ?expr0)))
       (let*-inline ((?var ?expr) ...)
	 ?body0 . ?body)))))

(define-syntax callet
  ;;Transforms:
  ;;
  ;;   (callet printf (string "ciao ~a") (arg 123))
  ;;
  ;;into:
  ;;
  ;;   (printf "ciao ~a" 123)
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?func ?arg ...)
       (let loop ((args		#'(?arg ...))
		  (keys		'())
		  (exprs	'()))
	 (syntax-case args ()
	   (()
	    #`(?func . #,(reverse exprs)))
	   (((?key ?expr) . ?args)
	    (identifier? #'?key)
	    (loop #'?args
		  (cons #'?key  keys)
		  (cons #'?expr exprs)))
	   (((?key ?expr) . ?args)
	    (syntax-violation 'callet
	      "expected identifier as argument key" stx #'(?key ?expr)))
	   ((?arg . ?args)
	    (loop #'?args
		  (cons (car (generate-temporaries '(#f))) keys)
		  (cons #'?arg exprs)))
	   ))))))

(define-syntax callet*
  ;;Transforms:
  ;;
  ;;   (callet printf (string "ciao ~a") (arg 123))
  ;;
  ;;into:
  ;;
  ;;   (let* ((string "ciao ~a")
  ;;          (arg    123))
  ;;     (printf string arg))
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?func ?arg ...)
       (let loop ((args		#'(?arg ...))
		  (keys		'())
		  (exprs	'()))
	 (syntax-case args ()
	   (()
	    (with-syntax (((KEY  ...) (reverse keys))
			  ((EXPR ...) (reverse exprs)))
	      #`(let* ((KEY EXPR) ...)
		  (?func KEY ...))))
	   (((?key ?expr) . ?args)
	    (identifier? #'?key)
	    (loop #'?args
		  (cons #'?key  keys)
		  (cons #'?expr exprs)))
	   (((?key ?expr) . ?args)
	    (syntax-violation 'callet
	      "expected identifier as argument key" stx #'(?key ?expr)))
	   ((?arg . ?args)
	    (loop #'?args
		  (cons (car (generate-temporaries '(#f))) keys)
		  (cons #'?arg exprs)))
	   ))))))

(define-syntax define-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ (?var ... ?var0) ?form0 ?form ...)
       (with-syntax (((VAR ... VAR0) (generate-temporaries #'(?var ... ?var0))))
	 #'(begin
	     ;;We  must make  sure that  the ?FORMs  do not  capture the
	     ;;?VARs.
	     (define (dummy)
	       ?form0 ?form ...)
	     (define ?var  #f)
	     ...
	     (define ?var0
	       (let-values (((VAR ... VAR0) (dummy)))
		 (set! ?var  VAR)
		 ...
		 VAR0))))))))


;;;; extended struct definition

(define-syntax define-struct-extended
  ;;Like DEFINE-STRUCT but define  also argument validators, an optional
  ;;printer and an optional destructor.
  ;;
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((?kwd ?type-id (?field-id ...))
	 #'(?kwd ?type-id (?field-id ...) #f #f))
	((_ ?type-id (?field-id ...) ?printer ?destructor)
	 (and (identifier? #'?type-id)
	      (for-all identifier? (syntax->list #'(?field-id ...))))
	 (let* ((type-id	#'?type-id)
		(type-str	(%id->string type-id)))
	   (with-syntax
	       ((TYPE-PRED
		 (%id->id type-id (lambda (type-str)
				    (string-append type-str "?"))))
		(TYPE-VALIDATOR
		 type-id)
		(TYPE-VALIDATOR-MESSAGE
		 (string-append "expected struct instance of type \""
				type-str
				"\" as argument"))
		(FALSE-OR-TYPE-VALIDATOR
		 (%id->id type-id (lambda (type-str)
				    (string-append "false-or-" type-str))))
		(FALSE-OR-TYPE-VALIDATOR-MESSAGE
		 (string-append "expected false or struct instance of type \""
				type-str
				"\" as argument"))
		((PRINTER-REGISTRATION ...)
		 (if (identifier? #'?printer)
		     #'((module ()
			  (set-rtd-printer! (type-descriptor ?type-id)
					    ?printer)))
		   #'()))
		((DESTRUCTOR-REGISTRATION ...)
		 (if (identifier? #'?destructor)
		     #'((module ()
			  (set-rtd-destructor! (type-descriptor ?type-id)
					       ?destructor)))
		   #'())))
	     #'(begin
		 (define-struct ?type-id
		   (?field-id ...))

		 (define-argument-validation (TYPE-VALIDATOR who obj)
		   (TYPE-PRED obj)
		   (assertion-violation who TYPE-VALIDATOR-MESSAGE obj))

		 (define-argument-validation (FALSE-OR-TYPE-VALIDATOR who obj)
		   (or (not obj) (TYPE-PRED obj))
		   (assertion-violation who FALSE-OR-TYPE-VALIDATOR-MESSAGE obj))
		 PRINTER-REGISTRATION ...
		 DESTRUCTOR-REGISTRATION ...
		 ))))
	))

    (define (%id->id src-id string-maker)
      (datum->syntax src-id (string->symbol (string-maker (%id->string src-id)))))

    (define (%id->string id)
      (symbol->string (syntax->datum id)))

    (define (syntax->list stx)
      (syntax-case stx ()
	((?car . ?cdr)
	 (cons #'?car (syntax->list #'?cdr)))
	(() '())))

    (let ((out (main stx)))
      #;(pretty-print (syntax->datum out) (current-error-port))
      out)))


;;;; extended R6RS record type definition

(define-syntax define-record-type-extended
  ;;Like DEFINE-RECORD-TYPE but define also argument validators.
  ;;
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ?type-id . ?body)
	 (identifier? #'?type-id)
	 (output #'?type-id #'?type-id #'?body))

	((_ (?name ?constructor ?predicate) . ?body)
	 (and (identifier? #'?name)
	      (identifier? #'?constructor)
	      (identifier? #'?predicate))
	 (output #'?name #'(?name ?constructor ?predicate) #'?body))))

    (define (output type-id type-spec-stx body-stx)
      (let ((type-str (%id->string type-id)))
	(with-syntax
	    ((TYPE-PRED
	      (%id->id type-id (lambda (type-str)
				 (string-append type-str "?"))))
	     (TYPE-VALIDATOR
	      type-id)
	     (TYPE-VALIDATOR-MESSAGE
	      (string-append "expected struct instance of type \""
			     type-str
			     "\" as argument"))
	     (FALSE-OR-TYPE-VALIDATOR
	      (%id->id type-id (lambda (type-str)
				 (string-append "false-or-" type-str))))
	     (FALSE-OR-TYPE-VALIDATOR-MESSAGE
	      (string-append "expected false or struct instance of type \""
			     type-str
			     "\" as argument")))
	  #`(begin
	      (define-record-type #,type-spec-stx
		. #,body-stx)
	      (define-argument-validation (TYPE-VALIDATOR who obj)
		(TYPE-PRED obj)
		(assertion-violation who TYPE-VALIDATOR-MESSAGE obj))
	      (define-argument-validation (FALSE-OR-TYPE-VALIDATOR who obj)
		(or (not obj) (TYPE-PRED obj))
		(assertion-violation who FALSE-OR-TYPE-VALIDATOR-MESSAGE obj))
	      ))))


    (define (%id->id src-id string-maker)
      (datum->syntax src-id (string->symbol (string-maker (%id->string src-id)))))

    (define (%id->string id)
      (symbol->string (syntax->datum id)))

    (define (syntax->list stx)
      (syntax-case stx ()
	((?car . ?cdr)
	 (cons #'?car (syntax->list #'?cdr)))
	(() '())))

    (let ((out (main stx)))
      #;(pretty-print (syntax->datum out) (current-error-port))
      out)))


;;;; other syntaxes

(define-syntax begin0
  ;;This  syntax  comes from  the  R6RS  original  document, Appendix  A
  ;;``Formal semantics''.
  (syntax-rules ()
    ((_ ?expr0 ?expr ...)
     (call-with-values
	 (lambda () ?expr0)
       (lambda args
	 ?expr ...
	 (apply values args))))))

(define-syntax begin0-let
  (syntax-rules ()
    ((_ ((?var0 ?init0) (?var ?init) ...) ?form0 ?form ...)
     (let ((?var0 ?init0)
	   (?var  ?init)
	   ...)
       ?form0 ?form ...
       ?var0))))

(define-syntax unwind-protect
  ;;Not a  general UNWIND-PROTECT for Scheme,  but fine where  we do not
  ;;use continuations to escape from the body.
  ;;
  (syntax-rules ()
    ((_ ?body ?cleanup0 ?cleanup ...)
     (let ((cleanup (lambda () ?cleanup0 ?cleanup ...)))
       (with-exception-handler
	   (lambda (E)
	     (cleanup)
	     (raise E))
	 (lambda ()
	   (begin0
	       ?body
	     (cleanup))
	   #;(call-with-values
	       (lambda () ?body)
	     (lambda return-values
	       (cleanup)
	       (apply values return-values)))
	   ))))))

(define-syntax debug-assert
  ;;This is meant to expand to nothing when debugging is turned off.
  ;;
  (if #t
      (syntax-rules ()
  	((_ ?pred)
  	 (assert ?pred)))
    (syntax-rules ()
      ((_ ?pred)
       (values)))))

(define-syntax with-pathnames
  (syntax-rules ()
    ((_ ((?pathname.bv ?pathname) ...) . ?body)
     (let ((?pathname.bv (let ((pathname ?pathname))
			   (if (bytevector? pathname)
			       pathname
			     ((string->filename-func) pathname))))
	   ...)
       . ?body))))

(define-syntax with-bytevectors
  ;;Used to  preprocess function arguments which must  be bytevectors or
  ;;strings;  the  strings are  converted  to  bytevectors.  This  macro
  ;;assumes that the arguments have already been validated.
  ;;
  ;;The ?VALUE.BV and ?VALUE input forms must be identifiers.
  ;;
  (syntax-rules ()
    ((_ ((?value.bv ?value) ...) . ?body)
     (let ((?value.bv (let ((V ?value))
			(if (bytevector? V)
			    V
			  (string->latin1 V))))
	   ...)
       . ?body))))

(define-syntax with-bytevectors/or-false
  ;;Used  to preprocess  function arguments  which must  be bytevectors,
  ;;strings or  false; the strings  are converted to  bytevectors.  This
  ;;macro assumes that the arguments have already been validated.
  ;;
  ;;The ?VALUE.BV and ?VALUE input forms must be identifiers.
  ;;
  (syntax-rules ()
    ((_ ((?value.bv ?value) ...) . ?body)
     (let ((?value.bv (let ((V ?value))
			(cond ((bytevector? V)
			       V)
			      ((string? V)
			       (string->latin1 V))
			      (else V))))
	   ...)
       . ?body))))

(define-syntax receive
  (syntax-rules ()
    ((_ ?formals ?expression ?form0 ?form ...)
     (call-with-values
	 (lambda () ?expression)
       (lambda ?formals ?form0 ?form ...)))))


(define-syntax case-word-size
  (if (= 4 config.wordsize)
      (syntax-rules ()
	((_ ((32) . ?body-32) ((64) . ?body-64))
	 (begin . ?body-32)))
    (syntax-rules ()
      ((_ ((32) . ?body-32) ((64) . ?body-64))
       (begin . ?body-64)))))

(define-syntax big	(syntax-rules ()))
(define-syntax little	(syntax-rules ()))

(define-syntax case-endianness
  (lambda (stx)
    (syntax-case stx (big little)
      ((case-endianness (?who ?endianness)
	 ((little)	. ?lit-body)
	 ((big)		. ?big-body))
       (and (identifier? #'?who)
	    (identifier? #'?endianness))
       #'(case-endianness (?who ?endianness)
	   ((big)	. ?big-body)
	   ((little)	. ?lit-body)))

      ((case-endianness (?who ?endianness)
	 ((big)		. ?big-body)
	 ((little)	. ?lit-body))
       (and (identifier? #'?who)
	    (identifier? #'?endianness))
       #'(case ?endianness
	   ((big)	. ?big-body)
	   ((little)	. ?lit-body)
	   (else
	    (assertion-violation ?who "expected endianness symbol as argument" ?endianness)))))))


;;;; specialised CASE

;;Here we define  couples of type-specialised CASE  syntaxes.  For every
;;type two syntaxes are defined.
;;
;;The syntaxes with name prefixed by $  do not check that the ?DATUM are
;;of the expected type and  evaluate them once, allowing for expressions
;;to be  used.  This is dangerous  but useful to allow  an identifier in
;;reference position to be used as ?DATUM.

(define-syntax define-typed-case
  (syntax-rules ()
    ((_ ?who ?unsafe-who
	?type-pred ?unsafe-type= ?type-name-string
	?custom-definition ...)
     (begin
       (define-syntax ?who
	 (lambda (stx)
	   (define who (quote ?who))
	   ?custom-definition ...
	   (define (%assert-all-datums LL)
	     (for-each
		 (lambda (L)
		   (for-each
		       (lambda (S)
			 (unless (?type-pred S)
			   (syntax-violation who
			     (string-append "expected " ?type-name-string " as datum")
			     L S)))
		     L))
	       (syntax->datum LL)))
	   (syntax-case stx (else)
	     ((_ ?expr
		 ((?datum0 ?datum (... ...))
		  ?datum-body0 ?datum-body (... ...))
		 (... ...)
		 (else
		  ?else-body0 ?else-body (... ...)))
	      (begin
		(%assert-all-datums #'((?datum0 ?datum (... ...)) (... ...)))
		#'(let ((key ?expr))
		    (cond ((or (?unsafe-type= ?datum0 key)
			       (?unsafe-type= ?datum  key)
			       (... ...))
			   ?datum-body0 ?datum-body (... ...))
			  (... ...)
			  (else
			   ?else-body0 ?else-body (... ...))))))
	     ((_ ?expr
		 ((?datum0 ?datum (... ...))
		  ?datum-body0 ?datum-body (... ...))
		 (... ...))
	      (begin
		(%assert-all-datums #'((?datum0 ?datum (... ...)) (... ...)))
		#'(let ((key ?expr))
		    (cond ((or (?unsafe-type= ?datum0 key)
			       (?unsafe-type= ?datum  key)
			       (... ...))
			   ?datum-body0 ?datum-body (... ...))
			  (... ...)))))
	     )))
       (define-syntax ?unsafe-who
	 (syntax-rules (else)
	   ((_ ?expr
	       ((?datum0 ?datum (... ...))
		?datum-body0 ?datum-body (... ...))
	       (... ...)
	       (else
		?else-body0 ?else-body (... ...)))
	    (let ((key ?expr))
	      (cond ((or (?unsafe-type= ?datum0 key)
			 (?unsafe-type= ?datum  key)
			 (... ...))
		     ?datum-body0 ?datum-body (... ...))
		    (... ...)
		    (else
		     ?else-body0 ?else-body (... ...)))))
	   ((_ ?expr
	       ((?datum0 ?datum (... ...))
		?datum-body0 ?datum-body (... ...))
	       (... ...))
	    (let ((key ?expr))
	      (cond ((or (?unsafe-type= ?datum0 key)
			 (?unsafe-type= ?datum  key)
			 (... ...))
		     ?datum-body0 ?datum-body (... ...))
		    (... ...))))
	   )))
     )))

(define-typed-case case-fixnums $case-fixnums
  fixnum? $fx= "fixnum")

(define-typed-case case-integers $case-integers
  %exact-integer? = "exact integer"
  (define (%exact-integer? obj)
    (or (fixnum? obj)
	(bignum? obj))))

(define-typed-case case-chars $case-chars
  char? $char= "char")

(define-typed-case case-strings $case-strings
  string? $string= "string")

;;; --------------------------------------------------------------------

;;Symbols are differents because they need to be quoted.

(define-syntax case-symbols
  (lambda (stx)
    (define who (quote case-symbols))
    (define (%assert-all-datums LL)
      (for-each
	  (lambda (L)
	    (for-each
		(lambda (S)
		  (unless (symbol? S)
		    (syntax-violation who
		      (string-append "expected symbol as datum")
		      L S)))
	      L))
	(syntax->datum LL)))
    (syntax-case stx (else)
      ((_ ?expr
	  ((?datum0 ?datum ...)
	   ?datum-body0 ?datum-body ...)
	  ...
	  (else
	   ?else-body0 ?else-body ...))
       (begin
	 (%assert-all-datums #'((?datum0 ?datum ...) ...))
	 #'(let ((key ?expr))
	     (cond ((or (eq? (quote ?datum0) key)
			(eq? (quote ?datum)  key)
			...)
		    ?datum-body0 ?datum-body ...)
		   ...
		   (else
		    ?else-body0 ?else-body ...)))))
      ((_ ?expr
	  ((?datum0 ?datum ...)
	   ?datum-body0 ?datum-body ...)
	  ...)
       (begin
	 (%assert-all-datums #'((?datum0 ?datum ...) ...))
	 #'(let ((key ?expr))
	     (cond ((or (eq? (quote ?datum0) key)
			(eq? (quote ?datum)  key)
			...)
		    ?datum-body0 ?datum-body ...)
		   ...))))
      )))

(define-syntax $case-symbols
  (syntax-rules (else)
    ((_ ?expr
	((?datum0 ?datum ...)
	 ?datum-body0 ?datum-body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (let ((key ?expr))
       (cond ((or (eq? (quote ?datum0) key)
		  (eq? (quote ?datum)  key)
		  ...)
	      ?datum-body0 ?datum-body ...)
	     ...
	     (else
	      ?else-body0 ?else-body ...))))
    ((_ ?expr
	((?datum0 ?datum ...)
	 ?datum-body0 ?datum-body ...)
	...)
     (let ((key ?expr))
       (cond ((or (eq? (quote ?datum0) key)
		  (eq? (quote ?datum)  key)
		  ...)
	      ?datum-body0 ?datum-body ...)
	     ...)))
    ))


;;;; math functions dispatching

(define-syntax cond-exact-integer-operand
  (syntax-rules (else fixnum? bignum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     (else		?body-el0 ?body-el ...))))))

(define-syntax cond-inexact-integer-operand
  (syntax-rules (else fixnum? bignum? flonum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     (else		?body-el0 ?body-el ...))))))

(define-syntax cond-exact-real-numeric-operand
  (syntax-rules (else fixnum? bignum? ratnum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rn0 ?body-rn ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rn0 ?body-rn ...)
	     (else		?body-el0 ?body-el ...))))))

;;; --------------------------------------------------------------------

(define-syntax cond-real-numeric-operand
  (syntax-rules (else fixnum? bignum? ratnum? flonum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     (else		?body-el0 ?body-el ...))))

    ;;As above but with flonums before ratnums.
    ;;
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     (else		?body-el0 ?body-el ...))))

    ;;As above but with flonums first.
    ;;
    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     (else		?body-el0 ?body-el ...))))
    ))

;;; --------------------------------------------------------------------

(define-syntax cond-numeric-operand
  (syntax-rules (else
		 zero? exact? inexact?
		 fixnum? bignum? ratnum? flonum? compnum? cflonum? real? complex?)

    ;;Dispatch for all the numeric types.
    ;;
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     (else		?body-el0 ?body-el ...))))

    ;;Dispatch for  all the  numeric types,  but flonums  before ratnums
    ;;because they are more likely.
    ;;
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     (else		?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ;;Dispatch for all the numeric types, but flonums first because they
    ;;are most likely.
    ;;
    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     (else		?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ((_ ?num
	((real?)	?body-re0 ?body-re ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((or (fixnum?  num)
		  (bignum?  num)
		  (ratnum?  num)
		  (flonum?  num))
	      ?body-re0 ?body-re ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     (else		?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((zero?)	?body-zr0 ?body-zr ...)
	((real? exact?)	?body-re0 ?body-re ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (import (only (ikarus system $fx)
		     $fxzero?))
       (cond ((flonum? num)
	      ?body-fl0 ?body-fl ...)
	     ((and (fixnum? num) ($fxzero? num))
	      ?body-zr0 ?body-zr ...)
	     ((or (fixnum? num) (bignum? num) (ratnum? num))
	      ?body-re0 ?body-re ...)
	     (else
	      ?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((real? exact?)	?body-re0 ?body-re ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((flonum? num)
	      ?body-fl0 ?body-fl ...)
	     ((or (fixnum? num) (bignum? num) (ratnum? num))
	      ?body-re0 ?body-re ...)
	     (else
	      ?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ;;Dispatch for all the numeric types, complex first.
    ;;
    ((_ ?num
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     (else		?body-el0 ?body-el ...))))
    ))

;;; --------------------------------------------------------------------

;; (define-syntax fixnum	(syntax-rules ()))
;; (define-syntax bignum	(syntax-rules ()))
;; (define-syntax flonum	(syntax-rules ()))
;; (define-syntax cflonum	(syntax-rules ()))
;; (define-syntax compnum	(syntax-rules ()))

;; (define-syntax case-one-operand
;;   (syntax-rules (fixnum bignum flonum cflonum compnum)
;;     ((case-one-operand (?who ?op)
;;        ((fixnum)	. ?fixnum-body)
;;        ((bignum)	. ?bignum-body)
;;        ((flonum)	. ?flonum-body)
;;        ((cflonum)	. ?cflonum-body)
;;        ((compnum)	. ?compnum-body))
;;      (let ((op ?op))
;;        (cond ((fixnum?  op)	. ?fixnum-body)
;; 	     ((bignum?  op)	. ?bignum-body)
;; 	     ((flonum?  op)	. ?flonum-body)
;; 	     ((cflonum? op)	. ?cflonum-body)
;; 	     ((compnum? op)	. ?compnum-body)
;; 	     (else
;; 	      (assertion-violation ?who "invalid numeric operand" op)))))))

;; (define-syntax case-two-operands
;;   (syntax-rules (fixnum bignum flonum cflonum compnum)
;;     ((case-two-operands (?who ?op1 ?op2)
;;        ((fixnum)
;; 	((fixnum)	. ?fixnum/fixnum-body)
;; 	((bignum)	. ?fixnum/bignum-body)
;; 	((flonum)	. ?fixnum/flonum-body)
;; 	((cflonum)	. ?fixnum/cflonum-body)
;; 	((compnum)	. ?fixnum/compnum-body))
;;        ((bignum)
;; 	((fixnum)	. ?bignum/fixnum-body)
;; 	((bignum)	. ?bignum/bignum-body)
;; 	((flonum)	. ?bignum/flonum-body)
;; 	((cflonum)	. ?bignum/cflonum-body)
;; 	((compnum)	. ?bignum/compnum-body))
;;        ((flonum)
;; 	((fixnum)	. ?flonum/fixnum-body)
;; 	((bignum)	. ?flonum/bignum-body)
;; 	((flonum)	. ?flonum/flonum-body)
;; 	((cflonum)	. ?flonum/cflonum-body)
;; 	((compnum)	. ?flonum/compnum-body))
;;        ((cflonum)
;; 	((fixnum)	. ?cflonum/fixnum-body)
;; 	((bignum)	. ?cflonum/bignum-body)
;; 	((flonum)	. ?cflonum/flonum-body)
;; 	((cflonum)	. ?cflonum/cflonum-body)
;; 	((compnum)	. ?cflonum/compnum-body))
;;        ((compnum)
;; 	((fixnum)	. ?compnum/fixnum-body)
;; 	((bignum)	. ?compnum/bignum-body)
;; 	((flonum)	. ?compnum/flonum-body)
;; 	((cflonum)	. ?compnum/cflonum-body)
;; 	((compnum)	. ?compnum/compnum-body)))
;;      (case-one-operand (?who ?op1)
;;        ((fixnum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?fixnum/fixnum-body)
;; 	  ((bignum)	. ?fixnum/bignum-body)
;; 	  ((flonum)	. ?fixnum/flonum-body)
;; 	  ((cflonum)	. ?fixnum/cflonum-body)
;; 	  ((compnum)	. ?fixnum/compnum-body)))
;;        ((bignum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?bignum/fixnum-body)
;; 	  ((bignum)	. ?bignum/bignum-body)
;; 	  ((flonum)	. ?bignum/flonum-body)
;; 	  ((cflonum)	. ?bignum/cflonum-body)
;; 	  ((compnum)	. ?bignum/compnum-body)))
;;        ((flonum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?flonum/fixnum-body)
;; 	  ((bignum)	. ?flonum/bignum-body)
;; 	  ((flonum)	. ?flonum/flonum-body)
;; 	  ((cflonum)	. ?flonum/cflonum-body)
;; 	  ((compnum)	. ?flonum/compnum-body)))
;;        ((cflonum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?cflonum/fixnum-body)
;; 	  ((bignum)	. ?cflonum/bignum-body)
;; 	  ((flonum)	. ?cflonum/flonum-body)
;; 	  ((cflonum)	. ?cflonum/cflonum-body)
;; 	  ((compnum)	. ?cflonum/compnum-body)))
;;        ((compnum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?compnum/fixnum-body)
;; 	  ((bignum)	. ?compnum/bignum-body)
;; 	  ((flonum)	. ?compnum/flonum-body)
;; 	  ((cflonum)	. ?compnum/cflonum-body)
;; 	  ((compnum)	. ?compnum/compnum-body)))))))


;;;; miscellaneous stuff

(define-syntax define-exact-integer->symbol-function
  ;;This syntax is used  to define a function that maps  an integer to a
  ;;symbol.  It  is to be  used when  interfacing Vicare with  a foreign
  ;;library; often  such foreign  libraries define constant  integers to
  ;;drive the behaviour of functions.
  ;;
  ;;Here is an example with symbols everybody knows:
  ;;
  ;;   (define SEEK_SET 1)
  ;;   (define SEEK_CUR 2)
  ;;   (define SEEK_END 3)
  ;;
  ;;   (define-exact-integer->symbol-function whence->symbol
  ;;     (SEEK_SET SEEK_CUR SEEK_END))
  ;;
  ;;the syntax will expand to:
  ;;
  ;;   (define (whence->symbol code)
  ;;     (define who 'whence->symbol)
  ;;     (with-arguments-validation (who)
  ;;         ((exact-integer      code))
  ;;       ($case-integers code
  ;;         ((SEEK_SET)     'SEEK_SET)
  ;;         ((SEEK_CUR)     'SEEK_CUR)
  ;;         ((SEEK_END)     'SEEK_END)
  ;;         (else #f))))
  ;;
  (syntax-rules ()
    ((_ ?who (?code ...))
     (define (?who code)
       (define who '?who)
       (with-arguments-validation (who)
	   ((exact-integer	code))
	 ($case-integers code
	   ((?code)	'?code)
	   ...
	   (else #f)))))))


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'case-one-operand 'scheme-indent-function 1)
;;eval: (put 'case-two-operands 'scheme-indent-function 1)
;;eval: (put 'case-integers 'scheme-indent-function 1)
;;End:
