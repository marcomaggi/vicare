;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: destructuring match syntax
;;;Date: Sat Apr 20, 2013
;;;
;;;Abstract
;;;
;;;	By putting  everything in  the body of  a macro  transformer, we
;;;	have no problems in using the MATCH syntax in the boot image.
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare language-extensions simple-match)
  (export
    match #;match-debug

    ;; auxiliary keywords
    else let quote quasiquote and or not apply eval syntax ... _ =>)
  (import (ikarus)
    (vicare unsafe operations)
    (only (ikarus system $bytevectors)
	  $bytevector=))


#|
The macro use:

   (match 3
     (1
      (display 1))
     (2
      (display 2)))

is expanded to:

   (letrec ((clause0 (lambda (expr)
		       (if (and (fixnum? expr)
				(fx=? expr 1))
			   (display 1)
			 (clause1 expr))))
            (clause1 (lambda (expr)
		       (if (and (fixnum? expr)
				(fx=? expr 2))
			   (display 2)
			 (failed-match-error expr))))
            (failed-match-error
             (lambda (expr)
	       (assertion-violation 'match
		 "failed destructuring match: no matching clause"
		 expr))))
     (clause0 expr clause2))

|#


(define-syntax debugging
  (syntax-rules ()))

(define-syntax match-debug
  (syntax-rules ()
    ((_)
     (match))

    ((_ ?expr)
     (match debugging ?expr))

    ((_ ?expr ?clause0 ?clause ...)
     (match debugging ?expr ?clause0 ?clause ...))
    ))


(define-syntax match
  (lambda (stx)
    (define debug? #f)

    (define (main stx)
      (syntax-case stx (else debugging)
	((_ debugging ?expr)
	 (begin
	   (set! debug? #t)
	   #'(begin ?expr)))

	((_ debugging ?expr ?clause0 ?clause ... (else ?else-body0 ?else-body ...))
	 (begin
	   (set! debug? #t)
	   (clauses-with-else #'?expr
			      (syntax->list #'(?clause0 ?clause ...))
			      #'(?else-body0 ?else-body ...))))

	((_ debugging ?expr ?clause0 ?clause ...)
	 (begin
	   (set! debug? #t)
	   (clauses-without-else #'?expr
				 (syntax->list #'(?clause0 ?clause ...)))))

	((_)
	 #'(values))

	((_ ?expr)
	 #'(begin ?expr))

	((_ ?expr ?clause0 ?clause ... (else ?else-body0 ?else-body ...))
	 (clauses-with-else #'?expr
			    (syntax->list #'(?clause0 ?clause ...))
			    #'(?else-body0 ?else-body ...)))

	((_ ?expr ?clause0 ?clause ...)
	 (clauses-without-else #'?expr
			       (syntax->list #'(?clause0 ?clause ...))))

	))


;;;; parsing clauses

(define (clauses-without-else expr-stx clauses)
  (compose-output-form expr-stx clauses
		       #'(lambda (expr)
			   (error 'match
			     "failed destructuring match: no matching clause"
			     expr))))

(define (clauses-with-else expr-stx clauses else-body)
  (compose-output-form expr-stx clauses
		       #`(lambda (expr) . #,else-body)))

(define (compose-output-form expr-stx clauses fail-thunk-stx)
  (with-syntax (((CLAUSE-ID0 CLAUSE-ID ...) (generate-temporaries clauses)))
    (define funcs-stx
      (let recur ((clauses     clauses)
		  (clause-ids  (syntax->list #'(CLAUSE-ID ... failed-match-error))))
	(if (null? clauses)
	    '()
	  (let ((fail-form-stx #`(#,($car clause-ids) expr)))
	    (cons #`(lambda (expr)
		      #,(parse-single-clause ($car clauses) #'expr fail-form-stx))
		  (recur ($cdr clauses)
			 ($cdr clause-ids)))))))
    (with-syntax
	(((CLAUSE-THUNK0 CLAUSE-THUNK ...)
	  funcs-stx))
      #`(letrec ((CLAUSE-ID0 CLAUSE-THUNK0)
		 (CLAUSE-ID  CLAUSE-THUNK)
		 ...
		 (failed-match-error #,fail-thunk-stx))
	  (CLAUSE-ID0 #,expr-stx)))))


;;;; clause parsing

(define (parse-single-clause clause expr-id fail-form-stx)
  ;;Parse a  match CLAUSE  and return a  syntax object  representing the
  ;;code to match an expression against the pattern.  Assume the initial
  ;;expression is bound  to the identifier EXPR-ID.   If matching fails:
  ;;the generated code will evaluate  the result of expanding the syntax
  ;;object FAIL-FORM-STX.
  ;;
  (syntax-case clause (=>)
    ;;Pattern without body.
    ((?pattern)
     (build-pattern-matching-code #'?pattern expr-id
				  #'(values) fail-form-stx #f))

    ;;Pattern with body.
    ((?pattern (=> ?escape) ?body0 ?body ...)
     (begin
       (unless (identifier? #'?escape)
	 (synner "expected identifier as escape name" clause))
       (build-pattern-matching-code #'?pattern expr-id
				    #`(let ((?escape (lambda () #,fail-form-stx)))
					?body0 ?body ...)
				    fail-form-stx #f)))

    ;;Pattern with body.
    ((?pattern ?body0 ?body ...)
     (build-pattern-matching-code #'?pattern expr-id
				  #'(begin ?body0 ?body ...) fail-form-stx #f))

    ;;Syntax error.
    (_
     (synner "invalid syntax in match clause" clause))))


;;;; pattern parsing

(define (build-pattern-matching-code pattern-stx in-expr-stx
				     success-stx failure-stx
				     wrapped-body?)
  ;;Recursive  function.   Parse  the  syntax  object  representing  the
  ;;pattern  and build  the expression  needed to  match it  against the
  ;;input expression.
  ;;
  ;;SUCCESS-STX is a syntax object representing the form to be evaluated
  ;;whenever the pattern fully matches the input expression.
  ;;
  ;;FAILURE-STX is a syntax object representing the form to be evaluated
  ;;whenever matching fails.
  ;;
  ;;WRAPPED-BODY? is  used only  when constructing the  code to  match a
  ;;pattern with associated  ellipsis; it must be #f at  the first call.
  ;;It is set to true whenever the success form is wrapped in a lambda:
  ;;
  ;;  #`(lambda () #,success-stx)
  ;;
  (define (recurse pattern-stx in-expr-stx)
    (build-pattern-matching-code pattern-stx in-expr-stx
				 success-stx failure-stx
				 wrapped-body?))
  (with-syntax (((expr)        (generate-temporaries #'(#f)))
		(IN-EXPR       in-expr-stx))
    (syntax-case pattern-stx (let quote quasiquote and or not apply eval syntax)

      ;;This matches  the end of  a clause when  the clause is  a proper
      ;;list.
      ;;
      (()
       #`(let ((expr IN-EXPR))
	   (if (null? expr)
	       #,success-stx
	     #,failure-stx)))

;;; --------------------------------------------------------------------
;;; variable bindings

      ;;LET alone is forbidden.
      ;;
      (let
       (synner "invalid standalone LET pattern" pattern-stx))

      ;;Empty LET pattern.
      ;;
      ((let)
       (synner "invalid empty LET pattern" pattern-stx))

      ((let ?var ?ellipsis)
       (and (identifier? #'?var)
	    (ellipsis?   #'?ellipsis))
       (synner "invalid LET-with-ellipsis pattern outside of list or vector pattern"
	       pattern-stx))

      ;;Two or more patterns in LET.  Syntax error.
      ;;
      ((let ?pattern0 ?pattern1 ?pattern ...)
       (synner "invalid multiple subpatterns in LET clause" pattern-stx))

      ;;Bind the input expression to a variable.
      ;;
      ((let ?var)
       (identifier? #'?var)
       #`(let ((?var IN-EXPR))
	   #,success-stx))

      ;;Match a  variable binding with embedded  ellipsis.  This pattern
      ;;can  appear only  in tail  position of  a list  or vector.   The
      ;;expression must be null or a proper list.
      ;;
      (((let ?var ?ellipsis))
       (and (identifier? #'?var)
	    (ellipsis?   #'?ellipsis))
       #`(let ((?var IN-EXPR))
	   (if (list? ?var) ;includes nil
	       #,success-stx
	     #,failure-stx)))

;;; --------------------------------------------------------------------
;;; predicate application

      ;;APPLY alone is forbidden.
      ;;
      (apply
       (synner "invalid standalone APPLY pattern" pattern-stx))

      ;;Empty APPLY pattern.
      ;;
      ((apply)
       (synner "invalid empty APPLY pattern" pattern-stx))

      ;;Match with a predicate function.
      ;;
      ((apply ?pred ...)
       #`(let ((expr IN-EXPR))
	   (if (and (?pred expr) ...)
	       #,success-stx
	     #,failure-stx)))

;;; --------------------------------------------------------------------
;;; expression evaluation

      ;;EVAL alone is forbidden.
      ;;
      (eval
       (synner "invalid standalone EVAL pattern" pattern-stx))

      ;;Empty EVAL pattern.
      ;;
      ((eval)
       (synner "invalid empty EVAL pattern" pattern-stx))

      ;;Two or more patterns in EVAL.  Syntax error.
      ;;
      ((eval ?pattern0 ?pattern1 ?pattern ...)
       (synner "invalid multiple subpatterns in EVAL clause" pattern-stx))

      ;;Match with a predicate function.
      ;;
      ((eval ?expr)
       #`(let ((expr IN-EXPR))
	   (if ?expr
	       #,success-stx
	     #,failure-stx)))

;;; --------------------------------------------------------------------
;;; logic AND multiple pattern mathing

      ;;AND alone is forbidden.
      ;;
      (and
       (synner "invalid standalone AND pattern" pattern-stx))

      ;;Empty AND pattern.
      ;;
      ((and)
       success-stx)

      ;;Non-empty AND pattern.  Match multiple patterns.
      ;;
      ((and ?pattern0 ?pattern ...)
       #`(let ((expr IN-EXPR))
	   #,(build-pattern-matching-code #'?pattern0 #'expr
					  (recurse #'(and ?pattern ...) #'expr)
					  failure-stx wrapped-body?)))

;;; --------------------------------------------------------------------
;;; logic OR multiple pattern matching

      ;;OR alone is forbidden.
      ;;
      (or
       (synner "invalid standalone OR pattern" pattern-stx))

      ;;Empty OR pattern.
      ;;
      ((or)
       failure-stx)

      ;;OR with single pattern.
      ((or ?pattern)
       (recurse #'?pattern in-expr-stx))

      ;;Multi-pattern OR pattern.  Match one among multiple patterns.
      ;;
      ;;The code  in the  body is  duplicated: we  accept it  because OR
      ;;clauses are rare  and we can always put big  bodies in functions
      ;;to be called.
      ;;
      ;;Notice  how we  explicitly  check here  for  the alternative  OR
      ;;patterns to bind variables with BOUND-IDENTIFIER=?  identifiers.
      ;;Without this explicit  check, if the OR  patterns bind different
      ;;variables:
      ;;
      ;;* If the variables are referenced  in the body: an error will be
      ;;  raised by the compiler.
      ;;
      ;;* If  the variables are *not*  referenced in the body:  no error
      ;;  will be raised.
      ;;
      ;;We make sure to raise an error in whatever the body.
      ;;
      ((or ?pattern0 ?pattern1 ?pattern ...)
       (or-patterns-with-same-bindings? #'?pattern0 #'?pattern1)
       #`(let ((expr IN-EXPR))
	   #,(build-pattern-matching-code #'?pattern0 #'expr
					  success-stx
					  (recurse #'(or ?pattern1 ?pattern ...) #'expr)
					  wrapped-body?)))

;;; --------------------------------------------------------------------
;;; logic NOT

      ;;NOT alone is forbidden.
      ;;
      (not
       (synner "invalid standalone NOT pattern" pattern-stx))

      ;;Empty NOT pattern.  Syntax error
      ;;
      ((not)
       (synner "invalid empty NOT clause" pattern-stx))

      ;;Two or more patterns in NOT.  Syntax error.
      ;;
      ((not ?pattern0 ?pattern1 ?pattern ...)
       (synner "invalid multiple subpatterns in NOT clause" pattern-stx))

      ;;NOT pattern.  Matches if the pattern does not match.
      ;;
      ((not ?pattern)
       (build-pattern-matching-code #'?pattern #'IN-EXPR
				    failure-stx success-stx wrapped-body?))

;;; --------------------------------------------------------------------
;;; quotation and quasiquotation

      ;;Match a quoted datum.
      ;;
      ((quote ?datum)
       (cond ((symbol? (syntax->datum #'?datum))
	      #`(let ((expr IN-EXPR))
		  (if (eq? expr (quote ?datum))
		      #,success-stx
		    #,failure-stx)))
	     (else
	      #`(let ((expr IN-EXPR))
		  (if (equal? expr (quote ?datum))
		      #,success-stx
		    #,failure-stx)))))

      ;;Match a quasiquoted datum.
      ;;
      ((quasiquote ?datum)
       #`(let ((expr IN-EXPR))
	   (if (equal? expr (quasiquote ?datum))
	       #,success-stx
	     #,failure-stx)))

;;; --------------------------------------------------------------------
;;; syntax

      ;;Match a syntax object.
      ;;
      ((syntax ?pattern)
       #`(let ((expr IN-EXPR))
	   (syntax-case expr ()
	     (?pattern
	      #,success-stx)
	     (_
	      #,failure-stx))))

      ;;Match a syntax object.  Literals specification.
      ;;
      ((syntax ?pattern (?literal ...))
       (let ((lits #'(?literal ...)))
	 (if (for-all symbol? (syntax->datum lits))
	     #`(let ((expr IN-EXPR))
		 (syntax-case expr (?literal ...)
		   (?pattern
		    #,success-stx)
		   (_
		    #,failure-stx)))
	   (synner "expected identifiers in literals list" lits))))

;;; --------------------------------------------------------------------
;;; ellipsis

      ;;Ellipsis alone is forbidden.
      ;;
      (?ellipsis
       (ellipsis? #'?ellipsis)
       (synner "the ellipsis can appear only after another pattern" pattern-stx))

      ;;The ellipsis can appear only as last item.
      ;;
      ((?ellipsis . ?pattern)
       (ellipsis? #'?ellipsis)
       (synner "the ellipsis can appear only as last item" pattern-stx))

      ;;Match the ellipsis  in list.  The ellipsis can appear  in a list
      ;;only as last item.
      ;;
      ;;We first  match all the  items in IN-EXPR against  the ?PATTERN,
      ;;and build a  list of thunks holding the  SUCCESS-STX closed upon
      ;;the  correct  bindings; then  we  evaluate  the thunks,  in  the
      ;;correct order, and return the list of results.
      ;;
      ;;We do not  evaluate any SUCCESS-STX before we are  sure that all
      ;;the items from IN-EXPR match the  ?PATTERN.  If an item does not
      ;;match the  pattern: we evaluate  the FAILURE-STX and  return its
      ;;return value;  to do this we  create a continuation and  use its
      ;;escape function.
      ;;
      ((?pattern ?ellipsis)
       (ellipsis? #'?ellipsis)
       (with-syntax
	   (((RETURN NEXT-ITEM THUNKS RECUR THUNK)
	     (generate-temporaries #'(1 2 3 4 5))))
	 #`(call/cc
	       (lambda (RETURN)
		 (let NEXT-ITEM ((expr   IN-EXPR)
				 (THUNKS '()))
		   (cond
		    ((pair? expr)
		     ;;Build a thunk for every matching input item.
		     (NEXT-ITEM ($cdr expr)
				(cons #,(build-pattern-matching-code
					 #'?pattern #'($car expr)
					 (if wrapped-body?
					     success-stx
					   #`(lambda () #,success-stx))
					 #`(RETURN #,failure-stx)
					 #t)
				      THUNKS)))
		    ((null? expr)
		     ;;Evaluate the  thunks, if  any, in the  order they
		     ;;were  created.   Return  null   or  the  list  of
		     ;;results.
		     (let ((results (let RECUR ((THUNKS (reverse THUNKS)))
				      (if (null? THUNKS)
					  '()
					(cons (($car THUNKS))
					      (RECUR ($cdr THUNKS)))))))
		       #,(if wrapped-body?
			     #'(lambda () results)
			   #'results)))
		    (else
		     ;;Fail  if the  input  is neither  null nor  a
		     ;;proper list.
		     #,failure-stx)))))))

;;; --------------------------------------------------------------------
;;; pairs, proper lists, improper lists

      ;;Match a pair.
      ;;
      ((?car . ?cdr)
       #`(let ((expr IN-EXPR))
	   (if (pair? expr)
	       #,(build-pattern-matching-code #'?car #'($car expr)
					      (recurse #'?cdr #'($cdr expr))
					      failure-stx wrapped-body?)
	     #,failure-stx)))

;;; --------------------------------------------------------------------
;;; vectors

      ;;Match an empty vector.
      ;;
      (#()
       #`(let ((expr IN-EXPR))
	   (if (and (vector? expr) ($fxzero? ($vector-length expr)))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a non-empty  vector.  Let's keep it simple  and accept the
      ;;cost of VECTOR->LIST.
      ;;
      ;;We include a test for vector length only if the last item in the
      ;;pattern vector  is neither  an ellipsis  nor a  variable binding
      ;;with nested ellipsis.
      ;;
      (#(?item ...)
       (let ((stx #'#(?item ...)))
	 (if (vector-with-ellipsis-as-tail? stx)
	     #`(let ((expr IN-EXPR))
		 (if (vector? expr)
		     #,(recurse #'(?item ...) #'(vector->list expr))
		   #,failure-stx))
	   (with-syntax
	       ((DATUM.len ($vector-length (syntax->datum stx))))
	     #`(let ((expr IN-EXPR))
		 (if (and (vector? expr)
			  ($fx= DATUM.len ($vector-length expr)))
		     #,(recurse #'(?item ...) #'(vector->list expr))
		   #,failure-stx))))))

;;; --------------------------------------------------------------------
;;; standalone identifiers

      ;;Match anything and ignore.
      ;;
      (?underscore
       (underscore? #'?underscore)
       success-stx)

      ;;Match a variable reference.
      ;;
      (?var
       (identifier? #'?var)
       #`(let ((expr IN-EXPR))
	   (if (equal? expr ?var)
	       #,success-stx
	     #,failure-stx)))

;;; --------------------------------------------------------------------
;;; syntactic data

      ;;Match the boolean true.
      ;;
      (#t
       #`(let ((expr IN-EXPR))
	   (if (and (boolean? expr) expr) #,success-stx #,failure-stx)))

      ;;Match the boolean false.
      ;;
      (#f
       #`(let ((expr IN-EXPR))
	   (if (and (boolean? expr) (not expr)) #,success-stx #,failure-stx)))

      ;;Match a character datum.
      ;;
      (?datum
       (char? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (char? expr) ($char= expr ?datum))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a fixnum datum.
      ;;
      (?datum
       (fixnum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (fixnum? expr) ($fx= expr ?datum))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a bignum datum.
      ;;
      (?datum
       (bignum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (bignum? expr) (= expr ?datum))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a  NaN datum.  This  clause must  come before the  one for
      ;;flonums.
      ;;
      (?datum
       (let ((D (syntax->datum #'?datum)))
	 (and (number? D)
	      (nan?    D)))
       #`(if (nan? IN-EXPR)
	     #,success-stx
	   #,failure-stx))

      ;;Match an infinite  datum.  This clause must come  before the one
      ;;for flonums.
      ;;
      (?datum
       (let ((D (syntax->datum #'?datum)))
	 (and (number?   D)
	      (infinite? D)))
       #`(let ((expr IN-EXPR))
	   (if (and (infinite? expr) (= expr ?datum))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a flonum datum.
      ;;
      (?datum
       (flonum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (flonum? expr) ($fl= expr ?datum))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a ratnum datum.
      ;;
      (?datum
       (ratnum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (ratnum? expr)
		    (= ($ratnum-num expr) ($ratnum-num ?datum))
		    (= ($ratnum-den expr) ($ratnum-den ?datum)))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a cflonum datum.
      ;;
      (?datum
       (cflonum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (cflonum? expr)
		    ($fl= ($cflonum-real expr) ($cflonum-real ?datum))
		    ($fl= ($cflonum-imag expr) ($cflonum-imag ?datum)))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a compnum datum.
      ;;
      (?datum
       (compnum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (compnum? expr)
		    (= ($compnum-real expr) ($compnum-real ?datum))
		    (= ($compnum-imag expr) ($compnum-imag ?datum)))
	       #,success-stx
	     #,failure-stx)))

      ;;Match  a number  datum.  This  clause should  never be  included
      ;;because the clauses  above should have matches all  the kinds of
      ;;numbers.
      ;;
      (?datum
       (number? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (number? expr) (= expr ?datum))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a string datum.
      ;;
      (?datum
       (string? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (string? expr) (string=? expr ?datum))
	       #,success-stx
	     #,failure-stx)))

      ;;Match a bytevector datum.
      ;;
      (?datum
       (bytevector? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (bytevector? expr)
		    ($bytevector= expr (quote ?datum)))
	       #,success-stx
	     #,failure-stx)))

;;; --------------------------------------------------------------------

      (_
       (synner "invalid syntax in match pattern" pattern-stx))

      )))


;;;; bindings stuff

(module (or-patterns-with-same-bindings?)

  (define (or-patterns-with-same-bindings? pattern1 pattern2)
    (if (same-bindings-lists? (get-pattern-bindings pattern1)
			      (get-pattern-bindings pattern2))
	#t
      (synner "OR patterns defining different bindings"
	      (list pattern1 pattern2))))

  (define get-pattern-bindings
    (case-lambda
     ((pattern)
      (get-pattern-bindings pattern '()))
     ((pattern ids)
      ;;Given a pattern  extract the list of identifiers  that are bound
      ;;in case of match.
      ;;
      (syntax-case pattern (let)
	((let ?var)
	 (identifier? #'?var)
	 (cons #'?var ids))
	(((let ?var) . ?patterns)
	 (identifier? #'?var)
	 (get-pattern-bindings #'?patterns (cons #'?var ids)))
	((?pattern0 . ?patterns)
	 (get-pattern-bindings #'?patterns ids))
	(()
	 ids)))))

  (define (same-bindings-lists? ids1 ids2)
    ;;Check that the list IDS1 contains the same identifiers of the list
    ;;IDS2 in undefined order.
    ;;
    (for-all (lambda (id1)
	       (find (lambda (id2)
		       (bound-identifier=? id1 id2))
		 ids2))
      ids1))

  #| end of module: OR-PATTERNS-WITH-SAME-BINDINGS |# )


;;;; helpers

(define (ellipsis? stx)
  (and (identifier? stx)
       (free-identifier=? stx #'(... ...))))

(define (underscore? stx)
  (and (identifier? stx)
       (free-identifier=? stx #'_)))

(define (vector-with-ellipsis-as-tail? stx)
  ;;Return true if STX is a syntax  object representing a vector of 2 or
  ;;more items,  and the last  item is  an ellipsis or  variable binding
  ;;with ellipsis; else return false.
  ;;
  (syntax-case stx (let)
    (#(?item0 ?item ... (let ?var ?ellipsis))
     (ellipsis? #'?ellipsis))
    (#(?item0 ?item ... ?ellipsis)
     (ellipsis? #'?ellipsis))
    (_
     #f)))

(define (syntax->list stx)
  (syntax-case stx ()
    (()
     '())
    ((?car . ?cdr)
     (cons #'?car (syntax->list #'?cdr)))
    (_
     (synner "expected syntax object representing proper list" stx))))

(define (synner message subform)
  (syntax-violation 'match message stx subform))


;;;; end of transformer

(let ((out (main stx)))
  (when (or #f debug?)
    (pretty-print (syntax->datum out) (current-error-port)))
  out)))


;;;; done

)

;;; end of file
