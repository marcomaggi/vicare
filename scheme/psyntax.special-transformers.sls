;;; -*- coding: utf-8-unix -*-
;;;
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (psyntax.special-transformers)
  (export
    make-variable-transformer
    variable-transformer?
    variable-transformer-procedure

    make-synonym-transformer
    synonym-transformer?
    synonym-transformer-identifier

    make-expand-time-value
    expand-time-value?
    expand-time-value-object)
  (import (except (vicare)
		  make-variable-transformer
		  variable-transformer?
		  variable-transformer-procedure

		  make-synonym-transformer
		  synonym-transformer?
		  synonym-transformer-identifier

		  make-expand-time-value
		  expand-time-value?
		  expand-time-value-object))


;;;; public interface: variable transformer
;;
;;As specified by R6RS: we can  define identifier syntaxes with IDENTIFIER-SYNTAX and
;;with MAKE-VARIABLE-TRANSFORMER; both  of these return a "special"  value that, when
;;used as right-hand side of a syntax  definition, is recognised by the expander as a
;;variable transformer as opposed to a normal transformer or a compile-time value.
;;
;;Let's say we define an identifier syntax with:
;;
;;   (define-syntax ?kwd ?expression)
;;
;;where ?EXPRESSION is:
;;
;;   (identifier-syntax ?stuff)
;;
;;here is what happen:
;;
;;1..The DEFINE-SYNTAX form is expanded and a syntax object is created:
;;
;;      (syntax ?expression)
;;
;;2..The syntax object  is expanded by %EXPAND-MACRO-TRANSFORMER and the  result is a
;;   core language sexp representing the transformer.
;;
;;3..The sexp is compiled and evaluated by the function %EVAL-MACRO-TRANSFORMER.  The
;;   result of the evaluation is a "special value" with format:
;;
;;      (identifier-macro! . ?transformer)
;;
;;   where ?TRANSFORMER is a transformer function.
;;
;;4..%EVAL-MACRO-TRANSFORMER    recognises    the     value    as    special    using
;;   VARIABLE-TRANSFORMER?  and transforms it to a "local-macro!"  syntactic binding.
;;

(define* (make-variable-transformer x)
  ;;R6RS's make-variable-transformer.  Build and return  a "special" value that, when
  ;;used as right-hand side of a syntax  definition, is recognised by the expander as
  ;;a  variable transformer  as opposed  to a  normal transformer  or a  compile-time
  ;;value.
  ;;
  (if (procedure? x)
      (cons 'identifier-macro! x)
    (assertion-violation __who__ "not a procedure" x)))

(define (variable-transformer? x)
  ;;Return true  if X  is recognised  by the  expander as  a variable  transformer as
  ;;opposed to a normal transformer or a compile-time value; otherwise return false.
  ;;
  (and (pair? x)
       (eq? (car x) 'identifier-macro!)
       (procedure? (cdr x))))

(define* (variable-transformer-procedure x)
  ;;If X is recognised  by the expander as a variable  transformer: return the actual
  ;;transformer function, otherwise raise an assertion violation.
  ;;
  (if (variable-transformer? x)
      (cdr x)
    (assertion-violation __who__ "not a variable transformer" x)))


;;;; public interface: synonym transformer
;;
;;

(define* (make-synonym-transformer {x identifier?})
  ;;Build and  return a  "special" value that,  when used  as right-hand
  ;;side of  a syntax  definition, is  recognised by  the expander  as a
  ;;synonym  transformer as  opposed to  a normal  transformer, variable
  ;;transformer or a compile-time value.
  ;;
  (cons 'synonym-transformer x))

(define (synonym-transformer? x)
  ;;Return  true  if X  is  recognised  by  the  expander as  a  synonym
  ;;transformer as opposed to a normal transformer, variable transformer
  ;;or a compile-time value; otherwise return false.
  ;;
  (and (pair? x)
       (eq? (car x) 'synonym-transformer)
       (identifier? (cdr x))))

(define* (synonym-transformer-identifier {x synonym-transformer?})
  ;;If X is recognised by the  expander as a synonym transformer: return
  ;;the source identifier, otherwise raise an exception.
  ;;
  (cdr x))


;;;; public interface: expand-time values
;;
;;Expand-time values  are objects computed at  expand-time and stored in  the lexical
;;environment.   We can  define  an expand-time  value  and push  it  on the  lexical
;;environment with:
;;
;;   (define-syntax it
;;     (make-expand-time-value (+ 1 2)))
;;
;;later  we  can retrieve  it  by  defining a  transformer  function  that returns  a
;;function:
;;
;;   (define-syntax get-it
;;     (lambda (stx)
;;       (retrieve-expand-time-value #'it))) => 3
;;
;;Let's say we define a expand-time value with:
;;
;;   (define-syntax ?kwd ?expression)
;;
;;where ?EXPRESSION is:
;;
;;   (make-expand-time-value ?stuff)
;;
;;here is what happen:
;;
;;1..The DEFINE-SYNTAX form is expanded and a syntax object is created:
;;
;;      (syntax ?expression)
;;
;;2..The syntax object  is expanded by %EXPAND-MACRO-TRANSFORMER and the  result is a
;;   core language sexp representing the right-hand side.
;;
;;3..The sexp is compiled and evaluated by the function %EVAL-MACRO-TRANSFORMER.  The
;;   result of the evaluation is a "special value" with format:
;;
;;      (local-etv . ?obj)
;;
;;   where ?OBJ is the actual expand-time value.
;;
;;4..%EVAL-MACRO-TRANSFORMER    recognises    the     value    as    special    using
;;   EXPAND-TIME-VALUE?  and transforms it to a "local-etv" syntactic binding.
;;

(define (make-expand-time-value obj)
  (cons 'etv obj))

(define (expand-time-value? obj)
  (and (pair? obj)
       (eq? 'etv (car obj))))

(define expand-time-value-object
  ;;Given a expand-time value datum: return the actual expand-time object.
  ;;
  cdr)


;;;; done

#| end of library |# )

;;; end of file
