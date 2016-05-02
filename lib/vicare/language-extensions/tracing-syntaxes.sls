;;; -*- coding: utf-8-unix -*-
;;;
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified in 2010-2016 by Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (vicare language-extensions tracing-syntaxes)
  (export
    make-traced-procedure
    trace-lambda
    trace-define
    trace-let
    trace-define-syntax
    trace-let-syntax
    trace-letrec-syntax)
  (import (rnrs)
    (only (psyntax system $all)
	  make-traced-procedure
	  make-traced-macro))


(define-syntax trace-let
  (syntax-rules ()
    ((_ ?recur ((?lhs ?rhs) ...) ?body0 ?body ...)
     ((letrec ((?recur (trace-lambda ?recur (?lhs ...) ?body0 ?body ...)))
	,?recur)
      ?rhs ...))
    ))

(define-syntax trace-lambda
  (syntax-rules ()
    ((_ ?who ?formals ?body0 ?body ...)
     (make-traced-procedure (quote ?who) (lambda ?formals ?body0 ?body ...)))
    ))

(define-syntax trace-define
  (syntax-rules ()
    ((_ (?who . ?formals) ?body0 ?body ...)
     (define ?who
       (make-traced-procedure (quote ?who) (lambda ?formals ?body0 ?body ...))))
    ((_ ?who ?expr)
     (define ?who
       (let ((v ?expr))
	 (if (procedure? v)
	     (make-traced-procedure (quote ?who) v)
	   v))))
    ))

(define-syntax trace-define-syntax
  (syntax-rules ()
    ((_ ?who ?expr)
     #'(define-syntax ?who
	 (make-traced-macro (quote ?who) ?expr)))
    ))

(define-syntax trace-let-syntax
  (syntax-rules ()
    ((_ ((?lhs ?rhs) ...) ?body0 ?body ...)
     (let-syntax ((?lhs (make-traced-macro (quote ?lhs) ?rhs)) ...)
       ?body0 ?body ...))
    ))

(define-syntax trace-letrec-syntax
  (syntax-rules ()
    ((_ ((?lhs ?rhs) ...) ?body0 ?body ...)
     (letrec-syntax ((?lhs (make-traced-macro (quote ?lhs) ?rhs)) ...)
       ?body0 ?body ...))
    ))


;;;; done

#| end of library |# )

;;; end of file
