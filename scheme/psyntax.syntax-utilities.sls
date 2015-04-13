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


(library (psyntax.syntax-utilities)
  (export

    ;; high-level syntax objects utilities
    generate-temporaries
    syntax-null?
    syntax-pair?			syntax-list?
    syntax-car				syntax-cdr
    syntax->list
    syntax-vector?			syntax-vector->list
    syntax-unwrap)
  (import (except (rnrs)
		  generate-temporaries)
    (psyntax.compat)
    (psyntax.syntax-match)
    (only (psyntax.lexical-environment)
	  mkstx
	  make-syntactic-identifier-for-temporary-variable
	  wrapped-syntax-object?
	  syntax-object-expression
	  syntax-object-marks
	  syntax-object-ribs
	  syntax-object-source-objects))


;;;; high-level syntax object utilities

(define* (generate-temporaries list-stx)
  (syntax-match list-stx ()
    ((?item* ...)
     (map (lambda (x)
	    (make-syntactic-identifier-for-temporary-variable
	     (if (identifier? x)
		 ;;If  it is  an identifier  we  do *not*  want  to use  its name  as
		 ;;temporary name, because  it looks ugly and  confusing when looking
		 ;;at the result of the expansion with PRINT-GENSYM set to #f.
		 (gensym 't)
	       (let ((x (syntax->datum x)))
		 (if (or (symbol? x)
			 (string? x))
		     (gensym x)
		   (gensym 't))))))
       ?item*))
    (_
     (assertion-violation __who__
       "expected list or syntax object holding a list as argument" list-stx))))

(module (syntax-pair?
	 syntax-vector?
	 syntax-null?)

  (define (syntax-pair? x)
    (syntax-kind? x pair?))

  (define (syntax-vector? x)
    (syntax-kind? x vector?))

  (define (syntax-null? x)
    (syntax-kind? x null?))

  (define (syntax-kind? x pred?)
    (cond ((wrapped-syntax-object? x)
	   (syntax-kind? (syntax-object-expression x) pred?))
	  ((annotation? x)
	   (syntax-kind? (annotation-expression x) pred?))
	  (else
	   (pred? x))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define (syntax-list? x)
  ;;FIXME Should terminate on cyclic input.  (Abdulaziz Ghuloum)
  (or (syntax-null? x)
      (and (syntax-pair? x)
	   (syntax-list? (syntax-cdr x)))))

(define* (syntax-car x)
  (cond ((wrapped-syntax-object? x)
	 (mkstx (syntax-car (syntax-object-expression x))
		(syntax-object-marks x)
		(syntax-object-ribs  x)
		(syntax-object-source-objects   x)))
	((annotation? x)
	 (syntax-car (annotation-expression x)))
	((pair? x)
	 ($car x))
	(else
	 (assertion-violation __who__ "not a pair" x))))

(define* (syntax-cdr x)
  (cond ((wrapped-syntax-object? x)
	 (mkstx (syntax-cdr (syntax-object-expression x))
		(syntax-object-marks x)
		(syntax-object-ribs  x)
		(syntax-object-source-objects   x)))
	((annotation? x)
	 (syntax-cdr (annotation-expression x)))
	((pair? x)
	 ($cdr x))
	(else
	 (assertion-violation __who__ "not a pair" x))))

(define* (syntax->list x)
  (cond ((syntax-pair? x)
	 (cons (syntax-car x)
	       (syntax->list (syntax-cdr x))))
	((syntax-null? x)
	 '())
	(else
	 (assertion-violation __who__ "invalid argument" x))))

(define* (syntax-vector->list x)
  (cond ((wrapped-syntax-object? x)
	 (let ((ls     (syntax-vector->list (syntax-object-expression x)))
	       (mark*  (syntax-object-marks x))
	       (rib*   (syntax-object-ribs  x))
	       (ae*    (syntax-object-source-objects   x)))
	   (map (lambda (x)
		  (mkstx x mark* rib* ae*))
	     ls)))
	((annotation? x)
	 (syntax-vector->list (annotation-expression x)))
	((vector? x)
	 (vector->list x))
	(else
	 (assertion-violation __who__ "not a syntax vector" x))))

(define (syntax-unwrap stx)
  ;;Given a syntax object STX  decompose it and return the corresponding
  ;;S-expression holding datums and identifiers.  Take care of returning
  ;;a proper  list when the  input is a  syntax object holding  a proper
  ;;list.
  ;;
  (syntax-match stx ()
    (()
     '())
    ((?car . ?cdr)
     (cons (syntax-unwrap ?car)
	   (syntax-unwrap ?cdr)))
    (#(?item* ...)
     (list->vector (syntax-unwrap ?item*)))
    (?atom
     (identifier? ?atom)
     ?atom)
    (?atom
     (syntax->datum ?atom))))




;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
