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


(module PSYNTAX-SYNTAX-UTILITIES
  ;; high-level syntax objects utilities
  (generate-temporaries
   syntax-null?
   syntax-pair?			syntax-list?
   syntax-car			syntax-cdr
   syntax->list			identifiers->list
   all-identifiers?
   syntax-vector?		syntax-vector->list
   syntax->vector
   syntax-unwrap		syntax=?

   parse-logic-predicate-syntax)


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
		 (gensym "tmp")
	       (let ((x (syntax->datum x)))
		 (if (or (symbol? x)
			 (string? x))
		     (gensym x)
		   (gensym "tmp"))))))
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
    (cond ((stx? x)
	   (syntax-kind? (stx-expr x) pred?))
	  ((reader-annotation? x)
	   (syntax-kind? (reader-annotation-expression x) pred?))
	  (else
	   (pred? x))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define (syntax-list? x)
  ;;FIXME Should terminate on cyclic input.  (Abdulaziz Ghuloum)
  (or (syntax-null? x)
      (and (syntax-pair? x)
	   (syntax-list? (syntax-cdr x)))))

(case-define* syntax-car
  ((stx)
   (syntax-car stx (lambda (message subform)
		     (syntax-violation __who__ message stx subform))))
  ((stx synner)
   (cond ((stx? stx)
	  (mkstx (syntax-car (stx-expr stx))
		 (stx-mark* stx)
		 (stx-rib*  stx)
		 (stx-annotated-expr* stx)))
	 ((reader-annotation? stx)
	  (syntax-car (reader-annotation-expression stx)))
	 ((pair? stx)
	  (car stx))
	 (else
	  (synner "expected syntax object holding pair as argument" stx)))))

(case-define* syntax-cdr
  ((stx)
   (syntax-cdr stx (lambda (message subform)
		     (syntax-violation __who__ message stx subform))))
  ((stx synner)
   (cond ((stx? stx)
	  (mkstx (syntax-cdr (stx-expr stx))
		 (stx-mark* stx)
		 (stx-rib*  stx)
		 (stx-annotated-expr* stx)))
	 ((reader-annotation? stx)
	  (syntax-cdr (reader-annotation-expression stx)))
	 ((pair? stx)
	  (cdr stx))
	 (else
	  (synner "expected syntax object holding pair as argument" stx)))))

(case-define* syntax->list
  ((stx)
   (syntax->list stx (lambda (message subform)
		       (syntax-violation __who__ message stx subform))))
  ((stx synner)
   (cond ((syntax-pair? stx)
	  (cons (syntax-car stx synner)
		(syntax->list (syntax-cdr stx synner))))
	 ((syntax-null? stx)
	  '())
	 (else
	  (synner "expected syntax object holding proper list as argument" stx)))))

(case-define* identifiers->list
  ((stx)
   (identifiers->list stx (lambda (message subform)
			    (syntax-violation __who__ message stx subform))))
  ((stx {synner procedure?})
   (let recur ((stx stx))
     (syntax-match stx ()
       (() '())
       ((?car . ?cdr)
	(identifier? ?car)
	(cons ?car (recur ?cdr)))
       (_
	(synner "expected syntax object holding proper list of identifiers as argument" stx))))))

(define (all-identifiers? stx)
  (syntax-match stx ()
    (() #t)
    ((?car . ?cdr)
     (identifier? ?car)
     (all-identifiers? ?cdr))
    (_ #f)))

;;; --------------------------------------------------------------------

(define* (syntax-vector->list x)
  (cond ((stx? x)
	 (let ((ls     (syntax-vector->list (stx-expr x)))
	       (mark*  (stx-mark* x))
	       (rib*   (stx-rib*  x))
	       (ae*    (stx-annotated-expr*   x)))
	   (map (lambda (x)
		  (mkstx x mark* rib* ae*))
	     ls)))
	((reader-annotation? x)
	 (syntax-vector->list (reader-annotation-expression x)))
	((vector? x)
	 (vector->list x))
	(else
	 (assertion-violation __who__ "not a syntax vector" x))))

(case-define* syntax->vector
  ((stx)
   (syntax->vector stx (lambda (message subform)
			 (syntax-violation __who__ message stx subform))))
  ((stx synner)
   (syntax-match stx ()
     (() '())
     (#(?item* ...)
      (list->vector (syntax->list ?item* synner)))
     (_
      (synner "expected syntax object holding vector as argument" stx)))))

;;; --------------------------------------------------------------------

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

(define (syntax=? stx1 stx2)
  (define (%syntax=? stx1 stx2)
    (cond ((and (identifier? stx1)
		(identifier? stx2))
	   (free-identifier=? stx1 stx2))
	  ((and (pair? stx1)
		(pair? stx2))
	   (and (syntax=? (car stx1) (car stx2))
		(syntax=? (cdr stx1) (cdr stx2))))
	  ((and (vector? stx1)
		(vector? stx2))
	   (vector-for-all syntax=? stx1 stx2))
	  (else
	   (equal? stx1 stx2))))
  (%syntax=? (syntax-unwrap stx1) (syntax-unwrap stx2)))


(case-define parse-logic-predicate-syntax
  ;;Given a  syntax object STX parse  it as logic predicate  expression with expected
  ;;format:
  ;;
  ;;   STX = (and ?expr0 ?expr ...)
  ;;       | (or  ?expr0 ?expr ...)
  ;;       | (xor ?expr0 ?expr ...)
  ;;       | (not ?expr)
  ;;       | ?expr
  ;;
  ;;where  AND,  OR,  XOR, NOT  are  the  identifiers  exported  by (vicare).   If  a
  ;;standalone ?EXPR is found: apply the  procedure TAIL-PROC to it gather its single
  ;;return value; TAIL-PROC defaults to the identity function.
  ;;
  ;;Return  a syntax  object representing  the  logic predicate  with the  standalone
  ;;expressions replaced by the return values of TAIL-PROC.
  ;;
  ((stx)
   (parse-logic-predicate-syntax stx (lambda (stx) stx)))
  ((stx tail-proc)
   (define (recurse expr)
     (parse-logic-predicate-syntax expr tail-proc))
   (syntax-match stx (and or xor not)
     ((and ?expr0 ?expr* ...)
      (bless
       `(and ,@(map recurse (cons ?expr0 ?expr*)))))
     ((or  ?expr0 ?expr* ...)
      (bless
       `(or  ,@(map recurse (cons ?expr0 ?expr*)))))
     ((xor ?expr0 ?expr* ...)
      (bless
       `(xor ,@(map recurse (cons ?expr0 ?expr*)))))
     ((not ?expr)
      (bless
       `(not ,(recurse ?expr))))
     (else
      (tail-proc stx)))))


;;;; done

#| end of module: PSYNTAX-SYNTAX-UTILITIES |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
