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
   syntax->list
   syntax-vector?		syntax-vector->list
   syntax-unwrap

   parse-logic-predicate-syntax
   error-invalid-formals-syntax)


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

(define* (syntax-car x)
  (cond ((stx? x)
	 (mkstx (syntax-car (stx-expr x))
		(stx-mark* x)
		(stx-rib*  x)
		(stx-annotated-expr*   x)))
	((reader-annotation? x)
	 (syntax-car (reader-annotation-expression x)))
	((pair? x)
	 ($car x))
	(else
	 (assertion-violation __who__ "not a pair" x))))

(define* (syntax-cdr x)
  (cond ((stx? x)
	 (mkstx (syntax-cdr (stx-expr x))
		(stx-mark* x)
		(stx-rib*  x)
		(stx-annotated-expr*   x)))
	((reader-annotation? x)
	 (syntax-cdr (reader-annotation-expression x)))
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


;;;; formals syntax validation

(define (error-invalid-formals-syntax input-form-stx formals-stx)
  ;;Raise an error for invalid formals of LAMBDA, CASE-LAMBDA, LET and similar.
  ;;
  ;;If no invalid  formals are found: return unspecified values,  else raise a syntax
  ;;violation.  This function is called when  it has been already determined that the
  ;;formals have something wrong.
  ;;
  ;;For a LAMBDA syntax:
  ;;
  ;;   (lambda ?formals . ?body)
  ;;
  ;;it is called as:
  ;;
  ;;   (error-invalid-formals-syntax
  ;;      #'(lambda ?formals . ?body)
  ;;      #'?formals)
  ;;
  ;;For a LET syntax:
  ;;
  ;;   (let ((?lhs* ?rhs*) ...) . ?body)
  ;;
  ;;it is called as:
  ;;
  ;;   (error-invalid-formals-syntax
  ;;      #'(let ((?lhs* ?rhs*) ...) . ?body)
  ;;      #'?lhs*)
  ;;
  ;;NOTE Invalid  LET-VALUES and LET*-VALUES  formals are processed by  this function
  ;;indirectly;  LET-VALUES  and  LET*-VALUES  syntaxes are  first  transformed  into
  ;;CALL-WITH-VALUES  syntaxes, then  it  is the  LAMBDA syntax  that  takes care  of
  ;;formals validation.
  ;;
  (define (%synner message subform)
    (syntax-violation 'error-invalid-formals-syntax message input-form-stx subform))
  (syntax-match formals-stx ()
    ((?id* ... . ?last)
     (let recur ((?id* (cond ((identifier? ?last)
			      (cons ?last ?id*))
			     ((syntax-null? ?last)
			      ?id*)
			     (else
			      (%synner "not an identifier" ?last)))))
       (cond ((null? ?id*)
	      (void))
	     ((not (identifier? (car ?id*)))
	      (%synner "not an identifier" (car ?id*)))
	     (else
	      (recur (cdr ?id*))
	      (when (bound-id-member? (car ?id*)
				      (cdr ?id*))
		(%synner "duplicate binding" (car ?id*)))))))

    (_
     (%synner "malformed binding form" formals-stx))))


;;;; done

#| end of module: PSYNTAX-SYNTAX-UTILITIES |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
