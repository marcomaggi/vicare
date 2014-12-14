;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; documentation
;;
;;The builders  are used  by the expander  to compose the  final, output
;;symbolic  expression after  all the  macros have  been  expanded.  The
;;symbolic  expressions  returned  by  the  builder  functions  must  be
;;understandable by the underlying compiler or interpreter.
;;
;;For  all   the  builders:  the  AE  argument   stands  for  "annotated
;;expression".
;;


(library (psyntax.builders)
  (export
    build-lexical-assignment		build-global-reference
    build-application			build-conditional
    build-lexical-reference		build-global-assignment
    build-global-definition		build-lambda
    build-case-lambda			#;build-let
    build-primref			build-foreign-call
    build-data				build-sequence
    build-void				build-letrec
    build-letrec*			#;build-global-define
    build-library-letrec*

    core-language->sexp)
  (import (rnrs)
    (psyntax.compat)
    (psyntax.config))


(define (build-void)
  '((primitive void)))

(define (build-global-define x)
  (if-wants-global-defines
   `(define ,x '#f)
   (build-void)))

(define (build-application ae fun-exp arg-exps)
  (if ae
      `(annotated-call ,ae ,fun-exp . ,arg-exps)
    (cons fun-exp arg-exps)))

(define-syntax build-conditional
  (syntax-rules ()
    ((_ ae test-exp then-exp else-exp)
     `(if ,test-exp ,then-exp ,else-exp))))


(define-syntax build-lexical-reference
  (syntax-rules ()
    ((_ ae var)
     var)))

(define-syntax build-lexical-assignment
  (syntax-rules ()
    ((_ ae var exp)
     `(set! ,var ,exp))))

(define-syntax build-global-reference
  (syntax-rules ()
    ((_ ae var)
     var)))

(define-syntax build-global-assignment
  (syntax-rules ()
    ((_ ae var exp)
     `(set! ,var ,exp))))

(define-syntax build-global-definition
  (syntax-rules ()
    ((_ ae var exp)
     (build-global-assignment ae var exp))))


(define (build-lambda ae vars exp)
  (if-wants-case-lambda
   (build-case-lambda ae (list vars) (list exp))
   `(lambda ,vars ,exp)))

(define build-case-lambda
  (if-wants-case-lambda
   (lambda (ae vars* exp*)
     (if ae
	 `(annotated-case-lambda ,ae . ,(map list vars* exp*))
       `(case-lambda . ,(map list vars* exp*))))
   (lambda (ae vars* exp*)
     (define (build-error ae)
       (build-application ae
			  (build-primref ae 'error)
			  (list (build-data ae 'apply)
				(build-data ae "invalid arg count"))))
     (define (build-pred ae n vars)
       (let-values (((count pred)
		     (let f ((vars vars) (count 0))
		       (cond
			((pair? vars) (f (cdr vars) (+ count 1)))
			((null? vars) (values count '=))
			(else (values count '>=))))))
	 (build-application ae (build-primref ae pred)
			    (list (build-lexical-reference ae n)
				  (build-data ae count)))))
     (define (build-apply ae g vars exp)
       (build-application ae (build-primref ae 'apply)
			  (list (build-lambda ae vars exp)
				(build-lexical-reference ae g))))
     (define (expand-case-lambda ae vars exp*)
       (let ((g (gensym)) (n (gensym)))
	 `(lambda ,g
	    ,(build-let ae
			(list n) (list (build-application ae
							  (build-primref ae 'length)
							  (list (build-lexical-reference ae g))))
			(let f ((vars* vars*) (exp* exp*))
			  (if (null? vars*)
			      (build-error ae)
			    (build-conditional ae
					       (build-pred ae n (car vars*))
					       (build-apply ae g (car vars*) (car exp*))
					       (f (cdr vars*) (cdr exp*)))))))))
     (if (= (length exp*) 1)
	 (build-lambda ae (car vars*) (car exp*))
       (expand-case-lambda ae vars* exp*)))))


(define (build-let ae lhs* rhs* body)
  ;;Transform a LET syntax into the appliction of a LAMBDA function:
  ;;
  ;;  (let ((?lhs ?rhs) ...) . ?body)
  ;;  --> ((lambda (?lhs ...) . ?body) ?rhs ...)
  ;;
  ;;This is used only when building LETREC* and CASE-LAMBDA.
  ;;
  (build-application ae (build-lambda ae lhs* body) rhs*))

(define-syntax build-primref
  (syntax-rules ()
    ((_ ae name)
     (build-primref ae 1 name))
    ((_ ae level name)
     `(primitive ,name))))

(define-syntax build-foreign-call
  (syntax-rules ()
    ((_ ae name arg*)
     `(foreign-call ,name . ,arg*))))

(define-syntax build-data
  (syntax-rules ()
    ((_ ae exp) `',exp)))

(define (build-sequence ae exps)
  ;;Given a list of expressions to be evaluated in sequence wrap it in a
  ;;BEGIN syntax.  Discard useless void expressions.
  ;;
  (let ((the-void (build-void)))
    (let loop ((exps exps))
      (cond ((null? (cdr exps))
	     (car exps))
	    ((equal? (car exps) the-void)
	     (loop (cdr exps)))
	    (else
	     `(begin ,@exps))))))


(define (build-letrec ae vars val-exps body-exp)
  (if (null? vars)
      body-exp
    `(letrec ,(map list vars val-exps) ,body-exp)))

(define (build-letrec* ae vars val-exps body-exp)
  (if (null? vars)
      body-exp
    (if-wants-letrec*
     `(letrec* ,(map list vars val-exps) ,body-exp)
     (build-let ae vars (map (lambda (x)
			       (build-data ae #f))
			  vars)
		(build-sequence ae
				(append (map (lambda (lhs rhs)
					       (build-lexical-assignment ae lhs rhs))
					  vars val-exps)
				 (list body-exp)))))))

(define (build-library-letrec* ae top? vars locs val-exps body-exp)
  (if-wants-library-letrec*
   `(library-letrec* ,(map list vars locs val-exps) ,body-exp)
   (build-letrec* ae vars val-exps
		  (if top?
		      body-exp
		    (build-sequence ae
				    (cons body-exp
					  (map (lambda (var loc)
						 (build-global-assignment ae loc var))
					    vars locs)))))))


(module (core-language->sexp)
  ;;Recursively convert  an expression in core  language (which contains
  ;;syntax objects as annotations) into a readable symbolic expression.
  ;;
  ;;FIXME This should be improved.  (Marco Maggi; Mon Apr 7, 2014)
  ;;
  (define (core-language->sexp core)
    (if (pair? core)
	(case (car core)
	  ((annotated-call)
	   (map core-language->sexp (cddr core)))

	  ((annotated-case-lambda)
	   (%process-case-lambda (cddr core)))

	  ((case-lambda)
	   (%process-case-lambda (cdr core)))

	  ((begin)
	   `(begin
	      ,@(map core-language->sexp (cdr core))))

	  ((if)
	   `(if ,(core-language->sexp (cadr core))
		,(core-language->sexp (caddr core))
	      ,(core-language->sexp (cadddr core))))

	  ((set!)
	   `(set! ,(cadr core) ,(core-language->sexp (caddr core))))

	  ((define)
	   `(define ,(cadr core) ,(core-language->sexp (caddr core))))

	  ((lerec letrec*)
	   ;;We expect CORE to have the format:
	   ;;
	   ;;   (letrec  ((?id ?expr) ...) ?body)
	   ;;   (letrec* ((?id ?expr) ...) ?body)
	   ;;
	   (let ((bind*.core (cadr  core))
		 (body.core  (caddr core)))
	     (let ((bind*.sexp (map (lambda (bind)
				      (let ((lex  (car  bind))
					    (expr (cadr bind)))
					(list lex (core-language->sexp expr))))
				 bind*.core))
		   (body.sexp  (core-language->sexp body.core)))
	       (if (eq? 'letrec (car core))
		   `(letrec ,bind*.sexp ,body.sexp)
		 `(letrec* ,bind*.sexp ,body.sexp)))))

	  (else
	   (map core-language->sexp core)))
      core))

  (define (%process-case-lambda meat)
    (let ((args*  (map car meat))
	  (body** (map cdr meat)))
      (if (= 1 (length args*))
	  `(lambda ,(car args*)
	     . ,(%process-lambda-body (car body**)))
	`(case-lambda
	  ,@(map cons args* (map %process-lambda-body body**))))))

  (define (%process-lambda-body body*)
    ;;If a LAMBDA body consists of a single BEGIN syntax:
    ;;
    ;;   (lambda ?formals (begin . ?body))
    ;;
    ;;we discard the BEGIN and keep the ?body:
    ;;
    ;;   (lambda ?formals . ?body)
    ;;
    (let ((body* (map core-language->sexp body*)))
      (cond ((and (= 1 (length body*))
		  (eq? 'begin (car body*)))
	     (cdr body*))
	    ((and (= 1 (length body*))
		  (pair? (car body*))
		  (eq? 'begin (caar body*)))
	     (cdar body*))
	    (else body*))))

  #| end of module: CORE-LANGUAGE->SEXP |# )


;;;; done

)

;;; end of file
