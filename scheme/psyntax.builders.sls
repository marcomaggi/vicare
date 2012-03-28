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


(library (psyntax builders)
  (export
    build-lexical-assignment		build-global-reference
    build-application			build-conditional
    build-lexical-reference		build-global-assignment
    build-global-definition		build-lambda
    build-case-lambda			#;build-let
    build-primref			build-foreign-call
    build-data				build-sequence
    build-void				build-letrec
    build-letrec*			build-global-define
    build-library-letrec*)
  (import (rnrs)
    (psyntax compat)
    (psyntax config))


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


;;;; done

)

;;; end of file
