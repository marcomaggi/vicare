;; old-syntaxes.sls --
;;
;; Old syntaxes  that are no  more used or  have been integrated  in the
;; boot image.
;;

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


(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))

;;Posted  by  "leppie"  on  the  Ikarus  mailing  list;  subject  "Macro
;;Challenge of Last Year [Difficulty: *****]", 20 Oct 2009.
;;
(define-syntax define-integrable
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

(define-syntax receive
  (syntax-rules ()
    ((_ ?formals ?expression ?form0 ?form ...)
     (call-with-values
	 (lambda () ?expression)
       (lambda ?formals ?form0 ?form ...)))))

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
	   ))))))

;;; end of file
