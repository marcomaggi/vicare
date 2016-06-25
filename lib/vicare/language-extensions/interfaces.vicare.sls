;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: interfaces for record-types
;;;Date: Sat Jun 25, 2016
;;;
;;;Abstract
;;;
;;;	Interfaces  are  collections  of  method  signatures  that  record-types  can
;;;	implement to expose a common API.
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare language-extensions interfaces (0 4 2016 6 25))
  (options typed-language)
  (export define-interface method)
  (import (vicare))


(define-syntax define-interface
  (internal-body
    (import (only (vicare expander) #;(psyntax system $all)
		  make-interface-type-spec
		  type-annotation->object-type-spec))
    (define-constant __module_who__ 'define-interface)

    (define-constant CLAUSE-SPEC*
      (syntax-clauses-validate-specs
       (list
	;;KEYWORD MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
	(new <syntax-clause-spec> #'method			0 +inf.0 2 2 '() '())
	#| end of LIST |# )))

;;; --------------------------------------------------------------------

    (define (main input-form.stx synner)
      (syntax-case input-form.stx ()
	((_ ?type-name . ?clauses)
	 (%parse-clauses #'?type-name #'?clauses synner))
	(_
	 (synner "invalid DEFINE-INTERFACE syntax use"))))

    (define (%parse-clauses type-name.id clauses.stx synner)
      (unless (identifier? type-name.id)
	(synner "expected identifier as interface type name" type-name.id))
      (let* ((clause*.stx (syntax-clauses-unwrap clauses.stx synner))
	     (table*	  (syntax-clauses-fold-specs
			   (lambda (table* {spec <syntax-clause-spec>} args)
			     (combine type-name.id table* spec args synner))
			   '() CLAUSE-SPEC* clause*.stx synner)))
	#`(define-syntax #,type-name.id
	    (make-interface-type-spec (quote #,table*)))))

;;; --------------------------------------------------------------------

    (define (combine type-name.id table* {spec <syntax-clause-spec>} args synner)
      ((case-identifiers (.keyword spec)
	 ((method)			%process-clause/method)
	 (else
	  (assertion-violation __module_who__ "invalid clause spec" spec)))
       type-name.id args spec table* synner))

    (define (%process-clause/method type-name.id args {spec <syntax-clause-spec>} table* synner)
      ;;This clause can  be present multiple times.  Each input  clause must have the
      ;;format:
      ;;
      ;;   (method ?name ?signature)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?name ?signature) ...)
      ;;
      (vector-fold-left (lambda (table* arg)
			  (cons (%process-method-spec arg synner)
				table*))
	table* args))

    (define (%process-method-spec arg synner)
      ;;We expect ARG to have the format:
      ;;
      ;;   #(?name ?signature)
      ;;
      ;;Return a pair: having as car a symbol representing the method name; having as
      ;;cdr an instance  of "<closure-type-spec>" representing the  type signature of
      ;;the method.
      (syntax-case arg ()
	(#(?name ?signature)
	 (cons (syntax-case #'?name (brace)
		 (?method-name
		  (identifier? #'?name)
		  (syntax->datum #'?name))
		 (_
		  (synner "invalid method name" #'?name)))
	       (type-annotation->object-type-spec #'?signature)))
	(#(?stuff ...)
	 (synner "invalid method specification" #'(method ?stuff ...)))))

;;; --------------------------------------------------------------------

    (lambda (input-form.stx)
      (case-define synner
	((message)
	 (syntax-violation __module_who__ message input-form.stx #f))
	((message subform)
	 (syntax-violation __module_who__ message input-form.stx subform)))
      (main input-form.stx synner))))


;;;; done

#| end of library |# )

;;; end of file
