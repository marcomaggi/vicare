;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: try, catch, finally syntax
;;;Date: Tue Dec  3, 2013
;;;
;;;Abstract
;;;
;;;
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
(library (vicare language-extensions try)
  (export try catch finally else)
  (import (vicare))


;;;; try ... catch ... finally ... syntax

(define-auxiliary-syntaxes catch finally)

(define-syntax* (try stx)
  (define who 'try)

  (define (main stx)
    (syntax-case stx (catch finally)

      ;;Full syntax.
      ((_ ?body (catch ?var ?catch-clause0 ?catch-clause ...) (finally ?finally-body0 ?finally-body ...))
       (let ((var-id             #'?var)
	     (catch-clauses-stx  #'(?catch-clause0 ?catch-clause ...)))
	 (validate-variable #'?var)
	 (with-syntax
	     (((GUARD-CLAUSE ...) (parse-multiple-catch-clauses var-id catch-clauses-stx)))
	   #'(with-compensations
	       (push-compensation ?finally-body0 ?finally-body ...)
	       (guard (?var GUARD-CLAUSE ...) ?body)))))

      ;;Only catch, no finally.
      ((_ ?body (catch ?var ?catch-clause0 ?catch-clause ...))
       (let ((var-id             #'?var)
	     (catch-clauses-stx  #'(?catch-clause0 ?catch-clause ...)))
	 (validate-variable #'?var)
	 (with-syntax
	     (((GUARD-CLAUSE ...) (parse-multiple-catch-clauses var-id catch-clauses-stx)))
	   #'(guard (?var GUARD-CLAUSE ...) ?body))))

      (_
       (synner "invalid try syntax"))))

  (define (parse-multiple-catch-clauses var-id clauses-stx)
    (syntax-case clauses-stx (else)
      ;;Match when  there is no  ELSE clause.  Remember that  GUARD will
      ;;reraise the exception when there is no ELSE clause.
      (()
       '())
      ;;The one with the ELSE clause must come first!!!
      (((else ?else-body0 ?else-body ...))
       clauses-stx)
      (((?tag ?tag-body0 ?tag-body ...) . ?other-clauses)
       (identifier? #'?tag)
       (cons #`((record-type-and-record? ?tag #,var-id)
		?tag-body0 ?tag-body ...)
	     (parse-multiple-catch-clauses var-id #'?other-clauses)))
      ((((?tag) ?tag-body0 ?tag-body ...) . ?other-clauses)
       (identifier? #'?tag)
       (cons #`((record-type-and-record? ?tag #,var-id)
		?tag-body0 ?tag-body ...)
	     (parse-multiple-catch-clauses var-id #'?other-clauses)))
      ((?clause . ?other-clauses)
       (synner "invalid catch clause in try syntax" #'?clause))))

  (define (validate-variable var-id)
    (unless (identifier? var-id)
      (synner "expected identifier as variable" var-id)))

  (main stx))


;;;; done

)

;;; end of file
