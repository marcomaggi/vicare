;;;
;;;Part of: Vicare
;;;Contents: ensure syntax
;;;Date: Wed Feb 25, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (vicare language-extensions ensure)
  (export ensure by else-by
	  &ensure make-ensure-error ensure-error?)
  (import (vicare))

  (define-condition-type &ensure
      &error
    make-ensure-error
    ensure-error?)

  (define (raise-ensure-error)
    (raise
     (condition (make-ensure-error)
		(make-message-condition "unable to ensure result"))))

  (define-auxiliary-syntaxes by else-by)

  (define-syntax* (ensure stx)
    (define (%process-clauses test-id clause*)
      (syntax-case clause* (else-by)
	(()
	 #`(if (#,test-id)
	       (values)
	     (raise-ensure-error)))

	(((else-by ?body-else0 ?body-else ...) . ?rest)
	 #`(if (#,test-id)
	       (values)
	     (begin
	       ?body-else0 ?body-else ...
	       #,(%process-clauses test-id #'?rest))))

	(_
	 (synner "invalid syntax in ELSE-BY clauses" clause*))))
    (syntax-case stx (by else-by)
      ((_ ?test
	  (by ?body-by0 ?body-by ...)
	  (else-by ?body-else0 ?body-else ...)
	  ...)
       (with-syntax
	   (((TEST) (generate-temporaries '(#f))))
	 #`(let ((TEST (lambda () ?test)))
	     (if (TEST)
		 (values)
	       (begin
		 ?body-by0 ?body-by ...
		 #,(%process-clauses #'TEST
				     #'((else-by ?body-else0 ?body-else ...)
					...)))))))
      ))

  #| end of LIBRARY |# )

;;; end of file
;;Local Variables:
;;coding: utf-8-unix
;;End:
