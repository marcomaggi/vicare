;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: case for identifiers
;;;Date: Sun Oct  6, 2013
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
(library (vicare language-extensions case-identifiers)
  (export case-identifiers)
  (import (vicare))


;;There is no unsafe version possible for identifiers.
;;
(define-syntax (case-identifiers stx)
  (define who 'case-identifiers)
  (define (%assert-all-datums LL)
    (for-each (lambda (L)
		(for-each (lambda (S)
			    (unless (symbol? S)
			      (syntax-violation who
				(string-append "expected identifier as datum")
				L S)))
		  L))
      (syntax->datum LL)))
  (syntax-case stx (else)
    ((_ ?expr
	((?datum0 ?datum ...)
	 ?datum-body0 ?datum-body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (begin
       (%assert-all-datums #'((?datum0 ?datum ...) ...))
       #'(let ((key ?expr))
	   (cond ((or (free-identifier=? #'?datum0 key)
		      (free-identifier=? #'?datum  key)
		      ...)
		  ?datum-body0 ?datum-body ...)
		 ...
		 (else
		  ?else-body0 ?else-body ...)))))

    ((_ ?expr
	((?datum0 ?datum ...)
	 ?datum-body0 ?datum-body ...)
	...)
     (begin
       (%assert-all-datums #'((?datum0 ?datum ...) ...))
       #'(let ((key ?expr))
	   (cond ((or (free-identifier=? #'?datum0 key)
		      (free-identifier=? #'?datum  key)
		      ...)
		  ?datum-body0 ?datum-body ...)
		 ...))))
    ))


;;;; done

)

;;; end of file
