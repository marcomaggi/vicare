;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions and syntaxes for generics implementation
;;;Date: Mon May 21, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language multimethods helpers-for-expand)
  (export case-identifier)
  (import (vicare))


(define-syntax case-identifier
  ;;Like CASE defined by R6RS,  but specialised to branch on identifiers
  ;;using FREE-IDENTIIFER=?.
  ;;
  (syntax-rules (else)
    ((_ ?expr
	((?id0 ?id ...)
	 ?id-body0 ?id-body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (let ((sym ?expr))
       (cond ((or (free-identifier=? sym #'?id0)
		  (free-identifier=? sym #'?id)
		  ...)
	      ?id-body0 ?id-body ...)
	     ...
	     (else
	      ?else-body0 ?else-body ...))))
    ((_ ?expr
	((?id0 ?id ...)
	 ?id-body0 ?id-body ...)
	...)
     (let ((sym ?expr))
       (cond ((or (free-identifier=? sym #'?id0)
		  (free-identifier=? sym #'?id)
		  ...)
	      ?id-body0 ?id-body ...)
	     ...)))
    ))


;;;; done

)

;;; end of file
;; Local Variables:
;; coding: utf-8
;; End:
