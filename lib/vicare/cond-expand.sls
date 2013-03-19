;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: cond-expand definer
;;;Date: Sun Mar 17, 2013
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

;;;Copyright 2009 Derick Eddington.  My MIT-style license is in the file
;;;named  LICENSE.srfi  from  the   original  collection  this  file  is
;;;distributed with.


#!r6rs
(library (vicare cond-expand)
  (export define-cond-expand)
  (import (rnrs)
    (for (only (vicare cond-expand registry)
	       available-features)
	 expand))


(define-syntax define-cond-expand
  (lambda (stx)
    (syntax-case stx ()
      ((?ctx ??who ??feature-func ...)
       (identifier? #'??who)
       (with-syntax
	   (((FEATURE ...) (generate-temporaries #'(??feature-func ...))))
	 #'(define-syntax ??who
	     (let ()
	       (define FEATURE ??feature-func)
	       ...
	       (lambda (stx)
		 (syntax-case stx (and or not else)
		   ((?cond-expand)
		    (syntax-violation #f "unfulfilled cond-expand" stx))

		   ((?cond-expand (else ?body (... ...)))
		    #'(begin ?body (... ...)))

		   ((?cond-expand ((and) ?body (... ...)) ?clauses (... ...))
		    #'(begin ?body (... ...)))

		   ((?cond-expand ((and ?req1 ?req2 (... ...)) ?body (... ...))
				  ?clauses (... ...))
		    #'(?cond-expand
		       (?req1
			(?cond-expand
			 ((and ?req2 (... ...)) ?body (... ...))
			 ?clauses (... ...)))
		       ?clauses (... ...)))

		   ((?cond-expand ((or) ?body (... ...)) ?clauses (... ...))
		    #'(?cond-expand ?clauses (... ...)))

		   ((?cond-expand ((or ?req1 ?req2 (... ...)) ?body (... ...)) ?clauses (... ...))
		    #'(?cond-expand (?req1 (begin ?body (... ...)))
				    (else
				     (?cond-expand
				      ((or ?req2 (... ...)) ?body (... ...))
				      ?clauses (... ...)))))

		   ((?cond-expand ((not ?req) ?body (... ...)) ?clauses (... ...))
		    #'(?cond-expand (?req (?cond-expand ?clauses (... ...)))
				    (else ?body (... ...))))

		   ((?cond-expand (?feature-id ?body (... ...)) ?clauses (... ...))
		    (if (or (member (syntax->datum #'?feature-id)
				    available-features)
			    (??feature-func #'?feature-id)
			    ...)
			#'(begin ?body (... ...))
		      #'(?cond-expand ?clauses (... ...))))

		   ))))
	 )))))


;;;; done

)

;;; end of file
