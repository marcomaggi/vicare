;;;
;;;Part of: Vicare Scheme
;;;Contents: dynamic Scheme libraries loading
;;;Date: Mon May 18, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.load.dynamic-library-loading)
  (export
    library-dynamic-load-and-intern
    library-dynamic-retrieve)
  (import (vicare)
    (prefix (only (psyntax.library-utils)
		  library-reference?)
	    libs.)
    (prefix (only (psyntax.library-manager)
		  library?
		  find-library-by-reference
		  invoke-library
		  visit-library
		  library-export-subst
		  library-global-env)
	    libs.)
    (prefix (only (psyntax.lexical-environment)
		  global-typed-variable-spec?
		  global-typed-variable-spec.variable-loc)
	    psyntax.))


;;;; public API

(define* (library-dynamic-load-and-intern {libref libs.library-reference?})
  (receive-and-return (lib)
      (libs.find-library-by-reference libref)
    (libs.invoke-library lib)))

(define* (library-dynamic-retrieve {lib libs.library?} {public-name symbol?})
  (cond ((assq public-name (libs.library-export-subst lib))
	 => (lambda (name.label)
	      (cond ((assq (cdr name.label) (libs.library-global-env lib))
		     => (lambda (label.descr)
			  (let ((type.loc (cdr label.descr)))
			    (case (car type.loc)
			      ((global)
			       (symbol-value (cdr type.loc)))
			      ((global-typed)
			       ;;We need to visit the  library so that the loc gensym
			       ;;is initialised.
			       (libs.visit-library lib)
			       (let ((gts (symbol-value (cdr type.loc))))
				 (if (psyntax.global-typed-variable-spec? gts)
				     (symbol-value (psyntax.global-typed-variable-spec.variable-loc gts))
				   (assertion-violation __who__
				     "invalid object in loc gensym's \"value\" slot of \"global-typed\" syntactic binding's descriptor"
				     public-name label.descr gts lib))))
			      (else
			       (error __who__
				 "attempt to dynamically retrieve a syntactic binding that is not a global variable"
				 public-name label.descr))))))
		    (else
		     (error __who__ "cannot find label in global-env" name.label)))))
	(else
	 (error __who__ "cannot find name symbol in export-subst" public-name))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8
;; End:
