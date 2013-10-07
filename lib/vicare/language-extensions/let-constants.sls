;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: special syntax for constant bindings
;;;Date: Mon Oct  7, 2013
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
(library (vicare language-extensions let-constants)
  (export
    let-constants
    let*-constants
    letrec-constants
    letrec*-constants)
  (import (vicare))


(define-syntax (let-constants stx)
  (syntax-case stx ()
    ((_ ((?id ?init) ...) ?body0 ?body ...)
     (all-identifiers? #'(?id ...))
     (with-syntax
	 (((SHADOW ...) (generate-temporaries #'(?id ...))))
       #'(let ((SHADOW ?init) ...)
	   (let-syntax ((?id (identifier-syntax SHADOW)) ...)
	     ?body0 ?body ...))))
    ))

(define-syntax (let*-constants stx)
  (syntax-case stx ()
    ((_ () ?body0 ?body ...)
     #'(let () ?body0 ?body ...))

    ((_ ((?id0 ?init0) (?id ?init) ...) ?body0 ?body ...)
     #'(let-constants ((?id0 ?init0))
	 (let*-constants ((?id ?init) ...)
	   ?body0 ?body ...)))
    ))

(define-syntax (letrec-constants stx)
  (syntax-case stx ()
    ((_ () ?body0 ?body ...)
     #'(let () ?body0 ?body ...))

    ((_ ((?id ?init) ...) ?body0 ?body ...)
     (with-syntax
	 (((TMP ...) (generate-temporaries #'(?id ...)))
	  ((VAR ...) (generate-temporaries #'(?id ...))))
       #'(let ((VAR #f) ...)
	   (let-syntax ((?id (identifier-syntax VAR)) ...)
	     ;;Do not enforce the order of evaluation of ?INIT.
	     (let ((TMP ?init) ...)
	       (set! VAR TMP) ...
	       (let () ?body0 ?body ...))))))
    ))

(define-syntax (letrec*-constants stx)
  (syntax-case stx ()
    ((_ () ?body0 ?body ...)
     #'(let () ?body0 ?body ...))

    ((_ ((?id ?init) ...) ?body0 ?body ...)
     (with-syntax
	 (((TMP ...) (generate-temporaries #'(?id ...)))
	  ((VAR ...) (generate-temporaries #'(?id ...))))
       #'(let ((VAR #f) ...)
	   (let-syntax ((?id (identifier-syntax VAR)) ...)
	     ;;Do enforce the order of evaluation of ?INIT.
	     (let* ((TMP ?init) ...)
	       (set! VAR TMP) ...
	       (let () ?body0 ?body ...))))))
    ))


;;;; done

)

;;; end of file
