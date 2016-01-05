;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for flonums core primitives
;;Date: Fri Jan  1, 2016
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;

#!vicare
(library (typed-core-primitives posix)
  (export typed-core-primitives.posix)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.posix)


;;;; POSIX API, safe primitives

(section

(declare-parameter string->filename-func	<procedure>)
(declare-parameter string->pathname-func	<procedure>)

(declare-parameter filename->string-func	<procedure>)
(declare-parameter pathname->string-func	<procedure>)

;;; --------------------------------------------------------------------

(declare-object-predicate file-pathname?)
(declare-object-predicate file-string-pathname?)
(declare-object-predicate file-bytevector-pathname?)

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<string>)		=> (<boolean>))
		    ((<bytevector>)	=> (<boolean>)))
		   (attributes
		    ;;Not foldable.
		    ((_)		effect-free))))
		)))
  (declare directory-exists?)
  (declare file-exists?)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<string>)		=> (<boolean>))
		    ((<bytevector>)	=> (<boolean>)))
		   (attributes
		    ((_)		foldable effect-free))))
		)))
  (declare file-absolute-pathname?)
  (declare file-relative-pathname?)
  #| end of LET-SYNTAX |# )

(declare-core-primitive file-colon-search-path?
    (safe)
  (signatures
   ((<string>)			=> (<boolean>))
   ((<bytevector>)		=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?argument-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((?argument-tag)	=> (<boolean>)))
		   (attributes
		    ((_)		foldable effect-free))))
		)))
  (declare file-string-colon-search-path?	<string>)
  (declare file-bytevector-colon-search-path?	<bytevector>)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; file operations

(declare-core-primitive delete-file
    (safe)
  (signatures
   ((<string>)		=> (<void>))
   ((<bytevector>)	=> (<void>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive file-modification-time
    (safe)
  (signatures
   ((<string>)		=> (<exact-integer>))
   ((<bytevector>)	=> (<exact-integer>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive real-pathname
    (safe)
  (signatures
   ((<string>)		=> (<string>))
   ((<bytevector>)	=> (<string>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive mkdir
    (safe)
  (signatures
   ((<string> <fixnum>)		=> (<void>))
   ((<bytevector> <fixnum>)	=> (<void>)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive mkdir/parents
    (safe)
  (signatures
   ((<string> <fixnum>)		=> (<void>))
   ((<bytevector> <fixnum>)	=> (<void>)))
  (attributes
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive search-file-in-environment-path
    (safe)
  (signatures
   ((<string> <string>)		=> (<top>)))
  (attributes
   ((_ _)			effect-free)))

(declare-core-primitive search-file-in-list-path
    (safe)
  (signatures
   ((<string> <list>)		=> (<top>)))
  (attributes
   ((_ _)			effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive split-pathname-root-and-tail
    (safe)
  (signatures
   ((<string>)		=> (<string> <string>)))
  (attributes
   ((_ _)		effect-free)))

;;;

(declare-core-primitive split-pathname
    (safe)
  (signatures
   ((<string>)		=> (<boolean> <list>))
   ((<bytevector>)	=> (<boolean> <list>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive split-pathname-bytevector
    (safe)
  (signatures
   ((<bytevector>)	=> (<boolean> <list>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive split-pathname-string
    (safe)
  (signatures
   ((<string>)		=> (<boolean> <list>)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive split-search-path
    (safe)
  (signatures
   ((<string>)		=> (<list>))
   ((<bytevector>)	=> (<list>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive split-search-path-bytevector
    (safe)
  (signatures
   ((<bytevector>)	=> (<list>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive split-search-path-string
    (safe)
  (signatures
   ((<string>)		=> (<list>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; errors

(declare-core-primitive strerror
    (safe)
  (signatures
   ((<boolean>)			=> (<string>))
   ((<fixnum>)			=> (<string>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive errno->string
    (safe)
  (signatures
   ((<fixnum>)		=> (<string>)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; environment

(declare-core-primitive getenv
    (safe)
  (signatures
   ((<string>)		=> (<top>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive environ
    (safe)
  (signatures
   (()		=> (<list>)))
  (attributes
   (()		result-true)))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 2)
;; End:
