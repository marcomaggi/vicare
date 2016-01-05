;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for library utilities core primitives
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
(library (typed-core-primitives library-utils)
  (export typed-core-primitives.library-utils)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.library-utils)


;;;; library names, safe primitives

(section

(declare-core-primitive library-name?
    (safe)
  (signatures
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-version-numbers?
    (safe)
  (signatures
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-version-number?
    (safe)
  (signatures
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-name-decompose
    (safe)
  (signatures
   ((<top>)		=> (<list> <list>)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-name->identifiers
    (safe)
  (signatures
   ((<list>)		=> (<list>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive library-name->version
    (safe)
  (signatures
   ((<list>)		=> (<list>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<list> <list>)	=> (<boolean>)))
		   (attributes
		    ((_ _)		foldable effect-free))))
		)))
  (declare library-name-identifiers=?)
  (declare library-name=?)
  (declare library-name<?)
  (declare library-name<=?)
  (declare library-version=?)
  (declare library-version<?)
  (declare library-version<=?)
  #| end of LET-SYNTAX |#)

/section)


;;;; library references and conformity, safe procedures

(section

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<top>)		=> (<boolean>)))
		   (attributes
		    ((_)		foldable effect-free))))
		)))
  (declare library-reference?)
  (declare library-version-reference?)
  (declare library-sub-version-reference?)
  (declare library-sub-version?)
  #| end of LET-SYNTAX |# )

(declare-core-primitive library-reference-decompose
    (safe)
  (signatures
   ((<top>)		=> (<list> <list>)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-reference->identifiers
    (safe)
  (signatures
   ((<list>)		=> (<list>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive library-reference->version-reference
    (safe)
  (signatures
   ((<list>)		=> (<list>)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive library-reference-identifiers=?
    (safe)
  (signatures
   ((<list> <list>)	=> (<boolean>)))
  (attributes
   ((_ _)		foldable effect-free)))

(declare-core-primitive conforming-sub-version-and-sub-version-reference?
    (safe)
  (signatures
   ((<non-negative-fixnum> <non-negative-fixnum>)	=> (<boolean>))
   ((<non-negative-fixnum> <list>)			=> (<boolean>)))
  (attributes
   ((_ _)		foldable effect-free)))

(declare-core-primitive conforming-version-and-version-reference?
    (safe)
  (signatures
   ((<list> <list>)	=> (<boolean>)))
  (attributes
   ((_ _)		foldable effect-free)))

(declare-core-primitive conforming-library-name-and-library-reference?
    (safe)
  (signatures
   ((<list> <list>)	=> (<boolean>)))
  (attributes
   ((_ _)		foldable effect-free)))

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
