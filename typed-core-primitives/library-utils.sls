;; -*- coding: utf-8-unix -*-
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


;;;; library objects, safe procedures

(section

(declare-type-predicate library?	<library>)

;;Accessors for "<library>" objects.
(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<library>)	=> (<top>)))))
		((_ ?who ?retval-type)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<library>)	=> (?retval-type))))))))
  (declare library-uid				<symbol>)
  (declare library-name				<library-name>)
  (declare library-imp-lib*			(list-of <library>))
  (declare library-vis-lib*			(list-of <library>))
  (declare library-inv-lib*			(list-of <library>))
  (declare library-export-subst			<list>)
  (declare library-global-env			<list>)
  (declare library-typed-locs			<list>)
  (declare library-visit-state)
  (declare library-invoke-state)
  (declare library-visit-code)
  (declare library-invoke-code)
  (declare library-guard-code)
  (declare library-guard-lib*			(list-of <library>))
  (declare library-visible?			<boolean>)
  (declare library-source-file-name		(or <false> <string>))
  (declare library-option*			<list>)
  (declare library-loaded-from-source-file?	<boolean>)
  (declare library-loaded-from-binary-file?	<boolean>)
  #| end of LET-SYNTAX |# )

(declare-core-primitive library-descriptor?
    (safe)
  (signatures
   ((<top>)		=> (<boolean>))))

(declare-core-primitive library-descriptor-uid
    (safe)
  (signatures
   ((<library-descriptor>)	=> (<symbol>))))

(declare-core-primitive library-descriptor-name
    (safe)
  (signatures
   ((<library-descriptor>)	=> (<library-name>))))

/section)


;;;; library operations, safe procedures

(section

(declare-parameter current-library-locator)
(declare-parameter run-time-library-locator)
(declare-parameter compile-time-library-locator)
(declare-parameter source-library-locator)

(declare-parameter current-library-source-search-path-scanner)
(declare-parameter current-library-binary-search-path-scanner)

(declare-core-primitive default-library-source-search-path-scanner
    (safe)
  (signatures
   ((<library-reference>)	=> ((or <false> <string>) (or <false> <procedure>)))))

(declare-core-primitive default-library-binary-search-path-scanner
    (safe)
  (signatures
   ((<library-reference>)	=> ((or <false> <string>) (or <false> <procedure>)))))

;;; --------------------------------------------------------------------

(declare-core-primitive directory+library-stem->library-source-pathname
    (safe)
  (signatures
   ((<string> <string>)		=> (<string>))))

(declare-core-primitive directory+library-stem->library-binary-pathname
    (safe)
  (signatures
   ((<string> <string>)		=> (<string>))))

;;; --------------------------------------------------------------------

(declare-core-primitive find-library-by-name
    (safe)
  (signatures
   ((<library-name>)		=> (<library>))))

(declare-core-primitive find-library-by-reference
    (safe)
  (signatures
   ((<library-reference>)	=> (<library>))))

(declare-core-primitive find-library-by-descriptor
    (safe)
  (signatures
   ((<library-descriptor>)	=> (<library>))))

(declare-core-primitive find-library-in-collection-by-predicate
    (safe)
  (signatures
   ((<procedure>)		=> ((or <false> <library>)))))

(declare-core-primitive find-library-in-collection-by-name
    (safe)
  (signatures
   ((<library-name>)		=> ((or <false> <library>)))))

(declare-core-primitive find-library-in-collection-by-reference
    (safe)
  (signatures
   ((<library-reference>)	=> ((or <false> <library>)))))

(declare-core-primitive find-library-in-collection-by-descriptor
    (safe)
  (signatures
   ((<library-descriptor>)	=> ((or <false> <library>)))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
