;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for structs core primitives
;;Date: Sun Jan  3, 2016
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2016, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (typed-core-primitives structs)
  (export typed-core-primitives.structs)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.structs)


;;;; structs, safe primitives

(section

;;; predicates

(declare-core-primitive struct?
    (safe)
  (signatures
   ((<struct>)				=> (<true>))
   ((<top>)				=> (<boolean>))
   ((<struct> <struct-type-descriptor>)	=> (<boolean>)))
  (attributes
   ((_)					foldable effect-free)
   ((_ _)				foldable effect-free)))

(declare-type-predicate struct-type-descriptor? <struct-type-descriptor>)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-struct-type
    (safe)
  (signatures
   ((<string> <list>)		=> (<struct-type-descriptor>))
   ((<string> <list> <symbol>)	=> (<struct-type-descriptor>)))
  (attributes
   ((_ _)		foldable effect-free result-true)
   ((_ _ _)		foldable effect-free result-true)))


;;; --------------------------------------------------------------------
;;; comparison

(declare-struct-binary/multi-comparison struct=?)
(declare-struct-binary/multi-comparison struct!=?)

;;; --------------------------------------------------------------------
;;; struct type descriptor accessors and mutators

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<struct-type-descriptor>)		=> (?return-value-tag)))
		   (attributes
		    ((_)		foldable effect-free))))
		)))
  (declare struct-type-name		<string>)
  (declare struct-type-symbol		<symbol>)
  (declare struct-type-field-names	<list>)
  (declare struct-type-destructor	(or <false> <procedure>))
  (declare struct-type-constructor	<procedure>)
  (declare struct-type-predicate	<procedure>)
  #| end of LET-SYNTAX |# )

(declare-core-primitive struct-type-field-accessor
    (safe)
  (signatures
   ((<struct-type-descriptor> (or <non-negative-fixnum> <symbol>))	=> ((lambda (<struct>) => (<top>)))))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive struct-type-field-mutator
    (safe)
  (signatures
   ((<struct-type-descriptor> (or <non-negative-fixnum> <symbol>))	=> ((lambda (<struct> <top>) => ()))))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive struct-type-field-method
    (safe)
  (signatures
   ((<struct-type-descriptor> (or <non-negative-fixnum> <symbol>))	=> ((case-lambda
									      ((<struct>)       => (<top>))
									      ((<struct> <top>) => ())))))
  (attributes
   ((_ _)		effect-free result-true)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?new-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<struct-type-descriptor> (or <false> ?new-value-tag))	=> ()))))
		)))
  (declare set-struct-type-printer!	<procedure>)
  (declare set-struct-type-destructor!	<procedure>)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; struct instance accessors and mutators

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<struct>)		=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare struct-std		<struct-type-descriptor>)
  (declare struct-name		<string>)
  (declare struct-length	<non-negative-fixnum>)
  (declare struct-field-names	(list-of <symbol>))
  (declare struct-printer	<procedure>)
  (declare struct-destructor	(or <false> <procedure>))
  #| end of LET-SYNTAX |# )

(declare-core-primitive struct-reset!
    (safe)
  (signatures
   ((<struct>)		=> ())))

(declare-core-primitive struct-ref
    (safe)
  (signatures
   ((<struct> <non-negative-fixnum>)	=> (<top>)))
  (attributes
   ;;This cannot be foldable because the referenced field may be mutated at run-time.
   ((_ _)		effect-free)))

(declare-core-primitive struct-set!
    (safe)
  (signatures
   ((<struct> <non-negative-fixnum> <top>)	=> ())))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-parameter struct-guardian-logger	(or <boolean> <procedure>))

(declare-core-primitive struct-guardian-log
    (safe)
  (signatures
   ((<struct> <top> <symbol>)	=> ())))

/section)


;;;; structs, unsafe primitives

(section

;;; constructors

;;The base struct type descriptor is a constant created at process boot time.
(declare-core-primitive base-rtd
    (unsafe)
  (signatures
   (()					=> (<struct-type-descriptor>)))
  (attributes
   (()					effect-free result-true)))

(declare-core-primitive $struct
    (unsafe)
  (signatures
   ((<struct-type-descriptor> . <list>)	=> (<struct>)))
  (attributes
   ;;It must return a new struct every time.
   ((_ . _)				effect-free result-true)))

(declare-core-primitive $make-struct
    (unsafe)
  (signatures
   ((<struct-type-descriptor> <non-negative-fixnum>)	=> (<struct>)))
  (attributes
   ;;Not foldable: it must return a new struct every time.
   ((_ _)				effect-free result-true)))

(declare-core-primitive $make-clean-struct
    (unsafe)
  (signatures
   ((<struct-type-descriptor>)		=> (<struct>))))

;;; --------------------------------------------------------------------
;;; predicates

(declare-core-primitive $struct?
    (unsafe)
  (signatures
   ((<struct>)				=> (<true>))
   ((<top>)				=> (<boolean>)))
  (attributes
   ((_)					foldable effect-free)))

(declare-core-primitive $struct/rtd?
    (unsafe)
  (signatures
   ((<top> <struct-type-descriptor>)	=> (<boolean>)))
  (attributes
   ((_ _)				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $struct-std
    (unsafe)
  (signatures
   ((<struct>)			=> (<struct-type-descriptor>))))

(declare-core-primitive $struct-ref
    (unsafe)
  (signatures
   ((<struct> <non-negative-fixnum>)		=> (_)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $struct-set!
    (unsafe)
  (signatures
   ((<struct> <non-negative-fixnum> <void>)	=> ())
   ((<struct> <non-negative-fixnum> <top>)	=> ()))
  (attributes
   ((_ _)			foldable result-true)))

;;;

(let-syntax
    ((declare-unsafe-struct-accessor
      (syntax-rules ()
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((<struct-type-descriptor>)	=> (?return-value-tag)))
	   (attributes
	    ((_)			effect-free))))
	)))
  (declare-unsafe-struct-accessor $std-std		<struct-type-descriptor>)
  (declare-unsafe-struct-accessor $std-name		<string>)
  (declare-unsafe-struct-accessor $std-length		<non-negative-fixnum>)
  (declare-unsafe-struct-accessor $std-fields		<list>)
  (declare-unsafe-struct-accessor $std-printer		<top>)
  (declare-unsafe-struct-accessor $std-symbol		<top>)
  (declare-unsafe-struct-accessor $std-destructor	<top>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-unsafe-struct-mutator
      (syntax-rules ()
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((<struct-type-descriptor> ?new-value-tag)	=> ()))))
	)))
  (declare-unsafe-struct-mutator $set-std-std!		<struct-type-descriptor>)
  (declare-unsafe-struct-mutator $set-std-name!		<string>)
  (declare-unsafe-struct-mutator $set-std-length!	<non-negative-fixnum>)
  (declare-unsafe-struct-mutator $set-std-fields!	<list>)
  (declare-unsafe-struct-mutator $set-std-printer!	<top>)
  (declare-unsafe-struct-mutator $set-std-symbol!	<top>)
  (declare-unsafe-struct-mutator $set-std-destructor!	<top>)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $struct-guardian
    (unsafe)
  (signatures
   ((<struct>)		=> (<struct>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
