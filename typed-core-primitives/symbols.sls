;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for symbols core primitives
;;Date: Tue Dec 22, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (typed-core-primitives symbols)
  (export typed-core-primitives.symbols)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.symbols)


;;;; core syntactic binding descriptors, typed core primitives: safe symbol primitives

(section

(declare-type-predicate symbol? <symbol>)

(declare-core-primitive symbol->string
    (safe)
  (signatures
   ((<symbol>) => (<string>))))

(declare-list-of-type-predicate list-of-symbols?	<symbol>)

;;Commented out because it is not an exported primitive.
;;
;; (declare-core-primitive string-or-symbol?
;;     (safe)
;;   (signatures
;;    ((<string>)			=> (<true>))
;;    ((<symbol>)			=> (<true>))
;;    ((<top>)			=> (<boolean>))))

;;; --------------------------------------------------------------------
;;; comparison

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((list-of <symbol>)		=> (<boolean>)))
		   (attributes
		    (_				foldable effect-free result-true)))))))
  (declare symbol=?)
  (declare symbol!=?)
  (declare symbol<?)
  (declare symbol>?)
  (declare symbol<=?)
  (declare symbol>=?)
  #| end of LET-SYNTAX |# )

(declare-core-primitive symbol-max
    (safe)
  (signatures
   ((<symbol> . (list-of <symbol>))		=> (<symbol>))))

(declare-core-primitive symbol-min
    (safe)
  (signatures
   ((<symbol> . (list-of <symbol>))		=> (<symbol>))))

;;; --------------------------------------------------------------------
;;; gensyms

(declare-type-predicate gensym?			<gensym>)

(declare-core-primitive gensym
    (safe)
  (signatures
   (()				=> (<gensym>))
   ((<symbol>)			=> (<gensym>))
   ((<string>)			=> (<gensym>)))
  (attributes
   ;;It must return a new gensym every time.
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive gensym->unique-string
    (safe)
  (signatures
   ((<gensym>)			=> (<string>)))
  (attributes
   ;;Once a  gensym has been  created, its unique  string is determined  forever.  So
   ;;this is foldable.
   ((_)				foldable effect-free result-true)))

(declare-parameter print-gensym)
(declare-parameter gensym-prefix)
(declare-parameter gensym-count)

;;; --------------------------------------------------------------------
;;; properties

(declare-core-primitive putprop
    (safe)
  (signatures
   ((<symbol> <symbol> <top>)	=> (<void>))))

(declare-core-primitive getprop
    (safe)
  (signatures
   ((<symbol> <symbol>)		=> (<top>))))

(declare-core-primitive remprop
    (safe)
  (signatures
   ((<symbol> <symbol>)		=> (<void>))))

(declare-core-primitive property-list
    (safe)
  (signatures
   ((<symbol>)			=> (<list>))))

;;; --------------------------------------------------------------------

(declare-core-primitive symbol-bound?
    (safe)
  (signatures
   ((<symbol>)			=> (<boolean>)))
  (attributes
   ;;Being bound or not is a run-time property; this is *not* foldable.
   ((_)				effect-free)))

(declare-core-primitive top-level-value
    (safe)
  (signatures
   ((<symbol>)			=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive reset-symbol-proc!
    (safe)
  (signatures
   ((<symbol>)			=> (<void>)))
  (attributes
   ((_)				result-true)))

(declare-core-primitive set-symbol-value!
    (safe)
  (signatures
   ((<symbol> <top>)		=> (<void>)))
  (attributes
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive symbol-value
    (safe)
  (signatures
   ((<symbol>)			=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive system-label
    (safe)
  (signatures
   ((<symbol>)			=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive system-id
    (safe)
  (signatures
   ((<symbol>)			=> (<top>)))
  (attributes
   ((_)				effect-free)))

;;; --------------------------------------------------------------------

;;Already defined in "hash-tables.sls".
#;(declare-hash-function symbol-hash <symbol> safe)

/section)


;;;; symbols, unsafe primitives

(section

(declare-core-primitive $make-symbol
    (unsafe)
  (signatures
   ((<false>)			=> (<symbol>))
   ((<string>)			=> (<symbol>)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; components

(let-syntax
    ((declare-symbol-accessor (syntax-rules ()
				((_ ?who ?rv-tag)
				 (declare-core-primitive ?who
				     (unsafe)
				   (signatures
				    ((<symbol>)		=> (?rv-tag)))
				   (attributes
				    ((_)		effect-free))))
				)))
  (declare-symbol-accessor $symbol-plist		<list>)
  (declare-symbol-accessor $symbol-proc			<top>)
  (declare-symbol-accessor $symbol-string		<string>)
  (declare-symbol-accessor $symbol-unique-string	<string>)
  (declare-symbol-accessor $symbol-value		<top>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-symbol-mutator (syntax-rules ()
			       ((_ ?who ?obj-tag)
				(declare-core-primitive ?who
				    (unsafe)
				  (signatures
				   ((<symbol> ?obj-tag)	=> (<void>)))))
			       )))
  (declare-symbol-mutator $set-symbol-value!		<top>)
  (declare-symbol-mutator $set-symbol-proc!		<top>)
  (declare-symbol-mutator $set-symbol-string!		<string>)
  (declare-symbol-mutator $set-symbol-plist!		<list>)
  #;(declare-symbol-mutator $set-symbol-unique-string!	<string/false>)
  #| end of LET-SYNTAX |# )

(declare-core-primitive ?who
    (unsafe)
  (signatures
   ((<symbol> <string>)	=> (<void>))
   ((<symbol> <false>)	=> (<void>))))

;;; --------------------------------------------------------------------
;;; property lists

(declare-core-primitive $putprop
    (unsafe)
  (signatures
   ((<symbol> <symbol> _) => (<void>)))
  (attributes
   ((_ _ _)		result-true)))

(declare-core-primitive $getprop
    (unsafe)
  (signatures
   ((<symbol> <symbol>) => (<void>)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive $remprop
    (unsafe)
  (signatures
   ((<symbol> <symbol>) => (<void>)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive $property-list
    (unsafe)
  (signatures
   ((<symbol>) => (_)))
  (attributes
   ((_)			effect-free	result-true)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $symbol->string
    (unsafe)
  (signatures
   ((<symbol>)			=> (<string>)))
  (attributes
   ((_)				foldable effect-free result-true)))

/section)


;;;; symbols, object utilities

(section

(declare-core-primitive <symbol>-value
    (safe)
  (signatures
   ((<symbol>)		=> (<top>))
   ((<symbol> <top>)	=> (<void>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
