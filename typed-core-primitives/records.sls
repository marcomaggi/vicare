;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for records core primitives
;;Date: Sat Jan  2, 2016
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
(library (typed-core-primitives records)
  (export typed-core-primitives.records)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.records)


;;;; R6RS record type descriptors, safe primitives

(section

(declare-type-predicate record-type-descriptor?	<record-type-descriptor>)

(declare-type-predicate record-constructor-descriptor? <record-constructor-descriptor>)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-record-type-descriptor
    (safe)
  (signatures
   ;;name parent uid sealed? opaque? fields
   #;((<symbol> (or <false> <record-type-descriptor>) (or <false> <symbol>) _ _ <vector>) => (<record-type-descriptor>))
   ((<symbol> <record-type-descriptor> <symbol> _ _ <vector>) => (<record-type-descriptor>))
   ((<symbol> <record-type-descriptor> <false>  _ _ <vector>) => (<record-type-descriptor>))
   ((<symbol> <false>                  <symbol> _ _ <vector>) => (<record-type-descriptor>))
   ((<symbol> <false>                  <false>  _ _ <vector>) => (<record-type-descriptor>)))
  (attributes
   ((_ _ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-record-constructor-descriptor
    (safe)
  (signatures
   #;((<record-type-descriptor> (or <false> <record-constructor-descriptor>) (or <false> <procedure>)) => (<record-constructor-descriptor>))
   ((<record-type-descriptor> <record-constructor-descriptor> <procedure>) => (<record-constructor-descriptor>))
   ((<record-type-descriptor> <record-constructor-descriptor> <false>)     => (<record-constructor-descriptor>))
   ((<record-type-descriptor> <false> <procedure>)                         => (<record-constructor-descriptor>))
   ((<record-type-descriptor> <false> <false>)                             => (<record-constructor-descriptor>)))
  (attributes
   ((_ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; procedures generation

(declare-core-primitive record-constructor
    (safe)
  (signatures
   ((<record-constructor-descriptor>)	=> (<procedure>)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive record-predicate
    (safe)
  (signatures
   ((<record-type-descriptor>)		=> (<procedure>)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive record-accessor
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <false>)	=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <symbol>)	=> (<procedure>))
   ((<record-type-descriptor> <symbol>)				=> (<procedure>))
   ((<record-type-descriptor> <symbol> <false>)			=> (<procedure>))
   ((<record-type-descriptor> <symbol> <symbol>)		=> (<procedure>)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive record-mutator
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <false>)	=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <symbol>)	=> (<procedure>))
   ((<record-type-descriptor> <symbol>)				=> (<procedure>))
   ((<record-type-descriptor> <symbol> <false>)			=> (<procedure>))
   ((<record-type-descriptor> <symbol> <symbol>)		=> (<procedure>)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive unsafe-record-accessor
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <false>)	=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <symbol>)	=> (<procedure>))
   ((<record-type-descriptor> <symbol>)				=> (<procedure>))
   ((<record-type-descriptor> <symbol> <false>)			=> (<procedure>))
   ((<record-type-descriptor> <symbol> <symbol>)		=> (<procedure>)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive unsafe-record-mutator
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <false>)	=> (<procedure>))
   ((<record-type-descriptor> <non-negative-fixnum> <symbol>)	=> (<procedure>))
   ((<record-type-descriptor> <symbol>)				=> (<procedure>))
   ((<record-type-descriptor> <symbol> <false>)			=> (<procedure>))
   ((<record-type-descriptor> <symbol> <symbol>)		=> (<procedure>)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive record-field-mutable?
    (safe)
  (signatures
   ((<record-type-descriptor> <non-negative-fixnum>)		=> (<boolean>)))
  (attributes
   ((_ _)		effect-free)))

;;;

(declare-core-primitive record-type-generative?
    (safe)
  (signatures
   ((<record-type-descriptor>)		=> (<boolean>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive record-type-opaque?
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> (<boolean>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-sealed?
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> (<boolean>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-field-names
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> (<vector>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive record-type-name
    (safe)
  (signatures
   ((<record-type-descriptor>)	=> (<symbol>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive record-type-parent
    (safe)
  (signatures
   #;((<record-type-descriptor>)	=> ((or <false> <record-type-descriptor>)))
   ((<record-type-descriptor>)		=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-uid
    (safe)
  (signatures
   #;((<record-type-descriptor>)	=> ((or <false> <symbol>)))
   ((<record-type-descriptor>)		=> (<top>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-all-field-names
    (safe)
  (signatures
   ((<record-type-descriptor>)		=> ((vector-of <symbol>)))))

;;; --------------------------------------------------------------------
;;; destructor

(declare-core-primitive record-destructor
    (safe)
  (signatures
   #;((<record-type-descriptor>)	=> ((or <false> <procedure>)))
   ((<record-type-descriptor>)		=> (<top>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive record-destructor-set!
    (safe)
  (signatures
   ((<record-type-descriptor> <procedure>)	=> (<void>)))
  (attributes
   ((_ _)			result-true)))

/section)


;;;; R6RS records, safe primitives

(section

(declare-type-predicate record? <record>)

(declare-type-predicate record-object? <record>)

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive record-and-rtd?
    (safe)
  (signatures
   ((<record> <record-type-descriptor>)		=> (<boolean>)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive record-rtd
    (safe)
  (signatures
   ((<record>)		=> (<record-type-descriptor>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive record-reset
    (safe)
  (signatures
   ((<record>)		=> (<void>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive record-guardian-log
    (safe)
  (signatures
   ((<record> _ <symbol>)	=> (<void>)))
  (attributes
   ((_ _ _)			result-true)))

#;(declare-parameter record-guardian-logger	(or <boolean> <procedure>))
(declare-parameter record-guardian-logger	<top>)

/section)


;;;; R6RS records, unsafe primitives

(section

(declare-core-primitive $record-constructor
    (unsafe)
  (signatures
   ((<record-constructor-descriptor>)		=> (<procedure>)))
  (attributes
   ((_)						effect-free result-true)))

(declare-core-primitive $record-and-rtd?
    (unsafe)
  (signatures
   ((<record> <record-type-descriptor>)		=> (<boolean>)))
  (attributes
   ((_)						effect-free)))

(declare-core-primitive $rtd-subtype?
    (unsafe)
  (signatures
   ((<record-type-descriptor> <record-type-descriptor>)		=> (<boolean>)))
  (attributes
   ((_)						effect-free)))

(declare-core-primitive $record-ref
    (unsafe)
  (signatures
   ((<record> <non-negative-fixnum>)		=> (<top>))))

(declare-core-primitive $record-type-destructor
    (unsafe)
  (signatures
   ((<record-type-descriptor>)			=> ((union <false> <procedure>)))))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $record-guardian
    (unsafe)
  (signatures
   ((<record>)		=> (<void>))))

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
