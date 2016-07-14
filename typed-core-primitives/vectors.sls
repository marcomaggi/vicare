;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for vectors core primitives
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
(library (typed-core-primitives vectors)
  (export typed-core-primitives.vectors)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.vectors)


;;;; core syntactic binding descriptors, typed safe core primitives: vectors

(section

;;; predicates

(declare-type-predicate vector?				<vector>)
(declare-type-predicate <nevector>-type-predicate	<nevector>)

(declare-vector-predicate vector-empty?)
(declare-vector-predicate non-empty-vector?)

(declare-core-primitive vectors-of-same-length?
    (safe)
  (signatures
   ((<vector> . (list-of <vector>))	=> (<boolean>))))

(declare-core-primitive list-of-vectors-of-same-length?
    (safe)
  (signatures
   (((list-of <vector>))		=> (<boolean>))))

(declare-core-primitive list-of-vectors?
    (safe)
  (signatures
   (((list-of <vector>))		=> (<boolean>))))

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive vector
    (safe)
  (signatures
   (()				=> (<empty-vector>))
   ((<top> . (list-of <top>))	=> (<nevector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (()				effect-free result-true)
   (_				effect-free result-true)))

(declare-core-primitive <nevector>-constructor
    (safe)
  (signatures
   ((<top> . <list>)		=> (<nevector>))))

(declare-core-primitive subvector
    (safe)
  (signatures
   ((<vector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive make-vector
    (safe)
  (signatures
   ((<positive-fixnum>)			=> (<nevector>))
   ((<positive-fixnum> <top>)		=> (<nevector>))
   ((<zero-fixnum>)			=> (<empty-vector>))
   ((<zero-fixnum> <top>)		=> (<empty-vector>))
   ((<non-negative-fixnum>)		=> (<vector>))
   ((<non-negative-fixnum> <top>)	=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive vector-resize
    (safe)
  (signatures
   ((<vector> <non-negative-fixnum>)		=> (<vector>))
   ((<vector> <non-negative-fixnum> <top>)	=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive vector-reset!
    (safe)
  (signatures
   ((<vector>)							=> (<void>))
   ((<vector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<void>))))

(declare-core-primitive vector-append
    (safe)
  (signatures
   ((list-of <vector>)		=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive vector-copy
    (safe)
  (signatures
   ((<vector>)			=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive vector-length
    (safe)
  (signatures
   ((<nevector>)		=> (<positive-fixnum>))
   ((<empty-vector>)		=> (<zero-fixnum>))
   ((<vector>)			=> (<non-negative-fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $vector-length))

;;; --------------------------------------------------------------------
;;; accessors and mutators

;;FIXME  This cannot  have $VECTOR-REF  as  replacement because  there is  no way  to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-ref
    (safe)
  (signatures
   ((<nevector> <non-negative-fixnum>)	=> (<top>)))
  (attributes
   ((_ _)		foldable effect-free)))

;;FIXME This  cannot have  $VECTOR-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-set!
    (safe)
  (signatures
   ((<nevector> <non-negative-fixnum> _)	=> (<void>)))
  (attributes
   ((_ _ _)			result-true)))

(declare-core-primitive vector-copy!
    (safe)
  (signatures
   ((<vector> <non-negative-fixnum> <vector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<void>)))
  ;;Not foldable  because it must return  a newly allocated vector.   Not effect free
  ;;because it mutates the operand.
  (attributes
   ((_ _ _ _ _)			result-true)))

(declare-core-primitive vector-fill!
    (safe)
  (signatures
   ((<vector> <top>)		=> (<void>)))
  ;;Not effect free because it mutates the operand.
  (attributes
   ((_ _)			foldable result-true)))

;;; --------------------------------------------------------------------
;;; sorting

(declare-core-primitive vector-sort
    (safe)
  (signatures
   ((<procedure> <vector>)	=> (<vector>)))
  (attributes
   ;;Not foldable  because it  must return  a new vector  at every  application.  Not
   ;;effect-free because it invokes an unknown procedure.
   ((_ _)			result-true)))

(declare-core-primitive vector-sort!
    (safe)
  (signatures
   ((<procedure> <vector>)	=> (<void>)))
  (attributes
   ;;Not foldable and not effect-free because  it invokes an unknown procedure and it
   ;;mutates the operand.
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive vector-map
    (safe)
  (signatures
   ((<procedure> <vector> . (list-of <vector>))		=> (<vector>)))
  (attributes
   ;;Not foldable  because it  must return  a new vector  at every  application.  Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-for-each
    (safe)
  (signatures
   ((<procedure> <vector> . (list-of <vector>))		=> (<void>)))
  (attributes
   ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-for-all
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((<procedure> <vector> . (list-of <vector>))		=> (<top>))))

(declare-core-primitive vector-exists
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((<procedure> <vector> . (list-of <vector>))		=> (<top>))))

(declare-core-primitive vector-find
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((<procedure> <vector>)			=> (<top>))))

(declare-core-primitive vector-fold-left
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((<procedure> <top> <vector> . (list-of <vector>))	=> (<top>))))

(declare-core-primitive vector-fold-right
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((<procedure> <top> <vector> . (list-of <vector>))	=> (<top>))))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive vector->list
    (safe)
  (signatures
   ((<vector>)			=> (<list>)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_)				effect-free result-true)))

/section)


;;;; core syntactic binding descriptors, typed unsafe core primitives: vectors

(section

;;; constructors

(declare-core-primitive $make-vector
    (unsafe)
  (signatures
   ((<non-negative-fixnum>)			=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive $make-clean-vector
    (unsafe)
  (signatures
   ((<non-negative-fixnum>)			=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive $subvector
    (unsafe)
  (signatures
   ((<vector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<vector>)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-vector-predicate $vector-empty? unsafe)

(declare-core-primitive $vectors-of-same-length?
    (unsafe)
  (signatures
   ((<vector> . (list-of <vector>))	=> (<boolean>))))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $vector-length
    (unsafe)
  (signatures
   ((<vector>)			=> (<non-negative-fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $vector-ref
    (unsafe)
  (signatures
   ((<nevector> <non-negative-fixnum>)		=> (<top>)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $vector-set!
    (unsafe)
  (signatures
   ((<nevector> <non-negative-fixnum> <void>)	=> (<void>))
   ((<nevector> <non-negative-fixnum> _)	=> (<void>)))
  (attributes
   ((_ _ _)			result-true)))

(declare-core-primitive $vector-set-void!
    (unsafe)
  (signatures
   ((<nevector> <non-negative-fixnum>)		=> (<void>)))
  (attributes
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive $vector-map1
    (unsafe)
  (signatures
   ((<procedure> <vector>)	=> (<vector>)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-each1
    (unsafe)
  (signatures
   ((<procedure> <vector>)	=> (<void>)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-all1
    (unsafe)
  (signatures
   ((<procedure> <vector>)	=> (<top>))))

(declare-core-primitive $vector-exists1
    (unsafe)
  (signatures
   ((<procedure> <vector>)	=> (<top>))))

(declare-core-primitive $vector-self-copy-forwards!
    (unsafe)
  (signatures
   ((<vector> <non-negative-fixnum> <non-negative-fixnum> <non-negative-fixnum>)
    => (<vector>))))

(declare-core-primitive $vector-self-copy-backwards!
    (unsafe)
  (signatures
   ((<vector> <non-negative-fixnum> <non-negative-fixnum> <non-negative-fixnum>)
    => (<vector>))))

(declare-core-primitive $vector-copy-source-range!
    (unsafe)
  (signatures
   ((<vector> <non-negative-fixnum> <non-negative-fixnum> <vector> <non-negative-fixnum>)
    => (<vector>))))

(declare-core-primitive $vector-copy-source-count!
    (unsafe)
  (signatures
   ((<vector> <non-negative-fixnum> <vector> <non-negative-fixnum> <non-negative-fixnum>)
    => (<vector>))))

(declare-core-primitive $fill-vector-from-list!
    (unsafe)
  (signatures
   ((<vector> <non-negative-fixnum> <list>)	=> (<vector>))))

/section)


;;;; core syntactic binding descriptors, typed OOP safe core primitives: vectors

(section

(declare-core-primitive <vector>-map
    (safe)
  (signatures
   ((<vector> <procedure> . (list-of <vector>))	=> (<vector>))))

(declare-core-primitive <vector>-for-each
    (safe)
  (signatures
   ((<vector> <procedure> . (list-of <vector>))	=> (<void>))))

(declare-core-primitive <vector>-for-all
    (safe)
  (signatures
   ((<vector> <procedure> . (list-of <vector>))	=> (<top>))))

(declare-core-primitive <vector>-exists
    (safe)
  (signatures
   ((<vector> <procedure> . (list-of <vector>))	=> (<top>))))

(declare-core-primitive <vector>-find
    (safe)
  (signatures
   ((<vector> <procedure>>)		=> (<top>))))

(declare-core-primitive <vector>-fold-left
    (safe)
  (signatures
   ((<vector> <procedure> <top> . (list-of <vector>))	=> (<top>))))

(declare-core-primitive <vector>-fold-right
    (safe)
  (signatures
   ((<vector> <procedure> <top> . (list-of <vector>))	=> (<top>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
