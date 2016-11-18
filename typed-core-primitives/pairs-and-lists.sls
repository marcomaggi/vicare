;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for pairs and lists core primitives
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
(library (typed-core-primitives pairs-and-lists)
  (export typed-core-primitives.pairs-and-lists)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.pairs-and-lists)


;;;; pairs and lists, safe functions

(section

;;; predicates

(declare-type-predicate null?			<null>)
(declare-type-predicate <nelist>-type-predicate	<nelist>)

(declare-core-primitive pair?
    (safe)
  (signatures
   ((<pair>)				=> (<true>))
   ((<null>)				=> (<false>))
   (((or <list>
	 (ancestor-of <list>)
	 (ancestor-of <pair>)))		=> (<boolean>))
   (((and (not <pair>)
	  (not <list>)))		=> (<false>)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive list?
    (safe)
  (signatures
   ((<list>)				=> (<true>))
   (((or <pair>
	 (ancestor-of <list>)
	 (ancestor-of <pair>)))		=> (<boolean>))
   (((and (not <list>)
	  (not <pair>)))		=> (<false>)))
  (attributes
   ((())		foldable effect-free result-true)
   ((_)			foldable effect-free)))

(declare-core-primitive circular-list?
    (safe)
  (signatures
   ((<list>)				=> (<false>))
   (((or <pair>
	 (ancestor-of <pair>)))		=> (<boolean>))
   (((not <pair>))			=> (<false>)))
  (attributes
   ((())		foldable effect-free result-false)
   ((_)			foldable effect-free)))

(declare-core-primitive list-of-single-item?
    (safe)
  (signatures
   (((pair <top> <null>))		=> (<true>))
   (((pair <top> (not <null>)))		=> (<false>))
   ((<pair>)				=> (<boolean>))
   ((<list>)				=> (<boolean>))
   (((and (not <pair>) (not <list>)))	=> (<false>))))

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive cons
    (safe)
  (signatures
   ((<top> <list>)			=> (<nelist>))
   ((_ _)				=> (<pair>)))
  (attributes
   ;;This is not foldable because it must return a newly allocated pair every time.
   ((_ _)			effect-free result-true)))

(declare-core-primitive cons*
    (safe)
  (signatures
   ((<top>)				=> (<top>))
   ;;Common cases.
   ((<top> <list>)			=> (<nelist>))
   ((<top> <top> <list>)		=> (<nelist>))
   ((<top> <top>)			=> (<pair>))
   ;;Here the result may be an improper list.
   ((<top> <top> <top> . <list>)	=> (<pair>)))
  (attributes
   ;;This will return the operand itself, so it is foldable.
   ((_)			foldable effect-free)
   ;;This is not foldable because it must return a newly allocated list every time.
   ((_ _ . _)		effect-free result-true)))

(declare-core-primitive list
    (safe)
  (signatures
   (()			=> (<null>))
   ((_ . _)		=> (<nelist>)))
  (attributes
   ;;Foldable because it returns null.
   (()			foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_ . _)		effect-free result-true)))

(declare-core-primitive make-list
    (safe)
  (signatures
   ((<positive-fixnum>)			=> (<nelist>))
   ((<positive-fixnum> <top>)		=> (<nelist>))
   ((<non-negative-fixnum>)		=> (<list>))
   ((<non-negative-fixnum> <top>)	=> (<list>)))
  (attributes
   ;;Foldable because it returns null.
   ((0)				foldable effect-free result-true)
   ((0 _)			foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive reverse
    (safe)
  (signatures
   ((<null>)			=> (<null>))
   ((<nelist>)			=> (<nelist>))
   ((<list>)			=> (<list>)))
  (attributes
   ;;This is foldable because it returns null itself.
   ((())			foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_)				effect-free result-true)))

(declare-core-primitive append
    (safe)
  (signatures
   (()					=> (<null>))
   ((list-of <list>)			=> (<list>))
   ;;In general the result may be an improper list.
   (<list>				=> (<top>)))
  (attributes
   ;;This is foldable because it returns null itself.
   (()				foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated improper list every time.
   ((_ . _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive length
    (safe)
  (signatures
   ((<null>)			=> (<zero-fixnum>))
   ((<nelist>)			=> (<positive-fixnum>))
   ((<list>)			=> (<non-negative-fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;;

(declare-list-finder memq <top>)
(declare-list-finder memv <top>)
(declare-list-finder member <top>)

(declare-core-primitive memp
    (safe)
  (signatures
   ((<procedure> <null>)		=> (<false>))
   ((<procedure> <list>)		=> (_)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

;;;

(declare-core-primitive remp
    (safe)
  (signatures
   ((<procedure> <null>)		=> (<null>))
   ((<procedure> <list>)		=> (<list>)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)
   ((_ _)				result-true)))

(declare-core-primitive remq
    (safe)
  (signatures
   ((<top> <null>)			=> (<null>))
   ((<top> <list>)			=> (<list>)))
  (attributes
   ((_ ())				foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive remv
    (safe)
  (signatures
   ((<top> <null>)			=> (<null>))
   ((<top> <list>)			=> (<list>)))
  (attributes
   ((_ ())				foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive remove
    (safe)
  (signatures
   ((<top> <null>)			=> (<null>))
   ((<top> <list>)			=> (<list>)))
  (attributes
   ((_ ())				foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)))

;;;

(declare-core-primitive last-pair
    (safe)
  (signatures
   ((<nelist>)				=> (<pair>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive list-tail
    (safe)
  (signatures
   ;;If the index is out of range: an exception is raised.
   ((<nelist> <non-negative-exact-integer>)		=> (<list>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive list-ref
    (safe)
  (signatures
   ;;If the index is out of range: an exception is raised.
   ((<nelist> <non-negative-exact-integer>)		=> (<top>)))
  (attributes
   ((_ _)				foldable effect-free)))

;;;

(declare-core-primitive map
    (safe)
  (signatures
   (((lambda <bottom> => (<top>)) <null> . (list-of <null>))	=> (<null>))
   (((lambda <bottom> => (<top>)) <list> . <list>)		=> (<list>)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)
   ((_ _ . _)				result-true)))

(declare-core-primitive for-each
    (safe)
  (signatures
   ((<procedure> <null> . (list-of <null>))	=> ())
   ((<procedure> <list> . <list>)		=> ()))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free)))

;;;

(declare-core-primitive find
    (safe)
  (signatures
   (((lambda (<bottom>) => (<top>)) <null>)		=> (<false>))
   (((lambda (<bottom>) => (<top>)) <list>)		=> (<top>)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

(declare-core-primitive exists
    (safe)
  (signatures
   (((lambda <bottom> => (<top>)) <null> . (list-of <null>))	=> (<false>))
   (((lambda <bottom> => (<top>)) <list> . <list>)		=> (<top>)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

(declare-core-primitive for-all
    (safe)
  (signatures
   (((lambda <bottom> => (<top>)) <null> . (list-of <null>))	=> (<true>))
   (((lambda <bottom> => (<top>)) <list> . <list>)		=> (<top>)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)))

(declare-core-primitive filter
    (safe)
  (signatures
   (((lambda (<bottom>) => (<top>)) <null>)		=> (<null>))
   (((lambda (<bottom>) => (<top>)) <list>)		=> (<list>)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)))

(declare-core-primitive partition
    (safe)
  (signatures
   (((lambda (<bottom>) => (<top>)) <null>)		=> (<null> <null>))
   (((lambda (<bottom>) => (<top>)) <list>)		=> (<list> <list>)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free)))

(declare-core-primitive fold-left
    (safe)
  (signatures
   (((lambda <bottom> => (<top>)) <top> <list> . <list>)	=> (<top>)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ _ ())				foldable effect-free)))

(declare-core-primitive fold-right
    (safe)
  (signatures
   (((lambda <bottom> => (<top>)) <top> <list> . <list>)	=> (<top>)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ _ ())			foldable effect-free)))

(declare-core-primitive andmap
    (safe)
  (signatures
   (((lambda <bottom> => (<top>)) <null>)		=> (<true>))
   (((lambda <bottom> => (<top>)) <list>)		=> (<top>))
   (((lambda <bottom> => (<top>)) <null> <null>)		=> (<true>))
   (((lambda <bottom> => (<top>)) <list> <list>)		=> (<top>)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)
   ((_ () ())				foldable effect-free result-true)))

(declare-core-primitive ormap
    (safe)
  (signatures
   (((lambda <bottom> => (<top>)) <null>)		=> (<false>))
   (((lambda <bottom> => (<top>)) <list>)		=> (<top>)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

(declare-core-primitive list-sort
    (safe)
  (signatures
   (((lambda (<bottom> <bottom>) => (<top>)) <list>)		=> (<list>)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive car
    (safe)
  (signatures
   ((<pair>)		=> (<top>))
   ((<nelist>)		=> (<top>)))
  #| end of DECLARE-CORE-PRIMITIVE |# )

(declare-core-primitive cdr
    (safe)
  (signatures
   ((<pair>)		=> (<top>))
   ((<nelist>)		=> (<top>)))
  #| end of DECLARE-CORE-PRIMITIVE |# )

(declare-pair-accessor caar)
(declare-pair-accessor cadr)
(declare-pair-accessor cdar)
(declare-pair-accessor cddr)
(declare-pair-accessor caaar)
(declare-pair-accessor caadr)
(declare-pair-accessor cadar)
(declare-pair-accessor caddr)
(declare-pair-accessor cdaar)
(declare-pair-accessor cdadr)
(declare-pair-accessor cddar)
(declare-pair-accessor cdddr)
(declare-pair-accessor caaaar)
(declare-pair-accessor caaadr)
(declare-pair-accessor caadar)
(declare-pair-accessor caaddr)
(declare-pair-accessor cadaar)
(declare-pair-accessor cadadr)
(declare-pair-accessor caddar)
(declare-pair-accessor cadddr)
(declare-pair-accessor cdaaar)
(declare-pair-accessor cdaadr)
(declare-pair-accessor cdadar)
(declare-pair-accessor cdaddr)
(declare-pair-accessor cddaar)
(declare-pair-accessor cddadr)
(declare-pair-accessor cdddar)
(declare-pair-accessor cddddr)

;;; --------------------------------------------------------------------
;;; mutators

(declare-pair-mutator set-car!
  (replacements $set-car!))

(declare-pair-mutator set-cdr!
  (replacements $set-cdr!))

;;; --------------------------------------------------------------------
;;; associative lists

(declare-alist-accessor assq <top>)
(declare-alist-accessor assv <top>)
(declare-alist-accessor assoc <top>)

(declare-core-primitive assp
    (safe)
  (signatures
   (((lambda (<bottom>) => (<top>)) <null>)		=> (<false>))
   (((lambda (<bottom>) => (<top>)) <list>)		=> (_)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; weak pairs

(declare-core-primitive weak-cons
    (safe)
  (signatures
   ((_ _)		=> (<pair>)))
  (attributes
   ;;This is not foldable because it must return a newly allocated pair every time.
   ((_ _)		effect-free result-true)))

(declare-core-primitive weak-pair?
    (safe)
  (signatures
   ((<null>)		=> (<false>))
   ((<pair>)		=> (<boolean>))
   ((_)			=> (<boolean>)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive list->string
    (safe)
  (signatures
   ((<null>)				=> (<empty-string>))
   (((pair <char> (list-of <char>)))	=> (<nestring>)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   ((_)				effect-free result-true)))

(declare-core-primitive list->vector
    (safe)
  (signatures
   ((<null>)		=> (<empty-vector>))
   ((<nelist>)		=> (<nevector>))
   ((<list>)		=> (<vector>)))
  (attributes
   ;;Not foldable because it must return a new vector every time.
   ((_)				effect-free result-true)))

(let-syntax
    ((declare-list->bytevector-conversion
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((<null>)		=> (<empty-bytevector>))
	    ((<nelist>)		=> (<nebytevector>)))
	   (attributes
	    ;;Not foldable because it must return a new bytevector every time.
	    ((_)				effect-free result-true))))
	)))
  (declare-list->bytevector-conversion c4b-list->bytevector)
  (declare-list->bytevector-conversion c4l-list->bytevector)
  (declare-list->bytevector-conversion c4n-list->bytevector)
  (declare-list->bytevector-conversion c8b-list->bytevector)
  (declare-list->bytevector-conversion c8l-list->bytevector)
  (declare-list->bytevector-conversion c8n-list->bytevector)
  (declare-list->bytevector-conversion f4b-list->bytevector)
  (declare-list->bytevector-conversion f4l-list->bytevector)
  (declare-list->bytevector-conversion f4n-list->bytevector)
  (declare-list->bytevector-conversion f8b-list->bytevector)
  (declare-list->bytevector-conversion f8l-list->bytevector)
  (declare-list->bytevector-conversion f8n-list->bytevector)
  (declare-list->bytevector-conversion s16b-list->bytevector)
  (declare-list->bytevector-conversion s16l-list->bytevector)
  (declare-list->bytevector-conversion s16n-list->bytevector)
  (declare-list->bytevector-conversion s32b-list->bytevector)
  (declare-list->bytevector-conversion s32l-list->bytevector)
  (declare-list->bytevector-conversion s32n-list->bytevector)
  (declare-list->bytevector-conversion s64b-list->bytevector)
  (declare-list->bytevector-conversion s64l-list->bytevector)
  (declare-list->bytevector-conversion s64n-list->bytevector)
  (declare-list->bytevector-conversion s8-list->bytevector)
  (declare-list->bytevector-conversion u16b-list->bytevector)
  (declare-list->bytevector-conversion u16l-list->bytevector)
  (declare-list->bytevector-conversion u16n-list->bytevector)
  (declare-list->bytevector-conversion u32b-list->bytevector)
  (declare-list->bytevector-conversion u32l-list->bytevector)
  (declare-list->bytevector-conversion u32n-list->bytevector)
  (declare-list->bytevector-conversion u64b-list->bytevector)
  (declare-list->bytevector-conversion u64l-list->bytevector)
  (declare-list->bytevector-conversion u64n-list->bytevector)
  (declare-list->bytevector-conversion u8-list->bytevector)
  #| end of LET-SYNTAX |# )

(declare-core-primitive sint-list->bytevector
    (safe)
  (signatures
   ((<list> <symbol> <positive-fixnum>)	=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)				effect-free result-true)))

(declare-core-primitive uint-list->bytevector
    (safe)
  (signatures
   ((<list> <symbol> <positive-fixnum>)	=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive make-queue-procs
    (safe)
  (signatures
   (()				=> (<procedure> <procedure> <procedure>))
   ((<list>)			=> (<procedure> <procedure> <procedure>)))
  (attributes
   (()				effect-free)
   ((_)				effect-free)))

/section)


;;;; pairs and lists, unsafe functions

(section

(declare-pair-accessor $car unsafe)
(declare-pair-accessor $cdr unsafe)

(declare-pair-mutator $set-car! unsafe)
(declare-pair-mutator $set-cdr! unsafe)

(declare-core-primitive $length
    (unsafe)
  (signatures
   ((<null>)			=> (<zero-fixnum>))
   ((<nelist>)			=> (<positive-fixnum>))
   ((<list>)			=> (<non-negative-fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

/section)


;;;; immutable pairs, safe functions

(declare-type-predicate ipair?	<ipair>)

(declare-core-primitive ipair
    (safe)
  (signatures
   ((<top> <top>)		=> (<ipair>))))

(declare-core-primitive icar
    (safe)
  (signatures
   ((<ipair>)			=> (<top>))))

(declare-core-primitive icdr
    (safe)
  (signatures
   ((<ipair>)			=> (<top>))))


;;;; object utilities

(section

(declare-core-primitive <null>-constructor
    (safe)
  (signatures
   (()				=> (<null>))))

(declare-core-primitive <nelist>-constructor
    (safe)
  (signatures
   ((<top> . <list>)		=> (<nelist>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
