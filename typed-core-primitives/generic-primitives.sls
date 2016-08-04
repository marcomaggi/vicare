;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for generic core primitives
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
(library (typed-core-primitives generic-primitives)
  (export typed-core-primitives.generic-primitives)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.generic-primitives)


;;;; generic primitives

(section

(declare-core-primitive immediate?
    (safe)
  (signatures
   ((<fixnum>)		=> (<true>))
   ((<char>)		=> (<true>))
   ((<null>)		=> (<true>))
   ((<boolean>)		=> (<true>))
   ((<eof>)		=> (<true>))
   ;;Notice that void is forbidden as types in arguments signatures.
   ;;((<void>)		=> (<true>))
   ((<transcoder>)	=> (<true>))

   ((<bignum>)		=> (<false>))
   ((<flonum>)		=> (<false>))
   ((<ratnum>)		=> (<false>))
   ((<compnum>)		=> (<false>))
   ((<cflonum>)		=> (<false>))
   ((<pair>)		=> (<false>))
   ((<string>)		=> (<false>))
   ((<vector>)		=> (<false>))
   ((<bytevector>)	=> (<false>))
   ((<struct>)		=> (<false>))
   ((<port>)		=> (<false>))
   ((<symbol>)		=> (<false>))
   ((<keyword>)		=> (<false>))
   ((<hashtable>)	=> (<false>))
   ((<would-block>)	=> (<false>))

   ((<top>)		=> (<boolean>)))
  (attributes
   ((_)			foldable effect-free)))

(declare-type-predicate code?		<code>)
(declare-type-predicate procedure?	<procedure>)

(declare-core-primitive procedure-annotation
    (safe)
  (signatures
   ((<procedure>)	=> (<top>)))
  (attributes
   ((_)			effect-free)))

(declare-object-binary-comparison eq?)
(declare-object-binary-comparison neq?)
(declare-object-binary-comparison eqv?)
(declare-object-binary-comparison equal?)

(declare-core-primitive not
    (safe)
  (signatures
   ((<false>)				=> (<true>))
   ((<true>)				=> (<false>))
   ((<void>)				=> (<void>))
   ;;Here <true> is repeated on purpose.
   (((or <true> (not <boolean>)))	=> (<false>))
   ((<top>)				=> (<boolean>))))

;;; --------------------------------------------------------------------

(declare-core-primitive vicare-argv0
    (safe)
  (signatures
   (()			=> (<bytevector>)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive vicare-argv0-string
    (safe)
  (signatures
   (()			=> (<string>)))
  (attributes
   (()			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive void
    (safe)
  (signatures
   (()				=> (<void>)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive void-object?
    (safe)
  (signatures
   ((<void>)			=> (<true>))
   ((<top>)			=> (<boolean>))))

(declare-core-primitive unspecified-values
    (safe)
  (signatures
   (()				=> (list-of <void>)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive load
    (safe)
  (signatures
   ((<string>)			=> <list>)
   ((<string> <procedure>)	=> <list>)))

(declare-core-primitive make-traced-procedure
    (safe)
  (signatures
   ((<symbol> <procedure>)	=> (<procedure>)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive make-traced-macro
    (safe)
  (signatures
   ((<symbol> <procedure>)	=> (<procedure>)))
  (attributes
   ((_ _)		effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive  random
    (safe)
  (signatures
   ((<fixnum>)		=> (<fixnum>)))
  (attributes
   ;;Not foldable  because the random number  must be generated at  run-time, one for
   ;;each invocation.
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-object-retriever uuid			<string>)

(declare-object-retriever bwp-object)
(declare-object-predicate bwp-object?)

(declare-object-retriever unbound-object)
(declare-object-predicate unbound-object?)

(declare-object-predicate $unbound-object?	unsafe)

;;; --------------------------------------------------------------------

(declare-parameter interrupt-handler	<procedure>)
(declare-parameter engine-handler	<procedure>)

;;; --------------------------------------------------------------------

(declare-core-primitive always-true
    (safe)
  (signatures
   (_				=> (<true>)))
  (attributes
   (_				foldable effect-free result-true)))

(declare-core-primitive always-false
    (safe)
  (signatures
   (_				=> (<false>)))
  (attributes
   (_				foldable effect-free result-false)))

;;; --------------------------------------------------------------------

(declare-core-primitive new-cafe
    (safe)
  (signatures
   (()			=> (<void>))
   ((<procedure>)	=> (<void>)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

(declare-parameter waiter-prompt-string		<string>)
(declare-parameter cafe-input-port		<textual-input-port>)

(declare-core-primitive apropos
    (safe)
  (signatures
   ((<string>)		=> (<void>)))
  (attributes
   ((_)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive readline-enabled?
    (safe)
  (signatures
   (()			=> (<boolean>)))
  (attributes
   (()			effect-free)))

(declare-core-primitive readline
    (safe)
  (signatures
   (()						=> (<string>))
   ((<false>)					=> (<string>))
   ((<bytevector>)				=> (<string>))
   ((<string>)					=> (<string>)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

(declare-core-primitive make-readline-input-port
    (safe)
  (signatures
   (()					=> (<textual-input-port>))
   ((<false>)				=> (<textual-input-port>))
   ((<procedure>)			=> (<textual-input-port>)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive fasl-write
    (safe)
  (signatures
   ((<top> <binary-output-port>)			=> (<void>))
   ((<top> <binary-output-port> <list>)	=> (<void>)))
  (attributes
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive fasl-read
    (safe)
  (signatures
   ((<binary-input-port>)	=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-primitive sentinel
    (safe)
  (signatures
   (()				=> (<sentinel>))))

(declare-type-predicate sentinel?	<sentinel>)

/section)


;;;; foldable core primitive variants

(section

(declare-core-primitive foldable-cons
    (safe)
  (signatures
   ((_ _)		=> (<pair>)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive foldable-list
    (safe)
  (signatures
   (()			=> (<null>))
   ((_ . _)		=> (<list>)))
  (attributes
   (()			foldable effect-free result-true)
   ((_ . _)		foldable effect-free result-true)))

(declare-core-primitive foldable-string
    (safe)
  (signatures
   (()			=> (<string>))
   ((list-of <char>)	=> (<string>)))
  (attributes
   (()			foldable effect-free result-true)
   (_			foldable effect-free result-true)))

(declare-core-primitive foldable-vector
    (safe)
  (signatures
   (()				=> (<vector>))
   (_				=> (<vector>)))
  (attributes
   (()				foldable effect-free result-true)
   (_				foldable effect-free result-true)))

(declare-core-primitive foldable-list->vector
    (safe)
  (signatures
   ((<list>)			=> (<vector>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive foldable-append
    (safe)
  (signatures
   (()				=> (<null>))
   ((<top> . <list>)		=> (<pair>)))
  (attributes
   (()				foldable effect-free result-true)
   ((_ . _)			foldable effect-free result-true)))

/section)


;;;; debugging helpers

(section

(declare-exact-integer-unary integer->machine-word)

(declare-core-primitive machine-word->integer
  (safe)
  (signatures
   ((<top>)			=> (<exact-integer>)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive flonum->bytevector
    (safe)
  (signatures
   ((<flonum>)		=> (<bytevector>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bytevector->flonum
    (safe)
  (signatures
   ((<bytevector>)	=> (<flonum>)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive bignum->bytevector
    (safe)
  (signatures
   ((<bignum>)		=> (<bytevector>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bytevector->bignum
    (safe)
  (signatures
   ((<bytevector>)	=> (<bignum>)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive time-it
    (safe)
  (signatures
   ((<string> <procedure>)	=> <list>)))

(declare-core-primitive time-and-gather
    (safe)
  (signatures
   ((<procedure> <procedure>)	=> <list>)))

(declare-parameter verbose-timer)

;;;

(declare-type-predicate stats?		<stats>)

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare ?who <top>))
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<stats>)		=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare stats-collection-id)
  (declare stats-user-secs	<exact-integer>)
  (declare stats-user-usecs	<exact-integer>)
  (declare stats-sys-secs	<exact-integer>)
  (declare stats-sys-usecs	<exact-integer>)
  (declare stats-real-secs	<exact-integer>)
  (declare stats-real-usecs	<exact-integer>)
  (declare stats-gc-user-secs	<exact-integer>)
  (declare stats-gc-user-usecs	<exact-integer>)
  (declare stats-gc-sys-secs	<exact-integer>)
  (declare stats-gc-sys-usecs	<exact-integer>)
  (declare stats-gc-real-secs	<exact-integer>)
  (declare stats-gc-real-usecs	<exact-integer>)
  (declare stats-bytes-minor	<exact-integer>)
  (declare stats-bytes-major	<exact-integer>)
  #| end of LET-SYNTAX |# )

/section)


;;;; coroutines

(declare-core-primitive coroutine
    (safe)
  (signatures
   ((<procedure>)		=> <list>)))

(declare-core-primitive yield
    (safe)
  (signatures
   (()				=> <list>)))

(declare-core-primitive finish-coroutines
    (safe)
  (signatures
   (()				=> <list>)
   ((<procedure>)		=> <list>)))

;;; --------------------------------------------------------------------

(declare-core-primitive current-coroutine-uid
    (safe)
  (signatures
   (()				=> (<gensym>))))

;;; --------------------------------------------------------------------

(declare-core-primitive reset-coroutines!
    (safe)
  (signatures
   (()				=> <list>)))

(declare-core-primitive resume-coroutine
    (safe)
  (signatures
   ((<gensym>)			=> <list>)))

(declare-core-primitive suspend-coroutine
    (safe)
  (signatures
   (()				=> <list>)))

(declare-core-primitive suspended-coroutine?
    (safe)
  (signatures
   ((<gensym>)			=> (<boolean>))))

;;; --------------------------------------------------------------------

(declare-core-primitive do-monitor
    (safe)
  (signatures
   ((<gensym> <positive-fixnum> <procedure>)	=> <list>)))


;;;; printing messages

(section

(declare-core-primitive print-stderr-message
    (safe)
  (signatures
   ((<string> <string> <list>)	=> (<void>))))

(declare-core-primitive print-error-message
    (safe)
  (signatures
   ((<string> . <list>)		=> (<void>))))

(declare-core-primitive print-verbose-message
    (safe)
  (signatures
   ((<string> . <list>)		=> (<void>))))

(declare-core-primitive print-debug-message
    (safe)
  (signatures
   ((<string> . <list>)		=> (<void>))))

/section)


;;;; core syntactic binding descriptors, typed safe OOP core primitives: generic objects

(section

(declare-core-primitive <top>-constructor
    (safe)
  (signatures
   ((<top>)		=> (<top>))))

(declare-core-primitive <top>-type-predicate
    (safe)
  (signatures
   ((<top>)		=> (<true>))))

(declare-core-primitive <untyped>-constructor
    (safe)
  (signatures
   (<list>		=> <bottom>)))

(declare-core-primitive <untyped>-type-predicate
    (safe)
  (signatures
   (<list>		=> <bottom>)))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
