;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: compile-time property definitions for core primitives
;;;Date: Mon Sep 22, 2014
;;;
;;;Abstract
;;;
;;;	The purpose of this module is to  associate values to the public name of core
;;;	primitive.  The values represent core  primitive properties: the arity of the
;;;	primitive; the  number of  returned values;  the core  types of  the expected
;;;	arguments; the  core types of  the returned values;  miscellaneous properties
;;;	used by the source optimiser.
;;;
;;;	  Scheme  object's core  types  are  defined by  the  module "Scheme  objects
;;;	ontology".  This file contains a table  of core primitive properties for both
;;;	primitive functions and primitive operations.
;;;
;;;Copyright (C) 2014, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (ikarus.compiler.core-primitive-properties.input-output)
  (export initialise-core-primitive-properties/input-output)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/input-output)


;;;; input/output, safe primitives

(declare-parameter current-input-port	T:textual-input-port)
(declare-parameter current-output-port	T:textual-output-port)
(declare-parameter current-error-port	T:textual-output-port)

(declare-object-retriever standard-input-port	T:binary-input-port)
(declare-object-retriever standard-output-port	T:binary-output-port)
(declare-object-retriever standard-error-port	T:binary-output-port)

(declare-parameter console-input-port	T:textual-input-port)
(declare-parameter console-output-port	T:textual-output-port)
(declare-parameter console-error-port	T:textual-output-port)

(declare-parameter print-graph			T:boolean)
(declare-parameter print-unicode		T:boolean)
(declare-parameter printer-integer-radix	T:non-negative-fixnum)

;;; --------------------------------------------------------------------
;;; predicates

(declare-type-predicate port?				T:port)
(declare-type-predicate binary-port?			T:binary-port)
(declare-type-predicate textual-port?			T:textual-port)

(declare-type-predicate input-port?			T:input-port)
(declare-type-predicate output-port?			T:output-port)
(declare-type-predicate input/output-port?		T:input/output-port)

(declare-type-predicate binary-input-port?		T:binary-input-port)
(declare-type-predicate binary-output-port?		T:binary-output-port)
(declare-type-predicate binary-input/output-port?	T:binary-input/output-port)

(declare-type-predicate textual-input-port?		T:textual-input-port)
(declare-type-predicate textual-output-port?		T:textual-output-port)
(declare-type-predicate textual-input/output-port?	T:textual-input/output-port)

(declare-core-primitive port-eof?
    (safe)
  (signatures
   ((T:input-port)	=> (T:boolean)))
  ;;Not foldable because EOF is an internal, run-time state.  Not effect-free because
  ;;it requires a lookahead, which consumes data from the underlying device.
  (attributes))

(declare-core-primitive port-closed?
    (safe)
  (signatures
   ((T:port)		=> (T:boolean)))
  (attributes
   ;;Not foldable because "port closed" is an internal, run-time state.
   ((_)			effect-free)))

(declare-port-predicate port-has-port-position?)
(declare-port-predicate port-has-set-port-position!?)
(declare-port-predicate port-in-non-blocking-mode?)

;;; --------------------------------------------------------------------
;;; constructors

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:file-descriptor T:string)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free result-true))))
		)))
  (declare make-binary-file-descriptor-input-port		T:binary-input-port)
  (declare make-binary-file-descriptor-input-port*		T:binary-input-port)
  (declare make-binary-file-descriptor-output-port		T:binary-output-port)
  (declare make-binary-file-descriptor-output-port*		T:binary-output-port)
  (declare make-binary-file-descriptor-input/output-port	T:binary-input/output-port)
  (declare make-binary-file-descriptor-input/output-port*	T:binary-input/output-port)

  (declare make-binary-socket-input-port			T:binary-input-port)
  (declare make-binary-socket-input-port*			T:binary-input-port)
  (declare make-binary-socket-output-port			T:binary-output-port)
  (declare make-binary-socket-output-port*			T:binary-output-port)
  (declare make-binary-socket-input/output-port			T:binary-input/output-port)
  (declare make-binary-socket-input/output-port*		T:binary-input/output-port)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:file-descriptor T:string T:transcoder)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free result-true))))
		)))
  (declare make-textual-file-descriptor-input-port		T:textual-input-port)
  (declare make-textual-file-descriptor-input-port*		T:textual-input-port)
  (declare make-textual-file-descriptor-output-port		T:textual-output-port)
  (declare make-textual-file-descriptor-output-port*		T:textual-output-port)
  (declare make-textual-file-descriptor-input/output-port	T:textual-input/output-port)
  (declare make-textual-file-descriptor-input/output-port*	T:textual-input/output-port)

  (declare make-textual-socket-input-port			T:textual-input-port)
  (declare make-textual-socket-input-port*			T:textual-input-port)
  (declare make-textual-socket-output-port			T:textual-output-port)
  (declare make-textual-socket-output-port*			T:textual-output-port)
  (declare make-textual-socket-input/output-port		T:textual-input/output-port)
  (declare make-textual-socket-input/output-port*		T:textual-input/output-port)
  #| end of LET-SYNTAX |# )

;;;

(declare-core-primitive make-custom-binary-input-port
    (safe)
  (signatures
   ;;id read! get-position set-position close
   ((T:string T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))	=> (T:binary-input-port)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-custom-textual-input-port
    (safe)
  (signatures
   ;;id read! get-position set-position close
   ((T:string T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))	=> (T:textual-input-port)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

;;;

(declare-core-primitive make-custom-binary-output-port
    (safe)
  (signatures
   ;;id write! get-position set-position close
   ((T:string T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))	=> (T:binary-output-port)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-custom-textual-output-port
    (safe)
  (signatures
   ;;id write! get-position set-position close
   ((T:string T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))	=> (T:textual-output-port)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

;;;

(declare-core-primitive make-custom-binary-input/output-port
    (safe)
  (signatures
   ;;id read! write! get-position set-position close
   ((T:string T:procedure T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))
    => (T:binary-input/output-port)))
  (attributes
   ((_ _ _ _ _ _)	effect-free result-true)))

(declare-core-primitive make-custom-textual-input/output-port
    (safe)
  (signatures
   ;;id read! write! get-position set-position close
   ((T:string T:procedure T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))
    => (T:textual-input/output-port)))
  (attributes
   ((_ _ _ _ _ _)	effect-free result-true)))

;;;

(declare-core-primitive open-input-file
    (safe)
  (signatures
   ((T:string)		=> (T:textual-input-port)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive open-output-file
    (safe)
  (signatures
   ((T:string)		=> (T:textual-output-port)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive open-file-input-port
    (safe)
  (signatures
   ((T:string)					=> (T:binary-input-port))
   ((T:string T:enum-set)			=> (T:binary-input-port))
   ((T:string T:enum-set T:symbol)		=> (T:binary-input-port))
   ((T:string T:enum-set T:symbol T:false)	=> (T:binary-input-port))
   ((T:string T:enum-set T:symbol T:transcoder)	=> (T:textual-input-port)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)
   ((_ _ _)				result-true)
   ((_ _ _ _)				result-true)))

(declare-core-primitive open-file-output-port
    (safe)
  (signatures
   ((T:string)					=> (T:binary-output-port))
   ((T:string T:enum-set)			=> (T:binary-output-port))
   ((T:string T:enum-set T:symbol)		=> (T:binary-output-port))
   ((T:string T:enum-set T:symbol T:false)	=> (T:binary-output-port))
   ((T:string T:enum-set T:symbol T:transcoder)	=> (T:textual-output-port)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)
   ((_ _ _)				result-true)
   ((_ _ _ _)				result-true)))

(declare-core-primitive open-file-input/output-port
    (safe)
  (signatures
   ((T:string)					=> (T:binary-input/output-port))
   ((T:string T:enum-set)			=> (T:binary-input/output-port))
   ((T:string T:enum-set T:symbol)		=> (T:binary-input/output-port))
   ((T:string T:enum-set T:symbol T:false)	=> (T:binary-input/output-port))
   ((T:string T:enum-set T:symbol T:transcoder)	=> (T:textual-input/output-port)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)
   ((_ _ _)				result-true)
   ((_ _ _ _)				result-true)))

;;;

(declare-core-primitive open-string-input-port
    (safe)
  (signatures
   ((T:string)		=> (T:textual-input-port))
   ((T:string T:symbol)	=> (T:textual-input-port)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive open-string-input-port/id
    (safe)
  (signatures
   ((T:string T:string)			=> (T:textual-input-port))
   ((T:string T:string T:symbol)	=> (T:textual-input-port)))
  (attributes
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

(declare-core-primitive open-string-output-port
    (safe)
  (signatures
   (()			=> (T:textual-output-port T:procedure))
   ((T:symbol)		=> (T:textual-output-port T:procedure)))
  (attributes
   (()			effect-free)))

;;;

(declare-core-primitive open-bytevector-input-port
    (safe)
  (signatures
   ((T:bytevector)		=> (T:binary-input-port))
   ((T:bytevector T:false)	=> (T:binary-input-port))
   ((T:bytevector T:transcoder)	=> (T:textual-input-port)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive open-bytevector-output-port
    (safe)
  (signatures
   (()			=> (T:binary-output-port T:procedure))
   ((T:false)		=> (T:binary-output-port T:procedure))
   ((T:transcoder)	=> (T:textual-output-port T:procedure)))
  (attributes
   (()			effect-free)
   ((_)			effect-free)))

;;;

(declare-core-primitive call-with-bytevector-output-port
    (safe)
  (signatures
   ((T:procedure)			=> T:object)
   ((T:procedure T:false)		=> T:object)
   ((T:procedure T:transcoder)		=> T:object)))

(declare-core-primitive call-with-string-output-port
    (safe)
  (signatures
   ((T:procedure)			=> T:object)))

(declare-core-primitive call-with-input-file
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive call-with-output-file
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive call-with-port
    (safe)
  (signatures
   ((T:port T:procedure)		=> T:object)))

(declare-core-primitive with-input-from-file
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive with-output-to-file
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive with-input-from-string
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive with-output-to-string
    (safe)
  (signatures
   ((T:procedure)			=> T:object)))

(declare-core-primitive transcoded-port
    (safe)
  (signatures
   ((T:binary-port T:transcoder)	=> (T:textual-port)))
  (attributes
   ;;NOTE This  is not  effect-free because  the source  binary port  is closed  in a
   ;;special way.
   ((_ _)				result-true)))

;;; --------------------------------------------------------------------
;;; closing

(declare-core-primitive close-port
    (safe)
  (signatures
   ((T:port)		=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive close-input-port
    (safe)
  (signatures
   ((T:input-port)	=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive close-output-port
    (safe)
  (signatures
   ((T:output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive port-id
    (safe)
  (signatures
   ((T:port)		=> (T:string)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive port-uid
    (safe)
  (signatures
   ((T:port)		=> (T:symbol)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive port-fd
    (safe)
  (signatures
   ((T:port)		=> ((or T:false T:file-descriptor))))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive port-transcoder
    (safe)
  (signatures
   ((T:port)		=> ((or T:false T:transcoder))))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive output-port-buffer-mode
    (safe)
  (signatures
   ((T:output-port)		=> (T:symbol)))
  (attributes
   ((_)				effect-free result-true)))

;;;

(declare-core-primitive port-set-non-blocking-mode!
    (safe)
  (signatures
   ((T:port)		=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive port-unset-non-blocking-mode!
    (safe)
  (signatures
   ((T:port)		=> (T:void)))
  (attributes
   ((_)			result-true)))

;;;

(declare-core-primitive port-mode
    (safe)
  (signatures
   ((T:port)		=> (T:symbol)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive set-port-mode!
    (safe)
  (signatures
   ((T:port T:symbol)		=> (T:void)))
  (attributes
   ((_ _)			result-true)))

;;;

(declare-core-primitive set-port-buffer-mode!
    (safe)
  (signatures
   ((T:port T:symbol)		=> (T:void)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive port-dump-status
    (safe)
  (signatures
   ((T:port)			=> (T:void)))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive port-position
    (safe)
  (signatures
   ((T:port)			=> (T:non-negative-exact-integer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive port-textual-position
    (safe)
  (signatures
   ((T:textual-port)		=> (T:record)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive set-port-position!
    (safe)
  (signatures
   ((T:port T:exact-integer)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))

;;;

(declare-core-primitive port-putprop
    (safe)
  (signatures
   ((T:port T:symbol T:object)		=> (T:void)))
  (attributes
   ((_ _ _)				result-true)))

(declare-core-primitive port-getprop
    (safe)
  (signatures
   ((T:port T:symbol)			=> (T:object)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive port-remprop
    (safe)
  (signatures
   ((T:port T:symbol)			=> (T:void)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive port-property-list
    (safe)
  (signatures
   ((T:port)		=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

;;;

(declare-core-primitive reset-input-port!
    (safe)
  (signatures
   ((T:input-port)	=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive reset-output-port!
    (safe)
  (signatures
   ((T:input-port)	=> (T:void)))
  (attributes
   ((_)			result-true)))

;;;

(declare-core-primitive get-output-string
    (safe)
  (signatures
   ((T:textual-input-port)	=> (T:string)))
  (attributes
   ((_)				result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-parameter bytevector-port-buffer-size		T:non-negative-fixnum)
(declare-parameter string-port-buffer-size		T:non-negative-fixnum)
(declare-parameter input-file-buffer-size		T:non-negative-fixnum)
(declare-parameter output-file-buffer-size		T:non-negative-fixnum)
(declare-parameter input/output-file-buffer-size	T:non-negative-fixnum)
(declare-parameter input/output-socket-buffer-size	T:non-negative-fixnum)

;;; --------------------------------------------------------------------
;;; input procedures

(declare-core-primitive get-bytevector-all
    (safe)
  (signatures
   ((T:binary-input-port)		=> ((or T:eof T:would-block T:bytevector))))
  (attributes
   ((_)					result-true)))

(declare-core-primitive get-bytevector-n
    (safe)
  (signatures
   ((T:binary-input-port T:non-negative-fixnum)		=> ((or T:eof T:would-block T:bytevector))))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive get-bytevector-n!
    (safe)
  (signatures
   ((T:binary-input-port T:bytevector T:non-negative-fixnum T:non-negative-fixnum)
    => ((or T:eof T:would-block T:bytevector))))
  (attributes
   ((_ _ _ _)			result-true)))

(declare-core-primitive get-bytevector-some
    (safe)
  (signatures
   ((T:binary-input-port)	=> ((or T:eof T:would-block T:bytevector))))
  (attributes
   ((_ _ _ _)			result-true)))

;;;

(declare-core-primitive get-string-all
    (safe)
  (signatures
   ((T:textual-input-port)		=> ((or T:eof T:would-block T:string))))
  (attributes
   ((_)					result-true)))

(declare-core-primitive get-string-n
    (safe)
  (signatures
   ((T:textual-input-port T:non-negative-fixnum)	=> ((or T:eof T:would-block T:string))))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive get-string-n!
    (safe)
  (signatures
   ((T:textual-input-port T:string T:non-negative-fixnum T:non-negative-fixnum)
    => ((or T:eof T:would-block T:string))))
  (attributes
   ((_ _ _ _)			result-true)))

(declare-core-primitive get-string-some
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:string))))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive get-u8
    (safe)
  (signatures
   ((T:binary-input-port)	=> ((or T:eof T:would-block T:octet))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-char
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-char-and-track-textual-position
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-datum
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:object))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-line
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:string))))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive lookahead-u8
    (safe)
  (signatures
   ((T:binary-input-port)	=> ((or T:eof T:would-block T:octet))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive lookahead-two-u8
    (safe)
  (signatures
   ((T:binary-input-port)	=> ((or T:eof T:would-block T:octet)
				    (or T:eof T:would-block T:octet))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive lookahead-char
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive read
    (safe)
  (signatures
   (()				=> (T:object))
   ((T:textual-input-port)	=> (T:object))))

(declare-core-primitive read-char
    (safe)
  (signatures
   (()				=> ((or T:eof T:would-block T:char)))
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   (()				result-true)
   ((_)				result-true)))

(declare-core-primitive peek-char
    (safe)
  (signatures
   (()				=> ((or T:eof T:would-block T:char)))
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   (()				result-true)
   ((_)				result-true)))

(declare-core-primitive read-line
    (safe)
  (signatures
   (()				=> ((or T:eof T:would-block T:string)))
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:string))))
  (attributes
   (()				result-true)
   ((_)				result-true)))

;;; --------------------------------------------------------------------
;;; output procedures

(declare-core-primitive put-bytevector
    (safe)
  (signatures
   ((T:binary-output-port T:bytevector)							=> (T:void))
   ((T:binary-output-port T:bytevector T:non-negative-fixnum)				=> (T:void))
   ((T:binary-output-port T:bytevector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:void)))
  (attributes
   ((_ _)			result-true)
   ((_ _ _)			result-true)
   ((_ _ _ _)			result-true)))

(declare-core-primitive put-string
    (safe)
  (signatures
   ((T:textual-output-port T:string)							=> (T:void))
   ((T:textual-output-port T:string T:non-negative-fixnum)				=> (T:void))
   ((T:textual-output-port T:string T:non-negative-fixnum T:non-negative-fixnum)	=> (T:void)))
  (attributes
   ((_ _)			result-true)
   ((_ _ _)			result-true)
   ((_ _ _ _)			result-true)))

(declare-core-primitive put-u8
    (safe)
  (signatures
   ((T:binary-output-port T:octet)		=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive put-char
    (safe)
  (signatures
   ((T:textual-output-port T:char)		=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive put-datum
    (safe)
  (signatures
   ((T:textual-output-port T:object)		=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive write-char
    (safe)
  (signatures
   ((T:char)				=> (T:void))
   ((T:char T:textual-output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive write
    (safe)
  (signatures
   ((T:object)				=> (T:void))
   ((T:object T:textual-output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive display
    (safe)
  (signatures
   ((T:object)				=> (T:void))
   ((T:object T:textual-output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive newline
    (safe)
  (signatures
   (()				=> (T:void))
   (( T:textual-output-port)	=> (T:void)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

;;;

(declare-core-primitive flush-output-port
    (safe)
  (signatures
   ((T:output-port)		=> (T:void)))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive format
    (safe)
  (signatures
   ((T:string . T:object)	=> (T:string)))
  (attributes
   ((_ . _)		result-true)))

(declare-core-primitive printf
    (safe)
  (signatures
   ((T:string . T:object)	=> (T:void)))
  (attributes
   ((_ . _)		result-true)))

(declare-core-primitive fprintf
    (safe)
  (signatures
   ((T:textual-output-port T:string . T:object)	=> (T:void)))
  (attributes
   ((_ _ . _)		result-true)))

;;; --------------------------------------------------------------------
;;; special values

(declare-object-retriever eof-object)
(declare-object-predicate eof-object?)

(declare-object-retriever would-block-object)
(declare-object-predicate would-block-object?)

(declare-core-primitive buffer-mode?
    (safe)
  (signatures
   ((T:symbol)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; pretty printing

(declare-core-primitive pretty-print
    (safe)
  (signatures
   ((T:object)				=> (T:void))
   ((T:object T:textual-output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive pretty-print*
    (safe)
  (signatures
   ((T:object T:textual-output-port T:non-negative-fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _ _)		result-true)))

(declare-parameter pretty-width		T:non-negative-exact-integer)

(declare-core-primitive pretty-format
    (safe)
  (signatures
   ((T:object)		=> (T:procedure)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive debug-print
    (safe)
  (signatures
   (T:object		=> (T:void)))
  (attributes
   (_			result-true)))

(declare-core-primitive debug-print*
    (safe)
  (signatures
   (T:object		=> (T:void)))
  (attributes
   (_			result-true)))

(declare-parameter debug-print-enabled?)


;;;; input/output, unsafe primitives

;;; constructors

(declare-core-primitive $make-port
    (unsafe)
  (signatures
   ;;attrs                 idx                   buffer-size           buffer
   ((T:non-negative-fixnum T:non-negative-fixnum T:non-negative-fixnum T:object
			   ;;transcoder
			   (or T:transcoder T:boolean)
			   ;;id     read     write    getp     setp     close    cookie
			   T:object T:object T:object T:object T:object T:object T:object)	=> (T:port)))
  (attributes
   ;;FIXME Should this be foldable?  (Marco Maggi; Wed Nov 26, 2014)
   ((_ _ _  _ _ _  _ _ _  _ _ _)		effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(letrec-syntax
    ((declare-unsafe-port-accessor
      (syntax-rules ()
	((_ ?who)
	 (declare-unsafe-port-accessor ?who T:object))
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((T:port)			=> (T:object)))
	   (attributes
	    ((_)			effect-free))))
	)))
  (declare-unsafe-port-accessor $port-tag		T:fixnum)
  (declare-unsafe-port-accessor $port-attrs		T:non-negative-fixnum)
  (declare-unsafe-port-accessor $port-index		T:non-negative-fixnum)
  (declare-unsafe-port-accessor $port-size		T:non-negative-fixnum)
  (declare-unsafe-port-accessor $port-buffer)
  (declare-unsafe-port-accessor $port-transcoder	T:transcoder)
  (declare-unsafe-port-accessor $port-cookie)
  (declare-unsafe-port-accessor $port-get-position)
  (declare-unsafe-port-accessor $port-close)
  (declare-unsafe-port-accessor $port-id)
  (declare-unsafe-port-accessor $port-read!)
  (declare-unsafe-port-accessor $port-write!)
  (declare-unsafe-port-accessor $port-set-position!)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-unsafe-port-mutator
      (syntax-rules ()
	((_ ?who)
	 (declare-unsafe-port-mutator ?who T:object))
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((T:port ?new-value-tag)	=> (T:void)))
	   (attributes
	    ((_ _)			result-true))))
	)))
  (declare-unsafe-port-mutator $set-port-index!		T:non-negative-fixnum)
  (declare-unsafe-port-mutator $set-port-size!		T:non-negative-fixnum)
  (declare-unsafe-port-mutator $set-port-attrs!		T:non-negative-fixnum)
  #| end of LET-SYNTAX |# )


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
