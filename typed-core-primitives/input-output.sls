;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for input/output core primitives
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
(library (typed-core-primitives input-output)
  (export typed-core-primitives.input-output)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.input-output)


;;;; input/output, safe primitives

(section

(declare-parameter current-input-port	<textual-input-port>)
(declare-parameter current-output-port	<textual-output-port>)
(declare-parameter current-error-port	<textual-output-port>)

(declare-object-retriever standard-input-port	<binary-input-port>)
(declare-object-retriever standard-output-port	<binary-output-port>)
(declare-object-retriever standard-error-port	<binary-output-port>)

(declare-parameter console-input-port	<textual-input-port>)
(declare-parameter console-output-port	<textual-output-port>)
(declare-parameter console-error-port	<textual-output-port>)

(declare-parameter print-graph			<boolean>)
(declare-parameter print-unicode		<boolean>)
(declare-parameter printer-integer-radix	<non-negative-fixnum>)
(declare-parameter printer-printing-style	<symbol>)

;;; --------------------------------------------------------------------
;;; predicates

(declare-type-predicate port?				<port>)
(declare-type-predicate binary-port?			<binary-port>)
(declare-type-predicate textual-port?			<textual-port>)

(declare-type-predicate input-port?			<input-port>)
(declare-type-predicate output-port?			<output-port>)
(declare-type-predicate input/output-port?		<input/output-port>)

(declare-type-predicate binary-input-port?		<binary-input-port>)
(declare-type-predicate binary-output-port?		<binary-output-port>)
(declare-type-predicate binary-input/output-port?	<binary-input/output-port>)

(declare-type-predicate textual-input-port?		<textual-input-port>)
(declare-type-predicate textual-output-port?		<textual-output-port>)
(declare-type-predicate textual-input/output-port?	<textual-input/output-port>)

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<top>)		=> (<boolean>)))))
		)))
  (declare open-input-port?)
  (declare open-output-port?)
  (declare open-textual-port?)
  (declare open-binary-port?)
  (declare open-input/output-port?)
  (declare open-binary-input-port?)
  (declare open-textual-input-port?)
  (declare open-binary-output-port?)
  (declare open-textual-output-port?)
  (declare open-binary-input/output-port?)
  (declare open-textual-input/output-port?)
  #| end of LET-SYNTAX |# )

(declare-core-primitive port-eof?
    (safe)
  (signatures
   ((<input-port>)	=> (<boolean>)))
  ;;Not foldable because EOF is an internal, run-time state.  Not effect-free because
  ;;it requires a lookahead, which consumes data from the underlying device.
  (attributes))

(declare-core-primitive port-closed?
    (safe)
  (signatures
   ((<port>)		=> (<boolean>)))
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
		    ((<file-descriptor> <string>)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free result-true))))
		)))
  (declare make-binary-file-descriptor-input-port		<binary-input-port>)
  (declare make-binary-file-descriptor-input-port*		<binary-input-port>)
  (declare make-binary-file-descriptor-output-port		<binary-output-port>)
  (declare make-binary-file-descriptor-output-port*		<binary-output-port>)
  (declare make-binary-file-descriptor-input/output-port	<binary-input/output-port>)
  (declare make-binary-file-descriptor-input/output-port*	<binary-input/output-port>)

  (declare make-binary-socket-input-port			<binary-input-port>)
  (declare make-binary-socket-input-port*			<binary-input-port>)
  (declare make-binary-socket-output-port			<binary-output-port>)
  (declare make-binary-socket-output-port*			<binary-output-port>)
  (declare make-binary-socket-input/output-port			<binary-input/output-port>)
  (declare make-binary-socket-input/output-port*		<binary-input/output-port>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<file-descriptor> <string> <transcoder>)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free result-true))))
		)))
  (declare make-textual-file-descriptor-input-port		<textual-input-port>)
  (declare make-textual-file-descriptor-input-port*		<textual-input-port>)
  (declare make-textual-file-descriptor-output-port		<textual-output-port>)
  (declare make-textual-file-descriptor-output-port*		<textual-output-port>)
  (declare make-textual-file-descriptor-input/output-port	<textual-input/output-port>)
  (declare make-textual-file-descriptor-input/output-port*	<textual-input/output-port>)

  (declare make-textual-socket-input-port			<textual-input-port>)
  (declare make-textual-socket-input-port*			<textual-input-port>)
  (declare make-textual-socket-output-port			<textual-output-port>)
  (declare make-textual-socket-output-port*			<textual-output-port>)
  (declare make-textual-socket-input/output-port		<textual-input/output-port>)
  (declare make-textual-socket-input/output-port*		<textual-input/output-port>)
  #| end of LET-SYNTAX |# )

;;;

(declare-core-primitive make-custom-binary-input-port
    (safe)
  (signatures
   ;;id read! get-position set-position close
   ((<string> <procedure> <false> <false> <false>)		=> (<binary-input-port>))

   ((<string> <procedure> <procedure> <false> <false>)		=> (<binary-input-port>))
   ((<string> <procedure> <false> <procedure> <false>)		=> (<binary-input-port>))
   ((<string> <procedure> <false> <false> <procedure>)		=> (<binary-input-port>))

   ((<string> <procedure> <procedure> <procedure> <false>)	=> (<binary-input-port>))
   ((<string> <procedure> <procedure> <false> <procedure>)	=> (<binary-input-port>))
   ((<string> <procedure> <false> <procedure> <procedure>)	=> (<binary-input-port>))

   ((<string> <procedure> <procedure> <procedure> <procedure>)	=> (<binary-input-port>)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-custom-textual-input-port
    (safe)
  (signatures
   ;;id read! get-position set-position close
   ((<string> <procedure> <false> <false> <false>)		=> (<textual-input-port>))

   ((<string> <procedure> <procedure> <false> <false>)		=> (<textual-input-port>))
   ((<string> <procedure> <false> <procedure> <false>)		=> (<textual-input-port>))
   ((<string> <procedure> <false> <false> <procedure>)		=> (<textual-input-port>))

   ((<string> <procedure> <procedure> <procedure> <false>)	=> (<textual-input-port>))
   ((<string> <procedure> <procedure> <false> <procedure>)	=> (<textual-input-port>))
   ((<string> <procedure> <false> <procedure> <procedure>)	=> (<textual-input-port>))

   ((<string> <procedure> <procedure> <procedure> <procedure>)	=> (<textual-input-port>)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

;;;

(declare-core-primitive make-custom-binary-output-port
    (safe)
  (signatures
   ;;id write! get-position set-position close
   ((<string> <procedure> <false> <false> <false>)		=> (<binary-output-port>))

   ((<string> <procedure> <procedure> <false> <false>)		=> (<binary-output-port>))
   ((<string> <procedure> <false> <procedure> <false>)		=> (<binary-output-port>))
   ((<string> <procedure> <false> <false> <procedure>)		=> (<binary-output-port>))

   ((<string> <procedure> <procedure> <procedure> <false>)	=> (<binary-output-port>))
   ((<string> <procedure> <procedure> <false> <procedure>)	=> (<binary-output-port>))
   ((<string> <procedure> <false> <procedure> <procedure>)	=> (<binary-output-port>))

   ((<string> <procedure> <procedure> <procedure> <procedure>)	=> (<binary-output-port>)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-custom-textual-output-port
    (safe)
  (signatures
   ;;id write! get-position set-position close
   ((<string> <procedure> <false> <false> <false>)		=> (<textual-output-port>))

   ((<string> <procedure> <procedure> <false> <false>)		=> (<textual-output-port>))
   ((<string> <procedure> <false> <procedure> <false>)		=> (<textual-output-port>))
   ((<string> <procedure> <false> <false> <procedure>)		=> (<textual-output-port>))

   ((<string> <procedure> <procedure> <procedure> <false>)	=> (<textual-output-port>))
   ((<string> <procedure> <procedure> <false> <procedure>)	=> (<textual-output-port>))
   ((<string> <procedure> <false> <procedure> <procedure>)	=> (<textual-output-port>))

   ((<string> <procedure> <procedure> <procedure> <procedure>)	=> (<textual-output-port>)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

;;;

(declare-core-primitive make-custom-binary-input/output-port
    (safe)
  (signatures
   ;;id read! write! get-position set-position close
   ((<string> <procedure> <procedure> <false> <false> <false>)			=> (<binary-input/output-port>))

   ((<string> <procedure> <procedure> <procedure> <false> <false>)		=> (<binary-input/output-port>))
   ((<string> <procedure> <procedure> <false> <procedure> <false>)		=> (<binary-input/output-port>))
   ((<string> <procedure> <procedure> <false> <false> <procedure>)		=> (<binary-input/output-port>))

   ((<string> <procedure> <procedure> <procedure> <procedure> <false>)		=> (<binary-input/output-port>))
   ((<string> <procedure> <procedure> <procedure> <false> <procedure>)		=> (<binary-input/output-port>))
   ((<string> <procedure> <procedure> <false> <procedure> <procedure>)		=> (<binary-input/output-port>))

   ((<string> <procedure> <procedure> <procedure> <procedure> <procedure>)	=> (<binary-input/output-port>)))
  (attributes
   ((_ _ _ _ _ _)	effect-free result-true)))

(declare-core-primitive make-custom-textual-input/output-port
    (safe)
  (signatures
   ;;id read! write! get-position set-position close
   ((<string> <procedure> <procedure> <false> <false> <false>)			=> (<textual-input/output-port>))

   ((<string> <procedure> <procedure> <procedure> <false> <false>)		=> (<textual-input/output-port>))
   ((<string> <procedure> <procedure> <false> <procedure> <false>)		=> (<textual-input/output-port>))
   ((<string> <procedure> <procedure> <false> <false> <procedure>)		=> (<textual-input/output-port>))

   ((<string> <procedure> <procedure> <procedure> <procedure> <false>)		=> (<textual-input/output-port>))
   ((<string> <procedure> <procedure> <procedure> <false> <procedure>)		=> (<textual-input/output-port>))
   ((<string> <procedure> <procedure> <false> <procedure> <procedure>)		=> (<textual-input/output-port>))

   ((<string> <procedure> <procedure> <procedure> <procedure> <procedure>)	=> (<textual-input/output-port>)))
  (attributes
   ((_ _ _ _ _ _)	effect-free result-true)))

;;;

(declare-core-primitive open-input-file
    (safe)
  (signatures
   ((<string>)		=> (<textual-input-port>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive open-output-file
    (safe)
  (signatures
   ((<string>)		=> (<textual-output-port>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive open-file-input-port
    (safe)
  (signatures
   ((<string>)					=> (<binary-input-port>))
   ((<string> <enum-set>)			=> (<binary-input-port>))
   ((<string> <enum-set> <symbol>)		=> (<binary-input-port>))
   ((<string> <enum-set> <symbol> <false>)	=> (<binary-input-port>))
   ((<string> <enum-set> <symbol> <transcoder>)	=> (<textual-input-port>)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)
   ((_ _ _)				result-true)
   ((_ _ _ _)				result-true)))

(declare-core-primitive open-file-output-port
    (safe)
  (signatures
   ((<string>)					=> (<binary-output-port>))
   ((<string> <enum-set>)			=> (<binary-output-port>))
   ((<string> <enum-set> <symbol>)		=> (<binary-output-port>))
   ((<string> <enum-set> <symbol> <false>)	=> (<binary-output-port>))
   ((<string> <enum-set> <symbol> <transcoder>)	=> (<textual-output-port>)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)
   ((_ _ _)				result-true)
   ((_ _ _ _)				result-true)))

(declare-core-primitive open-file-input/output-port
    (safe)
  (signatures
   ((<string>)					=> (<binary-input/output-port>))
   ((<string> <enum-set>)			=> (<binary-input/output-port>))
   ((<string> <enum-set> <symbol>)		=> (<binary-input/output-port>))
   ((<string> <enum-set> <symbol> <false>)	=> (<binary-input/output-port>))
   ((<string> <enum-set> <symbol> <transcoder>)	=> (<textual-input/output-port>)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)
   ((_ _ _)				result-true)
   ((_ _ _ _)				result-true)))

;;;

(declare-core-primitive open-string-input-port
    (safe)
  (signatures
   ((<string>)		=> (<textual-input-port>))
   ((<string> <symbol>)	=> (<textual-input-port>)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive open-string-input-port/id
    (safe)
  (signatures
   ((<string> <string>)			=> (<textual-input-port>))
   ((<string> <string> <symbol>)	=> (<textual-input-port>)))
  (attributes
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

(declare-core-primitive open-string-output-port
    (safe)
  (signatures
   (()			=> (<textual-output-port> <procedure>))
   ((<symbol>)		=> (<textual-output-port> <procedure>)))
  (attributes
   (()			effect-free)))

;;;

(declare-core-primitive open-bytevector-input-port
    (safe)
  (signatures
   ((<bytevector>)		=> (<binary-input-port>))
   ((<bytevector> <false>)	=> (<binary-input-port>))
   ((<bytevector> <transcoder>)	=> (<textual-input-port>)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive open-bytevector-output-port
    (safe)
  (signatures
   (()			=> (<binary-output-port> <procedure>))
   ((<false>)		=> (<binary-output-port> <procedure>))
   ((<transcoder>)	=> (<textual-output-port> <procedure>)))
  (attributes
   (()			effect-free)
   ((_)			effect-free)))

;;;

(declare-core-primitive call-with-bytevector-output-port
    (safe)
  (signatures
   ((<procedure>)			=> <list>)
   ((<procedure> <false>)		=> <list>)
   ((<procedure> <transcoder>)		=> <list>)))

(declare-core-primitive call-with-string-output-port
    (safe)
  (signatures
   ((<procedure>)			=> <list>)))

(declare-core-primitive call-with-input-file
    (safe)
  (signatures
   ((<string> <procedure>)		=> <list>)))

(declare-core-primitive call-with-output-file
    (safe)
  (signatures
   ((<string> <procedure>)		=> <list>)))

(declare-core-primitive call-with-port
    (safe)
  (signatures
   ((<port> <procedure>)		=> <list>)))

(declare-core-primitive with-input-from-file
    (safe)
  (signatures
   ((<string> <procedure>)		=> <list>)))

(declare-core-primitive with-output-to-file
    (safe)
  (signatures
   ((<string> <procedure>)		=> <list>)))

(declare-core-primitive with-input-from-string
    (safe)
  (signatures
   ((<string> <procedure>)		=> <list>)))

(declare-core-primitive with-output-to-string
    (safe)
  (signatures
   ((<procedure>)			=> <list>)))

(declare-core-primitive with-output-to-port
    (safe)
  (signatures
   ((<textual-output-port> <procedure>)	=> <list>)))

(declare-core-primitive transcoded-port
    (safe)
  (signatures
   ((<binary-port> <transcoder>)	=> (<textual-port>)))
  (attributes
   ;;NOTE This  is not  effect-free because  the source  binary port  is closed  in a
   ;;special way.
   ((_ _)				result-true)))

;;; --------------------------------------------------------------------
;;; closing

(declare-core-primitive close-port
    (safe)
  (signatures
   ((<port>)		=> (<void>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive close-input-port
    (safe)
  (signatures
   ((<input-port>)	=> (<void>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive close-output-port
    (safe)
  (signatures
   ((<output-port>)	=> (<void>)))
  (attributes
   ((_)			result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive port-id
    (safe)
  (signatures
   ((<port>)		=> (<string>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive port-uid
    (safe)
  (signatures
   ((<port>)		=> (<symbol>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive port-fd
    (safe)
  (signatures
   ((<port>)		=> ((or <false> <file-descriptor>))))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive port-transcoder
    (safe)
  (signatures
   ((<port>)		=> ((or <false> <transcoder>))))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive output-port-buffer-mode
    (safe)
  (signatures
   ((<output-port>)		=> (<symbol>)))
  (attributes
   ((_)				effect-free result-true)))

;;;

(declare-core-primitive port-set-non-blocking-mode!
    (safe)
  (signatures
   ((<port>)		=> (<void>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive port-unset-non-blocking-mode!
    (safe)
  (signatures
   ((<port>)		=> (<void>)))
  (attributes
   ((_)			result-true)))

;;;

(declare-core-primitive port-mode
    (safe)
  (signatures
   ((<port>)		=> (<symbol>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive set-port-mode!
    (safe)
  (signatures
   ((<port> <symbol>)		=> (<void>)))
  (attributes
   ((_ _)			result-true)))

;;;

(declare-core-primitive set-port-buffer-mode!
    (safe)
  (signatures
   ((<port> <symbol>)		=> (<void>)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive port-dump-status
    (safe)
  (signatures
   ((<port>)			=> (<void>)))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive port-position
    (safe)
  (signatures
   ((<port>)			=> (<non-negative-exact-integer>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive port-textual-position
    (safe)
  (signatures
   ((<textual-port>)		=> (<record>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive set-port-position!
    (safe)
  (signatures
   ((<port> <exact-integer>)	=> (<void>)))
  (attributes
   ((_ _)			result-true)))

;;;

(declare-core-primitive port-putprop
    (safe)
  (signatures
   ((<port> <symbol> <top>)		=> (<void>)))
  (attributes
   ((_ _ _)				result-true)))

(declare-core-primitive port-getprop
    (safe)
  (signatures
   ((<port> <symbol>)			=> (<top>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive port-remprop
    (safe)
  (signatures
   ((<port> <symbol>)			=> (<void>)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive port-property-list
    (safe)
  (signatures
   ((<port>)				=> (<list>)))
  (attributes
   ((_)					effect-free result-true)))

;;;

(declare-core-primitive reset-input-port!
    (safe)
  (signatures
   ((<input-port>)	=> (<void>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive reset-output-port!
    (safe)
  (signatures
   ((<input-port>)	=> (<void>)))
  (attributes
   ((_)			result-true)))

;;;

(declare-core-primitive get-output-string
    (safe)
  (signatures
   ((<textual-input-port>)	=> (<string>)))
  (attributes
   ((_)				result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-parameter bytevector-port-buffer-size		<non-negative-fixnum>)
(declare-parameter string-port-buffer-size		<non-negative-fixnum>)
(declare-parameter input-file-buffer-size		<non-negative-fixnum>)
(declare-parameter output-file-buffer-size		<non-negative-fixnum>)
(declare-parameter input/output-file-buffer-size	<non-negative-fixnum>)
(declare-parameter input/output-socket-buffer-size	<non-negative-fixnum>)

;;; --------------------------------------------------------------------
;;; input procedures

(declare-core-primitive get-bytevector-all
    (safe)
  (signatures
   ((<binary-input-port>)		=> ((or <eof> <would-block> <nebytevector>))))
  (attributes
   ((_)					result-true)))

(declare-core-primitive get-bytevector-n
    (safe)
  (signatures
   ((<binary-input-port> <non-negative-fixnum>)		=> ((or <eof> <would-block> <nebytevector>))))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive get-bytevector-n!
    (safe)
  (signatures
   ((<binary-input-port>	;port
     <bytevector>		;bv
     <non-negative-fixnum>	;start
     <non-negative-fixnum>	;count
     ) => ((or <eof> <would-block> <bytevector>))))
  (attributes
   ((_ _ _ _)			result-true)))

(declare-core-primitive get-bytevector-some
    (safe)
  (signatures
   ((<binary-input-port>)	=> ((or <eof> <would-block> <nebytevector>))))
  (attributes
   ((_ _ _ _)			result-true)))

;;;

(declare-core-primitive get-string-all
    (safe)
  (signatures
   ((<textual-input-port>)		=> ((or <eof> <would-block> <nestring>))))
  (attributes
   ((_)					result-true)))

(declare-core-primitive get-string-n
    (safe)
  (signatures
   ((<textual-input-port> <non-negative-fixnum>)	=> ((or <eof> <would-block> <nestring>))))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive get-string-n!
    (safe)
  (signatures
   ((<textual-input-port> <string> <non-negative-fixnum> <non-negative-fixnum>) => ((or <eof> <would-block> <nestring>))))
  (attributes
   ((_ _ _ _)			result-true)))

(declare-core-primitive get-string-some
    (safe)
  (signatures
   ((<textual-input-port>)	=> ((or <eof> <would-block> <nestring>))))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive get-u8
    (safe)
  (signatures
   ((<binary-input-port>)	=> ((or <eof> <would-block> <non-negative-fixnum>))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-char
    (safe)
  (signatures
   ((<textual-input-port>)	=> ((or <eof> <would-block> <char>))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-char-and-track-textual-position
    (safe)
  (signatures
   ((<textual-input-port>)	=> ((or <eof> <would-block> <char>))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-datum
    (safe)
  (signatures
   ((<textual-input-port>)	=> ((or <eof> <would-block> <top>))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-line
    (safe)
  (signatures
   ((<textual-input-port>)	=> ((or <eof> <would-block> <string>))))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive lookahead-u8
    (safe)
  (signatures
   ((<binary-input-port>)	=> ((or <eof> <would-block> <non-negative-fixnum>))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive lookahead-two-u8
    (safe)
  (signatures
   ((<binary-input-port>)	=> ((or <eof> <would-block> <non-negative-fixnum>)
				    (or <eof> <would-block> <non-negative-fixnum>))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive lookahead-char
    (safe)
  (signatures
   ((<textual-input-port>)	=> ((or <eof> <would-block> <char>))))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive read
    (safe)
  (signatures
   (()				=> (<top>))
   ((<textual-input-port>)	=> (<top>))))

(declare-core-primitive read-char
    (safe)
  (signatures
   (()				=> ((or <eof> <would-block> <char>)))
   ((<textual-input-port>)	=> ((or <eof> <would-block> <char>))))
  (attributes
   (()				result-true)
   ((_)				result-true)))

(declare-core-primitive peek-char
    (safe)
  (signatures
   (()				=> ((or <eof> <would-block> <char>)))
   ((<textual-input-port>)	=> ((or <eof> <would-block> <char>))))
  (attributes
   (()				result-true)
   ((_)				result-true)))

(declare-core-primitive read-line
    (safe)
  (signatures
   (()				=> ((or <eof> <would-block> <string>)))
   ((<textual-input-port>)	=> ((or <eof> <would-block> <string>))))
  (attributes
   (()				result-true)
   ((_)				result-true)))

;;; --------------------------------------------------------------------
;;; output procedures

(declare-core-primitive put-bytevector
    (safe)
  (signatures
   ((<binary-output-port> <bytevector>)							=> (<void>))
   ((<binary-output-port> <bytevector> <non-negative-fixnum>)				=> (<void>))
   ((<binary-output-port> <bytevector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<void>)))
  (attributes
   ((_ _)			result-true)
   ((_ _ _)			result-true)
   ((_ _ _ _)			result-true)))

(declare-core-primitive put-string
    (safe)
  (signatures
   ((<textual-output-port> <string>)							=> (<void>))
   ((<textual-output-port> <string> <non-negative-fixnum>)				=> (<void>))
   ((<textual-output-port> <string> <non-negative-fixnum> <non-negative-fixnum>)	=> (<void>)))
  (attributes
   ((_ _)			result-true)
   ((_ _ _)			result-true)
   ((_ _ _ _)			result-true)))

(declare-core-primitive put-u8
    (safe)
  (signatures
   ((<binary-output-port> <non-negative-fixnum>) => (<void>)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive put-char
    (safe)
  (signatures
   ((<textual-output-port> <char>)		=> (<void>)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive put-datum
    (safe)
  (signatures
   ((<textual-output-port> <top>)		=> (<void>)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive write-char
    (safe)
  (signatures
   ((<char>)				=> (<void>))
   ((<char> <textual-output-port>)	=> (<void>)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive write
    (safe)
  (signatures
   ((<top>)				=> (<void>))
   ((<top> <textual-output-port>)	=> (<void>)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive display
    (safe)
  (signatures
   ((<top>)				=> (<void>))
   ((<top> <textual-output-port>)	=> (<void>)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive newline
    (safe)
  (signatures
   (()				=> (<void>))
   (( <textual-output-port>)	=> (<void>)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

;;;

(declare-core-primitive flush-output-port
    (safe)
  (signatures
   ((<output-port>)		=> (<void>)))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive format
    (safe)
  (signatures
   ((<string> . <list>)	=> (<string>)))
  (attributes
   ((_ . _)		result-true)))

(declare-core-primitive printf
    (safe)
  (signatures
   ((<string> . <list>)	=> (<void>)))
  (attributes
   ((_ . _)		result-true)))

(declare-core-primitive fprintf
    (safe)
  (signatures
   ((<textual-output-port> <string> . <list>)	=> (<void>)))
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
   ((<symbol>)		=> (<boolean>)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; pretty printing

(declare-core-primitive pretty-print
    (safe)
  (signatures
   ((<top>)				=> (<void>))
   ((<top> <textual-output-port>)	=> (<void>)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive pretty-print*
    (safe)
  (signatures
   ((<top> <textual-output-port> <non-negative-fixnum> _)	=> (<void>)))
  (attributes
   ((_ _ _ _)		result-true)))

(declare-parameter pretty-width		<non-negative-exact-integer>)

(declare-core-primitive pretty-format
    (safe)
  (signatures
   ((<top>)		=> (<procedure>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive debug-print
    (safe)
  (signatures
   (<list>		=> (<void>)))
  (attributes
   (_			result-true)))

(declare-core-primitive debug-print*
    (safe)
  (signatures
   (<list>		=> (<void>)))
  (attributes
   (_			result-true)))

(declare-parameter debug-print-enabled?)

/section)


;;;; input/output, unsafe primitives

(section

;;; constructors

(declare-core-primitive $make-port
    (unsafe)
  (signatures
   ;; ;;attrs                 idx                   buffer-size           buffer
   ;; ((<non-negative-fixnum> <non-negative-fixnum> <non-negative-fixnum> <top>
   ;; 			   ;;transcoder
   ;; 			   (or <transcoder> <boolean>)
   ;; 			   ;;id     read     write    getp     setp     close    cookie
   ;; 			   <top> <top> <top> <top> <top> <top> <top>)	=> (<port>))
   ;;attrs                 idx                   buffer-size           buffer
   ((<non-negative-fixnum> <non-negative-fixnum> <non-negative-fixnum> <top>
			   ;;transcoder
			   <top>
			   ;;id     read     write    getp     setp     close    cookie
			   <top> <top> <top> <top> <top> <top> <top>)	=> (<port>)))
  (attributes
   ;;FIXME Should this be foldable?  (Marco Maggi; Wed Nov 26, 2014)
   ((_ _ _  _ _ _  _ _ _  _ _ _)		effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(letrec-syntax
    ((declare-unsafe-port-accessor
      (syntax-rules ()
	((_ ?who)
	 (declare-unsafe-port-accessor ?who <top>))
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((<port>)			=> (<top>)))
	   (attributes
	    ((_)			effect-free))))
	)))
  (declare-unsafe-port-accessor $port-tag		<fixnum>)
  (declare-unsafe-port-accessor $port-attrs		<non-negative-fixnum>)
  (declare-unsafe-port-accessor $port-index		<non-negative-fixnum>)
  (declare-unsafe-port-accessor $port-size		<non-negative-fixnum>)
  (declare-unsafe-port-accessor $port-buffer)
  (declare-unsafe-port-accessor $port-transcoder	<transcoder>)
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
	 (declare-unsafe-port-mutator ?who <top>))
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((<port> ?new-value-tag)	=> (<void>)))
	   (attributes
	    ((_ _)			result-true))))
	)))
  (declare-unsafe-port-mutator $set-port-index!		<non-negative-fixnum>)
  (declare-unsafe-port-mutator $set-port-size!		<non-negative-fixnum>)
  (declare-unsafe-port-mutator $set-port-attrs!		<non-negative-fixnum>)
  #| end of LET-SYNTAX |# )

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
