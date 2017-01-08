;;; -*- coding: utf-8-unix -*-
;;;
;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;
;;;Abstract
;;;
;;;	Define  and  export  all  the  I/O  functions  mandated  by  R6RS  plus  some
;;;	implementation-specific functions.  See also the file "ikarus.codecs.sls" for
;;;	transcoders and codecs.
;;;
;;;	This file tries to stick to this  convention: "byte" is a fixnum in the range
;;;	[-128, 127], "octet" is a fixnum in the range [0, 255].
;;;
;;;Copyright (c) 2011-2017 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006,2007,2008  Abdulaziz Ghuloum
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; the port data structure
;;
;;A port object is a fixed-length memory  block referenced by machine words tagged as
;;vectors; the first  word of the vector is  the bitwise OR between a port  tag and a
;;bitvector of port attributes.
;;
;;The port  object allocation, accessors  and mutators  are all defined  as primitive
;;operations inlined by the compiler.
;;
;;Constructor: $make-port ATTRS IDX SZ BUF TR ID READ WRITE GETP SETP CL COOKIE
;;Arguments:
;;	ATTRS	- attributes as fixnum
;;	IDX	- index in input/output buffer,
;;	SZ	- number of octets/chars used in the input/output buffer,
;;      BUF	- input/output buffer,
;;      TR	- a boolean or a transcoder object
;;      ID	- a Scheme string describing the underlying device
;;      READ	- read procedure
;;      WRITE	- write procedure
;;      GETP	- get-position procedure
;;      SETP	- set-position procedure
;;      CL	- close procedure
;;      COOKIE	- device, position, line and column tracking
;;  Build a new port structure and return a Scheme value referencing it.  Notice that
;;  the underlying device is not among the constructor arguments: it is stored in the
;;  cookie and implicitly referenced by the functions READ, WRITE, GETP, SETP, CL.
;;
;;
;;Fields of a port block
;;----------------------
;;
;;Field name: tag
;;Field accessor: $port-tag PORT
;;  Extract from a port reference a fixnum representing the port attributes.  If PORT
;;  is not a port reference the return value is zero.
;;
;;Field accessor: $port-attrs PORT
;;  Extract from  a port reference a  fixnum representing the port  attributes.  PORT
;;  must be a port reference, otherwise the result is unspecified.
;;
;;Field name: index
;;Field accessor: $port-index PORT
;;Field Mutator: $set-port-index! PORT IDX
;;  Zero-based  fixnum  offset  of  the  current position  in  the  buffer;  see  the
;;  description of the BUFFER field below.
;;
;;  For an output port: it is the offset in the output buffer of the next location to
;;  be written to.
;;
;;  For an input port:  it is the offset in the input buffer  of the next location to
;;  be read from.
;;
;;Field name: size
;;Field accessor: $port-size PORT
;;Field mutator: $set-port-size! PORT SIZE
;;  Fixnum representing the number of octets/chars currently used in the input/output
;;  buffer; see the description of the BUFFER field below.
;;
;;  When the  device is a  Scheme string or bytevector  itself being the  I/O buffer:
;;  this field  is set to  the number of  characters in the  string or the  number of
;;  octets in the bytevector.
;;
;;Field name: buffer
;;Field accessor: $port-buffer PORT
;;  The  input/output  buffer  for  the  port.   The  buffer  is  allocated  at  port
;;  construction time and never reallocated.  The size of the buffers is customisable
;;  through a set of parameters.
;;
;;  It is mandatory to  have a buffer at least wide enough to  hold 2 characters with
;;  the widest  serialisation in  bytes.  This is  because: it is  possible to  put a
;;  transcoder on top of  every binary port; we need a place  to store partially read
;;  or written  characters; 2 characters can  be read at once  when doing end-of-line
;;  (EOL) conversion.
;;
;;  Built-in binary  ports have a  bytevector as  buffer.  Built-in textual  port may
;;  have a bytevector or string as buffer, a  bytevector is used when the port uses a
;;  transcoder.  Custom  binary ports  have a bytevector  as buffer.   Custom textual
;;  ports have a string as buffer.
;;
;;  As special cases: the port  returned by OPEN-BYTEVECTOR-INPUT-PORT has the source
;;  bytevector itself as buffer; the  port returned by OPEN-STRING-INPUT-PORT has the
;;  source string itself as buffer;  the port returned by OPEN-BYTEVECTOR-OUTPUT-PORT
;;  has a  bytevector as buffer; the  port returned by OPEN-STRING-OUTPUT-PORT  has a
;;  string as buffer.
;;
;;  When the port  has a platform's file descriptor as  underlying device: the buffer
;;  is a Scheme bytevector.
;;
;;  When performing output operations on an  underlying device: data is first written
;;  to the buffer and once in a while flushed to the device.  The current output port
;;  position is computed with:
;;
;;	   port position = device position + index
;;
;;                 device position
;;                        v
;;	   |--------------+---------------------------|device
;;                        |*****+*******+--------|buffer
;;                        ^     ^       ^        ^
;;                        0   index  used-size  size
;;
;;  When performing  input operations  on an  underlying device: a  block of  data is
;;  first copied  from the device into  the buffer and  then read from the  buffer to
;;  produce Scheme values.  The current input port position is computed with:
;;
;;	   port position = device position - used-size + index
;;	                 = device position - (used-size - index)
;;
;;                               device position
;;                                      v
;;	   |----------------------------+-------------|device
;;                      |*******+*******+--------|buffer
;;                      ^       ^       ^        ^
;;                      0     index  used-size  size
;;
;;  When the port is  input and the buffer is itself the  device: the device position
;;  is perpetually  set to the  buffer size.  The  current port position  is computed
;;  with:
;;
;;	   port position = device position - used-size + index = index
;;
;;                                              device position
;;                                                    v
;;	   |------------------------------------------|device
;;         |***************+**************************|buffer
;;         ^               ^                          ^
;;         0             index                used-size = size
;;
;;  When the port is output and the  buffer is itself the device: the device position
;;  is perpetually set to zero.  The current port position is computed with:
;;
;;	   port position = device position + index = index
;;
;;     device position
;;         v
;;	   |------------------------------------------|device
;;         |***************+**************************|buffer
;;         ^               ^                          ^
;;         0             index                used-size = size
;;
;;
;;Field name: transcoder
;;Field accessor: $port-transcoder PORT
;;  A boolean (#t, #f)  or a value of disjoint type  returned by MAKE-TRANSCODER.  It
;;  is #f for a binary port.  It is #t for a textual port having a string buffer.  It
;;  is a transcoder  for textual ports for which a  transcoder is explicitly selected
;;  or selected by default.
;;
;;  It is #t for the ports created by:
;;
;;	make-custom-textual-input-port
;;	make-custom-textual-output-port
;;	open-string-input-port
;;	open-string-input-port/id
;;	open-string-output-port
;;
;;  When  a transcoder:  it encodes  the  Unicode codec,  the end-of-line  conversion
;;  sytle, the error handling modes for errors in the serialisation of characters.
;;
;;Field name: id
;;Field accessor: $port-id
;;  A Scheme  string describing the  underlying device.  For  a port associated  to a
;;  file: it is  a Scheme string representing  the file name given  to functions like
;;  OPEN-OUTPUT-FILE.
;;
;;Field name: read!
;;Field accessor: $port-read! PORT
;;  Fill buffer policy.   It can be a procedure, the  symbol "all-data-in-buffer", or
;;  the boolean #f.
;;
;;  When  the  value is  a  procedure,  it must  be  a  function with  the  following
;;  signature:
;;
;;	(read! dst dst.start count)
;;
;;  DST.START will be a non-negative fixnum, COUNT  will be a positive fixnum and DST
;;  will be  a bytevector or  string whose length  is at least  DST.START+COUNT.  The
;;  procedure should  obtain up  to COUNT  bytes or characters  from the  device, and
;;  should  write them  to DST  starting at  index DST.START.   The procedure  should
;;  return a fixnum representing the number of  bytes it has read; to indicate an end
;;  of file, the procedure should write no bytes and return 0.
;;
;;  When  the  value  is  the  Scheme symbol  ALL-DATA-IN-BUFFER:  the  port  has  no
;;  underlying device, the buffer itself is the device.
;;
;;  When the value is #f: the port is output-only.
;;
;;Field name: write!
;;Field accessor: $port-write! PORT
;;  Flush buffer policy.  It can be a procedure or the boolean #f.
;;
;;  When  the  value is  a  procedure,  it must  be  a  function with  the  following
;;  signature:
;;
;;	(write! src src.start count)
;;
;;  SRC.START and COUNT will be non-negative fixnums, and SRC will be a bytevector or
;;  string whose length is at least SRC.START+COUNT.
;;
;;  The procedure should write up to COUNT bytes from SRC starting at index SRC.START
;;  to the  device.  In any case,  the procedure should return  a fixnum representing
;;  the number of bytes it wrote.
;;
;;  When the value is #f: the port is input-only.
;;
;;Field name: get-position
;;Field accessor: $port-get-position PORT
;;  Get  position  policy:  it is  used  to  retrieve  the  current position  in  the
;;  underlying device.  The device position is tracked by the POS field of the port's
;;  cookie.  The current device position is different from the current port position:
;;  see the BUFFER field for details.
;;
;;  This field can be set to #f, #t  or a procedure taking no arguments and returning
;;  the current device position.
;;
;;  - The value is  #f when the underlying device has no position.   In this case the
;;  port does not support the GET-POSITION operation.
;;
;;  - The value is #t for ports in  which the cookie's POS field is successfully used
;;  to track  the device  position; this  is the  case for  all the  non-custom ports
;;  instantiated by this library.  Custom ports cannot have this field set to #t.
;;
;;  As  a  special   case  the  ports  returned   by  OPEN-BYTEVECTOR-INPUT-PORT  and
;;  OPEN-STRING-INPUT-PORT use  the bytevector  or string itself  as buffer;  in such
;;  cases: the POS field of the cookie  is perpetually set to the buffer=device size,
;;  so the port  position equals the device position which  equals the current buffer
;;  index.
;;
;;  - The value is a procedure when the underlying device has a position which cannot
;;  be tracked by the cookie's POS field.  This  is the case for all the custom ports
;;  having a device.  When acquiring the  position fails: the procedure must raise an
;;  exception.
;;
;;Field name: set-position!
;;Field accessor: $port-set-position! PORT
;;  Set position  policy: it is  used to set the  current position in  the underlying
;;  device.  The device  position is tracked by  the POS field of  the port's cookie.
;;  The current device position is different  from the current port position: see the
;;  BUFFER field for details.
;;
;;  This field can be set to #f, #t  or a procedure taking the new device position as
;;  argument and returning unspecified values.
;;
;;  - The value is  #f when the underlying device has no position.   In this case the
;;  port does not support the SET-POSITION! operation.
;;
;;  -  The value  is #t  when the  cookie's POS  field holds  a value  representing a
;;  correct and  immutable device position.  In  this case the current  port position
;;  can be moved only by moving the current buffer index.
;;
;;  For   example    the   ports    returned   by    OPEN-BYTEVECTOR-INPUT-PORT   and
;;  OPEN-STRING-INPUT-PORT have  the buffer  itself as  underlying device;  for these
;;  ports: the POS field of the cookie is perpetually set to zero or the buffer size.
;;
;;     NOTE  At present  only the  ports returned  by OPEN-BYTEVECTOR-INPUT-PORT  and
;;     OPEN-STRING-INPUT-PORT have  this policy, so SET-PORT-POSITION!   is optimised
;;     for this case (Marco Maggi; Sep 21, 2011).
;;
;;  - The  value is  a procedure  when the  underlying device  has a  position.  When
;;  acquiring the position fails: the procedure must raise an exception.
;;
;;  While buffer arithmetics  is handled exclusively by fixnums,  the device position
;;  and the port  position is represented by non-negative exact  integers (fixnums or
;;  bignums).
;;
;;Field name: close
;;Field accessor: $port-close PORT
;;  Close device procedure.  It accepts no arguments.
;;
;;Field name: cookie
;;Field accessor: $port-cookie PORT
;;Field mutator: $set-port-cookie PORT COOKIE
;;  A struct  of type COOKIE used  to keep a  reference to the underlying  device and
;;  track its current position.  Additionally it can track line and column numbers in
;;  textual input ports.
;;


;;;; On buffering
;;
;;Given the implementation restrictions:
;;
;;* Strings have length at most equal to the return value of GREATEST-FIXNUM.
;;
;;* Bytevectors have length at most equal to the return value of GREATEST-FIXNUM.
;;
;;we establish the following constraints:
;;
;;*  If an  exact integer  is in  the range  representable by  a fixnum,  Vicare will
;;represent it as a fixnum.
;;
;;* No matter which BUFFER-MODE was selected, every port has a buffer.
;;
;;* The buffer is a Scheme bytevector or a Scheme string.
;;
;;* The input functions always read data from the buffer first.
;;
;;* The output functions always write data to the buffer first.
;;
;;*  %REFILL-INPUT-PORT-BYTEVECTOR-BUFFER is  the  only function  calling the  port's
;;READ!  function for  ports having a bytevector  as buffer; it copies  data from the
;;underlying device to the input buffer.
;;
;;* %REFILL-INPUT-PORT-STRING-BUFFER  is the only  function calling the  port's READ!
;;function for ports  having a string as  buffer; it copies data  from the underlying
;;device to the input buffer.
;;
;;* %FLUSH-OUTPUT-PORT is  the only function calling the port's  WRITE!  function for
;;both ports having a  string buffer and ports having a  bytevector buffer; if copies
;;data from the output buffer to the underlying device.
;;
;; ----------------------------------------------------------------------------------
;;
;;From the constraints it follows that:
;;
;;* All  the arithmetics involving  the buffer can  be performed using  unsafe fixnum
;;functions.
;;
;; ----------------------------------------------------------------------------------
;;
;;The buffer  mode is  customisable only  for output operations;  the buffer  mode is
;;ignored by input ports.  Buffer mode handling is as follows:
;;
;;*  When the  mode  is  NONE: data  is  first written  to  the  output buffer,  then
;;immediately sent to the underlying device.
;;
;;* When the mode is LINE: data is first written to the output buffer up to the first
;;newline, then immediately sent to the underlying device.
;;
;;* When the mode is BLOCK: data is first written to the output buffer; only when the
;;buffer is full data is sent to the underlying device.
;;


#!vicare
(library (ikarus.io)
  (export
    ;; port parameters
    standard-input-port standard-output-port standard-error-port
    current-input-port  current-output-port  current-error-port
    console-output-port console-error-port   console-input-port

    bytevector-port-buffer-size		string-port-buffer-size
    input-file-buffer-size		output-file-buffer-size
    input/output-file-buffer-size	input/output-socket-buffer-size

    ;; predicates
    port?
    input-port?				output-port?
    input-only-port?			output-only-port?
    input/output-port?
    textual-port?			binary-port?
    binary-input-port?			textual-input-port?
    binary-output-port?			textual-output-port?
    binary-input/output-port?		textual-input/output-port?
    binary-input-only-port?		binary-output-only-port?
    textual-input-only-port?		textual-output-only-port?
    port-eof?

    open-port?				closed-port?
    open-input-port?			open-output-port?
    open-textual-port?			open-binary-port?
    open-input/output-port?
    open-binary-input-port?		open-textual-input-port?
    open-binary-output-port?		open-textual-output-port?
    open-binary-input/output-port?	open-textual-input/output-port?

    ;; generic port functions
    call-with-port

    ;; input from files
    open-file-input-port open-input-file
    call-with-input-file with-input-from-file

    ;; input from strings and bytevectors
    open-bytevector-input-port
    open-string-input-port open-string-input-port/id
    with-input-from-string

    ;; output functions
    flush-output-port

    ;; output to files
    open-file-output-port open-output-file
    call-with-output-file with-output-to-file

    ;; output to bytevectors
    open-bytevector-output-port call-with-bytevector-output-port

    ;; output to strings
    open-string-output-port with-output-to-string get-output-string
    with-output-to-port
    call-with-string-output-port

    ;; input/output to files
    open-file-input/output-port

    ;; custom ports
    make-custom-binary-input-port
    make-custom-binary-output-port
    make-custom-textual-input-port
    make-custom-textual-output-port
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port

    ;; file descriptor ports
    make-binary-file-descriptor-input-port
    make-binary-file-descriptor-input-port*
    make-binary-file-descriptor-output-port
    make-binary-file-descriptor-output-port*
    make-binary-file-descriptor-input/output-port
    make-binary-file-descriptor-input/output-port*
    make-textual-file-descriptor-input-port
    make-textual-file-descriptor-input-port*
    make-textual-file-descriptor-output-port
    make-textual-file-descriptor-output-port*
    make-textual-file-descriptor-input/output-port
    make-textual-file-descriptor-input/output-port*

    ;; transcoders
    transcoded-port port-transcoder

    ;; closing ports
    close-port port-closed? close-input-port close-output-port

    ;; port position
    port-position port-has-port-position?
    set-port-position! port-has-set-port-position!?
    get-char-and-track-textual-position
    port-textual-position

    ;; reading chars
    get-char lookahead-char read-char peek-char

    ;; reading strings
    get-string-n get-string-n! get-string-all get-string-some
    get-line read-line

    ;; reading octets
    get-u8 lookahead-u8 lookahead-two-u8

    ;; reading bytevectors
    get-bytevector-n get-bytevector-n!
    get-bytevector-some get-bytevector-all

    ;; writing octets and bytevectors
    put-u8 put-bytevector

    ;; writing chars and strings
    put-char write-char put-string newline

    ;; port configuration
    port-mode			set-port-mode!
    output-port-buffer-mode	set-port-buffer-mode!
    reset-input-port!		reset-output-port!
    port-id			port-fd
    port-uid			port-hash
    string->filename-func	filename->string-func
    (rename (string->filename-func	string->pathname-func)
	    (filename->string-func	pathname->string-func))
    port-dump-status
    port-set-non-blocking-mode!	port-unset-non-blocking-mode!
    port-in-non-blocking-mode?

    ;; port properties
    port-putprop		port-getprop
    port-remprop		port-property-list

    ;; networking
    make-binary-socket-input-port
    make-binary-socket-input-port*
    make-binary-socket-output-port
    make-binary-socket-output-port*
    make-binary-socket-input/output-port
    make-binary-socket-input/output-port*
    make-textual-socket-input-port
    make-textual-socket-input-port*
    make-textual-socket-output-port
    make-textual-socket-output-port*
    make-textual-socket-input/output-port
    make-textual-socket-input/output-port*

    #| end of EXPORT |# )
  (import (except (vicare)
		  ;; port parameters
		  standard-input-port standard-output-port standard-error-port
		  current-input-port  current-output-port  current-error-port
		  console-output-port console-error-port   console-input-port
		  bytevector-port-buffer-size	string-port-buffer-size
		  input-file-buffer-size	output-file-buffer-size
		  input/output-file-buffer-size	input/output-socket-buffer-size

		  ;; predicates
		  port?
		  input-port?				output-port?
		  input-only-port?			output-only-port?
		  input/output-port?
		  textual-port?				binary-port?
		  binary-input-port?			textual-input-port?
		  binary-output-port?			textual-output-port?
		  binary-input/output-port?		textual-input/output-port?
		  binary-input-only-port?		binary-output-only-port?
		  textual-input-only-port?		textual-output-only-port?
		  port-eof?

		  open-port?				closed-port?
		  open-input-port?			open-output-port?
		  open-textual-port?			open-binary-port?
		  open-input/output-port?
		  open-binary-input-port?		open-textual-input-port?
		  open-binary-output-port?		open-textual-output-port?
		  open-binary-input/output-port?	open-textual-input/output-port?

		  ;; generic port functions
		  call-with-port

		  ;; input from files
		  open-file-input-port open-input-file
		  call-with-input-file with-input-from-file

		  ;; input from strings and bytevectors
		  open-bytevector-input-port
		  open-string-input-port open-string-input-port/id
		  with-input-from-string

		  ;; output functions
		  flush-output-port

		  ;; output to files
		  open-file-output-port open-output-file
		  call-with-output-file with-output-to-file

		  ;; output to bytevectors
		  open-bytevector-output-port call-with-bytevector-output-port

		  ;; output to strings
		  open-string-output-port with-output-to-string
		  with-output-to-port
		  call-with-string-output-port
		  get-output-string

		  ;; input/output to files
		  open-file-input/output-port

		  ;; custom ports
		  make-custom-binary-input-port
		  make-custom-binary-output-port
		  make-custom-textual-input-port
		  make-custom-textual-output-port
		  make-custom-binary-input/output-port
		  make-custom-textual-input/output-port

		  ;; file descriptor ports
		  make-binary-file-descriptor-input-port
		  make-binary-file-descriptor-input-port*
		  make-binary-file-descriptor-output-port
		  make-binary-file-descriptor-output-port*
		  make-binary-file-descriptor-input/output-port
		  make-binary-file-descriptor-input/output-port*
		  make-textual-file-descriptor-input-port
		  make-textual-file-descriptor-input-port*
		  make-textual-file-descriptor-output-port
		  make-textual-file-descriptor-output-port*
		  make-textual-file-descriptor-input/output-port
		  make-textual-file-descriptor-input/output-port*

		  ;; transcoders
		  transcoded-port port-transcoder

		  ;; closing ports
		  close-port port-closed? close-input-port close-output-port

		  ;; port position
		  port-position port-has-port-position?
		  set-port-position! port-has-set-port-position!?
		  get-char-and-track-textual-position
		  port-textual-position

		  ;; reading chars
		  get-char lookahead-char read-char peek-char

		  ;; reading strings
		  get-string-n get-string-n! get-string-all get-string-some
		  get-line read-line

		  ;; reading octets
		  get-u8 lookahead-u8 lookahead-two-u8

		  ;; reading bytevectors
		  get-bytevector-n get-bytevector-n!
		  get-bytevector-some get-bytevector-all

		  ;; writing octets and bytevectors
		  put-u8 put-bytevector

		  ;; writing chars and strings
		  put-char write-char put-string newline

		  ;; port configuration
		  port-mode			set-port-mode!
		  output-port-buffer-mode	set-port-buffer-mode!
		  reset-input-port!		reset-output-port!
		  port-id			port-fd
		  port-uid			port-hash
		  string->filename-func		filename->string-func
		  string->pathname-func		pathname->string-func
		  port-dump-status
		  port-set-non-blocking-mode!	port-unset-non-blocking-mode!
		  port-in-non-blocking-mode?

		  ;; port properties
		  port-putprop			port-getprop
		  port-remprop			port-property-list

		  ;; networking
		  make-binary-socket-input-port
		  make-binary-socket-input-port*
		  make-binary-socket-output-port
		  make-binary-socket-output-port*
		  make-binary-socket-input/output-port
		  make-binary-socket-input/output-port*
		  make-textual-socket-input-port
		  make-textual-socket-input-port*
		  make-textual-socket-output-port
		  make-textual-socket-output-port*
		  make-textual-socket-input/output-port
		  make-textual-socket-input/output-port*)
    (vicare system structs)
    (only (ikarus.options)
	  strict-r6rs)
    (except (vicare system $fx)
	    $fxand
	    $fxior
	    $fxxor)
    (vicare system $chars)
    (vicare system $pairs)
    (vicare system $structs)
    (vicare system $strings)
    (vicare system $bytevectors)
    ;;This internal library is the one exporting: $MAKE-PORT, $PORT-* and $SET-PORT-*
    ;;bindings.
    (vicare system $io)
    (prefix (only (vicare) port?) primop::)
    (prefix (vicare unsafe capi) capi::)
    (prefix (vicare unsafe unicode) unicode::)
    (vicare platform constants)
    (only (ikarus unique-objects)
	  WOULD-BLOCK-OBJECT))


;;;; helpers

(define-syntax-rule (define-false-or-predicate ?who ?pred)
  (define (?who obj)
    (or (not   obj)
	(?pred obj))))

(define port-position?		non-negative-exact-integer?)
(define port-identifier?	string?)

(define-false-or-predicate false-or-procedure?	procedure?)
(define-false-or-predicate false-or-transcoder?	transcoder?)

(define filename?		string?)
(define file-options?		enum-set?)

(define (fixnum-octet? obj)
  (and (fixnum? obj)
       ($fxnonnegative? obj)
       ($fx< obj 256)))

(define (fixnum-count? obj)
  (and (fixnum? obj)
       ($fxnonnegative? obj)))

(define (fixnum-start-index? obj)
  (and (fixnum? obj)
       ($fxnonnegative? obj)))

(define (file-descriptor? obj)
  (and (fixnum? obj)
       ($fxnonnegative? obj)))

(define (port-mode? obj)
  (or (eq? obj 'r6rs)
      (eq? obj 'vicare)))

(define (port-buffer-mode? obj)
  (or (eq? obj 'line)
      (eq? obj 'none)
      (eq? obj 'block)))

(define (port-with-file-descriptor? obj)
  (and (port? obj)
       (let ((port obj))
	 (with-port (port) port.fd-device?))))

(define-syntax (define-module-who stx)
  (syntax-case stx ()
    ((_ ?module-who)
     (with-syntax
	 ((MODULE-WHO (datum->syntax #'?module-who '__module_who__)))
       #'(define-syntax MODULE-WHO
	   (identifier-syntax (quote ?module-who)))))
    ))

;;; --------------------------------------------------------------------
;;; bytevector-related assertions

(define-syntax-rule (assert-start-index-for-bytevector ?bv ?idx)
  (unless ($fx<= ?idx ($bytevector-length ?bv))
    (procedure-arguments-consistency-violation __who__
      (string-append "start index argument " (number->string ?idx)
		     " too big for bytevector of length "
		     (number->string ($bytevector-length ?bv)))
      ?bv ?idx)))

(define-syntax-rule (assert-count-from-start-index-for-bytevector ?bv ?start ?count)
  ;;We know  that COUNT and START  are fixnums, but  not if START+COUNT is  a fixnum,
  ;;too.
  ;;
  (unless (<= (+ ?start ?count) ($bytevector-length ?bv))
    (procedure-arguments-consistency-violation __who__
      (string-append "count argument "    (number->string ?count)
		     " from start index " (number->string ?start)
		     " too big for bytevector of length "
		     (number->string ($bytevector-length ?bv)))
      ?start ?count ($bytevector-length ?bv))))

;;; --------------------------------------------------------------------
;;; string-related assertions

(define-syntax-rule (assert-start-index-for-string ?str ?idx)
  (unless ($fx<= ?idx ($string-length ?str))
    (procedure-arguments-consistency-violation __who__
      (string-append "start index argument " (number->string ?idx)
		     " too big for string of length "
		     (number->string ($string-length ?str)))
      ?str ?idx)))

(define-syntax-rule (assert-count-from-start-index-for-string ?str ?start ?count)
  ;;We know  that COUNT and START  are fixnums, but  not if START+COUNT is  a fixnum,
  ;;too.
  ;;
  (unless (<= (+ ?start ?count) ($string-length ?str))
    (procedure-arguments-consistency-violation __who__
      (string-append "count argument "    (number->string ?count)
		     " from start index " (number->string ?start)
		     " too big for string of length "
		     (number->string ($string-length ?str)))
      ?start ?count ($string-length ?str))))

;;; --------------------------------------------------------------------

(define-syntax $fxand
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     ($fxlogand ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     ($fxlogand ?op1 ($fxand ?op2 . ?ops)))))

(define-syntax $fxior
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     ($fxlogor ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     ($fxlogor ?op1 ($fxior ?op2 . ?ops)))))

(define-syntax $fxxor
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     ($fxlogxor ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     ($fxlogxor ?op1 ($fxxor ?op2 . ?ops)))))

(define-syntax debug-assert
  ;;This is meant to expand to nothing when debugging is turned off.
  ;;
  (if #f
      (syntax-rules ()
  	((_ ?pred)
  	 (assert ?pred)))
    (syntax-rules ()
      ((_ ?pred)
       (values)))))
;;; --------------------------------------------------------------------

(define-inline ($char-is-single-char-line-ending? ch)
  (or ($char= ch #\x000A)   ;linefeed
      ($char= ch #\x0085)   ;next line
      ($char= ch #\x2028))) ;line separator

(define-inline ($char-is-carriage-return? ch)
  ($char= ch #\xD))

(define-inline ($char-is-newline-after-carriage-return? ch)
  ;;This is used to recognise 2-char newline sequences.
  ;;
  (or ($char= ch #\x000A)   ;linefeed
      ($char= ch #\x0085))) ;next line


;;;; error helpers

(define-syntax-rule (%implementation-violation ?who ?message . ?irritants)
  (assertion-violation ?who ?message . ?irritants))

(define (%raise-port-position-out-of-range who port new-position)
  (raise (condition
	  (make-who-condition who)
	  (make-message-condition "attempt to set port position beyond limit")
	  (make-i/o-invalid-position-error new-position)
	  (make-irritants-condition (list port)))))


;;;; generic helpers

;;ALL-DATA-IN-BUFFER is  used in place  of the READ!   procedure to mark  ports whose
;;buffer is all the data there is, that is: there is no underlying device.
;;
(define-syntax all-data-in-buffer
  (identifier-syntax 'all-data-in-buffer))

(define-constant UTF-8-BYTE-ORDER-MARK-LIST			'(#xEF #xBB #xBF))
(define-constant UTF-16-BIG-ENDIAN-BYTE-ORDER-MARK-LIST		'(#xFE #xFF))
(define-constant UTF-16-LITTLE-ENDIAN-BYTE-ORDER-MARK-LIST	'(#xFF #xFE))

(define-auxiliary-syntaxes
  data-is-needed-at:
  if-available-data:
  if-available-room:
  if-end-of-file:
  if-no-match-raise-assertion-violation
  if-no-match:
  if-successful-match:
  if-successful-refill:
  if-empty-buffer-and-refilling-would-block:
  if-refilling-would-block:
  room-is-needed-for:)

(define-syntax case-errno
  (syntax-rules (else)
    ((_ ?errno ((?code0 ?code ...) . ?body) ... (else . ?else-body))
     (let ((errno ?errno))
       (cond ((or (and (fixnum? ?code0) ($fx= errno ?code0))
		  (and (fixnum? ?code)  ($fx= errno ?code))
		  ...)
	      . ?body)
	     ...
	     (else . ?else-body))))
    ((_ ?errno ((?code0 ?code ...) . ?body) ...)
     (let ((errno ?errno))
       (cond ((or (and (fixnum? ?code0) ($fx= errno ?code0))
		  (and (fixnum? ?code)  ($fx= errno ?code))
		  ...)
	      . ?body)
	     ...
	     (else
	      (assertion-violation #f "unexpected errno code" errno)))))
    ))

(define-syntax ($case-fixnums stx)
  (syntax-case stx (else)
    ((_ ?id ((?fx) . ?body) ...)
     (identifier? #'?id)
     #'(cond (($fx= ?fx ?id) . ?body) ...))
    ((_ ?id ((?fx) . ?body) ... (else . ?else-body))
     (identifier? #'?id)
     #'(cond (($fx= ?fx ?id) . ?body) ...  (else . ?else-body)))
    ))


;;;; Byte Order Mark (BOM) parsing

(module (%parse-byte-order-mark)

  (define-syntax %parse-byte-order-mark
    (syntax-rules (if-successful-match: if-no-match: if-end-of-file:)
      ((%parse-byte-order-mark (?who ?port ?bom)
	 (if-successful-match:	. ?matched-body)
	 (if-no-match:		. ?failure-body)
	 (if-end-of-file:	. ?eof-body))
       (let ((result (%parse-it ?who ?port ?bom)))
	 (cond ((boolean? result)
		(if result
		    (begin . ?matched-body)
		  (begin . ?failure-body)))
	       ((eof-object? result)
		(begin . ?eof-body))
	       (else
		(assertion-violation ?who
		  "vicare internal error: invalid return value while parsing BOM" result)))))))

  (define (%parse-it who port bom)
    ;;Assuming PORT is an open input port  with a bytevector buffer: read and consume
    ;;octets from PORT verifying if they  match the given list of octets representing
    ;;a Byte Order Mark (BOM).
    ;;
    ;;PORT must  be an open  textual or binary input  port with a  bytevector buffer.
    ;;BOM  must be  a  list of  fixnums  representing the  expected  Byte Order  Mark
    ;;sequence.
    ;;
    ;;Return #t if the whole BOM sequence is  read and matched; in this case the port
    ;;position is left right after the BOM sequence.
    ;;
    ;;Return #f if  the octets from the port  do not match the BOM  sequence; in this
    ;;case the port  position is left at  the same point it was  before this function
    ;;call.
    ;;
    ;;Return the EOF object if the port  reaches EOF before the whole BOM is matched;
    ;;in this  case the port position  is left at the  same point it was  before this
    ;;function call.
    ;;
    (with-port (port)
      (let next-octet-in-bom ((number-of-consumed-octets 0)
			      (bom bom))
	(if (pair? bom)
	    (let retry-after-filling-buffer ()
	      (let ((buffer.offset (fx+ number-of-consumed-octets port.buffer.index)))
		(maybe-refill-bytevector-buffer-and-evaluate (port who)
		  (data-is-needed-at: buffer.offset)
		  (if-end-of-file:
		   (eof-object))
		  (if-empty-buffer-and-refilling-would-block:
		   WOULD-BLOCK-OBJECT)
		  (if-successful-refill:
		   (retry-after-filling-buffer))
		  (if-available-data:
		   (and (fx= (car bom) ($bytevector-u8-ref port.buffer buffer.offset))
			(next-octet-in-bom (fxadd1 number-of-consumed-octets) (cdr bom))))
		  )))
	  ;;Full success: all the octets in the given BOM sequence where matched.
	  (begin
	    (port.buffer.index.incr! number-of-consumed-octets)
	    #t)))))

  #| end of module |# )

(define (%parse-utf16-bom-and-add-fast-tag who port)
  ;;Assuming PORT is an open textual input port object with a bytevector buffer and a
  ;;UTF-16 transcoder not yet specialised for an endianness: read the Byte Order Mark
  ;;and  mutate the  port's attributes  tagging the  port as  FAST-GET-UTF16BE-TAG or
  ;;FAST-GET-UTF16LE-TAG.  Return #t if  port is at EOF, #f otherwise.   If no BOM is
  ;;present, select big endian by default.
  ;;
  (with-port-having-bytevector-buffer (port)
    (let ((result-if-successful-tagging		#f)
	  (result-if-end-of-file		#t))
      (%parse-byte-order-mark (who port UTF-16-BIG-ENDIAN-BYTE-ORDER-MARK-LIST)
	(if-successful-match:
	 (set! port.fast-attributes FAST-GET-UTF16BE-TAG)
	 result-if-successful-tagging)
	(if-no-match:
	 (%parse-byte-order-mark (who port UTF-16-LITTLE-ENDIAN-BYTE-ORDER-MARK-LIST)
	   (if-successful-match:
	    (set! port.fast-attributes FAST-GET-UTF16LE-TAG)
	    result-if-successful-tagging)
	   (if-no-match:
	    (set! port.fast-attributes FAST-GET-UTF16BE-TAG)
	    result-if-successful-tagging)
	   (if-end-of-file: result-if-end-of-file)))
	(if-end-of-file: result-if-end-of-file)))))

(module (%parse-bom-and-add-fast-tag)

  (define-syntax %parse-bom-and-add-fast-tag
    (syntax-rules (if-successful-match: if-end-of-file: if-no-match-raise-assertion-violation)
      ((%parse-bom-and-add-fast-tag (?who ?port)
	 (if-successful-match:		. ?matched-body)
	 (if-end-of-file:		. ?eof-body)
	 (if-no-match-raise-assertion-violation))
       (if (%parse-it ?who ?port)
	   (begin . ?eof-body)
	 (begin . ?matched-body)))))

  (define (%parse-it who port)
    ;;Assuming PORT is an open textual input  port with a bytevector buffer: read the
    ;;Byte Order Mark  expected for the port's transcoder and  mutate the port's fast
    ;;attributes, tagging  the port  accordingly.  Return  #t if port  is at  EOF, #f
    ;;otherwise.
    ;;
    ;;If PORT is already tagged: existing fast attributes are ignored.
    ;;
    ;;If  the input  octects  do not  match  the requested  BOM:  raise an  assertion
    ;;violation.
    ;;
    ;;Notice that Latin-1 encoding has no BOM.
    ;;
    (with-port-having-bytevector-buffer (port)
      (debug-assert port.transcoder)
      (case (transcoder-codec port.transcoder)
	((utf-8-codec)
	 (%parse-byte-order-mark (who port UTF-8-BYTE-ORDER-MARK-LIST)
	   (if-successful-match:
	    (set! port.fast-attributes FAST-GET-UTF8-TAG)
	    #f)
	   (if-no-match:
	    (assertion-violation who
	      "expected to read UTF-8 big endian Byte Order Mark from port" port))
	   (if-end-of-file: #t)))

	((utf-16-codec)
	 (%parse-utf16-bom-and-add-fast-tag who port))

	((utf-16be-codec)
	 (%parse-byte-order-mark (who port UTF-16-BIG-ENDIAN-BYTE-ORDER-MARK-LIST)
	   (if-successful-match:
	    (set! port.fast-attributes FAST-GET-UTF16BE-TAG)
	    #f)
	   (if-no-match:
	    (assertion-violation who
	      "expected to read UTF-16 big endian Byte Order Mark from port" port))
	   (if-end-of-file: #t)))

	((utf-16le-codec)
	 (%parse-byte-order-mark (who port UTF-16-LITTLE-ENDIAN-BYTE-ORDER-MARK-LIST)
	   (if-successful-match:
	    (set! port.fast-attributes FAST-GET-UTF16LE-TAG)
	    #f)
	   (if-no-match:
	    (assertion-violation who
	      "expected to read UTF-16 little endian Byte Order Mark from port" port))
	   (if-end-of-file: #t)))

	((utf-bom-codec)
	 ;;Try all the UTF encodings in the order: UTF-8, UTF-16-be, UTF-16-le.
	 (%parse-byte-order-mark (who port UTF-8-BYTE-ORDER-MARK-LIST)
	   (if-successful-match:
	    (set! port.fast-attributes FAST-GET-UTF8-TAG)
	    #f)
	   (if-no-match:
	    (%parse-byte-order-mark (who port UTF-16-BIG-ENDIAN-BYTE-ORDER-MARK-LIST)
	      (if-successful-match:
	       (set! port.fast-attributes FAST-GET-UTF16BE-TAG)
	       #f)
	      (if-no-match:
	       (%parse-byte-order-mark (who port UTF-16-LITTLE-ENDIAN-BYTE-ORDER-MARK-LIST)
		 (if-successful-match:
		  (set! port.fast-attributes FAST-GET-UTF16LE-TAG)
		  #f)
		 (if-no-match:
		  (assertion-violation who
		    "expected to read supported UTF Byte Order Mark from port" port))
		 (if-end-of-file: #t)))
	      (if-end-of-file: #t)))
	   (if-end-of-file: #t)))

	(else
	 (assertion-violation who
	   "codec not handled by BOM parser" (transcoder-codec port.transcoder))))))

  #| end of module |# )


;;;; dot notation macros for port structures

(module (with-port with-port-having-bytevector-buffer with-port-having-string-buffer)

  (define-syntax with-port
    (syntax-rules ()
      ((_ (?port) . ?body)
       (%with-port (?port #f) . ?body))))

  (define-syntax with-port-having-bytevector-buffer
    (syntax-rules ()
      ((_ (?port) . ?body)
       (%with-port (?port $bytevector-length) . ?body))))

  (define-syntax with-port-having-string-buffer
    (syntax-rules ()
      ((_ (?port) . ?body)
       (%with-port (?port $string-length) . ?body))))

  (define-syntax (%with-port stx)
    (syntax-case stx ()
      ((_ (?port ?buffer-length) . ?body)
       (let* ((port-id	#'?port)
	      (port-str	(symbol->string (syntax->datum port-id))))
	 (define (%dot-id field-str)
	   (datum->syntax port-id (string->symbol (string-append port-str field-str))))
	 (with-syntax
	     ((PORT.TAG				(%dot-id ".tag"))
		;fixnum, bits representing the port tag
	      (PORT.ATTRIBUTES			(%dot-id ".attributes"))
		;fixnum, bits representing the port attributes
	      (PORT.BUFFER.INDEX		(%dot-id ".buffer.index"))
		;fixnum, the offset from the buffer beginning
	      (PORT.BUFFER.USED-SIZE		(%dot-id ".buffer.used-size"))
		;fixnum, number of octets used in the buffer
	      (PORT.BUFFER			(%dot-id ".buffer"))
		;bytevector, the buffer
	      (PORT.TRANSCODER			(%dot-id ".transcoder"))
		;the transcoder or false
	      (PORT.ID				(%dot-id ".id"))
		;string, describes the port
	      (PORT.READ!			(%dot-id ".read!"))
		;function or false, the read function
	      (PORT.WRITE!			(%dot-id ".write!"))
		;function or false, the write function
	      (PORT.SET-POSITION!		(%dot-id ".set-position!"))
		;function or false, the function to set the position
	      (PORT.GET-POSITION		(%dot-id ".get-position"))
		;function or false, the function to get the position
	      (PORT.CLOSE			(%dot-id ".close"))
		;function or false, the function to close the port
	      (PORT.COOKIE			(%dot-id ".cookie"))
		;cookie record
	      (PORT.BUFFER.FULL?		(%dot-id ".buffer.full?"))
		;true if the buffer is full
	      (PORT.CLOSED?			(%dot-id ".closed?"))
		;true if the port is closed
	      (PORT.GUARDED?			(%dot-id ".guarded?"))
		;true if the port is registered in the port guardian
	      (PORT.FD-DEVICE?			(%dot-id ".fd-device?"))
		;true if the port has a file descriptor as device
	      (PORT.WITH-EXTRACTION?		(%dot-id ".with-extraction?"))
		;true if the port has an associated extraction function
	      (PORT.IS-INPUT-AND-OUTPUT?	(%dot-id ".is-input-and-output?"))
		;true if the port is both input and output
	      (PORT.IS-INPUT?			(%dot-id ".is-input?"))
		;true if the port is an input or input/output port
	      (PORT.IS-OUTPUT?			(%dot-id ".is-output?"))
		;true if the port is an output or input/output port
	      (PORT.IS-INPUT-ONLY?		(%dot-id ".is-input-only?"))
		;true if the port is an input-only port
	      (PORT.IS-OUTPUT-ONLY?		(%dot-id ".is-output-only?"))
		;true if the port is an output-only port
	      (PORT.LAST-OPERATION-WAS-INPUT?	(%dot-id ".last-operation-was-input?"))
		;true if the  port is input or the  port is input/output
		;and the  last operation was input; in  other words: the
		;buffer may contain input bytes
	      (PORT.LAST-OPERATION-WAS-OUTPUT?	(%dot-id ".last-operation-was-output?"))
		;true if the port is  output or the port is input/output
		;and the last operation  was output; in other words: the
		;buffer may contain output bytes
	      (PORT.BUFFER-MODE-LINE?		(%dot-id ".buffer-mode-line?"))
		;true if the port has LINE as buffer mode
	      (PORT.BUFFER-MODE-NONE?		(%dot-id ".buffer-mode-none?"))
		;true if the port has NONE as buffer mode
	      (PORT.FAST-ATTRIBUTES		(%dot-id ".fast-attributes"))
		;fixnum, the fast tag attributes bits
	      (PORT.OTHER-ATTRIBUTES		(%dot-id ".other-attributes"))
		;fixnum, the non-fast-tag attributes bits
	      (PORT.BUFFER.SIZE			(%dot-id ".buffer.size"))
		;fixnum, the buffer size
	      (PORT.BUFFER.RESET-TO-EMPTY!	(%dot-id ".buffer.reset-to-empty!"))
		;method, set the buffer index and used size to zero
	      (PORT.BUFFER.ROOM			(%dot-id ".buffer.room"))
		;method, the number of octets available in the buffer
	      (PORT.BUFFER.INDEX.INCR!		(%dot-id ".buffer.index.incr!"))
		;method, increment the buffer index
	      (PORT.BUFFER.USED-SIZE.INCR!	(%dot-id ".buffer.used-size.incr!"))
		;method, increment
	      (PORT.MARK-AS-CLOSED!		(%dot-id ".mark-as-closed!"))
		;method, mark the port as closed
	      (PORT.DEVICE			(%dot-id ".device"))
		;false or Scheme value, references the underlying device
	      (PORT.DEVICE.POSITION		(%dot-id ".device.position"))
		;exact integer, track the current device position
	      (PORT.DEVICE.POSITION.INCR!		(%dot-id ".device.position.incr!"))
		;method, increment the POS field of the cookie
	      (PORT.MODE				(%dot-id ".mode"))
		;Scheme symbol, select the port mode
	      )
	   #'(let-syntax
		 ((PORT.TAG				(identifier-syntax ($port-tag		?port)))
		  (PORT.BUFFER				(identifier-syntax ($port-buffer	?port)))
		  (PORT.TRANSCODER			(identifier-syntax ($port-transcoder	?port)))
		  (PORT.ID				(identifier-syntax ($port-id		?port)))
		  (PORT.READ!				(identifier-syntax ($port-read!		?port)))
		  (PORT.WRITE!				(identifier-syntax ($port-write!	?port)))
		  (PORT.SET-POSITION!			(identifier-syntax ($port-set-position!	?port)))
		  (PORT.GET-POSITION			(identifier-syntax ($port-get-position	?port)))
		  (PORT.CLOSE				(identifier-syntax ($port-close		?port)))
		  (PORT.COOKIE				(identifier-syntax ($port-cookie	?port)))
		  (PORT.CLOSED?				(identifier-syntax ($port-closed? ?port)))
		  (PORT.GUARDED?			(identifier-syntax ($guarded-port? ?port)))
		  (PORT.FD-DEVICE?			(identifier-syntax ($port-with-fd-device? ?port)))
		  (PORT.WITH-EXTRACTION?		(identifier-syntax ($port-with-extraction? ?port)))
		  (PORT.IS-INPUT-AND-OUTPUT?		(identifier-syntax ($input/output-port? ?port)))
		  (PORT.IS-INPUT?			(identifier-syntax ($input-port? ?port)))
		  (PORT.IS-OUTPUT?			(identifier-syntax ($output-port? ?port)))
		  (PORT.IS-INPUT-ONLY?			(identifier-syntax ($input-only-port? ?port)))
		  (PORT.IS-OUTPUT-ONLY?			(identifier-syntax ($output-only-port? ?port)))
		  (PORT.LAST-OPERATION-WAS-INPUT?	(identifier-syntax ($last-port-operation-was-input? ?port)))
		  (PORT.LAST-OPERATION-WAS-OUTPUT?	(identifier-syntax ($last-port-operation-was-output? ?port)))
		  (PORT.BUFFER-MODE-LINE?		(identifier-syntax ($port-buffer-mode-line? ?port)))
		  (PORT.BUFFER-MODE-NONE?		(identifier-syntax ($port-buffer-mode-none? ?port)))
		  (PORT.ATTRIBUTES			(identifier-syntax
							 (_		($port-attrs ?port))
							 ((set! id ?value) ($set-port-attrs! ?port ?value))))
		  (PORT.FAST-ATTRIBUTES			(identifier-syntax
							 (_		($port-fast-attrs ?port))
							 ((set! _ ?tag)	($set-port-fast-attrs! ?port ?tag))))
		  (PORT.OTHER-ATTRIBUTES		(identifier-syntax ($port-other-attrs ?port)))
		  (PORT.BUFFER.SIZE			(identifier-syntax (?buffer-length ($port-buffer ?port))))
		  (PORT.BUFFER.INDEX			(identifier-syntax
							 (_			($port-index ?port))
							 ((set! _ ?value)	($set-port-index! ?port ?value))))
		  (PORT.BUFFER.USED-SIZE		(identifier-syntax
							 (_			($port-size ?port))
							 ((set! _ ?value)	($set-port-size! ?port ?value)))))
	       (let-syntax
		   ((PORT.BUFFER.FULL?
		     (identifier-syntax ($fx= PORT.BUFFER.USED-SIZE PORT.BUFFER.SIZE)))
		    (PORT.BUFFER.ROOM
		     (syntax-rules ()
		       ((_)
			($fx- PORT.BUFFER.SIZE PORT.BUFFER.INDEX))))
		    (PORT.BUFFER.RESET-TO-EMPTY!
		     (syntax-rules ()
		       ((_)
			(begin
			  (set! PORT.BUFFER.INDEX     0)
			  (set! PORT.BUFFER.USED-SIZE 0)))))
		    (PORT.BUFFER.INDEX.INCR!
		     (syntax-rules ()
		       ((_)
			(set! PORT.BUFFER.INDEX ($fxadd1 PORT.BUFFER.INDEX)))
		       ((_ ?step)
			(set! PORT.BUFFER.INDEX ($fx+ ?step PORT.BUFFER.INDEX)))))
		    (PORT.BUFFER.USED-SIZE.INCR!
		     (syntax-rules ()
		       ((_)
			(set! PORT.BUFFER.USED-SIZE ($fxadd1 PORT.BUFFER.USED-SIZE)))
		       ((_ ?step)
			(set! PORT.BUFFER.USED-SIZE ($fx+ ?step PORT.BUFFER.USED-SIZE)))))
		    (PORT.MARK-AS-CLOSED!
		     (syntax-rules ()
		       ((_)
			($mark-port-closed! ?port))))
		    (PORT.DEVICE
		     (identifier-syntax
		      (_ (cookie-dest PORT.COOKIE))
		      ((set! _ ?new-device)
		       (set-cookie-dest! PORT.COOKIE ?new-device))))
		    (PORT.DEVICE.POSITION
		     (identifier-syntax
		      (_ (cookie-pos PORT.COOKIE))
		      ((set! _ ?new-position)
		       (set-cookie-pos! PORT.COOKIE ?new-position))))
		    (PORT.DEVICE.POSITION.INCR!
		     (syntax-rules ()
		       ((_ ?step)
			(let ((cookie PORT.COOKIE))
			  (set-cookie-pos! cookie (+ ?step (cookie-pos cookie)))))))
		    (PORT.MODE
		     (identifier-syntax
		      (_			(cookie-mode PORT.COOKIE))
		      ((set! _ ?new-mode)	(set-cookie-mode! PORT.COOKIE ?new-mode))))
		    )
		 . ?body)))))))

  #| end of module |# )


;;;; cookie data structure
;;
;;An instance of  this structure is referenced by every  port structure; it registers
;;the underlying  device (if  any) and  it tracks  the underlying  device's position,
;;number of rows and columns (when possible).
;;
;;The reason the  full port data structure  is split into the PORT  structure and the
;;COOKIE structure,  is that, in some  port types, the port's  own internal functions
;;must reference some of the guts of the data structure but cannot reference the port
;;itself.  This problem shows its uglyness when TRANSCODED-PORT is applied to a port:
;;the original  port is  closed and its  guts are transferred  to the  new transcoded
;;port.  By  partitioning the fields,  we allow  the internal functions  to reference
;;only the cookie and be free of the port value.
;;
;;Why are  the fields  not all  in the cookie  then?  Because  accessing the  port is
;;faster than accessing  the cookie and many operations on  ports only require access
;;to the buffer, which is referenced by the PORT structure.
;;
;;NOTE:  It  is  impossible  to  track  the  row  number  for  ports  supporting  the
;;SET-PORT-POSITION! operation.  The  ROW-NUM field of the cookie  is meaningful only
;;for  ports  whose  position  increases  monotonically  because  of  read  or  write
;;operations,  it should  be invalidated  whenever the  port position  is moved  with
;;SET-PORT-POSITION!.  Notice  that the input  port used  to read Scheme  source code
;;satisfies this requirement.
;;

;;Constructor: (make-cookie DEST MODE POS CH-OFF ROW-NUM COL-NUM UID HASH)
;;
;;Field name: dest
;;Accessor name: (cookie-dest COOKIE)
;;  If an underlying device exists: this field holds a reference to it, for example a
;;  fixnum representing an operating system file descriptor.
;;
;;  If no device exists: this field is set to false.
;;
;;  As a special case: this field can hold  a Scheme list managed as a stack in which
;;  data is temporarily  stored.  For example: this is the  case of output bytevector
;;  ports.
;;
;;Field name: mode
;;Accessor name: (cookie-mode COOKIE)
;;Mutator name: (set-cookie-mode! COOKIE MODE-SYMBOL)
;;  Hold the symbol representing the current port mode, one among: "vicare", "r6rs".
;;
;;Field name: pos
;;Accessor name: (cookie-pos COOKIE)
;;Mutator name: (set-cookie-pos! COOKIE NEW-POS)
;;  If an  underlying device  exists: this  field holds the  current position  in the
;;  underlying device.  If no underlying device exists: this field is set to zero.
;;
;;  It is  the responsibility of  the *callers* of  the port functions  READ!, WRITE!
;;  and SET-POSITION!  to update this field.   The port's own functions READ!, WRITE!
;;  and SET-POSITION!  must not touch the cookie.
;;
;;Field name: character-offset
;;Accessor name: (cookie-character-offset COOKIE)
;;Mutator name: (set-cookie-character-offset! COOKIE NEW-CH-OFF)
;;  Zero-based offset  of the current position  in a textual input  port expressed in
;;  characters.
;;
;;Field name: row-number
;;Accessor name: (cookie-row-number COOKIE)
;;Mutator name: (set-cookie-row-number! COOKIE NEW-POS)
;;  One-based row number of the current position in a textual input port.
;;
;;Field name: column-number
;;Accessor name: (cookie-column-number COOKIE)
;;Mutator name: (set-cookie-column-number! COOKIE NEW-POS)
;;  One-based column number of the current position in a textual input port.
;;
;;Field name: uid
;;Accessor name: (cookie-uid COOKIE)
;;Mutator name: (set-cookie-uid! COOKIE GENSYM)
;;  Gensym uniquely  associated to the  port.  To be  used, for example,  to generate
;;  hash keys.  The  gensym is generated only when needed;  this field is initialised
;;  to #f.
;;
;;Field name: hash
;;Accessor name: (cookie-hash COOKIE)
;;Mutator name: (set-cookie-hash! COOKIE HASH)
;;  Hash  value  to be  used  by  hashtables.  It  should  be  generated by  applying
;;  SYMBOL-HASH to  the gensym in  the UID field.  The  hash value is  generated when
;;  needed; this field is initialised to #f.
;;
(define-struct cookie
  (dest mode pos character-offset row-number column-number uid hash))

(define (default-cookie device)
  (make-cookie device 'vicare 0 #;device-position
	       0 #;character-offset 1 #;row-number 1 #;column-number
	       #f #;uid #f #;hash))

(define (get-char-and-track-textual-position port)
  ;;Defined by Vicare.  Like GET-CHAR but track the textual position.  Recognise only
  ;;linefeed characters as line-ending.
  ;;
  (let* ((ch     (get-char port))
	 (cookie ($port-cookie port)))
    (cond ((eof-object? ch)
	   ch)
	  (($char= ch #\newline)
	   (set-cookie-character-offset! cookie (+ 1 (cookie-character-offset cookie)))
	   (set-cookie-row-number!       cookie (+ 1 (cookie-row-number       cookie)))
	   (set-cookie-column-number!    cookie 1)
	   ch)
	  (($char= ch #\tab)
	   (set-cookie-character-offset! cookie (+ 1 (cookie-character-offset cookie)))
	   (set-cookie-column-number!    cookie (+ 8 (cookie-column-number    cookie)))
	   ch)
	  (else
	   (set-cookie-character-offset! cookie (+ 1 (cookie-character-offset cookie)))
	   (set-cookie-column-number!    cookie (+ 1 (cookie-column-number    cookie)))
	   ch))))

(define* (port-textual-position {port textual-port?})
  ;;Defined by Vicare.  Given a textual  port, return the current textual position as
  ;;a vector:  0-based byte position,  0-based character offset, 1-based  row number,
  ;;1-based column number.
  ;;
  (let ((cookie ($port-cookie port)))
    (make-source-position-condition (port-id port)
				    (%port-position __who__ port)
				    (cookie-character-offset cookie)
				    (cookie-row-number       cookie)
				    (cookie-column-number    cookie))))


;;;; would block object

(define-inline (eof-or-would-block-object? ch)
  (or (eof-object?         ch)
      (would-block-object? ch)))


;;;; port's buffer size customisation

;;For ports having a Scheme bytevector as buffer: the minimum buffer size must be big
;;enough to allow the buffer to hold  the full UTF encoding of two Unicode characters
;;for all  the supported transcoders.   For ports having  a Scheme string  as buffer:
;;there is no rational constraint on the buffer size.
;;
;;It makes  sense to have "as  small as possible"  minimum buffer size to  allow easy
;;writing of test suites exercising the logic of buffer flushing and filling.
;;
(define-constant BUFFER-SIZE-LOWER-LIMIT	8)

;;The maximum buffer size must be small enough to fit into a fixnum, which is defined
;;by R6RS  to be capable  of holding  at least 24  bits.  Under Vicare:  the greatest
;;fixnum is the greatest length of strings and bytevectors.
;;
(define-constant BUFFER-SIZE-UPPER-LIMIT	(greatest-fixnum))

;;For binary  ports: the default  buffer size should  be selected to  allow efficient
;;caching of portions of binary data blobs, which may be megabytes wide.
;;
(define-constant DEFAULT-BINARY-BLOCK-SIZE	(* 4 4096))

;;For textual  ports: the default buffer  size should be selected  to allow efficient
;;caching of portions of text for the most recurring use, which includes accumulation
;;of "small" strings, like in the use of printf-like functions.
;;
(define-constant DEFAULT-STRING-BLOCK-SIZE	256)

(module (bytevector-port-buffer-size
	 string-port-buffer-size
	 input-file-buffer-size
	 output-file-buffer-size
	 input/output-file-buffer-size
	 input/output-socket-buffer-size)

  (define-syntax define-buffer-size-parameter
    (syntax-rules ()
      ((_ ?who ?init)
       (define ?who (%make-buffer-size-parameter ?init '?who)))))

  (define-syntax-rule (%valid-buffer-size? obj)
    (and (fixnum? obj)
	 (fx>=? obj BUFFER-SIZE-LOWER-LIMIT)
	 (fx<?  obj BUFFER-SIZE-UPPER-LIMIT)))

  (define %make-buffer-size-parameter
    (let ((error-message/invalid-buffer-size
	   (string-append "expected fixnum in range "
			  (number->string BUFFER-SIZE-LOWER-LIMIT)
			  " <= x < "
			  (number->string BUFFER-SIZE-UPPER-LIMIT)
			  " as buffer size")))
      (lambda (init who)
	(make-parameter init
	  (lambda (obj)
	    (if (%valid-buffer-size? obj)
		obj
	      (error who error-message/invalid-buffer-size obj)))))))

;;; --------------------------------------------------------------------

  ;;Customisable buffer size for bytevector ports.  To be used by:
  ;;
  ;;   OPEN-BYTEVECTOR-OUTPUT-PORT
  ;;
  ;;and similar.
  ;;
  (define-buffer-size-parameter bytevector-port-buffer-size	DEFAULT-BINARY-BLOCK-SIZE)

  ;;Customisable buffer size for string ports.  To be used by:
  ;;
  ;;   OPEN-STRING-OUTPUT-PORT
  ;;
  ;;and similar.
  ;;
  (define-buffer-size-parameter string-port-buffer-size		DEFAULT-STRING-BLOCK-SIZE)

  ;;Customisable buffer size for file ports.  To be used by:
  ;;
  ;;  OPEN-FILE-INPUT-PORT
  ;;  OPEN-FILE-OUTPUT-PORT
  ;;
  ;;and similar.
  ;;
  (define-buffer-size-parameter input-file-buffer-size		DEFAULT-BINARY-BLOCK-SIZE)
  (define-buffer-size-parameter output-file-buffer-size		DEFAULT-BINARY-BLOCK-SIZE)
  (define-buffer-size-parameter input/output-file-buffer-size	DEFAULT-BINARY-BLOCK-SIZE)

  ;;Customisable buffer size for socket ports.
  ;;
  (define-buffer-size-parameter input/output-socket-buffer-size	DEFAULT-BINARY-BLOCK-SIZE)

  #| end of module |# )


;;;; buffer mode

(define* (output-port-buffer-mode {port output-port?})
  ;;Defined by R6RS.  Return the symbol that represents the buffer mode of PORT.
  ;;
  (with-port (port)
    (cond (port.buffer-mode-line?	'line)
	  (port.buffer-mode-none?	'none)
	  (else				'block))))

(define* (set-port-buffer-mode! {port port?} mode)
  ;;Defined by Vicare.  Reset the port buffer mode.
  ;;
  (with-port (port)
    (case mode
      ((line)
       (if ($textual-port? port)
	   (set! port.attributes ($fxior (%port-nullify-eol-style-bits port.attributes)
					 BUFFER-MODE-LINE-TAG))
	 (procedure-signature-argument-violation __who__
	   "expected textual port if buffer mode is \"line\"" 1 '(textual-port? port) port)))
      ((none)
       (set! port.attributes   ($fxior (%port-nullify-eol-style-bits port.attributes)
				       BUFFER-MODE-NONE-TAG)))
      ((block)
       (set! port.attributes (%port-nullify-eol-style-bits port.attributes)))
      (else
       (procedure-signature-argument-violation __who__
	 "failed argument validation" 2 '(port-buffer-mode? mode) mode)))))


;;;; guarded ports
;;
;;It  happens that  platforms  have a  maximum  limit on  the  number of  descriptors
;;allocated to  a process.   When a  port wrapping a  platform descriptor  is garbage
;;collected: we  want to close  it (if it  has not been  closed already) to  free its
;;descriptor.  We do it using a guardian and the POST-GC-HOOK.
;;
;;It is also a good idea to close custom ports.  We do it using the same technique.
;;

(module (%port->maybe-guarded-port)

  (define* (%port->maybe-guarded-port {port port?})
    ;;Accept a  port as argument  and return  the port itself.   If the port  wraps a
    ;;platform descriptor: register it in the guardian.
    ;;
    (when ($guarded-port? port)
      (port-guardian port))
    port)

  (define port-guardian
    (make-guardian))

  (define (%close-garbage-collected-ports)
    (let ((port (port-guardian)))
      (when port
	;;Notice  that, as  defined  by R6RS,  CLOSE-PORT does  nothing  if PORT  has
	;;already been closed.
	(close-port port)

	;;The code below differs from a call  to CLOSE-PORT because it does not flush
	;;the buffer.
	;;
	;; (with-port (port)
	;;   (unless port.closed?
	;;     (port.mark-as-closed!)
	;;     (when (procedure? port.close)
	;;       (port.close))))

	(%close-garbage-collected-ports))))

  (post-gc-hooks (cons %close-garbage-collected-ports (post-gc-hooks)))

  #| end of module |# )


;;;; external modules

(include "ikarus.io.port-attributes.scm"	#t)
(include "ikarus.io.port-constructors.scm"	#t)
(include "ikarus.io.port-predicates.scm"	#t)
(include "ikarus.io.port-procedures.scm"	#t)
(include "ikarus.io.reading.scm"		#t)
(include "ikarus.io.writing.scm"		#t)
(include "ikarus.io.file-descriptor-ports.scm"	#t)


;;;; done

;; (define end-of-file-dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.io end")))

#| end of library |# )

;;; end of file
;;; Local Variables:
;;; eval: (put 'case-errno				'scheme-indent-function 1)
;;; eval: (put 'with-port				'scheme-indent-function 1)
;;; eval: (put 'with-port-having-bytevector-buffer	'scheme-indent-function 1)
;;; eval: (put 'with-port-having-string-buffer		'scheme-indent-function 1)
;;; eval: (put '%implementation-violation		'scheme-indent-function 1)
;;; eval: (put '%case-eol-style				'scheme-indent-function 1)
;;; eval: (put '%parse-byte-order-mark			'scheme-indent-function 1)
;;; eval: (put '%parse-bom-and-add-fast-tag		'scheme-indent-function 1)
;;; eval: (put '%flush-bytevector-buffer-and-evaluate	'scheme-indent-function 1)
;;; eval: (put '%flush-string-buffer-and-evaluate	'scheme-indent-function 1)
;;; eval: (put 'refill-bytevector-buffer-and-evaluate		'scheme-indent-function 1)
;;; eval: (put 'maybe-refill-bytevector-buffer-and-evaluate	'scheme-indent-function 1)
;;; eval: (put 'refill-string-buffer-and-evaluate		'scheme-indent-function 1)
;;; eval: (put 'maybe-refill-string-buffer-and-evaluate		'scheme-indent-function 1)
;;; End:
