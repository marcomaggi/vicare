;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;
;;;Abstract
;;;
;;;	Define and  export almost all the  I/O functions mandated
;;;	by R6RS plus some implementation-specific functions.
;;;
;;;	The functions and macros  prefixed with "%" and "unsafe."
;;;	are  not  exported.  The  functions  and macros  prefixed
;;;	"unsafe." or  "%unsafe."  assume that  the arguments have
;;;	already been validated.  The functions prefixed "unsafe."
;;;	are imported from another library.
;;;
;;;	NOTE The primitive operations on a port value are defined
;;;	in "pass-specify-rep-primops.ss"; a  port value is just a
;;;	block of memory whose first  word is tagged with the port
;;;	tag.
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006,2007,2008  Abdulaziz Ghuloum
;;;
;;;This program is free  software: you can redistribute it and/or
;;;modify it  under the terms  of the GNU General  Public License
;;;version 3 as published by the Free Software Foundation.
;;;
;;;This  program is  distributed  in  the hope  that  it will  be
;;;useful,  but WITHOUT  ANY WARRANTY;  without even  the implied
;;;warranty  of  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR
;;;PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You  should have  received a  copy of  the GNU  General Public
;;;License    along   with   this    program.    If    not,   see
;;;<http://www.gnu.org/licenses/>.
;;;


;;;; To do list
;;
;;* Solve all the FIXME issues.
;;
;;* Write tests for all the untested functions.
;;
;;*   Test  the  transcoders   with  OPEN-BYTEVECTOR-OUTPUT-PORT,
;;especially the SET-PORT-POSITION! function.
;;
;;*  FIXME If SET-PORT-POSITION!   fails it  is possible  for the
;;field POS of the cookie to become invalid.  This situation must
;;be detected by all the functions, currently it is not.
;;
;;* Write documentation for the Ikarus-specific functions.
;;
;;* When a decoding error  occurs from an input port: R6RS states
;;that an "appropriate" number of  input bytes must be skipped in
;;search for the next character.  Implement this by searching for
;;the  next character beginning  discarding the  minimum possible
;;number of bytes, for example in UTF-8 discard at most 3 bytes.
;;
;;* Implement missing R6RS functions.
;;
;;*  Exceptions  raised by  SET-PORT-POSITION!  must be  reviewed
;;because they do not comply with R6RS.
;;
;;* R6RS states the following about SET-PORT-POSITION!:
;;
;;  If PORT is  a binary output port and  the current position is
;;  set beyond the current end of the data in the underlying data
;;  sink, the object is not extended until new data is written at
;;  that position.  The contents of any intervening positions are
;;  unspecified.   Binary ports created  by OPEN-FILE-OUTPUT-PORT
;;  and  OPEN-FILE-INPUT/OUTPUT-PORT can  always  be extended  in
;;  this  manner within  the limits  of the  underlying operating
;;  system.  In other cases, attempts  to set the port beyond the
;;  current end of data in the underlying object may result in an
;;  exception with condition type &i/o-invalid-position.
;;
;;* Change  OPEN-STRING-INPUT-PORT and OPEN-BYTEVECTOR-INPUT-PORT
;;to use the  string or buffer itself as buffer  when it is short
;;enough, and to use a separate buffer otherwise.
;;


;;;; The port data structure
;;
;;It  is  defined   in  "pass-specify-rep-primops.ss";  its  allocation,
;;accessors and mutators are all defined as primitive operations inlined
;;by the compiler.
;;
;;Constructor: $make-port ATTRS IDX SZ BUF TR ID READ WRITE GETP SETP CL COOKIE
;;Aguments: IDX		- index in input/output buffer,
;;	    SZ		- number of bytes/chars used in the input/output buffer,
;;          BUF		- input/output buffer,
;;          TR		- transcoder
;;          ID		- an object describing the underlying device
;;          READ	- read procedure
;;          WRITE	- write procedure
;;          GETP	- get-position procedure
;;          SETP	- set-position procedure
;;          CL		- close procedure
;;          COOKIE	- line and column tracker
;;  Build a new port structure and return a Scheme value referencing it.
;;  Notice  that the  underlying  device is  not  among the  constructor
;;  arguments: it is implicitly referenced by the functions READ, WRITE,
;;  GETP, SETP, CL.
;;
;;
;;Fields of a port block
;;----------------------
;;
;;Field name: tag
;;Field accessor: $port-tag PORT
;;
;;Field name: index
;;Field accessor: $port-index PORT
;;Field Mutator: $set-port-index! PORT IDX
;;  Zero-based fixnum offset of the  current position in the buffer; see
;;  the description of the BUFFER field below.
;;
;;  For an  output port: it  is the offset  in the output buffer  of the
;;  next location to be written to.
;;
;;  For an input port: it is the  offset of the input buffer of the next
;;  location to be read from.
;;
;;Field name: size
;;Field accessor: $port-size PORT
;;Field mutator: $set-port-size! PORT SIZE
;;  Fixnum representing the number  of bytes/chars currently used in the
;;  input/output buffer; see the description of the BUFFER field below.
;;
;;  When the device is a Scheme  string or bytevector: this field is set
;;  to the number of characters in  the string or the number of bytes in
;;  the bytevector.
;;
;;Field name: buffer
;;Field accessor: $port-buffer PORT
;;  The input/output  buffer for the  port.  The buffer is  allocated at
;;  port  construction time  and  never reallocated.   The  size of  the
;;  buffers is customisable through a set of parameters.
;;
;;  For the  logic of the functions to  work: it is mandatory  to have a
;;  buffer at  least wide  enough to hold  the largest  UTF-8 character.
;;  This is because  it is possible to put a transcoder  on top of every
;;  binary port, and we need a  place to store partially read or written
;;  characters.
;;
;;  Custom  binary ports  have a  bytevector as  buffer;  custom textual
;;  ports have a string as buffer.
;;
;;  As  special cases: the  port returned  by OPEN-BYTEVECTOR-INPUT-PORT
;;  has  the   bytevector  itself  as  buffer,  the   port  returned  by
;;  OPEN-STRING-INPUT-PORT has the string itself as buffer.
;;
;;  The port returned by OPEN-BYTEVECTOR-OUTPUT-PORT has a bytevector as
;;  buffer; the port returned by OPEN-STRING-OUTPUT-PORT has a string as
;;  buffer.
;;
;;  When the port  has a platform file as  underlying device: the buffer
;;  is a Scheme bytevector.
;;
;;  When doing output on an  underlying device: data is first written in
;;  the buffer and  once in a while flushed to  the device.  The current
;;  output port position is computed with:
;;
;;	   port position = device position + index
;;
;;                 device position
;;                        v                        device
;;	   |--------------+---------------------------|
;;                        |*****+*******+--------| buffer
;;                        ^     ^       ^        ^
;;                        0   index  used-size  size
;;
;;  When doing input  on an underlying device: a block  of data is first
;;  copied  from the device  into the  buffer and  then read  to produce
;;  Scheme values.  The current input port position is computed with:
;;
;;	   port position = device position - used-size + index
;;	                 = device position - (used-size - index)
;;
;;                               device position
;;                                      v           device
;;	   |----------------------------+-------------|
;;                      |*******+*******+--------| buffer
;;                      ^       ^       ^        ^
;;                      0     index  used-size  size
;;
;;Field name: transcoder
;;
;;Field name: id
;;Field accessor: $port-id
;;  An object  describing the underlying device.  For  a port associated
;;  to a file: it is a Scheme string representing the file name given to
;;  functions like OPEN-OUTPUT-FILE.
;;
;;Field name: read!
;;Field accessor: $port-read! PORT
;;  Fill buffer procedure.
;;
;;  When the value is a procedure:  it must be a function which, applied
;;  to  the port  itself,  fills the  input  buffer with  data from  the
;;  underlying device.
;;
;;  When the value is the Scheme symbol ALL-DATA-IN-BUFFER: the port has
;;  no underlying device, the buffer itself is the device.
;;
;;Field name: write!
;;Field accessor: $port-write! PORT
;;  Write butter procedure.
;;
;;Field name: get-position
;;Field accessor: $port-get-position PORT
;;  Get position policy: it is  used to retrieve the current position in
;;  the underlying  device.  The device  position is tracked by  the POS
;;  field  of  the  port's  cookie.   The  current  device  position  is
;;  different from the  current port position: see the  BUFFER field for
;;  details.
;;
;;  This field can  be set to #f, #t or a  procedure taking no arguments
;;  and returning the current device position.
;;
;;  - The  value is #f when  the underlying device has  no position.  In
;;  this case the port does not support the GET-POSITION operation.
;;
;;  -  The value is  #t for  ports in  which the  cookie's POS  field is
;;  successfully used to track the device position; this is the case for
;;  all the non-custom ports instantiated by this library.  Custom ports
;;  cannot have this field set to #t.
;;
;;  As a  special case the ports  returned by OPEN-BYTEVECTOR-INPUT-PORT
;;  and OPEN-STRING-INPUT-PORT  use the  bytevector or string  itself as
;;  buffer; in  such cases: the POS  field of the  cookie is perpetually
;;  set  to the  buffer=device size,  so  the port  position equals  the
;;  device position which equals the current buffer index.
;;
;;  - The value is a procedure when the underlying device has a position
;;  which cannot be tracked by the cookie's POS field.  This is the case
;;  for all the custom ports having a device.
;;
;;Field name: set-position!
;;Field accessor: $port-set-position! PORT
;;  Set position policy:  it is used to set the  current position in the
;;  underlying device.  The device position  is tracked by the POS field
;;  of the port's cookie.  The current device position is different from
;;  the current port position: see the BUFFER field for details.
;;
;;  This field can be set to #f, #t or a procedure taking the new device
;;  position as argument and returning unspecified values.
;;
;;  - The  value is #f when  the underlying device has  no position.  In
;;  this case the port does not support the SET-POSITION! operation.
;;
;;  -  The  value is  #t  when  the cookie's  POS  field  holds a  value
;;  representing a correct and  immutable device position.  In this case
;;  the current  port position can be  moved only by  moving the current
;;  buffer index.
;;
;;  For  example the  ports returned  by  OPEN-BYTEVECTOR-INPUT-PORT and
;;  OPEN-STRING-INPUT-PORT have the  buffer itself as underlying device;
;;  for these ports:  the POS field of the cookie  is perpetually set to
;;  the buffer=device size.
;;
;;  NOTE     At     present    only     the     ports    returned     by
;;  OPEN-BYTEVECTOR-INPUT-PORT  and   OPEN-STRING-INPUT-PORT  have  this
;;  policy,  so SET-PORT-POSITION!  is  optimised for  this case  (Marco
;;  Maggi; Sep 21, 2011).
;;
;;  -  The  value  is a  procedure  when  the  underlying device  has  a
;;  position.
;;
;;Field name: close
;;Field accessor: $port-close PORT
;;  Close device procedure.
;;
;;Field name: cookie
;;Field accessor: $port-cookie PORT
;;Field mutator: $set-port-cookie PORT COOKIE
;;  A COOKIE  record used to keep  a reference to  the underlying device
;;  and track its current position.   Additionally it can track line and
;;  column numbers in textual ports.
;;


;;;; On buffering
;;
;;We establish the following constraints:
;;
;;*  If an  exact  integer is  in  the range  representable by  a
;;fixnum, Vicare will represent it as a fixnum.
;;
;;* No  matter which BUFFER-MODE  was selected, every port  has a
;;buffer.
;;
;;* The  buffer is a Scheme  bytevector or a  Scheme string whose
;;length is representable by a fixnum.
;;
;;* The input functions always read data from the buffer first.
;;
;;* The output functions always write data to the buffer first.
;;
;;*   %UNSAFE.REFILL-INPUT-PORT-BYTEVECTOR-BUFFER  is   the  only
;;function calling the port's  READ!  function, copying data from
;;the underlying device to the input buffer.
;;
;;*  %UNSAFE.FLUSH-OUTPUT-PORT is the  only function  calling the
;;port's WRITE! function, copying  data from the output buffer to
;;the underlying device.
;;
;; --------------------------------------------------------------
;;
;;From the constraints it follows that:
;;
;;*  OPEN-STRING-INPUT-PORT  and OPEN-BYTEVECTOR-INPUT-PORT  will
;;refuse  to create a  port to  read characters  or bytes  from a
;;string or  bytevector whose length exceeds the  return value of
;;GREATEST-FIXNUM.
;;
;;*  All the arithmetics  involving the  buffer can  be performed
;;using unsafe fixnum functions.
;;
;; --------------------------------------------------------------
;;
;;Buffer mode handling is as follows:
;;
;;* When the buffering mode is NONE: data is first written to the
;;output buffer, then immediately sent to the underlying device.
;;
;;* When the buffering mode is LINE: data is first written to the
;;output buffer up to the first newline, then immediately sent to
;;the underlying device.
;;
;;* When  the buffering mode is  BLOCK: data is  first written to
;;the output buffer.  Only when  the buffer is full: data is sent
;;to the underlying device.
;;
;; --------------------------------------------------------------
;;
;;Things to notice:
;;
;;* The exact integer representing the current position of a port
;;in the underlying device can be either a fixnum or a bignum.
;;


(library (ikarus.io)
  (export
    ;; port parameters
    standard-input-port standard-output-port standard-error-port
    current-input-port  current-output-port  current-error-port
    console-output-port console-error-port   console-input-port
    bytevector-port-buffer-size string-port-buffer-size
    input-file-buffer-size	output-file-buffer-size

    ;; predicates
    port? input-port? output-port? textual-port? binary-port?
    port-eof?

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

    ;; custom ports
    make-custom-binary-input-port
    make-custom-binary-output-port
    make-custom-textual-input-port
    make-custom-textual-output-port

    ;; transcoders
    transcoded-port port-transcoder

    ;; closing ports
    close-port port-closed? close-input-port close-output-port

    ;; port position
    port-position port-has-port-position?
    set-port-position! port-has-set-port-position!?
    input-port-byte-position
    input-port-column-number input-port-row-number

    ;; reading chars
    get-char lookahead-char read-char peek-char

    ;; reading strings
    get-string-n get-string-n! get-string-all get-line read-line

    ;; reading bytes
    get-u8 lookahead-u8

    ;; reading bytevectors
    get-bytevector-n get-bytevector-n!
    get-bytevector-some get-bytevector-all

    ;; writing bytes and bytevectors
    put-u8 put-bytevector

    ;; writing chars and strings
    put-char write-char put-string newline

    ;; port configuration
    port-mode set-port-mode!
    output-port-buffer-mode
    reset-input-port!
    reset-output-port!
    port-id

    ;; spawning operative system processes
    process process-nonblocking process*

    ;; networking
    tcp-connect tcp-connect-nonblocking
    udp-connect udp-connect-nonblocking
    tcp-server-socket tcp-server-socket-nonblocking
    accept-connection accept-connection-nonblocking
    close-tcp-server-socket
    register-callback
    input-socket-buffer-size output-socket-buffer-size

    ;; directory inspection
    open-directory-stream directory-stream?
    read-directory-stream close-directory-stream)
  (import (except (ikarus)
		  ;; port parameters
		  standard-input-port standard-output-port standard-error-port
		  current-input-port  current-output-port  current-error-port
		  console-output-port console-error-port   console-input-port
		  bytevector-port-buffer-size	string-port-buffer-size
		  input-file-buffer-size	output-file-buffer-size

		  ;; predicates
		  port? input-port? output-port? textual-port? binary-port?
		  port-eof?

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

		  ;; custom ports
		  make-custom-binary-input-port
		  make-custom-binary-output-port
		  make-custom-textual-input-port
		  make-custom-textual-output-port

		  ;; transcoders
		  transcoded-port port-transcoder

		  ;; closing ports
		  close-port port-closed? close-input-port close-output-port

		  ;; port position
		  port-position port-has-port-position?
		  set-port-position! port-has-set-port-position!?
		  input-port-byte-position
		  input-port-column-number input-port-row-number

		  ;; reading chars
		  get-char lookahead-char read-char peek-char

		  ;; reading strings
		  get-string-n get-string-n! get-string-all get-line read-line

		  ;; reading bytes
		  get-u8 lookahead-u8

		  ;; reading bytevectors
		  get-bytevector-n get-bytevector-n!
		  get-bytevector-some get-bytevector-all

		  ;; writing bytes and bytevectors
		  put-u8 put-bytevector

		  ;; writing chars and strings
		  put-char write-char put-string newline

		  ;; port configuration
		  port-mode set-port-mode!
		  output-port-buffer-mode
		  reset-input-port!
		  reset-output-port!
		  port-id

		  ;; spawning operative system processes
		  process process-nonblocking process*

		  ;; networking
		  tcp-connect tcp-connect-nonblocking
		  udp-connect udp-connect-nonblocking
		  tcp-server-socket tcp-server-socket-nonblocking
		  accept-connection accept-connection-nonblocking
		  close-tcp-server-socket
		  register-callback
		  input-socket-buffer-size output-socket-buffer-size

		  ;; directory inspection
		  open-directory-stream directory-stream?
		  read-directory-stream close-directory-stream)
    ;;This  internal library  is the  one  exporting: $MAKE-PORT,
    ;;$PORT-* and $SET-PORT-* bindings.
    (ikarus system $io)
    (prefix (only (ikarus) port?) primop.)
    (prefix (rename (ikarus system $fx)
		    ($fxzero?	fxzero?)
		    ($fxadd1	fxadd1)	 ;increment
		    ($fxsub1	fxsub1)	 ;decrement
		    ($fxsra	fxsra)	 ;shift right
		    ($fxsll	fxsll)	 ;shift left
		    ($fxlogor	fxlogor) ;inclusive logic OR
		    ($fxlogand	fxand)	 ;logic AND
		    ($fx+	fx+)
		    ($fx-	fx-)
		    ($fx<	fx<)
		    ($fx>	fx>)
		    ($fx>=	fx>=)
		    ($fx<=	fx<=)
		    ($fx=	fx=))
	    unsafe.)
    (prefix (rename (ikarus system $chars)
		    ($char->fixnum	char->integer)
		    ($fixnum->char	integer->char))
	    unsafe.)
    (prefix (rename (ikarus system $bytevectors)
		    ($make-bytevector	make-bytevector)
		    ($bytevector-length	bytevector-length)
		    ($bytevector-set!	bytevector-u8-set!)
		    ($bytevector-u8-ref	bytevector-u8-ref))
	    unsafe.)
    (prefix (rename (ikarus system $strings)
		    ($make-string	make-string)
		    ($string-length	string-length)
		    ($string-ref	string-ref)
		    ($string-set!	string-set!))
	    unsafe.))


;;;; emergency debugging

(define (emergency-platform-write-fd str)
  ;;Interface to  "write()".  In case something  goes wrong while
  ;;modifying  the code  in  this  library, it  may  be that  the
  ;;compiled image fails to  write understandable messages to the
  ;;standard ports  using the R6RS functions.   This macro allows
  ;;direct interface to the platform's stderr.
  ;;
  (let ((bv (string->utf8 str)))
    (foreign-call "ikrt_write_fd" 2 bv 0 (unsafe.bytevector-length bv))
    ;;and a newline
    (foreign-call "ikrt_write_fd" 2 '#vu8(10) 0 1)))


;;;; port tags
;;
;;All the tags have 13 bits.
;;
;;The 12th and 13th bits are used only by CLOSED-PORT-TAG and the
;;commented out  R6RS-MODE-TAG, which  are not associated  to the
;;type of a port; for this  reason if we want to extract only the
;;type bits  we have to use  a mask with 11  bits set to  1 as in
;;FAST-ATTRS-MASK.
;;
;;                                32109876543210
(define input-port-tag		#b00000000000001)
(define output-port-tag		#b00000000000010)
(define textual-port-tag	#b00000000000100)
(define binary-port-tag		#b00000000001000)
(define fast-char-text-tag	#b00000000010000)
(define fast-u7-text-tag	#b00000000100000)
(define fast-u8-text-tag	#b00000001100000)
(define fast-u16be-text-tag	#b00000010000000)
(define fast-u16le-text-tag	#b00000100000000)
(define init-u16-text-tag	#b00000110000000)
;;(define r6rs-mode-tag		#b01000000000000)
(define closed-port-tag		#b10000000000000)

;;If we  are just interested in  the port type:  input or output,
;;binary or textual, we can do:
;;
;;  (let ((type-bits (unsafe.fxand ($port-attrs port) port-type-mask)))
;;    (unsafe.fx= type-bits *-port-bits))
;;
;;where *-PORT-BITS  is one of the constants  below.  Notice that
;;this predicate is  not influenced by the fact  that the port is
;;closed.
;;
(define port-type-mask			#b00000000001111)
(define binary-input-port-bits		(%unsafe.fxior binary-port-tag  input-port-tag))
(define binary-output-port-bits		(%unsafe.fxior binary-port-tag  output-port-tag))
(define textual-input-port-bits		(%unsafe.fxior textual-port-tag input-port-tag))
(define textual-output-port-bits	(%unsafe.fxior textual-port-tag output-port-tag))

;;The following  tag constants allow fast  classification of open
;;input ports by doing:
;;
;;  (let ((tag ($port-fast-attrs port)))
;;    (unsafe.fx= tag fast-get-*-tag))
;;
;;where  FAST-GET-*-TAG is  one of  the constants  below.  Notice
;;that if the  port is closed the predicate  will succeed because
;;$PORT-FAST-ATTRS excludes the closed? flag.
;;
;;This one  is used for binary  input ports from  which raw bytes
;;must be read.
(define fast-get-byte-tag        binary-input-port-bits)
;;
;;The  following are  used  for textual  input  ports from  which
;;characters in  some encoding must  be read.  Notice  that latin
;;encoding is also recognised as UTF-8 encoding.
(define fast-get-char-tag	(%unsafe.fxior fast-char-text-tag  textual-input-port-bits))
(define fast-get-utf8-tag	(%unsafe.fxior fast-u7-text-tag    textual-input-port-bits))
(define fast-get-latin-tag	(%unsafe.fxior fast-u8-text-tag    textual-input-port-bits))
(define fast-get-utf16be-tag	(%unsafe.fxior fast-u16be-text-tag textual-input-port-bits))
(define fast-get-utf16le-tag	(%unsafe.fxior fast-u16le-text-tag textual-input-port-bits))

;;The following tag constants allow fast classification of output
;;ports by doing:
;;
;;  (let ((tag ($port-fast-attrs port)))
;;    (unsafe.fx= tag fast-put-*-tag))
;;
;;where  FAST-PUT-*-TAG is  one of  the constants  below.  Notice
;;that if the  port is closed the predicate  will succeed because
;;$PORT-FAST-ATTRS excludes the closed? flag.
;;
;;This one  is used  for binary output  ports to which  raw bytes
;;must be written.
(define fast-put-byte-tag	binary-output-port-bits)
;;
;;The  following  are used  for  textual  output  ports to  which
;;characters in some encoding must be written.  Notice that latin
;;encoding is also recognised as UTF-8 encoding.
(define fast-put-char-tag	(%unsafe.fxior fast-char-text-tag  textual-output-port-bits))
(define fast-put-utf8-tag	(%unsafe.fxior fast-u7-text-tag    textual-output-port-bits))
(define fast-put-latin-tag	(%unsafe.fxior fast-u8-text-tag    textual-output-port-bits))
(define fast-put-utf16be-tag	(%unsafe.fxior fast-u16be-text-tag textual-output-port-bits))
(define fast-put-utf16le-tag	(%unsafe.fxior fast-u16le-text-tag textual-output-port-bits))
(define init-put-utf16-tag	(%unsafe.fxior fast-put-utf16be-tag
					       fast-put-utf16le-tag))

;;; --------------------------------------------------------------------

;;                                 32109876543210
(define fast-attrs-mask          #b00111111111111)

(define-syntax $port-fast-attrs
  ;;Extract the type bits from the tag of a port X.
  ;;
  (identifier-syntax (lambda (x)
		       (unsafe.fxand ($port-tag x) fast-attrs-mask))))

(define-syntax case-textual-input-port-fast-tag
  (syntax-rules ( ;;
		 fast-get-utf8-tag fast-get-char-tag fast-get-latin-tag
		 fast-get-utf16le-tag fast-get-utf16be-tag else)
    ((case-textual-input-port-fast-tag ?port
       ((fast-get-utf8-tag)		. ?utf8-tag-body)
       ((fast-get-char-tag)		. ?char-tag-body)
       ((fast-get-latin-tag)		. ?latin-tag-body)
       ((fast-get-utf16le-tag)		. ?utf16le-tag-body)
       ((fast-get-utf16be-tag)		. ?utf16be-tag-body)
       (else				. ?else-body))
     (let ((m ($port-fast-attrs ?port)))
       (cond ((eq? m fast-get-utf8-tag)		. ?utf8-tag-body)
	     ((eq? m fast-get-char-tag)		. ?char-tag-body)
	     ((eq? m fast-get-latin-tag)	. ?latin-tag-body)
	     ((eq? m fast-get-utf16le-tag)	. ?utf16le-tag-body)
	     ((eq? m fast-get-utf16be-tag)	. ?utf16be-tag-body)
	     (else				. ?else-body))))))

;;; --------------------------------------------------------------------
;;; Backup of original Ikarus values

;;(define port-type-mask		#b00000000001111)
;;(define binary-input-port-bits	#b00000000001001)
;;(define binary-output-port-bits	#b00000000001010)
;;(define textual-input-port-bits	#b00000000000101)
;;(define textual-output-port-bits	#b00000000000110)

;;(define fast-get-byte-tag		#b00000000001001)
;;(define fast-get-char-tag		#b00000000010101)
;;(define fast-get-utf8-tag		#b00000000100101)
;;(define fast-get-latin-tag		#b00000001100101)
;;(define fast-get-utf16be-tag		#b00000010000101)
;;(define fast-get-utf16le-tag		#b00000100000101)

;;(define fast-put-byte-tag		#b00000000001010)
;;(define fast-put-char-tag		#b00000000010110)
;;(define fast-put-utf8-tag		#b00000000100110)
;;(define fast-put-latin-tag		#b00000001100110)
;;(define fast-put-utf16be-tag		#b00000010000110)
;;(define fast-put-utf16le-tag		#b00000100000110)
;;(define init-put-utf16-tag		#b00000110000110)


;;;; syntax helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))

(define-syntax define-alias
  (syntax-rules ()
    ((_ ?alias ?subst)
     (define-syntax ?alias
       (identifier-syntax ?subst)))))


;;;; assertion helpers

(define-syntax %debug-assert
  ;;This is meant  to expand to nothing when  debugging is turned
  ;;off.
  ;;
  ;;NOTE It should be better to define it as:
  ;;
  ;; (if #t
  ;;     (syntax-rules ()
  ;;       ((_ ?pred . ?continuation)
  ;;        (begin (assert ?pred) . ?continuation)))
  ;;   (syntax-rules ()
  ;;     ((_ ?pred . ?continuation)
  ;;      (begin . ?continuation))))
  ;;
  ;;but it would increase the indentation level.
  ;;
  (if #t
      (syntax-rules ()
  	((_ ?pred)
  	 (assert ?pred)))
    (syntax-rules ()
      ((_ ?pred)
       (values)))))

(define-inline (%assert-value-is-port ?port ?who)
  (unless (port? ?port)
    (assertion-violation ?who "not a port" ?port)))

;;; --------------------------------------------------------------------

(define-inline (%assert-value-is-input-port ?port ?who)
  (unless (input-port? ?port)
    (assertion-violation ?who "not an input port" ?port)))

(define-inline (%unsafe.assert-value-is-input-port ?port ?who)
  (unless (%unsafe.input-port? ?port)
    (assertion-violation ?who "not an input port" ?port)))

;;; --------------------------------------------------------------------

(define-inline (%assert-value-is-output-port ?port ?who)
  (unless (output-port? ?port)
    (assertion-violation ?who "not an output port" ?port)))

(define-inline (%unsafe.assert-value-is-output-port ?port ?who)
  (unless ($unsafe.output-port? ?port)
    (assertion-violation ?who "not an output port" ?port)))

;;; --------------------------------------------------------------------

(define-inline (%unsafe.assert-value-is-textual-port ?port ?who)
  (unless (%unsafe.textual-port? ?port)
    (assertion-violation ?who "not a textual port" ?port)))

;;; --------------------------------------------------------------------

(define-inline (%assert-value-is-open-port ?port ?who)
  (when (port-closed? ?port)
    (assertion-violation ?who "port is closed" ?port)))

(define-inline (%unsafe.assert-value-is-open-port ?port ?who)
  (when (%unsafe.port-closed? ?port)
    (assertion-violation ?who "port is closed" ?port)))

;;; --------------------------------------------------------------------

(define-inline (%assert-value-is-port-position position who)
  (unless (and (or (fixnum? position)
		   (bignum? position))
	       (>= position 0))
    (assertion-violation who "position must be a nonnegative exact integer" position)))

(define-inline (%assert-value-is-get-position-result ?position ?port ?who)
  (unless (and (or (fixnum? ?position)
		   (bignum? ?position))
	       (>= ?position 0))
    (die ?who "invalid value returned by get-position" ?port ?position)))

(define-inline (%assert-value-is-transcoder ?transcoder ?who)
  (unless (transcoder? ?transcoder)
    (assertion-violation ?who "not a transcoder" ?transcoder)))

(define-inline (%assert-value-is-maybe-transcoder ?maybe-transcoder ?who)
  (when (and ?maybe-transcoder (not (transcoder? ?maybe-transcoder)))
    (assertion-violation ?who
      "expected false or a transcoder object as optional transcoder argument"
      ?maybe-transcoder)))

;;; --------------------------------------------------------------------

(define-inline (%assert-value-is-bytevector ?obj ?who)
  (unless (bytevector? ?obj)
    (assertion-violation ?who "not a bytevector" ?obj)))

(define-inline (%assert-value-is-procedure ?proc ?who)
  (unless (procedure? ?proc)
    (assertion-violation ?who "not a procedure" ?proc)))

(define-inline (%assert-value-is-port-identifier ?identifier ?who)
  (unless (string? ?identifier)
    (assertion-violation ?who "ID is not a string" ?identifier)))

(define-inline (%assert-value-is-read!-procedure ?proc ?who)
  (unless (procedure? ?proc)
    (assertion-violation ?who "READ! is not a procedure" ?proc)))

(define-inline (%assert-value-is-write!-procedure ?proc ?who)
  (unless (procedure? ?proc)
    (assertion-violation ?who "WRITE! is not a procedure" ?proc)))

(define-inline (%assert-value-is-maybe-close-procedure ?proc ?who)
  (unless (or (procedure? ?proc) (not ?proc))
    (assertion-violation ?who "CLOSE should be either a procedure or false" ?proc)))

(define-inline (%assert-value-is-maybe-get-position-procedure ?proc ?who)
  (unless (or (procedure? ?proc) (not ?proc))
    (assertion-violation ?who "GET-POSITION should be either a procedure or false" ?proc)))

(define-inline (%assert-value-is-maybe-set-position!-procedure ?proc ?who)
  (unless (or (procedure? ?proc) (not ?proc))
    (assertion-violation ?who "SET-POSITION! should be either a procedure or false" ?proc)))


;;;; error helpers

(define (%raise-port-position-out-of-range who port new-position)
  (raise (condition
	  (make-who-condition who)
	  (make-message-condition "attempt to set port position beyond limit")
	  (make-i/o-invalid-position-error new-position)
	  (make-irritants-condition (list port)))))


;;;; generic helpers

;;ALL-DATA-IN-BUFFER is  used in place  of the READ!  procedure  to mark
;;ports whose  buffer is  all the data  there is,  that is: there  is no
;;underlying device.
;;
(define all-data-in-buffer
  'all-data-in-buffer)

(define newline-integer
  (unsafe.char->integer #\newline))

(define-syntax %unsafe.fxior
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     (unsafe.fxlogor ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     (unsafe.fxlogor ?op1 (%unsafe.fxior ?op2 . ?ops)))))

(define-syntax %u8?
  ;;Evaluate to true if the  argument is a fixnum representing an
  ;;unsigned byte.
  ;;
  (syntax-rules ()
    ((_ x)
     (and (fixnum? x)
	  (unsafe.fx>= x 0)
	  (unsafe.fx<= x 255)))))

(define-syntax %the-true-value?
  ;;Evaluate to  true if the object  is the actual  #t value, not
  ;;just true according to Scheme semantics.
  ;;
  (syntax-rules ()
    ((_ ?obj)
     (eqv? #t ?obj))))

(define-inline (%list-holding-single-value? ell)
  (and (not (null? ell)) (null? (cdr ell))))

(define (%validate-and-tag-open-binary-input-port port who)
  ;;Assuming that PORT is a port object: validate PORT as an open
  ;;binary input port.  This function is to be called if the port
  ;;fast  attributes  are  not  equal to  FAST-GET-BYTE-TAG.   If
  ;;successful  mark  the  port  with the  FAST-GET-BYTE-TAG  and
  ;;return, else raise an exception.
  ;;
  ;;This function  is meant to  be used by  GET-U8, LOOKAHEAD-U8,
  ;;GET-BYTEVECTOR-N,   GET-BYTEVECTOR-N!,   GET-BYTEVECTOR-SOME,
  ;;GET-BYTEVECTOR-ALL.
  ;;
  (with-binary-port (port)
    (%unsafe.assert-value-is-open-port port who)
    (when port.transcoder
      (die who "port is not binary" port))
    (unless port.read!
      (die who "port is not an input port" port))
    (set! port.attributes (%unsafe.fxior port.attributes fast-get-byte-tag))))


;;;; bytevector helpers

(define (%unsafe.bytevector-u16-ref bv index endianness)
  ;;Like BYTEVECTOR-U16-REF  defined by R6RS.  Assume  all the arguments
  ;;to  have been  already validated;  expect the  index integers  to be
  ;;fixnums.
  ;;
  (if (eq? endianness 'little)
      (%unsafe.fxior (unsafe.bytevector-u8-ref bv index)
		     (unsafe.fxsll (unsafe.bytevector-u8-ref bv (unsafe.fxadd1 index)) 8))
    (%unsafe.fxior (unsafe.bytevector-u8-ref bv (unsafe.fxadd1 index))
		   (unsafe.fxsll (unsafe.bytevector-u8-ref bv index) 8))))

(define (%unsafe.bytevector-copy! src.bv src.start dst.bv dst.start count)
  ;;Like BYTEVECTOR-COPY!   defined by  R6RS.  Assume all  the arguments
  ;;have been  already validated;  expect all the  exact integers  to be
  ;;fixnums.
  ;;
  (when (unsafe.fx> count 0)
    (unsafe.bytevector-u8-set! dst.bv dst.start (unsafe.bytevector-u8-ref src.bv src.start))
    (%unsafe.bytevector-copy! src.bv (unsafe.fxadd1 src.start)
			      dst.bv (unsafe.fxadd1 dst.start)
			      (unsafe.fxsub1 count))))

(define (%unsafe.bigdst-bytevector-copy! src.bv src.start dst.bv dst.start count)
  ;;Like BYTEVECTOR-COPY!   defined by  R6RS.  Assume all  the arguments
  ;;have been  already validated;  expect all the  exact integers  to be
  ;;fixnums with the exception of DST.START.
  ;;
  (when (unsafe.fx> count 0)
    (unsafe.bytevector-u8-set! dst.bv dst.start (unsafe.bytevector-u8-ref src.bv src.start))
    (%unsafe.bigdst-bytevector-copy! src.bv (unsafe.fxadd1 src.start)
				     dst.bv (+ 1           dst.start)
				     (unsafe.fxsub1 count))))

(define (%unsafe.big-bytevector-copy! src.bv src.start dst.bv dst.start count)
  ;;Like BYTEVECTOR-COPY!   defined by  R6RS.  Assume all  the arguments
  ;;have been  already validated;  expect all the  exact integers  to be
  ;;either fixnums or bignums.
  ;;
  (when (> count 0)
    (unsafe.bytevector-u8-set! dst.bv dst.start (unsafe.bytevector-u8-ref src.bv src.start))
    (%unsafe.big-bytevector-copy! src.bv (+ 1 src.start)
				  dst.bv (+ 1 dst.start)
				  (- count 1))))

(define (%unsafe.bytevector-reverse-and-concatenate list-of-bytevectors)
  ;;Reverse the  argument and  concatenate its bytevector  items; return
  ;;the result.  Assume the argument has been already validated.
  ;;
  ;;The bytevectors may have either a fixnum or a bignum as length.
  ;;
  (call-with-values
      (lambda ()
	(let recur ((bvs list-of-bytevectors)
		    (accumulated-total-length 0))
	  (if (null? bvs)
	      (values (unsafe.make-bytevector accumulated-total-length) 0)
	    (let* ((src.bv  (car bvs))
		   (src.len (unsafe.bytevector-length src.bv)))
	      (let-values (((dst.bv next-byte-index)
			    (recur (cdr bvs) (+ src.len accumulated-total-length))))
		(%unsafe.big-bytevector-copy! src.bv 0 dst.bv next-byte-index src.len)
		(values dst.bv (+ src.len next-byte-index)))))))
    (lambda (full-bytevector dummy)
      full-bytevector)))


;;;; string helpers

(define (%unsafe.string-copy! src.str src.start dst.str dst.start count)
  ;;Like  BYTEVECTOR-COPY!   defined by  R6RS,  but for  strings;
  ;;expect all the exact integers to be fixnums.
  ;;
  (when (unsafe.fx> count 0)
    (unsafe.string-set! dst.str dst.start (unsafe.string-ref src.str src.start))
    (%unsafe.string-copy! src.str (unsafe.fxadd1 src.start)
			  dst.str (unsafe.fxadd1 dst.start)
			  (unsafe.fxsub1 count))))

(define (%unsafe.bigdst-string-copy! src.str src.start dst.str dst.start count)
  ;;Like BYTEVECTOR-COPY!  defined by R6RS, but for strings.  Assume all
  ;;the  arguments have  been already  validated; expect  all  the exact
  ;;integers to be fixnums with the exception of DST.START.
  ;;
  (when (unsafe.fx> count 0)
    (unsafe.string-set! dst.str dst.start (unsafe.string-ref src.str src.start))
    (%unsafe.bigdst-string-copy! src.str (unsafe.fxadd1 src.start)
				 dst.str (+ 1           dst.start)
				 (unsafe.fxsub1 count))))

(define (%unsafe.big-string-copy! src.str src.start dst.str dst.start count)
  ;;Like BYTEVECTOR-COPY!  defined by R6RS, but for strings.  Assume all
  ;;the  arguments have  been already  validated; expect  all  the exact
  ;;integers to be either fixnums or bignums.
  ;;
  (when (> count 0)
    (unsafe.string-set! dst.str dst.start (unsafe.string-ref src.str src.start))
    (%unsafe.big-string-copy! src.str (+ 1 src.start)
			      dst.str (+ 1 dst.start)
			      (- count 1))))

(define (%unsafe.string-reverse-and-concatenate list-of-strings)
  ;;Reverse the  argument and concatenate  its string items;  return the
  ;;result.  Assume the argument has been already validated.
  ;;
  ;;The strings may have either a fixnum or a bignum as length.
  ;;
  (call-with-values
      (lambda ()
	(let recur ((strs list-of-strings)
		    (accumulated-total-length 0))
	  (if (null? strs)
	      (values (unsafe.make-string accumulated-total-length) 0)
	    (let* ((src.str  (car strs))
		   (src.len (unsafe.string-length src.str)))
	      (let-values (((dst.str next-byte-index)
			    (recur (cdr strs) (+ src.len accumulated-total-length))))
		(%unsafe.big-string-copy! src.str 0 dst.str next-byte-index src.len)
		(values dst.str (+ src.len next-byte-index)))))))
    (lambda (full-string dummy)
      full-string)))


;;;; dot notation macros for port structures

(define-syntax with-port
  (syntax-rules ()
    ((_ (?port) . ?body)
     (%with-port (?port #f) . ?body))))

(define-syntax with-binary-port
  (syntax-rules ()
    ((_ (?port) . ?body)
     (%with-port (?port bytevector-length) . ?body))))

(define-syntax with-textual-port
  (syntax-rules ()
    ((_ (?port) . ?body)
     (%with-port (?port string-length) . ?body))))

(define-syntax %with-port
  (lambda (stx)
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
		;fixnum, number of bytes used in the buffer
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
	      (PORT.FAST-ATTRIBUTES		(%dot-id ".fast-attributes"))
		;fixnum, the type attributes bits
	      (PORT.BUFFER.SIZE			(%dot-id ".buffer.size"))
		;fixnum, the buffer size
	      (PORT.BUFFER.RESET-TO-EMPTY!	(%dot-id ".buffer.reset-to-empty!"))
		;method, set the buffer index and used size to zero
	      (PORT.BUFFER.ROOM			(%dot-id ".buffer.room"))
		;method, the number of bytes available in the buffer
	      (PORT.BUFFER.INDEX.INCR!		(%dot-id ".buffer.index.incr!"))
		;method, increment the buffer index
	      (PORT.BUFFER.USED-SIZE.INCR!	(%dot-id ".buffer.used-size.incr!"))
		;method, increment
	      (PORT.MARK-AS-CLOSED		(%dot-id ".mark-as-closed"))
		;method, mark the port as closed
	      (PORT.DEVICE			(%dot-id ".device"))
		;false or Scheme value, references the underlying device
	      (PORT.DEVICE.POSITION		(%dot-id ".device.position"))
		;exact integer, track the current device position
	      (PORT.DEVICE.POSITION.INCR!	(%dot-id ".device.position.incr!"))
		;method, increment the POS field of the cookie
	      (PORT.MODE			(%dot-id ".mode"))
		;Scheme symbol, select the port mode
	      (PORT.DEVICE.IS-DESCRIPTOR?	(%dot-id ".device.is-descriptor?"))
		;true if the device is a fixnum (it is interpreted as platform descriptor)
	      )
	   #'(let-syntax
		 ((PORT.TAG		(identifier-syntax ($port-tag		?port)))
		  (PORT.BUFFER		(identifier-syntax ($port-buffer	?port)))
		  (PORT.TRANSCODER	(identifier-syntax ($port-transcoder	?port)))
		  (PORT.ID		(identifier-syntax ($port-id		?port)))
		  (PORT.READ!		(identifier-syntax ($port-read!		?port)))
		  (PORT.WRITE!		(identifier-syntax ($port-write!	?port)))
		  (PORT.SET-POSITION!	(identifier-syntax ($port-set-position!	?port)))
		  (PORT.GET-POSITION	(identifier-syntax ($port-get-position	?port)))
		  (PORT.CLOSE		(identifier-syntax ($port-close		?port)))
		  (PORT.COOKIE		(identifier-syntax ($port-cookie	?port)))
		  (PORT.CLOSED?		(identifier-syntax (%unsafe.port-closed? ?port)))
		  (PORT.FAST-ATTRIBUTES	(identifier-syntax ($port-fast-attrs	?port)))
		  (PORT.BUFFER.SIZE
		   (identifier-syntax (?buffer-length ($port-buffer ?port))))
		  (PORT.ATTRIBUTES
		   (identifier-syntax
		    (_			($port-attrs ?port))
		    ((set! id ?value)	($set-port-attrs! ?port ?value))))
		  (PORT.BUFFER.INDEX
		   (identifier-syntax
		    (_			($port-index ?port))
		    ((set! _ ?value)	($set-port-index! ?port ?value))))
		  (PORT.BUFFER.USED-SIZE
		   (identifier-syntax
		    (_			($port-size ?port))
		    ((set! _ ?value)	($set-port-size! ?port ?value)))))
	       (let-syntax
		   ((PORT.BUFFER.FULL?
		     (identifier-syntax (unsafe.fx= PORT.BUFFER.USED-SIZE PORT.BUFFER.SIZE)))
		    (PORT.BUFFER.ROOM
		     (syntax-rules ()
		       ((_)
			(unsafe.fx- PORT.BUFFER.SIZE PORT.BUFFER.INDEX))))
		    (PORT.BUFFER.RESET-TO-EMPTY!
		     (syntax-rules ()
		       ((_)
			(begin
			  (set! PORT.BUFFER.INDEX     0)
			  (set! PORT.BUFFER.USED-SIZE 0)))))
		    (PORT.BUFFER.INDEX.INCR!
		     (syntax-rules ()
		       ((_)
			(set! PORT.BUFFER.INDEX (unsafe.fxadd1 PORT.BUFFER.INDEX)))
		       ((_ ?step)
			(set! PORT.BUFFER.INDEX (unsafe.fx+ ?step PORT.BUFFER.INDEX)))))
		    (PORT.BUFFER.USED-SIZE.INCR!
		     (syntax-rules ()
		       ((_)
			(set! PORT.BUFFER.USED-SIZE (unsafe.fxadd1 PORT.BUFFER.USED-SIZE)))
		       ((_ ?step)
			(set! PORT.BUFFER.USED-SIZE (unsafe.fx+ ?step PORT.BUFFER.USED-SIZE)))))
		    (PORT.MARK-AS-CLOSED
		     (syntax-rules ()
		       ((_)
			($mark-port-closed! ?port))))
		    (PORT.DEVICE
		     (identifier-syntax
		      (_			(cookie-dest PORT.COOKIE))
		      ((set! _ ?new-device)	(set-cookie-dest! PORT.COOKIE ?new-device))))
		    (PORT.DEVICE.POSITION
		     (identifier-syntax
		      (_			(cookie-pos PORT.COOKIE))
		      ((set! _ ?new-position)	(set-cookie-pos! PORT.COOKIE ?new-position))))
		    (PORT.DEVICE.POSITION.INCR!
		     (syntax-rules ()
		       ((_ ?step)
			(let ((cookie PORT.COOKIE))
			  (set-cookie-pos! cookie (+ ?step (cookie-pos cookie)))))))
		    (PORT.MODE
		     (identifier-syntax
		      (_			(cookie-mode PORT.COOKIE))
		      ((set! _ ?new-mode)	(set-cookie-mode! PORT.COOKIE ?new-mode))))
		    (PORT.DEVICE.IS-DESCRIPTOR?
		     (identifier-syntax (fixnum? (cookie-dest PORT.COOKIE))))
		    )
		 . ?body))))))))


;;;; Introduction to Unicode and UCS
;;
;;As required by R6RS,  the input/output libraries must implement
;;transcoders for textual  ports supporting encoding and decoding
;;between  Scheme characters  and UTF-8,  UTF-16,  ISO/IEC 8859-1
;;(also known as Latin-1).
;;
;;The mandatory starting points to learn about this stuff are the
;;following (URLs last verified on Sep 9, 2011):
;;
;;  <http://en.wikipedia.org/wiki/Universal_Character_Set>
;;  <http://en.wikipedia.org/wiki/Unicode>
;;  <http://en.wikipedia.org/wiki/Byte_order_mark>
;;  <http://en.wikipedia.org/wiki/UTF-8>
;;  <http://en.wikipedia.org/wiki/UTF-16>
;;  <http://en.wikipedia.org/wiki/UTF-32>
;;  <http://en.wikipedia.org/wiki/ISO/IEC_8859-1>
;;
;;here we  give only  a brief overview  of the  main definitions,
;;drawing text from those pages.
;;
;;The  "Universal  Character Set"  (UCS)  is  a  standard set  of
;;characters upon  which many  character encodings are  based; it
;;contains abstract characters, each identified by an unambiguous
;;name and an integer number called its "code point".
;;
;;"Unicode" is  a computing industry standard  for the consistent
;;encoding, representation and handling of text expressed in most
;;of the world's writing systems.
;;
;;UCS and  Unicode have an identical repertoire  and numbers: the
;;same characters with the  same numbers exist on both standards.
;;UCS  is  a  simple   character  map,  Unicode  adds  rules  for
;;collation,  normalization  of   forms,  and  the  bidirectional
;;algorithm for scripts.
;;
;;The  Unicode   Consortium,  the  nonprofit   organization  that
;;coordinates Unicode's  development, has the  goal of eventually
;;replacing existing character  encoding schemes with Unicode and
;;its standard "Unicode Transformation Format" (UTF) schemes.

;;By convention  a Unicode code  point is referred to  by writing
;;"U+" followed by its hexadecimal  number with at least 4 digits
;;(U+0044 is fine, U+12 is not).
;;
;;In  practice, Unicode  code points  are exact  integers  in the
;;range  [0, #x10FFFF],  but outside  the range  [#xD800, #xDFFF]
;;which has special meaning in  UTF schemes.  A code point can be
;;stored in  21 bits:
;;
;;  (string-length (number->string #x10FFFF 2)) => 21
;;
;;R6RS defines fixnums  to have at least 24 bits,  so a fixnum is
;;wide enough to hold a code point:
;;
;;  (fixnum? #x10FFFF) => #t
;;
;;and  indeed Scheme  characters  are a  disjoint  type of  value
;;holding such fixnums:
;;
;;  (integer->char #x10FFFF) => #\x10FFFF
;;


;;;; UTF-8 scheme and Latin-1
;;
;;UTF-8 is  a multibyte character encoding for  Unicode which can
;;represent every  character in the  Unicode set, that is  it can
;;represent  every  code point  in  the  ranges  [0, #xD800)  and
;;(#xDFFF, #x10FFFF].
;;
;;A stream of UTF-8 encoded characters is meant to be stored byte
;;by byte in fixed order (and  so without the need to specify the
;;endianness of words).
;;
;;The encoding  scheme uses sequences  of 1, 2,  3 or 4  bytes to
;;encode a each  code point as shown in  the following table; the
;;byte opening  a sequence has a  unique bit pattern  in the most
;;significant  bits and  so it  allows the  determination  of the
;;sequence length;  every byte contains a number  of payload bits
;;which   must  be   concatenated  (bitwise   inclusive   OR)  to
;;reconstruct the integer representation of a code point:
;;
;; | # of bytes | 1st byte | 2nd byte | 3rd byte | 4th byte |
;; |------------+----------+----------+----------+----------|
;; |     1       #b0xxxxxxx
;; |     2       #b110xxxxx #b10xxxxxx
;; |     3       #b1110xxxx #b10xxxxxx #b10xxxxxx
;; |     4       #b11110xxx #b10xxxxxx #b10xxxxxx #b10xxxxxx
;;
;; | # of bytes | # of payload bits |       hex range     |
;; |------------+-------------------+---------------------|
;; |     1                        7    [#x0000,   #x007F]
;; |     2               5 + 6 = 11    [#x0080,   #x07FF]
;; |     3           4 + 6 + 6 = 16    [#x0800,   #xFFFF]
;; |     4       3 + 6 + 6 + 6 = 21  [#x010000, #x10FFFF]
;;
;;Note that bytes  #xFE and #xFF cannot appear  in a valid stream
;;of UTF-8  encoded characters.  The  sequence of 3 bytes  is the
;;one  that  could encode  (but  must  not)  the forbidden  range
;;[#xD800, #xDFFF].
;;
;;The  first   128  characters  of  the   Unicode  character  set
;;correspond one-to-one with ASCII and are encoded using a single
;;octet  with the same  binary value  as the  corresponding ASCII
;;character, making valid ASCII  text valid UTF-8 encoded Unicode
;;text as well.  Such encoded bytes have the Most Significant Bit
;;(MSB) set to zero.
;;
;;Although the standard does not define it, many programs start a
;;UTF-8 stream  with a  Byte Order Mark  (BOM) composed of  the 3
;;bytes: #xEF, #xBB, #xBF.
;;

;;The following macro definitions  assume that the BYTE arguments
;;are  fixnums representing  1 byte  (they are  in the  range [0,
;;255]), while the  CODE-POINT arguments are fixnums representing
;;Unicode code points  (they are in the range  [0, #x10FFFF], but
;;outside the range [#xD800, #xDFFF]).
;;

(define-inline (utf-8-invalid-byte? byte)
  ;;Evaluate to true  if BYTE has a value  that must never appear
  ;;in a valid UTF-8 stream.
  ;;
  (or (unsafe.fx= byte #xC0)
      (unsafe.fx= byte #xC1)
      (and (unsafe.fx<= #xF5 byte) (unsafe.fx<= byte #xFF))))

;;; -------------------------------------------------------------
;;; decoding 1-byte UTF-8 to code points

(define-inline (utf-8-single-byte? byte)
  ;;Evaluate to true if BYTE is valid as 1-byte UTF-8 encoding of
  ;;a Unicode character.
  ;;
  (unsafe.fx< byte 128))

(define-inline (utf-8-decode-single-byte byte)
  ;;Decode the  code point of  a Unicode character from  a 1-byte
  ;;UTF-8 encoding.
  ;;
  byte)

(define-inline (utf-8-valid-code-point-from-1-byte? code-point)
  ;;Evaluate   to  true   if  CODE-POINT   is  a   valid  integer
  ;;representation for a code  point decoded from a 2-bytes UTF-8
  ;;sequence.
  ;;
  (and (unsafe.fx<= 0 code-point) (unsafe.fx<= code-point #x007F)))

;;; -------------------------------------------------------------
;;; decoding 2-bytes UTF-8 to code points

(define-inline (utf-8-first-of-two-bytes? byte0)
  ;;Evaluate to true if BYTE0  is valid as first of 2-bytes UTF-8
  ;;encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra byte0 5) #b110))

(define-inline (utf-8-second-of-two-bytes? byte1)
  ;;Evaluate to true if BYTE1 is valid as second of 2-bytes UTF-8
  ;;encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra byte1 6) #b10))

(define-inline (utf-8-decode-two-bytes byte0 byte1)
  ;;Decode the code  point of a Unicode character  from a 2-bytes
  ;;UTF-8 encoding.
  ;;
  (%unsafe.fxior (unsafe.fxsll (unsafe.fxand byte0 #b11111) 6)
		 (unsafe.fxand byte1 #b111111)))

(define-inline (utf-8-valid-code-point-from-2-bytes? code-point)
  ;;Evaluate   to  true   if  CODE-POINT   is  a   valid  integer
  ;;representation for a code  point decoded from a 2-bytes UTF-8
  ;;sequence.
  ;;
  (and (unsafe.fx<= #x0080 code-point) (unsafe.fx<= code-point #x07FF)))

;;; -------------------------------------------------------------
;;; decoding 3-bytes UTF-8 to code points

(define-inline (utf-8-first-of-three-bytes? byte0)
  ;;Evaluate to true if BYTE0  is valid as first of 3-bytes UTF-8
  ;;encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra byte0 4) #b1110))

(define-inline (utf-8-second-and-third-of-three-bytes? byte1 byte2)
  ;;Evaluate to true  if BYTE1 and BYTE2 are  valid as second and
  ;;third of 3-bytes UTF-8 encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra (%unsafe.fxior byte1 byte2) 6) #b10))

(define-inline (utf-8-decode-three-bytes byte0 byte1 byte2)
  ;;Decode the code  point of a Unicode character  from a 3-bytes
  ;;UTF-8 encoding.
  ;;
  (%unsafe.fxior (unsafe.fxsll (unsafe.fxand byte0   #b1111) 12)
		 (unsafe.fxsll (unsafe.fxand byte1 #b111111)  6)
		 (unsafe.fxand byte2 #b111111)))

(define-inline (utf-8-valid-code-point-from-3-bytes? code-point)
  ;;Evaluate   to  true   if  CODE-POINT   is  a   valid  integer
  ;;representation for a code  point decoded from a 3-bytes UTF-8
  ;;sequence.
  ;;
  (and (unsafe.fx<= #x0800 code-point) (unsafe.fx<= code-point #xFFFF)
       (or (unsafe.fx< code-point #xD800) (unsafe.fx<  #xDFFF code-point))))

;;; -------------------------------------------------------------
;;; decoding 4-bytes UTF-8 to code points

(define-inline (utf-8-first-of-four-bytes? byte0)
  ;;Evaluate to true if BYTE0  is valid as first of 4-bytes UTF-8
  ;;encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra byte0 3) #b11110))

(define-inline (utf-8-second-third-and-fourth-of-three-bytes? byte1 byte2 byte3)
  ;;Evaluate  to true  if BYTE1,  BYTE2  and BYTE3  are valid  as
  ;;second,  third and  fourth  of 4-bytes  UTF-8  encoding of  a
  ;;Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra (%unsafe.fxior byte1 byte2 byte3) 6) #b10))

(define-inline (utf-8-decode-four-bytes byte0 byte1 byte2 byte3)
  ;;Decode the code  point of a Unicode character  from a 4-bytes
  ;;UTF-8 encoding.
  ;;
  (%unsafe.fxior (unsafe.fxsll (unsafe.fxand byte0    #b111) 18)
		 (unsafe.fxsll (unsafe.fxand byte1 #b111111) 12)
		 (unsafe.fxsll (unsafe.fxand byte2 #b111111)  6)
		 (unsafe.fxand byte3 #b111111)))

(define-inline (utf-8-valid-code-point-from-4-bytes? code-point)
  ;;Evaluate   to  true   if  CODE-POINT   is  a   valid  integer
  ;;representation for a code  point decoded from a 3-bytes UTF-8
  ;;sequence.
  ;;
  (and (unsafe.fx<= #x010000 code-point) (unsafe.fx<= code-point #x10FFFF)))

;;; -------------------------------------------------------------
;;; encoding code points to 1-byte UTF-8

(define-inline (utf-8-code-point-single-byte? code-point)
  (and (unsafe.fx<= 0 code-point) (unsafe.fx<= 255)))

(define-inline (utf-8-encode-single-byte code-point)
  ;;Encode  the code  point of  a Unicode  character to  a 1-byte
  ;;UTF-8 encoding.
  ;;
  code-point)


;;;; UTF-16 decoding
;;
;;Given a 16 bits word  in a UTF-16 stream, represented in Scheme
;;as a fixnum  in the range [#x0000, #xFFFF],  we can classify it
;;on the following axis:
;;
;; 0000        D7FF D800    DBFF DC00      DFFF E000       FFFF
;;  |-------------||-----------||-------------||------------|
;;   single word    first in     second in      single word
;;   character      pair         pair           character
;;
;;or the following logic:
;;
;;   word in [#x0000, #xD7FF] => single word character
;;   word in [#xD800, #xDBFF] => first in surrogate pair
;;   word in [#xDC00, #xDFFF] => second in surrogate pair
;;   word in [#xE000, #xFFFF] => single word character
;;
;;The  following macros  assume  the WORD  arguments are  fixnums
;;representing 16-bit  words, that is  they must be in  the range
;;[0, #xFFFF].

;;; 1-word encoding

(define-inline (utf-16-single-word? word0)
  ;;Evaluate  to true  if WORD0  is valid  as single  16-bit word
  ;;UTF-16 encoding of a Unicode character.
  ;;
  (or (unsafe.fx< word0 #xD800) (unsafe.fx< #xDFFF word0)))

(define-inline (utf-16-decode-single-word word0)
  ;;Decode the integer representation of a Unicode character from
  ;;a 16-bit single word UTF-16 encoding.
  ;;
  word0)

;;; 2-words encoding

(define-inline (utf-16-first-of-two-words? word0)
  ;;Evaluate to true if WORD0 is  valid as first 16-bit word in a
  ;;surrogate pair UTF-16 encoding of a Unicode character.
  ;;
  (and (unsafe.fx<= #xD800 word0) (unsafe.fx<= word0 #xDBFF)))

(define-inline (utf-16-second-of-two-words? word1)
  ;;Evaluate to true if WORD1 is valid as second 16-bit word in a
  ;;surrogate pair UTF-16 encoding of a Unicode character.
  ;;
  (and (unsafe.fx<= #xDC00 word1) (unsafe.fx<= word1 #xDFFF)))

(define-inline (utf-16-decode-surrogate-pair word0 word1)
  ;;Decode the integer representation of a Unicode character from
  ;;a surrogate pair UTF-16 encoding.
  ;;
  (unsafe.fx+ #x10000
	      (%unsafe.fxior (unsafe.fxsll (unsafe.fxand word0 #x3FF) 10)
			     (unsafe.fxand word1 #x3FF))))


;;;; ISO/IEC 8859-1, Latin-1
;;
;;Latin-1 uses 1  byte per character; the first  256 Unicode code
;;points are  identical to the  content of Latin-1.   Every byte,
;;that is: every fixnum in the range [0, 255], can be interpreted
;;as a character in Latin-1 encoding.
;;

;;In  the following macros  the argument  BYTE is  meant to  be a
;;fixnum representing a byte, while the argument IREP is meant to
;;be the integer representation of a character.

(define-inline (latin-1-byte? byte)
  #t)

(define-inline (latin-1-decode byte)
  byte)

(define-inline (latin-1-irep? irep)
  #t)

(define-inline (latin-1-encode irep)
  irep)


;;;; port's buffer size customisation

;;For  ports having a  Scheme bytevector  as buffer:  the minimum
;;buffer size must be big enough  to allow the buffer to hold the
;;full byte-encoding of a Unicode character for all the supported
;;transcoders.  For ports having a Scheme string as buffer: there
;;is no rational constraint on the buffer size.
;;
;;It makes  sense to have  "as small as possible"  minimum buffer
;;size to allow easy writing  of test suites exercising the logic
;;of buffer flushing and filling.
;;
(define buffer-size-lower-limit		8)

;;The  maximum buffer size  must be  small enough  to fit  into a
;;fixnum, which  is defined by R6RS  to be capable  of holding at
;;least 24 bits.
(define buffer-size-upper-limit		(greatest-fixnum))

;;For binary ports: the default buffer size should be selected to
;;allow efficient caching of portions of binary data blobs, which
;;may be megabytes wide.
;;
(define default-binary-block-size	(* 4 4096))

;;For textual  ports: the default buffer size  should be selected
;;to allow  efficient caching  of portions of  text for  the most
;;recurring use, which  includes accumulation of "small" strings,
;;like in the use of printf-like functions.
;;
(define default-string-block-size	256)

(define-inline (%valid-buffer-size? obj)
  (and (fixnum? obj)
       (unsafe.fx>= obj buffer-size-lower-limit)
       (unsafe.fx<  obj buffer-size-upper-limit)))

(define %make-buffer-size-parameter
  (let ((error-message/invalid-buffer-size (string-append "expected fixnum in range "
							  (number->string buffer-size-lower-limit)
							  " <= x < "
							  (number->string buffer-size-upper-limit)
							  " as buffer size")))
    (lambda (init who)
      (make-parameter init
	(lambda (obj)
	  (if (%valid-buffer-size? obj)
	      obj
	    (error who error-message/invalid-buffer-size obj)))))))

(let-syntax ((define-buffer-size-parameter (syntax-rules ()
					     ((_ ?who ?init)
					      (define ?who
						(%make-buffer-size-parameter ?init ?who))))))

  ;;Customisable buffer  size for bytevector ports.  To  be used by
  ;;OPEN-BYTEVECTOR-OUTPUT-PORT and similar.
  ;;
  (define-buffer-size-parameter bytevector-port-buffer-size	default-binary-block-size)

  ;;Customisable  buffer size  for  string ports.   To  be used  by
  ;;OPEN-STRING-OUTPUT-PORT and similar.
  ;;
  (define-buffer-size-parameter string-port-buffer-size		default-string-block-size)

  ;;Customisable  buffer  size  for  file  ports.  To  be  used  by
  ;;OPEN-FILE-INPUT-PORT, OPEN-FILE-OUTPUT-PORT and similar.
  ;;
  (define-buffer-size-parameter input-file-buffer-size		(+ default-binary-block-size 128))
  (define-buffer-size-parameter output-file-buffer-size		default-binary-block-size)

  ;;Customisable  buffer size  for  socket ports.   To  be used  by
  ;;TCP-CONNECT and similar.
  ;;
  (define-buffer-size-parameter input-socket-buffer-size	(+ 128 default-binary-block-size))
  (define-buffer-size-parameter output-socket-buffer-size	default-binary-block-size)

  #| end of LET-SYNTAX |# )


;;;; predicates

;;Defined by R6RS.  Return #t if X is a port.
(define port?
  primop.port?)

;;The following  predicates are *not*  affected by the  fact that
;;the port is closed.
(let-syntax ((define-predicate
	       (syntax-rules ()
		 ((_ ?who ?bits)
		  (define (?who x)
		    (and (port? x)
			 (unsafe.fx= (unsafe.fxand ($port-tag x) ?bits) ?bits)))))))

  ;;Defined by R6RS.  Return #t if X is a textual port.
  (define-predicate textual-port? textual-port-tag)

  ;;Defined by R6RS.  Return #t if X is a binary port.
  (define-predicate binary-port? binary-port-tag)

  ;;Defined  by R6RS.   Return #t  if X  is an  output port  or a
  ;;combined input and output port.
  (define-predicate output-port? output-port-tag)

  ;;Defined  by R6RS.   Return #t  if  X is  an input  port or  a
  ;;combined input and output port.
  (define-predicate input-port? input-port-tag))

;;The following predicates have to be used after the argument has
;;been validated as  port value.  They are *not*  affected by the
;;fact that the port is closed.
(let-syntax
    ((define-unsafe-predicate (syntax-rules ()
				((_ ?who ?bits)
				 (define-inline (?who port)
				   (unsafe.fx= (unsafe.fxand ($port-attrs port) ?bits) ?bits))))))

  (define-unsafe-predicate %unsafe.binary-port?		binary-port-tag)
  (define-unsafe-predicate %unsafe.textual-port?	textual-port-tag)
  (define-unsafe-predicate %unsafe.input-port?		input-port-tag)
  (define-unsafe-predicate %unsafe.output-port?		output-port-tag)

  (define-unsafe-predicate %unsafe.binary-input-port?	binary-input-port-bits)
  (define-unsafe-predicate %unsafe.binary-output-port?	binary-output-port-bits)
  (define-unsafe-predicate %unsafe.textual-input-port?	textual-input-port-bits)
  (define-unsafe-predicate %unsafe.textual-output-port?	textual-output-port-bits))


;;;; cookie data structure
;;
;;An  instance of  this  structure is  associated  to every  port
;;position data  structure.  It registers  the underlying device,
;;if any, and it  tracks the underlying device's position, number
;;of rows and columns (when possible).
;;
;;NOTE:  It is  impossible  to  track the  row  number for  ports
;;supporting the SET-PORT-POSITION! operation.  The ROW-NUM field
;;of  the cookie  is  meaningful only  for  ports whose  position
;;increases monotonically because of read or write operations, it
;;should be invalidated whenever  the port position is moved with
;;SET-PORT-POSITION!.  Notice  that the  input port used  to read
;;Scheme source code satisfies this requirement.
;;

;;Constructor: (make-cookie DEST MODE POS ROW-NUM NEWLINE-POS)
;;
;;Field name: dest
;;Accessor name: (cookie-dest COOKIE)
;;  If an underlying device  exists: this field holds a reference
;;  to it, for example  a fixnum representing an operative system
;;  file descriptor.
;;
;;  If no device exists: this field is set to false.
;;
;;  As a special case: this  field can hold a Scheme list managed
;;  as a stack in which data is temporarily stored.  For example:
;;  this is the case of output bytevector ports.
;;
;;Field name: mode
;;Accessor name: (cookie-mode COOKIE)
;;Mutator name: (set-cookie-mode! COOKIE MODE-SYMBOL)
;;  Hold the symbol "vicare-mode" or "r6rs-mode".
;;
;;Field name: pos
;;Accessor name: (cookie-pos COOKIE)
;;Mutator name: (set-cookie-pos! COOKIE NEW-POS)
;;  If an underlying device  exists: this field holds the current
;;  position in  the underlying device.  If  no underlying device
;;  exists: this field is set to zero.
;;
;;  It  is  the  responsibility  of  the *callers*  of  the  port
;;  functions  READ!, WRITE!  and  SET-POSITION!  to  update this
;;  field.  The  port functions READ!,  WRITE!  and SET-POSITION!
;;  must not touch the cookie.
;;
;;Field name: row-num
;;Accessor name: (cookie-row-num COOKIE)
;;Mutator name: (set-cookie-row-num! COOKIE NEW-POS)
;;
;;Field name: newline-pos
;;Accessor name: (cookie-newline-pos COOKIE)
;;Mutator name: (set-cookie-newline-pos! COOKIE NEW-POS)
;;
(define-struct cookie
  (dest mode pos row-num newline-pos))

(define (default-cookie fd)
  (make-cookie fd 'vicare-mode 0 0 0))

(define (input-port-byte-position port)
  ;;Defined  by Ikarus.  Return  the port  position for  an input
  ;;port in bytes.
  ;;
  (%assert-value-is-input-port port 'input-port-byte-position)
  (with-port (port)
    (+ port.device.position (unsafe.fxadd1 port.buffer.index))))

(define (%mark/return-newline port)
  ;;Register the presence of a #\newline character at the current
  ;;position  in  the  port.   Return  a  newline  character  for
  ;;convenience at the call site.
  ;;
  (with-textual-port (port)
    (let ((cookie port.cookie))
      (set-cookie-row-num!     cookie (+ 1 (cookie-row-num cookie)))
      (set-cookie-newline-pos! cookie (+ port.device.position port.buffer.index))))
  #\newline)

(define (input-port-column-number port)
  ;;Defined by  Ikarus.  Return the current column  number for an
  ;;input port.
  ;;
;;;FIXME It computes the count assuming 1 byte = 1 character.
  (%assert-value-is-input-port port 'input-port-column-number)
  (with-textual-port (port)
    (let ((cookie port.cookie))
      (- (+ port.device.position port.buffer.index)
	 (cookie-newline-pos cookie)))))

(define (input-port-row-number port)
  ;;Defined  by Ikarus.   Return the  current row  number  for an
  ;;input port.
  ;;
;;;FIXME It computes the count assuming 1 byte = 1 character.
  (%assert-value-is-input-port port 'input-port-row-number)
  (with-textual-port (port)
    (cookie-row-num port.cookie)))


;;;; guarded ports
;;
;;It  happens that  platforms  have a  maximum  limit on  the number  of
;;descriptors allocated to  a process.  When a port  wrapping a platform
;;descriptor is  garbage collected: we want  to close it (if  it has not
;;been  closed  already) to  free  its descriptor.   We  do  it using  a
;;guardian and the POST-GC-HOOK.
;;
;;It is also a good idea to close custom ports.  We do it using the same
;;technique.
;;

(define port-guardian
  (let ((G (make-guardian)))
    (define (close-garbage-collected-ports)
      (let ((port (G)))
	(when port
	  ;;Notice that, as defined  by R6RS, CLOSE-PORT does nothing if
	  ;;PORT has already been closed.
;;; (with-port (port)
;;;   (emergency-platform-write-fd (string-append "closing guarded port " port.id)))
	  (close-port port)
	  (close-garbage-collected-ports))))
    (post-gc-hooks (cons close-garbage-collected-ports (post-gc-hooks)))
    G))

(define %port->guarded-port
  ;;Accept a port  as argument and return the port  itself.  If the port
  ;;wraps a platform descriptor: register it in the guardian.
  ;;
  (lambda (port)
    (%assert-value-is-port port '%port->guarded-port)
    (with-port (port)
      (when (or port.device.is-descriptor?
		(eq? port.device 'close-after-gc))
;;;(emergency-platform-write-fd (string-append "registering port " port.id))
	(port-guardian port)))
    port))


;;;; port position

(define (port-has-port-position? port)
  ;;Defined by R6RS.   Return #t if the port  supports the PORT-POSITION
  ;;operation, and #f otherwise.
  ;;
  (%assert-value-is-port port 'port-has-port-position?)
  (with-port (port)
    (and port.get-position #t)))

(define (port-has-set-port-position!? port)
  ;;Defined   by  R6RS.   The   PORT-HAS-SET-PORT-POSITION!?   procedure
  ;;returns #t  if the port supports  the SET-PORT-POSITION!  operation,
  ;;and #f otherwise.
  ;;
  (%assert-value-is-port port 'port-has-set-port-position!?)
  (with-port (port)
    (and port.set-position! #t)))

(define (port-position port)
  ;;Defined by R6RS.  For a binary port, PORT-POSITION returns the index
  ;;of the position at which the next byte would be read from or written
  ;;to the port as an exact non-negative integer object.
  ;;
  ;;For  a   textual  port,  PORT-POSITION  returns  a   value  of  some
  ;;implementation-dependent type representing the port's position; this
  ;;value may be useful only  as the POS argument to SET-PORT-POSITION!,
  ;;if the latter is supported on the port (see below).
  ;;
  ;;If the port does not  support the operation, PORT-POSITION raises an
  ;;exception with condition type "&assertion".
  ;;
  ;;*NOTE* For  a textual port, the port  position may or may  not be an
  ;;integer object.  If it is an integer object, the integer object does
  ;;not necessarily correspond to a byte or character position.
  ;;
  (define who 'port-position)
  (%assert-value-is-port port who)
  (%unsafe.port-get-position who port))

(define (%unsafe.port-get-position who port)
  ;;Return the current port position for PORT.
  ;;
  (with-port (port)
    (let ((device-position (%unsafe.device-get-position who port)))
      ;;DEVICE-POSITION is the position in the underlying device, but we
      ;;have to return the port  position taking into account the offset
      ;;in the input/output buffer.
      (if (%unsafe.output-port? port)
	  (+ device-position port.buffer.index)
	(- device-position (- port.buffer.used-size port.buffer.index))))))

(define (%unsafe.device-get-position who port)
  ;;Return the current device position for PORT.
  ;;
  (with-port (port)
    (let ((getpos port.get-position))
      (cond ((procedure? getpos)
	     ;;The port has a device whose position cannot be tracked by
	     ;;the cookie's POS field.
	     (let ((device-position (getpos)))
	       (%assert-value-is-get-position-result device-position port who)
	       device-position))
	    ((and (boolean? getpos) getpos)
	     ;;The  cookie's  POS  field  correctly tracks  the  current
	     ;;device position.
	     port.device.position)
	    (else
	     (assertion-violation who
	       "port does not support port-position operation" port))))))

(define (set-port-position! port new-port-position)
  ;;Defined by R6RS.  If PORT is a binary port, NEW-PORT-POSITION should
  ;;be a non-negative exact integer  object.  If PORT is a textual port,
  ;;NEW-PORT-POSITION  should   be  the  return  value  of   a  call  to
  ;;PORT-POSITION on PORT.
  ;;
  ;;The SET-PORT-POSITION! procedure  raises an exception with condition
  ;;type &ASSERTION if  the port does not support  the operation, and an
  ;;exception    with    condition    type   &I/O-INVALID-POSITION    if
  ;;NEW-PORT-POSITION is  not in the  range of valid positions  of PORT.
  ;;Otherwise, it sets the current position of the port to POS.  If PORT
  ;;is an output port, SET-PORT-POSITION!  first flushes PORT.
  ;;
  ;;If PORT  is a  binary output  port and the  current position  is set
  ;;beyond the current end of the  data in the underlying data sink, the
  ;;object is not  extended until new data is  written at that position.
  ;;The contents  of any intervening positions  are unspecified.  Binary
  ;;ports        created       by        OPEN-FILE-OUTPUT-PORT       and
  ;;OPEN-FILE-INPUT/OUTPUT-PORT  can always be  extended in  this manner
  ;;within  the limits  of the  underlying operating  system.   In other
  ;;cases, attempts  to set the port  beyond the current end  of data in
  ;;the underlying object may result in an exception with condition type
  ;;&I/O-INVALID-POSITION.
  ;;
  (define who 'set-port-position!)
  (%assert-value-is-port port who)
  (%assert-value-is-port-position new-port-position who)
  (with-port (port)
    (let ((set-position! port.set-position!))
      (cond ((procedure? set-position!)
	     ;;An   underlying   device   exists.    Call   the   port's
	     ;;SET-POSITION!   function only if  we cannot  satisfy this
	     ;;request by moving the index in the buffer.
	     (let ((old-port-position (%unsafe.port-get-position who port)))
	       (unless (= old-port-position new-port-position)
		 (let ((new-buffer.index (+ port.buffer.index
					    (- new-port-position old-port-position))))
		   (if (and (>= new-buffer.index 0)
			    (<= new-buffer.index port.buffer.used-size))
		       (set! port.buffer.index new-buffer.index)
		     (begin
		       (if (%unsafe.output-port? port)
			   (%unsafe.flush-output-port port who)
			 (port.buffer.reset-to-empty!))
		       (let-syntax ((new-device-position (identifier-syntax new-port-position)))
			 ;;If SET-POSITION!  fails we can assume nothing
			 ;;about the position in the device.
			 (set-position! new-device-position)
			 ;;Notice that the cookie's  POS field is set by
			 ;;this   function  AFTER   having  successfully
			 ;;called SET-POSITION!.
			 (set! port.device.position new-device-position))))))))
	    ((and (boolean? set-position!) set-position!)
	     ;;The  cookie's  POS field  holds  a  value representing  a
	     ;;correct  and  immutable  device  position.  We  move  the
	     ;;current port position by moving the current buffer index.
	     ;;
	     ;;Note that the correct  implementation of this case is the
	     ;;following:
	     #|
	     (let ((old-port-position (%unsafe.port-get-position who port)))
	       (unless (= old-port-position new-port-position)
		 (let ((delta (- new-port-position old-port-position)))
		   (if (<= 0 delta port.buffer.used-size)
		       (set! port.buffer.index delta)
		     (%raise-port-position-out-of-range who port new-port-position)))))
	     |#
	     ;;but we know that, at present, the only ports implementing
	     ;;this     policy    are     the    ones     returned    by
	     ;;OPEN-BYTEVECTOR-INPUT-PORT and OPEN-STRING-INPUT-PORT, so
	     ;;we can optimise with the following:
	     (unless (= new-port-position port.buffer.index)
	       (if (<= 0 new-port-position port.buffer.used-size)
		   (set! port.buffer.index new-port-position)
		 (%raise-port-position-out-of-range who port new-port-position))))
	    (else
	     (assertion-violation who
	       "port does not support set-port-position operation" port))))))


;;;; custom ports

(define (%make-custom-binary-port attributes identifier read! write! get-position set-position! close)
  ;;Build and  return a new  custom binary port, either  input or
  ;;output.  It is used by the following functions:
  ;;
  ;;	make-custom-binary-input-port
  ;;	make-custom-binary-output-port
  ;;
  (let ((buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector (bytevector-port-buffer-size)))
	(transcoder		#f)
	(cookie			(default-cookie 'close-after-gc)))
    (%port->guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer transcoder identifier
		 read! write! get-position set-position! close cookie))))

(define (%make-custom-textual-port attributes identifier read! write! get-position set-position! close)
  ;;Build and return  a new custom textual port,  either input or
  ;;output.  It is used by the following functions:
  ;;
  ;;	make-custom-textual-input-port
  ;;	make-custom-textual-output-port
  ;;
  (let ((buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-string (string-port-buffer-size)))
	(transcoder		#t)
	(cookie			(default-cookie 'close-after-gc)))
    (%port->guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer transcoder identifier
		 read! write! get-position set-position! close cookie))))

;;; --------------------------------------------------------------------

(define (make-custom-binary-input-port identifier read! get-position set-position! close)
  ;;Defined by  R6RS.  Return a  newly created binary  input port
  ;;whose byte  source is  an arbitrary algorithm  represented by
  ;;the READ!   procedure.  ID  must be a  string naming  the new
  ;;port, provided  for informational purposes  only.  READ! must
  ;;be a procedure and should  behave as specified below; it will
  ;;be called by operations that perform binary input.
  ;;
  ;;Each of the remaining arguments may be false; if any of those
  ;;arguments is  not false,  it must be  a procedure  and should
  ;;behave as specified below.
  ;;
  ;;(READ! BYTEVECTOR START COUNT)
  ;;	START will be a non--negative exact integer object, COUNT
  ;;	will be  a positive exact integer  object, and BYTEVECTOR
  ;;	will   be  a   bytevector  whose   length  is   at  least
  ;;	START+COUNT.
  ;;
  ;;	The READ! procedure should  obtain up to COUNT bytes from
  ;;	the  byte  source,  and  should write  those  bytes  into
  ;;	BYTEVECTOR starting at index START.  The READ!  procedure
  ;;	should  return  an exact  integer  object.  This  integer
  ;;	object should  represent the number of bytes  that it has
  ;;	read.  To  indicate an end of file,  the READ!  procedure
  ;;	should write no bytes and return 0.
  ;;
  ;;(GET-POSITION)
  ;;	The GET-POSITION procedure (if supplied) should return an
  ;;	exact integer object representing the current position of
  ;;	the input  port.  If not  supplied, the custom  port will
  ;;	not support the PORT-POSITION operation.
  ;;
  ;;(SET-POSITION! POS)
  ;;	POS will  be a  non--negative exact integer  object.  The
  ;;	SET-POSITION!   procedure (if  supplied)  should set  the
  ;;	position of the input port  to POS.  If not supplied, the
  ;;	custom  port  will  not  support  the  SET-PORT-POSITION!
  ;;	operation.
  ;;
  ;;(CLOSE)
  ;;	The  CLOSE  procedure (if  supplied)  should perform  any
  ;;	actions that are necessary when the input port is closed.
  ;;
  ;;*Implementation  responsibilities:*  The implementation  must
  ;;check the  return values of READ! and  GET-POSITION only when
  ;;it actually calls them as  part of an I/O operation requested
  ;;by the program.  The  implementation is not required to check
  ;;that these procedures otherwise behave as described.  If they
  ;;do  not,  however, the  behavior  of  the  resulting port  is
  ;;unspecified.
  ;;
  (define who 'make-custom-binary-input-port)
  (%assert-value-is-port-identifier identifier who)
  (%assert-value-is-read!-procedure read! who)
  (%assert-value-is-maybe-close-procedure close who)
  (%assert-value-is-maybe-get-position-procedure  get-position  who)
  (%assert-value-is-maybe-set-position!-procedure set-position! who)
  (let ((attributes		binary-input-port-bits)
	(write!			#f))
    (%make-custom-binary-port attributes identifier read! write! get-position set-position! close)))

(define (make-custom-binary-output-port identifier write! get-position set-position! close)
  ;;Defined by  R6RS.  Return a newly created  binary output port
  ;;whose byte sink is  an arbitrary algorithm represented by the
  ;;WRITE! procedure.  ID  must be a string naming  the new port,
  ;;provided for informational purposes  only.  WRITE!  must be a
  ;;procedure and  should behave as  specified below; it  will be
  ;;called by operations that perform binary output.
  ;;
  ;;Each of the remaining arguments may be false; if any of those
  ;;arguments is  not false,  it must be  a procedure  and should
  ;;behave    as     specified    in    the     description    of
  ;;MAKE-CUSTOM-BINARY-INPUT-PORT.
  ;;
  ;;(WRITE! BYTEVECTOR START count)
  ;;	START  and  COUNT  will  be  non-negative  exact  integer
  ;;	objects, and BYTEVECTOR will be a bytevector whose length
  ;;	is at least START+COUNT.
  ;;
  ;;	The WRITE! procedure should  write up to COUNT bytes from
  ;;	BYTEVECTOR starting at index  START to the byte sink.  In
  ;;	any case, the WRITE!   procedure should return the number
  ;;	of bytes that it wrote, as an exact integer object.
  ;;
  ;;*Implementation  responsibilities:*  The implementation  must
  ;;check the return values of WRITE! only when it actually calls
  ;;WRITE!  as part of an I/O operation requested by the program.
  ;;The  implementation  is not  required  to  check that  WRITE!
  ;;otherwise behaves as described.  If it does not, however, the
  ;;behavior of the resulting port is unspecified.
  ;;
  (define who 'make-custom-binary-output-port)
  (%assert-value-is-port-identifier identifier who)
  (%assert-value-is-write!-procedure write! who)
  (%assert-value-is-maybe-close-procedure close who)
  (%assert-value-is-maybe-get-position-procedure  get-position  who)
  (%assert-value-is-maybe-set-position!-procedure set-position! who)
  (let ((attributes		binary-output-port-bits)
	(read!			#f))
    (%make-custom-binary-port attributes identifier read! write! get-position set-position! close)))

;;; --------------------------------------------------------------------

(define (make-custom-textual-input-port identifier read! get-position set-position! close)
  ;;Define by  R6RS.  Return a  newly created textual  input port
  ;;whose character source  is an arbitrary algorithm represented
  ;;by the READ!  procedure.  ID  must be a string naming the new
  ;;port, provided for  informational purposes only.  READ!  must
  ;;be a procedure and should  behave as specified below; it will
  ;;be called by operations that perform textual input.
  ;;
  ;;Each of the remaining arguments may be false; if any of those
  ;;arguments is  not false,  it must be  a procedure  and should
  ;;behave as specified below.
  ;;
  ;;(READ! STRING START COUNT)
  ;;
  ;;	START will be a non--negative exact integer object, COUNT
  ;;	will be a positive  exact integer object, and STRING will
  ;;	be a string whose length is at least START+COUNT.
  ;;
  ;;	The READ!  procedure should obtain up to COUNT characters
  ;;	from  the  character   source,  and  should  write  those
  ;;	characters  into  STRING starting  at  index START.   The
  ;;	READ!   procedure should return  an exact  integer object
  ;;	representing  the  number   of  characters  that  it  has
  ;;	written.   To   indicate  an  end  of   file,  the  READ!
  ;;	procedure should write no bytes and return 0.
  ;;
  ;;(GET-POSITION)
  ;;	The GET-POSITION procedure  (if supplied) should return a
  ;;	single  value.   The return  value  should represent  the
  ;;	current position of the input port.  If not supplied, the
  ;;	custom port will not support the PORT-POSITION operation.
  ;;
  ;;(SET-POSITION! POS)
  ;;	The SET-POSITION! procedure  (if supplied) should set the
  ;;	position of  the input port to  POS if POS  is the return
  ;;	value of  a call to  GET-POSITION.  If not  supplied, the
  ;;	custom  port  will  not  support  the  SET-PORT-POSITION!
  ;;	operation.
  ;;
  ;;(CLOSE)
  ;;	The  CLOSE  procedure (if  supplied)  should perform  any
  ;;	actions that are necessary when the input port is closed.
  ;;
  ;;The port may or may  not have an an associated transcoder; if
  ;;it does, the transcoder is implementation-dependent.
  ;;
  ;;*Implementation  responsibilities:*  The implementation  must
  ;;check the  return values of READ! and  GET-POSITION only when
  ;;it actually calls them as  part of an I/O operation requested
  ;;by the program.  The  implementation is not required to check
  ;;that these procedures otherwise behave as described.  If they
  ;;do  not,  however, the  behavior  of  the  resulting port  is
  ;;unspecified.
  ;;
  (define who 'make-custom-textual-input-port)
  (%assert-value-is-port-identifier identifier who)
  (%assert-value-is-read!-procedure read! who)
  (%assert-value-is-maybe-close-procedure close who)
  (%assert-value-is-maybe-get-position-procedure  get-position  who)
  (%assert-value-is-maybe-set-position!-procedure set-position! who)
  (let ((attributes		fast-get-char-tag)
	(write!			#f))
    (%make-custom-textual-port attributes identifier read! write! get-position set-position! close)))

(define (make-custom-textual-output-port identifier write! get-position set-position! close)
  ;;Defined by R6RS.  Return  a newly created textual output port
  ;;whose byte sink is  an arbitrary algorithm represented by the
  ;;WRITE!  procedure.   IDENTIFIER must  be a string  naming the
  ;;new port,  provided for informational  purposes only.  WRITE!
  ;;must be a procedure and  should behave as specified below; it
  ;;will be called by operations that perform textual output.
  ;;
  ;;Each of the remaining arguments may be false; if any of those
  ;;arguments is  not false,  it must be  a procedure  and should
  ;;behave    as     specified    in    the     description    of
  ;;MAKE-CUSTOM-TEXTUAL-INPUT-PORT.
  ;;
  ;;(WRITE! STRING START COUNT)
  ;;
  ;;	START  and  COUNT  will  be non--negative  exact  integer
  ;;	objects, and STRING  will be a string whose  length is at
  ;;	least START+COUNT.
  ;;
  ;;	The WRITE!  procedure should write up to COUNT characters
  ;;	from  STRING starting  at  index START  to the  character
  ;;	sink.  In  any case, the WRITE!   procedure should return
  ;;	the  number of  characters  that it  wrote,  as an  exact
  ;;	integer object.
  ;,
  ;;The port may or may  not have an associated transcoder; if it
  ;;does, the transcoder is implementation-dependent.
  ;;
  ;;*Implementation  responsibilities:*  The implementation  must
  ;;check the return values of WRITE! only when it actually calls
  ;;WRITE!  as part of an I/O operation requested by the program.
  ;;The  implementation  is not  required  to  check that  WRITE!
  ;;otherwise behaves as described.  If it does not, however, the
  ;;behavior of the resulting port is unspecified.
  ;;
  (define who 'make-custom-textual-output-port)
  (%assert-value-is-port-identifier identifier who)
  (%assert-value-is-write!-procedure write! who)
  (%assert-value-is-maybe-close-procedure close who)
  (%assert-value-is-maybe-get-position-procedure  get-position  who)
  (%assert-value-is-maybe-set-position!-procedure set-position! who)
  (let ((attributes	fast-put-char-tag)
	(read!		#f))
    (%make-custom-textual-port attributes identifier read! write! get-position set-position! close)))


;;;; bytevector input ports

(define open-bytevector-input-port
  (case-lambda
   ((bv)
    (open-bytevector-input-port bv #f))
   ((bv maybe-transcoder)
    ;;Defined  by  R6RS.    MAYBE-TRANSCODER  must  be  either  a
    ;;transcoder or false.
    ;;
    ;;The  OPEN-BYTEVECTOR-INPUT-PORT procedure returns  an input
    ;;port whose bytes are  drawn from BYTEVECTOR.  If TRANSCODER
    ;;is specified, it becomes the transcoder associated with the
    ;;returned port.
    ;;
    ;;If MAYBE-TRANSCODER is false or  absent, the port will be a
    ;;binary  port   and  will  support   the  PORT-POSITION  and
    ;;SET-PORT-POSITION!  operations.  Otherwise the port will be
    ;;a textual  port, and whether it  supports the PORT-POSITION
    ;;and  SET-PORT-POSITION!  operations will  be implementation
    ;;dependent (and possibly transcoder-dependent).
    ;;
    ;;If BYTEVECTOR  is modified after OPEN-BYTEVECTOR-INPUT-PORT
    ;;has  been  called,  the  effect  on the  returned  port  is
    ;;unspecified.
    ;;
    (define who 'open-bytevector-input-port)
    (%assert-value-is-bytevector bv who)
    (%assert-value-is-maybe-transcoder maybe-transcoder who)
    ;;The input  bytevector is  itself the buffer!!!   The port is  in a
    ;;state equivalent to the following:
    ;;
    ;;                                           device position
    ;;                                                  v
    ;;   |----------------------------------------------| device
    ;;   |*******************+**************************| buffer
    ;;   ^                   ^                          ^
    ;;   0            index = port position       used-size = size
    ;;
    ;;the device position equals the  bytevector length and its value in
    ;;the cookie's POS field is never mutated.
    (let ((bv.len (unsafe.bytevector-length bv)))
      ;;FIXME  The following is  an artificial  limitation to  allow the
      ;;buffer to  be handled using  unsafe fixnum functions; it  can be
      ;;removed using a custom port for big bytevectors.
      (unless (< bv.len buffer-size-upper-limit)
	(error who "input bytevector length exceeds maximum supported size" bv.len))
      (let ((attributes		(%input-transcoder-attrs maybe-transcoder who))
	    (buffer.index	0)
	    (buffer.used-size	bv.len)
	    (buffer		bv)
	    (transcoder		maybe-transcoder)
	    (identifier		"*bytevector-input-port*")
	    (read!		all-data-in-buffer)
	    (write!		#f)
	    (get-position	#t)
	    (set-position!	#t)
	    (close		#f)
	    (cookie		(default-cookie #f)))
	(set-cookie-pos! cookie bv.len)
	($make-port attributes buffer.index buffer.used-size buffer transcoder identifier
		    read! write! get-position set-position! close cookie))))))


;;;; string input ports

(define (open-string-input-port str)
  ;;Defined  by   R6RS.   Return  a  textual   input  port  whose
  ;;characters are drawn from STR.   The port may or may not have
  ;;an  associated  transcoder; if  it  does,  the transcoder  is
  ;;implementation--dependent.   The   port  should  support  the
  ;;PORT-POSITION and SET-PORT-POSITION!  operations.
  ;;
  ;;If  STR  is modified  after  OPEN-STRING-INPUT-PORT has  been
  ;;called, the effect on the returned port is unspecified.
  ;;
  (open-string-input-port/id str "*string-input-port*"))

(define (open-string-input-port/id str id)
  ;;Defined  by Ikarus.   For  details see  the documentation  of
  ;;OPEN-STRING-INPUT-PORT.
  ;;
  ;;In this port there is  no underlying device: the input string
  ;;is set as the buffer.
  ;;
  (define who 'open-string-input-port)
  (unless (string? str)
    (die who "not a string" str))
  (%assert-value-is-port-identifier id who)
  ;;The input  string is itself  the buffer!!!  The  port is in  a state
  ;;equivalent to the following:
  ;;
  ;;                                           device position
  ;;                                                  v
  ;;   |----------------------------------------------| device
  ;;   |*******************+**************************| buffer
  ;;   ^                   ^                          ^
  ;;   0            index = port position       used-size = size
  ;;
  ;;the device  position equals the string  length and its  value in the
  ;;cookie's POS field is never mutated.
  (let ((str.len (string-length str)))
    (unless (< str.len buffer-size-upper-limit)
      (error who "input string length exceeds maximum supported size" str.len))
    (let ((attributes		fast-get-char-tag)
	  (buffer.index		0)
	  (buffer.used-size	str.len)
	  (buffer		str)
	  (transcoder		#t)
	  (read!		all-data-in-buffer)
	  (write!		#f)
	  (get-position		#t)
	  (set-position!	#t)
	  (close		#f)
	  (cookie		(default-cookie #f)))
      (set-cookie-pos! cookie str.len)
      ($make-port attributes buffer.index buffer.used-size buffer transcoder id
		  read! write! get-position set-position! close cookie))))


;;;; bytevector output ports

(define open-bytevector-output-port
  (case-lambda
   (()
    (open-bytevector-output-port #f))
   ((maybe-transcoder)
    ;;Defined  by  R6RS.    MAYBE-TRANSCODER  must  be  either  a
    ;;transcoder or false.
    ;;
    ;;The   OPEN-BYTEVECTOR-OUTPUT-PORT  procedure   returns  two
    ;;values: an  output port  and an extraction  procedure.  The
    ;;output port  accumulates the bytes written to  it for later
    ;;extraction by the procedure.
    ;;
    ;;If  MAYBE-TRANSCODER  is   a  transcoder,  it  becomes  the
    ;;transcoder associated  with the port.   If MAYBE-TRANSCODER
    ;;is false or absent, the port will be a binary port and will
    ;;support    the    PORT-POSITION   and    SET-PORT-POSITION!
    ;;operations.  Otherwise the port will be a textual port, and
    ;;whether     it    supports     the     PORT-POSITION    and
    ;;SET-PORT-POSITION!  operations  is implementation dependent
    ;;(and possibly transcoder-dependent).
    ;;
    ;;The extraction procedure  takes no arguments.  When called,
    ;;it  returns  a  bytevector  consisting of  all  the  port's
    ;;accumulated  bytes   (regardless  of  the   port's  current
    ;;position), removes the accumulated bytes from the port, and
    ;;resets the port's position.
    ;;
    (define who 'open-bytevector-output-port)
    (%assert-value-is-maybe-transcoder maybe-transcoder who)
    (let ((port			#f)
	  (attributes		(%output-transcoder-attrs maybe-transcoder who))
	  (buffer.index		0)
	  (buffer.used-size	0)
	  (buffer		(unsafe.make-bytevector (bytevector-port-buffer-size)))
	  (identifier		"*bytevector-output-port*")
	  (read!		#f)
	  (get-position		#t)
	  (close		#f)
	  (cookie		(default-cookie '())))
      ;;The most  common use of  this port type  is to append  bytes and
      ;;finally extract the whole output bytevector:
      ;;
      ;;  (call-with-values
      ;;      open-bytevector-output-port
      ;;    (lambda (port extract)
      ;;      (put-bytevector port '#vu8(1 2 3))
      ;;      ...
      ;;      (extract)))
      ;;
      ;;for  this reason  we  implement the  device  of the  port to  be
      ;;somewhat efficient for such use.  The device is a list stored in
      ;;the  cookie, called  OUTPUT-BVS in  the code;  whenever  data is
      ;;flushed  from the  buffer to  the  device: a  new bytevector  is
      ;;prepended to OUTPUT-BVS.  When  the extract function is invoked:
      ;;OUTPUT-BVS is reversed and concatenated obtaining a bytevector.
      ;;
      ;;This situation is violated  if SET-PORT-POSITION!  is applied to
      ;;the port  to move the position  before the end of  the data.  If
      ;;this  happens:  SET-POSITION!  converts  OUTPUT-BVS  to  a  list
      ;;holding a single full bytevector.
      ;;
      ;;Whenever OUTPUT-BVS  holds a single bytevector  and the position
      ;;is   less   than  such   bytevector   length:   it  means   that
      ;;SET-PORT-POSITION! was used.
      ;;
      (with-port (port)
	(define (%%serialise-device! who reset?)
	  (unless port.closed?
	    (%unsafe.flush-output-port port who))
	  (let ((bv (%unsafe.bytevector-reverse-and-concatenate port.device)))
	    (set! port.device (if reset? '() (list bv)))
	    bv))
	(define-inline (%serialise-device! who)
	  (%%serialise-device! who #f))
	(define-inline (%serialise-device-and-reset! who)
	  (%%serialise-device! who #t))

	(define (write! src.bv src.start count)
	  (%debug-assert (and (fixnum? count) (<= 0 count)))
	  (let ((output-bvs   port.device)
		(dev-position port.device.position))
	    (if (and (%list-holding-single-value? output-bvs)
		     (< dev-position (unsafe.bytevector-length (car output-bvs))))
		;;The  current  position  was  set  inside  the  already
		;;accumulated data.
		(write!/overwrite src.bv src.start count output-bvs dev-position)
	      ;;The  current  position is  at  the  end  of the  already
	      ;;accumulated data.
	      (write!/append src.bv src.start count output-bvs))
	    count))

	(define-inline (write!/overwrite src.bv src.start count output-bvs dev-position)
	  ;;Write data  to the device,  overwriting some of  the already
	  ;;existing data.   The device is already composed  of a single
	  ;;bytevector.   Remember  that DEV-POSITION  can  be either  a
	  ;;fixnum or a bignum; the same goes for DST.LEN and DST.ROOM.
	  ;;
	  (let* ((dst.bv   (car output-bvs))
		 (dst.len  (unsafe.bytevector-length dst.bv))
		 (dst.room (- dst.len dev-position)))
	    (if (<= count dst.room)
		;;The new data fits in the single bytevector.
		(%unsafe.bigdst-bytevector-copy! src.bv src.start dst.bv dev-position count)
	      (begin
		(%debug-assert (fixnum? dst.room))
		;;The new  data goes part  in the single  bytevector and
		;;part in a new bytevector.
		(%unsafe.bigdst-bytevector-copy! src.bv src.start dst.bv dev-position dst.room)
		(let* ((src.start (unsafe.fx+ src.start dst.room))
		       (count     (unsafe.fx- count     dst.room))
		       (dst.bv    (unsafe.make-bytevector count)))
		  (%unsafe.bytevector-copy! src.bv src.start dst.bv 0 count)
		  (set! port.device (cons dst.bv output-bvs)))))))

	(define-inline (write!/append src.bv src.start count output-bvs)
	  ;;Append new data to the  device.  Prepend a new bytevector to
	  ;;OUTPUT-BVS.
	  ;;
	  (let ((dst.bv (unsafe.make-bytevector count)))
	    (%unsafe.bytevector-copy! src.bv src.start dst.bv 0 count)
	    (set! port.device (cons dst.bv output-bvs))))

	(define (set-position! new-position)
	  ;;NEW-POSITION has already been  validated as exact integer by
	  ;;the  procedure  SET-PORT-POSITION!.  Here  we  only have  to
	  ;;verify  that  the  value  is  valid  as  offset  inside  the
	  ;;underlying  full bytevector.   If this  validation succeeds:
	  ;;SET-PORT-POSITION!  will store the position in the POS field
	  ;;of the cookie.
	  ;;
	  (define who 'open-bytevector-output-port/set-position!)
	  (unless (or (=  new-position port.device.position)
		      (<= new-position (unsafe.bytevector-length (%serialise-device! who))))
	    (%raise-port-position-out-of-range who port new-position)))

	(define (extract)
	  ;;The  extraction function.   Flush the  buffer to  the device
	  ;;list,  convert  the  device  list to  a  single  bytevector.
	  ;;Return the single bytevector and reset the port to its empty
	  ;;state.
	  ;;
	  (let ((bv (%serialise-device-and-reset! who)))
	    (port.buffer.reset-to-empty!)
	    (set! port.device.position 0)
	    bv))

	(set! port ($make-port attributes buffer.index buffer.used-size buffer maybe-transcoder identifier
			       read! write! get-position set-position! close cookie))
	(values port extract))))))

(define call-with-bytevector-output-port
  (case-lambda
   ((proc)
    (call-with-bytevector-output-port proc #f))
   ((proc transcoder)
    ;;Defined   by  R6RS.    PROC  must   accept   one  argument.
    ;;MAYBE-TRANSCODER must be either a transcoder or false.
    ;;
    ;;The  CALL-WITH-BYTEVECTOR-OUTPUT-PORT procedure  creates an
    ;;output port  that accumulates the  bytes written to  it and
    ;;calls PROC with that output port as an argument.
    ;;
    ;;Whenever PROC  returns, a  bytevector consisting of  all of
    ;;the  port's  accumulated bytes  (regardless  of the  port's
    ;;current position) is returned and the port is closed.
    ;;
    ;;The   transcoder  associated  with   the  output   port  is
    ;;determined as for a call to OPEN-BYTEVECTOR-OUTPUT-PORT.
    ;;
    (define who 'call-with-bytevector-output-port)
    (%assert-value-is-procedure proc who)
    (%assert-value-is-maybe-transcoder transcoder who)
    (let-values (((port extract) (open-bytevector-output-port transcoder)))
      (proc port)
      (extract)))))


;;;; string output ports

(define (open-string-output-port)
  ;;Defined by  R6RS.  Return two  values: a textual  output port
  ;;and an extraction procedure.  The output port accumulates the
  ;;characters  written  to  it   for  later  extraction  by  the
  ;;procedure.
  ;;
  ;;The port may or may  not have an associated transcoder; if it
  ;;does, the  transcoder is implementation-dependent.   The port
  ;;should  support   the  PORT-POSITION  and  SET-PORT-POSITION!
  ;;operations.
  ;;
  ;;The extraction procedure takes no arguments.  When called, it
  ;;returns a string consisting  of all of the port's accumulated
  ;;characters (regardless of  the current position), removes the
  ;;accumulated characters  from the port, and  resets the port's
  ;;position.
  ;;
  (define who 'open-string-output-port)
  (let ((port			#f)
	(attributes		fast-put-char-tag)
	(buffer.index		0)
	(buffer.used-size	0)
	(buffer			(unsafe.make-string (string-port-buffer-size)))
	(identifier		"*string-output-port*")
	(transcoder		#f)
	(read!			#f)
	(get-position		#t)
	(close			#f)
	(cookie			(default-cookie '())))
    ;;The most common use of this  port type is to append characters and
    ;;finally extract the whole output bytevector:
    ;;
    ;;  (call-with-values
    ;;      open-string-output-port
    ;;    (lambda (port extract)
    ;;      (put-string port "123")
    ;;      ...
    ;;      (extract)))
    ;;
    ;;for this reason we implement the device of the port to be somewhat
    ;;efficient  for such  use.   The device  is  a list  stored in  the
    ;;cookie, called  OUTPUT-STRS in the code; whenever  data is flushed
    ;;from  the buffer  to  the device:  a  new string  is prepended  to
    ;;OUTPUT-strs.  When the extract function is invoked: OUTPUT-STRS is
    ;;reversed and concatenated obtaining a bytevector.
    ;;
    ;;This situation  is violated  if SET-PORT-POSITION!  is  applied to
    ;;the port to move the position before the end of the data.  If this
    ;;happens: SET-POSITION!   converts OUTPUT-STRS to a  list holding a
    ;;single full string.
    ;;
    ;;Whenever  OUTPUT-STRS holds a  single string  and the  position is
    ;;less  than such  string length:  it means  that SET-PORT-POSITION!
    ;;was used.
    ;;
    (with-port (port)
      (define (%%serialise-device! who reset?)
	(unless port.closed?
	  (%unsafe.flush-output-port port who))
	(let ((bv (%unsafe.string-reverse-and-concatenate port.device)))
	  (set! port.device (if reset? '() (list bv)))
	  bv))
      (define-inline (%serialise-device! who)
	(%%serialise-device! who #f))
      (define-inline (%serialise-device-and-reset! who)
	(%%serialise-device! who #t))

      (define (write! src.str src.start count)
	(%debug-assert (and (fixnum? count) (<= 0 count)))
	(let ((output-strs  port.device)
	      (dev-position port.device.position))
	  (if (and (%list-holding-single-value? output-strs)
		   (< dev-position (unsafe.string-length (car output-strs))))
	      ;;The  current   position  was  set   inside  the  already
	      ;;accumulated data.
	      (write!/overwrite src.str src.start count output-strs dev-position)
	    ;;The  current  position  is  at  the  end  of  the  already
	    ;;accumulated data.
	    (write!/append src.str src.start count output-strs))
	  count))

      (define-inline (write!/overwrite src.str src.start count output-strs dev-position)
	;;Write  data to  the device,  overwriting some  of  the already
	;;existing  data.  The device  is already  composed of  a single
	;;string.  Remember that DEV-POSITION  can be either a fixnum or
	;;a bignum; the same goes for DST.LEN and DST.ROOM.
	;;
	(let* ((dst.str  (car output-strs))
	       (dst.len  (unsafe.string-length dst.str))
	       (dst.room (- dst.len dev-position)))
	  (if (<= count dst.room)
	      ;;The new data fits in the single string.
	      (%unsafe.bigdst-string-copy! src.str src.start dst.str dev-position count)
	    (begin
	      (%debug-assert (fixnum? dst.room))
	      ;;The new data goes part  in the single string and part in
	      ;;a new string.
	      (%unsafe.bigdst-string-copy! src.str src.start dst.str dev-position dst.room)
	      (let* ((src.start (unsafe.fx+ src.start dst.room))
		     (count     (unsafe.fx- count     dst.room))
		     (dst.str   (unsafe.make-string count)))
		(%unsafe.string-copy! src.str src.start dst.str 0 count)
		(set! port.device (cons dst.str output-strs)))))))

      (define-inline (write!/append src.str src.start count output-strs)
	;;Append  new data  to  the  device.  Prepend  a  new string  to
	;;OUTPUT-STRS.
	;;
	(let ((dst.str (unsafe.make-string count)))
	  (%unsafe.string-copy! src.str src.start dst.str 0 count)
	  (set! port.device (cons dst.str output-strs))))

      (define (set-position! new-position)
	;;NEW-POSITION has  already been  validated as exact  integer by
	;;the procedure SET-PORT-POSITION!.  Here we only have to verify
	;;that the value  is valid as offset inside  the underlying full
	;;string.  If this validation succeeds: SET-PORT-POSITION!  will
	;;store the position in the POS field of the cookie.
	;;
	(define who 'open-string-output-port/set-position!)
	(unless (or (=  new-position port.device.position)
		    (<= new-position (unsafe.string-length (%serialise-device! who))))
	  (%raise-port-position-out-of-range who port new-position)))

      (define (extract)
	;;The extraction function.  Flush the buffer to the device list,
	;;convert the device list to a single string.  Return the single
	;;string and reset the port to its empty state.
	;;
	(let ((bv (%serialise-device-and-reset! who)))
	  (port.buffer.reset-to-empty!)
	  (set! port.device.position 0)
	  bv))

      (set! port ($make-port attributes buffer.index buffer.used-size buffer transcoder identifier
			     read! write! get-position set-position! close cookie))
      (values port extract))))

(define (get-output-string port)
  ;;Defined by Ikarus.  Return the string accumulated in the PORT opened
  ;;by OPEN-STRING-OUTPUT-PORT.
  ;;
  (define who 'get-output-string)
  (define (wrong-port-error)
    (die who "not an output-string port" port))
  (%assert-value-is-port port who)
  (with-textual-port (port)
    (unless (%unsafe.textual-output-port? port)
      (wrong-port-error))
    (unless port.closed?
      (%unsafe.flush-output-port port who))
    (let ((cookie port.cookie))
      (unless (cookie? cookie)
	(wrong-port-error))
      (let ((output-strs port.device))
	(unless (or (null? output-strs) (pair? output-strs))
	  (wrong-port-error))
	(let ((str (%unsafe.string-reverse-and-concatenate output-strs)))
	  (port.buffer.reset-to-empty!)
	  (set! port.device '())
	  (set! port.device.position  0)
	  str)))))

;;; --------------------------------------------------------------------

(define (call-with-string-output-port proc)
  ;;Defined by R6RS.  PROC must accept one argument.
  ;;
  ;;The CALL-WITH-STRING-OUTPUT-PORT  procedure creates a textual
  ;;output port that accumulates the characters written to it and
  ;;calls PROC with that output port as an argument.
  ;;
  ;;Whenever  PROC returns,  a string  consisting of  all  of the
  ;;port's  accumulated  characters  (regardless  of  the  port's
  ;;current position) is returned and the port is closed.
  ;;
  ;;The port may or may  not have an associated transcoder; if it
  ;;does, the  transcoder is implementation-dependent.   The port
  ;;should  support   the  PORT-POSITION  and  SET-PORT-POSITION!
  ;;operations.
  ;;
  (define who 'call-with-string-output-port)
  (%assert-value-is-procedure proc who)
  (let-values (((port getter) (open-string-output-port)))
    (proc port)
    (getter)))

(define (with-output-to-string proc)
  ;;Defined  by  Ikarus.   Create  a  textual  output  port  that
  ;;accumulates  the characters  written to  it, sets  it  as the
  ;;current output  port and calls  PROC with no  arguments.  The
  ;;port is  the current output port  only for the  extent of the
  ;;call to PROC.
  ;;
  ;;Whenever  PROC returns,  a string  consisting of  all  of the
  ;;port's  accumulated  characters  (regardless  of  the  port's
  ;;current position) is returned and the port is closed.
  ;;
  (define who 'with-output-to-string)
  (%assert-value-is-procedure proc who)
  (let-values (((port extract) (open-string-output-port)))
    (parameterize ((current-output-port port))
      (proc))
    (extract)))


;;;; output to current output port

(define (with-output-to-port port proc)
  ;;Defined by  Ikarus.  Set PORT as  the current output  port and calls
  ;;PROC with  no arguments.  The port  is the current  output port only
  ;;for the extent of the call to PROC.
  ;;
  ;;PORT must  be a textual output port,  because CURRENT-OUTPUT-PORT is
  ;;supposed to return a textual output port.
  ;;
  (define who 'with-output-to-port)
  (%assert-value-is-procedure proc who)
  (%assert-value-is-output-port port who)
  (%unsafe.assert-value-is-textual-port port who)
  (parameterize ((current-output-port port))
    (proc)))


;;;; transcoded ports

(define (transcoded-port port transcoder)
  ;;Defined by R6RS.  The TRANSCODED-PORT procedure returns a new
  ;;textual  port with the  specified TRANSCODER.   Otherwise the
  ;;new textual port's state is largely the same as that of PORT,
  ;;which must be a binary port.
  ;;
  ;;If PORT  is an input  port, the new  textual port will  be an
  ;;input port  and will  transcode the bytes  that have  not yet
  ;;been  read from PORT.   If PORT  is an  output port,  the new
  ;;textual port will be an output port and will transcode output
  ;;characters  into bytes  that  are written  to  the byte  sink
  ;;represented by PORT.
  ;;
  ;;As a  side effect, however, TRANSCODED-PORT closes  PORT in a
  ;;special way that  allows the new textual port  to continue to
  ;;use the byte source or  sink represented by PORT, even though
  ;;PORT itself  is closed  and cannot be  used by the  input and
  ;;output operations.
  ;;
  (define who 'transcoded-port)
  (%assert-value-is-port port who)
  (%assert-value-is-transcoder transcoder who)
  (with-binary-port (port)
    (when port.transcoder
      (die who "not a binary port" port))
    (%unsafe.assert-value-is-open-port port who)
    (port.mark-as-closed)
    (%port->guarded-port
     ($make-port (cond (port.read!
			(%input-transcoder-attrs  transcoder who))
		       (port.write!
			(%output-transcoder-attrs transcoder who))
		       (else
			(die who "port is neither input nor output!" port)))
		 port.buffer.index port.buffer.used-size port.buffer
		 transcoder port.id
		 port.read! port.write! port.get-position port.set-position! port.close
		 port.cookie))))

(define (port-transcoder port)
  ;;Defined by R6RS.  Return  the transcoder associated with PORT
  ;;if  PORT is  textual and  has an  associated  transcoder, and
  ;;returns  false  if  PORT  is  binary  or  does  not  have  an
  ;;associated transcoder.
  ;;
  (%assert-value-is-port port 'port-transcoder)
  (with-port (port)
    (let ((tr port.transcoder))
      (and (transcoder? tr) tr))))

(define (%input-transcoder-attrs maybe-transcoder who)
  ;;Return a  fixnum representing the fast tag  attributes for an
  ;;input port using MAYBE-TRANSCODER.
  ;;
  (cond ((not maybe-transcoder)
	 binary-input-port-bits)
	((not (eq? 'none (transcoder-eol-style maybe-transcoder)))
	 (die who "unsupported transcoder eol-style" (transcoder-eol-style maybe-transcoder)))
	((eq? 'latin-1-codec (transcoder-codec maybe-transcoder))
	 fast-get-latin-tag)
	(else
	 ;;Attributes for UTF-8-CODEC and UTF-16-CODEC are set as
	 ;;part of the Byte Order Mark reading operation when the
	 ;;first char is read.
	 textual-input-port-bits)))

(define (%output-transcoder-attrs maybe-transcoder who)
  ;;Return a  fixnum representing the fast tag  attributes for an
  ;;output port using MAYBE-TRANSCODER.
  ;;
  (cond ((not maybe-transcoder)
	 binary-output-port-bits)
	((not (eq? 'none (transcoder-eol-style maybe-transcoder)))
	 (die who "unsupported transcoder eol-style" (transcoder-eol-style maybe-transcoder)))
	((eq? 'latin-1-codec (transcoder-codec maybe-transcoder))
	 fast-put-latin-tag)
	((eq? 'utf-8-codec   (transcoder-codec maybe-transcoder))
	 fast-put-utf8-tag)
	((eq? 'utf-16-codec  (transcoder-codec maybe-transcoder))
	 ;;By default we select little endian UTF-16.
	 fast-put-utf16le-tag)
	(else
	 (die who "unsupported codec" (transcoder-codec maybe-transcoder)))))


;;;; closing ports

(define (port-closed? port)
  ;;Defined  by Ikarus.   Return true  if PORT  has  already been
  ;;closed.
  ;;
  (%assert-value-is-port port 'port-closed?)
  (%unsafe.port-closed? port))

(define (%unsafe.port-closed? port)
  (with-port (port)
    (unsafe.fx= (unsafe.fxand port.attributes closed-port-tag) closed-port-tag)))

(define ($mark-port-closed! port)
  ;;Set the CLOSED?   bit to 1 in the  attributes or PORT; resets
  ;;to 0 all the fast tag bits in PORT.
  ;;
  (with-port (port)
    (set! port.attributes (%unsafe.fxior closed-port-tag
					 (unsafe.fxand port.attributes port-type-mask)))))

(define (%unsafe.close-port port)
  ;;Subroutine     for    CLOSE-PORT,     CLOSE-INPUT-PORT    and
  ;;CLOSE-OUTPUT-PORT.  Assume that PORT is a port object.
  ;;
  ;;Flush data in  the buffer to the underlying  device, mark the
  ;;port as closed and finally call the port's CLOSE function, if
  ;;any.
  ;;
  (with-port (port)
    (unless port.closed?
      (when port.write!
	(%unsafe.flush-output-port port '%unsafe.close-port))
      (port.mark-as-closed)
      (when (procedure? port.close)
	(port.close)))))

(define (close-port port)
  ;;Defined  by  R6RS.   Closes  the  port,  rendering  the  port
  ;;incapable  of delivering or  accepting data.   If PORT  is an
  ;;output port, it is flushed  before being closed.  This has no
  ;;effect if the port has already been closed.  A closed port is
  ;;still a  port.  The CLOSE-PORT  procedure returns unspecified
  ;;values.
  ;;
  (%assert-value-is-port port 'close-port)
  (%unsafe.close-port port))

(define (close-input-port port)
  ;;Define by R6RS.  Close an input port.
  ;;
  (%assert-value-is-input-port port 'close-input-port)
  (%unsafe.close-port port))

(define (close-output-port port)
  ;;Define by R6RS.  Close an output port.
  ;;
  (%assert-value-is-output-port port 'close-output-port)
  (%unsafe.close-port port))


;;;; auxiliary port functions

(define (port-id port)
  (%assert-value-is-port port 'port-id)
  (with-port (port)
    port.id))

(define (port-mode port)
  ;;Defined by Ikarus.
  ;;
  (%assert-value-is-port port 'port-mode)
  (with-port (port)
    port.mode))

(define (set-port-mode! port mode)
  ;;Defined by Ikarus.
  ;;
  (define who 'set-port-mode!)
  (%assert-value-is-port port who)
  (case mode
    ((r6rs-mode vicare-mode)
     (with-port (port)
       (set! port.mode mode)))
    (else
     (die who "invalid mode" mode))))

(define (port-eof? port)
  ;;Defined by R6RS.   PORT must be an input  port.  Return #t if
  ;;the LOOKAHEAD-U8 procedure (if PORT  is a binary port) or the
  ;;LOOKAHEAD-CHAR procedure  (if PORT  is a textual  port) would
  ;;return the  EOF object, and #f otherwise.   The operation may
  ;;block  indefinitely if  no  data is  available  but the  port
  ;;cannot be determined to be at end of file.
  ;;
  (define who 'port-eof?)
  (%assert-value-is-port port who)
  (%unsafe.assert-value-is-input-port port who)
  (with-port (port)
    (%unsafe.assert-value-is-open-port port who)
    ;;Checking the  buffer status  is the fastest  path to
    ;;the result.
    (cond ((unsafe.fx< port.buffer.index port.buffer.used-size)
	   #f)
	  (port.transcoder
	   (eof-object? (lookahead-char port)))
	  (else
	   (eof-object? (lookahead-u8 port))))))

(define (output-port-buffer-mode port)
  ;;Defined  by  R6RS.  Return  the  symbol  that represents  the
  ;;buffer mode of PORT.
  ;;
  (%assert-value-is-output-port port 'output-port-buffer-mode)
  'block)


;;;; buffer handling for output ports
;;
;;Output functions always append  data to the output buffer; when
;;the output  buffer is full:  data is flushed to  the underlying
;;device by calling the port's WRITE! function.
;;

(define flush-output-port
  (case-lambda
   (()
    (flush-output-port (current-output-port)))
   ((port)
    ;;Defined  by R6RS.   PORT  must be  an  output port,  either
    ;;binary  or textual.   Flush  any buffered  output from  the
    ;;buffer of  PORT to the underlying file,  device, or object.
    ;;Return unspecified values.
    ;;
    ;;See %UNSAFE.FLUSH-OUTPUT-PORT for further details.
    ;;
    (define who who)
    (%assert-value-is-output-port port who)
    (with-port (port)
      (%unsafe.assert-value-is-open-port port who)
      (%unsafe.flush-output-port port who)))))

(define (%unsafe.flush-output-port port who)
  ;;PORT must be  an open output port, either  binary or textual.
  ;;Flush  any buffered  output from  the buffer  of PORT  to the
  ;;underlying  file,  device,  or  object.   Return  unspecified
  ;;values.
  ;;
  ;;This should  be the only  function to call the  port's WRITE!
  ;;function.
  ;;
  ;;If PORT.WRITE!   returns an invalid  value: the state  of the
  ;;port is considered undefined  and the port unusable; the port
  ;;is marked closed to avoid further operations and an assertion
  ;;violation is raised.
  ;;
  ;;If PORT.WRITE!   returns zero written bytes  or cannot absorb
  ;;all  the bytes in  the buffer:  this function  loops retrying
  ;;until PORT.WRITE!  accepts the data, which may be forever but
  ;;it is  compliant to  R6RS requirement to  block as  needed to
  ;;output data.
  ;;
  ;;FIXME  As  a  Vicare-specific  customisation: we  may  add  a
  ;;parameter to optionally  request raising a special exception,
  ;;like "port  would block",  when the underlying  device cannot
  ;;absorb all the data in the buffer.
  ;;
  (with-port (port)
    (unless (unsafe.fxzero? port.buffer.used-size)
      ;;The  buffer  is  not  empty.   We  assume  the  following
      ;;scenario is possible:
      ;;
      ;;         cookie.pos
      ;;             v                             device
      ;;  |----------+-------------------------------|
      ;;             |*****+*******+--------| buffer
      ;;             ^     ^       ^
      ;;             0   index  used-size
      ;;
      ;;and we want to flush to  the device all of the used units
      ;;in the buffer; for this  purpose we think of the scenario
      ;;as the following:
      ;;
      ;;         cookie.pos
      ;;             v                             device
      ;;  |----------+-------------------------------|
      ;;             |*************+--------| buffer
      ;;             ^             ^
      ;;          0 = index     used-size
      ;;
      ;;and we try to write all the data between 0 and USED-SIZE.
      ;;If  we  succeed  we  leave  the  port  in  the  following
      ;;scenario:
      ;;
      ;;                       cookie.pos
      ;;                           v               device
      ;;  |------------------------+-----------------|
      ;;                           |----------------------| buffer
      ;;                           ^
      ;;                   0 = index = used-size
      ;;
      ;;with the buffer empty and the device position updated.
      ;;
      (let try-again-after-partial-write ((buffer.offset 0))
	(let ((count (port.write! port.buffer buffer.offset port.buffer.used-size)))
	  (if (not (and (fixnum? count)
			(unsafe.fx>= count 0)
			(unsafe.fx<= count port.buffer.used-size)))
	      (begin
		;;Avoid further operations and raise an error.
		(port.mark-as-closed)
		(die who "write! returned an invalid value" count))
	    (cond ((unsafe.fx= count port.buffer.used-size)
		   ;;Full success, all data absorbed.
		   (port.device.position.incr! port.buffer.used-size)
		   (port.buffer.reset-to-empty!))
		  ((unsafe.fxzero? count)
		   ;;Failure, no data absorbed.  Try again.
		   (try-again-after-partial-write buffer.offset))
		  (else
		   ;;Partial  success, some  data  absorbed.  Try
		   ;;again flushing the data left in the buffer.
		   (try-again-after-partial-write (unsafe.fx+ buffer.offset count))))))))))


;;;; bytevector buffer handling for input ports
;;
;;Input functions  always read bytes from the  input buffer; when
;;the  input buffer is  completely consumed:  new bytes  are read
;;from  the  underlying device  refilling  the buffer.   Whenever
;;refilling  reads no  characters (that  is: the  READ!  function
;;returns 0) the port is in EOF state.
;;
;;The following macros make it easier to handle this mechanism by
;;wrapping the %UNSAFE.REFILL-INPUT-PORT-BYTEVECTOR-BUFFER function.
;;

(define-syntax data-is-needed-at:	(syntax-rules ()))
(define-syntax if-available-data:	(syntax-rules ()))
(define-syntax if-successful-refill:	(syntax-rules ()))
(define-syntax if-end-of-file:		(syntax-rules ()))

(define-syntax %refill-bytevector-buffer-and-evaluate
  ;;?PORT must be an input  port with a bytevector as buffer; the
  ;;buffer  must  be  fully  consumed.   Refill  the  buffer  and
  ;;evaluate a sequence of forms.
  ;;
  ;;The  code in  this  macro  mutates the  fields  of the  ?PORT
  ;;structure representing the buffer  state, so the client forms
  ;;must reload the fields they use.
  ;;
  ;;If refilling the buffer succeeds: evaluate ?AFTER-REFILL-BODY
  ;;and return its result.
  ;;
  ;;If  refilling the  buffer finds  the  EOF with  no new  bytes
  ;;available: evaluate ?END-OF-FILE-BODY and return its result.
  ;;
  (syntax-rules (if-end-of-file: if-successful-refill:)
    ((%refill-bytevector-buffer-and-evaluate (?port ?who)
       (if-end-of-file:		. ?end-of-file-body)
       (if-successful-refill:	. ?after-refill-body))
     (let ((count (%unsafe.refill-input-port-bytevector-buffer ?port ?who)))
       (if (unsafe.fxzero? count)
	   (begin . ?end-of-file-body)
	 (begin . ?after-refill-body))))))

(define-syntax %maybe-refill-bytevector-buffer-and-evaluate
  ;;?PORT  must be  an input  port with  a bytevector  as buffer;
  ;;there is  no constraint on  the state of the  buffer.  Refill
  ;;the buffer if needed and evaluate a sequence of forms.
  ;;
  ;;The  code in  this  macro  mutates the  fields  of the  ?PORT
  ;;structure representing the buffer  state, so the client forms
  ;;must reload the fields they use.
  ;;
  ;;If there are  bytes to be consumed in  the buffer between the
  ;;offset ?BUFFER.OFFSET and the  end of the used area: evaluate
  ;;?AVAILABLE-DATA-BODY and return its result.
  ;;
  ;;If ?BUFFER.OFFSET  references the  end of buffer's  used area
  ;;(there are  no more bytes  to be consumed) and  refilling the
  ;;buffer succeeds:  evaluate ?AFTER-REFILL-BODY and  return its
  ;;result.
  ;;
  ;;If there are no more bytes and refilling the buffer finds the
  ;;EOF with  no new bytes  available: evaluate ?END-OF-FILE-BODY
  ;;and return its result.
  ;;
  (syntax-rules (if-end-of-file: if-successful-refill: if-available-data:)
    ((%maybe-refill-bytevector-buffer-and-evaluate (?port ?who)
       (data-is-needed-at:	?buffer.offset)
       (if-end-of-file:		. ?end-of-file-body)
       (if-successful-refill:	. ?after-refill-body)
       (if-available-data:	. ?available-data-body))
     (if (unsafe.fx< ?buffer.offset ($port-size ?port))
	 (begin . ?available-data-body)
       (%refill-bytevector-buffer-and-evaluate (?port ?who)
	 (if-end-of-file:	. ?end-of-file-body)
	 (if-successful-refill:	. ?after-refill-body))))))

(define (%unsafe.refill-input-port-bytevector-buffer port who)
  ;;Assume  PORT is  an input  port object  with a  bytevector as
  ;;buffer.   Fill  the input  buffer  keeping  in  it the  bytes
  ;;already there but not  yet consumed (see the pictures below);
  ;;mutate  the  PORT structure  fields  representing the  buffer
  ;;state and the port position.
  ;;
  ;;Return the number  of new bytes loaded.  If  the return value
  ;;is zero:  the underlying device has  no more bytes,  it is at
  ;;its EOF, but  there may still be bytes to  be consumed in the
  ;;buffer unless  the buffer  was already fully  consumed before
  ;;this function call.
  ;;
  ;;This  shold be  the only  function calling  the  port's READ!
  ;;function.
  ;;
  (with-binary-port (port)
    ;;Textual  ports (with  transcoder) can  be built  on  top of
    ;;binary  input  ports;  when  this happens:  the  underlying
    ;;binary port is closed "in a special way" which still allows
    ;;the upper  textual port to  access the device.  If  PORT is
    ;;such an  underlying port this function must  not be applied
    ;;to it.  The  following check is to avoid  such a mistake by
    ;;internal functions;  this mistake should not  happen if the
    ;;functions have not bugs.
    (%unsafe.assert-value-is-open-port port who)
    (if (eq? port.read! all-data-in-buffer)
	0
      (let ((buffer port.buffer))
	;;Shift  to the  beginning data  alraedy in  buffer but
	;;still  to  be  consumed;  commit  the  new  position.
	;;Before:
	;;
	;;                          cookie.pos
	;;                              v           device
	;; |----------------------------+-------------|
	;;       |**********+###########+---------|
	;;                  ^           ^       buffer
	;;                index     used size
	;;
	;;after:
	;;
	;;                          cookie.pos
	;;                              v           device
	;; |----------------------------+-------------|
	;;                 |+###########+-------------------|
	;;                  ^           ^                 buffer
	;;                index     used size
	;;
	(let ((delta (unsafe.fx- port.buffer.used-size port.buffer.index)))
	  (unless (unsafe.fxzero? delta)
	    (bytevector-copy! buffer port.buffer.index buffer 0 delta))
	  (set! port.buffer.index     0)
	  (set! port.buffer.used-size delta))
	;;Fill the buffer with data from the device.  Before:
	;;
	;;                          cookie.pos
	;;                              v           device
	;; |----------------------------+-------------|
	;;                 |+***********+-------------------|
	;;                  ^           ^                 buffer
	;;                index     used size
	;;
	;;after:
	;;
	;;                                        cookie.pos
	;;                                            v
	;; |------------------------------------------|device
	;;                 |+*************************+-----|
	;;                  ^                         ^   buffer
	;;                index                   used size
	;;
	(let* ((max   (unsafe.fx- port.buffer.size port.buffer.used-size))
	       (count (port.read! buffer port.buffer.used-size max)))
	  (unless (fixnum? count)
	    (die who "invalid return value from read! procedure" count))
	  (unless (and (unsafe.fx>= count 0)
		       (unsafe.fx<= count max))
	    (die who "read! returned a value out of range" count))
	  (port.device.position.incr!  count)
	  (port.buffer.used-size.incr! count)
	  count)))))


;;;; byte input functions

(define (get-u8 port)
  ;;Defined  by R6RS.   Read  from the  binary  input port  PORT,
  ;;blocking as necessary, until a byte is available from PORT or
  ;;until  an  end  of  file  is  reached.   If  a  byte  becomes
  ;;available, GET-U8  returns the byte  as an octet  and updates
  ;;PORT to point just past that  byte.  If no input byte is seen
  ;;before an end of file is reached, the EOF object is returned.
  ;;
  ;;Here  we handle  the case  of byte  already available  in the
  ;;buffer, if the buffer is empty: we call a subroutine.
  ;;
  (define who 'get-u8)
  (%assert-value-is-port port who)
  (with-binary-port (port)
    (unless (unsafe.fx= port.attributes fast-get-byte-tag)
      (%validate-and-tag-open-binary-input-port port who))
    (let ((buffer.offset port.buffer.index))
      (if (unsafe.fx< buffer.offset port.buffer.used-size)
	  (begin
	    (set! port.buffer.index (unsafe.fxadd1 buffer.offset))
	    (unsafe.bytevector-u8-ref port.buffer buffer.offset))
	(%get/peek-u8-byte-mode port who 1)))))

(define (lookahead-u8 port)
  ;;Defined by R6RS.  The  LOOKAHEAD-U8 procedure is like GET-U8,
  ;;but it does not update PORT to point past the byte.
  ;;
  ;;Here  we handle  the case  of byte  already available  in the
  ;;buffer, if the buffer is empty: we call a subroutine.
  ;;
  (define who 'lookahead-u8)
  (%assert-value-is-port port who)
  (with-binary-port (port)
    (unless (unsafe.fx= port.attributes fast-get-byte-tag)
      (%validate-and-tag-open-binary-input-port port who))
    (let ((buffer.offset port.buffer.index))
      (if (unsafe.fx< buffer.offset port.buffer.used-size)
	  (unsafe.bytevector-u8-ref port.buffer buffer.offset)
	(%get/peek-u8-byte-mode port who 0)))))

(define (%get/peek-u8-byte-mode port who buffer.offset-after)
  ;;Subroutine of GET-U8 and LOOKAHEAD-U8.  To be called when the
  ;;port buffer  is fully  consumed.  Get or  peek the  next byte
  ;;from PORT, set the buffer index to BUFFER.OFFSET-AFTER.
  ;;
  (with-binary-port (port)
    (%debug-assert (fx= port.buffer.index port.buffer.used-size))
    (%refill-bytevector-buffer-and-evaluate (port who)
      (if-end-of-file: (eof-object))
      (if-successful-refill:
       (set! port.buffer.index buffer.offset-after)
       (unsafe.bytevector-u8-ref port.buffer 0)))))


;;;; bytevector input functions

(define (get-bytevector-n port count)
  ;;Defined  by  R6RS.   COUNT  must be  an  exact,  non-negative
  ;;integer object representing the number of bytes to be read.
  ;;
  ;;The  GET-BYTEVECTOR-N procedure reads  from the  binary input
  ;;PORT, blocking as necessary,  until COUNT bytes are available
  ;;from PORT or until an end of file is reached.
  ;;
  ;;If  COUNT  bytes  are   available  before  an  end  of  file,
  ;;GET-BYTEVECTOR-N returns a bytevector of size COUNT.
  ;;
  ;;If  fewer  bytes  are   available  before  an  end  of  file,
  ;;GET-BYTEVECTOR-N   returns  a  bytevector   containing  those
  ;;bytes. In  either case,  the input port  is updated  to point
  ;;just past the bytes read.
  ;;
  ;;If an end of file  is reached before any bytes are available,
  ;;GET-BYTEVECTOR-N returns the EOF object.
  ;;
  (define who 'get-bytevector-n)
  (define (%subbytevector src.bv src.len)
    ;;Build  and return  a new  bytevector holding  the  bytes in
    ;;SRC.BV from 0 to SRC.LEN.
    ;;
    (let ((dst.bv (unsafe.make-bytevector src.len)))
      (let loop ((count src.len))
	(let ((count (unsafe.fxsub1 count)))
	  (unsafe.bytevector-u8-set! dst.bv count (unsafe.bytevector-u8-ref src.bv count))
	  (if (unsafe.fxzero? count)
	      dst.bv
	    (loop count))))))
  (%assert-value-is-port port who)
  (with-binary-port (port)
    (unless (unsafe.fx= port.fast-attributes fast-get-byte-tag)
      (%validate-and-tag-open-binary-input-port port who))
    (unless (fixnum? count)
      (die who "count is not a fixnum" count))
    (cond ((unsafe.fx> count 0)
	   (let-values (((out-port extract) (open-bytevector-output-port #f)))
	     (let retry-after-filling-buffer ((count count))
	       (define (data-available-in-buffer)
		 (let* ((buffer.used-size	port.buffer.used-size)
			(buffer.offset		port.buffer.index)
			(amount-of-available	(unsafe.fx- buffer.used-size buffer.offset))
			(all-count?		(unsafe.fx<= count amount-of-available))
			(amount-to-write	(if all-count? count amount-of-available)))
		   (put-bytevector out-port port.buffer buffer.offset amount-to-write)
                   (set! port.buffer.index (unsafe.fx+ buffer.offset amount-to-write))
                   (if all-count?
		       (extract)
		     (retry-after-filling-buffer (unsafe.fx- count amount-of-available)))))
	       (%maybe-refill-bytevector-buffer-and-evaluate (port who)
		 (data-is-needed-at: port.buffer.index)
		 (if-end-of-file:
		  (let ((result (extract)))
		    (if (zero? (unsafe.bytevector-length result))
			(eof-object)
		      result)))
		 (if-successful-refill: (data-available-in-buffer))
		 (if-available-data: (data-available-in-buffer))))))
	  ((unsafe.fxzero? count)
	   '#vu8())
	  (else
	   (die who "count is negative" count)))))

(define (get-bytevector-n! port dst.bv start count)
  ;;Defined  by  R6RS.   COUNT  must be  an  exact,  non-negative
  ;;integer object, representing the  number of bytes to be read.
  ;;DST.BV  must  be  a  bytevector  with  at  least  START+COUNT
  ;;elements.
  ;;
  ;;The GET-BYTEVECTOR-N!  procedure  reads from the binary input
  ;;PORT, blocking as necessary,  until COUNT bytes are available
  ;;or until an end of file is reached.
  ;;
  ;;If COUNT bytes are available  before an end of file, they are
  ;;written into  DST.BV starting at index START,  and the result
  ;;is COUNT.
  ;;
  ;;If fewer bytes are available before the next end of file, the
  ;;available  bytes are  written into  DST.BV starting  at index
  ;;START,  and the result  is a  number object  representing the
  ;;number of bytes actually read.
  ;;
  ;;In either case, the input  port is updated to point just past
  ;;the bytes read. If an end of file is reached before any bytes
  ;;are available, GET-BYTEVECTOR-N!  returns the EOF object.
  ;;
  (define who 'get-bytevector-n!)
  (%assert-value-is-port port who)
  (with-binary-port (port)
    (unless (unsafe.fx= port.attributes fast-get-byte-tag)
      (%validate-and-tag-open-binary-input-port port who))
    (%assert-value-is-bytevector dst.bv who)
    (unless (fixnum? start)
      (die who "starting index is not a fixnum" start))
    (unless (fixnum? count)
      (die who "count is not a fixnum" count))
    (let ((len (unsafe.bytevector-length dst.bv)))
      (when (or (unsafe.fx< start 0) (unsafe.fx<= len start))
	(die who "starting index is out of range" start))
      (cond ((unsafe.fx> count 0)
	     (let ((imax (+ start count)))
	       (when (> imax len)
		 (die who "count is out of range" count (- len start)))
	       ;;We must return EOF if the first read returns EOF.
	       (let ((x (get-u8 port)))
		 (if (eof-object? x)
		     x
		   (begin
		     (unsafe.bytevector-u8-set! dst.bv start x)
		     ;;From  now on  we must  return the  number of
		     ;;read bytes.
		     (let loop ((i (unsafe.fxadd1 start)))
		       (if (unsafe.fx= i imax)
			   i
			 (let ((x (get-u8 port)))
			   (if (eof-object? x)
			       i
			     (begin
			       (unsafe.bytevector-u8-set! dst.bv i x)
			       (loop (unsafe.fxadd1 i))))))))))))
	    ((unsafe.fxzero? count)
	     0)
	    (else
	     (die who "count is negative" count))))))

(define (get-bytevector-some port)
  ;;Defined by  R6RS.  Read from the binary  input PORT, blocking
  ;;as necessary,  until bytes are  available or until an  end of
  ;;file is reached.
  ;;
  ;;If  bytes  become  available, GET-BYTEVECTOR-SOME  returns  a
  ;;freshly allocated bytevector containing the initial available
  ;;bytes (at least one), and  it updates PORT to point just past
  ;;these bytes.
  ;;
  ;;If no input bytes are seen  before an end of file is reached,
  ;;the EOF object is returned.
  ;;
  (define who 'get-bytevector-some)
  (%assert-value-is-port port who)
  (with-binary-port (port)
    (unless (unsafe.fx= port.attributes fast-get-byte-tag)
      (%validate-and-tag-open-binary-input-port port who))
    (let ()
      (define (data-available-in-buffer)
	(let* ((buffer		port.buffer)
	       (buffer.used-size port.buffer.used-size)
	       (buffer.offset	port.buffer.index)
	       (dst.bv		(make-bytevector (unsafe.fx- buffer.used-size buffer.offset))))
	  (set! port.buffer.index buffer.used-size)
	  (let loop ((buffer.offset	buffer.offset)
		     (dst.index		0))
	    (if (unsafe.fx= buffer.offset buffer.used-size)
		dst.bv
	      (begin
		(unsafe.bytevector-u8-set! dst.bv dst.index (unsafe.bytevector-u8-ref buffer buffer.offset))
		(loop (unsafe.fxadd1 buffer.offset) (unsafe.fxadd1 dst.index)))))))
      (unless (unsafe.fx= port.attributes fast-get-byte-tag)
	(die who "invalid port argument" port))
      (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	(data-is-needed-at: port.buffer.index)
	(if-end-of-file: (eof-object))
	(if-successful-refill: (data-available-in-buffer))
	(if-available-data: (data-available-in-buffer))))))

(define (get-bytevector-all port)
  ;;Defined by R6RS.   Attempts to read all bytes  until the next
  ;;end of file, blocking as necessary.
  ;;
  ;;If one  or more bytes are read,  GET-BYTEVECTOR-ALL returns a
  ;;bytevector containing all  bytes up to the next  end of file.
  ;;Otherwise, get-bytevector-all returns the EOF object.
  ;;
  ;;The operation  may block indefinitely waiting to  see if more
  ;;bytes will  become available, even if some  bytes are already
  ;;available.
  ;;
  (define who 'get-bytevector-all)
  (%assert-value-is-port port who)
  (with-binary-port (port)
    (unless (unsafe.fx= port.attributes fast-get-byte-tag)
      (%validate-and-tag-open-binary-input-port port who))
    (let-values (((out-port getter) (open-bytevector-output-port #f)))
      (let retry-after-filling-buffer ()
	(define (data-available-in-buffer)
	  (put-bytevector out-port port.buffer port.buffer.index port.buffer.used-size)
	  (retry-after-filling-buffer))
	(%maybe-refill-bytevector-buffer-and-evaluate (port who)
	  (data-is-needed-at: port.buffer.index)
	  (if-end-of-file:
	   (let ((result (getter)))
	     (if (zero? (unsafe.bytevector-length result))
		 (eof-object)
	       result)))
	  (if-successful-refill: (data-available-in-buffer))
	  (if-available-data: (data-available-in-buffer)))))))


;;;; character input functions

(module (read-char get-char lookahead-char peek-char)

  (define (get-char p)
    ;;Defined  by  R6RS.   Read  from  the  textual  input  PORT,
    ;;blocking  as  necessary,  until  a  complete  character  is
    ;;available, or until an end of file is reached.
    ;;
    ;;If a complete character is available before the next end of
    ;;file, GET-CHAR returns that character and updates the input
    ;;port to  point past  the character.  If  an end of  file is
    ;;reached before any character  is read, GET-CHAR returns the
    ;;end--of--file object.
    ;;
    (do-get-char p 'get-char))

  (define read-char
    ;;Defined by  R6RS.  Reads from textual  input PORT, blocking
    ;;as necessary  until a character  is available, or  the data
    ;;that  is  available  cannot  be  the prefix  of  any  valid
    ;;encoding, or an end of file is reached.
    ;;
    ;;If a complete character is available before the next end of
    ;;file:  READ-CHAR  returns that  character  and updates  the
    ;;input port to point past that character.
    ;;
    ;;If  an end of  file is  reached before  any data  are read:
    ;;READ-CHAR returns the end--of--file object.
    ;;
    ;;If PORT  is omitted, it  defaults to the value  returned by
    ;;CURRENT-INPUT-PORT.
    ;;
    (case-lambda
     ((port)
      (do-get-char port 'read-char))
     (()
      (do-get-char (current-input-port) 'read-char))))

  (define (do-get-char port who)
    (define-inline (recurse)
      (do-get-char port who))
    (with-textual-port (port)
      (case-textual-input-port-fast-tag port
	((fast-get-utf8-tag)
	 ;;The  PORT  is  a   binary  input  port  with  a  UTF-8
	 ;;transcoder on  top of it.  We process  here the simple
	 ;;case of single-byte character available in the buffer,
	 ;;else  we  call the  specialised  function for  reading
	 ;;UTF-8 chars.
	 (let retry-after-filling-buffer ()
	   (let ((buffer.offset-byte0 port.buffer.index))
	     (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	       (data-is-needed-at: buffer.offset-byte0)
	       (if-end-of-file: (eof-object))
	       (if-successful-refill: (retry-after-filling-buffer))
	       (if-available-data:
		(let ((byte0 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte0)))
		  (if (utf-8-single-byte? byte0)
		      (let ((N (utf-8-decode-single-byte byte0)))
			(set! port.buffer.index (unsafe.fxadd1 buffer.offset-byte0))
			(if (unsafe.fx= N newline-integer)
			    (%mark/return-newline port)
			  (unsafe.integer->char N)))
		    (get-char-utf8-mode port who byte0))))))))

	((fast-get-char-tag)
	 ;;The PORT is a textual  input port with a Scheme string
	 ;;as  buffer.  We process  here the  simple case  of one
	 ;;char  available  in  the  buffer,  else  we  call  the
	 ;;specialised function for reading characters.
	 (let ((buffer.offset port.buffer.index))
	   (cond ((unsafe.fx< buffer.offset port.buffer.used-size)
		  (set! port.buffer.index (unsafe.fxadd1 buffer.offset))
		  (let ((ch (string-ref port.buffer buffer.offset)))
		    (if (eqv? ch #\newline)
			(%mark/return-newline port)
		      ch)))
		 ((eq? port.read! all-data-in-buffer)
		  ;;The  buffer itself  is the  device and  it is
		  ;;fully consumed.
		  (eof-object))
		 (else
		  (get/lookahead-char-char-mode port who 1)))))

	((fast-get-latin-tag)
	 ;;The  PORT  is  a  binary  input port  with  a  Latin-1
	 ;;transcoder  on  top   of  it.   Knowing  that  Latin-1
	 ;;characters are 1 byte wide: we process here the simple
	 ;;case of one char available in the buffer, else we call
	 ;;the   specialised   function   for   reading   Latin-1
	 ;;characters.
	 (let ((buffer.offset port.buffer.index))
	   (if (unsafe.fx< buffer.offset port.buffer.used-size)
	       (begin
		 (set! port.buffer.index (unsafe.fxadd1 buffer.offset))
		 (let ((byte (unsafe.bytevector-u8-ref port.buffer buffer.offset)))
		   (if (eqv? byte newline-integer)
		       (%mark/return-newline port)
		     (unsafe.integer->char byte))))
	     (get/lookahead-char-latin-mode port who 1))))

	((fast-get-utf16le-tag)
	 ;;The  PORT  is  a  binary  input  port  with  a  UTF-16
	 ;;transcoder on top of it  and it has been recognised as
	 ;;holding characters in little endian order.
	 (get-utf16 port who 'little))

	((fast-get-utf16be-tag)
	 ;;The  PORT  is  a  binary  input  port  with  a  UTF-16
	 ;;transcoder on top of it  and it has been recognised as
	 ;;holding characters in big endian order.
	 (get-utf16 port who 'big))

	(else
	 ;;If PORT  references a port  structure it has  not been
	 ;;tagged yet.
	 (if (validate-port-then-parse-bom-and-add-fast-tag-to-untagged-port port who)
	     (eof-object)
	   (recurse))))))

;;; --------------------------------------------------------------------

  (define (lookahead-char port)
    ;;Defined  by  R6RS.  The  LOOKAHEAD-CHAR  procedure is  like
    ;;GET-CHAR, but  it does  not update PORT  to point  past the
    ;;character.  PORT must be a textual input port.
    ;;
    (do-peek-char port 'lookahead-char))

  (define peek-char
    ;;Define by  R6RS.  This is  the same as READ-CHAR,  but does
    ;;not consume any data from the port.
    ;;
    (case-lambda
     (()
      (do-peek-char (current-input-port) 'peek-char))
     ((port)
      (do-peek-char port 'peek-char))))

  (define (do-peek-char port who)
    (define-inline (recurse)
      (do-peek-char port who))
    (with-textual-port (port)
      (case-textual-input-port-fast-tag port
	((fast-get-utf8-tag)
	 ;;The  PORT  is  a   binary  input  port  with  a  UTF-8
	 ;;transcoder on  top of it.  We process  here the simple
	 ;;case of one single  byte char available in the buffer,
	 ;;else we call the specialised function for reading UTF8
	 ;;characters.
	 (let retry-after-filling-buffer ()
	   (let ((buffer.offset-byte0 port.buffer.index))
	     (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	       (data-is-needed-at: buffer.offset-byte0)
	       (if-end-of-file: (eof-object))
	       (if-successful-refill: (retry-after-filling-buffer))
	       (if-available-data:
		(let ((byte0 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte0)))
		  (if (utf-8-single-byte? byte0)
		      (unsafe.integer->char (utf-8-decode-single-byte byte0))
		    (lookahead-char-utf8-mode port who byte0))))))))

	((fast-get-char-tag)
	 ;;The PORT is a textual  input port with a Scheme string
	 ;;as  buffer.  We process  here the  simple case  of one
	 ;;char  available  in  the  buffer,  else  we  call  the
	 ;;specialised function for reading characters.
	 (let ((buffer.offset-char port.buffer.index))
	   (cond ((unsafe.fx< buffer.offset-char port.buffer.used-size)
		  (string-ref port.buffer buffer.offset-char))
		 ((eq? port.read! all-data-in-buffer)
		  ;;The  buffer itself  is the  device and  it is
		  ;;fully consumed.
		  (eof-object))
		 (else
		  (get/lookahead-char-char-mode port who 0)))))

	((fast-get-latin-tag)
	 ;;The  PORT  is  a  binary  input port  with  a  Latin-1
	 ;;transcoder  on  top   of  it.   Knowing  that  Latin-1
	 ;;characters are 1 byte wide: we process here the simple
	 ;;case of one char available in the buffer, else we call
	 ;;the   specialised   function   for   reading   Latin-1
	 ;;characters.
	 (let ((buffer.offset-byte port.buffer.index))
	   (if (unsafe.fx< buffer.offset-byte port.buffer.used-size)
	       (unsafe.integer->char (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte))
	     (get/lookahead-char-latin-mode port who 0))))

	((fast-get-utf16le-tag)
	 ;;The  PORT  is  a  binary  input  port  with  a  UTF-16
	 ;;transcoder on top of it  and it has been recognised as
	 ;;holding characters in little endian order.
	 (peek-utf16 port who 'little))

	((fast-get-utf16be-tag)
	 ;;The  PORT  is  a  binary  input  port  with  a  UTF-16
	 ;;transcoder on top of it  and it has been recognised as
	 ;;holding characters in big endian order.
	 (peek-utf16 port who 'big))

	(else
	 ;;If PORT  references a port  structure it has  not been
	 ;;tagged yet.
	 (if (validate-port-then-parse-bom-and-add-fast-tag-to-untagged-port port who)
	     (eof-object)
	   (recurse))))))

;;; --------------------------------------------------------------------
;;; GET-CHAR and LOOKAHEAD-CHAR for ports with UTF-8 transcoder

  (define (get-char-utf8-mode port who byte0)
    ;;Subroutine of DO-GET-CHAR.  Read  from a textual input PORT
    ;;a UTF-8 encoded character for the cases of 2, 3 and 4 bytes
    ;;encoding;  the  case  of  1-byte  encoding  is  handled  by
    ;;DO-GET-CHAR.
    ;;
    ;;BYTE0  is the  first byte  of the  UTF-8  sequence, already
    ;;extracted by the calling function.
    ;;
    ;;Return a  Scheme character or  the EOF object.  In  case of
    ;;error: honor the error mode in the port's transcoder.
    ;;
    (with-textual-port (port)
      (define-inline (main)
	(define errmsg "invalid byte while expecting first byte of UTF-8 character")
	(cond ((utf-8-invalid-byte? byte0)
	       (error-handler errmsg byte0))
	      ((utf-8-first-of-two-bytes? byte0)
	       (get-2-bytes-character byte0))
	      ((utf-8-first-of-three-bytes? byte0)
	       (get-3-bytes-character byte0))
	      ((utf-8-first-of-four-bytes? byte0)
	       (get-4-bytes-character byte0))
	      (else
	       (error-handler errmsg byte0))))

      (define-inline (get-2-bytes-character byte0)
	(let retry-after-filling-buffer-for-1-more-byte ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
		 (buffer.offset-past  (unsafe.fxadd1 buffer.offset-byte1)))
	    (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte1)
	      (if-end-of-file:
	       (set! port.buffer.index port.buffer.used-size)
	       (eof-object))
	      (if-successful-refill:
	       (retry-after-filling-buffer-for-1-more-byte))
	      (if-available-data:
	       (let ((byte1  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (errmsg "invalid second byte in 2-bytes UTF-8 character"))
		 (set! port.buffer.index buffer.offset-past)
		 (cond ((utf-8-invalid-byte? byte1)
			(error-handler errmsg byte1))
		       ((utf-8-second-of-two-bytes? byte1)
			(let ((N (utf-8-decode-two-bytes byte0 byte1)))
			  (if (utf-8-valid-code-point-from-2-bytes? N)
			      (unsafe.integer->char N)
			    (error-handler "invalid code point as result \
                                            of decoding 2-bytes UTF-8 character"
					   byte0 byte1 N))))
		       (else
			(error-handler errmsg byte1)))))))))

      (define-inline (get-3-bytes-character byte0)
	(let retry-after-filling-buffer-for-2-more-bytes ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
		 (buffer.offset-byte2 (unsafe.fxadd1 buffer.offset-byte1))
		 (buffer.offset-past  (unsafe.fxadd1 buffer.offset-byte2)))
	    (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte2)
	      (if-end-of-file:
	       (set! port.buffer.index port.buffer.used-size)
	       (error-handler "unexpected end of file while decoding 3-bytes UTF-8 character"))
	      (if-successful-refill:
	       (retry-after-filling-buffer-for-2-more-bytes))
	      (if-available-data:
	       (let ((byte1  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (byte2  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2))
		     (errmsg "invalid second or third byte in 3-bytes UTF-8 character"))
		 (set! port.buffer.index buffer.offset-past)
		 (cond ((or (utf-8-invalid-byte? byte1)
			    (utf-8-invalid-byte? byte2))
			(error-handler errmsg byte1 byte2))
		       ((utf-8-second-and-third-of-three-bytes? byte1 byte2)
			(let ((N (utf-8-decode-three-bytes byte0 byte1 byte2)))
			  (if (utf-8-valid-code-point-from-3-bytes? N)
			      (unsafe.integer->char N)
			    (error-handler "invalid code point as result \
                                            of decoding 3-bytes UTF-8 character"
					   byte0 byte1 byte2 N))))
		       (else
			(error-handler errmsg byte1 byte2)))))))))

      (define-inline (get-4-bytes-character byte0)
	(let retry-after-filling-buffer-for-3-more-bytes ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
		 (buffer.offset-byte2 (unsafe.fxadd1 buffer.offset-byte1))
		 (buffer.offset-byte3 (unsafe.fxadd1 buffer.offset-byte2))
		 (buffer.offset-past  (unsafe.fxadd1 buffer.offset-byte3)))
	    (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte3)
	      (if-end-of-file:
	       (set! port.buffer.index port.buffer.used-size)
	       (error-handler "unexpected end of file while decoding 4-bytes UTF-8 character"))
	      (if-successful-refill:
	       (retry-after-filling-buffer-for-3-more-bytes))
	      (if-available-data:
	       (let ((byte1  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (byte2  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2))
		     (byte3  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte3))
		     (errmsg "invalid second, third or fourth byte in 4-bytes UTF-8 character"))
		 (set! port.buffer.index buffer.offset-past)
		 (cond ((or (utf-8-invalid-byte? byte1)
			    (utf-8-invalid-byte? byte2)
			    (utf-8-invalid-byte? byte3))
			(error-handler errmsg byte1 byte2 byte3))
		       ((utf-8-second-third-and-fourth-of-three-bytes? byte1 byte2 byte3)
			(let ((N (utf-8-decode-four-bytes byte0 byte1 byte2 byte3)))
			  (if (utf-8-valid-code-point-from-4-bytes? N)
			      (unsafe.integer->char N)
			    (error-handler "invalid code point as result \
                                            of decoding 4-bytes UTF-8 character"
					   byte0 byte1 byte2 byte3 N))))
		       (else
			(error-handler errmsg byte1 byte2 byte3)))))))))

      (define (error-handler message . irritants)
	(let ((mode (transcoder-error-handling-mode port.transcoder)))
	  (case mode
	    ((ignore)
	     (do-get-char port who))
	    ((replace)
	     #\xFFFD)
	    ((raise)
	     (raise (condition (make-i/o-decoding-error port)
			       (make-who-condition who)
			       (make-message-condition message)
			       (make-irritants-condition irritants))))
	    (else
	     (die who "internal error, wrong transcoder error handling mode" mode)))))

      (main)))

  (define (lookahead-char-utf8-mode port who byte0)
    ;;Subroutine of DO-PEEK-CHAR.  Peek from a textual input PORT
    ;;a UTF-8 encoded character for the cases of 2, 3 and 4 bytes
    ;;encoding;  the  case  of  1-byte  encoding  is  handled  by
    ;;DO-PEEK-CHAR.
    ;;
    ;;BYTE0  is the  first byte  in the  UTF-8  sequence, already
    ;;extracted by the calling function.
    ;;
    ;;Return a  Scheme character or  the EOF object.  In  case of
    ;;error: honor the error mode in the port's transcoder.
    ;;
    (with-textual-port (port)
      (define-inline (main)
	(define errmsg
	  "invalid byte while expecting first byte of UTF-8 character")
	(cond ((utf-8-invalid-byte? byte0)
	       (error-handler errmsg byte0))
	      ((utf-8-first-of-two-bytes? byte0)
	       (peek-2-bytes-character byte0))
	      ((utf-8-first-of-three-bytes? byte0)
	       (peek-3-bytes-character byte0))
	      ((utf-8-first-of-four-bytes? byte0)
	       (peek-4-bytes-character byte0))
	      (else
	       (error-handler errmsg byte0))))

      (define-inline (peek-2-bytes-character byte0)
	(let retry-after-filling-buffer-for-1-more-byte ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0)))
	    (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte1)
	      (if-end-of-file:
	       (error-handler "unexpected end of file while decoding 2-bytes UTF-8 character"))
	      (if-successful-refill: (retry-after-filling-buffer-for-1-more-byte))
	      (if-available-data:
	       (let ((byte1  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (errmsg "invalid second byte in 2-bytes UTF-8 character"))
		 (cond ((utf-8-invalid-byte? byte1)
			(error-handler errmsg byte1))
		       ((utf-8-second-of-two-bytes? byte1)
			(let ((N (utf-8-decode-two-bytes byte0 byte1)))
			  (if (utf-8-valid-code-point-from-2-bytes? N)
			      (unsafe.integer->char N)
			    (error-handler "invalid code point as result \
                                            of decoding 2-bytes UTF-8 character"
					   byte0 byte1 N))))
		       (else
			(error-handler errmsg byte1)))))))))

      (define-inline (peek-3-bytes-character byte0)
	(let retry-after-filling-buffer-for-2-more-bytes ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
		 (buffer.offset-byte2 (unsafe.fxadd1 buffer.offset-byte1)))
	    (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte2)
	      (if-end-of-file:
	       (error-handler "unexpected end of file while decoding 3-bytes UTF-8 character"))
	      (if-successful-refill: (retry-after-filling-buffer-for-2-more-bytes))
	      (if-available-data:
	       (let ((byte1  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (byte2  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2))
		     (errmsg "invalid second or third byte in 3-bytes UTF-8 character"))
		 (cond ((or (utf-8-invalid-byte? byte1)
			    (utf-8-invalid-byte? byte2))
			(error-handler errmsg byte1 byte2))
		       ((utf-8-second-and-third-of-three-bytes? byte1 byte2)
			(let ((N (utf-8-decode-three-bytes byte0 byte1 byte2)))
			  (if (utf-8-valid-code-point-from-3-bytes? N)
			      (unsafe.integer->char N)
			    (error-handler "invalid code point as result \
                                            of decoding 3-bytes UTF-8 character"
					   byte0 byte1 byte2 N))))
		       (else
			(error-handler errmsg byte1 byte2)))))))))

      (define-inline (peek-4-bytes-character byte0)
	(let retry-after-filling-buffer-for-3-more-bytes ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
		 (buffer.offset-byte2 (unsafe.fxadd1 buffer.offset-byte1))
		 (buffer.offset-byte3 (unsafe.fxadd1 buffer.offset-byte2)))
	    (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte3)
	      (if-end-of-file:
	       (error-handler "unexpected end of file while decoding 4-bytes UTF-8 character"))
	      (if-successful-refill: (retry-after-filling-buffer-for-3-more-bytes))
	      (if-available-data:
	       (let ((byte1  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (byte2  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2))
		     (byte3  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte3))
		     (errmsg "invalid second, third or fourth byte in 4-bytes UTF-8 character"))
		 (cond ((or (utf-8-invalid-byte? byte1)
			    (utf-8-invalid-byte? byte2)
			    (utf-8-invalid-byte? byte3))
			(error-handler errmsg byte1 byte2 byte3))
		       ((utf-8-second-third-and-fourth-of-three-bytes? byte1 byte2 byte3)
			(let ((N (utf-8-decode-four-bytes byte0 byte1 byte2 byte3)))
			  (if (utf-8-valid-code-point-from-4-bytes? N)
			      (unsafe.integer->char N)
			    (error-handler "invalid code point as result \
                                            of decoding 4-bytes UTF-8 character"
					   byte0 byte1 byte2 N))))
		       (else
			(error-handler errmsg byte1 byte2 byte3)))))))))

      (define (error-handler message . irritants)
	(case (transcoder-error-handling-mode port.transcoder)
	  ((ignore)
	   (do-peek-char port who))
	  ((replace)
	   #\xFFFD)
	  ((raise)
	   (raise (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition message)
			     (make-irritants-condition irritants))))
	  (else
	   (die who "cannot happen"))))

      (main)))

;;; --------------------------------------------------------------------
;;; GET-CHAR and LOOKAHEAD-CHAR for ports with UTF-16 transcoder

  (define (get-utf16 port who endianness)
    ;;Read and return from PORT a UTF-16 encoded character; leave
    ;;the input buffer pointing to  the first byte after the read
    ;;character.
    ;;
    ;;PORT must be an already validated textual input port with a
    ;;bytevector as buffer.
    ;;
    ;;ENDIANNESS  must  be  one  among the  symbols  accepted  by
    ;;BYTEVECTOR-U16-REF.
    ;;
    ;;In  case  of error  decoding  the  input:  honor the  error
    ;;handling mode  selected in the PORT's  transcoder and leave
    ;;the  input buffer  pointing  to the  first  byte after  the
    ;;offending sequence.
    ;;
    (with-textual-port (port)
      (define-inline (recurse)
	(get-utf16 port who endianness))

      (define (error-handler message . irritants)
	;;Handle the  error honoring  the error handling  mode in
	;;port's transcoder.
	;;
	(let ((mode (transcoder-error-handling-mode port.transcoder)))
	  (case mode
	    ((ignore)
	     (do-get-char port who endianness))
	    ((replace)
	     #\xFFFD)
	    ((raise)
	     (raise (condition (make-i/o-decoding-error port)
			       (make-who-condition who)
			       (make-message-condition message)
			       (make-irritants-condition irritants))))
	    (else
	     (die who "internal error: invalid error handling mode" port mode)))))


      (define (integer->char/invalid char-code-point)
      	;;If the argument is a valid integer representation for a
      	;;Unicode  character   according  to  R6RS:   return  the
      	;;corresponding character value, else handle the error.
      	;;
	;;The  fact that  we  validate the  16-bit  words in  the
	;;UTF-16  stream does  *not* guarantee  that  a surrogate
	;;pair, once decoded  into the integer representation, is
	;;a  valid Unicode representation.   The integers  in the
	;;invalid range [#xD800, #xDFFF]  can still be encoded as
	;;surrogate pairs.
	;;
	;;This  is  why we  check  the  representation with  this
	;;function: this check is *not* a repetition of the check
	;;on the 16-bit words.
	;;
	(define errmsg
	  "invalid code point decoded from UTF-16 surrogate pair")
      	(cond ((unsafe.fx<= char-code-point #xD7FF)
      	       (unsafe.integer->char char-code-point))
      	      ((unsafe.fx<  char-code-point #xE000)
      	       (error-handler errmsg char-code-point))
      	      ((unsafe.fx<= char-code-point #x10FFFF)
      	       (unsafe.integer->char char-code-point))
      	      (else
      	       (error-handler errmsg char-code-point))))

      (let* ((buffer.offset-word0 port.buffer.index)
	     (buffer.offset-word1 (unsafe.fx+ 2 buffer.offset-word0))
	     (buffer.offset-past  (unsafe.fx+ 2 buffer.offset-word1)))
	(cond ((unsafe.fx<= buffer.offset-word1 port.buffer.used-size)
	       ;;There  are  at  least  two bytes  in  the  input
	       ;;buffer,  enough  for  a  full  UTF-16  character
	       ;;encoded as single 16 bits word.
	       (let ((word0 (%unsafe.bytevector-u16-ref port.buffer buffer.offset-word0 endianness)))
		 (cond ((utf-16-single-word? word0)
			;;The word is in  the allowed range for a
			;;UTF-16 encoded character of 16 bits.
			(set! port.buffer.index buffer.offset-word1)
			(integer->char/invalid (utf-16-decode-single-word word0)))
		       ((not (utf-16-first-of-two-words? word0))
			(set! port.buffer.index buffer.offset-word1)
			(error-handler "invalid 16-bit word while decoding UTF-16 characters" word0))
		       ((unsafe.fx<= buffer.offset-past port.buffer.used-size)
			;;The  word  is  the  first of  a  UTF-16
			;;surrogate  pair  and  the input  buffer
			;;already holds the second word.
			(let ((word1 (%unsafe.bytevector-u16-ref port.buffer buffer.offset-word1
								endianness)))
			  (if (utf-16-second-of-two-words? word1)
			      (begin
				(set! port.buffer.index buffer.offset-past)
				(integer->char/invalid (utf-16-decode-surrogate-pair word0 word1)))
			    (begin
			      (set! port.buffer.index buffer.offset-word1)
			      (error-handler "invalid value as second 16-bit word of \
                                              UTF-16 character" word0)))))
		       (else
			;;The  word  is  the  first of  a  UTF-16
			;;surrogate  pair, but input  buffer does
			;;not hold the full second word.
			(%refill-bytevector-buffer-and-evaluate (port who)
			  (if-end-of-file:
			   (set! port.buffer.index port.buffer.used-size)
			   (error-handler "unexpected end of file while reading second \
                                           16-bit word in UTF-16 surrogate pair character" word0))
			  (if-successful-refill:
			   (recurse)))))))

	      ((unsafe.fx< buffer.offset-word0 port.buffer.used-size)
	       ;;There is only 1 byte in the input buffer.
	       (%refill-bytevector-buffer-and-evaluate (port who)
		 (if-end-of-file:
		  ;;The  input  data   is  corrupted  because  we
		  ;;expected at least a  16 bits word to be there
		  ;;before EOF.
		  (set! port.buffer.index port.buffer.used-size)
		  (error-handler "unexpected end of file after byte while reading \
                                  16-bit word of UTF-16 character"
				 (unsafe.bytevector-u8-ref port.buffer buffer.offset-word0)))
		 (if-successful-refill:
		  (recurse))))

	      (else
	       ;;The input buffer is empty.
	       (%refill-bytevector-buffer-and-evaluate (port who)
		 (if-end-of-file:	(eof-object))
		 (if-successful-refill:	(recurse))))))))

  (define (peek-utf16 port who endianness)
    ;;Peek and return from PORT a UTF-16 encoded character; leave
    ;;the input buffer pointing to  the same byte it was pointing
    ;;before the call to this function.
    ;;
    ;;PORT must be an already validated textual input port with a
    ;;bytevector as buffer.
    ;;
    ;;ENDIANNESS  must  be  one  among the  symbols  accepted  by
    ;;BYTEVECTOR-U16-REF.
    ;;
    ;;In  case  of error  decoding  the  input:  honor the  error
    ;;handling mode selected in the PORT's transcoder.
    ;;
    (with-textual-port (port)
      (define-inline (recurse)
	(peek-utf16 port who endianness))

      (define (error-handler message . irritants)
	;;Handle the  error honoring  the error handling  mode in
	;;port's transcoder.
	;;
	(let ((mode (transcoder-error-handling-mode port.transcoder)))
	  (case mode
	    ((ignore)
	     (do-get-char port who endianness))
	    ((replace)
	     #\xFFFD)
	    ((raise)
	     (raise (condition (make-i/o-decoding-error port)
			       (make-who-condition who)
			       (make-message-condition message)
			       (make-irritants-condition irritants))))
	    (else
	     (die who "internal error: invalid error handling mode" port mode)))))

      (define (integer->char/invalid char-code-point)
      	;;If the argument is a valid integer representation for a
      	;;Unicode  character   according  to  R6RS:   return  the
      	;;corresponding character value, else handle the error.
      	;;
	;;The  fact that  we  validate the  16-bit  words in  the
	;;UTF-16  stream does  *not* guarantee  that  a surrogate
	;;pair, once decoded  into the integer representation, is
	;;a  valid Unicode representation.   The integers  in the
	;;invalid range [#xD800, #xDFFF]  can still be encoded as
	;;surrogate pairs.
	;;
	;;This  is  why we  check  the  representation with  this
	;;function: this check is *not* a repetition of the check
	;;on the 16-bit words.
	;;
	(define errmsg
	  "invalid code point decoded from UTF-16 surrogate pair")
      	(cond ((unsafe.fx<= char-code-point #xD7FF)
      	       (unsafe.integer->char char-code-point))
      	      ((unsafe.fx<  char-code-point #xE000)
      	       (error-handler errmsg char-code-point))
      	      ((unsafe.fx<= char-code-point #x10FFFF)
      	       (unsafe.integer->char char-code-point))
      	      (else
      	       (error-handler errmsg char-code-point))))

      (let* ((buffer.offset-word0 port.buffer.index)
	     (buffer.offset-word1 (unsafe.fx+ 2 buffer.offset-word0))
	     (buffer.offset-past  (unsafe.fx+ 2 buffer.offset-word1)))
	(cond ((unsafe.fx<= buffer.offset-word1 port.buffer.used-size)
	       ;;There  are  at  least  two bytes  in  the  input
	       ;;buffer,  enough  for  a  full  UTF-16  character
	       ;;encoded as single 16 bits word.
	       (let ((word0 (%unsafe.bytevector-u16-ref port.buffer buffer.offset-word0 endianness)))
		 (cond ((utf-16-single-word? word0)
			(integer->char/invalid (utf-16-decode-single-word word0)))
		       ((not (utf-16-first-of-two-words? word0))
			(error-handler "invalid 16-bit word while decoding UTF-16 characters" word0))
		       ((unsafe.fx<= buffer.offset-past port.buffer.used-size)
			;;The  word  is  the  first of  a  UTF-16
			;;surrogate  pair  and  the input  buffer
			;;already holds the second word.
			(let ((word1 (%unsafe.bytevector-u16-ref port.buffer buffer.offset-word1
								 endianness)))
			  (if (utf-16-second-of-two-words? word1)
			      (integer->char/invalid (utf-16-decode-surrogate-pair word0 word1))
			    (error-handler "invalid value as second 16-bit word of \
                                            UTF-16 character" word0))))
		       (else
			;;The  word  is  the  first of  a  UTF-16
			;;surrogate  pair, but input  buffer does
			;;not hold the full second word.
			(%refill-bytevector-buffer-and-evaluate (port who)
			  (if-end-of-file:
			   (error-handler "unexpected end of file while reading second \
                                           16-bit word in UTF-16 surrogate pair character" word0))
			  (if-successful-refill:
			   (recurse)))))))

	      ((unsafe.fx< buffer.offset-word0 port.buffer.used-size)
	       ;;There is only 1 byte in the input buffer.
	       (%refill-bytevector-buffer-and-evaluate (port who)
		 (if-end-of-file:
		  ;;The  input  data   is  corrupted  because  we
		  ;;expected at least a  16 bits word to be there
		  ;;before EOF.
		  (error-handler "unexpected end of file after byte while reading \
                                  16-bit word of UTF-16 character"
				 (unsafe.bytevector-u8-ref port.buffer buffer.offset-word0)))
		 (if-successful-refill:
		  (recurse))))

	      (else
	       ;;The input buffer is empty.
	       (%refill-bytevector-buffer-and-evaluate (port who)
		 (if-end-of-file:	(eof-object))
		 (if-successful-refill:	(recurse))))))))

;;; --------------------------------------------------------------------
;;; GET-CHAR and LOOKAHEAD-CHAR for ports with Latin-1 transcoder

  (define (get/lookahead-char-latin-mode port who buffer-index-increment)
    ;;PORT  must be  a textual  input port  with a  bytevector as
    ;;buffer; such buffer must be already fully consumed.
    ;;
    ;;When  BUFFER-INDEX-INCREMENT  is 1  this  function acts  as
    ;;GET-CHAR,   when  it   is   0  this   function  acts   like
    ;;LOOKAHEAD-CHAR.
    ;;
    (with-textual-port (port)
      (%debug-assert (unsafe.fx= port.buffer.index port.buffer.used-size))
      (%refill-bytevector-buffer-and-evaluate (port who)
	(if-end-of-file: (eof-object))
	(if-successful-refill:
	 (let ((buffer.offset port.buffer.index))
	   (port.buffer.index.incr! buffer-index-increment)
	   (unsafe.integer->char (unsafe.bytevector-u8-ref port.buffer buffer.offset)))))))

;;; --------------------------------------------------------------------
;;; GET-CHAR and LOOKAHEAD-CHAR for ports with string input buffer

  (define (get/lookahead-char-char-mode port who buffer-index-increment)
    ;;Subroutine of DO-GET-CHAR or  DO-PEEK-CHAR.  PORT must be a
    ;;textual  input port with  a Scheme  string as  buffer; such
    ;;buffer must  have been already fully  consumed.  Refill the
    ;;input buffer reading from  the underlying device and return
    ;;the next Scheme character from the buffer consuming it.
    ;;
    ;;When  BUFFER-INDEX-INCREMENT  is 1  this  function acts  as
    ;;GET-CHAR,   when  it   is   0  this   function  acts   like
    ;;LOOKAHEAD-CHAR.
    ;;
    ;;If EOF  is found while reading from  the underlying device:
    ;;return the EOF object.
    ;;
    (with-textual-port (port)
      (%debug-assert (fx= port.buffer.index port.buffer.used-size))
      (let* ((buffer.length	port.buffer.size)
	     (count		(port.read! port.buffer 0 buffer.length)))
	;;We enter this function with this scenario:
	;;
	;;            old cookie.pos
	;;                v
	;;  |-------------+-----------------------| device
	;;        |*******+---------| buffer
	;;                ^
	;;          index = used-size
	;;
	;;and  right  after  calling  READ!  we  want  the  following
	;;scenario:
	;;
	;;       old cookie.pos     new cookie.pos
	;;                v            v
	;;  |-------------+------------+----------| device
	;;               |*************+---| buffer
	;;                ^            ^
	;;              index       used-size
	;;
	;; count = new cookie.pos - old cookie.pos
	;;
	(unless (fixnum? count)
	  (die who "invalid return value from read!" count))
	(unless (<= 0 count buffer.length)
	  (die who "return value from read! is out of range" count))
	(port.device.position.incr! count)
	(set! port.buffer.used-size count)
	(set! port.buffer.index buffer-index-increment)
	(if (unsafe.fxzero? count)
	    (eof-object)
	  (string-ref port.buffer 0)))))

;;; --------------------------------------------------------------------
;;; Byte Order Mark (BOM) processing

  (define (advance-bom port who bom)
    ;;Read and  consume bytes from  PORT verifying if  they match
    ;;the given sequence of  bytes representing a Byte Order Mark
    ;;(BOM).
    ;;
    ;;PORT must  be an  input port with  a bytevector  as buffer.
    ;;BOM  must be a  list of  fixnums representing  the expected
    ;;Byte Order Mark sequence.
    ;;
    ;;Return #t if the whole BOM sequence is read and matched; in
    ;;this case  the port  position is left  right after  the BOM
    ;;sequence.
    ;;
    ;;Return #f if  the bytes from the port do  not match the BOM
    ;;sequence; in  this case  the port position  is left  at the
    ;;same point it was before this function call.
    ;;
    ;;Return the  EOF object if  the port reaches EOF  before the
    ;;whole BOM  is matched;  in this case  the port  position is
    ;;left at the same point it was before this function call.
    ;;
    (with-port (port)
      (let next-byte-in-bom ((number-of-consumed-bytes 0)
			     (bom bom))
	(if (null? bom)
	    ;;Full  success:  all  the  bytes in  the  given  BOM
	    ;;sequence where matched.
	    (begin
	      (port.buffer.index.incr! number-of-consumed-bytes)
	      #t)
	  (let retry-after-filling-buffer ()
	    (let ((buffer.offset (unsafe.fx+ number-of-consumed-bytes port.buffer.index)))
	      (%maybe-refill-bytevector-buffer-and-evaluate (port who)
		(data-is-needed-at: buffer.offset)
		(if-end-of-file: (eof-object))
		(if-successful-refill: (retry-after-filling-buffer))
		(if-available-data:
		 (and (unsafe.fx= (car bom) (unsafe.bytevector-u8-ref port.buffer buffer.offset))
		      (next-byte-in-bom (unsafe.fxadd1 number-of-consumed-bytes) (cdr bom)))))))))))

  (define (validate-port-then-parse-bom-and-add-fast-tag-to-untagged-port port who)
    ;;Validate PORT  as a still  open, textual, input  port; read
    ;;the Byte Order Mark  expected for the port's transcoder and
    ;;mutate the port's  attributes tagging the port accordingly.
    ;;Return #t if port is at EOF, #f otherwise.
    ;;
    (%assert-value-is-input-port port who)
    (%unsafe.assert-value-is-textual-port port who)
    (with-textual-port (port)
      (%unsafe.assert-value-is-open-port port who)
      (unless port.transcoder
	(die who "expected port with transcoder" port))
      (case (transcoder-codec port.transcoder)
	((utf-8-codec)
	 (set! port.attributes fast-get-utf8-tag)
	 (eof-object? (advance-bom port who '(#xEF #xBB #xBF))))
	((utf-16-codec)
	 (let ((big-endian? (advance-bom port who '(#xFE #xFF))))
	   (case big-endian?
	     ((#t)
	      (set! port.attributes fast-get-utf16be-tag)
	      #f)
	     ((#f)
	      (let ((little-endian? (advance-bom port who '(#xFF #xFE))))
		(case little-endian?
		  ((#t #f)
		   ;;If  no  BOM  is  present, we  select  little
		   ;;endian by default.
		   (set! port.attributes fast-get-utf16le-tag)
		   #f)
		  (else
		   (%debug-assert (eof-object? little-endian?))
		   #t))))
	     (else
	      (%debug-assert (eof-object? big-endian?))
	      #t))))
	(else
	 (die who "BUG: codec not handled" (transcoder-codec port.transcoder))))))

  #| end of module |# )


;;;; string input functions

(define (get-string-n p n)
  (import (ikarus system $fx)
    (ikarus system $strings))
  (define who 'get-string-n)
  (%assert-value-is-input-port p who)
  (%unsafe.assert-value-is-textual-port p who)
  (unless (fixnum? n)
    (die who "count is not a fixnum" n))
  (cond
   (($fx> n 0)
    (let ((s ($make-string n)))
      (let f ((p p) (n n) (s s) (i 0))
	(let ((x (get-char p)))
	  (cond
	   ((eof-object? x)
	    (if ($fx= i 0)
		(eof-object)
	      (substring s 0 i)))
	   (else
	    ($string-set! s i x)
	    (let ((i ($fxadd1 i)))
	      (if ($fx= i n)
		  s
		(f p n s i)))))))))
   (($fx= n 0) "")
   (else (die 'get-string-n "count is negative" n))))

(define (get-string-n! p s i c)
  (import (ikarus system $fx) (ikarus system $strings))
  (define who 'get-string-n!)
  (%assert-value-is-input-port p who)
  (%unsafe.assert-value-is-textual-port p who)
  (unless (string? s)
    (die who "not a string" s))
  (let ((len ($string-length s)))
    (unless (fixnum? i)
      (die 'get-string-n! "starting index is not a fixnum" i))
    (when (or ($fx< i 0) ($fx> i len))
      (die 'get-string-n!
	   (format "starting index is out of range 0..~a" len)
	   i))
    (unless (fixnum? c)
      (die 'get-string-n! "count is not a fixnum" c))
    (cond
     (($fx> c 0)
      (let ((j (+ i c)))
	(when (> j len)
	  (die 'get-string-n!
               (format "count is out of range 0..~a" (- len i))
               c))
	(let ((x (get-char p)))
	  (cond
	   ((eof-object? x) x)
	   (else
	    ($string-set! s i x)
	    (let f ((p p) (s s) (start i) (i 1) (c c))
	      (let ((x (get-char p)))
		(cond
		 ((eof-object? x) i)
		 (else
		  ($string-set! s ($fx+ start i) x)
		  (let ((i ($fxadd1 i)))
		    (if ($fx= i c)
			i
		      (f p s start i c))))))))))))
     (($fx= c 0) 0)
     (else (die 'get-string-n! "count is negative" c)))))

(define ($get-line p who)
  (define (get-it p)
    (let f ((p p) (n 0) (ac '()))
      (let ((x (get-char p)))
	(cond
	 ((eqv? x #\newline)
	  (make-it n ac))
	 ((eof-object? x)
	  (if (null? ac) x (make-it n ac)))
	 (else (f p (unsafe.fxadd1 n) (cons x ac)))))))
  (define (make-it n revls)
    (let f ((s (make-string n)) (i (unsafe.fxsub1 n)) (ls revls))
      (cond
       ((pair? ls)
	(string-set! s i (car ls))
	(f s (unsafe.fxsub1 i) (cdr ls)))
       (else s))))
  (%assert-value-is-input-port p who)
  (%unsafe.assert-value-is-textual-port p who)
  (get-it p))
(define (get-line p)
  ($get-line p 'get-line))
(define read-line
  (case-lambda
   (() ($get-line (current-input-port) 'read-line))
   ((p) ($get-line p 'read-line))))


(define (get-string-all p)
  (define who 'get-string-all)
  (define (get-it p)
    (let f ((p p) (n 0) (ac '()))
      (let ((x (get-char p)))
	(cond
	 ((eof-object? x)
	  (if (null? ac)
	      (eof-object)
	    (make-it n ac)))
	 (else (f p (+ n 1) (cons x ac)))))))
  (define (make-it n revls)
    (let f ((s (make-string n)) (i (- n 1)) (ls revls))
      (cond
       ((pair? ls)
	(string-set! s i (car ls))
	(f s (- i 1) (cdr ls)))
       (else s))))
  (%assert-value-is-input-port p who)
  (%unsafe.assert-value-is-textual-port p who)
  (get-it p))


;;;; unbuffered byte and char output

(define (%put-byte/unbuffered! port byte who)
  ;;Defined by  Ikarus.  Write directly  the single BYTE  to PORT
  ;;without  using  the  output  buffer.   The  return  value  is
  ;;unspecified.
  ;;
  ;;PORT must be a binary  output port.  BYTE must be an unsigned
  ;;fixnum in the range [0, 255].
  ;;
  ;;This function calls the  port's WRITE! function to output the
  ;;byte; if  it writes  no byte: the  port is marked  as closed.
  ;;FIXME Is this correct?
  ;;
  (with-binary-port (port)
    (%unsafe.assert-value-is-open-port port who)
    (let ((bv (make-bytevector 1)))
      (unsafe.bytevector-u8-set! bv 0 byte)
      (let ((count (port.write! bv 0 1)))
	;;We  need to  account for  the case  the  WRITE!  function
	;;returns  an  incorrect value,  for  example a  non-fixnum
	;;value.
	(cond ((eq? count 1)
	       (port.device.position.incr! 1))
	      ((eq? count 0)
	       (port.mark-as-closed)
	       (die who "could not write bytes to output port" port))
	      (else
	       (die who "invalid return value from write! proc" count port)))))))

(define (%put-char/unbuffered! port char who)
  ;;Defined by  Ikarus.  Write directly  the single CHAR  to PORT
  ;;without  using  the  output  buffer.   The  return  value  is
  ;;unspecified.
  ;;
  ;;PORT must  be a textual output  port.  CHAR must  be a Scheme
  ;;character.
  ;;
  ;;This function calls the  port's WRITE! function to output the
  ;;char; if  it writes  no char: the  port is marked  as closed.
  ;;FIXME Is this correct?
  ;;
  (with-textual-port (port)
    (%unsafe.assert-value-is-open-port port who)
    (let ((str (string char)))
      (let ((count (port.write! str 0 1)))
	;;We need  to account for  the case the  WRITE!  function
	;;returns  an incorrect value,  for example  a non-fixnum
	;;value.
	(cond ((eq? count 1)
	       (port.device.position.incr! 1))
	      ((eq? count 0)
	       (port.mark-as-closed)
	       (die who "could not write char to output port" port))
	      (else
	       (die who "invalid return value from write! proc" count port)))))))


;;;; byte and bytevector output

(define (put-u8 port byte)
  ;;Defined by  R6RS.  Write BYTE  to the output port  and return
  ;;unspecified values.
  ;;
  (define who 'put-u8)
  (unless (%u8? byte)
    (die who "not a u8" byte))
  (with-binary-port (port)
    (cond ((unsafe.fx= fast-put-byte-tag port.fast-attributes)
	   (let try-again-after-flushing-buffer ()
	     (if (unsafe.fx< port.buffer.index port.buffer.size)
		 ;;The buffer  exists and  it has room  for one
		 ;;byte.
		 (begin
		   (%debug-assert (<= port.buffer.index port.buffer.used-size))
		   (unsafe.bytevector-u8-set! port.buffer port.buffer.index byte)
		   (when (unsafe.fx= port.buffer.index port.buffer.used-size)
		     (port.buffer.used-size.incr!))
		   (port.buffer.index.incr!))
	       ;;The buffer  exists but  it has no  room: flush
	       ;;the buffer and try again.
	       (begin
		 (%debug-assert (= port.buffer.used-size port.buffer.index))
		 (%debug-assert (= port.buffer.used-size port.buffer.size))
		 (%unsafe.flush-output-port port who)
		 (try-again-after-flushing-buffer)))))
	  ((output-port? port)
	   (die who "not a binary port" port))
	  (else
	   (die who "not an output port" port)))))

(define put-bytevector
  ;;(put-bytevector port bv)
  ;;(put-bytevector port bv start)
  ;;(put-bytevector port bv start count)
  ;;
  ;;START and  COUNT must  be non-negative exact  integer objects
  ;;that default to 0 and:
  ;;
  ;; (- (bytevector-length BV) START)
  ;;
  ;;respectively.  BV must have a length of at least START+COUNT.
  ;;The PUT-BYTEVECTOR  procedure writes  the COUNT bytes  of the
  ;;bytevector  BV starting at  index START  to the  output port.
  ;;The PUT-BYTEVECTOR procedure returns unspecified values.
  ;;
  (put-string/bv 'put-bytevector "not a bytevector"
		 bytevector? bytevector-length %unsafe.put-bytevector))

(define (%unsafe.put-bytevector port src.bv src.start count)
  ;;Write COUNT  bytes from the  bytevector SRC.BV to  the binary
  ;;output PORT starting at offset SRC.START.  Return unspecified
  ;;values.
  ;;
  (define who 'put-bytevector)
  (with-binary-port (port)
    (cond ((unsafe.fx= fast-put-byte-tag port.fast-attributes)
	   ;;Write bytes to the buffer and, when the buffer fills up, to
	   ;;the underlying device.
	   (let try-again-after-flushing-buffer ((src.start	src.start)
						 (count		count)
						 (room		(port.buffer.room)))
	     (cond ((unsafe.fxzero? room)
		    ;;The buffer exists and it is full.
		    (%unsafe.flush-output-port port who)
		    (try-again-after-flushing-buffer src.start count (port.buffer.room)))
		   ((unsafe.fx<= count room)
		    ;;The  buffer exists  and  there is  enough
		    ;;room for all of the COUNT bytes.
		    (%unsafe.bytevector-copy! src.bv src.start port.buffer port.buffer.index count)
		    (port.buffer.index.incr! count)
		    (when (unsafe.fx< port.buffer.used-size port.buffer.index)
		      (set! port.buffer.used-size port.buffer.index)))
		   (else
		    ;;The  buffer exists and  it can  hold some
		    ;;but not all of the COUNT bytes.
		    (%debug-assert (> count room))
		    (%unsafe.bytevector-copy! src.bv src.start port.buffer port.buffer.index room)
		    (set! port.buffer.index     port.buffer.size)
		    (set! port.buffer.used-size port.buffer.index)
		    (%unsafe.flush-output-port port who)
		    (try-again-after-flushing-buffer (unsafe.fx+ src.start room)
						     (unsafe.fx- count room)
						     (port.buffer.room))))))
	  ((output-port? port)
	   (die who "not a binary port" port))
	  (else
	   (die who "not an output port" port)))))


;;;; string output

(define-syntax put-string/bv
  (syntax-rules ()
    ((_ who not-a-what pred? len $put)
     (case-lambda
      ((p bv)
       (if (pred? bv)
	   ($put p bv 0 (len bv))
	 (die who not-a-what bv)))
      ((p bv i)
       (if (pred? bv)
	   (if (fixnum? i)
	       (let ((n (len bv)))
		 (if (and (fx<= i n) (fx>= i 0))
		     ($put p bv i (fx- n i))
		   (die who "index out of range" i)))
	     (die who "invalid index" i))
	 (die who not-a-what bv)))
      ((p bv i c)
       (if (pred? bv)
	   (if (fixnum? i)
	       (let ((n (len bv)))
		 (if (and (fx<= i n) (fx>= i 0))
		     (if (fixnum? c)
			 (if (and (fx>= c 0) (fx>= (fx- n c) i))
			     ($put p bv i c)
			   (die who "count out of range" c))
		       (die who "invalid count" c))
		   (die who "index out of range" i)))
	     (die who "invalid index" i))
	 (die who not-a-what bv)))))))

(module (put-char write-char put-string)
  (define (put-byte! p b who)
    (let ((i ($port-index p)) (j ($port-size p)))
      (if (unsafe.fx< i j)
	  (begin
	    (bytevector-u8-set! ($port-buffer p) i b)
	    ($set-port-index! p (unsafe.fxadd1 i)))
	(if (unsafe.fxzero? j)
	    (%put-byte/unbuffered! p b who)
	  (begin
	    (%unsafe.flush-output-port p who)
	    (put-byte! p b who))))))
  (define (put-char-utf8-mode p b who)
    (cond
     ((unsafe.fx< b 128)
      (put-byte! p b who))
     ((unsafe.fx<= b #x7FF)
      (put-byte! p (%unsafe.fxior #b11000000 (unsafe.fxsra b 6)) who)
      (put-byte! p (%unsafe.fxior #b10000000 (unsafe.fxand b #b111111)) who))
     ((unsafe.fx<= b #xFFFF)
      (put-byte! p (%unsafe.fxior #b11100000 (unsafe.fxsra b 12)) who)
      (put-byte! p (%unsafe.fxior #b10000000 (unsafe.fxand (unsafe.fxsra b 6) #b111111)) who)
      (put-byte! p (%unsafe.fxior #b10000000 (unsafe.fxand b #b111111)) who))
     (else
      (put-byte! p (%unsafe.fxior #b11110000 (unsafe.fxsra b 18)) who)
      (put-byte! p (%unsafe.fxior #b10000000 (unsafe.fxand (unsafe.fxsra b 12) #b111111)) who)
      (put-byte! p (%unsafe.fxior #b10000000 (unsafe.fxand (unsafe.fxsra b 6) #b111111)) who)
      (put-byte! p (%unsafe.fxior #b10000000 (unsafe.fxand b #b111111)) who))))

  (define write-char
    (case-lambda
     ((c p) (do-put-char p c 'write-char))
     ((c) (do-put-char (current-output-port) c 'write-char))))

  (define (put-char p c)
    (do-put-char p c 'put-char))

  (define ($put-string p str start count)
    (define who 'put-string)
    (%assert-value-is-output-port p who)
    (%unsafe.assert-value-is-textual-port p who)
    (let f ((i start) (j (unsafe.fx+ start count)))
      (unless (unsafe.fx= i j)
	(do-put-char p (string-ref str i) 'put-string)
	(f (unsafe.fxadd1 i) j))))

  (define put-string
    (put-string/bv 'put-string "not a string"
		   string? string-length $put-string))

  (define (do-put-char p c who)
    (unless (char? c) (die who "not a char" c))
    (let ((m ($port-fast-attrs p)))
      (cond
       ((eq? m fast-put-utf8-tag)
	(let ((i ($port-index p)) (j ($port-size p)))
	  (let ((b (char->integer c)))
	    (cond
	     ((unsafe.fx< b 128)
	      (if (unsafe.fx< i j)
		  (begin
		    (bytevector-u8-set! ($port-buffer p) i b)
		    ($set-port-index! p (unsafe.fxadd1 i)))
		(if (unsafe.fxzero? j)
		    (%put-byte/unbuffered! p b who)
		  (begin
		    (%unsafe.flush-output-port p who)
		    (put-byte! p b who)))))
	     (else
	      (put-char-utf8-mode p b who))))))
       ((eq? m fast-put-char-tag)
	(let ((i ($port-index p)) (j ($port-size p)))
	  (if (unsafe.fx< i j)
	      (begin
		(string-set! ($port-buffer p) i c)
		($set-port-index! p (unsafe.fxadd1 i)))
	    (if (unsafe.fxzero? j)
		(%put-char/unbuffered! p c who)
	      (begin
		(%unsafe.flush-output-port p who)
		(do-put-char p c who))))))
       ((eq? m fast-put-latin-tag)
	(let ((i ($port-index p)) (j ($port-size p)))
	  (let ((b (char->integer c)))
	    (cond
	     ((unsafe.fx< b 256)
	      (if (unsafe.fx< i j)
		  (begin
		    (bytevector-u8-set! ($port-buffer p) i b)
		    ($set-port-index! p (unsafe.fxadd1 i)))
		(if (unsafe.fxzero? j)
		    (%put-byte/unbuffered! p b who)
		  (begin
		    (%unsafe.flush-output-port p who)
		    (put-byte! p b who)))))
	     (else
	      (case (transcoder-error-handling-mode (port-transcoder p))
		((ignore) (void))
		((replace) (do-put-char p #\? who))
		((raise)
		 (raise (make-i/o-encoding-error p c)))
		(else (die who "BUG: invalid error handling mode" p))))))))
       ((eq? m fast-put-utf16be-tag)
	(let ((n (char->integer c)))
	  (cond
	   ((unsafe.fx< n #x10000)
	    (put-byte! p (unsafe.fxsra n 8) who)
	    (put-byte! p (unsafe.fxand n #xFF) who))
	   (else
	    (let ((u^ (unsafe.fx- n #x10000)))
	      (let ((w1 (%unsafe.fxior #xD800 (unsafe.fxsra u^ 10))))
		(put-byte! p (unsafe.fxsra w1 8) who)
		(put-byte! p (unsafe.fxand w1 #xFF) who))
	      (let ((w2 (%unsafe.fxior #xDC00 (unsafe.fxand u^ (- (unsafe.fxsll 1 10) 1)))))
		(put-byte! p (unsafe.fxsra w2 8) who)
		(put-byte! p (unsafe.fxand w2 #xFF) who)))))))
       (else
	(%assert-value-is-output-port         p who)
	(%unsafe.assert-value-is-textual-port p who)
	(%unsafe.assert-value-is-open-port    p who)
	(die who "unsupported port" p)))))

  #| end of module |# )


(define newline
  (case-lambda
   (()
    (let ((port (current-output-port)))
      (put-char port #\newline)
      (%unsafe.flush-output-port port 'newline)))
   ((port)
    (define who 'newline)
    (%assert-value-is-output-port port who)
    (%unsafe.assert-value-is-textual-port port who)
    (%unsafe.assert-value-is-open-port port who)
    (put-char port #\newline)
    (%unsafe.flush-output-port port who))))


;;;; platform API for file descriptors
;;
;;See detailed documentation in the Texinfo file.
;;

(define-inline (platform-open-input-fd pathname-bv)
  ;;Interface to  "open()".  Open a file  descriptor for reading;
  ;;if successful  return a non-negative  fixnum representing the
  ;;file descriptor;  else return a  negative fixnum representing
  ;;an ERRNO code.
  ;;
  (foreign-call "ikrt_open_input_fd" pathname-bv))

(define-inline (platform-open-output-fd pathname-bv open-options)
  ;;Interface to  "open()".  Open a file  descriptor for writing;
  ;;if successful  return a non-negative  fixnum representing the
  ;;file descriptor;  else return a  negative fixnum representing
  ;;an ERRNO code.
  ;;
  (foreign-call "ikrt_open_output_fd" pathname-bv open-options))

(define-inline (platform-read-fd fd dst.bv dst.start requested-count)
  ;;Interface to  "read()".  Read data from  the file descriptor;
  ;;if successful  return a non-negative  fixnum representind the
  ;;number of bytes actually  read; else return a negative fixnum
  ;;representing an ERRNO code.
  ;;
  (foreign-call "ikrt_read_fd" fd dst.bv dst.start requested-count))

(define-inline (platform-write-fd fd src.bv src.start requested-count)
  ;;Interface to  "write()".  Write data to  the file descriptor;
  ;;if successful  return a non-negative  fixnum representind the
  ;;number  of bytes  actually  written; else  return a  negative
  ;;fixnum representing an ERRNO code.
  ;;
  (foreign-call "ikrt_write_fd" fd src.bv src.start requested-count))

(define-inline (platform-set-position fd position)
  (foreign-call "ikrt_set_position" fd position))

(define-inline (platform-close-fd fd)
  ;;Interface to "close()".  Close the file descriptor and return
  ;;false or a fixnum representing an ERRNO code.
  ;;
  (foreign-call "ikrt_close_fd" fd))


;;;; platform I/O error handling

;;; FIXME: these hard coded constants should go away
(define EAGAIN-error-code -6) ;;; from ikarus-errno.c

(define raise-io-error
  ;;Raise a non-continuable  exception describing an input/output
  ;;system error from the value of ERRNO.
  ;;
  (case-lambda
   ((who port-identifier errno base-condition)
    (raise (condition base-condition
		      (make-who-condition who)
		      (make-message-condition (strerror errno))
		      (case errno
			;; from ikarus-errno.c: EACCES=-2, EFAULT=-21, EROFS=-71, EEXIST=-20,
			;;                      EIO=-29, ENOENT=-45
			;; Why is EFAULT included here?
			((-2 -21)
			 (make-i/o-file-protection-error port-identifier))
			((-71)
			 (make-i/o-file-is-read-only-error port-identifier))
			((-20)
			 (make-i/o-file-already-exists-error port-identifier))
			((-29)
			 (make-i/o-error))
			((-45)
			 (make-i/o-file-does-not-exist-error port-identifier))
			(else
			 (if port-identifier
			     (make-irritants-condition (list port-identifier))
			   (condition)))))))
   ((who port-identifier errno)
    (raise-io-error who port-identifier errno (make-error)))))


;;;; helper functions for platform's descriptors

(define (%open-input-file-descriptor filename who)
  ;;Subroutine for the functions  below opening a file for input.
  ;;Open  and  return  a  file descriptor  referencing  the  file
  ;;selected by  the string FILENAME.  If an  error occurs: raise
  ;;an exception.
  ;;
  (let ((fd (platform-open-input-fd (string->utf8 filename))))
    (if (fx< fd 0)
	(raise-io-error who filename fd)
      fd)))

(define (%open-output-file-descriptor filename file-options who)
  ;;Subroutine for the functions below opening a file for output.
  ;;Open  and  return  a  file descriptor  referencing  the  file
  ;;selected by  the string FILENAME.  If an  error occurs: raise
  ;;an exception.
  ;;
  (let ((opts (if (enum-set? file-options)
		  (%unsafe.fxior (if (enum-set-member? 'no-create   file-options) 1 0)
				 (if (enum-set-member? 'no-fail     file-options) 2 0)
				 (if (enum-set-member? 'no-truncate file-options) 4 0))
		(die who "file-options is not an enum set" file-options))))
    (let ((fd (platform-open-output-fd (string->utf8 filename) opts)))
      (if (fx< fd 0)
	  (raise-io-error who filename fd)
	fd))))

(define (%file-descriptor->input-port fd port-identifier buffer.size transcoder close-function who)
  ;;Given the fixnum file descriptor FD representing an open file
  ;;for the underlying platform:  build and return a Scheme input
  ;;port to be used to read the data.
  ;;
  ;;The  returned   port  supports  both   the  GET-POSITION  and
  ;;SET-POSITION! operations.
  ;;
  ;;If  CLOSE-FUNCTION  is  a  function:  it  is  used  as  close
  ;;function; if it  is true: a standard close  function for file
  ;;descriptors is used; else the port does not support the close
  ;;function.
  ;;
  (let ((attributes		(%input-transcoder-attrs transcoder who))
	(buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector buffer.size))
	(write!			#f)
	(get-position		#t)
	(cookie			(default-cookie fd)))

    (define set-position!
      (%make-set-position!-function-for-file-descriptor-port fd port-identifier))

    (define close
      (cond ((procedure? close-function)
	     close-function)
	    ((eqv? close-function #t)
	     (%make-close-function-for-platform-descriptor-port port-identifier fd))
	    (else #f)))

    (define (read! dst.bv dst.start requested-count)
      (let ((count (platform-read-fd fd dst.bv dst.start buffer.size)))
	(cond ((unsafe.fx>= count 0)
	       count)
	      ((unsafe.fx= count EAGAIN-error-code)
	       (call/cc
		   (lambda (k)
		     (add-io-event fd k 'r)
		     (process-events)))
	       (read! dst.bv dst.start requested-count))
	      (else
	       (raise-io-error 'read! port-identifier count (make-i/o-read-error))))))

    (%port->guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer transcoder port-identifier
		 read! write! get-position set-position! close cookie))))

(define (%file-descriptor->output-port fd port-identifier buffer.size transcoder close-function who)
  ;;Given the fixnum file descriptor FD representing an open file
  ;;for the underlying platform: build and return a Scheme output
  ;;port to be used to write the data.
  ;;
  ;;The  returned   port  supports  both   the  GET-POSITION  and
  ;;SET-POSITION! operations.
  ;;
  ;;If  CLOSE-FUNCTION  is  a  function:  it  is  used  as  close
  ;;function; if it  is true: a standard close  function for file
  ;;descriptors is used; else the port does not support the close
  ;;operation.
  ;;
  (let ((attributes		(%output-transcoder-attrs transcoder who))
	(buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector buffer.size))
	(read!			#f)
	(get-position		#t)
	(cookie			(default-cookie fd)))

    (define set-position!
      (%make-set-position!-function-for-file-descriptor-port fd port-identifier))

    (define close
      (cond ((procedure? close-function)
	     close-function)
	    ((eqv? close-function #t)
	     (%make-close-function-for-platform-descriptor-port port-identifier fd))
	    (else #f)))

    (define (write! src.bv src.start requested-count)
      (let ((count (platform-write-fd fd src.bv src.start requested-count)))
	(cond ((unsafe.fx>= count 0)
	       count)
	      ((unsafe.fx= count EAGAIN-error-code)
	       (call/cc
		   (lambda (k)
		     (add-io-event fd k 'w)
		     (process-events)))
	       (write! src.bv src.start requested-count))
	      (else
	       (raise-io-error 'write! port-identifier requested-count (make-i/o-write-error))))))

    (%port->guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer transcoder port-identifier
		 read! write! get-position set-position! close cookie))))

(define (%make-set-position!-function-for-file-descriptor-port fd port-identifier)
  ;;Build  and  return a  closure  to  be  used as  SET-POSITION!
  ;;function for  a port wrapping the  platform's file descriptor
  ;;FD.
  ;;
  (lambda (position)
    (let ((errno (platform-set-position fd position)))
      (when errno
	(raise-io-error 'set-position! port-identifier errno
			(make-i/o-invalid-position-error position))))))

(define (%make-close-function-for-platform-descriptor-port port-identifier fd)
  ;;Return  a standard  CLOSE function  for a  port  wrapping the
  ;;platform's  descriptor   FD.   It  is  used   for  both  file
  ;;descriptors  and socket  descriptors, and  in general  can be
  ;;used for any platform descriptor.
  ;;
  (lambda ()
    (let ((errno (platform-close-fd fd)))
      (when errno
	(raise-io-error 'close port-identifier errno)))))


;;;; opening ports wrapping platform file descriptors

(define open-file-input-port
  ;;Defined by R6RS.   The OPEN-FILE-INPUT-PORT procedure returns
  ;;an  input port  for  the named  file.   The FILE-OPTIONS  and
  ;;MAYBE-TRANSCODER arguments are optional.
  ;;
  ;;The  FILE-OPTIONS  argument,   which  may  determine  various
  ;;aspects  of the  returned  port, defaults  to  the value  of:
  ;;
  ;;   (file-options)
  ;;
  ;;MAYBE-TRANSCODER must be either a transcoder or false.
  ;;
  ;;The  BUFFER-MODE argument, if  supplied, must  be one  of the
  ;;symbols that  name a  buffer mode.  The  BUFFER-MODE argument
  ;;defaults to BLOCK.
  ;;
  ;;If   MAYBE-TRANSCODER  is  a   transcoder,  it   becomes  the
  ;;transcoder associated with the returned port.
  ;;
  ;;If MAYBE-TRANSCODER  is false or  absent, the port will  be a
  ;;binary   port  and   will  support   the   PORT-POSITION  and
  ;;SET-PORT-POSITION!  operations.  Otherwise the port will be a
  ;;textual port,  and whether it supports  the PORT-POSITION and
  ;;SET-PORT-POSITION!   operations  is  implementation-dependent
  ;;(and possibly transcoder-dependent).
  ;;
  (case-lambda
   ((filename)
    (open-file-input-port filename (file-options) 'block #f))

   ((filename file-options)
    (open-file-input-port filename file-options   'block #f))

   ((filename file-options buffer-mode)
    (open-file-input-port filename file-options buffer-mode #f))

   ((filename file-options buffer-mode maybe-transcoder)
    (define who 'open-file-input-port)
    (unless (string? filename)
      (die who "invalid filename" filename))
    (unless (enum-set? file-options)
      (die who "file-options is not an enum set" file-options))
    (%assert-value-is-maybe-transcoder maybe-transcoder who)
;;;FIXME: file-options ignored
;;;FIXME: buffer-mode ignored
    (let ((fd			(%open-input-file-descriptor filename who))
	  (port-identifier	filename)
	  (buffer-size		(input-file-buffer-size))
	  (close-function	#t))
      (%file-descriptor->input-port fd port-identifier buffer-size maybe-transcoder
				    close-function who)))))

(define open-file-output-port
  ;;Defined by R6RS.  The OPEN-FILE-OUTPUT-PORT procedure returns
  ;;an output port for the named file.
  ;;
  ;;The  FILE-OPTIONS  argument,   which  may  determine  various
  ;;aspects of the returned port, defaults to the value of:
  ;;
  ;;   (file-options)
  ;;
  ;;MAYBE-TRANSCODER must be either a transcoder or false.
  ;;
  ;;The  BUFFER-MODE argument, if  supplied, must  be one  of the
  ;;symbols that  name a  buffer mode.  The  BUFFER-MODE argument
  ;;defaults to BLOCK.
  ;;
  ;;If   MAYBE-TRANSCODER  is  a   transcoder,  it   becomes  the
  ;;transcoder associated with the port.
  ;;
  ;;If MAYBE-TRANSCODER  is false or  absent, the port will  be a
  ;;binary   port  and   will  support   the   PORT-POSITION  and
  ;;SET-PORT-POSITION!  operations.  Otherwise the port will be a
  ;;textual port,  and whether it supports  the PORT-POSITION and
  ;;SET-PORT-POSITION!   operations  is implementation-dependent
  ;;(and possibly transcoder-dependent).
  ;;
  (case-lambda
   ((filename)
    (open-file-output-port filename (file-options) 'block #f))

   ((filename file-options)
    (open-file-output-port filename file-options 'block #f))

   ((filename file-options buffer-mode)
    (open-file-output-port filename file-options buffer-mode #f))

   ((filename file-options buffer-mode maybe-transcoder)
    (define who 'open-file-output-port)
    (unless (string? filename)
      (die who "invalid filename" filename))
;;;FIXME: file-options ignored
;;;FIXME: line-buffered output ports are not handled
    (%assert-value-is-maybe-transcoder maybe-transcoder who)
    (let ((buffer-size (case buffer-mode
			 ((none)
			  0)
			 ((block line)
			  (output-file-buffer-size))
			 (else
			  (die who "invalid buffer mode" buffer-mode)))))
      (%file-descriptor->output-port (%open-output-file-descriptor filename file-options who)
				     filename buffer-size maybe-transcoder #t who)))))

(define (open-output-file filename)
  ;;Defined by  R6RS.  Open FILENAME for output,  with empty file
  ;;options, and returns the obtained port.
  ;;
  (define who 'open-output-file)
  (unless (string? filename)
    (die who "invalid filename" filename))
  (%file-descriptor->output-port (%open-output-file-descriptor filename (file-options) who)
				 filename (output-file-buffer-size) (native-transcoder) #t who))

(define (open-input-file filename)
  ;;Defined by  R6RS.  Open FILENAME  for input, with  empty file
  ;;options, and returns the obtained port.
  ;;
  (define who 'open-input-file)
  (unless (string? filename)
    (die who "invalid filename" filename))
  (%file-descriptor->input-port (%open-input-file-descriptor filename who)
				filename (input-file-buffer-size) (native-transcoder) #t who))


(define (with-output-to-file filename thunk)
  ;;Defined by R6RS.   THUNK must be a procedure  and must accept
  ;;zero arguments.
  ;;
  ;;The  file is  opened for  input  or output  using empty  file
  ;;options, and THUNK is called with no arguments.
  ;;
  ;;During the dynamic extent of  the call to THUNK, the obtained
  ;;port   is  made   the   value  returned   by  the   procedure
  ;;CURRENT-OUTPUT-PORT; the previous default value is reinstated
  ;;when the dynamic extent is exited.
  ;;
  ;;When THUNK  returns, the  port is closed  automatically.  The
  ;;values returned by THUNK are returned.
  ;;
  ;;If an escape  procedure is used to escape  back into the call
  ;;to   THUNK  after   THUNK  is   returned,  the   behavior  is
  ;;unspecified.
  ;;
  (define who 'with-output-to-file)
  (unless (string? filename)
    (die who "invalid filename" filename))
  (%assert-value-is-procedure thunk who)
  (call-with-port
      (%file-descriptor->output-port (%open-output-file-descriptor filename (file-options) who)
				     filename (output-file-buffer-size) (native-transcoder)
				     #t who)
    (lambda (port)
      (parameterize ((current-output-port port))
	(thunk)))))

(define (with-input-from-file filename thunk)
  ;;Defined by R6RS.   THUNK must be a procedure  and must accept
  ;;zero arguments.
  ;;
  ;;The  file is  opened for  input  or output  using empty  file
  ;;options, and THUNK is called with no arguments.
  ;;
  ;;During the dynamic extent of  the call to THUNK, the obtained
  ;;port   is  made   the   value  returned   by  the   procedure
  ;;CURRENT-INPUT-PORT; the previous  default value is reinstated
  ;;when the dynamic extent is exited.
  ;;
  ;;When THUNK  returns, the  port is closed  automatically.  The
  ;;values returned by THUNK are returned.
  ;;
  ;;If an escape  procedure is used to escape  back into the call
  ;;to   THUNK  after   THUNK  is   returned,  the   behavior  is
  ;;unspecified.
  ;;
  (define who 'with-input-from-file)
  (unless (string? filename)
    (die who "invalid filename" filename))
  (%assert-value-is-procedure thunk who)
  (call-with-port
      (%file-descriptor->input-port (%open-input-file-descriptor filename who)
				    filename (input-file-buffer-size) (native-transcoder) #t who)
    (lambda (port)
      (parameterize ((current-input-port port))
	(thunk)))))

(define (call-with-output-file filename proc)
  ;;Defined by R6RS.  PROC should accept one argument.
  ;;
  ;;This procedure  opens the file named by  FILENAME for output,
  ;;with  no specified  file  options, and  calls  PROC with  the
  ;;obtained port as an argument.
  ;;
  ;;If  PROC returns, the  port is  closed automatically  and the
  ;;values returned by PROC are returned.
  ;;
  ;;If   PROC  does   not  return,   the  port   is   not  closed
  ;;automatically, unless  it is possible to prove  that the port
  ;;will never again be used for an I/O operation.
  ;;
  (define who 'call-with-output-file)
  (unless (string? filename)
    (die who "invalid filename" filename))
  (%assert-value-is-procedure proc who)
  (call-with-port
      (%file-descriptor->output-port (%open-output-file-descriptor filename (file-options) who)
				     filename (output-file-buffer-size) (native-transcoder) #t who)
    proc))

(define (call-with-input-file filename proc)
  ;;Defined by R6RS.  PROC should accept one argument.
  ;;
  ;;This procedure  opens the file  named by FILENAME  for input,
  ;;with  no specified  file  options, and  calls  PROC with  the
  ;;obtained port as an argument.
  ;;
  ;;If  PROC returns, the  port is  closed automatically  and the
  ;;values returned by PROC are returned.
  ;;
  ;;If   PROC  does   not  return,   the  port   is   not  closed
  ;;automatically, unless  it is possible to prove  that the port
  ;;will never again be used for an I/O operation.
  ;;
  (define who 'call-with-input-file)
  (unless (string? filename)
    (die who "invalid filename" filename))
  (%assert-value-is-procedure proc who)
  (call-with-port
      (%file-descriptor->input-port (%open-input-file-descriptor filename who)
				    filename (input-file-buffer-size) (native-transcoder) #t who)
    proc))

(define (with-input-from-string string thunk)
  ;;Defined by Ikarus.  THUNK must be a procedure and must accept
  ;;zero arguments.
  ;;
  ;;STRING must be  a Scheme string, and THUNK  is called with no
  ;;arguments.
  ;;
  ;;The  STRING is used  as argument  for OPEN-STRING-INPUT-PORT;
  ;;during the dynamic extent of  the call to THUNK, the obtained
  ;;port    is   made   the    value   returned    by   procedure
  ;;CURRENT-INPUT-PORT; the previous  default value is reinstated
  ;;when the dynamic extent is exited.
  ;;
  ;;When THUNK  returns, the  port is closed  automatically.  The
  ;;values returned by THUNK are returned.
  ;;
  ;;If an escape  procedure is used to escape  back into the call
  ;;to   THUNK  after   THUNK  is   returned,  the   behavior  is
  ;;unspecified.
  ;;
  (define who 'with-input-from-string)
  (unless (string? string)
    (die who "not a string" string))
  (%assert-value-is-procedure thunk who)
  (parameterize ((current-input-port (open-string-input-port string)))
    (thunk)))

(define (call-with-port port proc)
  ;;Defined  by  R6RS.   PROC  must  accept  one  argument.   The
  ;;CALL-WITH-PORT procedure calls PROC with PORT as an argument.
  ;;
  ;;If PROC returns, PORT  is closed automatically and the values
  ;;returned by PROC are returned.
  ;;
  ;;If PROC  does not return,  PORT is not  closed automatically,
  ;;except perhaps  when it is  possible to prove that  PORT will
  ;;never again be used for an input or output operation.
  ;;
  (define who 'call-with-port)
  (%assert-value-is-port port who)
  (%assert-value-is-procedure proc who)
  (call-with-values
      (lambda ()
	(proc port))
    (lambda vals
      (%unsafe.close-port port)
      (apply values vals))))


(define (process cmd . args)
  (%spawn-process 'process #t #t #f #f #f #f cmd args))

(define (process* search? env stdin stdout stderr cmd . args)
  (%spawn-process 'process* search? #t env stdin stdout stderr cmd args))

(define (process-nonblocking cmd . args)
  (%spawn-process 'process-nonblocking #t #f #f #f #f cmd args))

(define (%spawn-process who search? blocking? env stdin stdout stderr cmd args)
  (define (port->fd port port-pred arg-name port-type)
    (with-port (port)
      (cond ((and (boolean? port) (not port))
	     -1)
	    ((port-pred port)
	     (if port.device.is-descriptor?
		 port.device
	       (die who (string-append arg-name " is not a file-based port") stdin)))
	    (else
	     (die who (string-append arg-name " is neither false nor an " port-type) stdin)))))

  (define (pair->env-utf8 pair)
    (let* ((key-utf8 (string->utf8 (car pair)))
	   (val-utf8 (string->utf8 (cdr pair)))
	   (key-len (bytevector-length key-utf8))
	   (val-len (bytevector-length val-utf8))
	   (result (make-bytevector (+ key-len val-len 2))))
      (bytevector-copy! key-utf8 0 result 0 key-len)
      (bytevector-u8-set! result key-len (char->integer #\=))
      (bytevector-copy! val-utf8 0 result (+ key-len 1) val-len)
      (bytevector-u8-set! result (+ key-len val-len 1) 0)
      result))

  (let ((stdin-fd (port->fd stdin input-port? "stdin" "input port"))
	(stdout-fd (port->fd stdout output-port? "stdout" "output port"))
	(stderr-fd (port->fd stderr output-port? "stderr" "output port")))
    (unless (string? cmd)
      (die who "command is not a string" cmd))
    (unless (andmap string? args)
      (die who "all command arguments must be strings"))
    (let ((r (foreign-call "ikrt_process"
			   (vector search? stdin-fd stdout-fd stderr-fd)
			   (and env (map pair->env-utf8 env))
			   (string->utf8 cmd)
			   (map string->utf8 (cons cmd args)))))
      (cond ((fixnum? r)
	     (raise-io-error who cmd r))
	    (else
	     (unless blocking?
	       (or stdin (set-fd-nonblocking (vector-ref r 1) who cmd))
	       (or stdout (set-fd-nonblocking (vector-ref r 2) who cmd))
	       (or stderr (set-fd-nonblocking (vector-ref r 3) who cmd)))
	     (values
	      (vector-ref r 0)        ; pid
	      (and (not stdin)
		   (%file-descriptor->output-port (vector-ref r 1)
				    cmd (output-file-buffer-size) #f #t
				    'process))
	      (and (not stdout)
		   (%file-descriptor->input-port (vector-ref r 2)
				   cmd (input-file-buffer-size) #f #t
				   'process))
	      (and (not stderr)
		   (%file-descriptor->input-port (vector-ref r 3)
				   cmd (input-file-buffer-size) #f #t
				   'process))))))))


;;;; platform socket functions

(define (set-fd-nonblocking fd who id)
  (let ((rv (foreign-call "ikrt_make_fd_nonblocking" fd)))
    (unless (eq? rv 0)
      (raise-io-error who id rv))))

(define (socket->ports socket who id block?)
  (if (< socket 0)
      (raise-io-error who id socket)
    (let ((close
	   (let ((closed-once? #f))
	     (lambda ()
	       (if closed-once?
		   ((%make-close-function-for-platform-descriptor-port id socket))
		 (set! closed-once? #t))))))
      (unless block?
	(set-fd-nonblocking socket who id))
      (values
       (%file-descriptor->input-port  socket id (input-socket-buffer-size)  #f close who)
       (%file-descriptor->output-port socket id (output-socket-buffer-size) #f close who)))))

(define-syntax define-connector
  (syntax-rules ()
    ((_ who foreign-name block?)
     (define (who host srvc)
       (unless (and (string? host) (string? srvc))
	 (die 'who "host and service must both be strings" host srvc))
       (socket->ports
	(or (foreign-call foreign-name
			  (string->utf8 host) (string->utf8 srvc))
	    (die 'who "failed to resolve host name or connect" host srvc))
	'who
	(string-append host ":" srvc)
	block?)))))

(define-connector tcp-connect             "ikrt_tcp_connect" #t)
(define-connector udp-connect             "ikrt_udp_connect" #t)
(define-connector tcp-connect-nonblocking "ikrt_tcp_connect" #f)
(define-connector udp-connect-nonblocking "ikrt_udp_connect" #f)

(module (add-io-event rem-io-event process-events)
  (define-struct t (fd proc type))
    ;;; callbacks
  (define pending '())
  (define out-queue '())
  (define in-queue '())

  (define (process-events)
    (if (null? out-queue)
	(if (null? in-queue)
	    (if (null? pending)
		(error 'process-events "no more events")
	      (begin
		(do-select)
		(process-events)))
	  (begin
	    (set! out-queue (reverse in-queue))
	    (set! in-queue '())
	    (process-events)))
      (let ((t (car out-queue)))
	(set! out-queue (cdr out-queue))
	((t-proc t))
	(process-events))))

  (define (add-io-event fd proc event-type)
    (set! pending
	  (cons (make-t fd proc event-type) pending)))

  (define (rem-io-event fd)
    (define (p x) (eq? (t-fd x) fd))
    (set! pending (remp p pending))
    (set! out-queue (remp p out-queue))
    (set! in-queue (remp p in-queue)))

  (define (get-max-fd)
    (assert (pair? pending))
    (let f ((m (t-fd (car pending)))
	    (ls (cdr pending)))
      (cond
       ((null? ls) m)
       (else (f (max m (t-fd (car ls))) (cdr ls))))))

  (define (do-select)
    (let ((n (add1 (get-max-fd))))
      (let ((vecsize (div (+ n 7) 8)))
	(let ((rbv (make-bytevector vecsize 0))
	      (wbv (make-bytevector vecsize 0))
	      (xbv (make-bytevector vecsize 0)))
            ;;; add all fds to their bytevectors depending on type
	  (for-each
              (lambda (t)
                (let ((fd (t-fd t)))
                  (let ((i (div fd 8)) (j (mod fd 8)))
                    (let ((bv (case (t-type t)
                                ((r) rbv)
                                ((w) wbv)
                                ((x) xbv)
                                (else
                                 (error 'do-select "invalid type" t)))))
                      (bytevector-u8-set! bv i
					  (fxlogor (fxsll 1 j)
						   (bytevector-u8-ref bv i)))))))
	    pending)
            ;;; do select
	  (let ((rv (foreign-call "ikrt_select" n rbv wbv xbv)))
	    (when (< rv 0)
	      (raise-io-error 'select #f rv)))
            ;;; go through fds again and see if they're selected
	  (for-each
              (lambda (t)
                (let ((fd (t-fd t)))
                  (let ((i (div fd 8)) (j (mod fd 8)))
                    (let ((bv (case (t-type t)
                                ((r) rbv)
                                ((w) wbv)
                                ((x) xbv)
                                (else
                                 (error 'do-select "invalid type" t)))))
                      (cond
		       ((fxzero?
			 (fxlogand (fxsll 1 j)
				   (bytevector-u8-ref bv i)))
                         ;;; not selected
			(set! pending (cons t pending)))
		       (else
                         ;;; ready
			(set! in-queue (cons t in-queue))))))))
	    (let ((ls pending))
	      (set! pending '())
	      ls))))))
  #| end of module |# )


(define-struct tcp-server (portnum fd))

(define (tcp-server-socket portnum)
  (unless (fixnum? portnum)
    (error 'tcp-server-socket "not a fixnum" portnum))
  (let ((sock (foreign-call "ikrt_listen" portnum)))
    (cond
     ((fx>= sock 0) (make-tcp-server portnum sock))
     (else (die 'tcp-server-socket "failed to start server")))))

(define (tcp-server-socket-nonblocking portnum)
  (let ((s (tcp-server-socket portnum)))
    (set-fd-nonblocking (tcp-server-fd s)
			'tcp-server-socket-nonblocking
			'#f)
    s))


(define (do-accept-connection s who blocking?)
  (define (make-socket-info x)
    (unless (= (bytevector-length x) 16)
      (error who "BUG: unexpected return value" x))
    (format "~s.~s.~s.~s:~s"
      (bytevector-u8-ref x 4)
      (bytevector-u8-ref x 5)
      (bytevector-u8-ref x 6)
      (bytevector-u8-ref x 7)
      (+ (* 256 (bytevector-u8-ref x 2))
	 (bytevector-u8-ref x 3))))
  (unless (tcp-server? s)
    (die who "not a tcp server" s))
  (let ((fd (tcp-server-fd s)) (bv (make-bytevector 16)))
    (unless fd
      (die who "server is closed" s))
    (let ((sock (foreign-call "ikrt_accept" fd bv)))
      (cond
       ((eq? sock EAGAIN-error-code)
	(call/cc
	    (lambda (k)
	      (add-io-event fd k 'r)
	      (process-events)))
	(do-accept-connection s who blocking?))
       ((< sock 0)
	(raise-io-error who s sock))
       (else
	(socket->ports sock who (make-socket-info bv) blocking?))))))

(define (accept-connection s)
  (do-accept-connection s 'accept-connection #t))

(define (accept-connection-nonblocking s)
  (do-accept-connection s 'accept-connection-nonblocking #f))

(define (close-tcp-server-socket s)
  (define who 'close-tcp-server-socket)
  (unless (tcp-server? s)
    (die who "not a tcp server" s))
  (let ((fd (tcp-server-fd s)))
    (unless fd
      (die who "server is closed" s))
    (let ((rv (foreign-call "ikrt_shutdown" fd)))
      (when (fx< rv 0)
	(die who "failed to shutdown")))))

(define (reset-input-port! p)
  (%assert-value-is-input-port p 'reset-input-port!)
  ($set-port-index! p ($port-size p))
  (unregister-callback p))

(define (reset-output-port! p)
  (%assert-value-is-output-port p 'reset-output-port!)
  ($set-port-index! p 0)
  (unregister-callback p))

(define (unregister-callback what)
  (define who 'unregister-callback)
  (cond ((or (output-port? what)
	     (input-port?  what))
	 (with-port (what)
	   (let ((c what.device))
	     (unless (fixnum? c)
	       (die who "not a file-based port" what))
	     (rem-io-event c))))
	((tcp-server? what)
	 (rem-io-event (tcp-server-fd what)))
	(else (die who "invalid argument" what))))

(define (register-callback what proc)
  (define who 'register-callback)
  (%assert-value-is-procedure proc who)
  (cond ((output-port? what)
	 (with-port (what)
	   (let ((c what.device))
	     (unless (fixnum? c)
	       (die who "not a file-based port" what))
	     (add-io-event c proc 'w))))
	((input-port? what)
	 (with-port (what)
	 (let ((c what.device))
	   (unless (fixnum? c)
	     (die who "not a file-based port" what))
	   (add-io-event c proc 'r))))
	((tcp-server? what)
	 (add-io-event (tcp-server-fd what) proc 'r))
	(else (die who "invalid argument" what))))


(module (directory-stream? open-directory-stream
			   read-directory-stream close-directory-stream)

  (define-struct directory-stream (filename pointer closed?))

  (define G (make-guardian))

  (define (clean-up)
    (cond
     ((G) =>
      (lambda (x)
	(close-directory-stream x #f)
	(clean-up)))))

  (define (open-directory-stream filename)
    (define who 'open-directory-stream)
    (unless (string? filename)
      (die who "not a string" filename))
    (clean-up)
    (let ((rv (foreign-call "ikrt_opendir" (string->utf8 filename))))
      (if (fixnum? rv)
	  (raise-io-error who filename rv)
	(let ((stream (make-directory-stream filename rv #f)))
	  (G stream)
	  stream))))

  (define (read-directory-stream x)
    (define who 'read-directory-stream)
    (unless (directory-stream? x)
      (die who "not a directory stream" x))
    (when (directory-stream-closed? x)
      (die who "directory stream is closed" x))
    (let ((rv (foreign-call "ikrt_readdir"
			    (directory-stream-pointer x))))
      (cond
       ((fixnum? rv)
	(close-directory-stream x #f)
	(raise-io-error who (directory-stream-filename x) rv))
       ((not rv) #f)
       (else (utf8->string rv)))))

  (define close-directory-stream
    (case-lambda
     ((x wanterror?)
      (define who 'close-directory-stream)
      (clean-up)
      (unless (directory-stream? x)
	(die who "not a directory stream" x))
      (unless (directory-stream-closed? x)
	(set-directory-stream-closed?! x #t)
	(let ((rv (foreign-call "ikrt_closedir"
				(directory-stream-pointer x))))
	  (when (and wanterror? (not (eqv? rv 0)))
	    (raise-io-error who (directory-stream-filename x) rv)))))
     ((x) (close-directory-stream x #t))))

  (set-rtd-printer! (type-descriptor directory-stream)
		    (lambda (x p wr)
		      (fprintf p "#<directory-stream ~a>"
			       (directory-stream-filename x)))))


		;(set-fd-nonblocking 0 'init '*stdin*)


;;;; standard, console and current ports

(define (standard-input-port)
  ;;Defined by R6RS.  Return a new binary input port connected to
  ;;standard input.  Whether  the port supports the PORT-POSITION
  ;;and   SET-PORT-POSITION!     operations   is   implementation
  ;;dependent.
  ;;
  (%file-descriptor->input-port  0 "*stdin*" 256 #f #f 'standard-input-port))

(define (standard-output-port)
  ;;Defined by  R6RS.  Return a new binary  output port connected
  ;;to  the  standard  output.   Whether the  port  supports  the
  ;;PORT-POSITION    and   SET-PORT-POSITION!     operations   is
  ;;implementation dependent.
  ;;
  (%file-descriptor->output-port 1 "*stdout*" 256 #f #f 'standard-output-port))

(define (standard-error-port)
  ;;Defined by  R6RS.  Return a new binary  output port connected
  ;;to  the  standard  error.   Whether  the  port  supports  the
  ;;PORT-POSITION    and   SET-PORT-POSITION!     operations   is
  ;;implementation dependent.
  ;;
  (%file-descriptor->output-port 2 "*stderr*" 256 #f #f 'standard-error-port))

(define current-input-port
  ;;Defined by  R6RS.  Return a  default textual port  for input.
  ;;Normally,  this  default  port  is associated  with  standard
  ;;input,   but  can   be  dynamically   reassigned   using  the
  ;;WITH-INPUT-FROM-FILE procedure from  the (rnrs io simple (6))
  ;;library.   The  port  may  or  may  not  have  an  associated
  ;;transcoder;  if  it does,  the  transcoder is  implementation
  ;;dependent.
  (make-parameter
      (transcoded-port (%file-descriptor->input-port 0 "*stdin*" (input-file-buffer-size) #f #f #f)
		       (native-transcoder))
    (lambda (x)
      (define who 'current-input-port)
      (%assert-value-is-input-port x who)
      (%unsafe.assert-value-is-textual-port x who)
      x)))

(define current-output-port
  ;;Defined by  R6RS.  Hold the default textual  port for regular
  ;;output.   Normally,  this  default  port is  associated  with
  ;;standard output.
  ;;
  ;;The  return value of  CURRENT-OUTPUT-PORT can  be dynamically
  ;;reassigned using  the WITH-OUTPUT-TO-FILE procedure  from the
  ;;(rnrs  io  simple (6))  library.   A  port  returned by  this
  ;;procedure may or may not have an associated transcoder; if it
  ;;does, the transcoder is implementation dependent.
  ;;
  (make-parameter
      (transcoded-port (%file-descriptor->output-port 1 "*stdout*" (output-file-buffer-size) #f #f #f)
		       (native-transcoder))
    (lambda (x)
      (define who 'current-output-port)
      (%assert-value-is-output-port x who)
      (%unsafe.assert-value-is-textual-port x who)
      x)))

(define current-error-port
  ;;Defined  by R6RS.  Hold  the default  textual port  for error
  ;;output.   Normally,  this  default  port is  associated  with
  ;;standard error.
  ;;
  ;;The  return value  of CURRENT-ERROR-PORT  can  be dynamically
  ;;reassigned using  the WITH-OUTPUT-TO-FILE procedure  from the
  ;;(rnrs  io  simple (6))  library.   A  port  returned by  this
  ;;procedure may or may not have an associated transcoder; if it
  ;;does, the transcoder is implementation dependent.
  ;;
  (make-parameter
      (transcoded-port (%file-descriptor->output-port 2 "*stderr*" 0 #f #f #f)
		       (native-transcoder))
    (lambda (x)
      (define who 'current-error-port)
      (%assert-value-is-output-port x who)
      (%unsafe.assert-value-is-textual-port x who)
      x)))

(define console-output-port
  ;;Defined by Ikarus.  Return a default textual port for output;
  ;;each call returns the same port.
  ;;
  (let ((port (current-output-port)))
    (lambda () port)))

(define console-error-port
  ;;Defined by Ikarus.  Return  a default textual port for error;
  ;;each call returns the same port.
  ;;
  (let ((port (current-error-port)))
    (lambda () port)))

(define console-input-port
  ;;Defined by Ikarus.  Return  a default textual port for input;
  ;;each call returns the same port.
  ;;
  (let ((port (current-input-port)))
    (lambda () port)))


;;;; done

)

;;; end of file
;;; Local Variables:
;;; fill-column: 72
;;; eval: (put 'with-port				'scheme-indent-function 1)
;;; eval: (put 'with-binary-port			'scheme-indent-function 1)
;;; eval: (put 'with-textual-port			'scheme-indent-function 1)
;;; eval: (put 'case-textual-input-port-fast-tag	'scheme-indent-function 1)
;;; eval: (put '%refill-bytevector-buffer-and-evaluate		'scheme-indent-function 1)
;;; eval: (put '%maybe-refill-bytevector-buffer-and-evaluate	'scheme-indent-function 1)
;;; End:
