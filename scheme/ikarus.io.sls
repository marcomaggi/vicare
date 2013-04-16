;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;
;;;Abstract
;;;
;;;	Define and  export all the  I/O functions mandated by  R6RS plus
;;;	some  implementation-specific  functions.   See  also  the  file
;;;	"ikarus.codecs.ss" for transcoders and codecs.
;;;
;;;	The functions  and macros prefixed  with "%" and  "unsafe."  are
;;;	not exported.   The functions  and macros prefixed  "unsafe." or
;;;	"%unsafe."   assume   that  the  arguments   have  already  been
;;;	validated.  The functions  prefixed "unsafe."  are imported from
;;;	another library.
;;;
;;;	This file tries to stick  to this convention: "byte" is a fixnum
;;;	in the range  [-128, 127], "octet" is a fixnum  in the range [0,
;;;	255].
;;;
;;;	NOTE The  primitive operations  on a port  value are  defined in
;;;	"pass-specify-rep-primops.ss"; a port value is a block of memory
;;;	allocated as a  vector whose first word is  tagged with the port
;;;	tag.
;;;
;;;Copyright (c) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006,2007,2008  Abdulaziz Ghuloum
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; The port data structure
;;
;;It  is  defined   in  "pass-specify-rep-primops.ss";  its  allocation,
;;accessors and mutators are all defined as primitive operations inlined
;;by the compiler.
;;
;;Constructor: $make-port ATTRS IDX SZ BUF TR ID READ WRITE GETP SETP CL COOKIE
;;Aguments: IDX		- index in input/output buffer,
;;	    SZ		- number of octets/chars used in the input/output buffer,
;;          BUF		- input/output buffer,
;;          TR		- transcoder
;;          ID		- a Scheme string describing the underlying device
;;          READ	- read procedure
;;          WRITE	- write procedure
;;          GETP	- get-position procedure
;;          SETP	- set-position procedure
;;          CL		- close procedure
;;          COOKIE	- device, position, line and column tracking
;;  Build a new port structure and return a Scheme value referencing it.
;;  Notice  that the  underlying  device is  not  among the  constructor
;;  arguments: it is  stored in the cookie and  implicitly referenced by
;;  the functions READ, WRITE, GETP, SETP, CL.
;;
;;
;;Fields of a port block
;;----------------------
;;
;;Field name: tag
;;Field accessor: $port-tag PORT
;;  Extract  from  a  port  reference  a fixnum  representing  the  port
;;  attributes.  If  PORT is  not a port  reference the return  value is
;;  zero.
;;
;;Field accessor: $port-attrs PORT
;;  Extract  from  a  port  reference  a fixnum  representing  the  port
;;  attributes.  PORT must be a  port reference, otherwise the result is
;;  unspecified.
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
;;  For an input port: it is the  offset in the input buffer of the next
;;  location to be read from.
;;
;;Field name: size
;;Field accessor: $port-size PORT
;;Field mutator: $set-port-size! PORT SIZE
;;  Fixnum representing the number of octets/chars currently used in the
;;  input/output buffer; see the description of the BUFFER field below.
;;
;;  When the  device is a Scheme  string or bytevector  being itself the
;;  device: this field is set to  the number of characters in the string
;;  or the number of octets in the bytevector.
;;
;;Field name: buffer
;;Field accessor: $port-buffer PORT
;;  The input/output  buffer for the  port.  The buffer is  allocated at
;;  port  construction time  and  never reallocated.   The  size of  the
;;  buffers is customisable through a set of parameters.
;;
;;  For the  logic of the functions to  work: it is mandatory  to have a
;;  buffer at  least wide  enough to hold  2 characters with  the widest
;;  serialisation in  bytes.  This is because:  it is possible  to put a
;;  transcoder on  top of every  binary port; we  need a place  to store
;;  partially read  or written characters;  2 characters can be  read at
;;  once when doing end-of-line (EOL) conversion.
;;
;;  Built in  binary port  types have a  bytevector as buffer;  built in
;;  textual port  types may have  a bytevector as  buffer if they  use a
;;  transcoder, or a string as  buffer.  Custom binary port types have a
;;  bytevector as  buffer; custom  textual port types  have a  string as
;;  buffer.
;;
;;  As  special cases: the  port returned  by OPEN-BYTEVECTOR-INPUT-PORT
;;  has the  source bytevector  itself as buffer,  the port  returned by
;;  OPEN-STRING-INPUT-PORT has the source string itself as buffer.
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
;;                        v
;;	   |--------------+---------------------------|device
;;                        |*****+*******+--------|buffer
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
;;                                      v
;;	   |----------------------------+-------------|device
;;                      |*******+*******+--------|buffer
;;                      ^       ^       ^        ^
;;                      0     index  used-size  size
;;
;;  When the  port is  input and  the buffer is  itself the  device: the
;;  device position is perpetually set  to the buffer size.  The current
;;  port position is computed with:
;;
;;	   port position = device position - used-size + index = index
;;
;;                                              device position
;;                                                    v
;;	   |------------------------------------------|device
;;         |*******+**********************************|buffer
;;         ^               ^                          ^
;;         0             index                used-size = size
;;
;;  When the  port is output  and the buffer  is itself the  device: the
;;  device  position  is perpetually  set  to  zero.   The current  port
;;  position is computed with:
;;
;;	   port position = device position + index = index
;;
;;     device position
;;         v
;;	   |------------------------------------------|device
;;         |*******+**********************************|buffer
;;         ^               ^                          ^
;;         0             index                used-size = size
;;
;;
;;Field name: transcoder
;;Field accessor: $port-transcoder PORT
;;  A value  of disjoint type  returned by MAKE-TRANSCODER.   It encodes
;;  the  Unicode  codec, the  end-of-line  conversion  sytle, the  error
;;  handling modes for errors in the serialisation of characters.
;;
;;Field name: id
;;Field accessor: $port-id
;;  A  Scheme  string describing  the  underlying  device.   For a  port
;;  associated to  a file: it is  a Scheme string  representing the file
;;  name given to functions like OPEN-OUTPUT-FILE.
;;
;;Field name: read!
;;Field accessor: $port-read! PORT
;;  Fill buffer policy.
;;
;;  When  the value  is a  procedure,  it must  be a  function with  the
;;  following signature:
;;
;;	READ! DST DST.START COUNT
;;
;;  DST.START will  be a non-negative  fixnum, COUNT will be  a positive
;;  fixnum and  DST will be  a bytevector or  string whose length  is at
;;  least  DST.START+COUNT.  The  procedure  should obtain  up to  COUNT
;;  bytes  or characters  from the  device,  and should  write them  DST
;;  starting at index DST.START.   The procedure should return a fixnum.
;;  representing the number of bytes it  has read; to indicate an end of
;;  file, the procedure should write no bytes and return 0.
;;
;;  When the value is the Scheme symbol ALL-DATA-IN-BUFFER: the port has
;;  no underlying device, the buffer itself is the device.
;;
;;  When the value is #f: the port is output-only.
;;
;;Field name: write!
;;Field accessor: $port-write! PORT
;;  Flush buffer policy.
;;
;;  When  the value  is a  procedure,  it must  be a  function with  the
;;  following signature:
;;
;;	WRITE! SRC SRC.START COUNT
;;
;;  SRC.START and COUNT will be  non-negative fixnums, and SRC will be a
;;  bytevector or string whose length is at least SRC.START+COUNT.
;;
;;  The procedure  should write up to  COUNT bytes from  SRC starting at
;;  index SRC.START  to the device.   In any case, the  procedure should
;;  return a fixnum representing the number of bytes it wrote.
;;
;;  When the value is #f: the port is input-only.
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
;;  for  all the  custom  ports  having a  device.   When acquiring  the
;;  position fails: the procedure must raise an exception.
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
;;  zero or the buffer size.
;;
;;     NOTE    At     present    only    the     ports    returned    by
;;     OPEN-BYTEVECTOR-INPUT-PORT  and OPEN-STRING-INPUT-PORT  have this
;;     policy, so SET-PORT-POSITION!  is  optimised for this case (Marco
;;     Maggi; Sep 21, 2011).
;;
;;  -  The  value  is a  procedure  when  the  underlying device  has  a
;;  position.   When acquiring  the position  fails: the  procedure must
;;  raise an exception.
;;
;;  While  buffer arithmetics  is  handled exclusively  by fixnums,  the
;;  device position and the port position is represented by non-negative
;;  exact integers (fixnums or bignums).
;;
;;Field name: close
;;Field accessor: $port-close PORT
;;  Close device procedure.  It accepts no arguments.
;;
;;Field name: cookie
;;Field accessor: $port-cookie PORT
;;Field mutator: $set-port-cookie PORT COOKIE
;;  A record of  type COOKIE used to keep a  reference to the underlying
;;  device and  track its current  position.  Additionally it  can track
;;  line and column numbers in textual input ports.
;;


;;;; On buffering
;;
;;Given the implementation restrictions:
;;
;;*  Strings  have  length  at   most  equal  to  the  return  value  of
;;GREATEST-FIXNUM.
;;
;;*  Bytevectors  have length  at  most equal  to  the  return value  of
;;GREATEST-FIXNUM.
;;
;;we establish the following constraints:
;;
;;*  If an  exact integer  is in  the range  representable by  a fixnum,
;;Vicare will represent it as a fixnum.
;;
;;* No matter which BUFFER-MODE was selected, every port has a buffer.
;;
;;* The buffer is a Scheme bytevector or a Scheme string.
;;
;;* The input functions always read data from the buffer first.
;;
;;* The output functions always write data to the buffer first.
;;
;;*  %UNSAFE.REFILL-INPUT-PORT-BYTEVECTOR-BUFFER  is  the only  function
;;calling the  port's READ!  function  for ports having a  bytevector as
;;buffer; it copies data from the underlying device to the input buffer.
;;
;;* %UNSAFE.REFILL-INPUT-PORT-STRING-BUFFER is the only function calling
;;the port's  READ!  function  for ports having  a string as  buffer; it
;;copies data from the underlying device to the input buffer.
;;
;;* %UNSAFE.FLUSH-OUTPUT-PORT  is the  only function calling  the port's
;;WRITE! function for  both ports having a string  or bytevector buffer;
;;if copies data from the output buffer to the underlying device.
;;
;; ---------------------------------------------------------------------
;;
;;From the constraints it follows that:
;;
;;*  All the  arithmetics involving  the buffer  can be  performed using
;;unsafe fixnum functions.
;;
;; ---------------------------------------------------------------------
;;
;;The buffer mode is customisable only for output operations; the buffer
;;mode is ignored by input ports.  Buffer mode handling is as follows:
;;
;;* When the  mode is NONE: data is first written  to the output buffer,
;;then immediately sent to the underlying device.
;;
;;* When the mode is LINE: data is first written to the output buffer up
;;to the first newline, then immediately sent to the underlying device.
;;
;;* When the mode is BLOCK:  data is first written to the output buffer;
;;only when the buffer is full data is sent to the underlying device.
;;


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
    port? input-port? output-port? input/output-port?
    textual-port? binary-port?
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
    get-string-n get-string-n! get-string-all get-line read-line

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

    ;; networking
    make-binary-socket-input/output-port
    make-binary-socket-input/output-port*
    make-textual-socket-input/output-port
    make-textual-socket-input/output-port*)
  (import (except (ikarus)
		  ;; port parameters
		  standard-input-port standard-output-port standard-error-port
		  current-input-port  current-output-port  current-error-port
		  console-output-port console-error-port   console-input-port
		  bytevector-port-buffer-size	string-port-buffer-size
		  input-file-buffer-size	output-file-buffer-size
		  input/output-file-buffer-size	input/output-socket-buffer-size

		  ;; predicates
		  port? input-port? output-port? input/output-port?
		  textual-port? binary-port?
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
		  get-string-n get-string-n! get-string-all get-line read-line

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

		  ;; networking
		  make-binary-socket-input/output-port
		  make-binary-socket-input/output-port*
		  make-textual-socket-input/output-port
		  make-textual-socket-input/output-port*)
    ;;This internal  library is  the one exporting:  $MAKE-PORT, $PORT-*
    ;;and $SET-PORT-* bindings.
    (ikarus system $io)
    (prefix (only (ikarus) port?) primop.)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe operations)
	    unsafe.)
    (prefix (vicare unsafe-capi)
	    capi.)
    (vicare unsafe-unicode)
    (vicare platform constants))


;;;; port attributes
;;
;;Each  port has  a tag  retrievable with  the $PORT-TAG  or $PORT-ATTRS
;;primitive  operations.   All  the   tags  have  24  bits.   The  least
;;significant  14 bits  are reserved  as  "fast attributes"  and can  be
;;independently extracted as a fixnum used by the macros:
;;
;;   %CASE-TEXTUAL-INPUT-PORT-FAST-TAG
;;   %CASE-TEXTUAL-OUTPUT-PORT-FAST-TAG
;;   %CASE-BINARY-INPUT-PORT-FAST-TAG
;;   %CASE-BINARY-OUTPUT-PORT-FAST-TAG
;;
;;to  quickly select  code to  run for  I/O operations  on a  port.  The
;;following bitpatterns  are used to  compose the fast  attributes; note
;;that bits  9, 10,  11 and  12 are currently  unused and  available for
;;additional codecs.  Notice that the fixnum  0 is not a valid fast tag,
;;this fact can be used to speed up a little dispatching of evaluation.
;;
;;These  values  must  be  kept  in   sync  with  the  ones  defined  in
;;"ikarus.compiler.sls".
;;
;;					  32109876543210
;;                type bits                         ||||
;;                codec bits                   |||||
;;                unused codec bits        ||||
;;                true-if-closed bit      |
;;					  32109876543210
(define INPUT-PORT-TAG			#b00000000000001)
(define OUTPUT-PORT-TAG			#b00000000000010)
(define TEXTUAL-PORT-TAG		#b00000000000100)
(define BINARY-PORT-TAG			#b00000000001000)
(define FAST-CHAR-TEXT-TAG		#b00000000010000)
(define FAST-U8-TEXT-TAG		#b00000000100000)
(define FAST-LATIN-TEXT-TAG		#b00000001000000)
(define FAST-U16BE-TEXT-TAG		#b00000010000000)
(define FAST-U16LE-TEXT-TAG		#b00000100000000)
(define INIT-U16-TEXT-TAG		#b00000110000000)
(define CLOSED-PORT-TAG			#b10000000000000)

;;The following bitpatterns are  used for additional attributes ("other"
;;attributes in the code).
;;
;;Notice that there  is no BUFFER-MODE-BLOCK-TAG bit: only  for LINE and
;;NONE  buffering  something  must   be  done.   BLOCK  buffer  mode  is
;;represented by setting to zero the 2 block mode bits.
;;
;;                                        321098765432109876543210
;;                      non-fast-tag bits ||||||||||
;;  true if port is both input and output          |
;;                       buffer mode bits        ||
;;        true if port is in the guardian       |
;;   true if port has an extract function      |
;;                         EOL style bits   |||
;;     true if the device is a file desc.  |
;;                            unused bits |
;;                                        321098765432109876543210
(define INPUT/OUTPUT-PORT-TAG		#b000000000100000000000000)
		;Used to tag ports that are both input and output.
(define BUFFER-MODE-NONE-TAG		#b000000001000000000000000)
		;Used to tag ports having NONE as buffer mode.
(define BUFFER-MODE-LINE-TAG		#b000000010000000000000000)
		;Used to tag ports having LINE as buffer mode.
(define GUARDED-PORT-TAG		#b000000100000000000000000)
		;Used  to tag  ports which  must be  closed by  the port
		;guardian   after  being   collected   by  the   garbage
		;collector.  See the definition of PORT-GUARDIAN.
(define PORT-WITH-EXTRACTION-TAG	#b000001000000000000000000)
		;Used to tag binary  output ports which accumulate bytes
		;to be later retrieved by an extraction function.  These
		;ports need special treatement when a transcoded port is
		;created on top of them.  See TRANSCODED-PORT.
(define PORT-WITH-FD-DEVICE		#b010000000000000000000000)
		;Used  to  tag ports  that  have  a  file descriptor  as
		;device.

;;                                                321098765432109876543210
(define EOL-STYLE-MASK				#b001110000000000000000000)
(define EOL-STYLE-NOT-MASK			#b110001111111111111111111)
(define EOL-LINEFEED-TAG			#b000010000000000000000000) ;;symbol -> lf
(define EOL-CARRIAGE-RETURN-TAG			#b000100000000000000000000) ;;symbol -> cr
(define EOL-CARRIAGE-RETURN-LINEFEED-TAG	#b000110000000000000000000) ;;symbol -> crlf
(define EOL-NEXT-LINE-TAG			#b001000000000000000000000) ;;symbol -> nel
(define EOL-CARRIAGE-RETURN-NEXT-LINE-TAG	#b001010000000000000000000) ;;symbol -> crnel
(define EOL-LINE-SEPARATOR-TAG			#b001100000000000000000000) ;;symbol -> ls
		;Used  to  tag  textual  ports  with  end-of-line  (EOL)
		;conversion style.

(define DEFAULT-OTHER-ATTRS			#b000000000000000000000000)
		;Default  non-fast attributes:  non-guarded  port, block
		;buffer mode, no extraction function, EOL style none.

;;If we are just interested in the port type: input or output, binary or
;;textual bits, we can do:
;;
;;  (let ((type-bits (unsafe.fxand ($port-attrs port) PORT-TYPE-MASK)))
;;    (unsafe.fx= type-bits *-PORT-BITS))
;;
;;or:
;;
;;  (let ((type-bits (unsafe.fxand ($port-attrs port) *-PORT-BITS)))
;;    (unsafe.fx= type-bits *-PORT-BITS))
;;
;;where *-PORT-BITS  is one  of the constants  below.  Notice  that this
;;predicate is not influenced by the fact that the port is closed.
;;
;;					  32109876543210
(define PORT-TYPE-MASK			#b00000000001111)
(define BINARY-INPUT-PORT-BITS		(unsafe.fxior BINARY-PORT-TAG  INPUT-PORT-TAG))
(define BINARY-OUTPUT-PORT-BITS		(unsafe.fxior BINARY-PORT-TAG  OUTPUT-PORT-TAG))
(define TEXTUAL-INPUT-PORT-BITS		(unsafe.fxior TEXTUAL-PORT-TAG INPUT-PORT-TAG))
(define TEXTUAL-OUTPUT-PORT-BITS	(unsafe.fxior TEXTUAL-PORT-TAG OUTPUT-PORT-TAG))

;;The following  tag constants allow  fast classification of  open input
;;ports by doing:
;;
;;   (unsafe.fx= ($port-fast-attrs port) FAST-GET-*-TAG)
;;
;;where FAST-GET-*-TAG  is one of  the constants below.  Notice  that if
;;the port is closed the predicate will fail because the return value of
;;$PORT-FAST-ATTRS includes the true-if-closed bit.

;;This one is  used for binary input ports,  having a bytevector buffer,
;;from which raw octets must be read.
(define FAST-GET-BYTE-TAG        BINARY-INPUT-PORT-BITS)
;;
;;This one is used for textual input ports, having a string buffer.
(define FAST-GET-CHAR-TAG	(unsafe.fxior FAST-CHAR-TEXT-TAG  TEXTUAL-INPUT-PORT-BITS))
;;
;;The following  are used for  textual input ports, having  a bytevector
;;buffer and a  transcoder, from which characters in  some encoding must
;;be read.
(define FAST-GET-UTF8-TAG	(unsafe.fxior FAST-U8-TEXT-TAG    TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;UTF-8 transcoder.
(define FAST-GET-LATIN-TAG	(unsafe.fxior FAST-LATIN-TEXT-TAG TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;Latin-1 transcoder.
(define FAST-GET-UTF16BE-TAG	(unsafe.fxior FAST-U16BE-TEXT-TAG TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;UTF-16 big endian transcoder.
(define FAST-GET-UTF16LE-TAG	(unsafe.fxior FAST-U16LE-TEXT-TAG TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;UTF-16 little endian transcoder.
(define INIT-GET-UTF16-TAG	(unsafe.fxior INIT-U16-TEXT-TAG TEXTUAL-INPUT-PORT-BITS))
		;Tag for textual input  ports with bytevector buffer and
		;UTF-16 transcoder  not yet recognised as  little or big
		;endian:  endianness selection  is performed  by reading
		;the Byte Order Mark (BOM) at the beginning of the input
		;data.

;;The following  tag constants allow fast classification  of open output
;;ports by doing:
;;
;;   (unsafe.fx= ($port-fast-attrs port) FAST-PUT-*-TAG)
;;
;;where FAST-PUT-*-TAG  is one of  the constants below.  Notice  that if
;;the port is closed the predicate will fail because the return value of
;;$PORT-FAST-ATTRS includes the true-if-closed bit.

;;This one is used for binary output ports, having bytevector buffer, to
;;which raw bytes must be written.
(define FAST-PUT-BYTE-TAG	BINARY-OUTPUT-PORT-BITS)
;;
;;This one is used for textual output ports, having a string buffer.
(define FAST-PUT-CHAR-TAG	(unsafe.fxior FAST-CHAR-TEXT-TAG  TEXTUAL-OUTPUT-PORT-BITS))
;;
;;The following are  used for textual output ports,  having a bytevector
;;buffer and a transcoder, to  which characters in some encoding must be
;;written.
(define FAST-PUT-UTF8-TAG	(unsafe.fxior FAST-U8-TEXT-TAG    TEXTUAL-OUTPUT-PORT-BITS))
		;Tag for textual output ports with bytevector buffer and
		;UTF-8 transcoder.
(define FAST-PUT-LATIN-TAG	(unsafe.fxior FAST-LATIN-TEXT-TAG TEXTUAL-OUTPUT-PORT-BITS))
		;Tag for textual output ports with bytevector buffer and
		;Latin-1 transcoder.
(define FAST-PUT-UTF16BE-TAG	(unsafe.fxior FAST-U16BE-TEXT-TAG TEXTUAL-OUTPUT-PORT-BITS))
		;Tag for textual output ports with bytevector buffer and
		;UTF-16 big endian transcoder.
(define FAST-PUT-UTF16LE-TAG	(unsafe.fxior FAST-U16LE-TEXT-TAG TEXTUAL-OUTPUT-PORT-BITS))
		;Tag for textual output ports with bytevector buffer and
		;UTF-16 little endian transcoder.
(define INIT-PUT-UTF16-TAG	(unsafe.fxior FAST-PUT-UTF16BE-TAG FAST-PUT-UTF16LE-TAG))
		;Tag for textual output ports with bytevector buffer and
		;UTF-16 transcoder  with data  not yet recognised  to be
		;little  or  big endian:  selection  is performed  after
		;writing  the  Byte Order  Mark  (BOM).   FIXME This  is
		;currently not supported.

;;; --------------------------------------------------------------------

(define-syntax %select-input-fast-tag-from-transcoder
  ;;When using  this macro without specifying the  other attributes: the
  ;;default attributes are automatically included.  When we specify some
  ;;non-fast attribute, it is  our responsibility to specify the default
  ;;attributes if we want them included.
  ;;
  (syntax-rules ()
    ((_ ?who ?transcoder)
     (%%select-input-fast-tag-from-transcoder ?who ?transcoder DEFAULT-OTHER-ATTRS))
    ((_ ?who ?transcoder ?other-attribute)
     (%%select-input-fast-tag-from-transcoder ?who ?transcoder ?other-attribute))
    ((_ ?who ?transcoder ?other-attribute . ?other-attributes)
     (%%select-input-fast-tag-from-transcoder ?who ?transcoder
					      (unsafe.fxior ?other-attribute . ?other-attributes)))))

(define (%%select-input-fast-tag-from-transcoder who maybe-transcoder other-attributes)
  ;;Return  a fixnum  containing the  tag attributes  for an  input port
  ;;using   MAYBE-TRANSCODER.   OTHER-ATTRIBUTES   must   be  a   fixnum
  ;;representing the  non-fast attributes  to compose with  the selected
  ;;fast attributes.
  ;;
;;;NOTE We  cannot put assertions  here because this function  is called
;;;upon initialisation  of the library  when the standard port  have not
;;;yet  been  fully constructed.   A  failing  assertion  would use  the
;;;standard ports, causing a segfault.
  (if (not maybe-transcoder)
      (unsafe.fxior other-attributes FAST-GET-BYTE-TAG)
    (case (transcoder-codec maybe-transcoder)
      ((utf-8-codec)	(unsafe.fxior other-attributes FAST-GET-UTF8-TAG))
      ((latin-1-codec)	(unsafe.fxior other-attributes FAST-GET-LATIN-TAG))
      ((utf-16le-codec)	(unsafe.fxior other-attributes FAST-GET-UTF16LE-TAG))
      ((utf-16be-codec)	(unsafe.fxior other-attributes FAST-GET-UTF16BE-TAG))
      ;;The      selection     between      FAST-GET-UTF16LE-TAG     and
      ;;FAST-GET-UTF16BE-TAG is performed as part of the Byte Order Mark
      ;;(BOM) reading operation when the first char is read.
      ((utf-16-codec)	(unsafe.fxior other-attributes INIT-GET-UTF16-TAG))
      ;;If no codec  is recognised, wait to read  the first character to
      ;;tag the port according to the Byte Order Mark.
      (else		(unsafe.fxior other-attributes TEXTUAL-INPUT-PORT-BITS)))))

(define-syntax %select-output-fast-tag-from-transcoder
  ;;When using  this macro without specifying the  other attributes: the
  ;;default attributes are automatically included.  When we specify some
  ;;non-fast attribute, it is  our responsibility to specify the default
  ;;attributes if we want them included.
  ;;
  (syntax-rules ()
    ((_ ?who ?transcoder)
     (%%select-output-fast-tag-from-transcoder ?who ?transcoder DEFAULT-OTHER-ATTRS))
    ((_ ?who ?transcoder ?other-attribute)
     (%%select-output-fast-tag-from-transcoder ?who ?transcoder ?other-attribute))
    ((_ ?who ?transcoder ?other-attribute . ?other-attributes)
     (%%select-output-fast-tag-from-transcoder ?who ?transcoder
					       (unsafe.fxior ?other-attribute . ?other-attributes)))))

(define (%%select-output-fast-tag-from-transcoder who maybe-transcoder other-attributes)
  ;;Return a  fixnum containing  the tag attributes  for an  output port
  ;;using   MAYBE-TRANSCODER.   OTHER-ATTRIBUTES   must   be  a   fixnum
  ;;representing the  non-fast attributes  to compose with  the selected
  ;;fast attributes.
  ;;
  ;;Notice  that  an output  port  is  never  tagged as  UTF-16  without
  ;;selection  of  endianness; by  default  big  endianness is  selected
  ;;because  it seems  to be  mandated  by the  Unicode Consortium,  see
  ;;<http://unicode.org/faq/utf_bom.html> question: "Why  do some of the
  ;;UTFs have a BE or LE in their label, such as UTF-16LE?"
  ;;
;;;NOTE We  cannot put assertions  here because this function  is called
;;;upon initialisation  of the library  when the standard port  have not
;;;yet  been  fully constructed.   A  failing  assertion  would use  the
;;;standard ports, causing a segfault.
  (if (not maybe-transcoder)
      (unsafe.fxior other-attributes FAST-PUT-BYTE-TAG)
    (case (transcoder-codec maybe-transcoder)
      ((utf-8-codec)	(unsafe.fxior other-attributes FAST-PUT-UTF8-TAG))
      ((latin-1-codec)	(unsafe.fxior other-attributes FAST-PUT-LATIN-TAG))
      ((utf-16le-codec)	(unsafe.fxior other-attributes FAST-PUT-UTF16LE-TAG))
      ((utf-16be-codec)	(unsafe.fxior other-attributes FAST-PUT-UTF16BE-TAG))
      ;;By default we select big endian UTF-16.
      ((utf-16-codec)	(unsafe.fxior other-attributes FAST-PUT-UTF16BE-TAG))
      (else
       (assertion-violation who "unsupported codec" (transcoder-codec maybe-transcoder))))))

(define-syntax %select-input/output-fast-tag-from-transcoder
  ;;Return  a fixnum  containing the  tag  attributes for  an input  and
  ;;output port.
  (syntax-rules ()
    ((_ . ?args)
     (%select-output-fast-tag-from-transcoder . ?args))))

;;; --------------------------------------------------------------------

(define-inline ($mark-port-closed! ?port)
  ;;Set the CLOSED?  bit to 1 in the attributes or PORT.
  ;;
  ($set-port-attrs! ?port (unsafe.fxior CLOSED-PORT-TAG ($port-attrs ?port))))

;;This  mask  is used  to  nullify  the buffer  mode  bits  in a  fixnum
;;representing port attributes.
;;
;;					  321098765432109876543210
(define BUFFER-MODE-NOT-MASK		#b111111100111111111111111)

(define-inline ($set-port-buffer-mode-to-block! ?port)
  ($set-port-attrs! ?port (unsafe.fxand BUFFER-MODE-NOT-MASK ($port-attrs ?port))))

(define-inline ($set-port-buffer-mode-to-none! ?port)
  ($set-port-attrs! ?port (unsafe.fxior (unsafe.fxand BUFFER-MODE-NOT-MASK ($port-attrs ?port))
					 BUFFER-MODE-NONE-TAG)))

(define-inline ($set-port-buffer-mode-to-line! ?port)
  ($set-port-attrs! ?port (unsafe.fxior (unsafe.fxand BUFFER-MODE-NOT-MASK ($port-attrs ?port))
					 BUFFER-MODE-LINE-TAG)))

;;; --------------------------------------------------------------------

;;                                 321098765432109876543210
(define FAST-ATTRS-MASK          #b000000000011111111111111)
(define OTHER-ATTRS-MASK         #b111111111100000000000000)

(define-inline ($port-fast-attrs port)
  ;;Extract the fast attributes from the tag of PORT.
  ;;
  (unsafe.fxand ($port-attrs port) FAST-ATTRS-MASK))

(define-inline ($port-other-attrs port)
  ;;Extract the non-fast attributes from the tag of PORT.
  ;;
  (unsafe.fxand ($port-attrs port) OTHER-ATTRS-MASK))

(define-inline ($set-port-fast-attrs! port fast-attrs)
  ;;Store new fast attributes in the tag of PORT.
  ;;
  ($set-port-attrs! port (unsafe.fxior ($port-other-attrs port) fast-attrs)))

(define-inline ($port-fast-attrs-or-zero obj)
  ;;Given  a Scheme  value:  if it  is  a port  value  extract the  fast
  ;;attributes and return  them, else return zero.  Notice  that zero is
  ;;not a valid fast tag.
  ;;
  (unsafe.fxand ($port-tag obj) FAST-ATTRS-MASK))


;;; --------------------------------------------------------------------

(define NEWLINE-CODE-POINT		#x000A) ;; U+000A
(define LINEFEED-CODE-POINT		#x000A) ;; U+000A
(define CARRIAGE-RETURN-CODE-POINT	#x000D) ;; U+000D
(define NEXT-LINE-CODE-POINT		#x0085) ;; U+0085
(define LINE-SEPARATOR-CODE-POINT	#x2028) ;; U+2028

(define NEWLINE-CHAR			#\x000A) ;; U+000A
(define LINEFEED-CHAR			#\x000A) ;; U+000A
(define CARRIAGE-RETURN-CHAR		#\x000D) ;; U+000D
(define NEXT-LINE-CHAR			#\x0085) ;; U+0085
(define LINE-SEPARATOR-CHAR		#\x2028) ;; U+2028

(define (%symbol->eol-attrs style)
  (case style
    ((none)	0)
    ((lf)	EOL-LINEFEED-TAG)
    ((cr)	EOL-CARRIAGE-RETURN-TAG)
    ((crlf)	EOL-CARRIAGE-RETURN-LINEFEED-TAG)
    ((nel)	EOL-NEXT-LINE-TAG)
    ((crnel)	EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
    ((ls)	EOL-LINE-SEPARATOR-TAG)
    (else	#f)))

(define (%select-eol-style-from-transcoder who maybe-transcoder)
  ;;Given a  transcoder return the non-fast  attributes representing the
  ;;selected end of line conversion style.
  ;;
;;;NOTE We  cannot put assertions  here because this function  is called
;;;upon initialisation  of the library  when the standard port  have not
;;;yet  been  fully constructed.   A  failing  assertion  would use  the
;;;standard ports, causing a segfault.
  (if (not maybe-transcoder)
      0		;EOL style none
    (let ((style (transcoder-eol-style maybe-transcoder))
	  (codec (transcoder-codec     maybe-transcoder)))
      (define (%unsupported-by-latin-1)
	(assertion-violation who
	  "EOL style conversion unsupported by Latin-1 codec" style))
      (case-symbols style
	((none)		0)
	((lf)		EOL-LINEFEED-TAG)
	((cr)		EOL-CARRIAGE-RETURN-TAG)
	((crlf)		EOL-CARRIAGE-RETURN-LINEFEED-TAG)
	((nel)		(cond ((eqv? codec (latin-1-codec))
			       (%unsupported-by-latin-1))
			      (else
			       EOL-NEXT-LINE-TAG)))
	((crnel)	(cond ((eqv? codec (latin-1-codec))
			       (%unsupported-by-latin-1))
			      (else
			       EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)))
	((ls)		(cond ((eqv? codec (latin-1-codec))
			       (%unsupported-by-latin-1))
			      (else
			       EOL-LINE-SEPARATOR-TAG)))
	(else
	 (assertion-violation who
	   "vicare internal error: invalid EOL style extracted from transcoder" style))))))

(define-inline (%unsafe.port-eol-style-bits ?port)
  (unsafe.fxand EOL-STYLE-MASK ($port-attrs ?port)))

(define-inline (%unsafe.port-nullify-eol-style-bits attributes)
  (unsafe.fxand EOL-STYLE-NOT-MASK attributes))

(define-syntax %case-eol-style
  ;;Select a body to be evaluated  if a port has the selected EOL style.
  ;;?EOL-BITS must be an identifier bound to the result of:
  ;;
  ;;   (%unsafe.port-eol-style-bits port)
  ;;
  (lambda (stx)
    (syntax-case stx ( ;;
		      EOL-LINEFEED-TAG			EOL-CARRIAGE-RETURN-TAG
		      EOL-CARRIAGE-RETURN-LINEFEED-TAG	EOL-NEXT-LINE-TAG
		      EOL-CARRIAGE-RETURN-NEXT-LINE-TAG	EOL-LINE-SEPARATOR-TAG
		      else)
      ((%case-eol-style (?eol-bits ?who)
	 ((EOL-LINEFEED-TAG)			. ?linefeed-body)
	 ((EOL-CARRIAGE-RETURN-TAG)		. ?carriage-return-body)
	 ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)	. ?carriage-return-linefeed-body)
	 ((EOL-NEXT-LINE-TAG)			. ?next-line-body)
	 ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)	. ?carriage-return-next-line-body)
	 ((EOL-LINE-SEPARATOR-TAG)		. ?line-separator-body)
	 (else					. ?none-body))
       (and (identifier? #'?eol-bits)
	    (identifier? #'?who))
       #'($case-fixnums ?eol-bits
	   ((0)					. ?none-body)
	   ((EOL-LINEFEED-TAG)			. ?linefeed-body)
	   ((EOL-CARRIAGE-RETURN-TAG)		. ?carriage-return-body)
	   ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)	. ?carriage-return-linefeed-body)
	   ((EOL-NEXT-LINE-TAG)			. ?next-line-body)
	   ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)	. ?carriage-return-next-line-body)
	   ((EOL-LINE-SEPARATOR-TAG)		. ?line-separator-body))
       ))))

;;; --------------------------------------------------------------------

(define-syntax* (%case-binary-input-port-fast-tag stx)
  ;;Assuming ?PORT  has already been  validated as a port  value, select
  ;;code to be evaluated  if it is a binary input port.   If the port is
  ;;I/O and tagged  as binary output: retag it as  binary input.  If the
  ;;port is textual, output or closed: raise an assertion violation.
  ;;
  (syntax-case stx (FAST-GET-BYTE-TAG else)
    ((%case-binary-input-port-fast-tag (?port ?who)
       ((FAST-GET-BYTE-TAG) . ?byte-tag-body))
     (and (identifier? #'?port)
	  (identifier? #'?who))
     #'(let retry-after-tagging ((m ($port-fast-attrs-or-zero ?port)))
	 (cond ((unsafe.fx= m FAST-GET-BYTE-TAG)
		(begin . ?byte-tag-body))
	       ((and (port? ?port)
		     (%unsafe.input-and-output-port? ?port)
		     (unsafe.fx= m FAST-PUT-BYTE-TAG))
		(%unsafe.reconfigure-output-buffer-to-input-buffer ?port ?who)
		($set-port-fast-attrs! ?port FAST-GET-BYTE-TAG)
		(retry-after-tagging FAST-GET-BYTE-TAG))
	       (else
		(%assert-argument-is-port ?port ?who)
		(%unsafe.assert-value-is-input-port  ?port ?who)
		(%unsafe.assert-value-is-binary-port ?port ?who)
		(%unsafe.assert-value-is-open-port   ?port ?who)
		(assertion-violation ?who "vicare internal error: corrupted port" ?port)))))))

(define-syntax* (%case-binary-output-port-fast-tag stx)
  ;;Assuming ?PORT  has already been  validated as a port  value, select
  ;;code to be evaluated if it is  a binary output port.  If the port is
  ;;I/O and tagged  as binary input: retag it as  binary output.  If the
  ;;port is textual, input or closed: raise an assertion violation.
  ;;
  (syntax-case stx (FAST-PUT-BYTE-TAG else)
    ((%case-binary-input-port-fast-tag (?port ?who)
       ((FAST-PUT-BYTE-TAG) . ?byte-tag-body))
     (and (identifier? #'?port)
	  (identifier? #'?who))
     #'(let retry-after-tagging ((m ($port-fast-attrs-or-zero ?port)))
	 (cond ((unsafe.fx= m FAST-PUT-BYTE-TAG)
		(begin . ?byte-tag-body))
	       ((and (port? ?port)
		     (%unsafe.input-and-output-port? ?port)
		     (unsafe.fx= m FAST-GET-BYTE-TAG))
		(%unsafe.reconfigure-input-buffer-to-output-buffer ?port ?who)
		($set-port-fast-attrs! ?port FAST-PUT-BYTE-TAG)
		(retry-after-tagging FAST-PUT-BYTE-TAG))
	       (else
		(%assert-argument-is-port ?port ?who)
		(%unsafe.assert-value-is-output-port ?port ?who)
		(%unsafe.assert-value-is-binary-port ?port ?who)
		(%unsafe.assert-value-is-open-port   ?port ?who)
		(assertion-violation ?who "vicare internal error: corrupted port" ?port)))))))

(define-syntax* (%case-textual-input-port-fast-tag stx)
  ;;For  a port  fast tagged  for input:  select a  body of  code  to be
  ;;evaluated.
  ;;
  ;;If the port is tagged for output and it is an I/O port: retag it for
  ;;input and select a body of code to be evaluated.
  ;;
  ;;If the  port is  untagged: validate it  as open textual  input port,
  ;;then try to  tag it reading the Byte Order  Mark.  If the validation
  ;;fails:  raise an  assertion violation.   If reading  the  BOM fails:
  ;;raise an exception of type &i/o-read.  If the port is at EOF: return
  ;;the EOF object.
  ;;
  (syntax-case stx ( ;;
		    FAST-GET-UTF8-TAG FAST-GET-CHAR-TAG FAST-GET-LATIN-TAG
		    FAST-GET-UTF16LE-TAG FAST-GET-UTF16BE-TAG)
    ((%case-textual-input-port-fast-tag (?port ?who)
       ((FAST-GET-UTF8-TAG)	. ?utf8-tag-body)
       ((FAST-GET-CHAR-TAG)	. ?char-tag-body)
       ((FAST-GET-LATIN-TAG)	. ?latin-tag-body)
       ((FAST-GET-UTF16LE-TAG)	. ?utf16le-tag-body)
       ((FAST-GET-UTF16BE-TAG)	. ?utf16be-tag-body))
     (and (identifier? #'?port)
	  (identifier? #'?who))
     #'(let retry-after-tagging-port ((m ($port-fast-attrs-or-zero ?port)))
	 (define (%validate-and-tag)
	   (with-arguments-validation (?who)
	       ((port ?port)
		(unsafe.input-port   ?port)
		(unsafe.textual-port ?port)
		(unsafe.open-port    ?port))
	     (%parse-bom-and-add-fast-tag (?who ?port)
	       (if-successful-match:
		(retry-after-tagging-port ($port-fast-attrs ?port)))
	       (if-end-of-file: (eof-object))
	       (if-no-match-raise-assertion-violation))))
	 (define (%reconfigure-as-input fast-attrs)
	   (%unsafe.reconfigure-output-buffer-to-input-buffer ?port ?who)
	   ($set-port-fast-attrs! ?port fast-attrs)
	   (retry-after-tagging-port fast-attrs))
	 ($case-fixnums m
	   ((FAST-GET-UTF8-TAG)		. ?utf8-tag-body)
	   ((FAST-GET-CHAR-TAG)		. ?char-tag-body)
	   ((FAST-GET-LATIN-TAG)	. ?latin-tag-body)
	   ((FAST-GET-UTF16LE-TAG)	. ?utf16le-tag-body)
	   ((FAST-GET-UTF16BE-TAG)	. ?utf16be-tag-body)
	   ((INIT-GET-UTF16-TAG)
	    (if (%unsafe.parse-utf16-bom-and-add-fast-tag ?who ?port)
		(eof-object)
	      (retry-after-tagging-port ($port-fast-attrs ?port))))
	   ((FAST-GET-BYTE-TAG)
	    (assertion-violation ?who "expected textual port" ?port))
	   (else
	    (if (and (port? ?port)
		     (%unsafe.input-and-output-port? ?port))
		($case-fixnums m
		  ((FAST-PUT-UTF8-TAG)
		   (%reconfigure-as-input FAST-GET-UTF8-TAG))
		  ((FAST-PUT-CHAR-TAG)
		   (%reconfigure-as-input FAST-GET-CHAR-TAG))
		  ((FAST-PUT-LATIN-TAG)
		   (%reconfigure-as-input FAST-GET-LATIN-TAG))
		  ((FAST-PUT-UTF16LE-TAG)
		   (%reconfigure-as-input FAST-GET-UTF16LE-TAG))
		  ((FAST-PUT-UTF16BE-TAG)
		   (%reconfigure-as-input FAST-GET-UTF16BE-TAG))
		  ((FAST-PUT-BYTE-TAG)
		   (assertion-violation ?who "expected textual port" ?port))
		  (else
		   (%validate-and-tag)))
	      (%validate-and-tag))))
	 ))))

(define-syntax* (%case-textual-output-port-fast-tag stx)
  ;;For  a port fast  tagged for  output: select  a body  of code  to be
  ;;evaluated.
  ;;
  ;;If the port is tagged for input  and it is an I/O port: retag it for
  ;;output and select a body of code to be evaluated.
  ;;
  ;;If the  port is untagged: validate  it as open  textual output port.
  ;;If the validation fails: raise an assertion violation.
  ;;
  (syntax-case stx ( ;;
		    FAST-PUT-UTF8-TAG FAST-PUT-CHAR-TAG FAST-PUT-LATIN-TAG
		    FAST-PUT-UTF16LE-TAG FAST-PUT-UTF16BE-TAG)
    ((%case-textual-output-port-fast-tag (?port ?who)
       ((FAST-PUT-UTF8-TAG)	. ?utf8-tag-body)
       ((FAST-PUT-CHAR-TAG)	. ?char-tag-body)
       ((FAST-PUT-LATIN-TAG)	. ?latin-tag-body)
       ((FAST-PUT-UTF16LE-TAG)	. ?utf16le-tag-body)
       ((FAST-PUT-UTF16BE-TAG)	. ?utf16be-tag-body))
     (and (identifier? #'?port)
	  (identifier? #'?who))
     #'(let retry-after-tagging-port ((m ($port-fast-attrs-or-zero ?port)))
	 (define (%validate)
	   (%assert-argument-is-port ?port ?who)
	   (%unsafe.assert-value-is-textual-port ?port ?who)
	   (%unsafe.assert-value-is-output-port  ?port ?who)
	   (%unsafe.assert-value-is-open-port    ?port ?who)
	   (assertion-violation ?who "unsupported port transcoder" ?port))
	 (define (%reconfigure-as-output fast-attrs)
	   (%unsafe.reconfigure-input-buffer-to-output-buffer ?port ?who)
	   ($set-port-fast-attrs! ?port fast-attrs)
	   (retry-after-tagging-port fast-attrs))
	 ($case-fixnums m
	   ((FAST-PUT-UTF8-TAG)		. ?utf8-tag-body)
	   ((FAST-PUT-CHAR-TAG)		. ?char-tag-body)
	   ((FAST-PUT-LATIN-TAG)	. ?latin-tag-body)
	   ((FAST-PUT-UTF16LE-TAG)	. ?utf16le-tag-body)
	   ((FAST-PUT-UTF16BE-TAG)	. ?utf16be-tag-body)
	   ((FAST-PUT-BYTE-TAG)
	    (assertion-violation ?who "expected textual port" ?port))
	   (else
	    (if (and (port? ?port)
		     (%unsafe.input-and-output-port? ?port))
		($case-fixnums m
		  ((FAST-GET-UTF8-TAG)
		   (%reconfigure-as-output FAST-PUT-UTF8-TAG))
		  ((FAST-GET-CHAR-TAG)
		   (%reconfigure-as-output FAST-PUT-CHAR-TAG))
		  ((FAST-GET-LATIN-TAG)
		   (%reconfigure-as-output FAST-PUT-LATIN-TAG))
		  ((FAST-GET-UTF16LE-TAG)
		   (%reconfigure-as-output FAST-PUT-UTF16LE-TAG))
		  ((FAST-GET-UTF16BE-TAG)
		   (%reconfigure-as-output FAST-PUT-UTF16BE-TAG))
		  ((FAST-GET-BYTE-TAG)
		   (assertion-violation ?who "expected textual port" ?port))
		  (else
		   (%validate)))
	      (%validate))))
	 ))))

;;; --------------------------------------------------------------------
;;; Backup of original Ikarus values

;;(define PORT-TYPE-MASK		#b00000000001111)
;;(define BINARY-INPUT-PORT-BITS	#b00000000001001)
;;(define BINARY-OUTPUT-PORT-BITS	#b00000000001010)
;;(define TEXTUAL-INPUT-PORT-BITS	#b00000000000101)
;;(define TEXTUAL-OUTPUT-PORT-BITS	#b00000000000110)

;;(define FAST-GET-BYTE-TAG		#b00000000001001)
;;(define FAST-GET-CHAR-TAG		#b00000000010101)
;;(define FAST-GET-UTF8-TAG		#b00000000100101)
;;(define FAST-GET-LATIN-TAG		#b00000001100101)
;;(define FAST-GET-UTF16BE-TAG		#b00000010000101)
;;(define FAST-GET-UTF16LE-TAG		#b00000100000101)

;;(define FAST-PUT-BYTE-TAG		#b00000000001010)
;;(define FAST-PUT-CHAR-TAG		#b00000000010110)
;;(define FAST-PUT-UTF8-TAG		#b00000000100110)
;;(define FAST-PUT-LATIN-TAG		#b00000001100110)
;;(define FAST-PUT-UTF16BE-TAG		#b00000010000110)
;;(define FAST-PUT-UTF16LE-TAG		#b00000100000110)
;;(define INIT-PUT-UTF16-TAG		#b00000110000110)


;;;; assertion helpers

(define-inline (%assert-argument-is-port ?port ?who)
  (unless (port? ?port)
    (assertion-violation ?who "expected port as argument" ?port)))

;;; --------------------------------------------------------------------

(define-inline (%unsafe.assert-value-is-input-port ?port ?who)
  (unless (%unsafe.input-port? ?port)
    (assertion-violation ?who "not an input port" ?port)))

(define-inline (%unsafe.assert-value-is-output-port ?port ?who)
  (unless (%unsafe.output-port? ?port)
    (assertion-violation ?who "not an output port" ?port)))

;;; --------------------------------------------------------------------

(define-inline (%unsafe.assert-value-is-binary-port ?port ?who)
  (unless (%unsafe.binary-port? ?port)
    (assertion-violation ?who "not a binary port" ?port)))

(define-inline (%unsafe.assert-value-is-textual-port ?port ?who)
  (unless (%unsafe.textual-port? ?port)
    (assertion-violation ?who "not a textual port" ?port)))

;;; --------------------------------------------------------------------

(define-inline (%unsafe.assert-value-is-open-port ?port ?who)
  (when (%unsafe.port-closed? ?port)
    (assertion-violation ?who "port is closed" ?port)))


;;;; arguments validation

(define-argument-validation (port who obj)
  (port? obj)
  (assertion-violation who "expected port as argument" obj))

(define-argument-validation (port-mode who obj)
  (or (eq? obj 'r6rs) (eq? obj 'vicare))
  (assertion-violation who "expected supported port mode as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (input-port who obj)
  (input-port? obj)
  (assertion-violation who "expected input port as argument" obj))

(define-argument-validation (unsafe.input-port who obj)
  (%unsafe.input-port? obj)
  (assertion-violation who "expected input port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (output-port who obj)
  (output-port? obj)
  (assertion-violation who "expected output port as argument" obj))

(define-argument-validation (unsafe.output-port who obj)
  (%unsafe.output-port? obj)
  (assertion-violation who "expected output port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsafe.not-input/output-port who obj)
  (not (%unsafe.input-and-output-port? obj))
  (assertion-violation who "invalid input/output port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsafe.binary-port who obj)
  (%unsafe.binary-port? obj)
  (assertion-violation who "expected binary port as argument" obj))

(define-argument-validation (unsafe.textual-port who obj)
  (%unsafe.textual-port? obj)
  (assertion-violation who "expected textual port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (open-port who obj)
  (not (port-closed? obj))
  (assertion-violation who "expected open port as argument" obj))

(define-argument-validation (unsafe.open-port who obj)
  (not (%unsafe.port-closed? obj))
  (assertion-violation who "expected open port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (port-position who position)
  (and (or (fixnum? position)
	   (bignum? position))
       (>= position 0))
  (raise
   (condition (make-who-condition who)
	      (make-message-condition "position must be a nonnegative exact integer")
	      (make-i/o-invalid-position-error position))))

(define-argument-validation (get-position-result who position port)
  (and (or (fixnum? position)
	   (bignum? position))
       (>= position 0))
  (assertion-violation who "invalid value returned by get-position" port position))

(define-argument-validation (transcoder who obj)
  (transcoder? obj)
  (assertion-violation who "expected transcoder as argument" obj))

(define-argument-validation (maybe-transcoder who obj)
  (or (not obj) (transcoder? obj))
  (assertion-violation who
    "expected false or a transcoder object as optional transcoder argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (octet who obj)
  (and (fixnum? obj)
       (unsafe.fx>= obj 0)
       (unsafe.fx<= obj 255))
  (assertion-violation who "expected fixnum in the range for octets as argument" obj))

(define-argument-validation (char who obj)
  (char? obj)
  (assertion-violation who "expected character as argument" obj))

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

(define-argument-validation (port-identifier who obj)
  (string? obj)
  (assertion-violation who "ID is not a string" obj))

(define-argument-validation (read!-procedure who obj)
  (procedure? obj)
  (assertion-violation who "READ! is not a procedure" obj))

(define-argument-validation (write!-procedure who obj)
  (procedure? obj)
  (assertion-violation who "WRITE! is not a procedure" obj))

(define-argument-validation (maybe-close-procedure who obj)
  (or (procedure? obj) (not obj))
  (assertion-violation who "CLOSE should be either a procedure or false" obj))

(define-argument-validation (maybe-get-position-procedure who obj)
  (or (procedure? obj) (not obj))
  (assertion-violation who "GET-POSITION should be either a procedure or false" obj))

(define-argument-validation (maybe-set-position!-procedure who obj)
  (or (procedure? obj) (not obj))
  (assertion-violation who "SET-POSITION! should be either a procedure or false" obj))

(define-argument-validation (filename who obj)
  (string? obj)
  (assertion-violation who "expected string as filename argument" obj))

(define-argument-validation (file-options who obj)
  (enum-set? obj)
  (assertion-violation who "expected enum set as file-options argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (fixnum-start-index who start)
  (and (fixnum? start) (unsafe.fx>= start 0))
  (assertion-violation who "expected non-negative fixnum as start index argument" start))

(define-argument-validation (unsafe.start-index-for-bytevector who dst.start dst.bv)
  ;;Notice that start=length is valid is the count argument is zero
  (unsafe.fx<= dst.start (unsafe.bytevector-length dst.bv))
  (assertion-violation who
    (string-append "start index argument " (number->string dst.start)
		   " too big for bytevector of length "
		   (number->string (unsafe.bytevector-length dst.bv)))
    dst.start))

(define-argument-validation (unsafe.start-index-for-string who dst.start dst.str)
  ;;Notice that start=length is valid is the count argument is zero
  (unsafe.fx< dst.start (unsafe.string-length dst.str))
  (assertion-violation who
    (string-append "start index argument " (number->string dst.start)
		   " too big for string of length " (number->string (unsafe.string-length dst.str)))
    dst.start))

;;; --------------------------------------------------------------------

(define-argument-validation (count who count)
  (and (integer? count)
       (exact? count)
       (>= count 0))
  (assertion-violation who "expected non-negative exact integer as count argument" count))

(define-argument-validation (fixnum-count who count)
  (and (fixnum? count)
       (unsafe.fx>= count 0))
  (assertion-violation who "expected non-negative fixnum as count argument" count))

(define-argument-validation (unsafe.count-from-start-in-bytevector who count start dst.bv)
  ;;We know that COUNT and START  are fixnums, but not if START+COUNT is
  ;;a fixnum, too.
  ;;
  (<= (+ start count) (unsafe.bytevector-length dst.bv))
  (assertion-violation who
    (string-append "count argument "    (number->string count)
		   " from start index " (number->string start)
		   " too big for bytevector of length "
		   (number->string (unsafe.bytevector-length dst.bv)))
    start count (unsafe.bytevector-length dst.bv)))

(define-argument-validation (unsafe.count-from-start-in-string who count start dst.str)
  ;;We know that COUNT and START  are fixnums, but not if START+COUNT is
  ;;a fixnum, too.
  ;;
  (<= (+ start count) (unsafe.string-length dst.str))
  (assertion-violation who
    (string-append "count argument "    (number->string count)
		   " from start index " (number->string start)
		   " too big for string of length "
		   (number->string (unsafe.string-length dst.str)))
    start count (unsafe.string-length dst.str)))


;;;; error helpers

(define-inline (%implementation-violation ?who ?message . ?irritants)
  (assertion-violation ?who ?message . ?irritants))

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

(define UTF-8-BYTE-ORDER-MARK-LIST			'(#xEF #xBB #xBF))
(define UTF-16-BIG-ENDIAN-BYTE-ORDER-MARK-LIST		'(#xFE #xFF))
(define UTF-16-LITTLE-ENDIAN-BYTE-ORDER-MARK-LIST	'(#xFF #xFE))

(define-auxiliary-syntaxes
  data-is-needed-at:
  if-available-data:
  if-available-room:
  if-end-of-file:
  if-no-match-raise-assertion-violation
  if-no-match:
  if-successful-match:
  if-successful-refill:
  room-is-needed-for:)

(define-syntax case-errno
  (syntax-rules (else)
    ((_ ?errno ((?code0 ?code ...) . ?body) ... (else . ?else-body))
     (let ((errno ?errno))
       (cond ((or (and (fixnum? ?code0) (unsafe.fx= errno ?code0))
		  (and (fixnum? ?code)  (unsafe.fx= errno ?code))
		  ...)
	      . ?body)
	     ...
	     (else . ?else-body))))
    ((_ ?errno ((?code0 ?code ...) . ?body) ...)
     (let ((errno ?errno))
       (cond ((or (and (fixnum? ?code0) (unsafe.fx= errno ?code0))
		  (and (fixnum? ?code)  (unsafe.fx= errno ?code))
		  ...)
	      . ?body)
	     ...
	     (else
	      (assertion-violation #f "unknown errno code" errno)))))
    ))


;;;; Byte Order Mark (BOM) parsing

(define-syntax %parse-byte-order-mark
  (syntax-rules (if-successful-match: if-no-match: if-end-of-file:)
    ((%parse-byte-order-mark (?who ?port ?bom)
       (if-successful-match:	. ?matched-body)
       (if-no-match:		. ?failure-body)
       (if-end-of-file:		. ?eof-body))
     (let ((result (%unsafe.parse-byte-order-mark ?who ?port ?bom)))
       (cond ((boolean? result)
	      (if result
		  (begin . ?matched-body)
		(begin . ?failure-body)))
	     ((eof-object? result)
	      (begin . ?eof-body))
	     (else
	      (assertion-violation ?who
		"vicare internal error: invalid return value while parsing BOM" result)))))))

(define (%unsafe.parse-byte-order-mark who port bom)
  ;;Assuming PORT is  an open input port with  a bytevector buffer: read
  ;;and consume octets from PORT  verifying if they match the given list
  ;;of octets representing a Byte Order Mark (BOM).
  ;;
  ;;PORT must be an open textual  or binary input port with a bytevector
  ;;buffer.  BOM  must be  a list of  fixnums representing  the expected
  ;;Byte Order Mark sequence.
  ;;
  ;;Return #t  if the whole  BOM sequence is  read and matched;  in this
  ;;case the port position is left right after the BOM sequence.
  ;;
  ;;Return #f if the octets from the port do not match the BOM sequence;
  ;;in this  case the  port position is  left at  the same point  it was
  ;;before this function call.
  ;;
  ;;Return the EOF  object if the port reaches EOF  before the whole BOM
  ;;is matched; in this case the port position is left at the same point
  ;;it was before this function call.
  ;;
  (with-port (port)
    (let next-octet-in-bom ((number-of-consumed-octets 0)
			    (bom bom))
      (if (null? bom)
	  ;;Full success: all the octets in the given BOM sequence where
	  ;;matched.
	  (begin
	    (port.buffer.index.incr! number-of-consumed-octets)
	    #t)
	(let retry-after-filling-buffer ()
	  (let ((buffer.offset (unsafe.fx+ number-of-consumed-octets port.buffer.index)))
	    (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset)
	      (if-end-of-file: (eof-object))
	      (if-successful-refill: (retry-after-filling-buffer))
	      (if-available-data:
	       (and (unsafe.fx= (car bom) (unsafe.bytevector-u8-ref port.buffer buffer.offset))
		    (next-octet-in-bom (unsafe.fxadd1 number-of-consumed-octets) (cdr bom)))))))))))

(define (%unsafe.parse-utf16-bom-and-add-fast-tag who port)
  ;;Assuming PORT is an open textual input port object with a bytevector
  ;;buffer  and   a  UTF-16  transcoder  not  yet   specialised  for  an
  ;;endianness:  read  the  Byte   Order  Mark  and  mutate  the  port's
  ;;attributes   tagging    the   port   as    FAST-GET-UTF16BE-TAG   or
  ;;FAST-GET-UTF16LE-TAG.  Return  #t if port  is at EOF,  #f otherwise.
  ;;If no BOM is present, select big endian by default.
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

(define-syntax %parse-bom-and-add-fast-tag
  (syntax-rules (if-successful-match: if-end-of-file: if-no-match-raise-assertio-violation)
    ((%parse-bom-and-add-fast-tag (?who ?port)
       (if-successful-match:	. ?matched-body)
       (if-end-of-file:		. ?eof-body)
       (if-no-match-raise-assertion-violation))
     (if (%unsafe.parse-bom-and-add-fast-tag ?who ?port)
	 (begin . ?eof-body)
       (begin . ?matched-body)))))

(define (%unsafe.parse-bom-and-add-fast-tag who port)
  ;;Assuming  PORT is  an  open  textual input  port  with a  bytevector
  ;;buffer: read the Byte Order  Mark expected for the port's transcoder
  ;;and mutate the port's fast attributes, tagging the port accordingly.
  ;;Return #t if port is at EOF, #f otherwise.
  ;;
  ;;If PORT is already tagged: existing fast attributes are ignored.
  ;;
  ;;If  the input  octects  do not  match  the requested  BOM: raise  an
  ;;assertion violation.
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
       (%unsafe.parse-utf16-bom-and-add-fast-tag who port))

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
       ;;Try  all the  UTF  encodings in  the  order: UTF-8,  UTF-16-be,
       ;;UTF-16-le.
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


;;;; bytevector helpers

(define (%unsafe.bytevector-reverse-and-concatenate who list-of-bytevectors dst.len)
  ;;Reverse  LIST-OF-BYTEVECTORS and  concatenate its  bytevector items;
  ;;return  the result.  The  resulting list  must have  length DST.LEN.
  ;;Assume the arguments have been already validated.
  ;;
  ;;IMPLEMENTATION RESTRICTION The bytevectors must have a fixnum length
  ;;and the whole bytevector must at maximum have a fixnum length.
  ;;
  (let next-bytevector ((dst.bv              (unsafe.make-bytevector dst.len))
			(list-of-bytevectors list-of-bytevectors)
			(dst.start           dst.len))
    (if (null? list-of-bytevectors)
	(begin
	  dst.bv)
      (let* ((src.bv    (car list-of-bytevectors))
	     (src.len   (unsafe.bytevector-length src.bv))
	     (dst.start (unsafe.fx- dst.start src.len)))
	(unsafe.bytevector-copy!/count src.bv 0 dst.bv dst.start src.len)
	(next-bytevector dst.bv (cdr list-of-bytevectors) dst.start)))))


;;;; string helpers

(define (%unsafe.string-reverse-and-concatenate who list-of-strings dst.len)
  ;;Reverse LIST-OF-STRINGS and concatenate its string items; return the
  ;;result.  The  resulting list must  have length DST.LEN.   Assume the
  ;;arguments have been already validated.
  ;;
  ;;IMPLEMENTATION RESTRICTION The strings must have a fixnum length and
  ;;the whole string must at maximum have a fixnum length.
  ;;
  (let next-string ((dst.str (unsafe.make-string dst.len))
		    (list-of-strings list-of-strings)
		    (dst.start       dst.len))
    (if (null? list-of-strings)
	dst.str
      (let* ((src.str   (car list-of-strings))
	     (src.len   (unsafe.string-length src.str))
	     (dst.start (unsafe.fx- dst.start src.len)))
	(unsafe.string-copy!/count src.str 0 dst.str dst.start src.len)
	(next-string dst.str (cdr list-of-strings) dst.start)))))


;;;; dot notation macros for port structures

(define-syntax with-port
  (syntax-rules ()
    ((_ (?port) . ?body)
     (%with-port (?port #f) . ?body))))

(define-syntax with-port-having-bytevector-buffer
  (syntax-rules ()
    ((_ (?port) . ?body)
     (%with-port (?port unsafe.bytevector-length) . ?body))))

(define-syntax with-port-having-string-buffer
  (syntax-rules ()
    ((_ (?port) . ?body)
     (%with-port (?port unsafe.string-length) . ?body))))

(define-syntax* (%with-port stx)
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
	    (PORT.BUFFER.INDEX			(%dot-id ".buffer.index"))
		;fixnum, the offset from the buffer beginning
	    (PORT.BUFFER.USED-SIZE		(%dot-id ".buffer.used-size"))
		;fixnum, number of octets used in the buffer
	    (PORT.BUFFER			(%dot-id ".buffer"))
		;bytevector, the buffer
	    (PORT.TRANSCODER			(%dot-id ".transcoder"))
		;the transcoder or false
	    (PORT.ID				(%dot-id ".id"))
		;string, describes the port
	    (PORT.READ!				(%dot-id ".read!"))
		;function or false, the read function
	    (PORT.WRITE!			(%dot-id ".write!"))
		;function or false, the write function
	    (PORT.SET-POSITION!			(%dot-id ".set-position!"))
		;function or false, the function to set the position
	    (PORT.GET-POSITION			(%dot-id ".get-position"))
		;function or false, the function to get the position
	    (PORT.CLOSE				(%dot-id ".close"))
		;function or false, the function to close the port
	    (PORT.COOKIE			(%dot-id ".cookie"))
		;cookie record
	    (PORT.BUFFER.FULL?			(%dot-id ".buffer.full?"))
		;true if the buffer is full
	    (PORT.CLOSED?			(%dot-id ".closed?"))
		;true if the port is closed
	    (PORT.GUARDED?			(%dot-id ".guarded?"))
		;true if the port is registered in the port guardian
	    (PORT.FD-DEVICE?			(%dot-id ".fd-device?"))
		;true if the port has a file descriptor as device
	    (PORT.WITH-EXTRACTION?		(%dot-id ".with-extraction?"))
		;true if the port has an associated extraction function
	    (PORT.IS-INPUT-AND-OUTPUT?		(%dot-id ".is-input-and-output?"))
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
		(PORT.GUARDED?		(identifier-syntax (%unsafe.guarded-port? ?port)))
		(PORT.FD-DEVICE?	(identifier-syntax (%unsafe.port-with-fd-device? ?port)))
		(PORT.WITH-EXTRACTION?	(identifier-syntax (%unsafe.port-with-extraction? ?port)))
		(PORT.IS-INPUT-AND-OUTPUT? (identifier-syntax (%unsafe.input-and-output-port? ?port)))
		(PORT.IS-INPUT?		(identifier-syntax (%unsafe.input-port? ?port)))
		(PORT.IS-OUTPUT?	(identifier-syntax (%unsafe.output-port? ?port)))
		(PORT.IS-INPUT-ONLY?	(identifier-syntax (%unsafe.input-only-port? ?port)))
		(PORT.IS-OUTPUT-ONLY?	(identifier-syntax (%unsafe.output-only-port? ?port)))
		(PORT.LAST-OPERATION-WAS-INPUT?
		 (identifier-syntax (%unsafe.last-port-operation-was-input? ?port)))
		(PORT.LAST-OPERATION-WAS-OUTPUT?
		 (identifier-syntax (%unsafe.last-port-operation-was-output? ?port)))
		(PORT.BUFFER-MODE-LINE?	(identifier-syntax (%unsafe.port-buffer-mode-line? ?port)))
		(PORT.BUFFER-MODE-NONE?	(identifier-syntax (%unsafe.port-buffer-mode-none? ?port)))
		(PORT.ATTRIBUTES	(identifier-syntax
					 (_		($port-attrs ?port))
					 ((set! id ?value) ($set-port-attrs! ?port ?value))))
		(PORT.FAST-ATTRIBUTES	(identifier-syntax
					 (_		($port-fast-attrs ?port))
					 ((set! _ ?tag)	($set-port-fast-attrs! ?port ?tag))))
		(PORT.OTHER-ATTRIBUTES	(identifier-syntax ($port-other-attrs ?port)))
		(PORT.BUFFER.SIZE	(identifier-syntax (?buffer-length ($port-buffer ?port))))
		(PORT.BUFFER.INDEX	(identifier-syntax
					 (_			($port-index ?port))
					 ((set! _ ?value)	($set-port-index! ?port ?value))))
		(PORT.BUFFER.USED-SIZE	(identifier-syntax
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


;;;; cookie data structure
;;
;;An instance of  this structure is referenced by  every port structure;
;;it  registers  the  underlying  device  (if any)  and  it  tracks  the
;;underlying  device's  position,  number  of  rows  and  columns  (when
;;possible).
;;
;;The  reason  the full  port  data structure  is  split  into the  PORT
;;structure and the  COOKIE structure, is that, in  some port types, the
;;port's own internal  functions must reference some of  the guts of the
;;data  structure but cannot  reference the  port itself.   This problem
;;shows  its uglyness  when TRANSCODED-PORT  is applied  to a  port: the
;;original  port is  closed  and its  guts  are transferred  to the  new
;;transcoded port.   By partitioning the  fields, we allow  the internal
;;functions to reference only the cookie and be free of the port value.
;;
;;Why are the fields not all  in the cookie then?  Because accessing the
;;port is faster than accessing  the cookie and many operations on ports
;;only require  access to  the buffer, which  is referenced by  the PORT
;;structure.
;;
;;NOTE: It  is impossible to track  the row number  for ports supporting
;;the SET-PORT-POSITION! operation.  The  ROW-NUM field of the cookie is
;;meaningful  only  for  ports  whose position  increases  monotonically
;;because of read or write operations, it should be invalidated whenever
;;the port  position is moved with SET-PORT-POSITION!.   Notice that the
;;input port used to read Scheme source code satisfies this requirement.
;;

;;Constructor: (make-cookie DEST MODE POS CH-OFF ROW-NUM COL-NUM UID HASH)
;;
;;Field name: dest
;;Accessor name: (cookie-dest COOKIE)
;;  If an underlying device exists:  this field holds a reference to it,
;;  for  example   a  fixnum  representing  an   operating  system  file
;;  descriptor.
;;
;;  If no device exists: this field is set to false.
;;
;;  As a  special case: this field can  hold a Scheme list  managed as a
;;  stack in which data is temporarily stored.  For example: this is the
;;  case of output bytevector ports.
;;
;;Field name: mode
;;Accessor name: (cookie-mode COOKIE)
;;Mutator name: (set-cookie-mode! COOKIE MODE-SYMBOL)
;;  Hold  the symbol  representing  the current  port  mode, one  among:
;;  "vicare", "r6rs".
;;
;;Field name: pos
;;Accessor name: (cookie-pos COOKIE)
;;Mutator name: (set-cookie-pos! COOKIE NEW-POS)
;;  If  an  underlying  device  exists:  this field  holds  the  current
;;  position in the underlying  device.  If no underlying device exists:
;;  this field is set to zero.
;;
;;  It  is the  responsibility of  the *callers*  of the  port functions
;;  READ!, WRITE!  and SET-POSITION!   to update this field.  The port's
;;  own functions  READ!, WRITE!  and SET-POSITION!  must  not touch the
;;  cookie.
;;
;;Field name: character-offset
;;Accessor name: (cookie-character-offset COOKIE)
;;Mutator name: (set-cookie-character-offset! COOKIE NEW-CH-OFF)
;;  Zero-based offset  of the current  position in a textual  input port
;;  expressed in characters.
;;
;;Field name: row-number
;;Accessor name: (cookie-row-number COOKIE)
;;Mutator name: (set-cookie-row-number! COOKIE NEW-POS)
;;  One-based  row number  of the  current position  in a  textual input
;;  port.
;;
;;Field name: column-number
;;Accessor name: (cookie-column-number COOKIE)
;;Mutator name: (set-cookie-column-number! COOKIE NEW-POS)
;;  One-based column number  of the current position in  a textual input
;;  port.
;;
;;Field name: uid
;;Accessor name: (cookie-uid COOKIE)
;;Mutator name: (set-cookie-uid! COOKIE GENSYM)
;;  Gensym uniquely associated to the port.  To be used, for example, to
;;  generate hash keys.
;;
;;Field name: hash
;;Accessor name: (cookie-hash COOKIE)
;;Mutator name: (set-cookie-hash! COOKIE HASH)
;;  Hash value  to be  used by  hashtables.  It  should be  generated by
;;  applying SYMBOL-HASH to the gensym in the UID field.
;;
(define-struct cookie
  (dest mode pos character-offset row-number column-number uid hash))

(define (default-cookie device)
  (make-cookie device 'vicare 0 #;device-position
	       0 #;character-offset 1 #;row-number 1 #;column-number
	       #f #;uid #f #;hash))

(define (get-char-and-track-textual-position port)
  ;;Defined by  Vicare.  Like GET-CHAR  but track the  textual position.
  ;;Recognise only linefeed characters as line-ending.
  ;;
  (let* ((ch     (get-char port))
	 (cookie ($port-cookie port)))
    (cond ((eof-object? ch)
	   ch)
	  ((unsafe.char= ch #\newline)
	   (set-cookie-character-offset! cookie (+ 1 (cookie-character-offset cookie)))
	   (set-cookie-row-number!       cookie (+ 1 (cookie-row-number       cookie)))
	   (set-cookie-column-number!    cookie 1)
	   ch)
	  ((unsafe.char= ch #\tab)
	   (set-cookie-character-offset! cookie (+ 1 (cookie-character-offset cookie)))
	   (set-cookie-column-number!    cookie (+ 8 (cookie-column-number    cookie)))
	   ch)
	  (else
	   (set-cookie-character-offset! cookie (+ 1 (cookie-character-offset cookie)))
	   (set-cookie-column-number!    cookie (+ 1 (cookie-column-number    cookie)))
	   ch))))

(define (port-textual-position port)
  ;;Defined by Vicare.  Given a textual port, return the current textual
  ;;position  as  a vector:  0-based  byte  position, 0-based  character
  ;;offset, 1-based row number, 1-based column number.
  ;;
  (define who 'port-textual-position)
  (with-arguments-validation (who)
      ((port			port)
       (unsafe.textual-port	port))
    (let ((cookie ($port-cookie port)))
      (make-source-position-condition (port-id port)
				      (%unsafe.port-position who port)
				      (cookie-character-offset cookie)
				      (cookie-row-number       cookie)
				      (cookie-column-number    cookie)))))


;;;; port's buffer size customisation

;;For ports  having a  Scheme bytevector as  buffer: the  minimum buffer
;;size  must be big  enough to  allow the  buffer to  hold the  full UTF
;;encoding of two Unicode  characters for all the supported transcoders.
;;For  ports having  a Scheme  string as  buffer: there  is  no rational
;;constraint on the buffer size.
;;
;;It makes sense  to have "as small as possible"  minimum buffer size to
;;allow  easy writing  of test  suites  exercising the  logic of  buffer
;;flushing and filling.
;;
(define BUFFER-SIZE-LOWER-LIMIT		8)

;;The maximum  buffer size must  be small enough  to fit into  a fixnum,
;;which is defined by R6RS to be capable of holding at least 24 bits.
;;
(define BUFFER-SIZE-UPPER-LIMIT		(greatest-fixnum))

;;For binary ports: the default  buffer size should be selected to allow
;;efficient  caching of  portions of  binary  data blobs,  which may  be
;;megabytes wide.
;;
(define DEFAULT-BINARY-BLOCK-SIZE	(* 4 4096))

;;For textual ports: the default buffer size should be selected to allow
;;efficient  caching of  portions of  text for  the most  recurring use,
;;which includes  accumulation of  "small" strings, like  in the  use of
;;printf-like functions.
;;
(define DEFAULT-STRING-BLOCK-SIZE	256)

(define-inline (%valid-buffer-size? obj)
  (and (fixnum? obj)
       (unsafe.fx>= obj BUFFER-SIZE-LOWER-LIMIT)
       (unsafe.fx<  obj BUFFER-SIZE-UPPER-LIMIT)))

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

(let-syntax ((define-buffer-size-parameter (syntax-rules ()
					     ((_ ?who ?init)
					      (define ?who
						(%make-buffer-size-parameter ?init '?who))))))

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
  (define-predicate textual-port? TEXTUAL-PORT-TAG)

  ;;Defined by R6RS.  Return #t if X is a binary port.
  (define-predicate binary-port? BINARY-PORT-TAG)

  (define-predicate input/output-port? INPUT/OUTPUT-PORT-TAG))

(define (input-port? x)
  ;;Defined by  R6RS.  Return  #t if X  is an input  port or  a combined
  ;;input and output port.
  (and (port? x)
       (let ((flags ($port-tag x)))
	 (or (unsafe.fx= (unsafe.fxand flags INPUT-PORT-TAG) INPUT-PORT-TAG)
	     (unsafe.fx= (unsafe.fxand flags INPUT/OUTPUT-PORT-TAG) INPUT/OUTPUT-PORT-TAG)))))

(define (output-port? x)
  ;;Defined by  R6RS.  Return #t  if X is  an output port or  a combined
  ;;input and output port.
  (and (port? x)
       (let ((flags ($port-tag x)))
	 (or (unsafe.fx= (unsafe.fxand flags OUTPUT-PORT-TAG) OUTPUT-PORT-TAG)
	     (unsafe.fx= (unsafe.fxand flags INPUT/OUTPUT-PORT-TAG) INPUT/OUTPUT-PORT-TAG)))))

;;The following predicates have to be used after the argument has
;;been validated as  port value.  They are *not*  affected by the
;;fact that the port is closed.
(let-syntax
    ((define-unsafe-predicate (syntax-rules ()
				((_ ?who ?bits)
				 (define-inline (?who port)
				   (unsafe.fx= (unsafe.fxand ($port-attrs port) ?bits) ?bits))))))

  (define-unsafe-predicate %unsafe.binary-port?		BINARY-PORT-TAG)
  (define-unsafe-predicate %unsafe.textual-port?	TEXTUAL-PORT-TAG)
  (define-unsafe-predicate %unsafe.input-and-output-port? INPUT/OUTPUT-PORT-TAG)

  (define-unsafe-predicate %unsafe.binary-input-port?	BINARY-INPUT-PORT-BITS)
  (define-unsafe-predicate %unsafe.binary-output-port?	BINARY-OUTPUT-PORT-BITS)
  (define-unsafe-predicate %unsafe.textual-input-port?	TEXTUAL-INPUT-PORT-BITS)
  (define-unsafe-predicate %unsafe.textual-output-port?	TEXTUAL-OUTPUT-PORT-BITS)

  (define-unsafe-predicate %unsafe.port-buffer-mode-none?	BUFFER-MODE-NONE-TAG)
  (define-unsafe-predicate %unsafe.port-buffer-mode-line?	BUFFER-MODE-LINE-TAG)

  (define-unsafe-predicate %unsafe.guarded-port?		GUARDED-PORT-TAG)
  (define-unsafe-predicate %unsafe.port-with-extraction?	PORT-WITH-EXTRACTION-TAG)
  (define-unsafe-predicate %unsafe.port-with-fd-device?		PORT-WITH-FD-DEVICE)
  )

(define-inline (%unsafe.input-only-port? port)
  ;;True if PORT is input and not input/output.
  ;;
  (let ((flags ($port-attrs port)))
    (and (unsafe.fx= (unsafe.fxand flags INPUT-PORT-TAG) INPUT-PORT-TAG)
	 (unsafe.fxzero? (unsafe.fxand flags INPUT/OUTPUT-PORT-TAG)))))

(define-inline (%unsafe.input-port? port)
  ;;True if PORT is input or input/output.
  ;;
  (let ((flags ($port-attrs port)))
    (or (unsafe.fx= (unsafe.fxand flags INPUT-PORT-TAG) INPUT-PORT-TAG)
	(unsafe.fx= (unsafe.fxand flags INPUT/OUTPUT-PORT-TAG) INPUT/OUTPUT-PORT-TAG))))

(define-inline (%unsafe.output-only-port? port)
  ;;True if PORT is output and not input/output.
  ;;
  (let ((flags ($port-attrs port)))
    (and (unsafe.fx= (unsafe.fxand flags OUTPUT-PORT-TAG) OUTPUT-PORT-TAG)
	 (unsafe.fxzero? (unsafe.fxand flags INPUT/OUTPUT-PORT-TAG)))))

(define-inline (%unsafe.output-port? port)
  ;;True if PORT is output or input/output.
  ;;
  (let ((flags ($port-attrs port)))
    (or (unsafe.fx= (unsafe.fxand flags OUTPUT-PORT-TAG) OUTPUT-PORT-TAG)
	(unsafe.fx= (unsafe.fxand flags INPUT/OUTPUT-PORT-TAG) INPUT/OUTPUT-PORT-TAG))))

(define-inline (%unsafe.last-port-operation-was-input? port)
  ;;True if PORT is input or PORT is input/output and the last operation
  ;;was input; in other words: the buffer may contain input bytes.
  ;;
  (unsafe.fx= (unsafe.fxand ($port-attrs port) INPUT-PORT-TAG) INPUT-PORT-TAG))

(define-inline (%unsafe.last-port-operation-was-output? port)
  ;;True  if  PORT  is output  or  PORT  is  input/output and  the  last
  ;;operation was output;  in other words: the buffer  may contain output
  ;;bytes.
  ;;
  (unsafe.fx= (unsafe.fxand ($port-attrs port) OUTPUT-PORT-TAG) OUTPUT-PORT-TAG))


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
  (make-guardian))

(define (%close-garbage-collected-ports)
  (let ((port (port-guardian)))
    (when port
      ;;FIXME  This CLOSE-PORT  call is  the whole  purpose of  having a
      ;;guardian here.  If it is commented  out: it is to investigate if
      ;;it is  causing an  "invalid frame size"  error with  some builds
      ;;(issue 35 at  Github).  Once the problem is fixed,  it should be
      ;;reincluded.  (Marco Maggi; Nov 2, 2011)

      ;;Notice that, as defined by R6RS, CLOSE-PORT does nothing if PORT
      ;;has already been closed.
      (close-port port)

      ;;The code below differs from CLOSE-PORT because it does not flush
      ;;the buffer.
      #;(with-port (port)
	(unless port.closed?
	  (port.mark-as-closed!)
	  (when (procedure? port.close)
	    (port.close))))

      (%close-garbage-collected-ports))))

(define (%port->maybe-guarded-port port)
  ;;Accept a port  as argument and return the port  itself.  If the port
  ;;wraps a platform descriptor: register it in the guardian.
  ;;
  (define who '%port->maybe-guarded-port)
  (with-arguments-validation (who)
      ((port port))
    (when (%unsafe.guarded-port? port)
      (port-guardian port))
    port))


;;;; port position

(define (port-has-port-position? port)
  ;;Defined by R6RS.   Return #t if the port  supports the PORT-POSITION
  ;;operation, and #f otherwise.
  ;;
  (define who 'port-has-port-position?)
  (with-arguments-validation (who)
      ((port port))
    (with-port (port)
      (and port.get-position #t))))

(define (port-has-set-port-position!? port)
  ;;Defined   by  R6RS.   The   PORT-HAS-SET-PORT-POSITION!?   procedure
  ;;returns #t  if the port supports  the SET-PORT-POSITION!  operation,
  ;;and #f otherwise.
  ;;
  (define who  'port-has-set-port-position!?)
  (with-arguments-validation (who)
      ((port port))
    (with-port (port)
      (and port.set-position! #t))))

(define (port-position port)
  ;;Defined by R6RS.  For a binary port, PORT-POSITION returns the index
  ;;of  the position  at which  the  next octet  would be  read from  or
  ;;written to the port as an exact non-negative integer object.
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
  ;;not necessarily correspond to a octet or character position.
  ;;
  (define who 'port-position)
  (with-arguments-validation (who)
      ((port port))
    (%unsafe.port-position who port)))

(define (%unsafe.port-position who port)
  ;;Return the current port position for PORT.
  ;;
  (with-port (port)
    (let ((device-position (%unsafe.device-position who port)))
      ;;DEVICE-POSITION is the position in the underlying device, but we
      ;;have to return the port  position taking into account the offset
      ;;in the input/output buffer.
      (if port.last-operation-was-output?
	  (+ device-position port.buffer.index)
	(- device-position (- port.buffer.used-size port.buffer.index))))))

(define (%unsafe.device-position who port)
  ;;Return the current device position for PORT.
  ;;
  (with-port (port)
    (let ((getpos port.get-position))
      (cond ((procedure? getpos)
	     ;;The port has a device whose position cannot be tracked by
	     ;;the cookie's POS field.
	     (let ((device-position (getpos)))
	       (with-arguments-validation (who)
		   ((get-position-result device-position port))
		 device-position)))
	    ((and (boolean? getpos) getpos)
	     ;;The  cookie's  POS  field  correctly tracks  the  current
	     ;;device position.
	     port.device.position)
	    (else
	     (assertion-violation who
	       "port does not support port-position operation" port))))))

(define (%unsafe.port-position/tracked-position who port)
  ;;If the port supports the GET-POSITION operation: use its own policy;
  ;;else trust the value in the POS cookie field.
  ;;
  (with-port (port)
    (let ((device-position (%unsafe.device-position/tracked-position who port)))
      (if port.last-operation-was-output?
	  (+ device-position port.buffer.index)
	(- device-position (- port.buffer.used-size port.buffer.index))))))

(define (%unsafe.device-position/tracked-position who port)
  ;;If the port supports the GET-POSITION operation: use its own policy;
  ;;else trust the value in the POS cookie field.
  ;;
  (with-port (port)
    (if port.get-position
	(%unsafe.device-position who port)
      port.device.position)))

;;; --------------------------------------------------------------------

(define (set-port-position! port requested-port-position)
  ;;Defined by R6RS.  If  PORT is a binary port, REQUESTED-PORT-POSITION
  ;;should be a non-negative exact integer object.  If PORT is a textual
  ;;port, REQUESTED-PORT-POSITION  should be the return value  of a call
  ;;to PORT-POSITION on PORT.
  ;;
  ;;The SET-PORT-POSITION! procedure  raises an exception with condition
  ;;type &ASSERTION if  the port does not support  the operation, and an
  ;;exception    with    condition    type   &I/O-INVALID-POSITION    if
  ;;REQUESTED-PORT-POSITION is  not in the  range of valid  positions of
  ;;PORT.  Otherwise, it  sets the current position of  the port to POS.
  ;;If PORT is an output port, SET-PORT-POSITION!  first flushes PORT.
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
  (with-port (port)
    (define-inline (main)
      (with-arguments-validation (who)
	  ((port		port)
	   (port-position	requested-port-position))
	(let ((set-position! port.set-position!))
	  (cond ((procedure? set-position!)
		 (let ((port.old-position (%unsafe.port-position/tracked-position who port)))
		   (unless (= port.old-position requested-port-position)
		     (%set-with-procedure port set-position! port.old-position))))
		((and (boolean? set-position!) set-position!)
		 (%set-with-boolean port))
		(else
		 (assertion-violation who
		   "port does not support SET-PORT-POSITION! operation" port))))))

    (define-inline (%set-with-procedure port set-position! port.old-position)
      ;;The SET-POSITION!   field is  a procedure: an  underlying device
      ;;exists.  If we are in the following scenario:
      ;;
      ;;                                        dev.old-pos
      ;;                                           v
      ;; |-----------------------------------------+---------| device
      ;;     |***+**************+******************+----|buffer
      ;;         ^              ^                  ^
      ;;      old-index     new-index          used-size
      ;;    port.old-pos   port.new-pos
      ;;
      ;;we  can satisfy  the request  just by  moving the  index  in the
      ;;buffer,  without calling SET-POSITION!  and changing  the device
      ;;position.
      ;;
      ;;Remember that port positions are not fixnums.
      ;;
      (let* ((delta-pos        (- requested-port-position port.old-position))
	     (buffer.new-index (+ port.buffer.index delta-pos)))
	(if (and (>= buffer.new-index 0)
		 (<= buffer.new-index port.buffer.used-size))
	    (set! port.buffer.index buffer.new-index)
	  ;;We  need to change  the device  position.  We  transform the
	  ;;requested  port  position  in  a requested  device  position
	  ;;computed  when the  buffer is  empty; we  need to  take into
	  ;;account the current buffer index.
	  (let ((device.new-position
		 (if port.last-operation-was-output?
		     ;;Output  port: flush  the buffer  and reset  it to
		     ;;empty.  Before flushing:
		     ;;
		     ;;    dev.old-pos
		     ;;       v
		     ;; |-----+---------------------------|device
		     ;;       |*******+*******+----|buffer
		     ;;               ^       ^
		     ;;             index  used-size
		     ;;
		     ;;after flushing:
		     ;;
		     ;;           dev.tmp-pos
		     ;;               v
		     ;; |-------------+-------------------|device
		     ;;               |--------------------|buffer
		     ;;               ^
		     ;;       index = used-size
		     ;;
		     ;;   dev.tmp-pos = dev.old-pos + index
		     ;;               = port.old-pos
		     ;;
		     ;;taking into account the requested port position:
		     ;;
		     ;;         dev.tmp-pos  dev.new-pos
		     ;;               v         v
		     ;; |-------------+---------+---------|device
		     ;;               |.........|delta-pos
		     ;;                         |--------------------|buffer
		     ;;                         ^
		     ;;                 index = used-size
		     ;;
		     ;;   dev.new-pos = dev.tmp-pos + delta-pos
		     ;;               = port.old-pos + delta-pos
		     ;;               = requested-port-position
		     ;;
		     (begin
		       (%unsafe.flush-output-port port who)
		       requested-port-position)
		   ;;Input  port:  compute  the device  position,  delay
		   ;;resetting the buffer  to after successfully calling
		   ;;SET-POSITION!.  Before moving the position:
		   ;;
		   ;;                           dev.old-pos
		   ;;                                v
		   ;; |------------------------------+----|device
		   ;;                     |..........|delta-idx
		   ;;               |*****+**********+---|buffer
		   ;;                     ^          ^
		   ;;                   index      used-size
		   ;;
		   ;;taking into account the index position:
		   ;;
		   ;;                dev.tmp-pos
		   ;;                     v
		   ;; |-------------------+---------------|device
		   ;;                     |..........|delta-idx
		   ;;                     |--------------------|buffer
		   ;;                     ^
		   ;;               index = used-size
		   ;;
		   ;;   dev.tmp-pos = dev.old-pos - delta-idx
		   ;;
		   ;;taking into account the requested port position:
		   ;;
		   ;;            dev.tmp-pos   dev.new-pos
		   ;;                     v        v
		   ;; |-------------------+--------+------|device
		   ;;                     |........|delta-pos
		   ;;                              |--------------------|buffer
		   ;;                              ^
		   ;;                      index = used-size
		   ;;
		   ;;   dev.new-pos = dev.tmp-pos + delta-pos
		   ;;               = dev.old-pos - delta-idx + delta-pos
		   ;;
		   (let ((delta-idx           (unsafe.fx- port.buffer.used-size port.buffer.index))
			 (device.old-position (%unsafe.device-position/tracked-position who port)))
		     (+ (- device.old-position delta-idx) delta-pos)))))
	    ;;If SET-POSITION!   fails we  can assume nothing  about the
	    ;;position in the device.
	    (set-position! device.new-position)
	    ;;Notice that we clean the port buffer and update the device
	    ;;position   field    AFTER   having   successfully   called
	    ;;SET-POSITION!.
	    (port.buffer.reset-to-empty!)
	    (set! port.device.position device.new-position)))))

    (define-inline (%set-with-boolean port)
      ;;The cookie's POS field holds  a value representing a correct and
      ;;immutable device position.  We move the current port position by
      ;;moving the current buffer index.
      ;;
      ;; dev.pos
      ;;   v
      ;;   |-------------------------------------|device
      ;;   |*******+*****************************|buffer
      ;;   ^       ^                             ^
      ;;   0     index                     used-size = size
      ;;
      ;;Note that  the generally correct implementation of  this case is
      ;;the following, which considers the buffer not being equal to the
      ;;device:
      #|
      (let ((port.old-position (%unsafe.port-position who port)))
	(unless (= port.old-position requested-port-position)
	  (let ((delta-pos (- requested-port-position port.old-position)))
	    (if (<= 0 delta port.buffer.used-size)
		(set! port.buffer.index delta-pos)
	      (%raise-port-position-out-of-range who port requested-port-position)))))
      |#
      ;;but we  know that,  at present, the  only ports  implementing this
      ;;policy  are the  ones returned  by  OPEN-BYTEVECTOR-INPUT-PORT and
      ;;OPEN-STRING-INPUT-PORT, so we can optimise with the following:
      ;;
      (unless (= requested-port-position port.buffer.index)
	(if (<= 0 requested-port-position port.buffer.used-size)
	    (set! port.buffer.index requested-port-position)
	  (%raise-port-position-out-of-range who port requested-port-position))))

    (main)))

(define (%unsafe.reconfigure-input-buffer-to-output-buffer port who)
  ;;Assuming  PORT is  an input  port: set  the device  position  to the
  ;;current port position taking into account the buffer index and reset
  ;;the buffer to  empty.  The device position may  change, but the port
  ;;position is unchanged.
  ;;
  ;;After this function call: the buffer and the device are in the state
  ;;needed to switch an input/output port from input to output.
  ;;
  (with-port (port)
    (let ((set-position! port.set-position!))
      (cond ((procedure? set-position!)
	     ;;An underlying device  exists.  Before moving the position
	     ;;the scenario is:
	     ;;
	     ;;                        dev.old-pos
	     ;;                           v
	     ;;   |-----------------------+-------------|device
	     ;;           |*******+*******+--------| buffer
	     ;;           ^       ^       ^        ^
	     ;;           0     index  used-size  size
	     ;;
	     ;;after setting the device position the scenario is:
	     ;;
	     ;;                dev.new-pos
	     ;;                   v
	     ;;   |---------------+---------------------|device
	     ;;                   |----------------------|buffer
	     ;;                   ^                      ^
	     ;;           0 = index = used-size         size
	     ;;
	     ;;which is fine for an output port with empty buffer.
	     ;;
	     (let* ((device.old-position (%unsafe.device-position/tracked-position who port))
		    (delta-idx           (unsafe.fx- port.buffer.used-size port.buffer.index))
		    (device.new-position (- device.old-position delta-idx)))
	       (set-position! device.new-position)
	       (set! port.device.position device.new-position)
	       (port.buffer.reset-to-empty!)))
	    ((and (boolean? set-position!) set-position!)
	     ;;The  cookie's  POS field  holds  a  value representing  a
	     ;;correct and immutable device position.  For this port the
	     ;;current position is changed  by moving the current buffer
	     ;;index.  So we do nothing here.
	     ;;
	     ;; dev.pos
	     ;;   v
	     ;;   |-------------------------------------|device
	     ;;   |*******+*****************************|buffer
	     ;;   ^       ^                             ^
	     ;;   0     index                     used-size = size
	     ;;
	     (values))
	    (else
	     ;;If  PORT does  not  support the  set  port position  (for
	     ;;example:  it is  a  network socket),  we  just reset  the
	     ;;buffer to empty state.
	     (port.buffer.reset-to-empty!))))))

(define (%unsafe.reconfigure-output-buffer-to-input-buffer port who)
  ;;Assuming  PORT is an  output port:  set the  device position  to the
  ;;current port position taking into account the buffer index and reset
  ;;the buffer to  empty.  The device position may  change, but the port
  ;;position is left unchanged.
  ;;
  ;;After this function call: the buffer and the device are in the state
  ;;needed to switch an input/output port from input to output.
  ;;
  (with-port (port)
    (let ((set-position! port.set-position!))
      (cond ((procedure? set-position!)
	     ;;Upon entering this function the scenario is:
	     ;;
	     ;;        dev.old-pos
	     ;;             v                          device
	     ;;   |---------+---------------------------|
	     ;;             |*****+*******+--------| buffer
	     ;;             ^     ^       ^
	     ;;             0   index  used-size
	     ;;
	     ;;after flushing the buffer the scenario is:
	     ;;
	     ;;                        dev.tmp-pos
	     ;;                           v            device
	     ;;   |-----------------------+-------------|
	     ;;                           |----------------------| buffer
	     ;;                           ^
	     ;;                     0 = index = used-size
	     ;;
	     ;;after adjusting  to keep unchanged the  port position the
	     ;;scenario is:
	     ;;
	     ;;               dev.new-pos
	     ;;                   v                    device
	     ;;   |---------------+---------------------|
	     ;;                   |----------------------| buffer
	     ;;                   ^
	     ;;          0 = index = used-size
	     ;;
	     ;;which is fine for an input port with empty buffer.
	     (let* ((port.old-position   (%unsafe.port-position who port))
		    (device.new-position port.old-position))
               (%unsafe.flush-output-port port who)
	       (set-position! device.new-position)
	       (set! port.device.position device.new-position)
	       (debug-assert (zero? port.buffer.index))
	       (debug-assert (zero? port.buffer.used-size))))
	    ((and (boolean? set-position!) set-position!)
	     ;;The  cookie's  POS field  holds  a  value representing  a
	     ;;correct and immutable device position.  For this port the
	     ;;current position is changed  by moving the current buffer
	     ;;index.  So we do nothing here.
	     ;;
	     ;; dev.pos
	     ;;   v
	     ;;   |-------------------------------------|device
	     ;;   |*******+*****************************|buffer
	     ;;   ^       ^                             ^
	     ;;   0     index                     used-size = size
	     ;;
	     (values))
	    (else
	     ;;If  PORT does  not  support the  set  port position  (for
	     ;;example:  it is  a  netword socket),  we  just reset  the
	     ;;buffer to empty state.
	     (port.buffer.reset-to-empty!))))))


;;;; custom ports

(define (%make-custom-binary-port attributes identifier read! write! get-position set-position! close)
  ;;Build and return  a new custom binary port,  either input or output.
  ;;It is used by the following functions:
  ;;
  ;;	make-custom-binary-input-port
  ;;	make-custom-binary-output-port
  ;;
  (let ((buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector (bytevector-port-buffer-size)))
	(transcoder		#f)
	(cookie			(default-cookie #f)))
    (%port->maybe-guarded-port
     ($make-port (unsafe.fxior attributes GUARDED-PORT-TAG)
		 buffer.index buffer.used-size buffer transcoder identifier
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
	(cookie			(default-cookie #f)))
    (%port->maybe-guarded-port
     ($make-port (unsafe.fxior attributes GUARDED-PORT-TAG)
		 buffer.index buffer.used-size buffer transcoder identifier
		 read! write! get-position set-position! close cookie))))

;;; --------------------------------------------------------------------

(define (make-custom-binary-input-port identifier read! get-position set-position! close)
  ;;Defined by  R6RS.  Return  a newly created  binary input  port whose
  ;;octet  source is  an arbitrary  algorithm represented  by  the READ!
  ;;procedure.  ID  must be a string  naming the new  port, provided for
  ;;informational purposes  only.  READ! must be a  procedure and should
  ;;behave  as specified  below; it  will be  called by  operations that
  ;;perform binary input.
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
  (with-arguments-validation (who)
      ((port-identifier			identifier)
       (read!-procedure			read!)
       (maybe-close-procedure		close)
       (maybe-get-position-procedure	get-position)
       (maybe-set-position!-procedure	set-position!))
    (let ((attributes		BINARY-INPUT-PORT-BITS)
	  (write!			#f))
      (%make-custom-binary-port attributes identifier read! write! get-position set-position! close))))

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
  (with-arguments-validation (who)
      ((port-identifier			identifier)
       (write!-procedure		write!)
       (maybe-close-procedure		close)
       (maybe-get-position-procedure	get-position)
       (maybe-set-position!-procedure	set-position!))
    (let ((attributes		BINARY-OUTPUT-PORT-BITS)
	  (read!			#f))
      (%make-custom-binary-port attributes identifier read! write! get-position set-position! close))))

(define (make-custom-binary-input/output-port identifier read! write! get-position set-position! close)
  ;;Defined by  R6RS.  Return a  newly created binary  input/output port
  ;;whose byte  source and sink are arbitrary  algorithms represented by
  ;;the READ! and WRITE!  procedures.
  ;;
  ;;ID must be a string  naming the new port, provided for informational
  ;;purposes only.
  ;;
  ;;READ! and WRITE!  must be procedures, and should behave as specified
  ;;for          the          MAKE-CUSTOM-BINARY-INPUT-PORT          and
  ;;MAKE-CUSTOM-BINARY-OUTPUT-PORT procedures.
  ;;
  ;;Each  of the  remaining  arguments may  be  false; if  any of  those
  ;;arguments is not false, it must  be a procedure and should behave as
  ;;specified in the description of MAKE-CUSTOM-BINARY-INPUT-PORT.
  ;;
  (define who 'make-custom-binary-input/output-port)
  (with-arguments-validation (who)
      ((port-identifier			identifier)
       (read!-procedure			read!)
       (write!-procedure		write!)
       (maybe-close-procedure		close)
       (maybe-get-position-procedure	get-position)
       (maybe-set-position!-procedure	set-position!))
    (let ((attributes (unsafe.fxior BINARY-OUTPUT-PORT-BITS INPUT/OUTPUT-PORT-TAG)))
      (%make-custom-binary-port attributes identifier read! write! get-position set-position! close))))

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
  (with-arguments-validation (who)
      ((port-identifier			identifier)
       (read!-procedure			read!)
       (maybe-close-procedure		close)
       (maybe-get-position-procedure	get-position)
       (maybe-set-position!-procedure	set-position!))
    (let ((attributes	FAST-GET-CHAR-TAG)
	  (write!	#f))
      (%make-custom-textual-port attributes identifier read! write! get-position set-position! close))))

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
  (with-arguments-validation (who)
      ((port-identifier			identifier)
       (write!-procedure		write!)
       (maybe-close-procedure		close)
       (maybe-get-position-procedure	get-position)
       (maybe-set-position!-procedure	set-position!))
    (let ((attributes	FAST-PUT-CHAR-TAG)
	  (read!	#f))
      (%make-custom-textual-port attributes identifier read! write! get-position set-position! close))))

(define (make-custom-textual-input/output-port identifier read! write! get-position set-position! close)
  ;;Defined by  R6RS.  Return a newly created  textual input/output port
  ;;whose textual  source and sink are  arbitrary algorithms represented
  ;;by the READ! and WRITE! procedures.
  ;;
  ;;IDENTIFIER  must be  a  string  naming the  new  port, provided  for
  ;;informational purposes only.
  ;;
  ;;READ!   and  WRITE!   must  be  procedures,  and  should  behave  as
  ;;specified     for     the     MAKE-CUSTOM-TEXTUAL-INPUT-PORT     and
  ;;MAKE-CUSTOM-TEXTUAL-OUTPUT-PORT procedures.
  ;;
  ;;Each  of the  remaining  arguments may  be  false; if  any of  those
  ;;arguments is not false, it must  be a procedure and should behave as
  ;;specified in the description of MAKE-CUSTOM-TEXTUAL-INPUT-PORT.
  ;;
  (define who 'make-custom-textual-input/output-port)
  (with-arguments-validation (who)
      ((port-identifier			identifier)
       (read!-procedure			read!)
       (write!-procedure		write!)
       (maybe-close-procedure		close)
       (maybe-get-position-procedure	get-position)
       (maybe-set-position!-procedure	set-position!))
    (let ((attributes (unsafe.fxior FAST-PUT-CHAR-TAG INPUT/OUTPUT-PORT-TAG)))
      (%make-custom-textual-port attributes identifier read! write! get-position set-position! close))))


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
    (with-arguments-validation (who)
	((bytevector        bv)
	 (maybe-transcoder  maybe-transcoder))
      ;;The input bytevector  is itself the buffer!!!  The  port is in a
      ;;state equivalent to the following:
      ;;
      ;;                                           device position
      ;;                                                  v
      ;;   |----------------------------------------------| device
      ;;   |*******************+**************************| buffer
      ;;   ^                   ^                          ^
      ;;   0            index = port position       used-size = size
      ;;
      ;;the device  position equals the bytevector length  and its value
      ;;in the cookie's POS field is never mutated.
      (let* ((bv.len (unsafe.bytevector-length bv))
	     (attributes		(unsafe.fxior
					 (%select-input-fast-tag-from-transcoder who maybe-transcoder)
					 (%select-eol-style-from-transcoder who maybe-transcoder)))
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

(define open-string-input-port
  ;;Defined by  R6RS, extended by  Vicare.  Return a textual  input port
  ;;whose characters are  drawn from STR.  The port may  or may not have
  ;;an   associated  transcoder;   if   it  does,   the  transcoder   is
  ;;implementation--dependent.     The   port    should    support   the
  ;;PORT-POSITION and SET-PORT-POSITION!  operations.
  ;;
  ;;If STR is modified after OPEN-STRING-INPUT-PORT has been called, the
  ;;effect on the returned port is unspecified.
  ;;
  (case-lambda
   ((str)
    (open-string-input-port/id str "*string-input-port*" (eol-style none)))
   ((str eol-style)
    (open-string-input-port/id str "*string-input-port*" eol-style))))

(define open-string-input-port/id
  ;;Defined   by  Ikarus.    For  details   see  the   documentation  of
  ;;OPEN-STRING-INPUT-PORT.
  ;;
  ;;In this port there is no  underlying device: the input string is set
  ;;as the buffer.
  ;;
  (case-lambda
   ((str id)
    (open-string-input-port/id str id 'none))
   ((str id eol-style)
    (define who 'open-string-input-port)
    (with-arguments-validation (who)
	((string	   str)
	 (port-identifier  id))
      ;;The input string is itself the buffer!!!  The port is in a state
      ;;equivalent to the following:
      ;;
      ;;                                           device position
      ;;                                                  v
      ;;   |----------------------------------------------| device
      ;;   |*******************+**************************| buffer
      ;;   ^                   ^                          ^
      ;;   0            index = port position       used-size = size
      ;;
      ;;the device  position equals the  string length and its  value in
      ;;the cookie's POS field is never mutated.
      (let ((str.len (string-length str)))
	(unless (< str.len BUFFER-SIZE-UPPER-LIMIT)
	  (error who "input string length exceeds maximum supported size" str.len))
	(let ((attributes	(unsafe.fxior
				 FAST-GET-CHAR-TAG
				 (or (%symbol->eol-attrs eol-style)
				     (assertion-violation who
				       "expected EOL style as argument" eol-style))
				 DEFAULT-OTHER-ATTRS))
	      (buffer.index	0)
	      (buffer.used-size	str.len)
	      (buffer		str)
	      (transcoder	#t)
	      (read!		all-data-in-buffer)
	      (write!		#f)
	      (get-position	#t)
	      (set-position!	#t)
	      (close		#f)
	      (cookie		(default-cookie #f)))
	  (set-cookie-pos! cookie str.len)
	  ($make-port attributes buffer.index buffer.used-size buffer transcoder id
		      read! write! get-position set-position! close cookie)))))))

(define (with-input-from-string string thunk)
  ;;Defined by Ikarus.   THUNK must be a procedure  and must accept zero
  ;;arguments.  STRING must be a Scheme string, and THUNK is called with
  ;;no arguments.
  ;;
  ;;The STRING  is used  as argument for  OPEN-STRING-INPUT-PORT; during
  ;;the dynamic extent  of the call to THUNK, the  obtained port is made
  ;;the  value returned  by procedure  CURRENT-INPUT-PORT;  the previous
  ;;default value is reinstated when the dynamic extent is exited.
  ;;
  ;;When THUNK  returns, the port  is closed automatically.   The values
  ;;returned by THUNK are returned.
  ;;
  ;;If an escape procedure is used to escape back into the call to THUNK
  ;;after THUNK is returned, the behavior is unspecified.
  ;;
  (define who 'with-input-from-string)
  (with-arguments-validation (who)
      ((string    string)
       (procedure thunk))
    (parameterize ((current-input-port (open-string-input-port string)))
      (thunk))))


;;;; generic port functions

(define (call-with-port port proc)
  ;;Defined by R6RS.  PROC must accept one argument.  The CALL-WITH-PORT
  ;;procedure calls PROC with PORT as an argument.
  ;;
  ;;If  PROC  returns,  PORT  is  closed automatically  and  the  values
  ;;returned by PROC are returned.
  ;;
  ;;If PROC  does not return,  PORT is not closed  automatically, except
  ;;perhaps when it  is possible to prove that PORT  will never again be
  ;;used for an input or output operation.
  ;;
  (define who 'call-with-port)
  (with-arguments-validation (who)
      ((port		 port)
       (unsafe.open-port port)
       (procedure	 proc))
    (call-with-values
	(lambda ()
	  (proc port))
      (lambda vals
	(%unsafe.close-port port who)
	(apply values vals)))))


;;;; bytevector output ports

(define open-bytevector-output-port
  (case-lambda
   (()
    (open-bytevector-output-port #f))
   ((maybe-transcoder)
    ;;Defined by R6RS.  MAYBE-TRANSCODER  must be either a transcoder or
    ;;false.
    ;;
    ;;The OPEN-BYTEVECTOR-OUTPUT-PORT  procedure returns two  values: an
    ;;output  port  and  an   extraction  procedure.   The  output  port
    ;;accumulates the  bytes written to  it for later extraction  by the
    ;;procedure.
    ;;
    ;;If  MAYBE-TRANSCODER is  a transcoder,  it becomes  the transcoder
    ;;associated with the port.  If MAYBE-TRANSCODER is false or absent,
    ;;the port will be a  binary port and will support the PORT-POSITION
    ;;and SET-PORT-POSITION!  operations.  Otherwise  the port will be a
    ;;textual  port,  and  whether  it supports  the  PORT-POSITION  and
    ;;SET-PORT-POSITION!   operations is  implementation  dependent (and
    ;;possibly transcoder-dependent).
    ;;
    ;;The  extraction procedure  takes  no arguments.   When called,  it
    ;;returns  a bytevector  consisting  of all  the port's  accumulated
    ;;bytes  (regardless of  the port's  current position),  removes the
    ;;accumulated bytes from the port, and resets the port's position.
    ;;
    (define who 'open-bytevector-output-port)
    (with-arguments-validation (who)
	((maybe-transcoder maybe-transcoder))
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
      ;;for  this reason  we  implement the  device of  the  port to  be
      ;;somewhat efficient for such use.
      ;;
      ;;The device is a struct instance  stored in the cookie; the field
      ;;BVS holds null  or a list of accumulated  bytevectors; the field
      ;;LEN holds the total number of bytes accumulated so far.
      ;;
      ;;Whenever data  is flushed from the  buffer to the device:  a new
      ;;bytevector  is   prepended  to  BVS,  and   LEN  is  incremented
      ;;accordingly.   When  the extract  function  is  invoked: BVS  is
      ;;reversed  and  concatenated  obtaining a  single  bytevector  of
      ;;length LEN.
      ;;
      ;;There is only  one function that can be applied  to the port and
      ;;causes  the   device  to   be  handled   in  a   different  way:
      ;;SET-PORT-POSITION!.  When  the new  port position is  before the
      ;;end of the data: SET-POSITION!  converts BVS to a list holding a
      ;;single full bytevector (LEN is left unchanged).
      ;;
      ;;Whenever BVS holds a single  bytevector and the position is less
      ;;than such  bytevector length:  it means  that SET-PORT-POSITION!
      ;;was used.
      ;;
      ;;Remember that bytevectors  hold at most a number  of bytes equal
      ;;to the return value of GREATEST-FIXNUM.
      ;;
      (define-struct device
	(len bvs))
      (let ((cookie (default-cookie (make-device 0 '()))))
	;;Notice  how  the  internal  functions are  closures  upon  the
	;;cookie, not the port.
	;;
	(define (%serialise-device! who reset?)
	  (let ((D (cookie-dest cookie)))
	    (cond ((unsafe.fxzero? ($device-len D))
		   ;;No bytes accumulated, the device is empty.
		   '#vu8())
		  ((null? (cdr ($device-bvs D)))
		   ;;The device  contains some bytes and  it has already
		   ;;been serialised: the list of bytevectors contains a
		   ;;single item.
		   (when reset?
		     (set-cookie-dest! cookie (make-device 0 '())))
		   ;;Return the single bytevector in the list.
		   (car ($device-bvs D)))
		  (else
		   ;;The device contains some bytes and the device needs
		   ;;to be serialised: the  list of bytevectors contains
		   ;;2 or more items.
		   (let ((bv (%unsafe.bytevector-reverse-and-concatenate
			      who ($device-bvs D) ($device-len D))))
		     ($set-cookie-dest! cookie
					(if reset?
					    (make-device 0 '())
					  (make-device ($device-len D) (list bv))))
		     bv)))))

	(define (write! src.bv src.start count)
	  ;;Write data to the device.
	  ;;
	  (define who 'open-bytevector-output-port/write!)
	  (debug-assert (and (fixnum? count) (<= 0 count)))
	  (let* ((D		(cookie-dest cookie))
		 (dev-position	(cookie-pos cookie))
		 (new-dev-position (+ count dev-position)))
	    ;;DANGER  This  check must  not  be  removed when  compiling
	    ;;without arguments validation.
	    (unless (fixnum? new-dev-position)
	      (%implementation-violation who
		"request to write data to port would exceed maximum size of bytevectors"
		new-dev-position))
	    (if (unsafe.fx< dev-position ($device-len D))
		;;The  current  position  was  set  inside  the  already
		;;accumulated data.
		(write!/overwrite src.bv src.start count
				  ($device-len D) ($device-bvs D)
				  dev-position new-dev-position)
	      ;;The  current  position is  at  the  end  of the  already
	      ;;accumulated data.
	      (write!/append src.bv src.start count ($device-bvs D) new-dev-position))
	    count))

	(define-inline (write!/overwrite src.bv src.start count
					 output.len output.bvs
					 dev-position new-dev-position)
	  ;;Write data  to the device,  overwriting some of  the already
	  ;;existing data.  The  device is already composed  of a single
	  ;;bytevector of length OUTPUT.LEN in the car of OUTPUT.BVS.
	  ;;
	  (let* ((dst.bv   (car output.bvs))
		 (dst.len  output.len)
		 (dst.room (unsafe.fx- dst.len dev-position)))
	    (debug-assert (fixnum? dst.room))
	    (if (unsafe.fx<= count dst.room)
		;;The new data fits  in the single bytevector.  There is
		;;no need to update the device.
		(unsafe.bytevector-copy!/count src.bv src.start dst.bv dev-position count)
	      (begin
		;;The new  data goes part  in the single  bytevector and
		;;part  in a  new  bytevector.  We  need  to update  the
		;;device.
		(unsafe.bytevector-copy!/count src.bv src.start dst.bv dev-position dst.room)
		(let* ((src.start (unsafe.fx+ src.start dst.room))
		       (count     (unsafe.fx- count     dst.room)))
		  (write!/append src.bv src.start count output.bvs new-dev-position))))))

	(define-inline (write!/append src.bv src.start count output.bvs new-dev-position)
	  ;;Append new data to  the accumulated bytevectors.  We need to
	  ;;update the device.
	  ;;
	  (let ((dst.bv (unsafe.make-bytevector count)))
	    (unsafe.bytevector-copy!/count src.bv src.start dst.bv 0 count)
	    (set-cookie-dest! cookie `(,new-dev-position . (,dst.bv . ,output.bvs)))))

	(define (set-position! new-position)
	  ;;NEW-POSITION has already been  validated as exact integer by
	  ;;the  procedure SET-PORT-POSITION!.   The buffer  has already
	  ;;been flushed  by SET-PORT-POSITION!.   Here we only  have to
	  ;;verify  that  the  value  is  valid  as  offset  inside  the
	  ;;underlying  full bytevector.   If this  validation succeeds:
	  ;;SET-PORT-POSITION!  will store the position in the cookie.
	  ;;
	  (define who 'open-bytevector-output-port/set-position!)
	  (unless (and (fixnum? new-position)
		       (or (unsafe.fx=  new-position (cookie-pos cookie))
			   (unsafe.fx<= new-position
					(unsafe.bytevector-length (%serialise-device! who #f)))))
	    (raise (condition
		    (make-who-condition who)
		    (make-message-condition "attempt to set bytevector output port position beyond limit")
		    (make-i/o-invalid-position-error new-position)))))

	(let* ((attributes	(%select-output-fast-tag-from-transcoder
				 who maybe-transcoder PORT-WITH-EXTRACTION-TAG
				 (%select-eol-style-from-transcoder who maybe-transcoder)
				 DEFAULT-OTHER-ATTRS))
	       (buffer.index	0)
	       (buffer.used-size 0)
	       (buffer		(unsafe.make-bytevector (bytevector-port-buffer-size)))
	       (identifier	"*bytevector-output-port*")
	       (read!		#f)
	       (get-position	#t)
	       (close		#f))

	  (define port
	    ($make-port attributes buffer.index buffer.used-size buffer
			maybe-transcoder identifier
			read! write! get-position set-position! close cookie))

	  (define (extract)
	    ;;The extraction  function.  Flush the buffer  to the device
	    ;;list,  convert the  device  list to  a single  bytevector.
	    ;;Return  the single bytevector  and reset  the port  to its
	    ;;empty state.
	    ;;
	    ;;This function  can be called  also when the port  has been
	    ;;closed.
	    ;;
	    (define who 'open-bytevector-output-port/extract)
	    (with-port (port)
	      (%unsafe.flush-output-port port who)
	      (let ((bv (%serialise-device! who #t)))
		(port.buffer.reset-to-empty!)
		(set! port.device.position 0)
		bv)))

	  (values port extract)))))))

(define call-with-bytevector-output-port
  (case-lambda
   ((proc)
    (call-with-bytevector-output-port proc #f))
   ((proc transcoder)
    ;;Defined by R6RS.  PROC must accept one argument.  MAYBE-TRANSCODER
    ;;must be either a transcoder or false.
    ;;
    ;;The  CALL-WITH-BYTEVECTOR-OUTPUT-PORT procedure creates  an output
    ;;port that accumulates the bytes  written to it and calls PROC with
    ;;that output port as an argument.
    ;;
    ;;Whenever  PROC returns,  a  bytevector consisting  of  all of  the
    ;;port's  accumulated  bytes   (regardless  of  the  port's  current
    ;;position) is returned and the port is closed.
    ;;
    ;;The transcoder  associated with the  output port is  determined as
    ;;for a call to OPEN-BYTEVECTOR-OUTPUT-PORT.
    ;;
    (define who 'call-with-bytevector-output-port)
    (with-arguments-validation (who)
	((procedure		proc)
	 (maybe-transcoder	transcoder))
      (let-values (((port extract) (open-bytevector-output-port transcoder)))
	(proc port)
	(extract))))))


;;;; string output ports

(define open-string-output-port
  ;;Defined by  R6RS.  Return two values:  a textual output  port and an
  ;;extraction  procedure.  The output  port accumulates  the characters
  ;;written to it for later extraction by the procedure.
  ;;
  ;;The port may  or may not have an associated  transcoder; if it does,
  ;;the transcoder is implementation-dependent.  The port should support
  ;;the PORT-POSITION and SET-PORT-POSITION!  operations.
  ;;
  ;;The  extraction  procedure  takes  no arguments.   When  called,  it
  ;;returns  a  string  consisting  of  all of  the  port's  accumulated
  ;;characters  (regardless  of   the  current  position),  removes  the
  ;;accumulated  characters  from  the   port,  and  resets  the  port's
  ;;position.
  ;;
  ;;IMPLEMENTATION  RESTRICTION  The  accumulated  string  can  have  as
  ;;maximum  length the  greatest fixnum,  which means  that  the device
  ;;position also can be at most the greatest fixnum.
  ;;
  (case-lambda
   (()
    (open-string-output-port (eol-style none)))
   ((eol-style)
    (define who 'open-string-output-port)
    (let ((cookie (default-cookie '(0 . ()))))
      ;;The most  common use of this  port type is  to append characters
      ;;and finally extract the whole output bytevector:
      ;;
      ;;  (call-with-values
      ;;      open-string-output-port
      ;;    (lambda (port extract)
      ;;      (put-string port "123")
      ;;      ...
      ;;      (extract)))
      ;;
      ;;for  this reason  we  implement the  device  of the  port to  be
      ;;somewhat efficient for such use.
      ;;
      ;;The  device is  a pair  stored in  the cookie;  the  cdr, called
      ;;OUTPUT.STRS  in the  code holds  null or  a list  of accumulated
      ;;strings; the car, called OUTPUT.LEN in the code, holds the total
      ;;number of characters accumulated so far.
      ;;
      ;;Whenever data  is flushed from the  buffer to the  device: a new
      ;;string is prepended to OUTPUT.STRS and OUTPUT.LEN is incremented
      ;;accordingly.  When the  extract function is invoked: OUTPUT.STRS
      ;;is reversed and concatenated obtaining a single string of length
      ;;OUTPUT.LEN.
      ;;
      ;;This situation is violated  if SET-PORT-POSITION!  is applied to
      ;;the port  to move the position  before the end of  the data.  If
      ;;this  happens:  SET-POSITION!  converts  OUTPUT.STRS  to a  list
      ;;holding a single full string (OUTPUT.LEN is left unchanged).
      ;;
      ;;Whenever OUTPUT.STRS  holds a single string and  the position is
      ;;less than  such string length: it  means that SET-PORT-POSITION!
      ;;was used.
      ;;
      ;;Remember that strings hold at most (GREATEST-FIXNUM) characters.
      ;;
      (define (%%serialise-device! who reset?)
	(let* ((dev		(cookie-dest cookie))
	       (output.len	(car dev))
	       (output.strs	(cdr dev)))
	  (cond ((unsafe.fxzero? output.len)
		 ;;No bytes accumulated, the device is empty.
		 "")
		((null? (cdr output.strs))
		 ;;The device has already been serialised.
		 (when reset?
		   (set-cookie-dest! cookie '(0 . ())))
		 (car output.strs))
		(else
		 (let ((str (%unsafe.string-reverse-and-concatenate who output.strs output.len)))
		   (set-cookie-dest! cookie (if reset? '(0 . ()) `(,output.len . (,str))))
		   str)))))
      (define-inline (%serialise-device! who)
	(%%serialise-device! who #f))
      (define-inline (%serialise-device-and-reset! who)
	(%%serialise-device! who #t))

      (define (write! src.str src.start count)
	;;Write data to the device.
	;;
	(define who 'open-string-output-port/write!)
	(debug-assert (and (fixnum? count) (<= 0 count)))
	(let* ((dev		(cookie-dest cookie))
	       (output.len	(car dev))
	       (output.strs	(cdr dev))
	       (dev-position	(cookie-pos cookie))
	       (new-dev-position	(+ count dev-position)))
	  ;;DANGER This check must not be removed when compiling without
	  ;;arguments validation.
	  (unless (fixnum? new-dev-position)
	    (%implementation-violation who
	      "request to write data to port would exceed maximum size of strings" new-dev-position))
	  (if (unsafe.fx< dev-position output.len)
	      ;;The  current   position  was  set   inside  the  already
	      ;;accumulated data.
	      (write!/overwrite src.str src.start count output.len output.strs dev-position
				new-dev-position)
	    ;;The  current  position  is  at  the  end  of  the  already
	    ;;accumulated data.
	    (write!/append src.str src.start count output.strs new-dev-position))
	  count))

      (define-inline (write!/overwrite src.str src.start count output.len output.strs dev-position
				       new-dev-position)
	;;Write  data to  the device,  overwriting some  of  the already
	;;existing  data.  The device  is already  composed of  a single
	;;string of length OUTPUT.LEN in the car of OUTPUT.STRS.
	;;
	(let* ((dst.str  (car output.strs))
	       (dst.len  output.len)
	       (dst.room (unsafe.fx- dst.len dev-position)))
	  (debug-assert (fixnum? dst.room))
	  (if (unsafe.fx<= count dst.room)
	      ;;The new  data fits  in the single  string.  There  is no
	      ;;need to update the device.
	      (unsafe.string-copy!/count src.str src.start dst.str dev-position count)
	    (begin
	      ;;The new data goes part  in the single string and part in
	      ;;a new string.  We need to update the device.
	      (unsafe.string-copy!/count src.str src.start dst.str dev-position dst.room)
	      (let* ((src.start (unsafe.fx+ src.start dst.room))
		     (count     (unsafe.fx- count     dst.room)))
		(write!/append src.str src.start count output.strs new-dev-position))))))

      (define-inline (write!/append src.str src.start count output.strs new-dev-position)
	;;Append new data to the accumulated strings.  We need to update
	;;the device.
	;;
	(let ((dst.str (unsafe.make-string count)))
	  (unsafe.string-copy!/count src.str src.start dst.str 0 count)
	  (set-cookie-dest! cookie `(,new-dev-position . (,dst.str . ,output.strs)))))

      (define (set-position! new-position)
	;;NEW-POSITION has  already been  validated as exact  integer by
	;;the procedure SET-PORT-POSITION!.  The buffer has already been
	;;flushed by  SET-PORT-POSITION!.  Here  we only have  to verify
	;;that the value  is valid as offset inside  the underlying full
	;;string.  If this validation succeeds: SET-PORT-POSITION!  will
	;;store the position in the cookie.
	;;
	(define who 'open-string-output-port/set-position!)
	(unless (and (fixnum? new-position)
		     (or (unsafe.fx=  new-position (cookie-pos cookie))
			 (unsafe.fx<= new-position (unsafe.string-length (%serialise-device! who)))))
	  (raise (condition
		  (make-who-condition who)
		  (make-message-condition "attempt to set string output port position beyond limit")
		  (make-i/o-invalid-position-error new-position)))))

      (let* ((attributes	(unsafe.fxior FAST-PUT-CHAR-TAG
					       (or (%symbol->eol-attrs eol-style)
						   (assertion-violation who
						     "expected EOL style as argument" eol-style))
					       DEFAULT-OTHER-ATTRS))
	     (buffer.index	0)
	     (buffer.used-size	0)
	     (buffer		(unsafe.make-string (string-port-buffer-size))
				#;(make-string (string-port-buffer-size) #\Z))
	     (identifier	"*string-output-port*")
	     (transcoder	#f)
	     (read!		#f)
	     (get-position	#t)
	     (close		#f))

	(define port
	  ($make-port attributes buffer.index buffer.used-size buffer transcoder identifier
		      read! write! get-position set-position! close cookie))

	(define (extract)
	  ;;The  extraction function.   Flush the  buffer to  the device
	  ;;list, convert  the device list  to a single  string.  Return
	  ;;the single string and reset the port to its empty state.
	  ;;
	  ;;This  function can  be called  also when  the port  has been
	  ;;closed.
	  ;;
	  (define who 'open-string-output-port/extract)
	  (%unsafe.flush-output-port port who)
	  (with-port (port)
	    (let ((str (%serialise-device-and-reset! who)))
	      (port.buffer.reset-to-empty!)
	      (set! port.device.position 0)
	      str)))

	(values port extract))))))

(define (get-output-string port)
  ;;Defined by Ikarus.  Return the string accumulated in the PORT opened
  ;;by OPEN-STRING-OUTPUT-PORT.
  ;;
  ;;This function can be called also when the port has been closed.
  ;;
  (define who 'get-output-string)
  (define (wrong-port-error)
    (assertion-violation who "not an output-string port" port))
  (with-arguments-validation (who)
      ((port port))
    (with-port-having-string-buffer (port)
      (unless (%unsafe.textual-output-port? port)
	(wrong-port-error))
      (let ((cookie port.cookie))
	(unless (cookie? cookie)
	  (wrong-port-error))
	(let ((dev port.device))
	  (unless (and (pair?   dev)
		       (fixnum? (car dev))
		       (or (null? (cdr dev))
			   (pair? (cdr dev))))
	    (wrong-port-error)))
	(%unsafe.flush-output-port port who)
	(let* ((dev		port.device) ;flushing the buffer can change the device!!!
	       (output.len	(car dev))
	       (output.strs	(cdr dev)))
	  (set! port.device.position 0)
	  (set! port.device '(0 . ()))
	  (cond ((unsafe.fxzero? output.len)
		 ;;No bytes accumulated, the device is empty.
		 "")
		((null? (cdr output.strs))
		 ;;The device has already been serialised.
		 (car output.strs))
		(else
		 (%unsafe.string-reverse-and-concatenate who output.strs output.len))))))))

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
  (with-arguments-validation (who)
      ((procedure proc))
    (let-values (((port getter) (open-string-output-port)))
      (proc port)
      (getter))))

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
  (with-arguments-validation (who)
      ((procedure proc))
    (let-values (((port extract) (open-string-output-port)))
      (parameterize ((current-output-port port))
	(proc))
      (extract))))


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
  (with-arguments-validation (who)
      ((procedure		proc)
       (output-port		port)
       (unsafe.textual-port	port))
    (parameterize ((current-output-port port))
      (proc))))


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
  (with-arguments-validation (who)
      ((port				port)
       (transcoder			transcoder)
       (unsafe.binary-port		port)
       (unsafe.open-port		port)
       (unsafe.not-input/output-port	port))
    (with-port-having-bytevector-buffer (port)
      (let ((transcoded-port ($make-port
			      (unsafe.fxior
			       (cond (port.last-operation-was-output?
				      (%select-output-fast-tag-from-transcoder who transcoder))
				     (port.last-operation-was-input?
				      (%select-input-fast-tag-from-transcoder who transcoder))
				     (else
				      (assertion-violation who "port is neither input nor output!" port)))
			       (%unsafe.port-nullify-eol-style-bits port.other-attributes)
			       (%select-eol-style-from-transcoder who transcoder))
			      port.buffer.index port.buffer.used-size port.buffer
			      transcoder port.id
			      port.read! port.write! port.get-position port.set-position! port.close
			      port.cookie)))
	;;If the binary  port accumulates bytes for later  extraction by a
	;;function, we  flush the  buffer now and  set the buffer  mode to
	;;none;  this way  the  buffer is  kept  empty by  all the  output
	;;functions.   This  allows the  original  extraction function  to
	;;still process correctly the device.
	(when port.with-extraction?
	  (%unsafe.flush-output-port port who)
	  ($set-port-buffer-mode-to-none! transcoded-port))
	(port.mark-as-closed!)
	(%port->maybe-guarded-port transcoded-port)))))

(define (port-transcoder port)
  ;;Defined by R6RS.  Return  the transcoder associated with PORT
  ;;if  PORT is  textual and  has an  associated  transcoder, and
  ;;returns  false  if  PORT  is  binary  or  does  not  have  an
  ;;associated transcoder.
  ;;
  (define who 'port-transcoder)
  (with-arguments-validation (who)
      ((port port))
    (with-port (port)
      (let ((tr port.transcoder))
	(and (transcoder? tr) tr)))))


;;;; closing ports

(define (port-closed? port)
  ;;Defined  by Ikarus.   Return true  if PORT  has  already been
  ;;closed.
  ;;
  (define who 'port-closed?)
  (with-arguments-validation (who)
      ((port port))
    (%unsafe.port-closed? port)))

(define (%unsafe.port-closed? port)
  (with-port (port)
    (unsafe.fx= (unsafe.fxand port.attributes CLOSED-PORT-TAG) CLOSED-PORT-TAG)))

(define (close-port port)
  ;;Defined  by  R6RS.   Closes  the  port,  rendering  the  port
  ;;incapable  of delivering or  accepting data.   If PORT  is an
  ;;output port, it is flushed  before being closed.  This has no
  ;;effect if the port has already been closed.  A closed port is
  ;;still a  port.  The CLOSE-PORT  procedure returns unspecified
  ;;values.
  ;;
  (define who 'close-port)
  (with-arguments-validation (who)
      ((port port))
    (%unsafe.close-port port who)))

(define (close-input-port port)
  ;;Define by R6RS.  Close an input port.
  ;;
  (define who 'close-input-port)
  (with-arguments-validation (who)
      ((input-port port))
    (%unsafe.close-port port who)))

(define (close-output-port port)
  ;;Define by R6RS.  Close an output port.
  ;;
  (define who 'close-output-port)
  (with-arguments-validation (who)
      ((output-port port))
    (%unsafe.close-port port who)))

(define (%unsafe.close-port port who)
  ;;Subroutine     for    CLOSE-PORT,     CLOSE-INPUT-PORT    and
  ;;CLOSE-OUTPUT-PORT.  Assume that PORT is a port object.
  ;;
  ;;Flush data in  the buffer to the underlying  device, mark the
  ;;port as closed and finally call the port's CLOSE function, if
  ;;any.
  ;;
  (with-port (port)
    (unless port.closed?
      (when port.last-operation-was-output?
	(%unsafe.flush-output-port port who))
      (port.mark-as-closed!)
      (when (procedure? port.close)
	(port.close)))))


;;;; auxiliary port functions

(define (port-id port)
  ;;Defined by Ikarus.  Return the string identifier of a port.
  ;;
  (define who 'port-id)
  (with-arguments-validation (who)
      ((port port))
    (with-port (port)
      port.id)))

(module (port-uid port-hash)

  (define (port-uid port)
    ;;Defined by Vicare.  Return a gensym uniquely associated the port.
    ;;
    (define who 'port-uid)
    (with-arguments-validation (who)
	((port port))
      (%port-uid port)))

  (define (port-hash port)
    ;;Defined by Vicare.  Return a hash value for a port.
    ;;
    (define who 'port-hash)
    (with-arguments-validation (who)
	((port port))
      (with-port (port)
	(let ((hash (cookie-hash port.cookie)))
	  (or hash
	      (let ((hash (symbol-hash (%port-uid port))))
		(set-cookie-hash! port.cookie hash)
		hash))))))

  (define (%port-uid port)
    (with-port (port)
      (let ((uid (cookie-uid port.cookie)))
	(or uid
	    (let ((uid (gensym "port")))
	      (set-cookie-uid! port.cookie uid)
	      uid)))))

  #| end of module |# )

(define (port-mode port)
  ;;Defined by Ikarus.  The port mode is used only by the reader.
  ;;
  (define who 'port-mode)
  (with-arguments-validation (who)
      ((port port))
    (with-port (port)
      port.mode)))

(define (set-port-mode! port mode)
  ;;Defined by Ikarus.  The port mode is used only by the reader.
  ;;
  (define who 'set-port-mode!)
  (with-arguments-validation (who)
      ((port      port)
       (port-mode mode))
    (with-port (port)
      (set! port.mode mode))))

(define (port-eof? port)
  ;;Defined by  R6RS.  PORT  must be  an input port.   Return #t  if the
  ;;LOOKAHEAD-U8  procedure   (if  PORT  is   a  binary  port)   or  the
  ;;LOOKAHEAD-CHAR procedure  (if PORT is  a textual port)  would return
  ;;the  EOF  object,  and   #f  otherwise.   The  operation  may  block
  ;;indefinitely  if  no  data  is  available but  the  port  cannot  be
  ;;determined to be at end of file.
  ;;
  (define who 'port-eof?)
  (with-arguments-validation (who)
      ((port              port)
       (unsafe.input-port port)
       (unsafe.open-port  port))
    (with-port (port)
      ;;Checking the buffer status is the fastest path to the result.
      (cond ((unsafe.fx< port.buffer.index port.buffer.used-size)
	     #f)
	    (port.transcoder
	     (eof-object? (lookahead-char port)))
	    (else
	     (eof-object? (lookahead-u8 port)))))))

(define (port-dump-status port)
  (define port
    (current-error-port))
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%newline)
    (newline port))
  (with-port (port)
    (%display "port-id: ")			(%display (port-id port))
    (%newline)
    (%display "port.buffer.index: ")		(%display port.buffer.index)
    (%newline)
    (%display "port.buffer.used-size: ")	(%display port.buffer.used-size)
    (%newline)
    ))


;;;; buffer mode

(define (output-port-buffer-mode port)
  ;;Defined  by  R6RS.  Return  the  symbol  that represents  the
  ;;buffer mode of PORT.
  ;;
  (define who 'output-port-buffer-mode)
  (with-arguments-validation (who)
      ((output-port port))
    (with-port (port)
      (cond (port.buffer-mode-line?	'line)
	    (port.buffer-mode-none?	'none)
	    (else			'block)))))

(define (set-port-buffer-mode! port mode)
  ;;Defined by Vicare.  Reset the port buffer mode.
  ;;
  (define who 'set-port-buffer-mode!)
  (with-arguments-validation (who)
      ((port port))
    (with-port (port)
      (case mode
	((line)
	 (with-arguments-validation (who)
	     ((unsafe.textual-port port))
	   (set! port.attributes (unsafe.fxior (%unsafe.port-nullify-eol-style-bits port.attributes)
					       BUFFER-MODE-LINE-TAG))))
	((none)
	 (set! port.attributes (unsafe.fxior (%unsafe.port-nullify-eol-style-bits port.attributes)
					     BUFFER-MODE-NONE-TAG)))
	((block)
	 (set! port.attributes (%unsafe.port-nullify-eol-style-bits port.attributes)))
	(else
	 (assertion-violation who "invalid symbol as buffer mode argument" mode))))))


;;;; buffer handling for output ports

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
    (define who 'flush-output-port)
    (with-arguments-validation (who)
	((port               port)
	 (unsafe.output-port port)
	 (unsafe.open-port   port))
      (%unsafe.flush-output-port port who)))))

;;; --------------------------------------------------------------------

(define-syntax %flush-bytevector-buffer-and-evaluate
  (syntax-rules ()
    ((%flush-bytevector-buffer-and-evaluate (?port ?who)
       (room-is-needed-for: ?number-of-bytes)
       (if-available-room: . ?available-body))
     (let ((port ?port))
       (with-port-having-bytevector-buffer (port)
	 (let try-again-after-flushing-buffer ()
	   (if (unsafe.fx<= ?number-of-bytes (unsafe.fx- port.buffer.size port.buffer.index))
	       (begin . ?available-body)
	     (begin
	       (debug-assert (= port.buffer.used-size port.buffer.index))
	       (%unsafe.flush-output-port port ?who)
	       (try-again-after-flushing-buffer)))))))))

(define-syntax %flush-string-buffer-and-evaluate
  (syntax-rules ()
    ((%flush-string-buffer-and-evaluate (?port ?who)
       (room-is-needed-for: ?number-of-chars)
       (if-available-room: . ?available-body))
     (let ((port ?port))
       (with-port-having-string-buffer (port)
	 (let try-again-after-flushing-buffer ()
	   (if (unsafe.fx<= ?number-of-bytes (unsafe.fx- port.buffer.size port.buffer.index))
	       (begin . ?available-body)
	     (begin
	       (debug-assert (= port.buffer.used-size port.buffer.index))
	       (%unsafe.flush-output-port port ?who)
	       (try-again-after-flushing-buffer)))))))))

(define (%unsafe.flush-output-port port who)
  ;;PORT must be  an open output port, either  binary or textual.  Flush
  ;;any buffered output from the  buffer of PORT to the underlying file,
  ;;device, or object.  Return unspecified values.
  ;;
  ;;This  should  be  the  only  function  to  call  the  port's  WRITE!
  ;;function.
  ;;
  ;;If PORT.WRITE!  returns  an invalid value: the state  of the port is
  ;;considered  undefined and  the  port unusable;  the  port is  marked
  ;;closed to avoid further operations  and a condition object is raised
  ;;with components: &i/o-port, &i/o-write, &who, &message, &irritants.
  ;;
  ;;If PORT.WRITE!  returns zero written  bytes or cannot absorb all the
  ;;bytes in the buffer:  this function loops retrying until PORT.WRITE!
  ;;accepts the data,  which may be forever but it  is compliant to R6RS
  ;;requirement to block as needed to output data.
  ;;
  ;;FIXME As a Vicare-specific customisation:  we may add a parameter to
  ;;optionally  request raising  a special  exception, like  "port would
  ;;block", when the underlying device cannot absorb all the data in the
  ;;buffer.
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
      (let ((buffer.used-size port.buffer.used-size))
	(let try-again-after-partial-write ((buffer.offset 0))
	  (let* ((requested-count (unsafe.fx- buffer.used-size buffer.offset))
		 (written-count   (port.write! port.buffer buffer.offset requested-count)))
	    (if (not (and (fixnum? written-count)
			  (unsafe.fx>= written-count 0)
			  (unsafe.fx<= written-count requested-count)))
		(begin
		  ;;Avoid further operations and raise an error.
		  (port.mark-as-closed!)
		  (raise (condition (make-i/o-write-error)
				    (make-i/o-port-error port)
				    (make-who-condition who)
				    (make-message-condition "write! returned an invalid value")
				    (make-irritants-condition written-count))))
	      (cond ((unsafe.fx= written-count buffer.used-size)
		     ;;Full success, all data absorbed.
		     (port.device.position.incr! written-count)
		     (port.buffer.reset-to-empty!))
		    ((unsafe.fxzero? written-count)
		     ;;Failure, no data absorbed.  Try again.
		     (try-again-after-partial-write buffer.offset))
		    (else
		     ;;Partial success,  some data absorbed.   Try again
		     ;;flushing the data left in the buffer.
		     (port.device.position.incr! written-count)
		     (try-again-after-partial-write (unsafe.fx+ buffer.offset written-count)))))))))))


;;;; bytevector buffer handling for input ports
;;
;;Input  functions always  read bytes  from the  input buffer;  when the
;;input  buffer is  completely consumed:  new  bytes are  read from  the
;;underlying device  refilling the buffer.  Whenever  refilling reads no
;;characters (that is: the READ!  function returns 0) the port is in EOF
;;state.
;;
;;The  following macros  make  it  easier to  handle  this mechanism  by
;;wrapping the %UNSAFE.REFILL-INPUT-PORT-BYTEVECTOR-BUFFER function.
;;

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
     (let ((port ?port))
       (with-port-having-string-buffer (port)
	 (if (unsafe.fx< ?buffer.offset port.buffer.used-size)
	     (begin . ?available-data-body)
	   (%refill-bytevector-buffer-and-evaluate (?port ?who)
	     (if-end-of-file:	. ?end-of-file-body)
	     (if-successful-refill:	. ?after-refill-body))))))))

(define (%unsafe.refill-input-port-bytevector-buffer port who)
  ;;Assume PORT  is an  input port object  with a bytevector  as buffer.
  ;;Fill the input buffer keeping in  it the bytes already there but not
  ;;yet  consumed (see the  pictures below);  mutate the  PORT structure
  ;;fields representing the buffer state and the port position.
  ;;
  ;;Return the number of new bytes loaded.  If the return value is zero:
  ;;the underlying device has no more bytes, it is at its EOF, but there
  ;;may still  be bytes to be  consumed in the buffer  unless the buffer
  ;;was already fully consumed before this function call.
  ;;
  ;;This should be the only function calling the port's READ!  function.
  ;;
  (with-arguments-validation (who)
      ;;Textual ports  (with transcoder) can  be built on top  of binary
      ;;input ports;  when this happens:  the underlying binary  port is
      ;;closed "in a  special way" which still allows  the upper textual
      ;;port to access the device.
      ;;
      ;;If PORT  is such  an underlying port  this function must  not be
      ;;applied to it.   The following check is to  avoid such a mistake
      ;;by  internal functions; this  mistake should  not happen  if the
      ;;functions have no bugs.
      ((unsafe.open-port port))
    (with-port-having-bytevector-buffer (port)
      (if (eq? port.read! all-data-in-buffer)
	  0
	(let ((buffer port.buffer))
	  ;;Shift to the  beginning data already in buffer  but still to
	  ;;be consumed; commit the new position.  Before:
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
	  (let* ((buffer.index	port.buffer.index)
		 (delta		(unsafe.fx- port.buffer.used-size buffer.index)))
	    (unless (unsafe.fxzero? delta)
	      (unsafe.bytevector-copy!/count buffer buffer.index buffer 0 delta))
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
	      (assertion-violation who "invalid return value from read! procedure" count))
	    (unless (and (unsafe.fx>= count 0)
			 (unsafe.fx<= count max))
	      (assertion-violation who "read! returned a value out of range" count))
	    (port.device.position.incr!  count)
	    (port.buffer.used-size.incr! count)
	    count))))))


;;;; string buffer handling for input ports
;;
;;Input functions always read characters from the input buffer; when the
;;input buffer is completely consumed:  new characters are read from the
;;underlying device  refilling the buffer.  Whenever  refilling reads no
;;characters (that is: the READ!  function returns 0) the port is in EOF
;;state.
;;
;;The  following macros  make  it  easier to  handle  this mechanism  by
;;wrapping the %UNSAFE.REFILL-INPUT-PORT-STRING-BUFFER function.
;;

(define-syntax %refill-string-buffer-and-evaluate
  ;;?PORT must be an input port with a string as buffer; the buffer must
  ;;be fully  consumed.  Refill  the buffer and  evaluate a  sequence of
  ;;forms.
  ;;
  ;;The code  in this  macro mutates the  fields of the  ?PORT structure
  ;;representing the buffer  state, so the client forms  must reload the
  ;;fields they use.
  ;;
  ;;If  refilling the buffer  succeeds: evaluate  ?AFTER-REFILL-BODY and
  ;;return its result.
  ;;
  ;;If  refilling  the buffer  finds  the  EOF  with no  new  characters
  ;;available: evaluate ?END-OF-FILE-BODY and return its result.
  ;;
  (syntax-rules (if-end-of-file: if-successful-refill:)
    ((%refill-string-buffer-and-evaluate (?port ?who)
       (if-end-of-file:		. ?end-of-file-body)
       (if-successful-refill:	. ?after-refill-body))
     (let ((count (%unsafe.refill-input-port-string-buffer ?port ?who)))
       (if (unsafe.fxzero? count)
	   (begin . ?end-of-file-body)
	 (begin . ?after-refill-body))))))

(define-syntax %maybe-refill-string-buffer-and-evaluate
  ;;?PORT must  be an input  port with a  string as buffer; there  is no
  ;;constraint on the state of  the buffer.  Refill the buffer if needed
  ;;and evaluate a sequence of forms.
  ;;
  ;;The code  in this  macro mutates the  fields of the  ?PORT structure
  ;;representing the buffer  state, so the client forms  must reload the
  ;;fields they use.
  ;;
  ;;If there  are characters  to be consumed  in the buffer  between the
  ;;offset  ?BUFFER.OFFSET  and  the  end  of the  used  area:  evaluate
  ;;?AVAILABLE-DATA-BODY and return its result.
  ;;
  ;;If ?BUFFER.OFFSET  references the end  of buffer's used  area (there
  ;;are  no more  characters to  be consumed)  and refilling  the buffer
  ;;succeeds: evaluate ?AFTER-REFILL-BODY and return its result.
  ;;
  ;;If there are  no more characters and refilling  the buffer finds the
  ;;EOF  with no  new  bytes available:  evaluate ?END-OF-FILE-BODY  and
  ;;return its result.
  ;;
  (syntax-rules (if-end-of-file: if-successful-refill: if-available-data:)
    ((%maybe-refill-string-buffer-and-evaluate (?port ?who)
       (data-is-needed-at:	?buffer.offset)
       (if-end-of-file:		. ?end-of-file-body)
       (if-successful-refill:	. ?after-refill-body)
       (if-available-data:	. ?available-data-body))
     (let ((port ?port))
       (with-port-having-string-buffer (port)
	 (if (unsafe.fx< ?buffer.offset port.buffer.used-size)
	     (begin . ?available-data-body)
	   (%refill-string-buffer-and-evaluate (?port ?who)
	     (if-end-of-file: . ?end-of-file-body)
	     (if-successful-refill: . ?after-refill-body))))))))

(define (%unsafe.refill-input-port-string-buffer port who)
  ;;Assume PORT is  an input port object with a  string as buffer.  Fill
  ;;the input buffer keeping in  it the characters already there but not
  ;;yet  consumed (see the  pictures below);  mutate the  PORT structure
  ;;fields representing the buffer state and the port position.
  ;;
  ;;Return the number of new  characters loaded.  If the return value is
  ;;zero: the underlying device has no more bytes, it is at its EOF, but
  ;;there may  still be characters to  be consumed in  the buffer unless
  ;;the buffer was already fully consumed before this function call.
  ;;
  ;;This should be the only function calling the port's READ!  function.
  ;;
  (with-arguments-validation (who)
      ((unsafe.open-port port))
    (with-port-having-string-buffer (port)
      (if (eq? port.read! all-data-in-buffer)
	  0
	(let ((buffer port.buffer))
	  ;;Shift to the  beginning data alraedy in buffer  but still to
	  ;;be consumed; commit the new position.  Before:
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
	      (unsafe.string-copy!/count buffer port.buffer.index buffer 0 delta))
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
	      (assertion-violation who "invalid return value from read! procedure" count))
	    (unless (and (unsafe.fx>= count 0)
			 (unsafe.fx<= count max))
	      (assertion-violation who "read! returned a value out of range" count))
	    (port.device.position.incr!  count)
	    (port.buffer.used-size.incr! count)
	    count))))))


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
  (%case-binary-input-port-fast-tag (port who)
    ((FAST-GET-BYTE-TAG)
     (with-port-having-bytevector-buffer (port)
       (let ((buffer.offset port.buffer.index))
	 (if (unsafe.fx< buffer.offset port.buffer.used-size)
	     (begin
	       (set! port.buffer.index (unsafe.fxadd1 buffer.offset))
	       (unsafe.bytevector-u8-ref port.buffer buffer.offset))
	   (%get/peek-u8-from-device port who 1)))))))

(define (lookahead-u8 port)
  ;;Defined by R6RS.  The  LOOKAHEAD-U8 procedure is like GET-U8,
  ;;but it does not update PORT to point past the byte.
  ;;
  ;;Here  we handle  the case  of byte  already available  in the
  ;;buffer, if the buffer is empty: we call a subroutine.
  ;;
  (define who 'lookahead-u8)
  (%case-binary-input-port-fast-tag (port who)
    ((FAST-GET-BYTE-TAG)
     (with-port-having-bytevector-buffer (port)
       (let ((buffer.offset port.buffer.index))
	 (if (unsafe.fx< buffer.offset port.buffer.used-size)
	     (unsafe.bytevector-u8-ref port.buffer buffer.offset)
	   (%get/peek-u8-from-device port who 0)))))))

(define (%get/peek-u8-from-device port who buffer.offset-after)
  ;;Subroutine of GET-U8 and LOOKAHEAD-U8.  To be called when the
  ;;port buffer  is fully  consumed.  Get or  peek the  next byte
  ;;from PORT, set the buffer index to BUFFER.OFFSET-AFTER.
  ;;
  (with-port-having-bytevector-buffer (port)
    (debug-assert (fx= port.buffer.index port.buffer.used-size))
    (%refill-bytevector-buffer-and-evaluate (port who)
      (if-end-of-file: (eof-object))
      (if-successful-refill:
       (set! port.buffer.index buffer.offset-after)
       (unsafe.bytevector-u8-ref port.buffer 0)))))

(define (lookahead-two-u8 port)
  ;;Defined  by Vicare.   Like LOOKAHEAD-U8  but peeks  at 2  octets and
  ;;return two values: EOF or  a fixnum representing first octet, EOF or
  ;;a fixnum representing the second octet.
  ;;
  (define who 'lookahead-u8)
  (define (%maybe-some-data-is-available)
    (with-port-having-bytevector-buffer (port)
      (let* ((buffer.offset-octet0	port.buffer.index)
	     (buffer.offset-octet1	(unsafe.fxadd1 buffer.offset-octet0))
	     (buffer.offset-past	(unsafe.fxadd1 buffer.offset-octet1)))
	(cond ((unsafe.fx<= buffer.offset-past port.buffer.used-size)
	       (values (unsafe.bytevector-u8-ref port.buffer buffer.offset-octet0)
		       (unsafe.bytevector-u8-ref port.buffer buffer.offset-octet1)))
	      ((unsafe.fx<= buffer.offset-octet1 port.buffer.used-size)
	       (values (unsafe.bytevector-u8-ref port.buffer buffer.offset-octet0)
		       (eof-object)))
	      (else
	       (values (eof-object) (eof-object)))))))
  (%case-binary-input-port-fast-tag (port who)
    ((FAST-GET-BYTE-TAG)
     (with-port-having-bytevector-buffer (port)
       (let* ((buffer.offset-octet0	port.buffer.index)
	      (buffer.offset-octet1	(unsafe.fxadd1 buffer.offset-octet0))
	      (buffer.offset-past	(unsafe.fxadd1 buffer.offset-octet1)))
	 (if (unsafe.fx<= buffer.offset-past port.buffer.used-size)
	     (values (unsafe.bytevector-u8-ref port.buffer buffer.offset-octet0)
		     (unsafe.bytevector-u8-ref port.buffer buffer.offset-octet1))
	   (%refill-bytevector-buffer-and-evaluate (port who)
	     (if-end-of-file:       (%maybe-some-data-is-available))
	     (if-successful-refill: (%maybe-some-data-is-available)))))))))


;;;; bytevector input functions

(define (get-bytevector-n port count)
  ;;Defined  by R6RS.   COUNT  must be  an  exact, non-negative  integer
  ;;object representing the number of bytes to be read.
  ;;
  ;;The  GET-BYTEVECTOR-N procedure  reads from  the binary  input PORT,
  ;;blocking as necessary, until COUNT  bytes are available from PORT or
  ;;until an end of file is reached.
  ;;
  ;;If COUNT bytes are available before an end of file, GET-BYTEVECTOR-N
  ;;returns a bytevector of size COUNT.
  ;;
  ;;If fewer bytes are available before an end of file, GET-BYTEVECTOR-N
  ;;returns  a bytevector containing  those bytes.  In either  case, the
  ;;input port is updated to point just past the bytes read.
  ;;
  ;;If  an  end of  file  is reached  before  any  bytes are  available,
  ;;GET-BYTEVECTOR-N returns the EOF object.
  ;;
  ;;IMPLEMENTATION RESTRICTION The COUNT argument must be a fixnum.
  ;;
  (define who 'get-bytevector-n)
  (%case-binary-input-port-fast-tag (port who)
    ((FAST-GET-BYTE-TAG)
     (with-arguments-validation (who)
	 ((fixnum-count count))
       (with-port-having-bytevector-buffer (port)
	 (if (zero? count)
	     (quote #vu8())
	   (let retry-after-filling-buffer ((output.len		  0)
					    (list-of-bytevectors  '())
					    (count		  count))
	     (define (%data-available-in-buffer)
	       (let* ((buffer.used-size		port.buffer.used-size)
		      (buffer.offset		port.buffer.index)
		      (amount-of-available	(unsafe.fx- buffer.used-size buffer.offset))
		      (all-count-is-available?	(<= count amount-of-available))
		      (amount-to-read		(if all-count-is-available? count amount-of-available))
		      (output.len1		(unsafe.fx+ output.len amount-to-read)))
		 ;;DANGER This  check must  not be removed  when compiling
		 ;;without arguments validation.
		 (unless (fixnum? output.len1)
		   (%implementation-violation who
		     "request to read data from port would exceed maximum size of bytevectors"
		     output.len1))
		 (let ((bv (unsafe.make-bytevector amount-to-read)))
		   (unsafe.bytevector-copy!/count port.buffer buffer.offset bv 0 amount-to-read)
		   (set! port.buffer.index (unsafe.fx+ buffer.offset amount-to-read))
		   (let ((list-of-bytevectors1 (cons bv list-of-bytevectors)))
		     (if all-count-is-available?
			 (%unsafe.bytevector-reverse-and-concatenate who list-of-bytevectors1 output.len1)
		       (retry-after-filling-buffer output.len1 list-of-bytevectors1
						   (- count amount-of-available)))))))
	     (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	       (data-is-needed-at: port.buffer.index)
	       (if-end-of-file:
		(if (zero? output.len)
		    (eof-object)
		  (%unsafe.bytevector-reverse-and-concatenate who list-of-bytevectors output.len)))
	       (if-successful-refill: (%data-available-in-buffer))
	       (if-available-data: (%data-available-in-buffer))))))))))

(define (get-bytevector-n! port dst.bv dst.start count)
  ;;Defined  by R6RS.   COUNT  must be  an  exact, non-negative  integer
  ;;object, representing the number of bytes to be read.  DST.BV must be
  ;;a bytevector with at least DST.START+COUNT elements.
  ;;
  ;;The GET-BYTEVECTOR-N!   procedure reads from the  binary input PORT,
  ;;blocking as necessary,  until COUNT bytes are available  or until an
  ;;end of file is reached.
  ;;
  ;;If COUNT bytes are available before an end of file, they are written
  ;;into DST.BV starting at index DST.START, and the result is COUNT.
  ;;
  ;;If  fewer bytes  are  available before  the  next end  of file,  the
  ;;available bytes are written into DST.BV starting at index DST.START,
  ;;and the result  is a number object representing  the number of bytes
  ;;actually read.
  ;;
  ;;In either  case, the input  port is updated  to point just  past the
  ;;bytes  read.  If  an end  of file  is reached  before any  bytes are
  ;;available, GET-BYTEVECTOR-N!  returns the EOF object.
  ;;
  ;;IMPLEMENTATION RESTRICTION The COUNT argument must be a fixnum.
  ;;
  (define who 'get-bytevector-n!)
  (%case-binary-input-port-fast-tag (port who)
    ((FAST-GET-BYTE-TAG)
     (with-arguments-validation (who)
	 ((bytevector		dst.bv)
	  (fixnum-start-index	dst.start)
	  (fixnum-count		count)
	  (unsafe.start-index-for-bytevector	  dst.start dst.bv)
	  (unsafe.count-from-start-in-bytevector  count dst.start dst.bv))
       (with-port-having-bytevector-buffer (port)
	 (if (zero? count)
	     count
	   (let retry-after-filling-buffer ((tmp.start dst.start)
					    (count     count))
	     (define (%data-available-in-buffer)
	       (let* ((buffer.used-size		port.buffer.used-size)
		      (buffer.offset		port.buffer.index)
		      (amount-of-available	(unsafe.fx- buffer.used-size buffer.offset))
		      (all-count-is-available?	(<= count amount-of-available))
		      (amount-to-read		(if all-count-is-available? count amount-of-available)))
		 (unsafe.bytevector-copy!/count port.buffer buffer.offset dst.bv tmp.start amount-to-read)
		 (set! port.buffer.index (unsafe.fx+ buffer.offset amount-to-read))
		 (let ((tmp.start (+ tmp.start amount-to-read)))
		   (if all-count-is-available?
		       (- tmp.start dst.start)
		     (retry-after-filling-buffer tmp.start (- count amount-to-read))))))
	     (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	       (data-is-needed-at: port.buffer.index)
	       (if-end-of-file: (if (= tmp.start dst.start)
				    (eof-object)
				  (- tmp.start dst.start)))
	       (if-successful-refill: (%data-available-in-buffer))
	       (if-available-data: (%data-available-in-buffer))))))))))

(define (get-bytevector-some port)
  ;;Defined  by R6RS.   Read from  the  binary input  PORT, blocking  as
  ;;necessary,  until bytes are  available or  until an  end of  file is
  ;;reached.
  ;;
  ;;If  bytes become  available, GET-BYTEVECTOR-SOME  returns  a freshly
  ;;allocated  bytevector  containing the  initial  available bytes  (at
  ;;least one), and it updates PORT to point just past these bytes.
  ;;
  ;;If no input bytes are seen before an end of file is reached, the EOF
  ;;object is returned.
  ;;
  (define who 'get-bytevector-some)
  (%case-binary-input-port-fast-tag (port who)
    ((FAST-GET-BYTE-TAG)
     (with-port-having-bytevector-buffer (port)
       (let retry-after-filling-buffer ()
	 (define (%data-available-in-buffer)
	   (let* ((buffer.used-size	port.buffer.used-size)
		  (buffer.offset	port.buffer.index)
		  (amount-of-available	(unsafe.fx- buffer.used-size buffer.offset))
		  (dst.bv		(unsafe.make-bytevector amount-of-available)))
	     (unsafe.bytevector-copy!/count port.buffer buffer.offset dst.bv 0 amount-of-available)
	     (set! port.buffer.index buffer.used-size)
	     dst.bv))
	 (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	   (data-is-needed-at: port.buffer.index)
	   (if-end-of-file: (eof-object))
	   (if-successful-refill: (%data-available-in-buffer))
	   (if-available-data: (%data-available-in-buffer))))))))

(define (get-bytevector-all port)
  ;;Defined by R6RS.   Attempts to read all bytes until  the next end of
  ;;file, blocking as necessary.
  ;;
  ;;If  one  or  more  bytes  are  read,  GET-BYTEVECTOR-ALL  returns  a
  ;;bytevector  containing  all  bytes  up  to the  next  end  of  file.
  ;;Otherwise, GET-BYTEVECTOR-ALL returns the EOF object.
  ;;
  ;;The operation  may block indefinitely  waiting to see if  more bytes
  ;;will become available, even if some bytes are already available.
  ;;
  (define who 'get-bytevector-all)
  (%case-binary-input-port-fast-tag (port who)
    ((FAST-GET-BYTE-TAG)
     (with-port-having-bytevector-buffer (port)
       (let retry-after-filling-buffer ((output.len          0)
					(list-of-bytevectors '()))
	 (define (%data-available-in-buffer)
	   (let* ((buffer.used-size	port.buffer.used-size)
		  (buffer.offset		port.buffer.index)
		  (amount-of-available	(unsafe.fx- buffer.used-size buffer.offset))
		  (output.len		(+ output.len amount-of-available)))
	     ;;DANGER  This check  must  not be  removed when  compiling
	     ;;without arguments validation.
	     (unless (fixnum? output.len)
	       (%implementation-violation who
		 "request to read data from port would exceed maximum size of bytevectors"
		 output.len))
	     (let ((dst.bv (unsafe.make-bytevector amount-of-available)))
	       (unsafe.bytevector-copy!/count port.buffer buffer.offset dst.bv 0 amount-of-available)
	       (set! port.buffer.index buffer.used-size)
	       (retry-after-filling-buffer output.len
					   (cons dst.bv list-of-bytevectors)))))
	 (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	   (data-is-needed-at: port.buffer.index)
	   (if-end-of-file: (if (zero? output.len)
				(eof-object)
			      (%unsafe.bytevector-reverse-and-concatenate who list-of-bytevectors
									  output.len)))
	   (if-successful-refill: (%data-available-in-buffer))
	   (if-available-data:    (%data-available-in-buffer))))))))


;;;; character input

(define (get-char port)
  ;;Defined  by R6RS.   Read from  the textual  input PORT,  blocking as
  ;;necessary, until a complete character  is available, or until an end
  ;;of file is reached.
  ;;
  ;;If a  complete character is available  before the next  end of file,
  ;;GET-CHAR returns that character and  updates the input port to point
  ;;past  the  character.  If  an  end of  file  is  reached before  any
  ;;character is read, GET-CHAR returns the EOF object.
  ;;
  (%do-read-char port 'get-char))

(define read-char
  ;;Defined  by  R6RS.   Reads  from  textual input  PORT,  blocking  as
  ;;necessary  until a  character  is  available, or  the  data that  is
  ;;available cannot be  the prefix of any valid encoding,  or an end of
  ;;file is reached.
  ;;
  ;;If a  complete character is available  before the next  end of file:
  ;;READ-CHAR returns that character and updates the input port to point
  ;;past that character.
  ;;
  ;;If an  end of file  is reached before  any data are  read: READ-CHAR
  ;;returns the EOF object.
  ;;
  ;;If  PORT  is   omitted,  it  defaults  to  the   value  returned  by
  ;;CURRENT-INPUT-PORT.
  ;;
  (case-lambda
   ((port)
    (%do-read-char port 'read-char))
   (()
    (%do-read-char (current-input-port) 'read-char))))

(define (%do-read-char port who)
  (define-inline (main)
    (%case-textual-input-port-fast-tag (port who)
      ((FAST-GET-UTF8-TAG)
       (%get-it %unsafe.read-char-from-port-with-fast-get-utf8-tag
		%unsafe.peek-char-from-port-with-fast-get-utf8-tag))
      ((FAST-GET-CHAR-TAG)
       (%get-it %unsafe.read-char-from-port-with-fast-get-char-tag
		%unsafe.peek-char-from-port-with-fast-get-char-tag))
      ((FAST-GET-LATIN-TAG)
       (%get-it %unsafe.read-char-from-port-with-fast-get-latin1-tag
		%unsafe.peek-char-from-port-with-fast-get-latin1-tag))
      ((FAST-GET-UTF16LE-TAG)
       (%get-it %read-utf16le %peek-utf16le))
      ((FAST-GET-UTF16BE-TAG)
       (%get-it %read-utf16be %peek-utf16be))))

  (define-inline (%get-it ?read-char ?peek-char)
    (let ((eol-bits (%unsafe.port-eol-style-bits port))
	  (ch (?read-char port who)))
      (cond ((eof-object? ch)
	     ch) ;return EOF
	    ((unsafe.fxzero? eol-bits) ;EOL style none
	     ch)
	    ((unsafe.char-is-single-char-line-ending? ch)
	     LINEFEED-CHAR)
	    ((unsafe.char-is-carriage-return? ch)
	     (let ((ch1 (?peek-char port who)))
	       (cond ((eof-object? ch1)
		      (void))
		     ((unsafe.char-is-newline-after-carriage-return? ch1)
		      (?read-char port who)))
	       LINEFEED-CHAR))
	    (else ch))))

  (define-inline (%read-utf16le ?port ?who)
    (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little))

  (define-inline (%peek-utf16le ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little 0))

  (define-inline (%read-utf16be ?port ?who)
    (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big))

  (define-inline (%peek-utf16be ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big 0))

  (main))

;;; --------------------------------------------------------------------

(define (lookahead-char port)
  ;;Defined by R6RS.  The LOOKAHEAD-CHAR procedure is like GET-CHAR, but
  ;;it does not update PORT to point past the character.  PORT must be a
  ;;textual input port.
  ;;
  (%do-peek-char port 'lookahead-char))

(define peek-char
  ;;Define by R6RS.  This is the same as READ-CHAR, but does not consume
  ;;any data from the port.
  ;;
  (case-lambda
   (()
    (%do-peek-char (current-input-port) 'peek-char))
   ((port)
    (%do-peek-char port 'peek-char))))

(define (%do-peek-char port who)
  (define-inline (main)
    (%case-textual-input-port-fast-tag (port who)
      ((FAST-GET-UTF8-TAG)
       (%do-it 1
	       %unsafe.peek-char-from-port-with-fast-get-utf8-tag
	       %unsafe.peek-char-from-port-with-utf8-codec))
      ((FAST-GET-CHAR-TAG)
       (%do-it 1 %peek-char %peek-char/offset))
      ((FAST-GET-LATIN-TAG)
       (%do-it 1 %peek-latin1 %peek-latin1/offset))
      ((FAST-GET-UTF16LE-TAG)
       (%do-it 2 %peek-utf16le %peek-utf16le/offset))
      ((FAST-GET-UTF16BE-TAG)
       (%do-it 2 %peek-utf16be %peek-utf16be/offset))))

  (define-inline (%do-it ?offset-of-ch2 ?peek-char ?peek-char/offset)
    ;;Actually  perform the  lookahead.   Return the  next char  without
    ;;modifying  the  port position.   If  no  characters are  available
    ;;return the  EOF object.  Remember  that, as mandated by  R6RS, for
    ;;input  ports  every  line-ending  sequence of  character  must  be
    ;;converted to linefeed when the EOL style is not NONE.
    ;;
    ;;EOL-BITS must be the port attribute bits to select the EOL style.
    ;;
    ;;?PEEK-CHAR  must  be the  identifier  of  a  macro performing  the
    ;;lookahead operation  for the next available character;  it is used
    ;;to peek the  next single char and the first char  in a sequence of
    ;;2-chars line-ending.
    ;;
    ;;?PEEK-CHAR/OFFSET must be the identifier of a macro performing the
    ;;forward lookahead operation for the second character in a sequence
    ;;of 2-chars line-ending.
    ;;
    ;;?OFFSET-OF-CH2 is the  offset of the second char  in a sequence of
    ;;2-chars  line-ending; for  ports having  bytevector buffer:  it is
    ;;expressed  in  bytes;  for  ports  having  string  buffer:  it  is
    ;;expressed   in  characters.    Fortunately:   2-chars  line-ending
    ;;sequences always have a carriage return as first char and we know,
    ;;once the codec has been selected, the offset of such character.
    ;;
    (let ((eol-bits (%unsafe.port-eol-style-bits port))
	  (ch       (?peek-char port who)))
      (cond ((eof-object? ch)
	     ch) ;return EOF
	    ((unsafe.fxzero? eol-bits) ;EOL style none
	     ch)
	    ((unsafe.char-is-single-char-line-ending? ch)
	     LINEFEED-CHAR)
	    ((unsafe.char-is-carriage-return? ch)
	     (let ((ch2 (?peek-char/offset port who ?offset-of-ch2)))
	       (cond ((eof-object? ch2)
		      LINEFEED-CHAR)
		     ((unsafe.char-is-newline-after-carriage-return? ch2)
		      LINEFEED-CHAR)
		     (else ch))))
	    (else ch))))

  (define-inline (%peek-char ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-char-tag ?port ?who))

  (define-inline (%peek-char/offset ?port ?who ?offset)
    (%unsafe.read/peek-char-from-port-with-string-buffer ?port ?who 0 ?offset))

  (define-inline (%peek-latin1 ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-latin1-tag ?port ?who))

  (define-inline (%peek-latin1/offset ?port ?who ?offset)
    (%unsafe.read/peek-char-from-port-with-latin1-codec ?port ?who 0 ?offset))

  (define-inline (%peek-utf16le ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little 0))

  (define-inline (%peek-utf16le/offset ?port ?who ?offset)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little ?offset))

  (define-inline (%peek-utf16be ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big 0))

  (define-inline (%peek-utf16be/offset ?port ?who ?offset)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big ?offset))

  (main))

;;; --------------------------------------------------------------------
;;; GET-CHAR and LOOKAHEAD-CHAR for ports with UTF-8 transcoder

(define-inline (%unsafe.read-char-from-port-with-fast-get-utf8-tag ?port ?who)
  ;;PORT  is a  textual  input  port with  bytevector  buffer and  UTF-8
  ;;transcoder.   We  process  here   the  simple  case  of  single-byte
  ;;character  available in  the buffer,  else we  call  the specialised
  ;;function for reading UTF-8 chars.
  ;;
  (let ((port ?port))
    (with-port-having-bytevector-buffer (port)
      (let ((buffer.offset-byte0 port.buffer.index))
	(if (unsafe.fx< buffer.offset-byte0 port.buffer.used-size)
	    (let ((byte0 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte0)))
	      (if (utf-8-single-octet? byte0)
		  (let ((N (utf-8-decode-single-octet byte0)))
		    (set! port.buffer.index (unsafe.fxadd1 buffer.offset-byte0))
		    (unsafe.fixnum->char N))
		(%unsafe.read-char-from-port-with-utf8-codec port ?who)))
	  (%unsafe.read-char-from-port-with-utf8-codec port ?who))))))

(define-inline (%unsafe.peek-char-from-port-with-fast-get-utf8-tag ?port ?who)
  ;;PORT must be  a textual input port with  bytevector buffer and UTF-8
  ;;transcoder.  We process here the simple case of one single byte char
  ;;available in the  buffer, else we call the  specialised function for
  ;;reading UTF8 characters.
  ;;
  (let ((port ?port))
    (with-port-having-bytevector-buffer (port)
      (let ((buffer.offset-byte0 port.buffer.index))
	(if (unsafe.fx< buffer.offset-byte0 port.buffer.used-size)
	    (let ((byte0 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte0)))
	      (if (utf-8-single-octet? byte0)
		  (unsafe.fixnum->char (utf-8-decode-single-octet byte0))
		(%unsafe.peek-char-from-port-with-utf8-codec port ?who 0)))
	  (%unsafe.peek-char-from-port-with-utf8-codec port ?who 0))))))

(define (%unsafe.read-char-from-port-with-utf8-codec port who)
  ;;PORT must be  a textual input port with  bytevector buffer and UTF-8
  ;;transcoder.  Read from PORT a  UTF-8 encoded character for the cases
  ;;of 1,  2, 3 and 4 bytes  encoding; return a Scheme  character or the
  ;;EOF object;  in case of  error: honor the  error mode in  the port's
  ;;transcoder.
  ;;
  ;;The port's  buffer may be fully  consumed or not.   This function is
  ;;meant to be used by  all the functions reading UTF-8 characters from
  ;;a port.
  ;;
  (with-port-having-bytevector-buffer (port)

    (define-inline (main)
      (let retry-after-filling-buffer ()
	(let ((buffer.offset-byte0 port.buffer.index))
	  (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte0)
	    (if-end-of-file: (eof-object))
	    (if-successful-refill: (retry-after-filling-buffer))
	    (if-available-data:
	     (let ((byte0 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte0)))
	       (define (%error-invalid-byte)
		 (%error-handler "invalid byte while expecting first byte of UTF-8 character" byte0))
	       (cond ((utf-8-invalid-octet? byte0)
		      (set! port.buffer.index (unsafe.fxadd1 buffer.offset-byte0))
		      (%error-invalid-byte))
		     ((utf-8-single-octet? byte0)
		      (get-single-byte-character byte0 buffer.offset-byte0))
		     ((utf-8-first-of-two-octets? byte0)
		      (get-2-bytes-character byte0 buffer.offset-byte0))
		     ((utf-8-first-of-three-octets? byte0)
		      (get-3-bytes-character byte0 buffer.offset-byte0))
		     ((utf-8-first-of-four-octets? byte0)
		      (get-4-bytes-character byte0 buffer.offset-byte0))
		     (else
		      (set! port.buffer.index (unsafe.fxadd1 buffer.offset-byte0))
		      (%error-invalid-byte)))))))))

    (define-inline (get-single-byte-character byte0 buffer.offset-byte0)
      (let ((N (utf-8-decode-single-octet byte0)))
	(set! port.buffer.index (unsafe.fxadd1 buffer.offset-byte0))
	(unsafe.fixnum->char N)))

    (define-inline (get-2-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-1-more-byte ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
	       (buffer.offset-past  (unsafe.fxadd1 buffer.offset-byte1)))
	  (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte1)
	    (if-end-of-file:
	     (%unexpected-eof-error "unexpected EOF while decoding 2-byte UTF-8 character" byte0))
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-1-more-byte port.buffer.index))
	    (if-available-data:
	     (let ((byte1 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1)))
	       (define (%error-invalid-second)
		 (%error-handler "invalid second byte in 2-byte UTF-8 character"
				 byte0 byte1))
	       (set! port.buffer.index buffer.offset-past)
	       (cond ((utf-8-invalid-octet? byte1)
		      (%error-invalid-second))
		     ((utf-8-second-of-two-octets? byte1)
		      (let ((N (utf-8-decode-two-octets byte0 byte1)))
			(if (utf-8-valid-code-point-from-2-octets? N)
			    (unsafe.fixnum->char N)
			  (%error-handler "invalid code point as result \
                                           of decoding 2-byte UTF-8 character"
					  byte0 byte1 N))))
		     (else
		      (%error-invalid-second)))))))))

    (define-inline (get-3-bytes-character byte0 buffer.offset-byte0)
      ;;After refilling we have to reload buffer indexes.
      (let retry-after-filling-buffer-for-2-more-bytes ((buffer.offset-byte0 buffer.offset-byte0))
	(let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
	       (buffer.offset-byte2 (unsafe.fxadd1 buffer.offset-byte1))
	       (buffer.offset-past  (unsafe.fxadd1 buffer.offset-byte2)))
	  (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte2)
	    (if-end-of-file:
	     (apply %unexpected-eof-error "unexpected EOF while decoding 3-byte UTF-8 character"
		    byte0 (if (unsafe.fx< buffer.offset-byte1 port.buffer.used-size)
			      (list (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
			    '())))
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-2-more-bytes port.buffer.index))
	    (if-available-data:
	     (let ((byte1 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (byte2 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2)))
	       (define (%error-invalid-second-or-third)
		 (%error-handler "invalid second or third byte while decoding UTF-8 character \
                                  and expecting 3-byte character"
				 byte0 byte1 byte2))
	       (set! port.buffer.index buffer.offset-past)
	       (cond ((or (utf-8-invalid-octet? byte1)
			  (utf-8-invalid-octet? byte2))
		      (%error-invalid-second-or-third))
		     ((utf-8-second-and-third-of-three-octets? byte1 byte2)
		      (let ((N (utf-8-decode-three-octets byte0 byte1 byte2)))
			(if (utf-8-valid-code-point-from-3-octets? N)
			    (unsafe.fixnum->char N)
			  (%error-handler "invalid code point as result \
                                           of decoding 3-byte UTF-8 character"
					  byte0 byte1 byte2 N))))
		     (else
		      (%error-invalid-second-or-third)))))))))

    (define-inline (get-4-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-3-more-bytes ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
	       (buffer.offset-byte2 (unsafe.fxadd1 buffer.offset-byte1))
	       (buffer.offset-byte3 (unsafe.fxadd1 buffer.offset-byte2))
	       (buffer.offset-past  (unsafe.fxadd1 buffer.offset-byte3)))
	  (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte3)
	    (if-end-of-file:
	     (apply %unexpected-eof-error "unexpected EOF while decoding 4-byte UTF-8 character"
		    byte0 (if (unsafe.fx< buffer.offset-byte1 port.buffer.used-size)
			      (cons (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1)
				    (if (unsafe.fx< buffer.offset-byte2 port.buffer.used-size)
					(list (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2))
				      '()))
			    '())))
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-3-more-bytes port.buffer.index))
	    (if-available-data:
	     (let ((byte1 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (byte2 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2))
		   (byte3 (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte3)))
	       (define (%error-invalid-second-third-or-fourth)
		 (%error-handler "invalid second, third or fourth byte while decoding UTF-8 character \
                                  and expecting 4-byte character"
				 byte0 byte1 byte2 byte3))
	       (set! port.buffer.index buffer.offset-past)
	       (cond ((or (utf-8-invalid-octet? byte1)
			  (utf-8-invalid-octet? byte2)
			  (utf-8-invalid-octet? byte3))
		      (%error-invalid-second-third-or-fourth))
		     ((utf-8-second-third-and-fourth-of-four-octets? byte1 byte2 byte3)
		      (let ((N (utf-8-decode-four-octets byte0 byte1 byte2 byte3)))
			(if (utf-8-valid-code-point-from-4-octets? N)
			    (unsafe.fixnum->char N)
			  (%error-handler "invalid code point as result \
                                           of decoding 4-byte UTF-8 character"
					  byte0 byte1 byte2 byte3 N))))
		     (else
		      (%error-invalid-second-third-or-fourth)))))))))

    (define (%unexpected-eof-error message . irritants)
      (let ((mode (transcoder-error-handling-mode port.transcoder)))
	(case mode
	  ((ignore)
	   ;;To ignore means to jump to the next.
	   (set! port.buffer.index port.buffer.used-size)
	   (eof-object))
	  ((replace)
	   (set! port.buffer.index port.buffer.used-size)
	   #\xFFFD)
	  ((raise)
	   (raise (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition message)
			     (make-irritants-condition irritants))))
	  (else
	   (assertion-violation who "vicare internal error: wrong transcoder error handling mode" mode)))))

    (define (%error-handler message . irritants)
      (let ((mode (transcoder-error-handling-mode port.transcoder)))
	(case mode
	  ((ignore)
	   ;;To ignore means to jump to the next.
	   (%unsafe.read-char-from-port-with-utf8-codec port who))
	  ((replace)
	   #\xFFFD)
	  ((raise)
	   (raise (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition message)
			     (make-irritants-condition irritants))))
	  (else
	   (assertion-violation who
	     "vicare internal error: wrong transcoder error handling mode" mode)))))

    (main)))

(define (%unsafe.peek-char-from-port-with-utf8-codec port who buffer-offset)
  ;;Subroutine of %DO-PEEK-CHAR.  Peek from a textual input PORT a UTF-8
  ;;encoded character  for the cases of  2, 3 and 4  bytes encoding; the
  ;;case of 1-byte encoding is handled by %DO-PEEK-CHAR.
  ;;
  ;;BYTE0 is the first byte  in the UTF-8 sequence, already extracted by
  ;;the calling function.
  ;;
  ;;Return  a Scheme character  or the  EOF object.   In case  of error:
  ;;honor the error mode in the port's transcoder.
  ;;
  ;;BUFFER-OFFSET  must be  the offset  to add  to  PORT.BUFFER.INDEX to
  ;;obtain the index of the next byte  in the buffer to read; it must be
  ;;zero upon entering  this function; it is used  in recursive calls to
  ;;this function to implement the IGNORE error handling method.
  ;;
  (with-port-having-bytevector-buffer (port)

    (define-inline (recurse buffer-offset)
      (%unsafe.peek-char-from-port-with-utf8-codec port who buffer-offset))

    (define-inline (main)
      (let retry-after-filling-buffer ()
	(let ((buffer.offset-byte0 (unsafe.fx+ port.buffer.index buffer-offset)))
	  (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte0)
	    (if-end-of-file: (eof-object))
	    (if-successful-refill: (retry-after-filling-buffer))
	    (if-available-data:
	     (let ((byte0		(unsafe.bytevector-u8-ref port.buffer buffer.offset-byte0))
		   (buffer-offset	(unsafe.fx+ 1 buffer-offset)))
	       (define (%error-invalid-byte)
		 (%error-handler (unsafe.fxadd1 buffer-offset)
				 "invalid byte while expecting first byte of UTF-8 character" byte0))
	       (cond ((utf-8-invalid-octet? byte0)
		      (%error-invalid-byte))
		     ((utf-8-single-octet? byte0)
		      (unsafe.fixnum->char (utf-8-decode-single-octet byte0)))
		     ((utf-8-first-of-two-octets? byte0)
		      (peek-2-bytes-character byte0 buffer.offset-byte0))
		     ((utf-8-first-of-three-octets? byte0)
		      (peek-3-bytes-character byte0 buffer.offset-byte0))
		     ((utf-8-first-of-four-octets? byte0)
		      (peek-4-bytes-character byte0 buffer.offset-byte0))
		     (else
		      (%error-invalid-byte)))))))))

    (define-inline (peek-2-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-1-more-byte ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0)))
	  (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte1)
	    (if-end-of-file:
	     (%unexpected-eof-error "unexpected EOF while decoding 2-byte UTF-8 character" byte0))
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-1-more-byte port.buffer.index))
	    (if-available-data:
	     (let ((byte1		(unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (buffer-offset	(unsafe.fx+ 2 buffer-offset)))
	       (define (%error-invalid-second)
		 (%error-handler buffer-offset "invalid second byte in 2-byte UTF-8 character"
				 byte0 byte1))
	       (cond ((utf-8-invalid-octet? byte1)
		      (%error-invalid-second))
		     ((utf-8-second-of-two-octets? byte1)
		      (let ((N (utf-8-decode-two-octets byte0 byte1)))
			(if (utf-8-valid-code-point-from-2-octets? N)
			    (unsafe.fixnum->char N)
			  (%error-handler buffer-offset
					  "invalid code point as result \
                                           of decoding 2-byte UTF-8 character"
					  byte0 byte1 N))))
		     (else
		      (%error-invalid-second)))))))))

    (define-inline (peek-3-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-2-more-bytes ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
	       (buffer.offset-byte2 (unsafe.fxadd1 buffer.offset-byte1)))
	  (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte2)
	    (if-end-of-file:
	     (apply %unexpected-eof-error "unexpected EOF while decoding 3-byte UTF-8 character"
		    byte0 (if (unsafe.fx< buffer.offset-byte1 port.buffer.used-size)
			      (list (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
			    '())))
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-2-more-bytes port.buffer.index))
	    (if-available-data:
	     (let ((byte1		(unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (byte2		(unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2))
		   (buffer-offset	(unsafe.fx+ 3 buffer-offset)))
	       (define (%error-invalid-second-or-third)
		 (%error-handler buffer-offset
				 "invalid second or third byte in 3-byte UTF-8 character"
				 byte0 byte1 byte2))
	       (cond ((or (utf-8-invalid-octet? byte1)
			  (utf-8-invalid-octet? byte2))
		      (%error-invalid-second-or-third))
		     ((utf-8-second-and-third-of-three-octets? byte1 byte2)
		      (let ((N (utf-8-decode-three-octets byte0 byte1 byte2)))
			(if (utf-8-valid-code-point-from-3-octets? N)
			    (unsafe.fixnum->char N)
			  (%error-handler buffer-offset
					  "invalid code point as result of \
                                           decoding 3-byte UTF-8 character"
					  byte0 byte1 byte2 N))))
		     (else
		      (%error-invalid-second-or-third)))))))))

    (define-inline (peek-4-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-3-more-bytes ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let* ((buffer.offset-byte1 (unsafe.fxadd1 buffer.offset-byte0))
	       (buffer.offset-byte2 (unsafe.fxadd1 buffer.offset-byte1))
	       (buffer.offset-byte3 (unsafe.fxadd1 buffer.offset-byte2)))
	  (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte3)
	    (if-end-of-file:
	     (apply %unexpected-eof-error "unexpected EOF while decoding 4-bytes UTF-8 character"
		    byte0 (if (unsafe.fx< buffer.offset-byte1 port.buffer.used-size)
			      (cons (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1)
				    (if (unsafe.fx< buffer.offset-byte2 port.buffer.used-size)
					(list (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2))
				      '()))
			    '())))
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-3-more-bytes port.buffer.index))
	    (if-available-data:
	     (let ((byte1  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (byte2  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte2))
		   (byte3  (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte3))
		   (buffer-offset (unsafe.fx+ 4 buffer-offset)))
	       (define (%error-invalid-second-third-or-fourth)
		 (%error-handler buffer-offset
				 "invalid second, third or fourth byte in 4-bytes UTF-8 character"
				 byte0 byte1 byte2 byte3))
	       (cond ((or (utf-8-invalid-octet? byte1)
			  (utf-8-invalid-octet? byte2)
			  (utf-8-invalid-octet? byte3))
		      (%error-invalid-second-third-or-fourth))
		     ((utf-8-second-third-and-fourth-of-four-octets? byte1 byte2 byte3)
		      (let ((N (utf-8-decode-four-octets byte0 byte1 byte2 byte3)))
			(if (utf-8-valid-code-point-from-4-octets? N)
			    (unsafe.fixnum->char N)
			  (%error-handler buffer-offset
					  "invalid code point as result \
                                           of decoding 4-bytes UTF-8 character"
					  byte0 byte1 byte2 N))))
		     (else
		      (%error-invalid-second-third-or-fourth)))))))))

    (define (%unexpected-eof-error message . irritants)
      (let ((mode (transcoder-error-handling-mode port.transcoder)))
	(case mode
	  ((ignore)
	   ;;To ignore means jump to the next.
	   (eof-object))
	  ((replace)
	   #\xFFFD)
	  ((raise)
	   (raise (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition message)
			     (make-irritants-condition irritants))))
	  (else
	   (assertion-violation who "vicare internal error: wrong transcoder error handling mode" mode)))))

    (define (%error-handler buffer-offset message . irritants)
      (let ((mode (transcoder-error-handling-mode port.transcoder)))
	(case mode
	  ((ignore)
	   ;;To ignore means jump to the next.
	   (recurse buffer-offset))
	  ((replace)
	   #\xFFFD)
	  ((raise)
	   (raise (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition message)
			     (make-irritants-condition irritants))))
	  (else
	   (assertion-violation who "vicare internal error: wrong transcoder error handling mode" mode)))))

    (main)))

;;; --------------------------------------------------------------------
;;; GET-CHAR and LOOKAHEAD-CHAR for ports with UTF-16 transcoder

(define (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag port who endianness)
  ;;Read  and return  from PORT  a UTF-16  encoded character;  leave the
  ;;input buffer pointing to the first byte after the read character.
  ;;
  ;;PORT  must  be  an  already  validated textual  input  port  with  a
  ;;bytevector  as  buffer  and  a  UTF-16  transcoder  with  endianness
  ;;matching ENDIANNESS.
  ;;
  ;;ENDIANNESS   must   be   one   among   the   symbols   accepted   by
  ;;BYTEVECTOR-U16-REF.
  ;;
  ;;In case of  error decoding the input: honor  the error handling mode
  ;;selected  in  the  PORT's  transcoder  and leave  the  input  buffer
  ;;pointing to the first byte after the offending sequence.
  ;;
  (with-port-having-bytevector-buffer (port)
    (define-inline (recurse)
      (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag port who endianness))

    (define (%error-handler message . irritants)
      ;;Handle  the error  honoring the  error handling  mode  in port's
      ;;transcoder.
      ;;
      (let ((mode (transcoder-error-handling-mode port.transcoder)))
	(case mode
	  ((ignore)
	   ;;To ignore means jump to the next.
	   (recurse))
	  ((replace)
	   #\xFFFD)
	  ((raise)
	   (raise (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition message)
			     (make-irritants-condition irritants))))
	  (else
	   (assertion-violation who "vicare internal error: invalid error handling mode" port mode)))))

    (define (%unexpected-eof-error message irritants)
      ;;Handle the unexpected EOF error honoring the error handling mode
      ;;in port's  transcoder.
      ;;
      (let ((mode (transcoder-error-handling-mode port.transcoder)))
	(case mode
	  ((ignore)
	   ;;To ignore means jump to the next.
	   (eof-object))
	  ((replace)
	   #\xFFFD)
	  ((raise)
	   (raise (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition message)
			     (make-irritants-condition irritants))))
	  (else
	   (assertion-violation who "vicare internal error: invalid error handling mode" port mode)))))

    (define (integer->char/invalid char-code-point)
      ;;If the argument is a  valid integer representation for a Unicode
      ;;character according to  R6RS: return the corresponding character
      ;;value, else handle the error.
      ;;
      ;;The fact that we validate  the 16-bit words in the UTF-16 stream
      ;;does *not*  guarantee that a  surrogate pair, once  decoded into
      ;;the integer  representation, is a  valid Unicode representation.
      ;;The integers in the invalid  range [#xD800, #xDFFF] can still be
      ;;encoded as surrogate pairs.
      ;;
      ;;This is why we check the representation with this function: this
      ;;check is *not* a repetition of the check on the 16-bit words.
      ;;
      (define errmsg
	"invalid code point decoded from UTF-16 surrogate pair")
      (cond ((unsafe.fx<= char-code-point #xD7FF)
	     (unsafe.fixnum->char char-code-point))
	    ((unsafe.fx<  char-code-point #xE000)
	     (%error-handler errmsg char-code-point))
	    ((unsafe.fx<= char-code-point #x10FFFF)
	     (unsafe.fixnum->char char-code-point))
	    (else
	     (%error-handler errmsg char-code-point))))

    (define-inline (%word-ref buffer.offset)
      (unsafe.bytevector-u16-ref port.buffer buffer.offset endianness))

    (let* ((buffer.offset-word0 port.buffer.index)
	   (buffer.offset-word1 (unsafe.fx+ 2 buffer.offset-word0))
	   (buffer.offset-past  (unsafe.fx+ 2 buffer.offset-word1)))
      (cond ((unsafe.fx<= buffer.offset-word1 port.buffer.used-size)
	     ;;There are at least two  bytes in the input buffer, enough
	     ;;for  a full UTF-16  character encoded  as single  16 bits
	     ;;word.
	     (let ((word0 (%word-ref buffer.offset-word0)))
	       (cond ((utf-16-single-word? word0)
		      ;;WORD0  is  in the  allowed  range  for a  UTF-16
		      ;;encoded character of 16 bits.
		      (set! port.buffer.index buffer.offset-word1)
		      (integer->char/invalid (utf-16-decode-single-word word0)))
		     ((not (utf-16-first-of-two-words? word0))
		      (set! port.buffer.index buffer.offset-word1)
		      (%error-handler "invalid 16-bit word while decoding UTF-16 characters \
                                       and expecting single word character or first word of \
                                       surrogate pair" word0))
		     ((unsafe.fx<= buffer.offset-past port.buffer.used-size)
		      ;;WORD0 is  the first  of a UTF-16  surrogate pair
		      ;;and  the input buffer  already holds  the second
		      ;;word.
		      (let ((word1 (%word-ref buffer.offset-word1)))
			(set! port.buffer.index buffer.offset-past)
			(if (utf-16-second-of-two-words? word1)
			    (integer->char/invalid (utf-16-decode-surrogate-pair word0 word1))
			  (%error-handler "invalid 16-bit word while decoding UTF-16 characters \
                                           and expecting second word in surrogate pair"
					  word0 word1))))
		     (else
		      ;;WORD0 is  the first of a  UTF-16 surrogate pair,
		      ;;but input  buffer does not hold  the full second
		      ;;word.
		      (%refill-bytevector-buffer-and-evaluate (port who)
			(if-end-of-file:
			 (set! port.buffer.index port.buffer.used-size)
			 (%unexpected-eof-error
			  "unexpected end of file while decoding UTF-16 characters \
                           and expecting second word in surrogate pair"
			  `(,(unsafe.bytevector-u8-ref port.buffer buffer.offset-word0)
			    ,(unsafe.bytevector-u8-ref port.buffer (unsafe.fxadd1 buffer.offset-word0))
			    . ,(if (unsafe.fx< buffer.offset-word1
					       port.buffer.used-size)
				   (list (unsafe.bytevector-u8-ref port.buffer buffer.offset-word1))
				 '()))))
			(if-successful-refill:
			 (recurse)))))))

	    ((unsafe.fx< buffer.offset-word0 port.buffer.used-size)
	     ;;There is only 1 byte in the input buffer.
	     (%refill-bytevector-buffer-and-evaluate (port who)
	       (if-end-of-file:
		;;The  input data  is corrupted  because we  expected at
		;;least a  16 bits word  to be there before  EOF.  Being
		;;the data  corrupted we discard the single  byte in the
		;;buffer.  (FIXME Is this  good whatever it is the error
		;;handling mode?)
		(set! port.buffer.index port.buffer.used-size)
		(%unexpected-eof-error
		 "unexpected end of file while decoding UTF-16 characters \
                  and expecting a whole 1-word or 2-word character"
		 `(,(unsafe.bytevector-u8-ref port.buffer buffer.offset-word0))))
	       (if-successful-refill:
		(recurse))))

	    (else
	     ;;The input buffer is empty.
	     (%refill-bytevector-buffer-and-evaluate (port who)
	       (if-end-of-file:	(eof-object))
	       (if-successful-refill: (recurse))))))))

(define (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who endianness buffer-offset)
  ;;Peek  and return  from PORT  a UTF-16  encoded character;  leave the
  ;;input buffer  pointing to the same  byte it was  pointing before the
  ;;call to this function.
  ;;
  ;;PORT  must  be  an  already  validated textual  input  port  with  a
  ;;bytevector  as  buffer  and  a UTF-16  transcoder  whose  endianness
  ;;matches ENDIANNESS.
  ;;
  ;;ENDIANNESS   must   be   one   among   the   symbols   accepted   by
  ;;BYTEVECTOR-U16-REF.
  ;;
  ;;In case of  error decoding the input: honor  the error handling mode
  ;;selected in the PORT's transcoder.
  ;;
  ;;BUFFER-OFFSET  must be  the offset  to add  to  PORT.BUFFER.INDEX to
  ;;obtain the index of the next byte in the buffer to read.  When doing
  ;;normal lookahead: it must be zero upon entering this function; it is
  ;;used in  recursive calls  to this function  to implement  the IGNORE
  ;;error handling  method.  When doing  double lookahead for  EOL style
  ;;conversion it  can be the offset  of the expected second  char in an
  ;;EOL sequence.
  ;;
  (with-port-having-bytevector-buffer (port)
    (define-inline (recurse buffer-offset)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who endianness buffer-offset))

    (define (%error-handler buffer-offset message . irritants)
      ;;Handle  the error  honoring the  error handling  mode  in port's
      ;;transcoder.
      ;;
      (let ((mode (transcoder-error-handling-mode port.transcoder)))
	(case mode
	  ((ignore)
	   ;;To ignore means jump to the next.
	   (recurse buffer-offset))
	  ((replace)
	   #\xFFFD)
	  ((raise)
	   (raise (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition message)
			     (make-irritants-condition irritants))))
	  (else
	   (assertion-violation who "vicare internal error: invalid error handling mode" port mode)))))

    (define (%unexpected-eof-error message irritants)
      ;;Handle the unexpected EOF error honoring the error handling mode
      ;;in port's  transcoder.
      ;;
      (let ((mode (transcoder-error-handling-mode port.transcoder)))
	(case mode
	  ((ignore)
	   ;;To ignore means jump to the next.
	   (eof-object))
	  ((replace)
	   #\xFFFD)
	  ((raise)
	   (raise (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition message)
			     (make-irritants-condition irritants))))
	  (else
	   (assertion-violation who "vicare internal error: invalid error handling mode" port mode)))))

    (define (integer->char/invalid char-code-point buffer-offset)
      ;;If the argument is a  valid integer representation for a Unicode
      ;;character according to  R6RS: return the corresponding character
      ;;value, else handle the error.
      ;;
      ;;The fact that we validate  the 16-bit words in the UTF-16 stream
      ;;does *not*  guarantee that a  surrogate pair, once  decoded into
      ;;the integer  representation, is a  valid Unicode representation.
      ;;The integers in the invalid  range [#xD800, #xDFFF] can still be
      ;;encoded as surrogate pairs.
      ;;
      ;;This is why we check the representation with this function: this
      ;;check is *not* a repetition of the check on the 16-bit words.
      ;;
      (let ((errmsg "invalid code point decoded from UTF-16 surrogate pair"))
	(cond ((unsafe.fx<= char-code-point #xD7FF)
	       (unsafe.fixnum->char char-code-point))
	      ((unsafe.fx<  char-code-point #xE000)
	       (%error-handler buffer-offset errmsg (list char-code-point)))
	      ((unsafe.fx<= char-code-point #x10FFFF)
	       (unsafe.fixnum->char char-code-point))
	      (else
	       (%error-handler buffer-offset errmsg (list char-code-point))))))

    (define-inline (%word-ref buffer.offset)
      (unsafe.bytevector-u16-ref port.buffer buffer.offset endianness))

    (let* ((buffer.offset-word0 (unsafe.fx+ port.buffer.index buffer-offset))
	   (buffer.offset-word1 (unsafe.fx+ 2 buffer.offset-word0))
	   (buffer.offset-past  (unsafe.fx+ 2 buffer.offset-word1)))
      (cond ((unsafe.fx<= buffer.offset-word1 port.buffer.used-size)
	     ;;There are at least two  bytes in the input buffer, enough
	     ;;for  a full UTF-16  character encoded  as single  16 bits
	     ;;word.
	     (let ((word0 (%word-ref buffer.offset-word0)))
	       (cond ((utf-16-single-word? word0)
		      (integer->char/invalid (utf-16-decode-single-word word0) buffer.offset-word1))
		     ((not (utf-16-first-of-two-words? word0))
		      (%error-handler (unsafe.fx+ 2 buffer-offset)
				      "invalid 16-bit word while decoding UTF-16 characters \
                                       and expecting single word character or first word in \
                                       surrogate pair"
				      word0))
		     ((unsafe.fx<= buffer.offset-past port.buffer.used-size)
		      ;;WORD0 is  the first  of a UTF-16  surrogate pair
		      ;;and  the input buffer  already holds  the second
		      ;;word.
		      (let ((word1		(%word-ref buffer.offset-word1))
			    (buffer-offset	(unsafe.fx+ 4 buffer-offset)))
			(if (utf-16-second-of-two-words? word1)
			    (integer->char/invalid (utf-16-decode-surrogate-pair word0 word1)
						   buffer-offset)
			  (%error-handler buffer-offset
					  "invalid 16-bit word while decoding UTF-16 characters \
                                           and expecting second word in surrogate pair"
					  word0 word1))))
		     (else
		      ;;WORD0 is  the first of a  UTF-16 surrogate pair,
		      ;;but  the input  buffer  does not  hold the  full
		      ;;second word.
		      (%refill-bytevector-buffer-and-evaluate (port who)
			(if-end-of-file:
			 (%unexpected-eof-error
			  "unexpected end of file while decoding UTF-16 characters \
                           and expecting second 16-bit word in surrogate pair"
			  `(,(unsafe.bytevector-u8-ref port.buffer buffer.offset-word0)
			    ,(unsafe.bytevector-u8-ref port.buffer (unsafe.fxadd1 buffer.offset-word0))
			    . ,(if (unsafe.fx< buffer.offset-word1
					       port.buffer.used-size)
				   (list (unsafe.bytevector-u8-ref port.buffer buffer.offset-word1))
				 '()))))
			(if-successful-refill:
			 (recurse buffer-offset)))))))

	    ((unsafe.fx< buffer.offset-word0 port.buffer.used-size)
	     ;;There is only 1 byte in the input buffer.
	     (%refill-bytevector-buffer-and-evaluate (port who)
	       (if-end-of-file:
		;;The  input data  is corrupted  because we  expected at
		;;least a 16 bits word to be there before EOF.
		(%unexpected-eof-error
		 "unexpected end of file after byte while decoding UTF-16 characters \
                  and expecting single word character or first word in surrogate pair"
		 `(,(unsafe.bytevector-u8-ref port.buffer buffer.offset-word0))))
	       (if-successful-refill:
		(recurse buffer-offset))))

	    (else
	     ;;The input buffer is empty.
	     (%refill-bytevector-buffer-and-evaluate (port who)
	       (if-end-of-file:	(eof-object))
	       (if-successful-refill: (recurse buffer-offset))))))))

;;; --------------------------------------------------------------------
;;; GET-CHAR and LOOKAHEAD-CHAR for ports with Latin-1 transcoder

(define-inline (%unsafe.read-char-from-port-with-fast-get-latin1-tag ?port ?who)
  ;;PORT must be a textual input port with bytevector buffer and Latin-1
  ;;transcoder.   Knowing that Latin-1  characters are  1 byte  wide: we
  ;;process here  the simple case of  one char available  in the buffer,
  ;;else  we   call  the   specialised  function  for   reading  Latin-1
  ;;characters.
  ;;
  (let ((port ?port))
    (with-port-having-bytevector-buffer (port)
      (let ((buffer.offset port.buffer.index))
	(if (unsafe.fx< buffer.offset port.buffer.used-size)
	    (begin
	      (set! port.buffer.index (unsafe.fxadd1 buffer.offset))
	      (let ((byte (unsafe.bytevector-u8-ref port.buffer buffer.offset)))
		(unsafe.fixnum->char byte)))
	  (%unsafe.read/peek-char-from-port-with-latin1-codec port ?who 1 0))))))

(define-inline (%unsafe.peek-char-from-port-with-fast-get-latin1-tag ?port ?who)
  ;;PORT must be a textual input port with bytevector buffer and Latin-1
  ;;transcoder.   Knowing that Latin-1  characters are  1 byte  wide: we
  ;;process here  the simple case of  one char available  in the buffer,
  ;;else  we   call  the   specialised  function  for   peeking  Latin-1
  ;;characters.
  ;;
  (let ((port ?port))
    (with-port-having-bytevector-buffer (port)
      (let ((buffer.offset-byte port.buffer.index))
	(if (unsafe.fx< buffer.offset-byte port.buffer.used-size)
	    (unsafe.fixnum->char (unsafe.bytevector-u8-ref port.buffer buffer.offset-byte))
	  (%unsafe.read/peek-char-from-port-with-latin1-codec port ?who 0 0))))))

(define (%unsafe.read/peek-char-from-port-with-latin1-codec port who buffer-index-increment offset)
  ;;Subroutine of %DO-READ-CHAR or %DO-PEEK-CHAR.  PORT must be a textual
  ;;input  port  with bytevector  buffer  and  Latin-1 transcoder;  such
  ;;buffer must be already fully consumed.
  ;;
  ;;Refill  the input  buffer  reading from  the  underlying device  and
  ;;return the a Scheme character from the buffer; if EOF is found while
  ;;reading from the underlying device: return the EOF object.
  ;;
  ;;When  BUFFER-INDEX-INCREMENT=1 and  OFFSET=0 this  function  acts as
  ;;GET-CHAR: it reads the next  character and consumes it advancing the
  ;;port position.
  ;;
  ;;When  BUFFER-INDEX-INCREMENT=0 and  OFFSET=0 this  function  acts as
  ;;PEEK-CHAR:  it  returns  the  next  character and  leaves  the  port
  ;;position unchanged.
  ;;
  ;;When  BUFFER-INDEX-INCREMENT=0 and  OFFSET>0 this  function  acts as
  ;;forwards PEEK-CHAR:  it reads the  the character at OFFSET  from the
  ;;current buffer  index and leaves the port  position unchanged.  This
  ;;usage is needed when converting EOL styles for PEEK-CHAR.  When this
  ;;usage is desired: usually it is OFFSET=1.
  ;;
  ;;Other  combinations  of  BUFFER-INDEX-INCREMENT  and  OFFSET,  while
  ;;possible, should not be needed.
  ;;
  (with-port-having-bytevector-buffer (port)
    (define (%available-data buffer.offset)
      (unless (unsafe.fxzero? buffer-index-increment)
	(port.buffer.index.incr! buffer-index-increment))
      (unsafe.fixnum->char (unsafe.bytevector-u8-ref port.buffer buffer.offset)))
    (let ((buffer.offset (unsafe.fx+ offset port.buffer.index)))
      (%maybe-refill-bytevector-buffer-and-evaluate (port who)
	(data-is-needed-at: buffer.offset)
	(if-end-of-file: (eof-object))
	(if-successful-refill:
	 ;;After refilling we must reload buffer indexes.
	 (%available-data (unsafe.fx+ offset port.buffer.index)))
	(if-available-data: (%available-data buffer.offset))))))

;;; --------------------------------------------------------------------
;;; GET-CHAR and LOOKAHEAD-CHAR for ports with string buffer

(define-inline (%unsafe.read-char-from-port-with-fast-get-char-tag ?port ?who)
  ;;PORT must  be a textual input  port with a Scheme  string as buffer.
  ;;We process here the simple case of one char available in the buffer,
  ;;else we call the specialised function for reading characters.
  ;;
  (let ((port ?port))
    (with-port-having-string-buffer (port)
      (let ((buffer.offset port.buffer.index))
	(if (unsafe.fx< buffer.offset port.buffer.used-size)
	    (begin
	      (set! port.buffer.index (unsafe.fxadd1 buffer.offset))
	      (unsafe.string-ref port.buffer buffer.offset))
	  (%unsafe.read/peek-char-from-port-with-string-buffer port ?who 1 0))))))

(define-inline (%unsafe.peek-char-from-port-with-fast-get-char-tag ?port ?who)
  ;;PORT must  be a textual input  port with a Scheme  string as buffer.
  ;;We process here the simple case of one char available in the buffer,
  ;;else we call the specialised function for reading characters.
  ;;
  (let ((port ?port))
    (with-port-having-string-buffer (port)
      (let ((buffer.offset-char port.buffer.index))
	(if (unsafe.fx< buffer.offset-char port.buffer.used-size)
	    (unsafe.string-ref port.buffer buffer.offset-char)
	  (%unsafe.read/peek-char-from-port-with-string-buffer port ?who 0 0))))))

(define (%unsafe.read/peek-char-from-port-with-string-buffer port who buffer-index-increment offset)
  ;;Subroutine  of  %DO-READ-CHAR  or  %DO-PEEK-CHAR.  PORT  must  be  a
  ;;textual input port with a  Scheme string as buffer; such buffer must
  ;;have been already fully consumed.
  ;;
  ;;Refill  the input  buffer  reading from  the  underlying device  and
  ;;return the a Scheme character from the buffer; if EOF is found while
  ;;reading from the underlying device: return the EOF object.
  ;;
  ;;When  BUFFER-INDEX-INCREMENT=1 and  OFFSET=0 this  function  acts as
  ;;GET-CHAR: it reads the next  character and consumes it advancing the
  ;;port position.
  ;;
  ;;When  BUFFER-INDEX-INCREMENT=0 and  OFFSET=0 this  function  acts as
  ;;PEEK-CHAR:  it  returns  the  next  character and  leaves  the  port
  ;;position unchanged.
  ;;
  ;;When  BUFFER-INDEX-INCREMENT=0 and  OFFSET>0 this  function  acts as
  ;;forwards PEEK-CHAR:  it reads the  the character at OFFSET  from the
  ;;current buffer  index and leaves the port  position unchanged.  This
  ;;usage is needed when converting EOL styles for PEEK-CHAR.  When this
  ;;usage is desired: usually it is OFFSET=1.
  ;;
  ;;Other  combinations  of  BUFFER-INDEX-INCREMENT  and  OFFSET,  while
  ;;possible, should not be needed.
  ;;
  (with-port-having-string-buffer (port)
    (define (%available-data buffer.offset)
      (unless (unsafe.fxzero? buffer-index-increment)
	(port.buffer.index.incr! buffer-index-increment))
      (unsafe.string-ref port.buffer buffer.offset))
    (let ((buffer.offset (unsafe.fx+ offset port.buffer.index)))
      (%maybe-refill-string-buffer-and-evaluate (port who)
	(data-is-needed-at: buffer.offset)
	(if-end-of-file: (eof-object))
	(if-successful-refill:
	 ;;After refilling we must reload buffer indexes.
	 (%available-data (unsafe.fx+ offset port.buffer.index)))
	(if-available-data: (%available-data buffer.offset))))))


;;;; string input

(define (%unsafe.get-string-n! who port dst.str dst.start count)
  ;;Subroutine  of GET-STRING-N!,  GET-STRING-N and  GET-STRING-ALL.  It
  ;;assumes the arguments have already been validated.
  ;;
  ;;DST.START and  COUNT must  be exact, non--negative  integer objects,
  ;;with  COUNT  representing  the  number  of characters  to  be  read.
  ;;DST.STR must be a string with at least DST.START+COUNT characters.
  ;;
  ;;If COUNT  characters are available before  an end of  file, they are
  ;;written  into DST.STR  starting  at index  DST.START,  and COUNT  is
  ;;returned.
  ;;
  ;;If fewer characters are available before  an end of file, but one or
  ;;more can be read, those characters are written into DST.STR starting
  ;;at index  DST.START and  the number of  characters actually  read is
  ;;returned as an exact integer object.
  ;;
  ;;If no characters  can be read before an end of  file, the EOF object
  ;;is returned.
  ;;
  ;;IMPLEMENTATION RESTRICTION The DST.START and COUNT arguments must be
  ;;fixnums; DST.START+COUNT must be a fixnum.
  ;;
  (define-inline (main)
    (let ((dst.past (+ dst.start count))
	  (eol-bits (%unsafe.port-eol-style-bits port)))
      (%case-textual-input-port-fast-tag (port who)
	((FAST-GET-UTF8-TAG)
	 (%get-it eol-bits dst.past
		  %unsafe.read-char-from-port-with-fast-get-utf8-tag
		  %unsafe.peek-char-from-port-with-fast-get-utf8-tag))
	((FAST-GET-CHAR-TAG)
	 (%get-it eol-bits dst.past
		  %unsafe.read-char-from-port-with-fast-get-char-tag
		  %unsafe.peek-char-from-port-with-fast-get-char-tag))
	((FAST-GET-LATIN-TAG)
	 (%get-it eol-bits dst.past
		  %unsafe.read-char-from-port-with-fast-get-latin1-tag
		  %unsafe.peek-char-from-port-with-fast-get-latin1-tag))
	((FAST-GET-UTF16LE-TAG)
	 (%get-it eol-bits dst.past %read-utf16le %peek-utf16le))
	((FAST-GET-UTF16BE-TAG)
	 (%get-it eol-bits dst.past %read-utf16be %peek-utf16be)))))

  (define-inline (%get-it eol-bits dst.past ?read-char ?peek-char)
    (let loop ((dst.index dst.start))
      (let ((ch (?read-char port who)))
	(if (eof-object? ch)
	    (if (unsafe.fx= dst.index dst.start)
		ch ;return EOF
	      (unsafe.fx- dst.index dst.start)) ;return the numbe of chars read
	  (let ((ch (cond ((unsafe.fxzero? eol-bits) ;EOL style none
			   ch)
			  ((unsafe.char-is-single-char-line-ending? ch)
			   LINEFEED-CHAR)
			  ((unsafe.char-is-carriage-return? ch)
			   (let ((ch1 (?peek-char port who)))
			     (cond ((eof-object? ch1)
				    (void))
				   ((unsafe.char-is-newline-after-carriage-return? ch1)
				    (?read-char port who)))
			     LINEFEED-CHAR))
			  (else ch))))
	    (unsafe.string-set! dst.str dst.index ch)
	    (let ((dst.index (unsafe.fxadd1 dst.index)))
	      (if (unsafe.fx= dst.index dst.past)
		  (unsafe.fx- dst.index dst.start)
		(loop dst.index))))))))

  (define-inline (%read-utf16le ?port ?who)
    (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little))

  (define-inline (%peek-utf16le ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little 0))

  (define-inline (%read-utf16be ?port ?who)
    (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big))

  (define-inline (%peek-utf16be ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big 0))

  (main))

(define (get-string-n port requested-count)
  ;;Defined by  R6RS.  REQUESTED-COUNT  must be an  exact, non--negative
  ;;integer object, representing the number of characters to be read.
  ;;
  ;;The  GET-STRING-N  procedure  reads  from the  textual  input  PORT,
  ;;blocking   as  necessary,   until  REQUESTED-COUNT   characters  are
  ;;available, or until an end of file is reached.
  ;;
  ;;If  REQUESTED-COUNT characters  are  available before  end of  file,
  ;;GET-STRING-N  returns a string  consisting of  those REQUESTED-COUNT
  ;;characters.
  ;;
  ;;If fewer characters are available before  an end of file, but one or
  ;;more  characters   can  be  read,  GET-STRING-N   returns  a  string
  ;;containing those characters.
  ;;
  ;;In either  case, the input  port is updated  to point just  past the
  ;;characters  read.  If no  characters can  be read  before an  end of
  ;;file, the end-of-file object is returned.
  ;;
  ;;IMPLEMENTATION  RESTRICTION The REQUESTED-COUNT  argument must  be a
  ;;fixnum.
  ;;
  (define who 'get-string-n)
  (with-arguments-validation (who)
      ((port          port)
       (fixnum-count  requested-count))
    (if (unsafe.fxzero? requested-count)
	""
      (let* ((dst.str	(unsafe.make-string requested-count))
	     (count	(%unsafe.get-string-n! who port dst.str 0 requested-count)))
	(cond ((eof-object? count)
	       count)
	      ((unsafe.fx= count requested-count)
	       dst.str)
	      (else
	       (unsafe.substring dst.str 0 count)))))))

(define (get-string-n! port dst.str dst.start count)
  ;;Defined by  R6RS.  DST.START and COUNT must  be exact, non--negative
  ;;integer objects, with COUNT representing the number of characters to
  ;;be read.   DST.STR must  be a string  with at  least DST.START+COUNT
  ;;characters.
  ;;
  ;;The GET-STRING-N!   procedure reads from  the textual input  PORT in
  ;;the same manner as GET-STRING-N.
  ;;
  ;;If COUNT  characters are available before  an end of  file, they are
  ;;written  into DST.STR  starting  at index  DST.START,  and COUNT  is
  ;;returned.
  ;;
  ;;If fewer characters are available before  an end of file, but one or
  ;;more can be read, those characters are written into DST.STR starting
  ;;at index  DST.START and  the number of  characters actually  read is
  ;;returned as an exact integer object.
  ;;
  ;;If no characters  can be read before an end of  file, the EOF object
  ;;is returned.
  ;;
  ;;IMPLEMENTATION RESTRICTION The DST.START and COUNT arguments must be
  ;;fixnums.
  ;;
  (define who 'get-string-n!)
  (with-arguments-validation (who)
      ((port		    port)
       (string		    dst.str)
       (fixnum-start-index  dst.start)
       (fixnum-count	    count)
       (unsafe.start-index-for-string      dst.start dst.str)
       (unsafe.count-from-start-in-string  count dst.start dst.str))
    (if (unsafe.fxzero? count)
	count
      (%unsafe.get-string-n! who port dst.str dst.start count))))

(define (get-string-all port)
  ;;Defined by R6RS.   Read from the textual input PORT  until an end of
  ;;file,  decoding characters in  the same  manner as  GET-STRING-N and
  ;;GET-STRING-N!.
  ;;
  ;;If  characters  are available  before  the  end  of file,  a  string
  ;;containing all  the characters decoded  from that data  is returned.
  ;;If  no  character  precedes the  end  of  file,  the EOF  object  is
  ;;returned.
  ;;
  ;;IMPLEMENTATION RESTRICTION The maximum  length of the retuned string
  ;;is the greatest fixnum.
  ;;
  (define who 'get-string-all)
  (with-arguments-validation (who)
      ((port port))
    (let ((dst.len (string-port-buffer-size)))
      (let next-buffer-string ((output.len	0)
			       (output.strs	'())
			       (dst.str		(unsafe.make-string dst.len)))
	(let ((count (%unsafe.get-string-n! who port dst.str 0 dst.len)))
	  (cond ((eof-object? count)
		 (if (null? output.strs)
		     count
		   (%unsafe.string-reverse-and-concatenate who output.strs output.len)))
		((unsafe.fx= count dst.len)
		 (next-buffer-string (unsafe.fx+ output.len dst.len)
				     (cons dst.str output.strs)
				     (unsafe.make-string dst.len)))
		(else
		 (%unsafe.string-reverse-and-concatenate who
							 (cons (substring dst.str 0 count) output.strs)
							 (unsafe.fx+ count output.len)))))))))


;;;; string line input

(define (get-line port)
  ;;Defined  by  R6RS.  Read  from  the textual  input  PORT  up to  and
  ;;including the linefeed character or end of file, decoding characters
  ;;in the same manner as GET-STRING-N and GET-STRING-N!.
  ;;
  ;;If a linefeed character is read, a string containing all of the text
  ;;up to  (but not including)  the linefeed character is  returned, and
  ;;the port is updated to point just past the linefeed character.
  ;;
  ;;If an  end of file is  encountered before any  linefeed character is
  ;;read, but some characters have  been read and decoded as characters,
  ;;a string containing those characters is returned.
  ;;
  ;;If an end of file is encountered before any characters are read, the
  ;;EOF object is returned.
  ;;
  ;;NOTE The end-of-line style, if not NONE, will cause all line endings
  ;;to be read as linefeed characters.
  ;;
  (%do-get-line port 'get-line))

(define read-line
  ;;Defined by Ikarus.  Like GET-LINE.
  ;;
  (case-lambda
   (()
    (%do-get-line (current-input-port) 'read-line))
   ((port)
    (%do-get-line port 'read-line))))

(define (%do-get-line port who)
  (define-inline (main)
    (%case-textual-input-port-fast-tag (port who)
      ((FAST-GET-UTF8-TAG)
       (%get-it %unsafe.read-char-from-port-with-fast-get-utf8-tag
		%unsafe.peek-char-from-port-with-fast-get-utf8-tag))
      ((FAST-GET-CHAR-TAG)
       (%get-it %unsafe.read-char-from-port-with-fast-get-char-tag
		%unsafe.peek-char-from-port-with-fast-get-char-tag))
      ((FAST-GET-LATIN-TAG)
       (%get-it %unsafe.read-char-from-port-with-fast-get-latin1-tag
		%unsafe.peek-char-from-port-with-fast-get-latin1-tag))
      ((FAST-GET-UTF16LE-TAG)
       (%get-it %read-utf16le %peek-utf16le))
      ((FAST-GET-UTF16BE-TAG)
       (%get-it %read-utf16be %peek-utf16be))))

  (define-inline (%get-it ?read-char ?peek-char)
    (let ((eol-bits (%unsafe.port-eol-style-bits port)))
      (let loop ((port			port)
		 (number-of-chars	0)
		 (reverse-chars		'()))
	(let ((ch (?read-char port who)))
	  (if (eof-object? ch)
	      (if (null? reverse-chars)
		  ch
		(%unsafe.reversed-chars->string number-of-chars reverse-chars))
	    (let ((ch (%convert-if-line-ending eol-bits ch ?read-char ?peek-char)))
	      (if (unsafe.char= ch LINEFEED-CHAR)
		  (%unsafe.reversed-chars->string number-of-chars reverse-chars)
		(loop port (unsafe.fxadd1 number-of-chars) (cons ch reverse-chars)))))))))

  (define-inline (%convert-if-line-ending eol-bits ch ?read-char ?peek-char)
    (cond ((unsafe.fxzero? eol-bits) ;EOL style none
	   ch)
	  ((unsafe.char-is-single-char-line-ending? ch)
	   LINEFEED-CHAR)
	  ((unsafe.char-is-carriage-return? ch)
	   (let ((ch1 (?peek-char port who)))
	     (cond ((eof-object? ch1)
		    (void))
		   ((unsafe.char-is-newline-after-carriage-return? ch1)
		    (?read-char port who)))
	     LINEFEED-CHAR))
	  (else ch)))

  (define (%unsafe.reversed-chars->string dst.len reverse-chars)
    (let next-char ((dst.str       (unsafe.make-string dst.len))
		    (dst.index     (unsafe.fxsub1 dst.len))
		    (reverse-chars reverse-chars))
      (if (null? reverse-chars)
	  dst.str
	(begin
	  (unsafe.string-set! dst.str dst.index (car reverse-chars))
	  (next-char dst.str (unsafe.fxsub1 dst.index) (cdr reverse-chars))))))

  (define-inline (%read-utf16le ?port ?who)
    (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little))

  (define-inline (%peek-utf16le ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little 0))

  (define-inline (%read-utf16be ?port ?who)
    (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big))

  (define-inline (%peek-utf16be ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big 0))

  (main))


;;;; byte and bytevector output

(define (put-u8 port octet)
  ;;Defined  by  R6RS.   Write  OCTET  to the  output  port  and  return
  ;;unspecified values.
  ;;
  (define who 'put-u8)
  (%case-binary-output-port-fast-tag (port who)
    ((FAST-PUT-BYTE-TAG)
     (with-arguments-validation (who)
	 ((octet octet))
       (with-port-having-bytevector-buffer (port)
	 (%flush-bytevector-buffer-and-evaluate (port who)
	   (room-is-needed-for: 1)
	   (if-available-room:
	    (let* ((buffer.index	port.buffer.index)
		   (buffer.past		(unsafe.fxadd1 buffer.index))
		   (buffer.used-size	port.buffer.used-size))
	      (debug-assert (<= buffer.index buffer.used-size))
	      (unsafe.bytevector-u8-set! port.buffer buffer.index octet)
	      (when (unsafe.fx= buffer.index buffer.used-size)
		(set! port.buffer.used-size buffer.past))
	      (set! port.buffer.index buffer.past))))
	 (when port.buffer-mode-none?
	   (%unsafe.flush-output-port port who)))))))

(define put-bytevector
  ;;Defined by R6RS.  START and COUNT must be non-negative exact integer
  ;;objects that default to 0 and:
  ;;
  ;;   (- (bytevector-length BV) START)
  ;;
  ;;respectively.  BV must  have a length of at  least START+COUNT.  The
  ;;PUT-BYTEVECTOR procedure writes the COUNT bytes of the bytevector BV
  ;;starting  at index  START to  the output  port.   The PUT-BYTEVECTOR
  ;;procedure returns unspecified values.
  ;;
  (case-lambda
   ((port bv)
    (define who 'put-bytevector)
    (%case-binary-output-port-fast-tag (port who)
      ((FAST-PUT-BYTE-TAG)
       (with-arguments-validation (who)
	   ((bytevector bv))
	 (%unsafe.put-bytevector port bv 0 (unsafe.bytevector-length bv) who)))))
   ((port bv start)
    (define who 'put-bytevector)
    (%case-binary-output-port-fast-tag (port who)
      ((FAST-PUT-BYTE-TAG)
       (with-arguments-validation (who)
	   ((bytevector          bv)
	    (fixnum-start-index  start)
	    (unsafe.start-index-for-bytevector start bv))
	 (%unsafe.put-bytevector port bv start (unsafe.fx- (unsafe.bytevector-length bv) start) who)))))
   ((port bv start count)
    (define who 'put-bytevector)
    (%case-binary-output-port-fast-tag (port who)
      ((FAST-PUT-BYTE-TAG)
       (with-arguments-validation (who)
	   ((bytevector          bv)
	    (fixnum-start-index  start)
	    (unsafe.start-index-for-bytevector start bv)
	    (fixnum-count        count)
	    (unsafe.count-from-start-in-bytevector count start bv))
	 (%unsafe.put-bytevector port bv start count who)))))))

(define (%unsafe.put-bytevector port src.bv src.start count who)
  ;;Write COUNT  bytes from the  bytevector SRC.BV to the  binary output
  ;;PORT starting at offset SRC.START.  Return unspecified values.
  ;;
  (with-port-having-bytevector-buffer (port)
    ;;Write octets to  the buffer and, when the buffer  fills up, to the
    ;;underlying device.
    (let try-again-after-flushing-buffer ((src.start	src.start)
					  (count	count)
					  (room		(port.buffer.room)))
      (cond ((unsafe.fxzero? room)
	     ;;The buffer is full.
	     (%unsafe.flush-output-port port who)
	     (try-again-after-flushing-buffer src.start count (port.buffer.room)))
	    ((unsafe.fx<= count room)
	     ;;Success!!! There is enough room  in the buffer for all of
	     ;;the COUNT octets.
	     (unsafe.bytevector-copy!/count src.bv src.start port.buffer port.buffer.index count)
	     (port.buffer.index.incr! count)
	     (when (unsafe.fx< port.buffer.used-size port.buffer.index)
	       (set! port.buffer.used-size port.buffer.index))
	     (when port.buffer-mode-none?
	       (%unsafe.flush-output-port port who)))
	    (else
	     ;;The buffer can hold some but not all of the COUNT bytes.
	     (debug-assert (unsafe.fx> count room))
	     (unsafe.bytevector-copy!/count src.bv src.start port.buffer port.buffer.index room)
	     (set! port.buffer.index     port.buffer.size)
	     (set! port.buffer.used-size port.buffer.size)
	     (%unsafe.flush-output-port port who)
	     (try-again-after-flushing-buffer (unsafe.fx+ src.start room)
					      (unsafe.fx- count room)
					      (port.buffer.room)))))))


;;;; character output

(define write-char
  ;;Defined by R6RS.   Write an encoding of the character  CH to the the
  ;;textual output PORT, and return unspecified values.
  ;;
  ;;If  PORT  is   omitted,  it  defaults  to  the   value  returned  by
  ;;CURRENT-OUTPUT-PORT.
  ;;
  (case-lambda
   ((ch port)
    (%do-put-char port ch 'write-char))
   ((ch)
    (%do-put-char (current-output-port) ch 'write-char))))

(define (put-char port ch)
  ;;Defined by R6RS.  Write CH to the PORT.  Return unspecified values.
  ;;
  (%do-put-char port ch 'put-char))

(define (%do-put-char port ch who)
  (with-arguments-validation (who)
      ((port port)
       (char ch))
    (let* ((code-point	(unsafe.char->fixnum ch))
	   (newline?	(unsafe.fx= code-point LINEFEED-CODE-POINT))
	   (eol-bits	(%unsafe.port-eol-style-bits port)))
      (%case-textual-output-port-fast-tag (port who)
	((FAST-PUT-UTF8-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-utf8-tag port ch code-point who))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%unsafe.put-char-to-port-with-fast-utf8-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-utf8-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%unsafe.put-char-to-port-with-fast-utf8-tag
		 port LINEFEED-CHAR LINEFEED-CODE-POINT who))
	       ((EOL-NEXT-LINE-TAG)
		(%unsafe.put-char-to-port-with-fast-utf8-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		(%unsafe.put-char-to-port-with-fast-utf8-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%unsafe.put-char-to-port-with-fast-utf8-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who))
	       ((EOL-LINE-SEPARATOR-TAG)
		(%unsafe.put-char-to-port-with-fast-utf8-tag
		 port LINE-SEPARATOR-CHAR LINE-SEPARATOR-CODE-POINT who))
	       (else
		(%unsafe.put-char-to-port-with-fast-utf8-tag port ch code-point who)))
	   (%unsafe.put-char-to-port-with-fast-utf8-tag port ch code-point who)))

	((FAST-PUT-CHAR-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-char-tag port ch code-point who))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%unsafe.put-char-to-port-with-fast-char-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-char-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%unsafe.put-char-to-port-with-fast-char-tag
		 port LINEFEED-CHAR LINEFEED-CODE-POINT who))
	       ((EOL-NEXT-LINE-TAG)
		(%unsafe.put-char-to-port-with-fast-char-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		(%unsafe.put-char-to-port-with-fast-char-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%unsafe.put-char-to-port-with-fast-char-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who))
	       ((EOL-LINE-SEPARATOR-TAG)
		(%unsafe.put-char-to-port-with-fast-char-tag
		 port LINE-SEPARATOR-CHAR LINE-SEPARATOR-CODE-POINT who))
	       (else
		(%unsafe.put-char-to-port-with-fast-char-tag port ch code-point who)))
	   (%unsafe.put-char-to-port-with-fast-char-tag port ch code-point who)))

	((FAST-PUT-LATIN-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-latin1-tag port ch code-point who))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%unsafe.put-char-to-port-with-fast-latin1-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-latin1-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%unsafe.put-char-to-port-with-fast-latin1-tag
		 port LINEFEED-CHAR LINEFEED-CODE-POINT who))
	       ((EOL-NEXT-LINE-TAG)
		(assertion-violation who
		  "EOL style NEL unsupported by Latin-1 codecs" port))
	       ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		(assertion-violation who
		  "EOL style CRNEL unsupported by Latin-1 codecs" port))
	       ((EOL-LINE-SEPARATOR-TAG)
		(assertion-violation who
		  "EOL style LS unsupported by Latin-1 transcoders" port))
	       (else
		(%unsafe.put-char-to-port-with-fast-latin1-tag port ch code-point who)))
	   (%unsafe.put-char-to-port-with-fast-latin1-tag port ch code-point who)))

	((FAST-PUT-UTF16LE-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'little))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'little))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'little)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port LINEFEED-CHAR LINEFEED-CODE-POINT who 'little))
	       ((EOL-NEXT-LINE-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who 'little))
	       ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'little)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who 'little))
	       ((EOL-LINE-SEPARATOR-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port LINE-SEPARATOR-CHAR LINE-SEPARATOR-CODE-POINT who 'little))
	       (else
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'little)))
	   (%unsafe.put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'little)))

	((FAST-PUT-UTF16BE-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'big))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'big))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'big)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port LINEFEED-CHAR LINEFEED-CODE-POINT who 'big))
	       ((EOL-NEXT-LINE-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who 'big))
	       ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'big)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who 'big))
	       ((EOL-LINE-SEPARATOR-TAG)
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag
		 port LINE-SEPARATOR-CHAR LINE-SEPARATOR-CODE-POINT who 'big))
	       (else
		(%unsafe.put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'big)))
	   (%unsafe.put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'big))))

      (when (or (%unsafe.port-buffer-mode-none? port)
		(and newline? (%unsafe.port-buffer-mode-line? port)))
	(%unsafe.flush-output-port port who)))))

;;; --------------------------------------------------------------------
;;; PUT-CHAR for port with string buffer

(define (%unsafe.put-char-to-port-with-fast-char-tag port ch code-point who)
  (with-port-having-string-buffer (port)
    (let retry-after-flushing-buffer ()
      (let* ((buffer.offset	port.buffer.index)
	     (buffer.past	(unsafe.fxadd1 buffer.offset)))
	(if (unsafe.fx<= buffer.past port.buffer.size)
	    (begin
	      (unsafe.string-set! port.buffer buffer.offset ch)
	      (set! port.buffer.index buffer.past)
	      (when (unsafe.fx> buffer.past port.buffer.used-size)
		(set! port.buffer.used-size buffer.past)))
	  (begin
	    (%unsafe.flush-output-port port who)
	    (retry-after-flushing-buffer)))))))

;;; --------------------------------------------------------------------
;;; PUT-CHAR for port with bytevector buffer and UTF-8 transcoder

(define-inline (%unsafe.put-char-to-port-with-fast-utf8-tag ?port ch code-point who)
  ;;Write  to PORT the  character CODE-POINT  encoded by  UTF-8.  Expand
  ;;inline the common case of single-octet encoding, call a function for
  ;;multioctet characters.
  ;;
  (let ((port ?port))
    (if (utf-8-single-octet-code-point? code-point)
	(with-port-having-bytevector-buffer (port)
	  (let retry-after-flushing-buffer ()
	    (let* ((buffer.offset	port.buffer.index)
		   (buffer.past		(unsafe.fxadd1 buffer.offset)))
	      (if (unsafe.fx<= buffer.past port.buffer.size)
		  (begin
		    (unsafe.bytevector-u8-set! port.buffer buffer.offset code-point)
		    (set! port.buffer.index buffer.past)
		    (when (unsafe.fx> buffer.past port.buffer.used-size)
		      (set! port.buffer.used-size buffer.past)))
		(begin
		  (%unsafe.flush-output-port port who)
		  (retry-after-flushing-buffer))))))
      (%unsafe.put-char-utf8-multioctet-char port ch code-point who))))

(define (%unsafe.put-char-utf8-multioctet-char port ch code-point who)
  ;;Write to  PORT the possibly multioctet CODE-POINT  encoded by UTF-8.
  ;;No  error handling  is performed  because UTF-8  can encode  all the
  ;;Unicode characters.
  ;;
  (cond ((utf-8-single-octet-code-point? code-point)
	 (let ((octet0 (utf-8-encode-single-octet code-point)))
	   (with-port-having-bytevector-buffer (port)
	     (let retry-after-flushing-buffer ()
	       (let* ((buffer.offset-octet0	port.buffer.index)
		      (buffer.past		(unsafe.fxadd1 buffer.offset-octet0)))
		 (define-inline (%buffer-set! offset octet)
		   (unsafe.bytevector-u8-set! port.buffer offset octet))
		 (if (unsafe.fx<= buffer.past port.buffer.size)
		     (begin
		       (%buffer-set! buffer.offset-octet0 octet0)
		       (set! port.buffer.index buffer.past)
		       (when (unsafe.fx> buffer.past port.buffer.used-size)
			 (set! port.buffer.used-size buffer.past)))
		   (begin
		     (%unsafe.flush-output-port port who)
		     (retry-after-flushing-buffer))))))))

	((utf-8-two-octets-code-point? code-point)
	 (let ((octet0 (utf-8-encode-first-of-two-octets  code-point))
	       (octet1 (utf-8-encode-second-of-two-octets code-point)))
	   (with-port-having-bytevector-buffer (port)
	     (let retry-after-flushing-buffer ()
	       (let* ((buffer.offset-octet0	port.buffer.index)
		      (buffer.offset-octet1	(unsafe.fxadd1 buffer.offset-octet0))
		      (buffer.past		(unsafe.fxadd1 buffer.offset-octet1)))
		 (define-inline (%buffer-set! offset octet)
		   (unsafe.bytevector-u8-set! port.buffer offset octet))
		 (if (unsafe.fx<= buffer.past port.buffer.size)
		     (begin
		       (%buffer-set! buffer.offset-octet0 octet0)
		       (%buffer-set! buffer.offset-octet1 octet1)
		       (set! port.buffer.index buffer.past)
		       (when (unsafe.fx> buffer.past port.buffer.used-size)
			 (set! port.buffer.used-size buffer.past)))
		   (begin
		     (%unsafe.flush-output-port port who)
		     (retry-after-flushing-buffer))))))))

	((utf-8-three-octets-code-point? code-point)
	 (let ((octet0 (utf-8-encode-first-of-three-octets  code-point))
	       (octet1 (utf-8-encode-second-of-three-octets code-point))
	       (octet2 (utf-8-encode-third-of-three-octets  code-point)))
	   (with-port-having-bytevector-buffer (port)
	     (let retry-after-flushing-buffer ()
	       (let* ((buffer.offset-octet0	port.buffer.index)
		      (buffer.offset-octet1	(unsafe.fxadd1 buffer.offset-octet0))
		      (buffer.offset-octet2	(unsafe.fxadd1 buffer.offset-octet1))
		      (buffer.past		(unsafe.fxadd1 buffer.offset-octet2)))
		 (define-inline (%buffer-set! offset octet)
		   (unsafe.bytevector-u8-set! port.buffer offset octet))
		 (if (unsafe.fx<= buffer.past port.buffer.size)
		     (begin
		       (%buffer-set! buffer.offset-octet0 octet0)
		       (%buffer-set! buffer.offset-octet1 octet1)
		       (%buffer-set! buffer.offset-octet2 octet2)
		       (set! port.buffer.index buffer.past)
		       (when (unsafe.fx> buffer.past port.buffer.used-size)
			 (set! port.buffer.used-size buffer.past)))
		   (begin
		     (%unsafe.flush-output-port port who)
		     (retry-after-flushing-buffer))))))))

	(else
	 (debug-assert (utf-8-four-octets-code-point? code-point))
	 (let ((octet0 (utf-8-encode-first-of-four-octets  code-point))
	       (octet1 (utf-8-encode-second-of-four-octets code-point))
	       (octet2 (utf-8-encode-third-of-four-octets  code-point))
	       (octet3 (utf-8-encode-fourth-of-four-octets code-point)))
	   (with-port-having-bytevector-buffer (port)
	     (let retry-after-flushing-buffer ()
	       (let* ((buffer.offset-octet0	port.buffer.index)
		      (buffer.offset-octet1	(unsafe.fxadd1 buffer.offset-octet0))
		      (buffer.offset-octet2	(unsafe.fxadd1 buffer.offset-octet1))
		      (buffer.offset-octet3	(unsafe.fxadd1 buffer.offset-octet2))
		      (buffer.past		(unsafe.fxadd1 buffer.offset-octet3)))
		 (define-inline (%buffer-set! offset octet)
		   (unsafe.bytevector-u8-set! port.buffer offset octet))
		 (if (unsafe.fx<= buffer.past port.buffer.size)
		     (begin
		       (%buffer-set! buffer.offset-octet0 octet0)
		       (%buffer-set! buffer.offset-octet1 octet1)
		       (%buffer-set! buffer.offset-octet2 octet2)
		       (%buffer-set! buffer.offset-octet3 octet3)
		       (set! port.buffer.index buffer.past)
		       (when (unsafe.fx> buffer.past port.buffer.used-size)
			 (set! port.buffer.used-size buffer.past)))
		   (begin
		     (%unsafe.flush-output-port port who)
		     (retry-after-flushing-buffer))))))))))

;;; --------------------------------------------------------------------
;;; PUT-CHAR for port with bytevector buffer and UTF-16 transcoder

(define (%unsafe.put-char-to-port-with-fast-utf16xe-tag port ch code-point who endianness)
  (cond ((utf-16-single-word-code-point? code-point)
	 (let ((word0 (utf-16-encode-single-word code-point)))
	   (with-port-having-bytevector-buffer (port)
	     (let retry-after-flushing-buffer ()
	       (let* ((buffer.offset-word0	port.buffer.index)
		      (buffer.past		(unsafe.fx+ 2 buffer.offset-word0)))
		 (define-inline (%buffer-set! offset word)
		   (unsafe.bytevector-u16-set! port.buffer offset word endianness))
		 (if (unsafe.fx<= buffer.past port.buffer.size)
		     (begin
		       (%buffer-set! buffer.offset-word0 word0)
		       (set! port.buffer.index buffer.past)
		       (when (unsafe.fx> buffer.past port.buffer.used-size)
			 (set! port.buffer.used-size buffer.past)))
		   (begin
		     (%unsafe.flush-output-port port who)
		     (retry-after-flushing-buffer))))))))
	(else
	 (let ((word0 (utf-16-encode-first-of-two-words  code-point))
	       (word1 (utf-16-encode-second-of-two-words code-point)))
	   (with-port-having-bytevector-buffer (port)
	     (let retry-after-flushing-buffer ()
	       (let* ((buffer.offset-word0	port.buffer.index)
		      (buffer.offset-word1	(unsafe.fx+ 2 buffer.offset-word0))
		      (buffer.past		(unsafe.fx+ 2 buffer.offset-word1)))
		 (define-inline (%buffer-set! offset word)
		   (unsafe.bytevector-u16-set! port.buffer offset word endianness))
		 (if (unsafe.fx<= buffer.past port.buffer.size)
		     (begin
		       (%buffer-set! buffer.offset-word0 word0)
		       (%buffer-set! buffer.offset-word1 word1)
		       (set! port.buffer.index buffer.past)
		       (when (unsafe.fx> buffer.past port.buffer.used-size)
			 (set! port.buffer.used-size buffer.past)))
		   (begin
		     (%unsafe.flush-output-port port who)
		     (retry-after-flushing-buffer))))))))))

;;; --------------------------------------------------------------------
;;; PUT-CHAR for port with bytevector buffer and Latin-1 transcoder

(define (%unsafe.put-char-to-port-with-fast-latin1-tag port ch code-point who)
  ;;Write to  PORT the character  CODE-POINT encoded by  Latin-1.  Honor
  ;;the  error  handling in  the  PORT's  transcoder,  selecting #\?  as
  ;;replacement character.
  ;;
  (define (%doit port ch code-point who)
    (with-port-having-bytevector-buffer (port)
      (let retry-after-flushing-buffer ()
	(let* ((buffer.offset	port.buffer.index)
	       (buffer.past	(unsafe.fxadd1 buffer.offset)))
	  (if (unsafe.fx<= buffer.past port.buffer.size)
	      (begin
		(unsafe.bytevector-u8-set! port.buffer buffer.offset code-point)
		(set! port.buffer.index buffer.past)
		(when (unsafe.fx> buffer.past port.buffer.used-size)
		  (set! port.buffer.used-size buffer.past)))
	    (begin
              (%unsafe.flush-output-port port who)
	      (retry-after-flushing-buffer)))))))
  (if (latin-1-code-point? code-point)
      (%doit port ch code-point who)
    (case (transcoder-error-handling-mode (port-transcoder port))
      ((ignore)
       (values))
      ((replace)
       (%doit port ch (char->integer #\?) who))
      ((raise)
       (raise
	(condition (make-i/o-encoding-error port ch)
		   (make-who-condition who)
		   (make-message-condition "character cannot be encoded by Latin-1"))))
      (else
       (assertion-violation who "vicare internal error: invalid error handling mode" port)))))


;;;; string output

(define put-string
  ;;Defined  by  R6RS.  START  and  COUNT  must  be non--negative  exact
  ;;integer objects.  STR must have a length of at least START+COUNT.
  ;;
  ;;START defaults to 0.  COUNT defaults to:
  ;;
  ;;   (- (string-length STR) START)
  ;;
  ;;The PUT-STRING procedure writes the COUNT characters of STR starting
  ;;at index START to the textual output PORT.  The PUT-STRING procedure
  ;;returns unspecified values.
  ;;
  (case-lambda
   ((port str)
    (define who 'put-string)
    (with-arguments-validation (who)
	((port   port)
	 (string str))
      (%unsafe.put-string port str 0 (unsafe.string-length str) who)))
   ((port str start)
    (define who 'put-string)
    (with-arguments-validation (who)
	((port		port)
	 (string	str)
	 (fixnum-start-index            start)
	 (unsafe.start-index-for-string start str))
      (%unsafe.put-string port str start (unsafe.fx- (unsafe.string-length str) start) who)))
   ((port str start count)
    (define who 'put-string)
    (with-arguments-validation (who)
	((port		port)
	 (string	str)
	 (fixnum-start-index             start)
	 (unsafe.start-index-for-string  start str)
	 (fixnum-count                       count)
	 (unsafe.count-from-start-in-string  count start str))
      (%unsafe.put-string port str start count who)))))

(define (%unsafe.put-string port src.str src.start count who)
  (define-inline (%put-it ?buffer-mode-line ?eol-bits ?put-char)
    (let next-char ((src.index src.start)
		    (src.past  (unsafe.fx+ src.start count)))
      (unless (unsafe.fx= src.index src.past)
	(let* ((ch		(unsafe.string-ref src.str src.index))
	       (code-point	(unsafe.char->fixnum ch))
	       (newline?	(unsafe.fx= code-point LINEFEED-CODE-POINT))
	       (flush?		(and ?buffer-mode-line newline?)))
	  (if newline?
	      (%case-eol-style (?eol-bits who)
		((EOL-LINEFEED-TAG)
		 (?put-char port ch code-point who))
		((EOL-CARRIAGE-RETURN-TAG)
		 (?put-char port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who))
		((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		 (?put-char port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		 (?put-char port LINEFEED-CHAR        LINEFEED-CODE-POINT        who))
		((EOL-NEXT-LINE-TAG)
		 (?put-char port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who))
		((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		 (?put-char port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		 (?put-char port NEXT-LINE-CHAR       NEXT-LINE-CODE-POINT       who))
		((EOL-LINE-SEPARATOR-TAG)
		 (?put-char port LINE-SEPARATOR-CHAR LINE-SEPARATOR-CODE-POINT who))
		(else
		 (?put-char port ch code-point who)))
	    (?put-char port ch code-point who))
	  (when flush?
	    (%unsafe.flush-output-port port)))
	(next-char (unsafe.fxadd1 src.index) src.past))))
  (define-inline (%put-utf16le ?port ?ch ?code-point ?who)
    (%unsafe.put-char-to-port-with-fast-utf16xe-tag ?port ?ch ?code-point ?who 'little))
  (define-inline (%put-utf16be ?port ?ch ?code-point ?who)
    (%unsafe.put-char-to-port-with-fast-utf16xe-tag ?port ?ch ?code-point ?who 'big))
  (let ((buffer-mode-line? (%unsafe.port-buffer-mode-line? port))
	(eol-bits          (%unsafe.port-eol-style-bits port)))
    (%case-textual-output-port-fast-tag (port who)
      ((FAST-PUT-UTF8-TAG)
       (%put-it buffer-mode-line? eol-bits %unsafe.put-char-to-port-with-fast-utf8-tag))
      ((FAST-PUT-CHAR-TAG)
       (%put-it buffer-mode-line? eol-bits %unsafe.put-char-to-port-with-fast-char-tag))
      ((FAST-PUT-LATIN-TAG)
       (%put-it buffer-mode-line? eol-bits %unsafe.put-char-to-port-with-fast-latin1-tag))
      ((FAST-PUT-UTF16LE-TAG)
       (%put-it buffer-mode-line? eol-bits %put-utf16le))
      ((FAST-PUT-UTF16BE-TAG)
       (%put-it buffer-mode-line? eol-bits %put-utf16be))))
  (when (%unsafe.port-buffer-mode-none? port)
    (%unsafe.flush-output-port port who)))


(define newline
  ;;Defined by  R6RS.  This is  equivalent to using WRITE-CHAR  to write
  ;;#\linefeed to the textual output PORT.
  ;;
  ;;If  PORT  is   omitted,  it  defaults  to  the   value  returned  by
  ;;CURRENT-OUTPUT-PORT.
  ;;
  ;;NOTE The original  Ikarus code flushed the buffer  after writing the
  ;;newline, I have removed this  because flushing must be controlled by
  ;;the buffering mode (Marco Maggi; Oct 3, 2011).
  ;;
  (case-lambda
   (()
    (%do-put-char (current-output-port) #\newline 'newline))
   ((port)
    (%do-put-char port #\newline 'newline))))


;;;; platform I/O error handling

(define (%raise-eagain-error who port)
  ;;Raise an exception to signal  that a system call was interrupted and
  ;;returned the EAGAIN errno code.
  ;;
  (raise
   (condition (make-i/o-eagain)
	      (make-who-condition who)
	      (make-message-condition (strerror EAGAIN))
	      (if port
		  (make-i/o-port-error port)
		(condition)))))

(define %raise-io-error
  ;;Raise a non-continuable  exception describing an input/output
  ;;system error from the value of ERRNO.
  ;;
  (case-lambda
   ((who port-identifier errno base-condition)
    (raise
     (condition base-condition
		(make-who-condition who)
		(make-message-condition (strerror errno))
		(case-errno errno
		  ((EACCES EFAULT)
		   ;;Why   is  EFAULT   included  here?    Because  many
		   ;;functions   may   return   EFAULT   even   if   the
		   ;;documentation in the GNU C Library does not mention
		   ;;it explicitly;  see the notes  in the documentation
		   ;;of the "errno" variable.
		   (make-i/o-file-protection-error port-identifier))
		  ((EROFS)
		   (make-i/o-file-is-read-only-error port-identifier))
		  ((EEXIST)
		   (make-i/o-file-already-exists-error port-identifier))
		  ((EIO)
		   (make-i/o-error))
		  ((ENOENT)
		   (make-i/o-file-does-not-exist-error port-identifier))
		  (else
		   (if port-identifier
		       (make-irritants-condition (list port-identifier))
		     (condition))))
		)))
   ((who port-identifier errno)
    (%raise-io-error who port-identifier errno (make-error)))))


;;;; helper functions for platform's descriptors

(define string->filename-func
  (make-parameter string->utf8
    (lambda (obj)
      (define who 'string->filename-func)
      (with-arguments-validation (who)
	  ((procedure obj))
	obj))))

(define filename->string-func
  (make-parameter utf8->string
    (lambda (obj)
      (define who 'filename->string-func)
      (with-arguments-validation (who)
	  ((procedure obj))
	obj))))

(define (%open-input-file-descriptor filename file-options who)
  ;;Subroutine for the  functions below opening a file  for input.  Open
  ;;and return  a file descriptor  referencing the file selected  by the
  ;;string FILENAME.  If an error occurs: raise an exception.
  ;;
  ;;R6RS states that the NO-CREATE, NO-FAIL and NO-TRUNCATE file options
  ;;have  no effect  when opening  a file  only for  input.   At present
  ;;FILE-OPTIONS is ignored, no flags are supported.
  ;;
  (let* ((opts 0)
	 (fd   (capi.platform-open-input-fd ((string->filename-func) filename) opts)))
    (if (fx< fd 0)
	(%raise-io-error who filename fd)
      fd)))

(define (%open-output-file-descriptor filename file-options who)
  ;;Subroutine for the functions below  opening a file for output.  Open
  ;;and return  a file descriptor  referencing the file selected  by the
  ;;string FILENAME.  If an error occurs: raise an exception.
  ;;
  (let* ((opts (if (enum-set? file-options)
		   (unsafe.fxior (if (enum-set-member? 'no-create   file-options) #b001 0)
				  (if (enum-set-member? 'no-fail     file-options) #b010 0)
				  (if (enum-set-member? 'no-truncate file-options) #b100 0))
		 (assertion-violation who "file-options is not an enum set" file-options)))
	 (fd (capi.platform-open-output-fd ((string->filename-func) filename) opts)))
    (if (fx< fd 0)
	(%raise-io-error who filename fd)
      fd)))

(define (%open-input/output-file-descriptor filename file-options who)
  ;;Subroutine  for the  functions below  opening a  file for  input and
  ;;output.   Open and  return a  file descriptor  referencing  the file
  ;;selected  by the  string FILENAME.   If  an error  occurs: raise  an
  ;;exception.
  ;;
  (let* ((opts (if (enum-set? file-options)
		   ;;In  future  the  options   for  I/O  ports  may  be
		   ;;different from the ones of output-only ports.
		   (unsafe.fxior (if (enum-set-member? 'no-create   file-options) 1 0)
				  (if (enum-set-member? 'no-fail     file-options) 2 0)
				  (if (enum-set-member? 'no-truncate file-options) 4 0))
		 (assertion-violation who "file-options is not an enum set" file-options)))
	 (fd (capi.platform-open-input/output-fd ((string->filename-func) filename) opts)))
    (if (fx< fd 0)
	(%raise-io-error who filename fd)
      fd)))

(define (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				      maybe-transcoder close-function who)
  ;;Given the  fixnum file descriptor  FD representing an open  file for
  ;;the underlying platform: build and  return a Scheme input port to be
  ;;used to read the data.
  ;;
  ;;The returned  port supports both the  GET-POSITION and SET-POSITION!
  ;;operations.
  ;;
  ;;If CLOSE-FUNCTION is a function: it is used as close function; if it
  ;;is true:  a standard  close function for  file descriptors  is used;
  ;;else the port does not support the close function.
  ;;
  (define set-position!
    (%make-set-position!-function-for-file-descriptor-port fd port-identifier))

  (define close
    (cond ((procedure? close-function)
	   close-function)
	  ((and (boolean? close-function) close-function)
	   (%make-close-function-for-platform-descriptor-port port-identifier fd))
	  (else #f)))

  (define (read! dst.bv dst.start requested-count)
    (let ((count (capi.platform-read-fd fd dst.bv dst.start requested-count)))
      (cond ((unsafe.fx>= count 0)
	     count)
	    ((unsafe.fx= count EAGAIN)
	     (%raise-eagain-error who #f))
	    (else
	     (%raise-io-error 'read! port-identifier count (make-i/o-read-error))))))

  (let ((attributes		(%select-input-fast-tag-from-transcoder
				 who maybe-transcoder
				 other-attributes GUARDED-PORT-TAG PORT-WITH-FD-DEVICE
				 (%select-eol-style-from-transcoder who maybe-transcoder)
				 DEFAULT-OTHER-ATTRS))
	(buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector buffer.size))
	(write!			#f)
	(get-position		#t)
	(cookie			(default-cookie fd)))
    (%port->maybe-guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer
		 maybe-transcoder port-identifier
		 read! write! get-position set-position! close cookie))))

(define (%file-descriptor->output-port fd other-attributes port-identifier buffer.size
				       transcoder close-function who)
  ;;Given the  fixnum file descriptor  FD representing an open  file for
  ;;the underlying platform: build and return a Scheme output port to be
  ;;used to write the data.
  ;;
  ;;The returned  port supports both the  GET-POSITION and SET-POSITION!
  ;;operations.
  ;;
  ;;If CLOSE-FUNCTION is a function: it is used as close function; if it
  ;;is true:  a standard  close function for  file descriptors  is used;
  ;;else the port does not support the close operation.
  ;;
  (define set-position!
    (%make-set-position!-function-for-file-descriptor-port fd port-identifier))

  (define close
    (cond ((procedure? close-function)
	   close-function)
	  ((and (boolean? close-function) close-function)
	   (%make-close-function-for-platform-descriptor-port port-identifier fd))
	  (else #f)))

  (define (write! src.bv src.start requested-count)
    (let ((count (capi.platform-write-fd fd src.bv src.start requested-count)))
      (cond ((unsafe.fx>= count 0)
	     count)
	    ((unsafe.fx= count EAGAIN)
	     (%raise-eagain-error who #f))
	    (else
	     (%raise-io-error 'write! port-identifier count (make-i/o-write-error))))))

  (let ((attributes		(%select-output-fast-tag-from-transcoder
				 who transcoder
				 other-attributes GUARDED-PORT-TAG PORT-WITH-FD-DEVICE
				 (%select-eol-style-from-transcoder who transcoder)
				 DEFAULT-OTHER-ATTRS))
	(buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector buffer.size))
	(read!			#f)
	(get-position		#t)
	(cookie			(default-cookie fd)))
    (%port->maybe-guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer transcoder port-identifier
		 read! write! get-position set-position! close cookie))))

(define (%file-descriptor->input/output-port fd other-attributes port-identifier buffer.size
					     transcoder close-function who)
  ;;Given the  fixnum file descriptor  FD representing an open  file for
  ;;the underlying platform: build and return a Scheme input/output port
  ;;to be used to read and write the data.
  ;;
  ;;The returned  port supports both the  GET-POSITION and SET-POSITION!
  ;;operations.
  ;;
  ;;If CLOSE-FUNCTION is a function: it is used as close function; if it
  ;;is true:  a standard  close function for  file descriptors  is used;
  ;;else the port does not support the close operation.
  ;;
  (define set-position!
    (%make-set-position!-function-for-file-descriptor-port fd port-identifier))

  (define close
    (cond ((procedure? close-function)
	   close-function)
	  ((and (boolean? close-function) close-function)
	   (%make-close-function-for-platform-descriptor-port port-identifier fd))
	  (else #f)))

  (define (read! dst.bv dst.start requested-count)
    (let ((count (capi.platform-read-fd fd dst.bv dst.start requested-count)))
      (cond ((unsafe.fx>= count 0)
	     count)
	    ((unsafe.fx= count EAGAIN)
	     (%raise-eagain-error who #f))
	    (else
	     (%raise-io-error 'read! port-identifier count (make-i/o-read-error))))))

  (define (write! src.bv src.start requested-count)
    (let ((count (capi.platform-write-fd fd src.bv src.start requested-count)))
      (cond ((unsafe.fx>= count 0)
	     count)
	    ((unsafe.fx= count EAGAIN)
	     (%raise-eagain-error who #f))
	    (else
	     (%raise-io-error 'write! port-identifier count (make-i/o-write-error))))))

  (let ((attributes		(%select-input/output-fast-tag-from-transcoder
				 who transcoder other-attributes
				 INPUT/OUTPUT-PORT-TAG GUARDED-PORT-TAG PORT-WITH-FD-DEVICE
				 (%select-eol-style-from-transcoder who transcoder)
				 DEFAULT-OTHER-ATTRS))
	(buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector buffer.size))
	(get-position		#t)
	(cookie			(default-cookie fd)))
    (%port->maybe-guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer transcoder port-identifier
		 read! write! get-position set-position! close cookie))))

(define (%socket->input/output-port sock other-attributes port-identifier buffer.size
				    transcoder close-function who)
  ;;Given  the  fixnum socket  descriptor  SOCK  representing a  network
  ;;socket  for  the underlying  platform:  build  and  return a  Scheme
  ;;input/output port to be used to read and write the data.
  ;;
  ;;The   returned  port   does   not  support   the  GET-POSITION   and
  ;;SET-POSITION!  operations.
  ;;
  ;;If CLOSE-FUNCTION is a function: it is used as close function; if it
  ;;is true:  a standard  close function for  file descriptors  is used;
  ;;else the port does not support the close operation.
  ;;
  (define close
    (cond ((procedure? close-function)
	   close-function)
	  ((and (boolean? close-function) close-function)
	   (%make-close-function-for-platform-descriptor-port port-identifier sock))
	  (else #f)))

  (define (read! dst.bv dst.start requested-count)
    (let ((count (capi.platform-read-fd sock dst.bv dst.start requested-count)))
      (cond ((unsafe.fx>= count 0)
	     count)
	    ((unsafe.fx= count EAGAIN)
	     (%raise-eagain-error who #f))
	    (else
	     (%raise-io-error 'read! port-identifier count (make-i/o-read-error))))))

  (define (write! src.bv src.start requested-count)
    (let ((rv (capi.platform-write-fd sock src.bv src.start requested-count)))
      (cond ((unsafe.fx>= rv 0)
	     rv)
	    ((unsafe.fx= rv EAGAIN)
	     (%raise-eagain-error who #f))
	    (else
	     (%raise-io-error 'write! port-identifier rv (make-i/o-write-error))))))

  (let ((attributes		(%select-input/output-fast-tag-from-transcoder
				 who transcoder other-attributes
				 INPUT/OUTPUT-PORT-TAG GUARDED-PORT-TAG PORT-WITH-FD-DEVICE
				 (%select-eol-style-from-transcoder who transcoder)
				 DEFAULT-OTHER-ATTRS))
	(buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector buffer.size))
	(get-position		#t)
	(cookie			(default-cookie sock)))
    (%port->maybe-guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer transcoder port-identifier
		 read! write! #f #f close cookie))))

(define (%make-set-position!-function-for-file-descriptor-port fd port-identifier)
  ;;Build and return a closure to be used as SET-POSITION!  function for
  ;;a port wrapping the platform's file descriptor FD.
  ;;
  (lambda (position)
    (let ((errno (capi.platform-set-position fd position)))
      (when errno
	(%raise-io-error 'set-position! port-identifier errno
			 (make-i/o-invalid-position-error position))))))

(define (%make-close-function-for-platform-descriptor-port port-identifier fd)
  ;;Return a standard CLOSE function  for a port wrapping the platform's
  ;;descriptor  FD.  It  is used  for both  file descriptors  and socket
  ;;descriptors, and in general can be used for any platform descriptor.
  ;;
  (lambda ()
    (let ((errno (capi.platform-close-fd fd)))
      (when errno
	(%raise-io-error 'close port-identifier errno)))))

(define (%buffer-mode->attributes buffer-mode who)
  (case buffer-mode
    ((block)	0)
    ((line)	BUFFER-MODE-LINE-TAG)
    ((none)	BUFFER-MODE-NONE-TAG)
    (else
     (assertion-violation who "invalid buffer-mode argument" buffer-mode))))

(define (port-fd port)
  ;;Defined by  Vicare.  If  PORT is  a port with  a file  descriptor as
  ;;device: return a fixnum representing the device, else return false.
  ;;
  (define who 'port-fd)
  (with-arguments-validation (who)
      ((port	port))
    (with-port (port)
      (and port.fd-device?
	   port.device))))


;;;; input ports wrapping platform file descriptors

(define open-file-input-port
  ;;Defined  by  R6RS.  The  OPEN-FILE-INPUT-PORT  procedure returns  an
  ;;input   port   for   the   named   file.    The   FILE-OPTIONS   and
  ;;MAYBE-TRANSCODER arguments are optional.
  ;;
  ;;The FILE-OPTIONS  argument, which  may determine various  aspects of
  ;;the returned port, defaults to the value of:
  ;;
  ;;   (file-options)
  ;;
  ;;MAYBE-TRANSCODER must be either a transcoder or false.
  ;;
  ;;The BUFFER-MODE  argument, if supplied,  must be one of  the symbols
  ;;that  name a  buffer  mode.  The  BUFFER-MODE  argument defaults  to
  ;;BLOCK.
  ;;
  ;;If  MAYBE-TRANSCODER  is a  transcoder,  it  becomes the  transcoder
  ;;associated with the returned port.
  ;;
  ;;If MAYBE-TRANSCODER  is false or absent,  the port will  be a binary
  ;;port  and  will  support  the PORT-POSITION  and  SET-PORT-POSITION!
  ;;operations.  Otherwise the port will  be a textual port, and whether
  ;;it supports the  PORT-POSITION and SET-PORT-POSITION!  operations is
  ;;implementation dependent (and possibly transcoder dependent).
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
    (with-arguments-validation (who)
	((filename		filename)
	 (file-options		file-options)
	 (maybe-transcoder	maybe-transcoder))
      (let* ((buffer-mode-attrs	(%buffer-mode->attributes buffer-mode who))
	     (other-attributes	buffer-mode-attrs)
	     (fd		(%open-input-file-descriptor filename file-options who))
	     (port-identifier	filename)
	     (buffer-size	(input-file-buffer-size))
	     (close-function	#t))
	(%file-descriptor->input-port fd other-attributes port-identifier buffer-size
				      maybe-transcoder close-function who))))))

(define (%open-input-file-with-defaults filename who)
  ;;Open FILENAME  for input, with  empty file options, and  returns the
  ;;obtained port.
  ;;
  (with-arguments-validation (who)
      ((filename filename))
    (let ((fd			(%open-input-file-descriptor filename (file-options) who))
	  (other-attributes	0)
	  (port-id		filename)
	  (buffer.size		(input-file-buffer-size))
	  (transcoder		(native-transcoder))
	  (close-function	#t))
      (%file-descriptor->input-port fd other-attributes port-id buffer.size
				    transcoder close-function who))))

(define (open-input-file filename)
  ;;Defined by  R6RS.  Open FILENAME  for input, with  empty file
  ;;options, and returns the obtained port.
  ;;
  (define who 'open-input-file)
  (%open-input-file-with-defaults filename who))

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
  (with-arguments-validation (who)
      ((filename   filename)
       (procedure  thunk))
    (call-with-port
	(%open-input-file-with-defaults filename who)
      (lambda (port)
	(parameterize ((current-input-port port))
	  (thunk))))))

(define (call-with-input-file filename proc)
  ;;Defined by R6RS.  PROC should accept one argument.
  ;;
  ;;This procedure opens  the file named by FILENAME  for input, with no
  ;;specified file options, and calls  PROC with the obtained port as an
  ;;argument.
  ;;
  ;;If PROC  returns, the  port is closed  automatically and  the values
  ;;returned by PROC are returned.
  ;;
  ;;If  PROC does  not return,  the  port is  not closed  automatically,
  ;;unless it  is possible to  prove that the  port will never  again be
  ;;used for an I/O operation.
  ;;
  (define who 'call-with-input-file)
  (with-arguments-validation (who)
      ((filename   filename)
       (procedure  proc))
    (call-with-port (%open-input-file-with-defaults filename who) proc)))


;;;; output ports wrapping platform file descriptors

(define open-file-output-port
  ;;Defined  by R6RS.   The OPEN-FILE-OUTPUT-PORT  procedure  returns an
  ;;output port for the named file.
  ;;
  ;;The FILE-OPTIONS  argument, which  may determine various  aspects of
  ;;the returned port, defaults to the value of:
  ;;
  ;;   (file-options)
  ;;
  ;;MAYBE-TRANSCODER must be either a transcoder or false.
  ;;
  ;;The BUFFER-MODE  argument, if supplied,  must be one of  the symbols
  ;;that  name a  buffer  mode.  The  BUFFER-MODE  argument defaults  to
  ;;BLOCK.
  ;;
  ;;If  MAYBE-TRANSCODER  is a  transcoder,  it  becomes the  transcoder
  ;;associated with the port.
  ;;
  ;;If MAYBE-TRANSCODER  is false or absent,  the port will  be a binary
  ;;port  and  will  support  the PORT-POSITION  and  SET-PORT-POSITION!
  ;;operations.  Otherwise the port will  be a textual port, and whether
  ;;it supports the  PORT-POSITION and SET-PORT-POSITION!  operations is
  ;;implementation-dependent (and possibly transcoder-dependent).
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
    (with-arguments-validation (who)
	((filename		filename)
	 (file-options		file-options)
	 (maybe-transcoder	maybe-transcoder))
      (let* ((buffer-mode-attrs	(%buffer-mode->attributes buffer-mode who))
	     (other-attributes	buffer-mode-attrs)
	     (fd		(%open-output-file-descriptor filename file-options who))
	     (port-identifier	filename)
	     (buffer-size	(output-file-buffer-size)))
	(%file-descriptor->output-port fd other-attributes port-identifier
				       buffer-size maybe-transcoder #t who))))))

(define (%open-output-file-with-defaults filename who)
  ;;Open FILENAME  for output, with  empty file options, and  return the
  ;;obtained port.
  ;;
  (with-arguments-validation (who)
      ((filename filename))
    (let ((fd			(%open-output-file-descriptor filename (file-options) who))
	  (other-attributes	0)
	  (port-id		filename)
	  (buffer.size		(output-file-buffer-size))
	  (transcoder		(native-transcoder))
	  (close-function	#t))
      (%file-descriptor->output-port fd other-attributes port-id buffer.size
				     transcoder close-function who))))

(define (open-output-file filename)
  ;;Defined by R6RS.  Open FILENAME for output, with empty file options,
  ;;and return the obtained port.
  ;;
  (define who 'open-output-file)
  (%open-output-file-with-defaults filename who))

(define (with-output-to-file filename thunk)
  ;;Defined by  R6RS.  THUNK  must be a  procedure and must  accept zero
  ;;arguments.  The file is opened  for output using empty file options,
  ;;and THUNK is called with no arguments.
  ;;
  ;;During the dynamic extent of the call to THUNK, the obtained port is
  ;;made the  value returned  by the procedure  CURRENT-OUTPUT-PORT; the
  ;;previous  default value  is reinstated  when the  dynamic  extent is
  ;;exited.
  ;;
  ;;When THUNK  returns, the port  is closed automatically.   The values
  ;;returned by THUNK are returned.
  ;;
  ;;If an escape procedure is used to escape back into the call to THUNK
  ;;after THUNK is returned, the behavior is unspecified.
  ;;
  (define who 'with-output-to-file)
  (with-arguments-validation (who)
      ((procedure thunk))
    (call-with-port (%open-output-file-with-defaults filename who)
      (lambda (port)
	(parameterize ((current-output-port port))
	  (thunk))))))

(define (call-with-output-file filename proc)
  ;;Defined by R6RS.  PROC should accept one argument.
  ;;
  ;;This procedure opens the file  named by FILENAME for output, with no
  ;;specified file options, and calls  PROC with the obtained port as an
  ;;argument.
  ;;
  ;;If PROC  returns, the  port is closed  automatically and  the values
  ;;returned by PROC are returned.
  ;;
  ;;If  PROC does  not return,  the  port is  not closed  automatically,
  ;;unless it  is possible to  prove that the  port will never  again be
  ;;used for an I/O operation.
  ;;
  (define who 'call-with-output-file)
  (with-arguments-validation (who)
      ((procedure proc))
    (call-with-port (%open-output-file-with-defaults filename who) proc)))


;;;; input/output ports wrapping platform file descriptors

(define open-file-input/output-port
  ;;Defined by  R6RS.  Return a single  port that is both  an input port
  ;;and  an output  port for  the  named file.   The optional  arguments
  ;;default as described  in the specification of OPEN-FILE-OUTPUT-PORT.
  ;;If   the    input/output   port   supports    PORT-POSITION   and/or
  ;;SET-PORT-POSITION!, the  same port position  is used for  both input
  ;;and output.
  ;;
  (case-lambda
   ((filename)
    (open-file-input/output-port filename (file-options) 'block #f))

   ((filename file-options)
    (open-file-input/output-port filename file-options 'block #f))

   ((filename file-options buffer-mode)
    (open-file-input/output-port filename file-options buffer-mode #f))

   ((filename file-options buffer-mode maybe-transcoder)
    (define who 'open-file-input/output-port)
    (with-arguments-validation (who)
	((filename		filename)
	 (file-options		file-options)
	 (maybe-transcoder	maybe-transcoder))
      (let* ((buffer-mode-attrs	(%buffer-mode->attributes buffer-mode who))
	     (other-attributes	(unsafe.fxior INPUT/OUTPUT-PORT-TAG buffer-mode-attrs))
	     (fd		(%open-input/output-file-descriptor filename file-options who))
	     (port-identifier	filename)
	     (buffer-size	(input/output-file-buffer-size)))
	(%file-descriptor->input/output-port fd other-attributes port-identifier
					     buffer-size maybe-transcoder #t who))))))


;;;; platform file descriptor ports

(define (make-binary-file-descriptor-input-port fd port-identifier)
  (define who 'make-binary-file-descriptor-input-port)
  (let ((other-attributes	0)
	(buffer.size		(input-file-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				  transcoder close-function who)))

(define (make-binary-file-descriptor-input-port* fd port-identifier)
  (define who 'make-binary-file-descriptor-input-port*)
  (let ((other-attributes	0)
	(buffer.size		(input-file-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				  transcoder close-function who)))

;;; --------------------------------------------------------------------

(define (make-textual-file-descriptor-input-port fd port-identifier transcoder)
  (define who 'make-textual-file-descriptor-input-port)
  (let ((other-attributes	0)
	(buffer.size		(input-file-buffer-size))
	(close-function		#t))
    (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				  transcoder close-function who)))

(define (make-textual-file-descriptor-input-port* fd port-identifier transcoder)
  (define who 'make-textual-file-descriptor-input-port*)
  (let ((other-attributes	0)
	(buffer.size		(input-file-buffer-size))
	(close-function		#f))
    (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				  transcoder close-function who)))

;;; --------------------------------------------------------------------

(define (make-binary-file-descriptor-output-port fd port-identifier)
  (define who 'make-binary-file-descriptor-output-port)
  (let ((other-attributes	0)
	(buffer.size		(output-file-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%file-descriptor->output-port fd other-attributes port-identifier buffer.size
				   transcoder close-function who)))

(define (make-binary-file-descriptor-output-port* fd port-identifier)
  (define who 'make-binary-file-descriptor-output-port*)
  (let ((other-attributes	0)
	(buffer.size		(output-file-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%file-descriptor->output-port fd other-attributes port-identifier buffer.size
				   transcoder close-function who)))

;;; --------------------------------------------------------------------

(define (make-textual-file-descriptor-output-port fd port-identifier transcoder)
  (define who 'make-textual-file-descriptor-output-port)
  (let ((other-attributes	0)
	(buffer.size		(output-file-buffer-size))
	(close-function		#t))
    (%file-descriptor->output-port fd other-attributes port-identifier buffer.size
				   transcoder close-function who)))

(define (make-textual-file-descriptor-output-port* fd port-identifier transcoder)
  (define who 'make-textual-file-descriptor-output-port*)
  (let ((other-attributes	0)
	(buffer.size		(output-file-buffer-size))
	(close-function		#f))
    (%file-descriptor->output-port fd other-attributes port-identifier buffer.size
				   transcoder close-function who)))

;;; --------------------------------------------------------------------

(define (make-binary-file-descriptor-input/output-port fd port-identifier)
  (define who 'make-binary-file-descriptor-input/output-port)
  (let ((other-attributes	0)
	(buffer.size		(input/output-file-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%file-descriptor->input/output-port fd other-attributes port-identifier buffer.size
					 transcoder close-function who)))

(define (make-binary-file-descriptor-input/output-port* fd port-identifier)
  (define who 'make-binary-file-descriptor-input/output-port*)
  (let ((other-attributes	0)
	(buffer.size		(input/output-file-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%file-descriptor->input/output-port fd other-attributes port-identifier buffer.size
					 transcoder close-function who)))

;;; --------------------------------------------------------------------

(define (make-textual-file-descriptor-input/output-port fd port-identifier transcoder)
  (define who 'make-textual-file-descriptor-input/output-port)
  (let ((other-attributes	0)
	(buffer.size		(input/output-file-buffer-size))
	(close-function		#t))
    (%file-descriptor->input/output-port fd other-attributes port-identifier buffer.size
					 transcoder close-function who)))

(define (make-textual-file-descriptor-input/output-port* fd port-identifier transcoder)
  (define who 'make-textual-file-descriptor-input/output-port*)
  (let ((other-attributes	0)
	(buffer.size		(input/output-file-buffer-size))
	(close-function		#f))
    (%file-descriptor->input/output-port fd other-attributes port-identifier buffer.size
					 transcoder close-function who)))


;;;; platform socket descriptor ports

(define (make-binary-socket-input/output-port sock port-identifier)
  (define who 'make-binary-socket-input/output-port)
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%socket->input/output-port sock other-attributes port-identifier buffer.size
				transcoder close-function who)))

(define (make-binary-socket-input/output-port* sock port-identifier)
  (define who 'make-binary-socket-input/output-port)
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%socket->input/output-port sock other-attributes port-identifier buffer.size
				transcoder close-function who)))

;;; --------------------------------------------------------------------

(define (make-textual-socket-input/output-port sock port-identifier transcoder)
  (define who 'make-textual-socket-input/output-port)
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(close-function		#t))
    (%socket->input/output-port sock other-attributes port-identifier buffer.size
				transcoder close-function who)))

(define (make-textual-socket-input/output-port* sock port-identifier transcoder)
  (define who 'make-textual-socket-input/output-port)
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(close-function		#f))
    (%socket->input/output-port sock other-attributes port-identifier buffer.size
				transcoder close-function who)))


(define (reset-input-port! port)
  (define who 'reset-input-port!)
  (with-arguments-validation (who)
      ((input-port port))
    (with-port (port)
      (set! port.buffer.index port.buffer.used-size))))

(define (reset-output-port! port)
  (define who 'reset-output-port!)
  (with-arguments-validation (who)
      ((output-port port))
    (with-port (port)
      (set! port.buffer.index 0))))


;;;; standard, console and current ports

(define (standard-input-port)
  ;;Defined by R6RS.  Return a new binary input port connected to
  ;;standard input.  Whether  the port supports the PORT-POSITION
  ;;and   SET-PORT-POSITION!     operations   is   implementation
  ;;dependent.
  ;;
  (let ((who		'standard-input-port)
	(fd		0)
	(attributes	0)
	(port-id	"*stdin*")
	(buffer.size	(input-file-buffer-size))
	(transcoder	#f)
	(close		#f))
    (%file-descriptor->input-port fd attributes port-id buffer.size transcoder close who)))

(define (standard-output-port)
  ;;Defined by  R6RS.  Return a new binary  output port connected
  ;;to  the  standard  output.   Whether the  port  supports  the
  ;;PORT-POSITION    and   SET-PORT-POSITION!     operations   is
  ;;implementation dependent.
  ;;
  (%file-descriptor->output-port 1 0
				 "*stdout*" (output-file-buffer-size) #f #f 'standard-output-port))

(define (standard-error-port)
  ;;Defined by  R6RS.  Return a new binary  output port connected
  ;;to  the  standard  error.   Whether  the  port  supports  the
  ;;PORT-POSITION    and   SET-PORT-POSITION!     operations   is
  ;;implementation dependent.
  ;;
  (%file-descriptor->output-port 2 0
				 "*stderr*" (output-file-buffer-size) #f #f 'standard-error-port))

(define current-input-port
  ;;Defined by  R6RS.  Return a  default textual port  for input.
  ;;Normally,  this  default  port  is associated  with  standard
  ;;input,   but  can   be  dynamically   reassigned   using  the
  ;;WITH-INPUT-FROM-FILE procedure from  the (rnrs io simple (6))
  ;;library.   The  port  may  or  may  not  have  an  associated
  ;;transcoder;  if  it does,  the  transcoder is  implementation
  ;;dependent.
  (make-parameter
      (transcoded-port (standard-input-port) (native-transcoder))
    (lambda (x)
      (define who 'current-input-port)
      (with-arguments-validation (who)
	  ((input-port x)
	   (unsafe.textual-port x))
	x))))

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
      (transcoded-port (standard-output-port) (native-transcoder))
    (lambda (x)
      (define who 'current-output-port)
      (with-arguments-validation (who)
	  ((output-port         x)
	   (unsafe.textual-port x))
	x))))

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
      (let ((port (transcoded-port (standard-error-port) (native-transcoder))))
	(set-port-buffer-mode! port (buffer-mode line))
	port)
    (lambda (x)
      (define who 'current-error-port)
      (with-arguments-validation (who)
	  ((output-port         x)
	   (unsafe.textual-port x))
	x))))

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

(post-gc-hooks (cons %close-garbage-collected-ports (post-gc-hooks)))

)

;;; end of file
;;; Local Variables:
;;; coding: utf-8-unix
;;; fill-column: 72
;;; eval: (put 'case-errno				'scheme-indent-function 1)
;;; eval: (put 'with-port				'scheme-indent-function 1)
;;; eval: (put 'with-port-having-bytevector-buffer	'scheme-indent-function 1)
;;; eval: (put 'with-port-having-string-buffer		'scheme-indent-function 1)
;;; eval: (put '%implementation-violation		'scheme-indent-function 1)
;;; eval: (put '%case-binary-input-port-fast-tag	'scheme-indent-function 1)
;;; eval: (put '%case-binary-output-port-fast-tag	'scheme-indent-function 1)
;;; eval: (put '%case-textual-input-port-fast-tag	'scheme-indent-function 1)
;;; eval: (put '%case-textual-output-port-fast-tag	'scheme-indent-function 1)
;;; eval: (put '%case-eol-style				'scheme-indent-function 1)
;;; eval: (put '%parse-byte-order-mark			'scheme-indent-function 1)
;;; eval: (put '%parse-bom-and-add-fast-tag		'scheme-indent-function 1)
;;; eval: (put '%flush-bytevector-buffer-and-evaluate	'scheme-indent-function 1)
;;; eval: (put '%flush-string-buffer-and-evaluate	'scheme-indent-function 1)
;;; eval: (put '%refill-bytevector-buffer-and-evaluate		'scheme-indent-function 1)
;;; eval: (put '%maybe-refill-bytevector-buffer-and-evaluate	'scheme-indent-function 1)
;;; eval: (put '%refill-string-buffer-and-evaluate		'scheme-indent-function 1)
;;; eval: (put '%maybe-refill-string-buffer-and-evaluate	'scheme-indent-function 1)
;;; End:
