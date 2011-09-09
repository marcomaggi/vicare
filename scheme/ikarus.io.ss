;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008,2011  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Abstract
;;;
;;;	The  primitive  operations  on  a  port  value  are  defined  in
;;;	"pass-specify-rep-primops.ss".  A port value  is just a block of
;;;	memory whose first word is tagged with the port tag.
;;;
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
;;It is defined in "pass-specify-rep-primops.ss"; its allocation,
;;accessors and mutators are  all defined as primitive operations
;;inlined by the compiler.
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
;;  Build  a  new  port  structure  and  return  a  Scheme  value
;;  referencing  it.  Notice  that the  underlying device  is not
;;  among the constructor  arguments: it is implicitly referenced
;;  by the functions READ, WRITE, GETP, SETP, CL.
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
;;  Zero-based  fixnum  offset of  the  current  position in  the
;;  buffer; see the description of the BUFFER field below.
;;
;;  For an output port: it is  the offset in the output buffer of
;;  the next location to be written to.
;;
;;  For an  input port: it is  the offset of the  input buffer of
;;  the next location to be read from.
;;
;;Field name: size
;;Field accessor: $port-size PORT
;;Field mutator: $set-port-size! PORT SIZE
;;  Fixnum representing the  number of bytes/chars currently used
;;  in the input/output buffer; see the description of the BUFFER
;;  field below.
;;
;;  When the device is a  Scheme string or bytevector: this field
;;  is  set to  the number  of characters  in the  string  or the
;;  number of bytes in the bytevector.
;;
;;Field name: buffer
;;Field accessor: $port-buffer PORT
;;  The  input/output  buffer  for   the  port.   The  buffer  is
;;  allocated  at port construction  time and  never reallocated.
;;
;;  For the  logic of the functions  to work: it  is mandatory to
;;  have a buffer at least  wide enough to hold the largest UTF-8
;;  character.   This  is  because   it  is  possible  to  put  a
;;  transcoder on top  of every binary port, and  we need a place
;;  to store partially read or written characters.
;;
;;  When the  port has a Scheme bytevector  as underlying device:
;;  the bytevector itself is the buffer.
;;
;;  When the port  has a Scheme string as  underlying device: the
;;  string itself is the buffer.
;;
;;  When the port  has a platform file as  underlying device: the
;;  buffer is a Scheme bytevector.   For input files: the size of
;;  the input buffer is the constant INPUT-FILE-BUFFER-SIZE.  For
;;  output files: the  size of the output buffer  is the constant
;;  OUTPUT-FILE-BUFFER-SIZE.
;;
;;  When doing  output on an underlying device  with buffer: data
;;  is first written in the buffer and once in a while flushed to
;;  the  device.   The  actual  offset  into an  output  port  is
;;  computed with:
;;
;;	   offset = pos + index
;;
;;                        pos
;;                        v                       port device
;;	   |--------------+---------------------------|
;;                        |*****+*******+--------| buffer
;;                        ^     ^       ^        ^
;;                        0   index  used-size  size
;;
;;  When doing input on an underlying device with buffer: a block
;;  of data is  first copied from the device  into the buffer and
;;  then read  to produce Scheme values.  The  actual offset into
;;  an input port is computed with:
;;
;;	   offset = pos - size + index
;;	          = pos - (size - index)
;;
;;                                      pos
;;                                      v          port device
;;	   |----------------------------+-------------|
;;                      |*******+*******+--------|
;;                      ^       ^       ^        ^
;;                      0     index  used size  size
;;
;;Field name: transcoder
;;
;;Field name: id
;;Field accessor: $port-id
;;  An  object  describing the  underlying  device.   For a  port
;;  associated to a file: it  is a Scheme string representing the
;;  file name given to functions like OPEN-OUTPUT-FILE.
;;
;;Field name: read!
;;Field accessor: $port-read! PORT
;;  Fill buffer procedure.
;;
;;  When the value  is a procedure: it must  be a function which,
;;  applied to the port itself,  fills the input buffer with data
;;  from the underlying device.
;;
;;  When the  value is the Scheme  symbol ALL-DATA-IN-BUFFER: the
;;  port  has no  underlying  device, the  buffer  itself is  the
;;  device.
;;
;;Field name: write!
;;Field accessor: $port-write! PORT
;;  Write butter procedure.
;;
;;Field name: get-position
;;Field accessor: $port-get-position PORT
;;  Get position procedure.
;;
;;Field name: set-position!
;;Field accessor: $port-set-position! PORT
;;  Set position procedure.
;;
;;Field name: close
;;Field accessor: $port-close PORT
;;  Close device procedure.
;;
;;Field name: cookie
;;Field accessor: $port-cookie PORT
;;Field mutator: $set-port-cookie PORT COOKIE
;;  A "cookie"  record used  to track line  and column  number in
;;  textual ports.
;;
;;

;;;; list of things to do
;;
;;*   Test  the  transcoders   with  OPEN-BYTEVECTOR-OUTPUT-PORT,
;;especially the SET-PORT-POSITION! function.
;;
;;*  FIXME If  SET-PORT-POSITION! fails  it is  possible  for the
;;field POS of the cookie to become invalid.  This situation must
;;be detected by all the functions, currently it is not.
;;
;;* Write documentation for the Ikarus-specific functions.
;;


(library (ikarus.io)
  (export
    ;; port parameters
    standard-input-port standard-output-port standard-error-port
    current-input-port  current-output-port  current-error-port
    console-output-port console-error-port   console-input-port
    bytevector-port-buffer-size string-port-buffer-size

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
    open-output-string get-output-string

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
		  bytevector-port-buffer-size string-port-buffer-size

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
		  open-output-string get-output-string

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
    (ikarus system $io)
    (prefix (only (ikarus) port?) primop.))


;;Replacing  this  module with  normal  imports  is  tricky, I  have  to
;;understand why it is here; maybe it has something to do with the order
;;in  which the  libraries  go  into the  boot  image without  violating
;;dependencies (Marco Maggi; Aug 28, 2011).
;;
(module UNSAFE
  (fx< fx<= fx> fx>= fx= fx+ fx-
       fxior fxand fxsra fxsll
       integer->char char->integer
       string-ref string-set! string-length
       bytevector-u8-ref bytevector-u8-set!
       bytevector-u16-ref)
  (import
      (rename (ikarus system $strings)
	      ($string-length string-length)
	      ($string-ref    string-ref)
	      ($string-set!   string-set!))
    (rename (ikarus system $chars)
	    ($char->fixnum char->integer)
	    ($fixnum->char integer->char))
    (rename (ikarus system $bytevectors)
	    ($bytevector-set!   bytevector-u8-set!)
	    ($bytevector-u8-ref bytevector-u8-ref))
    (rename (ikarus system $fx)
	    ($fxsra    fxsra)
	    ($fxsll    fxsll)
	    ($fxlogor  fxior)
	    ($fxlogand fxand)
	    ($fx+      fx+)
	    ($fx-      fx-)
	    ($fx<      fx<)
	    ($fx>      fx>)
	    ($fx>=     fx>=)
	    ($fx<=     fx<=)
	    ($fx=      fx=)))
  (define (bytevector-u16-ref x i endianness)
    (case endianness
      ((little)
       (fxlogor (bytevector-u8-ref x i)
		(fxsll (bytevector-u8-ref x (fx+ i 1)) 8)))
      (else
       (fxlogor (bytevector-u8-ref x (fx+ i 1))
		(fxsll (bytevector-u8-ref x i) 8))))))


;;;; constants

(define newline-integer			(char->integer #\newline))

(define input-block-size		(* 4 4096))
(define output-block-size		(* 4 4096))
(define input-file-buffer-size		(+ input-block-size 128))
(define output-file-buffer-size		output-block-size)

(define custom-binary-buffer-size	256)
(define custom-textual-buffer-size	256)

(define bytevector-port-buffer-size
  (make-parameter 4096
    (lambda (obj)
      (import (ikarus system $fx))
      (if (and (fixnum? obj) ($fx>= obj 128))
	  obj
	(error 'bytevector-port-buffer-size
	  "bytevector port buffer size should be a fixnum >= 128" obj)))))

(define string-port-buffer-size
  (make-parameter 256
    (lambda (obj)
      (import (ikarus system $fx))
      (if (and (fixnum? obj) ($fx>= obj 128))
	  obj
	(error 'string-port-buffer-size
	  "string port buffer size should be a fixnum >= 128" obj)))))


;;;; tags
;;
;;All the tags have 13 bits.
;;
;;The  12th and  13th  bits are  used  only by  CLOSED-PORT-TAG and  the
;;commented out R6RS-MODE-TAG, which are not associated to the type of a
;;port; for this reason if we want to extract only the type bits we have
;;to use a mask with 11 bits set to 1 as in FAST-ATTRS-MASK.
;;
;;                                 32109876543210
(define input-port-tag           #b00000000000001)
(define output-port-tag          #b00000000000010)
(define textual-port-tag         #b00000000000100)
(define binary-port-tag          #b00000000001000)
(define fast-char-text-tag       #b00000000010000)
(define fast-u7-text-tag         #b00000000100000)
(define fast-u8-text-tag         #b00000001100000)
(define fast-u16be-text-tag      #b00000010000000)
(define fast-u16le-text-tag      #b00000100000000)
(define init-u16-text-tag        #b00000110000000)
;;(define r6rs-mode-tag            #b01000000000000)
(define closed-port-tag          #b10000000000000)

(define port-type-mask           #b00000000001111)
(define binary-input-port-bits   #b00000000001001)
(define binary-output-port-bits  #b00000000001010)
(define textual-input-port-bits  #b00000000000101)
(define textual-output-port-bits #b00000000000110)

;;This one  is used for binary  input ports from  which raw bytes
;;must be read.
(define fast-get-byte-tag        #b00000000001001)
;;The  following are  used  for textual  input  ports from  which
;;characters in some encodeing must be read.
(define fast-get-char-tag        #b00000000010101)
(define fast-get-utf8-tag        #b00000000100101)
(define fast-get-latin-tag       #b00000001100101)
(define fast-get-utf16be-tag     #b00000010000101)
(define fast-get-utf16le-tag     #b00000100000101)

;;This one  is used  for binary output  ports to which  raw bytes
;;must be written.
(define fast-put-byte-tag        #b00000000001010)
;;The  following  are used  for  textual  output  ports to  which
;;characters in some encodeing must be written.
(define fast-put-char-tag        #b00000000010110)
(define fast-put-utf8-tag        #b00000000100110)
(define fast-put-latin-tag       #b00000001100110)
(define fast-put-utf16be-tag     #b00000010000110)
(define fast-put-utf16le-tag     #b00000100000110)
(define init-put-utf16-tag       #b00000110000110)

;;                                 32109876543210
(define fast-attrs-mask          #b00111111111111)

(define-syntax $port-fast-attrs
  ;;Extract the type bits from the tag of a port X.
  ;;
  (identifier-syntax (lambda (x)
		       (import (ikarus system $fx))
		       ($fxlogand ($port-tag x) fast-attrs-mask))))

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


;;;; helpers

;;ALL-DATA-IN-BUFFER is  used in place  of the READ!  procedure  to mark
;;ports whose  buffer is  all the data  there is,  that is: there  is no
;;underlying device.
;;
(define all-data-in-buffer 'all-data-in-buffer)

(define-syntax u8?
  ;;Evaluate to true if the argument is an unsigned byte.
  ;;
  (let ()
    (import (ikarus system $fx))
    (syntax-rules ()
      ((_ x)
       ($fxzero? ($fxlogand x -256))))))

(define-syntax %the-true-value?
  ;;Evaluate to  true if the object  is the actual  #t value, not
  ;;just true according to Scheme semantics.
  ;;
  (syntax-rules ()
    ((_ ?obj)
     (eqv? #t ?obj))))

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

(define (port-id p)
  (if (port? p)
      ($port-id p)
    (die 'port-id "not a port" p)))


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
	      (PORT.HAS-BUFFER?			(%dot-id ".has-buffer?"))
		;true if the port has a buffer
	      (PORT.HAS-NO-BUFFER?		(%dot-id ".has-no-buffer?"))
		;true if the port has no buffer
	      (PORT.BUFFER.FULL?		(%dot-id ".buffer.full?"))
		;true if the buffer is full
	      (PORT.CLOSED?			(%dot-id ".closed?"))
		;true if the port is closed
	      (PORT.FAST-ATTRS			(%dot-id ".fast-attrs"))
		;fixnum, the type attributes bits
	      (PORT.BUFFER.SIZE			(%dot-id ".buffer.size"))
		;fixnum, the buffer size
	      (PORT.BUFFER.ROOM			(%dot-id ".buffer.room"))
		;fixnum, the number of bytes available in the buffer
	      (PORT.BUFFER.RESET!		(%dot-id ".buffer.reset!"))
		;method, set the buffer index and used size to zero
	      (PORT.BUFFER.INDEX.INCR!		(%dot-id ".buffer.index.incr!"))
		;method, increment the buffer index
	      (PORT.BUFFER.USED-SIZE.INCR!	(%dot-id ".buffer.used-size.incr!"))
		;method, increment
	      (PORT.MARK-AS-CLOSED		(%dot-id ".mark-as-closed"))
		;method, mark the port as closed
	      (PORT.COOKIE.POS.INCR!		(%dot-id ".cookie.pos.incr!")))
		;method, increment the POS field of the cookie
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
		  (PORT.CLOSED?		(identifier-syntax ($port-closed?	?port)))
		  (PORT.FAST-ATTRS	(identifier-syntax ($port-fast-attrs	?port)))
		  (PORT.HAS-BUFFER?
		   (identifier-syntax ($port-buffer ?port)))
		  (PORT.HAS-NO-BUFFER?
		   (identifier-syntax (not ($port-buffer ?port))))
		  (PORT.BUFFER.SIZE
		   (identifier-syntax (?buffer-length ($port-buffer ?port))))
		  (PORT.ATTRIBUTES
		   (identifier-syntax
		    (_
		     ($port-attrs  ?port))
		    ((set! id ?value)
		     ($set-port-attrs! ?port ?value))))
		  (PORT.BUFFER.INDEX
		   (identifier-syntax
		    (_
		     ($port-index  ?port))
		    ((set! id ?value)
		     ($set-port-index! ?port ?value))))
		  (PORT.BUFFER.USED-SIZE
		   (identifier-syntax
		    (_
		     ($port-size  ?port))
		    ((set! id ?value)
		     ($set-port-size! ?port ?value)))))
	       (let ()
		 (import UNSAFE) ;for FX+ and FX=
		 (let-syntax
		     ((PORT.BUFFER.FULL?
		       (identifier-syntax (fx= PORT.BUFFER.USED-SIZE PORT.BUFFER.SIZE)))
		      (PORT.BUFFER.ROOM
		       (identifier-syntax (fx- PORT.BUFFER.SIZE PORT.BUFFER.INDEX)))
		      (PORT.BUFFER.RESET!
		       (syntax-rules ()
			 ((_)
			  (begin
			    (set! PORT.BUFFER.INDEX     0)
			    (set! PORT.BUFFER.USED-SIZE 0)))))
		      (PORT.BUFFER.INDEX.INCR!
		       (syntax-rules ()
			 ((_)
			  (set! PORT.BUFFER.INDEX (fx+ 1     PORT.BUFFER.INDEX)))
			 ((_ ?step)
			  (set! PORT.BUFFER.INDEX (fx+ ?step PORT.BUFFER.INDEX)))))
		      (PORT.BUFFER.USED-SIZE.INCR!
		       (syntax-rules ()
			 ((_)
			  (set! PORT.BUFFER.USED-SIZE (fx+ 1     PORT.BUFFER.USED-SIZE)))
			 ((_ ?step)
			  (set! PORT.BUFFER.USED-SIZE (fx+ ?step PORT.BUFFER.USED-SIZE)))))
		      (PORT.MARK-AS-CLOSED
		       (syntax-rules ()
			 ((_)
			  ($mark-port-closed! ?port))))
		      (PORT.COOKIE.POS.INCR!
		       (syntax-rules ()
			 ((_ ?step)
			  (let ((cookie PORT.COOKIE))
			    (set-cookie-pos! cookie (+ (cookie-pos cookie) ?step)))))))
		   . ?body)))))))))


;;;; predicates

;;Defined by R6RS.  Return #t if X is a port.
(define port?
  primop.port?)

(let-syntax ((define-predicate (syntax-rules ()
				 ((_ ?who ?bits)
				  (define (?who x)
				    (fx= (fxand ($port-tag x) ?bits) ?bits))))))

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


;;;; cookie data structure
;;
;;An  instance of  this  structure is  associated  to every  port
;;position data  structure.  It registers  the underlying device,
;;if any, and it  tracks the underlying device's position, number
;;of rows and columns.
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
;;  position in the underlying device.
;;
;;  If no underlying device exists: this field is set to zero.
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
  (if (input-port? port)
      (with-port (port)
	(+ (cookie-pos port.cookie) (fx+ 1 port.buffer.index)))
    (error 'input-port-byte-position "not an input port" port)))

(define (%mark/return-newline port)
  ;;Register the presence of a #\newline character at the current
  ;;position  in  the  port.   Return  a  newline  character  for
  ;;convenience at the call site.
  ;;
  (with-textual-port (port)
    (let ((cookie port.cookie))
      (set-cookie-row-num!     cookie (+ 1                 (cookie-row-num cookie)))
      (set-cookie-newline-pos! cookie (+ port.buffer.index (cookie-pos cookie)))))
  #\newline)

(define (input-port-column-number port)
  ;;Defined by  Ikarus.  Return the current column  number for an
  ;;input port.
  ;;
;;;FIXME It computes the count assuming 1 byte = 1 character.
  (if (input-port? port)
      (with-textual-port (port)
	(let ((cookie port.cookie))
	  (- (+ (cookie-pos cookie) port.buffer.index)
	     (cookie-newline-pos cookie))))
    (die 'input-port-column-number "not an input port" port)))

(define (input-port-row-number port)
  ;;Defined  by Ikarus.   Return the  current row  number  for an
  ;;input port.
  ;;
;;;FIXME It computes the count assuming 1 byte = 1 character.
  (if (input-port? port)
      (with-textual-port (port)
	(cookie-row-num port.cookie))
    (die 'input-port-row-number "not an input port" port)))


;;;; port position

(define (port-has-port-position? port)
  ;;Defined  by  R6RS.   Return  #t  if  the  port  supports  the
  ;;PORT-POSITION operation, and #f otherwise.
  ;;
  (define who 'port-has-port-position?)
  (if (port? port)
      (with-port (port)
	(and port.get-position #t))
    (die who "not a port" port)))

(define (port-has-set-port-position!? port)
  ;;Defined by R6RS.  The PORT-HAS-SET-PORT-POSITION!?  procedure
  ;;returns  #t  if  the  port  supports  the  SET-PORT-POSITION!
  ;;operation, and #f otherwise.
  ;;
  (define who 'port-has-set-port-position!?)
  (if (port? port)
      (with-port (port)
	(and port.set-position! #t))
    (die who "not a port" port)))

(define (port-position port)
  ;;Defined  by  R6RS.   For  a binary  port,  the  PORT-POSITION
  ;;procedure returns the index of the position at which the next
  ;;byte would  be read from or  written to the port  as an exact
  ;;non--negative   integer   object.    For  a   textual   port,
  ;;PORT-POSITION      returns      a      value     of      some
  ;;implementation-dependent   type   representing   the   port's
  ;;position; this value  may be useful only as  the POS argument
  ;;to SET-PORT-POSITION!, if the latter is supported on the port
  ;;(see below).
		;
  ;;If  the port  does not  support the  operation, PORT-POSITION
  ;;raises an exception with condition type "&assertion".
  ;;
  ;;*NOTE* For a  textual port, the port position  may or may not
  ;;be  an integer  object.   If  it is  an  integer object,  the
  ;;integer object  does not necessarily correspond to  a byte or
  ;;character position.
  ;;
  (define who 'port-position)
  (unless (port? port)
    (die who "not a port" port))
  (with-port (port)
    (cond ((procedure? port.get-position)
	   (let ((device-position (port.get-position)))
	     ;;DEVICE-POSITION is the  position in the underlying
	     ;;device, but  we have  to return the  full position
	     ;;taking into account the offset in the input/output
	     ;;buffer.
	     (if (or (fixnum? device-position) (bignum? device-position))
		 (if (input-port? port)
		     (- device-position (- port.buffer.used-size port.buffer.index))
		   (+ device-position port.buffer.index))
	       (die who "invalid value returned by get-position" port))))
	  ((%the-true-value? port.get-position)
	   ;;In this  case the  underlying device (if  any) still
	   ;;may have a position  stored in the cookie.  If there
	   ;;is  no underlying  device  (that is:  the buffer  is
	   ;;itself the  device) the POS  field of the  cookie is
	   ;;perpetually set to zero.
	   (+ port.buffer.index (cookie-pos port.cookie)))
	  (else
	   (die who "port does not support port-position operation" port)))))

(define (set-port-position! port pos)
  ;;Defined by R6RS.   If PORT is a binary port,  POS should be a
  ;;non-negative  exact integer  object.   If PORT  is a  textual
  ;;port,  POS  should   be  the  return  value  of   a  call  to
  ;;PORT-POSITION on PORT.
  ;;
  ;;The  SET-PORT-POSITION! procedure  raises  an exception  with
  ;;condition type  &assertion if the  port does not  support the
  ;;operation,   and    an   exception   with    condition   type
  ;;&i/o-invalid-position  if POS is  not in  the range  of valid
  ;;positions of  PORT.  Otherwise, it sets  the current position
  ;;of  the   port  to   POS.   If  PORT   is  an   output  port,
  ;;SET-PORT-POSITION!  first flushes PORT.
  ;;
  ;;If PORT is  a binary output port and  the current position is
  ;;set beyond the current end of the data in the underlying data
  ;;sink, the object is not extended until new data is written at
  ;;that position.  The contents of any intervening positions are
  ;;unspecified.   Binary ports created  by OPEN-FILE-OUTPUT-PORT
  ;;and  OPEN-FILE-INPUT/OUTPUT-PORT can  always  be extended  in
  ;;this  manner within  the limits  of the  underlying operating
  ;;system.  In other cases, attempts  to set the port beyond the
  ;;current end of data in the underlying object may result in an
  ;;exception with condition type &i/o-invalid-position.
  ;;
  (define who 'set-port-position!)
  (unless (port? port)
    (die who "not a port" port))
  (unless (and (or (fixnum? pos) (bignum? pos)) (>= pos 0))
    (die who "position must be a nonnegative exact integer" pos))
  (with-port (port)
    (let ((setpos! port.set-position!))
      (cond ((procedure? setpos!)
	     ;;An underlying device exists.
	     (if (output-port? port)
		 (flush-output-port port)
	       (port.buffer.reset!))
	     ;;If SETPOS!  fails we can assume  nothing about the
	     ;;position in the device.
	     (setpos! pos)
	     ;;Notice that the POS  field of the port's cookie is
	     ;;set  by this  function  AFTER having  successfully
	     ;;called the port's own SET-POSITION! function.
	     (set-cookie-pos! port.cookie pos))
	    ((eqv? setpos! #t)
	     ;;In this case the  underlying device (if any) still
	     ;;may  have a  position  stored in  the cookie.   If
	     ;;there is no underlying device (that is: the buffer
	     ;;is itself the device)  the POS field of the cookie
	     ;;is perpetually set to zero.
	     (if (<= pos port.buffer.used-size)
		 (set! port.buffer.index pos)
	       (die who "position out of range" pos)))
	    (else
	     (die who "port does not support port position" port))))))


;;;; custom ports

(define guarded-port
  (let ((G (make-guardian)))
    (define (clean-up)
      (let ((port (G)))
	(when port
	  (close-port port)
	  (clean-up))))
    (lambda (port)
      (clean-up)
      (when (fixnum? (cookie-dest ($port-cookie port)))
	(G port))
      port)))

(define ($make-custom-binary-port attributes buffer.used-size identifier
				  read! write! get-position set-position! close buffer-size)
  ;;Build and  return a new  custom binary port, either  input or
  ;;output.  It is used by the following functions:
  ;;
  ;;	make-custom-binary-input-port
  ;;	make-custom-binary-output-port
  ;;
  (let ((buffer.index	0)
	(buffer		(make-bytevector buffer-size))
	(transcoder	#f)
	(cookie		(default-cookie #f)))
    ($make-port attributes
		buffer.index buffer.used-size buffer
		transcoder identifier
		read! write! get-position set-position! close
		cookie)))

(define ($make-custom-textual-port attributes buffer.used-size identifier
				   read! write! get-position set-position! close buffer-size)
  ;;Build and return  a new custom textual port,  either input or
  ;;output.  It is used by the following functions:
  ;;
  ;;	make-custom-textual-input-port
  ;;	make-custom-textual-output-port
  ;;
  (let ((buffer.index	0)
	(buffer		(make-string buffer-size))
	(transcoder	#t)
	(cookie		(default-cookie #f)))
    ($make-port attributes
		buffer.index buffer.used-size buffer
		transcoder identifier
		read! write! get-position set-position! close
		cookie)))

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
;;; FIXME: get-position and set-position! are ignored for now
  (define who 'make-custom-binary-input-port)
  (unless (string? identifier)
    (die who "id is not a string" identifier))
  (unless (procedure? read!)
    (die who "read! is not a procedure" read!))
  (unless (or (procedure? close) (not close))
    (die who "close should be either a procedure or #f" close))
  (unless (or (procedure? get-position)
	      (not get-position))
    (die who "get-position is not a procedure or #f"
	 get-position))
  (let ((attributes		binary-input-port-bits)
	(buffer.used-size	0)
	(write!			#f)
	(buffer-size		custom-binary-buffer-size))
    ($make-custom-binary-port attributes buffer.used-size identifier
			      read! write! get-position set-position! close buffer-size)))

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
;;; FIXME: get-position and set-position! are ignored for now
  (define who 'make-custom-binary-output-port)
  (unless (string? identifier)
    (die who "id is not a string" identifier))
  (unless (procedure? write!)
    (die who "write! is not a procedure" write!))
  (unless (or (procedure? close) (not close))
    (die who "close should be either a procedure or #f" close))
  (unless (or (procedure? get-position)
	      (not get-position))
    (die who "get-position is not a procedure or #f"
	 get-position))
  (let ((attributes		binary-output-port-bits)
	(buffer.used-size	custom-binary-buffer-size)
	(read!			#f)
	(buffer-size		custom-binary-buffer-size))
    ($make-custom-binary-port attributes buffer.used-size identifier
			      read! write! get-position set-position! close buffer-size)))

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
;;;FIXME: get-position and set-position! are ignored for now
  (define who 'make-custom-textual-input-port)
  (unless (string? identifier)
    (die who "id is not a string" identifier))
  (unless (procedure? read!)
    (die who "read! is not a procedure" read!))
  (unless (or (procedure? close) (not close))
    (die who "close should be either a procedure or #f" close))
  (unless (or (procedure? get-position)
	      (not get-position))
    (die who "get-position is not a procedure or #f"
	 get-position))
  (let ((attributes		(fxior textual-input-port-bits fast-char-text-tag))
	(buffer.used-size	0)
	(write!			#f)
	(buffer-size		custom-textual-buffer-size))
    ($make-custom-textual-port attributes buffer.used-size identifier
			       read! write! get-position set-position! close buffer-size)))

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
;;;FIXME: get-position and set-position! are ignored for now
  (define who 'make-custom-textual-output-port)
  (unless (string? identifier)
    (die who "id is not a string" identifier))
  (unless (procedure? write!)
    (die who "write! is not a procedure" write!))
  (unless (or (procedure? close) (not close))
    (die who "close should be either a procedure or #f" close))
  (unless (or (procedure? get-position)
	      (not get-position))
    (die who "get-position is not a procedure or #f"
	 get-position))
  (let ((attributes		(fxior textual-output-port-bits fast-char-text-tag))
	(buffer.used-size	custom-textual-buffer-size)
	(read!			#f)
	(buffer-size		custom-textual-buffer-size))
    ($make-custom-textual-port attributes buffer.used-size identifier
			       read! write! get-position set-position! close buffer-size)))


;;;; bytevector input ports

(define open-bytevector-input-port
  (case-lambda
   ((bv)
    (open-bytevector-input-port bv #f))
   ((bv maybe-transcoder)
    ;;MAYBE-TRANSCODER must be either a transcoder or false.
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
    (unless (bytevector? bv)
      (die who "not a bytevector" bv))
    (when (and maybe-transcoder (not (transcoder? maybe-transcoder)))
      (die who "not a transcoder" maybe-transcoder))
    (let ((attributes		(input-transcoder-attrs maybe-transcoder who))
	  (buffer.index		0)
	  (buffer.used-size	(bytevector-length bv))
	  (buffer		bv)
	  (transcoder		maybe-transcoder)
	  (identifier		"*bytevector-input-port*")
	  (read!		all-data-in-buffer)
	  (write!		#f)
	  (get-position		#t)
	  (set-position!	#t)
	  (close		#f)
	  (cookie		(default-cookie #f)))
      ($make-port attributes
		  buffer.index buffer.used-size buffer
		  transcoder identifier
		  read! write! get-position set-position! close
		  cookie)))))


;;;; bytevector output ports

(define open-bytevector-output-port
  (case-lambda
   (()
    (open-bytevector-output-port #f))
   ((maybe-transcoder)
    ;;MAYBE-TRANSCODER must be either a transcoder or false.
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
    (unless (or (not maybe-transcoder) (transcoder? maybe-transcoder))
      (die who "invalid transcoder value" maybe-transcoder))
    (let ((output-bvs		'())
	  (position-in-buffer	#f))
      ;;The most common use of  this port type is to append bytes
      ;;and finally extract the whole output bytevector:
      ;;
      ;;  (let-values (((port getter) (open-bytevector-output-port)))
      ;;    (put-bytevector port '#vu8(1 2 3)) ...
      ;;    (getter))
      ;;
      ;;for this reason we implement  the state of the port to be
      ;;somewhat  efficient  for  such  use.   Whenever  data  is
      ;;written  to the port:  a new  bytevector is  prepended to
      ;;OUTPUT-BVS.   When the  getter  is invoked:  the list  is
      ;;converted to the actual bytevector.
      ;;
      ;;This  situation  is  violated if  SET-PORT-POSITION!   is
      ;;applied to  the port; in this case  we convert OUTPUT-BVS
      ;;to  a  list holding  a  single  bytevector  and save  the
      ;;position in  POSITION-IN-BUFFER.  When POSITION-IN-BUFFER
      ;;is   non-false:   OUTPUT-BVS   always  holds   a   single
      ;;bytevector.   Now the  WRITE! procedure  must distinguish
      ;;the  two cases:  data fitting  in the  single bytevector,
      ;;data extending out of the single bytevector.
      ;;
      ;;*NOTE*  The POS  field of  the  cookie is  always set  by
      ;;SET-PORT-POSITION!   and the  various  functions invoking
      ;;the WRITE!   operation, after having  successfully called
      ;;the port's own SET-POSITION!  function; we do not need to
      ;;set  it here  with  the single  exception  of the  getter
      ;;function which needs to reset it to zero.
      ;;
      (define port
	(let ((attributes	(output-transcoder-attrs maybe-transcoder who))
	      ;;For a bytevector  port the buffer index
	      ;;is always zero.
	      (buffer.index	0)
	      (buffer.used-size	(bytevector-port-buffer-size))
	      (buffer		(make-bytevector (bytevector-port-buffer-size)))
	      (transcoder	maybe-transcoder)
	      (identifier	"*bytevector-output-port*")
	      (read!		#f)
	      (get-position	#t)
	      (close		#f)
	      (cookie		(default-cookie #f)))
	  (define (write! src.bv src.start count)
	    (cond ((zero? count)
		   (values))
		  (position-in-buffer
		   (let* ((dst.bv	(car output-bvs))
			  (total-size	(bytevector-length dst.bv))
			  (old-position	(cookie-pos cookie))
			  (delta	(- total-size old-position)))
		     (if (<= count delta)
			 ;;The  new  data   fits  in  the  single
			 ;;bytevector.
			 (bytevector-copy! src.bv src.start dst.bv old-position count)
		       (begin
			 ;;The new  data goes part  in the single
			 ;;bytevector   and   part   in   a   new
			 ;;bytevector.
			 (bytevector-copy! src.bv src.start dst.bv old-position delta)
			 (let* ((src.start	(+ delta src.start))
				(delta		(- count delta))
				(dst.bv		(make-bytevector delta)))
			   (bytevector-copy! src.bv src.start dst.bv 0 delta)
			   (set! output-bvs (cons dst.bv output-bvs))
			   (set! position-in-buffer #f))))))
		  (else
		   ;;Prepend a new bytevector to OUTPUT-BVS.
		   (let ((dst.bv (make-bytevector count)))
		     (bytevector-copy! src.bv src.start dst.bv 0 count)
		     (set! output-bvs (cons dst.bv output-bvs)))))
	    count)
	  (define (set-position! new-position)
	    ;;NEW-POSITION  has  already  been validated  by  the
	    ;;procedure SET-PORT-POSITION!.
	    ;;
	    (let ((old-position (cookie-pos cookie)))
	      (cond ((< old-position new-position)
		     (raise (condition
			     (make-who-condition who)
			     (make-message-condition "attempt to set port position beyond limit")
			     (make-i/o-invalid-position-error new-position))))
		    ((> old-position new-position)
		     (flush-output-port port)
		     (let-values (((bv bv.len.unused) (append-bytevectors output-bvs)))
		       (set! output-bvs (list bv)))
		     (set! position-in-buffer new-position))
		    (else ;(= old-position new-position)
		     (values)))))
	  ($make-port attributes
		      buffer.index buffer.used-size buffer
		      transcoder identifier
		      read! write! get-position set-position! close
		      cookie)))
      (define (getter)
	(unless ($port-closed? port)
	  (flush-output-port port))
	(let-values (((bv bv.len.unused) (append-bytevectors output-bvs)))
	  (set! output-bvs '())
	  (set-cookie-pos! ($port-cookie port) 0)
	  bv))
      (define (append-bytevectors bvs)
	(let recur ((bvs bvs)
		    (accumulated-total-length 0))
	  (cond ((null? bvs)
		 (values (make-bytevector accumulated-total-length) 0))
		(else
		 (let* ((src.bv  (car bvs))
			(src.len (bytevector-length src.bv)))
		   (let-values
		       (((dst.bv next-byte-index)
			 (recur (cdr bvs) (fx+ src.len accumulated-total-length))))
		     (bytevector-copy! src.bv 0 dst.bv next-byte-index src.len)
		     (values dst.bv (fx+ src.len next-byte-index))))))))
      (values port getter)))))

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
    (unless (procedure? proc)
      (die who "not a procedure" proc))
    (unless (or (not transcoder) (transcoder? transcoder))
      (die who "invalid transcoder argument" transcoder))
    (let-values (((port getter) (open-bytevector-output-port transcoder)))
      (proc port)
      (getter)))))


;;;; binary ports, output to

(module (put-u8 put-bytevector)
  (import UNSAFE)

  (define (put-u8 port byte)
    ;;Defined by R6RS.  Write BYTE  to the output port and return
    ;;unspecified values.
    ;;
    (define who 'put-u8)
    (unless (u8? byte)
      (die who "not a u8" byte))
    (with-binary-port (port)
      (cond ((fx= fast-put-byte-tag port.fast-attrs)
	     (if port.has-no-buffer?
		 (put-byte/unbuffered! port byte who)
	       (let try-again-after-flushing-buffer ()
		 (if (fx< port.buffer.index port.buffer.size)
		     ;;The buffer exists and  it has room for one
		     ;;byte.
		     (begin
		       (assert (fx<= port.buffer.index port.buffer.used-size))
		       (bytevector-u8-set! port.buffer port.buffer.index byte)
		       (when (fx= port.buffer.index port.buffer.used-size)
			 (port.buffer.used-size.incr!))
		       (port.buffer.index.incr!))
		   ;;The buffer exists but  it has no room: flush
		   ;;the buffer and try again.
		   (begin
		     (assert (= port.buffer.used-size port.buffer.index))
		     (assert (= port.buffer.used-size port.buffer.size))
		     (flush-output-port port)
		     (try-again-after-flushing-buffer))))))
	    ((output-port? port)
	     (die who "not a binary port" port))
	    (else
	     (die who "not an output port" port)))))

  (define put-bytevector
    ;;(put-bytevector port bv)
    ;;(put-bytevector port bv start)
    ;;(put-bytevector port bv start count)
    ;;
    ;;START and COUNT must  be non-negative exact integer objects
    ;;that default to 0 and:
    ;;
    ;; (- (bytevector-length BV) START)
    ;;
    ;;respectively.   BV   must  have   a  length  of   at  least
    ;;START+COUNT.  The PUT-BYTEVECTOR procedure writes the COUNT
    ;;bytes of the  bytevector BV starting at index  START to the
    ;;output   port.    The   PUT-BYTEVECTOR  procedure   returns
    ;;unspecified values.
    ;;
    (put-string/bv 'put-bytevector "not a bytevector"
		   bytevector? bytevector-length $put-bytevector))

  (define ($put-bytevector port src.bv src.start count)
    ;;Write COUNT bytes from  the bytevector SRC.BV to the binary
    ;;output   PORT  starting   at   offset  SRC.START.    Return
    ;;unspecified values.
    ;;
    (define who 'put-bytevector)
    (with-binary-port (port)
      (cond ((fx= fast-put-byte-tag port.fast-attrs)
	     (if port.has-no-buffer?
		 ;;Write  bytes  one  by  one to  the  underlying
		 ;;device.
		 (let next-byte ((index src.start)
				 (last  (fx+ src.start count)))
		   (unless (fx= index last)
		     (put-byte/unbuffered! port (bytevector-u8-ref src.bv index) who)
		     (next-byte (fx+ 1 index) last)))
	       ;;Write  bytes to  the buffer  and, if  the buffer
	       ;;fills up, to the underlying device.
	       (let try-again-after-flushing-buffer ((room port.buffer.room))
		 (cond ((fx= 0 room)
			;;The buffer exists and it is full.
			(flush-output-port port)
			(try-again-after-flushing-buffer port.buffer.room))
		       ((fx<= count room)
			;;The buffer  exists and there  is enough
			;;room for all of the COUNT bytes.
			(copy! src.bv src.start port.buffer port.buffer.index count)
			(port.buffer.index.incr! count)
			(when (fx< port.buffer.used-size port.buffer.index)
			  (set! port.buffer.used-size port.buffer.index)))
		       (else
			;;The buffer exists  and it can hold some
			;;but not all of the COUNT bytes.
			(assert (fx> count room))
			(copy! src.bv src.start port.buffer port.buffer.index room)
			(set! port.buffer.index     port.buffer.size)
			(set! port.buffer.used-size port.buffer.index)
			(flush-output-port port)
			(try-again-after-flushing-buffer port.buffer.room))))))
	    ((output-port? port)
	     (die who "not a binary port" port))
	    (else
	     (die who "not an output port" port)))))

  (define (copy! src.bv src.start dst.bv dst.start count)
;;;FIXME We are not using  BYTEVECTOR-COPY! here in an attempt to
;;;have  more  efficiency  because  we  know  the  arguments  are
;;;correct.   It would  be  better  to replace  COPY!   with a  C
;;;implemented  function not  validating  its arguments.   (Marco
;;;Maggi; Aug 31, 2011)
    (when (fx> count 0)
      (bytevector-u8-set! dst.bv dst.start (bytevector-u8-ref src.bv src.start))
      (copy! src.bv (fx+ src.start 1) dst.bv (fx+ dst.start 1) (fx- count 1)))))


;;;; string output ports

(define (open-output-string)
  (%open-output-string 'open-output-string))

(define (%open-output-string who)
  ;;Defined   by   Ikarus.   Return   an   output  textual   port
  ;;accumulating   the  characters  written   to  it   for  later
  ;;extraction by GET-OUTPUT-STRING.
  ;;
  ;;GET-OUTPUT-STRING, when  called, returns a  string consisting
  ;;of all  the port's accumulated characters  (regardless of the
  ;;port's current position),  removes the accumulated characters
  ;;from the port, and resets the port's position.
  ;;
  ;;The most common use of this  port type is to append chars and
  ;;finally extract the whole output string:
  ;;
  ;;  (let ((port (open-output-string)))
  ;;    (display "ciao" port) ...
  ;;    (get-output-string))
  ;;
  ;;for  this reason we  implement the  state of  the port  to be
  ;;somewhat efficient for such use.
  ;;
  ;;This port has  no buffer and its underlying  device is a list
  ;;of strings stored in the DEST field of the port's cookie:
  ;;
  ;;*  When the  WRITE!  function  is invoked:  a  new string  is
  ;;prepended to the device.
  ;;
  ;;* When  the getter is invoked:  the list is  converted to the
  ;;actual  full string  and returned  and the  device is  set to
  ;;null.
  ;;
  ;;This most common  situation is violated if SET-PORT-POSITION!
  ;;is applied  to the port; the  SET-POSITION! function converts
  ;;the device to a list  holding a single string holding all the
  ;;accumulated  characters.  In  this  situation it  is easy  to
  ;;detect  if the  position  is inside  the already  accumulated
  ;;characters by  comparing the position index to  the length of
  ;;the   single  string.    Now  the   WRITE!    procedure  must
  ;;distinguish the two cases: data fitting in the single string,
  ;;data extending out of the single string.
  ;;
  ;;*NOTE*  The  POS  field  of  the  cookie  is  always  set  by
  ;;SET-PORT-POSITION!   and the  various functions  invoking the
  ;;WRITE!   operation,  after  having  successfully  called  the
  ;;port's own WRITE! or SET-POSITION! functions.  We do not need
  ;;to  set it  here  with  the single  exception  of the  getter
  ;;function which needs to reset it to zero.
  ;;
  (define port
    (let ((attributes		(fxior textual-output-port-bits fast-char-text-tag))
	  ;;For a string port there is no buffer.
	  (buffer.index		0)
	  (buffer.used-size	0)
	  (buffer		#f)
	  (transcoder		#t)
	  (identifier		"*string-output-port*")
	  (read!		#f)
	  (get-position		#t)
	  (close		#f)
	  (cookie		(default-cookie '())))
      ;; (define (%string-copy! src.str src.start dst.str dst.start count)
      ;;   (unless (fx= 0 count)
      ;;     (string-set! dst.str dst.start (string-ref srx.str src.start))
      ;;     (string-copy! src.str (fx+ 1 str.start) dst.str (fx+ 1 dst.start) (fx- count 1))))
      (define-inline (get-device)
	(cookie-dest cookie))
      (define-inline (set-device! ?new-device)
	(set-cookie-dest! cookie ?new-device))
      (define (write! src.str src.start count)
	(if (zero? count)
	    count
	  (let ((output-strs  (get-device))
		(old-position (cookie-pos  cookie)))
	    (if (and (not (null? output-strs))
		     (null? (cdr output-strs))
		     (< old-position (string-length (car output-strs))))
		;;The  current  position  was  set  inside  the
		;;already accumulated data.
		(let* ((dst.str		(car output-strs))
		       (total-size	(string-length dst.str))
		       (delta		(- total-size old-position)))
		  (if (<= count delta)
		      ;;The new data fits in the single string.
		      (string-copy! src.str src.start dst.str old-position count)
		    (begin
		      ;;The  new data  goes part  in  the single
		      ;;string and part in a new string.
		      (string-copy! src.str src.start dst.str old-position delta)
		      (let* ((src.start	(+ delta src.start))
			     (delta		(- count delta))
			     (dst.str		(make-string delta)))
			(string-copy! src.str src.start dst.str 0 delta)
			(set-device! (cons dst.str output-strs))))))
	      ;;The  current  position is  at  the  end of  the
	      ;;already accumulated data.  Prepend a new string
	      ;;to OUTPUT-STRS.
	      (let ((dst.str (make-string count)))
		(string-copy! src.str src.start dst.str 0 count)
		(set-device! (cons dst.str output-strs))))
	    count)))
      (define (set-position! new-position)
	;;NEW-POSITION   has  already   been  validated   by  the
	;;procedure SET-PORT-POSITION!.
	;;
	(let ((old-position (cookie-pos cookie)))
	  (cond ((< old-position new-position)
		 (raise (condition
			 (make-who-condition who)
			 (make-message-condition "attempt to set port position beyond limit")
			 (make-i/o-invalid-position-error new-position))))
		((> old-position new-position)
		 (set-device! (list (apply string-append (reverse (get-device))))))
		(else ;(= old-position new-position)
		 (values)))))
      ($make-port attributes
		  buffer.index buffer.used-size buffer
		  transcoder identifier
		  read! write! get-position set-position! close
		  cookie)))
  port)

(define (get-output-string port)
  ;;Defined by Ikarus.  Return the string accumulated in the PORT
  ;;opened by OPEN-OUTPUT-STRING.
  ;;
  (define who 'get-output-string)
  (define (wrong-port-error)
    (die who "not an output-string port" port))
  (unless (port? port)
    (die who "not a port" port))
  (with-textual-port (port)
    (unless (fx= (fxand port.attributes textual-output-port-bits) textual-output-port-bits)
      (wrong-port-error))
    (unless port.closed?
      (flush-output-port port))
    (let ((cookie port.cookie))
      (unless (cookie? cookie)
	(wrong-port-error))
      (let ((device (cookie-dest cookie)))
	(unless (or (null? device) (pair? device))
	  (wrong-port-error))
	(set-cookie-dest! cookie '())
	(set-cookie-pos!  cookie 0)
	(apply string-append (reverse device))))))

;;; --------------------------------------------------------------------

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
  (let ((port (%open-output-string 'open-string-output-port)))
    (values port (lambda ()
		   (get-output-string port)))))

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
  (unless (procedure? proc)
    (die who "not a procedure" proc))
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
  (unless (procedure? proc)
    (die who "not a procedure" proc))
  (let-values (((port extract) (open-string-output-port)))
    (parameterize ((current-output-port port))
      (proc))
    (extract)))


;;;; generic port output

(define (with-output-to-port port proc)
  ;;Defined by Ikarus.   Set port as the current  output port and
  ;;calls PROC with no arguments.  The port is the current output
  ;;port only for the extent of the call to PROC.
  ;;
  (define who 'with-output-to-port)
  (unless (procedure? proc)
    (die who "not a procedure" proc))
  (unless (output-port? port)
    (die who "not an output port" port))
  (unless (textual-port? port)
    (die who "not a textual port" port))
  (parameterize ((current-output-port port))
    (proc)))


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
  (let ((attributes		(fxior textual-input-port-bits fast-char-text-tag))
	(buffer.index		0)
	(buffer.used-size	(string-length str))
	(buffer			str)
	(transcoder		#t)
	(read!			all-data-in-buffer)
	(write!			#f)
	(get-position		#t)
	(set-position!		#t)
	(close			#f)
	(cookie			(default-cookie #f)))
    ($make-port attributes
		buffer.index buffer.used-size buffer
		transcoder id
		read! write! get-position set-position! close
		cookie)))


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
  (unless (transcoder? transcoder)
    (die who "not a transcoder" transcoder))
  (unless (port? port)
    (die who "not a port" port))
  (with-binary-port (port)
    (when port.transcoder
      (die who "not a binary port" port))
    (when port.closed?
      (die who "cannot transcode closed port" port))
    (let ((read! ($port-read! port))
	  (write! ($port-write! port)))
      (port.mark-as-closed)
      (guarded-port
       ($make-port (cond (port.read!
			  (input-transcoder-attrs  transcoder who))
			 (port.write!
			  (output-transcoder-attrs transcoder who))
			 (else
			  (die who "port is neither input nor output!" port)))
		   port.buffer.index port.buffer.used-size port.buffer
		   transcoder port.id
		   port.read! port.write! port.get-position port.set-position! port.close
		   port.cookie)))))

(define (port-transcoder port)
  ;;Defined by R6RS.  Return  the transcoder associated with PORT
  ;;if  PORT is  textual and  has an  associated  transcoder, and
  ;;returns  false  if  PORT  is  binary  or  does  not  have  an
  ;;associated transcoder.
  ;;
  (if (port? port)
      (let ((tr ($port-transcoder port)))
	(and (transcoder? tr) tr))
    (die 'port-transcoder "not a port" port)))

(define (input-transcoder-attrs x who)
  (cond ((not x) ;binary input port
	 binary-input-port-bits)
	((not (eq? 'none (transcoder-eol-style x)))
	 (die who "unsupported transcoder eol-style" (transcoder-eol-style x)))
	((eq? 'latin-1-codec (transcoder-codec x))
	 (fxior textual-input-port-bits fast-u8-text-tag))
	;;attrs  for   utf-8-codec  are   set  as  part   of  the
	;;bom-reading dance when the first char is read.
	(else
	 textual-input-port-bits)))

(define (output-transcoder-attrs x who)
  (cond ((not x) ;binary input port
	 binary-output-port-bits)
	((not (eq? 'none (transcoder-eol-style x)))
	 (die who "unsupported transcoder eol-style" (transcoder-eol-style x)))
	((eq? 'latin-1-codec (transcoder-codec x))
	 (fxior textual-output-port-bits fast-u8-text-tag))
	((eq? 'utf-8-codec   (transcoder-codec x))
	 (fxior textual-output-port-bits fast-u7-text-tag))
	((eq? 'utf-16-codec  (transcoder-codec x))
	 (fxior textual-output-port-bits fast-u16be-text-tag))
	(else
	 (die who "unsupported codec" (transcoder-codec x)))))


;;;; closing ports

(define (port-closed? port)
  ;;Defined  by Ikarus.   Return true  if PORT  has  already been
  ;;closed.
  ;;
  (if (port? port)
      ($port-closed? port)
    (error 'port-closed? "not a port" port)))

(define ($port-closed? p)
  (import UNSAFE)
  (not (fx= (fxand ($port-attrs p) closed-port-tag) 0)))

(define ($mark-port-closed! port)
  ($set-port-attrs! port (fxior closed-port-tag (fxand ($port-attrs port) port-type-mask))))

(define ($close-port port)
  ;;Assume that PORT is a port object.
  ;;
  (with-port (port)
    (unless port.closed?
      (when port.write!
	(flush-output-port port))
      (port.mark-as-closed)
      (let ((close port.close))
	(when (procedure? close)
	  (close))))))

(define (close-port port)
  ;;Defined  by  R6RS.   Closes  the  port,  rendering  the  port
  ;;incapable  of delivering or  accepting data.   If PORT  is an
  ;;output port, it is flushed  before being closed.  This has no
  ;;effect if the port has already been closed.  A closed port is
  ;;still a  port.  The CLOSE-PORT  procedure returns unspecified
  ;;values.
  ;;
  (unless (port? port)
    (die 'close-port "not a port" port))
  ($close-port port))

(define (close-input-port port)
  ;;Define by R6RS.  Close an input port.
  ;;
  (unless (input-port? port)
    (die 'close-input-port "not an input port" port))
  ($close-port port))

(define (close-output-port port)
  ;;Define by R6RS.  Close an output port.
  ;;
  (unless (output-port? port)
    (die 'close-output-port "not an output port" port))
  ($close-port port))


;;;; port mode

(define (port-mode port)
  ;;Defined by Ikarus.
  ;;
  (if (port? port)
      (cookie-mode ($port-cookie port))
    (die 'port-mode "not a port" port)))

(define (set-port-mode! port mode)
  ;;Defined by Ikarus.
  ;;
  (define who 'set-port-mode!)
  (unless (port? port)
    (die who "not a port" port))
  (case mode
    ((r6rs-mode vicare-mode)
     (set-cookie-mode! ($port-cookie port) mode))
    (else
     (die who "invalid mode" mode))))


(define (put-byte/unbuffered! port byte who)
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
    (when port.closed?
      (die who "port is closed" port))
    (let ((bv (make-bytevector 1)))
      (bytevector-u8-set! bv 0 byte)
      (let ((count (port.write! bv 0 1)))
	;;We  need to  account for  the case  the  WRITE!  function
	;;returns  an  incorrect value,  for  example a  non-fixnum
	;;value.
	(cond ((eq? count 1)
	       (let ((cookie port.cookie))
		 (set-cookie-pos! cookie (fx+ 1 (cookie-pos cookie)))))
	      ((eq? count 0)
	       (port.mark-as-closed)
	       (die who "could not write bytes to output port" port))
	      (else
	       (die who "invalid return value from write! proc" count port)))))))

(define (put-char/unbuffered! port char who)
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
    (when port.closed?
      (die who "port is closed" port))
    (let ((str (string char)))
      (let ((count (port.write! str 0 1)))
	;;We need  to account for  the case the  WRITE!  function
	;;returns  an incorrect value,  for example  a non-fixnum
	;;value.
	(cond ((eq? count 1)
	       (let ((cookie port.cookie))
		 (set-cookie-pos! cookie (fx+ 1 (cookie-pos cookie)))))
	      ((eq? count 0)
	       (port.mark-as-closed)
	       (die who "could not write char to output port" port))
	      (else
	       (die who "invalid return value from write! proc" count port)))))))


(define flush-output-port
  (case-lambda
   (()
    (flush-output-port (current-output-port)))
   ((port)
    ;;Defined by R6RS.  PORT must be an output port.  Flushes any
    ;;buffered output  from the buffer of PORT  to the underlying
    ;;file, device, or object.  Return unspecified values.
    ;;
    ;;When no bytes can be written by the port's WRITE! function,
    ;;the port is marked as closed.  FIXME Is this correct?
    ;;
    (import UNSAFE)
    (define who who)
    (unless (output-port? port)
      (die who "not an output port" port))
    (with-port (port)
      (when port.closed?
	(die who "port is closed" port))
      (unless (fx= port.buffer.index 0)
	;;The buffer is not empty.   We can safely write only the
	;;portion  of  the  buffer  between zero  and  the  index
	;;(rather than  between zero and the  used size), because
	;;moving  the position  causes the  buffer to  be flushed
	;;first.
	(let try-again-after-partial-write ()
	  (let ((count (port.write! port.buffer 0 port.buffer.index)))
	    (unless (and (fixnum? count)
			 (fx>= count 0)
			 (fx<= count port.buffer.index))
	      ;;FIXME Should we mark the port as closed here?
	      (die who "write! returned an invalid value" count))
	    (let ((cookie port.cookie))
	      (set-cookie-pos! cookie (+ (cookie-pos cookie) count)))
	    (cond ((fx= count port.buffer.index)
		   ;;Full success, all bytes absorbed.
		   (set! port.buffer.index     0)
		   (set! port.buffer.used-size 0))
		  ((fx= count 0)
		   ;;Failure, no  bytes absorbed.  Interpreted as
		   ;;unrecoverable failure.
		   (port.mark-as-closed)
		   (die who "could not flush bytes to output port"))
		  (else
		   ;;Partial success, some bytes absorbed.  Shift
		   ;;the  used bytes  left in  the buffer  to the
		   ;;beginning of the same, then recurse.
		   (let ((number-of-bytes-left-in-buffer (fx- port.buffer.index count)))
		     (bytevector-copy! port.buffer count port.buffer 0 number-of-bytes-left-in-buffer)
		     (set! port.buffer.index     number-of-bytes-left-in-buffer)
		     (set! port.buffer.used-size number-of-bytes-left-in-buffer)
		     (try-again-after-partial-write)))))))))))


;;;; bytevector buffer handling for input ports
;;
;;Input functions  always read bytes from the  input buffer; when
;;the  input buffer is  completely consumed:  new bytes  are read
;;from  the  underlying device  refilling  the buffer.   Whenever
;;refilling  reads no  characters (that  is: the  READ!  function
;;returns 0) the port is in EOF state.
;;
;;The following macros make it easier to handle this mechanism by
;;wrapping the %REFILL-BYTEVECTOR-BUFFER function.
;;

(define-syntax data-is-needed-at:	(syntax-rules ()))
(define-syntax if-available-data:	(syntax-rules ()))
(define-syntax if-successful-refill:	(syntax-rules ()))
(define-syntax if-end-of-file:		(syntax-rules ()))

(define-syntax refill-buffer-and-evaluate
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
    ((refill-buffer-and-evaluate (?port ?who)
       (if-end-of-file:		. ?end-of-file-body)
       (if-successful-refill:	. ?after-refill-body))
     (let ()
       (import UNSAFE)
       (let ((count (%refill-bytevector-buffer ?port ?who)))
	 (if (fx= 0 count)
	     (begin . ?end-of-file-body)
	   (begin . ?after-refill-body)))))))

(define-syntax maybe-refill-buffer-and-evaluate
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
    ((maybe-refill-buffer-and-evaluate (?port ?who)
       (data-is-needed-at:	?buffer.offset)
       (if-end-of-file:		. ?end-of-file-body)
       (if-successful-refill:	. ?after-refill-body)
       (if-available-data:	. ?available-data-body))
     (let ()
       (import UNSAFE)
       (if (fx< ?buffer.offset ($port-size ?port))
	   (begin . ?available-data-body)
	 (refill-buffer-and-evaluate (?port ?who)
	   (if-end-of-file:		. ?end-of-file-body)
	   (if-successful-refill:	. ?after-refill-body)))))))

(define (%refill-bytevector-buffer port who)
  ;;Defined by Ikarus.  Assume PORT  is an input port object with
  ;;a bytevector as buffer.  Fill  the input buffer keeping in it
  ;;the bytes already there but not yet consumed; mutate the PORT
  ;;structure fields representing the buffer state.
  ;;
  ;;Return the number  of new bytes loaded.  If  the return value
  ;;is zero:  the underlying device has  no more bytes,  it is at
  ;;its EOF.
  ;;
  (with-binary-port (port)
    (when port.closed?
      (die who "port is closed" port))
    (if (eq? port.read! all-data-in-buffer)
	0
      (let ((buffer port.buffer))
	;;Shift  to the  beginning data  alraedy in  buffer but
	;;still  to  be  consumed;  commit  the  new  position.
	;;Before:
	;;
	;;                                buffer
	;;  |**********+###########+---------|
	;;             ^           ^
	;;           index     used size
	;;
	;;after:
	;;
	;;                                buffer
	;;  |+###########+-------------------|
	;;   ^           ^
	;; index     used size
	;;
	(let ((delta (fx- port.buffer.used-size port.buffer.index)))
	  (port.cookie.pos.incr! port.buffer.index)
	  (unless (fx= delta 0)
	    (bytevector-copy! buffer port.buffer.index buffer 0 delta))
	  (set! port.buffer.index     0)
	  (set! port.buffer.used-size delta))
	;;Fill the buffer with data from the device.  Before:
	;;
	;;                                buffer
	;;  |+***********+-------------------|
	;;   ^           ^
	;; index     used size
	;;
	;;after:
	;;
	;;                                buffer
	;;  |+*******************************|
	;;   ^                               ^
	;; index                         used size
	;;
	(let* ((max   (fx- port.buffer.size port.buffer.used-size))
	       (count (port.read! buffer port.buffer.used-size max)))
	  (unless (fixnum? count)
	    (die who "invalid return value from read! procedure" count))
	  (unless (and (fx>= count 0)
		       (fx<= count max))
	    (die who "read! returned a value out of range" count))
	  (port.buffer.used-size.incr! count)
	  count)))))


(module (read-char get-char lookahead-char peek-char)
  (import UNSAFE)

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
	     (maybe-refill-buffer-and-evaluate (port who)
	       (data-is-needed-at: buffer.offset-byte0)
	       (if-end-of-file: (eof-object))
	       (if-successful-refill: (retry-after-filling-buffer))
	       (if-available-data:
		(let ((byte0 (bytevector-u8-ref port.buffer buffer.offset-byte0)))
		  (if (utf-8-single-byte? byte0)
		      (begin
			(set! port.buffer.index (fx+ 1 buffer.offset-byte0))
			(if (fx= byte0 newline-integer)
			    (%mark/return-newline port)
			  (integer->char byte0)))
		    (get-char-utf8-mode port who byte0))))))))

	((fast-get-char-tag)
	 ;;The PORT is a textual  input port with a Scheme string
	 ;;as  buffer.  We process  here the  simple case  of one
	 ;;char  available  in  the  buffer,  else  we  call  the
	 ;;specialised function for reading characters.
	 (let ((buffer.offset port.buffer.index))
	   (cond ((fx< buffer.offset port.buffer.used-size)
		  (set! port.buffer.index (fx+ 1 buffer.offset))
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
	   (if (fx< buffer.offset port.buffer.used-size)
	       (begin
		 (set! port.buffer.index (fx+ 1 buffer.offset))
		 (let ((byte (bytevector-u8-ref port.buffer buffer.offset)))
		   (if (eqv? byte newline-integer)
		       (%mark/return-newline port)
		     (integer->char byte))))
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
	     (maybe-refill-buffer-and-evaluate (port who)
	       (data-is-needed-at: buffer.offset-byte0)
	       (if-end-of-file: (eof-object))
	       (if-successful-refill: (retry-after-filling-buffer))
	       (if-available-data:
		(let ((byte0 (bytevector-u8-ref port.buffer buffer.offset-byte0)))
		  (if (utf-8-single-byte? byte0)
		      (integer->char byte0)
		    (lookahead-char-utf8-mode port who byte0))))))))

	((fast-get-char-tag)
	 ;;The PORT is a textual  input port with a Scheme string
	 ;;as  buffer.  We process  here the  simple case  of one
	 ;;char  available  in  the  buffer,  else  we  call  the
	 ;;specialised function for reading characters.
	 (let ((i port.buffer.index))
	   (cond ((fx< i port.buffer.used-size)
		  (string-ref port.buffer i))
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
	 (let ((i port.buffer.index))
	   (if (fx< i port.buffer.used-size)
	       (integer->char (bytevector-u8-ref port.buffer i))
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
	(cond ((utf-8-first-of-two-bytes? byte0)
	       (get-2-bytes-character byte0))
	      ((utf-8-first-of-three-bytes? byte0)
	       (get-3-bytes-character byte0))
	      ((utf-8-first-of-four-bytes? byte0)
	       (get-4-bytes-character byte0))
	      (else
	       (error-handler "invalid byte while expecting first byte of UTF-8 character" byte0))))

      (define-inline (get-2-bytes-character byte0)
	(let retry-after-filling-buffer-for-1-more-byte ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (fx+ 1 buffer.offset-byte0))
		 (buffer.offset-past  (fx+ 1 buffer.offset-byte1)))
	    (maybe-refill-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte1)
	      (if-end-of-file:
	       (set! port.buffer.index port.buffer.used-size)
	       (eof-object))
	      (if-successful-refill:
	       (retry-after-filling-buffer-for-1-more-byte))
	      (if-available-data:
	       (let ((byte1 (bytevector-u8-ref port.buffer buffer.offset-byte1)))
		 (set! port.buffer.index buffer.offset-past)
		 (if (utf-8-second-of-two-bytes? byte1)
		     (integer->char (utf-8-two-bytes-compose byte0 byte1))
		   (error-handler "invalid second byte in 2-bytes UTF-8 character" byte1))))))))

      (define-inline (get-3-bytes-character byte0)
	(let retry-after-filling-buffer-for-2-more-bytes ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (fx+ 1 buffer.offset-byte0))
		 (buffer.offset-byte2 (fx+ 1 buffer.offset-byte1))
		 (buffer.offset-past  (fx+ 1 buffer.offset-byte2)))
	    (maybe-refill-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte2)
	      (if-end-of-file:
	       (set! port.buffer.index port.buffer.used-size)
	       (error-handler "unexpected end of file while decoding 3-bytes UTF-8 character"))
	      (if-successful-refill:
	       (retry-after-filling-buffer-for-2-more-bytes))
	      (if-available-data:
	       (let ((byte1 (bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (byte2 (bytevector-u8-ref port.buffer buffer.offset-byte2)))
		 (set! port.buffer.index buffer.offset-past)
		 (if (utf-8-second-and-third-of-three-bytes? byte1 byte2)
		     (let ((n (utf-8-three-bytes-compose byte0 byte1 byte2)))
		       (if (and (fx<= #xD800 n) (fx<= n #xDFFF))
			   (error-handler "invalid integer representation \
                                           as result of decoding 3-bytes UTF-8 character"
					  n)
			 (integer->char n)))
		   (error-handler "invalid second or third byte in 3-bytes UTF-8 character"
				  byte1 byte2))))))))

      (define-inline (get-4-bytes-character byte0)
	(let retry-after-filling-buffer-for-3-more-bytes ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (fx+ 1 buffer.offset-byte0))
		 (buffer.offset-byte2 (fx+ 1 buffer.offset-byte1))
		 (buffer.offset-byte3 (fx+ 1 buffer.offset-byte2))
		 (buffer.offset-past  (fx+ 1 buffer.offset-byte3)))
	    (maybe-refill-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte3)
	      (if-end-of-file:
	       (set! port.buffer.index port.buffer.used-size)
	       (error-handler "unexpected end of file while decoding 4-bytes UTF-8 character"))
	      (if-successful-refill:
	       (retry-after-filling-buffer-for-3-more-bytes))
	      (if-available-data:
	       (let ((byte1 (bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (byte2 (bytevector-u8-ref port.buffer buffer.offset-byte2))
		     (byte3 (bytevector-u8-ref port.buffer buffer.offset-byte3)))
		 (set! port.buffer.index buffer.offset-past)
		 (if (utf-8-second-third-and-fourth-of-three-bytes? byte1 byte2 byte3)
		     (let ((n (utf-8-four-bytes-compose byte0 byte1 byte2 byte3)))
		       (if (and (fx<= #x10000 n) (fx<= n #x10FFFF))
			   (integer->char n)
			 (error-handler "invalid integer representation as result \
                                         of decoding 4-bytes UTF-8 character" n)))
		   (error-handler "invalid second, third or fourth byte in 4-bytes UTF-8 character"
				  byte1 byte2 byte3))))))))

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
	(cond ((utf-8-first-of-two-bytes? byte0)
	       (peek-2-bytes-character byte0))
	      ((utf-8-first-of-three-bytes? byte0)
	       (peek-3-bytes-character byte0))
	      ((utf-8-first-of-four-bytes? byte0)
	       (peek-4-bytes-character byte0))
	      (else
	       (error-handler "invalid byte while expecting first byte of UTF-8 character" byte0))))

      (define-inline (peek-2-bytes-character byte0)
	(let retry-after-filling-buffer-for-1-more-byte ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let ((buffer.offset-byte1 (fx+ 1 buffer.offset-byte0)))
	    (maybe-refill-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte1)
	      (if-end-of-file:
	       (error-handler "unexpected end of file while decoding 2-bytes UTF-8 character"))
	      (if-successful-refill: (retry-after-filling-buffer-for-1-more-byte))
	      (if-available-data:
	       (let ((byte1 (bytevector-u8-ref port.buffer buffer.offset-byte1)))
		 (if (utf-8-second-of-two-bytes? byte1)
		     (integer->char (utf-8-two-bytes-compose byte0 byte1))
		   (error-handler "invalid second byte in 2-bytes UTF-8 character"
				  byte1))))))))

      (define-inline (peek-3-bytes-character byte0)
	(let retry-after-filling-buffer-for-2-more-bytes ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (fx+ 1 buffer.offset-byte0))
		 (buffer.offset-byte2 (fx+ 1 buffer.offset-byte1)))
	    (maybe-refill-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte2)
	      (if-end-of-file:
	       (error-handler "unexpected end of file while decoding 3-bytes UTF-8 character"))
	      (if-successful-refill: (retry-after-filling-buffer-for-2-more-bytes))
	      (if-available-data:
	       (let ((byte1 (bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (byte2 (bytevector-u8-ref port.buffer buffer.offset-byte2)))
		 (if (utf-8-second-and-third-of-three-bytes? byte1 byte2)
		     (let ((n (utf-8-three-bytes-compose byte0 byte1 byte2)))
		       (if (and (fx<= #xD800 n) (fx<= n #xDFFF)) ;forbidden range
			   (error-handler "invalid integer representation as result \
                                           of decoding 3-bytes UTF-8 character"
					  n)
			 (integer->char n)))
		   (error-handler "invalid second or third byte in 3-bytes UTF-8 character"
				  byte1 byte2))))))))

      (define-inline (peek-4-bytes-character byte0)
	(let retry-after-filling-buffer-for-3-more-bytes ()
	  (define-alias buffer.offset-byte0 port.buffer.index)
	  (let* ((buffer.offset-byte1 (fx+ 1 buffer.offset-byte0))
		 (buffer.offset-byte2 (fx+ 1 buffer.offset-byte1))
		 (buffer.offset-byte3 (fx+ 1 buffer.offset-byte2)))
	    (maybe-refill-buffer-and-evaluate (port who)
	      (data-is-needed-at: buffer.offset-byte3)
	      (if-end-of-file:
	       (error-handler "unexpected end of file while decoding 4-bytes UTF-8 character"))
	      (if-successful-refill: (retry-after-filling-buffer-for-3-more-bytes))
	      (if-available-data:
	       (let ((byte1 (bytevector-u8-ref port.buffer buffer.offset-byte1))
		     (byte2 (bytevector-u8-ref port.buffer buffer.offset-byte2))
		     (byte3 (bytevector-u8-ref port.buffer buffer.offset-byte3)))
		 (if (utf-8-second-third-and-fourth-of-three-bytes? byte1 byte2 byte3)
		     (let ((n (utf-8-four-bytes-compose byte0 byte1 byte2 byte3)))
		       (if (and (fx<= #x10000 n) (fx<= n #x10FFFF))
			   (integer->char n)
			 (error-handler "invalid integer representation as result \
                                         of decoding 4-bytes UTF-8 character" n)))
		   (error-handler "invalid second, third or fourth byte \
                                   in 4-bytes UTF-8 character"))))))))

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


      (define (integer->char/invalid integer-representation-of-char)
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
	  "invalid integer representation decoded from UTF-16 surrogate pair")
      	(cond ((fx<= integer-representation-of-char #xD7FF)
      	       (integer->char integer-representation-of-char))
      	      ((fx<  integer-representation-of-char #xE000)
      	       (error-handler errmsg integer-representation-of-char))
      	      ((fx<= integer-representation-of-char #x10FFFF)
      	       (integer->char integer-representation-of-char))
      	      (else
      	       (error-handler errmsg integer-representation-of-char))))

      (let* ((buffer.offset-word0 port.buffer.index)
	     (buffer.offset-word1 (fx+ 2 buffer.offset-word0))
	     (buffer.offset-past  (fx+ 2 buffer.offset-word1)))
	(cond ((fx<= buffer.offset-word1 port.buffer.used-size)
	       ;;There  are  at  least  two bytes  in  the  input
	       ;;buffer,  enough  for  a  full  UTF-16  character
	       ;;encoded as single 16 bits word.
	       (let ((word0 (bytevector-u16-ref port.buffer buffer.offset-word0 endianness)))
		 (cond ((utf-16-single-word? word0)
			;;The word is in  the allowed range for a
			;;UTF-16 encoded character of 16 bits.
			(set! port.buffer.index buffer.offset-word1)
			(integer->char/invalid word0))
		       ((not (utf-16-first-of-two-words? word0))
			(set! port.buffer.index buffer.offset-word1)
			(error-handler "invalid 16-bit word while decoding UTF-16 characters" word0))
		       ((fx<= buffer.offset-past port.buffer.used-size)
			;;The  word  is  the  first of  a  UTF-16
			;;surrogate  pair  and  the input  buffer
			;;already holds the second word.
			(let ((word1 (bytevector-u16-ref port.buffer buffer.offset-word1 endianness)))
			  (if (utf-16-second-of-two-words? word1)
			      (begin
				(set! port.buffer.index buffer.offset-past)
				(integer->char/invalid (utf-16-compose-from-surrogate-pair word0 word1)))
			    (begin
			      (set! port.buffer.index buffer.offset-word1)
			      (error-handler "invalid value as second 16-bit word of \
                                              UTF-16 character" word0)))))
		       (else
			;;The  word  is  the  first of  a  UTF-16
			;;surrogate  pair, but input  buffer does
			;;not hold the full second word.
			(refill-buffer-and-evaluate (port who)
			  (if-end-of-file:
			   (set! port.buffer.index port.buffer.used-size)
			   (error-handler "unexpected end of file while reading second \
                                           16-bit word in UTF-16 surrogate pair character" word0))
			  (if-successful-refill:
			   (recurse)))))))

	      ((fx< buffer.offset-word0 port.buffer.used-size)
	       ;;There is only 1 byte in the input buffer.
	       (refill-buffer-and-evaluate (port who)
		 (if-end-of-file:
		  ;;The  input  data   is  corrupted  because  we
		  ;;expected at least a  16 bits word to be there
		  ;;before EOF.
		  (set! port.buffer.index port.buffer.used-size)
		  (error-handler "unexpected end of file after byte while reading \
                                  16-bit word of UTF-16 character"
				 (bytevector-u8-ref port.buffer buffer.offset-word0)))
		 (if-successful-refill:
		  (recurse))))

	      (else
	       ;;The input buffer is empty.
	       (refill-buffer-and-evaluate (port who)
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

      (define (integer->char/invalid integer-representation-of-char)
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
	  "invalid integer representation decoded from UTF-16 surrogate pair")
      	(cond ((fx<= integer-representation-of-char #xD7FF)
      	       (integer->char integer-representation-of-char))
      	      ((fx<  integer-representation-of-char #xE000)
      	       (error-handler errmsg integer-representation-of-char))
      	      ((fx<= integer-representation-of-char #x10FFFF)
      	       (integer->char integer-representation-of-char))
      	      (else
      	       (error-handler errmsg integer-representation-of-char))))

      (let* ((buffer.offset-word0 port.buffer.index)
	     (buffer.offset-word1 (fx+ 2 buffer.offset-word0))
	     (buffer.offset-past  (fx+ 2 buffer.offset-word1)))
	(cond ((fx<= buffer.offset-word1 port.buffer.used-size)
	       ;;There  are  at  least  two bytes  in  the  input
	       ;;buffer,  enough  for  a  full  UTF-16  character
	       ;;encoded as single 16 bits word.
	       (let ((word0 (bytevector-u16-ref port.buffer buffer.offset-word0 endianness)))
		 (cond ((utf-16-single-word? word0)
			(integer->char/invalid word0))
		       ((not (utf-16-first-of-two-words? word0))
			(error-handler "invalid 16-bit word while decoding UTF-16 characters" word0))
		       ((fx<= buffer.offset-past port.buffer.used-size)
			;;The  word  is  the  first of  a  UTF-16
			;;surrogate  pair  and  the input  buffer
			;;already holds the second word.
			(let ((word1 (bytevector-u16-ref port.buffer buffer.offset-word1 endianness)))
			  (if (utf-16-second-of-two-words? word1)
			      (integer->char/invalid (utf-16-compose-from-surrogate-pair word0 word1))
			    (error-handler "invalid value as second 16-bit word of \
                                            UTF-16 character" word0))))
		       (else
			;;The  word  is  the  first of  a  UTF-16
			;;surrogate  pair, but input  buffer does
			;;not hold the full second word.
			(refill-buffer-and-evaluate (port who)
			  (if-end-of-file:
			   (error-handler "unexpected end of file while reading second \
                                           16-bit word in UTF-16 surrogate pair character" word0))
			  (if-successful-refill:
			   (recurse)))))))

	      ((fx< buffer.offset-word0 port.buffer.used-size)
	       ;;There is only 1 byte in the input buffer.
	       (refill-buffer-and-evaluate (port who)
		 (if-end-of-file:
		  ;;The  input  data   is  corrupted  because  we
		  ;;expected at least a  16 bits word to be there
		  ;;before EOF.
		  (error-handler "unexpected end of file after byte while reading \
                                  16-bit word of UTF-16 character"
				 (bytevector-u8-ref port.buffer buffer.offset-word0)))
		 (if-successful-refill:
		  (recurse))))

	      (else
	       ;;The input buffer is empty.
	       (refill-buffer-and-evaluate (port who)
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
      (assert (fx=? port.buffer.index port.buffer.used-size))
      (refill-buffer-and-evaluate (port who)
	(if-end-of-file: (eof-object))
	(if-successful-refill:
	 (let ((buffer.offset port.buffer.index))
	   (port.buffer.index.incr! buffer-index-increment)
	   (integer->char (bytevector-u8-ref port.buffer buffer.offset)))))))

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
      (assert (fx=? port.buffer.index port.buffer.used-size))
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
	(port.cookie.pos.incr! count)
	(set! port.buffer.used-size count)
	(set! port.buffer.index buffer-index-increment)
	(if (fx= count 0)
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
	    (let ((buffer.offset (fx+ number-of-consumed-bytes port.buffer.index)))
	      (maybe-refill-buffer-and-evaluate (port who)
		(data-is-needed-at: buffer.offset)
		(if-end-of-file: (eof-object))
		(if-successful-refill: (retry-after-filling-buffer))
		(if-available-data:
		 (and (fx=? (car bom) (bytevector-u8-ref port.buffer buffer.offset))
		      (next-byte-in-bom (fx+ 1 number-of-consumed-bytes) (cdr bom)))))))))))

  (define (validate-port-then-parse-bom-and-add-fast-tag-to-untagged-port port who)
    ;;Validate PORT  as a still  open, textual, input  port; read
    ;;the Byte Order Mark  expected for the port's transcoder and
    ;;mutate the port's  attributes tagging the port accordingly.
    ;;Return #t if port is at EOF, #f otherwise.
    ;;
    (unless (input-port? port)
      (die who "not an input port" port))
    (unless (textual-port? port)
      (die who "not a textual port" port))
    (when ($port-closed? port)
      (die who "port is closed" port))
    (with-textual-port (port)
      (unless port.transcoder
	(die who "expected port with transcoder" port))
      (case (transcoder-codec port.transcoder)
	((utf-8-codec)
	 (set! port.attributes (fxior textual-input-port-bits fast-u7-text-tag))
	 (eof-object? (advance-bom port who '(#xEF #xBB #xBF))))
	((utf-16-codec)
	 (let ((big-endian? (advance-bom port who '(#xFE #xFF))))
	   (case big-endian?
	     ((#t)
	      (set! port.attributes (fxior textual-input-port-bits fast-u16be-text-tag))
	      #f)
	     ((#f)
	      (let ((little-endian? (advance-bom port who '(#xFF #xFE))))
		(case little-endian?
		  ((#t #f)
		   ;;If  no  BOM  is  present, we  select  little
		   ;;endian by default.
		   (set! port.attributes (fxior textual-input-port-bits fast-u16le-text-tag))
		   #f)
		  (else
		   (assert (eof-object? little-endian?))
		   #t))))
	     (else
	      (assert (eof-object? big-endian?))
	      #t))))
	(else
	 (die who "BUG: codec not handled" (transcoder-codec port.transcoder))))))

;;; --------------------------------------------------------------------

;;; 1-byte UTF-8 encoding

  (define-inline (utf-8-single-byte? byte)
    ;;Evaluate to true if BYTE  is valid as 1-byte UTF-8 encoding
    ;;of a Unicode character.
    ;;
    (fx< byte 128))

;;; 2-bytes UTF-8 encoding

  (define-inline (utf-8-first-of-two-bytes? byte0)
    ;;Evaluate to true if BYTE0  is valid as first of 2-bytes
    ;;UTF-8 encoding of a Unicode character.
    ;;
    (fx= (fxsra byte0 5) #b110))

  (define-inline (utf-8-second-of-two-bytes? byte1)
    ;;Evaluate to true if BYTE1 is valid as second of 2-bytes
    ;;UTF-8 encoding of a Unicode character.
    ;;
    (fx= (fxsra byte1 6) #b10))

  (define-inline (utf-8-two-bytes-compose byte0 byte1)
    ;;Compose  the   integer  representation  of   a  Unicode
    ;;character from a 2-bytes UTF-8 encoding.
    ;;
    (fxior (fxsll (fxand byte0 #b11111) 6)
	   (fxand byte1 #b111111)))

;;; 3-bytes UTF-8 encoding

  (define-inline (utf-8-first-of-three-bytes? byte0)
    ;;Evaluate to true if BYTE0  is valid as first of 3-bytes
    ;;UTF-8 encoding of a Unicode character.
    ;;
    (fx= (fxsra byte0 4) #b1110))

  (define-inline (utf-8-second-and-third-of-three-bytes? byte1 byte2)
    ;;Evaluate to true if BYTE1 and BYTE2 are valid as second
    ;;and  third  of  3-bytes  UTF-8 encoding  of  a  Unicode
    ;;character.
    ;;
    (fx= (fxsra (fxlogor byte1 byte2) 6) #b10))

  (define-inline (utf-8-three-bytes-compose byte0 byte1 byte2)
    ;;Compose  the   integer  representation  of   a  Unicode
    ;;character from a 3-bytes UTF-8 encoding.
    ;;
    (fxlogor (fxsll (fxand byte0   #b1111) 12)
	     (fxsll (fxand byte1 #b111111)  6)
	     (fxand byte2 #b111111)))

;;; 4-bytes UTF-8 encoding

  (define-inline (utf-8-first-of-four-bytes? byte0)
    ;;Evaluate to true if BYTE0  is valid as first of 4-bytes
    ;;UTF-8 encoding of a Unicode character.
    ;;
    (fx= (fxsra byte0 3) #b11110))

  (define-inline (utf-8-second-third-and-fourth-of-three-bytes? byte1 byte2 byte3)
    ;;Evaluate to true if BYTE1, BYTE2 and BYTE3 are valid as
    ;;second, third and fourth of 4-bytes UTF-8 encoding of a
    ;;Unicode character.
    ;;
    (fx= (fxsra (fxlogor byte1 byte2 byte3) 6) #b10))

  (define-inline (utf-8-four-bytes-compose byte0 byte1 byte2 byte3)
    ;;Compose  the   integer  representation  of   a  Unicode
    ;;character from a 4-bytes UTF-8 encoding.
    ;;
    (fxlogor (fxsll (fxand byte0    #b111) 18)
	     (fxsll (fxand byte1 #b111111) 12)
	     (fxsll (fxand byte2 #b111111)  6)
	     (fxand byte3 #b111111)))

;;; --------------------------------------------------------------------

  ;;Given 16 bits word in a  UTF-16 stream, a fixnum in the range
  ;;[#x0000, #xFFFF], we can classify it on the following axis:
  ;;
  ;; 0000        D7FF D800    DBFF DC00      DFFF E000       FFFF
  ;;  |-------------||-----------||-------------||------------|
  ;;   single word    first in     second in      single word
  ;;   character      pair         pair           character
  ;;
  ;;or the following logic:
  ;;
  ;;word in [#x0000, #xD7FF] => single word character
  ;;word in [#xD800, #xDBFF] => first in surrogate pair
  ;;word in [#xDC00, #xDFFF] => second in surrogate pair
  ;;word in [#xE000, #xFFFF] => single word character
  ;;

  (define-inline (utf-16-single-word? word0)
    (or (fx< word0 #xD800) (fx< #xDFFF word0)))

  (define-inline (utf-16-first-of-two-words? word0)
    (and (fx<= #xD800 word0) (fx<= word0 #xDBFF)))

  (define-inline (utf-16-second-of-two-words? word1)
    (and (fx<= #xDC00 word1) (fx<= word1 #xDFFF)))

  (define-inline (utf-16-compose-from-surrogate-pair word0 word1)
    (fx+ #x10000
	 (fxlogor (fxsll (fxand word0 #x3FF) 10)
		  (fxand word1 #x3FF))))


  #| end of module |# )


(module (get-u8 lookahead-u8)
  (import UNSAFE)

  (define (get-u8 p)
    (define who 'get-u8)
    (let ((m ($port-fast-attrs p)))
      (cond
       ((eq? m fast-get-byte-tag)
	(let ((i ($port-index p)))
	  (cond
	   ((fx< i ($port-size p))
	    ($set-port-index! p (fx+ i 1))
	    (bytevector-u8-ref ($port-buffer p) i))
	   (else (get-u8-byte-mode p who 1)))))
       (else (slow-get-u8 p who 1)))))

  (define (lookahead-u8 p)
    (define who 'lookahead-u8)
    (let ((m ($port-fast-attrs p)))
      (cond
       ((eq? m fast-get-byte-tag)
	(let ((i ($port-index p)))
	  (cond
	   ((fx< i ($port-size p))
	    (bytevector-u8-ref ($port-buffer p) i))
	   (else (get-u8-byte-mode p who 0)))))
       (else (slow-get-u8 p who 0)))))

  (define (get-u8-byte-mode p who start)
    (when ($port-closed? p) (die who "port is closed" p))
    (let ((cnt (%refill-bytevector-buffer p who)))
      (cond
       ((eqv? cnt 0) (eof-object))
       (else
	($set-port-index! p start)
	(bytevector-u8-ref ($port-buffer p) 0)))))

  (define (slow-get-u8 p who start)
    (%assert-binary-input-port p who)
    ($set-port-attrs! p fast-get-byte-tag)
    (get-u8-byte-mode p who start))

  (define (%assert-binary-input-port p who)
    (unless (port? p) (die who "not a port" p))
    (when ($port-closed? p) (die who "port is closed" p))
    (when ($port-transcoder p) (die who "port is not binary" p))
    (unless ($port-read! p)
      (die who "port is not an input port" p)))
  )


(define (port-eof? p)
  (import UNSAFE)
  (define who 'port-eof?)
  (let ((m ($port-fast-attrs p)))
    (cond ((not (eq? m 0))
	   (if (fx< ($port-index p) ($port-size p))
	       #f
	     (if ($port-transcoder p)
		 (eof-object? (lookahead-char p))
	       (eof-object? (lookahead-u8 p)))))
	  ((input-port? p)
	   (when ($port-closed? p)
	     (die who "port is closed" p))
	   (if (textual-port? p)
	       (eof-object? (lookahead-char p))
	     (eof-object? (lookahead-u8 p))))
	  (else
	   (die who "not an input port" p)))))


;;; FIXME: these hard coded constants should go away
(define EAGAIN-error-code -6) ;;; from ikarus-errno.c

(define io-error
  (case-lambda
   ((who id err base-condition)
    (raise
     (condition
      base-condition
      (make-who-condition who)
      (make-message-condition (strerror err))
      (case err
	;; from ikarus-errno.c: EACCES=-2, EFAULT=-21, EROFS=-71, EEXIST=-20,
	;;                      EIO=-29, ENOENT=-45
	;; Why is EFAULT included here?
	((-2 -21) (make-i/o-file-protection-error id))
	((-71)    (make-i/o-file-is-read-only-error id))
	((-20)    (make-i/o-file-already-exists-error id))
	((-29)    (make-i/o-error))
	((-45)    (make-i/o-file-does-not-exist-error id))
	(else (if id
		  (make-irritants-condition (list id))
		(condition)))))))
   ((who id err) (io-error who id err (make-error)))))

(define input-socket-buffer-size
  (make-parameter (+ input-block-size 128)
    (lambda (x)
      (import (ikarus system $fx))
      (if (and (fixnum? x) ($fx>= x 128))
	  x
	(error 'input-socket-buffer-size
	  "buffer size should be a fixnum >= 128"
	  x)))))

(define output-socket-buffer-size
  (make-parameter output-block-size
    (lambda (x)
      (import (ikarus system $fx))
      (if (and (fixnum? x) ($fx> x 0))
	  x
	(error 'output-socket-buffer-size
	  "buffer size should be a positive fixnum"
	  x)))))

(define (make-file-set-position-handler fd id)
  (lambda (pos) ;;; set-position!
    (let ((err (foreign-call "ikrt_set_position" fd pos)))
      (when err
	(io-error 'set-position! id err
		  (make-i/o-invalid-position-error pos))))))

(define (fh->input-port fd id size transcoder close who)
  ;;Given the file handle FD, as a fixnum, representing an opened
  ;;file for  the underlying platform: build and  return a Scheme
  ;;input port to be used to access the data.
  ;;
  (letrec
      ((port ($make-port (input-transcoder-attrs transcoder who) ;attrs
			 0			;index
			 0			;initial size
			 (make-bytevector size) ;buffer
			 transcoder		;transcoder
			 id			;port identifier
			 (letrec ((refill	;read! function
				   (lambda (bv idx cnt)
				     (import UNSAFE)
				     (let ((bytes
					    (foreign-call "ikrt_read_fd" fd bv idx
							  (if (fx< input-block-size cnt)
							      input-block-size
							    cnt))))
				       (cond
					((fx>= bytes 0) bytes)
					((fx= bytes EAGAIN-error-code)
					 (call/cc
					     (lambda (k)
					       (add-io-event fd k 'r)
					       (process-events)))
					 (refill bv idx cnt))
					(else
					 (io-error 'read id bytes
						   (make-i/o-read-error))))))))
			   refill)
			 #f ;write!
			 #t ;get-position
			 (make-file-set-position-handler fd id) ;set-position
			 (cond ((procedure? close) close) ;close
			       ((eqv? close #t) (file-close-proc id fd))
			       (else #f))
			 (default-cookie fd))))
    (guarded-port port)))


(define (fh->output-port fd id size transcoder close who)
  (letrec ((port
	    ($make-port
	     (output-transcoder-attrs transcoder who)
	     0 size (make-bytevector size)
	     transcoder
	     id
	     #f
	     (letrec ((refill
		       (lambda (bv idx cnt)
			 (import UNSAFE)
			 (let ((bytes
				(foreign-call "ikrt_write_fd" fd bv idx
					      (if (fx< output-block-size cnt)
						  output-block-size
						cnt))))

			   (cond
			    ((fx>= bytes 0) bytes)
			    ((fx= bytes EAGAIN-error-code)
			     (call/cc
				 (lambda (k)
				   (add-io-event fd k 'w)
				   (process-events)))
			     (refill bv idx cnt))
			    (else
			     (io-error 'write id bytes
				       (make-i/o-write-error))))))))
	       refill)
	     #t ;;; get-position
	     (make-file-set-position-handler fd id)
	     (cond
	      ((procedure? close) close)
	      ((eqv? close #t) (file-close-proc id fd))
	      (else #f))
	     (default-cookie fd))))
    (guarded-port port)))

(define (file-close-proc id fd)
  (lambda ()
    (cond
     ((foreign-call "ikrt_close_fd" fd) =>
      (lambda (err)
	(io-error 'close id err))))))


(define (open-input-file-handle filename who)
  (let ((fh (foreign-call "ikrt_open_input_fd"
			  (string->utf8 filename))))
    (cond
     ((fx< fh 0) (io-error who filename fh))
     (else fh))))

(define (open-output-file-handle filename file-options who)
  (define (opt->num x)
    (bitwise-ior
     (if (enum-set-member? 'no-create x)   1 0)
     (if (enum-set-member? 'no-fail x)     2 0)
     (if (enum-set-member? 'no-truncate x) 4 0)))
  (let ((opt (if (enum-set? file-options)
		 (opt->num file-options)
	       (die who "file-options is not an enum set"
		    file-options))))
    (let ((fh (foreign-call "ikrt_open_output_fd"
			    (string->utf8 filename)
			    opt)))
      (cond
       ((fx< fh 0) (io-error who filename fh))
       (else fh)))))

(define open-file-input-port
  (case-lambda
   ((filename)
    (open-file-input-port filename (file-options) 'block #f))
   ((filename file-options)
    (open-file-input-port filename file-options 'block #f))
   ((filename file-options buffer-mode)
    (open-file-input-port filename file-options buffer-mode #f))
   ((filename file-options buffer-mode transcoder)
    (define who 'open-file-input-port)
    (unless (string? filename)
      (die who "invalid filename" filename))
    (unless (enum-set? file-options)
      (die who "file-options is not an enum set" file-options))
    (unless (or (not transcoder) (transcoder? transcoder))
      (die who "invalid transcoder" transcoder))
		; FIXME: file-options ignored
		; FIXME: buffer-mode ignored
    (fh->input-port
     (open-input-file-handle filename who)
     filename
     input-file-buffer-size
     transcoder
     #t
     who))))

(define open-file-output-port
  (case-lambda
   ((filename)
    (open-file-output-port filename (file-options) 'block #f))
   ((filename file-options)
    (open-file-output-port filename file-options 'block #f))
   ((filename file-options buffer-mode)
    (open-file-output-port filename file-options buffer-mode #f))
   ((filename file-options buffer-mode transcoder)
    (define who 'open-file-output-port)
    (unless (string? filename)
      (die who "invalid filename" filename))
		; FIXME: file-options ignored
		; FIXME: line-buffered output ports are not handled
    (unless (or (not transcoder) (transcoder? transcoder))
      (die who "invalid transcoder" transcoder))
    (let ((buffer-size
	   (case buffer-mode
	     ((none) 0)
	     ((block line) output-file-buffer-size)
	     (else (die who "invalid buffer mode" buffer-mode)))))
      (fh->output-port
       (open-output-file-handle filename file-options who)
       filename buffer-size transcoder #t who)))))

(define (output-port-buffer-mode p)
  (unless (output-port? p)
    (die 'output-port-buffer-mode "not an output port" p))
  (if (fx= 0 ($port-size p)) 'none 'block))

(define (open-output-file filename)
  (unless (string? filename)
    (die 'open-output-file "invalid filename" filename))
  (fh->output-port
   (open-output-file-handle filename (file-options)
			    'open-output-file)
   filename
   output-file-buffer-size
   (native-transcoder)
   #t
   'open-output-file))

(define (open-input-file filename)
  (unless (string? filename)
    (die 'open-input-file "invalid filename" filename))
  (fh->input-port
   (open-input-file-handle filename 'open-input-file)
   filename
   input-file-buffer-size
   (native-transcoder)
   #t
   'open-input-file))


(define (with-output-to-file filename proc)
  (unless (string? filename)
    (die 'with-output-to-file "invalid filename" filename))
  (unless (procedure? proc)
    (die 'with-output-to-file "not a procedure" proc))
  (call-with-port
      (fh->output-port
       (open-output-file-handle filename (file-options)
				'with-output-to-file)
       filename
       output-file-buffer-size
       (native-transcoder)
       #t
       'with-output-to-file)
    (lambda (p)
      (parameterize ((current-output-port p))
	(proc)))))

(define (call-with-output-file filename proc)
  (unless (string? filename)
    (die 'call-with-output-file "invalid filename" filename))
  (unless (procedure? proc)
    (die 'call-with-output-file "not a procedure" proc))
  (call-with-port
      (fh->output-port
       (open-output-file-handle filename (file-options)
				'call-with-output-file)
       filename
       output-file-buffer-size
       (native-transcoder)
       #t
       'call-with-output-file)
    proc))

(define (call-with-input-file filename proc)
  (unless (string? filename)
    (die 'call-with-input-file "invalid filename" filename))
  (unless (procedure? proc)
    (die 'call-with-input-file "not a procedure" proc))
  (call-with-port
      (fh->input-port
       (open-input-file-handle filename 'call-with-input-file)
       filename
       input-file-buffer-size
       (native-transcoder)
       #t
       'call-with-input-file)
    proc))

(define (with-input-from-file filename proc)
  (unless (string? filename)
    (die 'with-input-from-file "invalid filename" filename))
  (unless (procedure? proc)
    (die 'with-input-from-file "not a procedure" proc))
  (call-with-port
      (fh->input-port
       (open-input-file-handle filename 'with-input-from-file)
       filename
       input-file-buffer-size
       (native-transcoder)
       #t
       'with-input-from-file)
    (lambda (p)
      (parameterize ((current-input-port p))
	(proc)))))

(define (with-input-from-string string proc)
  (unless (string? string)
    (die 'with-input-from-string "not a string" string))
  (unless (procedure? proc)
    (die 'with-input-from-string "not a procedure" proc))
  (parameterize ((current-input-port
		  (open-string-input-port string)))
    (proc)))


(define (standard-input-port)
  (fh->input-port 0 '*stdin* 256 #f #f 'standard-input-port))

(define (standard-output-port)
  (fh->output-port 1 '*stdout* 256 #f #f 'standard-output-port))

(define (standard-error-port)
  (fh->output-port 2 '*stderr* 256 #f #f 'standard-error-port))

(define current-input-port
  (make-parameter
      (transcoded-port
       (fh->input-port 0 '*stdin* input-file-buffer-size #f #f #f)
       (native-transcoder))
    (lambda (x)
      (if (and (input-port? x) (textual-port? x))
	  x
	(die 'current-input-port "not a textual input port" x)))))

(define current-output-port
  (make-parameter
      (transcoded-port
       (fh->output-port 1 '*stdout* output-file-buffer-size #f #f #f)
       (native-transcoder))
    (lambda (x)
      (if (and (output-port? x) (textual-port? x))
	  x
	(die 'current-output-port "not a textual output port" x)))))

(define current-error-port
  (make-parameter
      (transcoded-port
       (fh->output-port 2 '*stderr* 0 #f #f #f)
       (native-transcoder))
    (lambda (x)
      (if (and (output-port? x) (textual-port? x))
	  x
	(die 'current-errorput-port "not a textual output port" x)))))

(define console-output-port
  (let ((p (current-output-port)))
    (lambda () p)))

(define console-error-port
  (let ((p (current-error-port)))
    (lambda () p)))

(define console-input-port
  (let ((p (current-input-port)))
    (lambda () p)))

(define (call-with-port p proc)
  (define who 'call-with-port)
  (unless (port? p)
    (die who "not a port" p))
  (unless (procedure? proc)
    (die who "not a procedure" proc))
  (call-with-values
      (lambda ()
	(proc p))
    (lambda vals
      (close-port p)
      (apply values vals))))


(define (get-bytevector-n p n)
  (import (ikarus system $fx)
    (ikarus system $bytevectors))
  (define (subbytevector s n)
    (let ((p ($make-bytevector n)))
      (let f ((s s) (n n) (p p))
	(let ((n ($fx- n 1)))
	  ($bytevector-set! p n ($bytevector-u8-ref s n))
	  (if ($fx= n 0)
	      p
	    (f s n p))))))
  (unless (input-port? p)
    (die 'get-bytevector-n "not an input port" p))
  (unless (binary-port? p)
    (die 'get-bytevector-n "not a binary port" p))
  (unless (fixnum? n)
    (die 'get-bytevector-n "count is not a fixnum" n))
  (cond
   (($fx> n 0)
    (let ((s ($make-bytevector n)))
      (let f ((p p) (n n) (s s) (i 0))
	(let ((x (get-u8 p)))
	  (cond
	   ((eof-object? x)
	    (if ($fx= i 0)
		(eof-object)
	      (subbytevector s i)))
	   (else
	    ($bytevector-set! s i x)
	    (let ((i ($fxadd1 i)))
	      (if ($fx= i n)
		  s
		(f p n s i)))))))))
   (($fx= n 0) '#vu8())
   (else (die 'get-bytevector-n "count is negative" n))))

(define (get-bytevector-n! p s i c)
  (import (ikarus system $fx) (ikarus system $bytevectors))
  (unless (input-port? p)
    (die 'get-bytevector-n! "not an input port" p))
  (unless (binary-port? p)
    (die 'get-bytevector-n! "not a binary port" p))
  (unless (bytevector? s)
    (die 'get-bytevector-n! "not a bytevector" s))
  (let ((len ($bytevector-length s)))
    (unless (fixnum? i)
      (die 'get-bytevector-n! "starting index is not a fixnum" i))
    (when (or ($fx< i 0) ($fx> i len))
      (die 'get-bytevector-n!
	   (format "starting index is out of range 0..~a" len)
	   i))
    (unless (fixnum? c)
      (die 'get-bytevector-n! "count is not a fixnum" c))
    (cond
     (($fx> c 0)
      (let ((j (+ i c)))
	(when (> j len)
	  (die 'get-bytevector-n!
               (format "count is out of range 0..~a" (- len i))
               c))
	(let ((x (get-u8 p)))
	  (cond
	   ((eof-object? x) x)
	   (else
	    ($bytevector-set! s i x)
	    (let f ((p p) (s s) (start i) (i 1) (c c))
	      (cond
	       (($fx= i c) i)
	       (else
		(let ((x (get-u8 p)))
		  (cond
		   ((eof-object? x) i)
		   (else
		    ($bytevector-set! s ($fx+ start i) x)
		    (f p s start ($fx+ i 1) c))))))))))))
     (($fx= c 0) 0)
     (else (die 'get-bytevector-n! "count is negative" c)))))

(define (get-bytevector-some p)
  (define who 'get-bytevector-some)
		;    (import UNSAFE)
  (let ((m ($port-fast-attrs p)))
    (cond
     ((eq? m fast-get-byte-tag)
      (let ((i ($port-index p)) (j ($port-size p)))
	(let ((cnt (fx- j i)))
	  (cond
	   ((fx> cnt 0)
	    (let f ((bv (make-bytevector cnt))
		    (buf ($port-buffer p))
		    (i i) (j j) (idx 0))
	      (cond
	       ((fx= i j)
		($set-port-index! p j)
		bv)
	       (else
		(bytevector-u8-set! bv idx (bytevector-u8-ref buf i))
		(f bv buf (fx+ i 1) j (fx+ idx 1))))))
	   (else
	    (%refill-bytevector-buffer p who)
	    (if (fx= ($port-index p) ($port-size p))
		(eof-object)
	      (get-bytevector-some p)))))))
     (else (die who "invalid port argument" p)))))

(define (get-bytevector-all p)
  (define (get-it p)
    (let f ((p p) (n 0) (ac '()))
      (let ((x (get-u8 p)))
	(cond
	 ((eof-object? x)
	  (if (null? ac)
	      (eof-object)
	    (make-it n ac)))
	 (else (f p (+ n 1) (cons x ac)))))))
  (define (make-it n revls)
    (let f ((s (make-bytevector n)) (i (- n 1)) (ls revls))
      (cond
       ((pair? ls)
	(bytevector-u8-set! s i (car ls))
	(f s (- i 1) (cdr ls)))
       (else s))))
  (if (input-port? p)
      (if (binary-port? p)
	  (get-it p)
	(die 'get-bytevector-all "not a binary port" p))
    (die 'get-bytevector-all "not an input port" p)))


(define (get-string-n p n)
  (import (ikarus system $fx) (ikarus system $strings))
  (unless (input-port? p)
    (die 'get-string-n "not an input port" p))
  (unless (textual-port? p)
    (die 'get-string-n "not a textual port" p))
  (unless (fixnum? n)
    (die 'get-string-n "count is not a fixnum" n))
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
  (unless (input-port? p)
    (die 'get-string-n! "not an input port" p))
  (unless (textual-port? p)
    (die 'get-string-n! "not a textual port" p))
  (unless (string? s)
    (die 'get-string-n! "not a string" s))
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
  (import UNSAFE)
  (define (get-it p)
    (let f ((p p) (n 0) (ac '()))
      (let ((x (get-char p)))
	(cond
	 ((eqv? x #\newline)
	  (make-it n ac))
	 ((eof-object? x)
	  (if (null? ac) x (make-it n ac)))
	 (else (f p (fx+ n 1) (cons x ac)))))))
  (define (make-it n revls)
    (let f ((s (make-string n)) (i (fx- n 1)) (ls revls))
      (cond
       ((pair? ls)
	(string-set! s i (car ls))
	(f s (fx- i 1) (cdr ls)))
       (else s))))
  (if (input-port? p)
      (if (textual-port? p)
	  (get-it p)
	(die who "not a textual port" p))
    (die who "not an input port" p)))
(define (get-line p)
  ($get-line p 'get-line))
(define read-line
  (case-lambda
   (() ($get-line (current-input-port) 'read-line))
   ((p) ($get-line p 'read-line))))


(define (get-string-all p)
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
  (if (input-port? p)
      (if (textual-port? p)
	  (get-it p)
	(die 'get-string-all "not a textual port" p))
    (die 'get-string-all "not an input port" p)))


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
  (import UNSAFE)
  (define (put-byte! p b who)
    (let ((i ($port-index p)) (j ($port-size p)))
      (if (fx< i j)
	  (begin
	    (bytevector-u8-set! ($port-buffer p) i b)
	    ($set-port-index! p (fx+ i 1)))
	(if (fx= j 0)
	    (put-byte/unbuffered! p b who)
	  (begin
	    (flush-output-port p)
	    (put-byte! p b who))))))
  (define (put-char-utf8-mode p b who)
    (cond
     ((fx< b 128)
      (put-byte! p b who))
     ((fx<= b #x7FF)
      (put-byte! p (fxior #b11000000 (fxsra b 6)) who)
      (put-byte! p (fxior #b10000000 (fxand b #b111111)) who))
     ((fx<= b #xFFFF)
      (put-byte! p (fxior #b11100000 (fxsra b 12)) who)
      (put-byte! p (fxior #b10000000 (fxand (fxsra b 6) #b111111)) who)
      (put-byte! p (fxior #b10000000 (fxand b #b111111)) who))
     (else
      (put-byte! p (fxior #b11110000 (fxsra b 18)) who)
      (put-byte! p (fxior #b10000000 (fxand (fxsra b 12) #b111111)) who)
      (put-byte! p (fxior #b10000000 (fxand (fxsra b 6) #b111111)) who)
      (put-byte! p (fxior #b10000000 (fxand b #b111111)) who))))
    ;;;
  (define write-char
    (case-lambda
     ((c p) (do-put-char p c 'write-char))
     ((c) (do-put-char (current-output-port) c 'write-char))))
  (define (put-char p c)
    (do-put-char p c 'put-char))
  (define ($put-string p str start count)
    (unless (output-port? p)
      (die 'put-string "not an output port" p))
    (unless (textual-port? p)
      (die 'put-string "not a textual port" p))
    (let f ((i start) (j (fx+ start count)))
      (unless (fx= i j)
	(do-put-char p (string-ref str i) 'put-string)
	(f (fx+ i 1) j))))
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
	     ((fx< b 128)
	      (if (fx< i j)
		  (begin
		    (bytevector-u8-set! ($port-buffer p) i b)
		    ($set-port-index! p (fx+ i 1)))
		(if (fx= j 0)
		    (put-byte/unbuffered! p b who)
		  (begin
		    (flush-output-port p)
		    (put-byte! p b who)))))
	     (else
	      (put-char-utf8-mode p b who))))))
       ((eq? m fast-put-char-tag)
	(let ((i ($port-index p)) (j ($port-size p)))
	  (if (fx< i j)
	      (begin
		(string-set! ($port-buffer p) i c)
		($set-port-index! p (fx+ i 1)))
	    (if (fx= j 0)
		(put-char/unbuffered! p c who)
	      (begin
		(flush-output-port p)
		(do-put-char p c who))))))
       ((eq? m fast-put-latin-tag)
	(let ((i ($port-index p)) (j ($port-size p)))
	  (let ((b (char->integer c)))
	    (cond
	     ((fx< b 256)
	      (if (fx< i j)
		  (begin
		    (bytevector-u8-set! ($port-buffer p) i b)
		    ($set-port-index! p (fx+ i 1)))
		(if (fx= j 0)
		    (put-byte/unbuffered! p b who)
		  (begin
		    (flush-output-port p)
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
	   ((fx< n #x10000)
	    (put-byte! p (fxsra n 8) who)
	    (put-byte! p (fxand n #xFF) who))
	   (else
	    (let ((u^ (fx- n #x10000)))
	      (let ((w1 (fxior #xD800 (fxsra u^ 10))))
		(put-byte! p (fxsra w1 8) who)
		(put-byte! p (fxand w1 #xFF) who))
	      (let ((w2 (fxior #xDC00 (fxand u^ (- (fxsll 1 10) 1)))))
		(put-byte! p (fxsra w2 8) who)
		(put-byte! p (fxand w2 #xFF) who)))))))
       (else
	(if (output-port? p)
	    (if (textual-port? p)
		(if (port-closed? p)
		    (die who "port is closed" p)
		  (die who "unsupported port" p))
	      (die who "not a textual port" p))
	  (die who "not an output port" p)))))))


(define newline
  (case-lambda
   (()
    (put-char (current-output-port) #\newline)
    (flush-output-port (current-output-port)))
   ((p)
    (unless (output-port? p)
      (die 'newline "not an output port" p))
    (unless (textual-port? p)
      (die 'newline "not a textual port" p))
    (when ($port-closed? p)
      (die 'newline "port is closed" p))
    (put-char p #\newline)
    (flush-output-port p))))


(define (%spawn-process who search? blocking? env stdin stdout stderr cmd args)
  (define (port->fd port port-pred arg-name port-type)
    (cond ((eqv? port #f) -1)
	  ((port-pred port)
	   (let ((fd (cookie-dest ($port-cookie port))))
	     (unless (fixnum? fd)
	       (die who
		    (string-append arg-name " is not a file-based port")
		    stdin))
	     fd))
	  (else
	   (die who
		(string-append arg-name " is neither false nor an " port-type)
		stdin))))

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
	     (io-error who cmd r))
	    (else
	     (unless blocking?
	       (or stdin (set-fd-nonblocking (vector-ref r 1) who cmd))
	       (or stdout (set-fd-nonblocking (vector-ref r 2) who cmd))
	       (or stderr (set-fd-nonblocking (vector-ref r 3) who cmd)))
	     (values
	      (vector-ref r 0)        ; pid
	      (and (not stdin)
		   (fh->output-port (vector-ref r 1)
				    cmd output-file-buffer-size #f #t
				    'process))
	      (and (not stdout)
		   (fh->input-port (vector-ref r 2)
				   cmd input-file-buffer-size #f #t
				   'process))
	      (and (not stderr)
		   (fh->input-port (vector-ref r 3)
				   cmd input-file-buffer-size #f #t
				   'process))))))))

(define (process cmd . args)
  (%spawn-process 'process #t #t #f #f #f #f cmd args))

(define (process* search? env stdin stdout stderr cmd . args)
  (%spawn-process 'process* search? #t env stdin stdout stderr cmd args))

(define (process-nonblocking cmd . args)
  (%spawn-process 'process-nonblocking #t #f #f #f #f cmd args))


(define (set-fd-nonblocking fd who id)
  (let ((rv (foreign-call "ikrt_make_fd_nonblocking" fd)))
    (unless (eq? rv 0)
      (io-error who id rv))))

(define (socket->ports socket who id block?)
  (if (< socket 0)
      (io-error who id socket)
    (let ((close
	   (let ((closed-once? #f))
	     (lambda ()
	       (if closed-once?
		   ((file-close-proc id socket))
		 (set! closed-once? #t))))))
      (unless block?
	(set-fd-nonblocking socket who id))
      (values
       (fh->input-port socket
		       id (input-socket-buffer-size) #f close who)
       (fh->output-port socket
			id (output-socket-buffer-size) #f close who)))))

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
	      (io-error 'select #f rv)))
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
  )


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
	(io-error who s sock))
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
  (if (input-port? p)
      (begin
	($set-port-index! p ($port-size p))
	(unregister-callback p))
    (die 'reset-input-port! "not an input port" p)))

(define (reset-output-port! p)
  (if (output-port? p)
      (begin
	($set-port-index! p 0)
	(unregister-callback p))
    (die 'reset-output-port! "not an output port" p)))

(define (unregister-callback what)
  (define who 'unregister-callback)
  (cond
   ((output-port? what)
    (let ((c (cookie-dest ($port-cookie what))))
      (unless (fixnum? c) (die who "not a file-based port" what))
      (rem-io-event c)))
   ((input-port? what)
    (let ((c (cookie-dest ($port-cookie what))))
      (unless (fixnum? c) (die who "not a file-based port" what))
      (rem-io-event c)))
   ((tcp-server? what)
    (rem-io-event (tcp-server-fd what)))
   (else (die who "invalid argument" what))))

(define (register-callback what proc)
  (define who 'register-callback)
  (unless (procedure? proc)
    (die who "not a procedure" proc))
  (cond
   ((output-port? what)
    (let ((c (cookie-dest ($port-cookie what))))
      (unless (fixnum? c) (die who "not a file-based port" what))
      (add-io-event c proc 'w)))
   ((input-port? what)
    (let ((c (cookie-dest ($port-cookie what))))
      (unless (fixnum? c) (die who "not a file-based port" what))
      (add-io-event c proc 'r)))
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
	  (io-error who filename rv)
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
	(io-error who (directory-stream-filename x) rv))
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
	    (io-error who (directory-stream-filename x) rv)))))
     ((x) (close-directory-stream x #t))))

  (set-rtd-printer! (type-descriptor directory-stream)
		    (lambda (x p wr)
		      (fprintf p "#<directory-stream ~a>"
			       (directory-stream-filename x)))))


		;(set-fd-nonblocking 0 'init '*stdin*)

;;;; done

)

;;; end of file
;;; Local Variables:
;;; fill-column: 65
;;; eval: (put 'with-port				'scheme-indent-function 1)
;;; eval: (put 'with-binary-port			'scheme-indent-function 1)
;;; eval: (put 'with-textual-port			'scheme-indent-function 1)
;;; eval: (put 'case-textual-input-port-fast-tag	'scheme-indent-function 1)
;;; eval: (put 'refill-buffer-and-evaluate		'scheme-indent-function 1)
;;; eval: (put 'maybe-refill-buffer-and-evaluate	'scheme-indent-function 1)
;;; End:
