;;;Copyright (c) 2011-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; byte input functions

(module (get-u8 lookahead-u8)

  (define* (get-u8 port)
    ;;Defined by  R6RS, extended by  Vicare.  Read from  the binary input  port PORT,
    ;;blocking as necessary, until  a byte is available from PORT or  until an end of
    ;;file is reached.
    ;;
    ;;Return the EOF object, the would-block object or a fixnum:
    ;;
    ;;* If a byte is available: return the  byte as an octet and update PORT to point
    ;;just past that byte.
    ;;
    ;;* If no input byte is seen before an  end of file is reached: the EOF object is
    ;;returned.
    ;;
    ;;* If the underlying device is in  non-blocking mode and no bytes are available:
    ;;return the would-block object.
    ;;
    (%case-binary-input-port-fast-tag (port __who__)
      ((FAST-GET-BYTE-TAG)
       (with-port-having-bytevector-buffer (port)
	 (let ((buffer.offset port.buffer.index))
	   (if ($fx< buffer.offset port.buffer.used-size)
	       ;;Here we handle the case of  byte already available in the buffer, if
	       ;;the buffer is empty: we call a subroutine.
	       (begin
		 (set! port.buffer.index ($fxadd1 buffer.offset))
		 ($bytevector-u8-ref port.buffer buffer.offset))
	     (%get/peek-u8-from-device port __who__ 1)))))))

  (define* (lookahead-u8 port)
    ;;Defined by  R6RS.  The LOOKAHEAD-U8 procedure  is like GET-U8, but  it does not
    ;;update PORT to point past the byte.
    ;;
    (%case-binary-input-port-fast-tag (port __who__)
      ((FAST-GET-BYTE-TAG)
       (with-port-having-bytevector-buffer (port)
	 (let ((buffer.offset port.buffer.index))
	   (if ($fx< buffer.offset port.buffer.used-size)
	       ;;Here we handle the case of  byte already available in the buffer, if
	       ;;the buffer is empty: we call a subroutine.
	       ($bytevector-u8-ref port.buffer buffer.offset)
	     (%get/peek-u8-from-device port __who__ 0)))))))

  (define (%get/peek-u8-from-device port who buffer.offset-after)
    ;;Subroutine of  GET-U8 and LOOKAHEAD-U8.  To  be called when the  port buffer is
    ;;fully consumed.  Get or  peek the next byte from PORT, set  the buffer index to
    ;;BUFFER.OFFSET-AFTER.
    ;;
    (with-port-having-bytevector-buffer (port)
      (debug-assert (fx= port.buffer.index port.buffer.used-size))
      (let retry-after-would-block ()
	(refill-bytevector-buffer-and-evaluate (port who)
	  (if-end-of-file: (eof-object))
	  (if-refilling-would-block:
	   (if (strict-r6rs)
	       (retry-after-would-block)
	     WOULD-BLOCK-OBJECT))
	  (if-successful-refill:
	   (set! port.buffer.index buffer.offset-after)
	   ($bytevector-u8-ref port.buffer 0))
	  ))))

  #| end of module |# )

(module (lookahead-two-u8)
  ;;Defined  by Vicare.   Like LOOKAHEAD-U8  but  peeks at  2 octets  and return  two
  ;;values: EOF, would-block  or a fixnum representing first  octet; EOF, would-block
  ;;or a fixnum representing the second octet.
  ;;
  (define* (lookahead-two-u8 port)
    (%case-binary-input-port-fast-tag (port __who__)
      ((FAST-GET-BYTE-TAG)
       (with-port-having-bytevector-buffer (port)
	 (let* ((buffer.offset-octet0	port.buffer.index)
		(buffer.offset-octet1	($fxadd1 buffer.offset-octet0))
		(buffer.offset-past	($fxadd1 buffer.offset-octet1)))
	   (if ($fx<= buffer.offset-past port.buffer.used-size)
	       ;;There are 2 or more bytes available.
	       (values ($bytevector-u8-ref port.buffer buffer.offset-octet0)
		       ($bytevector-u8-ref port.buffer buffer.offset-octet1))
	     ;;There are 0 bytes or 1 byte available.
	     (let retry-after-would-block ()
	       (refill-bytevector-buffer-and-evaluate (port __who__)
		 (if-end-of-file:
		  (%maybe-some-data-is-available port 0))
		 (if-refilling-would-block:
		  (receive (rv1 rv2)
		      (%maybe-some-data-is-available port 1)
		    (if (and (strict-r6rs)
			     (or (would-block-object? rv2)
				 (would-block-object? rv1)))
			(retry-after-would-block)
		      (values rv1 rv2))))
		 (if-successful-refill:
		  (receive (rv1 rv2)
		      (%maybe-some-data-is-available port 2)
		    (if (and (strict-r6rs)
			     (or (would-block-object? rv2)
				 (would-block-object? rv1)))
			(retry-after-would-block)
		      (values rv1 rv2))))))))))))

  (define (%maybe-some-data-is-available port mode)
    (with-port-having-bytevector-buffer (port)
      (let* ((buffer.offset-octet0	port.buffer.index)
	     (buffer.offset-octet1	($fxadd1 buffer.offset-octet0))
	     (buffer.offset-past	($fxadd1 buffer.offset-octet1)))
	(cond (($fx<= buffer.offset-past port.buffer.used-size)
	       (values ($bytevector-u8-ref port.buffer buffer.offset-octet0)
		       ($bytevector-u8-ref port.buffer buffer.offset-octet1)))
	      (($fx<= buffer.offset-octet1 port.buffer.used-size)
	       (values ($bytevector-u8-ref port.buffer buffer.offset-octet0)
		       (if ($fx= mode 0)
			   (eof-object)
			 WOULD-BLOCK-OBJECT)))
	      (else
	       (let ((rv (if ($fx= mode 0)
			     (eof-object)
			   WOULD-BLOCK-OBJECT)))
		 (values rv rv)))))))

  #| end of module: LOOKAHEAD-TWO-U8 |# )


;;;; bytevector input functions: GET-BYTEVECTOR-N

(module (get-bytevector-n)
  ;;Defined  by R6RS,  extended  by Vicare.   COUNT must  be  an exact,  non-negative
  ;;integer object representing the number of bytes to be read.
  ;;
  ;;The  GET-BYTEVECTOR-N procedure  reads from  the binary  input PORT,  blocking as
  ;;necessary, until COUNT bytes  are available from PORT or until an  end of file is
  ;;reached.
  ;;
  ;;Return the EOF object, the would-block object or a bytevector:
  ;;
  ;;* If COUNT bytes are available before an end of file: return a bytevector of size
  ;;COUNT.  The input port is updated to point just past the bytes read.
  ;;
  ;;*  If fewer  bytes are  available  before an  end  of file:  return a  bytevector
  ;;containing those bytes.  The  input port is updated to point  just past the bytes
  ;;read.
  ;;
  ;;* If  an end of file  is reached before any  bytes are available: return  the EOF
  ;;object.
  ;;
  ;;* If the underlying device is in non-blocking mode and fewer than COUNT bytes are
  ;;available: return a bytevector containing those bytes.  The input port is updated
  ;;to point just past the bytes read.
  ;;
  ;;* If the  underlying device is in  non-blocking mode and no  bytes are available:
  ;;return the would-block object.
  ;;
  ;;IMPLEMENTATION RESTRICTION The COUNT argument must be a fixnum.
  ;;
  (define-module-who get-bytevector-n)

  (define* (get-bytevector-n port {count fixnum-count?})
    (%case-binary-input-port-fast-tag (port __who__)
      ((FAST-GET-BYTE-TAG)
       (if (zero? count)
	   (quote #vu8())
	 (%consume-bytes port count)))))

  (define (%consume-bytes port requested-count)
    ;;To be  called when the  request must be satisfied  by consuming bytes  from the
    ;;input buffer and maybe from the  underlying device.  Return EOF or a bytevector
    ;;representing the read bytes.
    ;;
    (with-port-having-bytevector-buffer (port)
      (let retry-after-filling-buffer ((output.len       0)
				       (output.bvs       '())
				       (remaining-count  requested-count))
	(define (data-available)
	  (%data-available-in-buffer port output.len remaining-count output.bvs
				     retry-after-filling-buffer))
	(define (compose-output)
	  ($bytevector-reverse-and-concatenate output.len output.bvs))
	(maybe-refill-bytevector-buffer-and-evaluate (port __module_who__)
	  (data-is-needed-at:    port.buffer.index)
	  ;;The buffer  was empty and  after attempting to  refill it: EOF  was found
	  ;;without available data.
	  (if-end-of-file: (if (zero? output.len)
			       (eof-object)
			     (compose-output)))
	  ;;Attempting to refill the buffer caused an "&i/o-eagain" exception.  If we
	  ;;are here:  the buffer is  now empty, but  maybe some data  was available,
	  ;;even though not enough to satisfy the full request.
	  ;;
	  ;;If some  data is available  just return  it, else return  the would-block
	  ;;object.
	  (if-empty-buffer-and-refilling-would-block:
	   (if (zero? output.len)
	       (if (strict-r6rs)
		   (retry-after-filling-buffer output.len output.bvs remaining-count)
		 WOULD-BLOCK-OBJECT)
	     (compose-output)))
	  ;;The buffer was empty, but after refilling it: there is available data.
	  (if-successful-refill: (data-available))
	  ;;The buffer has available data without reading from the device.
	  (if-available-data:    (data-available))
	  ))))

  (define (%data-available-in-buffer port output.len remaining-count output.bvs
				     retry-after-filling-buffer)
    ;;To be called  when data is available  in the input buffer.   Consume bytes from
    ;;the input buffer to satisfy the request:
    ;;
    ;;* If these bytes are enough: compose the output bytevector and return it.
    ;;
    ;;* If these bytes are not enough: call the function RETRY-AFTER-FILLING-BUFFER.
    ;;
    (with-port-having-bytevector-buffer (port)
      (define-constant count-of-bytes-available-in-buffer
	($fx- port.buffer.used-size port.buffer.index))
      (define-constant bytes-from-buffer-satisfy-the-request?
	($fx<= remaining-count count-of-bytes-available-in-buffer))
      (define-constant amount-to-consume-from-buffer
	(if bytes-from-buffer-satisfy-the-request?
	    remaining-count
	  count-of-bytes-available-in-buffer))
      (define-constant output.len/after-consuming-buffer-bytes
	(let ((N (+ output.len amount-to-consume-from-buffer)))
	  ;;DANGER This check must not be removed when compiling without
	  ;;arguments validation.
	  (if (fixnum? N)
	      N
	    (%implementation-violation __module_who__
	      "request to read data from port would exceed maximum size of bytevectors" N))))
      (let ((bv ($make-bytevector amount-to-consume-from-buffer)))
	;;Consume data from the input buffer into BV.
	(begin
	  ($bytevector-copy!/count port.buffer port.buffer.index
				   bv          0
				   amount-to-consume-from-buffer)
	  (set! port.buffer.index ($fx+ port.buffer.index
					amount-to-consume-from-buffer)))
	(let ((output.bvs/after-consuming-buffer-bytes (cons bv output.bvs)))
	  (if bytes-from-buffer-satisfy-the-request?
	      ;;Compose output.
	      ($bytevector-reverse-and-concatenate output.len/after-consuming-buffer-bytes
						   output.bvs/after-consuming-buffer-bytes)
	    (retry-after-filling-buffer output.len/after-consuming-buffer-bytes
					output.bvs/after-consuming-buffer-bytes
					($fx- remaining-count
					      count-of-bytes-available-in-buffer)))))))

  #| end of module: GET-BYTEVECTOR-N |# )


;;;; bytevector input functions: GET-BYTEVECTOR-N!

(module (get-bytevector-n!)
  ;;Defined  by R6RS,  extended  by Vicare.   COUNT must  be  an exact,  non-negative
  ;;integer object, representing  the number of bytes  to be read.  DST.BV  must be a
  ;;bytevector with at least DST.START+COUNT elements.
  ;;
  ;;The GET-BYTEVECTOR-N!   procedure reads from  the binary input PORT,  blocking as
  ;;necessary, until COUNT bytes are available or until an end of file is reached.
  ;;
  ;;Return the EOF object,  the would-block object or the number  of bytes written in
  ;;the given bytevector:
  ;;
  ;;* If  COUNT bytes are  available before  the end of  file: they are  written into
  ;;DST.BV starting at index  DST.START, and the result is COUNT.   The input port is
  ;;updated to point just past the bytes read.
  ;;
  ;;* If fewer  bytes are available before  the end of file: the  available bytes are
  ;;written  into DST.BV  starting at  index DST.START,  and the  result is  a number
  ;;object representing the number of bytes actually read.  The input port is updated
  ;;to point just past the bytes read.
  ;;
  ;;* If the  end of file is reached  before any bytes are available:  return the EOF
  ;;object.
  ;;
  ;;* If the underlying device is in non-blocking mode and fewer than COUNT bytes are
  ;;available:  the  available  bytes  are  written into  DST.BV  starting  at  index
  ;;DST.START, and  the result is  a number object  representing the number  of bytes
  ;;actually read.  The input port is updated to point just past the bytes read.
  ;;
  ;;* If the  underlying device is in  non-blocking mode and no  bytes are available:
  ;;return the would-block object.
  ;;
  ;;IMPLEMENTATION RESTRICTION The COUNT argument must be a fixnum.
  ;;
  (define-module-who get-bytevector-n!)

  (define* (get-bytevector-n! port {dst.bv bytevector?} {dst.start fixnum-start-index?} {count fixnum-count?})
    (assert-start-index-for-bytevector dst.bv dst.start)
    (assert-count-from-start-index-for-bytevector dst.bv dst.start count)
    (%case-binary-input-port-fast-tag (port __who__)
      ((FAST-GET-BYTE-TAG)
       (if ($fxzero? count)
	   count
	 (%consume-bytes port dst.bv dst.start count)))))

  (define (%consume-bytes port dst.bv dst.start requested-count)
    (with-port-having-bytevector-buffer (port)
      (let retry-after-filling-buffer
	  ((dst.index        dst.start)
	   (remaining-count  requested-count))
	(define (data-available)
	  (%data-available-in-buffer port dst.bv dst.start dst.index remaining-count
				     retry-after-filling-buffer))
	(maybe-refill-bytevector-buffer-and-evaluate (port __module_who__)
	  (data-is-needed-at: port.buffer.index)
	  (if-end-of-file:
	   (if ($fx= dst.index dst.start)
	       (eof-object)
	     ($fx- dst.index dst.start)))
	  (if-empty-buffer-and-refilling-would-block:
	   (if ($fx= dst.index dst.start)
	       (if (strict-r6rs)
		   (retry-after-filling-buffer dst.index remaining-count)
		 WOULD-BLOCK-OBJECT)
	     ($fx- dst.index dst.start)))
	  (if-successful-refill:
	   (data-available))
	  (if-available-data:
	   (data-available))
	  ))))

  (define (%data-available-in-buffer port dst.bv dst.start dst.index remaining-count
				     retry-after-filling-buffer)
    ;;To be called when data is available in the buffer.
    ;;
    (with-port-having-bytevector-buffer (port)
      (define-constant count-of-bytes-available-in-buffer
	($fx- port.buffer.used-size port.buffer.index))
      (define-constant bytes-from-buffer-satisfy-the-request?
	($fx<= remaining-count count-of-bytes-available-in-buffer))
      (define-constant amount-to-consume-from-buffer
	(if bytes-from-buffer-satisfy-the-request?
	    remaining-count
	  count-of-bytes-available-in-buffer))
      ($bytevector-copy!/count port.buffer port.buffer.index
			       dst.bv      dst.index
			       amount-to-consume-from-buffer)
      (set! port.buffer.index ($fx+ port.buffer.index amount-to-consume-from-buffer))
      (set! dst.index         ($fx+ dst.index amount-to-consume-from-buffer))
      (if bytes-from-buffer-satisfy-the-request?
	  ($fx- dst.index dst.start)
	(retry-after-filling-buffer dst.index
				    ($fx- remaining-count amount-to-consume-from-buffer)))))

  #| end of module: GET-BYTEVECTOR-N! |# )


;;;; bytevector input functions: GET-BYTEVECTOR-SOME

(module (get-bytevector-some)
  ;;Defined by R6RS,  extended by Vicare.  Read from the  binary input PORT, blocking
  ;;as necessary, until bytes are available or until an end of file is reached.
  ;;
  ;;Return the EOF object, the would-block object or a bytevector:
  ;;
  ;;* If  bytes are available: return  a freshly allocated bytevector  containing the
  ;;initial available bytes (at least one), and  update PORT to point just past these
  ;;bytes.
  ;;
  ;;* If no input bytes are seen before an  end of file is reached: the EOF object is
  ;;returned.
  ;;
  ;;* If the  underlying device is in  non-blocking mode and no  bytes are available:
  ;;return the would-block object.
  ;;
  (define* (get-bytevector-some port)
    (%case-binary-input-port-fast-tag (port __who__)
      ((FAST-GET-BYTE-TAG)
       (with-port-having-bytevector-buffer (port)
	 (let retry-after-would-block ()
	   (maybe-refill-bytevector-buffer-and-evaluate (port __who__)
	     (data-is-needed-at: port.buffer.index)
	     (if-end-of-file:
	      (eof-object))
	     (if-empty-buffer-and-refilling-would-block:
	      (if (strict-r6rs)
		  (retry-after-would-block)
		WOULD-BLOCK-OBJECT))
	     (if-successful-refill:
	      (%data-available-in-buffer port))
	     (if-available-data:
	      (%data-available-in-buffer port))
	     ))))))

  (define (%data-available-in-buffer port)
    (with-port-having-bytevector-buffer (port)
      (define-constant count-of-bytes-available-in-buffer
	($fx- port.buffer.used-size port.buffer.index))
      (define-constant dst.bv
	($make-bytevector count-of-bytes-available-in-buffer))
      ($bytevector-copy!/count port.buffer port.buffer.index
			       dst.bv      0
			       count-of-bytes-available-in-buffer)
      (set! port.buffer.index port.buffer.used-size)
      dst.bv))

  #| end of module: GET-BYTEVECTOR-SOME |# )


;;;; bytevector input functions: GET-BYTEVECTOR-ALL

(module (get-bytevector-all)
  ;;Defined by R6RS, modified  by Vicare.  Attempts to read all  bytes until the next
  ;;end of file, blocking as necessary.
  ;;
  ;;If  the  underlying  device  is  in   blocking  mode:  the  operation  may  block
  ;;indefinitely waiting  to see if  more bytes will  become available, even  if some
  ;;bytes are already available.
  ;;
  ;;Return the EOF object, the would-block object or a bytevector:
  ;;
  ;;* If one or  more bytes are read: return a bytevector containing  all bytes up to
  ;;the next end of file.
  ;;
  ;;* If no bytes are available and the end-of-file is found: return the EOF object.
  ;;
  ;;Even  when  the underlying  device  is  in non-blocking  mode  and  no bytes  are
  ;;available: this function attempts to read input until the EOF is found.
  ;;
  (define-module-who get-bytevector-all)

  (define* (get-bytevector-all port)
    (%case-binary-input-port-fast-tag (port __who__)
      ((FAST-GET-BYTE-TAG)
       (with-port-having-bytevector-buffer (port)
	 (let retry-after-filling-buffer ((output.len  0)
					  (output.bvs  '()))
	   (define-syntax-rule (compose-output)
	     ($bytevector-reverse-and-concatenate output.len output.bvs))
	   (define-syntax-rule (data-available)
	     (%data-available-in-buffer port output.len output.bvs
					retry-after-filling-buffer))
	   (maybe-refill-bytevector-buffer-and-evaluate (port __who__)
	     (data-is-needed-at: port.buffer.index)
	     (if-end-of-file:
	      (if (zero? output.len)
		  (eof-object)
		(compose-output)))
	     (if-empty-buffer-and-refilling-would-block:
	      (retry-after-filling-buffer output.len output.bvs))
	     (if-successful-refill: (data-available))
	     (if-available-data:    (data-available))
	     ))))))

  (define (%data-available-in-buffer port output.len output.bvs
				     retry-after-filling-buffer)
    (with-port-having-bytevector-buffer (port)
      (define-constant count-of-bytes-available-in-buffer
	($fx- port.buffer.used-size port.buffer.index))
      (define-constant output.len/after-consuming-buffer-bytes
	(let ((N (+ output.len count-of-bytes-available-in-buffer)))
	  ;;DANGER This  check must not  be removed when compiling  without arguments
	  ;;validation.
	  (if (fixnum? N)
	      N
	    (%implementation-violation __module_who__
	      "request to read data from port would exceed maximum size of bytevectors" N))))
      (define-constant dst.bv
	($make-bytevector count-of-bytes-available-in-buffer))
      ($bytevector-copy!/count port.buffer port.buffer.index
			       dst.bv      0
			       count-of-bytes-available-in-buffer)
      (set! port.buffer.index port.buffer.used-size)
      (retry-after-filling-buffer output.len/after-consuming-buffer-bytes
				  (cons dst.bv output.bvs))))

  #| end of module: GET-BYTEVECTOR-ALL |# )


;;;; single-character input

(module (get-char read-char lookahead-char peek-char)

  (define* (get-char port)
    ;;Defined  by R6RS,  extended  by  Vicare.  Read  from  the  textual input  PORT,
    ;;blocking as necessary, until a complete character is available, or until an end
    ;;of file is reached, or until a would-block condition is reached.
    ;;
    ;;* If a complete character is available before the next end of file: return that
    ;;character and update the input port to point past the character.
    ;;
    ;;* If  an end of file  is reached before any  character is read: return  the EOF
    ;;object.
    ;;
    ;;* If  the underlying device  is in non-blocking mode  and no full  character is
    ;;available: return the would-block object.
    ;;
    (%do-read-char port __who__))

  (case-define* read-char
    ;;Defined by R6RS.   Read from textual input PORT, blocking  as necessary until a
    ;;character is available, or  the data that is available cannot  be the prefix of
    ;;any valid encoding, or an end of file is reached, or a would-block condition is
    ;;reached.
    ;;
    ;;* If a complete character is available before the next end of file: return that
    ;;character and update the input port to point past that character.
    ;;
    ;;* If an end of file is reached before any data are read: return the EOF object.
    ;;
    ;;* If  the underlying device  is in non-blocking mode  and no full  character is
    ;;available: return the would-block object.
    ;;
    ;;If PORT is omitted, it defaults to the value returned by CURRENT-INPUT-PORT.
    ;;
    ((port)
     (%do-read-char port __who__))
    (()
     (%do-read-char (current-input-port) __who__)))

;;; --------------------------------------------------------------------

  (define* (lookahead-char port)
    ;;Defined  by R6RS,  extended by  Vicare.  The  LOOKAHEAD-CHAR procedure  is like
    ;;GET-CHAR, but it does  not update PORT to point past  the character.  PORT must
    ;;be a textual input port.
    ;;
    (%do-peek-char port __who__))

  (case-define* peek-char
    ;;Defined by R6RS, extended  by Vicare.  This is the same  as READ-CHAR, but does
    ;;not consume any data from the port.
    ;;
    (()
     (%do-peek-char (current-input-port) __who__))
    ((port)
     (%do-peek-char port __who__)))

;;; --------------------------------------------------------------------

  (define (%do-read-char port who)
    ;;Subroutine  of GET-CHAR  and  READ-CHAR.   Read from  the  textual input  PORT,
    ;;blocking as necessary, until a complete character is available, or until an end
    ;;of file is reached, or until a would-block condition is reached.
    ;;
    ;;* If a complete character is available before the next end of file: return that
    ;;character and update the input port to point past the character.
    ;;
    ;;* If  an end of file  is reached before any  character is read: return  the EOF
    ;;object.
    ;;
    ;;* If  the underlying device  is in non-blocking mode  and no full  character is
    ;;available: return the would-block object.
    ;;
    (define (main)
      (%case-textual-input-port-fast-tag (port who)
	((FAST-GET-UTF8-TAG)
	 (%read-it 1
		   %unsafe.read-char-from-port-with-fast-get-utf8-tag
		   %unsafe.peek-char-from-port-with-fast-get-utf8-tag
		   %unsafe.peek-char-from-port-with-utf8-codec))
	((FAST-GET-CHAR-TAG)
	 (%read-it 1
		   %unsafe.read-char-from-port-with-fast-get-char-tag
		   %peek-char
		   %peek-char/offset))
	((FAST-GET-LATIN-TAG)
	 (%read-it 1
		   %unsafe.read-char-from-port-with-fast-get-latin1-tag
		   %peek-latin1
		   %peek-latin1/offset))
	((FAST-GET-UTF16LE-TAG)
	 (%read-it 2
		   %read-utf16le
		   %peek-utf16le
		   %peek-utf16le/offset))
	((FAST-GET-UTF16BE-TAG)
	 (%read-it 2
		   %read-utf16be
		   %peek-utf16be
		   %peek-utf16be/offset))))

    (define-syntax-rule (%read-it ?offset-of-ch2 ?read-char-proc
				  ?peek-char-proc ?peek-char/offset-proc)
      ;;Actually  perform the  reading.  Return  the next  char and  update the  port
      ;;position to point  past the char.  If no characters  are available return the
      ;;EOF object  or the would-block object.   Remember that, as mandated  by R6RS,
      ;;for input ports every line-ending sequence of characters must be converted to
      ;;linefeed when the EOL style is not NONE.
      ;;
      ;;?READ-CHAR-PROC  must be  the identifier  of a  macro performing  the reading
      ;;operation  for the  next available  character; it  is used  to read  the next
      ;;single  char,  which  might be  the  first  char  in  a sequence  of  2-chars
      ;;line-ending.
      ;;
      ;;?PEEK-CHAR-PROC must  be the identifier  of a macro performing  the lookahead
      ;;operation  for the  next available  character; it  is used  to peek  the next
      ;;single  char,  which  might be  the  first  char  in  a sequence  of  2-chars
      ;;line-ending.
      ;;
      ;;?PEEK-CHAR/OFFSET-PROC  must be  the  identifier of  a  macro performing  the
      ;;forward lookahead operation for the second character in a sequence of 2-chars
      ;;line-ending.
      ;;
      ;;?OFFSET-OF-CH2 is  the offset  of the  second char in  a sequence  of 2-chars
      ;;line-ending; for  ports having bytevector  buffer: it is expressed  in bytes;
      ;;for ports having string buffer:  it is expressed in characters.  Fortunately:
      ;;2-chars line-ending sequences always have a carriage return as first char and
      ;;we know, once the codec has been selected, the offset of such character.
      ;;
      (let retry-after-would-block ()
	(let ((ch (?peek-char-proc port who)))
	  (cond
	   ;;EOF without available characters: return the EOF object.
	   ;;
	   ;;Notice that: the peek char operation performed above can return EOF even
	   ;;when  there are  bytes in  the  input buffer;  it happens  when all  the
	   ;;following conditions are met:
	   ;;
	   ;;1. The bytes do *not* represent a valid Unicode character in the context
	   ;;of the selected codec.
	   ;;
	   ;;2. The transcoder has ERROR-HANDLING-MODE set to "ignore".
	   ;;
	   ;;3. The invalid bytes are followed by an EOF.
	   ;;
	   ;;We could just perform a read  operation here to remove those characters,
	   ;;but consider  the following  case: at  the REPL  the user  hits "Ctrl-D"
	   ;;sending a "soft" EOF  condition up to the port; such  EOF is consumed by
	   ;;the peek  operation above, so  if we perform  a read operation  here the
	   ;;port will block waiting for more characters.
	   ;;
	   ;;We solve by just resetting the input buffer to empty here.
	   ;;
	   ((eof-object? ch)
	    (with-port (port)
	      (port.buffer.reset-to-empty!))
	    ch)
	   ;;Would-block without available characters: return the would-block object.
	   ((would-block-object? ch)
	    (if (strict-r6rs)
		(retry-after-would-block)
	      ch))
	   ;;If no  end-of-line conversion is  configured for this port:  consume the
	   ;;character and return it.
	   ((%unsafe.port-eol-style-is-none? port)
	    (?read-char-proc port who))
	   ;;End-of-line  conversion   is  configured   for  this  port:   every  EOL
	   ;;single-char must be converted to  #\linefeed.  Consume the character and
	   ;;return #\linefeed.
	   (($char-is-single-char-line-ending? ch)
	    (?read-char-proc port who)
	    LINEFEED-CHAR)
	   ;;End-of-line  conversion  is  configured  for  this  port:  all  the  EOL
	   ;;multi-char sequences start with #\return, convert them to #\linefeed.
	   (($char-is-carriage-return? ch)
	    (let ((ch2 (?peek-char/offset-proc port who ?offset-of-ch2)))
	      (cond
	       ;;A standalone #\return followed by EOF: reset the buffer to empty and
	       ;;return #\linefeed.
	       ;;
	       ;;See the  case of  EOF above for  the reason we  reset the  buffer to
	       ;;empty.
	       ((eof-object? ch2)
		(with-port (port)
		  (port.buffer.reset-to-empty!))
		LINEFEED-CHAR)
	       ;;A #\return  followed by  would-block: return the  would-block object
	       ;;*without*  consuming any  character; maybe  later another  character
	       ;;will be available.
	       ((would-block-object? ch2)
		(if (strict-r6rs)
		    (retry-after-would-block)
		  ch2))
	       ;;A #\return  followed by  the closing character  in an  EOL sequence:
	       ;;consume both the characters in the sequence and return #\linefeed.
	       (($char-is-newline-after-carriage-return? ch2)
		(?read-char-proc port who)
		(?read-char-proc port who)
		LINEFEED-CHAR)
	       ;;A #\return followed by a character *not* in an EOL sequence: consume
	       ;;the first character and return #\linefeed, leave CH2 in the port for
	       ;;later reading.
	       (else
		(?read-char-proc port who)
		LINEFEED-CHAR))))
	   ;;A character not part of EOL sequences: consume it and return it.
	   (else
	    (?read-char-proc port who))))))

    ;;

    (define-syntax-rule (%read-char port who)
      (%unsafe.read-char-from-port-with-fast-get-char-tag port who))

    (define-syntax-rule (%peek-char port who)
      (%unsafe.peek-char-from-port-with-fast-get-char-tag port who))

    (define-syntax-rule (%peek-char/offset port who offset)
      (%unsafe.read/peek-char-from-port-with-string-buffer port who 0 offset))

    ;;

    (define-syntax-rule (%read-latin1 port who)
      (%unsafe.read-char-from-port-with-fast-get-latin1-tag port who))

    (define-syntax-rule (%peek-latin1 port who)
      (%unsafe.peek-char-from-port-with-fast-get-latin1-tag port who))

    (define-syntax-rule (%peek-latin1/offset port who offset)
      (%unsafe.read/peek-char-from-port-with-latin1-codec port who 0 offset))

    ;;

    (define-syntax-rule (%read-utf16le port who)
      (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag port who 'little))

    (define-syntax-rule (%peek-utf16le port who)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'little 0))

    (define-syntax-rule (%peek-utf16le/offset port who offset)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'little offset))

    ;;

    (define-syntax-rule (%read-utf16be port who)
      (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag port who 'big))

    (define-syntax-rule (%peek-utf16be port who)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'big 0))

    (define-syntax-rule (%peek-utf16be/offset port who offset)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'big offset))

    (main))

;;; --------------------------------------------------------------------

  (define (%do-peek-char port who)
    ;;Subroutine of LOOKAHEAD-CHAR and PEEK-CHAR.   Read from the textual input PORT,
    ;;blocking as necessary, until a complete character is available, or until an end
    ;;of file is reached, or until a would-block condition is reached.
    ;;
    ;;* If a complete character is available before the next end of file: return that
    ;;character *without* moving the port position.
    ;;
    ;;* If  an end of file  is reached before any  character is read: return  the EOF
    ;;object.
    ;;
    ;;* If  the underlying device  is in non-blocking mode  and no full  character is
    ;;available: return the would-block object.
    ;;
    (define (main)
      (%case-textual-input-port-fast-tag (port who)
	((FAST-GET-UTF8-TAG)
	 (%peek-it 1
		   %unsafe.peek-char-from-port-with-fast-get-utf8-tag
		   %unsafe.peek-char-from-port-with-utf8-codec))
	((FAST-GET-CHAR-TAG)
	 (%peek-it 1 %peek-char %peek-char/offset))
	((FAST-GET-LATIN-TAG)
	 (%peek-it 1 %peek-latin1 %peek-latin1/offset))
	((FAST-GET-UTF16LE-TAG)
	 (%peek-it 2 %peek-utf16le %peek-utf16le/offset))
	((FAST-GET-UTF16BE-TAG)
	 (%peek-it 2 %peek-utf16be %peek-utf16be/offset))))

    (define-syntax-rule (%peek-it ?offset-of-ch2 ?peek-char-proc ?peek-char/offset-proc)
      ;;Actually perform the  lookahead.  Return the next char  without modifying the
      ;;port position.  If  no characters are available return the  EOF object or the
      ;;would-block  object.  Remember  that, as  mandated by  R6RS, for  input ports
      ;;every line-ending sequence  of characters must be converted  to linefeed when
      ;;the EOL style is not NONE.
      ;;
      ;;?PEEK-CHAR-PROC must  be the identifier  of a macro performing  the lookahead
      ;;operation  for the  next available  character; it  is used  to peek  the next
      ;;single  char,  which  might be  the  first  char  in  a sequence  of  2-chars
      ;;line-ending.
      ;;
      ;;?PEEK-CHAR/OFFSET-PROC  must be  the  identifier of  a  macro performing  the
      ;;forward lookahead operation for the second character in a sequence of 2-chars
      ;;line-ending.
      ;;
      ;;?OFFSET-OF-CH2 is  the offset  of the  second char in  a sequence  of 2-chars
      ;;line-ending; for  ports having bytevector  buffer: it is expressed  in bytes;
      ;;for ports having string buffer:  it is expressed in characters.  Fortunately:
      ;;2-chars line-ending sequences always have a carriage return as first char and
      ;;we know, once the codec has been selected, the offset of such character.
      ;;
      (let ((ch (?peek-char-proc port who)))
	(cond ((eof-or-would-block-object? ch)
	       ch) ;return EOF object or would-block object
	      ;;If no end-of-line conversion is configured for this port: just return
	      ;;the character.
	      ((%unsafe.port-eol-style-is-none? port)
	       ch)
	      ;;End-of-line conversion is configured for  this port: every EOL single
	      ;;char must be converted to #\linefeed.
	      (($char-is-single-char-line-ending? ch)
	       LINEFEED-CHAR)
	      ;;End-of-line conversion is configured for this port: all the EOL multi
	      ;;char sequences start with #\return, convert them to #\linefeed.
	      (($char-is-carriage-return? ch)
	       (let ((ch2 (?peek-char/offset-proc port who ?offset-of-ch2)))
		 (cond
		  ;;A standalone #\return followed by EOF: just return #\linefeed.
		  ((eof-object? ch2)
		   LINEFEED-CHAR)
		  ;;A  #\return  followed  by  would-block:  return  the  would-block
		  ;;object; maybe later another character will be available.
		  ((would-block-object? ch2)
		   ch2)
		  ;;A #\return followed by the  closing character in an EOL sequence:
		  ;;return #\linefeed.
		  (($char-is-newline-after-carriage-return? ch2)
		   LINEFEED-CHAR)
		  ;;A #\return followed by a character *not* in an EOL sequence: just
		  ;;return #\linefeed.
		  (else ch))))
	      ;;A character not part of EOL sequences.
	      (else ch))))

    ;;

    (define-syntax-rule (%peek-char port who)
      (%unsafe.peek-char-from-port-with-fast-get-char-tag port who))

    (define-syntax-rule (%peek-char/offset port who offset)
      (%unsafe.read/peek-char-from-port-with-string-buffer port who 0 offset))

    ;;

    (define-syntax-rule (%peek-latin1 port who)
      (%unsafe.peek-char-from-port-with-fast-get-latin1-tag port who))

    (define-syntax-rule (%peek-latin1/offset port who offset)
      (%unsafe.read/peek-char-from-port-with-latin1-codec port who 0 offset))

    ;;

    (define-syntax-rule (%peek-utf16le port who)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'little 0))

    (define-syntax-rule (%peek-utf16le/offset port who offset)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'little offset))

    ;;

    (define-syntax-rule (%peek-utf16be port who)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'big 0))

    (define-syntax-rule (%peek-utf16be/offset port who offset)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'big offset))

    (main))

  #| end of module |# )


;;;; string input

(module (get-string-n get-string-n! get-string-all get-string-some)

  (define* (get-string-n {port textual-input-port?} {requested-count fixnum-count?})
    ;;Defined  by  R6RS, extended  by  Vicare.   REQUESTED-COUNT  must be  an  exact,
    ;;non--negative integer object, representing the number of characters to be read.
    ;;
    ;;The  GET-STRING-N procedure  reads from  the  textual input  PORT, blocking  as
    ;;necessary, until REQUESTED-COUNT  characters are available, or until  an end of
    ;;file is reached.
    ;;
    ;;* If  REQUESTED-COUNT characters  are available  before end  of file:  return a
    ;;string consisting of  those REQUESTED-COUNT characters.  Update  the input port
    ;;to point just past the characters read.
    ;;
    ;;* If  fewer characters are  available before  an end of  file, but one  or more
    ;;characters can  be read: return  a string containing those  characters.  Update
    ;;the input port to point just past the characters read.
    ;;
    ;;* If no characters can be read before an end of file: return the EOF object.
    ;;
    ;;*  If the  underlying device  is  in non-blocking  mode and  no characters  are
    ;;available: return the would-block object.
    ;;
    ;;IMPLEMENTATION RESTRICTION The REQUESTED-COUNT argument must be a fixnum.
    ;;
    (if ($fxzero? requested-count)
	""
      (let* ((dst.str	($make-string requested-count))
	     (rv	(%get-string-n! __who__ port dst.str 0 requested-count)))
	(cond ((eof-or-would-block-object? rv)
	       ;;Return EOF object or would-block object.
	       rv)
	      (($fx= rv requested-count)
	       dst.str)
	      (else
	       ($substring dst.str 0 rv))))))

  (define* (get-string-n! {port textual-input-port?} {dst.str string?} {dst.start fixnum-start-index?} {char-count fixnum-count?})
    ;;Defined by R6RS,  extended by Vicare.  DST.START and CHAR-COUNT  must be exact,
    ;;non--negative  integer  objects, with  CHAR-COUNT  representing  the number  of
    ;;characters   to  be   read.   DST.STR   must  be   a  string   with  at   least
    ;;DST.START+CHAR-COUNT characters.
    ;;
    ;;The GET-STRING-N!   procedure reads  from the  textual input  PORT in  the same
    ;;manner as GET-STRING-N.
    ;;
    ;;*  If CHAR-COUNT  characters are  available  before an  end of  file: they  are
    ;;written into DST.STR starting at index DST.START, and CHAR-COUNT is returned.
    ;;
    ;;* If fewer than CHAR-COUNT characters are  available before an end of file, but
    ;;one or more can be read: those  characters are written into DST.STR starting at
    ;;index DST.START  and the number of  characters actually read is  returned as an
    ;;exact integer object.
    ;;
    ;;*  If no  characters can  be read  before an  end of  file: the  EOF object  is
    ;;returned.
    ;;
    ;;* If  the underlying device is  in non-blocking mode and  fewer than CHAR-COUNT
    ;;characters are available before a would-block condition, but one or more can be
    ;;read: those characters are written into DST.STR starting at index DST.START and
    ;;the number of characters actually read is returned as an exact integer object.
    ;;
    ;;*  If the  underlying device  is  in non-blocking  mode and  no characters  are
    ;;available: return the would-block object.
    ;;
    ;;IMPLEMENTATION  RESTRICTION  The DST.START  and  CHAR-COUNT  arguments must  be
    ;;fixnums.
    ;;
    (assert-start-index-for-string dst.str dst.start)
    (assert-count-from-start-index-for-string dst.str dst.start char-count)
    (if ($fxzero? char-count)
	char-count
      (%get-string-n! __who__ port dst.str dst.start char-count)))

  (define* (get-string-all {port textual-input-port?})
    ;;Defined by R6RS, extended by Vicare.  Read from the textual input PORT until an
    ;;end  of file,  decoding  characters  in the  same  manner  as GET-STRING-N  and
    ;;GET-STRING-N!.
    ;;
    ;;* If characters are  available before the end of file:  a string containing all
    ;;the characters  decoded from that data  is returned.  Further reading  from the
    ;;port will return the EOF object.
    ;;
    ;;* If no character precedes the end of file: the EOF object is returned.
    ;;
    ;;Even when the underlying device is in non-blocking mode: this function attempts
    ;;to read input until the EOF is found.
    ;;
    ;;IMPLEMENTATION  RESTRICTION The  maximum length  of the  retuned string  is the
    ;;greatest fixnum.
    ;;
    (let next-buffer-string ((output.len   0)
			     (output.strs  '())
			     (dst.len      (string-port-buffer-size))
			     (dst.str      ($make-string (string-port-buffer-size))))
      (let ((count (%get-string-n! __who__ port dst.str 0 dst.len)))
	(cond ((eof-object? count)
	       (if (null? output.strs)
		   ;;Return EOF or would-block.
		   count
		 ;;Return the accumulated string.
		 ($string-reverse-and-concatenate output.len output.strs)))
	      ((would-block-object? count)
	       (next-buffer-string output.len output.strs dst.len dst.str))
	      (($fx= count dst.len)
	       (next-buffer-string ($fx+ output.len dst.len)
				   (cons dst.str output.strs)
				   dst.len
				   ($make-string dst.len)))
	      (else
	       ;;Some characters were read, but less than COUNT.
	       ($string-reverse-and-concatenate (fx+ count output.len)
						(cons ($substring dst.str 0 count) output.strs)))))))

  (define* (get-string-some {port textual-input-port?})
    ;;Defined by  Vicare.  Read from the  textual input PORT, blocking  as necessary,
    ;;until characters are available or until an end of file is reached.
    ;;
    ;;*  If characters  become available  before the  end of  file: return  a freshly
    ;;allocated string  containing the initial  available characters (at  least one),
    ;;and update PORT to point just past these characters.
    ;;
    ;;* If no input  characters are available before the end of  file: the EOF object
    ;;is returned.
    ;;
    ;;* If  no input  characters are  available before  a would-block  condition: the
    ;;would-block object is returned.
    ;;
    ;; ------------------------------------------------------------------------------
    ;;
    ;;This  is like  GET-STRING-ALL, but  we do  not loop  reading strings  again and
    ;;again: we are satisfied by just reading the next DST.LEN characters.
    (let* ((dst.len   (string-port-buffer-size))
	   (dst.str	($make-string dst.len))
	   (count	(%get-string-n! __who__ port dst.str 0 dst.len)))
      (cond ((eof-or-would-block-object? count)
	     count)
	    (($fx= count dst.len)
	     dst.str)
	    (else
	     ($substring dst.str 0 count)))))

;;; --------------------------------------------------------------------

  (define (%get-string-n! who port dst.str dst.start count)
    ;;Subroutine of GET-STRING-N!,  GET-STRING-N, GET-STRING-SOME and GET-STRING-ALL.
    ;;It assumes the arguments have already been validated.  NONE.
    ;;
    ;;DST.START and  COUNT must  be exact, non-negative  integer objects,  with COUNT
    ;;representing the  number of characters  to be read.   DST.STR must be  a string
    ;;with at least DST.START+COUNT characters.
    ;;
    ;;* If  COUNT characters are  available before an end  of file: they  are written
    ;;into DST.STR starting at index DST.START, and COUNT is returned.
    ;;
    ;;* If fewer than  COUNT characters are available before an end  of file, but one
    ;;or more  can be  read: those  characters are written  into DST.STR  starting at
    ;;index DST.START  and the number of  characters actually read is  returned as an
    ;;exact integer object.
    ;;
    ;;*  If no  characters can  be read  before an  end of  file: the  EOF object  is
    ;;returned.
    ;;
    ;;*  If the  underlying  device is  in  non-blocking mode  and  fewer than  COUNT
    ;;characters are available before a would-block condition, but one or more can be
    ;;read: those characters are written into DST.STR starting at index DST.START and
    ;;the number of characters actually read is returned as an exact integer object.
    ;;
    ;;* If  the underlying device  is in non-blocking mode  and no characters  can be
    ;;read before a would-block condition: the would-block object is returned.
    ;;
    ;;IMPLEMENTATION RESTRICTION The  DST.START and COUNT arguments  must be fixnums;
    ;;DST.START+COUNT must be a fixnum.
    ;;
    (define-inline (main)
      (let ((dst.past ($fx+ dst.start count)))
	(%case-textual-input-port-fast-tag (port who)
	  ((FAST-GET-UTF8-TAG)
	   (%get-it dst.past
		    1
		    %unsafe.read-char-from-port-with-fast-get-utf8-tag
		    %unsafe.peek-char-from-port-with-fast-get-utf8-tag
		    %unsafe.peek-char-from-port-with-utf8-codec))
	  ((FAST-GET-CHAR-TAG)
	   (%get-it dst.past
		    1
		    %unsafe.read-char-from-port-with-fast-get-char-tag
		    %peek-char
		    %peek-char/offset))
	  ((FAST-GET-LATIN-TAG)
	   (%get-it dst.past
		    1
		    %unsafe.read-char-from-port-with-fast-get-latin1-tag
		    %peek-latin1
		    %peek-latin1/offset))
	  ((FAST-GET-UTF16LE-TAG)
	   (%get-it dst.past
		    2
		    %read-utf16le
		    %peek-utf16le
		    %peek-utf16le/offset))
	  ((FAST-GET-UTF16BE-TAG)
	   (%get-it dst.past
		    2
		    %read-utf16be
		    %peek-utf16be
		    %peek-utf16be/offset)))))

    (define-syntax-rule (%get-it ?dst.past ?offset-of-ch2 ?read-char-proc
				 ?peek-char-proc ?peek-char/offset-proc)
      ;;Actually perform  the reading.  Loop reading  the next char and  updating the
      ;;port position;  read chars  are stored  into DST.STR.   If no  characters are
      ;;available return the EOF object or the would-block object.  Remember that, as
      ;;mandated by  R6RS, for input  ports every line-ending sequence  of characters
      ;;must be converted to linefeed when the EOL style is not NONE.
      ;;
      ;;?DST.PAST is  the index one-past  the last position  in DST.STR that  must be
      ;;filled.
      ;;
      ;;?READ-CHAR-PROC  must be  the identifier  of a  macro performing  the reading
      ;;operation  for the  next available  character; it  is used  to read  the next
      ;;single  char,  which  might be  the  first  char  in  a sequence  of  2-chars
      ;;line-ending.
      ;;
      ;;?PEEK-CHAR-PROC must  be the identifier  of a macro performing  the lookahead
      ;;operation  for the  next available  character; it  is used  to peek  the next
      ;;single  char,  which  might be  the  first  char  in  a sequence  of  2-chars
      ;;line-ending.
      ;;
      ;;?PEEK-CHAR/OFFSET-PROC  must be  the  identifier of  a  macro performing  the
      ;;forward lookahead operation for the second character in a sequence of 2-chars
      ;;line-ending.
      ;;
      ;;?OFFSET-OF-CH2 is  the offset  of the  second char in  a sequence  of 2-chars
      ;;line-ending; for  ports having bytevector  buffer: it is expressed  in bytes;
      ;;for ports having string buffer:  it is expressed in characters.  Fortunately:
      ;;2-chars line-ending sequences always have a carriage return as first char and
      ;;we know, once the codec has been selected, the offset of such character.
      ;;
      (let read-next-char ((dst.index dst.start))
	(define (%store-char-then-loop-or-return ch)
	  ($string-set! dst.str dst.index ch)
	  (let ((dst.index ($fxadd1 dst.index)))
	    (if ($fx= dst.index ?dst.past)
		($fx- dst.index dst.start)
	      (read-next-char dst.index))))
	(let ((ch (?peek-char-proc port who)))
	  (cond
	   ;;EOF without available characters: if  no characters were previously read
	   ;;return the EOF object, else return the number of characters read.
	   ;;
	   ;;Notice that: the peek char operation performed above can return EOF even
	   ;;when  there are  bytes in  the  input buffer;  it happens  when all  the
	   ;;following conditions are met:
	   ;;
	   ;;1. The bytes do *not* represent a valid Unicode character in the context
	   ;;of the selected codec.
	   ;;
	   ;;2. The transcoder has ERROR-HANDLING-MODE set to "ignore".
	   ;;
	   ;;3. The invalid bytes are followed by an EOF.
	   ;;
	   ;;We could just perform a read  operation here to remove those characters,
	   ;;but consider  the following  case: at  the REPL  the user  hits "Ctrl-D"
	   ;;sending a "soft" EOF  condition up to the port; such  EOF is consumed by
	   ;;the peek  operation above, so  if we perform  a read operation  here the
	   ;;port will block waiting for more characters.
	   ;;
	   ;;We solve by just resetting the input buffer to empty here.
	   ;;
	   ((eof-object? ch)
	    (with-port (port)
	      (port.buffer.reset-to-empty!))
	    (if ($fx= dst.index dst.start)
		;;Return the EOF object object.
		ch
	      ;;Return the number of chars read.
	      ($fx- dst.index dst.start)))
	   ;;Would-block  with no  new characters  available: if  no characters  were
	   ;;previously read return the would-block object, else return the number of
	   ;;characters read.
	   ((would-block-object? ch)
	    (if (strict-r6rs)
		;;In R6RS  mode we ignore  the would-block condition and  just insist
		;;reading with the same destination index.
		(read-next-char dst.index)
	      (if ($fx= dst.index dst.start)
		  ;;Return the would-block object.
		  ch
		;;Return the number of chars read.
		($fx- dst.index dst.start))))
	   ;;If no  end-of-line conversion is  configured for this port:  consume the
	   ;;character, store it and loop.
	   ((%unsafe.port-eol-style-is-none? port)
	    (?read-char-proc port who)
	    (%store-char-then-loop-or-return ch))
	   ;;End-of-line  conversion   is  configured   for  this  port:   every  EOL
	   ;;single-char  must be  converted to  #\linefeed.  Consume  the character,
	   ;;store #\linefeed and loop.
	   (($char-is-single-char-line-ending? ch)
	    (?read-char-proc port who)
	    (%store-char-then-loop-or-return LINEFEED-CHAR))
	   ;;End-of-line  conversion  is  configured  for  this  port:  all  the  EOL
	   ;;multi-char sequences start with #\return, convert them to #\linefeed.
	   (($char-is-carriage-return? ch)
	    (let ((ch2 (?peek-char/offset-proc port who ?offset-of-ch2)))
	      (cond
	       ;;A standalone  #\return followed by  EOF: reset the buffer  to empty,
	       ;;store #\linefeed then loop or return.
	       ;;
	       ;;See the  case of  EOF above for  the reason we  reset the  buffer to
	       ;;empty.
	       ((eof-object? ch2)
		(with-port (port)
		  (port.buffer.reset-to-empty!))
		(%store-char-then-loop-or-return LINEFEED-CHAR))
	       ;;A  #\return   followed  by  would-block:  return   object  *without*
	       ;;consuming the  return character; maybe later  another character will
	       ;;be available.
	       ((would-block-object? ch2)
		(if (strict-r6rs)
		    ;;In  R6RS mode  we  ignore the  would-block  condition and  just
		    ;;insist reading with the same destination index.
		    (read-next-char dst.index)
		  (if ($fx= dst.index dst.start)
		      ;;Return the would-block object.
		      ch
		    ;;Return the number of chars read.
		    ($fx- dst.index dst.start))))
	       ;;A #\return  followed by  the closing character  in an  EOL sequence:
	       ;;consume both the  characters in the sequence,  store #\linefeed then
	       ;;loop or return.
	       (($char-is-newline-after-carriage-return? ch2)
		(?read-char-proc port who)
		(?read-char-proc port who)
		(%store-char-then-loop-or-return LINEFEED-CHAR))
	       ;;A #\return followed by a character *not* in an EOL sequence: consume
	       ;;the first character, store #\linefeed then loop or return; leave CH2
	       ;;in the port for later reading.
	       (else
		(?read-char-proc port who)
		(%store-char-then-loop-or-return LINEFEED-CHAR)))))
	   ;;A character not part of EOL sequences: consume it, store it then loop or
	   ;;return.
	   (else
	    (?read-char-proc port who)
	    (%store-char-then-loop-or-return ch))))))

    ;;

    (define-syntax-rule (%read-char port who)
      (%unsafe.read-char-from-port-with-fast-get-char-tag port who))

    (define-syntax-rule (%peek-char port who)
      (%unsafe.peek-char-from-port-with-fast-get-char-tag port who))

    (define-syntax-rule (%peek-char/offset port who offset)
      (%unsafe.read/peek-char-from-port-with-string-buffer port who 0 offset))

    ;;

    (define-syntax-rule (%read-latin1 port who)
      (%unsafe.read-char-from-port-with-fast-get-latin1-tag port who))

    (define-syntax-rule (%peek-latin1 port who)
      (%unsafe.peek-char-from-port-with-fast-get-latin1-tag port who))

    (define-syntax-rule (%peek-latin1/offset port who offset)
      (%unsafe.read/peek-char-from-port-with-latin1-codec port who 0 offset))

    ;;

    (define-syntax-rule (%read-utf16le port who)
      (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag port who 'little))

    (define-syntax-rule (%peek-utf16le port who)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'little 0))

    (define-syntax-rule (%peek-utf16le/offset port who offset)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'little offset))

    ;;

    (define-syntax-rule (%read-utf16be port who)
      (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag port who 'big))

    (define-syntax-rule (%peek-utf16be port who)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'big 0))

    (define-syntax-rule (%peek-utf16be/offset port who offset)
      (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag port who 'big offset))


    (main))

  #| end of module |# )


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
  ;;If an  end of file is  encountered before any linefeed  character is
  ;;read, but  some bytes have  been read  and decoded as  characters, a
  ;;string containing those characters is returned.
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

  (define-syntax-rule (%get-it ?read-char ?peek-char)
    (let ((eol-bits (%unsafe.port-eol-style-bits port)))
      (let loop ((port			port)
		 (number-of-chars	0)
		 (reverse-chars		'()))
	(let ((ch (?read-char port who)))
	  (cond ((eof-object? ch)
		 (if (null? reverse-chars)
		     ch
		   (%unsafe.reversed-chars->string number-of-chars reverse-chars)))
		;;We are waiting for the end of line here.
		((would-block-object? ch)
		 (loop port number-of-chars reverse-chars))
		(else
		 (let ((ch (%convert-if-line-ending eol-bits ch ?read-char ?peek-char)))
		   (if ($char= ch LINEFEED-CHAR)
		       (%unsafe.reversed-chars->string number-of-chars reverse-chars)
		     (loop port ($fxadd1 number-of-chars) (cons ch reverse-chars))))))))))

  (define-syntax-rule (%convert-if-line-ending eol-bits ch ?read-char ?peek-char)
    (cond (($fxzero? eol-bits) ;EOL style none
	   ch)
	  (($char-is-single-char-line-ending? ch)
	   LINEFEED-CHAR)
	  (($char-is-carriage-return? ch)
	   (let ((ch1 (?peek-char port who)))
	     (cond ((eof-object? ch1)
		    (void))
		   (($char-is-newline-after-carriage-return? ch1)
		    (?read-char port who)))
	     LINEFEED-CHAR))
	  (else ch)))

  (define (%unsafe.reversed-chars->string dst.len reverse-chars)
    (let next-char ((dst.str       ($make-string dst.len))
		    (dst.index     ($fxsub1 dst.len))
		    (reverse-chars reverse-chars))
      (if (null? reverse-chars)
	  dst.str
	(begin
	  ($string-set! dst.str dst.index (car reverse-chars))
	  (next-char dst.str ($fxsub1 dst.index) (cdr reverse-chars))))))

  (define-inline (%read-utf16le ?port ?who)
    (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little))

  (define-inline (%peek-utf16le ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'little 0))

  (define-inline (%read-utf16be ?port ?who)
    (%unsafe.read-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big))

  (define-inline (%peek-utf16be ?port ?who)
    (%unsafe.peek-char-from-port-with-fast-get-utf16xe-tag ?port ?who 'big 0))

  (main))


;;;; GET-CHAR and LOOKAHEAD-CHAR for ports with UTF-8 transcoder

(define-inline (%unsafe.read-char-from-port-with-fast-get-utf8-tag port who)
  ;;PORT  is a  textual  input  port with  bytevector  buffer and  UTF-8
  ;;transcoder.   We  process  here   the  simple  case  of  single-byte
  ;;character already available in the buffer, for multi-byte characters
  ;;we call the specialised function for reading UTF-8 chars.
  ;;
  ;;Return  the  EOF object,  the  would-block  object or  a  character.
  ;;Update the port position to point past the consumed character.
  ;;
  (with-port-having-bytevector-buffer (port)
    (let ((buffer.offset-byte0 port.buffer.index))
      (if ($fx< buffer.offset-byte0 port.buffer.used-size)
	  (let ((byte0 ($bytevector-u8-ref port.buffer buffer.offset-byte0)))
	    (if (unicode.utf-8-single-octet? byte0)
		(let ((N (unicode.utf-8-decode-single-octet byte0)))
		  ;;Update the port position  to point past the consumed
		  ;;character.
		  (set! port.buffer.index ($fxadd1 buffer.offset-byte0))
		  ($fixnum->char N))
	      (%unsafe.read-char-from-port-with-utf8-codec port who)))
	(%unsafe.read-char-from-port-with-utf8-codec port who)))))

(define-inline (%unsafe.peek-char-from-port-with-fast-get-utf8-tag port who)
  ;;PORT must be  a textual input port with bytevector  buffer and UTF-8
  ;;transcoder.   We  process  here   the  simple  case  of  single-byte
  ;;character already available in the buffer, for multi-byte characters
  ;;we call the specialised function for peeking UTF8 characters.
  ;;
  ;;Return the EOF object, the would-block object or a character.
  ;;
  (with-port-having-bytevector-buffer (port)
    (let ((buffer.offset-byte0 port.buffer.index))
      (if ($fx< buffer.offset-byte0 port.buffer.used-size)
	  (let ((byte0 ($bytevector-u8-ref port.buffer buffer.offset-byte0)))
	    (if (unicode.utf-8-single-octet? byte0)
		($fixnum->char (unicode.utf-8-decode-single-octet byte0))
	      (%unsafe.peek-char-from-port-with-utf8-codec port who 0)))
	(%unsafe.peek-char-from-port-with-utf8-codec port who 0)))))

(define (%unsafe.read-char-from-port-with-utf8-codec port who)
  ;;PORT must be  a textual input port with bytevector  buffer and UTF-8
  ;;transcoder.  Read from PORT a  UTF-8 encoded character for the cases
  ;;of 1, 2,  3 and 4 bytes  encoding; return a Scheme  character or the
  ;;EOF object  or the would-block object;  in case of error:  honor the
  ;;error mode in the port's transcoder.
  ;;
  ;;The port's buffer might be fully  consumed or not.  This function is
  ;;meant to be used by all  the functions reading UTF-8 characters from
  ;;a port.
  ;;
  (with-port-having-bytevector-buffer (port)

    (define-inline (main)
      (let retry-after-filling-buffer ()
	(let ((buffer.offset-byte0 port.buffer.index))
	  (maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte0)
	    (if-end-of-file: (eof-object))
	    (if-empty-buffer-and-refilling-would-block: WOULD-BLOCK-OBJECT)
	    (if-successful-refill: (retry-after-filling-buffer))
	    (if-available-data:
	     (let ((byte0 ($bytevector-u8-ref port.buffer buffer.offset-byte0)))
	       (define (%error-invalid-byte)
		 (%error-handler "invalid byte while expecting first byte of UTF-8 character"
				 byte0))
	       (cond ((unicode.utf-8-invalid-octet? byte0)
		      (set! port.buffer.index ($fxadd1 buffer.offset-byte0))
		      (%error-invalid-byte))
		     ((unicode.utf-8-single-octet? byte0)
		      (get-single-byte-character byte0 buffer.offset-byte0))
		     ((unicode.utf-8-first-of-two-octets? byte0)
		      (get-2-bytes-character byte0 buffer.offset-byte0))
		     ((unicode.utf-8-first-of-three-octets? byte0)
		      (get-3-bytes-character byte0 buffer.offset-byte0))
		     ((unicode.utf-8-first-of-four-octets? byte0)
		      (get-4-bytes-character byte0 buffer.offset-byte0))
		     (else
		      (set! port.buffer.index ($fxadd1 buffer.offset-byte0))
		      (%error-invalid-byte)))))
	    ))))

    (define-inline (get-single-byte-character byte0 buffer.offset-byte0)
      (let ((N (unicode.utf-8-decode-single-octet byte0)))
	(set! port.buffer.index ($fxadd1 buffer.offset-byte0))
	($fixnum->char N)))

    (define-inline (get-2-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-1-more-byte
	  ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let* ((buffer.offset-byte1 ($fxadd1 buffer.offset-byte0))
	       (buffer.offset-past  ($fxadd1 buffer.offset-byte1)))
	  (maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte1)
	    (if-end-of-file:
	     (%unexpected-eof-error "unexpected EOF while decoding 2-byte UTF-8 character"
				    byte0))
	    (if-empty-buffer-and-refilling-would-block: WOULD-BLOCK-OBJECT)
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-1-more-byte port.buffer.index))
	    (if-available-data:
	     (let ((byte1 ($bytevector-u8-ref port.buffer buffer.offset-byte1)))
	       (define (%error-invalid-second)
		 (%error-handler "invalid second byte in 2-byte UTF-8 character"
				 byte0 byte1))
	       (set! port.buffer.index buffer.offset-past)
	       (cond ((unicode.utf-8-invalid-octet? byte1)
		      (%error-invalid-second))
		     ((unicode.utf-8-second-of-two-octets? byte1)
		      (let ((N (unicode.utf-8-decode-two-octets byte0 byte1)))
			(if (unicode.utf-8-valid-code-point-from-2-octets? N)
			    ($fixnum->char N)
			  (%error-handler "invalid code point as result \
                                           of decoding 2-byte UTF-8 character"
					  byte0 byte1 N))))
		     (else
		      (%error-invalid-second)))))
	    ))))

    (define-inline (get-3-bytes-character byte0 buffer.offset-byte0)
      ;;After refilling we have to reload buffer indexes.
      (let retry-after-filling-buffer-for-2-more-bytes
	  ((buffer.offset-byte0 buffer.offset-byte0))
	(let* ((buffer.offset-byte1 ($fxadd1 buffer.offset-byte0))
	       (buffer.offset-byte2 ($fxadd1 buffer.offset-byte1))
	       (buffer.offset-past  ($fxadd1 buffer.offset-byte2)))
	  (maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte2)
	    (if-end-of-file:
	     (apply %unexpected-eof-error "unexpected EOF while decoding 3-byte UTF-8 character"
		    byte0 (if ($fx< buffer.offset-byte1 port.buffer.used-size)
			      (list ($bytevector-u8-ref port.buffer buffer.offset-byte1))
			    '())))
	    (if-empty-buffer-and-refilling-would-block: WOULD-BLOCK-OBJECT)
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-2-more-bytes port.buffer.index))
	    (if-available-data:
	     (let ((byte1 ($bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (byte2 ($bytevector-u8-ref port.buffer buffer.offset-byte2)))
	       (define (%error-invalid-second-or-third)
		 (%error-handler "invalid second or third byte while decoding UTF-8 character \
                                  and expecting 3-byte character"
				 byte0 byte1 byte2))
	       (set! port.buffer.index buffer.offset-past)
	       (cond ((or (unicode.utf-8-invalid-octet? byte1)
			  (unicode.utf-8-invalid-octet? byte2))
		      (%error-invalid-second-or-third))
		     ((unicode.utf-8-second-and-third-of-three-octets? byte1 byte2)
		      (let ((N (unicode.utf-8-decode-three-octets byte0 byte1 byte2)))
			(if (unicode.utf-8-valid-code-point-from-3-octets? N)
			    ($fixnum->char N)
			  (%error-handler "invalid code point as result \
                                           of decoding 3-byte UTF-8 character"
					  byte0 byte1 byte2 N))))
		     (else
		      (%error-invalid-second-or-third)))))
	    ))))

    (define-inline (get-4-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-3-more-bytes
	  ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let* ((buffer.offset-byte1 ($fxadd1 buffer.offset-byte0))
	       (buffer.offset-byte2 ($fxadd1 buffer.offset-byte1))
	       (buffer.offset-byte3 ($fxadd1 buffer.offset-byte2))
	       (buffer.offset-past  ($fxadd1 buffer.offset-byte3)))
	  (maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte3)
	    (if-end-of-file:
	     (apply %unexpected-eof-error "unexpected EOF while decoding 4-byte UTF-8 character"
		    byte0 (if ($fx< buffer.offset-byte1 port.buffer.used-size)
			      (cons ($bytevector-u8-ref port.buffer buffer.offset-byte1)
				    (if ($fx< buffer.offset-byte2 port.buffer.used-size)
					(list ($bytevector-u8-ref port.buffer buffer.offset-byte2))
				      '()))
			    '())))
	    (if-empty-buffer-and-refilling-would-block: WOULD-BLOCK-OBJECT)
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-3-more-bytes port.buffer.index))
	    (if-available-data:
	     (let ((byte1 ($bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (byte2 ($bytevector-u8-ref port.buffer buffer.offset-byte2))
		   (byte3 ($bytevector-u8-ref port.buffer buffer.offset-byte3)))
	       (define (%error-invalid-second-third-or-fourth)
		 (%error-handler "invalid second, third or fourth byte while decoding UTF-8 character \
                                  and expecting 4-byte character"
				 byte0 byte1 byte2 byte3))
	       (set! port.buffer.index buffer.offset-past)
	       (cond ((or (unicode.utf-8-invalid-octet? byte1)
			  (unicode.utf-8-invalid-octet? byte2)
			  (unicode.utf-8-invalid-octet? byte3))
		      (%error-invalid-second-third-or-fourth))
		     ((unicode.utf-8-second-third-and-fourth-of-four-octets? byte1 byte2 byte3)
		      (let ((N (unicode.utf-8-decode-four-octets byte0 byte1 byte2 byte3)))
			(if (unicode.utf-8-valid-code-point-from-4-octets? N)
			    ($fixnum->char N)
			  (%error-handler "invalid code point as result \
                                           of decoding 4-byte UTF-8 character"
					  byte0 byte1 byte2 byte3 N))))
		     (else
		      (%error-invalid-second-third-or-fourth)))))
	    ))))

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
	   (assertion-violation who
	     "vicare internal error: wrong transcoder error handling mode" mode)))))

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
	(let ((buffer.offset-byte0 ($fx+ port.buffer.index buffer-offset)))
	  (maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte0)
	    (if-end-of-file: (eof-object))
	    (if-empty-buffer-and-refilling-would-block: WOULD-BLOCK-OBJECT)
	    (if-successful-refill: (retry-after-filling-buffer))
	    (if-available-data:
	     (let ((byte0		($bytevector-u8-ref port.buffer buffer.offset-byte0))
		   (buffer-offset	($fx+ 1 buffer-offset)))
	       (define (%error-invalid-byte)
		 (%error-handler ($fxadd1 buffer-offset)
				 "invalid byte while expecting first byte of UTF-8 character"
				 byte0))
	       (cond ((unicode.utf-8-invalid-octet? byte0)
		      (%error-invalid-byte))
		     ((unicode.utf-8-single-octet? byte0)
		      ($fixnum->char (unicode.utf-8-decode-single-octet byte0)))
		     ((unicode.utf-8-first-of-two-octets? byte0)
		      (peek-2-bytes-character byte0 buffer.offset-byte0))
		     ((unicode.utf-8-first-of-three-octets? byte0)
		      (peek-3-bytes-character byte0 buffer.offset-byte0))
		     ((unicode.utf-8-first-of-four-octets? byte0)
		      (peek-4-bytes-character byte0 buffer.offset-byte0))
		     (else
		      (%error-invalid-byte)))))
	    ))))

    (define-inline (peek-2-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-1-more-byte
	  ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let ((buffer.offset-byte1 ($fxadd1 buffer.offset-byte0)))
	  (maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte1)
	    (if-end-of-file:
	     (%unexpected-eof-error "unexpected EOF while decoding 2-byte UTF-8 character"
				    byte0))
	    (if-empty-buffer-and-refilling-would-block: WOULD-BLOCK-OBJECT)
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-1-more-byte port.buffer.index))
	    (if-available-data:
	     (let ((byte1		($bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (buffer-offset	($fx+ 2 buffer-offset)))
	       (define (%error-invalid-second)
		 (%error-handler buffer-offset "invalid second byte in 2-byte UTF-8 character"
				 byte0 byte1))
	       (cond ((unicode.utf-8-invalid-octet? byte1)
		      (%error-invalid-second))
		     ((unicode.utf-8-second-of-two-octets? byte1)
		      (let ((N (unicode.utf-8-decode-two-octets byte0 byte1)))
			(if (unicode.utf-8-valid-code-point-from-2-octets? N)
			    ($fixnum->char N)
			  (%error-handler buffer-offset
					  "invalid code point as result \
                                           of decoding 2-byte UTF-8 character"
					  byte0 byte1 N))))
		     (else
		      (%error-invalid-second)))))
	    ))))

    (define-inline (peek-3-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-2-more-bytes
	  ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let* ((buffer.offset-byte1 ($fxadd1 buffer.offset-byte0))
	       (buffer.offset-byte2 ($fxadd1 buffer.offset-byte1)))
	  (maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte2)
	    (if-end-of-file:
	     (apply %unexpected-eof-error "unexpected EOF while decoding 3-byte UTF-8 character"
		    byte0 (if ($fx< buffer.offset-byte1 port.buffer.used-size)
			      (list ($bytevector-u8-ref port.buffer buffer.offset-byte1))
			    '())))
	    (if-empty-buffer-and-refilling-would-block: WOULD-BLOCK-OBJECT)
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-2-more-bytes port.buffer.index))
	    (if-available-data:
	     (let ((byte1		($bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (byte2		($bytevector-u8-ref port.buffer buffer.offset-byte2))
		   (buffer-offset	($fx+ 3 buffer-offset)))
	       (define (%error-invalid-second-or-third)
		 (%error-handler buffer-offset
				 "invalid second or third byte in 3-byte UTF-8 character"
				 byte0 byte1 byte2))
	       (cond ((or (unicode.utf-8-invalid-octet? byte1)
			  (unicode.utf-8-invalid-octet? byte2))
		      (%error-invalid-second-or-third))
		     ((unicode.utf-8-second-and-third-of-three-octets? byte1 byte2)
		      (let ((N (unicode.utf-8-decode-three-octets byte0 byte1 byte2)))
			(if (unicode.utf-8-valid-code-point-from-3-octets? N)
			    ($fixnum->char N)
			  (%error-handler buffer-offset
					  "invalid code point as result of \
                                           decoding 3-byte UTF-8 character"
					  byte0 byte1 byte2 N))))
		     (else
		      (%error-invalid-second-or-third)))))
	    ))))

    (define-inline (peek-4-bytes-character byte0 buffer.offset-byte0)
      (let retry-after-filling-buffer-for-3-more-bytes
	  ((buffer.offset-byte0 buffer.offset-byte0))
	;;After refilling we have to reload buffer indexes.
	(let* ((buffer.offset-byte1 ($fxadd1 buffer.offset-byte0))
	       (buffer.offset-byte2 ($fxadd1 buffer.offset-byte1))
	       (buffer.offset-byte3 ($fxadd1 buffer.offset-byte2)))
	  (maybe-refill-bytevector-buffer-and-evaluate (port who)
	    (data-is-needed-at: buffer.offset-byte3)
	    (if-end-of-file:
	     (apply %unexpected-eof-error "unexpected EOF while decoding 4-bytes UTF-8 character"
		    byte0 (if ($fx< buffer.offset-byte1 port.buffer.used-size)
			      (cons ($bytevector-u8-ref port.buffer buffer.offset-byte1)
				    (if ($fx< buffer.offset-byte2 port.buffer.used-size)
					(list ($bytevector-u8-ref port.buffer
								  buffer.offset-byte2))
				      '()))
			    '())))
	    (if-empty-buffer-and-refilling-would-block: WOULD-BLOCK-OBJECT)
	    (if-successful-refill:
	     (retry-after-filling-buffer-for-3-more-bytes port.buffer.index))
	    (if-available-data:
	     (let ((byte1  ($bytevector-u8-ref port.buffer buffer.offset-byte1))
		   (byte2  ($bytevector-u8-ref port.buffer buffer.offset-byte2))
		   (byte3  ($bytevector-u8-ref port.buffer buffer.offset-byte3))
		   (buffer-offset ($fx+ 4 buffer-offset)))
	       (define (%error-invalid-second-third-or-fourth)
		 (%error-handler buffer-offset
				 "invalid second, third or fourth byte \
                                  in 4-bytes UTF-8 character"
				 byte0 byte1 byte2 byte3))
	       (cond ((or (unicode.utf-8-invalid-octet? byte1)
			  (unicode.utf-8-invalid-octet? byte2)
			  (unicode.utf-8-invalid-octet? byte3))
		      (%error-invalid-second-third-or-fourth))
		     ((unicode.utf-8-second-third-and-fourth-of-four-octets? byte1 byte2 byte3)
		      (let ((N (unicode.utf-8-decode-four-octets byte0 byte1 byte2 byte3)))
			(if (unicode.utf-8-valid-code-point-from-4-octets? N)
			    ($fixnum->char N)
			  (%error-handler buffer-offset
					  "invalid code point as result \
                                           of decoding 4-bytes UTF-8 character"
					  byte0 byte1 byte2 N))))
		     (else
		      (%error-invalid-second-third-or-fourth)))))
	    ))))

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
	   (assertion-violation who
	     "vicare internal error: wrong transcoder error handling mode" mode)))))

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
	   (assertion-violation who
	     "vicare internal error: wrong transcoder error handling mode" mode)))))

    (main)))


;;;; GET-CHAR and LOOKAHEAD-CHAR for ports with UTF-16 transcoder

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
      (cond (($fx<= char-code-point #xD7FF)
	     ($fixnum->char char-code-point))
	    (($fx<  char-code-point #xE000)
	     (%error-handler errmsg char-code-point))
	    (($fx<= char-code-point #x10FFFF)
	     ($fixnum->char char-code-point))
	    (else
	     (%error-handler errmsg char-code-point))))

    (define-inline (%word-ref buffer.offset)
      ($bytevector-u16-ref port.buffer buffer.offset endianness))

    (let* ((buffer.offset-word0 port.buffer.index)
	   (buffer.offset-word1 ($fx+ 2 buffer.offset-word0))
	   (buffer.offset-past  ($fx+ 2 buffer.offset-word1)))
      (cond (($fx<= buffer.offset-word1 port.buffer.used-size)
	     ;;There are at least two  bytes in the input buffer, enough
	     ;;for  a full UTF-16  character encoded  as single  16 bits
	     ;;word.
	     (let ((word0 (%word-ref buffer.offset-word0)))
	       (cond ((unicode.utf-16-single-word? word0)
		      ;;WORD0  is  in the  allowed  range  for a  UTF-16
		      ;;encoded character of 16 bits.
		      (set! port.buffer.index buffer.offset-word1)
		      (integer->char/invalid (unicode.utf-16-decode-single-word word0)))
		     ((not (unicode.utf-16-first-of-two-words? word0))
		      (set! port.buffer.index buffer.offset-word1)
		      (%error-handler "invalid 16-bit word while decoding UTF-16 characters \
                                       and expecting single word character or first word of \
                                       surrogate pair" word0))
		     (($fx<= buffer.offset-past port.buffer.used-size)
		      ;;WORD0 is  the first  of a UTF-16  surrogate pair
		      ;;and  the input buffer  already holds  the second
		      ;;word.
		      (let ((word1 (%word-ref buffer.offset-word1)))
			(set! port.buffer.index buffer.offset-past)
			(if (unicode.utf-16-second-of-two-words? word1)
			    (integer->char/invalid (unicode.utf-16-decode-surrogate-pair word0 word1))
			  (%error-handler "invalid 16-bit word while decoding UTF-16 characters \
                                           and expecting second word in surrogate pair"
					  word0 word1))))
		     (else
		      ;;WORD0 is  the first of a  UTF-16 surrogate pair,
		      ;;but input  buffer does not hold  the full second
		      ;;word.
		      (refill-bytevector-buffer-and-evaluate (port who)
			(if-end-of-file:
			 (set! port.buffer.index port.buffer.used-size)
			 (%unexpected-eof-error
			  "unexpected end of file while decoding UTF-16 characters \
                           and expecting second word in surrogate pair"
			  `(,($bytevector-u8-ref port.buffer buffer.offset-word0)
			    ,($bytevector-u8-ref port.buffer ($fxadd1 buffer.offset-word0))
			    . ,(if ($fx< buffer.offset-word1
					       port.buffer.used-size)
				   (list ($bytevector-u8-ref port.buffer buffer.offset-word1))
				 '()))))
			(if-refilling-would-block:
			 WOULD-BLOCK-OBJECT)
			(if-successful-refill:
			 (recurse))
			)))))

	    (($fx< buffer.offset-word0 port.buffer.used-size)
	     ;;There is only 1 byte in the input buffer.
	     (refill-bytevector-buffer-and-evaluate (port who)
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
		 `(,($bytevector-u8-ref port.buffer buffer.offset-word0))))
	       (if-refilling-would-block: WOULD-BLOCK-OBJECT)
	       (if-successful-refill:
		(recurse))
	       ))

	    (else
	     ;;The input buffer is empty.
	     (refill-bytevector-buffer-and-evaluate (port who)
	       (if-end-of-file:	(eof-object))
	       (if-refilling-would-block: WOULD-BLOCK-OBJECT)
	       (if-successful-refill:
		(recurse))
	       ))))))

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
	(cond (($fx<= char-code-point #xD7FF)
	       ($fixnum->char char-code-point))
	      (($fx<  char-code-point #xE000)
	       (%error-handler buffer-offset errmsg (list char-code-point)))
	      (($fx<= char-code-point #x10FFFF)
	       ($fixnum->char char-code-point))
	      (else
	       (%error-handler buffer-offset errmsg (list char-code-point))))))

    (define-inline (%word-ref buffer.offset)
      ($bytevector-u16-ref port.buffer buffer.offset endianness))

    (let* ((buffer.offset-word0 ($fx+ port.buffer.index buffer-offset))
	   (buffer.offset-word1 ($fx+ 2 buffer.offset-word0))
	   (buffer.offset-past  ($fx+ 2 buffer.offset-word1)))
      (cond (($fx<= buffer.offset-word1 port.buffer.used-size)
	     ;;There are at least two  bytes in the input buffer, enough
	     ;;for  a full UTF-16  character encoded  as single  16 bits
	     ;;word.
	     (let ((word0 (%word-ref buffer.offset-word0)))
	       (cond ((unicode.utf-16-single-word? word0)
		      (integer->char/invalid (unicode.utf-16-decode-single-word word0) buffer.offset-word1))
		     ((not (unicode.utf-16-first-of-two-words? word0))
		      (%error-handler ($fx+ 2 buffer-offset)
				      "invalid 16-bit word while decoding UTF-16 characters \
                                       and expecting single word character or first word in \
                                       surrogate pair"
				      word0))
		     (($fx<= buffer.offset-past port.buffer.used-size)
		      ;;WORD0 is  the first  of a UTF-16  surrogate pair
		      ;;and  the input buffer  already holds  the second
		      ;;word.
		      (let ((word1		(%word-ref buffer.offset-word1))
			    (buffer-offset	($fx+ 4 buffer-offset)))
			(if (unicode.utf-16-second-of-two-words? word1)
			    (integer->char/invalid (unicode.utf-16-decode-surrogate-pair word0 word1)
						   buffer-offset)
			  (%error-handler buffer-offset
					  "invalid 16-bit word while decoding UTF-16 characters \
                                           and expecting second word in surrogate pair"
					  word0 word1))))
		     (else
		      ;;WORD0 is  the first of a  UTF-16 surrogate pair,
		      ;;but  the input  buffer  does not  hold the  full
		      ;;second word.
		      (refill-bytevector-buffer-and-evaluate (port who)
			(if-end-of-file:
			 (%unexpected-eof-error
			  "unexpected end of file while decoding UTF-16 characters \
                           and expecting second 16-bit word in surrogate pair"
			  `(,($bytevector-u8-ref port.buffer buffer.offset-word0)
			    ,($bytevector-u8-ref port.buffer ($fxadd1 buffer.offset-word0))
			    . ,(if ($fx< buffer.offset-word1
					       port.buffer.used-size)
				   (list ($bytevector-u8-ref port.buffer buffer.offset-word1))
				 '()))))
			(if-refilling-would-block:
			 WOULD-BLOCK-OBJECT)
			(if-successful-refill:
			 (recurse buffer-offset))
			)))))

	    (($fx< buffer.offset-word0 port.buffer.used-size)
	     ;;There is only 1 byte in the input buffer.
	     (refill-bytevector-buffer-and-evaluate (port who)
	       (if-end-of-file:
		;;The  input data  is corrupted  because we  expected at
		;;least a 16 bits word to be there before EOF.
		(%unexpected-eof-error
		 "unexpected end of file after byte while decoding UTF-16 characters \
                  and expecting single word character or first word in surrogate pair"
		 `(,($bytevector-u8-ref port.buffer buffer.offset-word0))))
	       (if-refilling-would-block:
		WOULD-BLOCK-OBJECT)
	       (if-successful-refill:
		(recurse buffer-offset))
	       ))

	    (else
	     ;;The input buffer is empty.
	     (refill-bytevector-buffer-and-evaluate (port who)
	       (if-end-of-file:	(eof-object))
	       (if-refilling-would-block:
		WOULD-BLOCK-OBJECT)
	       (if-successful-refill:
		(recurse buffer-offset))
	       ))))))


;;;; GET-CHAR and LOOKAHEAD-CHAR for ports with Latin-1 transcoder

(module (%unsafe.read-char-from-port-with-fast-get-latin1-tag
	 %unsafe.peek-char-from-port-with-fast-get-latin1-tag
	 %unsafe.read/peek-char-from-port-with-latin1-codec)

  (define-inline (%unsafe.read-char-from-port-with-fast-get-latin1-tag port who)
    ;;PORT  must be  a textual  input  port with  bytevector buffer  and
    ;;Latin-1 transcoder.   Knowing that  Latin-1 characters are  1 byte
    ;;wide: we process here the simple case of one char available in the
    ;;buffer, else we call the  specialised function for reading Latin-1
    ;;characters.
    ;;
    (with-port-having-bytevector-buffer (port)
      (let recurse ((buffer.offset port.buffer.index))
	(if ($fx< buffer.offset port.buffer.used-size)
	    (begin
	      (set! port.buffer.index ($fxadd1 buffer.offset))
	      (let ((octet ($bytevector-u8-ref port.buffer buffer.offset)))
		(if (unicode.latin-1-code-point? octet)
		    ($fixnum->char octet)
		  (let ((mode (transcoder-error-handling-mode port.transcoder)))
		    (case mode
		      ((ignore)
		       ;;To ignore means jump to the next.
		       (recurse port.buffer.index))
		      ((replace)
		       #\xFFFD)
		      ((raise)
		       (raise
			(condition (make-i/o-decoding-error port)
				   (make-who-condition who)
				   (make-message-condition "invalid code point for Latin-1 coded port")
				   (make-irritants-condition (list octet ($fixnum->char octet))))))
		      (else
		       (assertion-violation who "vicare internal error: invalid error handling mode" port mode)))))))
	  (%unsafe.read/peek-char-from-port-with-latin1-codec port who 1 0)))))

  (define-inline (%unsafe.peek-char-from-port-with-fast-get-latin1-tag port who)
    ;;PORT  must be  a textual  input  port with  bytevector buffer  and
    ;;Latin-1 transcoder.   Knowing that  Latin-1 characters are  1 byte
    ;;wide: we process here the simple case of one char available in the
    ;;buffer, else we call the  specialised function for peeking Latin-1
    ;;characters.
    ;;
    (with-port-having-bytevector-buffer (port)
      (let ((buffer.offset-byte port.buffer.index))
	(if ($fx< buffer.offset-byte port.buffer.used-size)
	    (let ((octet ($bytevector-u8-ref port.buffer buffer.offset-byte)))
	      (if (unicode.latin-1-code-point? octet)
		  ($fixnum->char octet)
		(let ((mode (transcoder-error-handling-mode port.transcoder)))
		  (case mode
		    ((replace)
		     #\xFFFD)
		    ;;When peeking we cannot ignore.
		    ((ignore raise)
		     (raise
		      (condition (make-i/o-decoding-error port)
				 (make-who-condition who)
				 (make-message-condition "invalid code point for Latin-1 coded port")
				 (make-irritants-condition (list octet ($fixnum->char octet))))))
		    (else
		     (assertion-violation who "vicare internal error: invalid error handling mode" port mode))))))
	  (%unsafe.read/peek-char-from-port-with-latin1-codec port who 0 0)))))

  (define (%unsafe.read/peek-char-from-port-with-latin1-codec port who buffer-index-increment offset)
    ;;Subroutine  of %DO-READ-CHAR  or  %DO-PEEK-CHAR.  PORT  must be  a
    ;;textual input port with  bytevector buffer and Latin-1 transcoder;
    ;;such buffer must be already fully consumed.
    ;;
    ;;Refill the  input buffer  reading from  the underlying  device and
    ;;return the  a Scheme character  from the  buffer; if EOF  is found
    ;;while reading from the underlying device: return the EOF object.
    ;;
    ;;When BUFFER-INDEX-INCREMENT=1  and OFFSET=0 this function  acts as
    ;;GET-CHAR: it  reads the next  character and consumes  it advancing
    ;;the port position.
    ;;
    ;;When BUFFER-INDEX-INCREMENT=0  and OFFSET=0 this function  acts as
    ;;PEEK-CHAR:  it returns  the  next character  and  leaves the  port
    ;;position unchanged.
    ;;
    ;;When BUFFER-INDEX-INCREMENT=0  and OFFSET>0 this function  acts as
    ;;forwards PEEK-CHAR: it reads the  the character at OFFSET from the
    ;;current buffer index and leaves the port position unchanged.  This
    ;;usage is  needed when converting  EOL styles for  PEEK-CHAR.  When
    ;;this usage is desired: usually it is OFFSET=1.
    ;;
    ;;Other  combinations of  BUFFER-INDEX-INCREMENT  and OFFSET,  while
    ;;possible, should not be needed.
    ;;
    (with-port-having-bytevector-buffer (port)
      (define (%available-data buffer.offset)
	(unless ($fxzero? buffer-index-increment)
	  (port.buffer.index.incr! buffer-index-increment))
	(let ((octet ($bytevector-u8-ref port.buffer buffer.offset)))
	  (if (unicode.latin-1-code-point? octet)
	      ($fixnum->char octet)
	    (let ((mode (transcoder-error-handling-mode port.transcoder)))
	      (case mode
		((ignore)
		 ;;To ignore means jump to the next.
		 (%unsafe.read/peek-char-from-port-with-latin1-codec port who buffer-index-increment offset))
		((replace)
		 #\xFFFD)
		((raise)
		 (raise
		  (condition (make-i/o-decoding-error port)
			     (make-who-condition who)
			     (make-message-condition "invalid code point for Latin-1 coded port")
			     (make-irritants-condition (list octet ($fixnum->char octet))))))
		(else
		 (assertion-violation who "vicare internal error: invalid error handling mode" port mode)))))))
      (let ((buffer.offset ($fx+ offset port.buffer.index)))
	(maybe-refill-bytevector-buffer-and-evaluate (port who)
	  (data-is-needed-at: buffer.offset)
	  (if-end-of-file: (eof-object))
	  (if-empty-buffer-and-refilling-would-block:
	   WOULD-BLOCK-OBJECT)
	  (if-successful-refill:
	   ;;After refilling we must reload buffer indexes.
	   (%available-data ($fx+ offset port.buffer.index)))
	  (if-available-data: (%available-data buffer.offset))
	  ))))

  #| end of module |# )


;;;; GET-CHAR and LOOKAHEAD-CHAR for ports with string buffer

(define-inline (%unsafe.read-char-from-port-with-fast-get-char-tag ?port ?who)
  ;;PORT must  be a textual input  port with a Scheme  string as buffer.
  ;;We process here the simple case of one char available in the buffer,
  ;;else we call the specialised function for reading characters.
  ;;
  (let ((port ?port))
    (with-port-having-string-buffer (port)
      (let ((buffer.offset port.buffer.index))
	(if ($fx< buffer.offset port.buffer.used-size)
	    (begin
	      (set! port.buffer.index ($fxadd1 buffer.offset))
	      ($string-ref port.buffer buffer.offset))
	  (%unsafe.read/peek-char-from-port-with-string-buffer port ?who 1 0))))))

(define-inline (%unsafe.peek-char-from-port-with-fast-get-char-tag ?port ?who)
  ;;PORT must  be a textual input  port with a Scheme  string as buffer.
  ;;We process here the simple case of one char available in the buffer,
  ;;else we call the specialised function for reading characters.
  ;;
  (let ((port ?port))
    (with-port-having-string-buffer (port)
      (let ((buffer.offset-char port.buffer.index))
	(if ($fx< buffer.offset-char port.buffer.used-size)
	    ($string-ref port.buffer buffer.offset-char)
	  (%unsafe.read/peek-char-from-port-with-string-buffer port ?who 0 0))))))

(define (%unsafe.read/peek-char-from-port-with-string-buffer
	 port who buffer-index-increment offset)
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
      (unless ($fxzero? buffer-index-increment)
	(port.buffer.index.incr! buffer-index-increment))
      ($string-ref port.buffer buffer.offset))
    (let ((buffer.offset ($fx+ offset port.buffer.index)))
      (maybe-refill-string-buffer-and-evaluate (port who)
	(data-is-needed-at: buffer.offset)
	(if-end-of-file: (eof-object))
	(if-empty-buffer-and-refilling-would-block:
	 WOULD-BLOCK-OBJECT)
	(if-successful-refill:
	 ;;After refilling we must reload buffer indexes.
	 (%available-data ($fx+ offset port.buffer.index)))
	(if-available-data: (%available-data buffer.offset))))))




;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
