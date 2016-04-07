;;;Copyright (c) 2011-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; buffer handling for output ports

(case-define* flush-output-port
  (()
   (%flush-output-port (current-output-port) __who__))
  (({port open-output-port?})
   ;;Defined by R6RS.  PORT must be an  output port, either binary or textual.  Flush
   ;;any buffered output from  the buffer of PORT to the  underlying file, device, or
   ;;object.  Return unspecified values.
   ;;
   ;;See %FLUSH-OUTPUT-PORT for further details.
   ;;
   (%flush-output-port port __who__)))

(case-define* $flush-output-port
  (()
   (%flush-output-port (current-output-port) __who__))
  ((port)
   (%flush-output-port port __who__)))

;;; --------------------------------------------------------------------

(define-syntax %flush-bytevector-buffer-and-evaluate
  (syntax-rules (room-is-needed-for: if-available-room:)
    ((%flush-bytevector-buffer-and-evaluate (?port ?who)
       (room-is-needed-for: ?number-of-bytes)
       (if-available-room: . ?available-body))
     (let ((port ?port))
       (with-port-having-bytevector-buffer (port)
	 (let try-again-after-flushing-buffer ()
	   (if ($fx<= ?number-of-bytes ($fx- port.buffer.size port.buffer.index))
	       (begin . ?available-body)
	     (begin
	       (debug-assert (= port.buffer.used-size port.buffer.index))
	       (%flush-output-port port ?who)
	       (try-again-after-flushing-buffer)))))))))

(define-syntax %flush-string-buffer-and-evaluate
  (syntax-rules (room-is-needed-for: if-available-room:)
    ((%flush-string-buffer-and-evaluate (?port ?who)
       (room-is-needed-for: ?number-of-chars)
       (if-available-room: . ?available-body))
     (let ((port ?port))
       (with-port-having-string-buffer (port)
	 (let try-again-after-flushing-buffer ()
	   (if ($fx<= ?number-of-bytes ($fx- port.buffer.size port.buffer.index))
	       (begin . ?available-body)
	     (begin
	       (debug-assert (= port.buffer.used-size port.buffer.index))
	       (%flush-output-port port ?who)
	       (try-again-after-flushing-buffer)))))))))

(define (%flush-output-port port who)
  ;;PORT must be an  open output port, either binary or  textual.  Flush any buffered
  ;;output from the buffer of PORT to the underlying file, device, or object.  Return
  ;;unspecified values.
  ;;
  ;;This should be the only function to call the port's WRITE!  function.
  ;;
  ;;If PORT.WRITE!   returns an invalid  value: the state  of the port  is considered
  ;;undefined  and the  port unusable;  the port  is marked  closed to  avoid further
  ;;operations  and  a  condition  object   is  raised  with  components:  &i/o-port,
  ;;&i/o-write, &who, &message, &irritants.
  ;;
  ;;If PORT.WRITE!  returns zero written bytes or  cannot absorb all the bytes in the
  ;;buffer: this function  loops retrying until PORT.WRITE!  accepts  the data, which
  ;;may be  forever but it  is compliant  to R6RS requirement  to block as  needed to
  ;;output data.
  ;;
  ;;FIXME As  a Vicare-specific customisation: we  may add a parameter  to optionally
  ;;request raising a special exception, like "port would block", when the underlying
  ;;device cannot absorb all the data in the buffer.
  ;;
  (with-port (port)
    (unless ($fxzero? port.buffer.used-size)
      ;;The buffer is not empty.  We assume the following scenario is possible:
      ;;
      ;;         cookie.pos
      ;;             v                             device
      ;;  |----------+-------------------------------|
      ;;             |*****+*******+--------| buffer
      ;;             ^     ^       ^
      ;;             0   index  used-size
      ;;
      ;;and we want to  flush to the device all of the used  units in the buffer; for
      ;;this purpose we think of the scenario as the following:
      ;;
      ;;         cookie.pos
      ;;             v                             device
      ;;  |----------+-------------------------------|
      ;;             |*************+--------| buffer
      ;;             ^             ^
      ;;          0 = index     used-size
      ;;
      ;;and we try to  write all the data between 0 and USED-SIZE.   If we succeed we
      ;;leave the port in the following scenario:
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
	  (let* ((requested-count ($fx- buffer.used-size buffer.offset))
		 (written-count   (port.write! port.buffer buffer.offset requested-count)))
	    (if (not (and (fixnum? written-count)
			  ($fx>= written-count 0)
			  ($fx<= written-count requested-count)))
		(begin
		  ;;Avoid further operations and raise an error.
		  (port.mark-as-closed!)
		  (raise (condition (make-i/o-write-error)
				    (make-i/o-port-error port)
				    (make-who-condition who)
				    (make-message-condition "write! returned an invalid value")
				    (make-irritants-condition written-count))))
	      (cond (($fx= written-count buffer.used-size)
		     ;;Full success, all data absorbed.
		     (port.device.position.incr! written-count)
		     (port.buffer.reset-to-empty!))
		    (($fxzero? written-count)
		     ;;Failure, no data absorbed.  Try again.
		     (try-again-after-partial-write buffer.offset))
		    (else
		     ;;Partial success,  some data absorbed.   Try again
		     ;;flushing the data left in the buffer.
		     (port.device.position.incr! written-count)
		     (try-again-after-partial-write ($fx+ buffer.offset written-count)))))))))))


;;;; byte and bytevector output

(module (put-u8 put-bytevector)

  (define* (put-u8 port {octet fixnum-octet?})
    ;;Defined by R6RS.  Write OCTET to the output port and return unspecified values.
    ;;
    (%case-binary-output-port-fast-tag (port __who__)
      ((FAST-PUT-BYTE-TAG)
       (with-port-having-bytevector-buffer (port)
	 (%flush-bytevector-buffer-and-evaluate (port __who__)
	   (room-is-needed-for: 1)
	   (if-available-room:
	    (let* ((buffer.index	port.buffer.index)
		   (buffer.past		($fxadd1 buffer.index))
		   (buffer.used-size	port.buffer.used-size))
	      (debug-assert (<= buffer.index buffer.used-size))
	      ($bytevector-set! port.buffer buffer.index octet)
	      (when ($fx= buffer.index buffer.used-size)
		(set! port.buffer.used-size buffer.past))
	      (set! port.buffer.index buffer.past))))
	 (when port.buffer-mode-none?
	   (%flush-output-port port __who__))))))

  (case-define* put-bytevector
    ;;Defined by  R6RS.  START and COUNT  must be non-negative exact  integer objects
    ;;that default to 0 and:
    ;;
    ;;   (- (bytevector-length BV) START)
    ;;
    ;;respectively.   BV   must  have  a   length  of  at  least   START+COUNT.   The
    ;;PUT-BYTEVECTOR procedure writes  the COUNT bytes of the  bytevector BV starting
    ;;at  index START  to  the  output port.   The  PUT-BYTEVECTOR procedure  returns
    ;;unspecified values.
    ;;
    ((port {bv bytevector?})
     (%case-binary-output-port-fast-tag (port __who__)
       ((FAST-PUT-BYTE-TAG)
	(%put-bytevector port bv 0 ($bytevector-length bv) __who__))))

    ((port {bv bytevector?} {start fixnum-start-index?})
     (assert-start-index-for-bytevector bv start)
     (%case-binary-output-port-fast-tag (port __who__)
       ((FAST-PUT-BYTE-TAG)
	(%put-bytevector port bv start ($fx- ($bytevector-length bv) start) __who__))))

    ((port {bv bytevector?} {start fixnum-start-index?} {count fixnum-count?})
     (assert-start-index-for-bytevector bv start)
     (assert-count-from-start-index-for-bytevector bv start count)
     (%case-binary-output-port-fast-tag (port __who__)
       ((FAST-PUT-BYTE-TAG)
	(%put-bytevector port bv start count __who__)))))

  (define (%put-bytevector port src.bv src.start count who)
    ;;Write COUNT bytes from the bytevector SRC.BV to the binary output PORT starting
    ;;at offset SRC.START.  Return unspecified values.
    ;;
    (with-port-having-bytevector-buffer (port)
      ;;Write octets to the  buffer and, when the buffer fills  up, to the underlying
      ;;device.
      (let try-again-after-flushing-buffer ((src.start	src.start)
					    (count	count)
					    (room		(port.buffer.room)))
	(cond (($fxzero? room)
	       ;;The buffer is full.
	       (%flush-output-port port who)
	       (try-again-after-flushing-buffer src.start count (port.buffer.room)))
	      (($fx<= count room)
	       ;;Success!!! There is  enough room in the buffer for  all of the COUNT
	       ;;octets.
	       ($bytevector-copy!/count src.bv src.start port.buffer port.buffer.index count)
	       (port.buffer.index.incr! count)
	       (when ($fx< port.buffer.used-size port.buffer.index)
		 (set! port.buffer.used-size port.buffer.index))
	       (when port.buffer-mode-none?
		 (%flush-output-port port who)))
	      (else
	       ;;The buffer can hold some but not all of the COUNT bytes.
	       (debug-assert ($fx> count room))
	       ($bytevector-copy!/count src.bv src.start port.buffer port.buffer.index room)
	       (set! port.buffer.index     port.buffer.size)
	       (set! port.buffer.used-size port.buffer.size)
	       (%flush-output-port port who)
	       (try-again-after-flushing-buffer ($fx+ src.start room)
						($fx- count room)
						(port.buffer.room)))))))

  #| end of module |# )


;;;; low-level put-char functions

(module PUT-CHAR-PROCEDURES
  (%put-char-to-port-with-fast-char-tag
   %put-char-to-port-with-fast-utf8-tag
   %put-char-utf8-multioctet-char
   %put-char-to-port-with-fast-utf16xe-tag
   %put-char-to-port-with-fast-latin1-tag)

;;; --------------------------------------------------------------------
;;; PUT-CHAR for port with string buffer

  (define (%put-char-to-port-with-fast-char-tag port ch code-point who)
    (with-port-having-string-buffer (port)
      (let retry-after-flushing-buffer ()
	(let* ((buffer.offset	port.buffer.index)
	       (buffer.past	($fxadd1 buffer.offset)))
	  (if ($fx<= buffer.past port.buffer.size)
	      (begin
		($string-set! port.buffer buffer.offset ch)
		(set! port.buffer.index buffer.past)
		(when ($fx> buffer.past port.buffer.used-size)
		  (set! port.buffer.used-size buffer.past)))
	    (begin
	      (%flush-output-port port who)
	      (retry-after-flushing-buffer)))))))

;;; --------------------------------------------------------------------
;;; PUT-CHAR for port with bytevector buffer and UTF-8 transcoder

  (define-inline (%put-char-to-port-with-fast-utf8-tag ?port ch code-point who)
    ;;Write to  PORT the character  CODE-POINT encoded  by UTF-8.  Expand  inline the
    ;;common  case  of   single-octet  encoding,  call  a   function  for  multioctet
    ;;characters.
    ;;
    (let ((port ?port))
      (if (unicode::utf-8-single-octet-code-point? code-point)
	  (with-port-having-bytevector-buffer (port)
	    (let retry-after-flushing-buffer ()
	      (let* ((buffer.offset	port.buffer.index)
		     (buffer.past		($fxadd1 buffer.offset)))
		(if ($fx<= buffer.past port.buffer.size)
		    (begin
		      ($bytevector-set! port.buffer buffer.offset code-point)
		      (set! port.buffer.index buffer.past)
		      (when ($fx> buffer.past port.buffer.used-size)
			(set! port.buffer.used-size buffer.past)))
		  (begin
		    (%flush-output-port port who)
		    (retry-after-flushing-buffer))))))
	(%put-char-utf8-multioctet-char port ch code-point who))))

  (define (%put-char-utf8-multioctet-char port ch code-point who)
    ;;Write to  PORT the possibly multioctet  CODE-POINT encoded by UTF-8.   No error
    ;;handling is performed because UTF-8 can encode all the Unicode characters.
    ;;
    (cond ((unicode::utf-8-single-octet-code-point? code-point)
	   (let ((octet0 (unicode::utf-8-encode-single-octet code-point)))
	     (with-port-having-bytevector-buffer (port)
	       (let retry-after-flushing-buffer ()
		 (let* ((buffer.offset-octet0	port.buffer.index)
			(buffer.past		($fxadd1 buffer.offset-octet0)))
		   (define-inline (%buffer-set! offset octet)
		     ($bytevector-set! port.buffer offset octet))
		   (if ($fx<= buffer.past port.buffer.size)
		       (begin
			 (%buffer-set! buffer.offset-octet0 octet0)
			 (set! port.buffer.index buffer.past)
			 (when ($fx> buffer.past port.buffer.used-size)
			   (set! port.buffer.used-size buffer.past)))
		     (begin
		       (%flush-output-port port who)
		       (retry-after-flushing-buffer))))))))

	  ((unicode::utf-8-two-octets-code-point? code-point)
	   (let ((octet0 (unicode::utf-8-encode-first-of-two-octets  code-point))
		 (octet1 (unicode::utf-8-encode-second-of-two-octets code-point)))
	     (with-port-having-bytevector-buffer (port)
	       (let retry-after-flushing-buffer ()
		 (let* ((buffer.offset-octet0	port.buffer.index)
			(buffer.offset-octet1	($fxadd1 buffer.offset-octet0))
			(buffer.past		($fxadd1 buffer.offset-octet1)))
		   (define-inline (%buffer-set! offset octet)
		     ($bytevector-set! port.buffer offset octet))
		   (if ($fx<= buffer.past port.buffer.size)
		       (begin
			 (%buffer-set! buffer.offset-octet0 octet0)
			 (%buffer-set! buffer.offset-octet1 octet1)
			 (set! port.buffer.index buffer.past)
			 (when ($fx> buffer.past port.buffer.used-size)
			   (set! port.buffer.used-size buffer.past)))
		     (begin
		       (%flush-output-port port who)
		       (retry-after-flushing-buffer))))))))

	  ((unicode::utf-8-three-octets-code-point? code-point)
	   (let ((octet0 (unicode::utf-8-encode-first-of-three-octets  code-point))
		 (octet1 (unicode::utf-8-encode-second-of-three-octets code-point))
		 (octet2 (unicode::utf-8-encode-third-of-three-octets  code-point)))
	     (with-port-having-bytevector-buffer (port)
	       (let retry-after-flushing-buffer ()
		 (let* ((buffer.offset-octet0	port.buffer.index)
			(buffer.offset-octet1	($fxadd1 buffer.offset-octet0))
			(buffer.offset-octet2	($fxadd1 buffer.offset-octet1))
			(buffer.past		($fxadd1 buffer.offset-octet2)))
		   (define-inline (%buffer-set! offset octet)
		     ($bytevector-set! port.buffer offset octet))
		   (if ($fx<= buffer.past port.buffer.size)
		       (begin
			 (%buffer-set! buffer.offset-octet0 octet0)
			 (%buffer-set! buffer.offset-octet1 octet1)
			 (%buffer-set! buffer.offset-octet2 octet2)
			 (set! port.buffer.index buffer.past)
			 (when ($fx> buffer.past port.buffer.used-size)
			   (set! port.buffer.used-size buffer.past)))
		     (begin
		       (%flush-output-port port who)
		       (retry-after-flushing-buffer))))))))

	  (else
	   (debug-assert (unicode::utf-8-four-octets-code-point? code-point))
	   (let ((octet0 (unicode::utf-8-encode-first-of-four-octets  code-point))
		 (octet1 (unicode::utf-8-encode-second-of-four-octets code-point))
		 (octet2 (unicode::utf-8-encode-third-of-four-octets  code-point))
		 (octet3 (unicode::utf-8-encode-fourth-of-four-octets code-point)))
	     (with-port-having-bytevector-buffer (port)
	       (let retry-after-flushing-buffer ()
		 (let* ((buffer.offset-octet0	port.buffer.index)
			(buffer.offset-octet1	($fxadd1 buffer.offset-octet0))
			(buffer.offset-octet2	($fxadd1 buffer.offset-octet1))
			(buffer.offset-octet3	($fxadd1 buffer.offset-octet2))
			(buffer.past		($fxadd1 buffer.offset-octet3)))
		   (define-inline (%buffer-set! offset octet)
		     ($bytevector-set! port.buffer offset octet))
		   (if ($fx<= buffer.past port.buffer.size)
		       (begin
			 (%buffer-set! buffer.offset-octet0 octet0)
			 (%buffer-set! buffer.offset-octet1 octet1)
			 (%buffer-set! buffer.offset-octet2 octet2)
			 (%buffer-set! buffer.offset-octet3 octet3)
			 (set! port.buffer.index buffer.past)
			 (when ($fx> buffer.past port.buffer.used-size)
			   (set! port.buffer.used-size buffer.past)))
		     (begin
		       (%flush-output-port port who)
		       (retry-after-flushing-buffer))))))))))

;;; --------------------------------------------------------------------
;;; PUT-CHAR for port with bytevector buffer and UTF-16 transcoder

  (define (%put-char-to-port-with-fast-utf16xe-tag port ch code-point who endianness)
    (cond ((unicode::utf-16-single-word-code-point? code-point)
	   (let ((word0 (unicode::utf-16-encode-single-word code-point)))
	     (with-port-having-bytevector-buffer (port)
	       (let retry-after-flushing-buffer ()
		 (let* ((buffer.offset-word0	port.buffer.index)
			(buffer.past		($fx+ 2 buffer.offset-word0)))
		   (define-inline (%buffer-set! offset word)
		     ($bytevector-u16-set! port.buffer offset word endianness))
		   (if ($fx<= buffer.past port.buffer.size)
		       (begin
			 (%buffer-set! buffer.offset-word0 word0)
			 (set! port.buffer.index buffer.past)
			 (when ($fx> buffer.past port.buffer.used-size)
			   (set! port.buffer.used-size buffer.past)))
		     (begin
		       (%flush-output-port port who)
		       (retry-after-flushing-buffer))))))))
	  (else
	   (let ((word0 (unicode::utf-16-encode-first-of-two-words  code-point))
		 (word1 (unicode::utf-16-encode-second-of-two-words code-point)))
	     (with-port-having-bytevector-buffer (port)
	       (let retry-after-flushing-buffer ()
		 (let* ((buffer.offset-word0	port.buffer.index)
			(buffer.offset-word1	($fx+ 2 buffer.offset-word0))
			(buffer.past		($fx+ 2 buffer.offset-word1)))
		   (define-inline (%buffer-set! offset word)
		     ($bytevector-u16-set! port.buffer offset word endianness))
		   (if ($fx<= buffer.past port.buffer.size)
		       (begin
			 (%buffer-set! buffer.offset-word0 word0)
			 (%buffer-set! buffer.offset-word1 word1)
			 (set! port.buffer.index buffer.past)
			 (when ($fx> buffer.past port.buffer.used-size)
			   (set! port.buffer.used-size buffer.past)))
		     (begin
		       (%flush-output-port port who)
		       (retry-after-flushing-buffer))))))))))

;;; --------------------------------------------------------------------
;;; PUT-CHAR for port with bytevector buffer and Latin-1 transcoder

  (define (%put-char-to-port-with-fast-latin1-tag port ch code-point who)
    ;;Write to  PORT the character  CODE-POINT encoded  by Latin-1.  Honor  the error
    ;;handling in the PORT's transcoder, selecting #\?  as replacement character.
    ;;
    (define (%doit port ch code-point who)
      (with-port-having-bytevector-buffer (port)
	(let retry-after-flushing-buffer ()
	  (let* ((buffer.offset	port.buffer.index)
		 (buffer.past	($fxadd1 buffer.offset)))
	    (if ($fx<= buffer.past port.buffer.size)
		(begin
		  ($bytevector-set! port.buffer buffer.offset code-point)
		  (set! port.buffer.index buffer.past)
		  (when ($fx> buffer.past port.buffer.used-size)
		    (set! port.buffer.used-size buffer.past)))
	      (begin
		(%flush-output-port port who)
		(retry-after-flushing-buffer)))))))
    (if (unicode::unicode-code-point-representable-as-latin-1-code-point? code-point)
	(%doit port ch code-point who)
      (case (transcoder-error-handling-mode (port-transcoder port))
	((ignore)
	 (void))
	((replace)
	 (%doit port ch (char->integer #\?) who))
	((raise)
	 (raise
	  (condition (make-i/o-encoding-error port ch)
		     (make-who-condition who)
		     (make-message-condition "character cannot be encoded by Latin-1"))))
	(else
	 (assertion-violation who "vicare internal error: invalid error handling mode" port)))))

  #| end of module: PUT-CHAR-PROCEDURES |# )


;;;; character output

(module (write-char put-char newline)
  (import PUT-CHAR-PROCEDURES)

  (case-define* write-char
    ;;Defined by  R6RS.  Write  an encoding of  the character CH  to the  the textual
    ;;output PORT, and return unspecified values.
    ;;
    ;;If PORT is omitted, it defaults to the value returned by CURRENT-OUTPUT-PORT.
    ;;
    (({ch char?} {port textual-output-port?})
     (%do-put-char port ch __who__))
    (({ch char?})
     (%do-put-char (current-output-port) ch __who__)))

  (define* (put-char {port textual-output-port?} {ch char?})
    ;;Defined by R6RS.  Write CH to the PORT.  Return unspecified values.
    ;;
    (%do-put-char port ch __who__))

  (case-define* newline
    ;;Defined by R6RS.  This is equivalent to using WRITE-CHAR to write #\linefeed to
    ;;the textual output PORT.
    ;;
    ;;If PORT is omitted, it defaults to the value returned by CURRENT-OUTPUT-PORT.
    ;;
    (()
     (%do-put-char (current-output-port) #\newline __who__))
    (({port textual-output-port?})
     (%do-put-char port #\newline __who__)))

  (define (%do-put-char port ch who)
    (let* ((code-point	($char->fixnum ch))
	   (newline?	($fx= code-point LINEFEED-CODE-POINT))
	   (eol-bits	(%port-eol-style-bits port)))
      (%case-textual-output-port-fast-tag (port who)
	((FAST-PUT-UTF8-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%put-char-to-port-with-fast-utf8-tag port ch code-point who))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%put-char-to-port-with-fast-utf8-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%put-char-to-port-with-fast-utf8-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%put-char-to-port-with-fast-utf8-tag
		 port LINEFEED-CHAR LINEFEED-CODE-POINT who))
	       ((EOL-NEXT-LINE-TAG)
		(%put-char-to-port-with-fast-utf8-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		(%put-char-to-port-with-fast-utf8-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%put-char-to-port-with-fast-utf8-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who))
	       ((EOL-LINE-SEPARATOR-TAG)
		(%put-char-to-port-with-fast-utf8-tag
		 port LINE-SEPARATOR-CHAR LINE-SEPARATOR-CODE-POINT who))
	       (else
		(%put-char-to-port-with-fast-utf8-tag port ch code-point who)))
	   (%put-char-to-port-with-fast-utf8-tag port ch code-point who)))

	((FAST-PUT-CHAR-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%put-char-to-port-with-fast-char-tag port ch code-point who))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%put-char-to-port-with-fast-char-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%put-char-to-port-with-fast-char-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%put-char-to-port-with-fast-char-tag
		 port LINEFEED-CHAR LINEFEED-CODE-POINT who))
	       ((EOL-NEXT-LINE-TAG)
		(%put-char-to-port-with-fast-char-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		(%put-char-to-port-with-fast-char-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%put-char-to-port-with-fast-char-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who))
	       ((EOL-LINE-SEPARATOR-TAG)
		(%put-char-to-port-with-fast-char-tag
		 port LINE-SEPARATOR-CHAR LINE-SEPARATOR-CODE-POINT who))
	       (else
		(%put-char-to-port-with-fast-char-tag port ch code-point who)))
	   (%put-char-to-port-with-fast-char-tag port ch code-point who)))

	((FAST-PUT-LATIN-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%put-char-to-port-with-fast-latin1-tag port ch code-point who))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%put-char-to-port-with-fast-latin1-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%put-char-to-port-with-fast-latin1-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who)
		(%put-char-to-port-with-fast-latin1-tag
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
		(%put-char-to-port-with-fast-latin1-tag port ch code-point who)))
	   (%put-char-to-port-with-fast-latin1-tag port ch code-point who)))

	((FAST-PUT-UTF16LE-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'little))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'little))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'little)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port LINEFEED-CHAR LINEFEED-CODE-POINT who 'little))
	       ((EOL-NEXT-LINE-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who 'little))
	       ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'little)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who 'little))
	       ((EOL-LINE-SEPARATOR-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port LINE-SEPARATOR-CHAR LINE-SEPARATOR-CODE-POINT who 'little))
	       (else
		(%put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'little)))
	   (%put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'little)))

	((FAST-PUT-UTF16BE-TAG)
	 (if newline?
	     (%case-eol-style (eol-bits who)
	       ((EOL-LINEFEED-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'big))
	       ((EOL-CARRIAGE-RETURN-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'big))
	       ((EOL-CARRIAGE-RETURN-LINEFEED-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'big)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port LINEFEED-CHAR LINEFEED-CODE-POINT who 'big))
	       ((EOL-NEXT-LINE-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who 'big))
	       ((EOL-CARRIAGE-RETURN-NEXT-LINE-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port CARRIAGE-RETURN-CHAR CARRIAGE-RETURN-CODE-POINT who 'big)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port NEXT-LINE-CHAR NEXT-LINE-CODE-POINT who 'big))
	       ((EOL-LINE-SEPARATOR-TAG)
		(%put-char-to-port-with-fast-utf16xe-tag
		 port LINE-SEPARATOR-CHAR LINE-SEPARATOR-CODE-POINT who 'big))
	       (else
		(%put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'big)))
	   (%put-char-to-port-with-fast-utf16xe-tag port ch code-point who 'big))))

      (when (or ($port-buffer-mode-none? port)
		(and newline? ($port-buffer-mode-line? port)))
	(%flush-output-port port who))))

  #| end of module |# )


;;;; string output

(module (put-string)
  (import PUT-CHAR-PROCEDURES)

  (case-define* put-string
    ;;Defined by R6RS.  START and COUNT  must be non--negative exact integer objects.
    ;;STR must have a length of at least START+COUNT.
    ;;
    ;;START defaults to 0.  COUNT defaults to:
    ;;
    ;;   (- (string-length STR) START)
    ;;
    ;;The PUT-STRING procedure  writes the COUNT characters of STR  starting at index
    ;;START to the textual output PORT.  The PUT-STRING procedure returns unspecified
    ;;values.
    ;;
    (({port textual-output-port?} {str string?})
     (%put-string port str 0 ($string-length str) __who__))

    (({port textual-output-port?} {str string?} {start fixnum-start-index?})
     (assert-start-index-for-string str start)
     (%put-string port str start ($fx- ($string-length str) start) __who__))

    (({port textual-output-port?} {str string?} {start fixnum-start-index?} {char-count fixnum-count?})
     (assert-start-index-for-string str start)
     (assert-count-from-start-index-for-string str start char-count)
     (%put-string port str start char-count __who__)))

  (define (%put-string port src.str src.start char-count who)
    (define-syntax-rule (%put-it ?buffer-mode-line ?eol-bits ?put-char)
      (let next-char ((src.index src.start)
		      (src.past  ($fx+ src.start char-count)))
	(unless ($fx= src.index src.past)
	  (let* ((ch		($string-ref src.str src.index))
		 (code-point	($char->fixnum ch))
		 (newline?	($fx= code-point LINEFEED-CODE-POINT))
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
	      (%flush-output-port port who)))
	  (next-char ($fxadd1 src.index) src.past))))
    (define-inline (%put-utf16le ?port ?ch ?code-point ?who)
      (%put-char-to-port-with-fast-utf16xe-tag ?port ?ch ?code-point ?who 'little))
    (define-inline (%put-utf16be ?port ?ch ?code-point ?who)
      (%put-char-to-port-with-fast-utf16xe-tag ?port ?ch ?code-point ?who 'big))
    (let ((buffer-mode-line? ($port-buffer-mode-line? port))
	  (eol-bits          (%port-eol-style-bits port)))
      (%case-textual-output-port-fast-tag (port who)
	((FAST-PUT-UTF8-TAG)
	 (%put-it buffer-mode-line? eol-bits %put-char-to-port-with-fast-utf8-tag))
	((FAST-PUT-CHAR-TAG)
	 (%put-it buffer-mode-line? eol-bits %put-char-to-port-with-fast-char-tag))
	((FAST-PUT-LATIN-TAG)
	 (%put-it buffer-mode-line? eol-bits %put-char-to-port-with-fast-latin1-tag))
	((FAST-PUT-UTF16LE-TAG)
	 (%put-it buffer-mode-line? eol-bits %put-utf16le))
	((FAST-PUT-UTF16BE-TAG)
	 (%put-it buffer-mode-line? eol-bits %put-utf16be))))
    (when ($port-buffer-mode-none? port)
      (%flush-output-port port who)))

  #| end of module: PUT-STRING |# )


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
