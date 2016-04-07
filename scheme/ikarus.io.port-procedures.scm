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


;;;; port position

(define* (port-has-port-position? {port port?})
  ;;Defined by R6RS.  Return #t if the port supports the PORT-POSITION operation, and
  ;;#f otherwise.
  ;;
  (with-port (port)
    (and port.get-position #t)))

(define* (port-has-set-port-position!? {port port?})
  ;;Defined by R6RS.   The PORT-HAS-SET-PORT-POSITION!?  procedure returns  #t if the
  ;;port supports the SET-PORT-POSITION!  operation, and #f otherwise.
  ;;
  (with-port (port)
    (and port.set-position! #t)))

(define* (port-position {port port?})
  ;;Defined  by R6RS.   For a  binary port,  PORT-POSITION returns  the index  of the
  ;;position at which the next octet would be  read from or written to the port as an
  ;;exact non-negative integer object.
  ;;
  ;;For    a   textual    port,    PORT-POSITION   returns    a    value   of    some
  ;;implementation-dependent type representing the port's position; this value may be
  ;;useful only as the POS argument to SET-PORT-POSITION!, if the latter is supported
  ;;on the port (see below).
  ;;
  ;;If the  port does not  support the  operation, PORT-POSITION raises  an exception
  ;;with condition type "&assertion".
  ;;
  ;;*NOTE* For a textual port, the port position may or may not be an integer object.
  ;;If it is an integer object, the integer object does not necessarily correspond to
  ;;a octet or character position.
  ;;
  (%port-position __who__ port))

(define (%port-position who port)
  ;;Return the current port position for PORT.
  ;;
  (with-port (port)
    (let ((device-position (%device-position who port)))
      ;;DEVICE-POSITION is  the position  in the  underlying device,  but we  have to
      ;;return the port  position taking into account the offset  in the input/output
      ;;buffer.
      (if port.last-operation-was-output?
	  (+ device-position port.buffer.index)
	(- device-position (- port.buffer.used-size port.buffer.index))))))

(define (%device-position who port)
  ;;Return the current device position for PORT.
  ;;
  (with-port (port)
    (let ((getpos port.get-position))
      (cond ((procedure? getpos)
	     ;;The port has a device whose position cannot be tracked by the cookie's
	     ;;POS field.
	     (receive-and-return (position)
		 (getpos)
	       (unless (and (or (fixnum? position)
				(bignum? position))
			    (>= position 0))
		 (expression-return-value-violation who
		   "invalid value returned by get-position"
		   1 position))))
	    ((and (boolean? getpos) getpos)
	     ;;The  cookie's  POS  field  correctly tracks  the  current
	     ;;device position.
	     port.device.position)
	    (else
	     (procedure-argument-violation who
	       "port does not support port-position operation"
	       port))))))

(define (%port-position/tracked-position who port)
  ;;If the port  supports the GET-POSITION operation: use its  own policy; else trust
  ;;the value in the POS cookie field.
  ;;
  (with-port (port)
    (let ((device-position (%device-position/tracked-position who port)))
      (if port.last-operation-was-output?
	  (+ device-position port.buffer.index)
	(- device-position (- port.buffer.used-size port.buffer.index))))))

(define (%device-position/tracked-position who port)
  ;;If the port  supports the GET-POSITION operation: use its  own policy; else trust
  ;;the value in the POS cookie field.
  ;;
  (with-port (port)
    (if port.get-position
	(%device-position who port)
      port.device.position)))

;;; --------------------------------------------------------------------

(define* (set-port-position! {port port?} {requested-port-position port-position?})
  ;;Defined by R6RS.   If PORT is a binary port,  REQUESTED-PORT-POSITION should be a
  ;;non-negative   exact   integer   object.    If    PORT   is   a   textual   port,
  ;;REQUESTED-PORT-POSITION should be the return value  of a call to PORT-POSITION on
  ;;PORT.
  ;;
  ;;The  SET-PORT-POSITION!  procedure  raises   an  exception  with  condition  type
  ;;&ASSERTION if  the port  does not  support the operation,  and an  exception with
  ;;condition  type &I/O-INVALID-POSITION  if REQUESTED-PORT-POSITION  is not  in the
  ;;range of valid positions of PORT.  Otherwise, it sets the current position of the
  ;;port to POS.  If PORT is an output port, SET-PORT-POSITION!  first flushes PORT.
  ;;
  ;;If  PORT is  a binary  output port  and the  current position  is set  beyond the
  ;;current end of the  data in the underlying data sink, the  object is not extended
  ;;until new  data is  written at  that position.  The  contents of  any intervening
  ;;positions  are unspecified.   Binary ports  created by  OPEN-FILE-OUTPUT-PORT and
  ;;OPEN-FILE-INPUT/OUTPUT-PORT  can always  be extended  in this  manner within  the
  ;;limits of the  underlying operating system.  In other cases,  attempts to set the
  ;;port beyond  the current end of  data in the  underlying object may result  in an
  ;;exception with condition type &I/O-INVALID-POSITION.
  ;;
  (with-port (port)
    (define-syntax-rule (main)
      (let ((set-position! port.set-position!))
	(cond ((procedure? set-position!)
	       (let ((port.old-position (%port-position/tracked-position __who__ port)))
		 (unless (= port.old-position requested-port-position)
		   (%set-with-procedure port set-position! port.old-position))))
	      ((and (boolean? set-position!) set-position!)
	       (%set-with-boolean port))
	      (else
	       (assertion-violation __who__
		 "port does not support SET-PORT-POSITION! operation" port)))))

    (define (%set-with-procedure port set-position! port.old-position)
      ;;The SET-POSITION!  field is a procedure:  an underlying device exists.  If we
      ;;are in the following scenario:
      ;;
      ;;                                        dev.old-pos
      ;;                                           v
      ;; |-----------------------------------------+---------| device
      ;;     |***+**************+******************+----|buffer
      ;;         ^              ^                  ^
      ;;      old-index     new-index          used-size
      ;;    port.old-pos   port.new-pos
      ;;
      ;;we can satisfy  the request just by  moving the index in  the buffer, without
      ;;calling SET-POSITION!  and changing the device position.
      ;;
      ;;Remember that port positions are not fixnums.
      ;;
      (let* ((delta-pos        (- requested-port-position port.old-position))
	     (buffer.new-index (+ port.buffer.index delta-pos)))
	(if (and (>= buffer.new-index 0)
		 (<= buffer.new-index port.buffer.used-size))
	    (set! port.buffer.index buffer.new-index)
	  ;;We need to  change the device position.  We transform  the requested port
	  ;;position  in a  requested device  position  computed when  the buffer  is
	  ;;empty; we need to take into account the current buffer index.
	  (let ((device.new-position
		 (if port.last-operation-was-output?
		     ;;Output port: flush  the buffer and reset it  to empty.  Before
		     ;;flushing:
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
		       (%flush-output-port port __who__)
		       requested-port-position)
		   ;;Input  port: compute  the device  position, delay  resetting the
		   ;;buffer  to  after  successfully calling  SET-POSITION!.   Before
		   ;;moving the position:
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
		   (let ((delta-idx           ($fx- port.buffer.used-size port.buffer.index))
			 (device.old-position (%device-position/tracked-position __who__ port)))
		     (+ (- device.old-position delta-idx) delta-pos)))))
	    ;;If SET-POSITION!  fails we can assume nothing about the position in the
	    ;;device.
	    (set-position! device.new-position)
	    ;;Notice that  we clean the  port buffer  and update the  device position
	    ;;field AFTER having successfully called SET-POSITION!.
	    (port.buffer.reset-to-empty!)
	    (set! port.device.position device.new-position)))))

    (define (%set-with-boolean port)
      ;;The cookie's  POS field holds  a value  representing a correct  and immutable
      ;;device position.   We move the  current port  position by moving  the current
      ;;buffer index.
      ;;
      ;; dev.pos
      ;;   v
      ;;   |-------------------------------------|device
      ;;   |*******+*****************************|buffer
      ;;   ^       ^                             ^
      ;;   0     index                     used-size = size
      ;;
      ;;Note that the generally correct implementation of this case is the following,
      ;;which considers the buffer not being equal to the device:
      ;;
      ;; (let ((port.old-position (%port-position __who__ port)))
      ;;   (unless (= port.old-position requested-port-position)
      ;;     (let ((delta-pos (- requested-port-position port.old-position)))
      ;;       (if (<= 0 delta port.buffer.used-size)
      ;;           (set! port.buffer.index delta-pos)
      ;;         (%raise-port-position-out-of-range __who__ port requested-port-position)))))
      ;;
      ;;but we know that, at present, the only ports implementing this policy are the
      ;;ones returned by OPEN-BYTEVECTOR-INPUT-PORT and OPEN-STRING-INPUT-PORT, so we
      ;;can optimise with the following:
      ;;
      (unless (= requested-port-position port.buffer.index)
	(if (<= 0 requested-port-position port.buffer.used-size)
	    (set! port.buffer.index requested-port-position)
	  (%raise-port-position-out-of-range __who__ port requested-port-position))))

    (main)))

(define (%reconfigure-input-buffer-to-output-buffer port who)
  ;;Assuming PORT  is an  input port:  set the  device position  to the  current port
  ;;position taking into account the buffer index and reset the buffer to empty.  The
  ;;device position may change, but the port position is unchanged.
  ;;
  ;;After this function  call: the buffer and  the device are in the  state needed to
  ;;switch an input/output port from input to output.
  ;;
  (with-port (port)
    (let ((set-position! port.set-position!))
      (cond ((procedure? set-position!)
	     ;;An underlying device exists.  Before  moving the position the scenario
	     ;;is:
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
	     (let* ((device.old-position (%device-position/tracked-position who port))
		    (delta-idx           ($fx- port.buffer.used-size port.buffer.index))
		    (device.new-position (- device.old-position delta-idx)))
	       (set-position! device.new-position)
	       (set! port.device.position device.new-position)
	       (port.buffer.reset-to-empty!)))
	    ((and (boolean? set-position!) set-position!)
	     ;;The  cookie's POS  field  holds  a value  representing  a correct  and
	     ;;immutable  device position.   For this  port the  current position  is
	     ;;changed by moving the current buffer index.  So we do nothing here.
	     ;;
	     ;; dev.pos
	     ;;   v
	     ;;   |-------------------------------------|device
	     ;;   |*******+*****************************|buffer
	     ;;   ^       ^                             ^
	     ;;   0     index                     used-size = size
	     ;;
	     (void))
	    (else
	     ;;If PORT does not  support the set port position (for  example: it is a
	     ;;network socket), we just reset the buffer to empty state.
	     (port.buffer.reset-to-empty!))))))

(define (%reconfigure-output-buffer-to-input-buffer port who)
  ;;Assuming PORT  is an  output port: set  the device position  to the  current port
  ;;position taking into account the buffer index and reset the buffer to empty.  The
  ;;device position may change, but the port position is left unchanged.
  ;;
  ;;After this function  call: the buffer and  the device are in the  state needed to
  ;;switch an input/output port from input to output.
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
	     ;;after adjusting to keep unchanged the port position the scenario is:
	     ;;
	     ;;               dev.new-pos
	     ;;                   v                    device
	     ;;   |---------------+---------------------|
	     ;;                   |----------------------| buffer
	     ;;                   ^
	     ;;          0 = index = used-size
	     ;;
	     ;;which is fine for an input port with empty buffer.
	     (let* ((port.old-position   (%port-position who port))
		    (device.new-position port.old-position))
               (%flush-output-port port who)
	       (set-position! device.new-position)
	       (set! port.device.position device.new-position)
	       (debug-assert (zero? port.buffer.index))
	       (debug-assert (zero? port.buffer.used-size))))
	    ((and (boolean? set-position!) set-position!)
	     ;;The  cookie's POS  field  holds  a value  representing  a correct  and
	     ;;immutable  device position.   For this  port the  current position  is
	     ;;changed by moving the current buffer index.  So we do nothing here.
	     ;;
	     ;; dev.pos
	     ;;   v
	     ;;   |-------------------------------------|device
	     ;;   |*******+*****************************|buffer
	     ;;   ^       ^                             ^
	     ;;   0     index                     used-size = size
	     ;;
	     (void))
	    (else
	     ;;If PORT does not  support the set port position (for  example: it is a
	     ;;netword socket), we just reset the buffer to empty state.
	     (port.buffer.reset-to-empty!))))))


;;;; generic port functions

(define* (call-with-port {port open-port?} {proc procedure?})
  ;;Defined by  R6RS.  PROC must  accept one argument.  The  CALL-WITH-PORT procedure
  ;;calls PROC with PORT as an argument.
  ;;
  ;;If PROC returns, PORT is closed automatically and the values returned by PROC are
  ;;returned.
  ;;
  ;;If PROC does not return, PORT is not closed automatically, except perhaps when it
  ;;is possible to  prove that PORT will never  again be used for an  input or output
  ;;operation.
  ;;
  (call-with-values
      (lambda ()
	(proc port))
    (lambda args
      (%close-port port __who__)
      (apply values args))))

(define* (with-output-to-port {port open-textual-output-port?} {proc procedure?})
  ;;Defined by Ikarus.   Set PORT as the  current output port and calls  PROC with no
  ;;arguments.  The port is  the current output port only for the  extent of the call
  ;;to PROC.
  ;;
  ;;PORT must  be a textual output  port, because CURRENT-OUTPUT-PORT is  supposed to
  ;;return a textual output port.
  ;;
  (parameterize ((current-output-port port))
    (proc)))


;;;; closing ports

(define* (close-port {port port?})
  ;;Defined by R6RS.  Closes the port,  rendering the port incapable of delivering or
  ;;accepting data.  If  PORT is an output  port, it is flushed  before being closed.
  ;;This has no effect if the port has already been closed.  A closed port is still a
  ;;port.  The CLOSE-PORT procedure returns unspecified values.
  ;;
  (%close-port port __who__))

(define* (close-input-port {port input-port?})
  ;;Define by R6RS.  Close an input port.
  ;;
  (%close-port port __who__))

(define* (close-output-port {port output-port?})
  ;;Define by R6RS.  Close an output port.
  ;;
  (%close-port port __who__))

(define (%close-port port who)
  ;;Subroutine for  CLOSE-PORT, CLOSE-INPUT-PORT and CLOSE-OUTPUT-PORT.   Assume that
  ;;PORT is a port object.
  ;;
  ;;Flush data in  the buffer to the  underlying device, mark the port  as closed and
  ;;finally call the port's CLOSE function, if any.
  ;;
  (with-port (port)
    (unless port.closed?
      (when port.last-operation-was-output?
	(%flush-output-port port who))
      (port.mark-as-closed!)
      (when (procedure? port.close)
	(port.close)))))


;;;; auxiliary port functions

(define* (port-id {port port?})
  ;;Defined by Ikarus.  Return the string identifier of a port.
  ;;
  (with-port (port)
    port.id))

(module (port-uid port-hash)

  (define* (port-uid {port port?})
    ;;Defined by Vicare.  Return a gensym uniquely associated the port.
    ;;
    (%port-uid port))

  (define* (port-hash {port port?})
    ;;Defined by Vicare.  Return a hash value for a port.
    ;;
    (with-port (port)
      (let ((hash (cookie-hash port.cookie)))
	(or hash
	    (receive-and-return (hash)
		(symbol-hash (%port-uid port))
	      (set-cookie-hash! port.cookie hash))))))

  (define (%port-uid port)
    (with-port (port)
      (let ((uid (cookie-uid port.cookie)))
	(or uid
	    (receive-and-return (uid)
		(gensym "port")
	      (set-cookie-uid! port.cookie uid))))))

  #| end of module |# )

(define* (port-mode {port port?})
  ;;Defined by Ikarus.  The port mode is used only by the reader.
  ;;
  (with-port (port)
    port.mode))

(define* (set-port-mode! {port port?} {mode port-mode?})
  ;;Defined by Ikarus.  The port mode is used only by the reader.
  ;;
  (with-port (port)
    (set! port.mode mode)))

(define* (port-eof? {port open-input-port?})
  ;;Defined by  R6RS.  PORT  must be an  input port.  Return  #t if  the LOOKAHEAD-U8
  ;;procedure (if PORT is a binary port)  or the LOOKAHEAD-CHAR procedure (if PORT is
  ;;a textual port) would return the EOF object, and #f otherwise.  The operation may
  ;;block indefinitely if no  data is available but the port  cannot be determined to
  ;;be at end of file.
  ;;
  (with-port (port)
    ;;Checking the buffer status is the fastest path to the result.
    (cond (($fx< port.buffer.index port.buffer.used-size)
	   #f)
	  (port.transcoder
	   (eof-object? (lookahead-char port)))
	  (else
	   (eof-object? (lookahead-u8 port))))))

;;; --------------------------------------------------------------------

(define* (port-fd {port port?})
  ;;Defined by Vicare.  If PORT is a port  with a file descriptor as device: return a
  ;;fixnum representing the device, else return false.
  ;;
  (with-port (port)
    (and port.fd-device?
	 port.device)))

(define* (port-set-non-blocking-mode! {port port-with-file-descriptor?})
  ;;Defined by  Vicare.  Set non-blocking  mode for PORT; return  unspecified values.
  ;;PORT must have a file descriptor as underlying device.
  ;;
  (let ((rv (capi::platform-fd-set-non-blocking-mode (with-port (port)
						      port.device))))
    (when ($fxnegative? rv)
      (%raise-io-error __who__ rv port))))

(define* (port-unset-non-blocking-mode! {port port-with-file-descriptor?})
  ;;Defined by Vicare.  Unset non-blocking  mode for PORT; return unspecified values.
  ;;PORT must have a file descriptor as underlying device.
  ;;
  (let ((rv (capi::platform-fd-unset-non-blocking-mode (with-port (port)
							port.device))))
    (when ($fxnegative? rv)
      (%raise-io-error __who__ rv port))))

(define* (port-in-non-blocking-mode? {port port?})
  ;;Defined by Vicare.   Query PORT for its non-blocking mode;  if successful: return
  ;;true if the port  is in non-blocking mode, false otherwise.   If an error occurs:
  ;;raise an exception.
  ;;
  (with-port (port)
    (and port.fd-device?
	 (let ((rv (capi::platform-fd-ref-non-blocking-mode port.device)))
	   (if (boolean? rv)
	       rv
	     (%raise-io-error __who__ rv port))))))

;;; --------------------------------------------------------------------

(define* (port-putprop {port port?} {key symbol?} value)
  (putprop (port-uid port) key value))

(define* (port-getprop {port port?} {key symbol?})
  (getprop (port-uid port) key))

(define* (port-remprop {port port?} {key symbol?})
  (remprop (port-uid port) key))

(define* (port-property-list {port port?})
  (property-list (port-uid port)))

;;; --------------------------------------------------------------------

(define* (reset-input-port! {port input-port?})
  (with-port (port)
    (set! port.buffer.index port.buffer.used-size)))

(define* (reset-output-port! {port output-port?})
  (with-port (port)
    (set! port.buffer.index 0)))

;;; --------------------------------------------------------------------

(define* (port-dump-status {port port?})
  (define err-port
    (current-error-port))
  (define-syntax-rule (%display thing)
    (display thing err-port))
  (define-syntax-rule (%newline)
    (newline err-port))
  (with-port (port)
    (%display "port-id: ")			(%display (port-id port))
    (%newline)
    (%display "port.buffer.index: ")		(%display port.buffer.index)
    (%newline)
    (%display "port.buffer.used-size: ")	(%display port.buffer.used-size)
    (%newline)
    ))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
