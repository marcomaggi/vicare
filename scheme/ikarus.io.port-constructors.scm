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


;;;; custom ports

(module (make-custom-binary-input-port
	 make-custom-binary-output-port
	 make-custom-binary-input/output-port
	 make-custom-textual-input-port
	 make-custom-textual-output-port
	 make-custom-textual-input/output-port)


;;;; custom ports helpers

(define (%make-custom-binary-port attributes identifier read! write! get-position set-position! close)
  ;;Build and return a new custom binary port, either input or output.  It is used by
  ;;the following functions:
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
     ($make-port ($fxior attributes GUARDED-PORT-TAG)
		 buffer.index buffer.used-size buffer transcoder identifier
		 read! write! get-position set-position! close cookie))))

(define (%make-custom-textual-port attributes identifier read! write! get-position set-position! close)
  ;;Build and return a  new custom textual port, either input or  output.  It is used
  ;;by the following functions:
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
     ($make-port ($fxior attributes GUARDED-PORT-TAG)
		 buffer.index buffer.used-size buffer transcoder identifier
		 read! write! get-position set-position! close cookie))))


;;;; custom binary ports

(define* (make-custom-binary-input-port {identifier	port-identifier?}
					{read!		procedure?}
					{get-position	false-or-procedure?}
					{set-position!	false-or-procedure?}
					{close		false-or-procedure?})
  ;;Defined by R6RS.  Return a newly created  binary input port whose octet source is
  ;;an arbitrary algorithm represented by the  READ!  procedure.  ID must be a string
  ;;naming the new  port, provided for informational purposes only.   READ! must be a
  ;;procedure and should  behave as specified below; it will  be called by operations
  ;;that perform binary input.
  ;;
  ;;Each of the  remaining arguments may be  false; if any of those  arguments is not
  ;;false, it must be a procedure and should behave as specified below.
  ;;
  ;;(READ! BYTEVECTOR START COUNT)
  ;;	START will be a non--negative exact  integer object, COUNT will be a positive
  ;;	exact integer object, and BYTEVECTOR will  be a bytevector whose length is at
  ;;	least START+COUNT.
  ;;
  ;;	The READ! procedure should obtain up to COUNT bytes from the byte source, and
  ;;	should write those bytes into BYTEVECTOR  starting at index START.  The READ!
  ;;	procedure should return an exact  integer object.  This integer object should
  ;;	represent the number of bytes that it  has read.  To indicate an end of file,
  ;;	the READ!  procedure should write no bytes and return 0.
  ;;
  ;;(GET-POSITION)
  ;;	The  GET-POSITION procedure  (if  supplied) should  return  an exact  integer
  ;;	object representing the current position of the input port.  If not supplied,
  ;;	the custom port will not support the PORT-POSITION operation.
  ;;
  ;;(SET-POSITION! POS)
  ;;	POS  will  be  a  non--negative  exact  integer  object.   The  SET-POSITION!
  ;;	procedure (if supplied) should set the position of the input port to POS.  If
  ;;	not  supplied,  the  custom  port will  not  support  the  SET-PORT-POSITION!
  ;;	operation.
  ;;
  ;;(CLOSE)
  ;;	The  CLOSE  procedure (if  supplied)  should  perform  any actions  that  are
  ;;	necessary when the input port is closed.
  ;;
  ;;*Implementation  responsibilities:*  The  implementation must  check  the  return
  ;;values of READ! and  GET-POSITION only when it actually calls them  as part of an
  ;;I/O operation  requested by the program.   The implementation is not  required to
  ;;check  that these  procedures otherwise  behave as  described.  If  they do  not,
  ;;however, the behavior of the resulting port is unspecified.
  ;;
  (let ((attributes		BINARY-INPUT-PORT-BITS)
	(write!			#f))
    (%make-custom-binary-port attributes identifier read! write! get-position set-position! close)))

(define* (make-custom-binary-output-port {identifier	port-identifier?}
					 {write!	procedure?}
					 {get-position	false-or-procedure?}
					 {set-position!	false-or-procedure?}
					 {close		false-or-procedure?})
  ;;Defined by R6RS.  Return a newly created binary output port whose byte sink is an
  ;;arbitrary algorithm  represented by the  WRITE! procedure.   ID must be  a string
  ;;naming the new port, provided for informational purposes only.  WRITE!  must be a
  ;;procedure and should  behave as specified below; it will  be called by operations
  ;;that perform binary output.
  ;;
  ;;Each of the  remaining arguments may be  false; if any of those  arguments is not
  ;;false, it must be  a procedure and should behave as  specified in the description
  ;;of MAKE-CUSTOM-BINARY-INPUT-PORT.
  ;;
  ;;(WRITE! BYTEVECTOR START count)
  ;;	START and  COUNT will be  non-negative exact integer objects,  and BYTEVECTOR
  ;;	will be a bytevector whose length is at least START+COUNT.
  ;;
  ;;	The WRITE! procedure should write up  to COUNT bytes from BYTEVECTOR starting
  ;;	at index START to  the byte sink.  In any case,  the WRITE!  procedure should
  ;;	return the number of bytes that it wrote, as an exact integer object.
  ;;
  ;;*Implementation  responsibilities:*  The  implementation must  check  the  return
  ;;values of WRITE! only when it actually  calls WRITE!  as part of an I/O operation
  ;;requested  by the  program.  The  implementation is  not required  to check  that
  ;;WRITE!  otherwise behaves as described.  If it does not, however, the behavior of
  ;;the resulting port is unspecified.
  ;;
  (let ((attributes		BINARY-OUTPUT-PORT-BITS)
	(read!			#f))
    (%make-custom-binary-port attributes identifier read! write! get-position set-position! close)))

(define* (make-custom-binary-input/output-port {identifier	port-identifier?}
					       {read!		procedure?}
					       {write!		procedure?}
					       {get-position	false-or-procedure?}
					       {set-position!	false-or-procedure?}
					       {close		false-or-procedure?})
  ;;Defined by  R6RS.  Return  a newly  created binary  input/output port  whose byte
  ;;source and  sink are  arbitrary algorithms  represented by  the READ!  and WRITE!
  ;;procedures.
  ;;
  ;;ID must  be a  string naming  the new port,  provided for  informational purposes
  ;;only.
  ;;
  ;;READ! and  WRITE!  must  be procedures,  and should behave  as specified  for the
  ;;MAKE-CUSTOM-BINARY-INPUT-PORT and MAKE-CUSTOM-BINARY-OUTPUT-PORT procedures.
  ;;
  ;;Each of the  remaining arguments may be  false; if any of those  arguments is not
  ;;false, it must be  a procedure and should behave as  specified in the description
  ;;of MAKE-CUSTOM-BINARY-INPUT-PORT.
  ;;
  (let ((attributes ($fxior BINARY-OUTPUT-PORT-BITS INPUT/OUTPUT-PORT-TAG)))
    (%make-custom-binary-port attributes identifier read! write! get-position set-position! close)))


;;;; custom textual ports

(define* (make-custom-textual-input-port {identifier	port-identifier?}
					 {read!		procedure?}
					 {get-position	false-or-procedure?}
					 {set-position!	false-or-procedure?}
					 {close		false-or-procedure?})
  ;;Define by R6RS.  Return a newly created textual input port whose character source
  ;;is an  arbitrary algorithm  represented by  the READ!  procedure.   ID must  be a
  ;;string  naming the  new port,  provided for  informational purposes  only.  READ!
  ;;must be a  procedure and should behave  as specified below; it will  be called by
  ;;operations that perform textual input.
  ;;
  ;;Each of the  remaining arguments may be  false; if any of those  arguments is not
  ;;false, it must be a procedure and should behave as specified below.
  ;;
  ;;(READ! STRING START COUNT)
  ;;	START will be a non--negative exact  integer object, COUNT will be a positive
  ;;	exact integer object,  and STRING will be  a string whose length  is at least
  ;;	START+COUNT.
  ;;
  ;;	The READ!  procedure should obtain up  to COUNT characters from the character
  ;;	source,  and should  write those  characters  into STRING  starting at  index
  ;;	START.   The  READ!    procedure  should  return  an   exact  integer  object
  ;;	representing the  number of characters that  it has written.  To  indicate an
  ;;	end of file, the READ!  procedure should write no bytes and return 0.
  ;;
  ;;(GET-POSITION)
  ;;	The GET-POSITION procedure  (if supplied) should return a  single value.  The
  ;;	return value should represent the current position of the input port.  If not
  ;;	supplied, the custom port will not support the PORT-POSITION operation.
  ;;
  ;;(SET-POSITION! POS)
  ;;	The  SET-POSITION! procedure  (if supplied)  should set  the position  of the
  ;;	input port to POS  if POS is the return value of a  call to GET-POSITION.  If
  ;;	not  supplied,  the  custom  port will  not  support  the  SET-PORT-POSITION!
  ;;	operation.
  ;;
  ;;(CLOSE)
  ;;	The  CLOSE  procedure (if  supplied)  should  perform  any actions  that  are
  ;;	necessary when the input port is closed.
  ;;
  ;;The  port may  or may  not have  an  an associated  transcoder; if  it does,  the
  ;;transcoder is implementation-dependent.
  ;;
  ;;*Implementation  responsibilities:*  The  implementation must  check  the  return
  ;;values of READ! and  GET-POSITION only when it actually calls them  as part of an
  ;;I/O operation  requested by the program.   The implementation is not  required to
  ;;check  that these  procedures otherwise  behave as  described.  If  they do  not,
  ;;however, the behavior of the resulting port is unspecified.
  ;;
  (let ((attributes	FAST-GET-CHAR-TAG)
	(write!		#f))
    (%make-custom-textual-port attributes identifier read! write! get-position set-position! close)))

(define* (make-custom-textual-output-port {identifier		port-identifier?}
					  {write!		procedure?}
					  {get-position		false-or-procedure?}
					  {set-position!	false-or-procedure?}
					  {close		false-or-procedure?})
  ;;Defined by R6RS.  Return  a newly created textual output port  whose byte sink is
  ;;an arbitrary algorithm represented by  the WRITE!  procedure.  IDENTIFIER must be
  ;;a string naming  the new port, provided for informational  purposes only.  WRITE!
  ;;must be a  procedure and should behave  as specified below; it will  be called by
  ;;operations that perform textual output.
  ;;
  ;;Each of the  remaining arguments may be  false; if any of those  arguments is not
  ;;false, it must be  a procedure and should behave as  specified in the description
  ;;of MAKE-CUSTOM-TEXTUAL-INPUT-PORT.
  ;;
  ;;(WRITE! STRING START COUNT)
  ;;	START and COUNT will be non--negative  exact integer objects, and STRING will
  ;;	be a string whose length is at least START+COUNT.
  ;;
  ;;	The  WRITE!   procedure should  write  up  to  COUNT characters  from  STRING
  ;;	starting  at index  START to  the character  sink.  In  any case,  the WRITE!
  ;;	procedure should return  the number of characters that it  wrote, as an exact
  ;;	integer object.
  ;;
  ;;The port may or may not have an associated transcoder; if it does, the transcoder
  ;;is implementation-dependent.
  ;;
  ;;*Implementation  responsibilities:*  The  implementation must  check  the  return
  ;;values of WRITE! only when it actually  calls WRITE!  as part of an I/O operation
  ;;requested  by the  program.  The  implementation is  not required  to check  that
  ;;WRITE!  otherwise behaves as described.  If it does not, however, the behavior of
  ;;the resulting port is unspecified.
  ;;
  (let ((attributes	FAST-PUT-CHAR-TAG)
	(read!	#f))
    (%make-custom-textual-port attributes identifier read! write! get-position set-position! close)))

(define* (make-custom-textual-input/output-port {identifier	port-identifier?}
						{read!		procedure?}
						{write!		procedure?}
						{get-position	false-or-procedure?}
						{set-position!	false-or-procedure?}
						{close		false-or-procedure?})
  ;;Defined by R6RS.  Return a newly  created textual input/output port whose textual
  ;;source and  sink are  arbitrary algorithms  represented by  the READ!  and WRITE!
  ;;procedures.
  ;;
  ;;IDENTIFIER  must be  a string  naming the  new port,  provided for  informational
  ;;purposes only.
  ;;
  ;;READ!  and  WRITE!  must be  procedures, and should  behave as specified  for the
  ;;MAKE-CUSTOM-TEXTUAL-INPUT-PORT and MAKE-CUSTOM-TEXTUAL-OUTPUT-PORT procedures.
  ;;
  ;;Each of the  remaining arguments may be  false; if any of those  arguments is not
  ;;false, it must be  a procedure and should behave as  specified in the description
  ;;of MAKE-CUSTOM-TEXTUAL-INPUT-PORT.
  ;;
  (let ((attributes ($fxior FAST-PUT-CHAR-TAG INPUT/OUTPUT-PORT-TAG)))
    (%make-custom-textual-port attributes identifier read! write! get-position set-position! close)))


;;;; end of custom ports module

#| end of module |# )


;;;; core bytevector input ports constructor

(case-define* open-bytevector-input-port
  ((bv)
   (open-bytevector-input-port bv #f))
  (({bv bytevector?} {maybe-transcoder false-or-transcoder?})
   ;;Defined by R6RS.  MAYBE-TRANSCODER must be either a transcoder or false.
   ;;
   ;;The OPEN-BYTEVECTOR-INPUT-PORT procedure  returns an input port  whose bytes are
   ;;drawn from BV.  If TRANSCODER is specified, it becomes the transcoder associated
   ;;with the returned port.
   ;;
   ;;If MAYBE-TRANSCODER is false or absent, the  port will be a binary port and will
   ;;support  the PORT-POSITION  and SET-PORT-POSITION!   operations.  Otherwise  the
   ;;port  will be  a textual  port, and  whether it  supports the  PORT-POSITION and
   ;;SET-PORT-POSITION!  operations  will be  implementation dependent  (and possibly
   ;;transcoder-dependent).
   ;;
   ;;If BV is  modified after OPEN-BYTEVECTOR-INPUT-PORT has been  called, the effect
   ;;on the returned port is unspecified.
   ;;
   ;;The input bytevector is itself the buffer!!!   The port is in a state equivalent
   ;;to the following:
   ;;
   ;;                                           device position
   ;;                                                  v
   ;;   |----------------------------------------------| device
   ;;   |*******************+**************************| buffer
   ;;   ^                   ^                          ^
   ;;   0            index = port position       used-size = size
   ;;
   ;;the device position  equals the bytevector length and its  value in the cookie's
   ;;POS field is never mutated.
   (let* ((bv.len		($bytevector-length bv))
	  (attributes		($fxior (%select-input-fast-tag-from-transcoder __who__ maybe-transcoder)
					(%select-eol-style-from-transcoder      __who__ maybe-transcoder)))
	  (buffer.index		0)
	  (buffer.used-size	bv.len)
	  (buffer		bv)
	  (transcoder		maybe-transcoder)
	  (identifier		"*bytevector-input-port*")
	  (read!		all-data-in-buffer)
	  (write!		#f)
	  (get-position		#t)
	  (set-position!	#t)
	  (close		#f)
	  (cookie		(default-cookie #f)))
     (set-cookie-pos! cookie bv.len)
     ($make-port attributes buffer.index buffer.used-size buffer transcoder identifier
		 read! write! get-position set-position! close cookie))))


;;;; core bytevector output ports constructor

(case-define* open-bytevector-output-port
  (()
   (open-bytevector-output-port #f))
  (({maybe-transcoder false-or-transcoder?})
   ;;Defined by R6RS.  MAYBE-TRANSCODER must be either a transcoder or false.
   ;;
   ;;The OPEN-BYTEVECTOR-OUTPUT-PORT procedure returns two values: an output port and
   ;;an extraction  procedure.  The output port  accumulates the bytes written  to it
   ;;for later extraction by the procedure.
   ;;
   ;;If MAYBE-TRANSCODER is  a transcoder, it becomes the  transcoder associated with
   ;;the port.   If MAYBE-TRANSCODER is  false or absent, the  port will be  a binary
   ;;port  and will  support  the PORT-POSITION  and SET-PORT-POSITION!   operations.
   ;;Otherwise  the  port  will be  a  textual  port,  and  whether it  supports  the
   ;;PORT-POSITION  and SET-PORT-POSITION!   operations  is implementation  dependent
   ;;(and possibly transcoder-dependent).
   ;;
   ;;The  extraction  procedure  takes  no  arguments.  When  called,  it  returns  a
   ;;bytevector consisting  of all  the port's accumulated  bytes (regardless  of the
   ;;port's  current position),  removes the  accumulated  bytes from  the port,  and
   ;;resets the port's position.
   ;;
   ;; -------------------------------------------------------------------------------
   ;;
   ;;The most common use of this port type is to append bytes and finally extract the
   ;;whole output bytevector:
   ;;
   ;;  (call-with-values
   ;;      open-bytevector-output-port
   ;;    (lambda (port extract)
   ;;      (put-bytevector port '#vu8(1 2 3))
   ;;      ...
   ;;      (extract)))
   ;;
   ;;for this reason we implement the device of the port to be somewhat efficient for
   ;;such use.
   ;;
   ;;The device is a  struct instance stored in the cookie; the  field BVS holds null
   ;;or a list  of accumulated bytevectors; the  field LEN holds the  total number of
   ;;bytes accumulated so far.
   ;;
   ;;Whenever data  is flushed  from the buffer  to the device:  a new  bytevector is
   ;;prepended to BVS, and LEN is incremented accordingly.  When the extract function
   ;;is invoked:  BVS is reversed and  concatenated obtaining a single  bytevector of
   ;;length LEN.
   ;;
   ;;There is only one function that can be applied to the port and causes the device
   ;;to  be handled  in  a  different way:  SET-PORT-POSITION!.   When  the new  port
   ;;position is  before the  end of  the data:  the internal  function SET-POSITION!
   ;;converts BVS to a list holding a single full bytevector (LEN is left unchanged).
   ;;
   ;;Whenever  BVS holds  a single  bytevector  and the  position is  less than  such
   ;;bytevector length: it means that SET-PORT-POSITION!  was used.
   ;;
   ;;Remember that  bytevectors hold at  most a number of  bytes equal to  the return
   ;;value of GREATEST-FIXNUM.
   ;;
   ;;Notice how the internal functions are closures upon the cookie, not the port.
   ;;
   (define-struct device
     (len bvs))

   (define-constant THE-COOKIE
     (default-cookie (make-device 0 '())))

   (define (main)
     (let* ((attributes		(%select-output-fast-tag-from-transcoder __who__ maybe-transcoder PORT-WITH-EXTRACTION-TAG
									 (%select-eol-style-from-transcoder __who__ maybe-transcoder)
									 DEFAULT-OTHER-ATTRS))
	    (buffer.index	0)
	    (buffer.used-size	0)
	    (buffer		($make-bytevector (bytevector-port-buffer-size)))
	    (identifier		"*bytevector-output-port*")
	    (read!		#f)
	    (get-position	#t)
	    (close		#f))

       (define-constant port
	 ($make-port attributes buffer.index buffer.used-size buffer
		     maybe-transcoder identifier
		     read!
		     open-bytevector-output-port/write!
		     get-position
		     open-bytevector-output-port/set-position!
		     close
		     THE-COOKIE))

       (define* (open-bytevector-output-port/extract)
	 ;;The extraction function.  Flush the buffer to the device list, convert the
	 ;;device  list to  a single  bytevector.  Return  the single  bytevector and
	 ;;reset the port to its empty state.
	 ;;
	 ;;This function can be called also when the port has been closed.
	 ;;
	 (with-port (port)
	   (%flush-output-port port __who__)
	   (receive-and-return (bv)
	       (%serialise-device! __who__ #t)
	     (port.buffer.reset-to-empty!)
	     (set! port.device.position 0))))

       (values port open-bytevector-output-port/extract)))

;;; --------------------------------------------------------------------

   (define (%serialise-device! who reset?)
     (let ((D (cookie-dest THE-COOKIE)))
       (cond (($fxzero? ($device-len D))
	      ;;No bytes accumulated, the device is empty.
	      '#vu8())
	     ((null? (cdr ($device-bvs D)))
	      ;;The device  contains some bytes  and it has already  been serialised:
	      ;;the list of bytevectors contains a single item.
	      (when reset?
		(set-cookie-dest! THE-COOKIE (make-device 0 '())))
	      ;;Return the single bytevector in the list.
	      (car ($device-bvs D)))
	     (else
	      ;;The device contains some bytes and the device needs to be serialised:
	      ;;the list of bytevectors contains 2 or more items.
	      (receive-and-return (bv)
		  ($bytevector-reverse-and-concatenate ($device-len D) ($device-bvs D))
		($set-cookie-dest! THE-COOKIE
				   (if reset?
				       (make-device 0 '())
				     (make-device ($device-len D) (list bv)))))))))

;;; --------------------------------------------------------------------

   (module (open-bytevector-output-port/write!)

     (define* (open-bytevector-output-port/write! src.bv src.start count)
       ;;Write COUNT octets from the bytevector SRC.BV, starting at offset SRC.START,
       ;;to the device.
       ;;
       (debug-assert (and (fixnum? count) (fxpositive? count)))
       (let* ((D                (cookie-dest THE-COOKIE))
	      (dev-position.cur (cookie-pos THE-COOKIE))
	      (dev-position.new (+ count dev-position.cur)))
	 ;;DANGER This  check must  not be removed  when compiling  without arguments
	 ;;validation.
	 (unless (fixnum? dev-position.new)
	   (%implementation-violation __who__
	     "request to write data to port would exceed maximum size of bytevectors"
	     dev-position.new))
	 (if ($fx< dev-position.cur ($device-len D))
	     ;;The current position was set inside the already accumulated data.
	     (%write!/overwrite src.bv src.start count
				($device-len D) ($device-bvs D)
				dev-position.cur dev-position.new)
	   ;;The current position is at the end of the already accumulated data.
	   (%write!/append src.bv src.start count ($device-bvs D) dev-position.new))
	 count))

     (define (%write!/overwrite src.bv src.start count
				device.len device.bvs
				dev-position new-dev-position)
       ;;Write data  to the device,  overwriting some  of the already  existing data.
       ;;The device is  already composed of a single bytevector  of length DEVICE.LEN
       ;;in the car of DEVICE.BVS.
       ;;
       (let* ((dst.bv   (car device.bvs))
	      (dst.len  device.len)
	      (dst.room ($fx- dst.len dev-position)))
	 (debug-assert (fixnum? dst.room))
	 (if ($fx<= count dst.room)
	     ;;The  new data  fits in  the single  bytevector.  There  is no  need to
	     ;;update the device.
	     ($bytevector-copy!/count src.bv src.start dst.bv dev-position count)
	   (begin
	     ;;The new  data goes  part in the  single bytevector and  part in  a new
	     ;;bytevector.  We need to update the device.
	     ($bytevector-copy!/count src.bv src.start dst.bv dev-position dst.room)
	     (let* ((src.start ($fx+ src.start dst.room))
		    (count     ($fx- count     dst.room)))
	       (%write!/append src.bv src.start count device.bvs new-dev-position))))))

     (define (%write!/append src.bv src.start count device.bvs new-dev-position)
       ;;Append  new data  to the  accumulated bytevectors.   We need  to update  the
       ;;device.
       ;;
       (let ((dst.bv ($make-bytevector count)))
	 ($bytevector-copy!/count src.bv src.start dst.bv 0 count)
	 (set-cookie-dest! THE-COOKIE (make-device new-dev-position (cons dst.bv device.bvs)))))

     #| end of module: OPEN-BYTEVECTOR-OUTPUT-PORT/WRITE! |# )

;;; --------------------------------------------------------------------

   (define* (open-bytevector-output-port/set-position! new-position)
     ;;NEW-POSITION has  already been validated  as exact integer by  the procedure
     ;;SET-PORT-POSITION!.    The    buffer   has    already   been    flushed   by
     ;;SET-PORT-POSITION!.  Here we only have to  verify that the value is valid as
     ;;offset inside the underlying full  bytevector.  If this validation succeeds:
     ;;SET-PORT-POSITION!  will store the position in the cookie.
     ;;
     (unless (and (fixnum? new-position)
		  (or ($fx=  new-position (cookie-pos THE-COOKIE))
		      ($fx<= new-position
			     ($bytevector-length (%serialise-device! __who__ #f)))))
       (raise
	(condition (make-who-condition __who__)
		   (make-message-condition "attempt to set bytevector output port position beyond limit")
		   (make-i/o-invalid-position-error new-position)))))

   (main)))


;;;; miscellaneous bytevector output ports functions

(case-define* call-with-bytevector-output-port
  ((proc)
   (call-with-bytevector-output-port proc #f))
  (({proc procedure?} {transcoder false-or-transcoder?})
   ;;Defined  by R6RS.   PROC must  accept  one argument.   MAYBE-TRANSCODER must  be
   ;;either a transcoder or false.
   ;;
   ;;The  CALL-WITH-BYTEVECTOR-OUTPUT-PORT  procedure  creates an  output  port  that
   ;;accumulates the bytes written  to it and calls PROC with that  output port as an
   ;;argument.
   ;;
   ;;Whenever PROC returns, a bytevector consisting  of all of the port's accumulated
   ;;bytes (regardless  of the port's current  position) is returned and  the port is
   ;;closed.
   ;;
   ;;The transcoder associated  with the output port  is determined as for  a call to
   ;;OPEN-BYTEVECTOR-OUTPUT-PORT.
   ;;
   (receive (port extract)
       (open-bytevector-output-port transcoder)
     (proc port)
     (extract))))


;;;; miscellaneous string input ports functions

(case-define* open-string-input-port
  ;;Defined  by  R6RS,  extended  by  Vicare.  Return  a  textual  input  port  whose
  ;;characters  are drawn  from STR.   The port  may or  may not  have an  associated
  ;;transcoder; if  it does, the  transcoder is implementation--dependent.   The port
  ;;should support the PORT-POSITION and SET-PORT-POSITION!  operations.
  ;;
  ;;If STR  is modified after OPEN-STRING-INPUT-PORT  has been called, the  effect on
  ;;the returned port is unspecified.
  ;;
  ((str)
   (open-string-input-port/id str "*string-input-port*" (eol-style none)))
  ((str eol-style)
   (open-string-input-port/id str "*string-input-port*" eol-style)))

(case-define* open-string-input-port/id
  ;;Defined by Ikarus.  For details see the documentation of OPEN-STRING-INPUT-PORT.
  ;;
  ;;In  this port  there is  no underlying  device: the  input string  is set  as the
  ;;buffer.
  ;;
  ((str id)
   (open-string-input-port/id str id 'none))
  (({str string?} {id port-identifier?} eol-style)
   ;;The input string is  itself the buffer!!!  The port is in  a state equivalent to
   ;;the following:
   ;;
   ;;                                           device position
   ;;                                                  v
   ;;   |----------------------------------------------| device
   ;;   |*******************+**************************| buffer
   ;;   ^                   ^                          ^
   ;;   0            index = port position       used-size = size
   ;;
   ;;the device position equals  the string length and its value  in the cookie's POS
   ;;field is never mutated.
   (let ((str.len (string-length str)))
     (unless (< str.len BUFFER-SIZE-UPPER-LIMIT)
       (error __who__ "input string length exceeds maximum supported size" str.len))
     (let ((attributes	($fxior FAST-GET-CHAR-TAG
				(or (%symbol->eol-attrs eol-style)
				    (assertion-violation __who__ "expected EOL style as argument" eol-style))
				DEFAULT-OTHER-ATTRS))
	   (buffer.index	0)
	   (buffer.used-size	str.len)
	   (buffer		str)
	   (transcoder		#t)
	   (read!		all-data-in-buffer)
	   (write!		#f)
	   (get-position	#t)
	   (set-position!	#t)
	   (close		#f)
	   (cookie		(default-cookie #f)))
       (set-cookie-pos! cookie str.len)
       ($make-port attributes buffer.index buffer.used-size buffer transcoder id
		   read! write! get-position set-position! close cookie)))))

(define* (with-input-from-string {str string?} {thunk procedure?})
  ;;Defined by  Ikarus.  THUNK must  be a procedure  and must accept  zero arguments.
  ;;STRING must be a Scheme string, and THUNK is called with no arguments.
  ;;
  ;;The STRING  is used  as argument for  OPEN-STRING-INPUT-PORT; during  the dynamic
  ;;extent of  the call to  THUNK, the  obtained port is  made the value  returned by
  ;;procedure CURRENT-INPUT-PORT; the  previous default value is  reinstated when the
  ;;dynamic extent is exited.
  ;;
  ;;When THUNK  returns, the port  is closed  automatically.  The values  returned by
  ;;THUNK are returned.
  ;;
  ;;If an escape procedure is used to escape  back into the call to THUNK after THUNK
  ;;is returned, the behavior is unspecified.
  ;;
  (parameterize ((current-input-port (open-string-input-port str)))
    (thunk)))


;;;; core string output ports constructor

(case-define* open-string-output-port
  ;;Defined by  R6RS.  Return  two values:  a textual output  port and  an extraction
  ;;procedure.  The  output port accumulates the  characters written to it  for later
  ;;extraction by the procedure.
  ;;
  ;;The port may or may not have an associated transcoder; if it does, the transcoder
  ;;is  implementation-dependent.   The port  should  support  the PORT-POSITION  and
  ;;SET-PORT-POSITION!  operations.
  ;;
  ;;The extraction  procedure takes no arguments.   When called, it returns  a string
  ;;consisting of all of the port's accumulated characters (regardless of the current
  ;;position),  removes the  accumulated characters  from  the port,  and resets  the
  ;;port's position.
  ;;
  ;;IMPLEMENTATION RESTRICTION The accumulated string  can have as maximum length the
  ;;greatest fixnum,  which means that  the device position also  can be at  most the
  ;;greatest fixnum.
  ;;
  ;; --------------------------------------------------------------------------------
  ;;
  ;;The most common use of this port type is to append characters and finally extract
  ;;the whole output bytevector:
  ;;
  ;;  (call-with-values
  ;;      open-string-output-port
  ;;    (lambda (port extract)
  ;;      (put-string port "123")
  ;;      ...
  ;;      (extract)))
  ;;
  ;;for this reason we implement the device  of the port to be somewhat efficient for
  ;;such use.
  ;;
  ;;The device  is a pair stored  in the cookie;  the cdr, called OUTPUT.STRS  in the
  ;;code holds null or  a list of accumulated strings; the  car, called OUTPUT.LEN in
  ;;the code, holds the total number of characters accumulated so far.
  ;;
  ;;Whenever data is flushed from the buffer to the device: a new string is prepended
  ;;to  OUTPUT.STRS and  OUTPUT.LEN  is incremented  accordingly.   When the  extract
  ;;function is invoked: OUTPUT.STRS is  reversed and concatenated obtaining a single
  ;;string of length OUTPUT.LEN.
  ;;
  ;;This situation is violated if SET-PORT-POSITION!   is applied to the port to move
  ;;the position before the end of the  data.  If this happens: the internal function
  ;;SET-POSITION!   converts OUTPUT.STRS  to  a  list holding  a  single full  string
  ;;(OUTPUT.LEN is left unchanged).
  ;;
  ;;Whenever OUTPUT.STRS  holds a single  string and the  position is less  than such
  ;;string length: it means that SET-PORT-POSITION!  was used.
  ;;
  ;;Remember that strings hold at most (GREATEST-FIXNUM) characters.
  ;;
  (()
   (open-string-output-port (eol-style none)))
  ((eol-style)
   (define-constant THE-COOKIE
     (default-cookie '(0 . ())))

   (define (main)
     (let* ((attributes		($fxior FAST-PUT-CHAR-TAG
					(or (%symbol->eol-attrs eol-style)
					    (assertion-violation __who__ "expected EOL style as argument" eol-style))
					DEFAULT-OTHER-ATTRS))
	    (buffer.index	0)
	    (buffer.used-size	0)
	    (buffer		($make-string (string-port-buffer-size)))
	    (identifier		"*string-output-port*")
	    (transcoder		#t)
	    (read!		#f)
	    (get-position	#t)
	    (close		#f))

       (define port
	 ($make-port attributes buffer.index buffer.used-size buffer transcoder identifier
		     read!
		     open-string-output-port/write!
		     get-position
		     open-string-output-port/set-position!
		     close THE-COOKIE))

       (define (open-string-output-port/extract)
	 ;;The extraction function.  Flush the buffer to the device list, convert the
	 ;;device list  to a single string.   Return the single string  and reset the
	 ;;port to its empty state.
	 ;;
	 ;;This function can be called also when the port has been closed.
	 ;;
	 (%flush-output-port port __who__)
	 (with-port (port)
	   (let ((str (%serialise-device-and-reset! __who__)))
	     (port.buffer.reset-to-empty!)
	     (set! port.device.position 0)
	     str)))

       (values port open-string-output-port/extract)))

;;; --------------------------------------------------------------------

   (define (%%serialise-device! who reset?)
     (let* ((dev		(cookie-dest THE-COOKIE))
	    (output.len	(car dev))
	    (output.strs	(cdr dev)))
       (cond (($fxzero? output.len)
	      ;;No bytes accumulated, the device is empty.
	      "")
	     ((null? (cdr output.strs))
	      ;;The device has already been serialised.
	      (when reset?
		(set-cookie-dest! THE-COOKIE '(0 . ())))
	      (car output.strs))
	     (else
	      (receive-and-return (str)
		  ($string-reverse-and-concatenate output.len output.strs)
		(set-cookie-dest! THE-COOKIE (if reset? '(0 . ()) `(,output.len . (,str)))))))))

   (define (%serialise-device! who)
     (%%serialise-device! who #f))

   (define (%serialise-device-and-reset! who)
     (%%serialise-device! who #t))

;;; --------------------------------------------------------------------

   (module (open-string-output-port/write!)

     (define* (open-string-output-port/write! src.str src.start count)
       ;;Write data to the device.
       ;;
       (debug-assert (and (fixnum? count) (<= 0 count)))
       (let* ((dev		(cookie-dest THE-COOKIE))
	      (output.len	(car dev))
	      (output.strs	(cdr dev))
	      (dev-position	(cookie-pos THE-COOKIE))
	      (new-dev-position	(+ count dev-position)))
	 ;;DANGER This  check must  not be removed  when compiling  without arguments
	 ;;validation.
	 (unless (fixnum? new-dev-position)
	   (%implementation-violation __who__
	     "request to write data to port would exceed maximum size of strings" new-dev-position))
	 (if ($fx< dev-position output.len)
	     ;;The current position was set inside the already accumulated data.
	     (%write!/overwrite src.str src.start count output.len output.strs dev-position
				new-dev-position)
	   ;;The current position is at the end of the already accumulated data.
	   (%write!/append src.str src.start count output.strs new-dev-position))
	 count))

     (define (%write!/overwrite src.str src.start count output.len output.strs dev-position
				new-dev-position)
       ;;Write data  to the device,  overwriting some  of the already  existing data.
       ;;The device  is already composed of  a single string of  length OUTPUT.LEN in
       ;;the car of OUTPUT.STRS.
       ;;
       (let* ((dst.str  (car output.strs))
	      (dst.len  output.len)
	      (dst.room ($fx- dst.len dev-position)))
	 (debug-assert (fixnum? dst.room))
	 (if ($fx<= count dst.room)
	     ;;The new data  fits in the single  string.  There is no  need to update
	     ;;the device.
	     ($string-copy!/count src.str src.start dst.str dev-position count)
	   (begin
	     ;;The new data goes part in the  single string and part in a new string.
	     ;;We need to update the device.
	     ($string-copy!/count src.str src.start dst.str dev-position dst.room)
	     (let* ((src.start ($fx+ src.start dst.room))
		    (count     ($fx- count     dst.room)))
	       (%write!/append src.str src.start count output.strs new-dev-position))))))

     (define (%write!/append src.str src.start count output.strs new-dev-position)
       ;;Append new data to the accumulated strings.  We need to update the device.
       ;;
       (let ((dst.str ($make-string count)))
	 ($string-copy!/count src.str src.start dst.str 0 count)
	 (set-cookie-dest! THE-COOKIE `(,new-dev-position . (,dst.str . ,output.strs)))))

     #| end of module: OPEN-STRING-OUTPUT-PORT/WRITE! |# )

;;; --------------------------------------------------------------------

   (define* (open-string-output-port/set-position! new-position)
     ;;NEW-POSITION  has already  been validated  as exact  integer by  the procedure
     ;;SET-PORT-POSITION!.     The   buffer    has    already    been   flushed    by
     ;;SET-PORT-POSITION!.  Here  we only have to  verify that the value  is valid as
     ;;offset  inside  the underlying  full  string.   If this  validation  succeeds:
     ;;SET-PORT-POSITION!  will store the position in the cookie.
     ;;
     (unless (and (fixnum? new-position)
		  (or ($fx=  new-position (cookie-pos THE-COOKIE))
		      ($fx<= new-position ($string-length (%serialise-device! __who__)))))
       (raise
	(condition (make-who-condition __who__)
		   (make-message-condition "attempt to set string output port position beyond limit")
		   (make-i/o-invalid-position-error new-position)))))

   (main)))


;;;; other string output ports functions

(define* (get-output-string {port port?})
  ;;Defined  by  Ikarus.   Return  the  string accumulated  in  the  PORT  opened  by
  ;;OPEN-STRING-OUTPUT-PORT.
  ;;
  ;;This function can be called also when the port has been closed.
  ;;
  (define-syntax-rule (wrong-port-error)
    (assertion-violation __who__ "not an output-string port" port))
  (with-port-having-string-buffer (port)
    (unless ($textual-output-port? port)
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
      (%flush-output-port port __who__)
      (let* ((dev		port.device) ;flushing the buffer can change the device!!!
	     (output.len	(car dev))
	     (output.strs	(cdr dev)))
	(set! port.device.position 0)
	(set! port.device '(0 . ()))
	(cond (($fxzero? output.len)
	       ;;No bytes accumulated, the device is empty.
	       "")
	      ((null? (cdr output.strs))
	       ;;The device has already been serialised.
	       (car output.strs))
	      (else
	       ($string-reverse-and-concatenate output.len output.strs)))))))

;;; --------------------------------------------------------------------

(define* (call-with-string-output-port {proc procedure?})
  ;;Defined by R6RS.  PROC must accept one argument.
  ;;
  ;;The  CALL-WITH-STRING-OUTPUT-PORT procedure  creates a  textual output  port that
  ;;accumulates the characters written to it and  calls PROC with that output port as
  ;;an argument.
  ;;
  ;;Whenever  PROC returns,  a string  consisting of  all of  the port's  accumulated
  ;;characters (regardless of  the port's current position) is returned  and the port
  ;;is closed.
  ;;
  ;;The port may or may not have an associated transcoder; if it does, the transcoder
  ;;is  implementation-dependent.   The port  should  support  the PORT-POSITION  and
  ;;SET-PORT-POSITION!  operations.
  ;;
  (receive (port extract)
      (open-string-output-port)
    (proc port)
    (extract)))

(define* (with-output-to-string {proc procedure?})
  ;;Defined by Ikarus.  Create a textual  output port that accumulates the characters
  ;;written  to it,  sets  it as  the  current output  port and  calls  PROC with  no
  ;;arguments.  The port is  the current output port only for the  extent of the call
  ;;to PROC.
  ;;
  ;;Whenever  PROC returns,  a string  consisting of  all of  the port's  accumulated
  ;;characters (regardless of  the port's current position) is returned  and the port
  ;;is closed.
  ;;
  (receive (port extract)
      (open-string-output-port)
    (parameterize ((current-output-port port))
      (proc))
    (extract)))


;;;; transcoded ports

(define* (transcoded-port {port open-binary-input-or-output-port?} {transcoder transcoder?})
  ;;Defined by R6RS.   The TRANSCODED-PORT procedure returns a new  textual port with
  ;;the specified TRANSCODER.  Otherwise the new  textual port's state is largely the
  ;;same as that of PORT, which must be a binary port.
  ;;
  ;;If PORT  is an input port,  the new textual port  will be an input  port and will
  ;;transcode the bytes that have not yet been  read from PORT.  If PORT is an output
  ;;port, the  new textual  port will  be an  output port  and will  transcode output
  ;;characters into bytes that are written to the byte sink represented by PORT.
  ;;
  ;;As a  side effect,  however, TRANSCODED-PORT  closes PORT in  a special  way that
  ;;allows  the  new  textual port  to  continue  to  use  the byte  source  or  sink
  ;;represented by PORT, even though PORT itself  is closed and cannot be used by the
  ;;input and output operations.
  ;;
  (with-port-having-bytevector-buffer (port)
    (let ((trans-port ($make-port ($fxior (cond (port.last-operation-was-output?
						 (%select-output-fast-tag-from-transcoder __who__ transcoder))
						(port.last-operation-was-input?
						 (%select-input-fast-tag-from-transcoder __who__ transcoder))
						(else
						 (assertion-violation __who__ "port is neither input nor output!" port)))
					  (%port-nullify-eol-style-bits port.other-attributes)
					  (%select-eol-style-from-transcoder __who__ transcoder))
				  port.buffer.index port.buffer.used-size port.buffer
				  transcoder port.id
				  port.read! port.write! port.get-position port.set-position! port.close
				  port.cookie)))
      ;;If the binary  port accumulates bytes for later extraction  by a function, we
      ;;flush the buffer now and set the buffer  mode to none; this way the buffer is
      ;;kept empty by all the output  functions.  This allows the original extraction
      ;;function to still process correctly the device.
      (when port.with-extraction?
	(%flush-output-port port __who__)
	($set-port-buffer-mode-to-none! trans-port))
      (port.mark-as-closed!)
      (%port->maybe-guarded-port trans-port))))

(define* (port-transcoder {port port?})
  ;;Defined by R6RS.   Return the transcoder associated with PORT  if PORT is textual
  ;;and has an associated transcoder, and returns false if PORT is binary or does not
  ;;have an associated transcoder.
  ;;
  (with-port (port)
    (let ((tr port.transcoder))
      (and (transcoder? tr) tr))))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
