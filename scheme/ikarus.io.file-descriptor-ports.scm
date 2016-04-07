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


;;;; platform I/O error handling

(define (%raise-eagain-error who port port-identifier)
  ;;Raise an exception to signal that a  system call was interrupted and returned the
  ;;EAGAIN errno code.
  ;;
  (raise
   (condition (make-i/o-eagain)
	      (make-who-condition who)
	      (make-message-condition (strerror EAGAIN))
	      (if port
		  (make-i/o-port-error port)
		(condition))
	      (make-irritants-condition (list port-identifier)))))

(case-define* %raise-io-error
  ;;Raise a  non-continuable exception describing  an input/output system  error from
  ;;the value of ERRNO.
  ;;
  ((who port-identifier errno base-condition)
   (raise
    (condition base-condition
	       (make-who-condition who)
	       (make-message-condition (strerror errno))
	       (case-errno errno
		 ((EACCES EFAULT)
		  ;;Why is EFAULT  included here?  Because many  functions may return
		  ;;EFAULT even  if the documentation in  the GNU C Library  does not
		  ;;mention it explicitly; see the  notes in the documentation of the
		  ;;"errno" variable.
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
   (%raise-io-error who port-identifier errno (make-error))))


;;;; helper functions for platform's descriptors

(define string->filename-func
  (make-parameter string->utf8
    (lambda (obj)
      (if (procedure? obj)
	  obj
	(procedure-argument-violation 'string->filename-func
	  "expected procedure as parameter value" obj)))))

(define filename->string-func
  (make-parameter utf8->string
    (lambda (obj)
      (if (procedure? obj)
	  obj
	(procedure-argument-violation 'filename->string-func
	  "expected procedure as parameter value" obj)))))

(define (%open-input-file-descriptor filename file-options who)
  ;;Subroutine for the functions  below opening a file for input.   Open and return a
  ;;file descriptor  referencing the  file selected  by the  string FILENAME.   If an
  ;;error occurs: raise an exception.
  ;;
  ;;R6RS states  that the  NO-CREATE, NO-FAIL  and NO-TRUNCATE  file options  have no
  ;;effect when opening  a file only for input.  At  present FILE-OPTIONS is ignored,
  ;;no flags are supported.
  ;;
  (let* ((opts 0)
	 (fd   (capi::platform-open-input-fd ((string->filename-func) filename) opts)))
    (if (fx< fd 0)
	(%raise-io-error who filename fd)
      fd)))

(define (%open-output-file-descriptor filename file-options who)
  ;;Subroutine for the functions below opening a  file for output.  Open and return a
  ;;file descriptor  referencing the  file selected  by the  string FILENAME.   If an
  ;;error occurs: raise an exception.
  ;;
  (let* ((opts (if (enum-set? file-options)
		   ($fxior (if (enum-set-member? 'no-create   file-options) #b0001 0)
			   (if (enum-set-member? 'no-fail     file-options) #b0010 0)
			   (if (enum-set-member? 'no-truncate file-options) #b0100 0)
			   (if (enum-set-member? 'executable  file-options) #b1000 0))
		 (assertion-violation who "file-options is not an enum set" file-options)))
	 (fd (capi::platform-open-output-fd ((string->filename-func) filename) opts)))
    (if (fx< fd 0)
	(%raise-io-error who filename fd)
      fd)))

(define (%open-input/output-file-descriptor filename file-options who)
  ;;Subroutine for the functions below opening a file for input and output.  Open and
  ;;return a  file descriptor referencing the  file selected by the  string FILENAME.
  ;;If an error occurs: raise an exception.
  ;;
  (let* ((opts (if (enum-set? file-options)
		   ;;In future  the options for I/O  ports may be different  from the
		   ;;ones of output-only ports.
		   ($fxior (if (enum-set-member? 'no-create   file-options) 1 0)
				  (if (enum-set-member? 'no-fail     file-options) 2 0)
				  (if (enum-set-member? 'no-truncate file-options) 4 0))
		 (assertion-violation who "file-options is not an enum set" file-options)))
	 (fd (capi::platform-open-input/output-fd ((string->filename-func) filename) opts)))
    (if (fx< fd 0)
	(%raise-io-error who filename fd)
      fd)))

(define (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				      maybe-transcoder close-function who)
  ;;Given the fixnum file descriptor FD  representing an open file for the underlying
  ;;platform: build and return a Scheme input port to be used to read the data.
  ;;
  ;;The returned port supports both the GET-POSITION and SET-POSITION!  operations.
  ;;
  ;;If CLOSE-FUNCTION is a  function: it is used as close function; if  it is true: a
  ;;standard close  function for  file descriptors  is used; else  the port  does not
  ;;support the close function.
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
    (let ((count (capi::platform-read-fd fd dst.bv dst.start requested-count)))
      (cond (($fx>= count 0)
	     count)
	    (($fx= count EAGAIN)
	     (%raise-eagain-error 'read! #f port-identifier))
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
  ;;Given the fixnum file descriptor FD  representing an open file for the underlying
  ;;platform: build and return a Scheme output port to be used to write the data.
  ;;
  ;;The returned port supports both the GET-POSITION and SET-POSITION!  operations.
  ;;
  ;;If CLOSE-FUNCTION is a  function: it is used as close function; if  it is true: a
  ;;standard close  function for  file descriptors  is used; else  the port  does not
  ;;support the close operation.
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
    (let ((count (capi::platform-write-fd fd src.bv src.start requested-count)))
      (cond (($fx>= count 0)
	     count)
	    (($fx= count EAGAIN)
	     (%raise-eagain-error 'write! #f port-identifier))
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
  ;;Given the fixnum file descriptor FD  representing an open file for the underlying
  ;;platform: build  and return  a Scheme input/output  port to be  used to  read and
  ;;write the data.
  ;;
  ;;The returned port supports both the GET-POSITION and SET-POSITION!  operations.
  ;;
  ;;If CLOSE-FUNCTION is a  function: it is used as close function; if  it is true: a
  ;;standard close  function for  file descriptors  is used; else  the port  does not
  ;;support the close operation.
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
    (let ((count (capi::platform-read-fd fd dst.bv dst.start requested-count)))
      (cond (($fx>= count 0)
	     count)
	    (($fx= count EAGAIN)
	     (%raise-eagain-error 'read! #f port-identifier))
	    (else
	     (%raise-io-error 'read! port-identifier count (make-i/o-read-error))))))

  (define (write! src.bv src.start requested-count)
    (let ((count (capi::platform-write-fd fd src.bv src.start requested-count)))
      (cond (($fx>= count 0)
	     count)
	    (($fx= count EAGAIN)
	     (%raise-eagain-error 'write! #f port-identifier))
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

;;; --------------------------------------------------------------------

(define (%socket->input-port sock other-attributes port-identifier buffer.size
			     transcoder close-function who)
  ;;Given the  fixnum socket descriptor  SOCK representing  a network socket  for the
  ;;underlying platform: build and return a Scheme  input port to be used to read the
  ;;data.
  ;;
  ;;The  returned   port  does  not   support  the  GET-POSITION   and  SET-POSITION!
  ;;operations.
  ;;
  ;;If CLOSE-FUNCTION is a  function: it is used as close function; if  it is true: a
  ;;standard close  function for  file descriptors  is used; else  the port  does not
  ;;support the close operation.
  ;;
  (define close
    (cond ((procedure? close-function)
	   close-function)
	  ((and (boolean? close-function) close-function)
	   (%make-close-function-for-platform-descriptor-port port-identifier sock))
	  (else #f)))

  (define (read! dst.bv dst.start requested-count)
    (let ((count (capi::platform-read-fd sock dst.bv dst.start requested-count)))
      (cond (($fx>= count 0)
	     count)
	    (($fx= count EAGAIN)
	     (%raise-eagain-error 'read! #f port-identifier))
	    (else
	     (%raise-io-error 'read! port-identifier count (make-i/o-read-error))))))

  (let ((attributes		(%select-input-fast-tag-from-transcoder
				 who transcoder other-attributes
				 GUARDED-PORT-TAG PORT-WITH-FD-DEVICE
				 (%select-eol-style-from-transcoder who transcoder)
				 DEFAULT-OTHER-ATTRS))
	(buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector buffer.size))
	(write!			#f)
	(get-position		#t)
	(cookie			(default-cookie sock)))
    (%port->maybe-guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer transcoder port-identifier
		 read! write! #f #f close cookie))))

(define (%socket->output-port sock other-attributes port-identifier buffer.size
			      transcoder close-function who)
  ;;Given the  fixnum socket descriptor  SOCK representing  a network socket  for the
  ;;underlying platform: build  and return a Scheme  output port to be  used to write
  ;;the data.
  ;;
  ;;The  returned   port  does  not   support  the  GET-POSITION   and  SET-POSITION!
  ;;operations.
  ;;
  ;;If CLOSE-FUNCTION is a  function: it is used as close function; if  it is true: a
  ;;standard close  function for  file descriptors  is used; else  the port  does not
  ;;support the close operation.
  ;;
  (define close
    (cond ((procedure? close-function)
	   close-function)
	  ((and (boolean? close-function) close-function)
	   (%make-close-function-for-platform-descriptor-port port-identifier sock))
	  (else #f)))

  (define (write! src.bv src.start requested-count)
    (let ((rv (capi::platform-write-fd sock src.bv src.start requested-count)))
      (cond (($fx>= rv 0)
	     rv)
	    (($fx= rv EAGAIN)
	     (%raise-eagain-error 'read! #f port-identifier))
	    (else
	     (%raise-io-error 'write! port-identifier rv (make-i/o-write-error))))))

  (let ((attributes		(%select-output-fast-tag-from-transcoder
				 who transcoder other-attributes
				 GUARDED-PORT-TAG PORT-WITH-FD-DEVICE
				 (%select-eol-style-from-transcoder who transcoder)
				 DEFAULT-OTHER-ATTRS))
	(buffer.index		0)
	(buffer.used-size	0)
	(buffer			(make-bytevector buffer.size))
	(read!			#f)
	(get-position		#t)
	(cookie			(default-cookie sock)))
    (%port->maybe-guarded-port
     ($make-port attributes buffer.index buffer.used-size buffer transcoder port-identifier
		 read! write! #f #f close cookie))))

(define (%socket->input/output-port sock other-attributes port-identifier buffer.size
				    transcoder close-function who)
  ;;Given the  fixnum socket descriptor  SOCK representing  a network socket  for the
  ;;underlying platform:  build and return a  Scheme input/output port to  be used to
  ;;read and write the data.
  ;;
  ;;The  returned   port  does  not   support  the  GET-POSITION   and  SET-POSITION!
  ;;operations.
  ;;
  ;;If CLOSE-FUNCTION is a  function: it is used as close function; if  it is true: a
  ;;standard close  function for  file descriptors  is used; else  the port  does not
  ;;support the close operation.
  ;;
  (define close
    (cond ((procedure? close-function)
	   close-function)
	  ((and (boolean? close-function) close-function)
	   (%make-close-function-for-platform-descriptor-port port-identifier sock))
	  (else #f)))

  (define (read! dst.bv dst.start requested-count)
    (let ((count (capi::platform-read-fd sock dst.bv dst.start requested-count)))
      (cond (($fx>= count 0)
	     count)
	    (($fx= count EAGAIN)
	     (%raise-eagain-error 'read! #f port-identifier))
	    (else
	     (%raise-io-error 'read! port-identifier count (make-i/o-read-error))))))

  (define (write! src.bv src.start requested-count)
    (let ((rv (capi::platform-write-fd sock src.bv src.start requested-count)))
      (cond (($fx>= rv 0)
	     rv)
	    (($fx= rv EAGAIN)
	     (%raise-eagain-error 'read! #f port-identifier))
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

;;; --------------------------------------------------------------------

(define (%make-set-position!-function-for-file-descriptor-port fd port-identifier)
  ;;Build and  return a  closure to  be used  as SET-POSITION!   function for  a port
  ;;wrapping the platform's file descriptor FD.
  ;;
  (lambda (position)
    (let ((errno (capi::platform-set-position fd position)))
      (when errno
	(%raise-io-error 'set-position! port-identifier errno
			 (make-i/o-invalid-position-error position))))))

(define (%make-close-function-for-platform-descriptor-port port-identifier fd)
  ;;Return a  standard CLOSE function for  a port wrapping the  platform's descriptor
  ;;FD.  It is used for both file  descriptors and socket descriptors, and in general
  ;;can be used for any platform descriptor.
  ;;
  (lambda ()
    (let ((errno (capi::platform-close-fd fd)))
      (when errno
	(%raise-io-error 'close port-identifier errno)))))

(define (%buffer-mode->attributes buffer-mode who)
  (case buffer-mode
    ((block)	0)
    ((line)	BUFFER-MODE-LINE-TAG)
    ((none)	BUFFER-MODE-NONE-TAG)
    (else
     (procedure-signature-argument-violation who
       "invalid buffer-mode argument"
       #f '(buffer-mode? buffer-mode) buffer-mode))))


;;;; input ports wrapping platform file descriptors

(case-define* open-file-input-port
  ;;Defined by  R6RS.  The OPEN-FILE-INPUT-PORT  procedure returns an input  port for
  ;;the named file.  The FILE-OPTIONS and MAYBE-TRANSCODER arguments are optional.
  ;;
  ;;The FILE-OPTIONS  argument, which may  determine various aspects of  the returned
  ;;port, defaults to the value of:
  ;;
  ;;   (file-options)
  ;;
  ;;MAYBE-TRANSCODER must be either a transcoder or false.
  ;;
  ;;The BUFFER-MODE  argument, if supplied,  must be one of  the symbols that  name a
  ;;buffer mode.  The BUFFER-MODE argument defaults to BLOCK.
  ;;
  ;;If MAYBE-TRANSCODER  is a transcoder,  it becomes the transcoder  associated with
  ;;the returned port.
  ;;
  ;;If MAYBE-TRANSCODER is false  or absent, the port will be a  binary port and will
  ;;support the PORT-POSITION and SET-PORT-POSITION!  operations.  Otherwise the port
  ;;will  be  a  textual  port,  and   whether  it  supports  the  PORT-POSITION  and
  ;;SET-PORT-POSITION!    operations  is   implementation  dependent   (and  possibly
  ;;transcoder dependent).
  ;;
  ((filename)
   (open-file-input-port filename (file-options) 'block #f))

  ((filename file-options)
   (open-file-input-port filename file-options   'block #f))

  ((filename file-options buffer-mode)
   (open-file-input-port filename file-options   buffer-mode #f))

  (({filename filename?} {file-options file-options?} buffer-mode {maybe-transcoder false-or-transcoder?})
   (let* ((buffer-mode-attrs	(%buffer-mode->attributes buffer-mode __who__))
	  (other-attributes	buffer-mode-attrs)
	  (fd			(%open-input-file-descriptor filename file-options __who__))
	  (port-identifier	filename)
	  (buffer-size		(input-file-buffer-size))
	  (close-function	#t))
     (%file-descriptor->input-port fd other-attributes port-identifier buffer-size
				   maybe-transcoder close-function __who__))))

(module (open-input-file
	 with-input-from-file
	 call-with-input-file)

  (define* (open-input-file {filename filename?})
    ;;Defined by R6RS.  Open FILENAME for input, with empty file options, and returns
    ;;the obtained port.
    ;;
    (%open-input-file-with-defaults filename __who__))

  (define* (with-input-from-file {filename filename?} {thunk procedure?})
    ;;Defined by R6RS.  THUNK must be a procedure and must accept zero arguments.
    ;;
    ;;The file is opened  for input or output using empty file  options, and THUNK is
    ;;called with no arguments.
    ;;
    ;;During the dynamic extent  of the call to THUNK, the obtained  port is made the
    ;;value returned by the procedure  CURRENT-INPUT-PORT; the previous default value
    ;;is reinstated when the dynamic extent is exited.
    ;;
    ;;When THUNK returns,  the port is closed automatically.  The  values returned by
    ;;THUNK are returned.
    ;;
    ;;If an  escape procedure is  used to  escape back into  the call to  THUNK after
    ;;THUNK is returned, the behavior is unspecified.
    ;;
    (call-with-port
	(%open-input-file-with-defaults filename __who__)
      (lambda (port)
	(parametrise ((current-input-port port))
	  (thunk)))))

  (define* (call-with-input-file {filename filename?} {proc procedure?})
    ;;Defined by R6RS.  PROC should accept one argument.
    ;;
    ;;This procedure  opens the file named  by FILENAME for input,  with no specified
    ;;file options, and calls PROC with the obtained port as an argument.
    ;;
    ;;If PROC  returns, the port is  closed automatically and the  values returned by
    ;;PROC are returned.
    ;;
    ;;If PROC  does not return,  the port is not  closed automatically, unless  it is
    ;;possible to prove that the port will never again be used for an I/O operation.
    ;;
    (call-with-port (%open-input-file-with-defaults filename __who__) proc))

  (define* (%open-input-file-with-defaults filename who)
    ;;Open FILENAME  for input,  with empty  file options,  and returns  the obtained
    ;;port.
    ;;
    (let ((fd			(%open-input-file-descriptor filename (file-options) who))
	  (other-attributes	0)
	  (port-id		filename)
	  (buffer.size		(input-file-buffer-size))
	  (transcoder		(native-transcoder))
	  (close-function		#t))
      (%file-descriptor->input-port fd other-attributes port-id buffer.size
				    transcoder close-function who)))

  #| end of module |# )


;;;; output ports wrapping platform file descriptors

(case-define* open-file-output-port
  ;;Defined by R6RS.  The OPEN-FILE-OUTPUT-PORT  procedure returns an output port for
  ;;the named file.
  ;;
  ;;The FILE-OPTIONS  argument, which may  determine various aspects of  the returned
  ;;port, defaults to the value of:
  ;;
  ;;   (file-options)
  ;;
  ;;MAYBE-TRANSCODER must be either a transcoder or false.
  ;;
  ;;The BUFFER-MODE  argument, if supplied,  must be one of  the symbols that  name a
  ;;buffer mode.  The BUFFER-MODE argument defaults to BLOCK.
  ;;
  ;;If MAYBE-TRANSCODER  is a transcoder,  it becomes the transcoder  associated with
  ;;the port.
  ;;
  ;;If MAYBE-TRANSCODER is false  or absent, the port will be a  binary port and will
  ;;support the PORT-POSITION and SET-PORT-POSITION!  operations.  Otherwise the port
  ;;will  be  a  textual  port,  and   whether  it  supports  the  PORT-POSITION  and
  ;;SET-PORT-POSITION!    operations   is  implementation-dependent   (and   possibly
  ;;transcoder-dependent).
  ;;
  ((filename)
   (open-file-output-port filename (file-options) 'block #f))

  ((filename file-options)
   (open-file-output-port filename file-options 'block #f))

  ((filename file-options buffer-mode)
   (open-file-output-port filename file-options buffer-mode #f))

  (({filename filename?} {file-options file-options?} buffer-mode {maybe-transcoder false-or-transcoder?})
   (let* ((buffer-mode-attrs	(%buffer-mode->attributes buffer-mode __who__))
	  (other-attributes	buffer-mode-attrs)
	  (fd			(%open-output-file-descriptor filename file-options __who__))
	  (port-identifier	filename)
	  (buffer-size		(output-file-buffer-size)))
     (%file-descriptor->output-port fd other-attributes port-identifier
				    buffer-size maybe-transcoder #t __who__))))

(module (open-output-file
	 with-output-to-file
	 call-with-output-file)

  (define* (open-output-file {filename filename?})
    ;;Defined by R6RS.  Open FILENAME for output, with empty file options, and return
    ;;the obtained port.
    ;;
    (%open-output-file-with-defaults filename __who__))

  (define* (with-output-to-file {filename filename?} {thunk procedure?})
    ;;Defined by  R6RS.  THUNK must  be a procedure  and must accept  zero arguments.
    ;;The file  is opened for  output using empty file  options, and THUNK  is called
    ;;with no arguments.
    ;;
    ;;During the dynamic extent  of the call to THUNK, the obtained  port is made the
    ;;value returned by the procedure CURRENT-OUTPUT-PORT; the previous default value
    ;;is reinstated when the dynamic extent is exited.
    ;;
    ;;When THUNK returns,  the port is closed automatically.  The  values returned by
    ;;THUNK are returned.
    ;;
    ;;If an  escape procedure is  used to  escape back into  the call to  THUNK after
    ;;THUNK is returned, the behavior is unspecified.
    ;;
    (call-with-port
	(%open-output-file-with-defaults filename __who__)
      (lambda (port)
	(parametrise ((current-output-port port))
	  (thunk)))))

  (define* (call-with-output-file {filename filename?} {proc procedure?})
    ;;Defined by R6RS.  PROC should accept one argument.
    ;;
    ;;This procedure opens  the file named by FILENAME for  output, with no specified
    ;;file options, and calls PROC with the obtained port as an argument.
    ;;
    ;;If PROC  returns, the port is  closed automatically and the  values returned by
    ;;PROC are returned.
    ;;
    ;;If PROC  does not return,  the port is not  closed automatically, unless  it is
    ;;possible to prove that the port will never again be used for an I/O operation.
    ;;
    (call-with-port (%open-output-file-with-defaults filename __who__) proc))

  (define (%open-output-file-with-defaults filename who)
    ;;Open FILENAME  for output,  with empty  file options,  and return  the obtained
    ;;port.
    ;;
    (let ((fd			(%open-output-file-descriptor filename (file-options) who))
	  (other-attributes	0)
	  (port-id		filename)
	  (buffer.size		(output-file-buffer-size))
	  (transcoder		(native-transcoder))
	  (close-function	#t))
      (%file-descriptor->output-port fd other-attributes port-id buffer.size
				     transcoder close-function who)))

  #| end of module |# )


;;;; input/output ports wrapping platform file descriptors

(case-define* open-file-input/output-port
  ;;Defined by R6RS.  Return  a single port that is both an input  port and an output
  ;;port for  the named  file.  The  optional arguments default  as described  in the
  ;;specification  of  OPEN-FILE-OUTPUT-PORT.   If  the  input/output  port  supports
  ;;PORT-POSITION and/or SET-PORT-POSITION!, the same  port position is used for both
  ;;input and output.
  ;;
  ((filename)
   (open-file-input/output-port filename (file-options) 'block #f))

  ((filename file-options)
   (open-file-input/output-port filename file-options 'block #f))

  ((filename file-options buffer-mode)
   (open-file-input/output-port filename file-options buffer-mode #f))

  (({filename filename?} {file-options file-options?} buffer-mode {maybe-transcoder false-or-transcoder?})
   (let* ((buffer-mode-attrs	(%buffer-mode->attributes buffer-mode __who__))
	  (other-attributes	($fxior INPUT/OUTPUT-PORT-TAG buffer-mode-attrs))
	  (fd			(%open-input/output-file-descriptor filename file-options __who__))
	  (port-identifier	filename)
	  (buffer-size		(input/output-file-buffer-size)))
     (%file-descriptor->input/output-port fd other-attributes port-identifier
					  buffer-size maybe-transcoder #t __who__))))


;;;; platform file descriptor ports

(define* (make-binary-file-descriptor-input-port {fd file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input-file-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				  transcoder close-function __who__)))

(define* (make-binary-file-descriptor-input-port* {fd file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input-file-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				  transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-textual-file-descriptor-input-port fd port-identifier transcoder)
  (let ((other-attributes	0)
	(buffer.size		(input-file-buffer-size))
	(close-function		#t))
    (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				  transcoder close-function __who__)))

(define* (make-textual-file-descriptor-input-port* fd port-identifier transcoder)
  (let ((other-attributes	0)
	(buffer.size		(input-file-buffer-size))
	(close-function		#f))
    (%file-descriptor->input-port fd other-attributes port-identifier buffer.size
				  transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-binary-file-descriptor-output-port {fd file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(output-file-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%file-descriptor->output-port fd other-attributes port-identifier buffer.size
				   transcoder close-function __who__)))

(define* (make-binary-file-descriptor-output-port* {fd file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(output-file-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%file-descriptor->output-port fd other-attributes port-identifier buffer.size
				   transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-textual-file-descriptor-output-port fd port-identifier transcoder)
  (let ((other-attributes	0)
	(buffer.size		(output-file-buffer-size))
	(close-function		#t))
    (%file-descriptor->output-port fd other-attributes port-identifier buffer.size
				   transcoder close-function __who__)))

(define* (make-textual-file-descriptor-output-port* fd port-identifier transcoder)
  (let ((other-attributes	0)
	(buffer.size		(output-file-buffer-size))
	(close-function		#f))
    (%file-descriptor->output-port fd other-attributes port-identifier buffer.size
				   transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-binary-file-descriptor-input/output-port {fd file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-file-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%file-descriptor->input/output-port fd other-attributes port-identifier buffer.size
					 transcoder close-function __who__)))

(define* (make-binary-file-descriptor-input/output-port* {fd file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-file-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%file-descriptor->input/output-port fd other-attributes port-identifier buffer.size
					 transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-textual-file-descriptor-input/output-port {fd file-descriptor?} {port-identifier port-identifier?} {transcoder transcoder?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-file-buffer-size))
	(close-function		#t))
    (%file-descriptor->input/output-port fd other-attributes port-identifier buffer.size
					 transcoder close-function __who__)))

(define* (make-textual-file-descriptor-input/output-port* {fd file-descriptor?} {port-identifier port-identifier?} {transcoder transcoder?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-file-buffer-size))
	(close-function		#f))
    (%file-descriptor->input/output-port fd other-attributes port-identifier buffer.size
					 transcoder close-function __who__)))


;;;; platform socket descriptor ports

(define* (make-binary-socket-input-port {sock file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%socket->input-port sock other-attributes port-identifier buffer.size
			 transcoder close-function __who__)))

(define* (make-binary-socket-input-port* {sock file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%socket->input-port sock other-attributes port-identifier buffer.size
			 transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-binary-socket-output-port {sock file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%socket->output-port sock other-attributes port-identifier buffer.size
			  transcoder close-function __who__)))

(define* (make-binary-socket-output-port* {sock file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%socket->output-port sock other-attributes port-identifier buffer.size
			  transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-binary-socket-input/output-port {sock file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(transcoder		#f)
	(close-function		#t))
    (%socket->input/output-port sock other-attributes port-identifier buffer.size
				transcoder close-function __who__)))

(define* (make-binary-socket-input/output-port* {sock file-descriptor?} {port-identifier port-identifier?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(transcoder		#f)
	(close-function		#f))
    (%socket->input/output-port sock other-attributes port-identifier buffer.size
				transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-textual-socket-input-port {sock file-descriptor?} {port-identifier port-identifier?} {transcoder transcoder?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(close-function		#t))
    (%socket->input-port sock other-attributes port-identifier buffer.size
			 transcoder close-function __who__)))

(define* (make-textual-socket-input-port* {sock file-descriptor?} {port-identifier port-identifier?} {transcoder transcoder?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(close-function		#f))
    (%socket->input-port sock other-attributes port-identifier buffer.size
			 transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-textual-socket-output-port {sock file-descriptor?} {port-identifier port-identifier?} {transcoder transcoder?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(close-function		#t))
    (%socket->output-port sock other-attributes port-identifier buffer.size
			  transcoder close-function __who__)))

(define* (make-textual-socket-output-port* {sock file-descriptor?} {port-identifier port-identifier?} {transcoder transcoder?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(close-function		#f))
    (%socket->output-port sock other-attributes port-identifier buffer.size
			  transcoder close-function __who__)))

;;; --------------------------------------------------------------------

(define* (make-textual-socket-input/output-port {sock file-descriptor?} {port-identifier port-identifier?} {transcoder transcoder?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(close-function		#t))
    (%socket->input/output-port sock other-attributes port-identifier buffer.size
				transcoder close-function __who__)))

(define* (make-textual-socket-input/output-port* {sock file-descriptor?} {port-identifier port-identifier?} {transcoder transcoder?})
  (let ((other-attributes	0)
	(buffer.size		(input/output-socket-buffer-size))
	(close-function		#f))
    (%socket->input/output-port sock other-attributes port-identifier buffer.size
				transcoder close-function __who__)))


;;;; standard, console and current ports

(define* (standard-input-port)
  ;;Defined by  R6RS.  Return a  new binary input  port connected to  standard input.
  ;;Whether the port supports the PORT-POSITION and SET-PORT-POSITION!  operations is
  ;;implementation dependent.
  ;;
  (let ((fd		0)
	(attributes	0)
	(port-id	"*stdin*")
	(buffer.size	(input-file-buffer-size))
	(transcoder	#f)
	(close		#f))
    (%file-descriptor->input-port fd attributes port-id buffer.size transcoder close __who__)))

(define* (standard-output-port)
  ;;Defined  by R6RS.   Return a  new binary  output port  connected to  the standard
  ;;output.   Whether  the port  supports  the  PORT-POSITION and  SET-PORT-POSITION!
  ;;operations is implementation dependent.
  ;;
  (%file-descriptor->output-port 1 0
				 "*stdout*" (output-file-buffer-size) #f #f __who__))

(define* (standard-error-port)
  ;;Defined  by R6RS.   Return a  new binary  output port  connected to  the standard
  ;;error.   Whether  the  port  supports the  PORT-POSITION  and  SET-PORT-POSITION!
  ;;operations is implementation dependent.
  ;;
  (%file-descriptor->output-port 2 0
				 "*stderr*" (output-file-buffer-size) #f #f __who__))

;;; --------------------------------------------------------------------

(define current-input-port
  ;;Defined  by R6RS.   Return  a default  textual port  for  input.  Normally,  this
  ;;default port is associated with standard input, but can be dynamically reassigned
  ;;using the WITH-INPUT-FROM-FILE  procedure from the (rnrs io  simple (6)) library.
  ;;The port may or may not have an associated transcoder; if it does, the transcoder
  ;;is implementation dependent.
  ;;
  (make-parameter
      #f
    (lambda (obj)
      (if (textual-input-port? obj)
	  obj
	(procedure-argument-violation 'current-input-port
	  "expected textual input port as parameter value" obj)))))

(define current-output-port
  ;;Defined by  R6RS.  Hold the default  textual port for regular  output.  Normally,
  ;;this default port is associated with standard output.
  ;;
  ;;The return value  of CURRENT-OUTPUT-PORT can be dynamically  reassigned using the
  ;;WITH-OUTPUT-TO-FILE  procedure from  the (rnrs  io simple  (6)) library.   A port
  ;;returned by this  procedure may or may  not have an associated  transcoder; if it
  ;;does, the transcoder is implementation dependent.
  ;;
  (make-parameter
      #f
    (lambda (obj)
      (if (textual-output-port? obj)
	  obj
	(procedure-argument-violation 'current-output-port
	  "expected textual input port as parameter value" obj)))))

(define current-error-port
  ;;Defined by R6RS.  Hold the default textual port for error output.  Normally, this
  ;;default port is associated with standard error.
  ;;
  ;;The return  value of CURRENT-ERROR-PORT  can be dynamically reassigned  using the
  ;;WITH-OUTPUT-TO-FILE  procedure from  the (rnrs  io simple  (6)) library.   A port
  ;;returned by this  procedure may or may  not have an associated  transcoder; if it
  ;;does, the transcoder is implementation dependent.
  ;;
  (make-parameter
      #f
    (lambda (obj)
      (if (textual-output-port? obj)
	  obj
	(procedure-argument-violation 'current-error-port
	  "expected textual input port as parameter value" obj)))))

;;; --------------------------------------------------------------------

(define %console-input-port	#f)
(define %console-output-port	#f)
(define %console-error-port	#f)

(case-define* console-input-port
  ;;Defined by Ikarus.   Return the default textual error port:  the default value of
  ;;the parameter CURRENT-INPUT-PORT; each call  returns the same port.  When applied
  ;;to an argument:  the argument must be  a textual output port and  it replaces the
  ;;old value; the old port is left untouched (it is not closed).
  ;;
  (()
   %console-input-port)
  (({P textual-input-port?})
   (set! %console-input-port P)))

(case-define* console-output-port
  ;;Defined by Ikarus.  Return the default  textual output port: the default value of
  ;;the parameter CURRENT-OUTPUT-PORT; each call returns the same port.  When applied
  ;;to an argument:  the argument must be  a textual output port and  it replaces the
  ;;old value; the old port is left untouched (it is not closed).
  ;;
  (()
   %console-output-port)
  (({P textual-output-port?})
   (set! %console-output-port P)))

(case-define* console-error-port
  ;;Defined by Ikarus.   Return the default textual error port:  the default value of
  ;;the parameter CURRENT-ERROR-PORT; each call  returns the same port.  When applied
  ;;to an argument:  the argument must be  a textual output port and  it replaces the
  ;;old value; the old port is left untouched (it is not closed).
  ;;
  (()
   %console-error-port)
  (({P textual-output-port?})
   (set! %console-error-port P)))

;;; --------------------------------------------------------------------

(define (initialise-io-ports)
  ;;This function is needed to allow easy rotation of boot images.
  ;;
  (current-input-port	(transcoded-port (standard-input-port)  (native-transcoder)))
  (current-output-port	(transcoded-port (standard-output-port) (native-transcoder)))
  (current-error-port	(transcoded-port (standard-error-port)  (native-transcoder)))
  (set-port-buffer-mode! (current-error-port) (buffer-mode line))
  (set! %console-input-port	(current-input-port))
  (set! %console-output-port	(current-output-port))
  (set! %console-error-port	(current-error-port)))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
