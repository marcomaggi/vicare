;;;!vicare
;;;
;;;Part of: Vicare Scheme
;;;Contents: toy HTTP server
;;;Date: Tue Mar 19, 2013
;;;
;;;Abstract
;;;
;;;	This  program  implements  a  toy HTTP  server  as  testbed  for
;;;	libraries, especially the Simple Event Loop.
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (except (vicare)
		log)
  (prefix (vicare posix)
	  px.)
  (prefix (vicare simple-event-loop)
	  sel.)
  (vicare platform constants)
  (vicare syntactic-extensions))


;;;; global variables

;;True if this  process is the root  server process; false if  this is a
;;children process  resulting from a  call to PX.FORK.  The  root server
;;process has cleaning duties.
;;
(define root-server?
  (make-parameter #t))

;;An instance of  record type "<options>" holding  global server options
;;configured from the command line.
;;
(define options
  (make-parameter #f))

;;; --------------------------------------------------------------------

(define-constant VERSION-NUMBER
  "0.1d0")

;;The exit status in case of "bad configuration option value".  It is to
;;be handed to EXIT.
;;
(define-constant BAD-OPTION-EXIT-STATUS 2)


;;;; main function

(define (main argv)
  (import PID-FILE LOGGING DAEMONISATION SERVER-EVENTS-LOOP)
  (parametrise
      ((logging	#t)
       (sel.logging	log)
       (options	(make-<options> argv)))
    (when (options.daemonise?)
      (daemonise))
    (open-log-file options.log-file)
    (unwind-protect
	(begin
	  (log "starting HTTP server, pid=~a" (px.getpid))
	  (log "document root: ~a" (options.document-root))
	  (log "listening to: ~a:~a" (options.server-interface) (options.server-port))
	  (create-pid-file options.pid-file log)
	  (unwind-protect
	      (begin
		(sel.initialise)
		(unwind-protect
		    (server-loop (options.server-interface) (options.server-port))
		  (sel.finalise)
		  (log "exiting HTTP server")))
	    (when (root-server?)
	      (remove-pid-file))))
      (when (root-server?)
	(close-log-file)))
    (exit 0)))


;;;; type definitions

;;Hold global server options configured from the command line.
;;
(define-record-type <options>
  (fields (mutable server-interface)
		;A string representing the  server interface to bind to.
		;Defaults to "localhost".
	  (mutable server-port)
		;An exact integer representing the server port to listen
		;to.  Defaults to 8080.
	  (mutable document-root)
		;A string representing the absolute pathname of the root
		;directory for  documents to  serve.  If  not explicitly
		;configured: an error occurs.
	  (mutable pid-file)
		;False or a string representing  the pathname of the PID
		;file.
	  (mutable log-file)
		;False or a string representing  the pathname of the log
		;file.
	  (mutable daemonise?)
		;Boolean, true if the server must be daemonised.
	  (mutable verbosity)
		;An exact integer.  When zero: run the program silently;
		;this is the default.  When  a positive integer: run the
		;program  with  verbose   messages  at  the  appropriate
		;verbosity level.
	  )
  (protocol
   (lambda (maker)
     (lambda (argv)
       (import COMMAND-LINE-ARGS)
       (define (%err template . args)
	 (apply error-message-and-exit BAD-OPTION-EXIT-STATUS template args))
       (let ((self (maker "localhost" 8080 #f #;document-root
			  #f #;pid-file #f #;log-file #f #;daemonise?
			  0 #;verbosity )))
	 (parse-command-line-arguments self argv)

	 ;; validate document root
	 (let ((dirname ($<options>-document-root self)))
	   (cond ((not dirname)
		  (%err "missing selection of document root"))
		 ((not (string? dirname))
		  (%err "internal error selecting document root pathname: ~a" dirname))
		 ((zero? (string-length dirname))
		  (%err "selected empty document root pathname"))
		 (else
		  (let ((dirname (absolutise-pathname dirname)))
		    (if (and (px.file-is-directory? dirname)
			     (px.file-readable? dirname))
			(<options>-document-root-set! self dirname)
		      (%err "selected document root unexistent or not readable: ~a"
			    dirname))))))

	 ;; validate server interface, more validation later
	 (let ((interface ($<options>-server-interface self)))
	   (unless (and (string? interface)
			(not (zero? (string-length interface))))
	     (%err "invalid server interface: \"~a\"" interface)))

	 ;; validate server port
	 (let ((port ($<options>-server-port self)))
	   (import NETWORKING)
	   (cond ((not (network-port? port))
		  (%err "invalid server port: \"~a\"" port))))

	 ;; validate pid file
	 (let ((filename ($<options>-pid-file self)))
	   (cond ((not filename)
		  (void))
		 ((not (string? filename))
		  (%err "internal error selecting PID file pathname: ~a" filename))
		 ((zero? (string-length filename))
		  (%err "selected empty PID file pathname"))
		 (else
		  (let ((filename (absolutise-pathname filename)))
		    (if (file-exists? filename)
			(%err "selected PID file pathname already exists: ~a" filename)
		      (<options>-pid-file-set! self filename))))))

	 ;; validate log file
	 (let ((filename ($<options>-log-file self)))
	   (cond ((not filename)
		  (void))
		 ((not (string? filename))
		  (%err "internal error selecting log file pathname: ~a" filename))
		 ((string=? "-" filename)
		  ;;Log to the current error port.
		  (void))
		 ((zero? (string-length filename))
		  (%err "selected empty log file pathname"))
		 (else
		  (let ((filename (absolutise-pathname filename)))
		    (when (and (file-exists? filename)
			       (not (and (px.file-is-regular-file? filename)
					 (px.file-writable? filename))))
		      (%err "selected log file pathname not writable" filename))
		    (<options>-log-file-set! self filename)))))

	 self)))))

;;; --------------------------------------------------------------------

(define (<options>-increment-verbosity! opts)
  (<options>-verbosity-set! opts (+ +1 (<options>-verbosity opts))))

(define (<options>-decrement-verbosity! opts)
  (<options>-verbosity-set! opts (+ -1 (<options>-verbosity opts))))

;;; --------------------------------------------------------------------

(define (options.document-root)
  ($<options>-document-root (options)))

(define (options.server-interface)
  ($<options>-server-interface (options)))

(define (options.server-port)
  ($<options>-server-port (options)))

(define (options.pid-file)
  ($<options>-pid-file (options)))

(define (options.log-file)
  ($<options>-log-file (options)))

(define (options.verbosity)
  ($<options>-verbosity (options)))

(define (options.daemonise?)
  ($<options>-daemonise? (options)))


;;;; log file handling

(module LOGGING
  (logging
   log-port
   open-log-file
   close-log-file
   log
   log-condition-message)

  ;;False or a textual output port to which log messages must be written.
  ;;
  (define log-port
    (make-parameter #f))

  (define pathname-thunk
    ;;It must be  set to a thunk returning a  Scheme string representing
    ;;the log  file pathname, or false  if no PID file  must be created.
    ;;The special string "-" means: log to the current error port.
    ;;
    (make-parameter #f
      (lambda (obj)
	(assert (procedure? obj))
	obj)))

;;; --------------------------------------------------------------------

  (define (open-log-file ptn-thunk)
    ;;If logging  is enabled: configure  the log port.  If  the selected
    ;;pathname is  "-" assume the  log messages  must go to  the current
    ;;error port.  Otherwise open a log file.
    ;;
    (when (logging)
      (pathname-thunk ptn-thunk)
      (let ((log-file ((pathname-thunk))))
	(if (string=? "-" log-file)
	    (log-port (current-error-port))
	  (let ((size (if (file-exists? log-file)
			  (px.file-size log-file)
			0)))
	    (when log-file
	      (log-port (open-file-output-port log-file
					       (file-options no-fail no-truncate)
					       (buffer-mode line)
					       (native-transcoder)))
	      (set-port-position! (log-port) size))))))
    (void))

  (define (close-log-file)
    ;;Close the  log port unless it  is the current error  port.  Notice
    ;;that the LOGGING parameter is ignored.
    ;;
    (when (and (log-port)
	       (not (equal? (log-port)
			    (current-error-port))))
      (close-port (log-port)))
    (void))

;;; --------------------------------------------------------------------

  ;;Boolean; true if logging is enabled, false otherwise.
  ;;
  (define logging
    (make-parameter #f
      (lambda (obj)
	(if obj #t #f))))

  (define (log template . args)
    ;;If  logging is  enabled: format  a log  line and  write it  to the
    ;;current log port.  Return unspecified values.
    ;;
    (when (and (logging)
	       (log-port))
      (let* ((date	(px.strftime/string "%F-T%T%Z" (px.localtime (px.time))))
	     (template	(string-append (format "~a: " date) template)))
	(%format-and-print (log-port) template args)))
    (void))

  (define (log-condition-message template cnd)
    (log template (if (message-condition? E)
		      (condition-message E)
		    "unknown error")))

;;; --------------------------------------------------------------------

  (define (%format-and-print port template args)
    ;;Format a  line of text and  display it to the  given textual port.
    ;;We expect the port to have buffer mode set to "line".
    ;;
    (fprintf port "vicare httpd: ")
    (apply fprintf port template args)
    (newline port))

  #| end of module: LOGGING |# )


;;;; PID file handling

(module PID-FILE
  (create-pid-file
   remove-pid-file)

  (define pathname-thunk
    ;;It must be  set to a thunk returning a  Scheme string representing
    ;;the PID file pathname, or false if no PID file must be created.
    ;;
    (make-parameter #f
      (lambda (obj)
	(assert (procedure? obj))
	obj)))

  (define log-proc
    ;;It must be  set to a function accepting  FORMAT-like arguments and
    ;;logging the result.
    ;;
    (make-parameter #f
      (lambda (obj)
	(assert (procedure? obj))
	obj)))

  (define (log template . args)
    (apply (log-proc) template args))

;;; --------------------------------------------------------------------

  (define (create-pid-file ptn-thunk log-func)
    ;;Create the  PID file and  write the PID  number in it,  followed a
    ;;newline.  Fail if the file already exists.
    ;;
    (pathname-thunk ptn-thunk)
    (log-proc log-func)
    (let ((pid-file ((pathname-thunk))))
      (when pid-file
	(if (file-exists? pid-file)
	    (log "selected PID file pathname already exists: ~a" pid-file)
	  (begin
	    (log "creating PID file: ~a" ((pathname-thunk)))
	    (with-output-to-file pid-file
	      (lambda ()
		(display (px.getpid))
		(newline))))))))

  (define (remove-pid-file)
    ;;Remove  the PID  file.  Fail  if  the selected  pathname does  not
    ;;contain this process' PID followed by  a newline.  If the PID file
    ;;does not exists: do nothing.
    ;;
    (define pid-file ((pathname-thunk)))
    (when (and pid-file (file-exists? pid-file))
      (log "removing PID file")
      (guard (E (else
		 (log-condition-message "error removing pid file: ~a" E)))
	(with-input-from-file pid-file
	  (lambda ()
	    (unless (string=? (string-append (number->string (px.getpid)) "\n")
			      ;;A valid PID file  does not contain a lot
			      ;;of characters; let's say 16 at most.
			      (get-string-n (current-input-port) 16))
	      (error #f "corrupted PID file contents, avoiding removal"))))
	(delete-file pid-file))))

  #| end of module: PID-FILE |# )


;;;; process daemonisation

(module DAEMONISATION
  (daemonise)

  (define (daemonise)
    ;;Daemonise the current process.
    ;;
    (px.signal-bub-init)
    (unwind-protect
	(begin
	  (exit-parent-keep-children)
	  (replace-standard-ports)
	  (detach-from-terminal-and-become-session-leader))
      (px.signal-bub-final)))

  (define (exit-parent-keep-children)
    (let ((pid (px.fork)))
      (unless (zero? pid)
	;;We are in the parent.
	(exit 0))
      ;;We are in the children.
      (void)))

  (define (replace-standard-ports)
    (let ((port (open-file-input/output-port "/dev/null"
					     (file-options no-create
							   no-fail
							   no-truncate)
					     (buffer-mode none)
					     (native-transcoder))))
      (close-port (current-input-port))
      (current-input-port port)
      (close-port (current-output-port))
      (current-output-port port)
      (close-port (current-error-port))
      (current-error-port port)))

  (define (detach-from-terminal-and-become-session-leader)
    (px.setsid))

  #| end of module: DAMONISATION |# )


;;;; command line arguments parsing

(module COMMAND-LINE-ARGS
  (parse-command-line-arguments)
  (import (srfi :37))

  (define (parse-command-line-arguments seed argv)
    (args-fold (cdr argv) program-options
	       unrecognised-option-proc
	       argument-processor
	       seed))

;;; --------------------------------------------------------------------

  (define-constant HELP-SCREEN
    "Usage: httpd.sps [vicare options] -- [options] --document-root=DIRNAME
Options:
   --document-root=/path/to/html
\tSelect the root directory of documents to serve.
   -I IFACE, --interface IFACE
\tSelect the server interface to bind to.
   -P PORT, --port PORT
\tSelect the server port to listen to (1...65535)
   --pid-file /path/to/pid-file
\tSelect the pathname for the PID file.
   --log-file /path/to/log-file
\tSelect the pathname for the log file.  Use \"-\" to log
\ton the error port.
   --daemon
\tTurn the server process into a daemon.
   -V, --version
\tPrint version informations and exit.
   --version-only
\tPrint version number only and exit.
   -v, --verbose
\tPrint verbose messages.
   -h, --help
\tPrint this help screen and exit.\n")

  (define-constant VERSION-SCREEN
    "Vicare HTTPD ~a\n\
     Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>\n\
     This is free software; see the source for copying conditions.  There is NO\n\
     warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n")

;;; --------------------------------------------------------------------

  (define (interface-option-processor option name operand seed)
    ;;Select the interface to bind to.  We will validate this later.
    ;;
    (<options>-server-interface-set! seed operand)
    seed)

  (define (port-option-processor option name operand seed)
    (let ((port (string->number operand)))
      (unless port
	(invalid-option-value name operand))
      (<options>-server-port-set! seed port))
    seed)

  (define (document-root-option-processor option name operand seed)
    (<options>-document-root-set! seed operand)
    seed)

  (define (pid-file-option-processor option name operand seed)
    (<options>-pid-file-set! seed operand)
    seed)

  (define (log-file-option-processor option name operand seed)
    (<options>-log-file-set! seed operand)
    seed)

  (define (daemon-option-processor option name operand seed)
    (<options>-daemonise?-set! seed #t)
    seed)

;;; --------------------------------------------------------------------
;;; auxiliary options

  (define (verbosity-option-processor option name operand seed)
    (<options>-increment-verbosity! seed)
    seed)

  (define (help-option-processor option name operand seed)
    (fprintf (current-error-port) HELP-SCREEN)
    (exit 0))

  (define (version-option-processor option name operand seed)
    (fprintf (current-error-port) VERSION-SCREEN VERSION-NUMBER)
    (exit 0))

  (define (version-only-option-processor option name operand seed)
    (fprintf (current-error-port) "~a\n" VERSION-NUMBER)
    (exit 0))

;;; --------------------------------------------------------------------
;;; options definition

  (define program-options
    ;;List of options recognised by this program.
    ;;
    (list
     (option '(#\I "interface")	#t #f interface-option-processor)
     (option '(#\P "port")	#t #f port-option-processor)
     (option '("document-root")	#t #f document-root-option-processor)
     (option '("pid-file")	#t #f pid-file-option-processor)
     (option '("log-file")	#t #f log-file-option-processor)
     (option '("daemon")	#f #f daemon-option-processor)

     (option '("version-only")	#f #f version-only-option-processor)
     (option '(#\V "version")	#f #f version-option-processor)
     (option '(#\v "verbose")	#f #f verbosity-option-processor)
     (option '(#\h "help")	#f #f help-option-processor)
     ))

;;; --------------------------------------------------------------------
;;; helper functions

  (define (argument-processor operand seed)
    (%err "invalid command line argument: ~a" operand))

  (define (invalid-option-value option value)
    (%err "invalid value for option \"~a\": ~a" option value))

  (define (unrecognised-option-proc option name arg seed)
    (%err "unknown command line option: ~a" name))

  (define (%err template . args)
    (apply error-message-and-exit BAD-OPTION-EXIT-STATUS template args))

  #| end of module: COMMAND-LINE-ARGS |#)


;;;; networking

(module SERVER-EVENTS-LOOP
  (server-loop)
  (import NETWORKING HTTP-SERVER INTERPROCESS-SIGNALS LOGGING)

  (define (server-loop interface port)
    ;;Given  a  string INTERFACE  representing  a  network interface  to
    ;;listen to and network PORT number: create the master server socket
    ;;bound to  the given  interface and  enter the  SEL loop  to accept
    ;;incoming connections.  Return unspecified values.
    ;;
    (define master-sock
      (make-master-sock interface port))
    (define (schedule-incoming-connection-event)
      (sel.readable master-sock (lambda ()
				  (schedule-incoming-connection-event)
				  (accept-connection master-sock))))
    (unwind-protect
	(begin
	  (initialise-signal-handlers)
	  (sel.readable master-sock schedule-incoming-connection-event)
	  (sel.enter))
      (close-master-sock master-sock))
    (void))

  (define (accept-connection master-sock)
    ;;Given  the  socket  descriptor MASTER-SOCK  representing  a  bound
    ;;socket with a pending connection:  accept the connection, create a
    ;;server  socket, enter  the  coroutine that  will  handle the  HTTP
    ;;requests.
    ;;
    ;;This function  returns unspecified  values whenever  the coroutine
    ;;finishes.
    ;;
    (receive (server-sock server-port)
	(make-server-sock master-sock)
      (define (suspend-until-readable)
	;;The server  coroutine can  call this  thunk multiple  times to
	;;suspend  itself until  the SEL  detects data  incoming on  the
	;;server socket.
	;;
	(call/cc (lambda (resume)
		   (sel.readable server-sock resume))))
      (unwind-protect
	  (coroutine
	      (lambda ()
		(http-server-coroutine server-port suspend-until-readable)))
	(close-server-port server-port)))
    (void))

  #| end of module: NETWORKING |# )


;;;; sockets handling

(module NETWORKING
  (network-port?
   make-master-sock	make-server-sock
   close-master-sock	close-server-port)
  (import LOGGING)

  (define (make-master-sock interface port)
    ;;Given  a  string INTERFACE  representing  a  network interface  to
    ;;listen to and network PORT number: open a master server socket and
    ;;bind  it to  the interface  and  port.  Return  the master  socket
    ;;descriptor.
    ;;
    (let ((sockaddr    (%make-sockaddr interface (number->string port)))
	  (master-sock (px.socket PF_INET SOCK_STREAM 0)))
      (socket-set-non-blocking master-sock)
      (px.setsockopt/linger master-sock #t 3)
      (px.bind   master-sock sockaddr)
      (px.listen master-sock 10)
      master-sock))

  (define (make-server-sock master-sock)
    ;;Given  the  socket  descriptor MASTER-SOCK  representing  a  bound
    ;;socket  with  a  pending  connection: accept  the  connection  and
    ;;configure the resulting server socket descriptor.
    ;;
    ;;Return  2   values:  the   server  socket  descriptor,   a  Scheme
    ;;input/output  binary port  wrapping the  descriptor.  Closing  the
    ;;Scheme port will also close the server socket.
    ;;
    (receive (server-sock client-address)
	(px.accept master-sock)
      (let ((id (string-append
		 (px.inet-ntop AF_INET (px.sockaddr.in_addr client-address))
		 ":"
		 (number->string (px.sockaddr.in_port client-address)))))
	(log "accepted connection from: ~a\n" id)
	(socket-set-non-blocking server-sock)
	(let ((server-port (make-binary-socket-input/output-port server-sock id)))
	  (values server-sock server-port)))))

;;; --------------------------------------------------------------------

  (define (close-master-sock sock)
    ;;Close the master socket descriptor, shutting down listening to the
    ;;bound interface.  SOCK must be the return value of a previous call
    ;;to MAKE-MASTER-SOCK.
    ;;
    (px.close sock))

  (define (close-server-port port)
    ;;Close  the  Scheme  port  wrapping a  server  connection's  socket
    ;;descriptor;  closing the  port will  alsot shut  down the  socket.
    ;;PORT   must  be   the  return   value  of   a  previous   call  to
    ;;MAKE-SERVER-SOCK.
    ;;
    (close-port port))

;;; --------------------------------------------------------------------

  (define (%make-sockaddr interface port)
    ;;Given  a  string INTERFACE  representing  a  network interface  to
    ;;listen to and  network PORT number: query the  system for sockaddr
    ;;structures  representing such  interface and  port and  supporting
    ;;IPv4.
    ;;
    ;;Return  a Scheme  bytevector  holding the  first matching  "struct
    ;;sockaddr"; if no address can be determined: raise an exception.
    ;;
    (let* ((hints (px.make-struct-addrinfo 0 AF_INET SOCK_STREAM 0 0 #f #f))
	   (infos (px.getaddrinfo interface port hints)))
      (%pretty-print infos)
      (if (null? infos)
	  (error 'make-sockaddr "unable to acquire master socket address")
	(px.struct-addrinfo-ai_addr (car infos)))))

;;; --------------------------------------------------------------------

  (define (socket-set-non-blocking sock)
    ;;Configure the given socket descriptor for non-blocking mode.
    ;;
    (let ((x (px.fcntl sock F_GETFL 0)))
      (px.fcntl sock F_SETFL (bitwise-ior x O_NONBLOCK))))

  (define (network-port? obj)
    ;;Return true  if OBJ is  an exact integer  in the range  of network
    ;;ports.
    ;;
    (and (fixnum? obj)
	 (<= 1 obj 65535)))

  #| end of module: SOCKETS |# )


;;;; HTTP server

(module HTTP-SERVER
  (http-server-session)
  (import (vicare coroutines)
    (vicare net channels)
    LOGGING)

  (define (http-server-session port suspend-until-readable)
    ;;
    ;;
    (define (main port suspend-until-readable)
      (receive (error? http-version-1.1? requested-document)
	  (%parse-first-request (%recv-request))
	(if error?
	    (begin
	      (log "invalid HTTP request")
	      (%close-connection))
	  (begin
	    (when http-version-1.1?
	      (%send-message chan OPEN-CONTINUE-RESPONSE))
	    (%send-document-response requested-document)
	    (if http-version-1.1?
		(let next-request ()
		  (receive (error? close-http-1.1-continue? requested-document)
		      (%parse-request (%recv-request))
		    (cond (error?
			   (log "invalid HTTP request")
			   (%close-connection))
			  (close-http-1.1-continue?
			   (%close-connection))
			  (else
			   (%send-document-response requested-document)
			   (next-request)))))
	      (%close-connection)))))
      (void))

    (define chan
      (chan.open-input/output-channel port))

    (define (%send-document-response requested-document)
      (let ((pathname (%requested-document->local-pathname requested-document)))
	(receive (contents contents.type)
	    (%local-pathname->contents pathname)
	  (%send-message chan (if contents
				  (begin
				    (log "serving document: ~a" pathname)
				    (%prepare-document-response contents contents.type))
				(begin
				  (log "error serving document: ~a" pathname)
				  (%prepare-error-response)))))))

    (define (%recv-request)
      (%recv-message chan suspend-until-readable))

    (define (%close-connection)
      (chan.close-channel chan))

    (guard (E (else
	       (log-condition-message "server error: ~a" E)
	       (%send-message chan SERVER-ERROR-RESPONSE)
	       (%close-connection)))
      (main port suspend-until-readable)))

;;; --------------------------------------------------------------------

  (define (%parse-first-request request)
    ;;Given the bytevector REQUEST holding  the opening request from the
    ;;client,  parse it  and return  3 values:
    ;;
    ;;1. A boolean, true if the request is invalid.
    ;;
    ;;2. A boolean, true if the HTTP protocol version is 1.1.
    ;;
    ;;3. A  bytevector representing the requested  document pathname, or
    ;;   false if no pathname is available.
    ;;
    (values #f #f '#vu8()))

  (define (%parse-request request)
    ;;Given  the  bytevector REQUEST  holding  the  a request  from  the
    ;;client, parse it and return 3 values:
    ;;
    ;;1. A boolean, true if the request is invalid.
    ;;
    ;;2.   A  boolean, true  if  the  request  closes an  "HTTP/1.1  100
    ;;   Continue" connection.
    ;;
    ;;3. A  bytevector representing the requested  document pathname, or
    ;;   false if no pathname is available.
    ;;
    (values #f #t '#vu8()))

  (define (%prepare-document-response contents contents.type)
    (let ((contents.len (bytevector-length contents)))
      (bytevector-append
       SUCCESS-DOCUMENT-RESPONSE
       '#ve(ascii "Content-Type: ") contents.type END-OF-LINE
       '#ve(ascii "Content-Length: ") (number->string contents.len) END-OF-LINE
       END-OF-LINE
       contents
       MESSAGE-TERMINATOR)))

  (define (%prepare-error-response)
    DOCUMENT-ERROR-RESPONSE)

;;; --------------------------------------------------------------------

  (define (%requested-document->local-pathname doc.bv)
    ;;Given  a bytevector  representing  the  requested local  document:
    ;;return  a  Scheme  string   representing  the  correspoding  local
    ;;pathname.  This function does not  check for the existence of such
    ;;pathname.
    ;;
    (let ((doc.str (ascii->string doc.bv)))
      (case-strings doc.str
	(("/")
	 (string-append (options.document-root) "index.html"))
	(else
	 (string-append (options.document-root) doc.str)))))

  (define (%local-pathname->contents pathname)
    ;;Given  a  Scheme  string  representing the  local  pathname  of  a
    ;;requested document:  open the file,  load its contents,  close the
    ;;file,  return 2  values: the  contents in  a single  bytevector, a
    ;;bytevector representing the contents type.
    ;;
    ;;If an error occurs: log a message and return false and false.
    ;;
    (guard (E ((i/o-file-does-not-exist-error? E)
	       (log-condition-message "requested unexistent document: ~a, ~a"
				      cnd pathname)
	       (values #f #f))
	      (else
	       (log-condition-message "error loading document: ~a, ~a"
				      cnd pathname)
	       (values #f #f)))
      (values (%read-file pathname)
	      (%file-type pathname))))

  (define (%read-file pathname)
    (let ((P (open-file-input-port pathname
	       (file-options no-create no-truncate)
	       (buffer-mode block))))
      (unwind-protect
	  (get-bytevector-all P)
	(close-port P))))

  (define (%file-type pathname)
    '#ve(ascii "text/html"))

;;; --------------------------------------------------------------------

  (define (%send-message chan data)
    (chan.channel-send-begin! chan)
    (chan.channel-send-message-portion! chan data)
    (chan.channel-send-end! chan))

  (define (%recv-message chan suspend-until-readable)
    (chan.channel-recv-begin! chan)
    (let wait-for-message-terminator ()
      (suspend-until-readable)
      (unless (chan.channel-recv-message-portion! chan data)
	(wait-for-message-terminator)))
    (chan.channel-send-end! chan))

;;; --------------------------------------------------------------------

  (define-constant OPEN-CONTINUE-RESPONSE
    '#ve(ascii "HTTP/1.1 100 Continue\r\n\r\n"))

  (define-constant SUCCESS-DOCUMENT-RESPONSE
    '#ve(ascii "HTTP/1.1 200 OK\r\n\r\n"))

  (define-constant DOCUMENT-ERROR-RESPONSE
    '#ve(ascii "HTTP/1.1 404 Not Found\r\n\r\n"))

  (define-constant SERVER-ERROR-RESPONSE
    '#ve(ascii "HTTP/1.1 500 Server Error\r\n\r\n"))

  (define-constant MESSAGE-TERMINATOR
    '#ve(ascii "\r\n\r\n"))

  (define-constant END-OF-LINE
    '#ve(ascii "\r\n"))

  #| end of module: HTTP-SERVER |# )


;;;; interprocess signal handlers

(module INTERPROCESS-SIGNALS
  (initialise-signal-handlers)

  (define (initialise-signal-handlers)
    (sel.receive-signal SIGTERM %sigterm-handler)
    (sel.receive-signal SIGQUIT %sigquit-handler)
    (sel.receive-signal SIGINT  %sigint-handler)
    (sel.receive-signal SIGTSTP %sigtstp-handler)
    (sel.receive-signal SIGCONT %sigcont-handler))

  (define (%sigterm-handler)
    (import LOGGING)
    (sel.receive-signal SIGTERM %sigterm-handler)
    (log "received SIGTERM")
    (sel.leave-asap))

  (define (%sigquit-handler)
    ;;SIGQUIT comes from Ctrl-\.
    (import LOGGING)
    (sel.receive-signal SIGQUIT %sigquit-handler)
    (log "received SIGQUIT")
    (sel.leave-asap))

  (define (%sigint-handler)
    ;;SIGINT comes from Ctrl-C.
    (import LOGGING)
    (sel.receive-signal SIGINT %sigint-handler)
    (log "received SIGINT")
    (sel.leave-asap))

  (define (%sigtstp-handler)
    ;;SIGTSTP  comes from  Ctrl-Z.   We should  put  some program  state
    ;;cleanup in this  handler.  Finally we send ourselves  a SIGSTOP to
    ;;suspend the process.
    (import LOGGING)
    (guard (E (else
	       (log "error in SIGTSTP handler: ~s\n" E)
	       (exit 1)))
      (sel.receive-signal SIGTSTP %sigtstp-handler)
      (log "received SIGTSTP")
      (px.kill (px.getpid) SIGSTOP)))

  (define (%sigcont-handler)
    ;;SIGCONT comes from the controlling process and allows us to resume
    ;;the program.   We should put  some state reinitialisation  in this
    ;;handler.
    (import LOGGING)
    (guard (E (else
	       (log "error in SIGCONT handler: ~s\n" E)
	       (exit 1)))
      (sel.receive-signal SIGCONT %sigcont-handler)
      (log "received SIGCONT")))

  #| end of module: INTERPROCESS-SIGNALS |# )


;;;; printing helpers

(define (%pretty-print . args)
  ;;This is for debugging purposes.
  ;;
  (pretty-print args (current-error-port)))

(module (verbose-message error-message-and-exit)

  (define (verbose-message requested-level template . args)
    (when (<= (options.verbosity) requested-level)
      (%format-and-print (current-error-port) template args)))

  (define (error-message-and-exit exit-status template . args)
    (%format-and-print (current-error-port) template args)
    (exit exit-status))

;;; --------------------------------------------------------------------

  (define (%format-and-print port template args)
    (fprintf port "vicare httpd: ")
    (apply fprintf port template args)
    (newline port)
    (flush-output-port port))

  #| end of module |# )


;;;; helpers

(define (absolutise-pathname pathname)
  (if (char=? #\/ (string-ref pathname 0))
      pathname
    (string-append (px.getcwd) pathname)))


;;;; done

(main (command-line))

;;; end of file
