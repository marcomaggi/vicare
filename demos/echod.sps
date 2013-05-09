;;;!vicare
;;;
;;;Part of: Vicare Scheme
;;;Contents: toy ECHO server
;;;Date: Mon May  6, 2013
;;;
;;;Abstract
;;;
;;;	This  program  implements  a  toy ECHO  server  as  testbed  for
;;;	libraries.  An echo server just accepts input strings terminated
;;;	by a LF or CRLF sequence and echoes them back to the client.
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
(import (vicare)
  (prefix (vicare posix)
	  px.)
  (prefix (vicare posix simple-event-loop)
	  sel.)
  (prefix (vicare posix log-files)
	  log.)
  (prefix (vicare posix pid-files)
	  pidfile.)
  (vicare platform constants)
  (vicare language-extensions syntaxes))


;;;; global variables

;;True if this  process is the root  server process; false if  this is a
;;children process  resulting from a  call to PX.FORK.  The  root server
;;process has cleaning duties.
;;
;;For the ECHO server the process is always the root one.
;;
(define root-server?
  (make-parameter #t))

;;An instance  of record  type "<global-options>" holding  global server
;;options configured from the command line.
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

(module (main)

  (define (main argv)
    ;;We catch the exceptions to exit with an error status.  If we catch
    ;;an exception here: we cannot log a message because we have already
    ;;closed the log file.
    ;;
    ;;Log lines specific  to the raised error should be  output near the
    ;;cause  of the  error, where  we can  better explain  what we  were
    ;;doing.
    ;;
    (guard (E (else (exit 1)))
      ;;Set configuration  parameters; it is useless  to use PARAMETRISE
      ;;here.
      (options
       (guard (E (else
		  (error-message-and-exit 1 "parsing options: ~a" (condition-message E))))
	 (make-<global-options> argv)))
      (log.logging-enabled?	(options.log-file))
      (log.log-pathname		(options.log-file))
      (pidfile.pid-pathname	(options.pid-file))
      (pidfile.log-procedure	log.log)
      (sel.log-procedure	log.log)
      (with-compensations
	(%main.open-log-file/c)
	(%main.log-server-start-messages)
	;;First daemonise, then open the PID file.
	(%main.daemonise)
	(%main.open-pid-file/c)
	(%main.enter-event-loop)
	(log.log "exiting ECHO server"))
      ;;First get out of WITH-COMPENSATIONS, then exit.
      (exit 0)))

  (define (%main.open-log-file/c)
    (compensate
	(guard (E (else
		   (error-message-and-exit 1 "opening log file: ~a" (condition-message E))))
	  (log.open-logging))
      (with
       (guard (E (else (void)))
	 (when (root-server?)
	   (log.close-logging))))))

  (define (%main.log-server-start-messages)
    (let ((pid (px.getpid)))
      (log.log-prefix (format "vicare echod[~a]: " pid))
      (log.log "*** starting ECHO server, pid=~a" pid)))

  (define (%main.daemonise)
    (import (vicare posix daemonisations))
    (when (options.daemonise?)
      (log.with-logging-handler
	  (condition-message "while daemonising server: ~a")
	(log.log "daemonising process")
	;;This returns the  new process group ID.  It is  not used here,
	;;but it might be useful in other servers.
	(begin0
	    (daemonise)
	  (let ((pid (px.getpid)))
	    (log.log-prefix (format "vicare echod[~a]: " pid))
	    (log.log "after daemonisation pid=~a" pid))))))

  (define (%main.open-pid-file/c)
    ;;Create the PID file, if requested, and push a compensation for its
    ;;removal.  When this  function is called: the log  facility must be
    ;;already set up and running; error messages are logged.
    ;;
    (compensate
	(pidfile.create-pid-file)
      (with
       (when (root-server?)
	 (pidfile.remove-pid-file)))))

  (define (%main.enter-event-loop)
    ;;Catch the exceptions here to log the event: exit because of error.
    ;;Then raise again  the exception to run the  compensations and exit
    ;;with error code.
    ;;
    (import SERVER-EVENTS-LOOP)
    (log.with-logging-handler
	(condition-message "exiting ECHO server because of error: ~a")
      (enter-event-loop (options.server-interface)
			(options.server-port)
			(options.server-loop-config))))

  #| end of module: MAIN |# )


;;;; type definitions

;;Hold global server options configured from the command line.
;;
(define-record-type <global-options>
  (fields (mutable server-interface)
		;A string representing the  server interface to bind to.
		;Defaults to "localhost".
	  (mutable server-port)
		;An exact integer representing the server port to listen
		;to.  Defaults to 8081.
	  (mutable pid-file)
		;False or a string representing  the pathname of the PID
		;file.  When false: no PID file is created.
	  (mutable log-file)
		;False or a string representing  the pathname of the log
		;file.   As  special case:  if  the  string is  "-"  log
		;messages should go to stderr.
	  (mutable daemonise?)
		;Boolean, true if the server must be daemonised.
	  (mutable server-loop-config)
		;ENUM-SET of type SERVER-LOOP-OPTION.
	  (mutable verbosity)
		;An exact integer.  When zero: run the program silently;
		;this is the default.  When  a positive integer: run the
		;program  with  verbose   messages  at  the  appropriate
		;verbosity level.
	  )
  (protocol
   (lambda (maker)
     (lambda (argv)
       (import COMMAND-LINE-ARGS SERVER-EVENTS-LOOP)
       (define-syntax-rule (%err ?template . ?args)
	 (error-message-and-exit BAD-OPTION-EXIT-STATUS ?template . ?args))
       (let ((self (maker "localhost" 8081
			  #f #;pid-file #f #;log-file
			  #f #;daemonise? (server-loop-config)
			  0 #;verbosity )))
	 (parse-command-line-arguments self argv)

	 ;; validate server interface, more validation later
	 (let ((interface ($<global-options>-server-interface self)))
	   (unless (and (string? interface)
			(not (zero? (string-length interface))))
	     (%err "invalid server interface: \"~a\"" interface)))

	 ;; validate server port
	 (let ((port ($<global-options>-server-port self)))
	   (import NETWORKING)
	   (cond ((not (net.network-port? port))
		  (%err "invalid server port: \"~a\"" port))))

	 ;; validate pid file
	 (let ((filename ($<global-options>-pid-file self)))
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
		      (<global-options>-pid-file-set! self filename))))))

	 ;; validate log file
	 (let ((filename ($<global-options>-log-file self)))
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
		    (<global-options>-log-file-set! self filename)))))

	 self)))))

;;; --------------------------------------------------------------------

(define (<global-options>-increment-verbosity! opts)
  (<global-options>-verbosity-set! opts (+ +1 (<global-options>-verbosity opts))))

(define (<global-options>-decrement-verbosity! opts)
  (<global-options>-verbosity-set! opts (+ -1 (<global-options>-verbosity opts))))

;;; --------------------------------------------------------------------

(define (options.server-interface)
  ($<global-options>-server-interface (options)))

(define (options.server-port)
  ($<global-options>-server-port (options)))

(define (options.pid-file)
  ;;Return false if no PID file must be created.  Return a string if the
  ;;PID file must be created; the  string represents the pathname of the
  ;;PID file.
  ;;
  ($<global-options>-pid-file (options)))

(define (options.log-file)
  ;;Return  false if  logging  must  be disabled.   Return  a string  if
  ;;logging must be  enabled; the string represents the  pathname of the
  ;;log file; as special case: if  the string is "-" log messages should
  ;;go to stderr.
  ;;
  ($<global-options>-log-file (options)))

(define (options.verbosity)
  ($<global-options>-verbosity (options)))

(define (options.daemonise?)
  ($<global-options>-daemonise? (options)))

(define (options.server-loop-config)
  ($<global-options>-server-loop-config (options)))


;;;; command line arguments parsing

(module COMMAND-LINE-ARGS
  (parse-command-line-arguments)
  (import (srfi :37 args-fold))

  (define (parse-command-line-arguments seed argv)
    (args-fold (cdr argv) program-options
	       unrecognised-option-proc
	       argument-processor
	       seed))

;;; --------------------------------------------------------------------

  (define-constant HELP-SCREEN
    "Usage: echod.sps [vicare options] -- [options]
Options:
   -I IFACE
   --interface IFACE
\tSelect the server interface to bind to.

   -P PORT
   --port PORT
\tSelect the server port to listen to (1...65535)

   --pid-file /path/to/pid-file
\tSelect the pathname for the PID file.  When not given
\tno PID file is created.

   --log-file /path/to/log-file
\tSelect the pathname for the log file.  Use \"-\" to log
\ton the error port.  When not given no log file is created.

   --daemon
\tTurn the server process into a daemon.

   --dry-run
\tCreate the master socket, bind it to the interface, then
\tshut it down and exit.

   -V
   --version
\tPrint version informations and exit.

   --version-only
\tPrint version number only and exit.

   -v
   --verbose
\tPrint verbose messages.

   -h
   --help
\tPrint this help screen and exit.\n")

  (define-constant VERSION-SCREEN
    "Vicare ECHOD ~a\n\
     Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>\n\
     This is free software; see the source for copying conditions.  There is NO\n\
     warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n")

;;; --------------------------------------------------------------------

  (define (interface-option-processor option name operand seed)
    ;;Select the interface to bind to.  We will validate this later.
    ;;
    (<global-options>-server-interface-set! seed operand)
    seed)

  (define (port-option-processor option name operand seed)
    (let ((port (string->number operand)))
      (unless port
	(invalid-option-value name operand))
      (<global-options>-server-port-set! seed port))
    seed)

  (define (pid-file-option-processor option name operand seed)
    (<global-options>-pid-file-set! seed operand)
    seed)

  (define (log-file-option-processor option name operand seed)
    (<global-options>-log-file-set! seed operand)
    seed)

  (define (daemon-option-processor option name operand seed)
    (<global-options>-daemonise?-set! seed #t)
    seed)

  (define (dry-run-option-processor option name operand seed)
    (import SERVER-EVENTS-LOOP)
    (<global-options>-server-loop-config-set! seed (enum-set-union
						    (server-loop-config dry-run)
						    (<global-options>-server-loop-config seed)))
    seed)

;;; --------------------------------------------------------------------
;;; auxiliary options

  (define (verbosity-option-processor option name operand seed)
    (<global-options>-increment-verbosity! seed)
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
    ;;List of  options recognised by  this program.  The format  of each
    ;;option specification is:
    ;;
    ;;   names required-arg? optional-arg? option-proc
    ;;
    (list
     (option '(#\I "interface")	#t #f interface-option-processor)
     (option '(#\P "port")	#t #f port-option-processor)
     (option '("pid-file")	#t #f pid-file-option-processor)
     (option '("log-file")	#t #f log-file-option-processor)
     (option '("daemon")	#f #f daemon-option-processor)
     (option '("dry-run")	#f #f dry-run-option-processor)

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

  (define-syntax-rule (%err ?template . ?args)
    (error-message-and-exit BAD-OPTION-EXIT-STATUS ?template . ?args))

  #| end of module: COMMAND-LINE-ARGS |#)


;;;; server events loop

(module SERVER-EVENTS-LOOP
  (enter-event-loop server-loop-option server-loop-config)

  (define-enumeration server-loop-option
    (dry-run
		;Initialise the loop, create the master sock and bind it
		;to the  given interface.  Do  not enter the  loop: just
		;shut down the master socket and return.
     )
    server-loop-config)

  (define (enter-event-loop interface port options-set)
    ;;To be called  by the main server function.   Initialise the Simple
    ;;Event Loop library and enter the loop.  This function returns when
    ;;the process must be exited.
    ;;
    ;;Given  a  string INTERFACE  representing  a  network interface  to
    ;;listen  to and  a network  PORT number:  create the  master server
    ;;socket bound  to the  given interface  and enter  the SEL  loop to
    ;;accept incoming connections.  Return unspecified values.
    ;;
    ;;INTERFACE must  be a string  representing the server  interface to
    ;;bind to; for example "localhost".
    ;;
    ;;PORT  must be  an exact  integer representing  the server  port to
    ;;listen to; for example 8081.
    ;;
    ;;OPTIONS-SET must be an ENUM-SET  of type SERVER-OPTION; it is used
    ;;to configure the server loop.
    ;;
    (import NETWORKING INTERPROCESS-SIGNALS)
    (with-compensations
      (define master-sock
	(compensate
	    (log.with-logging-handler
		(condition-message "while creating master socket: ~a")
	      (net.make-master-sock interface port))
	  (with
	   (net.close-master-sock master-sock))))
      (define (schedule-incoming-connection-event)
	(sel.readable master-sock (lambda ()
				    (schedule-incoming-connection-event)
				    (%accept-connection master-sock))))
      (compensate
	  (sel.initialise)
	(with
	 (sel.finalise)))
      (initialise-signal-handlers)
      (if (%config.dry-run? options-set)
	  (log.log "requested dry run")
	(begin
	  (log.log "listening to: ~a:~a" interface port)
	  (schedule-incoming-connection-event)
	  ;;We return from this form only when it is time to exit the process.
	  (sel.enter)))
      (void)))

  (define (%accept-connection master-sock)
    ;;Given  the  socket  descriptor MASTER-SOCK  representing  a  bound
    ;;socket with a pending connection:  accept the connection, create a
    ;;server socket, enter the procedure that handles incoming data.
    ;;
    (import NETWORKING)
    (log.with-logging-handler
	(condition-message "while accepting connection: ~a")
      (receive (server-sock server-port client-address)
	  (net.make-server-sock-and-port master-sock)
	;;We never use the SERVER-SOCK in this application.
	(sel.readable server-port
		      (lambda ()
			(import ECHO-SERVER)
			(proto.start-session server-port client-address)))
	(void))))

  (define (%config.dry-run? options-set)
    (enum-set-member? (server-loop-option dry-run) options-set))

  #| end of module: NETWORKING |# )


;;;; sockets handling

(module NETWORKING
  (net.network-port?
   net.make-master-sock		net.make-server-sock-and-port
   net.close-master-sock	net.close-server-port)

  (define (net.make-master-sock interface port)
    ;;Given  a  string INTERFACE  representing  a  network interface  to
    ;;listen to and  a network PORT number: open a  master server socket
    ;;and bind it  to the interface and port.  Return  the master socket
    ;;descriptor.
    ;;
    ;;INTERFACE must  be a string  representing the server  interface to
    ;;bind to; for example "localhost".
    ;;
    ;;PORT  must be  an exact  integer representing  the server  port to
    ;;listen to; for example 8081.
    ;;
    (let ((sockaddr    (%make-sockaddr interface (number->string port)))
	  (master-sock (px.socket PF_INET SOCK_STREAM 0)))
      (%socket-set-non-blocking master-sock)
      (px.setsockopt/linger master-sock #t 1)
      (px.setsockopt/int    master-sock SOL_SOCKET SO_REUSEADDR #t)
      (px.bind   master-sock sockaddr)
      (px.listen master-sock 10)
      master-sock))

  (define (net.make-server-sock-and-port master-sock)
    ;;Given  the  socket  descriptor MASTER-SOCK  representing  a  bound
    ;;socket  with  a  pending  connection: accept  the  connection  and
    ;;configure the resulting server socket descriptor to non-blocking.
    ;;
    ;;Return 3 values:
    ;;
    ;;1. The server socket descriptor.
    ;;
    ;;2.  A Scheme  input/output  binary port  wrapping the  descriptor.
    ;;   Closing the Scheme port will also close the server socket.
    ;;
    ;;3.  A  bytevector  representing  the  client  address  as  "struct
    ;;   sockaddr".
    ;;
    (receive (server-sock client-address)
	(px.accept master-sock)
      (let* ((remote-address.bv   (px.sockaddr_in.in_addr client-address))
	     (remote-address.str  (px.inet-ntop/string AF_INET remote-address.bv))
	     (remote-port         (px.sockaddr_in.in_port client-address))
	     (remote-port.str     (number->string remote-port))
	     (port-id             (string-append remote-address.str ":" remote-port.str)))
	(%socket-set-non-blocking server-sock)
	(let ((server-port (make-binary-socket-input/output-port server-sock port-id)))
	  (values server-sock server-port client-address)))))

;;; --------------------------------------------------------------------

  (define (net.close-master-sock sock)
    ;;Close the master socket descriptor, shutting down listening to the
    ;;bound interface.  SOCK must be the return value of a previous call
    ;;to NET.MAKE-MASTER-SOCK.
    ;;
    (px.close sock))

  (define (net.close-server-port port)
    ;;Close  the  Scheme  port  wrapping a  server  connection's  socket
    ;;descriptor; closing the port will also shut down the socket.  PORT
    ;;must   be   the    return   value   of   a    previous   call   to
    ;;NET.MAKE-SERVER-SOCK-AND-PORT.
    ;;
    (close-port port))

;;; --------------------------------------------------------------------

  (define (%make-sockaddr interface port)
    ;;Given  a  string INTERFACE  representing  a  network interface  to
    ;;listen to and a network PORT number: query the system for SOCKADDR
    ;;structures  representing such  interface and  port and  supporting
    ;;IPv4.
    ;;
    ;;Return  a Scheme  bytevector  holding the  first matching  "struct
    ;;sockaddr"; if no address can be determined: raise an exception.
    ;;
    (let* ((hints (px.make-struct-addrinfo 0 AF_INET SOCK_STREAM 0 0 #f #f))
	   (infos (px.getaddrinfo interface port hints)))
      #;(debug-print infos)
      (if (null? infos)
	  (error 'make-sockaddr "unable to acquire master socket address")
	(px.struct-addrinfo-ai_addr (car infos)))))

;;; --------------------------------------------------------------------

  (define (%socket-set-non-blocking sock)
    ;;Configure the given socket descriptor for non-blocking mode.
    ;;
    (let ((x (px.fcntl sock F_GETFL 0)))
      (px.fcntl sock F_SETFL (bitwise-ior x O_NONBLOCK))))

  (define (net.network-port? obj)
    ;;Return true  if OBJ is  an exact integer  in the range  of network
    ;;ports.
    ;;
    (and (fixnum? obj)
	 (<= 1 obj 65535)))

  #| end of module: SOCKETS |# )


;;;; ECHO server

(module ECHO-SERVER
  (proto.start-session)
  (import NETWORKING (prefix (vicare net channels) chan.))

  (define (proto.start-session server-port client-address)
    (log.with-logging-handler
	(condition-message "while starting session: ~a")
      (define connection-id
	(gensym->unique-string (gensym)))
      (%log-accepted-connection client-address connection-id)
      (let ((chan (chan.open-input/output-channel server-port)))
	(chan.channel-set-message-terminators! chan '(#ve(ascii "\r\n") #ve(ascii "\n")))
	(%send-message chan '(#ve(ascii "Vicare ECHO daemon.\r\n")))
	(chan.channel-recv-begin! chan)
	(%process-incoming-data server-port chan connection-id))))

  (define (%log-accepted-connection client-address connection-id)
    (let* ((remote-address.bv   (px.sockaddr_in.in_addr client-address))
	   (remote-address.str  (px.inet-ntop/string AF_INET remote-address.bv))
	   (remote-port         (px.sockaddr_in.in_port client-address))
	   (remote-port.str     (number->string remote-port)))
      (log.log "accepted connection from: ~a:~a, connection-id=~a"
	       remote-address.str remote-port.str connection-id)))

  (define (%process-incoming-data server-port chan connection-id)
    (define (%reschedule)
      (sel.readable server-port
		    (lambda ()
		      (%process-incoming-data server-port chan connection-id))
		    (time-from-now (make-time 5 0))
		    (lambda ()
		      (log.log "connection ~a expired" connection-id)
		      (%stop-session connection-id server-port chan))))
    (log.with-logging-handler
	(condition-message "while processing incoming data: ~a")
      (cond ((chan.channel-recv-message-portion! chan)
	     => (lambda (dummy)
		  (let* ((data.bv  (chan.channel-recv-end! chan))
			 (data.str (utf8->string data.bv)))
		    (log.log "connection ~a echoing: ~a"
			     connection-id (ascii->string (uri-encode data.bv)))
		    (%send-message chan (list #ve(ascii "echo> ") data.bv))
		    (if (%received-quit? data.bv)
			(%stop-session connection-id server-port chan)
		      (begin
			(chan.channel-recv-begin! chan)
			(%reschedule))))))
	    (else
	     (%reschedule)))))

  (define (%stop-session connection-id server-port chan)
    (log.log "closing connection ~a" connection-id)
    (chan.close-channel chan)
    (net.close-server-port server-port))

  (define (%send-message chan data)
    ;;Send the list  of bytevectors DATA through the  channel; perform a
    ;;full send operation.  Return unspecified values.
    ;;
    (chan.channel-send-begin! chan)
    (for-each-in-order
	(lambda (bv)
	  (chan.channel-send-message-portion! chan bv))
      data)
    (chan.channel-send-end! chan))

  (define (%received-quit? bv)
    ;;We know that the last bytes of BV must represent \n or \r\n.
    ;;
    (let ((bv.len (bytevector-length bv)))
      (cond ((and (fx<= 2 bv.len)
		  (fx=? (char->integer #\return)
			(bytevector-u8-ref bv (fx- bv.len 2)))
		  (fx=? (char->integer #\newline)
			(bytevector-u8-ref bv (fx- bv.len 1))))
	     (bytevector=? bv #ve(ascii "quit\r\n")))
	    ((and (fx<= 1 bv.len)
		  (fx=? (char->integer #\newline)
			(bytevector-u8-ref bv (fx- bv.len 1))))
	     (bytevector=? bv #ve(ascii "quit\n")))
	    (else #f))))

  #| end of module: ECHO-SERVER |# )


;;;; interprocess signal handlers

(module INTERPROCESS-SIGNALS
  (initialise-signal-handlers)

  (define (initialise-signal-handlers)
    (sel.receive-signal SIGTERM %sigterm-handler)
    (sel.receive-signal SIGQUIT %sigquit-handler)
    (sel.receive-signal SIGINT  %sigint-handler)
    (sel.receive-signal SIGTSTP %sigtstp-handler)
    (sel.receive-signal SIGCONT %sigcont-handler)
    (sel.receive-signal SIGUSR1 %sigusr1-handler)
    (sel.receive-signal SIGUSR2 %sigusr2-handler))

  (define (%sigterm-handler)
    (sel.receive-signal SIGTERM %sigterm-handler)
    (log.log "received SIGTERM")
    (sel.leave-asap))

  (define (%sigquit-handler)
    ;;SIGQUIT comes from Ctrl-\.  The documentation of the GNU C Library
    ;;has this to say about SIGQUIT:
    ;;
    ;;   The SIGQUIT  signal  is  similar to  SIGINT,  except that  it's
    ;;  controlled by  a different key and produces a  core dump when it
    ;;  terminates the  process, just like a program  error signal.  You
    ;;  can think of this as a program error condition "detected" by the
    ;;  user.
    ;;
    ;;  Certain kinds of cleanups  are best omitted in handling SIGQUIT.
    ;;  For example,  if the program creates temporary  files, it should
    ;;  handle the other termination  requests by deleting the temporary
    ;;  files.  But it is better for SIGQUIT not to delete them, so that
    ;;  the user can examine them in conjunction with the core dump.
    ;;
    (sel.receive-signal SIGQUIT %sigquit-handler)
    (log.log "received SIGQUIT")
    (sel.leave-asap))

  (define (%sigint-handler)
    ;;SIGINT comes from Ctrl-C.
    (sel.receive-signal SIGINT %sigint-handler)
    (log.log "received SIGINT")
    (sel.leave-asap))

  (define (%sigtstp-handler)
    ;;SIGTSTP  comes from  Ctrl-Z.   We should  put  some process  state
    ;;suspension  finalisation   in  this  handler.   Finally   we  send
    ;;ourselves a SIGSTOP to suspend the process.
    (log.with-logging-handler
	(condition-message "error in SIGTSTP handler: ~a")
      (sel.receive-signal SIGTSTP %sigtstp-handler)
      (log.log "received SIGTSTP")
      (px.kill (px.getpid) SIGSTOP)))

  (define (%sigcont-handler)
    ;;SIGCONT comes from the controlling process and allows us to resume
    ;;the program.  We should put some process state reinitialisation in
    ;;this handler.
    (log.with-logging-handler
	(condition-message "error in SIGCONT handler: ~a")
      (sel.receive-signal SIGCONT %sigcont-handler)
      (log.log "received SIGCONT")))

  (define (%sigusr1-handler)
    ;;SIGUSR1  is  explicitly  sent  by  someone  to  perform  a  custom
    ;;procedure.
    ;;
    (log.with-logging-handler
	(condition-message "error in SIGUSR1 handler: ~a")
      (sel.receive-signal SIGUSR1 %sigusr1-handler)
      (log.log "received SIGUSR1")))

  (define (%sigusr2-handler)
    ;;SIGUSR2  is  explicitly  sent  by  someone  to  perform  a  custom
    ;;procedure.
    ;;
    (log.with-logging-handler
	(condition-message "error in SIGUSR2 handler: ~a")
      (sel.receive-signal SIGUSR2 %sigusr2-handler)
      (log.log "received SIGUSR2")))

  #| end of module: INTERPROCESS-SIGNALS |# )


;;;; printing helpers

(define (debug-print . args)
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
    (fprintf port "vicare echod: ")
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
;; Local Variables:
;; eval: (put 'log.with-logging-handler 'scheme-indent-function 1)
;; End:
