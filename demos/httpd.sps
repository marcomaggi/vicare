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
  (vicare net channels)
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

;;False or a textual output port to which log messages must be written.
;;
(define log-port
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
    (parametrise
	((logging	#t)
	 (sel.logging	#t)
	 (options	(make-<options> argv)))
      (create-pid-file)
      (unwind-protect
	  (begin
	    (open-log-file)
	    (log "starting HTTP server")
	    (unwind-protect
		(begin
		  (sel.initialise)
		  (unwind-protect
		      (begin
			(%initialise-signal-handlers)
			#;(let ((sockaddr
			       (px.make-sockaddr_in '#vu8(127 0 0 1)
						    (<options>-server-port (options))))
			      (master-sock (px.socket PF_INET SOCK_STREAM 0)))
			  (unwind-protect
			      (begin
				(let ((x (px.fcntl master-sock F_GETFL 0)))
				  (px.fcntl master-sock F_SETFL (bitwise-ior x O_NONBLOCK)))
				(px.bind   master-sock sockaddr)
				(px.listen master-sock 10)
				(sel.readable master-sock
					      (make-http-master-server-accept-handler master-sock))
				(sel.enter))
			    (px.close master-sock))))
		    (sel.finalise)
		    (log "exiting HTTP server")))
	      (close-log-file)))
	(remove-pid-file))
      (exit 0)))

;;; --------------------------------------------------------------------

  (define (create-pid-file)
    (define pid-file (<options>-pid-file (options)))
    (when pid-file
      (with-output-to-file pid-file
	  (lambda ()
	    (display (px.getpid))
	    (newline)))))

  (define (remove-pid-file)
    (define pid-file (<options>-pid-file (options)))
    (when pid-file
      (guard (E (else
		 (log "error removing pid file: ~a"
		      (if (message-condition? E)
			  (condition-message E)
			"unknown error"))))
	(when (and (root-server?)
		   (file-exists? pid-file))
	  (delete-file pid-file)))))

;;; --------------------------------------------------------------------

  (define (open-log-file)
    (define log-file (<options>-log-file (options)))
    (when log-file
      (log-port (open-file-output-port log-file
				       (file-options no-fail no-truncate)
				       (buffer-mode line)
				       (native-transcoder)))
      (set-port-position! (log-port) 0)))

  (define (close-log-file)
    (when log-port
      (close-port (log-port))))

  #| end of module: main |# )


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
       (let ((self (maker "localhost" 8080 #f #;document-root #f
			  #;pid-file #f #;log-file 0 #;verbosity )))
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
		 ((zero? (string-length filename))
		  (%err "selected empty log file pathname"))
		 (else
		  (let ((filename (absolutise-pathname filename)))
		    (when (and (file-exists? filename)
			       (not (and (px.file-is-regular-file? filename)
					 (px.file-writable? filename))))
		      (%err "selected log file pathname not writable" filename))
		    (<options>-log-file-set! self filename)))))

	 (log "document root: ~a"
	      ($<options>-document-root self))
	 (log "listening to: ~a:~a"
	      ($<options>-server-interface self)
	      ($<options>-server-port self))

	 self)))))

;;; --------------------------------------------------------------------

(define (<options>-increment-verbosity! opts)
  (<options>-verbosity-set! opts (+ +1 (<options>-verbosity opts))))

(define (<options>-decrement-verbosity! opts)
  (<options>-verbosity-set! opts (+ -1 (<options>-verbosity opts))))

;;; --------------------------------------------------------------------

(define (document-root)
  (<options>-document-root (options)))

(define (verbosity)
  (<options>-verbosity (options)))


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
\tSelect the pathname for the log file.
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


;;;; sockets handling

(define (network-port? obj)
  ;;Return  true if  OBJ is  an exact  integer in  the range  of network
  ;;ports.
  ;;
  (and (fixnum? obj)
       (<= 1 obj 65535)))


;;;; socket event handlers

(define (make-http-master-server-accept-handler master-sock)
  (import (srfi :31))
  (rec (handler)
    ;;Whenever the master server socket  becomes readable the event loop
    ;;applies this function to it.
    ;;
    ;;Accept a  connection creating a  server socket and  scheduling the
    ;;readable event  for it;  reschedule accepting readable  events for
    ;;MASTER-SOCKET; return unspecified values.
    ;;
    (define who 'http-master-server-accept-handler)
    (sel.readable master-sock handler)
    (let-values (((server-sock client-address)
		  (px.accept master-sock)))
      (guard (E (else
		 (log "exception in ~a: ~a\n" who E)
		 (px.close server-sock)
		 (exit 1)))
	(log "accepting connection from ~a\n" client-address)
	(sel.readable server-sock
		      (make-http-server-readable-socket server-sock))))))

(define (make-http-server-readable-socket server-sock)
  (import INPUT/OUTPUT
    (srfi :31))
  (let ((state 'start)
	(port	 (make-socket-port server-sock)))
    (rec (handler)
      (define who 'readable-socket-handler)
      (guard (E (else
		 (log "exception in ~a: ~a\n" who E)
		 (close-port port)))
	(case state
	  ((start)
;;;FIXME Do not read  all the lines, just one chunk at  a time using the
;;;event loop.
	   (let ((lines (read-until-empty-line port)))
;;;FIXME Extract the requested pathname.
	     (display (call-with-input-file
			  (string-append (document-root) "/index.html")
			get-string-all)
		      port)
	     (flush-output-port port)
	     (close-port port)
	     #;(sel.readable server-sock handler)
	     #;(set! state 'done)))
	  (else
	   (close-port port)))))))


;;;; input/output handling

(module INPUT/OUTPUT
  (make-socket-port
   read-until-empty-line)

  (define-constant SOCKET-TRANSCODER
    (make-transcoder (utf-8-codec)
		     (eol-style crlf)
		     (error-handling-mode replace)))

  (define-constant MAX-NUMBER-OF-ACCUMULATED-LINES
    64)

  (define (make-socket-port server-sock)
    (make-textual-socket-input/output-port server-sock "server port" SOCKET-TRANSCODER))

  (define read-until-empty-line
    (case-lambda
     ((port)
      (read-until-empty-line port 0))
     ((port number-of-accumulated-lines)
      ;;Recursively read  lines from PORT  until an empty line  is read.
      ;;If an  empty line is read:  return the (possibly empty)  list of
      ;;lines.  If EOF is read: return  #f.  If the number of read lines
      ;;exceeds the configured maximum: raise an "&error" exception.
      ;;
      (define who 'read-until-empty-line)
      (unless (fx< number-of-accumulated-lines MAX-NUMBER-OF-ACCUMULATED-LINES)
	(error who "too many lines read from client" number-of-accumulated-lines))
      (let ((line (read-line port)))
	(cond ((eof-object? line)
	       #f)
	      ((zero? (string-length line))
	       '())
	      (else
	       (cons line
		     (read-until-empty-line port (fxadd1 number-of-accumulated-lines))))
	      )))))

  #| end of module: INPUT/OUTPUT |# )


;;;; interprocess signal handlers

(define (%initialise-signal-handlers)
  (sel.receive-signal SIGTERM %sigterm-handler)
  (sel.receive-signal SIGQUIT %sigquit-handler)
  (sel.receive-signal SIGINT  %sigint-handler)
  (sel.receive-signal SIGTSTP %sigtstp-handler)
  (sel.receive-signal SIGCONT %sigcont-handler))

(define (%sigterm-handler)
  (sel.receive-signal SIGTERM %sigterm-handler)
  (log "received SIGTERM")
  (sel.leave-asap))

(define (%sigquit-handler)
  ;;SIGINT comes from Ctrl-\.
  (sel.receive-signal SIGQUIT %sigquit-handler)
  (log "received SIGQUIT")
  (sel.leave-asap))

(define (%sigint-handler)
  ;;SIGINT comes from Ctrl-C.
  (sel.receive-signal SIGINT %sigint-handler)
  (log "received SIGINT")
  (sel.leave-asap))

(define (%sigtstp-handler)
  ;;SIGTSTP comes from Ctrl-Z.  We should put some program state cleanup
  ;;in this handler.  Finally we send ourselves a SIGSTOP to suspend the
  ;;process.
  (guard (E (else
	     (log "error in SIGTSTP handler: ~s\n" E)
	     (exit 1)))
    (sel.receive-signal SIGTSTP %sigtstp-handler)
    (log "received SIGTSTP")
    (px.kill (px.getpid) SIGSTOP)))

(define (%sigcont-handler)
  ;;SIGCONT comes from  the controlling process and allows  us to resume
  ;;the  program.  We  should put  some state  reinitialisation in  this
  ;;handler.
  (guard (E (else
	     (log "error in SIGCONT handler: ~s\n" E)
	     (exit 1)))
    (sel.receive-signal SIGCONT %sigcont-handler)
    (log "received SIGCONT")))


;;;; printing helpers

(define (%pretty-print . args)
  ;;This is for debugging purposes.
  ;;
  (pretty-print args (current-error-port)))

(module (logging log verbose-message error-message-and-exit)

  (define logging
    (make-parameter #f
      (lambda (obj)
	(if obj #t #f))))

  (define (log template . args)
    (when (and (logging)
	       (log-port))
      (let* ((date	(px.strftime/string "%F-T%T%Z" (px.localtime (px.time))))
	     (template	(string-append (format "~a: " date) template)))
	(%format-and-print (log-port) template args))))

  (define (verbose-message requested-level template . args)
    (when (<= (verbosity) requested-level)
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
