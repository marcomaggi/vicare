;;;
;;;Part of: Vicare Scheme
;;;Contents: toy HTTP server
;;;Date: Tue Mar 19, 2013
;;;
;;;Abstract
;;;
;;;
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
  (prefix (vicare simple-event-loop)
	  sel.)
  (vicare platform constants)
  (vicare syntactic-extensions))


(define (main argv)
  (parametrise ((logging     #t)
		(sel.logging #t))
    (guard (E (else
	       (%pretty-print E)))
      (define options (make-<options> argv))
      (%log "starting HTTP server")
      (sel.initialise)
      (%initialise-signal-handlers)
      (unwind-protect
	  (let ((sockaddr    (px.make-sockaddr_in '#vu8(127 0 0 1)
						  (<options>-server-port options)))
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
	      (px.close master-sock)))
	(sel.finalise)))
    (%log "exiting HTTP server")
    (exit 0)))

(define-record-type <options>
  (fields (mutable server-port)
	  (mutable document-root))
  (protocol
   (lambda (maker)
     (lambda (argv)
       (import COMMAND-LINE-ARGS)
       (let ((self (maker 8080 (px.getcwd/string))))
	 (parse-command-line-arguments self argv)
	 (%log "listening to port: ~a" ($<options>-server-port   self))
	 (%log "document root: ~a"     ($<options>-document-root self))
	 self)))))

(define (network-port? obj)
  (and (fixnum? obj)
       (<= 1 obj 64000)))


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

  (define (port-option-processor option name operand seed)
    (let ((port (string->number operand)))
      (unless (and port (network-port? port))
	(invalid-option-value name operand))
      (<options>-server-port-set! seed port))
    seed)

  (define (document-root-option-processor option name operand seed)
    (unless (px.file-is-directory? operand)
      (invalid-option-value name operand))
    (<options>-document-root-set! seed (px.realpath/string operand))
    seed)

  (define program-options
    (list (option '(#\p "port") #t #f port-option-processor)
	  (option '("document-root") #t #f document-root-option-processor)))

;;; --------------------------------------------------------------------

  (define (argument-processor operand seed)
    (error-message-and-exit "invalid command line argument: ~a" operand))

  (define (invalid-option-value option value)
    (error-message-and-exit "invalid value for option \"~a\": ~a" option value))

  (define (unrecognised-option-proc option name arg seed)
    (error-message-and-exit "unknown command line option: ~a" name))

  (define (error-message-and-exit template . args)
    (fprintf (current-error-port) "vicare httpd: ")
    (apply fprintf (current-error-port) template args)
    (newline (current-error-port))
    (exit 1))

  #| end of module: COMMAND-LINE-ARGS |#)


;;;; socket event handlers

(define (make-http-master-server-accept-handler master-sock)
  (define (handler)
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
		 (%log "exception in ~a: ~s\n" who E)
		 (px.close server-sock)
		 (exit 1)))
	(%log "accepting connection from ~a\n" client-address)
	(sel.readable server-sock
		      (make-http-server-readable-socket server-sock)))))
  handler)

(define (make-http-server-readable-socket server-sock)
  (let ((state #f))
    (lambda ()
      (px.close server-sock))))


;;;; interprocess signal handlers

(define (%initialise-signal-handlers)
  (sel.receive-signal SIGTERM %sigterm-handler)
  (sel.receive-signal SIGQUIT %sigquit-handler)
  (sel.receive-signal SIGINT  %sigint-handler)
  (sel.receive-signal SIGTSTP %sigtstp-handler)
  (sel.receive-signal SIGCONT %sigcont-handler))

(define (%sigterm-handler)
  (sel.receive-signal SIGTERM %sigterm-handler)
  (%log "received SIGTERM")
  (sel.leave-asap))

(define (%sigquit-handler)
  ;;SIGINT comes from Ctrl-\.
  (sel.receive-signal SIGQUIT %sigquit-handler)
  (%log "received SIGQUIT")
  (sel.leave-asap))

(define (%sigint-handler)
  ;;SIGINT comes from Ctrl-C.
  (sel.receive-signal SIGINT %sigint-handler)
  (%log "received SIGINT")
  (sel.leave-asap))

(define (%sigtstp-handler)
  ;;SIGTSTP comes from Ctrl-Z.  We should put some program state cleanup
  ;;in this handler.  Finally we send ourselves a SIGSTOP to suspend the
  ;;process.
  (guard (E (else
	     (%log "error in SIGTSTP handler: ~s\n" E)
	     (exit 1)))
    (sel.receive-signal SIGTSTP %sigtstp-handler)
    (%log "received SIGTSTP")
    (px.kill (px.getpid) SIGSTOP)))

(define (%sigcont-handler)
  ;;SIGCONT comes from  the controlling process and allows  us to resume
  ;;the  program.  We  should put  some state  reinitialisation in  this
  ;;handler.
  (guard (E (else
	     (%log "error in SIGCONT handler: ~s\n" E)
	     (exit 1)))
    (sel.receive-signal SIGCONT %sigcont-handler)
    (%log "received SIGCONT")))


;;;; helpers

(define (%pretty-print . args)
  (pretty-print args (current-error-port)))

(define logging
  (make-parameter #f
    (lambda (obj)
      (if obj #t #f))))

(define (%log template . args)
  (when (logging)
    (let ((port (current-error-port)))
      (fprintf port "vicare httpd: ")
      (apply fprintf port template args)
      (fprintf port "\n")
      (flush-output-port port))))


;;;; done

(main (command-line))

;;; end of file
