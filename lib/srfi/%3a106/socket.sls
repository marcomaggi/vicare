;;;derived from the reference implementation of SRFI 106
;;;
;;;Copyright (C) 2012 Takashi Kato <ktakashi@ymail.com>
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;All Rights Reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;``Software''), to deal in the Software without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission notice  shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE  IS PROVIDED  ``AS IS'', WITHOUT  WARRANTY OF  ANY KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT SHALL  THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER  IN AN
;;;ACTION OF  CONTRACT, TORT OR  OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION WITH  THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


(library (srfi :106 socket)
  (export
    make-client-socket		make-server-socket
    socket?			socket-descriptor

    socket-input-port		socket-output-port
    call-with-socket

    socket-merge-flags		socket-purge-flags
    socket-accept
    socket-send			socket-recv
    socket-shutdown		socket-close

    *af-unspec*		*af-inet*		*af-inet6*
    *sock-stream*	*sock-dgram*
    *ai-canonname*	*ai-numerichost*
    *ai-v4mapped*	*ai-all*		*ai-addrconfig*
    *ipproto-ip*	*ipproto-tcp*		*ipproto-udp*
    *msg-peek*		*msg-oob*		*msg-waitall*
    *shut-rd*		*shut-wr*		*shut-rdwr*

    address-family	socket-domain		address-info
    ip-protocol		message-type		shutdown-method)
  (import (vicare)
    (except (srfi :106 compat)
	    socket-port))


(define-constant %address-family
  `((inet    ,*af-inet*)
    (inet6   ,*af-inet6*)
    (unspec  ,*af-unspec*)))

(define-constant %address-info
  `((canoname     ,*ai-canonname*)
    (canonname    ,*ai-canonname*)
    (numerichost  ,*ai-numerichost*)
    (v4mapped     ,*ai-v4mapped*)
    (all          ,*ai-all*)
    (addrconfig   ,*ai-addrconfig*)))

(define-constant %ip-protocol
  `((ip  ,*ipproto-ip*)
    (tcp ,*ipproto-tcp*)
    (udp ,*ipproto-udp*)))

(define-constant %socket-domain
  `((stream   ,*sock-stream*)
    (datagram ,*sock-dgram*)))

(define-constant %message-types
  `((none 0)
    (peek ,*msg-peek*)
    (oob  ,*msg-oob*)
    (wait-all ,*msg-waitall*)))

(define (lookup who sets name)
  (cond ((assq name sets)
	 => cadr)
	(else
	 (assertion-violation who "no name defined" name))))

(define-syntax address-family
  (syntax-rules ()
    ((_ name)
     (lookup 'address-family %address-family 'name))))

(define-syntax address-info
  (syntax-rules ()
    ((_ names ...)
     (apply socket-merge-flags
	    (map (lambda (name) (lookup 'address-info %address-info name))
	      '(names ...))))))

(define-syntax socket-domain
  (syntax-rules ()
    ((_ name)
     (lookup 'socket-domain %socket-domain 'name))))

(define-syntax ip-protocol
  (syntax-rules ()
    ((_ name)
     (lookup 'ip-protocol %ip-protocol 'name))))

(define-syntax message-type
  (syntax-rules ()
    ((_ names ...)
     (apply socket-merge-flags
	    (map (lambda (name) (lookup 'message-type %message-types name))
	      '(names ...))))))

(define (%proper-method methods)
  (define allowed-methods '(read write))
  (define (check-methods methods)
    (let loop ((methods methods) (seen '()))
      (cond ((null? methods))
	    ((memq (car methods) allowed-methods)
	     => (lambda (m)
		  (if (memq (car m) seen)
		      (assertion-violation 'shutdown-method
			"duplicate method" m)
		    (loop (cdr methods) (cons (car m) seen)))))
	    (else (assertion-violation 'shutdown-method
		    "unknown method" (car methods))))))
  (check-methods methods)
  (if (null? (cdr methods))
      (case (car methods)
	((read) *shut-rd*)
	((write) *shut-wr*))
    *shut-rdwr*))

(define-syntax shutdown-method
  (syntax-rules ()
    ((_ methods ...)
     (%proper-method '(methods ...)))))


;;;; socket ports

;;These are  derived from the reference  implementation functions.  They
;;are replaced  by functions in  the compat library.  (Marco  Maggi; Thu
;;Aug 22, 2013)
;;
;; (define (socket-input-port socket)
;;   (define (read! bv start count)
;;     (let ((r (socket-recv socket count)))
;;       (bytevector-copy! r 0 bv start (bytevector-length r))
;;       (bytevector-length r)))
;;   (make-custom-binary-input-port "socket-input-port" read! #f #f #f))
;;
;; (define (socket-output-port socket)
;;   (define (write! bv start count)
;;     (let ((buf (make-bytevector count)))
;;       (bytevector-copy! bv start buf 0 count)
;;       (socket-send socket buf)))
;;   (make-custom-binary-output-port "socket-output-port" write! #f #f #f))


;;;; done

)

;;; end of file
