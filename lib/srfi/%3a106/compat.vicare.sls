;;;derived from the reference implementation of SRFI 106
;;;
;;;Copyright (C) 2012 Takashi Kato ktakashi@ymail.com
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


(library (srfi :106 compat)
  (export
    make-client-socket		make-server-socket
    socket?
    call-with-socket
    (rename (bitwise-ior socket-merge-flags)
	    (bitwise-xor socket-purge-flags))
    socket-accept
    socket-send			socket-recv
    socket-shutdown		socket-close

    *af-unspec*		*af-inet*		*af-inet6*
    *sock-stream*	*sock-dgram*
    *ai-canonname*	*ai-numerichost*
    *ai-v4mapped*	*ai-all*		*ai-addrconfig*
    *ipproto-ip*	*ipproto-tcp*		*ipproto-udp*
    *msg-peek*		*msg-oob*		*msg-waitall*
    *shut-rd*		*shut-wr*		*shut-rdwr*)
  (import (vicare)
    (vicare platform constants)
    (prefix (vicare posix) px.))


;;;; constants

(define-constant *af-unspec*		AF_UNSPEC)
(define-constant *af-inet*		AF_INET)
(define-constant *af-inet6*		AF_INET6)

(define-constant *sock-stream*		SOCK_STREAM)
(define-constant *sock-dgram*		SOCK_DGRAM)

(define-constant *ai-canonname*		AI_CANONNAME)
(define-constant *ai-numerichost*	AI_NUMERICHOST)
(define-constant *ai-v4mapped*		AI_V4MAPPED)
(define-constant *ai-all*		AI_ALL)
(define-constant *ai-addrconfig*	AI_ADDRCONFIG)

(define-constant *ipproto-ip*		IPPROTO_IP)
(define-constant *ipproto-tcp*		IPPROTO_TCP)
(define-constant *ipproto-udp*		IPPROTO_UDP)

(define-constant *msg-peek*		MSG_PEEK)
(define-constant *msg-oob*		MSG_OOB)
(define-constant *msg-waitall*		MSG_WAITALL)

(define-constant *shut-rd*		SHUT_RD)
(define-constant *shut-wr*		SHUT_WR)
(define-constant *shut-rdwr*		SHUT_RDWR)


;;;; socket constructors

(define-record-type (:socket :make-socket socket?)
  (nongenerative srfi:106:socket)
  (fields fd node service ai-family ai-socktype ai-flags ai-protocol))

;;; --------------------------------------------------------------------

(define-constant DEFAULT-AI-FLAGS
  (bitwise-ior *ai-v4mapped* *ai-addrconfig*))

(define-constant FAILED-CONNECTION-ERRNOS
  (list EADDRNOTAVAIL ETIMEDOUT ECONNREFUSED ENETUNREACH))

(define (%error-unable-to-find-remote-address who . irritants)
  (raise
   (condition (make-i/o-error)
	      (make-who-condition who)
	      (make-message-condition "cannot find address of remote host")
	      (make-irritants-condition irritants))))

;;This value is not specified by the SRFI.
;;
(define-constant MAX-PENDING-CONNECTIONS
  64)

;;; --------------------------------------------------------------------

(define make-client-socket
  (case-lambda
   ((node service)
    (make-client-socket node service *af-inet* *sock-stream* DEFAULT-AI-FLAGS *ipproto-ip*))

   ((node service ai-family)
    (make-client-socket node service ai-family *sock-stream* DEFAULT-AI-FLAGS *ipproto-ip*))

   ((node service ai-family ai-socktype)
    (make-client-socket node service ai-family ai-socktype   DEFAULT-AI-FLAGS *ipproto-ip*))

   ((node service ai-family ai-socktype ai-flags)
    (make-client-socket node service ai-family ai-socktype ai-flags *ipproto-ip*))

   ((node service ai-family ai-socktype ai-flags ai-protocol)
    (define who 'make-client-socket)
    (with-compensations/on-error
      (define fd
	(compensate
	    (px.socket ai-family ai-socktype ai-protocol)
	  (with
	   (px.close fd))))
      (define hints
	(px.make-struct-addrinfo ai-flags ai-family ai-socktype ai-protocol #f #f #f))
      (let next-addrinfo ((addrinfos (px.getaddrinfo node service hints)))
	(if (null? addrinfos)
	    (%error-unable-to-find-remote-address who node service ai-family ai-socktype ai-flags ai-protocol)
	  (let* ((info      (car addrinfos))
		 (sockaddr  (px.struct-addrinfo-ai_addr info)))
	    (guard (E ((and (errno-condition? E)
			    (memv (condition-errno E) FAILED-CONNECTION-ERRNOS))
		       (next-addrinfo (cdr addrinfos)))
		      (else
		       (raise E)))
	      (px.connect fd sockaddr)
	      (:make-socket fd node service ai-family ai-socktype ai-flags ai-protocol)))))))
   ))

(define make-server-socket
  (case-lambda
   ((service)
    (make-server-socket service *af-inet* *sock-stream* *ipproto-ip*))

   ((service ai-family)
    (make-server-socket service ai-family *sock-stream* *ipproto-ip*))

   ((service ai-family ai-socktype)
    (make-server-socket service ai-family ai-socktype   *ipproto-ip*))

   ((service ai-family ai-socktype ai-protocol)
    (define who 'make-server-socket)
    (with-compensations/on-error
      (define fd
	(compensate
	    (px.socket ai-family ai-socktype ai-protocol)
	  (with
	   (px.close fd))))
      (define hints
	;;Using AI_PASSIVE  here and specifying  #f as NODE  argument to
	;;GETADDRINFO  will  make  the  returned  address  suitable  for
	;;binding.
	(px.make-struct-addrinfo AI_PASSIVE ai-family ai-socktype ai-protocol #f #f #f))
      (let next-addrinfo ((addrinfos (px.getaddrinfo #f service hints)))
	(if (null? addrinfos)
	    (begin
	      (guard (E (else #f))
		(px.close fd))
	      (%error-unable-to-find-remote-address who service ai-family ai-socktype ai-protocol))
	  (let* ((info      (car addrinfos))
		 (sockaddr  (px.struct-addrinfo-ai_addr info)))
	    (guard (E ((and (errno-condition? E)
			    (memv (condition-errno E) FAILED-CONNECTION-ERRNOS))
		       (next-addrinfo (cdr addrinfos)))
		      (else
		       (raise E)))
	      (px.bind fd sockaddr)
	      (px.listen fd MAX-PENDING-CONNECTIONS)
	      (:make-socket fd #f service ai-family ai-socktype #f ai-protocol)))))))
   ))


;;;; socket operations

(define (socket-accept socket)
  (px.accept (:socket-fd socket)))

(define socket-send
  (case-lambda
   ((socket bv)
    (socket-send socket bv 0))
   ((socket bv flags)
    (px.send (:socket-fd socket) bv #f flags))
   ))

(define socket-recv
  (case-lambda
   ((socket size)
    (socket-recv socket size 0))
   ((socket size flags)
    (let* ((buf.bv   (make-bytevector size))
	   (recv.len (px.recv (:socket-fd socket) buf.bv #f flags)))
      (subbytevector-u8 buf.bv 0 recv.len)))
   ))

(define (socket-shutdown socket how)
  (px.shutdown (:socket-fd socket) how))

(define (socket-close socket)
  (px.close (:socket-fd socket)))

(define (call-with-socket socket proc)
  (begin0
      (proc socket)
    (socket-close socket)))


;;;; done

)

;;; end of file
