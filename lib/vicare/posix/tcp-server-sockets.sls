;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: TCP server sockets facilities
;;;Date: Thu May  9, 2013
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


#!r6rs
(library (vicare posix tcp-server-sockets)
  (export
    make-master-sock			close-master-sock
    make-server-sock-and-port		close-server-port)
  (import (vicare)
    (prefix (vicare posix) px.)
    (vicare platform constants)
    (vicare arguments validation))


(define (make-master-sock interface port)
  ;;Given a string INTERFACE representing  a network interface to listen
  ;;to and a  network PORT number: open a master  server socket and bind
  ;;it to the interface and port.  Return the master socket descriptor.
  ;;
  ;;INTERFACE must be a string representing the server interface to bind
  ;;to; for example "localhost".
  ;;
  ;;PORT must be an exact integer representing the server port to listen
  ;;to; for example 8081.
  ;;
  (define who 'make-master-sock)
  (with-arguments-validation (who)
      ((non-empty-string	interface)
       (px.network-port-number	port))
    (let ((sockaddr    (%make-sockaddr interface (number->string port)))
	  (master-sock (px.socket PF_INET SOCK_STREAM 0)))
      (%socket-set-non-blocking master-sock)
      (px.setsockopt/linger master-sock #t 1)
      (px.setsockopt/int    master-sock SOL_SOCKET SO_REUSEADDR #t)
      (px.bind   master-sock sockaddr)
      (px.listen master-sock 10)
      master-sock)))

(define (close-master-sock sock)
  ;;Close the master  socket descriptor, shutting down  listening to the
  ;;bound interface.  SOCK  must be the return value of  a previous call
  ;;to MAKE-MASTER-SOCK.
  ;;
  (px.close sock))


(define (make-server-sock-and-port master-sock)
  ;;Given the socket descriptor  MASTER-SOCK representing a bound socket
  ;;with a pending  connection: accept the connection  and configure the
  ;;resulting server socket descriptor to non-blocking.
  ;;
  ;;Return 3 values:
  ;;
  ;;1. The server socket descriptor.
  ;;
  ;;2.   A  Scheme input/output  binary  port  wrapping the  descriptor.
  ;;   Closing the Scheme port will also close the server socket.
  ;;
  ;;3.   A  bytevector  representing   the  client  address  as  "struct
  ;;   sockaddr".
  ;;
  (define who 'make-server-sock-and-port)
  (with-arguments-validation (who)
      ((px.file-descriptor	master-sock))
    (receive (server-sock client-address)
	(px.accept master-sock)
      (let* ((remote-address.bv   (px.sockaddr_in.in_addr client-address))
	     (remote-address.str  (px.inet-ntop/string AF_INET remote-address.bv))
	     (remote-port         (px.sockaddr_in.in_port client-address))
	     (remote-port.str     (number->string remote-port))
	     (port-id             (string-append remote-address.str ":" remote-port.str)))
	(%socket-set-non-blocking server-sock)
	(let ((server-port (make-binary-socket-input/output-port server-sock port-id)))
	  (values server-sock server-port client-address))))))

(define (close-server-port port)
  ;;Close  the  Scheme  port   wrapping  a  server  connection's  socket
  ;;descriptor; closing the  port will also shut down  the socket.  PORT
  ;;must   be    the   return    value   of    a   previous    call   to
  ;;MAKE-SERVER-SOCK-AND-PORT.
  ;;
  (close-port port))


;;;; helpers

(define (%make-sockaddr interface port)
  ;;Given a string INTERFACE representing  a network interface to listen
  ;;to  and  a  network  PORT  number: query  the  system  for  SOCKADDR
  ;;structures representing such interface and port and supporting IPv4.
  ;;
  ;;Return  a  Scheme  bytevector  holding the  first  matching  "struct
  ;;sockaddr"; if no address can be determined: raise an exception.
  ;;
  (let* ((hints (px.make-struct-addrinfo 0 AF_INET SOCK_STREAM 0 0 #f #f))
	 (infos (px.getaddrinfo interface port hints)))
    #;(debug-print infos)
    (if (null? infos)
	(error 'make-sockaddr "unable to acquire master socket address")
      (px.struct-addrinfo-ai_addr (car infos)))))

(define (%socket-set-non-blocking sock)
  ;;Configure the given socket descriptor for non-blocking mode.
  ;;
  (let ((x (px.fcntl sock F_GETFL 0)))
    (px.fcntl sock F_SETFL (bitwise-ior x O_NONBLOCK))))


;;;; done

)

;;; end of file
