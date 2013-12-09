;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for POSIX library, network and sockets
;;;Date: Tue Jul 17, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (vicare posix) px.)
  (vicare platform constants)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing POSIX library, network and sockets\n")


;;;; helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))

(define-syntax catch-error
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((error? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))

(define-syntax with-temporary-file
  (syntax-rules ()
    ((_ (?pathname) . ?body)
     (let ((ptn ?pathname))
       (px.system (string-append "echo 123 > " ptn))
       (unwind-protect
	   (begin . ?body)
	 (px.system (string-append "rm -f " ptn)))))))


(parametrise ((check-test-name	'sockets))

  (check
      (px.sockaddr_un.pathname/string (px.make-sockaddr_un "/tmp/marco/the-unix-socket"))
    => "/tmp/marco/the-unix-socket")

;;; --------------------------------------------------------------------

  (check
      (let ((sockaddr (px.make-sockaddr_in '#vu8(1 2 3 4) 88)))
	(list (px.sockaddr_in.in_addr sockaddr)
	      (px.sockaddr_in.in_port sockaddr)))
    => '(#vu8(1 2 3 4) 88))

  (check
      (let ((sockaddr (px.make-sockaddr_in '#vu8(127 0 0 1) 88)))
	(px.sockaddr_in.in_addr sockaddr))
    => '#vu8(127 0 0 1))

  (check
      (let ((sockaddr (px.make-sockaddr_in '#vu8(127 0 0 1) 88)))
	(px.sockaddr_in.in_port sockaddr))
    => 88)

  (check
      (let ((sockaddr (px.make-sockaddr_in '#vu8(127 0 0 1) 88)))
	(px.sockaddr_in.in_addr.number sockaddr))
    => #x7f000001)

  (check
      (let* ((addr	(let ((bv (make-bytevector 4)))
			  (bytevector-u32-set! bv 0 INADDR_LOOPBACK (endianness big))
			  bv))
	     (sockaddr (px.make-sockaddr_in addr 88)))
	(list (px.sockaddr_in.in_addr sockaddr)
	      (px.sockaddr_in.in_port sockaddr)))
    => '(#vu8(127 0 0 1) 88))

  (check
      (let* ((addr	(let ((bv (make-bytevector 4)))
			  (bytevector-u32-set! bv 0 INADDR_BROADCAST (endianness big))
			  bv))
	     (sockaddr (px.make-sockaddr_in addr 88)))
	(list (px.sockaddr_in.in_addr sockaddr)
	      (px.sockaddr_in.in_port sockaddr)))
    => '(#vu8(255 255 255 255) 88))

;;; --------------------------------------------------------------------

  (check
      (let ((sockaddr (px.make-sockaddr_in6 '#vu16b(1 2 3 4  5 6 7 8) 88)))
	(list (px.sockaddr_in6.in6_addr sockaddr)
	      (px.sockaddr_in6.in6_port sockaddr)))
    => '(#vu16b(1 2 3 4  5 6 7 8) 88))

;;; --------------------------------------------------------------------

  (check
      (px.in6addr_loopback)
    => '#vu8(0 0 0 0   0 0 0 0   0 0 0 0   0 0 0 1))

  (check
      (px.in6addr_any)
    => '#vu8(0 0 0 0   0 0 0 0   0 0 0 0   0 0 0 0))

;;; --------------------------------------------------------------------

  (check
      (px.inet-aton "127.0.0.1")
    => '#vu8(127 0 0 1))

  (check
      (catch-error #f
	(px.inet-aton "ciao"))
    => '("ciao"))

  (check
      (px.inet-ntoa/string '#vu8(127 0 0 1))
    => "127.0.0.1")

;;; --------------------------------------------------------------------

  (check
      (px.inet-pton AF_INET "127.0.0.1")
    => '#vu8(127 0 0 1))

  (check
      (catch-error #f
	(px.inet-pton AF_INET "ciao"))
    => `(,AF_INET "ciao"))

  (check
      (px.inet-pton AF_INET6 "1:2:3:4:5:6:7:8")
    => '#vu16b(1 2 3 4 5 6 7 8))

  (check
      (px.inet-ntop/string AF_INET '#vu8(127 0 0 1))
    => "127.0.0.1")

  (check
      (px.inet-ntop/string AF_INET6 '#vu16b(1 2 3 4 5 6 7 8))
    => "1:2:3:4:5:6:7:8")

;;; --------------------------------------------------------------------

  (check
      (px.htonl #xA5B6C7D8)
    => #xD8C7B6A5)

  (check
      (px.ntohl #xA5B6C7D8)
    => #xD8C7B6A5)

  (check
      (px.htons #xA5B6)
    => #xB6A5)

  (check
      (px.ntohs #xA5B6)
    => #xB6A5)

  (check
      (px.ntohl (px.htonl #xA5B6C7D8))
    => #xA5B6C7D8)

  (check
      (px.ntohs (px.htons #xA5B6))
    => #xA5B6)

;;; --------------------------------------------------------------------

  (check
    (let ((S (px.gethostbyname "localhost")))
;;;      (check-pretty-print S)
      (list (px.struct-hostent? S)
	    (utf8->string (px.struct-hostent-h_name S))
	    (for-all bytevector? (px.struct-hostent-h_aliases S))
	    (px.struct-hostent-h_addrtype  S)
	    (px.struct-hostent-h_length    S)
	    (px.struct-hostent-h_addr_list S)
	    (px.struct-hostent-h_addr      S)))
    => `(#t "localhost" #t ,AF_INET 4 (#vu8(127 0 0 1)) #vu8(127 0 0 1)))

  (check
      (let ((S (px.gethostbyname2 "localhost" AF_INET)))
;;;(check-pretty-print S)
	(list (px.struct-hostent? S)
	      (utf8->string (px.struct-hostent-h_name S))
	      (for-all bytevector? (px.struct-hostent-h_aliases S))
	      (px.struct-hostent-h_addrtype  S)
	      (px.struct-hostent-h_length    S)
	      (px.struct-hostent-h_addr_list S)
	      (px.struct-hostent-h_addr      S)))
    => `(#t "localhost" #t ,AF_INET 4 (#vu8(127 0 0 1)) #vu8(127 0 0 1)))

  (check
      (let ((S (px.gethostbyaddr '#vu8(127 0 0 1))))
;;;(check-pretty-print S)
	(list (px.struct-hostent? S)
	      (utf8->string (px.struct-hostent-h_name S))
	      (px.struct-hostent-h_aliases   S)
	      (px.struct-hostent-h_addrtype  S)
	      (px.struct-hostent-h_length    S)
	      (px.struct-hostent-h_addr_list S)
	      (px.struct-hostent-h_addr      S)))
    => `(#t "localhost" () ,AF_INET 4 (#vu8(127 0 0 1)) #vu8(127 0 0 1)))

  (check
      (for-all px.struct-hostent? (px.host-entries))
    => #t)

;;;(check-pretty-print (cons '/etc/hosts (px.host-entries)))

;;; --------------------------------------------------------------------

  ;;GETADDRINFO  works only  when we  are  connected on  the Net  (Marco
  ;;Maggi; Thu Nov 17, 2011).
  (when #f
    (check
	(let* ((hints	(px.make-struct-addrinfo AI_CANONNAME AF_INET SOCK_STREAM 0 #f #f #f))
	       (rv	(px.getaddrinfo "localhost" "smtp" hints)))
	  (for-all px.struct-addrinfo? rv))
      => #t)

    (check
	(let ((rv (px.getaddrinfo "localhost" "smtp" #f)))
	  (for-all px.struct-addrinfo? rv))
      => #t)

    (check
	(let ((rv (px.getaddrinfo "localhost" #f #f)))
	  (for-all px.struct-addrinfo? rv))
      => #t)

    #f)

  (check
      (string? (px.gai-strerror EAI_FAMILY))
    => #t)

;;; --------------------------------------------------------------------

    (check
	(let ((rv (px.getprotobyname "icmp")))
;;;	(check-pretty-print rv)
	  (px.struct-protoent? rv))
      => #t)

    (check
	(let ((rv (px.getprotobyname "udp")))
;;;	(check-pretty-print rv)
	  (px.struct-protoent? rv))
      => #t)

    (check
	(let ((rv (px.getprotobyname "tcp")))
;;;	(check-pretty-print rv)
	  (px.struct-protoent? rv))
      => #t)

    (check
	(let ((rv (px.getprotobynumber 6)))
;;;	(check-pretty-print rv)
	  (px.struct-protoent? rv))
      => #t)

;;; (check-pretty-print (px.protocol-entries))

;;; --------------------------------------------------------------------

    (check
	(let ((rv (px.getservbyname "smtp" "tcp")))
;;;	(check-pretty-print rv)
	  (px.struct-servent? rv))
      => #t)

    (check
	(let ((rv (px.getservbyname "http" "tcp")))
;;;	(check-pretty-print rv)
	  (px.struct-servent? rv))
      => #t)

    (check
	(let ((rv (px.getservbyname "ntp" "tcp")))
;;;	(check-pretty-print rv)
	  (px.struct-servent? rv))
      => #t)

    (check
	(let ((rv (px.getservbyport 25 "tcp")))
;;;	(check-pretty-print rv)
	  (px.struct-servent? rv))
      => #t)

    (check
	(for-all px.struct-servent? (px.service-entries))
      => #t)

;;;  (check-pretty-print (px.service-entries))

;;; --------------------------------------------------------------------

    (when #f

      (check
	  (let ((rv (px.getnetbyname "loopback")))
	    (check-pretty-print rv)
	    (px.struct-netent? rv))
	=> #t)

      (check
	  (let ((rv (px.getnetbyaddr (bytevector-u32-ref '#vu8(127 0 0 0) 0 (endianness big))
				     AF_INET)))
;;;	    (check-pretty-print rv)
	    (px.struct-netent? rv))
	=> #t)

      (check
	  (for-all px.struct-netent? (px.network-entries))
	=> #t)

;;;   (check-pretty-print (px.network-entries))

      #f)

    #t)


(parametrise ((check-test-name	'net))

  (define run-inet-tests? ;the firewall must allow it
    (or #f (px.getenv "RUN_INET_TESTS")))

  (check	;socketpair, read, write, shutdown
      (let-values (((a b) (px.socketpair PF_LOCAL SOCK_DGRAM 0)))
	(px.write a '#vu8(1 2 3 4))
	(let ((buf (make-bytevector 4)))
	  (px.read b buf)
	  (px.shutdown a SHUT_RDWR)
	  (px.shutdown b SHUT_RDWR)
	  buf))
    => '#vu8(1 2 3 4))

  (check	;socketpair, send, recv, shutdown
      (let-values (((a b) (px.socketpair PF_LOCAL SOCK_DGRAM 0)))
	(px.send a '#vu8(1 2 3 4) #f 0)
	(let ((buf (make-bytevector 4)))
	  (px.recv b buf #f 0)
	  (px.shutdown a SHUT_RDWR)
	  (px.shutdown b SHUT_RDWR)
	  buf))
    => '#vu8(1 2 3 4))

;;; --------------------------------------------------------------------
;;; options

;;;Do not  use SHUTDOWN here  because the  socket may not  be connected.
;;;Use CLOSE instead.

  (check
      (let* ((sock   (px.socket PF_LOCAL SOCK_STREAM 0))
	     (result (px.getsockopt/int sock SOL_SOCKET SO_DEBUG)))
	(px.close sock)
	result)
    => 0)

  (check
      (let ((sock   (px.socket PF_LOCAL SOCK_STREAM 0)))
	(px.setsockopt/int sock SOL_SOCKET SO_REUSEADDR #t)
	(let ((result (px.getsockopt/int sock SOL_SOCKET SO_REUSEADDR)))
	  (px.close sock)
	  result))
    => 1)

  (check
      (let ((sock   (px.socket PF_LOCAL SOCK_STREAM 0)))
	(px.setsockopt/int sock SOL_SOCKET SO_REUSEADDR #f)
	(let ((result (px.getsockopt/int sock SOL_SOCKET SO_REUSEADDR)))
	  (px.close sock)
	  result))
    => 0)

  (check
      (let* ((sock   (px.socket PF_LOCAL SOCK_STREAM 0))
	     (result (px.getsockopt/int sock SOL_SOCKET SO_KEEPALIVE)))
	(px.close sock)
	result)
    => 0)

  (check
      (let ((sock   (px.socket PF_LOCAL SOCK_STREAM 0)))
	(px.setsockopt/int sock SOL_SOCKET SO_KEEPALIVE #t)
	(let ((result (px.getsockopt/int sock SOL_SOCKET SO_KEEPALIVE)))
	  (px.close sock)
	  result))
    => 1)

  (check
      (let* ((sock   (px.socket PF_LOCAL SOCK_STREAM 0))
	     (result (px.getsockopt/size_t sock SOL_SOCKET SO_SNDBUF)))
	(px.close sock)
	(and (integer?  result)
	     (exact?    result)
	     (positive? result)))
    => #t)

;;;This test behaves  like this for no documented  reason; it looks like
;;;the  size of  the buffer  set  by SO_SNDBUF  is a  suggestion to  the
;;;system, rather than a strict order.
;;;
  (check
      (let ((sock   (px.socket PF_LOCAL SOCK_STREAM 0)))
	(px.setsockopt/size_t sock SOL_SOCKET SO_SNDBUF 3000)
	(let ((result (px.getsockopt/size_t sock SOL_SOCKET SO_SNDBUF)))
	  (px.close sock)
	  (and (integer?  result)
	       (exact?    result)
	       (positive? result))))
    => #t)

  (check
      (let* ((sock   (px.socket PF_LOCAL SOCK_STREAM 0))
	     (result (px.getsockopt/int sock SOL_SOCKET SO_TYPE)))
	(px.close sock)
	result)
    => SOCK_STREAM)

  (check
      (let ((sock   (px.socket PF_LOCAL SOCK_STREAM 0)))
	(let-values (((onoff linger) (px.getsockopt/linger sock)))
	  (px.close sock)
	  (cons onoff linger)))
    => '(#f . 0))

  (check
      (let ((sock   (px.socket PF_LOCAL SOCK_STREAM 0)))
	(px.setsockopt/linger sock #t 123)
	(let-values (((onoff linger) (px.getsockopt/linger sock)))
	  (px.close sock)
	  (cons onoff linger)))
    => '(#t . 123))

;;; --------------------------------------------------------------------
;;; PF_LOCAL, SOCK_STREAM

  (check	;fork process, raw bytevector input/output
      (with-result
       (let* ((pathname	(string-append (px.getenv "TMPDIR") "/proof"))
	      (sockaddr	(px.make-sockaddr_un pathname)))
	 (define (parent pid)
	   (let ((server-sock (px.socket PF_LOCAL SOCK_STREAM 0)))
	     (unwind-protect
		 (begin
		   (px.setsockopt/int server-sock SOL_SOCKET SO_REUSEADDR #t)
		   (px.bind   server-sock sockaddr)
		   (px.listen server-sock 2)
		   (let-values (((sock client-address) (px.accept server-sock)))
;;;		     (check-pretty-print (sockaddr_un.pathname/string client-address))
		     (px.setsockopt/linger sock #t 1)
		     (unwind-protect
			 (let ((bv (make-bytevector 3)))
			   (px.write sock '#vu8(1 2 3))
			   (px.read  sock bv)
			   (add-result bv))
		       (px.close sock))))
	       (px.close server-sock)
	       (px.waitpid pid 0))))
	 (define (child)
	   (px.nanosleep 1 0) ;give parent the time to listen
	   (let ((sock (px.socket PF_LOCAL SOCK_STREAM 0)))
	     (px.setsockopt/linger sock #t 1)
	     (unwind-protect
		 (let ((bv (make-bytevector 3)))
		   (px.connect sock sockaddr)
		   (px.read sock bv)
		   (assert (equal? bv '#vu8(1 2 3)))
		   (px.write sock bv))
	       (px.close sock)))
	   (exit 0))
	 (when (file-exists? pathname) (px.unlink pathname))
	 (px.fork parent child)
	 (when (file-exists? pathname) (px.unlink pathname))
	 #t))
    => '(#t (#vu8(1 2 3))))

  (check	;fork process, binary port input/output
      (with-result
       (let* ((pathname	(string-append (px.getenv "TMPDIR") "/proof"))
	      (sockaddr	(px.make-sockaddr_un pathname)))
	 (define (parent pid)
	   (let ((server-sock (px.socket PF_LOCAL SOCK_STREAM 0)))
	     (unwind-protect
		 (begin
		   (px.setsockopt/int server-sock SOL_SOCKET SO_REUSEADDR #t)
		   (px.bind   server-sock sockaddr)
		   (px.listen server-sock 2)
		   (let-values (((sock client-address) (px.accept server-sock)))
		     (px.setsockopt/linger sock #t 1)
		     (let ((port (make-binary-socket-input/output-port sock "*parent-sock*")))
		       (unwind-protect
			   (let ((bv (make-bytevector 3)))
			     (put-bytevector port '#vu8(1 2 3))
			     (flush-output-port port)
			     (get-bytevector-n! port bv 0 3)
;;;			     (check-pretty-print (list 'parent-recv bv))
			     (add-result bv))
			 (close-port port)))))
	       (px.close server-sock)
	       (px.waitpid pid 0))))
	 (define (child)
	   (guard (E (else
		      (check-pretty-print E)))
	     (px.nanosleep 1 0) ;give parent the time to listen
	     (let* ((sock (px.socket PF_LOCAL SOCK_STREAM 0))
		    (port (make-binary-socket-input/output-port sock "*child-sock*")))
	       (px.setsockopt/linger sock #t 1)
	       (unwind-protect
		   (begin
		     (px.connect sock sockaddr)
		     (let ((bv (get-bytevector-n port 3)))
;;;		       (check-pretty-print (list 'child-recv bv))
		       (assert (equal? '#vu8(1 2 3) bv)))
		     (put-bytevector port '#vu8(4 5 6))
		     (flush-output-port port))
		 (close-port port)))
	     (exit 0)))
	 (when (file-exists? pathname) (px.unlink pathname))
	 (px.fork parent child)
	 (when (file-exists? pathname) (px.unlink pathname))
	 #t))
    => '(#t (#vu8(4 5 6))))

  (check	;fork process, textual port input/output
      (with-result
       (let* ((pathname	(string-append (px.getenv "TMPDIR") "/proof"))
	      (sockaddr	(px.make-sockaddr_un pathname)))
	 (define (parent pid)
	   (let ((server-sock (px.socket PF_LOCAL SOCK_STREAM 0)))
	     (unwind-protect
		 (begin
		   (px.setsockopt/int server-sock SOL_SOCKET SO_REUSEADDR #t)
		   (px.bind   server-sock sockaddr)
		   (px.listen server-sock 2)
		   (let-values (((sock client-address) (px.accept server-sock)))
		     (px.setsockopt/linger sock #t 1)
		     (let ((port (make-textual-socket-input/output-port sock "*parent-sock*"
									(native-transcoder))))
		       (unwind-protect
			   (let ((S (make-string 4)))
			     (put-string port "ciao")
			     (flush-output-port port)
			     (get-string-n! port S 0 4)
			     (add-result S))
			 (close-port port)))))
	       (px.close server-sock)
	       (px.waitpid pid 0))))
	 (define (child)
	   (px.nanosleep 1 0) ;give parent the time to listen
	   (let* ((sock (px.socket PF_LOCAL SOCK_STREAM 0))
		  (port (make-textual-socket-input/output-port sock "*child-sock*"
							       (native-transcoder))))
	     (px.setsockopt/linger sock #t 1)
	     (unwind-protect
		 (begin
		   (px.connect sock sockaddr)
		   (assert (equal? "ciao" (get-string-n port 4)))
		   (put-string port "ciao")
		   (flush-output-port port))
	       (close-port port)))
	   (exit 0))
	 (when (file-exists? pathname)
	   (px.unlink pathname))
	 (px.fork parent child)
	 (when (file-exists? pathname)
	   (px.unlink pathname))
	 #t))
    => '(#t ("ciao")))

;;; --------------------------------------------------------------------
;;; PF_LOCAL SOCK_DGRAM

  (let* ((tmpdir	(px.getenv "TMPDIR"))
	 (pathname1	(string-append tmpdir "/proof-1"))
	 (pathname2	(string-append tmpdir "/proof-2")))
    (check	;fork process, raw bytevector input/output
	(with-result
	 (let ((sockaddr1 (px.make-sockaddr_un pathname1))
	       (sockaddr2 (px.make-sockaddr_un pathname2)))
;;;(check-pretty-print (sockaddr_un.pathname/string sockaddr1))
;;;(check-pretty-print (sockaddr_un.pathname/string sockaddr2))
	   (define (parent pid)
	     (let ((sock (px.socket PF_LOCAL SOCK_DGRAM 0)))
	       (unwind-protect
		   (let ((buffer (make-bytevector 3)))
		     (px.bind sock sockaddr1)
		     (px.nanosleep 1 0) ;give child some time
		     (px.sendto sock '#vu8(1 2 3) #f 0 sockaddr2)
		     (let-values (((len sockaddr) (px.recvfrom sock buffer #f 0)))
		       (add-result len)
		       (add-result (px.sockaddr_un.pathname/string sockaddr))
		       (add-result buffer)
		       #t))
		 (px.close sock)
		 (px.waitpid pid 0))))
	   (define (child)
	     (let ((sock (px.socket PF_LOCAL SOCK_DGRAM 0)))
	       (unwind-protect
		   (let ((buffer (make-bytevector 3)))
		     (px.bind sock sockaddr2)
		     (px.nanosleep 1 0) ;give parent some time
		     (let-values (((len sockaddr) (px.recvfrom sock buffer #f 0)))
		       (assert (equal? 3 len))
		       (assert (equal? pathname1 (px.sockaddr_un.pathname/string sockaddr)))
		       (assert (equal? buffer '#vu8(1 2 3)))
		       (px.sendto sock '#vu8(4 5 6) #f 0 sockaddr)
		       #t))
		 (px.close sock)))
	     (exit 0))
	   (when (file-exists? pathname1) (px.unlink pathname1))
	   (when (file-exists? pathname2) (px.unlink pathname2))
	   (px.fork parent child)
	   (when (file-exists? pathname1) (px.unlink pathname1))
	   (when (file-exists? pathname2) (px.unlink pathname2))
	   #t))
      => `(#t (3 ,pathname2 #vu8(4 5 6)))))

;;; --------------------------------------------------------------------
;;; PF_INET SOCK_STREAM

  (when (or #f run-inet-tests?)
    (check	;fork process, raw bytevector input/output, getpeername, getsockname
	(with-result
	 (let ((sockaddr (px.make-sockaddr_in '#vu8(127 0 0 1) 8080)))
	   (define (parent pid)
	     (let ((server-sock (px.socket PF_INET SOCK_STREAM 0)))
	       (unwind-protect
		   (begin
		     (px.setsockopt/int server-sock SOL_SOCKET SO_REUSEADDR #t)
		     (px.bind   server-sock sockaddr)
		     (px.listen server-sock 2)
		     (let-values (((sock client-address) (px.accept server-sock)))
		       (unwind-protect
			   (let ((bv (make-bytevector 3)))
			     (px.setsockopt/linger sock #t 1)
			     (let ((sockaddr (px.getsockname sock)))
			       (add-result (px.sockaddr_in.in_addr sockaddr))
			       (add-result (px.sockaddr_in.in_port sockaddr)))
			     (let ((sockaddr (px.getpeername sock)))
			       (add-result (px.sockaddr_in.in_addr sockaddr))
			       ;;nobody knows the port number
			       #;(add-result (px.sockaddr_in.in_port sockaddr)))
			     (px.write sock '#vu8(1 2 3))
			     (px.read  sock bv)
			     (add-result bv))
			 (px.close sock))))
		 (px.close server-sock)
		 (px.waitpid pid 0))))
	   (define (child)
	     (px.nanosleep 1 0) ;give parent the time to listen
	     (let ((sock (px.socket PF_INET SOCK_STREAM 0)))
	       (unwind-protect
		   (let ((bv (make-bytevector 3)))
		     (px.setsockopt/linger sock #t 1)
		     (px.connect sock sockaddr)
		     (let ((sockaddr (px.getpeername sock)))
		       (assert (equal? '#vu8(127 0 0 1) (px.sockaddr_in.in_addr sockaddr)))
		       (assert (equal? 8080 (px.sockaddr_in.in_port sockaddr))))
		     (let ((sockaddr (px.getsockname sock)))
		       (assert (equal? '#vu8(127 0 0 1) (px.sockaddr_in.in_addr sockaddr)))
		       (assert (fixnum? (px.sockaddr_in.in_port sockaddr))))
		     (px.read sock bv)
		     (assert (equal? bv '#vu8(1 2 3)))
		     (px.write sock bv))
		 (px.close sock)))
	     (exit 0))
	   (px.fork parent child)
	   #t))
      => '(#t (#vu8(127 0 0 1) 8080 #vu8(127 0 0 1) #vu8(1 2 3)))))

  (when (or #f run-inet-tests?)
    (check ;fork process, raw bytevector input/output, Out Of Band data
      (with-result
       (let ((sockaddr (px.make-sockaddr_in '#vu8(127 0 0 1) 8080)))
	 (define (parent pid)
	   (let ((server-sock (px.socket PF_INET SOCK_STREAM 0)))
	     (unwind-protect
		 (begin
		   (px.setsockopt/int server-sock SOL_SOCKET SO_REUSEADDR #t)
		   (px.bind   server-sock sockaddr)
		   (px.listen server-sock 2)
		   (let-values (((sock client-address) (px.accept server-sock)))
		     (unwind-protect
			 (let ((bv (make-bytevector 10)))
			   (px.setsockopt/linger sock #t 1)
			   (let loop ()
			     (let-values  (((rd wr ex) (px.select-fd sock 2 0)))
			       (cond (ex
				      (let ((len (px.recv sock bv #f MSG_OOB)))
					(add-result (list 'oob (subbytevector-u8 bv 0 len)))
					(loop)))
				     (rd
				      (let ((len (px.recv sock bv #f 0)))
					(when (positive? len)
					  (add-result (subbytevector-u8 bv 0 len))
					  (loop))))
				     (else (loop))))))
		       (px.close sock))))
	       (px.close server-sock)
	       (px.waitpid pid 0))))
	 (define (child)
	   (px.nanosleep 0 1000000) ;give parent the time to listen
	   (let ((sock (px.socket PF_INET SOCK_STREAM 0)))
	     (unwind-protect
		 (let ((bv (make-bytevector 3)))
		   (px.connect sock sockaddr)
		   (px.setsockopt/linger sock #t 2)
		   (px.send sock '#vu8(1 2 3) #f 0)
		   (px.nanosleep 0 1000000) ;give it some thrill
		   (px.send sock '#vu8(4 5 6) #f MSG_OOB)
		   (px.send sock '#vu8(7 8 9) #f 0))
	       (px.close sock)))
	   (exit 0))
	 (px.fork parent child)
	 #t))
      => '(#t (#vu8(1 2 3) (oob #vu8(6)) #vu8(4 5) #vu8(7 8 9)))))

;;; --------------------------------------------------------------------
;;; PF_INET6 SOCK_STREAM

  (when (or #f run-inet-tests?)
    (check	;fork process, raw bytevector input/output
	(with-result
	 (let ((sockaddr (px.make-sockaddr_in6 '#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 8080)))
	   (define (parent pid)
	     (let ((server-sock (px.socket PF_INET6 SOCK_STREAM 0)))
	       (unwind-protect
		   (begin
		     (px.setsockopt/int server-sock SOL_SOCKET SO_REUSEADDR #t)
		     (px.bind   server-sock sockaddr)
		     (px.listen server-sock 2)
		     (let-values (((sock client-address) (px.accept server-sock)))
		       (px.setsockopt/linger sock #t 1)
		       (unwind-protect
			   (let ((bv (make-bytevector 3)))
			     (px.write sock '#vu8(1 2 3))
			     (px.read  sock bv)
			     (add-result bv))
			 (px.close sock))))
		 (px.close server-sock)
		 (px.waitpid pid 0))))
	   (define (child)
	     (px.nanosleep 1 0) ;give parent the time to listen
	     (let ((sock (px.socket PF_INET6 SOCK_STREAM 0)))
	       (px.setsockopt/linger sock #t 1)
	       (unwind-protect
		   (let ((bv (make-bytevector 3)))
		     (px.connect sock sockaddr)
		     (px.read sock bv)
		     (assert (equal? bv '#vu8(1 2 3)))
		     (px.write sock bv))
		 (px.close sock)))
	     (exit 0))
	   (px.fork parent child)
	   #t))
      => '(#t (#vu8(1 2 3)))))

;;; --------------------------------------------------------------------
;;; PF_INET SOCK_DGRAM

  (when (or #f run-inet-tests?)
    (check	;fork process, raw bytevector input/output
	(with-result
	 (let ((sockaddr1 (px.make-sockaddr_in '#vu8(127 0 0 1) 8080))
	       (sockaddr2 (px.make-sockaddr_in '#vu8(127 0 0 1) 8081)))
	   (define (parent pid)
	     (let ((sock (px.socket PF_INET SOCK_DGRAM 0)))
	       (unwind-protect
		   (let ((buffer (make-bytevector 3)))
		     (px.bind sock sockaddr1)
		     (px.setsockopt/int sock SOL_SOCKET SO_REUSEADDR #t)
		     (px.nanosleep 0 1000000) ;give child some time
		     (px.sendto sock '#vu8(1 2 3) #f 0 sockaddr2)
		     (let-values (((len sockaddr) (px.recvfrom sock buffer #f 0)))
		       (add-result len)
		       (add-result (bytevector-copy buffer))
		       (add-result (px.sockaddr_in.in_addr sockaddr))
		       (add-result (px.sockaddr_in.in_port sockaddr))))
		 (px.close sock)
		 (px.waitpid pid 0))))
	   (define (child)
	     (let ((sock (px.socket PF_INET SOCK_DGRAM 0)))
	       (unwind-protect
		   (let ((buffer (make-bytevector 3)))
		     (px.bind sock sockaddr2)
		     (px.setsockopt/linger sock #t 1)
		     (px.nanosleep 0 1000000) ;give parent some time
		     (let-values (((len sockaddr) (px.recvfrom sock buffer #f 0)))
		       (assert (equal? 3 len))
		       (assert (equal? '#vu8(1 2 3) buffer))
		       (assert (equal? '#vu8(127 0 0 1) (px.sockaddr_in.in_addr sockaddr)))
		       (assert (equal? 8080 (px.sockaddr_in.in_port sockaddr)))
		       (px.sendto sock '#vu8(4 5 6) #f 0 sockaddr)))
		 (px.close sock)))
	     (exit 0))
	   (px.fork parent child)
	   #t))
      => '(#t (3 #vu8(4 5 6) #vu8(127 0 0 1) 8081))))

  (when (or #f run-inet-tests?)
    (check	;raw bytevector input/output
	(with-result
	 (let ((sockaddr1	(px.make-sockaddr_in '#vu8(127 0 0 1) 8080))
	       (sockaddr2	(px.make-sockaddr_in '#vu8(127 0 0 1) 8081))
	       (sock1		(px.socket PF_INET SOCK_DGRAM 0))
	       (sock2		(px.socket PF_INET SOCK_DGRAM 0)))
	   (unwind-protect
	       (let ((buffer (make-bytevector 3)))
		 (px.bind sock1 sockaddr1)
		 (px.bind sock2 sockaddr2)
		 (px.setsockopt/int sock1 SOL_SOCKET SO_REUSEADDR #t)
		 (px.setsockopt/int sock2 SOL_SOCKET SO_REUSEADDR #t)
		 (px.sendto sock1 '#vu8(1 2 3) #f 0 sockaddr2)
		 (let-values (((len sockaddr) (px.recvfrom sock2 buffer #f 0)))
		   (add-result len)
		   (add-result (bytevector-copy buffer))
		   (add-result (px.sockaddr_in.in_addr sockaddr))
		   (add-result (px.sockaddr_in.in_port sockaddr))
		   (px.sendto sock2 '#vu8(4 5 6) #f 0 sockaddr)
		   (let-values (((len sockaddr) (px.recvfrom sock1 buffer #f 0)))
		     (add-result len)
		     (add-result (bytevector-copy buffer))
		     (add-result (px.sockaddr_in.in_addr sockaddr))
		     (add-result (px.sockaddr_in.in_port sockaddr))))
		 #t)
	     (px.close sock1)
	     (px.close sock2))))
      => '(#t ( ;;
	       3 #vu8(1 2 3) #vu8(127 0 0 1) 8080
	       3 #vu8(4 5 6) #vu8(127 0 0 1) 8081))))

  #t)


;;;; done

(check-report)

;;; end of file
