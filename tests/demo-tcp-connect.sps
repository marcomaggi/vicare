;;; demo-tcp-connect.sps --
;;
;;Show how to use the convenience function TCP-CONNECT.
;;

#!r6rs
(import (except (vicare)
		log)
  (prefix (vicare posix) px.))

(define (log template . args)
  (apply fprintf (current-error-port)
	 template args))

(define (send line port)
  (log "sending: ~s\n" line)
  (display line port))

(define (recv in-port)
  (let-values (((str-port getter) (open-string-output-port)))
    (let next ((line (read-line in-port)))
      (if (or (eof-object? line)
	      (string=? line "\r")
	      (string=? line ".\r"))
	  (getter)
	(begin
	  (log "received: ~s\n" line)
	  (display line str-port)
	  (next (read-line in-port)))))))

(define p
  (px.tcp-connect "google.it" "http"))

(send "GET / HTTP/1.0\r\n\r\n" p)
(recv p)
(recv p)

(close-port p)

;;; end of file
;;Local Variables:
;;coding: utf-8
;;End:
