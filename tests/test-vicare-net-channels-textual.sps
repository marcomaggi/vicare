;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for textual net channels library
;;;Date: Fri Apr  5, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (test-vicare-net-channels-textual)
  (options typed-language)
  (import (vicare)
    (vicare net channels)
    (vicare language-extensions custom-ports)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: net channels library, textual channels\n")


;;;; helpers

(define in-port (open-string-input-port "123"))

(define ou-port
  (receive (port extract)
      (open-string-output-port)
    port))

(define io-port
  (receive (port1 port2)
      (open-textual-input/output-port-pair)
    port1))


(parametrise ((check-test-name	'object))

;;; input channel

  (check
      (let ((chan (new <textual-input-only-channel> in-port)))
        (list (is-a? chan <channel>)
	      (.inactive?  chan)
	      (.sending?   chan)
	      (.receiving? chan)))
    => '(#t #t #f #f))

   (check
       (let ((chan (new <textual-input-only-channel> in-port)))
	 (void-object? (delete chan)))
    => #t)

   (check
       (let ((chan (new <textual-input-only-channel> in-port)))
	 (.recv-begin! chan)
	 (list (.inactive? chan)
	       (.sending? chan)
	       (.receiving? chan)))
     => '(#f #f #t))

   (check
       (let ((chan (new <textual-input-only-channel> in-port)))
	 (.recv-begin! chan)
	 (.recv-abort! chan)
	 (list (.inactive? chan)
	       (.sending? chan)
	       (.receiving? chan)))
     => '(#t #f #f))

;;; --------------------------------------------------------------------
;;; output channel

  (check
      (receive (port extract)
	  (open-string-output-port)
	(let ((chan (new <textual-output-only-channel> port)))
	  (list (is-a? chan <channel>)
		(.inactive? chan)
		(.sending? chan)
		(.receiving? chan))))
    => '(#t #t #f #f))

  (check
      (receive (port extract)
	  (open-string-output-port)
	(let ((chan (new <textual-output-only-channel> port)))
	  (void-object? (delete chan))))
    => #t)

   (check
       (let ((chan (new <textual-output-only-channel> ou-port)))
	 (.send-begin! chan)
	 (list (.inactive? chan)
	       (.sending? chan)
	       (.receiving? chan)))
     => '(#f #t #f))

   (check
       (let ((chan (new <textual-output-only-channel> ou-port)))
	 (.send-begin! chan)
	 (.send-end! chan)
	 (list (.inactive? chan)
	       (.sending? chan)
	       (.receiving? chan)))
     => '(#t #f #f))

  #t)


(parametrise ((check-test-name	'config))

  (check
      (let ((chan (new <textual-input-only-channel> in-port)))
        (receive ()
	    (.maximum-message-size chan 1000)
	  #t))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((chan	(new <textual-input-only-channel> in-port))
	    (ET		(.+ (current-time) (new <time> 10 0))))
	(.expiration-time chan ET)
	(equal? ET (.expiration-time chan)))
    => #t)

  (check
      (let ((chan	(new <textual-input-only-channel> in-port))
	    (ET		(faraway-time)))
	(.expiration-time chan ET)
	(equal? ET (.expiration-time chan)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((chan (new <textual-input-only-channel> in-port)))
	(.message-terminators chan '#("123"))
	(.message-terminators chan))
    => '#("123"))

  (void))


(parametrise ((check-test-name	'message-portions))

  ;;Exchange    messages   between    coroutines   using    a   two    instances   of
  ;;"<textual-input/output-channel>".
  ;;
  (check
      (with-result
	(internal-body
	  (define (send {chan <textual-input/output-channel>} chunks)
	    ;;For debugging  purposes we want  to flush  the output port  after every
	    ;;portion is sent.
	    ;;
	    (.send-begin! chan)
	    (for-each-in-order
		(lambda (portion)
		  (.send-message-portion! chan portion)
		  (.flush chan)
		  (yield))
	      chunks)
	    (.send-end! chan))

	  (define (recv {chan <textual-input/output-channel>})
	    (.recv-begin! chan)
	    (let loop ()
	      (let ((rv (.recv-message-portion! chan)))
		(cond ((not rv)
		       (yield)
		       (loop))
		      ((eof-object? rv)
		       (eof-object))
		      (else
		       (.recv-end! chan))))))

	  (define (master-log obj)
	    (add-result (list 'master-recv obj)))

	  (define (slave-log obj)
	    (add-result (list 'slave-recv obj)))

	  (receive (master.port slave.port)
	      (open-textual-input/output-port-pair)
	    (coroutine ;master
		(lambda ()
		  (let ((chan (new <textual-input/output-channel> master.port master.port))
			(log  master-log))
		    (.message-terminators chan '#("\r\n\r\n"))
		    (send chan '("hel" "lo sla" "ve\r\n\r\n"))
		    (log (recv chan))
		    (send chan '("som" "e dat" "a\r" "\n"
				 "som" "e other dat" "a\r" "\n" "\r" "\n"))
		    (log (recv chan))
		    (send chan '("quit\r\n\r\n"))
		    (delete chan)
		    (close-port master.port))))
	    (coroutine ;slave
		(lambda ()
		  (let ((chan (new <textual-input/output-channel> slave.port slave.port))
			(log  slave-log))
		    (.message-terminators chan '#("\r\n\r\n"))
		    (log (recv chan))
		    (send chan '("hel" "lo mas" "ter\r\n\r\n"))
		    (log (recv chan))
		    (send chan '("OK" "\r\n" "\r\n"))
		    (log (recv chan))
		    (delete chan)
		    (close-port slave.port))))
	    (finish-coroutines))))
    => `(((slave-recv "hello slave\r\n\r\n")
	  (master-recv "hello master\r\n\r\n")
	  (slave-recv "some data\r\nsome other data\r\n\r\n")
	  (master-recv "OK\r\n\r\n")
	  (slave-recv "quit\r\n\r\n")
	  )))

;;; --------------------------------------------------------------------

  ;;Exchange    messages   between    coroutines   using    a   two    instances   of
  ;;"<textual-input/output-channel>" accessed using interfaces.
  ;;
  (check
      (with-result
	(internal-body

	  (define (send {chan <<textual-output-channel>>} chunks)
	    ;;For debugging  purposes we want  to flush  the output port  after every
	    ;;portion is sent.
	    ;;
	    (.send-begin! chan)
	    (for-each-in-order
		(lambda (portion)
		  (.send-message-portion! chan portion)
		  (.flush chan)
		  (yield))
	      chunks)
	    (.send-end! chan))

	  (define (recv {chan <<textual-input-channel>>})
	    (.recv-begin! chan)
	    (let loop ()
	      (let ((rv (.recv-message-portion! chan)))
		(cond ((not rv)
		       (yield)
		       (loop))
		      ((eof-object? rv)
		       (eof-object))
		      (else
		       (.recv-end! chan))))))

	  (define (master-log obj)
	    (add-result (list 'master-recv obj)))

	  (define (slave-log obj)
	    (add-result (list 'slave-recv obj)))

	  (receive (master.port slave.port)
	      (open-textual-input/output-port-pair)
	    (coroutine ;master
		(lambda ()
		  (let ((chan (new <textual-input/output-channel> master.port master.port))
			(log  master-log))
		    (.message-terminators chan '#("\r\n\r\n"))
		    (send chan '("hel" "lo sla" "ve\r\n\r\n"))
		    (log (recv chan))
		    (send chan '("som" "e dat" "a\r" "\n"
				 "som" "e other dat" "a\r" "\n" "\r" "\n"))
		    (log (recv chan))
		    (send chan '("quit\r\n\r\n"))
		    (delete chan)
		    (close-port master.port))))
	    (coroutine ;slave
		(lambda ()
		  (let ((chan (new <textual-input/output-channel> slave.port slave.port))
			(log  slave-log))
		    (.message-terminators chan '#("\r\n\r\n"))
		    (log (recv chan))
		    (send chan '("hel" "lo mas" "ter\r\n\r\n"))
		    (log (recv chan))
		    (send chan '("OK" "\r\n" "\r\n"))
		    (log (recv chan))
		    (delete chan)
		    (close-port slave.port))))
	    (finish-coroutines))))
    => `(((slave-recv "hello slave\r\n\r\n")
	  (master-recv "hello master\r\n\r\n")
	  (slave-recv "some data\r\nsome other data\r\n\r\n")
	  (master-recv "OK\r\n\r\n")
	  (slave-recv "quit\r\n\r\n")
	  )))

;;; --------------------------------------------------------------------

  ;;Exchange     messages      between     coroutines     using      instances     of
  ;;"<textual-input-only-channel>" and "<textual-output-only-channel>".
  ;;
  (check
      (with-result
	(internal-body
	  (define (send {chan <textual-output-only-channel>} chunks)
	    ;;For debugging  purposes we want  to flush  the output port  after every
	    ;;portion is sent.
	    ;;
	    (.send-begin! chan)
	    (for-each-in-order
		(lambda (portion)
		  (.send-message-portion! chan portion)
		  (.flush chan)
		  (yield))
	      chunks)
	    (.send-end! chan))

	  (define (recv {chan <textual-input-only-channel>})
	    (.recv-begin! chan)
	    (let loop ()
	      (let ((rv (.recv-message-portion! chan)))
		(cond ((not rv)
		       (yield)
		       (loop))
		      ((eof-object? rv)
		       (eof-object))
		      (else
		       (.recv-end! chan))))))

	  (with-compensations
	    (let-values
		(((master.iport slave.oport) (open-textual-input-port-pair))
		 ((slave.iport  master.oport) (open-textual-input-port-pair)))
	      (push-compensation
	       (close-port master.iport)
	       (close-port master.oport)
	       (close-port slave.iport)
	       (close-port slave.oport))
	      (coroutine ;master
		  (lambda ()
		    (define (master-log obj)
		      (add-result (list 'master-recv obj)))
		    (with-compensations
		      (letrec
			  ((ichan (compensate
				      (new <textual-input-only-channel>  master.iport)
				    (with
				     (delete ichan))))
			   (ochan (compensate
				      (new <textual-output-only-channel> master.oport)
				    (with
				     (delete ochan)))))
			(send ochan '("hel" "lo sla" "ve\r\n\r\n"))
			(master-log (recv ichan))
			(send ochan '("som" "e dat" "a\r" "\n"
				      "som" "e other dat" "a\r" "\n" "\r" "\n"))
			(master-log (recv ichan))
			(send ochan '("quit\r\n\r\n"))
			))))
	      (coroutine ;slave
		  (lambda ()
		    (define (slave-log obj)
		      (add-result (list 'slave-recv obj)))
		    (with-compensations
		      (letrec
			  ((ichan (compensate
				      (new <textual-input-only-channel>  slave.iport)
				    (with
				     (delete ichan))))
			   (ochan (compensate
				      (new <textual-output-only-channel> slave.oport)
				    (with
				     (delete ochan)))))
			(slave-log (recv ichan))
			(send ochan '("hel" "lo mas" "ter\r\n\r\n"))
			(slave-log (recv ichan))
			(send ochan '("OK" "\r\n" "\r\n"))
			(slave-log (recv ichan))
			))))
	      (finish-coroutines)))))
    => `(((slave-recv "hello slave\r\n\r\n")
	  (master-recv "hello master\r\n\r\n")
	  (slave-recv "some data\r\nsome other data\r\n\r\n")
	  (master-recv "OK\r\n\r\n")
	  (slave-recv "quit\r\n\r\n")
	  )))

;;; --------------------------------------------------------------------

  ;;Exchange     messages      between     coroutines     using      instances     of
  ;;"<textual-input-only-channel>" and "<textual-output-only-channel>" accessed through
  ;;the interface types.
  ;;
  (check
      (with-result
	(internal-body
	  (define (send {chan <<textual-output-channel>>} chunks)
	    ;;For debugging  purposes we want  to flush  the output port  after every
	    ;;portion is sent.
	    ;;
	    (.send-begin! chan)
	    (for-each-in-order
		(lambda (portion)
		  (.send-message-portion! chan portion)
		  (.flush chan)
		  (yield))
	      chunks)
	    (.send-end! chan))

	  (define (recv {chan <<textual-input-channel>>})
	    (.recv-begin! chan)
	    (let loop ()
	      (let ((rv (.recv-message-portion! chan)))
		(cond ((not rv)
		       (yield)
		       (loop))
		      ((eof-object? rv)
		       (eof-object))
		      (else
		       (.recv-end! chan))))))

	  (with-compensations
	    (let-values
		(((master.iport slave.oport) (open-textual-input-port-pair))
		 ((slave.iport  master.oport) (open-textual-input-port-pair)))
	      (push-compensation
	       (close-port master.iport)
	       (close-port master.oport)
	       (close-port slave.iport)
	       (close-port slave.oport))
	      (coroutine ;master
		  (lambda ()
		    (define (master-log obj)
		      (add-result (list 'master-recv obj)))
		    (with-compensations
		      (letrec
			  ((ichan (compensate
				      (new <textual-input-only-channel>  master.iport)
				    (with
				     (delete ichan))))
			   (ochan (compensate
				      (new <textual-output-only-channel> master.oport)
				    (with
				     (delete ochan)))))
			(send ochan '("hel" "lo sla" "ve\r\n\r\n"))
			(master-log (recv ichan))
			(send ochan '("som" "e dat" "a\r" "\n"
				      "som" "e other dat" "a\r" "\n" "\r" "\n"))
			(master-log (recv ichan))
			(send ochan '("quit\r\n\r\n"))
			))))
	      (coroutine ;slave
		  (lambda ()
		    (define (slave-log obj)
		      (add-result (list 'slave-recv obj)))
		    (with-compensations
		      (letrec
			  ((ichan (compensate
				      (new <textual-input-only-channel>  slave.iport)
				    (with
				     (delete ichan))))
			   (ochan (compensate
				      (new <textual-output-only-channel> slave.oport)
				    (with
				     (delete ochan)))))
			(slave-log (recv ichan))
			(send ochan '("hel" "lo mas" "ter\r\n\r\n"))
			(slave-log (recv ichan))
			(send ochan '("OK" "\r\n" "\r\n"))
			(slave-log (recv ichan))
			))))
	      (finish-coroutines)))))
    => `(((slave-recv "hello slave\r\n\r\n")
	  (master-recv "hello master\r\n\r\n")
	  (slave-recv "some data\r\nsome other data\r\n\r\n")
	  (master-recv "OK\r\n\r\n")
	  (slave-recv "quit\r\n\r\n")
	  )))

  #t)


(parametrise ((check-test-name	'sending))

  (check	;max message size error
      (let ((chan (new <textual-output-only-channel> ou-port)))
	(.maximum-message-size chan 2)
	(.send-begin! chan)
	(try
	    (.send-message-portion! chan "123")
	  (catch E
	    ((&maximum-message-size-exceeded)
	     #t)
	    (else E))))
    => #t)

;;; --------------------------------------------------------------------

  (check	;timeout expired error
      (let ((chan (new <textual-output-only-channel> ou-port)))
        (.expiration-time chan (current-time))
	(.send-begin! chan)
	(try
	    (.send-message-portion! chan "123")
	  (catch E
	    ((&delivery-timeout-expired)
	     #t)
	    (else E))))
    => #t)

  #t)


(parametrise ((check-test-name	'receiving))

  (check	;max message size error
      (let ((chan (new <textual-input-only-channel> (open-string-input-port "123"))))
	(.maximum-message-size chan 2)
	(.recv-begin! chan)
	(try
	    (.recv-message-portion! chan)
	  (catch E
	    ((&maximum-message-size-exceeded)
	     #t)
	    (else E))))
    => #t)

;;; --------------------------------------------------------------------

  (check	;timeout expired error
      (let ((chan (new <textual-input-only-channel> in-port)))
	(.expiration-time chan (current-time))
	(.recv-begin! chan)
	(try
	    (.recv-message-portion! chan)
	  (catch E
	    ((&delivery-timeout-expired)
	     #t)
	    (else E))))
    => #t)

  #!void)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
