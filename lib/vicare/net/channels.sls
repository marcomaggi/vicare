;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: communicating with remote processes
;;;Date: Fri Apr  5, 2013
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
(library (vicare net channels)
  (export
    ;; record type
    channel

    ;; initialisation and finalisation
    open-input-channel		open-output-channel
    open-input/output-channel	close-channel

    ;; configuration
    channel-set-maximum-message-size!
    channel-set-expiration-time!
    channel-set-message-terminators!

    ;; predicates and arguments validation
    channel?
    channel.vicare-arguments-validation
    false-or-channel.vicare-arguments-validation
    receiving-channel?		receiving-channel.vicare-arguments-validation
    sending-channel?		sending-channel.vicare-arguments-validation
    inactive-channel?		inactive-channel.vicare-arguments-validation
    input-channel?		input-channel.vicare-arguments-validation
    output-channel?		output-channel.vicare-arguments-validation
    input/output-channel?	input/output-channel.vicare-arguments-validation

    ;; message reception
    channel-recv-begin!		channel-recv-end!
    channel-recv-message-portion!

    ;; message sending
    channel-send-begin!		channel-send-end!
    channel-send-message-portion!

    ;; condition objects
    &channel
    make-channel-condition
    channel-condition?
    condition-channel

    &delivery-timeout-expired
    make-delivery-timeout-expired-condition
    delivery-timeout-expired-condition?

    &maximum-message-size-exceeded
    make-maximum-message-size-exceeded-condition
    maximum-message-size-exceeded-condition?)
  (import (vicare)
    (vicare arguments validation)
    (vicare language-extensions syntaxes)
    (vicare arguments validation)
    (prefix (vicare unsafe operations)
	    $))


;;;; data structures

(define-record-type-extended channel
  (nongenerative vicare:net:channels:channel)
  (fields (immutable connect-in-port)
		;An input  or input/output  binary port used  to receive
		;messages from a remote process.
	  (immutable connect-ou-port)
		;An  output or  input/output  binary port  used to  send
		;messages to a remote process.
	  (mutable action)
		;False or the symbol "recv" or the symbol "send".
	  (mutable expiration-time)
		;A time object representing the  limit of time since the
		;Epoch  to complete  message delivery;  if the  allotted
		;time expires:  sending or  receiving this  message will
		;fail.
	  (mutable message-buffer)
		;Null  or a  list of  bytevectors representing  the data
		;accumulated so far; last input first.
	  (mutable message-size)
		;A non-negative  exact integer representing  the current
		;message size.
	  (mutable maximum-message-size)
		;A non-negative exact integer representing the inclusive
		;maximum  message  size;  if  the size  of  the  message
		;exceeds this value: message delivery will fail.
	  (mutable message-terminators)
		;A non-empty list  of non-empty bytevectors representing
		;possible message terminators.
	  (mutable message-terminated?)
		;A  boolean,  true  if  while receiving  a  message  the
		;terminator has already been read.
	  )
  (protocol
   (lambda (maker)
     (lambda (in-port ou-port)
       (define who 'channel-constructor)
       (define-argument-validation (one-port who in-port ou-port)
	 (or in-port ou-port)
	 (assertion-violation who "both port arguments are false" in-port ou-port))
       (with-arguments-validation (who)
	   ((binary-port/false	in-port)
	    (binary-port/false	ou-port)
	    (input-port/false	in-port)
	    (output-port/false	ou-port)
	    (one-port		in-port ou-port))
	 (maker in-port ou-port
		#f #;action #f #;expiration-time
		'() #;message-buffer 0 #;message-size 4096 #;maximum-message-size
		DEFAULT-TERMINATORS #;message-terminatosr #f #;message-terminated?
		))))))

(define-constant DEFAULT-TERMINATORS
  '(#ve(ascii "\r\n\r\n") #ve(ascii "\n\n")))


;;;; unsafe operations

(define ($channel-message-buffer-push! chan bv)
  ($channel-message-buffer-set! chan (cons bv ($channel-message-buffer chan)))
  ($channel-message-increment-size! chan ($bytevector-length bv)))

(define ($channel-message-increment-size! chan delta-size)
  ($channel-message-size-set! chan (+ delta-size ($channel-message-size chan))))

(define ($delivery-timeout-expired? chan)
  (cond (($channel-expiration-time chan)
	 => (lambda (expiration-time)
	      (time<=? expiration-time (current-time))))
	(else #f)))

(define ($maximum-size-exceeded? chan)
  (> ($channel-message-size chan)
     ($channel-maximum-message-size chan)))


;;;; initialisation and finalisation

(define (open-input-channel port)
  (define who 'open-input-channel)
  (with-arguments-validation (who)
      ((binary-port	port)
       (input-port	port))
    (make-channel port #f)))

(define (open-output-channel port)
  (define who 'open-output-channel)
  (with-arguments-validation (who)
      ((binary-port	port)
       (output-port	port))
    (make-channel #f port)))

(define open-input/output-channel
  (case-lambda
   ((port)
    (open-input/output-channel port port))
   ((in-port ou-port)
    (define who 'open-input/output-channel)
    (with-arguments-validation (who)
	((binary-port	in-port)
	 (binary-port	ou-port)
	 (input-port	in-port)
	 (output-port	ou-port))
      (make-channel in-port ou-port)))))

(define (close-channel chan)
  ;;Finalise a  channel closing its connection  port; return unspecified
  ;;values.  A pending message delivery is aborted.
  ;;
  (define who 'close-channel)
  (define (%close port)
    (and port (close-port port)))
  (with-arguments-validation (who)
      ((channel		chan))
    (%close ($channel-connect-in-port chan))
    (%close ($channel-connect-ou-port chan))
    (struct-reset chan)
    (void)))


;;;; configuration

(define (channel-set-maximum-message-size! chan maximum-message-size)
  ;;MAXIMUM-MESSAGE-SIZE   must   be   a  non-negative   exact   integer
  ;;representing the inclusive maximum message  size; if the size of the
  ;;message exceeds this value: message delivery will fail.
  ;;
  (define who 'channel-set-maximum-message-size!)
  (with-arguments-validation (who)
      ((channel			chan)
       (positive-exact-integer	maximum-message-size))
    ($channel-maximum-message-size-set! chan maximum-message-size)
    (void)))

(define (channel-set-expiration-time! chan expiration-time)
  ;;EXPIRATION-TIME  must be  false or  a time  object representing  the
  ;;limit of time  since the Epoch to complete message  delivery; if the
  ;;allotted time expires: message delivery will fail.
  ;;
  (define who 'channel-set-expiration-time!)
  (with-arguments-validation (who)
      ((channel		chan)
       (time/false	expiration-time))
    ($channel-expiration-time-set! chan expiration-time)
    (void)))

(define (channel-set-message-terminators! chan terminators)
  ;;TERMINATORS  must  be  a  non-empty list  of  non-empty  bytevectors
  ;;representing possible message terminators.
  ;;
  (define who 'channel-set-expiration-time!)
  (define-argument-validation (terminators who obj)
    (and (not (null? terminators))
	 (list? terminators)
	 (for-all (lambda (item)
		    (and (bytevector? item)
			 (not ($fxzero? ($bytevector-length item)))))
	   terminators))
    (assertion-violation who
      "expected non-empty list of non-empty bytevectors as argument" obj))
  (with-arguments-validation (who)
      ((channel		chan)
       (terminators	terminators))
    ($channel-expiration-time-set! chan terminators)
    (void)))


;;;; predicates and arguments validation: receiving messages

(define (receiving-channel? chan)
  ;;Return #t  if CHAN  is in  the course of  receiving a  message, else
  ;;return #f.  It is an error if CHAN is not an instance of CHANNEL.
  ;;
  (define who 'receiving-channel?)
  (with-arguments-validation (who)
      ((channel	chan))
    ($receiving-channel? chan)))

(define ($receiving-channel? chan)
  ;;Unsafe function returning #t if CHAN is in the course of receiving a
  ;;message, else return #f.
  ;;
  (eq? 'recv ($channel-action chan)))

;;; --------------------------------------------------------------------

(define-argument-validation (receiving-channel who obj)
  ;;Succeed if OBJ is an instance of  CHANNEL and it is in the course of
  ;;receiving a message.
  ;;
  (and (channel? obj)
       ($receiving-channel? obj))
  (assertion-violation who
    "expected channel in the course of receving a message" obj))

(define-argument-validation (not-receiving-channel who chan)
  ;;Succeed if  CHAN is an  instance of CHANNEL and  it is *not*  in the
  ;;course of receiving a message.
  ;;
  (and (channel? chan)
       (not ($receiving-channel? chan)))
  (assertion-violation who
    "expected channel not in the course of receving a message" chan))


;;;; predicates and arguments validation: sending messages

(define (sending-channel? chan)
  ;;Return #t if CHAN is in the course of sending a message, else return
  ;;#f.  It is an error if CHAN is not an instance of CHANNEL.
  ;;
  (define who 'sending-channel?)
  (with-arguments-validation (who)
      ((channel	chan))
    ($sending-channel? chan)))

(define ($sending-channel? chan)
  ;;Unsafe function returning  #t if CHAN is in the  course of sending a
  ;;message, else return #f.
  ;;
  (eq? 'send ($channel-action chan)))

;;; --------------------------------------------------------------------

(define-argument-validation (sending-channel who obj)
  ;;Succeed if OBJ is an instance of  CHANNEL and it is in the course of
  ;;sending a message.
  ;;
  (and (channel? obj)
       ($sending-channel? obj))
  (assertion-violation who
    "expected channel in the course of sending a message" obj))

(define-argument-validation (not-sending-channel who chan)
  ;;Succeed if  CHAN is an  instance of CHANNEL and  it is *not*  in the
  ;;course of sending a message.
  ;;
  (and (channel? chan)
       (not ($sending-channel? chan)))
  (assertion-violation who
    "expected channel not in the course of sending a message" chan))


;;;; predicates and arguments validation: inactive channel

(define (inactive-channel? chan)
  ;;Return #t if CHAN is neither  in the course of sending nor receiving
  ;;a  message, else  return #f.   It  is an  error  if CHAN  is not  an
  ;;instance of CHANNEL.
  ;;
  (define who 'inactive-channel?)
  (with-arguments-validation (who)
      ((channel	chan))
    ($inactive-channel? chan)))

(define ($inactive-channel? chan)
  ;;Unsafe function  returning #t if  CHAN is  neither in the  course of
  ;;sending nor receiving a message, else return #f.
  ;;
  (not ($channel-action chan)))

;;; --------------------------------------------------------------------

(define-argument-validation (inactive-channel who obj)
  ;;Succeed if OBJ  is an instance of  CHANNEL and it is  neither in the
  ;;course of sending nor receiving a message.
  ;;
  (and (channel? obj)
       ($inactive-channel? obj))
  (assertion-violation who
    "expected channel in the course of inactive a message" obj))

(define-argument-validation (not-inactive-channel who chan)
  ;;Succeed if CHAN  is an instance of CHANNEL and  it is either sending
  ;;or receiving a message.
  ;;
  (and (channel? chan)
       (not ($inactive-channel? chan)))
  (assertion-violation who
    "expected channel not in the course of inactive a message" chan))


;;;; predicates and arguments validation: input channel

(define (input-channel? chan)
  ;;Return #t if  CHAN is an input or input/output  channel, else return
  ;;#f.  It is an error if CHAN is not an instance of CHANNEL.
  ;;
  (define who 'input-channel?)
  (with-arguments-validation (who)
      ((channel	chan))
    ($input-channel? chan)))

(define ($input-channel? chan)
  ;;Unsafe function  returning #t  if CHAN is  an input  or input/output
  ;;channel, else return #f.
  ;;
  (and ($channel-connect-in-port chan) #t))

;;; --------------------------------------------------------------------

(define-argument-validation (input-channel who obj)
  ;;Succeed if  OBJ is  an instance  of CHANNEL  and it  is an  input or
  ;;input/output channel.
  ;;
  (and (channel? obj)
       ($input-channel? obj))
  (assertion-violation who
    "expected input or input/output channel as argument" obj))


;;;; predicates and arguments validation: output channel

(define (output-channel? chan)
  ;;Return #t if CHAN is an  output or input/output channel, else return
  ;;#f.  It is an error if CHAN is not an instance of CHANNEL.
  ;;
  (define who 'output-channel?)
  (with-arguments-validation (who)
      ((channel	chan))
    ($output-channel? chan)))

(define ($output-channel? chan)
  ;;Unsafe function  returning #t if  CHAN is an output  or input/output
  ;;channel, else return #f.
  ;;
  (and ($channel-connect-ou-port chan) #t))

;;; --------------------------------------------------------------------

(define-argument-validation (output-channel who obj)
  ;;Succeed if  OBJ is  an instance of  CHANNEL and it  is an  output or
  ;;input/output channel.
  ;;
  (and (channel? obj)
       ($output-channel? obj))
  (assertion-violation who
    "expected output or input/output channel as argument" obj))


;;;; predicates and arguments validation: input/output channel

(define (input/output-channel? chan)
  ;;Return #t if CHAN is an input/output channel, else return #f.  It is
  ;;an error if CHAN is not an instance of CHANNEL.
  ;;
  (define who 'input/output-channel?)
  (with-arguments-validation (who)
      ((channel	chan))
    ($input/output-channel? chan)))

(define ($input/output-channel? chan)
  ;;Unsafe function  returning #t  if CHAN  is an  input/output channel,
  ;;else return #f.
  ;;
  (and ($channel-connect-in-port chan)
       ($channel-connect-ou-port chan)
       #t))

;;; --------------------------------------------------------------------

(define-argument-validation (input/output-channel who obj)
  ;;Succeed if OBJ  is an instance of CHANNEL and  it is an input/output
  ;;channel.
  ;;
  (and (channel? obj)
       ($input/output-channel? obj))
  (assertion-violation who "expected input/output channel as argument" obj))


;;;; condition objects and exception raising

(define-condition-type &channel
    &condition
  make-channel-condition
  channel-condition?
  (channel	condition-channel))

(define-condition-type &delivery-timeout-expired
    &error
  make-delivery-timeout-expired-condition
  delivery-timeout-expired-condition?)

(define-condition-type &maximum-message-size-exceeded
    &error
  make-maximum-message-size-exceeded-condition
  maximum-message-size-exceeded-condition?)

;;; --------------------------------------------------------------------

(define (%error-message-delivery-timeout-expired who chan)
  ;;Raise a  non-continuable exception  representing the  error: message
  ;;message delivery  timeout expired.  The raised  condition object has
  ;;components: &who, &message, &channel, &timeout-expired.
  ;;
  (raise
   (condition (make-channel-condition chan)
	      (make-delivery-timeout-expired-condition)
	      (make-who-condition who)
	      (make-message-condition "message reception timeout expired"))))

(define (%error-maximum-message-size-exceeded who chan)
  ;;Raise a  non-continuable exception  representing the  error: maximum
  ;;message size exceeded.  The  raised condition object has components:
  ;;&who, &message, &channel, &maximum-message-size-exceeded.
  ;;
  (raise
   (condition (make-channel-condition chan)
	      (make-maximum-message-size-exceeded-condition)
	      (make-who-condition who)
	      (make-message-condition "message reception timeout expired"))))


;;;; receiving messages

(define (channel-recv-begin! chan)
  ;;Configure a channel to start receiving a message; return unspecified
  ;;values.  CHAN  must be an  input or  input/output channel; it  is an
  ;;error if the channel is not inactive.
  ;;
  (define who 'channel-recv-begin!)
  (with-arguments-validation (who)
      ((inactive-channel	chan)
       (input-channel		chan))
    ($channel-action-set!              chan 'recv)
    ($channel-message-buffer-set!      chan '())
    ($channel-message-size-set!        chan 0)
    ($channel-message-terminated?-set! chan #f)
    (void)))

(define (channel-recv-end! chan)
  ;;Finish receiving  a message and  return the accumulated octets  in a
  ;;bytevector.  It is an  error if the channel is not  in the course of
  ;;receiving a message.
  ;;
  ;;After this function  is applied to a channel: the  channel itself is
  ;;configured  as  inactive; so  it  is  available to  start  receiving
  ;;another message or to send a message.
  ;;
  (let-values (((reverse-buffers total-size)
		(channel-recv-end!/rbl chan)))
    ($bytevector-reverse-and-concatenate reverse-buffers total-size)))

(define (channel-recv-end!/rbl chan)
  ;;Finish  receiving a  message  and return  the 2  values:  a list  of
  ;;bytevectors  representing the  data buffers  accumulated in  reverse
  ;;order, an exact integer representing the  total data size.  It is an
  ;;error if the channel is not in the course of receiving a message.
  ;;
  ;;After this function  is applied to a channel: the  channel itself is
  ;;configured  as  inactive; so  it  is  available to  start  receiving
  ;;another message or to send a message.
  ;;
  (define who 'channel-recv-end!)
  (with-arguments-validation (who)
      ((receiving-channel	chan))
    (begin0
	(values ($channel-message-buffer chan)
		($channel-message-size   chan))
      ($channel-action-set!          chan #f)
      ($channel-message-buffer-set!  chan '())
      ($channel-message-size-set!    chan 0)
      ($channel-message-terminated?-set! chan #f))))

(module (channel-recv-message-portion!)

  (define who 'channel-recv-message-portion!)

  (define (channel-recv-message-portion! chan)
    ;;Receive a portion of input  message from the given channel; return
    ;;true if  the configured message  terminator was read,  else return
    ;;false.  It  is an  error if the  channel is not  in the  course of
    ;;receiving a message.
    ;;
    ;;If  the  message delivery  timeout  is  expired or  expires  while
    ;;receiving  data:  raise an  exception.   If  the accumulated  data
    ;;exceeds the maximum message size: raise an exception.
    ;;
    ;;If  CHAN is  already set  to the  "message terminated"  status: do
    ;;nothing and return true.  If a message terminator is received: set
    ;;CHAN to "message terminated" status.
    ;;
    (with-arguments-validation (who)
	((receiving-channel	chan))
      (cond (($delivery-timeout-expired? chan)
	     (%error-message-delivery-timeout-expired who chan))
	    (($channel-message-terminated? chan)
	     #t)
	    (else
	     (%recv-loop chan)))))

  (define (%recv-loop chan)
    ;;Loop receiving  bytevectors from the  input port, until EOF  or an
    ;;empty read; we must not accumulate empty bytevectors.
    ;;
    ;;Return true if the end of message  is read, which case CHAN is set
    ;;to "message terminated" status; return false if no more input data
    ;;is available for now.  Raise  an exception if: the maximum message
    ;;size is exceeded, the message delivery timeout expires.
    ;;
    (let ((bv (%read-bytevector chan)))
      (if (or (eof-object? bv)
	      ($fxzero? ($bytevector-length bv)))
	  (begin
	    #f)
	(begin
	  ($channel-message-buffer-push! chan bv)
	  (cond (($maximum-size-exceeded? chan)
		 (%error-maximum-message-size-exceeded who chan))
		(($delivery-timeout-expired? chan)
		 (%error-message-delivery-timeout-expired who chan))
		((%received-message-terminator? chan)
		 ($channel-message-terminated?-set! chan #t)
		 #t)
		(else
		 (%recv-loop chan)))))))

  (define (%read-bytevector chan)
    (get-bytevector-n ($channel-connect-in-port chan) 4096))

  (module (%received-message-terminator?)

    (define (%received-message-terminator? chan)
      ;;Compare  all  the  message   terminators  with  the  bytevectors
      ;;accumulated in  the buffer of CHAN.   If the tail of  the buffer
      ;;equals one of the terminators: return true, else return false.
      ;;
      (let ((terminators ($channel-message-terminators chan))
	    (buffers     ($channel-message-buffer chan)))
	(find (lambda (terminator)
		($terminated-octets-stream? buffers terminator))
	  terminators)))

    (define ($terminated-octets-stream? reverse-stream terminator)
      ;;Compare a terminator  with the tail of an octets  stream; if the
      ;;stream  is terminated  return #t,  else return  #f.  This  is an
      ;;unsafe  function: it  assumes  the arguments  have been  already
      ;;validated.
      ;;
      ;;TERMINATOR  must  be  a non-empty  bytevector  representing  the
      ;;stream  terminator; the  last octet  in TERMINATOR  is the  last
      ;;octet in a properly terminated stream.
      ;;
      ;;REVERSE-SEQUENCE must be null or a list of non-empty bytevectors
      ;;representing the stream of octects in bytevector-reversed order;
      ;;as  if the  stream of  octets  has been  accumulated (=  CONSed)
      ;;bytevector by bytevector:
      ;;
      ;;* The first  item of REVERSE-SEQUENCE is the  last bytevector in
      ;;  the  stream, the  last item of  REVERSE-SEQUENCE is  the first
      ;;  bytevector in the stream.
      ;;
      ;;*  Every bytevector  in REVERSE-SEQUENCE  represents a  chunk of
      ;;  stream: the  first octet in the bytevector is  the first octet
      ;;  in  the chunk, the  last octet in  the bytevector is  the last
      ;;  octet in the chunk.
      ;;
      (define ($bytevector-last-index bv)
	($fxsub1 ($bytevector-length bv)))
      (let loop ((terminator.idx  ($bytevector-last-index terminator))
		 (buffers         reverse-stream))
	(cond (($fx= -1 terminator.idx)
	       #t)
	      ((null? buffers)
	       #f)
	      ((let* ((buf     ($car buffers))
		      (buf.idx ($bytevector-last-index buf)))
		 ($compare-bytevector-tails terminator terminator.idx buf buf.idx))
	       => (lambda (terminator.idx)
		    (loop ($fxsub1 terminator.idx) ($cdr buffers))))
	      (else #f))))

    (define ($compare-bytevector-tails A A.idx B B.idx)
      ;;Recursive  function.  Compare  the bytevector  A, starting  from
      ;;index A.idx inclusive, to the  bytevector B, starting from index
      ;;B.idx inclusive.  If:
      ;;
      ;;* All the octets are equal  up to (zero? A.idx) included: return
      ;;  0.  An example of this case is a call with arguments:
      ;;
      ;;     A =       #vu8(3 4 5)	A.idx = 2
      ;;     B = #vu8(0 1 2 3 4 5)	B.idx = 5
      ;;
      ;;  another example of this case:
      ;;
      ;;     A = #vu8(0 1 2 3 4 5)	A.idx = 5
      ;;     B = #vu8(0 1 2 3 4 5)	B.idx = 5
      ;;
      ;;* All the octets are equal up to (zero?  B.idx) included: return
      ;;  the value  of A.idx referencing the last compared  octet in A.
      ;;  An example of this case is a call with arguments:
      ;;
      ;;     A = #vu8(0 1 2 3 4 5)	A.idx = 5
      ;;     B =       #vu8(3 4 5)	B.idx = 2
      ;;
      ;;  the returned value is: A.idx == 3.
      ;;
      ;;* Octects  having (positive?  A.idx) and  (positive?  B.idx) are
      ;;  different: return false.
      ;;
      (and ($fx= ($bytevector-u8-ref A A.idx)
		 ($bytevector-u8-ref B B.idx))
	   (cond (($fxzero? A.idx)
		  0)
		 (($fxzero? B.idx)
		  A.idx)
		 (else
		  ($compare-bytevector-tails A ($fxsub1 A.idx)
					     B ($fxsub1 B.idx))))))

    #| end of module: %received-message-terminator? |# )

  #| end of module: channel-recv-message-portion! |# )


;;;; sending messages

(define (channel-send-begin! chan)
  ;;Configure a channel  to start sending a  message; return unspecified
  ;;values.  CHAN  must be an output  or input/output channel; it  is an
  ;;error if the channel is not inactive.
  ;;
  (define who 'channel-send-begin!)
  (with-arguments-validation (who)
      ((inactive-channel	chan)
       (output-channel		chan))
    ($channel-action-set!          chan 'send)
    ($channel-message-buffer-set!  chan '())
    ($channel-message-size-set!    chan 0)
    (void)))

(define (channel-send-end! chan)
  ;;Finish sending a message by flushing the connect port and return the
  ;;total number of octets  sent.  It is an error if  the channel is not
  ;;in the course of sending a message.
  ;;
  ;;After this function  is applied to a channel: the  channel itself is
  ;;configured  as  inactive; so  it  is  available to  start  receiving
  ;;another message or to send a message.
  ;;
  (define who 'channel-send-end!)
  (with-arguments-validation (who)
      ((sending-channel	chan))
    (begin0
	($channel-message-size chan)
      (flush-output-port ($channel-connect-ou-port chan))
      ($channel-action-set!          chan #f)
      ($channel-message-buffer-set!  chan '())
      ($channel-message-size-set!    chan 0))))

(define (channel-send-message-portion! chan portion)
  ;;Send a portion  of output message through the  given channel; return
  ;;unspecified values.   It is an  error if the  channel is not  in the
  ;;course of sending a message.
  ;;
  ;;PORTION must be a bytevector representing the message portion.
  ;;
  ;;This function does not flush the connection port.
  ;;
  (define who 'channel-send-message-portion!)
  (with-arguments-validation (who)
      ((sending-channel	chan)
       (bytevector	portion))
    ($channel-message-increment-size! chan ($bytevector-length portion))
    (cond (($delivery-timeout-expired? chan)
	   (%error-message-delivery-timeout-expired who chan))
	  (($maximum-size-exceeded? chan)
	   (%error-maximum-message-size-exceeded who chan))
	  (else
	   (put-bytevector ($channel-connect-ou-port chan) portion)))))


(define ($bytevector-reverse-and-concatenate list-of-bytevectors full-length)
  ;;Reverse  LIST-OF-BYTEVECTORS and  concatenate its  bytevector items;
  ;;return  the  result.   The  resulting bytevector  must  have  length
  ;;FULL-LENGTH.  Assume the arguments have been already validated.
  ;;
  ;;IMPLEMENTATION RESTRICTION The bytevectors must have a fixnum length
  ;;and the whole bytevector must at maximum have a fixnum length.
  ;;
  (let loop ((dst.bv	($make-bytevector full-length))
	     (dst.start	full-length)
	     (bvs	list-of-bytevectors))
    (if (null? bvs)
	dst.bv
      (let* ((src.bv    ($car bvs))
	     (src.len   ($bytevector-length src.bv))
	     (dst.start ($fx- dst.start src.len)))
	($bytevector-copy!/count src.bv 0 dst.bv dst.start src.len)
	(loop dst.bv dst.start (cdr bvs))))))


;;;; done

)

;;; end of file
