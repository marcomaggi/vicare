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


#!r6rs
(library (vicare net channels)
  (export
    ;; initialisation and finalisation
    open-input-channel		open-output-channel
    open-input/output-channel	close-channel

    ;; configuration
    channel-set-maximum-message-size!
    channel-set-expiration-time!
    channel-set-message-terminator!

    ;; predicates and arguments validation
    channel?			channel.vicare-arguments-validation
    receiving-channel?		receiving-channel.vicare-arguments-validation
    sending-channel?		sending-channel.vicare-arguments-validation
    inactive-channel?		inactive-channel.vicare-arguments-validation
    input-channel?		input-channel.vicare-arguments-validation
    output-channel?		output-channel.vicare-arguments-validation
    input/output-channel?	input/output-channel.vicare-arguments-validation

    ;; message reception
    channel-reception-begin!	channel-reception-end!
    channel-get-message-portion!

    ;; message sending

    ;; condition objects
    &channel
    make-channel-condition
    condition-channel?
    condition-channel

    &delivery-timeout-expired
    make-delivery-timeout-expired-condition
    condition-delivery-timeout-expired?

    &maximum-message-size-exceeded
    make-maximum-message-size-exceeded-condition
    condition-maximum-message-size-exceeded?)
  (import (vicare)
    (vicare arguments validation)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    $))


;;;; data structures

(define-struct-extended channel
  (connect-in-port
		;An input  or input/output  binary port used  to receive
		;messages from a remote process.
   connect-ou-port
		;An  output or  input/output  binary port  used to  send
		;messages to a remote process.
   action
		;False or the symbol "recv" or the symbol "send".
   message-buffer
		;Null  or a  list of  bytevectors representing  the data
		;accumulated so far; last input first.
   expiration-time
		;A time object representing the  limit of time since the
		;Epoch  to complete  message delivery;  if the  allotted
		;time expires:  sending or  receiving this  message will
		;fail.
   message-size
		;A non-negative  exact integer representing  the current
		;message size.
   maximum-message-size
		;A non-negative exact integer representing the inclusive
		;maximum  message  size;  if  the size  of  the  message
		;exceeds this value: message delivery will fail.
   message-terminator
		;A   non-empty  bytevector   representing  the   message
		;terminator.
   ))


;;;; unsafe operations

(define ($channel-message-buffer-enqueue! chan bv)
  ($set-channel-message-buffer! chan (cons bv ($channel-message-buffer chan)))
  ($channel-message-increment-size! chan ($bytevector-length bv)))

(define ($channel-message-increment-size! chan delta-size)
  ($set-channel-message-size! chan (+ delta-size ($channel-message-size chan))))

(define ($time-expired? chan)
  (cond (($channel-expiration-time chan)
	 => (lambda (expiration-time)
	      (time<=? (current-time) expiration-time)))
	(else #f)))

(define ($maximum-size-exceeded? chan)
  (> ($channel-message-size chan)
     ($channel-maximum-message-size chan)))


;;;; initialisation and finalisation

(define-constant DEFAULT-TERMINATOR
  (string->ascii "\r\n\r\n"))

(define (open-input-channel port)
  (define who 'open-input-channel)
  (with-arguments-validation (who)
      ((binary-port	port)
       (input-port	port))
    (make-channel port #f
		  #f #;action
		  '() #;message-buffer
		  #f #;expiration-time
		  0 #;message-size
		  4096 #;maximum-message-size
		  DEFAULT-TERMINATOR #;message-terminator)))

(define (open-output-channel port)
  (define who 'open-output-channel)
  (with-arguments-validation (who)
      ((binary-port	port)
       (output-port	port))
    (make-channel #f port
		  #f #;action
		  '() #;message-buffer
		  #f #;expiration-time
		  0 #;message-size
		  4096 #;maximum-message-size
		  DEFAULT-TERMINATOR #;message-terminator)))

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
      (make-channel in-port ou-port
		    #f #;action
		    '() #;message-buffer
		    #f #;expiration-time
		    0 #;message-size
		    4096 #;maximum-message-size
		    DEFAULT-TERMINATOR #;message-terminator)))))

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
    ($set-channel-maximum-message-size! chan maximum-message-size)
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
    ($set-channel-expiration-time! chan expiration-time)
    (void)))

(define (channel-set-message-terminator! chan terminator)
  ;;TERMINATOR must  be a non-empty bytevector  representing the message
  ;;terminator.
  ;;
  (define who 'channel-set-expiration-time!)
  (with-arguments-validation (who)
      ((channel			chan)
       (non-empty-bytevector	terminator))
    ($set-channel-expiration-time! chan terminator)
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
  (and ($channel-action chan)
       #t))

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
  condition-channel?
  (channel	condition-channel))

(define-condition-type &delivery-timeout-expired
    &error
  make-delivery-timeout-expired-condition
  condition-delivery-timeout-expired?)

(define-condition-type &maximum-message-size-exceeded
    &error
  make-maximum-message-size-exceeded-condition
  condition-maximum-message-size-exceeded?)

;;; --------------------------------------------------------------------

(define (%error-message-expiration-time-expired who chan)
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

(define (channel-reception-begin! chan)
  ;;Configure a channel to start receiving a message; return unspecified
  ;;values.  CHAN  must be an  input or  input/output channel; it  is an
  ;;error if the channel is not inactive.
  ;;
  (define who 'channel-reception-begin!)
  (with-arguments-validation (who)
      ((inactive-channel	chan)
       (input-channel		chan))
    ($set-channel-action!          chan 'recv)
    ($set-channel-message-buffer!  chan '())
    ($set-channel-message-size!    chan 0)
    (void)))

(define (channel-reception-end! chan)
  ;;Finish receiving  a message and  return the accumulated octets  in a
  ;;bytevector.  It is an  error if the channel is not  in the course of
  ;;receiving a message.
  ;;
  ;;After this function  is applied to a channel: the  channel itself is
  ;;configured  as  inactive; so  it  is  available to  start  receiving
  ;;another message or to send a message.
  ;;
  (define who 'channel-reception-end!)
  (with-arguments-validation (who)
      ((receiving-channel	chan))
    (begin0
	($bytevector-reverse-and-concatenate ($channel-message-buffer chan)
					     ($channel-message-size   chan))
      ($set-channel-action!          chan #f)
      ($set-channel-message-buffer!  chan '())
      ($set-channel-message-size!    chan 0)
      ($set-channel-expiration-time! chan #f))))

(module (channel-get-message-portion!)

  (define who 'channel-get-message-portion!)

  (define (channel-get-message-portion! chan)
    ;;Receive a portion of input  message from the given channel; return
    ;;true if  the configured message  terminator was read,  else return
    ;;false.  It  is an  error if the  channel is not  in the  course of
    ;;receiving a message.
    ;;
    (with-arguments-validation (who)
	((receiving-channel	chan))
      (if ($time-expired? chan)
	  (%error-message-expiration-time-expired who chan)
	(%loop chan))))

  (define (%loop chan)
    (let ((bv (%read chan)))
      (if (eof-object? bv)
	  #f
	(begin
	  ($channel-message-buffer-enqueue! chan bv)
	  (cond (($maximum-size-exceeded? chan)
		 (%error-maximum-message-size-exceeded who chan))
		((%end-of-message? chan)
		 #t)
		(else
		 (%loop chan)))))))

  (define (%read chan)
    (get-bytevector-n ($channel-connect-in-port chan) 4096))

  (define (%end-of-message? chan)
    ;;Compare the message terminator with the bytevectors accumulated in
    ;;the  buffer  of CHAN.   If  the  tail  of  the buffer  equals  the
    ;;terminator return true, else return false.
    ;;
    (let ((T ($channel-message-terminator chan)))
      (let loop ((T.idx ($fxsub1 ($bytevector-length T)))
		 (bufs  ($channel-message-buffer chan)))
	(cond (($fxzero? T.idx)
	       #t)
	      ((null? bufs)
	       #f)
	      ((let* ((next-bv     ($car bufs))
		      (next-bv.end ($fxsub1 ($bytevector-length next-bv))))
		 ($compare-bytevector-tails T T.idx next-bv next-bv.end))
	       => (lambda (T.idx)
		    (loop T.idx ($cdr bufs))))))))

  (define ($compare-bytevector-tails A A.idx B B.idx)
    ;;Recurse  comparing the  bytevector  A, starting  from index  A.idx
    ;;inclusive,  to  the  bytevector   B,  starting  from  index  B.idx
    ;;inclusive.  If:
    ;;
    ;;* All the octets are equal up to (zero? A.idx) included: return 0.
    ;;
    ;;* All the  octets are equal up to (zero?   B.idx) included: return
    ;;  the value of A.idx.
    ;;
    ;;* Octets  before the first index  in A or B  are different: return
    ;;  false.
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

  #| end of module |# )


;;;; sending messages

(define (channel-sending-begin! chan)
  ;;Configure a channel  to start sending a  message; return unspecified
  ;;values.  CHAN  must be an output  or input/output channel; it  is an
  ;;error if the channel is not inactive.
  ;;
  (define who 'channel-sending-begin!)
  (with-arguments-validation (who)
      ((inactive-channel	chan)
       (output-channel		chan))
    ($set-channel-action!          chan 'send)
    ($set-channel-message-buffer!  chan '())
    ($set-channel-message-size!    chan 0)
    (void)))

(define (channel-sending-end! chan)
  ;;Finish sending a message by flushing the connect port and return the
  ;;total number of octets  sent.  It is an error if  the channel is not
  ;;in the course of sending a message.
  ;;
  ;;After this function  is applied to a channel: the  channel itself is
  ;;configured  as  inactive; so  it  is  available to  start  receiving
  ;;another message or to send a message.
  ;;
  (define who 'channel-sending-end!)
  (with-arguments-validation (who)
      ((sending-channel	chan))
    (begin0
	($channel-message-size chan)
      (flush-output-port ($channel-connect-ou-port chan))
      ($set-channel-action!          chan #f)
      ($set-channel-message-buffer!  chan '())
      ($set-channel-message-size!    chan 0)
      ($set-channel-expiration-time! chan #f))))

(define (channel-set-message-portion! chan portion)
  ;;Send a portion  of output message through the  given channel; return
  ;;unspecified values.   It is an  error if the  channel is not  in the
  ;;course of sending a message.
  ;;
  ;;PORTION must be a bytevector representing the message portion.
  ;;
  ;;This function does not flush the connection port.
  ;;
  (define who 'channel-set-message-portion!)
  (with-arguments-validation (who)
      ((sending-channel	chan)
       (bytevector	portion))
    ($channel-message-increment-size! chan ($bytevector-length portion))
    (cond (($time-expired? chan)
	   (%error-message-expiration-time-expired who chan))
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
