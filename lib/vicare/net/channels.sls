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
    open-channel		close-channel

    ;; configuration
    channel-set-maximum-message-size!
    channel-set-expiration-time!

    ;; predicates and arguments validation
    channel?			channel.vicare-arguments-validation
    receiving-channel?		receiving-channel.vicare-arguments-validation
    sending-channel?		sending-channel.vicare-arguments-validation
    inactive-channel?		inactive-channel.vicare-arguments-validation

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
  (connect-port
		;An input/output  binary port  used to send  and receive
		;messages to and from a remote process.
   action
		;False or the symbol "recv" or the symbol "send".
   buffer-port
		;False or  a bytevector  output port used  to accumulate
		;input for a message.
   buffer-extract
		;False or a  thunk to be called to  retrieve the message
		;accumulated in the buffer output port.
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
   ))


;;;; unsafe operations

(define ($increment-channel-message-size! chan delta-size)
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

(define (open-channel port)
  (define who 'open-channel)
  (with-arguments-validation (who)
      ((binary-port	port))
    (make-channel port
		  #f #;action
		  #f #;buffer-port
		  #f #;buffer-extract
		  #f #;expiration-time
		  0 #;message-size
		  4096 #;maximum-message-size)))

(define (close-channel chan)
  ;;Finalise a  channel closing its connection  port; return unspecified
  ;;values.  A pending message delivery is aborted.
  ;;
  (define who 'close-channel)
  (with-arguments-validation (who)
      ((channel		chan))
    (close-port ($channel-connect-port chan))
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
    ($set-channel-expiration-time! chan expiration-time)))


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

(define (channel-reception-begin! chan expiration-time)
  ;;Configure a channel to start receiving a message; return unspecified
  ;;values.  It  is an error  if the  connection port registered  in the
  ;;channel is  neither input nor input/output.   It is an error  if the
  ;;channel is not inactive.
  ;;
  (define who 'channel-start-get-message)
  (with-arguments-validation (who)
      ((inactive-channel	chan))
    (let ((in-port ($channel-connect-port chan)))
      (with-arguments-validation (who)
	  ((input-port	in-port))
	(receive (port extract)
	    (open-bytevector-output-port)
	  ($set-channel-action!          chan 'recv)
	  ($set-channel-buffer-port!     chan port)
	  ($set-channel-buffer-extract!  chan extract)
	  ($set-channel-message-size!    chan 0)
	  (void))))))

(define (channel-reception-end! chan)
  ;;Finish receiving  a message and  return the accumulated octets  in a
  ;;bytevector.  It is an  error if the channel is not  in the course of
  ;;receiving a message.
  ;;
  ;;After this function  is applied to a channel: the  channel itself is
  ;;configured  as  inactive; so  it  is  available to  start  receiving
  ;;another message or to send a message.
  ;;
  (define who 'channel-finish-receiving)
  (with-arguments-validation (who)
      ((receiving-channel	chan))
    (begin0
	(($channel-buffer-extract chan))
      ($set-channel-action!          chan #f)
      ($set-channel-buffer-port!     chan #f)
      ($set-channel-buffer-extract!  chan #f)
      ($set-channel-message-size!    chan 0))))

(define (channel-get-message-portion! chan)
  ;;Receive a  portion of input  message from the given  channel; return
  ;;true  if the  configured message  terminator was  read, else  return
  ;;false.   It is  an error  if the  channel is  not in  the course  of
  ;;receiving a message.
  ;;
  (define who 'channel-get-message-portion)
  (with-arguments-validation (who)
      ((receiving-channel	chan))
    (when ($time-expired? chan)
      (%error-message-expiration-time-expired who chan))
    (let ((in-port  ($channel-connect-port chan))
	  (buf-port ($channel-buffer-port chan)))

      (define (%read)
	(get-bytevector-n in-port 4096))

      (define (%end-of-message? chan)
	#f)

      (let loop ((buffer (%read)))
	(if (eof-object? buffer)
	    #f
	  (begin
	    (put-bytevector buf-port buffer)
	    ($increment-channel-message-size! chan ($bytevector-length buffer))
	    (cond (($maximum-size-exceeded? chan)
		   (%error-maximum-message-size-exceeded who chan))
		  ((%end-of-message? chan)
		   #t)
		  (else
		   (loop (%read))))))))))


;;;; done

)

;;; end of file
