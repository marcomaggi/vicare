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
(library (vicare net channels (0 4 2016 08 28))
  (options typed-language)
  (export
    ;; record type
    <channel>
    <binary-channel>			<textual-channel>
    <binary-input-channel>		<textual-input-channel>
    <binary-output-channel>		<textual-output-channel>
    <binary-input/output-channel>	<textual-input/output-channel>

    ;; initialisation and finalisation
    open-binary-input-channel		open-textual-input-channel
    open-binary-output-channel		open-textual-output-channel
    open-binary-input/output-channel	open-textual-input/output-channel
    close-channel
    channel-abort!

    ;; configuration
    channel-set-maximum-message-size!
    channel-set-expiration-time!
    channel-set-message-terminators!
    channel-set-maximum-message-portion-size!

    ;; getters
    channel-connect-in-port
    channel-connect-ou-port

    ;; predicates and arguments validation
    receiving-channel?
    sending-channel?
    inactive-channel?

    ;; message reception
    channel-recv-begin!		channel-recv-end!
    channel-recv-message-portion!
    channel-recv-full-message

    ;; message sending
    channel-send-begin!		channel-send-end!
    channel-send-message-portion!
    channel-send-full-message

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
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $chars)
    (vicare system $strings)
    (vicare system $bytevectors))


;;;; helpers

(define-constant DEFAULT-BINARY-TERMINATORS
  '(#ve(ascii "\r\n\r\n") #ve(ascii "\r\n")))

(define-constant DEFAULT-TEXTUAL-TERMINATORS
  '("\r\n\r\n" "\r\n"))


;;;; data structures

(define-record-type <channel>
  (nongenerative vicare:net:channels:<channel>)
  (fields (immutable {connect-in-port <recv-port>})
		;An input or input/output binary port used to receive messages from a
		;remote process.
	  (immutable {connect-ou-port <send-port>})
		;An output  or input/output binary  port used  to send messages  to a
		;remote process.
	  (mutable {action (enumeration none send recv)})
		;A symbol representing the current action for this channel.
	  (mutable {expiration-time <time>})
		;A   "<time>"  object   (as  defined   by  the   library  "(vicare)")
		;representing the limit  of time since the Epoch  to complete message
		;delivery; if  the allotted time  expires: sending or  receiving this
		;message will fail.
	  (mutable {message-buffer (list-of <nebytevector>)})
		;Null or a  list of bytevectors representing the  data accumulated so
		;far; last input first.
	  (mutable {message-size <non-negative-exact-integer>})
		;A non-negative exact integer representing the current message size.
	  (mutable {maximum-message-size <non-negative-exact-integer>})
		;A  non-negative exact  integer  representing  the inclusive  maximum
		;message size; if the size of the message exceeds this value: message
		;delivery will fail.
	  (mutable {message-terminators <terminators>})
		;A  non-empty list  of  non-empty  bytevectors representing  possible
		;message terminators.
	  (mutable {message-terminated? <boolean>})
		;A  boolean, true  if while  receiving a  message the  terminator has
		;already been read.
	  (mutable {maximum-message-portion-size <positive-fixnum>})
		;A positive fixnum  representing the maximum number  of units (bytes,
		;characters) read at each "message portion receive" operation.
	  #| end of FIELDS |# )
  (protocol
    (lambda (make-channel)
      (define (make-channel {in-port <input-port>} {ou-port <output-port>} default-terminators max-portion-size)
	(let ((expiration-time (new <time> (greatest-fixnum) 0)))
	  (make-channel in-port ou-port
			#f		  ;action
			expiration-time	  ;expiration-time
			'()		  ;message-buffer
			0		  ;message-size
			4096		  ;maximum-message-size
			default-terminators ;message-terminators
			#f		    ;message-terminated?
			max-portion-size    ;maximum-message-portion-size
			)))
      make-channel))
  (constructor-signature
    (lambda (<input-port> <output-port> <top> <positive-fixnum>) => (<channel>)))
  #| end of DEFINE-RECORD-TYPE |# )

(define-type <binary-terminators>
  (nelist-of <nebytevector>))

(define-type <textual-terminators>
  (nelist-of <nestring>))

(define-type <terminators>
  (or <binary-terminators>
      <textual-terminators>))

;;; --------------------------------------------------------------------

(define-record-type <binary-channel>
  (nongenerative vicare:net:channels:<binary-channel>)
  (parent <channel>)
  (protocol
    (lambda (make-channel)
      (define ({make-binary-channel <binary-channel>}
	       {in-port (or <false> <binary-input-port>)}
	       {ou-port (or <false> <binary-output-port>)})
	((make-channel in-port ou-port DEFAULT-BINARY-TERMINATORS 4096)))
      make-binary-channel))
  (constructor-signature
    (lambda ((or <false> <binary-input-port>) (or <false> <binary-output-port>)) => (<binary-channel>)))
  #| end of DEFINE-RECORD-TYPE |# )

(define-record-type <textual-channel>
  (nongenerative vicare:net:channels:<textual-channel>)
  (parent <channel>)
  (protocol
    (lambda (make-channel)
      (define ({make-textual-channel <textual-channel>}
	       {in-port (or <false> <textual-input-port>)}
	       {ou-port (or <false> <textual-output-port>)})
	((make-channel in-port ou-port DEFAULT-TEXTUAL-TERMINATORS 4096)))
      make-textual-channel))
  (constructor-signature
    (lambda ((or <false> <textual-input-port>) (or <false> <textual-output-port>)) => (<textual-channel>)))
  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define-record-type <binary-input-channel>
  (nongenerative vicare:net:channels:<binary-input-channel>)
  (parent <binary-channel>)
  (protocol
    (lambda (make-binary-channel)
      (define ({make-binary-input-channel <binary-input-channel>} {port <binary-input-port>})
	((make-binary-channel port #f)))
      make-binary-input-channel))
  (constructor-signature
    (lambda (<binary-input-port>) => (<binary-input-channel>))))

(define-record-type <binary-output-channel>
  (nongenerative vicare:net:channels:<binary-output-channel>)
  (parent <binary-channel>)
  (protocol
    (lambda (make-binary-channel)
      (define ({make-binary-output-channel <binary-output-channel>} {port <binary-output-port>})
	((make-binary-channel #f port)))
      make-binary-output-channel))
  (constructor-signature
    (lambda (<binary-output-port>) => (<binary-output-channel>))))

(define-record-type <binary-input/output-channel>
  (nongenerative vicare:net:channels:<binary-input/output-channel>)
  (parent <binary-channel>)
  (protocol
    (lambda (make-binary-channel)
      (define ({make-binary-input/output-channel <binary-input/output-channel>} {port <binary-input/output-port>})
	((make-binary-channel port port)))
      make-binary-input/output-channel))
  (constructor-signature
    (lambda (<binary-input/output-port>) => (<binary-input/output-channel>))))

;;; --------------------------------------------------------------------

(define-record-type <textual-input-channel>
  (nongenerative vicare:net:channels:<textual-input-channel>)
  (parent <textual-channel>)
  (protocol
    (lambda (make-textual-channel)
      (define ({make-textual-input-channel <textual-input-channel>} {port <textual-input-port>})
	((make-textual-channel port #f)))
      make-textual-input-channel))
  (constructor-signature
    (lambda (<textual-input-port>) => (<textual-input-channel>))))

(define-record-type <textual-output-channel>
  (nongenerative vicare:net:channels:<textual-output-channel>)
  (parent <textual-channel>)
  (protocol
    (lambda (make-textual-channel)
      (define ({make-textual-output-channel <textual-output-channel>} {port <textual-output-port>})
	((make-textual-channel #f port)))
      make-textual-output-channel))
  (constructor-signature
    (lambda (<textual-output-port>) => (<textual-output-channel>))))

(define-record-type <textual-input/output-channel>
  (nongenerative vicare:net:channels:<textual-input/output-channel>)
  (parent <textual-channel>)
  (protocol
    (lambda (make-textual-channel)
      (define ({make-textual-input/output-channel <textual-input/output-channel>} {port <textual-input/output-port>})
	((make-textual-channel port port)))
      make-textual-input/output-channel))
  (constructor-signature
    (lambda (<textual-input/output-port>) => (<textual-input/output-channel>))))


;;;; type definitions: interfaces

(define-interface-type <binary-recv-channel>
  (nongenerative vicare:net:channels:<binary-recv-channel>)

  (method-prototype channel-recv-begin!
    (lambda () => (<void>)))
		;Configure a channel to start receiving a message; return unspecified
		;values.  It is an error if the channel is not inactive.

  (method-prototype channel-recv-end!
    (lambda () => (<bytevector>)))
		;Finish receiving  a message and  return the accumulated octets  in a
		;bytevector.  It is an  error if the channel is not  in the course of
		;receiving a message.
		;
		;After this function  is applied to a channel: the  channel itself is
		;configured  as  inactive; so  it  is  available to  start  receiving
		;another message or to send a message.

  (method-prototype recv-end!/rbl
    (lambda () => ((list-of <nebytevector>) <positive-exact-integer>)))
		;Finish  receiving a  message  and return  the 2  values:  a list  of
		;bytevectors  representing the  data buffers  accumulated in  reverse
		;order; a  positive exact integer  representing the total  data size.
		;It is an  error if the channel  is not in the course  of receiving a
		;message.
		;
		;After this function  is applied to a channel: the  channel itself is
		;configured  as  inactive; so  it  is  available to  start  receiving
		;another message or to send a message.

  (method-prototype recv-message-portion!
    (lambda () => ((or <boolean> <eof>))))
		;Receive a portion of input message from the given channel.  It is an
		;error if the channel is not in the course of receiving a message.
		;
		;* Return  true if a configured  message terminator is read  from the
		;input port or if the channel already read a terminator in a previous
		;operation.   If  a  message  terminator is  received:  set  CHAN  to
		;"message terminated" status.
		;
		;* Return the EOF object if EOF  is read from the input port before a
		;message terminator.
		;
		;* Return false  if neither a message terminator nor  EOF is read; in
		;this  case we  need to  call this  function again  later to  receive
		;further message portions.
		;
		;*  If the  message  delivery  timeout is  expired  or expires  while
		;receiving data: raise an exception.
		;
		;* If the accumulated data exceeds the maximum message size: raise an
		;exception.

  (method-prototype recv-full-message
    (lambda () => ()))
		;Receive  a full  message.  It  is  an error  if the  channel is  not
		;inactive.
		;
		;Read a full  message from the channel and  return: eof, would-block,
		;or a bytevector holding the message contents.

  #| end of DEFINE-INTERFACE-TYPE |# )

;;; --------------------------------------------------------------------

(define-interface-type <textual-recv-channel>
  (nongenerative vicare:net:channels:<textual-recv-channel>)
  #| end of DEFINE-INTERFACE-TYPE |# )


;;;; unsafe operations

(define ($channel-message-buffer-push! chan data)
  ($channel-message-buffer-set! chan (cons data ($channel-message-buffer chan)))
  ($channel-message-increment-size! chan (if (binary-channel? chan)
					     ($bytevector-length data)
					   ($string-length data))))

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

(define ({open-binary-input-channel <binary-channel>} {port <binary-input-port>})
  (new <binary-channel> port #f))

(define ({open-binary-output-channel <binary-channel>} {port <binary-output-port>})
  (new <binary-channel> #f port))

(define/overload ({open-binary-input/output-channel <binary-channel>} {port <binary-input/output-port>})
  (new <binary-channel> port port))

(define/overload ({open-binary-input/output-channel <binary-channel>} {in-port <binary-input-port>} {ou-port <binary-ouptut-port>})
  (new <binary-channel> in-port ou-port))

;;; --------------------------------------------------------------------

(define ({open-textual-input-channel <textual-channel>} {port <textual-input-port>})
  (new <textual-channel> port #f))

(define ({open-textual-output-channel <textual-channel>} {port <textual-output-port>})
  (new <textual-channel> #f port))

(define/overload ({open-textual-input/output-channel <textual-channel>} {port <textual-input/output-port>})
  (new <textual-channel> port port))

(define/overload ({open-textual-input/output-channel <textual-channel>} {in-port <textual-input-port>} {ou-port <textual-ouptut-port>})
  (new <textual-channel> in-port ou-port))

;;; --------------------------------------------------------------------

(define ({close-channel <void>} {chan <channel>})
  ;;Finalise a  channel closing  its connection port;  return unspecified  values.  A
  ;;pending message delivery is aborted.
  ;;
  (define (%close port)
    (when port
      (close-port port)))
  (%close ($channel-connect-in-port chan))
  (%close ($channel-connect-ou-port chan))
  (record-reset chan)
  (void))

(define ({channel-abort! <void>} {chan <channel>})
  ;;Abort the current operation and reset the channel to inactive; return unspecified
  ;;values.
  ;;
  ($channel-action-set!              chan 'none)
  ($channel-message-buffer-set!      chan '())
  ($channel-message-size-set!        chan 0)
  ($channel-message-terminated?-set! chan #f)
  (void))


;;;; configuration

(define ({channel-set-maximum-message-size! <void>} {chan <channel>} {maximum-message-size <positive-exact-integer>})
  ;;MAXIMUM-MESSAGE-SIZE must be a positive  exact integer representing the inclusive
  ;;maximum message size in octets or characters;  if the size of the message exceeds
  ;;this value: message delivery will fail.
  ;;
  ($channel-maximum-message-size-set! chan maximum-message-size)
  (void))

(define ({channel-set-expiration-time! <void>} {chan <channel>} {expiration-time (or <time> <false>)})
  ;;EXPIRATION-TIME must  be false or  a time object  representing the limit  of time
  ;;since  the Epoch  to complete  message delivery;  if the  allotted time  expires:
  ;;message delivery will fail.
  ;;
  ($channel-expiration-time-set! chan expiration-time)
  (void))

(define/overload ({channel-set-message-terminators! <void>} {chan <binary-channel>} {terminators <binary-terminators>})
  ;;TERMINATORS  must be  a non-empty  list of  non-empty bytevectors  representing
  ;;possible message terminators.
  ;;
  ($channel-message-terminators-set! chan terminators))

(define/overload ({channel-set-message-terminators! <void>} {chan <textual-channel>} {terminators <textual-terminators>})
  ;;TERMINATORS must be a non-empty list of non-empty strings representing possible
  ;;message terminators.
  ;;
  ($channel-message-terminators-set! chan terminators))

(define ({channel-set-maximum-message-portion-size! <void>} {chan <channel>} {max-portion-size <positive-fixnum>})
  ;;MAX-PORTION-SIZE must  be a  positive fixnum  representing the  inclusive maximum
  ;;size, in octets or characters, requested when receiving message portions.
  ;;
  ($channel-maximum-message-portion-size-set! chan max-portion-size)
  (void))


;;;; predicates and arguments validation: receiving messages

(define ({receiving-channel? <boolean>} {chan <channel>})
  ;;Return #t if CHAN is in the course of receiving a message, else return #f.
  ;;
  (eq? 'recv (.action chan)))

;;; --------------------------------------------------------------------

(define (assert-receiving-channel {who <symbol>} {chan <channel>})
  ;;Succeed  if CHAN  is  an instance  of  "<channel>" and  it is  in  the course  of
  ;;receiving a message.
  ;;
  (unless (receiving-channel? chan)
    (assertion-violation who
      "expected channel in the course of receving a message as argument" chan)))

(define (assert-not-receiving-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN  is an instance of "<channel>"  and it is *not* in  the course of
  ;;receiving a message.
  ;;
  (when (receiving-channel? chan)
    (assertion-violation who
      "expected channel not in the course of receving a message as argument" chan)))


;;;; predicates and arguments validation: sending messages

(define ({sending-channel? <boolean>} {chan <channel>})
  ;;Return #t if CHAN is in the course of sending a message, else return #f.
  ;;
  (eq? 'send (.action chan)))

;;; --------------------------------------------------------------------

(define (assert-sending-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN is an instance of "<channel>"  and it is in the course of sending
  ;;a message.
  ;;
  (unless (sending-channel? chan)
    (assertion-violation who
      "expected channel in the course of sending a message as argument" chan)))

(define (asert-not-sending-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN  is an instance of "<channel>"  and it is *not* in  the course of
  ;;sending a message.
  ;;
  (when (sending-channel? chan)
    (assertion-violation who
      "expected channel not in the course of sending a message as argument" chan)))


;;;; predicates and arguments validation: inactive channel

(define ({inactive-channel? <boolean>} {chan <channel>})
  ;;Return #t if  CHAN is neither in  the course of sending nor  receiving a message,
  ;;else return #f.
  ;;
  (eq? 'none (.action chan)))

;;; --------------------------------------------------------------------

(define (assert-inactive-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN is an instance of "<channel>"  and it is neither in the course of
  ;;sending nor receiving a message.
  ;;
  (unless (inactive-channel? chan)
    (assertion-violation who "expected inactive channel as argument" chan)))

(define (assert-not-inactive-channel {who <symbol>} {chan <channel>})
  ;;Succeed  if CHAN  is an  instance  of "<channel>"  and  it is  either sending  or
  ;;receiving a message.
  ;;
  (when (inactive-channel? chan)
    (assertion-violation who "expected inactive channel as argument" chan)))


;;;; condition objects and exception raising

(define-condition-type &channel
    &condition
  make-channel-condition
  channel-condition?
  ({channel <channel>}	condition-channel))

(define-condition-type &delivery-timeout-expired
    &error
  make-delivery-timeout-expired-condition
  delivery-timeout-expired-condition?)

(define-condition-type &maximum-message-size-exceeded
    &error
  make-maximum-message-size-exceeded-condition
  maximum-message-size-exceeded-condition?)

;;; --------------------------------------------------------------------

(define ({%error-message-delivery-timeout-expired . <bottom>} {who <symbol>} {chan <channel>})
  ;;Raise  a  non-continuable  exception  representing the  error:  message  delivery
  ;;timeout expired.   The raised  condition object  has components:  &who, &message,
  ;;&channel, &timeout-expired.
  ;;
  (raise
   (condition (make-channel-condition chan)
	      (make-delivery-timeout-expired-condition)
	      (make-who-condition who)
	      (make-message-condition "message reception timeout expired"))))

(define ({%error-maximum-message-size-exceeded . <bottom>} {who <symbol>} {chan <channel>})
  ;;Raise a  non-continuable exception representing  the error: maximum  message size
  ;;exceeded.  The raised condition object  has components: &who, &message, &channel,
  ;;&maximum-message-size-exceeded.
  ;;
  (raise
   (condition (make-channel-condition chan)
	      (make-maximum-message-size-exceeded-condition)
	      (make-who-condition who)
	      (make-message-condition "message reception timeout expired"))))


;;;; receiving messages

(define ({channel-recv-begin! <void>} {chan <channel>})
  ;;Configure  a channel  to start  receiving a  message; return  unspecified values.
  ;;CHAN must be an  input or input/output channel; it is an error  if the channel is
  ;;not inactive.
  ;;
  (assert-inactive-channel __who__ chan)
  (.action		chan 'recv)
  (.message-buffer	chan '())
  (.message-size	chan 0)
  (.message-terminated?	chan #f)
  (void))

(define ({channel-recv-end! <top>} {chan <channel>})
  ;;Finish receiving a  message and return the accumulated octets  in a bytevector or
  ;;chars  in a  string.  It  is an  error if  the channel  is not  in the  course of
  ;;receiving a message.
  ;;
  ;;After this function is applied to a  channel: the channel itself is configured as
  ;;inactive; so  it is  available to start  receiving another message  or to  send a
  ;;message.
  ;;
  (assert-receiving-channel __who__ chan)
  (receive (reverse-buffers total-size)
      ($channel-recv-end!/rbl chan)
    (if (binary-channel? chan)
	($bytevector-reverse-and-concatenate total-size reverse-buffers)
      ($string-reverse-and-concatenate total-size reverse-buffers))))

(define (channel-recv-end!/rbl chan)
  ;;Finish receiving  a message  and return the  2 values: a  list of  bytevectors or
  ;;strings  representing the  data buffers  accumulated in  reverse order,  an exact
  ;;integer representing the total  data size.  It is an error if  the channel is not
  ;;in the course of receiving a message.
  ;;
  ;;After this function is applied to a  channel: the channel itself is configured as
  ;;inactive; so  it is  available to start  receiving another message  or to  send a
  ;;message.
  ;;
  (assert-receiving-channel __who__ chan)
  (begin0
      (values ($channel-message-buffer chan)
	      ($channel-message-size   chan))
    (.action			chan 'none)
    (.message-buffer		chan '())
    (.message-size		chan 0)
    (.message-terminated?	chan #f)))

;;; --------------------------------------------------------------------

(define (channel-recv-message-portion! chan)
  ;;Receive a portion of input message from the given channel.  It is an error if the
  ;;channel is not in the course of receiving a message.
  ;;
  ;;* Return true if  a configured message terminator is read from  the input port or
  ;;  if the channel already read a terminator in a previous operation.  If a message
  ;;  terminator is received: set CHAN to "message terminated" status.
  ;;
  ;;* Return  the EOF  object if EOF  is read  from the input  port before  a message
  ;;  terminator.
  ;;
  ;;* Return false if  neither a message terminator nor EOF is read;  in this case we
  ;;  need to call this function again later to receive further message portions.
  ;;
  ;;* If  the message delivery  timeout is expired  or expires while  receiving data:
  ;;  raise an exception.
  ;;
  ;;* If the accumulated data exceeds the maximum message size: raise an exception.
  ;;
  (cond ((and (binary-channel?     chan)
	      ($receiving-channel? chan))
	 ($channel-recv-binary-message-portion! chan))
	((and (textual-channel?    chan)
	      ($receiving-channel? chan))
	 ($channel-recv-textual-message-portion! chan))
	(else
	 (assertion-violation __who__
	   "expected net channel in the course of receiving a message as argument" chan))))

;;; --------------------------------------------------------------------

(define (channel-recv-full-message {chan <recv-channel>})
  ($channel-recv-begin! chan)
  (let next-portion ()
    (let ((rv (if (binary-channel? chan)
		  ($channel-recv-binary-message-portion! chan)
		($channel-recv-textual-message-portion! chan))))
      (cond ((eof-object? rv)
	     rv)
	    ((would-block-object? rv)
	     rv)
	    ((not rv)
	     (next-portion))
	    (else
	     ($channel-recv-end! chan))))))


;;;; receiving messages: binary message portion

(module ($channel-recv-binary-message-portion!)

  (define who 'channel-recv-message-portion!)

  (define ($channel-recv-binary-message-portion! chan)
    ;;Receive a portion of input message  from the given channel.  It is
    ;;an  error if  the channel  is  not in  the course  of receiving  a
    ;;message.
    ;;
    ;;* Return true if a configured  message terminator is read from the
    ;;input  port or  if  the channel  already read  a  terminator in  a
    ;;previous operation.  If a message terminator is received: set CHAN
    ;;to "message terminated" status.
    ;;
    ;;* Return the EOF object if EOF  is read from the input port before
    ;;a message terminator.
    ;;
    ;;* Return false if neither a message terminator nor EOF is read; in
    ;;this case  we need  to call  this function  again later  to receive
    ;;further message portions.
    ;;
    ;;*  If the  message delivery  timeout is  expired or  expires while
    ;;receiving  data:  raise an  exception.
    ;;
    ;;* If the accumulated data  exceeds the maximum message size: raise
    ;;an exception.
    ;;
    (cond
     ;;If the  message is terminated: we  do not care anymore  about the
     ;;timeout.
     (($channel-message-terminated? chan)
      #t)
     ;;If the message  is not terminated and the  timeout expired: raise
     ;;an error.
     (($delivery-timeout-expired? chan)
      (%error-message-delivery-timeout-expired who chan))
     (else
      (let ((bv (get-bytevector-n ($channel-connect-in-port chan)
				  ($channel-maximum-message-portion-size chan))))
	(cond
	 ;;If  the EOF  is found  before reading  a message  terminator:
	 ;;return the EOF object.
	 ((eof-object? bv)
	  bv)
	 ;;If reading causes a would-block  condition with no input data
	 ;;or an empty bytevector is  read: return would block to signal
	 ;;the need to read further message portions.
	 ((or (would-block-object? bv)
	      ($fxzero? ($bytevector-length bv)))
	  #!would-block)
	 ;;If a message portion is read: push it on the internal buffer;
	 ;;check message size and timeout expiration; return true if the
	 ;;message is terminated, false otherwise.
	 (else
	  ($channel-message-buffer-push! chan bv)
	  (cond (($maximum-size-exceeded? chan)
		 (%error-maximum-message-size-exceeded who chan))
		(($delivery-timeout-expired? chan)
		 (%error-message-delivery-timeout-expired who chan))
		((%received-message-terminator? chan)
		 ($channel-message-terminated?-set! chan #t)
		 #t)
		(else #f))))))))

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


;;;; receiving messages: textual message portion

(module ($channel-recv-textual-message-portion!)

  (define who 'channel-recv-message-portion!)

  (define ($channel-recv-textual-message-portion! chan)
    ;;Receive a portion of input message  from the given channel.  It is
    ;;an  error if  the channel  is  not in  the course  of receiving  a
    ;;message.
    ;;
    ;;* Return true if a configured  message terminator is read from the
    ;;input  port or  if  the channel  already read  a  terminator in  a
    ;;previous operation.  If a message terminator is received: set CHAN
    ;;to "message terminated" status.
    ;;
    ;;* Return the EOF object if EOF  is read from the input port before
    ;;a message terminator.
    ;;
    ;;* Return false if neither a message terminator nor EOF is read; in
    ;;this case  we need to  call this  function again later  to receive
    ;;further message portions.
    ;;
    ;;*  If the  message delivery  timeout is  expired or  expires while
    ;;receiving  data:  raise an  exception.
    ;;
    ;;* If the accumulated data  exceeds the maximum message size: raise
    ;;an exception.
    ;;
    (cond
     ;;If the  message is terminated: we  do not care anymore  about the
     ;;timeout.
     (($channel-message-terminated? chan)
      #t)
     ;;If the message  is not terminated and the  timeout expired: raise
     ;;an error.
     (($delivery-timeout-expired? chan)
      (%error-message-delivery-timeout-expired who chan))
     (else
      (let ((str (get-string-n ($channel-connect-in-port chan)
			       ($channel-maximum-message-portion-size chan))))
	(cond
	 ;;If  the EOF  is found  before reading  a message  terminator:
	 ;;return the EOF object.
	 ((eof-object? str)
	  str)
	 ;;If reading causes a would-block  condition with no input data
	 ;;or an empty string is  read: return would-block to signal the
	 ;;need to read further message portions.
	 ((or (would-block-object? str)
	      ($fxzero? ($string-length str)))
	  #!would-block)
	 ;;If a message portion is read: push it on the internal buffer;
	 ;;check message size and timeout expiration; return true if the
	 ;;message is terminated, false otherwise.
	 (else
	  ($channel-message-buffer-push! chan str)
	  (cond (($maximum-size-exceeded? chan)
		 (%error-maximum-message-size-exceeded who chan))
		(($delivery-timeout-expired? chan)
		 (%error-message-delivery-timeout-expired who chan))
		((%received-message-terminator? chan)
		 ($channel-message-terminated?-set! chan #t)
		 #t)
		(else #f))))))))

  (module (%received-message-terminator?)

    (define (%received-message-terminator? chan)
      ;;Compare all the message terminators with the strings accumulated
      ;;in the buffer of CHAN.  If the  tail of the buffer equals one of
      ;;the terminators: return true, else return false.
      ;;
      (let ((terminators ($channel-message-terminators chan))
	    (buffers     ($channel-message-buffer chan)))
	(find (lambda (terminator)
		($terminated-chars-stream? buffers terminator))
	  terminators)))

    (define ($terminated-chars-stream? reverse-stream terminator)
      ;;Compare a terminator  with the tail of an chars  stream; if the
      ;;stream  is terminated  return #t,  else return  #f.  This  is an
      ;;unsafe  function: it  assumes  the arguments  have been  already
      ;;validated.
      ;;
      ;;TERMINATOR must  be a  non-empty string representing  the stream
      ;;terminator; the  last char in TERMINATOR  is the last char  in a
      ;;properly terminated stream.
      ;;
      ;;REVERSE-SEQUENCE must  be null  or a  list of  non-empty strings
      ;;representing the stream of chars in string-reversed order; as if
      ;;the stream  of chars has  been accumulated (= CONSed)  string by
      ;;string:
      ;;
      ;;* The first  item of REVERSE-SEQUENCE is the last  string in the
      ;;   stream,  the  last  item of  REVERSE-SEQUENCE  is  the  first
      ;;  string in the stream.
      ;;
      ;;* Every string in REVERSE-SEQUENCE represents a chunk of stream:
      ;;  the first char  in the string is the first  char in the chunk,
      ;;  the last char in the string is the last char in the chunk.
      ;;
      (define ($string-last-index str)
	($fxsub1 ($string-length str)))
      (let loop ((terminator.idx  ($string-last-index terminator))
		 (buffers         reverse-stream))
	(cond (($fx= -1 terminator.idx)
	       #t)
	      ((null? buffers)
	       #f)
	      ((let* ((buf     ($car buffers))
		      (buf.idx ($string-last-index buf)))
		 ($compare-string-tails terminator terminator.idx buf buf.idx))
	       => (lambda (terminator.idx)
		    (loop ($fxsub1 terminator.idx) ($cdr buffers))))
	      (else #f))))

    (define ($compare-string-tails A A.idx B B.idx)
      ;;Recursive function.   Compare the string A,  starting from index
      ;;A.idx  inclusive, to  the string  B, starting  from index  B.idx
      ;;inclusive.  If:
      ;;
      ;;* All the chars are equal  up to (zero? A.idx) included: return
      ;;  0.  An example of this case is a call with arguments:
      ;;
      ;;     A =    "345"	A.idx = 2
      ;;     B = "012345"	B.idx = 5
      ;;
      ;;  another example of this case:
      ;;
      ;;     A = "012345"	A.idx = 5
      ;;     B = "012345"	B.idx = 5
      ;;
      ;;* All the chars are equal  up to (zero?  B.idx) included: return
      ;;  the  value of A.idx referencing  the last compared char  in A.
      ;;  An example of this case is a call with arguments:
      ;;
      ;;     A = "012345"	A.idx = 5
      ;;     B =    "345"	B.idx = 2
      ;;
      ;;  the returned value is: A.idx == 3.
      ;;
      ;;*  Chars having  (positive?  A.idx)  and (positive?   B.idx) are
      ;;  different: return false.
      ;;
      (and ($char= ($string-ref A A.idx)
		   ($string-ref B B.idx))
	   (cond (($fxzero? A.idx)
		  0)
		 (($fxzero? B.idx)
		  A.idx)
		 (else
		  ($compare-string-tails A ($fxsub1 A.idx)
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
    ($channel-send-begin! chan)))

(define ($channel-send-begin! chan)
  ($channel-action-set!          chan 'send)
  ($channel-message-buffer-set!  chan '())
  ($channel-message-size-set!    chan 0)
  (void))

;;; --------------------------------------------------------------------

(define (channel-send-end! chan)
  ;;Finish sending a message by flushing the connect port and return the
  ;;total number of octets or chars sent.  It is an error if the channel
  ;;is not in the course of sending a message.
  ;;
  ;;After this function  is applied to a channel: the  channel itself is
  ;;configured  as  inactive; so  it  is  available to  start  receiving
  ;;another message or to send a message.
  ;;
  (define who 'channel-send-end!)
  (with-arguments-validation (who)
      ((sending-channel	chan))
    ($channel-send-end! chan)))

(define ($channel-send-end! chan)
  (begin0
      ($channel-message-size chan)
    (flush-output-port ($channel-connect-ou-port chan))
    ($channel-action-set!          chan #f)
    ($channel-message-buffer-set!  chan '())
    ($channel-message-size-set!    chan 0)))

;;; --------------------------------------------------------------------

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
  (define-argument-validation (portion who obj chan)
    (if (binary-channel? chan)
	(bytevector? obj)
      (string? obj))
    (assertion-violation who "expected appropriate message portion as argument" obj))
  (with-arguments-validation (who)
      ((sending-channel	chan)
       (portion		portion chan))
    ($channel-send-message-portion! chan portion)))

(define ($channel-send-message-portion! chan portion)
  (define who '$channel-send-message-portion!)
  ($channel-message-increment-size! chan (if (binary-channel? chan)
					     ($bytevector-length portion)
					   ($string-length portion)))
  (cond (($delivery-timeout-expired? chan)
	 (%error-message-delivery-timeout-expired who chan))
	(($maximum-size-exceeded? chan)
	 (%error-maximum-message-size-exceeded who chan))
	(else
	 (if (binary-channel? chan)
	     (put-bytevector ($channel-connect-ou-port chan) portion)
	   (put-string ($channel-connect-ou-port chan) portion)))))

;;; --------------------------------------------------------------------

(define (channel-send-full-message chan . message-portions)
  (define who 'channel-send-full-message)
  (with-arguments-validation (who)
      ((inactive-channel	chan)
       (output-channel		chan))
    ($channel-send-full-message chan message-portions)))

(define ($channel-send-full-message chan message-portions)
  ($channel-send-begin! chan)
  (for-each-in-order (lambda (portion)
		       ($channel-send-message-portion! chan portion))
    message-portions)
  ($channel-send-end! chan))


;;;; done

#| end of library |# )

;;; end of file
