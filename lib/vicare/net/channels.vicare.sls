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
  (options typed-language predicate-type-propagation)
  (export
    ;; record type
    <channel>
    <binary-channel>			<textual-channel>
    <binary-input-channel>		<textual-input-channel>
    <binary-output-channel>		<textual-output-channel>
    <binary-input/output-channel>	<textual-input/output-channel>
    <input-channel>			<output-channel>

    ;; condition objects
    &channel
    ;; make-channel-condition
    ;; channel-condition?
    ;; condition-channel

    &delivery-timeout-expired
    ;; make-delivery-timeout-expired-condition
    ;; delivery-timeout-expired-condition?

    &maximum-message-size-exceeded
    ;; make-maximum-message-size-exceeded-condition
    ;; maximum-message-size-exceeded-condition?
    #| end of EXPORT |# )
  (import (vicare)
    (vicare system $fx)
    (vicare system $pairs)
    (vicare system $chars)
    (vicare system $strings)
    (vicare system $bytevectors)
    #;(vicare language-extensions interfaces)
    (vicare language-extensions mixins)
    (prefix (vicare platform words (0 4))
	    words::))


;;;; helpers

(define-constant DEFAULT-BINARY-TERMINATORS
  '(#ve(ascii "\r\n\r\n") #ve(ascii "\r\n")))

(define-constant DEFAULT-TEXTUAL-TERMINATORS
  '("\r\n\r\n" "\r\n"))

;;This is the length (in bytes) of the bytevectors used to hold portions of messages.
;;We want each bytevector to fill a single memory page.  See the documentation of the
;;bytevector object layout.
;;
(define-constant DEFAULT-BINARY-MESSAGE-MAXIMUM-PORTION-SIZE
  (- 4096
     ;;This is  the offset of the  first data byte  from the beginning of  the memory
     ;;block.  The offset is equal on 32-bit and 64-bit platforms.
     8
     ;;This is the size (in bytes) of the word needed to have a terminating zero byte
     ;;right after the data area.
     (words::case-word-size
      ((32)	4)
      ((64)	8))))

(define-constant DEFAULT-BINARY-MESSAGE-MAXIMUM-SIZE
  DEFAULT-BINARY-MESSAGE-MAXIMUM-PORTION-SIZE)

;;This  is the  length  (in characters)  of  the  strings used  to  hold portions  of
;;messages.  We want each string to fill a single memory page.  See the documentation
;;of the string object layout.
;;
(define-constant DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-PORTION-SIZE
  (div (- 4096
	  ;;This word size is for the fixnum representing the string length.
	  (words::case-word-size
	   ((32)	4)
	   ((64)	8)))
       ;;Every character is represented by a 32-bit slot.
       4))

(define-constant DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-SIZE
  DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-PORTION-SIZE)

;;; --------------------------------------------------------------------

(define-type <binary-terminators>
  (nelist-of <nebytevector>))

(define-type <textual-terminators>
  (nelist-of <nestring>))

(define-type <terminators>
  (or <binary-terminators>
      <textual-terminators>))

;;Number of bytes or characters sent or received through a channel.
;;
(define-alias <message-portion-length>
  <non-negative-fixnum>)

;;; --------------------------------------------------------------------

(define ({maximum-expiration-time <epoch-time>})
  (new <epoch-time> (greatest-fixnum) 0))

;;; --------------------------------------------------------------------

(define-type <binary-recv-port>
  (or <binary-input-port> <binary-input/output-port>))

(define-type <binary-send-port>
  (or <binary-output-port> <binary-input/output-port>))

(define-type <textual-recv-port>
  (or <textual-input-port> <textual-input/output-port>))

(define-type <textual-send-port>
  (or <textual-output-port> <textual-input/output-port>))


(define-record-type <channel>
  (nongenerative vicare:net:channels:<channel>)
  (strip-angular-parentheses)

  (fields
    (mutable {action (enumeration none send recv)})
		;A symbol representing the current action for this channel.
    (mutable {maximum-message-size <positive-fixnum>})
		;A  non-negative exact  integer  representing  the inclusive  maximum
		;message size; if the size of the message exceeds this value: message
		;delivery will fail.
    (mutable {maximum-message-portion-size <positive-fixnum>})
		;A positive fixnum  representing the maximum number  of units (bytes,
		;characters) read at each "message portion receive" operation.

    (mutable {message-size <non-negative-fixnum>})
		;A non-negative exact integer representing the current message size.
    (mutable {message-terminated? <boolean>})
		;A  boolean, true  if while  receiving a  message the  terminator has
		;already been read.

    (mutable {expiration-time <epoch-time>})
		;A   "<time>"  object   (as  defined   by  the   library  "(vicare)")
		;representing the limit  of time since the Epoch  to complete message
		;delivery; if  the allotted time  expires: sending or  receiving this
		;message will fail.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (named-lambda make-channel
	  ({_ <channel>} {max-message-size <positive-fixnum>} {max-portion-size <positive-fixnum>})
	(make-record 'none	      ;action
		     max-message-size ;maximum-message-size
		     max-portion-size ;maximum-message-portion-size
		     0		      ;message-size
		     #f		      ;message-terminated?
		     (maximum-expiration-time)))))

  (constructor-signature
    (lambda (<positive-fixnum> <positive-fixnum>) => (<channel>)))

;;; --------------------------------------------------------------------
;;; status inspection methods

  (method ({receiving? <boolean>})
    ;;Return #t if THIS is in the course of receiving a message, else return #f.
    ;;
    (eq? 'recv (.action this)))

  (method ({sending? <boolean>})
    ;;Return #t if THIS is in the course of sending a message, else return #f.
    ;;
    (eq? 'send (.action this)))

  (method ({inactive? <boolean>})
    ;;Return #t if CHAN is neither in  the course of sending nor receiving a message,
    ;;else return #f.
    ;;
    (eq? 'none (.action this)))

;;;

  (method ({delivery-timeout-expired? <boolean>})
    ;;Return true if the delivery timeout has expired; otherwise return false.
    ;;
    (time<=? (.expiration-time this) (current-time)))

  (method ({maximum-size-exceeded? <boolean>})
    ;;Return true if adding the last portion of message made the total message length
    ;;exceed the configured maximum; otherwise return false.
    ;;
    (> (.message-size this)
       (.maximum-message-size this)))

;;; --------------------------------------------------------------------
;;; operation methods

  (method ({message-increment-size! <void>} {delta-size <positive-fixnum>})
    ;;Increment the total  message size by DELTA-SIZE.  Notice that  DELTA-SIZE is no
    ;;always the configured "maximum message portion size": at the end of a message a
    ;;smaller number of units is usually present.
    ;;
    (.message-size this (+ delta-size (.message-size this))))

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <binary-channel>
  (nongenerative vicare:net:channels:<binary-channel>)
  (parent <channel>)
  (fields
    (mutable {message-buffer (list-of <nebytevector>)})
		;Null or a  list of bytevectors representing the  data accumulated so
		;far; last input first.
    (mutable {message-terminators <binary-terminators>})
		;A  non-empty list  of  non-empty  bytevectors representing  possible
		;message terminators.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-channel)
      (named-lambda make-binary-channel ({_ <binary-channel>})
	((make-channel DEFAULT-BINARY-MESSAGE-MAXIMUM-SIZE DEFAULT-BINARY-MESSAGE-MAXIMUM-PORTION-SIZE)
	 '() DEFAULT-BINARY-TERMINATORS))))

  (constructor-signature
    (lambda () => (<binary-channel>)))

;;; --------------------------------------------------------------------

  (method ({message-buffer-push! <void>} {data <nebytevector>})
    (.message-buffer this (cons data (.message-buffer this)))
    (.message-increment-size! this (.length data)))

  (method ({reverse-and-concatenate-buffer <bytevector>})
    ($bytevector-reverse-and-concatenate (.message-size this) (.message-buffer this)))

;;; --------------------------------------------------------------------

  (method ({abort! <void>})
    ;;Abort the current operation and reset the channel to inactive; return unspecified
    ;;values.  Send and receive nothing.
    ;;
    (.action              this 'none)
    (.message-buffer      this '())
    (.message-size        this 0)
    (.message-terminated? this #f)
    (.expiration-time     this (maximum-expiration-time))
    (void))

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <textual-channel>
  (nongenerative vicare:net:channels:<textual-channel>)
  (parent <channel>)
  (fields
    (mutable {message-buffer (list-of <nestring>)})
		;Null or a list of strings  representing the data accumulated so far;
		;last input first.
    (mutable {message-terminators <textual-terminators>})
		;A non-empty list of  non-empty strings representing possible message
		;terminators.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-channel)
      (named-lambda make-textual-channel ({_ <textual-channel>})
	((make-channel DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-SIZE DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-PORTION-SIZE)
	 '() DEFAULT-TEXTUAL-TERMINATORS))))

  (constructor-signature
    (lambda () => (<textual-channel>)))

;;; --------------------------------------------------------------------

  (method ({message-buffer-push! <void>} {data <nestring>})
    (.message-buffer this (cons data (.message-buffer this)))
    (.message-increment-size! this (.length data)))

  (method ({reverse-and-concatenate-buffer <string>})
    ($string-reverse-and-concatenate (.message-size this) (.message-buffer this)))

;;; --------------------------------------------------------------------

  (method ({abort! <void>})
    ;;Abort the current operation and reset the channel to inactive; return unspecified
    ;;values.  Send and receive nothing.
    ;;
    (.action              this 'none)
    (.message-buffer      this '())
    (.message-size        this 0)
    (.message-terminated? this #f)
    (.expiration-time     this (maximum-expiration-time))
    (void))

  #| end of DEFINE-RECORD-TYPE |# )


;;;; common methods for input channels

(define-mixin-type <receiving-channel-methods>

  (method ({recv-begin! <void>})
    ;;Configure a  channel to start  receiving a message; return  unspecified values.
    ;;THIS must be an input or input/output channel; it is an error if the channel is
    ;;not inactive.
    ;;
    (assert-inactive-channel __who__ this)
    (.action this 'recv)
    (void))

  (method (recv-end!/rbl)
    ;;Finish receiving a message; it is an error  if the channel is not in the course
    ;;of receiving a message.  Return the values:
    ;;
    ;;1. A list  of bytevectors or strings representing the  data buffers accumulated
    ;;in reverse order (reverse bytevector list, RBL).
    ;;
    ;;2. An non-negative fixnum representing the total  data size.
    ;;
    ;;After this  function is applied  to a channel: the  channel itself is  reset to
    ;;inactive; so it  is available to start  receiving another message or  to send a
    ;;message.
    ;;
    (assert-receiving-channel __who__ this)
    (begin0
	(values (.message-size   this)
		(.message-buffer this))
      (.abort! this)))

  (method (recv-end!)
    ;;Finish receiving a message; it is an error  if the channel is not in the course
    ;;of receiving a message.  Return the accumulated octets in a bytevector or chars
    ;;in a string.
    ;;
    ;;After this  function is applied  to a channel: the  channel itself is  reset to
    ;;inactive; so it  is available to start  receiving another message or  to send a
    ;;message.
    ;;
    (assert-receiving-channel __who__ this)
    (begin0
	(.reverse-and-concatenate-buffer this)
      (.abort! this)))

  ;;Here we let type propagation select the type of the return value.
  ;;
  (method (recv-full-message)
    (assert-inactive-channel __who__ this)
    (.recv-begin! this)
    (let next-portion ()
      (let ((rv (.recv-message-portion! this)))
	(cond ((eof-object? rv)
	       rv)
	      ((would-block-object? rv)
	       (next-portion))
	      ((not rv)
	       (next-portion))
	      (else
	       (.recv-end! this))))))

  #| end of DEFINE-MIXIN-TYPE |# )

;;; --------------------------------------------------------------------

(define-mixin-type <receiving-binary-channel-methods>

  (mixins <receiving-channel-methods>)

  (method ({recv-message-portion! (or <bytevector> <eof> <would-block> <boolean>)})
    ;;Receive a  portion of input message  from the channel.   It is an error  if the
    ;;channel is not in the course of receiving a message.
    ;;
    ;;* Return true if a configured message terminator is read from the input port or
    ;;if the channel already read a terminator in a previous operation.  If a message
    ;;terminator is received: set THIS to "message terminated" status.
    ;;
    ;;* Return  the EOF object if  EOF is read from  the input port before  a message
    ;;terminator.
    ;;
    ;;* Return the would-block object if a  would-block object is read from the input
    ;;port.
    ;;
    ;;* Return false if neither a message terminator nor EOF is read; in this case we
    ;;need to call this function again later to receive further message portions.
    ;;
    ;;* If the  message delivery timeout is expired or  expires while receiving data:
    ;;raise an exception.
    ;;
    ;;* If the accumulated data exceeds the maximum message size: raise an exception.
    ;;
    (assert-receiving-channel __who__ this)
    (%channel-recv-binary-message-portion! this (.connect-in-port this)))

  #| end of mixin |# )

;;; --------------------------------------------------------------------

(define-mixin-type <receiving-textual-channel-methods>
  (mixins <receiving-channel-methods>)

  (method ({recv-message-portion! (or <string> <eof> <would-block> <boolean>)})
    ;;Receive a  portion of input message  from the channel.   It is an error  if the
    ;;channel is not in the course of receiving a message.
    ;;
    ;;* Return true if a configured message terminator is read from the input port or
    ;;if the channel already read a terminator in a previous operation.  If a message
    ;;terminator is received: set THIS to "message terminated" status.
    ;;
    ;;* Return  the EOF object if  EOF is read from  the input port before  a message
    ;;terminator.
    ;;
    ;;* Return the would-block object if a  would-block object is read from the input
    ;;port.
    ;;
    ;;* Return false if neither a message  terminator nor EOF is read; in this case
    ;;we  need  to call  this  function  again  later  to receive  further  message
    ;;portions.
    ;;
    ;;* If the  message delivery timeout is expired or  expires while receiving data:
    ;;raise an exception.
    ;;
    ;;* If the accumulated data exceeds the maximum message size: raise an exception.
    ;;
    (assert-receiving-channel __who__ this)
    (%channel-recv-textual-message-portion! this (.connect-in-port this)))

  #| end of mixin |# )


;;;; common methods for output channels

(define-mixin-type <sending-channel-methods>

  (method ({send-begin! <void>})
    ;;Configure a channel to start sending  a message; return unspecified values.  It
    ;;is an error if the channel is not inactive.
    ;;
    (assert-inactive-channel __who__ this)
    (.action this 'send)
    (void))

  (method ({send-end! <message-portion-length>})
    ;;Finish sending  a message  by flushing  the connect port  and return  the total
    ;;number of octets  or chars sent.  It is  an error if the channel is  not in the
    ;;course of sending a message.
    ;;
    ;;After this function  is applied to a channel: the  channel itself is configured
    ;;as inactive; so it is available to start receiving another message or to send a
    ;;message.
    ;;
    (assert-sending-channel __who__ this)
    (begin0
	($channel-message-size this)
      (flush-output-port (.connect-ou-port this))
      (.abort! this)))

  #| end of mixin |# )

;;; --------------------------------------------------------------------

(define-mixin-type <sending-binary-channel-methods>

  (mixins <sending-channel-methods>)

  (method ({send-message-portion! <void>} {portion <bytevector>})
    ;;Send a portion of output message  through the given channel; return unspecified
    ;;values.  It  is an  error if  the channel  is not  in the  course of  sending a
    ;;message.
    ;;
    ;;PORTION must be a bytevector representing the message portion.
    ;;
    ;;This function does not flush the connection port.
    ;;
    (assert-sending-channel __who__ this)
    (.message-increment-size! this (.length portion))
    (cond ((.delivery-timeout-expired? this)
	   (%error-message-delivery-timeout-expired __who__ this))
	  ((.maximum-size-exceeded? this)
	   (%error-maximum-message-size-exceeded    __who__ this))
	  (else
	   (put-bytevector (.connect-ou-port this) portion))))

  (method ({send-full-message <message-portion-length>} . {message-portions (list-of <bytevector>)})
    (assert-inactive-channel __who__ this)
    (.send-begin! this)
    (for-each-in-order (lambda (portion)
			 (.send-message-portion! this portion))
      message-portions)
    (.send-end! this))

  #| end of mixin |# )

;;; --------------------------------------------------------------------

(define-mixin-type <sending-textual-channel-methods>

  (mixins <sending-channel-methods>)

  (method ({send-message-portion! <void>} {portion <string>})
    ;;Send a portion of output message  through the given channel; return unspecified
    ;;values.  It  is an  error if  the channel  is not  in the  course of  sending a
    ;;message.
    ;;
    ;;PORTION must be a string representing the message portion.
    ;;
    ;;This function does not flush the connection port.
    ;;
    (assert-sending-channel __who__ this)
    (.message-increment-size! this (.length portion))
    (cond ((.delivery-timeout-expired? this)
	   (%error-message-delivery-timeout-expired __who__ this))
	  ((.maximum-size-exceeded? this)
	   (%error-maximum-message-size-exceeded    __who__ this))
	  (else
	   (put-string (.connect-ou-port this) portion))))

  (method ({send-full-message <message-portion-length>} . {message-portions (list-of <string>)})
    (assert-inactive-channel __who__ this)
    (.send-begin! this)
    (for-each-in-order (lambda (portion)
			 (.send-message-portion! this portion))
      message-portions)
    (.send-end! this))

  #| end of mixin |# )


(define-record-type <binary-input-channel>
  (nongenerative vicare:net:channels:<binary-input-channel>)
  (parent <binary-channel>)
  (fields
    (immutable {connect-in-port <binary-recv-port>})
		;An input or input/output binary port used to receive messages from a
		;remote process.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-binary-channel)
      (named-lambda make-binary-input-channel ({_ <binary-input-channel>} {port <binary-recv-port>})
	((make-binary-channel) port))))

  (constructor-signature
    (lambda (<binary-recv-port>) => (<binary-input-channel>)))

  (custom-printer
    (lambda ({this <binary-input-channel>} {port <textual-send-port>} subprinter)
      (display "#[<binary-input-channel>" port)
      (display " port=" port)	(display (.connect-in-port this) port)
      (display "]" port)))

  (mixins <receiving-binary-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <textual-input-channel>
  (nongenerative vicare:net:channels:<textual-input-channel>)
  (parent <textual-channel>)
  (fields
    (immutable {connect-in-port <textual-recv-port>})
		;An input or input/output textual port used to receive messages from a
		;remote process.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-textual-channel)
      (named-lambda make-textual-input-channel ({_ <textual-input-channel>} {port <textual-recv-port>})
	((make-textual-channel) port))))

  (constructor-signature
    (lambda (<textual-recv-port>) => (<textual-input-channel>)))

  (custom-printer
    (lambda ({this <textual-input-channel>} {port <textual-send-port>} subprinter)
      (display "#[<textual-input-channel>" port)
      (display " port=" port)	(display (.connect-in-port this) port)
      (display "]" port)))

  (mixins <receiving-textual-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <binary-output-channel>
  (nongenerative vicare:net:channels:<binary-output-channel>)
  (parent <binary-channel>)
  (fields
    (immutable {connect-ou-port <binary-send-port>})
		;An output  or input/output binary  port used  to send messages  to a
		;remote process.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-binary-channel)
      (named-lambda make-binary-output-channel ({_ <binary-output-channel>} {port <binary-send-port>})
	((make-binary-channel) port))))

  (constructor-signature
    (lambda (<binary-send-port>) => (<binary-output-channel>)))

  (custom-printer
    (lambda ({this <binary-output-channel>} {port <textual-send-port>} subprinter)
      (display "#[<binary-output-channel>" port)
      (display " port=" port)	(display (.connect-ou-port this) port)
      (display "]" port)))

  (mixins <sending-binary-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <textual-output-channel>
  (nongenerative vicare:net:channels:<textual-output-channel>)
  (parent <textual-channel>)
  (fields
    (immutable {connect-ou-port <textual-send-port>})
		;An output  or input/output textual  port used  to send messages  to a
		;remote process.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-textual-channel)
      (named-lambda make-textual-output-channel ({_ <textual-output-channel>} {port <textual-send-port>})
	((make-textual-channel) port))))

  (constructor-signature
    (lambda (<textual-send-port>) => (<textual-output-channel>)))

  (custom-printer
    (lambda ({this <textual-output-channel>} {port <textual-send-port>} subprinter)
      (display "#[<textual-output-channel>" port)
      (display " port=" port)	(display (.connect-ou-port this) port)
      (display "]" port)))

  (mixins <sending-textual-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <binary-input/output-channel>
  (nongenerative vicare:net:channels:<binary-input/output-channel>)
  (parent <binary-channel>)
  (fields
    (immutable {connect-in-port <binary-recv-port>})
		;An input or input/output binary  port used to receive messages from
		;a remote process.
    (immutable {connect-ou-port <binary-send-port>})
		;An output  or input/output binary port  used to send messages  to a
		;remote process.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-binary-channel)
      (named-lambda make-binary-input/output-channel
	  ({_ <binary-input/output-channel>}
	   {in-port <binary-recv-port>}
	   {ou-port <binary-send-port>})
	((make-binary-channel) in-port ou-port))))

  (constructor-signature
    (lambda (<binary-recv-port> <binary-send-port>) => (<binary-input/output-channel>)))

  (custom-printer
    (lambda ({this <binary-input/output-channel>} {port <textual-send-port>} subprinter)
      (display "#[<binary-input/output-channel>" port)
      (display " in-port=" port)	(display (.connect-in-port this) port)
      (display " ou-port=" port)	(display (.connect-ou-port this) port)
      (display "]" port)))

  (mixins <sending-binary-channel-methods>)
  (mixins <receiving-binary-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <textual-input/output-channel>
  (nongenerative vicare:net:channels:<textual-input/output-channel>)
  (parent <textual-channel>)
  (fields
    (immutable {connect-in-port <textual-recv-port>})
		;An input or input/output textual  port used to receive messages from
		;a remote process.
    (immutable {connect-ou-port <textual-send-port>})
		;An output  or input/output textual port  used to send messages  to a
		;remote process.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-textual-channel)
      (named-lambda make-textual-input/output-channel
	  ({_ <textual-input/output-channel>}
	   {in-port <textual-recv-port>}
	   {ou-port <textual-send-port>})
	((make-textual-channel) in-port ou-port))))

  (constructor-signature
    (lambda (<textual-recv-port> <textual-send-port>) => (<textual-input/output-channel>)))

  (custom-printer
    (lambda ({this <textual-input/output-channel>} {port <textual-send-port>} subprinter)
      (display "#[<textual-input/output-channel>" port)
      (display " in-port=" port)	(display (.connect-in-port this) port)
      (display " ou-port=" port)	(display (.connect-ou-port this) port)
      (display "]" port)))

  (mixins <sending-textual-channel-methods>)
  (mixins <receiving-textual-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


;;;; predicates and arguments validation

(define (assert-receiving-channel {who <symbol>} {chan <channel>})
  ;;Succeed  if CHAN  is  an instance  of  "<channel>" and  it is  in  the course  of
  ;;receiving a message.
  ;;
  (unless (.receiving? chan)
    (assertion-violation who "expected channel in the course of receving a message as argument" chan)))

(define (assert-not-receiving-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN  is an instance of "<channel>"  and it is *not* in  the course of
  ;;receiving a message.
  ;;
  (when (.receiving? chan)
    (assertion-violation who "expected channel not in the course of receving a message as argument" chan)))

;;; --------------------------------------------------------------------

(define (assert-sending-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN is an instance of "<channel>"  and it is in the course of sending
  ;;a message.
  ;;
  (unless (.sending? chan)
    (assertion-violation who "expected channel in the course of sending a message as argument" chan)))

(define (asert-not-sending-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN  is an instance of "<channel>"  and it is *not* in  the course of
  ;;sending a message.
  ;;
  (when (.sending? chan)
    (assertion-violation who "expected channel not in the course of sending a message as argument" chan)))

;;; --------------------------------------------------------------------

(define (assert-inactive-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN is an instance of "<channel>"  and it is neither in the course of
  ;;sending nor receiving a message.
  ;;
  (unless (.inactive? chan)
    (assertion-violation who "expected inactive channel as argument" chan)))

(define (assert-not-inactive-channel {who <symbol>} {chan <channel>})
  ;;Succeed  if CHAN  is an  instance  of "<channel>"  and  it is  either sending  or
  ;;receiving a message.
  ;;
  (when (.inactive? chan)
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


;;;; receiving messages: binary message portion

(module (%channel-recv-binary-message-portion!)

  (define-constant __module_who__ 'channel-recv-binary-message-portion!)

  (define ({%channel-recv-binary-message-portion! (or <bytevector> <eof> <would-block> <boolean>)}
	   {chan <binary-channel>} {in-port <binary-recv-port>})
    ;;Receive a portion of  input message from the given channel.  It  is an error if
    ;;the channel is not in the course of receiving a message.
    ;;
    ;;* Return true if a configured message terminator is read from the input port or
    ;;if the channel already read a terminator in a previous operation.  If a message
    ;;terminator is received: set CHAN to "message terminated" status.
    ;;
    ;;* Return  the EOF object if  EOF is read from  the input port before  a message
    ;;terminator.
    ;;
    ;;* Return false if neither a message terminator nor EOF is read; in this case we
    ;;need to call this function again later to receive further message portions.
    ;;
    ;;* If the  message delivery timeout is expired or  expires while receiving data:
    ;;raise an exception.
    ;;
    ;;* If the accumulated data exceeds the maximum message size: raise an exception.
    ;;
    (cond
     ;;If the message is terminated: we do not care anymore about the timeout.
     ((.message-terminated? chan)
      #t)
     ;;If the message is not terminated and the timeout expired: raise an error.
     ((.delivery-timeout-expired? chan)
      (%error-message-delivery-timeout-expired __module_who__ chan))
     (else
      (let ((bv (get-bytevector-n in-port (.maximum-message-portion-size chan))))
	(cond
	 ;;If the  EOF is found before  reading a message terminator:  return the EOF
	 ;;object.
	 ((eof-object? bv)
	  bv)

	 ;;If reading causes a would-block condition with no input data: return would
	 ;;block to signal the need to read further message portions.
	 ((would-block-object? bv)
	  #!would-block)

	 ;;NOTE If a bytevector is read: it is non-empty.
	 ;;
	 ;; ((and (bytevector? bv)
	 ;;       (fxzero? (bytevector-length bv)))
	 ;;  #!would-block)

	 ;;If  a message  portion is  read:  push it  on the  internal buffer;  check
	 ;;message  size  and timeout  expiration;  return  true  if the  message  is
	 ;;terminated, false otherwise.
	 (else
	  (.message-buffer-push! chan bv)
	  (cond ((.maximum-size-exceeded? chan)
		 (%error-maximum-message-size-exceeded __module_who__ chan))
		((.delivery-timeout-expired? chan)
		 (%error-message-delivery-timeout-expired __module_who__ chan))
		((%received-message-terminator? chan)
		 (.message-terminated? chan #t)
		 #t)
		(else #f))))))))

  (module (%received-message-terminator?)

    (define (%received-message-terminator? {chan <binary-channel>})
      ;;Compare all the  message terminators with the bytevectors  accumulated in the
      ;;buffer of  CHAN.  If the  tail of the buffer  equals one of  the terminators:
      ;;return true, else return false.
      ;;
      (let ((terminators (.message-terminators chan))
	    (buffers     (.message-buffer chan)))
	(find (lambda (terminator)
		($terminated-octets-stream? buffers terminator))
	  terminators)))

    (define ($terminated-octets-stream? reverse-stream terminator)
      ;;Compare a  terminator with  the tail of  an octets stream;  if the  stream is
      ;;terminated return #t, else return #f.  This is an unsafe function: it assumes
      ;;the arguments have been already validated.
      ;;
      ;;TERMINATOR must be a non-empty bytevector representing the stream terminator;
      ;;the  last octet  in TERMINATOR  is the  last octet  in a  properly terminated
      ;;stream.
      ;;
      ;;REVERSE-SEQUENCE must be null or a list of non-empty bytevectors representing
      ;;the  stream of  octects in  bytevector-reversed order;  as if  the stream  of
      ;;octets has been accumulated (= CONSed) bytevector by bytevector:
      ;;
      ;;* The  first item of REVERSE-SEQUENCE  is the last bytevector  in the stream,
      ;;  the last item of REVERSE-SEQUENCE is the first bytevector in the stream.
      ;;
      ;;*  Every bytevector  in REVERSE-SEQUENCE  represents a  chunk of  stream: the
      ;;  first  octet in the bytevector  is the first  octet in the chunk,  the last
      ;;  octet in the bytevector is the last octet in the chunk.
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
      ;;Recursive  function.  Compare  the bytevector  A, starting  from index  A.idx
      ;;inclusive, to the bytevector B, starting from index B.idx inclusive.  If:
      ;;
      ;;*  All the  octets are  equal up  to (zero?  A.idx) included:  return 0.   An
      ;;example of this case is a call with arguments:
      ;;
      ;;   A =       #vu8(3 4 5)	A.idx = 2
      ;;   B = #vu8(0 1 2 3 4 5)	B.idx = 5
      ;;
      ;;another example of this case:
      ;;
      ;;   A = #vu8(0 1 2 3 4 5)	A.idx = 5
      ;;   B = #vu8(0 1 2 3 4 5)	B.idx = 5
      ;;
      ;;* All the octets are equal up to (zero?  B.idx) included: return the value of
      ;;A.idx referencing the last compared octet in A.  An example of this case is a
      ;;call with arguments:
      ;;
      ;;   A = #vu8(0 1 2 3 4 5)	A.idx = 5
      ;;   B =       #vu8(3 4 5)	B.idx = 2
      ;;
      ;;the returned value is: A.idx == 3.
      ;;
      ;;* Octects  having (positive?   A.idx) and  (positive?  B.idx)  are different:
      ;;return false.
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

(module (%channel-recv-textual-message-portion!)

  (define-constant __module_who__ 'channel-recv-textual-message-portion!)

  (define ({%channel-recv-textual-message-portion! (or <bytevector> <eof> <would-block> <boolean>)}
	   {chan <textual-channel>} {in-port <textual-recv-port>})
    ;;Receive a portion of  input message from the given channel.  It  is an error if
    ;;the channel is not in the course of receiving a message.
    ;;
    ;;* Return true if a configured message terminator is read from the input port or
    ;;if the channel already read a terminator in a previous operation.  If a message
    ;;terminator is received: set CHAN to "message terminated" status.
    ;;
    ;;* Return  the EOF object if  EOF is read from  the input port before  a message
    ;;terminator.
    ;;
    ;;* Return false if neither a message terminator nor EOF is read; in this case we
    ;;need to call this function again later to receive further message portions.
    ;;
    ;;* If the  message delivery timeout is expired or  expires while receiving data:
    ;;raise an exception.
    ;;
    ;;* If the accumulated data exceeds the maximum message size: raise an exception.
    ;;
    (cond
     ;;If the message is terminated: we do not care anymore about the timeout.
     ((.message-terminated? chan)
      #t)
     ;;If the message is not terminated and the timeout expired: raise an error.
     ((.delivery-timeout-expired? chan)
      (%error-message-delivery-timeout-expired __module_who__ chan))
     (else
      (let ((str (get-string-n in-port (.maximum-message-portion-size chan))))
	(cond
	 ;;If the  EOF is found before  reading a message terminator:  return the EOF
	 ;;object.
	 ((eof-object? str)
	  str)

	 ;;If  reading causes  a would-block  condition  with no  input data:  return
	 ;;would-block to signal the need to read further message portions.
	 ((would-block-object? str)
	  #!would-block)

	 ;;NOTE If a string is read it is non-empty.
	 ;;
	 ;; ((and (string? str)
	 ;;       (fxzero? (string-length str)))
	 ;;  #!would-block)

	 ;;If  a message  portion is  read:  push it  on the  internal buffer;  check
	 ;;message  size  and timeout  expiration;  return  true  if the  message  is
	 ;;terminated, false otherwise.
	 (else
	  (.message-buffer-push! chan str)
	  (cond ((.maximum-size-exceeded? chan)
		 (%error-maximum-message-size-exceeded __module_who__ chan))
		((.delivery-timeout-expired? chan)
		 (%error-message-delivery-timeout-expired __module_who__ chan))
		((%received-message-terminator? chan)
		 (.message-terminated? chan #t)
		 #t)
		(else #f))))))))

  (module (%received-message-terminator?)

    (define (%received-message-terminator? {chan <textual-channel>})
      ;;Compare  all the  message terminators  with  the strings  accumulated in  the
      ;;buffer of  CHAN.  If the  tail of the buffer  equals one of  the terminators:
      ;;return true, else return false.
      ;;
      (let ((terminators (.message-terminators chan))
	    (buffers     (.message-buffer      chan)))
	(find (lambda (terminator)
		(%terminated-chars-stream? buffers terminator))
	  terminators)))

    (define (%terminated-chars-stream? {reverse-stream (list-of <nestring>)} {terminator <nestring>})
      ;;Compare a  terminator with  the tail  of an  chars stream;  if the  stream is
      ;;terminated return #t, else return #f.  This is an unsafe function: it assumes
      ;;the arguments have been already validated.
      ;;
      ;;TERMINATOR must be a non-empty string representing the stream terminator; the
      ;;last char in TERMINATOR is the last char in a properly terminated stream.
      ;;
      ;;REVERSE-SEQUENCE must be null or a list of non-empty strings representing the
      ;;stream of chars in string-reversed order; as  if the stream of chars has been
      ;;accumulated (= CONSed) string by string:
      ;;
      ;;* The first  item of REVERSE-SEQUENCE is  the last string in  the stream, the
      ;;last item of REVERSE-SEQUENCE is the first string in the stream.
      ;;
      ;;* Every  string in REVERSE-SEQUENCE represents  a chunk of stream:  the first
      ;;char in  the string  is the first  char in  the chunk, the  last char  in the
      ;;string is the last char in the chunk.
      ;;
      (define ({%string-last-index <fixnum>} {str <string>})
	(fxsub1 (.length str)))
      (let loop (({terminator.idx <fixnum>}		(%string-last-index terminator))
		 ({buffers (list-of <nestring>)}	reverse-stream))
	(cond ((fx=? -1 terminator.idx)
	       #t)
	      ((and (pair? buffers)
		    (let* (({buffers (nelist-of <nestring>)}	buffers)
			   (buf		(car buffers))
			   (buf.idx	(%string-last-index buf)))
		      (%compare-string-tails terminator terminator.idx buf buf.idx)))
	       => (lambda (terminator.idx)
		    (loop (fxsub1 terminator.idx) (cdr buffers))))
	      (else #f))))

    (define ({%compare-string-tails (or <false> <non-negative-fixnum>)}
	     {A <string>} {A.idx <non-negative-fixnum>}
	     {B <string>} {B.idx <non-negative-fixnum>})
      ;;Recursive  function.   Compare  the  string  A,  starting  from  index  A.idx
      ;;inclusive, to the string B, starting from index B.idx inclusive.  If:
      ;;
      ;;* All the chars are equal up to (zero? A.idx) included: return 0.  An example
      ;;of this case is a call with arguments:
      ;;
      ;;     A =    "345"	A.idx = 2
      ;;     B = "012345"	B.idx = 5
      ;;
      ;;another example of this case:
      ;;
      ;;     A = "012345"	A.idx = 5
      ;;     B = "012345"	B.idx = 5
      ;;
      ;;* All the chars are equal up  to (zero?  B.idx) included: return the value of
      ;;A.idx referencing the last compared char in  A.  An example of this case is a
      ;;call with arguments:
      ;;
      ;;     A = "012345"	A.idx = 5
      ;;     B =    "345"	B.idx = 2
      ;;
      ;;  the returned value is: A.idx == 3.
      ;;
      ;;*  Chars having  (positive?   A.idx) and  (positive?   B.idx) are  different:
      ;;return false.
      ;;
      (and (char=? (string-ref A A.idx)
		   (string-ref B B.idx))
	   (cond ((fxzero? A.idx)
		  0)
		 ((fxzero? B.idx)
		  A.idx)
		 (else
		  (%compare-string-tails A (fxsub1 A.idx)
					 B (fxsub1 B.idx))))))

    #| end of module: %received-message-terminator? |# )

  #| end of module: channel-recv-message-portion! |# )


;;;; types

(define-type <input-channel>
  (or <binary-input-channel> <textual-input-channel>))

(define-type <output-channel>
  (or <binary-output-channel> <textual-output-channel>))


;;;; done

#| end of library |# )

;;; end of file
