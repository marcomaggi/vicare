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
(library (vicare net channels (0 4 2016 10 20))
  (options typed-language predicate-type-propagation)
  (export
    ;; record type
    <channel>
    <binary-input-only-channel>		<textual-input-only-channel>
    <binary-output-only-channel>	<textual-output-only-channel>
    <binary-input/output-channel>	<textual-input/output-channel>

    <<channel>>
    <<binary-input-channel>>		<<binary-output-channel>>
    <<textual-input-channel>>		<<textual-output-channel>>

    ;; condition objects
    &channel
    &delivery-timeout-expired
    &maximum-message-size-exceeded
    #| end of EXPORT |# )
  (import (vicare)
    (only (vicare system $bytevectors)
	  $bytevector-reverse-and-concatenate)
    (only (vicare system $strings)
	  $string-reverse-and-concatenate)
    (vicare language-extensions mixins)
    (vicare language-extensions interfaces)
    (prefix (vicare platform words (0 4)) words::))


;;;; helpers

(define-constant DEFAULT-BINARY-TERMINATORS
  '#(#ve(ascii "\r\n\r\n")))

(define-constant DEFAULT-TEXTUAL-TERMINATORS
  '#("\r\n\r\n"))

;;This is the maximum  length (in bytes) of the bytevectors used  to hold portions of
;;messages; such bytevectors  are never empty.  We want each  bytevector, at most, to
;;fill a  single memory page (4096  bytes).  See the documentation  of the bytevector
;;object layout.
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

;;This is  the maximum  length (in  bytes) of  a full  binary message;  it must  be a
;;positive fixnum.
;;
(define-constant DEFAULT-BINARY-MESSAGE-MAXIMUM-SIZE
  DEFAULT-BINARY-MESSAGE-MAXIMUM-PORTION-SIZE)

;;This  is the  length  (in characters)  of  the  strings used  to  hold portions  of
;;messages; such strings  are never empty.  We  want each string, at most,  to fill a
;;single  memory page  (4096  bytes).  See  the documentation  of  the string  object
;;layout.
;;
(define-constant DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-PORTION-SIZE
  (div (- 4096
	  ;;This word size is for the fixnum representing the string length.
	  (words::case-word-size
	   ((32)	4)
	   ((64)	8)))
       ;;Every character is represented by a 32-bit slot.
       4))

;;This is the maximum length (in characters) of  a full textual message; it must be a
;;positive fixnum.
;;
(define-constant DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-SIZE
  DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-PORTION-SIZE)

;;; --------------------------------------------------------------------

(define-type <binary-terminators>
  (nevector-of <nebytevector>))

(define-type <textual-terminators>
  (nevector-of <nestring>))


;;;; interfaces

(define-interface-type <<channel>>
  (method-prototype expiration-time		(case-lambda
						  (()			=> (<epoch-time>))
						  ((<epoch-time>)	=> (<void>))))
  (method-prototype maximum-message-size	(case-lambda
						  (()			=> (<positive-fixnum>))
						  ((<positive-fixnum>)	=> (<void>))))
  (method-prototype current-message-size	(lambda ()			=> (<non-negative-fixnum>)))
  (method-prototype receiving?			(lambda ()			=> (<boolean>)))
  (method-prototype sending?			(lambda ()			=> (<boolean>)))
  (method-prototype inactive?			(lambda ()			=> (<boolean>)))
  (method-prototype delivery-timeout-expired?	(lambda ()			=> (<boolean>)))
  #| end of DEFINE-INTERFACE-TYPE |# )

;;; --------------------------------------------------------------------

(define-interface-type <<binary-input-channel>>
  (parent <<channel>>)
  (method-prototype message-terminators		(case-lambda
						  (()				=> (<binary-terminators>))
						  ((<binary-terminators>)	=> (<void>))))
  (method-prototype recv-begin!			(lambda () => (<void>)))
  (method-prototype recv-end!/rbl		(lambda () => (<positive-fixnum> (list-of <nebytevector>))))
  (method-prototype recv-end!			(lambda () => (<nebytevector>)))
  (method-prototype recv-abort!			(lambda () => (<void>)))
  (method-prototype recv-full-message		(lambda () => ((or <eof> <bytevector>))))
  (method-prototype recv-message-portion!	(lambda () => ((or <eof> <would-block> <boolean>))))
  #| end of DEFINE-INTERFACE-TYPE |# )

(define-interface-type <<textual-input-channel>>
  (parent <<channel>>)
  (method-prototype message-terminators		(case-lambda
						  (()				=> (<textual-terminators>))
						  ((<textual-terminators>)	=> (<void>))))
  (method-prototype recv-begin!			(lambda () => (<void>)))
  (method-prototype recv-end!/rbl		(lambda () => (<positive-fixnum> (list-of <nestring>))))
  (method-prototype recv-end!			(lambda () => (<nestring>)))
  (method-prototype recv-abort!			(lambda () => (<void>)))
  (method-prototype recv-full-message		(lambda () => ((or <eof> <string>))))
  (method-prototype recv-message-portion!	(lambda () => ((or <eof> <would-block> <boolean>))))
  #| end of DEFINE-INTERFACE-TYPE |# )

;;; --------------------------------------------------------------------

(define-interface-type <<binary-output-channel>>
  (parent <<channel>>)
  (method-prototype send-begin!			(lambda () => (<void>)))
  (method-prototype send-end!			(lambda () => (<non-negative-fixnum>)))
  (method-prototype send-abort!			(lambda () => (<void>)))
  (method-prototype send-message-portion!	(lambda (<bytevector>) => (<void>)))
  (method-prototype send-full-message		(lambda (list-of <bytevector>) => (<non-negative-fixnum>)))
  (method-prototype flush			(lambda () => (<void>)))
  #| end of DEFINE-INTERFACE-TYPE |# )

(define-interface-type <<textual-output-channel>>
  (parent <<channel>>)
  (method-prototype send-begin!			(lambda () => (<void>)))
  (method-prototype send-end!			(lambda () => (<non-negative-fixnum>)))
  (method-prototype send-abort!			(lambda () => (<void>)))
  (method-prototype send-message-portion!	(lambda (<string>) => (<void>)))
  (method-prototype send-full-message		(lambda (list-of <string>) => (<non-negative-fixnum>)))
  (method-prototype flush			(lambda () => (<void>)))
  #| end of DEFINE-INTERFACE-TYPE |# )


(define-record-type <channel>
  (nongenerative vicare:net:channels:<channel>)
  (strip-angular-parentheses)

  (protected
    (fields
      (mutable {action (enumeration none send recv)})
		;A symbol representing the current action for this channel.
      (mutable {message-terminated? <boolean>})
		;True if  while receiving a  message the terminator has  already been
		;read.
      #| end of FIELDS |# ))
  (fields
    (mutable {expiration-time <epoch-time>})
		;A time  object (as defined  by the library  "(vicare)") representing
		;the limit of  time since the Epoch to complete  message delivery; if
		;the allotted  time expires: sending  or receiving this  message will
		;fail.
    (mutable {maximum-message-size <positive-fixnum>})
		;The  inclusive maximum  message size;  if  the size  of the  message
		;exceeds this value: message delivery will fail.
    #| end of FIELDS |# )
  (protected
    (fields
      (mutable {message-size <non-negative-fixnum>})
		;The current message size.
      #| end of FIELDS |# ))

  (protocol
    (lambda (make-record)
      (named-lambda make-channel
	  ({_ <channel>} {max-message-size <positive-fixnum>})
	(make-record 'none	      ;action
		     #f		      ;message-terminated?
		     (faraway-time)   ;expiration-time
		     max-message-size ;maximum-message-size
		     0		      ;message-size
		     ))))

  (constructor-signature
    (lambda (<positive-fixnum>) => (<channel>)))

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
    ;;Return true  if the delivery timeout  has expired; otherwise return  false.  We
    ;;can  use  this  method  to  query   this  status  even  without  attempting  an
    ;;input/output operation.
    ;;
    (time<=? (.expiration-time this) (current-time)))

  (protected
    (method ({maximum-size-exceeded? <boolean>})
      ;;Return true  if adding  the last  portion of message  made the  total message
      ;;length exceed the configured maximum; otherwise return false.
      ;;
      (fx>? (.message-size this)
	    (.maximum-message-size this))))

;;; --------------------------------------------------------------------
;;; operation methods

  (method ({current-message-size <non-negative-fixnum>})
    ;;Return the current message size.   This method allows querying the MESSAGE-SIZE
    ;;field without exposing its mutator.
    ;;
    (.message-size this))

  (protected
    (method ({message-increment-size! <void>} {delta-size <positive-fixnum>})
      ;;Increment the total message size by DELTA-SIZE.
      ;;
      ;;FIXME  A generic  exception is  raised if  the sum  of the  numbers is  not a
      ;;fixnum.  We should do better here.  (Marco Maggi; Thu Sep 29, 2016)
      ;;
      (.message-size this (+ delta-size (.message-size this)))))

  #| end of DEFINE-RECORD-TYPE |# )


(define-mixin-type <receiving-channel-methods>
  (fields
    (mutable {maximum-message-portion-size <positive-fixnum>})
		;The  maximum  number  of  units (bytes,  characters)  read  at  each
		;"receive message portion" operation.
    #| end of FIELDS |# )

  (method ({recv-begin! <void>})
    ;;Configure a  channel to start  receiving a message; return  unspecified values.
    ;;THIS must be an input or input/output channel; it is an error if the channel is
    ;;not inactive.
    ;;
    (assert-inactive-channel __who__ this)
    (.action this 'recv)
    (void))

  (method ({recv-abort! <void>})
    ;;Abort  the  current  operation  and  reset  the  channel  to  inactive;  return
    ;;unspecified values.  Send and receive nothing.
    ;;
    (assert-receiving-channel __who__ this)
    (.action              this 'none)
    (.message-buffer      this '())
    (.message-size        this 0)
    (.message-terminated? this #f)
    (.expiration-time     this (faraway-time))
    (void))

  #| end of DEFINE-MIXIN-TYPE |# )


(define-mixin-type <receiving-binary-channel-methods>
  (implements <<binary-input-channel>>)

  (fields
    (immutable {connect-in-port <binary-input-port>})
		;An input or input/output binary port used to receive messages from a
		;remote process.
    #| end of FIELDS |# )

  (protected
    (fields
      (mutable {message-buffer (list-of <nebytevector>)})
		;Null or a  list of bytevectors representing the  data accumulated so
		;far; last input first.
      #| end of FIELDS |# ))

  (fields
    (mutable {message-terminators <binary-terminators>})
		;A  non-empty list  of  non-empty  bytevectors representing  possible
		;message terminators.
    #| end of FIELDS |# )

  (mixins <receiving-channel-methods>)

;;; --------------------------------------------------------------------

  (protected
    (method ({message-buffer-push! <void>} {data <nebytevector>})
      (.message-buffer this (cons data (.message-buffer this)))
      (.message-increment-size! this (.length data))))

  (protected
    (method ({reverse-and-concatenate-buffer <nebytevector>})
      ($bytevector-reverse-and-concatenate (.message-size this) (.message-buffer this))))

;;; --------------------------------------------------------------------

  (method ({recv-end!/rbl <positive-fixnum> (list-of <nebytevector>)})
    ;;Finish receiving a message; it is an error  if the channel is not in the course
    ;;of receiving a message.  Return the values:
    ;;
    ;;1. A positive fixnum representing the total data size.
    ;;
    ;;2.   Null or  a list  of non-empty  bytevectors representing  the data  buffers
    ;;accumulated in reverse order (reverse buffer list, RBL).  The data includes the
    ;;terminator.
    ;;
    ;;After this  function is applied  to a channel: the  channel itself is  reset to
    ;;inactive; so it  is available to start  receiving another message or  to send a
    ;;message.
    ;;
    (assert-receiving-channel __who__ this)
    (begin0
	(values (.message-size   this)
		(.message-buffer this))
      (.recv-abort! this)))

  (method ({recv-end! <nebytevector>})
    ;;Finish receiving a message; it is an error  if the channel is not in the course
    ;;of receiving  a message.  Return  the accumulated  octets in a  bytevector; the
    ;;data includes the terminator.
    ;;
    ;;After this  function is applied  to a channel: the  channel itself is  reset to
    ;;inactive; so it  is available to start  receiving another message or  to send a
    ;;message.
    ;;
    (assert-receiving-channel __who__ this)
    (if (.message-terminated? this)
	(begin0
	    (.reverse-and-concatenate-buffer this)
	  (.recv-abort! this))
      (assertion-violation __who__
	"attempting to end reception of non-terminated message" this)))

  (method ({recv-full-message (or <eof> <bytevector>)})
    (assert-inactive-channel __who__ this)
    (.recv-begin! this)
    (let next-portion ()
      (let ((rv (.recv-message-portion! this)))
	(cond ((eof-object? rv)
	       (.recv-abort! this)
	       rv)
	      ((would-block-object? rv)
	       (next-portion))
	      ((not rv)
	       (next-portion))
	      (else
	       (.recv-end! this))))))

  (method ({recv-message-portion! (or <eof> <would-block> <boolean>)})
    ;;Receive a  portion of input message  from the channel.   It is an error  if the
    ;;channel is not in the course of receiving a message.
    ;;
    ;;* Return false if a portion of message was read, and it does not terminate with
    ;;a message  terminator; in  this case  we need  to call  this function  again to
    ;;receive further message portions.
    ;;
    ;;* Return true if a configured message terminator is read from the input port or
    ;;if the channel already read a terminator in a previous operation.  If a message
    ;;terminator is received: set THIS to "message terminated" status.
    ;;
    ;;* Return the EOF object if EOF is read from the input port.
    ;;
    ;;* Return the would-block object if a  would-block object is read from the input
    ;;port.
    ;;
    ;;* If the  message delivery timeout is expired or  expires while receiving data:
    ;;raise an exception.
    ;;
    ;;* If the accumulated data exceeds the maximum message size: raise an exception.
    ;;
    (assert-receiving-channel __who__ this)
    (cond
     ;;If the message is terminated: return now, ignore input data.
     ((.message-terminated? this)
      #t)
     ;;If the message is not terminated and the timeout expired: raise an error.
     ((.delivery-timeout-expired? this)
      (%error-message-delivery-timeout-expired __who__ this))
     (else
      ;;Read the next message portion.
      (let ((bv (get-bytevector-n (.connect-in-port this) (.maximum-message-portion-size this))))
	(cond
	 ;;If the EOF was read here: no new  data was available.  It means we got EOF
	 ;;before reading a message terminator: return the EOF object.
	 ((eof-object? bv)
	  bv)

	 ;;If a would-block object  was read here: no new data  was available and the
	 ;;port is configured in non-blocking mode.  Return now to signal the need to
	 ;;read further message portions later.
	 ((would-block-object? bv)
	  #!would-block)

	 ;;If  a message  portion is  read:  push it  on the  internal buffer;  check
	 ;;message  size  and timeout  expiration;  return  true  if the  message  is
	 ;;terminated, false otherwise.
	 (else
	  (.message-buffer-push! this bv)
	  (cond ((.maximum-size-exceeded? this)
		 (%error-maximum-message-size-exceeded __who__ this))
		((.delivery-timeout-expired? this)
		 (%error-message-delivery-timeout-expired __who__ this))
		((%received-binary-message-terminator? (.message-terminators this) (.message-buffer this))
		 (.message-terminated? this #t)
		 #t)
		(else #f))))))))

  #| end of mixin |# )


(define-mixin-type <receiving-textual-channel-methods>
  (implements <<textual-input-channel>>)

  (public
    (fields
      (immutable {connect-in-port <textual-input-port>})
		;An input or input/output textual port used to receive messages from a
		;remote process.
      #| end of FIELDS |# ))

  (protected
    (fields
      (mutable {message-buffer (list-of <nestring>)})
		;Null or a list of strings  representing the data accumulated so far;
		;last input first.
      #| end of FIELDS |# ))

  (public
    (fields
      (mutable {message-terminators <textual-terminators>})
		;A non-empty list of  non-empty strings representing possible message
		;terminators.
      #| end of FIELDS |# ))

  (mixins <receiving-channel-methods>)

;;; --------------------------------------------------------------------

  (protected
    (method ({message-buffer-push! <void>} {data <nestring>})
      (.message-buffer this (cons data (.message-buffer this)))
      (.message-increment-size! this (.length data))))

  (protected
    (method ({reverse-and-concatenate-buffer <nestring>})
      ($string-reverse-and-concatenate (.message-size this) (.message-buffer this))))

;;; --------------------------------------------------------------------

  (method ({recv-end!/rbl <positive-fixnum> (list-of <nestring>)})
    ;;Finish receiving a message; it is an error  if the channel is not in the course
    ;;of receiving a message.  Return the values:
    ;;
    ;;1. A positive fixnum representing the total data size.
    ;;
    ;;2. A  list of non-empty  strings representing  the data buffers  accumulated in
    ;;reverse order (reverse buffer list, RBL).  The data includes the terminator.
    ;;
    ;;After this  function is applied  to a channel: the  channel itself is  reset to
    ;;inactive; so it  is available to start  receiving another message or  to send a
    ;;message.
    ;;
    (assert-receiving-channel __who__ this)
    (begin0
	(values (.message-size   this)
		(.message-buffer this))
      (.recv-abort! this)))

  (method ({recv-end! <nestring>})
    ;;Finish receiving a message; it is an error  if the channel is not in the course
    ;;of receiving  a message.  Return  the accumulated  characters in a  string; the
    ;;data includes the terminator.
    ;;
    ;;After this  function is applied  to a channel: the  channel itself is  reset to
    ;;inactive; so it  is available to start  receiving another message or  to send a
    ;;message.
    ;;
    (assert-receiving-channel __who__ this)
    (if (.message-terminated? this)
	(begin0
	    (.reverse-and-concatenate-buffer this)
	  (.recv-abort! this))
      (assertion-violation __who__
	"attempting to end reception of non-terminated message" this)))

  (method ({recv-full-message (or <eof> <string>)})
    (assert-inactive-channel __who__ this)
    (.recv-begin! this)
    (let next-portion ()
      (let ((rv (.recv-message-portion! this)))
	(cond ((eof-object? rv)
	       (.recv-abort! this)
	       rv)
	      ((would-block-object? rv)
	       (next-portion))
	      ((not rv)
	       (next-portion))
	      (else
	       (.recv-end! this))))))

  (method ({recv-message-portion! (or <eof> <would-block> <boolean>)})
    ;;Receive a  portion of input message  from the channel.   It is an error  if the
    ;;channel is not in the course of receiving a message.
    ;;
    ;;* Return false if a portion of message was read, and it does not terminate with
    ;;a message  terminator; in  this case  we need  to call  this function  again to
    ;;receive further message portions.
    ;;
    ;;* Return true if a configured message terminator is read from the input port or
    ;;if the channel already read a terminator in a previous operation.  If a message
    ;;terminator is received: set THIS to "message terminated" status.
    ;;
    ;;* Return the EOF object if EOF is read from the input port.
    ;;
    ;;* Return the would-block object if a  would-block object is read from the input
    ;;port.
    ;;
    ;;* If the  message delivery timeout is expired or  expires while receiving data:
    ;;raise an exception.
    ;;
    ;;* If the accumulated data exceeds the maximum message size: raise an exception.
    ;;
    (assert-receiving-channel __who__ this)
    (cond
     ;;If the message is terminated: return now, ignore input data.
     ((.message-terminated? this)
      #t)
     ;;If the message is not terminated and the timeout expired: raise an error.
     ((.delivery-timeout-expired? this)
      (%error-message-delivery-timeout-expired __who__ this))
     (else
      ;;Read a new message portion.
      (let ((str (get-string-n (.connect-in-port this) (.maximum-message-portion-size this))))
	(cond
	 ;;If the EOF is found here: no new  data was available.  It means we got EOF
	 ;;before reading a message terminator: return the EOF object.
	 ((eof-object? str)
	  str)

	 ;;If a would-block object  was read here: no new data  was available and the
	 ;;port is configured in non-blocking mode.  Return now to signal the need to
	 ;;read further message portions later.
	 ((would-block-object? str)
	  #!would-block)

	 ;;If  a message  portion is  read:  push it  on the  internal buffer;  check
	 ;;message  size  and timeout  expiration;  return  true  if the  message  is
	 ;;terminated, false otherwise.
	 (else
	  (.message-buffer-push! this str)
	  (cond ((.maximum-size-exceeded? this)
		 (%error-maximum-message-size-exceeded __who__ this))
		((.delivery-timeout-expired? this)
		 (%error-message-delivery-timeout-expired __who__ this))
		((%received-textual-message-terminator? (.message-terminators this) (.message-buffer this))
		 (.message-terminated? this #t)
		 #t)
		(else #f))))))))

  #| end of mixin |# )


(define-mixin-type <sending-channel-methods>

  (method ({send-begin! <void>})
    ;;Configure a channel to start sending  a message; return unspecified values.  It
    ;;is an error if the channel is not inactive.
    ;;
    (assert-inactive-channel __who__ this)
    (.action this 'send)
    (void))

  (method ({send-end! <non-negative-fixnum>})
    ;;Finish sending a  message by flushing the connect port  and returning the total
    ;;number of octets or  characters sent.  It is an error if the  channel is not in
    ;;the course of sending a message.
    ;;
    ;;After this function  is applied to a channel: the  channel itself is configured
    ;;as inactive; so it is available to start receiving another message or to send a
    ;;message.
    ;;
    (assert-sending-channel __who__ this)
    (flush-output-port (.connect-ou-port this))
    (begin0
	(.message-size this)
      (.send-abort! this)))

  (method ({flush <void>})
    ;;Flush to the destination the data buffered in the underlying device.
    ;;
    (assert-sending-channel __who__ this)
    (if (.delivery-timeout-expired? this)
	(%error-message-delivery-timeout-expired __who__ this)
      (flush-output-port (.connect-ou-port this))))

  (method ({send-abort! <void>})
    ;;Abort  the  current  operation  and  reset  the  channel  to  inactive;  return
    ;;unspecified values.  Send and receive nothing.
    ;;
    (assert-sending-channel __who__ this)
    (.action              this 'none)
    (.message-size        this 0)
    (.message-terminated? this #f)
    (.expiration-time     this (faraway-time))
    (void))

  #| end of mixin |# )


(define-mixin-type <sending-binary-channel-methods>
  (implements <<binary-output-channel>>)

  (fields
    (immutable {connect-ou-port <binary-output-port>})
		;An output  or input/output binary  port used  to send messages  to a
		;remote process.
    #| end of FIELDS |# )

  (mixins <sending-channel-methods>)

  (method ({send-message-portion! <void>} {portion <bytevector>})
    ;;Send a portion of output message  through the given channel; return unspecified
    ;;values.  It  is an  error if  the channel  is not  in the  course of  sending a
    ;;message.
    ;;
    ;;The argument PORTION must be a bytevector representing the message portion.
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

  (method ({send-full-message <non-negative-fixnum>} . {message-portions (list-of <bytevector>)})
    ;;Send a  full message composed of  the given MESSAGE-PORTIONS; return  the total
    ;;number of  bytes or  characters sent.   It is an  error if  the channel  is not
    ;;inactive.
    ;;
    (assert-inactive-channel __who__ this)
    (.send-begin! this)
    (for-each-in-order (lambda ({portion <bytevector>})
			 (.send-message-portion! this portion))
      message-portions)
    (.send-end! this))

  #| end of mixin |# )


(define-mixin-type <sending-textual-channel-methods>
  (implements <<textual-output-channel>>)

  (fields
    (immutable {connect-ou-port <textual-output-port>})
		;An output  or input/output binary  port used  to send messages  to a
		;remote process.
    #| end of FIELDS |# )

  (mixins <sending-channel-methods>)

  (method ({send-message-portion! <void>} {portion <string>})
    ;;Send a portion of output message  through the given channel; return unspecified
    ;;values.  It  is an  error if  the channel  is not  in the  course of  sending a
    ;;message.
    ;;
    ;;The argument PORTION must be a string representing the message portion.
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

  (method ({send-full-message <non-negative-fixnum>} . {message-portions (list-of <string>)})
    ;;Send a  full message composed of  the given MESSAGE-PORTIONS; return  the total
    ;;number of  bytes or  characters sent.   It is an  error if  the channel  is not
    ;;inactive.
    ;;
    (assert-inactive-channel __who__ this)
    (.send-begin! this)
    (for-each-in-order (lambda ({portion <string>})
			 (.send-message-portion! this portion))
      message-portions)
    (.send-end! this))

  #| end of mixin |# )


(define-record-type <binary-input-only-channel>
  (nongenerative vicare:net:channels:<binary-input-only-channel>)
  (parent <channel>)

  (protocol
    (lambda (make-channel)
      (named-lambda make-binary-input-channel
	  ({_ <binary-input-only-channel>} {in-port <binary-input-port>})
	((make-channel DEFAULT-BINARY-MESSAGE-MAXIMUM-SIZE)
	 in-port				     ;connect-in-port
	 '()					     ;message-buffer
	 DEFAULT-BINARY-TERMINATORS		     ;message-terminators
	 DEFAULT-BINARY-MESSAGE-MAXIMUM-PORTION-SIZE ;maximum-message-portion-size
	 ))))

  (constructor-signature
    (lambda (<binary-input-port>) => (<binary-input-only-channel>)))

  (custom-printer
    (lambda ({this <binary-input-only-channel>} {port <textual-output-port>} subprinter)
      (display "#[<binary-input-only-channel>" port)
      (display " port=" port)	(display (.connect-in-port this) port)
      (display "]" port)))

  (mixins <receiving-binary-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <textual-input-only-channel>
  (nongenerative vicare:net:channels:<textual-input-only-channel>)
  (parent <channel>)

  (protocol
    (lambda (make-channel)
      (named-lambda make-textual-input-channel
	  ({_ <textual-input-only-channel>} {in-port <textual-input-port>})
	((make-channel DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-SIZE)
	 in-port				      ;connect-in-port
	 '()					      ;message-buffer
	 DEFAULT-TEXTUAL-TERMINATORS		      ;message-terminators
	 DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-PORTION-SIZE ;maximum-message-portion-size
	 ))))

  (constructor-signature
    (lambda (<textual-input-port>) => (<textual-input-only-channel>)))

  (custom-printer
    (lambda ({this <textual-input-only-channel>} {port <textual-output-port>} subprinter)
      (display "#[<textual-input-only-channel>" port)
      (display " port=" port)	(display (.connect-in-port this) port)
      (display "]" port)))

  (mixins <receiving-textual-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <binary-output-only-channel>
  (nongenerative vicare:net:channels:<binary-output-only-channel>)
  (parent <channel>)

  (protocol
    (lambda (make-channel)
      (named-lambda make-binary-output-channel
	  ({_ <binary-output-only-channel>} {ou-port <binary-output-port>})
	((make-channel DEFAULT-BINARY-MESSAGE-MAXIMUM-SIZE) ou-port))))

  (constructor-signature
    (lambda (<binary-output-port>) => (<binary-output-only-channel>)))

  (custom-printer
    (lambda ({this <binary-output-only-channel>} {port <textual-output-port>} subprinter)
      (display "#[<binary-output-only-channel>" port)
      (display " port=" port)	(display (.connect-ou-port this) port)
      (display "]" port)))

  (mixins <sending-binary-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <textual-output-only-channel>
  (nongenerative vicare:net:channels:<textual-output-only-channel>)
  (parent <channel>)

  (protocol
    (lambda (make-channel)
      (named-lambda make-textual-output-channel
	  ({_ <textual-output-only-channel>} {ou-port <textual-output-port>})
	((make-channel DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-SIZE) ou-port))))

  (constructor-signature
    (lambda (<textual-output-port>) => (<textual-output-only-channel>)))

  (custom-printer
    (lambda ({this <textual-output-only-channel>} {port <textual-output-port>} subprinter)
      (display "#[<textual-output-only-channel>" port)
      (display " port=" port)	(display (.connect-ou-port this) port)
      (display "]" port)))

  (mixins <sending-textual-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <binary-input/output-channel>
  (nongenerative vicare:net:channels:<binary-input/output-channel>)
  (parent <channel>)

  (protocol
    (lambda (make-channel)
      (named-lambda make-binary-input/output-channel
	  ({_ <binary-input/output-channel>}
	   {in-port <binary-input-port>}
	   {ou-port <binary-output-port>})
	((make-channel DEFAULT-BINARY-MESSAGE-MAXIMUM-SIZE)
	 in-port				     ;connect-in-port
	 '()					     ;message-buffer
	 DEFAULT-BINARY-TERMINATORS		     ;message-terminators
	 DEFAULT-BINARY-MESSAGE-MAXIMUM-PORTION-SIZE ;maximum-message-portion-size
	 ;;
	 ou-port ;connect-ou-port
	 ))))

  (constructor-signature
    (lambda (<binary-input-port> <binary-output-port>) => (<binary-input/output-channel>)))

  (custom-printer
    (lambda ({this <binary-input/output-channel>} {port <textual-output-port>} subprinter)
      (display "#[<binary-input/output-channel>" port)
      (display " in-port=" port)	(display (.connect-in-port this) port)
      (display " ou-port=" port)	(display (.connect-ou-port this) port)
      (display "]" port)))

  (mixins <receiving-binary-channel-methods> <sending-binary-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


(define-record-type <textual-input/output-channel>
  (nongenerative vicare:net:channels:<textual-input/output-channel>)
  (parent <channel>)

  (protocol
    (lambda (make-channel)
      (named-lambda make-textual-input/output-channel
	  ({_ <textual-input/output-channel>}
	   {in-port <textual-input-port>}
	   {ou-port <textual-output-port>})
	((make-channel DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-SIZE)
	 in-port				      ;connect-in-port
	 '()					      ;message-buffer
	 DEFAULT-TEXTUAL-TERMINATORS		      ;message-terminators
	 DEFAULT-TEXTUAL-MESSAGE-MAXIMUM-PORTION-SIZE ;maximum-message-portion-size
	 ;;
	 ou-port ;connect-ou-port
	 ))))

  (constructor-signature
    (lambda (<textual-input-port> <textual-output-port>) => (<textual-input/output-channel>)))

  (custom-printer
    (lambda ({this <textual-input/output-channel>} {port <textual-output-port>} subprinter)
      (display "#[<textual-input/output-channel>" port)
      (display " in-port=" port)	(display (.connect-in-port this) port)
      (display " ou-port=" port)	(display (.connect-ou-port this) port)
      (display "]" port)))

  (mixins <receiving-textual-channel-methods> <sending-textual-channel-methods>)

  #| end of DEFINE-RECORD-TYPE |# )


;;;; predicates and arguments validation

(define (assert-receiving-channel {who <symbol>} {chan <channel>})
  ;;Succeed  if CHAN  is  an instance  of  "<channel>" and  it is  in  the course  of
  ;;receiving a message.
  ;;
  (unless (.receiving? chan)
    (assertion-violation who "expected channel in the course of receving a message as argument" chan)))

(define (assert-sending-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN is an instance of "<channel>"  and it is in the course of sending
  ;;a message.
  ;;
  (unless (.sending? chan)
    (assertion-violation who "expected channel in the course of sending a message as argument" chan)))

(define (assert-inactive-channel {who <symbol>} {chan <channel>})
  ;;Succeed if CHAN is an instance of "<channel>"  and it is neither in the course of
  ;;sending nor receiving a message.
  ;;
  (unless (.inactive? chan)
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
   (condition (new &channel chan)
	      (new &delivery-timeout-expired)
	      (make-who-condition who)
	      (make-message-condition "message reception timeout expired"))))

(define ({%error-maximum-message-size-exceeded . <bottom>} {who <symbol>} {chan <channel>})
  ;;Raise a  non-continuable exception representing  the error: maximum  message size
  ;;exceeded.  The raised condition object  has components: &who, &message, &channel,
  ;;&maximum-message-size-exceeded.
  ;;
  (raise
   (condition (new &channel chan)
	      (new &maximum-message-size-exceeded)
	      (make-who-condition who)
	      (make-message-condition "message reception timeout expired"))))


;;;; receiving messages: binary message portion

(module (%received-binary-message-terminator?)

  (define (%received-binary-message-terminator? {terminators <binary-terminators>} {buffers (list-of <nebytevector>)})
    ;;Compare all  the message  TERMINATORS with the  bytevectors accumulated  in the
    ;;BUFFERS.   If the  tail of  the buffer  equals one  of the  terminators: return
    ;;non-false, else return false.
    ;;
    (vector-find (lambda ({_ <top>} terminator)
		   (%terminated-octets-stream? buffers terminator))
      terminators))

  (define ({%terminated-octets-stream? <top>} {reverse-stream (list-of <nebytevector>)} {terminator <nebytevector>})
    ;;Compare  a terminator  with the  tail of  an octets  stream; if  the stream  is
    ;;terminated return #t, else return #f.
    ;;
    ;;The argument TERMINATOR must be  a non-empty bytevector representing the stream
    ;;terminator;  the last  octet in  TERMINATOR  is the  last octet  in a  properly
    ;;terminated stream.
    ;;
    ;;The argument REVERSE-SEQUENCE  must be null or a list  of non-empty bytevectors
    ;;representing  the stream  of octects  in bytevector-reversed  order; as  if the
    ;;stream of octets has been accumulated (= CONSed) bytevector by bytevector:
    ;;
    ;;* The first item of REVERSE-SEQUENCE is  the last bytevector in the stream, the
    ;;last item of REVERSE-SEQUENCE is the first bytevector in the stream.
    ;;
    ;;* Every bytevector in REVERSE-SEQUENCE represents  a chunk of stream: the first
    ;;octet in the bytevector is the first octet  in the chunk, the last octet in the
    ;;bytevector is the last octet in the chunk.
    ;;
    (define ({%bytevector-last-index <fixnum>} {bv <bytevector>})
      (fxsub1 (.length bv)))
    (let loop ((terminator.idx  (%bytevector-last-index terminator))
	       (buffers         reverse-stream))
      (cond ((fx=? -1 terminator.idx)
	     #t)
	    ((null? buffers)
	     #f)
	    ((let* ((buf     (car buffers))
		    (buf.idx (%bytevector-last-index buf)))
	       (%compare-bytevector-tails terminator terminator.idx buf buf.idx))
	     => (lambda (terminator.idx)
		  (loop (fxsub1 terminator.idx) (cdr buffers))))
	    (else #f))))

  (define ({%compare-bytevector-tails (or <false> <non-negative-fixnum>)}
	   {A <bytevector>} {A.idx <non-negative-fixnum>}
	   {B <bytevector>} {B.idx <non-negative-fixnum>})
    ;;Recursive  function.   Compare the  bytevector  A,  starting from  index  A.idx
    ;;inclusive, to the bytevector B, starting from index B.idx inclusive.  If:
    ;;
    ;;* All the octets are equal up to (zero?  A.idx) included: return 0.  An example
    ;;of this case is a call with arguments:
    ;;
    ;;   A =       #vu8(3 4 5)	A.idx = 2
    ;;   B = #vu8(0 1 2 3 4 5)	B.idx = 5
    ;;
    ;;another example of this case:
    ;;
    ;;   A = #vu8(0 1 2 3 4 5)	A.idx = 5
    ;;   B = #vu8(0 1 2 3 4 5)	B.idx = 5
    ;;
    ;;* All the octets  are equal up to (zero?  B.idx) included:  return the value of
    ;;A.idx referencing the last  compared octet in A.  An example of  this case is a
    ;;call with arguments:
    ;;
    ;;   A = #vu8(0 1 2 3 4 5)	A.idx = 5
    ;;   B =       #vu8(3 4 5)	B.idx = 2
    ;;
    ;;the returned value is: A.idx == 3.
    ;;
    ;;*  Octects having  (positive?   A.idx) and  (positive?   B.idx) are  different:
    ;;return false.
    ;;
    (and (fx=? (bytevector-u8-ref A A.idx)
	       (bytevector-u8-ref B B.idx))
	 (cond ((fxzero? A.idx)
		0)
	       ((fxzero? B.idx)
		A.idx)
	       (else
		(%compare-bytevector-tails A (fxsub1 A.idx)
					   B (fxsub1 B.idx))))))

  #| end of module: %RECEIVED-MESSAGE-TERMINATOR? |# )


;;;; receiving messages: textual message portion

(module (%received-textual-message-terminator?)

  (define (%received-textual-message-terminator? {terminators <textual-terminators>} {buffers (list-of <nestring>)})
    ;;Compare  all  the message  TERMINATORS  with  the  strings accumulated  in  the
    ;;BUFFERS.   If the  tail of  the buffer  equals one  of the  terminators: return
    ;;non-false, else return false.
    ;;
    (vector-find (lambda ({_ <top>} terminator)
		   (%terminated-chars-stream? buffers terminator))
      terminators))

  (define ({%terminated-chars-stream? <top>} {reverse-stream (list-of <nestring>)} {terminator <nestring>})
    ;;Compare  a terminator  with the  tail  of an  chars  stream; if  the stream  is
    ;;terminated return #t, else return #f.
    ;;
    ;;The  argument TERMINATOR  must be  a non-empty  string representing  the stream
    ;;terminator;  the  last char  in  TERMINATOR  is the  last  char  in a  properly
    ;;terminated stream.
    ;;
    ;;The  argument REVERSE-SEQUENCE  must be  null or  a list  of non-empty  strings
    ;;representing the stream of chars in  string-reversed order; as if the stream of
    ;;chars has been accumulated (= CONSed) string by string:
    ;;
    ;;* The first item of REVERSE-SEQUENCE is the last string in the stream, the last
    ;;item of REVERSE-SEQUENCE is the first string in the stream.
    ;;
    ;;* Every string in REVERSE-SEQUENCE represents a chunk of stream: the first char
    ;;in the string  is the first char in  the chunk, the last char in  the string is
    ;;the last char in the chunk.
    ;;
    (define ({%string-last-index <fixnum>} {str <nestring>})
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
	   {A <nestring>} {A.idx <non-negative-fixnum>}
	   {B <nestring>} {B.idx <non-negative-fixnum>})
    ;;Recursive function.  Compare the string A, starting from index A.idx inclusive,
    ;;to the string B, starting from index B.idx inclusive.  If:
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


;;;; done

#| end of library |# )

;;; end of file
