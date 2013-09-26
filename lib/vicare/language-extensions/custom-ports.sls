;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: custom ports library
;;;Date: Sat Apr  6, 2013
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
(library (vicare language-extensions custom-ports)
  (export
    open-binary-input-port-pair
    open-binary-output-port-pair
    open-binary-input/output-port-pair

    open-textual-input-port-pair
    open-textual-output-port-pair
    open-textual-input/output-port-pair)
  (import (vicare)
    (vicare unsafe operations))


;;;; input and output binary port pairs

(define (open-binary-output-port-pair)
  (let-values (((in-port ou-port)
		(open-binary-input-port-pair)))
    (values ou-port in-port)))

(define (open-binary-input-port-pair)
  (let-values (((ou->in.empty? ou->in.enqueue! ou->in.dequeue!) (make-queue-procs)))

    ;;Output port to input port device.
    (define ou->in.device.bv '#vu8())
    (define ou->in.device.start 0)
    (define ou->in.device.count 0)

    (define (in-port.read! in.buffer.bv in.buffer.start in.buffer.count)
      ;;Move data from  the OU->IN device and queue to  the input port's
      ;;buffer.
      ;;
      ;;The arguments select a portion of input port's buffer.
      ;;
      (let loop ((in.buffer.read  0)
		 (in.buffer.count in.buffer.count))
	(if ($fx< in.buffer.count ou->in.device.count)
	    ;;The request can  be satisfied with octets  from the OU->IN
	    ;;device alone, without popping  bytevectors from the OU->IN
	    ;;queue.
	    ;;
	    (begin
	      ($bytevector-copy!/count ou->in.device.bv ou->in.device.start
				       in.buffer.bv     in.buffer.start
				       in.buffer.count)
	      (set! ou->in.device.start ($fx+ ou->in.device.start in.buffer.count))
	      (set! ou->in.device.count ($fx- ou->in.device.count in.buffer.count))
	      ($fx+ in.buffer.read in.buffer.count))
	  ;;To  satisfy the  requests we  consume  all the  data in  the
	  ;;OU->IN device and  also pop the next buffer  from the OU->IN
	  ;;queue.
	  ;;
	  (begin
	    ($bytevector-copy!/count ou->in.device.bv ou->in.device.start
				     in.buffer.bv     in.buffer.start
				     ou->in.device.count)
	    (if (ou->in.empty?)
		;;No more buffers in the OU->IN queue.
		(begin0
		    ;;Use  OU->IN.DEVICE.COUNT  before resetting  it  to
		    ;;zero!!!
		    ($fx+ in.buffer.read ou->in.device.count)
		  (set! ou->in.device.bv '#vu8())
		  (set! ou->in.device.start 0)
		  (set! ou->in.device.count 0))
	      ;;Pop the next buffer from the OU->IN queue.
	      (let ((count ou->in.device.count))
		(set! ou->in.device.bv (ou->in.dequeue!))
		(set! ou->in.device.start 0)
		(set! ou->in.device.count ($bytevector-length ou->in.device.bv))
		(loop ($fx+ in.buffer.read  count)
		      ($fx- in.buffer.count count))))))))

    (define (ou-port.write! ou.buffer.bv ou.buffer.start ou.buffer.count)
      ;;Move data from output port's buffer to OU->IN queue.
      ;;
      ;;The arguments select  a portion of output  port's output buffer.
      ;;Consume all the selected octets  from the output buffer and push
      ;;a new bytevector on the OU->IN queue.
      ;;
      (let ((buffer (make-bytevector ou.buffer.count)))
	($bytevector-copy!/count ou.buffer.bv ou.buffer.start buffer 0 ou.buffer.count)
	(ou->in.enqueue! buffer)
	ou.buffer.count))

;;; --------------------------------------------------------------------

    (define in-port
      (make-custom-binary-input-port
       "binary-input-port-pair.input-port"
       in-port.read! #f #f #f))

    (define ou-port
      (make-custom-binary-output-port
       "binary-input-port-pair.output-port"
       ou-port.write! #f #f #f))

    (values in-port ou-port)))


;;;; input/output binary port pairs

(define (open-binary-input/output-port-pair)
  (let-values (((one->two.empty? one->two.enqueue! one->two.dequeue!) (make-queue-procs))
	       ((two->one.empty? two->one.enqueue! two->one.dequeue!) (make-queue-procs)))

    ;;Device used to transfer data from port ONE to port TWO.
    (define one->two.device.bv '#vu8())
    (define one->two.device.start 0)
    (define one->two.device.count 0)

    ;;Device used to transfer data from port TWO to port ONE.
    (define two->one.device.bv '#vu8())
    (define two->one.device.start 0)
    (define two->one.device.count 0)

;;; --------------------------------------------------------------------
;;; acting on TWO->ONE.DEVICE

    (define (one.read! one.buffer.bv one.buffer.start one.buffer.count)
      ;;Move data from the TWO->ONE device into port ONE's input buffer.
      ;;The arguments select a portion of port ONE's input buffer.
      ;;
      (let loop ((one.buffer.read  0)
		 (one.buffer.count one.buffer.count))
	(if ($fx< one.buffer.count two->one.device.count)
	    ;;The request can be satisfied  with bytes from the TWO->ONE
	    ;;device, without popping a buffer from the TWO->ONE queue.
	    ;;
	    (begin
	      ($bytevector-copy!/count two->one.device.bv two->one.device.start
				       one.buffer.bv      one.buffer.start
				       one.buffer.count)
	      (set! two->one.device.start ($fx+ two->one.device.start one.buffer.count))
	      (set! two->one.device.count ($fx- two->one.device.count one.buffer.count))
	      ($fx+ one.buffer.read one.buffer.count))
	  ;;To  satisfy the  requests we  consume  all the  data in  the
	  ;;TWO->ONE  device  and also  pop  the  next buffer  from  the
	  ;;TWO->ONE queue.
	  ;;
	  (begin
	    ;;Copy all the  data avaiable in the TWO->ONE  device to the
	    ;;buffer of port ONE.
	    ($bytevector-copy!/count two->one.device.bv two->one.device.start
				     one.buffer.bv      one.buffer.start
				     two->one.device.count)
	    (if (two->one.empty?)
		;;No more buffers in the TWO->ONE queue.
		(begin0
		    ;;Use TWO->ONE.DEVICE.COUNT  before resetting  it to
		    ;;zero!!!
		    ($fx+ one.buffer.read two->one.device.count)
		  (set! two->one.device.bv    '#vu8())
		  (set! two->one.device.start 0)
		  (set! two->one.device.count 0))
	      ;;Pop the next buffer from the TWO->ONE queue.
	      (let ((count two->one.device.count))
		(set! two->one.device.bv (two->one.dequeue!))
		(set! two->one.device.start 0)
		(set! two->one.device.count ($bytevector-length two->one.device.bv))
		(loop ($fx+ one.buffer.read  count)
		      ($fx- one.buffer.count count))))))))

    (define (two.write! two.buffer.bv two.buffer.start two.buffer.count)
      ;;Move data from port TWO's output buffer to TWO->ONE queue.
      ;;
      ;;The  arguments select  a portion  of port  TWO's output  buffer.
      ;;Consume all the selected octets  from the output buffer and push
      ;;a new bytevector on the TWO->ONE queue.
      ;;
      (let ((buffer (make-bytevector two.buffer.count)))
	($bytevector-copy!/count two.buffer.bv two.buffer.start buffer 0 two.buffer.count)
	(two->one.enqueue! buffer)
	two.buffer.count))

;;; --------------------------------------------------------------------
;;; acting on ONE->TWO.DEVICE

    (define (two.read! two.buffer.bv two.buffer.start two.buffer.count)
      ;;Move data from the ONE->TWO device into port TWO's input buffer.
      ;;The arguments select a portion of port TWO's input buffer.
      ;;
      (let loop ((two.buffer.read  0)
		 (two.buffer.count two.buffer.count))
	(if ($fx< two.buffer.count one->two.device.count)
	    ;;The request can be satisfied  with bytes from the ONE->TWO
	    ;;device, without popping a buffer from the ONE->TWO queue.
	    ;;
	    (begin
	      ($bytevector-copy!/count one->two.device.bv one->two.device.start
				       two.buffer.bv two.buffer.start
				       two.buffer.count)
	      (set! one->two.device.start ($fx+ one->two.device.start two.buffer.count))
	      (set! one->two.device.count ($fx- one->two.device.count two.buffer.count))
	      ($fx+ two.buffer.read two.buffer.count))
	  ;;To  satisfy the  requests we  consume  all the  data in  the
	  ;;ONE->TWO  device  and also  pop  the  next buffer  from  the
	  ;;ONE->TWO queue.
	  ;;
	  (begin
	    ($bytevector-copy!/count one->two.device.bv one->two.device.start
				     two.buffer.bv two.buffer.start
				     one->two.device.count)
	    (if (one->two.empty?)
		;;No more buffers in the ONE->TWO queue.
		(begin0
		    ;;Use ONE->TWO.DEVICE.COUNT  before resetting  it to
		    ;;zero!!!
		    ($fx+ two.buffer.read one->two.device.count)
		  (set! one->two.device.bv '#vu8())
		  (set! one->two.device.start 0)
		  (set! one->two.device.count 0))
	      ;;Pop the next buffer from the ONE->TWO queue.
	      (let ((count one->two.device.count))
		(set! one->two.device.bv (one->two.dequeue!))
		(set! one->two.device.start 0)
		(set! one->two.device.count ($bytevector-length one->two.device.bv))
		(loop ($fx+ two.buffer.read  count)
		      ($fx- two.buffer.count count))))))))

    (define (one.write! one.buffer.bv one.buffer.start one.buffer.count)
      ;;Move data from port ONE's output buffer to ONE->TWO queue.
      ;;
      ;;The  arguments select  a portion  of port  ONE's output  buffer.
      ;;Consume all the selected octets  from the output buffer and push
      ;;a new bytevector on the ONE->TWO queue.
      ;;
      (let ((buffer (make-bytevector one.buffer.count)))
	($bytevector-copy!/count one.buffer.bv one.buffer.start buffer 0 one.buffer.count)
	(one->two.enqueue! buffer)
	one.buffer.count))

;;; --------------------------------------------------------------------

    (define one.port
      (make-custom-binary-input/output-port
       "binary-input/output-port-pair.one.port"
       one.read! one.write! #f #f #f))

    (define two.port
      (make-custom-binary-input/output-port
       "binary-input/output-port-pair.two.port"
       two.read! two.write! #f #f #f))

    (values one.port two.port)))


;;;; input and output textual port pairs

(define (open-textual-output-port-pair)
  (let-values (((in-port ou-port)
		(open-textual-input-port-pair)))
    (values ou-port in-port)))

(define (open-textual-input-port-pair)
  (let-values (((ou->in.empty? ou->in.enqueue! ou->in.dequeue!) (make-queue-procs)))

    ;;Output port to input port device.
    (define ou->in.device.str '#vu8())
    (define ou->in.device.start 0)
    (define ou->in.device.count 0)

    (define (in-port.read! in.buffer.str in.buffer.start in.buffer.count)
      ;;Move data from  the OU->IN device and queue to  the input port's
      ;;buffer.
      ;;
      ;;The arguments select a portion of input port's buffer.
      ;;
      (let loop ((in.buffer.read  0)
		 (in.buffer.count in.buffer.count))
	(if ($fx< in.buffer.count ou->in.device.count)
	    ;;The request can  be satisfied with octets  from the OU->IN
	    ;;device  alone, without  popping  buffers  from the  OU->IN
	    ;;queue.
	    ;;
	    (begin
	      ($string-copy!/count ou->in.device.str ou->in.device.start
				   in.buffer.str     in.buffer.start
				   in.buffer.count)
	      (set! ou->in.device.start ($fx+ ou->in.device.start in.buffer.count))
	      (set! ou->in.device.count ($fx- ou->in.device.count in.buffer.count))
	      ($fx+ in.buffer.read in.buffer.count))
	  ;;To  satisfy the  requests we  consume  all the  data in  the
	  ;;OU->IN device and  also pop the next buffer  from the OU->IN
	  ;;queue.
	  ;;
	  (begin
	    ($string-copy!/count ou->in.device.str ou->in.device.start
				 in.buffer.str     in.buffer.start
				 ou->in.device.count)
	    (if (ou->in.empty?)
		;;No more buffers in the OU->IN queue.
		(begin0
		    ;;Use  OU->IN.DEVICE.COUNT  before resetting  it  to
		    ;;zero!!!
		    ($fx+ in.buffer.read ou->in.device.count)
		  (set! ou->in.device.str '#vu8())
		  (set! ou->in.device.start 0)
		  (set! ou->in.device.count 0))
	      ;;Pop the next buffer from the OU->IN queue.
	      (let ((count ou->in.device.count))
		(set! ou->in.device.str (ou->in.dequeue!))
		(set! ou->in.device.start 0)
		(set! ou->in.device.count ($string-length ou->in.device.str))
		(loop ($fx+ in.buffer.read  count)
		      ($fx- in.buffer.count count))))))))

    (define (ou-port.write! ou.buffer.str ou.buffer.start ou.buffer.count)
      ;;Move data from output port's buffer to OU->IN queue.
      ;;
      ;;The arguments select  a portion of output  port's output buffer.
      ;;Consume all the selected octets  from the output buffer and push
      ;;a new string on the OU->IN queue.
      ;;
      (let ((buffer (make-string ou.buffer.count)))
	($string-copy!/count ou.buffer.str ou.buffer.start buffer 0 ou.buffer.count)
	(ou->in.enqueue! buffer)
	ou.buffer.count))

;;; --------------------------------------------------------------------

    (define in-port
      (make-custom-textual-input-port
       "textual-input-port-pair.input-port"
       in-port.read! #f #f #f))

    (define ou-port
      (make-custom-textual-output-port
       "textual-input-port-pair.output-port"
       ou-port.write! #f #f #f))

    (values in-port ou-port)))



;;;; input/output textual port pairs

(define (open-textual-input/output-port-pair)
  (let-values (((one->two.empty? one->two.enqueue! one->two.dequeue!) (make-queue-procs))
	       ((two->one.empty? two->one.enqueue! two->one.dequeue!) (make-queue-procs)))

    ;;Device used to transfer data from port ONE to port TWO.
    (define one->two.device.str '#vu8())
    (define one->two.device.start 0)
    (define one->two.device.count 0)

    ;;Device used to transfer data from port TWO to port ONE.
    (define two->one.device.str '#vu8())
    (define two->one.device.start 0)
    (define two->one.device.count 0)

;;; --------------------------------------------------------------------
;;; acting on TWO->ONE.DEVICE

    (define (one.read! one.buffer.str one.buffer.start one.buffer.count)
      ;;Move data from the TWO->ONE device into port ONE's input buffer.
      ;;The arguments select a portion of port ONE's input buffer.
      ;;
      (let loop ((one.buffer.read  0)
		 (one.buffer.count one.buffer.count))
	(if ($fx< one.buffer.count two->one.device.count)
	    ;;The request can be satisfied  with bytes from the TWO->ONE
	    ;;device, without popping a buffer from the TWO->ONE queue.
	    ;;
	    (begin
	      ($string-copy!/count two->one.device.str two->one.device.start
				   one.buffer.str one.buffer.start
				   one.buffer.count)
	      (set! two->one.device.start ($fx+ two->one.device.start one.buffer.count))
	      (set! two->one.device.count ($fx- two->one.device.count one.buffer.count))
	      ($fx+ one.buffer.read one.buffer.count))
	  ;;To  satisfy the  requests we  consume  all the  data in  the
	  ;;TWO->ONE  device  and also  pop  the  next buffer  from  the
	  ;;TWO->ONE queue.
	  ;;
	  (begin
	    ;;Copy all the  data avaiable in the TWO->ONE  device to the
	    ;;buffer of port ONE.
	    ($string-copy!/count two->one.device.str two->one.device.start
				 one.buffer.str one.buffer.start
				 two->one.device.count)
	    (if (two->one.empty?)
		;;No more buffers in the TWO->ONE queue.
		(begin0
		    ;;Use TWO->ONE.DEVICE.COUNT  before resetting  it to
		    ;;zero!!!
		    ($fx+ one.buffer.read two->one.device.count)
		  (set! two->one.device.str '#vu8())
		  (set! two->one.device.start 0)
		  (set! two->one.device.count 0))
	      ;;Pop the next buffer from the TWO->ONE queue.
	      (let ((count two->one.device.count))
		(set! two->one.device.str (two->one.dequeue!))
		(set! two->one.device.start 0)
		(set! two->one.device.count ($string-length two->one.device.str))
		(loop ($fx+ one.buffer.read  count)
		      ($fx- one.buffer.count count))))))))

    (define (two.write! two.buffer.str two.buffer.start two.buffer.count)
      ;;Move data from port TWO's output buffer to TWO->ONE queue.
      ;;
      ;;The  arguments select  a portion  of port  TWO's output  buffer.
      ;;Consume all the selected octets  from the output buffer and push
      ;;a new string on the TWO->ONE queue.
      ;;
      (let ((buffer (make-string two.buffer.count)))
	($string-copy!/count two.buffer.str two.buffer.start buffer 0 two.buffer.count)
	(two->one.enqueue! buffer)
	two.buffer.count))

;;; --------------------------------------------------------------------
;;; acting on ONE->TWO.DEVICE

    (define (two.read! two.buffer.str two.buffer.start two.buffer.count)
      ;;Move data from the ONE->TWO device into port TWO's input buffer.
      ;;The arguments select a portion of port TWO's input buffer.
      ;;
      (let loop ((two.buffer.read  0)
		 (two.buffer.count two.buffer.count))
	(if ($fx< two.buffer.count one->two.device.count)
	    ;;The request can be satisfied  with bytes from the ONE->TWO
	    ;;device, without popping a buffer from the ONE->TWO queue.
	    ;;
	    (begin
	      ($string-copy!/count one->two.device.str one->two.device.start
				   two.buffer.str two.buffer.start
				   two.buffer.count)
	      (set! one->two.device.start ($fx+ one->two.device.start two.buffer.count))
	      (set! one->two.device.count ($fx- one->two.device.count two.buffer.count))
	      ($fx+ two.buffer.read two.buffer.count))
	  ;;To  satisfy the  requests we  consume  all the  data in  the
	  ;;ONE->TWO  device  and also  pop  the  next buffer  from  the
	  ;;ONE->TWO queue.
	  ;;
	  (begin
	    ($string-copy!/count one->two.device.str one->two.device.start
				 two.buffer.str two.buffer.start
				 one->two.device.count)
	    (if (one->two.empty?)
		;;No more buffers in the ONE->TWO queue.
		(begin0
		    ;;Use ONE->TWO.DEVICE.COUNT  before resetting  it to
		    ;;zero!!!
		    ($fx+ two.buffer.read one->two.device.count)
		  (set! one->two.device.str '#vu8())
		  (set! one->two.device.start 0)
		  (set! one->two.device.count 0))
	      ;;Pop the next buffer from the ONE->TWO queue.
	      (let ((count one->two.device.count))
		(set! one->two.device.str (one->two.dequeue!))
		(set! one->two.device.start 0)
		(set! one->two.device.count ($string-length one->two.device.str))
		(loop ($fx+ two.buffer.read  count)
		      ($fx- two.buffer.count count))))))))

    (define (one.write! one.buffer.str one.buffer.start one.buffer.count)
      ;;Move data from port ONE's output buffer to ONE->TWO queue.
      ;;
      ;;The  arguments select  a portion  of port  ONE's output  buffer.
      ;;Consume all the selected octets  from the output buffer and push
      ;;a new string on the ONE->TWO queue.
      ;;
      (let ((buffer (make-string one.buffer.count)))
	($string-copy!/count one.buffer.str one.buffer.start buffer 0 one.buffer.count)
	(one->two.enqueue! buffer)
	one.buffer.count))

;;; --------------------------------------------------------------------

    (define one.port
      (make-custom-textual-input/output-port
       "textual-input/output-port-pair.one.port"
       one.read! one.write! #f #f #f))

    (define two.port
      (make-custom-textual-input/output-port
       "textual-input/output-port-pair.two.port"
       two.read! two.write! #f #f #f))

    (values one.port two.port)))


;;;; done

)

;;; end of file
