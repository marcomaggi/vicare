;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for close-on-exec ports
;;;Date: Fri May 17, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (prefix (vicare posix) px.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: POSIX close-on-exec ports\n")


;;;; helpers

(define (make-binary-ports)
  (receive (in ou)
      (px.pipe)
    (let ((inp (make-binary-file-descriptor-input-port  in "in"))
	  (oup (make-binary-file-descriptor-output-port ou "ou")))
      (push-compensation (close-port inp))
      (push-compensation (close-port oup))
      (px.port-set-close-on-exec-mode! inp)
      (px.port-set-close-on-exec-mode! oup)
      (values inp oup))))


(parametrise ((check-test-name	'table))

  (check	;setting, testing
      (with-compensations
	(receive (inp oup)
	    (make-binary-ports)
	  (list (px.port-in-close-on-exec-mode? inp)
		(px.port-in-close-on-exec-mode? oup))))
    => '(#t #t))

  (check	;setting, unsetting, testing
      (with-compensations
	(receive (inp oup)
	    (make-binary-ports)
	  (px.port-unset-close-on-exec-mode! inp)
	  (px.port-unset-close-on-exec-mode! oup)
	  (list (px.port-in-close-on-exec-mode? inp)
		(px.port-in-close-on-exec-mode? oup))))
    => '(#f #f))

;;; --------------------------------------------------------------------

  (check	;setting, flushing, closing all
      (with-compensations
	(receive (inp oup)
	    (make-binary-ports)
	  (px.flush-ports-in-close-on-exec-mode)
	  (px.close-ports-in-close-on-exec-mode)
	  (list (port-closed? inp)
		(port-closed? oup))))
    => '(#t #t))

  (check	;setting, unsetting, closing all
      (with-compensations
	(receive (inp oup)
	    (make-binary-ports)
	  (px.port-unset-close-on-exec-mode! inp)
	  (px.port-unset-close-on-exec-mode! oup)
	  (px.close-ports-in-close-on-exec-mode)
	  (list (port-closed? inp)
		(port-closed? oup)
		(px.port-in-close-on-exec-mode? inp)
		(px.port-in-close-on-exec-mode? oup))))
    => '(#f #f #f #f))

  #t)


(parametrise ((check-test-name		'errors)
	      (string-port-buffer-size	16))

  (define (trace template . args)
    (when #t
      (apply fprintf (current-error-port) template args)))

  (define error-on-write
    (make-parameter #f))

  (define (make-test-port)
    ;;Create a port that wraps the one created by OPEN-STRING-OUTPUT-PORT.
    ;;
    (receive (subport extract)
	(open-string-output-port)

      (define (write! src.str src.start count)
	(trace "writing ~a chars\n" count)
	(when (error-on-write)
	  (error __who__ "error writing characters" count))
	(do ((i 0 (+ 1 i)))
	    ((= i count)
	     count)
	  (put-char subport (string-ref src.str (+ i src.start)))))

      (define (get-position)
	(port-position subport))

      (define (set-position! new-position)
	(set-port-position! subport new-position))

      (define (close)
	#f)

      (values (make-custom-textual-output-port
	       "*test-port*" write! get-position set-position! close)
	      extract)))

  (define-constant the-string-len
    (* 4 (string-port-buffer-size)))

  (define-constant the-string
    (make-string the-string-len #\A))

;;; --------------------------------------------------------------------

  (check	;no error, no close-on-exec
      (let-values (((port extract) (make-test-port)))
	(trace "writing ~a chars\n" the-string-len)
	(display the-string port)
	(trace "flushing output\n")
	(flush-output-port port)
	(trace "closing\n")
	(close-port port)
	(receive-and-return (rv)
	    (extract)
	  (trace "full contents: ~s\n" rv)))
    => the-string)

  (check	;no error, close-on-exec
      (let-values (((port extract) (make-test-port)))
	(px.port-set-close-on-exec-mode! port)
	(trace "writing ~a chars\n" the-string-len)
	(display the-string port)
	(trace "flushing output\n")
	(px.flush-ports-in-close-on-exec-mode (lambda (E)
						(debug-print E)))
	(trace "closing\n")
	(px.close-ports-in-close-on-exec-mode port)
	(receive-and-return (rv)
	    (extract)
	  (trace "full contents: ~s\n" rv)))
    => the-string)

  (check	;error on flushing, close-on-exec
      (with-result
	(let-values (((port extract) (make-test-port)))
	  (px.port-set-close-on-exec-mode! port)
	  (trace "writing ~a chars\n" the-string-len)
	  (add-result 'writing)
	  (display the-string port)
	  (trace "flushing output\n")
	  (add-result 'flushing)
	  (parametrise ((error-on-write #t))
	    (px.flush-ports-in-close-on-exec-mode (lambda (E)
						    (trace "flush exception: ~s\n" E))))
	  (trace "closing\n")
	  (add-result 'closing)
	  (px.close-ports-in-close-on-exec-mode port)
	  (receive-and-return (rv)
	      (string-length (extract))
	    (trace "full contents: ~s\n" rv))))
    => `(,(- the-string-len (string-port-buffer-size)) (writing flushing closing)))

  (check	;error on closing, close-on-exec
      (with-result
	(let-values (((port extract) (make-test-port)))
	  (px.port-set-close-on-exec-mode! port)
	  (trace "writing ~a chars\n" the-string-len)
	  (add-result 'writing)
	  (display the-string port)
	  (trace "closing\n")
	  (add-result 'closing)
	  (parametrise ((error-on-write #t))
	    (px.close-ports-in-close-on-exec-mode (lambda (E)
						    (trace "close exception: ~s\n" E))))
	  (receive-and-return (rv)
	      (string-length (extract))
	    (trace "full contents: ~s\n" rv))))
    => `(,(- the-string-len (string-port-buffer-size)) (writing closing)))

  #t)


;;;; done

(check-report)

;;; end of file
