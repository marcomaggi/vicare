;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for file descriptors in non-blocking mode
;;;Date: Sun May 12, 2013
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
(import (vicare)
  (prefix (vicare posix) px.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: POSIX file descriptor in non-blocking mode\n")


;;;; helpers

(define (make-pipe)
  (receive (in ou)
      (px.pipe)
    (push-compensation (px.close in))
    (push-compensation (px.close ou))
    (px.fd-set-non-blocking in)
    (px.fd-set-non-blocking ou)
    (values in ou)))

(define (make-binary-ports)
  (receive (in ou)
      (make-pipe)
    (let ((inp (make-binary-file-descriptor-input-port*  in "in"))
	  (oup (make-binary-file-descriptor-output-port* ou "ou")))
      (values inp oup))))

(define (make-textual-ports)
  (receive (in ou)
      (make-pipe)
    (let ((inp (make-textual-file-descriptor-input-port*  in "in" (native-transcoder)))
	  (oup (make-textual-file-descriptor-output-port* ou "ou" (native-transcoder))))
      (values inp oup))))

(define-constant CJK-COMPATIBILITY-IDEOGRAPH-2F9D1-UTF-8
  '#vu8(#xF0 #xAF #xA7 #x91))


(parametrise ((check-test-name	'fd))

  ;;Reading from file descriptors with available bytes
  (check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-textual-file-descriptor-input-port* in "in" (native-transcoder))))
	    (px.write ou '#ve(ascii "ciao") 4)
	    (receive-and-return (bv)
		(make-bytevector 4)
	      (px.read in bv)))))
    => '#ve(ascii "ciao"))

;;; --------------------------------------------------------------------

  ;;Reading file descriptor without available bytes causes EAGAIN.
  (check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (guard (E ((i/o-eagain-error? E)
		     #t)
		    (else E))
	    (px.read in (make-bytevector 4)))))
    => #t)

  #t)


(parametrise ((check-test-name	'port-binary))

  ;;Reading from binary ports with available bytes: GET-U8.
  (check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-binary-file-descriptor-input-port* in "in")))
	    (px.write ou '#ve(ascii "ciao") 4)
	    (get-u8 P))))
    => (char->integer #\c))

  ;;Reading from binary ports with available bytes: LOOKAHEAD-U8.
  (check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-binary-file-descriptor-input-port* in "in")))
	    (px.write ou '#ve(ascii "ciao") 4)
	    (lookahead-u8 P))))
    => (char->integer #\c))

  ;;Reading from  binary ports  with available  bytes: GET-BYTEVECTOR-N.
  ;;Ask exactly as available.
  (check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-binary-file-descriptor-input-port* in "in")))
	    (px.write ou '#ve(ascii "ciao") 4)
	    (get-bytevector-n P 4))))
    => '#ve(ascii "ciao"))

  ;;Reading from  binary ports  with available  bytes: GET-BYTEVECTOR-N.
  ;;Ask more than available.
  (check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-binary-file-descriptor-input-port* in "in")))
	    (px.write ou '#ve(ascii "ciao"))
	    (get-bytevector-n P 20))))
    => '#ve(ascii "ciao"))

  ;;Reading from  binary ports with available  bytes: GET-BYTEVECTOR-N!.
  ;;Ask exactly as available.
  (check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P  (make-binary-file-descriptor-input-port* in "in")))
	    (px.write ou '#ve(ascii "ciao"))
	    (let* ((bv (make-bytevector 4))
		   (rv (get-bytevector-n! P bv 0 4)))
	      (list rv bv)))))
    => '(4 #ve(ascii "ciao")))

  ;;Reading from  binary ports with available  bytes: GET-BYTEVECTOR-N!.
  ;;Ask more than available.
  (check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-binary-file-descriptor-input-port* in "in")))
	    (px.write ou '#ve(ascii "ciao"))
	    (let* ((bv (make-bytevector 10 0))
		   (rv (get-bytevector-n! P bv 0 10)))
	      (list rv bv)))))
    => `(4 #vu8(99 105 97 111 0 0 0 0 0 0)))

  ;;Reading from binary ports with available bytes: GET-BYTEVECTOR-ALL.
  #;(check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-binary-file-descriptor-input-port* in "in")))
	    (px.write ou '#ve(ascii "ciao") 4)
	    (get-bytevector-all P))))
    => '#ve(ascii "ciao"))

  ;;Reading from binary ports with available bytes: GET-BYTEVECTOR-SOME.
  #;(check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-binary-file-descriptor-input-port* in "in")))
	    (px.write ou '#ve(ascii "ciao") 4)
	    (get-bytevector-some P))))
    => '#ve(ascii "ciao"))

;;; --------------------------------------------------------------------
;;; EAGAIN from empty binary ports

  ;;Reading binary port without available bytes causes EAGAIN: GET-U8.
  ;; (check
  ;;     (with-compensations
  ;; 	(receive (in ou)
  ;; 	    (make-binary-ports)
  ;; 	  (guard (E ((i/o-eagain-error? E)
  ;; 		     #t)
  ;; 		    (else E))
  ;; 	    (get-u8 in))))
  ;;   => #t)

  ;; ;;Reading  binary   port  without   available  bytes   causes  EAGAIN:
  ;; ;;LOOKAHEAD-U8.
  ;; (check
  ;;     (with-compensations
  ;; 	(receive (in ou)
  ;; 	    (make-binary-ports)
  ;; 	  (guard (E ((i/o-eagain-error? E)
  ;; 		     #;(debug-print E)
  ;; 		     #t)
  ;; 		    (else E))
  ;; 	    (lookahead-u8 in))))
  ;;   => #t)

  ;; ;;Reading  binary   port  without   available  bytes   causes  EAGAIN:
  ;; ;;GET-BYTEVECTOR-ALL.
  ;; (check
  ;;     (with-compensations
  ;; 	(receive (in ou)
  ;; 	    (make-binary-ports)
  ;; 	  (guard (E ((i/o-eagain-error? E)
  ;; 		     #;(debug-print E)
  ;; 		     #t)
  ;; 		    (else E))
  ;; 	    (get-bytevector-all in))))
  ;;   => #t)

  ;; ;;Reading  binary   port  without   available  bytes   causes  EAGAIN:
  ;; ;;GET-BYTEVECTOR-N.
  ;; (check
  ;;     (with-compensations
  ;; 	(receive (in ou)
  ;; 	    (make-binary-ports)
  ;; 	  (guard (E ((i/o-eagain-error? E)
  ;; 		     #;(debug-print E)
  ;; 		     #t)
  ;; 		    (else E))
  ;; 	    (get-bytevector-n in 1))))
  ;;   => #t)

  ;; ;;Reading  binary   port  without   available  bytes   causes  EAGAIN:
  ;; ;;GET-BYTEVECTOR-N!.
  ;; (check
  ;;     (with-compensations
  ;; 	(receive (in ou)
  ;; 	    (make-binary-ports)
  ;; 	  (guard (E ((i/o-eagain-error? E)
  ;; 		     #;(debug-print E)
  ;; 		     #t)
  ;; 		    (else E))
  ;; 	    (get-bytevector-n! in (make-bytevector 1) 0 1))))
  ;;   => #t)

  ;; ;;Reading  binary   port  without   available  bytes   causes  EAGAIN:
  ;; ;;GET-BYTEVECTOR-SOME.
  ;; (check
  ;;     (with-compensations
  ;; 	(receive (in ou)
  ;; 	    (make-binary-ports)
  ;; 	  (guard (E ((i/o-eagain-error? E)
  ;; 		     #;(debug-print E)
  ;; 		     #t)
  ;; 		    (else E))
  ;; 	    (get-bytevector-some in))))
  ;;   => #t)

  #t)


#;(parametrise ((check-test-name	'port-textual))

  ;;Reading from textual port with available characters: GET-STRING-ALL.
  #;(check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-textual-file-descriptor-input-port* in "in" (native-transcoder))))
	    (px.write ou '#ve(ascii "ciao") 4)
	    (get-string-all P))))
    => "ciao")

;;; --------------------------------------------------------------------
;;; reading textual ports with full Unicode character available

  ;;Reading textual port with full Unicode character available: GET-STRING-SOME.
  #;(check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-textual-file-descriptor-input-port* in "in" (native-transcoder))))
	    (px.write ou CJK-COMPATIBILITY-IDEOGRAPH-2F9D1-UTF-8)
	    (get-string-some P))))
    => (utf8->string CJK-COMPATIBILITY-IDEOGRAPH-2F9D1-UTF-8))

;;; --------------------------------------------------------------------
;;; EAGAIN from empty textual ports

  ;;Reading  textual   port  without  available  bytes   causes  EAGAIN:
  ;;GET-CHAR.
  (check
      (with-compensations
	(receive (in ou)
	    (make-textual-ports)
	  (guard (E ((i/o-eagain-error? E)
		     #t)
		    (else E))
	    (get-char in))))
    => #t)

  ;;Reading  textual   port  without  available  bytes   causes  EAGAIN:
  ;;LOOKAHEAD-CHAR.
  (check
      (with-compensations
	(receive (in ou)
	    (make-textual-ports)
	  (guard (E ((i/o-eagain-error? E)
		     #;(debug-print E)
		     #t)
		    (else E))
	    (lookahead-char in))))
    => #t)

  ;;Reading  textual   port  without  available  bytes   causes  EAGAIN:
  ;;PEEK-CHAR.
  (check
      (with-compensations
	(receive (in ou)
	    (make-textual-ports)
	  (guard (E ((i/o-eagain-error? E)
		     #;(debug-print E)
		     #t)
		    (else E))
	    (peek-char in))))
    => #t)

  ;;Reading  textual   port  without  available  bytes   causes  EAGAIN:
  ;;GET-STRING-ALL.
  (check
      (with-compensations
	(receive (in ou)
	    (make-textual-ports)
	  (guard (E ((i/o-eagain-error? E)
		     #;(debug-print E)
		     #t)
		    (else E))
	    (get-string-all in))))
    => #t)

  ;;Reading  textual   port  without  available  bytes   causes  EAGAIN:
  ;;GET-STRING-N.
  (check
      (with-compensations
	(receive (in ou)
	    (make-textual-ports)
	  (guard (E ((i/o-eagain-error? E)
		     #;(debug-print E)
		     #t)
		    (else E))
	    (get-string-n in 1))))
    => #t)

  ;;Reading  textual   port  without  available  bytes   causes  EAGAIN:
  ;;GET-STRING-N!.
  (check
      (with-compensations
	(receive (in ou)
	    (make-textual-ports)
	  (guard (E ((i/o-eagain-error? E)
		     #;(debug-print E)
		     #t)
		    (else E))
	    (get-string-n! in (make-string 1) 0 1))))
    => #t)

  ;;Reading  textual   port  without  available  bytes   causes  EAGAIN:
  ;;GET-STRING-SOME.
  (check
      (with-compensations
	(receive (in ou)
	    (make-textual-ports)
	  (guard (E ((i/o-eagain-error? E)
		     #;(debug-print E)
		     #t)
		    (else E))
	    (get-string-some in))))
    => #t)

;;; --------------------------------------------------------------------
;;; EAGAIN from textual ports with partial characters

  ;;Reading textual port with partial character available causes EAGAIN:
  ;;GET-STRING-SOME.
  (check
      (with-compensations
	(receive (in ou)
	    (make-pipe)
	  (let ((P (make-textual-file-descriptor-input-port* in "in" (native-transcoder))))
	    ;;Write  a  partial  Unicode   charin  UTF-8  encoding:  CJK
	    ;;COMPATIBILITY IDEOGRAPH 2F9D1.
	    (px.write ou '#vu8(#xF0 #xAF #xA7))
	    (guard (E ((i/o-eagain-error? E)
		       #;(debug-print E)
		       #t)
		      (else E))
	      (get-string-some P)))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
