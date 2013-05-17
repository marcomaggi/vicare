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

  (check	;setting, closing all
      (with-compensations
	(receive (inp oup)
	    (make-binary-ports)
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


;;;; done

(check-report)

;;; end of file
