;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for custom ports
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
(import (vicare)
  (vicare language-extensions custom-ports)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare custom ports library\n")


(parametrise ((check-test-name	'binary-pairs))

;;; input

  (check
      (let-values (((in-port ou-port)
		    (open-binary-input-port-pair)))
	(get-bytevector-all in-port))
    => (eof-object))

  (check
      (let-values (((in-port ou-port)
		    (open-binary-input-port-pair)))
        (put-bytevector ou-port '#vu8(1 2 3))
	(flush-output-port ou-port)
	(get-bytevector-all in-port))
    => '#vu8(1 2 3))

  (check
      (let-values (((in-port ou-port)
		    (open-binary-input-port-pair)))
        (put-bytevector ou-port '#vu8(1 2 3))
        (put-bytevector ou-port '#vu8(4 5 6))
        (put-bytevector ou-port '#vu8(7 8 9))
	(flush-output-port ou-port)
	(get-bytevector-all in-port))
    => '#vu8(1 2 3 4 5 6 7 8 9))

  (check
      (with-result
       (let-values (((in-port ou-port)
		     (open-binary-input-port-pair)))
	 (put-bytevector ou-port '#vu8(1 2 3))
	 (put-bytevector ou-port '#vu8(4 5 6))
	 (put-bytevector ou-port '#vu8(7 8 9))
	 (flush-output-port ou-port)
	 (add-result (get-bytevector-n in-port 2))
	 (add-result (get-bytevector-n in-port 2))
	 (add-result (get-bytevector-n in-port 2))
	 (add-result (get-bytevector-n in-port 2))
	 (get-bytevector-n in-port 2)))
    => '(#vu8(9) (#vu8(1 2) #vu8(3 4) #vu8(5 6) #vu8(7 8))))

;;; --------------------------------------------------------------------
;;; output

  (check
      (let-values (((ou-port in-port)
		    (open-binary-output-port-pair)))
        (put-bytevector ou-port '#vu8(1 2 3))
	(flush-output-port ou-port)
	(get-bytevector-all in-port))
    => '#vu8(1 2 3))

  (check
      (let-values (((ou-port in-port)
		    (open-binary-output-port-pair)))
        (put-bytevector ou-port '#vu8(1 2 3))
        (put-bytevector ou-port '#vu8(4 5 6))
        (put-bytevector ou-port '#vu8(7 8 9))
	(flush-output-port ou-port)
	(get-bytevector-all in-port))
    => '#vu8(1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------
;;; input/output

  (check
      (let-values (((first-port second-port)
		    (open-binary-input/output-port-pair)))
        (put-bytevector first-port '#vu8(1 2 3))
	(flush-output-port first-port)
	(get-bytevector-all second-port))
    => '#vu8(1 2 3))

  (check
      (let-values (((first-port second-port)
		    (open-binary-input/output-port-pair)))
        (put-bytevector first-port '#vu8(1 2 3))
        (put-bytevector first-port '#vu8(4 5 6))
        (put-bytevector first-port '#vu8(7 8 9))
	(flush-output-port first-port)
	(get-bytevector-all second-port))
    => '#vu8(1 2 3 4 5 6 7 8 9))

  (check
      (with-result
       (let-values (((first-port second-port)
		     (open-binary-input/output-port-pair)))
	 (put-bytevector second-port '#vu8(1 2 3))
	 (put-bytevector second-port '#vu8(4 5 6))
	 (put-bytevector second-port '#vu8(7 8 9))
	 (flush-output-port second-port)
	 (add-result (get-bytevector-n first-port 2))
	 (add-result (get-bytevector-n first-port 2))
	 (add-result (get-bytevector-n first-port 2))
	 (add-result (get-bytevector-n first-port 2))
	 (get-bytevector-n first-port 2)))
    => '(#vu8(9) (#vu8(1 2) #vu8(3 4) #vu8(5 6) #vu8(7 8))))

  (check
      (with-result
       (let-values (((first-port second-port)
		     (open-binary-input/output-port-pair)))
	 (put-bytevector first-port '#vu8(1 2 3))
	 (put-bytevector first-port '#vu8(4 5 6))
	 (put-bytevector first-port '#vu8(7 8 9))
	 (flush-output-port first-port)
	 (add-result (get-bytevector-n second-port 2))
	 (add-result (get-bytevector-n second-port 2))
	 (add-result (get-bytevector-n second-port 2))
	 (add-result (get-bytevector-n second-port 2))
	 (get-bytevector-n second-port 2)))
    => '(#vu8(9) (#vu8(1 2) #vu8(3 4) #vu8(5 6) #vu8(7 8))))

  #t)


(parametrise ((check-test-name	'textual-pairs))

;;; input

  (check
      (let-values (((in-port ou-port)
		    (open-textual-input-port-pair)))
	(get-string-all in-port))
    => (eof-object))

  (check
      (let-values (((in-port ou-port)
		    (open-textual-input-port-pair)))
        (put-string ou-port "123")
	(flush-output-port ou-port)
	(get-string-all in-port))
    => "123")

  (check
      (let-values (((in-port ou-port)
		    (open-textual-input-port-pair)))
        (put-string ou-port "123")
        (put-string ou-port "456")
        (put-string ou-port "789")
	(flush-output-port ou-port)
	(get-string-all in-port))
    => "123456789")

  (check
      (with-result
       (let-values (((in-port ou-port)
		     (open-textual-input-port-pair)))
	 (put-string ou-port "123")
	 (put-string ou-port "456")
	 (put-string ou-port "789")
	 (flush-output-port ou-port)
	 (add-result (get-string-n in-port 2))
	 (add-result (get-string-n in-port 2))
	 (add-result (get-string-n in-port 2))
	 (add-result (get-string-n in-port 2))
	 (get-string-n in-port 2)))
    => '("9" ("12" "34" "56" "78")))

;;; --------------------------------------------------------------------
;;; output

  (check
      (let-values (((ou-port in-port)
		    (open-textual-output-port-pair)))
        (put-string ou-port "123")
	(flush-output-port ou-port)
	(get-string-all in-port))
    => "123")

  (check
      (let-values (((ou-port in-port)
		    (open-textual-output-port-pair)))
        (put-string ou-port "123")
        (put-string ou-port "456")
        (put-string ou-port "789")
	(flush-output-port ou-port)
	(get-string-all in-port))
    => "123456789")

;;; --------------------------------------------------------------------
;;; input/output

  (check
      (let-values (((first-port second-port)
		    (open-textual-input/output-port-pair)))
        (put-string first-port "123")
	(flush-output-port first-port)
	(get-string-all second-port))
    => "123")

  (check
      (let-values (((first-port second-port)
		    (open-textual-input/output-port-pair)))
        (put-string first-port "123")
        (put-string first-port "456")
        (put-string first-port "789")
	(flush-output-port first-port)
	(get-string-all second-port))
    => "123456789")

  (check
      (with-result
       (let-values (((first-port second-port)
		     (open-textual-input/output-port-pair)))
	 (put-string second-port "123")
	 (put-string second-port "456")
	 (put-string second-port "789")
	 (flush-output-port second-port)
	 (add-result (get-string-n first-port 2))
	 (add-result (get-string-n first-port 2))
	 (add-result (get-string-n first-port 2))
	 (add-result (get-string-n first-port 2))
	 (get-string-n first-port 2)))
    => '("9" ("12" "34" "56" "78")))

  (check
      (with-result
       (let-values (((first-port second-port)
		     (open-textual-input/output-port-pair)))
	 (put-string first-port "123")
	 (put-string first-port "456")
	 (put-string first-port "789")
	 (flush-output-port first-port)
	 (add-result (get-string-n second-port 2))
	 (add-result (get-string-n second-port 2))
	 (add-result (get-string-n second-port 2))
	 (add-result (get-string-n second-port 2))
	 (get-string-n second-port 2)))
    => '("9" ("12" "34" "56" "78")))

  #t)


;;;; done

(check-report)

;;; end of file
