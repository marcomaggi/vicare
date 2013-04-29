;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for fasl reading and writing
;;;Date: Mon Apr 29, 2013
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: reading and writing FASL objects\n")


;;;; helpers

(define (object->fasl obj)
  (let-values (((port getter) (open-bytevector-output-port)))
    (fasl-write obj port)
    (getter)))

(define (fasl->object fasl)
  (let* ((port (open-bytevector-input-port fasl))
	 (fasl (fasl-read port))
	 (eof  (get-bytevector-all port)))
    (close-port port)
    (values fasl eof)))

(define-syntax fasl->fasl
  (syntax-rules ()
    ((_ ?obj)
     (check
	 (let ((fasl (object->fasl ?obj)))
	   (call-with-values
	       (lambda ()
		 (fasl->object fasl))
	     list))
       => (list ?obj (eof-object))))))


(parametrise ((check-test-name	'basic))

  ;; fixnums
  (fasl->fasl 123)
  (fasl->fasl (greatest-fixnum))
  (fasl->fasl (least-fixnum))

  ;; bignums
  (fasl->fasl (+ +10 (greatest-fixnum)))
  (fasl->fasl (+ -10 (least-fixnum)))
  (fasl->fasl (+ +1 (greatest-fixnum)))
  (fasl->fasl (+ -1 (least-fixnum)))

  ;; ratnums
  (fasl->fasl 1/2)
  (fasl->fasl -10/13)
  (fasl->fasl (div (greatest-fixnum) (least-fixnum)))
  (fasl->fasl (div (+ 10 (greatest-fixnum))
		   (least-fixnum)))
  (fasl->fasl (div (+ +10 (greatest-fixnum))
		   (+ -10 (least-fixnum))))
  (fasl->fasl (div (greatest-fixnum)
		   (+ -10 (least-fixnum))))

  ;; flonums
  (fasl->fasl +0.0)
  (fasl->fasl -0.0)
  (fasl->fasl +1.2)
  (fasl->fasl -1.2)
  (fasl->fasl -inf.0)
  (fasl->fasl -inf.0)
  (fasl->fasl -nan.0)

  ;; cflonums
  (fasl->fasl +0.0+0.0i)
  (fasl->fasl +0.0-0.0i)
  (fasl->fasl -0.0+0.0i)
  (fasl->fasl -0.0-0.0i)
  (fasl->fasl +1.0+0.0i)
  (fasl->fasl +0.0-2.0i)
  (fasl->fasl -1.0+2.0i)
  (fasl->fasl -2.0-2.0i)
  (fasl->fasl +inf.0+inf.0i)
  (fasl->fasl +inf.0-inf.0i)
  (fasl->fasl -inf.0+inf.0i)
  (fasl->fasl -inf.0-inf.0i)
  (fasl->fasl +nan.0+nan.0i)

  ;; compnums
  (fasl->fasl 1+2i)
  (fasl->fasl 1+2.0i)
  (fasl->fasl 1.0+2i)
  (fasl->fasl 1/2+2i)
  (fasl->fasl 1+2/3i)
  (fasl->fasl 1/2+2/3i)
  (fasl->fasl +inf.0+2i)
  (fasl->fasl 2+inf.0i)
  (fasl->fasl +inf.0+2/3i)
  (fasl->fasl 2/3+inf.0i)

  ;; characters
  (fasl->fasl #\A)

  ;; pairs
  (fasl->fasl '())
  (fasl->fasl '(1 . 2))
  (fasl->fasl '(1 2 3))
  (fasl->fasl '(1 2 . 3))

  ;; vectors
  (fasl->fasl '#())
  (fasl->fasl '#(1))
  (fasl->fasl '#(1 2))
  (fasl->fasl '#(1 2 3))

  ;; bytevectors
  (fasl->fasl '#vu8())
  (fasl->fasl '#vu8(1))
  (fasl->fasl '#vu8(1 2))
  (fasl->fasl '#vu8(1 2 3))

  ;; symbols
  (fasl->fasl 'c)
  (fasl->fasl 'ciao)
  (let ((G (gensym)))
    (fasl->fasl G))

  #t)


(parametrise ((check-test-name	'structs))

  (define-struct alpha
    (a b c))

  (fasl->fasl (type-descriptor alpha))
  (fasl->fasl (make-alpha 1 2 3))

  #t)


#;(parametrise ((check-test-name	'records))

  (define-record-type alpha
    (fields a b c))

  (fasl->fasl (record-type-descriptor alpha))
  (fasl->fasl (make-alpha 1 2 3))

  #t)


;;;; done

(check-report)

;;; end of file
