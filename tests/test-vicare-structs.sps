;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for structs
;;;Date: Mon Oct 24, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (vicare checks)
  (ikarus system $structs))

(print-unicode #f)
(check-set-mode! 'report-failed)
(check-display "*** testing Vicare structs\n")


;;;; helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))


(parametrise ((check-test-name	'definition))

  (define-struct color
    (red green blue))

  (check
      (let ((S (make-color 1 2 3)))
	(color? S))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((S (make-color 1 2 3)))
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(1 2 3))

  (check
      (let ((S (make-color 1 2 3)))
	(set-color-red!   S 10)
	(set-color-green! S 20)
	(set-color-blue!  S 30)
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(10 20 30))

;;; --------------------------------------------------------------------

  (check
      (let ((S (make-color 1 2 3)))
	(list ($color-red   S)
	      ($color-green S)
	      ($color-blue  S)))
    => '(1 2 3))

  (check
      (let ((S (make-color 1 2 3)))
	($set-color-red!   S 10)
	($set-color-green! S 20)
	($set-color-blue!  S 30)
	(list ($color-red   S)
	      ($color-green S)
	      ($color-blue  S)))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'rtd))

  (define color-rtd
    (make-struct-type "color" '(red green blue)))

  (check
      (struct-type-name color-rtd)
    => "color")

  (check
      (symbol? (struct-type-symbol color-rtd))
    => #t)

  (check
      (struct-type-field-names color-rtd)
    => '(red green blue))

;;; --------------------------------------------------------------------

  (check
      (catch #f
	(struct-type-name 123))
    => '(123))

  (check
      (catch #f
	(struct-type-symbol 123))
    => '(123))

  (check
      (catch #f
	(struct-type-field-names 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'using))

  (define color-rtd
    (make-struct-type "color" '(red green blue)))

  (define make-color
    (struct-constructor color-rtd))

  (define color?
    (struct-predicate color-rtd))

  (define color-red
    (struct-field-accessor color-rtd 0))

  (define color-green
    (struct-field-accessor color-rtd 1))

  (define color-blue
    (struct-field-accessor color-rtd 2))

  (define set-color-red!
    (struct-field-mutator color-rtd 0))

  (define set-color-green!
    (struct-field-mutator color-rtd 1))

  (define set-color-blue!
    (struct-field-mutator color-rtd 2))

;;; --------------------------------------------------------------------

  (check
      (let ((S (make-color 1 2 3)))
	(color? S))
    => #t)

  (check
      (let ((S (make-color 1 2 3)))
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(1 2 3))

  (check
      (let ((S (make-color 1 2 3)))
	(set-color-red!   S 10)
	(set-color-green! S 20)
	(set-color-blue!  S 30)
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(10 20 30))

;;; --------------------------------------------------------------------

  (check
      (catch #f
	(struct-constructor 123))
    => '(123))

  (check
      (catch #f
	(struct-predicate 123))
    => '(123))

  (check
      (catch #f
	(struct-field-accessor 123 0))
    => '(123))

  (check
      (catch #f
	(struct-field-mutator 123 0))
    => '(123))

  (check
      (catch #f
	(struct-field-accessor color-rtd 3))
    => (list 3 color-rtd))

  (check
      (catch #f
	(struct-field-mutator color-rtd 3))
    => (list 3 color-rtd))

  #t)


(parametrise ((check-test-name	'inspect))

  (define color-rtd
    (make-struct-type "color" '(red green blue)))

  (define S
    ((struct-constructor color-rtd) 1 2 3))

;;; --------------------------------------------------------------------

  (check
      (struct? 123)
    => #f)

  (check
      (struct? color-rtd)
    => #t)

  (check
      (struct? S)
    => #t)

  (check
      (struct? S color-rtd)
    => #t)

  (check
      (struct-length color-rtd)
    => 6)

  (check
      (struct-length S)
    => 3)

  (check
      (struct-name color-rtd)
    => "base-rtd")

  (check
      (struct-name S)
    => "color")

  (check
      (struct-ref S 0)
    => 1)

  (check
      (struct-ref S 1)
    => 2)

  (check
      (struct-ref S 2)
    => 3)

  (check
      (begin
	(struct-set! S 0 10)
	(struct-ref S 0))
    => 10)

  (check
      (begin
	(struct-set! S 1 20)
	(struct-ref S 1))
    => 20)

  (check
      (begin
	(struct-set! S 2 30)
	(struct-ref S 2))
    => 30)

;;; --------------------------------------------------------------------

  (check
      (catch #f
	(struct-length 123))
    => '(123))

  (check
      (catch #f
	(struct-name 123))
    => '(123))

  (check
      (catch #f
	(struct-set! 123 0 0))
    => '(123))

  (check
      (catch #f
	(struct-ref 123 0))
    => '(123))

  (check
      (catch #f
	(struct-set! S 4 0))
    => (list 4 S))

  (check
      (catch #f
	(struct-ref S 4))
    => (list 4 S))

;;; --------------------------------------------------------------------

  (check
      (let ((S ((struct-constructor color-rtd) 1 2 3)))
	(struct-reset S)
	(list (struct-ref S 0)
	      (struct-ref S 1)
	      (struct-ref S 2)))
    => `(,(void) ,(void) ,(void)))

  #t)


;;;; done

(check-report)

;;; end of file
