;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Scheme
;;;Contents: tests for namespaces
;;;Date: Wed Aug  7, 2013
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
  (vicare language-extensions namespaces)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: namespaces\n")


(parametrise ((check-test-name	'base))

  (let ()
    (define-namespace hellos
      (export ciao salut)
      (define (ciao)
	'ciao)
      (define-syntax salut
	(syntax-rules ()
	  ((_)
	   'salut))))

    (check
	(hellos ciao)
      => 'ciao)

    (check
	(hellos salut)
      => 'salut)

    (let ()
      (using hellos)
      (check
	  (ciao)
	=> 'ciao)
      (check
	  (salut)
	=> 'salut)
      #f)

    #f)

  #t)


(parametrise ((check-test-name	'nested))

  (let ()
    (define-namespace hellos
      (export ciao salut japanese)
      (define (ciao)
      	'ciao)
      (define (salut)
      	'salut)
      (define-namespace japanese
	(export ohayo)
	(define (ohayo)
	  'ohayo)))

    (check
	(hellos ciao)
      => 'ciao)

    (check
	(hellos salut)
      => 'salut)

    (check
	(hellos japanese ohayo)
      => 'ohayo)

    #f)

  #t)


(parametrise ((check-test-name	'siblings))

  (let ()
    (define-namespace hellos
      (export ciao salut japanese)
      (define (ciao)
      	'ciao)
      (define (salut)
      	'salut))

    (define-namespace japanese
      (export ohayo)
      (define (ohayo)
	'ohayo))

    (check
	(hellos ciao)
      => 'ciao)

    (check
	(hellos salut)
      => 'salut)

    (check
	(hellos japanese ohayo)
      => 'ohayo)

    #f)

  #t)


(parametrise ((check-test-name	'variables))

  (define-syntax-rule (define-variable ?name ?init-value)
    (define the-variable ?init-value)
    (define-syntax ?name
      (syntax-rules ()
	((_)
	 the-variable)
	((_ ?new-value)
	 (set! the-variable ?new-value)))))

  (let ()
    (define-namespace woah
      (export blue red)
      (define-variable blue #f)
      (define-variable red  #f))

    (check
	(woah blue)
      => #f)

    (check
	(begin
	  (woah blue 123)
	  (woah blue))
      => 123)

    (let ()
      (using woah)

      (check
	  (red)
	=> #f)

      (check
	  (begin
	    (red 123)
	    (red))
	=> 123)

      #f)

    #f)

  #t)



;;;; done

(check-report)

;;; end of file
