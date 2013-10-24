;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for expand-time identifier properties
;;;Date: Mon Nov  8, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (vicare)
  (for (vicare language-extensions identifier-properties) expand)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: expand-time identifier properties\n")


(parametrise ((check-test-name	'table))

  (define-auxiliary-syntaxes type1 spiffy1)

  (eval-for-expand
    (register-identifier-property #'type1)
    (register-identifier-property #'spiffy1))

  (check
      (let ()
	(define-syntax (test stx)
	  (identifier-property-exists? #'type1))
	(test))
    => #t)

  (check
      (let ()
  	(define-syntax (test stx)
  	  (identifier-property-exists? #'spiffy1))
  	(test))
    => #t)

  (check
      (let ()
  	(define-syntax (test stx)
  	  (forget-identifier-property #'spiffy1)
  	  (identifier-property-exists? #'spiffy1))
  	(test))
    => #f)

  (check
      (let ()
  	(define-syntax (test stx)
  	  (identifier-property-exists? #'ciao))
  	(test))
    => #f)

  (let ()
    (eval-for-expand
      (forget-identifier-property #'type1)
      (forget-identifier-property #'spiffy1))
    #f)

  #t)


(parametrise ((check-test-name	'basic))

  (define-auxiliary-syntaxes type2 spiffy2)

  (eval-for-expand
   (register-identifier-property #'type2)
   (register-identifier-property #'spiffy2))

  (let ((a "ciao")
	(b 123)
	(c #f))

    (eval-for-expand
     (identifier-property-set! #'type2 #'a #'(quote string))
     (identifier-property-set! #'type2 #'b #'(quote fixnum))
     (identifier-property-set! #'type2 #'c #f)
     #;(debug-print 'table (identifier-property-table #'type2))
     #;(debug-print (identifier-property-ref #'type2 #'a #f))
     )

    (define-syntax (get-type2 stx)
      (syntax-case stx ()
	((_ ?id)
	 (begin
	   #;(debug-print 'get-type2 'table (identifier-property-table #'type2))
	   (identifier-property-ref #'type2 #'?id #'(void))))))

    (define-syntax (get-spiffy2 stx)
      (syntax-case stx ()
	((_ ?id)
	 (identifier-property-ref #'spiffy2 #'?id #'(void)))))

    (check (get-type2 a) => 'string)
    (check (get-type2 b) => 'fixnum)
    (check (get-type2 c) => #f)

    (check (get-spiffy2 a) => (void))
    (check (get-spiffy2 b) => (void))

    #f)

  #t)


(parametrise ((check-test-name	'example-for-docs))

  (define-syntax (type-of stx)
    (syntax-case stx ()
      ((_ ?id)
       (identifier-property-ref #'type-of #'?id #f))
      ))

  (eval-for-expand
   (register-identifier-property #'type-of))

  (let ((a "ciao"))
    (eval-for-expand
     (identifier-property-set! #'type-of #'a #'(quote string)))

    (check (type-of a) => 'string)

    #f)
  #t)


;;;; done

(check-report)

;;; end of file
