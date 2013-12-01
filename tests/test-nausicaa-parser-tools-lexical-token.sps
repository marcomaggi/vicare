;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for lexical token records
;;;Date: Fri Oct 29, 2010
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
(import (nausicaa)
  (prefix (nausicaa parser-tools lexical-tokens)   lt.)
  (prefix (nausicaa parser-tools source-locations) sl.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa parser tools: lexical tokens\n")


(parametrise ((check-test-name	'lexical-tokens))

  (define (make-it C)
    (lt.<lexical-token> ((lt.category: C)
			 (lt.value:    'value)
			 (lt.length:   5))))

;;; --------------------------------------------------------------------

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: 'category)
							 (lt.value:    'value)
							 (lt.length:   5)))))
	(list (T category) (T location unspecified?) (T value) (T length)))
    => '(category #t value 5))

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: 'woppa)))))
	(list (T category) (T location unspecified?) (T value) (T length)))
    => '(woppa #t #f 0))

;;; --------------------------------------------------------------------

  (check
      (let (((T lt.<lexical-token>) (make-it 'category)))
	(list (T special?) (T end-of-input?) (T lexer-error?)))
    => '(#f #f #f))

  (check
      (let (((T lt.<lexical-token>) (make-it '*eoi*)))
	(list (T special?) (T end-of-input?) (T lexer-error?)))
    => '(#t #t #f))

  (check
      (let (((T lt.<lexical-token>) (make-it '*lexer-error*)))
	(list (T special?) (T end-of-input?) (T lexer-error?)))
    => '(#t #f #t))

  #t)


(parametrise ((check-test-name	'end-of-input))

  (define (make-it)
    (lt.<end-of-input> ()))

;;; --------------------------------------------------------------------

  (check
      (let (((T lt.<end-of-input>) (lt.<end-of-input> ())))
	(list (T category) (T location unspecified?) (T value) (T length)))
    => `(*eoi* #t ,(eof-object) 0))

;;; --------------------------------------------------------------------

  (check
      (let (((T lt.<end-of-input>) (make-it)))
	(list (T special?) (T end-of-input?) (T lexer-error?)))
    => '(#t #t #f))

  #t)


(parametrise ((check-test-name	'lexer-error))

  (define (make-it)
    (lt.<lexer-error> ((lt.error-message: "ciao"))))

;;; --------------------------------------------------------------------

  (check
      (let (((T lt.<lexer-error>) (lt.<lexer-error> ((lt.error-message: "darn!")))))
	(list (T category) (T location unspecified?) (T value) (T length)
	      (T message)))
    => `(*lexer-error* #t #f 0 "darn!"))

;;; --------------------------------------------------------------------

  (check
      (let (((T lt.<lexer-error>) (make-it)))
	(list (T special?) (T lexer-error?) (T end-of-input?)))
    => '(#t #t #f))

  #t)


;;;; done

(check-report)

;;; end of file
