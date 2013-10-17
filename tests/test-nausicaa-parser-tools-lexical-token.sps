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
  (prefix (nausicaa parser-tools lexical-tokens) lt.)
  (prefix (nausicaa parser-tools source-locations) sl.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa parser tools: lexical tokens\n")


(parametrise ((check-test-name	'makers))

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: 'category)
							 (lt.location: 'location)
							 (lt.value:    'value)
							 (lt.length:   5)))))
	(list (T category) (T location) (T value) (T length)))
    => '(category location value 5))

;;; --------------------------------------------------------------------

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: 'woppa)))))
	(list (T category) (T location) (T value) (T length)))
    => '(woppa #f #f 0))

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token>
				     ((lt.category:	'category)
				      (lt.location:	'location)
				      (lt.value:	'value)
				      (lt.length:	5)))))
	(list (T category) (T location) (T value) (T length)))
    => '(category location value 5))

  #t)


(parametrise ((check-test-name	'predicates))

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: '*eoi*)))))
	(T end-of-input?))
    => #t)

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: 'woppa)))))
	(T end-of-input?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: '*lexer-error*)))))
	(T lexer-error?))
    => #t)

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: 'woppa)))))
	(T lexer-error?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: '*eoi*)))))
	(T special?))
    => #t)

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: '*lexer-error*)))))
	(T special?))
    => #t)

  (check
      (let (((T lt.<lexical-token>) (lt.<lexical-token> ((lt.category: 'woppa)))))
	(T special?))
    => #f)

  #t)


(parametrise ((check-test-name	'error))

  (check
      (let (((T lt.<lexer-error>) (lt.<lexer-error> ((lt.location: 123)
						     (lt.value: "ciao")
						     (lt.length: 4)
						     (error-message: "darn!")))))
	(list (T category) (T location) (T value) (T length) (T message)
	      (is-a? T lt.<lexer-error>)))
    => '(*lexer-error* 123 "ciao" 4 "darn!" #t))

  #t)


;;;; done

(check-report)

;;; end of file
