;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for parser-tools libraries
;;;Date: Tue Sep  8, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa parser tools: source location objects\n")


(parametrise ((check-test-name 'maker))

  (check
      (let (((S lt.<source-location>) (lt.<source-location> ((lt.input: "here")
							     (lt.line: 10)
							     (lt.column: 20)
							     (lt.offset: 30)))))
	(list (S input) (S line) (S column) (S offset)))
    => '("here" 10 20 30))

  #t)


(parametrise ((check-test-name 'predicates))

  (check
      (is-a? (lt.<start-source-location> (lt.error-message: "this")) lt.<source-location>)
    => #t)

  (check
      (is-a? #f lt.<source-location>)
    => #f)

  (check
      (is-a? 123 lt.<source-location>)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (lt.<source-location>? (lt.<start-source-location> (lt.error-message: "this")))
    => #t)

  (check
      (lt.<source-location>? #f)
    => #f)

  (check
      (lt.<source-location>? 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (lt.<source-location>?/or-false (lt.<start-source-location> (lt.error-message: "this")))
    => #t)

  (check
      (lt.<source-location>?/or-false #f)
    => #t)

  (check
      (lt.<source-location>?/or-false 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (lt.<source-location>?/start (lt.<start-source-location> ("this" 1 2 3)))
    => #f)

  (check
      (lt.<source-location>?/start (lt.<start-source-location> ("this")))
    => #t)

  (check
      (lt.<source-location>?/start #f)
    => #f)

  (check
      (lt.<source-location>?/start 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (lt.<source-location>?/start/or-false (lt.<source-location> ("this" 1 2 3)))
    => #f)

  #t)


(parametrise ((check-test-name 'comparison))

  (let ((a (lt.<source-location> (#f 1 2 3)))
  	(b (lt.<source-location> (#f 1 2 3))))

    (check (source-location=? a b)		=> #t)
    (check (source-location-point=? a b)	=> #t)
    (check (source-location-point<? a b)	=> #f)
    (check (source-location-point>? a b)	=> #f)
    (check (source-location-point<=? a b)	=> #t)
    (check (source-location-point>=? a b)	=> #t)

    #f)

  (let ((a1 (lt.<source-location> (#f 1 2 3)))
  	(b1 (lt.<source-location> (#f 1 2 30))))

    (check (source-location=? a1 b1)		=> #f)
    (check (source-location-point=? a1 b1)	=> #t)
    (check (source-location-point<? a1 b1)	=> #f)
    (check (source-location-point>? a1 b1)	=> #f)
    (check (source-location-point<=? a1 b1)	=> #t)
    (check (source-location-point>=? a1 b1)	=> #t)

    #f)

  (let ((a2 (lt.<source-location> (#f 1 2 3)))
  	(b2 (lt.<source-location> (#f 10 2 3))))

    (check (source-location=? a2 b2)		=> #f)
    (check (source-location-point=? a2 b2)	=> #f)
    (check (source-location-point<? a2 b2)	=> #t)
    (check (source-location-point>? a2 b2)	=> #f)
    (check (source-location-point<=? a2 b2)	=> #t)
    (check (source-location-point>=? a2 b2)	=> #f)

    #f)

  (let ((a3 (lt.<source-location> (#f 1 2 3)))
	(b3 (lt.<source-location> (#f 1 20 3))))

    (check (source-location=? a3 b3)		=> #f)
    (check (source-location-point=? a3 b3)	=> #f)
    (check (source-location-point<? a3 b3)	=> #t)
    (check (source-location-point>? a3 b3)	=> #f)
    (check (source-location-point<=? a3 b3)	=> #t)
    (check (source-location-point>=? a3 b3)	=> #f)

    #f)

  #t)


(parametrise ((check-test-name 'update))

  (check	;token length
      (let ((loc (lt.<source-location> ('this 10 20 30))))
	(source-location-update loc 4))
    (=> source-location=?)
    (lt.<source-location> ('this 10 (+ 20 4) (+ 30 4))))

  (check	;newline char
      (let ((loc (lt.<source-location> ('this 10 20 30))))
	(source-location-update loc #\newline))
    (=> source-location=?)
    (lt.<source-location> ('this (+ 10 1) 1 (+ 30 1))))

  (check	;return char
      (let ((loc (lt.<source-location> ('this 10 20 30))))
	(source-location-update loc #\return))
    (=> source-location=?)
    (lt.<source-location> ('this 10 (+ 20 1) (+ 30 1))))

  (check	;return char
      (parametrise ((source-location-honor-return #f))
	(let ((loc (lt.<source-location> ('this 10 20 30))))
	  (source-location-update loc #\return)))
    (=> source-location=?)
    (lt.<source-location> ('this 10 (+ 20 1) (+ 30 1))))

  (check	;return char
      (parametrise ((source-location-honor-return #t))
	(let ((loc (lt.<source-location> ('this 10 20 30))))
	  (source-location-update loc #\return)))
    (=> source-location=?)
    (lt.<source-location> ('this 10 1 (+ 30 1))))

  (check	;tab char, 8chars
      (let ((loc (lt.<source-location> ('this 10 20 30))))
	(source-location-update loc #\tab))
    (=> source-location=?)
    (lt.<source-location> ('this 10 24 (+ 30 1))))

  (check	;tab char, 8chars
      (parametrise ((source-location-tab-function source-location-tab-function/8chars))
	(let ((loc (lt.<source-location> ('this 10 20 30))))
	  (source-location-update loc #\tab)))
    (=> source-location=?)
    (lt.<source-location> ('this 10 24 (+ 30 1))))

  (check	;tab char, table function
      (parametrise ((source-location-tab-function source-location-tab-function/tab-table))
	(let ((loc (lt.<source-location> ('this 10 20 30))))
	  (source-location-update loc #\tab)))
    (=> source-location=?)
    (lt.<source-location> ('this 10 24 (+ 30 1))))

  (check	;tab char, table function, tab table
      (parametrise ((source-location-tab-function	source-location-tab-function/tab-table)
		    (source-location-tab-table		'(6 12 18 24 30 36)))
	(let ((loc (lt.<source-location> ('this 10 20 30))))
	  (source-location-update loc #\tab)))
    (=> source-location=?)
    (lt.<source-location> ('this 10 24 (+ 30 1))))

  (check	;tab char, table function, short tab table
      (parametrise ((source-location-tab-function	source-location-tab-function/tab-table)
		    (source-location-tab-table		'(6 12 18)))
	(let ((loc (lt.<source-location> ('this 10 20 30))))
	  (source-location-update loc #\tab)))
    (=> source-location=?)
    (lt.<source-location> ('this 10 24 (+ 30 1))))

  #t)


(parametrise ((check-test-name 'misc))

  (check
      (object->string (lt.<source-location> ((lt.input: 'this)
					     (lt.line: 10)
					     (lt.column: 20)
					     (lt.offset: 30))))
    => "this:10:20")

  (check
      (let (((S lt.<source-location>) (lt.<source-location> ((lt.input: "here")
							     (lt.line: 10)
							     (lt.column: 20)
							     (lt.offset: 30)))))
	(list (S line string) (S column string) (S offset string)))
    => '("10" "20" "30"))

  #t)


;;;; done

(check-report)

;;; end of file
