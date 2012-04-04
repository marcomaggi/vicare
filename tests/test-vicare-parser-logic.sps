;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for parser logic library
;;;Date: Thu Mar 29, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare parser-logic)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare parser logic library\n")


(parametrise ((check-test-name	'abba))

  (module (parse-abba)

    (define (parse-abba input-string)
      (assert (string? input-string))
      (%parse-string input-string (string-length input-string) 0 '()))

    ;;Parser logic  to convert a  string of #\a  and #\b into a  list of
    ;;characters.
    (define-parser-logic define-string->abba-parser ch next fail
      (%parse-string (accumulator)
		     ((:end-of-input)
		      (reverse accumulator))
		     ((#\a)
		      (next %parse-string (cons #\a accumulator)))
		     ((#\b)
		      (next %parse-string (cons #\b accumulator)))))

    ;;Actual parser drawing characters from an input string.
    (define-string->abba-parser string->token-or-false
      (%parse-string))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (check (parse-abba "")		=> '())
  (check (parse-abba "a")		=> '(#\a))
  (check (parse-abba "b")		=> '(#\b))
  (check (parse-abba "1")		=> #f)
  (check (parse-abba "ciao")		=> #f)
  (check (parse-abba "abb")		=> '(#\a #\b #\b))

  #t)


(parametrise ((check-test-name	'fail))

  (module (parse-abab)

    (define (parse-abab input-string)
      (assert (string? input-string))
      (%parse-string input-string (string-length input-string) 0 '()))

    ;;Parser logic to convert a string  of intermixed #\a and #\b into a
    ;;list of characters.
    (define-parser-logic define-string->abab-parser ch next fail
      (%parse-string (accumulator)
		     ((:end-of-input)
		      (reverse accumulator))
		     ((#\a #\b)
		      (if (or (null? accumulator)
			      (case ch
				((#\a) (char=? #\b (car accumulator)))
				((#\b) (char=? #\a (car accumulator)))))
			  (next %parse-string (cons ch accumulator))
			(fail)))))

    ;;Actual parser drawing characters from an input string.
    (define-string->abab-parser string->token-or-false
      (%parse-string))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (check (parse-abab "")		=> '())
  (check (parse-abab "a")		=> '(#\a))
  (check (parse-abab "b")		=> '(#\b))
  (check (parse-abab "1")		=> #f)
  (check (parse-abab "ciao")		=> #f)
  (check (parse-abab "abb")		=> #f)
  (check (parse-abab "baa")		=> #f)
  (check (parse-abab "abab")		=> '(#\a #\b #\a #\b))
  (check (parse-abab "baba")		=> '(#\b #\a #\b #\a))

  #t)


(parametrise ((check-test-name	'integers))

  (define (parse-integer input-string)
    (define (%digit ch)
      ;;Given a  character argument: return the  corresponding fixnum if
      ;;the character is between #\0 and #\9, else return false.
      ;;
      (let ((N (fx- (char->integer ch) (char->integer #\0))))
	(and (fx>= N 0)
	     (fx<  N 10)
	     N)))
    ;;Parser logic to convert a string into an exact integer in base 10.
    (define-parser-logic define-string->integer-parser ch next fail
      (%parse-integer ()
		      ((%digit) => D
		       (next %parse-digit+  D)))
      (%parse-digit+ (accumulator)
		     ((:end-of-input)
		      accumulator)
		     ((%digit) => D
		      (next %parse-digit+ (+ D (* 10 accumulator))))))
    ;;Actual parser drawing characters from an input string.
    (define-string->integer-parser string->token-or-false
      (%parse-integer))
    (assert (string? input-string))
    (%parse-integer input-string (string-length input-string) 0))

;;; --------------------------------------------------------------------

  (check (parse-integer "")		=> #f)
  (check (parse-integer "1")		=> 1)
  (check (parse-integer "123")		=> 123)
  (check (parse-integer "ciao")		=> #f)
  (check (parse-integer "123ciao")	=> #f)

  #t)


;;;; done

(check-report)

;;; end of file
