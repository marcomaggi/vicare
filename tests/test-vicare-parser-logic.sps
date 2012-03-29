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


(define-syntax string->token/false
  ;;Define  the device logic  to parse  a numeric  string from  a Scheme
  ;;string object.
  ;;
  ;;The literal identifiers  must be free identifiers, both  here and in
  ;;the context where this macro is used.
  ;;
  (syntax-rules (:introduce-device-arguments
		 :generate-eof-then-chars-tests
		 :generate-delimiter-test
		 :unexpected-eof-error
		 :fail)

    ;;Introduce a list of identifiers used as device-specific arguments;
    ;;they  will  be  the  first  arguments  for  each  parser  operator
    ;;function.
    ((_ :introduce-device-arguments ?kont . ?rest)
     (?kont (input.string input.length input.index) . ?rest))

    ;;Whenever  an  input  character  is  not accepted  by  an  operator
    ;;function  this   rule  is  used   to  decide  what  to   do.   For
    ;;STRING->NUMBER the action is to return false.
    ((_ :fail (?input.string ?input.length ?input.index) ?ch-var)
     #f)

    ;;Whenever the  end-of-input is found in  a position in  which it is
    ;;unexpected,  this  rule  is  used  to  decide  what  to  do.   For
    ;;STRING->NUMBER the action is to return false.
    ((_ :unexpected-eof-error (?input.string ?input.length ?input.index))
     #f)

    ;;This rule is  used for input devices for  which the numeric string
    ;;is embedded into a sequence of other characters, so there exists a
    ;;set  of characters  that  delimit the  end-of-number.  The  parser
    ;;delegates  to  the  device  the responsibility  of  knowing  which
    ;;characters are delimiters, if any.
    ;;
    ;;When the input  device is a string containing  only the number, as
    ;;is  the case  for  STRING->NUMBER: there  are  no delimiters,  the
    ;;end-of-number  is the  end of  the  string.  We  avoid looking  at
    ;;?CH-VAR and just expand to the not-delimiter continuation form.
    ((_ :generate-delimiter-test ?ch-var ?ch-is-delimiter-kont ?ch-is-not-delimiter-kont)
     ?ch-is-not-delimiter-kont)

    ;;This rule is used to  generate the tests for an operator function.
    ;;First  of all  the  end-of-input condition  is  checked; then  the
    ;;continuation form for more characters is expanded.
    ((_ :generate-eof-then-chars-tests ?ch-var ?next ?fail
	(?input.string ?input.length ?input.index)
	?end-of-input-kont ?more-characters-kont)
     (let-syntax
	 ((?fail (syntax-rules ()
		   ((_) #f)))
	  (?next (syntax-rules ()
		   ((_ ?operator-name ?operator-arg (... ...))
		    (?operator-name ?input.string ?input.length (fxadd1 ?input.index)
				    ?operator-arg (... ...))))))
       (if (fx= ?input.index ?input.length) ;end-of-input
	   ?end-of-input-kont
	 (let ((?ch-var (string-ref ?input.string ?input.index)))
	   ?more-characters-kont))))
    ))


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
    (define-parser-logic define-string->integer-parser next fail
      (%parse-integer ()
		      ((%digit) => D
		       (next %parse-digit+  D)))
      (%parse-digit+ (accumulator)
		     ((eof)
		      accumulator)
		     ((%digit) => D
		      (next %parse-digit+ (+ D (* 10 accumulator))))))
    ;;Actual parser drawing characters from an input string.
    (define-string->integer-parser string->token/false
      (%parse-integer))
    (assert (string? input-string))
    (%parse-integer input-string (string-length input-string) 0))

;;; --------------------------------------------------------------------

  (check (parse-integer "1")		=> 1)
  (check (parse-integer "123")		=> 123)
  (check (parse-integer "ciao")		=> #f)
  (check (parse-integer "123ciao")	=> #f)

  #t)


;;;; done

(check-report)

;;; end of file
