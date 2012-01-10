;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for the reader
;;;Date: Thu Oct 13, 2011
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
(import (rename (vicare)
		(parameterize	parametrise))
  (checks))

(print-unicode #f)
(check-set-mode! 'report-failed)
(check-display "*** testing Vicare reader\n")


;;;; helpers

(define-syntax read-and-lexical-violation
  (syntax-rules ()
    ((_ ?input ?irritant)
     (check
	 (let ((port (open-string-input-port ?input)))
	   (guard (E ((lexical-violation? E)
;;;		      (check-pretty-print (condition-message E))
		      (if (irritants-condition? E)
			  (condition-irritants E)
			'(no-irritants)))
		     (else E))
	     (read port)))
       => '(?irritant)))))

(define-syntax read-and-syntax-violation
  (syntax-rules ()
    ((_ ?input ?irritant)
     (check
	 (let ((port (open-string-input-port ?input)))
	   (guard (E ((syntax-violation? E)
;;;		      (check-pretty-print (condition-message E))
		      (if (irritants-condition? E)
			  (condition-irritants E)
			'(no-irritants)))
		     (else E))
	     (read port)))
       => '(?irritant)))))


(parametrise ((check-test-name	'newlines))

  (check	;lf
      (read (open-string-input-port "(ciao\xA;hello)"))
    => '(ciao hello))

  (check	;crlf
      (read (open-string-input-port "(ciao\xD;\xA;hello)"))
    => '(ciao hello))

  (check	;nel
      (read (open-string-input-port "(ciao\x0085;hello)"))
    => '(ciao hello))

  (check	;crnel
      (read (open-string-input-port "(ciao\xD;\x0085;hello)"))
    => '(ciao hello))

  (check	;ls
      (read (open-string-input-port "(ciao\x2028;hello)"))
    => '(ciao hello))

  #t)


(parametrise ((check-test-name	'symbols))

  (define-syntax read-symbol-and-eof
    (syntax-rules ()
      ((_ ?input)
       (read-symbol-and-eof ?input ?input))
      ((_ ?input ?string)
       (check
	   (let* ((port (open-string-input-port ?input))
		  (sym  (read port))
		  (eof  (port-eof? port)))
	     (list (symbol? sym) (symbol->string sym) eof))
	 => '(#t ?string #t)))))

;;; --------------------------------------------------------------------

  (read-symbol-and-eof "ciao")
  (read-symbol-and-eof "ciao-\\x41;\\x42;\\x43;" "ciao-ABC")
  (read-symbol-and-eof "\\x41;\\x42;\\x43;-ciao" "ABC-ciao")
  (read-symbol-and-eof "ciao-\\x41;\\x42;\\x43;-hello" "ciao-ABC-hello")
  (read-symbol-and-eof "|ciao|" "ciao")
  (read-symbol-and-eof "|123|" "123")
  (read-symbol-and-eof "|123-\\x41;\\x42;\\x43;|" "123-\x41;\x42;\x43;")
  (read-symbol-and-eof "|123-\\x41;\\x42;\\x43;-456|" "123-\x41;\x42;\x43;-456")

;;; --------------------------------------------------------------------
;;; tests for bugs in Ikarus' reader

  (read-symbol-and-eof "-"		"-")
  (read-symbol-and-eof "->"		"->")
  (read-symbol-and-eof "->ciao"		"->ciao")
  (read-symbol-and-eof "->-ciao"	"->-ciao")

  (read-and-lexical-violation "-ciao"		"-c")
  (read-and-lexical-violation "--ciao"		"--")
  (read-and-lexical-violation "-->ciao"		"--")
  (read-and-lexical-violation "-i123"		"-i1")
  (read-and-lexical-violation "-iCIAO"		"-iC")
  (read-and-lexical-violation "-i-ciao"		"-i-")

;;; --------------------------------------------------------------------
;;; tests for bug in Ikarus' reader

  (read-symbol-and-eof "..."		"...")

  (read-and-lexical-violation ".."		"..")
  (read-and-lexical-violation ".ciao"		".c")
  (read-and-lexical-violation "..ciao"		"..c")
  (read-and-lexical-violation "...ciao"		"...c")
  (read-and-lexical-violation "....ciao"	"....")

  (read-symbol-and-eof "\\x2E;ciao"			".ciao")
  (read-symbol-and-eof "\\x2E;\\x2E;ciao"		"..ciao")
  (read-symbol-and-eof "\\x2E;\\x2E;\\x2E;ciao"		"...ciao")
  (read-symbol-and-eof "\\x2E;\\x2E;\\x2E;\\x2E;ciao"	"....ciao")

;;; --------------------------------------------------------------------
;;; weird cases

  (check
      (let* ((port (open-string-input-port "ciao|"))
	     (sym  (read port))
	     (eof1 (port-eof? port))
	     (ch   (get-char port))
	     (eof2 (port-eof? port)))
	(list (symbol? sym) (symbol->string sym) eof1 ch eof2))
    => '(#t "ciao" #f #\| #t))

  (check
      (let* ((port (open-string-input-port "ciao-\\x41;|"))
	     (sym  (read port))
	     (eof1 (port-eof? port))
	     (ch   (get-char port))
	     (eof2 (port-eof? port)))
	(list (symbol? sym) (symbol->string sym)
	      eof1 ch eof2))
    => '(#t "ciao-A" #f #\| #t))

  (check
      ;;This is split into: "ciao", "hello", "|" and the ending vertical
      ;;bar would cause an error if read.
      ;;
      (let* ((port (open-string-input-port "|ciao|hello|"))
	     (sym1 (read port))
	     (sym2 (read port))
	     (eof1 (port-eof? port))
	     (ch   (get-char port))
	     (eof2 (port-eof? port)))
	(list (symbol? sym1) (symbol->string sym1)
	      (symbol? sym2) (symbol->string sym2)
	      eof1 ch eof2))
    => '(#t "ciao" #t "hello" #f #\| #t))

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid backslash sequence
      (let ((port (open-string-input-port "ciao-\\xZ;")))
	(guard (E ((lexical-violation? E)
;;;		   (check-pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (read port)))
    => '("\\xZ" "ciao-"))

  #t)


(parametrise ((check-test-name	'gensym))

  (check
      (let ((sym (read (open-string-input-port "#{ciao}"))))
	(list (gensym? sym) (symbol? sym) (gensym->unique-string sym)))
    => '(#t #t "ciao"))

  (check
      (let ((sym (read (open-string-input-port "#{|95BEx%X86N?8X&yC|}"))))
	(list (gensym? sym) (symbol? sym) (gensym->unique-string sym)))
    => '(#t #t "95BEx%X86N?8X&yC"))

  (check
      (let ((sym (read (open-string-input-port "#{ciao hello}"))))
  	(list (gensym? sym) (symbol? sym) (gensym->unique-string sym)))
    => '(#t #t "hello"))

  (check
      (let ((sym (read (open-string-input-port "#{|ciao| |hello|}"))))
  	(list (gensym? sym) (symbol? sym) (gensym->unique-string sym)))
    => '(#t #t "hello"))

  (check
      (let ((sym (read (open-string-input-port "#{ciao |hello|}"))))
  	(list (gensym? sym) (symbol? sym) (gensym->unique-string sym)))
    => '(#t #t "hello"))

  (check
      (let ((sym (read (open-string-input-port "#{|ciao| hello}"))))
  	(list (gensym? sym) (symbol? sym) (gensym->unique-string sym)))
    => '(#t #t "hello"))

  (check
      (let ((sym (read (open-string-input-port "#{d |95BEx%X86N?8X&yC|}"))))
  	(gensym->unique-string sym))
    => '"95BEx%X86N?8X&yC")

  #t)


(parametrise ((check-test-name	'chars))

  (define-syntax read-char-and-eof
    (syntax-rules ()
      ((_ ?input ?result)
       (check
      	   (let* ((port (open-string-input-port ?input))
		  (obj  (read port))
		  (eof	(port-eof? port)))
      	     (list (char? obj) obj eof))
      	 => (list #t ?result #t)))))

;;; --------------------------------------------------------------------

  (read-char-and-eof "#\\A"		(integer->char 65))

;;; --------------------------------------------------------------------

  (read-char-and-eof "#\\a"		#\a)
  (read-char-and-eof "#\\b"		#\b)
  (read-char-and-eof "#\\d"		#\d)
  (read-char-and-eof "#\\e"		#\e)
  (read-char-and-eof "#\\l"		#\l)
  (read-char-and-eof "#\\n"		#\n)
  (read-char-and-eof "#\\r"		#\r)
  (read-char-and-eof "#\\s"		#\s)
  (read-char-and-eof "#\\t"		#\t)
  (read-char-and-eof "#\\p"		#\p)
  (read-char-and-eof "#\\v"		#\v)

  (read-char-and-eof "#\\nul"		#\x00)
  (read-char-and-eof "#\\newline"	#\x0A)
  (read-char-and-eof "#\\alarm"		#\x07)
  (read-char-and-eof "#\\backspace"	#\x08)
  (read-char-and-eof "#\\tab"		#\x09)
  (read-char-and-eof "#\\linefeed"	#\x0A)
  (read-char-and-eof "#\\vtab"		#\x0B)
  (read-char-and-eof "#\\page"		#\x0C)
  (read-char-and-eof "#\\return"	#\x0D)
  (read-char-and-eof "#\\esc"		#\x1B)
  (read-char-and-eof "#\\space"		#\x20)
  (read-char-and-eof "#\\delete"	#\x7F)

;;; --------------------------------------------------------------------

  (read-char-and-eof "#\\x"		#\x)

  (read-char-and-eof "#\\xA"		#\x0A)
  (read-char-and-eof "#\\xB"		#\x0B)
  (read-char-and-eof "#\\xC"		#\x0C)
  (read-char-and-eof "#\\xD"		#\x0D)
  (read-char-and-eof "#\\xE"		#\x0E)
  (read-char-and-eof "#\\xF"		#\x0F)

  (read-char-and-eof "#\\xa"		#\x0A)
  (read-char-and-eof "#\\xb"		#\x0B)
  (read-char-and-eof "#\\xc"		#\x0C)
  (read-char-and-eof "#\\xd"		#\x0D)
  (read-char-and-eof "#\\xe"		#\x0E)
  (read-char-and-eof "#\\xf"		#\x0F)

  (read-char-and-eof "#\\x01"		#\x01)
  (read-char-and-eof "#\\x12"		#\x12)
  (read-char-and-eof "#\\x123"		#\x123)
  (read-char-and-eof "#\\xFAF"		#\xFAF)

  #t)


(parametrise ((check-test-name	'strings))

  (define-syntax read-string
    (syntax-rules ()
      ((_ (?chars ...))
       (read-string (?chars ...) (?chars ...))
       )
      ((_ (?input-chars ...) (?result-chars ...))
       (check
      	   (let* ((str  (string #\" ?input-chars ... #\"))
      		  (port (open-string-input-port str)))
      	     (read port))
      	 => (string ?result-chars ...)))))

  (define lf	#\x000A)
  (define cr	#\x000D)
  (define nel	#\x0085)
  (define ls	#\x2028)

  (define space		#\space)
  (define backslash	#\\)

;;; --------------------------------------------------------------------

  (read-string (#\c #\i #\a #\o))
  (read-string (#\A lf #\Z))
  (read-string (#\A cr #\Z))
  (read-string (#\A nel #\Z))
  (read-string (#\A ls #\Z))
  (read-string (#\A cr lf #\Z))
  (read-string (#\A cr nel #\Z))

;;; --------------------------------------------------------------------
;;; \<line ending><intraline whitespace>

  (read-string (#\A       backslash lf     space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash lf     space space #\Z) (#\A space #\Z))

  (read-string (#\A       backslash nel    space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash nel    space space #\Z) (#\A space #\Z))

  (read-string (#\A       backslash ls     space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash ls     space space #\Z) (#\A space #\Z))

  (read-string (#\A       backslash cr lf  space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash cr lf  space space #\Z) (#\A space #\Z))

  (read-string (#\A       backslash cr nel space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash cr nel space space #\Z) (#\A space #\Z))

;;; --------------------------------------------------------------------
;;; \<intraline whitespace><line ending>

  (read-string (#\A       backslash space lf     #\Z) (#\A #\Z))
  (read-string (#\A space backslash space lf     #\Z) (#\A space #\Z))

  (read-string (#\A       backslash space nel    #\Z) (#\A #\Z))
  (read-string (#\A space backslash space nel    #\Z) (#\A space #\Z))

  (read-string (#\A       backslash space ls     #\Z) (#\A #\Z))
  (read-string (#\A space backslash space ls     #\Z) (#\A space #\Z))

  (read-string (#\A       backslash space cr lf  #\Z) (#\A #\Z))
  (read-string (#\A space backslash space cr lf  #\Z) (#\A space #\Z))

  (read-string (#\A       backslash space cr nel #\Z) (#\A #\Z))
  (read-string (#\A space backslash space cr nel #\Z) (#\A space #\Z))
;;; --------------------------------------------------------------------
;;; \<intraline whitespace><line ending><intraline whitespace>

  (read-string (#\A       backslash space lf     space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash space lf     space space #\Z) (#\A space #\Z))

  (read-string (#\A       backslash space nel    space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash space nel    space space #\Z) (#\A space #\Z))

  (read-string (#\A       backslash space ls     space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash space ls     space space #\Z) (#\A space #\Z))

  (read-string (#\A       backslash space cr lf  space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash space cr lf  space space #\Z) (#\A space #\Z))

  (read-string (#\A       backslash space cr nel space space #\Z) (#\A #\Z))
  (read-string (#\A space backslash space cr nel space space #\Z) (#\A space #\Z))

  #t)


(parametrise ((check-test-name	'numbers))

  (define-syntax read-number-and-eof
    (syntax-rules ()
      ((_ ?input)
       (read-symbol-and-eof ?input (string->number ?input)))
      ((_ ?input ?number)
       (check
	   (let* ((port (open-string-input-port ?input))
		  (obj  (read port))
		  (eof  (port-eof? port)))
	     (list (number? obj) obj eof))
	 => '(#t ?number #t)))))

;;; --------------------------------------------------------------------
;;; exact integers

  (read-number-and-eof "1"		+1)
  (read-number-and-eof "12"		+12)
  (read-number-and-eof "123"		+123)

  (read-number-and-eof "+1"		+1)
  (read-number-and-eof "+12"		+12)
  (read-number-and-eof "+123"		+123)

  (read-number-and-eof "-1"		-1)
  (read-number-and-eof "-12"		-12)
  (read-number-and-eof "-123"		-123)

  (read-number-and-eof "#e1"		+1)
  (read-number-and-eof "#e12"		+12)
  (read-number-and-eof "#e123"		+123)

  (read-number-and-eof "#e+1"		+1)
  (read-number-and-eof "#e+12"		+12)
  (read-number-and-eof "#e+123"		+123)

  (read-number-and-eof "#e-1"		-1)
  (read-number-and-eof "#e-12"		-12)
  (read-number-and-eof "#e-123"		-123)

;;; --------------------------------------------------------------------
;;; inexact integers

  (read-number-and-eof "1."		+1.0)
  (read-number-and-eof "12."		+12.0)
  (read-number-and-eof "123."		+123.0)
  (read-number-and-eof "123.0"		+123.0)

  (read-number-and-eof "+1."		+1.0)
  (read-number-and-eof "+12."		+12.0)
  (read-number-and-eof "+123."		+123.0)
  (read-number-and-eof "+123.0"		+123.0)

  (read-number-and-eof "-1."		-1.0)
  (read-number-and-eof "-12."		-12.0)
  (read-number-and-eof "-123."		-123.0)
  (read-number-and-eof "-123.0"		-123.0)

  (read-number-and-eof "#i1"		+1.0)
  (read-number-and-eof "#i12"		+12.0)
  (read-number-and-eof "#i123"		+123.0)

  (read-number-and-eof "#i+1"		+1.0)
  (read-number-and-eof "#i+12"		+12.0)
  (read-number-and-eof "#i+123"		+123.0)

  (read-number-and-eof "#i-1"		-1.0)
  (read-number-and-eof "#i-12"		-12.0)
  (read-number-and-eof "#i-123"		-123.0)

;;; --------------------------------------------------------------------
;;; flonums

  (read-number-and-eof "123.456"	+123.456)
  (read-number-and-eof "-123.456"	-123.456)
  (read-number-and-eof "+123.456"	+123.456)

  (read-number-and-eof ".1"		0.1)
  (read-number-and-eof ".12"		0.12)
  (read-number-and-eof ".123"		0.123)

  (read-number-and-eof "+.1"		0.1)
  (read-number-and-eof "+.12"		0.12)
  (read-number-and-eof "+.123"		0.123)

  (read-number-and-eof "-.1"		-0.1)
  (read-number-and-eof "-.12"		-0.12)
  (read-number-and-eof "-.123"		-0.123)

;;; --------------------------------------------------------------------
;;; cflonums

  (read-number-and-eof "1.1+2.2i"	1.1+2.2i)
  (read-number-and-eof "1.+2.2i"	1.0+2.2i)
  (read-number-and-eof "1.1+2.i"	1.0+2.i)
  (read-number-and-eof "1.+2.i"		1.+2.i)
  (read-number-and-eof "1.+2.i"		1.+2.i)

  (read-number-and-eof "1.1@2.2"	1.1@2.2)
  (read-number-and-eof "1.@2.2"		1.0@2.2)
  (read-number-and-eof "1.1@2."		1.0@2.)
  (read-number-and-eof "1.@2."		1.@2.)
  (read-number-and-eof "1.@2."		1.@2.)

;;; --------------------------------------------------------------------
;;;


;;; --------------------------------------------------------------------
;;; distinguishing between numbers and symbols

  (read-number-and-eof "+i"		+1i)
  (read-number-and-eof "-i"		-1i)
  (read-number-and-eof "-inf.0"		-inf.0)
  (read-number-and-eof "+inf.0"		+inf.0)
  (read-number-and-eof "-nan.0"		+nan.0)
  (read-number-and-eof "+nan.0"		+nan.0)

  #t)


(parametrise ((check-test-name	'comments))

  (define-syntax doit-quoted
    (syntax-rules ()
      ((_ ?input ?to-be-quoted-result)
       (check
	   (let ((port (open-string-input-port ?input)))
	     (read port))
	 => (quote ?to-be-quoted-result)))))

  (define-syntax doit-unquoted
    (syntax-rules ()
      ((_ ?input ?unquoted-result)
       (check
	   (let ((port (open-string-input-port ?input)))
	     (read port))
	 => ?unquoted-result))))

;;; --------------------------------------------------------------------

  (doit-quoted "#!vicare 123"		123)
  (doit-quoted "#!ikarus 123"		123)
  (doit-quoted "#!r6rs   123"		123)
  (doit-unquoted "#!eof 123"		(eof-object))

  ;;The ones not recognised  are just comments.  Test identifiers having
  ;;the  first  char  equal to  the  first  char  of the  recognised  #!
  ;;comments.
  (doit-quoted "#!ciao   123"		123)
  (doit-quoted "#!verde  123"		123)
  (doit-quoted "#!indaco 123"		123)
  (doit-quoted "#!rosso  123"		123)

;;; --------------------------------------------------------------------
;;; comment lists

  (check
      (let ((port (open-string-input-port "#!vicare #!()")))
	(eof-object? (read port)))
    => #t)

  (check
      (let ((port (open-string-input-port "#!vicare #!() 123")))
	(read port))
    => 123)

  (check	;invalid in r6rs mode
      (let ((port (open-string-input-port "#!r6rs #!()")))
	(guard (E ((lexical-violation? E)
		   (condition-irritants E))
		  (else E))
	  (read port)))
    => '("#!("))

;;; --------------------------------------------------------------------

  (check	;unexpected EOF
      (let ((port (open-string-input-port "#!r6rs #!")))
	(guard (E ((lexical-violation? E)
		   (condition-message E))
		  (else E))
	  (read port)))
    => "invalid EOF while reading hash datum")

  #t)


(parametrise ((check-test-name	'lists))

  (define-syntax read-list-and-eof
    (syntax-rules ()
      ((_ ?input ?result)
       (check
	   (let* ((port (open-string-input-port ?input))
		  (obj  (read port))
		  (eof  (port-eof? port)))
	     (list (list? obj) obj eof))
	 => `(#t ?result #t)))))

;;; --------------------------------------------------------------------

  (check
      (let* ((port (open-string-input-port "()"))
	     (obj  (read port))
	     (eof  (port-eof? port)))
	(list (null? obj) obj eof))
    => `(#t () #t))

  (check
      (let* ((port (open-string-input-port "(1 . 2)"))
	     (obj  (read port))
	     (eof  (port-eof? port)))
	(list (pair? obj) obj eof))
    => `(#t (1 . 2) #t))

  (check
      (let* ((port (open-string-input-port "(1 2 . 3)"))
	     (obj  (read port))
	     (eof  (port-eof? port)))
	(list (pair? obj) obj eof))
    => `(#t (1 2 . 3) #t))

;;; --------------------------------------------------------------------

  (read-list-and-eof "(1)"		(1))
  (read-list-and-eof "(1 2)"		(1 2))
  (read-list-and-eof "(1 2 3)"		(1 2 3))

  (read-list-and-eof "(#0=1 2 3)"	(1 2 3))
  (read-list-and-eof "(#0=1 2 #0#)"	(1 2 1))
  (read-list-and-eof "(#0#  2 #0=3)"	(3 2 3))

;;; --------------------------------------------------------------------
;;; errors

  ;; missing closing parenthesis
  (read-and-lexical-violation "  (1 2"	no-irritants)
  (read-and-lexical-violation "  [1 2"	no-irritants)

  ;; mismatched parentheses
  (read-and-lexical-violation "(1 2]"	no-irritants)
  (read-and-lexical-violation "[1 2)"	no-irritants)

;;; --------------------------------------------------------------------
;;; misplaced dots

  (read-and-lexical-violation "(.)"	no-irritants)
  (read-and-lexical-violation "(. 1)"	no-irritants)
  (read-and-lexical-violation "(1 .)"	no-irritants)
  (read-and-lexical-violation "(1 . .)"	no-irritants)

  (read-and-lexical-violation "(1 . 2 3)"	(datum . 3))

  #t)


(parametrise ((check-test-name	'vectors))

  (define-syntax read-vector-and-eof
    (syntax-rules ()
      ((_ ?input ?result)
       (check
	   (let* ((port (open-string-input-port ?input))
		  (obj  (read port))
		  (eof  (port-eof? port)))
	     (list (vector? obj) obj eof))
	 => `(#t ?result #t)))))

;;; --------------------------------------------------------------------

  (read-vector-and-eof "#()"		#())
  (read-vector-and-eof "#(1)"		#(1))
  (read-vector-and-eof "#(1 2)"		#(1 2))
  (read-vector-and-eof "#(1 2 3)"	#(1 2 3))

  (read-vector-and-eof "#(#0=1 2 3)"		#(1 2 3))
  (read-vector-and-eof "#(#0=1 2 #0#)"		#(1 2 1))
  (read-vector-and-eof "#(#0#  2 #0=3)"		#(3 2 3))

;;; --------------------------------------------------------------------
;;; errors

  ;; missing closing parenthesis
  (read-and-lexical-violation "  #(1 2"	no-irritants)

  ;; mismatched parentheses
  (read-and-lexical-violation "#(1 2]"	no-irritants)

;;; --------------------------------------------------------------------
;;; misplaced dots

  (read-and-lexical-violation "#(.)"	no-irritants)
  (read-and-lexical-violation "#(. 1)"	no-irritants)
  (read-and-lexical-violation "#(1 .)"	no-irritants)

  #t)


(parametrise ((check-test-name	'bytevectors))

  (define-syntax read-bv-and-eof
    (syntax-rules ()
      ((_ ?input ?result)
       (check
	   (let* ((port (open-string-input-port ?input))
		  (obj  (read port))
		  (eof  (port-eof? port)))
	     (list (bytevector? obj) obj eof))
	 => `(#t ?result #t)))))

;;; --------------------------------------------------------------------

  (read-bv-and-eof "#vu8()"		,(u8-list->bytevector '()))
  (read-bv-and-eof "#vu8(1)"		,(u8-list->bytevector '(1)))
  (read-bv-and-eof "#vu8(1 2 3)"	,(u8-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs8()"		,(s8-list->bytevector '()))
  (read-bv-and-eof "#vs8(1)"		,(s8-list->bytevector '(1)))
  (read-bv-and-eof "#vs8(-1)"		,(s8-list->bytevector '(-1)))
  (read-bv-and-eof "#vs8(1 2 3)"	,(s8-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs8(-1 -2 -3)"	,(s8-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; 16-bit little endian

  (read-bv-and-eof "#vu16l(#x1234)"		#vu8(#x34 #x12))
  (read-bv-and-eof "#vu16l(#x1234 #x5678)"	#vu8(#x34 #x12 #x78 #x56))

  (read-bv-and-eof "#vu16l()"		,(u16l-list->bytevector '()))
  (read-bv-and-eof "#vu16l(1)"		,(u16l-list->bytevector '(1)))
  (read-bv-and-eof "#vu16l(1 2 3)"	,(u16l-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs16l()"		,(s16l-list->bytevector '()))
  (read-bv-and-eof "#vs16l(1)"		,(s16l-list->bytevector '(1)))
  (read-bv-and-eof "#vs16l(-1)"		,(s16l-list->bytevector '(-1)))
  (read-bv-and-eof "#vs16l(1 2 3)"	,(s16l-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs16l(-1 -2 -3)"	,(s16l-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; 16-bit big endian

  (read-bv-and-eof "#vu16b(#x1234)"		#vu8(#x12 #x34))
  (read-bv-and-eof "#vu16b(#x1234 #x5678)"	#vu8(#x12 #x34 #x56 #x78))

  (read-bv-and-eof "#vu16b()"		,(u16b-list->bytevector '()))
  (read-bv-and-eof "#vu16b(1)"		,(u16b-list->bytevector '(1)))
  (read-bv-and-eof "#vu16b(1 2 3)"	,(u16b-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs16b()"		,(s16b-list->bytevector '()))
  (read-bv-and-eof "#vs16b(1)"		,(s16b-list->bytevector '(1)))
  (read-bv-and-eof "#vs16b(-1)"		,(s16b-list->bytevector '(-1)))
  (read-bv-and-eof "#vs16b(1 2 3)"	,(s16b-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs16b(-1 -2 -3)"	,(s16b-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; 16-bit native endian

  (read-bv-and-eof "#vu16n()"		,(u16n-list->bytevector '()))
  (read-bv-and-eof "#vu16n(1)"		,(u16n-list->bytevector '(1)))
  (read-bv-and-eof "#vu16n(1 2 3)"	,(u16n-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs16n()"		,(s16n-list->bytevector '()))
  (read-bv-and-eof "#vs16n(1)"		,(s16n-list->bytevector '(1)))
  (read-bv-and-eof "#vs16n(-1)"		,(s16n-list->bytevector '(-1)))
  (read-bv-and-eof "#vs16n(1 2 3)"	,(s16n-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs16n(-1 -2 -3)"	,(s16n-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; 32-bit little endian

  (read-bv-and-eof "#vu32l()"		,(u32l-list->bytevector '()))
  (read-bv-and-eof "#vu32l(1)"		,(u32l-list->bytevector '(1)))
  (read-bv-and-eof "#vu32l(1 2 3)"	,(u32l-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs32l()"		,(s32l-list->bytevector '()))
  (read-bv-and-eof "#vs32l(1)"		,(s32l-list->bytevector '(1)))
  (read-bv-and-eof "#vs32l(-1)"		,(s32l-list->bytevector '(-1)))
  (read-bv-and-eof "#vs32l(1 2 3)"	,(s32l-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs32l(-1 -2 -3)"	,(s32l-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; 32-bit big endian

  (read-bv-and-eof "#vu32b()"		,(u32b-list->bytevector '()))
  (read-bv-and-eof "#vu32b(1)"		,(u32b-list->bytevector '(1)))
  (read-bv-and-eof "#vu32b(1 2 3)"	,(u32b-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs32b()"		,(s32b-list->bytevector '()))
  (read-bv-and-eof "#vs32b(1)"		,(s32b-list->bytevector '(1)))
  (read-bv-and-eof "#vs32b(-1)"		,(s32b-list->bytevector '(-1)))
  (read-bv-and-eof "#vs32b(1 2 3)"	,(s32b-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs32b(-1 -2 -3)"	,(s32b-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; 32-bit native endian

  (read-bv-and-eof "#vu32n()"		,(u32n-list->bytevector '()))
  (read-bv-and-eof "#vu32n(1)"		,(u32n-list->bytevector '(1)))
  (read-bv-and-eof "#vu32n(1 2 3)"	,(u32n-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs32n()"		,(s32n-list->bytevector '()))
  (read-bv-and-eof "#vs32n(1)"		,(s32n-list->bytevector '(1)))
  (read-bv-and-eof "#vs32n(-1)"		,(s32n-list->bytevector '(-1)))
  (read-bv-and-eof "#vs32n(1 2 3)"	,(s32n-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs32n(-1 -2 -3)"	,(s32n-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; 64-bit little endian

  (read-bv-and-eof "#vu64l()"		,(u64l-list->bytevector '()))
  (read-bv-and-eof "#vu64l(1)"		,(u64l-list->bytevector '(1)))
  (read-bv-and-eof "#vu64l(1 2 3)"	,(u64l-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs64l()"		,(s64l-list->bytevector '()))
  (read-bv-and-eof "#vs64l(1)"		,(s64l-list->bytevector '(1)))
  (read-bv-and-eof "#vs64l(-1)"		,(s64l-list->bytevector '(-1)))
  (read-bv-and-eof "#vs64l(1 2 3)"	,(s64l-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs64l(-1 -2 -3)"	,(s64l-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; 64-bit big endian

  (read-bv-and-eof "#vu64b()"		,(u64b-list->bytevector '()))
  (read-bv-and-eof "#vu64b(1)"		,(u64b-list->bytevector '(1)))
  (read-bv-and-eof "#vu64b(1 2 3)"	,(u64b-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs64b()"		,(s64b-list->bytevector '()))
  (read-bv-and-eof "#vs64b(1)"		,(s64b-list->bytevector '(1)))
  (read-bv-and-eof "#vs64b(-1)"		,(s64b-list->bytevector '(-1)))
  (read-bv-and-eof "#vs64b(1 2 3)"	,(s64b-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs64b(-1 -2 -3)"	,(s64b-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; 64-bit native endian

  (read-bv-and-eof "#vu64n()"		,(u64n-list->bytevector '()))
  (read-bv-and-eof "#vu64n(1)"		,(u64n-list->bytevector '(1)))
  (read-bv-and-eof "#vu64n(1 2 3)"	,(u64n-list->bytevector '(1 2 3)))

  (read-bv-and-eof "#vs64n()"		,(s64n-list->bytevector '()))
  (read-bv-and-eof "#vs64n(1)"		,(s64n-list->bytevector '(1)))
  (read-bv-and-eof "#vs64n(-1)"		,(s64n-list->bytevector '(-1)))
  (read-bv-and-eof "#vs64n(1 2 3)"	,(s64n-list->bytevector '(1 2 3)))
  (read-bv-and-eof "#vs64n(-1 -2 -3)"	,(s64n-list->bytevector '(-1 -2 -3)))

;;; --------------------------------------------------------------------
;;; single-precision flonum

  (read-bv-and-eof "#vf4l()"		,(f4l-list->bytevector '()))
  (read-bv-and-eof "#vf4l(1.2)"		,(f4l-list->bytevector '(1.2)))
  (read-bv-and-eof "#vf4l(1.2 3.4)"	,(f4l-list->bytevector '(1.2 3.4)))
  (read-bv-and-eof "#vf4l(-1.2 -3.4)"	,(f4l-list->bytevector '(-1.2 -3.4)))

  (read-bv-and-eof "#vf4b()"		,(f4b-list->bytevector '()))
  (read-bv-and-eof "#vf4b(1.2)"		,(f4b-list->bytevector '(1.2)))
  (read-bv-and-eof "#vf4b(1.2 3.4)"	,(f4b-list->bytevector '(1.2 3.4)))
  (read-bv-and-eof "#vf4b(-1.2 -3.4)"	,(f4b-list->bytevector '(-1.2 -3.4)))

  (read-bv-and-eof "#vf4n()"		,(f4n-list->bytevector '()))
  (read-bv-and-eof "#vf4n(1.2)"		,(f4n-list->bytevector '(1.2)))
  (read-bv-and-eof "#vf4n(1.2 3.4)"	,(f4n-list->bytevector '(1.2 3.4)))
  (read-bv-and-eof "#vf4n(-1.2 -3.4)"	,(f4n-list->bytevector '(-1.2 -3.4)))

;;; --------------------------------------------------------------------
;;; double-precision flonum

  (read-bv-and-eof "#vf8l()"		,(f8l-list->bytevector '()))
  (read-bv-and-eof "#vf8l(1.2)"		,(f8l-list->bytevector '(1.2)))
  (read-bv-and-eof "#vf8l(1.2 3.4)"	,(f8l-list->bytevector '(1.2 3.4)))
  (read-bv-and-eof "#vf8l(-1.2 -3.4)"	,(f8l-list->bytevector '(-1.2 -3.4)))

  (read-bv-and-eof "#vf8b()"		,(f8b-list->bytevector '()))
  (read-bv-and-eof "#vf8b(1.2)"		,(f8b-list->bytevector '(1.2)))
  (read-bv-and-eof "#vf8b(1.2 3.4)"	,(f8b-list->bytevector '(1.2 3.4)))
  (read-bv-and-eof "#vf8b(-1.2 -3.4)"	,(f8b-list->bytevector '(-1.2 -3.4)))

  (read-bv-and-eof "#vf8n()"		,(f8n-list->bytevector '()))
  (read-bv-and-eof "#vf8n(1.2)"		,(f8n-list->bytevector '(1.2)))
  (read-bv-and-eof "#vf8n(1.2 3.4)"	,(f8n-list->bytevector '(1.2 3.4)))
  (read-bv-and-eof "#vf8n(-1.2 -3.4)"	,(f8n-list->bytevector '(-1.2 -3.4)))

;;; --------------------------------------------------------------------
;;; single-precision cflonum

  (read-bv-and-eof "#vc4l()"			,(c4l-list->bytevector '()))
  (read-bv-and-eof "#vc4l(1.2+3.4i)"		,(c4l-list->bytevector '(1.2+3.4i)))
  (read-bv-and-eof "#vc4l(1.2+3.4i 5.6-7.8i)"	,(c4l-list->bytevector '(1.2+3.4i 5.6-7.8i)))
  (read-bv-and-eof "#vc4l(-1.2+3.4i -5.6-7.8i)"	,(c4l-list->bytevector '(-1.2+3.4i -5.6-7.8i)))

  (read-bv-and-eof "#vc4b()"			,(c4b-list->bytevector '()))
  (read-bv-and-eof "#vc4b(1.2+3.4i)"		,(c4b-list->bytevector '(1.2+3.4i)))
  (read-bv-and-eof "#vc4b(1.2+3.4i 5.6-7.8i)"	,(c4b-list->bytevector '(1.2+3.4i 5.6-7.8i)))
  (read-bv-and-eof "#vc4b(-1.2+3.4i -5.6-7.8i)"	,(c4b-list->bytevector '(-1.2+3.4i -5.6-7.8i)))

  (read-bv-and-eof "#vc4n()"			,(c4n-list->bytevector '()))
  (read-bv-and-eof "#vc4n(1.2+3.4i)"		,(c4n-list->bytevector '(1.2+3.4i)))
  (read-bv-and-eof "#vc4n(1.2+3.4i 5.6-7.8i)"	,(c4n-list->bytevector '(1.2+3.4i 5.6-7.8i)))
  (read-bv-and-eof "#vc4n(-1.2+3.4i -5.6-7.8i)"	,(c4n-list->bytevector '(-1.2+3.4i -5.6-7.8i)))

;;; --------------------------------------------------------------------
;;; double-precision cflonum

  (read-bv-and-eof "#vc8l()"			,(c8l-list->bytevector '()))
  (read-bv-and-eof "#vc8l(1.2+3.4i)"		,(c8l-list->bytevector '(1.2+3.4i)))
  (read-bv-and-eof "#vc8l(1.2+3.4i 5.6-7.8i)"	,(c8l-list->bytevector '(1.2+3.4i 5.6-7.8i)))
  (read-bv-and-eof "#vc8l(-1.2+3.4i -5.6-7.8i)"	,(c8l-list->bytevector '(-1.2+3.4i -5.6-7.8i)))

  (read-bv-and-eof "#vc8b()"			,(c8b-list->bytevector '()))
  (read-bv-and-eof "#vc8b(1.2+3.4i)"		,(c8b-list->bytevector '(1.2+3.4i)))
  (read-bv-and-eof "#vc8b(1.2+3.4i 5.6-7.8i)"	,(c8b-list->bytevector '(1.2+3.4i 5.6-7.8i)))
  (read-bv-and-eof "#vc8b(-1.2+3.4i -5.6-7.8i)"	,(c8b-list->bytevector '(-1.2+3.4i -5.6-7.8i)))

  (read-bv-and-eof "#vc8n()"			,(c8n-list->bytevector '()))
  (read-bv-and-eof "#vc8n(1.2+3.4i)"		,(c8n-list->bytevector '(1.2+3.4i)))
  (read-bv-and-eof "#vc8n(1.2+3.4i 5.6-7.8i)"	,(c8n-list->bytevector '(1.2+3.4i 5.6-7.8i)))
  (read-bv-and-eof "#vc8n(-1.2+3.4i -5.6-7.8i)"	,(c8n-list->bytevector '(-1.2+3.4i -5.6-7.8i)))

  #t)


(parametrise ((check-test-name	'locations))

  (define-syntax doit-unquoted
    (syntax-rules ()
      ((_ ?input ?unquoted-result)
       (check
	   (let ((port (open-string-input-port ?input)))
	     (read port))
	 => (quote ?unquoted-result)))))

;;; --------------------------------------------------------------------

  (doit-unquoted "(#0=1 . #0#)" (1 . 1))
  (doit-unquoted "(#0# . #0=1)" (1 . 1))

  (doit-unquoted "(#1=ciao . #1#)" (ciao . ciao))

;;; --------------------------------------------------------------------

  (doit-unquoted "(#1=ciao #1# #1# #1#)" (ciao ciao ciao ciao))
  (doit-unquoted "(#1# #1=ciao #1# #1#)" (ciao ciao ciao ciao))
  (doit-unquoted "(#1# #1# #1=ciao #1#)" (ciao ciao ciao ciao))
  (doit-unquoted "(#1# #1# #1# #1=ciao)" (ciao ciao ciao ciao))


  #t)


;;;; done

(check-report)

;;; end of file
