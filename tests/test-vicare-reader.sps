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
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (ikarus)
		(parameterize	parametrise))
  (checks))

(print-unicode #f)
(check-set-mode! 'report-failed)
(display "*** testing Vicare reader\n")


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
;;;		   (pretty-print (condition-message E))
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


(parametrise ((check-test-name	'strings))

  (define-syntax read-string
    (syntax-rules ()
      ((_ (?chars ...))
       (read-string (?chars ...) (?chars ...)))
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
  (doit-quoted "#!r6rs   123"		123)
  (doit-unquoted "#!eof"		(eof-object))

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
