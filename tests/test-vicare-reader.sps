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

;;; --------------------------------------------------------------------

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


;;;; done

(check-report)

;;; end of file
