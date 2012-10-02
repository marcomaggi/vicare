;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for generalised C buffers
;;;Date: Tue Oct  2, 2012
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (vicare arguments general-c-buffers)
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare general C buffers handling\n")


(parametrise ((check-test-name	'strings))

  (check	;empty
      (with-general-c-strings ()
	  (string-to-bytevector string->ascii)
	#t)
    => #t)

;;; --------------------------------------------------------------------

  (check	;string
      (let ((a "ciao"))
        (with-general-c-strings ((a^ a))
	    (string-to-bytevector string->ascii)
	  (ascii->string a^)))
    => "ciao")

  (check	;strings
      (let ((a "ciao")
	    (b "hello"))
        (with-general-c-strings ((a^ a)
				 (b^ b))
	    (string-to-bytevector string->ascii)
	  (list (ascii->string a^)
		(ascii->string b^))))
    => '("ciao" "hello"))

;;; --------------------------------------------------------------------

  (check	;bytevectors
      (let ((a #ve(ascii "ciao"))
	    (b #ve(ascii "hello")))
        (with-general-c-strings ((a^ a)
				 (b^ b))
	    (string-to-bytevector string->ascii)
	  (list (ascii->string a^)
		(ascii->string b^))))
    => '("ciao" "hello"))

  (check	;pointers
      (let ((a (string->guarded-cstring "ciao"))
	    (b (string->guarded-cstring "hello")))
        (with-general-c-strings ((a^ a)
				 (b^ b))
	    (string-to-bytevector string->ascii)
	  (list (cstring->string a^)
		(cstring->string b^))))
    => '("ciao" "hello"))

  (check	;memory-blocks
      (let ((a (make-memory-block (string->guarded-cstring "ciao")  4))
	    (b (make-memory-block (string->guarded-cstring "hello") 5)))
        (with-general-c-strings ((a^ a)
				 (b^ b))
	    (string-to-bytevector string->ascii)
	  (list (cstring->string (memory-block-pointer a^))
		(cstring->string (memory-block-pointer b^)))))
    => '("ciao" "hello"))

  #t)


(parametrise ((check-test-name	'strings-false))

  (check	;empty
      (with-general-c-strings/false ()
	  (string-to-bytevector string->ascii)
	#t)
    => #t)

;;; --------------------------------------------------------------------

  (check	;string
      (let ((a "ciao"))
        (with-general-c-strings/false ((a^ a))
	    (string-to-bytevector string->ascii)
	  (ascii->string a^)))
    => "ciao")

  (check	;strings
      (let ((a "ciao")
	    (b "hello"))
        (with-general-c-strings/false ((a^ a)
				       (b^ b))
	    (string-to-bytevector string->ascii)
	  (list (ascii->string a^)
		(ascii->string b^))))
    => '("ciao" "hello"))

;;; --------------------------------------------------------------------

  (check	;bytevectors
      (let ((a #ve(ascii "ciao"))
	    (b #ve(ascii "hello")))
        (with-general-c-strings/false ((a^ a)
				       (b^ b))
	    (string-to-bytevector string->ascii)
	  (list (ascii->string a^)
		(ascii->string b^))))
    => '("ciao" "hello"))

  (check	;pointers
      (let ((a (string->guarded-cstring "ciao"))
	    (b (string->guarded-cstring "hello")))
        (with-general-c-strings/false ((a^ a)
				       (b^ b))
	    (string-to-bytevector string->ascii)
	  (list (cstring->string a^)
		(cstring->string b^))))
    => '("ciao" "hello"))

  (check	;memory-blocks
      (let ((a (make-memory-block (string->guarded-cstring "ciao")  4))
	    (b (make-memory-block (string->guarded-cstring "hello") 5)))
        (with-general-c-strings/false ((a^ a)
				       (b^ b))
	    (string-to-bytevector string->ascii)
	  (list (cstring->string (memory-block-pointer a^))
		(cstring->string (memory-block-pointer b^)))))
    => '("ciao" "hello"))

;;; --------------------------------------------------------------------

  (check	;false
      (with-general-c-strings/false ((a^ #f))
	  (string-to-bytevector string->ascii)
	a^)
    => #f)

  #t)


(parametrise ((check-test-name	'pathnames))

  (check	;empty
      (with-general-c-pathnames ()
	#t)
    => #t)

;;; --------------------------------------------------------------------

  (check	;pathname
      (let ((a "ciao"))
        (with-general-c-pathnames ((a^ a))
	  (ascii->string a^)))
    => "ciao")

  (check	;pathnames
      (let ((a "ciao")
	    (b "hello"))
        (with-general-c-pathnames ((a^ a)
				   (b^ b))
	  (list (ascii->string a^)
		(ascii->string b^))))
    => '("ciao" "hello"))

;;; --------------------------------------------------------------------

  (check	;bytevectors
      (let ((a #ve(ascii "ciao"))
	    (b #ve(ascii "hello")))
        (with-general-c-pathnames ((a^ a)
				   (b^ b))
	  (list (ascii->string a^)
		(ascii->string b^))))
    => '("ciao" "hello"))

  (check	;pointers
      (let ((a (string->guarded-cstring "ciao"))
	    (b (string->guarded-cstring "hello")))
        (with-general-c-pathnames ((a^ a)
				   (b^ b))
	  (list (cstring->string a^)
		(cstring->string b^))))
    => '("ciao" "hello"))

  (check	;memory-blocks
      (let ((a (make-memory-block (string->guarded-cstring "ciao")  4))
	    (b (make-memory-block (string->guarded-cstring "hello") 5)))
        (with-general-c-pathnames ((a^ a)
				   (b^ b))
	  (list (cstring->string (memory-block-pointer a^))
		(cstring->string (memory-block-pointer b^)))))
    => '("ciao" "hello"))

  #t)


(parametrise ((check-test-name	'pathnames-false))

  (check	;empty
      (with-general-c-pathnames/false ()
	#t)
    => #t)

;;; --------------------------------------------------------------------

  (check	;pathname
      (let ((a "ciao"))
        (with-general-c-pathnames/false ((a^ a))
	  (ascii->string a^)))
    => "ciao")

  (check	;pathnames
      (let ((a "ciao")
	    (b "hello"))
        (with-general-c-pathnames/false ((a^ a)
					 (b^ b))
	  (list (ascii->string a^)
		(ascii->string b^))))
    => '("ciao" "hello"))

;;; --------------------------------------------------------------------

  (check	;bytevectors
      (let ((a #ve(ascii "ciao"))
	    (b #ve(ascii "hello")))
        (with-general-c-pathnames/false ((a^ a)
					 (b^ b))
	  (list (ascii->string a^)
		(ascii->string b^))))
    => '("ciao" "hello"))

  (check	;pointers
      (let ((a (string->guarded-cstring "ciao"))
	    (b (string->guarded-cstring "hello")))
        (with-general-c-pathnames/false ((a^ a)
					 (b^ b))
	  (list (cstring->string a^)
		(cstring->string b^))))
    => '("ciao" "hello"))

  (check	;memory-blocks
      (let ((a (make-memory-block (string->guarded-cstring "ciao")  4))
	    (b (make-memory-block (string->guarded-cstring "hello") 5)))
        (with-general-c-pathnames/false ((a^ a)
					 (b^ b))
	  (list (cstring->string (memory-block-pointer a^))
		(cstring->string (memory-block-pointer b^)))))
    => '("ciao" "hello"))

;;; --------------------------------------------------------------------

  (check	;false
      (with-general-c-pathnames/false ((a^ #f))
	a^)
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'with-general-c-strings 'scheme-indent-function 2)
;;eval: (put 'with-general-c-strings/false 'scheme-indent-function 2)
;;eval: (put 'with-general-c-pathnames 'scheme-indent-function 1)
;;eval: (put 'with-general-c-pathnames/false 'scheme-indent-function 1)
;;End:
