;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for builtin posix functions
;;;Date: Wed Oct  9, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare $posix)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: built-in POSIX functions\n")


(parametrise ((check-test-name	'file-predicates))

  (check-for-true  (file-absolute-pathname? "/ciao"))
  (check-for-true  (file-absolute-pathname? "/ciao/hello.txt"))
  (check-for-false (file-absolute-pathname? "ciao"))
  (check-for-false (file-absolute-pathname? "./ciao"))
  (check-for-false (file-absolute-pathname? "ciao/ciao"))

;;; --------------------------------------------------------------------

  (check-for-false (file-relative-pathname? "/ciao"))
  (check-for-false (file-relative-pathname? "/ciao/hello.txt"))
  (check-for-true  (file-relative-pathname? "ciao"))
  (check-for-true  (file-relative-pathname? "./ciao"))
  (check-for-true  (file-relative-pathname? "ciao/ciao"))

;;; --------------------------------------------------------------------

  (check-for-false	(file-string-pathname? ""))
  (check-for-false	(file-string-pathname? "\x00;"))
  (check-for-false	(file-string-pathname? "ciao\x00;hello"))
  (check-for-false	(file-string-pathname? "ciao/\x00;/hello"))
  (check-for-true	(file-string-pathname? "."))
  (check-for-true	(file-string-pathname? "ciao"))
  (check-for-true	(file-string-pathname? "./ciao"))
  (check-for-true	(file-string-pathname? "/ciao"))

  (check-for-false	(file-bytevector-pathname? '#ve(ascii "")))
  (check-for-false	(file-bytevector-pathname? '#ve(ascii "\x00;")))
  (check-for-false	(file-bytevector-pathname? '#ve(ascii "ciao\x00;hello")))
  (check-for-false	(file-bytevector-pathname? '#ve(ascii "ciao/\x00;/hello")))
  (check-for-true	(file-bytevector-pathname? '#ve(ascii ".")))
  (check-for-true	(file-bytevector-pathname? '#ve(ascii "ciao")))
  (check-for-true	(file-bytevector-pathname? '#ve(ascii "./ciao")))
  (check-for-true	(file-bytevector-pathname? '#ve(ascii "/ciao")))

  (check-for-false	(file-pathname? ""))
  (check-for-false	(file-pathname? "\x00;"))
  (check-for-false	(file-pathname? "ciao\x00;hello"))
  (check-for-false	(file-pathname? "ciao/\x00;/hello"))
  (check-for-true	(file-pathname? "."))
  (check-for-true	(file-pathname? "ciao"))
  (check-for-true	(file-pathname? "./ciao"))
  (check-for-true	(file-pathname? "/ciao"))

  (check-for-false	(file-pathname? '#ve(ascii "")))
  (check-for-false	(file-pathname? '#ve(ascii "\x00;")))
  (check-for-false	(file-pathname? '#ve(ascii "ciao\x00;hello")))
  (check-for-false	(file-pathname? '#ve(ascii "ciao/\x00;/hello")))
  (check-for-true	(file-pathname? '#ve(ascii ".")))
  (check-for-true	(file-pathname? '#ve(ascii "ciao")))
  (check-for-true	(file-pathname? '#ve(ascii "./ciao")))
  (check-for-true	(file-pathname? '#ve(ascii "/ciao")))

;;; --------------------------------------------------------------------

  (check-for-true	(file-string-colon-search-path? ""))
  (check-for-false	(file-string-colon-search-path? "\x00;"))
  (check-for-false	(file-string-colon-search-path? "ciao\x00;hello"))
  (check-for-false	(file-string-colon-search-path? "ciao:\x00;:hello"))
  (check-for-true	(file-string-colon-search-path? "."))
  (check-for-true	(file-string-colon-search-path? "ciao"))
  (check-for-true	(file-string-colon-search-path? ".:ciao"))
  (check-for-true	(file-string-colon-search-path? ":ciao"))

  (check-for-true	(file-bytevector-colon-search-path? '#ve(ascii "")))
  (check-for-false	(file-bytevector-colon-search-path? '#ve(ascii "\x00;")))
  (check-for-false	(file-bytevector-colon-search-path? '#ve(ascii "ciao\x00;hello")))
  (check-for-false	(file-bytevector-colon-search-path? '#ve(ascii "ciao:\x00;:hello")))
  (check-for-true	(file-bytevector-colon-search-path? '#ve(ascii ".")))
  (check-for-true	(file-bytevector-colon-search-path? '#ve(ascii "ciao")))
  (check-for-true	(file-bytevector-colon-search-path? '#ve(ascii ".:ciao")))
  (check-for-true	(file-bytevector-colon-search-path? '#ve(ascii ":ciao")))

  (check-for-true	(file-colon-search-path? ""))
  (check-for-false	(file-colon-search-path? "\x00;"))
  (check-for-false	(file-colon-search-path? "ciao\x00;hello"))
  (check-for-false	(file-colon-search-path? "ciao:\x00;:hello"))
  (check-for-true	(file-colon-search-path? "."))
  (check-for-true	(file-colon-search-path? "ciao"))
  (check-for-true	(file-colon-search-path? ".:ciao"))
  (check-for-true	(file-colon-search-path? ":ciao"))

  (check-for-true	(file-colon-search-path? '#ve(ascii "")))
  (check-for-false	(file-colon-search-path? '#ve(ascii "\x00;")))
  (check-for-false	(file-colon-search-path? '#ve(ascii "ciao\x00;hello")))
  (check-for-false	(file-colon-search-path? '#ve(ascii "ciao:\x00;:hello")))
  (check-for-true	(file-colon-search-path? '#ve(ascii ".")))
  (check-for-true	(file-colon-search-path? '#ve(ascii "ciao")))
  (check-for-true	(file-colon-search-path? '#ve(ascii ".:ciao")))
  (check-for-true	(file-colon-search-path? '#ve(ascii ":ciao")))

  #t)


(parametrise ((check-test-name	'split-search-path))

  (check
      (receive (root tail)
	  (split-pathname-root-and-tail "a/b")
	(list root tail))
    => '("a" "b"))

  (check
      (receive (root tail)
	  (split-pathname-root-and-tail "ciao")
	(list root tail))
    => '("" "ciao"))

  #t)


(parametrise ((check-test-name	'split-search-path))

  (check
      (split-search-path-bytevector '#vu8())
    => '())

  (check
      (split-search-path-bytevector '#ve(ascii "ciao"))
    => '(#ve(ascii "ciao")))

  (check
      (split-search-path-bytevector '#ve(ascii "ciao:"))
    => '(#ve(ascii "ciao")))

  (check
      (split-search-path-bytevector '#ve(ascii ":ciao"))
    => '(#ve(ascii "ciao")))

  (check
      (split-search-path-bytevector '#ve(ascii ":"))
    => '())

  (check
      (split-search-path-bytevector '#ve(ascii "::::"))
    => '())

  (check
      (split-search-path-bytevector '#ve(ascii "ciao:hello"))
    => '(#ve(ascii "ciao") #ve(ascii "hello")))

  (check
      (split-search-path-bytevector '#ve(ascii "ciao:hello:salut"))
    => '(#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  (check
      (split-search-path-bytevector '#ve(ascii "ciao:::hello"))
    => '(#ve(ascii "ciao") #ve(ascii "hello")))

;;; --------------------------------------------------------------------

  (check
      (split-search-path-string "")
    => '())

  (check
      (split-search-path-string "ciao")
    => '("ciao"))

  (check
      (split-search-path-string "ciao:")
    => '("ciao"))

  (check
      (split-search-path-string ":ciao")
    => '("ciao"))

  (check
      (split-search-path-string ":")
    => '())

  (check
      (split-search-path-string "::::")
    => '())

  (check
      (split-search-path-string "ciao:hello")
    => '("ciao" "hello"))

  (check
      (split-search-path-string "ciao:hello:salut")
    => '("ciao" "hello" "salut"))

  (check
      (split-search-path-string "ciao:::hello")
    => '("ciao" "hello"))

;;; --------------------------------------------------------------------

  (check
      (split-search-path "")
    => '())

  (check
      (split-search-path '#vu8())
    => '())

  (check
      (split-search-path "ciao:hello:salut")
    => '("ciao" "hello" "salut"))

  (check
      (split-search-path '#ve(ascii "ciao:hello:salut"))
    => '(#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  #t)


(parametrise ((check-test-name	'split-pathname))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?form => ?abs-result ?comp-result)
       (check
	   (call-with-values
	       (lambda () ?form)
	     list)
	 => '(?abs-result ?comp-result)))))

  (doit (split-pathname-bytevector '#ve(ascii "ciao"))
	=> #f (#ve(ascii "ciao")))

  (doit (split-pathname-bytevector '#ve(ascii "ciao/"))
	=> #f (#ve(ascii "ciao")))

  (doit (split-pathname-bytevector '#ve(ascii "/ciao"))
	=> #t (#ve(ascii "ciao")))

  (doit (split-pathname-bytevector '#ve(ascii "/"))
	=> #t ())

  (doit (split-pathname-bytevector '#ve(ascii "////"))
	=> #t ())

  (doit (split-pathname-bytevector '#ve(ascii "ciao/hello"))
	=> #f (#ve(ascii "ciao") #ve(ascii "hello")))

  (doit (split-pathname-bytevector '#ve(ascii "ciao/hello/salut"))
	=> #f (#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  (doit (split-pathname-bytevector '#ve(ascii "/ciao/hello/salut"))
	=> #t (#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  (doit (split-pathname-bytevector '#ve(ascii "ciao///hello"))
	=> #f (#ve(ascii "ciao") #ve(ascii "hello")))

;;; --------------------------------------------------------------------

  (doit (split-pathname-string "ciao")
	=> #f ("ciao"))

  (doit (split-pathname-string "ciao/")
	=> #f ("ciao"))

  (doit (split-pathname-string "/ciao")
	=> #t ("ciao"))

  (doit (split-pathname-string "/")
	=> #t ())

  (doit (split-pathname-string "////")
	=> #t ())

  (doit (split-pathname-string "ciao/hello")
	=> #f ("ciao" "hello"))

  (doit (split-pathname-string "ciao/hello/salut")
	=> #f ("ciao" "hello" "salut"))

  (doit (split-pathname-string "ciao///hello")
	=> #f ("ciao" "hello"))

;;; --------------------------------------------------------------------

  (doit (split-pathname "ciao/hello/salut")
	=> #f ("ciao" "hello" "salut"))

  (doit (split-pathname "/ciao/hello/salut")
	=> #t ("ciao" "hello" "salut"))

  (doit (split-pathname '#ve(ascii "ciao/hello/salut"))
	=> #f (#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  #t)


;;;; done

(check-report)

;;; end of file
