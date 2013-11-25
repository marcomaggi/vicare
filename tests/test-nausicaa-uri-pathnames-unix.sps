;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the Unix pathnames library
;;;Date: Sun Nov 24, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (nausicaa uri pathnames unix)
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Nausicaa libraries: file system pathnames, Unix library\n")


(parametrise ((check-test-name	'absolute/core))

;;; initialised with string

  (check
      (let ()
	(<absolute-unix-pathname> O (<> ("/path/to/file.ext")))
	((<absolute-unix-pathname>) O))
    => #t)

  (check
      (let ()
	(<absolute-unix-pathname> O (<> ("/path/to/file.ext")))
	(O bytevector))
    => '#ve(ascii "/path/to/file.ext"))

  (check
      (let ()
	(<absolute-unix-pathname> O (<> ("/path/to/file.ext")))
	(O string))
    => "/path/to/file.ext")

;;; --------------------------------------------------------------------
;;; initialised with bytevector

  (check
      (let ()
	(<absolute-unix-pathname> O (<> ('#ve(ascii "/path/to/file.ext"))))
	((<absolute-unix-pathname>) O))
    => #t)

  (check
      (let ()
	(<absolute-unix-pathname> O (<> ('#ve(ascii "/path/to/file.ext"))))
	(O bytevector))
    => '#ve(ascii "/path/to/file.ext"))

  (check
      (let ()
	(<absolute-unix-pathname> O (<> ('#ve(ascii "/path/to/file.ext"))))
	(O string))
    => "/path/to/file.ext")

;;; --------------------------------------------------------------------
;;; access through platform-agnostic tag

  (check
      (let ()
	(<absolute-pathname> O (<absolute-unix-pathname> ("/path/to/file.ext")))
	((<absolute-pathname>) O))
    => #t)

  (check
      (let ()
	(<absolute-pathname> O (<absolute-unix-pathname> ("/path/to/file.ext")))
	(O bytevector))
    => '#ve(ascii "/path/to/file.ext"))

  (check
      (let ()
	(<absolute-pathname> O (<absolute-unix-pathname> ("/path/to/file.ext")))
	(O string))
    => "/path/to/file.ext")

;;; --------------------------------------------------------------------
;;; access through platform-agnostic and category-agnostic tag

  (check
      (let ()
	(<pathname> O (<absolute-unix-pathname> ("/path/to/file.ext")))
	((<pathname>) O))
    => #t)

  (check
      (let ()
	(<pathname> O (<absolute-unix-pathname> ("/path/to/file.ext")))
	(O bytevector))
    => '#ve(ascii "/path/to/file.ext"))

  (check
      (let ()
	(<pathname> O (<absolute-unix-pathname> ("/path/to/file.ext")))
	(O string))
    => "/path/to/file.ext")

;;; --------------------------------------------------------------------
;;; errors

  (check	;expected absolute, initialised with relative
      (try
	  (let ()
	    (<absolute-unix-pathname> O (<> ("path/to/file.ext")))
	    O)
	(catch E
	  (&procedure-argument-violation
	   #t)
	  (else E)))
    => #t)

  #t)


(parametrise ((check-test-name	'relative/core))

;;; initialised with string

  (check
      (let ()
	(<relative-unix-pathname> O (<> ("path/to/file.ext")))
	((<relative-unix-pathname>) O))
    => #t)

  (check
      (let ()
	(<relative-unix-pathname> O (<> ("path/to/file.ext")))
	(O bytevector))
    => '#ve(ascii "path/to/file.ext"))

  (check
      (let ()
	(<relative-unix-pathname> O (<> ("path/to/file.ext")))
	(O string))
    => "path/to/file.ext")

;;; --------------------------------------------------------------------
;;; initialised with bytevector

  (check
      (let ()
	(<relative-unix-pathname> O (<> ('#ve(ascii "path/to/file.ext"))))
	((<relative-unix-pathname>) O))
    => #t)

  (check
      (let ()
	(<relative-unix-pathname> O (<> ('#ve(ascii "path/to/file.ext"))))
	(O bytevector))
    => '#ve(ascii "path/to/file.ext"))

  (check
      (let ()
	(<relative-unix-pathname> O (<> ('#ve(ascii "path/to/file.ext"))))
	(O string))
    => "path/to/file.ext")

;;; --------------------------------------------------------------------
;;; access through platform-agnostic tag

  (check
      (let ()
	(<relative-pathname> O (<relative-unix-pathname> ("path/to/file.ext")))
	((<relative-pathname>) O))
    => #t)

  (check
      (let ()
	(<relative-pathname> O (<relative-unix-pathname> ("path/to/file.ext")))
	(O bytevector))
    => '#ve(ascii "path/to/file.ext"))

  (check
      (let ()
	(<relative-pathname> O (<relative-unix-pathname> ("path/to/file.ext")))
	(O string))
    => "path/to/file.ext")

;;; --------------------------------------------------------------------
;;; access through platform-agnostic and category-agnostic tag

  (check
      (let ()
	(<pathname> O (<relative-unix-pathname> ("path/to/file.ext")))
	((<pathname>) O))
    => #t)

  (check
      (let ()
	(<pathname> O (<relative-unix-pathname> ("path/to/file.ext")))
	(O bytevector))
    => '#ve(ascii "path/to/file.ext"))

  (check
      (let ()
	(<pathname> O (<relative-unix-pathname> ("path/to/file.ext")))
	(O string))
    => "path/to/file.ext")

;;; --------------------------------------------------------------------

  (check	;expected relative, initialised with absolute
      (try
	  (let ()
	    (<relative-unix-pathname> O (<> ("/path/to/file.ext")))
	    O)
	(catch E
	  (&procedure-argument-violation
	   #t)
	  (else E)))
    => #t)

  #t)


(parametrise ((check-test-name	'generic-constructor))

;;; initialised with string

  (check
      (let ((O (pathname "/path/to/file.ext")))
	((<absolute-unix-pathname>) O))
    => #t)

  (check
      (let ((O (pathname "path/to/file.ext")))
	((<relative-unix-pathname>) O))
    => #t)

;;; initialised with bytevector

  (check
      (let ((O (pathname '#ve(ascii "/path/to/file.ext"))))
	((<absolute-unix-pathname>) O))
    => #t)

  (check
      (let ((O (pathname '#ve(ascii "path/to/file.ext"))))
	((<relative-unix-pathname>) O))
    => #t)

  #t)


(parametrise ((check-test-name	'components/extension))

  (define-syntax-rule (doit ?pathname ?expected)
    (check
	(let (((O <pathname>) (pathname ?pathname)))
	  (O extension))
      => (string->ascii ?expected)))

;;; --------------------------------------------------------------------

  (doit "ciao.it"			"it")
  (doit "ciao"				"")
  (doit "/path/to/file.ext"		"ext")
  (doit "/path/to/file."		"")
  (doit "/path/to/file"			"")
  (doit "/path/to/file.ext/ab"		"")
  (doit "/path/to/some.file.ext"	"ext")
  (doit "ciao.it//"			"it")
  (doit "a/"				"")
  (doit "a."				"")
  (doit "."				"")
  (doit ".."				"")
  (doit "..."				"")
  (doit ".a"				"")
  (doit ".emacsrc"			"")
  (doit "..a"				"a")
  (doit "...a"				"a")
  (doit "..a.b"				"b")
  (doit "~/."				"")
  (doit "~/.."				"")
  (doit "~/..."				"")
  (doit "~/.a"				"")
  (doit "~/.emacsrc"			"")
  (doit "~/..a"				"a")
  (doit "~/...a"			"a")
  (doit "~/..a.b"			"b")

  #t)


(parametrise ((check-test-name	'components/dirname))

  (define-syntax-rule (doit ?pathname ?expected)
    (check
	(let (((O <pathname>) (pathname ?pathname)))
	  (O dirname))
      (=> pathname=?)
      (pathname ?expected)))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext"		"/path/to")
  (doit "file.ext"			".")
  (doit "/file.ext"			"/")
  (doit "/file.ext//"			"/")
  (doit "//file.ext"			"/")
  (doit "/path/to///file.ext"		"/path/to")
  (doit "//////file.ext"		"/")
  (doit "//////file.ext//"		"/")
  (doit "a/b"				"a")
  (doit "a"				".")
  (doit "../a"				"..")
  (doit "./a"				".")
  (doit "../abcd"			"..")
  (doit "./abcd"			".")
  (doit "../abcd/efgh"			"../abcd")
  (doit "./abcd/efgh"			"./abcd")
  (doit "/ciao/"			"/")
  (doit "ciao/"				".")
  (doit "./ciao/"			".")
  (doit "hello/ciao/"			"hello")
  (doit "//////"			"/")
  (doit "ciao//////"			".")

  #t)


;;;; done

(check-report)

;;; end of file
