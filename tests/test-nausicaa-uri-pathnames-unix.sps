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
(check-display "*** testing Nausicaa libraries: file system pathnames, Unix library\n")


(parametrise ((check-test-name	'absolute/core))

;;; initialised with string

  (check
      (let ()
	(<absolute-unix-pathname> O (<> ("/path/to/file.ext")))
	((<absolute-unix-pathname> #:predicate) O))
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
	((<absolute-unix-pathname> #:predicate) O))
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
	((<absolute-pathname> #:predicate) O))
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
	((<pathname> #:predicate) O))
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
	((<relative-unix-pathname> #:predicate) O))
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
	((<relative-unix-pathname> #:predicate) O))
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
	((<relative-pathname> #:predicate) O))
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
	((<pathname> #:predicate) O))
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
	((<absolute-unix-pathname> #:predicate) O))
    => #t)

  (check
      (let ((O (pathname "path/to/file.ext")))
	((<relative-unix-pathname> #:predicate) O))
    => #t)

;;; initialised with bytevector

  (check
      (let ((O (pathname '#ve(ascii "/path/to/file.ext"))))
	((<absolute-unix-pathname> #:predicate) O))
    => #t)

  (check
      (let ((O (pathname '#ve(ascii "path/to/file.ext"))))
	((<relative-unix-pathname> #:predicate) O))
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


(parametrise ((check-test-name	'components/tailname))

  (define-syntax-rule (doit ?pathname ?expected)
    (check
	(let (((O <pathname>) (pathname ?pathname)))
	  (O tailname))
      (=> pathname=?)
      (pathname ?expected)))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext"		"file.ext")
  (doit "file.ext"			"file.ext")
  (doit "/file.ext"			"file.ext")
  (doit "/file.ext//"			"file.ext")
  (doit "//file.ext"			"file.ext")
  (doit "/path/to///file.ext"		"file.ext")
  (doit "//////file.ext"		"file.ext")
  (doit "a/b"				"b")
  (doit "a"				"a")
  (doit "../a"				"a")
  (doit "./a"				"a")
  (doit "../abcd"			"abcd")
  (doit "./abcd"			"abcd")
  (doit "../abcd/efgh"			"efgh")
  (doit "./abcd/efgh"			"efgh")
  (doit "/ciao/"			"ciao")
  (doit "ciao/"				"ciao")
  (doit "./ciao/"			"ciao")
  (doit "hello/ciao/"			"ciao")
  (doit "ciao//////"			"ciao")
  (doit "/"				".")
  (doit "//////"			".")

  #t)


(parametrise ((check-test-name	'components/rootname))

  (define-syntax-rule (doit ?pathname ?expected)
    (check
	(let (((O <pathname>) (pathname ?pathname)))
	  (O rootname))
      (=> pathname=?)
      (pathname ?expected)))

;;; --------------------------------------------------------------------

  (doit "ciao.it"			"ciao")
  (doit "ciao"				"ciao")
  (doit "/path/to/file.ext"		"/path/to/file")
  (doit "/path/to/file."		"/path/to/file")
  (doit "/path/to/file"			"/path/to/file")
  (doit "/path/to/file.ext/ab"		"/path/to/file.ext/ab")
  (doit "/path/to/some.file.ext"	"/path/to/some.file")
  (doit "a/"				"a")
  (doit "a."				"a")
  (doit "."				".")
  (doit ".."				"..")
  (doit "..."				"..")
  (doit ".a"				".a")
  (doit ".emacsrc"			".emacsrc")
  (doit "..a"				".")
  (doit "...a"				"..")
  (doit "..a.b"				"..a")
  (doit "~/."				"~/.")
  (doit "~/.."				"~/..")
  (doit "~/..."				"~/..")
  (doit "~/.a"				"~/.a")
  (doit "~/.emacsrc"			"~/.emacsrc")
  (doit "~/..a"				"~/.")
  (doit "~/...a"			"~/..")
  (doit "~/..a.b"			"~/..a")
  (doit "///"				"/")
  (doit "ciao///"			"ciao")
  (doit "ciao.it///"			"ciao")
  (doit "ciao.it.en///"			"ciao.it")

  #t)


(parametrise ((check-test-name	'components/split))

  (define-syntax-rule (doit ?pathname . ?expected)
    (check
	(let (((O <pathname>) (pathname ?pathname)))
	  (O split))
      => . ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext"		#t '(#ve(ascii "path") #ve(ascii "to") #ve(ascii "file.ext")))
  (doit "path/to/file.ext"		#f '(#ve(ascii "path") #ve(ascii "to") #ve(ascii "file.ext")))
  (doit "ciao//"			#f '(#ve(ascii "ciao")))
  (doit "/"				#t '())
  (doit "."				#f '())
  (doit ".."				#f '(#ve(ascii "..")))
  (doit "ciao/.."			#f '())
  (doit "/."				#t '())
  (doit "/.."				#t '())
  (doit "/ciao/.."			#t '())

  #t)


(parametrise ((check-test-name	'components/prefix))

  (define-syntax-rule (doit ?pathname1 ?pathname2 ?expected)
    (check
	(let (((A <pathname>) (pathname ?pathname1))
	      ((B <pathname>) (pathname ?pathname2)))
	  (A prefix? B))
      => ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext" "/path/to/file.ext"		#t)
  (doit "/path/to/"         "/path/to/file.ext"		#t)
  (doit "/path/from"        "/path/to/file.ext"		#f)

  #t)


(parametrise ((check-test-name	'components/suffix))

  (define-syntax-rule (doit ?pathname1 ?pathname2 ?expected)
    (check
	(let (((A <pathname>) (pathname ?pathname1))
	      ((B <pathname>) (pathname ?pathname2)))
	  (A suffix? B))
      => ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext" "/path/to/file.ext"		#t)
  (doit "/to/file.ext" "/path/to/file.ext"		#t)
  (doit "/from/file.ext" "/path/to/file.ext"		#f)

  #t)


(parametrise ((check-test-name	'components/append))

  (define-syntax-rule (doit ?pathname1 ?pathname2 ?expected)
    (check
	(let (((A <relative-pathname>) (pathname ?pathname1))
	      ((B <pathname>) (pathname ?pathname2)))
	  (A append B))
      (=> pathname=?)
      (pathname ?expected)))

;;; --------------------------------------------------------------------

  (doit "file.ext" "/path/to"			"/path/to/file.ext")
  (doit "path/to/file.ext" "/"			"/path/to/file.ext")
  (doit "path/to/file.ext" "."			"./path/to/file.ext")
  (doit "path/to/file.ext" ".."			"../path/to/file.ext")

  #t)


(parametrise ((check-test-name	'components/prepend))

  (define-syntax-rule (doit ?pathname1 ?pathname2 ?expected)
    (check
	(let (((A <pathname>) (pathname ?pathname1))
	      ((B <pathname>) (pathname ?pathname2)))
	  (A prepend B))
      (=> pathname=?)
      (pathname ?expected)))

;;; --------------------------------------------------------------------

  (doit "/path/to" "file.ext"			"/path/to/file.ext")
  (doit "/" "path/to/file.ext"			"/path/to/file.ext")
  (doit "." "path/to/file.ext"			"./path/to/file.ext")
  (doit ".." "path/to/file.ext"			"../path/to/file.ext")

  #t)


(parametrise ((check-test-name	'components/replace-extension))

  (define-syntax-rule (doit ?pathname ?extension ?expected)
    (check
	(let (((A <pathname>) (pathname ?pathname)))
	  (A replace-extension ?extension))
      (=> pathname=?)
      (pathname ?expected)))

  (define-syntax-rule (doit-special-pathname-error ?pathname)
    (check
	(try
	    (doit ?pathname "two" "nothing")
	  (catch E
	    (&unix-pathname-normalisation-error
	     (condition-message E))
	    (else E)))
      => "cannot append extension to special directory pathname"))

;;; --------------------------------------------------------------------

  (doit "file.one" "two"		"file.two")
  (doit "/path/to/file.one" "two"	"/path/to/file.two")
  (doit ".emacs" "elc"			".emacs.elc")
  (doit "/path/to/.emacs" "elc"		"/path/to/.emacs.elc")

  (doit-special-pathname-error "/")
  (doit-special-pathname-error "///")
  (doit-special-pathname-error ".")
  (doit-special-pathname-error "..")

  #t)


(parametrise ((check-test-name	'uri-representation))

  (define-syntax-rule (doit ?pathname ?expected)
    (check
	(let (((O <pathname>) (pathname ?pathname)))
	  (octets->string (O uri)))
      => ?expected))

;;; --------------------------------------------------------------------

  (doit "ciao.it"			"file:ciao.it")
  (doit "ciao"				"file:ciao")
  (doit "/path/to/file.ext"		"file:///path/to/file.ext")
  (doit "/path/to/file."		"file:///path/to/file.")
  (doit "/path/to/file"			"file:///path/to/file")
  (doit "/path/to/file.ext/ab"		"file:///path/to/file.ext/ab")
  (doit "/path/to/some.file.ext"	"file:///path/to/some.file.ext")
  (doit "a/"				"file:a")
  (doit "a."				"file:a.")
  (doit "."				"file:.")
  (doit ".."				"file:..")
  (doit "..."				"file:...")
  (doit ".a"				"file:.a")
  (doit ".emacsrc"			"file:.emacsrc")
  (doit "..a"				"file:..a")
  (doit "...a"				"file:...a")
  (doit "..a.b"				"file:..a.b")
  (doit "///"				"file:///")
  (doit "ciao///"			"file:ciao")
  (doit "ciao.it///"			"file:ciao.it")
  (doit "ciao.it.en///"			"file:ciao.it.en")

  #t)


;;;; done

(check-report)

;;; end of file
