;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for Unix pathnames parser functions
;;;Date: Wed Nov 20, 2013
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
  (prefix (vicare parser-tools unix-pathnames) uxptn.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: Unix pathnames parser functions\n")


;;;; helpers

(define (mkport obj)
  (cond ((string? obj)
	 (open-bytevector-input-port (string->utf8 obj)))
	((bytevector? obj)
	 (open-bytevector-input-port obj))
	(else
	 (assertion-violation 'mkport "expecting string or bytevector" obj))))


(parametrise ((check-test-name	'predicates))

  (check-for-false (uxptn.pathname? ""))
  (check-for-true (uxptn.pathname? "."))
  (check-for-true (uxptn.pathname? ".."))
  (check-for-true (uxptn.pathname? "/"))
  (check-for-true (uxptn.pathname? "ciao"))
  (check-for-true (uxptn.pathname? "ciao/hello"))

;;; --------------------------------------------------------------------

  (check-for-false (uxptn.pathname? '#ve(ascii "")))
  (check-for-true (uxptn.pathname? '#ve(ascii ".")))
  (check-for-true (uxptn.pathname? '#ve(ascii "..")))
  (check-for-true (uxptn.pathname? '#ve(ascii "/")))
  (check-for-true (uxptn.pathname? '#ve(ascii "ciao")))
  (check-for-true (uxptn.pathname? '#ve(ascii "ciao/hello")))

;;; --------------------------------------------------------------------

  (check-for-false (uxptn.pathname? "ciao\x1FF;"))
  (check-for-false (uxptn.pathname? "ciao\x0;"))

;;; --------------------------------------------------------------------

  (check-for-true (uxptn.segment? ""))
  (check-for-true (uxptn.segment? "."))
  (check-for-true (uxptn.segment? ".."))
  (check-for-false (uxptn.segment? "/"))
  (check-for-true (uxptn.segment? "ciao"))
  (check-for-false (uxptn.segment? "ciao/hello"))

;;; --------------------------------------------------------------------

  (check-for-true (uxptn.segment? '#ve(ascii "")))
  (check-for-true (uxptn.segment? '#ve(ascii ".")))
  (check-for-true (uxptn.segment? '#ve(ascii "..")))
  (check-for-false (uxptn.segment? '#ve(ascii "/")))
  (check-for-true (uxptn.segment? '#ve(ascii "ciao")))
  (check-for-false (uxptn.segment? '#ve(ascii "ciao/hello")))

;;; --------------------------------------------------------------------

  (check-for-false (uxptn.segment? "ciao\x1FF;"))
  (check-for-false (uxptn.segment? "ciao\x0;"))

;;; --------------------------------------------------------------------

  (check-for-true (uxptn.list-of-segments? '("")))
  (check-for-true (uxptn.list-of-segments? '(".")))
  (check-for-true (uxptn.list-of-segments? '("..")))
  (check-for-false (uxptn.list-of-segments? '("/")))
  (check-for-true (uxptn.list-of-segments? '("ciao")))
  (check-for-false (uxptn.list-of-segments? '("ciao/hello")))
  (check-for-true (uxptn.list-of-segments? '("ciao" "hello")))

;;; --------------------------------------------------------------------

  (check-for-true (uxptn.list-of-segments? '(#ve(ascii ""))))
  (check-for-true (uxptn.list-of-segments? '(#ve(ascii "."))))
  (check-for-true (uxptn.list-of-segments? '(#ve(ascii ".."))))
  (check-for-false (uxptn.list-of-segments? '(#ve(ascii "/"))))
  (check-for-true (uxptn.list-of-segments? '(#ve(ascii "ciao"))))
  (check-for-false (uxptn.list-of-segments? '(#ve(ascii "ciao/hello"))))
  (check-for-true (uxptn.list-of-segments? '(#ve(ascii "ciao") #ve(ascii "hello"))))

;;; --------------------------------------------------------------------

  (check-for-false (uxptn.list-of-segments? '("ciao\x1FF;")))
  (check-for-false (uxptn.list-of-segments? '("ciao\x0;")))

  #t)


(parametrise ((check-test-name	'conversion))

  (check
      (uxptn.string/bytevector->pathname-bytevector "")
    => '#ve(ascii "."))

  (check
      (uxptn.string/bytevector->pathname-bytevector ".")
    => '#ve(ascii "."))

  (check
      (uxptn.string/bytevector->pathname-bytevector "..")
    => '#ve(ascii ".."))

  (check
      (uxptn.string/bytevector->pathname-bytevector "/")
    => '#ve(ascii "/"))

  (check
      (uxptn.string/bytevector->pathname-bytevector "ciao")
    => '#ve(ascii "ciao"))

  (check
      (uxptn.string/bytevector->pathname-bytevector "ciao/hello")
    => '#ve(ascii "ciao/hello"))

;;; --------------------------------------------------------------------

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii ""))
    => '#ve(ascii "."))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii "."))
    => '#ve(ascii "."))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii ".."))
    => '#ve(ascii ".."))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii "/"))
    => '#ve(ascii "/"))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii "ciao"))
    => '#ve(ascii "ciao"))

  (check
      (uxptn.string/bytevector->pathname-bytevector '#ve(ascii "ciao/hello"))
    => '#ve(ascii "ciao/hello"))

;;; --------------------------------------------------------------------

  (check
      (uxptn.pathname-bytevector->string #vu8())
    => ".")

  (check
      (uxptn.pathname-bytevector->string (uxptn.string/bytevector->pathname-bytevector "ciao"))
    => "ciao")

  (check
      (uxptn.pathname-bytevector->string (uxptn.string/bytevector->pathname-bytevector "ciao/hello"))
    => "ciao/hello")

;;; --------------------------------------------------------------------

  (check
      (guard (E ((uxptn.unix-pathname-parser-error? E)
		 #t)
		(else E))
	(uxptn.string/bytevector->pathname-bytevector "ciao\x1FF;"))
    => #t)

  (check
      (guard (E ((uxptn.unix-pathname-parser-error? E)
		 #t)
		(else E))
	(uxptn.string/bytevector->pathname-bytevector "ciao\x0;"))
    => #t)

  #t)


(parametrise ((check-test-name	'parse-segments))

;;; path segment

  (check
      (uxptn.parse-segment (mkport ""))
    => '#ve(ascii ""))

  (check
      (let* ((in-port	(mkport "ciao"))
	     (segment	(uxptn.pathname-bytevector->string (uxptn.parse-segment in-port)))
	     (eof	(lookahead-u8 in-port)))
	(values segment eof))
    => "ciao" (eof-object))

  (check
      (uxptn.pathname-bytevector->string (uxptn.parse-segment (mkport "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (uxptn.pathname-bytevector->string (uxptn.parse-segment (mkport "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (uxptn.pathname-bytevector->string (uxptn.parse-segment (mkport "ciao!$&'()*+,;=:@-._~")))
    => "ciao!$&'()*+,;=:@-._~")

  (check
      (let* ((in-port	(mkport "/hello"))
	     (segment1	(uxptn.pathname-bytevector->string (uxptn.parse-segment in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(uxptn.pathname-bytevector->string (uxptn.parse-segment in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("." #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(mkport "ciao/hello"))
	     (segment1	(uxptn.pathname-bytevector->string (uxptn.parse-segment in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(uxptn.pathname-bytevector->string (uxptn.parse-segment in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

;;; --------------------------------------------------------------------
;;; path segment-nz

  (check
      (uxptn.parse-segment-nz (mkport ""))
    => #f)

  (check
      (let* ((in-port	(mkport "{"))
  	     (segment	(uxptn.pathname-bytevector->string (uxptn.parse-segment-nz in-port))))
  	(list segment (eof-object? (lookahead-u8 in-port))))
    => '("{" #t))

  (check
      (let* ((in-port	(mkport "/"))
  	     (segment	(uxptn.parse-segment-nz in-port))
  	     (char	(integer->char (get-u8 in-port))))
  	(list segment char))
    => '(#f #\/))

  (check
      (let* ((in-port	(mkport "ciao"))
  	     (segment	(uxptn.pathname-bytevector->string (uxptn.parse-segment-nz in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao" ,(eof-object)))

  (check
      (let* ((in-port	(mkport "ciao:ciao"))
  	     (segment	(uxptn.pathname-bytevector->string (uxptn.parse-segment-nz in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao:ciao" ,(eof-object)))

  (check
      (let* ((in-port	(mkport "ciao/hello"))
  	     (segment1	(uxptn.pathname-bytevector->string (uxptn.parse-segment-nz in-port)))
  	     (slash	(integer->char (get-u8 in-port)))
  	     (segment2	(uxptn.pathname-bytevector->string (uxptn.parse-segment-nz in-port))))
  	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(mkport "ciao%3dciao"))
  	     (segment	(uxptn.pathname-bytevector->string (uxptn.parse-segment-nz in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao%3dciao" ,(eof-object)))

  (let ((S "ciao%3d%3dciao"))
    (check
  	(let* ((in-port	(mkport S))
  	       (segment	(uxptn.pathname-bytevector->string (uxptn.parse-segment-nz in-port))))
  	  (list segment (lookahead-u8 in-port)))
      => `(,S ,(eof-object))))

;;; --------------------------------------------------------------------
;;; slash and segment

  (check
      (uxptn.parse-slash-and-segment (mkport ""))
    => #f)

  (check
      (let* ((in-port	(mkport "ciao"))
  	     (segment	(uxptn.parse-slash-and-segment in-port))
  	     (rest	(uxptn.pathname-bytevector->string (get-bytevector-some in-port))))
  	(list segment rest))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(mkport "/"))
  	     (segment	(uxptn.parse-slash-and-segment in-port)))
  	(list segment (lookahead-u8 in-port)))
    => `(#vu8(46) ,(eof-object)))

  (check
      (let* ((in-port	(mkport "/ciao/hello"))
  	     (segment1	(uxptn.pathname-bytevector->string (uxptn.parse-slash-and-segment in-port)))
  	     (segment2	(uxptn.pathname-bytevector->string (uxptn.parse-slash-and-segment in-port))))
  	(list segment1 segment2 (lookahead-u8 in-port)))
    => `("ciao" "hello" ,(eof-object)))

  (check
      (let* ((in-port	(mkport "/ciao/hello/"))
  	     (segment1	(uxptn.pathname-bytevector->string (uxptn.parse-slash-and-segment in-port)))
  	     (segment2	(uxptn.pathname-bytevector->string (uxptn.parse-slash-and-segment in-port)))
  	     (segment3	(uxptn.pathname-bytevector->string (uxptn.parse-slash-and-segment in-port))))
  	(list segment1 segment2 segment3 (lookahead-u8 in-port)))
    => `("ciao" "hello" "." ,(eof-object)))

  (check
      (let* ((in-port	(mkport "/ciao%3dciao"))
  	     (segment	(uxptn.pathname-bytevector->string (uxptn.parse-slash-and-segment in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao%3dciao" ,(eof-object)))

  (check
      (let* ((in-port	(mkport "/ciao%3d%3dciao"))
  	     (segment	(uxptn.pathname-bytevector->string (uxptn.parse-slash-and-segment in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao%3d%3dciao" ,(eof-object)))

  #t)


(parametrise ((check-test-name	'parse-pathname))

  (check
      (receive (absolute? segments)
	  (uxptn.parse-pathname (mkport "/"))
	(list absolute? segments))
    => '(#t (#vu8(46))))

  (check
      (receive (absolute? segments)
	  (uxptn.parse-pathname (mkport "ciao"))
	(list absolute? (map uxptn.pathname-bytevector->string segments)))
    => '(#f ("ciao")))

  (check
      (receive (absolute? segments)
	  (uxptn.parse-pathname (mkport "ciao/salut"))
	(list absolute? (map uxptn.pathname-bytevector->string segments)))
    => '(#f ("ciao" "salut")))

  (check
      (receive (absolute? segments)
	  (uxptn.parse-pathname (mkport "/ciao/salut"))
	(list absolute? (map uxptn.pathname-bytevector->string segments)))
    => '(#t ("ciao" "salut")))

  (check
      (receive (absolute? segments)
	  (uxptn.parse-pathname (mkport "ciao/salut/hello"))
	(list absolute? (map uxptn.pathname-bytevector->string segments)))
    => '(#f ("ciao" "salut" "hello")))

  (check
      (receive (absolute? segments)
	  (uxptn.parse-pathname (mkport "/ciao/salut/hello"))
	(list absolute? (map uxptn.pathname-bytevector->string segments)))
    => '(#t ("ciao" "salut" "hello")))

  (check
      (receive (absolute? segments)
	  (uxptn.parse-pathname (mkport "ciao/./.."))
	(list absolute? (map uxptn.pathname-bytevector->string segments)))
    => '(#f ("ciao" "." "..")))

  (check
      (receive (absolute? segments)
	  (uxptn.parse-pathname (mkport "///"))
	(list absolute? (map uxptn.pathname-bytevector->string segments)))
    => '(#t ("." "." ".")))

;;; --------------------------------------------------------------------

  (check
      (let ((in-port (mkport "")))
	(guard (E ((uxptn.unix-pathname-parser-error? E)
		   (get-bytevector-some in-port))
		  (else E))
	  (uxptn.parse-pathname in-port)))
    => (eof-object))

  (check
      (let ((in-port (mkport "/ciao/salut/he#\x0;llo")))
	(guard (E ((uxptn.unix-pathname-parser-error? E)
		   (utf8->string (get-bytevector-some in-port)))
		  (else E))
	  (uxptn.parse-pathname in-port)))
    => "/ciao/salut/he#\x0;llo")

  #t)


(parametrise ((check-test-name	'normalise))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?absolute ?input ?changed ?output)
       (check
	   (receive (changed? bvs)
	       (uxptn.normalise-segments ?absolute (map uxptn.string/bytevector->pathname-bytevector (quote ?input)))
	     (list changed? (and bvs (map uxptn.pathname-bytevector->string bvs))))
	 => '(?changed ?output)))))

  (doit #f () #f ())

  (doit #t ("a") #f ("a"))
  (doit #f ("a") #f ("a"))

  (doit #t (".") #t ())
  (doit #f (".") #t ())

  (doit #t ("..") #t ())
  (doit #f ("..") #f (".."))

  (doit #t (".." "..") #t ())
  (doit #f (".." "..") #f (".." ".."))

  (doit #t ("ciao" "." ".") #t ("ciao"))
  (doit #f ("ciao" "." ".") #t ("ciao"))

  (doit #f ("a" "b" "c" "." ".." ".." "g")
	#t ("a" "g"))
  (doit #f ("a" "b" "c" "." ".." ".." "g")
	#t ("a" "g"))

  (doit #t ("ciao" "hello" "..") #t ("ciao"))
  (doit #f ("ciao" "hello" "..") #t ("ciao"))

  (doit #t ("ciao" "hello" ".." "..") #t ())
  (doit #f ("ciao" "hello" ".." "..") #t ())

  (doit #t ("ciao" "hello" ".." ".." "..") #t ())
  (doit #f ("ciao" "hello" ".." ".." "..") #t (".."))

  (doit #t ("ciao" "hello" ".." ".." ".." "..") #t ())
  (doit #f ("ciao" "hello" ".." ".." ".." "..") #t (".." ".."))

  (doit #t ("a" "." "." "." "." "." ".") #t ("a"))
  (doit #f ("a" "." "." "." "." "." ".") #t ("a"))

  (doit #t ("." "." "." "." "." ".") #t ())
  (doit #f ("." "." "." "." "." ".") #t ())

  (doit #t ("." "a" "..") #t ())
  (doit #f ("." "a" "..") #t ())

  (doit #t ("a" ".." "b") #t ("b"))
  (doit #f ("a" ".." "b") #t ("b"))

  (doit #t ("a" ".." "b" ".." "c") #t ("c"))
  (doit #f ("a" ".." "b" ".." "c") #t ("c"))

  #t)


(parametrise ((check-test-name	'serialise))

  (check
      (uxptn.pathname-bytevector->string
       (uxptn.serialise-segments #t '()))
    => "/")

  (check
      (uxptn.pathname-bytevector->string
       (uxptn.serialise-segments #f '()))
    => ".")

  (check
      (uxptn.pathname-bytevector->string
       (uxptn.serialise-segments
	#t (map uxptn.string/bytevector->pathname-bytevector '("ciao"))))
    => "/ciao")

  (check
      (uxptn.pathname-bytevector->string
       (uxptn.serialise-segments
	#f (map uxptn.string/bytevector->pathname-bytevector '("ciao"))))
    => "ciao")

  (check
      (uxptn.pathname-bytevector->string
       (uxptn.serialise-segments
	#t (map uxptn.string/bytevector->pathname-bytevector '("ciao" "salut"))))
    => "/ciao/salut")

  (check
      (uxptn.pathname-bytevector->string
       (uxptn.serialise-segments
	#f (map uxptn.string/bytevector->pathname-bytevector '("ciao" "salut"))))
    => "ciao/salut")

  (check
      (uxptn.pathname-bytevector->string
       (uxptn.serialise-segments
	#t (map uxptn.string/bytevector->pathname-bytevector '("ciao" "salut" "hello"))))
    => "/ciao/salut/hello")

  (check
      (uxptn.pathname-bytevector->string
       (uxptn.serialise-segments
	#f (map uxptn.string/bytevector->pathname-bytevector '("ciao" "salut" "hello"))))
    => "ciao/salut/hello")

  #t)


(parametrise ((check-test-name	'absolute))

  (define-syntax-rule (doit ?pathname ?expected)
    (check (uxptn.absolute? ?pathname) => ?expected))

  (define-syntax-rule (doit1 ?pathname ?expected)
    (check (uxptn.absolute? (string->ascii ?pathname)) => ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext"		#t)
  (doit "path/to/file.ext"		#f)
  (doit "/"				#t)
  (doit "."				#f)
  (doit ".."				#f)

;;; --------------------------------------------------------------------

  (doit1 "/path/to/file.ext"		#t)
  (doit1 "path/to/file.ext"		#f)
  (doit1 "/"				#t)
  (doit1 "."				#f)
  (doit1 ".."				#f)

  #t)


(parametrise ((check-test-name	'relative))

  (define-syntax-rule (doit ?pathname ?expected)
    (check (uxptn.relative? ?pathname) => ?expected))

  (define-syntax-rule (doit1 ?pathname ?expected)
    (check (uxptn.relative? (string->ascii ?pathname)) => ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext"		#f)
  (doit "path/to/file.ext"		#t)
  (doit "/"				#f)
  (doit "."				#t)
  (doit ".."				#t)

;;; --------------------------------------------------------------------

  (doit1 "/path/to/file.ext"		#f)
  (doit1 "path/to/file.ext"		#t)
  (doit1 "/"				#f)
  (doit1 "."				#t)
  (doit1 ".."				#t)

  #t)


(parametrise ((check-test-name	'components/extension))

  (define-syntax-rule (doit ?pathname ?expected)
    (check (uxptn.extension ?pathname) => ?expected))

  (define-syntax-rule (doit1 ?pathname ?expected)
    (check (ascii->string (uxptn.extension (string->ascii ?pathname))) => ?expected))

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

;;; --------------------------------------------------------------------

  (doit1 "ciao.it"			"it")
  (doit1 "ciao"				"")
  (doit1 "/path/to/file.ext"		"ext")
  (doit1 "/path/to/file."		"")
  (doit1 "/path/to/file"		"")
  (doit1 "/path/to/file.ext/ab"		"")
  (doit1 "/path/to/some.file.ext"	"ext")
  (doit1 "ciao.it//"			"it")
  (doit1 "a/"				"")
  (doit1 "."				"")
  (doit1 ".."				"")
  (doit1 "..."				"")
  (doit1 ".a"				"")
  (doit1 ".emacsrc"			"")
  (doit1 "..a"				"a")
  (doit1 "...a"				"a")
  (doit1 "..a.b"			"b")
  (doit1 "~/."				"")
  (doit1 "~/.."				"")
  (doit1 "~/..."			"")
  (doit1 "~/.a"				"")
  (doit1 "~/.emacsrc"			"")
  (doit1 "~/..a"			"a")
  (doit1 "~/...a"			"a")
  (doit1 "~/..a.b"			"b")

  #t)


(parametrise ((check-test-name	'components/dirname))

  (define-syntax-rule (doit ?pathname ?expected)
    (check (uxptn.dirname ?pathname) => ?expected))

  (define-syntax-rule (doit1 ?pathname ?expected)
    (check (ascii->string (uxptn.dirname (string->ascii ?pathname))) => ?expected))

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

;;; --------------------------------------------------------------------

  (begin
    (doit1 "/path/to/file.ext"		"/path/to")
    (doit1 "file.ext"			 ".")
    (doit1 "/file.ext"			 "/")
    (doit1 "/file.ext//"		"/")
    (doit1 "//file.ext"			 "/")
    (doit1 "/path/to///file.ext"	 "/path/to")
    (doit1 "//////file.ext"		 "/")
    (doit1 "a/b"			 "a")
    (doit1 "a"				 ".")
    (doit1 "../a"			 "..")
    (doit1 "./a"			 ".")
    (doit1 "../abcd"			 "..")
    (doit1 "./abcd"			 ".")
    (doit1 "../abcd/efgh"		 "../abcd")
    (doit1 "./abcd/efgh"		 "./abcd")
    (doit1 "/ciao/"			 "/")
    (doit1 "ciao/"			 ".")
    (doit1 "./ciao/"			 ".")
    (doit1 "hello/ciao/"		 "hello")
    (doit1 "//////"			 "/")
    (doit1 "ciao//////"			 ".")
    (void))

  #t)


(parametrise ((check-test-name	'components/tailname))

  (define-syntax-rule (doit ?pathname ?expected)
    (check (uxptn.tailname ?pathname) => ?expected))

  (define-syntax-rule (doit1 ?pathname ?expected)
    (check (ascii->string (uxptn.tailname (string->ascii ?pathname))) => ?expected))

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
  (doit "//////"			"")

;;; --------------------------------------------------------------------

  (doit1 "/path/to/file.ext"		"file.ext")
  (doit1 "file.ext"			"file.ext")
  (doit1 "/file.ext"			"file.ext")
  (doit1 "/file.ext//"			"file.ext")
  (doit1 "//file.ext"			"file.ext")
  (doit1 "/path/to///file.ext"		"file.ext")
  (doit1 "//////file.ext"		"file.ext")
  (doit1 "a/b"				"b")
  (doit1 "a"				"a")
  (doit1 "../a"				"a")
  (doit1 "./a"				"a")
  (doit1 "../abcd"			"abcd")
  (doit1 "./abcd"			"abcd")
  (doit1 "../abcd/efgh"			"efgh")
  (doit1 "./abcd/efgh"			"efgh")
  (doit1 "/ciao/"			"ciao")
  (doit1 "ciao/"				"ciao")
  (doit1 "./ciao/"			"ciao")
  (doit1 "hello/ciao/"			"ciao")
  (doit1 "ciao//////"			"ciao")
  (doit1 "//////"			"")

  #t)


(parametrise ((check-test-name	'components/rootname))

  (define-syntax-rule (doit ?pathname ?expected)
    (check (uxptn.rootname ?pathname) => ?expected))

  (define-syntax-rule (doit1 ?pathname ?expected)
    (check (ascii->string (uxptn.rootname (string->ascii ?pathname))) => ?expected))

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

;;; --------------------------------------------------------------------

  (doit1 "ciao.it"			"ciao")
  (doit1 "ciao"				"ciao")
  (doit1 "/path/to/file.ext"		"/path/to/file")
  (doit1 "/path/to/file."		"/path/to/file")
  (doit1 "/path/to/file"		"/path/to/file")
  (doit1 "/path/to/file.ext/ab"		"/path/to/file.ext/ab")
  (doit1 "/path/to/some.file.ext"	"/path/to/some.file")
  (doit1 "a/"				"a")
  (doit1 "a."				"a")
  (doit1 "."				".")
  (doit1 ".."				"..")
  (doit1 "..."				"..")
  (doit1 ".a"				".a")
  (doit1 ".emacsrc"			".emacsrc")
  (doit1 "..a"				".")
  (doit1 "...a"				"..")
  (doit1 "..a.b"			"..a")
  (doit1 "~/."				"~/.")
  (doit1 "~/.."				"~/..")
  (doit1 "~/..."			"~/..")
  (doit1 "~/.a"				"~/.a")
  (doit1 "~/.emacsrc"			"~/.emacsrc")
  (doit1 "~/..a"			"~/.")
  (doit1 "~/...a"			"~/..")
  (doit1 "~/..a.b"			"~/..a")
  (doit1 "///"				"/")
  (doit1 "ciao///"			"ciao")
  (doit1 "ciao.it///"			"ciao")
  (doit1 "ciao.it.en///"		"ciao.it")

  #t)


(parametrise ((check-test-name	'components/strip-trailing-slashes))

  (define-syntax-rule (doit ?pathname ?expected)
    (check (uxptn.strip-trailing-slashes ?pathname) => ?expected))

  (define-syntax-rule (doit1 ?pathname ?expected)
    (check (ascii->string (uxptn.strip-trailing-slashes (string->ascii ?pathname))) => ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext"		"/path/to/file.ext")
  (doit "ciao//"			"ciao")
  (doit "/"				"/")
  (doit "///"				"/")

;;; --------------------------------------------------------------------

  (doit1 "/path/to/file.ext"		"/path/to/file.ext")
  (doit1 "ciao//"			"ciao")
  (doit1 "/"				"/")
  (doit1 "///"				"/")

  #t)


(parametrise ((check-test-name	'components/split))

  (define-syntax-rule (doit ?pathname . ?expected)
    (check (uxptn.split ?pathname) => . ?expected))

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


(parametrise ((check-test-name	'components/normalise))

  (define-syntax-rule (doit ?pathname . ?expected)
    (check (uxptn.normalise ?pathname) => . ?expected))

  (define-syntax-rule (doit1 ?pathname ?expected)
    (check (ascii->string (uxptn.normalise (string->ascii ?pathname))) => ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext"		"/path/to/file.ext")
  (doit "path/to/file.ext"		"path/to/file.ext")
  (doit "ciao//"			"ciao")
  (doit "/"				"/")
  (doit "."				".")
  (doit ".."				"..")
  (doit "ciao/.."			".")
  (doit "/."				"/")
  (doit "/.."				"/")
  (doit "/ciao/.."			"/")

;;; --------------------------------------------------------------------

  (doit1 "/path/to/file.ext"		"/path/to/file.ext")
  (doit1 "path/to/file.ext"		"path/to/file.ext")
  (doit1 "ciao//"			"ciao")
  (doit1 "/"				"/")
  (doit1 "."				".")
  (doit1 ".."				"..")
  (doit1 "ciao/.."			".")
  (doit1 "/."				"/")
  (doit1 "/.."				"/")
  (doit1 "/ciao/.."			"/")

  #t)


(parametrise ((check-test-name	'components/prefix))

  (define-syntax-rule (doit ?pathname1 ?pathname2 ?expected)
    (check (uxptn.prefix? ?pathname1 ?pathname2) => ?expected))

  (define-syntax-rule (doit1 ?pathname1 ?pathname2 ?expected)
    (check (uxptn.prefix? (string->ascii ?pathname1)
			  (string->ascii ?pathname2)) => ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext" "/path/to/file.ext"		#t)
  (doit "/path/to/"         "/path/to/file.ext"		#t)
  (doit "/path/from"        "/path/to/file.ext"		#f)

;;; --------------------------------------------------------------------

  (doit1 "/path/to/file.ext" "/path/to/file.ext"	#t)
  (doit1 "/path/to/"         "/path/to/file.ext"	#t)
  (doit1 "/path/from"        "/path/to/file.ext"	#f)

  #t)


(parametrise ((check-test-name	'components/suffix))

  (define-syntax-rule (doit ?pathname1 ?pathname2 ?expected)
    (check (uxptn.suffix? ?pathname1 ?pathname2) => ?expected))

  (define-syntax-rule (doit1 ?pathname1 ?pathname2 ?expected)
    (check (uxptn.suffix? (string->ascii ?pathname1)
			  (string->ascii ?pathname2)) => ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to/file.ext" "/path/to/file.ext"		#t)
  (doit "/to/file.ext" "/path/to/file.ext"		#t)
  (doit "/from/file.ext" "/path/to/file.ext"		#f)

;;; --------------------------------------------------------------------

  (doit1 "/path/to/file.ext" "/path/to/file.ext"	#t)
  (doit1 "/to/file.ext" "/path/to/file.ext"		#t)
  (doit1 "/from/file.ext" "/path/to/file.ext"		#f)

  #t)


(parametrise ((check-test-name	'components/prepend))

  (define-syntax-rule (doit ?pathname1 ?pathname2 ?expected)
    (check (uxptn.prepend ?pathname1 ?pathname2) => ?expected))

  (define-syntax-rule (doit1 ?pathname1 ?pathname2 ?expected)
    (check (ascii->string (uxptn.prepend (string->ascii ?pathname1)
					 (string->ascii ?pathname2))) => ?expected))

;;; --------------------------------------------------------------------

  (doit "/path/to" "file.ext"			"/path/to/file.ext")
  (doit "/" "path/to/file.ext"			"/path/to/file.ext")
  (doit "." "path/to/file.ext"			"./path/to/file.ext")
  (doit ".." "path/to/file.ext"			"../path/to/file.ext")

;;; --------------------------------------------------------------------

  (doit1 "/path/to" "file.ext"			"/path/to/file.ext")
  (doit1 "/" "path/to/file.ext"			"/path/to/file.ext")
  (doit1 "." "path/to/file.ext"			"./path/to/file.ext")
  (doit1 ".." "path/to/file.ext"		"../path/to/file.ext")

  #t)


(parametrise ((check-test-name	'components/append))

  (define-syntax-rule (doit ?pathname1 ?pathname2 ?expected)
    (check (uxptn.append ?pathname1 ?pathname2) => ?expected))

  (define-syntax-rule (doit1 ?pathname1 ?pathname2 ?expected)
    (check (ascii->string (uxptn.append (string->ascii ?pathname1)
					(string->ascii ?pathname2))) => ?expected))

;;; --------------------------------------------------------------------

  (doit "file.ext" "/path/to"			"/path/to/file.ext")
  (doit "path/to/file.ext" "/"			"/path/to/file.ext")
  (doit "path/to/file.ext" "."			"./path/to/file.ext")
  (doit "path/to/file.ext" ".."			"../path/to/file.ext")

;;; --------------------------------------------------------------------

  (doit1 "file.ext" "/path/to"			"/path/to/file.ext")
  (doit1 "path/to/file.ext" "/"			"/path/to/file.ext")
  (doit1 "path/to/file.ext" "."			"./path/to/file.ext")
  (doit1 "path/to/file.ext" ".."		"../path/to/file.ext")

  #t)


(parametrise ((check-test-name	'components/replace-extension))

  (define-syntax-rule (doit ?pathname ?extension ?expected)
    (check (uxptn.replace-extension ?pathname ?extension) => ?expected))

  (define-syntax-rule (doit1 ?pathname ?extension ?expected)
    (check (ascii->string (uxptn.replace-extension (string->ascii ?pathname)
						   (string->ascii ?extension))) => ?expected))

  (define-syntax-rule (doit-special-pathname-error ?pathname)
    (check
	(guard (E ((uxptn.unix-pathname-normalisation-error? E)
		   (condition-message E))
		  (else E))
	  (doit ?pathname "two" ""))
      => "cannot append extension to special directory pathname"))

  (define-syntax-rule (doit-special-pathname-error1 ?pathname)
    (check
	(guard (E ((uxptn.unix-pathname-normalisation-error? E)
		   (condition-message E))
		  (else E))
	  (doit1 ?pathname "two" ""))
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

;;; --------------------------------------------------------------------

  (doit1 "file.one" "two"		"file.two")
  (doit1 "/path/to/file.one" "two"	"/path/to/file.two")
  (doit1 ".emacs" "elc"			".emacs.elc")
  (doit1 "/path/to/.emacs" "elc"	"/path/to/.emacs.elc")

  (doit-special-pathname-error1 "/")
  (doit-special-pathname-error1 "///")
  (doit-special-pathname-error1 ".")
  (doit-special-pathname-error1 "..")

  #t)


;;;; done

(check-report)

;;; end of file
