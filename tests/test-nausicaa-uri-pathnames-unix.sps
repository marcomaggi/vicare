;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the pathnames library
;;;Date: Fri Mar 26, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2006-2007, 2010-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (nausicaa uri pathnames unix)
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Nausicaa libraries: file system pathnames, unix library\n")


;;; helpers

(define (make-lexer-port obj)
  (cond ((string? obj)
	 (open-bytevector-input-port (any-to-bytevector obj)))
	((bytevector? obj)
	 (open-bytevector-input-port obj))
	(else
	 (assertion-violation 'make-lexer-port "expecting string or bytevector" obj))))

(define (any-to-bytevector obj)
  ;;Convert the string OBJ  to a bytevector representation regardless of
  ;;its contents.
  ;;
  (if (bytevector? obj)
      obj
    (let* ((len (string-length obj))
	   (bv  (make-bytevector len)))
      (dotimes (i len bv)
	(bytevector-u8-set! bv i (char->integer (string-ref obj i)))))))

(define (any-to-string obj)
  ;;Convert  the bytevector OBJ  to a  string representation.
  ;;
  (if (string? obj)
      obj
    (let* ((len (bytevector-length obj))
	   (str (make-string len)))
      (dotimes (i len str)
	(string-set! str i (integer->char (bytevector-u8-ref obj i)))))))


(parametrise ((check-test-name	'unix-low/string-bytevector))

  (check (ul.to-string (ul.to-bytevector ""))		=> "")
  (check (ul.to-string (ul.to-bytevector "ciao"))	=> "ciao")
  (check (ul.to-string (ul.to-bytevector "ciao/hello"))	=> "ciao/hello")

  (check
      (guard (E ((cnd.byte-condition? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(ul.to-bytevector "ciao\x1FF;"))
    => #t)

  (check
      (guard (E ((cnd.byte-condition? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(ul.to-bytevector "ciao\x0;"))
    => #t)


  #t)


(parametrise ((check-test-name	'unix-low/parse-segments))

;;; path segment

  (check
      (ul.to-string (ul.parse-segment (make-lexer-port "")))
    => "")

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
	     (segment	(ul.to-string (ul.parse-segment in-port)))
	     (eof	(lookahead-u8 in-port)))
	(list segment eof))
    => `("ciao" ,(eof-object)))

  (check
      (ul.to-string (ul.parse-segment (make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (ul.to-string (ul.parse-segment (make-lexer-port "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (ul.to-string (ul.parse-segment (make-lexer-port "ciao!$&'()*+,;=:@-._~")))
    => "ciao!$&'()*+,;=:@-._~")

  (check
      (let* ((in-port	(make-lexer-port "/hello"))
	     (segment1	(ul.to-string (ul.parse-segment in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(ul.to-string (ul.parse-segment in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("" #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello"))
	     (segment1	(ul.to-string (ul.parse-segment in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(ul.to-string (ul.parse-segment in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

;;; --------------------------------------------------------------------
;;; path segment-nz

  (check
      (ul.parse-segment-nz (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "{"))
  	     (segment	(ul.to-string (ul.parse-segment-nz in-port))))
  	(list segment (eof-object? (lookahead-u8 in-port))))
    => '("{" #t))

  (check
      (let* ((in-port	(make-lexer-port "/"))
  	     (segment	(ul.parse-segment-nz in-port))
  	     (char	(integer->char (get-u8 in-port))))
  	(list segment char))
    => '(#f #\/))

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
  	     (segment	(ul.to-string (ul.parse-segment-nz in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao:ciao"))
  	     (segment	(ul.to-string (ul.parse-segment-nz in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao:ciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao/hello"))
  	     (segment1	(ul.to-string (ul.parse-segment-nz in-port)))
  	     (slash	(integer->char (get-u8 in-port)))
  	     (segment2	(ul.to-string (ul.parse-segment-nz in-port))))
  	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "ciao%3dciao"))
  	     (segment	(ul.to-string (ul.parse-segment-nz in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao%3dciao" ,(eof-object)))

  (let ((S "ciao%3d%3dciao"))
    (check
  	(let* ((in-port	(make-lexer-port S))
  	       (segment	(ul.to-string (ul.parse-segment-nz in-port))))
  	  (list segment (lookahead-u8 in-port)))
      => `(,S ,(eof-object))))

;;; --------------------------------------------------------------------
;;; slash and segment

  (check
      (ul.parse-slash-and-segment (make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(make-lexer-port "ciao"))
  	     (segment	(ul.parse-slash-and-segment in-port))
  	     (rest	(ul.to-string (get-bytevector-some in-port))))
  	(list segment rest))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(make-lexer-port "/"))
  	     (segment	(ul.parse-slash-and-segment in-port)))
  	(list segment (lookahead-u8 in-port)))
    => `(#vu8(46) ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello"))
  	     (segment1	(ul.to-string (ul.parse-slash-and-segment in-port)))
  	     (segment2	(ul.to-string (ul.parse-slash-and-segment in-port))))
  	(list segment1 segment2 (lookahead-u8 in-port)))
    => `("ciao" "hello" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao/hello/"))
  	     (segment1	(ul.to-string (ul.parse-slash-and-segment in-port)))
  	     (segment2	(ul.to-string (ul.parse-slash-and-segment in-port)))
  	     (segment3	(ul.to-string (ul.parse-slash-and-segment in-port))))
  	(list segment1 segment2 segment3 (lookahead-u8 in-port)))
    => `("ciao" "hello" "." ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao%3dciao"))
  	     (segment	(ul.to-string (ul.parse-slash-and-segment in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao%3dciao" ,(eof-object)))

  (check
      (let* ((in-port	(make-lexer-port "/ciao%3d%3dciao"))
  	     (segment	(ul.to-string (ul.parse-slash-and-segment in-port))))
  	(list segment (lookahead-u8 in-port)))
    => `("ciao%3d%3dciao" ,(eof-object)))

  #t)


(parametrise ((check-test-name	'unix-low/parse-pathname))

  (check
      (receive (absolute? segments)
	  (ul.parse-pathname (make-lexer-port ""))
	(list absolute? segments))
    => '(#f #f))

  (check
      (receive (absolute? segments)
	  (ul.parse-pathname (make-lexer-port "/"))
	(list absolute? segments))
    => '(#t (#vu8(46))))

  (check
      (receive (absolute? segments)
	  (ul.parse-pathname (make-lexer-port "ciao"))
	(list absolute? (map ul.to-string segments)))
    => '(#f ("ciao")))

  (check
      (receive (absolute? segments)
	  (ul.parse-pathname (make-lexer-port "ciao/salut"))
	(list absolute? (map ul.to-string segments)))
    => '(#f ("ciao" "salut")))

  (check
      (receive (absolute? segments)
	  (ul.parse-pathname (make-lexer-port "/ciao/salut"))
	(list absolute? (map ul.to-string segments)))
    => '(#t ("ciao" "salut")))

  (check
      (receive (absolute? segments)
	  (ul.parse-pathname (make-lexer-port "ciao/salut/hello"))
	(list absolute? (map ul.to-string segments)))
    => '(#f ("ciao" "salut" "hello")))

  (check
      (receive (absolute? segments)
	  (ul.parse-pathname (make-lexer-port "/ciao/salut/hello"))
	(list absolute? (map ul.to-string segments)))
    => '(#t ("ciao" "salut" "hello")))

  (check
      (receive (absolute? segments)
	  (ul.parse-pathname (make-lexer-port "ciao/./.."))
	(list absolute? (map ul.to-string segments)))
    => '(#f ("ciao" "." "..")))

  (check
      (receive (absolute? segments)
	  (ul.parse-pathname (make-lexer-port "///"))
	(list absolute? (map ul.to-string segments)))
    => '(#t ("." "." ".")))

;;; --------------------------------------------------------------------

  (check
      (let ((in-port (make-lexer-port "/ciao/salut/he#\x0;llo")))
	(guard (E ((cnd.parser-condition? E)
		   (any-to-string (get-bytevector-some in-port)))
		  (else E))
	  (ul.parse-pathname in-port)))
    => "/ciao/salut/he#\x0;llo")

  #t)


(parametrise ((check-test-name	'unix-low/normalise-pathname))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?absolute ?input ?changed ?output)
       (check
	   (receive (changed? bvs)
	       (ul.normalise-pathname ?absolute (map ul.to-bytevector (quote ?input)))
	     (list changed? (and bvs (map ul.to-string bvs))))
	 => '(?changed ?output)))))

  (doit #f () #f ())

  (doit #t ("a") #f ("a"))
  (doit #f ("a") #f ("a"))

  (doit #t (".") #t ())
  (doit #f (".") #t ())

  (doit #t ("..") #f #f)
  (doit #f ("..") #f (".."))

  (doit #t (".." "..") #f #f)
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

  (doit #t ("ciao" "hello" ".." ".." "..") #f #f)
  (doit #f ("ciao" "hello" ".." ".." "..") #t (".."))

  (doit #t ("ciao" "hello" ".." ".." ".." "..") #f #f)
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


(parametrise ((check-test-name	'unix-low/serialise))

  (check
      (ul.to-string (ul.serialise-segments #t '()))
    => "/")

  (check
      (ul.to-string (ul.serialise-segments #f '()))
    => ".")

  (check
      (ul.to-string (ul.serialise-segments #t (map ul.to-bytevector '("ciao"))))
    => "/ciao")

  (check
      (ul.to-string (ul.serialise-segments #f (map ul.to-bytevector '("ciao"))))
    => "ciao")

  (check
      (ul.to-string (ul.serialise-segments #t (map ul.to-bytevector '("ciao" "salut"))))
    => "/ciao/salut")

  (check
      (ul.to-string (ul.serialise-segments #f (map ul.to-bytevector '("ciao" "salut"))))
    => "ciao/salut")

  (check
      (ul.to-string (ul.serialise-segments #t (map ul.to-bytevector '("ciao" "salut" "hello"))))
    => "/ciao/salut/hello")

  (check
      (ul.to-string (ul.serialise-segments #f (map ul.to-bytevector '("ciao" "salut" "hello"))))
    => "ciao/salut/hello")

  #t)


(parametrise ((check-test-name	'unix-class/constructor-absolute))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?expected)
       (check
	   (let (((o <absolute-pathname>) (make* <absolute-pathname> ?input)))
	     (list (is-a? o <absolute-pathname>)
		   o.string
		   (map ul.to-string o.segments)))
	 => '?expected))))

  (doit "/" (#t "/" ()))
  (doit "/ciao" (#t "/ciao" ("ciao")))
  (doit "/ciao/" (#t "/ciao" ("ciao")))
  (doit "/ciao//" (#t "/ciao" ("ciao")))
  (doit "/ciao//////" (#t "/ciao" ("ciao")))
  (doit "/ciao/salut/hello" (#t "/ciao/salut/hello" ("ciao" "salut" "hello")))
  (doit "/a/b/c/./../../g"  (#t "/a/g" ("a" "g")))

;;; --------------------------------------------------------------------

  (check
      (guard (E ((cnd.parser-condition? E)
;;;	       (write (condition-message E))(newline)
		 #t)
		(else
;;;	       (write (condition-message E))(newline)
		 E))
	(make* <absolute-pathname> ""))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
;;;	       (write (condition-message E))(newline)
		 #t)
		(else E))
	(make* <absolute-pathname> "ciao"))
    => #t)

  #t)


(parametrise ((check-test-name	'unix-class/maker-absolute))

  (check
      (let (((o <absolute-pathname>) (make <absolute-pathname>
					    (string: "/"))))
	(list (is-a? o <absolute-pathname>)
	      o.string
	      (map ul.to-string o.segments)))
    => '(#t "/" ()))

  (check
      (let (((o <absolute-pathname>) (make <absolute-pathname>
					    (string: "/ciao/salut/hello"))))
	(list (is-a? o <absolute-pathname>)
	      o.string
	      (map ul.to-string o.segments)))
    => '(#t "/ciao/salut/hello" ("ciao" "salut" "hello")))

  (check
      (let (((o <absolute-pathname>) (make <absolute-pathname>
					    (bytevector: (ul.to-bytevector "/ciao/salut/hello")))))
	(list (is-a? o <absolute-pathname>)
	      o.string
	      (map ul.to-string o.segments)))
    => '(#t "/ciao/salut/hello" ("ciao" "salut" "hello")))

  (check
      (let (((o <absolute-pathname>) (make <absolute-pathname>
					    (segments: (map ul.to-bytevector
							 '("ciao" "salut" "hello"))))))
	(list (is-a? o <absolute-pathname>)
	      o.string
	      (map ul.to-string o.segments)))
    => '(#t "/ciao/salut/hello" ("ciao" "salut" "hello")))


  #t)


(parametrise ((check-test-name	'unix-class/constructor-relative))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?expected)
       (check
	   (let (((o <relative-pathname>) (make* <relative-pathname> ?input)))
	     (list (is-a? o <relative-pathname>)
		   o.string
		   (map ul.to-string o.segments)))
	 => '?expected))))

  (doit "." (#t "." ()))
  (doit "ciao" (#t "ciao" ("ciao")))
  (doit "ciao/" (#t "ciao" ("ciao")))
  (doit "ciao//" (#t "ciao" ("ciao")))
  (doit "ciao//////" (#t "ciao" ("ciao")))
  (doit "ciao/salut/hello" (#t "ciao/salut/hello" ("ciao" "salut" "hello")))
  (doit "a/b/c/./../../g"  (#t "a/g" ("a" "g")))

;;; --------------------------------------------------------------------

  (check
      (guard (E ((cnd.parser-condition? E)
;;;	       (write (condition-message E))(newline)
		 #t)
		(else E))
	(make* <relative-pathname> ""))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
;;;	       (write (condition-message E))(newline)
		 #t)
		(else E))
	(make* <relative-pathname> "/ciao"))
    => #t)

  #t)


(parametrise ((check-test-name	'unix-class/maker-relative))

  (check
      (let (((o <relative-pathname>) (make <relative-pathname>
					    (string: "."))))
	(list (is-a? o <relative-pathname>)
	      o.string
	      (map ul.to-string o.segments)))
    => '(#t "." ()))

  (check
      (let (((o <relative-pathname>) (make <relative-pathname>
					    (string: "ciao/salut/hello"))))
	(list (is-a? o <relative-pathname>)
	      o.string
	      (map ul.to-string o.segments)))
    => '(#t "ciao/salut/hello" ("ciao" "salut" "hello")))

  (check
      (let (((o <relative-pathname>) (make <relative-pathname>
					    (bytevector: (ul.to-bytevector "ciao/salut/hello")))))
	(list (is-a? o <relative-pathname>)
	      o.string
	      (map ul.to-string o.segments)))
    => '(#t "ciao/salut/hello" ("ciao" "salut" "hello")))

  (check
      (let (((o <relative-pathname>) (make <relative-pathname>
					    (segments: (map ul.to-bytevector
							 '("ciao" "salut" "hello"))))))
	(list (is-a? o <relative-pathname>)
	      o.string
	      (map ul.to-string o.segments)))
    => '(#t "ciao/salut/hello" ("ciao" "salut" "hello")))


  #t)


(parametrise ((check-test-name	'unix-class/constructor-generic))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?expected)
       (check
	   (let (((o abs.<pathname>) (pathname ?input)))
	     (list (is-a? o <absolute-pathname>)
		   (is-a? o <relative-pathname>)
		   o.string
		   (map ul.to-string o.segments)))
	 => '?expected))))

  (doit "/" (#t #f "/" ()))
  (doit "/ciao" (#t #f "/ciao" ("ciao")))
  (doit "/ciao/" (#t #f "/ciao" ("ciao")))
  (doit "/ciao//" (#t #f "/ciao" ("ciao")))
  (doit "/ciao//////" (#t #f "/ciao" ("ciao")))
  (doit "/ciao/salut/hello" (#t #f "/ciao/salut/hello" ("ciao" "salut" "hello")))
  (doit "/a/b/c/./../../g"  (#t #f "/a/g" ("a" "g")))

  (doit "." (#f #t "." ()))
  (doit "ciao" (#f #t "ciao" ("ciao")))
  (doit "ciao/" (#f #t "ciao" ("ciao")))
  (doit "ciao//" (#f #t "ciao" ("ciao")))
  (doit "ciao//////" (#f #t "ciao" ("ciao")))
  (doit "ciao/salut/hello" (#f #t "ciao/salut/hello" ("ciao" "salut" "hello")))
  (doit "a/b/c/./../../g"  (#f #t "a/g" ("a" "g")))

;;; --------------------------------------------------------------------

  (check
      (guard (E ((cnd.parser-condition? E)
;;;		 (write (condition-message E))(newline)
		 #t)
		(else
;;;		 (write (condition-message E))(newline)
		 E))
	(pathname ""))
    => #t)

  #t)


(parametrise ((check-test-name	'unix-class/to-absolute))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?expected)
       (check
	   (let* (((o abs.<pathname>)		(pathname ?input))
		  ((p abs.<absolute-pathname>)	(o.absolute (pathname "/a/b/c"))))
	     p.string)
	 => '?expected))))

  (doit "." "/a/b/c")
  (doit "ciao" "/a/b/c/ciao")
  (doit "./ciao" "/a/b/c/ciao")
  (doit "../ciao" "/a/b/ciao")
  (doit "../../../ciao" "/ciao")

  (doit "/" "/")
  (doit "/ciao" "/ciao")
  (doit "/ciao/hello" "/ciao/hello")

;;; --------------------------------------------------------------------

  (check
      (guard (E ((cnd.normalisation-condition? E)
;;;(write (condition-message E))(newline)
		 #t)
		(else E))
	(let (((o abs.<pathname>) (pathname "../../../../ciao")))
	  (o.absolute (pathname "/ciao"))))
    => #t)

  #t)


(parametrise ((check-test-name	'unix-class/compose))

  (let ()	;prepending relative

    (define-syntax doit
      (syntax-rules ()
	((_ ?input ?expected)
	 (check
	     (let* (((o abs.<relative-pathname>) (pathname ?input))
		    ((p abs.<relative-pathname>) (o.prepend (pathname "./a/b"))))
	       p.string)
	   => '?expected))))

    (doit "." "a/b")
    (doit "c" "a/b/c")
    (doit "c/d/e" "a/b/c/d/e")
    (doit ".." "a")
    (doit "../.." ".")
    (doit "../../.." "..")
    (doit "../../../.." "../..")

    #f)

  (let ()	;prepending absolute

    (define-syntax doit
      (syntax-rules ()
	((_ ?input ?expected)
	 (check
	     (let* (((o abs.<relative-pathname>) (pathname ?input))
		    ((p abs.<absolute-pathname>) (o.prepend (pathname "/a/b"))))
	       p.string)
	   => '?expected))))

    (doit "." "/a/b")
    (doit "c" "/a/b/c")
    (doit "c/d/e" "/a/b/c/d/e")
    (doit ".." "/a")
    (doit "../.." "/")

    (check
	(guard (E ((is-a? cnd.&normalisation)
;;;(write (condition-message E))(newline)
		   #t)
		  (else E))
	  (let (((o abs.<relative-pathname>) (pathname "../../../")))
	    (o.prepend (pathname "/a/b"))))
      => #t)

    #f)

  (let ()	;appending to relative

    (define-syntax doit
      (syntax-rules ()
	((_ ?input ?expected)
	 (check
	     (let* (((o abs.<relative-pathname>) (pathname ?input))
		    ((p abs.<relative-pathname>) (o.append (pathname "./a/b"))))
	       p.string)
	   => '?expected))))

    (doit "." "a/b")
    (doit "c" "c/a/b")
    (doit "c/d/e" "c/d/e/a/b")
    (doit ".." "../a/b")
    (doit "../.." "../../a/b")
    (doit "../../.." "../../../a/b")
    (doit "../../../.." "../../../../a/b")

    #f)

  (let ()	;appending to absolute
    (define-syntax doit
      (syntax-rules ()
	((_ ?input ?expected)
	 (check
	     (let* (((o abs.<absolute-pathname>) (pathname ?input))
		    ((p abs.<absolute-pathname>) (o.append (pathname "./d/e"))))
	       p.string)
	   => '?expected))))

    (doit "/" "/d/e")
    (doit "/a/b" "/a/b/d/e")

    #f)

  #t)


(parametrise ((check-test-name	'unix-class/components-tail))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?expected)
       (check
	   (let* (((o abs.<absolute-pathname>) (pathname ?input))
		  ((p abs.<pathname>) (o.tail)))
	     (list (is-a? p abs.<relative-pathname>)
		   p.string))
	 => '?expected))))

  (doit "/" (#t "."))
  (doit "/a" (#t "a"))
  (doit "/a/b/c" (#t "c"))

  #t)


(parametrise ((check-test-name	'unix-class/components-dirname))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?expected)
       (check
	   (let* (((o abs.<absolute-pathname>) (pathname ?input))
		  ((p abs.<pathname>) (o.dirname)))
	     (list (is-a? p abs.<absolute-pathname>)
		   p.string))
	 => '?expected))))

  (doit "/" (#t "/"))
  (doit "/a" (#t "/"))
  (doit "/a/b/c" (#t "/a/b"))

  #t)


(parametrise ((check-test-name	'unix-class/components-rootname))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?expected)
       (check
	   (let* (((o abs.<absolute-pathname>) (pathname ?input))
		  ((p abs.<pathname>) (o.rootname)))
	     (list (is-a? p abs.<absolute-pathname>)
		   p.string))
	 => '?expected))))

  (doit "/" (#t "/"))
  (doit "/a" (#t "/a"))
  (doit "/a/b/c" (#t "/a/b/c"))

  (doit "/.ciao" (#t "/.ciao"))
  (doit "/a/.ciao" (#t "/a/.ciao"))
  (doit "/a/b/c/.ciao" (#t "/a/b/c/.ciao"))

  (doit "/the-file.ext" (#t "/the-file"))
  (doit "/a/the-file.ext" (#t "/a/the-file"))
  (doit "/a/b/c/the-file.ext" (#t "/a/b/c/the-file"))

  (doit "/the-file.the-extension" (#t "/the-file"))
  (doit "/a/the-file.the-extension" (#t "/a/the-file"))
  (doit "/a/b/c/the-file.the-extension" (#t "/a/b/c/the-file"))

  (doit "/.the-file.ext" (#t "/.the-file"))
  (doit "/a/.the-file.ext" (#t "/a/.the-file"))
  (doit "/a/b/c/.the-file.ext" (#t "/a/b/c/.the-file"))

  #t)


(parametrise ((check-test-name	'unix-class/components-extension))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?expected)
       (check
	   (let* (((o abs.<absolute-pathname>) (pathname ?input))
		  (ext (o.extension)))
	     (and ext (ul.to-string ext)))
	 => '?expected))))

  (doit "/" #f)
  (doit "/a" #f)
  (doit "/a/b/c" #f)

  (doit "/.ciao" #f)
  (doit "/a/.ciao" #f)
  (doit "/a/b/c/.ciao" #f)

  (doit "/the-file.ext" ".ext")
  (doit "/a/the-file.ext" ".ext")
  (doit "/a/b/c/the-file.ext" ".ext")

  (doit "/the-file.the-extension" ".the-extension")
  (doit "/a/the-file.the-extension" ".the-extension")
  (doit "/a/b/c/the-file.the-extension" ".the-extension")

  (doit "/.the-file.ext" ".ext")
  (doit "/a/.the-file.ext" ".ext")
  (doit "/a/b/c/.the-file.ext" ".ext")

  #t)


(parametrise ((check-test-name	'unix-class/components-name))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?expected)
       (check
	   (let* (((o abs.<absolute-pathname>) (pathname ?input))
		  (name (o.name)))
	     (and name (ul.to-string name)))
	 => '?expected))))

  (doit "/" ".")
  (doit "/a" "a")
  (doit "/a/b/c" "c")

  (doit "/.ciao" ".ciao")
  (doit "/a/.ciao" ".ciao")
  (doit "/a/b/c/.ciao" ".ciao")

  (doit "/the-file.ext" "the-file")
  (doit "/a/the-file.ext" "the-file")
  (doit "/a/b/c/the-file.ext" "the-file")

  (doit "/the-file.the-extension" "the-file")
  (doit "/a/the-file.the-extension" "the-file")
  (doit "/a/b/c/the-file.the-extension" "the-file")

  (doit "/.the-file.ext" ".the-file")
  (doit "/a/.the-file.ext" ".the-file")
  (doit "/a/b/c/.the-file.ext" ".the-file")

  #t)


(parametrise ((check-test-name	'unix-class/replace-extension))

  (let ()	;from string
    (define (doit input)
      (let* (((o abs.<absolute-pathname>)	(make* <absolute-pathname>
						  input))
	     ((p abs.<pathname>)		(o.replace-extension ".zip")))
	p.string))

    (check-for-true
     (let (((o abs.<absolute-pathname>) (make* <absolute-pathname>
					  "/a/b/file.ext")))
       (is-a? (o.replace-extension ".zip") <absolute-pathname>)))

    (check
	(doit "/a/b/file.ext")
      => "/a/b/file.zip")

    (check
	(doit "/a/b/file")
      => "/a/b/file.zip")

    (check
	(doit "/a/b/file.")
      => "/a/b/file.zip")

    (check
	(doit "/a/b/.")
      => "/a/b.zip")

    (check
	(doit "/a/b/..")
      => "/a.zip")

    (check
	(doit "/")
      => "/")

    (check
	(doit "/a/b/.file.ext")
      => "/a/b/.file.zip")

    (check
	(doit "/a/b/.file")
      => "/a/b/.file.zip")

    #f)

;;; --------------------------------------------------------------------
;;; from pathname

  (check-for-true
   (let (((o abs.<absolute-pathname>) (make* <absolute-pathname>
					"/a/b/file.ext")))
     (is-a? (o.replace-extension (pathname "/d/e/other.zip")) <absolute-pathname>)))

  (check
      (let* (((o abs.<absolute-pathname>)	(make* <absolute-pathname>
						  "/a/b/file.ext"))
	     ((p abs.<pathname>)		(o.replace-extension (pathname "/d/e/other.zip"))))
	p.string)
    => "/a/b/file.zip")

  (check
      (let* (((o abs.<absolute-pathname>)	(make* <absolute-pathname>
						  "/a/b/file"))
	     ((p abs.<pathname>)		(o.replace-extension (pathname "/d/e/other.zip"))))
	p.string)
    => "/a/b/file.zip")

  (check
      (let* (((o abs.<absolute-pathname>)	(make* <absolute-pathname>
						  "/a/b/file."))
	     ((p abs.<pathname>)		(o.replace-extension (pathname "/d/e/other.zip"))))
	p.string)
    => "/a/b/file.zip")

  (check
      (let* (((o abs.<absolute-pathname>)	(make* <absolute-pathname>
						  "/a/b/."))
	     ((p abs.<pathname>)		(o.replace-extension (pathname "/d/e/other.zip"))))
	p.string)
    => "/a/b.zip")

  (check
      (let* (((o abs.<absolute-pathname>)	(make* <absolute-pathname>
						  "/a/b/.."))
	     ((p abs.<pathname>)		(o.replace-extension (pathname "/d/e/other.zip"))))
	p.string)
    => "/a.zip")

  (check
      (let* (((o abs.<absolute-pathname>)	(make* <absolute-pathname>
						  "/"))
	     ((p abs.<pathname>)		(o.replace-extension (pathname "/d/e/other.zip"))))
	p.string)
    => "/")

  (check
      (let* (((o abs.<absolute-pathname>)	(make* <absolute-pathname>
						  "/a/b/.file.ext"))
	     ((p abs.<pathname>)		(o.replace-extension (pathname "/d/e/other.zip"))))
	p.string)
    => "/a/b/.file.zip")

  (check
      (let* (((o abs.<absolute-pathname>)	(make* <absolute-pathname>
						  "/a/b/.file"))
	     ((p abs.<pathname>)		(o.replace-extension (pathname "/d/e/other.zip"))))
	p.string)
    => "/a/b/.file.zip")

;;; --------------------------------------------------------------------
;;; errors

  (check
      (let (((o abs.<absolute-pathname>) (make* <absolute-pathname>
					   "/a/b/.file")))
	(guard (E ((is-a? &error (with-class &message))
		   ;;(pretty-print E.message)
		   #t)
		  (else E))
	  (o.replace-extension (pathname "/d/e/other"))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
