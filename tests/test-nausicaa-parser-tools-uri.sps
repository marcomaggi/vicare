;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for URI library
;;;Date: Wed Jun  2, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (nausicaa parser-tools uri)  uri.)
  (prefix (nausicaa net addresses uri) uri.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: URI parser and object types\n")


;;;; helpers

(define (%make-lexer-port obj)
  (define who '%make-lexer-port)
  (cond ((string? obj)
	 (open-bytevector-input-port (string->ascii obj)))
	((bytevector? obj)
	 (open-bytevector-input-port obj))
	(else
	 (assertion-violation who "expecting string or bytevector" obj))))


(parametrise ((check-test-name	'conditions))

  (check
      (let (((C uri.&uri-parser-error) (uri.&uri-parser-error (10))))
	((uri.&uri-parser-error) C))
    => #t)

  (check
      (let (((C uri.&uri-parser-error) (uri.&uri-parser-error (10))))
	(C offset))
    => 10)

  (check
      (let (((C uri.&uri-parser-error) (uri.&uri-parser-error (10))))
	(try
	    (raise C)
	  (catch E
	    (uri.&uri-parser-error
	     #t)
	    (else #f))))
    => #t)

  (check
      (try
	  (uri.raise-uri-parser-error 'ciao "message" 123)
	(catch E
	  (uri.&uri-parser-error
	   #t)
	  (else #f)))
    => #t)

  #t)


(parametrise ((check-test-name	'percent-encoding))

  (let ()

    (define-syntax-rule (doit ch str)
      (check (uri.percent-encode ch  (uri.string-result? #t)) => str)
      (check (uri.percent-decode str (uri.string-result? #t)) => (string ch)))

    (doit #\. ".")
    (doit #\- "-")
    (doit #\_ "_")
    (doit #\~ "~")
    (doit #\% "%25")
    (doit #\? "%3F")
    (doit #\= "%3D")
    (doit #\# "%23")

    #f)

  (let ()

    (define-syntax-rule (doit ch str)
      (check
	  (uri.percent-encode ch
			      (uri.string-result? #t)
			      (uri.char-selector (lambda (chi)
						   (memv (integer->char chi)
							 '(#\. #\- #\_ #\~ #\%
							   #\: #\/ #\?
							   #\# #\[ #\]
							   #\@ #\\ #\!
							   #\$ #\& #\'
							   #\( #\) #\*
							   #\+ #\, #\;
							   #\=))
						   )))
	=> str)
      (check (uri.percent-decode str (uri.string-result? #t)) => (string ch)))

    (doit #\. "%2E")
    (doit #\- "%2D")
    (doit #\_ "%5F")
    (doit #\~ "%7E")
    (doit #\% "%25")
    (doit #\? "%3F")
    (doit #\= "%3D")
    (doit #\# "%23")

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-syntax-rule (doit dec enc)
      (check (uri.percent-encode dec (uri.string-result? #t)) => enc)
      (check (uri.percent-decode enc (uri.string-result? #t)) => dec))

    (doit "" "")
    (doit "ciao" "ciao")
    (doit "cia=o" "cia%3Do")
    (doit "ci?a=o" "ci%3Fa%3Do")

    #f)

  (check
      (uri.percent-encode "ciao")
    => '#vu8(99 105 97 111))

  (check
      (uri.percent-decode '#vu8(99 105 97 111))
    => '#vu8(99 105 97 111))

  (check
      (uri.percent-decode '#vu8(99 105 97 111) (uri.string-result? #t))
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (uri.normalise-percent-encoded-string "")
    => "")

  (check
      (uri.normalise-percent-encoded-string "ciao")
    => "ciao")

  (check
      (uri.normalise-percent-encoded-string "cia%3do")
    => "cia%3Do")

  (check
      (uri.normalise-percent-encoded-string "cia%3Do")
    => "cia%3Do")

  (check
      (uri.normalise-percent-encoded-string "ci%3fa%3do")
    => "ci%3Fa%3Do")

  (check
      (uri.normalise-percent-encoded-string "ci%3Fa%3Do")
    => "ci%3Fa%3Do")

  (check
      (uri.normalise-percent-encoded-string "%7Eciao")
    => "~ciao")

  (check
      (uri.normalise-percent-encoded-string "ci%5Fao")
    => "ci_ao")

;;; --------------------------------------------------------------------

  (check
      (uri.normalise-percent-encoded-bytevector '#ve(ascii ""))
    => '#ve(ascii ""))

  (check
      (uri.normalise-percent-encoded-bytevector '#ve(ascii "ciao"))
    => '#ve(ascii "ciao"))

  (check
      (uri.normalise-percent-encoded-bytevector '#ve(ascii "cia%3do"))
    => '#ve(ascii "cia%3Do"))

  (check
      (uri.normalise-percent-encoded-bytevector '#ve(ascii "cia%3Do"))
    => '#ve(ascii "cia%3Do"))

  (check
      (uri.normalise-percent-encoded-bytevector '#ve(ascii "ci%3fa%3do"))
    => '#ve(ascii "ci%3Fa%3Do"))

  (check
      (uri.normalise-percent-encoded-bytevector '#ve(ascii "ci%3Fa%3Do"))
    => '#ve(ascii "ci%3Fa%3Do"))

  (check
      (uri.normalise-percent-encoded-bytevector '#ve(ascii "%7eciao"))
    => '#ve(ascii "~ciao"))

  (check
      (uri.normalise-percent-encoded-bytevector '#ve(ascii "ci%5fao"))
    => '#ve(ascii "ci_ao"))

  #t)


(parametrise ((check-test-name	'parsing-misc))

;;; valid component

  (let-syntax ((doit	(syntax-rules ()
			  ((_ ?expected ?input)
			   (check
			       (receive (bool pos)
				   (uri.valid-component? (%make-lexer-port ?input))
				 (list bool pos))
			     => ?expected)))))
    (doit '(#t  4) "ciao")
    (doit '(#t  4) "ciao")
    (doit '(#t  3) "%3d")
    (doit '(#t  9) "%3d%3d%3d")
    (doit '(#t 11) "ciao%3dciao")
    (doit '(#f  1) "?")
    (doit '(#f  5) "ciao?")
    #f)

  #t)


(parametrise ((check-test-name	'parsing-splitting-uri/scheme))

;;; scheme

  (check
      (uri.parse-scheme (%make-lexer-port ""))
    => #f)

  (check
      (uri.parse-scheme (%make-lexer-port "hello"))
    => #f)

  (check
      (uri.parse-scheme (%make-lexer-port "hel/lo:"))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "http://ciao"))
	     (scheme	(ascii->string (uri.parse-scheme in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list scheme rest))
    => '("http" "//ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "A123+-.://ciao"))
	     (scheme	(ascii->string (uri.parse-scheme in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list scheme rest))
    => '("A123+-." "//ciao"))

  #t)


(parametrise ((check-test-name	'parsing-splitting-uri/hier-part))

  (check
      (uri.collect-hier-part (%make-lexer-port ""))
    => #f)

;;; --------------------------------------------------------------------
;;; paths

  (check
      (let* ((in-port	(%make-lexer-port "//"))
  	     (part	(ascii->string (uri.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("//" #t))

  (check
      (let* ((in-port	(%make-lexer-port "//ciao"))
  	     (part	(ascii->string (uri.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("//ciao" #t))

  (check
      (let* ((in-port	(%make-lexer-port "//ciao/salut"))
  	     (part	(ascii->string (uri.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("//ciao/salut" #t))

;;;

  (check
      (let* ((in-port	(%make-lexer-port "/"))
  	     (part	(ascii->string (uri.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("/" #t))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao"))
  	     (part	(ascii->string (uri.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("/ciao" #t))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/salut"))
  	     (part	(ascii->string (uri.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("/ciao/salut" #t))

;;;

  (check
      (let* ((in-port	(%make-lexer-port "."))
  	     (part	(ascii->string (uri.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("." #t))

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
  	     (part	(ascii->string (uri.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("ciao" #t))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/salut"))
  	     (part	(ascii->string (uri.collect-hier-part in-port))))
  	(list part (eof-object? (lookahead-u8 in-port))))
    => '("ciao/salut" #t))

;;; --------------------------------------------------------------------
;;; query

  (check
      (let* ((in-port	(%make-lexer-port "//ciao?query"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//ciao" "?query"))

  (check
      (let* ((in-port	(%make-lexer-port "//?query"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//" "?query"))

  (check
      (let* ((in-port	(%make-lexer-port "//ciao/salut?query"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//ciao/salut" "?query"))

;;;

  (check
      (let* ((in-port	(%make-lexer-port "/?query"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("/" "?query"))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao?query"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("/ciao" "?query"))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/salut?query"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("/ciao/salut" "?query"))

;;;

  (check
      (let* ((in-port	(%make-lexer-port "?query"))
  	     (part	(uri.collect-hier-part in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '(#f "?query"))

  (check
      (let* ((in-port	(%make-lexer-port ".?query"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("." "?query"))

  (check
      (let* ((in-port	(%make-lexer-port "ciao?query"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("ciao" "?query"))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/salut?query"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("ciao/salut" "?query"))

;;; --------------------------------------------------------------------
;;; fragment

  (check
      (let* ((in-port	(%make-lexer-port "//#fragment"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//" "#fragment"))

  (check
      (let* ((in-port	(%make-lexer-port "//ciao#fragment"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//ciao" "#fragment"))

  (check
      (let* ((in-port	(%make-lexer-port "//ciao/salut#fragment"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("//ciao/salut" "#fragment"))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao#fragment"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("/ciao" "#fragment"))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/salut#fragment"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("/ciao/salut" "#fragment"))

;;;

  (check
      (let* ((in-port	(%make-lexer-port "#fragment"))
  	     (part	(uri.collect-hier-part in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '(#f "#fragment"))

  (check
      (let* ((in-port	(%make-lexer-port ".#fragment"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("." "#fragment"))

  (check
      (let* ((in-port	(%make-lexer-port "ciao#fragment"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("ciao" "#fragment"))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/salut#fragment"))
  	     (part	(ascii->string (uri.collect-hier-part in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
  	(list part rest))
    => '("ciao/salut" "#fragment"))

  #t)


(parametrise ((check-test-name	'parsing-splitting-uri/relative-part))

  (check
      (uri.collect-relative-part (%make-lexer-port ""))
    => #f)

  (check
      (ascii->string (uri.collect-relative-part (%make-lexer-port "//ciao")))
    => "//ciao")

  (check
      (let* ((p (%make-lexer-port "//ciao?query"))
  	     (r (ascii->string (uri.collect-relative-part p))))
  	(list r (get-u8 p)))
    => `("//ciao" ,(char->integer #\?)))

  (check
      (let* ((p (%make-lexer-port "//ciao#fragment"))
  	     (r (ascii->string (uri.collect-relative-part p))))
  	(list r (get-u8 p)))
    => `("//ciao" ,(char->integer #\#)))

  #t)


(parametrise ((check-test-name	'parsing-splitting-uri/query))

  (check
      (uri.parse-query (%make-lexer-port ""))
    => #f)

  (check
      (uri.parse-query (%make-lexer-port "hello"))
    => #f)

  (check
      (uri.parse-query (%make-lexer-port "#hello"))
    => #f)

  (check
      (ascii->string (uri.parse-query (%make-lexer-port "?")))
    => "")

  (check
      (ascii->string (uri.parse-query (%make-lexer-port "?the-query???")))
    => "the-query???")

  (check
      (let* ((in-port	(%make-lexer-port "?ciao%3dciao#fragment"))
	     (query	(ascii->string (uri.parse-query in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list query rest))
    => '("ciao%3dciao" "#fragment"))

  #t)


(parametrise ((check-test-name	'parsing-splitting-uri/fragment))

  (check
      (uri.parse-fragment (%make-lexer-port ""))
    => #f)

  (check
      (ascii->string (uri.parse-fragment (%make-lexer-port "#")))
    => "")

  (check
      (uri.parse-fragment (%make-lexer-port "#hello#"))
    => #f)

  (check
      (uri.parse-fragment (%make-lexer-port "hello"))
    => #f)

  (check
      (uri.parse-fragment (%make-lexer-port "?hello"))
    => #f)

  (check
      (ascii->string (uri.parse-fragment (%make-lexer-port "#the-fragment???")))
    => "the-fragment???")

  (check
      (ascii->string (uri.parse-fragment (%make-lexer-port "#ciao%3dciao")))
    => "ciao%3dciao")

  #t)


(parametrise ((check-test-name	'parsing-splitting-uri/documentation))

;;; examples for the documentation

  (let ()

    (define (mkport S)
      (open-bytevector-input-port (string->ascii S)))

    (define (doit S)
      (cond ((uri.parse-scheme (mkport S))
	     => ascii->string)
	    (else #f)))

    (check (doit "")               => #f)
    (check (doit "hello")          => #f)
    (check (doit "hel/lo:")        => #f)

    (check
	(let* ((P (mkport "http://ciao"))
	       (A (ascii->string (uri.parse-scheme P)))
	       (B (ascii->string (get-bytevector-some P)))
	       (C (get-bytevector-all P)))
	  (list A B C))
      => `("http" "//ciao" ,(eof-object)))

    (check
	(let* ((P (mkport "A123+-.://ciao"))
	       (A (ascii->string (uri.parse-scheme P)))
	       (B (ascii->string (get-bytevector-some P))))
	  (list A B))
      => '("A123+-." "//ciao"))

    (void))

  #t)


#;(parametrise ((check-test-name	'parsing-authority))

;;; authority

  (check
      (uri.parse-authority (%make-lexer-port ""))
    => #f)

  (check
      (uri.parse-authority (%make-lexer-port "ciao"))
    => #f)

  (check
      (uri.parse-authority (%make-lexer-port "/ciao"))
    => #f)

  (check
      (uri.parse-authority (%make-lexer-port "?ciao"))
    => #f)

  (check
      (uri.parse-authority (%make-lexer-port "#ciao"))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "//"))
	     (authority	(ascii->string (uri.parse-authority in-port))))
	(list authority (eof-object? (lookahead-u8 in-port))))
    => '("" #t))

  (check
      (let* ((in-port	(%make-lexer-port "//?query"))
	     (authority	(ascii->string (uri.parse-authority in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list authority rest))
    => '("" "?query"))

  (check
      (let* ((in-port	(%make-lexer-port "//#fragment"))
	     (authority	(ascii->string (uri.parse-authority in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list authority rest))
    => '("" "#fragment"))

  (check
      (let* ((in-port	(%make-lexer-port "///"))
	     (authority	(ascii->string (uri.parse-authority in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list authority rest))
    => '("" "/"))

  (check
      (let* ((in-port	(%make-lexer-port "//ciao/salut"))
	     (authority	(ascii->string (uri.parse-authority in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list authority rest))
    => '("ciao" "/salut"))

  (check
      (let* ((in-port	(%make-lexer-port "//ciao:8080/salut"))
	     (authority	(ascii->string (uri.parse-authority in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list authority rest))
    => '("ciao:8080" "/salut"))

  (check
      (let* ((in-port	(%make-lexer-port "//ciao.it:8080/salut"))
	     (authority	(ascii->string (uri.parse-authority in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list authority rest))
    => '("ciao.it:8080" "/salut"))

  (check
      (let* ((in-port	(%make-lexer-port "//marco@ciao.it:8080/salut"))
	     (authority	(ascii->string (uri.parse-authority in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list authority rest))
    => '("marco@ciao.it:8080" "/salut"))

;;; --------------------------------------------------------------------
;;; userinfo

  (check
      (uri.parse-userinfo (%make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "ciao.it"))
	     (info	(uri.parse-userinfo in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f "ciao.it"))

  (check
      (let* ((in-port	(%make-lexer-port ":8080"))
	     (info	(uri.parse-userinfo in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f ":8080"))

  (check
      (let* ((in-port	(%make-lexer-port "/hello"))
	     (info	(uri.parse-userinfo in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f "/hello"))

  (check
      (let* ((in-port	(%make-lexer-port "?hello"))
	     (info	(uri.parse-userinfo in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f "?hello"))

  (check
      (let* ((in-port	(%make-lexer-port "#hello"))
	     (info	(uri.parse-userinfo in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list info rest))
    => '(#f "#hello"))

  (check
      (let* ((in-port	(%make-lexer-port "@"))
	     (userinfo	(ascii->string (uri.parse-userinfo in-port)))
	     (eof?	(eof-object? (lookahead-u8 in-port))))
	(list userinfo eof?))
    => '("" #t))

  (check
      (let* ((in-port	(%make-lexer-port "@host"))
	     (userinfo	(ascii->string (uri.parse-userinfo in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list userinfo rest))
    => '("" "host"))

  (check
      (let* ((in-port	(%make-lexer-port "userinfo@host"))
	     (userinfo	(ascii->string (uri.parse-userinfo in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list userinfo rest))
    => '("userinfo" "host"))

  (check
      (let* ((in-port	(%make-lexer-port "ciao%3dciao@host"))
	     (userinfo	(ascii->string (uri.parse-userinfo in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list userinfo rest))
    => '("ciao%3dciao" "host"))

;;; --------------------------------------------------------------------
;;; IPv4 address

  (check
      (receive (addr ell)
	  (uri.parse-ipv4-address (%make-lexer-port ""))
	addr)
    => #f)

  (check
      (let*-values (((in-port)	(%make-lexer-port "ciao"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "ciao"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1."))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "1."))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "1.2"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "1.2.3"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3.4.5"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "1.2.3.4.5"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "123ciao"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "123ciao"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3.ciao"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "1.2.3.ciao"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3.4"))
		    ((addr ell)	(uri.parse-ipv4-address in-port)))
	(list ell (eof-object? (lookahead-u8 in-port))))
    => '((1 2 3 4) #t))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3.4."))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "1.2.3.4."))

  (check
      (let*-values (((in-port)	(%make-lexer-port "191.223.376.434"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "191.223.376.434"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "191.223.76.255"))
		    ((addr ell)	(uri.parse-ipv4-address in-port)))
	(list ell (eof-object? (lookahead-u8 in-port))))
    => '((191 223 76 255) #t))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3.4/5"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list (ascii->string addr) ell rest))
    => '("1.2.3.4" (1 2 3 4) "/5"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3.4/ciao"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list ell rest))
    => '((1 2 3 4) "/ciao"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3.4:8080"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list ell rest))
    => '((1 2 3 4) ":8080"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3.4ciao"))
		    ((addr ell)	(uri.parse-ipv4-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list ell rest))
    => '((1 2 3 4) "ciao"))

;;; --------------------------------------------------------------------
;;; IPv6 address

  (check
      (receive (addr ell)
	  (uri.parse-ipv6-address (%make-lexer-port ""))
	addr)
    => #f)

  (check
      (let*-values (((in-port)	(%make-lexer-port "ciao"))
		    ((addr ell)	(uri.parse-ipv6-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "ciao"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1.2.3.ciao"))
		    ((addr ell)	(uri.parse-ipv6-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list addr rest))
    => '(#f "1.2.3.ciao"))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1:2:3:4:5:6:7:8"))
		    ((addr ell)	(uri.parse-ipv6-address in-port)))
	(list (ascii->string addr) ell (eof-object? (lookahead-u8 in-port))))
    => '("1:2:3:4:5:6:7:8" (1 2 3 4 5 6 7 8) #t))

  (check
      (let*-values (((in-port)	(%make-lexer-port "::1"))
		    ((addr ell)	(uri.parse-ipv6-address in-port)))
	(list (ascii->string addr) ell (eof-object? (lookahead-u8 in-port))))
    => '("::1" (0 0 0 0 0 0 0 1) #t))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1::"))
		    ((addr ell)	(uri.parse-ipv6-address in-port)))
	(list (ascii->string addr) ell (eof-object? (lookahead-u8 in-port))))
    => '("1::" (1 0 0 0 0 0 0 0) #t))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1:2::3"))
		    ((addr ell)	(uri.parse-ipv6-address in-port)))
	(list (ascii->string addr) ell (eof-object? (lookahead-u8 in-port))))
    => '("1:2::3" (1 2 0 0 0 0 0 3) #t))

  (check
      (let*-values (((in-port)	(%make-lexer-port "1:2:3:4::172.30.67.254"))
		    ((addr ell)	(uri.parse-ipv6-address in-port)))
	(list (ascii->string addr) ell (eof-object? (lookahead-u8 in-port))))
    => '("1:2:3:4::172.30.67.254" (1 2 3 4 0 0 #xac1e #x43fe) #t))

  (check
      (let*-values (((in-port)	(%make-lexer-port "::ffff:192.168.99.1"))
		    ((addr ell)	(uri.parse-ipv6-address in-port)))
	(list (ascii->string addr) ell (eof-object? (lookahead-u8 in-port))))
    => '("::ffff:192.168.99.1" (0 0 0 0 0 #xFFFF #xC0A8 #x6301) #t))

  (check
      (let*-values (((in-port)	(%make-lexer-port "::1/60"))
		    ((addr ell)	(uri.parse-ipv6-address in-port))
		    ((rest)	(ascii->string (get-bytevector-some in-port))))
	(list (ascii->string addr) ell rest))
    => '("::1" (0 0 0 0 0 0 0 1) "/60"))

;;; --------------------------------------------------------------------
;;; IP-literal

  (check
      (uri.parse-ip-literal (%make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
	     (ip	(uri.parse-ip-literal in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list ip rest))
    => '(#f "ciao"))

  (check
      (ascii->string (uri.parse-ip-literal (%make-lexer-port "[]")))
    => "")

  (check
      (ascii->string (uri.parse-ip-literal (%make-lexer-port "[::0:1:2]")))
    => "::0:1:2")

  (check
      (let* ((in-port	(%make-lexer-port "[::0:1:2]:8080"))
	     (ip	(ascii->string (uri.parse-ip-literal in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list ip rest))
    => '("::0:1:2" ":8080"))

;;; --------------------------------------------------------------------
;;; IPvFuture

  (check
      (call-with-values
	  (lambda ()
	    (uri.parse-ipvfuture (%make-lexer-port "")))
	list)
    => '(#f #f))

  (check
      (call-with-values
	  (lambda ()
	    (uri.parse-ipvfuture (%make-lexer-port "ciao")))
	list)
    => '(#f #f))

  (check
      (call-with-values
	  (lambda ()
	    (uri.parse-ipvfuture (%make-lexer-port "v1")))
	list)
    => '(1 #vu8()))

  (check
      (call-with-values
	  (lambda ()
	    (uri.parse-ipvfuture (%make-lexer-port "v9ciao")))
	(lambda (version bv)
	  (list version (ascii->string bv))))
    => '(9 "ciao"))

  (check
      (call-with-values
	  (lambda ()
	    (uri.parse-ipvfuture (%make-lexer-port "vFciao")))
	(lambda (version bv)
	  (list version (ascii->string bv))))
    => '(15 "ciao"))

  (check
      (call-with-values
	  (lambda ()
	    (uri.parse-ipvfuture (%make-lexer-port "VEciao")))
	(lambda (version bv)
	  (list version (ascii->string bv))))
    => '(14 "ciao"))

;;; --------------------------------------------------------------------
;;; reg-name

  (check
      (ascii->string (uri.parse-reg-name (%make-lexer-port "")))
    => "")

  (check	;no more than 255 chars
      (ascii->string (uri.parse-reg-name (%make-lexer-port (make-string 255 #\a))))
    => (make-string 255 #\a))

  (check	;no more than 256 chars
      (uri.parse-reg-name (%make-lexer-port (make-string 256 #\a)))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port ":80"))
	     (reg	(ascii->string (uri.parse-reg-name in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list reg rest))
    => '("" ":80"))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao"))
	     (reg	(ascii->string (uri.parse-reg-name in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list reg rest))
    => '("" "/ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "?query"))
	     (reg	(ascii->string (uri.parse-reg-name in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list reg rest))
    => '("" "?query"))

  (check
      (let* ((in-port	(%make-lexer-port "#fragment"))
	     (reg	(ascii->string (uri.parse-reg-name in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list reg rest))
    => '("" "#fragment"))

  (check
      (ascii->string (uri.parse-reg-name (%make-lexer-port "the-reg-name")))
    => "the-reg-name")

  (check
      (ascii->string (uri.parse-reg-name (%make-lexer-port "the.reg.name")))
    => "the.reg.name")

  (check
      (ascii->string (uri.parse-reg-name (%make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (let* ((in-port	(%make-lexer-port "the-reg-name:80"))
	     (reg	(ascii->string (uri.parse-reg-name in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list reg rest))
    => '("the-reg-name" ":80"))

  (check
      (let* ((in-port	(%make-lexer-port "the-reg-name/ciao"))
	     (reg	(ascii->string (uri.parse-reg-name in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list reg rest))
    => '("the-reg-name" "/ciao"))

;;; --------------------------------------------------------------------
;;; host

  (check
      (let-values (((kind data) (uri.parse-host (%make-lexer-port ""))))
	(list kind data))
    => '(reg-name #vu8()))

  (check
      (let*-values (((in-port)		(%make-lexer-port "/"))
		    ((kind data)	(uri.parse-host in-port))
		    ((rest)		(ascii->string (get-bytevector-some in-port))))
	(list kind data rest))
    => '(reg-name #vu8() "/"))

  (check
      (let*-values (((in-port)		(%make-lexer-port ":80"))
		    ((kind data)	(uri.parse-host in-port))
		    ((rest)		(ascii->string (get-bytevector-some in-port))))
	(list kind data rest))
    => '(reg-name #vu8() ":80"))

  (check
      (let*-values (((in-port)		(%make-lexer-port "1.2.3.4:80"))
		    ((kind data)	(uri.parse-host in-port))
		    ((rest)		(ascii->string (get-bytevector-some in-port))))
	(list kind (ascii->string (car data)) (cdr data) rest))
    => '(ipv4-address "1.2.3.4" (1 2 3 4) ":80"))

  (check
      (let*-values (((in-port)		(%make-lexer-port "1.2.3.4/ciao"))
  		    ((kind data)	(uri.parse-host in-port))
  		    ((rest)		(ascii->string (get-bytevector-some in-port))))
  	(list kind (ascii->string (car data)) (cdr data) rest))
    => '(ipv4-address "1.2.3.4" (1 2 3 4) "/ciao"))

  (check
      (let*-values (((in-port)		(%make-lexer-port "[::ffff:192.168.99.1]:80"))
  		    ((kind data)	(uri.parse-host in-port))
  		    ((rest)		(ascii->string (get-bytevector-some in-port))))
  	(list kind (ascii->string (car data)) (cdr data) rest))
    => '(ipv6-address "::ffff:192.168.99.1" (0 0 0 0 0 #xFFFF #xC0A8 #x6301) ":80"))

  (check
      (let*-values (((in-port)		(%make-lexer-port "[::ffff:192.168.99.1]/ciao"))
  		    ((kind data)	(uri.parse-host in-port))
  		    ((rest)		(ascii->string (get-bytevector-some in-port))))
  	(list kind (ascii->string (car data)) (cdr data) rest))
    => '(ipv6-address "::ffff:192.168.99.1" (0 0 0 0 0 #xFFFF #xC0A8 #x6301) "/ciao"))

  (check
      (let*-values (((in-port)		(%make-lexer-port "[v9,ciao,ciao]/ciao"))
  		    ((kind data)	(uri.parse-host in-port))
  		    ((rest)		(ascii->string (get-bytevector-some in-port))))
  	(list kind (car data) (ascii->string (cdr data)) rest))
    => '(ipvfuture 9 ",ciao,ciao" "/ciao"))

;;; --------------------------------------------------------------------
;;; port

  (check
      (uri.parse-port (%make-lexer-port ""))
    => #f)

  (check
      (ascii->string (uri.parse-port (%make-lexer-port ":")))
    => "")

  (check
      (ascii->string (uri.parse-port (%make-lexer-port ":2")))
    => "2")

  (check
      (ascii->string (uri.parse-port (%make-lexer-port ":8080")))
    => "8080")

  (check
      (let* ((in-port	(%make-lexer-port ":8080ciao"))
	     (port	(ascii->string (uri.parse-port in-port)))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list port rest))
    => '("8080" "ciao"))

  #t)


#;(parametrise ((check-test-name	'parsing-path-segments))

;;; path segment

  (check
      (ascii->string (uri.parse-segment (%make-lexer-port "")))
    => "")

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
	     (segment	(ascii->string (uri.parse-segment in-port)))
	     (eof	(lookahead-u8 in-port)))
	(list segment eof))
    => `("ciao" ,(eof-object)))

  (check
      (ascii->string (uri.parse-segment (%make-lexer-port "ciao%3dciao")))
    => "ciao%3dciao")

  (check
      (ascii->string (uri.parse-segment (%make-lexer-port "ciao%3d%3dciao")))
    => "ciao%3d%3dciao")

  (check
      (ascii->string (uri.parse-segment (%make-lexer-port "ciao!$&'()*+,;=:@-._~")))
    => "ciao!$&'()*+,;=:@-._~")

  (check
      (let* ((in-port	(%make-lexer-port "/hello"))
	     (segment1	(ascii->string (uri.parse-segment in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(ascii->string (uri.parse-segment in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("" #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hello"))
	     (segment1	(ascii->string (uri.parse-segment in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(ascii->string (uri.parse-segment in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "?ciao"))
	     (segment	(uri.parse-segment in-port))
	     (query	(ascii->string (uri.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `(#vu8() "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "hello?ciao"))
	     (segment	(ascii->string (uri.parse-segment in-port)))
	     (query	(ascii->string (uri.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "#ciao"))
	     (segment	(uri.parse-segment in-port))
	     (fragment	(ascii->string (uri.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `(#vu8() "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "hello#ciao"))
	     (segment	(ascii->string (uri.parse-segment in-port)))
	     (fragment	(ascii->string (uri.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check	;invalid percent-encoded sequence
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else E))
	(ascii->string (uri.parse-segment (%make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check	;invalid percent-encoded sequence
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else E))
	(ascii->string (uri.parse-segment (%make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; path segment-nz

  (check
      (uri.parse-segment-nz (%make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "{"))
	     (segment	(uri.parse-segment-nz in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list segment char))
    => '(#f #\{))

  (check
      (let* ((in-port	(%make-lexer-port "/"))
	     (segment	(uri.parse-segment-nz in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list segment char))
    => '(#f #\/))

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
	     (segment	(ascii->string (uri.parse-segment-nz in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao:ciao"))
	     (segment	(ascii->string (uri.parse-segment-nz in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao:ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hello"))
	     (segment1	(ascii->string (uri.parse-segment-nz in-port)))
	     (slash	(integer->char (get-u8 in-port)))
	     (segment2	(ascii->string (uri.parse-segment-nz in-port))))
	(list segment1 slash segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao%3dciao"))
	     (segment	(ascii->string (uri.parse-segment-nz in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao%3dciao" ,(eof-object)))

  (let ((S "ciao%3d%3dciao"))
    (check
	(let* ((in-port	(%make-lexer-port S))
	       (segment	(ascii->string (uri.parse-segment-nz in-port))))
	  (list segment (lookahead-u8 in-port)))
      => `(,S ,(eof-object))))

  (check
      (let* ((in-port	(%make-lexer-port "?ciao"))
	     (segment	(uri.parse-segment-nz in-port))
	     (query	(ascii->string (uri.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `(#f "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "#ciao"))
	     (segment	(uri.parse-segment-nz in-port))
	     (fragment	(ascii->string (uri.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `(#f "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "hello?ciao"))
	     (segment	(ascii->string (uri.parse-segment-nz in-port)))
	     (query	(ascii->string (uri.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "hello#ciao"))
	     (segment	(ascii->string (uri.parse-segment-nz in-port)))
	     (fragment	(ascii->string (uri.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check	;invalid percent-encoded sequence
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else #f))
	(ascii->string (uri.parse-segment-nz (%make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check	;invalid percent-encoded sequence
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else #f))
	(ascii->string (uri.parse-segment-nz (%make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; path segment-nz-nc

  (check
      (uri.parse-segment-nz-nc (%make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "{"))
	     (segment	(uri.parse-segment-nz-nc in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list segment char))
    => '(#f #\{))

  (check
      (let* ((in-port	(%make-lexer-port "/"))
	     (segment	(uri.parse-segment-nz-nc in-port))
	     (char	(integer->char (get-u8 in-port))))
	(list segment char))
    => '(#f #\/))

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
	     (segment	(ascii->string (uri.parse-segment-nz-nc in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao" ,(eof-object)))

  (let ((S "ciao:ciao"))
    (check
	(let* ((in-port	(%make-lexer-port S))
	       (segment	(ascii->string (uri.parse-segment-nz-nc in-port)))
	       (rest	(ascii->string (get-bytevector-some in-port))))
	  (list segment rest))
      => '("ciao" ":ciao")))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hello"))
	     (segment1	(ascii->string (uri.parse-segment-nz-nc in-port)))
	     (char	(integer->char (get-u8 in-port)))
	     (segment2	(ascii->string (uri.parse-segment-nz-nc in-port))))
	(list segment1 char segment2 (lookahead-u8 in-port)))
    => `("ciao" #\/ "hello" ,(eof-object)))

  (let ((S "ciao%3dciao"))
    (check
	(let* ((in-port	(%make-lexer-port S))
	       (segment	(ascii->string (uri.parse-segment-nz-nc in-port))))
	  (list segment (lookahead-u8 in-port)))
      => `(,S ,(eof-object))))

  (let ((S "ciao%3d%3dciao"))
    (check
	(let* ((in-port	(%make-lexer-port S))
	       (segment	(ascii->string (uri.parse-segment-nz-nc in-port))))
	  (list segment (lookahead-u8 in-port)))
      => `(,S ,(eof-object))))

  (check
      (let* ((in-port	(%make-lexer-port "?ciao"))
	     (segment	(uri.parse-segment-nz-nc in-port))
	     (query	(ascii->string (uri.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `(#f "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "hello?ciao"))
	     (segment	(ascii->string (uri.parse-segment-nz-nc in-port)))
	     (query	(ascii->string (uri.parse-query in-port))))
	(list segment query (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "#ciao"))
	     (segment	(uri.parse-segment-nz-nc in-port))
	     (fragment	(ascii->string (uri.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `(#f "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "hello#ciao"))
	     (segment	(ascii->string (uri.parse-segment-nz-nc in-port)))
	     (fragment	(ascii->string (uri.parse-fragment in-port))))
	(list segment fragment (lookahead-u8 in-port)))
    => `("hello" "ciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port ":ciao"))
	     (segment	(uri.parse-segment-nz-nc in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list segment rest))
    => '(#f ":ciao"))

  (check	;invalid percent-encoded sequence
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else #f))
	(ascii->string (uri.parse-segment-nz-nc (%make-lexer-port "ciao%3d%3,ciao"))))
    => #t)

  (check	;invalid percent-encoded sequence
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else #f))
	(ascii->string (uri.parse-segment-nz-nc (%make-lexer-port "ciao%,3%3dciao"))))
    => #t)

;;; --------------------------------------------------------------------
;;; slash and segment

  (check
      (uri.parse-slash-and-segment (%make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
	     (segment	(uri.parse-slash-and-segment in-port))
	     (rest	(ascii->string (get-bytevector-some in-port))))
	(list segment rest))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "?ciao"))
	     (segment	(uri.parse-slash-and-segment in-port))
	     (query	(ascii->string (uri.parse-query in-port))))
	(list segment query))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "#ciao"))
	     (segment	(uri.parse-slash-and-segment in-port))
	     (fragment	(ascii->string (uri.parse-fragment in-port))))
	(list segment fragment))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "/"))
	     (segment	(uri.parse-slash-and-segment in-port)))
	(list segment (lookahead-u8 in-port)))
    => `(#vu8() ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello"))
	     (segment1	(ascii->string (uri.parse-slash-and-segment in-port)))
	     (segment2	(ascii->string (uri.parse-slash-and-segment in-port))))
	(list segment1 segment2 (lookahead-u8 in-port)))
    => `("ciao" "hello" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/"))
	     (segment1	(ascii->string (uri.parse-slash-and-segment in-port)))
	     (segment2	(ascii->string (uri.parse-slash-and-segment in-port)))
	     (segment3	(ascii->string (uri.parse-slash-and-segment in-port))))
	(list segment1 segment2 segment3 (lookahead-u8 in-port)))
    => `("ciao" "hello" "" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao%3dciao"))
	     (segment	(ascii->string (uri.parse-slash-and-segment in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao%3dciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao%3d%3dciao"))
	     (segment	(ascii->string (uri.parse-slash-and-segment in-port))))
	(list segment (lookahead-u8 in-port)))
    => `("ciao%3d%3dciao" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/?ciao"))
	     (segment	(ascii->string (uri.parse-slash-and-segment in-port)))
	     (query	(ascii->string (uri.parse-query in-port))))
	(list segment query))
    => '("" "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "/#ciao"))
	     (segment	(ascii->string (uri.parse-slash-and-segment in-port)))
	     (fragment	(ascii->string (uri.parse-fragment in-port))))
	(list segment fragment))
    => '("" "ciao"))

  (check	;invalid percent-encoded sequence
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else #f))
	(ascii->string (uri.parse-slash-and-segment (%make-lexer-port "/ciao%3d%3,ciao"))))
    => #t)

  (check	;invalid percent-encoded sequence
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else #f))
	(ascii->string (uri.parse-slash-and-segment (%make-lexer-port "/ciao%,3%3dciao"))))
    => #t)

  #t)


#;(parametrise ((check-test-name	'parsing-path-types))

;;; path-empty

  (check
      (uri.parse-path-empty (%make-lexer-port ""))
    => '())

  (check
      (let* ((in-port	(%make-lexer-port "?ciao"))
  	     (path	(uri.parse-path-empty in-port))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(vector path query))
    => '#(() "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "#ciao"))
  	     (path	(uri.parse-path-empty in-port))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(vector path fragment))
    => '#(() "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
  	     (path	(uri.parse-path-empty in-port))
  	     (rest	(uri.to-string (get-bytevector-some in-port))))
  	(list path rest))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao"))
  	     (path	(uri.parse-path-empty in-port))
  	     (rest	(uri.to-string (get-bytevector-some in-port))))
  	(list path rest))
    => '(#f "/ciao"))

;;; --------------------------------------------------------------------
;;; path-abempty

  (check
      (uri.parse-path-abempty (%make-lexer-port ""))
    => '())

  (check
      (let* ((in-port	(%make-lexer-port "?query"))
  	     (path	(uri.parse-path-abempty in-port))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query (lookahead-u8 in-port)))
    => `(() "query" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "#fragment"))
  	     (path	(uri.parse-path-abempty in-port))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment (lookahead-u8 in-port)))
    => `(() "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao?query"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query (lookahead-u8 in-port)))
    => `(("ciao") "query" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao#fragment"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao") "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/salut"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/salut?query"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") "query" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/salut/?query"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut" "") "query" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/salut#fragment"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/salut/#fragment"))
  	     (path	(map uri.to-string (uri.parse-path-abempty in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut" "") "fragment" ,(eof-object)))

  (check
      (map uri.to-string (uri.parse-path-abempty (%make-lexer-port "///")))
    => '("" "" ""))

;;; --------------------------------------------------------------------
;;; path-absolute

  (check
      (uri.parse-path-absolute (%make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
  	     (path	(uri.parse-path-absolute in-port))
  	     (rest	(uri.to-string (get-bytevector-some in-port))))
  	(list path rest))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "/"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "//"))
  	     (path	(uri.parse-path-absolute in-port))
  	     (rest	(uri.to-string (get-bytevector-some in-port))))
  	(list path rest))
    => `(#f "//"))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/salut"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/?query"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query (lookahead-u8 in-port)))
    => `(("") "query" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello?query"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query (lookahead-u8 in-port)))
    => `(("ciao" "hello") "query" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/?query"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") "query" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/#fragment"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment (lookahead-u8 in-port)))
    => `(("") "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello#fragment"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao" "hello") "fragment" ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao/hello/#fragment"))
  	     (path	(map uri.to-string (uri.parse-path-absolute in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") "fragment" ,(eof-object)))

;;; --------------------------------------------------------------------
;;; path-noscheme

  (check
      (uri.parse-path-noscheme (%make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/"))
  	     (path	(uri.parse-path-noscheme in-port))
  	     (char	(integer->char (get-u8 in-port))))
  	(list path char))
    => '(#f #\/))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao"))
  	     (path	(uri.parse-path-noscheme in-port))
  	     (rest	(uri.to-string (get-bytevector-some in-port))))
  	(list path rest))
    => '(#f "/ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hello"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hello/salut"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hello/"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/he:llo"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "he:llo") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ci:ao/hello"))
  	     (path	(uri.parse-path-noscheme in-port))
  	     (rest	(uri.to-string (get-bytevector-some in-port))))
  	(list path rest))
    => `(#f "ci:ao/hello"))

  (check
      (let* ((in-port	(%make-lexer-port "?ciao"))
  	     (path	(uri.parse-path-noscheme in-port))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello?ciao"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query))
    => '(("hello") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello/salut?ciao"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query))
    => '(("hello" "salut") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello/salut/?ciao"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query))
    => '(("hello" "salut" "") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "#ciao"))
  	     (path	(uri.parse-path-noscheme in-port))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello#ciao"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment))
    => '(("hello") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello/salut#ciao"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment))
    => '(("hello" "salut") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello/salut/#ciao"))
  	     (path	(map uri.to-string (uri.parse-path-noscheme in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment))
    => '(("hello" "salut" "") "ciao"))

;;; --------------------------------------------------------------------
;;; path-rootless

  (check
      (uri.parse-path-rootless (%make-lexer-port ""))
    => #f)

  (check
      (let* ((in-port	(%make-lexer-port "ciao"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "/"))
  	     (path	(uri.parse-path-rootless in-port))
  	     (char	(integer->char (get-u8 in-port))))
  	(list path char))
    => '(#f #\/))

  (check
      (let* ((in-port	(%make-lexer-port "/ciao"))
  	     (path	(uri.parse-path-rootless in-port))
  	     (rest	(uri.to-string (get-bytevector-some in-port))))
  	(list path rest))
    => '(#f "/ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hello"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hello/salut"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "salut") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hel:lo"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hel:lo") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ci:ao/hel:lo"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ci:ao" "hel:lo") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "ciao/hello/"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port))))
  	(list path (lookahead-u8 in-port)))
    => `(("ciao" "hello" "") ,(eof-object)))

  (check
      (let* ((in-port	(%make-lexer-port "?ciao"))
  	     (path	(uri.parse-path-rootless in-port))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello?ciao"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query))
    => '(("hello") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello/salut?ciao"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query))
    => '(("hello" "salut") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello/salut/?ciao"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port)))
  	     (query	(uri.to-string (uri.parse-query in-port))))
  	(list path query))
    => '(("hello" "salut" "") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "#ciao"))
  	     (path	(uri.parse-path-rootless in-port))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment))
    => '(#f "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello#ciao"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment))
    => '(("hello") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello/salut#ciao"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment))
    => '(("hello" "salut") "ciao"))

  (check
      (let* ((in-port	(%make-lexer-port "hello/salut/#ciao"))
  	     (path	(map uri.to-string (uri.parse-path-rootless in-port)))
  	     (fragment	(uri.to-string (uri.parse-fragment in-port))))
  	(list path fragment))
    => '(("hello" "salut" "") "ciao"))

  #t)


#;(parametrise ((check-test-name	'parsing-path))

  (check
      (receive (type segments)
	  (uri.parse-path (%make-lexer-port ""))
	(vector type (map uri.to-string segments)))
    => '#(path-empty ()))

  (check
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else E))
	(uri.parse-path (%make-lexer-port "?query")))
    => #t)

  (check
      (guard (E ((uri.parser-error-condition? E)
		 #t)
		(else E))
	(uri.parse-path (%make-lexer-port "#fragment")))
    => #t)

  (check
      (receive (type segments)
	  (uri.parse-path (%make-lexer-port "/ciao/hello/salut"))
	(vector type (map uri.to-string segments)))
    => '#(path-absolute ("ciao" "hello" "salut")))

  (check
      (receive (type segments)
	  (uri.parse-path (%make-lexer-port "/"))
	(vector type segments))
    => '#(path-absolute (#vu8())))

  (check
      (receive (type segments)
	  (uri.parse-path (%make-lexer-port "//"))
	(vector type (map uri.to-string segments)))
    => '#(path-abempty ("" "")))

  (check
      (receive (type segments)
	  (uri.parse-path (%make-lexer-port "///"))
	(vector type (map uri.to-string segments)))
    => '#(path-abempty ("" "" "")))

  (check
      (receive (type segments)
	  (uri.parse-path (%make-lexer-port "//ciao/"))
	(vector type (map uri.to-string segments)))
    => '#(path-abempty ("" "ciao" "")))

  (check
      (receive (type segments)
	  (uri.parse-path (%make-lexer-port "ciao/hello/salut"))
	(vector type (map uri.to-string segments)))
    => '#(path-noscheme ("ciao" "hello" "salut")))

  (check
      (receive (type segments)
	  (uri.parse-path (%make-lexer-port "ci:ao/hello/salut"))
	(vector type (map uri.to-string segments)))
    => '#(path-rootless ("ci:ao" "hello" "salut")))

  #t)


#;(parametrise ((check-test-name	'parse-uri))

  (define-inline (doit in-string expected-value)
    (check
	(let-values (((scheme authority userinfo host-type host port path-type path query fragment)
		      (uri.parse-uri (%make-lexer-port in-string))))
	  (list (and scheme		(uri.to-string scheme))
		(and authority		(uri.to-string authority))
		(and userinfo		(uri.to-string userinfo))
		host-type
		(and host
		     (case host-type
		       ((reg-name)
			(uri.to-string host))
		       ((ipv4-address)
			(cons (uri.to-string (car host)) (cdr host)))
		       ((ipv6-address)
			(cons (uri.to-string (car host)) (cdr host)))
		       ((ipvfuture)
			(cons (car host) (uri.to-string (cdr host))))
		       (else #f)))
		(and port		(uri.to-string port))
		path-type
		(map uri.to-string path)
		(and query		(uri.to-string query))
		(and fragment		(uri.to-string fragment))))
      => (quasiquote expected-value)))

;;; whith scheme

    (doit "ci:ao/"
    	  ("ci" #f #f reg-name "" #f path-rootless ("ao" "") #f #f))

    (doit "ci:ao/a///"
    	  ("ci" #f #f reg-name "" #f path-rootless ("ao" "a" "" "" "") #f #f))

    (doit "ci:ao/ciao"
    	  ("ci" #f #f reg-name "" #f path-rootless ("ao" "ciao") #f #f))

    (doit "ci:ao/ciao/hello/salut"
    	  ("ci" #f #f reg-name "" #f path-rootless ("ao" "ciao" "hello" "salut") #f #f))

    (doit "http://"
	  ("http" "" #f reg-name "" #f path-abempty () #f #f))

    (doit "http://?query" ;empty authority
    	  ("http" "" #f reg-name "" #f path-abempty () "query" #f))

    (doit "http://#fragment"	;empty authority
    	  ("http" "" #f reg-name "" #f path-abempty () #f "fragment"))

    (doit "http:///"	;empty authority
    	  ("http" "" #f reg-name "" #f path-abempty ("") #f #f))

    (doit "http:///?query" ;empty authority
    	  ("http" "" #f reg-name "" #f path-abempty ("") "query" #f))

    (doit "http:///#fragment" ;empty authority
    	  ("http" "" #f reg-name "" #f path-abempty ("") #f "fragment"))

    (doit "http:///ciao" ;empty authority
    	  ("http" "" #f reg-name "" #f path-abempty ("ciao") #f #f))

    (doit "http://ciao.com"
    	  ("http" "ciao.com" #f reg-name "ciao.com" #f path-abempty () #f #f))

    (doit "http://ciao.com:8080"
    	  ("http" "ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty () #f #f))

    (doit "http://marco@ciao.com:8080"
    	  ("http" "marco@ciao.com:8080" "marco" reg-name "ciao.com" "8080" path-abempty () #f #f))

    (doit "http://ciao.com:8080/"
    	  ("http" "ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty ("") #f #f))

    (doit "http://ciao.com:8080/a"
    	  ("http" "ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty ("a") #f #f))

    (doit "http://ciao.com/a/b/c"
    	  ("http" "ciao.com" #f reg-name "ciao.com" #f path-abempty ("a" "b" "c") #f #f))

    (doit "http://ciao.com:8080/a/b/c"
    	  ("http" "ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty ("a" "b" "c") #f #f))

    (doit "http://1.2.3.4:8080/a/b/c"
    	  ("http" "1.2.3.4:8080" #f ipv4-address ("1.2.3.4" . (1 2 3 4))
	   "8080" path-abempty ("a" "b" "c") #f #f))

    (doit "http://[1:2:3:4:5:6:7:8]:8080/a/b/c"
    	  ("http" "[1:2:3:4:5:6:7:8]:8080" #f ipv6-address ("1:2:3:4:5:6:7:8" . (1 2 3 4 5 6 7 8))
	   "8080" path-abempty ("a" "b" "c") #f #f))

    (doit "http://[vEciao]:8080/a/b/c"
    	  ("http" "[vEciao]:8080" #f ipvfuture (14 . "ciao")
	   "8080" path-abempty ("a" "b" "c") #f #f))

;;; with authority, no scheme

    (doit "//"
	  (#f "" #f reg-name "" #f path-abempty () #f #f))

    (doit "//?query" ;empty authority
    	  (#f "" #f reg-name "" #f path-abempty () "query" #f))

    (doit "//#fragment"	;empty authority
    	  (#f "" #f reg-name "" #f path-abempty () #f "fragment"))

    (doit "///"	;empty authority
    	  (#f "" #f reg-name "" #f path-abempty ("") #f #f))

    (doit "///?query" ;empty authority
    	  (#f "" #f reg-name "" #f path-abempty ("") "query" #f))

    (doit "///#fragment" ;empty authority
    	  (#f "" #f reg-name "" #f path-abempty ("") #f "fragment"))

    (doit "///ciao" ;empty authority
    	  (#f "" #f reg-name "" #f path-abempty ("ciao") #f #f))

    (doit "//ciao.com"
    	  (#f "ciao.com" #f reg-name "ciao.com" #f path-abempty () #f #f))

    (doit "//ciao.com:8080"
    	  (#f "ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty () #f #f))

    (doit "//marco@ciao.com:8080"
    	  (#f "marco@ciao.com:8080" "marco" reg-name "ciao.com" "8080" path-abempty () #f #f))

    (doit "//ciao.com:8080/"
    	  (#f "ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty ("") #f #f))

    (doit "//ciao.com:8080/a"
    	  (#f "ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty ("a") #f #f))

    (doit "//ciao.com/a/b/c"
    	  (#f "ciao.com" #f reg-name "ciao.com" #f path-abempty ("a" "b" "c") #f #f))

    (doit "//ciao.com:8080/a/b/c"
    	  (#f "ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty ("a" "b" "c") #f #f))

;;; no authority, emtpy path

    (doit ""
    	  (#f #f #f reg-name "" #f path-empty () #f #f))

    (doit "?query"
    	  (#f #f #f reg-name "" #f path-empty () "query" #f))

    (doit "#fragment"
    	  (#f #f #f reg-name "" #f path-empty () #f "fragment"))

;;; no authority, absolute path

    (doit "/"
    	  (#f #f #f reg-name "" #f path-absolute ("") #f #f))

    (doit "/a///"
    	  (#f #f #f reg-name "" #f path-absolute ("a" "" "" "") #f #f))

    (doit "/ciao"
    	  (#f #f #f reg-name "" #f path-absolute ("ciao") #f #f))

    (doit "/ciao/hello/salut"
    	  (#f #f #f reg-name "" #f path-absolute ("ciao" "hello" "salut") #f #f))

;;; no authority, relative path rootless

    (doit "./"
    	  (#f #f #f reg-name "" #f path-rootless ("." "") #f #f))

    (doit "./a///"
    	  (#f #f #f reg-name "" #f path-rootless ("." "a" "" "" "") #f #f))

    (doit "./ciao"
    	  (#f #f #f reg-name "" #f path-rootless ("." "ciao") #f #f))

    (doit "./ciao/hello/salut"
    	  (#f #f #f reg-name "" #f path-rootless ("." "ciao" "hello" "salut") #f #f))

  #t)


#;(parametrise ((check-test-name	'parse-relative-ref))

  (define-inline (doit in-string expected-value)
    (check
	(let-values (((authority userinfo host-type host port path-type path query fragment)
		      (uri.parse-relative-ref (%make-lexer-port in-string))))
	  (list (and authority		(uri.to-string authority))
		(and userinfo		(uri.to-string userinfo))
		host-type
		(and host		(uri.to-string host))
		(and port		(uri.to-string port))
		path-type
		(map uri.to-string path)
		(and query		(uri.to-string query))
		(and fragment		(uri.to-string fragment))))
      => (quote expected-value)))

;;; with authority, no scheme

    (doit "//"
	  ("" #f reg-name "" #f path-abempty () #f #f))

    (doit "//?query" ;empty authority
    	  ("" #f reg-name "" #f path-abempty () "query" #f))

    (doit "//#fragment"	;empty authority
    	  ("" #f reg-name "" #f path-abempty () #f "fragment"))

    (doit "///"	;empty authority
    	  ("" #f reg-name "" #f path-abempty ("") #f #f))

    (doit "///?query" ;empty authority
    	  ("" #f reg-name "" #f path-abempty ("") "query" #f))

    (doit "///#fragment" ;empty authority
    	  ("" #f reg-name "" #f path-abempty ("") #f "fragment"))

    (doit "///ciao" ;empty authority
    	  ("" #f reg-name "" #f path-abempty ("ciao") #f #f))

    (doit "//ciao.com"
    	  ("ciao.com" #f reg-name "ciao.com" #f path-abempty () #f #f))

    (doit "//ciao.com:8080"
    	  ("ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty () #f #f))

    (doit "//marco@ciao.com:8080"
    	  ("marco@ciao.com:8080" "marco" reg-name "ciao.com" "8080" path-abempty () #f #f))

    (doit "//ciao.com:8080/"
    	  ("ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty ("") #f #f))

    (doit "//ciao.com:8080/a"
    	  ("ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty ("a") #f #f))

    (doit "//ciao.com/a/b/c"
    	  ("ciao.com" #f reg-name "ciao.com" #f path-abempty ("a" "b" "c") #f #f))

    (doit "//ciao.com:8080/a/b/c"
    	  ("ciao.com:8080" #f reg-name "ciao.com" "8080" path-abempty ("a" "b" "c") #f #f))

;;; no authority, emtpy path

    (doit ""
    	  (#f #f reg-name "" #f path-empty () #f #f))

    (doit "?query"
    	  (#f #f reg-name "" #f path-empty () "query" #f))

    (doit "#fragment"
    	  (#f #f reg-name "" #f path-empty () #f "fragment"))

;;; no authority, absolute path

    (doit "/"
    	  (#f #f reg-name "" #f path-absolute ("") #f #f))

    (doit "/a///"
    	  (#f #f reg-name "" #f path-absolute ("a" "" "" "") #f #f))

    (doit "/ciao"
    	  (#f #f reg-name "" #f path-absolute ("ciao") #f #f))

    (doit "/ciao/hello/salut"
    	  (#f #f reg-name "" #f path-absolute ("ciao" "hello" "salut") #f #f))

;;; no authority, relative path rootless

    (doit "./"
    	  (#f #f reg-name "" #f path-noscheme ("." "") #f #f))

    (doit "./a///"
    	  (#f #f reg-name "" #f path-noscheme ("." "a" "" "" "") #f #f))

    (doit "./ciao"
    	  (#f #f reg-name "" #f path-noscheme ("." "ciao") #f #f))

    (doit "./ciao/hello/salut"
    	  (#f #f reg-name "" #f path-noscheme ("." "ciao" "hello" "salut") #f #f))

;;; --------------------------------------------------------------------

    (check	;whith scheme-like first segment
	(guard (E ((uri.parser-error-condition? E)
;;;(write (condition-message E))(newline)
		   #t)
		  (else E))
	  (uri.parse-relative-ref (%make-lexer-port "ci:ao/")))
      => #t)

  #t)


#;(parametrise ((check-test-name	'class-uri))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?string)
       (doit ?string ?string))
      ((_ ?input-string ?expected-string)
       (begin
	 (check
	     (let (((o uri.<uri>) (make uri.<uri>
				    (uri.source-bytevector (uri.to-bytevector ?input-string)))))
	       o.string)
	   => ?expected-string)
	 (check
	     (let (((o uri.<uri>) (make uri.<uri>
				    (uri.source-bytevector (uri.to-bytevector ?input-string)))))
	       o.bytevector)
	   => (uri.to-bytevector ?expected-string))))))

;;; --------------------------------------------------------------------

  (doit "http://www.spiffy.org/the/path/name?question%3Danswer#anchor-point")

  (doit "ci:ao/")
  (doit "ci:ao/a///")
  (doit "ci:ao/ciao")
  (doit "ci:ao/ciao/hello/salut")
  (doit "http://")
  (doit "http://?query")
  (doit "http://#fragment")
  (doit "http:///")
  (doit "http:///?query" )
  (doit "http:///ciao" )
  (doit "http://ciao.com:8080")
  (doit "http://ciao.com:8080/")
  (doit "http://ciao.com/a/b/c")

;;; with authority

  (doit "http://")
  (doit "http://#fragment")
  (doit "http:///?query")
  (doit "http:///ciao")
  (doit "http://ciao.com:8080")
  (doit "http://ciao.com:8080/")
  (doit "http://ciao.com/a/b/c")

;;; no authority, emtpy path

  (doit "http:" "http://")
  (doit "http:?query" "http://?query")
  (doit "http:#fragment" "http://#fragment")

;;; no authority, absolute path

  (doit "http:/")
  (doit "http:/ciao")
  (doit "http:/ciao/hello/salut")

;;; no authority, relative path rootless

  (doit "http:./")
  (doit "http:./a///")
  (doit "http:./ciao")
  (doit "http:./ciao/hello/salut")

;;; IPv4address

  (doit "http://1.2.3.4/a/b/c")
  (doit "http://10.20.30.40/a/b/c")

;;; IPv6address

  (doit "http://[1:2:3:4:5:6:7:8]/a/b/c")
  (doit "http://[a:b:c:d:e:f:a:b]/a/b/c")
  (doit "http://[1:2:3:4::172.30.67.254]/a/b/c")

;;; ipvfuture

  (doit "http://[v412345]/a/b/c")
  (doit "http://[vF12345]/a/b/c")

  #t)


#;(parametrise ((check-test-name	'class-relative-ref))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?string)
       (doit ?string ?string))
      ((_ ?input-string ?expected-string)
       (begin
	 (check
	     (let (((o uri.<relative-ref>) (make uri.<relative-ref>
					     (uri.source-bytevector (uri.to-bytevector ?input-string)))))
	       o.string)
	   => ?expected-string)
	 (check
	     (let (((o uri.<relative-ref>) (make uri.<relative-ref>
					     (uri.source-bytevector (uri.to-bytevector ?input-string)))))
	       o.bytevector)
	   => (uri.to-bytevector ?expected-string))))))

;;; --------------------------------------------------------------------

;;; with authority, no scheme

  (doit "//")
  (doit "//?query")
  (doit "//#fragment")
  (doit "///")
  (doit "///?query")
  (doit "///#fragment")
  (doit "///ciao")
  (doit "//ciao.com")
  (doit "//ciao.com:8080")
  (doit "//marco@ciao.com:8080")
  (doit "//ciao.com:8080/")
  (doit "//ciao.com:8080/a")
  (doit "//ciao.com/a/b/c")
  (doit "//ciao.com:8080/a/b/c")

;;; no authority, emtpy path

  (doit "" "//")
  (doit "?query" "//?query")
  (doit "#fragment" "//#fragment")

;;; no authority, absolute path

  (doit "/")
  (doit "/a///")
  (doit "/ciao")
  (doit "/ciao/hello/salut")

;;; no authority, relative path rootless

  (doit "./")
  (doit "./a///")
  (doit "./ciao")
  (doit "./ciao/hello/salut")

;;; IPv4address

  (doit "//1.2.3.4/a/b/c")
  (doit "//10.20.30.40/a/b/c")

;;; IPv6address

  (doit "//[1:2:3:4:5:6:7:8]/a/b/c")
  (doit "//[a:b:c:d:e:f:a:b]/a/b/c")
  (doit "//[1:2:3:4::172.30.67.254]/a/b/c")

;;; ipvfuture

  (doit "//[v412345]/a/b/c")
  (doit "//[vF12345]/a/b/c")

  #t)


#;(parametrise ((check-test-name	'normalise-path))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?input ?output)
       (check
	   (map uri.to-string
	     (uri.normalise-path
	      (map uri.to-bytevector
		(quote ?input))))
	 => (quote ?output)))))

  (doit () ())

  (doit ("a") ("a"))

  (doit (".") ())

  (doit ("..") ())

  (doit ("a" "b" "c" "." ".." ".." "g")
	("a" "g"))

  #t)


;;;; done

(check-report)

;;; end of file
