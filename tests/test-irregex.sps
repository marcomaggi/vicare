;;;
;;;Part of: Vicare/Scheme
;;;Contents: compilation script
;;;Date: Thu Dec 25, 2008
;;;
;;;Abstract
;;;
;;;	This  file was  originally from  the IrRegex  distribution, then
;;;	adapted  to  the Nausicaa  distributiona,  then  adapted to  the
;;;	Vicare Scheme distribution.
;;;
;;;Copyright (c) 2008, 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.itn>
;;;Copyright (c) 2005-2009 Alex Shinn.  All rights reserved.
;;;Sometests adapted from SCSH SRE tests by Christoph Hetz.
;;;
;;;Redistribution and  use in source  and binary forms, with  or without
;;;modification,  are permitted provided  that the  following conditions
;;;are met:
;;;
;;;1. Redistributions  of source  code must  retain the  above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;;2. Redistributions in binary  form must reproduce the above copyright
;;;   notice, this  list of conditions  and the following  disclaimer in
;;;   the  documentation  and/or   other  materials  provided  with  the
;;;   distribution.
;;;
;;;3. The name of  the author  may not  be used  to endorse  or  promote
;;;   products derived from this software without specific prior written
;;;   permission.
;;;
;;;THIS SOFTWARE IS PROVIDED BY THE  AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;;IMPLIED  WARRANTIES,  INCLUDING,  BUT  NOT LIMITED  TO,  THE  IMPLIED
;;;WARRANTIES OF  MERCHANTABILITY AND  FITNESS FOR A  PARTICULAR PURPOSE
;;;ARE  DISCLAIMED.  IN  NO EVENT  SHALL THE  AUTHOR BE  LIABLE  FOR ANY
;;;DIRECT,  INDIRECT, INCIDENTAL,  SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL
;;;DAMAGES  (INCLUDING, BUT  NOT LIMITED  TO, PROCUREMENT  OF SUBSTITUTE
;;;GOODS  OR  SERVICES; LOSS  OF  USE,  DATA,  OR PROFITS;  OR  BUSINESS
;;;INTERRUPTION) HOWEVER CAUSED AND  ON ANY THEORY OF LIABILITY, WHETHER
;;;IN  CONTRACT,  STRICT LIABILITY,  OR  TORT  (INCLUDING NEGLIGENCE  OR
;;;OTHERWISE) ARISING IN  ANY WAY OUT OF THE USE  OF THIS SOFTWARE, EVEN
;;;IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#!r6rs
(import (vicare)
  (vicare irregex)
  (vicare checks)
  (only (srfi :1 lists) iota))

(check-set-mode! 'report-failed)
(check-display "*** testing irregex\n")

(define-syntax rx
  (syntax-rules ()
    ((rx sre) `sre)
    ((rx sre0 sre1 ...) `(seq sre0 sre1 ...))))

(define test-string
  "Dieser Test-String wurde am 29.07.2004 um 5:23PM erstellt.\na aa aaa aaaa\nab aabb aaabbb\naba abba abbba\n1 12 123 1234\nyyyyyyyyyy\n")


;;;; helpers

(define rope-chunker
  (make-irregex-chunker
   (lambda (x) (and (pair? (cdr x)) (cdr x)))
   caar
   cadar
   caddar
   (lambda (src1 i src2 j)
     (if (eq? src1 src2)
         (substring (caar src1) i j)
       (let lp ((src (cdr src1))
		(res (list (substring (caar src1) i (caddar src1)))))
	 (if (eq? src src2)
	     (string-append
	      (reverse (cons (substring (caar src2) (cadar src2) j) res)))
	   (lp (cdr src)
	       (cons (substring (caar src) (cadar src) (caddar src))
		     res))))))))



(parametrise ((check-test-name 'pcre))

  (check-for-true
   (irregex-match-data? (irregex-search "\\x41," "A,")))

  (check-for-true
   (irregex-match-data? (irregex-search "\\x{0041}" "A,")))

  (check-for-true
   (irregex-match-data? (irregex-search "\\x{0041}" "A,")))

  (check-for-true
   (irregex-match-data? (irregex-search "^a+$" "caabb" 1 3)))

  ;;This tests that the last index argument to IRREGEX-SEARCH is a "past
  ;;index",  that  is  a  non-inclusive  right limit  for  the  selected
  ;;substring (it was not explicitly documented).
  (check-for-false
   (irregex-match-data? (irregex-search "^a+$" "caabb" 1 4)))

;;; --------------------------------------------------------------------

  (check
      (irregex-match-data? (irregex-search "<[[:alpha:]]+>" "<abc>"))
    => #t)

  (check
      ;;does not match
      (irregex-search "<[[:alpha:]]+>" "<ab7c>")
    => #f)

  (check
      (irregex-match-data? (irregex-search "<[[^:alpha:]]+>" "<123>"))
    => #t)

  (check
      ;;does not match
      (irregex-search "<[[^:alpha:]]+>" "<12a3>")
    => #f)

  (check
      (guard (exc (else (condition? exc)))
	(irregex-search "<[[=alpha=]]+>" "<abc>"))
    => #t)

  (check
      (guard (exc (else (condition? exc)))
	(irregex-search "<[[.alpha.]]+>" "<abc>"))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (irregex-match-data? (irregex-match "\\Q.*\\+" ".*\\+"))
    => #t)

  (check
      (irregex-match-data? (irregex-match "\\Q.*\\+\\E" ".*\\+"))
    => #t)

  (check
      ;;does not match
      (irregex-match "\\Q.*\\+\\E" "x*\\+")
    => #f)

  (check
      (irregex-match-data? (irregex-match "\\Q.*\\" ".*\\"))
    => #t)

  (let ((rex "[a-z]+"))
    (check-for-true (irregex-search rex "123abc456"))
    (check (irregex-search rex "123456") => #f))

  (let ((rex "foobar"))
    (check-for-true (irregex-search rex "abcfoobardef"))
    (check (irregex-search rex "abcFOOBARdef") => #f))

  (let ((rex (string->irregex "[a-z]+")))
    (check-for-true (irregex-search rex "123abc456"))
    (check (irregex-search rex "123456") => #f))

  (let ((rex (string->irregex "foobar" 'case-insensitive)))
    (check-for-true (irregex-search rex "abcfoobardef"))
    (check-for-true (irregex-search rex "abcFOOBARdef")))

  )


(parametrise ((check-test-name 'sre))

  (let ((rex '(w/nocase "foobar")))
    (check-for-true (irregex-search rex "abcFOOBARdef"))
    (check-for-true (irregex-search rex "foobar"))
    (check-for-true (irregex-search rex "FOOBAR")))

  ;;IRREGEX-MATCH performs a  search anchored to the beginning  and end of
  ;;the string.
  (let ((rex '(w/nocase "foobar")))
    (check (irregex-match rex "abcFOOBARdef") => #f)
    (check-for-true (irregex-match rex "foobar"))
    (check-for-true (irregex-match rex "FOOBAR")))

  (check-for-true
   (irregex-match-data? (irregex-search '(- alpha ("aeiouAEIOU")) test-string)))

  (check-for-true
   (irregex-match-data? (irregex-search '(- (/"azAZ") ("aeiouAEIOU")) test-string)))

  (check-for-true
   (irregex-match-data? (irregex-search '(- (/"azAZ") ("aeiouAEIOU")) test-string)))

  (check-for-true
   (irregex-match-data? (irregex-search '(- alpha ("aeiou") ("AEIOU")) test-string)))

  (check-for-true
   (irregex-match-data? (irregex-search '(- alpha ("aeiou") ("AEIOU")) test-string)))

  (check-for-true
   (irregex-match-data? (irregex-search '(w/nocase (- alpha ("aeiou"))) test-string)))

  (check-for-true
   (irregex-match-data? (irregex-search '(w/nocase (- alpha ("aeiou"))) test-string)))

  (check-for-true
   (irregex-match-data? (irregex-search '(w/nocase (- (/ "az") ("aeiou"))) test-string)))

  (check-for-true
   (irregex-match-data? (irregex-search '(or upper ("aeiou") digit) "xxx A yyy")))

  (check-for-true
   (irregex-match-data? (irregex-search '(or (/ "AZ09") ("aeiou")) "xxx A yyy")))

  (check-for-true
   (irregex-match-data? (irregex-search '(or upper ("aeiou") digit) "xxx a yyy")))

  (check-for-true
   (irregex-match-data? (irregex-search '(or (/ "AZ09") ("aeiou")) "xxx a yyy")))

  (check-for-true
   (irregex-match-data? (irregex-search '(or upper ("aeiou") digit) "xxx 6 yyy")))

  (check-for-true
   (irregex-match-data? (irregex-search  '(or (/ "AZ09") ("aeiou")) "xxx 6 yyy")))

  (let ((csl (lambda (re) `(or "" (: ,re (* ", " ,re))))))
    (check-for-true
     (irregex-match-data? (irregex-search (csl '(or "John" "Paul" "George" "Ringo"))
					  "George, Ringo, Paul, John"))))

  (check-for-false
   (irregex-search (rx "Erstellt.") test-string))

  (check-for-true
   (irregex-search (rx ("abcde")) test-string))

  (check-for-true
   (irregex-search (rx ("edcba")) test-string))

  (check-for-true
   (irregex-search (rx (or "erstellt." "xxx")) test-string))

  (check-for-true
   (irregex-search (rx (or "xxx" "erstellt.")) test-string))

  (check-for-false
   (irregex-search (rx (>= 11 "y")) test-string))

  (check-for-false
   (irregex-search (rx (= 11 "y")) test-string))

  (check-for-false
   (irregex-search (rx (** 11 12 "y")) test-string))

  (check-for-false
   (irregex-search (rx (** 12 11 any)) test-string))

  (check-for-true
   (irregex-search (rx ("abcd")) test-string))

  (check-for-true
   (irregex-search (rx (or #\a #\b #\c #\d)) test-string))

  (check-for-true
   (irregex-search (rx ("xy")) test-string))

  (check-for-true
   (irregex-search (rx (or #\x #\y)) test-string))

  (check-for-true
   (irregex-search (rx lower-case) test-string))

  (check-for-true
   (irregex-search (rx (- alphabetic upper-case)) test-string))

  (check-for-true
   (irregex-search (rx upper-case) test-string))

  (check-for-true
   (irregex-search (rx (- alphabetic lower-case)) test-string))

  (check-for-false
   (irregex-search (rx (w/nocase (~ "a"))) "aA"))

  (check-for-true
   (irregex-search (rx (w/nocase "abc"
				 (* "FOO" (w/case "Bar"))
				 ("aeiou")))
		   "kabcfooBariou"))

  (check-for-false
   (irregex-search (rx (w/nocase "abc"
				 (* "FOO" (w/case "Bar"))
				 ("aeiou")))
		   "kabcfooBARiou"))

  (check-for-false
   (irregex-search (rx "abc") "abcdefg" 3))

  (check-for-false
   (irregex-search (rx "cba") "abcdefg"))

  )


(parametrise ((check-test-name 'match))

  (let ((match (irregex-search "ciao" "hello ciao salut")))
    (check (irregex-match-substring match) => "ciao")
    (check (irregex-match-substring match 0) => "ciao")
    #;(check (irregex-match-substring match 1) => #f))

  ;;Grouping parentheses.
  (let ((match (irregex-search "c(i(a(o)))"
			       "hello ciao salut")))
    (check (irregex-match-substring match) => "ciao")
    (check (irregex-match-substring match 0) => "ciao")
    (check (irregex-match-substring match 1) => "iao")
    (check (irregex-match-substring match 2) => "ao")
    (check (irregex-match-substring match 3) => "o")

    (check (irregex-match-start-index match 0) => 6)
    )

  ;;Non-grouping parentheses.
  (let ((match (irregex-search "c(i(?:a(o)))"
			       "hello ciao salut")))
    (check (irregex-match-substring match) => "ciao")
    (check (irregex-match-substring match 0) => "ciao")
    (check (irregex-match-substring match 1) => "iao")
    (check (irregex-match-substring match 2) => "o"))

  (check
      (let ((str "(caaadadr ..."))
	(irregex-match-substring
	 (irregex-search '(: "c" (+ (or "a" "d")) "r") str)))
    => "caaadadr")

  (check
      (let ((str "(caaadadr ..."))
	(irregex-match-substring
	 (irregex-search '(: "c" (** 1 6 ("ad")) "r") str)))
    => "caaadadr")

  (check-for-false
   (irregex-search '(: "c" (** 1 4 ("ad")) "r") "(caaadadr ..."))

  (let ((str "bla hello bla"))
    (check
	  (irregex-match-substring
	   (irregex-search (rx (dsm 1 0 (submatch "hello"))) str)
	   2)
      => "hello")

    (check-for-false
     (irregex-match-substring
      (irregex-search (rx (dsm 1 0 (submatch "hello"))) str)
      1))

    (check
	(irregex-match-substring
	 (irregex-search (rx (dsm 2 0 (submatch "hello"))) str)
	 3)
      => "hello")

    (check-for-false
     (irregex-match-substring
      (irregex-search (rx (dsm 2 0 (submatch "hello"))) str)
      1))

    (check-for-false
     (irregex-match-substring
      (irregex-search (rx (dsm 2 0 (submatch "hello"))) str)
      2))
    )

  (check
      (irregex-match-substring (irregex-search (rx "erstellt.") test-string))
    => "erstellt.")

  (check
    (irregex-match-substring
     (irregex-search (rx (: "1" any any "4")) test-string))
    => "1234")

  (check
      (irregex-match-substring (irregex-search (rx (* "y")) test-string))
    =>  "")

  (check
      (irregex-match-substring (irregex-search (rx (* "D")) test-string))
    => "D")

  (check
      (irregex-match-substring (irregex-search (rx (+ "y")) test-string))
    =>  "yyyyyyyyyy")

  (check
      (irregex-match-substring (irregex-search (rx (+ "D")) test-string))
    =>  "D")

  (check
      (irregex-match-substring (irregex-search (rx (? "y")) test-string))
    =>  "")

  (check
      (irregex-match-substring (irregex-search (rx (? "D")) test-string))
    =>  "D")

  (check
      (irregex-match-substring (irregex-search (rx (= 5 "y")) test-string))
    =>  "yyyyy")

  (check
      (irregex-match-substring (irregex-search (rx (>= 5 "y")) test-string))
    =>  "yyyyyyyyyy")

  (check
      (irregex-match-substring (irregex-search (rx (>= 10 "y")) test-string))
    => "yyyyyyyyyy")

  (check
      (irregex-match-substring (irregex-search (rx (** 1 30 "y")) test-string))
    => "yyyyyyyyyy")

  (check
      (irregex-match-substring (irregex-search (rx (** 1 5 "y")) test-string))
    => "yyyyy")

  (check
      (irregex-match-substring (irregex-search (rx (** 0 0 any)) test-string))
    => "")

  (check
      (irregex-match-substring (irregex-search (rx (/ #\A #\Z #\a #\z #\0 #\9)) test-string))
    =>  "D")

  (check
      (irregex-match-substring (irregex-search (rx (/ #\A "Zaz0" #\9)) test-string))
    =>  "D")

  (check
      (irregex-match-substring (irregex-search (rx (/ #\a #\z #\0 #\9)) test-string))
    =>  "i")

  (check
      (irregex-match-substring (irregex-search (rx (/ #\a "z0" #\9)) test-string))
    => "i")

  (check
      (irregex-match-substring (irregex-search (rx (/ #\0 #\9)) test-string))
    => "2")

  (check
      (irregex-match-substring (irregex-search (rx (/ "0" #\9)) test-string))
    =>  "2")

  (check
      (irregex-match-substring (irregex-search (rx numeric) test-string))
    =>  "2")

  (check
      (irregex-match-substring (irregex-search (rx punctuation) test-string))
    => "-")

  (check
      (irregex-match-substring (irregex-search (rx blank) test-string))
    => " ")

  (check
      (irregex-match-substring (irregex-search (rx whitespace) test-string))
    => " ")

  (check
      (irregex-match-substring (irregex-search (rx control) test-string))
    =>  "\n")

  (check
      (irregex-match-substring (irregex-search (rx hex-digit) test-string))
    => "D")

  (check
      (irregex-match-substring (irregex-search (rx ascii) test-string))
    => "D")

  (let ((str "I am feeding the goose, you are feeding the geese.")
	(me 1)
	(you 2))
    (check
	(irregex-match-substring
	 (irregex-search (rx (: "feeding the " ,(if (> me 1) "geese" "goose")))
			 str))
      => "feeding the goose")

    (check
	(irregex-match-substring
	 (irregex-search (rx (: "feeding the " ,(if (> you 1) "geese" "goose")))
			 str))
      => "feeding the geese"))

  (let* ((ws (rx (+ whitespace)))
	 (date (rx (: (or "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul")
		      ,ws
		      (or ("123456789")
			  (: ("12") digit)
			  "30"
			  "31"))))
	 (str1 "it was on Mar 14 ...")
	 (str2 "it was on May 31 ..."))
    (check
	(irregex-match-substring (irregex-search (rx (: "on " ,date)) str1))
      => "on Mar 14")
    (check
	(irregex-match-substring (irregex-search (rx (: "on " ,date)) str2))
      => "on May 31"))

  (check
      (irregex-match-substring (irregex-search (rx "abc") "abcdefg"))
    => "abc")

  ;; unmatchable patterns
  (check
      (irregex-search '(or) "abc")
    => #f)
  (check
      (irregex-search '(: "ab" (or)) "abc")
    => #f)
  (check
      (irregex-search '(submatch "ab" (or)) "abc")
    => #f)
  (check
      (irregex-search '(: "ab" (submatch (or))) "abc")
    => #f)
  (check
      (irregex-search '(/) "abc")
    => #f)
  (check
      (irregex-search '(: "ab" (/)) "abc")
    => #f)
  (check
      (irregex-search '(~ any) "abc")
    => #f)
  (check
      (irregex-search '(: "ab" (~ any)) "abc")
    => #f)
  (check
      (irregex-search '("") "abc")
    => #f)
  (check
      (irregex-search '(: "ab" ("")) "abc")
    => #f)

  ;; beginning/end of chunks
  (check-for-true
      (irregex-search/chunked '(: bos "foo") rope-chunker '((" foo" 0 4)) 1))
  (check-for-true
      (irregex-search/chunked '(: bos "foo") rope-chunker '(("  foo" 1 5)) 2))
  (check-for-true
      (irregex-search/chunked '(: bos "foo" eos) rope-chunker '((" foo" 1 4)) 1))
  (check-for-true
      (irregex-search/chunked '(: bos "foo" eos) rope-chunker '(("  foo" 2 5)) 2))
  (check-for-true
      (irregex-search/chunked '(: bos "foo" eos) rope-chunker '((" foo" 0 4)) 1))
  (check-for-true
      (irregex-search/chunked '(: bos "foo" eos) rope-chunker '(("  foo" 1 5)) 2))


  #t)


(parametrise ((check-test-name 'replace))

  (check
      (irregex-replace/all (rx "Cotton") "dry Cotton" "Jin")
    => "dry Jin")

  (check
      (irregex-replace/all (rx (submatch (+ digit)) "/"
			       (submatch (+ digit)) "/"
			       (submatch (+ digit)))
			   "03/01/79"
			   2 "/" 1 "/" 3)
    => "01/03/79")

  (let ((str "9/29/61"))
    (check
	(irregex-replace/all
	 (rx (submatch (+ digit)) "/"
	     (submatch (+ digit)) "/"
	     (submatch (+ digit)))
	 str
	 (lambda (m)
	   (let ((mon (vector-ref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
				     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
				  (- (string->number
				      (irregex-match-substring m 1))
				     1)))
		 (day (irregex-match-substring m 2))
		 (year (irregex-match-substring m 3)))
	     (string-append mon " " day ", 19" year))))
      => "Sep 29, 1961"))

  (let ((kill-matches (lambda (re s)
			(irregex-replace/all re s))))
    (check
	(kill-matches (rx (or "Windows" "tcl" "Intel"))
		      "Windows will disappear, also tcl and Intel")
      => " will disappear, also  and "))


  )


;;;; done

(check-report)

;;; end of file
